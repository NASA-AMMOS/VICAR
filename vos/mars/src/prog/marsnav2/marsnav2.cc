////////////////////////////////////////////////////////////////////////
// marsnav2 - multimission Bundle Adjustment program
////////////////////////////////////////////////////////////////////////

#include "vicmain_c"
 
#include "xvector.h"

#include "mars_support.h"
#include "mars_tiepoints.h"


#include "PigMission.h"
#include "PigFileModel.h"
#include "PigCameraModel.h"
#include "PigPointingModel.h"
#include "PigSurfaceModel.h"
#include "PigVector.h"
#include "RadiometryModel.h"
#include "PigCoordSystem.h"
#include "PigCAHV.h"
#include "PigCAHVOR.h"
#include "PigCAHVORE.h"
#include "PigCSReference.h"
#include "PigMER.h"
#include "PigSurfacePlane.h"


#ifdef _OPENMP
#include <omp.h>
#endif

#include "ceres/ceres.h"
#include "ceres/types.h"
#include "ceres/normal_prior.h"   
#include "glog/logging.h"

#include "tradiprojection.h"
#include <Eigen/Dense>   
#include <array>

using namespace std;
using namespace Eigen;


/* buffer sizes in main program */
#define MAX_INPUTS 2000
#define MAX_THREADS 256

#define MAX_NL MARS_MAX_NL
#define MAX_NS MARS_MAX_NS


// Maintain the list of Sites in the image list, and which ones get adjusted
struct SiteList {
    PigCoordSystem *site;
    int num_images;		// # of images using this Site
    int adjust_location;	// do we adjust this location (directly)?
    int adjust_orientation;	// do we adjust this orientation (directly)?
    int match_site_loc;		// Site # to stay in sync with for location
    int match_site_ori;		// Site # to stay in sync with for orientation
};


// Observation (half tie-point)
struct observation {
    int imageIndex; // equivalent to left/right_image in TiePoint structure
    double sample;
    double line;
    double quality;
    int interactive;
    PigPoint xyz;
    // Addition for Extended tiepoint output
    double init_residual_sample  = 0;
    double init_residual_line    = 0;
    double final_residual_sample = 0;
    double final_residual_line   = 0;
    PigPoint init_xyz;
    PigPoint final_xyz;
    int track;
 };

struct color_3u8 {
   std::array<unsigned char, 3> rgb;
};

namespace Colors {
   constexpr color_3u8 RED{255,0,0};
   constexpr color_3u8 GREEN{0,255,0};
   constexpr color_3u8 BLUE{0,0,255};
   constexpr color_3u8 BLACK{0,0,0};
   constexpr color_3u8 CYAN{0,255,255};
   constexpr color_3u8 ORANGE{255,153,51};
};

template<typename T> 
vector <vector<T> > union_find(vector< vector<T> > tiePointsVect);
vector <std::pair<double, vector<int> > > getResidualPairs(
                            const std::vector<std::vector<std::string>> &tracks,
                            const vector<double> &residualArray);
std::map<double, int> getResidualHistogram(
          const std::vector<std::pair<double, std::vector<int>>> &residualPairs,
          const double &binW);
int filter_tracks(vector<vector<string> > &tracks, 
                  const map<string,observation> &mapOfPoints);
bool sortByResidual(const pair<double, vector<int> > &i, 
                     const pair<double, vector<int> > &j);
bool sortByIndex(const pair<double, vector<int> > &i, 
                 const pair<double, vector<int> > &j);
void save_pointing(int xml_mode, char *name, int n,
		       PigPointingModel *pointings[],
                       double pparams_org[][PIG_MAX_PARAMS], 
                       PigFileModel *file_models[], 
		       PigSurfaceModel *surface_model, 
                       char *solution_id, int originalCM);
void save_pointing_file(char *name, int n, PigPointingModel *pointings[],
		       double pparams_orig[][PIG_MAX_PARAMS]);
void save_pointing_xml(char *name, int n, PigPointingModel *pointings[],
                       double pparams_org[][PIG_MAX_PARAMS], 
                       PigFileModel *file_models[],
                       PigSurfaceModel *surface_model, 
                       char *solution_id, int original_cm);
void print_sites(SiteList sites[], int num_sites);

typedef Matrix<double, Dynamic, Dynamic> StiffnessMat;
typedef Matrix<double, Dynamic, 1> PointingVect;






// Visual Residual Display functions

void drawTieLocation(const std::vector<std::vector<std::string>> &tracks,
                     const std::map<string,observation> &mapOfPoints,
                     const color_3u8 &c, 
                     std::vector<SimpleImage<unsigned char> *> &tpImgs);

void drawTieResidual(const std::vector<std::vector<std::string>> &tracks,
                     const std::map<string,observation> &mapOfPoints,
                     const std::vector<double> &residuals,
                     const double scaler,
                     const color_3u8 &c, 
                     std::vector<SimpleImage<unsigned char> *> &tpImgs);

template<class T>
void drawCross(SimpleImage<T> *img, const int x, const int y, const color_3u8 &c);

template<class T1, class T2>
void drawLine(SimpleImage<T1> *img, T2 x1, T2 y1, T2 x2, T2 y2, const color_3u8 &c);

std::string getFileName(const char * fullName);

template<class T>
void saveImg(const std::string name, SimpleImage<T> * img);

SimpleImage<float> * loadImage(PigFileModel * file_model, int band = 1); 


// Triangulation functions (proper to marsnav2)

int xvector_pairwise_twoview(const PigPoint *lookOrigin, 
							 const PigVector *lookVector, 
							 PigPoint &xyz_final, 
							 double *error, 
							 const int currentTrackSize, 
							 const double parallel_limit, 
							 const double epsilon, 
							 const double error_thresh,
							 std::array<float, 6> & bounding_box);

int bounding_box_check(const PigPoint xyz, const std::array<float, 6> bounds);

////////////////////////////////////////////////////////////////////////
// MarsNAV2 program
////////////////////////////////////////////////////////////////////////

void main44()
{
    int i, j, status, index;
    int count, def, nbObservations;
    char msg[256];
    double epsilon = 1e-6;

    // Inputs

    int nids;
    PigFileModel *file_models[MAX_INPUTS];
    PigCameraModel *camera_in_all[MAX_INPUTS];
    PigPointingModel *pointing_in_all[MAX_INPUTS];
    PigSurfaceModel *surface_model;
    int homogeneous_inputs = TRUE;
    PigCoordSystem *output_cs;

    // Tiepoints

    int n_overlaps = MARS_MAX_TIEPOINTS;
    map<string,observation>::iterator it1, it2;

    TiePoint *tiepoints = new (std::nothrow) TiePoint[MARS_MAX_TIEPOINTS];
    if (tiepoints == NULL) {
       zvmessage("Memory allocation error during tiepoints array init.","");
       zabend();
    }

    int refimg[MAX_INPUTS];

    // Input params after input nav file applied; used for resetting solutions
    double pparams_in[MAX_INPUTS][PIG_MAX_PARAMS];

    // Original input params before nav file; used for saving nav file
    double pparams_in_save[MAX_INPUTS][PIG_MAX_PARAMS];

    // Sites list and pointing params
    int num_sites;
    struct SiteList site_list[PIG_MAX_CS];
    double sitesParams_in_save[PIG_MAX_CS][PIG_MAX_PARAMS];

    // User Parameters
    char outfilename[150];
    char tptlist[150], out_tptlist[150];
    char mission[64], instrument[64];
    char solution_id[64];
    double permitted_error;
    int xml_mode;
    int adjust_pointing;
    int adjust_surface;
    int adjust_sites;
    int remove_flag;			// whether or not to remove tiepoints
    double max_residual;
    int max_remove;
    int start_key;
    int opti_fiducial;
    int remove_fiducial;
	// For multi-view triangulation:
	int multiviewtrig;
	int optimize_points;
	int bbox_check;

    // Solver Parameters

    double * observationsXY;
    int * observationsGroundPointID;
    int * observationsCameraID;
    double * sitesPointingsParams;
    double * sitesPointingsParamsErr;
    double * groundPointsParams;
    double * pointingsParams;
    double * pointingsParamsErr;
    int * numPointingParams;

    // Some Ceres objects, 
    // There are a lot more options to adjust if needed in Ceres interface. 
    // Only a subset are parametrized so far.
    ceres::Problem::Options optionsProblem;     // Options for the problem
    ceres::Solver::Options optionsSolver;       // Options for the NLLS solver
    ceres::Problem::EvaluateOptions optionsEval;// Options for output evaluation



    ///////////////////////////////////////////////////////////////////////////



    zvmessage("MARSNAV2 version 2022-09-30", "");

    // Initialization of PIG elements.
    // Note that Pig surface model is not used in marsnav2. It is just retrieved
    // here and passed along to save_pointing. This is to keep consistency 
    // for programs using .nav file and expecting a surface model
    mars_setup(nids, file_models, camera_in_all, pointing_in_all, 
               surface_model, NULL, output_cs, mission, instrument,
	       homogeneous_inputs, MAX_NL, MAX_NS, MAX_INPUTS);

 
    // Get the initial pointing
    for (i = 0; i < nids; i++) {
	pointing_in_all[i]->getPointingParameters(pparams_in_save[i],
							PIG_MAX_PARAMS);
    }


    // Are we multi-threading? If yes, how many threads?
    int omp_on = zvptst("OMP_ON");
    int num_thr = 1;
#ifdef _OPENMP
    if (omp_on)
        num_thr = omp_get_max_threads();
#endif



    // Check for an input tiepoint table

    zvp("in_tpt", tptlist, &count);
    if (count != 1) {
	zvmessage("Input tiepoint file required!", "");
	zabend();
    }
    zvp("out_tpt", out_tptlist, &count);


    // Check for output bad tie points
    int outBadTies=0;
    char out_bad_ties[150];
    zvp("OUT_BAD", out_bad_ties, &outBadTies);

    // Check for extended tie point output
    int extendedTP = zvptst("EXT_TP");

    // Save original CM to nav file?
    int originalCM = zvptst("CM_ORI");

    // WARNING: If fiducial tiepoints are used, the REFERENCE_FRAME field in the
    // tiepoint file must be set to the corresponding fiducial CS. 
    mars_load_tiepoints(tptlist, tiepoints, n_overlaps, file_models, nids,
                        output_cs, PigMission::getMissionObject(mission));
    sprintf(msg, "%d tiepoints read from %s", n_overlaps, tptlist);
    zvmessage(msg, "");

    if (n_overlaps == 0) {
       zvmessage("No tiepoints. Returning...","");
       return;
    }



    // get parameter overrides if any

    zvparmd("ERROR", &permitted_error, &count, &def, 1, 0);

    int refimg_set = FALSE;
    for (i=0; i<nids; i++)
	refimg[i] = FALSE;

    int refimg_array[MAX_INPUTS];
    status = zvparm("REFIMAGE", refimg_array, &count, &def, MAX_INPUTS, 0);
    if (status == 1 && count > 0) {
	for (i=0; i < count; i++) {
	    if (refimg_array[i] == -1)
		continue;			// set nothing in the array
	    if (refimg_array[i] > nids || refimg_array[i] < -nids) {
		zvmessage("REFIMAGE value bigger than # of inputs!", "");
		zabend();
	    }
	    if (refimg_array[i] <= 0) {
		if (i < 1 || refimg_array[i-1] <= 0 ||
			refimg_array[i-1] >= -refimg_array[i]) {
		    zvmessage("REFIMAGE value less than 0 and prior ref not correct (neg or > this)", "");
		    zabend();
		}
		for (int j = refimg_array[i-1]; j <= -refimg_array[i]; j++) {
		    refimg[j-1] = TRUE;
		}
	    }
	    else {
	        refimg[refimg_array[i]-1] = TRUE;
	    }
	}
	refimg_set = TRUE;
    }

    if (zvptst("UNTIL")) {
	if (count < 1 || refimg_array[0] <= 0) {
	    zvmessage("Invalid entry for REFIMAGE with UNTIL specified", "");
	    zabend();
	}
	for (i=0; i < refimg_array[0]; i++)
	    refimg[i] = TRUE;
    }

    // Look at the ignore array.  Any tiepoints using an image in this list
    // will be inactivated.

    int ignore_set = FALSE;
    int ignore_array[MAX_INPUTS];
    int ignore[MAX_INPUTS];
    for (i=0; i<nids; i++)
        ignore[i] = FALSE;
    status = zvparm("IGNORE", ignore_array, &count, &def, MAX_INPUTS, 0);
    if (status == 1 && count > 0) {
        ignore_set = TRUE;
        for (i=0; i < count; i++) {
            if (ignore_array[i] > nids || ignore_array[i] < -nids) {
                zvmessage("IGNORE value bigger than # of inputs!", "");
                zabend();
            }
            if (ignore_array[i] <= 0) {
                if (i < 1 || ignore_array[i-1] <= 0 ||
                        ignore_array[i-1] >= -ignore_array[i]) {
                    zvmessage("IGNORE value less than 0 and prior ignore not correct (neg or > this)", "");
                    zabend();
                }
                for (int j = ignore_array[i-1]; j <= -ignore_array[i]; j++) {
                    ignore[j-1] = TRUE;
                }
            }
            else {
                ignore[ignore_array[i]-1] = TRUE;
            }
        }
    }

    if (ignore_set) {
        int n_ignored = 0;
        for (i=0; i < n_overlaps; i++) {
            TiePoint *tie = &tiepoints[i];
            if (!tie->active)
                continue;

	    if ((ignore[tie->left_image]) || ignore[tie->right_image]) {
                tie->active = FALSE;
                n_ignored++;
                continue;
            }
        }
        sprintf(msg, "%d tiepoints ignored due to IGNORE parameter", n_ignored);
        zvmessage(msg, "");
    }

    // Ignore intra-set matches.  In other words, if a tiepoint has two
    // (or more) images from the non-reference (active) set, ignore the
    // tiepoint.  This allows you to run a bunch of images at once, ignoring
    // connections between them, and looking only at how they connect to
    // their reference-image neighbors.

    if (zvptst("IGNORE_INTRA")) {
	int n_ignored = 0;
	for (i=0; i < n_overlaps; i++) {
	    TiePoint *tie = &tiepoints[i];
	    if (!tie->active)
		continue;
	
	    // Preserving some logic looking forward to multi-image tiepoints.
	    // See marsbrt for how that might work.
	
	    int n_in_nonref = 0;
	    if (!refimg[tie->left_image])
		n_in_nonref++;
	    if (!refimg[tie->right_image])
		n_in_nonref++;
	    if (n_in_nonref == 0 || n_in_nonref > 1) {
		tie->active = FALSE;
		n_ignored++;
	    }
	}
	sprintf(msg, "%d tiepoints ignored due to IGNORE_INTRA parameter",
		n_ignored);
	zvmessage(msg, "");
    }


    zvp("START_KEY", &start_key, &count);

    
    double parallel_limit;    
    zvparmd("PARALLEL_LIMIT", &parallel_limit, &count, &def, 1, 0); 
    

    // Check on what to adjust...

    adjust_pointing = zvptst("DO_POINTING");
    adjust_surface = zvptst("DO_XYZ");

    remove_flag = zvptst("REMOVE");

    zvparmd("MAX_RESIDUAL", &max_residual, &count, &def, 1, 0);

    zvp("MAX_REMOVE", &max_remove, &count);

    // Check the output format; if SOLUTION_ID is not given, fail now rather
    // than after the computation!

    xml_mode = zvptst("xml");

    if (xml_mode) {
        zvp("OUT_SOLUTION_ID", solution_id, &count);
        if (count != 1) {
            zvmessage("Output Solution ID required!", "");
	    zvmessage("Use OUT_SOLUTION_ID (recently changed from SOLUTION_ID)",
						"");
            zabend();
        }
    }
    zvpone("OUT", outfilename, 1, sizeof(outfilename));


	//Choose between two-view and multi-view triangulation:

	multiviewtrig = zvptst("DO_MVT");
	if (multiviewtrig) {
		zvmessage("Using multi-view triangulation", "");
	}

	//Choose between standard (linear/two-view) result without 
	//optmization and angular optimization:

	optimize_points = zvptst("DO_TRIOPT");
	if (optimize_points) {
		zvmessage("Applying angular optimization on the initial XYZ points", "");
	}	
	else {
		zvmessage("Not applying optimization on the initial XYZ points", "");
	}

	//Error threshould for multi-view triangulation:
	double error_thresh;    
    zvparmd("MVT_ERR_MAX", &error_thresh, &count, &def, 1, 0); 
	
	//Condition number threshould for multi-view triangulation:
	double cond_number_thresh;    
    zvparmd("CONDNUM_MAX", &cond_number_thresh, &count, &def, 1, 0); 

	//Maximum drift allowed after optimization:
	double drift_thresh;    
    zvparmd("DRIFT_THRESH", &drift_thresh, &count, &def, 1, 0); 

	// Check if an optimized point falls outside of the original bounding 
	// box produced by pairwise two-view estimates:
	bbox_check = zvptst("DO_BBOXCHECK");
	if (bbox_check) {
		zvmessage("Using bounding box check", "");
	}

    // For now, Dynamic XYZ tiepoints are treated as Traditional tiepoints,
    // that is, the XYZ coordinates are not read from images, but infered
    // from triangulation. This will probably change in the future, but some
    // points need to be clarified: how to handle tracks, error on XYZ, etc.
    //
    // XYZ retrieval commented out

    // Read in the XYZ values for all the Dynamic XYZ tiepoints, and convert
    // them to their own Rover frames.  This is a little inefficient in that
    // a file with multiple tiepoints is opened and closed for each tiepoint,
    // but at least we read in only the tiepoint, not the entire file.
    // UPDATE: For now, Dynamic XYZ tiepoints code below is commented out as
    // these tiepoints are just considered as traditional tiepoints 
    // (only looking at the left and right line/sample). This might change in
    // the future
#if 0
    PigMission *m = PigMission::getMissionObject(mission);
    for (i=0; i < n_overlaps; i++) {
	if (!tiepoints[i].active)
	    continue;
	if (tiepoints[i].type == TIEPOINT_DYNAMIC_XYZ) {
	    tiepoints[i].active = FALSE;	// until we get it right

	    // Sanity check and open the image

	    int file = tiepoints[i].left_image;
	    if (file < 0 || file >= nids) {
		sprintf(msg, "Invalid left file number (%d) for tiepoint %d",
				file, i);
		zvmessage(msg, "");
		continue;		// bail
	    }
	    int xyz_unit = file_models[file]->getUnit();
	    status = zvopen(xyz_unit, "op","read", "open_act","sa",
		"io_act","sa", "u_format","doub", NULL);
	    file_models[file]->setFileOpen(TRUE);
	    if (file_models[file]->getNB() != 3) {
		sprintf(msg, "Dynamic XYZ input file must have 3 bands (file %d, tiepoint %d", file, i);
		zvmessage(msg, "");
		file_models[file]->closeFile();
		continue;		// bail
	    }

	    // Read the XYZ point

	    int line = (int)(tiepoints[i].left_line + 0.5);
	    int samp = (int)(tiepoints[i].left_sample + 0.5);
	    double x, y, z;
	    zvread(xyz_unit, &x, "band",1, "line",line+1, "samp",samp+1,
				"nsamps", 1, NULL);
	    zvread(xyz_unit, &y, "band",2, "line",line+1, "samp",samp+1,
				"nsamps", 1, NULL);
	    zvread(xyz_unit, &z, "band",3, "line",line+1, "samp",samp+1,
				"nsamps", 1, NULL);
	    PigPoint xyz(x,y,z);

	    if (x == 0.0 && y == 0.0 && z == 0.0) {
		sprintf(msg, "XYZ value for dynamic XYZ tiepoint %d is missing.  Tiepoint ignored.", i);
		zvmessage(msg, "");
		file_models[file]->closeFile();
		continue;			// bail
	    }

	    // Get CS for the XYZ file itself

	    PigCSReference ref;
	    file_models[file]->getDerivedImageCS(&ref);
	    PigCoordSystem *xyz_cs = m->getCoordSystem(&ref);

	    // Get "natural" CS for the instrument in this image (usually
	    // a Rover frame)

	    PigCoordSystem *rover_cs=m->getCoordSystem(file_models[file],NULL);

	    // Convert

	    PigPoint rover_xyz = rover_cs->convertPoint(xyz, xyz_cs);

	    // Save it all away

	    tiepoints[i].xyz = rover_xyz;
	    tiepoints[i].cs = rover_cs;

	    tiepoints[i].active = TRUE;		// good, finally!
	    file_models[file]->closeFile();
	}
    }
#endif

    // Print out input status from labels

    mars_print_inputs(nids, pointing_in_all, camera_in_all, file_models,
			homogeneous_inputs, mission, instrument);

    // save the original pointing (after input nav file)

    for (i=0; i < nids; i++) {
	pointing_in_all[i]->getPointingParameters(pparams_in[i],
						     PIG_MAX_PARAMS);
    }



    // Set the initial pointing

    for (i=0; i < nids; i++) {
       pointing_in_all[i]->setPointingParameters(pparams_in[i],
						    PIG_MAX_PARAMS);
    }


    // Gather the Site list
 
    PigMission *m = PigMission::getMissionObject(mission);
    int filesSites[nids];  //to record which site is associated with this image 

    num_sites = 0;
    for (i=0; i < nids; i++) {
	PigCoordSystem *cs = m->getCoordSystem(file_models[i], NULL);

	// See if we have this one already

	int j;
	for (j=0; j < num_sites; j++) {
	    if (site_list[j].site == cs) {
		site_list[j].num_images++;
                filesSites[i] = j; 
		break;
	    }
	}
	if (j == num_sites) {			// not found, add one
	    site_list[num_sites].site = cs;
	    site_list[num_sites].num_images = 1;
	    site_list[num_sites].adjust_location = FALSE;
	    site_list[num_sites].adjust_orientation = FALSE;
	    site_list[num_sites].match_site_loc = -1;
	    site_list[num_sites].match_site_ori = -1;
            filesSites[i] = j; 
	    num_sites++;
	}
    }

    for (i = 0; i < num_sites; i++) {
       int n = site_list[i].site->getPointingParamCount();
       site_list[i].site->getPointingParameters(sitesParams_in_save[i], n);
    }


    // Figure out which sites to adjust

    int list[PIG_MAX_CS];
    int nlist;
    adjust_sites = FALSE;
    zvp("DO_LOCATION", list, &nlist);
    for (i=0; i < nlist; i++) {
	site_list[list[i]].adjust_location = TRUE;
	adjust_sites = TRUE;
    }
    zvp("DO_ORIENTATION", list, &nlist);
    for (i=0; i < nlist; i++) {
	site_list[list[i]].adjust_orientation = TRUE;
	adjust_sites = TRUE;
    }

//!!!! NEED TO SET UP MATCH_SITE's
//!!!! But how does the UI work???

    // Print out the Site list

    zvmessage("List of Sites used by input files:", "");
    for (i=0; i < num_sites; i++) {
	sprintf(msg, "  Site %d,'%s',count=%d. Adj loc=%d,ori=%d. match loc=%d,ori=%d",
		i, site_list[i].site->getFullName(), site_list[i].num_images,
		site_list[i].adjust_location, site_list[i].adjust_orientation,
		site_list[i].match_site_loc, site_list[i].match_site_ori);
	zvmessage(msg, "");
    }


    // If refimg wasn't specified in parameters, calculate it now
    // It will be the "most connected" image.

    if (!refimg_set) {

	int image_frequency[MAX_INPUTS];

	for (i=0; i < nids; i++)
	    image_frequency[i] = 0;
	for (i=0; i < n_overlaps; i++) {
	    if (!tiepoints[i].active)
		continue;
	    image_frequency[tiepoints[i].left_image]++;
	    image_frequency[tiepoints[i].right_image]++;
	}
	int k = 0;
	int m = 0;
	for (i=0; i < nids; i++) {
	    if (m < image_frequency[i]) {
		m = image_frequency[i];
		k = i;
	    }
	}
	refimg[k] = TRUE;
    }

    refimg_set = FALSE;
    for (i=0; i < nids; i++) {
	if (refimg[i]) {
	    sprintf(msg, "Reference image: %d", i+1);
	    zvmessage(msg, "");
	    refimg_set = TRUE;
	}
    }
    if (!refimg_set) {
	sprintf(msg, "No reference image");
        zvmessage(msg, "");
    }



    // Entering the part new compared to Marsnav
    
    // TO DO/INVESTIGATE:
    // Deal with very close tiepoints coordinates but not rigorously equals
    // such as the ones obtained from correlation and as opposed to tie points 
    // obtained from keypoint matching (e.g., sift).


    google::InitGoogleLogging("Marsnav2 google logging");


    // Remove erroneous tracks?
    int filter_on = zvptst("DO_FILTER");
    
 
    // Retrieving and setting the solver type.
    // DENSE_QR, DENSE_SCHUR, are the solvers that are most likely to be used. 
    // More information available at ceres-solver.org.
    char solver_char[128];
    zvp("SOLVER",solver_char, &count);
    string solver_name(solver_char);
    if (!StringToLinearSolverType(solver_name, &optionsSolver.linear_solver_type)) {
       zvmessage("Solver type not supported.","");
       zabend();
    }

    // Initialize a loss function, that is a function that will minimize the
    // weight (in the least square budget) of spurious residual in order to 
    // limit their influence (a large residual, squared, will have a significant
    // effect in the solution if not dampened)
    ceres::LossFunction *  loss_function; 
    if (zvptst("HUBERLOSS"))
       loss_function = new ceres::HuberLoss(1.0);
    else if (zvptst("CAUCHYLOSS"))
       loss_function = new ceres::CauchyLoss(1.0);
    else
       loss_function = NULL; //No loss function applied


    // By default, Ceres problem takes ownership of the loss function which 
    // means that the loss function would be destroyed if the process is 
    // iterated due to the removal of bad observations (problem gets reset). 
    // We remove the ownership such that the loss funtion stays available
    optionsProblem.loss_function_ownership = 
                                        ceres::Ownership::DO_NOT_TAKE_OWNERSHIP;


    // Setting multithreading option:
    // Ceres has two multithreading options. One for computing the
    // residuals and jacobian for a given parameter set, and one for solving the
    // linearized system. Don't turn on multithreading for the former because of
    // racing conditions as PIG objects are just pointed to. We could instead 
    // instantiate PIG objects for each observation, but first it's slow, and 
    // second it is RAM hungry (150k observations is about 8-10 GB in RAM). 
    // Multithreading for the linear solver part is fine.
    //
    // options.num_threads= num_thr;   //DON'T DO IT - see comment
    optionsSolver.num_linear_solver_threads = num_thr;

    // Printing summary for each solver iteration
    if (zvptst("ITER_DISP")) {
       optionsSolver.logging_type = ceres::LoggingType::PER_MINIMIZER_ITERATION;
       optionsSolver.minimizer_progress_to_stdout = TRUE;
    }
    else {
       optionsSolver.logging_type = ceres::LoggingType::SILENT; 
       optionsSolver.minimizer_progress_to_stdout = FALSE;
    }

    // Maximum number of iterations the solver can take before bailing out
    int maxIter;
    zvp("ITER_MAX", &maxIter, &count);
    if (count) {
       if (maxIter < 1) {
          zvmessage("Max number of BA solving iterations set to 0. Makes no sense!","");
          zabend();
       }
       optionsSolver.max_num_iterations = maxIter;
    }

    // The loss function is automatically applied when the problem is solved, 
    // but when the residual are estimated (after the optimization is done) for 
    // evaluation, the loss function can be applied to the residual or not.
    // If loss function is set to TRIVIALLOSS, residuals with or without
    // apply_loss will be identical. 
    if (zvptst("APPLY_LOSS"))
       optionsEval.apply_loss_function = TRUE;
    else   
       optionsEval.apply_loss_function = FALSE;


    // Which minimization report to display in the stdout: none, full, brief
    string summary_disp = "FULL_SUM";
    if (zvptst("NO_SUM"))
       summary_disp = "NO_SUM";
    else if (zvptst("BRIEF_SUM"))
       summary_disp = "BRIEF_SUM";
    
   
    // Display images connectivity matrix?
    // If the images number is large, the matrix won't be displayed nicely
    int connectivity_disp = zvptst("DISP_CONNECT"); 

    // Get and set the number of residuals to print in the stdout, ordered in 
    // worst to best order. Negative number means to print all observations
    // residuals
    int residual_disp;
    zvp("RESIDUAL_DISP", &residual_disp, &count); 
   
    // Are the residuals distribution function to be displayed?
    int numBin = 0;
    zvp("HIST_BIN", &numBin, &count); 
    // If output bad ties is activated, numBin must be activated. This is just
    // because printing bad tie need to compute the residual pre-bundle which
    // is done only if histogram of residual is displayed. It's only an 
    // implementation thing. 
    if (outBadTies && numBin <= 0)    
       numBin = 100;

 

    // Get the parameters bounds coefficient for pointing parameters
    double nsigma_pointings=0;
    zvparmd("BOUNDS_POINT", &nsigma_pointings, &count, &def, 1, 0); 


    // Get the parameters bounds coefficient for sites parameters
    double nsigma_sites=0;
    zvparmd("BOUNDS_SITE", &nsigma_sites, &count, &def, 1, 0); 

    // Get manually entered pointing errors. Normally errors are 
    // retrieved from some calibration file. Overwrite then with
    // manual entry. 
    // NOTE: This parameters might eventually be deprecated. Born
    // from the lack of error estimate on the Ingenuity data
    float manPointErrors[10];
    int numManPointErrors;
    zvp("POINT_ERR", manPointErrors, &numManPointErrors);

    // Get the parameters for fiducial point management 
    opti_fiducial = zvptst("OPTI_FIDUCIAL");
    remove_fiducial = zvptst("FID_REMOVE");

    // Is a Normal prior constraint on the pointing parameters applied?
    double inertia=0;
    zvparmd("INERTIA", &inertia, &count, &def, 1, 0);
    if (inertia < 0)
       inertia = 0;


    // Are there any tracks size to ignore
    std::vector<int> remove_tracks;
    int remove_tracks_arr[MAX_INPUTS];
    for (i=0; i < MAX_INPUTS; i++) remove_tracks_arr[i]=0;
    status = zvparm("TRACK_REMOVE", remove_tracks_arr, &count, &def, MAX_INPUTS, 0);
    if (status == 1 && count > 0) {
	for (i=0; i < count; i++) {
	    if (remove_tracks_arr[i] == -1 || remove_tracks_arr[i] == 0)
		continue;			// set nothing in the array
	    if (remove_tracks_arr[i] > nids || remove_tracks_arr[i] < -nids) {
		zvmessage("TRACK_REMOVE value bigger than # of inputs!", "");
		zabend();
	    }
	    if (remove_tracks_arr[i] < 0) {
		if (i < 1 || remove_tracks_arr[i-1] < 0 ||
			remove_tracks_arr[i-1] >= -remove_tracks_arr[i]) {
		    zvmessage("TRACK_REMOVE value less than 0 and prior ref not correct (neg or > this)", "");
		    zabend();
		}
		for (int j = remove_tracks_arr[i-1]+1; j <= -remove_tracks_arr[i]; j++) {
		    remove_tracks.push_back(j);
		}
	    }
	    else {
	        remove_tracks.push_back(remove_tracks_arr[i]);
	    }
	}
    }

   if (remove_tracks.size() != 0) {
      char *pos = msg;
      zvmessage("Tracks of the following size(s) will be removed:","");
      for (const auto &v:remove_tracks)
         pos += sprintf(pos, "%d,",v);
      zvmessage(msg," ");
   }
   else
      zvmessage("All track sizes considered.","");


   // Set container of annotated images
   // Does the user want to save reprojection error displays
   int visualResidual = zvptst("VISRES");
   std::vector<SimpleImage<unsigned char> *> tpImgs;

   // Scaler to increase/decrease the tiepoint residual display (length) on the
   // output images
   double visualScaler = 1;
   zvparmd("SCALE_RES", &visualScaler, &count, &def, 1, 0);
   if (count) {
      // Make sure it's not 0. Would set all residual length to 0.
      if (visualScaler == 0)
         visualScaler = 1;
   }


   // Similar to the output bad tie activation, if visual image of tp residual
   // is activated, numBin must be activated. This is just because the 
   // pre-bundle solution needs to be computed, which is done only if histogram
   // of residual is displayed. It's only a poor implementation thing. 
   if (visualResidual && numBin <= 0)    
       numBin = 100;

   // Same as above. Will need some cleaning
   if (extendedTP && numBin <= 0)    
       numBin = 100;
   

   // If yes, copy and normalize images into container.
   // ATTENTION: Depending on the number of images this can consume a lot of 
   // memory
   if (visualResidual) {
   
      // For now, take all input. Later only the ones with active tiepoints
      tpImgs.resize(nids);
 
      for (int i = 0; i < nids; i++) { 

         // Image will be B&W only, first band 
         SimpleImage<float> * img = loadImage(file_models[i], 1); // 0=band num
         tpImgs[i] = new SimpleImage<unsigned char>(3, img->getNL(), img->getNS());
         int n = img->getNL() * img->getNS();

         // Compute average/min/max of DN values to get good contrast with drawing
         float min = (std::numeric_limits<float>::max)();
         float max = (std::numeric_limits<float>::min)();
         float *p = img->linePtr(0);   
         std::for_each(p, p + n, [&](float &x){ if (x>max) max=x; if (x<min) min=x;});

         // Copy image and normalize
         // First band
         std::transform(p, p+n, tpImgs[i]->linePtr(0), 
                       [&](float &x){ return (unsigned char)(255 * (x - min)/(max-min)); });
         // Second and third band are the same - copy.
         std::memcpy(tpImgs[i]->linePtr(0) + n, tpImgs[i]->linePtr(0), n * sizeof(unsigned char));
         std::memcpy(tpImgs[i]->linePtr(0) + 2*n, tpImgs[i]->linePtr(0), n * sizeof(unsigned char));

         // Free mem
         img->free();
      }
   }






    //All parameters retrieved, move on to analyzing the input data 



    zvmessage(" ",""); 
    sprintf(msg, "%d tiepoints read from %s", n_overlaps, tptlist);
    zvmessage(msg,"");

    // Look up for number of active tie points. Inactive tie points
    // are removed from the BA. Tiepoints have to be either Traditional, 
    // XYZ Dynamic, Z Surface,  or Miss, to be usable in a BA. 
    count = 0;
    for (i=0; i<n_overlaps; i++) {
        if (tiepoints[i].active && 
           (tiepoints[i].type == TIEPOINT_TRADITIONAL || 
            tiepoints[i].type == TIEPOINT_FIDUCIAL || 
            tiepoints[i].type == TIEPOINT_Z_SURFACE || 
            tiepoints[i].type == TIEPOINT_DYNAMIC_XYZ || 
            tiepoints[i].type == TIEPOINT_MISS_DISTANCE))
            count++;
    }

    sprintf(msg, "%d active and valid tie-points", count);
    zvmessage(msg,"");


    // Each tiepoint is composed of two observations (left and right). From the
    // list of tiepoints, we need to generates a list of tracks. A track 
    // contains all observations that *look* at the same ground point. Each 
    // observation needs to be uniquely identified. To do that we generate a 
    // unique ID for each of them which is a string concatenation of 
    // imageID_sample_line, and associates (std::map) it with a structure 
    // containing image index, sample, line.
    map<string,observation> mapOfPoints;
    vector<vector<string> > tiePointsVect;

    observation infoPoint;
    // Default initialization of Extented tiepoints fields
    infoPoint.init_residual_sample  = 0;
    infoPoint.init_residual_line    = 0;
    infoPoint.final_residual_sample = 0;
    infoPoint.final_residual_line   = 0;
    infoPoint.init_xyz              = PigPoint(0,0,0);
    infoPoint.final_xyz             = PigPoint(0,0,0);
    infoPoint.track                 = 0;
           


    int alreadyFound = 0;
    for (i=0; i<n_overlaps; i++) {
        if (!tiepoints[i].active)
            continue;

        if (tiepoints[i].type == TIEPOINT_FIDUCIAL) {
           string obs = std::to_string(tiepoints[i].left_key) + '_'
                        + std::to_string(tiepoints[i].left_sample) + '_'
                        + std::to_string(tiepoints[i].left_line);

           //Add fiducial in list of points
           vector<string> fiducialPointPair = {obs, obs};
           tiePointsVect.push_back(fiducialPointPair);

           //Save pixels image id and samp/line information in a map
           infoPoint.imageIndex  = tiepoints[i].left_image;
           infoPoint.sample      = tiepoints[i].left_sample;
           infoPoint.line        = tiepoints[i].left_line;
           infoPoint.quality     = tiepoints[i].quality;
           infoPoint.interactive = tiepoints[i].interactive;
           infoPoint.xyz         = tiepoints[i].xyz;
           mapOfPoints.insert(std::make_pair(obs,infoPoint));

        }
        else if (tiepoints[i].type == TIEPOINT_TRADITIONAL || 
                 tiepoints[i].type == TIEPOINT_Z_SURFACE || 
                 tiepoints[i].type == TIEPOINT_DYNAMIC_XYZ || 
                 tiepoints[i].type == TIEPOINT_MISS_DISTANCE) {

           //generate a unique id for current left image pixel
           string leftObs = std::to_string(tiepoints[i].left_key) + '_'
                            + std::to_string(tiepoints[i].left_sample) + '_'
                            + std::to_string(tiepoints[i].left_line);

           //generate a unique id for current right image pixel
           string rightObs = std::to_string(tiepoints[i].right_key) + '_'
                             + std::to_string(tiepoints[i].corrected_sample) + '_'
                             + std::to_string(tiepoints[i].corrected_line);

           //Add pair to vector of tie points
           vector<string> tiePointPair = {leftObs, rightObs};
           tiePointsVect.push_back(tiePointPair);

           //Save pixels image id and samp/line information in a map
           infoPoint.imageIndex  = tiepoints[i].left_image;
           infoPoint.sample      = tiepoints[i].left_sample;
           infoPoint.line        = tiepoints[i].left_line;
           infoPoint.quality     = tiepoints[i].quality;
           infoPoint.interactive = tiepoints[i].interactive;
           infoPoint.xyz         = PigPoint(0,0,0);
           mapOfPoints.insert(std::make_pair(leftObs,infoPoint));


           infoPoint.imageIndex  = tiepoints[i].right_image;
           infoPoint.sample      = tiepoints[i].corrected_sample;
           infoPoint.line        = tiepoints[i].corrected_line;
           mapOfPoints.insert(std::make_pair(rightObs,infoPoint));
        }

        else {
           if (!alreadyFound) {
              zvmessage("Warning: Tiepoint must be TRAD, Fiducial, Z Surface, DYNAMIC, or MISS","");
              zvmessage("Unsupported tiepoint(s) are removed","");
              alreadyFound = 1;
           }
        }

    }


    // Get the tracks.
    // A track is closely related to Ground Point. The two are sometime used 
    // interchangeably in the comments. Ground Point usually refer to the XYZ 
    // coordinates, whether tracks refer to the list of image obsevations 
    // looking at the same ground point. A track is composed of at least two 
    // observations (i.e., left and right of a tie-points), but can contain 
    // more than two observations in a multi-view context (i.e., more than
    // 2 images). For instance, assume pix ids 1,2,3 from three different 
    // images, and the following two tie points relationship: (1-2) and (2-3).
    // This means that 1,2 and 3 *look* at the same location on the ground. 
    // The track will therefore contain the three pix ids:1,2,3
    // Same result  if we had these two tie points: (1-2) and (1-3), or these 
    // three tie points: (1-2), (1-3) and (2-3)
    //
    // The number of tracks is therefore equals to the number of unique
    // ground points observed in the entire tiepoints list.

    // "tracks" is a vector of vector. "tracks" contains all the tracks, and 
    // each track is composed of at least two image observations
    vector<vector<string> > tracks = union_find(tiePointsVect);

    // If output of visual residual 
    // Draw all tiepoints orange. Tiepoints belonging to non-corrupted tracks
    // will be drawn over, leaving in orange the tiepoints belonging to a 
    // corrupted tracks
    if (visualResidual)
       drawTieLocation(tracks, mapOfPoints, Colors::ORANGE, tpImgs);

    // Filter the track for corrupted tracks
    if (filter_on) {
       int nbTracksRemoved = filter_tracks(tracks, mapOfPoints); 
       sprintf(msg, "%ld initial tracks found (%d corrupted track(s) found and removed)", 
            tracks.size(), nbTracksRemoved);
    }
    else
       sprintf(msg, "%ld initial tracks found (no tracks filtering applied)", 
            tracks.size());
    zvmessage(msg,"");

    // If output of visual residual 
    // Draw all remaining tiepoints black. Tiepoints belonging to tracks whose
    // length is not filtered out by user criteria will be drawn over, leaving
    // in black the tiepoints that belong to tracks filtered out by user 
    // criteria.
    if (visualResidual)
       drawTieLocation(tracks, mapOfPoints, Colors::BLACK, tpImgs);

    // If some track size(s) are to be NOT considered during the bundle
    // remove them
    // (not efficient, but seldom used)
    if (remove_tracks.size() != 0) {
       std::vector<int> removedNum(remove_tracks.size());
       for (int i=tracks.size()-1; i >= 0; i--) {
          for (int j=0; j<remove_tracks.size(); j++) {
             if (tracks[i].size() == remove_tracks[j]) {
                tracks.erase(tracks.begin()+i);
                removedNum[j] += 1;
                break;
             }
          }
       }
       zvmessage("","");
       for (int j=0; j<removedNum.size(); j++) {
          sprintf(msg, "%d %d-observations tracks removed (user request)", removedNum[j], remove_tracks[j]);
          zvmessage(msg,"");
       }
    }

    // If output of visual residual 
    // Draw all remaining tiepoints cyan. Tiepoints whose triangulation is valid
    // will be draw over, leaving in cyan the tiepoints whose triangulation 
    // failed.
    if (visualResidual)
       drawTieLocation(tracks, mapOfPoints, Colors::CYAN, tpImgs);

    // Entering an iterative area.
    // A given loop computes the Ground Points XYZ for each track, set up the
    // bundle adjustment problem, and solves it. Depending on the bundle 
    // adjusment results (residuals) and on the user input parameters, the worst
    // residual observations are removed from the tracks and the process is 
    // iterated.
    // Note that it is not very efficient to recompute all the XYZ if only a
    // few observations are removed, but it's simpler and does not add a 
    // significant time penalty

    int redo_solution = TRUE;
    int remove_loops = 0;

    // For output (visual tiepoints residual, extended tiepoints)
    std::vector<double> finalResidual;
    double * finalXYZ;

    // For Extented tie point, to save initial tracks before bundle
    vector<vector<string> > initial_tracks;


    while (redo_solution) {

       if (remove_loops) { 
          zvmessage(" ","");
          sprintf(msg,"****************** Remove iteration %d ***********************",
                 remove_loops);
          zvmessage(msg,"");
       }

       redo_solution = FALSE;

       // Iterate over the tracks list and compute the average ground 
       // coordinates corresponding to each track. The average ground 
       // coordinates is obtained from the triangulation of pairwise combination
       // of observations composing that track. 
       // Once the ground point (XYZ) is computed, the ground point ID is 
       // associated to the corresponding observation (for bookkeeping).
       // This task is processing intensive so we use multithreading if 
       // available. If the convergence angle is too parallel (above limit set 
       // by user) the triangulation is not computed. If a track has no 
       // triangulation, it is flagged as a bad track and removed from the BA.

       vector<PigPoint> avgGroundPoints(tracks.size());
	   //All tracks set to invalid until proven otherwise
       vector<int> badTracks(tracks.size(), 1);  



#pragma omp parallel for schedule(dynamic) if (omp_on)
       for (i=0; i<tracks.size(); i++) {

          int currentTrackSize = tracks[i].size();
          PigVector lookVector[currentTrackSize];
          PigPoint  lookOrigin[currentTrackSize], xyz, sum_xyz;
          map<string,observation>::iterator it;
          double err, sumOfPointWeights = 0.0;


          //Special case of fiducial point (1-element track), for which
          //the XYZ is already available
          if (currentTrackSize == 1) {
             it = mapOfPoints.find(tracks[i][0]);
             if (it != mapOfPoints.end()) 
                xyz = (it->second).xyz; 
             else {
                zvmessage("Error fiducial not found. Internal error","");
                zabend();
             }
             avgGroundPoints[i] = xyz;
             badTracks[i] = 0;
             continue;
          }
          
 

          // First, compute the look vector and look origin of each observation
          // contained in the current track.       
          for (int j=0; j<currentTrackSize; j++) {
             it = mapOfPoints.find(tracks[i][j]);

             if (it != mapOfPoints.end())
               camera_in_all[(it->second).imageIndex]->LStoLookVector(
                                                   (it->second).line,
                                                   (it->second).sample,
                                                   lookOrigin[j], lookVector[j],
                                                   output_cs);
             else {
                zvmessage("Unknown observation ID. Internal error","");
                zabend();
             }

          }

          // Second, for each pairwaise combination between all observations of 
          // the current track, compute the XYZ (simple triangulation), and sum 
          // them up to latter estimate the XYZ average. Each XYZ is weighted by
          // the inverse of the "miss" distance, given by "err" variable.
          // Note that the err variable (miss distance) is only a partially
          // reliable indicator of confidence. Miss distance can be small but 
          // XYZ largely wrong for instance if rays are close to co-planar.
          // If triangulation fails, we skip the current triangulation.
          // If one of the XYZ in the track end up being "behind" the camera, 
          // that XYZ is discarded from the averaging.
          // WARNING: the last condition on the XYZ works ok in case the rays
          // are very close to being // and a small error in pointing and pixel
          // coordinates can cause the ray to diverge. Removing the entire 
          // track is wasteful as the bundle can recover for that error.
          // However, in case of really bad tiepoints (outlier), the outlier is
          // kept in the bundle although it should be removed.
          // May be the "weighting" of the XYZ should account for both miss 
          // distance and incidence angle.
          PigVector los;
          float dotVal;

		  // **********************************************************
		  //Apply multi-view triangulation:

		  if(multiviewtrig) {
		
			 // *******************************************************
			 
			 // This run will consist of several steps:
			 //
			 // 1) Compute all pairwise two-view midpoints, and compute
			 // the mean error. This also works on a single pair.
			 //
			 // 2) Apply angular optimization on this point, if 
			 // specified. The optimization SHOULD improve on the error,
			 // but in case it doesn't, stick with the original point 
			 // from (1).
			 //
			 // 3) Apply linear triangulation via xvector_multiview, and
			 // obtain a mean error.
			 //
			 // 4) Similarly to (2), apply angular optimization to see 
			 // if the error improves. If it doesn't, stick with the 
			 // original multi-view point.
			 //
			 // 5) Finally, compare the pair-wise and multi-view errors, 
			 // and choose the final point as the one with the lowest
			 // error.
			 // *******************************************************
			 
			 // (1) Initial triangulation

			 int status_final = 1;
			 PigPoint xyz_pairwise, xyz_opt, xyz_multi, xyz_multi_opt, 						  initial_point, point_multi, final_point;
			 double err_pairwise, err_multi, initial_error, error_multi, 						final_error;

  			 // Bounding box for the current XYZ point:
			 std::array<float, 6> bounding_box;
		 
		     // Take all pairwise estimates; also works for 2-view:
			 int status_pairwise = xvector_pairwise_twoview(lookOrigin, 				 lookVector, xyz_pairwise, &err_pairwise,
				 currentTrackSize, parallel_limit, epsilon, 
				 error_thresh, bounding_box);
			 //Debug print for the bounding box:
			 //if(currentTrackSize > 2)
			 //    cout << "bbox: (" << bounding_box[0] << ", " << 
			 //					  bounding_box[1] << ", " <<
			 //					  bounding_box[2] << ", " <<
			 //					  bounding_box[3] << ", " <<
			 //					  bounding_box[4] << ", " <<
			 //					  bounding_box[5] << ")" << endl;

			 if(status_pairwise == 0) {
			 	initial_error = err_pairwise;
			 	initial_point = xyz_pairwise;
			 }

             // (2) Apply angular optimization

			 if(status_pairwise == 0) {
			     if(optimize_points) {
				
					PigPoint xyz_opt;
					int status_opt_pairwise = xvector_optimize(lookOrigin, 						lookVector, xyz_opt, initial_point, currentTrackSize, 
					drift_thresh);
					
					// Bounding box check
					if( bbox_check && 
					    (status_opt_pairwise == 0) && 
						(currentTrackSize > 2) ) {
						int current_bbox_check = bounding_box_check(xyz_opt,
								bounding_box);
						if(current_bbox_check)
							status_opt_pairwise = 1; // failed bbox check
					}

					// Update initial_point with the optimized XYZ
					if(status_opt_pairwise == 0) 
					 	initial_point = xyz_opt; 
				 }
			 }
		
			 // (3) Multi-view triangulation
			 // If the parallel limit is not violated, linear 
			 // triangulation is likely to provide a better start to 
			 // angular optimization than the pairwise two-view method

			 int status_multi = xvector_multiview(lookOrigin, 	
					 lookVector, xyz_multi, &err_multi, currentTrackSize, 						 epsilon, error_thresh, cond_number_thresh);

			 // Bounding box check
			 if( bbox_check && 
				 (status_multi == 0) && 
				 (currentTrackSize > 2) ) {
			     int current_bbox_check = bounding_box_check(xyz_multi,
				        bounding_box);
				 if(current_bbox_check)
					 status_multi = 1; // failed bbox check
			 }

			 if(status_multi == 0) {
			 	point_multi = xyz_multi;
			 	error_multi = err_multi;
			 }

			 // (4) Angular optimization of the multi-view result

			 if(status_multi == 0) {
				 if(optimize_points) {
				
					 PigPoint xyz_multi_opt;
					 int status_opt_multi = xvector_optimize(lookOrigin, 
								lookVector, xyz_multi_opt, xyz_multi, 									currentTrackSize, drift_thresh);
				
			 		 // Bounding box check
				     if(bbox_check && 
					    (status_opt_multi == 0) && 
						(currentTrackSize > 2) ) {
			     		 int current_bbox_check =
						     bounding_box_check(xyz_multi_opt, bounding_box);
				 	 	 if(current_bbox_check)
					 	 	 status_opt_multi = 1; // failed bbox check
					 }

					 // Update multi_point with the optimized XYZ
					 if(status_opt_multi == 0) 	
					 	 point_multi = xyz_multi_opt; 
				 }
			 }

			 // (5) Error comparison and choice of the final point

			 if(status_pairwise == 0 && status_multi == 0) {
				 if(error_multi < initial_error) {
					 final_point = point_multi;
					 final_error = error_multi;
					 status_final = status_multi;
				 }
				 else {
					 final_point = initial_point;
					 final_error = initial_error;
					 status_final = status_pairwise;
				 }
			 }
			 else if(status_pairwise == 1 && status_multi == 0) {
	 			 final_point = point_multi;
				 final_error = error_multi;
				 status_final = status_multi;
			 }
			 else if(status_pairwise == 0 && status_multi == 1) {
				 final_point = initial_point;
				 final_error = initial_error;
				 status_final = status_pairwise;
			 }
			 
			 // Set the final point, if one was successfully produced

			 if(status_final == 0) {
				 avgGroundPoints[i] = final_point;
	        	 badTracks[i] = 0;  //ok, it's a valid track
			 }
		  }
		  // **********************************************************
		  // Original triangulation:
		  // **********************************************************
		  else {
		      for (int j=0; j<(currentTrackSize-1); j++) {
		         for (int k=(j+1); k<currentTrackSize; k++) {

		            if (lookVector[j] % lookVector[k] <= parallel_limit) {

		               // Triangulate and get XYZ
		               int status = xvector(lookOrigin[j], lookOrigin[k],
		                                lookVector[j], lookVector[k],
		                                xyz, &err);

		               // If triangulation failed, skip to next pair
		               if (status != 0) 
		                  continue;

		               // Check if triangulated XYZ is in front of 
					   // first camera.
		               // If erroneous tiepoints, XYZ could end up
					   // behind cam.
		               los = xyz - lookOrigin[j];
		               los.normalize();
		               dotVal = los.getX() * lookVector[j].getX() +
		                        los.getY() * lookVector[j].getY() +
		                        los.getZ() * lookVector[j].getZ();

		               if (dotVal < -epsilon) {
		                  continue;
		               }
		               else {  
		                  // Check with second camera
		                  los = xyz - lookOrigin[k];
		                  los.normalize();
		                  dotVal = los.getX() * lookVector[k].getX() +
		                           los.getY() * lookVector[k].getY() +
		                           los.getZ() * lookVector[k].getZ();

		                  if (dotVal < -epsilon) {
		                     continue;
		                  }
		               }                   
		               if (err < epsilon)
		                  err = epsilon;
		               sum_xyz += xyz/err;
		               sumOfPointWeights += 1.0/err;
		            }
		         }
		      }


		      // If there was at least one successful triangulation,
			  // save the estimated average ground point coordinates 
			  // and flag the current track as valid

		      if (sumOfPointWeights != 0.0) {
		         avgGroundPoints[i] = sum_xyz/sumOfPointWeights;
		         badTracks[i] = 0;  //ok, it's a valid track
		      }
		  }

       }


       // If there are Ground Points that could not be computed because rays 
       // were too "parallel" (flagged as bad track), we remove them from the 
       // track list and ground point list. Note that track list and ground 
       // point list must be maintained concurrently as they have a one-to-one 
       // relationship. 
       // The removal is done in a backward way (from large indices to small) to
       // keep indicing coherent in the track and ground points vector while 
       // removing.

       count = 0;
       for (i=tracks.size()-1; i>=0; i--) {
          if (badTracks[i]) {	
             tracks.erase(tracks.begin()+i);
             avgGroundPoints.erase(avgGroundPoints.begin()+i);
             count++;
          }
       }
       
       if (count) {
          sprintf(msg, "%d tracks removed for bad triangulation (parallel rays, XYZ behind camera, or high errors)", 
                 count);
          zvmessage(msg,"");
       }

       // If output of visual residual 
       // Draw in red the remaining tiepoints. These are the tiepoints that are
       // part of the solution at the BEGINING of the process. That is, part of 
       // the first bundle, without accounting for any iterative removal of 
       // tiepoints. 
       if (visualResidual && !remove_loops)
          drawTieLocation(tracks, mapOfPoints, Colors::RED, tpImgs);

       // In the following, we extract some information on the tracks topology.
       // This is where we assess if the multi-view geometry is well 
       // constrained, if image or groups of images are disconnected between 
       // each other, etc. We are extracting:
       // - information on tracks number and length
       // - number of observations per image
       // - images connectivity
       int connectivity[nids][nids];
       int tracksSize[nids];
       int obsPerImage[nids];
       // Arrays and variable initialization
       nbObservations = 0;
       for (i=0; i< nids; i++) {
          tracksSize[i] = 0;  
          obsPerImage[i] = 0;
          for (j=0; j<nids;j++)
             connectivity[i][j] = 0;
       }  

       // Scan the tracks and store relevant information
       for (i=0; i<tracks.size(); i++) {
          int currentTrackSize = tracks[i].size();

          // Cumulate for total number of observations
          nbObservations += currentTrackSize;

          // Cumulate the number of N-observations tracks. The maximum track 
          // length is by construction equals to the number of images
          tracksSize[currentTrackSize-1] += 1;

          for (j=0; j<currentTrackSize; j++) {
             it1 = mapOfPoints.find(tracks[i][j]);
             if (it1 != mapOfPoints.end())
                // Cumulate the number of observations for each image
                obsPerImage[(it1->second).imageIndex] += 1; 
          
             for (int k=0; k<currentTrackSize; k++) {
                if (j==k)
                   continue;
                it2 = mapOfPoints.find(tracks[i][k]);
                if (it2 != mapOfPoints.end())
                   // Cumulate the connectivity between images
                   connectivity[(it1->second).imageIndex]
                               [(it2->second).imageIndex] += 1;
             }
          }
       }


       // Identify if some images or group of images are disconnected from each 
       // other. This is inferred from the connectivity matrix. Despite apparent
       // simplicity, this requires the Union-Find algorithm to be run:
       // 1- Reformat the connectivity matrix in a format compliant for the 
       // union_find function. That is, we create a list of artificial pairs
       // (equivalent to tiepoints pair) of image IDs. A pair of image ID is 
       // created if the two images are connected. To account for the 
       // possibility of isolated image (i.e., image not connected with any 
       // other) we also create for each image a "self" pair, i.e. 
       // (image i, image i).
       // 2 - Run the union-find function. The output is a vector of group of 
       // images ID that are connected trough the tracks.

       // First, format image connectivity
       vector<vector<int> > imgTracks;
       for (i=0; i<nids; i++) {
          imgTracks.push_back({i,i}); // adding "self" pair
          for (j=0; j<nids; j++) {
             if (connectivity[i][j])
                imgTracks.push_back({i,j});
          }
       }

       // Second, run union-find algorithm
       vector<vector<int> > imgGroups = union_find(imgTracks);




       // Finally, displaying the information...

       // Print in stdout the number of unique observations
       zvmessage(" ","");
       sprintf(msg, "%d unique observations in image list", nbObservations);
       zvmessage(msg, "");

       if (nbObservations == 0) {
          zvmessage("No observation, nothing to do, returning...", "");
          return;
       }


       // Print in stdout statistics on size of tracks
       zvmessage(" ", " ");
       zvmessage("Tracks statistics:", "");
       for (i=0; i<nids; i++) {
          if (tracksSize[i] == 0)
             continue;
          sprintf(msg, "%7d  %d-observations tracks (%g %% of total tracks)",
                  tracksSize[i], i+1, 
                  (double)tracksSize[i] / (double)tracks.size() * 100.0);
          zvmessage(msg, "");
       }


       // Print in stdout the number of observations for each image
       zvmessage(" ", " ");
       zvmessage("Images observations:","");
       zvmessage("Image key   Nb observations","");
       for (i=0; i<nids; i++) {
          sprintf(msg, "%5d %13d", i+1, obsPerImage[i]);
          zvmessage(msg," ");
       }


       // Print in stdout the connectivity matrix
       // The connectivity matrix won't print out nicely if nids is large.
       // Key info is printed out with group connection anyway (after the
       // matrix printing)

       // Get the largest connectivity number
       int maxConnection = 0;
       for (i=0; i<nids; i++) {
          for (j=0; j<nids; j++){
             if (i == j)
                continue;
             if (maxConnection < connectivity[i][j])
                maxConnection = connectivity[i][j];
          }
       }

       // We want the largest number between number of image and largest number
       // of connection for screen printing formatting purposes
       if (maxConnection < nids)
          maxConnection = nids;
       
       // For printing spacing
       int dispLength = std::to_string(maxConnection).length();


       // Add +1 space to avoid largests printed numbers to be collated
       dispLength++;

       size_t numMsgChars = (nids+1)*dispLength+1;
       char * msg2 = new (std::nothrow) char[numMsgChars];
       if (msg2 == NULL) {
          zvmessage("Memory allocation error during msg2 init.","");
          zabend();
       }

       char *pos = msg2;

       if (connectivity_disp) {

          zvmessage(" ", " ");
          zvmessage("Images connectivity:","");
          pos += snprintf(pos,numMsgChars - (pos-msg2),"%*s",dispLength," ");
          for (i=0; i<nids && ((pos-msg2) < numMsgChars); i++)
          {
             pos += snprintf(pos,numMsgChars - (pos-msg2), "%*d", dispLength,i+1);
          }
          zvmessage(msg2," ");

          pos = msg2;

          for (i=0; i<nids; i++) {
             pos += snprintf(pos,numMsgChars - (pos-msg2),"%*d", dispLength,i+1);
             for (j=0; j<nids && ((pos-msg2) < numMsgChars); j++) {
                if (i ==j){                   
                   pos += snprintf(pos,numMsgChars - (pos-msg2),"%*s",dispLength,"-");
                }
                else {
                   if (connectivity[i][j] == 0){
                     pos += snprintf(pos,numMsgChars - (pos-msg2),"%*s",dispLength,".");
                   }
                   else 
                   {
                     pos += snprintf(pos,numMsgChars - (pos-msg2), "%*d", dispLength,connectivity[i][j]);
                   }
                }
             }   
             zvmessage(msg2," ");
             pos = msg2;
          }
       }

       delete [] msg2;
      

       // Print in stdout statistics if there are image or group of
       // images not connected. All images should be connected for a good
       // bundle adjustment.
       numMsgChars = ((std::to_string(nids).length())+2)*nids+1;
       msg2 = new (std::nothrow) char[numMsgChars];

       zvmessage(" ", " ");
       if (imgGroups.size() == 1)
          zvmessage("All images are connected","");
       else {
          zvmessage("There are disconnected groups of images (one group per line).","");
          for (i=0; i<imgGroups.size(); i++) {
             pos=msg2;
             for (j=0; j<imgGroups[i].size() && ((pos-msg2) < numMsgChars); j++) {
                if (j) {
                   pos += snprintf(pos,numMsgChars - (pos-msg2),", ");
                }
                pos += snprintf(pos,numMsgChars - (pos-msg2), "%d", imgGroups[i][j]+1);
             }
             zvmessage(msg2, "");
          }
       }

       delete [] msg2;

       // Done printing statistics, moving on to building and solving the 
       // bundle adjustment problem...


       // Initialize arrays that will store all the tie points information to be
       // passed to Ceres solver
       observationsXY = new (std::nothrow) double[2 * nbObservations];
       observationsGroundPointID = new (std::nothrow) int[nbObservations];
       observationsCameraID = new (std::nothrow) int[nbObservations];
       sitesPointingsParams = new (std::nothrow) double[num_sites * 6];
       sitesPointingsParamsErr = new (std::nothrow) double[num_sites * 6];
       groundPointsParams = new (std::nothrow) double[tracks.size() * 3];
       pointingsParams = new (std::nothrow) double[nids * PIG_MAX_PARAMS];
       pointingsParamsErr = new (std::nothrow) double[nids * PIG_MAX_PARAMS];
       numPointingParams = new (std::nothrow) int[nids];
    
       if ((observationsXY == NULL) || (observationsGroundPointID ==NULL) || 
          (observationsCameraID == NULL) || (groundPointsParams ==NULL) ||
          (pointingsParams ==NULL) || (sitesPointingsParams == NULL) ||
           numPointingParams == NULL) {
             zvmessage("Memory allocation error during Bundle Adjustment initialization","");
             zabend();
       }
   

       // Filling the arrays with relevant information for Ceres 
       index = 0;
       for (i=0; i<tracks.size(); i++) {
          // Storing Ground Points coordinates
          groundPointsParams[i*3+0] = avgGroundPoints[i].getX();
          groundPointsParams[i*3+1] = avgGroundPoints[i].getY();
          groundPointsParams[i*3+2] = avgGroundPoints[i].getZ();

          // Storing observations pixel coordinates, and associated
          // image key and Ground Point index
          for(j=0; j<(tracks[i]).size(); j++){
             it1 = mapOfPoints.find(tracks[i][j]);
             if(it1 != mapOfPoints.end()){
                observationsXY[index*2+0]=(it1->second).sample;
                observationsXY[index*2+1]=(it1->second).line;
                observationsCameraID[index] = (it1->second).imageIndex;
                observationsGroundPointID[index]=i;
                index++;
             }  
          }
       }



       // Get the pointing parameters of the images and fill arrays for Ceres
       // Idem with Sites pointing parameters.
       // TO DO: Instead of PIG_MAX_PARAMS, get actual value for each image?
       double p[PIG_MAX_PARAMS];      // For pointings parameters
       double perr[PIG_MAX_PARAMS];
       double pp[6];                  // For sites parameters. Hard-coded as
       double pperr[6];               // with marsnav. Should be parmetrized.
       zvmessage("","");
       zvmessage("Nominal Pointing Errors:", "");
       std::set<std::string> alreadyDisplayed;
       for (i=0; i<nids; i++) {
          // Get Image/Camera pointing parameters and store them in array
	  int n = (pointing_in_all[i])->getPointingParamCount();
          numPointingParams[i] = n;
	  (pointing_in_all[i])->getPointingParameters(p, n);
	  (pointing_in_all[i])->getPointingErrorEstimate(perr, n);

          for (j=0; j<n;j++) {
             pointingsParams[i*PIG_MAX_PARAMS+j]=p[j];
             pointingsParamsErr[i*PIG_MAX_PARAMS+j]=perr[j];
          }
 


          // Get Sites pointing parameters and store them in array
          site_list[filesSites[i]].site->getPointingParameters(pp,6);
          site_list[filesSites[i]].site->getPointingErrorEstimate(pperr,6);

          for (j=0; j<6; j++) {
             sitesPointingsParams[filesSites[i]*6+j]=pp[j];
             sitesPointingsParamsErr[filesSites[i]*6+j]=pperr[j];
          }


          // Print camera pointing error for information
          char *pos = msg;
          const char *name = file_models[i]->getInstrumentId();
          auto res = alreadyDisplayed.insert(std::string(name));
          if (res.second) {
             pos += sprintf(pos, "%s: ", name);
             for (int j=0; j<n; j++) 
                pos += sprintf(pos, "%8.4f (%s)  ", perr[j], pointing_in_all[i]->getPointingParamName(j));
             zvmessage(msg, "");
          }

       }

       // Update pointing error if user manually entered them
       // Note: see pdf comment about the hackiness of this, and it's eventual
       // deprecate-ness
       if (numManPointErrors) {
          for (int i=0; i<nids; i++) {
             for (int j=0; j<numPointingParams[i]; j++) {
                pointingsParamsErr[i*PIG_MAX_PARAMS+j]=manPointErrors[j];
             }
          }

          zvmessage("Pointing errors manually overwritten with:", "");
          char *pos = msg;
          for (int i=0; i<numManPointErrors; i++) 
             pos += sprintf(pos, "%8.4f  ", manPointErrors[i]);
          zvmessage(msg, "");

       }




 
       zvmessage("","");




       // Ceres Solver initialization - this is were the bundle adjustment
       // problem (Non-linear least square optimization) is built in ceres.
       // Each observation is added to the "problem":
       // - First an observation (pixel coord, camera, pointing, site,...) is 
       // associated to a residual function. The "association" results in a cost
       // function. There are as many cost funcions initialized as there are 
       // observations.
       // - Second this cost function is associated to a set of parameters it 
       // depends on and that are to be optimized.

       ceres::Problem problem(optionsProblem);

 
       for (i=0; i<nbObservations; ++i) {

          // First...
          ceres::CostFunction* cost_function =
             traditionalReprojectionError::Create(
                observationsXY[2 * i + 0],
                observationsXY[2 * i + 1],
                pointing_in_all[observationsCameraID[i]],
                numPointingParams[observationsCameraID[i]],
                camera_in_all[observationsCameraID[i]],
                site_list[filesSites[observationsCameraID[i]]].site, 
                solution_id, 
                output_cs);
       
 
          // If cost function is invalid, the observation may be discarded and 
          // the process may be continued (with proper handling of variables).
          // However this should not happen, so for now, we simply quit the 
          // program.
          if (cost_function == NULL) {
             zvmessage("Invalid cost function. Internal error","");
             zabend();
          }

 
          // Second...
          problem.AddResidualBlock(cost_function, loss_function,
             groundPointsParams + observationsGroundPointID[i]*3,
             pointingsParams + observationsCameraID[i]*PIG_MAX_PARAMS,
             sitesPointingsParams + filesSites[observationsCameraID[i]]*6,
             sitesPointingsParams + filesSites[observationsCameraID[i]]*6 + 3);
       }


       // Here, the parameter prior (e.g., INERTIA) should be taken care of.
       // For now, a normal prior is implemented. The center of the prior is 
       // set to the initial estimates (quite an assumption), and the deviation
       // is set to the *pointingParamsError* retrieved from the PigPointing.
       //
       // NOTE: It is critical that the pointingParamsError correctly estimates
       // the actual pointing error. Otherwise, too much/less weight the prior
       // can force the solution to be non-optimal.
       if (inertia) {
          for (i=0; i<nids; i++) {
             if (problem.HasParameterBlock(pointingsParams + i*PIG_MAX_PARAMS)) {
                int nbParams = numPointingParams[i];
                StiffnessMat stiffness = StiffnessMat::Zero(nbParams,nbParams);
                PointingVect pointingVect = PointingVect::Zero(nbParams);
                for (j=0; j<nbParams; j++) {
                   index = i*PIG_MAX_PARAMS+j;
                   if (pointingsParamsErr[index] == 0.0)
                      break;
                   stiffness(j,j) = 1.0/(pointingsParamsErr[index]) * inertia;
                   pointingVect(j) = pointingsParams[index];
                }

                if (stiffness(nbParams-1,nbParams-1) == 0.0) {
                   sprintf(msg,"Image %d has null pointing error - No INERTIA applied", i);
                   zvmessage(msg,"");
                   continue;
                }

                // Initialize the Normal Prior cost function
                ceres::NormalPrior* normalPriorCost_function = 
                                new ceres::NormalPrior(stiffness, pointingVect);

                // Check that the cost function was properly constructed
                // If it is not, then print a message and skip that image
                if (normalPriorCost_function == NULL) {
                   sprintf(msg,"Invalid Normal Prior cost function for image %d", i);
                   zvmessage(msg,"");
                   continue;
                }

                // Add a residual block to the problem 
                problem.AddResidualBlock(normalPriorCost_function, NULL,
                                         pointingsParams + i*PIG_MAX_PARAMS);
             }
          }
       }
 

       // Now that the problem is build, we set up any specific options required
       // by the user. For instance parameters we want to maintain constant
       // (e.g., reference image) or bounds on pointing parameters, etc...
       //
       // Note that to setup pointing parameters constant or setup its bounds, 
       // the parameters have first to be added to the ceres problem. An 
       // image is not added to the problem if it does not have observation 
       // (which can happen, e.g., because of IGNORE parameter or if all 
       // tie-points to an image have been discarded when filtering the tracks).
       // In that case we can't set the constant or bounds on the pointing 
       // parameters as they have not been added to the ceres problem. Check
       // first, otherwise ceres will throw an error. 
       double bound;
       for (i=0; i<nids; i++) {
          if (problem.HasParameterBlock(pointingsParams + i*PIG_MAX_PARAMS)) {
             if (refimg[i] || !adjust_pointing)
                problem.SetParameterBlockConstant(
                                            pointingsParams + i*PIG_MAX_PARAMS);
             else if (nsigma_pointings){
                for (j=0; j<numPointingParams[i]; j++) {
                   index = i*PIG_MAX_PARAMS;
                   bound = nsigma_pointings*pointingsParamsErr[index+j];
                   problem.SetParameterLowerBound(pointingsParams+index, j,
                                            pointingsParams[index+j] - bound);
                   problem.SetParameterUpperBound(pointingsParams+index, j,
                                            pointingsParams[index+j] + bound);
                }
             } 
          }
       }


       // Set Sites LOCATION and ORIENTATION constant or not depending on the 
       // user input parameters. Set up Upper/Lower bounds on the parameters
       // if required
       for (i=0; i<num_sites; i++) {
          if (problem.HasParameterBlock(sitesPointingsParams + i*6)) {
             if (!site_list[i].adjust_location) {
                 problem.SetParameterBlockConstant(sitesPointingsParams + i*6);
             }
             else if (nsigma_sites){
                for (j=0; j<3; j++) {
                   bound = nsigma_sites*sitesPointingsParamsErr[i*6+j];
                   problem.SetParameterLowerBound(sitesPointingsParams+i*6, j,
                                           sitesPointingsParams[i*6+j] - bound);
                   problem.SetParameterUpperBound(sitesPointingsParams+i*6, j,
                                           sitesPointingsParams[i*6+j] + bound);
                }
             }

             if (!site_list[i].adjust_orientation) {
                 problem.SetParameterBlockConstant(sitesPointingsParams + 
                                                                       i*6 + 3);
             }
             else if (nsigma_sites) {
                for (j=0; j<3; j++) {
                   bound = nsigma_sites*sitesPointingsParamsErr[i*6+3+j];
                   problem.SetParameterLowerBound(sitesPointingsParams+i*6+3, j,
                                         sitesPointingsParams[i*6+3+j] - bound);
                   problem.SetParameterUpperBound(sitesPointingsParams+i*6+3, j,
                                         sitesPointingsParams[i*6+3+j] + bound);
                }
             }

          }
       } 
    
       // Set Ground Points constant (via DO_XYZ)
       // Normally all ground points have been added to problem by construction,
       // so no need to check their existence in problem (?)
       if (!adjust_surface) {
          for (i=0; i<tracks.size(); i++) {
             problem.SetParameterBlockConstant(groundPointsParams+i*3);
          }
       }
       
       //Set XYZ constant for fiducial points. Unless asked otherwise by
       //user. Fiducial points are 1-element tracks.
       for (i=0; i<tracks.size(); i++) {
          if (tracks[i].size() == 1 && !opti_fiducial)
             problem.SetParameterBlockConstant(groundPointsParams+i*3);
       }


       // Display the residual distribution BEFORE bundle. This could be useful
       // to detect the presence of gross outliers (assuming initial parameters
       // are good) and possibly remove them before they make a mess of the 
       // bundle
       if (numBin > 0) {

          vector<double> residualArray(nbObservations*2);
          double residualTotal = 0.0;
          problem.Evaluate(optionsEval, &residualTotal, &residualArray, NULL, NULL);


          // Save tracks on first loop for Extended tiepoints
          initial_tracks= tracks;

          // If output of visual residual 
          // Draw the tiepoints residual vector prior (in blue) any adjustment.
          // Essentially, these are pre-bundle residual.
          if (visualResidual && !remove_loops)
              drawTieResidual(tracks, mapOfPoints, residualArray, visualScaler, 
                              Colors::BLUE, tpImgs);

          // Format residual to allow tracking image ids
          auto residualPairs = getResidualPairs(tracks, residualArray);

          // Sort the residuals in decreasing order (from worst to best)
          std::sort(residualPairs.begin(), residualPairs.end(), 
                               [](const std::pair<double, std::vector<int> > &a,
                                  const std::pair<double, std::vector<int> > &b)
                                  {return a.first > b.first;});

          // Compute the residual histogram
          double binW = residualPairs.front().first / double(numBin);
          auto histogram = getResidualHistogram(residualPairs, binW);

          // Display the residual histogram
          zvmessage("Pre-bundle adjustment Residual distribution","");
          sprintf(msg, "Residual range : Cumul hist : Count");
          zvmessage(msg, "");
          double cumul = 0;
          for (const auto& x : histogram) { 
             cumul += x.second;
             sprintf(msg, "[%.2f,%.2f) : %6.2f% : %d", x.first, x.first + binW, 
                                       cumul * 100. / nbObservations, x.second);
             zvmessage(msg, "");
          }
       

          // This part is for saving extended Tie Points if requested.
          // This is the "initial" residual (first loop, if looping with points
          // removal)
          if (extendedTP && !remove_loops ) {
             long index = 0;
             for (int i=0; i<tracks.size(); i++) {
                for (int j=0; j<(tracks[i]).size(); j++) {
                   it1 = mapOfPoints.find(tracks[i][j]);
                   if (it1 != mapOfPoints.end()) {
                     (it1->second).init_residual_sample = residualArray[2*index]; 
                     (it1->second).init_residual_line   = residualArray[2*index+1]; 
                     (it1->second).init_xyz             = PigPoint(groundPointsParams[3*i],
                                                                   groundPointsParams[3*i+1], 
                                                                   groundPointsParams[3*i+2]); 
                     (it1->second).track = i+1; //1-based clearer 

                   }
                index++;
                }
             }
          }
         


          // This is for debug and investigation. If required by user, save bad
          // tiepoints to a file and exit
          if (outBadTies > 0) {

             sprintf(msg, "Saving all the tiepoints whose residual (pre-bundle) are larger than %f pixels", permitted_error);
             zvmessage(msg, "");

             int badTpCount = 0;
             std::vector<TiePoint> badTies;
             for (const auto &e: residualPairs) {
                if (e.first > max_residual) {
                   for (j=0; j<tracks[e.second[0]].size()-1; j++) {
                      TiePoint tie;
                      it1 = mapOfPoints.find(tracks[(e.second)[0]][j]);
                      it2 = mapOfPoints.find(tracks[(e.second)[0]][j+1]);
                      if (it1 != mapOfPoints.end() && it2 != mapOfPoints.end()) {
                         tie.type = TIEPOINT_TRADITIONAL;
                         tie.left_image = (it1->second).imageIndex;
                         tie.right_image = (it2->second).imageIndex;
                         tie.left_key = (it1->second).imageIndex;
                         tie.right_key = (it2->second).imageIndex;
                         tie.left_sample = (it1->second).sample;
                         tie.left_line = (it1->second).line;
                         tie.right_sample = (it2->second).sample;
                         tie.right_line = (it2->second).line;
                         tie.corrected_sample = (it2->second).sample;
                         tie.corrected_line = (it2->second).line;
                         tie.quality = (it1->second).quality;
                         tie.interactive = (it1->second).interactive;
                         tie.active = 1;
                         if (extendedTP) {
                            tie.nav_residual = 1;
                            tie.left_init_residual_sample =  (it1->second).init_residual_sample;
                            tie.left_init_residual_line =  (it1->second).init_residual_line;
                            tie.left_final_residual_sample =  (it1->second).final_residual_sample;
                            tie.left_final_residual_line =  (it1->second).final_residual_line;
                            tie.right_init_residual_sample = (it2->second).init_residual_sample;
                            tie.right_init_residual_line = (it2->second).init_residual_line;
                            tie.right_final_residual_sample = (it2->second).final_residual_sample;
                            tie.right_final_residual_line = (it2->second).final_residual_line;
                            tie.init_miss_dist = 0;
                            tie.final_miss_dist = 0;
                            tie.init_xyz = (it1->second).init_xyz;
                            tie.final_xyz = (it1->second).final_xyz;
                            tie.track =  (it1->second).track;
                         }
                         badTies.push_back(tie);
                      }
                   }
                }
             }

             status = mars_save_tiepoints(out_bad_ties, badTies.data(), 
                       badTies.size(), file_models, nids, start_key, output_cs);

             zvmessage("Exiting program after saving Bad Tiepoints in file", "");
             zabend();
          }
       }



       // This is the core of the program, the bundle adjustment optimization
       zvmessage(" ","");
       zvmessage("Solving for the Bundle Adjustment...","");
       ceres::Solver::Summary summary;
       ceres::Solve(optionsSolver, &problem, &summary);


       // In case something bad happen, save the result
       for (i=0; i<nids; i++) {
          int n = numPointingParams[i];
          (pointing_in_all[i])->setPointingParameters(pointingsParams + 
                                                          i*PIG_MAX_PARAMS, n);
       }
       save_pointing(xml_mode, outfilename, nids, pointing_in_all,
          pparams_in_save, file_models, surface_model, solution_id, originalCM);


       // Printing in stdout a report on the optimization run
       if (summary_disp != "NO_SUM") {
          string report;
          if (summary_disp == "BRIEF_SUM")
             report = summary.BriefReport();
          else 
             report = summary.FullReport();

          char * cstr = new char[report.length()+1];
          std::strcpy(cstr, report.c_str());
          zvmessage(" ","");
          zvmessage(cstr, "");
          delete [] cstr;
       }




       // Retrieving observation residuals and final total cost after 
       // optimization
       vector<double> residualArray(nbObservations*2);
       double residualTotal = 0.0;
       problem.Evaluate(optionsEval, &residualTotal, &residualArray, NULL, NULL);


       // Computing residual magnitude in pixel for each observation and 
       // associating it to observation ID for later sorting of the 
       // observations per residual magnitude.
       auto residualPairs = getResidualPairs(tracks, residualArray);

       // Sort the residuals in decreasing order (from worst to best)
       //std::sort(residualPairs.begin(), residualPairs.end(), sortByResidual);
       std::sort(residualPairs.begin(), residualPairs.end(), 
                               [](const std::pair<double, std::vector<int> > &a,
                                  const std::pair<double, std::vector<int> > &b)
                                  {return a.first > b.first;});
 

      // Inform user that the residuals, total cost, and mean cost that are 
      // going to be print out on stdout have the loss function applied to 
      // them or not.
      zvmessage(" ","");
      if (optionsEval.apply_loss_function)
         zvmessage("Loss function applied to residuals, total, mean and median cost:","");
      else
         zvmessage("Loss function not applied to residuals, total, mean and median cost:","");

      // Display the N worst observations along with Image ID, sample, line
      if (residual_disp != 0) {
         int nbPointsDisplay = residual_disp;
         if ((residual_disp < 0) || (residual_disp > nbObservations))   
            nbPointsDisplay = nbObservations;

         sprintf(msg, "The %d largest observation residuals are:", nbPointsDisplay);
         zvmessage(msg,"");
         zvmessage("Residual (pix)   Image #   Sample      Line      Nb observations in track","");
         for (i=0; i<nbPointsDisplay; i++) {
            string maxResidualID = tracks[((residualPairs[i]).second)[0]]
                                         [((residualPairs[i]).second)[1]];

            it1 = mapOfPoints.find(maxResidualID);
            if (it1 != mapOfPoints.end()) {
               sprintf(msg, "%14.3f %6d %11.3f %11.3f %16ld",
                       residualPairs[i].first, (it1->second).imageIndex+1, 
                       (it1->second).sample, (it1->second).line, 
                       tracks[(residualPairs[i].second)[0]].size());
               zvmessage(msg, "");     
            }
         }
      }
      

      sprintf(msg, "Solution total cost: %f (pixels)", residualTotal);
      zvmessage(msg, "");

      vector<double> forStat(residualPairs.size());
      for (i=0; i<residualPairs.size(); i++)
         forStat[i] = (residualPairs[i]).first;
      
      // Compute mean residual
      double tot = 0.0;
      for (i=0; i<residualPairs.size(); i++)
         tot += (residualPairs[i]).first;
      sprintf(msg, "Solution mean error: %f (pixels)", 
                                             tot/(double)residualPairs.size());
      zvmessage(msg, "");

      // Compute median residual
      std::sort(forStat.begin(), forStat.end());
      sprintf(msg, "Solution median error: %f (pixels)", 
                                             forStat[residualPairs.size()/2]);
      zvmessage(msg, "");


      // Display the residual distribution AFTER bundle. 
      if (numBin > 0) {
         double binW = residualPairs.front().first / numBin;
         auto histogram = getResidualHistogram(residualPairs, binW);
         zvmessage(" ", "");
         zvmessage("Post-bundle adjustment Residual distribution","");
         sprintf(msg, "Residual range : Cumul hist : Count");
         zvmessage(msg, "");
         double cumul = 0;
         for (const auto& x : histogram) { 
            cumul += x.second;
            sprintf(msg, "[%.2f,%.2f) : %6.2f% : %d", x.first, x.first + binW, 
                    cumul * 100. / nbObservations, x.second);
            zvmessage(msg, "");
         }
      }




      // Print difference between original and optimized pointing parameters
      zvmessage(" ", "");
      zvmessage("Input     Change in pointing parameters (final - initial):", "");
      for (i=0; i < nids; i++) {
         char *pos = msg;
         pos += sprintf(pos, "%3d ", i+1);
         for (j=0; j<numPointingParams[i]; j++) 
            pos += sprintf(pos, "%9f (%s) ", pointingsParams[i*PIG_MAX_PARAMS+j] - pparams_in_save[i][j], 
                                             pointing_in_all[i]->getPointingParamName(j));
         zvmessage(msg, "");
      }



      // Now, depending on User input, remove worst points and recompute a BA, 
      // or move on to save optimization result
      zvmessage(" ", "");

      if ((residualPairs[0]).first > permitted_error) {

         redo_solution = TRUE;  // redo it after tiepoint deleted

         // Delete worst point(s) and do a new BA, etc. until we get 
         // small residual, or run out of points, or gets to the 
         // point removal number limit.

         if (!remove_flag) {	// Don't remove; quit
            redo_solution = FALSE;
         }
         else if ((max_remove != 0) && (remove_loops >= max_remove)) {
            redo_solution = FALSE;	// too many loops
            zvmessage("Reached max_remove limit on remove loops","");
         }
         else {
            remove_loops++;
            if (max_residual == 0.0) { // i.e., just remove the worst obs.
               index=0;
               if (!remove_fiducial) {
                  while (1) {
                     if (index > (nbObservations-1)) {
                        index=-1; // all points are fiducials. Nothing to do.
                        break;
                     }
                     if (tracks[(residualPairs[index].second)[0]].size() != 1)
                        break;
                     index++;
                  }
               }

               if (index == -1) {
                  redo_solution = FALSE;
                  continue;
               }

               // Remove the worst residual
               int i = (residualPairs[index].second)[0];
               int j = (residualPairs[index].second)[1];
               vector<string> *track = &tracks[i];
               track->erase(track->begin()+j);
               if (track->size() < 2)
                  track->clear();
            }
            else {  // Remove all residual above MAX_RESIDUAL
               count=0;
               int count2=0; //To keep track of fiducial with residual > max
               while (residualPairs[count].first >= max_residual){
                  if (tracks[(residualPairs[count].second)[0]].size() == 1)
                     count2++; 
                  count++;
               }

               // Get the actual number of observations to be removed, that is
               // number of observation with large residual - number of fiducial
               // observations with large residual (if fiducial are not
               // removable) 
               // store number in count2
               if (!remove_fiducial)
                  count2 = count - count2;
               else count2 = count; 

               if (count2 != 0) {
                  sprintf(msg,"%d observations to be removed for next iteration.",count2);
                  zvmessage(msg,"");
                  
                  for (int k=0; k<count; k++) {
                     int i = (residualPairs[k].second)[0];
                     int j = (residualPairs[k].second)[1];
                     vector<string> *track = &tracks[i];
                     
                     // Case for fiducial points. Remove point if allowed
                     // by user
                     if (track->size() == 1) {
                        if (remove_fiducial) 
                           track->clear();
                     }
                     // Other cases. Remove observation and if there are less
                     // than 2 elements remaining after the removal, empty the
                     // entire track as no possible triangulation will be 
                     // possible in next loop.
                     else if (!track->empty()) {
                        track->erase(track->begin()+j);
                        if (track->size() < 2)
                           track->clear();
                     }                       
                  }
               }
               else
                  redo_solution = FALSE;
            }      
            
            // Now that observations with bad residual have been removed, 
            // we remove the tracks which are empty
            for (i=tracks.size()-1; i>=0; i--) {
               if (tracks[i].empty()) {
                  avgGroundPoints.erase(avgGroundPoints.begin()+i);
               }
            }

            if (tracks.empty()) {	// nothing to remove!
               redo_solution = FALSE;	// (probably can't happen?)
               zvmessage("No more observations left! Returning...","");
            }
         }
     }

     // If redo_solution, we initialize the pointings back to their original
     // values and re-run the optimization. If no redo_solution, the pointings
     // are updated with the optimization results and we move on to saving
     // the outcome to files.
     for (i=0; i<nids; i++) {
        int n = numPointingParams[i];
        if (redo_solution)
           (pointing_in_all[i])->setPointingParameters(&pparams_in_save[i][0],n);
        else
           (pointing_in_all[i])->setPointingParameters(pointingsParams + 
                                                          i*PIG_MAX_PARAMS, n);
     }

     if (adjust_sites) {
        for (i=0; i<num_sites; i++) {
           int n = 6;
           if (redo_solution)
              site_list[i].site->setPointingParameters(
                                                  &sitesParams_in_save[i][0],n);
           else
              site_list[i].site->setPointingParameters(
                                                 sitesPointingsParams + i*n, n);
        }
     }


     // If output of visual residual 
     // Save residual of last loop if visual residual display
     if (!redo_solution) {
        finalResidual = residualArray; 
        finalXYZ = new double[3 * tracks.size()]; 
        std::memcpy(finalXYZ, groundPointsParams, 3*tracks.size()*sizeof(double));
     }


     delete [] observationsXY;
     delete [] observationsGroundPointID;
     delete [] observationsCameraID;
     delete [] sitesPointingsParams;
     delete [] sitesPointingsParamsErr;
     delete [] groundPointsParams;
     delete [] pointingsParams;
     delete [] pointingsParamsErr;
     delete [] numPointingParams;





   } // end while loop



   // Save the pointings to file - This is the main output of the program   
   save_pointing(xml_mode, outfilename, nids, pointing_in_all,
          pparams_in_save, file_models, surface_model, solution_id, originalCM);


   // If Sites have been adjusted, print the new sites pointings on the stdout
   // as with standard marsnav
   if (adjust_sites)
      print_sites(site_list, num_sites);
  


   // This part is for saving extended Tie Points if requested.
   // This is the "final" residuals
   if (extendedTP) {
      long index = 0;
      for (int i=0; i<tracks.size(); i++) {
         for (int j=0; j<(tracks[i]).size(); j++) {
            it1 = mapOfPoints.find(tracks[i][j]);
            if (it1 != mapOfPoints.end()) {
               (it1->second).final_residual_sample = finalResidual[2*index]; 
               (it1->second).final_residual_line   = finalResidual[2*index+1]; 
               (it1->second).final_xyz             = PigPoint(finalXYZ[3*i],
                                                              finalXYZ[3*i+1], 
                                                              finalXYZ[3*i+2]); 
            }
            index++;
         }
      }
   }


 

   // If required by the user, save the tie points. However, because the 
   // structure of pairwise tie points from the original files has been lost 
   // when building the tracks, we reconstruct an artificial tie points
   // file that represents the tracks. To do that, observations from the same
   // tracks are re-paired. For instance, if one track has 4 observations
   // 1,2,3,4, we are making the following pairs (1,2), (2,3), and (3,4).
   // Note that for tracks with more than 2 observations, there is no 
   // guarantee that a specific tie point in the tiepoints input file will be 
   // re-paired in the output tiepoint file.
   // All tie-points type are forced to TRADITIONAL.
   // ATTENTION: The above does not apply to FIDUCIAL, which are recorded
   // as is.
   // TO DO: case of dynamic tie points

   // Compute the number of (artificial) traditional tiepoints + fiducial 
   // if any. Track size == 1 are fiducials, otherwise traditional tiepoints that
   // are reconstructed from track
   int nbTiePointsOut = 0;
   for (const auto &t : tracks) 
      nbTiePointsOut += (t.size() == 1) ? 1 : t.size() - 1;

   // Initialize the new tiepoints array
   TiePoint *newtiepoints = new (std::nothrow) TiePoint[nbTiePointsOut];
   if (newtiepoints == NULL) {
      zvmessage("Memory allocation error during newTiePoints array initialization","");
      zabend();
    }
 
   // Fill the array with tiepoints
   index=0;
   for (i=0; i<tracks.size(); i++) {
      if (tracks[i].size() == 1) {
         TiePoint tie;
         it1 = mapOfPoints.find(tracks[i][0]);
         if (it1 != mapOfPoints.end()) {
            tie.type = TIEPOINT_FIDUCIAL;
            tie.left_image = (it1->second).imageIndex;
            tie.left_key = (it1->second).imageIndex;
            tie.left_sample = (it1->second).sample;
            tie.left_line = (it1->second).line;
            tie.xyz = (it1->second).xyz,
            tie.quality = (it1->second).quality;
            tie.interactive = (it1->second).interactive;
            tie.active = 1;
          
            if (extendedTP) {
               tie.nav_residual = 1;
               tie.left_init_residual_sample =  (it1->second).init_residual_sample;
               tie.left_init_residual_line =  (it1->second).init_residual_line;
               tie.left_final_residual_sample =  (it1->second).final_residual_sample;
               tie.left_final_residual_line =  (it1->second).final_residual_line;
               tie.right_init_residual_sample = 0;
               tie.right_init_residual_line = 0;
               tie.right_final_residual_sample = 0;
               tie.right_final_residual_line = 0;
               tie.init_miss_dist = 0;
               tie.final_miss_dist = 0;
               tie.init_xyz = (it1->second).init_xyz;
               tie.final_xyz = (it1->second).final_xyz;
               tie.track =  (it1->second).track;
            }

            newtiepoints[index] = tie;
            index++;
         }   
      }
      else {
         for (j=0; j<tracks[i].size()-1; j++) {
            TiePoint tie;
            it1 = mapOfPoints.find(tracks[i][j]);
            it2 = mapOfPoints.find(tracks[i][j+1]);
            if (it1 != mapOfPoints.end() && it2 != mapOfPoints.end()) {
               tie.type = TIEPOINT_TRADITIONAL;
               tie.left_image = (it1->second).imageIndex;
               tie.right_image = (it2->second).imageIndex;
               tie.left_key = (it1->second).imageIndex;
               tie.right_key = (it2->second).imageIndex;
               tie.left_sample = (it1->second).sample;
               tie.left_line = (it1->second).line;
               tie.right_sample = (it2->second).sample;
               tie.right_line = (it2->second).line;
               tie.corrected_sample = (it2->second).sample;
               tie.corrected_line = (it2->second).line;
               tie.quality = (it1->second).quality;
               tie.interactive = (it1->second).interactive;
               tie.active = 1;

               if (extendedTP) {
                  tie.nav_residual = 1;
                  tie.left_init_residual_sample =  (it1->second).init_residual_sample;
                  tie.left_init_residual_line =  (it1->second).init_residual_line;
                  tie.left_final_residual_sample =  (it1->second).final_residual_sample;
                  tie.left_final_residual_line =  (it1->second).final_residual_line;
                  tie.right_init_residual_sample = (it2->second).init_residual_sample;
                  tie.right_init_residual_line = (it2->second).init_residual_line;
                  tie.right_final_residual_sample = (it2->second).final_residual_sample;
                  tie.right_final_residual_line = (it2->second).final_residual_line;
                  tie.init_miss_dist = 0;
                  tie.final_miss_dist = 0;
                  tie.init_xyz = (it1->second).init_xyz;
                  tie.final_xyz = (it1->second).final_xyz;
                  tie.track =  (it1->second).track;
               }

               newtiepoints[index] = tie;
               index++;
            }
         }
      }
   }

   status = mars_save_tiepoints(out_tptlist, newtiepoints, nbTiePointsOut, 
                             file_models, nids, start_key, output_cs);

   delete [] newtiepoints;




   // If output of visual residual 
   // Draw the final tiepoint residual in green and save the images.
   if (visualResidual) {

      zvmessage("Saving tiepoints residuals to visual display:","");
      zvmessage("Blue vectors are tiepoint residuals in pixel (scaled with SCALE_RES) before any adjustment","");
      zvmessage("Green vectors are tiepoint residuals in pixel (scaled with SCALE_RES) after adjustment","");
      zvmessage("Red tiepoints are tiepoints that are part of the first adjustment","");
      zvmessage("Cyan tiepoints are removed from the process because of poor triangulation","");
      zvmessage("Black tiepoints are removed from the process because part of track whose length is deactivated by user","");
      zvmessage("Orange tiepoints are removed from the process because part of corrupted track","");
 
      drawTieResidual(tracks, mapOfPoints, finalResidual, visualScaler, Colors::GREEN, tpImgs);

      char str[4];
      for (int i = 0; i < tpImgs.size(); i++) {
         saveImg(getFileName(file_models[i]->getFilename())+".RES", tpImgs[i]);
      }
   }



 }  //end of main





////////////////////////////////////////////////////////////////////////
// Get the residual in a form where they can be tracked back to the 
// image
////////////////////////////////////////////////////////////////////////
std::vector <std::pair<double, std::vector<int> > > getResidualPairs(
                               const std::vector<std::vector<std::string>> &tracks,
                               const std::vector<double> &residualArray) {

   int index = 0;
   std::vector<std::pair<double, std::vector<int> > > residualPairs;

   for (int i=0; i<tracks.size(); i++) {
      for (int j=0; j<(tracks[i]).size(); j++) {
         double currentResidual = sqrt(pow(residualArray[2*index+0], 2.0) +
                                       pow(residualArray[2*index+1], 2.0));
         std::vector<int> resIndexes = {i, j, index};
         residualPairs.push_back(std::make_pair(currentResidual, resIndexes));
         index++;
      }
   }  

   return residualPairs;
}





////////////////////////////////////////////////////////////////////////
// Compute the histogram of the residuals.
// Assumes that the residual list is sorted in descending order (largest
// residual to smallest residual)
////////////////////////////////////////////////////////////////////////
std::map<double, int> getResidualHistogram(
          const std::vector<std::pair<double, std::vector<int>>> &residualPairs,
          const double &binW) {

   std::map<double, int> histogram;
   double bin = 0;
   for (auto r = residualPairs.rbegin(); r != residualPairs.rend(); r++) {
      while ((*r).first >= (bin+binW)) { 
         bin += binW;
      }
      ++histogram[bin];
   }

   return histogram;
}



////////////////////////////////////////////////////////////////////////
// Comparison function to sort BA residual pairs returning the pair
// with the largest residual. 
////////////////////////////////////////////////////////////////////////
bool sortByResidual(const pair<double, vector<int> > &i, 
                     const pair<double, vector<int> > &j)
{
    return i.first > j.first;
}


////////////////////////////////////////////////////////////////////////
// Comparison function to sort observations in tracks (vector of vector)
// from the largest indice to smallest. This is useful when removing elements 
// from the vector based on indices to keep the consistency.
////////////////////////////////////////////////////////////////////////
bool sortByIndex(const pair<double, vector<int> > &i, 
                 const pair<double, vector<int> > &j)
{
    return (i.second)[2] > (j.second)[2];
}



////////////////////////////////////////////////////////////////////////
// This function runs the Union-Find algorthim over a list of pairs to 
// return the underlying connected group.
// When this function is applied to a list of tiepoints, it returns the tracks,
// which are the groups of observations that "look" at the same ground location.
// This funtion is also used to find the images connectivity, that is to 
// identify the groups of images that are connected based on a list of pairwise
// connection.
////////////////////////////////////////////////////////////////////////

template<typename T>
vector< vector<T> > union_find(vector< vector<T> > tiePointsVect)
{
    unsigned int i, idSecond, trackIndex = 0;

    //containers and iterators declaration
    map<T, int> points;
    typename map<T, int>::iterator it1, it2;
    
    set<T> track2;
    typename set<T>::iterator itTrack;
    
    vector< set<T> > allTracks;
    typename vector< set<T> >::iterator itAllTracks;
    vector< vector<T> > finalTracks;
 

    // First part of the Union-Find algorithm
    // Run through the list of tie points and save each unique point in 
    // a std::map. The map-key is a unique observation (pixel) identifier, and 
    // map-value is the identifier (the index really) of the corresponding track
    // (initialized to -1). std:map is usefull here to avoid duplicates.
    for (i=0; i<tiePointsVect.size(); i++) {
       points.insert(std::make_pair(tiePointsVect[i][0], -1));
       points.insert(std::make_pair(tiePointsVect[i][1], -1)); 
    } 


    // Second part of the Union-Find algorithm. The objective is to associate
    // each observation to a track ID. We're iterating over the tie-points 
    // (a pair of observations) and find the track ID of each point.
    // Each observation with identical track ID points to the same ground point.
    for (i=0; i<tiePointsVect.size(); i++) {

        // For the current tie-points (pair) find their corresponding track ID
        // using the std::map previously built.
        it1 = points.find(tiePointsVect[i][0]);
        it2 = points.find(tiePointsVect[i][1]);

        // If both track ID are -1, that means that these two points are 
        // currently not associated to an existing track. Create a new track
        // and update the observations corresponding track ID.
        if (it1->second == -1 && it2->second == -1) {
            set<T> newTrack;                    // Create a new track container.
            newTrack.insert(it1->first);        // Insert point ID in this new
            newTrack.insert(it2->first);        // track.
            allTracks.push_back(newTrack);      // Add the new track to the list
            it1->second = trackIndex;           // Update the track ID of these
            it2->second = trackIndex;           // these two observations.
            trackIndex++;                       // Update the track ID index
        }

        // If only track ID of "left" tie-point is -1, that means that
        // "left"  point belong to a track already initialized and identified by
        // "right" point track ID. Add "left" observation to the track and 
        // update its track ID
        else if (it1->second == -1) {
            (allTracks[it2->second]).insert(it1->first);
            it1->second = it2->second;
        }

        // Same as above but with opposite situation for "left" and "right"
        else if (it2->second == -1) {
            (allTracks[it1->second]).insert(it2->first);
            it2->second = it1->second;
        }

        // If both observations already have a track ID. If the track IDs are 
        // identical, there is nothing to do, as these two points have already 
        // been assigned to the same track. If they are different, this means 
        // that the two points belong to two different tracks that are actually
        // referring to the same ground point, and need to be merged.
        // To merge them, we are copying all points belonging to "right" track 
        // into "left" track. Update the "right" observations track ID 
        // accordingly, and finally empty the "right" track. Note, the track is
        // emptied, not erased, to preserve the track index coherence.
        else {
            if (it1->second != it2->second) {
               idSecond = it2->second;
               track2 = allTracks[it2->second];
               for (itTrack = track2.begin(); itTrack != track2.end(); itTrack++) { 
                  (allTracks[it1->second]).insert(*itTrack);
                  it2 = points.find(*itTrack);
                  it2->second = it1->second;
               }
               (allTracks[idSecond]).clear();
            }
        }   
    }


    // Reformating the tracks
    // std::map has been a useful container (non-duplicate and fast tree search)
    // for finding the tracks during the second part of the Union-Find algorithm.
    // However, for the rest of the work, it's not very handy to have the tracks
    // stored like that. We "reformat" the track as a vector of vector (i.e., 
    // first vector contains the tracks, second vector contains observations ID 
    // belonging to a given track.
    // TO DO: See if we could avoid this step. No significant time penalty
    // observed.  
    for (itAllTracks = allTracks.begin(); itAllTracks != allTracks.end(); 
                                                                itAllTracks++) {
        if (itAllTracks->size() == 0)
           continue;
        vector<T> track;
        std::copy(itAllTracks->begin(), itAllTracks->end(), 
                                                     std::back_inserter(track));
        finalTracks.push_back(track);
    }


    return finalTracks;
 }






////////////////////////////////////////////////////////////////////////
// Track filter:
// Because tie-points may contains erroneous pairing, we might have corrupted 
// tracks. A corrupted track for instance is a track that contains two 
// observations from the same image (i.e. two points in the same image "look" at
// the same ground point). 
// There are algorithms (e.g.,Gomory-Hu tree analysis) that try to prune the 
// track from bad points. However, for now, if a track contains two points from 
// the same image (corruption), we delete the track.
// Note that the current approach does not remove all bad tie-points. Erroneous 
// tie-points that are not connected to other tie-points belonging to same image
// will go undetected.
// For each track, if we find two instances of the same image ID means a 
// corrupted track. 
////////////////////////////////////////////////////////////////////////
int filter_tracks(vector<vector<string> > &tracks, 
                  const map<string,observation> &mapOfPoints)
{
   set<int> imageSet;
   int nbTrackRemoved = 0;
   //Going backward to keep indices consistant if a track is deleted
   for (int i=tracks.size()-1; i >= 0; i--) {
      imageSet.clear();
      for (int j=0; j<tracks[i].size(); j++) {
         auto it = mapOfPoints.find(tracks[i][j]);
         if (it != mapOfPoints.end()) {
            //If addition of image ID to imageSet fails, that means that
            //this image is already represented in the current track
            //-->corruption. Delete the track and move on
            if (!imageSet.insert((it->second).imageIndex).second) {
               nbTrackRemoved++;
               tracks.erase(tracks.begin()+i);
               break;
            }
         }
      }
   }

   return nbTrackRemoved;
}










////////////////////////////////////////////////////////////////////////
// Wrapper for save_pointing_file and save_pointing_xml
////////////////////////////////////////////////////////////////////////

void save_pointing(int xml_mode, char *name, int n,
		PigPointingModel *pointings[],
		double pparams_orig[][PIG_MAX_PARAMS],
     		PigFileModel *files[], PigSurfaceModel *surface_model, 
	        char *solution_id, int originalCM)
{
    if (xml_mode)
        save_pointing_xml(name, n, pointings, pparams_orig, files, 
                          surface_model, solution_id, originalCM);
    else
        save_pointing_file(name, n, pointings, pparams_orig);
}





////////////////////////////////////////////////////////////////////////
// Write out the pointing corrections table.  Current pointings are
// obtained from the pointing model; pparams_orig array contains the
// original pointings before corrections were applied.
//
// File format:
// header:	"marsnav pointing corrections file, version 1"
// Each line is a space-separated list of numbers.  The first is an
// integer, n, which specifies the # of pointing params on this line.
// Then follow n original pointing values, followed by n corrected pointing
// values.
////////////////////////////////////////////////////////////////////////

void save_pointing_file(char *name, int n, PigPointingModel *pointings[],
			double pparams_orig[][PIG_MAX_PARAMS])
{
    FILE *fout;
    int i, j;
    char msg[150];

    if ((fout = fopen(name, "w")) == NULL) {
	sprintf(msg, "Error opening pointing corrections file %s\n", name);
	zvmessage(msg, "");
	zabend();
    }

    fprintf(fout, "marsnav pointing corrections file, version 1\n");
    for (i=0; i < n; i++) {
	double p[PIG_MAX_PARAMS];
	int n = pointings[i]->getPointingParamCount();
	if (n == 0)
	    continue;

	fprintf(fout, "%d", n);			// output n

	pointings[i]->getPointingParameters(p, n);

	for (j=0; j < n; j++)			// Print original params
	    fprintf(fout, " %8.3f", pparams_orig[i][j]);
	for (j=0; j < n; j++)			// Print corrected params
	    fprintf(fout, " %8.3f", p[j]);
	fprintf(fout, "\n");
    }
    fclose(fout);
}


////////////////////////////////////////////////////////////////////////
// Write out the pointing corrections table in XML format.  Current 
// pointings are obtained from the pointing model; pparams_orig array
// contains the original pointings before corrections were applied.
//
// File format:
// header:      <?xml version="1.0" encoding="UTF-8"?>
// For more detail for the format, ask Bob.
////////////////////////////////////////////////////////////////////////
void save_pointing_xml(char *name, int n, PigPointingModel *pointings[],
		       double pparams_orig[][PIG_MAX_PARAMS], 
		       PigFileModel *files[],
                       PigSurfaceModel *surface_model,
		       char *solution_id,
                       int original_cm)
{ 
    FILE *fout;
    int i, j, /* status, */ count;
    char msg[150];
    char point_file[255];

    PigMission *m = PigMission::getMissionObject(files[0]);

    PigCameraModel *camera_model = pointings[0]->getCameraModel();
    const char *mission = camera_model->getMissionName();

//!!!! This should really be multimission-ized...
    if (!strncasecmp(mission, "MER", 3)) 
        sprintf(point_file, "%s_pma.point", ((PigMER *)m)->getHostID());
    else 
        sprintf(point_file, "%s_mast.point", mission);

    if ((fout = fopen(name, "w")) == NULL) {
        sprintf(msg, "Error opening pointing corrections file %s\n", name);
        zvmessage(msg, "");
        zabend();
    }

    const char *calib_id = files[0]->getCalibrationSourceId();
    const char *filter = files[0]->getFilterNumber();
    const char *frame_id = files[0]->getFrameId();
    const char *inst_id = camera_model->getInstrumentName();

    fprintf(fout, "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");
    fprintf(fout, "<pointing_correction"); 
    if (mission != NULL)
        fprintf(fout, " mission=\"%s\"", mission);
    fprintf(fout, ">\n");
    fprintf(fout, "  <origination");
    fprintf(fout, " id=\"%s\"", solution_id);
    fprintf(fout, " institution=\"%s\" program=\"%s\">\n", "mipl", "marsnav2");

    fprintf(fout, "    <purpose>A pointing correction file for one mosaic might look for %s%s\n",
            mission, "</purpose>");
    fprintf(fout, "  </origination>\n");

    //this part may not need 
    fprintf(fout, "  <static_parameters file=\"%s\" instrument=\"%s\"", 
            point_file, inst_id);
    fprintf(fout, " solution_id=\"%s\"", solution_id);
    fprintf(fout, "/>\n");

    fprintf(fout, "  <calibration");
    if (calib_id != NULL) 
        fprintf(fout, " calibration_id=\"%s\"", calib_id);
    if (filter != NULL)
        fprintf(fout, " filter=\"%s\"", filter);
    if (frame_id != NULL)
        fprintf(fout, " frame_id=\"%s\"", frame_id);
    if (inst_id != NULL)
        fprintf(fout, " instrument=\"%s\"", inst_id);
    fprintf(fout, " solution_id=\"%s\"", solution_id);
    fprintf(fout, "/>\n");

    fprintf(fout, "  <priority>\n");
    fprintf(fout, "    <entry solution_id=\"%s\"/>\n", solution_id);
    fprintf(fout, "  </priority>\n"); 

    for (i=0; i < n; i++) {
        int count = pointings[i]->getPointingParamCount();
        if (count == 0)
            continue;

        PigCSReference *csRef;
        fprintf(fout, "  <solution");
        /* status = */ files[i]->getCameraModelCS(csRef, 1);
        int size = csRef->getNumIndices();
        for (j=0; j<size; j++) {
            fprintf(fout, " index%d=\"%d\"", j+1, csRef->getIndex(j));
        }
        fprintf(fout, " solution_id=\"%s\"", solution_id);
        fprintf(fout, ">\n");

        inst_id = files[i]->getInstrumentId();
        const char *image_id = files[i]->getImageId();
        frame_id = files[i]->getFrameId();
        filter = files[i]->getFilterNumber();
        char unique_id[33] = ""; 
        files[i]->getUniqueId(unique_id);

        fprintf(fout, "    <image");
        if (filter != NULL)
            fprintf(fout, " filter=\"%s\"", filter);
        if (frame_id != NULL)
            fprintf(fout, " frame_id=\"%s\"", frame_id);
        if (image_id != NULL)
            fprintf(fout, " image_id=\"%s\"", image_id);
        if (inst_id != NULL)
            fprintf(fout, " instrument=\"%s\"", inst_id);
        if (strcmp(unique_id, ""))
            fprintf(fout, " unique_id=\"%s\"", unique_id);
        fprintf(fout, ">\n");

        double p[PIG_MAX_PARAMS];
        pointings[i]->getPointingParameters(p, count);

        fprintf(fout, "      <original_parameters type=\"%s\">\n", 
                pointings[i]->getModelName());
        for (j=0; j<count; j++)                   // Print original params
            fprintf(fout, "        <parameter id=\"%s\" value=\"%.8lf\"/>\n", 
                    pointings[i]->getPointingParamName(j), pparams_orig[i][j]);
        fprintf(fout, "      </original_parameters>\n");


        // If saving the original camera model
        if (original_cm) {

           // Setting up original pointing
           pointings[i]->setPointingParameters(pparams_orig[i], count);

           PigPoint c_point;
           PigVector a_vector, h_vector, v_vector, o_vector, r_vector, e_vector;
           int mtype = 0;
           double mparam = 0.;
           camera_model = pointings[i]->getCameraModel();
           const char *cm_type = camera_model->getModelName();

    
           if (strcasecmp(cm_type, "CAHV") == 0) {
              ((PigCAHV*)camera_model)->getCurrentCAHV(c_point, a_vector, 
                                            h_vector, v_vector);
           }
           else if (strcasecmp(cm_type, "CAHVOR") == 0) {
              ((PigCAHVOR*)camera_model)->getCurrentCAHVOR(c_point, a_vector,
                                        h_vector, v_vector, o_vector, r_vector);
           }
           else if (strcasecmp(cm_type, "CAHVORE") == 0) {
              ((PigCAHVORE*)camera_model)->getCurrentCAHVORE(c_point, a_vector,
                         h_vector, v_vector, o_vector, r_vector, e_vector, mtype, mparam);
           }
           else { // for camera_model = NONE
                 sprintf(msg, "Camera model type: %s\n", cm_type);
                 zvmessage(msg, "");
           }

           fprintf(fout, "      <original_camera_model type=\"%s\">\n", cm_type);
           if (strncasecmp(cm_type, "CAHV", 4)==0) {
              fprintf(fout, "        <parameter id=\"C\" type=\"float_3\" ");
              fprintf(fout, "value1=\"%.8lf\" value2=\"%.8lf\" value3=\"%.8lf\"/>\n",
                               c_point.getX(), c_point.getY(), c_point.getZ());
   
              fprintf(fout, "        <parameter id=\"A\" type=\"float_3\" ");
              fprintf(fout, "value1=\"%.8lf\" value2=\"%.8lf\" value3=\"%.8lf\"/>\n", 
                               a_vector.getX(), a_vector.getY(), a_vector.getZ());

              fprintf(fout, "        <parameter id=\"H\" type=\"float_3\" ");
              fprintf(fout, "value1=\"%.8lf\" value2=\"%.8lf\" value3=\"%.8lf\"/>\n", 
                               h_vector.getX(), h_vector.getY(), h_vector.getZ());

              fprintf(fout, "        <parameter id=\"V\" type=\"float_3\" ");
              fprintf(fout, "value1=\"%.8lf\" value2=\"%.8lf\" value3=\"%.8lf\"/>\n", 
                               v_vector.getX(), v_vector.getY(), v_vector.getZ());
           }

           if (strncasecmp(cm_type, "CAHVOR", 6)==0) {
              fprintf(fout, "        <parameter id=\"O\" type=\"float_3\" ");
              fprintf(fout, "value1=\"%g\" value2=\"%g\" value3=\"%g\"/>\n", 
                               o_vector.getX(), o_vector.getY(), o_vector.getZ());

              fprintf(fout, "        <parameter id=\"R\" type=\"float_3\" ");
              fprintf(fout, "value1=\"%g\" value2=\"%g\" value3=\"%g\"/>\n", 
                               r_vector.getX(), r_vector.getY(), r_vector.getZ());
           }

           if (strncasecmp(cm_type, "CAHVORE", 7)==0) {
              fprintf(fout, "        <parameter id=\"E\" type=\"float_3\" ");
              fprintf(fout, "value1=\"%g\" value2=\"%g\" value3=\"%g\"/>\n", 
                               e_vector.getX(), e_vector.getY(), e_vector.getZ());
    
              fprintf(fout, "        <parameter id=\"T\" value=\"%d\"/>\n", mtype);
              fprintf(fout, "        <parameter id=\"P\" value=\"%g\"/>\n", mparam);
           }


           const char *ref_name = csRef->getFrameName();
           if ((ref_name != NULL) || (size > 0))
              fprintf(fout, "        <reference_frame");
           if (ref_name != NULL)
              fprintf(fout, " name=\"%s\"", csRef->getFrameName());
           for (j=0; j<size; j++) {
              fprintf(fout, " index%d=\"%d\"", j+1, csRef->getIndex(j));
           }
           if ((ref_name != NULL) || (size > 0))
              fprintf(fout, "/>\n");
           fprintf(fout, "      </original_camera_model>\n");


           // Reverting back to final pointing
           pointings[i]->setPointingParameters(p, count);
        }



        fprintf(fout, "    </image>\n");
      
        fprintf(fout, "    <pointing_parameters type=\"%s\">\n",
                pointings[i]->getModelName());
        for (j=0; j<count; j++)                   // Print corrected params
            fprintf(fout, "      <parameter id=\"%s\" value=\"%.8lf\"/>\n", 
                    pointings[i]->getPointingParamName(j), p[j]);

        fprintf(fout, "    </pointing_parameters>\n");


        PigPoint c_point;
        PigVector a_vector, h_vector, v_vector, o_vector, r_vector, e_vector;
        int mtype = 0;
        double mparam = 0.;
        camera_model = pointings[i]->getCameraModel();
        const char *cm_type = camera_model->getModelName();
    
        //char tmp[350]; 
        if (strcasecmp(cm_type, "CAHV") == 0) {
            ((PigCAHV*)camera_model)->getCurrentCAHV(c_point, a_vector, 
                                                     h_vector, v_vector);
        }
        else if (strcasecmp(cm_type, "CAHVOR") == 0) {
            ((PigCAHVOR*)camera_model)->getCurrentCAHVOR(c_point, a_vector,
                                        h_vector, v_vector, o_vector, r_vector);
        }
        else if (strcasecmp(cm_type, "CAHVORE") == 0) {
            ((PigCAHVORE*)camera_model)->getCurrentCAHVORE(c_point, a_vector,
               h_vector, v_vector, o_vector, r_vector, e_vector, mtype, mparam);
        }
        else { // for camera_model = NONE
            sprintf(msg, "Camera model type: %s\n", cm_type);
            zvmessage(msg, "");
        }

        fprintf(fout, "    <camera_model type=\"%s\">\n", cm_type);
        if (strncasecmp(cm_type, "CAHV", 4)==0) {
            fprintf(fout, "      <parameter id=\"C\" type=\"float_3\" ");
            fprintf(fout, "value1=\"%.8lf\" value2=\"%.8lf\" value3=\"%.8lf\"/>\n",
                    c_point.getX(), c_point.getY(), c_point.getZ());
   
            fprintf(fout, "      <parameter id=\"A\" type=\"float_3\" ");
            fprintf(fout, "value1=\"%.8lf\" value2=\"%.8lf\" value3=\"%.8lf\"/>\n", 
                    a_vector.getX(), a_vector.getY(), a_vector.getZ());

            fprintf(fout, "      <parameter id=\"H\" type=\"float_3\" ");
            fprintf(fout, "value1=\"%.8lf\" value2=\"%.8lf\" value3=\"%.8lf\"/>\n", 
                    h_vector.getX(), h_vector.getY(), h_vector.getZ());

            fprintf(fout, "      <parameter id=\"V\" type=\"float_3\" ");
            fprintf(fout, "value1=\"%.8lf\" value2=\"%.8lf\" value3=\"%.8lf\"/>\n", 
                    v_vector.getX(), v_vector.getY(), v_vector.getZ());
        }

        if (strncasecmp(cm_type, "CAHVOR", 6)==0) {
            fprintf(fout, "      <parameter id=\"O\" type=\"float_3\" ");
            fprintf(fout, "value1=\"%g\" value2=\"%g\" value3=\"%g\"/>\n", 
                    o_vector.getX(), o_vector.getY(), o_vector.getZ());

            fprintf(fout, "      <parameter id=\"R\" type=\"float_3\" ");
            fprintf(fout, "value1=\"%g\" value2=\"%g\" value3=\"%g\"/>\n", 
                    r_vector.getX(), r_vector.getY(), r_vector.getZ());
        }

        if (strncasecmp(cm_type, "CAHVORE", 7)==0) {
            fprintf(fout, "      <parameter id=\"E\" type=\"float_3\" ");
            fprintf(fout, "value1=\"%g\" value2=\"%g\" value3=\"%g\"/>\n", 
                    e_vector.getX(), e_vector.getY(), e_vector.getZ());
    
            fprintf(fout, "      <parameter id=\"T\" value=\"%d\"/>\n", mtype);
            fprintf(fout, "      <parameter id=\"P\" value=\"%g\"/>\n", mparam);
        }


        const char *ref_name = csRef->getFrameName();
        if ((ref_name != NULL) || (size > 0))
            fprintf(fout, "      <reference_frame");
        if (ref_name != NULL)
            fprintf(fout, " name=\"%s\"", csRef->getFrameName());
        for (j=0; j<size; j++) {
            fprintf(fout, " index%d=\"%d\"", j+1, csRef->getIndex(j));
        }
        if ((ref_name != NULL) || (size > 0))
            fprintf(fout, "/>\n");
        fprintf(fout, "    </camera_model>\n");
        fprintf(fout, "  </solution>\n");
    }
    // write out the surface model
    // One surface model allowed per solution id
    fprintf(fout, "  <surface_model type=\"%s\"", surface_model->getModelName());
    fprintf(fout, " solution_id=\"%s\">\n", solution_id);
    double surf_pp[PIG_MAX_PARAMS];
    count = surface_model->getPointingParamCount();
    surface_model->getPointingParameters(surf_pp, count);

    for (int cnt = 0; cnt<count; cnt++)            // Print surface model params
    fprintf(fout, "      <parameter id=\"%s\" value=\"%g\"/>\n", 
    surface_model->getPointingParamName(cnt), surf_pp[cnt]);

    // print out surface model's coordinate system definition
    const char *ref_name = surface_model->getCoordSystem()->getFrameName();
    if (ref_name != NULL) {
       fprintf(fout, "      <reference_frame");
       fprintf(fout, " name=\"%s\"", ref_name);
       
       PigCSReference *ref = surface_model->getCoordSystem()->getIdentity();
       for (int cnt = 0; cnt<ref->getNumIndices(); cnt++) {
	   fprintf(fout, " index%d=\"%d\"", cnt+1, ref->getIndex(cnt));
       }
       fprintf(fout, "/>\n");
    }
    fprintf(fout, "  </surface_model>\n");
    fprintf(fout, "</pointing_correction>\n");
    fclose(fout);
}







////////////////////////////////////////////////////////////////////////
// Print out any sites that are being adjusted.  This is in lieu of actually
// writing out an RMC file, for the time being... !!!!
////////////////////////////////////////////////////////////////////////

void print_sites(SiteList site_list[], int num_sites)
{
    char msg[512];

    for (int i=0; i < num_sites; i++) {
	if (site_list[i].adjust_location || site_list[i].adjust_orientation) {
	    sprintf(msg, "Site %d, '%s'", i, site_list[i].site->getFrameName());
	    zvmessage(msg, "");
	    double p[6];			// we know it's 6 already
	    site_list[i].site->getPointingParameters(p, 6);
	    // Print xyz explicitly instead of getPointintParamName() so
	    // the case is right for cut-and-paste...
	    sprintf(msg, "x=\"%g\" y=\"%g\" z=\"%g\"", p[0], p[1], p[2]);
	    zvmessage(msg, "");

	    sprintf(msg, "%s=\"%g\" %s=\"%g\" %s=\"%g\"",
		site_list[i].site->getPointingParamName(3), p[3],
		site_list[i].site->getPointingParamName(4), p[4],
		site_list[i].site->getPointingParamName(5), p[5]);
	    zvmessage(msg, "");

	    PigQuaternion q;
	    q.setEulerAngles(PigDeg2Rad(p[3]), PigDeg2Rad(p[4]),
							PigDeg2Rad(p[5]));
	    sprintf(msg, "s=\"%g\" v1=\"%g\" v2=\"%g\" v3=\"%g\"",
		q.getS(), q.getV().getX(), q.getV().getY(), q.getV().getZ());
	    zvmessage(msg, "");
	}
    }
}





////////////////////////////////////////////////////////////////////////
// Visual Residual Display functions 
////////////////////////////////////////////////////////////////////////

void drawTieLocation(const std::vector<std::vector<std::string>> &tracks,
                     const std::map<string,observation> &mapOfPoints,
                     const color_3u8 &color, 
                     std::vector<SimpleImage<unsigned char> *> &tpImgs) {

   // Draw location of tie points
   int index = 0;
   for (int i=0; i<tracks.size(); i++) {
      for (int j=0; j<(tracks[i]).size(); j++) {
         auto it = mapOfPoints.find(tracks[i][j]);
         auto imgId = tpImgs[(it->second).imageIndex];
         int x = (int)((it->second).sample + 0.5);
         int y = (int)((it->second).line + 0.5);
         index++; 

         drawCross(imgId, x, y, color);
      }
   }
}


void drawTieResidual(const std::vector<std::vector<std::string>> &tracks,
                     const std::map<string,observation> &mapOfPoints,
                     const std::vector<double> &residuals,
                     const double scaler,
                     const color_3u8 &color, 
                     std::vector<SimpleImage<unsigned char> *> &tpImgs) {

   // Draw location of tie points
   int index = 0;
   for (int i=0; i<tracks.size(); i++) {
      for (int j=0; j<(tracks[i]).size(); j++) {
         auto it = mapOfPoints.find(tracks[i][j]);
         auto imgId = tpImgs[(it->second).imageIndex];
         int x = (int)((it->second).sample + 0.5);
         int y = (int)((it->second).line + 0.5);
         int x2 = (int)(x - scaler * residuals[index*2] + 0.5); 
         int y2 = (int)(y - scaler * residuals[index*2+1] + 0.5);

         index++; 

         drawLine(imgId, x, y, x2, y2, color);
      }
   }
}




template<class T>
void drawCross(SimpleImage<T> *img, const int x, const int y, const color_3u8 &c) {

      int ns = img->getNS();
      int nl = img->getNL();

      if (y > 2) {
         img->set(0, y-2, x, c.rgb[0]);
         img->set(1, y-2, x, c.rgb[1]);
         img->set(2, y-2, x, c.rgb[2]);
      }
      if (y > 1) {
         img->set(0, y-1, x, c.rgb[0]);
         img->set(1, y-1, x, c.rgb[1]);
         img->set(2, y-1, x, c.rgb[2]);
      }
      if (x > 2) {
         img->set(0, y, x-2, c.rgb[0]);
         img->set(1, y, x-2, c.rgb[1]);
         img->set(2, y, x-2, c.rgb[2]);
      }
      if (x > 1) {
         img->set(0, y, x-1, c.rgb[0]);
         img->set(1, y, x-1, c.rgb[1]);
         img->set(2, y, x-1, c.rgb[2]);
      }
      img->set(0, y, x, c.rgb[0]);
      img->set(1, y, x, c.rgb[1]);
      img->set(2, y, x, c.rgb[2]);
      if (x < ns-1) {
         img->set(0, y, x+1, c.rgb[0]);
         img->set(1, y, x+1, c.rgb[1]);
         img->set(2, y, x+1, c.rgb[2]);
      }
      if (x < ns-2) {
         img->set(0, y, x+2, c.rgb[0]);
         img->set(1, y, x+2, c.rgb[1]);
         img->set(2, y, x+2, c.rgb[2]);
      }
      if (y < nl-1) {
         img->set(0, y+1, x, c.rgb[0]);
         img->set(1, y+1, x, c.rgb[1]);
         img->set(2, y+1, x, c.rgb[2]);
      }
      if (y < nl-2) {
         img->set(0, y+2, x, c.rgb[0]);
         img->set(1, y+2, x, c.rgb[1]);
         img->set(2, y+2, x, c.rgb[2]);
      }

}


template<class T1, class T2>
void drawLine(SimpleImage<T1> *img, T2 x1, T2 y1, T2 x2, T2 y2, const color_3u8 &c) {

   if (img == nullptr) 
      return;

   const bool steep = (fabs(y2 - y1) > fabs(x2 - x1));
   if(steep){
      std::swap(x1, y1);
      std::swap(x2, y2);
   }

   if(x1 > x2) {
      std::swap(x1, x2);
      std::swap(y1, y2);
   }

   const float dx = x2 - x1;
   const float dy = fabs(y2 - y1);

   float error = dx / 2.0f;
   const int ystep = (y1 < y2) ? 1 : -1;
   int y = (int)y1;

   const int maxX = (int)x2;


   int ns = img->getNS();
   int nl = img->getNL();
   for(int x=(int)x1; x<=maxX; x++) {
      if(steep) {
         if ((x > 0) && (x < (nl-1)) &&
             (y > 0) && (y < (ns-1))) {
            img->set(0, x, y, c.rgb[0]); //c1a[i]
            img->set(1, x, y, c.rgb[1]);
            img->set(2, x, y, c.rgb[2]);
         }
      }
      else {
         if ((x > 0) && (x < (ns-1)) &&
             (y > 0) && (y < (nl-1))) {
            img->set(0, y, x, c.rgb[0]);
            img->set(1, y, x, c.rgb[1]);
            img->set(2, y, x, c.rgb[2]);
         }
      }

      error -= dy;
      if(error < 0) {
         y += ystep;
         error += dx;
      }
   }

}



std::string getFileName(const char * fullName) {

   int i, offset_extension, offset_name;
   int len = strlen(fullName);

   for (i = len; i >= 0; i--) {
      if (fullName[i] == '.')
         break;
      if (fullName[i] == '/') {
         i = len;
         break;
      }
   }

   if (i == -1) {
      zvmessage("File name invalid, returning","");
      zabend();
   }

   offset_extension = i;
   for (; i >= 0; i--) {
      if (fullName[i] == '/')
         break;
   }


   offset_name = i;

   return std::string(&fullName[offset_name+1], &fullName[offset_extension]);


}







// Utility function to load an image in memory. Input is the index of the image 
// to load.
SimpleImage<float> * loadImage(PigFileModel * file_model, int band) {


   // If current file is open, close it first
   if (file_model->isFileOpen())
      file_model->closeFile();

   // Open the file for reading
   zvopen(file_model->getUnit(), "OP", "READ", "U_FORMAT", "REAL", 
          "OPEN_ACT", "SA", NULL);

   file_model->setFileOpen(TRUE);

   // Initialize a container to load the image in memory
   SimpleImage<float> * image = new SimpleImage<float>(file_model->getNL(), 
                                                       file_model->getNS());
   // Load the image in memory
   for (int j=0; j<file_model->getNL(); j++)
      zvread(file_model->getUnit(), image->linePtr(j), "BAND", band, 
             "LINE", j+1, NULL);

   // Close the file as it is not needed anymore
   file_model->closeFile();

   // Not sure if we need to zvclose the unit here:
   zvclose(file_model->getUnit(), "CLOS_ACT","FREE", NULL);

   return image;
}




template<class T>
void saveImg(const std::string name, SimpleImage<T> * img) {

   int unit_out;

   const char *cstr = name.c_str();
   zvunit(&unit_out, "", 1, "u_name", cstr, NULL);
   zvopen(unit_out, "OP", "WRITE", "U_FORMAT", img->getVicarTypeString(), 
          "O_FORMAT", img->getVicarTypeString(), "U_NS", img->getNS(), "U_NL", 
          img->getNL(), "OPEN_ACT", "AS", "U_NB", img->getNB(), "U_ORG", "BSQ",
          NULL);
   zvplabel(unit_out, 0, 1);
   if (img->getNB() > 1)
      for (int i=0; i<img->getNB(); i++)
         for (int j=0; j<img->getNL(); j++)
            zvwrit(unit_out, img->linePtr(i,j), "LINE", j+1, "BAND", i+1,NULL);
   else
      for (int j=0; j<img->getNL(); j++)
         zvwrit(unit_out, img->linePtr(j), "LINE", j+1, NULL);
   zvclose(unit_out, "CLOS_ACT", "FREE", NULL);
}


// Compute an XYZ point based on the aveage of pairwise two-view 
// measurements via 'xvector'

int xvector_pairwise_twoview(const PigPoint *lookOrigin, 
							 const PigVector *lookVector, 
							 PigPoint &xyz_final, 
							 double *error, 
							 const int currentTrackSize, 
							 const double parallel_limit, 
							 const double epsilon, 
							 const double error_thresh,
							 std::array<float, 6> & bounding_box)
{
	PigVector los;
    float dotVal;
    PigPoint xyz, sum_xyz;
	double err, sumOfPointWeights = 0.0;
	int counter = 0;
	int status = 1;
	float xmin = std::numeric_limits<float>::max();
	float xmax = -std::numeric_limits<float>::max();
	float ymin = std::numeric_limits<float>::max();
	float ymax = -std::numeric_limits<float>::max();
	float zmin = std::numeric_limits<float>::max();
	float zmax = -std::numeric_limits<float>::max();


	for (int j=0; j<(currentTrackSize-1); j++) {
	    for (int k=(j+1); k<currentTrackSize; k++) {
	        if (lookVector[j] % lookVector[k] <= parallel_limit) {

 	            // Triangulate and get XYZ:

                int status = xvector(lookOrigin[j], lookOrigin[k],
                                    lookVector[j], lookVector[k],
                                    xyz, &err);

            	// If triangulation failed, skip to next pair:

            	if (status != 0) 
            		continue;

            	// Check if triangulated XYZ is in front of the first
				// camera. If erroneous tiepoints, XYZ could end up 
				// behind cam:

            	los = xyz - lookOrigin[j];
            	los.normalize();
            	dotVal = los.getX() * lookVector[j].getX() +
                            los.getY() * lookVector[j].getY() +
                            los.getZ() * lookVector[j].getZ();

            	if (dotVal < -epsilon) {
	            	continue;
            	}
            	else {  

	            	// Check with second camera:

                	los = xyz - lookOrigin[k];
                	los.normalize();
                	dotVal = los.getX() * lookVector[k].getX() +
                               los.getY() * lookVector[k].getY() +
                               los.getZ() * lookVector[k].getZ();

                	if (dotVal < -epsilon) {
                		continue;
                	}
            	}      
				*error += err;
				counter++;             
            	if (err < epsilon)
            		err = epsilon;
            	sum_xyz += xyz/err;
            	sumOfPointWeights += 1.0/err;
				
				// Compute the bounding box created by all the pairwise 
				// estimates, if the track size is > 2:
				if(currentTrackSize > 2){
					if(xyz.getX() < xmin) xmin = xyz.getX();
					if(xyz.getX() > xmax) xmax = xyz.getX();
					if(xyz.getY() < ymin) ymin = xyz.getY();
					if(xyz.getY() > ymax) ymax = xyz.getY();
					if(xyz.getZ() < zmin) zmin = xyz.getZ();
					if(xyz.getZ() > zmax) zmax = xyz.getZ();
				}

            }
        }
    }

	bounding_box[0] = xmin;
	bounding_box[1] = xmax;
	bounding_box[2] = ymin;
	bounding_box[3] = ymax;
	bounding_box[4] = zmin;
	bounding_box[5] = zmax;

	if (sumOfPointWeights != 0.0) { // a point was computed

		xyz_final = sum_xyz/sumOfPointWeights;

		// Obtain the average error:

		if(currentTrackSize > 2) {
			*error /= counter;
		}	

		// Compute the Euclidean distance of the point from the origin. 
		// If the error is some percentage of that or higher, discard 
		// the point, otherwise keep it:

		double euclidean_dist_origin = sqrt(xyz.getX()*xyz.getX() + 								xyz.getY()*xyz.getY() +
								xyz.getZ()*xyz.getZ());
		if(*error < euclidean_dist_origin*error_thresh/100.0) {
	        status = 0;  //ok, it's a valid point and track
		}
    }
	else {
	}

	return status;

}

// Check if a linearly-triangulated or optimized XYZ point falls outside of
// the bounding box created by pairwise two-view estimates

int bounding_box_check(const PigPoint xyz, const std::array<float, 6> bounds) 
{
	if( (xyz.getX() < bounds[0]) || // x_min
	    (xyz.getX() < bounds[1]) || // x_max
	    (xyz.getY() < bounds[2]) || // y_min
	    (xyz.getY() < bounds[3]) || // y_max
		(xyz.getZ() < bounds[4]) || // z_min
		(xyz.getZ() < bounds[5])) { // z_max
			return 1;
	}

	return 0;

}

