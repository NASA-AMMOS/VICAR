
///////////////////////////////////////////////////////////////////////////////
// posegraph - multimission program for pose graph optimization
//
// This program performs pose graph optimization, which estimates the
// trajectory (collection of poses) from relative pose measurements. Input
// relative poses are computed from a set of input images or a navigation file,
// their relative poses are computed in g2o format, and the corrected pointing
// is provided as an output navigation file in XML format (similarly to
// marsnav2).
//
///////////////////////////////////////////////////////////////////////////////

#include "vicmain_c"

#include "mars_support.h"
#include "mars_tiepoints.h"

#include <stdlib.h>
#include <stdio.h>
#include <iostream>
#include <cfloat>
#include <math.h> 
#include <string>
#include <vector>
#include <fstream>
#include <map>
#include <valarray>
#include <numeric>
#include <algorithm>
#include <random>

#include <Eigen/Dense>
#include <Eigen/SVD>

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

#include "ceres/ceres.h"
#include "read_g2o.h"
#include "glog/logging.h"
#include "pose_graph_3d_error_term.h"
#include "types.h"
#include "ceres/local_parameterization.h"


#ifdef _OPENMP
#include <omp.h>
#endif


using namespace std;
using namespace Eigen;


/* buffer sizes in main program */
#define MAX_INPUTS 2000
#define MAX_THREADS 256

#define MAX_NL MARS_MAX_NL
#define MAX_NS MARS_MAX_NS


// Maintain the list of Sites in the image list, and which ones get
// adjusted (curently unused in keyframe_selection, but kept for 
// compatibility with other programs such as marsnav2, and for potential
// future use

struct SiteList {
    PigCoordSystem *site;
    int num_images;		    // # of images using this Site
    int adjust_location;	// adjust this location (directly)?
    int adjust_orientation;	// adjust this orientation (directly)?
    int match_site_loc;		// Site # to stay in sync with for location
    int match_site_ori;		// Site # to stay in sync with for orientation
};

// Observation (half tie-point) - this is mainly for storing fiducial
// tiepoints

struct observation {
    int imageIndex; // equivalent to left/right_image in TiePoint structure
    double sample;
    double line;
    double quality;
    int interactive;
    double X;
    double Y;
    double Z;
 };


// Tie points definition. Just a pairing of 2 pixels between L and R 
// images
struct tiePoint{float xL; float yL; float xR; float yR;};

template <typename T>
vector<size_t> sort_indeces(const vector<T> &v);


// Epipolar geometry and Fundamental matrix utilities:

MatrixXf computeEssential(std::array<float, 9> & F, 
					 const int imageindex1, const int imageindex2,
					 const MatrixXf focal_lengths,
					 const MatrixXf projection_centers);

int decomposeEssential(const MatrixXf E, 
					   std::array<float, 9> & R, 
					   std::array<float, 3> & T,
					   const int j, const int k, const MatrixXf pointings,
					   const double max_rand_noise, 
					   const double max_quat_error, 
					   const double max_pos_error); 

int computeFundamental(const vector<TiePoint> & ties, 
                      std::array<float, 9> & F, unsigned char* used, 
					  float &cond_number);

float calculateFundamentalError(const vector<TiePoint> & ties,
		std::array<float, 9> & F, unsigned char* used, float& var);

MatrixXi computeConnectivityMatrix(TiePoint *tiepoints, 
		const int n_overlaps, int nids);

int computeRelativePose(TiePoint *tiepoints, const int n_overlaps, 
		const int j, const int k, const float edge_discard, 
		const MatrixXi imagesizes, 
		const MatrixXf focal_lengths, const MatrixXf projection_centers,
		std::array<float, 4> &Q, std::array<float, 3> &T,
		float &info_value, const int max_connectivity, 
		const MatrixXf pointings, const int use_essential,
		const double max_rand_noise, 
		const double max_quat_error, 
		const double max_pos_error);


// Ceres-based functions:

void BuildOptimizationProblem(const VectorOfConstraints& constraints,
                              MapOfPoses* poses,
                              ceres::Problem* problem);

bool SolveOptimizationProblem(ceres::Problem* problem, 
		const string summary_disp, const string solver_name);

bool OutputPoses(const std::string& filename, const MapOfPoses& poses);


// Utilities for saving the final pointing file (borrowed from marsnav2):

void save_pointing(int xml_mode, char *name, int n,
		PigPointingModel *pointings[],
		double pparams_orig[][PIG_MAX_PARAMS],
     	PigFileModel *files[], PigSurfaceModel *surface_model, 
	    char *solution_id, int originalCM);

void save_pointing_file(char *name, int n, PigPointingModel *pointings[],
			double pparams_orig[][PIG_MAX_PARAMS]);
			
void save_pointing_xml(char *name, int n, PigPointingModel *pointings[],
		       double pparams_orig[][PIG_MAX_PARAMS], 
		       PigFileModel *files[], PigSurfaceModel *surface_model,
		       char *solution_id, int original_cm);
                       

////////////////////////////////////////////////////////////////////////
// Posegraph program
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

    // Input params after input nav file applied; used for resetting
	// solutions
    double pparams_in[MAX_INPUTS][PIG_MAX_PARAMS];

    // Original input params before nav file; used for saving nav file
    double pparams_in_save[MAX_INPUTS][PIG_MAX_PARAMS];

    // Sites list and pointing params
    int num_sites;
    struct SiteList site_list[PIG_MAX_CS];
    double sitesParams_in_save[PIG_MAX_CS][PIG_MAX_PARAMS];

    // User parameters
    char outfilename[150];
    char g2ofilename[150];
    char tptlist[150]; 
    char mission[64], instrument[64];
    char solution_id[64];
    int xml_mode;
    int start_key;


    ////////////////////////////////////////////////////////////////////

    zvmessage("POSEGRAPH 2022-10-11", "");

    // Initialization of PIG elements.
    // Note that Pig surface model is not used in keyframe_selection. 
	// It is just retrieved here and passed along to save_pointing. 
	// This is to keep consistency across programs using .nav file 
	// and expecting a surface model

    mars_setup(nids, file_models, camera_in_all, pointing_in_all, 
               surface_model, NULL, output_cs, mission, instrument,
	       	   homogeneous_inputs, MAX_NL, MAX_NS, MAX_INPUTS);
 
	//Print out input filenames:
	char **input_filenames = new char *[MAX_INPUTS];
    if (input_filenames == NULL) {
		zvmessage("Memory error in mars_setup, filename array", "");
		zabend();
    }
    mars_get_filelist("INP", nids, input_filenames, MAX_INPUTS, FALSE);
	for(int i=0; i<nids; i++) {
		cout << "Input file " << i << "= " << input_filenames[i] << endl;      
	}

    // Get the initial pointing. Not that this program EXPECTS seven pointing
	// parameters, and will not work if there is any other number since a g2o
	// file needs a quaternion and 3D position vector. 

	int * numPointingParamsOrig;
    numPointingParamsOrig = new (std::nothrow) int[nids];
	for (int i=0; i<nids; i++) {
    	int n = (pointing_in_all[i])->getPointingParamCount();
        numPointingParamsOrig[i] = n;
		if (n != 7) {
			sprintf(msg, "     Camera %d number of pointing params: %d", i, n);
			zvmessage(msg,"");
			zvmessage("There should be exactly seven to create a g2o file! " 						  "Exiting.", "");
			zabend();
    	}
	}

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

    // Check for an input tiepoint file

    zvp("in_tpt", tptlist, &count);
    if (count != 1) {
		zvmessage("Input tiepoint file required!", "");
		zabend();
    }

    // WARNING: If fiducial tiepoints are used, the REFERENCE_FRAME 
	// field in the tiepoint file must be set to the corresponding
	// fiducial CS. 

    mars_load_tiepoints(tptlist, tiepoints, n_overlaps, file_models, 
			nids, output_cs, PigMission::getMissionObject(mission));
    sprintf(msg, "%d tiepoints read from %s", n_overlaps, tptlist);
    zvmessage(msg, "");

    if (n_overlaps == 0) {
       zvmessage("No tiepoints. Returning...","");
       return;
    }

    // Get parameter overrides, if any

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
		    zvmessage("REFIMAGE value less than 0 and prior ref not "
					  "correct (neg or > this)", "");
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

    // Look at the ignore array.  Any tiepoints using an image in this 
	// list will be inactivated.

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
                    zvmessage("IGNORE value less than 0 and prior "
							  "ignore not correct (neg or > this)", "");
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
    // tiepoint.  This allows you to run a bunch of images at once,
	// ignoring connections between them, and looking only at how they
	// connect to their reference-image neighbors.

    if (zvptst("IGNORE_INTRA")) {
	int n_ignored = 0;
	for (i=0; i < n_overlaps; i++) {
	    TiePoint *tie = &tiepoints[i];
	    if (!tie->active)
			continue;
	
	    // Preserving some logic looking forward to multi-image 
		// tiepoints. See marsbrt for how that might work.
	
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

    // Save original CM to nav file?
	int originalCM = zvptst("CM_ORI");
    
    zvp("START_KEY", &start_key, &count);
    
    // Check the output format; if SOLUTION_ID is not given, fail now rather
    // than after the computation (keeping this to match marsnav2)

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

	// LEAVING THIS SINCE IT'S PART OF MARSNAV2, FOR REFERENCE
	//
    // For now, Dynamic XYZ tiepoints are treated as Traditional 
	// tiepoints, that is, the XYZ coordinates are not read from images,
	// but infered from triangulation. This will probably change in the
	// future, but some points need to be clarified: how to handle 
	// tracks, error on XYZ, etc.
    //
    // XYZ retrieval commented out

    // Read in the XYZ values for all the Dynamic XYZ tiepoints, and
	// convert them to their own Rover frames.  This is a little
	// inefficient in that a file with multiple tiepoints is opened and
	// closed for each tiepoint, but at least we read in only the
	// tiepoint, not the entire file.
    // UPDATE: For now, Dynamic XYZ tiepoints code below is commented 
	// out as these tiepoints are just considered as traditional 
	// tiepoints (only looking at the left and right line/sample). This
	// might change in the future
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

    // Save the original pointing (after input nav file)

    for (i=0; i < nids; i++) {
		pointing_in_all[i]->getPointingParameters(pparams_in[i],
						     PIG_MAX_PARAMS);
    }

    // Set the initial pointing

    for (i=0; i < nids; i++) {
        pointing_in_all[i]->setPointingParameters(pparams_in[i],
						    PIG_MAX_PARAMS);
    }

    // Gather the site list

    PigMission *m = PigMission::getMissionObject(mission);
    int filesSites[nids];  // to record which site is associated 
						   // with this image 

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
		if (j == num_sites) {  // not found, add one
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

    // Print out the site list

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


	//*****************************************************************
	// Entering code that is specific to "posegraph"	
	//*****************************************************************

 	// Output navigation filename

    zvpone("OUT", outfilename, 1, sizeof(outfilename));
    
    // Output g2o filename

    zvpone("OUT_G2O", g2ofilename, 1, sizeof(g2ofilename));

    // Save images connectivity matrix (matches per pair)?
    // If the images number is large, the matrix won't be displayed 
	// nicely

    int connectivity_save = zvptst("SAVE_CONNECT"); 

    // Save the before and after pointings to files named 'poses_original.txt'
	// and 'poses_optimized.txt'? These can be used to analyze and/or plot
	// the before/after changes

    int poses_disp = zvptst("BEFORE_AFTER"); 

	// Get the factor (percentage) for discarding matches near image 
	// edges

	double edge_discard = 0.0;
    zvparmd("EDGE_DIST", &edge_discard, &count, &def, 1, 0); 

	// Use the Essential matrix for computing relative poses? Or the 
	// input telemetry position and orientation differences?
	
    int use_essential = zvptst("USE_ESSENTIAL"); 

	// Maximum random noise to add to relative telemetry positions and 
	// orientations

	double max_rand_noise = 0.0;
    zvparmd("MAX_RAND_NOISE", &max_rand_noise, &count, &def, 1, 0); 

	// Maximum allowed quaternion and position errors for the Essential
	// matrix decomposition relative to telemetry differences

	double max_quat_error = 0.0;
    zvparmd("MAX_QUAT_ERROR", &max_quat_error, &count, &def, 1, 0);
	double max_pos_error = 0.0;
    zvparmd("MAX_POS_ERROR", &max_pos_error, &count, &def, 1, 0);

    google::InitGoogleLogging("Posegraph Google logging");
    
    // Retrieving and setting the solver type.
    // More information available at ceres-solver.org

    ceres::Solver::Options optionsSolver;        
    char solver_char[128];
    zvp("SOLVER",solver_char, &count);
    string solver_name(solver_char);

    if (!StringToLinearSolverType(solver_name,
		&optionsSolver.linear_solver_type)) {
        zvmessage("Solver type not supported.","");
        zabend();
    }

    // Which minimization report to display in the stdout: none, full, brief

    string summary_disp = "FULL_SUM";
    if (zvptst("NO_SUM"))
        summary_disp = "NO_SUM";
    else if (zvptst("BRIEF_SUM"))
        summary_disp = "BRIEF_SUM";
    

    // All parameters retrieved, move on to analyzing the input data 

	// THE FOLLOWING CODE IS BORROWED FROM MARSNAV2, TO STORE TIEPOINTS

    zvmessage(" ",""); 
    sprintf(msg, "%d tiepoints read from %s", n_overlaps, tptlist);
    zvmessage(msg,"");

    // Look up for number of active tie points. Tiepoints have to be
	// either Traditional,  XYZ Dynamic, Z Surface,  or Miss, to be 
	// usable for keyframe selection 
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

    // Each tiepoint is composed of two observations (left and right).
	// From the list of tiepoints, we need to generates a list of 
	// track. A track contains all observations that *look* at the same
	// ground point. Each observation needs to be uniquely identified.
	// To do that we generate a unique ID for each of them which is a
	// string concatenation of imageID_sample_line, and associates
	// (std::map) it with a structure containing image index, sample,
    // line.

    map<string,observation> mapOfPoints;
    observation infoPoint;
    vector<vector<string> > tiePointsVect;

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

           PigPoint XYZtie = tiepoints[i].xyz;
           infoPoint = {tiepoints[i].left_image,
                        tiepoints[i].left_sample,
                        tiepoints[i].left_line,
                        tiepoints[i].quality,
                        tiepoints[i].interactive, 
                        XYZtie.getX(), 
                        XYZtie.getY(), 
                        XYZtie.getZ()};
           mapOfPoints.insert(std::make_pair(obs,infoPoint));

        }
        else if (tiepoints[i].type == TIEPOINT_TRADITIONAL || 
                 tiepoints[i].type == TIEPOINT_Z_SURFACE || 
                 tiepoints[i].type == TIEPOINT_DYNAMIC_XYZ || 
                 tiepoints[i].type == TIEPOINT_MISS_DISTANCE) {

           //generate a unique id for current left image pixel

           string leftObs = std::to_string(tiepoints[i].left_key) + 
				'_' + std::to_string(tiepoints[i].left_sample) + 
				'_' + std::to_string(tiepoints[i].left_line);

           //generate a unique id for current right image pixel

           string rightObs = std::to_string(tiepoints[i].right_key) + 
				'_' + std::to_string(tiepoints[i].corrected_sample) + 
				'_' + std::to_string(tiepoints[i].corrected_line);

           //Add pair to vector of tie points

           vector<string> tiePointPair = {leftObs, rightObs};
           tiePointsVect.push_back(tiePointPair);

           //Save pixels image id and samp/line information in a map

           infoPoint = {tiepoints[i].left_image,
                        tiepoints[i].left_sample,
                        tiepoints[i].left_line,
                        tiepoints[i].quality,
                        tiepoints[i].interactive}; 
           mapOfPoints.insert(std::make_pair(leftObs,infoPoint));

           infoPoint = {tiepoints[i].right_image,
                        tiepoints[i].corrected_sample,
                        tiepoints[i].corrected_line,
                        tiepoints[i].quality,
                        tiepoints[i].interactive}; 
           mapOfPoints.insert(std::make_pair(rightObs,infoPoint));
        }

        else {
           if (!alreadyFound) {
              zvmessage("Warning: Tiepoint must be TRAD, Fiducial, Z "
						"Surface, DYNAMIC, or MISS","");
              zvmessage("Unsupported tiepoint(s) are removed","");
              alreadyFound = 1;
           }
        }

    }

	//*****************************************************************
	// VERTICES
	//
	// Iterate over all cameras and output their camera center and 
	// orientation quaternion
	//*****************************************************************

	FILE *fout;
	if ((fout = fopen(g2ofilename, "w")) == NULL) {
		sprintf(msg, "Error opening output g2o file %s\n", 
				outfilename);
		zvmessage(msg, "");
		zabend();
	}

	// Save pointings to file

	MatrixXf pointing_params(nids, 7);
	for(int i=0; i<nids; i++) {

		pointing_in_all[i]->getPointingParameters(pparams_in[i],
						     PIG_MAX_PARAMS);

		// In order (S, V1, V2, V3, X, Y, Z):
		pointing_params(i, 0) = pparams_in[i][0];
		pointing_params(i, 1) = pparams_in[i][1];
		pointing_params(i, 2) = pparams_in[i][2];
		pointing_params(i, 3) = pparams_in[i][3];
		pointing_params(i, 4) = pparams_in[i][4];
		pointing_params(i, 5) = pparams_in[i][5];
		pointing_params(i, 6) = pparams_in[i][6];

		fprintf(fout, "VERTEX_SE3:QUAT %d %f %f %f %f %f %f %f\n",
					  i, pointing_params(i, 4), pointing_params(i, 5), 
					  pointing_params(i, 6), pointing_params(i, 1), 					  pointing_params(i, 2), pointing_params(i, 3), 					  pointing_params(i, 0));
	}


	//*****************************************************************
	// NODES
	//
	// Iterate over all tiepoints, for all active ones compute the
	// Fundamental matrix, Essential matrix and relative pose between 
	// all pairs of images
	//
	// Currently DOES NOT WORK for fiducial tiepoints, since we need 
	// left AND right samples to compute a fundamental matrix
	//
	//*****************************************************************

	// Get image dimensions
    // Open each file, load image in memory, close file and free memory

	MatrixXi imagesizes = MatrixXi::Zero(nids, 2);
  
#pragma omp parallel for schedule(dynamic) private(msg) if (omp_on)
    for (int i=0; i<nids; i++) {

        int ns, nl;
#pragma omp critical
{
        // If current file is open, close it first

        if (file_models[i]->isFileOpen()) {
           file_models[i]->closeFile();
           zvclose(file_models[i]->getUnit(), "CLOS_ACT","FREE", NULL);
        }

        // Open the file for reading

        zvopen(file_models[i]->getUnit(), "OP", "READ", "U_FORMAT",
									"REAL", "OPEN_ACT", "SA", NULL);
        file_models[i]->setFileOpen(TRUE);

        // Get image size

        ns = file_models[i]->getNS();
        nl = file_models[i]->getNL();
		imagesizes(i,0) = ns;
		imagesizes(i,1) = nl;

        // Close the file as it is not needed anymore

        file_models[i]->closeFile();
        zvclose(file_models[i]->getUnit(), "CLOS_ACT","FREE", NULL);
}
	}

	// Obtain all camera position and orientation vectors, and CAHV parameters
	// Also, compute the horizontal and vertical focal lengths and center of 
	// projection, which will be used later in the creation of the epipolar
	// geometry's Essential matrix
	//
	// NOTE: it's more efficient to pre-compute these now than to do this 
	// inside computeEssential, which is called for every valid pair of images

	double hs, vs, hc, vc;
	PigPoint c_point;
    PigVector a_vector, h_vector, v_vector;
	MatrixXf focal_lengths(nids, 2);
	MatrixXf projection_centers(nids, 2);

	for(int i=0; i<nids; i++) {

		// Get the camera position and orientation vectors ('C' and 'A' from 
		// CAHVORE):

		PigCameraModel* cmod = camera_in_all[i];

		// Now, extract the CAHV parameters:

 		((PigCAHV*)cmod)->getCurrentCAHV(c_point, a_vector, 
                                         h_vector, v_vector);

		// Compute the horizontal and vertical focal lengths:
		//
		// hs = || A x H || (horizontal) -> fx
		// vs = || A x V || (vertical)   -> fy

		PigVector acrossh = a_vector * h_vector; // PigVector cross product
		hs = sqrt(acrossh.getX()*acrossh.getX() + 
				  acrossh.getY()*acrossh.getY() +
				  acrossh.getZ()*acrossh.getZ());
		PigVector acrossv = a_vector * v_vector; 
		vs = sqrt(acrossv.getX()*acrossv.getX() + 
				  acrossv.getY()*acrossv.getY() +
				  acrossv.getZ()*acrossv.getZ());
		focal_lengths(i,0) = hs;
		focal_lengths(i,1) = vs;

		// Compute the center of projection:
		//
		// hc = A dot H = image_width/2  + x0/delta_x
		// vc = A dot V = image_height/2 - y0/delta_y

		hc = a_vector % h_vector; // PigVector dot product
		vc = a_vector % v_vector;
		projection_centers(i,0) = hc;
		projection_centers(i,1) = vc;

	}

	// Create and compute a connectivity matrix, containing the number 
	// of matches per image pair:

	MatrixXi connectivity_matrix = 		
		computeConnectivityMatrix(tiepoints,n_overlaps,nids);		
	
	// Compute the maximum connectivity value:

	int max_connectivity = 0;
	MatrixXi::Index maxIndex[nids]; 
	VectorXi maxVal(nids);
	for(int j = 0; j<nids; j++) {
		maxVal(j) = connectivity_matrix.row(j).maxCoeff(&maxIndex[j]);
		if(maxVal(j) > max_connectivity) 
			max_connectivity = maxVal(j);
	}

	// Compute fundamental matrices, essential matrices, and the relative
	// pose for all pairwise combinations of images
	//
	// NOTE: This way of doing things is not too efficient, but works.
	// Using iterators is probably better (but need tracks?), something 	
	// like:
	// for (j=0; j<currentTrackSize; j++) {
	//	   it1 = mapOfPoints.find(tracks[i][j]);

	sprintf(msg, "\nComputing relative poses between all pairs");
	zvmessage(msg, "");

	std::array<float, 4> Q;
	std::array<float, 3> T;
	int pair_count = 0;
    for(int j=0; j<nids; j++) {
	    for(int k=0; k<nids; k++) {			
			float info_value = 0.0;
			if(connectivity_matrix(j,k) != 0) {
				sprintf(msg, "\nEvaluating pair (%d, %d)", j, k);
				zvmessage(msg, "");
				int status = computeRelativePose(tiepoints, n_overlaps, j, 
								k, edge_discard, imagesizes,
								focal_lengths, projection_centers, Q, T,
								info_value, max_connectivity, 
								pointing_params, use_essential,
								max_rand_noise, max_quat_error, 
								max_pos_error);

				// IMPORTANT NOTE: g2o files require an "information matrix" 					// for each pair, which is related to the correlation between
				// them and is computed as the inverse of the respective
				// covariance matrix. It is a 6x6 matrix but is usually 
				// provided as the 21-element upper-triangular part of the 
				// matrix, for example:
				//
				//		info = '1 0 0 0 0 0 1 0 0 0 0 1 0 0 0 1 0 0 1 0 1'
				//
				// For simplicitly, we will assume that the matrix is diagonal,
				// such that only one single information value (the inverse
				// of covariance) needs to be obtained and plugged into the 
				// output file

			    // Append the node information to the g2o file:

				if(status == 0) {
					double info = 1.0; // Identity matrix
					fprintf(fout, "EDGE_SE3:QUAT %d %d %f %f %f %f %f %f %f "
								  "%f 0 0 0 0 0 %f 0 0 0 0 %f 0 0 0 %f 0 0 %f "
								  "0 %f\n",
						  		  j, k, T[0], T[1], T[2],
								  Q[0], Q[1], Q[2], Q[3],
								  info, info, info, info, info, info); 
							      //
								  // Note that there are multiple ways to set 
								  // the information matrix, but setting all 
								  // to identity for now. Could also use:
								  //
								  //info_value, info_value, info_value, 
								  //info_value, info_value, info_value);  
								  // -OR-
								  //info, info, info, 
								  //info_value, info_value, info_value);  
					pair_count++;
				}
				
			}
		}
	}

	fclose(fout);

	// If specified, save the connectivity matrix to file:

	if(connectivity_save) {

		FILE *fout_info;
		char *outfilename_info = "connectivity.txt";
		if ((fout_info = fopen(outfilename_info, "w")) == NULL) {
			sprintf(msg, "Error opening output keyframe file %s\n", 
					outfilename);
			zvmessage(msg, "");
			zabend();
		}

		fprintf(fout_info, "\nConnectivity (matches per pair):\n");
		fprintf(fout_info, "image ");
		for (i=0; i<nids; i++) {
			//fprintf(fout_info, "%d:  ", i);
			if(i >= 100)
				fprintf(fout_info, "%d: ", i);
			else if(i >= 10)
				fprintf(fout_info, "%d:  ", i);
			else
				fprintf(fout_info, "%d:   ", i);
		}
		fprintf(fout_info, "\n");
		for (i=0;i<nids;i++) {
			if(i >= 100)
				fprintf(fout_info, " %d: ", i);
			else if(i >= 10)
				fprintf(fout_info, "  %d: ", i);
			else
				fprintf(fout_info, "   %d: ", i);
			for (j=0;j<nids;j++) 	{
				if(connectivity_matrix(i,j) >= 1000)
					fprintf(fout_info, "%d ",
						connectivity_matrix(i,j)); 
				else if(connectivity_matrix(i,j) >= 100)
					fprintf(fout_info, "%d  ",
						connectivity_matrix(i,j)); 
				else if(connectivity_matrix(i,j) >= 10)
					fprintf(fout_info, "%d   ",
						connectivity_matrix(i,j)); 
				else
					fprintf(fout_info, "%d    ",
						connectivity_matrix(i,j)); 
			}
			fprintf(fout_info, "\n");
		}

		fclose(fout_info);
	}
	
	
	///////////////////////////////////////////////////////////////////////////
	// Next, open the resulting g2o file from above and apply Ceres
	// pose graph optimization
	
    sprintf(msg, "\n\nProcessing input g2o file: %s", g2ofilename);
    zvmessage(msg, "");
 
	// Read the input file. First, need to initialize poses and constraints
	// containers to read into them from the file:

    MapOfPoses poses;
    VectorOfConstraints constraints;
	CHECK(ReadG2oFile(g2ofilename, &poses, &constraints))
    		<< "Error reading the file: " << g2ofilename;

	sprintf(msg, "Number of poses: %d", poses.size());
    zvmessage(msg, "");

	sprintf(msg, "Number of constraints: %d", constraints.size());
    zvmessage(msg, "");

	// Output the input poses into a txt file:

	if(poses_disp) {
		char *g2ofilename_original = "poses_original.txt";
		CHECK(OutputPoses(g2ofilename_original, poses))
        	<< "Error outputting to the original poses file";
	}

	// Build the optimization problem:

    ceres::Problem problem;
	BuildOptimizationProblem(constraints, &poses, &problem);

	// Check to see if the solve was successful or not:

	CHECK(SolveOptimizationProblem(&problem, summary_disp, solver_name))
        << "The solve was not successful, exiting.";

	// Output the optimized poses to a file:

	if(poses_disp) {
		char *g2ofilename_optimized = "poses_optimized.txt";
    	CHECK(OutputPoses(g2ofilename_optimized, poses))
        	<< "Error outputting to the optimized g2o file";
	}


	///////////////////////////////////////////////////////////////////////////
	// Finally, save the updated pointings to a nav file:
	
	// Read output filename:

    FILE *fout_nav;
	if ((fout_nav = fopen(outfilename, "w")) == NULL) {
		sprintf(msg, "Error opening output nav file %s\n", 
				outfilename);
		zvmessage(msg, "");
		zabend();
	}

	// Obtain and save optimized poses:

	double p[PIG_MAX_PARAMS];      
    double * pointingsParams;
    int * numPointingParams;
  	pointingsParams = new (std::nothrow) double[nids * PIG_MAX_PARAMS];
    numPointingParams = new (std::nothrow) int[nids];

	// Save each of the poses to the pointingParams vector:

	int ii=0;
	for (std::map<int, Pose3d, std::less<int>,
        Eigen::aligned_allocator<std::pair<const int, Pose3d>>>::
        const_iterator poses_iter = poses.begin();
        poses_iter != poses.end();
        ++poses_iter) {
    	const std::map<int, Pose3d, std::less<int>,
        Eigen::aligned_allocator<std::pair<const int, Pose3d>>>::
        value_type& pair = *poses_iter;
		
  		p[0] = pair.second.q.w(); // SCALAR FIRST; (S, V1, V2, V3, X, Y, Z)
		p[1] = pair.second.q.x();
		p[2] = pair.second.q.y();
		p[3] = pair.second.q.z();   
		p[4] = pair.second.p.transpose()[0];
		p[5] = pair.second.p.transpose()[1];
		p[6] = pair.second.p.transpose()[2];
 
		for(int j=0; j<7;j++) {
        	pointingsParams[ii*7+j]=p[j];
        }
		numPointingParams[ii] = 7;
		ii++;    
    }
	
	// Set the new pointings from the pointingParams vector:

    for(int i=0; i<nids; i++) {
    	int n = numPointingParams[i];
        (pointing_in_all[i])->setPointingParameters(pointingsParams + 
                                                          i*7, n);
    }

	// Save the pointings to file - This is the main output of the program:
  
    save_pointing(xml_mode, outfilename, nids, pointing_in_all,
        pparams_in_save, file_models, surface_model, solution_id, originalCM);

}  //end of main


//*********************************************************************
//*********************************************************************


// Sort a vector in descending order and get the sorted indices

template <typename T>
vector<size_t> sort_indeces(const vector<T> &v) {

    // initialize original index locations
    vector<size_t> idx(v.size());
    iota(idx.begin(), idx.end(), 0);

    // stable_sort avoids unnecessary index re-orderings when 'v' contains
 	// elements of equal values. Use return v[i1] > v[i2] for ascending order
    stable_sort(idx.begin(), idx.end(),
       [&v](size_t i1, size_t i2) {return v[i1] > v[i2];});

    return idx;

}


// Compute the "Essential matrix" from a fundamental matrix plus focal lengths
// and centers of projection in the line and sample directions 

MatrixXf computeEssential(std::array<float, 9> & F, 
					 const int imageindex1, const int imageindex2,
					 const MatrixXf focal_lengths,
					 const MatrixXf projection_centers) {

	// The Essential matrix 'E' is computed as follows:
	//     E = K2^T * F * K1		
	//
	// where 'F' is the funcamental matrix for a given image pair and 'K'
	// is the camera calibration matrix (pinhole model) for each camera:
	//
	// K = [hs  0  hc]     //hs, vs = horizontal, vertical focal lengths
	//     [0  vs  vc]     //hc, vc = horizontal, vertical center of projection
	//     [0   0   1]

	MatrixXf fundamental(3,3);
	fundamental << F[0], F[1], F[2],
				   F[3], F[4], F[5],
				   F[6], F[7], F[8];

	double hs1 = focal_lengths(imageindex1, 0);
	double vs1 = focal_lengths(imageindex1, 1);
	double hc1 = projection_centers(imageindex1, 0);
	double vc1 = projection_centers(imageindex1, 1);
	MatrixXf K1(3,3);

	K1 << hs1, 0.0, hc1, 
	      0.0, vs1, vc1, 
		  0.0, 0.0, 1.0;

	double hs2 = focal_lengths(imageindex2, 0);
	double vs2 = focal_lengths(imageindex2, 1);
	double hc2 = projection_centers(imageindex2, 0);
	double vc2 = projection_centers(imageindex2, 1);
	MatrixXf K2(3,3);

	K2 << hs2, 0.0, hc2, 
	      0.0, vs2, vc2, 
		  0.0, 0.0, 1.0;

	MatrixXf E = K2.transpose() * fundamental * K1;

	return E;

}


// Decompose the "Essential matrix" into a relative rotation and translation
// between a pair of images 

int decomposeEssential(const MatrixXf E, 
					   std::array<float, 4> & Q, 
					   std::array<float, 3> & T,
					   const int j, const int k, const MatrixXf pointings,
					   const double max_rand_noise, 
					   const double max_quat_error, 
					   const double max_pos_error) {

	// Generate small random numbers:

	std::random_device rd; // obtain a random number from hardware
    std::mt19937 gen(rd()); // seed the generator
    std::uniform_int_distribution<> distr(-max_rand_noise, max_rand_noise); 

	// First, read in the pointings for the curent pair, as this will be used
	// to evaluate the quality of the obtained rotation and translation

	float qw1 = pointings(j, 0); // S
	float qx1 = pointings(j, 1); // V1
	float qy1 = pointings(j, 2); // V2
	float qz1 = pointings(j, 3); // V3
	float x1 = pointings(j, 4);  // X
	float y1 = pointings(j, 5);  // Y
	float z1 = pointings(j, 6);  // Z
	Quaternionf qj;
	qj.x() = qx1; qj.y() = qy1; qj.z() = qz1; qj.w() = qw1;
	Vector3f tj;
	tj.x() = x1; tj.y() = y1; tj.z() = z1;

	float qw2 = pointings(k, 0); // S
	float qx2 = pointings(k, 1); // V1
	float qy2 = pointings(k, 2); // V2
	float qz2 = pointings(k, 3); // V3
	float x2 = pointings(k, 4);  // X
	float y2 = pointings(k, 5);  // Y
	float z2 = pointings(k, 6);  // Z
	Quaternionf qk;
	qk.x() = qx2; qk.y() = qy2; qk.z() = qz2; qk.w() = qw2;
	Vector3f tk;
	tk.x() = x2; tk.y() = y2; tk.z() = z2;

	// Relative quaternion:
	Quaternionf qjk = qj.inverse() * qk;
	
	// Next, compute the rotation matrix corresponding to this quaternion:
	Matrix3f relrotation = qjk.toRotationMatrix();

	// Relative translation:
	Vector3f tjk = relrotation.transpose()*(tk - tj);
	
	float baseline = tjk.norm();

    // Next, solve for Ae = 0 using SVD

    JacobiSVD<MatrixXf> svd(E, ComputeThinU | ComputeThinV);
	Matrix3f U = svd.matrixU();
	Matrix3f V = svd.matrixV();

	// Compute the rank of E, which should be 2 in theory:
	int rank = svd.rank();

    // The singular vector corresponding to the smallest singular value
	// is the solution (F), and corresponds to the last column of the V
	// matrix. Note that F is here with respect to the centered/scaled 
	// ref system. 

	// Next, the following matrices need to be initialized:

	Matrix3f G;
	G << 0.0, 1.0, 0.0,
	    -1.0, 0.0, 0.0,
		 0.0, 0.0, 1.0;    

	Matrix3f W;
	W << 0.0, -1.0, 0.0,
		 1.0,  0.0, 0.0,
		 0.0,  0.0, 1.0;

	Matrix3f Z;
	Z << 0.0, 1.0, 0.0,
	    -1.0, 0.0, 0.0,
		 0.0, 0.0, 0.0;


	// Four cases will be analyzed, the four combinations (R1, T1), (R1, T2),
	// (R2, T1), (R2, T2), where
	//
	// 		R1 = U*W*V^T
	// 		R2 = U*W^T*V^T
	// 		T1 = U last column
	// 		T2 = -U last column

	Matrix3f R1 = U*W*V.transpose();
	Matrix3f R2 = U*W.transpose()*V.transpose();
	Vector3f T1 = U.col(2);  // last column of U
	Vector3f T2 = -U.col(2); // last column of U

	Quaternionf q1(R1);
	q1.normalize();
	
	Quaternionf q2(R2);
	q2.normalize();
	
	// Normally, a depth test is performed to find the (R, t) pair which yields
	// points in front of both cameras, known as a cheirality test. However, 
	// since we have telemetry data (pointings), we can simplify things by 
	// looking for the pair which most "resembles" the relative translation 
	// 'tjk' and orientation 'qjk' obtained from the telemetry readings for
	// the pair. If there is such a pair, set a flag to "good". If none of the
	// pairs satisfy the similarity constraint, we can use the relative
	// translation and orientation from telemetry, but we MUST add noise to it
	// so that the edge information differs from the vertices and an error
	// can be computed by Ceres. 

	Quaternionf quatdiff;
	double quatdiffw1, quatdiffw2;
	double posdiffnorm1, posdiffnorm2;

	// q1
	quatdiffw1 = (q1.inverse() * qjk).w(); 

	// q2
	quatdiffw2 = (q2.inverse() * qjk).w(); 

	// T1
	posdiffnorm1 = (T1*baseline - tjk).norm();

	// T2
	posdiffnorm2 = (T2*baseline - tjk).norm();

	// Choose the best combination:

	// Quaternions
	// Note: CAREFUL! Eigen's constructor ordering is [w, x, y, z], 
	// and the coeffs() ordering ordering is [x, y, z, w]

	if((quatdiffw1 > quatdiffw2) && (quatdiffw1 > 1.0-max_quat_error)) {
		Q[0] = q1.x();
		Q[1] = q1.y();
		Q[2] = q1.z();
		Q[3] = q1.w();
	}
	else if((quatdiffw2 > quatdiffw1) && 
			(quatdiffw2 > 1.0-max_quat_error)) {
		Q[0] = q2.x();
		Q[1] = q2.y();
		Q[2] = q2.z();
		Q[3] = q2.w();
	}
	else { // the relative telemetry is best; add noise
		qjk.x() += distr(gen);
		qjk.y() += distr(gen);
		qjk.z() += distr(gen);
		qjk.w() += distr(gen);
		qjk.normalize();
		Q[0] = qjk.x();
		Q[1] = qjk.y();
		Q[2] = qjk.z();
		Q[3] = qjk.w();
	}
 
	// Translations:
	// IMPORTANT: The decomposition of the Essential matrix will return a 
	// unit translation vector. We multiply it by the actual baseline between
	// the pair of images to provide a more accurate input to Ceres:

	if((posdiffnorm1 < posdiffnorm2) && 
	   (posdiffnorm1 < max_pos_error*baseline)) {
		T[0] = T1.x()*baseline;
		T[1] = T1.y()*baseline;
		T[2] = T1.z()*baseline;
	}
	else if((posdiffnorm2 < posdiffnorm1) && 
			(posdiffnorm2 < max_pos_error*baseline)) {
		T[0] = T2.x()*baseline;
		T[1] = T2.y()*baseline;
		T[2] = T2.z()*baseline;
	}
	else { // the relative telemetry is best; add noise
		tjk.x() += distr(gen);
		tjk.y() += distr(gen);
		tjk.z() += distr(gen);
		T[0] = tjk.x();
		T[1] = tjk.y();
		T[2] = tjk.z();
	}

	return 0;

}


// Compute the "Fundamental matrix" from a set of tiepoints

int computeFundamental(const std::vector<TiePoint> & ties, 
                      std::array<float, 9> & F, unsigned char* used,
					  float &cond_number) {

	float txL, tyL, txR, tyR, sL, sR;
	/*
	Line = x
	Sample = y
	Left = L
	Right = R -> corrected
	->
	xL = left_line
	xR = corrected_line
	yL = left_sample 
	yR = corrected_sample
	*/

	int nbTies = ties.size();
    int nbTies2 = 2 * nbTies;

    // Need at least 8 ties to compute F

    if (nbTies < 8)
       return 1;

    // [1] Normalization of the input data
    // Recenter and rescale the (x,y) coordinates of one reference 
	// (e.g., Left image) such that points are centered around 0, and 
	// the average distance between points and 0 is sqrt(2). We have a
	// translation+scale transform that we store. Do same thing with
	// points of the other reference (e.g., Right image) 
      
    // Compute the centroid of R and L points

    txL = tyL = txR = tyR = 0.0;
    for (auto p:ties) {
        txL += p.left_line;
        tyL += p.left_sample;
        txR += p.corrected_line;
        tyR += p.corrected_sample;
    }
    txL /= (float)(-nbTies);
    tyL /= (float)(-nbTies);
    txR /= (float)(-nbTies);
    tyR /= (float)(-nbTies);

    // Compute the scaling factor such that the average distance of the
	// points with respect to the centroid is sqrt(2) for both L and R

    sL = sR = 0.0;
    for (auto p:ties) {
        sL += sqrt((p.left_line + txL) * (p.left_line + txL) + 
		(p.left_sample + tyL) * (p.left_sample + tyL));
        sR += sqrt((p.corrected_line + txR) * (p.corrected_line + txR) + 
		(p.corrected_sample + tyR) * (p.corrected_sample + tyR));
    }

    if (sL == 0)
        return 1;
    else 
        sL  = sqrt(2) * (float)nbTies / sL; 


    if (sR == 0)
        return 1;
    else 
        sR  = sqrt(2) * (float)nbTies / sR; 

    // [2] Compute the new coordinates of the translated/scaled points 
	// and store them directly in a matrix form following the DLT
	// algorithm and solve for the fundamental matrix F, using the
	// "Normalized 8-point algorithm". This provides 8 equations, where
	// the ninth fundamental matrix parameter at (3 ,3) is set to 1.
	//
	// The fundamental matrix is of the form x'^T*F*x = 0, where x are 
	// the left image coordinates, and x' for the right. By writing out
	// the resulting equations, we get a system of the form Af = 0, 
	// where 'A' is the data matrix and 'f' is the vector form of F

    MatrixXf A(nbTies, 9);
    for (int i=0; i < nbTies; i++ ) {
		float normxL = sL*(ties[i].left_line + txL);
		float normyL = sL*(ties[i].left_sample + tyL);
		float normxR = sR*(ties[i].corrected_line + txR);
		float normyR = sR*(ties[i].corrected_sample + tyR);
		A(i,0) = normxR*normxL;
		A(i,1) = normxR*normyL;
		A(i,2) = normxR;
		A(i,3) = normyR*normxL;
		A(i,4) = normyR*normyL;
		A(i,5) = normyR;
		A(i,6) = normxL;
		A(i,7) = normyL;
		A(i,8) = 1.0;
    }

    // Solve for Af = 0 using SVD

    JacobiSVD<MatrixXf> svd(A, ComputeThinU | ComputeThinV);

	// Compute the rank of A:

	int rank = svd.rank();
	if(rank < 9) {

		// Matrix not full rank! Return since rank-deficient SVD can 
		// fail

		return 1;
	}
	
	// Compute the condition number of A:

	cond_number = svd.singularValues()(0) / 
		   svd.singularValues()(svd.singularValues().size()-1);

    // The singular vector corresponding to the smallest singular value
	// is the solution (F), and corresponds to the last column of the V
	// matrix. Note that F is here with respect to the centered/scaled 
	// ref system. Let's call it Fcentered

    MatrixXf m = svd.matrixV();
	
    // [3] Use the transform/scales to transform back the Fcentered to 
	// the original coordinate system 
	//
    //      F = (TR)^-1 Fcentered TL
	//
    // with TL the "translation then scale" tranform matrix for the L
	// coord sys and TR^-1, the inverse of the "translation then scale"
	// transform matrix for the R coord sys
    //      Ftemp = Fcentered TL

    F[0] = m(0,m.cols()-1)*sL;
    F[1] = m(1,m.cols()-1)*sL;
    F[2] = m(0,m.cols()-1)*sL*txL + m(1,m.cols()-1)*sL*tyL + 
			 				m(2,m.cols()-1);
    F[3] = m(3,m.cols()-1)*sL;
    F[4] = m(4,m.cols()-1)*sL;
    F[5] = m(3,m.cols()-1)*sL*txL + m(4,m.cols()-1)*sL*tyL + 	
		   				m(5,m.cols()-1);
    F[6] = m(6,m.cols()-1)*sL;
    F[7] = m(7,m.cols()-1)*sL;
    F[8] = m(6,m.cols()-1)*sL*txL + m(7,m.cols()-1)*sL*tyL + 	
			m(8,m.cols()-1);
	
    // (TR)^-1 Ftemp
    float txR_inv = -txR*sR;
    float tyR_inv = -tyR*sR;
    float sR_inv = 1.0/sR;
    F[0] = sR_inv * F[0] + sR_inv * txR_inv * F[6];
    F[1] = sR_inv * F[1] + sR_inv * txR_inv * F[7];
    F[2] = sR_inv * F[2] + sR_inv * txR_inv * F[8];
    F[3] = sR_inv * F[3] + sR_inv * tyR_inv * F[6];
    F[4] = sR_inv * F[4] + sR_inv * tyR_inv * F[7];
    F[5] = sR_inv * F[5] + sR_inv * tyR_inv * F[8];

    return 0;
}


// Calculate the error for a given fundamental matrix

float calculateFundamentalError(const vector<TiePoint> & ties, 	
			std::array<float, 9> & F, unsigned char* used, float& var) {

	float n = ties.size(); 
	float total_error = 0.0f;
	int number_used = 0;
	int number_good = 0;

	Matrix3d Fmatrix;
	Fmatrix << F[0], F[1], F[2],
			   F[3], F[4], F[5],
			   F[6], F[7], F[8];

	for(int i = 0; i<n; i++) {
		number_used++;
		Vector3d left_pixel;
		left_pixel(0) = ties[i].left_line;   //x
		left_pixel(1) = ties[i].left_sample; //y
		left_pixel(2) = 1.0f;                //z
		Vector3d line = Fmatrix*left_pixel;
		Vector3d right_pixel;
		right_pixel(0) = ties[i].right_line;    //x
		right_pixel(1) = ties[i].right_sample;  //y
		right_pixel(2) = 1.0f;                  //z

		float err1 = (line(0)*right_pixel(0) + 
					  line(1)*right_pixel(1) + 
					  line(2))/sqrt(line(0)*line(0)+line(1)*line(1));
		line = Fmatrix.transpose()*right_pixel;
		float err2 = (line(0)*left_pixel(0) + 
					  line(1)*left_pixel(1)	+ 
		   			  line(2))/sqrt(line(0)*line(0)+line(1)*line(1));
		// Changed to sqrt: 			
		float error = sqrt(err1*err1 + err2*err2);
		if(sqrt(error) < 1.0f) {
			number_good++;
		}

		total_error += error;
		number_used++;
	}

	total_error = total_error / (float)number_used;

	var = 0.0f;
	for(int i = 0; i < n; i++) {
		Vector3d left_pixel;
		left_pixel(0) = ties[i].left_line;   //x
		left_pixel(1) = ties[i].left_sample; //y
		left_pixel(2) = 1.0f;                //z
		Vector3d line = Fmatrix*left_pixel;
		Vector3d right_pixel;
		right_pixel(0) = ties[i].right_line;    //x
		right_pixel(1) = ties[i].right_sample;  //y
		right_pixel(2) = 1.0f;                  //z

		float err1 = (line(0)*right_pixel(0) + 
					  line(1)*right_pixel(1) + 
		 			  line(2))/sqrt(line(0)*line(0)+line(1)*line(1));
		line = Fmatrix.transpose()*right_pixel;
		float err2 = (line(0)*left_pixel(0) + 
					  line(1)*left_pixel(1)	+ 
					  line(2))/sqrt(line(0)*line(0)+line(1)*line(1));
		// Changed to sqrt: 			
		float error = sqrt(err1*err1 + err2*err2);

		var += (pow((total_error - error), 2)/(float)number_used);
	}

	return total_error;
}


// Compute a connectivity matrix, which tallies the number of tiepoints
// across all image pairs. The resulting matrix is NxN, where N 
// corresponds to the number of images

MatrixXi computeConnectivityMatrix(TiePoint *tiepoints, const int n_overlaps, 
	int nids) {

	MatrixXi connectivity_matrix = MatrixXi::Zero(nids, nids);

	for(int j=0; j<nids; j++) {
		for(int k=0; k<nids; k++) {	
			int count = 0;
			for (int i=0; i<n_overlaps; i++) {

				if (!tiepoints[i].active)
			  		continue;

				if (tiepoints[i].type == TIEPOINT_FIDUCIAL) {
					zvmessage("FIDUCIAL tiepoints not currently "
							  "supported","");
					continue;
				}
				else if (tiepoints[i].type == TIEPOINT_TRADITIONAL || 
			       		 tiepoints[i].type == TIEPOINT_Z_SURFACE || 
			       		 tiepoints[i].type == TIEPOINT_DYNAMIC_XYZ || 
			       		 tiepoints[i].type == TIEPOINT_MISS_DISTANCE) {
					if(tiepoints[i].left_image == j && 						
					   tiepoints[i].right_image == k) {
						count++;
					}
				}
				else {   
		   			zvmessage("Warning: Tiepoint must be TRAD, Z "
							  "Surface, DYNAMIC, or MISS","");
				}
			}
			connectivity_matrix(j, k) = count;
		}
	}

	return connectivity_matrix;

}


// Compute the relative pose between a pair of images. See the PDF for 
// further details

int computeRelativePose(TiePoint *tiepoints, const int n_overlaps, 
	const int j, const int k, const float edge_discard, 
	const MatrixXi imagesizes, const MatrixXf focal_lengths, 
	const MatrixXf projection_centers,
	std::array<float, 4> &Q, std::array<float, 3> &T,
	float &info_value, const int max_connectivity, 
	const MatrixXf pointings, const int use_essential,
	const double max_rand_noise, const double max_quat_error, 
	const double max_pos_error) {

	int count = 0;
    char msg[256];

	// Get current image dimensions

	float currentwidth = (float)imagesizes(j,0);
	float currentheight = (float)imagesizes(j,1);

	// Store the tiepoints corresponding to each and all pairs of images

	vector<TiePoint> currentties;

	// Get the current pointing parameters:

	float cx_j = pointings(j,4);
	float cy_j = pointings(j,5);
	float cz_j = pointings(j,6);
	float qx_j = pointings(j,1);
	float qy_j = pointings(j,2);
	float qz_j = pointings(j,3);
	float qw_j = pointings(j,0);
	float cx_k = pointings(k,4);
	float cy_k = pointings(k,5);
	float cz_k = pointings(k,6);
	float qx_k = pointings(k,1);
	float qy_k = pointings(k,2);
	float qz_k = pointings(k,3);
	float qw_k = pointings(k,0);

	float baseline = sqrt((cx_j-cx_k)*(cx_j-cx_k) + 
					   (cy_j-cy_k)*(cy_j-cy_k) + 
					   (cz_j-cz_k)*(cz_j-cz_k));
	sprintf(msg, "     Baseline: %f", baseline);
    zvmessage(msg, "");

	// Get the tiepoints and number of tieponts; if too few, skip this pair

	for (int i=0; i<n_overlaps; i++) {

		if (!tiepoints[i].active)
		  	continue;

		if (tiepoints[i].type == TIEPOINT_FIDUCIAL) {
			zvmessage("FIDUCIAL tiepoints not currently supported","");
			continue;
		}
		else if (tiepoints[i].type == TIEPOINT_TRADITIONAL || 
		     	 tiepoints[i].type == TIEPOINT_Z_SURFACE || 
		       	 tiepoints[i].type == TIEPOINT_DYNAMIC_XYZ || 
		       	 tiepoints[i].type == TIEPOINT_MISS_DISTANCE) {

			if(tiepoints[i].left_image==j && 
			   tiepoints[i].right_image==k) {
					
				// Only keep matches not near image edges

				double ed = edge_discard;
				if((tiepoints[i].left_line > currentheight*ed) &&   
				   (tiepoints[i].left_sample > currentwidth*ed) &&
				   (tiepoints[i].left_line < currentheight*(1.0-ed)) &&
				   (tiepoints[i].left_sample < currentwidth*(1.0-ed)) &&
				   (tiepoints[i].corrected_line > currentheight*ed) &&   
				   (tiepoints[i].corrected_sample > currentwidth*ed) &&
				   (tiepoints[i].corrected_line < currentheight*(1.0-ed)) && 
				   (tiepoints[i].corrected_sample < currentwidth*(1.0-ed))) {
						currentties.push_back(tiepoints[i]);
						count++;
				}
			}
		}
		else {   
			zvmessage("Warning: Tiepoint must be TRAD, Z Surface, "
					  "DYNAMIC, or MISS","");
		}
	}

	// Need at least 8 matches for a fundamental matrix. In the case of using
	// telemetry only this should not matter, but relative poses should not be
	// computed for pairs that are not well connected anyway, and having the 
	// same pairs evaluated allows for an apples-to-apples comparison of
	// results 

	sprintf(msg, "     Number of valid tiepoints after discarding near "
				 "image edges: %d", currentties.size());
	zvmessage(msg, "");

	// Set the 'info_value' information matrix diagonal as the number of 
	// tiepoints between the current pair. This is simplistic, but better
	// than setting all such matrices to identity:

	info_value = (float)currentties.size();

	if(use_essential) {

		if (currentties.size() < 8) {
			zvmessage("     Fewer than 8 tiepoints, can't compute F!","");
			return 1;
		}

		// Fundamental matrix stored here:

		std::array<float, 9> F;

		for (int i=0; i<n_overlaps; i++) {

			if (!tiepoints[i].active)
		  		continue;

			if (tiepoints[i].type == TIEPOINT_FIDUCIAL) {
				zvmessage("FIDUCIAL tiepoints not currently supported","");
				continue;
			}
			else if (tiepoints[i].type == TIEPOINT_TRADITIONAL || 
		       		 tiepoints[i].type == TIEPOINT_Z_SURFACE || 
		       		 tiepoints[i].type == TIEPOINT_DYNAMIC_XYZ || 
		       		 tiepoints[i].type == TIEPOINT_MISS_DISTANCE) {

				if(tiepoints[i].left_image==j && 
				   tiepoints[i].right_image==k) {
					
					// Only keep matches not near image edges

					double ed = edge_discard;
					if((tiepoints[i].left_line > currentheight*ed) &&   
				   	(tiepoints[i].left_sample > currentwidth*ed) &&
					(tiepoints[i].left_line < currentheight*(1.0-ed)) &&
					(tiepoints[i].left_sample < currentwidth*(1.0-ed)) &&
					(tiepoints[i].corrected_line > currentheight*ed) &&   
					(tiepoints[i].corrected_sample > currentwidth*ed) &&
					(tiepoints[i].corrected_line < currentheight*(1.0-ed)) && 
					(tiepoints[i].corrected_sample < currentwidth*(1.0-ed))) {
						currentties.push_back(tiepoints[i]);
						count++;
					}
				}
			}
			else {   
			  	zvmessage("Warning: Tiepoint must be TRAD, Z Surface, "
						  "DYNAMIC, or MISS","");
			}
		}

		// Need at least 8 matches for a fundamental matrix

		sprintf(msg, "     Number of valid tiepoints after discarding near "
					 "image edges: %d", currentties.size());
		zvmessage(msg, "");
		if (currentties.size() < 8) {
			zvmessage("     Fewer than 8 tiepoints, can't compute F!","");
			return 1;
		}

		// Variances computed through error estimation

		float varF;

		// Condition number for the data matrix:

		float condnumberF;

		// Store locations of tiepoints that should not be used
		// Note: this capability is currently unused

		unsigned char* usedF = new unsigned char[currentties.size()];

		//Compute the fundamental matrix

		int status_F = computeFundamental(currentties, F, usedF, condnumberF);
		//sprintf(msg, "     Condition number - F: %f", condnumberF);
		//zvmessage(msg, "");

		// If this step failed, exit

		if(status_F == 1) {
			zvmessage("     Fundamental matrix estimation failed!","");
			return 1;
		}

		//Compute F error, from which we'll obtain the variance for F

		float error_F = calculateFundamentalError(currentties, F, usedF, varF);
		//sprintf(msg, "     Error - F: %f", error_F);
		//zvmessage(msg, "");

		// Additional checks

		// 1) No movement between consecutive frames: 
		
		float quatdiff = abs(qx_j-qx_k) + abs(qy_j-qy_k) + 
						 abs(qz_j-qz_k) + abs(qw_j-qw_k);
		if(baseline == 0.0 || quatdiff == 0.0) {
			zvmessage("     No relative motion between pair","");
			return 1;
		}

		// Bad condition number for the data matrix, which could result in an
		// inaccurate estimation of F:

		if( condnumberF > 1000.0 )  {
			zvmessage("     High condition number detected, no F computed","");
			return 1;
		}
		
		// Compute the Essential matrix:

		MatrixXf E = computeEssential(F, j, k, focal_lengths,
						 projection_centers);
		
		// Decompose the Essential matrix into relative rotation and
		// translation:

		int status_essential = decomposeEssential(E, Q, T, j, k, pointings,
				max_rand_noise, max_quat_error, max_pos_error); 

	}
	else {

		if (currentties.size() < 8) {
			zvmessage("     Fewer than 8 tiepoints, not well connected!","");
			return 1;
		}
		// Compute relative poses from the pointings for the curent pair:

		// Generate small random numbers

		std::random_device rd; // obtain a random number from hardware
    	std::mt19937 gen(rd()); // seed the generator
    	std::uniform_int_distribution<> distr(-max_rand_noise,
											   max_rand_noise); 

		Quaternionf qj;
		qj.x() = qx_j; qj.y() = qy_j; qj.z() = qz_j; qj.w() = qw_j;
		Vector3f tj;
		tj.x() = cx_j; tj.y() = cy_j; tj.z() = cz_j;

		Quaternionf qk;
		qk.x() = qx_k; qk.y() = qy_k; qk.z() = qz_k; qk.w() = qw_k;
		Vector3f tk;
		tk.x() = cx_k; tk.y() = cy_k; tk.z() = cz_k;

		// Relative quaternion:
		Quaternionf qjk = qj.inverse() * qk;
		
		// Next, compute the rotation matrix corresponding to this quaternion:
		Matrix3f relrotation = qjk.toRotationMatrix();

		// Relative translation:
		Vector3f tjk = relrotation.transpose()*(tk - tj);
		
		qjk.x() += distr(gen);
		qjk.y() += distr(gen);
		qjk.z() += distr(gen);
		qjk.w() += distr(gen);
		qjk.normalize();
		Q[0] = qjk.x();
		Q[1] = qjk.y();
		Q[2] = qjk.z();
		Q[3] = qjk.w();

		tjk.x() += distr(gen);
		tjk.y() += distr(gen);
		tjk.z() += distr(gen);
		T[0] = tjk.x();
		T[1] = tjk.y();
		T[2] = tjk.z();

	}

	// Finally, return if nothing else failed

	return 0;
		
}


// Constructs the nonlinear least squares optimization problem from the pose
// graph constraints

void BuildOptimizationProblem(const VectorOfConstraints& constraints,
                              MapOfPoses* poses,
                              ceres::Problem* problem) {

    CHECK(poses != NULL);
    CHECK(problem != NULL);
    if (constraints.empty()) {
        LOG(INFO) << "No constraints, no problem to optimize.";
        return;
    }

    ceres::LossFunction* loss_function = NULL;
    ceres::LocalParameterization* quaternion_local_parameterization = 
			new ceres::EigenQuaternionParameterization;
	
    for (VectorOfConstraints::const_iterator constraints_iter =
    		constraints.begin();
    	  constraints_iter != constraints.end();
          ++constraints_iter) {
    
    	const Constraint3d& constraint = *constraints_iter;
    	MapOfPoses::iterator pose_begin_iter = 
				poses->find(constraint.id_begin);
    	CHECK(pose_begin_iter != poses->end())
        	<< "Pose with ID: " << constraint.id_begin << " not found.";
    	MapOfPoses::iterator pose_end_iter = 
				poses->find(constraint.id_end);
    	CHECK(pose_end_iter != poses->end())
        	<< "Pose with ID: " << constraint.id_end << " not found.";
    	
		const Eigen::Matrix<double, 6, 6> sqrt_information =
	        constraint.information.llt().matrixL();

	    // Ceres will take ownership of the pointer:

	    ceres::CostFunction* cost_function =
	        PoseGraph3dErrorTerm::Create(constraint.t_be, sqrt_information);

	    problem->AddResidualBlock(cost_function,
                              loss_function,
                              pose_begin_iter->second.p.data(),
                              pose_begin_iter->second.q.coeffs().data(),
                              pose_end_iter->second.p.data(),
                              pose_end_iter->second.q.coeffs().data());

    	problem->SetParameterization(pose_begin_iter->second.q.coeffs().data(),
                                 quaternion_local_parameterization);
    	problem->SetParameterization(pose_end_iter->second.q.coeffs().data(),
                                 quaternion_local_parameterization);
    }

    // The pose graph optimization problem has six DOFs that are not fully 
	// constrained. This is typically referred to as 'gauge freedom'. A rigid 
	// body transformation can be applied to all the nodes and the optimization
	// problem will still have the exact same cost. The Levenberg-Marquardt
	// algorithm has internal damping which mitigates this issue, but it is 
	// better to properly constrain the gauge freedom. This can be done by 
	// setting one of the poses as constant so the optimizer cannot change it.

    MapOfPoses::iterator pose_start_iter = poses->begin();
    CHECK(pose_start_iter != poses->end()) << "There are no poses.";
    problem->SetParameterBlockConstant(pose_start_iter->second.p.data());
    problem->SetParameterBlockConstant(pose_start_iter->second.q.coeffs().data());

}


// Solve the optimization problem; returns true if the solve was successful

bool SolveOptimizationProblem(ceres::Problem* problem, 
	const string summary_disp, const string solver_name) {

    CHECK(problem != NULL);

    ceres::Solver::Options options;
    options.max_num_iterations = 200;

	// NOTE: sparse not working for some reason; try DENSE_NORMAL_CHOLESKY,
	// DENSE_QR, DENSE_SCHUR

    if (!StringToLinearSolverType(solver_name,
		&options.linear_solver_type)) {
        zvmessage("Solver type not supported.","");
        zabend();
    }

    ceres::Solver::Summary summary;
    ceres::Solve(options, problem, &summary);

    // Print in stdout a report on the optimization run

    if (summary_disp != "NO_SUM") {
        if (summary_disp == "BRIEF_SUM")
        	std::cout << summary.BriefReport() << '\n';
        else 
            std::cout << summary.FullReport() << '\n';
    }
    
    return summary.IsSolutionUsable();

}


// Output the poses to the file with format: id x y z q_x q_y q_z q_w

bool OutputPoses(const std::string& filename, const MapOfPoses& poses) {

    std::fstream outfile;
    outfile.open(filename.c_str(), std::istream::out);
    if (!outfile) {
    	LOG(ERROR) << "Error opening the file: " << filename;
    	return false;
  	}

  	for (std::map<int, Pose3d, std::less<int>,
         Eigen::aligned_allocator<std::pair<const int, Pose3d>>>::
         const_iterator poses_iter = poses.begin();
         poses_iter != poses.end();
         ++poses_iter) {
    	     const std::map<int, Pose3d, std::less<int>,
          	 Eigen::aligned_allocator<std::pair<const int, Pose3d>>>::
          	 value_type& pair = *poses_iter;

    	 outfile << pair.first << " " << pair.second.p.transpose() << " "
         		 << pair.second.q.x() << " " << pair.second.q.y() << " "
            	 << pair.second.q.z() << " " << pair.second.q.w() << '\n';
    }
  
	return true;

}


// Wrapper for save_pointing_file and save_pointing_xml (borrowed from marsnav2)

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


// Write out the pointing corrections table (borrowed from marsnav2).  
// Current pointings are obtained from the pointing model; pparams_orig array
// contains the original pointings before corrections were applied.
//
// File format:
// header:	"marsnav pointing corrections file, version 1"
// Each line is a space-separated list of numbers.  The first is an
// integer, n, which specifies the # of pointing params on this line.
// Then follow n original pointing values, followed by n corrected pointing
// values.

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


// Write out the pointing corrections table in XML format (borrowed from
// marsnav2).  
// Current pointings are obtained from the pointing model; pparams_orig array
// contains the original pointings before corrections were applied.
//
// File format:
// header:      <?xml version="1.0" encoding="UTF-8"?>

void save_pointing_xml(char *name, int n, PigPointingModel *pointings[],
		       double pparams_orig[][PIG_MAX_PARAMS], 
		       PigFileModel *files[], PigSurfaceModel *surface_model,
		       char *solution_id, int original_cm) 
{ 
    FILE *fout;
    int i, j, count;
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

    fprintf(fout, "    <purpose>A pointing correction file for one mosaic "
				  "might look for %s%s\n",
            mission, "</purpose>");
    fprintf(fout, "  </origination>\n");

    // This part may not be needed
 
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
        files[i]->getCameraModelCS(csRef, 1);
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
        for (j=0; j<count; j++)  // Print original params
            fprintf(fout, "        <parameter id=\"%s\" value=\"%.8lf\"/>\n", 
                    pointings[i]->getPointingParamName(j), pparams_orig[i][j]);
        fprintf(fout, "      </original_parameters>\n");


        // If saving the original camera model

        if (original_cm) {

           // Setting up original pointing

           pointings[i]->setPointingParameters(pparams_orig[i], count);

           PigPoint c_point;
           PigVector a_vector, h_vector, v_vector, o_vector, r_vector,
					 e_vector;
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
                         h_vector, v_vector, o_vector, r_vector, e_vector,
						 mtype, mparam);
           }
           else { // for camera_model = NONE
                 sprintf(msg, "Camera model type: %s\n", cm_type);
                 zvmessage(msg, "");
           }

           fprintf(fout, "      <original_camera_model type=\"%s\">\n",
				   cm_type);
           if (strncasecmp(cm_type, "CAHV", 4)==0) {
              fprintf(fout, "        <parameter id=\"C\" type=\"float_3\" ");
              fprintf(fout, 
				  "value1=\"%.8lf\" value2=\"%.8lf\" value3=\"%.8lf\"/>\n",
                  c_point.getX(), c_point.getY(), c_point.getZ());
   
              fprintf(fout, "        <parameter id=\"A\" type=\"float_3\" ");
              fprintf(fout, 
				  "value1=\"%.8lf\" value2=\"%.8lf\" value3=\"%.8lf\"/>\n", 
                  a_vector.getX(), a_vector.getY(), a_vector.getZ());

              fprintf(fout, "        <parameter id=\"H\" type=\"float_3\" ");
              fprintf(fout, 
				  "value1=\"%.8lf\" value2=\"%.8lf\" value3=\"%.8lf\"/>\n", 
                  h_vector.getX(), h_vector.getY(), h_vector.getZ());

              fprintf(fout, "        <parameter id=\"V\" type=\"float_3\" ");
              fprintf(fout, 
				  "value1=\"%.8lf\" value2=\"%.8lf\" value3=\"%.8lf\"/>\n", 
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
    
              fprintf(fout, "        <parameter id=\"T\" value=\"%d\"/>\n",
					  mtype);
              fprintf(fout, "        <parameter id=\"P\" value=\"%g\"/>\n",
					  mparam);
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
        for (j=0; j<count; j++) // Print corrected params
            fprintf(fout, "      <parameter id=\"%s\" value=\"%.8lf\"/>\n", 
                    pointings[i]->getPointingParamName(j), p[j]);

        fprintf(fout, "    </pointing_parameters>\n");


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
                                        h_vector, v_vector, o_vector,
										r_vector);
        }
        else if (strcasecmp(cm_type, "CAHVORE") == 0) {
            ((PigCAHVORE*)camera_model)->getCurrentCAHVORE(c_point, a_vector,
               h_vector, v_vector, o_vector, r_vector, e_vector, mtype,
			   mparam);
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
            fprintf(fout, "      <parameter id=\"P\" value=\"%g\"/>\n",
					mparam);
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

    // write out the surface model; one surface model allowed per solution id

    fprintf(fout, "  <surface_model type=\"%s\"", 
		surface_model->getModelName());
    fprintf(fout, " solution_id=\"%s\">\n", solution_id);
    double surf_pp[PIG_MAX_PARAMS];
    count = surface_model->getPointingParamCount();
    surface_model->getPointingParameters(surf_pp, count);

    for (int cnt = 0; cnt<count; cnt++)  // Print surface model params
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

