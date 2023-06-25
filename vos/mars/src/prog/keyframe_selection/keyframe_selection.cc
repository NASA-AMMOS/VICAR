
////////////////////////////////////////////////////////////////////////
// keyframe_selection - multimission keyframe selection program
////////////////////////////////////////////////////////////////////////

#include "vicmain_c"

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

//Added to avoid compilation errors
#include <map>
#include <valarray>
#include <iostream>
#include <fstream>
#include <vector>
#include <numeric>
#include <algorithm>

#ifdef _OPENMP
#include <omp.h>
#endif

#include <Eigen/Dense>   

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
    int num_images;		// # of images using this Site
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

// Homography/Fundamental matrix utilities:

int computeFundamental(const vector<TiePoint> & ties, 
                      std::array<float, 9> & F, unsigned char* used, 
					  float &cond_number);

float calculateFundamentalError(const vector<TiePoint> & ties, std::array<float, 9> & F, unsigned char* used, float& var);

int computeHomography(const vector<TiePoint> & ties, 
                      std::array<float, 9> & H, unsigned char* used, 
					  float &cond_number);

float calculateHomographyError(const vector<TiePoint> & ties, std::array<float, 9> & H, unsigned char* used, float& var);

float calculateGRIC(float e, float d, float r, float var, float limit);

float calculateFundamentalGRIC(const vector<TiePoint> & ties, 				std::array<float, 9> & F, unsigned char* used, float var);

float calculateHomographyGRIC(const vector<TiePoint> & ties, 
            std::array<float, 9> & H, unsigned char* used, float var);

int computeAffineTransform(const std::vector<tiePoint> & ties, 
                           std::array<float, 6> & outA);

MatrixXi computeConnectivityMatrix(TiePoint *tiepoints, const int n_overlaps, int nids);

int evalCostFunction(TiePoint *tiepoints, const int n_overlaps, const int j, const int k, const float edge_discard, const MatrixXd imagesizes, const MatrixXf camparams, float &score, const int max_connectivity);

int pickKeyframes(const MatrixXf &keyframe_scores, std::vector<int> &keyframe_locations, const int nids, int &keyframe_counter, 
const int selection_mode, const double sel_percent);  


////////////////////////////////////////////////////////////////////////
// Keyframe_selection program
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
    char tptlist[150]; 
    char mission[64], instrument[64];
    int xml_mode;
    int start_key;


    ////////////////////////////////////////////////////////////////////

    zvmessage("KEYFRAME SELECTION 2022-09-30", "");

    // Initialization of PIG elements.
    // Note that Pig surface model is not used in keyframe_selection. 
	// It is just retrieved here and passed along to save_pointing. 
	// This is to keep consistency across programs using .nav file 
	// and expecting a surface model
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

	// Get image dimensions
    // Open each file, load image in memory, close file and free memory
	MatrixXd imagesizes = MatrixXd::Zero(nids, 2);
  
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

    // Get parameter overrides, if any

    //zvparmd("ERROR", &permitted_error, &count, &def, 1, 0);

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

    zvp("START_KEY", &start_key, &count);

	// Currently unused, but can incorporate this if needed, similarly
	// to its use in marsnav2:
    //double parallel_limit;    
    //zvparmd("PARALLEL_LIMIT", &parallel_limit, &count, &def, 1, 0); 
    
 	// Output filename
    zvpone("OUT", outfilename, 1, sizeof(outfilename));

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
	// Entering code that is specific to "keyframe_selection"	
	//*****************************************************************

	// Display cost matrix?
	int cost_matrix_disp = zvptst("DISP_COST");

    // Display images connectivity matrix (matches per pair)?
    // If the images number is large, the matrix won't be displayed 
	// nicely
    int connectivity_disp = zvptst("DISP_CONNECT"); 

	// Get the factor (percentage) for discarding matches near image 
	// edges
	double edge_discard = 0.0;
    zvparmd("EDGE_DIST", &edge_discard, &count, &def, 1, 0); 

	// Selection mode, either sequential (default) or global
	int selection_mode = zvptst("SEQUENTIAL");	

	// For global processing, read in the percentage of top scores to select
	double sel_percent = 0.0;
    zvparmd("SEL_PERCENT", &sel_percent, &count, &def, 1, 0); 
	

    //All parameters retrieved, move on to analyzing the input data 

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

	// Get the CAHVORE parameters we'll need these later, namely the
	// 'C' position and 'A' orientation vectors:

	MatrixXf camparams(nids, 6);
	sprintf(msg, "\n'C' position and 'A' orientation parameters "
				 "per camera: ");
    zvmessage(msg,"");
	for(int i=0; i<nids; i++) {
		PigCameraModel* cmod = camera_in_all[i];
		PigVector Avector = cmod->getCameraOrientation(); //cam model cs
    	PigPoint Cposition = cmod->getCameraPosition();
    	sprintf(msg, "Camera %d, ('C'): (%f %f %f), ('A'): (%f %f %f)", 
        i, Cposition.getX(), Cposition.getY(), Cposition.getZ(), 	
		Avector.getX(), Avector.getY(), Avector.getZ());
    	zvmessage(msg, "");
		camparams(i,0) = Cposition.getX();
		camparams(i,1) = Cposition.getY();
		camparams(i,2) = Cposition.getZ();
		camparams(i,3) = Avector.getX();
		camparams(i,4) = Avector.getY();
		camparams(i,5) = Avector.getZ();
	}

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
	// Iterate over all tiepoints, for all active ones compute the
	// keyframe cost function between all pairs of images
	//
	// Currently DOES NOT WORK for fiducial tiepoints, since we need 
	// left AND right samples to compute a homography and fundamental
	// matrix
	//*****************************************************************
	
	// Create a matrix which will store all pairwise scores:
	MatrixXf keyframe_scores = MatrixXf::Zero(nids, nids);

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


	// Compute homographies, fundamental matrices, and the cost function
	// for all pairwise combinations of images
	//
	// NOTE: This way of doing things is not too efficient, but works.
	// Using iterators is probably better (but need tracks?), something 	
	// like:
	// for (j=0; j<currentTrackSize; j++) {
	//	   it1 = mapOfPoints.find(tracks[i][j]);

	sprintf(msg, "\nKeyframe cost function info for all pairs");
	zvmessage(msg, "");

	double max_score = -10.0;
    for(int j=0; j<nids; j++) {
	    for(int k=0; k<nids; k++) {			
			float score = 0.0;
			if(connectivity_matrix(j,k) != 0) {
				sprintf(msg, "\nEvaluating pair (%d, %d)", j, k);
				zvmessage(msg, "");
				int status = evalCostFunction(tiepoints, n_overlaps, j, 
								k, edge_discard, imagesizes, camparams,
								score, max_connectivity);

				// If the cost function evaluation failed, set to -1:
				if(status == 1)
					keyframe_scores(j, k) = -1.0;
				else 
					keyframe_scores(j, k) = score;

				// Keep track of the highest score:				
				if(score > max_score) max_score = score;
			}
		}
	}

	// Normalize valid (positive) scores by the highest score, so they're all
	// between 0 and 1:
	
	for(int j=0; j<nids; j++) {
	    for(int k=0; k<nids; k++) {		
		if(keyframe_scores(j, k) > 0.0) 
			keyframe_scores(j, k) /= max_score;
		}
	}

	// Next, compute the list of keyframes from the cost matrix:

	char **out_keyframe_filenames;
	int keyframe_counter = 0;

	// Location of keyframes:

	std::vector<int> keyframe_locations(nids, 0);
	int algotype = 2;
	int status_pick_kf = pickKeyframes(keyframe_scores, 
					keyframe_locations, nids, keyframe_counter,
					selection_mode, sel_percent);

	if(status_pick_kf == 0) {
		sprintf(msg, "\nFound %d keyframes.", keyframe_counter);
		zvmessage(msg, "");
		sprintf(msg, "\nFound the corresponding keyframe locations:");
		zvmessage(msg, "");
		for(int i=0; i<nids; i++) {
			if(keyframe_locations[i] == 1) {
				sprintf(msg, "%d ", i);
				zvmessage(msg, "");
			}
		}
		//zvmessage(msg, "");
	}
	else {
		sprintf(msg, "Keyframe selection failed!");
		zvmessage(msg, "");
		//zabend();
	}
	
	// Keyframe selection complete, proceed to output values:
	if(status_pick_kf == 0) {
		FILE *fout;
		if ((fout = fopen(outfilename, "w")) == NULL) {
			sprintf(msg, "Error opening output keyframe file %s\n", 
					outfilename);
			zvmessage(msg, "");
			zabend();
		}

		// Print out the keyframes that were selected to a 
		// keyframes file:

		for(int i=0; i<nids; i++) {
			if(keyframe_locations[i] == 1) {
				fprintf(fout, "%s\n", input_filenames[i]);  
			}
		}
		fclose(fout);
	}

	// If specified, print the keyframe scores matrix and/or the
	// connectivity matrix as well:

	if(cost_matrix_disp || connectivity_disp) {

		FILE *fout_info;
		char *outfilename_info = strcat(outfilename, ".info");
		if ((fout_info = fopen(outfilename_info, "w")) == NULL) {
			sprintf(msg, "Error opening output keyframe file %s\n", 
					outfilename);
			zvmessage(msg, "");
			zabend();
		}
	
		if(cost_matrix_disp) {
			fprintf(fout_info, "Keyframe scores matrix:\n");
			fprintf(fout_info, "image ");
			for (i=0; i<nids; i++) {
				//fprintf(fout_info, "%d:   ", i);
				if(i >= 100)
					fprintf(fout_info, "%d:   ", i);
				else if(i >= 10)
					fprintf(fout_info, "%d:    ", i);
				else
					fprintf(fout_info, "%d:     ", i);
			}
			fprintf(fout_info, "\n");
			for (i=0;i<nids;i++) {
				//fprintf(fout_info, "%d: ", i);
				if(i >= 100)
					fprintf(fout_info, "%d:  ", i);
				else if(i >= 10)
					fprintf(fout_info, "%d:   ", i);
				else
					fprintf(fout_info, "%d:    ", i);
				for (j=0;j<nids;j++) 	{
					if(keyframe_scores(i,j) < 0.0) 
						fprintf(fout_info, "%3.3f ", keyframe_scores(i,j)); 
					else
						fprintf(fout_info, "%3.3f  ", keyframe_scores(i,j)); 
				}
				fprintf(fout_info, "\n");
			}
		}
	
		if(connectivity_disp) {
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
		}

		fclose(fout_info);
	}

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


// Compute the "fundamental matrix" from a set of tiepoints
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
	
    // (TR)^-1 Htemp
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
		//if(used[i]) {
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
			//float error = err1*err1 + err2*err2;
			float error = sqrt(err1*err1 + err2*err2);
			if(sqrt(error) < 1.0f) {
				number_good++;
			}

			total_error += error;
			number_used++;
		//}
	}

	total_error = total_error / (float)number_used;

	var = 0.0f;
	for(int i = 0; i < n; i++) {
		//if(used[i] == 1) {
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
			//float error = err1*err1 + err2*err2;
			float error = sqrt(err1*err1 + err2*err2);

			var += (pow((total_error - error), 2)/(float)number_used);
		//}
	}

	return total_error;
}


// Compute a homography matrix from a set of tiepoints
int computeHomography(const std::vector<TiePoint> & ties, 
                      std::array<float, 9> & H, unsigned char* used,
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

    // Need at least four ties to compute homography

    if (nbTies < 4)
        return 1;

	// THIS IS IMPORTANT BUT NOT WORKING FOR NX, ANALYZE FURTHER
	//
	// The value for nX always remains at 0 for some reason, while 
	// nY can go into the hundreds..
	/*
	//Check that there are at least 4 points that are not colinear.
    int ycur = -1, nX = 0, nY = 0, newLine = 0;
    for (const auto & tie: ties) {
        if (ycur == tie.left_sample) {
            if (newLine) {
                nX++;
                newLine = 0;
            }
        }
        else {
            ycur = tie.left_sample;
            newLine = 1;
            nY++;
        }
		cout << "nX = " << nX << ", nY = " << nY << ", newline = " << newLine << endl;
    }  
    // If the minimum number of non-colinear points is not found, 
	// clear ties container.
    if (nX < 2 || nY < 2)
		return 1;
	*/

    // [1] Normalization of the input data
    // Recenter and rescale the (x,y) coordinates of one reference 
	// (e.g., Left image) such that points are centered around 0, and 
	// the average distance between points and 0 is sqrt(2). We have a 		
	// translation+scale transform that we store.
    // Do same thing with points of the other reference (e.g., Right 		
	// image) 
      
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
	// algorithm and solve for the homography transform H. Five ties 
	// gives 10 equations, with the 9 parameters of the Homography
    // transform

    MatrixXf mm(nbTies2, 9);
    for (int i=0; i < nbTies; i++ ) {
        mm(2*i,0) = 0.0;
        mm(2*i,1) = 0.0;
        mm(2*i,2) = 0.0;
        mm(2*i,3) = -(sL * (ties[i].left_line + txL));
        mm(2*i,4) = -(sL * (ties[i].left_sample + tyL));
        mm(2*i,5) = -1.0;
        mm(2*i,6) = sR*(ties[i].corrected_sample+tyR)  * 
					sL*(ties[i].left_line + txL);
        mm(2*i,7) = sR*(ties[i].corrected_sample+tyR)  * 
					sL*(ties[i].left_sample + tyL);
        mm(2*i,8) = sR*(ties[i].corrected_sample+tyR);

        mm(2*i+1,0) = sL*(ties[i].left_line + txL);
        mm(2*i+1,1) = sL*(ties[i].left_sample + tyL);
        mm(2*i+1,2) = 1.0;
        mm(2*i+1,3) = 0.0;
        mm(2*i+1,4) = 0.0;
        mm(2*i+1,5) = 0.0;
        mm(2*i+1,6) = -sR*(ties[i].corrected_line+txR) *
					   sL*(ties[i].left_line + txL);
        mm(2*i+1,7) = -sR*(ties[i].corrected_line+txR) *
					   sL*(ties[i].left_sample + tyL);
        mm(2*i+1,8) = -sR*(ties[i].corrected_line+txR);
    }

    // Solve for x'=Hx using SVD
    JacobiSVD<MatrixXf> svd(mm, ComputeThinU | ComputeThinV);

	// Compute the rank of mm:
	int rank = svd.rank();
	if(rank < 9) {
		// Matrix not full rank! Return since rank-deficient SVD can 
		// fail
		return 1;
	}
	
	// Compute the condition number of mm:
	cond_number = svd.singularValues()(0) / 
		   svd.singularValues()(svd.singularValues().size()-1);

    // The singular vector corresponding to the smallest singular value 
	// is the solution (H), and corresponds to the last column of the V
	// matrix. Note that H is here with respect to the centered/scaled 
	// ref system. Let's call it Hcentered
    int dim = (nbTies2 > 9) ? 9 : nbTies2;  // Because of ThinV rule
    MatrixXf m(9, dim);
    m = svd.matrixV();

    // [3] Use the transform/scales to transform back the Hcentered to 
	// the original coordinate system 
	//
    // 		H = (TR)^-1 Hcentered TL
	//
    // with TL the "translation then scale" tranform matrix for the L
	// coord sys and TR^-1, the inverse of the "translation then scale"
	// transform matrix for the R coord sys

    // Htemp = Hcentered TL
    H[0] = m(0,dim-1) * sL;
    H[1] = m(1,dim-1) * sL;
    H[2] = m(0,dim-1) * sL * txL + m(1,dim-1) * sL * tyL + m(2,dim-1);
    H[3] = m(3,dim-1) * sL;
    H[4] = m(4,dim-1) * sL;
    H[5] = m(3,dim-1) * sL * txL + m(4,dim-1) * sL * tyL + m(5,dim-1);
    H[6] = m(6,dim-1) * sL;
    H[7] = m(7,dim-1) * sL;
    H[8] = m(6,dim-1) * sL * txL + m(7,dim-1) * sL * tyL + m(8,dim-1);


    // (TR)^-1 Htemp
    float txR_inv = -txR*sR;
    float tyR_inv = -tyR*sR;
    float sR_inv = 1.0/sR;
    H[0] = sR_inv * H[0] + sR_inv * txR_inv * H[6];
    H[1] = sR_inv * H[1] + sR_inv * txR_inv * H[7];
    H[2] = sR_inv * H[2] + sR_inv * txR_inv * H[8];
    H[3] = sR_inv * H[3] + sR_inv * tyR_inv * H[6];
    H[4] = sR_inv * H[4] + sR_inv * tyR_inv * H[7];
    H[5] = sR_inv * H[5] + sR_inv * tyR_inv * H[8];

    return 0;
}


// Calculate the error for given homography matrix
float calculateHomographyError(const vector<TiePoint> & ties, 
	std::array<float, 9> & H, unsigned char* used, float& var) {

	float n = ties.size(); 
	float total_error = 0.0f;
	int number_used = 0;
	int number_good = 0;

	Matrix3d Hmatrix;
	Hmatrix << H[0], H[1], H[2],
			   H[3], H[4], H[5],
			   H[6], H[7], H[8];

	for(int i = 0; i<n; i++) {
		//if(used[i]) {
			number_used++;
			Vector3d left_pixel;
			left_pixel(0) = ties[i].left_line;   //x
			left_pixel(1) = ties[i].left_sample; //y
			left_pixel(2) = 1.0f;                //z
			left_pixel = Hmatrix*left_pixel;
			Vector3d right_pixel;
			right_pixel(0) = ties[i].right_line;    //x
			right_pixel(1) = ties[i].right_sample;  //y
			right_pixel(2) = 1.0f;                  //z

			float error = (left_pixel - right_pixel).norm(); 
			total_error += error;
			number_used++;
		//}
	}

	total_error = total_error / (float)number_used;

	var = 0.0f;
	for(int i = 0; i < n; i++) {
		//if(used[i] == 1) {
			Vector3d left_pixel;
			left_pixel(0) = ties[i].left_line;   //x
			left_pixel(1) = ties[i].left_sample; //y
			left_pixel(2) = 1.0f;                //z
			left_pixel = Hmatrix*left_pixel;
			Vector3d right_pixel;
			right_pixel(0) = ties[i].right_line;    //x
			right_pixel(1) = ties[i].right_sample;  //y
			right_pixel(2) = 1.0f;                  //z

			float error = (left_pixel - right_pixel).norm(); 
			var += (pow((total_error - error), 2)/(float)number_used);
		//}
	}

	return total_error;
}


// Calculate the Geometric Robust Information Criterion (GRIC), a 
// measure of the "goodness of fit" of a fundamental or homography 
// matrix
// Source: https://link.springer.com/article/10.1023/A:1007948927139
float calculateGRIC(float e, float d, float r, float var, float limit) {

	float p;

	limit = 10.0f; // using this constant for now
	float l3 = limit;

	p = std::min((float)(e/var), l3*(r-d));

	float res = p;

	return res;
}


// Calculate the GRIC value for a fundamental matrix
float calculateFundamentalGRIC(const std::vector<TiePoint> & ties, 
			std::array<float, 9> & F, unsigned char* used, float var) {

	float total_error = 0.0f;

	float r = 4;
	float n = ties.size(); 
	float d = 3;
	float k = 7;

	Matrix3d Fmatrix;
	Fmatrix << F[0], F[1], F[2],
			   F[3], F[4], F[5],
			   F[6], F[7], F[8];

	int number_used = 0;

	for(int i = 0; i<n; i++) {
		//if(used[i]) {
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
						  line(1)*left_pixel(1) + 
					  line(2))/sqrt(line(0)*line(0)+line(1)*line(1));
			// Changed to sqrt:	
			//float error = err1*err1 + err2*err2;
			float error = sqrt(err1*err1 + err2*err2);

			error = calculateGRIC(error*error, d, r, var, 2);

			total_error += error;
			number_used++;
		//}
	}

	n = number_used;
	float l1 = log(r);
	float l2 = log(r*n);

	total_error = total_error + l1*d*n + l2*k;

	return total_error;
}


// Calculate the GRIC value for a homography matrix
float calculateHomographyGRIC(const std::vector<TiePoint> & ties, 
			std::array<float, 9> & H, unsigned char* used, float var) {

	float total_error = 0.0f;

	float r = 4;
	float n = ties.size(); 
	float d = 2;
	float k = 8;

	int number_used = 0;

	Matrix3d Hmatrix;
	Hmatrix << H[0], H[1], H[2],
			   H[3], H[4], H[5],
			   H[6], H[7], H[8];
	
	for(int i = 0; i<n; i++) {
		//if(used[i]) {
			Vector3d left_pixel;
			left_pixel(0) = ties[i].left_line;   //x
			left_pixel(1) = ties[i].left_sample; //y
			left_pixel(2) = 1.0f;                //z
			left_pixel = Hmatrix*left_pixel;
			Vector3d right_pixel;
			right_pixel(0) = ties[i].right_line;    //x
			right_pixel(1) = ties[i].right_sample;  //y
			right_pixel(2) = 1.0f;                  //z

			float err = (left_pixel - right_pixel).norm(); 

			err = calculateGRIC(err*err, d, r, var, 2);

			total_error += err;
			number_used++;
		//}
	}

	n = number_used;

	float l1 = log(r);
	float l2 = log(r*n);
	total_error = total_error + l1*d*n + l2*k;
	
	return total_error;
}


// Affine transform using SVD; may eventually replace F or H for 
// certain configurations
int computeAffineTransform(const std::vector<tiePoint> & ties, 
			std::array<float, 6> & outA) {

    int nbTies = ties.size();

    // Need at least three ties to compute affine transform

    if (nbTies < 3)
       return 1;

    // Use of standard Least Square to define the affine transform
    // parameters
    //      AX = B ---> X = inverse(At A) (At B)

    MatrixXf A(nbTies*2, 6);
    VectorXf B(nbTies*2);

    // Fill the matrix A and vector B

    for (int i=0; i<nbTies; i++) {
       A(2*i, 0) = ties[i].xL;
       A(2*i, 1) = ties[i].yL;
       A(2*i, 2) = 1.0;
       A(2*i, 3) = 0.0;
       A(2*i, 4) = 0.0;
       A(2*i, 5) = 0.0;

       A(2*i+1, 0) = 0.0;
       A(2*i+1, 1) = 0.0;
       A(2*i+1, 2) = 0.0;
       A(2*i+1, 3) = ties[i].xL;
       A(2*i+1, 4) = ties[i].yL;
       A(2*i+1, 5) = 1.0;

       B(2*i)   = ties[i].xR;
       B(2*i+1) = ties[i].yR;
    }

    // Solve using SVD

    JacobiSVD<MatrixXf> svd(A, ComputeThinU | ComputeThinV);
    Matrix<float, 6, 1> X = svd.solve(B);

    // Save affine transform parameters

    outA[0] = X(0);
    outA[1] = X(1);
    outA[2] = X(2);
    outA[3] = X(3);
    outA[4] = X(4);
    outA[5] = X(5);

    return 0;
}


// Compute a connectivity matrix, which tallies the number of tiepoints
// across all image pairs. The resulting matrix is NxN, where N 
// corresponds to the number of images
MatrixXi computeConnectivityMatrix(TiePoint *tiepoints, const int n_overlaps, int nids) {

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


// Evaluate the keyframe selection cost function. See the PDF for 
// further details
int evalCostFunction(TiePoint *tiepoints, const int n_overlaps, 
	const int j, const int k, const float edge_discard, 
	const MatrixXd imagesizes, const MatrixXf camparams, 
	float &score, const int max_connectivity) {

	int count = 0;
    char msg[256];

	// Get current image dimensions
	float currentwidth = imagesizes(j,0);
	float currentheight = imagesizes(j,1);

	// Store the tiepoints corresponding to each and all pairs of images
	vector<TiePoint> currentties;

	// Get the current 'C' and 'A' parameters:
	float cx_j = camparams(j,0);
	float cy_j = camparams(j,1);
	float cz_j = camparams(j,2);
	float ax_j = camparams(j,3);
	float ay_j = camparams(j,4);
	float az_j = camparams(j,5);
	float cx_k = camparams(k,0);
	float cy_k = camparams(k,1);
	float cz_k = camparams(k,2);
	float ax_k = camparams(k,3);
	float ay_k = camparams(k,4);
	float az_k = camparams(k,5);
	float distC = sqrt((cx_j-cx_k)*(cx_j-cx_k) + 
					   (cy_j-cy_k)*(cy_j-cy_k) + 
					   (cz_j-cz_k)*(cz_j-cz_k));
	sprintf(msg, "     Baseline: %f", distC);
    zvmessage(msg, "");
	float angle = acos(ax_j*ax_k + ay_j*ay_k + az_j*az_k);
	sprintf(msg, "     Angle: %f", angle);
    zvmessage(msg, "");

	// Homography transform coefficients between images (i,j):
	std::array<float, 9> H;

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

	// Need at least 4 matches for a homography, and 8 for a 
	// fundamental matrix

	sprintf(msg, "     Number of valid tiepoints after discarding near "
				 "image edges: %d", currentties.size());
    zvmessage(msg, "");
	if (currentties.size() < 8) {
		zvmessage("     Fewer than 8 tiepoints, can't compute F and H! "
				  "Set score to -1","");
		score = -1.0;
		return 0;
	}

	// Variances computed through error estimation
	float varF, varH;

	// Condition numbers for the data matrices:
	float condnumberF, condnumberH;

	// Store locations of tiepoints that should not be used
	// Note: this capability is currently unused

	unsigned char* usedF = new unsigned char[currentties.size()];
	unsigned char* usedH = new unsigned char[currentties.size()];

	// Apply homography estimation

	int status_H = computeHomography(currentties, H, usedH, condnumberH);
	sprintf(msg, "     Condition number - H: %f", condnumberH);
    zvmessage(msg, "");

	// If this step failed, we cannot compute the keyframe cost function
	if(status_H == 1) {
		zvmessage("Homography estimation failed! Set score to -2","");
		score = -2.0;
		return 0;
	}

	//Compute H error, from which we'll obtain the variance for H

	float error_H = calculateHomographyError(currentties, H, usedH, varH);
	sprintf(msg, "     Error - H: %f", error_H);
    zvmessage(msg, "");
	//sprintf(msg, "     Variance - H: %f", varH);
    //zvmessage(msg, "");

	//Compute GRIC for H

	float gric_H = calculateHomographyGRIC(currentties, H, usedH, varH);
	sprintf(msg, "     GRIC(H): %f", gric_H);
    zvmessage(msg, "");

	//Compute the fundamental matrix

	int status_F = computeFundamental(currentties, F, usedF, condnumberF);
	sprintf(msg, "     Condition number - F: %f", condnumberF);
    zvmessage(msg, "");

	// If this step failed, we cannot compute the keyframe cost function
	if(status_F == 1) {
		zvmessage("     Fundamental matrix estimation failed! Set score to -3","");
		score = -3.0;
		return 0;
	}

	//Compute F error, from which we'll obtain the variance for F

	float error_F = calculateFundamentalError(currentties, F, usedF, varF);
	sprintf(msg, "     Error - F: %f", error_F);
    zvmessage(msg, "");
	//sprintf(msg, "     Variance - F: %f", varF);
    //zvmessage(msg, "");

	//Compute GRIC for F:
	float gric_F = calculateFundamentalGRIC(currentties, F, usedF, varF);
	sprintf(msg, "     GRIC(F): %f", gric_F);
    zvmessage(msg, "");
	
	//Compute the area of matches relative to total image area

	float min_x = currentheight + 1.0;
	float min_y = currentwidth + 1.0;
	float max_x = -1.0;
	float max_y = -1.0;
	int number_used = 0;
	for(int p = 0; p < currentties.size(); p++) {
		//if(usedF[i] == 1) {
			number_used++;
			if(currentties[p].left_line < min_x) {
				min_x = currentties[p].left_line;
			}
			if(currentties[p].left_sample < min_y) {
				min_y = currentties[p].left_sample;
			}
			if(currentties[p].left_line > max_x) {
				max_x = currentties[p].left_line;
			}
			if(currentties[p].left_sample > max_y) {
				max_y = currentties[p].left_sample;
			}
		//}
	}

	float total_size = currentwidth*currentheight;
	float matches_area = (max_x - min_x)*(max_y - min_y);
	float matches_area_ratio = matches_area / total_size; 
	sprintf(msg, "     Matches-to-area ratio: %f", matches_area_ratio);
    zvmessage(msg, "");

	// Compute the ratio between matches and the highest value in the
	// tiepoints connectivity matrix
	
	float matches_amount_ratio = 
			(float)currentties.size()/(float)max_connectivity;  
	sprintf(msg, "     Matches to max connectivity ratio: %f",
			matches_amount_ratio);
    zvmessage(msg, "");

	// Compute the relative GRIC between F and H. If negative, we should
	// not use this pair

	float rel_GRIC;
	//rel_GRIC = (gric_H - gric_F) / gric_H;
	rel_GRIC = gric_F / gric_H;

	sprintf(msg, "     Relative GRIC: %f", rel_GRIC);
    zvmessage(msg, "");

	if(rel_GRIC < 0.0f) {
		zvmessage("     Negative rel_GRIC, set score to -4","");
		score = -4.0;
		return 0;
	} 

	// Additional checks

	// 1) No movement between consecutive frames: 
	
	if(distC == 0.0 || angle == 0.0) {
		zvmessage("     No relative motion between pair, set score to -5","");
		score = -5.0;
		return 0;
	}

	// 6) Pure rotations:
	// Detect pure rotations by a) very small A angle plus b) relatively
	// high number of matches relative to other pairs, and c) bad condition
	// number for computing F relative to computing H
	//
	// NOTE: hard-coding these values for now, but they should actually
	// be parameters in the PDF

	double condnumberratio = condnumberH/condnumberF;
	sprintf(msg, "     Condition number ratio (H/F): %f", condnumberratio);
    zvmessage(msg, "");

	if( (angle < 0.01) && 
		(condnumberratio < 0.1) &&
		(matches_amount_ratio > 0.8) )  {
		zvmessage("     Pure rotation detected, set score to -6","");
		score = -6.0;
		return 0;
	}
	

	// Finally, compute the cost function value if nothing else failed:

	score = condnumberratio*
			matches_area_ratio*
			matches_amount_ratio*rel_GRIC; 

	sprintf(msg, "     Final score: %f", score);
    zvmessage(msg, "");

	return 0;
		
}


// Pick keyframes given a score matrix
int pickKeyframes(const MatrixXf &keyframe_scores, 
	std::vector<int> &keyframe_locations, const int nids, 
	int &keyframe_counter, const int selection_mode, const double sel_percent) {

	// There are a number of ways to pick keyframes given pairwise scores:
	//
	// 1) Sequential selection: find the first keyframe based on highest
	// average score for the first frames, and then fill a list of the highest
	// scores per line and their respective pairs. This simple method works 
	// surprisingly well.  
	//
	// 2) Global selection: the keyframe scores matrix is viewed as a whole, 
	// regardless of frame ordering, and the highest X% of average keyframe
	// scores is chosen.
	//
	// FUTURE IMPROVEMENT: Look for “optimal paths” on a graph. If the 
	// keyframe scores matrix is treated as a graph, composed of nodes
	// (image pairs) and links (weights) representing the keyframe score, 
	// algorithms such as "Djikstra’s shortest path” algorithm, the "longest
	// path problem", and others can be implemented to traverse the graph.
		

    char msg[256];

	int limited_motion[nids], valid_scores[nids], failure_codes[nids];

	for(int i=0; i<nids; i++) {
		limited_motion[i] = 0;
		valid_scores[i] = 0;
		failure_codes[i] = 0;
	}

	// Take statistics on the keyframe scores:
	for(int i = 0; i<nids; i++) {
		for(int j = 0; j<nids; j++) {

			// Tally pairs with little or no relative movement:
			if( (keyframe_scores(i, j) == -5.0) ||
				(keyframe_scores(i, j) == -6.0) ) {
				limited_motion[i]++;
				limited_motion[j]++;

			}

			// Find locations with valid scores (positive):
			if(keyframe_scores(i, j) > 0.0) {
				valid_scores[i]++;
				valid_scores[j]++;

			}

			// Find locations with any failure code (negative):
			if(keyframe_scores(i, j) < 0.0) {
				failure_codes[i]++;
				failure_codes[j]++;
			}

		}
	}

	sprintf(msg, "\nValid scores, failures, and limited motion per image");
	zvmessage(msg, "");
	for(int i = 0; i<nids; i++) {
		sprintf(msg, "     %d: %d, %d, %d", i, valid_scores[i], 					failure_codes[i], limited_motion[i]);
		zvmessage(msg, "");
	}


	// Choose between sequential and global processing:

	if(selection_mode)
	{ 

		// -----Sequential processing-----
		
		// Look at the list of the frames after the first keyframe
		// which appear in BOTH the left list of keys and the right list of
		// max indeces. These should all be good to use as keyframes.
		
		sprintf(msg, "\nApplying sequential processing\n");
		zvmessage(msg, "");

		// The last row doesn't count because we cannot jump from there to 
		// anywhere else in sequential processing
		MatrixXf::Index maxIndex[nids-1]; 
		VectorXf maxVal(nids-1);

		int firstvalidframe = 0;

		// Find maxIndex per frame:
		for(int i = 0; i<nids-1; i++) {
			maxVal(i) = keyframe_scores.row(i).maxCoeff(&maxIndex[i]);
		}

		// Find the first valid frame:
		for(int i = 0; i<nids-1; i++) {
			maxVal(i) = keyframe_scores.row(i).maxCoeff(&maxIndex[i]);
			if(maxVal(i) > 0.0) {
				firstvalidframe = i;
				break;
			}
		}
		sprintf(msg, "\nFirst valid frame (score > 0): %d", firstvalidframe);
		zvmessage(msg, "");

		// Now, given the first valid frame, find the actual first keyframe.
		//
		// NOTE 1: we can assume the first valid frame is always a keyframe,  
		// but this is a weak assumption. A better way to do it: see which of 
		// the first few frames is "best connected", as in which has the 
		// highest sum of scores. For now, use the first 5 scores for 
		// the first 5 frames (this should eventually become a pdf parameter).
		// If fewer than 10 frames, take the first as a keyframe.
		//
		// NOTE 2: An even better way to do this is by using "Beder and 
		// Steffen - Determining an Initial Image Pair for Fixing the Scale 
		// of a 3D Reconstruction from an Image Sequence (2006)"

		int j = firstvalidframe;
		float maxscore = 0.0;

		if(nids > firstvalidframe+10) {
			for(int i = firstvalidframe; i < firstvalidframe+5; i++) {

				float currentscore = 0.0;

				for(int k=0; k<5; k++) 
					currentscore += keyframe_scores(i,i+k);		

				if(currentscore > maxscore) {
					maxscore = currentscore;
					j = i;
				}
			}
		}
		else {
			j = firstvalidframe;
		}

		keyframe_locations[j] = 1;
		sprintf(msg, "First keyframe: %d", j);
		zvmessage(msg, "");
		
		int counter = 0;
		int* candidates = new int[2*(nids-j)];
		memset(candidates, 0, 2*(nids-j)*sizeof(*candidates)); 
		int* candidates_final = new int[2*(nids-j)];
		memset(candidates_final, 0, 2*(nids-j)*sizeof(*candidates_final)); 
		
		for(int i = j; i<nids-1; i++) {
			maxVal(i) = keyframe_scores.row(i).maxCoeff(&maxIndex[i]);
			if(maxVal(i) > 0.0) {
				candidates[2*i] = i;
				candidates[2*i+1] = maxIndex[i];
			}
		}
		
		// Sort the array in increasing order:
		int len = 2*(nids-j); 
		sort(candidates, candidates + len);

		// Keep duplicates:
		int numunique = 0;
		for (int i=0; i < len-1; i++)
		    if (candidates[i] == candidates[i+1] && 
				candidates[i] > j && 
				candidates[i] < nids-1) {
		        candidates_final[numunique] = candidates[i];
				numunique++;
			}

		// Set keyframe locations:
		for (int i=0; i<numunique; i++) {
			keyframe_locations[candidates_final[i]] = 1;
		}

		// Just to be safe, eliminate frames in limited motion pairs:
		for(int i=0; i<nids; i++) {	
			if(limited_motion[i] > 0)
				keyframe_locations[i] = 0;
		}
		
		for(int i=0; i<nids; i++) {	
			if(keyframe_locations[i] == 1)
				counter++;
		}
		keyframe_counter = counter;

	}
	else {

		// -----Global processing-----
		sprintf(msg, "\nApplying global processing\n");
		zvmessage(msg, "");

		vector<double> ave_scores(nids);

		// Obtain the average keyframe score per frame
		for(int i = 0; i<nids; i++) {
			ave_scores[i] = 0.0;
			int score_count = 0;
			for(int j = 0; j<nids; j++) {
				if(keyframe_scores(i, j) > 0.0) {
					ave_scores[i] += keyframe_scores(i, j);
					score_count++;
				}
			}
			if(score_count > 0)
				ave_scores[i] /= score_count;
		}

		int num_final_frames = round(sel_percent*nids/100.0);

		// Sort by value, and get the indices

		int sorted_counter = 0;
		for (auto i: sort_indeces(ave_scores)) {
			if( (sorted_counter < num_final_frames) &&
				(ave_scores[i] > 0.0) )
				keyframe_locations[i] = 1;
			sorted_counter++;
		}
	
		// Just to be safe, eliminate frames in limited motion pairs:
		for(int i=0; i<nids; i++) {	
			if(limited_motion[i] > 0)
				keyframe_locations[i] = 0;
		}

		int counter = 0;
		for(int i=0; i<nids; i++) {	
			if(keyframe_locations[i] == 1)
				counter++;
		}
		keyframe_counter = counter;

	}


	// Make sure keyframes were selected! Otherwise something failed,
	// and return 1:

	if(keyframe_counter < 2)
		return 1;

	return 0;

}

