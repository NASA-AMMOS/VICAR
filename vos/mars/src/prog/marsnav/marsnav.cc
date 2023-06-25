/////////////////////////////////////////////////////////////////////////
// marsnav - multimission pointing correction program
////////////////////////////////////////////////////////////////////////

#include "vicmain_c"

#include "xvector.h"

#include "mars_support.h"
#include "mars_tiepoints.h"

#include "pamoeba.h"

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

#include <string.h>

#ifdef _OPENMP
#include <omp.h>
#endif

/* buffer sizes in main program */
#define MAX_INPUTS 2000
#define MAX_THREADS 128

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

// Parameter structure for "objective" cost function (the function that
// evaluates pointing solutions).  Note that several of these are just
// pointers to the main program's arrays.

struct ObjectiveParams {
    int nids;
    PigPointingModel *(*pointing_in_all)[MAX_THREADS][MAX_INPUTS];
    PigCameraModel *(*camera_in_all)[MAX_THREADS][MAX_INPUTS];
    PigSurfaceModel **surface_model_all;
    PigCoordSystem *cs;

    int *n_overlaps;
    struct TiePoint *tiepoints;

    // _save is initial pointing, used for ref images only
    double (*pparams_in_save)[MAX_INPUTS][PIG_MAX_PARAMS];

    double (*inertia)[PIG_MAX_PARAMS];	// wt to keep images in place

    int refimg[MAX_INPUTS];
    int refadj;

    int adjust_pointing;	// Whether or not to adjust pointing
    int adjust_surface;		// Whether or not to adjust surface model
    int adjust_sites;		// Whether or not to ANY sites

    double max_error;		// biggest error on any tiepoint (output)
    int who;			// which tiepoint was biggest (output)

    struct SiteList (*site_list)[PIG_MAX_CS];
    int num_sites;

    // mode for processing traditional tiepoints in objective function
#define TRADITIONAL 0
#define MISS_DISTANCE 1
#define BOTH_TRADITIONAL_AND_MISS 2
    int traditional_mode_i;
    double miss_mult;
    int range_miss;		// Whether or not to scale miss dist by range
    double parallel_limit;	// How close vectors (dot product) can be to
				// call them parallel for miss distance
    int omp_on;			// true iff omp is turned on
};

extern "C" double Objective(double p[], int ndim, void *func_args);
void save_pointing(int xml_mode, char *name, int n,
		       PigPointingModel *pointings[],
                       double pparams_org[][PIG_MAX_PARAMS], 
                       PigFileModel *file_models[], 
		       PigSurfaceModel *surface_model, 
                       char *solution_id);
void save_pointing_file(char *name, int n, PigPointingModel *pointings[],
		       double pparams_orig[][PIG_MAX_PARAMS]);
void save_pointing_xml(char *name, int n, PigPointingModel *pointings[],
                       double pparams_org[][PIG_MAX_PARAMS], 
                       PigFileModel *file_models[],
                       PigSurfaceModel *surface_model, 
                       char *solution_id);
void pack_pointing_params(double Pzero[], double lambda[], int &ndim,
		int nids, PigPointingModel *pointing_in[],
		int refimg[], int refadj, PigSurfaceModel *surface_model,
		int adjust_pointing, int adjust_surface, int adjust_sites,
		SiteList *site_list, int num_sites);
double unpack_pointing_params(double Pzero[], int ndim,
		int nids, PigPointingModel *pointing_in[],
		int refimg[], int refadj, PigSurfaceModel *surface_model,
		int adjust_pointing, int adjust_surface, int adjust_sites,
		SiteList *site_list, int num_sites,
		double (*pparams_save)[MAX_INPUTS][PIG_MAX_PARAMS],
		double (*inertia)[PIG_MAX_PARAMS]);
void print_sites(SiteList sites[], int num_sites);

////////////////////////////////////////////////////////////////////////
// MarsNAV program
////////////////////////////////////////////////////////////////////////

void main44()
{

    int i, status;
    int count, def;
#define MSG_SIZE 512
    char msg[MSG_SIZE];

    // Inputs

    int nids;
    PigFileModel *file_models[MAX_INPUTS];
    //!!!! camera_in_all and pointing_in_all need to be dynamically allocated
    //!!!! to avoid limit stacksize problems on 64-bit.  Reduced the max #
    //!!!! of threads from 256 to 128 as a temporary workaround.
    PigCameraModel *camera_in_all[MAX_THREADS][MAX_INPUTS];
    PigPointingModel *pointing_in_all[MAX_THREADS][MAX_INPUTS];
    PigSurfaceModel *surface_model_all[MAX_THREADS];
    int homogeneous_inputs = TRUE;
    PigCoordSystem *cs, *output_cs;

    // Tiepoints

    //struct TiePoint tiepoints[MARS_MAX_TIEPOINTS];
    //int n_overlaps;
    TiePoint *tiepoints = new (std::nothrow) TiePoint[MARS_MAX_TIEPOINTS];
    if(tiepoints == NULL){
       zvmessage("Memory allocation error during TiePoints[MARS_MAX_TIEPOINTS] initialization","");
       zabend();
    }
    int n_overlaps = MARS_MAX_TIEPOINTS;

    int refimg[MAX_INPUTS];
    int refadj;

    // Input params after input nav file applied; used for resetting solutions
    double pparams_in[MAX_INPUTS][PIG_MAX_PARAMS];
    // Original input params before nav file; used for saving nav file
    double pparams_in_save[MAX_INPUTS][PIG_MAX_PARAMS];

    double residual;

    struct SiteList site_list[PIG_MAX_CS];
    int num_sites;

    // User Parameters

    char outfilename[150];
    char tptlist[150], out_tptlist[150];
    char mission[64], instrument[64];
    char solution_id[64];
    int interactive;
    double permitted_error;
    int recycle;
    int xml_mode;
    int adjust_pointing;
    int adjust_surface;
    int adjust_sites;
    int remove_flag;			// whether or not to remove tiepoints
    double max_residual;
    int max_remove;
    int remove_loops = 0;
    double ftol;
    double inertia[PIG_MAX_PARAMS];
    int start_key;

    zvmessage("MARSNAV version 2020-05-01", "");

    mars_setup(nids, file_models, camera_in_all[0], pointing_in_all[0], 
               surface_model_all[0], NULL, output_cs, mission, instrument,
	       homogeneous_inputs, MAX_NL, MAX_NS, MAX_INPUTS);

    cs = surface_model_all[0]->getCoordSystem();
 
    // Get the initial pointing

    for (i = 0; i < nids; i++) {
	pointing_in_all[0][i]->getPointingParameters(pparams_in_save[i],
							PIG_MAX_PARAMS);
    }

    int omp_on = zvptst("OMP_ON");
    int par_degree;
    zvp("PAR_DEGREE", &par_degree, &count);

    // If we're parallel, we have to create a set of pointing, camera, and
    // surface models for each thread.  That's so each thread can adjust them
    // independently (via unpack) in the objective routine.  Otherwise, the
    // threads would trounce all over each other in a horrid mess.

    // To do this, pointing_in and camera_in are actually 2-D arrays of pointers
    // to objects.  The first dimension is based on the thread number, or 0
    // if we're in the master (or not parallel now).

    // One might think we could limit num_thr based on par_degree in case the
    // par_degree is less than the number of threads.  That would be true if
    // you looked only at the main pamoeba loop.  However, there are subsidiary
    // parallel loops in the contract-all case as well as the initialization
    // where the number of threads is not limited by par_degree.  Thus we must
    // have enough models available to handle those cases as well.  It is
    // unknown whether the time saved in those loops is worth the extra
    // initialization overhead... but it's also rare that par_degree would be
    // much less than the number of threads so it probably doesn't matter.
    // Probably not worth complicating the pamoeba API any more to address this.

    int num_thr = 1;
#ifdef _OPENMP
    if (omp_on)
        num_thr = omp_get_max_threads();
#endif

    // Re-create the pointing/camera model pairs for each additional thread.
    // Would be nice if we could just copy them, but alas.  Unfortunately we
    // have to deal with the input nav file in order to get the right model
    // set up.  Most of this is cribbed from mars_setup().
    //!!!! This really should be subroutine-ized and called from both
    //!!!! here and mars_setup...

    if (num_thr > 1) {

	if (num_thr > MAX_THREADS) {
	    snprintf(msg, MSG_SIZE, "Number of threads %d exceeds the maximum %d.  Reduce or increase the limit.", num_thr, MAX_THREADS);
	    zvmessage(msg, "");
	    zabend();
	}

	PigPointingCorrections *pointing_corrections =
				mars_get_pointing_corrections(
					PigMission::getMissionObject(mission));
	char solution_id[256];
	int count;
	zvp("SOLUTION_ID", solution_id, &count);
	char *sid = NULL;
	if (count != 0)
	    sid = solution_id;
	else if (pointing_corrections != NULL) {
	    // No soln on cmd line, default to highest prio in nav file
	    sid = pointing_corrections->getHighestPriority();
	}

	for (int j=1; j < num_thr; j++) {

	    // Construct surface model

	    if (pointing_corrections != NULL) {
		PigSurfaceModelParams *smp =
				pointing_corrections->getSurfaceModel(sid);
		//!!!! HACK - see comments in mars_setup()
		int cnt = 0;
		char surface_coord[20];
		PigModelBase::getStaticParam("SURF_COORD",
						 surface_coord, &cnt, 1, 0);
		if (smp == NULL || cnt == 1) {
		    surface_model_all[j] = PigSurfaceModel::create(file_models[0]);
		} else {
		    PigMission *m = pointing_corrections->getMission();
		    PigCSReference *csr = smp->getCoordSystemParams();
		    PigCSReference csRef(m, csr->getFrameName(), sid,
				csr->getIndices(), csr->getNumIndices(),
				csr->getInstId());

		    surface_model_all[j] = PigSurfaceModel::create(
			m->getMissionName(), instrument,
			smp->getPointingParams()->getType(),
			smp->getPointingParams()->getParameters(),
			smp->getPointingParams()->getPointingParamCount(),
			m->getCoordSystem(&csRef));
		}
	    } else {		// no nav
		surface_model_all[j] = PigSurfaceModel::create(file_models[0]);
	    }

	    // Now create camera and pointing models

	    for (int i=0; i < nids; i++) {
		camera_in_all[j][i] = PigCameraModel::create(file_models[i], NULL);
		if (camera_in_all[j][i] == NULL) {
		    snprintf(msg, MSG_SIZE, "Unable to create camera model for input %d thread %d", i+1, j);
		    zvmessage(msg, "");
		    zabend();
		}

		int status;
	        pointing_in_all[j][i] = mars_create_pointing_model(
			camera_in_all[j][i], file_models[i], sid,
			pointing_corrections, status);
		if (pointing_in_all[j][i] == NULL) {
		    snprintf(msg, MSG_SIZE, "Unable to create pointing model for input %d thread %d", i+1, j);
		    zvmessage(msg, "");
		    zabend();
		}
		if (status == -1) {	// nav exists but no match
		    // message issued in setup
		    pointing_in_all[j][i]->pointCamera(file_models[i]);
		}
		else if (status == 0) {	// no nav file
		    pointing_in_all[j][i]->pointCamera(file_models[i]);
		}
		// else print pointing correction applied; done in setup
	    }
	    // Check for old-style, text-based nav file and apply values
	    if (pointing_corrections == NULL) {
		mars_apply_navtable(nids, pointing_in_all[j], NULL);
	    }
	}
    }

    // Now back to our regularly scheduled program...

    // Check for an input tiepoint table

    zvp("in_tpt", tptlist, &count);
    if (count != 1) {
	zvmessage("Input tiepoint file required!", "");
	zabend();
    }
    zvp("out_tpt", out_tptlist, &count);

    PigCoordSystem *load_cs = cs;    // pre-convert if no CS adj is being done
    zvpcnt("DO_LOCATION", &count);
    if (count != 0)
	load_cs = NULL;		// If adjusting, don't pre-convert!
    zvpcnt("DO_ORIENTATION", &count);
    if (count != 0)
	load_cs = NULL;		// If adjusting, don't pre-convert!

    mars_load_tiepoints(tptlist, tiepoints, n_overlaps, file_models, nids,
		load_cs, PigMission::getMissionObject(mission));
    snprintf(msg, MSG_SIZE, "%d tiepoints read from %s", n_overlaps, tptlist);
    zvmessage(msg, "");
 
    if (n_overlaps == 0) {
	zvmessage("No tiepoints found, exiting...", "");
	zabend();
    }

    // get parameter overrides if any

    zvparmd("ERROR", &permitted_error, &count, &def, 1, 0);
    zvp("RECYCLE", &recycle, &count);

    zvp("ADJREF", &refadj, &count);
    if (count == 0)
	refadj = PIG_MAX_PARAMS;

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
        snprintf(msg, MSG_SIZE, "%d tiepoints ignored due to IGNORE parameter", n_ignored);
        zvmessage(msg, "");
    }

    // Ignore intra-set matches.  In other words, if a tiepoint has two
    // (or more) images from the non-reference (active) set, ignore the
    // tiepoint.  This allows you to run a bunch of images at once, ignoring
    // connections between them, and looking only at how they connect to
    // their reference-image neighbors.  Use case: adding mastcams into an
    // already-nav'd navcam background.

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
	snprintf(msg, MSG_SIZE, "%d tiepoints ignored due to IGNORE_INTRA parameter",
		n_ignored);
	zvmessage(msg, "");
    }

    for (i=0; i < PIG_MAX_PARAMS; i++)
        inertia[i] = 0.0;
    status = zvparmd("INERTIA", inertia, &count, &def,PIG_MAX_PARAMS,0);

    zvp("START_KEY", &start_key, &count);

    interactive = zvptst("INTERACT");
    if (interactive)
	zvmessage("Interactive mode", "");
    else
	zvmessage("Batch mode", "");

    // Check on what to adjust...

    adjust_pointing = zvptst("DO_POINTING");
    adjust_surface = zvptst("DO_SURFACE");

    remove_flag = zvptst("REMOVE");

    zvparmd("FTOL", &ftol, &count, &def, 1, 0);

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

    // Read in the XYZ values for all the Dynamic XYZ tiepoints, and convert
    // them to their own Rover frames.  This is a little inefficient in that
    // a file with multiple tiepoints is opened and closed for each tiepoint,
    // but at least we read in only the tiepoint, not the entire file.

    PigMission *m = PigMission::getMissionObject(mission);
    for (i=0; i < n_overlaps; i++) {
	if (!tiepoints[i].active)
	    continue;
	if (tiepoints[i].type == TIEPOINT_DYNAMIC_XYZ) {
	    tiepoints[i].active = FALSE;	// until we get it right

	    // Sanity check and open the image

	    int file = tiepoints[i].left_image;
	    if (file < 0 || file >= nids) {
		snprintf(msg, MSG_SIZE, "Invalid left file number (%d) for tiepoint %d",
				file, i);
		zvmessage(msg, "");
		continue;		// bail
	    }
	    int xyz_unit = file_models[file]->getUnit();
	    status = zvopen(xyz_unit, "op","read", "open_act","sa",
		"io_act","sa", "u_format","doub", NULL);
	    file_models[file]->setFileOpen(TRUE);
	    if (file_models[file]->getNB() != 3) {
		snprintf(msg, MSG_SIZE, "Dynamic XYZ input file must have 3 bands (file %d, tiepoint %d", file, i);
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
		snprintf(msg, MSG_SIZE, "XYZ value for dynamic XYZ tiepoint %d is missing.  Tiepoint ignored.", i);
		zvmessage(msg, "");
		file_models[file]->closeFile();
		continue;			// bail
	    }

	    // Get CS for the XYZ file itself

	    PigCSReference *ref;
	    file_models[file]->getDerivedImageCS(ref);
	    PigCoordSystem *xyz_cs = m->getCoordSystem(ref);

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

    // Print out input status from labels

    mars_print_inputs(nids, pointing_in_all[0], camera_in_all[0], file_models,
			homogeneous_inputs, mission, instrument);

    // save the original pointing (after input nav file)

    for (i=0; i < nids; i++) {
	pointing_in_all[0][i]->getPointingParameters(pparams_in[i],
						     PIG_MAX_PARAMS);
    }

    // Having only one input is legal now, for e.g. fiducials

#if 0
    if (nids == 1) {
	zvmessage("Only one input file; creating nav file & quitting", "");
        
        save_pointing(xml_mode, outfilename, nids, pointing_in_all[0],
		      pparams_in_save, file_models, surface_model_all[0],
		      solution_id);
	return;
    }
#endif

    // Set the initial pointing

    for (i=0; i < nids; i++) {
       pointing_in_all[0][i]->setPointingParameters(pparams_in[i],
						    PIG_MAX_PARAMS);
    }

    // Gather the Site list

    num_sites = 0;
    adjust_sites = FALSE;
    for (i=0; i < nids; i++) {
	PigCoordSystem *cs = m->getCoordSystem(file_models[i], NULL);

	// See if we have this one already

	int j;
	for (j=0; j < num_sites; j++) {
	    if (site_list[j].site == cs) {
		site_list[j].num_images++;
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
	    num_sites++;
	}
    }

    // Figure out which sites to adjust

    int list[PIG_MAX_CS];
    int nlist;
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
	snprintf(msg, MSG_SIZE, "  Site %d,'%s',count=%d. Adj loc=%d,ori=%d. match loc=%d,ori=%d",
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
	    snprintf(msg, MSG_SIZE, "Reference image: %d", i+1);
	    zvmessage(msg, "");
	    refimg_set = TRUE;
	}
    }
    if (!refimg_set) {
	snprintf(msg, MSG_SIZE, "No reference image");
        zvmessage(msg, "");
    }

    //////////////////////////
    // Now compute the actual solutions
    //////////////////////////

    double Pzero[MAX_INPUTS*PIG_MAX_PARAMS];
    double lambda[MAX_INPUTS*PIG_MAX_PARAMS];
    int ndim, iter;

    // Fill in the parameters for the cost function

    ObjectiveParams params;

    params.nids = nids;
    params.pointing_in_all = &pointing_in_all;
    params.camera_in_all = &camera_in_all;
    params.surface_model_all = surface_model_all;
    params.n_overlaps = &n_overlaps;
    params.tiepoints = tiepoints;
    params.pparams_in_save = &pparams_in_save;
    memcpy(params.refimg, refimg, sizeof(refimg));
    params.refadj = refadj;
    params.inertia = NULL;		// only set if something is non-0
    for (i=0; i < PIG_MAX_PARAMS; i++) {
	if (inertia[i] != 0.0)
	    params.inertia = &inertia;
    }
    params.cs = cs;
    params.adjust_pointing = adjust_pointing;
    params.adjust_surface = adjust_surface;
    params.adjust_sites = adjust_sites;
    params.site_list = &site_list;
    params.num_sites = num_sites;

    if (omp_on && params.adjust_sites) {
	// We don't do sites in parallel, because it's too hard currently to
	// set up thread-specific site structures.... which is because they
	// are cached and shared amongst images with the same RMC.  We'd have
	// to do a major overhaul to be able to have an alternate set of Sites.
	// While this should be done at some point, it's not trivial... so we
	// just disable parallel adjustment of sites for now.
	zvmessage("Adjusting sites (location/orientation) cannot be done with OMP on", "");
	zvmessage("The feature is not yet implemented.", "");
	zabend();
    }

    int redo_solution = TRUE;
    int start_over;
    int recycle_count = 0;	// # of recycles done in batch mode

    // Get processing mode for traditional tiepoints
    if (zvptst("MISS_DISTANCE"))
      params.traditional_mode_i = MISS_DISTANCE;
    else if (zvptst("BOTH"))
      params.traditional_mode_i = BOTH_TRADITIONAL_AND_MISS;
    else
      params.traditional_mode_i = TRADITIONAL;
    zvparmd("MISS_MULT", &params.miss_mult, &count, &def, 1, 0);
    zvparmd("PARALLEL_LIMIT", &params.parallel_limit, &count, &def, 1, 0);

    params.range_miss = zvptst("RANGE_MISS");

    params.omp_on = omp_on;

    while (redo_solution) {
	redo_solution = FALSE;
	start_over = TRUE;	// start over or continue from where we left off

	// Get initial guess

	pack_pointing_params(Pzero, lambda, ndim, nids, pointing_in_all[0],
			refimg, refadj, surface_model_all[0],
			adjust_pointing, adjust_surface,
			adjust_sites, site_list, num_sites);

	snprintf(msg, MSG_SIZE, "Commanded mean pixel error %f",
					sqrt(Objective(Pzero, ndim, &params)));
	zvmessage(msg, "");
	snprintf(msg, MSG_SIZE,
		"Tiepoint %d has a commanded residual of %f pixels (%d->%d)",
			params.who, sqrt(params.max_error),
			tiepoints[params.who].left_image+1,
			tiepoints[params.who].right_image+1);
	zvmessage(msg, "");

	// solve for all pointing params at once using simplex method

	residual = pamoeba4(Pzero, lambda, ndim, ftol, 9000000, &iter,
			Objective, &params, 100000, par_degree, omp_on);

	// retrieve solutions

	unpack_pointing_params(Pzero, ndim, nids, pointing_in_all[0],
			refimg, refadj, surface_model_all[0],
			adjust_pointing, adjust_surface,
			adjust_sites, site_list, num_sites, NULL, NULL);

	// Check solution for residual.  We calculate it again just to get
	// max_error and who right.

	residual = Objective(Pzero, ndim, &params);

	snprintf(msg, MSG_SIZE, "Solution mean pixel error %f after %d iterations",
			sqrt(residual), iter);
	zvmessage(msg, "");
	snprintf(msg, MSG_SIZE, "Tiepoint %d has a residual of %f pixels",
			params.who, sqrt(params.max_error));
	zvmessage(msg, "");

	if (adjust_surface)		// Print out current surface
	    surface_model_all[0]->print();

	if (adjust_sites)
	    print_sites(site_list, num_sites);

	// Save this pointing, in case user quits early

        save_pointing(xml_mode, outfilename, nids, pointing_in_all[0],
		      pparams_in_save, file_models, surface_model_all[0],
		      solution_id);

	// Edit out a bad tiepoint

	if (sqrt(params.max_error) > permitted_error) {

	    redo_solution = TRUE;	// redo it after tiepoint deleted

	    if (interactive) {			// interactive

		// interactive tiepoint editing for the worst point

		i=params.who;
		int status = mars_edit_tiepoint(params.who, n_overlaps,
			tiepoints, file_models);
		if (status > 0)			// user abort
		    redo_solution = FALSE;
		if (status < 0)
		    start_over = FALSE;	// continue from where we left off
	    }

	    else {				// batch

		// Batch tiepoint editing.  Restart from where we left off
		// "recycle" times, then delete the worst point, then do the
		// recycles again, etc. until we get a good solution or run
		// out of points.

		if (recycle_count < recycle) {
		    start_over = FALSE;	// continue from where we left off
		    recycle_count++;
		    zvmessage("--------", "");
		    snprintf(msg, MSG_SIZE, "Recycling solution, iteration #%d of %d",
							recycle_count, recycle);
		    zvmessage(msg, "");
		}
		else if (!remove_flag) {	// Don't remove; quit
		    redo_solution = FALSE;
		}
		else if ((max_remove != 0) && (remove_loops >= max_remove)) {
		    redo_solution = FALSE;	// too many loops
		    zvmessage("Reached max_remove limit on remove loops","");
		}
		else {
		    recycle_count = 0;
		    if (n_overlaps == 0) {	// nothing to remove!
			redo_solution = FALSE;	// (probably can't happen?)
			zvmessage(
			  "Out of tiepoints!  Last solution being returned","");
		    }
		    else {
			remove_loops++;
			if (max_residual == 0.0) {
			    snprintf(msg, MSG_SIZE, "Tiepoint %d removed",
					params.who);
			    zvmessage(msg, "");
			    tiepoints[params.who].quality = 0.0;
			}
			else {	// Remove everything above max_residual
			    // resids are squared in the tiepoint list
			    double mr = max_residual * max_residual;
			    int nr = 0;
			    for (int r=0; r < n_overlaps; r++) {
				if (!tiepoints[r].active)
				    continue;
				if (tiepoints[r].residual >= mr) {
				    if (mr == 0)
					zvmessage("Removing tiepoints:","");
				    mars_print_one_tiepoint(&tiepoints[r], r);
				    tiepoints[r].quality = 0.0;
				    nr++;
				}
			    }
			    if (nr == 0) {	// nothing removed
				redo_solution = FALSE;
			    }
			    else {
				snprintf(msg, MSG_SIZE,
					"%d tiepoints removed for being above max_residual",
					nr);
				zvmessage(msg, "");
			    }
			}
		    }
		}
	    }

	    mars_remove_bad_points(tiepoints, n_overlaps, 0.0);
	}

	// If we're redoing the solution and starting over, reset the
	// pointing to where we started (effectively erasing the last attempt).

	if (redo_solution) {
	    if (start_over) {		// Reset pointing to where we started
		zvmessage("Resetting pointing to initial conditions", "");
		for (i=0; i < nids; i++) {
		    // Probably don't need to reset all the threads, but it
		    // just seems safer...
		    for (int j=0; j < num_thr; j++) {
		        pointing_in_all[j][i]->setPointingParameters(
						pparams_in[i], PIG_MAX_PARAMS);
		    }
		}
	    }
	    else {
		zvmessage("Restarting with last solution as starting point","");
		// Copying the current to all the threads is *probably*
		// unnecessary, but it just seems safer...
		for (int i=0; i < nids; i++) {
		    double ptmp[PIG_MAX_PARAMS];
		    pointing_in_all[0][i]->getPointingParameters(ptmp,
								PIG_MAX_PARAMS);
		    for (int j=1; j < num_thr; j++) {
		        pointing_in_all[j][i]->setPointingParameters(ptmp,
								PIG_MAX_PARAMS);
		    }
		}
	    }
	}
    }

    // save tiepoints -- XML format only

    mars_save_tiepoints(out_tptlist, tiepoints, n_overlaps,
		file_models, nids, start_key, cs);

    // print out tiepoints

    zvmessage("Final tiepoint list (Camera coordinates)", "");
    mars_print_tiepoint_list(tiepoints, n_overlaps);

    // determine which pictures are represented (when present[i] is 1)

    int present[MAX_INPUTS];

    for (i=0; i < nids; i++)
	present[i] = 0;
    for (i=0; i < n_overlaps; i++) {
	if (!tiepoints[i].active)
	    continue;
	present[tiepoints[i].left_image]++;
	present[tiepoints[i].right_image]++;
    }

    for (i=0; i < nids; i++) {
	if (present[i] == 0) {
	    snprintf(msg, MSG_SIZE, "Input image %d not represented by tiepoints", i+1);
	    zvmessage(msg, "");
	}
    }

    // If asked, replace those that have too few tiepoints with their initial
    // values to prevent drift

    int mintie;
    zvp("MINTIE", &mintie, &count);

    for (i=0; i < nids; i++) {
	if (present[i] < mintie) {
	    snprintf(msg, MSG_SIZE, "Too few ties for image %d (%d), resetting to initial",
				i+1, present[i]);
	    zvmessage(msg, "");
	    pointing_in_all[0][i]->setPointingParameters(pparams_in[i], refadj);
	}
    }

zvmessage("Mean Shift stuff not implemented yet!!!!","");
#if 0			/*!!!!*/
/* Compute mean shift for az & el using only the represented images.
   All other solutions are nonsense */
 double d_az,d_el;
 d_az=0.;
 d_el=0.;
 k=0;
 for(i=0; i < nids; i++){
   if(present[i] == 1){
     d_az += azimuth_in[i]-azimuth_in_save[i];
     d_el += elevation_in[i]-elevation_in_save[i];
     k += 1;
   }
 }
 d_az=d_az/(double)k;
 d_el=d_el/(double)k;
 snprintf(msg, MSG_SIZE,"Mean shifts in Azimuth & Elevation: %8.3f %8.3f",d_az,d_el);
 zvmessage(msg," ");

/* replace solution for unrepresented images by mean shift */
 for (i=0; i < nids; i++){
   if(present[i] == 0){
      azimuth_in[i]=azimuth_in_save[i]+d_az;
      elevation_in[i]=elevation_in_save[i]+d_el;
   }
 }
#endif	/*!!!!*/

    for (i=0; i < nids; i++) {
        if ((refimg[i]) && !(present[i])) {
	    snprintf(msg, MSG_SIZE, "Reference image %d not connected to rest of mosaic.",
									i);
	    zvmessage(msg, "");
	}
    }

    // reset reference image to saved value

    for (i=0; i < nids; i++) {
	if (refimg[i]) {
	    pointing_in_all[0][i]->setPointingParameters(pparams_in[i], refadj);
	}
    }

    // re-check normalized solution to be certain

    pack_pointing_params(Pzero, lambda, ndim, nids, pointing_in_all[0],
			refimg, refadj, surface_model_all[0],
			adjust_pointing, adjust_surface,
			adjust_sites,  site_list, num_sites);

    residual = Objective(Pzero, ndim, &params);

    snprintf(msg, MSG_SIZE, "Final solution mean pixel error %f", sqrt(residual));
    zvmessage(msg, "");

    snprintf(msg, MSG_SIZE, "%d iterations required", iter);
    zvmessage(msg, "");

    if (!homogeneous_inputs)
	zvmessage("(Instrument coord labels apply only to first input and matching instruments)", "");

    zvmessage("    --Surface Fixed-- --------------------Instrument[0]--------------------", "");
    zvmessage("Img ----corrected---- ----original----- ----corrected---- ------delta------", "");
    snprintf(msg, MSG_SIZE, " #  azimuth elevation %9.9s %9.9s %9.9s %9.9s %9.9s %9.9s",
		pointing_in_all[0][0]->getPointingParamName(0),
		pointing_in_all[0][0]->getPointingParamName(1),
		pointing_in_all[0][0]->getPointingParamName(0),
		pointing_in_all[0][0]->getPointingParamName(1),
		pointing_in_all[0][0]->getPointingParamName(0),
		pointing_in_all[0][0]->getPointingParamName(1));
    zvmessage(msg, "");

    for (i=0; i < nids; i++) {
	double p[PIG_MAX_PARAMS];
	p[0] = p[1] = 0.0;		// in case not enough params
	pointing_in_all[0][i]->getPointingParameters(p, PIG_MAX_PARAMS);
	snprintf(msg, MSG_SIZE, "%3d %8.3f %8.3f %8.3f %8.3f %8.3f %8.3f %8.3f %8.3f", i+1,
	 PigRad2Deg(cs->getAz(pointing_in_all[0][i]->getCameraOrientation(cs))),
	 PigRad2Deg(cs->getEl(pointing_in_all[0][i]->getCameraOrientation(cs))),
		pparams_in_save[i][0], pparams_in_save[i][1],
		p[0], p[1],
		p[0] - pparams_in_save[i][0],
		p[1] - pparams_in_save[i][1]);
	zvmessage(msg, "");
    }

    // write out pointingtable
    //!!!! This assumes only 2 entries in the nav table to be compatible
    // with mpfnav format.  File format needs to be updated!!!!

//!!!! forone:; used to be here.  Should subroutinize this, and call from above

    save_pointing(xml_mode, outfilename, nids, pointing_in_all[0],
		  pparams_in_save, file_models, surface_model_all[0],
		  solution_id);

    if (adjust_surface) {
        snprintf(msg, MSG_SIZE, "Surface model in %s frame", cs->getFrameName());
	zvmessage(msg, "");
	surface_model_all[0]->print();
	if (cs != output_cs) {
	    snprintf(msg, MSG_SIZE, "Surface model in %s frame",output_cs->getFrameName());
	    zvmessage(msg, "");
	    surface_model_all[0]->setCoordSystem(output_cs);
	    surface_model_all[0]->print();
	}
    }

    if (adjust_sites)
	print_sites(site_list, num_sites);

    delete[] tiepoints;

}

////////////////////////////////////////////////////////////////////////
// Objective function for navigation solution
//
// Each type of tiepoint has a different cost function in here.
////////////////////////////////////////////////////////////////////////

extern "C"
double Objective(double p[], int ndim, void *func_args)
{
    char msg[MSG_SIZE];
    ObjectiveParams *params = (ObjectiveParams *)func_args;

    // Point cameras based on parameters.  It will not touch the pointing
    // of the reference images.

    int thr_idx = 0;
#ifdef _OPENMP
    if (params->omp_on)
	thr_idx = omp_get_thread_num();
#endif

    double inertia_wt =
	unpack_pointing_params(p, ndim, params->nids,
		(*params->pointing_in_all)[thr_idx],
		params->refimg, params->refadj,
		params->surface_model_all[thr_idx],
		params->adjust_pointing, params->adjust_surface,
		params->adjust_sites,
		*(params->site_list), params->num_sites,
		params->pparams_in_save, params->inertia);

    // compare predicted with actual tiepoints

    double sum = inertia_wt;
    params->max_error = 0.0;
    params->who = 0;

    for (int k=0; k < *params->n_overlaps; k++) {
	PigPoint origin;
	PigVector look;
	PigPoint surf_pt;
	int hits;
	double line_in, samp_in;
	double line_out, samp_out;
	int i, j;
	double delta_x, delta_y, dot;
	PigVector delta;
	PigVector norm;
	PigSurfacePlane *sm = NULL;

	TiePoint *t = &params->tiepoints[k];
	if (!t->active)
	    continue;

       	double err = 0.0;

	PigPoint left_origin, right_origin;
	PigVector left_vector, right_vector;
	double left_l, left_s, right_l, right_s;
	// int status;
	PigPoint xyz;

	switch (t->type) {

	    case TIEPOINT_TRADITIONAL:
	    case TIEPOINT_MISS_DISTANCE:

		err = 0.0;

		if (t->type == TIEPOINT_MISS_DISTANCE ||
		    params->traditional_mode_i == MISS_DISTANCE ||
		    params->traditional_mode_i == BOTH_TRADITIONAL_AND_MISS) {
		    // set err to miss dist error

		    // Compute unit vectors for left and right images

		    left_l = t->left_line;
		    left_s = t->left_sample;
		    right_l = t->corrected_line;
		    right_s = t->corrected_sample;
    
		    i = t->left_image;
		    j = t->right_image;

//printf("----\n");		//!!!!
//printf("i=%d, j=%d, ll=%lf, ls=%lf, rl=%lf, rs=%lf\n", i, j, left_l, left_s, right_l, right_s);	//!!!!
		    (*params->camera_in_all)[thr_idx][i]->LStoLookVector(
			left_l,left_s, left_origin, left_vector, params->cs);
		    (*params->camera_in_all)[thr_idx][j]->LStoLookVector(
			right_l,right_s, right_origin, right_vector,params->cs);

		    // If the rays are parallel (or very close to it), then
		    // miss distance makes no sense, as there's no intersection.
		    // Rather than have an arbitrary intersection a huge
		    // distance away, simply suppress any that are too close
		    // together (making their error 0).

		    if (left_vector % right_vector > params->parallel_limit) {
			err = 0.0;
//printf("SKIPPING\n");	//!!!!
		    } else {
		        // Compute x,y,z and error by finding the intersection
		        // of the two rays.  We really compute the closest
		        // point between the lines since they'll almost never
		        // intersect. Here we ignore the x,y,z and use the err.
    
		      /* status = */ xvector(left_origin, right_origin,
				left_vector, right_vector, xyz, &err);
//double origerr = err;	//!!!!
//if (status != 0) printf("xvector status: %d %s\n", status, (status==0)?"ok":"***FAILURE***");	//!!!!
//printf("dot product: %lf, err=%lg\n", left_vector % right_vector, err);	//!!!!

//printf("left origin: %lf %lf %lf\nright origin:%lf %lf %lf\nxyz:         %lf %lf %lf\n", left_origin.getX(), left_origin.getY(), left_origin.getZ(), right_origin.getX(), right_origin.getY(), right_origin.getZ(), xyz.getX(), xyz.getY(), xyz.getZ());	//!!!!
//printf("left_vector: %lf %lf %lf\nright_vector:%lf %lf %lf err=%20.15lf\n", left_vector.getX(), left_vector.getY(), left_vector.getZ(), right_vector.getX(), right_vector.getY(), right_vector.getZ(), err);	//!!!!

		        // miss_mult is always applied to xyz errors, even
		        // when mode==MISS_DISTANCE, because there may be
		        // a mix of tiepoint types.  Do it before squaring
			// so the numbers don't get ridiculously small.

		        err *= params->miss_mult;

		        // Square the error
		        err = err * err;

		        // Optionally scale by the range.  Really this is
		        // (err/range)^2 but doing err^2/range^2 saves a sqrt.
		        if (params->range_miss) {
			    double range = (xyz - left_origin).magnitude_sq();
//printf("err: %lg, rangesq: %lf, range=%lf\n", err, range, sqrt(range));	//!!!!
			    err /= range;
//printf("post:%lg\n", err);		//!!!!


#if 0	//!!!!
if (err>1e10) {		//!!!!
printf("----  origerr=%lf\n", origerr);		//!!!!
printf("i=%d, j=%d, ll=%lf, ls=%lf, rl=%lf, rs=%lf\n", i, j, left_l, left_s, right_l, right_s);	//!!!!
if (status != 0) printf("xvector status: %d %s\n", status, (status==0)?"ok":"***FAILURE***");	//!!!!
printf("dot product: %lf, err=%lg\n", left_vector % right_vector, err);	//!!!!

printf("left origin: %lf %lf %lf\nright origin:%lf %lf %lf\nxyz:         %lf %lf %lf\n", left_origin.getX(), left_origin.getY(), left_origin.getZ(), right_origin.getX(), right_origin.getY(), right_origin.getZ(), xyz.getX(), xyz.getY(), xyz.getZ());	//!!!!
printf("left_vector: %lf %lf %lf\nright_vector:%lf %lf %lf err=%20.15lf\n", left_vector.getX(), left_vector.getY(), left_vector.getZ(), right_vector.getX(), right_vector.getY(), right_vector.getZ(), err);	//!!!!
printf("err: %lg, rangesq: %lf, range=%lf\n", err, range, sqrt(range));	//!!!!
printf("post:%lg\n", err);		//!!!!
}	//!!!!
#endif	//!!!!
		        }

		    }
		}

		if (t->type == TIEPOINT_TRADITIONAL &&
		    (params->traditional_mode_i == TRADITIONAL ||
		     params->traditional_mode_i == BOTH_TRADITIONAL_AND_MISS)) {
		    // increment err by traditional error; may be adding to miss dist error from above

		    // traditional tiepoint.  Project the Left point to the
		    // surface, and then back to the Right image.  Compare the
		    // difference.

		    i = t->left_image;
		    j = t->right_image;

		    samp_in = t->left_sample;
		    line_in = t->left_line;

		    (*params->camera_in_all)[thr_idx][i]->LStoLookVector(
				line_in, samp_in, origin, look, params->cs);
		    hits=params->surface_model_all[thr_idx]->
					intersectRay(origin, look, surf_pt);

		    //!!!! enable this for matching with mpfnav (surface is
		    //!!!! mirrored instead of at infinity)
		    //!!!! if (hits < 0) hits = params->surface_model_all[thr_idx]->intersectRay(origin, look*-1, surf_pt);
		    (*params->camera_in_all)[thr_idx][j]->XYZtoLS(surf_pt, (hits<=0),
					  &line_out, &samp_out, params->cs);

		    // line/samp_out are now right-side predicted location
		    err +=
			(line_out - t->corrected_line) *
			(line_out - t->corrected_line) +
			(samp_out - t->corrected_sample) *
			(samp_out - t->corrected_sample);
		}

		break;

	    case TIEPOINT_FIDUCIAL:
	    case TIEPOINT_ORBITAL_XYZ:

		// Fiducial tiepoint.  Project the XYZ point into the
		// Left image and compare the difference.
		// Orbital XYZ tiepoints work the same way.

		i = t->left_image;

		samp_in = t->left_sample;
		line_in = t->left_line;

		(*params->camera_in_all)[thr_idx][i]->XYZtoLS(t->xyz, FALSE,
				&line_out, &samp_out, t->cs);

		// line/samp_out are now left-side projected location

		err = (line_out - line_in) *
		      (line_out - line_in) +
		      (samp_out - samp_in) *
		      (samp_out - samp_in);

		break;

	    case TIEPOINT_1D:

		// 1-D tiepoint.  Project the Left point to the surface,
		// and then back to the Right image.  Compare the difference.
		// However, only the component of the difference perpendicular
		// to the given angle is used.

		i = t->left_image;
		j = t->right_image;

		samp_in = t->left_sample;
		line_in = t->left_line;

		(*params->camera_in_all)[thr_idx][i]->LStoLookVector(line_in, samp_in,
					origin, look, params->cs);
		hits=params->surface_model_all[thr_idx]->intersectRay(
					origin, look, surf_pt);
		(*params->camera_in_all)[thr_idx][j]->XYZtoLS(surf_pt, (hits<=0),
				&line_out, &samp_out, params->cs);

		// line/samp_out are now right-side predicted location

		// Normal to line defined by the angle (unit vector)
		norm.setXYZ(-sin(PigDeg2Rad(t->angle)),
			     cos(PigDeg2Rad(t->angle)), 0.0);
		// Vector representing the delta
		delta_y = -(line_out - t->corrected_line);
		delta_x = samp_out - t->corrected_sample;
		delta.setXYZ(delta_x, delta_y, 0.0);
		// Dot product is projection of delta onto norm
		dot = norm % delta;
		// Square the error
		err = dot * dot;

		break;

	    case TIEPOINT_INFINITY:

		// Infinity tiepoint.  Project the Left point to infinity,
		// and then back to the Right image.  Compare the difference.
		// Used for tying distant points when range should be ignored.

		i = t->left_image;
		j = t->right_image;

		samp_in = t->left_sample;
		line_in = t->left_line;

		(*params->camera_in_all)[thr_idx][i]->LStoLookVector(line_in, samp_in,
					origin, look, params->cs);
		// Look vector *is* the direction projected to infinity

		(*params->camera_in_all)[thr_idx][j]->XYZtoLS(look, TRUE,
				&line_out, &samp_out, params->cs);

		// line/samp_out are now right-side predicted location

		err = (line_out - t->corrected_line) *
		     (line_out - t->corrected_line) +
		     (samp_out - t->corrected_sample) *
		     (samp_out - t->corrected_sample);

		break;

	    case TIEPOINT_Z_SURFACE:

		// Z-surface tiepoint.  Project the Left point to a special
		// surface model with normal (0,0,-1) and ground (0,0,z),
		// and then back to the Right image.  Compare the difference.

		i = t->left_image;
		j = t->right_image;

		samp_in = t->left_sample;
		line_in = t->left_line;

		(*params->camera_in_all)[thr_idx][i]->LStoLookVector(line_in, samp_in,
					origin, look, params->cs);
		{			// avoid issues with local vars here
		  PigVector surf_norm(0, 0, -1);
		  PigPoint surf_ground(0, 0, t->xyz.getZ());
		  sm = new PigSurfacePlane(surf_norm, surf_ground, t->cs);
		}

		hits = sm->intersectRay(origin, look, surf_pt);
		delete sm;

		(*params->camera_in_all)[thr_idx][j]->XYZtoLS(surf_pt, (hits<=0),
				&line_out, &samp_out, params->cs);

		// line/samp_out are now right-side predicted location

		err = (line_out - t->corrected_line) *
		     (line_out - t->corrected_line) +
		     (samp_out - t->corrected_sample) *
		     (samp_out - t->corrected_sample);

		break;

	    case TIEPOINT_DYNAMIC_XYZ:

		// Dynamic XYZ tiepoint.  Convert the XYZ point into the
		// common coordinate system and then project it to the
		// RIGHT image and compare the difference.

		err = 0.0;
		if (!t->active)
		    break;			// not a valid point, ignore it

		i = t->right_image;

		// Tne coordinates automatically get converted by the
		// XYZtoLS routine...

		(*params->camera_in_all)[thr_idx][i]->XYZtoLS(t->xyz, FALSE,
				&line_out, &samp_out, t->cs);

		// line/samp_out are now right-side projected location

		err = (line_out - t->right_line) *
		     (line_out - t->right_line) +
		     (samp_out - t->right_sample) *
		     (samp_out - t->right_sample);

		break;

	    case TIEPOINT_ORBITAL_XY:

// Used to localize rover by tying to features visible in orbital images.  The
// Z value of the point is unknown.  This translates to looking at azimuth
// errors only (ignoring elevation errors).  Thus XY position and azimuth (yaw)
// orientation of the rover can be determined, but not pitch/roll or Z.
// Method: Project image_point_1 to get az/el.  Use that elevation to compute
// a Z at the XY location.  Project that XYZ into the image.  Use only the
// component of error in the azimuth direction (requires determining the
// orientation of the image w.r.t. the coordinate system).

	    case TIEPOINT_AZIMUTH:

// Used when horizon features are at a known azimuth, but elevation may not be
// known.
// Method:  Project image_point_1 to get az/el, then combine that el with the
// supplied azimuth and project back.  Error is azimuth component only, like
// orbital-XY tiepoints.
// COORD SYSTEM TRANSLATION NOT IMPLEMENTED YET IN THE READER

	    case TIEPOINT_ELEVATION:

// Used when elevation of features is known (e.g. a distant horizon) but azimuth
// is not.  Note that if both az and el are desired, the same image_point can
// be supplied with az and el separately.
// Method:  Like azimuth tiepoint, but swap az/el.
// COORD SYSTEM TRANSLATION NOT IMPLEMENTED YET IN THE READER

		// FOR ALL OF THE ABOVE:

		snprintf(msg, MSG_SIZE, "Tiepoint type %d is not yet implemented.  Ignored", t->type);
		zvmessage(msg, "");
		err = 0.0;
		break;

	    default:
		snprintf(msg, MSG_SIZE, "Warning: Unknown tiepoint type %d. Ignored", t->type);
		zvmessage(msg, "");
		err = 0.0;
		break;
	}

	if (err > 0.0) {
	    if (err > params->max_error) {
//printf("max err: %d %lf\n", k, err);	//!!!!
		// These aren't used in parallel calls from pamoeba, only in
		// single calls from the main program.  So, there's no need to
		// deal with them properly (which would be pragma omp atomic).
		params->max_error = err;
		params->who = k;
	    }
	}
	else {
	    err = 0.0;
	}
	t->residual = err;

	sum += err;

    }

//printf("Objective returning: %lf\n", (sum+.0001) / (double)(*params->n_overlaps));	//!!!!
    return (sum+.0001) / (double)(*params->n_overlaps);
}

////////////////////////////////////////////////////////////////////////
// Wrapper for save_pointing_file and save_pointing_xml
////////////////////////////////////////////////////////////////////////

void save_pointing(int xml_mode, char *name, int n,
		PigPointingModel *pointings[],
		double pparams_orig[][PIG_MAX_PARAMS],
     		PigFileModel *files[], PigSurfaceModel *surface_model, 
	        char *solution_id)
{
    if (xml_mode)
        save_pointing_xml(name, n, pointings, pparams_orig, files, 
                          surface_model, solution_id);
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
    char msg[MSG_SIZE];

    if ((fout = fopen(name, "w")) == NULL) {
	snprintf(msg, MSG_SIZE, "Error opening pointing corrections file %s\n", name);
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
		       char *solution_id)
{ 
    FILE *fout;
    int i, j, count;
    char msg[MSG_SIZE];
#define POINT_FILE_SIZE 255
    char point_file[POINT_FILE_SIZE];

    PigMission *m = PigMission::getMissionObject(files[0]);

    PigCameraModel *camera_model = pointings[0]->getCameraModel();
    const char *mission = camera_model->getMissionName();

//!!!! This should really be multimission-ized...
    if (!strncasecmp(mission, "MER", 3)) 
        snprintf(point_file, POINT_FILE_SIZE, "%s_pma.point", ((PigMER *)m)->getHostID());
    else 
        snprintf(point_file, POINT_FILE_SIZE, "%s_mast.point", mission);

    if ((fout = fopen(name, "w")) == NULL) {
        snprintf(msg, MSG_SIZE, "Error opening pointing corrections file %s\n", name);
        zvmessage(msg, "");
        zabend();
    }

    // Check to see if we want to use UniqueId2 in the file instead
    int use_uniqueid2 = false;
    char point_method[256];
    char *value;
    zvp("POINT_METHOD", point_method, &count);
    if (count != 0) {
        value = PigModelBase::parseParamString(point_method, "USE_UNIQUEID2");
        if (value != NULL)
            use_uniqueid2 = true;
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
    fprintf(fout, " institution=\"%s\" program=\"%s\">\n", "mipl", "marsnav");

    fprintf(fout, "    <purpose>A pointing correction file for one mosaic might look for %s%s\n",
            mission, "</purpose>");
    fprintf(fout, "  </origination>\n");

    //this part may not need 
    fprintf(fout, "  <static_parameters file=\"%s\" instrument=\"%s\"", point_file, inst_id);
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
	if (use_uniqueid2)
            files[i]->getUniqueId2(unique_id);
	else
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

        fprintf(fout, "      <original_parameters type=\"%s\">\n", pointings[i]->getModelName());
        for (j=0; j<count; j++)                   // Print original params
            fprintf(fout, "        <parameter id=\"%s\" value=\"%g\"/>\n", 
                    pointings[i]->getPointingParamName(j), pparams_orig[i][j]);
        fprintf(fout, "      </original_parameters>\n");
        fprintf(fout, "    </image>\n");
      
        fprintf(fout, "    <pointing_parameters type=\"%s\">\n", pointings[i]->getModelName());
        for (j=0; j<count; j++)                   // Print corrected params
            fprintf(fout, "      <parameter id=\"%s\" value=\"%g\"/>\n", 
                    pointings[i]->getPointingParamName(j), p[j]);

        fprintf(fout, "    </pointing_parameters>\n");

        PigPoint c_point;
        PigVector a_vector, h_vector, v_vector, o_vector, r_vector, e_vector;
        int mtype = 0;
        double mparam = 0.;
        camera_model = pointings[i]->getCameraModel();
        const char *cm_type = camera_model->getModelName();
    
        if (strcasecmp(cm_type, "CAHV") == 0) {
            ((PigCAHV*)camera_model)->getCurrentCAHV(c_point, a_vector, h_vector, v_vector);
        }
        else if (strcasecmp(cm_type, "CAHVOR") == 0) {
            ((PigCAHVOR*)camera_model)->getCurrentCAHVOR(c_point, a_vector, h_vector, 
                                                         v_vector, o_vector, r_vector);
        }
        else if (strcasecmp(cm_type, "CAHVORE") == 0) {
            ((PigCAHVORE*)camera_model)->getCurrentCAHVORE(c_point, a_vector, h_vector, v_vector, 
                                                           o_vector, r_vector, e_vector, mtype, mparam);
        }
        else { // for camera_model = NONE
            snprintf(msg, MSG_SIZE, "Camera model type: %s\n", cm_type);
            zvmessage(msg, "");
        }

        fprintf(fout, "    <camera_model type=\"%s\">\n", cm_type);
        if (strncasecmp(cm_type, "CAHV", 4)==0) {
            fprintf(fout, "      <parameter id=\"C\" type=\"float_3\" ");
            fprintf(fout, "value1=\"%g\" value2=\"%g\" value3=\"%g\"/>\n",
                    c_point.getX(), c_point.getY(), c_point.getZ());
   
            fprintf(fout, "      <parameter id=\"A\" type=\"float_3\" ");
            fprintf(fout, "value1=\"%g\" value2=\"%g\" value3=\"%g\"/>\n", 
                    a_vector.getX(), a_vector.getY(), a_vector.getZ());

            fprintf(fout, "      <parameter id=\"H\" type=\"float_3\" ");
            fprintf(fout, "value1=\"%g\" value2=\"%g\" value3=\"%g\"/>\n", 
                    h_vector.getX(), h_vector.getY(), h_vector.getZ());

            fprintf(fout, "      <parameter id=\"V\" type=\"float_3\" ");
            fprintf(fout, "value1=\"%g\" value2=\"%g\" value3=\"%g\"/>\n", 
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

    for (int cnt = 0; cnt<count; cnt++)                   // Print surface model params
    fprintf(fout, "      <parameter id=\"%s\" value=\"%g\"/>\n", 
    surface_model->getPointingParamName(cnt), surf_pp[cnt]);

    // print out surface model's coordinate system definition
    const char *ref_name = surface_model->getCoordSystem()->getFrameName();
    if (ref_name != NULL) {
       fprintf(fout, "      <reference_frame");
       fprintf(fout, " name=\"%s\"", ref_name);
       
       if (surface_model->getCoordSystem() != NULL) {
	   PigCoordSystem *cs_r = surface_model->getCoordSystem();
	   for (int cnt = 0; cnt<cs_r->getNumIndices(); cnt++) {
	       fprintf(fout, " index%d=\"%d\"", cnt+1, (cs_r->getIndex(cnt)));
	   }
	   fprintf(fout, "/>\n");
       }
    }
    fprintf(fout, "  </surface_model>\n");
    fprintf(fout, "</pointing_correction>\n");
    fclose(fout);
}

////////////////////////////////////////////////////////////////////////
// Take the pointing parameters from the PointingModel's and pack them
// into a single array Pzero (for use by the minimization algorithms).
// ndim is returned, it's the number of parameters in Pzero.  Reference
// images are ignored.  Also returns lambda, the "length scale" for each
// element in Pzero (really the expected error).
////////////////////////////////////////////////////////////////////////

// Take one item implementing PigAdjustable and add its parameters to the
// Pzero and lambda arrays

#define STUFF(what, min, max)					\
	int n = (what)->getPointingParamCount();		\
	(what)->getPointingParameters(p, n);			\
	(what)->getPointingErrorEstimate(err, n);		\
	for (int j = min; j < max; j++) {			\
	    Pzero[ndim] = p[j];					\
	    lambda[ndim++] = err[j];				\
	}

void pack_pointing_params(double Pzero[], double lambda[], int &ndim, int nids,
		PigPointingModel **pointing_in,
		int refimg[], int refadj, PigSurfaceModel *surface_model,
		int adjust_pointing, int adjust_surface, int adjust_sites,
		SiteList *site_list, int num_sites)
{
    double p[PIG_MAX_PARAMS];
    double err[PIG_MAX_PARAMS];

    ndim = 0;
    if (adjust_pointing) {
        for (int i = 0; i < nids; i++) {

	    // Get the pointing parameters

	    if (refimg[i]) {			// only some for refimg's
		if (refadj != PIG_MAX_PARAMS) {
		    STUFF(pointing_in[i], refadj, n);
		}
	    }
	    else {
	        STUFF(pointing_in[i], 0, n)
	    }
        }
    }
    if (adjust_surface) {

	// Get the surface parameters

	STUFF(surface_model, 0, n);
    }

    // Look for Sites to adjust

    if (adjust_sites) {
        for (int i=0; i < num_sites; i++) {
	    if (site_list[i].adjust_location) {
	        if (site_list[i].site->getPointingParamCount() != 6) {
		    zvmessage("Internal error!!  Site pointing param count not 6!!","");
		    zabend();
	        }
	        STUFF(site_list[i].site, 0, 3);
	    }
	    if (site_list[i].adjust_orientation) {
	        if (site_list[i].site->getPointingParamCount() != 6) {
		    zvmessage("Internal error!!  Site pointing param count not 6!!","");
		    zabend();
	        }
	        STUFF(site_list[i].site, 3, 6);
	    }
        }
    }
}

////////////////////////////////////////////////////////////////////////
// Take the a packed array Pzero, extract pointing parameters from it,
// and update the PointingModel's.  Reference images are ignored, but
// refimg must match what was given the pack routine.
//
// If pparams_orig and inertia are given, then the deltas between
// the current and orig params are computed, multiplied by weight, and
// summed to become the function return.  This provides an incentive for
// the pointings to "stay in place", or an inertia.  It is computed here
// simply because this is where we have pointing parameters readily
// available.
////////////////////////////////////////////////////////////////////////

double unpack_pointing_params(double Pzero[], int ndim, int nids, 
		PigPointingModel **pointing_in,
		int refimg[], int refadj, PigSurfaceModel *surface_model,
		int adjust_pointing, int adjust_surface, int adjust_sites,
		SiteList *site_list, int num_sites,
		double (*pparams_save)[MAX_INPUTS][PIG_MAX_PARAMS],
		double (*inertia)[PIG_MAX_PARAMS])
{
    double p[PIG_MAX_PARAMS];

    int nctr = 0;
    double weight = 0.0;

    if (adjust_pointing) {
        for (int i = 0; i < nids; i++) {

	    // Get the pointing parameters

	    int n = pointing_in[i]->getPointingParamCount();

	    if (refimg[i])	// get initial parameters to pass through
		pointing_in[i]->getPointingParameters(p, n);

	    for (int j = 0; j < n; j++) {
		if (refimg[i] && j < refadj)  // ref, not in refadj range
		    continue;
	        if (nctr >= ndim) {		// oops!  Ran out of params!
		    zvmessage("Internal error:  Not enough parameters in unpack_pointing_params (pointing)!", "");
		    zabend();
	        }
	        p[j] = Pzero[nctr++];

	        // Compute the stay-in-place weights

	        if (pparams_save != NULL && inertia != NULL) {
		    double wt = (p[j] - (*pparams_save)[i][j]) *
							(*inertia)[j];
		    weight += (wt * wt);
		}
	    }

	    // Set the parameters in the model

	    pointing_in[i]->setPointingParameters(p, n);

        }
    }

    if (adjust_surface) {

	// Get the surface parameters

	int n = surface_model->getPointingParamCount();

	for (int j = 0; j < n; j++) {
	    if (nctr >= ndim) {		// oops!  Ran out of params!
		zvmessage("Internal error:  Not enough parameters in unpack_pointing_params (surface)!", "");
		zabend();
	    }
	    p[j] = Pzero[nctr++];
	}

	// Set the parameters in the model

	surface_model->setPointingParameters(p, n);
    }

    // Adjust any Sites.  We've already verified there are 6 params

    if (adjust_sites) {
        for (int i=0; i < num_sites; i++) {
	    if (site_list[i].adjust_location ||
		site_list[i].adjust_orientation) {

	        // Get params in case we are adjusting only half
	        site_list[i].site->getPointingParameters(p, 6);

                if (site_list[i].adjust_location) {
	            for (int j = 0; j < 3; j++) {
	                if (nctr >= ndim) {	// oops!  Ran out of params!
		            zvmessage("Internal error:  Not enough parameters in unpack_pointing_params (location)!", "");
		            zabend();
	                }
	                p[j] = Pzero[nctr++];
	            }
	        }

                if (site_list[i].adjust_orientation) {
	            for (int j = 3; j < 6; j++) {
	                if (nctr >= ndim) {	// oops!  Ran out of params!
		            zvmessage("Internal error:  Not enough parameters in unpack_pointing_params (orientation)!", "");
		            zabend();
	                }
	                p[j] = Pzero[nctr++];
	            }
	        }

	        // Set the parameters in the model

	        site_list[i].site->setPointingParameters(p, 6);
	    }
	}
    }

    if (nctr != ndim) {			// oops!  Too many params!
	zvmessage("Internal error:  Too many parameters in unpack_pointing_params!", "");
	zabend();
    }

    return weight;
}

////////////////////////////////////////////////////////////////////////
// Print out any sites that are being adjusted.  This is in lieu of actually
// writing out an RMC file, for the time being... !!!!
////////////////////////////////////////////////////////////////////////

void print_sites(SiteList site_list[], int num_sites)
{
    char msg[MSG_SIZE];

    for (int i=0; i < num_sites; i++) {
	if (site_list[i].adjust_location || site_list[i].adjust_orientation) {
	    snprintf(msg, MSG_SIZE, "CS %d, '%s'", i, site_list[i].site->getFullName());
	    zvmessage(msg, "");
	    double p[6];			// we know it's 6 already
	    site_list[i].site->getPointingParameters(p, 6);
	    // Print xyz explicitly instead of getPointintParamName() so
	    // the case is right for cut-and-paste...
	    snprintf(msg, MSG_SIZE, "x=\"%g\" y=\"%g\" z=\"%g\"", p[0], p[1], p[2]);
	    zvmessage(msg, "");

	    snprintf(msg, MSG_SIZE, "%s=\"%g\" %s=\"%g\" %s=\"%g\"",
		site_list[i].site->getPointingParamName(3), p[3],
		site_list[i].site->getPointingParamName(4), p[4],
		site_list[i].site->getPointingParamName(5), p[5]);
	    zvmessage(msg, "");

	    PigQuaternion q;
	    q.setEulerAngles(PigDeg2Rad(p[3]), PigDeg2Rad(p[4]),
							PigDeg2Rad(p[5]));
	    snprintf(msg, MSG_SIZE, "s=\"%g\" v1=\"%g\" v2=\"%g\" v3=\"%g\"",
		q.getS(), q.getV().getX(), q.getV().getY(), q.getV().getZ());
	    zvmessage(msg, "");
	}
    }
}


