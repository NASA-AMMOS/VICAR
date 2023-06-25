/* marsmap */

#include "vicmain_c"
#include "zvprintf.h"

// mpi_id and mpi_numprocs are set up by vicmain_c if ENABLE_MPI is on.
// However, we use them below in some cases if MPI is off, too.  So,
// we define them here if MPI is not enabled.

#ifdef ENABLE_MPI
#include "mpi.h"
#else
int mpi_id = 0;
int mpi_numprocs = 1;
#include "mpi_fake.h"
#endif

#include "mars_support.h"
#include "mars_overlaps.h"

#include "PigMission.h"
#include "PigFileModel.h"
#include "PigLabelModel.h"
#include "PigCameraModel.h"
#include "PigPointingModel.h"
#include "PigSurfaceModel.h"
#include "RadiometryModel.h"
#include "PigCoordSystem.h"
#include "PigBrtCorrModel.h"

#include "return_status.h"

#include <math.h>

#include "mpi_timing_macro.h"

TIME_DEFINE(total);
TIME_DEFINE(setup);
TIME_DEFINE(read);
TIME_DEFINE(image);
TIME_DEFINE(write);
TIME_DEFINE(footgrid);
TIME_DEFINE(waitend);
TIME_DEFINE(comm);

#ifdef DEBUG
#define DPR(x) { x }
#else
#define DPR(x)
#endif

/* General matrix and vector data structures for dynamic memory allocation. */
#include "mvec.h"

////////////////////////////////////////////////////////////////////////
// NOTES ON PARALLEL PROCESSING:
//
// The work on the final mosaic is divided into horizontal segments.
// Each segment is described by a beginning and an ending line.
// The master node is solely responsible for writing the output, all
// other nodes send the completed mosaic chunks to the master.
//
// mpi_id == 0 means we're the master node.
////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////

// buffer sizes in main program - these should be made dynamic, eventually!
#define MAX_INPUTS 2000
#define MAX_OPEN 2000 		// Largest size for max_open
#define MAX_NL 0		// no max
#define MAX_NS 0

#define MAX_OBUF 125000

#define MARGIN 2		// around text labels for grids

typedef enum { Cylindrical, Polar, Vertical, Sinusoidal } ProjectionType;

////////////////////////////////////////////////////////////////////////
// Used by the inverse projection routine
////////////////////////////////////////////////////////////////////////
struct MarsMapProjArgs {
    PigCameraModel **camera_in;
    PigSurfaceModel *surface_model;
    ProjectionType proj_mode;		// projection mode
    double scale;			// radians/pixel, cyl/polar only
    PigPoint proj_origin;		// cylindrical/polar only
    double line_zero_el=0;		// cylindrical only
    double az_first_sample=0;		// cylindrical only
    double az_last_sample;		// cylindrical only (info only)
    double up_azimuth;			// polar only
    int nadir_line, nadir_samp;		// polar only
    int nlo, nso;			// vertical only (valid for all)
    int nbo;				// # of bands (all)
    double vert_scale;			// pixels/meter, vertical only
    double minx, miny;			// meters, vertical only
    double maxx, maxy;			// meters, vertical only
    double min_elev, max_elev;		// cylindrical only (info only)
    double center_az, center_el;	// sinusoidal only
    PigCoordSystem *cs;			// projection coord system (all)
    int azdir;				// azimuth direction from cs (all)
};

extern "C" int cylindrical_proj_inverse(double in_line, double in_samp,
		double *out_line, double *out_samp,
		int input_number, void *proj_args);
extern "C" int polar_proj_inverse(double in_line, double in_samp,
		double *out_line, double *out_samp,
		int input_number, void *proj_args);
extern "C" int vertical_proj_inverse(double in_line, double in_samp,
		double *out_line, double *out_samp,
		int input_number, void *proj_args);
extern "C" int sinusoidal_proj_inverse(double in_line, double in_samp,
		double *out_line, double *out_samp,
		int input_number, void *proj_args);

extern "C" int cylindrical_width_func(double line, double samp, double ndeg,
		int *out_pix, double *out_zero_el,
		int input_number, void *proj_args);
extern "C" int sinusoidal_width_func(double line, double samp, double ndeg,
		int *out_pix, double *out_zero_el,
		int input_number, void *proj_args);
extern "C" int other_width_func(double line, double samp, double ndeg,
		int *out_pix, double *out_zero_el,
		int input_number, void *proj_args);

void pig_msg_suppressor(char *msg, PigMsgSeverity severity, void *clientData);

void get_linestart_end(int *linestart, int *lineend, int nlo, int cpu_id);

void draw_grid(int unit_out, ProjectionType proj_mode, MarsMapProjArgs *proj);

void draw_tiepoints(int unit_out, int nso, int nlo, PigCameraModel **camera_in,
	ProjectFunc func, MarsMapProjArgs *proj,
	PigFileModel **file_models, int nids, PigCoordSystem *cs,PigMission *m,
	int min_input, int max_input);

void draw_scalebar(int unit_out, int nso, int nlo,
	ProjectFunc func, MarsMapProjArgs *proj);
int scalebar_get_size_and_text(double ppm, int minSize, char *unitSuffix,
			char *stringPtr);
double scalebar_normalize(double value, int minSize);
double scalebar_getSignificand(double x);
double scalebar_roundSignificand(double x, int decimalPlaces);
void scalebar_getWithUnit(double value, char *unitSuffix, char *stringPtr);


////////////////////////////////////////////////////////////////////////

void main44()
{
    int status, count, def;

    char mission[64], instrument[64];

    MarsMapProjArgs proj;

    // Inputs

    int band;
    int nids;

    PigFileModel *file_models[MAX_INPUTS];
    PigCameraModel *camera_in[MAX_INPUTS];
    PigPointingModel *pointing_in[MAX_INPUTS];
    PigSurfaceModel *surface_model;
    int homogeneous_inputs = TRUE;
    SimpleImage<short int> *si_imgs[MAX_INPUTS];
    SimpleImage<float> *f_imgs[MAX_INPUTS];
    double FOV[MAX_INPUTS];				// cache for efficiency
    PigVector camera_orientation[MAX_INPUTS];		// cache for efficiency
    PigPoint camera_position[MAX_INPUTS];		// cache for efficiency
    double min_elev=0., max_elev=0., min_az, max_az=0., start_az=0., stop_az=0.;
    RadiometryModel *radiometric[MAX_INPUTS];
    PigBrtCorrModel *brt_corr[MAX_INPUTS];
    PigCoordSystem *proj_cs;
    int azdir;				// +1 == CW, -1 == CCW

    memset(si_imgs, 0, sizeof(si_imgs));
    memset(f_imgs, 0, sizeof(f_imgs));

    int incremental_mode;

    int short_int_data = 1;
    int do_clamp = 1;
    int do_interp = 1;
    char format[10];                   // input data format

    // Brightness overlap stuff

    int max_open;			// Number of inputs to read at once
    double horizon;
    int use_horizon;
    char ovr_filename[PIG_MAX_FILENAME_SIZE];
    int do_ovr, do_ovr_normal, do_ovr_overall, do_ovr_hsi;
    Overlap *overlaps = NULL;		// saved overlaps
    int num_overlaps = 0;		// Number of overlaps
    double overlap_radius = 0.0;	// Nominal size of overlaps
    double overlap_radius_sq = 0.0;
    Overlap *overall_overlaps;

    // Outputs

    int unit_out=0;
    int unit_idx_out=0, unit_icm_out=0;
    int nlo=0, nso=0;
    double scale=0.;			// radians/pixel, cyl/polar only
    PigPoint proj_origin;		// cylindrical/polar only
    double line_zero_el=0;		// cylindrical only
    double az_first_sample=0;	// cylindrical only
    double up_azimuth=0.;		// polar only
    int nadir_line=0, nadir_samp=0;	// polar only
    double minx=0., miny=0.;		// vertical only
    double maxx=0., maxy=0.;		// vertical only
    double vert_scale=0.;		// pixels/meter, vertical only
    double center_az=0., center_el=0.;	// sinusoidal only

    
    // Output buffers.  These look complicated due to MPI but really are
    // just a 2D array and a pointer to one line within that array.

    mve_simatrix obuf2d_si[MARS_MAX_NB];
    mve_sivectr  obuf2d_si_ptr[MARS_MAX_NB];
    
    mve_fmatrix obuf2d_f[MARS_MAX_NB];
    mve_fvectr  obuf2d_f_ptr[MARS_MAX_NB];

    // Coordinate output buffers

    mve_simatrix obuf_idx = NULL;
    mve_sivectr  obuf_idx_ptr = NULL;
    mve_fmatrix  obuf_icm_l = NULL;
    mve_fvectr   obuf_icm_l_ptr = NULL;
    mve_fmatrix  obuf_icm_s = NULL;
    mve_fvectr   obuf_icm_s_ptr = NULL;

    int do_print = TRUE;		// True if info message should be issued
    
    // User Parameters

    double tile_bias_value[MAX_INPUTS];
    double *bias_ptr;
    ProjectionType proj_mode;
    double dbl_param;
    int min_input, max_input;

    TIME_START(total);
    TIME_START(setup);

    zvmessage("MARSMAP version 2020-04-30", "");

    // Since all parallel machines' stdout/stderr get jumbled together on
    // the master, do_print is used to control whether or not that stuff
    // gets printed.  We want to do the printing only on the master node.
    // If you want the prints for debug reasons, set the flag.

#ifdef ENABLE_MPI
    do_print = (mpi_id == 0);
    if (!do_print)
	PigModelBase::setDefaultMsgFunction(pig_msg_suppressor, NULL);
#else
    do_print = TRUE;
#endif

    // Get the input file list, and set up initial camera/pointing models
    // for each input.

    mars_setup(nids, file_models, camera_in, pointing_in, surface_model,
               radiometric, brt_corr,
	       proj_cs, mission, instrument, homogeneous_inputs,
               MAX_NL, MAX_NS, MAX_INPUTS);

    PigMission *m = PigMission::getMissionObject(mission);

    // find out the input data type.  We check the data type of the first input
    // and assume that ALL inputs have the same data type.  Currently we 
    // support SHORT INT images and FLOAT images.  BYTE image are supported by
    // converting bytes into short ints.

    file_models[0]->openFile();
    status = zvget(file_models[0]->getUnit(), "FORMAT", format, NULL);

    if (status == 1) {
	if ((strcmp(format, "HALF") == 0) || (strcmp(format, "BYTE") == 0)) {
	    strcpy(format, "HALF");
        } else {  
	    strcpy(format, "REAL");
	    short_int_data = 0;
            do_clamp = 0;
	}
    } else {
        zvmessage("Unable to determine input's data format", "");
        zabend();
    }
        

    // We don't do any radiometric correction 
    // for float images.

    if (!short_int_data) {
	int printed = 0;
        for (int cnt=0; cnt < nids; cnt++) {
	    if (radiometric[cnt] != NULL) {
		delete radiometric[cnt];
		radiometric[cnt] = NULL;
	        if (!printed) {
	  zvnprintf(256,
			"Cannot do radiometric correction for FLOAT images");
		    printed = 1;
		}
	    }
	}
    }

    // get parameter overrides if any
    proj_mode = Cylindrical;
    if (zvptst("POLAR"))
	proj_mode = Polar;
    if (zvptst("VERTICAL"))
	proj_mode = Vertical;
    if (zvptst("SINUSOIDAL"))
	proj_mode = Sinusoidal;

    int maxNB = 1;
    for (int n = 0; n < nids; n++) {
        maxNB = file_models[n]->getNB() > maxNB ?
                file_models[n]->getNB() : maxNB;
    }
    status=zvparm("BAND",&band,&count,&def,1,0);
    int band_count = 1;
    
    if (count == 0) {
        // no input band specified; process all bands
        band_count = maxNB;
    zvnprintf(256, "Number of bands to be processed is (%d)", band_count);
	band = 0;
    } else {
        // check if input band number is greater than number of bands in input
        if (band > maxNB) {
      zvnprintf(256,
                     "Input band (%d) is greater than number of bands in input image. Band set to 1.",
                     band);
             band = 1;
        }
    }

    // reads the BIAS values then fill unspecified ones
    zvparmd("BIAS", tile_bias_value, &count, &def, nids, 0);
    for (int i=count; i < nids; i++) {
	tile_bias_value[i] = 1.0;
    } 
    bias_ptr = NULL;
    if (count != 0)
	bias_ptr = tile_bias_value;

    // Reads the INPUT_RANGE parameter
    int range[2];
    min_input = 0;
    max_input = nids;
    zvparm("INPUT_RANGE", range, &count, &def, 2, 0);
    if (count >= 1)
	min_input = range[0] - 1;		// 1-based
    if (count >= 2)
	max_input = range[1];			// max is actually end+1
    if (min_input < 0)
	min_input = 0;
    if (max_input > nids)
	max_input = nids;

    // Check the INTERP parameter.
    do_interp = zvptst("INTERP");

    // Check the MODE parameter.  If it is INCREMENTAL, we read and write
    // each line as we go.  This was historically the only mode.  In MEMORY
    // mode, the entire mosaic is held in memory, and written once at the
    // end.  This is faster, but consumes MUCH more memory and does not
    // allow progress to be monitored (e.g. in xvd).  Note that for MPI
    // parallel processing, MEMORY mode is required.  OMP parallel processing
    // can be used with either mode.

    incremental_mode = zvptst("INCREMENTAL");

#ifdef ENABLE_MPI
    if (incremental_mode && mpi_numprocs > 1) {
	zvmessage("Incremental mode not compatible with MPI parallel processing.","");
	zvmessage("Changed to Memory mode.", "");
	incremental_mode = FALSE;
    }
#endif

    // Check for index and ICM outputs

    char idx_filename[PIG_MAX_FILENAME_SIZE];
    char icm_filename[PIG_MAX_FILENAME_SIZE];

    zvp("IDX_OUT", idx_filename, &count);
    int do_idx = (count != 0);
    zvp("ICM_OUT", icm_filename, &count);
    int do_icm = (count != 0);

    // Read the HORIZON parameter
    zvparmd("HORIZON", &horizon, &count, &def, 1, 0);
    use_horizon = (count != 0);
    horizon = PigDeg2Rad(horizon);

    // Read the RADIUS parameter
    zvparmd("RADIUS", &overlap_radius, &count, &def, 1, 0);
    overlap_radius_sq = overlap_radius * overlap_radius;

    // Note:  MAX_OPEN is the ultimate max, for allocation.  max_open is
    // the actual size of each pass.

    zvp("MAX_OPEN", &max_open, &count);
    if (count == 0)
	max_open = 0;			// set to default below
    if (max_open > MAX_OPEN)
	max_open = MAX_OPEN;		// no need for message
    if (max_open < 0)
	max_open = 0;

    int do_reverse = zvptst("REVERSE");

    // See if we're doing overlap brightness statistics

    zvp("OVR_OUT", ovr_filename, &count);
    do_ovr = (count != 0);
    if (max_open == 0) {
        if (do_ovr)
	    max_open = MAX_OPEN;	// everything must be in memory
	else
	    max_open = 100;		// default pass size (was 20)
    }
    do_ovr_normal = zvptst("NORMAL_OVR") || zvptst("BOTH_OVR");
    do_ovr_overall = zvptst("OVERALL_OVR") || zvptst("BOTH_OVR");
    if (!do_ovr) {		// turn them both off
	do_ovr_normal = FALSE;
	do_ovr_overall = FALSE;
    }
    do_ovr_hsi = zvptst("HSI");

#ifdef ENABLE_MPI
    if (do_ovr && mpi_numprocs > 1) {
	zvmessage("MPI parallel processing cannot be used with overlap processing", "");
	zabend();
    }
#endif

    if (do_ovr) {
	overlaps = new Overlap[MAX_OVERLAPS];
	if (overlaps == NULL) {
	    zvmessage("unable to allocate memory for Overlap array!!!", "");
	    zabend();
	}
	memset(overlaps, 0, sizeof(Overlap)*MAX_OVERLAPS);
	num_overlaps = 0;

	if (max_open < nids) {
	    zvmessage("WARNING: max_open is less than the number of inputs in overlap mode!", "");
	    zvmessage("Results may be undefined, as not all images will fit in memory.", "");
	    // keep going, who knows... this could be intentional, somehow...
	}
    }


//!!!!
#if 0
    zvp("GRID", grid, &count);
    igrid=grid[0];
    if(igrid < 0)igrid=-igrid;
    rgrid=igrid;
#endif
//!!!!

  if (do_print)
    zvnprintf(256, "Mosaic's surface model parameters are specified in the %s coordinate frame",
		surface_model->getCoordSystem()->getFrameName());

    // Report on coord system in use

    surface_model->setCoordSystem(proj_cs);

    azdir = proj_cs->getAzimuthDirection();
  if (do_print)
    zvnprintf(256, "Mosaic is projected in the %s coordinate frame",
		proj_cs->getFrameName());

    // Print out input status from labels

    if (do_print)
	mars_print_inputs(nids, pointing_in, camera_in, file_models,
		homogeneous_inputs, mission, instrument);

    if (proj_mode == Cylindrical || proj_mode == Polar || proj_mode == Sinusoidal) {
        // Determine the min and max azimuth and elevation.  This is only
	// approximate, but that's okay since it is only used to determine
	// the output size.   

        double wrap_az = 0.0;
        zvparmd("WRAP_AZ", &dbl_param, &count, &def, 1, 0);
        if (count > 0) {

            PigCoordSystem *wrap_cs = proj_cs;
	    char wrap_coord[256];

            wrap_az = PigDeg2Rad(dbl_param);

	    double wrap_el = 0.0;
            zvparmd("WRAP_EL", &dbl_param, &count, &def, 1, 0);
            wrap_el = PigDeg2Rad(dbl_param);

            zvp("WRAP_COORD", wrap_coord, &count);
            if (count != 0) {
                wrap_cs = m->getCoordSystem(file_models[0], wrap_coord);
	if (wrap_cs == NULL)
	  zvnabend(256, "Invalid WRAP_COORD: %s", wrap_coord);
	if (do_print)
	  zvnprintf(256, "Interpreting WRAP_COORD in the %s frame, with azimuth %f and elevation %f",
                        wrap_cs->getFrameName(), PigRad2Deg(wrap_az), PigRad2Deg(wrap_el));
            }

	    if (wrap_cs != proj_cs) {

		double wrap_az_new, wrap_el_new;
	if (do_print)
	  zvnprintf(256,"Frames are not the same. Converting wrap to %s",proj_cs->getFrameName());
      	
		proj_cs->convertAzEl(wrap_az, wrap_el, wrap_az_new, wrap_el_new, wrap_cs);
		wrap_az = wrap_az_new;
		wrap_el = wrap_el_new;
		
	    }
        } 

        mars_get_azel_minmax(nids, pointing_in, camera_in, file_models,
		         min_elev, max_elev, min_az, max_az, proj_cs, wrap_az);

    if (do_print)
      zvnprintf(256, "Elevation minimum %f, Elevation maximum %f",
	        PigRad2Deg(min_elev), PigRad2Deg(max_elev));
    if (do_print)
      zvnprintf(256, "Azimuth minimum %f, Azimuth maximum %f",
	        PigRad2Deg(min_az), PigRad2Deg(max_az));

        if (azdir == 1) {
            start_az = min_az;
            stop_az = max_az;
        } else {
            start_az = max_az;
            stop_az = min_az;
        }

        // compute output image limits
        zvparmd("LEFTAZ", &dbl_param, &count, &def, 1, 0);
        if (count > 0) {
            start_az = PigDeg2Rad(dbl_param);
      if (do_print)
	zvnprintf(256, "Override: Starting Azimuth (left) = %f",
	            PigRad2Deg(start_az));
        }
        zvparmd("RIGHTAZ", &dbl_param, &count, &def, 1, 0);
        if (count > 0) {
            stop_az = PigDeg2Rad(dbl_param);
      if (do_print)
	zvnprintf(256, "Override: Ending Azimuth (right) = %f",
	            PigRad2Deg(stop_az));
        }

        zvparmd("BOTTOMEL", &dbl_param, &count, &def, 1, 0);
        if (count > 0) {
	    min_elev = PigDeg2Rad(dbl_param);
            if (do_print)
	        zvnprintf(256, "Override: Elevation Minimum = %f",
	            PigRad2Deg(min_elev));
        }
	zvparmd("LIMIT_BOTTOMEL", &dbl_param, &count, &def, 1, 0);
	if (count > 0) {
	    double limit = PigDeg2Rad(dbl_param);
	    if (min_elev < limit) {
		min_elev = limit;
		if (do_print)
		    zvnprintf(256, "Elevation Minimum limited to %f",
			PigRad2Deg(min_elev));
	    }
	}

        zvparmd("TOPEL", &dbl_param, &count, &def, 1, 0);
        if (count > 0) {
	    max_elev = PigDeg2Rad(dbl_param);
            if (do_print)
	        zvnprintf(256, "Override: Elevation Maximum = %f",
	            PigRad2Deg(max_elev));
        }

	zvparmd("LIMIT_TOPEL", &dbl_param, &count, &def, 1, 0);
	if (count > 0) {
	    double limit = PigDeg2Rad(dbl_param);
	    if (max_elev > limit) {
		max_elev = limit;
		if (do_print)
		    zvnprintf(256, "Elevation Maximum limited to %f",
			PigRad2Deg(max_elev));
	    }
	}


        // Determine location of the center of projection.  This defaults
        // to the average of all the input camera locations, but may be
        // overridden.

        proj_origin.setXYZ(0.0, 0.0, 0.0);
        for (int i=0; i < nids; i++) {
	    proj_origin += pointing_in[i]->getCameraPosition(proj_cs);
        }
        proj_origin /= (double)nids;

    if (do_print)
      zvnprintf(256, "Projection origin = (%f, %f, %f)",
	        proj_origin.getX(), proj_origin.getY(), proj_origin.getZ());

        zvpcnt("PROJ_ORIGIN", &count);
        if (count == 3) {
            PigCoordSystem *po_cs = proj_cs;
	    char po_coord[256];
	    zvp("PO_COORD", po_coord, &count);
	    if (count != 0) {
    	        po_cs = m->getCoordSystem(file_models[0], po_coord);
	if (po_cs == NULL)
	  zvnabend(256, "Invalid PO_COORD: %s", po_coord);
	if (do_print)
	  zvnprintf(256, "Interpreting PROJ_ORIGIN in the %s frame",
    			po_cs->getFrameName());
	    }
	    double array[3];
	    zvparmd("PROJ_ORIGIN", array, &count, &def, 3, 0);
	    proj_origin.setXYZ(array);
	    proj_origin = proj_cs->convertPoint(proj_origin, po_cs);
      if (do_print)
	zvnprintf(256, "Override: Projection origin = (%f, %f, %f)",
	            proj_origin.getX(), proj_origin.getY(), proj_origin.getZ());
        }
    }

    // Determine the scale of the output by looking at the first input's
    // horizontal direction (the direction is arbitrary).

    scale = camera_in[0]->getPixelAngle(0);		// radians per pixel
  if (do_print)
    zvnprintf(256, "Pixel scale: %f radians/pixel or %f pixels/degree",
	    scale, 1.0/PigRad2Deg(scale));

    zvparmd("SCALE", &dbl_param, &count, &def, 1, 0);
    if (count > 0) {
	scale = PigDeg2Rad(1.0 / dbl_param);
    if (do_print)
      zvnprintf(256,
	        "Override: Pixel scale: %f radians/pixel or %f pixels/degree",
		scale, 1.0/PigRad2Deg(scale));
    }

    zvparmd("ZOOM", &dbl_param, &count, &def, 1, 0);
    if (count > 0) {
	scale /= dbl_param;
    if (do_print)
      zvnprintf(256, "Override: Pixel scale zoomed by %f", dbl_param);
    if (do_print)
      zvnprintf(256, "  to %f radians/pixel or %f pixels/degree",
		scale, 1.0/PigRad2Deg(scale));
    }

    // Calculate image size based on projection type.  Also calculates
    // line/samp of projection origins for Cylindrical, nadir locations
    // for Polar, and scale for Vertical.

    switch (proj_mode) {
	case Cylindrical:
	    if (do_print) zvmessage("Cylindrical Projection", "");

	    nlo = (int)((max_elev - min_elev) / scale);
	    if (azdir == 1) {				// CW, increasing az
		if (start_az < stop_az)
		    nso = (int)((stop_az - start_az) / scale);
		else
		    nso = (int)((stop_az + PigDeg2Rad(360.) - start_az) /scale);
	    }
	    else {					// CCW, decreasing az
		if (start_az > stop_az)
		    nso = (int)((start_az - stop_az) / scale);
		else
		    nso = (int)((start_az + PigDeg2Rad(360.) - stop_az) /scale);
	    }

	    az_first_sample = start_az;
    if (do_print)
      zvnprintf(256, "azimuth of first sample = %f",
						PigRad2Deg(az_first_sample));
	    line_zero_el = max_elev / scale;
    if (do_print)
      zvnprintf(256, "line of zero elevation = %f", line_zero_el);

	    break;

	case Polar:
	    if (do_print) zvmessage("Polar Projection", "");

	    // Start at nadir always, and it's circular
	    nlo = (int)(2.0 * (max_elev - PigDeg2Rad(-90.)) / scale);
	    if (nlo % 2 == 0)
		nlo++;		// make sure it's odd so we have a center pixel
	    nso = nlo;		// always square

	    nadir_line = nlo / 2;		// rounds down
	    nadir_samp = nso / 2;

	    // Get the azimuth to put up (normally 0) in the output.
	    // Defaults (in the PDF) to 0.0.  Other values have the effect
	    // of rotation the entire image so the given value is at the
	    // top of the mosaic.

	    zvparmd("UP_AZ", &up_azimuth, &count, &def, 1, 0);
	    up_azimuth = PigDeg2Rad(up_azimuth);
    if (do_print)
      zvnprintf(256,"Azimuth at top of mosaic = %f",PigRad2Deg(up_azimuth));

	    break;

	case Vertical:
	    if (do_print) zvmessage("Vertical Projection", "");

	    // There's no practical way to calculate this from the inputs
	    // so depend on the PDF default.
	    zvparmd("MAXX", &maxx, &count, &def, 1, 0);
	    zvparmd("MAXY", &maxy, &count, &def, 1, 0);
	    zvparmd("MINX", &minx, &count, &def, 1, 0);
	    if (count == 0) minx = -maxx;
	    zvparmd("MINY", &miny, &count, &def, 1, 0);
	    if (count == 0) miny = -maxy;
	    zvparmd("VERT_SCALE", &vert_scale, &count, &def, 1, 0);
	    vert_scale = 1.0 / vert_scale;	// m/pix -> pix/meter

    if (do_print)
      zvnprintf(256, "Min X = %f, Min Y = %f", minx,miny);
    if (do_print)
      zvnprintf(256, "Max X = %f, Max Y = %f", maxx,maxy);
    if (do_print)
      zvnprintf(256, "Scale = %f meters/pixel", 1.0 / vert_scale);

	    zvparmd("ZOOM", &dbl_param, &count, &def, 1, 0);
	    if (count > 0) {
		vert_scale *= dbl_param;
      if (do_print)
	zvnprintf(256, "Override: Scale zoomed by %f to %f meters/pixel",
			dbl_param, 1.0 / vert_scale);
	    }

	    nlo = (int)((maxx - minx) * vert_scale);
	    nso = (int)((maxy - miny) * vert_scale);
 
	    break;

	case Sinusoidal:
	    if (do_print) zvmessage("Sinusoidal Projection", "");

	    nlo = (int)((max_elev - min_elev) / scale);
	    if (azdir == 1) {				// CW, increasing az
		if (start_az < stop_az)
		    nso = (int)((stop_az - start_az) / scale);
		else
		    nso = (int)((stop_az + PigDeg2Rad(360.) - start_az) /scale);
	    }
	    else {					// CCW, decreasing az
		if (start_az > stop_az)
		    nso = (int)((start_az - stop_az) / scale);
		else
		    nso = (int)((start_az + PigDeg2Rad(360.) - stop_az) /scale);
	    }
 
	    az_first_sample = start_az;
    if (do_print)
      zvnprintf(256, "azimuth of first sample = %f",
						PigRad2Deg(az_first_sample));
	    zvparmd("CENTER_EL", &center_el, &count, &def, 1, 0);
	    center_el = PigDeg2Rad(center_el);
	    // Center of projection is the middle by default
	    center_az = start_az + nso*scale/2.0;
    if (do_print)
      zvnprintf(256, "Center azimuth = %f, center elevation = %lf",
		PigRad2Deg(center_az), PigRad2Deg(center_el));

	    double temp;
	    zvparmd("CENTER_AZ", &temp, &count, &def, 1, 0);
	    if (count != 0) {
      if (do_print)
	zvnprintf(256, "Center azimuth OVERRIDE: %f\n", temp);
		center_az = PigDeg2Rad(temp);
	    }

	    line_zero_el = (max_elev - center_el) / scale;
    if (do_print)
      zvnprintf(256, "line of center elevation = %f", line_zero_el);

	    break;

	default:
	    zvmessage("Internal error: bad proj_mode!", "");
	    zabend();
    }

    // If start or stop azimuth fall outside 0-360 move them into the range
    if(az_first_sample < 0.0)
        az_first_sample += PigDeg2Rad(360.0);
    if(az_first_sample > PigDeg2Rad(360.0))
        az_first_sample -= PigDeg2Rad(360.0);
    if(stop_az < 0.0)
        stop_az += PigDeg2Rad(360.0);
    if(stop_az > PigDeg2Rad(360.0))
        stop_az -= PigDeg2Rad(360.0);

    // If start and stop azimuth equal 0, set start/stop to 0/360 respectively
    if(az_first_sample == 0.0)
        if(stop_az == 0.0){
        	az_first_sample = PigDeg2Rad(0.0);
                stop_az = PigDeg2Rad(360.0);
	}

  if (do_print)
    zvnprintf(256, "Output lines = %d, samples = %d, bands = %d", nlo, nso, band_count);

    //!!!! Do we need size override?  Or is T/B/L/R az/el sufficient?

    if (nso > MAX_OBUF) {
	zvmessage("Output buffer too short for picture width.", "");
	zabend();
    }

    if ((nlo > MAX_OBUF) || (nlo < 1) || (nso < 1)) {
	zvmessage("Unreasonable output file dimensions", "");
	zabend();
    }


    int linestart = 0;
    int lineend = nlo;
    int maxdatalen = -32768;

    if (mpi_id == 0) {
	// make sure that the master CPU has as much memory assigned as the
        // largest slave CPU (in the current set-up that is the last CPU).
	// This way we do not have to check on the block sizes later in
	// the data transmittal phase.

	// For incremental mode, only one line's worth of data is allocated.
	// We know in that case we're not parallel.

	// assign memory for the largest data element

        get_linestart_end(&linestart, &lineend, nlo, mpi_numprocs-1);
	if (incremental_mode) {
            for (int b = 0; b < band_count; b++) {
	        obuf2d_f[b] = mve_Fmatrix(1, nso);
            }
	    if (do_idx)
		obuf_idx = mve_Simatrix(1,nso);
	    if (do_icm) {
		obuf_icm_l = mve_Fmatrix(1,nso);
		obuf_icm_s = mve_Fmatrix(1,nso);
	    }
	} else {
            for (int b = 0; b < band_count; b++) {
		obuf2d_f[b] = mve_Fmatrix(lineend - linestart, nso);
            }
	    if (do_idx)
		obuf_idx = mve_Simatrix(lineend-linestart, nso);
	    if (do_icm) {
	        obuf_icm_l = mve_Fmatrix(lineend-linestart, nso);
		obuf_icm_s = mve_Fmatrix(lineend-linestart, nso);
	    }
	}
	
	maxdatalen = (lineend-linestart)*nso;
	
	// operate only on the required data size
	
	get_linestart_end(&linestart, &lineend, nlo, mpi_id);
    } else {
	get_linestart_end(&linestart, &lineend, nlo, mpi_id);
	for (int b = 0; b < band_count; b++) {
	    obuf2d_f[b] = mve_Fmatrix(lineend - linestart, nso);
        }
	if (do_idx)
	    obuf_idx = mve_Simatrix(lineend-linestart, nso);
	if (do_icm) {
	    obuf_icm_l = mve_Fmatrix(lineend-linestart, nso);
	    obuf_icm_s = mve_Fmatrix(lineend-linestart, nso);
	}
    }
    
  DPR(zvnprintf(256, "CPU %d starting with lines %d to %d\n",mpi_id, linestart,lineend));
    
    // Set up projection parameters structure.  Used for label writing and
    // footprints.
    
    proj.camera_in = camera_in;
    proj.surface_model = surface_model;
    proj.proj_mode = proj_mode;
    proj.scale = scale;
    proj.proj_origin = proj_origin;
    proj.line_zero_el = line_zero_el;
    proj.az_first_sample = az_first_sample;
    proj.az_last_sample = max_az;
    proj.up_azimuth = up_azimuth;
    proj.nadir_line = nadir_line;
    proj.nadir_samp = nadir_samp;
    proj.nlo = nlo;
    proj.nso = nso;
    proj.nbo = band_count;
    proj.vert_scale = vert_scale;
    proj.minx = minx;
    proj.miny = miny;
    proj.maxx = maxx;
    proj.maxy = maxy;
    proj.center_el = center_el;
    proj.center_az = center_az;
    proj.min_elev = min_elev;
    proj.max_elev = max_elev;
    proj.cs = proj_cs;
    proj.azdir = azdir;

    // Create the output file

    // PARALLEL NOTES:
    // In incremental mode, the file must be opened here and filled with 0's,
    // because each pass stores its data in the file.  In memory mode
    // (required for parallel processing), the entire mosaic (or the
    // slice the node is working on, for parallel runs) is held in memory.
    // Thus, there is no need to open the output file until we're ready to
    // write, and no need to fill it with 0's.  However, for consistency,
    // we at least open the file here anyway.  Note that we still need UPDATE
    // mode for writing grids and such (and possibly writing slave results
    // in random order).

    // Only the master id writes output to disk.

    if (mpi_id == 0) {			// master node
	zvselpi(0);			// don't transfer any labels
	zvunit(&unit_out, "OUT", 1, NULL);
	zvopen(unit_out, "OP", "WRITE", "U_FORMAT", format,
               "U_NS", nso, "U_NL", nlo, "OPEN_ACT", "AS",
               "U_NB", band_count, "U_ORG", "BSQ", "O_FORMAT", format, NULL);

	// Open the idx/icm files

	if (do_idx) {
	    zvunit(&unit_idx_out, "IDX", 1, "U_NAME", idx_filename, NULL);
	    zvopen(unit_idx_out, "OP", "WRITE", "U_FORMAT", "HALF",
		"U_NS", nso, "U_NL", nlo, "OPEN_ACT", "AS",
		"U_NB", 1, "U_ORG", "BSQ", "O_FORMAT", "HALF", NULL);
	}
	if (do_icm) {
	    zvunit(&unit_icm_out, "ICM", 1, "U_NAME", icm_filename, NULL);
	    zvopen(unit_icm_out, "OP", "WRITE", "U_FORMAT", "REAL",
		"U_NS", nso, "U_NL", nlo, "OPEN_ACT", "AS",
		"U_NB", 2, "U_ORG", "BSQ", "O_FORMAT", "REAL", NULL);
	}

	// Write the output labels

	PigLabelModel *labelModel = m->createLabelModel(unit_out);
	PigLabelModel *idxLabel = NULL;
	PigLabelModel *icmLabel = NULL;
	if (do_idx) idxLabel = m->createLabelModel(unit_idx_out);
	if (do_icm) icmLabel = m->createLabelModel(unit_icm_out);

	// This is absolutely horrid due to the IDX and ICM files.  We
	// really need a copy-label-model or some such if possible.
	// This is done as a macro simply so we don't have tons and tons
	// of arguments to a small subroutine.


#define SET_LABEL(model) \
	switch (proj_mode) {						\
	  case Cylindrical:						\
	    (model)->setMapCyl(file_models,radiometric, brt_corr,	\
				  bias_ptr, NULL, nids, 		\
				  surface_model,			\
				  scale, proj_origin, line_zero_el, 	\
				  az_first_sample, stop_az, 		\
				  min_elev, max_elev, proj_cs);		\
	    break;							\
	  case Polar:							\
	    (model)->setMapPolar(file_models, radiometric, brt_corr,	\
				    bias_ptr, NULL, nids, 		\
				    surface_model,			\
				    scale, proj_origin, up_azimuth, 	\
				    nadir_line, nadir_samp, max_elev, proj_cs);\
	    break;							\
	  case Vertical:						\
	    (model)->setMapVert(file_models, radiometric, brt_corr,	\
				   bias_ptr, NULL, nids,		\
				   surface_model,			\
				   nlo, nso, vert_scale,		\
				   maxx, maxy, proj_cs);		\
	    break;							\
	  case Sinusoidal:						\
	    (model)->setMapSin(file_models,radiometric, brt_corr,	\
				  bias_ptr, NULL, nids, 		\
				  surface_model,			\
				  scale, proj_origin, line_zero_el, 	\
				  az_first_sample, stop_az, 		\
				  min_elev, max_elev, center_az, center_el, \
				  proj_cs);				\
	    break;							\
	  default:							\
	    zvmessage("Internal error: bad proj_mode!!!!", "");		\
	    zabend();							\
	}
//!!!! VERTICAL MODE needs to set min in the label too!!!!

	SET_LABEL(labelModel);
	zvplabel(unit_out, 0, 1);

	if (do_idx) {
	    SET_LABEL(idxLabel);
	    idxLabel->setDerivedImageType("IDX_MAP");
	    zvplabel(unit_idx_out, 0, 1);
	}
	if (do_icm) {
	    SET_LABEL(icmLabel);
	    icmLabel->setDerivedImageType("ICM_MAP");
	    zvplabel(unit_icm_out, 0, 1);
	}

	// write output as zero's in incremental mode only

	if (incremental_mode) {
            for (int b = 0; b < band_count; b++) {
	        for (int j = 0; j < nlo; j++)
		    zvwrit(unit_out, obuf2d_f[b][0], "LINE", j+1,
                            "BAND", b+1, NULL);
            }
	    
	    if (do_idx) {
		for (int j=0; j<nlo; j++)
		    zvwrit(unit_idx_out, obuf_idx[0], "LINE", j+1, NULL);
	    }
	    if (do_icm) {
		for (int j=0; j<nlo; j++) {
		    zvwrit(unit_icm_out, obuf_icm_l[0], "LINE", j+1, "BAND", 1,
								NULL);
		    zvwrit(unit_icm_out, obuf_icm_s[0], "LINE", j+1, "BAND", 2,
								NULL);
		}
	    }
	}
	
	// reopen output for update

	zvclose(unit_out, NULL);
	zvopen(unit_out, "OP", "UPDATE", "U_FORMAT", "REAL",
               "U_NS", nso, "U_NL", nlo, "OPEN_ACT", "AS",
               "U_NB", band_count, "U_ORG", "BSQ", "O_FORMAT", format, NULL);
	if (do_idx) {
	    zvclose(unit_idx_out, NULL);
	    zvopen(unit_idx_out, "OP", "UPDATE", "U_FORMAT", "HALF",
		"U_NS", nso, "U_NL", nlo, "OPEN_ACT", "AS",
		"U_NB", 1, "U_ORG", "BSQ", "O_FORMAT", "HALF", NULL);
	}
	if (do_icm) {
	    zvclose(unit_icm_out, NULL);
	    zvopen(unit_icm_out, "OP", "UPDATE", "U_FORMAT", "REAL",
		"U_NS", nso, "U_NL", nlo, "OPEN_ACT", "AS",
		"U_NB", 2, "U_ORG", "BSQ", "O_FORMAT", "REAL", NULL);
	}
    }
    
    // Calculate the approximate Field Of View for each input, for use in
    // a quick decision whether to project the point into this image or not.
    // Adding H and V FOV's covers the diagonals, plus provides some slop.
    // We divide by 2 because the H and V FOV's cover from edge to edge, while
    // we need only from the center to the edge.  The cosine is stored for
    // easy comparison with a dot product.  We limit the FOV to 90 degrees
    // (180 total) (really 89.4) to avoid any back-projection (MER hazcam H+V
    // is > 90).  But we make sure we have at least 30 degrees to accomodate
    // very tiny subframes.  These limits are adjustable via the FOV_LIMIT
    // parameter (which is in degrees).
    // We also compute and save the camera position and orientation here
    // once, rather than rederiving it for each pixel.

    double fov_limit[2];
    zvparmd("FOV_LIMIT", &fov_limit, &count, &def, 2, 0);
    if (count == 2) {
	fov_limit[0] = cos(PigDeg2Rad(fov_limit[0]));
	fov_limit[1] = cos(PigDeg2Rad(fov_limit[1]));
    } else {
        fov_limit[0] = 0.01;	// 89.4 degrees; really 90 is an infinity case
        fov_limit[1] = 0.866;	// 30 degrees
    }
    zvnprintf(256, "FOV limits set to %f, %f degrees",
		PigRad2Deg(acos(fov_limit[0])), PigRad2Deg(acos(fov_limit[1])));

    for (int i=0; i < nids; i++) {
	FOV[i] = cos((file_models[i]->getFOV(camera_in[i], 0) +
		      file_models[i]->getFOV(camera_in[i], 1))/2);
	if (FOV[i] < fov_limit[0])
	    FOV[i] = fov_limit[0];		// limit on the large side
	if (FOV[i] > fov_limit[1])
	    FOV[i] = fov_limit[1];		// limit on the small side

	// We used to call pointing_in[i]->getCameraOrientation(proj_cs) here.
	// However, that's just a fancy wrapper around returning the A vector,
	// which is okay in most cases but horrible for e.g. M20 RMI, where
	// the A vector is a good 30+ degrees away from the true pointing
	// vector.  So we do what we should have done since the beginning...
	// project the central pixel out to get a vector.  While we're at it,
	// we might as well use the returned position instead of
	// pointing_in[i]->getCameraPosition(proj_cs).  Although there's
	// nothing known to be wrong with the position, it just feel cleaner
	// this way.

	double ll = (file_models[i]->getNL()-0.5) / 2 + 0.5;
	double ss = (file_models[i]->getNS()-0.5) / 2 + 0.5;
	PigVector look;
	PigPoint origin;
	camera_in[i]->LStoLookVector(ll, ss, origin, look, proj_cs);

	camera_position[i] = origin;
	camera_orientation[i] = look;
    }
    
    // Used for vertical projection.  up and down are really interchangeable,
    // but we set them right for consistency.

    PigVector down(0.0, 0.0, -1.0), up(0.0, 0.0, 1.0);
    down = down * proj_cs->getElevationDirection();
    up = up * proj_cs->getElevationDirection();

    //Create and initialize variables necessary for pass loop
    int first_input, last_input;

    int omp_on = zvptst("OMP_ON");

    TIME_EVAL(setup);
    
    // Make a number of passes, processing max_open inputs each pass
    for (first_input = min_input; first_input < max_input;
	 first_input += max_open) {
	TIME_START(read);
	
	if (mpi_numprocs > 1)
	zvnprintf(256, "Pass %d of %d for CPU %d", (first_input/max_open)+1,
		    ((max_input-1)/max_open)+1, mpi_id);
	else
	zvnprintf(256, "Pass %d of %d", (first_input/max_open)+1,
		    ((max_input-1)/max_open)+1);
	
	last_input = first_input + max_open-1;
	if (last_input >= max_input)
	    last_input = max_input-1;
	
	// Read a set of inputs into memory

	if (do_ovr_hsi) {
            mars_read_inputs_hsi(first_input, last_input, file_models, f_imgs, 
                                 MAX_NL, MAX_NS, 3, radiometric, brt_corr);
        } else {
            mars_read_inputs(first_input, last_input, file_models, f_imgs, 
                             MAX_NL, MAX_NS, band, radiometric, brt_corr);
        }

	// If doing overall overlaps, compute and save the stats

	if (do_ovr_overall) {
	    overall_overlaps = new Overlap[MAX_INPUTS];
	    if (overall_overlaps == NULL) {
	        zvmessage("unable to allocate memory for overall Overlap array!!!", "");
	        zabend();
	    }
	    memset(overall_overlaps, 0, sizeof(Overlap)*MAX_INPUTS);

  	    for (int k = first_input; k <= last_input; k++) {
		int kp = k-first_input;
	        overall_overlaps[k].type = OVERLAP_OVERALL;
		if (do_ovr_hsi)
		    overall_overlaps[k].type = OVERLAP_OVERALL_HSI;
		overall_overlaps[k].active = TRUE;
		overall_overlaps[k].n_images = 1;
		overall_overlaps[k].radius = 0.0;
		overall_overlaps[k].error = 0.0;
		overall_overlaps[k].stats[0].image = k;
		overall_overlaps[k].stats[0].line = 0.0;
		overall_overlaps[k].stats[0].samp = 0.0;

		// Messy... should really have a get phys limits in filemodel
		double sl, ss, el, es;
		file_models[k]->getImageBorders(sl, ss, el, es);
		sl -= file_models[k]->getYOffset();
		if (sl < 0) sl = 0;
		el -= file_models[k]->getYOffset();
		if (el >= file_models[k]->getNL())
		    el = file_models[k]->getNL() - 1;
		ss -= file_models[k]->getXOffset();
		if (ss < 0) ss = 0;
		es -= file_models[k]->getXOffset();
		if (es >= file_models[k]->getNS())
		    es = file_models[k]->getNS() - 1;

		double sum = 0.0;
		double sumsq = 0.0;
		int npix = 0;
		double dn = 0.0;
		for (int lll = (int)(sl+0.5); lll < (int)(el+0.5); lll++) {
		    for (int sss = (int)(ss+0.5); sss < (int)(es+0.5); sss++) {
			dn = f_imgs[kp]->get(lll, sss);
			sum += dn;
			sumsq += dn*dn;
			npix++;
		    }
		}
		if (npix == 0) npix = 1;		// whoops!
		double m = sum / npix;
		overall_overlaps[k].stats[0].mean = m;
		double diff = sumsq/npix - m*m;
		if (diff > 0.0)
		    overall_overlaps[k].stats[0].stdev = sqrt(diff);
		else
		    overall_overlaps[k].stats[0].stdev = 0.0;
		overall_overlaps[k].n_pixels = npix;
	    }
	}

	TIME_EVAL_START(read,image);
	
	// Loop through each sample of the output, and pick up the input
	// pixel that corresponds to it
	
	for (int j=linestart; j<lineend; j++) {			// line loop
	    if (j % 100 == 0) {
		if (mpi_numprocs > 1)
	    zvnprintf(256, "line %d, CPU %d", j, mpi_id);
		else
	    zvnprintf(256, "line %d", j);
	    }

            if (incremental_mode) {
                for (int b = 0; b < band_count; b++) {
                    obuf2d_f_ptr[b] = obuf2d_f[b][0];
                }
                if (do_idx)
                    obuf_idx_ptr = obuf_idx[0];
                if (do_icm) {
                    obuf_icm_l_ptr = obuf_icm_l[0];
                    obuf_icm_s_ptr = obuf_icm_s[0];
                }
            } else {
                for (int b = 0; b < band_count; b++) {
                    obuf2d_f_ptr[b] = obuf2d_f[b][j - linestart];
                }
                if (do_idx)
                    obuf_idx_ptr = obuf_idx[j-linestart];
                if (do_icm) {
                    obuf_icm_l_ptr = obuf_icm_l[j-linestart];
                    obuf_icm_s_ptr = obuf_icm_s[j-linestart];
                }
            }

	    if (incremental_mode) {			// re-read output line
	        for (int b = 0; b < band_count; b++) {
		    zvread(unit_out, obuf2d_f[b][0], "LINE", j+1, "BAND", b+1, 
                           NULL);
                }
	        if (do_idx) {
		    zvread(unit_idx_out, obuf_idx[0], "LINE", j+1, NULL);
		}
	        if (do_icm) {
		    zvread(unit_icm_out, obuf_icm_l[0], "LINE", j+1, "BAND", 1,
								NULL);
		    zvread(unit_icm_out, obuf_icm_s[0], "LINE", j+1, "BAND", 2,
								NULL);
		}
           }

// Tests show that guided,16 is (marginally) the best mechanism.  We want
// some kind of dynamic scheduling because there is extreme pixel to pixel
// variability (if the pixel is already done, it's super fast).  Dynamic,1
// was noticeably slower, likely due to a combination of too much thread
// overhead and cache-line hits from threads too close together in the arrays.
// Dynamic,16 and above was decent, but it looked like it was starting to get
// slow again at 256 (for all three schedule methods, actually).  Probably
// the pixel to pixel variability bites us at 256.  Anyway, guided is a form
// of dynamic that seems to lend itself well to this scenario.

#pragma omp parallel for schedule(guided,16) if (omp_on) 
		for (int i = 0; i < nso; i++) {		// sample loop
                    int continue_flag = 0;
                    for (int b = 0; b < band_count; b++) {
		        if (obuf2d_f_ptr[b][i] != 0.0) {
		            // already set, do next iteration
		            continue_flag = 1;
                            break;
                        }
		    }
		    // If we're in reverse mode, don't skip stuff
                    if (continue_flag && !do_reverse)
                        continue;
		
	        // Compute azimuth and elevation for this pixel, and project
	        // into space and then XYZ.
		
		PigVector look;
		PigPoint surf_pt, proj_pt;
		double out_az, out_el;
		double x_ctr, y_ctr;
		PigVector polar;
		int infinity=0, hits;
		
		switch (proj_mode) {
		    case Cylindrical:
			out_az = (azdir * i * scale) + az_first_sample;
			out_el = (line_zero_el - j) * scale;

			if (use_horizon && out_el > horizon)
			    continue;	// ignore sky for brightness overlaps

			look = proj_cs->constructVector(out_az, out_el);
			hits = surface_model->intersectRay(proj_origin,
							   look, surf_pt);
			infinity = (hits <= 0);
			break;
			
		    case Polar:
			x_ctr = i - nadir_samp;
			y_ctr = nadir_line - j;
			polar.setXYZ(x_ctr, y_ctr, 0.0);
			out_el = polar.getRange() * scale - PigDeg2Rad(90.0);
			// PigVector az is CCW from +X, we want CW/CCW from +Y
			out_az = up_azimuth + 
				(PigDeg2Rad(90.0) - polar.getAz()) * azdir;

			if (use_horizon && out_el > horizon)
			    continue;	// ignore sky for brightness overlaps

			look = proj_cs->constructVector(out_az, out_el);
			hits = surface_model->intersectRay(proj_origin,
							look, surf_pt);
			infinity = (hits <= 0);
			break;

		    case Vertical:
			x_ctr = maxx - (j / vert_scale);	// +X is up
			if (azdir > 0)
			    y_ctr = (i / vert_scale) + miny;	// +Y is right
			else
			    y_ctr = maxy - (j / vert_scale);	// +Y is left
			// +Y is right for azdir=1, left for -1
			proj_pt.setXYZ(x_ctr, y_ctr, 0.0);
			
			// Project this point to the actual surface, vertically
			// We take the first intersection (if more than one)
			// since it is likely the surface we want is close to
			// z == 0.
			hits = surface_model->intersectRay(proj_pt, down, 1,
							   surf_pt);
			if (hits <= 0)
			    hits = surface_model->intersectRay(proj_pt, up, 1,
							       surf_pt);
			infinity = (hits <= 0);

			// Infinity case is undefined for vertical
			// (there's no "look" to fall back on)

			if (infinity)
			    continue;
			break;
			
		    case Sinusoidal:
			out_el = (line_zero_el - j) * scale;
			out_az = (azdir * i * scale) + az_first_sample -
						center_az;
			if (center_az < az_first_sample)
			    out_az += PigDeg2Rad(360.0);
			out_az /= cos(out_el - center_el);
			if (out_az < - PigDeg2Rad(180.0) ||
			    out_az > PigDeg2Rad(180.0))
			    continue;		// don't wrap more than once
			out_az += center_az;

			if (use_horizon && out_el > horizon)
			    continue;	// ignore sky for brightness overlaps

			look = proj_cs->constructVector(out_az, out_el);
			hits = surface_model->intersectRay(proj_origin,
							   look, surf_pt);
			infinity = (hits <= 0);
			break;
			
		    default:
			zvmessage("Internal error: bad proj_mode!!", "");
			zabend();
		}

		int overlap_num_images = 0; // Number of images in this overlap
		int overlap_image_num[MAX_OPEN];//which imgs are in this overlap
		double overlap_dn[MAX_OPEN]; // DN for each img in this overlap
		double overlap_line[MAX_OPEN];	// line number for each image
		double overlap_samp[MAX_OPEN];	// samp number for each image

  		for (int k = first_input; k <= last_input; k++) { // img loop

		    int kp = k-first_input;
		    
		    // skip picture if this point is outside of the FOV of the
		    // input picture.  To handle cameras that move, we take the
		    // XYZ point - the camera center, then dot that with the
		    // camera look vector to get the angle between the camera
		    // center and the point.  If we're at infinity, we can
		    // simply check the point's look vector.
		    //
		    // One might question the computational efficiency of this
		    // rather than just projecting the pixel, but something 
		    // like it is necessary to keep the point from projecting
		    // "backwards" into a camera.

		    if (infinity) {
			if ((look % camera_orientation[k]) < FOV[k])
			    continue;
		    }
		    else {
			PigVector new_look = surf_pt - camera_position[k];
			new_look.normalize();
			if ((new_look % camera_orientation[k]) < FOV[k])
			    continue;
		    }
		    
		    // Convert output XYZ point into camera coordinates
		    // in the input image
		    
		    double in_line, in_samp;
		    camera_in[k]->XYZtoLS(surf_pt, infinity, 
					  &in_line, &in_samp,
					  proj_cs);
		    
		    // Check if point is within the input image
		    
		    if (file_models[k]->testPixelLocation(in_line,in_samp) != 0)
			continue;			// skip if not
		    
		    // Convert to phys image coords (compensate for sub-areas)
		    double image_line = in_line - file_models[k]->getYOffset();
		    double image_samp = in_samp - file_models[k]->getXOffset();
		    
		    
		    double dn = 0.0;
		    
		    if (do_interp) {
			// interpolate in the input image 
			// (bilinear interpolation)
			
			int m = (int) image_samp;
			int n = (int) image_line;
			
			double wr = image_samp - m;
			double wl = 1.0 - wr;
			
			double wb = image_line - n;
			
			double top, bot;
			
                        SimpleImage<float> *img = f_imgs[kp];
                        int bb;
                        for (int b = 0; b < band_count; b++) {
                            bb = b;
                            if (bb >= img->getNB())
                                bb = img->getNB() - 1;
			    double ul = img->get(bb,n,m);
			    double ur = img->get(bb,n,m+1);
			    double ll = img->get(bb,n+1,m);
			    double lr = img->get(bb,n+1,m+1);
                            if (ul == 0.0) {
			        if (ur != 0.0) ul = ur;
			        else if (ll != 0.0) ul = ll;
				else ul = lr;
			    }
                            // ul is now known to be non-0 if possible
			    
                            // Slightly sub-optimal if ur was originally 0
			    // (in which case we should take lr) but that's
			    // not worth dealing with.
			    if (ur == 0.0) ur = ul;
			    if (ll == 0.0) ll = ul;
			    if (lr == 0.0) lr = ur;
			    top = wl * ul + wr * ur;
			    bot = wl * ll + wr * lr;
			    
			    dn = (bot * wb + top * (1.0 - wb))
                                        * tile_bias_value[k];

                            if (do_clamp) {
				// If we have short int data but it's nonzero we
				// really want to avoid 0 in the output as it
				// gets weirdly transparent... unless the input
				// was actually 0
				if (short_int_data && dn < 1.0 && dn != 0.0)
				    dn = 1.0;
                                if (dn <= 0.0) dn = 0.0;
                                if (dn > 32766.0) dn = 32766.0;
                            }
			    obuf2d_f_ptr[b][i] = dn;
                        }
		    } else {			// Don't interpolate
                        SimpleImage<float> *img = f_imgs[kp];
			int bb;
                        for (int b = 0; b < band_count; b++) {
                            bb = b;
                            if (bb >= img->getNB())
                                bb = img->getNB() - 1;
                            dn = img->get(bb, (int)(image_line + 0.5),
				    (int)(image_samp + 0.5))
                                    * tile_bias_value[k];

                            if (do_clamp) {
				// If we have short int data but it's nonzero we
				// really want to avoid 0 in the output as it
				// gets weirdly transparent... unless the input
				// was actually 0
				if (short_int_data && dn < 1.0 && dn != 0.0)
				    dn = 1.0;
                                if (dn <= 0.0) dn = 0.0;
                                if (dn > 32766.0) dn = 32766.0;
                            }
			    obuf2d_f_ptr[b][i] = dn;
                        }
		    }

		    // Save index and ICM data

		    if (do_idx)
			obuf_idx_ptr[i] = k+1;		// 1-based index
		    if (do_icm) {
			obuf_icm_l_ptr[i] = image_line + 1;	// 1-based
			obuf_icm_s_ptr[i] = image_samp + 1;	// 1-based
		    }

		    if (do_ovr_normal) {	// Save ovr info
			if (dn > 0.0) {
			    if (overlap_num_images >= MAX_OVERLAP_IMAGES) {
				zvmessage("***ERROR*** overlap_num_images >= MAX_OPEN.  Results may be incorrect","");	// really shouldn't happen
				continue;
			    }
			    overlap_image_num[overlap_num_images] = k;
			    overlap_dn[overlap_num_images] = dn;
			    overlap_line[overlap_num_images] = image_line;
			    overlap_samp[overlap_num_images] = image_samp;
			    overlap_num_images++;
			}
		    } else {	// No skipping if ovr is on, or reverse
			if (!do_reverse) {
                            int break_flag = 0;
                            for (int b = 0; b < band_count; b++) {
		                if (obuf2d_f_ptr[b][i] != 0.0) {
			            // skip the rest of the pictures
			            break_flag = 1;
                                    break;
                                }
                            }
                            if (break_flag)
                                break;    // skip the rest of the pictures
			}
		    }
		    
		}			// picture loop

		// Brightness overlaps.  Ignore if there is only one.

		if (do_ovr_normal && overlap_num_images > 1) {

		    // This is a critical section because overlaps[] is global
		    // and not indexed by the thread loop variable (i).  So
		    // we make sure only one thread is messing with it
		    // at a time.  Do not think it's worth trying to get
		    // finer grained in here with the critical section.
		    // Note that the output can vary slightly depending on
		    // which thread executes first: the first pixel in a new
		    // overlap area defines the area so if a different thread
		    // is first then the areas might be slightly different.
		    // But the count of pixels in each image tuple should
		    // be the same; merely their distribution to overlaps
		    // varies.

#pragma omp critical (overlaps)
		  {
		    int ii, jj;

		    // Look for another occurrence of this set of images
		    // that is also within the given radius for the first
		    // in the set

		    for (ii=0; ii < num_overlaps; ii++) {
			if (overlaps[ii].n_images == overlap_num_images) {
			    int match = TRUE;
			    for (jj=0; jj < overlap_num_images; jj++) {
				if (overlaps[ii].stats[jj].image !=
							overlap_image_num[jj]) {
				    match = FALSE;
				    break;
				}
			    }
			    if (match && overlap_radius != 0.0) {  // chk radius
				// A little messy.  Radius applies to the first
				// entry in the overlap set.  So get that from
				// the match, then find that same image in our
				// new overlaps
				int chk_img = overlaps[ii].stats[0].image;
				int ovr_img = -1;
				for (int kk=0; kk<overlap_num_images; kk++) {
				    if (overlap_image_num[kk] == chk_img) {
					ovr_img = kk;
					break;
				    }
				}
				if (ovr_img == -1)
				    match = FALSE;	// whoops
				else {
				    // found matching image, so check distance
				    double diffy = overlap_line[ovr_img] -
						overlaps[ii].stats[0].line;
				    double diffx = overlap_samp[ovr_img] -
						overlaps[ii].stats[0].samp;
				    if (diffy*diffy + diffx*diffx >
						overlap_radius_sq)
					match = FALSE;	// not in the radius
				}
			    }

			    if (match) {
				overlaps[ii].n_pixels++;
				for (jj=0; jj < overlap_num_images; jj++) {
				    overlaps[ii].stats[jj].sum_dn +=
								overlap_dn[jj];
				    overlaps[ii].stats[jj].sum2_dn +=
						overlap_dn[jj] * overlap_dn[jj];
				}
				overlap_num_images = 0;		// done with it
				break;		// out of ii loop
			    }
			}
		    }

		    // Not found, make new overlap

		    if (overlap_num_images > 1) {
			if (num_overlaps >= MAX_OVERLAPS) {
			    zvmessage("***ERROR*** Too many overlaps!!!  Overlap ignored","");
			} else {
			    overlaps[num_overlaps].type = OVERLAP_MEAN_STDEV;
			    if (do_ovr_hsi)
			        overlaps[num_overlaps].type = OVERLAP_HSI;
			    overlaps[num_overlaps].active = TRUE;
			    overlaps[num_overlaps].n_images =overlap_num_images;
			    overlaps[num_overlaps].n_pixels = 1;
			    overlaps[num_overlaps].radius = overlap_radius;
			    for (jj=0; jj < overlap_num_images; jj++) {
			        overlaps[num_overlaps].stats[jj].image =
							overlap_image_num[jj];
			        overlaps[num_overlaps].stats[jj].sum_dn =
								overlap_dn[jj];
			        overlaps[num_overlaps].stats[jj].sum2_dn =
						overlap_dn[jj] * overlap_dn[jj];
			        overlaps[num_overlaps].stats[jj].line =
						overlap_line[jj];
			        overlaps[num_overlaps].stats[jj].samp =
						overlap_samp[jj];
			    }
			    num_overlaps++;
			}
		    }
		  }		// end of overlap critical section
		}		// end ovr stuff
	    }				// sample loop
	    
	    // Out of omp parallel section now

	    if (incremental_mode) {
                for (int b = 0; b < band_count; b++) {
		    zvwrit(unit_out, obuf2d_f_ptr[b], "LINE", j+1,
                            "BAND", b+1, NULL);
                }
		if (do_idx)
		    zvwrit(unit_idx_out, obuf_idx_ptr, "LINE", j+1, NULL);
		if (do_icm) {
		    zvwrit(unit_icm_out, obuf_icm_l_ptr, "LINE", j+1, "BAND", 1,
									NULL);
		    zvwrit(unit_icm_out, obuf_icm_s_ptr, "LINE", j+1, "BAND", 2,
									NULL);
		}
	    }
	}				// line loop
        TIME_EVAL(image);
    }					// pass loop

    // If we're doing overlaps, write out the statistics file

    if (do_ovr) {

	// Finish mean and stdev calculations
	if (do_ovr_normal) {
	    for (int i = 0; i < num_overlaps; i++) {
		if (!overlaps[i].active)
		    continue;		// no inactive ones in this program
	        for (int j = 0; j < overlaps[i].n_images; j++) {
		    double m = overlaps[i].stats[j].sum_dn / overlaps[i].n_pixels;
		    overlaps[i].stats[j].mean = m;
		    double diff =
			    overlaps[i].stats[j].sum2_dn / overlaps[i].n_pixels
			    - m*m;
		    if (diff > 0)
		        overlaps[i].stats[j].stdev = sqrt(diff);
		    else
			overlaps[i].stats[j].stdev = 0.0;
		}
	    }
	}

	// Add overalls to ovr list
	if (do_ovr_overall) {
	    if (num_overlaps + nids > MAX_OVERLAPS) {
		zvmessage("***ERROR*** Too many overlaps!!!  Overall Overlaps ignored","");
	    } else {
	        for (int i = 0; i < nids; i++ ) {
		    if (!overall_overlaps[i].active)
			continue;	// no inactive ones in this program
		    overlaps[num_overlaps++] = overall_overlaps[i];
		}
	    }
	}

	int start_key;
	zvp("START_KEY", &start_key, &count);
	status = mars_save_overlaps(ovr_filename, overlaps, num_overlaps,
			file_models, nids, start_key);
      if (status != 0)
	zvnprintf(256, "Error saving overlaps!! code=%d", status);
    }

    // Now write the data if we're not in incremental mode.  Parallel runs
    // must communicate the data to the master here.
    
    if (!incremental_mode) {		// already written if so
	
	if (mpi_id == 0) {
	    
	    TIME_START(write);
	    
	    // write the data that were computed in this segment.
            for (int b = 0; b < band_count; b++) {
                for (int j=linestart; j<lineend; j++) {	// line loop
		    zvwrit(unit_out, obuf2d_f[b][j-linestart], "LINE", j+1,
                           "BAND", b+1, NULL);
                }
            }
            for (int j = linestart; j < lineend; j++) {
	        if (do_idx)
		    zvwrit(unit_idx_out, obuf_idx[j-linestart], "LINE", j+1,
		                                        NULL);
	        if (do_icm) {
	            zvwrit(unit_icm_out, obuf_icm_l[j-linestart], "LINE", j+1,
							"BAND", 1, NULL);
		    zvwrit(unit_icm_out, obuf_icm_s[j-linestart], "LINE", j+1,
							"BAND", 2, NULL);
                }
	    }
	    
	    TIME_EVAL(write);
	    
	    // Collect the data from all the other CPU's (if any).
	    // Perform this in an asynchronous operation.
            TIME_START(comm);
            
            for (int b = 0; b < band_count; b++) {
	        int data_rcvd = 1;		// # of nodes from which we have data
	    
	        while (data_rcvd < mpi_numprocs) {
		    MPI_Status mpi_status;
		    MPI_Recv(&obuf2d_f[b][0][0], maxdatalen, MPI_FLOAT,
		             MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD,
			     &mpi_status);
                    data_rcvd++;
		
		    TIME_EVAL_START(comm,write);
		
		    int iii = mpi_status.MPI_TAG;		// get sending node #
		    get_linestart_end(&linestart, &lineend, nlo, iii);
	    DPR(zvnprintf(256, "CPU %d received data from CPU %d/%d\n",
			    mpi_id, iii, mpi_numprocs));
		
		    // write the data that were transmitted in this segment.
                    for (int j=linestart; j<lineend; j++) {		// line loop                   
		        zvwrit(unit_out, obuf2d_f[b][j-linestart],
                               "LINE", j+1, "BAND", b+1, NULL);
                    }
                    TIME_EVAL(write);
                }
	    }

	    int data_rcvd = 1;

            while (data_rcvd < mpi_numprocs) {
	        TIME_START(comm);
                MPI_Status mpi_status;
		if (do_idx)
		    MPI_Recv(&obuf_idx[0][0], maxdatalen, MPI_SHORT,
				MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD,
				&mpi_status);
		if (do_icm) {
		    MPI_Recv(&obuf_icm_l[0][0], maxdatalen, MPI_SHORT,
				MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD,
				&mpi_status);
		    MPI_Recv(&obuf_icm_s[0][0], maxdatalen, MPI_SHORT,
				MPI_ANY_SOURCE, MPI_ANY_TAG, MPI_COMM_WORLD,
				&mpi_status);
		}

                data_rcvd++;
		
	        TIME_EVAL_START(comm,write);
		
	        int iii = mpi_status.MPI_TAG;		// get sending node #
		get_linestart_end(&linestart, &lineend, nlo, iii);
	  DPR(zvnprintf(256, "CPU %d received data from CPU %d/%d\n",
			    mpi_id, iii, mpi_numprocs));

                for (int j=linestart; j<lineend; j++) {		// line loop
		    if (do_idx)
			zvwrit(unit_idx_out, obuf_idx[j-linestart],
					"LINE", j+1, NULL);
		    if (do_icm) {
			zvwrit(unit_icm_out, obuf_icm_l[j-linestart],
					"LINE", j+1, "BAND", 1, NULL);
			zvwrit(unit_icm_out, obuf_icm_s[j-linestart],
					"LINE", j+1, "BAND", 2, NULL);
		    }
		}
		TIME_EVAL(write);
	    }
	} else {			// Slave node, so send data to master
	    
	DPR(zvnprintf(256, "CPU %d sends data to CPU 0",mpi_id));
	    TIME_START(comm);
	    
            for (int b = 0; b < band_count; b++) {
	        MPI_Send(&obuf2d_f[b][0][0], nso*(lineend-linestart),
                         MPI_FLOAT, 0, mpi_id, MPI_COMM_WORLD);
            }
	    if (do_idx)
		MPI_Send(&obuf_idx[0][0], nso*(lineend-linestart), MPI_SHORT,
			0, mpi_id, MPI_COMM_WORLD);
	    if (do_icm) {
		MPI_Send(&obuf_icm_l[0][0], nso*(lineend-linestart), MPI_SHORT,
			0, mpi_id, MPI_COMM_WORLD);
		MPI_Send(&obuf_icm_s[0][0], nso*(lineend-linestart), MPI_SHORT,
			0, mpi_id, MPI_COMM_WORLD);
	    }
	    
	    TIME_EVAL(comm);
	}
    }
    
    TIME_START(setup);
    
    // Should free up the input images here... but why bother, the
    // program is about to end....

    ProjectFunc func = cylindrical_proj_inverse;
    MosaicWidthFunc wfunc = cylindrical_width_func;

    switch (proj_mode) {
	case Cylindrical:
	    func = cylindrical_proj_inverse;
	    wfunc = cylindrical_width_func;
	    break;
	case Polar:
	    func = polar_proj_inverse;
	    wfunc = other_width_func;
	    break;
	case Vertical:
	    func = vertical_proj_inverse;
	    wfunc = other_width_func;
	    break;
	case Sinusoidal:
	    func = sinusoidal_proj_inverse;
	    wfunc = sinusoidal_width_func;
	    break;
	default:
	    zvmessage("Internal error: bad proj_mode!!!", "");
	    zabend();
    }

    TIME_EVAL(setup);

    if (mpi_id == 0) {		       // Only the master does footprints et al

	// Add footprints/numbering if requested

	TIME_START(footgrid);
	zvclose(unit_out, NULL);
        // mars_footprints will reopen file and close it when it's done.
	int max_foot;
	zvp("MAX_FOOT", &max_foot, &count);
	if (count == 0 || max_foot <= 0)
	    max_foot = nids;
	mars_footprints(unit_out, max_foot, nso, nlo, band_count,
			camera_in, file_models, func, &proj);

	// Draw overlay or underlay grid lines (and annotation) if requested
	if (zvptst("GRID") ||zvptst("GRID_OVERLAY") || zvptst("GRID_LABELS")) {
           // draw_grid will reopen file and close it when all done.
           draw_grid(unit_out, proj_mode, &proj);
	}

	// Draw tiepoints if requested
	int count;
	zvpcnt("TIEPOINTS", &count);
	if (!zvptst("NO_TIES") && count != 0) {
	    draw_tiepoints(unit_out, nso, nlo, camera_in, func, &proj,
			file_models, nids, NULL, m, min_input, max_input);
	}

	// Draw scale bar if requested
	if (zvptst("BAR")) {
	    draw_scalebar(unit_out, nso, nlo, func, &proj);
	}

	// Do bounding boxes

	mars_mosaic_bbox(nids, camera_in, file_models, nlo, nso,
			func, wfunc, &proj);

	TIME_EVAL(footgrid);
    }

    TIME_START(waitend);
    MPI_Barrier(MPI_COMM_WORLD);
    TIME_EVAL(waitend);
    TIME_EVAL(total);
    TIME_REPORT_MPIID;
    TIME_REPORT(total);
    TIME_REPORT(setup);
    TIME_REPORT(read);
    TIME_REPORT(image);
    TIME_REPORT(comm);
    TIME_REPORT(write);
    TIME_REPORT(waitend);
    TIME_REPORT(footgrid);
    TIME_REPORT_END;

}

////////////////////////////////////////////////////////////////////////
// Inverse projection routines (input -> output) for use by footprinter
////////////////////////////////////////////////////////////////////////


extern "C" int cylindrical_proj_inverse(double in_line, double in_samp,
		double *out_line, double *out_samp,
		int input_number, void *proj_args)
{
    MarsMapProjArgs *proj = (MarsMapProjArgs *)proj_args;
    PigPoint origin;
    PigVector look;
    PigPoint surf_pt;
    int hits;

    proj->camera_in[input_number]->LStoLookVector(in_line, in_samp,origin,look,
							proj->cs);
    hits = proj->surface_model->intersectRay(origin, look, surf_pt);
    // getRay returns "look"
    proj->surface_model->getRay(proj->proj_origin, surf_pt, (hits <=0), look);
    double out_az = proj->cs->getAz(look);
    double out_el = proj->cs->getEl(look);

    double new_az = (out_az - proj->az_first_sample) * proj->azdir;
    while (new_az >= PigDeg2Rad(360.0)) new_az -= PigDeg2Rad(360.0);
    while (new_az < 0) new_az += PigDeg2Rad(360.0);
    *out_samp = new_az / proj->scale;
    *out_line = proj->line_zero_el - (out_el / proj->scale);

    return 0;
}

////////////////////////////////////////////////////////////////////////

extern "C" int polar_proj_inverse(double in_line, double in_samp,
		double *out_line, double *out_samp,
		int input_number, void *proj_args)
{
    MarsMapProjArgs *proj = (MarsMapProjArgs *)proj_args;
    PigPoint origin;
    PigVector look;
    PigPoint surf_pt;
    int hits;
    PigPoint polar;

    proj->camera_in[input_number]->LStoLookVector(in_line, in_samp,origin,look,
							proj->cs);
    hits = proj->surface_model->intersectRay(origin, look, surf_pt);
    // getRay returns "look"
    proj->surface_model->getRay(proj->proj_origin, surf_pt, (hits <=0), look);
    double out_az = PigDeg2Rad(90.0) -
		(proj->cs->getAz(look) - proj->up_azimuth) * proj->azdir;
    double out_range = (proj->cs->getEl(look) + PigDeg2Rad(90.0)) / proj->scale;
    polar.setSpherical(out_az, 0.0, out_range);

    *out_samp = polar.getX() + proj->nadir_samp;
    *out_line = proj->nadir_line - polar.getY();

    return 0;
}

////////////////////////////////////////////////////////////////////////

extern "C" int vertical_proj_inverse(double in_line, double in_samp,
		double *out_line, double *out_samp,
		int input_number, void *proj_args)
{
    MarsMapProjArgs *proj = (MarsMapProjArgs *)proj_args;
    PigPoint origin;
    PigVector look;
    PigPoint surf_pt;
    int hits;

    proj->camera_in[input_number]->LStoLookVector(in_line, in_samp,origin,look,
								proj->cs);
    hits = proj->surface_model->intersectRay(origin, look, surf_pt);

    // *out_line = proj->nlo/2 - (surf_pt.getX() * proj->vert_scale);   // +X is up
    // *out_samp = (surf_pt.getY() * proj->vert_scale) * proj->azdir +
// 			proj->nso/2;   // +Y is right for azdir=1, left for -1

    *out_line = (proj->maxx - surf_pt.getX()) * proj->vert_scale;   // +X is up
    if (proj->azdir > 0)			// +Y is right
	*out_samp = (surf_pt.getY() - proj->miny) * proj->vert_scale;
    else					// +Y is left
	*out_samp = (proj->maxy - surf_pt.getY()) * proj->vert_scale;

    if (hits <= 0)
	return 1;			// doesn't hit plane, error

    return 0;
}

////////////////////////////////////////////////////////////////////////

extern "C" int sinusoidal_proj_inverse(double in_line, double in_samp,
		double *out_line, double *out_samp,
		int input_number, void *proj_args)
{
    MarsMapProjArgs *proj = (MarsMapProjArgs *)proj_args;
    PigPoint origin;
    PigVector look;
    PigPoint surf_pt;
    int hits;

    proj->camera_in[input_number]->LStoLookVector(in_line, in_samp,origin,look,
							proj->cs);
    hits = proj->surface_model->intersectRay(origin, look, surf_pt);
    // getRay returns "look"
    proj->surface_model->getRay(proj->proj_origin, surf_pt, (hits <=0), look);
    double out_az = proj->cs->getAz(look);
    double out_el = proj->cs->getEl(look);

    double new_az = (out_az - proj->center_az);
    while (new_az > PigDeg2Rad(180.0)) new_az -= PigDeg2Rad(360.0);
    while (new_az <= PigDeg2Rad(-180.0)) new_az += PigDeg2Rad(360.0);

    new_az = new_az * cos(out_el-proj->center_el) + proj->center_az;
    new_az = (new_az - proj->az_first_sample) * proj->azdir;
    *out_samp = new_az / proj->scale;
    *out_line = proj->line_zero_el - (out_el / proj->scale);

    return 0;
}

////////////////////////////////////////////////////////////////////////
// Mosaic width routines for bounding box
////////////////////////////////////////////////////////////////////////


extern "C" int cylindrical_width_func(double line, double samp, double ndeg,
		int *out_pix, double *out_line_zeroel,
		int input_number, void *proj_args)
{
    MarsMapProjArgs *proj = (MarsMapProjArgs *)proj_args;
    *out_pix = (int)((PigDeg2Rad(ndeg) / proj->scale) + 0.5);
    *out_line_zeroel = proj->line_zero_el;
    return 0;
}

extern "C" int sinusoidal_width_func(double line, double samp, double ndeg,
		int *out_pix, double *out_line_zeroel,
		int input_number, void *proj_args)
{
    MarsMapProjArgs *proj = (MarsMapProjArgs *)proj_args;
    PigPoint origin;
    PigVector look;

    proj->camera_in[input_number]->LStoLookVector(line, samp,origin,look,
							proj->cs);
    double el = proj->cs->getEl(look);

    *out_pix = (int)((PigDeg2Rad(ndeg) / cos(el-proj->center_el) /
					proj->scale) + 0.5);
    *out_line_zeroel = proj->line_zero_el + 0.5;
    return 0;
}

extern "C" int other_width_func(double line, double samp, double ndeg,
		int *out_pix, double *out_line_zeroel,
		int input_number, void *proj_args)
{
    *out_pix = 0;
    *out_line_zeroel = 0.0;
    return 0;
}


////////////////////////////////////////////////////////////////////////
// A PigMsgFunction that suppresses all printout less than Fatal severity.
// Used by slave nodes during parallel processing.
////////////////////////////////////////////////////////////////////////

void pig_msg_suppressor(char *msg, PigMsgSeverity severity, void *clientData)
{
    if (severity == PigMsgFatal)
	zvmessage(msg, "");
}

////////////////////////////////////////////////////////////////////////
// Given a total number of lines given by "nlo", a total number of
// CPUs, and the cpi_id, we can determine a unique sequence of mosaic
// segments using the function below.
//
// Note that the total number of CPUs is fed in through the global
// variable mpi_numprocs.
//
// We do not use the global variable mpi_id here to identify the cpu
// number.  We feed in the desired cpu id in an explicit variable to
// enable any CPU to figure out how big of a chunk any other CPU will
// receive.  This is actually very important for the master, which
// must figure out the largest possible chunk size it might receive for
// the appropriate memory allocation of message buffers.  This will
// always be the last node, since the remainder is allocated to it.
////////////////////////////////////////////////////////////////////////

void get_linestart_end(int *linestart, int *lineend, int nlo, int cpu_id)
{
  int linesegment=nlo/mpi_numprocs;
  *linestart=cpu_id*linesegment;
  if (cpu_id != mpi_numprocs-1)
    *lineend  = *linestart+linesegment;
  else
    *lineend  = nlo;
  return;
}

////////////////////////////////////////////////////////////////////////
// Draw grid lines on top of (or underneath) the mosaic
////////////////////////////////////////////////////////////////////////

void draw_grid(int unit_out, ProjectionType proj_mode, MarsMapProjArgs *proj)
{
    int i, j, count, def;
    double grid_spacing;
    float grid_dn[MARS_MAX_NB];
    int grid_zoom;
    int line, samp;
    int n_lines, n_lines_to_max;
    double start;
    double az, el;
    double x, y;
    PigPoint polar;
    float *obuf[MARS_MAX_NB];
    char str[10];
    
    for (int b = 0; b < proj->nbo; b++) {
        obuf[b] = new float[proj->nso];
        if (obuf[b] == NULL) {
            zvmessage("Cannot allocate memory for obuf!", "");
            zabend();
        }
    }

    zvmessage("Adding grid lines...", "");
    zvparmd("GRID_SPACING", &grid_spacing, &count, &def, 1, 0);
    if (count == 0)
	grid_spacing = 0.0;		// depends on projection
    zvp("GRID_DN", grid_dn, &count);
    if (count < proj->nbo) {
        for (int b = count; b < proj->nbo; b++)
            grid_dn[b] = grid_dn[count - 1];
    }
    zvp("GRID_ZOOM", &grid_zoom, &count);
    int overlay = zvptst("GRID_OVERLAY");
    int labels_only = zvptst("GRID_LABELS");

    // reopen output for update.  We set U_FORMAT to REAL,
    // that way both "short int" and "float" files can
    // share the same code.  
    zvopen(unit_out, "OP", "UPDATE", "U_FORMAT", "REAL",
	   "OPEN_ACT", "AS", NULL);

    switch (proj_mode) {

	case Cylindrical:

	    if (grid_spacing == 0.0)
		grid_spacing = 10.0;

	    // Create an azimuth/elevation grid

	    // Elevation first.  Calculate the starting location to
	    // ensure that we hit 0.

	    n_lines = 90 / (int)grid_spacing;
	    start = n_lines * (int)grid_spacing;
	    for (i=(int)start; i >= -90; i -= (int)grid_spacing) {
		el = PigDeg2Rad(i);

		// Draw lines

		line = (int)(proj->line_zero_el - (el / proj->scale) + 0.5);
		if (line >= 0 && line < proj->nlo) {
		    if (!labels_only) {
                        for (int b = 0; b < proj->nbo; b++) {
		            zvread(unit_out, obuf[b], "LINE", line+1, "BAND",
                                    b+1, NULL);
		            if (overlay) {
			        for (j=0; j<proj->nso; j++)
				    obuf[b][j] = grid_dn[b];
			    } else {
			        for (j=0; j<proj->nso; j++) {
				    if (obuf[b][j] == 0.0)
                                        obuf[b][j] = grid_dn[b];
                                }
			    }
			    zvwrit(unit_out, obuf[b], "LINE", line+1, "BAND",
                                    b+1, NULL);
		        }
                    }

		    // Draw labels

	snprintf(str, 10, "%d", i);
		    int text_line = (i>=0) ? line-MARGIN : line+MARGIN;
		    mars_write_text(unit_out,proj->nso,proj->nlo,proj->nbo, str,
			0+MARGIN, text_line, grid_zoom, grid_dn,
			(i>=0) ? Lower_Left : Upper_Left);
		    mars_write_text(unit_out,proj->nso,proj->nlo,proj->nbo, str,
			proj->nso-1-MARGIN, text_line, grid_zoom, grid_dn,
			(i>=0) ? Lower_Right : Upper_Right);
		}
	    }

	    // Now draw azimuth lines.  We must go through the entire
	    // image for this, unfortunately.

	    if (!labels_only) {
                for (int b = 0; b < proj->nbo; b++) {
		    for (j=0; j < proj->nlo; j++) {
		        zvread(unit_out, obuf[b], "LINE", j+1, "BAND", b+1,
                                NULL);

		        for (i = 0 ; i <= 360; i += (int)grid_spacing) {
			    double grid_az = 
                                    (PigDeg2Rad(i) - proj->az_first_sample) *
								    proj->azdir;
			    if (grid_az >= PigDeg2Rad(360.0))
			        grid_az -= PigDeg2Rad(360.0);
			    if (grid_az < 0)
			        grid_az += PigDeg2Rad(360.0);
			    samp = (int)(grid_az / proj->scale + 0.5);

			    if (samp >= 0 && samp < proj->nso) {
			        if (overlay) {
				    obuf[b][samp] = grid_dn[b];
			        } else {
				    if (obuf[b][samp] == 0.0) obuf[b][samp] = 
                                                                     grid_dn[b];
			        }
			    }
		        }
		        zvwrit(unit_out, obuf[b], "LINE", j+1, "BAND", b+1,
                                NULL);
		    }
                }
	    }

	    // Azimuth labels

	    for (i=0; i < 360; i += (int)grid_spacing) {
		double grid_az = (PigDeg2Rad(i) - proj->az_first_sample) *
								proj->azdir;
		if (grid_az >= PigDeg2Rad(360.0))
		    grid_az -= PigDeg2Rad(360.0);
		if (grid_az < 0)
		    grid_az += PigDeg2Rad(360.0);
		samp = (int)(grid_az / proj->scale + 0.5);
		if (samp < 0 || samp >= proj->nso)
		    continue;

      snprintf(str, 10, "%d", i);
		mars_write_text(unit_out, proj->nso, proj->nlo, proj->nbo, str,
			samp+MARGIN, 0+MARGIN, grid_zoom, grid_dn,
			Upper_Left);
		mars_write_text(unit_out, proj->nso, proj->nlo, proj->nbo, str,
			samp+MARGIN, proj->nlo-1-MARGIN, grid_zoom, grid_dn,
			Lower_Left);
	    }

	    break;

	case Polar:

	    if (grid_spacing == 0)
		grid_spacing = 10.0;

	    // Draw all the grids at once.  Unfortunately, we have to cycle
	    // through the whole image to do so.

	    if (!labels_only) {
                for (int b = 0; b < proj->nbo; b++) {
		    for (j=0; j<proj->nlo; j++) {
		        zvread(unit_out, obuf[b], "LINE", j+1, "BAND", b+1,
                                NULL);

		        for (i=0; i<proj->nso; i++) {
			    if (!overlay && obuf[b][i] != 0.0)
			        continue;

			    // Compute az/el for this pixel

			    double x_ctr = i - proj->nadir_samp;
			    double y_ctr = proj->nadir_line - j;
			    polar.setXYZ(x_ctr, y_ctr, 0.0);
			    el = polar.getRange() * proj->scale - 
                                    PigDeg2Rad(90.0);
			    // PigVector az is CCW from +X, we want CW/CCW from
			    // +Y
			    az = proj->up_azimuth + (PigDeg2Rad(90.0) -
                                    polar.getAz()) * proj->azdir;

			    // Compute distance from nearest az/el line in 
			    // pixels

			    if (el < PigDeg2Rad(-80.))
			        continue;

			    double grid_sp = PigDeg2Rad(grid_spacing);
			    double ang_dist = fabs(az - ((int)(az/grid_sp)) *
                                    grid_sp);
			    if (ang_dist > grid_sp/2.0)
			        ang_dist = grid_sp - ang_dist;
			    double dist = polar.getRange() * sin(ang_dist);
			    if (dist < 0.5)
			        obuf[b][i] = grid_dn[b];

			    ang_dist = fabs(el - ((int)(el / grid_sp)) *
                                    grid_sp);
			    if (ang_dist > grid_sp/2.0)
			        ang_dist = grid_sp - ang_dist;
			    dist = ang_dist / proj->scale;
			    if (dist < 0.5)
			        obuf[b][i] = grid_dn[b];
		        }
		        zvwrit(unit_out, obuf[b], "LINE", j+1, "BAND", b+1,
                                NULL);
                    }
		}
	    }

	    // Now draw the annotation.  This is complicated for the azimuth
	    // labels because north might not be up (up_azimuth).  Numeric
	    // elevation labels always go straight down from center though.

	    mars_write_text(unit_out, proj->nso, proj->nlo, proj->nbo, "NADIR",
			proj->nadir_samp, proj->nadir_line, grid_zoom, grid_dn,
			Center);

	    for (i = 0; i < 360; i += (int)grid_spacing) {
		az = PigDeg2Rad(90.0) -
			(PigDeg2Rad(i) - proj->up_azimuth) * proj->azdir;
		polar.setSpherical(az, 0.0, proj->nadir_line-MARGIN);
		if (i == 0)
	snprintf(str, 10, "NORTH");
		else
	snprintf(str, 10, "%d", i);
		mars_write_text(unit_out, proj->nso, proj->nlo, proj->nbo, str,
			(int)(polar.getX() + proj->nadir_samp),
			(int)(proj->nadir_line - polar.getY()), grid_zoom,
			grid_dn, Center);
	    }

	    // Print elevation labels.  Calculate the starting location to
	    // ensure we hit 0.

	    n_lines = 90 / (int)grid_spacing;
	    start = n_lines * (int)grid_spacing;
	    for (i=(int)start; i > -90; i -= (int)grid_spacing) {
      snprintf(str, 10, "%d", i);
		int y = (int)(PigDeg2Rad(i+90) / proj->scale+proj->nadir_line);
		if (y >= 0 && y < proj->nlo)
		    mars_write_text(unit_out,proj->nso,proj->nlo,proj->nbo, str,
			proj->nadir_samp, y+MARGIN, grid_zoom, grid_dn,
			Upper_Center);
	    }

	    break;

	case Vertical:

	    if (grid_spacing == 0)	// this is different from the others...
		grid_spacing = 1.0;

	    // Create an X/Y grid

	    // X lines first.  They actually go horizontal, since +X is at
	    // the top (matching North).  Calculate the starting location to
	    // ensure that we hit 0.

	    n_lines_to_max = (int)floor(proj->maxx / grid_spacing);
	    start = (n_lines_to_max * grid_spacing);
	    for (x=start; x >= proj->minx; x -= grid_spacing) {

		// Draw lines

		// line = (int)(proj->nlo/2 - (x * proj->vert_scale) + 0.5);
		line = (int)((proj->maxx - x) * proj->vert_scale + 0.5);
                if (line > 0 && line < proj->nlo-1) {
		    if (!labels_only) {
                        for (int b = 0; b < proj->nbo; b++) {
			    zvread(unit_out, obuf[b], "LINE", line+1, "BAND",
                                    b+1, NULL);
			    if (overlay) {
			        for (j=0; j<proj->nso; j++)
				    obuf[b][j] = grid_dn[b];
			    } else {
			        for (j=0; j<proj->nso; j++)
				    if (obuf[b][j]==0.0) obuf[b][j] =
                                            grid_dn[b];
			    }
			    zvwrit(unit_out, obuf[b], "LINE", line+1, "BAND",
                                    b+1, NULL);
		        }
                    }

		    // Draw labels

	snprintf(str, 10, "%5.1lf", x);
		    int text_line = (x>=0) ? line-MARGIN : line+MARGIN;
		    if (text_line >= 0 && text_line < proj->nlo) {
			mars_write_text(unit_out, proj->nso,proj->nlo,proj->nbo,
				str, 0+MARGIN, text_line, grid_zoom, grid_dn,
				(x>=0) ? Lower_Left : Upper_Left);
			mars_write_text(unit_out, proj->nso,proj->nlo,proj->nbo,
			   str, proj->nso-1-MARGIN, text_line,grid_zoom,grid_dn,
				(x>=0) ? Lower_Right : Upper_Right);
		    }
		}
	    }

	    // Now draw Y lines (vertical, since +Y is to the right or left).
	    // We must go through the entire image for this, unfortunately.
	    // If azdir is 1, +Y is right; if -1, +Y is left.

	    n_lines_to_max = (int)floor(proj->maxy / grid_spacing);
	    start = (n_lines_to_max * grid_spacing);
	    if (!labels_only) {
                for (int b = 0; b < proj->nbo; b++) {
		    for (j=0; j < proj->nlo; j++) {
		        zvread(unit_out, obuf[b], "LINE", j+1, "BAND", b+1,
                                NULL);
		        for (y = start; y >= proj->miny; y -= grid_spacing) {
			    if (proj->azdir > 0)
			        samp = (int)((y - proj->miny) *
                                        proj->vert_scale);
			    else
			        samp = (int)((proj->maxy - y) *
                                        proj->vert_scale);

			    if (samp > 0 && samp < proj->nso-1) {
			        if (overlay) {
				    obuf[b][samp] = grid_dn[b];
			        } else {
				    if (obuf[b][samp] == 0.0)
                                        obuf[b][samp] = grid_dn[b];
			        }
			    }
		        }
		        zvwrit(unit_out, obuf[b], "LINE", j+1, "BAND", b+1,
                                NULL);
                    }
		}
	    }

	    // Y labels

	    for (y = start; y >= proj->miny; y -= grid_spacing) {
		if (proj->azdir > 0)
		    samp = (int)((y - proj->miny) * proj->vert_scale);
		else
		    samp = (int)((proj->maxy - y) * proj->vert_scale);

		if (samp >= 0 && samp < proj->nso) {
	snprintf(str, 10, "%5.1lf", y);
		    mars_write_text(unit_out,proj->nso,proj->nlo,proj->nbo, str,
			samp+MARGIN, 0+MARGIN, grid_zoom, grid_dn,
			Upper_Left);
		    mars_write_text(unit_out,proj->nso,proj->nlo,proj->nbo, str,
			samp+MARGIN, proj->nlo-1-MARGIN, grid_zoom, grid_dn,
			Lower_Left);
		}
	    }

	    // Axis labels.  Put them to the Right/bottom of the lines to
	    // avoid conflicting with the numeric labels.

	    int pos;			// X label goes at Y=0
	    if (proj->azdir > 0)
		pos = (int)((- proj->miny) * proj->vert_scale) - MARGIN;
	    else
		pos = (int)((proj->maxy) * proj->vert_scale) - MARGIN;
	    if (pos <= 0+MARGIN || pos >= proj->nso-1-MARGIN)
		pos = proj->nso/2 - MARGIN;	// Y=0 is off screen, use center

	    mars_write_text(unit_out, proj->nso, proj->nlo, proj->nbo, "+X",
		pos, 0+MARGIN, grid_zoom, grid_dn, Upper_Right);
	    mars_write_text(unit_out, proj->nso, proj->nlo, proj->nbo, "-X",
		pos, proj->nlo-1-MARGIN, grid_zoom, grid_dn,
		Lower_Right);

	    // Y label goes at X=0
	    pos = (int)((proj->maxx) * proj->vert_scale) + MARGIN;
	    if (pos <= 0+MARGIN || pos >= proj->nlo-1-MARGIN)
		pos = proj->nlo/2 + MARGIN;	// X=0 is off screen, use center

	    mars_write_text(unit_out, proj->nso, proj->nlo, proj->nbo,
		(char *)((proj->azdir == 1) ? "-Y" : "+Y"),
		0+MARGIN, pos, grid_zoom, grid_dn, Upper_Left);
	    mars_write_text(unit_out, proj->nso, proj->nlo, proj->nbo,
		(char *)((proj->azdir == 1) ? "+Y" : "-Y"),
		proj->nso-1-MARGIN, pos, grid_zoom, grid_dn,
		Upper_Right);
	    break;

	case Sinusoidal:

	    if (grid_spacing == 0.0)
		grid_spacing = 10.0;

	    // Create an azimuth/elevation grid

	    // Elevation first.  Calculate the starting location to
	    // ensure that we hit 0.

	    n_lines = 90 / (int)grid_spacing;
	    start = n_lines * (int)grid_spacing;
	    for (i=(int)start; i >= -90; i -= (int)grid_spacing) {
		el = PigDeg2Rad(i);

		// Draw lines

		line = (int)(proj->line_zero_el - (el / proj->scale) + 0.5);
		if (line >= 0 && line < proj->nlo) {
		    if (!labels_only) {
                        for (int b = 0; b < proj->nbo; b++) {
		            zvread(unit_out, obuf[b], "LINE", line+1, "BAND",
                                    b+1, NULL);
			    if (overlay) {
			        for (j=0; j<proj->nso; j++)
				    obuf[b][j] = grid_dn[b];
			    } else {
			        for (j=0; j<proj->nso; j++)
				    if (obuf[b][j]==0.0)
                                        obuf[b][j] = grid_dn[b];
			    }
			    zvwrit(unit_out, obuf[b], "LINE", line+1, "BAND",
                                    b+1,  NULL);
                        }
		    }

		    // Draw labels

	snprintf(str, 10, "%d", i);
		    int text_line = (i>=0) ? line-MARGIN : line+MARGIN;
		    mars_write_text(unit_out,proj->nso,proj->nlo,proj->nbo, str,
			0+MARGIN, text_line, grid_zoom, grid_dn,
			(i>=0) ? Lower_Left : Upper_Left);
		    mars_write_text(unit_out,proj->nso,proj->nlo,proj->nbo, str,
			proj->nso-1-MARGIN, text_line, grid_zoom, grid_dn,
			(i>=0) ? Lower_Right : Upper_Right);
		}
	    }

	    // Now draw azimuth lines.  We must go through the entire
	    // image for this, unfortunately.

	    if (!labels_only) {
		for (j=0; j < proj->nlo; j++) {
                    for (int b = 0; b < proj->nbo; b++) {
		        zvread(unit_out, obuf[b], "LINE", j+1, "BAND", b+1,
                                NULL);
		        for (i = 0 ; i <= 360; i += (int)grid_spacing) {
			    double grid_az=(PigDeg2Rad(i) -
                                    proj->az_first_sample) * proj->azdir;
			    if (grid_az >= PigDeg2Rad(360.0))
			        grid_az -= PigDeg2Rad(360.0);
			    if (grid_az < 0)
			        grid_az += PigDeg2Rad(360.0);
			    double el = (proj->line_zero_el - j) * proj->scale;
			    samp = (int)(((grid_az - proj->center_az) * cos(el)
                                    + proj->center_az) / proj->scale + 0.5);

			    if (samp >= 0 && samp < proj->nso) {
			        if (overlay) {
				    obuf[b][samp] = grid_dn[b];
			        } else {
				 if (obuf[b][samp] == 0.0)
                                     obuf[b][samp] = grid_dn[b];
			        }
			    }
		        }
		        zvwrit(unit_out, obuf[b], "LINE", j+1, "BAND", b+1,
                                NULL);
                    }
		}
	    }

	    // Azimuth labels

	    for (i=0; i < 360; i += (int)grid_spacing) {
		double grid_az = (PigDeg2Rad(i) - proj->az_first_sample) *
								proj->azdir;
		if (grid_az >= PigDeg2Rad(360.0))
		    grid_az -= PigDeg2Rad(360.0);
		if (grid_az < 0)
		    grid_az += PigDeg2Rad(360.0);
		samp = (int)(grid_az / proj->scale + 0.5);
		if (samp < 0 || samp >= proj->nso)
		    continue;

      snprintf(str, 10, "%d", i);
		mars_write_text(unit_out, proj->nso, proj->nlo, proj->nbo, str,
			samp+MARGIN, 0+MARGIN, grid_zoom, grid_dn,
			Upper_Left);
		mars_write_text(unit_out, proj->nso, proj->nlo, proj->nbo, str,
			samp+MARGIN, proj->nlo-1-MARGIN, grid_zoom, grid_dn,
			Lower_Left);
	    }

	    break;
	default:
	    zvmessage("Internal error: bad proj_mode!!!!", "");
	    zabend();
    }
    
    // Lastly close the file.  This is needed since we opened it with
    // U_FORMAT=REAL and the actual file data type might be different
    // (SHORT for example).  
    zvclose(unit_out, NULL);
}

////////////////////////////////////////////////////////////////////////
// Draw grid lines on top of (or underneath) the mosaic
////////////////////////////////////////////////////////////////////////

#include "mars_tiepoints.h"

void draw_tiepoints(int unit_out, int nso, int nlo, PigCameraModel **camera_in,
	ProjectFunc func, MarsMapProjArgs *proj,
	PigFileModel **file_models, int nids, PigCoordSystem *cs, PigMission *m,
	int min_input, int max_input)
{
    int count;
    char tptlist[250];
    TiePoint *tiepoints = new (std::nothrow) TiePoint[MARS_MAX_TIEPOINTS];
    if(tiepoints == NULL){
       zvmessage("Error allocating memory for tie points array","");
       zabend();
    }
    int n_ties = MARS_MAX_TIEPOINTS;
    float dn_base[MARS_MAX_NB], dn_buf;

    zvmessage("Adding tiepoint footprints...","");

    zvp("tie_dn", dn_base, &count);
    if (count < proj->nbo) {
        for (int b = count; b < proj->nbo; b++)
            dn_base[b] = dn_base[count - 1];
    }
    zvp("tiepoints", tptlist, &count);
    if (count != 1) {
	zvmessage("Input tiepoint file required!", "");
	zabend();
    }

    mars_load_tiepoints(tptlist, tiepoints, n_ties, file_models, nids,cs,m);

    zvopen(unit_out, "OP", "UPDATE", "U_FORMAT", "REAL",
	   "OPEN_ACT", "AS", NULL);

    // int do_vector = zvptst("VECTOR");
    // int do_flag = zvptst("FLAG");

    for (int i=0; i < n_ties; i++) {
	if (tiepoints[i].type != TIEPOINT_TRADITIONAL)
	    continue;			// only do traditional, for now
	int left = tiepoints[i].left_image;
	int right = tiepoints[i].right_image;

	if (left < min_input || left > max_input)
	    continue;			// outside of input_range
	if (right < min_input || right > max_input)
	    continue;			// outside of input_range
	double left_line = tiepoints[i].left_line;
	double left_samp = tiepoints[i].left_sample;
	double right_line = tiepoints[i].corrected_line;
	double right_samp = tiepoints[i].corrected_sample;

// printf("plotting tie %d,%d : %f,%f\n", left,right, left_line,left_samp);	//!!!!
	
	// Compute where left side is

	double line, samp;

	if ((*func)(left_line, left_samp, &line, &samp, left, proj) != 0)
	    continue;			// skip on error
	int x = (int)(samp + 0.5);
	int y = (int)(line + 0.5);

// printf("x=%d, y=%d\n", x,y);	//!!!!
	if (x < 0 || x > nso || y < 0 || y >= nlo)
	    continue;			// out of bounds

	// Compute where right side is

	double line2, samp2;
	if ((*func)(right_line, right_samp, &line2, &samp2, right, proj) != 0)
	    continue;			// skip on error
	int x2 = (int)(samp2 + 0.5);
	int y2 = (int)(line2 + 0.5);

	// Compute distance between the tiepoints.  Add that to the base DN
	// to get the number to write out.  We use mosaic coords for convenience
	// (to avoid having to deal with projecting to the sample model here).
	// The difference * 10 is added to the DN base, so you can tell how
	// close a tiepoint is to a tenth of a pixel.

	double diff = sqrt((double)(samp-samp2)*(samp-samp2) +
				   (line-line2)*(line-line2));
	
        for (int b = 0; b < proj->nbo; b++) {
            dn_buf = dn_base[b] + diff*10.0;
	    zvwrit(unit_out, &dn_buf, "LINE", y, "SAMP", x, "BAND", b+1,
                    "NSAMPS", 1, NULL);
        }

	// If we're in flag mode, draw a vector (of length 10) towards the
	// center of the right image.  This shows us what we're tying to.
	// We use half brightness to not obscure the main spot.  The line is
	// parallel to the line between the two image centers.

	if (zvptst("FLAG")) {

	    double y_center1, x_center1;
	    double line3, samp3;
	    double line4, samp4;
	    camera_in[left]->getCameraCenter(y_center1, x_center1);
	    if ((*func)(y_center1, x_center1, &line3, &samp3, left, proj) != 0)
	        continue;			// skip on error
	    double y_center2, x_center2;
	    camera_in[right]->getCameraCenter(y_center2, x_center2);
	    if ((*func)(y_center2, x_center2, &line4, &samp4, right, proj) != 0)
	        continue;			// skip on error
	    double angle = atan2(line3-line4, samp4-samp3);
	    for (int j=1; j<10; j++) {
		y2 = (int)(line - sin(angle)*j + 0.5);
		x2 = (int)(samp + cos(angle)*j + 0.5);
                for (int b = 0; b < proj->nbo; b++) {
                    dn_buf = (dn_base[b] + diff*10.0)/2;
                    zvwrit(unit_out,&dn_buf, "LINE",y2, "SAMP",x2, "BAND",b+1,
                            "NSAMPS",1, NULL);
                }
	    }
	}

	// If we're in vector mode, draw a vector from the left tiepoint
	// towards the right tiepoint.  The length of the vector is 10x the
	// distance between them.  This makes it easier to find outlier
	// tiepoints (because their vectors will be longer).
	// We use half brightness to not obscure the main spot.

	if (zvptst("VECTOR")) {

	    double angle = atan2(line-line2, samp2-samp);
	    for (int j=1; j<(diff*10.0); j++) {
		y2 = (int)(line - sin(angle)*j + 0.5);
		x2 = (int)(samp + cos(angle)*j + 0.5);
                for (int b = 0; b < proj->nbo; b++) {
                    dn_buf = (dn_base[b] + diff*10.0)/2;
		    zvwrit(unit_out, &dn_buf, "LINE",y2, "SAMP",x2, "BAND",b+1,
                            "NSAMPS",1,NULL);
                }
	    }
	}

	// Same as VECTOR but we use the true distance rather than 10x.  This
	// shows exactly where both ends of the tiepoint are (bright end is
	// the start).

	if (zvptst("TRUE_VECTOR")) {

	    double angle = atan2(line-line2, samp2-samp);
	    for (int j=1; j<(diff); j++) {
		y2 = (int)(line - sin(angle)*j + 0.5);
		x2 = (int)(samp + cos(angle)*j + 0.5);
                for (int b = 0; b < proj->nbo; b++) {
                    dn_buf = (dn_base[b] + diff*10.0)/2;
		    zvwrit(unit_out,&dn_buf, "LINE",y2, "SAMP",x2, "BAND",b+1,
                            "NSAMPS",1,NULL);
                }
	    }
	}
    }
    zvclose(unit_out, NULL);

    delete[] tiepoints;
}

////////////////////////////////////////////////////////////////////////
// Draw a scale bar on the mosaic
////////////////////////////////////////////////////////////////////////

void draw_scalebar(int unit_out, int nso, int nlo,
	ProjectFunc func, MarsMapProjArgs *proj)
{
    int count, def;
    int i, b;
#define MSG_SIZE 256    
    char msg[MSG_SIZE];

    // Get location to draw

    int pos[2];
    zvp("BAR_POS", pos, &count);

    int l_pos = pos[0];
    int s_pos = pos[1];
    if (l_pos < 0)		// Negative means up from bottom
	l_pos = nlo + l_pos;
    if (s_pos < 0)		// Negative means left from right edge
	s_pos = nso + s_pos;

    // Get pixels per meter

    double ppm = 0.0;

    double out_az, out_el;
    PigVector look;
    PigPoint surf_pt;
    int hits;
    double x_ctr, y_ctr;
    PigPoint polar;

    double range = 0.0;
    double mpp = 0.0;
    zvparmd("BAR_RANGE", &range, &count, &def, 1, 0);
    if (count == 0)
	range = 0.0;

    switch (proj->proj_mode) {

	case Cylindrical:
	    if (range != 0) {
	        snprintf(msg, MSG_SIZE, "Bar range overridden to %f", range);
		zvmessage(msg, "");
	    } else {
	        out_az = (proj->azdir * s_pos * proj->scale) +
							proj->az_first_sample;
	        out_el = (proj->line_zero_el - l_pos) * proj->scale;

	        look = proj->cs->constructVector(out_az, out_el);
	        hits = proj->surface_model->intersectRay(proj->proj_origin,
							   look, surf_pt);
	        if (hits > 0) {		// not infinity
		    // ppm = 1 / range * tan(radians/pixel) but we can
		    // ignore the tan due to small-angle approximation
		    range = (surf_pt - proj->proj_origin).magnitude();
	        }
	    }
	    mpp = range * proj->scale;
	    if (mpp != 0)
		ppm = 1.0 / mpp;
	    if (ppm < 0.01)		// unreasonably far away (>100m/pix)
		ppm = 0.0;
	    break;

	case Polar:
	    if (range != 0) {
	        snprintf(msg, MSG_SIZE, "Bar range overridden to %f", range);
		zvmessage(msg, "");
	    } else {
	        x_ctr = s_pos - proj->nadir_samp;
	        y_ctr = proj->nadir_line - l_pos;
	        polar.setXYZ(x_ctr, y_ctr, 0.0);
	        out_el = polar.getRange() * proj->scale - PigDeg2Rad(90.0);
	        // PigVector az is CCW from +X, we want CW/CCW from +Y
	        out_az = proj->up_azimuth +
			(PigDeg2Rad(90.0) - polar.getAz()) * proj->azdir;

	        look = proj->cs->constructVector(out_az, out_el);
	        hits = proj->surface_model->intersectRay(proj->proj_origin,
			look, surf_pt);
	        if (hits > 0) {		// not infinity
		    // ppm = 1 / range * tan(radians/pixel) but we can
		    // ignore the tan due to small-angle approximation
		    range = (surf_pt - proj->proj_origin).magnitude();
		}
	    }
	    mpp = range * proj->scale;
	    if (mpp != 0)
		ppm = 1.0 / mpp;
	    if (ppm < 0.01)		// unreasonably far away (>100m/pix)
		ppm = 0.0;
	    break;

	case Vertical:
	    ppm = proj->vert_scale;		// Simplicity!!
	    break;

	case Sinusoidal:
	    if (range != 0) {
		snprintf(msg, MSG_SIZE, "Bar range overridden to %f", range);
		zvmessage(msg, "");
	    } else {
	        out_el = (proj->line_zero_el - l_pos) * proj->scale;
	        out_az = (proj->azdir * s_pos * proj->scale) +
				proj->az_first_sample - proj->center_az;
	        if (proj->center_az < proj->az_first_sample)
		    out_az += PigDeg2Rad(360.0);
	        out_az /= cos(out_el - proj->center_el);
	        if (out_az < - PigDeg2Rad(180.0) ||
		    out_az > PigDeg2Rad(180.0)) {
		    break;		// wrapped more than once, so ignore
	        }
	        out_az += proj->center_az;

	        look = proj->cs->constructVector(out_az, out_el);
	        hits = proj->surface_model->intersectRay(proj->proj_origin,
							look, surf_pt);
	        if (hits > 0) {			// not infinity
		    // ppm = 1 / range * tan(radians/pixel) but we can
		    // ignore the tan due to small-angle approximation
		    range = (surf_pt - proj->proj_origin).magnitude();
		}
	    }
	    mpp = range * proj->scale;
	    if (mpp != 0)
		ppm = 1.0 / mpp;
	    if (ppm < 0.01)		// unreasonably far away (>100m/pix)
		ppm = 0.0;
	    break;

	default:
	    zvmessage("Internal error: bad proj_mode!!", "");
	    zabend();
    }

    if (ppm == 0.0) {
	zvmessage("Scale bar info unavailable at selected point.  Bar not drawn.", "");
	return;
    }

    zvmessage("Plotting scale bar...", "");

    // We have the pixels per meter now.  Figure out the actual bar size.

    int bar_size;
    zvp("BAR_SIZE", &bar_size, &count);
#define BUF_SIZE 100
    char buf[BUF_SIZE];

    int actual_size = scalebar_get_size_and_text(ppm, bar_size, "m", buf);

    int minx = s_pos - actual_size/2;
    int maxx = minx + actual_size;

    int bar_height;
    zvp("BAR_HEIGHT", &bar_height, &count);

    if (minx < 0 || maxx > proj->nso || l_pos-bar_height < 0) {
	zvmessage("Scale bar goes off edge of mosaic; not plotted", "");
	return;
    }

    zvopen(unit_out, "OP", "UPDATE", "U_FORMAT", "REAL",
	   "OPEN_ACT", "AS", NULL);

    // Set up buffers and get parameters

    float *obuf[MARS_MAX_NB];

    for (b = 0; b < proj->nbo; b++) {
        obuf[b] = new float[proj->nso];
        if (obuf[b] == NULL) {
            zvmessage("Cannot allocate memory for obuf!", "");
            zabend();
        }
    }

    float bar_dn[MARS_MAX_NB];
    zvp("BAR_DN", bar_dn, &count);
    if (count < proj->nbo) {
        for (b = count; b < proj->nbo; b++)
            bar_dn[b] = bar_dn[count - 1];
    }

    // First, draw the label.  This way if the label is too long the bar
    // will overplot it

    int bar_space;
    zvp("BAR_SPACE", &bar_space, &count);
    float bar_zoom;
    zvp("BAR_ZOOM", &bar_zoom, &count);

    mars_write_text(unit_out, proj->nso, proj->nlo, proj->nbo, buf,
			s_pos+1, l_pos-bar_space+1, bar_zoom, bar_dn,
			Lower_Center);

    // Now draw the main bar line

    for (i=0; i < actual_size; i++) {
	for (b=0; b < proj->nbo; b++) {
	    obuf[b][i] = bar_dn[b];
	}
    }

    for (b=0; b < proj->nbo; b++) {
	zvwrit(unit_out, obuf[b], "LINE", l_pos+1, "BAND", b+1,
				"SAMP", minx+1, "NSAMPS", actual_size, NULL);
    }

    // Draw the end verticals.  obuf is already set up.

    for (b=0; b < proj->nbo; b++) {
	for (i=0; i < bar_height; i++) {
	    zvwrit(unit_out, obuf[b], "LINE", l_pos-i, "BAND", b+1,
				"SAMP", minx+1, "NSAMPS", 1, NULL);
	    zvwrit(unit_out, obuf[b], "LINE", l_pos-i, "BAND", b+1,
				"SAMP", maxx+1, "NSAMPS", 1, NULL);
	}
    }


    zvclose(unit_out, NULL);
}


////////////////////////////////////////////////////////////////////////
// The code below is ripped off from OpenSeadragon, openseadragon-scalebar.js
// and then converted to C.  OSD has a BSD license and is freely sharable.
//
// Supply the pixels per meter and the minimum size in pixels of the scalebar,
// and it will return the size in pixels of the scalebar and the text label
// for it.  It rounds the scalebar to multiples of 1, 2, 2.5, or 5, with
// powers of 10 applied to all of that.
//
// The caller must provide the buffer for the text string (100 chars would
// be more than sufficient).  The return value is the size in pixels.  The
// unitSuffix is e.g. "m" for meters.
////////////////////////////////////////////////////////////////////////

int scalebar_get_size_and_text(double ppm, int minSize, char *unitSuffix,
			char *stringPtr)
{
    double value = scalebar_normalize(ppm, minSize);
    double factor = scalebar_roundSignificand((value / ppm) * minSize, 3);
    int size = (int) round(value * minSize);
    scalebar_getWithUnit(factor, unitSuffix, stringPtr);
    return size;
}

double scalebar_normalize(double value, int minSize)
{
    double significand = scalebar_getSignificand(value);
    double minSizeSign = scalebar_getSignificand(minSize);
    double result = scalebar_getSignificand(significand / minSizeSign);
    if (result >= 5) {
        result /= 5;
    }
    if (result >= 4) {
        result /= 4;
    }
    if (result >= 2) {
        result /= 2;
    }
    return result;
}

double scalebar_getSignificand(double x)
{
    return x * pow(10, ceil(-log10(x)));
}

double scalebar_roundSignificand(double x, int decimalPlaces)
{
    double exponent = -ceil(-log10(x));
    double power = decimalPlaces - exponent;
    double significand = x * pow(10, power);
    // To avoid rounding problems, always work with integers
    if (power < 0) {
        return round(significand) * pow(10, -power);
    }
    return round(significand) / pow(10, power);
}

/* Note the hacky use of | for mu (micro), see the ztext() subroutine */

void scalebar_getWithUnit(double value, char *unitSuffix, char *stringPtr)
{
    int approx = zvptst("APPROX");
    char ab[10];
    strcpy(ab, "");
    if (approx)
        strcpy(ab, "~");

    if (value < 0.000001) {
        snprintf(stringPtr, BUF_SIZE, "%s%.3f n%s", ab, value * 1000000000, unitSuffix);
	return;
    }
    if (value < 0.001) {
	snprintf(stringPtr, BUF_SIZE, "%s%.0f |%s", ab, value * 1000000, unitSuffix);
	return;
    }
    if (value < .01) {
	snprintf(stringPtr, BUF_SIZE, "%s%.0f m%s", ab, value * 1000, unitSuffix);
	return;
    }
    if (value < 1) {
	snprintf(stringPtr, BUF_SIZE, "%s%.0f c%s", ab, value * 100, unitSuffix);
	return;
    }
    if (value >= 1000) {
	snprintf(stringPtr, BUF_SIZE, "%s%.0f k%s", ab, value / 1000, unitSuffix);
	return;
    }
    snprintf(stringPtr, BUF_SIZE, "%s%.0f %s", ab, value, unitSuffix);
    return;
}
