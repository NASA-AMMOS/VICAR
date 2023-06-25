/* marsint */

#include "vicmain_c"

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

#include "PigMission.h"
#include "PigFileModel.h"
#include "PigLabelModel.h"
#include "PigCameraModel.h"
#include "PigPointingModel.h"
#include "PigSurfaceModel.h"
#include "RadiometryModel.h"
#include "PigBrtCorrModel.h"
#include "PigCoordSystem.h"

#include "return_status.h"

#include <math.h>
#include <stdio.h>

#include "mpi_timing_macro.h"

TIME_DEFINE(total);
TIME_DEFINE(setup);
TIME_DEFINE(read);
TIME_DEFINE(image);
TIME_DEFINE(write);
TIME_DEFINE(waitend);
TIME_DEFINE(comm);

#ifdef DEBUG
#define DPR(x) { char msg[256]; x; zvmessage(msg,""); }
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
#define MAX_INPUTS 200
#define MAX_OPEN 200  // all in memory !
#define MAX_OVERLAPS 600  // max number of image overlap combinations
#define MAX_NL MARS_MAX_NL
#define MAX_NS MARS_MAX_NS

#define MAX_OBUF 30000

//Assumes "ibuf", "fileNS" local variables.
#define IBUF(i,j,k) ( *(ibuf[(i)] + (j) * fileNS + (k)) )
//Assumes "fbuf", "fileNS" local variables.
#define FBUF(i,j,k) ( *(fbuf[(i)] + (j) * fileNS + (k)) )

typedef enum { Cylindrical, Polar, Vertical } ProjectionType;

void pig_msg_suppressor(char *msg, PigMsgSeverity severity, void *clientData);

void get_linestart_end(int *linestart, int *lineend, int nlo, int cpu_id);

////////////////////////////////////////////////////////////////////////

void main44()
{
    int i, j, k;
    int status, count, def;
	const size_t msgLen = 150;
    char msg[msgLen];

    PigSurfaceModel *surface_model = NULL;
    char mission[64], instrument[64];

    // Inputs

    int band;
    int nids;

    PigFileModel *file_models[MAX_INPUTS];
    PigCameraModel *camera_in[MAX_INPUTS];
    PigPointingModel *pointing_in[MAX_INPUTS];
    int homogeneous_inputs = TRUE;
    short int *ibuf[MAX_OPEN];
    float *fbuf[MAX_OPEN];
    double FOV[MAX_INPUTS];				// cache for efficiency
    PigVector camera_orientation[MAX_INPUTS];		// cache for efficiency
    PigPoint camera_position[MAX_INPUTS];		// cache for efficiency
    double min_elev = 0.0, max_elev = 0.0;
	double min_az = 0.0, max_az = 0.0;
	double start_az = 0.0, stop_az = 0.0;
    double horizon = 0.0;
    int use_horizon = 0;
    RadiometryModel *radiometric[MAX_INPUTS];
    PigBrtCorrModel *brt_corr[MAX_INPUTS];
    PigCoordSystem *proj_cs = NULL;
    int azdir = 0;				// +1 == CW, -1 == CCW

    int incremental_mode;

    int short_int_data = 1;
    int do_interp = 1;
    char format[10];                   // input data format

    // Outputs

    int unit_out = 0;
    int nlo = 0, nso = 0;
    double scale = 0.0;					// radians/pixel, cyl/polar only
    PigPoint proj_origin;				// cylindrical/polar only
    double line_zero_el = 0.0;			// cylindrical only
    double az_first_sample = 0.0;		// cylindrical only
    double up_azimuth = 0.0;			// polar only
    int nadir_line = 0, nadir_samp = 0;	// polar only
    double maxx = 0.0, maxy = 0.0;		// vertical only
    double vert_scale = 0.0;			// pixels/meter, vertical only

    
    mve_simatrix obuf2d_si = NULL;
    mve_sivectr  obuf2d_si_ptr = NULL;
    
    mve_fmatrix obuf2d_f = NULL;
    mve_fvectr  obuf2d_f_ptr = NULL;

    int do_print = TRUE;		// True if info message should be issued

    // User Parameters

    double tile_bias_value[MAX_INPUTS];
    double *bias_ptr;
    ProjectionType proj_mode;
    double dbl_param;
    int min_input, max_input;

   // Create arrays that store archive for intensity overlap info.
   short int image_num[MAX_OPEN];
   double save_dns[MAX_OPEN];
   int combinations,num_overlaps,match_ids;
   int k1,k2;
   int num_images_archive[MAX_OVERLAPS];
   int num_pixels_archive[MAX_OVERLAPS];
   int image_num_archive[MAX_OVERLAPS][50];
   double save_dns_archive[MAX_OVERLAPS][50];
   combinations=0;


    TIME_START(total);
    TIME_START(setup);

    zvmessage("MARSINT, version 2", "");

    zvmessage("NOTE:  THIS PROGRAM IS NOW OBSOLETE.  Please use", "");
    zvmessage("       marsmap with the ovr_out parameter instead.", "");

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
	       radiometric, brt_corr, proj_cs, mission, instrument, 
	       homogeneous_inputs, MAX_NL, MAX_NS, MAX_INPUTS);


    // find out the input data type.  We check the data type of the first input
    // and assume that ALL inputs have the same data type.  Currently we 
    // support SHORT INT images and FLOAT images.  BYTE image are supported by
    // converting bytes into short ints.

    file_models[0]->openFile();
    status = zvget(file_models[0]->getUnit(), "FORMAT", format, NULL);

    if (status == 1) {
	if ((strcmp(format, "HALF") == 0) || (strcmp(format, "BYTE") == 0))
	    strcpy(format, "HALF");
	else {  
	    strcpy(format, "REAL");
	    short_int_data = 0;
	}
    }
    else {
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
	            snprintf(msg, msgLen,
			"Cannot do radiometric correction for FLOAT images");
		    zvmessage(msg, "");
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

    status=zvparm("BAND",&band,&count,&def,1,0);

    // reads the BIAS values then fill unspecified ones
    zvparmd("BIAS", tile_bias_value, &count, &def, nids, 0);
    for (i=count; i < nids; i++) {
	tile_bias_value[i] = 1.0;
    } 
    bias_ptr = NULL;
    if (count != 0)
	bias_ptr = tile_bias_value;

    // Read the HORIZON parameter
    zvparmd("HORIZON", &horizon, &count, &def, nids, 0);
    use_horizon = (count != 0);
    horizon = PigDeg2Rad(horizon);

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
    // allow progress to be monitored (e.g. in xvd).  Note that for parallel
    // processing, MEMORY mode is required.

    incremental_mode = zvptst("INCREMENTAL");

#ifdef ENABLE_MPI
    if (incremental_mode && mpi_numprocs > 1) {
	zvmessage("Incremental mode not compatible with parallel processing.","");
	zvmessage("Changed to Memory mode.", "");
	incremental_mode = FALSE;
    }
#endif

    // Report on coord system in use

    surface_model->setCoordSystem(proj_cs);

    azdir = proj_cs->getAzimuthDirection();
    snprintf(msg, msgLen, "Mosaic is projected in the %s coordinate frame",
		proj_cs->getFrameName());
    if (do_print) zvmessage(msg, "");

    // Print out input status from labels

    if (do_print)
	mars_print_inputs(nids, pointing_in, camera_in, file_models,
		homogeneous_inputs, mission, instrument);

    if (proj_mode == Cylindrical || proj_mode == Polar) {

	// Determine the min and max azimuth and elevation.  This is only
	// approximate, but that's okay since it is only used to determine
	// the output size.
	double wrap_az = 0.0;
	mars_get_azel_minmax(nids, pointing_in, camera_in, file_models,
			min_elev, max_elev, min_az, max_az, proj_cs, wrap_az);

	snprintf(msg, msgLen,"Elevation minimum %f, Elevation maximum %f",
		PigRad2Deg(min_elev), PigRad2Deg(max_elev));
	if (do_print) zvmessage(msg, "");
	snprintf(msg, msgLen,  "Azimuth minimum %f, Azimuth maximum %f",
		PigRad2Deg(min_az), PigRad2Deg(max_az));
	if (do_print) zvmessage(msg, "");

	if (azdir == 1) {
	    start_az = min_az;
	    stop_az = max_az;
	}
	else {
	    start_az = max_az;
	    stop_az = min_az;
	}

	// compute output image limits

	zvparmd("LEFTAZ", &dbl_param, &count, &def, 1, 0);
	if (count > 0) {
	    start_az = PigDeg2Rad(dbl_param);
	    snprintf(msg, msgLen, "Override: Starting Azimuth (left) = %f",
							PigRad2Deg(start_az));
	    if (do_print) zvmessage(msg, "");
	}
	zvparmd("RIGHTAZ", &dbl_param, &count, &def, 1, 0);
	if (count > 0) {
	    stop_az = PigDeg2Rad(dbl_param);
	    snprintf(msg, msgLen, "Override: Ending Azimuth (right) = %f",
							PigRad2Deg(stop_az));
	    if (do_print) zvmessage(msg, "");
	}
	zvparmd("BOTTOMEL", &dbl_param, &count, &def, 1, 0);
	if (count > 0) {
	    min_elev = PigDeg2Rad(dbl_param);
	    snprintf(msg, msgLen, "Override: Elevation Minimum = %f",
							PigRad2Deg(min_elev));
	    if (do_print) zvmessage(msg, "");
	}
	zvparmd("TOPEL", &dbl_param, &count, &def, 1, 0);
	if (count > 0) {
	    max_elev = PigDeg2Rad(dbl_param);
	    snprintf(msg, msgLen, "Override: Elevation Maximum = %f",
							PigRad2Deg(max_elev));
	    if (do_print) zvmessage(msg, "");
	}

	// Determine location of the center of projection.  This defaults
	// to the average of all the input camera locations, but may be
	// overridden.

	proj_origin.setXYZ(0.0, 0.0, 0.0);
	for (i=0; i < nids; i++) {
	    proj_origin += pointing_in[i]->getCameraPosition(proj_cs);
	}
	proj_origin /= (double)nids;

	snprintf(msg, msgLen, "Projection origin = (%f, %f, %f)",
		proj_origin.getX(), proj_origin.getY(), proj_origin.getZ());
	if (do_print) zvmessage(msg, "");

	zvpcnt("PROJ_ORIGIN", &count);
	if (count == 3) {
	    double array[3];
	    zvparmd("PROJ_ORIGIN", array, &count, &def, 3, 0);
	    proj_origin.setXYZ(array);
	    snprintf(msg, msgLen, "Override: Projection origin = (%f, %f, %f)",
		proj_origin.getX(), proj_origin.getY(), proj_origin.getZ());
	    if (do_print) zvmessage(msg, "");
	}

	// Determine the scale of the output by looking at the first input's
	// horizontal direction (the direction is arbitrary).

	scale = camera_in[0]->getPixelAngle(0);		// radians per pixel
	snprintf(msg, msgLen, "Pixel scale: %f radians/pixel or %f pixels/degree",
		scale, 1.0/PigRad2Deg(scale));
	if (do_print) zvmessage(msg, "");

	zvparmd("SCALE", &dbl_param, &count, &def, 1, 0);
	if (count > 0) {
	    scale = PigDeg2Rad(1.0 / dbl_param);
	    snprintf(msg, msgLen,
		"Override: Pixel scale: %f radians/pixel or %f pixels/degree",
		scale, 1.0/PigRad2Deg(scale));
	    if (do_print) zvmessage(msg, "");
	}

	zvparmd("ZOOM", &dbl_param, &count, &def, 1, 0);
	if (count > 0) {
	    scale /= dbl_param;
	    snprintf(msg, msgLen, "Override: Pixel scale zoomed by %f", dbl_param);
	    if (do_print) zvmessage(msg, "");
	    snprintf(msg, msgLen, "  to %f radians/pixel or %f pixels/degree",
		scale, 1.0/PigRad2Deg(scale));
	    if (do_print) zvmessage(msg, "");
	}
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
	    snprintf(msg, msgLen, "azimuth of first sample = %f",
						PigRad2Deg(az_first_sample));
	    if (do_print) zvmessage(msg, "");
	    line_zero_el = max_elev / scale;
	    snprintf(msg, msgLen, "line of zero elevation = %f", line_zero_el);
	    if (do_print) zvmessage(msg, "");


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
	    snprintf(msg, msgLen, "Azimuth at top of mosaic = %f",PigRad2Deg(up_azimuth));
	    if (do_print) zvmessage(msg, "");

	    break;

	case Vertical:
	    if (do_print) zvmessage("Vertical Projection", "");

	    // There's no practical way to calculate this from the inputs
	    // so depend on the PDF default.
	    zvparmd("MAXX", &maxx, &count, &def, 1, 0);
	    zvparmd("MAXY", &maxy, &count, &def, 1, 0);
	    zvparmd("VERT_SCALE", &vert_scale, &count, &def, 1, 0);
	    vert_scale = 1.0 / vert_scale;	// m/pix -> pix/meter

	    snprintf(msg, msgLen, "Max X distance = %f, Max Y distance = %f", maxx,maxy);
	    if (do_print) zvmessage(msg, "");
	    snprintf(msg, msgLen, "Scale = %f meters/pixel", 1.0 / vert_scale);
	    if (do_print) zvmessage(msg, "");

	    zvparmd("ZOOM", &dbl_param, &count, &def, 1, 0);
	    if (count > 0) {
		vert_scale *= dbl_param;
		snprintf(msg,msgLen, "Override: Scale zoomed by %f to %f meters/pixel",
			dbl_param, 1.0 / vert_scale);
		if (do_print) zvmessage(msg, "");
	    }

	    nso = (int)(2 * maxx * vert_scale);
	    nlo = (int)(2 * maxy * vert_scale);
 
	    break;

	default:
	    zvmessage("Internal error: bad proj_mode!", "");
	    zabend();
    }

    snprintf(msg, msgLen, "Output lines = %d, samples = %d", nlo, nso);
    if (do_print) zvmessage(msg, "");

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
	    if (short_int_data)
	        obuf2d_si = mve_Simatrix(1, nso);
	    else
		obuf2d_f = mve_Fmatrix(1, nso);
	}
	else {
	    if (short_int_data)
	        obuf2d_si = mve_Simatrix(lineend-linestart, nso);
	    else
		obuf2d_f = mve_Fmatrix(lineend-linestart, nso);
	}
	
	maxdatalen = (lineend-linestart)*nso;
	
	// operate only on the required data size
	
	get_linestart_end(&linestart, &lineend, nlo, mpi_id);
    }
    else {
	get_linestart_end(&linestart, &lineend, nlo, mpi_id);
	if (short_int_data)
	    obuf2d_si = mve_Simatrix(lineend-linestart, nso);
	else
	    obuf2d_f = mve_Fmatrix(lineend-linestart, nso);
    }
    
    DPR(snprintf(msg, msgLen, "CPU %d starting with lines %d to %d\n",mpi_id, linestart,lineend));
    
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
               "U_NB", 1, "U_ORG", "BSQ", "O_FORMAT", format, NULL);

	// Write the output labels
	PigMission *m = PigMission::getMissionObject(mission);
	PigLabelModel *labelModel = m->createLabelModel(unit_out);

	switch (proj_mode) {

	case Cylindrical:
	    labelModel->setMapCyl(file_models,
				radiometric, brt_corr, bias_ptr, NULL, nids,
				surface_model, scale, proj_origin, line_zero_el,
				az_first_sample, max_az, 
				min_elev, max_elev, proj_cs);
	    break;

	case Polar:
	    labelModel->setMapPolar(file_models,
				radiometric, brt_corr, bias_ptr, NULL, nids, 
				surface_model, scale, 
				proj_origin, up_azimuth, 
				nadir_line, nadir_samp, max_elev, proj_cs);
	    break;

	case Vertical:
	    labelModel->setMapVert(file_models,
				radiometric, brt_corr, bias_ptr, NULL, nids,
				surface_model, nlo, nso, vert_scale,
				maxx, maxy, proj_cs);
	    break;

	default:
	    zvmessage("Internal error: bad proj_mode!!!!", "");
	    zabend();
	}
	zvplabel(unit_out, 0, 1);

	// write output as zero's in incremental mode only

	if (incremental_mode) {
	    if (short_int_data) {
	        for (j=0; j<nlo; j++)
		    zvwrit(unit_out, obuf2d_si[0], "LINE", j+1, NULL);
	    }
	    else {
	        for (j=0; j<nlo; j++)
		    zvwrit(unit_out, obuf2d_f[0], "LINE", j+1, NULL);
	    }
	}
	
	// reopen output for update

	zvclose(unit_out, NULL);
	zvopen(unit_out, "OP", "UPDATE", "U_FORMAT", format,
	       "U_NS", nso, "U_NL", nlo, "OPEN_ACT", "AS",
	       "U_NB", 1, "U_ORG", "BSQ", "O_FORMAT", format, NULL);
	       
	       
    }
    
    // Calculate the approximate Field Of View for each input, for use in
    // a quick decision whether to project the point into this image or not.
    // Adding H and V FOV's covers the diagonals, plus provides some slop.
    // We divide by 2 because the H and V FOV's cover from edge to edge, while
    // we need only from the center to the edge.  The cosine is stored for
    // easy comparison with a dot product.  We limit the FOV to 90 degrees
    // (180 total) to avoid any back-projection (MER hazcam H+V is > 90).
    // We also compute and save the camera position and orientation here
    // once, rather than rederiving it for each pixel.
    
    for (i=0; i < nids; i++) {
	FOV[i] = cos((file_models[i]->getFOV(camera_in[i], 0) +
		      file_models[i]->getFOV(camera_in[i], 1))/2);
	if (FOV[i] < 0.0)
	    FOV[i] = 0.0;		// Limit to 90 degrees
	camera_position[i] = pointing_in[i]->getCameraPosition(proj_cs);
	camera_orientation[i] = pointing_in[i]->getCameraOrientation(proj_cs);
    }
    
    // Used for vertical projection.  up and down are really interchangeable,
    // but we set them right for consistency.

    PigVector down(0.0, 0.0, -1.0), up(0.0, 0.0, 1.0);
    down = down * proj_cs->getElevationDirection();
    up = up * proj_cs->getElevationDirection();

    //Create and initilize variables necessary for pass loop
    int first_input, last_input;

    int sliceSize = 0;
    int allocatedSize[MAX_OPEN];

    //initialize allocatedSize array
    for (int cnt = 0; cnt < MAX_OPEN; cnt++) {
        allocatedSize[cnt] = 0;
        ibuf[cnt] = NULL;
	fbuf[cnt] = NULL;
    }
    
    TIME_EVAL(setup);
    
    // Make a number of passes, processing MAX_OPEN inputs each pass
    for (first_input = min_input; first_input < max_input;
	 first_input += MAX_OPEN) {
	
	TIME_START(read);
	
	if (mpi_numprocs > 1)
	    snprintf(msg, msgLen, "Pass %d of %d for CPU %d", (first_input/MAX_OPEN)+1,
		    ((max_input-1)/MAX_OPEN)+1, mpi_id);
	else
	    snprintf(msg, msgLen, "Pass %d of %d", (first_input/MAX_OPEN)+1,
		    ((max_input-1)/MAX_OPEN)+1);
	zvmessage(msg, "");
	
	last_input = first_input + MAX_OPEN-1;
	if (last_input >= max_input)
	    last_input = max_input-1;
	
	
        // Calculate the needed memory size for allocation
        int numOfFiles = last_input - first_input + 1;
	if (short_int_data) {
	    for( int iterator = 0; iterator < numOfFiles; iterator++ ) {
		sliceSize = file_models[first_input + iterator]->getNL() * 
		    file_models[first_input + iterator]->getNS();
		if (sliceSize > allocatedSize[iterator] ) {
		    if (ibuf[iterator] != NULL) {
			// Free previously allocated storage since 
			// it's not big enough
			delete[] ibuf[iterator];
		    }
		    ibuf[iterator] = new short int[sliceSize];
		    if(ibuf[iterator] == NULL) {
			zvmessage("unable to allocate input memory array!!!", "");
			zabend();
		    }
		    allocatedSize[iterator] = sliceSize;
		}
	    }
	}
	else {
	    for( int iterator = 0; iterator < numOfFiles; iterator++ ) {
		sliceSize = file_models[first_input + iterator]->getNL() * 
		    file_models[first_input + iterator]->getNS();
		if (sliceSize > allocatedSize[iterator] ) {
		    if (fbuf[iterator] != NULL) {
			// Free previously allocated storage since 
			// it's not big enough
			delete[] fbuf[iterator];
		    }
		    fbuf[iterator] = new float[sliceSize];
		    if(fbuf[iterator] == NULL) {
			zvmessage("unable to allocate input memory array!!!", "");
			zabend();
		    }
		    allocatedSize[iterator] = sliceSize;
		}
	    }
	}
	
	// Read a set of inputs into memory

	if (short_int_data)
	    mars_read_inputs(first_input, last_input, file_models,
			     ibuf, MAX_NL, MAX_NS, band, radiometric, brt_corr);
	else
	    mars_read_inputs(first_input, last_input, file_models,
			     fbuf, MAX_NL, MAX_NS, band, radiometric, brt_corr);
	
	TIME_EVAL_START(read,image);
	
	// Loop through each sample of the output, and pick up the input
	// pixel that corresponds to it
	
	for (j=linestart; j<lineend; j++) {			// line loop
	    
	    if (incremental_mode) {
	        if (short_int_data)
		    obuf2d_si_ptr = obuf2d_si[0];
		else
		    obuf2d_f_ptr = obuf2d_f[0];
            }
	    else {
	        if (short_int_data)
		    obuf2d_si_ptr = obuf2d_si[j-linestart];
	        else 
		    obuf2d_f_ptr = obuf2d_f[j-linestart];
            }
	    
	    if (j % 100 == 0) {
		if (mpi_numprocs > 1)
		    snprintf(msg, msgLen, "line %d, CPU %d", j, mpi_id);
		else
		    snprintf(msg, msgLen, "line %d", j);
		zvmessage(msg, "");
	    }
	    
	    if (incremental_mode) {			// re-read output line
	        if (short_int_data)
		    zvread(unit_out, obuf2d_si[0], "LINE", j+1, NULL);
		else
		    zvread(unit_out, obuf2d_f[0], "LINE", j+1, NULL);
	    }
	    
	    for (i=0; i<nso; i++) {		// sample loop
		if (short_int_data) {
		    if (obuf2d_si_ptr[i] != 0)
		        continue;		// already set, do next iteration
		}
		else {
		    if (obuf2d_f_ptr[i] != 0.0) {
		        continue;		// already set, do next iteration
		    }
		}
		
		// Compute azimuth and elevation for this pixel, and project
		// into space and then XYZ.
		
		PigVector look;
		PigPoint surf_pt, proj_pt;
		double out_az = 0.0, out_el = 0.0;
		double x_ctr = 0.0, y_ctr = 0.0;
		PigVector polar;
		int infinity =0, hits = 0;
		
		switch (proj_mode) {
		    case Cylindrical:
			out_az = (azdir * i * scale) + az_first_sample;
			out_el = (line_zero_el - j) * scale;

			if (use_horizon && out_el > horizon)
			    continue;	// ignore sky for brightness computation

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
			look = proj_cs->constructVector(out_az, out_el);
			hits = surface_model->intersectRay(proj_origin,
							look, surf_pt);
			infinity = (hits <= 0);
			break;

		    case Vertical:
			//!!!! Should these be offset around a projection
			//!!!! center (assumes origin now)????
			x_ctr = (nlo/2 - j) / vert_scale;	// +X is up
			y_ctr = azdir * (i - nso/2) / vert_scale;
			// +Y is right for azdir=1, left for -1
			proj_pt.setXYZ(x_ctr, y_ctr, 0.0);
			
			// Project this point to the actual surface, vertically
			hits = surface_model->intersectRay(proj_pt, down,
							   surf_pt);
			if (hits < 0)
			    hits = surface_model->intersectRay(proj_pt, up,
							       surf_pt);
			infinity = (hits <= 0);
			break;
			
		    default:
			zvmessage("Internal error: bad proj_mode!!", "");
			zabend();
		}

	        num_overlaps=0;
  		for (k = first_input; k <= last_input; k++) {	// picture loop

		    int kp = k-first_input;
                    int fileNS = file_models[k]->getNS();
		    
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
		    
		    
		    double dn;
		    if (short_int_data) {
			    dn = IBUF(kp,(int)(image_line + 0.5),
				   (int)(image_samp+0.5)) * tile_bias_value[k];
			    if (dn <= 0.0)
				dn = 0.0;
			    if (dn > 32766.0)
				dn = 32766.0;
			    obuf2d_si_ptr[i] = (short int)(dn + 0.5);
		    }
	            else {
			    dn = FBUF(kp, (int)(image_line + 0.5),
				   (int)(image_samp+0.5)) * tile_bias_value[k];
			    obuf2d_f_ptr[i] = dn;
		    }
		    num_overlaps += 1;
		    image_num[num_overlaps]=k+1;
		    save_dns[num_overlaps]=dn;

		   
		}			// picture loop
		
		//Abort pixel if any are zero
		if(num_overlaps > 1){
		  for(k1=1; k1 <= num_overlaps; k1++){
		    if(save_dns[k1] <= 0.0){
		      num_overlaps = 0;
		      break;
		    }
		  }
		}
		
		//Look for another occurrence of this set of image overlaps.
		if(num_overlaps > 1){
		  if(combinations > 0){
		    for(k1=1; k1 <= combinations; k1++){    // search overlaps
		      if(num_overlaps == num_images_archive[k1]){
		        match_ids=1;
		        for(k2=1; k2 <= num_overlaps; k2++){ // check images
		          if(image_num[k2] != image_num_archive[k1][k2]){
		            match_ids=0;
		            break;
		          }
		        }
		        if(match_ids == 1){   // match, add to same overlap
                          num_pixels_archive[k1] += 1;
		          for(k2=1; k2 <= num_overlaps; k2++){
		            save_dns_archive[k1][k2] += save_dns[k2];
		          }
		          num_overlaps=0;
		          break;
		        }
		      }
		    }
		  }
		  if(num_overlaps > 1 || combinations == 0){ // make new overlap
		    combinations += 1;
		    if(combinations >= MAX_OVERLAPS){
		      zvmessage("Increase MAX_OVERLAPS value","");
		      zabend();
		    }
		    num_images_archive[combinations]=num_overlaps;
                    num_pixels_archive[combinations] = 1;
		    for(k2=1; k2 <= num_overlaps; k2++){
		      image_num_archive[combinations][k2] = image_num[k2];
		      save_dns_archive[combinations][k2] = save_dns[k2];
		    }
		  }
		}

		
	    }				// sample loop
	    
	    if (incremental_mode)
		if (short_int_data)
		    zvwrit(unit_out, obuf2d_si_ptr, "LINE", j+1, NULL);
		else
		    zvwrit(unit_out, obuf2d_f_ptr, "LINE", j+1, NULL);

	}				// line loop
        TIME_EVAL(image);
    }					// pass loop
    
    // write intensity table
    printf("number of overlaps= %d\n",combinations);
//    for(j=1; j <= combinations; j++){
//      printf("%d\n",num_images_archive[j]);
//      for(i=1; i <= num_images_archive[j]; i++){
//        printf("%d  ",image_num_archive[j][i]);
//      }
//      for(i=1; i <= num_images_archive[j]; i++){
//        printf("%f  ",save_dns_archive[j][i]/num_pixels_archive[j]);
//      }
//      printf("\n");
//    }
    zvpone("OUT",msg,2,150);
    FILE *f;
    f = fopen(msg, "w");
    if (f == NULL) {
       printf("Error opening second output file\n");
       zabend();
    }
    for(j=1; j <= combinations; j++){
      fprintf(f,"%d\n",num_images_archive[j]);
      for(i=1; i <= num_images_archive[j]; i++){
        fprintf(f,"%d  ",image_num_archive[j][i]);
      }
      for(i=1; i <= num_images_archive[j]; i++){
        fprintf(f,"%f  ",save_dns_archive[j][i]/num_pixels_archive[j]);
      }
      fprintf(f,"\n");
    }
    fclose(f);


    // Now write the data if we're not in incremental mode.  Parallel runs
    // must communicate the data to the master here.
    
    if (!incremental_mode) {		// already written if so
	
	if (mpi_id == 0) {
	    
	    TIME_START(write);
	    
	    // write the data that were computed in this segment.
	    for (j=linestart; j<lineend; j++) {			// line loop
		if (short_int_data)
		    zvwrit(unit_out, obuf2d_si[j-linestart], "LINE", j+1, NULL);
		else
		    zvwrit(unit_out, obuf2d_f[j-linestart], "LINE", j+1, NULL);
	    }
	    
	    TIME_EVAL(write);
	    
	    // Collect the data from all the other CPU's (if any).
	    // Perform this in an asynchronous operation.
	    
	    int data_rcvd = 1;		// # of nodes from which we have data
	    
	    while (data_rcvd < mpi_numprocs) {
		MPI_Status mpi_status;
		TIME_START(comm);
		
		if (short_int_data)
		    MPI_Recv(&obuf2d_si[0][0], maxdatalen, MPI_SHORT, MPI_ANY_SOURCE,
			     MPI_ANY_TAG, MPI_COMM_WORLD, &mpi_status);
		else
		    MPI_Recv(&obuf2d_f[0][0], maxdatalen, MPI_FLOAT, MPI_ANY_SOURCE,
			     MPI_ANY_TAG, MPI_COMM_WORLD, &mpi_status);
		data_rcvd++;
		
		TIME_EVAL_START(comm,write);
		
		i = mpi_status.MPI_TAG;		// get sending node #
		get_linestart_end(&linestart, &lineend, nlo, i);
		DPR(snprintf(msg, msgLen, "CPU %d received data from CPU %d/%d\n",
			    mpi_id, i, mpi_numprocs));
		
		// write the data that were transmitted in this segment.
		for (j=linestart; j<lineend; j++) {		// line loop
		    if (short_int_data)
		        zvwrit(unit_out, obuf2d_si[j-linestart], "LINE", j+1, NULL);
		    else
		        zvwrit(unit_out, obuf2d_f[j-linestart], "LINE", j+1, NULL);
		}
		TIME_EVAL(write);
	    }
	}
	else {			// Slave node, so send data to master
	    
	    DPR(snprintf(msg, msgLen, "CPU %d sends data to CPU 0",mpi_id));
	    TIME_START(comm);
	    
	    if (short_int_data)
	        MPI_Send(&obuf2d_si[0][0], nso*(lineend-linestart), MPI_SHORT, 0,
			 mpi_id, MPI_COMM_WORLD);
	    MPI_Send(&obuf2d_f[0][0], nso*(lineend-linestart), MPI_FLOAT, 0,
		     mpi_id, MPI_COMM_WORLD);
	    
	    TIME_EVAL(comm);
	}
    }
    
    TIME_START(setup);
    
    //Free up all dynamically allocated memory for ibuf
    for (int counter = 0; counter < MAX_OPEN; counter++) {
        if (ibuf[counter] != NULL)
	    delete[] ibuf[counter];
	
        if (fbuf[counter] != NULL)
	    delete[] fbuf[counter];
	
    }

    TIME_EVAL(setup);

    // Footprints, grid, and numbering have been removed.

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
    TIME_REPORT_END;

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

