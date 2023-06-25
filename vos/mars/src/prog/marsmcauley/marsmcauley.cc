/* marsmcauley */
#include "vicmain_c"
#include "zvprintf.h"

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
#include "SimpleImage.h"

#include "amoeba.h"

#include "return_status.h"

// buffer sizes in main program - these should be made dynamic, eventually!
#define MAX_INPUTS 2000
#define MAX_OPEN 100
#define MAX_NL MARS_MAX_NL
#define MAX_NS MARS_MAX_NS
#define MAX_OBUF 100000
#define MAX_COLUMNS 100000        // max # of columns (thus camera models)

////////////////////////////////////////////////////////////////////////
// Used by the inverse projection routine
////////////////////////////////////////////////////////////////////////
struct McauleyProjArgs {
    PigCameraModel **camera_in;
    PigSurfaceModel *surface_model;
    PigCameraModel **output_cameras;
    PigCameraModel *camera_out_copy;    // prototype output camera (info only)
    double *x_offset;
    double *y_offset;
    int nso;
    int nso_360;
    double start_az;
    double stop_az;               // info only
    double proj_el, proj_line;    // info only
    PigCoordSystem *cs;
    int azdir;
    PigPoint ring_center;
};

struct RingParams {
    PigPoint camera_c[MAX_COLUMNS];
    int nso;
};

extern "C" int mcauley_proj_inverse(double in_line, double in_samp,
        double *out_line, double *out_samp,
        int input_number, void *proj_args);

extern "C" int mcauley_width_func(double line, double samp, double ndeg,
		int *out_pix, double *out_line_zeroel,
		int input_number, void *proj_args);

int getRingParams(int nso, PigCameraModel **output_cameras,
        PigPoint ring_guess[3],
        PigPoint &ring_center, PigVector &ring_axis, double &ring_radius);
extern "C" double RingObjective(double p[], int mdim, void *func_args);

void rotateCameraModel(PigCameraModel *camera,
        int do_ring_ovr, PigPoint ring_center, PigPoint ring_ctr_ovr,
        PigQuaternion ring_rot,
        int do_ring_rad_ovr, double ring_rad_ovr);

////////////////////////////////////////////////////////////////////////

void main44()
{
    int i, j, k;
    int status, count, def;
#define MSG_SIZE 256
    char msg[MSG_SIZE];

    int band;
    int band_count;
    char mission[64], instrument[64];

    PigSurfaceModel *surface_model;

    McauleyProjArgs proj;

    // Inputs
    // There are 4 versions of all the input lists:
    // 		left, right, active, combined
    // left and right come from INP and RINP respectively.
    // active (which has no prefix) is a pointer to which set we're running
    // (left or right)
    // combined is a combined list of both left and right.
    // Some are missing, e.g. don't need a combined brt corr.
    // Note that "left" and "right" are a naming convention and do not
    // have to reflect what's ACTUALLY the left or right eye.

    int nids, l_nids, r_nids, c_nids;
    PigFileModel *l_file_models[MAX_INPUTS];
    PigFileModel *r_file_models[MAX_INPUTS];
    PigFileModel *c_file_models[MAX_INPUTS];
    PigFileModel **file_models;
    PigCameraModel *l_camera_in[MAX_INPUTS];
    PigCameraModel *r_camera_in[MAX_INPUTS];
    PigCameraModel *c_camera_in[MAX_INPUTS];
    PigCameraModel **camera_in;
    PigPointingModel *l_pointing_in[MAX_INPUTS];
    PigPointingModel *r_pointing_in[MAX_INPUTS];
    PigPointingModel *c_pointing_in[MAX_INPUTS];
    PigPointingModel **pointing_in;
    RadiometryModel *l_radiometric[MAX_INPUTS];
    RadiometryModel *r_radiometric[MAX_INPUTS];
    RadiometryModel **radiometric;
    PigBrtCorrModel *l_brt_corr[MAX_INPUTS];
    PigBrtCorrModel *r_brt_corr[MAX_INPUTS];
    PigBrtCorrModel **brt_corr;

    int homogeneous_inputs = TRUE;
    double FOV[MAX_INPUTS];                      // cache for efficiency
    PigVector camera_orientation[MAX_INPUTS];    // cache for efficiency
    PigPoint camera_position[MAX_INPUTS];        // cache for efficiency
    double min_elev, max_elev, min_az, max_az;
    PigCameraModel *camera_in_rot[MAX_INPUTS];
    PigCoordSystem *proj_cs;
    PigCoordSystem *start_az_cs;    // only for start_az
    PigCoordSystem *site_cs;
    int azdir;    // +1 == CW, -1 == CCW
    SimpleImage<short int> *short_int_images[MAX_INPUTS];
    memset(short_int_images, 0, sizeof(short_int_images));
    SimpleImage<float> *float_images[MAX_INPUTS];
    memset(float_images, 0, sizeof(float_images));

    int short_int_data = 1;    // default data type
    int do_interp = 1;    // interpolation flag
    int do_clamp = 1;   // dn values clamp 
    char format[10];    // input data format

    // Outputs

    int unit_out = 0;
    int unit_idx_out = 0, unit_icm_out = 0;
    int nlo, nso, nso_360;
    PigCameraModel *camera_out, *output_cameras[MAX_COLUMNS];
    double y_offset[MAX_COLUMNS];    // includes center and offset to edge
    double x_offset[MAX_COLUMNS];    // just the center point of the col
    double top_nl, bottom_nl;
    PigPointingModel *pointing_out;
    short int *obuf_si[MARS_MAX_NB];
    float *obuf_f[MARS_MAX_NB];
    short int *obuf_idx;
    float *obuf_icm_l;
    float *obuf_icm_s;

    double start_az, start_az_site;
    double stop_az, stop_az_site;
    double proj_el, proj_line;
    int do_print = TRUE;             // True if info message should be issued

    // User Parameters

    int border_edge;
    double tile_bias_value[MAX_INPUTS];
    double *bias_ptr;
    int full_frame;
    int minsamp, maxsamp;
    int min_input, max_input;
    PigPoint ring_center;
    PigVector ring_axis;
    double ring_radius = 0.0;
    double ring_rad_ovr;            // Overrides for ring
    PigVector ring_axis_ovr;
    PigPoint ring_ctr_ovr;
    int do_ring_rad_ovr = FALSE;
    int do_ring_ovr = FALSE;        // axis or center
    int disp_pix;

    PigVector output_direction;

    zvmessage("MARSMCAULEY version 2020-11-30", "");

    // Get the input file list, and set up initial camera/pointing models
    // for each input.

    mars_setup(l_nids, l_file_models, l_camera_in, l_pointing_in,
	    surface_model,
            l_radiometric, l_brt_corr,
            start_az_cs, mission, instrument, homogeneous_inputs,
            MAX_NL, MAX_NS, MAX_INPUTS);

    // See which image to use as cmod prototypes

    int l_proto = 0;
    int r_proto = 0;
    int pr;
    zvp("L_PROTO", &pr, &count);
    if (count != 0 && pr != 0) {
	l_proto = pr-1;
	snprintf(msg, MSG_SIZE, "Using Left image %d as prototype instead of 0", l_proto);
	zvmessage(msg, "");
    }
    if (l_proto > l_nids) {
	zvmessage("L_PROTO set larger than the number of images","");
	zabend();
    }

    zvp("R_PROTO", &pr, &count);
    if (count != 0 && pr != 0) {
	r_proto = pr-1;
	snprintf(msg, MSG_SIZE, "Using Right image %d as prototype instead of 0", r_proto);
	zvmessage(msg, "");
    }

    int proto = l_proto;

    // Set up all the pointers to use the left side as the default.

    nids = l_nids;
    file_models = l_file_models;
    camera_in = l_camera_in;
    pointing_in = l_pointing_in;
    radiometric = l_radiometric;
    brt_corr = l_brt_corr;

    int two_lists = FALSE;
    PigFileModel *file_partner = NULL;
    PigCameraModel *camera_partner = NULL;

    // If the right-side input exists, read it in and create the combined
    // lists.  If -right is given, reset the pointers to right.

    zvpcnt("RINP", &count);
    r_nids = 0;
    if (count != 0) {

	two_lists = TRUE;

	char **filenames = new char *[MAX_INPUTS];
	if (filenames == NULL) {
	    zvmessage("Memory error calling mars_get_filelist","");
	    zabend();
	}
	mars_get_filelist("RINP", r_nids, filenames, MAX_INPUTS, FALSE);

	PigCoordSystem *dummy_cs;
	mars_read_filelist(r_nids, filenames,
		r_file_models, r_camera_in, r_pointing_in,
		r_radiometric, r_brt_corr,
		dummy_cs, mission, instrument, homogeneous_inputs,
		MAX_NL, MAX_NS, NULL, NULL);

        if (r_proto > r_nids) {
	    zvmessage("R_PROTO set larger than the number of images","");
	    zabend();
        }

	if (zvptst("RIGHT")) {		// reset pointers to R side
	    nids = r_nids;
	    file_models = r_file_models;
	    camera_in = r_camera_in;
	    pointing_in = r_pointing_in;
	    radiometric = r_radiometric;
	    brt_corr = r_brt_corr;
	    proto = r_proto;
	    file_partner = l_file_models[l_proto];
	    camera_partner = l_camera_in[l_proto];
	}
	else {				// L side
	    file_partner = r_file_models[r_proto];
	    camera_partner = r_camera_in[r_proto];
	}

	// Create the combined lists

	c_nids = l_nids + r_nids;
	if (c_nids > MAX_INPUTS) {
	    zvmessage("Combined L+R lists are too large", "");
	    zabend();
	}
	int c = 0;
	for (int i=0; i < l_nids; i++) {
	    c_file_models[c] = l_file_models[i];
	    c_camera_in[c] = l_camera_in[i];
	    c_pointing_in[c] = l_pointing_in[i];
	    c++;
	}
	for (int i=0; i < r_nids; i++) {
	    c_file_models[c] = r_file_models[i];
	    c_camera_in[c] = r_camera_in[i];
	    c_pointing_in[c] = r_pointing_in[i];
	    c++;
	}
    } else {

	// No right-side list given (traditional mode)... so just copy the
	// left lists to the combined

	c_nids = l_nids;
	int c = 0;
	for (int i=0; i < l_nids; i++) {
	    c_file_models[c] = l_file_models[i];
	    c_camera_in[c] = l_camera_in[i];
	    c_pointing_in[c] = l_pointing_in[i];
	    c++;
	}
	file_partner = l_file_models[l_proto];
    }

    // At this point, the arrays are set up as before... if you just use
    // file_models or camera_in or pointing_in, you get the correct set based
    // on doing a L or R side mosaic.  So nothing needs to change from this
    // point on in the bulk of the program... EXCEPT where it's figuring out
    // the geometry... in which case we need to use the combined lists.  But
    // we can unconditionally use the combined lists even in traditional mode.

    PigMission *m = PigMission::getMissionObject(mission);

    // Everything is done in the Rover frame now.  The only thing the
    // coord parameter controls now is how to interpret start_az.

    proj_cs = camera_in[proto]->getCoordSystem();
    site_cs = m->getCoordSystem(file_models[proto], "SITE");

    // find out the input data type.  We check the data type of the first input
    // and assume that ALL inputs have the same data type.  Currently we 
    // support SHORT INT images and FLOAT images.  BYTE image are supported by
    // converting bytes into short ints.

    file_models[proto]->openFile();
    status = zvget(file_models[proto]->getUnit(), "FORMAT", format, NULL );

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
                    snprintf(msg, MSG_SIZE,
                        "Cannot do radiometric correction for FLOAT images");
                    zvmessage(msg, "");
                    printed = 1;
                }
           }
       }
    }

    int maxNB = 1;
    for (int n = 0; n < nids; n++) {
        maxNB = file_models[n]->getNB() > maxNB ?
                file_models[n]->getNB() : maxNB;
    }
    // get parameter overrides if any
    band = 0;
    zvp("BAND", &band, &count);
    band_count = 1;

    if (count == 0) {
        // no input band specified; process all bands
        band_count = maxNB;
        snprintf(msg, MSG_SIZE, "Number of bands to be processed is (%d)", band_count);
        zvmessage(msg, "");
    } else {
        // check if input band number is greater than number of bands in input
        if (band > maxNB) {
            snprintf(msg, MSG_SIZE, "Input band (%d) is greater than number of "
                    "bands in input image. Band set to 1.", band);
            zvmessage(msg, "");
            band = 1;
        }
    }

    // Compute an output camera model which is a synthesis of both left and
    // right models, if they exist, in order to optimize stereo viewing.
    // The camera model of the first input is used, but with the subtype
    // nulled out - the theory being that this is often used for a filter,
    // but for the output, we want to use only *one* model for all filters,
    // so the colors will register.  This is somewhat mission-specific due
    // to the reliance on the subtype usage!!!!

    // Previously, the output model was calculated later, and the input[proto]
    // model was used to compute the image size.  However, the linearization
    // process can produce an output with significantly different pixel scale
    // than its input, especially for something like the MER hazcam.  So, we
    // create the basic output model first, use that for image sizing, then
    // everything matches up.

    // Normally we use "nominal" (unpointed) models, because they don't have
    // the vagaries of camera pointing included.  This works if all the cal
    // camera models have the same pointing, e.g. any of the mast mounted
    // cameras on MSL or 2020.  However, if that's not the case (e.g. very
    // disparate cameras, like arm vs mast) then we have to use the final
    // pointed cameras... presumably they're at least close, or stereo won't
    // work!
    //

    PigCameraModel *camera = NULL;
    PigCameraModel *camera_other = NULL;

    int use_pointing = zvptst("USE_POINTING");
    if (use_pointing)
	zvmessage("Using ACTUAL pointing of the first cameras!", "");

    // Get the model of the first input

    if (use_pointing) {
	camera = camera_in[proto]->clone();
    } else {

        camera = PigCameraModel::create(
            camera_in[proto]->getMissionName(),
            camera_in[proto]->getInstrumentName(),
            camera_in[proto]->getCameraVersion(),
            camera_in[proto]->getCameraSubtype(),
            NULL,    // special 
            camera_in[proto]->getCameraConstruction(),
            camera_in[proto]->getCameraCalibration(),
            NULL);
    }

    // Get the model for its "stereo partner".  If no R-side list is given,
    // we predict this by getting the stereo partner camera.  However, this
    // is not always possible... for example, the stereo partner for MSL
    // focus-adjusted Mastcams cannot be predicted because we don't know the
    // focus of the partner.  So if an R-side list is given, we get the partner
    // from that.  This is the primary motivation for why R-side lists were
    // added.

    if (two_lists) {			// Two lists given

	if (use_pointing) {
	    camera_other = camera_partner->clone();
	} else {

	    // We specificially want an unpointed camera here to match the
	    // main camera.  So that's the main reason we re-make from strings.
	    // It probably won't work if the cameras have different cal
	    // pointings (like a mast camera and an arm camera...)

            camera_other = PigCameraModel::create(
                camera_partner->getMissionName(),
                camera_partner->getInstrumentName(),
                camera_partner->getCameraVersion(),
                camera_partner->getCameraSubtype(),
                NULL,    // special 
                camera_partner->getCameraConstruction(),
                camera_partner->getCameraCalibration(),
                NULL);
	}

	zvmessage("Using stereo partner from opposite list", "");
    }
    else {
	zvmessage("Using standard stereo partner", "");

	if (use_pointing) {
	    zvmessage("Can't use -USE_POINTING unless RINP is used", "");
	    zabend();
	}
        // get the model for its "stereo partner"
        camera_other = PigCameraModel::create(
                camera_in[proto]->getMissionName(),
                camera_in[proto]->getInstrumentName(),
                camera_in[proto]->getCameraVersion(),
                camera_in[proto]->getCameraSubtype(),
                "stereo",    //special
                camera_in[proto]->getCameraConstruction(),
                camera_in[proto]->getCameraCalibration(),
                NULL);
    }

    // Create Pointing Model based on the main input. 
    pointing_out = PigPointingModel::create(camera,
            camera->getMissionName(), 
            camera->getInstrumentName(),
            NULL,
            true);

    // Special case: we really do want to use the l_ or r_file_models here.
    // That's because we have to choose a consistent size for the output that
    // works for both eyes.  We arbitrarily choose the left as the default,
    // but -master_right will choose the right instead.  This size can get
    // modified by the zoom factor.

    double xdim_out = l_file_models[l_proto]->getNS();
    double ydim_out = l_file_models[l_proto]->getNL();

    if (zvptst("MASTER_RIGHT")) {
	if (!two_lists) {
	    zvmessage("-master_right cannot be used unless there are two lists!","");
	    zabend();
	}
	xdim_out = r_file_models[r_proto]->getNS();
	ydim_out = r_file_models[r_proto]->getNL();
    }

    // Zoom the output.  This is done by changing the nl/ns of the output
    // camera model.

    double zoom;
    zvparmd("ZOOM", &zoom, &count, &def, 1, 0);
    if (count > 0) {
	xdim_out *= zoom;
	ydim_out *= zoom;
	snprintf(msg, MSG_SIZE, "Zooming mosaic by %f", zoom);
	zvmessage(msg, "");
    }

    // Setting an explicit scale is a bit tricky, because it's all based
    // on the input camera models.  We get the (horizontal) IFOV of the
    // camera, which is good for the center only but we really just use
    // the center of the cameras anyway.  Compare that to the desired IFOV
    // (really, scale) and adjust as if there was a zoom factor.
    //
    // As above, we want to use the explicit L or R here for consistency.

    double scale;
    zvparmd("SCALE", &scale, &count, &def, 1, 0);
    if (count > 0) {
	// it's pixels/deg so convert to deg/pix before going to rad
	scale = PigDeg2Rad(1.0/scale);
	double ifov = l_camera_in[l_proto]->getPixelAngle(1);
        if (zvptst("MASTER_RIGHT")) {
	    ifov = r_camera_in[l_proto]->getPixelAngle(1);
	}
	double zoom = ifov / scale;
	xdim_out *= zoom;
	ydim_out *= zoom;
	snprintf(msg,MSG_SIZE, "Scaling mosaic to %f deg/pix, means zoom of %f",
				PigRad2Deg(scale), zoom);
	zvmessage(msg, "");
    }

    // now align the models.  This is tricky because we do not know the
    // aspect ratio it will come up with, and the line/samp need to be in
    // that same aspect ratio.  So, we set the sample size to the same as
    // the line size, then compute an alignment, derive an aspect ratio via
    // getPixelAngle() (Hs and Vs would be better, if we had easy access to
    // them here), use that to modify the sample size, and then re-align
    // the models with that size.

    xdim_out = ydim_out;

    camera_out = camera->alignStereoCameras(
		file_models[proto]->getNS(), file_models[proto]->getNL(),
		file_partner->getNS(), file_partner->getNL(),
		(int)(xdim_out+0.5), (int)(ydim_out+0.5),
		camera_other);
    double aspect = camera_out->getPixelAngle(1)/camera_out->getPixelAngle(0);

    xdim_out = xdim_out * aspect;

    camera_out = camera->alignStereoCameras(
		file_models[proto]->getNS(), file_models[proto]->getNL(),
		file_partner->getNS(), file_partner->getNL(),
		xdim_out, ydim_out,
		camera_other);

    // Create Pointing Model based on the main input. 
    // Now that alignment is done and we have new camera model, update
    // Pointing Model to use that model
    pointing_out->setCameraModel(camera_out);
    delete camera;
    delete camera_other;

    // Since PointingModel was created using original "camera" Camera Model
    // pointing camera using "camera's" file_model would lead to results
    // as expected.
    pointing_out->pointCamera(file_models[proto]);

    //Apply nav file correction, if any, to the output pointing
    status = mars_apply_navtable(1, &pointing_out, file_models);
    if (status >= 2)        // 1 means user didn't ask for file
        zabend();

    // get parameter overrides if any
    zvp("BORDER", &border_edge, &count);
    full_frame = zvptst("FULL_FRAME");

    // read BIAS values then fill unspecified ones
    zvparmd("BIAS", tile_bias_value, &count, &def, nids, 0);
    for (i = count; i < nids; i++) {
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
        min_input = range[0] - 1;               // 1-based
    if (count >= 2)
        max_input = range[1];            // max is actually end+1
    if (min_input < 0)
        min_input = 0;
    if (max_input > nids)
        max_input = nids;

    // Check the INTERP parameter.
    do_interp = zvptst("INTERP");

    snprintf(msg, MSG_SIZE, "Mosaic's surface model parameters are specified "
             "in the %s coordinate frame", 
             surface_model->getCoordSystem()->getFrameName());
    if (do_print)
        zvmessage(msg, "");

    // Report coordinate systems in use

    surface_model->setCoordSystem(proj_cs);

    azdir = proj_cs->getAzimuthDirection();
    snprintf(msg, MSG_SIZE, "Mosaic is projected in the %s coordinate frame",
             proj_cs->getFrameName());
    zvmessage(msg, "");

    // Print out input status from labels

    mars_print_inputs(nids, pointing_in, camera_in, file_models,
            homogeneous_inputs, mission, instrument);

    ////////////////////////////////////////////////////////////////////
    // Get the ring parameters.  There is a chicken-and-egg problem here in that
    // we have to know the ring params in order to determine the extent of the
    // mosaic, but we need the output camera models to properly determine the
    // ring.  The output camera models can't be created until we determine the
    // extent of the mosaic!  So, we assume a full 360 degree output here,
    // create output camera models, and use that to figure the ring.  Then
    // later we re-create the output models once all the params are set.
    // Similar issues exist for proj_el, which should be used to construct the
    // output models but can't be determined until later.  So for the ring
    // determination, we use elevation 0, which is probably more correct anyway
    // (doesn't matter except for the ring center).
    ////////////////////////////////////////////////////////////////////

    // Set up the temp output camera models
    int temp_nso = (int)(PigDeg2Rad(360) / camera_out->getPixelAngle(1));
    if (temp_nso > MAX_COLUMNS) {
        temp_nso = MAX_COLUMNS;     // really doesn't matter, so allow in case
                                    // it's not a full 360 mosaic
    }

    for (i=0; i < temp_nso; i++) {
        // Point the main camera, then clone it for the output array
        double az = azdir * i * camera_out->getPixelAngle(1); // radians from 0

        output_direction = proj_cs->constructVector(az, 0.0);
        pointing_out->setCameraOrientation(output_direction, proj_cs);

        output_cameras[i] = camera_out->clone();
        if (output_cameras[i] == NULL) {
            zvmessage("Memory error cloning cameras!!", "");
            zabend();
        }
    }

    // Determine the ring parameters

    // First repoint 3 times, 120 deg apart, to establish the initial ring
    // estimate.  Since we don't know proj_el yet, we use elevation=0.

    PigPoint ring_guess[3];
    output_direction = proj_cs->constructVector(0.0, 0.0);
    pointing_out->setCameraOrientation(output_direction, proj_cs);
    ring_guess[0] = camera_out->getCameraPosition();
    output_direction = proj_cs->constructVector(PigDeg2Rad(120.0), 0.0);
    pointing_out->setCameraOrientation(output_direction, proj_cs);
    ring_guess[1] = camera_out->getCameraPosition();
    output_direction = proj_cs->constructVector(PigDeg2Rad(240.0), 0.0);
    pointing_out->setCameraOrientation(output_direction, proj_cs);
    ring_guess[2] = camera_out->getCameraPosition();

    int write_ring = FALSE;
    status = getRingParams(temp_nso, output_cameras, ring_guess,
            ring_center, ring_axis, ring_radius);

    if (status == 0) {
        write_ring = TRUE;
        snprintf(msg, MSG_SIZE, "Ring center actual = (%f, %f, %f), radius = %f, "
                 "baseline = %f", ring_center.getX(), ring_center.getY(), 
                 ring_center.getZ(), ring_radius, ring_radius * 2);
        zvmessage(msg, "");
        snprintf(msg, MSG_SIZE, "Ring axis actual = (%f, %f, %f)",
                 ring_axis.getX(), ring_axis.getY(), ring_axis.getZ());
        zvmessage(msg, "");
    }

    for (i = 0; i < temp_nso; i++) {
        // Free these temp cameras
        delete output_cameras[i];
    }

    // Check for ring overrides.  This can be used to adjust the disparity,
    // at least if the surface model is close to accurate.  It should work
    // well in approximately the same cases as the "untilt" works.
    // Note, these are all expressed in the camera (rover) coord system.

    double baseline;
    zvparmd("BASELINE", &baseline, &count, &def, 1, 0);
    ring_rad_ovr = baseline/2.0;
    if (count == 1) {
        do_ring_rad_ovr = TRUE;
        snprintf(msg, MSG_SIZE, "Baseline OVERRIDE = %f, Ring Radius = %f",
                 baseline, ring_rad_ovr);
        zvmessage(msg, "");
    }

    // Check for untilt
    ring_axis_ovr = ring_axis;
    if (zvptst("UNTILT")) {
        PigVector v(0,0,1);        // Vertical
        ring_axis_ovr = proj_cs->convertVector(v, site_cs);
        do_ring_ovr = TRUE;
        snprintf(msg, MSG_SIZE, "Untilt mode setting axis to: (%f, %f, %f\n", 
                 ring_axis_ovr.getX(), ring_axis_ovr.getY(),
                 ring_axis_ovr.getZ());
        zvmessage(msg, "");
    }

    double tmp_dbl[3];
    zvparmd("RING_AXIS", tmp_dbl, &count, &def, 3, 0);
    if (count == 3) {
        do_ring_ovr = TRUE;
        ring_axis_ovr.setXYZ(tmp_dbl);
        snprintf(msg, MSG_SIZE, "Ring Axis OVERRIDE = (%f, %f, %f)",
                 tmp_dbl[0], tmp_dbl[1], tmp_dbl[2]);
        zvmessage(msg, "");
    }

    ring_ctr_ovr = ring_center;
    zvparmd("RING_CENTER", tmp_dbl, &count, &def, 3, 0);
    if (count == 3) {
        do_ring_ovr = TRUE;
        ring_ctr_ovr.setXYZ(tmp_dbl);
        snprintf(msg, MSG_SIZE, "Ring Center OVERRIDE = (%f, %f, %f)",
                 tmp_dbl[0], tmp_dbl[1], tmp_dbl[2]);
        zvmessage(msg, "");
    }

    PigQuaternion ring_rot;
    if (do_ring_ovr) {
        // Precompute rotations
        PigVector rot_axis = ring_axis * ring_axis_ovr;
        rot_axis.normalize();
        double rot_ang = acos(ring_axis % ring_axis_ovr);
        ring_rot = PigQuaternion(rot_axis, rot_ang);
    }

    ////////////////////////////////////////////////////////////////////
    // Determine the min and max azimuth and elevation.  This is only
    // approximate, but that's okay since it is only used to determine
    // the output size.  In order to do this, we have to create a copy
    // of the *input* camera models and transform them via the ring
    // parameters.  Note that we transform by the *inverse* of the
    // quaternion because we're adjusting the inputs, not the outputs.
    //!!!! It is unclear whether the center and radius adjustments work
    //!!!! here... or if they need some analogous adjustment.
    // We use the combined list to ensure we do the same thing for both eyes.
    ////////////////////////////////////////////////////////////////////

    for (i = 0; i < c_nids; i++) {
        camera_in_rot[i] = c_camera_in[i]->clone();

        rotateCameraModel(camera_in_rot[i],
                do_ring_ovr, ring_center, ring_ctr_ovr, ~ring_rot,
                do_ring_rad_ovr, ring_rad_ovr);
    }

    // Use the rotated camera models to get the azel minmax.  We do not
    // use these rotated models again.  We want to get el in the proj_cs
    // frame but we want az in the start_az_cs frame, so we just call the
    // routine twice.

    double wrap_az = 0.0, dbl_param;
    zvparmd("WRAP_AZ", &dbl_param, &count, &def, 1, 0);
    if (count > 0) {

        PigCoordSystem *wrap_cs = start_az_cs;
        char wrap_coord[256];

        wrap_az = PigDeg2Rad(dbl_param);

        double wrap_el = 0.0;
        zvparmd("WRAP_EL", &dbl_param, &count, &def, 1, 0);
        wrap_el = PigDeg2Rad(dbl_param);

        zvp("WRAP_COORD", wrap_coord, &count);
        if (count != 0) {
            wrap_cs = m->getCoordSystem(file_models[proto], wrap_coord);
            if (wrap_cs == NULL) {
                snprintf(msg, MSG_SIZE, "Invalid WRAP_COORD: %s", wrap_coord);
                zvmessage(msg, "");
                zabend();
            }
            snprintf(msg, MSG_SIZE, "Interpreting WRAP_COORD in the %s frame, "
                     "with azimuth %f and elevation %f", wrap_cs->getFrameName(), 
                     PigRad2Deg(wrap_az), PigRad2Deg(wrap_el));
            if (do_print)
                zvmessage(msg, "");
        }

        if (wrap_cs != start_az_cs) {

                double wrap_az_new, wrap_el_new;
                snprintf(msg, MSG_SIZE, "Frames are not the same. Converting "
                         "wrap to %s", start_az_cs->getFrameName());
                if (do_print)
                    zvmessage(msg, "");

                start_az_cs->convertAzEl(wrap_az, wrap_el, wrap_az_new,
                        wrap_el_new, wrap_cs);
                wrap_az = wrap_az_new;
                wrap_el = wrap_el_new;

        }
    }

    mars_get_azel_minmax(c_nids, c_pointing_in, camera_in_rot, c_file_models,
            min_elev, max_elev, min_az, max_az, proj_cs, wrap_az);

    double ctr_el_proj_cs = (min_elev+max_elev)/2.;     // save for later
    double ctr_az_proj_cs = (min_az+max_az)/2.;		// save for later
    if (max_az < min_az)		// wrapped!
	ctr_az_proj_cs += PigDeg2Rad(180);

    double dummy1, dummy2;
    mars_get_azel_minmax(c_nids, c_pointing_in, camera_in_rot, c_file_models,
            dummy1, dummy2, min_az, max_az, start_az_cs, wrap_az);

    zvparmd("LIMIT_BOTTOMEL", &dbl_param, &count, &def, 1, 0);
    if (count > 0) {
	double limit = PigDeg2Rad(dbl_param);
	if (min_elev < limit) {
	    if (do_print)
		zvnprintf(256, "Elevation Minimum of %f limited to %f",
			PigRad2Deg(min_elev), PigRad2Deg(limit));
	    min_elev = limit;
	}
    }

    zvparmd("LIMIT_TOPEL", &dbl_param, &count, &def, 1, 0);
    if (count > 0) {
	double limit = PigDeg2Rad(dbl_param);
	if (max_elev > limit) {
	    if (do_print)
		zvnprintf(256, "Elevation Maximum of %f limited to %f",
			PigRad2Deg(max_elev), PigRad2Deg(limit));
	    max_elev = limit;
	}
    }

    zvparmd("TOPEL", &dbl_param, &count, &def, 1, 0);
    if (count > 0) {
	max_elev = PigDeg2Rad(dbl_param);
	if (do_print)
	    zvnprintf(256, "Elevation Maximum overridden to %f",
			PigRad2Deg(max_elev));
    }

    zvparmd("BOTTOMEL", &dbl_param, &count, &def, 1, 0);
    if (count > 0) {
	min_elev = PigDeg2Rad(dbl_param);
	if (do_print)
	    zvnprintf(256, "Elevation Minimum overridden to %f",
			PigRad2Deg(min_elev));
    }

    snprintf(msg, MSG_SIZE, "proj_cs: el = %f %f az = %f %f\n", PigRad2Deg(min_elev),
             PigRad2Deg(max_elev), PigRad2Deg(min_az), PigRad2Deg(max_az));
    zvmessage(msg, "");

    // Set output picture dimensions.  Use the pixel scale of the output camera
    // Also determine the projection parameters (starting az for horizontal,
    // projection el for vertical (where the camera is pointing)).

    // Azimuth first

    nso_360 = (int)(PigDeg2Rad(360) / camera_out->getPixelAngle(1));
    if (full_frame) {
        nso = (int)(PigDeg2Rad(360) / camera_out->getPixelAngle(1));
        if (azdir == 1) {
            start_az = 0.0;
            stop_az = PigDeg2Rad(360);
        } else {
            start_az = PigDeg2Rad(360);
            stop_az = 0.0;
        }
    } else {
        snprintf(msg, MSG_SIZE, "minimum az = %f, maximum az = %f",
                 PigRad2Deg(min_az), PigRad2Deg(max_az));
        zvmessage(msg, "");
        if (min_az < max_az) {
            nso = (int)((max_az - min_az) / camera_out->getPixelAngle(1));
        } else {
            nso = (int)((max_az + PigDeg2Rad(360.) - min_az) /
                    camera_out->getPixelAngle(1));
        }
        if (azdir == 1) {
            start_az = min_az;
            stop_az = max_az;
        } else {
            start_az = max_az;
            stop_az = min_az;
        }
    }

    // Now calculate elevation size.  Top and bottom are calculated
    // independently because the size depends on the elevation w.r.t.
    // the projection center.

    proj_el = (min_elev + max_elev) / 2.0;
    snprintf(msg, MSG_SIZE, "Projection elevation = %f", PigRad2Deg(proj_el));
    zvmessage(msg, "");

    zvparmd("PROJ_EL", &dbl_param, &count, &def, 1, 0);
    if (count != 0) {
        proj_el = PigDeg2Rad(dbl_param);
        snprintf(msg, MSG_SIZE, "Projection elevation OVERRIDE = %f", PigRad2Deg(proj_el));
        zvmessage(msg, "");
    }

    snprintf(msg, MSG_SIZE, "min_el=%f, max_el=%f, proj_el=%f", PigRad2Deg(min_elev),
             PigRad2Deg(max_elev), PigRad2Deg(proj_el));
    zvmessage(msg, "");
    top_nl = tan(max_elev - proj_el) / camera_out->getPixelAngle(0);
    bottom_nl = tan(min_elev - proj_el) / camera_out->getPixelAngle(0);
    top_nl += border_edge/2;
    bottom_nl -= border_edge/2;

    // Set proj_line
    proj_line = top_nl;
    snprintf(msg, MSG_SIZE, "Projection line (matches projection elevation) = %f",
             proj_line);
    zvmessage(msg, "");
    zvparmd("PROJ_LINE", &dbl_param, &count, &def, 1, 0);
    if (count != 0) {
        proj_line = dbl_param;
        snprintf(msg, MSG_SIZE, "Projection line OVERRIDE = %f", proj_line);
        zvmessage(msg, "");
    }
    
    nlo = (int)(top_nl - bottom_nl);

    snprintf(msg, MSG_SIZE, "top_nl=%f, bottom_nl=%f, nlo=%d", top_nl, bottom_nl, nlo);
    zvmessage(msg, "");

    snprintf(msg, MSG_SIZE, "nlout = %d, nsout = %d", nlo, nso);
    zvmessage(msg, "");

    // Check for image size override

    zvpcnt("outsize", &count);
    if (count > 0) {
        int size[2];
        zvp("outsize", size, &count);
        nlo = size[0];
        nso = size[1];
        snprintf(msg, MSG_SIZE, "Override: Output lines & samples=%10d %10d", nlo, nso);
        zvmessage(msg, "");
    }

    if (nso > MAX_COLUMNS) {
        zvmessage("Number of samples exceeds MAX_COLUMNS!  Unable to project at this pixel scale", "");
        zabend();
    }

    // Handle the start_az override.  Allowing different coord systems here is
    // a little complicated because we can't convert coord systems using an
    // azimuth alone.  So we construct a vector at azimuth 0 and the projection
    // elevation, convert that to the start_az_cs, replace azimuth, and convert
    // back.

    snprintf(msg, MSG_SIZE, "Starting azimuth = %f in %s frame", PigRad2Deg(start_az),
            start_az_cs->getFrameName());
    zvmessage(msg, "");

    zvparmd("START_AZ", &dbl_param, &count, &def, 1, 0);
    if (count != 0) {
        start_az = PigDeg2Rad(dbl_param);
        snprintf(msg, MSG_SIZE, "Starting azimuth OVERRIDE = %f in %s frame",
                 PigRad2Deg(start_az), start_az_cs->getFrameName());
        zvmessage(msg, "");
    }

    // Adjust starting azimuth for the disparity pixel delta

    zvp("DISP_PIX", &disp_pix, &count);
    start_az += disp_pix * camera_out->getPixelAngle(1);
    if (start_az < 0.0)
        start_az += PigDeg2Rad(360.0);
    if (start_az > PigDeg2Rad(360.0))
        start_az -= PigDeg2Rad(360.0);

    if (disp_pix != 0) {
        snprintf(msg, MSG_SIZE, "Starting azimuth adjusted for DISP_PIX = %f",
                 PigRad2Deg(start_az));
        zvmessage(msg, "");
    }

    // Compute stop_az in same CS

    stop_az = start_az + azdir * nso * camera_out->getPixelAngle(1);
    if (stop_az < 0.0)
        stop_az += PigDeg2Rad(360.0);
    if (stop_az > PigDeg2Rad(360.0))
        stop_az -= PigDeg2Rad(360.0);

    // Convert start_az and stop_az to both site and rover coord sys.
    // Check to see which conversions are actually needed.  Likely only
    // one will be.  This is also complicated due to the need for an
    // elevation.  El=0 was tried but performed poorly with e.g. Mastcam
    // when looking down.  So we use the center az/el from the minmax
    // calculation above, converted to the proper frame.

    // Site frame versions first

    if (start_az_cs == site_cs) {
        start_az_site = start_az;        // already in site frame
        stop_az_site = stop_az;
    } else {
	PigVector rvr_vec = proj_cs->constructVector(ctr_az_proj_cs, ctr_el_proj_cs);
	PigVector site_vec = site_cs->convertVector(rvr_vec, proj_cs);
        PigVector az_vec = start_az_cs->convertVector(site_vec, site_cs);
        az_vec.setAz(start_az);
        site_vec = site_cs->convertVector(az_vec, start_az_cs);
        start_az_site = site_vec.getAz();
        az_vec.setAz(stop_az);
        site_vec = site_cs->convertVector(az_vec, start_az_cs);
        stop_az_site = site_vec.getAz();
    }

    // Rover frame version.  Note, we convert in-place so there's nothing
    // to do if the CS's match.

    if (start_az_cs != proj_cs) {
	PigVector rvr_vec = proj_cs->constructVector(ctr_az_proj_cs, ctr_el_proj_cs);
        PigVector az_vec = start_az_cs->convertVector(rvr_vec, proj_cs);
        az_vec.setAz(start_az);
        rvr_vec = proj_cs->convertVector(az_vec, start_az_cs);
        start_az = rvr_vec.getAz();
        az_vec.setAz(stop_az);
        rvr_vec = proj_cs->convertVector(az_vec, start_az_cs);
        stop_az = rvr_vec.getAz();
        az_vec.setAz(stop_az);
    }

    if (start_az < 0.0) start_az += PigDeg2Rad(360.0);
    if (start_az > PigDeg2Rad(360.0)) start_az -= PigDeg2Rad(360.0);

    if (start_az_site < 0.0) start_az_site += PigDeg2Rad(360.0);
    if (start_az_site > PigDeg2Rad(360.0)) start_az_site -= PigDeg2Rad(360.0);

    if (stop_az < 0.0) stop_az += PigDeg2Rad(360.0);
    if (stop_az > PigDeg2Rad(360.0)) stop_az -= PigDeg2Rad(360.0);

    if (stop_az_site < 0.0) stop_az_site += PigDeg2Rad(360.0);
    if (stop_az_site > PigDeg2Rad(360.0)) stop_az_site -= PigDeg2Rad(360.0);

    snprintf(msg, MSG_SIZE, "Starting azimuth in projection frame = %f, Site = %f",
             PigRad2Deg(start_az), PigRad2Deg(start_az_site));
    zvmessage(msg, "");

    snprintf(msg, MSG_SIZE, "Ending azimuth in projection frame = %f, Site = %f",
             PigRad2Deg(stop_az), PigRad2Deg(stop_az_site));
    zvmessage(msg, "");

    // Check to see if we should do only a piece of the image
    zvp("MINSAMP", &minsamp, &count);
    if (count == 0)
        minsamp = 0;
    zvp("MAXSAMP", &maxsamp, &count);
    if (count == 0)
        maxsamp = nso;

    snprintf(msg, MSG_SIZE, "Starting azimuth = %f, Projection elevation = %f",
             PigRad2Deg(start_az), PigRad2Deg(proj_el));
    zvmessage(msg, "");

    // Save the initial camera model for the labels.  Point it to azimuth 0
    // first to aid in reconstructing other models later.

    output_direction = proj_cs->constructVector(0.0, proj_el);
    pointing_out->setCameraOrientation(output_direction, proj_cs);
    PigCameraModel *camera_out_copy = camera_out->clone();

    ////////////////////////////////////////////////////////////////////
    // (Re-)Create the initial output camera models.  There is one of these per
    // pixel column in the output.  Also adjust the models for the ring.
    ////////////////////////////////////////////////////////////////////

    for (i = 0; i < nso; i++) {
        // Point the main camera, then clone it for the output array

        double az = azdir * i * camera_out->getPixelAngle(1); // radians from 0
        az += start_az;                    // radians from start
        if (az >= PigDeg2Rad(360.0)) az -= PigDeg2Rad(360.0);
        if (az < 0) az += PigDeg2Rad(360.0);

        output_direction = proj_cs->constructVector(az, proj_el);
        pointing_out->setCameraOrientation(output_direction, proj_cs);

        output_cameras[i] = camera_out->clone();
        if (output_cameras[i] == NULL) {
            zvmessage("Memory error cloning cameras!!", "");
            zabend();
        }

        // Rotate for the ring

        rotateCameraModel(output_cameras[i],
                do_ring_ovr, ring_center, ring_ctr_ovr, ring_rot,
                do_ring_rad_ovr, ring_rad_ovr);

        // Get coordinates of camera center (used for calculating vertical
        // line number).  Combine with offset of 0 elevation point (calculated
        // earlier as proj_lline) to get the adjustment used to compute Camera
        // Coord line # from physical line #.
        // Also remember the center in the samp direction (that's what we'll
        // always use with this camera for sample).

        double y_center, x_center;

	// What we really want here is the line/sample that the constructed
	// pointing vector points to.  That's what the "center" really is here.
	// The assumption later is that when we take a mosaic line/sample
	// and convert it to output camera coords (using x/y_offset), the
	// camera coord line/sample matches the pointing vector used to
	// construct the model.  To do that we project the pointing vector
	// into the output model and use that as the camera "center".  Note
	// that we project from infinity, so it's ignoring projection effects.

	output_cameras[i]->XYZtoLS(output_direction, TRUE,
					&y_center, &x_center, proj_cs);

        y_offset[i] = y_center - proj_line;    // plus line number
        x_offset[i] = x_center;
    }

    // Reset new values for label.  Don't reset ring_axis because we need
    // both old and new.

    if (do_ring_ovr)
        ring_center = ring_ctr_ovr;
    if (do_ring_rad_ovr)
        ring_radius = ring_rad_ovr;

    // Echo final geometry params in cut-and-paste format

    zvmessage("Projection Geometry Parameters (not incl ring overrides):","");
    snprintf(msg, MSG_SIZE, "START_AZ=%f PROJ_EL=%f PROJ_LINE=%f OUTSIZE=\\(%d %d\\)",
             PigRad2Deg(start_az_site), PigRad2Deg(proj_el), proj_line, nlo,
             nso);
    if (zvptst("UNTILT"))
        strcat(msg, " -untilt");
    zvmessage(msg, "");

    // Set up projection parameters structure.  Used for label writing and
    // footprints.

    proj.camera_in = camera_in;
    proj.surface_model = surface_model;
    proj.output_cameras = output_cameras;
    proj.camera_out_copy = camera_out_copy;
    proj.x_offset = x_offset;
    proj.y_offset = y_offset;
    proj.nso = nso;
    proj.nso_360 = nso_360;
    proj.start_az = start_az;
    proj.stop_az = stop_az;
    proj.proj_el = proj_el;
    proj.proj_line = proj_line;
    proj.cs = proj_cs;
    proj.azdir = azdir;
    proj.ring_center = ring_center;

    // Create the output file

    zvselpi(0);                // don't transfer any labels
    zvunit(&unit_out, "OUT", 1, NULL);
    zvopen(unit_out, "OP", "WRITE", "U_FORMAT", "REAL",
            "U_NS", nso, "U_NL", nlo, "OPEN_ACT", "AS",
           "U_NB", band_count, "U_ORG", "BSQ", "O_FORMAT", format, NULL);

    // Write the output labels
    PigLabelModel *labelModel = m->createLabelModel(unit_out);
    labelModel->setMcauley(file_models,
            radiometric, brt_corr, bias_ptr, NULL, nids, 
            surface_model, camera_out_copy,
            start_az, stop_az, proj_el, proj_line, proj_cs,
            write_ring, ring_center, ring_axis, ring_axis_ovr,
            ring_radius, start_az_site, stop_az_site, site_cs);
    zvplabel(unit_out, 0, 1);

    // Allocate buffers write output as zero's
    for (int b = 0; b < band_count; b++) {
        obuf_f[b] = new float[nso];
        if (obuf_f[b] == NULL) {
            zvmessage("Unable to allocate obuf memory!", "");
            zabend();
        }

        memset(obuf_f[b], 0, nso*sizeof(float));
        for(j = 0; j < nlo; j++) 
            status = zvwrit(unit_out, obuf_f[b], "LINE", j+1, "BAND", b+1,
                        NULL);
    }

    // reopen output for update
    zvclose(unit_out, NULL);
    zvopen(unit_out, "OP", "UPDATE", "U_FORMAT", "REAL",
           "U_NS", nso, "U_NL", nlo, "OPEN_ACT", "AS",
           "U_NB", band_count, "U_ORG", "BSQ", "O_FORMAT", format, NULL);


    // Create the output IDX & ICM files if needed

    char idx_filename[PIG_MAX_FILENAME_SIZE] = { '\0' };
    char icm_filename[PIG_MAX_FILENAME_SIZE] = { '\0' };
    zvp("IDX_OUT", idx_filename, &count);
    bool do_idx = (count != 0);
    zvp("ICM_OUT", icm_filename, &count);
    bool do_icm = (count != 0);

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

    // Write the idx/icm output labels
    PigLabelModel *idxLabel = NULL;
    if (do_idx) {
        idxLabel = m->createLabelModel(unit_idx_out);
        idxLabel->setMcauley(file_models,
            radiometric, brt_corr, bias_ptr, NULL, nids, 
            surface_model, camera_out_copy,
            start_az, stop_az, proj_el, proj_line, proj_cs,
            write_ring, ring_center, ring_axis, ring_axis_ovr,
            ring_radius, start_az_site, stop_az_site, site_cs);
        idxLabel->setDerivedImageType("IDX_MAP");
        zvplabel(unit_idx_out, 0, 1);
    }
    PigLabelModel *icmLabel = NULL;
    if (do_icm) {
        icmLabel = m->createLabelModel(unit_icm_out);
        icmLabel->setMcauley(file_models,
            radiometric, brt_corr, bias_ptr, NULL, nids, 
            surface_model, camera_out_copy,
            start_az, stop_az, proj_el, proj_line, proj_cs,
            write_ring, ring_center, ring_axis, ring_axis_ovr,
            ring_radius, start_az_site, stop_az_site, site_cs);
        icmLabel->setDerivedImageType("ICM_MAP");
        zvplabel(unit_icm_out, 0, 1);
    }

    // Allocate buffers and write idx/icm outputs as zeros
    if (do_idx) {
        obuf_idx = new short int[nso];
	if (obuf_idx == NULL) {
            zvmessage("Unable to allocate obuf-idx memory!","");
	    zabend();
	}
	memset(obuf_idx, 0, nso*sizeof(short int));
	for (int j=0; j<nlo; j++)
	    zvwrit(unit_idx_out, obuf_idx, "LINE", j+1, NULL);
    }
    if (do_icm) {
        obuf_icm_l = new float[nso];
        obuf_icm_s = new float[nso];
	if ((obuf_icm_l == NULL) || (obuf_icm_s == NULL)) {
            zvmessage("Unable to allocate obuf-icm memory!","");
	    zabend();
	}
	memset(obuf_icm_l, 0, nso*sizeof(float));
	memset(obuf_icm_s, 0, nso*sizeof(float));
	for (int j=0; j<nlo; j++) {
	    zvwrit(unit_icm_out, obuf_icm_l, "LINE", j+1, "BAND", 1,
        						NULL);
	    zvwrit(unit_icm_out, obuf_icm_s, "LINE", j+1, "BAND", 2,
							NULL);
        }
    }

    // reopen idx/icm outputs if requested
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


    // Calculate the approximate Field Of View for each input, for use in
    // a quick decision whether to project the point into this image or not.
    // Adding H and V FOV's covers the diagonals, plus provides some slop.
    // We divide by 2 because the H and V FOV's cover from edge to edge, while
    // we need only from the center to the edge.  The cosine is stored for
    // easy comparison with a dot product.  We limit the FOV to 90 degrees
    // (180 total) (really 89.4) to avoid any back-projection (MER hazcam H+V
    // is > 90).  But we make sure we have at least 30 degrees to accommodate
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
	fov_limit[0] = 0.01;	// 89.4 degress; really 90 is an infinity case
	fov_limit[1] = 0.866;	// 30 degrees
    }
    zvnprintf(256, "FOV limits set to %f, %f degrees",
		PigRad2Deg(acos(fov_limit[0])), PigRad2Deg(acos(fov_limit[1])));

    for (i = 0; i < nids; i++) {
        FOV[i] = cos((file_models[i]->getFOV(camera_in[i], 0) +
                file_models[i]->getFOV(camera_in[i], 1)) / 2);
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

    //Create and initilize variables necessary for pass loop
    int first_input, last_input;

    // Make a number of passes, processing MAX_OPEN inputs each pass
    for (first_input = min_input; first_input < max_input;
                first_input += MAX_OPEN) {
        snprintf(msg, MSG_SIZE, "Pass %d of %d", (first_input / MAX_OPEN) + 1,
                 ((max_input - 1) / MAX_OPEN) + 1);
        zvmessage(msg, "");

        last_input = first_input + MAX_OPEN - 1;
        if (last_input >= max_input)
            last_input = max_input - 1;

        // Read a set of inputs into memory
        mars_read_inputs(first_input, last_input, file_models, float_images, 
                         MAX_NL, MAX_NS, band, radiometric, brt_corr);

        // Loop through each sample of the output, and pick up the input
        // pixel that corresponds to it

        for (j = 0; j < nlo; j++) {

            if (j % 100 == 0) {
                snprintf(msg, MSG_SIZE, "line %d", j);
                zvmessage(msg, "");
            }

            // re-read output line
            for (int b = 0; b < band_count; b++) {
                zvread(unit_out, obuf_f[b], "LINE", j+1, "BAND", b+1, NULL);
            }
	    if (do_idx) {
	        zvread(unit_idx_out, obuf_idx, "LINE", j+1, NULL);
	    }
	    if (do_icm) {
	        zvread(unit_icm_out, obuf_icm_l, "LINE", j+1, "BAND", 1, NULL);
		zvread(unit_icm_out, obuf_icm_s, "LINE", j+1, "BAND", 2, NULL);
	    }

            for (i = minsamp; i < maxsamp; i++) {
                int continue_flag = 0;
                for (int b = 0; b < band_count; b++) {
                    if (obuf_f[b][i] != 0) {
                        // already set, do next iteration
                        continue_flag = 1;
                        break;
                    }
                }
                if (continue_flag)
                    continue;

                // Compute the camera-coordinate line and sample, and project
                // into space and then XYZ.  The sample is always the center
                // of the camera (looking right down the view vector).

                double out_samp = x_offset[i];
                double out_line = (double)j + y_offset[i];

                PigPoint origin;
                PigVector look;
                PigPoint surf_pt;

                output_cameras[i]->LStoLookVector(out_line, out_samp, origin,
                                                  look, proj_cs);
                int hits = surface_model->intersectRay(origin, look, surf_pt);
                int infinity = (hits <= 0);
    
                for (k = first_input; k <= last_input; k++) {    
                    // picture loop
                    int kp = k - first_input;
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
                    } else {
                        PigVector new_look = surf_pt - camera_position[k];
                        new_look.normalize();
                        if ((new_look % camera_orientation[k]) < FOV[k])
                            continue;
                    }

                    // convert output image coordinate to camera coordinates
                    // in the input image
    
                    double in_line, in_samp;
                    camera_in[k]->XYZtoLS(surf_pt, infinity, &in_line, &in_samp,
                                            proj_cs);

                    // check if point is within the input image

                    if (file_models[k]->testPixelLocation(in_line,in_samp) != 0)
                        continue; // skip if not

                    // Convert to phys image coords (compensate for sub-areas)
                    double image_line = in_line - file_models[k]->getYOffset();
                    double image_samp = in_samp - file_models[k]->getXOffset();

                    double dn;

                    if (do_interp) {
                        // interpolate in the input image 
                        // (bilinear interpolation)

                        int m = (int)image_samp;
                        int n = (int)image_line;
    
                        double wr = image_samp - m;
                        double wl = 1.0 - wr;
                        double wb = image_line - n;
                        double top, bot;

                        SimpleImage<float> *img = float_images[kp];
                        double ul, ur, ll, lr;
                        int bb;
                        for (int b = 0; b < band_count; b++) {
                            bb = b;
                            if (bb >= img->getNB())
                                bb = img->getNB() - 1;
                            ul = img->get(bb, n, m);
                            ur = img->get(bb, n, m + 1);
                            ll = img->get(bb, n + 1, m);
                            lr = img->get(bb, n + 1, m + 1);
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
                            if (lr == 0.0) lr = ul;
    
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
                                if (dn < 0.0) dn = 0.0;
                                if (dn > 32766.0) dn = 32766.0;
                            }
                            obuf_f[b][i] = dn;
                        }
                    } else {
                        // Don't interpolate
                        SimpleImage<float> *img = float_images[kp];
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
                                if (dn < 0.0) dn = 0.0;
                                if (dn > 32766.0) dn = 32766.0;
                            }
                            obuf_f[b][i] = dn;
                        }
                    }

		    // Save index and ICM data

		    if (do_idx)
			obuf_idx[i] = k+1;		// 1-based index
		    if (do_icm) {
			obuf_icm_l[i] = image_line + 1;	// 1-based
			obuf_icm_s[i] = image_samp + 1;	// 1-based
		    }

                    int break_flag = 0;
                    for (int b=0; b < band_count; b++) {
                        if (obuf_f[b][i] != 0.0) {
                            // skip the rest of the pictures
                            break_flag = 1;
                            break;
                        }
                    }
                    if (break_flag)
                        break;    // skip rest of pictures
                } // picture loop
            } // sample loop

            for (int b = 0; b < band_count; b++) {
                zvwrit(unit_out, obuf_f[b], "LINE", j+1, "BAND", b+1, NULL);
            }
	    if (do_idx)
		zvwrit(unit_idx_out, obuf_idx, "LINE", j+1, NULL);
	    if (do_icm) {
		zvwrit(unit_icm_out, obuf_icm_l, "LINE", j+1, "BAND", 1, NULL);
		zvwrit(unit_icm_out, obuf_icm_s, "LINE", j+1, "BAND", 2, NULL);
	    }
        } // line loop
    } // pass loop
    
    // No need to deallocate input arrays anymore

    // Add footprints/numbering if requested
    // mars_footprints will reopen file and close it when it's done.
    zvclose(unit_out, NULL);
    mars_footprints(unit_out, nids, nso, nlo, band_count, camera_in,
            file_models, mcauley_proj_inverse, &proj);

    // Do bounding boxes

    mars_mosaic_bbox(nids, camera_in, file_models, nlo, nso,
		mcauley_proj_inverse, mcauley_width_func, &proj);

    zvclose(unit_idx_out, NULL);
    zvclose(unit_icm_out, NULL);
}

////////////////////////////////////////////////////////////////////////
// Inverse projection routine (input -> output) for use by footprinter
////////////////////////////////////////////////////////////////////////

#define NUM_TRIALS 50

extern "C" int mcauley_proj_inverse(double in_line, double in_samp,
                                    double *out_line, double *out_samp,
                                    int input_number, void *proj_args)
{
    McauleyProjArgs *proj = (McauleyProjArgs *)proj_args;
    PigPoint origin;
    PigVector look;
    PigPoint surf_pt;
    int hits;

    // This is complicated because we don't know which camera model to use.
    // Project once using a guess, then project again if it indicates a
    // different model.  We set an iteration limit of NUM_TRIALS.

    proj->camera_in[input_number]->LStoLookVector(in_line, in_samp, origin,
                                                    look, proj->cs);
    hits = proj->surface_model->intersectRay(origin, look, surf_pt);

    // Now get the az from the vector from the ring center to that point.
    // Unless we missed the surface model, in which case we use the look
    // direction (i.e. infinity).  Use this to choose the model

    PigPoint direction = look;
    if (hits > 0) {
	direction = surf_pt - proj->ring_center;
    }

    double az = proj->cs->getAz(direction);
    az -= proj->start_az;
    az *= proj->azdir;
    if (az >= PigDeg2Rad(360.0)) az -= PigDeg2Rad(360.0);
    if (az < 0) az += PigDeg2Rad(360.0);
    int az_column = (int)(az / proj->output_cameras[0]->getPixelAngle(1));

    int old_az_column = az_column + 1;    // to ensure it's different 1st time

    for (int trial = 0; trial < NUM_TRIALS; trial++) {

        if (old_az_column == az_column) {
            *out_samp = az_column;    // give up, we tried this already
            return 0;
        }
        old_az_column = az_column;

        // If out of range, try to wrap around, otherwise set to the limit
    
        if (az_column < 0) {
            if (az_column + proj->nso_360 < proj->nso) {
		while (az_column < 0) {
                    az_column += proj->nso_360;
		}
            } else
                az_column = 0;
        }
        if (az_column >= proj->nso) {
            if (az_column - proj->nso_360 >= 0) {
		while (az_column >= proj->nso_360) {
                    az_column -= proj->nso_360;
		}
            } else
                az_column = proj->nso-1;
        }

        proj->output_cameras[az_column]->XYZtoLS(surf_pt, (hits <= 0),
                                                out_line, out_samp, proj->cs);
        *out_line -= proj->y_offset[az_column];    // convert camera->phys
        *out_samp -= proj->x_offset[az_column];

        if ((int)(*out_samp+0.5) == 0) {
            // found the right model!
            *out_samp = az_column;
            return 0;
        }

        // Get a better output camera model and try again

        az_column += (int)(*out_samp+0.5);
    }

    // Ran out of iterations, return an error

    *out_samp = az_column;
    return 1;
}

////////////////////////////////////////////////////////////////////////
// Mosaic width routines for bounding box
////////////////////////////////////////////////////////////////////////

extern "C" int mcauley_width_func(double line, double samp, double ndeg,
                int *out_pix, double *out_line_zeroel,
		int input_number, void *proj_args)
{
    McauleyProjArgs *proj = (McauleyProjArgs *)proj_args;
    *out_pix = (int)((PigDeg2Rad(ndeg) /
		proj->camera_out_copy->getPixelAngle(1)) + 0.5);

    // Line of zero elevation is approximate here, but that's sufficient
    // for the given purpose.  Take the proj_el and proj_line, and adjust
    // it to el=0 by adding the proj_el / ifov to the line number.  This
    // is approximate because the ifov is not constant in the vertical
    // direction (it is a perspective projection after all), but this suffices
    // for the zenith/nadir check.
 
    *out_line_zeroel = proj->proj_line +
		proj->proj_el / proj->camera_out_copy->getPixelAngle(0);
 
    return 0;

}


////////////////////////////////////////////////////////////////////////
// Compute the center, axis, and radius of the ring of C points.
// Get an inital guess using three equally-spaced points, then refine it
// using amoeba.  There are probably far more efficient ways, but since
// the points should all be on the ring with very little noise, it should
// converge very quickly.  We probably could get by with the initial guess
// but the fitting provides some safety for odd pointing effects if they
// exist.
// Returns 0 if successful, 1 if not.
////////////////////////////////////////////////////////////////////////

int getRingParams(int nso, PigCameraModel **output_cameras,
                    PigPoint ring_guess[3], PigPoint &ring_center,
                    PigVector &ring_axis, double &ring_radius)
{
    RingParams param;
    char msg[MSG_SIZE];

    // Compute the initial guess.  Center is the average

    ring_center = (ring_guess[0] + ring_guess[1] + ring_guess[2]) / 3.0;

    // Axis is the cross-product of vectors from 0->1 and 0->2, normalized

    PigVector v1 = ring_guess[1] - ring_guess[0];
    PigVector v2 = ring_guess[2] - ring_guess[0];
    ring_axis = v1 * v2;
    ring_axis.normalize();

    // Radius is the distance from any point to the center

    PigVector rad = ring_guess[0] - ring_center;
    ring_radius = rad.magnitude();

    snprintf(msg, MSG_SIZE, "Ring center guess  = (%f, %f, %f), radius = %f",
             ring_center.getX(), ring_center.getY(), ring_center.getZ(),
             ring_radius);
    zvmessage(msg, "");
    snprintf(msg, MSG_SIZE, "Ring axis guess  = (%f, %f, %f)",
             ring_axis.getX(), ring_axis.getY(), ring_axis.getZ());
    zvmessage(msg, "");

    // Now prepare amoeba.  The Pzero array is in the order:
    // center.x, center.y, center.z, axis.x, axis.y, axis.z, radius
    // We let axis drift unnormalized during amoeba then renormalize at
    // the end

#define NUM_PARAMS 7
    double Pzero[NUM_PARAMS];
    double lambda[NUM_PARAMS];
    int ndim = NUM_PARAMS;
    int iter;
    double ftol = 0.000001;

    Pzero[0] = ring_center.getX();
    Pzero[1] = ring_center.getY();
    Pzero[2] = ring_center.getZ();
    Pzero[3] = ring_axis.getX();
    Pzero[4] = ring_axis.getY();
    Pzero[5] = ring_axis.getZ();
    Pzero[6] = ring_radius;
    lambda[0] = lambda[1] = lambda[2] = 0.005;      // 5 mm on center
    lambda[3] = lambda[4] = lambda[5] = .01;        // about a half degree
    lambda[6] = .005;                               // 5 mm on radius

    for (int i = 0; i < nso; i++) {
        param.camera_c[i] = output_cameras[i]->getCameraPosition();
    }
    param.nso = nso;

    double residual = amoeba3(Pzero, lambda, ndim, ftol, 1000, &iter,
                                RingObjective, &param);

    ring_center.setXYZ(Pzero[0], Pzero[1], Pzero[2]);
    ring_axis.setXYZ(Pzero[3], Pzero[4], Pzero[5]);
    ring_radius = Pzero[6];

    if (ring_axis.magnitude() == 0 || ring_radius == 0)
        return 1;            // bad

    ring_axis.normalize();

    snprintf(msg, MSG_SIZE, "Amoeba residual=%15.13lf, iterations=%d", 
             residual-1.0, iter);
    zvmessage(msg, "");

    return 0;
}

////////////////////////////////////////////////////////////////////////
// Objective function for determining the ring.  Note, we add 1.0 to
// the error to avoid some instabilities in amoeba when the errors are
// very small.
////////////////////////////////////////////////////////////////////////

extern "C"
double RingObjective(double p[], int mdim, void *func_args)
{
    RingParams *params = (RingParams *)func_args;
    PigPoint ring_center;
    PigVector ring_axis;
    double ring_radius;

    ring_center.setXYZ(p[0], p[1], p[2]);
    ring_axis.setXYZ(p[3], p[4], p[5]);
    ring_axis.normalize();
    ring_radius = p[6];

    // Compute the quaternion needed to "unrotate" the ring axis, i.e. make
    // it vertical.  Cross product of axis and vertical gives us the quaternion
    // axis while acos(dot product) gives us the amount to rotate.

    // Yes, z=+1 means the vector points down.  That follows the right hand
    // rule since azimuths increase clockwise.

    PigVector vertical(0.0, 0.0, 1.0);

    PigVector quat_axis = ring_axis * vertical;
    double quat_angle = acos(ring_axis % vertical);
    PigQuaternion quat;
    if (quat_angle != 0 && quat_axis.magnitude() != 0) {
        quat_axis.normalize();
        quat = PigQuaternion(quat_axis, quat_angle);
    }

    // Go through all the points.  Translate based on the ring center,
    // and rotate using the quat to get a "flat" ring at the origin.  Then
    // the error terms are sqrt(x^2+y^2) - radius and (z-0).

    double cost = 0;
    for (int i = 0; i < params->nso; i++) {
        PigPoint rot_c = quat * (params->camera_c[i] - ring_center);
        double err1 =
                sqrt(rot_c.getX() * rot_c.getX() + rot_c.getY() * rot_c.getY())
                - ring_radius;
        double err2 = rot_c.getZ();
        cost += err1 * err1 + err2*err2;
    }

    return cost + 1.0;
}

////////////////////////////////////////////////////////////////////////
// Rotate a camera model based on the ring params.  Note that this uses
// the camera model's moveCamera() directly in violation of the contract
// (that all pointing should be done via the pointing model).  But, since
// we do not use the pointing model again for these cameras, the fact that
// it's out of sync is irrelevant.  Thus we can use the arbitrary moveCamera()
// to move the output in ways not allowed by the pointing model, in order to
// change the ring.  Ring center change is a simple translation.  Ring axis
// change is a rotation of the entire camera model about the center.  Ring
// radius change is a translation along the radius vector from the center to
// the camera position.  Radius is handled separately from the other two.
////////////////////////////////////////////////////////////////////////

void rotateCameraModel(PigCameraModel *camera,
                        int do_ring_ovr, PigPoint ring_center,
                        PigPoint ring_ctr_ovr, PigQuaternion ring_rot,
                        int do_ring_rad_ovr, double ring_rad_ovr)
{

    if (do_ring_ovr) {        // axis and/or center
        // moveCamera works via an initial and final quaternion and
        // computes the relative rotation between them.  So we use identity
        // as the initial and the precomputed ring_rot as the final.

        camera->moveCamera(ring_center, PigQuaternion(), ring_ctr_ovr,
                            ring_rot, camera->getCoordSystem());
    }
    if (do_ring_rad_ovr) {        // ring radius
        // Vector from (new) center to camera position
        PigVector radvec = camera->getCameraPosition() - ring_ctr_ovr;
        radvec.normalize();        // make a unit vector
        // Move to the new place along the same radius
        PigPoint new_ctr = ring_ctr_ovr + radvec * ring_rad_ovr;
        camera->setCameraPosition(new_ctr, camera->getCoordSystem());
    }
}
