/* marsmos */
#include "vicmain_c"

#include "mars_support.h"

#include "PigMission.h"
#include "PigFileModel.h"
#include "PigLabelModel.h"
#include "PigCameraModel.h"
#include "PigPointingModel.h"
#include "PigSurfaceModel.h"
#include "PigVector.h"
#include "RadiometryModel.h"
#include "PigBrtCorrModel.h"
#include "PigCoordSystem.h"
#include "SimpleImage.h"	

#include "return_status.h"

#include <stdio.h>
#include <fcntl.h>
#include <string.h>
#include <ctype.h>

// buffer sizes in main program - these should be made dynamic, eventually!
#define MAX_INPUTS 1000
#define MAX_OPEN 20		/*!!!! 20 !!!!*/
#define MAX_NL 0
#define MAX_NS 0
#define MAX_OBUF 100000

////////////////////////////////////////////////////////////////////////
// Used by the inverse projection routine
////////////////////////////////////////////////////////////////////////
struct MosProjArgs {
    PigCameraModel **camera_in;
    PigSurfaceModel *surface_model;
    PigCameraModel *camera_out;
    double x_offset;
    double y_offset;
    PigCoordSystem *proj_cs;
    PigVector output_direction;		// for labels only
};

extern "C" int mos_proj_inverse(double in_line, double in_samp,
		double *out_line, double *out_samp,
		int input_number, void *proj_args);
extern "C" int mos_width_func(double line, double samp, double ndeg,
                int *out_pix, double *out_line_zeroel,
                int input_number, void *proj_args);

////////////////////////////////////////////////////////////////////////

void main44()
{
    int i, j, k;
    int status, count, def;
#define MSG_SIZE 150
    char msg[MSG_SIZE];

    // Output bands are 0 to band_count-1
    // Input bands are 0 to getNB, because mars_read_inputs returns them
    // starting at 0.  Note that there may not be enough bands to go around
    // (e.g. bw and color mix) so we have to check NB.
    // "band" is used only to pass into mars_read_inputs
    int band;			// 0 means all bands, >0 means that one band
    int band_count;		// # of bands on output, # bands used on input
    PigVector output_direction;
    int nids;
    char mission[64], instrument[64];

    PigSurfaceModel *surface_model;

    MosProjArgs proj;

    // Inputs

    PigFileModel *file_models[MAX_INPUTS];
    PigCameraModel *camera_in[MAX_INPUTS];
    PigPointingModel *pointing_in[MAX_INPUTS];
    int xdim_in, ydim_in;
    int homogeneous_inputs = TRUE;
    double FOV[MAX_INPUTS];				// cache for efficiency
    PigVector camera_orientation[MAX_INPUTS];		// cache for efficiency
    PigPoint camera_position[MAX_INPUTS];		// cache for efficiency
    RadiometryModel *radiometric[MAX_INPUTS];
    PigBrtCorrModel *brt_corr[MAX_INPUTS];
    PigCoordSystem *proj_cs;
    SimpleImage<short int> *short_int_images[MAX_INPUTS];
    memset(short_int_images, 0, sizeof(short_int_images));	
    SimpleImage<float> *float_images[MAX_INPUTS];		
    memset(float_images, 0, sizeof(float_images));	

    int short_int_data = 1;
    int do_clamp = 1;
    int do_interp = 1;
    char format[10];                   // input data format
   

    // Outputs

    int unit_out = 0;
    int unit_idx_out = 0, unit_icm_out = 0;
    int nlo, nso;
    double x_offset, y_offset;
    PigCameraModel *camera_out;
    PigPointingModel *pointing_out;
    short int *obuf_si[MARS_MAX_NB];
    float *obuf_f[MARS_MAX_NB];
    short int *obuf_idx;
    float *obuf_icm_l;
    float *obuf_icm_s;

    int do_print = TRUE;		// True if info message should be issued

    // User Parameters
    double tile_bias_value[MAX_INPUTS];
    double *bias_ptr;
    int azim_count, elev_count;
    double azimuth_out, elevation_out, twist;
    PigCoordSystem *azelout_cs;
    int maximumnl, maximumns;
    int min_input, max_input;
    double out_pos[3], zoom[2];
    double delta_z;

    zvmessage("MARSMOS version 2020-11-17", "");

    // Get the input file list, and set up initial camera/pointing models
    // for each input.

    mars_setup(nids, file_models, camera_in, pointing_in, surface_model,
		radiometric, brt_corr,
		proj_cs, mission, instrument,
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
            do_clamp = 0;
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
        // No input band specified; process all bands.
	band_count = maxNB;
        snprintf(msg, MSG_SIZE, "Number of bands to be processed is (%d)", band_count);
        zvmessage(msg, "");
    }
    else {

        // check if input band number is greater than number of bands in input
        if (band > file_models[0]->getNB()) {
            snprintf(msg, MSG_SIZE, "Input band (%d) is greater than number of bands in input image. Band set to 1.", band);
            zvmessage(msg, "");
            band = 1;
        }
    }

    // reads the BIAS values then fill unspecified ones
    zvparmd("BIAS", tile_bias_value, &count, &def, nids, 0);
    for (i=count; i < nids; i++) {
	tile_bias_value[i] = 1.0;
    } 
    bias_ptr = NULL;
    if (count != 0)
        bias_ptr = tile_bias_value;

    zvparmd("AZOUT", &azimuth_out, &azim_count, &def, 1, 0);
    zvparmd("ELOUT", &elevation_out, &elev_count, &def, 1, 0);
    zvp("MAXNL", &maximumnl, &count);
    zvp("MAXNS", &maximumns, &count);

    // Reads the INPUT_RANGE parameter
    int range[2];
    min_input = 0;
    max_input = nids;
    zvparm("INPUT_RANGE", range, &count, &def, 2, 0);
    if (count >= 1)
	min_input = range[0] - 1;               // 1-based
    if (count >= 2)
	max_input = range[1];			// max is actually end+1
    if (min_input < 0)
	min_input = 0;
    if (max_input > nids)
	max_input = nids;

    // Check the INTERP parameter.
    do_interp = zvptst("INTERP");

    snprintf(msg, MSG_SIZE, "Mosaic's surface model parameters are specified in the %s coordinate frame",
		surface_model->getCoordSystem()->getFrameName());
    if (do_print) zvmessage(msg, "");

    surface_model->setCoordSystem(proj_cs);

    // Create coordinate systems to use

    azelout_cs = proj_cs;

    // Output pointing is "average" of all the inputs.  Just add their
    // direction vectors and normalize when done.  Ignore twist, if any.
    // Use the projection frame.

    for (i=0; i < nids; i++) {
	output_direction += pointing_in[i]->getCameraOrientation(proj_cs);
    }
    output_direction.normalize();

    // Compute an output camera model which is a synthesis of both left and
    // right models, if they exist, in order to optimize stereo viewing.
    // The camera model of the first input is used, but with the subtype
    // nulled out - the theory being that this is often used for a filter,
    // but for the output, we want to use only *one* model for all filters,
    // so the colors will register.  This is somewhat mission-specific due
    // to the reliance on the subtype usage!!!!

    // TBD: Can this handle image rotations (e.g. spacecraft tilt)?  Or will
    // the stereo get messed up?? !!!!

    // Get the model of the first input
    PigCameraModel *camera = PigCameraModel::create(
		camera_in[0]->getMissionName(),
		camera_in[0]->getInstrumentName(),
		camera_in[0]->getCameraVersion(),
		camera_in[0]->getCameraSubtype(), 
		NULL,    // special 
		camera_in[0]->getCameraConstruction(),
		camera_in[0]->getCameraCalibration(),
		NULL);

    // get the model for its "stereo partner"
    PigCameraModel *camera_other = PigCameraModel::create(
		camera_in[0]->getMissionName(),
		camera_in[0]->getInstrumentName(),
		camera_in[0]->getCameraVersion(),
		camera_in[0]->getCameraSubtype(),
		"stereo", //special
		camera_in[0]->getCameraConstruction(),
		camera_in[0]->getCameraCalibration(),
		NULL);

    // Create Pointing Model based on the main input. 
    pointing_out = PigPointingModel::create(camera,
					    camera->getMissionName(), 
					    camera->getInstrumentName(),
					    NULL,
					    true);

    // now align the models
    xdim_in = file_models[0]->getNL();
    ydim_in = file_models[0]->getNS();

    camera_out = camera->alignStereoCameras(xdim_in, ydim_in,
					    xdim_in, ydim_in,
					    xdim_in, ydim_in,
					    camera_other);

    // Now that alignment is done and we have new camera model, update
    // Pointing Model to use that model
    pointing_out->setCameraModel(camera_out);

    delete camera;
    delete camera_other;

    // Since PointingModel was created using original "camera" Camera Model
    // pointing camera using "camera's" file_model would lead to results
    // as expected.
    pointing_out->pointCamera(file_models[0]);

    //Apply nav file correction, if any, to the output pointing
    status = mars_apply_navtable(1, &pointing_out, file_models);
    if (status >= 2)		// 1 means user didn't ask for file
    	zabend();		// msg already printed   


    // Convert output direction to specified coords, reset az/el from
    // params, then convert it back to projection cs.
    output_direction = azelout_cs->convertVector(output_direction, proj_cs);
    if (azim_count != 0 && elev_count != 0)
	output_direction = azelout_cs->constructVector(PigDeg2Rad(azimuth_out),
						PigDeg2Rad(elevation_out));
    else if (azim_count != 0)
	output_direction = azelout_cs->constructVector(PigDeg2Rad(azimuth_out),
					azelout_cs->getEl(output_direction));
    else if (elev_count != 0)
	output_direction = azelout_cs->constructVector(
					azelout_cs->getAz(output_direction),
					PigDeg2Rad(elevation_out));
    output_direction = proj_cs->convertVector(output_direction, azelout_cs);

    pointing_out->pointCamera(file_models[0]);

    // Only do this pointing if ALT pointing is not specified
    int use_alt = zvptst("USE_ALT_POINT");
    if (!use_alt)
	pointing_out->setCameraOrientation(output_direction, proj_cs);

    // Print out input status from labels

    mars_print_inputs(nids, pointing_in, camera_in, file_models,
			homogeneous_inputs, mission, instrument);

    // Apply twist value, if any, to the output camera

    zvparmd("TWIST", &twist, &count, &def, 1, 0);
    if (count != 0) {
	snprintf(msg, MSG_SIZE, "Initial Twist Value: %f",
				PigRad2Deg(pointing_out->getCameraTwist()));
	zvmessage(msg, "");
	pointing_out->setCameraTwist(PigDeg2Rad(twist));
	snprintf(msg, MSG_SIZE, "Overridden Twist Value: %f",
				PigRad2Deg(pointing_out->getCameraTwist()));
	zvmessage(msg, "");
    }

    // Set the camera position, if specified

    zvparmd("OUTPOS", out_pos, &count, &def, 3, 0);
    if (count == 3) {
	PigPoint pos(out_pos);
	pointing_out->setCameraPosition(pos, azelout_cs);
	snprintf(msg, MSG_SIZE, "Camera position reset to (%f,%f,%f) (%s frame)",
		pos.getX(), pos.getY(), pos.getZ(), azelout_cs->getFrameName());
	zvmessage(msg, "");
    }

    // Adjust the Z component of the camera position, if specified

    zvparmd("DELTAZ", &delta_z, &count, &def, 3, 0);
    if (count != 0) {
	PigPoint pos = pointing_out->getCameraPosition(azelout_cs);
	delta_z += pos.getZ();
	pos.setZ(delta_z);
	pointing_out->setCameraPosition(pos, azelout_cs);
    }

    // Set the zoom factor, if specified

    zvparmd("ZOOM", zoom, &count, &def, 2, 0);
    if (count == 1) {
	camera_out->scaleCamera(zoom[0], zoom[0]);
	snprintf(msg, MSG_SIZE, "Camera zoomed by %f", zoom[0]);
	zvmessage(msg, "");
    }
    if (count == 2) {
	camera_out->scaleCamera(zoom[0], zoom[1]);
	snprintf(msg, MSG_SIZE, "Camera zoomed by x=%f, y=%f", zoom[0], zoom[1]);
	zvmessage(msg, "");
    }

    // Print more stuff

    snprintf(msg, MSG_SIZE, "%s output azimuth = %9f", proj_cs->getFrameName(),
				PigRad2Deg(proj_cs->getAz(output_direction)));
    zvmessage(msg, "");
    snprintf(msg, MSG_SIZE, "%s output elevation = %9f", proj_cs->getFrameName(),
				PigRad2Deg(proj_cs->getEl(output_direction)));
    zvmessage(msg, "");

    double output_params[PIG_MAX_PARAMS];
    pointing_out->getPointingParameters(output_params, PIG_MAX_PARAMS);
    j = pointing_out->getPointingParamCount();
    if (j > PIG_MAX_PARAMS)
	j = PIG_MAX_PARAMS;
    for (i=0; i < j; i++) {
	snprintf(msg, MSG_SIZE, "Output Instrument %s: %f",
		pointing_out->getPointingParamName(i), output_params[i]);
	zvmessage(msg, "");
    }

    // If alternate pointing was asked for, now's the time to use it

    if (use_alt) {

	double alt_pos_a[3];
	zvparmd("ALT_POS", alt_pos_a, &count, &def, 3, 0);
	PigPoint alt_pos(alt_pos_a);
	if (count != 3) {		// use camera C instead
	    alt_pos = camera_out->getCameraPosition();
	    zvmessage("ALT_POS not given, using camera position", "");
	}

	double quat[4];
	PigQuaternion alt_quat;
	zvparmd("ALT_QUAT", quat, &count, &def, 4, 0);
	if (count == 4) {
	    alt_quat.setComponents(quat);
	} else {
	    double axis[3], angle;
	    zvparmd("ALT_AXIS", axis, &count, &def, 3, 0);
	    if (count != 3) {
		zvmessage("If ALT pointing is used, one of ALT_QUAT or ALT_AXIS/ANGLE must be used", "");
		zabend();
	    }
	    PigVector q_axis(axis);
	    zvparmd("ALT_ANGLE", &angle, &count, &def, 1, 0);
	    if (count != 1) {
		zvmessage("If ALT pointing is used, one of ALT_QUAT or ALT_AXIS/ANGLE must be used", "");
		zabend();
	    }
	    angle = PigDeg2Rad(angle);
	    alt_quat = PigQuaternion(q_axis, angle);
	    snprintf(msg, MSG_SIZE, "Using alt axis,angle of (%f %f %f), %f radians (%f deg)",
		axis[0], axis[1], axis[2], angle, PigRad2Deg(angle));
	    zvmessage(msg, "");
	}
	snprintf(msg, MSG_SIZE, "Alternate pointing: pos=(%f %f %f), quat=(%f %f %f %f)",
		alt_pos.getX(), alt_pos.getY(), alt_pos.getZ(),
		alt_quat.getS(), alt_quat.getV().getX(), alt_quat.getV().getY(),
		alt_quat.getV().getZ());
	zvmessage(msg, "");

	// Now apply the alternate pointing

	PigPoint c = camera_out->getCameraPosition();
	camera_out->moveCamera(c, PigQuaternion(), alt_pos, alt_quat, proj_cs);

    }

    // compute size of output image
    // Find corner points of each input, project point to the surface
    // and back, and keep min and max coordinates.  Because this projects
    // the *middle* of the pixel, not the edge, we widen the images by 1
    // to make sure we don't cut off those half pixels around the edge.

    double x_min = 1.e+20;
    double x_max = -1.e+20;
    double y_min = 1.e+20;
    double y_max = -1.e+20;
    for (k=0; k < nids; k++) {
	PigPoint origin;
	PigVector look;
	PigPoint surf_pt;
	int hits;
	double line, samp;

        samp = file_models[k]->getXOffset() - 1;
        line = file_models[k]->getYOffset() - 1;

	camera_in[k]->LStoLookVector(line, samp, origin, look, proj_cs);
	hits = surface_model->intersectRay(origin, look, surf_pt);
	camera_out->XYZtoLS(surf_pt, (hits <= 0), &line, &samp, proj_cs);
        if (x_min > samp) x_min=samp;
        if (x_max < samp) x_max=samp;
        if (y_min > line) y_min=line;
        if (y_max < line) y_max=line;

        samp = file_models[k]->getXOffset() + file_models[k]->getNS();
        line = file_models[k]->getYOffset() - 1;

	camera_in[k]->LStoLookVector(line, samp, origin, look, proj_cs);
	hits = surface_model->intersectRay(origin, look, surf_pt);
	camera_out->XYZtoLS(surf_pt, (hits <= 0), &line, &samp, proj_cs);
        if (x_min > samp) x_min=samp;
        if (x_max < samp) x_max=samp;
        if (y_min > line) y_min=line;
        if (y_max < line) y_max=line;

        samp = file_models[k]->getXOffset() - 1;
        line = file_models[k]->getYOffset() + file_models[k]->getNL();

	camera_in[k]->LStoLookVector(line, samp, origin, look, proj_cs);
	hits = surface_model->intersectRay(origin, look, surf_pt);
	camera_out->XYZtoLS(surf_pt, (hits <= 0), &line, &samp, proj_cs);
        if (x_min > samp) x_min=samp;
        if (x_max < samp) x_max=samp;
        if (y_min > line) y_min=line;
        if (y_max < line) y_max=line;

        samp = file_models[k]->getXOffset() + file_models[k]->getNS();
        line = file_models[k]->getYOffset() + file_models[k]->getNL();

	camera_in[k]->LStoLookVector(line, samp, origin, look, proj_cs);
	hits = surface_model->intersectRay(origin, look, surf_pt);
	camera_out->XYZtoLS(surf_pt, (hits <= 0), &line, &samp, proj_cs);
        if (x_min > samp) x_min=samp;
        if (x_max < samp) x_max=samp;
        if (y_min > line) y_min=line;
        if (y_max < line) y_max=line;
    }

    // If MAXFOV parameter is specified, overwrite min/max 
    double fov[2];
    zvparmd("MAXFOV", fov, &count, &def, 2, 0);
    if (count == 2)
        camera_out->getMinMaxLS(fov[0], fov[1], &x_min, &x_max, &y_min, &y_max);
    if (count == 1) // uses same value for both horiz. and vert.
        camera_out->getMinMaxLS(fov[0], fov[0], &x_min, &x_max, &y_min, &y_max);

    // set output picture dimensions
    nlo = (int)(y_max - y_min);
    nso = (int)(x_max - x_min);
    if (nlo > maximumnl) {
        double center_x, center_y;
        camera_out->XYZtoLS(camera_out->getCameraOrientation(), 1, 
		&center_y, &center_x, camera_out->getCoordSystem());
	y_min = center_y - maximumnl/2;
	y_max = center_y + maximumnl/2;
	nlo = (int)(y_max - y_min);
    }
    if (nso > maximumns) {
        double center_x, center_y;
        camera_out->XYZtoLS(camera_out->getCameraOrientation(), 1, 
		&center_y, &center_x, camera_out->getCoordSystem());
	x_min = center_x - maximumns/2;
	x_max = center_x + maximumns/2;
	nso = (int)(x_max - x_min);
    }

    printf("x_min=%f, x_max=%f\ny_min=%f, y_max=%f\n", 
	   x_min, x_max, y_min, y_max);

    snprintf(msg, MSG_SIZE, "Output # lines & samples=%10d %10d", nlo, nso);
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

    // compute offsets between camera l/s coords and physical image

    x_offset = x_min;
    y_offset = y_min;

    snprintf(msg, MSG_SIZE,"Line offset %10f Sample offset %10f", y_offset, x_offset);
    zvmessage(msg, "");

    // Check for output offset override

    zvpcnt("outoff", &count);
    if (count > 0) {
	double off[2];
	zvparmd("outoff", off, &count, &def, 2, 0);
	y_offset = off[0];
	x_offset = off[1];
	snprintf(msg, MSG_SIZE, "Override: Line offset %10f Sample offset %10f",
							y_offset, x_offset);
	zvmessage(msg, "");
    }

    // If we want to incorporate the shift into the camera model, do it now

    if (zvptst("SHIFT")) {
	camera_out->shiftCamera(x_offset, y_offset);
	x_offset = 0;
	y_offset = 0;
	zvmessage("Camera model shifted to eliminate offset", "");
    }

    // Set up projection parameters structure.  Used for label writing and
    // footprints.

    proj.camera_in = camera_in;
    proj.surface_model = surface_model;
    proj.camera_out = camera_out;
    proj.x_offset = x_offset;
    proj.y_offset = y_offset;
    proj.proj_cs = proj_cs;
    proj.output_direction = output_direction;

    // open output file

    // Because the projection can get infinitely large at 180 degrees,
    // we apply a sanity check.
    if ((nlo > MAX_OBUF) || (nlo < 1) || (nso > MAX_OBUF) || (nso < 1)) {
	zvmessage("Unreasonable output file dimensions", "");
	zabend();
    }
    zvselpi(0);			// don't transfer any labels
    status=zvunit(&unit_out, "OUT", 1, NULL);
    status=zvopen(unit_out, "OP", "WRITE", "U_FORMAT", format,
               "U_NS", nso, "U_NL", nlo, "OPEN_ACT", "AS",
               "U_NB", band_count, "U_ORG", "BSQ", "O_FORMAT", format, NULL);

    // Open the idx/icm files

    // Check for index and ICM outputs

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
 
    // Write the output labels

    PigMission *m = PigMission::getMissionObject(mission);
    PigLabelModel *labelModel = m->createLabelModel(unit_out);
    labelModel->setMos(file_models,
			radiometric, brt_corr, bias_ptr, NULL, nids, 
			surface_model, camera_out, x_offset, 
                	y_offset, proj_cs, output_direction);
    zvplabel(unit_out, 0, 1);

    // Write the idx/icm output labels

    PigLabelModel *idxLabel = NULL;
    if (do_idx) {
        idxLabel = m->createLabelModel(unit_idx_out);
        idxLabel->setDerivedImageType("IDX_MAP");
        zvplabel(unit_idx_out, 0, 1);
    }
    PigLabelModel *icmLabel = NULL;
    if (do_icm) {
        icmLabel = m->createLabelModel(unit_icm_out);
        icmLabel->setDerivedImageType("ICM_MAP");
        zvplabel(unit_icm_out, 0, 1);
    }


    // Allocate buffers and write output as zero's

    for (int b=0; b < band_count; b++) {
	obuf_f[b] = new float[nso];
	if (obuf_f[b] == NULL) {
            zvmessage("Unable to allocate obuf memory!","");
	    zabend();
	}
	memset(obuf_f[b], 0, nso*sizeof(float));
	for (j=0; j<nlo; j++) 
	    status=zvwrit(unit_out, obuf_f[b],"LINE",j+1,"BAND",b+1, NULL);
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

    // reopen output for update

    status=zvclose(unit_out, NULL);
    status=zvopen(unit_out, "OP", "UPDATE", "U_FORMAT", "REAL",
               "U_NS", nso, "U_NL", nlo, "OPEN_ACT", "AS",
               "U_NB", band_count, "U_ORG", "BSQ", "O_FORMAT", format, NULL);

    // reopen idx/icm outputs as well if requested

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
    // (180 total) to avoid any back-projection (MER hazcam H+V is > 90).
    // We also compute and save the camera position and orientation here
    // once, rather than rederiving it for each pixel.

    for (i=0; i < nids; i++) {
	FOV[i] = cos((file_models[i]->getFOV(camera_in[i], 0) +
		      file_models[i]->getFOV(camera_in[i], 1)) / 2);
	if (FOV[i] < 0.0)
	    FOV[i] = 0.0;		// Limit to 90 degrees
	camera_position[i] = pointing_in[i]->getCameraPosition(proj_cs);
	camera_orientation[i] = pointing_in[i]->getCameraOrientation(proj_cs);
    }

    //Create and initilize variables necessary for pass loop
    int first_input, last_input;

    // Make a number of passes, processing MAX_OPEN inputs each pass
    for (first_input = min_input; first_input < max_input;
					first_input += MAX_OPEN) {

	snprintf(msg, MSG_SIZE, "Pass %d of %d", (first_input/MAX_OPEN)+1,
				      ((max_input-1)/MAX_OPEN)+1);
	zvmessage(msg, "");

	last_input = first_input + MAX_OPEN-1;
	if (last_input >= max_input)
	    last_input = max_input-1;

	// Read a set of inputs into memory

	mars_read_inputs(first_input, last_input, file_models,
			 float_images, MAX_NL, MAX_NS, band,
			 radiometric, brt_corr);

	// Loop through each sample of the output, and pick up the input
	// pixel that corresponds to it

	for (j=0; j<nlo; j++) {			// line loop
 
	    if (j%100 == 0) {
		snprintf(msg, MSG_SIZE, "line %d", j);
		zvmessage(msg, "");
	    }

	    // re-read output line
	    for (int b = 0; b < band_count; b++) {
		zvread(unit_out, obuf_f[b], "LINE",j+1, "BAND",b+1, NULL); 
	    }
	    if (do_idx) {
	        zvread(unit_idx_out, obuf_idx, "LINE", j+1, NULL);
	    }
	    if (do_icm) {
	        zvread(unit_icm_out, obuf_icm_l, "LINE", j+1, "BAND", 1, NULL);
		zvread(unit_icm_out, obuf_icm_s, "LINE", j+1, "BAND", 2, NULL);
	    }

	    for (i=0; i<nso; i++) {		// sample loop

		int continue_flag = 0;
		for (int b = 0; b < band_count; b++) {
		    if (obuf_f[b][i] != 0) {
			continue_flag=1;  // already set, do next iteration
			break;
	            }
		}
		if (continue_flag) {
		    continue;
		}

		// compute the camera-coordinate line and sample, and project
		// into space and then XYZ.

		double out_samp = (double)i + x_offset;
		double out_line = (double)j + y_offset;

		PigPoint origin;
		PigVector look;
		PigPoint surf_pt;

		camera_out->LStoLookVector(out_line, out_samp, origin, look,
								proj_cs);
		int hits = surface_model->intersectRay(origin, look, surf_pt);
		int infinity = (hits <= 0);

#if 0	/* Do this to match mpfmos, where it projects backwards to a plane */
	/* Instead, we treat things above the "horizon" at infinity.	   */
		if (hits < 0) {		// "behind" camera
		    //!!!! This should be modeled at infinity.  It inverts the
		    //!!!! look direction to be compatible with mpfmos!!!!
		    PigVector zero(0.0, 0.0, 0.0);
		    surface_model->intersectRay(origin, zero - look, surf_pt);
		    infinity = FALSE;
		}
#endif


		for (k=first_input; k <= last_input; k++) {	// picture loop
		    int kp=k-first_input;
		    // int fileNS = file_models[k]->getNS();

		    // skip picture if this point is outside of the FOV of the
		    // input picture.  To handle cameras that move, we take the
		    // XYZ point - the camera center, then dot that with the
		    // camera look vector to get the angle between the camera
		    // center and the point.  If we're at infinity, we can
		    // simply check the point's look vector.
		    //
		    // One might question the computational efficiency of this
		    // rather than just projecting the pixel, but something like
		    // it is necessary to keep the point from projecting
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

		    // convert output image coordinate to camera coordinates
		    // in the input image

		    double in_line, in_samp;

		    camera_in[k]->XYZtoLS(surf_pt, infinity, 
					  &in_line, &in_samp,
					  proj_cs);

		    // check if point is within the input image

		    if (file_models[k]->testPixelLocation(in_line,in_samp) != 0)
			continue;			// skip if not
 
		    // Convert to phys image coords (compensate for sub-areas)
		    double image_line = in_line - file_models[k]->getYOffset();
		    double image_samp = in_samp - file_models[k]->getXOffset();

		    
		    double dn;
		    
		    if (do_interp) {
			// interpolate in the input image 
			// (bilinear interpolation)

			int m = (int) image_samp;
			int n = (int) image_line;
			
			double wr = image_samp - m;
			double wl = 1.0 - wr;
			
			double wb = image_line - n;
			
			double top, bot;
			

			// Interp for floats

			register SimpleImage<float> *img = float_images[kp];

			for (int b=0; b < band_count; b++) {
		            register int bb = b;
			    if (bb >= img->getNB())
				bb = img->getNB()-1;
			    register double ul = img->get(bb,n,m);
			    register double ur = img->get(bb,n,m+1);
			    register double ll = img->get(bb,n+1,m);
			    register double lr = img->get(bb,n+1,m+1);
			    if (ul == 0.0) {
				if (ur != 0.0) ul = ur;
				else if (ll != 0.0) ul = ll;
				else ul = lr;
			    }   // ul is now known to be non-0 if possible
			        // Slightly sub-optimal if ur was originally 0
			        // (in which case we should take lr) but that's
			        // not worth dealing with.
			    if (ur == 0.0) ur = ul;
			    if (ll == 0.0) ll = ul;
			    if (lr == 0.0) lr = ul;

			    top = wl * ul + wr * ur;
			    bot = wl * ll + wr * lr;

			    dn = (bot * wb + top * (1.0-wb))
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
			    obuf_f[b][i] = dn;			
			}
		    }  // end of interpolation conditional

		    else {  // Don't interpolate
			register SimpleImage<float> *img = float_images[kp];
			for (int b=0; b < band_count; b++) {
		            register int bb = b;
			    if (bb >= img->getNB())
				bb = img->getNB()-1;
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
			    break_flag = 1; // skip the rest of the pictures
			    break;
			}
	            }
		    if (break_flag)
			break;		// skip rest of pictures
		}			// picture loop
	    }				// sample loop

	    for (int b=0; b < band_count; b++) {
		zvwrit(unit_out, obuf_f[b], "LINE",j+1, "BAND",b+1, NULL);
	    }
	    if (do_idx)
		zvwrit(unit_idx_out, obuf_idx, "LINE", j+1, NULL);
	    if (do_icm) {
		zvwrit(unit_icm_out, obuf_icm_l, "LINE", j+1, "BAND", 1, NULL);
		zvwrit(unit_icm_out, obuf_icm_s, "LINE", j+1, "BAND", 2, NULL);
	    }
	}				// line loop
    }					// pass loop

    //  No need to deallocate input arrays because program is about to end

    // Add footprints/numbering if requested
    // mars_footprints will reopen file and close it when it's done.
    zvclose(unit_out, NULL);
    mars_footprints(unit_out, nids, nso, nlo, band_count,
			camera_in, file_models, mos_proj_inverse, &proj);

    // Add bounding boxes if requested
    mars_mosaic_bbox(nids, camera_in, file_models, nlo, nso,
		mos_proj_inverse, mos_width_func, &proj);

    zvclose(unit_idx_out, NULL);
    zvclose(unit_icm_out, NULL);
}

////////////////////////////////////////////////////////////////////////
// Inverse projection routine (input -> output) for use by footprinter
////////////////////////////////////////////////////////////////////////

extern "C" int mos_proj_inverse(double in_line, double in_samp,
		double *out_line, double *out_samp,
		int input_number, void *proj_args)
{
    MosProjArgs *proj = (MosProjArgs *)proj_args;
    PigPoint origin;
    PigVector look;
    PigPoint surf_pt;
    int hits;

    proj->camera_in[input_number]->LStoLookVector(in_line, in_samp,origin,look,
								proj->proj_cs);
    hits = proj->surface_model->intersectRay(origin, look, surf_pt);
    proj->camera_out->XYZtoLS(surf_pt, (hits <= 0), out_line, out_samp,
								proj->proj_cs);

    *out_line -= proj->y_offset;
    *out_samp -= proj->x_offset;

    return 0;
}

////////////////////////////////////////////////////////////////////////
// Mosaic width routine for bounding box.  We can't wrap, so it's trivial.
////////////////////////////////////////////////////////////////////////

extern "C" int mos_width_func(double line, double samp, double ndeg,
                int *out_pix, double *out_line_zeroel,
                int input_number, void *proj_args)
{
    *out_pix = 0;
    *out_line_zeroel = 0.0;
    return 0;
}

