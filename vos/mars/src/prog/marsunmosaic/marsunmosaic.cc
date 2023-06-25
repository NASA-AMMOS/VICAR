/* marsunmosaic */

#include "vicmain_c"

#include "mars_support.h"

#include "PigMission.h"
#include "PigFileModel.h"
#include "PigLabelModel.h"
#include "PigCameraModel.h"
#include "PigCAHVORE.h"		// also gets PigCAHVOR.h and PigCAHV.h
#include "PigPointingModel.h"
#include "PigSurfaceModel.h"
#include "RadiometryModel.h"
#include "PigCoordSystem.h"

#include "return_status.h"

#include <math.h>

////////////////////////////////////////////////////////////////////////

// buffer sizes in main program - these should be made dynamic, eventually!
#define MAX_OBUF 100000
#define MAX_NL MARS_MAX_NL
#define MAX_NS MARS_MAX_NS


//Assumes "ibuf", "nso" local variables.
#define IBUF(i,j) ( *(ibuf + (i) * (nso+1) + (j)) )
//Assumes "fbuf", "fileNS" local variables.
#define FBUF(i,j) ( *(fbuf + (i) * (nso+1) + (j)) )

////////////////////////////////////////////////////////////////////////

void main44()
{
    int i, j;
    int status, count, def;
    const int MSG_LEN = 150;
    char msg[MSG_LEN];

    PigSurfaceModel *surface_model;

    // Inputs

    PigFileModel *mosaic_file;
    PigFileModel *example_file;
    PigCameraModel *camera_out = NULL;
    PigPointingModel *pointing_out = NULL;
    short int *ibuf = NULL, *oibuf = NULL;	// input, output buffers
    float *fbuf = NULL, *ofbuf = NULL;
    PigCoordSystem *proj_cs, *param_cs = NULL, *camera_cs = NULL;
    int azdir;				// +1 == CW, -1 == CCW
    PigMission *m;

    int short_int_data = 1;
    int do_interp = 1;
    char format[10];                   // input data format

    // User Parameters

    PigPoint C;
    PigVector A, H, V, O, R, E;
    int mtype = 0;
    float mparm = 0.;
    int nl, ns;
    double x_offset = 0., y_offset = 0.;

    double pointing[PIG_MAX_PARAMS];
    double azimuth, elevation;
    int num_pointing;
    PigPoint position;

    // Projection

    double scale_y, scale_x;		// radians/pixel
    PigPoint proj_origin;
    double line_zero_el;
    double az_first_sample, az_last_sample;
    double min_elev, max_elev;

    // Outputs

    int unit_out;
    int nlo, nso;
    
    char filename[PIG_MAX_FILENAME_SIZE];

    zvmessage("MARSUNMOSAIC, version 2020-05-21", "");
 
    do_interp = zvptst("INTERP");

    ////////////////////////////////////////////////////////////////////////
    // If the example file is given, get it and set it up
    ////////////////////////////////////////////////////////////////////////

    zvp("EXAMPLE", filename, &count);
    if (count != 0) {

	example_file = PigFileModel::create(filename);
	if (example_file == NULL) {
	    zvmessage("Unable to create file model for EXAMPLE file", "");
	    zabend();
	}

	nl = example_file->getNL();
	ns = example_file->getNS();

	zvselpiu(example_file->getUnit());

	// Initialize the coordinate systems

        m = PigMission::getMissionObject(example_file->getMissionName());
	mars_read_rsf(example_file->getMissionName());
	param_cs = mars_setup_coords(example_file->getMissionName(),
			1, &example_file);

	camera_out = PigCameraModel::create(example_file, NULL);
	camera_cs = camera_out->getCoordSystem();

	if (camera_out == NULL) {
	    zvmessage("Unable to create camera model for example input", "");
	    zabend();
	}

	pointing_out = PigPointingModel::create(camera_out, example_file, NULL, true);

	if (pointing_out == NULL) {
	    zvmessage("Unable to create pointing model for example input", "");
	    zabend();
	}

	pointing_out->pointCamera(example_file);

	x_offset = example_file->getXOffset();
	y_offset = example_file->getYOffset();
    }

    ////////////////////////////////////////////////////////////////////////
    // No file is given; get the relevant parts from the parameters
    ////////////////////////////////////////////////////////////////////////

    else {

	if (zvptst("FILE")) {
	    zvmessage("FILE camera model type given without example file", "");
	    zabend();
	}
	example_file = NULL;

	///////////////////////////////
	// Set up the coordinate system
	///////////////////////////////

	m = PigMission::getMissionObject("Generic");
	mars_read_rsf("Generic");
	camera_cs = mars_setup_coords("Generic", 0, NULL);
	param_cs = camera_cs;

        /////////////////////
	// Get the parameters
        /////////////////////

	zvparm("NL", &nl, &count, &def, 1, 0);
	if (count != 1) {
	    zvmessage("NL must be specified", "");
	    zabend();
	}
	zvparm("NS", &ns, &count, &def, 1, 0);
	if (count != 1) {
	    zvmessage("NS must be specified", "");
	    zabend();
	}
    }

    ////////////////////////////////////////////////////////////////////////
    // See if we need to construct the camera model the hard way
    ////////////////////////////////////////////////////////////////////////

    if (!zvptst("FILE")) {

	double vec[3];

	zvparmd("C", vec, &count, &def, 3, 0);
	if (count != 3) {
	    zvmessage("C vector must have 3 values", "");
	    zabend();
	}
	C.setXYZ(vec);

	zvparmd("A", vec, &count, &def, 3, 0);
	if (count != 3) {
	    zvmessage("A vector must have 3 values", "");
	    zabend();
	}
	A.setXYZ(vec);

	zvparmd("H", vec, &count, &def, 3, 0);
	if (count != 3) {
	    zvmessage("H vector must have 3 values", "");
	    zabend();
	}
	H.setXYZ(vec);

	zvparmd("V", vec, &count, &def, 3, 0);
	if (count != 3) {
	    zvmessage("V vector must have 3 values", "");
	    zabend();
	}
	V.setXYZ(vec);

        if (zvptst("CAHVOR") || zvptst("CAHVORE")) {

	    zvparmd("O", vec, &count, &def, 3, 0);
	    if (count != 3) {
		zvmessage("O vector must have 3 values", "");
		zabend();
	    }
	    O.setXYZ(vec);

	    zvparmd("R", vec, &count, &def, 3, 0);
	    if (count != 3) {
		zvmessage("R vector must have 3 values", "");
		zabend();
	    }
	    R.setXYZ(vec);
	}

	if (zvptst("CAHVORE")) {
	    zvparmd("E", vec, &count, &def, 3, 0);
	    if (count != 3) {
		zvmessage("E vector must have 3 values", "");
		zabend();
	    }
	    E.setXYZ(vec);

	    zvparm("T", &mtype, &count, &def, 1, 0);
	    if (count != 1) {
		zvmessage("T parameter must be specified", "");
		zabend();
	    }
	    zvparmd("P", &mparm, &count, &def, 1, 0);
	    if (count != 1) {
		zvmessage("P parameter must be specified", "");
		zabend();
	    }
	}

        //////////////////////////////
	// Now set up the camera model
        //////////////////////////////

	char *instrument = NULL;
	if (example_file != NULL)
	    instrument = (char *)example_file->getInstrumentName();

	if (zvptst("CAHVORE")) {
	    camera_out = new PigCAHVORE(m->getMissionName(), instrument,
					NULL, NULL, NULL, NULL);
	    ((PigCAHVORE *)camera_out)->setInitialCAHVORE(C, A, H, V, O, R, E,
						mtype, mparm, param_cs);
	}
	else if (zvptst("CAHVOR")) {
	    camera_out = new PigCAHVOR(m->getMissionName(), instrument,
					NULL, NULL, NULL, NULL);
	    ((PigCAHVOR *)camera_out)->setInitialCAHVOR(C, A, H, V, O, R,
						param_cs);
	}
	else {			// CAHV
	    camera_out = new PigCAHV(m->getMissionName(), instrument,
					NULL, NULL, NULL, NULL);
	    ((PigCAHV *)camera_out)->setInitialCAHV(C, A, H, V, param_cs);
	}
	camera_out->setInitialCoordSystem(camera_cs);

	////////////////////////////
	// Set up the pointing model
	////////////////////////////

        // Yes, example_file could be NULL.
	pointing_out = m->createPointingModel(camera_out, example_file, NULL, true);
 	if (example_file != NULL)
	    pointing_out->pointCamera(example_file);

	// Use the camera center as the offsets

	camera_out->getCameraCenter(y_offset, x_offset);
    }

    ////////////////////////////////////////////////////////////////////////
    // Now (re-) point the camera if requested
    ////////////////////////////////////////////////////////////////////////

    double val = 0.;
    zvparmd("XOFF", &val, &count, &def, 1, 0);
    if (count != 0) {
	snprintf(msg, MSG_LEN, "X offset overridden:  was %lf, now %lf", x_offset, val);
	zvmessage(msg, "");
	x_offset = val;
    }
    zvparmd("YOFF", &val, &count, &def, 1, 0);
    if (count != 0) {
	snprintf(msg, MSG_LEN, "Y offset overridden:  was %lf, now %lf", y_offset, val);
	zvmessage(msg, "");
	y_offset = val;
    }

    snprintf(msg, MSG_LEN, "Camera model offsets: y=%lf, x=%lf", y_offset, x_offset);
    zvmessage(msg, "");

    zvparmd("PPARAM", pointing, &num_pointing, &def, PIG_MAX_PARAMS, 0);

    int az_count, el_count;
    zvparmd("AZIMUTH", &azimuth, &az_count, &def, 1, 0);
    zvparmd("ELEVATION", &elevation, &el_count, &def, 1, 0);
    if (az_count != el_count) {
	zvmessage("If one of AZIMUTH or ELEVATION are specified, both must be",
								"");
	zabend();
    }

    if (az_count != 0) {
	PigCoordSystem *point_cs = pointing_out->getCoordSystem();
	point_cs->convertAzElDegrees(azimuth, elevation,
			pointing[0], pointing[1], param_cs);

	snprintf(msg, MSG_LEN, "Az/el converted to azimuth=%lf, elevation=%lf",
					pointing[0], pointing[1]);
	zvmessage(msg, "");

	if (num_pointing < 2)
	    num_pointing = 2;
    }

    if (num_pointing != 0) {
	pointing_out->setPointingParameters(pointing, num_pointing);
    }

    zvmessage("Pointing parameters used:", "");
    num_pointing = pointing_out->getPointingParamCount();
    pointing_out->getPointingParameters(pointing, num_pointing);
    for (i=0; i < num_pointing; i++) {
	snprintf(msg, MSG_LEN, "Parameter %d: %s = %lf", i,
			pointing_out->getPointingParamName(i), pointing[i]);
	zvmessage(msg, "");
    }

    // Adjust the position of the camera

    double vec[3];
    zvparmd("POSITION", vec, &count, &def, 3, 0);
    if (count != 0) {
	position.setXYZ(vec);
	pointing_out->setCameraPosition(position, param_cs);
	zvmessage("WARNING: Camera position reset by request", "");
    }

    ////////////////////////////////////////////////////////////////////////
    // Load in the mosaic and its parameters
    ////////////////////////////////////////////////////////////////////////

    zvp("INP", filename, &count);
    if (count != 1) {		// should never happen
	zvmessage("Internal ERROR: INP count is not 1!", "");
	zabend();
    }

    mosaic_file = PigFileModel::create(filename);
    if (mosaic_file == NULL) {
	zvmessage("Unable to create file model for mosaic INP file", "");
	zabend();
    }

    status = mosaic_file->getCylindricalProjectionParams(
		&surface_model, scale_y, scale_x, proj_origin, line_zero_el,
		az_first_sample, az_last_sample, min_elev, max_elev, &proj_cs);
    surface_model->setCoordSystem(proj_cs);	// just in case...
    azdir = proj_cs->getAzimuthDirection();

    // Determine output format

    mosaic_file->openFile();
    status = zvget(mosaic_file->getUnit(), "FORMAT", format, NULL);
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

    snprintf(msg, MSG_LEN, "Mosaic is projected in the %s coordinate frame",
		proj_cs->getFrameName());
    zvmessage(msg, "");
    snprintf(msg, MSG_LEN, "Output camera model is in the %s coordinate frame",
		camera_cs->getFrameName());
    zvmessage(msg, "");

    zvmessage("Mosaic Paramters:", "");

    snprintf(msg, MSG_LEN, "Elevation minimum %f, Elevation maximum %f",
		PigRad2Deg(min_elev), PigRad2Deg(max_elev));
    zvmessage(msg, "");
    snprintf(msg, MSG_LEN, "Left Azimuth %f, Right Azimuth %f",
		PigRad2Deg(az_first_sample), PigRad2Deg(az_last_sample));
    zvmessage(msg, "");

    snprintf(msg, MSG_LEN, "Projection origin = (%f, %f, %f)",
		proj_origin.getX(), proj_origin.getY(), proj_origin.getZ());
    zvmessage(msg, "");

    snprintf(msg, MSG_LEN, "Y Pixel scale: %f radians/pixel or %f pixels/degree",
		scale_y, 1.0/PigRad2Deg(scale_y));
    zvmessage(msg, "");
    snprintf(msg, MSG_LEN, "X Pixel scale: %f radians/pixel or %f pixels/degree",
		scale_x, 1.0/PigRad2Deg(scale_x));
    zvmessage(msg, "");

    nlo = mosaic_file->getNL();		// nlo/nso is mosaic size;
    nso = mosaic_file->getNS();		// nl/ns is output size.

    snprintf(msg, MSG_LEN, "Mosaic lines = %d, samples = %d", nlo, nso);
    zvmessage(msg, "");

    snprintf(msg, MSG_LEN, "line of zero elevation = %f", line_zero_el);
    zvmessage(msg, "");

    snprintf(msg, MSG_LEN, "Output lines = %d, samples = %d", nl, ns);
    zvmessage(msg, "");

    if (nso+1 > MAX_OBUF) {
	zvmessage("Mosaic buffer too short for mosaic width.", "");
	zabend();
    }
    if ((nlo > MAX_OBUF) || (nlo < 1) || (nso < 1)) {
	zvmessage("Unreasonable mosaic file dimensions", "");
	zabend();
    }

    if (ns > MAX_OBUF) {
	zvmessage("Output buffer too short for output width.", "");
	zabend();
    }
    if ((nl > MAX_OBUF) || (nl < 1) || (ns < 1)) {
	zvmessage("Unreasonable output file dimensions", "");
	zabend();
    }

    ////////////////////////////////////////////////////////////////////////
    // Create the output file
    ////////////////////////////////////////////////////////////////////////

    // zvselpi already called for example file
    zvunit(&unit_out, "OUT", 1, NULL);
    zvopen(unit_out, "OP", "WRITE", "U_FORMAT", format,
               "U_NS", ns, "U_NL", nl, "OPEN_ACT", "AS",
               "U_NB", 1, "U_ORG", "BSQ", "O_FORMAT", format, NULL);

    // Write the output labels
    PigLabelModel *labelModel = m->createLabelModel(unit_out);

    zvplabel(unit_out, 0, 1);

    labelModel->writeCM(camera_out, camera_cs);
    //
//!!!! write pointing etc. to file
//!!!! lotsa lotsa labels!!

    ////////////////////////////////////////////////////////////////////////
    // Allocate buffers and read in the mosaic
    // The buffer is one pixel wider.  We use this if the mosaic wraps
    // around to allow bilinear interpolation over the wrap.
    ////////////////////////////////////////////////////////////////////////

    int full_360 =
	(fabs(az_first_sample - az_last_sample - PigDeg2Rad(360.0))) <= .00001;
    if (full_360)
	zvmessage("Full 360 degree mosaic detected", "");

    zvmessage("Reading mosaic...", "");

    if (short_int_data) {
	ibuf = (short int *)malloc(nlo * (nso+1) * sizeof(short int));
	if (ibuf == NULL) {
	    zvmessage("Unable to allocate memory to read mosaic!!", "");
	    zabend();
	}
	oibuf = (short int *)malloc(ns * sizeof(short int));
	if (oibuf == NULL) {
	    zvmessage("Unable to allocate memory for output buffer!!", "");
	    zabend();
	}
	for (i=0; i < nlo; i++) {
	    zvread(mosaic_file->getUnit(), &IBUF(i,0), "LINE", i+1, NULL);
	    if (full_360)
		IBUF(i,nso) = IBUF(i,0);
	    else
		IBUF(i,nso) = IBUF(i,nso-1);
	}
    }
    else {
	fbuf = (float *)malloc(nlo * (nso+1) * sizeof(float));
	if (fbuf == NULL) {
	    zvmessage("Unable to allocate memory to read mosaic!!", "");
	    zabend();
	}
	ofbuf = (float *)malloc(ns * sizeof(float));
	if (ofbuf == NULL) {
	    zvmessage("Unable to allocate memory for outbuf buffer!!", "");
	    zabend();
	}
	for (i=0; i < nlo; i++) {
	    zvread(mosaic_file->getUnit(), &FBUF(i,0), "LINE", i+1, NULL);
	    if (full_360)
		FBUF(i,nso) = FBUF(i,0);
	    else
		FBUF(i,nso) = FBUF(i,nso-1);
	}
    }

    ////////////////////////////////////////////////////////////////////////
    // Loop through each sample of the output, and pick up the input
    // pixel that corresponds to it
    ////////////////////////////////////////////////////////////////////////

    zvmessage("Generating output...", "");

    for (j=0; j<nl; j++) {			// line loop
	    
	for (i=0; i<ns; i++) {			// sample loop

	    PigPoint origin;
	    PigVector look;
	    PigPoint surf_pt;
	    int hits;

	    if (short_int_data)
		oibuf[i] = 0;
	    else
		ofbuf[i] = 0.0;

	    // Convert to phys image coords (compensate for sub-areas)

	    double in_line = j + y_offset;
	    double in_samp = i + x_offset;

	    // Construct view ray for this pixel from the output camera,
	    // intersect with surface, and compute the azimuth and elevation
	    // in the mosaic.

	    camera_out->LStoLookVector(in_line, in_samp, origin, look, proj_cs);
	    hits = surface_model->intersectRay(origin, look, surf_pt);
	    // getRay returns "look"
	    surface_model->getRay(proj_origin, surf_pt, (hits <= 0), look);
	    double out_az = proj_cs->getAz(look);
	    double out_el = proj_cs->getEl(look);

	    // Convert mosaic az/el to line/sample space

	    double new_az = (out_az - az_first_sample) * azdir;
	    if (new_az >= PigDeg2Rad(360.0)) new_az -= PigDeg2Rad(360.0);
	    if (new_az < 0) new_az += PigDeg2Rad(360.0);
	    double mosaic_samp = new_az / scale_x;
	    double mosaic_line = line_zero_el - (out_el / scale_y);

	    // Make sure we are on the mosaic, and wrap around a 360

	    if (mosaic_line < 0 || mosaic_line >= nlo)
		continue;			// off the mosaic
	    if (do_interp && mosaic_line >= nlo-1)
		continue;			// can't interp last row
	    if (full_360) {		// adjust for overlap
		if (mosaic_samp >= nso)
		    mosaic_samp -= nso;
		if (mosaic_samp < 0)
		    mosaic_samp += nso;
	    }
	    else {			// last column adjusted for during read
	        if (mosaic_samp < 0 || mosaic_samp >= nso)
		    continue;			// off the mosaic
	    }

	    double dn;

	    if (do_interp) {
		// interpolate in the input image 
		// (bilinear interpolation)
			
		int m = (int) mosaic_samp;
		int n = (int) mosaic_line;

		double wr = mosaic_samp - m;
		double wl = 1.0 - wr;

		double wb = mosaic_line - n;

		double top, bot;

	        if (short_int_data) {
		    top = wl * IBUF(n,  m) + wr * IBUF(n,  m+1);
		    bot = wl * IBUF(n+1,m) + wr * IBUF(n+1,m+1);

		    dn = bot*wb + top * (1.0-wb);

		    if (dn <= 0.0)
			dn = 0.0;
		    if (dn > 32766.0)
			dn = 32766.0;
		    oibuf[i] = (short int)(dn + 0.5);
		}
		else {
		    top = wl * FBUF(n,  m) + wr * FBUF(n,  m+1);
		    bot = wl * FBUF(n+1,m) + wr * FBUF(n+1,m+1);

		    dn = bot*wb + top * (1.0-wb);
		    ofbuf[i] = dn;
		}
	    }
	    else {			// Don't interpolate
	        if (short_int_data) {
		    dn = IBUF((int)(mosaic_line + 0.5),
			   (int)(mosaic_samp+0.5));

		    if (dn <= 0.0)
			dn = 0.0;
		    if (dn > 32766.0)
			dn = 32766.0;
		    oibuf[i] = (short int)(dn + 0.5);
	        }
		else {
		    dn = FBUF((int)(mosaic_line + 0.5),
			   (int)(mosaic_samp+0.5));
		    ofbuf[i] = dn;
		}
	    }

	}				// sample loop

	if (short_int_data)
	    zvwrit(unit_out, oibuf, "LINE", j+1, NULL);
	else
	    zvwrit(unit_out, ofbuf, "LINE", j+1, NULL);
    }					// line loop

    zvclose(unit_out, NULL);

}

