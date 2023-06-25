/* marscahv */
#include "vicmain_c"

#include "mars_support.h"

#include "PigMission.h"
#include "PigFileModel.h"
#include "PigLabelModel.h"
#include "PigCameraModel.h"
#include "PigPointingModel.h"
#include "PigSurfaceModel.h"
#include "PigSurfaceSphere.h"
#include "PigCameraMapper.h"
#include "PigCameraMapEntry.h"
#include "PigRoverStateManager.h"
#include "SimpleImage.h"

#include "return_status.h"

// buffer sizes in main program - these should be made dynamic, eventually!
#define MAX_INPUTS 1
#define MAX_NL MARS_MAX_NL
#define MAX_NS MARS_MAX_NS
#define MAX_NB MARS_MAX_NB
#define MAX_OBUF 30000
#define LIN_MODE_NOMINAL "NOMINAL"
#define LIN_MODE_ACTUAL "ACTUAL"
#define LIN_MODE_MIN "MIN"
#define LIN_MODE_MAX "MAX"
#define LIN_MODE_LINEAR "LINEAR"

#ifdef MAX
#undef MAX
#endif
#ifdef MIN
#undef MIN
#endif
#define MAX(a,b) (((a)>(b))?(a):(b))
#define MIN(a,b) (((a)<(b))?(a):(b))

////////////////////////////////////////////////////////////////////////

void main44()
{
  int status, count, def;
  const int BUF_LEN = 150;
  char msg[BUF_LEN];

  int band;           // 0 means all bands, >0 means one band
  int band_count;     // # of bands on output, # of bands used on input
  int nids;
  char mission[64], instrument[64];

  PigSurfaceModel *surface_model;

  // Inputs

  PigFileModel *file_models[MAX_INPUTS];
  PigCameraModel *camera_in[MAX_INPUTS];
  PigPointingModel *pointing_in[MAX_INPUTS];
  int xdim_in, ydim_in, xdim_in2, ydim_in2;
  int homogeneous_inputs = TRUE;
  SimpleImage<short int> *short_int_images[MAX_INPUTS];
  memset(short_int_images, 0, sizeof(short_int_images));
  SimpleImage<float> *float_images[MAX_INPUTS];
  memset(float_images, 0, sizeof(float_images));
  RadiometryModel *radiometric[MAX_INPUTS];
  PigCoordSystem *output_cs;
  PigCoordSystem *fixed_cs;		// Used only for surface model init
  int short_int_data = 1;
  int do_interp = 1;
  char format[10];                   // input data format

  // Outputs

  int unit_out;
  int nlo, nso;
  double x_offset, y_offset;
  PigCameraModel *camera_out;
  PigPointingModel *pointing_out;

  int do_print = TRUE;		// True if info message should be issued

  // User Parameters

  zvmessage("MARSCAHV version 2020-02-18", "");

  // Get the input file name, and set up initial camera/pointing model
  // for it.  Although we accept only single file input, mars_setup
  // does lots of other nice things for us.

  mars_setup(nids, file_models, camera_in, pointing_in, surface_model,
	     radiometric, output_cs, mission, instrument,homogeneous_inputs,
	     MAX_NL, MAX_NS, MAX_INPUTS);

  PigMission *m = PigMission::getMissionObject(mission);

  // Repoint the camera to undo any nav file correction.  Note that we
  // want to look at the nav file to determine pointing model type, but
  // don't actually want the correction until *after* the model has been
  // aligned with its stereo partner.

  // find out the input data type.  Currently we support SHORT INT images 
  // and FLOAT images.  BYTE image are supported by converting bytes 
  // into short ints.

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

  // get parameter overrides if any
  band = 0;
  band_count = 1;
  zvp("BAND", &band, &count);
  if (count == 0) {
    // No input band specified; process all bands.
    band_count = file_models[0]->getNB();
    snprintf(msg, BUF_LEN, "Number of bands to be processed is (%d)", band_count);
    zvmessage(msg, "");
  } else {
    // check if input band number is greater than number of bands in input
    if (band > file_models[0]->getNB()) {
      snprintf(msg, BUF_LEN, "Input band (%d) is greater than number of bands in "
	       "input image. Band set to 1.", band);
      zvmessage(msg, "");
      band = 1;
    }
  }

  // Fixed frame used only to create surface model (so SM params are in
  // that frame).  It is then transformed to the output frame for the rest
  // of the run.

  fixed_cs = m->getFixedCS();

  // Check the INTERP parameter.
  do_interp = zvptst("INTERP");

  // Reads the SURFACE parameter
  // marscahv is unique in defining default surface model
  // as a unit sphere.  Because of that, we override
  // whatever surface mars_setup created if default has
  // not been changed.
  char surface[10];
  zvp("SURFACE", surface, &count);
  if((count > 0) && (strcasecmp(surface, "SPHERE") == 0)) {
    // Create surface model based on the first input.  Looks at user
    // parameters.
    double radius;
    zvparmd("RADIUS", &radius, &count, &def, 1, 0);
    surface_model = new PigSurfaceSphere(radius, fixed_cs);
  }

  snprintf(msg, BUF_LEN, "Mosaic's surface model parameters are specified in the %s "
	   "coordinate frame", surface_model->getCoordSystem()->getFrameName());
  if (do_print) zvmessage(msg, "");

  surface_model->setCoordSystem(output_cs);

  // Check for un-subframing.  If the flag is set, we create an output
  // the size of the nominal frame, put the image at the right place within
  // that frame (based on the subframe start), and make sure the camera
  // model matches.

  int unsub = zvptst("unsub");

  // set input image size dimensions
  xdim_in = file_models[0]->getNS();
  ydim_in = file_models[0]->getNL();
  xdim_in2 = file_models[0]->getNS();
  ydim_in2 = file_models[0]->getNL();

  // Check for input image size override
  zvpcnt("fullsize", &count);
  if (count > 0) {
    int size[2];
    zvp("fullsize", size, &count);
    xdim_in = size[0];
    ydim_in = size[1];
    xdim_in2 = size[0];
    ydim_in2 = size[1];
    snprintf(msg, BUF_LEN, "Override: Input FullSize lines & samples=%10d %10d", 
	     xdim_in, ydim_in);
    zvmessage(msg, "");
  }

  // set output picture dimensions
  nlo = file_models[0]->getNL();
  nso = file_models[0]->getNS();
  snprintf(msg, BUF_LEN, "Output # lines & samples=%10d %10d", nlo, nso);
  zvmessage(msg, "");
  if(nso > MAX_OBUF){
    zvmessage("Output buffer too short for picture width.", "");
    zabend();
  }

  // If we are un-subframing, set the output size to the nominal size
  // of this camera frame.  Set the input size too.

  if (unsub) {
    int new_nlo = 1024;		// defaults
    int new_nso = 1024;
    PigXerces::initialize();
    PigCameraMapper *map = new PigCameraMapper(NULL, m->getHostID());
    if (map == NULL)
      zvmessage("Unable to create CameraMapper.  Assuming 1024x1024 "
		"nominal camera","");
    else {
      PigCameraMapEntry *entry = map->findFromID(instrument);
      if (entry == NULL)
	zvmessage("Unable to find camera in CameraMap.  Assuming "
		  "1024x1024 nominal camera","");
      else {
	new_nlo = entry->getNL();
	new_nso = entry->getNS();
      }
    }
    // Adjust for downsampling
    new_nlo /= file_models[0]->getDownsampleYFactor(1.0);
    new_nso /= file_models[0]->getDownsampleXFactor(1.0);

    if (nlo != new_nlo || nso != new_nso) {
      snprintf(msg, BUF_LEN, "Un-subframing: output line/samp changed to %d,%d",
	       new_nlo, new_nso);
      zvmessage(msg, "");
    }
    nlo = new_nlo;
    nso = new_nso;
    xdim_in = nso;
    ydim_in = nlo;
    xdim_in2 = nso;
    ydim_in2 = nlo;
  }
 
  // Check for image size override
  zvpcnt("outsize", &count);
  if (count > 0) {
    int size[2];
    zvp("outsize", size, &count);
    nlo = size[0];
    nso = size[1];
    snprintf(msg, BUF_LEN, "Override: Output lines & samples=%10d %10d", nlo, nso);
    zvmessage(msg, "");
  }

  // Compute an output camera model which is a synthesis of both left and
  // right models, if they exist, in order to optimize stereo viewing.
  // We are assuming that all calibration camera models were created pointing
  // the same way.  If we don't make such assumtion, we would be forced to 
  // assume that left pointing could be applied to the right eye, since we 
  // have no pointing information for the stereo_partner.  So having a choice
  // between these two assumptions, we chose the first one since in 
  // our view it is the more likely assumption.  Given the stereo_partner, 
  // we assume that it's pointed exactly the same as the main image.
  // More specifically camera_models created from the file_models are 
  // pointed the same, as we assumed for the calibration_models.  Usually it
  // will be the calibration_models, except for the GENERIC case, where it 
  // would be label-based camera models.


  // Get the model of the first input    
  PigCameraModel *camera = NULL;
  // get the model for its "stereo partner"
  PigCameraModel *camera_partner = NULL;

  PigFileModel *partner_fm = NULL;
  int have_partner = FALSE;

  // check for stereo partner override
  char stereo_partner[PIG_MAX_FILENAME_SIZE+1];
  zvp("STEREO_PARTNER", stereo_partner, &count);
  if (count > 0) {
    // In case when we have stereo partner as input parameter,
    // we use file names/models to create camera model
    // then align them.  If nav file correction
    // is applied, do it after the alignment.

    camera = PigCameraModel::create(file_models[0], NULL);
    // Point camera model

    //Create camera model for the "stereo partner"
    partner_fm = PigFileModel::create(stereo_partner);
    if (partner_fm == NULL) {
      zvmessage("Could not create stereo partner file model!", "");
      zabend();
    }

    // Adjust the output size to the min or max of the inputs if desired

    int partner_ns = partner_fm->getNS();
    int partner_nl = partner_fm->getNL();
    if (zvptst("MAXSIZE")) {
      nso = MAX(nso, partner_ns);
      nlo = MAX(nlo, partner_nl);
    } else if (zvptst("MINSIZE")) {
      nso = MIN(nso, partner_ns);
      nlo = MIN(nlo, partner_nl);
    }

    if (!unsub) {	// size of second input, but unsub remains full frame
      xdim_in2 = partner_ns;
      ydim_in2 = partner_nl;
    }

    // Add the coord sys (in case we're doing long-baseline)
    PigRoverStateManager *rsm = m->getRoverStateManager();
    if (rsm)
      rsm->addFileCoordSystems(partner_fm);

    camera_partner = PigCameraModel::create(partner_fm, NULL);
    have_partner = TRUE;
	
    // Because the inputs are separate, we have to un-subframe them
    // separately too.

    if (unsub) {
      int sl = file_models[0]->getFirstLine(1) - 1;	// make 0 based
      int ss = file_models[0]->getFirstLineSample(1) - 1;

      // Adjust for downsampling
      sl /= file_models[0]->getDownsampleYFactor(1.0);
      ss /= file_models[0]->getDownsampleXFactor(1.0);

      // Note: when un-subframing, we have to set initial from current
      // or it won't "stick" when the camera is pointed.

      camera->shiftCamera(-ss, -sl);
      camera->setInitialFromCurrent();

      snprintf(msg, BUF_LEN, "Removing subframe of sl=%d, ss=%d from image",
	       sl+1, ss+1);
      zvmessage(msg, "");

      // Now the partner

      sl = partner_fm->getFirstLine(1) - 1;
      ss = partner_fm->getFirstLineSample(1) - 1;
      // Adjust for downsampling
      sl /= partner_fm->getDownsampleYFactor(1.0);
      ss /= partner_fm->getDownsampleXFactor(1.0);
      camera_partner->shiftCamera(-ss, -sl);
      camera_partner->setInitialFromCurrent();

      snprintf(msg,BUF_LEN, "Removing subframe of sl=%d, ss=%d from stereo partner",
	       sl+1, ss+1);
      zvmessage(msg, "");

    }
  }
  else {
    camera = PigCameraModel::create(
				    camera_in[0]->getMissionName(),
				    camera_in[0]->getInstrumentName(),
				    camera_in[0]->getCameraVersion(),
				    camera_in[0]->getCameraSubtype(),
				    NULL,  //special
				    camera_in[0]->getCameraConstruction(),
				    camera_in[0]->getCameraCalibration(),
				    NULL);

    if (camera == NULL) {
      const int BUF_LEN = 256;
      char msg[BUF_LEN];
      snprintf(msg, BUF_LEN, "Failed to create camera model for Mission: %s, "
	       "Instrument: %s!", camera_in[0]->getMissionName(), 
	       camera_in[0]->getInstrumentName());
      zvmessage(msg, "");
      zabend();
    }
    camera_partner = PigCameraModel::create(
					    camera_in[0]->getMissionName(),
					    camera_in[0]->getInstrumentName(),
					    camera_in[0]->getCameraVersion(),
					    camera_in[0]->getCameraSubtype(),
					    "stereo", //special
					    camera_in[0]->getCameraConstruction(),
					    camera_in[0]->getCameraCalibration(),
					    NULL);

    // Un-subframe if necessary.  Same as the partner case above except
    // that there's only one file so they're both subframed the same.

    if (unsub) {
      int sl = file_models[0]->getFirstLine(1) - 1;	// make 0 based
      int ss = file_models[0]->getFirstLineSample(1) - 1;

      // Adjust for downsampling
      sl /= file_models[0]->getDownsampleYFactor(1.0);
      ss /= file_models[0]->getDownsampleXFactor(1.0);

      camera->shiftCamera(-ss, -sl);
      camera->setInitialFromCurrent();

      camera_partner->shiftCamera(-ss, -sl);
      camera_partner->setInitialFromCurrent();

      snprintf(msg, BUF_LEN, "Removing subframe of sl=%d, ss=%d from image",
	       sl+1, ss+1);
      zvmessage(msg, "");
    }

  }

  // Create Pointing Model based on the main input. 
  pointing_out = PigPointingModel::create(camera,
					  camera->getMissionName(), 
					  camera->getInstrumentName(),
					  pointing_in[0]->getModelName(),
					  true);

  // If we have a partner, we must point the cameras *before* alignment,
  // and NOT after.  This is because the pointings might not be the same
  // (think arm stereo, repointed stereo, long baseline, cross-instrument
  // coregistration, etc).  We also apply any navtable corrections here
  // (separately, per .

  if (have_partner) {
    pointing_in[0]->setCameraModel(camera);
    pointing_in[0]->pointCamera(file_models[0]);
    status = mars_apply_navtable(1, pointing_in, file_models);
    if (status >= 2)		// 1 means user didn't ask for file
      zabend();			// msg already printed   

    // Create and point the partner.

    PigPointingModel *pointing_partner = mars_create_pointing_model(
								    camera_partner, partner_fm, "stereo partner");
    if (pointing_partner == NULL) {
      zabend();			// message already printed
    }
  }

  camera_out = camera->alignStereoCameras(xdim_in, ydim_in,
					  xdim_in2, ydim_in2,
					  nso, nlo,
					  camera_partner);

  // Now that alignment is done and we have new camera model, update
  // Pointing Model to use that model.  But we only repoint if this is
  // a synthetic partner - an actual partner was pointed above.
  // For the non-partner case, we actually could point before OR after,
  // but doing it after preserves consistency with other code pathways
  // and makes it easier for nav files to match.

  pointing_out->setCameraModel(camera_out);
  if (!have_partner) {
    pointing_out->pointCamera(file_models[0]);

    //Apply nav file correction, if any, to the output pointing
    status = mars_apply_navtable(1, &pointing_out, file_models);
    if (status >= 2)		// 1 means user didn't ask for file
      zabend();		// msg already printed   
  }

  // Don't need original input's camera models anymore.
  delete camera;
  delete camera_partner;

  // get offsets between camera l/s coords and physical image
  // For MER offset is always (0, 0) as camera model is computed
  // for given subframe.  So if the full frame image had (C1, A1,...)
  // camera model, then subframe will have different (C2, A2, ...) camera
  // model applicable specifically to this subframe.
  // For older missions that is not the case.  All subframes will
  // have the same (C1, A1, ...) camera model as given full frame.
  // If full frame size is not supplied as an input parameter, we assume
  // that full frame size is (x_offset+NS, y_offset+NL) which most likely
  // is wrong, so it is very important to pass input size of a full frame
  // whenever dealing with subframes for missions other than MER.

  x_offset = file_models[0]->getXOffset();
  y_offset = file_models[0]->getYOffset();
  snprintf(msg, BUF_LEN, "Line offset %10f Sample offset %10f", y_offset, x_offset);
  zvmessage(msg, "");

  // Check for output offset override
  zvpcnt("outoff", &count);
  if (count > 0) {
    double off[2];
    zvparmd("outoff", off, &count, &def, 2, 0);
    y_offset = off[0];
    x_offset = off[1];
    snprintf(msg, BUF_LEN, "Override: Line offset %10f Sample offset %10f",
	     y_offset, x_offset);
    zvmessage(msg, "");
  }

  // Check for input image size override
  zvpcnt("fullsize", &count);
  if (count > 0) {
    int size[2];
    zvp("fullsize", size, &count);
    xdim_in = size[0];
    ydim_in = size[1];
    snprintf(msg, BUF_LEN, "Override: Input lines & samples=%10d %10d", nlo, nso);
    zvmessage(msg, "");
    //!!!! nothing is ever done with the above so.....
    zvmessage("Warning: FULLSIZE is not properly implemented currently","");
  }

  // open output file
  if ((nlo > MAX_OBUF) || (nlo < 1) || (nso > MAX_OBUF) || (nso < 1)) {
    zvmessage("Unreasonable output file dimensions", "");
    zabend();
  }

  zvselpiu(file_models[0]->getUnit());	  // transfer labels
  status=zvunit(&unit_out, "OUT", 1, NULL);
  status=zvopen(unit_out, "OP", "WRITE", "U_FORMAT", format,
		"U_NS", nso, "U_NL", nlo, "OPEN_ACT", "AS",
		"U_NB", band_count, "U_ORG", "BSQ", "O_FORMAT", format, NULL);
 
  // Write the output labels
  char point_method[1024];
  char *pointMethodValue = NULL;
  char *linPartnerType = NULL;
  PigModelBase::getStaticParam("POINT_METHOD", point_method, &count, 1, 0);
  if (count == 1) 
    pointMethodValue = PigModelBase::parseParamString(point_method, "cahv_fov");

  if (pointMethodValue) {
    if (!strncasecmp(pointMethodValue, "MAX", 3) ||
	!strncasecmp(pointMethodValue, "UNION", 5))
      pointMethodValue = LIN_MODE_MAX;
    else if (!strncasecmp(pointMethodValue, "MIN", 3) ||
	     !strncasecmp(pointMethodValue, "INTERSECT", 9))
      pointMethodValue = LIN_MODE_MIN;
    else if (!strncasecmp(pointMethodValue, "LINEAR", 6))
      pointMethodValue = LIN_MODE_LINEAR;
    else
      pointMethodValue = LIN_MODE_MIN;
  } else {
    pointMethodValue = LIN_MODE_MIN;
  }

  if (have_partner)
    linPartnerType = LIN_MODE_ACTUAL;
  else
    linPartnerType = LIN_MODE_NOMINAL;
        
  PigLabelModel *labelModel = m->createLabelModel(unit_out);
  labelModel->setLinearized(file_models, nids, linPartnerType, pointMethodValue,
			    partner_fm);
  labelModel->writeCM(camera_out, output_cs);
  zvplabel(unit_out, 0, 1);

  // Write output as zero's.  This is so we can random-write each line
  // later in separate threads.  May not be strictly necessary to pre-write
  // the file, but it's insurance.

  if (short_int_data) {
    short int *obuf_si[MAX_NB];
    for (int b = 0; b < band_count; b++) {
      obuf_si[b] = new short int[nso];
      if (obuf_si[b] == NULL) {
	zvmessage("Unable to allocate obuf memory!","");
	zabend();
      }
     
      memset(obuf_si[b], 0, nso * sizeof(short int));
      for (int j = 0; j < nlo; j++) 
	status = zvwrit(unit_out, obuf_si[b], "LINE", j + 1, "BAND", 
			b + 1, NULL);
    }
  }
  else {
    float *obuf_f[MAX_NB];
    for (int b = 0; b < band_count; b++) {
      obuf_f[b] = new float[nso];
      if (obuf_f[b] == NULL) {
	zvmessage("Unable to allocate obuf memory!","");
	zabend();
      }

      memset(obuf_f[b], 0, nso * sizeof(float));
      for (int j = 0; j < nlo; j++) 
	status = zvwrit(unit_out, obuf_f[b], "LINE", j + 1, "BAND", 
			b + 1, NULL);
    }
  }

  // reopen output for update
  status=zvclose(unit_out, NULL);
  status=zvopen(unit_out, "OP", "UPDATE", "U_FORMAT", format,
		"U_NS", nso, "U_NL", nlo, "OPEN_ACT", "AS",
		"U_NB", band_count, "U_ORG", "BSQ", "O_FORMAT", format, NULL);
    
  // Read a set of inputs into memory
  if (short_int_data) { 
    mars_read_inputs(0, 0, file_models, short_int_images, MAX_NL, MAX_NS, 
		     band, radiometric);
  } else {
    mars_read_inputs(0, 0, file_models, float_images, MAX_NL, MAX_NS, band, 
		     radiometric);
  }
    
  int omp_on = zvptst("OMP_ON");

  // Loop through each line, and then sample, of the output, and pick up the
  // input pixel that corresponds to it    

#pragma omp parallel for schedule(dynamic) if (omp_on)
  for (int j=0; j<nlo; j++) {			// line loop

    // Must declare these here so they're thread-private...
    // (actually there are ways in the #pragma omp to declare that, but
    // this is far simpler...)

    short int *obuf_si[MAX_NB];
    float *obuf_f[MAX_NB];

    if (j%100 == 0) {
      snprintf(msg, BUF_LEN, "line %d", j);
      zvmessage(msg, "");
    }

    // Zero output line
    if (short_int_data) {
      for (int b = 0; b < band_count; b++) {
	obuf_si[b] = new short int[nso];
	if (obuf_si[b] == NULL) {
	  zvmessage("Unable to allocate obuf memory!","");
	  zabend();
	}

	memset(obuf_si[b], 0, nso * sizeof(short int));
      }
    } else {
      for (int b = 0; b < band_count; b++) {
	obuf_f[b] = new float[nso];
	if (obuf_f[b] == NULL) {
	  zvmessage("Unable to allocate obuf memory!","");
	  zabend();
	}

	memset(obuf_f[b], 0, nso * sizeof(float));
      }
    }

    for (int i=0; i<nso; i++) {		// sample loop

      // compute the camera-coordinate line and sample, and project
      // into space and then XYZ.	
      double out_samp = (double)i + x_offset;
      double out_line = (double)j + y_offset;

      PigPoint origin;
      PigVector look;
      PigPoint surf_pt;

      camera_out->LStoLookVector(out_line, out_samp, origin, look,
				 output_cs);

      int hits = surface_model->intersectRay(origin, look, surf_pt);
      int infinity = (hits <= 0);

      // int fileNS = file_models[0]->getNS();

      // convert output image coordinate to camera coordinates
      // in the input image	
      double in_line, in_samp;

      camera_in[0]->XYZtoLS(surf_pt, infinity, &in_line, &in_samp,
			    output_cs);

      // check if point is within the input image	
      if (file_models[0]->testPixelLocation(in_line,in_samp) != 0)
	continue;			// skip if not

      // Convert to phys image coords (compensate for sub-areas)
      double image_line = in_line - file_models[0]->getYOffset();
      double image_samp = in_samp - file_models[0]->getXOffset();

      double dn;

      if (do_interp) {
	// interpolate in the input image 
	// (bilinear interpolation)	
	int m = (int) image_samp;
	int n = (int) image_line;

	double wr = image_samp - m;
	double wl = 1.0 - wr;

	double wb = image_line - n;

	if (short_int_data) {
	  for (int b = 0; b < band_count; b++) {
	    short int ul = short_int_images[0]->get(b, n, m);
	    short int ur = short_int_images[0]->get(b, n, m + 1);
	    short int ll = short_int_images[0]->get(b, n + 1, m);
	    short int lr = short_int_images[0]->get(b, n + 1, m + 1);
 
	    double top = wl * ul + wr * ur;
	    double bot = wl * ll + wr * lr;

	    dn = bot * wb + top * (1.0-wb);

	    if (dn <= 0.0)
	      dn = 0.0;
	    if (dn > 32766.0)
	      dn = 32766.0;
	    obuf_si[b][i] = (short int)(dn + 0.5);
	  }
	}
	else {            // interpolate floats
	  for (int b = 0; b < band_count; b++) {
	    double ul = float_images[0]->get(b, n, m);
	    double ur = float_images[0]->get(b, n, m + 1);
	    double ll = float_images[0]->get(b, n + 1, m);
	    double lr = float_images[0]->get(b, n + 1, m + 1);
 
	    double top = wl * ul + wr * ur;
	    double bot = wl * ll + wr * lr;

	    dn = bot * wb + top * (1.0-wb);

	    obuf_f[b][i] = dn;
	  }
	}
      }
      else {                      // don't interpolate
	if (short_int_data) {
	  for (int b = 0; b < band_count; b++) {
	    dn = short_int_images[0]->get(b, 
					  (int)(image_line + 0.5), 
					  (int)(image_samp + 0.5));

	    if (dn <= 0.0)
	      dn = 0.0;
	    if (dn > 32766.0)
	      dn = 32766.0;
	    obuf_si[b][i] = (short int)(dn + 0.5);
	  }
	}
	else {
	  for (int b = 0; b < band_count; b++) {
	    dn = float_images[0]->get(b, 
				      (int)(image_line + 0.5), 
				      (int)(image_samp + 0.5));
	    obuf_f[b][i] = dn;
	  }
	}
      }
    }				// sample loop

    // The RTL is not re-entrant, so we have to put the write in a
    // critical section... since it's done per thread.

#pragma omp critical (rtl_output)
    {
      for (int b = 0; b < band_count; b++) {
	if (short_int_data)
	  zvwrit(unit_out, obuf_si[b], "LINE", j + 1, "BAND", b + 1, 
		 NULL);
	else
	  zvwrit(unit_out, obuf_f[b], "LINE", j + 1, "BAND", b + 1, 
		 NULL);
      }
    }
  }				// line loop

  zvclose(unit_out, NULL);
}
