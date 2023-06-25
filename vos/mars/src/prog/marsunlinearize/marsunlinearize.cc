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
#include "PigRoverStateManager.h"

#include "return_status.h"

// buffer sizes in main program - these should be made dynamic, eventually!
#define MAX_INPUTS 1
#define MAX_OPEN 1
#define MAX_NL MARS_MAX_NL
#define MAX_NS MARS_MAX_NS
#define MAX_OBUF 30000

//Assumes "fbuf", "fileNS" local variables.
#define FBUF(j,k) ( *(fbuf[(0)] + (j) * fileNS + (k)) )
#define FBUF2(j,k) ( *(fbuf2[(0)] + (j) * fileNS + (k)) )

////////////////////////////////////////////////////////////////////////

double getGSD(PigCameraModel *cam, int xc, int yc, PigCoordSystem *cs);


void main44()
{
    int i, j, k;
    int status, count, def;
    const size_t msgLen = 150;
    char msg[msgLen];

    int band;
    int nids;
    char mission[64], instrument[64];

    PigSurfaceModel *surface_model;

    // Inputs

    PigFileModel *file_models[MAX_INPUTS];
    PigCameraModel *camera_in[MAX_INPUTS];
    PigPointingModel *pointing_in[MAX_INPUTS];
    int xdim_in, ydim_in;
    int homogeneous_inputs = TRUE;
    float *fbuf[MAX_OPEN], *fbuf2[MAX_OPEN];
    double FOV;				                // cache for efficiency
    PigVector camera_orientation[MAX_INPUTS];		// cache for efficiency
    PigPoint camera_position[MAX_INPUTS];		// cache for efficiency
    RadiometryModel *radiometric[MAX_INPUTS];
    PigCoordSystem *output_cs;
    PigCoordSystem *fixed_cs;		// Used only for surface model init
    int do_interp = 1;
    char format[10];                   // input data format

    // Outputs

    int unit_out;
    int nlo, nso;
    double x_offset, y_offset;
    PigCameraModel *camera_out;
    PigCameraModel *camera_out_right;
    PigCameraModel *camera_in_right;
    PigPointingModel *pointing_out;
    float obuf_f[MAX_OBUF], obuf_f2[MAX_OBUF];

    // User Parameters

    zvmessage("*** MARSUNLINEARIZE version 2017-08-15 ***", "");

    // Get the input file name, and set up initial camera/pointing model
    // for it.  Although we accept only single file input, mars_setup
    // does lots of other nice things for us.

    mars_setup(nids, file_models, camera_in, pointing_in, radiometric,
		output_cs, mission, instrument,homogeneous_inputs,
                MAX_NL, MAX_NS, MAX_INPUTS);

    PigMission *m = PigMission::getMissionObject(mission);

    // find out the input data type.  Currently we support SHORT INT images 
    // and FLOAT images.  BYTE image are supported by converting bytes 
    // into short ints.

    file_models[0]->openFile();
    status = zvget(file_models[0]->getUnit(), "FORMAT", format, NULL);

    // Apply navtable corrections, if any

    status = mars_apply_navtable(nids, pointing_in, file_models);
    if (status >= 2)		// 1 means user didn't ask for file
	zabend();		// msg already printed

    // get parameter overrides if any
    zvp("BAND", &band, &count);

    // Fixed frame used only to create surface model (so SM params are in
    // that frame).  It is then transformed to the output frame for the rest
    // of the run.

    fixed_cs = m->getFixedCS();

    // Check the INTERP parameter.
    do_interp = zvptst("INTERP");

    // Reads the SURFACE parameter
    char surface[10];
    zvp("SURFACE", surface, &count);
    if((count > 0) && (strcasecmp(surface, "SPHERE") == 0)) {
        // Create surface model based on the first input.  Looks at user
        // parameters.
        double radius;
	zvparmd("RADIUS", &radius, &count, &def, 1, 0);
	surface_model = new PigSurfaceSphere(radius, fixed_cs);
    }
    else {
        surface_model = PigSurfaceModel::create(file_models[0]);
    }

    surface_model->setCoordSystem(output_cs);

    // set input image size dimensions
    xdim_in = file_models[0]->getNS();
    ydim_in = file_models[0]->getNL();

    // Check for input image size override
    zvpcnt("fullsize", &count);
    if (count > 0) {
	int size[2];
	zvp("fullsize", size, &count);
	xdim_in = size[0];
	ydim_in = size[1];
	snprintf(msg, msgLen, "Override: Input FullSize lines & samples=%10d %10d", 
		xdim_in, ydim_in);
	zvmessage(msg, "");
    }

    // Get the RSM so we can add all the other file coord sys's in
    PigRoverStateManager *rsm = m->getRoverStateManager();

    // Get the Left output camera model from the UNLIN_PAIR parameter (which
    // must be a pair of normal image files)

    char unlin_pair_filename[PIG_MAX_FILENAME_SIZE+1];
    zvpone("UNLIN_PAIR", unlin_pair_filename, 1, sizeof(unlin_pair_filename));

    PigFileModel *file_out = PigFileModel::create(unlin_pair_filename);
    if (rsm)
	rsm->addFileCoordSystems(file_out);
    camera_out = PigCameraModel::create(file_out, NULL);
    PigPointingModel *point_out = PigPointingModel::create(camera_out,
						file_out, NULL, true);
    point_out->pointCamera(file_out);

    //Apply nav file correction, if any, to the output pointing
    status = mars_apply_navtable(1, &point_out, file_models);
    if (status >= 2)		// 1 means user didn't ask for file
    	zabend();		// msg already printed   

    // Get the GSD of the Left output camera.
    double leftGSD = getGSD(camera_out, file_out->getNL()/2, 
                            file_out->getNS()/2, output_cs);



    // Get the Right output camera model from the UNLIN_PAIR parameter (which
    // must be a pair of normal image files)

    zvpone("UNLIN_PAIR", unlin_pair_filename, 2, sizeof(unlin_pair_filename));

    PigFileModel *file_out_right = PigFileModel::create(unlin_pair_filename);
    if (rsm)
	rsm->addFileCoordSystems(file_out_right);
    camera_out_right = PigCameraModel::create(file_out_right, NULL);
    point_out = PigPointingModel::create(camera_out_right,
						file_out_right, NULL, true);
    point_out->pointCamera(file_out_right);

    //Apply nav file correction, if any, to the output pointing
    status = mars_apply_navtable(1, &point_out, file_models);
    if (status >= 2)		// 1 means user didn't ask for file
    	zabend();		// msg already printed   

    // Get the GSD of the Right output camera.
    double rightGSD = getGSD(camera_out_right, file_out_right->getNL()/2, 
                             file_out_right->getNS()/2, output_cs);



    // delete point_out??? !!!!


    // Get the Right input camera model from the LIN_RIGHT parameter

    zvpone("LIN_RIGHT", unlin_pair_filename, 1, sizeof(unlin_pair_filename));

    PigFileModel *file_out_lin = PigFileModel::create(unlin_pair_filename);
    if (rsm)
	rsm->addFileCoordSystems(file_out_lin);
    camera_in_right = PigCameraModel::create(file_out_lin, NULL);
    point_out = PigPointingModel::create(camera_in_right,
					file_out_lin, NULL, true);
    point_out->pointCamera(file_out_lin);

    //Apply nav file correction, if any, to the output pointing
    status = mars_apply_navtable(1, &point_out, file_models);
    if (status >= 2)		// 1 means user didn't ask for file
    	zabend();		// msg already printed   




    // set output picture dimensions.  Use the unlin pair "left" since
    // that's ultimately what we're trying to match.

    nlo = file_out->getNL();
    nso = file_out->getNS();
    snprintf(msg, msgLen, "Output # lines & samples=%10d %10d", nlo, nso);
    zvmessage(msg, "");
    if(nso > MAX_OBUF){
        zvmessage("Output buffer too short for picture width.", "");
        zabend();
    }

    // Check for image size override
    zvpcnt("outsize", &count);
    if (count > 0) {
	int size[2];
	zvp("outsize", size, &count);
	nlo = size[0];
	nso = size[1];
	snprintf(msg, msgLen, "Override: Output lines & samples=%10d %10d", nlo, nso);
	zvmessage(msg, "");
    }

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

    x_offset = file_out->getXOffset();
    y_offset = file_out->getYOffset();
    snprintf(msg, msgLen, "Line offset %10f Sample offset %10f", y_offset, x_offset);
    zvmessage(msg, "");

    // Check for output offset override
    zvpcnt("outoff", &count);
    if (count > 0) {
	double off[2];
	zvparmd("outoff", off, &count, &def, 2, 0);
	y_offset = off[0];
	x_offset = off[1];
	snprintf(msg, msgLen, "Override: Line offset %10f Sample offset %10f",
							y_offset, x_offset);
	zvmessage(msg, "");
    }

    // Check for input image size override
    zvpcnt("fullsize", &count);
    if (count > 0) {
	int size[2];
	zvp("outsize", size, &count);
	xdim_in = size[0];
	ydim_in = size[1];
	snprintf(msg, msgLen, "Override: Input lines & samples=%10d %10d", nlo, nso);
	zvmessage(msg, "");
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
               "U_NB", 2, "U_ORG", "BSQ", "O_FORMAT", format, NULL);


    // write output as zero's
    for (i=0; i<nso; i++) {
	obuf_f[i]=0.0;
	obuf_f2[i]=0.0;
    }
    for (j=0; j<nlo; j++)  {
	status = zvwrit(unit_out, obuf_f, "LINE", j+1, "BAND", 1, NULL);
	status = zvwrit(unit_out, obuf_f, "LINE", j+1, "BAND", 2, NULL);
    }

    // reopen output for update
    status=zvclose(unit_out, NULL);
    status=zvopen(unit_out, "OP", "UPDATE", "U_FORMAT", format,
               "U_NS", nso, "U_NL", nlo, "OPEN_ACT", "AS",
               "U_NB", 2, "U_ORG", "BSQ", "O_FORMAT", format, NULL);
 
    // Calculate the approximate Field Of View for each input, for use in
    // a quick decision whether to project the point into this image or not.
    // Adding H and V FOV's covers the diagonals, plus provides some slop.
    // We divide by 2 because the H and V FOV's cover from edge to edge, while
    // we need only from the center to the edge.  The cosine is stored for
    // easy comparison with a dot product.  We limit the FOV to 90 degrees
    // (180 total) to avoid any back-projection (MER hazcam H+V is > 90).
    // We also compute and save the camera position and orientation here
    // once, rather than rederiving it for each pixel.

    // Note: FOV is disabled for marscahv because it is not necessary.

    /*
    FOV = cos((file_models[0]->getFOV(camera_in[0], 0) +
	       file_models[0]->getFOV(camera_in[0], 1)) / 2);
    if (FOV < 0.0)
	FOV = 0.0;		// Limit to 90 degrees
    */
    camera_position[0] = pointing_in[0]->getCameraPosition(output_cs);
    camera_orientation[0] = pointing_in[0]->getCameraOrientation(output_cs);

    int sliceSize = 0;
    int allocatedSize;
    //initilize allocatedSize array
    allocatedSize = 0;
    fbuf[0] = NULL;
    fbuf2[0] = NULL;


    //Calculate the needed memory size for allocation
    sliceSize = file_models[0]->getNL() * file_models[0]->getNS();

	if (sliceSize > allocatedSize ) {
	    if (fbuf[0] != NULL) {
		//Free previously allocated storage since it's not big enough
		delete[] fbuf[0];
	    }
	    if (fbuf2[0] != NULL) {
		//Free previously allocated storage since it's not big enough
		delete[] fbuf2[0];
	    }
	    fbuf[0] = new float[sliceSize];
	    fbuf2[0] = new float[sliceSize];
	    if (fbuf[0] == NULL || fbuf2[0] == NULL) {
		zvmessage("unable to allocate input memory array!!!", "");
		zabend();
	    }
	    allocatedSize = sliceSize;
	}
    
    // Read a set of inputs into memory
	mars_read_inputs(0, 0, file_models, fbuf, MAX_NL, MAX_NS, band, radiometric);
	mars_read_inputs(0, 0, file_models, fbuf2, MAX_NL, MAX_NS, band+1, radiometric);
    
    
    // Counter for number of valid (non 0) pixel in the output file
    long numPixValid = 0;

    // Loop through each sample of the output, and pick up the input
    // pixel that corresponds to it    
    for (j=0; j<nlo; j++) {			// line loop
      
	if (j%100 == 0) {
	    snprintf(msg, msgLen, "line %d", j);
	    zvmessage(msg, "");
	}
      
	// re-read output line
	    zvread(unit_out, obuf_f, "LINE", j+1, "BAND", 1, NULL); 
	    zvread(unit_out, obuf_f2, "LINE", j+1, "BAND", 2, NULL); 
	
	for (i=0; i<nso; i++) {		// sample loop
	    
	    if (obuf_f[i] != 0.0)
	       continue;	     // already set, do next iteration
	    
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
	    
	    int fileNS = file_models[0]->getNS();
	    
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
	    
	    /*
	      if (infinity) {
	      if ((look % camera_orientation[0]) < FOV)
	      continue;
	      }
	      else {
	      PigVector new_look = surf_pt - camera_position[0];
	      new_look.normalize();
	      if ((new_look % camera_orientation[0]) < FOV)
	      continue;
	      }
	    */ 
	    
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
	    
	    // Get the pixel value at this location.  Optionally interpolate
	    // the values to attempt to preserve some subpixel accuracy.

	    if (do_interp) {

		// bilinear interpolation

		int m = (int) image_samp;
		int n = (int) image_line;

		double wr = image_samp - m;
		double wl = 1.0 - wr;

		double wb = image_line - n;

		double top, bot;

		// If any of the interpolated pixels is 0 (not available),
		// give up on interpolation by weighting to the other side.
		// This is because we don't want to get a partial interpolation.

		if ((FBUF(n,m) == 0 && FBUF2(n,m) == 0) ||
		    (FBUF(n+1,m) == 0 && FBUF2(n+1,m) == 0)) {
			wl = 0.0;		// something on the left is 0
			wr = 1.0;
		}
		if ((FBUF(n,m+1) == 0 && FBUF2(n,m+1) == 0) ||
		    (FBUF(n+1,m+1) == 0 && FBUF2(n+1,m+1) == 0)) {
			wl = 1.0;		// something on the right is 0
			wr = 0.0;
		}
		if ((FBUF(n,m) == 0 && FBUF2(n,m) == 0) ||
		    (FBUF(n,m+1) == 0 && FBUF2(n,m+1) == 0)) {
			wb = 1.0;		// something on the top is 0
		}
		if ((FBUF(n+1,m) == 0 && FBUF2(n+1,m) == 0) ||
		    (FBUF(n+1,m+1) == 0 && FBUF2(n+1,m+1) == 0)) {
			wb = 0.0;		// something on the bottom is 0
		}

		// Line

		top = wl * FBUF(n,m) + wr * FBUF(n,m+1);
		bot = wl * FBUF(n+1,m) + wr * FBUF(n+1,m+1);
		out_line = bot * wb + top * (1.0-wb);

		// Samp

		top = wl * FBUF2(n,m) + wr * FBUF2(n,m+1);
		bot = wl * FBUF2(n+1,m) + wr * FBUF2(n+1,m+1);
		out_samp = bot * wb + top * (1.0-wb);
	    }
	    else {		// no interp
	        out_line = FBUF ((int)(image_line+0.5), (int)(image_samp+0.5));
	        out_samp = FBUF2((int)(image_line+0.5), (int)(image_samp+0.5));
	    }
	    
	    if (out_samp == 0.0 && out_line == 0.0)
		continue;		// no value here

	    // Convert pixel value, which is a coordinate in the Right image,
	    // back to unlinearized coordinates.

	    camera_in_right->LStoLookVector(out_line, out_samp, origin, look,
				       output_cs);
	    
	    hits = surface_model->intersectRay(origin, look, surf_pt);
	    infinity = (hits <= 0);
	    
	    camera_out_right->XYZtoLS(surf_pt, infinity, &in_line, &in_samp,
				  output_cs);
	    
	    // check if point is within the unlinearized image	
	    if (file_out_right->testPixelLocation(in_line,in_samp) != 0)
		continue;			// skip if not
	    
	    // Convert to phys image coords (compensate for sub-areas)
	    image_line = in_line - file_out_right->getYOffset();
	    image_samp = in_samp - file_out_right->getXOffset();


	    obuf_f[i] = image_line;
	    obuf_f2[i] = image_samp;

            // Update counter of valid pixel
            numPixValid++;
	}				// sample loop
	
        zvwrit(unit_out, obuf_f, "LINE", j+1, "BAND", 1, NULL);
        zvwrit(unit_out, obuf_f2, "LINE", j+1, "BAND", 2, NULL);

	
    }				// line loop
    
    //Free up all dynamically allocated memory for fbuf
    for (int counter = 0; counter < MAX_OPEN; counter++) {

        if (fbuf[counter] != NULL)
	    delete[] fbuf[counter];
        if (fbuf2[counter] != NULL)
	    delete[] fbuf2[counter];
    }
    


    // Write the output labels

    PigLabelModel *labelModel = m->createLabelModel(unit_out);

    // We want the input product ID in the order:
    // unlin_pair[0], unlin_pair[1], inp, lin_right

    PigFileModel *tmp_models[4];
    tmp_models[0] = file_out;
    tmp_models[1] = file_out_right;
    tmp_models[2] = file_models[0];
    tmp_models[3] = file_out_lin;
    labelModel->setUnlinearized(tmp_models, 4);
    labelModel->writeCM(camera_out, output_cs);

    zvplabel(unit_out, 0, 1);


    // Some more hack here...
    // Need to delete the forwarded labels regarding scaling and
    // overlap as these changes when unlinearizing. Then set them
    // with proper values
    int status1 = zldel(unit_out, "PROPERTY", "CORRELATION_PIXEL_COUNT",
                       "PROPERTY", "DERIVED_IMAGE_PARMS", NULL);

    int status2 = zldel(unit_out, "PROPERTY", "CORRELATION_AVERAGE_SCALE",
                       "PROPERTY", "DERIVED_IMAGE_PARMS", NULL);

    int status3 = zldel(unit_out, "PROPERTY", "CORRELATION_OVERLAP_PERCENTAGE",
                       "PROPERTY", "DERIVED_IMAGE_PARMS", NULL);

    int status4 = zldel(unit_out, "PROPERTY", "CORRELATION_PYRAMID_LEVEL",
                       "PROPERTY", "DERIVED_IMAGE_PARMS", NULL);

    if (status1 <= 0 || status2 <= 0 || status3 <= 0 || status4 <=0) 
       zvmessage("Error in deleting labels value for Property DERIVED_IMAGE_PARMS", "");

    // Now, update the label values that we just deleted
    labelModel->setDisparityExtra(numPixValid, 
                                  leftGSD/rightGSD, 
                                  (double)numPixValid / ((double)nlo*nso) * 100.,
                                  0);


    zvclose(unit_out, NULL);
}

// Compute what the Ground Sampling Distance (GSD), i.e., the pixel size when
// projected out on the 3D world.
// The approach is to project the center pixel (input xc, yc) out in 3D on a 
// plane perpendiculat to the look vector of that center pixel and located at
// 1m, and so the same thing for the neighboor pixel (we arbitrarily takes the
// pixel located a (xc+1, yc)) and look at the distance between the two 3D 
// points
double getGSD(PigCameraModel *cam, int xc, int yc, PigCoordSystem *cs) {

   PigPoint center;
   PigVector look;

   // Get 3D point out of center pixel
   cam->LStoLookVector((double)yc, (double)xc, center, look, cs);
   look.normalize();
   PigPoint p1 = center + look; 

   // Get 3D point out of neighboor pixel
   cam->LStoLookVector((double)yc, (double)(xc+1), center, look, cs);
   look.normalize();
   PigPoint p2 = center + look;

   return (p1-p2).magnitude(); 
}
