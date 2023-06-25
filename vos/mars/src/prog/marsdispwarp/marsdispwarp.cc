#include "vicmain_c"
#include "PigMission.h"
#include "PigFileModel.h"
#include "PigCameraModel.h"
#include "PigLabelModel.h"

#include "mars_support.h"
#include "SimpleImage.h"

#include "lbl_image_data.h"
#include "return_status.h"

#define MAX_INPUTS 2
#define MAX_NS 0
#define MAX_NL 0


// This inline function fills zeros for interpolation.  If the pixel is
// zero, we attempt to replace it, using a simple average of non-zero
// neighbors in one of several patterns.  If we can't find a pattern with
// all non-0 values, we give up entirely (and return 0).  It's a poor-man's
// way of doing single-pixel hole filling.
//
// The patterns are all symmetric so we don't introduce bias when interp'ing
// disparity or XYZ.  We first look at 4 neighbors in a + or X, then look
// for 2 neighbors vertically, horizontally, or the two diagonals.
//
// Note that it is legit here to compare floats for equality to 0.0 because
// the 0.0 is an explicitly-set flag value (no-data), not the result of a
// calculation.
//
//!!!! This should probably be replaced with something better....

inline float fill_zero_interp(float in_dn, SimpleImage<float>*img,
			     int b, int n, int m)
{
    float dn = in_dn;
    if (dn == 0.0) {
	// Check +
	float p1 = img->get(b,n-1,m);
	float p2 = img->get(b,n+1,m);
	float p3 = img->get(b,n,m-1);
	float p4 = img->get(b,n,m+1);
	if (p1 != 0.0 && p2 != 0.0 && p3 != 0.0 && p4 != 0.0)
	    dn = (p1+p2+p3+p4)/4.0;
	else {
	    // Check X
	    float p5 = img->get(b,n-1,m-1);
	    float p6 = img->get(b,n+1,m-1);
	    float p7 = img->get(b,n-1,m+1);
	    float p8 = img->get(b,n+1,m+1);
	    if (p5 != 0.0 && p6 != 0.0 && p7 != 0.0 && p8 != 0.0) {
		dn = (p5+p6+p7+p8)/4.0;
	    }
	    else {
		// At this point we have neither a full + nor X.  But we'd like
		// more coverage.  We have to stay unbiased though so we
		// look only for balanced pairs: I - / \ .
		// Initially I was taking pairs only if their difference was
		// under a threshold.  But if we are going to do that we must
		// make the same decision across all bands (at least when
		// interp'ing disparity or xyz) so that we at least get
		// consistent results across the bands.  Seemed not worth the
		// effort at the moment so I removed it.  (rgd 2018-02-15).

		if (p1 != 0.0 && p2 != 0.0)
		    dn = (p1+p2)/2.0;			// I
		else if (p3 != 0.0 && p4 != 0.0)
		    dn = (p3+p4)/2.0;			// -
		else if (p5 != 0.0 && p8 != 0.0)
		    dn = (p5+p8)/2.0;			// backslash
		else if (p6 != 0.0 && p7 != 0.0)
		    dn = (p6+p7)/2.0;			// slash
	    }
	}
    }
    return dn;
}




////////////////////////////////////////////////////////////////////////
// Program to warp the Right image into the Left image's space using the
// disparity map.  Requires 2-band disparity input.

void main44()
{
    int nids;
    char mission[64], instrument[64];
    PigFileModel *file_models[MAX_INPUTS+1];
    PigCameraModel *camera_in[MAX_INPUTS];
    PigPointingModel *pointing_in[MAX_INPUTS];
    int homogeneous_inputs = TRUE;
    PigCoordSystem *cs;

    int in_unit, out_unit, disp_unit;
    char disp_filename[256];
    int sl, ss, nl, ns, nld, nsd, nlo, nso, nli, nsi;
    int band, band_count;
    SimpleImage<float> *in_buf;
    int pyramid;
    float zoom, total_zoom, disp_zoom;
    int count;
	const size_t msgLen = 1024;
    char msg[msgLen];

    mars_setup(nids, file_models, camera_in, pointing_in, NULL, cs,
		mission, instrument, homogeneous_inputs,
		MAX_NL, MAX_NS, MAX_INPUTS);
    PigMission *m = PigMission::getMissionObject(mission);

    int interp = zvptst("INTERP");
    int disp_interp = zvptst("DISP_INTERP");
    int non_zero_interp = zvptst("NONZERO_INTERP");
    int disp_zoom_only = zvptst("DISP_ZOOM_ONLY");

    int do_pixel_count = zvptst("PIXEL_COUNT");

    zvp("DISP_PYRAMID", &pyramid, &count);
    zvp("DISP_ZOOM", &disp_zoom, &count);
    zvp("ZOOM", &zoom, &count);

    disp_zoom = disp_zoom * (1 << pyramid);
    total_zoom = disp_zoom * zoom;

    // Open and read in the input

    int maxNB = file_models[0]->getNB();

    zvp("BAND", &band, &count);
    band_count = 1;

    if (count == 0) {
	// no input band specified; process all bands

	band_count = maxNB;
	snprintf(msg, msgLen, "Number of bands to be processed is %d", band_count);
	zvmessage(msg, "");
	band = 0;
    } else {
	if (band > maxNB) {
	    snprintf(msg, msgLen,
	      "Input band %d > number of bands in input.  Band set to 1", band);
	    zvmessage(msg, "");
	    band = 1;
	}
    }

    if (disp_zoom_only) {	// output must be 2-band disparity
	band_count = 2;
	band = 0;
    }


    file_models[0]->closeFile();
    in_unit = file_models[0]->getUnit();

    zveaction("SA", "");
    zvopen(in_unit ,"OP", "READ", "U_FORMAT", "REAL", NULL);
    file_models[0]->setFileOpen(TRUE);

    nl = file_models[0]->getNL();
    ns = file_models[0]->getNS();

    in_buf = new SimpleImage<float>(maxNB, nl, ns);
    if (in_buf == NULL) {
	zvmessage("Error allocating input buffer", "");
	zabend();
    }

    for (int b=0; b < maxNB; b++) {
	int bb = b;
	if (band != 0)
	    bb = b+band-1;

	for (int i=0; i < nl; i++) {
	    zvread(in_unit, in_buf->linePtr(b,i), "line", i+1,
						  "band", bb+1, NULL);
	}
    }

    // Open disparity map and get size.  Note that the disparity size
    // could be different from the image size.  We put it at [nids]
    // because that's convenient for setting the input product ID's, later.

    zvpone("DISP", disp_filename, 1, 255);
    zvunit(&disp_unit, "DISP", 1, "U_NAME", disp_filename, NULL);
    zvopen(disp_unit, "OP", "READ", "U_FORMAT", "REAL", NULL);
    file_models[nids] = m->createFileModel(disp_filename, disp_unit);

    nld = file_models[nids]->getNL();
    nsd = file_models[nids]->getNS();

    // Simple trick:  we can simply use index 1 here.  If the second input
    // is given, we want to use it, at index 1.  If not, nids is 1 so the
    // disparity model is used instead.  The disp is backup for the image,
    // so it works out cleanly.

    nlo = file_models[1]->getNL() * zoom;
    nso = file_models[1]->getNS() * zoom;

    double cmod_zoom = zoom;

    // If the second input file is not given, and disp_zoom is not 1, then
    // we have to adjust nlo/nso and issue a warning that the camera model
    // will be wrong

    if (nids == 1 && disp_zoom != 1.0) {
	nlo = nlo * disp_zoom;
	nso = nso * disp_zoom;
	zvmessage("**WARNING** Second input not given and disp_pyramid is not 0.", "");
	zvmessage("            This means the camera model in the label will be incorrect.", "");
    }
    else if (nids != 1 && zvptst("REF_NOT_ZOOMED")) {
        // If the ref image is not zoomed, then we need to expand the output
        // size by the amount of the disparity zoom (and adjust the camera model
        // accordingly).  

	nlo *= disp_zoom;
	nso *= disp_zoom;
	cmod_zoom *= disp_zoom;
    }

    // Check for overrides on size

    int slo, sso, nlo_override, nso_override;
    zvp("SL", &slo, &count);
    zvp("SS", &sso, &count);

    zvp("NL", &nlo_override, &count);
    if (count > 0) {
	if (nlo_override <= 0)		// sanity checks
	    nlo_override = 1;
	if (nlo_override > nlo)
	    nlo_override = nlo;
	nlo = nlo_override;
    }

    zvp("NS", &nso_override, &count);
    if (count > 0) {
	if (nso_override <= 0)		// sanity checks
	    nso_override = 1;
	if (nso_override > nso)
	    nso_override = nso;
	nso = nso_override;
    }

    // Open the output file.  We use the image file as the primary input,
    // because the filters, eye, etc. are all correct.  But the size and
    // camera model are wrong.  So we get the size from the second input
    // (the "L" file), and transfer the camera model over too.  If the
    // second model is not given, we use the disparity map as a proxy (the
    // camera model will be wrong if there's a disp_zoom in that case).
    // In either case, the resultant label will not work properly with
    // kinematics (it's still labeled as an "R" image even though the
    // geometry is "L") but that's not possible to do while still retaining
    // the info that it came from an "R" image.

    zvselpiu(in_unit);
    zvunit(&out_unit, "OUT", 1, NULL);
    zvopen(out_unit, "OP", "WRITE", "U_FORMAT", "REAL",
			"U_NS", nso, "U_NL", nlo, "U_NB", band_count, NULL);
    zvplabel(out_unit, 0, 1);

    // Same trick as above, with index 1.

    PigLabelModel *labelModel = m->createLabelModel(out_unit);
    PigCameraModel *camera_model = PigCameraModel::create(file_models[1], NULL);

    if (camera_model == NULL) {
	zvmessage("NO CAMERA MODEL in output because no model in disparity file","");
    } else {
        if (cmod_zoom != 1.0) {
	    camera_model->scaleCamera(cmod_zoom, cmod_zoom);
        }
        labelModel->writeCM(camera_model, camera_model->getCoordSystem());
    }
    labelModel->writeProductIds(file_models, nids+1);

    SimpleImage<float> *line_disp_img = new SimpleImage<float>(nld, nsd);
    SimpleImage<float> *samp_disp_img = new SimpleImage<float>(nld, nsd);

    if (line_disp_img == NULL || samp_disp_img == NULL) {
	zvmessage("Error allocating disparity memory", "");
	zabend();
    }

    // Read disparity image

    for (int j=0; j < nld; j++) {
	zvread(disp_unit, line_disp_img->linePtr(j), "line", j+1,"band",1,NULL);
    }
    for (int j=0; j < nld; j++) {
	zvread(disp_unit, samp_disp_img->linePtr(j), "line", j+1,"band",2,NULL);
    }

    // Only need one line of output at a time...

    SimpleImage<float> *dn_buf = new SimpleImage<float>(band_count, 1, nso);
    if (dn_buf == NULL) {
	zvmessage("Unable to allocate output buffer!", "");
	zabend();
    }

    // The zoom coordinates might be more easily thought of as (j-0.5)/zoom+0.5
    // which would work if they were 1-based coordinates.  But with 0-based
    // coordinates, the .5 signs reverse.

    int pixel_count = 0;

    // i, j are actual output file coordinates.  ii and jj are logical
    // output file coordinates (disregarding the window).

    for (int j=0; j < nlo; j++) {
	int jj = j + slo - 1;
	double disp_j = (jj + 0.5) / total_zoom - 0.5;

	for (int i = 0; i < nso; i++) {
	    int ii = i + sso - 1;
	    double disp_i = (ii + 0.5) / total_zoom - 0.5; 

	    for (int b=0; b < band_count; b++) {
	        dn_buf->set(b, 0, i, 0.0);
	    }

	    double line_disp = line_disp_img->get((int)(disp_j+0.5), (int)(disp_i+0.5));
	    double samp_disp = samp_disp_img->get((int)(disp_j+0.5), (int)(disp_i+0.5));

	    // This check was disabled because with nonzero interpolation,
	    // it's safe to interpolate a hole even for the "primary" pixel
	    // because the interpolation is nonbiased.  rgd 2019-01-25
//////	    if (line_disp == 0.0 || samp_disp == 0.0)
//////		continue;			// no input disparity

	    // Now interpolate in the *disparity* image.  This allows us to
	    // have low-res disparities (e.g. navcam to mastcam) but still get
	    // a high-res output.  We use the same "non_zero_interp" as we
	    // (optionally) use for pixels.  Note that we lose 2 pixels along
	    // the edges this way.

	    // If the coordinates already integers, then we don't need to
	    // interpolate... as a matter of fact, doing so *reduces* the
	    // coverage because we need neighbors to interpolate, which then
	    // go unused.

	    if (disp_interp && (((disp_j - (int)disp_j) > 0.0001) ||
			        ((disp_i - (int)disp_i) > 0.0001))) {

	        if (disp_j < 1 || disp_j >= nld-2 ||
		    disp_i < 1 || disp_i >= nsd-2)

		    continue;

	        int m = (int)disp_i;
	        int n = (int)disp_j;
	        double wr = disp_i - m;
	        double wl = 1.0 - wr;
	        double wb = disp_j - n;

		// Line direction

		double ul = line_disp_img->get(n,m);
		double ur = line_disp_img->get(n,m+1);
		double ll = line_disp_img->get(n+1,m);
		double lr = line_disp_img->get(n+1,m+1);

		// If any of our interp pixels is 0, try to fill it in

		ul = fill_zero_interp(ul, line_disp_img, 0, n, m);
		if (ul == 0.0)
		    continue;		// no value, skip
		ur = fill_zero_interp(ur, line_disp_img, 0, n, m+1);
		if (ur == 0.0)
		    continue;		// no value, skip
		ll = fill_zero_interp(ll, line_disp_img, 0, n+1, m);
		if (ll == 0.0)
		    continue;		// no value, skip
		lr = fill_zero_interp(lr, line_disp_img, 0, n+1, m+1);
		if (lr == 0.0)
		    continue;		// no value, skip

	        double top = wl * ul + wr * ur;
	        double bot = wl * ll + wr * lr;
		// We only want to use it if BOTH interps succeed...
	        float tentative_line_disp = bot*wb + top*(1.0-wb);

		// Sample direction

		ul = samp_disp_img->get(n,m);
		ur = samp_disp_img->get(n,m+1);
		ll = samp_disp_img->get(n+1,m);
		lr = samp_disp_img->get(n+1,m+1);

		// If any of our interp pixels is 0, try to fill it in

		ul = fill_zero_interp(ul, samp_disp_img, 0, n, m);
		if (ul == 0.0)
		    continue;		// no value, skip
		ur = fill_zero_interp(ur, samp_disp_img, 0, n, m+1);
		if (ur == 0.0)
		    continue;		// no value, skip
		ll = fill_zero_interp(ll, samp_disp_img, 0, n+1, m);
		if (ll == 0.0)
		    continue;		// no value, skip
		lr = fill_zero_interp(lr, samp_disp_img, 0, n+1, m+1);
		if (lr == 0.0)
		    continue;		// no value, skip

	        top = wl * ul + wr * ur;
	        bot = wl * ll + wr * lr;
	        samp_disp = bot*wb + top*(1.0-wb);
		line_disp = tentative_line_disp;

	    }	// else no disp interp - already set above

	    // What we just read from the file are 1-based coordinates.
	    // So subtract 1 to get 0-based coordinates.

	    line_disp -= 1.0;
	    samp_disp -= 1.0;

	    // Scale disp back up by disparity zoom only (not extra zoom)
	    // because pyramid'd disparities relate to the small image.
	    // 0-based so the signs reverse (see above)

	    float line = (line_disp+0.5)*disp_zoom-0.5;
	    float samp = (samp_disp+0.5)*disp_zoom-0.5;

	    // For the disp_zoom_only case we actually have no idea what
	    // the right side image size is.  So we can't check for upper
	    // bounds.  We still check for lower bounds though.

	    if (line < 0 || samp < 0)		// sanity check
		continue;
	    if ((!disp_zoom_only) && (line >= (nl-1) || samp >= (ns-1)))
		continue;			// sanity check
	    if (non_zero_interp) {	// need one more pixel margin
	        if (line < 1 || samp < 1)	// sanity check
		    continue;
	        if ((!disp_zoom_only) && (line >= (nl-2) || samp >= (ns-2)))
		    continue;			// sanity check
	    }


	    if (disp_zoom_only) {

		// Special case: we're just zooming the disparity map
		// so the values we have now are what goes in the output.
		// Except... make them 1-based.

		dn_buf->set(0, 0, i, line+1);
		dn_buf->set(1, 0, i, samp+1);

		// we don't have to check for 0 because it was done above...

		pixel_count++;

	    } else if (interp) {

	        // Bilinear interpolation of *pixels*

	        int m = (int)samp;
	        int n = (int)line;
	        double wr = samp - m;
	        double wl = 1.0 - wr;
	        double wb = line - n;

		int num_set = 0;

	        for (int b = 0; b < band_count; b++) {
		    double ul = in_buf->get(b,n,m);
		    double ur = in_buf->get(b,n,m+1);
		    double ll = in_buf->get(b,n+1,m);
		    double lr = in_buf->get(b,n+1,m+1);

		    if (non_zero_interp) {

			// If any of our interp pixels is 0, try to fill it in

			ul = fill_zero_interp(ul, in_buf, b, n, m);
			if (ul == 0.0)
			    continue;		// no value, skip
			ur = fill_zero_interp(ur, in_buf, b, n, m+1);
			if (ur == 0.0)
			    continue;		// no value, skip
			ll = fill_zero_interp(ll, in_buf, b, n+1, m);
			if (ll == 0.0)
			    continue;		// no value, skip
			lr = fill_zero_interp(lr, in_buf, b, n+1, m+1);
			if (lr == 0.0)
			    continue;		// no value, skip
		    }

	            double top = wl * ul + wr * ur;
	            double bot = wl * ll + wr * lr;
		    double dn = bot*wb + top*(1.0-wb);

	            dn_buf->set(b, 0, i, dn);
		    if (dn != 0)
			num_set++;
		}
		if (num_set > 0) {
		    pixel_count++;
		}
	    } else {

		// No interp

		int m = (int)(samp+0.5);
		int n = (int)(line+0.5);
		int num_set = 0;
		for (int b = 0; b < band_count; b++) {
		    double dn = in_buf->get(b,n,m);
		    dn_buf->set(b, 0, i, dn);
		    if (dn != 0)
			num_set++;
		}
		if (num_set > 0) {
		    pixel_count++;
		}
	    }
	}

	for (int b=0; b < band_count; b++) {
	    zvwrit(out_unit, dn_buf->linePtr(b,0), "line", j+1, "band", b+1, NULL);
	}
    }

    if (do_pixel_count) {
	labelModel->setDisparityExtra(pixel_count, 0.0, -1.0, -1);
    }

    // Write the subframe coordinates.  Should this be in PIG?  Probably...

    LblImageData_typ ImageData;
    memset(&ImageData, 0, sizeof(LblImageData_typ));

    ImageData.FirstLine.Value = slo;
    ImageData.FirstLine.Valid = 1;
    ImageData.FirstLineSample.Value = sso;
    ImageData.FirstLineSample.Valid = 1;

    int status = LblImageData(out_unit, LBL_AUGMENT, &ImageData, 1);
    if (RTN_FAILURE(status)) {
	snprintf(msg, msgLen, "Label write failure: %s", (char *)LblErrorMessage());
	zvmessage(msg, "");
    }

    zvclose(in_unit, NULL);
    zvclose(disp_unit, NULL);
    zvclose(out_unit, NULL);

}

