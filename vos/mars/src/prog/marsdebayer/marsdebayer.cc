/* marsdebayer */
#include "vicmain_c"

#include "mars_support.h"
#include "SimpleImage.h"

#include "PigMission.h"
#include "PigFileModel.h"
#include "PigLabelModel.h"
#include "PigCameraModel.h"
#include "PigPointingModel.h"
#include "PigVector.h"
#include "PigCoordSystem.h"
#include <stdlib.h>

/* buffer sizes in main program */
#define MAX_INPUTS 1
// Value of 0 indicates there is no
// upper-bound for input image size.
#define MAX_NS 0
#define MAX_NL 0
// offset line/sample value for start of Bayer pattern 
#define OFFSET 1

typedef enum {
	NORMAL=0, HALF_SCALE, UPPER, LOWER, TALL, AVERAGE, NONE
} ZoomMode;

typedef enum {
	REPLICATE, BILINEAR, MALVAR
} InterpMode;

static char *mode_strings_repl[] = {
	"MIPL_REPLICATED",
	"MIPL_AVERAGED_COLOR",
	"MIPL_UPPER",
	"MIPL_LOWER",
	"MIPL_TALL_REPLICATED",
	"MIPL_AVERAGED_MONO",
	NULL,
};

static char *mode_strings_interp[] = {
	"MIPL_INTERPOLATED",
	"MIPL_AVERAGED_COLOR",
	"MIPL_UPPER",
	"MIPL_LOWER",
	"MIPL_TALL_INTERPOLATED",
	"MIPL_AVERAGED_MONO",
	NULL,
        "MIPL_MALVAR_INTERPOLATED"
};

static char *mode_strings_repl_new[] = {
	"OTHER",
	"AVERAGED",
	"OTHER",
	"OTHER",
	"OTHER",
	"PANCHROMATIC",
	"IDENTITY",
};

static char *mode_strings_interp_new[] = {
	"BILINEAR",
	"AVERAGED",
	"OTHER",
	"OTHER",
	"OTHER",
	"PANCHROMATIC",
	"IDENTITY",
        "MALVAR"
};

static char *average_string[] = {
	"RED_AVERAGED",
	"GREEN_AVERAGED",
	"BLUE_AVERAGED"
};

static char *bilinear_string[] = {
	"RED_BILINEAR",
	"GREEN_BILINEAR",
	"BLUE_BILINEAR"
};

void rotate90(SimpleImage<double> *in_image, SimpleImage<double> *rot);
void rotate180(SimpleImage<double> *in_image, SimpleImage<double> *rot);
void rotate270(SimpleImage<double> *in_image, SimpleImage<double> *rot);
void bayer_offset(SimpleImage<double> *in_image, SimpleImage<double> *offset); 
void multiplicative_factor(SimpleImage<double> *in_image,
                           double r_mult, double g_mult, double b_mult);
void fill_holes_rep(SimpleImage<double> *db, SimpleImage<double> *out);
void fill_holes_interp(SimpleImage<double> *db,
						SimpleImage<double> *out);
double apply_kern(double kern[5][5], double region[5][5]);
double green_at_red_interp(double region[5][5]);
double green_at_blue_interp(double region[5][5]);
double red_at_green_Rrow_Bcol_interp(double region[5][5]);
double red_at_green_Brow_Rcol_interp(double region[5][5]);
double red_at_blue_Brow_Bcol_interp(double region[5][5]);
double blue_at_green_Brow_Rcol_interp(double region[5][5]);
double blue_at_green_Rrow_Bcol_interp(double region[5][5]);
double blue_at_red_Rrow_Rcol_interp(double region[5][5]);
void fill_holes_malvar_interp(SimpleImage<double> *db, 
                              SimpleImage<double> *out1,
                              SimpleImage<double> *out2,
                              SimpleImage<double> *out3,
                              double min_dn, double max_dn);
void tall_rep(SimpleImage<double> *db, SimpleImage<double> *out);
void tall_interp(SimpleImage<double> *db, SimpleImage<double> *out);
void average_down(SimpleImage<double> *db, SimpleImage<double> *out);

////////////////////////////////////////////////////////////////////////

void main44()
{
    int i, /*j,*/ band, line, samp;
    int /*status,*/ count, def;
    // char msg[256];

    int nids;
    char mission[64], instrument[64];

    // Inputs

    PigFileModel *file_models[MAX_INPUTS];
    PigCameraModel *camera_in[MAX_INPUTS];
    PigPointingModel *pointing_in[MAX_INPUTS];
    int homogeneous_inputs = TRUE;
    PigCoordSystem *cs;
    int in_unit;

    // Outputs
    int out_unit[3];
    int out_band[3];

    // User Parameters
    ZoomMode mode;
    InterpMode interp;
    int only_band;

    double min_dn, max_dn;
    double rgb_mult[3] = {0};     // multipicative factors for RGB bands
    int total_bands;			// for the output
    int zoom_l, zoom_s;			// 1 = full-size, 2 == half-size
   
    // Active size of input subframe (direct from params)
    int sl, ss, nl, ns;
    // Active size of output subframe (after zoom)
    int slo, sso, nlo, nso;	
    // Actual size of input, without subframing but adjusted to be even
    int nlin, nsin;
    // Theoretical size of output, if there were no subframing
    int nlout, nsout;
    int nlout_rot, nsout_rot;
    // Bayer offset (direct from params)
    //int l_offset, s_offset;
    //l_offset = s_offset = 0;

    SimpleImage<double> in_image;	// input image nlin x nsin bayer image
    SimpleImage<double> rot;            // rotated image (both input and output)
    SimpleImage<double> offset_image;     // bayer offset image (line, samp, or both)
    SimpleImage<double>* imgPtr;        // pointer to image to be processed 
    SimpleImage<double> db_image[3];	// debayer image nlin x nsin
    SimpleImage<double> out_image[3];	// output image nlout x nsout
						// Subframing done during write
    zvmessage("MARSDEBAYER version 2019-10-02", "");

    // Get the input file list, and set up initial camera/pointing models
    // for each input.  Although we accept one input only, mars_setup
    // does lots of other nice things for us.

    mars_setup(nids, file_models, camera_in, pointing_in, NULL, cs,
                mission, instrument, homogeneous_inputs,
                MAX_NL, MAX_NS, MAX_INPUTS);

    PigMission *m = PigMission::getMissionObject(mission);

    // Get parameters

    zvp("SL", &sl, &count);
    sl--;			// make 0-based
    zvp("SS", &ss, &count);
    ss--;
    zvp("NL", &nl, &count);
    zvp("NS", &ns, &count);
    zvparmd("MIN_DN", &min_dn, &count, &def,1,0);
    zvparmd("MAX_DN", &max_dn, &count, &def,1,0);
    
    // make sure we have complete Bayer cells

    if (sl % 2 != 0) {
	zvmessage("SL (1-based) must be odd", "");
	zabend();
    }
    if (ss % 2 != 0) {
	zvmessage("SS (1-based) must be odd", "");
	zabend();
    }
    if (nl % 2 != 0) {
	zvmessage("NL must be even", "");
	zabend();
    }
    if (ns % 2 != 0) {
	zvmessage("NS must be even", "");
	zabend();
    }

    mode = NORMAL;
    if (zvptst("HALF_SCALE")) mode = HALF_SCALE;
    if (zvptst("UPPER")) mode = UPPER;
    if (zvptst("LOWER")) mode = LOWER;
    if (zvptst("TALL")) mode = TALL;
    if (zvptst("AVERAGE")) mode = AVERAGE;
    if (zvptst("NONE")) mode = NONE;

    interp = BILINEAR;
    if (zvptst("REPLICATE")) interp = REPLICATE;
    if (zvptst("MALVAR")){
        interp = MALVAR;
        //  ignore zoom mode for MALVAR interp
        if (mode != NORMAL){
            zvmessage("MALVAR interp used, ignoring ZOOM_MODE parameter","");
        }
        mode = NORMAL;
    }

    only_band = -1;
    if (zvptst("RED")) only_band=0;
    if (zvptst("GREEN")) only_band=1;
    if (zvptst("BLUE")) only_band=2;
    if ((mode==NONE || mode==AVERAGE) && only_band != -1) {
	zvmessage("ONLY_BAND can not be used in NONE or AVERAGE modes", "");
	zabend();
    }

    // Open input file.
    // Make sure file is open with U_FORMAT of DOUB to match our buffer.

    // get Unit id
    in_unit = file_models[0]->getUnit();

    if (file_models[0]->isFileOpen())
	file_models[0]->closeFile();
    /*status =*/ zvopen(in_unit, "op", "read", "open_act", "sa",
		"io_act", "sa", "u_format", "doub", NULL);
    file_models[0]->setFileOpen(TRUE);

    if (file_models[0]->getNB() != 1) {
	  zvmessage("Input file must have one band", "");
	  zabend();
    }

    nlin = file_models[0]->getNL();
    nsin = file_models[0]->getNS();

    if (nlin % 2 != 0) {
	if (nl == 0)		// only warn if not truncating
	    zvmessage("Warning: Input NL is not even; last line truncated","");
	nlin--;
    }
    if (nsin % 2 != 0) {
	if (ns == 0)		// only warn if not truncating
	   zvmessage("Warning: Input NS is not even; last column truncated","");
	nsin--;
    }

    // Resolve nl/ns (subframe size) and nlout/nsout (full output image size)
    if (nl == 0)
	nl = nlin - sl;
    if (ns == 0)
	ns = nsin - ss;

    // Sanity checks

    if ((sl < 0) || (sl+nl > nlin) || (ss < 0) || (ss+ns > nsin)) {
	zvmessage("Subframe exceeds bounds of inupt image","");
	zabend();
    }

    // Figure out the output size based on the zoom.

    zoom_l = 2;			// half size
    zoom_s = 2;
    if (mode == NORMAL || mode == NONE) {
	zoom_l = 1;		// full size
	zoom_s = 1;
    }
    if (mode == TALL) {
	zoom_l = 1;
	zoom_s = 2;
    }

    nlout = nlin / zoom_l;
    nlout_rot = nlout;
    nsout = nsin / zoom_s;
    nsout_rot = nsout;

    // Adjust subframe params to measure output
    slo = sl / zoom_l;
    sso = ss / zoom_s;
    nlo = nl / zoom_l;
    nso = ns / zoom_s;

    // Open output files.
    // OUT can be 1 or 3 files, for a single, 3-banded file, or 3 single-band
    // files.  If the mode is NONE or AVERAGE, only a single 1-band file is
    // allowed.

    // Note that the label routine updates and writes the camera model,
    // and also updates the downsample/subframe labels based on the zoom
    // or subframe.  We have both old-style and new-style mode strings,
    // unfortunately.

    char *mode_string = mode_strings_interp[mode];
    char *mode_string_new = mode_strings_interp_new[mode];
    if (interp == REPLICATE) {
	mode_string = mode_strings_repl[mode];
	mode_string_new = mode_strings_repl_new[mode];
    }

    if (interp == MALVAR) {
        mode_string = mode_strings_interp[7];  // MIPL_MALVAR_INTERPOLATED
        mode_string_new = mode_strings_interp_new[7];  // MALVAR
    }

    if (only_band != -1) {
	if (strcmp(mode_string_new, "AVERAGED") == 0) {
	    mode_string_new = average_string[only_band];
	}
	if (strcmp(mode_string_new, "BILINEAR") == 0) {
	    mode_string_new = bilinear_string[only_band];
	}
    }

    total_bands = 3;
    if (mode == NONE || mode == AVERAGE)
	total_bands = 1;
    if (only_band != -1)
	total_bands = 1;

    zvpcnt("OUT", &count);
    if (count == 1) {
	zvunit(&out_unit[0], "OUT", 1, NULL);
	zvopen(out_unit[0], "op", "write",
	       "u_ns", nso, "u_nl", nlo, "u_nb", total_bands,
	       "open_act", "sa", "u_org", "bsq",
	       "u_format", "doub", NULL);
	zvplabel(out_unit[0], 0, 1);

	out_unit[2] = out_unit[1] = out_unit[0];
	out_band[0] = 1;
	out_band[1] = 2;
	out_band[2] = 3;
	if (mode == NONE || mode == AVERAGE) {
	    out_band[1] = out_band[2] = 1;
	}

	// write output label
	PigLabelModel *labelModel = m->createLabelModel(out_unit[0]);
	labelModel->setBayer(file_models, nids, mode_string, mode_string_new,
			camera_in[0], zoom_l, zoom_s, sl, ss);
    }
    else if (count == 3) {
	if (mode == NONE || mode == AVERAGE) {
	    zvmessage("Zoom_modes of NONE or AVERAGE require only one output file","");
	    zabend();
	}

	for (i=0; i<3; i++) {
	    zvunit(&out_unit[i], "OUT", i+1, NULL);
	    zvopen(out_unit[i], "op", "write",
	       "u_ns", nso, "u_nl", nlo, "u_nb", 1,
	       "open_act", "sa", "u_org", "bsq",
	       "u_format", "doub", NULL);
	    zvplabel(out_unit[i], 0, 1);
	    out_band[i] = 1;

	    // write output label
	    PigMission *m = PigMission::getMissionObject(mission);
	    PigLabelModel *labelModel = m->createLabelModel(out_unit[i]);
	    labelModel->setBayer(file_models, nids, mode_string,mode_string_new,
			camera_in[0], zoom_l, zoom_s, sl, ss);
	}
    }
    else {
	zvmessage("OUT must have 1 or 3 filenames", "");
	zabend();
    }

    // Allocate memory for input image.  The entire image is
    // stored in memory for the subroutine.
    in_image.alloc(nlin, nsin);

////////////////////////////////////////////////////////////////////////
// Processing is done in two stages:
// 1) in_image -> db_image: De-bayer to full size, leaving holes as -1
// 2) db_image -> out_image: Interpolate and zoom to final output size
// Subframing is done when the file is written out.
//
// All processing is in memory
////////////////////////////////////////////////////////////////////////

    // Read the entire input image (including areas outside the subframe).
    // We do this rather than process a line at a time in order to facilitate
    // interpolation.

    for (line = 0; line < nlin; line++) {
	zvread(in_unit, in_image.linePtr(line), "BAND",1, "LINE", line+1,
			"NSAMPS", nsin, NULL);
    }

    imgPtr = &in_image;   // set imgPtr to input image

    // check if need to rotate image
    if (zvptst("ROT90")){
        // rotate image 90 degrees CW
        rotate90(&in_image,&rot);
        imgPtr = &rot;    // set imgPtr to rotated input image
        nlout_rot = nsout;		// swap these
        nsout_rot = nlout;
    }

    else if (zvptst("ROT180")){
        // rotate image 180 degrees
        rotate180(&in_image,&rot);
        imgPtr = &rot;    // set imgPtr to rotated input image
    }

    else if (zvptst("ROT270")){
        // rotate image 270 CW (90 degress CCW)
        rotate270(&in_image,&rot);
        imgPtr = &rot;
        nlout_rot = nsout;		// swap these
        nsout_rot = nlout;
    }

    else{} // rotation keyword not set or equals rot0 (no rotation)
    
    // check for bayer line/sample start offset
    // should be R G for the upper left corner of image
    //           G B
    if (zvptst("LINE_OFFSET")||zvptst("SAMPLE_OFFSET")||zvptst("BOTH_OFFSET")){
        bayer_offset(imgPtr,&offset_image);
        imgPtr = &offset_image;
    }

    // if we want to apply multiplicative factor to raw image
    zvparmd("MULT", rgb_mult, &count, &def,3,0);
    if (count == 0);    // do nothing
    else if (count == 3){
        multiplicative_factor(imgPtr,rgb_mult[0],rgb_mult[1],rgb_mult[2]);
    } 
    else {
       zvmessage("MULT must have 3 values", "");
       zabend(); 
    }

    // after possible rotations, offsets, multiplicative factors,
    // get number of lines and samples of input image (imgPtr) 
    // to allocate memory for debayering
    int db_NL = imgPtr->getNL();
    int db_NS = imgPtr->getNS();

    for (i=0;i<3;i++){
        db_image[i].alloc(db_NL,db_NS);
    }

    // allocate memory for output image.  Because we might be rotated, we
    // can't use nlout/nsout (which relate only to pre-rotation sizes)
    // but instead have to use the potentially swapped versions nlout_rot
    // nsout_rot.  Sub-framing and unrotation of out_image is handled after
    // interpolation
    for (i=0;i<3;i++)
        out_image[i].alloc(nlout_rot,nsout_rot);

    // Clear the out images ... only needed for error check

    for (int i=0; i<nlout_rot; i++) {
        for (int j=0; j<nsout_rot; j++)
            for (int k=0; k<3; k++)
                out_image[k].set(i,j,-1);
    }

    // Clear the DB images to -1 to indicate no value

    for (int i=0; i<db_NL; i++) {
        for (int j=0; j<db_NS; j++)
            for (int k=0; k<3; k++)
                db_image[k].set(i,j,-1);
    }


    // Go through the image cell by cell and de-Bayer it.  This just
    // pulls the appropriate data (de-Bayers) without any interpolation.
    // If we want only the upper or lower green cells, the others are
    // excluded here.

    for (line = 0; line < db_NL; line += 2) {
	for (samp = 0; samp < db_NS; samp += 2) {
                // no de-bayering
	    if (mode == NONE || mode == AVERAGE || 
                                        (mode == NORMAL && interp == MALVAR)){
		db_image[0].set(line,samp, imgPtr->get(line,samp));
		db_image[0].set(line+1,samp, imgPtr->get(line+1,samp));
		db_image[0].set(line,samp+1, imgPtr->get(line,samp+1));
		db_image[0].set(line+1,samp+1, imgPtr->get(line+1,samp+1));
	    }
                // de-bayer
	    else {
		db_image[0].set(line,samp, imgPtr->get(line,samp));	// red
		if (mode != UPPER)		// upper only excludes lower
		    db_image[1].set(line+1,samp, imgPtr->get(line+1,samp));//gL
		if (mode != LOWER)		// lower only excludes upper
		    db_image[1].set(line,samp+1, imgPtr->get(line,samp+1));//gH
		db_image[2].set(line+1,samp+1, imgPtr->get(line+1,samp+1)); //b
	    }
	}
    }

    // free up memory no longer needed
    in_image.free();
    rot.free();
    offset_image.free();

    // Now, transfer db_image to out_image using the appropriate mode
    switch (mode) {
	case NORMAL:
	    if (interp == REPLICATE) {
		fill_holes_rep(&db_image[0], &out_image[0]);
		fill_holes_rep(&db_image[1], &out_image[1]);
		fill_holes_rep(&db_image[2], &out_image[2]);
	    }
            else if (interp == MALVAR) {
                zvmessage("MALVAR INTERPOLATION","");
                fill_holes_malvar_interp(&db_image[0], &out_image[0],
                                         &out_image[1], &out_image[2],
                                         min_dn,max_dn);
            }
	    else {
		fill_holes_interp(&db_image[0], &out_image[0]);
		fill_holes_interp(&db_image[1], &out_image[1]);
		fill_holes_interp(&db_image[2], &out_image[2]);
	    }
	    break;

	// These cases are the same because the differences were handled
	// above already

	case HALF_SCALE:
	case UPPER:
	case LOWER:
	    average_down(&db_image[0], &out_image[0]);
	    average_down(&db_image[1], &out_image[1]);
	    average_down(&db_image[2], &out_image[2]);
	    break;

	case TALL:
	    if (interp == REPLICATE) {
		tall_rep(&db_image[0], &out_image[0]);
		tall_rep(&db_image[2], &out_image[2]);
	    }
	    else {
		tall_interp(&db_image[0], &out_image[0]);
		tall_interp(&db_image[2], &out_image[2]);
	    }
	    // G is the same in both cases
	    tall_rep(&db_image[1], &out_image[1]);
	    break;

	case AVERAGE:
	    average_down(&db_image[0], &out_image[0]);
	    break;

	case NONE:

	    // There are actually no holes to fill so this just copies
	    fill_holes_rep(&db_image[0], &out_image[0]);
	    break;
    }


    // Write out the images.  Subframing is handled here.
    int write_band = 0;
    for (band = 0; band < 3; band++) {
        imgPtr = &out_image[band];
        if ((mode == NONE || mode == AVERAGE) && band != 0)
            continue;           // only one band for NONE, AVERAGE

	if (only_band != -1) {
	    if (only_band != band)
		continue;			// skip all but given band
	}

        // check if we need to unrotate the images
        if (zvptst("ROT90")){
            // rotate image 270 degrees CW
            rotate270(&out_image[band],&rot);
            imgPtr = &rot;
        }

        else if (zvptst("ROT180")){
            // rotate image 180 degrees CW
            rotate180(&out_image[band],&rot);
            imgPtr = &rot;
        }

        else if (zvptst("ROT270")){
            // rotate image 90 degrees CW
            rotate90(&out_image[band],&rot);
            imgPtr = &rot;
        }

        else{} // rotation keyword not set or equals rot0
               // do nothing, no un-rotation necessary

        // write to output file
	for (line = 0; line < nlo; line++) {
	    zvwrit(out_unit[band], imgPtr->linePtr(line+slo)+sso,
		"LINE", (line+1), "BAND", out_band[write_band],
		"NSAMPS", nso, NULL);
	}

        // free memory no longer needed
        out_image[band].free();
        rot.free();
	write_band++;
    }

    zvclose(out_unit[0], NULL);
    if (out_unit[1] != out_unit[0])
	zvclose(out_unit[1], NULL);
    if (out_unit[2] != out_unit[0])
	zvclose(out_unit[2], NULL);

    // clean-up all memory we allocated
    rot.free();
    for (i=0;i<3;i++)
        out_image[i].free();
}

// rotate by 90 degrees.  Image is just a simple 2D matrix (MxN).
void rotate90(SimpleImage<double> *in_image, SimpleImage<double> *rot)
{
    int M = in_image->getNL();
    int N = in_image->getNS();

    // swap NL and NS for rotation
    int rotLines = N;  // rotLines = num samps of in_image
    int rotSamps = M;  // rotSamps = num lines of in_image
    rot->alloc(rotLines,rotSamps);

    for (int line = 0; line < M; line++){
        for (int samp = 0; samp < N; samp++){
            rot->set(samp,M-line-1,in_image->get(line,samp));
        }
    }
}

// rotate by 180 degrees.  Image is just a simple 2D matrix (MxN).
void rotate180(SimpleImage<double> *in_image, SimpleImage<double> *rot)
{
    int M = in_image->getNL();
    int N = in_image->getNS();
    int rotLines = M;  // no swap of lines and samps necessary for 180
    int rotSamps = N;  // degree rotation
    rot->alloc(rotLines,rotSamps);

    for (int line = 0; line < M; line++){
        for (int samp = 0; samp < N; samp++){
            rot->set(M-line-1,N-samp-1,in_image->get(line,samp));
        }
    }
}

// rotate by 270 degrees.  Image is just a simple 2D matrix (MxN).
void rotate270(SimpleImage<double> *in_image, SimpleImage<double> *rot)
{
    int M = in_image->getNL();
    int N = in_image->getNS();

    // swap NL and NS for rotation
    int rotLines = N;
    int rotSamps = M;
    rot->alloc(rotLines,rotSamps);

    for (int line = 0; line < M; line++){
        for (int samp = 0; samp < N; samp++){
            rot->set(N-samp-1,line,in_image->get(line,samp));
        }
    }
}

// Shift bayer pattern in line, sample, or both
void bayer_offset(SimpleImage<double> *in_image, SimpleImage<double> *offset_image)   
{
    int line, samp;
    int M = in_image->getNL();
    int N = in_image->getNS();

    offset_image->alloc(M,N);

    // line offset only
    if (zvptst("LINE_OFFSET")){
        for (line = 0; line < M-1; line++){
            for (samp = 0; samp < N; samp++ ){
                offset_image->set(line,samp,in_image->get(line+OFFSET,samp));
            }
        }
        // set last line to 0 DN value
        for (samp = 0; samp < N; samp++)
            offset_image->set(M-1,samp,0);
    }
    // sample offset only
    else if (zvptst("SAMPLE_OFFSET")){
        for (line = 0; line < M; line++){
            for (samp = 0; samp < N-1; samp++ ){
                offset_image->set(line,samp,in_image->get(line,samp+OFFSET));
            }
        }
        // set last sample to 0 DN value
        for (line = 0; line < M; line++)
            offset_image->set(line,N-1,0);
    }
    // both line and sample offset
    else{
        for (line = 0; line < M-1; line++){
            for (samp = 0; samp < N-1; samp++ ){
                offset_image->set(line,samp,in_image->get(line+OFFSET,samp+OFFSET));
            }
        }
        // set last line and sample to 0 DN value
        for (samp = 0; samp < N; samp++)
            offset_image->set(M-1,samp,0);
        for (line = 0; line < M; line++)
            offset_image->set(line,N-1,0);
    }
}
// multiply RGB pixels by corresponding factor.
void multiplicative_factor(SimpleImage<double> *in_image, double r_mult,
                           double g_mult, double b_mult)
{
    int line, samp;
    //int l_offset = 0;
    //int s_offset = 0;
    double pix, pix_mult=0.0;
    
    for (line = 0; line < in_image->getNL(); line++){
        for (samp = 0; samp < in_image->getNS(); samp++){   
            pix = in_image->get(line,samp);
            // red pixel
            if (line % 2 == 0 && samp % 2 == 0){
                pix_mult = pix * r_mult;
            }
            // green pixel
            if (line % 2 == 0 && samp % 2 == 1){
                pix_mult = pix * g_mult;
            }
            // green pixel
            if (line % 2 == 1 && samp % 2 == 0){
                pix_mult = pix * g_mult;
            }
            // blue pixel
            if (line % 2 == 1 && samp % 2 == 1){
                pix_mult = pix * b_mult;
            }

            in_image->set(line,samp,pix_mult);
        } // end samp for loop
    }  // end line for loop
}

////////////////////////////////////////////////////////////////////////
// Fill holes by replication.  If there are any holes, first look on the
// same line within the Bayer cell (which covers the green case).  Then if
// we still need to, look on the other line.  Images must be the same size.
////////////////////////////////////////////////////////////////////////

void fill_holes_rep(SimpleImage<double> *db, SimpleImage<double> *out)
{
    int line, samp;
    int error_count = 0;

    for (line = 0; line < out->getNL(); line++) {
	for (samp = 0; samp < out->getNS(); samp++) {
	    if (db->get(line,samp) != -1) {
		out->set(line,samp, db->get(line,samp));	// good value
	    } else {

		// find the right cell to replicate

		int cell_l = (line / 2) * 2;	// find cell boundary
		int cell_s = (samp / 2) * 2;	// find cell boundary
		int other_line = cell_l;
		if (cell_l == line) other_line = cell_l+1;
		// Look on the same line
		if (db->get(line,cell_s) != -1)
		    out->set(line,samp, db->get(line,cell_s));
		else if (db->get(line, cell_s+1) != -1)
		    out->set(line,samp, db->get(line,cell_s+1));
		// Look on the other line
		else if (db->get(other_line, cell_s) != -1)
		    out->set(line,samp, db->get(other_line, cell_s));
		else if (db->get(other_line, cell_s+1) != -1)
		    out->set(line,samp, db->get(other_line, cell_s+1));
		if (out->get(line,samp) == -1) {	// whoops!
		    out->set(line,samp,0);
		    error_count++;
		}
	    }
	}
    }

    if (error_count != 0) {
	char msg[1024];
	snprintf(msg, 1024, "Warning: %d pixels did not have a proper value", error_count);
	zvmessage(msg, "");
    }
}

////////////////////////////////////////////////////////////////////////
// Fill holes by interpolation.  If the pixel has a value, simply copy it.
// Otherwise, interpolate based on the 8 neighboring pixels.  This is easy
// because the patterns are always symmetric - we do not have to compute
// weights, we can simply add up all the surrounding valid pixels and divide
// by the number of valid pixels.  So while it actually IS "bilinear
// interpolation", it is implemented much more simply.
//
// Images must be the same size.
////////////////////////////////////////////////////////////////////////

void fill_holes_interp(SimpleImage<double> *db,
					SimpleImage<double> *out)
{
    int line, samp, i, j;
    int error_count = 0;

    for (line = 0; line < out->getNL(); line++) {
	for (samp = 0; samp < out->getNS(); samp++) {
	    if (db->get(line,samp) != -1) {
		out->set(line,samp, db->get(line,samp));	// good value
	    } else {

		// interpolate.  We don't have to skip the center pixel
		// explicitly, it'll just happen because it's -1.

		double sum = 0.0;
		int npix = 0;

		for (i = -1; i <= 1; i++) {
		   int ll = line+i;
		   if (ll < 0) continue;	// skip if off edge
		   if (ll >= out->getNL()) continue;

		   for (j = -1; j <= 1; j++) {
			int ss = samp+j;
			if (ss < 0) continue;
			if (ss >= out->getNS()) continue;

			if (db->get(ll,ss) != -1) {
			    sum += db->get(ll,ss);
			    npix++;
			}
		    }
		}

		if (npix == 0) {
		    error_count++;
		    out->set(line,samp, 0);
		}
		else {
		    out->set(line,samp, sum / npix);
		}
	    }
	}
    }

    if (error_count != 0) {
	char msg[1024];
	snprintf(msg, 1024, "Warning: %d pixels did not have a proper value", error_count);
	zvmessage(msg, "");
    }
}

// apply kernel to 5x5 region of input image
double apply_kern(double kern[5][5], double region[5][5])
{
    int i,j;
    double sum = 0.0;

    for (i=0;i<5;i++)
        for (j=0;j<5;j++)
            sum += kern[i][j] * region[i][j];

    return sum;
}

// Kernel for G at R locations (even row, even col)
double green_at_red_interp(double region[5][5])
{
    double green;
    double kern[5][5] = {
        {  0,     0,    -0.125, 0,     0     },
        {  0,     0,     0.25,  0,     0     },
        { -0.125, 0.25,  0.5,   0.25, -0.125 },
        {  0,     0,     0.25,  0,     0     },
        {  0,     0,    -0.125, 0,     0     }
        };

    green = apply_kern(kern,region);
    return green;
}

// Kernel for G at B locations (odd row, odd col)
double green_at_blue_interp(double region[5][5])
{
    double green;
    double kern[5][5] = {
        {  0,     0,    -0.125, 0,     0     },
        {  0,     0,     0.25,  0,     0     },
        { -0.125, 0.25,  0.5,   0.25, -0.125 },
        {  0,     0,     0.25,  0,     0     },
        {  0,     0,    -0.125, 0,     0     }
        };

    green = apply_kern(kern,region);
    return green;
}

// Kernel for Red at Green in Red row, Blue column (even row, odd col)
double red_at_green_Rrow_Bcol_interp(double region[5][5])
{
    double red;
    double kern[5][5] = {
        {  0,      0,     0.0625,  0,      0     },
        {  0,     -0.125, 0     , -0.125,  0     },
        { -0.125,  0.5,   0.625,   0.5,   -0.125 },
        {  0,     -0.125, 0,      -0.125,  0     },
        {  0,      0,     0.0625,  0,      0     }
        };

    red = apply_kern(kern,region);
    return red;
}

    // Kernel for Red at Green in Blue row, Red column (odd row, even col)
double red_at_green_Brow_Rcol_interp(double region[5][5])
{
    double red;
    double kern[5][5] = {
        { 0,      0,     -0.125,  0,     0      },
        { 0,     -0.125,  0.5,   -0.125, 0      },
        { 0.0625, 0,      0.625,  0,     0.0625 },
        { 0,     -0.125,  0.5,   -0.125, 0      },
        { 0,      0,     -0.125,  0,     0      }
        };

    red = apply_kern(kern,region);
    return red;
}

    // Kernel for R at blue in B row, B column (odd row, odd col)
double red_at_blue_Brow_Bcol_interp(double region[5][5])
{
    double red;
    double kern[5][5] = {
        {  0,      0,    -0.1875, 0,     0      },
        {  0,      0.25,  0,      0.25,  0      },
        { -0.1875, 0,     0.75,   0,    -0.1875 },
        {  0,      0.25,  0,      0.25,  0      },
        {  0,      0,    -0.1875, 0,     0      }
        };

    red = apply_kern(kern,region);
    return red;
}

// Kernel for B at green in B row, R column (odd row, even col)
double blue_at_green_Brow_Rcol_interp(double region[5][5])
{
    double blue;
    double kern[5][5] = {
        {  0,      0,     0.0625,  0,      0     },
        {  0,     -0.125, 0,      -0.125,  0     },
        { -0.125,  0.5,   0.625,   0.5,   -0.125 },
        {  0,     -0.125, 0,      -0.125,  0     },
        {  0,      0,     0.0625,  0,      0     }
        };

    blue = apply_kern(kern,region);
    return blue;
}

// Kernel for B at green in R row, B column (even row, odd col)
double blue_at_green_Rrow_Bcol_interp(double region[5][5])
{
    double blue;
    double kern[5][5] = {
        { 0,      0,     -0.125,  0,     0      },
        { 0,     -0.125,  0.5,   -0.125, 0      },
        { 0.0625, 0,      0.625,  0,     0.0625 },
        { 0,     -0.125,  0.5,   -0.125, 0      },
        { 0,      0,     -0.125,  0,     0      }
        };

    blue = apply_kern(kern,region);
    return blue;
}

// Kernel for B at red in R row, R column (even row, even col)
double blue_at_red_Rrow_Rcol_interp(double region[5][5])
{
    double blue;
    double kern[5][5] = {
        {  0,      0,    -0.1875, 0,     0      },
        {  0,      0.25,  0,      0.25,  0      },
        { -0.1875, 0,     0.75,   0,    -0.1875 },
        {  0,      0.25,  0,      0.25,  0      },
        {  0,      0,    -0.1875, 0,     0      }
        };

    blue = apply_kern(kern,region);
    return blue;
}

////////////////////////////////////////////////////////////////////////
// Fill holes by Malvar interp.  If the pixel has a value, simply copy it.
// Otherwise, interpolate based on a designated kernel for a color (RGB)
// at a given color row and column.  The kernels used came from a paper
// written by Malvar, He, and Cutler at Microsoft Reasearch.  The paper
// is titled "High-Quality Linear Interpolation for Demosaicing of Bayer-
// Patterned Color Images".  This is the algorithm used on MSL for on-board
// debayering of MMM images.
//
////////////////////////////////////////////////////////////////////////
void fill_holes_malvar_interp(SimpleImage<double> *db, 
                              SimpleImage<double> *out1,
                              SimpleImage<double> *out2,
                              SimpleImage<double> *out3,
                              double min_dn, double max_dn)
{
// MMM Bayer layout (2x2) =
//     R G
//     G B

// Zero-based row,col
//     even row, even col = R
//     even row, odd  col = G
//     odd  row, even col = G
//     odd  row, odd  col = B
    int line, samp, i, j;
    int error_count = 0;
    double region[5][5] = {0};
    double red=0.0, green=0.0, blue=0.0;
    const size_t msgLen = 1024;
    char msg[msgLen];

    int MAXL, MAXS;
    MAXL = db->getNL();
    MAXS = db->getNS();

    // algorithm for Zero-based raster
    // for every pixel calculate RGB values based on the kernels above
    // start at (2,2) and finish at (MAXL,MAXS-2) for edges on 5x5 kernel
    // to prevent going off of the image during interpolation
    for (line = 0; line < MAXL; line++){
        for (samp = 0; samp < MAXS; samp++){
            // create 2 pixel wide line border
            if (line < 2 || line >= MAXL-2){
                red = green = blue = 0;
            }
            // create 2 pixel wide sample border
            else if (samp < 2 || samp >= MAXS-2){
                red = green = blue = 0;
            }
            else{  // inside 2x2 border
                // if pixel has no data then skip the RGB interpolation
                if (db->get(line,samp) == 0){
                    // dont do anything;
                    red = green = blue = 0;
                }
                else{
                    // get 5x5 region around current pixel
                    for (i=-2;i<=2;i++)
                        for(j=-2;j<=2;j++)
                            region[i+2][j+2]=db->get(line+i,samp+j);
                 
                    // if even row, even col then pixel is Red
                    // interp GB at red row, red column
                    if (line % 2 == 0 && samp % 2 == 0){
                        // copy R band pixel value
                        red = db->get(line,samp);
                        // interp G band pixel value
                        green = green_at_red_interp(region);               
                        // interp B band pixel value
                        blue = blue_at_red_Rrow_Rcol_interp(region);
                        // set RGB output
                        if (red == -1)
                            error_count += 1;
                    }   
                    // if even row, odd col then pixel is Green
                    // interp RB at red row, blue column
                    if (line % 2 == 0 && samp % 2 == 1){
                        // interp R band pixel value
                        red = red_at_green_Rrow_Bcol_interp(region);
                        // copy G band pixel value
                        green = db->get(line,samp);
                        // interp B band pixel value
                        blue = blue_at_green_Rrow_Bcol_interp(region);

                        // set RGB output
                        if (green == -1)
                            error_count += 1;
                    }
                    // if odd row, even col then pixel is Green 
                    // interp RB at blue row, red column
                    if (line % 2 == 1 && samp % 2 == 0){
                        // interp R band pixel value
                        red = red_at_green_Brow_Rcol_interp(region);
                        // copy G band pixel value
                        green = db->get(line,samp);
                        // interp B band pixel value
                        blue = blue_at_green_Brow_Rcol_interp(region);

                        // set RGB output
                        if (green == -1)
                            error_count += 1;
                    } 
                    // if odd row, odd col then pixel is Blue
                    // interp RG at blue row, blue column
                    if (line % 2 == 1 && samp % 2 == 1){
                        // interp R band pixel value
                        red = red_at_blue_Brow_Bcol_interp(region);
                        // interp G band pixel value
                        green = green_at_blue_interp(region);
                        // copy B band pixel value
                        blue = db->get(line,samp);
 
                        // set RGB output
                        if (blue == -1)
                            error_count += 1;
                    }
                    // default is to set underflow to 1 because 0 means no data
                    // set overflow to 2^n - 1 where n = input number of bits, default = 255 (8-bit input)
                    // user can set underflow to 0 if desired
                    if (red < min_dn) red = min_dn;
                    if (red > max_dn) red = max_dn;
                    if (green < min_dn) green = min_dn;
                    if (green > max_dn) green = max_dn;
                    if (blue < min_dn) blue = min_dn;
                    if (blue > max_dn) blue = max_dn;
                }  // end else pixel > 0 DN value 

                memset(region,0,25*(sizeof(double)));    // reset 5x5 region
            }    // end else (inside 2x2 border region)
           
            out1->set(line,samp,red);
            out2->set(line,samp,green);
            out3->set(line,samp,blue);
        }    // end samp for loop
    }    // end line for loop

    if (error_count != 0) {
        snprintf(msg, msgLen, "Warning: %d pixels did not have a proper value", error_count);
        zvmessage(msg, "");
    }
}

////////////////////////////////////////////////////////////////////////
// Shrink the image by 2x horizonally, while filling holes by replication
// (which looks vertically only).  There cannot be values in both horizontal
// slots due to how the pattern is defined, so we just pick the first one
// we find.  If both are holes, look to the other line in the cell and do
// the same thing.
//
// The output is 1/2 size horizontally only, compared to the input.
////////////////////////////////////////////////////////////////////////

void tall_rep(SimpleImage<double> *db, SimpleImage<double> *out)
{
    int line, samp;
    int error_count = 0;

    for (line = 0; line < out->getNL(); line++) {
	for (samp = 0; samp < out->getNS(); samp++) {
	    if (db->get(line,samp*2) != -1) {
		out->set(line,samp, db->get(line,samp*2));	// good value
	    }
	    else if (db->get(line, samp*2+1) != -1) {
		out->set(line,samp, db->get(line,samp*2+1));
	    } else {

		// look at the other line

		int other_line = (line / 2) * 2;	// cell boundary
		if (other_line == line) other_line++;

		if (db->get(other_line,samp*2) != -1) {
		    out->set(line,samp, db->get(other_line,samp*2));
		}
		else {
		    out->set(line,samp, db->get(other_line, samp*2+1));
		}
	    }
	    if (out->get(line,samp) == -1) {	// whoops!
		out->set(line,samp,0);
		error_count++;
	    }
	}
    }

    if (error_count != 0) {
	char msg[1024];
	snprintf(msg, 1024, "Warning: %d pixels did not have a proper value", error_count);
	zvmessage(msg, "");
    }
}


////////////////////////////////////////////////////////////////////////
// Shrink the image by 2x horizonally, while filling holes by interpolation
// (which looks vertically only).  There cannot be values in both horizontal
// slots due to how the pattern is defined, so we just pick the first one
// we find.  This works for the greens, and the reds or blues if you're on
// the same line.  If both are holes, interpolate from the lines above and
// below (which could be in either column).
//
// The output is 1/2 size horizontally only, compared to the input.
////////////////////////////////////////////////////////////////////////

void tall_interp(SimpleImage<double> *db, SimpleImage<double> *out)
{
    int line, samp, i, j;
    int error_count = 0;

    for (line = 0; line < out->getNL(); line++) {
	for (samp = 0; samp < out->getNS(); samp++) {
	    if (db->get(line,samp*2) != -1) {
		out->set(line,samp, db->get(line,samp*2));	// good value
	    }
	    else if (db->get(line, samp*2+1) != -1) {
		out->set(line,samp, db->get(line,samp*2+1));
	    } else {

		// No value on this line, interpolate above & below.  For
		// simplicity, we simply interpolate all 4 above-and-below
		// neighbors in the same horizontal cell.  Only 2 will be
		// active.

		double sum = 0.0;
		int npix = 0;

		for (i = -1; i <= 1; i++) {
		   int ll = line+i;
		   if (ll < 0) continue;	// skip if off edge
		   if (ll >= out->getNL()) continue;

		   for (j = 0; j <= 1; j++) {	// Just in this cell
			int ss = samp*2 + j;

			if (db->get(ll,ss) != -1) {
			    sum += db->get(ll,ss);
			    npix++;
			}
		    }
		}

		if (npix == 0) {
		    error_count++;
		    out->set(line,samp, 0);
		}
		else {
		    out->set(line,samp, sum / npix);
		}
	    }
	    if (out->get(line,samp) == -1) {	// whoops!
		out->set(line,samp,0);
		error_count++;
	    }
	}
    }

    if (error_count != 0) {
	char msg[1024];
	snprintf(msg, 1024, "Warning: %d pixels did not have a proper value", error_count);
	zvmessage(msg, "");
    }
}

////////////////////////////////////////////////////////////////////////
// Subsample by 2x, averaging all values in the cell.
//
// The output should be 1/2 size compared to the input
////////////////////////////////////////////////////////////////////////

void average_down(SimpleImage<double> *db, SimpleImage<double> *out)
{
    int line, samp, i, j;
    int error_count = 0;

    for (line = 0; line < out->getNL(); line++) {
	for (samp = 0; samp < out->getNS(); samp++) {
	    // Average the values in the cell.  Should only be one or two
	    // values.

	    double sum = 0.0;
	    int npix = 0;

	    for (i = 0; i <= 1; i++) {
		for (j = 0; j <= 1; j++) {

		    if (db->get(line*2 + i, samp*2 + j) != -1) {
			sum += db->get(line*2 + i, samp*2 + j);
			npix++;
		    }
		}
	    }

	    if (npix == 0) {
		error_count++;
		out->set(line,samp, 0);
	    }
	    else {
		out->set(line,samp, sum / npix);
	    }
	}
    }

    if (error_count != 0) {
	char msg[1024];
	snprintf(msg, 1024, "Warning: %d pixels did not have a proper value", error_count);
	zvmessage(msg, "");
    }
}

