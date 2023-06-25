/* marsmap */

#include "vicmain_c"


#include "mars_support.h"

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
#include <vector>

// buffer sizes in main program - these should be made dynamic, eventually!
#define MAX_INPUTS 2000
#define MAX_OPEN 2000 		// Largest size for max_open
#define MAX_NL 0		// no max
#define MAX_NS 0


////////////////////////////////////////////////////////////////////////

void main44()
{
    int status = 0;
    int count = 0; 
    int def = 0;
    const size_t msgLen = 256;
    char msg[msgLen];
    char mission[64], instrument[64];

    // Inputs

    int band = 0;
    int nids = 0;

    PigFileModel *file_models[MAX_INPUTS];
    PigCameraModel *camera_in[MAX_INPUTS];
    PigPointingModel *pointing_in[MAX_INPUTS];
    PigSurfaceModel *surface_model;
    int homogeneous_inputs = TRUE;
    SimpleImage<short int> *si_imgs[MAX_INPUTS];
    SimpleImage<float> *f_imgs[MAX_INPUTS];
    RadiometryModel *radiometric[MAX_INPUTS];
    PigBrtCorrModel *brt_corr[MAX_INPUTS];
    PigCoordSystem *proj_cs;

    int short_int_data = 1;
    int do_interp = 1;
    char format[10];                   // input data format
    format[0] = '\0';
    
    int max_open;			// Number of inputs to read at once

    // Outputs

    int unit_out=0;
    int nlo=0, nso=0;
    
    int do_print = TRUE;		// True if info message should be issued
    
    // User Parameters

    int min_input, max_input;

    zvmessage("MARSREMOS version 2020-02-10", "");

    do_print = TRUE;

    // Get the input file list, and set up initial camera/pointing models
    // for each input.

    mars_setup(nids, file_models, camera_in, pointing_in, surface_model,
               radiometric, brt_corr,
	       proj_cs, mission, instrument, homogeneous_inputs,
               MAX_NL, MAX_NS, MAX_INPUTS);

    PigMission *m = PigMission::getMissionObject(mission);

    // Find out the input data type.  We check the data type of the first 
    // available input and assume that ALL inputs have the same data type.
    // Currently we  support SHORT INT images and FLOAT images.  BYTE image
    // are supported by converting bytes into short ints.

    int i;
    for (i = 0; i < nids; i++) { 
        if (file_models[i] != NULL) {
            file_models[i]->openFile();
            status = zvget(file_models[i]->getUnit(), "FORMAT", format, NULL);
            break;
        }
    }
    if ((i >= nids) || (status != 1)) {
        zvmessage("Unable to determine input's data format", "");
	zabend();
    }
    if ((strcmp(format, "HALF") == 0) || (strcmp(format, "BYTE") == 0)) {
	strcpy(format, "HALF");
    } else {  
	strcpy(format, "REAL");
	short_int_data = 0;
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

    int maxNB = 1;
    for (int n = 0; n < nids; n++) {
        if (file_models[n] != NULL)
            maxNB = file_models[n]->getNB() > maxNB ?
                    file_models[n]->getNB() : maxNB;
    }
    status=zvparm("BAND",&band,&count,&def,1,0);
    int band_count = 1;
    
    if (count == 0) {
        // no input band specified; process all bands
        band_count = maxNB;
        snprintf(msg, msgLen, "Number of bands to be processed is (%d)", band_count);
        zvmessage(msg, "");
	band = 0;
    } else {
        // check if input band number is greater than number of bands in input
        if (band > maxNB) {
             snprintf(msg, msgLen,
                     "Input band (%d) is greater than number of bands in input image. Band set to 1.",
                     band);
             zvmessage(msg, "");
             band = 1;
        }
    }

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

    // We read and write each line as we go.  In this way the program does not
    // cnsume too much memory and allows progress to be monitored (e.g. in xvd).

    // Check for index and ICM inputs

    char idx_filename[PIG_MAX_FILENAME_SIZE];
    char icm_filename[PIG_MAX_FILENAME_SIZE];
    zvp("IDX", idx_filename, &count);
    if (count == 0) {
	zvmessage("Input IDX file name required", "");
	zabend();
    }
    zvp("ICM", icm_filename, &count);
    if (count == 0) {
	zvmessage("Input ICM file name required", "");
	zabend();
    }

    // Note:  MAX_OPEN is the ultimate max, for allocation.  max_open is
    // the actual size of each pass.

    zvp("MAX_OPEN", &max_open, &count);
    if (count == 0)
	max_open = 100;			// set to default below
    if (max_open > MAX_OPEN)
	max_open = MAX_OPEN;		// no need for message
    if (max_open < 0)
	max_open = 100;


    // Print out input status from labels.  It is expected that with
    // this program, the input list of image files may have place-
    // holder entries for files that may not exist when this program
    // is used.  Currently, mars_print_inputs(...) cannot handle
    // files that do not exist, and the program aborts.

    if (do_print)
        mars_print_inputs(nids, pointing_in, camera_in, file_models,
                homogeneous_inputs, mission, instrument);


    // Open input IDX and ICM files. They must have the same number
    // of lines and the same number of sample points.

    PigFileModel *idx_fm = PigFileModel::create(idx_filename);
    if (idx_fm == NULL) {
        snprintf(msg, msgLen, "Unable to create file-model for %s", idx_filename);
        zvmessage(msg, "");
        zabend();
    }
    PigFileModel *icm_fm = PigFileModel::create(icm_filename);
    if (icm_fm == NULL) {
        snprintf(msg, msgLen, "Unable to create file-model for %s", icm_filename);
        zvmessage(msg, "");
        zabend();
    }
    int unit_idx_in = idx_fm->getUnit();
    int unit_icm_in = icm_fm->getUnit();
    int nl_idx=0, ns_idx=0, nbnd_idx=0;
    int nl_icm=0, ns_icm=0, nbnd_icm=0;

    zvopen(unit_idx_in, "OP", "READ", NULL);
    idx_fm->setFileOpen(TRUE);
    status = zvget(unit_idx_in, "NS", &ns_idx, "NL", &nl_idx, "NB", &nbnd_idx, NULL);

    snprintf(msg, msgLen, "Input IDX lines = %d, samples = %d, bands = %d", nl_idx, ns_idx, nbnd_idx);
    if (do_print) zvmessage(msg, "");

    zvopen(unit_icm_in, "OP", "READ", NULL);
    icm_fm->setFileOpen(TRUE);
    status = zvget(unit_icm_in, "NS", &ns_icm, "NL", &nl_icm, "NB", &nbnd_icm, NULL);

    snprintf(msg, msgLen, "Input ICM lines = %d, samples = %d, bands = %d", nl_icm, ns_icm, nbnd_icm);
    if (do_print) zvmessage(msg, "");

    if ( (nl_idx != nl_icm) || (ns_idx != ns_icm) ) {
	zvmessage("Input ICM and input IDX don't have the same dimensions.", "");
	zabend();
    }


    // Ouput dimensions are the same as input IDX (or ICM) dimensions.

    nlo = nl_idx;
    nso = ns_idx;

    if ((nlo < 1) || (nso < 1)) {
	zvmessage("Unreasonable output file dimensions", "");
	zabend();
    }


    // Output buffers.

    std::vector< std::vector<short int> > obuf2d_si(MARS_MAX_NB);
    std::vector< std::vector<float> >  obuf2d_f(MARS_MAX_NB);

    // Coordinate input buffers

    std::vector<short int> ibuf_idx(nso);
    std::vector<float> ibuf_icm_l(nso);
    std::vector<float> ibuf_icm_s(nso);


    // Create the output file.  The output file is created in an unusual way.
    // The file is WRITE-opened, then closed, and then UPDATE-opened.  Ideally,
    // UPDATE-open only should work, but it does not.  As of 04/17/2020, only
    // one UPDATE-open at the beginning would cause the program to behave 
    // unpredictably.  Sometimes it would work, sometimes it would complain
    // that it could not open the file, and sometimes the program would crash.
    //
    // Another option explored was to use WRITE-open to write data during the
    // first pass, and then do a close/UPDATE-open to write data during the 
    // remaining passes (second and later).  However, writing to the file is
    // very slow when an already existing file is WRITE-opened:
    //    - WRITE-opened when the file already exists:  slow
    //    - WRITE-opened when the file does not already exist:  fast
    //    - UPDATE-opened when the file already exists:  fast
    //    - UPDATE-opened when the file does not already exist:  fast
    // Therefore, to maximize the speed of writing data to the file, (as of
    // 04/17/2020) it is best write only when the file is UPDATE-opened.

    zvselpiu(unit_idx_in);	// Transfer labels from IDX file
    zvunit(&unit_out, "OUT", 1, NULL);
    zvopen(unit_out, "OP", "WRITE", "U_FORMAT", format,
           "U_NS", nso, "U_NL", nlo, "OPEN_ACT", "AS",
           "U_NB", band_count, "U_ORG", "BSQ", "O_FORMAT", format, NULL);


    // Write the output labels

    PigLabelModel *labelModel = m->createLabelModel(unit_out);
    labelModel->setRemosaic(file_models, idx_fm, icm_fm, radiometric, brt_corr, 
                            nids, proj_cs);

    // reopen output for update

    zvclose(unit_out, NULL);
    zvopen(unit_out, "OP", "UPDATE", "U_FORMAT", format,
   	   "U_NS", nso, "U_NL", nlo, "OPEN_ACT", "AS",
   	   "U_NB", band_count, "U_ORG", "BSQ", "O_FORMAT", format, NULL);


    // Create and initialize variables necessary for pass loop
    int first_input, last_input;

    // Make a number of passes, processing max_open inputs each pass
    for (first_input = min_input; first_input < max_input;
	 first_input += max_open) {

	snprintf(msg, msgLen, "Pass %d of %d", (first_input/max_open)+1,
		((max_input-1)/max_open)+1);
	zvmessage(msg, "");

	last_input = first_input + max_open-1;
	if (last_input >= max_input)
	    last_input = max_input-1;
	
	// Read a set of inputs into memory

	if (short_int_data) {
            memset(si_imgs, 0, sizeof(si_imgs));
            mars_read_inputs(first_input, last_input, file_models,
			si_imgs, MAX_NL, MAX_NS, band, radiometric, brt_corr);
	} else {
            memset(f_imgs, 0, sizeof(f_imgs));
	    mars_read_inputs(first_input, last_input, file_models,
			f_imgs, MAX_NL, MAX_NS, band, radiometric, brt_corr);
	}

	// Loop through each sample of the output, and pick up the input
	// pixel that corresponds to it
	
	for (int j=0; j<nlo; j++) {			// line loop

	    if (j % 100 == 0) {
		snprintf(msg, msgLen, "line %d", j);
		zvmessage(msg, "");
	    }

	    zvread(unit_idx_in, ibuf_idx.data(), "LINE", j+1, NULL);

	    zvread(unit_icm_in, ibuf_icm_l.data(), "LINE", j+1, "BAND", 1, NULL);
	    zvread(unit_icm_in, ibuf_icm_s.data(), "LINE", j+1, "BAND", 2, NULL);

	    // Populate the output buffer. 
            // If this is the first pass, then set it to zero,
            // else, re-read output line.

	    for (int b = 0; b < band_count; b++) {
                if (first_input == min_input) {
                    if (short_int_data)
                        obuf2d_si[b].assign(nso, 0);
                    else
                        obuf2d_f[b].assign(nso, 0);
                } else {
                    if (short_int_data)
                        zvread(unit_out, obuf2d_si[b].data(), "LINE", j+1,"BAND", b+1,
                                NULL);
		    else
		        zvread(unit_out, obuf2d_f[b].data(), "LINE", j+1, "BAND", b+1,
                                NULL);
                }
            }

	    for (int i = 0; i < nso; i++) {		// sample loop
                int continue_flag = 0;
                for (int b = 0; b < band_count; b++) {
        	    if (short_int_data) {
		        if (obuf2d_si[b][i] != 0) {
		            // already set, do next iteration
                            continue_flag = 1;
                            break;
		        }
		    } else {
		        if (obuf2d_f[b][i] != 0.0) {
		             // already set, do next iteration
		             continue_flag = 1;
                             break;
                        }
		    }
		}
                if (continue_flag)
                    continue;
                int k = ibuf_idx[i]-1;
        	int kp = k-first_input;
                if ((k<first_input) || (k>last_input))
                    continue;
                if (short_int_data) {
                    if(!si_imgs[kp]) continue;
                } else {
                    if(!f_imgs[kp]) continue;
                }

		double image_line = ibuf_icm_l[i]-1;
		double image_samp = ibuf_icm_s[i]-1;

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
			
		    if (short_int_data) {
                        SimpleImage<short int> *img = si_imgs[kp];
                        int bb;
                        for (int b = 0; b < band_count; b++) {
			    bb = b;
                            if (bb >= img->getNB())
                                bb = img->getNB() - 1;
                            short int ul = img->get(bb,n,m);
			    short int ur = img->get(bb,n,m+1);
			    short int ll = img->get(bb,n+1,m);
			    short int lr = img->get(bb,n+1,m+1);
			    if (ul == 0) {
				if (ur != 0) ul = ur;
				else if (ll != 0) ul = ll;
				else ul = lr;
			    }
                            // ul is now known to be non-0 if possible
			    
                            // Slightly sub-optimal if ur was originally 0
			    // (in which case we should take lr) but that's
			    // not worth dealing with.
			    if (ur == 0) ur = ul;
			    if (ll == 0) ll = ul;
			    if (lr == 0) lr = ur;
			    top = wl * ul + wr * ur;
			    bot = wl * ll + wr * lr;

			    dn = (bot * wb + top * (1.0 - wb));
			    
			    if (dn <= 0.0)
				dn = 0.0;
			    if (dn > 32766.0)
				dn = 32766.0;
			    obuf2d_si[b][i] = (short int)(dn + 0.5);
                        }
		    } else {
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
			    
			    dn = (bot * wb + top * (1.0 - wb));
			    obuf2d_f[b][i] = dn;
                        }
		    }
		} else {			// Don't interpolate
		    if (short_int_data) {
                        SimpleImage<short int> *img = si_imgs[kp];
                        int bb;
                        for (int b = 0; b < band_count; b++) {
                            bb = b;
                            if (bb >= img->getNB())
                                bb = img->getNB() - 1;
			    dn = img->get(bb, (int)(image_line + 0.5),
				   (int)(image_samp+0.5));
			    
			    if (dn <= 0.0)
				dn = 0.0;
			    if (dn > 32766.0)
				dn = 32766.0;
			    obuf2d_si[b][i] = (short int)(dn + 0.5);
                        }
		    } else {
                        SimpleImage<float> *img = f_imgs[kp];
			int bb;
                        for (int b = 0; b < band_count; b++) {
                            bb = b;
                            if (bb >= img->getNB())
                                bb = img->getNB() - 1;
                            dn = img->get(bb, (int)(image_line + 0.5),
				    (int)(image_samp + 0.5));
			    obuf2d_f[b][i] = dn;
                        }
		    }
		}

	    }				// sample loop
	    

            for (int b = 0; b < band_count; b++) {
		if (short_int_data) {
		    zvwrit(unit_out, obuf2d_si[b].data(), "LINE", j+1,
                            "BAND", b+1, NULL);
	        } else {
       	            zvwrit(unit_out, obuf2d_f[b].data(), "LINE", j+1,
                            "BAND", b+1, NULL);
                }
            }
	}				// line loop
    }					// pass loop

    zvclose(unit_out, NULL);

}
