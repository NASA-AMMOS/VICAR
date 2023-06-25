////////////////////////////////////////////////////////////////////////
// mars_footprints.cc
//
// Adds overlap footprints and image numbers into the output image.
// Caller must supply a function to project an input coordinate into the
// the output with the following signature:
//    extern "C" int ProjectFunc(double in_line, double in_samp,
//                              double *out_line, double *out_samp,
//                              int input_number, void *proj_args);
// where "proj_args" is usually a structure of other parameters defined by
// the caller (the function should return 0 for success or non-zero for
// error, such as unprojectable point).  The routine writes directly to the
// given unit number which should be CLOSED before calling this function.
//
// This routine requires the following parameters be in the PDF:
//
// PARM NUMBER TYPE=KEYWORD VALID=("NUMBER","NONUMBER","INV_NUMBER") DEFAULT="NONUMBER"
// PARM NUMBER_DN TYPE=REAL DEFAULT=4096 COUNT=(1:32)
// PARM NUMBER_ZOOM TYPE=INTEGER DEFAULT=1
// PARM NUMBER_START TYPE=INTEGER DEFAULT=1
// PARM FOOTPRT TYPE=KEYWORD VALID=("NOFOOTPRINT", "FOOTPRINT", "OVERLAP") +
//		DEFAULT=NOFOOTPRINT
// PARM FOOT_DN TYPE=REAL DEFAULT=4096 COUNT=(1:32)
// PARM FOOT_RANGE TYPE=INTEGER COUNT=0:2 DEFAULT=--
// PARM FOOT_COLOR TYPE=STRING COUNT=0:1 DEFAULT=--
//
// (only the OVERLAP keyword is used from FOOTPRT).  For the two DN's, if
// fewer values are given than the number of bands, the last value will be
// repeated as needed.  And any value of 0 ends up transparent (is not written
// to the file, i.e. it's not written as a 0).
////////////////////////////////////////////////////////////////////////

#include "mars_support.h"

#include "PigFileModel.h"
#include "PigCameraModel.h"

#include "zvproto.h"

#include "text.h"

#include <string.h>
#include <stdio.h>

void mars_footprints(int unit, int nids, int nso, int nlo, int nbo,
		PigCameraModel *camera_in[], PigFileModel *file_in[],
		ProjectFunc func, void *proj_args)
{
    int i, j, k, b, status;
    int count;
    int number, number_inv, number_zoom, number_start;
    float number_dn[MARS_MAX_NB];
    int footprint_overlap;
    float foot_dn[MARS_MAX_NB];

    // reopen output for update.  We set U_FORMAT to REAL,
    // that way both "short int" and "float" files can
    // share the same code.  
    zvopen(unit, "OP", "UPDATE", "U_FORMAT", "REAL",
	   "OPEN_ACT", "AS", NULL);

    // First get the relevant parameters

    number = zvptst("NUMBER");
    number_inv = zvptst("INV_NUMBER");
    zvp("NUMBER_DN", number_dn, &count);
    if (count == 0)			// in case the caller program forgot
	number_dn[0] = 32767;
    if (count < nbo) {			// fill out unspecified bands
	for (int i=count; i<nbo; i++)
	    number_dn[i] = number_dn[count-1];
    }
    zvp("NUMBER_ZOOM", &number_zoom, &count);
    if (count == 0)
	number_zoom = 1;		// in case the caller program forgot
    zvp("NUMBER_START", &number_start, &count);
    if (count == 0)
	number_start = 1;		// in case the caller program forgot

    footprint_overlap = zvptst("OVERLAP");
    zvp("FOOT_DN", foot_dn, &count);
    if (count == 0)			// in case the caller program forgot
	foot_dn[0] = 32767;
    if (count < nbo) {			// fill out unspecified bands
	for (int i=count; i < nbo; i++)
	    foot_dn[i] = foot_dn[count-1];
    }

    // See if we have a footprint color file.  This is a simple text file
    // that specifies the rgb color to use for a range of footprint inputs.
    // For example:
    // 1-3 4095,1,1
    // 5-10 1,4095,1
    // would color the footprints for images 1-3 red, 5-10 green, and anything
    // else the normal foot_dn color.  Applies to image numbers as well.

    int use_foot_color = 0;
    auto foot_colors = new float[nids][MARS_MAX_NB];
    auto num_colors = new float[nids][MARS_MAX_NB];
    if (foot_colors == NULL || num_colors == NULL) {
	zvmessage("Error allocating foot_colors or num_colors", "");
	zabend();
    }

    for (i=0; i < nids; i++) {
	for (j=0; j < MARS_MAX_NB; j++) {
	    foot_colors[i][j] = foot_dn[j];
	    num_colors[i][j] = number_dn[j];
	}
    }

    char foot_color_file[PIG_MAX_FILENAME_SIZE];
    zvp("FOOT_COLOR", foot_color_file, &count);
    if (count != 0) {
	use_foot_color = 1;
	FILE *f = fopen(foot_color_file, "r");
	if (f == NULL) {
	    zvmessage("Error opening foot_color file", "");
	    zabend();
	}

	// The file read is very brittle... must be the EXACT format

	int start, end;
	double r, g, b;
	while (fscanf(f, "%d-%d %lf,%lf,%lf\n", &start, &end, &r, &g, &b) != EOF) {
	    for (i=start; i<=end; i++) {
		foot_colors[i-1][0] = r;
		foot_colors[i-1][1] = g;
		foot_colors[i-1][2] = b;
		num_colors[i-1][0] = r;
		num_colors[i-1][1] = g;
		num_colors[i-1][2] = b;
	    }
	}
	fclose(f);
    }

    // See if we want to do just a subset for footprints and numbers

    int foot_range[2];
    zvp("FOOT_RANGE", foot_range, &count);
    if (count != 2) {
	foot_range[0] = 1;
	foot_range[1] = nids;
    }

    // Add numbers annotation if requested

    if (number || number_inv) {
	zvmessage("Adding image numbers...", "");
	for (int ii = foot_range[0]-1; ii < foot_range[1]; ii++) {
 	    i = ii;
	    if (number_inv)		// number_inv just counts backwards
	        i = (nids-1-i);

            if ((camera_in[i] == NULL) || (file_in[i] == NULL))
                continue;

	    // Get coords of center of image.  This used to use getCameraCenter
	    // but that doesn't work for some cameras, notably M20 RMI.  So we
	    // just get the center of the image from the file model, instead.

	    double x_center = (file_in[i]->getNS() - 0.5) / 2.0 + 0.5;
	    double y_center = (file_in[i]->getNL() - 0.5) / 2.0 + 0.5;

	    // Project to output image

	    double line, samp;

	    if ((*func)(y_center, x_center, &line, &samp, i, proj_args) != 0)
		continue;			// skip on error
	    int x = (int)(samp + 0.5);
	    int y = (int)(line + 0.5);

	    if (x < 0 || x >= nso || y < 0 || y >= nlo)
		continue;		// skip if center is out of bounds

	    char str[10];
	    sprintf(str, "%d", i+number_start);	// i is 0-based

	    mars_write_text(unit, nso, nlo, nbo, str, x, y,
				number_zoom, num_colors[i], Center);
	}
    }

    // Add overlapping footprints if desired.  This is done rather
    // inefficiently (via single-pixel writes) for code simplicity, and
    // since this should never be used for a production mosaic!  It's too
    // hard to predict which borders will appear on the same output line
    // (they will rarely be horizontal), so the inefficient method is used.

    if (footprint_overlap) {
	zvmessage("Adding overlapping footprints...", "");

        for (b = 0; b < nbo; b++) {
	    for (i = foot_range[0]-1; i < foot_range[1]; i++) {
                if ((camera_in[i] == NULL) || (file_in[i] == NULL))
                    continue;

	        double sl, ss, el, es;
	        double line, samp;
	        int phys_line, phys_samp;

	        file_in[i]->getImageBorders(sl, ss, el, es);

	        // Loop down the left and right edges
 
	        for (j = (int)sl; j < (int)el; j++) {
		    status = (*func)((double)j, ss, &line, &samp, i, proj_args);
		    phys_line = (int)(line+0.5) + 1;
		    phys_samp = (int)(samp+0.5) + 1;
		    if (status == 0 &&
		        phys_line > 0 && phys_line <= nlo &&
		        phys_samp > 0 && phys_samp <= nso) {
			if (foot_colors[i][b] != 0) {
		    	    zvwrit(unit, &foot_colors[i][b], "LINE", phys_line,
				   "SAMP",phys_samp, "NSAMPS",1, "BAND",b+1, NULL);
			}
		    }
		

		    status = (*func)((double)j, es, &line, &samp, i, proj_args);
		    phys_line = (int)(line+0.5) + 1;
		    phys_samp = (int)(samp+0.5) + 1;
		    if (status == 0 &&
		        phys_line > 0 && phys_line <= nlo &&
		        phys_samp > 0 && phys_samp <= nso) {

		        if (foot_colors[i][b] != 0) {
		            zvwrit(unit, &foot_colors[i][b], "LINE", phys_line,
			           "SAMP",phys_samp, "NSAMPS",1, "BAND",b+1, NULL);
		        }
		    }
	        }

	        // Loop across the top and bottom edges
	        for (j = (int)ss; j < (int)es; j++) {
		    status = (*func)(sl, (double)j, &line, &samp, i, proj_args);
		    phys_line = (int)(line+0.5) + 1;
		    phys_samp = (int)(samp+0.5) + 1;
		    if (status == 0 &&
		        phys_line > 0 && phys_line <= nlo &&
		        phys_samp > 0 && phys_samp <= nso) {

			if (foot_colors[i][b] != 0) {
		    	    zvwrit(unit, &foot_colors[i][b], "LINE", phys_line,
				"SAMP",phys_samp, "NSAMPS",1, "BAND",b+1, NULL);
			}
                    }

		    status = (*func)(el, (double)j, &line, &samp, i, proj_args);
		    phys_line = (int)(line+0.5) + 1;
		    phys_samp = (int)(samp+0.5) + 1;
		    if (status == 0 &&
		        phys_line > 0 && phys_line <= nlo &&
		        phys_samp > 0 && phys_samp <= nso) {

		        if (foot_colors[i][b] != 0) {
		    	    zvwrit(unit, &foot_colors[i][b], "LINE", phys_line,
			       "SAMP",phys_samp, "NSAMPS",1, "BAND",b+1, NULL);
		        }
		    }
		}
	    }
	}
    }
    // Lastly close the file.  This is needed since we opened it with
    // U_FORMAT=REAL and the actual file data type might be different
    // (SHORT for example).  
    zvclose(unit, NULL);
}

