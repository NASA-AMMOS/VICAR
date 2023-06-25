/* medfill */
#include "vicmain_c"
#include "math.h"
#include "SimpleImage.h"

int is_bad(double dn, int zero, int nonan, int use_min, int use_max,
					  double min, double max);

////////////////////////////////////////////////////////////////////////

void main44()
{
    int i, j;
    int status, count, def;
    char msg[255];

    zvmessage("MEDFILL version 1", "");

    zveaction("SA", "");

    // Read input file

    int inp_unit, nl, ns, nb;

    zvunit(&inp_unit, "INP", 1, NULL);

    zvopen(inp_unit, "op", "read", "u_format", "doub", NULL);
    zvget(inp_unit, "NL", &nl, "NS", &ns, "NB", &nb, NULL);

    SimpleImage<double> img;
    img.alloc(nb, nl, ns);
    SimpleImage<double> out_img;
    out_img.alloc(nb, nl, ns);

    for (int b = 0; b < nb; b++) {
	for (int l = 0; l < nl; l++) {
	    zvread(inp_unit, img.linePtr(b,l), "BAND", b+1, "LINE", l+1, NULL);
	}
    }
    zvclose(inp_unit, NULL);

    // Get parameters

    int nlw, nsw;
    zvp("NLW", &nlw, &count);
    zvp("NSW", &nsw, &count);

    if (nlw % 2 != 1 || nsw % 2 != 1) {
	zvmessage("NLW and NSW must be odd", "");
	zabend();
    }

    int bayer = zvptst("BAYER");
    int linc = 1;
    int sinc = 1;
    if (bayer) {
	linc = 2;
	sinc = 2;
    }

    double nodata;
    int use_nodata = 0;
    zvparmd("NODATA", &nodata, &count, &def, 1, 0);
    if (count != 0)
	use_nodata = 1;

    int zero = zvptst("ZERO");
    int nonan = zvptst("NONAN");

    double min, max;
    int use_min = 0;
    int use_max = 0;

    zvparmd("MIN", &min, &count, &def, 1, 0);
    if (count != 0)
	use_min = 1;

    zvparmd("MAX", &max, &count, &def, 1, 0);
    if (count != 0)
	use_max = 1;
    
    // Do the filtering!

    double *window_array = new double[nlw*nsw];
    int num_window = 0;

    int nlw2 = (nlw-1)/2;		// half window sizes
    int nsw2 = (nsw-1)/2;

    for (int band = 0; band < nb; band++) {
	for (int line = 0; line < nl; line++) {
	    for (int samp = 0; samp < ns; samp++) {

		double dn = img.get(band, line, samp);

		// Nothing to see here, move along...

		if (!is_bad(dn, zero,nonan,use_min,use_max,min,max)) {
		    out_img.set(band, line, samp, dn);
		    continue;
		}

		// We have a bad pixel, deal with it

		num_window = 0;

		for (int wl = -nlw2; wl <= +nlw2; wl+=linc) {
		    if ((line + wl) < 0) continue;
		    if ((line + wl) >= nl) continue;

		    for (int ws = -nsw2; ws <= +nsw2; ws+=sinc) {
		        if ((samp + ws) < 0) continue;
		        if ((samp + ws) >= ns) continue;

			double window_dn = img.get(band, line+wl, samp+ws);
			if (is_bad(window_dn, zero,nonan,use_min,use_max,min,max)) {
			    continue;		// skip bad pixels
			}

			// Good pixel, save it

			window_array[num_window++] = window_dn;
		    }
		}

		// If there's nothing in the window, punt

		if (num_window == 0) {
		    if (use_nodata) {
			out_img.set(band, line, samp, nodata);
		    } else {
			out_img.set(band, line, samp, dn);
		    }
		    continue;
		}

		// Now we have a window full of pixels, find the median.
		// There are probably lots of more efficient ways to do this,
		// but we KISS.  Go through the list n/2-1 times, each time
		// knocking out the minimum value (by setting it to something
		// super high).  At that point the minimum in the list is the
		// median.

		for (int i = 0; i < num_window/2-1; i++) {
		    // find the min
		    double minval = 1e30;
		    int minindex = 0;
		    for (int j = 0; j < num_window; j++) {
			if (window_array[j] < minval) {
			    minval = window_array[j];
			    minindex = j;
			}
		    }
		    // Knock out the min value
		    window_array[minindex] = 1e30;
		}

		// Find the min again, that's our median

		double minval = 1e30;
		for (int j = 0; j < num_window; j++) {
		    if (window_array[j] < minval) {
			minval = window_array[j];
		    }
		}

		// Finally... set the value

		out_img.set(band, line, samp, minval);
	    }
	}
    }

    // Write out the image

    int out_unit;
    zvunit(&out_unit, "OUT", 1, NULL);

    zvopen(out_unit, "op", "write", "u_format", "doub", NULL);
    zvplabel(out_unit, 0, 1);

    for (int b = 0; b < nb; b++) {
	for (int l = 0; l < nl; l++) {
	    zvwrit(out_unit, out_img.linePtr(b,l), "BAND", b+1,
						   "LINE", l+1, NULL);
	}
    }
    zvclose(out_unit, NULL);
}


////////////////////////////////////////////////////////////////////////
// Check to see if a pixel should be filled or ignored
////////////////////////////////////////////////////////////////////////

int is_bad(double dn, int zero, int nonan, int use_min, int use_max,
					  double min, double max)
{
    if (zero && dn == 0)
	return 1;
    if (!nonan && isnan(dn))
	return 1;
    if (use_min && dn < min)
	return 1;
    if (use_max && dn > max)
	return 1;
    return 0;
}

