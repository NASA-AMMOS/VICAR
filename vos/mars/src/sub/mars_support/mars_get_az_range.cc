////////////////////////////////////////////////////////////////////////
// mars_get_az_range.cc
//
// Determines the azimuth range for a set of images, where each is marked
// by az_low[i] and az_high[i].  This is complicated by the possibility
// of wrapping around (az=0/360); we want to find the smallest contiguous
// range that includes all the images.  The min_az and max_az are returned.
// If min_az > max_az, then the image wrapped around, i.e. the 0 point is
// in the middle.  All values are in radians.
//
// The algorithm is from Jean Lorre.
////////////////////////////////////////////////////////////////////////

#include "mars_support.h"

#include "PigVector.h"

void mars_get_az_range(const int nids, double az_low[], double az_high[],
		double &min_az, double &max_az, double wrap_az)
{
    int i, j;
    int flags[360];		// one per degree
    double azl[360], azh[360];	// min and max edge for that degree

    for (i=0; i < 360; i++) {
	flags[i] = 0;
	azl[i] = 1e8;
	azh[i] = -1e8;
    }

    // Fill in the flags for each degree column covered by each image

    for (i=0; i < nids; i++) {
	double az1 = PigRad2Deg(az_low[i]);
	double az2 = PigRad2Deg(az_high[i]);
	if (az1 >= 360.) az1 -= 360.;
	if (az1 < 0.) az1 += 360.;
	if (az2 >= 360.) az2 -= 360.;
	if (az2 < 0.) az2 += 360.;

	// Fill in the easy way

	if (az2 > az1) {
	    for (j = (int)az1; j <= (int)az2; j++)
		flags[j] = 1;
	}
	else {

	    // Fill in 2 pieces... az1..360 and 0..az2

	    for (j = (int)az1; j < 360; j++)
		flags[j] = 1;
	    for (j = 0; j <= (int)az2; j++)
		flags[j] = 1;

	    // And make sure the edges cover everything

	    azl[0] = 0.0;
	    azh[359] = 360.0;
	}

	// Fill in actual values of edges

	if (azl[(int)az1] > az1) azl[(int)az1] = az1;
	if (azh[(int)az2] < az2) azh[(int)az2] = az2;
    }

    // Now determine the azimuth limits by examining the flags
    double wrap_az_degrees = PigRad2Deg(wrap_az);

    if (flags[0] && flags[359]) {		// bridge the gap
	j = 0;
	for (i=1; i < 360; i++) {
	    if (!flags[i]) {			// found the first empty column
		max_az = azh[i-1];		// it's the max
		j = i;
		break;
	    }
	}
	if (j == 0) {				// Entire thing is full
	    min_az = wrap_az_degrees;
	    max_az = wrap_az_degrees;
	}
	else {
	    for (i=j; i < 360; i++) {
		if (flags[i]) {			// found the first full column
		    min_az = azl[i];		// it's the min
		    break;
		}
	    }
	}
    }
    else {					// No bridging
	for (i=0; i < 360; i++) {
	    if (flags[i]) {			// found the first full column
		min_az = azl[i];		// it's the min
		break;
	    }
	}
	for (i=359; i > 0; i--) {
	    if (flags[i]) {			// found the last full column
		max_az = azh[i];		// it's the max
		break;
	    }
	}
    }

    // Renormalize to radians

    max_az = PigDeg2Rad(max_az);
    min_az = PigDeg2Rad(min_az);

    return;
}

