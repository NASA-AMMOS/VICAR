////////////////////////////////////////////////////////////////////////
// mars_get_azel_minmax.cc
//
// Determines the minimum and maximum azimuth and elevation (in the passed-in
// coordinate system) for a set of input images by examining the corner and
// center-of-edge points.  This routine is not precise because the actual
// projection is not done... it examines only at the look vectors for each
// camera, ignoring any changes in camera location.  The centers-of-edges
// are done because some projections result in a convex curved image edge.
// Values are returned in radians.
////////////////////////////////////////////////////////////////////////

#include "mars_support.h"

#include "PigFileModel.h"
#include "PigPointingModel.h"
#include "PigCameraModel.h"
#include "PigCoordSystem.h"

#include "zvproto.h"

void mars_get_azel_minmax(const int nids, PigPointingModel *pointing_in[],
			PigCameraModel *camera_in[], PigFileModel *file_in[],
			double &min_elev, double &max_elev,
			double &min_az, double &max_az, PigCoordSystem *cs,
			double wrap_az)
{
    int i, i_good;
    min_elev = 1e8;				// these are in radians
    max_elev = -1e8;
    double *az_low, *az_high;

    az_low = new double[nids];
    az_high = new double[nids];

    if (az_low == NULL || az_high == NULL) {
	zvmessage("Fatal memory error in mars_get_azel_minmax!", "");
	zabend();
    }

// If the azimuth is more than 180 degrees away from the "center", then
// we assume it must have wrapped and make the az <0 or >360 (to retain
// the image limits on the proper sides of the center).
#define MINMAX_AZEL_CHECK( n )					\
	azim = cs->getAz(look);						\
	if (azim >= PigDeg2Rad(360.0)) azim -= PigDeg2Rad(360.0);	\
	if (azim < 0) azim += PigDeg2Rad(360.0);			\
	if (azim - az_center > PigDeg2Rad(180.0))			\
	    azim -= PigDeg2Rad(360.0);					\
	else if (az_center - azim > PigDeg2Rad(180.0))			\
	    azim += PigDeg2Rad(360.0);					\
	elev = cs->getEl(look);						\
	if (azim > az_high[(n)]) az_high[(n)] = azim;			\
	if (azim < az_low[(n)]) az_low[(n)] = azim;			\
	if (elev > max_elev) max_elev = elev;				\
	if (elev < min_elev) min_elev = elev;

    i_good=0;
    for (i=0; i < nids; i++) {
        if ((file_in[i] == NULL) || (camera_in[i] == NULL))
            continue;

	PigPoint origin;
	PigVector look;
	double line, samp;
	double azim, elev;
	double sl, ss, el, es;
	double az_center;

	// This used to be done with getCameraOrientation()... but that fails
	// miserably in some linearization situations where the A vector
	// doesn't even hit the image!  (notably MSL Mastcam stereo).  So we
	// use the actual center of the image instead.

	file_in[i]->getImageBorders(sl, ss, el, es);
	camera_in[i]->LStoLookVector((sl+el)/2, (ss+es)/2, origin, look, cs);

	az_center = cs->getAz(look);		// in order to check for wrap
	if (az_center >= PigDeg2Rad(360.0)) az_center -= PigDeg2Rad(360.0);
	if (az_center < 0) az_center += PigDeg2Rad(360.0);
	az_low[i_good] = az_high[i_good] = az_center;	// just in case...


	camera_in[i]->LStoLookVector(sl, ss, origin, look, cs);	// UL corner
	MINMAX_AZEL_CHECK(i_good)

	camera_in[i]->LStoLookVector(sl, es, origin, look, cs);	// UR corner
	MINMAX_AZEL_CHECK(i_good)

	camera_in[i]->LStoLookVector(el, es, origin, look, cs);	// LR corner
	MINMAX_AZEL_CHECK(i_good)

	camera_in[i]->LStoLookVector(el, ss, origin, look, cs);	// LL corner
	MINMAX_AZEL_CHECK(i_good)

	camera_in[i]->LStoLookVector((sl+el)/2, ss, origin, look, cs);	// left
	MINMAX_AZEL_CHECK(i_good)

	camera_in[i]->LStoLookVector((sl+el)/2, es, origin, look, cs);	// right
	MINMAX_AZEL_CHECK(i_good)

	camera_in[i]->LStoLookVector(sl, (ss+es)/2, origin, look, cs);	// top
	MINMAX_AZEL_CHECK(i_good)

	camera_in[i]->LStoLookVector(el, (ss+es)/2, origin, look, cs);	// btm
	MINMAX_AZEL_CHECK(i_good)

	// Now check for wrap and adjust accordingly (make az_low > az_high
	// if wrap occurred)
	if (az_low[i_good] < 0.0 && az_high[i_good] >= PigDeg2Rad(360.0)) {
	    az_low[i_good] = 0.0;				// both wrapped??
	    az_high[i_good] = PigDeg2Rad(360.0);		// just use max range
	}
	else if (az_low[i_good] < 0.0)			// low side wrapped
	    az_low[i_good] += PigDeg2Rad(360.0);
	else if (az_high[i_good] >= PigDeg2Rad(360.0))	// high side wrapped
	    az_high[i_good] -= PigDeg2Rad(360.0);

        i_good++;
    }

    if (i_good == 0) {
	zvmessage("No data for azimuth range determination", "");
	zabend();
    }

    mars_get_az_range(i_good, az_low, az_high, min_az, max_az, wrap_az);

    delete[] az_low;
    delete[] az_high;

    return;
}

