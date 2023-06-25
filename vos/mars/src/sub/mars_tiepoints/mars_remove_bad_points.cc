////////////////////////////////////////////////////////////////////////
// mars_remove_bad_points.cc
//
// Remove bad points from the tiepoint list.  Bad points are defined as
// points with a quality <= thresh (usually 0.0).
////////////////////////////////////////////////////////////////////////

#include "mars_tiepoints.h"

void mars_remove_bad_points(TiePoint tiepoints[], int &n_overlaps,
		double thresh)
{

    int k = 0;
    for (int i=0; i < n_overlaps; i++) {
	if (tiepoints[i].quality > thresh) {
	    tiepoints[k] = tiepoints[i];
	    k++;
	}
    }
    n_overlaps = k;
}

