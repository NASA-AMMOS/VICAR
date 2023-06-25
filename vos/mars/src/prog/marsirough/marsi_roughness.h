#ifndef NSYT_ROUGHNESS_H
#define NSYT_ROUGHNESS_H

#include "marsi_instruments.h"
#include "SimpleImage.h"

typedef struct RoughnessParams
{
    PigInstrumentParams *iparams;	// params for instrument footprints
    int leaf_max_size;
    double point_epsilon;
    double sinkage;		// settling value to use

    double body_thresh;
    double feet_thresh;

    double clock_range[2];
    double clock_step;
    double body_step;

    int max_window_size;	// Conservative guess at maximum window size
				// which points falling into the radius will
				// be found.  This is used to prevent infinite
				// loops, and a zero or negative values will
				// cause a default of MIN(num_rowd,num_cols)/8
				// to be used.
    double bad_roughness;	// Value to use for lack of roughness data.
    double filter_scale;	// Filter points with roughness >= filter*std,
				// where std is the standard deviation of all
				// points within radius
    int min_close_points;	// Minimum number of points used in the plane
				// fit.  Should be greater than or equal
				// to min_num_points; a value of 6 or more
				// is recommended.
    double x_center, y_center;	// Center of bounding box in XYZ
				// space.  Points lying outside the box will
				// be ignored.
    double box_radius;		// Half-width of bounding box

} RoughnessParams;

// Compute the roughness for the given shape.  This computes the actual
// numeric roughness value for one band, not the status band.
// One version for standard roughness, the other for hill roughness

void computeStdRoughness(const RoughnessParams &params,
			SimpleImage<PigPoint> xyz,
			SimpleImage<double> uvw[3],
			InstrumentShape &shape,
			SimpleImage<double> &rough,
			double clock_range[2],
			double clock_step);
void computeHillRoughness(const RoughnessParams &params,
			SimpleImage<PigPoint> xyz,
			SimpleImage<double> uvw[3],
			SimpleImage<double> zix,
			InstrumentShape &shape,
			SimpleImage<double> &rough,
			double clock_range[2],
			double clock_step);

#endif
