#ifndef NSYT_TILT_H
#define NSYT_TILT_H

#include "nsyt_instruments.h"
#include "PigVector.h"
#include "PigCoordSystem.h"

#include <vector>


struct TiltParams
{
    InsightInstrument inst;
    InstrumentParams iparams;
    double tilt_threshold;
    double point_epsilon;
    int leaf_max_size;
    double possible_sinkage;
    int foot_window;
    double bad_tilt;
    double clock_range[2];
    double clock_step;
    double delta_wts_range[2];
    double delta_wts_step;
    double filter_scale;
};

// Unit tested in "test_insight_tilt.cc".
std::pair<double, double> min_max_tilt(
    std::vector<PigPoint> feet_points,
    double possible_sinkage);

// Unit tested in "test_insight_tilt.cc".
std::pair<double, double> min_max_delta_tilt(
    std::vector<PigPoint> instrument_a,
    std::vector<PigPoint> feet_points,
    double possible_sinkage);

void compute_tilt(const TiltParams &params,
                  SimpleImage<double> input_xyz[3],
                  const std::vector<SampleCircle> &circles,
                  SimpleImage<double> output_min_max_tilt[2],
		  SimpleImage<double> output_avg_normal[3],
		  SimpleImage<double> &output_z,
                  SimpleImage<double> output_xpx[3],
                  PigCoordSystem *cs, PigCoordSystem *p_cs, int do_z_lander);

void compute_delta_tilt(const TiltParams &params,
                        SimpleImage<double> input_xyz[3],
                        const std::vector<SampleCircle> &instrument_a,
                        const std::vector<SampleCircle> &instrument_b,
                        InsightInstrument instrument_type_a,
                        InsightInstrument instrument_type_b,
                        SimpleImage<double> output_min_max_tilt[2]);

#endif
