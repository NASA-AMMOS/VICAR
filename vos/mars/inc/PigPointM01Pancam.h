////////////////////////////////////////////////////////////////////////
// PigPointM01Pancam
//
// Pointing model for M01 Pancam camera.
//
// Currently works like Generic camera with 2 degrees of freedom:  Azimuth
// and Elevation.  Physically based pointing model is NOT yet implemented
// !!!!TBD
////////////////////////////////////////////////////////////////////////
#ifndef PIGPOINTM01PANCAM_H
#define PIGPOINTM01PANCAM_H

#include "PigPointTwoPivotCamera.h"

#include "PigVector.h"
#include "PigQuaternion.h"
#include "PigCoordSystem.h"

class PigPointM01Pancam : public PigPointTwoPivotCamera {

  protected:

    // Parameters from the pointing file...

    double _azimuth_offset;
    double _elevation_offset;
    double _degrees_per_motor_count_az;
    double _degrees_per_motor_count_el;
    double _cal_clicks_azimuth;
    double _cal_clicks_elevation;

    // Read in the calibration pointing parameters for a given pointing file

    void read_point_info(char *filename);

  public:

    PigPointM01Pancam(PigCameraModel *cm, PigMission *mission,
				       const char *instrument);
    virtual ~PigPointM01Pancam();

    // Point a camera model, given an image or an observation ID.  This
    // does not have to be the same image as was given in the constructor,
    // although presumably the mission/camera names should match!
    // data_source is included mainly to allow the overloading of these
    // two functions, but is intended to specify where to get the potining
    // data from, e.g. SPICE, database, etc.  Just pass NULL for this subclass.

    virtual void pointCamera(PigFileModel *file);
    virtual void pointCamera(const char *obs_id, char *data_source);

    // point the M01 Pancam via motor counts
    virtual void pointCamera(const int azimuth_motor_counts,
			     const int elevation_motor_counts);

    // This is repeated here only to make C++ compiler happy.
    virtual void pointCamera(const double azimuth, const double elevation,
							PigCoordSystem *cs)
	{ PigPointTwoPivotCamera::pointCamera(azimuth, elevation, cs); }

    virtual const char *const getModelName() { return "M01Pancam"; }

};

#endif
