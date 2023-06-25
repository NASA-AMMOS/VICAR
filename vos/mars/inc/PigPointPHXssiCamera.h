////////////////////////////////////////////////////////////////////////
// PigPointPHXssi
//
// Pointing model for Phoenix Lander SSI camera.
//
// Pointing parameters are derived from the image label.  Overrides can
// occur by passing in the az/el directly via routines specific to this
// subclass (by mission-specific code only of course).
////////////////////////////////////////////////////////////////////////
#ifndef PIGPOINTPHXSSICAMERA_H
#define PIGPOINTPHXSSICAMERA_H

#include "PigPointPanTiltCamera.h"

#include "PigVector.h"
#include "PigQuaternion.h"
#include "PigCoordSystem.h"
#include "PigFileModelPHX.h"

class PigPointPHXssiCamera : public PigPointPanTiltCamera {

  protected:

    int _deployed;
    int _deployed_override;

    // Parameters from the pointing file...

    double _ssi_calibration_az;
    double _ssi_elev_axis_az;
    PigVector _ssi_az_axis;
    PigPoint _ssi_az_rotation_point;
    double _ssi_calibration_el;
    PigVector _ssi_el_axis;
    PigPoint _ssi_el_rotation_point;

    // Read in the calibration pointing parameters for a given pointing file

    void read_point_info(char *filename, const char *host_id);

  public:

    PigPointPHXssiCamera(PigCameraModel *cm, PigMission *mission,
				       const char *instrument);
    virtual ~PigPointPHXssiCamera();

    // Point a camera model, given an image or an observation ID.  This
    // does not have to be the same image as was given in the constructor,
    // although presumably the mission/camera names should match!
    // data_source is included mainly to allow the overloading of these
    // two functions, but is intended to specify where to get the potining
    // data from, e.g. SPICE, database, etc.  Just pass NULL for this subclass.

    virtual void pointCamera(PigFileModel *file);
    virtual void pointCamera(const char *obs_id, char *data_source);

    // Override of PigPointPanTiltCamera's pointCamera()
    virtual void pointCamera(const double azimuth, const double elevation,
				PigCoordSystem *cs);

    virtual const char *const getModelName() { return "PHXssiCamera"; }

};

#endif
