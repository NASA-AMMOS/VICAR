////////////////////////////////////////////////////////////////////////
// PigPointCamera7dof
//
// Pointing model for generic camera using 7 degrees of freedom
//
// Pointing parameters are derived from the image label.  Overrides can
// occur by passing in the az/el directly via routines specific to this
// subclass (by mission-specific code only of course).
////////////////////////////////////////////////////////////////////////
#ifndef PIGPOINTCAMERA7DOF_H
#define PIGPOINTCAMERA7DOF_H

#include "PigPointingModel.h"
#include "PigVector.h"
#include "PigQuaternion.h"
#include "PigCoordSystem.h"
#include "PigFileModel.h"

class PigPointCamera7dof : public PigPointingModel {

  protected:

    PigPoint _current_location;
    PigQuaternion _current_orientation;

    // These parameters must be filled in by the subclass before calling
    // pointCamera() - either in constructor, or in override of pC().

    PigPoint _calibration_location;
    PigQuaternion _calibration_orientation;
    double _pointing_error[7];

    // Read in the calibration pointing parameters for a given pointing file

    void read_point_info(char *filename, const char *host_id);

  public:

    PigPointCamera7dof(PigCameraModel *cm, 
	       	       PigMission *mission,
	 	       const char *instrument);
    virtual ~PigPointCamera7dof();

    // Point a camera model, given an image or an observation ID.  This
    // does not have to be the same image as was given in the constructor,
    // although presumably the mission/camera names should match!
    // data_source is included mainly to allow the overloading of these
    // two functions, but is intended to specify where to get the potining
    // data from, e.g. SPICE, database, etc.  Just pass NULL for this subclass.

    virtual void pointCamera(PigFileModel *file) = 0;
    virtual void pointCamera(const char *obs_id, char *data_source);

    // Point the camera using position and orientation from the label
    virtual void pointCamera(PigPoint &position,
			     PigQuaternion &orientation,
			     PigCoordSystem *cs);

    // These functions allow access to the "raw" pointing data, in order
    // to accomplish pointing corrections or "tweaks".
    //    params[0] = quaternion s
    //    params[1] = quaternion v1    
    //    params[2] = quaternion v2
    //    params[2] = quaternion v3
    //    params[3] = X position
    //    params[4] = Y position
    //    params[5] = Z position

     virtual void setCameraOrientation(const PigVector &orientation,
                                                        PigCoordSystem *cs);

    // whatever the parameters end up being...
    virtual int getPointingParamCount() { return 7; }
    virtual void getPointingParameters(double params[], const int max_count);
    virtual void setPointingParameters(const double params[], const int count);
    virtual void forceCameraRepoint();
    virtual void getPointingErrorEstimate(double params[], const int max_count);

    virtual const char *const getPointingParamName(int i);      // 0-based
    virtual const char *const getModelName() { return "PointCamera7dof"; }

};

#endif
