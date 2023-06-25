////////////////////////////////////////////////////////////////////////
// PigPointM20heliCamera
//
// Pointing model for Ingenuity helicopter cameras (nav, RTE).
// Ingenuity cameras system is conceptually considered as a arm camera
// (a virtual long arm from the take off location to the heli position/
// orientation)
//
// Uses a 7-DOF model (quat + pos).
//
// Pointing parameters are derived from the image label.  
////////////////////////////////////////////////////////////////////////
#ifndef PIGPOINTM20HELICAMERA_H
#define PIGPOINTM20HELICAMERA_H

#include "PigPointM20armCamera.h"
#include "PigFileModelM20.h"

class PigPointM20heliCamera : public PigPointM20armCamera {

  protected:


  public:

    PigPointM20heliCamera(PigCameraModel *cm, 
			 PigMission *mission,
                         const char *instrument) : PigPointM20armCamera(cm, mission, instrument){};
    virtual ~PigPointM20heliCamera();

    // Point a camera model, given an image or an observation ID.  This
    // does not have to be the same image as was given in the constructor,
    // although presumably the mission/camera names should match!
    // data_source is included mainly to allow the overloading of these
    // two functions, but is intended to specify where to get the potining
    // data from, e.g. SPICE, database, etc.  Just pass NULL for this subclass.

    virtual void pointCamera(PigFileModel *file);

    virtual void pointCamera(const char *obs_id, char *data_source)
	{ PigPointCamera7dof::pointCamera(obs_id, data_source); }
    virtual void pointCamera(PigPoint &position,
                             PigQuaternion &orientation,
                             PigCoordSystem *cs)
	{ PigPointCamera7dof::pointCamera(position, orientation, cs); }

    virtual const char *const getModelName() { return "M20heliCamera"; }

};

#endif
