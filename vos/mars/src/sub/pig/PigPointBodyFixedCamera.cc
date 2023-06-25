////////////////////////////////////////////////////////////////////////
// PigPointBodyFixedCamera
//
// Pointing model for generic  cameras, such as Mars Pathfinder and M01 Rovers.
//
// This class is intended merely as a convenience to hold functions common
// to all body fixed cameras.  Subclasses may override anything in here.
//
// Note that this contains pure virtual functions; as such, a subclass
// *must* be used for each mission type.
////////////////////////////////////////////////////////////////////////

#include "PigPointBodyFixedCamera.h"

#include "PigCameraModel.h"
#include "PigCoordSystem.h"
#include "PigMission.h"

#include <iostream>
using namespace std;

////////////////////////////////////////////////////////////////////////
// Constructor
////////////////////////////////////////////////////////////////////////

PigPointBodyFixedCamera::PigPointBodyFixedCamera(PigCameraModel *cm,
						 PigMission *mission, 
						 const char *instrument)
                        : PigPointingModel(cm, mission, instrument) {

    // This is changed in pointCamera(PigFile *)
    _pointing_cs = cm->getCoordSystem();
}

////////////////////////////////////////////////////////////////////////
// Destructor
////////////////////////////////////////////////////////////////////////

PigPointBodyFixedCamera::~PigPointBodyFixedCamera()
{
	// nothing to do...
}

////////////////////////////////////////////////////////////////////////
// Point a camera model, given an image.  This does not have to be the
// same image as was given in the constructor, although presumably the
// mission/camera names should match!
////////////////////////////////////////////////////////////////////////

void PigPointBodyFixedCamera::pointCamera(PigFileModel *file)
{
    // Reset the coord system to match this image
    _pointing_cs = _mission->getCoordSystem(file, NULL);
    _camera_model->setInitialCoordSystemNoTrans(_pointing_cs);
}

////////////////////////////////////////////////////////////////////////
// Point a camera model, given an image or an observation ID.  This
// does not have to be the same image as was given in the constructor,
// although presumably the mission/camera names should match!
// data_source is included mainly to allow the overloading of these
// two functions, but is intended to specify where to get the potining
// data from, e.g. SPICE, database, etc.  Just pass NULL for this subclass.
////////////////////////////////////////////////////////////////////////

//!!!! NOT IMPLEMENTED YET !!!!   Main problem is where to get info from if
// not label!!!!

void PigPointBodyFixedCamera::pointCamera(const char *obs_id, char *data_source)
{
    printFatal("PigPointMpfRover::pointCamera(obs_id, data_source) not implemented yet!!");
}

////////////////////////////////////////////////////////////////////////
// Do the actual work of pointing the camera.
//
// All we have to do is to transform the camera model to the Fixed frame 
// for this mission.
//
////////////////////////////////////////////////////////////////////////

void PigPointBodyFixedCamera::pointCamera(const PigQuaternion  *qt, 
                                                PigCoordSystem *cs)
{
    if (_camera_model == NULL) {
	printFatal("Can't point a NULL camera in PigPointBodyFixedCamera!");
	return;
    }

}

void PigPointBodyFixedCamera::pointCamera(PigCoordSystem *cs)
{
    if (_camera_model == NULL) {
	printFatal("Can't point a NULL camera in PigPointBodyFixedCamera!");
	return;
    }

}
////////////////////////////////////////////////////////////////////////
// These functions set the camera position, in the given coordinates.
// They are *not* intended for pointing correction.
////////////////////////////////////////////////////////////////////////

void PigPointBodyFixedCamera::setCameraOrientation(const PigVector &orientation,
						   PigCoordSystem *cs)
{
    if (!checkCameraModel())
	return;

    //!!!! Need to figure out what actually should be done here.
    //!!!! Is this the appropriate thing?

    _camera_model->setCameraOrientation(orientation, cs);
}

void PigPointBodyFixedCamera::forceCameraRepoint( )
{
    pointCamera(_pointing_cs);
}
