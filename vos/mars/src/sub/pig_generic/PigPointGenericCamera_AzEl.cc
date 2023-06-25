////////////////////////////////////////////////////////////////////////
// PigPointGenericCamera_AzEl
//
// Pointing model for a generic camera with 2 degrees of freedom:  Azimuth
// and Elevation.
//
// The "calibration" pointing, as well as initial pointing, is derived from
// the initial camera model.
////////////////////////////////////////////////////////////////////////

#include "PigPointGenericCamera_AzEl.h"
#include "PigFileModel.h"
#include "PigCameraModel.h"
#include "PigCoordSystem.h"
#include "PigMission.h"

#include <iostream>
using namespace std;
#include <string.h>
#include <stdio.h>

////////////////////////////////////////////////////////////////////////
// Constructor
////////////////////////////////////////////////////////////////////////

PigPointGenericCamera_AzEl::PigPointGenericCamera_AzEl(PigCameraModel *cm,
			PigMission *mission, const char *instrument)
	: PigPointPanTiltCamera(cm, mission, instrument)
{
    cm->resetCameraLocation();		// get the initial position

    // This is changed in pointCamera(PigFile *)
    _pointing_cs = cm->getCoordSystem();

    // For a generic camera, the rotation point is the camera position
    // (C of CAHV) and the "calibration" location is the initial camera
    // pointing.

    _rotation_point = cm->getCameraPosition();
    _calibration_rotation_point = _rotation_point;

    PigVector pointing = cm->getCameraOrientation();
    _calibration_azimuth = PigRad2Deg(_pointing_cs->getAz(pointing));
    _calibration_elevation = PigRad2Deg(_pointing_cs->getEl(pointing));

    _azimuth = _calibration_azimuth;
    _elevation = _calibration_elevation;

    // Error estimates.  These are arbitrary (they actually come from M98).

    _pointing_error[0] = .28;		// azimuth
    _pointing_error[1] = .28;		// elevation
}

////////////////////////////////////////////////////////////////////////
// Destructor
////////////////////////////////////////////////////////////////////////

PigPointGenericCamera_AzEl::~PigPointGenericCamera_AzEl()
{
	// nothing to do...
}

////////////////////////////////////////////////////////////////////////
// Point a camera model, given an image.  This does not have to be the
// same image as was given in the constructor, although presumably the
// mission/camera names should match!
//
// For generic cameras, the only way we have to get pointing is to look at
// the CameraModel associated with the file.  The simplest thing to do here
// is simply reset the given camera model to initial conditions.  However,
// in order to support (at least in a rudimentary way) pointing with different
// images than the object was created with (according to the function's
// contract), we have to create a new CM based on this file, extract its
// pointing, then point the camera based on that.  Messy, but this should
// not be called in an inner loop.
////////////////////////////////////////////////////////////////////////

void PigPointGenericCamera_AzEl::pointCamera(PigFileModel *file)
{
    _pointing_cs = _mission->getCoordSystem(file, NULL);
    _camera_model->setInitialCoordSystemNoTrans(_pointing_cs);

    // Create a new CameraModel object for this file

    PigCameraModel *cm = PigMission::getMissionObject(file)->
						createCameraModel(file, NULL);
    if (cm == NULL)			// do the best we can
	pointCamera(_calibration_azimuth, _calibration_elevation, _pointing_cs);
    else {
	cm->setInitialCoordSystem(_pointing_cs);
	_rotation_point = cm->getCameraPosition();	// this is cheating
	PigVector pointing = cm->getCameraOrientation();
	double az = PigRad2Deg(_pointing_cs->getAz(pointing));
	double el = PigRad2Deg(_pointing_cs->getEl(pointing));

	pointCamera(az, el, _pointing_cs);

	delete cm;
    }

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

void PigPointGenericCamera_AzEl::pointCamera(const char *obs_id, char *data_source)
{
    printFatal("PigPointGenericCamera_AzEl::pointCamera(obs_id, data_source) not implemented yet!!");
}

