////////////////////////////////////////////////////////////////////////
// PigPointMpfImp
//
// Pointing model for Mars Pathfinder IMP camera.
//
// Pointing parameters are derived from the image label.  Overrides can
// occur by passing in the az/el directly via routines specific to this
// subclass (by mission-specific code only of course).
////////////////////////////////////////////////////////////////////////

#include "PigPointMpfImp.h"

#include "PigFileModel.h"
#include "PigCameraModel.h"
#include "PigCoordSystem.h"

#include <iostream>
using namespace std;
#include <string.h>

////////////////////////////////////////////////////////////////////////
// Constructor
////////////////////////////////////////////////////////////////////////

PigPointMpfImp::PigPointMpfImp(PigCameraModel *cm,
			PigMission *mission, const char *instrument)
	: PigPointPanTiltCamera(cm, mission, instrument)
{
    _deployed_override = FALSE;
    _deployed = TRUE;

    // Error estimates.  Could be read from file (?)

    _pointing_error[0] = .28;		// azimuth
    _pointing_error[1] = .28;		// elevation

    // This is changed in pointCamera(PigFile *)
    if (_camera_model)
	_pointing_cs = _mission->getCoordSystem(
		_camera_model->getCoordSystem(), "INSTRUMENT");
    else
        _pointing_cs = _mission->getCoordSystem("INSTRUMENT");
}

////////////////////////////////////////////////////////////////////////
// Destructor
////////////////////////////////////////////////////////////////////////

PigPointMpfImp::~PigPointMpfImp()
{
	// nothing to do...
}

////////////////////////////////////////////////////////////////////////
// Point a camera model, given an image.  This does not have to be the
// same image as was given in the constructor, although presumably the
// mission/camera names should match!
////////////////////////////////////////////////////////////////////////

void PigPointMpfImp::pointCamera(PigFileModel *file)
{
    char msg[256];

    // Reset the coord system to match this image

    _pointing_cs = _mission->getCoordSystem(file, NULL);
    _camera_model->setInitialCoordSystemNoTrans(_pointing_cs);

    if (!_deployed_override) {
	const char *deploy_str = file->getInstrumentDeploymentState();
	if (deploy_str && strcasecmp(deploy_str, "STOWED") == 0)
	    _deployed = FALSE;
	else if (deploy_str && strcasecmp(deploy_str, "DEPLOYED") == 0)
	    _deployed = TRUE;
	else {
	    sprintf(msg, "Unrecognized deploy state: %s.  Deployed assumed",
			deploy_str ? deploy_str : "NULL");
	    printWarning(msg);
	    _deployed = TRUE;
	}
    }

    pointCamera(file->getLanderInstrumentAzimuth(0.0),
		file->getLanderInstrumentElevation(0.0), _pointing_cs);

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

void PigPointMpfImp::pointCamera(const char *obs_id, char *data_source)
{
    printFatal("PigPointMpfImp::pointCamera(obs_id, data_source) not implemented yet!!");
}

////////////////////////////////////////////////////////////////////////
// Do the actual work of pointing the camera.
//
// This override merely determines the proper parameters, then calls the
// base class to do the work.
//
// Az and el are in degrees, measured in the given cs.  Everything else
// internally uses the natural frame (Lander, for MPF).
////////////////////////////////////////////////////////////////////////

void PigPointMpfImp::pointCamera(const double azimuth,
				 const double elevation, PigCoordSystem *cs)
{

// These pointing constants could be in a file somewhere...

// IMP coordinate origin in Lander frame (not used)
    static PigPoint imp0_lan(-0.1984, -0.009, -0.4367);

// Stowed camera rotation point in lander coords
    static PigPoint rot_pt_stowed(-0.1984, -0.009, -.6122);

// Deployed camera rotation point in lander coords
    static PigPoint rot_pt_deployed(-0.1984, -0.009, -1.23045);

    if (_camera_model == NULL) {
	printFatal("Can't point a NULL camera in PigPointMpfImp!");
	return;
    }
    if (strcasecmp(_camera_model->getCameraVersion(), "engineering") == 0) {
	_calibration_azimuth = 0.0;
	_calibration_elevation = 0.0;
    }
    else {
	_calibration_azimuth = 20.146 + 90.0;
	_calibration_elevation = 1.107;
    }

    if (_deployed)
	_rotation_point = rot_pt_deployed;
    else
	_rotation_point = rot_pt_stowed;

    // The cal frame was taken in stowed position
    _calibration_rotation_point = rot_pt_stowed;

    PigPointPanTiltCamera::pointCamera(azimuth, elevation, cs);
}

////////////////////////////////////////////////////////////////////////
// These are specific to MPF IMP.  The set... functions override any
// value the label might have for all future pointings.  The clear...
// functions clear this override, so the label will be used again.
// The get functions return the override, or the value from the last
// pointing if there is no override.
////////////////////////////////////////////////////////////////////////

void PigPointMpfImp::setDeployedState(const int state)
{
    _deployed = state;
    _deployed_override = TRUE;
}
void PigPointMpfImp::clearDeployedState()
{
    _deployed_override = FALSE;
}

