////////////////////////////////////////////////////////////////////////
// PigPointingModel
//
// Base class for Pointing models.  This aims a camera model.  This is
// generally highly mission-dependent, as the pointing parameters are
// different for each mission (so each mission should have its own subclass
// created by the create() factory functions).  The pointing model calls
// routines on the camera model to aim it.  Pointing models are associated
// one-to-one with camera models, although the associations can be changed.
//
// Pointing parameters can be derived from many different sources, including
// image label, SPICE, project database, mission file, and parameters.
// Generally, the image itself is sufficient, because an image identifier
// (like SCET or SCLK) can be derived from the label, and used to access
// the databases.  Or the data can be in the label itself.  It is up to each
// projects' subclass to search the various data sources in the manner most
// appropriate to that project.  The generic pointing function therefore
// takes only an image identifier, or a spacecraft/camera/observation ID
// set.  More specific functions with other parameters can be in the
// mission-specific subclasses, for use by mission-specific programs.
//
// TBD:  how to handle params needed by SPICE, direct parameters, etc...
////////////////////////////////////////////////////////////////////////

#include "PigPointingModel.h"

#include "PigCameraModel.h"
#include "PigFileModel.h"
#include "PigMission.h"
#include "PigCoordSystem.h"

#include <stdio.h>
#include <string.h>

////////////////////////////////////////////////////////////////////////
// Constructor
////////////////////////////////////////////////////////////////////////

PigPointingModel::PigPointingModel(PigCameraModel *cm,
				PigMission *mission, const char *instrument)
{
    _mission = mission;
    _instrument = NULL;
    if (instrument)
	_instrument = strdup(instrument);
    _camera_model = cm;
    if (_camera_model)
	_camera_model->setPointingModel(this);
}

////////////////////////////////////////////////////////////////////////
// Destructor
////////////////////////////////////////////////////////////////////////

PigPointingModel::~PigPointingModel()
{
    if (_instrument)
	delete _instrument;

    if (_camera_model)
	_camera_model->setPointingModel(NULL);
}

////////////////////////////////////////////////////////////////////////
// Check to make sure the camera model exists
////////////////////////////////////////////////////////////////////////

int PigPointingModel::checkCameraModel()
{
    if (_camera_model == NULL) {
	printFatal("Camera model not specified!");
	return 0;
    }
    return 1;
}

////////////////////////////////////////////////////////////////////////
// These factory methods create and return an instance of the
// proper subclass for the given mission/camera.  These parameters
// are specified by the file itself (look at the label to figure it
// out), or by the mission name/instrument name.  The actual pointing data
// from the file is not used, so the same PointingModel object can be
// used to point multiple observations (from the same camera, of course).
////////////////////////////////////////////////////////////////////////

PigPointingModel *PigPointingModel::create(PigCameraModel *cm, const char *filename,
                                           const char *type, bool allow_type_override)
{
    PigFileModel *file = PigFileModel::create(filename);
    if (file == NULL)
	return NULL;
    PigPointingModel *point = create(cm, file, type, allow_type_override);
    delete file;
    return point;
}

PigPointingModel *PigPointingModel::create(PigCameraModel *cm, PigFileModel *file,
                                           const char *type, bool allow_type_override)
{
    PigMission *m = PigMission::getMissionObject(file);
    return m->createPointingModel(cm, file, type, allow_type_override);
}

PigPointingModel *PigPointingModel::create(PigCameraModel *cm, const char *mission, 
                                           const char *instrument, const char *type,
                                           bool allow_type_override)
{
    PigMission *m = PigMission::getMissionObject(mission);
    return m->createPointingModel(cm, instrument, type, allow_type_override);
}

////////////////////////////////////////////////////////////////////////
// Set the camera model.  Usually done by the constructor.
// Subclasses may override this in order to check the camera model for 
// a subclass they can handle... but if so, they need to call their own
// version of this in their constructor, due to problems with calling
// virtual functions from constructors.
////////////////////////////////////////////////////////////////////////

void PigPointingModel::setCameraModel(PigCameraModel *cm)
{
    if (cm == _camera_model)
	return;					// already set, avoid recursion
    if (_camera_model != NULL) {
	PigCameraModel *cm = _camera_model;
	_camera_model = NULL;
	cm->setPointingModel(NULL);
    }
    _camera_model = cm;
    if (_camera_model != NULL)
	_camera_model->setPointingModel(this);
}

////////////////////////////////////////////////////////////////////////
// Point the camera, given a filename.  We simply create a PigFileModel and
// call the other pointCamera routine.
////////////////////////////////////////////////////////////////////////

void PigPointingModel::pointCamera(const char *filename)
{
    const short int MAX_MSG_SIZE=512;
    char msg[MAX_MSG_SIZE];
    PigFileModel *file = PigFileModel::create(filename);
    if (file == NULL) {
        snprintf(msg, MAX_MSG_SIZE, "Could not open file \"%s\"", filename);
        printWarning(msg);
        return;
    }

    pointCamera(file);
    delete file;
}
//////////////////////////////////////////////////////////////////////////
// Point the camera using Pointing Model described in the Label.  Returns
// false if none was found.
//////////////////////////////////////////////////////////////////////////

bool PigPointingModel::pointCameraViaLabel(PigFileModel *file)
{
    char point_method[256];
    char *point_method_value = NULL;
    char  *value = NULL;
    int count;

    getParam("POINT_METHOD", point_method, &count, 1, 0);

    if (count != 0) {
        value = parseParamString(point_method, "NO_LBL_POINT");

        // command flag is saying don't do this
        if (value != NULL)
            return false;
    }

    const char *pointingModelName = file->getPointingModelName();

    // check if pm model is defined in the label and if it is 
    // defined wether it matches this instance
    if (pointingModelName == NULL || 
        strcasecmp(pointingModelName, getModelName()))
        return false;
    // get PointingParams from the label
    double pm_params[PIG_MAX_PARAMS];
    int n = PIG_MAX_PARAMS;
    file->getPointingModelParams(pm_params, n);
    this->setPointingParameters(pm_params, n);

    return true;

}
////////////////////////////////////////////////////////////////////////
// These functions get and set the pointing and camera position, in
// the given coordinates.  They are *not* intended for pointing
// correction, use the get/setPointingParameters() functions for that.
//
// The coordinates and angles returned are in the supplied coordinate
// frame.  For the set functions, they are expressed in the supplied frame.
// Also, the orientation could be specified via a quaternion, but
// given the work involved to pack and unpack it, it is instead returned
// as a look direction unit vector, and a twist rotation angle about
// that look direction (defined using the right-hand rule, in radians).
//
// Subclasses should feel free to override these if needed, but usually
// this base class implementation should suffice... except for
// setOrientation().
////////////////////////////////////////////////////////////////////////

PigPoint PigPointingModel::getCameraPosition(PigCoordSystem *cs)
{
    if (!checkCameraModel())
	return PigPoint(0.0, 0.0, 0.0);

    return cs->convertPoint(_camera_model->getCameraPosition(),
			    _camera_model->getCoordSystem());
}

PigVector PigPointingModel::getCameraOrientation(PigCoordSystem *cs)
{
    if (!checkCameraModel())
	return PigVector(1.0, 0.0, 0.0);

    return cs->convertVector(_camera_model->getCameraOrientation(),
			     _camera_model->getCoordSystem());
}

double PigPointingModel::getCameraTwist()
{
    if (!checkCameraModel())
	return 0.0;

    return _camera_model->getCameraTwist();
}


void PigPointingModel::setCameraPosition(const PigPoint &position,
					      PigCoordSystem *cs)
{
    if (!checkCameraModel())
	return;

    _camera_model->setCameraPosition(position, cs);
}

void PigPointingModel::setCameraTwist(const double twist)
{
    if (!checkCameraModel())
	return;

    _camera_model->setCameraTwist(twist);
}

