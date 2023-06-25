////////////////////////////////////////////////////////////////////////
// PigPointGenericCamera_7dof
//
// Pointing model for a generic camera with 7 degrees of freedom:
// quat-s, quat-v1, quat-v2, quat-v3, x, y, z
//
// The "calibration" pointing, as well as initial pointing, is derived from
// the initial camera model.  Calibration pointing could come from a
// point file, but that's probably not useful.  What might be useful, is
// setting the error parameters via the point file.
////////////////////////////////////////////////////////////////////////

#include "PigPointGenericCamera_7dof.h"
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

PigPointGenericCamera_7dof::PigPointGenericCamera_7dof(PigCameraModel *cm,
			PigMission *mission, const char *instrument)
	: PigPointCamera7dof(cm, mission, instrument)
{
    cm->resetCameraLocation();		// get the initial position

    // This is changed in pointCamera(PigFile *)
    _pointing_cs = cm->getCoordSystem();

    // Read the .point file, or set defaults
    // Naming convention for point files: "Generic_inst.point"
    read_point_file("param_files/Generic_inst.point", "Generic");

    // Not sure if this is needed but shouldn't hurt
    _current_location = _calibration_location;
    _current_orientation = _calibration_orientation;

}

////////////////////////////////////////////////////////////////////////
// Destructor
////////////////////////////////////////////////////////////////////////

PigPointGenericCamera_7dof::~PigPointGenericCamera_7dof()
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

void PigPointGenericCamera_7dof::pointCamera(PigFileModel *file)
{
    PigMission *m = PigMission::getMissionObject(file);
    _pointing_cs = m->getCoordSystem(file, NULL);
    _camera_model->setInitialCoordSystemNoTrans(_pointing_cs);

    // Create a new CameraModel object for this file

    PigCameraModel *cm = m->createCameraModel(file, NULL);
    if (cm == NULL)			// do the best we can
	pointCamera(_calibration_location, _calibration_orientation,
			 _pointing_cs);
    else {
	cm->setInitialCoordSystem(_pointing_cs);
	
	PigPoint loc = cm->getCameraPosition();
        PigVector home = PigVector(1,0,0);
        PigVector pointing = cm->getCameraOrientation();
        double ang = acos(home % pointing);	// dot product gives cos(ang)
        PigVector axis = (home * pointing);	// cross prod gives axis
	axis.normalize();
	PigQuaternion quat = PigQuaternion(axis, ang);

	pointCamera(loc, quat, _pointing_cs);

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


void PigPointGenericCamera_7dof::pointCamera(const char *obs_id, char *data_source)
{
    printFatal("PigPointGenericCamera_7dof::pointCamera(obs_id, data_source) not implemented yet!!");
}

////////////////////////////////////////////////////////////////////////
// Read in the generic calibration pointing params, or use a default
//
// For a generic 7dof camera, the default rotation point is the camera position
// (C of CAHV) and the cal quat is derived from the pointing (a rotation
// from an arbitrarily-assumed (1,0,0)).
////////////////////////////////////////////////////////////////////////

void PigPointGenericCamera_7dof::read_point_file(char *filename, const char *host_id)
{
    FILE *inClientFile;
    char line[255];

    // Set defaults

    _calibration_location = _camera_model->getCameraPosition();

    PigVector home = PigVector(1,0,0);
    PigVector pointing = _camera_model->getCameraOrientation();
    double ang = acos(home % pointing);		// dot product gives cos(ang)
    PigVector axis = (home * pointing);		// cross prod gives axis
    axis.normalize();
    _calibration_orientation = PigQuaternion(axis, ang);

    // Error estimates. These are arbitrary (they actually come from MSL MAHLI).

    for (int i=0; i < 7; i++)
	_pointing_error[i] = .001;

    // Now see if a file exists to override

    inClientFile = PigModelBase::openConfigFile(filename, NULL);
    if (inClientFile == NULL) {
	sprintf(line, "Generic point file %s could not be opened, using defaults", filename);
	printWarning(line);
	return;
    }

    while (fgets(line, sizeof(line), inClientFile) != NULL) {
	double dum[4];

        if (strncasecmp(line, "7dof_calibration_position", 25) == 0) {
	    sscanf(line, "7dof_calibration_position = %lf %lf %lf",
		&dum[0], &dum[1], &dum[2]);
	    _calibration_location.setXYZ(dum);
	}
        if (strncasecmp(line, "7dof_calibration_quaternion", 27) == 0) {
	    sscanf(line, "7dof_calibration_quaternion = %lf %lf %lf %lf",
		&dum[0], &dum[1], &dum[2], &dum[3]);
	    _calibration_orientation.setComponents(dum);
	}
        if (strncasecmp(line, "7dof_pointing_error", 19) == 0) {
	    sscanf(line, "7dof_pointing_error = %lf %lf %lf %lf %lf %lf %lf",
		&_pointing_error[0], &_pointing_error[1],
		&_pointing_error[2], &_pointing_error[3],
		&_pointing_error[4], &_pointing_error[5],
		&_pointing_error[6]);
	}
    }
    fclose(inClientFile);

}

