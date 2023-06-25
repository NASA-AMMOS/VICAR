////////////////////////////////////////////////////////////////////////
// PigPointGenericCamera_6dof
//
// Pointing model for a generic camera with 6 degrees of freedom:
// az, el, twist, x, y, z
//
// The "calibration" pointing, as well as initial pointing, is derived from
// the initial camera model.  Calibration pointing could come from a
// point file, but that's probably not useful.  What might be useful, is
// setting the error parameters via the point file.
////////////////////////////////////////////////////////////////////////

#include "PigPointGenericCamera_6dof.h"
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

PigPointGenericCamera_6dof::PigPointGenericCamera_6dof(PigCameraModel *cm,
			PigMission *mission, const char *instrument)
	: PigPointCamera6dof(cm, mission, instrument)
{
    cm->resetCameraLocation();		// get the initial position

    // This is changed in pointCamera(PigFile *)
    _pointing_cs = cm->getCoordSystem();

    // Set defaults

    _calibration_location = _camera_model->getCameraPosition();

    PigVector pointing = _camera_model->getCameraOrientation();
    double az = _pointing_cs->getAz(pointing);
    double el = _pointing_cs->getEl(pointing);
    double twist = 0.0;

    _calibration_orientation = PigQuaternion(1.0, 0, 0, 0);
    _calibration_orientation.setEulerAngles(twist, el, az);

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

PigPointGenericCamera_6dof::~PigPointGenericCamera_6dof()
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

void PigPointGenericCamera_6dof::pointCamera(PigFileModel *file)
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

        PigVector pointing = cm->getCameraOrientation();
        double az = PigRad2Deg(_pointing_cs->getAz(pointing));
        double el = PigRad2Deg(_pointing_cs->getEl(pointing));
        double twist = 0.0;	//!!!! get from angle V makes with vert?

	PigPointCamera6dof::pointCamera(az, el, twist, loc.getX(), loc.getY(), loc.getZ(), _pointing_cs);

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


void PigPointGenericCamera_6dof::pointCamera(const char *obs_id, char *data_source)
{
    printFatal("PigPointGenericCamera_6dof::pointCamera(obs_id, data_source) not implemented yet!!");
}


////////////////////////////////////////////////////////////////////////
// Read in the generic calibrartion pointing params, or use a default
//
// For a generic 6dof camera, the default rotation point is the camera position
// (C of CAHV) and the cal quat is derived from the pointing (a rotation
// from an arbitrarily-assumed (1,0,0)).
////////////////////////////////////////////////////////////////////////

void PigPointGenericCamera_6dof::read_point_file(char *filename, const char *host_id)
{
    FILE *inClientFile;
    char line[255];

    // Error estimates. These are arbitrary (they actually come from MSL MAHLI).

    for (int i=0; i < 3; i++)
	_pointing_error[i] = 0.1;
    for (int i=3; i < 5; i++)
	_pointing_error[i] = 0.5;
    _pointing_error[5] = 0.1;

    // Now see if a file exists to override

    inClientFile = PigModelBase::openConfigFile(filename, NULL);
    if (inClientFile == NULL) {
	sprintf(line, "Generic point file %s could not be opened, using defaults", filename);
	printWarning(line);
	return;
    }

    while (fgets(line, sizeof(line), inClientFile) != NULL) {
	double dum[4];

	if (strncasecmp(line, "6dof_calibration_position", 25) == 0) {
	    sscanf(line, "6dof_calibration_position = %lf %lf %lf",
		&dum[0], &dum[1], &dum[2]);
	    _calibration_location.setXYZ(dum);
	}

	if (strncasecmp(line, "6dof_calibration_quaternion", 25) == 0) {
	    sscanf(line, "6dof_calibration_quaternion = %lf %lf %lf %lf",
		&dum[0], &dum[1], &dum[2], &dum[3]);
	    _calibration_orientation.setComponents(dum);
	}


	if (strncasecmp(line, "6dof_pointing_error", 19) == 0) {
	    sscanf(line, "6dof_pointing_error = %lf %lf %lf %lf %lf %lf",
		&_pointing_error[0], &_pointing_error[1],
		&_pointing_error[2], &_pointing_error[3],
		&_pointing_error[4], &_pointing_error[5]);
	}
    }
    fclose(inClientFile);
}

