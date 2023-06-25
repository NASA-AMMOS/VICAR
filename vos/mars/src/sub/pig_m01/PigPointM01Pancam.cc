////////////////////////////////////////////////////////////////////////
// PigPointM01Pancam
//
// Pointing model for M01 Pancam.
//
// Physically-based pointing model, based on az and el motor counts.
////////////////////////////////////////////////////////////////////////

#include "PigPointM01Pancam.h"
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

PigPointM01Pancam::PigPointM01Pancam(PigCameraModel *cm,
			PigMission *mission, const char *instrument)
	: PigPointTwoPivotCamera(cm, mission, instrument)
{
    // This is changed in pointCamera(PigFile *)
    if (_camera_model)
	_pointing_cs = _mission->getCoordSystem(
			_camera_model->getCoordSystem(), "INSTRUMENT");
    else
        _pointing_cs = _mission->getCoordSystem("INSTRUMENT");

    // Read the .point file
    // Note:  same file for left and right cameras

    // Check version (flight/eng) !!!!TBD

    char *pointFile = "point_files/M01_Pancam.point";

    read_point_info(pointFile);

    // Error estimates.  These are arbitrary (they actually come from M98).
    // Could be read from file (?)

    _pointing_error[0] = .28;		// azimuth
    _pointing_error[1] = .28;		// elevation
}

////////////////////////////////////////////////////////////////////////
// Destructor
////////////////////////////////////////////////////////////////////////

PigPointM01Pancam::~PigPointM01Pancam()
{
	// nothing to do...
}

////////////////////////////////////////////////////////////////////////
// Point a camera model, given an image.  This does not have to be the
// same image as was given in the constructor, although presumably the
// mission/camera names should match!
////////////////////////////////////////////////////////////////////////

void PigPointM01Pancam::pointCamera(PigFileModel *file)
{
    _pointing_cs = _mission->getCoordSystem(file, NULL);
    _camera_model->setInitialCoordSystemNoTrans(_pointing_cs);

    if (!file->checkInstrumentAzimuthCount())
	printWarning("Warning: no azimuth count, 0 assumed");
    int az_count = file->getInstrumentAzimuthCount(0);

    if (!file->checkInstrumentElevationCount())
	printWarning("Warning: no elevation count, 0 assumed");
    int el_count = file->getInstrumentElevationCount(0);

    pointCamera(az_count, el_count);

}

////////////////////////////////////////////////////////////////////////
// Point the camera using reported motor counts
////////////////////////////////////////////////////////////////////////

void PigPointM01Pancam::pointCamera(const int azimuth_motor_counts,
				    const int elevation_motor_counts)
{
    // Calculate the camera pointing in degrees

    double az = azimuth_motor_counts * _degrees_per_motor_count_az
		+ _azimuth_offset;

    while (az < 0) az += 360.0;
    while (az >= 360.0) az -= 360.0;

    double el = elevation_motor_counts * _degrees_per_motor_count_el
		+ _elevation_offset;

    pointCamera(az, el, _pointing_cs);
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

void PigPointM01Pancam::pointCamera(const char *obs_id, char *data_source)
{
    printFatal("PigPointM01Pancam::pointCamera(obs_id, data_source) not implemented yet!!");
}

////////////////////////////////////////////////////////////////////////
// Read in the calibration pointing parameters for a given "point" file.
////////////////////////////////////////////////////////////////////////

void PigPointM01Pancam::read_point_info(char *filename)
{
    FILE *f;
    char line[255];

    // open the file

    f = PigModelBase::openConfigFile(filename, NULL);

    if (f == NULL) {
	sprintf(line, "Point file %s could not be opened!  Fatal error!",
		filename);
	printFatal(line);
	return;
    }

    while (fgets(line, sizeof(line), f) != NULL) {

	// pull out the parameters

	double dum[3];

	if (strncasecmp(line, "camera_rotation_point_az", 24) == 0) {
	    sscanf(line, "camera_rotation_point_az = %lf %lf %lf",
			&dum[0], &dum[1], &dum[2]);
	    _rotation_point.setXYZ(dum);
	}
	if (strncasecmp(line, "camera_rotation_point_el", 24) == 0) {
	    sscanf(line, "camera_rotation_point_el = %lf %lf %lf",
			&dum[0], &dum[1], &dum[2]);
	    _rotation_point_el.setXYZ(dum);
	}
	if (strncasecmp(line, "azimuth_offset", 14) == 0) {
	    sscanf(line, "azimuth_offset = %lf",
			&_azimuth_offset);
	}
	if (strncasecmp(line, "elevation_offset", 16) == 0) {
	    sscanf(line, "elevation_offset = %lf",
			&_elevation_offset);
	}
	if (strncasecmp(line, "degrees_per_motor_count_az", 26) == 0) {
	    sscanf(line, "degrees_per_motor_count_az = %lf",
			&_degrees_per_motor_count_az);
	}
	if (strncasecmp(line, "degrees_per_motor_count_el", 26) == 0) {
	    sscanf(line, "degrees_per_motor_count_el = %lf",
			&_degrees_per_motor_count_el);
	}
	if (strncasecmp(line, "cal_clicks_azimuth", 18) == 0) {
	    sscanf(line, "cal_clicks_azimuth = %lf",
			&_cal_clicks_azimuth);
	}
	if (strncasecmp(line, "cal_clicks_elevation", 20) == 0) {
	    sscanf(line, "cal_clicks_elevation = %lf",
			&_cal_clicks_elevation);
	}
    }
    fclose(f);

    // Calculate the calibration azimuth, in degrees
    _calibration_azimuth = _cal_clicks_azimuth * _degrees_per_motor_count_az
			+ _azimuth_offset;

    while (_calibration_azimuth < 0.0)
	_calibration_azimuth += 360.0;
    while (_calibration_azimuth >= 360.0)
	_calibration_azimuth -= 360.0;

    // Calculate the calibration elevation, in degrees
    _calibration_elevation = _cal_clicks_elevation * _degrees_per_motor_count_el
			+ _elevation_offset;

    // Calibration rotation point is the same as the image rotation point
    _calibration_rotation_point = _rotation_point;
    _calibration_rotation_point_el = _rotation_point_el;

}

