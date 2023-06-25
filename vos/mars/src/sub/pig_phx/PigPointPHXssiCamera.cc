////////////////////////////////////////////////////////////////////////
// PigPointPHXssiCamera
//
// Pointing model for PHX SSI cameras.
//
// Normally pointing is done in the Payload(Lander) frame.  
//
////////////////////////////////////////////////////////////////////////

#include "PigPointPHXssiCamera.h"
#include "PigPHX.h"
#include "PigCameraModel.h"

////////////////////////////////////////////////////////////////////////
// Constructor
////////////////////////////////////////////////////////////////////////

PigPointPHXssiCamera::PigPointPHXssiCamera(PigCameraModel *cm,
					   PigMission *mission, 
					   const char *instrument)
                     : PigPointPanTiltCamera(cm, mission, instrument)
{
    // This is changed in pointCamera(PigFile *)
    if (_camera_model)
	_pointing_cs = _mission->getCoordSystem(
		_camera_model->getCoordSystem(), "Payload");
    else
        _pointing_cs = _mission->getCoordSystem("INSTRUMENT");

    // Read the .point file
    // Naming convention for pointing files: "mission_hostid_ssi.point"
    // where mission = PHX and host_id = FM or EM
    char point_file[255];
    sprintf(point_file, "param_files/PHX_%s_ssi.point",
				((PigPHX *)_mission)->getHostID());

    read_point_info(point_file, ((PigPHX *)_mission)->getHostID());
    _rotation_point = _ssi_az_rotation_point;		// just in case
}

////////////////////////////////////////////////////////////////////////
// Destructor
////////////////////////////////////////////////////////////////////////

PigPointPHXssiCamera::~PigPointPHXssiCamera()
{
	// nothing to do...
}

////////////////////////////////////////////////////////////////////////
// Point a camera model, given an image file.  This does not have to be the
// same image as was given in the constructor, although presumably the
// mission/camera names should match!
////////////////////////////////////////////////////////////////////////

void PigPointPHXssiCamera::pointCamera(PigFileModel *file)
{
    if (pointCameraViaLabel(file))
        return;

    PigFileModelPHX *filePHX = (PigFileModelPHX*) file;
    _pointing_cs = _mission->getCoordSystem(file, NULL);

    _camera_model->setInitialCoordSystemNoTrans(_pointing_cs);

    // Az/El values are stored in radians in a label

    
    if (!filePHX->checkAzimuth())
	printWarning("Warning: noSSI Azimuth value found, 0 assumed");
    double azimuth = filePHX->getAzimuth(0.0);

    if (!filePHX->checkElevation())
	printWarning("Warning: no SSI Elevation value found, 0 assumed");
    double elevation = filePHX->getElevation(0.0);
    
    pointCamera(PigRad2Deg(azimuth), PigRad2Deg(elevation), _pointing_cs);
   
}

////////////////////////////////////////////////////////////////////////
// Do the actual work of pointing the camera.
//
// Az and el are in degrees.  They are mesured in the given coordinate
// system (if NULL, _pointing_cs, the natural instrument frame, is used)
//
// There is only one kind of az/el value we care about:  Backlash-adjusted
// angles.  These are the first two items in the ARTICULATION_DEVICE_ANGLE
// label.  We do NOT use either the pre-backlash values, or the motor counts,
// in this mission.  Furthermore we know nothing about backlash at all; that
// is taken care of by telemproc.  We just get angles.
//
// IMPORTANT NOTE:  There is an implicit assumption here that the elevation
// direction is -1 and the az dir is 1 in the coordinate system used.
// Changing this will probably break some stuff.  Fortunately it's not
// really relevant for Phoenix.
//
// The az_axis points up, which is toward -Z.  The Right-Hand-Rule then
// means that positive azimuth rotations should be CCW.  However, azimuth
// increases CW by convention.  Therefore we change the sign of all azimuth
// rotations about az_axis in here.
//
// The azimuth at which the elevation circle (and thus the elevation axis)
// was derived is about 180, or down -X.  Since the axis points to +Y, the
// RHR tells us that positive rotations around that axis move us down.  Since
// +elevation is up by convention, we *also* have to change the sign of the
// elevation rotations about el_axis.
////////////////////////////////////////////////////////////////////////

void PigPointPHXssiCamera::pointCamera(const double az,
		          	       const double el,
				       PigCoordSystem *cs )
{
    static PigQuaternion identity;		// no rotation
    double elevation = el;
    double azimuth = az;

    if (cs != NULL)				// convert
	    _pointing_cs->convertAzElDegrees(az, el, azimuth, elevation, cs);

    _azimuth = azimuth;
    _elevation = elevation;

    // See the ssi.point file for more on how to rotate the camera

    // 1) Start with the cal model

    _camera_model->resetCameraLocation();

    // 2) Rotate in azimuth to where the elevation axis was measured

    double cal_az_rot = - PigDeg2Rad(_ssi_elev_axis_az - _ssi_calibration_az);
    PigQuaternion cal_az_q(_ssi_az_axis, cal_az_rot);

    _camera_model->moveCamera(_ssi_az_rotation_point, identity,
			      _ssi_az_rotation_point, cal_az_q, _pointing_cs);

    // 3) Rotate in elevation to where the image is

    double el_rot = - PigDeg2Rad(elevation - _ssi_calibration_el);
    PigQuaternion el_q(_ssi_el_axis, el_rot);

    _camera_model->moveCamera(_ssi_el_rotation_point, identity,
			      _ssi_el_rotation_point, el_q, _pointing_cs);

    // 4) Rotate in azimuth from el-axis position to where the image is

    double az_rot = - PigDeg2Rad(azimuth - _ssi_elev_axis_az);
    PigQuaternion az_q(_ssi_az_axis, az_rot);

    _camera_model->moveCamera(_ssi_az_rotation_point, identity,
			      _ssi_az_rotation_point, az_q, _pointing_cs);

    // Done!

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

void PigPointPHXssiCamera::pointCamera(const char *obs_id, 
					    char *data_source)
{
    printFatal("PigPointPHXssiCamera::pointCamera(obs_id, data_source) not implemented yet!!");
}

////////////////////////////////////////////////////////////////////////
// Read in the calibration pointing parameters for a given "point" file.
////////////////////////////////////////////////////////////////////////

void PigPointPHXssiCamera::read_point_info(char *filename, const char *host_id)
{
    FILE *inClientFile;
    char line[255];

    // open the file
    
    inClientFile = PigModelBase::openConfigFile(filename, NULL);


    // Default pointing values, in case the .point file isn't found
    // or the necessary values are not in the file.
    _ssi_calibration_az = 88.783;
    _ssi_elev_axis_az = -179.257;
    _ssi_az_axis.setXYZ(0.00073661,0.00095163,-0.99999928);
    _ssi_az_rotation_point.setXYZ(-0.44033960,0.11429840,-0.78133300);
    _ssi_calibration_el = -1.19;
    _ssi_el_axis.setXYZ(-0.02714644,0.99963118,0.00076261);
    _ssi_el_rotation_point.setXYZ(-0.44340010,0.22559830,-0.79349190);
    _pointing_error[0] = _pointing_error[1] =0.0675;
    _pointing_error[2] = 0.06;

    if (inClientFile == NULL) {
	    sprintf(line, 
		"Point file %s could not be opened, using default values",
		filename);
		printWarning(line);
    }
    else {
	while (fgets(line, sizeof(line), inClientFile) != NULL) {

	    // pull out the parameters

	    double dum[3];

	    // Pointing parameters

	    if (strncasecmp(line, "ssi_calibration_az", 18) == 0) {
		sscanf(line, "ssi_calibration_az = %lf", &_ssi_calibration_az);
	    }
	    if (strncasecmp(line, "ssi_elev_axis_az", 16) == 0) {
		sscanf(line, "ssi_elev_axis_az = %lf", &_ssi_elev_axis_az);
	    }
	    if (strncasecmp(line, "ssi_az_axis", 11) == 0) {
		sscanf(line, "ssi_az_axis = %lf %lf %lf",
		       &dum[0], &dum[1], &dum[2]);
		_ssi_az_axis.setXYZ(dum);
	    }
	    if (strncasecmp(line, "ssi_az_rotation_point", 21) == 0) {
		sscanf(line, "ssi_az_rotation_point = %lf %lf %lf",
		       &dum[0], &dum[1], &dum[2]);
		_ssi_az_rotation_point.setXYZ(dum);
	    }
	    if (strncasecmp(line, "ssi_calibration_el", 18) == 0) {
		sscanf(line, "ssi_calibration_el = %lf", &_ssi_calibration_el);
	    }
	    if (strncasecmp(line, "ssi_el_axis", 11) == 0) {
		sscanf(line, "ssi_el_axis = %lf %lf %lf",
		       &dum[0], &dum[1], &dum[2]);
		_ssi_el_axis.setXYZ(dum);
	    }
	    if (strncasecmp(line, "ssi_el_rotation_point", 21) == 0) {
		sscanf(line, "ssi_el_rotation_point = %lf %lf %lf",
		       &dum[0], &dum[1], &dum[2]);
		_ssi_el_rotation_point.setXYZ(dum);
	    }

	    // Pointing error
            // We are reading up to 3 values, though only first are used by
            // this class.  The third value(twist) is used by the subclass,
            // but it's read here to avoid code duplication and to simplify
            // the code.

	    if (strncasecmp(line, "ssi_pointing_error", 18) == 0) {
	        sscanf(line, "ssi_pointing_error = %lf %lf %lf",
		       &_pointing_error[0], &_pointing_error[1], &_pointing_error[2]);
	    }

	    // Degrees per motor count, homes, etc. not needed here
	    // (telemproc uses them)

	}
	fclose(inClientFile);
    }
}
