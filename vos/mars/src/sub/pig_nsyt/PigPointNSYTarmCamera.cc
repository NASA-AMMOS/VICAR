////////////////////////////////////////////////////////////////////////
// PigPointNSYTarmCamera
//
// Pointing model for NSYT arm (IDC) camera.
//
// Normally pointing is done in the Lander frame.  
////////////////////////////////////////////////////////////////////////

#include "PigPointNSYTarmCamera.h"
#include "PigCameraModel.h"
#include "PigNSYT.h"

////////////////////////////////////////////////////////////////////////
// Constructor
////////////////////////////////////////////////////////////////////////

PigPointNSYTarmCamera::PigPointNSYTarmCamera(PigCameraModel *cm,
					   PigMission *mission, 
					   const char *instrument)
                     : PigPointCamera7dof(cm, mission, instrument)
{
    // This is changed in pointCamera(PigFile *)
    if (_camera_model)
	_pointing_cs = _mission->getCoordSystem(
		_camera_model->getCoordSystem(), "INSTRUMENT");
		// INSTRUMENT == Lander for NSYT
    else
        _pointing_cs = _mission->getCoordSystem("INSTRUMENT");

    // Read the .point file
    // Naming convention for pointing files: "host_id_idc.point"
    // where host_id = NSYT, NSYTTB, etc.
    char point_file[255];
    sprintf(point_file, "param_files/%s_idc.point",
				((PigNSYT *)_mission)->getHostID());

    read_point_info(point_file, ((PigNSYT *)_mission)->getHostID());
}

////////////////////////////////////////////////////////////////////////
// Destructor
////////////////////////////////////////////////////////////////////////

PigPointNSYTarmCamera::~PigPointNSYTarmCamera()
{
	// nothing to do...
}

////////////////////////////////////////////////////////////////////////
// Point a camera model, given an image file.  This does not have to be the
// same image as was given in the constructor, although presumably the
// mission/camera names should match!
////////////////////////////////////////////////////////////////////////

void PigPointNSYTarmCamera::pointCamera(PigFileModel *file)
{
    if (pointCameraViaLabel(file))
        return;

    PigFileModelNSYT *fileNSYT = (PigFileModelNSYT*) file;
    _pointing_cs = _mission->getCoordSystem(file, NULL);

    _camera_model->setInitialCoordSystemNoTrans(_pointing_cs);

    PigPoint location = fileNSYT->getArticulationDevLocation(_calibration_location);
    PigQuaternion orientation = fileNSYT->getArticulationDevOrient(_calibration_orientation);

    pointCamera(location, orientation, _pointing_cs);


}


////////////////////////////////////////////////////////////////////////
// Read in the calibration pointing parameters for a given "point" file.
////////////////////////////////////////////////////////////////////////

void PigPointNSYTarmCamera::read_point_info(char *filename, const char *host_id)
{
    FILE *inClientFile;
    char line[255];

    // open the file
    
    inClientFile = PigModelBase::openConfigFile(filename, NULL);

    // Default pointing values, in case the .point file isn't found
    // or the necessary values are not in the file.
//!!!! THESE VALUES ARE WRONG AND REPRESENT PHX KINEMATICS!!!!
//!!!! Keeping them as the "wrong" values for now to make sure
//!!!! we are properly reading the config files... the difference is
//!!!! obvious.  Fix later, preferably with flight defaults.
    _calibration_location.setXYZ(-0.047839, -0.151562, -0.412642);
    double v[4] = {0.859965, -0.018021, 0.509126, 0.030439};
    _calibration_orientation.setComponents(v);
    _pointing_error[0] = _pointing_error[1] = _pointing_error[2] =
    _pointing_error[3] = _pointing_error[4] = _pointing_error[5]  =
    _pointing_error[6] = 0.001;	


    if (inClientFile == NULL) {
	    sprintf(line, 
		"Point file %s could not be opened, using default values",
		filename);
		printWarning(line);
    }
    else {
	while (fgets(line, sizeof(line), inClientFile) != NULL) {

	    // pull out the parameters

	    double dum[4];
            // Pointing parameters

	    if (strncasecmp(line, "idc_calibration_position", 24) == 0) {
		sscanf(line, "idc_calibration_position = %lf %lf %lf",
		       &dum[0], &dum[1], &dum[2]);
		_calibration_location.setXYZ(dum);
	    }
	    if (strncasecmp(line, "idc_calibration_quaternion", 26) == 0) {
		sscanf(line, "idc_calibration_quaternion = %lf %lf %lf %lf",
		       &dum[0], &dum[1], &dum[2], &dum[3]);
		_calibration_orientation.setComponents(dum);
	    }
            if (strncasecmp(line, "idc_pointing_error_7dof", 23) == 0) {
                sscanf(line, "idc_pointing_error = %lf %lf %lf %lf %lf %lf %lf",
                       &_pointing_error[0], &_pointing_error[1],
                       &_pointing_error[2], &_pointing_error[3],
                       &_pointing_error[4], &_pointing_error[5],
		       &_pointing_error[6]);
            }

	}
	fclose(inClientFile);
    }
}
