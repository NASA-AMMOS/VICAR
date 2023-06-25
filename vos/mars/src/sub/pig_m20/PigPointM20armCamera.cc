////////////////////////////////////////////////////////////////////////
// PigPointM20armCamera
//
// Pointing model for M20 arm-mounted cameras (Watson, ACI, MCC).
//
// Normally pointing is done in the Payload(Lander) frame.  
////////////////////////////////////////////////////////////////////////

#include "PigPointM20armCamera.h"
#include "PigCameraModel.h"
#include "PigM20.h"

////////////////////////////////////////////////////////////////////////
// Constructor
////////////////////////////////////////////////////////////////////////

PigPointM20armCamera::PigPointM20armCamera(PigCameraModel *cm,
					   PigMission *mission, 
					   const char *instrument)
                     : PigPointCamera7dof(cm, mission, instrument)
{
    // This is changed in pointCamera(PigFile *)
    if (_camera_model)
	_pointing_cs = _mission->getCoordSystem(
		_camera_model->getCoordSystem(), "INSTRUMENT");
		// INSTRUMENT == Rover Nav for M20
    else
        _pointing_cs = _mission->getCoordSystem("INSTRUMENT");

    // Read the .point file
    // Naming convention for pointing files: "host_id_arm.point"
    // where host_id = M20, M20SIM, etc.
    char point_file[255];
    sprintf(point_file, "param_files/%s_arm.point",
				((PigM20 *)_mission)->getHostID());

    read_point_info(point_file, ((PigM20 *)_mission)->getHostID());
}

////////////////////////////////////////////////////////////////////////
// Destructor
////////////////////////////////////////////////////////////////////////

PigPointM20armCamera::~PigPointM20armCamera()
{
	// nothing to do...
}

////////////////////////////////////////////////////////////////////////
// Point a camera model, given an image file.  This does not have to be the
// same image as was given in the constructor, although presumably the
// mission/camera names should match!
////////////////////////////////////////////////////////////////////////

void PigPointM20armCamera::pointCamera(PigFileModel *file)
{
    if (pointCameraViaLabel(file))
        return;

    PigFileModelM20 *fileM20 = (PigFileModelM20*) file;
    _pointing_cs = _mission->getCoordSystem(file, NULL);

    _camera_model->setInitialCoordSystemNoTrans(_pointing_cs);

    PigPoint location = fileM20->getArticulationDevLocation(
						_calibration_location);
    PigQuaternion orientation = fileM20->getArticulationDevOrient(
						_calibration_orientation);
    pointCamera(location, orientation, _pointing_cs);


}


////////////////////////////////////////////////////////////////////////
// Read in the calibration pointing parameters for a given "point" file.
////////////////////////////////////////////////////////////////////////

void PigPointM20armCamera::read_point_info(char *filename, const char *host_id)
{
    FILE *inClientFile;
    char line[255];

    // Get the camera SN to look for camera-specific cal pose

    char *sn = NULL;
    PigCameraMapper *map = new PigCameraMapper(NULL, _mission->getHostID());
    if (map != NULL && _instrument != NULL) {
        PigCameraMapEntry *entry = map->findFromID(_instrument);
        if (entry != NULL) {
            sn = entry->getSerialNumber();
        }
    }

    // open the file
    
    inClientFile = PigModelBase::openConfigFile(filename, NULL);

    // Default pointing values, in case the .point file isn't found
    // or the necessary values are not in the file.

    // These are from PIXL FLIGHT values current as of 2020-11-10

    _calibration_location.setXYZ(-0.094289625, 0.073548180, 0.031097020);
    double v[4] = {0.583845, 0.397640, -0.395486, 0.587025};
    _calibration_orientation.setComponents(v);
    _pointing_error[0] = _pointing_error[1] = _pointing_error[2] =
    _pointing_error[3] = _pointing_error[4] = _pointing_error[5]  =
    _pointing_error[6] = 0.001;	

    PigPoint sn_cal_pos;
    PigQuaternion sn_cal_quat;
    int found_sn_cal_pos = FALSE;
    int found_sn_cal_quat = FALSE;

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
            char dum_sn[100];

            // Pointing parameters

            if (strncasecmp(line, "arm_cal_position_SN_", 20) == 0) {
                sscanf(line, "arm_cal_position_SN_%s = %lf %lf %lf",
                       dum_sn, &dum[0], &dum[1], &dum[2]);
                if (sn != NULL && (strcmp(sn, dum_sn) == 0)) {
                    sn_cal_pos.setXYZ(dum);
                    found_sn_cal_pos = TRUE;
                }
            }

            if (strncasecmp(line, "arm_cal_quaternion_SN_", 22) == 0) {
                sscanf(line, "arm_cal_quaternion_SN_%s = %lf %lf %lf %lf",
                       dum_sn, &dum[0], &dum[1], &dum[2], &dum[3]);
                if (sn != NULL && (strcmp(sn, dum_sn) == 0)) {
                    sn_cal_quat.setComponents(dum);
                    found_sn_cal_quat = TRUE;
                }
            }

	    if (strncasecmp(line, "arm_calibration_position", 24) == 0) {
		sscanf(line, "arm_calibration_position = %lf %lf %lf",
		       &dum[0], &dum[1], &dum[2]);
		_calibration_location.setXYZ(dum);
	    }
	    if (strncasecmp(line, "arm_calibration_quaternion", 26) == 0) {
		sscanf(line, "arm_calibration_quaternion = %lf %lf %lf %lf",
		       &dum[0], &dum[1], &dum[2], &dum[3]);
		_calibration_orientation.setComponents(dum);
	    }
            if (strncasecmp(line, "arm_pointing_error_7dof", 25) == 0) {
                sscanf(line, "arm_pointing_error = %lf %lf %lf %lf %lf %lf %lf",
                       &_pointing_error[0], &_pointing_error[1],
                       &_pointing_error[2], &_pointing_error[3],
                       &_pointing_error[4], &_pointing_error[5],
		       &_pointing_error[6]);
            }

	}
	fclose(inClientFile);
    }

    if (found_sn_cal_pos)
        _calibration_location = sn_cal_pos;
    if (found_sn_cal_quat)
        _calibration_orientation = sn_cal_quat;

}
