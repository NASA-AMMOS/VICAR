////////////////////////////////////////////////////////////////////////
// PigPointNSYTarmCamera6dof
//
// Pointing model for NSYT arm (IDC) camera.
//
// Normally pointing is done in the Lander frame.  
////////////////////////////////////////////////////////////////////////

#include "PigNSYT.h"
#include "PigCameraModel.h"
#include "PigPointCamera6dof.h"
#include "PigPointNSYTarmCamera6dof.h"

////////////////////////////////////////////////////////////////////////
// Constructor
////////////////////////////////////////////////////////////////////////

PigPointNSYTarmCamera6dof::PigPointNSYTarmCamera6dof(PigCameraModel *cm,
					       PigMission *mission, 
					       const char *instrument)
                              : PigPointCamera6dof(cm, mission, instrument)
{
    // Read the .point file only for this subclass specific info
    // the rest is done in super class
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

PigPointNSYTarmCamera6dof::~PigPointNSYTarmCamera6dof()
{
	// nothing to do...
}
////////////////////////////////////////////////////////////////////////
// Point a camera model, given an image file.  This does not have to be the
// same image as was given in the constructor, although presumably the
// mission/camera names should match!
////////////////////////////////////////////////////////////////////////

void PigPointNSYTarmCamera6dof::pointCamera(PigFileModel *file)
{
    if (pointCameraViaLabel(file))
        return;

    PigFileModelNSYT *fileNSYT = (PigFileModelNSYT*) file;
    _pointing_cs = _mission->getCoordSystem(file, NULL);

    _camera_model->setInitialCoordSystemNoTrans(_pointing_cs);

    PigPoint location = fileNSYT->getArticulationDevLocation(_calibration_location);
    PigQuaternion orientation = fileNSYT->getArticulationDevOrient(_calibration_orientation);
    
    orientation.getEulerAngles(_twist, _elevation, _azimuth);
    
    _azimuth = PigRad2Deg(_azimuth);
    _elevation = PigRad2Deg(_elevation);
    _twist = PigRad2Deg(_twist);
    
    pointCamera(_azimuth, _elevation, _twist,
                location.getX(), location.getY(), location.getZ(), 
	        _pointing_cs);
    

}


////////////////////////////////////////////////////////////////////////
// Read in the calibration pointing parameters for a given "point" file.
////////////////////////////////////////////////////////////////////////

void PigPointNSYTarmCamera6dof::read_point_info(char *filename, const char *host_id)
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

//!!!! NOT UPDATED FOR INSIGHT !!!!
    _calibration_orientation.setComponents(v);
    _pointing_error[0] = _pointing_error[1] = _pointing_error[2] = 0.085;
    _pointing_error[3] = _pointing_error[4] = _pointing_error[5] = 0.001;

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
            if (strncasecmp(line, "idc_pointing_error_6dof", 23) == 0) {
                sscanf(line, "idc_pointing_error_6dof = %lf %lf %lf %lf %lf %lf %lf",
                       &_pointing_error[0], &_pointing_error[1],
                       &_pointing_error[2], &_pointing_error[3],
                       &_pointing_error[4], &_pointing_error[5],
		       &_pointing_error[6]);
            }

	}
	fclose(inClientFile);
    }
}

