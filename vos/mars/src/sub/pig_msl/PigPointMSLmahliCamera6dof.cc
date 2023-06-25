////////////////////////////////////////////////////////////////////////
// PigPointMSLmahliCamera6dof
//
// Pointing model for MSL MAHLI cameras.
//
// Normally pointing is done in the Payload(Lander) frame.  
////////////////////////////////////////////////////////////////////////

#include "PigMSL.h"
#include "PigCameraModel.h"
#include "PigPointCamera6dof.h"
#include "PigPointMSLmahliCamera6dof.h"

////////////////////////////////////////////////////////////////////////
// Constructor
////////////////////////////////////////////////////////////////////////

PigPointMSLmahliCamera6dof::PigPointMSLmahliCamera6dof(PigCameraModel *cm,
					       PigMission *mission, 
					       const char *instrument)
                              : PigPointCamera6dof(cm, mission, instrument)
{
    // Read the .point file only for this subclass specific info
    // the rest is done in super class
    // Naming convention for pointing files: "host_id_mahli.point"
    // where host_id = MSL, MSLSIM, etc.
    char point_file[255];
    sprintf(point_file, "param_files/%s_mahli.point",
				((PigMSL *)_mission)->getHostID());

    read_point_info(point_file, ((PigMSL *)_mission)->getHostID());
}

////////////////////////////////////////////////////////////////////////
// Destructor
////////////////////////////////////////////////////////////////////////

PigPointMSLmahliCamera6dof::~PigPointMSLmahliCamera6dof()
{
	// nothing to do...
}
////////////////////////////////////////////////////////////////////////
// Point a camera model, given an image file.  This does not have to be the
// same image as was given in the constructor, although presumably the
// mission/camera names should match!
////////////////////////////////////////////////////////////////////////

void PigPointMSLmahliCamera6dof::pointCamera(PigFileModel *file)
{
    if (pointCameraViaLabel(file))
        return;

    PigFileModelMSL *fileMSL = (PigFileModelMSL*) file;
    _pointing_cs = _mission->getCoordSystem(file, NULL);

    _camera_model->setInitialCoordSystemNoTrans(_pointing_cs);

    PigPoint location = fileMSL->getArticulationDevLocation(_calibration_location);
    PigQuaternion orientation = fileMSL->getArticulationDevOrient(_calibration_orientation);
    
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

void PigPointMSLmahliCamera6dof::read_point_info(char *filename, const char *host_id)
{
    FILE *inClientFile;
    char line[255];
    int num_read;

    // open the file
    
    inClientFile = PigModelBase::openConfigFile(filename, NULL);

    // Default pointing values, in case the .point file isn't found
    // or the necessary values are not in the file.
    _calibration_location.setXYZ(2.348408, -0.402685, -1.881315);
    double v[4] = {0.506944542, 0.859133374, -0.009551398, 0.069324227};
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

	    if (strncasecmp(line, "mahli_calibration_position", 26) == 0) {
		num_read = sscanf(line, "mahli_calibration_position = %lf %lf %lf",
		                  &dum[0], &dum[1], &dum[2]);
                if (num_read == 3)
		   _calibration_location.setXYZ(dum);
                else
                   printError("Mahli calibration position read incorrectly!");
	    }
	    if (strncasecmp(line, "mahli_calibration_quaternion", 28) == 0) {
		num_read = sscanf(line, "mahli_calibration_quaternion = %lf %lf %lf %lf",
		                  &dum[0], &dum[1], &dum[2], &dum[3]);
                if (num_read == 4)
		   _calibration_orientation.setComponents(dum);
                else
                   printError("Mahli calibration quaternion read incorrectly!");
		_calibration_orientation.setComponents(dum);
	    }
            if (strncasecmp(line, "mahli_pointing_error_6dof", 25) == 0) {
                num_read = sscanf(line, "mahli_pointing_error_6dof = %lf %lf %lf %lf %lf %lf",
                       &_pointing_error[0], &_pointing_error[1],
                       &_pointing_error[2], &_pointing_error[3],
                       &_pointing_error[4], &_pointing_error[5]);
                if (num_read != 6) {
                   printError("Mahli calibration pointing errors read incorrectly!");
                   // Back to default
                   _pointing_error[0] = _pointing_error[1] = _pointing_error[2] = 0.085;
                   _pointing_error[3] = _pointing_error[4] = _pointing_error[5] = 0.001;
                }
            }

	}
	fclose(inClientFile);
    }
}

