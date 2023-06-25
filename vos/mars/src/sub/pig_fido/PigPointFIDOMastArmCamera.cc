////////////////////////////////////////////////////////////////////////
// PigPointFIDOMastArmCamera

// Pointing model for FIDO Pancam and Navcam located on the Mast Arm.
//
// Physically-based pointing model, based on 4 mast angles.
////////////////////////////////////////////////////////////////////////

#include "PigPointFIDOMastArmCamera.h"
#include "PigFileModel.h"
#include "PigCameraModel.h"
#include "PigCoordSystem.h"
#include "PigMission.h"
#include "PigCAHVOR.h"
#include "mat3.h"

extern "C" {
    #include "fido_kinem.h"
    #include "cmod_xform.h"
}

#include <iostream>
using namespace std;
#include <string.h>
#include <stdio.h>


////////////////////////////////////////////////////////////////////////
// Constructor
////////////////////////////////////////////////////////////////////////

PigPointFIDOMastArmCamera::PigPointFIDOMastArmCamera(PigCameraModel *cm,
			PigMission *mission, const char *instrument)
	: PigPointingModel(cm, mission, instrument)
{
    _mastJoint1 = _mastJoint2 = _mastJoint3 = _mastJoint4 = 0.0;

    // This is changed in pointCamera(PigFile *)
    if (_camera_model)
	_pointing_cs = _mission->getCoordSystem(
		_camera_model->getCoordSystem(), "INSTRUMENT");
    else
        _pointing_cs = _mission->getCoordSystem("INSTRUMENT");

    // Read the .point file
    // Note:  same file for left and right cameras

    //char *pointFile = "point_files/FIDO_MAST_HI.point";
    char *pointFile = NULL;

    read_point_info(pointFile);
}

////////////////////////////////////////////////////////////////////////
// Destructor
////////////////////////////////////////////////////////////////////////

PigPointFIDOMastArmCamera::~PigPointFIDOMastArmCamera()
{
	// nothing to do...
}

////////////////////////////////////////////////////////////////////////
// Point a camera model, given an image.  This does not have to be the
// same image as was given in the constructor, although presumably the
// mission/camera names should match!
////////////////////////////////////////////////////////////////////////

void PigPointFIDOMastArmCamera::pointCamera(PigFileModel *file)
{
    PigFileModelFIDO* fileFIDO = (PigFileModelFIDO*) file;
    _pointing_cs = _mission->getCoordSystem(file, NULL);

    _camera_model->setInitialCoordSystemNoTrans(_pointing_cs);

    if (!fileFIDO->checkInstrumentMastJoint1Angle())
	printWarning("Warning: no MastJoint1Angle, 0 assumed");
    double mast_joint_angle1 = fileFIDO->getInstrumentMastJoint1Angle(0);

    if (!fileFIDO->checkInstrumentMastJoint2Angle())
	printWarning("Warning: no MastJoint2Angle, 90 assumed");
    double mast_joint_angle2 = fileFIDO->getInstrumentMastJoint2Angle(90);

    if (!fileFIDO->checkInstrumentMastJoint3Angle())
	printWarning("Warning: no MastJoint3Angle, 0 assumed");
    double mast_joint_angle3 = fileFIDO->getInstrumentMastJoint3Angle(0);

    if (!fileFIDO->checkInstrumentMastJoint4Angle())
	printWarning("Warning: no MastJoint4Angle, 0 assumed");
    double mast_joint_angle4 = fileFIDO->getInstrumentMastJoint4Angle(0);

    pointCamera(mast_joint_angle1,
		mast_joint_angle2,
		mast_joint_angle3,
		mast_joint_angle4);

}

////////////////////////////////////////////////////////////////////////
// Point the camera using four angles
////////////////////////////////////////////////////////////////////////
void PigPointFIDOMastArmCamera::pointCamera(const double mastJoint1,
					    const double mastJoint2,
					    const double mastJoint3,
					    const double mastJoint4)
{
   // saving the local parameters to member variables
    _mastJoint1 = mastJoint1;
    _mastJoint2 = mastJoint2;
    _mastJoint3 = mastJoint3;
    _mastJoint4 = mastJoint4;

  // Calculate the camera pointing in degrees
  double theta_rad[4];
  double r_cam[3][3];
  double t_cam[3];
  double c1[3];
  double c2[3];
  double a1[3], h1[3], v1[3], o1[3], r1[3];
  double a2[3], h2[3], v2[3], o2[3], r2[3];

  PigPoint c1_point;
  PigPoint  c2_point;
  PigVector a1_vector, h1_vector, v1_vector, o1_vector, r1_vector;
  PigVector a2_vector, h2_vector, v2_vector, o2_vector, r2_vector;

  theta_rad[0] = PigDeg2Rad(mastJoint1);
  theta_rad[1] = PigDeg2Rad(mastJoint2);
  theta_rad[2] = PigDeg2Rad(mastJoint3);
  theta_rad[3] = PigDeg2Rad(mastJoint4);

  fido_kinem_cam(theta_rad, r_cam, t_cam);

  _camera_model->resetCameraLocation();

  const char *name = _camera_model->getModelName();
  int is_cahvor = false;
  if (strcasecmp(name, "CAHVOR") == 0 || strcasecmp(name, "CAHVORE") == 0)
      is_cahvor = true;
  else if (strcasecmp(name, "CAHV") != 0) {
      char msg[256];
      sprintf(msg, "Unknown camera model type '%s' in FIDO pointing model");
      printError(msg);
      printError("CAHV subclass required.  Code may crash...");
  }

  if (is_cahvor)
      ((PigCAHVOR *)_camera_model)->getCurrentCAHVOR(c1_point, a1_vector, 
						h1_vector, v1_vector, 
						o1_vector, r1_vector);
  else {
      ((PigCAHV *)_camera_model)->getCurrentCAHV(c1_point, a1_vector, 
						h1_vector, v1_vector);
      o1_vector = a1_vector;
      r1_vector.setXYZ(0.0, 0.0, 0.0);
  }

  //convert from PigPoint, PigVector to doubles
  c1_point.getXYZ(c1);
  a1_vector.getXYZ(a1);
  h1_vector.getXYZ(h1);
  v1_vector.getXYZ(v1);
  o1_vector.getXYZ(o1);
  r1_vector.getXYZ(r1);

  cmod_cahvor_xform(r_cam, t_cam, c1, a1, h1, v1, o1, r1,
		      c2, a2, h2, v2, o2, r2);

  //now copy back to PigPoint PigVector classes
  c2_point.setXYZ(c2);
  a2_vector.setXYZ(a2);
  h2_vector.setXYZ(h2);
  v2_vector.setXYZ(v2);
  o2_vector.setXYZ(o2);
  r2_vector.setXYZ(r2);
  
  if (is_cahvor)
      ((PigCAHVOR*)_camera_model)->setCurrentCAHVOR(c2_point, a2_vector, 
						h2_vector, v2_vector, 
						o2_vector, r2_vector,
					    _camera_model->getCoordSystem());
  else
      ((PigCAHV*)_camera_model)->setCurrentCAHV(c2_point, a2_vector, 
						h2_vector, v2_vector, 
					    _camera_model->getCoordSystem());

}

////////////////////////////////////////////////////////////////////
// Point camera using az and el
////////////////////////////////////////////////////////////////////
void PigPointFIDOMastArmCamera::pointCamera(const double azimuth, const double elevation) {

  pointCamera(azimuth, 
	      _mastJoint2, 
	      _mastJoint3,
	      FIDO_JOINT4(elevation, _mastJoint2, _mastJoint3));
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

void PigPointFIDOMastArmCamera::pointCamera(const char *obs_id, 
					    char *data_source)
{
    printFatal("PigPointFIDOMastArmCamera::pointCamera(obs_id, data_source) not implemented yet!!");
}

////////////////////////////////////////////////////////////////////////
// These functions get and set the pointing and camera position, in
// the given coordinates.  They are *not* intended for pointing
// correction, use the get/setPointingParameters() functions for that.
////////////////////////////////////////////////////////////////////////

void PigPointFIDOMastArmCamera::setCameraOrientation(const PigVector 
						     &orientation,
						     PigCoordSystem *cs)
{
    if (!checkCameraModel())
	return;

    // The rotations must be done in the "natural" CS frame to mimic the az/el
    // motors... NOT in Fixed coordinates, which would introduce a twist!
  
    PigVector inst_pointing = _pointing_cs->convertVector(orientation, cs);
    pointCamera(PigRad2Deg(_pointing_cs->getAz(inst_pointing)),
		_mastJoint2,
		_mastJoint3,
		FIDO_JOINT4(PigRad2Deg(_pointing_cs->getEl(inst_pointing)),
				       _mastJoint2,
				       _mastJoint3));
}

////////////////////////////////////////////////////////////////////////
// These functions allow access to the "raw" pointing data, in order
// to accomplish pointing corrections or "tweaks".
// For the RAC camera:
//    params[0] = MastJoint1
//    params[1] = MastJoint2
//    params[2] = MastJoint3
//    params[3] = MastJoint4
//
// Angles are in degrees!
// Subclasses could define extra parameters, e.g. for a rover where the
// position is also unknown.
////////////////////////////////////////////////////////////////////////

void PigPointFIDOMastArmCamera::getPointingParameters(double params[],
						  const int max_count)
{
    if (max_count >= 1)
	params[0] = _mastJoint1;
    if (max_count >= 2)
	params[1] = _mastJoint2;
    if (max_count >= 3)
	params[2] = _mastJoint3;
    if (max_count >= 4)
	params[3] = _mastJoint4;
}

void PigPointFIDOMastArmCamera::setPointingParameters(const double params[],
						const int count)
{
    // Don't re-point if nothing has changed.
    if (count >= 4) {
        if (params[0] != _mastJoint1 || params[1] != _mastJoint2
                || params[2] != _mastJoint3
                || params[3] != _mastJoint4 )
            pointCamera(params[0], params[1], params[2],
                        params[3]);
    }
    else if (count == 3) {
        if (params[0] != _mastJoint1 || params[1] != _mastJoint2
                || params[2] != _mastJoint3)
            pointCamera(params[0], params[1], params[2],
                _mastJoint4);
    }
    else if (count == 2) {
        if (params[0] != _mastJoint1 || params[1] != _mastJoint2)
            pointCamera(params[0], params[1], _mastJoint3, _mastJoint4);
    }
    else if (count == 1) {
        if (params[0] != _mastJoint1)
            pointCamera(params[0], _mastJoint2,
                _mastJoint3, _mastJoint4);
    }
}

const char *const PigPointFIDOMastArmCamera::getPointingParamName(int i)
{
    switch (i) {
        case 0:
            return "MastJoint1";
        case 1:
            return "MastJoint2";
        case 2:
            return "MastJoint3";
        case 3:
            return "MastJoint4";
        default:
            return "Unknown";
    }
}
////////////////////////////////////////////////////////////////////////
// Read in the calibration pointing parameters for a given "point" file.
////////////////////////////////////////////////////////////////////////

void PigPointFIDOMastArmCamera::read_point_info(char *filename)
{

}

