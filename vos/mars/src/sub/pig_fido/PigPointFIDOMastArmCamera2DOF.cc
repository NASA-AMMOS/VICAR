////////////////////////////////////////////////////////////////////////
// PigPointFIDOMastArmCamera2DOF

// Pointing model for FIDO Pancam and Navcam located on the Mast Arm.
//
// Virtual pointing model, based on azimuth and elevation
// which derived from four joint angles.
////////////////////////////////////////////////////////////////////////

#include "PigPointFIDOMastArmCamera2DOF.h"

////////////////////////////////////////////////////////////////////////
// Constructor
////////////////////////////////////////////////////////////////////////

PigPointFIDOMastArmCamera2DOF::PigPointFIDOMastArmCamera2DOF(
							PigCameraModel *cm,
							PigMission *mission, 
							const char *instrument)
	: PigPointFIDOMastArmCamera(cm, mission, instrument)
{
	// nothing to do...
}

////////////////////////////////////////////////////////////////////////
// Destructor
////////////////////////////////////////////////////////////////////////

PigPointFIDOMastArmCamera2DOF::~PigPointFIDOMastArmCamera2DOF()
{
	// nothing to do...
}

////////////////////////////////////////////////////////////////////////
// These functions allow access to the "raw" pointing data, in order
// to accomplish pointing corrections or "tweaks".
// For the RAC camera:
//    params[0] = Azimuth
//    params[1] = Elevation
// Angles are in degrees!
// Subclasses could define extra parameters, e.g. for a rover where the
// position is also unknown.
////////////////////////////////////////////////////////////////////////

// change this to use the actual, agreed upon, final, parameters.

void PigPointFIDOMastArmCamera2DOF::getPointingParameters(double params[],
						  const int max_count)
{
    if (max_count >= 1)
	params[0] = _mastJoint1;
    if (max_count >= 2)
	params[1] = FIDO_ELEVATION(_mastJoint2, _mastJoint3, _mastJoint4);
}

void PigPointFIDOMastArmCamera2DOF::setPointingParameters(
						   const double params[],
						   const int count)
{
    // Don't re-point if nothing has changed.
    if (count >= 2) {
        if (params[0] != _mastJoint1
	    || params[1] != 
	    FIDO_ELEVATION(_mastJoint2, _mastJoint3, _mastJoint4)
	    )
	  pointCamera(params[0], params[1]);
    }
    else if (count == 1) {
        if (params[0] != _mastJoint1)
            pointCamera(params[0], 
			FIDO_ELEVATION(_mastJoint2, _mastJoint3, _mastJoint4));
    }
}

const char *const PigPointFIDOMastArmCamera2DOF::getPointingParamName(int i)
{
    switch (i) {
        case 0:
            return "Azimuth";
        case 1:
            return "Elevation";
        default:
            return "Unknown";
    }
}
