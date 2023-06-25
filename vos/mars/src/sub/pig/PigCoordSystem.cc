////////////////////////////////////////////////////////////////////////
// PigCoordSystem
//
// See .h file for comments.
////////////////////////////////////////////////////////////////////////

#include "PigCoordSystem.h"

#include "PigVector.h"
#include "PigFileModel.h"
#include "PigMission.h"
#include "mat3.h"

#include "return_status.h"

////////////////////////////////////////////////////////////////////////
// Constructor.  Should be called only by the create functions in PigMission
////////////////////////////////////////////////////////////////////////

PigCoordSystem::PigCoordSystem(PigMission *m, PigCSDefinition *csdef)
{
    char msg[1024];

    _mission = m;

    _csdef = new PigCSDefinition(*csdef);

    _ref_cs = NULL;

    _cacheValid = FALSE;
    _updatingCache = FALSE;

    // Add ourselves to the master list

    PigCoordSystem **csl = _mission->getCSList();
    int *csn = _mission->getCSCounter();
    if (*csn >= PIG_MAX_CS) {
	printFatal("Too many CS objects!");
    } else {
	csl[*csn] = this;
	(*csn)++;
    }
}

////////////////////////////////////////////////////////////////////////
// If we delete the fixed CS we're in trouble... but I don't want an error
// when the program terminates in case it's actually cleaning up.  So
// just hope that doesn't happen...
////////////////////////////////////////////////////////////////////////

PigCoordSystem::~PigCoordSystem()
{
    if (_csdef)
	delete _csdef;

    // Delete our entry in the master list

    PigCoordSystem **csl = _mission->getCSList();
    int *csn = _mission->getCSCounter();
    for (int i=0; i < *csn; i++) {
	if (csl[i] == this) {		// found, delete and shift others down
	    for (int j=i; j < *csn-1; j++) {
		csl[j] = csl[j+1];
	    }
	    (*csn)--;
	    break;
	}
    }
}

////////////////////////////////////////////////////////////////////////
// Set the quaternion or offset.
////////////////////////////////////////////////////////////////////////

void PigCoordSystem::setOffset(PigVector offset)
{
    _csdef->setOffset(offset);
    invalidateCache();
}

void PigCoordSystem::setQuaternion(PigQuaternion quat)
{
    _csdef->setQuaternion(quat);
    invalidateCache();
}

void PigCoordSystem::setQuaternion(double heading, double pitch, double roll)
{
    double rotMatrix[3][3];
    double quaternion[4];
    
    double cosPitch   = cos(pitch);
    double sinPitch   = sin(pitch);
    double cosHeading = cos(heading);
    double sinHeading = sin(heading);
    double cosRoll    = cos(roll);
    double sinRoll    = sin(roll);

    //initialize rotation matrix
    rotMatrix[0][0] =  cosPitch * cosHeading;
    rotMatrix[0][1] = -cosPitch * sinHeading;
    rotMatrix[0][2] =  sinPitch;
    rotMatrix[1][0] =  cosRoll * sinHeading + sinRoll * sinPitch * cosHeading;
    rotMatrix[1][1] =  cosRoll * cosHeading - sinRoll * sinPitch * sinHeading;
    rotMatrix[1][2] = -sinRoll * cosPitch;
    rotMatrix[2][0] =  sinRoll * sinHeading - cosRoll * sinPitch * cosHeading;
    rotMatrix[2][1] =  sinRoll * cosHeading + cosRoll * sinPitch * sinHeading;
    rotMatrix[2][2] =  cosRoll * cosPitch; 

    //compute quaternion
    quatr(rotMatrix, quaternion);

    PigQuaternion q;
    q.setComponents(quaternion);
    setQuaternion(q);
}

////////////////////////////////////////////////////////////////////////
// Update the cache.  Triggers setting the Fixed CS, if not already done
////////////////////////////////////////////////////////////////////////

void PigCoordSystem::updateCache()
{
    char msg[1024];
    if (_cacheValid)
	return;				// already done...

    if (_updatingCache) {		// BIG WHOOPS !!!!
	printError("ERROR!  Loop detected in reference coordinate system chain for:");
	print();
	printError("CS transformations are likely incorrect!!!");
	_cacheValid = TRUE;	// not really, but avoids infinite printing
	_updatingCache = FALSE;
	return;			// nothing else to do...
    }

    _updatingCache = TRUE;

    // Obtain the Fixed CS

    PigCoordSystem *fixed = _mission->getFixedCS();

    // If we ARE the fixed site, just set the transforms to identity

    if (fixed == this) {
	_fixed_to_this_offset = PigVector();
	_this_to_fixed_quat = PigQuaternion();
    }
    else {

	// See if we need to instantiate the reference site

	if (_ref_cs == NULL) {
	    _ref_cs = _mission->getCoordSystem(_csdef->getReference());
	    if (_ref_cs == NULL) {
		if (_csdef->getReference() != NULL) {
		    sprintf(msg, "Unable to create reference frame for: %s",
				_csdef->getReference()->getFullName());
		    printError(msg);
		    printError("Coordinate transforms are likely incorrect!!!");
		    _cacheValid = TRUE;  // not really, but avoids infinite printing
		    _updatingCache = FALSE;
		    return;			// nothing else to do...
		}
	    }
	}

	// If we're still null at this point, it's because the reference csref
	// was null... which is legal in some cases.

 	if (_ref_cs == NULL) {

	    _fixed_to_this_offset = _csdef->getOffset();
	    _this_to_fixed_quat = _csdef->getQuaternion();

	} else {

	    // Composite our transform with that of the reference to get the
	    // fixed -> us transform.

	    // Get the reference's offset/quat w.r.t. the Fixed site.  This can
	    // trigger its own cache update in a cascade.

	    PigVector fixed_to_ref_offset = _ref_cs->getFixedOffset();
	    PigQuaternion ref_to_fixed_quat = _ref_cs->getFixedQuaternion();

	    // _csdef contains this to ref
	    // the above has ref to fixed
	    // composite them to get this to fixed

	    _fixed_to_this_offset = fixed_to_ref_offset +
				ref_to_fixed_quat * _csdef->getOffset();

	    _this_to_fixed_quat = ref_to_fixed_quat * _csdef->getQuaternion();
	}
    }

//!!!! Need to check for several things here:
//!!!!   Backward chaining (have to run the transform backwards if we go up)
//!!!!   Ref chain not leading to fixed (may be covered by _ref_cs==NULL check
//!!!!     above)
//!!!! For full generality we'd search the tree for the fixed frame in order
//!!!! to find a path (bidirectionally) but that's a huge job and has never
//!!!! been needed...


    _updatingCache = FALSE;

    _cacheValid = TRUE;

}


////////////////////////////////////////////////////////////////////////
// Conversion functions...
////////////////////////////////////////////////////////////////////////

PigPoint PigCoordSystem::convertPoint(PigPoint old_point,
				      PigCoordSystem *old_cs)
{
    if (old_cs == this)			// no conversion
	return old_point;
    if (old_cs == NULL) {
	char msg[256];
	sprintf(msg, "Converting point from null CS to %s, conversion ignored",
			getFullName());
	printError(msg);
	return old_point;
    }
    if (_mission != old_cs->getMission()) {
	printError("Coordinate conversion between missions not implemented");
	return old_point;
    }
    return convertPointFromFixed(old_cs->convertPointToFixed(old_point));
}

PigVector PigCoordSystem::convertVector(PigVector old_vector,
				       PigCoordSystem *old_cs)
{
    if (old_cs == this)			// no conversion
	return old_vector;
    if (old_cs == NULL) {
	char msg[256];
	sprintf(msg, "Converting vector from null CS to %s, conversion ignored",
			getFullName());
	printError(msg);
	return old_vector;
    }
    if (_mission != old_cs->getMission()) {
	printError("Coordinate conversion between missions not implemented");
	return old_vector;
    }
    return convertVectorFromFixed(old_cs->convertVectorToFixed(old_vector));
}

void PigCoordSystem::convertAzEl(double old_az, double old_el,
		double &new_az, double &new_el, PigCoordSystem *old_cs)
{
    if (old_cs == this) {		// no conversion
	new_az = old_az;
	new_el = old_el;
	return;
    }
    if (old_cs == NULL) {
	char msg[256];
	sprintf(msg, "Converting az/el from null CS to %s, conversion ignored",
			getFullName());
	printError(msg);
	new_az = old_az;
	new_el = old_el;
	return;
    }
    if (_mission != old_cs->getMission()) {
	printError("Coordinate conversion between missions not implemented");
	new_az = old_az;
	new_el = old_el;
	return;
    }
    double fixed_az, fixed_el;
    old_cs->convertAzElToFixed(old_az, old_el, fixed_az, fixed_el);
    convertAzElFromFixed(fixed_az, fixed_el, new_az, new_el);
}

void PigCoordSystem::convertAzElDegrees(double old_az, double old_el,
		double &new_az, double &new_el, PigCoordSystem *old_cs)
{
    if (old_cs == this) {		// no conversion
	new_az = old_az;
	new_el = old_el;
	return;
    }
    double old_az_r = PigDeg2Rad(old_az);
    double old_el_r = PigDeg2Rad(old_el);
    double new_az_r, new_el_r;
    convertAzEl(old_az_r, old_el_r, new_az_r, new_el_r, old_cs);
    new_az = PigRad2Deg(new_az_r);
    new_el = PigRad2Deg(new_el_r);
}

// Just rotate the vector part of the quaternion.  Subclasses do not need
// to override this.

PigQuaternion PigCoordSystem::convertQuat(PigQuaternion old_quat,
					  PigCoordSystem *old_cs)
{
    if (old_cs == this)			// no conversion
	return old_quat;
    double theta = old_quat.getTheta();
    PigVector u = old_quat.getU();
    u = convertVector(u, old_cs);
    return PigQuaternion(u, theta);
}

////////////////////////////////////////////////////////////////////////
// The pointing parameters are:
// X, Y, Z
// Roll, Pitch, Yaw
////////////////////////////////////////////////////////////////////////

void PigCoordSystem::getPointingParameters(double params[], const int max_count)
{
    double roll, pitch, yaw;
    if (!_cacheValid)
	updateCache();

    if (max_count >= 1)
	params[0] = getOffset().getX();
    if (max_count >= 2)
	params[1] = getOffset().getY();
    if (max_count >= 3)
	params[2] = getOffset().getZ();

    if (max_count >= 4)
        getQuaternion().getEulerAngles(roll, pitch, yaw);

    if (max_count >= 4)
	params[3] = PigRad2Deg(roll);
    if (max_count >= 5)
	params[4] = PigRad2Deg(pitch);
    if (max_count >= 6)
	params[5] = PigRad2Deg(yaw);
}

void PigCoordSystem::setPointingParameters(const double params[],
							const int count)
{
    double roll, pitch, yaw;
    int changedO = FALSE;
    int changedQ = FALSE;

    PigVector offset = getOffset();
    if (count >= 1) {
	if (offset.getX() != params[0]) {
	    changedO = TRUE;
	    offset.setX(params[0]);
	}
    }
    if (count >= 2) {
	if (offset.getY() != params[1]) {
	    changedO = TRUE;
	    offset.setY(params[1]);
	}
    }
    if (count >= 3) {
	if (offset.getZ() != params[2]) {
	    changedO = TRUE;
	    offset.setZ(params[2]);
	}
    }
    if (changedO)
	setOffset(offset);

    PigQuaternion quat = getQuaternion();
    if (count >= 4)
        quat.getEulerAngles(roll, pitch, yaw);

    double v;
    if (count >= 4) {
	v = PigDeg2Rad(params[3]);
	if (roll != v) {
	    changedQ = TRUE;
	    roll = v;
	}
    }
    if (count >= 5) {
	v = PigDeg2Rad(params[4]);
	if (pitch != v) {
	    changedQ = TRUE;
	    pitch = v;
	}
    }
    if (count >= 6) {
	v = PigDeg2Rad(params[5]);
	if (yaw != v) {
	    changedQ = TRUE;
	    yaw = v;
	}
    }

    if (count >= 4 && changedQ) {
	quat.setEulerAngles(roll, pitch, yaw);
	setQuaternion(quat);
    }

    if (changedO || changedQ)
	invalidateCache();
}

const char *const PigCoordSystem::getPointingParamName(int i)
{
    switch (i) {
	case 0:
            return "X";
	case 1:
            return "Y";
	case 2:
            return "Z";
	case 3:
	    return "Roll";
	case 4:
	    return "Pitch";
	case 5:
	    return "Yaw";
	default:
            return "Unknown";
    }
}

void PigCoordSystem::getPointingErrorEstimate(double errors[],
							const int max_count)
{
    int i;
    int n = max_count;
    if (n > 6) n = 6;		// only 6 params

    for (i=0; i < n; i++) {
	if (i < 3)
	    errors[i] = 0.01;	// 1cm positional error
	else
	    errors[i] = 0.1;	// 0.1 degree angular error
    }
}


////////////////////////////////////////////////////////////////////////
// Invalidates the cache for this CS.  In order to maintain consistency,
// we must find all other CS's for the current mission which use us as a
// reference, and invalidate THEIR caches as well.
////////////////////////////////////////////////////////////////////////

void PigCoordSystem::invalidateCache()
{
    if (!_cacheValid)
	return;				// already done...

    _cacheValid = FALSE;

    PigCoordSystem **list = _mission->getCSList();
    int n = *_mission->getCSCounter();

    for (int i=0; i < n; i++) {
	if (getIdentity()->isEqual(list[i]->getReference())) {
	    list[i]->invalidateCache();
	}
    }
}

////////////////////////////////////////////////////////////////////////
// Prints the CS
////////////////////////////////////////////////////////////////////////

void PigCoordSystem::print()
{
    char msg[1024];

    sprintf(msg, "CS:  %s", getIdentity()->getFullName());
    printInfo(msg);
    sprintf(msg, "Ref: %s", getReference() ?
			    getReference()->getFullName() : "NULL");
    printInfo(msg);
    double q[4];
    getQuaternion().getComponents(q);
    sprintf(msg, "Offset: (%f %f %f)  quat: (%f %f %f %f)",
	getOffset().getX(), getOffset().getY(), getOffset().getZ(),
	q[0], q[0], q[2], q[3]);
    printInfo(msg);
}

