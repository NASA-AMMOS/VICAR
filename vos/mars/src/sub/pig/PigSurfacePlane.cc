////////////////////////////////////////////////////////////////////////
// PigSurfacePlane
//
// Surface model for a flat, tilted plane going through the origin.
// The constructor takes the Normal to this plane.
////////////////////////////////////////////////////////////////////////

#include "PigSurfacePlane.h"

#include "PigCoordSystem.h"

#include "iostream"
using namespace std;

////////////////////////////////////////////////////////////////////////
// Constructors
////////////////////////////////////////////////////////////////////////

PigSurfacePlane::PigSurfacePlane(char *mission, char *instrument, char *target)
	: PigSurfaceModel(mission, instrument, target)
{
    _normal.setXYZ(0.0, 0.0, 1.0);
    _normal.getXYZ(_normal_params);
    _ground.setXYZ(0.0, 0.0, 0.0);
    _ground.getXYZ(_ground_params);
}

PigSurfacePlane::PigSurfacePlane(char *target, PigCoordSystem *cs)
	: PigSurfaceModel(target, cs)
{
    _normal.setXYZ(0.0, 0.0, 1.0);
    _normal.getXYZ(_normal_params);
    _ground.setXYZ(0.0, 0.0, 0.0);
    _ground.getXYZ(_ground_params);
}

PigSurfacePlane::PigSurfacePlane(PigVector &normal, PigCoordSystem *cs)
	: PigSurfaceModel(NULL, cs)
{
    _normal = normal;
    _normal.normalize();
    _normal.getXYZ(_normal_params);
    _ground.setXYZ(0.0, 0.0, 0.0);
    _ground.getXYZ(_ground_params);
    _cs = cs;
}

PigSurfacePlane::PigSurfacePlane(PigVector &normal, PigPoint &ground,
							PigCoordSystem *cs)
	: PigSurfaceModel(NULL, cs)
{
    _normal = normal;
    _normal.normalize();
    _normal.getXYZ(_normal_params);
    _ground = ground;
    _ground.getXYZ(_ground_params);
    _cs = cs;
}

////////////////////////////////////////////////////////////////////////
// Destructor
////////////////////////////////////////////////////////////////////////

PigSurfacePlane::~PigSurfacePlane()
{
	// nothing to do...
}

////////////////////////////////////////////////////////////////////////
// Set the coordinate system.  This also adjusts the ground and normal
// vectors.
////////////////////////////////////////////////////////////////////////

void PigSurfacePlane::setCoordSystem(PigCoordSystem *cs)
{
    _normal = cs->convertVector(_normal, _cs);
    _normal.getXYZ(_normal_params);
    _ground = cs->convertPoint(_ground, _cs);
    _ground.getXYZ(_ground_params);
    _cs = cs;
}

////////////////////////////////////////////////////////////////////////
// Intersect a ray with the surface, giving a 3-D world location.
// This is the main purpose of surface models.
//
// Return codes:
// 0 == no intersection (ray parallel to plane)
// 1 == intersection "in front"
// -1 == intersects plane "behind" the camera.  There is no facility
//      for retrieving "backwards" intercepts; just invert the look
//      direction and try again.  (this subclass actually returns the backward
//	intercept, since it's already calculated)
// A return code > 0 means that the returned point is valid.  For return
// codes <= 0, the returned point is actually a unit vector pointing in
// the "infinity" direction (the same as the look vector since origin
// doesn't matter at infinity).
// Note that the caller must pass in the vector to be filled; this
// routine does not allocate it.  In the case of multiple intersections,
// it returns the "closest" one.
////////////////////////////////////////////////////////////////////////

int PigSurfacePlane::intersectRay(const PigPoint &origin,
			     const PigVector &look_direction,
			     PigPoint &intersect_point)
{
    // Set intersect point to the look direction for the inifinity case.

    intersect_point = look_direction;

    // ((ground - origin) dot normal) is the perp. distance to the plane.
    // (look_direction dot normal) is the projection of the look direction onto
    // that perpendicular.  Then by similar triangles, the ratio is multiplied
    // by the look direction to get the vector to the intercept point.

    double dot = look_direction % _normal;

    if (dot == 0.0)		// looking parallel to plane!
	return 0;
    double ratio = ((_ground - origin) % _normal) / dot;

    intersect_point = (PigPoint)(origin + (look_direction * ratio));

    if (ratio < 0.0) {
	intersect_point = look_direction;		// reset for infinity
	return -1;
    }

    return 1;
}

////////////////////////////////////////////////////////////////////////
// Like the above, except you can specify which of the n intercepts to
// return (which_intercept is 1-based).  Return >=1 if the given intercept
// is valid, or <=0 if it isn't.  For planes, only intercept 1 is valid.
////////////////////////////////////////////////////////////////////////

int PigSurfacePlane::intersectRay(const PigPoint &origin,
			     const PigVector &look_direction,
			     const int which_intercept,
			     PigPoint &intersect_point)
{
    intersect_point = look_direction;			// for infinity case
    if (which_intercept != 1)
	return 0;
    return intersectRay(origin, look_direction, intersect_point);
}

////////////////////////////////////////////////////////////////////////
// Counts the number of times a ray hits the surface.
// Return codes:
// 0 == no intersection
// 1 == 1 intersect point
// -1 == intersects surface "behind" the camera.  There is no facility
//       for retrieving "backwards" intercepts; just invert the look
//       direction and try again.
// Not too exciting for this subclass, since there will always be 0 or 1
// (or -1).
////////////////////////////////////////////////////////////////////////

int PigSurfacePlane::countIntersections(const PigPoint &origin,
				   const PigVector &look_direction)
{
    PigPoint intercept;			// thrown away

    return intersectRay(origin, look_direction, intercept);
}

////////////////////////////////////////////////////////////////////////
// Copy from the label struct to instance vars.  The read framework is
// handled by the superclass.
////////////////////////////////////////////////////////////////////////

void PigSurfacePlane::useLabelStruct(LblSurfaceModel_typ *SurfaceModel)
{
    if (! (SurfaceModel->SurfaceNormalVector.Valid &&
	   SurfaceModel->SurfaceGroundLocation.Valid)) {
	printError("Surface PLANE parameters not valid in label");
	return;
    }

    // These values are always stored in the mission's Fixed frame

    PigVector normal(SurfaceModel->SurfaceNormalVector.Value);
    _normal = _cs->convertVectorFromFixed(normal);
    _normal.getXYZ(_normal_params);

    PigPoint ground(SurfaceModel->SurfaceGroundLocation.Value);
    _ground = _cs->convertPointFromFixed(ground);
    _ground.getXYZ(_ground_params);

    return;
}

////////////////////////////////////////////////////////////////////////
// Copy from instance vars to the label struct.  The write framework is
// handled by the superclass.
////////////////////////////////////////////////////////////////////////

void PigSurfacePlane::fillLabelStruct(LblSurfaceModel_typ *SurfaceModel) const
{
    SurfaceModel->SurfaceNormalVector.Value[0] = _normal.getX();
    SurfaceModel->SurfaceNormalVector.Value[1] = _normal.getY();
    SurfaceModel->SurfaceNormalVector.Value[2] = _normal.getZ();
    SurfaceModel->SurfaceNormalVector.Valid = 1;

    SurfaceModel->SurfaceGroundLocation.Value[0] = _ground.getX();
    SurfaceModel->SurfaceGroundLocation.Value[1] = _ground.getY();
    SurfaceModel->SurfaceGroundLocation.Value[2] = _ground.getZ();
    SurfaceModel->SurfaceGroundLocation.Valid = 1;

    return;
}

////////////////////////////////////////////////////////////////////////
// Print the model in a readable form...
////////////////////////////////////////////////////////////////////////

void PigSurfacePlane::print()
{
    char msg[512];
    sprintf(msg, "Surface: Plane, ground=\\( %f, %f, %f \\) normal=\\( %f, %f, %f \\)",
		_ground.getX(), _ground.getY(), _ground.getZ(),
		_normal.getX(), _normal.getY(), _normal.getZ());
    printInfo(msg);
}

////////////////////////////////////////////////////////////////////////
// These functions allow access to the parameters, in order to allow
// "tweaks" to the surface model.
// Parameters are:
//    X, Y, Z, Normal_X, Normal_Y, Normal_Z
// NORMAL values are components of a unit vector, which is normalized before
// use, but the parameters themselves are not changed.
////////////////////////////////////////////////////////////////////////

void PigSurfacePlane::getPointingParameters(double params[],
							const int max_count)
{
    if (max_count >= 1)
	params[0] = _ground_params[0];
    if (max_count >= 2)
	params[1] = _ground_params[1];
    if (max_count >= 3)
	params[2] = _ground_params[2];
    if (max_count >= 4)
	params[3] = _normal_params[0];
    if (max_count >= 5)
	params[4] = _normal_params[1];
    if (max_count >= 6)
	params[5] = _normal_params[2];
}

void PigSurfacePlane::setPointingParameters(const double params[],
						const int count)
{
    // Note that there's no "re-pointing" involved here...

    if (count >= 1)
	_ground_params[0] = params[0];
    if (count >= 2)
	_ground_params[1] = params[1];
    if (count >= 3)
	_ground_params[2] = params[2];
    if (count >= 4)
	_normal_params[0] = params[3];
    if (count >= 5)
	_normal_params[1] = params[4];
    if (count >= 6)
	_normal_params[2] = params[5];

    _ground.setXYZ(_ground_params);
    _normal.setXYZ(_normal_params);
    _normal.normalize();
}

const char *const PigSurfacePlane::getPointingParamName(int i)
{
    switch (i) {
	case 0:
	    return "X";
	case 1:
	    return "Y";
	case 2:
	    return "Z";
	case 3:
	    return "Normal_X";
	case 4:
	    return "Normal_Y";
	case 5:
	    return "Normal_Z";
	default:
	    return "Unknown";
    }
}

void PigSurfacePlane::getPointingErrorEstimate(double errors[],
					const int max_count)
{
    int i;
    int n = max_count;
    if (n > 6) n = 6;		// only 6 params

    for (i=0; i < n; i++)
	errors[i] = .02;		// 2 cm error, or 2% of unit vector
}

