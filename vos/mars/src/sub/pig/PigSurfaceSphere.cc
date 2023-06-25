////////////////////////////////////////////////////////////////////////
// PigSurfaceSphere
//
// Surface model for a flat, tilted sphere going through the origin.
// The constructor takes the Normal to this sphere.
//
// Note that _center is completely ignored (or rather, an error results
// if you attempt to use it).  Use the Sphere1 or Sphere2 models if you
// need an alternate center.
////////////////////////////////////////////////////////////////////////

#include "PigSurfaceSphere.h"

#include "PigCoordSystem.h"

#include "iostream"
using namespace std;

////////////////////////////////////////////////////////////////////////
// Constructors
////////////////////////////////////////////////////////////////////////

PigSurfaceSphere::PigSurfaceSphere(char *mission, char *instrument,char *target)
	: PigSurfaceModel(mission, instrument, target)
{
    _radius = 0.0;
}

PigSurfaceSphere::PigSurfaceSphere(double &radius, PigCoordSystem *cs)
	: PigSurfaceModel(NULL, cs)
{
    _radius = radius;
    _cs = cs;
    _isCenterDefined = FALSE;
}
PigSurfaceSphere::PigSurfaceSphere(double &radius, PigPoint &center,
				   PigCoordSystem *cs)
	: PigSurfaceModel(NULL, cs)
{
    _radius = radius;
    _center = center;
    _cs = cs;
    _isCenterDefined = TRUE;
}

////////////////////////////////////////////////////////////////////////
// Destructor
////////////////////////////////////////////////////////////////////////

PigSurfaceSphere::~PigSurfaceSphere()
{
	// nothing to do...
}

////////////////////////////////////////////////////////////////////////
// Convert to the given coord system
////////////////////////////////////////////////////////////////////////
void PigSurfaceSphere::setCoordSystem(PigCoordSystem *cs)
{
    if (_isCenterDefined)
	_center = cs->convertPoint(_center, _cs);
    PigSurfaceModel::setCoordSystem(cs);
}

////////////////////////////////////////////////////////////////////////
// Intersect a ray with the surface, giving a 3-D world location.
// This is the main purpose of surface models.
//
// Return codes:
// 0 == no intersection (ray parallel to sphere)
// 1 == intersection "in front"
// -1 == intersects sphere "behind" the camera.  There is no facility
//      for retrieving "backwards" intercepts; just invert the look
//      direction and try again.  (this subclass actually returns the backward
//	intercept, since it's already calculated)
// A return code > 0 means that the returned point is valid.  For return
// codes <= 0, the returned point is actually a unit vector pointing in
// the "infinity" direction (the same as the look vector since origin
// doesn't matter at infinity).
// Note that the caller must pass in the vector to be filled; this
// routine does not allocate it.
//
// Note that currently only special case when Ray and Sphere origins 
// coinside is supported.  That implies that the return code is always 1
////////////////////////////////////////////////////////////////////////

int PigSurfaceSphere::intersectRay(const PigPoint &origin,
				   const PigVector &look_direction,
				   PigPoint &intersect_point)
{
    if(_isCenterDefined)
      printf("Warning: The case when surface sphere has center different from ray's origin is not currently supported!!!!");

      PigVector look;

      look = look_direction;
      look.normalize();
      look*=_radius;
      intersect_point = look + origin;
      return 1;
}

// Like the above, except you can specify which of the n intercepts to
// return (which_intercept is 1-based).  Return >=1 if the given intercept
// is valid, or <=0 if it isn't.  For , only intercept 1 is valid.
// The point is set for infinity, as above.
// Since we support only special case where ray originates from center of the
// sphere, we always have only one intersection.  So which_intercept is 
// irrelevant for this case.
int PigSurfaceSphere::intersectRay(const PigPoint &origin,
				   const PigVector &look_direction,
				   const int which_intercept,
				   PigPoint &intersect_point)
{
    return intersectRay(origin, look_direction, intersect_point);
}
// Counts the number of times a ray hits the surface.
// Return codes:
// 0 == no intersection
// 1 == 1 intersect point
// -1 == intersects surface "behind" the camera.  There is no facility
//       for retrieving "backwards" intercepts; just invert the look
//       direction and try again.
// For the sphere's special case(ray originates from center of the sphere)
// we always have 1 intersection.
int PigSurfaceSphere::countIntersections(const PigPoint &origin,
		       const PigVector &look_direction)
{
  return 1;
}

////////////////////////////////////////////////////////////////////////
// Copy from instance vars to the label struct.  The write framework is
// handled by the superclass.
////////////////////////////////////////////////////////////////////////

void PigSurfaceSphere::fillLabelStruct(LblSurfaceModel_typ *SurfaceModel) const
{
    SurfaceModel->SurfaceNormalVector.Value[0] = _radius;
    SurfaceModel->SurfaceNormalVector.Value[1] = 0.0;
    SurfaceModel->SurfaceNormalVector.Value[2] = 0.0;
    SurfaceModel->SurfaceNormalVector.Valid = 1;

    SurfaceModel->SurfaceGroundLocation.Value[0] = _center.getX();
    SurfaceModel->SurfaceGroundLocation.Value[1] = _center.getY();
    SurfaceModel->SurfaceGroundLocation.Value[2] = _center.getZ();
    SurfaceModel->SurfaceGroundLocation.Valid = 1;

    return;
}
////////////////////////////////////////////////////////////////////////
// Print the model in a readable form...
////////////////////////////////////////////////////////////////////////

void PigSurfaceSphere::print()
{
    char msg[512];
    sprintf(msg, "Surface: Sphere, radius=%f", _radius);
    printInfo(msg);
}

////////////////////////////////////////////////////////////////////////
// These functions allow access to the parameters, in order to allow
// "tweaks" to the surface model.
// Parameters are:
//    Radius
////////////////////////////////////////////////////////////////////////

void PigSurfaceSphere::getPointingParameters(double params[],
							const int max_count)
{
    if (max_count >= 1)
	params[0] = _radius;
}

void PigSurfaceSphere::setPointingParameters(const double params[],
						const int count)
{
    // Note that there's no "re-pointing" involved here...

    if (count >= 1)
	_radius = params[0];
}

const char *const PigSurfaceSphere::getPointingParamName(int i)
{
    switch (i) {
	case 0:
	    return "Radius";
	default:
	    return "Unknown";
    }
}

void PigSurfaceSphere::getPointingErrorEstimate(double errors[],
					const int max_count)
{
    if (max_count >= 1)
	errors[0] = .01;		// 1 cm error
}

