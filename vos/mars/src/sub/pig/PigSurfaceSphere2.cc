////////////////////////////////////////////////////////////////////////
// PigSurfaceSphere2
//
// Surface model for a Sphere described by its Center and Radius.
// The main purpose of this model, method intersectRay() returns 
// farthest (second) intersection for the case when ray crosses
// the sphere twice.
// The "second intersection point" model is useful to model concave
// surfaces, like crater, when the camera point is outside looking in.
// It returns farthest(second) ray-surface intersection point.  For
// the cases where camera is inside the sphere, like rover sitting
// in the crater, there is only a single intersection point and
// both SPHERE1 and SPHERE2 behave exactly the same.
////////////////////////////////////////////////////////////////////////

#include "PigSurfaceSphere2.h"

#include "PigCoordSystem.h"

#include "iostream"
using namespace std;

////////////////////////////////////////////////////////////////////////
// Constructors
////////////////////////////////////////////////////////////////////////

// By default we construct unit sphere centered at the origin
PigSurfaceSphere2::PigSurfaceSphere2(char *mission, char *instrument, char *target)
	: PigSurfaceModel(mission, instrument, target)
{
    _radius = 1.0;
    _center.setXYZ(0.0, 0.0, 0.0);
}
PigSurfaceSphere2::PigSurfaceSphere2(char *target, PigCoordSystem *cs)
	: PigSurfaceModel(target, cs)
{
    _radius = 1.0;
    _center.setXYZ(0.0, 0.0, 0.0);
}
// set surface radius
PigSurfaceSphere2::PigSurfaceSphere2(double &radius, PigCoordSystem *cs)
	: PigSurfaceModel(NULL, cs)
{
    _radius = radius;
    _center.setXYZ(0.0, 0.0, 0.0);
    _cs = cs;
}
PigSurfaceSphere2::PigSurfaceSphere2(double &radius, PigPoint &center,
				     PigCoordSystem *cs)
	: PigSurfaceModel(NULL, cs)
{
    _radius = radius;
    _center = center;
    _cs = cs;
}

////////////////////////////////////////////////////////////////////////
// Destructor
////////////////////////////////////////////////////////////////////////

PigSurfaceSphere2::~PigSurfaceSphere2()
{
	// nothing to do...
}

////////////////////////////////////////////////////////////////////////
// Convert to the given coord system.  Only origin needs to be converted.
////////////////////////////////////////////////////////////////////////
void PigSurfaceSphere2::setCoordSystem(PigCoordSystem *cs)
{
    _center = cs->convertPoint(_center, _cs);
    _cs = cs;
}

////////////////////////////////////////////////////////////////////////
// Like the below, except you can specify which of the n intercepts to
// return (which_intercept is 1-based).  Return >=1 if the given intercept
// is valid, or <=0 if it isn't. 
// Since we have different subclasses PigSurfaceSphere1 and PigSurfaceSphere2
// depending weather we want first(nearest) or second(farthest) intersect, 
// which_intercept is ignored.
//
// Note that for Sphere2, we don't bother checking which_intercept unless
// it matters...
////////////////////////////////////////////////////////////////////////
//
int PigSurfaceSphere2::intersectRay(const PigPoint &origin,
				   const PigVector &look_direction,
				   const int which_intercept,
				   PigPoint &intersect_point)
{
    // Set intersect point to the look direction for the inifinity case.

    intersect_point = look_direction;

    // Note: below the vector is denoted by upper case
    // For a sphere described by its _center c and _radius r,
    // a point p belongs to the sphere if its distance from 
    // the center is equal to r, i.e. |CP|=|p-c|=r
    // A point on the ray has a form o + t*D (where o is origin
    // D is look_direction and t is non-negative number)
    // An intersection of a ray o + t*D and a sphere can be
    // found from the above equations by substituting
    // p:= o + t*D:
    // r = |o + t*D - c| = |CO + t*D|
    // By squaring both sides of the above equation we obtain:
    // r^2 = |CO|^2 + 2*(CO*D)*t + |D|^2 * t^2
    // We can write this as quadratic equation:
    // a*t^2 + b*t + c = 0  (c is a coeff. here NOT center)
    // where:
    // a = |d|^2
    // b = 2*(CO*D)
    // c = |co|^2 - r^2
    double look_magnitude = look_direction.magnitude();
    double co_mag = (origin - _center).magnitude();
    double a = look_magnitude * look_magnitude;
    double b = 2*((origin - _center) % look_direction);
    double c = co_mag*co_mag - _radius*_radius;
    
    // Forgetting that we need t >= 0 for a while, we 
    // can say that the number of solutions depends
    // on its discriminant:
    double discriminant = b*b - 4*a*c;
    
    // if discriminant < 0, no solution exist,
    // ray certainly misses the sphere
    if (discriminant < 0)
        return 0;

    // if discriminant = 0, there is only one solution
    // (ray is tangent to the sphere).  
    // return 0 if t < 0, otherwise return 1
    // Note: that this case is mostly just for completion
    // here, since with float-precision arithmetic there
    // is very little chance that disc. would be precisely 0.0
    if (discriminant == 0.0)  {
	double t = -b/(2*a);
	if (t>0) {
	    intersect_point = origin + look_direction * t;
	    return 1;
	}
	else
	    return 0;
    }

    // If discriminant is > 0, there are two solutions.
    // Since ray extends only in one direction(positive t)
    // the solutions with t < 0 have to be rejected.
    double t1 = (-b + sqrt(discriminant))/(2*a);
    double t2 = (-b - sqrt(discriminant))/(2*a);

    // If we have two solutions, the most positive t is the second one.

    if ( (t1 >= 0) && (t2 >= 0) )  {		//both positive or 0
        if ( t1 > t2) {
	    if (which_intercept == 2)
		intersect_point = origin + look_direction * t1; // most positive
	    else
		intersect_point = origin + look_direction * t2; // least pos
	  return 2;
	}
	else {
	    if (which_intercept == 2)
		intersect_point = origin + look_direction * t2; // most positive
	    else
		intersect_point = origin + look_direction * t1; // least pos
	  return 2;
	}
    }

    // if we are here we have at most one valid solution

    if (t1 >= 0.0) {
        intersect_point = origin + look_direction * t1;
	return 1;
    }

    if (t2 >= 0.0) {
        intersect_point = origin + look_direction * t2;
	return 1;
    }
    
    return 0;  // no valid solution exists.
        
}

////////////////////////////////////////////////////////////////////////
// Intersect a ray with Sphere, giving a 3-D world location.
// This is the main purpose of surface models.
//
// Return codes:
// 0 == no intersection
// >=1 == intersects surface at least once.  Use countIntersections() to
//      distinguish between intercepts, e.g. tangent and two-intercept
//      cases for spheres.  Subclasses may return the actual count if it
//      is easy to calculate (but should spend no computational effort to
//      do so; that's the purpose behind countIntersections()).
// A return code > 0 means that the returned point is valid.  For return
// codes <=0, the returned point is actually a unit vector pointing in
// the "infinity" direction (usually the same as the look vector since
// origin doesn't matter at infinity).
// Note that the caller must pass in the vector to be filled; this
// routine does not allocate it.  In the case of multiple intersections,
// it returns the "closest" one.
//
// For Sphere2, we return the 2nd intersection if there is more than one...
////////////////////////////////////////////////////////////////////////

int PigSurfaceSphere2::intersectRay(const PigPoint &origin,
				   const PigVector &look_direction,
				   PigPoint &intersect_point)
{
    return intersectRay(origin, look_direction, 2, intersect_point);
}

// Counts the number of times a ray hits the surface.
// Return codes:
// 0 == no intersection
// 1 == 1 intersect point
// 2 == 2 intersect points
int PigSurfaceSphere2::countIntersections(const PigPoint &origin,
		       const PigVector &look_direction)
{
    PigPoint intercept;			// thrown away

    return intersectRay(origin, look_direction, intercept);
}

////////////////////////////////////////////////////////////////////////
// Copy from instance vars to the label struct.  The write framework is
// handled by the superclass.
////////////////////////////////////////////////////////////////////////

void PigSurfaceSphere2::fillLabelStruct(LblSurfaceModel_typ *SurfaceModel) const
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
// Print the model in a readable form...  this works for Sphere1 as well
// as Sphere2.
////////////////////////////////////////////////////////////////////////

void PigSurfaceSphere2::print()
{
    char msg[512];
    sprintf(msg, "Surface: %s, center=(%f, %f, %f) radius=%f", getModelName(),
		_center.getX(), _center.getY(), _center.getZ(), _radius);
    printInfo(msg);
    sprintf(msg, "Surface: %s, ground=\\( %f, %f, %f \\) normal=\\( %f, 0, 0 \\)",
		getModelName(),
		_center.getX(), _center.getY(), _center.getZ(), _radius);
    printInfo(msg);
}

////////////////////////////////////////////////////////////////////////
// These functions allow access to the parameters, in order to allow
// "tweaks" to the surface model.
// Parameters are:
//    X, Y, Z, Radius
////////////////////////////////////////////////////////////////////////

void PigSurfaceSphere2::getPointingParameters(double params[],
							const int max_count)
{
    double p[3];
    _center.getXYZ(p);

    if (max_count >= 1)
	params[0] = p[0];
    if (max_count >= 2)
	params[1] = p[1];
    if (max_count >= 3)
	params[2] = p[2];
    if (max_count >= 4)
	params[3] = _radius;
}

void PigSurfaceSphere2::setPointingParameters(const double params[],
						const int count)
{
    // Note that there's no "re-pointing" involved here...

    double p[3];
    _center.getXYZ(p);

    if (count >= 1)
	p[0] = params[0];
    if (count >= 2)
	p[1] = params[1];
    if (count >= 3)
	p[2] = params[2];
    if (count >= 4)
	_radius = params[3];

    _center.setXYZ(p);
}

const char *const PigSurfaceSphere2::getPointingParamName(int i)
{
    switch (i) {
	case 0:
	    return "X";
	case 1:
	    return "Y";
	case 2:
	    return "Z";
	case 3:
	    return "Radius";
	default:
	    return "Unknown";
    }
}

void PigSurfaceSphere2::getPointingErrorEstimate(double errors[],
					const int max_count)
{
    int i;
    int n = max_count;
    if (n > 4) n = 4;		// only 4 params

    for (i=0; i < n; i++)
	errors[i] = .02;	// 2 cm error both in position and radius
}

