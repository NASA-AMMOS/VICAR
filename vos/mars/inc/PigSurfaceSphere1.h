////////////////////////////////////////////////////////////////////////
// PigSurfaceSphere1
//
// Surface model for a Sphere described by its Center and Radius.
// The main purpose of this model, method intersectRay() returns 
// closest (first) intersection for the case when ray crosses
// the sphere twice.
// The "first intersection point" model is useful to model convex
// surfaces, like hills, when the camera point is outside looking in.
// It returns the nearest (first) ray-surface intersection point.  For
// the cases where camera is inside the sphere, there is only a single
// intersection point and both SPHERE1 and SPHERE2 behave exactly the
// same.  This case is unlikely to occur with a "hill" model.
////////////////////////////////////////////////////////////////////////
#ifndef PIGSURFACESPHERE1_H
#define PIGSURFACESPHERE1_H

#include "PigSurfaceSphere2.h"

// 2 should be derived from 1 from an aesthetic standpoint, but 2 was
// implmented first so it wins...
//
class PigSurfaceSphere1 : public PigSurfaceSphere2 {

  public:

    PigSurfaceSphere1(char *mission, char *instrument, char *target) :
	    		PigSurfaceSphere2(mission, instrument, target) { }
    PigSurfaceSphere1(char *target, PigCoordSystem *cs) :
	    		PigSurfaceSphere2(target, cs) { }
    PigSurfaceSphere1(double &radius, PigCoordSystem *cs) :
	    		PigSurfaceSphere2(radius, cs) { }
    PigSurfaceSphere1(double &radius, PigPoint &center, PigCoordSystem *cs) :
	    		PigSurfaceSphere2(radius, center, cs) { }

    virtual ~PigSurfaceSphere1() { }

    // Everything is the same as Sphere2 except...
 
    virtual int intersectRay(const PigPoint &origin,
			     const PigVector &look_direction,
			     PigPoint &intersect_point)
	    { return PigSurfaceSphere2::intersectRay(origin, look_direction,
			   			 1, intersect_point); }

    // stupid compilers...
    virtual int intersectRay(const PigPoint &origin,
		const PigVector &look_direction, const int which_intercept,
		PigPoint &intersect_point)
	{ return PigSurfaceSphere2::intersectRay(origin, look_direction,
		which_intercept, intersect_point); }

    virtual const char *const getModelName() { return "Sphere1"; }

    virtual char *surfaceModelLabelType() const { return "SPHERE2"; }
};

#endif

