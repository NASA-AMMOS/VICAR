////////////////////////////////////////////////////////////////////////
// PigSurfaceInfinity
//
// Surface model for a "non-surface", which is at infinity everywhere.
////////////////////////////////////////////////////////////////////////
#ifndef PIGSURFACEINFINITY_H
#define PIGSURFACEINFINITY_H

#include "PigSurfaceModel.h"

class PigSurfaceInfinity : public PigSurfaceModel {

  protected:

    virtual char *surfaceModelLabelType() const { return "INFINITY"; }

  public:

    PigSurfaceInfinity(char *mission, char *instrument, char *target)
	: PigSurfaceModel(mission, instrument, target) { }
    PigSurfaceInfinity(char *target, PigCoordSystem *cs)
	: PigSurfaceModel(target, cs) { }
    PigSurfaceInfinity()
	: PigSurfaceModel(NULL, NULL) { }

    virtual ~PigSurfaceInfinity() { }

    // Note that the actuall coord system doesn't matter for the Infinity
    // surface, since the input vectors are simply returned.

    // Intersect a ray with the surface, giving a 3-D world location.
    // This is the main purpose of surface models.

    // Return codes:
    // 0 == no intersection (only case for infinity "surface")
    // The returned point is actually a unit vector pointing in
    // the "infinity" direction (the same as the look vector since origin
    // doesn't matter at infinity).
    // Note that the caller must pass in the vector to be filled; this
    // routine does not allocate it.

    virtual int intersectRay(const PigPoint &origin,
			     const PigVector &look_direction,
			     PigPoint &intersect_point)
	{ intersect_point = look_direction;  return 0; }

    // Like the above, except you can specify which of the n intercepts to
    // return (which_intercept is 1-based).  Since there are no intercepts,
    // return 0, with the point set to infinity, as above.

    virtual int intersectRay(const PigPoint &origin,
			     const PigVector &look_direction,
			     const int which_intercept,
			     PigPoint &intersect_point)
	{ intersect_point = look_direction;  return 0; }

    // Counts the number of times a ray hits the surface.
    // Return codes:
    // 0 == no intersection (only case for infinity "surface")

    virtual int countIntersections(const PigPoint &origin,
				   const PigVector &look_direction)
	{ return 0; }

    // Print the surface model in a readable form
    
    virtual void print() { printInfo("Infinity Surface Model"); }

    virtual const char *const getModelName() { return "Infinity"; }

};

#endif

