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
#ifndef PIGSURFACESPHERE2_H
#define PIGSURFACESPHERE2_H

#include "PigSurfaceModel.h"

class PigSurfaceSphere2 : public PigSurfaceModel {

  protected:

    double _radius;
    PigPoint _center;		// origin of the sphere

    virtual char *surfaceModelLabelType() const { return "SPHERE2"; }

    // Copy from instance vars to label struct
    virtual void fillLabelStruct(LblSurfaceModel_typ *lbl) const;

  public:

    PigSurfaceSphere2(char *mission, char *instrument, char *target);
    PigSurfaceSphere2(char *target, PigCoordSystem *cs);
    PigSurfaceSphere2(double &radius, PigCoordSystem *cs);
    PigSurfaceSphere2(double &radius, PigPoint &center, PigCoordSystem *cs);

    virtual ~PigSurfaceSphere2();

    // Convert to the given coord system

    virtual void setCoordSystem(PigCoordSystem *cs);

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
    ////////////////////////////////////////////////////////////////////////

    virtual int intersectRay(const PigPoint &origin,
			     const PigVector &look_direction,
			     PigPoint &intersect_point);

    // Like the above, except you can specify which of the n intercepts to
    // return (which_intercept is 1-based).  Return >=1 if the given intercept
    // is valid, or <=0 if it isn't.  For planes, only intercept 1 is valid.
    // The point is set for infinity, as above.

    virtual int intersectRay(const PigPoint &origin,
			     const PigVector &look_direction,
			     const int which_intercept,
			     PigPoint &intersect_point);


    // Counts the number of times a ray hits the surface.
    // Return codes:
    // 0 == no intersection
    // 1 == 1 intersect point
    // 2 == 2 intersect points

    virtual int countIntersections(const PigPoint &origin,
				   const PigVector &look_direction);

    // Print the surface model in a readable form

    virtual void print();

    // Accessors....

    virtual PigPoint getCenter() { return _center; }
    virtual double getRadius() { return _radius; }

    // The below functions come from PigAdjustable...
    // Parameters are: X, Y, Z, Radius

    virtual int getPointingParamCount() { return 4; }
    virtual void getPointingParameters(double params[], const int max_count);
    virtual void setPointingParameters(const double params[], const int count);
    virtual void getPointingErrorEstimate(double errors[], const int max_count);
    virtual const char *const getPointingParamName(int i);	 // 0-based

    virtual const char *const getModelName() { return "Sphere2"; }

};

#endif

