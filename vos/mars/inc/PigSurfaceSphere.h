////////////////////////////////////////////////////////////////////////
// PigSurfaceSphere
//
// Surface model for a sphere of a given radius, centered on the origin.
// In other words, rays are assumed to emanate from the sphere's center;
// other radiants are not supported.  See PigSurfaceSphere1 or
// PigSurfaceSphere2 for that functionality.
////////////////////////////////////////////////////////////////////////
#ifndef PIGSURFACESPHERE_H
#define PIGSURFACESPHERE_H

#include "PigSurfaceModel.h"

class PigSurfaceSphere : public PigSurfaceModel {

  protected:

    double _radius;
    PigPoint _center;		// origin of the sphere
    int    _isCenterDefined;

    virtual char *surfaceModelLabelType() const { return "SPHERE"; }

    // Copy from instance vars to label struct
    virtual void fillLabelStruct(LblSurfaceModel_typ *lbl) const;

  public:

    PigSurfaceSphere(char *mission, char *instrument, char *target);
    PigSurfaceSphere(double &radius, PigCoordSystem *cs);
    PigSurfaceSphere(double &radius, PigPoint &center, PigCoordSystem *cs);

    virtual ~PigSurfaceSphere();

    // Convert to the given coord system

    virtual void setCoordSystem(PigCoordSystem *cs);

    // Intersect a ray with the surface, giving a 3-D world location.
    // This is the main purpose of surface models.

    // Return codes:
    // 0 == no intersection (ray parallel to sphere)
    // 1 == intersection "in front"
    // -1 == intersects sphere "behind" the camera.  There is no facility
    //      for retrieving "backwards" intercepts; just invert the look
    //      direction and try again.
    // A return code > 0 means that the returned point is valid.  For return
    // codes <= 0, the returned point is actually a unit vector pointing in
    // the "infinity" direction (the same as the look vector since origin
    // doesn't matter at infinity).
    // Note that the caller must pass in the vector to be filled; this
    // routine does not allocate it.  In the case of multiple intersections,
    // it returns the "closest" one.

    // Note that currently only special case when Ray and Sphere origins 
    // coinside is supported.  That implies that the return code is always 1

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
    // -1 == intersects surface "behind" the camera.  There is no facility
    //       for retrieving "backwards" intercepts; just invert the look
    //       direction and try again.
    // Not too exciting for this subclass, since there will always be 0 or 1
    // (or -1).
    virtual int countIntersections(const PigPoint &origin,
				   const PigVector &look_direction);

    // Print the surface model in a readable form

    virtual void print();

    // Accessor....

    virtual double getRadius() { return _radius; }

    // The below functions come from PigAdjustable...
    // Parameters are: Radius

    virtual int getPointingParamCount() { return 1; }
    virtual void getPointingParameters(double params[], const int max_count);
    virtual void setPointingParameters(const double params[], const int count);
    virtual void getPointingErrorEstimate(double errors[], const int max_count);
    virtual const char *const getPointingParamName(int i);	 // 0-based

    virtual const char *const getModelName() { return "Sphere"; }

};

#endif

