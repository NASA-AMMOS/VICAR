////////////////////////////////////////////////////////////////////////
// PigSurfacePlane
//
// Surface model for a flat, tilted plane going through the origin.
// The constructor takes the Normal to this plane.
////////////////////////////////////////////////////////////////////////
#ifndef PIGSURFACEPLANE_H
#define PIGSURFACEPLANE_H

#include "PigSurfaceModel.h"

class PigSurfacePlane : public PigSurfaceModel {

  protected:

    PigVector _normal;
    double _normal_params[3];
    PigPoint _ground;		// any point on the plane (its "origin")
    double _ground_params[3];

    virtual char *surfaceModelLabelType() const { return "PLANE"; }

    // Copy from the label struct to instance vars
    virtual void useLabelStruct(LblSurfaceModel_typ *lbl);

    // Copy from instance vars to label struct
    virtual void fillLabelStruct(LblSurfaceModel_typ *lbl) const;

  public:

    PigSurfacePlane(char *mission, char *instrument, char *target);
    PigSurfacePlane(char *target, PigCoordSystem *cs);
    PigSurfacePlane(PigVector &normal, PigCoordSystem *cs);
    PigSurfacePlane(PigVector &normal, PigPoint &ground, PigCoordSystem *cs);

    virtual ~PigSurfacePlane();

    virtual void setCoordSystem(PigCoordSystem *cs);

    // Intersect a ray with the surface, giving a 3-D world location.
    // This is the main purpose of surface models.

    // Return codes:
    // 0 == no intersection (ray parallel to plane)
    // 1 == intersection "in front"
    // -1 == intersects plane "behind" the camera.  There is no facility
    //      for retrieving "backwards" intercepts; just invert the look
    //      direction and try again.
    // A return code > 0 means that the returned point is valid.  For return
    // codes <= 0, the returned point is actually a unit vector pointing in
    // the "infinity" direction (the same as the look vector since origin
    // doesn't matter at infinity).
    // Note that the caller must pass in the vector to be filled; this
    // routine does not allocate it.  In the case of multiple intersections,
    // it returns the "closest" one.

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

    // Accessors....

    virtual PigPoint getGround() { return _ground; }
    virtual PigVector getNormal() { return _normal; }

    // The below functions come from PigAdjustable...
    // Parameters are: X, Y, Z, Normal_X, Normal_Y, Normal_Z
    // Note that the Normal's are normalized before use... but the params
    // themselves are not.  So you will get back the same value you set.
    // The accessors above return the normalized values.

    virtual int getPointingParamCount() { return 6; }
    virtual void getPointingParameters(double params[], const int max_count);
    virtual void setPointingParameters(const double params[], const int count);
    virtual void getPointingErrorEstimate(double errors[], const int max_count);
    virtual const char *const getPointingParamName(int i);       // 0-based

    virtual const char *const getModelName() { return "TiltedPlane"; }

};

#endif

