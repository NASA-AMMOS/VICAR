////////////////////////////////////////////////////////////////////////
// PigSurfaceMesh
//
// Surface model for a Mesh described by its .OBJ file.
// The main purpose of this model, method intersectRay() returns 
// closest intersection with the mesh.
////////////////////////////////////////////////////////////////////////
#ifndef PIGSURFACEMESH_H
#define PIGSURFACEMESH_H

#include "PigMission.h"

// Mesh support activated depending on OS arch -  See comment in PigMission.h
// Pig supports .obj mesh for surface model. The ray tracer used internally to
// intersect the mesh with viewing pixels is handled by the Intel Embree lib, 
// which is only available on 64-bit OSes. The mesh support is activated 
// accordingly. See imake_util.tmp and imake.config
#if USES_PIG_MESH

#include "PigSurfaceModel.h"
#include "meshman.h"

class PigSurfaceMesh : public PigSurfaceModel {

  protected:

    MeshMan * _mesh = nullptr;

    char * meshName = nullptr;

    virtual char *surfaceModelLabelType() const { return "MESH"; }

    // Copy from instance vars to label struct
    virtual void fillLabelStruct(LblSurfaceModel_typ *lbl) const;

    // Copy from the label struct to instance vars
    virtual void useLabelStruct(LblSurfaceModel_typ *lbl);

  public:

    PigSurfaceMesh(char *mission, char *instrument, char *target);
    PigSurfaceMesh(char *target, PigCoordSystem *cs);
    PigSurfaceMesh(PigCoordSystem *cs, char *meshFile);

    virtual ~PigSurfaceMesh();

    // Convert to the given coord system

    virtual void setCoordSystem(PigCoordSystem *cs);

    ////////////////////////////////////////////////////////////////////////
    // Intersect a ray with Mesh, giving a 3-D world location.
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
    virtual int countIntersections(const PigPoint &origin,
				   const PigVector &look_direction);

    // Print the surface model in a readable form

    virtual void print();

    // Accessors....

//    virtual PigPoint getCenter() { return _center; }
//    virtual double getRadius() { return _radius; }

    // The below functions come from PigAdjustable...
    // Parameters are: X, Y, Z, Radius

    virtual int getPointingParamCount() { return 0; }
    virtual void getPointingParameters(double params[], const int max_count);
    virtual void setPointingParameters(const double params[], const int count);
    virtual void getPointingErrorEstimate(double errors[], const int max_count);
    virtual const char *const getPointingParamName(int i);	 // 0-based

    virtual const char *const getModelName() { return "Mesh"; }

};

#endif
#endif
