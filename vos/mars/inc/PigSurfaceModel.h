////////////////////////////////////////////////////////////////////////
// PigSurfaceModel
//
// Base class for Surface models.  This models a surface, and intersects
// rays with the surface.  Examples of surfaces include a sphere or ellipsoid
// for orbiter images, a flat plane for landers, or an actual terrain map.
// The factory functions return the "standard" surface model for the given
// mission/camera/target or image.  Often, specific subclasses will be
// instantiated for the desired terrain.
//
// Many surfaces have parameters associated with them.  
//
// TBD:  are surfaces time-dependent for rotating modies (i.e. relationship
// to J2000)?
//
// Surface models have a set of parameters that describe them.  These normally
// come from user parameters.  However, we now implement the Adjustable
// interface so that the surface model can be changed.
//
// HOWEVER... it is critically important for the CALLER to recompute anything
// that needs to be recomputed as a result of a surface model change.  The
// surface model classes themselves do not notify anyone of the change.
//
// Note that subclasses should provide query functions for their parameters,
// as a convenience.
////////////////////////////////////////////////////////////////////////
#ifndef PIGSURFACEMODEL_H
#define PIGSURFACEMODEL_H

#include "PigModelBase.h"
#include "PigAdjustable.h"
#include "PigVector.h"

#include "lbl_surface_model.h"

class PigFileModel;
class PigCoordSystem;

class PigSurfaceModel : public PigModelBase, public PigAdjustable {

  protected:

    char *_mission;		// These could very well be NULL...
    char *_instrument;
    char *_target;

    PigCoordSystem *_cs;

    PigSurfaceModel(char *mission, char *instrument, char *target);
    PigSurfaceModel(char *target, PigCoordSystem *cs);

    virtual char *surfaceModelLabelType() const = 0;	// returns key for label

    // Copy from the label struct to instance vars
    virtual void useLabelStruct(LblSurfaceModel_typ *lbl) { }

    // Copy from instance vars to label struct
    virtual void fillLabelStruct(LblSurfaceModel_typ *lbl) const { };

  public:

    // These factory methods create and return an instance of the "standard"
    // surface subclass for the given mission/instrument/target.  These
    // parameters are specified by the file itself (look at the label to figure
    // it out), or by the mission name/instrument/target name.  Applications
    // should feel free to instantiate their own specific subclasses for
    // specific desired surfaces rather than using these factories (as opposed
    // to the camera and pointing models, where the factories are almost
    // always used).
    // The default coord system will be the Fixed frame for the given mission.

    static PigSurfaceModel *create(const char *filename);
    static PigSurfaceModel *create(PigFileModel *file);
    static PigSurfaceModel *create(const char *mission, const char *instrument,
				   const char *target);
    static PigSurfaceModel *create(const char *mission, const char *instrument, const char *type, 
				   const double params[], const int count, PigCoordSystem *cs);

    virtual ~PigSurfaceModel();

    const char *getMissionName() const { return _mission; }
    const char *getInstrumentName() const { return _instrument; }
    const char *getTargetName() const { return _target; }

    // Subclasses should override setCoordSystem to transform internal vectors
    virtual void setCoordSystem(PigCoordSystem *cs) { _cs = cs; }
    PigCoordSystem *getCoordSystem() { return _cs; }

    // For efficiency (since this is called for every pixel of a mosaic),
    // the vectors are all ASSUMED to be already in the object's coordinate
    // system for ALL of the below routines.  Callers must make sure of this
    // beforehand.
//!!!!!!!!!!!!!!!!!!!!!! make this a parameter???? !!!!!!!!!!!!!!!!!!

    // Intersect a ray with the surface, giving a 3-D world location.
    // This is the main purpose of surface models.

    // Return codes:
    // 0 == no intersection
    // >=1 == intersects surface at least once.  Use countIntersections() to
    //      distinguish between intercepts, e.g. tangent and two-intercept
    //      cases for spheres.  Subclasses may return the actual count if it
    //      is easy to calculate (but should spend no computational effort to
    //      do so; that's the purpose behind countIntersections()).
    // -1 == intersects surface "behind" the camera.  There is no facility
    //      for retrieving "backwards" intercepts; just invert the look
    //      direction and try again.
    // A return code > 0 means that the returned point is valid.  For return
    // codes <= 0, the returned point is actually a unit vector pointing in
    // the "infinity" direction (usually the same as the look vector since
    // origin doesn't matter at infinity).
    // Note that the caller must pass in the vector to be filled; this
    // routine does not allocate it.  In the case of multiple intersections,
    // it returns the "closest" one.

    virtual int intersectRay(const PigPoint &origin,
			     const PigVector &look_direction,
			     PigPoint &intersect_point) = 0;

    // Like the above, except you can specify which of the n intercepts to
    // return (which_intercept is 1-based).  Return >=1 if the given intercept
    // is valid, or <=0 if it isn't.  The point is set for infinity, as above.

    virtual int intersectRay(const PigPoint &origin,
			     const PigVector &look_direction,
			     const int which_intercept,
			     PigPoint &intersect_point) = 0;

    // Counts the number of times a ray hits the surface.
    // Return codes:
    // 0 == no intersection
    // 1 == 1 intersect point (e.g. plane or tangent to sphere)
    // 2 == 2 intersect points (e.g. front and back of sphere)
    // +n == intersects surface n times (could happen for irregular bodies)
    // -1 == intersects surface "behind" the camera.  There is no facility
    //       for retrieving "backwards" intercepts; just invert the look
    //       direction and try again.

    virtual int countIntersections(const PigPoint &origin,
				   const PigVector &look_direction) = 0;

    // This is basically the inverse of intersectRay().  Don't know if it's
    // useful.  Returns the look direction unit vector given the origin and
    // an XYZ point on the surface.  The base class implements this via
    // simple subtraction and normalization of the points, but e.g. a celestial
    // sphere might behave differently.  If infinity_flag is true, the given
    // point *is* the look vector (as returned by intersectRay()), and this
    // is just returned.

    virtual void getRay(const PigPoint &origin,
			const PigPoint &intersect_point,
			const int infinity_flag,
			PigVector &look_direction);

    // I/O routines return 0 on success, non-0 on failure.
    // The label routines expect a VICAR unit number, already open in the
    // proper mode (read/write).
    // For writeToLabel, the caller may fill in fields in the aux
    // structure if desired, which are not otherwise set by the label
    // writer.  Set the Valid flag for any fields set.  Defaults are
    // provided for required fields.  aux may be passed in as NULL.
    //!!!!!!!!!!!!!!!!!!! coord systems for i/o routines?

    virtual int readFromLabel(int unit, int instance);
    virtual int writeToLabel(int unit, int instance,
				const LblSurfaceModel_typ *aux) const;

    // This factory method creates and returns a surface model from a label.
    // It differs from readFromLabel because that is subclassed and expectsr
    // a specific type of SM.  This factory looks to see what kind of SM to
    // create first, then creates the appropriate type, and calls readFromLabel
    // to fill it in.
    // Returns NULL if the model could not be read for some reason.

    static PigSurfaceModel *createFromLabel(PigFileModel *file);

    // Print the surface model in a readable form

    virtual void print() { printInfo("Surface Model Base (should not see this)"); }

    // The below functions come from PigAdjustable... and are appropriate for
    // no-parameter cases such as Infinity.

    virtual int getPointingParamCount() { return 0; }
    virtual void getPointingParameters(double params[], const int max_count) { }
    virtual void setPointingParameters(const double params[], const int count)
                        { }
    virtual void getPointingErrorEstimate(double errors[], const int max_count)
                        { }
    virtual const char *const getPointingParamName(int i)	// 0-based
                        { return ""; }

    virtual const char *const getModelName() { return "Base"; }

};

#endif

