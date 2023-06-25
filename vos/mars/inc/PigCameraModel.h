////////////////////////////////////////////////////////////////////////
// PigCameraModel
//
// Base class for Camera models.  Responsible for maintaining calibration
// information for a camera, applying pointing when requested, and doing
// the actual ray projection calculations.
//
// Calibration parameters might consist of initial CAHVOR parameters for that
// model, or things like focal length, boresight offset, and radial distiortion
// for an orbiter model.
//
// One and only one PointingModel can be attached to the camera model at a
// time, although they can be switched out if desired.
//
// The CameraModel maintains an initial, and a current, coordinate system.
// The caller who sets the camera parameters (via subclass functions) must
// set the initial coordinate system as well.  This may be changed via
// setCoordSystem(), but is restored by resetCameraLocation().
//
// In addition, both an initial and current set of parameters are maintained.
// The current set are the ones that should be used at all times.  The
// Initial set stores the parameters from a calibration model.  Certain
// users will reset pointing to the Initial set (via resetCameraLocation())
// to get to a known initial state.  The current parameters may be
// transferred to the initial set via setInitialFromCurrent().
////////////////////////////////////////////////////////////////////////
#ifndef PIGCAMERAMODEL_H
#define PIGCAMERAMODEL_H

#include <string.h>
#include "PigModelBase.h"
#include "PigVector.h"

#include "lbl_camera_model.h"
#include "PigQuaternion.h"

class PigQuaternion;
class PigFileModel;
class PigPointingModel;
class PigCoordSystem;

#define PIG_MAX_INTERP_CAMERA_MODELS	32	// See CMOD_INTERP_MAX_MODELS

enum PigCmodWarpAlgorithm
	{ CMOD_WARP_NONE, CMOD_WARP_1, CMOD_WARP_2, CMOD_WARP_PSPH };

enum PigCmodType
	{ CMOD_TYPE_NONE = 0,
	  CMOD_TYPE_CAHV = 1,
	  CMOD_TYPE_CAHVOR = 2,
	  CMOD_TYPE_CAHVORE = 3,
	  CMOD_TYPE_PSPH = 4 };

class PigCameraModel : public PigModelBase {

  protected:
    char *_mission;		// These could very well be NULL...
    char *_instrument;
    char *_version;
    char *_subtype;
    char *_construction;
    char *_calibration;

    PigPointingModel *_pointing_model;

    PigCoordSystem *_initial_cs;
    PigCoordSystem *_current_cs;

    // Additional fields carried forward from label (or possibly modified,
    // in the case of the interps) that aren't part of the model per se.

    int _transformVectorValid;		// whether or not to write transform
    PigVector _transformVector;
    int _transformQuatValid;
    PigQuaternion _transformQuat;
    int _interpMethodValid;		// whether or not to write interp
    char _interpMethod[64];
    int _interpValueValid;
    double _interpValue;

    PigCameraModel(const char *mission, const char *instrument,
		   const char *version, const char *subtype, 
		   const char *construction, const char *calibration);
    PigCameraModel();

    // Get the algorithm to use for epipolar alignment.  There is a default
    // per mission (in PigMission) but this can be overridden via CMOD_WARP=n
    // in POINT_METHOD.  Currently the values are:
    // 1 = old model, used by MER and MSL since 2012
    // 2 = new model (2017), which works better for non-traditional stereo
    // 3 or PSPH = aligns to (and returns) PSPH model
    // Subclasses should NOT need to override this function
    virtual PigCmodWarpAlgorithm getCmodWarpAlgorithm();

  public:

    virtual ~PigCameraModel();

    // These factory methods create and return an instance of the
    // proper subclass for the given camera.  Cameras are specified
    // by the file itself (look at the label to figure it out), or by
    // the mission name/instrument name/camera id/subtype(often a filter name).
    // The "special" string, if present, allows for standard variations
    // recognized by most subclasses (return NULL if the string is unknown):
    //    "stereo" - create the "stereo partner" for the specified camera,
    //			if it is stereo.  If not, return NULL.
    // If the PigCoordSystem is NULL in the string version, the "natural"
    // frame for the given mission/instrument, at the default site, are used.

    static PigCameraModel *create(const char *filename, const char *special);
    static PigCameraModel *create(PigFileModel *file, const char *special);
    static PigCameraModel *create(const char *mission, 
				  const char *instrument,
				  const char *version, 
				  const char *subtype,
				  const char *special, 
				  const char *construction,
				  const char *calibration, 
				  PigCoordSystem *cs);

    // These factory methods create and return a camera model from a label
    // or file.  They differ from readFromFile et al because those are
    // subclassed and expect a specific type of CM.  These factories look to
    // see what kind of CM to create first, then create the appropriate type,
    // and call the readFromFile et al routines to fill it in.

    static PigCameraModel *createFromLabel(PigFileModel *file,
				const char *name,
				const char *mission,
				const char *instrument,
				const char *version,
				const char *subtype,
				const char *construction,
				const char *calibration,
				PigCoordSystem *cs);

    static PigCameraModel *createFromLabel(PigFileModel *file,
				int instance,
				const char *mission,
				const char *instrument,
				const char *version,
				const char *subtype,
				const char *construction,
				const char *calibration,
				PigCoordSystem *cs);

    static PigCameraModel *createFromFile(const char *filename,
				const char *mission,
				const char *instrument,
				const char *version,
				const char *subtype,
				const char *construction,
				const char *calibration,
				PigCoordSystem *cs);

    // I/O routines return 0 on success, non-0 on failure.
    // The label routines expect a VICAR unit number, already open in the
    // proper mode (read/write).
    // For writeToLabel, the caller may fill in certain fields in the aux
    // structure if desired:  CalibrationSourceId, CameraModelDesc,
    // CameraModelName, GeometrySourceId.  Set the Valid flag for any
    // fields set.  Defaults are provided for required fields.  aux may
    // be passed in as NULL.
    //
    // Coordinate systems are NOT set by the read routines; the vectors are
    // converted as needed.  If the file does not contain CS information,
    // the CS is specified by the parameter.

    virtual int readFromFile(const char *filename, PigCoordSystem *cs) = 0;
    virtual int writeToFile(const char *filename) const = 0;
    virtual int readFromLabel(PigFileModel *file, int instance) = 0;
    virtual int writeToLabel(int unit, int instance, PigCoordSystem *cs,
			     const LblCameraModel_typ *aux) const= 0;
    // replaces the method above as a way to populate Camera Model fields
    // in the label.  Does NOT actually write to the label.
    virtual void setupWriteToLabel(LblCameraModel_typ &CM, PigCoordSystem *cs)
					const = 0;
    // These routines "serialize" the contents of the camera model to/from
    // a string.  Note that only specific CM parameters are saved... things
    // like coordinate systems, mission name, pointing model pointers, etc.
    // are NOT saved.  The string should be human readable, if not necessarily
    // *easy* to read.  They return 0 on success.
    //
    // The type of camera model followed by ':' should be at the front of
    // the string, e.g. "CAHVOR:...".
    //
    // For writeToString, the string must be pre-allocated, with max_length
    // indicating its size.

    virtual int readFromString(const char *string, PigCoordSystem *cs) = 0;
    virtual int writeToString(char *string, int max_length) = 0;

    virtual const char *getMissionName() const { return _mission; }
    virtual const char *getInstrumentName() const { return _instrument; }
    virtual const char *getCameraVersion() const { return _version; }
    virtual const char *getCameraSubtype() const { return _subtype; }
    virtual const char *getCameraConstruction() const { return _construction; }
    virtual const char *getCameraCalibration() const { return _calibration; }



    // Set or get the pointing model.  Usually done by the PM itself.

    virtual void setPointingModel(PigPointingModel *pm);
    virtual PigPointingModel *getPointingModel() { return _pointing_model; }

    // Set or get the coordinate systems.  The Initial versions should
    // only be called by the PigMission create routines.  The Set routines
    // must be overridden by subclasses to change all internal vectors
    // and points to match.

    virtual void setInitialCoordSystem(PigCoordSystem *cs) = 0;
    virtual void setCoordSystem(PigCoordSystem *cs) = 0;

    // This version sets the initial CS but DOES NOT CHANGE any internal
    // vectors.  This is useful when you know the Site changed but the
    // underlying parameters really didn't.
    virtual void setInitialCoordSystemNoTrans(PigCoordSystem *cs)
	{ _initial_cs = cs;  _current_cs = cs; }

    PigCoordSystem *getInitialCoordSystem() { return _initial_cs; }
    PigCoordSystem *getCoordSystem() { return _current_cs; }

    // Relocate a camera model, based on initial and final positions and
    // orientations of a reference point, which is rigidly connected to the
    // camera but is otherwise arbitrary.  Usually this will be the axis
    // of rotation for the actuators.  This should normally be called only
    // by a PointingModel.  The PigCoordSystem indicates what the
    // four given parameters are measured in; they are converted internally
    // to match the current CS as necessary.

    virtual void moveCamera(const PigPoint initial_point,
			    const PigQuaternion initial_orientation,
			    const PigPoint final_point,
			    const PigQuaternion final_orientation,
			    PigCoordSystem *cs) = 0;

    // This function scales a camera model. The scale factors should be
    // understood as the same scale factors that would be applied to a 2D
    // coordinate in the original model to convert it to a coordinate in
    // the resulting model.
    // Note that the CURRENT values of the camera model are modified, not
    // the initial ones.

    virtual void scaleCamera(const double x_factor, 
				 const double y_factor) = 0;

    // This function shifts a camera model. The shift values should be
    // understood as the coordinates of the original model where the origin
    // will fall in the new one.  In another words first_line, first_sample
    // is the start of the image w.r.t. the Full Frame.

    virtual void shiftCamera(const double dx, 
				 const double dy) = 0;
 
    // Aligns the model with it's stereo partner(other) for use with stereo 
    // imagery.  The output model should retain the location of the original, 
    // but should be pointing parallel to the other, at the "average" look 
    // direction of the two.  This essentially removes toe-in and other nasty 
    // physical non-alignments of stereo cameras, giving a nice, parallel look
    // direction to both cameras.
    // It should also be able to "align" pictures taken from the same camera
    // at different times, or wildly different look angles (to some extent).
    //
    // If a given camera model doesn't know what to do with this, or if the
    // model types don't match (sometimes a requirement), then simply warp
    // "this" model to a linear model and return it, ignoring "other".
    //
    // x/ydim_in1 refers to "this" model, x/ydim_in2 refers to "other".
    // If the "other" size is not known, pass in the same for 1 and 2.
    //
    // See the PigCAHV implementation for more info.

    virtual PigCameraModel *alignStereoCameras(int xdim_in1, int ydim_in1,
					       int xdim_in2, int ydim_in2,
					       int xdim_out, int ydim_out,
					       PigCameraModel *other) = 0;

    // Resets the camera pointing to the "reference" location (usually
    // where the calibration was taken), in preparation for a new pointing.
    // Also sets the coordinate system to the initial one.

    virtual void resetCameraLocation() = 0;

    // Copy the Current parameters to the Initial ("reference") model
    // tha tis used by resetCameraLocation().  This is often used to
    // modify the initial model for downsample/subframe before pointing
    // takes place.

    virtual void setInitialFromCurrent() = 0;

    // Retrieves the image-space coordinates (LS below) of the "pointing
    // axis" of the camera.  For orbiters, this is normally defined to be
    // 0,0, but for CAHV-type cameras, the 0,0 point is encoded in the H and
    // V vectors, so the center is not at 0,0.  This can be used to relate
    // "logical" LS coordinates to the physical coordinates of a file.
    // Coordinates are assumed to be 0-based (unlike VICAR files).

    virtual void getCameraCenter(double &line, double &sample) const
		{ line = 0.0;  sample = 0.0; }

    // Returns the angle subtended by a pixel, in either the line or sample
    // directions (normally these will be the same).  The angle is in radians,
    // so the units are radians/pixel.  If this is not constant across the
    // camera, the angle for the central pixel (looking down the pointing
    // vector) is returned.  If "which" is 0, the Line direction is returned;
    // 1 returns the Sample direction.

    virtual double getPixelAngle(const int which) const { return 0.0; }

    // These functions are the whole purpose behind this stuff.  Translate
    // a line/samp into an origin and look direction vector, and from a
    // 3D point into a line/samp location.  Note that the surface is not
    // involved here; use the look vector with a SurfaceModel, or use a
    // GeometryModel to do it all at once.  Note that the caller must
    // pass in vector objects to be filled; these routines don't allocate
    // them.  For the XYZtoLS case, if infinity_flag is true, the xyz point
    // should actually be a unit vector pointing in the infinity direction,
    // as returned by PigSurfaceModel::intersectRay().
    //
    // Note:  The passed-in coordinate system defines the input and output
    // CS for the vectors.  They are converted to/from the internal CS as
    // needed.  For efficiency, this should match the camera model's CS.

    virtual void LStoLookVector(const double line, const double sample,
				PigPoint &origin, PigVector &look_direction,
				PigCoordSystem *cs) const = 0;
    virtual void XYZtoLS(const PigPoint &xyz, const int infinity_flag,
			 double *line, double *sample, PigCoordSystem *cs)
		const = 0;
    virtual void XYZtoLS(const PigPoint &xyz, const int infinity_flag,
			 double *line, double *sample, PigCoordSystem *cs,
                         double *range, double par[2][3]) const = 0;
    
    // Given FOV angles in both line and sample directions,
    // return LS min/max values. FOV angles should be 
    // specified in degrees. min/max parameters are both input/output.  
    // That way the implementation of this API can use input min/max
    // to determine image size.

    virtual void getMinMaxLS(double fov_h, double fov_v, 
			     double *x_min, double *x_max,
			     double *y_min, double *y_max) const = 0;

    // These functions should *ONLY* be called by the corresponding
    // PointingModel!!  They generally mirror the similar functions in PM,
    // but calling the PM's version allows the pointing model to do mission-
    // specific pointing corrections or sanity checks.  See the comments
    // in PigPointingModel for descriptions.  They are all returned in the
    // current coordinate system.

    virtual PigPoint getCameraPosition() { return PigPoint(0.0, 0.0, 0.0); }
    virtual PigVector getCameraOrientation() { return PigVector(1.0,0.0,0.0); }
    virtual double getCameraTwist() { return 0.0; }

    virtual void setCameraPosition(PigPoint position,
		PigCoordSystem *cs) { }
    virtual void setCameraOrientation(PigVector orientation,
		PigCoordSystem *cs) { }
    virtual void setCameraTwist(double twist) { }

    // Interpolate this model based on the provided list.  Returns 1 on
    // success, 0 on failure.
    // Note: the CAHV implementation assumes that all models are expressed
    // in the same coordinate system!!

    virtual int interpolateModels(int n_models, PigCameraModel *interp_models[],
				double interp_values[], double value)
		{ return 0; }

    // clone() returns a copy of this camera model, although it's not connected
    // to any pointing model.  Subclasses should override to create the correct
    // subclass type.  copy(m) copies the contents of m into the model;
    // subclasses call superclass copy before copying their own fields.

    virtual PigCameraModel *clone()
	{   return NULL;		// can't clone the abstract base class!
	}
    virtual void copy(PigCameraModel *m)
	{   if (m->_mission) _mission = strdup(m->_mission);
	    if (m->_instrument) _instrument = strdup(m->_instrument);
	    if (m->_version) _version = strdup(m->_version);
	    if (m->_subtype) _subtype = strdup(m->_subtype);
	    if (m->_construction) _construction = strdup(m->_construction);
	    if (m->_calibration) _calibration = strdup(m->_calibration);
	    _pointing_model = NULL;
	    _initial_cs = m->_initial_cs;
	    _current_cs = m->_current_cs;
	    m->transferMetadata(this);
	}

    // This is an UNSAFE function.  It must be called only by the PigMission
    // object that created the CM because it's the only one that knows if it
    // is safe to change the Construction or not.

    void setConstruction(const char *construction);

    // Accessors for the additional fields.  These are not part of the model
    // per se but say something about how it is constructed.  The get's
    // return the valid flag with the value put in the argument.
    //!!!! Note: this stuff is not being transferred from the input models
    //!!!! to the output model when the model is recomputed.  Probably need
    //!!!! to make an ancillary structure to hold metadata that's passed
    //!!!! around in the camera model create routines.

    virtual void setTransformVector(PigVector v);
    virtual void setTransformQuaternion(PigQuaternion q);
    virtual int getTransformVector(PigVector &v);
    virtual int getTransformQuaternion(PigQuaternion &q);

    virtual void setInterpType(const char *type);
    virtual int getInterpType(char *type);	// must be big enough
    virtual void setInterpValue(double val);
    virtual int getInterpValue(double &val);

    // Transfer the additional fields (above) to another model

    virtual void transferMetadata(PigCameraModel *to);

    // This really should be protected but C++ won't allow access that
    // way.  So this should not be called in user code.
    // Return cahvore-equivalent vectors.  For non-cahv style models,
    // either fill in with a translation or just use the base class'
    // null implementation.  CS translation is done if needed.
    virtual void get_cahvore_vectors(double c[3], double a[3],
	double h[3], double v[3], double o[3], double r[3], double e[3],
	int &mtype, double &mparm, PigCmodType &type, PigCoordSystem *cs)
	{ memset(c, 0, sizeof(double)*3);
	  memset(a, 0, sizeof(double)*3);
	  memset(h, 0, sizeof(double)*3);
	  memset(v, 0, sizeof(double)*3);
	  memset(o, 0, sizeof(double)*3);
	  memset(r, 0, sizeof(double)*3);
	  memset(e, 0, sizeof(double)*3);
	  mtype = 1; mparm = 0;
	  type = getModelType();
	}
    // Same but for the Initial vectors
    virtual void get_initial_cahvore_vectors(double c[3], double a[3],
	double h[3], double v[3], double o[3], double r[3], double e[3],
	int &mtype, double &mparm, PigCmodType &type, PigCoordSystem *cs)
	{ memset(c, 0, sizeof(double)*3);
	  memset(a, 0, sizeof(double)*3);
	  memset(h, 0, sizeof(double)*3);
	  memset(v, 0, sizeof(double)*3);
	  memset(o, 0, sizeof(double)*3);
	  memset(r, 0, sizeof(double)*3);
	  memset(e, 0, sizeof(double)*3);
	  mtype = 1; mparm = 0;
	  type = getModelType();
	}

    virtual const char *const getModelName() { return "Base"; }
    virtual PigCmodType getModelType() { return CMOD_TYPE_NONE; }

};

#endif

