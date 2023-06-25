////////////////////////////////////////////////////////////////////////
// PigPSPH
//
// Implements the PSPH camera model.
////////////////////////////////////////////////////////////////////////
#ifndef PIGPSPH_H
#define PIGPSPH_H

#include "PigCameraModel.h"
#include "PigVector.h"

#define	PI (3.14159265358979323846)
#define LIM_FOV (.98*PI)
#define MIN_FOV 1

struct cmod_t;
struct cmod_psph_t;

class PigPSPH : public PigCameraModel {

  protected:
    int _psph_set;

    // We could use cmod_psph_t here to hold this stuff, and probably should
    // for efficiency.  However, that mean #include'ing the cmod .h files
    // which would expose them to all users of this class, violating
    // information hiding principles.  Weak argument, but..

    PigPoint _orig_c;
    PigVector _orig_ax;
    PigVector _orig_ay;
    PigVector _orig_nx;
    PigVector _orig_ny;
    double _orig_sx;
    double _orig_sy;

    PigPoint _current_c;
    PigVector _current_ax;
    PigVector _current_ay;
    PigVector _current_nx;
    PigVector _current_ny;
    double _current_sx;
    double _current_sy;

    int _xdim;
    int _ydim;
    int _dims_set;		// true if xdim/ydim are set

    // Create a cmod_psph_t structure for use in calling the cmod routines,
    // or move a struct back in.
    virtual void fill_psph_t(cmod_psph_t *psph) const;
    virtual void empty_psph_t(cmod_psph_t *psph);

  public:

    PigPSPH(const char *mission, const char *instrument,
			const char *version, const char *subtype,
			const char *construction, const char *calibration);
    PigPSPH();

    virtual ~PigPSPH();

    // I/O routines return 0 on success, non-0 on failure
    // The label routines expect a VICAR unit number, already open in the
    // proper mode (read/write).
    // For writeToLabel, the caller may fill in certain fields in the aux
    // structure if desired:  CalibrationSourceId, CameraModelDesc,
    // CameraModelName, GeometrySourceId.  Set the Valid flag for any
    // fields set.  Defaults are provided for required fields.  aux may
    // be passed in as NULL.
    //
    // Coordinate systems are NOT set by the read routines; the vectors
    // are converted as needed.  Since the cahv file format does not contain
    // CS information, the parameter is used to specify the frame.

    virtual int readFromFile(const char *filename, PigCoordSystem *cs);
    virtual int writeToFile(const char *filename) const;
    virtual int readFromLabel(PigFileModel *file, int instance);
    virtual int writeToLabel(int unit, int instance, PigCoordSystem *cs,
			     const LblCameraModel_typ *aux) const;
    // Does the PSPH part of the label work
    virtual void setupWriteToLabel(LblCameraModel_typ &CM, PigCoordSystem *cs)
                                   const;

    // These routines "serialize" the contents of the camera model to/from
    // a string.  Note that only specific CM parameters are saved... things
    // like coordinate systems, mission name, pointing model pointers, etc.
    // are NOT saved.  The string should be human readable, if not necessarily
    // *easy* to read.  They return 0 on success.
    //
    // The type of camera model followed by ':' should be at the front of
    // the string, e.g. "PSPH:...".
    //
    // For writeToString, the string must be pre-allocated, with max_length
    // indicating its size.

    virtual int readFromString(const char *string, PigCoordSystem *cs);
    virtual int writeToString(char *string, int max_length);

    // Set the coordinate systems

    virtual void setInitialCoordSystem(PigCoordSystem *cs);
    virtual void setCoordSystem(PigCoordSystem *cs);

    // Set the initial PSPH vectors.  Note that the cs argument defines
    // what the given vectors are measured in... it does NOT change
    // the coordinate system definition of the object.  To do that, callers
    // must set the CS first, then set the PSPH vectors.

    virtual void setInitialPSPH(const PigPoint c,
				const PigVector ax, const PigVector ay,
				const PigVector nx, const PigVector ny,
				double sx, double sy,
				PigCoordSystem *cs);
    virtual void setInitialPSPH(const double c[3],
				const double ax[3], const double ay[3],
				const double nx[3], const double ny[3],
				double sx, double sy,
				PigCoordSystem *cs);

    // Set the current PSPH vectors.  Note that the cs argument defines
    // what the given vectors are measured in... it does NOT change
    // the coordinate system definition of the object.  To do that, callers
    // must set the CS first, then set the PSPH vectors.

    virtual void setCurrentPSPH(const PigPoint c,
				const PigVector ax, const PigVector ay,
				const PigVector nx, const PigVector ny,
				double sx, double sy,
				PigCoordSystem *cs);
    virtual void setCurrentPSPH(const double c[3],
				const double ax[3], const double ay[3],
				const double nx[3], const double ny[3],
				double sx, double sy,
				PigCoordSystem *cs);

    // Get the current PSPH vectors

    virtual void getCurrentPSPH(PigPoint &c, PigVector &ax, PigVector &ay,
				PigVector &nx, PigVector &ny,
				double &sx, double &sy);

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
			    PigCoordSystem *cs);

    // This function scales a camera model. The scale factors should be
    // understood as the same scale factors that would be applied to a 2D
    // coordinate in the original model to convert it to a coordinate in
    // the resulting model.

    virtual void scaleCamera(const double x_factor, 
			     const double y_factor);

    // This function shifts a camera model. The shift values should be
    // understood as the coordinates of the original model where the origin
    // will fall in the new one.  In another words first_line, first_sample
    // is the start of the image w.r.t. the Full Frame.

    virtual void shiftCamera(const double dx, 
			     const double dy);

    // Aligns the model with another for use with stereo imagery.
    //
    // PSPH does not support this, so we simply return a clone of the
    // original model.

    virtual PigCameraModel *alignStereoCameras(int xdim_in, int ydim_in,
					       int xdim_out, int ydim_out,
					       PigCameraModel *other);

    // Resets the camera pointing to the "reference" location (usually
    // where the calibration was taken), in preparation for a new pointing.

    virtual void resetCameraLocation();

    // Copy the Current parameters to the Initial ("reference") model
    // tha tis used by resetCameraLocation().  This is often used to
    // modify the initial model for downsample/subframe before pointing
    // takes place.

    virtual void setInitialFromCurrent();

    // Retrieves the image-space coordinates (LS below) of the "pointing
    // axis" of the camera.  We do this by adding the approximate pointing
    // direction to C and projecting that into the image.

    virtual void getCameraCenter(double &line, double &sample) const;

    // Returns the angle subtended by a pixel, in either the line or sample
    // directions (normally these will be the same).  The angle is in radians,
    // so the units are radians/pixel.  If this is not constant across the
    // camera, the angle for the central pixel (looking down the pointing
    // vector) is returned.  If "which" is 0, the Line direction is returned;
    // 1 returns the Sample direction.

    virtual double getPixelAngle(const int which) const;

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
				PigCoordSystem *cs) const;
    virtual void XYZtoLS(const PigPoint &xyz, const int infinity_flag,
			 double *line, double *sample, 
			 PigCoordSystem *cs) const;
    virtual void XYZtoLS(const PigPoint &xyz, const int infinity_flag,
                         double *line, double *sample, PigCoordSystem *cs,
                         double *range, double par[2][3])
                         const;


    // Given FOV angles in both line and sample directions,
    // return LS min/max values. FOV angles should be 
    // specified in degrees. min/max parameters are both input/output.  
    // That way the implementation of this API can use input min/max
    // to determine image size, though it's not that way here.  

    virtual void getMinMaxLS(double fov_h, double fov_v,
			     double *x_min, double *x_max,
			     double *y_min, double *y_max) const;

    // These functions should *ONLY* be called by the corresponding
    // PointingModel!!  They generally mirror the similar functions in PM,
    // but calling the PM's version allows the pointing model to do mission-
    // specific pointing corrections or sanity checks.  See the comments
    // in PigPointingModel for descriptions.  They are all returned in the
    // current coordinate system.
    //

    virtual PigPoint getCameraPosition() { return _current_c; } const
    virtual PigVector getCameraOrientation() const
	{ PigVector v = (_current_ax * _current_ay);
	  v.normalize();
	  return v;
	}
    virtual double getCameraTwist();
    virtual void setCameraPosition(PigPoint position, PigCoordSystem *cs);
    virtual void setCameraOrientation(PigVector orientation,
				      PigCoordSystem *cs);
    virtual void setCameraTwist(double twist);

    // Interpolate this model based on the provided list.  Returns 1 on
    // success, 0 on failure.  Not implemented for PSPH, so the model
    // is just set to match the first given model (assuming it's also PSPH).

    virtual int interpolateModels(int n_models, PigCameraModel *interp_models[],
                                double interp_values[], double value);

    // No-op for PSPH, just return the same model (cloned)
    virtual PigCameraModel *alignStereoCameras(int xdim_in1, int ydim_in1,
                                               int xdim_in2, int ydim_in2,
                                               int xdim_out, int ydim_out,
                                               PigCameraModel *other)
	{ return clone(); }

    // Convert to/from the generic cmod_t type.  Intended for internal
    // use only, although public just in case.  The from_cmod_t() method
    // requires that "this"s type match the cmod_t type or an error is
    // printed and returned (success==1, error==0).

    virtual void to_cmod_t(cmod_t *cmod);
    virtual int from_cmod_t(cmod_t *cmod);

    // clone() returns a copy of this camera model, although it's not connected
    // to any pointing model.  Subclasses should override to create the correct
    // subclass type.  copy(m) copies the contents of m into the model;
    // subclasses call superclass copy before copying their own fields.

    virtual PigCameraModel *clone()
        {   PigPSPH *new_m = new PigPSPH();
            if (new_m) new_m->copy(this);
            return new_m;
        }
    virtual void copy(PigCameraModel *mm)
        {   PigPSPH *m = (PigPSPH *) mm;
	    PigCameraModel::copy(m);		// copy base
	    _psph_set = m->_psph_set;
	    _orig_c = m->_orig_c;
	    _orig_ax = m->_orig_ax;
	    _orig_ay = m->_orig_ay;
	    _orig_nx = m->_orig_nx;
	    _orig_ny = m->_orig_ny;
	    _orig_sx = m->_orig_sx;
	    _orig_sy = m->_orig_sy;
	    _current_c = m->_current_c;
	    _current_ax = m->_current_ax;
	    _current_ay = m->_current_ay;
	    _current_nx = m->_current_nx;
	    _current_ny = m->_current_ny;
	    _current_sx = m->_current_sx;
	    _current_sy = m->_current_sy;
        }

    virtual const char *const getModelName() { return "PSPH"; }
    virtual PigCmodType getModelType() { return CMOD_TYPE_PSPH; }

};

#endif

