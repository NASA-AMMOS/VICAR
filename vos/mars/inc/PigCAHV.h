////////////////////////////////////////////////////////////////////////
// PigCAHV
//
// Implements the CAHV camera model.
////////////////////////////////////////////////////////////////////////
#ifndef PIGCAHV_H
#define PIGCAHV_H

#include "PigCameraModel.h"
#include "PigVector.h"
#include "PigCoordSystem.h"

#define	PI (3.14159265358979323846)
#define LIM_FOV (.98*PI)
#define MIN_FOV 1

struct cmod_t;

class PigCAHV : public PigCameraModel {

  protected:
    int _cahv_set;

    PigPoint _orig_c;
    PigVector _orig_a;
    PigVector _orig_h;
    PigVector _orig_v;

    PigPoint _current_c;
    PigVector _current_a;
    PigVector _current_h;
    PigVector _current_v;

  public:

    PigCAHV(const char *mission, const char *instrument,
			const char *version, const char *subtype,
			const char *construction, const char *calibration);
    PigCAHV();

    virtual ~PigCAHV();

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
    // Does the CAHV part of the label work
    virtual void setupWriteToLabel(LblCameraModel_typ &CM, PigCoordSystem *cs)
                                   const;

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

    virtual int readFromString(const char *string, PigCoordSystem *cs);
    virtual int writeToString(char *string, int max_length);

    // Set the coordinate systems

    virtual void setInitialCoordSystem(PigCoordSystem *cs);
    virtual void setCoordSystem(PigCoordSystem *cs);

    // Set the initial CAHV vectors.  Note that the cs argument defines
    // what the given vectors are measured in... it does NOT change
    // the coordinate system definition of the object.  To do that, callers
    // must set the CS first, then set the CAHV vectors.

    virtual void setInitialCAHV(const PigPoint c, const PigVector a,
				const PigVector h, const PigVector v,
				PigCoordSystem *cs);
    virtual void setInitialCAHV(const double c[3], const double a[3],
				const double h[3], const double v[3],
				PigCoordSystem *cs);

    // Set the current CAHV vectors.  Note that the cs argument defines
    // what the given vectors are measured in... it does NOT change
    // the coordinate system definition of the object.  To do that, callers
    // must set the CS first, then set the CAHV vectors.

    virtual void setCurrentCAHV(const PigPoint c, const PigVector a,
				const PigVector h, const PigVector v,
				PigCoordSystem *cs);
    virtual void setCurrentCAHV(const double c[3], const double a[3],
				const double h[3], const double v[3],
				PigCoordSystem *cs);

    // Get the current CAHV vectors

    virtual void getCurrentCAHV(PigPoint &c, PigVector &a,
				PigVector &h, PigVector &v);

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

    ////////////////////////////////////////////////////////////////////////
    // Aligns the model with another for use with stereo imagery.
    //
    // This method works with ANY of the CAHV* models as input, so it should
    // not be subclassed.
    //
    // The output model matches "this" model.  Call alignStereoCameras on the
    // other camera model with this as a parameter to get the aligned partner.
    //
    // The input models are unchanged. The output model retains the location
    // of the original, but will be pointing parallel to the (aligned) partner
    // model, at the "average" look direction of the two.  This essentially
    // removes toe-in and other nasty physical non-alignments of stereo
    // cameras, giving a nice, parallel look direction to both cameras.
    // It should also be able to "align" pictures taken from the same camera
    // at different times, or wildly different look angles.
    //
    // If there is no stereo partner, or the partner is not CAHV*, or if the
    // two cameras are coincident, then this model is simply warped to a linear
    // model and returned.  NULL is returned if something really bad happens.
    //
    // Note: we linearize (separately) both the Initial and Current values, so
    // that we can point the camera either before or after.  For the stereo
    // partner (Actual lin) case, you really want to point before, and not
    // repoint after linearization.  For the synthesized partner (nominal lin)
    // case, you can go either way.
    //
    // x/ydim_in1 refers to "this" model, x/ydim_in2 refers to "other".
    // If the "other" size is not known, pass in the same for 1 and 2.
    //
    // Two parameters in POINT_METHOD control the behavior:
    //
    // CMOD_WARP=n
    // Sets the algorithm used for warping.  Current values:
    //   1 = old mode, used by MER and MSL since 2012
    //   2 = new model (2017), used by InSight and Mars 2020
    //   3 or PSPH = aligns to (and returns) PSPH model instead of CAHV
    // Note that algorithm 2 is much better for nontraditional stereo, such as
    // vertical or other weird geometries. CAHV supports only 1, CAHVOR supports
    // only 1 and 2, CAHVOR supports all three.
    //
    // CAHV_FOV=x
    // Sets the field of view for the output cameras.
    //   MIN or INTERSECT: (default) Picks the FOV as the intersection of the
    //      intput cameras.  The output image will be missing a (sometimes
    //      significant) portion of the overlap area, but there are no black
    //      areas; the entire image overlaps its partner.
    //   MAX or UNION: Picks the FOV as the union of the two input cameras.
    //      Thus, there are black areas on the side, but all overlapping pixels
    //      are preserved.  Depending on camera geometry, significant resolution
    //      may be lost, especially for fisheye cameras, due to the black areas.
    //   LINEAR:  Uses only CAHV vectors, ignoring higher order (O,R,E) terms.
    //      This has the advantage of best preserving horizontal aspect ratio
    //      and results in images that are similar scale-wise to the original.
    //      It usually ends up in between MAX and MIN as a compromise.
    //      Note that there is no choice of CMOD_WARP algorithm for LINEAR mode.
    ////////////////////////////////////////////////////////////////////////

    virtual PigCameraModel *alignStereoCameras(int xdim_in1, int ydim_in1,
					       int xdim_in2, int ydim_in2,
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
    // axis" of the camera.  The 0,0 point is encoded in the H and V vectors
    // (basically, it is the origin of the image plane during calibration).
    // This can be used to relate "logical" LS coordinates to the physical
    // coordinates of a file.  Coordinates are assumed to be 0-based (unlike
    // VICAR files).

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
    // This implementation should be sufficient for most uses, 
    // subclasses should override it only if they need something
    // unusual.

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
    // The Twist defined as is the angle between the a-v plane and the Z axis.

    virtual PigPoint getCameraPosition() { return _current_c; }
    virtual PigVector getCameraOrientation() { return _current_a; }
    virtual double getCameraTwist();
    virtual void setCameraPosition(PigPoint position, PigCoordSystem *cs);
    virtual void setCameraOrientation(PigVector orientation,
				      PigCoordSystem *cs);
    virtual void setCameraTwist(double twist);

    // Interpolate this model based on the provided list.  Returns 1 on
    // success, 0 on failure.
    // Note: implementation assumes that all models are expressed in
    // the same CS!!

    virtual int interpolateModels(int n_models, PigCameraModel *interp_models[],
                                double interp_values[], double value);

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
        {   PigCAHV *new_m = new PigCAHV();
            if (new_m) new_m->copy(this);
            return new_m;
        }
    virtual void copy(PigCameraModel *mm)
        {   PigCAHV *m = (PigCAHV *) mm;
	    PigCameraModel::copy(m);		// copy base
	    _cahv_set = m->_cahv_set;
	    _orig_c = m->_orig_c;
	    _orig_a = m->_orig_a;
	    _orig_h = m->_orig_h;
	    _orig_v = m->_orig_v;
	    _current_c = m->_current_c;
	    _current_a = m->_current_a;
	    _current_h = m->_current_h;
	    _current_v = m->_current_v;
        }

    // This really should be protected but C++ won't allow access that
    // way.  So this should not be called in user code.
    virtual void get_cahvore_vectors(double c[3], double a[3],
               double h[3], double v[3], double o[3], double r[3], double e[3],
               int &mtype, double &mparm, PigCmodType &type, PigCoordSystem *cs)
	{ PigCameraModel::get_cahvore_vectors(
					c,a,h,v,o,r,e,mtype,mparm,type,cs);
	  _current_c.getXYZ(c);
	  _current_a.getXYZ(a);
	  _current_h.getXYZ(h);
	  _current_v.getXYZ(v);
	  _current_a.getXYZ(o);		// O has to equal A
	  if (_current_cs != cs) {		// convert
	      cs->convertPoint(_current_c,_current_cs).getXYZ(c);
	      cs->convertVector(_current_a,_current_cs).getXYZ(a);
	      cs->convertVector(_current_h,_current_cs).getXYZ(h);
	      cs->convertVector(_current_v,_current_cs).getXYZ(v);
	      cs->convertVector(_current_a,_current_cs).getXYZ(o);
	  }
	}
    virtual void get_initial_cahvore_vectors(double c[3], double a[3],
               double h[3], double v[3], double o[3], double r[3], double e[3],
               int &mtype, double &mparm, PigCmodType &type, PigCoordSystem *cs)
	{ PigCameraModel::get_initial_cahvore_vectors(
					c,a,h,v,o,r,e,mtype,mparm,type,cs);
	  _orig_c.getXYZ(c);
	  _orig_a.getXYZ(a);
	  _orig_h.getXYZ(h);
	  _orig_v.getXYZ(v);
	  _orig_a.getXYZ(o);		// O has to equal A
	  if (_initial_cs != cs) {		// convert
	      cs->convertPoint(_current_c,_initial_cs).getXYZ(c);
	      cs->convertVector(_current_a,_initial_cs).getXYZ(a);
	      cs->convertVector(_current_h,_initial_cs).getXYZ(h);
	      cs->convertVector(_current_v,_initial_cs).getXYZ(v);
	      cs->convertVector(_current_a,_initial_cs).getXYZ(o);
	  }
	}


    virtual const char *const getModelName() { return "CAHV"; }
    virtual PigCmodType getModelType() { return CMOD_TYPE_CAHV; }

};

#endif

