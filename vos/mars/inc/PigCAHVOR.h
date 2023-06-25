////////////////////////////////////////////////////////////////////////
// PigCAHVOR
//
// Implements the CAHVOR camera model.
////////////////////////////////////////////////////////////////////////
#ifndef PIGCAHVOR_H
#define PIGCAHVOR_H

#include "PigCAHV.h"

class PigCAHVOR : public PigCAHV {

  protected:

    PigVector _orig_o;
    PigVector _orig_r;		// not really a vector, just 3 numbers

    PigVector _current_o;
    PigVector _current_r;

  public:

    PigCAHVOR(const char *mission, const char *instrument,
	      const char *version, const char *subtype,
	      const char *construction, const char *calibration);
    PigCAHVOR();

    virtual ~PigCAHVOR();

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
    // Does the CAHVOR part of the label work
    virtual void setupWriteToLabel(LblCameraModel_typ &CM, 
				   PigCoordSystem *cs) const;
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
    // must set the CS first, then set the CAHVOR vectors.

    virtual void setInitialCAHV(const PigPoint c, const PigVector a,
				const PigVector h, const PigVector v,
				PigCoordSystem *cs);
    virtual void setInitialCAHV(const double c[3], const double a[3],
				const double h[3], const double v[3],
				PigCoordSystem *cs);

    virtual void setInitialCAHVOR(const PigPoint c, const PigVector a,
				const PigVector h, const PigVector v,
				const PigVector o, const PigVector r,
				PigCoordSystem *cs);
    virtual void setInitialCAHVOR(const double c[3], const double a[3],
				const double h[3], const double v[3],
				const double o[3], const double r[3],
				PigCoordSystem *cs);

    // Set the current CAHV vectors.  Note that the cs argument defines
    // what the given vectors are measured in... it does NOT change
    // the coordinate system definition of the object.  To do that, callers
    // must set the CS first, then set the CAHVOR vectors.

    virtual void setCurrentCAHV(const PigPoint c, const PigVector a,
				const PigVector h, const PigVector v,
				PigCoordSystem *cs);
    virtual void setCurrentCAHV(const double c[3], const double a[3],
				const double h[3], const double v[3],
				PigCoordSystem *cs);

    virtual void setCurrentCAHVOR(const PigPoint c, const PigVector a,
				const PigVector h, const PigVector v,
				const PigVector o, const PigVector r,
				PigCoordSystem *cs);
    virtual void setCurrentCAHVOR(const double c[3], const double a[3],
				const double h[3], const double v[3],
				const double o[3], const double r[3],
				PigCoordSystem *cs);

    // Get the current CAHVOR vectors

    virtual void getCurrentCAHVOR(PigPoint &c, PigVector &a,
				PigVector &h, PigVector &v,
				PigVector &o, PigVector &r);

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

    // Resets the camera pointing to the "reference" location (usually
    // where the calibration was taken), in preparation for a new pointing.

    virtual void resetCameraLocation();

    // Copy the Current parameters to the Initial ("reference") model
    // tha tis used by resetCameraLocation().  This is often used to
    // modify the initial model for downsample/subframe before pointing
    // takes place.

    virtual void setInitialFromCurrent();

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
			double *line, double *sample, PigCoordSystem *cs)
			const;
    virtual void XYZtoLS(const PigPoint &xyz, const int infinity_flag,
			double *line, double *sample, PigCoordSystem *cs,
                        double *range, double par[2][3])
			const;

    // The get/set Camera Position/Orientation/Twist as implemented in CAHV
    // are sufficient for CAHVOR, because they all use moveCamera(), which is
    // smart enough to handle the OR terms.

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
        {   PigCameraModel *new_m = new PigCAHVOR();
            if (new_m) new_m->copy(this);
            return new_m;
        }
    virtual void copy(PigCameraModel *mm)
        {   PigCAHVOR *m = (PigCAHVOR *)mm;
	    PigCAHV::copy(m);			// copy base
	    _orig_o = m->_orig_o;
	    _orig_r = m->_orig_r;
	    _current_o = m->_current_o;
	    _current_r = m->_current_r;
        }

    // This really should be protected but C++ won't allow access that
    // way.  So this should not be called in user code.
    virtual void get_cahvore_vectors(double c[3], double a[3],
               double h[3], double v[3], double o[3], double r[3], double e[3],
               int &mtype, double &mparm, PigCmodType &type, PigCoordSystem *cs)
        { PigCAHV::get_cahvore_vectors(c,a,h,v,o,r,e,mtype,mparm,type,cs);
          _current_o.getXYZ(o);
          _current_r.getXYZ(r);
	  if (_current_cs != cs) {		// convert
	      cs->convertVector(_current_o,_current_cs).getXYZ(o);
	      // r is not translated
	  }
        }
    virtual void get_initial_cahvore_vectors(double c[3], double a[3],
               double h[3], double v[3], double o[3], double r[3], double e[3],
               int &mtype, double &mparm, PigCmodType &type, PigCoordSystem *cs)
        { PigCAHV::get_initial_cahvore_vectors(
					c,a,h,v,o,r,e,mtype,mparm,type,cs);
          _orig_o.getXYZ(o);
          _orig_r.getXYZ(r);
	  if (_initial_cs != cs) {		// convert
	      cs->convertVector(_orig_o,_initial_cs).getXYZ(o);
	      // r is not translated
	  }
        }

    virtual const char *const getModelName() { return "CAHVOR"; }
    virtual PigCmodType getModelType() { return CMOD_TYPE_CAHVOR; }

};

#endif

