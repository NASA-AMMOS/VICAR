////////////////////////////////////////////////////////////////////////
// PigPointingModel
//
// Base class for Pointing models.  This aims a camera model.  This is
// generally highly mission-dependent, as the pointing parameters are
// different for each mission (so each mission should have its own subclass
// created by the create() factory functions).  The pointing model calls
// routines on the camera model to aim it.  Pointing models are associated
// one-to-one with camera models, although the associations can be changed.
//
// Pointing parameters can be derived from many different sources, including
// image label, SPICE, project database, mission file, and parameters.
// Generally, the image itself is sufficient, because an image identifier
// (like SCET or SCLK) can be derived from the label, and used to access
// the databases.  Or the data can be in the label itself.  It is up to each
// projects' subclass to search the various data sources in the manner most
// appropriate to that project.  The generic pointing function therefore
// takes only an image identifier, or a spacecraft/camera/observation ID
// set.  More specific functions with other parameters can be in the
// mission-specific subclasses, for use by mission-specific programs.
//
// Note that the coordinate system used is the "natural" frame for the
// instrument, and this is not settable (it is embedded in how the camera
// is pointed).  A default is created in the subclass constructor, which
// is re-set when pointCamera(PigFile *) is called.  If pointing from a
// file is NOT used (e.g. in telemproc where pointing is done directly),
// then the default Site is used and the coordinate system Site parameters
// appropriate for the mission (often, quaternion and offset) must be set
// explicitly.  This can be done via getCoordSystem->getSite() and changing
// the Site directly.  Note that ALL PointingModel's pointed in this manner
// (actually, everything using the default Site) will share the SAME Site info.
//
// TBD:  how to handle params needed by SPICE, direct parameters, etc...
////////////////////////////////////////////////////////////////////////
#ifndef PIGPOINTINGMODEL_H
#define PIGPOINTINGMODEL_H

#include "PigModelBase.h"
#include "PigAdjustable.h"
#include "PigVector.h"
#include "PigMission.h"

class PigFileModel;
class PigCameraModel;
class PigCoordSystem;

class PigPointingModel : public PigModelBase, public PigAdjustable {

  protected:

    PigMission *_mission;		// This could very well be NULL...

    char *_instrument;

    PigCameraModel *_camera_model;

    PigCoordSystem *_pointing_cs;

    PigPointingModel(PigCameraModel *cm,
				PigMission *mission, const char *instrument);

    int checkCameraModel();		// Checks for NULL

  public:

    // These factory methods create and return an instance of the
    // proper subclass for the given mission/camera.  These parameters
    // are specified by the file itself (look at the label to figure it
    // out), or by the mission name/instrument name.  The actual pointing data
    // from the file is not used, so the same PointingModel object can be
    // used to point multiple observations (from the same camera, of course).

    static PigPointingModel *create(PigCameraModel *cm, const char *filename,
                                    const char *type, bool allow_type_override);
    static PigPointingModel *create(PigCameraModel *cm, PigFileModel *file,
                                    const char *type, bool allow_type_override);
    static PigPointingModel *create(PigCameraModel *cm,
				const char *mission, const char *instrument,
                                const char *type, bool allow_type_override);

    virtual const char *getMissionName() const
	{ if (_mission)
		return _mission->getMissionName();
	  return NULL;
	}
    virtual PigMission *getMission() { return _mission; }
    virtual const char *getInstrumentName() const { return _instrument; }

    // Set or get the camera model.  Usually done in the constructor.
    // Subclasses may override this in order to check the camera model for
    // a subclass they can handle... but if so, they need to call their own
    // version of this in their constructor, due to problems with calling
    // virtual functions from constructors.

    virtual void setCameraModel(PigCameraModel *cm);
    virtual PigCameraModel *getCameraModel() { return _camera_model; }

    // Get the "natural" coordinate system used with this PointingModel
    // (note: this MUST be set by subclass constructors!

    PigCoordSystem *getCoordSystem() { return _pointing_cs; }

    // Set the pointing model's coord sys.  This should only be called if
    // you know what you're doing... it should be automatically set in most
    // cases.  Most likely the only time this will be needed is in a situation
    // like telemproc where you have no file and are building everything from
    // scratch.

    virtual void setCoordSystem(PigCoordSystem *cs)
	{ _pointing_cs = cs; }

    // Point a camera model, given an image or an observation ID.  This
    // does not have to be the same image as was given in the constructor,
    // although presumably the mission/camera names should match!  Mission-
    // specific subclasses will probably have other pointing functions,
    // which take parameters such as az/el directly.
    // data_source is included mainly to allow the overloading of these
    // two functions, but is intended to specify where to get the potining
    // data from, e.g. SPICE, database, etc. (TBD!!!!)
    //
    // Note:  the only reason the filename version is not virtual, is to
    // get around the STUPID C++ compiler warning about hiding virtual
    // functions.  With overloaded VF's, you have to override all or none in
    // the subclasses.  All this one does is create a FileModel and call
    // that version of pointCamera().

    void pointCamera(const char *filename);
    virtual void pointCamera(PigFileModel *file) = 0;
    // Attempt to point camera using Pointing Model
    // defined in the Label.  
    bool pointCameraViaLabel(PigFileModel *file);

    virtual void pointCamera(const char *obs_id, char *data_source) = 0;

    // These functions get and set the pointing and camera position, in
    // the given coordinates.  They are *not* intended for pointing
    // correction, use the get/setPointingParameters() functions for that.
    // Note that the position/orientation are somewhat "idealized" in that
    // they may or may not correspond to either the physical or optical
    // axes of the camera (this is up to the subclass); the intention is
    // to be able to define the output location for a reprojected mosaic.
    //
    // Often, this will be a trivial operation for the PM subclass; it
    // just feeds the info to the CM and gets the info from it.  The reason
    // the PM is involved is to allow it to do "sanity checks" on the data
    // if desired, or other massaging required by the camera model.
    //
    // The coordinates and angles are returned in the supplied coordinate
    // frame.  For the set functions, they are expressed in the supplied frame.
    // Also, the orientation could be specified via a quaternion, but
    // given the work involved to pack and unpack it, it is instead returned
    // as a look direction unit vector, and a twist rotation angle about
    // that look direction (defined using the right-hand rule, in radians).
    //
    // Subclasses should feel free to override these if needed, but usually
    // the base class implementation should suffice... except for
    // setOrientation().

    virtual PigPoint getCameraPosition(PigCoordSystem *cs);
    virtual PigVector getCameraOrientation(PigCoordSystem *cs);
    virtual double getCameraTwist();			// no CS needed

    virtual void setCameraPosition(const PigPoint &position,PigCoordSystem *cs);
    virtual void setCameraOrientation(const PigVector &orientation,
							PigCoordSystem *cs) = 0;
    virtual void setCameraTwist(const double twist);		// no CS needed

    // The below functions come from PigAdjustable...
    //
    // These functions allow access to the "raw" pointing data, in order
    // to accomplish pointing corrections or "tweaks".  The content of
    // this data is highly mission-specific, as each instrument has its own
    // set of parameters.  The getPointingParamName() function returns the
    // "name" of this parameter, so common software can identify what it is.
    // Each mission's subclass *MUST* specify exactly which parameters are
    // returned in what order, so that mission-specific code can interpret
    // them.  Also, they should if possible follow a common pattern:
    // az/ra/lon, el/dec/lat, twist
    // Parameter type names should be standardized as much as possible.
    // A new type of pointing info (such as a robot arm elbow angle) can
    // be added if necessary, but the existing strings should be used if
    // possible:
    // Azimuth, Elevation, Right Ascension, Declination, Twist
    // The error estimate is rather loosely defined as how much error is
    // expected in the pointing system.
    //!!!! Error estimate should perhaps be more rigorously defined
    // (e.g. 3-sigma value) but that is not yet necessary.
    // All relevant values are measured in the instrument "natural"
    // coordinate system (_pointing_cs).

    //!!!! TBD: The list above should be extended
    //!!!! TBD: Min/max ranges and error bars should be available
    //!!!! TBD: Need some way to identify parameters which affect lots of
    //!!!!      images, e.g. rover position for a panorama series.
    //!!!!	Resolution: use PigSite
    //!!!! TBD: Need to know basic "type" of param: angle, position, etc.
    //!!!! TBD: Do we need to deal with cross-dependencies (e.g. a unit
    //!!!!      vector's elements must maintain magnitude 1)?

    virtual int getPointingParamCount() { return 0; }
    virtual void getPointingParameters(double params[], const int max_count) { }
    virtual void setPointingParameters(const double params[], const int count)
			{ }
    virtual void getPointingErrorEstimate(double errors[], const int max_count)
			{ }
    virtual const char *const getPointingParamName(int i)	// 0-based
			{ return ""; }

    virtual void forceCameraRepoint( )
                        { }

    virtual const char *const getModelName() { return "Base"; }
    virtual ~PigPointingModel();

};

#endif

