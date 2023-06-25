////////////////////////////////////////////////////////////////////////
// PigFileModel
//
// Base class for File models.  Responsible for maintaining an input
// file, and managing label information from it.
//
// Certain function calls return data in the multimission Label API (Lbl*)
// structures.  For missions which do not conform with this format (e.g.
// MPF), these function calls should read appropriate labels and fill in
// the important fields in the Label API structures.
//
// The base class is intended to be sufficient for most missions using the
// MM label structures.  Mission-specific subclasses can be created as
// needed.  The create() function should be modified to call a mission-
// specific create() function for this case.
//
// Several functions in the File model were formerly in the Pointing model,
// but as they apply to the input file more than the camera pointing, they
// were moved to this model.
//
// Note that a FileModel is NOT required for any camera/pointing model
// combo (i.e. you can create camera/pointing models without files).
// However, certain mosaic-related functions (e.g. is the pixel within the
// file) DO require a FileModel.
//
// There is a class of label accessors that are pseudo-mission-specific in
// that they are not shared by all missions: Rsm, Arm, Hga, Chassis,
// RoverCoordSys, ArmCoordSys, InitState, ... but they are shared across 
// several. (MER maps PMA->RMC, IDD->ARM, while NSYT maps LanderCoordSys->
// RoverCoordSys).  
//
// These used to be in mission-specific subclasses but have been
// moved to the primary level.  Missions that do not implement these are
// free to override them to return null, or simply let them error out if
// called.  They are rarely used except within the file model; outside they
// are generally called only in contexts where we know they should be present.
//
////////////////////////////////////////////////////////////////////////
#ifndef PIGFILEMODEL_H
#define PIGFILEMODEL_H

#include "PigModelBase.h"

#include "PigVector.h"
#include "PigQuaternion.h"
#include "PigMission.h"

class PigCameraModel;
class PigCSReference;
class PigCSDefinition;
class PigSurfaceModel;
class PigCoordSystem;

#include "lbl_identification.h"
#include "lbl_compression.h"
#include "lbl_instrument_state.h"
#include "lbl_image_data.h"
#include "lbl_camera_model.h"
#include "lbl_derived_geometry.h"
#include "lbl_derived_image.h"
#include "lbl_surface_projection.h"
#include "lbl_surface_model.h"
#include "lbl_articulation.h"
#include "lbl_coordinate.h"
#include "lbl_initstate.h"

#include <string.h>

#define MAX_CS_OBJ 250		/* for readCoordSystems() */

class PigFileModel : public PigModelBase {

  protected:
    char *_filename;
    char *_mission;
    char _format[10];
    int _unit;
    int _nl, _ns, _nb;

    int _file_open;

    // These need to be set in the constructor
    int _borders_set;		// TRUE if borders are set up
    int _border_sl;		// Border of "bad" pixel area for the camera
    int _border_ss;		// (camera param; doesn't depend on the image)
    int _border_el;
    int _border_es;

    double _x_offset;		// These need to be set in the ctor too
    double _y_offset;		// (see getXOffset/getYOffset)
    virtual void setupOffsets();	// ctor should use this to set them

    char _strTmp[100];

    LblIdentification_typ *_lblIdentification;
    LblInstrumentState_typ *_lblInstrumentState;
    LblImageData_typ *_lblImageData;
    LblCameraModel_typ *_lblCameraModel;
    int _lblCameraModel_instance;
    LblDerivedGeometry_typ *_lblDerivedGeometry;
    LblDerivedGeometry_typ *_lblSiteDerivedGeometry;
    LblDerivedImage_typ *_lblDerivedImage;
    LblSurfaceProjection_typ *_lblSurfaceProjection;
    LblSurfaceModel_typ *_lblSurfaceModel;
    LblCompression_typ *_lblCompression;

    // Pseudo-mission-specific structures (see class intro comments

    LblArticulation_typ    *_lblRsmArticulation;
    LblArticulation_typ    *_lblArmArticulation;
    LblArticulation_typ    *_lblHgaArticulation;
    LblArticulation_typ    *_lblChassisArticulation;
    LblCoordinate_typ      *_lblRoverCoordSys;
    LblCoordinate_typ      *_lblArmCoordSys;
    LblInitState_typ       *_lblInitState;

    virtual int setupBorders();		// return TRUE if they are set

    // Methods that are reused in some subclasses but not all, refactored
    // here to avoid duplicating code.  No need for these to be virtual
    // because subclasses can simply not call them from the main routine.

    void getFlatFieldCorrectionCounterImpl(float parms[], int &num_parms);
    int compareSCLKImpl(const char *sclk1, const char *sclk2);
    void getUniqueIdImpl(char *uid);
    void getUniqueIdMsecImpl(char *uid);
    int isIlutNeededSBM();
    const char *getIlutNameSBM();

  public:

    // Constructor should only be called via PigMission's create functions!
    PigFileModel(const char *filename, int unit, const char *mission);
    virtual ~PigFileModel();

    ////////////////////////////////////////////////////////////////////
    // This factory method creates and returns an instance of the
    // proper mission-specific subclass for the given file.  Missions
    // using the multimission label API may not need a mission-specific
    // subclass.

    static PigFileModel *create(const char *filename);

    ////////////////////////////////////////////////////////////////////
    // File management functions.
    ////////////////////////////////////////////////////////////////////

    // Return the VICAR unit number, filename, mission name, or file format.
    int getUnit() { return _unit; }
    const char *getFilename() { return _filename; }
    const char *getMissionName() { return _mission; }
    const char *getFormat() { return _format; }

    // Opens the file if it's not already.  Done automatically in most cases;
    // needed only for apps that need direct access via getUnit() and don't
    // want to do their own open.
    virtual void openFile();

    // Close the file, but do not free the unit.  Apps may re-open using
    // their own options if setFileOpen(TRUE) is called.
    virtual void closeFile();

    // Notify this object that the file has been opened or closed externally
    void setFileOpen(int state) { _file_open = state; }

    // Return TRUE if the file is open (valid only if app has called
    // setFileOpen properly, if it does its own open/close).
    int isFileOpen() { return _file_open; }

    // Return basic file info
    int getNL() { return _nl; }
    int getNS() { return _ns; }
    int getNB() { return _nb; }

    ////////////////////////////////////////////////////////////////////
    // File information functions (these were formerly in PigPointingModel())
    ////////////////////////////////////////////////////////////////////

    ////////////////////////////////////////////////////////////////////
    // Gets the offset between the camera model L/S coordinates, and the
    // "physical" coordinates in the image.  Note that these physical
    // coordinates are 0-based, e.g. you have to add 1 for VICAR files.
    //
    // For the Multimission case, this simply returns the FirstLine/
    // FirstLineSample label items (-1 to make them 0-based).
    //
    // Note that these simply return _x_offset and _y_offset for efficiency.
    // These must be set in the constructor.
    ////////////////////////////////////////////////////////////////////

    double getXOffset() { return _x_offset; }
    double getYOffset() { return _y_offset; }

    ////////////////////////////////////////////////////////////////////
    // Returns whether or not a line/samp location (in camera model
    // coordinates!) is inside the "good" part of the image or not.
    // This allows subclasses to create a "border" of known bad pixels,
    // exclude boom obscurations, exclude ragged edges (e.g. GLL compression
    // artifacts), etc.  It could even exclude data dropouts, rad hits,
    // holes, etc. if desired.  Applications can use this to determine
    // whether the given image pixel should be used in a mosaic or not.
    //
    // Note that this is a multi-state return.  It would be nice to say
    // if (x->testPixelLocation(...))
    // but with the extra cases, you really need to say
    // if (x->testPixelLocation(...) == 0)
    // in the spirit of strcmp().
    // Return values:
    // 0  = inside the image (really, "true" return, like strcmp())
    // 1  = outside the image physically
    // >1 = outside the image logically (i.e. inside the physical bounds but
    //      in a bad-pixel area, border, obscuration, etc.)  The actual
    //      return code could be used by mission-specific code to determine
    //      what the actual problem is.
    // -1 = unknown.  This could happen if the pointing was done via means
    //      that did not involve an image, so the pointing model doesn't know
    //      what the image size/offsets are.
    ////////////////////////////////////////////////////////////////////

    virtual int testPixelLocation(const double line, const double sample);

    ////////////////////////////////////////////////////////////////////
    // Return the camera coordinates of the edges of the image.  These should
    // not be used for checking if a pixel is inside the image; use
    // testPixelLocation() instead.  They are only here for purposes such as
    // drawing edges, where it is not practical to test each point.  Subclasses
    // should take into account obscurations/bad pixel areas as much as
    // possible, but that is obviously only good for square borders (irregular
    // obscurations can't be taken into account).  Since the coordinates are
    // doubles, they represent the extreme edges of the good pixels (not the
    // pixel centers), thus the coordinates may be between pixels.  Returns
    // 0 for success, or >0 if the coordinates are unavailable.
    ////////////////////////////////////////////////////////////////////

    virtual int getImageBorders(double &sl, double &ss, double &el, double &es);

    ////////////////////////////////////////////////////////////////////
    // Returns the Field of View of the image, in either the Line or Sample
    // direction (note: this is not the diagonal FOV, which is bigger than
    // either of these!).  This is in the FM rather than the CameraModel
    // because it depends on the size of the image, which is known only to
    // the FM (note that the CameraModel must be supplied).  The FOV is
    // returned in radians.  If "which" is 0, the Line direction is returned;
    // 1 returns the Sample direction.  The base class implementation of this
    // function should be sufficient for most uses; override it only if you
    // need to do something unusual.
    ////////////////////////////////////////////////////////////////////

    virtual double getFOV(PigCameraModel *cm, const int which);

    ////////////////////////////////////////////////////////////////////
    // Returns the nominal size of the frame for this camera, without any
    // downsampling or subframing.  This is determined using the camera
    // mapping facility; if that's not available it defaults to 1024 since
    // that's valid for most older camera (before the mapping file).  This
    // is a static function so it can be used in e.g. telemproc, meaning
    // there is no mission override.
    ////////////////////////////////////////////////////////////////////

    static void getNominalCameraSize(PigMission *mission,
					const char *inst, int &nl, int &ns);

    ////////////////////////////////////////////////////////////////////
    // Like getFOV, but it returns the nominal FOV of the camera: what it
    // would be without any downsampling or subframing.  This is determined
    // using getNominalCameraSize().  Note that, like getFOV, this simply
    // multiplies the number of pixels by the instantaneous field of view,
    // so it is an approximation.
    ////////////////////////////////////////////////////////////////////

    virtual double getNominalFOV(PigCameraModel *cm, const int which);

    ////////////////////////////////////////////////////////////////////
    // These functions return complete Label API property structures.
    // The Valid flags in each field should be checked before using the
    // value.  Regardless of the label API, there is no guarantee that
    // any particular value is present.  Non-standard missions should
    // fill in at least the appropriate fields.
    //
    // IMPORTANT!  A pointer is returned to an internal structure.  The
    // caller MUST NOT change the structure that is returned.  If changes
    // are desired, memcpy() the structure to your own first.  Also, the
    // returned pointer will go invalid if the FileModel is destroyed.
    // Use it or copy it first.
    //
    // NULL may be returned if the given property doesn't exist at all,
    // or is otherwise not relevant.
    //
    // These functions all have the side-effect of filling in the
    // structure for the corresponding member variable.  This is depended
    // on by internal routines.
    ////////////////////////////////////////////////////////////////////

    virtual const LblIdentification_typ *getLblIdentification();
    virtual const LblInstrumentState_typ *getLblInstrumentState();
    virtual const LblImageData_typ *getLblImageData();
    virtual const LblCameraModel_typ *getLblCameraModel()
			{ return getLblCameraModel(1); }
    virtual const LblCameraModel_typ *getLblCameraModel(int instance);
    virtual const LblDerivedGeometry_typ *getLblDerivedGeometry();
    virtual const LblDerivedGeometry_typ *getLblSiteDerivedGeometry();
    virtual const LblDerivedImage_typ *getLblDerivedImage();
    virtual const LblSurfaceProjection_typ *getLblSurfaceProjection();
    virtual const LblSurfaceModel_typ *getLblSurfaceModel();
    virtual const LblCompression_typ *getLblCompression();

    // These are the pseudo-mission-specific ones, see class intro comments

    virtual const LblArticulation_typ *getLblRsmArticulation();
    virtual const LblArticulation_typ *getLblArmArticulation();
    virtual const LblArticulation_typ *getLblHgaArticulation();
    virtual const LblArticulation_typ *getLblChassisArticulation();
    virtual const LblCoordinate_typ *getLblRoverCoordSys();
    virtual const LblCoordinate_typ *getLblArmCoordSys();
    virtual const LblInitState_typ *getLblInitState();

    ////////////////////////////////////////////////////////////////////
    // These functions return basic information from the label.
    // The information may be repeated in various Label API structures,
    // but the common stuff is repeated here for convenience.
    // Multivalued items return index 0 only.
    // For non-strings, an argument gives the default value to return
    // if the label item is not present or can't be read.
    ////////////////////////////////////////////////////////////////////

    virtual const char *getFrameId();			// Identification
    virtual const char *getImageId();			// Identification
    virtual const char *getInstrumentHostId();		// Identification
    virtual const char *getInstrumentId();		// Identification
    virtual const char *getInstrumentName();		// Identification
    virtual const char *getInstrumentVersionId();	// Identification
    virtual const char *getSpacecraftClockStartCount(); // Identification
    virtual const char *getSpacecraftClockStopCount();  // Identification
    virtual const char *getGeometryProjectionType();    // Identification
    virtual const char *getProductId();			// Identification
    virtual const char *getCompressionName();		// Compression
    virtual float getCompressionRate(float def);	// Compression
    virtual int getFirstLine(int def);			// ImageData
    virtual int getFirstLineSample(int def);		// ImageData
    virtual const char *getSampleBitMask();		// ImageData
    virtual float getDownsampleXFactor(float def);	// Instrument State
    virtual float getDownsampleYFactor(float def);	// Instrument State
    virtual float getExposureDuration(float def);	// InstrumentState
    virtual const char *getFilterNumber();	 	// InstrumentState
    virtual const char *getFilterName();	 	// InstrumentState
    virtual const char *getFlatFieldCorrectionFlag();   // InstrumentState
    virtual int getInstrumentAzimuthCount(int def);	// InstrumentState
    virtual const char *getInstrumentDeploymentState();	// InstrumentState
    virtual int getInstrumentElevationCount(int def);	// InstrumentState
    virtual int getInstrumentFocalLengthCount(int def);	// InstrumentState
    virtual int getInstrumentFocusPosition(int def);	// InstrumentState
    virtual int getInstrumentZoomPosition(int def);	// InstrumentState
    virtual float getInstrumentTemperature(float def);	// InstrumentState
    virtual PigPoint getInstHostPosition(PigPoint def); // InstrumentState
    virtual float getInstrumentHostHeading(float def);  // InstrumentState
    virtual float getInstrumentHostPitch(float def);    // InstrumentState
    virtual float getInstrumentHostRoll(float def);     // InstrumentState
    virtual float getTelemetryInstrumentElevation(float def); //InstState
    virtual PigPoint getTelemetryInstrumentPosition(PigPoint def); //InstState
    virtual const char *getInstrumentModeId();          // InstrumentState
    virtual const char *getBayerMode();			// InstrumentState
    virtual int getShutterEffectCorrectionFlag(int def); // InstState (Boolean)
    virtual const char *getShutterCorrectionMode();	// InstState
    virtual int getOriginalSampleBits(int def);		// InstrumentState
    virtual float getLanderInstrumentAzimuth(float def); // DerivedGeometry
    virtual float getLanderInstrumentElevation(float def); // DerivedGeometry
    virtual PigQuaternion getInstLocalLevelQuaternion(PigQuaternion def); // DG
    virtual float getSolarElevation(float def);		// SiteDerivedGeometry
    virtual float getSolarAzimuth(float def);		// SiteDerivedGeometry
    virtual PigVector getSrfcFxdLclLvlVector(PigVector def); // DerivedGeometry
    virtual const char *getCalibrationSourceId();       // CameraModel
    virtual int getOffsetNumber(int def);		// InstrumentState
    virtual int getDCOffset(int def);			// InstrumentState


    // Returns the name of the pointing model if defined in the label.
    // Otherwise returns NULL.
    virtual const char *getPointingModelName();         // DerivedImage

    virtual const char *getColorSpace();                // DerivedImage
    virtual float getRadianceOffset(float def);         // DerivedImage
    virtual float getRadianceScaleFactor(float def);    // DerivedImage

    virtual float getCorrelationAverageScale(float def);// DerivedImage
    virtual float getCorrelationPixelCount(float def);	// DerivedImage
    
    virtual float getStereoBaseline(float def);		// DerivedImage
    virtual const char *getStereoProductId();		// DerivedImage

    // Get the Pointing Model's params.  The array must be supplied
    // num_params on input should contain the max size of the supplied
    // array.  On output it will contain the number of elements actually
    // found.  If pointing model is not defined in the label, will return 0
    virtual void getPointingModelParams(double params[], int &num_params);


    // Get the Camera model Transform vector and quaternions, and convert
    // them to the desiredCS if supplied.
    // If camera model vector and quaternions are invalid, the output location 
    // and orientation are unchanged. To the user to init them with proper 
    // default values.
    virtual void getArticulationDevLocationOrient(PigPoint &location,
                                                  PigQuaternion &orientation,
                                                  PigCoordSystem *desired_cs,
                                                  PigMission *mission);



    // Caller is responsible for memory allocation of uid char array
    // uid array should be big enough to hold at least 33 char
    virtual void getUniqueId(char *uid);                // Comb of fields

    // Second unique ID if needed (e.g. on M20, both with and without msec)
    virtual void getUniqueId2(char *uid) { getUniqueId(uid); }

    // Input param indicates what element number to get
    virtual char*  getSourceProductId(int num);// Identification

    // Compares 2 SPACECRFAFT_CLOCK_XXX_COUNT strings.
    // The function returns an integer greater than, equal  to,  or
    // less  than  0, if the string pointed to by sclk1 is greater
    // than, equal to, or less than the string  pointed  to  by  s2
    // respectively.
    // For more info see strcmp(..) man pages, since base class 
    // implementation uses it.
    virtual int compareSCLK(const char *sclk1, const char *sclk2);

    // In only a few cases is the lack of value significant...

    virtual int checkInstrumentAzimuthCount();		// InstrumentState
    virtual int checkInstrumentElevationCount();	// InstrumentState
    virtual int checkTelemetryInstrumentElevation();	// InstrumentState
    virtual int checkTelemetryInstrumentPosition();	// InstrumentState

    // Extra temperatures

    virtual float getInstrumentTemperatureStart(float def)
				{ return getInstrumentTemperature(def); }
    virtual float getInstrumentTemperatureEnd(float def)
				{ return getInstrumentTemperature(def); }
    virtual float getInstrumentElectronicsTemperature(float def)
				{ return getInstrumentTemperature(def); }
    virtual float getInstrumentOpticsTemperature(float def)
				{ return getInstrumentTemperature(def); }

    // This requires a computation, normally

    virtual int getSampleBits(int def);

    ////////////////////////////////////////////////////////////////////
    // Get the Rover Motion Counter for the file.  The array must be supplied.
    // num_index on input should contain the max size of the supplied array;
    // on output it will contain the number of elements actually found (which
    // could be bigger than the supplied max size, but only that many will
    // actually be returned in the array).  PIG_MAX_RMC_INDEX (from
    // PigMission.h) can be useful for array dimensioning.
    //
    // The base class impl always returns 0, for those missions without RMC.
    ////////////////////////////////////////////////////////////////////

    virtual void getRoverMotionCounter(int indices[], int &num_indices)
	{ num_indices = 0; }

    // Returns the nominal number of RMC elements for this mission, or 0
    // if none.  A given image may not have all elements specified; this
    // call returns the max.
    virtual int getRoverMotionCounterCount() { return 0; }

    // Returns the string names of each RMC element.  String is statically
    // allocated so user need not free.  Returns NULL if index out of range.
    virtual char *getRoverMotionCounterName(int index) { return NULL; }

    ////////////////////////////////////////////////////////////////////
    // Get the onboard preboost factors ONBOARD_RESPONSIVITY for the file.
    // The array must be supplied, and its length must be 3.
    ////////////////////////////////////////////////////////////////////
    
    virtual int getOnboardResponsivity(float factors[3]);

    ////////////////////////////////////////////////////////////////////
    // Get the Flat Field Correction Parms for the file.  The array must 
    // be supplied.  num_index on input should contain the max size of the 
    // supplied array;  on output it will contain the number of elements 
    // actually found (which could be bigger than the supplied max size, 
    // but only that many will actually be returned in the array).  
    // PIG_MAX_FLAT_FIELD_INDEX (from RadiometryModel.h) can be useful 
    // for array dimensioning.
    //
    // The base class impl always returns 0, for those missions without
    // on-board flat field correction.
    ////////////////////////////////////////////////////////////////////

    virtual void getFlatFieldCorrectionCounter(float parms[], int &num_parms)
	{ num_parms = 0; }

    ////////////////////////////////////////////////////////////////////
    // Read the reference coord sys info from various places and fill in
    // the provided PigCSReference structure.  This can then be used to
    // create the reference PigCoordSystem object if needed.
    // The PigCSReference is always returned valid; if nothing else works
    // then a suitable default is returned.  The status code indicates
    // whether the value was properly read from the label (1) or not (0).
    // The CSRef can be used even if the return code is 0.
    //
    // The base class implementations used to simply return 0 and the FIXED CS
    // (or INSTRUMENT CS for the camera model).  But, because all subclasses
    // had the same code, and it's innocuous if there is no reference in
    // the label, the code was refactored to the base class.
    ////////////////////////////////////////////////////////////////////

    virtual int getDerivedImageCS(PigCSReference *&ref);
    virtual int getSurfaceProjectionCS(PigCSReference *&ref);
    virtual int getCameraModelCS(PigCSReference *&ref, int instance);
    virtual int getSurfaceModelCS(PigCSReference *&ref);

    ////////////////////////////////////////////////////////////////////
    // Static routine to read in all coordinate system properties in the
    // label.  This method is static because it is used in several different
    // places, on files that (properly) don't have a PigFileModel.
    // Fills up an array of PigCSDefinition objects, which must be passed in.
    //
    // The count parameter on input is the max dimension of the array;
    // on output it is the number actually _in_ the array.  There is no
    // facility for determining the maximum needed, if the array is too small.
    // MAX_CS_OBJ is a good maximum to use.
    //
    // The file associated with the passed-in unit must be already open.
    ////////////////////////////////////////////////////////////////////

    static void readAllCoordSystems(PigMission *m, int unit, int &count,
						PigCSDefinition *defs[]);

    ////////////////////////////////////////////////////////////////////
    // ILUT routines.  The first checks to see if an ILUT needs to be
    // applied.  The second returns the name of the ILUT.  Missions that
    // support ILUTs should override these.  Note the protected
    // SBM (for SAMPLE_BIT_METHOD|MODE) functions above, which implement
    // the MER/MSL model.
    ////////////////////////////////////////////////////////////////////

    virtual int isIlutNeeded() { return FALSE; }
    virtual const char *getIlutName() { return NULL; }

    ////////////////////////////////////////////////////////////////////
    // Determines whether or not the image has radiometric correction
    // related labels (RADIANCE_OFFSET, RADIANCE_SCALING_FACTOR,
    // RADIOMETRIC_CORRECTION_TYPE). The function returns TRUE if one of
    // the labels is there.
    ////////////////////////////////////////////////////////////////////

    virtual int hasRadiometricCorrectionLabel();

    ////////////////////////////////////////////////////////////////////
    // Determines whether or not the input has sufficient information to
    // be converted from integers to floating point numbers.
    // In order to be converted from intergers to floating point numbers:
    // 1. The input must be in either HALF or BYTE format.
    // 2. The input must have at least one of the three RAD related labels
    //    listed below:
    //        * RadianceOffset
    //        * RadianceScaleFactor
    //        * RadiometericCorrectionType
    ////////////////////////////////////////////////////////////////////

    virtual int canConvertRADToFloat();

    ////////////////////////////////////////////////////////////////////
    // Read the standard Cylindrical mosaic properties from the label.
    // The base class implementation assumes MER-style SurfaceProjection
    // labels; other missions can get the info differently.  Values are
    // returned via the reference parameters.  Returns 0 on success, non-0
    // on failure.
    ////////////////////////////////////////////////////////////////////

    virtual int getCylindricalProjectionParams(
				PigSurfaceModel **surface_model,
				double &scale_y, double &scale_x, //radians/pix
				PigPoint &proj_origin,
				double &line_zero_el,
				double &az_first_sample,
				double &az_last_sample,
				double &min_elev,
				double &max_elev,
				PigCoordSystem **cs); // projection coord system

    ////////////////////////////////////////////////////////////////////
    // Read the standard Vertical or Ortho mosaic properties from the label.
    // The base class implementation assumes MER-style SurfaceProjection
    // labels; other missions can get the info differently.  Values are
    // returned via the reference parameters.  Returns 0 on success, non-0
    // on failure.  Other than surface_model and cs, items that are not
    // found in the label are left untouched, so default values can be
    // set before calling this routine.
    // Note that surface model does not apply to ortho, in which case NULL
    // is returned.
    // Projection_type is 1 for vertical, 2 for orthorectified.  Others
    // may be added in the future.
    // NOTE: Scale is in m/pixel, which matches the label.  Most code
    // internally uses pixel/m... so you must invert it if that's what you want.
    ////////////////////////////////////////////////////////////////////

    virtual int getVertOrthoProjectionParams(
                                PigSurfaceModel **surface_model,
                                double &scale_line,     // m/pixel
                                double &scale_samp,     // m/pixel
                                double &line_offset,
                                double &samp_offset,
                                double &x_min,
                                double &x_max,
                                double &y_min,
                                double &y_max,
                                int &proj_type, // 1=vert, 2=ortho, ...
                                PigCoordSystem **cs); // projection coord system

    ////////////////////////////////////////////////////////////////////
    // Read the standard Polar mosaic properties from the label.
    // The base class implementation assumes MER-style SurfaceProjection
    // labels; other missions can get the info differently.  Values are
    // returned via the reference parameters.  Returns 0 on success, non-0
    // on failure.  Other than surface_model and cs, items that are not
    // found in the label are left untouched, so default values can be
    // set before calling this routine.
    // 
    // scale_x and scale_y are provided as two separate parameters as
    // they are bookkept as separate parameters in the label. In general,
    // they will always have the same value for a polar projected mosaic,
    // but they are left separate because it is inexpensive to do so and
    // who knows? Maybe there will be an elliptical polar projection in
    // the future.
    ////////////////////////////////////////////////////////////////////

    virtual int getPolarProjectionParams(
				PigSurfaceModel **surface_model,
				double &scale_y,	// radians/pixel
				double &scale_x,	// radians/pixel
				PigPoint &proj_origin,
				double &up_azimuth,
				int &nadir_line,
				int &nadir_samp,
				double &max_elev,
				PigCoordSystem **cs); // projection coord system
    

    ////////////////////////////////////////////////////////////////////
    // This is a set of routines used to get sets of mechanism angles or
    // positions for a mission.  A set might be all the mobility angles
    // (steering, bogie, differential), or the camera mast az/el, or arm
    // joints.  The intent of these routines is to provide info for the
    // PLACES database in a mission-independent manner.
    ////////////////////////////////////////////////////////////////////

    // Get the number of sets for this mission
    virtual int getMechanismSets() { return 0; }

    // Get info for a mechanism set.  The string should be assumed to
    // point to a static area (i.e. caller does not have to allocate or free).
    // Returns empty string and 0 if the set number is out of range.
    virtual void getMechanismSetInfo(int set, char *&name, int &num_elements)
	{ name = ""; num_elements = 0; }

    // Get info for a single element (angle, usually).  Again, the returned
    // strings should be assumed to point to a static area, so the caller does
    // not have to allocate or free.  Returns empty string and 0 if the inputs
    // are out of range.  Unit may be an empty string if N/A.
    virtual void getMechanismInfo(int set, int element,
			char *&name, double &value, char *&unit)
	{ name = ""; unit = ""; value = 0.0; }

    ////////////////////////////////////////////////////////////////////

    virtual const char *const getModelName() { return "FileModel"; }

};



// This macro implements all the full-structure get routines.
// This probably could (and maybe should?) be implemented via templates,
// but hey I'm old-school.
// example of call:
//  READ_LABEL_STRUCTURE(LblInstrumentState_typ, PigFileModel, InstrumentState,
//		"InstrumentState", "INSTRUMENT_STATE")
//  READ_LABEL_STRUCTURE_NOSET(LblImageData_typ, PigFileModel, ImageData,
//							"ImageData")
//  READ_LABEL_STRUCTURE_STR(LblArticulation_typ, PigFileModel, ArmArticulation,
//		"ArmArticulation", "ARM_INSTRUMENT_STATE")
//
// There are three macros because there are three different flavors of API.
// 1) Standard: a LblSetXxx(name) call defines the name then you call
//    LblXxxApi() to read
// 2) NOSET: There is no set function (thus, only one label group available),
//    use LblXxxApi() to read
// 3) STR: The LblSetXxx functionality is built into the Api, so there's an
//    extra string argument at the end of the LblXxxApi() call.

#define PIG_READ_LABEL_STRUCTURE(LBLTYPE, CLASS, NAME, QNAME, BASENAME, SETNAME) \
const LBLTYPE *CLASS::getLbl##NAME()					\
{									\
    if (_lbl##NAME != NULL)						\
        return _lbl##NAME;						\
    openFile();								\
    _lbl##NAME = new LBLTYPE;						\
    if (_lbl##NAME == NULL) {						\
        printMsg("Memory error", QNAME, PigMsgError);			\
        return NULL;							\
    }									\
    LblSet##BASENAME(SETNAME);						\
    int status = Lbl##BASENAME##Api(_unit, LBL_READ, _lbl##NAME, 1);	\
    if (RTN_FAILURE(status)) {						\
        printMsg((char *)LblErrorMessage(), QNAME, PigMsgError);	\
        return NULL;							\
    }									\
    return _lbl##NAME;							\
}

// Same as the above except for those label API's where there is no Set
// function (and thus no corresponding Api function
#define PIG_READ_LABEL_STRUCTURE_NOSET(LBLTYPE, CLASS, NAME, QNAME)	\
const LBLTYPE *CLASS::getLbl##NAME()					\
{									\
    if (_lbl##NAME != NULL)						\
        return _lbl##NAME;						\
    openFile();								\
    _lbl##NAME = new LBLTYPE;						\
    if (_lbl##NAME == NULL) {						\
        printMsg("Memory error", QNAME, PigMsgError);			\
        return NULL;							\
    }									\
    int status = Lbl##NAME(_unit, LBL_READ, _lbl##NAME, 1);		\
    if (RTN_FAILURE(status)) {						\
        printMsg((char *)LblErrorMessage(), QNAME, PigMsgError);	\
        return NULL;							\
    }									\
    return _lbl##NAME;							\
}

#define PIG_READ_LABEL_STRUCTURE_STR(LBLTYPE, CLASS, NAME, QNAME, BASENAME, SETNAME) \
const LBLTYPE *CLASS::getLbl##NAME()					\
{									\
    if (_lbl##NAME != NULL)						\
        return _lbl##NAME;						\
    openFile();								\
    _lbl##NAME = new LBLTYPE;						\
    if (_lbl##NAME == NULL) {						\
        printMsg("Memory error", QNAME, PigMsgError);			\
        return NULL;							\
    }									\
    int status = Lbl##BASENAME##Api(_unit, LBL_READ, _lbl##NAME, 1, SETNAME); \
    if (RTN_FAILURE(status)) {						\
        printMsg((char *)LblErrorMessage(), QNAME, PigMsgError);	\
        return NULL;							\
    }									\
    return _lbl##NAME;							\
}


#endif

