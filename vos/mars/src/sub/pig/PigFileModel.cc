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

#include "PigFileModel.h"

#include "PigCameraModel.h"
#include "PigSurfaceModel.h"

#include "PigUtilities.h"

#include "PigMission.h"

#include "PigVector.h"
#include "PigQuaternion.h"

#include "PigCSReference.h"
#include "PigCSDefinition.h"
#include "RadiometryModel.h"

#include "PigCameraMapper.h"
#include "PigCameraMapEntry.h"
#include "PigCoordSystem.h"

#include "return_status.h"

#include "zvproto.h"
#include "applic.h"		// for SUCCESS

#define MAX_LBL_KEY_SIZE 32	/* Same as RTL MAX_LABEL_KEY_SIZE but avoids */
			/* problems with certain definitions in defines.h */

////////////////////////////////////////////////////////////////////////
// The constructor is protected because the create() function should be
// used instead.  The file passed in must be open.
////////////////////////////////////////////////////////////////////////

PigFileModel::PigFileModel(const char *filename, int unit, const char *mission)
		: PigModelBase()
{
    _filename = strdup(filename);
    _mission = strdup(mission);
    _unit = unit;
    _file_open = TRUE;

    _lblIdentification = NULL;
    _lblInstrumentState = NULL;
    _lblImageData = NULL;
    _lblCameraModel = NULL;
    _lblCameraModel_instance = 0;	// 0 really isn't valid... starts at 1
    _lblDerivedGeometry = NULL;
    _lblSiteDerivedGeometry = NULL;
    _lblDerivedImage = NULL;
    _lblSurfaceProjection = NULL;
    _lblSurfaceModel = NULL;
    _lblCompression = NULL;

    _lblRsmArticulation = NULL;
    _lblArmArticulation = NULL;
    _lblHgaArticulation = NULL;
    _lblChassisArticulation = NULL;
    _lblRoverCoordSys = NULL;
    _lblArmCoordSys = NULL;
    _lblInitState = NULL;

    // Get basic file info
    zvget(_unit, "NL", &_nl, "NS", &_ns, "NB", &_nb, NULL);
    zvget(_unit, "FORMAT", _format, NULL);

    _borders_set = FALSE;

    setupOffsets();		// This should be done again in subclass ctor,
				// if the source label items are changed
}

PigFileModel::~PigFileModel()
{
    if (_filename)
	delete _filename;
    if (_mission)
	delete _mission;

    if (_lblIdentification)
	delete _lblIdentification;
    if (_lblInstrumentState)
	delete _lblInstrumentState;
    if (_lblImageData)
	delete _lblImageData;
    if (_lblCameraModel)
	delete _lblCameraModel;
    if (_lblDerivedGeometry)
	delete _lblDerivedGeometry;
    if (_lblSiteDerivedGeometry)
	delete _lblSiteDerivedGeometry;
    if (_lblDerivedImage)
	delete _lblDerivedImage;
    if (_lblSurfaceProjection)
	delete _lblSurfaceProjection;
    if (_lblSurfaceModel)
	delete _lblSurfaceModel;
    if (_lblCompression)
	delete _lblCompression;

    if (_lblRsmArticulation)
	delete _lblRsmArticulation;
    if (_lblArmArticulation)
	delete _lblArmArticulation;
    if (_lblHgaArticulation)
	delete _lblHgaArticulation;
    if (_lblChassisArticulation)
	delete _lblChassisArticulation;
    if (_lblRoverCoordSys)
	delete _lblRoverCoordSys;
    if (_lblArmCoordSys)
	delete _lblArmCoordSys;
    if (_lblInitState)
	delete _lblInitState;
}

////////////////////////////////////////////////////////////////////////
// This factory method creates and returns an instance of the
// proper mission-specific subclass for the given file.  Missions
// using the multimission label API may not need a mission-specific
// subclass.
//
// It is not expected that instrument-specific subclasses will be needed,
// but if so, a static create for the mission should be called to determine
// the instrument and create the proper subclass.
////////////////////////////////////////////////////////////////////////

PigFileModel *PigFileModel::create(const char *filename)
{
    int status;
    char mission[33];
    int unit;

    PigMission *m = PigMission::getMissionObject(filename, &unit);

    if (m == NULL)
        return NULL;
    return m->createFileModel(filename, unit);
}

////////////////////////////////////////////////////////////////////////
// Opens the file if it's not already.  Done automatically in most cases;
// needed only for apps that need direct access via getUnit() and don't
// want to do their own open.
////////////////////////////////////////////////////////////////////////
void PigFileModel::openFile()
{
    if (_file_open)
	return;

    int status = zvopen(_unit, "op", "read", "open_act", "", NULL);
    if (status != SUCCESS)
	return;
    _file_open = TRUE;
}

////////////////////////////////////////////////////////////////////////
// Fill in the image borders.  Return TRUE if they are properly set.
// This really should be done in a subclass, but since this is the
// only thing that needs a subclass for missions conforming to the
// Label API, we check for mission here and call something in the
// Pig<Mission> static class.
//
// Adjust the border according to the BORDER* keywords for POINT_METHOD,
// if any.  The value for BORDER* is a number, interpreted as a delta to
// be added to the corresponding border.  Positive numbers always shrink
// the image (more border); negative numbers expand it.  The parameters are:
//    BORDER_LEFT - left edge
//    BORDER_RIGHT - right edge
//    BORDER_TOP - top edge
//    BORDER_BOTTOM - bottom edge
// For example:
//    point="border_left=1,border_right=-1"
// No specific check is made for setting the borders too large or too
// small - caveat emptor!
//
// An additional set of these exists using MARGIN instead of BORDER.
// The difference is that MARGIN is adjusted based on the downsample
// factor - so that you can mix thumnails in with regular images and
// still trim off a bad edge.  It is possible to have both; they are
// cumulative.
////////////////////////////////////////////////////////////////////////

int PigFileModel::setupBorders()
{
    if (_borders_set)
	return TRUE;				// already done

    PigMission *m = PigMission::getMissionObject(_mission);
    _borders_set = m->getBorders(this,  _border_sl, _border_ss,
					_border_el, _border_es);

    char point_method[256], *value;
    int count;
    getParam("POINT_METHOD", point_method, &count, 1, 0);
    if (count == 0)
        return _borders_set;

    value = parseParamString(point_method, "BORDER_LEFT");
    if (value != NULL) {
	int v = atoi(value);
	_border_ss += v;
    }
    value = parseParamString(point_method, "BORDER_RIGHT");
    if (value != NULL) {
	int v = atoi(value);
	_border_es -= v;
    }
    value = parseParamString(point_method, "BORDER_TOP");
    if (value != NULL) {
	int v = atoi(value);
	_border_sl += v;
    }
    value = parseParamString(point_method, "BORDER_BOTTOM");
    if (value != NULL) {
	int v = atoi(value);
	_border_el -= v;
    }

    value = parseParamString(point_method, "MARGIN_LEFT");
    if (value != NULL) {
	int v = atoi(value);
	_border_ss += (int)(v / getDownsampleXFactor(1.0) + 0.5);
    }
    value = parseParamString(point_method, "MARGIN_RIGHT");
    if (value != NULL) {
	int v = atoi(value);
	_border_es -= (int)(v / getDownsampleXFactor(1.0) + 0.5);
    }
    value = parseParamString(point_method, "MARGIN_TOP");
    if (value != NULL) {
	int v = atoi(value);
	_border_sl += (int)(v / getDownsampleYFactor(1.0) + 0.5);
    }
    value = parseParamString(point_method, "MARGIN_BOTTOM");
    if (value != NULL) {
	int v = atoi(value);
	_border_el -= (int)(v / getDownsampleYFactor(1.0) + 0.5);
    }

    return _borders_set;
}

////////////////////////////////////////////////////////////////////////
// Close the file if it's open, but do not free the unit.  May be called
// externally.  Apps may open using their own options if setFileOpen(TRUE)
// is called.
////////////////////////////////////////////////////////////////////////
void PigFileModel::closeFile()
{
    if (!_file_open)
	return;

    zvclose(_unit, NULL);
    _file_open = FALSE;
}

////////////////////////////////////////////////////////////////////////
// Initializes the x/y offsets, which are the offsets between the camera
// model L/S coordinates, and the "physical" coordinates in the image.
// Note that these physical coordinates are 0-based, e.g. you have to
// add 1 for VICAR files.
//
// For the Multimission case, this simply returns the FirstLine/
// FirstLineSample label items (-1 to make them 0-based).
////////////////////////////////////////////////////////////////////////

void PigFileModel::setupOffsets()
{
    _x_offset = getFirstLineSample(0) - 1;		// convert to 0-based
    if (_x_offset < 0)
	_x_offset = 0;					// sanity check

    _y_offset = getFirstLine(0) - 1;			// convert to 0-based
    if (_y_offset < 0)
	_y_offset = 0;					// sanity check
}

////////////////////////////////////////////////////////////////////////
// Returns whether or not a line/samp location (in camera model
// coordinates!) is inside the "good" part of the image or not.
// The Multimission version here allows for a a "border" of bad pixels in
// Camera coordinates (which may not be relevant for partial-frame images).
// Applications can use this to determine whether the given image pixel
// should be used in a mosaic or not.  This border must be set in the
// constructor (if used, or clear the flag if not).
//
// Note that this is a multi-state return.  It would be nice to say
// if (x->testPixelLocation(...))
// but with the extra cases, you really need to say
// if (x->testPixelLocation(...) == 0)
// in the spirit of strcmp().
// Return values:
// 0  = inside the image (really, "true" return, like strcmp())
// 1  = outside the image physically
// 2  = outside the image logically (i.e. inside the physical bounds but
//      in the border area)
// -1 = unknown.  This could happen if the pointing was done via means
//      that did not involve an image, so the pointing model doesn't know
//      what the image size/offsets are.
////////////////////////////////////////////////////////////////////////

int PigFileModel::testPixelLocation(const double line, const double sample)
{
    double l = line - getYOffset();
    double s = sample - getXOffset();

    // Check physical file limits
    // It is zero-based, also the last raw, column are discarded since 
    // there is not enough data to do bilinear interpolation.

    if ((l < 0.0) || (l > getNL()-2.0) || (s < 0.0) || (s > getNS()- 2.0))
	return 1;

    // Check borders (in camera coords, not image)
    // NOTE:  mpfmos didn't include the = case.

    if (!_borders_set)
	setupBorders();
    if (_borders_set) {
	if ((line < _border_sl) || (line >= _border_el) ||
	    (sample < _border_ss) || (sample >= _border_es))

	    return 2;
    }

    return 0;			// it's good!
}

////////////////////////////////////////////////////////////////////////
// Return the camera coordinates of the edges of the image.  These should
// not be used for checking if a pixel is inside the image; use
// testPixelLocation() instead.  They are only here for purposes such as
// drawing edges, where it is not practical to test each point.  Since
// the coordinates are doubles, they represent the extreme edges of the
// good pixels (not the pixel centers), thus the coordinates may be
// between pixels.  Returns 0 for success, or >0 if the coordinates are
// unavailable.
//
// The _border_xx variables indicate where the border (e.g. of the CCD)
// is in camera space.  If the ls_offset indicates that the image starts
// *past* the border, then there is no border and we return the offset
// (log = phys + offset, where phys=0).
////////////////////////////////////////////////////////////////////////

int PigFileModel::getImageBorders(double &sl, double &ss,
				  double &el, double &es)
{
    if (!setupBorders())
	return 1;		//!!!! should this return physical image limits?

    sl = _border_sl;			//!!!! Check for edges properly!!!!
    ss = _border_ss;			//!!!! Might be off by 1, or .5
    el = _border_el;
    es = _border_es;

    if (sl < getYOffset())
        sl = getYOffset();
    if (ss < getXOffset())
        ss = getXOffset();
    if (el > (getNL()-1) + getYOffset())
        el = (getNL()-1) + getYOffset();
    if (es > (getNS()-1) + getXOffset())
        es = (getNS()-1) + getXOffset();

    return 0;
}

////////////////////////////////////////////////////////////////////////
// Returns the Field of View of the image, in either the Line or Sample
// direction (note: this is not the diagonal FOV, which is bigger than
// either of these!).  This is in the FM rather than the CameraModel
// because it depends on the size of the image, which is known only to
// the FM (note that the CameraModel must be supplied).  The FOV is
// returned in radians.  If "which" is 0, the Line direction is returned;
// 1 returns the Sample direction.  The base class implementation of this
// function should be sufficient for most uses; override it only if you 
// need to do something unusual.
//!!!! Should this function be in the CM, with a pointer to the FM, instead?
////////////////////////////////////////////////////////////////////////

double PigFileModel::getFOV(PigCameraModel *cm, const int which)
{
    double sl, ss, el, es;
    double size;

    if (!cm)
	return 0.0;

    int status = getImageBorders(sl, ss, el, es);
    if (status != 0)
	return 0.0;			// uable to determine

//!!!! Should we do the + 1.0 here????
    if (which == 0)			// Line
	size = (el - sl + 1.0);
    else				// Samp
	size = (es - ss + 1.0);

    return size * cm->getPixelAngle(which);
}

////////////////////////////////////////////////////////////////////////
// Returns the nominal size of the frame for this camera, without any
// downsampling or subframing.  This is determined using the camera
// mapping facility; if that's not available it defaults to 1024 since
// that's valid for most older camera (before the mapping file).  Mission
// specific subclasses can override, of course.
////////////////////////////////////////////////////////////////////////

void PigFileModel::getNominalCameraSize(PigMission *mission, const char *inst,
					int &nl, int &ns)
{
    nl = 1024; ns = 1024;		// default if entry not found

    PigCameraMapper *map = NULL;
    PigCameraMapEntry *entry = NULL;

    if (mission == NULL || inst == NULL)
	return;

    PigXerces::initialize();
    const char *host_id = mission->getHostID();
    if (host_id == NULL)
	return;

    //!!!! This is technically incorrect; the mission name should be supplied.
    //!!!! However, that doesn't work for MSL... and it doesn't matter for
    //!!!! anyone else since all prior missions are 1024x1024.  The MER
    //!!!! "bug" mentioned in the PigCameraMapper applies to MSL too...
    map = new PigCameraMapper(NULL, host_id);
    if (map == NULL)
	return;

    entry = map->findFromID(inst);
    if (entry == NULL) {
	delete map;
	return;
    }

    nl = entry->getNL();
    ns = entry->getNS();

    delete map;
    return;
}

////////////////////////////////////////////////////////////////////////
// Like getFOV, but it returns the nominal FOV of the camera: what it
// would be without any downsampling or subframing.  This is determined
// using getNominalCameraSize().  Note that, like getFOV, this simply
// multiplies the number of pixels by the instantaneous field of view,
// so it is an approximation.
////////////////////////////////////////////////////////////////////////

double PigFileModel::getNominalFOV(PigCameraModel *cm, const int which)
{
    int nl, ns;

    getNominalCameraSize(PigMission::getMissionObject(this), getInstrumentId(),
			nl, ns);

    // We must adjust the number of pixels by the downsample factor, because
    // the camera model has been downsampled and thus the IFOV relates to
    // downsampled pixels.  So we have to give it fewer pixels.

    float factor = getDownsampleYFactor(1.0);
    if (factor != 0.0)
	nl = (int)(nl / factor + 0.5);
    factor = getDownsampleXFactor(1.0);
    if (factor != 0.0)
	ns = (int)(ns / factor + 0.5);

    if (which == 0)
	return nl * cm->getPixelAngle(which);

    return ns * cm->getPixelAngle(which);
}

////////////////////////////////////////////////////////////////////////
// These functions return complete Label API property structures.
// The Valid flags in each field should be checked before using the
// value.  Regardless of the label API, there is no guarantee that
// any particular value is present.  Non-standard missions should
// fill in at least the appropriate fields.
//
// IMPORTANT!  A pointer is returned to an internal structure.  The
// caller MUST NOT change the structure that is returned.  If changes
// are desired, memcpy() the structure to your own first.
//
// NULL may be returned if the given property doesn't exist at all,
// or is otherwise not relevant.
////////////////////////////////////////////////////////////////////////

// Identification
PIG_READ_LABEL_STRUCTURE_NOSET(LblIdentification_typ, PigFileModel,
	Identification, "Identification")

// Note that older missions use INSTRUMENT_STATE; newer ones (starting with MER)
// should override this to INSTRUMENT_STATE_PARMS.
// InstrumentState
PIG_READ_LABEL_STRUCTURE(LblInstrumentState_typ, PigFileModel,
	InstrumentState, "InstrumentState", InstrumentState, "INSTRUMENT_STATE")

// ImageData
PIG_READ_LABEL_STRUCTURE_NOSET(LblImageData_typ, PigFileModel,
	ImageData, "ImageData")

// DerivedGeometry
PIG_READ_LABEL_STRUCTURE_NOSET(LblDerivedGeometry_typ, PigFileModel,
	DerivedGeometry, "DerivedGeometry")

// SiteDerivedGeometry
PIG_READ_LABEL_STRUCTURE_STR(LblDerivedGeometry_typ, PigFileModel,
	SiteDerivedGeometry, "SiteDerivedGeometry", DerivedGeometry,
	"SITE_DERIVED_GEOMETRY_PARMS")

// DerivedImage
PIG_READ_LABEL_STRUCTURE(LblDerivedImage_typ, PigFileModel,
	DerivedImage, "DerivedImage", DerivedImage, "DERIVED_IMAGE_PARMS")

// SurfaceProjection
PIG_READ_LABEL_STRUCTURE(LblSurfaceProjection_typ, PigFileModel,
	SurfaceProjection, "SurfaceProjection", SurfaceProjection,
						"SURFACE_PROJECTION_PARMS")

// SurfaceModel
PIG_READ_LABEL_STRUCTURE(LblSurfaceModel_typ, PigFileModel,
	SurfaceModel, "SurfaceModel", SurfaceModel, "SURFACE_MODEL_PARMS")

// Compression
PIG_READ_LABEL_STRUCTURE(LblCompression_typ, PigFileModel,
	Compression, "Compression", Compression, "COMPRESSION_PARMS");

////////////////////////////////////////////////////////////////////////
// CameraModel - can't use macro due to "instance" param
////////////////////////////////////////////////////////////////////////
const LblCameraModel_typ *PigFileModel::getLblCameraModel(int instance)
{
    if (_lblCameraModel != NULL) {
	if (_lblCameraModel_instance == instance)
	    return _lblCameraModel;
	delete _lblCameraModel;		// wrong instance
    }

    openFile();

    _lblCameraModel = new LblCameraModel_typ;
    if (_lblCameraModel == NULL) {
	printMsg("Memory error", "CameraModel", PigMsgError);
	return NULL;
    }

    int status = LblGeometricCameraModel(_unit, LBL_READ, 
					 _lblCameraModel, instance);
    if (RTN_FAILURE(status)) {
	printMsg((char *)LblErrorMessage(), "CameraModel", 
		 PigMsgError);
	return NULL;
    }

    _lblCameraModel_instance = instance;
    return _lblCameraModel;
}

////////////////////////////////////////////////////////////////////////
// These are the pseudo-mission-specific full-structure read routines
// See the class header comments.
////////////////////////////////////////////////////////////////////////

// RsmArticulation
PIG_READ_LABEL_STRUCTURE_STR(LblArticulation_typ, PigFileModel,
	RsmArticulation, "RsmArticulation", Articulation,
						"RSM_ARTICULATION_STATE")

// ArmArticulation
PIG_READ_LABEL_STRUCTURE_STR(LblArticulation_typ, PigFileModel,
	ArmArticulation, "ArmArticulation", Articulation,
						"ARM_ARTICULATION_STATE")

// HgaArticulation
PIG_READ_LABEL_STRUCTURE_STR(LblArticulation_typ, PigFileModel,
	HgaArticulation, "HgaArticulation", Articulation,
						"HGA_ARTICULATION_STATE")

// ChassisaArticulation
PIG_READ_LABEL_STRUCTURE_STR(LblArticulation_typ, PigFileModel,
	ChassisArticulation, "ChassisArticulation", Articulation,
						"CHASSIS_ARTICULATION_STATE")

// RoverCoordSys
PIG_READ_LABEL_STRUCTURE_STR(LblCoordinate_typ, PigFileModel,
	RoverCoordSys, "RoverCoordSys", Coordinate, "ROVER_COORDINATE_SYSTEM")

// ArmCoordSys
PIG_READ_LABEL_STRUCTURE_STR(LblCoordinate_typ, PigFileModel,
	ArmCoordSys, "ArmCoordSys", Coordinate, "ARM_COORDINATE_SYSTEM")

// InitState
PIG_READ_LABEL_STRUCTURE_STR(LblInitState_typ, PigFileModel,
	InitState, "InitState", InitState, "INITIAL_STATE_PARMS")


////////////////////////////////////////////////////////////////////////
// These functions return basic information from the label.
// The information may be repeated in various Label API structures,
// but the common stuff is repeated here for convenience.
// Multivalued items return index 0 only.
// For non-strings, an argument gives the default value to return 
// if the label item is not present or can't be read.
////////////////////////////////////////////////////////////////////////

// Scalar string item
#define FILE_LABEL_S(Property,Name)					\
	const char *PigFileModel::get##Name()				\
	{								\
	    if (_lbl##Property == NULL)					\
		getLbl##Property();					\
	    if (_lbl##Property == NULL)					\
		return NULL;						\
	    if (!_lbl##Property->Name.Valid)				\
		return NULL;						\
	    return _lbl##Property->Name.Value;				\
	}

// Scalar int item returned as string
#define FILE_LABEL_IS(Property,Name,StrTmp)				\
	const char *PigFileModel::get##Name()				\
	{								\
	    if (_lbl##Property == NULL)					\
		getLbl##Property();					\
	    if (_lbl##Property == NULL)					\
		return NULL;						\
	    if (!_lbl##Property->Name.Valid)				\
		return NULL;						\
            sprintf(StrTmp,"%d", _lbl##Property->Name.Value);          \
	    return StrTmp;				                \
	}

// Item from string array
#define FILE_LABEL_S2(Property,Name,ItemName)				\
	const char *PigFileModel::get##Name()				\
	{								\
	    if (_lbl##Property == NULL)					\
		getLbl##Property();					\
	    if (_lbl##Property == NULL)					\
		return NULL;						\
	    if (!_lbl##Property->ItemName.Valid)			\
		return NULL;						\
	    return _lbl##Property->ItemName.Value;			\
	}

// Scalar numeric item
#define FILE_LABEL(Property,Name,Type)					\
	Type PigFileModel::get##Name(Type def)				\
	{								\
	    if (_lbl##Property == NULL)					\
		getLbl##Property();					\
	    if (_lbl##Property == NULL)					\
		return def;						\
	    if (!_lbl##Property->Name.Valid)				\
		return def;						\
	    return _lbl##Property->Name.Value;				\
	}

// Scalar numeric item, either in A or B label (but A is primary)
#define FILE_LABEL_AB(Property,NameA,NameB,Type)			\
	Type PigFileModel::get##NameA(Type def)				\
	{								\
	    if (_lbl##Property == NULL)					\
		getLbl##Property();					\
	    if (_lbl##Property == NULL)					\
		return def;						\
	    if (_lbl##Property->NameA.Valid)				\
	        return _lbl##Property->NameA.Value;			\
	    if (!_lbl##Property->NameB.Valid)				\
		return def;						\
	    return _lbl##Property->NameB.Value;				\
	}
// Item from numeric array
#define FILE_LABEL2(Property,Name,Type,ItemName)			\
	Type PigFileModel::get##Name(Type def)				\
	{								\
	    if (_lbl##Property == NULL)					\
		getLbl##Property();					\
	    if (_lbl##Property == NULL)					\
		return def;						\
	    if (!_lbl##Property->ItemName.Valid)			\
		return def;						\
	    return _lbl##Property->ItemName.Value;			\
	}

// Array item, converted to type "Type".  Type must have ctor for Value's type.
#define FILE_LABEL3(Property,Name,Type)					\
	Type PigFileModel::get##Name(Type def)				\
	{								\
	    if (_lbl##Property == NULL)					\
		getLbl##Property();					\
	    if (_lbl##Property == NULL)					\
		return def;						\
	    if (!_lbl##Property->Name.Valid)				\
		return def;						\
	    return Type(_lbl##Property->Name.Value);			\
	}

// Scalar Boolean item
#define FILE_LABEL_B(Property,Name)					\
	int PigFileModel::get##Name(int def)				\
	{								\
	    if (_lbl##Property == NULL)					\
		getLbl##Property();					\
	    if (_lbl##Property == NULL)					\
		return def;						\
	    if (!_lbl##Property->Name.Valid)				\
		return def;						\
	    if (strncasecmp(_lbl##Property->Name.Value, "T", 1) == 0)	\
		return TRUE;						\
	    if (strncasecmp(_lbl##Property->Name.Value, "F", 1) == 0)	\
		return FALSE;						\
	    return def;							\
	}

// This macro is specific to the triplet of Value/Valid pair
// ie validation is done for every subfield value.  The type
// should be a triplet such as PigVector or PigPoint.
#define FILE_LABEL4(Property,Name,Type)					\
	Type PigFileModel::get##Name(Type def)				\
	{								\
	    if (_lbl##Property == NULL)					\
		getLbl##Property();					\
	    if (_lbl##Property == NULL)					\
		return def;						\
	    if (!(_lbl##Property->Name[0].Valid &&                      \
		  _lbl##Property->Name[1].Valid &&                      \
		  _lbl##Property->Name[2].Valid ))			\
		return def;						\
	    return Type(_lbl##Property->Name[0].Value,                  \
		        _lbl##Property->Name[1].Value,                  \
			_lbl##Property->Name[2].Value);	        	\
	}

// Fake label item, with nothing in the label API structures... yet
#define FAKE_LABEL(Property,Name,Type)					\
	Type PigFileModel::get##Name(Type def) { return def; }

FILE_LABEL_S2(Identification,  FrameId, FrameId[0])
FILE_LABEL_S (Identification,  ImageId)
FILE_LABEL_S2(Identification,  InstrumentHostId, InstrumentHostId[0])
FILE_LABEL_S2(Identification,  InstrumentId, InstrumentId[0])
FILE_LABEL_S2(Identification,  InstrumentName, InstrumentName[0])
FILE_LABEL_S (Identification,  InstrumentVersionId)
FILE_LABEL_S (Identification,  SpacecraftClockStartCount)
FILE_LABEL_S (Identification,  SpacecraftClockStopCount)
FILE_LABEL_S (Identification,  GeometryProjectionType)
FILE_LABEL_S (Identification,  ProductId)

FILE_LABEL   (ImageData,       FirstLine, int)
FILE_LABEL   (ImageData,       FirstLineSample, int)
FILE_LABEL_S (ImageData,       SampleBitMask)

FILE_LABEL   (InstrumentState, ExposureDuration, float)
FILE_LABEL_IS (InstrumentState, FilterNumber, _strTmp)
FILE_LABEL_S2(InstrumentState, FilterName, FilterName[0])
FILE_LABEL_S (InstrumentState, FlatFieldCorrectionFlag)
FILE_LABEL   (InstrumentState, InstrumentAzimuthCount, int)
FILE_LABEL_S (InstrumentState, InstrumentDeploymentState)
FILE_LABEL   (InstrumentState, InstrumentElevationCount, int)
FILE_LABEL   (InstrumentState, InstrumentFocalLengthCount, int)
FILE_LABEL_AB(InstrumentState, InstrumentFocusPosition, FocusPositionCount, int)
FILE_LABEL_AB(InstrumentState, InstrumentZoomPosition, ZoomPositionCount, int)

FILE_LABEL2  (InstrumentState, InstrumentTemperature, float, InstrumentTemperature[0])
FILE_LABEL2   (InstrumentState, DownsampleXFactor, float, PixelAveragingWidth)
FILE_LABEL2   (InstrumentState, DownsampleYFactor, float, PixelAveragingHeight)

FILE_LABEL   (InstrumentState, OffsetNumber, int)
FILE_LABEL   (InstrumentState, DCOffset, int)
FILE_LABEL_S (InstrumentState, InstrumentModeId)
FILE_LABEL_B (InstrumentState, ShutterEffectCorrectionFlag)
FILE_LABEL_S (InstrumentState, ShutterCorrectionMode)
FILE_LABEL_S (InstrumentState, BayerMode)
FILE_LABEL   (InstrumentState, OriginalSampleBits, int)

FILE_LABEL   (DerivedGeometry, LanderInstrumentAzimuth, float)
FILE_LABEL   (DerivedGeometry, LanderInstrumentElevation, float)
FILE_LABEL3  (DerivedGeometry, SrfcFxdLclLvlVector, PigVector)
FILE_LABEL   (SiteDerivedGeometry, SolarElevation, float)
FILE_LABEL   (SiteDerivedGeometry, SolarAzimuth, float)

FILE_LABEL4  (InstrumentState, InstHostPosition, PigPoint)
//!!!! The Euler angles temporary stored in InsrumentPosition[]
// Change it as soon generic label API are updated.
FILE_LABEL2(InstrumentState, InstrumentHostHeading, float, InstrumentPosition[0])
FILE_LABEL2(InstrumentState, InstrumentHostPitch, float, InstrumentPosition[1])
FILE_LABEL2(InstrumentState, InstrumentHostRoll, float, InstrumentPosition[2])

FILE_LABEL_S (CameraModel,  CalibrationSourceId)

FILE_LABEL_S (DerivedImage,  PointingModelName)

FILE_LABEL_S (DerivedImage,  ColorSpace)
FILE_LABEL   (DerivedImage, RadianceOffset, float)
FILE_LABEL   (DerivedImage, RadianceScaleFactor, float)

FILE_LABEL   (DerivedImage, CorrelationAverageScale, float)
FILE_LABEL   (DerivedImage, CorrelationPixelCount, float)
FILE_LABEL   (DerivedImage, StereoBaseline, float)
FILE_LABEL_S (DerivedImage, StereoProductId)

FILE_LABEL_S2(Compression,     CompressionName, InstCmprsName)
FILE_LABEL2  (Compression,     CompressionRate, float, InstCmprsRate)

//!!!! There's nothing in the generic label API for these... yet...
FAKE_LABEL   (InstrumentState, TelemetryInstrumentElevation, float)
FAKE_LABEL   (InstrumentState, TelemetryInstrumentPosition, PigPoint)
FAKE_LABEL   (DerivedGeometry, InstLocalLevelQuaternion, PigQuaternion)

// In only a few cases is the lack of value significant...
#define CHECK_FILE_LABEL(Property,Name)					\
	int PigFileModel::check##Name()					\
	{								\
	    if (_lbl##Property == NULL)					\
		getLbl##Property();					\
	    if (_lbl##Property == NULL)					\
		return FALSE;						\
	    return _lbl##Property->Name.Valid;				\
	}

CHECK_FILE_LABEL(InstrumentState, InstrumentAzimuthCount)
CHECK_FILE_LABEL(InstrumentState, InstrumentElevationCount)
//!!!! There's nothing in the generic label API for these... yet...
int PigFileModel::checkTelemetryInstrumentElevation() { return FALSE; }
int PigFileModel::checkTelemetryInstrumentPosition() { return FALSE; }

//////////////////////////////////////////////////////////////////////
// Get the number of sample bits.  If we're lucky, SampleBits has it.
// If not, we have to count the bits in SampleBitMask.
//////////////////////////////////////////////////////////////////////
int PigFileModel::getSampleBits(int def)
{
    int bits = -1;

    if (_lblImageData == NULL)
	getLblImageData();
    if (_lblImageData != NULL) {
	if (_lblImageData->SampleBits.Valid)
	    bits = _lblImageData->SampleBits.Value;
    }

    if (bits > 0)
	return bits;

    // If we don't have it, compute from the mask

    const char *mask = getSampleBitMask();
    if (mask == NULL || strlen(mask) == 0)
	return def;

    bits = 0;
    for (const char *p = mask; *p != '\0'; p++) {
	if (*p == '1') {
	    bits++;
	}
    }
    if (bits > 0)
	return bits;
    return def;
}

//////////////////////////////////////////////////////////////////////
// Caller is responsible for memory allocation of uid char array
// uid array should be big enough to hold at least 33 char.
// If ImageId is not available, try using the (last 32 chars of) the
// filename (this impl should really only be used by the Generic mission).
// If that doesn't work, return a constant.
// 
//////////////////////////////////////////////////////////////////////
void PigFileModel::getUniqueId(char *uid)
{
   const char *id = getImageId();
   if (id != NULL) {
      strcpy(uid, id);
      return;
   }
   if (_filename != NULL) {
      int len = strlen(_filename);
      char *p = _filename;
      if (len > 32) p += (len-32);
      strcpy(uid, p);
      return;
   }
   strcpy(uid, "UNKNOWN");
}

//////////////////////////////////////////////////////////////////////
// This implementation consists of:
//  HostId + InstrumentId + FrameId + SClock
//////////////////////////////////////////////////////////////////////
void PigFileModel::getUniqueIdImpl(char *uid)
{
    strcpy(uid, "");
    strcat(uid, getInstrumentHostId());
    strncat(uid, getInstrumentId(), 1);
    strncat(uid, getFrameId(), 1);
    strcat(uid, "_");

    // append integer part of sclk
    char sclk[100];
    char sclk_integer_part[100];
    strcpy(sclk, getSpacecraftClockStartCount());
    const char *str_fraction_part = strrchr(sclk, '.');
    if (str_fraction_part) {
        int integer_part_length = strlen(sclk) - strlen(str_fraction_part);
        strncpy(sclk_integer_part, sclk, integer_part_length);
        sclk_integer_part[integer_part_length] = '\0';
        strcat(uid, sclk_integer_part);
    }
    else {  // no fraction part in sclk
        strcat(uid, sclk);
    }

    strcat(uid, "\0");
}

//////////////////////////////////////////////////////////////////////
// This implementation consists of:
//  HostId + InstrumentId + FrameId + "_" + SClock_Msec
//////////////////////////////////////////////////////////////////////
void PigFileModel::getUniqueIdMsecImpl(char *uid)
{
    strcpy(uid, "");
    strcat(uid, getInstrumentHostId());
    strncat(uid, getInstrumentId(), 1);
    strncat(uid, getFrameId(), 1);
    strcat(uid, "_");

    // append sclk, with 3 fractional digits for msec
    char sclk[100];
    char sclk_temp[100];
    strcpy(sclk, getSpacecraftClockStartCount());
    const char *str_fraction_part = strrchr(sclk, '.');
    if (str_fraction_part) {
	// Integer part
        int integer_part_length = strlen(sclk) - strlen(str_fraction_part);
        strncpy(sclk_temp, sclk, integer_part_length);
        sclk_temp[integer_part_length] = '\0';
        strcat(uid, sclk_temp);
	strcat(uid, "_");
	strcpy(sclk_temp, str_fraction_part+1);		// skip the .
	sclk_temp[3] = '\0';				// limit to 3 chars
	while (strlen(sclk_temp) < 3)
	    strcat(sclk_temp, "0");			// fill out 0's if needed
	strcat(uid, sclk_temp);
    }
    else {  // no fraction part in sclk
        strcat(uid, sclk);
	strcat(uid, "_000");
    }

    strcat(uid, "\0");
}


///////////////////////////////////////////////////////////////////////
//    Return SOURCE_PRODUCT_ID's requested ELEMENT num
//////////////////////////////////////////////////////////////////////
char* PigFileModel::getSourceProductId(int num)
{
    openFile();
    char* src_prod_id = (char* )malloc(80* sizeof(char));
    int num_ret = 0;
    //src_prod_id = NULL;
    int status = zlget(_unit, "PROPERTY", "SOURCE_PRODUCT_ID", src_prod_id,
                     "ELEMENT", num, "NELEMENT", 1, "FORMAT", "STRING",
                     "PROPERTY", "IDENTIFICATION", "ERR_ACT", "", NULL);
    if (status != 1)
        return NULL;

    return src_prod_id;
} 

//////////////////////////////////////////////////////////////////////
// Compares 2 SPACECRFAFT_CLOCK_XXX_COUNT strings.
// The function returns an integer greater than, equal  to,  or
// less  than  0, if the string pointed to by sclk1 is greater
// than, equal to, or less than the string  pointed  to  by  s2
// respectively.
// For more info see strcmp(..) man pages, since base class 
// implementation uses it.
//////////////////////////////////////////////////////////////////////
int PigFileModel::compareSCLK(const char *sclk1, const char *sclk2)
{
    return strcmp(sclk1, sclk2);
}

///////////////////////////////////////////////////////////////////
// This implementation, useful for MER, PHX, MSL at least, takes advantage
// of knowing the format of SCLK which is dddddddddd.ddd i.e. integer part
// of up to 10 numbers and fraction part of up to 3 numbers separated by '.'
///////////////////////////////////////////////////////////////////

int PigFileModel::compareSCLKImpl(const char *sclk1, const char *sclk2)
{
    long diff;

    diff = atol(sclk1) - atol(sclk2);
    if (diff > 0)
        return 1;  //sclk1 > sclk2

    if (diff < 0)
        return -1;  //sclk1 < sclk2

    // Integer parts are equal, compare fraction parts

    // separate fraction part
    const char *str_fraction_part_1 = strrchr(sclk1, '.');
    const char *str_fraction_part_2 = strrchr(sclk2, '.');

    diff = atol(str_fraction_part_1 + 1) - atol(str_fraction_part_2 + 1);
    if (diff > 0)
        return 1;  //sclk1 > sclk2

    if (diff < 0)
        return -1;  //sclk1 < sclk2

    return 0;
}


////////////////////////////////////////////////////////////////////
// Get the Flat Field Correction Parms for the file.  The array must
// be supplied.  num_index on input should contain the max size of the
// supplied array;  on output it will contain the number of elements
// actually found (which could be bigger than the supplied max size,
// but only that many will actually be returned in the array).
// PIG_MAX_FLAT_FIELD_INDEX (from RadiometryModel.h) can be useful
// for array dimensioning.
///////////////////////////////////////////////////////////////////

void PigFileModel::getFlatFieldCorrectionCounterImpl(float parms[],
                                                    int &num_parms)
{
    num_parms = 0;

    const LblInstrumentState_typ *lblInstrumentState
                                       = getLblInstrumentState();

    for (int cnt = 0; cnt < PIG_MAX_FLAT_FIELD_INDEX; cnt++) {
        if(!lblInstrumentState->FlatFieldCorrectionParm[cnt].Valid) {
            printUniqueStaticMsg("FlatFieldCorrectionParm not found in label!  0.0 assumed", PigMsgWarning);
            parms[cnt] = 0.0;
        }
        else {
            parms[cnt] = lblInstrumentState->FlatFieldCorrectionParm[cnt].Value;
            num_parms++;
        }
    }
    return;
}

//////////////////////////////////////////////////////////////////////
// Get the onboard preboost factors ONBOARD_RESPONSIVITY for the file.
// The array must be supplied, and its length must be 3. 
////////////////////////////////////////////////////////////////////////

int PigFileModel::getOnboardResponsivity(float factors[3])
{
    const LblInstrumentState_typ *lblInstrumentState = getLblInstrumentState();

    if (!lblInstrumentState->OnboardResponsivity[0].Valid ||
        !lblInstrumentState->OnboardResponsivity[1].Valid ||
        !lblInstrumentState->OnboardResponsivity[2].Valid) {
        
        factors[0] = 0.0;
        factors[1] = 0.0;
        factors[2] = 0.0;

        return 0;
    }

    factors[0] = lblInstrumentState->OnboardResponsivity[0].Value;
    factors[1] = lblInstrumentState->OnboardResponsivity[1].Value;
    factors[2] = lblInstrumentState->OnboardResponsivity[2].Value;

    return 1;
}

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

int PigFileModel::getDerivedImageCS(PigCSReference *&ref)
{
    const char *frame = "UNKNOWN";                  // just in case
    const char *solution_id = NULL;

    const LblDerivedImage_typ *lblDerivedImage = getLblDerivedImage();
    if (lblDerivedImage == NULL) {
	ref = new PigCSReference(*PigMission::getMissionObject(_mission)->
						getFixedCS()->getIdentity());
        return 0;
    }

    if (!lblDerivedImage->ReferenceCoordSystemName.Valid) {
	ref = new PigCSReference(*PigMission::getMissionObject(_mission)->
						getFixedCS()->getIdentity());
        return 0;
    }

    frame = lblDerivedImage->ReferenceCoordSystemName.Value;

    if (LBL_CHK_VALID(lblDerivedImage->ReferenceCoordSystemSolnId.Valid))
        solution_id = lblDerivedImage->ReferenceCoordSystemSolnId.Value;

    int size = 0;
    int index[PIG_MAX_CS_INDEX];
    for (int cnt = 0; cnt < LBL_COORD_SYS_INDEX; cnt++) {
        if (lblDerivedImage->ReferenceCoordSystemIndex[cnt].Valid) {
            index[cnt] = lblDerivedImage->ReferenceCoordSystemIndex[cnt].Value;
            size++;
        }
    }
    ref = new PigCSReference(PigMission::getMissionObject(_mission),
		 	frame, solution_id, index, size, getInstrumentId());

    return 1;
}

////////////////////////////////////////////////////////////////////

int PigFileModel::getSurfaceProjectionCS(PigCSReference *&ref)
{
    const char *frame = "UNKNOWN";		// just in case
    const char *solution_id = NULL;

    const LblSurfaceProjection_typ *lblSurfaceProjection
                                       = getLblSurfaceProjection();
    if (lblSurfaceProjection == NULL) {
	ref = new PigCSReference(*PigMission::getMissionObject(_mission)->
						getFixedCS()->getIdentity());
        return 0;
    }

    if (!lblSurfaceProjection->ReferenceCoordSystemName.Valid) {
	ref = new PigCSReference(*PigMission::getMissionObject(_mission)->
						getFixedCS()->getIdentity());
        return 0;
    }

    frame = lblSurfaceProjection->ReferenceCoordSystemName.Value;

    if (lblSurfaceProjection->ReferenceCoordSystemSolnId.Valid)
       solution_id = lblSurfaceProjection->ReferenceCoordSystemSolnId.Value;

    int size = 0;
    int index[PIG_MAX_CS_INDEX];
    for (int cnt = 0; cnt < LBL_COORD_SYS_INDEX; cnt++) {
        if (lblSurfaceProjection->ReferenceCoordSystemIndex[cnt].Valid) {
            index[cnt] =
                 lblSurfaceProjection->ReferenceCoordSystemIndex[cnt].Value;
            size++;
        }
    }
    ref = new PigCSReference(PigMission::getMissionObject(_mission),
		 	frame, solution_id, index, size, getInstrumentId());

    return 1;
}

////////////////////////////////////////////////////////////////////

int PigFileModel::getCameraModelCS(PigCSReference *&ref, int instance)
{
    const char *frame = "UNKNOWN";		// just in case
    const char *solution_id = NULL;

    const LblCameraModel_typ *lblCameraModel = getLblCameraModel(instance);
    if (lblCameraModel == NULL) {
	ref = new PigCSReference(*PigMission::getMissionObject(_mission)->
						getFixedCS()->getIdentity());
        return 0;
    }

    if (!lblCameraModel->ReferenceCoordSystemName.Valid) {
	ref = new PigCSReference(*PigMission::getMissionObject(_mission)->
						getFixedCS()->getIdentity());
        return 0;
    }

    frame = lblCameraModel->ReferenceCoordSystemName.Value;

    if (lblCameraModel->ReferenceCoordSystemSolnId.Valid)
        solution_id = lblCameraModel->ReferenceCoordSystemSolnId.Value;

    int size = 0;
    int index[PIG_MAX_CS_INDEX];
    for (int cnt = 0; cnt < LBL_COORD_SYS_INDEX; cnt++) {
        if (lblCameraModel->ReferenceCoordSystemIndex[cnt].Valid) {
            index[cnt] =
                 lblCameraModel->ReferenceCoordSystemIndex[cnt].Value;
            size++;
        }
    }
    ref = new PigCSReference(PigMission::getMissionObject(_mission),
		 	    frame, solution_id, index, size, getInstrumentId());

    return 1;

}

////////////////////////////////////////////////////////////////////

int PigFileModel::getSurfaceModelCS(PigCSReference *&ref)
{
    const char *frame = "UNKNOWN";		// just in case
    const char *solution_id = NULL;

    const LblSurfaceModel_typ *lblSurfaceModel
                                       = getLblSurfaceModel();
    if (lblSurfaceModel == NULL) {
	ref = new PigCSReference(*PigMission::getMissionObject(_mission)->
						getFixedCS()->getIdentity());
        return 0;
    }

    if (!lblSurfaceModel->ReferenceCoordSystemName.Valid) {
	ref = new PigCSReference(*PigMission::getMissionObject(_mission)->
						getFixedCS()->getIdentity());
        return 0;
    }

    frame = lblSurfaceModel->ReferenceCoordSystemName.Value;

    if (LBL_CHK_VALID(lblSurfaceModel->ReferenceCoordSystemSolnId.Valid))
        solution_id = lblSurfaceModel->ReferenceCoordSystemSolnId.Value;

    int size = 0;
    int index[PIG_MAX_CS_INDEX];
    for (int cnt = 0; cnt < LBL_COORD_SYS_INDEX; cnt++) {
        if (lblSurfaceModel->ReferenceCoordSystemIndex[cnt].Valid) {
            index[cnt] = lblSurfaceModel->ReferenceCoordSystemIndex[cnt].Value;
            size++;
        }
    }

    ref = new PigCSReference(PigMission::getMissionObject(_mission),
		 	frame, solution_id, index, size, getInstrumentId());

    return 1;
}

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

void PigFileModel::readAllCoordSystems(PigMission *m, int unit, int &count,
						PigCSDefinition *defs[])
{
    int max_count = count;
    count = 0;
    if (max_count == 0)
	return;				// pathological case...

    int status;
    int instances[MAX_CS_OBJ];
    int number_of_props = MAX_CS_OBJ;
    char prop_names[MAX_CS_OBJ][MAX_LBL_KEY_SIZE + 1];
    LblCoordinate_typ *lblCoordinate = new LblCoordinate_typ;
    char *found;

    // Get the instrument ID if available.  Why is this so hard...

    char *inst = NULL;
    LblIdentification_typ *lblIdent = new LblIdentification_typ;
    status = LblIdentification(unit, LBL_READ, lblIdent, 1);
    if (!RTN_FAILURE(status)) {
	if (lblIdent->InstrumentId[0].Valid) {
	    if (lblIdent->InstrumentId[0].Value != NULL) {
	        inst = strdup(lblIdent->InstrumentId[0].Value);
	    }
	}
    }
    delete lblIdent;

    //find all property names
    status = zlpinfo(unit,*prop_names, &number_of_props,
                     "INST_NUM", instances,
                     "ULEN", MAX_LBL_KEY_SIZE+1, NULL);

    for (int cnt = 0; cnt < number_of_props; cnt++) {
        found = strstr(prop_names[cnt], "COORDINATE_SYSTEM");
        if (found != NULL) {
            LblSetCoordinate(prop_names[cnt]);
            status = LblCoordinateApi(unit, LBL_READ, lblCoordinate, 1,
					(const char*)NULL);
            if (RTN_FAILURE(status)) {
                printStaticMsg((char *)LblErrorMessage(),
                     		"COORDINATE_SYSTEM", PigMsgError);
            }
            else {

#if 0	//!!!! NOSITE has been disabled
		// If NOSITE is given, zero out the indices
		char nosite[16];
		int c;
		PigModelBase::getStaticParam("NOSITE", nosite, &c,
						1, sizeof(nosite));
		if (c != 0) {
		    for (int i=0; i<PIG_MAX_CS_INDEX; i++) {
			lblCoordinate->CoordinateSystemIndex[i].Value = 0;
			lblCoordinate->ReferenceCoordSystemIndex[i].Value = 0;
		    }
		}
#endif

		PigCSDefinition *csd = new PigCSDefinition(m, lblCoordinate,
				inst);
		if (csd->isValid()) {
                    defs[count++] = new PigCSDefinition(m, lblCoordinate,
				inst);
		    if (count >= max_count)
		        return;			// out of space
		}
		else {
		    delete csd;			// invalid
		}
	    }
        }
    }
    if (inst) free(inst);
}

///////////////////////////////////////////////////////////////////////////
// Get Pointing Model Parameters for the file.  The array must be supplied.
// num_params in input should contain max size of the supplied array.
// On output it will contain the number of elements actually found(which
// could be bigger than the supplied max size, but only that many will
// actually be returned in the array).  PIG_MAX_PARAMS from PigAdjustable
// might be useful for array dimensioning.
///////////////////////////////////////////////////////////////////////////
void PigFileModel::getPointingModelParams(double params[], int &num_params)
{
    int max_params;

    if (num_params > PIG_MAX_PARAMS)
        max_params = PIG_MAX_PARAMS;
    else if (num_params < 0)
        max_params = 0;
    else
        max_params = num_params;

    num_params = 0;

    const LblDerivedImage_typ *lblDerivedImage = getLblDerivedImage();
    for (int cnt = 0; cnt < max_params; cnt++)
    {
        if (lblDerivedImage->PointingModelParams[cnt].Valid) {
            params[cnt] = lblDerivedImage->PointingModelParams[cnt].Value;
            num_params++;
        }
        else
            return;
     }
}


////////////////////////////////////////////////////////////////////
// Get the Camera model Transform vector and quaternions, and convert
// them to the desiredCS if supplied.
// If camera model vector and quaternions are invalid, the output (
// location and orientation are unchanged. To the user to init them with
// proper default values.
// This function is to be redefined in derived classes (mission specific)
////////////////////////////////////////////////////////////////////
void PigFileModel::getArticulationDevLocationOrient(PigPoint &location,
                                                   PigQuaternion &orientation,
                                                   PigCoordSystem *desired_cs,
                                                   PigMission *mission) {

   location.setX(0);
   location.setY(0);
   location.setZ(0);

   double quat[4] = {1, 0, 0, 0};
   orientation.setComponents(quat);

}



////////////////////////////////////////////////////////////////////
// Read the standard Cylindrical mosaic properties from the label.
// The base class implementation assumes MER-style SurfaceProjection
// labels; other missions can get the info differently.  Values are
// returned via the reference parameters.  Returns 0 on success, non-0
// on failure.  Other than surface_model and cs, items that are not
// found in the label are left untouched, so default values can be
// set before calling this routine.
////////////////////////////////////////////////////////////////////

int PigFileModel::getCylindricalProjectionParams(
                                PigSurfaceModel **surface_model,
                                double &scale_y,       // radians/pixel
                                double &scale_x,       // radians/pixel
                                PigPoint &proj_origin,
                                double &line_zero_el,
                                double &az_first_sample,
                                double &az_last_sample,
                                double &min_elev,
                                double &max_elev,
                                PigCoordSystem **cs) // projection coord system
{
    // Get the surface model

    *surface_model = PigSurfaceModel::createFromLabel(this);
    if (surface_model == NULL)
	return 1;		// msg already printed

    // Get the projection coord system.  (not necessarily the same as the
    // surface model coord system, which is in the SM object)

    PigCSReference *ref;
    getSurfaceProjectionCS(ref);
    PigMission *m = PigMission::getMissionObject(_mission);
    *cs = m->getCoordSystem(ref);

    const LblSurfaceProjection_typ *sp = getLblSurfaceProjection();
    if (sp == NULL) {
	printMsg("Unable to find mosaic projection params", "SurfaceProjection",
					PigMsgError);
	return 1;
    }

    // Now pick out each value and stuff it in the parameters

    if (sp->MapResolution.Valid) {	// label is pixels/degree
	scale_y = PigDeg2Rad(1.0 / sp->MapResolution.Value[0]);
	scale_x = PigDeg2Rad(1.0 / sp->MapResolution.Value[1]);
    }
    else
	printMsg("MapResolution not found in mosaic label", PigMsgWarning);

    if (sp->ProjectionOriginVector.Valid) {
	proj_origin = PigPoint(sp->ProjectionOriginVector.Value);
    }
    else
	printMsg("ProjectionOriginVector not found in mosaic label",
								PigMsgWarning);

    if (sp->ZeroElevationLine.Valid) {
	line_zero_el = sp->ZeroElevationLine.Value - 1;
    }
    else
	printMsg("ZeroElevationLine not found in mosaic label", PigMsgWarning);

    if (sp->StartAzimuth.Valid) {
	az_first_sample = PigDeg2Rad(sp->StartAzimuth.Value);
    }
    else
	printMsg("StartAzimuth not found in mosaic label", PigMsgWarning);

    if (sp->StopAzimuth.Valid) {
	az_last_sample = PigDeg2Rad(sp->StopAzimuth.Value);
    }
    else
	printMsg("StopAzimuth not found in mosaic label", PigMsgWarning);

    if (sp->MinimumElevation.Valid) {
	min_elev = PigDeg2Rad(sp->MinimumElevation.Value);
    }
    else
	printMsg("MinimumElevation not found in mosaic label", PigMsgWarning);

    if (sp->MaximumElevation.Valid) {
	max_elev = PigDeg2Rad(sp->MaximumElevation.Value);
    }
    else
	printMsg("MaximumElevation not found in mosaic label", PigMsgWarning);


    return 0;
}

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

int PigFileModel::getVertOrthoProjectionParams(
                                PigSurfaceModel **surface_model,
                                double &scale_line,	// m/pixel
                                double &scale_samp,	// m/pixel
				double &line_offset,
				double &samp_offset,
				double &x_min,
				double &x_max,
				double &y_min,
				double &y_max,
				int &proj_type,	// 1=vert, 2=ortho, ...
                                PigCoordSystem **cs) // projection coord system
{
    char msg[2048];

    // Get the projection coord system.  (not necessarily the same as the
    // surface model coord system, which is in the SM object)

    PigCSReference *ref;
    getSurfaceProjectionCS(ref);
    PigMission *m = PigMission::getMissionObject(_mission);
    *cs = m->getCoordSystem(ref);

    const LblSurfaceProjection_typ *sp = getLblSurfaceProjection();
    if (sp == NULL) {
	printMsg("Unable to find mosaic projection params", "SurfaceProjection",
					PigMsgError);
	return 1;
    }

    // Now pick out each value and stuff it in the parameters

    if (sp->MapScale[0].Valid) {		// label is m/pix
	scale_line = sp->MapScale[0].Value;
    } else {
	printMsg("MapScale[0] not found in mosaic label", PigMsgWarning);
    }

    if (sp->MapScale[1].Valid) {		// label is m/pix
	scale_samp = sp->MapScale[1].Value;
    } else {
	printMsg("MapScale[1] not found in mosaic label", PigMsgWarning);
    }

    if (sp->LineProjectionOffset.Valid) {
	line_offset = sp->LineProjectionOffset.Value;
    } else {
	printMsg("LineProjectionOffset not found in mosaic label", PigMsgWarning);
    }

    if (sp->SampleProjectionOffset.Valid) {
	samp_offset = sp->SampleProjectionOffset.Value;
    } else {
	printMsg("SampleProjectionOffset not found in mosaic label", PigMsgWarning);
    }

    if (sp->XAxisMinimum.Valid) {
	x_min = sp->XAxisMinimum.Value;
    } else {
	printMsg("XAxisMinimum not found in mosaic label", PigMsgWarning);
    }

    if (sp->XAxisMaximum.Valid) {
	x_max = sp->XAxisMaximum.Value;
    } else {
	printMsg("XAxisMaximum not found in mosaic label", PigMsgWarning);
    }

    if (sp->YAxisMinimum.Valid) {
	y_min = sp->YAxisMinimum.Value;
    } else {
	printMsg("YAxisMinimum not found in mosaic label", PigMsgWarning);
    }

    if (sp->YAxisMaximum.Valid) {
	y_max = sp->YAxisMaximum.Value;
    } else {
	printMsg("YAxisMaximum not found in mosaic label", PigMsgWarning);
    }

    if (sp->MapProjectionType.Valid) {
	if (strcasecmp(sp->MapProjectionType.Value, "VERTICAL") == 0)
	    proj_type = 1;
	else if (strcasecmp(sp->MapProjectionType.Value, "ORTHORECTIFIED") == 0)
	    proj_type = 2;
	else {
	    sprintf(msg, "Map Projection type '%s' is not vertical or orthorectified", sp->MapProjectionType.Value);
	    printMsg(msg, PigMsgWarning);
	}
    } else {
	printMsg("MapProjectionType not found in mosaic label", PigMsgWarning);
    }

    // Get the surface model, for vertical only

    if (proj_type == 1) {

        *surface_model = PigSurfaceModel::createFromLabel(this);
        if (surface_model == NULL)
	    return 1;		// msg already printed
    } else {
	*surface_model = NULL;
    }

    return 0;
}

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

int PigFileModel::getPolarProjectionParams(
				PigSurfaceModel **surface_model,
				double &scale_y,	// radians/pixel
				double &scale_x,	// radians/pixel
				PigPoint &proj_origin,
				double &up_azimuth,
				int &nadir_line,
			        int &nadir_samp,
				double &max_elev,
				PigCoordSystem **cs) // projection coord system
{

    // Get the surface model
    *surface_model = PigSurfaceModel::createFromLabel(this);
    if (surface_model == NULL)
    	return 1;		// msg already printed

    // Get the projection coord system. (Not necessarily the same as the surface
    // model coord system, which is in the surface model object)
    PigCSReference *ref;
    getSurfaceProjectionCS(ref);
    PigMission *m = PigMission::getMissionObject(_mission);
    *cs = m->getCoordSystem(ref);

    const LblSurfaceProjection_typ *sp = getLblSurfaceProjection();
    if (sp == NULL) {
    	printMsg("Unable to find polar mosaic projection params", "SurfaceProjection",
    		PigMsgError);
    	return 1;
    }

    // Now collect each value and enter it into the parameters

    if (sp->MapResolution.Valid) {
    	// convert the pixels/degree unit in the label to radians/pixel
    	scale_y = PigDeg2Rad(1.0 / sp->MapResolution.Value[0]);
    	scale_x = PigDeg2Rad(1.0 / sp->MapResolution.Value[1]);
    }
    else {
    	printMsg("MapResolution not found in polar mosaic label", PigMsgWarning);
    }

    if (sp->ProjectionOriginVector.Valid) {
    	proj_origin = PigPoint(sp->ProjectionOriginVector.Value);
    }
    else {
    	printMsg("ProjectionOriginVector not found in polar mosaic label",
    		PigMsgWarning);
    }

    if (sp->ReferenceAzimuth.Valid) {
    	up_azimuth = PigDeg2Rad(sp->ReferenceAzimuth.Value);
    }
    else {
    	printMsg("ReferenceAzimuth not found in polar mosaic label", PigMsgWarning);
    }

    if (sp->LineProjectionOffset.Valid) {
    	nadir_line = (int)sp->LineProjectionOffset.Value;
    }
    else {
    	printMsg("LineProjectionOffset not found in polar mosaic label",
    		PigMsgWarning);
    }
    
    if (sp->SampleProjectionOffset.Valid) {
    	nadir_samp = (int)sp->SampleProjectionOffset.Value;
    }
    else {
    	printMsg("SampleProjectionOffset not found in polar mosaic label",
    		PigMsgWarning);
    }

    if (sp->MaximumElevation.Valid) {
    	max_elev = PigDeg2Rad(sp->MaximumElevation.Value);
    }
    else {
    	printMsg("MaximumElevation not found in polar mosaic label", PigMsgWarning);
    }

    return 0;

}


////////////////////////////////////////////////////////////////////
// ILUT routines.  These protected methods implement the SBM (for
// SAMPLE_BIT_METHOD|MODE) model as used by MER and MSL.  Mission
// subclasses (not just MER/MSL) can call these from the virtual functions.
////////////////////////////////////////////////////////////////////

int PigFileModel::isIlutNeededSBM()
{
    if (_lblInstrumentState == NULL)
        getLblInstrumentState();
 
    if (!_lblInstrumentState->SampleBitMethod.Valid)
	return FALSE;			// No SAMPLE_BIT_METHOD

    char *sbmeth = _lblInstrumentState->SampleBitMethod.Value;
    if (strcasecmp(sbmeth, "HARDWARE") == 0)
	return TRUE;
    if (strcasecmp(sbmeth, "SOFTWARE") == 0)
	return TRUE;

    // Note, values of HARDWARE_INVERTED or SOFTWARE_INVERTED mean that
    // the inversion has already been done, thus we implicitly return
    // FALSE for those cases.

    return FALSE;
}

const char *PigFileModel::getIlutNameSBM()
{
    if (_lblInstrumentState == NULL)
        getLblInstrumentState();
 
    if (!_lblInstrumentState->SampleBitModeId.Valid)
	return NULL;			// No SAMPLE_BIT_MODE_ID

    return _lblInstrumentState->SampleBitModeId.Value;
}

////////////////////////////////////////////////////////////////////
// Determines whether or not the image has radiometric correction 
// related labels (RADIANCE_OFFSET, RADIANCE_SCALING_FACTOR, 
// RADIOMETRIC_CORRECTION_TYPE). The function returns TRUE if one of 
// the labels is there. 
////////////////////////////////////////////////////////////////////

int PigFileModel::hasRadiometricCorrectionLabel()
{
    int status = FALSE;
    const LblDerivedImage_typ *DerivedImage = getLblDerivedImage();

    if (DerivedImage->RadianceOffset.Valid || 
        DerivedImage->RadianceScaleFactor.Valid || 
        DerivedImage->RadiometricCorrectionType.Valid) {
        status = TRUE;
    }

    return status;
}

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
int PigFileModel::canConvertRADToFloat()
{
    int isInputInt = FALSE;
    int hasNeededLabels = FALSE;

    if (strcmp(getFormat(), "HALF") == 0 || strcmp(getFormat(), "BYTE") == 0 ||
        strcmp(getFormat(), "FULL") == 0) {
        isInputInt = TRUE;
    }

    if (hasRadiometricCorrectionLabel()) {
        hasNeededLabels = TRUE;
    }

    if (isInputInt && hasNeededLabels) {
        return TRUE;
    }

    return FALSE;
}

