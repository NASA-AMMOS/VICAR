////////////////////////////////////////////////////////////////////////
// PigMission
//
// Base class for all mission-specific interface functions.  These functions
// are used whenever mission-independent code needs to create a mission-
// specific object.  The static function getMissionObject() returns an
// instance of this class specific for the given mission.
//
// The interface functions are all gathered here to make it easier to
// eventually go to a dynamically-loaded shared library for all project
// support, and to minimize the mission-specific items that must be in
// multimission code.
//
// Note that for missions that have multiple spacecrafts(hosts) per mission,
// such as MER, the mission name is the string of the form: "mission:host_id".
// Also note, that usually mission objects contain no data, just virtual 
// functions to create other objects.  This is not always true, for missions
// with multiple spacecrafts as described above, we store mission_name and
// host_id.
// 
////////////////////////////////////////////////////////////////////////
#ifndef PIGMISSION_H
#define PIGMISSION_H

#include "xvmaininc.h"

#include "PigModelBase.h"
#include "PigVector.h"

#include <string.h>

class PigFileModel;
class PigLabelModel;
class PigCameraModel;
class PigPointingModel;
class PigSurfaceModel;
class RadiometryModel;
class PigCoordSystem;
class PigRoverStateManager;
class PigCSReference;
class PigCSDefinition;
class PigColorModel;
class PigCameraMapper;
class PigCameraMapEntry;

#include "PigCameraModel.h"	// for warp algorithm definition

// Mission ID's.  Subclasses should return the proper one from getMissionID().
// Names are normally used for comparison, however.

#define PIG_MISSION_ID_UNKNOWN		0
#define PIG_MISSION_ID_MPF		1
// #define PIG_MISSION_ID_M98		2
#define PIG_MISSION_ID_GENERIC_IMAGE	3
#define PIG_MISSION_ID_M01		4
#define PIG_MISSION_ID_FIDO             5
#define PIG_MISSION_ID_MER              6
#define PIG_MISSION_ID_PHX              7
#define PIG_MISSION_ID_MSL              8
#define PIG_MISSION_ID_NSYT             9
#define PIG_MISSION_ID_M20		10
#define PIG_MISSION_ID_COLDARM		11
#define PIG_MISSION_ID_PSYCHE		12

// Mesh support in the Pig library uses Intel Embree library for its ray     
// tracing needs. Embree is only available on 64-bit arch on Mac and Linux.  
// Activates Mesh support for 64-bit arch only.                              
// Important: Note that this variable goes hand in hand with EMBREE          
// library environment variable definition in vicset1 and in Pig code        
#undef USES_PIG_MESH 
#if X86_64_LINX_ARCH + MAC64_OSX_ARCH
#define USES_PIG_MESH 1 
#else
#define USES_PIG_MESH 0 
#endif


// Various maxes, for array allocation

// this probably should be dynamic when we have time... so don't use this
// value in any external code!!!
#define PIG_MAX_CS 10000

// Max number of indices (M20 PIXL has 12)
#define PIG_MAX_CS_INDEX 12
#define PIG_MAX_RMC_INDEX 12


class PigMission : public PigModelBase {

    friend PigCSReference;	// for canonicalizeFrameName()
    friend PigCoordSystem;	// for fixed-frame management stuff

  protected:

    // Cached mission objects.  NULL if not yet set.
    static PigMission *_mpf;
//    static PigMission *_m98;
    static PigMission *_generic_image;
    static PigMission *_m01;
    static PigMission *_fido;
    static PigMission *_mer[10];
    static PigMission *_phx[10];
    static PigMission *_msl[10];
    static PigMission *_nsyt[10];
    static PigMission *_m20[10];
    static PigMission *_coldarm[10];
    static PigMission *_psyche[10];
    static PigMission _defaultObject;	// A real object... not a pointer

    static int _mer_host_count;
    static int _phx_host_count;
    static int _msl_host_count;
    static int _nsyt_host_count;
    static int _m20_host_count;
    static int _coldarm_host_count;
    static int _psyche_host_count;

    // Create default surface normal vector.  For PLANE only, normal
    // indicates how the plane is tilted(the quaternion is already taken
    // into account).  The default is (0, 0, -1) since +Z is down for
    // most of the MARS missions.
    // For other surface types like SPHERE, "normal" is used as a placeholder
    // to store other info like radius thus default "normal" value is not
    // useful.
    virtual PigVector *getDefaultSurfaceNormal(const char *instrument)
      {return new PigVector(0.0, 0.0, -1.0); }
    
    // Creates default surface origin point.  For PLANE only, GROUND defines
    // a point through which the plane passes.  Together with the normal vector
    // that uniquely defines an infinite plane.  
    // For other surface types like SPHERE, "ground" is used to define
    // a sphere origin and the default value most likely is not useful.
    virtual PigPoint *getDefaultSurfaceGround(const char *instrument)
      {return new PigPoint(0.0, 0.0, 0.0); }
    
    // Designates default surface model type which is a tilted plane.  It
    // is defined by normal and ground parameters that are described above.
    virtual void getDefaultSurfaceType(const char *instrument, char *type)
      {strcpy(type, "PLANE\0");}

    // Creates default coordinate system for surface model.  
    virtual PigCoordSystem *getDefaultSurfaceCS(PigFileModel *file,
						const char *instrument);
    
    ////////////////////////////////////////////////////////////////////
    // Most of the routines below (until the end of the protected section)
    // were duplicated in MSL, NSYT, M20. Moved here to reduce code duplication.
    ////////////////////////////////////////////////////////////////////


    // This function obtains a camera model, possibly doing interpolation
    // if needed.  If this is used in a mission, several functions below
    // must also be implemented.

    virtual PigCameraModel *createCameraModelInterp(const char *instrument,
					      const char *version, 
					      const char *subtype, 
					      const char *special,
					      const char *construction,
					      const char *calibration,
					      PigCoordSystem *cs);

    //####
    // The following functions must be overridden by any mission wanting to do
    // interpolation (i.e. that uses CreateCameraModelInterp() )

    virtual PigCameraModel *createCameraModel(PigCameraMapEntry *entry,
                                          const char *subtype,
                                          const char *construction,
                                          const char *calibration)
	{ return NULL; }

    // Given strings constructs full-path filename
    // Note that calibration currently is not used to construct filename.
    // Returns cmod_file char string.
    virtual void constructCameraModelFileName(const char *calibration,
                                                      const char *serial,
                                                      const char *subtype,
                                                      const char *type,
                                                      int interp_focus,
						      int interp_zoom,
                                                      int interp_temp,
                                                      int interp_partner_temp,
                                                      int use_filter,
                                                      char *cmod_file)
	{ return; }
    // End of functions that need overriding for CreateCameraModelInterp()
    //####

    // Construct the string of calibration camera model filename
    // Here we allow any of the strings to be NULL.
    // Returns status: SUCESS = 1, FAIL = 0
    virtual int getCameraModelParmsFromFile(PigCameraModel *model,
                                                    PigCoordSystem *cs,
                                                    const char *calibration,
                                                    const char *serial,
                                                    const char *subtype,
                                                    const char *type,
                                                    int interp_focus,
						    int interp_zoom,
                                                    int interp_temp,
                                                    int interp_partner_temp);

    // Given filename string(cmod_file) read camera model from that
    // Returns status: SUCESS = 1, FAIL = 0
    virtual int readCameraModelFromFile(PigCameraModel *model,
                                                PigCoordSystem *cs,
                                                const char *subtype,
                                                int interp_focus,
						int interp_zoom,
                                                int interp_temp,
                                                int interp_partner_temp,
                                                char *cmod_file);


    // Do the interpolation for a camera model.  Reads in the interp file,
    // all the camera models it points to, and does the actual interpolation.
    virtual int doInterpolation(PigCameraModel *model,
                                        PigCoordSystem *cs,
                                        const char *subtype,
                                        char *interp_file,
                                        int interp_focus,
					int interp_zoom,
                                        int interp_temp,
                                        int interp_partner_temp);

    // Using the info in construction string, modify input camera model
    // for "Linearized", "Subframed", "Downsampled" images
    // Also takes care of geometric temperature compensation.
    virtual PigCameraModel* adjustCameraModel(PigCameraModel *model,
                                                      PigCoordSystem *cs,
                                                      PigCameraMapper *map,
                                                      PigCameraMapEntry *entry,
                                                      const char *calibration,
                                                      const char *construction,
                                                      const char *subtype,
                                                      int is_stereo);

    // These are used to manage the list of CS objects for the mission.
    // They MUST be overridden by each subclass to return the mission-specific
    // CS list and counter, and the fixed-frame singleton for each mission.

    virtual PigCoordSystem **getCSList() { return NULL; }
    virtual int *getCSCounter() { return NULL; }
    virtual PigCoordSystem *getFixedCSInternal() { return NULL; }
    virtual void setFixedCSInternal(PigCoordSystem *cs) { }


    // Canonicalize a frame name and get the # of indices it should have.
    // This must be overriden by each subclass.
    // The returned strings are pointers to internal strings and MUST NOT
    // be modified!
    // Strings that must be supported for multimission compatibility:
    //     FIXED - fixed frame
    //     INSTRUMENT - the "natural" frame for the instrument
    //     SITE
    //     ROVER
    //     LOCAL_LEVEL
    // Missions that don't support site/rover/LL should just get the CS
    // that matches that intent as closely as possible
    // inst_id is used only if the frame is INSTRUMENT and can be NULL
    // (if it's NULL and frame is INSTRUMENT, just get the "most common")
    // Return 1 for success, 0 for frame not recognized (in which case it's
    // as if FIXED was given)
    // mask_indices (output) will contain 1 for relevant indices and -1 for
    // those that should be wildcarded.  Valid pointer must be provided, or
    // NULL for dont-care.  Mask will be filled up to max_indices.


    virtual int canonicalizeFrameName(const char *frame, const char *inst_id,
			char *&canon_frame, char *&short_frame,
			int &max_indices, int *mask_indices)
	{ canon_frame = "UNKNOWN";
	  short_frame = "UNKNOWN";
	  max_indices = 0;
          return 0;
	}

    // Return the "natural" frame for this mission.  Should be overriden.

    virtual const char *getNaturalFrame(const char *instrument)
	{ return "FIXED"; }

    // Return the RMC indices for this mission.  Must be overridden by subclasses.

    virtual const char *getRmcIndexName(int i) { return "NULL"; }

  public:

    // Mission is as returned by PigGetMission()
    static PigMission *getMissionObject(const char *filename, int *unit);
    static PigMission *getMissionObject(PigFileModel *file);
    static PigMission *getMissionObject(const char *mission);

    // Dummy constructor
    PigMission() { }

    // The default is sufficient, if no mission-specific subclass is needed
    virtual PigLabelModel *createLabelModel(int unit);

    virtual const char *getMissionName() { return "Unknown"; }
    virtual const char *getMissionLongName() { return "Unknown"; }
    virtual int getMissionID() { return PIG_MISSION_ID_UNKNOWN; }

    // Host ID is the ID of the hardware if there's more than one for
    // the mission (e.g. rovers for MER).  Otherwise it's the mission name.

    virtual const char *getHostID() { return getMissionName(); }

    // These are the functions... they all return NULL unless overridden.

    virtual PigCameraModel *createCameraModel(PigFileModel *file,
		const char *special)
	{ return NULL; }
    virtual PigCameraModel *createCameraModel(const char *instrument,
					      const char *version, 
					      const char *subtype, 
					      const char *special,
					      const char *construction,
					      const char *calibration,
					      PigCoordSystem *cs)
	{ return NULL; }

    virtual PigPointingModel *createPointingModel(PigCameraModel *cm,
						  PigFileModel *file,
                                                  const char *type,
                                                  bool allow_type_override);

    virtual PigPointingModel *createPointingModel(PigCameraModel *cm,
						  const char *instrument,
                                                  const char *type,
                                                  bool allow_type_override)
	{ return NULL; }

    virtual PigSurfaceModel *createSurfaceModel(PigFileModel *file);
    virtual PigSurfaceModel *createSurfaceModel(const char *instrument,
						const char *target);
    virtual PigSurfaceModel *createSurfaceModel(const char *instrument,
						const char *type, 
						const double params[], 
						const int count,
						PigCoordSystem *cs);
    virtual PigSurfaceModel *createSurfaceModel(const char *instrument,
						const char *target, 
						PigCoordSystem *cs);
    virtual PigSurfaceModel *createSurfaceModel(const char *instrument,
    					        const char *target,
                                                const char *type,
                                                PigVector *normal,
                                                PigPoint *point, 
                                                const char *meshFile, 
    				  	        PigCoordSystem *cs);

    virtual RadiometryModel *createRadiometryModel(PigFileModel *file)
	{ return NULL; }


    // The default is sufficient, if no mission-specific subclass is needed
    virtual PigFileModel *createFileModel(const char *filename, int unit);

    // Should only be called by PigFileModel!
    virtual int getBorders(PigFileModel *file,  int &sl, int &ss,
						int &el, int &es)
	{ return FALSE; }

    ////////////////////////////////////////////////////////////////////
    // PigCoordSystem-related methods
    ////////////////////////////////////////////////////////////////////

    // Returns or creates if needed the coordinate system matching the given 
    // reference.  First looks for a CS that already exists, then looks in the 
    // RoverStateManager for one that's eligible for creation.
    // 
    // This should be the only place in the system a CS obj is actually created.
    virtual PigCoordSystem *getCoordSystem(PigCSReference *ident);

    // Create a CS using RMC info from the given file. NULL returns the
    // "natural" frame for the given file (typically a rover frame or
    // equivalent) and is equivalent to specifying "instrument".

    virtual PigCoordSystem *getCoordSystem(PigFileModel *fm, const char *frame);

    // Returns whatever match it finds for the given CS name in the
    // list of currently-instantiated CS objects.  If not found, it searches
    // the PigRoverStateManager to find any csdef with the given frame
    // name.
    // THIS ONE SHOULD BE AVOIDED.  It tacitly assumes that there is only
    // one RMC value in the CS databases, which is often not the case.  If
    // there is more than one, it is arbitrary which is returned.  Also,
    // NULL and INSTRUMENT are not allowed for the frame name on this one.

    virtual PigCoordSystem *getCoordSystem(const char *frame);

    // Returns a CS using the same index set as the given CS but a
    // different frame name

    virtual PigCoordSystem *getCoordSystem(PigCoordSystem *templ,
				const char *frame);

    // Add constant coordinate systems related to this one to the RSM
    // database.  Missions that need this should override.  Default
    // implementation loads LOCAL_LEVEL when given ROVER or ROVER_NAV.

    virtual void addConstantCoordSystems(PigRoverStateManager *rsm,
					 PigCSDefinition *csdef);

    //////
    // Get the Fixed frame
    virtual PigCoordSystem *getFixedCS();

    // Sets the fixed CS.  Uses FIXED_SITE parameter if given, or the
    // lowest-numbered site in both the internal CS database and the
    // solution list if not (where "site" is defined as anything with
    // only one RMC index).  If all else fails, look for anything with
    // 0 indices, and then anything at all.
    //
    // Looks for the following in the PDF (although will work fine without:
    // PARM FIXED_SITE TYPE=INTEGER COUNT=0:1 DEFAULT=--

    virtual void setFixedCS();

    // Sets the given CSRef to be the Fixed CS.  If the CS pointed at by
    // this CSRef does not exist, it is created with an identity transform
    // and no reference frame.  Since we ARE the Fixed CS, no ref is needed.
    // The use case here is the telemprocs, which create the Rover CS pointing
    // to Site and then expect Site to exist (as the fixed) too.

    virtual void setFixedCS(PigCSReference *ident);

    // Sets the fixed CS to be the given CS
    virtual void setFixedCS(PigCoordSystem *cs);

    // Invalidate all CS caches
    virtual void invalidateAllCSCaches();

    // create and return a color model specified by the file model.
    virtual PigColorModel *createColorModel(PigFileModel *file)
        { return NULL; }

    // This is usually a singleton per mission...
    virtual PigRoverStateManager *getRoverStateManager()
	{ return NULL; }

    // Return the default algorithm to use for epipolar alignment for this
    // mission (the default could vary by instrument).  This can be overridden
    // by CMOD_WARP= in POINT_METHOD.  Currently the values are:
    // 1 = old model, used by MER and MSL since 2012
    // 2 = new model (2017), which works better for non-traditional stereo
    // 3 or PSPH = aligns to (and returns) PSPH model
    // Since 1 is the default, we do not need subclasses for MER or MSL.
    // Note that it's possible instrument may be NULL.
    virtual PigCmodWarpAlgorithm
			getDefaultCmodWarpAlgorithm(const char *instrument)
	{ return CMOD_WARP_1; }


};

#endif

