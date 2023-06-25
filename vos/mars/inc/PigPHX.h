////////////////////////////////////////////////////////////////////////
// PigPHX
//
// Contains all mission-specific code for dealing with the Mars PHX spacecraft.
//
////////////////////////////////////////////////////////////////////////
#ifndef PIGPHX_H
#define PIGPHX_H

#include "PigMission.h"
#include "PigCameraMapEntry.h"
#include "PigCameraMapper.h"

#include "string.h"

#define NL_PHX 1024
#define NS_PHX 1024

#define PIG_PHX_NATURAL_FRAME "PAYLOAD"
#define PIG_PHX_NATURAL_FRAME_OM "OM"

class PigPHX : public PigMission {

  protected:
    static PigRoverStateManager *_phx_rsm;

    static PigCoordSystem *_phx_cs_db[PIG_MAX_CS];
    static int _phx_cs_db_count;
    static PigCoordSystem *_phx_fixed_cs;

    char _mission_name[10];
    char _host_id[10];

    // Provide access to the mission-specific singletons

    virtual PigCoordSystem **getCSList() { return _phx_cs_db; }
    virtual int *getCSCounter() { return &_phx_cs_db_count; }
    PigCoordSystem *getFixedCSInternal() { return _phx_fixed_cs; }
    void setFixedCSInternal(PigCoordSystem *cs) { _phx_fixed_cs = cs; }

    virtual int canonicalizeFrameName(const char *frame, const char *inst_id,
			char *&canon_frame, char *&short_frame,
			int &max_indices, int *mask_indices);

    // Return the "natural" frame for an instrument

    virtual const char *getNaturalFrame(const char *instrument)
	{ if (instrument != NULL && strcasecmp(instrument, "OM") == 0)
		return PIG_PHX_NATURAL_FRAME_OM;
	  return PIG_PHX_NATURAL_FRAME;
	}

    // Creates default PHX-specific surface origin point.  Only meaningful
    // for PLANE surface model type which is a default type.
    // Default ground value has been taken from PHX engineering drawings
    // as the distance from the deck to the ground.  Because of the springs
    // on the legs, this value is assumed to be an average between the
    // maximum (1.0704) and minimum (0.8289) values.
    //
    // This value has NOT YET BEEN CONFIRMED by actual imagery (rgd 12/11/06).

    virtual PigPoint *getDefaultSurfaceGround(const char *instrument)
      {return new PigPoint(0.0, 0.0, 0.94965); }

    // Construct the string of calibration camera model filename
    // Here we allow any of the strings to be NULL.
    // Returns status: SUCESS = 1, FAIL = 0
    virtual int getCameraModelParmsFromFile(PigCameraModel *model,
						    PigCoordSystem *cs,
						    const char *calibration,
						    const char *serial,
						    const char *filter,
						    const char *type);

    // Given strings constructs full-path filename
    // Note that calibration currently is not used to construct filename.
    // Returns cmod_file char string.
    virtual void constructCameraModelFileName(const char *calibration,
						      const char *serial,
						      const char *filter,
						      const char *type,
						      char *cmod_file);

    // Given filename string(cmod_file) read camera model from that
    // Returns status: SUCESS = 1, FAIL = 0
    virtual int readCameraModelFromFile(PigCameraModel *model,
						PigCoordSystem *cs,
						char *cmod_file);

    // Using the info in construction string, modify input camera model
    // for "Linearized", "Subframed", "Downsampled" images 
    // Also takes care of geometric temperature compensation.
    virtual PigCameraModel* adjustCameraModel(PigCameraModel *model,
						      PigCoordSystem *cs,
						      PigCameraMapper *map,
						      PigCameraMapEntry *entry,
						      const char *calibration,
						      const char *construction,
						      const char *filter);

    virtual const char *getRmcIndexName(int i);

  public:

    PigPHX(const char *mission_name, const char *host_id);

    virtual const char *getMissionName() { return _mission_name; }
    virtual const char *getHostID() { return _host_id; }
    virtual const char *getMissionLongName() {return "PHOENIX LANDER"; }
    virtual int getMissionID() { return PIG_MISSION_ID_PHX; }

    virtual PigCameraModel *createCameraModel(PigFileModel *file,
					      const char *special);

    virtual PigCameraModel *createCameraModel(const char *instrument,
					      const char *version, 
					      const char *subtype, 
					      const char *special,
					      const char *construction,
					      const char *calibration,
					      PigCoordSystem *cs);

    // Creates camera model give PigCameraMapEntry
    virtual PigCameraModel *createCameraModel(PigCameraMapEntry *entry,
					      const char *subtype,
					      const char *construction,
					      const char *calibration);

    virtual PigPointingModel *createPointingModel(PigCameraModel *cm,
						  PigFileModel *file,
                                                  const char *type, 
						  bool allow_type_override)
       { return PigMission::createPointingModel(cm, file,
                                                 type, allow_type_override); }
    virtual PigPointingModel *createPointingModel(PigCameraModel *cm,
						  const char *instrument,
                                                  const char *type, 
						  bool allow_type_override);
 
    virtual RadiometryModel *createRadiometryModel(PigFileModel *file);

    virtual PigLabelModel *createLabelModel(int unit);

    virtual PigFileModel *createFileModel(const char  *filename, int unit);

    // Should only be called by PigFileModel!
    virtual int getBorders(PigFileModel *file,  int &sl, int &ss,
						int &el, int &es);

    // This is usually a singleton per mission...
    virtual PigRoverStateManager *getRoverStateManager();

};

#endif
