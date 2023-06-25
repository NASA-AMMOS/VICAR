////////////////////////////////////////////////////////////////////////
// PigGenericImage
//
// Contains all "mission-specific" code for dealing with generic images
// (those that don't have a recognized mission).  See PigMission.
////////////////////////////////////////////////////////////////////////
#ifndef PIGGENERICIMAGE_H
#define PIGGENERICIMAGE_H

#include "PigMission.h"

#define PIG_GENERIC_NATURAL_FRAME "CAMERA"

class PigGenericImage : public PigMission {

  public:

    static PigRoverStateManager *_generic_rsm;

    static PigCoordSystem *_generic_cs_db[PIG_MAX_CS];
    static int _generic_cs_db_count;
    static PigCoordSystem *_generic_fixed_cs;

    PigGenericImage() { }

    virtual const char *getMissionName() { return "Generic"; }
    virtual const char *getMissionLongName() { return "Generic (no mission) Image"; }
    virtual int getMissionID() { return PIG_MISSION_ID_GENERIC_IMAGE; }

    virtual PigCameraModel *createCameraModel(PigFileModel *file,
					      const char *special);
    virtual PigCameraModel *createCameraModel(const char *instrument,
					      const char *version, 
					      const char *subtype, 
					      const char *special,
					      const char *construction,
					      const char *calibration,
					      PigCoordSystem *cs);

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

    virtual PigFileModel *createFileModel(const char  *filename, int unit);

    // Should only be called by PigFileModel!
    virtual int getBorders(PigFileModel *file,  int &sl, int &ss,
						int &el, int &es);

    // Provide access to the mission-specific singletons

    virtual PigCoordSystem **getCSList() { return _generic_cs_db; }
    virtual int *getCSCounter() { return &_generic_cs_db_count; }
    PigCoordSystem *getFixedCSInternal() { return _generic_fixed_cs; }
    void setFixedCSInternal(PigCoordSystem *cs) { _generic_fixed_cs = cs; }

    virtual int canonicalizeFrameName(const char *frame, const char *inst_id,
                        char *&canon_frame, char *&short_frame,
                        int &max_indices, int *mask_indices);

    // Return the "natural" frame for an instrument
    virtual const char *getNaturalFrame(const char *instrument)
        { return PIG_GENERIC_NATURAL_FRAME; }

    // Return the RMC indices for this mission.

    virtual const char *getRmcIndexName(int i);

    // This is usually a singleton per mission...
    virtual PigRoverStateManager *getRoverStateManager();

};

#endif

