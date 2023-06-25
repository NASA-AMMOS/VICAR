////////////////////////////////////////////////////////////////////////
// PigM01
//
// Contains all mission-specific code for dealing with the Mars '01 testbed.
// This is not a real mission (any more); it is mainly a testbed for the
// Pancam camera.
////////////////////////////////////////////////////////////////////////
#ifndef PIGM01_H
#define PIGM01_H

#include "PigMission.h"

#define PIG_M01_NATURAL_FRAME "CAMERA"

class PigM01 : public PigMission {

 protected:

    static PigRoverStateManager *_m01_rsm;

    static PigCoordSystem *_m01_cs_db[PIG_MAX_CS];
    static int _m01_cs_db_count;
    static PigCoordSystem *_m01_fixed_cs;

    // Creates default M01-specific surface origin point.  Only meaningful
    // for PLANE surface model type which is a default type.  
    // Default ground value has been taken from M01 engineering drawings and confirmed
    // using correlation of an image looking at the ground of the lab.
    virtual PigPoint *getDefaultSurfaceGround(const char *instrument)
      {return new PigPoint(0.0, 0.0, -0.784); }

  public:
    PigM01() { }

    virtual const char *getMissionName() { return "M01"; }
    virtual const char *getMissionLongName() { return "Mars '01 testbed"; }
    virtual int getMissionID() { return PIG_MISSION_ID_M01; }

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
						  bool allow_type_override);
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

    virtual PigCoordSystem **getCSList() { return _m01_cs_db; }
    virtual int *getCSCounter() { return &_m01_cs_db_count; }
    PigCoordSystem *getFixedCSInternal() { return _m01_fixed_cs; }
    void setFixedCSInternal(PigCoordSystem *cs) { _m01_fixed_cs = cs; }

    virtual int canonicalizeFrameName(const char *frame, const char *inst_id,
                        char *&canon_frame, char *&short_frame,
                        int &max_indices, int *mask_indices);

    // Return the "natural" frame for an instrument
    virtual const char *getNaturalFrame(const char *instrument)
        { return PIG_M01_NATURAL_FRAME; }

    // Return the RMC indices for this mission.

    virtual const char *getRmcIndexName(int i);

    // This is usually a singleton per mission...
    virtual PigRoverStateManager *getRoverStateManager();

};

#endif

