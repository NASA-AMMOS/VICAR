////////////////////////////////////////////////////////////////////////
// PigMER
//
// Contains all mission-specific code for dealing with the Mars MER testbed.
//
////////////////////////////////////////////////////////////////////////
#ifndef PIGMER_H
#define PIGMER_H

#include "PigMission.h"
#include "PigCameraMapEntry.h"
#include "PigCameraMapper.h"

#include "string.h"

#define NL_MER 1024
#define NS_MER 1024

#define PIG_MER_NATURAL_FRAME "ROVER"

class PigMER : public PigMission {

  protected:
    static PigRoverStateManager *_mer_rsm;

    static PigCoordSystem *_mer_cs_db[PIG_MAX_CS];
    static int _mer_cs_db_count;
    static PigCoordSystem *_mer_fixed_cs;

    char _mission_name[10];
    char _host_id[10];

    // Creates default MER-specific surface origin point.  Only meaningful
    // for PLANE surface model type which is a default type.  
    // Default ground value has been taken from MER engineering drawings and confirmed
    // using correlation of an image looking at the ground of the lab.
    // Confirmed again during nominal OPS
    virtual PigPoint *getDefaultSurfaceGround(const char *instrument)
      {return new PigPoint(0.0, 0.0, 0.294); }

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
    virtual PigCameraModel* adjustCameraModel(PigCameraModel *model,
						      PigCoordSystem *cs,
						      PigCameraMapper *map,
						      PigCameraMapEntry *entry,
						      const char *calibration,
						      const char *construction,
						      const char *filter);

    // Provide access to the mission-specific singletons

    virtual PigCoordSystem **getCSList() { return _mer_cs_db; }
    virtual int *getCSCounter() { return &_mer_cs_db_count; }
    PigCoordSystem *getFixedCSInternal() { return _mer_fixed_cs; }
    void setFixedCSInternal(PigCoordSystem *cs) { _mer_fixed_cs = cs; }

    virtual int canonicalizeFrameName(const char *frame, const char *inst_id,
                        char *&canon_frame, char *&short_frame,
                        int &max_indices, int *mask_indices);


    // Return the "natural" frame for an instrument

    virtual const char *getNaturalFrame(const char *instrument)
        { return PIG_MER_NATURAL_FRAME; }

    // Return the RMC indices for this mission.

    virtual const char *getRmcIndexName(int i);


  public:

    PigMER(const char *mission_name, const char *host_id);
   
    virtual const char *getMissionName() { return _mission_name; }
    virtual const char *getHostID() { return _host_id; }
    virtual const char *getMissionLongName() {return "MARS EXPLORATION ROVER"; }
    virtual int getMissionID() { return PIG_MISSION_ID_MER; }

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
