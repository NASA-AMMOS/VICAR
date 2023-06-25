////////////////////////////////////////////////////////////////////////
// PigMPF
//
// Contains all mission-specific code for creating mission-specific subclasses
// of various Pig objects.  See PigMission.
////////////////////////////////////////////////////////////////////////
#ifndef PIGMPF_H
#define PIGMPF_H

#include "PigMission.h"
#include "PigQuaternion.h"

#define PIG_MPF_IMP_NATURAL_FRAME "Lander"
#define PIG_MPF_ROVER_NATURAL_FRAME "Rover"

class PigMPF : public PigMission {

  private:
    static PigRoverStateManager *_mpf_rsm;

    static PigCoordSystem *_mpf_cs_db[PIG_MAX_CS];
    static int _mpf_cs_db_count;
    static PigCoordSystem *_mpf_fixed_cs;

  public:
    PigMPF() { }

    virtual const char *getMissionName() { return "MPF"; }
    virtual const char *getMissionLongName() { return "MARS PATHFINDER"; }
    virtual int getMissionID() { return PIG_MISSION_ID_MPF; }

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

    virtual PigFileModel *createFileModel(const char *filename, int unit);

    // Should only be called by PigFileModel!
    virtual int getBorders(PigFileModel *file,  int &sl, int &ss,
						int &el, int &es);

    // Provide access to the mission-specific singletons

    virtual PigCoordSystem **getCSList() { return _mpf_cs_db; }
    virtual int *getCSCounter() { return &_mpf_cs_db_count; }
    PigCoordSystem *getFixedCSInternal() { return _mpf_fixed_cs; }
    void setFixedCSInternal(PigCoordSystem *cs) { _mpf_fixed_cs = cs; }

    virtual int canonicalizeFrameName(const char *frame, const char *inst_id,
                        char *&canon_frame, char *&short_frame,
                        int &max_indices, int *mask_indices);

    // Return the "natural" frame for an instrument
    virtual const char *getNaturalFrame(const char *instrument)
	{ if (instrument != NULL && strncasecmp(instrument, "ROVER", 5) == 0)
	      return PIG_MPF_ROVER_NATURAL_FRAME;
	  return PIG_MPF_IMP_NATURAL_FRAME;
	}

    // Return the RMC indices for this mission.

    virtual const char *getRmcIndexName(int i);

    // This is usually a singleton per mission...
    virtual PigRoverStateManager *getRoverStateManager();

    static PigQuaternion eulerAnglesToQuaternion(double heading, 
					  double pitch, 
					  double roll   );

};

#endif

