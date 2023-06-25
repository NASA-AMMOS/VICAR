////////////////////////////////////////////////////////////////////////
// PigM20
//
// Contains all mission-specific code for dealing with the Mars 2020 mission.
//
////////////////////////////////////////////////////////////////////////
#ifndef PIGM20_H
#define PIGM20_H

#include "PigMission.h"
#include "PigCameraMapEntry.h"
#include "PigCameraMapper.h"

#include "string.h"

#define PIG_M20_NATURAL_FRAME "ROVER"
#define PIG_M20_NATURAL_PIXL_FRAME "PIXL_SENSOR"
#define PIG_M20_NATURAL_HELI_FRAME "HELI_G"
#define PIG_M20_NATURAL_LVS_FRAME "CINT"

// Constant coordinate frame offsets.  These really should be in the
// .cc file (not exposed) but the rimfax edrgen uses the rmech offsets...

// Keep in sync with M20_mast.point config file
#define M20_RMECH_TO_RNAV_OFFSET_X 0.09002
#define M20_RMECH_TO_RNAV_OFFSET_Y 0.0
#define M20_RMECH_TO_RNAV_OFFSET_Z -1.13338

// Keep in sync with M20_arm.point config file          //!!!! CONFIG FILE NAME
// Per email from Phil Bailey, 2020-05-01:
#define M20_PIXL_TOOL_TO_BASE_OFFSET_X -0.2227
#define M20_PIXL_TOOL_TO_BASE_OFFSET_Y -0.136
#define M20_PIXL_TOOL_TO_BASE_OFFSET_Z 0.131
// Per email from Phil Bailey, 2020-05-01:
// Tool X = Base Z, Tool Y = - Base X, Tool Z = - Base Y
#define M20_PIXL_BASE_TO_TOOL_ROLL PigDeg2Rad(-90)
#define M20_PIXL_BASE_TO_TOOL_YAW PigDeg2Rad(-90)

class PigM20 : public PigMission {

    using PigMission::getCoordSystem;

  protected:
    static PigRoverStateManager *_m20_rsm;

    static PigCoordSystem *_m20_cs_db[PIG_MAX_CS];
    static int _m20_cs_db_count;
    static PigCoordSystem *_m20_fixed_cs;

    char _mission_name[20];
    char _host_id[20];

    // Creates default M20-specific surface origin point.  Only meaningful
    // for PLANE surface model type which is a default type.
    // Default ground is easy since RNAV has been defined to have its origin
    // at the nominal ground... thus, (0,0,0).

    virtual PigPoint *getDefaultSurfaceGround(const char *instrument)
      {return new PigPoint(0.0, 0.0, 0.0); }


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
						      char *cmod_file);

    // Creates camera model given PigCameraMapEntry

    // This is protected because it does not do interpolation (and is
    // specific to this class).
    virtual PigCameraModel *createCameraModel(PigCameraMapEntry *entry,
					      const char *subtype,
					      const char *construction,
					      const char *calibration);

    // Provide access to the mission-specific singletons

    virtual PigCoordSystem **getCSList() { return _m20_cs_db; }
    virtual int *getCSCounter() { return &_m20_cs_db_count; }
    PigCoordSystem *getFixedCSInternal() { return _m20_fixed_cs; }
    void setFixedCSInternal(PigCoordSystem *cs) { _m20_fixed_cs = cs; }

    virtual int canonicalizeFrameName(const char *frame, const char *inst_id,
			char *&canon_frame, char *&short_frame,
			int &max_indices, int *mask_indices);

    // Return the "natural" frame for an instrument

    virtual const char *getNaturalFrame(const char *instrument)
	{  if (instrument != NULL && strcasecmp(instrument, "PIXL_MCC") == 0)
		return PIG_M20_NATURAL_PIXL_FRAME;
	   if (instrument != NULL && strcasecmp(instrument, "HELI_NAV") == 0)
		return PIG_M20_NATURAL_HELI_FRAME;
	   if (instrument != NULL && strcasecmp(instrument, "HELI_RTE") == 0)
		return PIG_M20_NATURAL_HELI_FRAME;
	   if (instrument != NULL && strcasecmp(instrument, "LCAM") == 0)
		return PIG_M20_NATURAL_LVS_FRAME;
	   return PIG_M20_NATURAL_FRAME;
	}

    // Return the RMC indices for this mission.

    virtual const char *getRmcIndexName(int i);

  public:

    PigM20(const char *mission_name, const char *host_id);

    virtual const char *getMissionName() { return _mission_name; }
    virtual const char *getHostID() { return _host_id; }
    virtual const char *getMissionLongName() {return "MARS 2020";}
    virtual int getMissionID() { return PIG_MISSION_ID_M20; }

    virtual PigCameraModel *createCameraModel(PigFileModel *file,
					      const char *special);

    // Note:  This version will not interpolate unless the subtype string
    // is set up with the interpolant values.  The File version of create
    // should always be used in preference to this.  Valid uses of this are
    // to create output models that approximate the inputs, NOT to model an
    // input file.
    virtual PigCameraModel *createCameraModel(const char *instrument,
					      const char *version, 
					      const char *subtype, 
					      const char *special,
					      const char *construction,
					      const char *calibration,
					      PigCoordSystem *cs)
        { return createCameraModelInterp(instrument, version, subtype,
                        special, construction, calibration, cs); }


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

    virtual PigColorModel *createColorModel(PigFileModel *file);

    virtual PigLabelModel *createLabelModel(int unit);

    virtual PigFileModel *createFileModel(const char  *filename, int unit);

    // Should only be called by PigFileModel!
    virtual int getBorders(PigFileModel *file,  int &sl, int &ss,
						int &el, int &es);

    virtual void addConstantCoordSystems(PigRoverStateManager *rsm,
					 PigCSDefinition *csdef);

    // This is usually a singleton per mission...
    virtual PigRoverStateManager *getRoverStateManager();

    // Use algorithm 2 as default for linearization on M2020, because we
    // have a camera that could very well do vertical stereo or other
    // non-traditional modes.  See PigMission.h
    virtual PigCmodWarpAlgorithm
                        getDefaultCmodWarpAlgorithm(const char *instument)
        { return CMOD_WARP_2; }


};

#endif
