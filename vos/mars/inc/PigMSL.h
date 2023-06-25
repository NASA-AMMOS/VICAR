////////////////////////////////////////////////////////////////////////
// PigMSL
//
// Contains all mission-specific code for dealing with the Mars MSL mission.
//
////////////////////////////////////////////////////////////////////////
#ifndef PIGMSL_H
#define PIGMSL_H

#include "PigMission.h"
#include "PigCameraMapEntry.h"
#include "PigCameraMapper.h"

#include "string.h"

#define PIG_MSL_NATURAL_FRAME "ROVER"

class PigMSL : public PigMission {

    using PigMission::getCoordSystem;

  protected:
    static PigRoverStateManager *_msl_rsm;

    static PigCoordSystem *_msl_cs_db[PIG_MAX_CS];
    static int _msl_cs_db_count;
    static PigCoordSystem *_msl_fixed_cs;

    char _mission_name[10];
    char _host_id[10];
    char _orig_host_id[10];

    // Creates default MSL-specific surface origin point.  Only meaningful
    // for PLANE surface model type which is a default type.
    // Default ground is easy since RNAV has been redefined to have its origin
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

    virtual PigCoordSystem **getCSList() { return _msl_cs_db; }
    virtual int *getCSCounter() { return &_msl_cs_db_count; }
    PigCoordSystem *getFixedCSInternal() { return _msl_fixed_cs; }
    void setFixedCSInternal(PigCoordSystem *cs) { _msl_fixed_cs = cs; }

    virtual int canonicalizeFrameName(const char *frame, const char *inst_id,
                        char *&canon_frame, char *&short_frame,
                        int &max_indices, int *mask_indices);

    // Return the "natural" frame for an instrument

    virtual const char *getNaturalFrame(const char *instrument)
        { return PIG_MSL_NATURAL_FRAME; }

    // Return the RMC indices for this mission.

    virtual const char *getRmcIndexName(int i);

  public:

    PigMSL(const char *mission_name, const char *host_id);

    virtual const char *getMissionName() { return _mission_name; }
    virtual const char *getHostID() { return _host_id; }
    virtual const char *getMissionLongName() {return "MARS SCIENCE LABORATORY";}
    virtual int getMissionID() { return PIG_MISSION_ID_MSL; }

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

    virtual PigLabelModel *createLabelModel(int unit);

    virtual PigFileModel *createFileModel(const char  *filename, int unit);

    // Should only be called by PigFileModel!
    virtual int getBorders(PigFileModel *file,  int &sl, int &ss,
						int &el, int &es);

    virtual void addConstantCoordSystems(PigRoverStateManager *rsm,
                                         PigCSDefinition *csdef);

    // This is usually a singleton per mission...
    virtual PigRoverStateManager *getRoverStateManager();

    // This need not be virtual because it is specific to this class.
    // Returns the unadorned host ID (without the MSL prefix) for use
    // **ONLY** by PigMission::getMissionObject(mission).  See the
    // PigMSL constructor for details.
    const char *getOrigHostID() { return _orig_host_id; }

};

#endif
