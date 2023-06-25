////////////////////////////////////////////////////////////////////////
// PigPsyche
//
// Contains all mission-specific code for dealing with the Psyche mission.
//
////////////////////////////////////////////////////////////////////////
#ifndef PIGPSYCHE_H
#define PIGPSYCHE_H

#include "PigMission.h"
#include "PigCameraMapEntry.h"
#include "PigCameraMapper.h"

#include "string.h"

#define PIG_PSYCHE_NATURAL_FRAME "ROVER"

class PigPsyche : public PigMission {

    using PigMission::getCoordSystem;

  protected:
    static PigRoverStateManager *_psyche_rsm;

    static PigCoordSystem *_psyche_cs_db[PIG_MAX_CS];
    static int _psyche_cs_db_count;
    static PigCoordSystem *_psyche_fixed_cs;

    char _mission_name[20];
    char _host_id[20];

    // Creates default Psyche-specific surface origin point.  Only meaningful
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

    virtual PigCoordSystem **getCSList() { return _psyche_cs_db; }
    virtual int *getCSCounter() { return &_psyche_cs_db_count; }
    PigCoordSystem *getFixedCSInternal() { return _psyche_fixed_cs; }
    void setFixedCSInternal(PigCoordSystem *cs) { _psyche_fixed_cs = cs; }

    virtual int canonicalizeFrameName(const char *frame, const char *inst_id,
			char *&canon_frame, char *&short_frame,
			int &max_indices, int *mask_indices);

    // Return the "natural" frame for an instrument

    virtual const char *getNaturalFrame(const char *instrument)
	{
	   return PIG_PSYCHE_NATURAL_FRAME;
	}

    // Return the RMC indices for this mission.

    virtual const char *getRmcIndexName(int i);

  public:

    PigPsyche(const char *mission_name, const char *host_id);

    virtual const char *getMissionName() { return _mission_name; }
    virtual const char *getHostID() { return _host_id; }
    virtual const char *getMissionLongName() {return "Psyche";}
    virtual int getMissionID() { return PIG_MISSION_ID_PSYCHE; }

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

    // This is usually a singleton per mission...
    virtual PigRoverStateManager *getRoverStateManager();

    // Use algorithm 2 as default for linearization on Psyche
    virtual PigCmodWarpAlgorithm
                        getDefaultCmodWarpAlgorithm(const char *instument)
        { return CMOD_WARP_2; }


};

#endif
