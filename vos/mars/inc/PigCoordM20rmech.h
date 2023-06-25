////////////////////////////////////////////////////////////////////////
// PigCoordM20rmech
//
// Defines the M20 "Rover Mechanical" coordinate system.  This is identical
// to Rover Nav except for a translation along the X axis.
//
////////////////////////////////////////////////////////////////////////
#ifndef PIGCOORDM20RMECH_H
#define PIGCOORDM20RMECH_H

#include "PigCoordM20rover.h"

// Keep in sync with M20_mast.point config file
#define M20_RMECH_TO_RNAV_OFFSET_X 0.09002
#define M20_RMECH_TO_RNAV_OFFSET_Y 0.0
#define M20_RMECH_TO_RNAV_OFFSET_Z -1.13338

class PigCoordM20rmech : public PigCoordM20rover {

  protected:

    friend class PigM20;		// only one with access to ctor
    PigPoint _RMechToRNav;		// constant offset between the frames
    PigCoordM20rmech(PigSite *site)
		: PigCoordM20rover(site, PIG_FRAME_NAME_M20_RMECH,
					 PIG_FRAME_ID_M20_RMECH)
	{ _RMechToRNav = PigPoint(M20_RMECH_TO_RNAV_OFFSET_X,
				  M20_RMECH_TO_RNAV_OFFSET_Y,
				  M20_RMECH_TO_RNAV_OFFSET_Z); }

  public:

    // Short names are abbreviated names for headers, etc.  Should be
    // overridden by subclasses.
    virtual const char *getFrameShortName() { return "RMECH"; }

    // Convert to/from the Fixed frame for this mission.  This involves
    // offset from the base Rover (nav) frame only - rotation is the same.
 
    // Convert to rnav first, then call superclass
    virtual PigPoint convertPointToFixed(PigPoint point)
	{ return PigCoordM20rover::convertPointToFixed(point + _RMechToRNav); }

    // Call superclass first to get rnav, then convert to rmech
    virtual PigPoint convertPointFromFixed(PigPoint fixed_point)
	{ return PigCoordM20rover::convertPointFromFixed(fixed_point) -
								_RMechToRNav; }

    // This is complicated because the offset between rnav and rmech is
    // measured along their X axes, while the offset is measured in the
    // Fixed frame.  So the true offset has to take the quaternion into
    // account.  Since this is defined as the origin of the frame measured
    // in the Fixed coordinate system, we simply convertToFixed(origin).
    virtual PigVector getFixedToFrameOffset()
				{ return convertPointToFixed(PigPoint(0,0,0)); }

    virtual const char *const getModelName()
			{ return PIG_FRAME_NAME_M20_RMECH; }

};

#endif

