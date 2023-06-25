////////////////////////////////////////////////////////////////////////
// PigRoverStateManagerNSYT
//
// NSYT-specific version of PigRoverStateManager.  It doesn't have to do
// much beyond what the superclass does.
//
// Although InSight is not a rover, it's easier to have this class
// even if it basically does nothing.
////////////////////////////////////////////////////////////////////////
#ifndef PIGROVERSTATEMANAGERNSYT_H
#define PIGROVERSTATEMANAGERNSYT_H

#include "PigRoverStateManager.h"

class PigMission;
class PigNSYT;

class PigRoverStateManagerNSYT : public PigRoverStateManager {

  protected:

  public:
    PigRoverStateManagerNSYT(PigMission *mission) :
			PigRoverStateManager(mission) { }
    virtual ~PigRoverStateManagerNSYT() { }

    // Note that readFile() is not overridden here since we do not have
    // an RMC database.

};

#endif

