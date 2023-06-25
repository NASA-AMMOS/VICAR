////////////////////////////////////////////////////////////////////////
// PigRoverStateManagerFIDO
//
// FIDO-specific version of PigRoverStateManager.  It doesn't have to do
// much beyond what the superclass does.
////////////////////////////////////////////////////////////////////////
#ifndef PIGROVERSTATEMANAGERFIDO_H
#define PIGROVERSTATEMANAGERFIDO_H

#include "PigRoverStateManager.h"

class PigRoverStateManagerFIDO : public PigRoverStateManager {

  protected:

  public:
    PigRoverStateManagerFIDO(PigMission *mission) :
			PigRoverStateManager(mission) { }
    virtual ~PigRoverStateManagerFIDO() { }

};

#endif

