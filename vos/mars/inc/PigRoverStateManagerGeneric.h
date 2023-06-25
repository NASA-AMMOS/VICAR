////////////////////////////////////////////////////////////////////////
// PigRoverStateManagerGeneric
//
// Generic-mission version of PigRoverStateManager.  It doesn't have to do
// much beyond what the superclass does.  But it does manage the CS's from
// the file.
////////////////////////////////////////////////////////////////////////
#ifndef PIGROVERSTATEMANAGERGENERIC_H
#define PIGROVERSTATEMANAGERGENERIC_H

#include "PigRoverStateManager.h"

class PigRoverStateManagerGeneric : public PigRoverStateManager {

  protected:

  public:
    PigRoverStateManagerGeneric(PigMission *mission) :
			PigRoverStateManager(mission) { }
    virtual ~PigRoverStateManagerGeneric() { }

    // Override to do it special...

    virtual void addFileCoordSystems(PigFileModel *file);

};

#endif

