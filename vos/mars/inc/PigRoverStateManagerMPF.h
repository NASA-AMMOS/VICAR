////////////////////////////////////////////////////////////////////////
// PigRoverStateManagerMPF
//
// MPF-specific version of PigRoverStateManager.  It doesn't have to do
// much beyond what the superclass does.  But it does manage the CS's from
// the file.
////////////////////////////////////////////////////////////////////////
#ifndef PIGROVERSTATEMANAGERMPF_H
#define PIGROVERSTATEMANAGERMPF_H

#include "PigRoverStateManager.h"

class PigRoverStateManagerMPF : public PigRoverStateManager {

  protected:

  public:
    PigRoverStateManagerMPF(PigMission *mission) :
			PigRoverStateManager(mission) { }
    virtual ~PigRoverStateManagerMPF() { }

    // Override to do it special...

    virtual void addFileCoordSystems(PigFileModel *file);

};

#endif

