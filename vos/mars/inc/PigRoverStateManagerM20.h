////////////////////////////////////////////////////////////////////////
// PigRoverStateManagerM20
//
// M20-specific version of PigRoverStateManager.  It doesn't have to do
// much beyond what the superclass does.
////////////////////////////////////////////////////////////////////////
#ifndef PIGROVERSTATEMANAGERM20_H
#define PIGROVERSTATEMANAGERM20_H

#include "PigRoverStateManager.h"

class PigMission;
class PigM20;

class PigRoverStateManagerM20 : public PigRoverStateManager {

  protected:

  public:
    PigRoverStateManagerM20(PigMission *mission) :
		PigRoverStateManager(mission) { }
    virtual ~PigRoverStateManagerM20() { }

    // Read in the given file.  This is an override of PigSolutionManager's
    // version.  Here, we look to see if the file is a directory and if so,
    // look for the M20 RMC database files, passing each one to
    // PSM::readSolutionFile.  If not, the original file is passed along to
    // the superclass.

    virtual void readSolutionFile(char *filename, int exclude_telemetry);

};

#endif

