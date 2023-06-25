////////////////////////////////////////////////////////////////////////
// PigRoverStateManagerColdarm
//
// Coldarm-specific version of PigRoverStateManager.  It doesn't have to do
// much beyond what the superclass does.
////////////////////////////////////////////////////////////////////////
#ifndef PIGROVERSTATEMANAGERCOLDARM_H
#define PIGROVERSTATEMANAGERCOLDARM_H

#include "PigRoverStateManager.h"

class PigMission;
class PigColdarm;

class PigRoverStateManagerColdarm : public PigRoverStateManager {

  protected:

  public:
    PigRoverStateManagerColdarm(PigMission *mission) :
		PigRoverStateManager(mission) { }
    virtual ~PigRoverStateManagerColdarm() { }

    // Read in the given file.  This is an override of PigSolutionManager's
    // version.  Here, we look to see if the file is a directory and if so,
    // look for the Coldarm RMC database files, passing each one to
    // PSM::readSolutionFile.  If not, the original file is passed along to
    // the superclass.

    virtual void readSolutionFile(char *filename, int exclude_telemetry);

};

#endif

