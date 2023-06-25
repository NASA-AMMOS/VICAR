////////////////////////////////////////////////////////////////////////
// PigRoverStateManagerMSL
//
// MSL-specific version of PigRoverStateManager.  It doesn't have to do
// much beyond what the superclass does.
////////////////////////////////////////////////////////////////////////
#ifndef PIGROVERSTATEMANAGERMSL_H
#define PIGROVERSTATEMANAGERMSL_H

#include "PigRoverStateManager.h"

class PigMission;
class PigMSL;

class PigRoverStateManagerMSL : public PigRoverStateManager {

  protected:

  public:
    PigRoverStateManagerMSL(PigMission *mission) :
			 PigRoverStateManager(mission) { }
    virtual ~PigRoverStateManagerMSL() { }

    // Read in the given file.  This is an override of PigSolutionManager's
    // version.  Here, we look to see if the file is a directory and if so,
    // look for the MSL RMC database files, passing each one to
    // PSM::readSolutionFile.  If not, the original file is passed along to
    // the superclass.

    virtual void readSolutionFile(char *filename, int exclude_telemetry);

};

#endif

