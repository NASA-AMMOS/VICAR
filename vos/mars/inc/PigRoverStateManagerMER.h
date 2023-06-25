////////////////////////////////////////////////////////////////////////
// PigRoverStateManagerMER
//
// MER-specific version of PigRoverStateManager.  It doesn't have to do
// much beyond what the superclass does.
////////////////////////////////////////////////////////////////////////
#ifndef PIGROVERSTATEMANAGERMER_H
#define PIGROVERSTATEMANAGERMER_H

#include "PigRoverStateManager.h"

class PigMission;
class PigMER;

class PigRoverStateManagerMER : public PigRoverStateManager {

  protected:

  public:
    PigRoverStateManagerMER(PigMission *mission) :
			PigRoverStateManager(mission) { }
    virtual ~PigRoverStateManagerMER() { }

    // Read in the given file.  This is an override of PigSolutionManager's
    // version.  Here, we look to see if the file is a directory and if so,
    // look for the MER RMC database files, passing each one to
    // PSM::readSolutionFile.  If not, the original file is passed along to
    // the superclass.

    virtual void readSolutionFile(char *filename, int exclude_telemetry);

};

#endif

