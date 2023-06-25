////////////////////////////////////////////////////////////////////////
// PigRoverStateManagerPHX
//
// PHX-specific version of PigRoverStateManager.  It doesn't have to do
// much beyond what the superclass does.
////////////////////////////////////////////////////////////////////////
#ifndef PIGROVERSTATEMANAGERPHX_H
#define PIGROVERSTATEMANAGERPHX_H

#include "PigRoverStateManager.h"

class PigMission;
class PigPHX;

class PigRoverStateManagerPHX : public PigRoverStateManager {

  protected:

    PigPHX *_mission;	// Mission obj needed to get host ID in readFile

  public:
    PigRoverStateManagerPHX(PigMission *mission) :
			PigRoverStateManager(mission) { }
    virtual ~PigRoverStateManagerPHX() { }

    // Read in the given file.  This is an override of PigSolutionManager's
    // version.  Here, we look to see if the file is a directory and if so,
    // look for the PHX RMC database files, passing each one to PSM::readFile.
    // If not, the original file is passed along to the superclass.

    virtual void readSolutionFile(char *filename, int exclude_telemetry);

};

#endif

