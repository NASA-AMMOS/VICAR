////////////////////////////////////////////////////////////////////////
// PigFileModelNSYT
//
// NSYT-specific File model.  This only needs to handle exceptions to the
// multimission label API.
////////////////////////////////////////////////////////////////////////
#ifndef PIGFILEMODELNSYT_H
#define PIGFILEMODELNSYT_H

#include "PigFileModel.h"

class PigFileModelNSYT : public PigFileModel {

  protected:

    virtual void setupOffsets();   // ctor should use this to set x,y offsets
  
  public:

    PigFileModelNSYT(const char *filename, int unit, const char *mission);
    virtual ~PigFileModelNSYT();

    virtual const LblCoordinate_typ *getLblRoverCoordSys();	// Lander CS
    virtual const LblInstrumentState_typ *getLblInstrumentState();

    ////////////////////////////////////////////////////////////////////////
    // Determine instrument temperature using sensors data
    //////////////////////////////////////////////////////////////////////

    virtual float getInstrumentTemperature(float def); //InstrumentState

    //////////////////////////////////////////////////////////////////////
    // Caller is responsible for memory allocation of uid char array
    // uid array should be big enough to hold at least 33 char
    // Consists of HostId + InstrumentId + FrameId + SClock
    //////////////////////////////////////////////////////////////////////
    virtual void getUniqueId(char *uid)
	{ getUniqueIdImpl(uid); }

    //////////////////////////////////////////////////////////////////////
    // Compares 2 SPACECRFAFT_CLOCK_XXX_COUNT strings.
    // The function returns an integer greater than, equal  to,  or
    // less  than  0, if the string pointed to by sclk1 is greater
    // than, equal to, or less than the string  pointed  to  by  s2
    // respectively.
    // For more info see strcmp(..) man pages, since base class 
    // implementation uses it.
    // NSYT implementation takes advantage of knowing that clocks
    // are numerlic values of format: xxxx.xxx
    //////////////////////////////////////////////////////////////////////
    virtual int compareSCLK(const char *sclk1, const char *sclk2)
	{ return compareSCLKImpl(sclk1, sclk2); }

    // This is a bit of a hack.  We use the getArticulationDev() calls to
    // be compatible with PHX... but we get the values themselves from the
    // Camera Model group.
    virtual PigPoint getArticulationDevLocation(PigPoint def);
    virtual PigQuaternion getArticulationDevOrient(PigQuaternion def);

    ////////////////////////////////////////////////////////////////////
    // Get the Rover Motion Counter for the file.  The array must be supplied.
    // num_index on input should contain the max size of the supplied array;
    // on output it will contain the number of elements actually found (which
    // could be bigger than the supplied max size, but only that many will
    // actually be returned in the array).  PIG_MAX_SITE_INDEX (from
    // PigRoverStateManager.h) can be useful for array dimensioning.
    ////////////////////////////////////////////////////////////////////

    virtual void getRoverMotionCounter(int indices[], int &num_indices);

    // Returns the nominal number of RMC elements for this mission, or 0
    // if none.  A given image may not have all elements specified; this
    // call returns the max.
    virtual int getRoverMotionCounterCount();

    // Returns the string names of each RMC element.  String is statically
    // allocated so user need not free.  Returns NULL if index out of range.
    virtual char *getRoverMotionCounterName(int index);

    ////////////////////////////////////////////////////////////////////
    // Get the Flat Field Correction Parms for the file.  The array must 
    // be supplied.  num_index on input should contain the max size of the 
    // supplied array;  on output it will contain the number of elements 
    // actually found (which could be bigger than the supplied max size, 
    // but only that many will actually be returned in the array).  
    // PIG_MAX_FLAT_FIELD_INDEX (from RadiometryModel.h) can be useful 
    // for array dimensioning.
    ////////////////////////////////////////////////////////////////////

    virtual void getFlatFieldCorrectionCounter(float parms[], int &num_parms)
	{ getFlatFieldCorrectionCounterImpl(parms, num_parms); }

    ////////////////////////////////////////////////////////////////////
    // ILUT routines.  The first checks to see if an ILUT needs to be
    // applied.  The second returns the name of the ILUT.  We call the
    // SBM versions here; see the superclass .h file.
    ////////////////////////////////////////////////////////////////////
    
    virtual int isIlutNeeded() { return PigFileModel::isIlutNeededSBM(); }
    virtual const char *getIlutName() { return PigFileModel::getIlutNameSBM(); }

    ////////////////////////////////////////////////////////////////////
    // This is a set of routines used to get sets of mechanism angles or
    // positions for a mission.  A set might be all the mobility angles
    // (steering, bogie, differential), or the camera mast az/el, or arm
    // joints.  The intent of these routines is to provide info for the
    // PLACES database in a mission-independent manner.
    ////////////////////////////////////////////////////////////////////

    // Get the number of sets for this mission
    virtual int getMechanismSets();

    // Get info for a mechanism set.  The string should be assumed to
    // point to a static area (i.e. caller does not have to allocate or free).
    // Returns empty string and 0 if the set number is out of range.
    virtual void getMechanismSetInfo(int set, char *&name, int &num_elements);

    // Get info for a single element (angle, usually).  Again, the returned
    // strings should be assumed to point to a static area, so the caller does
    // not have to allocate or free.  Returns empty string and 0 if the inputs
    // are out of range.  Unit may be an empty string if N/A.
    virtual void getMechanismInfo(int set, int element,
                        char *&name, double &value, char *&unit);

    ////////////////////////////////////////////////////////////////////

    virtual const char *const getModelName() { return "FileModelNSYT"; }

};

#endif

