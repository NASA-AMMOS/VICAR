////////////////////////////////////////////////////////////////////////
// PigFileModelPsyche
//
// Psyche-specific File model.  This only needs to handle exceptions to the
// multimission label API.
////////////////////////////////////////////////////////////////////////
#ifndef PIGFILEMODELPSYCHE_H
#define PIGFILEMODELPSYCHE_H

#include "PigFileModel.h"

class PigFileModelPsyche : public PigFileModel {

  protected:

    virtual void setupOffsets();   // ctor should use this to set x,y offsets
  
  public:

    PigFileModelPsyche(const char *filename, int unit, const char *mission);
    virtual ~PigFileModelPsyche();

    virtual const LblInstrumentState_typ *getLblInstrumentState();

    ////////////////////////////////////////////////////////////////////////
    // Determine instrument temperature using sensors data
    //////////////////////////////////////////////////////////////////////
    // See the .CC for extensive comment block...

    virtual float getInstrumentTemperature(float def); //InstrumentState
    virtual float getInstrumentTemperature(float def,int use_partner);

    // PRIVATE temperature routine used only by the above
  protected:
    float getInstrumentTemperatureFromName(float def, const char *inst_id);
  public:

    //////////////////////////////////////////////////////////////////////
    // Compares 2 SPACECRFAFT_CLOCK_XXX_COUNT strings.
    // The function returns an integer greater than, equal  to,  or
    // less  than  0, if the string pointed to by sclk1 is greater
    // than, equal to, or less than the string  pointed  to  by  s2
    // respectively.
    // For more info see strcmp(..) man pages, since base class 
    // implementation uses it.
    // Psyche implementation takes advantage of knowing that clocks
    // are numslic values of format: xxxx.xxx
    //////////////////////////////////////////////////////////////////////
    virtual int compareSCLK(const char *sclk1, const char *sclk2)
	{ return compareSCLKImpl(sclk1, sclk2); }

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


    virtual const char *const getModelName() { return "FileModelPsyche"; }

};

#endif

