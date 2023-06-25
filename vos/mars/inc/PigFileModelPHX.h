////////////////////////////////////////////////////////////////////////
// PigFileModelPHX
//
// PHX-specific File model.  This only needs to handle exceptions to the
// multimission label API.
////////////////////////////////////////////////////////////////////////
#ifndef PIGFILEMODELPHX_H
#define PIGFILEMODELPHX_H

#include "PigFileModel.h"
#include "phx_lbl_articulation.h"
#include "phx_lbl_coordinate.h"
#include "phx_lbl_product_request.h"

class PigFileModelPHX : public PigFileModel {

  protected:

  LblArticulation_typ    *_phxLblSsiArticulation;
  LblArticulation_typ    *_phxLblRacArticulation;
  LblArticulation_typ    *_phxLblRaArticulation;

  virtual void setupOffsets();   // ctor should use this to set x,y offsets
  
  public:

    PigFileModelPHX(const char *filename, int unit, const char *mission);
    virtual ~PigFileModelPHX();

    virtual const LblArticulation_typ *getPhxLblSsiArticulation();
    virtual const LblArticulation_typ *getPhxLblRacArticulation();
    virtual const LblArticulation_typ *getPhxLblRaArticulation();
    virtual const LblInstrumentState_typ *getLblInstrumentState();

    ////////////////////////////////////////////////////////////////////////
    // Determine instrument temperature using sensors data
    //////////////////////////////////////////////////////////////////////
    virtual float getInstrumentTemperature(float def);	// InstrumentState
    virtual float getInstrumentTemperatureStart(float def);
    virtual float getInstrumentTemperatureEnd(float def);
    virtual float getInstrumentElectronicsTemperature(float def);
    virtual float getInstrumentOpticsTemperature(float def);


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
    // PHX implementation takes advantage of knowing that clocks
    // are nuphxic values of format: xxxx.xxx
    //////////////////////////////////////////////////////////////////////
    virtual int compareSCLK(const char *sclk1, const char *sclk2)
	{ return compareSCLKImpl(sclk1, sclk2); }

    virtual float getAzimuth(float def);
    virtual float getElevation(float def);

    virtual int checkAzimuth();
    virtual int checkElevation();

    virtual PigPoint getArticulationDevLocation(PigPoint def);
    virtual PigQuaternion getArticulationDevOrient(PigQuaternion def);

     // SSI has Focus adjustment mechanism which technically isn't
     // "focal length" which doesn't change, but in reality that
     // doesn't make any difference.
     virtual int getInstrumentFocalLengthCount(int def);

    // Use the Deployment state to indicate the RAC cover.
    // Not useful for other instruments.
    virtual const char *getInstrumentDeploymentState();

    ////////////////////////////////////////////////////////////////////
    // Get the Rover Motion Counter for the file.  The array must be supplied.
    // num_index on input should contain the max size of the supplied array;
    // on output it will contain the number of elements actually found (which
    // could be bigger than the supplied max size, but only that many will
    // actually be returned in the array).  PIG_MAX_SITE_INDEX (from
    // PigRoverStateManager.h) can be useful for array dimensioning.
    ////////////////////////////////////////////////////////////////////

    virtual void getRoverMotionCounter(int indices[], int &num_indices);

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

    virtual const char *const getModelName() { return "FileModelPHX"; }

};

#endif

