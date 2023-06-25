////////////////////////////////////////////////////////////////////////
// PigFileModelMER
//
// MER-specific File model.  This only needs to handle exceptions to the
// multimission label API.
////////////////////////////////////////////////////////////////////////
#ifndef PIGFILEMODELMER_H
#define PIGFILEMODELMER_H

#include "PigFileModel.h"
#include "mer_lbl_articulation.h"
#include "mer_lbl_coordinate.h"
#include "mer_lbl_product_request.h"

class PigFileModelMER : public PigFileModel {

  protected:

  virtual void setupOffsets();   // ctor should use this to set x,y offsets
  
  public:

    PigFileModelMER(const char *filename, int unit, const char *mission);
    virtual ~PigFileModelMER();

    virtual const LblArticulation_typ *getLblRsmArticulation();		// PMA
    virtual const LblArticulation_typ *getLblArmArticulation();		// IDD
    virtual const LblInstrumentState_typ *getLblInstrumentState();

    ////////////////////////////////////////////////////////////////////////
    // MER has 9 temperature sensors.  We use the following rules to determine
    // the "best" temperature to use:
    // 
    // 1. Use the CCD temp of said camera, if it exists.
    // Else
    // 2. Use the CCD temp of neighboring camera (left/right partner), 
    //    if it is available.
    // Else
    // 3. Use the CCD temp of "similar" camera (i.e., Navcam/Pancam)
    // Else
    // 4. Use CCD temperature from any camera.
    // Else
    // 5. Use the electronics temperature of said camera.
    // Else
    // 6. Use the electronics temperature of similar camera.
    // Else
    // 7. Use default.
    //
    // #5-7 are a last resort in view of the fact that MER will be operating
    // warmup heaters inside the electronics(during nighttime and early morning)
    // that will raise camera electronics temperatures above CCD temperatures.
    // Thus any CCD temperature is at higher priority than any electronics 
    // temperature measurement.  The most significant consequence of this is
    // that MI CCD is the best available proxy for all four Hazcam CCDs.
    //
    // This rules are according to Keith Novak from the thermal team.
    // 
    // The available sensors are :
    // 0 FRONT HAZ ELECTRONICS
    // 1 REAR HAZ ELECTRONICS
    // 2 LEFT PAN ELECTRONICS
    // 3 LEFT PAN CCD
    // 4 RIGHT PAN CCD
    // 5 LEFT NAV CCD
    // 6 MI CCD
    // 7 MI ELECTRONICS
    // 8 EDL CCD
    //
    // The number in front of sensor's name is the position in 
    // INSTRUMENT_TEMPERATURE array.  The values in that array are floats
    // and represent degC.  The value 0.0, is treated as no-reading from 
    // the sensor.
    // 
    // If the sensor breaks, hardware people usually see it as an infinite 
    // resistance and thus it reads a full scale DN value.  We check against
    // large DN value (50 C) as the max criterion for ignoring one sensor's 
    // data and moving on to the next one.
    // 
    //////////////////////////////////////////////////////////////////////
    virtual float getInstrumentTemperature(float def);	// InstrumentState

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
    // MER implementation takes advantage of knowing that clocks
    // are numeric values of format: xxxx.xxx
    //////////////////////////////////////////////////////////////////////
    virtual int compareSCLK(const char *sclk1, const char *sclk2)
	{ return compareSCLKImpl(sclk1, sclk2); }

    virtual float getAzimuth(float def);
    virtual float getElevation(float def);

    virtual int checkAzimuth();
    virtual int checkElevation();

    virtual float getIddJoint1Angle(float def);
    virtual float getIddJoint2Angle(float def);
    virtual float getIddJoint3Angle(float def);
    virtual float getIddJoint4Angle(float def);
    virtual float getIddJoint5Angle(float def);

    virtual int checkIddJoint1Angle();
    virtual int checkIddJoint2Angle();
    virtual int checkIddJoint3Angle();
    virtual int checkIddJoint4Angle();
    virtual int checkIddJoint5Angle();

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

    virtual const char *const getModelName() { return "FileModelMER"; }

};

#endif

