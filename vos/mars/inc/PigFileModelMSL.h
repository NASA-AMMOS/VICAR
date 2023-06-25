////////////////////////////////////////////////////////////////////////
// PigFileModelMSL
//
// MSL-specific File model.  This only needs to handle exceptions to the
// multimission label API.
////////////////////////////////////////////////////////////////////////
#ifndef PIGFILEMODELMSL_H
#define PIGFILEMODELMSL_H

#include "PigFileModel.h"

class PigFileModelMSL : public PigFileModel {

  protected:

    virtual void setupOffsets();   // ctor should use this to set x,y offsets
  
  public:

    PigFileModelMSL(const char *filename, int unit, const char *mission);
    virtual ~PigFileModelMSL();

    virtual const LblInstrumentState_typ *getLblInstrumentState();

    ////////////////////////////////////////////////////////////////////////
    // Determine instrument temperature using sensors data
    //////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////////
    // MSL has 24 camera-related temperature sensors.  We use the following
    // rules to determine the "best" temperature value to use.  The first
    // value that is available is used.  If a given camera doesn't exist (e.g.
    // MAHLI stereo partner), skip that step and move on.
    //
    // 1. Use the CCD temp of said camera.
    // 2. Use the CCD temp of left/right partner.
    // 3. Use the CCD temp of alternate A/B side side camera.
    // 4. Use the CCD temp of alternate A/B side left/right partner.
    // 5. Use the CCD temp of "similar" camera (i.e., Navcam/Mastcam/Chemcam,
    // fhaz/rhaz), in the order of 1-4 above.
    // 6. Use CCD temperature from any camera.
    // 7. Use the electronics temperature of said camera.
    // 8. Use the electronics temperature of partner cameras, in the order of
    //    2-4 above.
    // 9. Use the electronics temperature of similar camera, in the order of
    // 1-4 above.
    // 10.Use default.
    //
    // #7-10 are a last resort in view of the fact that MSL will be operating
    // warmup heaters inside the electronics (during nighttime and early morning)
    // that will raise camera electronics temperatures above CCD temperatures.
    // Thus any CCD temperature is at higher priority than any electronics
    // temperature measurement.
    //
    // These rules are a modification of the MER rules, which came from Keith
    // Novak from the MER thermal team.
    //
    // The available sensors and it's order is provided by Todd Litwin:
    // 0 A_FRONT_LEFT_HAZ_CCD
    // 1 A_FRONT_LEFT_HAZ_ELECTRONICS
    // 2 A_FRONT_RIGHT_HAZ_CCD
    // 3 A_FRONT_RIGHT_HAZ_ELECTRONICS
    // 4 A_REAR_LEFT_HAZ_CCD
    // 5 A_REAR_LEFT_HAZ_ELECTRONICS
    // 6 A_REAR_RIGHT_HAZ_CCD
    // 7 A_REAR_RIGHT_HAZ_ELECTRONICS
    // 8 A_LEFT_NAV_CCD
    // 9 A_LEFT_NAV_ELECTRONICS
    // 10 A_RIGHT_NAV_CCD
    // 11 A_RIGHT_NAV_ELECTRONICS
    // 12 B_FRONT_LEFT_HAZ_CCD
    // 13 B_FRONT_LEFT_HAZ_ELECTRONICS
    // 14 B_FRONT_RIGHT_HAZ_CCD
    // 15 B_FRONT_RIGHT_HAZ_ELECTRONICS
    // 16 B_REAR_LEFT_HAZ_CCD
    // 17 B_REAR_LEFT_HAZ_ELECTRONICS
    // 18 B_REAR_RIGHT_HAZ_CCD
    // 19 B_REAR_RIGHT_HAZ_ELECTRONICS
    // 20 B_LEFT_NAV_CCD
    // 21 B_LEFT_NAV_ELECTRONICS
    // 22 B_RIGHT_NAV_CCD
    // 23 B_RIGHT_NAV_ELECTRONICS
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
    // Most users should call the first version.  The second is a special
    // version used ONLY for camera model interpolation.  Here's what it does:
    // * If INTERPOLATION_METHOD is ONBOARD and INTERPOLATION_VALUE exists:
    //   + If the value is equal to either PRT
    //     - return the appropriate PRT (this eye or partner eye based on
    //       use_partner)
    //   + else return the interp value, regardless of use_partner
    // * else return the appropriate PRT (based on use_partner)
    //
    // This is based on: the FSW sets interp_value to the appropriate PRT
    // temperature for interpolation.  Unless there's only one model loaded,
    // in which case it's the temperature of that model.
    //
    // Use cases:
    // 1) Nominal, both cameras interp'd
    //    -> Use PRT values for both eyes (interp_value == prt for this case)
    // 2) Only one model loaded in both cameras (no interp)
    //    -> Use interp_value for both eyes.  If the one loaded model is one
    //       of the ones we have for interp, then this will return that model
    //       (it's interpolated but effectively returned  without change
    //       because the interp value is the same as the cmod's temperature).
    // 3) As in #2, but value happens to equal one of the PRT's
    //    -> Use PRT values.  Wrong, but not by much since the actual temp
    //       is very close to the model temp.  The results are consistent
    //       whether looking at the L or R image.
    // 4) One eye has interp models, the other has a constant model
    //    -> Eye with interp uses PRT, eye with constant uses that models' temp.
    //       This is very wrong, but is a totally off nominal case, and there's
    //       no way to do better without flags from the FSW.
    //////////////////////////////////////////////////////////////////////

    virtual float getInstrumentTemperature(float def); //InstrumentState
    virtual float getInstrumentTemperature(float def,int use_partner);

    // PRIVATE temperature routine used only by the above
  protected:
    float getInstrumentTemperatureFromName(float def, const char *inst_id);
  public:

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
    // MSL implementation takes advantage of knowing that clocks
    // are numslic values of format: xxxx.xxx
    //////////////////////////////////////////////////////////////////////
    virtual int compareSCLK(const char *sclk1, const char *sclk2)
	{ return compareSCLKImpl(sclk1, sclk2); }

    virtual float getAzimuth(float def);
    virtual float getElevation(float def);

    virtual int checkAzimuth();
    virtual int checkElevation();

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

    virtual const char *const getModelName() { return "FileModelMSL"; }

};

#endif

