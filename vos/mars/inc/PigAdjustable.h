////////////////////////////////////////////////////////////////////////
// PigAdjustable
//
// Base class for anything that is adjustable via "pointing parameters".
// This is intended to be exactly like a Java interface.  It can be used
// for anything with a named set of parameters that can be adjusted via
// something like amoeba() (or manually).  The classic example is a pointing
// model, but this can also be used for surface models and rover locations.
////////////////////////////////////////////////////////////////////////
#ifndef PIGADJUSTABLE_H
#define PIGADJUSTABLE_H

// Must match the value defined in lbl_derived_image.h:LBL_PM_ITEMS
#define PIG_MAX_PARAMS	10	// Max # of pointing params for any subclass...
				// (update as needed)

class PigAdjustable {

  protected:
    PigAdjustable() { }		// not instantiable

  public:

    // These functions allow access to the pointing parameters, in order
    // to accomplish pointing corrections or "tweaks".  The content of
    // this data is highly subclass-specific, as each subclass and instrument
    // has its own set of parameters.  The getPointingParamName() function
    // returns the "name" of this parameter, so common software can identify
    // what it is.
    //
    // Each subclass *MUST* specify exactly which parameters are
    // returned in what order, so that mission-specific code can interpret
    // them.  Also, they should if possible follow a common pattern:
    // az/ra/lon, el/dec/lat, twist
    //
    // Parameter type names should be standardized as much as possible.
    // A new type of pointing info (such as a robot arm elbow angle) can
    // be added if necessary, but the existing strings should be used if
    // possible:
    // Azimuth, Elevation, Right Ascension, Declination, Twist
    // The error estimate is rather loosely defined as how much error is
    // expected in the pointing system.
    //!!!! Error estimate should perhaps be more rigorously defined
    // (e.g. 3-sigma value) but that is not yet necessary.
    // All relevant values are measured in the instrument "natural"
    // coordinate system (_pointing_cs).

    //!!!! TBD: The list above should be extended
    //!!!! TBD: Min/max ranges and error bars should be available
    //!!!! TBD: Need some way to identify parameters which affect lots of
    //!!!!      images, e.g. rover position for a panorama series.
    //!!!!	Resolution: use PigSite
    //!!!! TBD: Need to know basic "type" of param: angle, position, etc.
    //!!!! TBD: Do we need to deal with cross-dependencies (e.g. a unit
    //!!!!      vector's elements must maintain magnitude 1)?

    virtual int getPointingParamCount() = 0;
    virtual void getPointingParameters(double params[], const int max_count)=0;
    virtual void setPointingParameters(const double params[],const int count)=0;
    virtual void getPointingErrorEstimate(double errors[], const int max_count)
									= 0;
    virtual const char *const getPointingParamName(int i) = 0;	// 0-based

};

#endif

