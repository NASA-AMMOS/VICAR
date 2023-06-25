////////////////////////////////////////////////////////////////////////
// SlopeFunction.h
//
// Base class for computing slope-related products.
//
////////////////////////////////////////////////////////////////////////

#ifndef _SLOPE_FUNCTION___
#define _SLOPE_FUNCTION___

#include "mars_support.h"

#include "PigMission.h"
#include "PigFileModel.h"
#include "PigLabelModel.h"
#include "PigCameraModel.h"
#include "PigPointingModel.h"
#include "PigSurfaceModel.h"
#include "PigVector.h"
#include "RadiometryModel.h"
#include "PigCoordSystem.h"
#include "PigCSReference.h"
#include "mat3.h"


class SlopeFunction
{
protected:
	
	// The SINGLETON PATTERN requires the code initializing static
	// variables below be uncommented:
	/*
	static SlopeFunction *_slope;
	static SlopeFunction *_heading;
	static SlopeFunction *_magnitude;
	static SlopeFunction *_direction;
	static SlopeFunction *_ntilt;
	static SlopeFunction *_solar;
	*/

	char* _sf_name;

	double SA;	// solar angle

public:

	PigPoint sfOrigin;

	// Returns the name of the SlopeFunction type as specified by the user
	const char* getSlopeFunctionName() { return _sf_name;	}

	// Returns the long description of SlopeFunction type
	virtual char *getPrintName() = 0;

	// Returns the name as it should be in the label (or at least,
	// what PigLabelModel.setSlope() expects).
	virtual char *getLabelName() = 0;
	
	// Returns boolean indicating whether SlopeFunction object 
	// uses Origin or Solar Angle in its calculations
	virtual bool usesOrigin()	{	return false;	}
	virtual bool usesSA()		{	return false;	}

	// Sets Solar Angle to value specified by user
	void setSolarAngle( double solar_angle )	{  SA = solar_angle;	}

	// Calculates the value of SlopeFunction type
	virtual double evaluate( const PigPoint& xyz, const PigVector& uvw ) = 0;

	// Compare the SlopeFunction to known names and return the
	// proper subclass.
	static SlopeFunction *getSlopeFunctionObject( const char *slopefunction_name );

	// Constructor
	SlopeFunction( const char *sf_name )
	{	
		_sf_name = NULL;

		if( sf_name )
		{	_sf_name = strdup(sf_name);	}
	}

	// Destructor
	~SlopeFunction()
	{	
		if( _sf_name )
		{	delete _sf_name;	}
	}
};

#endif
