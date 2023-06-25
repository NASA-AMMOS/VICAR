#include <math.h>

#include <iostream>
using namespace std;

#include "SlopeFunction.h"
#include "Slope.h"
#include "Heading.h"
#include "Magnitude.h"
#include "Direction.h"
#include "Ntilt.h"
#include "Solar.h"


// The SINGLETON PATTERN requires the code initializing static
// variables below be uncommented:
/*
SlopeFunction *SlopeFunction::_slope;
SlopeFunction *SlopeFunction::_heading;
SlopeFunction *SlopeFunction::_magnitude;
SlopeFunction *SlopeFunction::_direction;
SlopeFunction *SlopeFunction::_ntilt;
SlopeFunction *SlopeFunction::_solar;
*/


//#####################################################################
//
// Compare the SlopeFunction to known names and return the
// proper subclass.
//
//#####################################################################

//#####################################################################

SlopeFunction *SlopeFunction::getSlopeFunctionObject( const char *sf_name )
{
	if( strcasecmp(sf_name, "slope") == 0 )
	{
		cout << "Creating SLOPE object" << endl;
		return new Slope( sf_name );
	}
	else if( strcasecmp(sf_name, "heading") == 0 )
	{
		cout << "Creating SLOPE HEADING object" << endl;
		return new Heading( sf_name );
	}
	else if( strcasecmp(sf_name, "magnitude") == 0 )
	{
		cout << "Creating SLOPE MAGNITUDE object" << endl;
		return new Magnitude( sf_name );
	}
	else if( strcasecmp(sf_name, "direction") == 0 )
	{
		cout << "Creating ROVER DIRECTION object" << endl;
		return new Direction( sf_name );
	}
	else if( strcasecmp(sf_name, "ntilt") == 0 )
	{
		cout << "Creating NORTHERLY TILT object" << endl;
		return new Ntilt( sf_name );
	}
	else if( strcasecmp(sf_name, "solar") == 0 )
	{
		cout << "Creating SOLAR ENERGY object" << endl;
		return new Solar( sf_name );
	}
	else
	{	cout << "Invalid SlopeFunction type entered!" << endl;	exit(1);	}
	
	return 0;
}

//#####################################################################
//
// The SINGLETON PATTERN requires the code initializing global 
// variables above be uncommented.
//
//#####################################################################
/*
SlopeFunction *SlopeFunction::getSlopeFunctionObject( const char *sf_name )
{
	if( strcasecmp(sf_name, "slope") == 0 )
	{
		cout << "Creating SLOPE object" << endl;
		_slope = new Slope( sf_name );		// create new Slope object
		return _slope;
	}
	else if( strcasecmp(sf_name, "heading") == 0)
	{
		cout << "Creating SLOPE HEADING object" << endl;
		_heading = new Heading( sf_name );
		return _heading;
	}
	else if( strcasecmp(sf_name, "magnitude") == 0)
	{
		cout << "Creating SLOPE MAGNITUDE object" << endl;
		_magnitude = new Magnitude( sf_name );
		return _magnitude;
	}
	else if( strcasecmp(sf_name, "direction") == 0)
	{
		cout << "Creating ROVER DIRECTION object" << endl;
		_direction = new Direction( sf_name );
		return _direction;
	}
	else if( strcasecmp(sf_name, "ntilt") == 0)
	{
		cout << "Creating NORTHERLY TILT object" << endl;
		_ntilt = new Ntilt( sf_name );
		return _ntilt;
	}
	else if( strcasecmp(sf_name, "solar") == 0)
	{
		cout << "Creating SOLAR ENERGY object" << endl;
		_solar = new Solar( sf_name );
		return _solar;
	}
	else
	{	cout << "Invalid SlopeFunction type entered!" << endl;	exit(1);	}
	
	return 0;
}
*/

//#####################################################################
