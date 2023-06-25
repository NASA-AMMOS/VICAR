////////////////////////////////////////////////////////////////////////
// Solar.h
//
// Solar, a derived class from SlopeFunction, evaluates 
// Solar Energy using:
//   - Solar Angle
//   - UVW normal vector
//
////////////////////////////////////////////////////////////////////////

#ifndef _SOLAR___
#define _SOLAR___

#include "SlopeFunction.h"


class Solar : public SlopeFunction
{
public:

	Solar( const char *sf_name ) : SlopeFunction( sf_name )
	{ }

	char *getPrintName() { return "SOLAR ENERGY"; }
	char *getLabelName() { return "SOLAR_ENERGY_MAP"; }

	bool usesSA()	{	return true;	}

	double evaluate( const PigPoint& xyz, const PigVector& uvw )
	{
		PigVector sunDirection( cos(SA*M_PI/180.0), 0, -cos((90.0-SA)*M_PI/180.0) );

		double S = sunDirection % uvw;

		return S;
	}
};


#endif
