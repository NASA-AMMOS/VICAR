////////////////////////////////////////////////////////////////////////
// Magnitude.h
//
// Magnitude, a derived class from SlopeFunction, evaluates 
// Slope Magnitude using:
//   - UVW normal vector
//
////////////////////////////////////////////////////////////////////////

#ifndef _MAGNITUDE___
#define _MAGNITUDE___

#include "SlopeFunction.h"


class Magnitude : public SlopeFunction
{
public:

	Magnitude( const char *sf_name ) : SlopeFunction( sf_name )
	{ }

	char *getPrintName() { return "SLOPE MAGNITUDE"; }
	char *getLabelName() { return "SLOPE_MAGNITUDE_MAP"; }

	double evaluate( const PigPoint& xyz, const PigVector& uvw )
	{
		double u = uvw.getX();
		double v = uvw.getY();

		double S = sqrt(u*u + v*v);

		return S;
	}
};


#endif
