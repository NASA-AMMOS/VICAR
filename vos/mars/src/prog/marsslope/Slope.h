////////////////////////////////////////////////////////////////////////
// Slope.h
//
// Slope, a derived class from SlopeFunction, evaluates 
// Slope using:
//   - UVW normal vector
//
////////////////////////////////////////////////////////////////////////

#ifndef _SLOPE___
#define _SLOPE___

#include "SlopeFunction.h"


class Slope : public SlopeFunction
{
public:

	Slope( const char *sf_name ) : SlopeFunction( sf_name )
	{ }

	char *getPrintName() { return "SLOPE"; }
	char *getLabelName() { return "SLOPE_MAP"; }

	double evaluate( const PigPoint& xyz, const PigVector& uvw )
	{
		double u = uvw.getX();
		double v = uvw.getY();
		double w = uvw.getZ();

		double S = (180.0/M_PI) * ( (M_PI/2.0) + atan( w/ sqrt( u*u + v*v ) ) );
		
		return S;
	}
};


#endif
