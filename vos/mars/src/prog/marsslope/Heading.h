////////////////////////////////////////////////////////////////////////
// Heading.h
//
// Heading, a derived class from SlopeFunction, evaluates 
// Slope Heading using:
//   - UVW normal vector
//
////////////////////////////////////////////////////////////////////////

#ifndef _HEADING___
#define _HEADING___

#include "SlopeFunction.h"


class Heading : public SlopeFunction
{
public:

	Heading( const char *sf_name )	: SlopeFunction( sf_name )
	{ }

	char *getPrintName() { return "SLOPE HEADING"; }
	char *getLabelName() { return "SLOPE_HEADING_MAP"; }

	double evaluate( const PigPoint& xyz, const PigVector& uvw )
	{
		double u = uvw.getX();
		double v = uvw.getY();

		double S = (180.0/M_PI) * ( atan2(v,u) );

		return S;
	}
};


#endif
