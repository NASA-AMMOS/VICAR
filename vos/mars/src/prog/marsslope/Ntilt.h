////////////////////////////////////////////////////////////////////////
// Ntilt.h
//
// Ntilt, a derived class from SlopeFunction, evaluates 
// Northerly Tilt using:
//   - UVW normal vector
//
////////////////////////////////////////////////////////////////////////

#ifndef _NTILT___
#define _NTILT___

#include "SlopeFunction.h"


class Ntilt : public SlopeFunction
{
public:

	Ntilt( const char *sf_name ) : SlopeFunction( sf_name )
	{ }

	char *getPrintName() { return "NORTHERLY TILT"; }
	char *getLabelName() { return "NORTHERLY_TILT_MAP"; }

	double evaluate( const PigPoint& xyz, const PigVector& uvw )
	{
		double u = uvw.getX();

		double S = (180.0/M_PI) * asin(u);

		return S;
	}
};


#endif
