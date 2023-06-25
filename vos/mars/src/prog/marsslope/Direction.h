////////////////////////////////////////////////////////////////////////
// Direction.h
//
// Direction, a derived class from SlopeFunction, evaluates 
// Rover Direction using:
//   - Origin point
//   - XYZ point
//   - UVW normal vector
//
////////////////////////////////////////////////////////////////////////

#ifndef _DIRECTION___
#define _DIRECTION___

#include "SlopeFunction.h"


class Direction : public SlopeFunction
{
public:

	Direction( const char *sf_name ) : 	SlopeFunction( sf_name )
	{ }

	char *getPrintName() { return "ROVER DIRECTION"; }
	char *getLabelName() { return "RADIAL_SLOPE_MAP"; }
	
	bool usesOrigin()	{	return true;	}

	double evaluate( const PigPoint& xyz, const PigVector& uvw )
	{
		double x = xyz.getX();
		double y = xyz.getY();
		/* double z = */ xyz.getZ();

		double u = uvw.getX();
		double v = uvw.getY();
		double w = uvw.getZ();

		double x0 = sfOrigin.getX();
		double y0 = sfOrigin.getY();
		/* double z0 = */ sfOrigin.getZ();		

		double dist = sqrt( (x-x0)*(x-x0) + (y-y0)*(y-y0) );
		double Vx = (x-x0) / dist;
		double Vy = (y-y0) / dist;
		double S = -(180.0/M_PI) * atan2( Vx*u + Vy*v, -w );
			
		return S;
	}
};


#endif
