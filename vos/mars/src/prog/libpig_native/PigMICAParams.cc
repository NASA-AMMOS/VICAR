#include <stdlib.h>
#include "PigMICAParamsJ.h"


double	PigMICAParams::_normal[3] = { 0.0, 0.0, -1.0 };
double	PigMICAParams::_ground[3] = { 0.0, 0.0, 0.294 };
char *	PigMICAParams::_surface;
char *	PigMICAParams::_pointmethod;

PigMICAParams::PigMICAParams ()
{
	_surface = ( char * )malloc( 12 );
	strcpy( PigMICAParams::_surface, "PLANE" );
	_pointmethod = ( char * )malloc( 36 );
	strcpy( PigMICAParams::_pointmethod, "" );
	//printf("PigMICAParamGetter, constructor \n" );
}

/////////////////////////////////////////////////////////////////////////
//// This parameter processor is to be used with MICA.
//// It simply returns (count=0) for all parameter requests except
//// CONFIG_PATH (which is required for most processing).
////
//// Note that this function is not called normally; the user must register
//// it if desired.
//////////////////////////////////////////////////////////////////////////
//
void PigMICAParams::PigMICAParamGetter(char *name, void *value, int *count, int maxcnt,
								int length, void *clientData)
{
	//printf("PigMICAParamGetter, name = %s\n", name );
	if (strcasecmp(name, "CONFIG_PATH") == 0) {
		if (length == 0 || length > 17)
			strcpy((char *)value, "$MARS_CONFIG_PATH");
		else
			strcpy((char *)value, "");
		*count = 1;
		return;
	}
	else if (strcasecmp(name, "NORMAL") == 0) {
		double* myValue = (double *)value;
		myValue[0] = PigMICAParams::_normal[0];
		myValue[1] = PigMICAParams::_normal[1];
		myValue[2] = PigMICAParams::_normal[2];
		*count = 3;
		//printf("PigMICAParamGetter, X = %e, Y = %e, Z=%e\n", myValue[0], myValue[1], myValue[2] );
		return;
	}
	else if (strcasecmp(name, "GROUND") == 0) {
		double* myValue = (double *)value;
		myValue[0] = PigMICAParams::_ground[0];
		myValue[1] = PigMICAParams::_ground[1];
		myValue[2] = PigMICAParams::_ground[2];
		*count = 3;
		//printf("PigMICAParamGetter, X = %e, Y = %e, Z=%e\n", myValue[0], myValue[1], myValue[2] );
		return;
	}
	else if (strcasecmp(name, "SURFACE") == 0) {
		strcpy((char *)value, PigMICAParams::_surface);
		*count = 1;
		//printf("PigMICAParamGetter, surface = %s\n", (char *)value );
		return;
	}
	else if (strcasecmp(name, "POINT_METHOD") == 0) {
		strcpy((char *)value, PigMICAParams::_pointmethod);
		*count = 1;
		//printf("PigMICAParamGetter, pointmethod = %s\n", (char *)value );
		return;
	}
	// Return nothing for any other parameter
	*count = 0;
	return;
}

void PigMICAParams::PigMICASetNormal( double* normal )
{
	//printf("PigMICASetNormal, X = %e, Y = %e, Z=%e\n", normal[0], normal[1], normal[2] );
	PigMICAParams::_normal[0] = normal[0];
	PigMICAParams::_normal[1] = normal[1];
	PigMICAParams::_normal[2] = normal[2];
}

void PigMICAParams::PigMICASetGround( double* ground )
{
	PigMICAParams::_ground[0] = ground[0];
	PigMICAParams::_ground[1] = ground[1];
	PigMICAParams::_ground[2] = ground[2];
	//printf("PigMICASetGround, X = %e, Y = %e, Z=%e\n", ground[0], ground[1], ground[2] );
}

void PigMICAParams::PigMICASetSurface( const char* surface )
{
	//printf("PigMICASetSurface, surface = %s\n", surface );
	strcpy( PigMICAParams::_surface, surface );
}

void PigMICAParams::PigMICASetPointMethod( const char* pointmethod )
{
	//printf("PigMICASetSurface, pointmethod = %s\n", pointmethod );
	strcpy( PigMICAParams::_pointmethod, pointmethod );
}
