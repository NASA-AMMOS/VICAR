#ifndef _PIGMICAPARAMSJ_H_
#define _PIGMICAPARAMSJ_H_

#include "PigVector.h"
#include "mars_support.h"
#include "PigSurfaceModel.h"
#include "PigCoordSystem.h"
#include "PigPoint2D_C.h"

class PigMICAParams {
protected:
public:
  static double _normal[3];
  static double _ground[3];
  static char *  _surface;
  static char *  _pointmethod;
//  PigVector *_origin;

  PigMICAParams ();

  static void PigMICAParamGetter(char *name, void *value, int *count, 
		  						int maxcnt, int length, void *clientData);

 void PigMICASetNormal( double* normal );
 void PigMICASetGround( double* ground );
 void PigMICASetSurface( const char* surface );
 void PigMICASetPointMethod( const char* pointmethod );
};

#endif
