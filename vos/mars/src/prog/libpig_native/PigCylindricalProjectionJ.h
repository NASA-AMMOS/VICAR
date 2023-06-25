#ifndef _PIGCYLINDRICALPROJECTIONJ_H_
#define _PIGCYLINDRICALPROJECTIONJ_H_

#include "PigProjectionJ.h"
#include "PigVector.h"
#include "mars_support.h"
#include "PigSurfaceModel.h"
#include "PigCoordSystem.h"
#include "PigPoint2D_C.h"

class PigCylindricalProjection : public PigProjection {

public:

  double _w;
  double _h;
  double _scale;
  double _natural_scale;
  double _min_elev;
  double _max_elev;
  double _min_az;
  double _max_az;
  double _az_first_sample;
  double _az_last_sample;
  double _line_zero_el;

  PigVector *_origin;

  PigCylindricalProjection ();

  PigVector getLookVector (double sample, double line, PigCoordSystem *cs);

  PigPoint2D *getFromLookVector (PigSurfaceModel &sm, PigVector *look, 
				PigVector cameraOrigin, PigCoordSystem cs, int &infinity);

  PigVector *getSurfacePoint (PigSurfaceModel &sm, PigVector &look, PigVector &surf, int &infinity);

  double getWidth ();
  double getHeight ();

  double getMinAz ();
  void   setMinAz (double z);
  
  double getMaxAz ();
  void   setMaxAz(double z);

  double getMinEl ();
  void   setMinEl(double z);

  double getMaxEl();
  void   setMaxEl(double z);

  double getAzFirstSample ();
  void   setAzFirstSample (double z);

  double getLineZeroElevation ();
  void   setLineZeroElevation (double el);

  void setScale (double newScale);
  double getScale ();

  void setNaturalScale (double newScale);
  double getNaturalScale ();

  void setOrigin (PigPoint *p);
  PigVector *getOrigin ();
};

#endif
