#ifndef _PIGPOLARPROJECTIONJ_H_
#define _PIGPOLARPROJECTIONJ_H_

#include <math.h>

#include "PigProjectionJ.h"
#include "PigVector.h"
#include "mars_support.h"
#include "PigSurfaceModel.h"
#include "PigCoordSystem.h"
#include "PigPoint2D_C.h"

class PigPolarProjection : public PigProjection {
public:
  double   _w;
  double   _h;
  double   _min_elev;
  double   _max_elev;
  double   _min_az;
  double   _max_az;
  double   _nadir_line;
  double   _nadir_sample;
  double   _up_azimuth;
  double   _pi_half_radians;
  double   _az_first_sample;
  int      _nlo;
  int      _nso;
  double   _scale;
  double   _natural_scale;
  PigPoint *_origin;

  PigPolarProjection(double newScale) ;
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

  void setScale (double newScale);
  double getScale ();

  void setNaturalScale (double newScale);
  double getNaturalScale ();

  void setOrigin (PigPoint *p);
  PigVector *getOrigin ();

  void setUpAzimuth(double u);
  double getUpAzimuth();

private:
  void setBasics();
  void setBasics (double newScale);
};

#endif
