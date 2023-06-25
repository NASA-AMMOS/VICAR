#ifndef _PIGVERTICALPROJECTIONJ_H_
#define _PIGVERTICALPROJECTIONJ_H_


#include "PigProjectionJ.h"
#include "PigVector.h"
#include "mars_support.h"
#include "PigSurfaceModel.h"
#include "PigCoordSystem.h"
#include "PigPoint2D_C.h"

class PigVerticalProjection : public PigProjection {
public:
  double   _w;
  double   _h;

  double _maxx;
  double _maxy;
  double _vert_scale;

  int      _nlo;
  int      _nso;
  double   _scale;

  PigPoint *_origin;
  PigVector *_down;
  PigVector *_up;

  PigVerticalProjection() ;
  PigVector getLookVector (double sample, double line, PigCoordSystem *cs);

  PigPoint2D *getFromLookVector (PigSurfaceModel &sm, PigVector *look, 
				PigVector cameraOrigin, PigCoordSystem cs);

  PigVector *getSurfacePoint (PigSurfaceModel &sm, PigVector &look, PigVector &surf);

  double getWidth ();
  double getHeight ();


  double getMaxX();
  void   setMaxX (double z);

  double getMaxY();
  void   setMaxY (double z);

  double getVertScale();
  void   setVertScale (double z);

  void setScale (double newScale);
  double getScale ();

  void setOrigin (PigPoint *p);
  PigVector *getOrigin ();

  void setUpAzimuth(double u);
  double getUpAzimuth();

private:
  void setBasics();
  void setBasics (double newScale);
};

#endif
