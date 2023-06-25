#ifndef _PIGPROJECTIONJ_H_
#define _PIGPROJECTIONJ_H_

#include "PigVector.h"
#include "mars_support.h"
#include "PigSurfaceModel.h"
#include "PigCoordSystem.h"
#include "PigPoint2D_C.h"

class PigProjection {
protected:
  double _scale;
  PigVector *_origin;

public:
  virtual PigVector getLookVector (double sample, double line, PigCoordSystem *cs);
  virtual PigPoint2D *getFromLookVector (PigSurfaceModel &sm, PigVector *look, 
				PigVector cameraOrigin, PigCoordSystem cs, int &infinity);
  virtual PigVector *getSurfacePoint (PigSurfaceModel &sm, PigVector &look, PigVector &surf, int &infinity);

  PigVector *getOrigin () ;
  virtual void setOrigin (PigVector *pt);

  virtual double getScale ();
  virtual void setScale (double s);

  virtual double getWidth ();
  virtual double getHeight ();

//  int    infinity;  violates thread safety
};

#endif
