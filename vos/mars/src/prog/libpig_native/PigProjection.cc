#include "PigProjectionJ.h"

PigVector PigProjection::getLookVector (double sample, double line, PigCoordSystem *cs)
{
  PigVector v;
  return v;
}
PigPoint2D *PigProjection::getFromLookVector (PigSurfaceModel &sm, PigVector *look, 
					     PigVector cameraOrigin, PigCoordSystem cs, int &infinity)
{
  return new PigPoint2D ();
}
PigVector *PigProjection::getSurfacePoint (PigSurfaceModel &sm, PigVector &look, PigVector &surf, int &infinity)
{
  return 0;
}

PigVector *PigProjection::getOrigin () {
  return _origin;
}
void PigProjection::setOrigin (PigVector *o) {
  _origin = o;
}

double PigProjection::getScale () {
  return _scale;
}
void PigProjection::setScale (double s) {
  _scale = s;
}

double PigProjection::getWidth() { return 0; }
double PigProjection::getHeight() { return 0; }
