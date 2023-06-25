#include"PigVerticalProjectionJ.h"

PigVerticalProjection::PigVerticalProjection ()
{
  _w = 1100.0;
  _h = _w / 2.0;
  _maxx = 5.0;         // got this from the marsmap.pdf
  _maxy = 5.0;         // same for this one
  _vert_scale = 100.0; // pixels/meter

  _down = new PigVector (0.0, 0.0, -1.0);
  _up = new PigVector (0.0, 0.0, 1.0);

  setScale (M_PI / _h);
  setOrigin (new PigPoint (0.0, 0.0, -1.0));
  setBasics ();
}

PigVector PigVerticalProjection::getLookVector (double sample, double line, PigCoordSystem *cs)
{
  
  double x_ctr = (_nlo/2 - sample) / _vert_scale;
  double y_ctr = cs->getAzimuthDirection() * (line - _nso/2) / _vert_scale;
  
  PigVector p(x_ctr, y_ctr, 0.0);
  return p;

}

PigPoint2D *PigVerticalProjection::getFromLookVector (PigSurfaceModel &sm, PigVector *look, PigPoint cameraOrigin, PigCoordSystem cs)
{
  // code was done in line of the implementation file!
  return 0;

}

PigPoint *PigVerticalProjection::getSurfacePoint (PigSurfaceModel &sm, PigVector &look, PigPoint &surf)
{
  int hits = sm.intersectRay(look, *_down, surf);
  if (hits < 0)
    hits =  sm.intersectRay(look, *_up, surf);
  return &surf;

}

void PigVerticalProjection::setBasics ()
{
    _nso = (long)(2 * _maxx * _vert_scale);
    _nlo = (long)(2 * _maxy * _vert_scale);
}
double PigVerticalProjection::getWidth ()
{
  return M_PI * 2.0 / _scale;
}
double PigVerticalProjection::getHeight()
{
  return M_PI / _scale;
}

void PigVerticalProjection::setOrigin (PigVector *p)
{
  _origin = p;
}
void PigVerticalProjection::setScale (double s)
{
  _scale = s;

}
double PigVerticalProjection::getScale ()
{
  return _scale;
}
PigVector *PigVerticalProjection::getOrigin()
{
  return _origin;
}
void PigVerticalProjection::setVertScale (double newVScale)
{
    _vert_scale = newVScale;
}
double PigVerticalProjection::getVertScale ()
{
  return _vert_scale;
}

void PigVerticalProjection::setMaxX(double nx)
{
    _maxx = nx;
}
double PigVerticalProjection::getMaxX()
{
    return _maxx;
}
void PigVerticalProjection::setMaxY(double ny)
{
    _maxy = ny;
}
double PigVerticalProjection::getMaxY()
{
    return _maxy;
}
