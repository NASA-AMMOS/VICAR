#include "PigPolarProjectionJ.h"

PigPolarProjection::PigPolarProjection (double scale)
{
  _w = 1100.0;
  _h = _w / 2.0;
  _min_elev = -M_PI / 2.0;
  _max_elev = M_PI / 2.0;
  _min_az = 0.0;
  _max_az = 2.0 * M_PI;
  _up_azimuth = 0;
  _az_first_sample = 0.0;
  _pi_half_radians = M_PI / 2.0;

  _origin = 0;

  setScale (M_PI / _h);
  setNaturalScale (M_PI / _h);
  setOrigin (new PigPoint (0.0, 0.0, -1.0));
  setBasics (getScale());
}

PigVector PigPolarProjection::getLookVector (double sample, double line, PigCoordSystem *cs)
{
      double x_ctr = sample - _nadir_sample;
      double y_ctr = _nadir_line - line;

      //-----------------------------------------------------------------------
      // set "polar" and get range 
      // create a vector, determine out_el using (vector.range * scale - pi/2)
      // get out_az using pi/2 - (vector.getAzimuth - up_azimuth)
      //-----------------------------------------------------------------------
      double range = sqrt(x_ctr * x_ctr + y_ctr * y_ctr);

      double out_el = range * _scale - _pi_half_radians;
      double out_az = _up_azimuth + (_pi_half_radians - atan2 (y_ctr, x_ctr)) * cs->getAzimuthDirection();

      return cs->constructVector(out_az, out_el);

}

PigPoint2D *PigPolarProjection::getFromLookVector (PigSurfaceModel &sm, PigVector *look, PigPoint cameraOrigin, PigCoordSystem cs, int &infinity)
{
  double new_az;
  PigPoint2D newPoint;
  double pi2Radians = 2.0 * M_PI;

  PigPoint *surf=new PigPoint(0.0,0.0,0.0); 
  int hits = sm.intersectRay(cameraOrigin, *look, *surf);
  infinity = (hits <= 0);
  
  sm.getRay(*(getOrigin()), *surf, infinity, *look);
  
  double out_az = _pi_half_radians - (cs.getAz(*look) - _up_azimuth) * cs.getAzimuthDirection();
  
  double out_range = (cs.getEl(*look) + _pi_half_radians) / getScale();
  
  double rc = out_range * cos (0.0);

  delete surf;

  return new PigPoint2D( ((rc * cos(out_az)) + _nadir_sample), (_nadir_line - (rc * sin(out_az))) );

}

PigPoint *PigPolarProjection::getSurfacePoint (PigSurfaceModel &sm, PigVector &look, PigPoint &surf, int &infinity)
{
  int hits = sm.intersectRay (*_origin, look, surf);
  infinity = (hits <= 0);

  return &surf;
}
void PigPolarProjection::setBasics ()
{
  printf ("in setBasics\n");
  setBasics (0.0);
}

void PigPolarProjection::setBasics (double nwScale)
{
  if (nwScale != 0) {
    _nlo=(long)floor((_max_elev - (-_pi_half_radians)) / nwScale);
    if ((_nlo % 2) == 0)
     _nlo++;
    
    _nso = _nlo;
    _nadir_line = _nlo / 2;
    _nadir_sample = _nso / 2;
  }
  printf ("N. nso %d nlo %d samp %.5f line %.5f\n", _nso, _nlo, _nadir_sample, _nadir_line); 
}
double PigPolarProjection::getWidth ()
{
  return M_PI * 2.0 / _scale;
}
double PigPolarProjection::getHeight()
{
  return M_PI / _scale;
}

double PigPolarProjection::getMinAz ()
{
  return _min_az;
}

void PigPolarProjection::setMinAz (double z)
{
  _min_az = z;
}

double PigPolarProjection::getMaxAz ()
{
  return _max_az;
}

void PigPolarProjection::setMaxAz (double z)
{
  _max_az = z;
}

double PigPolarProjection::getMinEl ()
{
  return _min_elev;
}

void PigPolarProjection::setMinEl (double z)
{
  _min_elev = z;
}

double PigPolarProjection::getMaxEl ()
{
  return _max_elev;
}

void PigPolarProjection::setMaxEl (double z)
{
  _max_elev = z;
  printf ("N called maxEl\n");
  setBasics();
}

double PigPolarProjection::getAzFirstSample()
{
  return _az_first_sample;
}

void PigPolarProjection::setAzFirstSample (double z)
{
  _az_first_sample = z;
}
void PigPolarProjection::setOrigin (PigVector *p)
{
  if (_origin)
    delete _origin;

  _origin = p;
}
void PigPolarProjection::setScale (double s)
{
  _scale = s;
}
void PigPolarProjection::setNaturalScale (double s)
{
	_natural_scale = s;
}
double PigPolarProjection::getNaturalScale ()
{
  return _natural_scale;
}
double PigPolarProjection::getScale ()
{
  return _scale;
}
PigVector *PigPolarProjection::getOrigin()
{
  return _origin;
}
void PigPolarProjection::setUpAzimuth (double u)
{
  _up_azimuth = u;
}
double PigPolarProjection::getUpAzimuth ()
{
  return _up_azimuth;
}
