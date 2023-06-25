#include "PigCylindricalProjectionJ.h"

PigCylindricalProjection::PigCylindricalProjection ()
{
  _w = 1100.0;
  _h = _w / 2.0;
  _min_elev = -M_PI / 2.0;
  _max_elev = M_PI / 2.0;
  _min_az = 0.0;
  _max_az = 2.0 * M_PI;

  _az_first_sample = 0.0;


  setScale (M_PI / _h);
  setNaturalScale (M_PI / _h);
  setOrigin (new PigPoint (0.0, 0.0, -1.0));
  
  _line_zero_el = _max_elev / getScale ();
}

PigVector PigCylindricalProjection::getLookVector (double sample, double line, PigCoordSystem *cs)
{
  double out_az, out_el;
  double ad = cs->getAzimuthDirection();
  double sc = getScale ();

  out_az = (ad * sample * sc) + _az_first_sample;

  out_el = (_line_zero_el - line) * getScale();

  //printf ("--< %.3f %.6f %.3f %.3f %.3f %.3f>--\n", ad, sc, out_az, out_el, sample, line);

  return cs->constructVector(out_az, out_el);

}

PigPoint2D *PigCylindricalProjection::getFromLookVector (PigSurfaceModel &sm, 
					PigVector *look, PigPoint cameraOrigin, PigCoordSystem cs,
					int &infinity)
{
  double new_az;
  double pi2Radians = 2 * M_PI;

  PigPoint *surf=new PigPoint(0.0,0.0,0.0); 

  int hits=sm.intersectRay(cameraOrigin, *look, *surf);

  if (hits <= 0) {
    infinity = 1;
  } else {
    infinity = 0;
  }

  sm.getRay(*(getOrigin()), *surf, infinity, *look);

  PigVector *look2=new PigVector(look->getX(), look->getY(), look->getZ());
  
  new_az = (cs.getAz(*look2) - _az_first_sample) * cs.getAzimuthDirection();
  if (new_az >= pi2Radians)
         new_az -=  pi2Radians;
  if (new_az < 0)
    new_az += pi2Radians;
  
  return new PigPoint2D((new_az / getScale()),
			(_line_zero_el - cs.getEl(*look2) / getScale())); 
  

}

PigVector *PigCylindricalProjection::getSurfacePoint (PigSurfaceModel &sm, 
					PigVector &look, PigVector &surf, int &infinity)
{
    int hits=sm.intersectRay(*(getOrigin()), look, surf);
    
	  if (hits <= 0) {
		infinity = 1;
	  } else {
		infinity = 0;
	  }
//    infinity = (hits <= 0);

    return &surf;
    
}
double PigCylindricalProjection::getWidth ()
{
  return _w;
}
double PigCylindricalProjection::getHeight()
{
  return _h;
}

double PigCylindricalProjection::getMinAz ()
{
  return _min_az;
}

void PigCylindricalProjection::setMinAz (double z)
{
  _min_az = z;
}

double PigCylindricalProjection::getMaxAz ()
{
  return _max_az;
}

void PigCylindricalProjection::setMaxAz (double z)
{
  _max_az = z;
}

double PigCylindricalProjection::getMinEl ()
{
  return _min_elev;
}

void PigCylindricalProjection::setMinEl (double z)
{
  _min_elev = z;
}

double PigCylindricalProjection::getMaxEl ()
{
  return _max_elev;
}

void PigCylindricalProjection::setMaxEl (double z)
{
  _max_elev = z;
}

double PigCylindricalProjection::getAzFirstSample()
{
  return _az_first_sample;
}

void PigCylindricalProjection::setAzFirstSample (double z)
{
  _az_first_sample = z;
}
void PigCylindricalProjection::setOrigin (PigVector *p)
{
  _origin = p;
}
void PigCylindricalProjection::setScale (double s)
{
  _line_zero_el = _max_elev / s;

  _scale = s;

}
double PigCylindricalProjection::getScale ()
{
  return _scale;
}
void PigCylindricalProjection::setNaturalScale (double s)
{
  _natural_scale = s;
}
double PigCylindricalProjection::getNaturalScale ()
{
  return _natural_scale;
}
PigVector *PigCylindricalProjection::getOrigin()
{
  return _origin;
}

double PigCylindricalProjection::getLineZeroElevation ()
{
  return _line_zero_el;
}

void PigCylindricalProjection::setLineZeroElevation(double val)
{
  _line_zero_el = val;
}

