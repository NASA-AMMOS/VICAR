////////////////////////////////////////////////////////////////////////
// FilterShapes
//
// Perform the actual filtering of the shapes
////////////////////////////////////////////////////////////////////////

#include "FilterShapes.h"

#include "PigVector.h"
#include "PigCameraModel.h"
#include "PigFileModel.h"
#include "PigCoordSystem.h"

#include "zvproto.h"

#include <stdlib.h>
#include <math.h>

int FilterShape::_do_print = true;
int FilterShape::_cache_valid = false;
PigFileModel *FilterShape::_cached_file = NULL;
PigCameraModel *FilterShape::_cached_camera = NULL;
double FilterShape::_cached_min_fov = 0.0;

#define LIMIT_MIN_FOV 0.01
#define MAX_ITER 100

////////////////////////////////////////////////////////////////////////
// Note that xyz can be passed as NULL, which indicates there is no
// XYZ data available.  Any volume-based shapes will be quietly ignored.
// Params is a pointer to the params array, which is copied internally.
// It can be null, in which case all parameters are 0.
////////////////////////////////////////////////////////////////////////

void FilterShape::setSourceImage(PigFileModel *file, PigCameraModel *camera,
                PigCoordSystem *cs, PigCoordSystem *site_cs, double *xyz[3],
                int nl, int ns, double *params)
{
    _file = file; _camera = camera; _cs = cs; _site_cs = site_cs;
    _nl=nl; _ns=ns;
    if (xyz != NULL) {
	_xyz[0]=xyz[0];
	_xyz[1]=xyz[1];
	_xyz[2]=xyz[2];
    }
    else {
	_xyz[0] = _xyz[1] = _xyz[2] = NULL;
    }
    if (params != NULL) {
	for (int i=0; i < PIG_MAX_FILTER_PARAMS; i++)
	    _params[i] = params[i];
    }
    // Compute the diagonal FOV.  As an approximation, we add the H and V
    // FOV's, and divide by 2 to get the angle from the center to the edge.
    // We then make sure we don't go bigger than 90 degrees (actually somewhat
    // shy of 90).
    // Note that we use the nominal FOV here, because it can get too tight
    // with extreme subframes, and the camera model math is good for the
    // full size frame even after adjusting for subframes, etc.
    //
    // Because this requires reading the camera mapping file every time,
    // and is done for every shape, we use a simple cache.  If the inputs
    // are the same as the cache, we re-use the value; otherwise we
    // re-compute it.  This could in theory break down if the camera is
    // modified (e.g. re-pointed) but pointing doesn't affect the nominal
    // FOV and other changes that could make a difference are very unlikely.

    if (_cache_valid && _file == _cached_file && _camera == _cached_camera) {
	_min_fov = _cached_min_fov;
    } else {
	_min_fov = cos((_file->getNominalFOV(_camera,0) +
			_file->getNominalFOV(_camera,1)) / 2.0);
	if (_min_fov < LIMIT_MIN_FOV)
	    _min_fov = LIMIT_MIN_FOV;
	_cached_file = _file;
	_cached_camera = camera;
	_cached_min_fov = _min_fov;
	_cache_valid = true;
	char msg[1024];
	sprintf(msg, "Using cos(FOV) of %f", _min_fov);
	zvmessage(msg, "");
    }

}


////////////////////////////////////////////////////////////////////////
// Polygon in image space.  Should be a convex polygon but others may work
// with the implementation.  Coordinates are relative to the nominal
// full frame and are 0-based.  Downsampling and subframing adjustments
// are applied here.  Use addFilterToMask2 if you don't want these
// adjustments.
//
// We have to create another polygon object (sigh) to avoid modifying the
// original.
////////////////////////////////////////////////////////////////////////

void FilterShapeImagePolygon::addFilterToMask(unsigned char *mask,
						int nlo, int nso, int dn)
{
    FilterShapeImagePolygon poly;
    poly.setPolygon(_nVertices, _vertices);

    poly.setSourceImage(_file, _camera, _cs, _site_cs, _xyz, _nl, _ns, _params);
    poly.convertToImageSpace();
    zvmessage("Image Polygon converted to local Image coordinates:", "");
    if (_do_print)
	poly.print();
    poly.addFilterToMask2(mask, nlo, nso, dn);

}

////////////////////////////////////////////////////////////////////////
// Similar to above, but we test an individual point.
////////////////////////////////////////////////////////////////////////

int FilterShapeImagePolygon::isPointFiltered(int line, int samp)
{
    FilterShapeImagePolygon poly;
    poly.setPolygon(_nVertices, _vertices);

    poly.setSourceImage(_file, _camera, _cs, _site_cs, _xyz, _nl, _ns, _params);
    poly.convertToImageSpace();

    return poly.pointInPoly(samp, line);		// x,y order
}

////////////////////////////////////////////////////////////////////////
// Actually modifies the vertices based on the downsampling/subframing of
// the source image.
////////////////////////////////////////////////////////////////////////

void FilterShapeImagePolygon::convertToImageSpace()
{
    float down_x = _file->getDownsampleXFactor(1.0);
    if (down_x == 0.0)
	down_x = 1.0;				// safety... shouldn't happen
    float down_y = _file->getDownsampleYFactor(1.0);
    if (down_y == 0.0)
	down_y = 1.0;				// safety... shouldn't happen

    int subframe_y = _file->getFirstLine(1) - 1;	// convert to 0-based
    int subframe_x = _file->getFirstLineSample(1) - 1;

    // Now create a temporary FilterShapeImagePolygon and call it to do the
    // filtering.  Note we use the 2 version because our points are already
    // properly compensated for subframing and such, due to the camera model.

    for (int i=0; i < _nVertices; i++) {
	_vertices[i]._x = (_vertices[i]._x - subframe_x) / down_x;
	_vertices[i]._y = (_vertices[i]._y - subframe_y) / down_y;
    }
}

////////////////////////////////////////////////////////////////////////
// Really do the masking.
//
// Coordinates are assumed to be in actual image space and are 0-based.
////////////////////////////////////////////////////////////////////////

void FilterShapeImagePolygon::addFilterToMask2(unsigned char *mask,
						int nlo, int nso, int dn)
{
    int min_x = nso;
    int min_y = nlo;
    int max_x = 0;
    int max_y = 0;

    for (int i=0; i < _nVertices; i++) {
	if (_vertices[i]._x < min_x)
	    min_x = (int)_vertices[i]._x;
	if (_vertices[i]._y < min_y)
	    min_y = (int)_vertices[i]._y;
	if (_vertices[i]._x > max_x)
	    max_x = (int)_vertices[i]._x;
	if (_vertices[i]._y > max_y)
	    max_y = (int)_vertices[i]._y;
    }
    if (min_x < 0)
	min_x = 0;
    if (min_y < 0)
	min_y = 0;
    if (max_x >= nso)
	max_x = nso-1;
    if (max_y >= nlo)
	max_y = nlo-1;

    for (int y = min_y; y <= max_y; y++) {
	for (int x = min_x; x <= max_x; x++) {

	    if (mask[y*nso + x] == 0) {	  // No need to check if already masked

		if (pointInPoly(x, y))
		    mask[y*nso + x] = dn;
	    }
	}
    }
}

////////////////////////////////////////////////////////////////////////
// Determine if a single point is inside the polygon or not.
//
// Code adapted from original by Chris Leger of section 348.
////////////////////////////////////////////////////////////////////////

int FilterShapeImagePolygon::pointInPoly(int x, int y)
{
    int i;
    double t0, t1, t2, sum;

    if (_nVertices <= 0)
	return 0;

    t0 = atan2(_vertices[0]._y - y, _vertices[0]._x - x);
    t1 = t0;
    sum = 0;
    for (i = 1; i < _nVertices; i++) {

	t2 = atan2(_vertices[i]._y - y, _vertices[i]._x - x);
	sum += (t1-t2);
	if (t1-t2 > M_PI) {
	    sum -= 2*M_PI;
	} else if (t1-t2 < -M_PI) {
	    sum += 2*M_PI;
	}
	t1 = t2;
    }

    t2 = t0;
    sum += (t1-t2);
    if (t1-t2 > M_PI) {
	sum -= 2*M_PI;
    } else if (t1-t2 < -M_PI) {
	sum += 2*M_PI;
    }

    if (fabs(sum) < 0.05) return 0;

    return 1;
}

////////////////////////////////////////////////////////////////////////
// Set the polygon to the specified points.  Note that this should only
// be done once, and should be done in lieu of readFromXML().  Calling
// more than one of those will cause a memory leak.
////////////////////////////////////////////////////////////////////////

void FilterShapeImagePolygon::setPolygon(int n, Point2D *vertices)
{
    _nVertices = n;
    _vertices = new Point2D[n];
    if (_vertices == NULL) {
	zvmessage("Out of memory in FilterShapeImagePolygon.setPolygon()", "");
	zabend();
    }
    for (int i=0; i < n; i++) {
	_vertices[i] = vertices[i];
    }
}

////////////////////////////////////////////////////////////////////////
// Polygon in projected space.  We first project the corners through the
// camera models into image space.  We then construct a FilterShapeImagePolygon
// with the resulting points and use that to actually do the filtering.
//
// There's no need to clip the polygon to the image; the Polygon will
// do that implicitly.  However, we do have to worry about projecting behind
// the camera and react to it.
//
// The FOV is the cos of the angle from the camera's pointing direction
// to the point in question.  An FOV <= 0 means the point is actually
// "behind" the camera.  This is not good for projecting the points, so
// we must adjust these.  Also, there is a numerical instability in the camera
// model math if you get too far away from the image area, so we limit the
// FOV to approximately the diagonal FOV (with some slop).
//
// If all the FOV's are < the limit, then we ignore the polygon.  If they're
// all < 0, it must be completely behind us, since there's no way to have all
// vertices behind us unless the polygon is.  If they're all < the limit, that
// means either the polygon is completely off the image, or it completely
// surrounds the image.  We ignore the surround case as something that should
// not occur, practically speaking.
//
// If that passes, then take the farthest point and "contract" it towards
// the best point (which must be in the FOV) by a factor of 3/4
// (arbitrarily chosen).  We keep doing this until all points are in front
// of us.  We never recalculate the "best" point so that the edges most
// likely to be in the image will not move.
//
// This algorithm has the potential of shrinking too much, exposing what
// shouldn't be exposed.  Better would be to calculate the edge of the
// image and clip the line at that point.  But, that's a lot more complex
// with the non-linear camera models, and the geometry is such that we
// should never be in this situation.  The FOV of the camera compared to
// the max angle subtended by any polygon means that we'll come in from
// "behind" us before the new point enters the image.  We just have to
// make sure the polygons aren't too narrow.
//
// The algorithm also ignores polygons that completely fill the FOV of
// the camera, because all the vertices are outside the FOV limit.  This
// should not occur, practically speaking.
////////////////////////////////////////////////////////////////////////

void FilterShapeProjectedPolygon::projectPoly()
{
    Point2D *poly_vertices = new Point2D[_nVertices];
    PigPoint *points = new PigPoint[_nVertices];
    double *FOV = new double[_nVertices];
    int i;

    if (poly_vertices == NULL || points == NULL || FOV == NULL) {
	zvmessage("Memory error in FilterShapeProjectedPolygon::projectPoly!","");
	_need_projection = FALSE;
	_projection_good = FALSE;
	delete poly_vertices; delete points; delete FOV;
	return;
    }

    PigCoordSystem *camera_cs = _camera->getCoordSystem();
    PigPoint camera_pos = _camera->getCameraPosition();
    PigVector camera_ori = _camera->getCameraOrientation();

    // Get PigPoint's for each vertex, and convert them to the CM's coord sys
    // Also calculate the FOV, the dot product between the look vector for
    // this point and the look vector of the camera.  This is used to reject
    // and adjust points "behind" the camera.

    double best_fov = -1.0;
    int best_fov_index = 0;
    double worst_fov = 1.0;
    int worst_fov_index = 0;

    for (i=0; i < _nVertices; i++) {
	PigPoint pt(_vertices[i]._x, _vertices[i]._y, _vertices[i]._z);
	// points[i] = camera_cs->convertPoint(pt, _cs);	// use camera_cs
	points[i] = pt;						// use camera_cs

	PigVector diff = points[i] - camera_pos;
	diff.normalize();
	FOV[i] = diff % camera_ori;
	if (FOV[i] > best_fov) {
	    best_fov = FOV[i];
	    best_fov_index = i;
	}
	if (FOV[i] < worst_fov) {
	    worst_fov = FOV[i];
	    worst_fov_index = i;
	}
    }

    // If the "best" FOV is behind us, the whole polygon is... so do nothing.
    // The * 2 is to avoid a theoretical infinite loop below.  If best was
    // infinitesimally bigger than _min_fov, then the 1/4 trick might not ever
    // bring worst within range...  as it is, we ultimately terminate since
    // the worst points migrate toward a point that is known good.

    if (best_fov <= _min_fov) {
	_need_projection = FALSE;
	_projection_good = FALSE;
	delete poly_vertices; delete points; delete FOV;
	return;
    }

    // Keep looping, adjusting the "worst" FOV until it is no longer behind us.
    // An infinite look can happen if the best is infinitesimally bigger than
    // _min_fov; the 1/4 trick might not ever bring the worst within range.
    // To avoid that, we impose an iteration limit.

    int n_iter = 0;

    while (worst_fov <= _min_fov && n_iter < MAX_ITER) {
	n_iter++;
	PigVector direction =
		(points[best_fov_index] - points[worst_fov_index]) / 4.0;

	// Move 1/4 of the way toward the best point
	points[worst_fov_index] = points[worst_fov_index] + direction;
	PigVector diff = points[worst_fov_index] - camera_pos;
	diff.normalize();
        FOV[worst_fov_index] = diff % camera_ori;

	// Recalculate the worst point

	worst_fov = 1.0;
	worst_fov_index = best_fov_index;	// just in case...

	for (i=0; i < _nVertices; i++) {
	    if (FOV[i] < worst_fov) {
	        worst_fov = FOV[i];
	        worst_fov_index = i;
	    }
	}

	if (worst_fov_index == best_fov_index) {	// Whoops!!
	    if (worst_fov <= _min_fov) {
		_need_projection = FALSE;
		_projection_good = FALSE;
		delete poly_vertices; delete points; delete FOV;
		return;			// Bad, do nothing
	    }
	    // else we'll pop out of the while loop
	}
    }

    // If we didn't get there, we must have exceeded the iteration count.
    // Ignore the polygon because it's close to the edge anyway.

    if (worst_fov <= _min_fov) {
	_need_projection = FALSE;
	_projection_good = FALSE;
	delete poly_vertices; delete points; delete FOV;
	return;
    }

    // Now that we've fixed up the points, project them to line/sample space

    for (i=0; i < _nVertices; i++) {
        double line, sample;

	_camera->XYZtoLS(points[i], 0, &line, &sample, camera_cs);

	// Convert to physical image coordinates
	line -= _file->getYOffset();
	sample -= _file->getXOffset();

	poly_vertices[i]._x = sample;
	poly_vertices[i]._y = line;
    }

    // Now create a temporary FilterShapeImagePolygon and call it to do the
    // filtering.  Note we use the 2 version because our points are already
    // properly compensated for subframing and such, due to the camera model.

    _projected_poly.setPolygon(_nVertices, poly_vertices);
    _projected_poly.setSourceImage(_file, _camera, _cs,_site_cs, _xyz,
					_nl, _ns, _params);
    _need_projection = FALSE;
    _projection_good = TRUE;
    delete poly_vertices; delete points; delete FOV;

}

////////////////////////////////////////////////////////////////////////

void FilterShapeProjectedPolygon::addFilterToMask(unsigned char *mask,
						int nlo, int nso, int dn)
{
    if (_need_projection) {
	projectPoly();
	if (_do_print) {
            zvmessage("Projected Polygon converted to Image Polygon:", "");
            _projected_poly.print();
	}
    }
    if (!_projection_good)			// projection failed...
	return;

    _projected_poly.addFilterToMask2(mask, nlo, nso, dn);
}

////////////////////////////////////////////////////////////////////////

int FilterShapeProjectedPolygon::isPointFiltered(int line, int samp)
{
    if (_need_projection) {
	projectPoly();
    }
    if (!_projection_good)			// projection failed
	return FALSE;

    return _projected_poly.isPointFiltered(line, samp);
}

////////////////////////////////////////////////////////////////////////
// Horizon in projected space.  For each point we simply project that
// point to a look vector, get the elevation of that vector, and compare
// it to the threshold.  It has the effect of chopping the horizon evenly
// all around... including distant mountains.  Since we can't range that
// far anyway, small loss.  But it could chop off nearby tall rocks, which
// would be a bad thing (especially on the MER hazcams).
//
// Note we must use the Site coord system for this!
////////////////////////////////////////////////////////////////////////

void FilterShapeProjectedHorizon::addFilterToMask(unsigned char *mask,
						int nlo, int nso, int dn)
{
    int i, j;

    // Simply go through the entire image, projecting each point and looking
    // at its elevation.  This isn't the most efficient way to do things, but
    // it's simple and gets the job done...

    for (j = 0; j < _file->getNL(); j++) {
	for (i = 0; i < _file->getNS(); i++) {

	    if (isPointFiltered(j, i)) {		// Mask it!
	        int idx = (int)(j*nso + i);
		mask[idx] = dn;
	    }
	}
    }
}

////////////////////////////////////////////////////////////////////////

int FilterShapeProjectedHorizon::isPointFiltered(int iline, int isamp)
{
    double line = (double)iline + _file->getYOffset();
    double sample = (double)isamp + _file->getXOffset();

    PigPoint origin;
    PigVector look;
    _camera->LStoLookVector(line, sample, origin, look, _site_cs);

    double elev = _site_cs->getEl(look);

    return (elev >= _elevation);		// Mask it!!
}

////////////////////////////////////////////////////////////////////////
// Box in XYZ space.
////////////////////////////////////////////////////////////////////////

void FilterShapeVolumeBox::addFilterToMask(unsigned char *mask,
						int nlo, int nso, int dn)
{
    if (_xyz[0] == NULL || _xyz[1] == NULL || _xyz[2] == NULL)
	return;				// no XYZ, can't do volumes

    _rotate.computeRotation(_params);

    double *xx = _xyz[0];
    double *yy = _xyz[1];
    double *zz = _xyz[2];
    for (int line=0; line<nlo; line++) {
	for (int samp=0; samp<nso; samp++) {
	    int idx = line*nso + samp;
	    if (mask[idx] == 0) {	  // No need to check if already masked

		double x = xx[idx];
		double y = yy[idx];
		double z = zz[idx];

		if (x == 0.0 && y == 0.0 && z == 0.0)
		    continue;		// invalid XYZ point

		if (_rotate._use_rotate)
		    _rotate.Rotate(x, y, z);

		// Check for being inside the box

		if ((x >= _min._x && x <= _max._x) &&
		    (y >= _min._y && y <= _max._y) &&
		    (z >= _min._z && z <= _max._z)) {

			mask[idx] = dn;
		}
	    }
	}
    }
}

////////////////////////////////////////////////////////////////////////
// Cylinder in XYZ space aligned with the Z axis.
////////////////////////////////////////////////////////////////////////

void FilterShapeVolumeZCylinder::addFilterToMask(unsigned char *mask,
						int nlo, int nso, int dn)
{
    if (_xyz[0] == NULL || _xyz[1] == NULL || _xyz[2] == NULL)
	return;				// no XYZ, can't do volumes

    // Precalculate bounding rectangles for speed

    // If we're outside here we're outside the cylinder
    double o_min_x = _axis_x - _radius;
    double o_max_x = _axis_x + _radius;
    double o_min_y = _axis_y - _radius;
    double o_max_y = _axis_y + _radius;

    // If we're inside here we're inside the cylinder
    double diag = sqrt(0.5);
    double i_min_x = _axis_x - _radius * diag;
    double i_max_x = _axis_x + _radius * diag;
    double i_min_y = _axis_y - _radius * diag;
    double i_max_y = _axis_y + _radius * diag;

    double rad_sqr = _radius * _radius;

    _rotate.computeRotation(_params);

    double *xx = _xyz[0];
    double *yy = _xyz[1];
    double *zz = _xyz[2];

    for (int line=0; line<nlo; line++) {
	for (int samp=0; samp<nso; samp++) {
	    int idx = line*nso + samp;
	    if (mask[idx] == 0) {	  // No need to check if already masked

		double x = xx[idx];
		double y = yy[idx];
		double z = zz[idx];

		if (x == 0.0 && y == 0.0 && z == 0.0)
		    continue;		// invalid XYZ point

		if (_rotate._use_rotate)
		    _rotate.Rotate(x, y, z);

		// Check for being inside the cylinder

		if (z < _min_z || z > _max_z)
		    continue;				// outside Z bounds

		if ((x < o_min_x || x > o_max_x) ||
		    (y < o_min_y || y > o_max_y))
		    continue;				// outside bounding box

		if ((x >= i_min_x && x <= i_max_x) &&
		    (y >= i_min_y && y <= i_max_y)) {
		    mask[idx] = dn;			// inside cylinder
		    continue;
		}

		// Nothing else worked, do it the hard way...

		if (((x - _axis_x) * (x - _axis_x)) +
		    ((y - _axis_y) * (y - _axis_y))  <= rad_sqr)
		    mask[idx] = dn;			// we're in!
	    }
	}
    }
}

////////////////////////////////////////////////////////////////////////
// "Washer" in XYZ space aligned with the Y axis.  A cylinder with an
// exclusion cylinder inside.
////////////////////////////////////////////////////////////////////////

void FilterShapeVolumeYWasher::addFilterToMask(unsigned char *mask,
						int nlo, int nso, int dn)
{

    if (_xyz[0] == NULL || _xyz[1] == NULL || _xyz[2] == NULL)
	return;				// no XYZ, can't do volumes

    // Precalculate bounding rectangles for speed

    // If we're outside here we're outside the outer cylinder(not part of shape)
    double o_min_x = _axis_x - _max_radius;
    double o_max_x = _axis_x + _max_radius;
    double o_min_z = _axis_z - _max_radius;
    double o_max_z = _axis_z + _max_radius;

    // If we're inside here we're inside the inner cylinder (not part of shape)
    double diag = sqrt(0.5);
    double i_min_x = _axis_x - _min_radius * diag;
    double i_max_x = _axis_x + _min_radius * diag;
    double i_min_z = _axis_z - _min_radius * diag;
    double i_max_z = _axis_z + _min_radius * diag;

    double min_rad_sqr = _min_radius * _min_radius;
    double max_rad_sqr = _max_radius * _max_radius;

    _rotate.computeRotation(_params);

    double *xx = _xyz[0];
    double *yy = _xyz[1];
    double *zz = _xyz[2];

    for (int line=0; line<nlo; line++) {
	for (int samp=0; samp<nso; samp++) {
	    int idx = line*nso + samp;
	    if (mask[idx] == 0) {	  // No need to check if already masked

		double x = xx[idx];
		double y = yy[idx];
		double z = zz[idx];

		if (x == 0.0 && y == 0.0 && z == 0.0)
		    continue;		// invalid XYZ point

		if (_rotate._use_rotate)
		    _rotate.Rotate(x, y, z);

		// Check for being inside the washer

		if (y < _min_y || y > _max_y)
		    continue;				// outside Y bounds

		if ((x < o_min_x || x > o_max_x) ||
		    (z < o_min_z || z > o_max_z))
		    continue;				// outside outer cyl

		if ((x > i_min_x && x < i_max_x) &&
		    (z > i_min_z && z < i_max_z)) {
		    continue;				// inside inner cylinder
		}

		// Nothing else worked, do it the hard way...

		double dist = ((x - _axis_x) * (x - _axis_x)) +
			      ((z - _axis_z) * (z - _axis_z));
		if (dist <= max_rad_sqr && dist >= min_rad_sqr)
		    mask[idx] = 255;			// we're in!
	    }
	}
    }
}

////////////////////////////////////////////////////////////////////////
// Pre-compute Rotation quaternion
////////////////////////////////////////////////////////////////////////

void FilterRotate::computeRotation(double *params)
{
    double angle = _angle;
    if (_add_angle != 0 && params != NULL &&
	_add_angle <= PIG_MAX_FILTER_PARAMS) {

	    angle += params[_add_angle-1];
    }
    angle = PigDeg2Rad(angle);

    PigVector axis(_axis_x, _axis_y, _axis_z);
    axis.normalize();
    _quat = PigQuaternion(axis, angle);

    _origin = PigVector(_x, _y, _z);
}

////////////////////////////////////////////////////////////////////////
// Apply rotation.  Because we are rotating the XYZ coordinate instead of
// the mask, we actually rotate backward by inverting the quat.
////////////////////////////////////////////////////////////////////////

void FilterRotate::Rotate(double &x, double &y, double &z)
{
    if (!_use_rotate)		// nothing to do...
	return;

    PigPoint xyz(x, y, z);

    PigPoint vect = xyz - _origin;
    vect = (~_quat) * vect;
    xyz = vect + _origin;

    xyz.getXYZ(x, y, z);
}

