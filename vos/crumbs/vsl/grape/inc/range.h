#ifndef _RANGE_H_
#define _RANGE_H_
// range.h 1.3 03/03/13 13:49:20
/** \file
 ** Coordinate space range/limits/bounding volume definition
 **/

#include <stdio.h>
#include <float.h>
#include "grape/matrix.h"

/// coordinate space options (leave 0 undefined e.g. for 'no limits')
enum {	RANGE_OBJECT_SPACE = 1,
	RANGE_MODEL_SPACE = 2,
	RANGE_WORLD_SPACE = 3
};

/// Coordinate range/limits/bounding volume
class Range {

public:
   int space; 		///< eg RANGE_OBJECT_SPACE
   double xmin;
   double xmax;
   double ymin;
   double ymax;
   double zmin; 
   double zmax;
   
   /// initialize limits to "nothing"
   void init() {	
   	xmin = ymin = zmin = FLT_MAX;
   	xmax = ymax = zmax = -FLT_MAX;
   }
   void empty() { init(); }
   
   /// initialize limits to "everything"
   void full() { 
   	xmin = ymin = zmin = -FLT_MAX;
   	xmax = ymax = zmax = FLT_MAX;
   }

   /// Return TRUE if bounding volume is empty
   int is_empty() {
   	return xmin > xmax || ymin > ymax || zmin > zmax;
   }

   /// Return TRUE if volume is exactly one single point
   int is_singular() {
   	return xmin==xmax && ymin==ymax && zmin==zmax;
   }

   /// Return TRUE if point is within specified range limits
   int in_range(const double p[3]) {
	return 	p[0] >= xmin && p[0] <= xmax &&
		p[1] >= ymin && p[1] <= ymax &&
		p[2] >= zmin && p[2] <= zmax;
   }

   /// Return TRUE if (float) point is within specified range limits
   int in_range(const float p[3]) {
	return 	p[0] >= xmin && p[0] <= xmax &&
		p[1] >= ymin && p[1] <= ymax &&
		p[2] >= zmin && p[2] <= zmax;
   }

   /// Return TRUE if other volume has non-zero overlap with this volume
   int overlaps(const Range *other) {
	return other->xmin < xmax && other->xmax > xmin &&
	       other->ymin < ymax && other->ymax > ymin &&
	       other->zmin < zmax && other->zmax > zmin;
   }

   /// Get size of bounding volume in each dimension
   void size(double *dx, double *dy, double *dz) {
	*dx = xmax - xmin;
	*dy = ymax - ymin;
	*dz = zmax - zmin;
   }

   /// Expand range limits if necessary to include point
   void include(const double p[3]) {
	if(p[0] < xmin) xmin = p[0];
	if(p[0] > xmax) xmax = p[0];
	if(p[1] < ymin) ymin = p[1];
	if(p[1] > ymax) ymax = p[1];
	if(p[2] < zmin) zmin = p[2];
	if(p[2] > zmax) zmax = p[2];
   }

   /// Expand range limits if necessary to include (float) point
   void include(const float p[3]) {
	if(p[0] < xmin) xmin = p[0];
	if(p[0] > xmax) xmax = p[0];
	if(p[1] < ymin) ymin = p[1];
	if(p[1] > ymax) ymax = p[1];
	if(p[2] < zmin) zmin = p[2];
	if(p[2] > zmax) zmax = p[2];
   }

   /// Expand volume to include another volume
   void include(const Range *other) {
	if(other->xmin < xmin) xmin = other->xmin;
	if(other->xmax > xmax) xmax = other->xmax;
	if(other->ymin < ymin) ymin = other->ymin;
	if(other->ymax > ymax) ymax = other->ymax;
	if(other->zmin < zmin) zmin = other->zmin;
	if(other->zmax > zmax) zmax = other->zmax;
   }

   /// Transform volume to different coordinate space.
   /** The result may not be an optimally small bounding volume.
    ** Brute force method is to transform 8 corners and find limits.
    ** This optimization based on "Transforming Axis-Aligned Bounding Boxes"
    ** by Jim Arvo, "Graphics Gems", Academic Press, 1990
    **/
   void transform(ZMatrix xf) {
	/* store limits in array for loop access */
	double Amin[3], Amax[3], Bmin[3], Bmax[3];
	Amin[0] = xmin; Amax[0] = xmax;
	Amin[1] = ymin; Amax[1] = ymax;
	Amin[2] = zmin; Amax[2] = zmax;

	/* Take care of translation */
	Bmin[0] = Bmax[0] = xf[0][3];
	Bmin[1] = Bmax[1] = xf[1][3];
	Bmin[2] = Bmax[2] = xf[2][3];

	/* Now find the extreme points by considering the product of the */
	/* min and max with each component of rotation matrix */
	for (int i = 0; i < 3; i++) {
		for (int j = 0; j < 3; j++) {
			double a = xf[i][j] * Amin[j];
			double b = xf[i][j] * Amax[j];
			if (a < b) {
				Bmin[i] += a; 
				Bmax[i] += b;
			} else {
				Bmin[i] += b;
				Bmax[i] += a;
			}
		}
	}

	/* move result back to this object */
	xmin = Bmin[0]; xmax = Bmax[0];
	ymin = Bmin[1]; ymax = Bmax[1];
	zmin = Bmin[2]; zmax = Bmax[2];
   }

   /// Dump range limits
   void dump(FILE *fp, const char *title) {
	fprintf(fp, "Range %s: X=%g:%g Y=%g:%g Z=%g:%g\n",
		title, xmin, xmax, ymin, ymax, zmin, zmax);
   }
};

#endif
