////////////////////////////////////////////////////////////////////////
// xvector.h
//
// Routine that accepts two unit vectors and origins, and computes the
// intersection point of the vectors (actually, the point between the vectors
// that is closest to both).  Also returns the error (miss distance).
//
// Returns 0 for okay, or 1 if there is no solution
//
// Note that this subroutine uses C++ linkage and is thus not
// callable from C or Fortran without bridges.
////////////////////////////////////////////////////////////////////////

#ifndef _XVECTOR_H
#define _XVECTOR_H

#include "PigVector.h"

int xvector(const PigPoint cam1, const PigPoint cam2,
	    const PigVector uvw1, const PigVector uvw2,
	    PigPoint &xyz, double *error);

int xvector_multiview(const PigPoint *lookOrigin, 
				      const PigVector *lookVector, 
					  PigPoint &xyz, 
					  double *error, 
					  const int numimages, 
					  const double epsilon, 
					  const double error_thresh, 
					  const double cond_number_thresh);

int angular_gradient(const PigPoint *lookOrigin, 
					 const PigVector *lookVector, 
					 PigPoint &xyz, 
					 PigPoint current_point, 
					 int numimages);

int xvector_optimize(const PigPoint *lookOrigin, 
					 const PigVector *lookVector, 
					 PigPoint &xyz, 
					 PigPoint current_point, 
					 const int numimages,
					 const double drift_threshold);

#endif

