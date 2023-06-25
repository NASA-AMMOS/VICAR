///////////////////////////////////////////////////////////////////////
// xvector_optimize.cc
//
// Routines for computing an optimized intersection point (triangulation) of
// multiple unit direction vectors, given an initial estimated obtained
// through averaging pairwise midpoint estimates or linear triangulation. The 
// error (miss distance) is also computed.
//
// Returns 0 for okay, or 1 if there is no solution
//
// Note that some subroutines use C++ linkage and are thus not
// callable from C or Fortran without bridges.
///////////////////////////////////////////////////////////////////////

#include "xvector.h"
#include <math.h>
#include <iostream>
#include <Eigen/Dense>
#include <Eigen/SVD>
#include <Eigen/Core>


using namespace std;
using namespace Eigen;


////////////////////////////////////////////////////////////////////////

// Computes the angular gradient value for an XYZ point in 3D space relative
// to a set of look origins and vectors

int angular_gradient(const PigPoint *lookOrigin, 
					 const PigVector *lookVector, 
					 PigPoint &xyz, 
					 PigPoint current_point, 
					 int numimages) 
{
	Vector3d gradient = Vector3d::Zero();
	Vector3d point;
	point << current_point.getX(), current_point.getY(), current_point.getZ();
      
    for(unsigned int i = 0; i < numimages; ++i) {
        Vector3d w; // look vector of the current observation
		w << lookVector[i].getX(), lookVector[i].getY(), lookVector[i].getZ();
		Vector3d C;
		C << lookOrigin[i].getX(), lookOrigin[i].getY(), lookOrigin[i].getZ();
        Vector3d v; // vector from the camera center to the current point
		v = point - C;
        double denom2 = v.dot(v);
        double denom = sqrt(denom2);
        double denom15 = pow(denom2, 1.5);
        double vdotw = v.dot(w);
        gradient.x() += (-w.x()/denom) + ((v.x()*vdotw)/denom15);
        gradient.y() += (-w.y()/denom) + ((v.y()*vdotw)/denom15);
        gradient.z() += (-w.z()/denom) + ((v.z()*vdotw)/denom15);
    }

	PigPoint final_point(gradient(0), gradient(1), gradient(2));

	// Check for NaN and Inf:

	if(std::isnan(final_point.getX()) || 
	   std::isnan(final_point.getY()) || 
	   std::isnan(final_point.getZ())) {
		return 1;
	}
	if(std::isinf(final_point.getX()) || 
	   std::isinf(final_point.getY()) || 
	   std::isinf(final_point.getZ())) {
		return 1;
	}

	xyz = final_point;

	return 0;
}


// Apply angular optimization to an initially-computed XYZ point by applying
// gradient descent on an angular cost function

int xvector_optimize(const PigPoint *lookOrigin, 
					 const PigVector *lookVector, 
					 PigPoint &xyz, 
					 PigPoint current_point, 
					 const int numimages,
					 const double drift_threshold)
{
    double precision = 1e-25;
    Vector3d gradient_old;
    Vector3d x_old;
    Vector3d x_new;
	Vector3d gradient;
    double epsilon = .001;
    double point_diff_norm2;
    int count = 150;
	PigPoint grad_result; 

	x_new << current_point.getX(), current_point.getY(), current_point.getZ();
	int status = angular_gradient(lookOrigin, lookVector, grad_result, 
								  current_point, numimages);
	gradient << grad_result.getX(), grad_result.getY(), grad_result.getZ();
   
    do {
        x_old = x_new;
        gradient_old = gradient;
        x_new = x_old - epsilon * gradient_old;
		PigPoint point(x_new(0), x_new(1), x_new(2));
       	int status = angular_gradient(lookOrigin, lookVector, grad_result,
									  point, numimages);

		// If the result of the gradient descent is an invalid value (Inf or 
		// NaN, return 1:

		if(status == 1)
			return 1;

		gradient << grad_result.getX(), grad_result.getY(), grad_result.getZ();
        Vector3d point_diff = x_new - x_old;
        Vector3d grad_diff = gradient - gradient_old;
        double point_diff_x = point_diff.x();
        double point_diff_y = point_diff.y();
        double point_diff_z = point_diff.z();
        point_diff_norm2 = point_diff_x*point_diff_x +
			   			   point_diff_y*point_diff_y +
			   			   point_diff_z*point_diff_z;

        // Compute adaptive step size (sometimes get a divide by zero hence
        // the subsequent check):

        epsilon = point_diff_norm2/(point_diff_x*grad_diff.x() +
									point_diff_y*grad_diff.y() +
									point_diff_z*grad_diff.z());
        epsilon = (epsilon != epsilon) ||
          (epsilon == numeric_limits<double>::infinity()) ? .001 : epsilon;
        --count;

    } while(point_diff_norm2 > precision && count-- > 0);


	// Check for NaN and Inf (again, just in case):

	PigPoint vec(x_new(0), x_new(1), x_new(2));

	if(std::isnan(vec.getX()) || 
	   std::isnan(vec.getY()) || 
	   std::isnan(vec.getZ())) {
		return 1;
	}
	if(std::isinf(vec.getX()) || 
	   std::isinf(vec.getY()) || 
	   std::isinf(vec.getZ())) {
		return 1;
	}

	// Check for drifting. If the final point drifted very far from the
	// initial, it likely fell into a bad local minima, or was ill-posed 
	// to begin with, usually due to short baselines. This usually leads 
	// to very large coordinates. The following simple check does the 
	// job, though it might be problematic for very small coordinates:

	double euclidean_dist_origin_current =
			sqrt(current_point.getX()*current_point.getX() +
				 current_point.getY()*current_point.getY() +
				 current_point.getZ()*current_point.getZ());
	double euclidean_dist_origin_new =
			sqrt(x_new(0)*x_new(0) +
				 x_new(1)*x_new(1) +
				 x_new(2)*x_new(2));

	if(euclidean_dist_origin_new>drift_threshold*euclidean_dist_origin_current) 
		return 1;

	PigPoint point(x_new(0), x_new(1), x_new(2));
	xyz = point;

	return 0;

}

