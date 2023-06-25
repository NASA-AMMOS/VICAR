///////////////////////////////////////////////////////////////////////
// xvector_multiview.cc
//
// Routine for computing the best intersection point (triangulation) of
// multiple unit direction vectors, given these and their origins. The 
// error (miss distance) is also computed. This method accounts for all 
// direction vectors at once inside the same near system, as opposed to 
// averaging pairwise triangulations.
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

//********************************************************************
// Convert from image coordinates to xyz coordinates given a multi-view
// feature track spanning "numimages" images viewing a common point.
// *lookOrigin points to x,y,z object space position of all cameras (PigPoint)
// *lookVector points to direction cosines for each track pixel (PigVector)
// xyz = XYZ object space coordinates
// return value: 0 = OK, 1 = no solution
//**********************************************************************

int xvector_multiview(const PigPoint *lookOrigin, 
					  const PigVector *lookVector, 
					  PigPoint &xyz, 
					  double *error, 
					  const int numimages, 
					  const double epsilon, 
					  const double error_thresh, 
					  const double cond_number_thresh)
{
    PigVector los;
    float dotVal;
 
    // The following method is based on the following report:
    // https://www.crewes.org/Documents/ResearchReports/2010/CRR201032.pdf
	//
	// Consider m lines given in 3D space, with each defined by a known point 
	// and direction cosines. We denote the m lines as l1, l2, l3, .., lm, the
	// respective on-line points as
	//
	//      p1=[x01, y01, z01], p2=[x02, y02, z02], p3=[x03, y03, z03], ..., 
	//		pm=[x0m, y0m, z0m]
	//
	// and the respective direction cosines as 
	//
	//      U1=[ux1, uy1, uz1], U2=[ux2, uy2, uz2], U3=[ux3, uy3, uz3], ...,
	//		Um=[uxm, uym, uzm]. 
	//
	// Then the m lines can be represented respectively as:
	//
	//      (x-x01)/ux1 = (y-u01)/uy1 = (z-z01)uz1 = a1
	//      .....
	//      (x-x0m)/uxm = (y-u0m)/uym = (z-z0m)uzm = am
	//
	// where a1, a2, a3,…, am are parameter variables representing the
	// Euclidean lengths along l1, l2, l3, ..., lm respectively. Thus, the 
	// m-line linear system can be expanded in the following way: 
	//
	//		Gm = d
	//
	// where the m vector is (x,y,z,a1,a2..am)^T, and G denotes an (m*3) by 
	// (m+3) matrix. 
	//

	// Solve for the x,y,z point nearest to all rays:

	MatrixXf G = MatrixXf::Zero(3*numimages,3+numimages);
	VectorXf d = VectorXf::Zero(3*numimages);

	// Fill-in G with 1's where needed:

	for (int i=0; i<numimages; i+=1) {
		G(3*i,0) = 1.0; 
		G(3*i+1,1) = 1.0;
		G(3*i+2,2) = 1.0;
		G(3*i,3+i) = -lookVector[i].getX();
		G(3*i+1,3+i) = -lookVector[i].getY();
		G(3*i+2,3+i) = -lookVector[i].getZ();
		d(3*i) = lookOrigin[i].getX();
		d(3*i+1) = lookOrigin[i].getY();
		d(3*i+2) = lookOrigin[i].getZ();
	}
	
	// Solve the system via SVD (note that it's more robust than linear
	// regression!). SVD should yield a result even if G is rank-deficient,
	// which should only happen if any set of rays is parallel or nearly
	// parallel.
	//
	// NOTE: In the two-view case, near-parallel rays matter, but here not 
	// as much because there are more constraints, so this is not explicitly
	// taken into account.

	// Compute the rank of G:

	VectorXf m(3+numimages);
	JacobiSVD<MatrixXf> svd(G, ComputeThinU | ComputeThinV);
	int rank = svd.rank();

	// Compute the condition number of G:

	double cond = svd.singularValues()(0) / 
		   svd.singularValues()(svd.singularValues().size()-1);

	if(rank == 3+numimages) {
		m = svd.solve(d);
		PigPoint point(m(0), m(1), m(2));
	    xyz = point;
	}
	else {

		// Matrix not full rank! Return since rank-deficient SVD can fail
		// Note: running the SVD anyway will result in this error:
		// "Assertion `computeU() && "This JacobiSVD decomposition didn't
		// compute U. Did you ask for it?"' failed"

		return 1;
	}

	// Next, compute the error for the computed point. This is simply the 
	// average of the difference between the point and the closest point 
	// on each ray, obtained by plugging in x + a*y into each vector:

	int count = 0;
	for (int j=0; j<(numimages-1); j++) {
		PigVector errvect = xyz - (lookOrigin[j]+lookVector[j]*m(3+j));
		PigPoint err(errvect.getX(), errvect.getY(), errvect.getZ());
		*error += err.magnitude();
		count++;
	}
	*error /= count;

	// Check if the triangulated point is in front of ALL cameras
	// ("cheirality"). If erroneous tiepoints, XYZ could end up behind cam:

	for (int j=0; j<(numimages-1); j++) {

		los = xyz - lookOrigin[j];
		los.normalize();

		dotVal = los.getX() * lookVector[j].getX() +
			     los.getY() * lookVector[j].getY() +
		         los.getZ() * lookVector[j].getZ();

		if (dotVal < -epsilon) {

			// Point not in front of the camera, skip!

			return 1;
		}
	}

	// Further quality checks:
	//
	// 1) Compute the Euclidean distance of the point from the origin. If 
	// the error is some percentage of that or higher, discard the point,
	// otherwise keep it
	//
	// 2) If the condition number of the data matrix is too high, the problem
	// is ill-posed and the result obtained not stable. The condition 
	// number is indicative of how much the error is amplified in the output
  
	double euclidean_dist_origin = sqrt(xyz.getX()*xyz.getX() +
										xyz.getY()*xyz.getY() +
										xyz.getZ()*xyz.getZ());

	if(*error > euclidean_dist_origin*error_thresh/100.0 || 
		cond > cond_number_thresh) {

		// Point failed quality check; error greater than some percent of 
		// the point's Euclidean distance to the origin, or the condition
		// number is too high

	    return 1;  
	}
	 
  return 0;

}

