///////////////////////////////////////////////////////////////////////////////
// Radial Basis Function interpolation
// The routines in this file define (1) a Radial Basis Function (RBF) 
// interpolant from a list of control points and (2) compute the interpolated
// value at any arbitraty locations using that interpolant.
//
// The routines in this file are based on the approach described in:
// "Approximation Methods for Thin Plate Spline Mappings and Principal Warps"
// Gianluca Donato and Serge Belongie - ECCV 2002 pp 21-31
// Although that paper refers to Thin Plate Spline interpolant specifically, 
// the approach can be applied to other function as implemented here.
//
// The core principle of a RBF interpolant is to define the "surface" value (Z)
// at a given 2D location (X,Y), solely based on the distances between that 
// location and a set of other locations (controls points). 
// It is of the form:
// Z = f(X,Y) = a1 + a2*X + a3*Y + SUM(W_i * RBF(distance((X,Y),(Xc_i,Yc_i)))
// with:
// (X,Y): planar location of the point
// (Xc_i,Yc_i): planar location of the i-th control point
// SUM: sum over all the control points
// W_i: scalar weight of the i-th control point.
// RBF: Radial basis function (e.g., linear, thin plate, cubic,...)
//
// Note that the implementation in this file is for 3D situations (X,Y: planar 
// location, Z: "elevation" of the surface), but the method is applicable to N 
// dimensions.
// 
///////////////////////////////////////////////////////////////////////////////

#ifndef RBFFIT_H
#define RBFFIT_H

#include <vector>
#include <functional>


// Radial Basis Functions
typedef enum {
   LINEAR,             // return: distance
   CUBIC,              // return: distance^3
   QUINTIC,            // return: distance^5
   THIN_PLATE          // return: distance^2 * log(distance)
} RBFMode;



// Compute a RBF interpolant from a set of controls points (X,Y,Z). 
// INPUT:
// numNodes: Number of control points (X,Y,Z) to define the interpolant from.
// X,Y,Z: Arrays of control points coordinates. Must contain numNodes elements.
// regularization: Relaxes the exact interpolation to an approximation in case
// the input data is noisy.
// mode: RBFMode element. Identify which RBF function is used.
// OUTPUT:
// returned value: 0 for success, non-0 otherwise.
// coefs: Allocated array that will contain the RBF coefficients (W_i in the 
// description above). The array must contain at least (numNodes+3) elements.
//
// Note on decomposition:
// The definition of the interpolant use Eigen decomposition HouseholderQR. 
// There are a variety of decompositions available in Eigen, each with their
// tradeoff between speed, accuracy, and stability. See Eigen library doc for
// more information. If HouseholderQR does not satisfy the need, other 
// decomposition could be added in the future.
//
// Note on regularization parameter:
// This parameter acts as a "knob" on the amount of smoothness that the output
// surface function will have. If set to 0, there's no regularization, the 
// output is exact interpolation (the surface function will pass exactly through
// the control points). If set to infinity (conceptually), the surface function 
// reduces to a plane. In between, the output surface will be more or less 
// smooth. Setting the right regularization is case dependent, and requires 
// trial-and-error approach. A good starting point is to set regularization 
// between 0.1 and 1.
template<class T1, class T2>
int computeRBF(const int numNodes,              // input
               const T1 *X,                     // input
               const T1 *Y,                     // input
               const T2 *Z,                     // input
               const float regularization,      // input
               const RBFMode mode,              // input
               double *coefs);                  // output

// Overload of the above using std::vectors
template<class T1, class T2>
int computeRBF(const std::vector<T1> &X,        // input
               const std::vector<T1> &Y,        // input
               const std::vector<T2> &Z,        // input
               const float regularization,      // input
               const RBFMode mode,              // input
               std::vector<double> &coefs);     // output



// Apply a RBF interpolant to a set of (X,Y) locations to compute the 
// "elevation" (Z) component of the surface at these locations.
// INPUT:
// numPoints: Number of points (X,Y) to compute the elevation of.
// X,Y: Arrays of points coordinates. Must contain numPoints elements. 
// numNodes: Number of control points used to defined the RBF coefficients 
// Xn,Yn: Arrays of control points used to define the RBF. Must contain 
//        numNodes elements.
// coefs: Array containing the RBF coefficients as computed from "computeRBF"
//        function. Must contains "numNodes + 3" elements.
// mode: RBFMode element. Identify which RBF function is used. Must match the
//       mode used in "computeRBF" call.
// omp: use of OMP? 1: yes, 0: no.
// OUTPUT:
// returned value: 0 for success, non-0 otherwise.
// out: Allocated array that will contain the rbf surface "elevation" at the
//      input points locations (X,Y). Must contain at least numPoints elements.
template<class T1, class T2>
int applyRBF(const int numPoints,    // input
             const T1 *X,            // input
             const T1 *Y,            // input 
             const int numNodes,     // input 
             const T2 *Xn,           // input
             const T2 *Yn,           // input
             const double *coefs,    // input
             const RBFMode mode,     // input
             const int omp,          // input
             double * out);          // output 


// Overload of the function above with std::vectors
template<class T1, class T2>
int applyRBF(const std::vector<T1> &X,             // input
             const std::vector<T1> &Y,             // input
             const std::vector<T2> &Xn,            // input
             const std::vector<T2> &Yn,            // input
             const std::vector<double> &coefs,     // input
             const RBFMode mode,                   // input
             const int omp,                        // input
             std::vector<double> &out);            // output






#endif

