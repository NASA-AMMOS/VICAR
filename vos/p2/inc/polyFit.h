///////////////////////////////////////////////////////////////////////////////
// 3D polynomial surface fit
// The routines in this file define an polynomial surface from a list of control
// points and compute the corresponding surface elevation at any arbitrary 
// locations.
// The polynomial surface is defined from the fit (in a least square sense) of 
// the control points to a polynomial function of a given degree. 
// There are two kind of functions:
// - computePolyFit: This function (and its overloads) defines the polynomial 
//   surface fit from a list of control points. It returns the polynomial 
//   surface coefficients and (optionally) the fitted surface elevation at the 
//   control point locations.
// - applyPolyFit: This function (and its overloads) compute the polynomial 
//   surface elevation at arbitraty locations using the coefficients returned
//   by the polynomial surface fit routine (computePolyFit).
//
// The polynomial surface fit uses a function of this form:
//    Z = f(X,Y) =  k0 + k1*X + k2*X^2 + ... +km*Y + kn*XY + ko*X^2Y +...
// with f(X,Y) being the surface elevation value (Z) of the points and (X, Y) 
// the planar coordinates of the points.
// The degree of the polynomial function is defined by the caller and represents
// the  maximum degree of X and Y combined:
// DEGREE=0: f(X,Y) = k0    i.e., an offset 
// DEGREE=1: f(X,Y) = k0 + k1*X + k2*Y    i.e., a plane
// DEGREE=2: f(X,Y) = k0 + k1*X + k2*X^2 + k3*Y + k4*X*Y + k5*Y^2   i.e., a 2nd
//           order polynomial
// DEGREE=3: f(X,Y) = k0 + k1*X + k2*X^2 + k3*X^3 + k4*Y + k5*X*Y + k6*X^2*Y
//                    + k7*Y^2 + k8*X*Y^2 + k9*Y^3  i.e., a 3rd order polynomial
// etc...
//
// Implementation note: Instantiations for these templated functions are at the 
// end of the .cpp. If a data type is missing for your need, please add them in
// the cpp.
///////////////////////////////////////////////////////////////////////////////

#ifndef POLYFIT_H
#define POLYFIT_H

#include <vector>


//----------------------------------------------------------------------------


// Compute the polynomial surface from an input list of control points.
// INPUTS:
// - xarr, yarr: arrays containing the contol points planar coordinates.
// - zarr: array containing the "elevation" at the control points locations.. 
// - numPoints: Number of control points. xarr, yarr, zarr must have the same
//   number of elements.
// - degree: Degree of the poynomial surface to fit the control points.
// OUTPUTS:
// - returned value: 0 if successfull, not-0 otherwise.
// - coefs: array containing the coefficients of the polynomial surface fit. The
//   coefficients are ordered with X variable growing the fastest. The number of
//   coefficients solely depends on the degree of the polynomial surface and 
//   is given by: number of coefficients = (degree+1)*(degree+2)/2.
//   coefs can be a nullpointer in which case the function will allocate it, or
//   it can be an allocated array whose size is given by numCoefs input variable.
//   If the size of the coefs array is inferior to the number of coefficients, 
//   if will be deallocated and reallocated with the proper size. 
// - numCoefs: as input it is the size of the input coefs array and is updated
//   by the routine to contain the number of coefficients stored in coefs array.
// - zfit: if not a nullptr, must be an allocated array of at least numPoints 
//   elements. It will be filled with the surface fit at the xarr/yarr locations.
//
// Example for a polynomial suface fit of degree 3:
// xarr, yarr, zarr: arrays of N elements (i.e., N control points).
//
// Case where output 'coefs' array is pre-allocated:
// double * coefs = new double[10]; numCoefs = (3+1)*(3+2)/2 = 10
// int status = computePolyFit(xarr, yarr, zarr, N, 3, coefs, 10, nullptr);
//
// Case where coefs is not pre-allocated
// double * coefs = nullptr;
// int status = computePolyFit(xarr, yarr, zarr, N, 3, coefs, 0, nullptr);

template<class T1, class T2>
int computePolyFit(const T1 *xarr,          // input
                   const T1 *yarr,          // input
                   const T2 *zarr,          // input
                   const int numPoints,     // input
                   const int degree,        // input
                   double *coefs,           // output
                   int &numCoefs,           // input/output
                   double *zfit );          // output


//----------------------------------------------------------------------------


// Convenience function overload 
template<class T1, class T2>
int computePolyFit(const std::vector<T1> &xarr,   // input
                   const std::vector<T1> &yarr,   // input
                   const std::vector<T2> &zarr,   // input
                   const int degree,              // input
                   std::vector<double> &coefs,    // output
                   std::vector<double> &zfit);    // output


//----------------------------------------------------------------------------


// Convenience function overload 
template<class T1, class T2>
int computePolyFit(const std::vector<T1> &xarr,   // input
                   const std::vector<T1> &yarr,   // input
                   const std::vector<T2> &zarr,   // input
                   const int degree,              // input
                   std::vector<double> &coefs);   // output


//----------------------------------------------------------------------------


// Compute the surface elevation of a list of input points (x,y) from the 
// coefficients of the polynomial surface function..
// INPUT:
// - xarr,yarr are the planar location of the points for which we want to 
//   compute the polynomial surface fit "elevation"
// - numPoints: Number of points.
// - degree: Degree of the polynomial fit.
// - coefs: coefficients of the polynomial fit, as obtained from computePolyFit.
// - numCoefs: Number of coefficients.
// OUTPUT:
// - returned value: 0 for success, non-0 otherwise.
// - zarr: "Elevation" of the surface at the input points locations (xarr, yarr).
//         Array must be allocated with at least numPoints elements. 

template<class T1, class T2>
int applyPolyFit(const T1 *xarr,       // input
                 const T1 *yarr,       // input
                 const int numPoints,  // input 
                 const int degree,     // input
                 const double *coefs,  // input
                 const int numCoefs,   // input
                 T2 *zarr,             // output 
                 const int omp);       // input  


//----------------------------------------------------------------------------


// Convenience function overload
// If output array zarr is undersided, it will be resized by function.
template<class T1, class T2>
int applyPolyFit(const std::vector<T1> &xarr,      // input
                 const std::vector<T1> &yarr,      // input
                 const int degree,                 // input
                 const std::vector<double> &coefs, // input
                 std::vector<T2> &zarr,            // output
                 const int omp);                   // input


#endif

