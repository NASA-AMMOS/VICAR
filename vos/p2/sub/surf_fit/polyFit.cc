#include <cmath>
#include <Eigen/Dense>
#include "zvproto.h"
#include "polyFit.h"
#ifdef _OPENMP
#include <omp.h>
#endif



//----------------------------------------------------------------------------


template<class T1, class T2>
int computePolyFit(const T1 *xarr,
                   const T1 *yarr,
                   const T2 *zarr,
                   const int numPoints,
                   const int degree,
                   double *coefs,
                   int &numCoefs,
                   double *zfit ) 
{
   char msg[150];

   if (xarr == nullptr || yarr == nullptr || zarr == nullptr) {
      sprintf(msg, "Input x/y/z arrays cannot be null");
      zvmessage(msg, ""); 
      return 1;
   }

   // Compute the number of coefficients. Depends on the polynomial degree.
   int nc = (degree + 1) * (degree + 2 ) / 2;
   if (numPoints < nc) {
      sprintf(msg, "Not enough valid points to define the polynomial surface");
      zvmessage(msg, "");
      return 1;
   }

   // Initialize matrix for upcoming linear algebra manipulation, of
   // the form of a linear set AX = B. Looking to define X.
   
   // Define matrix A 
   Eigen::MatrixXd A(numPoints, nc);
   unsigned int c =0;
   for (int j=0; j<=degree; j++) {
      for (int i=0; i<=(degree-j); i++) {
         for(int l=0; l<numPoints; l++) { 
            A(l,c) = std::pow((double)xarr[l],i) * std::pow((double)yarr[l], j);
         }
         c++;
      }
   }

   // Define vector B (which is input zarr - Map zarr to Eigen vector B)
   // It would be nice to map the input array to an Eigen matrix to avoid hard
   // copyy (such as the commented line below) but it seems that Eigen does not 
   // do type promotion and therefore can't mix type during linear algebra. 
   // So hard copy of zarr with cast to double seems necessary..
   //Eigen::Map<const Eigen::Matrix<T2, Eigen::Dynamic, 1> > B(zarr, numPoints);
   Eigen::VectorXd B(numPoints);
   for(int i=0; i<numPoints; i++) 
      B(i) = (double)zarr[i];   

   // Allocate output polynomial coefficients array if necessary
   if (coefs == nullptr) {
      coefs = new double[nc];
   }
   else if (numCoefs < nc) {
      delete [] coefs;
      coefs = new double[nc];
   }
   else {}
   numCoefs = nc;
   
   // Map output coefs to Eigen matrix X
   Eigen::Map<Eigen::MatrixXd> X(coefs, numCoefs, 1);

   // Solve!
   //X = (A.transpose() * A).ldlt().solve(A.transpose() * B);
   X = A.colPivHouseholderQr().solve(B);


   // Save fitted elevation for input points if needed
   if (zfit != nullptr) 
      Eigen::VectorXd::Map(zfit, numPoints) = A*X;
   

   return 0;

}
   

//----------------------------------------------------------------------------


template<class T1, class T2>
int computePolyFit(const std::vector<T1> &xarr,
                   const std::vector<T1> &yarr,
                   const std::vector<T2> &zarr,
                   const int degree,
                   std::vector<double> &coefs,
                   std::vector<double> &zfit) 
{


   char msg[150];

   // Check input coherence
   const int numPoints = xarr.size();
   if (yarr.size() != numPoints || zarr.size() != numPoints) {
      sprintf(msg, "Input x/y/z arrays must have same number of elements.");
      zvmessage(msg, ""); 
      return 1;
   }

   // Compute number of polynomial coefficients given the polynomial function
   // degree
   int numCoefs = (degree + 1) * (degree + 2) / 2;

   // Check that the output container for coefficients is large enough.
   if (coefs.size() < numCoefs)
      coefs.resize(numCoefs);    

   // Check that the output container for control point elevation fit is large 
   // enough.
   if (zfit.size() < numPoints)
      zfit.resize(numPoints);    

   return computePolyFit(xarr.data(), 
                         yarr.data(), 
                         zarr.data(), 
                         numPoints,
                         degree,
                         coefs.data(),
                         numCoefs,
                         zfit.data());
 

}


//----------------------------------------------------------------------------


template<class T1, class T2>
int computePolyFit(const std::vector<T1> &xarr,
                   const std::vector<T1> &yarr,
                   const std::vector<T2> &zarr,
                   const int degree,
                   std::vector<double> &coefs) 
{


   char msg[150];

   // Check input coherence
   const int numPoints = xarr.size();
   if (yarr.size() != numPoints || zarr.size() != numPoints) {
      sprintf(msg, "Input x/y/z arrays must have same number of elements.");
      zvmessage(msg, ""); 
      return 1;
   }

   // Compute number of polynomial coefficients given the polynomial function
   // degree
   int numCoefs = (degree + 1) * (degree + 2) / 2;

   // Check that the output container for coefficients is large enough.
   if (coefs.size() < numCoefs)
      coefs.resize(numCoefs);    

   return computePolyFit(xarr.data(), 
                         yarr.data(), 
                         zarr.data(), 
                         numPoints,
                         degree,
                         coefs.data(),
                         numCoefs,
                         nullptr);
 

}


//----------------------------------------------------------------------------


template<class T1, class T2>
int applyPolyFit(const T1 *xarr,
                 const T1 *yarr,
                 const int numPoints,
                 const int degree,
                 const double *coefs,
                 const int numCoefs, 
                 T2 *zarr, 
                 const int omp) 
{
                

   char msg[150];

   if (xarr == nullptr || yarr == nullptr || zarr == nullptr) {
      sprintf(msg, "Input x/y/z arrays cannot be null");
      zvmessage(msg, ""); 
      return 1;
   }
  
    
   // Degree could be inferred from the number of coefficients, but having it as
   // a parameter allows to check coherence of inputs.
   // Compute the number of coefficients. Depends on the polynomial degree.
   int nc = (degree + 1) * (degree + 2 ) / 2;

   // Double check!
   if (numCoefs != nc) {
      sprintf(msg, "Coefficient array size and polynomial degree not coherent.");
      zvmessage(msg, "");
      return 1;
   }
   #pragma omp parallel for if (omp)
   for (int p=0; p < numPoints; p++) {
      double val = 0.0;
      unsigned int c = 0;
      for(int j=0; j<=degree; j++) {
         for(int i=0; i<=(degree-j); i++) {
            val += coefs[c] * std::pow(xarr[p],i) * std::pow(yarr[p], j); //pow cast to dble
            c++;
         }
      }
      zarr[p] = (T2)val;
   }

   return 0;

}


//----------------------------------------------------------------------------


template<class T1, class T2>
int applyPolyFit(const std::vector<T1> &xarr,
                 const std::vector<T1> &yarr,
                 const int degree,
                 const std::vector<double> &coefs,
                 std::vector<T2> &zarr,
                 const int omp) 
{

   char msg[150];

   int numPoints = xarr.size();
  
   // Check that input points X and Y are same length
   if (yarr.size() != numPoints) {
      sprintf(msg, "Input X and Y arrays must be same size.");
      zvmessage(msg, "");
      return 1;
   }

   if (zarr.size() < numPoints)    
      zarr.resize(numPoints);
   
 
   return applyPolyFit(xarr.data(),
                       yarr.data(),
                       numPoints,
                       degree,
                       coefs.data(),
                       coefs.size(),
                       zarr.data(),
                       omp);

}


//----------------------------------------------------------------------------


// Macro to instantiate templates
#define INSTANTIATE_POLYFIT(T1, T2) \
template int computePolyFit<T1,T2>(const T1 *xarr, const T1 *yarr, const T2 *zarr, const int numPoints, const int degree, double *coefs, int &numCoefs, double *zfit); \
template int computePolyFit<T1,T2>(const std::vector<T1> &xarr, const std::vector<T1> &yarr, const std::vector<T2> &zarr, const int degree, std::vector<double> &coefs, std::vector<double> &zfit); \
template int computePolyFit<T1,T2>(const std::vector<T1> &xarr, const std::vector<T1> &yarr, const std::vector<T2> &zarr, const int degree, std::vector<double> &coefs); \
template int applyPolyFit<T1,T2>(const T1 *xarr, const T1 *yarr, const int numPoints, const int degree, const double *coefs, const int numCoefs, T2 *zarr, const int omp); \
template int applyPolyFit<T1,T2>(const std::vector<T1> &xarr, const std::vector<T1> &yarr, const int degree, const std::vector<double> &coefs, std::vector<T2> &zarr, const int omp); 

// Template instantiation
// To be extended as needed
INSTANTIATE_POLYFIT(int, float)
INSTANTIATE_POLYFIT(float, float)
INSTANTIATE_POLYFIT(int, double)
INSTANTIATE_POLYFIT(double, double)


