
#include "rbfFit.h"
#include <Eigen/Dense>
#include <Eigen/SVD>
#include "zvproto.h"
#include <cmath>

#ifdef _OPENMP
#include <omp.h>
#endif

//-----------------------------------------------------------------------------

std::function<double(double, double, double, double)> getRBFfunction(const RBFMode mode) {

   switch (mode) {
      // linear: function = radius
      case LINEAR: return [](double x, double xc, double y, double yc) {
                          return std::sqrt((x-xc)*(x-xc)+(y-yc)*(y-yc));};
      // cubic: function = radius^3
      case CUBIC: return [](double x, double xc, double y, double yc) {
                         return std::pow((x-xc)*(x-xc)+(y-yc)*(y-yc),3.0/2.0);};
      // quintic: function = radius^5
      case QUINTIC: return [](double x, double xc, double y, double yc) {
                           return std::pow((x-xc)*(x-xc)+(y-yc)*(y-yc), 5.0/2.0);};
      // thin plate: functio = radius^2 * log(radius)
      case THIN_PLATE: return [](double x, double xc, double y, double yc) {
                              double r2 = (x-xc)*(x-xc)+(y-yc)*(y-yc);
                              return (r2 == 0) ? 0.0 : r2 * std::log(std::sqrt(r2));};
      default: {
                  zvmessage("Unknown Radial Basis Function mode. Defaulting to LINEAR","");
                  return [](double x, double xc, double y, double yc) {
                         return std::sqrt((x-xc)*(x-xc)+(y-yc)*(y-yc));};
               };
   }
}


//-----------------------------------------------------------------------------


template<class T1, class T2>
int computeRBF(const std::vector<T1> &X,
               const std::vector<T1> &Y,
               const std::vector<T2> &Z,
               const float regularization,
               const RBFMode mode,
               std::vector<double> &coefs) 
{

   // Some sanity checks
   if (X.size() == 0 || X.size() != Y.size() || X.size() != Z.size()) {
      zvmessage("X,Y,Z vectors cannot be empty and must have same size", "");
      return 1;
   }

   // Check that coefs is large enough to receive the rbf coefficients
   if (coefs.size() < X.size() + 3)
      coefs.resize(X.size() + 3);

   // Compute rbf
   return computeRBF(X.size(), X.data(), Y.data(), Z.data(), regularization,
                     mode, coefs.data());

}


//-----------------------------------------------------------------------------


template<class T1, class T2>
int computeRBF(const int numNodes, 
               const T1 *X,
               const T1 *Y, 
               const T2 *Z, 
               const float regularization,
               const RBFMode mode,
               double *coefs) 
{

   // Some sanity checks
   if (X == nullptr || Y == nullptr || Z == nullptr || coefs == nullptr) {
      zvmessage("X,Y,Z,coefs cannot be null arrays.", "");
      return 1;
   }

   // Get the RBF to use
   auto rbfFunction = getRBFfunction(mode);


   // AW = B ---> find W (rbf coefficients) using least square (e.g, 
   // W = inverse(At A) (At B)  ).
   // A = |K  P| with (row,col) P:numNodesx3, K:numNodesxnumNodes, O:3x3, 
   //     |Pt O|                Pt:3xnumNodes
   Eigen::MatrixXd A = Eigen::MatrixXd::Zero(numNodes + 3, numNodes + 3);
   Eigen::VectorXd B = Eigen::VectorXd::Zero(numNodes + 3);

   // Populate O
   // O is a 3x3  0-filled - nothing to do

   // Populate K
   // K is symetric, so only compute one element (top right triangle) and copy 
   // it to corresponding lower left triangle. Also, use iteration over points 
   // to cumulate the scale-independant factor (s) for the regularization.
   double s = 0;
   for (int row=0; row<numNodes; row++) {
      for (int col=row+1; col<numNodes; col++) {
         double v = rbfFunction(X[col], X[row], Y[col], Y[row]);
         A(row, col) = v;
         A(col, row) = v;
         s += 2 * std::sqrt(std::pow(X[col]-X[row],2) + std::pow(Y[col]-Y[row],2));
      }
   }

   // Populate K diagonal (which is 0 for now) with the regularization term
   s /= std::pow(numNodes,2);
   s = regularization * s * s;
   for (int p=0; p<numNodes; p++)
      A(p,p) = s;

   // Populate P and Pt
   for (int p=0; p<numNodes; p++) {
      // P
      A(p,numNodes)   = 1;
      A(p,numNodes+1) = X[p];
      A(p,numNodes+2) = Y[p];
      // Pt
      A(numNodes,p)   = 1;
      A(numNodes+1,p) = X[p];
      A(numNodes+2,p) = Y[p];
   }


   // Populate B
   // The last 3 terms of B are 0 (condition for square integrable second 
   // derivatives). Would be nice to map (instead of deep copy) B to Z, but
   // Eigen doesn't do type promotion. B needs to be double type.
   for (int p=0; p<numNodes; p++)
      B(p) = (double)Z[p];

   // Map output coef array to Eigen vector
   Eigen::Map<Eigen::MatrixXd> W(coefs, numNodes+3, 1);

   // Apply Least Square
   // Note: depending on the matrix size to invert, there are various algorithms
   // that could be used with various tradeoff between speed/accuracy. See Eigen
   // doc for more details.
   //W = A.ldlt().solve(B);
   //W = A.colPivHouseholderQr().solve(B);
   W = A.householderQr().solve(B);

   return 0;


}


//-----------------------------------------------------------------------------


template<class T1, class T2>
int applyRBF(const std::vector<T1> &X,           // input
             const std::vector<T1> &Y,           // input
             const std::vector<T2> &Xn,          // input
             const std::vector<T2> &Yn,          // input
             const std::vector<double> &coefs,   // input
             const RBFMode mode,                 // input
             const int omp,                      // input
             std::vector<double> &out)           // output
{


   // Some sanity checks
   if (X.size() != Y.size()) {
      zvmessage("X,Y vectors must have same size", "");
      return 1;
   }

   if (Xn.size() != Yn.size() || Xn.size() != (coefs.size()-3)) {
      zvmessage("Xn,Yn,coefs vectors don't have ageeable size", "");
      return 1;
   }

   // Check that output vector is large enough
   if (out.size() < X.size())
      out.resize(X.size());

   // Apply rbf
   return applyRBF(X.size(), X.data(), Y.data(), Xn.size(), Xn.data(), 
                   Yn.data(), coefs.data(), mode, omp, out.data());

} 


//-----------------------------------------------------------------------------


template<class T1, class T2>
int applyRBF(const int numPoints,     // input
             const T1 *X,             // input
             const T1 *Y,             // input 
             const int numNodes,      // input 
             const T2 *Xn,            // input
             const T2 *Yn,            // input
             const double *coefs,     // input
             const RBFMode mode,      // input
             const int omp,           // input
             double * out)            // output 
{
   int msgLen = 150;
   char msg[msgLen];

   // Some sanity checks
   if (X == nullptr || Y == nullptr || Xn == nullptr || Yn == nullptr || 
       coefs == nullptr || out == nullptr) {
      zvmessage("X,Y,Xn,Yn,coefs,out cannot be null arrays", "");
      return 1;
   }

   // Get the RBF function to use
   auto rbfFunction = getRBFfunction(mode);

   // Compute interpolated value for each point
   #pragma omp parallel for if (omp)
   for (int point=0; point<numPoints; point++) { 
      out[point] = coefs[numNodes] + coefs[numNodes+1] * X[point] + coefs[numNodes+2] * Y[point];
      for (int node=0; node<numNodes; node++) { 
         out[point] += coefs[node] * rbfFunction(X[point], Xn[node], Y[point], Yn[node]);
      }
   }
   
   return 0;

}


//-----------------------------------------------------------------------------


// Macro to instanciate templates
#define INSTANCIATE_COMPUTERBF(T1, T2) \
template int computeRBF<T1,T2>(const std::vector<T1> &X, const std::vector<T1> &Y, const std::vector<T2> &Z, const float regularization, const RBFMode mode, std::vector<double> &coefs);  
#define INSTANCIATE_APPLYRBF(T1, T2) \
template int applyRBF<T1,T2>(const std::vector<T1> &X, const std::vector<T1> &Y, const std::vector<T2> &Xn, const std::vector<T2> &Yn, const std::vector<double> &coefs, const RBFMode mode, const int omp, std::vector<double> &out);  


// Template Instanciation
// To be extented as needed
INSTANCIATE_COMPUTERBF(int, float)
INSTANCIATE_COMPUTERBF(int, double)
INSTANCIATE_COMPUTERBF(float, float)
INSTANCIATE_COMPUTERBF(double, double)
INSTANCIATE_APPLYRBF(int, int)
INSTANCIATE_APPLYRBF(float, int)
INSTANCIATE_APPLYRBF(double, int)
INSTANCIATE_APPLYRBF(float, float)
INSTANCIATE_APPLYRBF(double, double)
