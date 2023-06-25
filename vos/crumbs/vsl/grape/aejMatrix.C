// aejMatrix.C 1.2 02/07/10 15:02:50
/** \file
 + AUTHOR:  Andrew E. Johnson (aej@ri.cmu.edu)
 + DATE:    4-Feb-98
 + PURPOSE: aejMatrix library functions.
 **/

#include "grape/aej.h"
#include "grape/aejError.h"
#include "grape/aejMatrix.h"
#include <iomanip>

using namespace std;

/// Copies the double values from a double* to the data storage of an aejVector
void aejVector::setData(double *newData, int start, int size) {

  int r;

  if ((start+size>SIZE)||(size<1)||(start<0)) 
    HandleError("aejVector: Data size does not fit Vector.");

  for (r=start;r<start+size;r++) 
    DATA[r] = newData[(r-start)];
  
}

/// clears the data in the aejVector by assigning a constant double value to it
void aejVector::clearData(double fillVal) {

  int r;

  for (r=0;r<SIZE;r++) 
      DATA[r] = fillVal;
  
}

/// Create aejVectors from varing inputs

/// constructer that sets size of Vector to nrxnc
aejVector::aejVector(int size) {
  if (size<1) HandleError("Trying to create NULL aejVector.");
  SIZE = size;
  DATA = new double[SIZE];
  clearData(0);
}

/// constructor that copies an aejVector
aejVector::aejVector(const aejVector& v) {
  SIZE = v.SIZE;
  DATA = new double[SIZE];
  setData(v.DATA,0,SIZE);
}

/// constructor that creates a 2-vector
aejVector::aejVector(double x ,double y) {
  SIZE = 2;
  DATA = new double[SIZE];
  DATA[0] = x; DATA[1] = y;
}

/// constructor that creates a 3-vector
aejVector::aejVector(double x ,double y, double z) {
  SIZE = 3;
  DATA = new double[SIZE];
  DATA[0] = x; DATA[1] = y; DATA[2] = z;
}

/// constructor that creates a 4-vector
aejVector::aejVector(double w, double x ,double y, double z) {
  SIZE = 4;
  DATA = new double[SIZE];
  DATA[0] = w; DATA[1] = x; DATA[2] = y; DATA[3] = z;
}

/// assignment operator
aejVector aejVector::operator=(const aejVector& v) {
  SIZE = v.size();
  delete DATA;
  DATA = new double[SIZE];
  setData(v.DATA,0,SIZE);
  return *this;
}

/// changes the memory allocated for a vector and clears the data
void aejVector::resize(int n) {
  
  if (n<1)
    HandleError("aejVector: Trying to create NULL aejvector.");
  
  delete DATA;
  SIZE = n;
  DATA = new double[n];
  clearData(0);
}

/// Copies data from one vector into another without changing the dimensions
void aejVector::copyData(const aejVector& v)

{
  int ms = MIN(SIZE,v.size()); // copy data 
  for(int i=0;i<ms;i++) DATA[i] = v[i];
}

/// aejVector coordinate interpolated access function
double aejVector::interpolate(double r) const {
  if ((r<0)||(r>SIZE-1))
    HandleError("aejVector: Coordinate interpolated access out of bounds.");
  int R = (int)r;
  return (r-R)*DATA[R+1]+(1-(r-R))*DATA[R];
}

/// Dot product of two vectors of the same dimension 
double aejVector::operator*(aejVector V) const {
  
  int i;
  
  if (SIZE!=V.SIZE)
    HandleError("aejVector: Vectors have different dimensions. Cannot multiply.");

  double result = 0;
  for (i=0;i<SIZE;i++) result+=DATA[i]*V[i];
  return result;

}

/// Scales a vector by a double 
aejVector aejVector::operator*(double scale) const {
  
  int i;
  aejVector Vnew(SIZE);
  for (i=0;i<SIZE;i++) Vnew[i]=DATA[i]*scale;
  return Vnew;
  
}

/// vector addition member function
aejVector aejVector::operator+(aejVector v) const {
  
  int i;
  if (SIZE!=v.size()) 
    HandleError("aejVector operator+: Vectors have different dimension.");

  aejVector Vnew(SIZE);
  for (i=0;i<SIZE;i++) Vnew[i]=DATA[i]+v[i];
  return Vnew;
  
}

/// vector subtraction member function
aejVector aejVector::operator-(aejVector v) const {
  
  int i;
  if (SIZE!=v.size()) 
    HandleError("aejVector operator-: Vectors have different dimension.");

  aejVector Vnew(SIZE);
  for (i=0;i<SIZE;i++) Vnew[i]=DATA[i]-v[i];
  return Vnew;
  
}

/// vector equality 
int aejVector::operator==(aejVector v) const {
  
  int i;
  if (SIZE!=v.size()) 
    HandleError("aejVector operator==: Vectors have different dimension.");

  for (i=0;i<SIZE;i++) if (v[i]!=DATA[i]) return 0;
  return 1;
  
}

/// length of vector
double aejVector::length() const {
  
  int i;
  double l = 0;
  for (i=0;i<SIZE;i++) l+= SQR(DATA[i]);
  l = sqrt(l);
  return l;
  
}

/// length of vector squared (more efficient for some checks)
double aejVector::lengthSqr() const {
  
  int i;
  double l = 0;
  for (i=0;i<SIZE;i++) l+= SQR(DATA[i]);
  return l;
  
}

/// Sort the coordinates of a vector in place.
int compareDouble( const void *d1, const void *d2)
{
  if (*(double *)d1>*(double *)d2) return 1;
  if (*(double *)d1<*(double *)d2) return -1;
  return 0;

}

void aejVector::sort() {

  qsort(DATA,SIZE,sizeof(double),compareDouble);
  
}

/// Compute mean and standard deviation of mean of vector
void aejVector::stats(double *mean, double *sig) {
  int i;
  double sum = 0;
  double sumSq = 0;
  for(i=0;i<SIZE;i++) {
    sum += DATA[i];
    sumSq += SQR(DATA[i]);
  }
  
  *mean = sum/SIZE;
  *sig = sqrt(sumSq/SIZE-SQR(*mean));

}

/// read aejVectors from stream
istream& operator>>(istream& I, aejVector& v) {
  for(int i=0;i<v.SIZE;i++) I >> v[i];
  return I;
}

/// write aejVectors to streams
ostream& operator<<(ostream& O, const aejVector& v) {
  for(int i=0;i<v.SIZE;i++) O << v[i] << " ";
  return O;
}

/// Copies the double values from a double* to the data storage of an aejMatrix
void aejMatrix::setData(double *newData, int rs, int cs, int nr, int nc) {

  int r,c;

  if ((rs+nr>NR)||(cs+nc>NC)||(nr<1)||(nc<1)||(rs<0)||(cs<0)) 
    HandleError("aejMatrix: Data size does not fit Matrix.");

  for (r=rs;r<rs+nr;r++) 
    for (c=cs;c<cs+nc;c++) 
      DATA[r*NC+c] = newData[(r-rs)*NC+(c-cs)];
  
}

/// clears the data in the aejMatrix by assigning a constant double value to it
void aejMatrix::clearData(double fillVal) {

  int r,c;

  for (r=0;r<NR;r++) 
    for (c=0;c<NC;c++) 
      DATA[r*NC+c] = fillVal;
  
}

/// Create aejMatrixs from varing inputs

/// constructer that sets size of Matrix to nrxnc
aejMatrix::aejMatrix(int nr, int nc) {
  if ((nr<1)||(nc<1)) HandleError("Trying to create NULL aejMatrix.");
  NR = nr;
  NC = nc;
  DATA = new double[NR*NC];
  clearData(0);
}

/// constructer that creates nxn identity matrix
aejMatrix::aejMatrix(int n) {
  if (n<1) HandleError("Trying to create NULL aejMatrix.");
  NR = n;
  NC = n;
  DATA = new double[n*n];
  clearData(0);
  for(int i=0;i<n;i++) DATA[i*NC+i] = 1;
}

/// constructor that copies an aejMatrix
aejMatrix::aejMatrix(const aejMatrix& mat) {
  NR = mat.NR;
  NC = mat.NC;
  DATA = new double[NR*NC];
  setData(mat.DATA,0,0,NR,NC);
}

/// assignment operator
aejMatrix aejMatrix::operator=(const aejMatrix& mat) {
  NR = mat.NR; 
  NC = mat.NC;
  delete DATA;
  DATA = new double[NR*NC];
  setData(mat.DATA,0,0,NR,NC);
  return *this;
}

/// Changes size of matrix and clears data 
void aejMatrix::resize(int nr, int nc) {

  if ((nr<1)||(nc<1)) 
    HandleError("aejMatrix: Trying to create NULL aejMatrix.");
  
  delete DATA;
  NR = nr;
  NC = nc;
  DATA = new double[nr*nc];
  clearData(0);
  
}

/// copies matrix data without changing dimensions of matrices
void aejMatrix::copyData(const aejMatrix& m) {
  
  int mr = MIN(NR,m.numRows()); // copy data
  int mc = MIN(NC,m.numCols());
  for(int i=0;i<mr;i++) 
    for(int j=0;j<mc;j++) 
      DATA[i*NC+j] = m(i,j);
  
}

/// aejMatrix pixel interpolation access function
double aejMatrix::interpolate(double r,double c) const {
  if ((r<0)||(r>NR-1)||(c<0)||(c>NC-1))
    HandleError("aejMatrix: Pixel interpolated access out of Matrix bounds.");
  
  int R = (int)r;
  int C = (int)c;
  
  return (1-(r-R))*(1-(c-C))*DATA[R*NC+C]+
    (r-R)*(1-(c-C))*DATA[(R+1)*NC+C]+
    (1-(r-R))*(c-C)*DATA[R*NC+C+1]+
    (r-R)*(c-C)*DATA[(R+1)*NC+C+1];
}

/// multiply two matrices checking to see if they have the same dimension
aejMatrix aejMatrix::operator*(aejMatrix M) const {
  
  int r,c,d;

  if (NC!=M.NR) {
    cout << NR << " " << NC << " " << M.NR << " " << M.NC << endl;
    HandleError("aejMatrix: Matrices have different dimensions. Cannot multiply.");
  }
  aejMatrix Mnew(NR,M.NC);

  for (r=0;r<NR;r++) // loop over elements of rows of first matrix
      for (c=0;c<M.NC;c++) { // loop over columns of second matrix
      Mnew(r,c) = 0;
      for (d=0;d<NC;d++)  // loop over columns of first matrix 
	Mnew(r,c) += DATA[r*NC+d]*M(d,c); 
      }

  return Mnew;

}

/// multiply a vector times a matrix if they have the same dimension.
aejVector aejMatrix::operator*(aejVector V) const {
  
  int r,d;

  if (NC!=V.size()) {
    cout << V.size() << " " << NR << " " << NC << endl;
      HandleError("aejMatrix: Matrices and vector have unmatched dimensions. Cannot multiply.");
  }
  aejVector Vnew(NR);

  for (r=0;r<NR;r++) {// loop over elements of rows of first matrix
    Vnew[r] = 0;
    for (d=0;d<NC;d++)  // loop over columns of first matrix 
      Vnew[r] += DATA[r*NC+d]*V[d];
  }
  
  return Vnew;

}

/// scales a matrix by a double 
aejMatrix aejMatrix::operator*(double scale) const {
  
  int r,c;

  aejMatrix Mnew(NR,NC);

  for (r=0;r<NR;r++) // loop over of rows of matrix
    for (c=0;c<NC;c++) // loop over columns of matrix
      Mnew(r,c) = DATA[r*NC+c]*scale;

  return Mnew;

}

/// matrix addition
aejMatrix aejMatrix::operator+(aejMatrix m) const {
  
  int r,c;
  if ((NR!=m.numRows())||(NC!=m.numCols())) {
    cout << NR << " " << NC << " " << m.NR << " " << m.NC << endl;
    HandleError("aejMatrix operator+: Matrices have different dimensiona.");
  }
  aejMatrix Mnew(NR,NC);
  
  for (r=0;r<NR;r++) // loop over of rows of matrix
    for (c=0;c<NC;c++) // loop over columns of matrix
      Mnew(r,c) = DATA[r*NC+c]+m(r,c);

  return Mnew;

}

/// matrix subtraction
aejMatrix aejMatrix::operator-(aejMatrix m) const {
  
  int r,c;
  if ((NR!=m.numRows())||(NC!=m.numCols())) 
    HandleError("aejMatrix operator-: Matrices have different dimensiona.");

  aejMatrix Mnew(NR,NC);
  
  for (r=0;r<NR;r++) // loop over of rows of matrix
    for (c=0;c<NC;c++) // loop over columns of matrix
      Mnew(r,c) = DATA[r*NC+c]-m(r,c);

  return Mnew;

}

/// Computes transpose of Matrix
aejMatrix aejMatrix::transpose() const {
  
  int r,c;
  
  aejMatrix Mt(NC,NR);
  
  for (r=0;r<NR;r++) 
    for (c=0;c<NC;c++) 
      Mt(c,r) = DATA[r*NC+c];
  
  return Mt;
  
}

/// Computes Frobenius norm of matrix - sum of squares of elements.
double aejMatrix::frobenius() const {
  
  int r,c;
  double sum = 0;
  for (r=0;r<NR;r++) 
    for (c=0;c<NC;c++) 
      sum += SQR(DATA[r*NC+c]);
  
  return sqrt(sum);
  
  
}

/// Computes matrix trace 
double aejMatrix::trace() const {
  
  if (NR!=NC) HandleError("aejMatrix::trace: matrix not square.");
  
  int r;
  double sum = 0;
  for (r=0;r<NR;r++) 
    sum += DATA[r*NC+r];
  
  return sum;
  
}

/// Extracts a row from matrix and returns it as an aejVector
aejVector aejMatrix::getRow(int r) const {
  
  aejVector v(NC);
  int c;
  
  for (c=0;c<NC;c++) 
    v[c] = DATA[r*NC+c];
  
  return v;
  
}

/// Extracts a column from matrix and returns it as an aejVector
aejVector aejMatrix::getCol(int c) const {
  
  aejVector v(NR);
  int r;

  for (r=0;r<NR;r++) 
    v[r] = DATA[r*NC+c];
  
  return v;
  
}

/// Sets a matrix from from an aejVector
void aejMatrix::setRow(int r, const aejVector& v) {
  
  if (v.size()!=NC) HandleError("aejMatrix::setROw: different dimensions.");
  int c;
  for (c=0;c<NC;c++) 
    DATA[r*NC+c] = v[c];
}


/// Sets a matrix column from an aejVector
void aejMatrix::setCol(int c, const aejVector& v) {
  
  if (v.size()!=NR) HandleError("aejMatrix::setCol: different dimensions.");
 
  int r;
  for (r=0;r<NR;r++) 
    DATA[r*NC+c] = v[r];
}

/// read aejMatrices from streams
istream& operator>>(istream& I, aejMatrix& m) {

  
  for(int i=0;i<m.numRows();i++) 
    for(int j=0;j<m.numCols();j++) I >> m(i,j);
  
  return I;
}

/// write aejMatrices to streams
ostream& operator<<(ostream& O, const aejMatrix& m) {

  for(int i=0;i<m.numRows();i++) {
    O << "\n";
    for(int j=0;j<m.numCols();j++) O << m(i,j) << " ";
  }
  return O;
}
