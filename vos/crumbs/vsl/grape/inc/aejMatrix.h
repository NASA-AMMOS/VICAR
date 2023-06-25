#ifndef _AEJMATRIX_H
#define _AEJMATRIX_H
// aejMatrix.h 1.2 02/07/10 12:48:09/
/** \file
 + AUTHOR:  Andrew E. Johnson (aej@ri.cmu.edu)
 + DATE:    4-Feb-98
 + PURPOSE: aejMatrix class definition
 **/

/// Definition of aejVector class and methods

class aejVector

{
  int SIZE;    		///< number of rows and columns
  double *DATA;    	//< Vector data
  
public:
  
  /// constructors, destructors and assignment
  
  aejVector()		{ SIZE = 0; DATA = NULL; }

  aejVector(int);
  aejVector(double,double);
  aejVector(double,double,double);
  aejVector(double,double,double,double);
  aejVector(const aejVector&);
  aejVector operator=(const aejVector&);
  ~aejVector()		{ delete DATA; }
  
  /// slot access
  
  int size() const 	{ return SIZE; }
  double* data()	{ return DATA; }

  double operator[](int r) const {
#ifdef AEJ_CAREFUL
	if ((r<0)||(r>=SIZE))
	  HandleError("aejVector: Coordinate access out of Vector bounds.");
#endif
	return DATA[r];
  }

  double& operator[](int r) {
#ifdef AEJ_CAREFUL
	if ((r<0)||(r>=SIZE))
	  HandleError("aejVector: Coordinate assignment out of Vector bounds.");
#endif
	return DATA[r];
  }

  double operator[](double) const;

  /// input/output

  friend istream& operator>>(istream&, aejVector&);
  friend ostream& operator<<(ostream&, const aejVector&);

  /// data assignment
  
  void setData(double*,int,int);
  void clearData(double);
  void resize(int);
  void copyData(const aejVector&);

  /// vector operations

  double operator*(aejVector) const;
  aejVector operator*(double) const;
  aejVector operator+(aejVector) const;
  aejVector operator-(aejVector) const;
  int operator==(aejVector) const;
  double length() const;
  double lengthSqr() const;
  void sort();
  void normalize() { (*this) = (*this)*(1.0/length()); }
  void stats(double*,double*);
  double interpolate(double) const;

};

/// Definition of aejMatrix class and methods 

class aejMatrix

{
  int NR,NC;    	///< number of rows and columns
  double *DATA;    	///< Matrix data
  
public:
  
  /// constructors, destructors and assignment
  
  aejMatrix()	{ NR = NC = 0; DATA = NULL; }

  aejMatrix(int, int);
  aejMatrix(int);
  aejMatrix(const aejMatrix&);
  aejMatrix operator=(const aejMatrix&);
  ~aejMatrix()	{ delete DATA; }
  
  /// data access

  int numRows() const		{ return NR; }
  int numCols() const		{ return NC; }
  double* data()		{ return DATA; }
  double operator()(int r, int c) const {
#ifdef AEJ_CAREFUL
	if ((r<0)||(r>=NR)||(c<0)||(c>=NC))
	  HandleError("aejMatrix: Pixel access out of Matrix bounds.");
#endif
	return DATA[r*NC+c];
  }

  double& operator()(int r, int c)	{
#ifdef AEJ_CAREFUL
	if ((r<0)||(r>=NR)||(c<0)||(c>=NC)) {
	  cout << *this << " " << r << " " << c << " " << NR 
	  	<< " " << NC << endl;
	  HandleError("aejMatrix: Pixel assignment out of Matrix bounds.");
	}
#endif
	return DATA[r*NC+c];
  }

  aejVector getRow(int) const;
  aejVector getCol(int) const;



  /// data assignment
  
  void setData(double*,int,int,int,int);
  void clearData(double);
  void resize(int,int);
  void copyData(const aejMatrix&);
  void setRow(int,const aejVector&);
  void setCol(int,const aejVector&);
  
  /// input/output

  friend istream& operator>>(istream&, aejMatrix&);
  friend ostream& operator<<(ostream&, const aejMatrix&);

  /// matrix operations

  aejMatrix operator*(double) const;
  aejMatrix operator*(aejMatrix) const;
  aejVector operator*(aejVector) const;
  aejMatrix operator+(aejMatrix) const;
  aejMatrix operator-(aejMatrix) const;
  aejMatrix transpose() const;
  double frobenius() const;
  double trace() const;
  double interpolate(double,double) const;
};

/// function prototypes

void ComputeEigen(const aejMatrix&,aejMatrix&,aejVector&);
void ComputeSVD(const aejMatrix&, aejMatrix&, aejMatrix&, aejMatrix&);
aejVector SolveSVD(const aejMatrix&, const aejVector&);
aejMatrix InvertMatrix(aejMatrix&);
float ran1(long*);
float gasdev(long*);

#endif
