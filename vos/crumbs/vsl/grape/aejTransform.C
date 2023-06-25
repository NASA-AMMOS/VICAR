// aejTransform.C 1.2 02/07/10 15:02:50
/** \file
 + AUTHOR:  Andrew E. Johnson (aej@ri.cmu.edu)
 + DATE:    30-Oct-97
 + PURPOSE: Contains functions for manipulating and using aejTransform classes
 **/

#include "grape/aej.h"
#include "grape/aejError.h"
#include "grape/aejMatrix.h"
#include "grape/aejTransform.h"

/// Create aejTransforms from varing inputs


/// NULL aejTransform
aejTransform::aejTransform() {
  TRANS = aejVector(0,0,0);
  ROT = aejMatrix(3);
  ERROR = 0;
  ID = 0;
}

/// copy aejTransform
aejTransform::aejTransform(const aejTransform& c) { 
  TRANS = c.TRANS;
  ROT = c.ROT;
  ERROR = c.ERROR;
  ID = c.ID; 
}

/// assign aejTransform
aejTransform& aejTransform::operator=(const aejTransform& c) {
  TRANS = c.TRANS;
  ROT = c.ROT;
  ERROR = c.ERROR;
  ID = c.ID; 
  return *this;
}

/// destroy aejTransform
aejTransform::~aejTransform(){}


/// Functions for accessing aejFeature slots

int aejTransform::id() const { return ID; }
const aejVector& aejTransform::trans() const { return TRANS; }
const aejMatrix& aejTransform::rot() const { return ROT; }
double aejTransform::error() const { return ERROR; }

void aejTransform::setID(int i) { ID = i;}
void aejTransform::setTrans(const aejVector& t) { TRANS = t;}
void aejTransform::setRot(const aejMatrix& r) { ROT = r;}
void aejTransform::setError(double err) { ERROR = err; }

/// Functions for operating on transforms

/// invert current transform
void aejTransform::invert() {
  ROT = ROT.transpose();
  TRANS = ROT*TRANS*(-1); // rotation transposed in above line
}

/// return new transform that's the inverse of the current one
aejTransform aejTransform::inverse() const {
  aejTransform Tnew;
  Tnew.ROT = ROT.transpose();
  Tnew.TRANS = ROT.transpose()*TRANS*(-1);
  return Tnew;
}

/// post-multiply (concatenate) transforms
aejTransform aejTransform::operator*(aejTransform T) const {
  
  aejTransform Tnew;
  Tnew.ROT = ROT*T.ROT;
  Tnew.TRANS = ROT*T.TRANS+TRANS;
  return Tnew;
}

/// apply transform to a point
aejVector aejTransform::operator*(aejVector v) const {
  aejVector p(3);

  p[0] = ROT(0,0)*v[0]+ROT(0,1)*v[1]+ROT(0,2)*v[2]+TRANS[0];
  p[1] = ROT(1,0)*v[0]+ROT(1,1)*v[1]+ROT(1,2)*v[2]+TRANS[1];
  p[2] = ROT(2,0)*v[0]+ROT(2,1)*v[1]+ROT(2,2)*v[2]+TRANS[2];

  return p;
}

/// read aejTransform from stream
istream& operator>>(istream& I, aejTransform& c) {
  char buf[255];
  int i;
  aejVector t(3);
  aejMatrix r(3,3);
  I >> buf >> i; c.setID(i);
  I >> buf >> t; c.setTrans(t);  
  I >> buf >> r; c.setRot(r);
  return I;
}

/// write aejTransform to stream
ostream& operator<<(ostream& O, const aejTransform& c) {
  return O << "\naejTransform        " << c.id() 
	   << "\ntranslation:        " << c.trans() 
	   << "\nrotation:           " << c.rot()
	   << endl;
}

/// read aejTransform from file
int aejTransform::readFile(char* filename)
{
  ifstream readStream(filename);
  if (!readStream) 
    HandleError("aejTransform::readFile: Cannot open file for reading.");
  readStream >> *this;
  readStream.close();
  return 1;
}

/// write aejTransform to file
int aejTransform::writeFile(char* filename)
{
  ofstream writeStream(filename);
  if (!writeStream) 
    HandleError("aejTransform::writeFile: Cannot open file for writing.");
  writeStream << *this;
  writeStream.close();  
  return 1;

}

/// return fixed angles of tranform as a vector in radians
aejVector aejTransform::fixedAngles()
{
  return FixedAnglesFromRotation(ROT);
}
