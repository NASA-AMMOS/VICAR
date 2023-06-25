#ifndef _AEJTRANSFORM_H
#define _AEJTRANSFORM_H
// aejTransform.h 1.3 02/07/10 12:48:09
/** \file
 + AUTHOR:  Andrew E. Johnson (aej@robotics.jpl.nasa.gov)
 + DATE:    17-Apr-98
 + PURPOSE: Defines all classes used for aejTransform classs
 **/

/// Definition of aejTransform class and methods

class aejTransform
{

  
  int ID;
  aejVector TRANS;
  aejMatrix ROT;
  double ERROR;
  
public:

  /// constructors, destructors and assignment
  
  aejTransform();
  aejTransform(const aejTransform&);
  aejTransform& operator=(const aejTransform&);
  ~aejTransform();

  /// slot access

  int id() const;
  const aejVector& trans() const;
  const aejMatrix& rot() const;
  double error() const;

  /// data assignment
  
  void setID(int);
  void setTrans(const aejVector&);
  void setRot(const aejMatrix&);
  void setError(double);

  /// functions

  void invert();
  class aejVector fixedAngles();
  aejTransform inverse() const;
  aejTransform operator*(aejTransform) const;
  class aejVector operator*(class aejVector) const;

  /// input/output

  friend istream& operator>>(istream&, aejTransform&);
  friend ostream& operator<<(ostream&, const aejTransform&);
  int readFile(char*);
  int writeFile(char*);
  
};


/// function prototypes

aejVector Cross(const aejVector&,const aejVector&);
aejMatrix CrossMat(const aejVector&);
double Determinate3(const aejMatrix& m);
aejMatrix RotationFromAxisAngle( const aejVector& K, double th);
void AxisAngleFromRotation( const aejMatrix& R, aejVector& K, double *th);
aejMatrix RotationFromVectToVect(const aejVector& v1, const aejVector& v2);
aejVector FixedAnglesFromRotation(const aejMatrix& m);
aejMatrix RotationFromFixedAngles(double rx, double ry, double rz);
aejVector FixedZYXAnglesFromRotation(const aejMatrix& m);
aejMatrix RotationFromQuaternion(const aejVector& q);
aejVector QuaternionFromRotation(const aejMatrix&);
aejMatrix RotationFromMatchedPoints(int, aejVector*, aejVector*);
aejTransform TransformationFromMatchedPoints(int, aejVector*,aejVector*);
aejMatrix RandomRotation(double,int);
aejMatrix RotationFromFixedAngles(aejVector&);
double PointCloudPlaneParams(int, aejVector*, aejVector&, double*);
aejTransform* ReadTrajectory(char*, int*);
aejTransform RobustTransformationFromMatchedPoints
(int,aejVector*,aejVector*,long*,int,double,double);
aejVector SphericalToCartesian(aejVector& s);
aejVector CartesianToSpherical(aejVector& p);
aejTransform TranslationFromMatchedPoints(int, aejVector*,aejVector*);

#endif
