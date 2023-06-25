// aejTransformAlgo.C 1.6 02/07/10 15:02:50
/** \file
 + AUTHOR:  Andrew E. Johnson (aej@ri.cmu.edu)
 + DATE:    30-Oct-97
 + PURPOSE: Contains functions for manipulating and using aejTransform classes
 **/

#include "grape/aej.h"
#include "grape/aejError.h"
#include "grape/aejMatrix.h"
#include "grape/aejTransform.h"

/// returns the cross product of two 3 vectors
aejVector Cross( const aejVector& v1,  const aejVector& v2)
{

  aejVector c(3);

  c[0] = v1[1]*v2[2]-v2[1]*v1[2];
  c[1] = -(v1[0]*v2[2]-v2[0]*v1[2]);
  c[2] = v1[0]*v2[1]-v2[0]*v1[1];

  return c;

}

aejMatrix CrossMat(const aejVector& v)

{

  if (v.size()!=3) 
    HandleError("CrossMat: aejVector not 3-D.");
  
  aejMatrix c(3,3);
  c.clearData(0);
  
  c(0,1) = -v[2]; c(1,0) = - c(0,1);
  c(0,2) = v[1]; c(2,0) = - c(0,2);
  c(1,2) = - v[0]; c(2,1) = - c(1,2);
  return c;
  
}

/// returns determinate of 3x3 aejMatrix
double Determinate3(const aejMatrix& m) 

{

  if ((m.numRows()!=3)||(m.numCols()!=3)) 
    HandleError("det3: Matrix not 3x3.");

  double det;
  
  det = m(0,0)*(m(1,1)*m(2,2)-m(2,1)*m(1,2))+
    m(0,1)*(m(1,2)*m(2,0)-m(1,0)*m(2,2))+
    m(0,2)*(m(1,0)*m(2,1)-m(1,1)*m(2,0));
  
  return det;

}

/// Returns the rotation matrix generated from an axis and an angle.
aejMatrix RotationFromAxisAngle(const aejVector& a, double th)

{
  if (a.size()!=3) 
    HandleError("RotationFromAxisAngle: Axis does not have dimension 3.");

  aejVector K = a*(1.0/a.length());

  aejMatrix R(3,3);
  double c = cos(th);
  double s = sin(th);
  double v = 1-c;

  R(0,0) = K[0]*K[0]*v+c;
  R(0,1) = K[0]*K[1]*v-K[2]*s;
  R(0,2) = K[0]*K[2]*v+K[1]*s;
  R(1,0) = K[0]*K[1]*v+K[2]*s;
  R(1,1) = K[1]*K[1]*v+c;
  R(1,2) = K[1]*K[2]*v-K[0]*s;
  R(2,0) = K[0]*K[2]*v-K[1]*s;
  R(2,1) = K[1]*K[2]*v+K[0]*s;
  R(2,2) = K[2]*K[2]*v+c;

  return R;

}

/// Returns the equivalent angle axis representation for a rotation aejMatrix
void AxisAngleFromRotation( const aejMatrix& R, aejVector& K, double *th)

{

  if ((R.numRows()!=3)||(R.numCols()!=3)) 
    HandleError("AxisAngleFromRotation: Matrix not 3x3.");
  
  K.resize(3);

  if (fabs((R(0,0)+R(1,1)+R(2,2)-1.0)/2.0)>1) {
    cerr << "AxisAngleFromRotation: Input may not be rotation matrix."<< endl; 
    cerr << R << endl;
    *th = 0;
  }
  else *th = acos((R(0,0)+R(1,1)+R(2,2)-1)/2);
  
  if (fabs(*th)<EPSILON) {
    *th = 0;
    K = aejVector(0,0,1);
    return;
  }
  
  if (fabs(*th)>Pi-EPSILON) {

    *th = Pi;
    
    K[0] = sqrt((R(0,0)+1)/2.0); // sign K[0] is arbitrary when rotated by PI
    K[1] = sqrt((R(1,1)+1)/2.0);
    K[2] = sqrt((R(2,2)+1)/2.0);
    K[1] *= SGN(R(0,1)/K[0]*2.0);
    K[2] *= SGN(R(0,2)/K[0]*2.0);

    K = K*(1.0/K.length());
    return;
  }
  
  K[0] = R(2,1)-R(1,2);
  K[1] = R(0,2)-R(2,0);
  K[2] = R(1,0)-R(0,1);
  
  K = K*(1.0/K.length());
  
}

/// Determines the rotation that will map v1 onto v2 
// taking into account singularities. 
aejMatrix RotationFromVectToVect(const aejVector& v1, const aejVector& v2)

{

  if ((v1.size()!=3)||(v2.size()!=3)) 
    HandleError("RotationFromVectToVect: Vectors are not dimension 3.");
  aejVector nv1 = v1*(1.0/v1.length());
  aejVector nv2 = v2*(1.0/v1.length());
  aejVector rot_axis(3);
  aejMatrix rot(3,3);
  double rot_angle;
  
  // first check for identity rotation when vectors are the same
  if ((nv1-nv2).length()<EPSILON)  return aejMatrix(3);

  rot_axis = Cross(nv1,nv2);
  if (fabs(rot_axis.length())>EPSILON) {
    rot_axis = rot_axis*(1.0/rot_axis.length());
    rot_angle = acos((nv1*nv2)/fabs(nv1.length()*nv2.length())); }

  else if (acos(nv1*nv2)==0) {
    rot_axis[0] = rot_axis[1] = 0; rot_axis[2] = 1;
    rot_angle = 0;
  }
  else {

    if ((fabs(nv1[0])>EPSILON)&&(fabs(nv1[1])>EPSILON)) { 
      rot_axis[0] = 1/nv1[0]; 
      rot_axis[1] = -1/nv1[1]; 
      rot_axis[2] = 0;
    }    

    if ((fabs(nv1[0])>EPSILON)&&(fabs(nv1[2])>EPSILON)) { 
      rot_axis[0] = 1/nv1[0]; 
      rot_axis[2] = -1/nv1[2]; 
      rot_axis[1] = 0;
    }    

    if ((fabs(nv1[2])>EPSILON)&&(fabs(nv1[1])>EPSILON)) { 
      rot_axis[2] = 1/nv1[2]; 
      rot_axis[1] = -1/nv1[1]; 
      rot_axis[0] = 0;
    }    

    if ((fabs(nv1[0])<EPSILON)&&(fabs(nv1[1])<EPSILON)) { 
      rot_axis[0] = rot_axis[1] = sqrt(2)/2; rot_axis[2] = 0;
    }

    if ((fabs(nv1[0])<EPSILON)&&(fabs(nv1[2])<EPSILON)) { 
      rot_axis[0] = rot_axis[2] = sqrt(2)/2; rot_axis[1] = 0;
    }

    if ((fabs(nv1[2])<EPSILON)&&(fabs(nv1[1])<EPSILON)) { 
      rot_axis[2] = rot_axis[1] = sqrt(2)/2; rot_axis[0] = 0;
    }
    
    rot_angle = Pi;

  }
  
  rot = RotationFromAxisAngle(rot_axis,rot_angle); 

  return rot;
}


/// determine the fixed angle representation (rx,ry,rz) of rotation aejMatrix 
aejVector FixedAnglesFromRotation(const aejMatrix& m)

{

  if ((m.numRows()!=3)||(m.numCols()!=3)) 
    HandleError("FixedAnglesFromRotation: Matrix not 3x3.");

  aejVector angle(3);

  angle[1] = atan2(-m(2,0),sqrt(m(0,0)*m(0,0)+m(1,0)*m(1,0)));
  angle[2] = atan2(m(1,0)/cos(angle[1]),m(0,0)/cos(angle[1]));
  angle[0] = atan2(m(2,1)/cos(angle[1]),m(2,2)/cos(angle[1]));

  if (angle[1]> 1.56905) {
    
    angle[1] = Pi/2;
    angle[2] = 0;
    angle[0] = atan2(m(0,1),m(1,1));
  }
  
  else if (angle[1]<-1.56905) {
    
    angle[1] = -Pi/2;
    angle[2] = 0;
    angle[0] = -atan2(m(0,1),m(1,1)); 
  }
  
  return angle;
  
}

/// Computes rotation matix from fixed angles rx,ry,rz.
aejMatrix RotationFromFixedAngles(double rx, double ry, double rz)

{
  aejMatrix R(3,3);
  
  R(0,0) = cos(rz)*cos(ry); 
  R(1,0) = sin(rz)*cos(ry);
  R(2,0) = -sin(ry);
  
  R(0,1) = cos(rz)*sin(ry)*sin(rx)-sin(rz)*cos(rx);
  R(1,1) = sin(rz)*sin(ry)*sin(rx)+cos(rz)*cos(rx);
  R(2,1) = cos(ry)*sin(rx);
  
  R(0,2) = cos(rz)*sin(ry)*cos(rx)+sin(rz)*sin(rx);
  R(1,2) = sin(rz)*sin(ry)*cos(rx)-cos(rz)*sin(rx);
  R(2,2) = cos(ry)*cos(rx);
  
  return R;

}

/// Computes rotation matix from fixed angles in a vector
aejMatrix RotationFromFixedAngles(aejVector& a) 

{
  aejMatrix R(3,3);
  double rx = a[0],ry = a[1], rz = a[2];

  R(0,0) = cos(rz)*cos(ry); 
  R(1,0) = sin(rz)*cos(ry);
  R(2,0) = -sin(ry);
  
  R(0,1) = cos(rz)*sin(ry)*sin(rx)-sin(rz)*cos(rx);
  R(1,1) = sin(rz)*sin(ry)*sin(rx)+cos(rz)*cos(rx);
  R(2,1) = cos(ry)*sin(rx);
  
  R(0,2) = cos(rz)*sin(ry)*cos(rx)+sin(rz)*sin(rx);
  R(1,2) = sin(rz)*sin(ry)*cos(rx)-cos(rz)*sin(rx);
  R(2,2) = cos(ry)*cos(rx);
  
  return R;

}

///  Takes a rotation aejMatrix and returns angles for RX*RY*RZ
aejVector FixedZYXAnglesFromRotation(const aejMatrix& m)

{

  if ((m.numRows()!=3)||(m.numCols()!=3)) 
    HandleError("FixedZYXAnglesFromRotation: Matrix not 3x3.");

  aejVector angle(3);

  angle[1] = atan2(m(0,2),sqrt(m(0,0)*m(0,0)+m(0,1)*m(0,1)));
  angle[2] = atan2(-m(0,1)/cos(angle[1]),m(0,0)/cos(angle[1]));
  angle[0] = atan2(-m(1,2)/cos(angle[1]),m(2,2)/cos(angle[1]));

  if (angle[1]>1.56905) {
      
      angle[1] = Pi/2;
      angle[2] = 0;
      angle[0] = atan2(m(1,0),m(1,1));
  }
  
  else if (angle[1]< -1.56905) {
      
    angle[1] = -Pi/2;
    angle[2] = 0;
    angle[0] = atan2(-m(1,0),m(1,1)); 
  }
  
  return angle;
  
}

/// Determines rotation matrix corresponding to unit quaternion 
aejMatrix RotationFromQuaternion(const aejVector& inQ)

{

  if (inQ.size()!=4) 
    HandleError("RotationFromQuaternion: not input 4-vector.");

  // normalize quaternion

  aejVector q = inQ*(1.0/inQ.length());

  aejMatrix R(3,3);

  R(0,0) = SQR(q[0])+SQR(q[1])-SQR(q[2])-SQR(q[3]);
  R(1,1) = SQR(q[0])-SQR(q[1])+SQR(q[2])-SQR(q[3]);
  R(2,2) = SQR(q[0])-SQR(q[1])-SQR(q[2])+SQR(q[3]);

  R(0,1) = 2*(q[1]*q[2]-q[0]*q[3]);
  R(0,2) = 2*(q[1]*q[3]+q[0]*q[2]);
  R(1,2) = 2*(q[2]*q[3]-q[0]*q[1]);

  R(1,0) = 2*(q[1]*q[2]+q[0]*q[3]);
  R(2,0) = 2*(q[1]*q[3]-q[0]*q[2]);
  R(2,1) = 2*(q[2]*q[3]+q[0]*q[1]);

  return R;

}

/// Converts rotation matrix into a unit quaternion 
aejVector QuaternionFromRotation(const aejMatrix& R)

{
  double angle;
  aejVector axis(3),q(0,0,0,0);

  AxisAngleFromRotation(R,axis,&angle);
  
  if (angle==0) q[0] = 1;
  else {

    q[0] = cos(angle/2);
    q[1] = sin(angle/2)*axis[0];
    q[2] = sin(angle/2)*axis[1];
    q[3] = sin(angle/2)*axis[2];
    
    q = q * (1.0/q.length());

  }
  
  return q;

}

/// Find best rotation from matched points using Hebert quaternion method
/** It is the rotation that maps model points to scene points.
 ** The rotation is about the origin of the points. 
 **/
aejMatrix RotationFromMatchedPoints(int npts, aejVector *mpts, aejVector *spts)

{
    
  aejMatrix rot(3),M(4,4),Mt(4,4),A(4,4),e(4,4);
  aejVector ev(4),v(3);
  int i;
  double theta;
  
  for(i=0;i<npts;i++) {

    M(0,1) = Mt(1,0) = mpts[i][2]+spts[i][2];
    M(0,2) = Mt(2,0) = -mpts[i][1]-spts[i][1];
    M(0,3) = Mt(3,0) = mpts[i][0]-spts[i][0];

    M(1,0) = Mt(0,1) = -mpts[i][2]-spts[i][2];
    M(1,2) = Mt(2,1) = mpts[i][0]+spts[i][0];
    M(1,3) = Mt(3,1) = mpts[i][1]-spts[i][1];
   
    M(2,0) = Mt(0,2) = mpts[i][1]+spts[i][1];
    M(2,1) = Mt(1,2) = -mpts[i][0]-spts[i][0];
    M(2,3) = Mt(3,2) = mpts[i][2]-spts[i][2];

    M(3,0) = Mt(0,3) = -mpts[i][0]+spts[i][0];
    M(3,1) = Mt(1,3) = -mpts[i][1]+spts[i][1];
    M(3,2) = Mt(2,3) = -mpts[i][2]+spts[i][2];

    A(0,0) += Mt(0,1)*M(1,0)+Mt(0,2)*M(2,0)+Mt(0,3)*M(3,0);
    A(0,1) += Mt(0,2)*M(2,1)+Mt(0,3)*M(3,1);
    A(0,2) += Mt(0,1)*M(1,2)+Mt(0,3)*M(3,2);
    A(0,3) += Mt(0,1)*M(1,3)+Mt(0,2)*M(2,3);

    A(1,0) += Mt(1,2)*M(2,0)+Mt(1,3)*M(3,0);
    A(1,1) += Mt(1,0)*M(0,1)+Mt(1,2)*M(2,1)+Mt(1,3)*M(3,1);
    A(1,2) += Mt(1,0)*M(0,2)+Mt(1,3)*M(3,2);
    A(1,3) += Mt(1,0)*M(0,3)+Mt(1,2)*M(2,3);

    A(2,0) += Mt(2,1)*M(1,0)+Mt(2,3)*M(3,0);
    A(2,1) += Mt(2,0)*M(0,1)+Mt(2,3)*M(3,1);
    A(2,2) += Mt(2,0)*M(0,2)+Mt(2,1)*M(1,2)+Mt(2,3)*M(3,2);
    A(2,3) += Mt(2,0)*M(0,3)+Mt(2,1)*M(1,3);

    A(3,0) += Mt(3,1)*M(1,0)+Mt(3,2)*M(2,0);
    A(3,1) += Mt(3,0)*M(0,1)+Mt(3,2)*M(2,1);
    A(3,2) += Mt(3,0)*M(0,2)+Mt(3,1)*M(1,2);
    A(3,3) += Mt(3,0)*M(0,3)+Mt(3,1)*M(1,3)+Mt(3,2)*M(2,3);
  
  }

  ComputeEigen(A,e,ev);

  theta = 2.0*acos(e(3,3));
    
  if (theta!=0) {
    
    v[0] = e(0,3)/sin(theta/2.0);  
    v[1] = e(1,3)/sin(theta/2.0);
    v[2] = e(2,3)/sin(theta/2.0);
    
    rot = RotationFromAxisAngle(v, theta);
    return rot;
    
  } 

  rot(0,0) = rot(1,1) = rot(2,2) = 1;
  
  return rot;
}

/// Find best transformation (translation and rotation) for matched points
/**
 **  Returns the translation between a set of points when the 
 **  rotation is supplied.  Best translation is the translation
 **  between the centroid of the model points and the rotated scene points.
 **  Transformation maps mpts to spts. 
 **          R*(pm-cm) = ps-cs ->  R*pm + (cs-R*cm) = ps; 
 **          so, rot = R, trans = cs-R*cm
 **/
aejTransform TransformationFromMatchedPoints(int npts, aejVector *mpts, 
                                             aejVector* spts)
{

  aejTransform TR;
  aejVector cm(3),cs(3);
  int i;
  
  // compute centroids of points

  for(i=0;i<npts;i++) {
    cs = cs+spts[i];
    cm = cm+mpts[i];
  }
  cs = cs*(1.0/npts);
  cm = cm*(1.0/npts);
  
  // subtract centroids from points
  
  for(i=0;i<npts;i++) {
    spts[i] = spts[i]-cs;
    mpts[i] = mpts[i]-cm;
  }
  
  // compute best rigid transformation

  TR.setRot(RotationFromMatchedPoints(npts,mpts,spts));
  TR.setTrans(cs-TR.rot()*cm);
  
  // reset points by adding back centroids 
  
  for(i=0;i<npts;i++) {
    spts[i] = spts[i]+cs;
    mpts[i] = mpts[i]+cm;
  }

#if 0	/* ** disabled to save time, only needed if doing "robust..." */
  // set error to sum of sqr distances between spts and transformed mpts 

  double error = 0;
  for(i=0;i<npts;i++) error += (spts[i]-TR*mpts[i]).lengthSqr();
  
  TR.setError(error/npts);
#endif

  return TR;
  
}    


/// Find tranformation between matched sets of 3-D points using LMedS
/**
 +           Method taken based loosely on Masuda and Yokoya "A Robust Method
 +           for Registration and Segmentation of Multiple Range Images",
 +           2nd CAD Based Vision Workshop, 1994.
 +           Transformation is one that produces the least amount of 
 +           squared error when matching random subsets of points.
 +           Squared error is used because it guarantees that all points are
 +           mathed consistently. Median error would only assure that 1/2 
 +           of the points are matched consistently.
 **/
aejTransform RobustTransformationFromMatchedPoints
               (int npts, aejVector *mpts, aejVector* spts,
		long *seed, int setSize, double goodSampleProb, 
		double outlierPercent)
  
{
  
  int s,i,index;
  aejVector *rSpts = new aejVector[setSize];
  aejVector *rMpts = new aejVector[setSize];

  aejTransform TR,minTR;

  minTR.setError(LARGE*LARGE);
  
  // compute number of sets needed to find sample without outliers
  int nSets = int(log(1-goodSampleProb)/log(1-pow(1-outlierPercent,setSize)));
  
  cout << "Number of sets: " << nSets << endl;
  
  // init random variable
  ran1(seed);
  
  // loop over number of random samples
  
  for(s=0;s<nSets;s++) {
    for(i=0;i<setSize;i++) { // make random point sets
      index = (int) (ran1(seed)*npts);
      rSpts[i] = spts[index];
      rMpts[i] = mpts[index];
    }
    // compute transformation
    TR = TransformationFromMatchedPoints(setSize,rMpts,rSpts); 

    // keep is error is min encountered so far
    
    if (TR.error()<minTR.error()) minTR = TR;
    
  }

  delete []rSpts;
  delete []rMpts;
  
  return minTR;

}

/// Create a random rotation matrix with a maximum angle of maxA degrees. 
aejMatrix RandomRotation(double maxA,int fixAngleFlag = 0)

{
  
  double maxAngle  = maxA*RAD2DEG;

  aejMatrix R(3,3);
  double x1,x2,x3,z,t,r,w,a,b,c,d;
  long seed = -time(NULL); ran1(&seed); // initialize random variable
  
  /* create random doubles in [0..1] */

  x1 = ran1(&seed);
  x2 = ran1(&seed);
  if (fixAngleFlag) x3 = 1;
  else x3 = ran1(&seed);

  /* scale x3 by max_angle */
  
  x3*=(maxAngle/180.0);

  z = x1;
  t = 2*Pi*x2;
  r = sqrt(1-z*z);
  w = Pi*x3;

  /* create quaternion */

  a = cos(w);
  b = sin(w)*cos(t)*r;
  c = sin(w)*sin(t)*r;
  d = sin(w)*z;

  /* create rotation aejMatrix */

  R(0,0) = 1-2*(c*c+d*d);
  R(0,1) = 2*(b*c+a*d);
  R(0,2) = 2*(b*d-a*c);
  R(1,0) = 2*(b*c-a*d);
  R(1,1) = 1-2*(b*b+d*d);
  R(1,2) = 2*(c*d+a*b);
  R(2,0) = 2*(b*d+a*c);
  R(2,1) = 2*(c*d-a*b);
  R(2,2) = 1-2*(b*b+c*c);
  
  return R;

}

/// Fit plane to 3D vectors
/**
 +           Fits the bet LSQ fit plane to a set of 3-D vectors stored in 
 +           an array and returns the plane parameters as a unit surface 
 +           normal n and a n offset d.  The LSQ error is returned
 **/
double PointCloudPlaneParams(int npts, aejVector *PTS, aejVector& n, double* d)

{
  int i;
  double s,sx,sy,sz,sxx,syy,szz,sxy,sxz,syz,x,y,z;
  double err;

  // compute sums
  
  s=sx=sy=sz=sxx=syy=szz=sxy=sxz=syz = 0;
  
  for(i=0;i<npts;i++) {
    
    x = PTS[i][0];
    y = PTS[i][1];
    z = PTS[i][2];
    
    s++;
    sx+=x;
    sy+=y;
    sz+=z;
    sxx+=x*x;
    syy+=y*y;
    szz+=z*z;
    sxy+=x*y;
    sxz+=x*z;
    syz+=y*z;
  }
  
  
  if (s>=3) {
    
    double oneoverN = 1/s;
    
    aejMatrix inertia(3,3);
    aejMatrix e(3,3);
    aejVector ev(3);
    
    inertia(0,0) = (sxx-SQR(sx)*oneoverN);
    inertia(1,1) = (syy-SQR(sy)*oneoverN);
    inertia(2,2) = (szz-SQR(sz)*oneoverN);
    inertia(1,0) = inertia(0,1) = (sxy-sx*sy*oneoverN);
    inertia(2,0) = inertia(0,2) = (sxz-sx*sz*oneoverN);
    inertia(2,1) = inertia(1,2) = (syz-sy*sz*oneoverN);
    
    ComputeEigen(inertia,e,ev);
    
    // The eigen vector corresponding to the smallest eigen value of 
    // the inertia matrix is the normal of the plane.
    
    n = e.getCol(2);
    
    err = fabs(ev[2]);
    
    *d = -(sx*n[0]+sy*n[1]+sz*n[2])/s;

  }
  
  return err;

}
  
/// Reads a sequence of transformations from a file and stores them in array
aejTransform* ReadTrajectory(char* filename, int* nt)
  
{

  int i; 
  
  ifstream readStream(filename);
  if (!readStream) 
    HandleError("ReadTrajectory: Cannot open file for reading.");
  readStream >> *nt;
  aejTransform *TR = new aejTransform[*nt];
  for(i=0;i<*nt;i++)     readStream >> TR[i];

  readStream.close();

  return TR;
  
}

/// Converts a rho theta phi spherical vector to x,y,z
aejVector SphericalToCartesian(aejVector& s) {
  
  aejVector p(3);

  p[0] = s[0]*cos(s[2])*sin(s[1]);
  p[1] = s[0]*sin(s[2]);
  p[2] = s[0]*cos(s[2])*cos(s[1]);

  return p;

}

/// Converts an xyz vector to a rho theta phi spherical vector
aejVector CartesianToSpherical(aejVector& p) {
  
  aejVector s(3);

  s[1] = atan2(p[0],p[2]); // atan2(x/z);
  s[2] = atan2(p[1],sqrt(SQR(p[0])+SQR(p[2])));
  s[0] = p.length();

  return s;

}
