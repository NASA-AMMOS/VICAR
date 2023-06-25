// aejMatrixAlgo.C 1.3 02/07/10 15:02:50
/** \file
 + AUTHOR:  Andrew E. Johnson (aej@ri.cmu.edu)
 + DATE:    4-Feb-98
 + PURPOSE: aejMatrix algorithms 
 **/

#include "grape/aej.h"
#include "grape/aejError.h"
#include "grape/aejMatrix.h"
#include "grape/aejNR.h"

/// Finds eigenvectors and eigenvalues of a matrix. 
/**
 + AUTHOR:   Numerical Recipes in C
 + MODIFIED: Andrew E. Johnson (aej@robotics.jpl.nasa.gov)
 + DATE:     4-Apr-98
 + PURPOSE:   Modified to work with C++ and aejMatrices by aej
 **/

#define ROTATE(a,i,j,k,l) g=a(i,j);h=a(k,l);a(i,j)=g-s*(h+g*tau);\
	a(k,l)=h+s*(g-h*tau);

int Jacobi(aejMatrix& a, int n, aejVector& d, aejMatrix& v, int *nrot)
  
{

  int j,iq,ip,i;
  double tresh,theta,tau,t,sm,s,h,g,c;
  aejVector b(n+1);
  aejVector z(n+1);
  
  for (ip=1;ip<=n;ip++) {
    for (iq=1;iq<=n;iq++) v(ip,iq)=0.0;
    v(ip,ip)=1.0;
  }
  for (ip=1;ip<=n;ip++) {
    b[ip]=d[ip]=a(ip,ip);
    z[ip]=0.0;
  }
  *nrot=0;
  for (i=1;i<=50;i++) {
    sm=0.0;
    for (ip=1;ip<=n-1;ip++) {
      for (iq=ip+1;iq<=n;iq++)
	sm += fabs(a(ip,iq));
    }
    if (sm == 0.0) {
      return 1;
    }
    if (i < 4)
      tresh=0.2*sm/(n*n);
    else
      tresh=0.0;
    for (ip=1;ip<=n-1;ip++) {
      for (iq=ip+1;iq<=n;iq++) {
	g=100.0*fabs(a(ip,iq));
	if (i > 4 && fabs(d[ip])+g == fabs(d[ip])
	    && fabs(d[iq])+g == fabs(d[iq]))
	  a(ip,iq)=0.0;
	else if (fabs(a(ip,iq)) > tresh) {
	  h=d[iq]-d[ip];
	  if (fabs(h)+g == fabs(h))
	    t=(a(ip,iq))/h;
	  else {
	    theta=0.5*h/(a(ip,iq));
	    t=1.0/(fabs(theta)+sqrt(1.0+theta*theta));
	    if (theta < 0.0) t = -t;
	  }
	  c=1.0/sqrt(1+t*t);
	  s=t*c;
	  tau=s/(1.0+c);
	  h=t*a(ip,iq);
	  z[ip] -= h;
	  z[iq] += h;
	  d[ip] -= h;
	  d[iq] += h;
	  a(ip,iq)=0.0;
	  for (j=1;j<=ip-1;j++) {
	    ROTATE(a,j,ip,j,iq)
	      }
	  for (j=ip+1;j<=iq-1;j++) {
	    ROTATE(a,ip,j,j,iq)
	      }
	  for (j=iq+1;j<=n;j++) {
	    ROTATE(a,ip,j,iq,j)
	      }
	  for (j=1;j<=n;j++) {
	    ROTATE(v,j,ip,j,iq)
	      }
	  ++(*nrot);
	}
      }
    }
    for (ip=1;ip<=n;ip++) {
      b[ip] += z[ip];
      d[ip]=b[ip];
      z[ip]=0.0;
    }
  }
  cerr << "Too many iterations in routine JACOBI"; return 0;
  
}

#undef ROTATE

/// Sorts eigenvectors from jacobi based on eigenvalues.
/**
 + AUTHOR:   Numerical Recipes in C
 + MODIFIED: Andrew E. Johnson (aej@robotics.jpl.nasa.gov)
 + DATE:     3-Nov-94
 +	     Modified to work with C++ and LEDA package by aej.
 +           Also modified to sort based on absolute value.
 **/
void Eigsrt(aejVector& d,aejMatrix& v, int n)
  
{
  int k,j,i;
  double p;
  
  for (i=1;i<n;i++) {
    p=d[k=i];
    for (j=i+1;j<=n;j++)
      if (fabs(d[j]) >= fabs(p)) p=d[k=j];
    if (k != i) {
      d[k]=d[i];
      d[i]=p;
      for (j=1;j<=n;j++) {
	p=v(j,i);
	v(j,i)=v(j,k);
	v(j,k)=p;
      }
    }
  }
}

/**
 + AUTHOR:   Numerical Recipes in C
 + MODIFIED: Andrew E. Johnson (aej@robotics.jpl.nasa.gov)
 + DATE:     9-Apr-98
 **/
double Pythag(double a, double b)
{
  double absa,absb;
  absa=fabs(a);
  absb=fabs(b);
  if (absa > absb) return absa*sqrt(1.0+SQR(absb/absa));
  else return (absb == 0.0 ? 0.0 : absb*sqrt(1.0+SQR(absa/absb)));
}

/**
 + AUTHOR:   Numerical Recipes in C
 + MODIFIED: Andrew E. Johnson (aej@robotics.jpl.nasa.gov)
 + DATE:     9-Apr-98
 **/
void Svdcmp(aejMatrix& a, int m, int n, aejVector& w, aejMatrix& v)
{

  int flag,i,its,j,jj,k,l,nm;
  double anorm,c,f,g,h,s,scale,x,y,z;
  
  aejVector rv1(n+1);

  g=scale=anorm=0.0;
  for (i=1;i<=n;i++) {
    l=i+1;
    rv1[i]=scale*g;
    g=s=scale=0.0;
    if (i <= m) {
      for (k=i;k<=m;k++) scale += fabs(a(k,i));
      if (scale) {
	for (k=i;k<=m;k++) {
	  a(k,i) /= scale;
	  s += a(k,i)*a(k,i);
	}
	f=a(i,i);
	g = -SIGN(sqrt(s),f);
	h=f*g-s;
	a(i,i)=f-g;
	for (j=l;j<=n;j++) {
	  for (s=0.0,k=i;k<=m;k++) s += a(k,i)*a(k,j);
	  f=s/h;
	  for (k=i;k<=m;k++) a(k,j) += f*a(k,i);
	}
	for (k=i;k<=m;k++) a(k,i) *= scale;
      }
    }
    w[i]=scale *g;
    g=s=scale=0.0;
    if (i <= m && i != n) {
      for (k=l;k<=n;k++) scale += fabs(a(i,k));
      if (scale) {
	for (k=l;k<=n;k++) {
	  a(i,k) /= scale;
	  s += a(i,k)*a(i,k);
	}
	f=a(i,l);
	g = -SIGN(sqrt(s),f);
	h=f*g-s;
	a(i,l)=f-g;
	for (k=l;k<=n;k++) rv1[k]=a(i,k)/h;
	for (j=l;j<=m;j++) {
	  for (s=0.0,k=l;k<=n;k++) s += a(j,k)*a(i,k);
	  for (k=l;k<=n;k++) a(j,k) += s*rv1[k];
	}
	for (k=l;k<=n;k++) a(i,k) *= scale;
      }
    }
    anorm=FMAX(anorm,(fabs(w[i])+fabs(rv1[i])));
  }
  for (i=n;i>=1;i--) {
    if (i < n) {
      if (g) {
	for (j=l;j<=n;j++)
	  v(j,i)=(a(i,j)/a(i,l))/g;
	for (j=l;j<=n;j++) {
	  for (s=0.0,k=l;k<=n;k++) s += a(i,k)*v(k,j);
	  for (k=l;k<=n;k++) v(k,j) += s*v(k,i);
	}
      }
      for (j=l;j<=n;j++) v(i,j)=v(j,i)=0.0;
    }
    v(i,i)=1.0;
    g=rv1[i];
    l=i;
  }
  for (i=IMIN(m,n);i>=1;i--) {
    l=i+1;
    g=w[i];
    for (j=l;j<=n;j++) a(i,j)=0.0;
    if (g) {
      g=1.0/g;
      for (j=l;j<=n;j++) {
	for (s=0.0,k=l;k<=m;k++) s += a(k,i)*a(k,j);
	f=(s/a(i,i))*g;
	for (k=i;k<=m;k++) a(k,j) += f*a(k,i);
      }
      for (j=i;j<=m;j++) a(j,i) *= g;
    } else for (j=i;j<=m;j++) a(j,i)=0.0;
    ++a(i,i);
  }
  for (k=n;k>=1;k--) {
    for (its=1;its<=30;its++) {
      flag=1;
      for (l=k;l>=1;l--) {
	nm=l-1;
	if ((double)(fabs(rv1[l])+anorm) == anorm) {
	  flag=0;
	  break;
	}
	if ((double)(fabs(w[nm])+anorm) == anorm) break;
      }
      if (flag) {
	c=0.0;
	s=1.0;
	for (i=l;i<=k;i++) {
	  f=s*rv1[i];
	  rv1[i]=c*rv1[i];
	  if ((double)(fabs(f)+anorm) == anorm) break;
	  g=w[i];
	  h=Pythag(f,g);
	  w[i]=h;
	  h=1.0/h;
	  c=g*h;
	  s = -f*h;
	  for (j=1;j<=m;j++) {
	    y=a(j,nm);
	    z=a(j,i);
	    a(j,nm)=y*c+z*s;
	    a(j,i)=z*c-y*s;
	  }
	}
      }
      z=w[k];
      if (l == k) {
	if (z < 0.0) {
	  w[k] = -z;
	  for (j=1;j<=n;j++) v(j,k) = -v(j,k);
	}
	break;
      }
      if (its == 30) HandleError("no convergence in 30 svdcmp iterations");
      x=w[l];
      nm=k-1;
      y=w[nm];
      g=rv1[nm];
      h=rv1[k];
      f=((y-z)*(y+z)+(g-h)*(g+h))/(2.0*h*y);
      g=Pythag(f,1.0);
      f=((x-z)*(x+z)+h*((y/(f+SIGN(g,f)))-h))/x;
      c=s=1.0;
      for (j=l;j<=nm;j++) {
	i=j+1;
	g=rv1[i];
	y=w[i];
	h=s*g;
	g=c*g;
	z=Pythag(f,h);
	rv1[j]=z;
	c=f/z;
	s=h/z;
	f=x*c+g*s;
	g = g*c-x*s;
	h=y*s;
	y *= c;
	for (jj=1;jj<=n;jj++) {
	  x=v(jj,j);
	  z=v(jj,i);
	  v(jj,j)=x*c+z*s;
	  v(jj,i)=z*c-x*s;
	}
	z=Pythag(f,h);
	w[j]=z;
	if (z) {
	  z=1.0/z;
	  c=f*z;
	  s=h*z;
	}
	f=c*g+s*y;
	x=c*y-s*g;
	for (jj=1;jj<=m;jj++) {
	  y=a(jj,j);
	  z=a(jj,i);
	  a(jj,j)=y*c+z*s;
	  a(jj,i)=z*c-y*s;
	}
      }
      rv1[l]=0.0;
      rv1[k]=f;
      w[k]=x;
    }
  }
}

/// Reorders matrices from Svdcmp to order rows by magnitude of singular values
/**
 + AUTHOR:   Andrew E. Johnson (aej@robotics.jpl.nasa.gov)
 + DATE:     9-Apr-98
 **/
void Svdsrt(aejMatrix& u, aejVector& d, aejMatrix& v)
  
{

  int m = u.numRows()-1;
  int n = u.numCols()-1;
  
  int k,j,i;
  double p;
  
  for (i=1;i<n;i++) {
    p=d[k=i];
    for (j=i+1;j<=n;j++)
      if (fabs(d[j]) >= fabs(p)) p=d[k=j];
    if (k != i) {
      d[k]=d[i];
      d[i]=p;
      for (j=1;j<=n;j++) { // flip columns of v
	p=v(j,i);
	v(j,i)=v(j,k);
	v(j,k)=p;
      }
      for (j=1;j<=m;j++) { // flip columns of u
	p=u(j,i);
	u(j,i)=u(j,k);
	u(j,k)=p;
      }
    }
  }
}

/// Computes LU decomposition of a matrix
/**
 + AUTHOR:   Numerical Recipes in C
 + DATE:     1-Sep-98
 **/

#define TINY 1.0e-20;

void Ludcmp(aejMatrix& a, int n, aejVector& indx, double *d)
{
  int i,imax,j,k;
  float big,dum,sum,temp;

  indx.resize(n+1);
  aejVector vv(n+1);

  *d=1.0;
  for (i=1;i<=n;i++) {
    big=0.0;
    for (j=1;j<=n;j++)
      if ((temp=fabs(a(i,j))) > big) big=temp;
    if (big == 0.0) HandleError("Ludcmp: Singular matrix");
    vv[i]=1.0/big;
  }
  for (j=1;j<=n;j++) {
    for (i=1;i<j;i++) {
      sum=a(i,j);
      for (k=1;k<i;k++) sum -= a(i,k)*a(k,j);
      a(i,j)=sum;
    }
    big=0.0;
    for (i=j;i<=n;i++) {
      sum=a(i,j);
      for (k=1;k<j;k++)
	sum -= a(i,k)*a(k,j);
      a(i,j)=sum;
      if ( (dum=vv[i]*fabs(sum)) >= big) {
	big=dum;
	imax=i;
      }
    }
    if (j != imax) {
      for (k=1;k<=n;k++) {
	dum=a(imax,k);
	a(imax,k)=a(j,k);
	a(j,k)=dum;
      }
      *d = -(*d);
      vv[imax]=vv[j];
    }
    indx[j]=imax;
    if (a(j,j) == 0.0) a(j,j)=TINY;
    if (j != n) {
      dum=1.0/(a(j,j));
      for (i=j+1;i<=n;i++) a(i,j) *= dum;
    }
  }

}
#undef TINY

/// Performs LU backsubstitution
/**
 + AUTHOR:   Numerical Recipes in C
 + DATE:     1-Sep-98
 **/
void Lubksb(aejMatrix& a, int n, aejVector& indx, aejVector& b)
{
  int i,ii=0,ip,j;
  double sum;
  
  for (i=1;i<=n;i++) {
    ip=int(indx[i]);
    sum=b[ip];
    b[ip]=b[i];
    if (ii)
      for (j=ii;j<=i-1;j++) sum -= a(i,j)*b[j];
    else if (sum) ii=i;
    b[i]=sum;
  }
  for (i=n;i>=1;i--) {
    sum=b[i];
    for (j=i+1;j<=n;j++) sum -= a(i,j)*b[j];
    b[i]=sum/a(i,i);
  }
}

/// computes eigenvalues and eigenvectors sorted by eigenvalue
/**
 + AUTHOR:   Andrew E. Johnson (aej@robotics.jpl.nasa.gov)
 + DATE:     6-Apr-98
 + PURPOSE:  Takes in a square aejMatrix and computes eigenvalues and 
 +           eigenvectors sorted by eigenvalue
 **/
void ComputeEigen(const aejMatrix& A, aejMatrix& eigVecs, aejVector& eigVals) 

{

  int n = A.numRows();
  int r,c,nrot;
  
  if (A.numRows()!=A.numCols()) 
    HandleError("ComputeEigen: aejMatrix not square.");
  
  // shift A by 1 row and 1 column for use in NRC algorithm

  aejMatrix a(n+1,n+1);
  for (r=0;r<n;r++)
    for (c=0;c<n;c++) a(r+1,c+1) = A(r,c);
  a(0,0) = 1.0;
  
  aejMatrix v(n+1,n+1);
  aejVector d(n+1);

  Jacobi(a,n,d,v,&nrot);
  Eigsrt(d,v,n);
  
  eigVecs.resize(n,n);
  eigVals.resize(n);

  // copy shifted eigenvals and vecs into return variables
  
  for (r=0;r<n;r++){
    for (c=0;c<n;c++) eigVecs(r,c) = v(r+1,c+1);
    eigVals[r] = d[r+1];
  }

}

/**
 + AUTHOR:   Andrew E. Johnson (aej@robotics.jpl.nasa.gov)
 + DATE:     9-Apr-98
 **/
void ComputeSVD(const aejMatrix& A, aejMatrix& U, aejMatrix& D, aejMatrix& V) 

{

  int m = A.numRows();
  int n = A.numCols();
  int r,c;
  
  // shift A by 1 row and 1 column for use in NRC algorithm

  aejMatrix a(m+1,n+1);
  for (r=0;r<m;r++)
    for (c=0;c<n;c++) a(r+1,c+1) = A(r,c);
  a(0,0) = 1.0;
  
  aejMatrix v(n+1,n+1);
  aejVector w(n+1);
  
  Svdcmp(a,m,n,w,v);
  
  // reorder matrices according to magnitude of singular values 

  Svdsrt(a,w,v);

  U.resize(m,n);
  D.resize(n,n);
  V.resize(n,n);

  // copy shifted matrics and vectors into return variables 
  
  for (r=0;r<m;r++)
    for (c=0;c<n;c++) U(r,c) = a(r+1,c+1);
  
  for (r=0;r<n;r++) {
    for (c=0;c<n;c++) V(r,c) = v(r+1,c+1);
    D(r,r) = w[r+1];
  }
    
}

/**
 + AUTHOR:   Andrew E. Johnson (aej@robotics.jpl.nasa.gov)
 + DATE:     9-Apr-98
 */
aejVector SolveSVD(const aejMatrix& A, const aejVector& b)

{

  int m = A.numRows();
  int n = A.numCols();
  int i;
  
  aejMatrix U(m,n);
  aejMatrix V(n,n);
  aejMatrix D(n,n);
  
  ComputeSVD(A,U,D,V);
  
  aejVector x(n);

  for(i=0;i<n;i++) D(i,i) = 1.0/D(i,i);
  
  x = V*D*(U.transpose()*b);

  return x;

}

/// Invert square matrix
/**
 + AUTHOR:   Andrew E. Johnson (aej@robotics.jpl.nasa.gov)
 + DATE:     1-Sep-98
 + PURPOSE:  Takes in a square aejMatrix and inverts it using 
 +           LU decomposition
 **/
aejMatrix InvertMatrix(aejMatrix& A) 

{

  if (A.numCols()!=A.numRows()) 
    HandleError("InvertMatrix: array is not square");

  int N = A.numCols();
  int r,c,i,j;
  double d;
  aejVector col(N+1),indx(N+1);
  aejMatrix a(N+1,N+1),inv(N,N);
  
  // shift A by 1 row and 1 column for use in NRC algorithm
  
  for (r=0;r<N;r++)
    for (c=0;c<N;c++) a(r+1,c+1) = A(r,c);
  a(0,0) = 1.0;
  
  Ludcmp(a,N,indx,&d);

  for(j=1;j<=N;j++) {
    for(i=1;i<=N;i++) col[i] = 0;
    col[j] = 1.0;
    Lubksb(a,N,indx,col);
    for(i=1;i<=N;i++) inv(i-1,j-1) = col[i];
  }
  
  return inv;
  
}

/// Returns uniform random variable in the range 0.0 to 1.0
/**
 + AUTHOR:   Numerical Recipes in C
 + MODIFIED: Andrew E. Johnson (aej@robotics.jpl.nasa.gov)
 + DATE:     28-Apr-98
 */

#define IA 16807
#define IM 2147483647
#define AM (1.0/IM)
#define IQ 127773
#define IR 2836
#define NTAB 32
#define NDIV (1+(IM-1)/NTAB)
#define EPS 1.2e-7
#define RNMX (1.0-EPS)

float ran1(long *idum)
{
  int j;
  long k;
  static long iy=0;
  static long iv[NTAB];
  float temp;
  
  if (*idum <= 0 || !iy) {
    if (-(*idum) < 1) *idum=1;
    else *idum = -(*idum);
    for (j=NTAB+7;j>=0;j--) {
      k=(*idum)/IQ;
      *idum=IA*(*idum-k*IQ)-IR*k;
      if (*idum < 0) *idum += IM;
      if (j < NTAB) iv[j] = *idum;
    }
    iy=iv[0];
  }
  k=(*idum)/IQ;
  *idum=IA*(*idum-k*IQ)-IR*k;
  if (*idum < 0) *idum += IM;
  j=iy/NDIV;
  iy=iv[j];
  iv[j] = *idum;
  if ((temp=AM*iy) > RNMX) return RNMX;
  else return temp;
}
#undef IA
#undef IM
#undef AM
#undef IQ
#undef IR
#undef NTAB
#undef NDIV
#undef EPS
#undef RNMX

/// Returns random variable from normal distribution of variance 1
/**
 + AUTHOR:   Numerical Recipes in C
 + MODIFIED: Andrew E. Johnson (aej@robotics.jpl.nasa.gov)
 + DATE:     28-Apr-98
 **/
float gasdev(long *idum)
{
  float ran1(long *idum);
  static int iset=0;
  static float gset;
  float fac,rsq,v1,v2;
  
  if  (iset == 0) {
    do {
      v1=2.0*ran1(idum)-1.0;
      v2=2.0*ran1(idum)-1.0;
      rsq=v1*v1+v2*v2;
    } while (rsq >= 1.0 || rsq == 0.0);
    fac=sqrt(-2.0*log(rsq)/rsq);
    gset=v1*fac;
    iset=1;
    return v2*fac;
  } else {
    iset=0;
    return gset;
  }
}

