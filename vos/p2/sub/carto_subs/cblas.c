/*******************************************************************************

  Title:     cblas
  Author:    Mike Burl (translated from FORTRAN dblas.f, drotg.f drot.f)
  Date:      Nov 25, 1998
  Function:  C replicas of the linpack dblas, drotg, and drot routines

*******************************************************************************/
#include <stdio.h>
#include <math.h>
#include "cblas.h"
#include "burl.h"

/********************/
/* dsign            */
/********************/

/* This is an intrinsic function in fortran. Return magnitude of
   a with sign taken from b */

double dsign(double a, double b) {
 
  double ans;

  ans = ((b >= 0) ? fabs(a) : -fabs(a));

  return(ans);
}

/********************/
/* dasum            */
/********************/

/* takes the sum of the absolute values.
   jack dongarra, linpack, 3/11/78. */

double dasum(int n, double *dx, int incx) {

  int     i, nincx;
  double  dtemp;
  char    infunc[] = "dasum";

  dtemp = D_ZERO;
  if (n <= 0) {
    printf ("ERROR (%s): n = %d <= 0", infunc, n);
    return(D_ZERO);
  }
  nincx = n*incx;
  for (i = 0; i < nincx; i += incx) {
    dtemp += fabs(dx[i]);
  }

  return(dtemp);
}

/********************/
/* daxpy            */
/********************/

/* constant times a vector plus a vector.
   jack dongarra, linpack, 3/11/78. */

int daxpy(int n, double da, double *dx, int incx, double *dy, int incy) {
  
  int   i, ix, iy;
  char  infunc[] = "daxpy";

  if (n <= 0) {
    printf("ERROR (%s): n = %d < 0\n", infunc, n);
    return(ERR);
  }
  if (da == D_ZERO) {
    return(OK);
  }
      
  ix = 0;
  iy = 0;
  if (incx < 0) {
    ix = (-n+1)*incx;
  }
  if (incy < 0) {
    iy = (-n+1)*incy;
  }
  for (i = 0; i < n; i++) {
    dy[iy] += da*dx[ix];
    ix += incx;
    iy += incy;
  }

  return(OK);
}


/********************/
/* dcopy            */
/********************/

/* copies a vector, x, to a vector, y.
   jack dongarra, linpack, 3/11/78. */

int dcopy(int n, double *dx, int incx, double *dy, int incy) {

  int   i, ix, iy;
  char  infunc[] = "dcopy";

  if (n <= 0) {
    printf("ERROR (%s): n = %d < 0\n", infunc, n);
    return(ERR);
  }

  ix = 0;
  iy = 0;
  if (incx < 0) {
    ix = (-n+1)*incx;
  }
  if (incy < 0) {
    iy = (-n+1)*incy;
  }
  for (i = 0; i < n; i++) {
    dy[iy] = dx[ix];
    ix += incx;
    iy += incy;
  }

  return(OK);
}


/********************/
/* ddot             */
/********************/

/* forms the dot product of two vectors.
   jack dongarra, linpack, 3/11/78. */

double ddot(int n, double *dx, int incx, double *dy, int incy) {

  double  dtemp;
  int     i, ix, iy;
  char    infunc[] = "ddot";

  dtemp = D_ZERO;
  if (n < 0) {
    printf("ERROR (%s): n = %d < 0\n", infunc, n);
    return(dtemp);
  }
  ix = 0;
  iy = 0;
  if (incx < 0) {
    ix = (-n+1)*incx;
  }
  if (incy < 0) {
    iy = (-n+1)*incy;
  }
  for (i = 0; i < n; i++) {
    dtemp += dx[ix]*dy[iy];
    ix += incx;
    iy += incy;
  }
  return(dtemp);
}

/********************/
/* dmach            */
/********************/

double dmach(int job) {
 
  char infunc[] = "dmach";

  printf("ERROR (%s): not ported\n", infunc);
  return(D_ZERO);
}

/********************/
/* dnrm2            */
/********************/

/* euclidean norm of the n-vector stored in dx() with storage increment incx. */
/* MOD: 981125 (MCB) - Replaced complicated dblas.f function with my own 
   naive implementation. */

double dnrm2(int n, double *dx, int incx) {

  int    i, nincx;
  double dtemp;
  /*  char   infunc[] = "dnrm2"; */

  dtemp = D_ZERO;
  nincx = n * incx;
  for (i = 0; i < nincx; i += incx) {
    dtemp += dx[i] * dx[i];
  }
  return(sqrt(dtemp));
}

/********************/
/* dscal            */
/********************/

/* scales a vector by a constant.
   jack dongarra, linpack, 3/11/78. */

int dscal(int n, double da, double *dx, int incx) {

  int   i, nincx;
  char  infunc[] = "dscal";

  if (n <= 0) {
    printf("ERROR (%s): n = %d < 0\n", infunc, n);
    return(ERR);
  }
  nincx = n*incx;
  for (i = 0; i < nincx; i += incx) {
    dx[i] *= da;
  }

  return(OK);
}


/********************/
/* dswap            */
/********************/

/* interchanges two vectors.
    jack dongarra, linpack, 3/11/78. */

int dswap(int n, double *dx, int incx, double *dy, int incy) {

  int     i, ix, iy;
  double  dtemp;
  char    infunc[] = "dswap";

  if (n <= 0) {
    printf("ERROR (%s): n = %d <= 0\n", infunc, n);
    return(ERR);
  }
  ix = 0;
  iy = 0;
  if (incx < 0) {
    ix = (-n+1)*incx;
  }
  if (incy < 0) {
    iy = (-n+1)*incy;
  }
  for (i = 0; i < n; i++) {
    dtemp = dx[ix];
    dx[ix] = dy[iy];
    dy[iy] = dtemp;
    ix += incx;
    iy += incy;
  }
  return(OK);
}

/********************/
/* idamax           */
/********************/

/* finds the index of element having max. absolute value.
   jack dongarra, linpack, 3/11/78. */

int idamax(int n, double *dx, int incx) {

  int     ind, i, ix;
  double  tmp, dmax;
  char    infunc[] = "idamax";

  if (n <= 0) {
    printf("ERROR (%s): n = %d <= 0\n", infunc, n);
    return(ERR);
  }

  ind = 0;
  if (n == 1) {
    return(ind);
  }
  ix = 0;
  dmax = fabs(dx[0]);
  ix += incx;
  for (i = 1; i < n; i++) {
    tmp = fabs(dx[ix]);
    if (tmp > dmax) {
      ind = i;
      dmax = tmp;
    }
    ix += incx;
  }

  return(ind);
}


/********************/
/* drotg            */
/********************/

/* construct givens plane rotation.
   jack dongarra, linpack, 3/11/78. */

/* NOTE: To emulate Fortran, this routine receives pointers */
int drotg(double *da_adr, double *db_adr, double *c_adr, double *s_adr) {

  double da, db, s, c;
  double scale, roe, z, r, ta, tb;
  /*  char   infunc[] = "drotg"; */

  da = *da_adr;
  db = *db_adr;
  c  = *c_adr;
  s  = *s_adr;

  roe = db;
  if (fabs(da) > fabs(db)) {
    roe = da;
  }
  scale = fabs(da) + fabs(db);
  if (scale == D_ZERO) {
    c = D_ONE;
    s = D_ZERO;
    r = D_ZERO;
  }
  else {
    ta = da/scale;
    tb = db/scale;
    r = scale*sqrt(ta*ta + tb*tb);
    r *= dsign(D_ONE, roe);
    c = da/r;
    s = db/r;
  }
  z = D_ONE;
  if (fabs(da) > fabs(db)) {
    z = s;
  }
  if ((fabs(db) >= fabs(da)) && (c != D_ZERO)) {
    z = D_ONE/c;
  }
  da = r;
  db = z;

  *da_adr = da;
  *db_adr = db;
  *c_adr  = c;
  *s_adr  = s;

  return(OK);
}


/********************/
/* drot             */
/********************/

/* applies a plane rotation.
   jack dongarra, linpack, 3/11/78. */

int drot(int n, double *dx, int incx, double *dy, int incy, double c, double s) {

  int    i, ix, iy;
  double dtemp;
  char   infunc[] = "drot";

  if (n < 0) {
    printf("ERROR (%s): n = %d < 0\n", infunc, n);
    return(ERR);
  }
  ix = 0;
  iy = 0;
  if (incx < 0) {
    ix = (-n+1)*incx;
  }
  if (incy < 0) {
    iy = (-n+1)*incy;
  }
  for (i = 0; i < n; i++) {
    dtemp  = c*dx[ix] + s*dy[iy];
    dy[iy] = c*dy[iy] - s*dx[ix];
    dx[ix] = dtemp;
    ix += incx;
    iy += incy;
  }

  return(OK);
}


