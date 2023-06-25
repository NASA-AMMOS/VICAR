/*******************************************************************************

  Title:    least_squares
  Author:   Mike Burl 
  Function: Compute least squares solution to Mx = b, where M and
            b are given and x is unknown. Return RMS error.
            x must be preallocated to proper size (mc x 1) 
           
*******************************************************************************/
#include <stdio.h>
#include <math.h>
#include "least_squares.h"
#include "qmalloc.h"
#include "pinv.h"
#include "burl.h"

double least_squares(int mr, int mc, double *M, double *b, double *x)
{
  int    ar, ac;
  int    i, j;
  double *A;
  double *bhat;
  double e;
  char   infunc[] = "least_squares";

  ar = mc;
  ac = mr;
  A = (double *) qmalloc(ar*ac, sizeof(double), 0, infunc, "A");  
  pinv(mr, mc, M, A);

  for (i = 0; i < ar; i++) {
    x[i] = D_ZERO;
    for (j = 0; j < ac; j++) {
      x[i] += A[i*ac+j] * b[j];
    }
  }

  bhat = (double *) qmalloc(mr, sizeof(double), 0, infunc, "bhat");
  e = D_ZERO;
  for (i = 0; i < mr; i++) {
    bhat[i] = D_ZERO;
    for (j = 0; j < mc; j++) {
      bhat[i] += M[i*mc+j] * x[j];
    }
    e += (bhat[i] - b[i]) * (bhat[i] - b[i]);
  }

  free((void *) A);
  free((void *) bhat);

  return(sqrt(e));
}

