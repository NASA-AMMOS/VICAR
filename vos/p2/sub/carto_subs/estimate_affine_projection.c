/*******************************************************************************
Title:    estimate_affine_projection.c
Author:   Mike Burl
Date:     2005/03/02

Function: Given 3D to 2D correspondences, estimate the affine projection
            matrix from 3D to 2D. M must be allocated to have 2*4 double
            elements. Note that the 3D points should not all lie in the
            same plane (degnerate configuration). See B-0105.

History: 2005/03/09 (MCB) - added freeing of G. 

*******************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "estimate_affine_projection.h"
#include "least_squares.h"
#include "burl.h"
#include "qmalloc.h"

/**************************************/
/* GLOBAL DECLARATIONS                */
/**************************************/

/**************************************/
/* estimate_affine_projection         */
/**************************************/
/* xyz should be preallocated to be (n X 3) */
/* uv  should be preallocated to be (n X 2) */
/* P   should be preallocated to be (2 X 4) */


int estimate_affine_projection(int n, double *xyz, double *uv, double *P)
{
  int     i, j, k;
  double  *G;
  char    infunc[] = "estimate_affine_projection";


  G = (double *) qmalloc(2*n * 8, sizeof(double), 1, infunc, "G");

  for (i = 0, j = 0, k = 0; i < n; i++, j+=3, k+=16) {
    G[k]    = xyz[j];
    G[k+1]  = xyz[j+1];
    G[k+2]  = xyz[j+2];
    G[k+3]  = D_ONE;
    G[k+12] = xyz[j];
    G[k+13] = xyz[j+1];
    G[k+14] = xyz[j+2];
    G[k+15] = D_ONE;
  }

  least_squares(2*n, 8, G, uv, P);

  free((void *) G);
  return(OK);
}
