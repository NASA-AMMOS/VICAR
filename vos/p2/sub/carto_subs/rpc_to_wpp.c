/*******************************************************************************
Title:    rpc_to_wpp.c
Author:   Mike Burl
Date:     2006/10/13

Function: Recover a weak perspective projection camera model from an rpc camera
            model. This function is similar to safire_metadata.c except that the
            wpp model here is specified relative to the standard ECEF coordinate 
            system.

History:  
*******************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <strings.h>
#include "qmalloc.h"
#include "burl.h"
#include "rpc.h"
#include "io_view.h"
#include "rpc_to_wpp.h"
#include "camera_frame.h"
#include "quaternion.h"
#include "earth_coordinates.h"
#include "estimate_wpp_camera.h"
#include "estimate_affine_projection.h"

#define   EPS 1e-07

/**************************************/
/* GLOBAL DECLARATIONS                */
/**************************************/

/* helper/debugging function that we dont need to expose externally */
int print_plh(int n_plh, double *PLH);

/**************************************/
/* rpc_to_wpp                         */
/**************************************/

/* Note: PLH is (n_plh X 3) with n_plh typically equal to 2.        */
/* The first row would have the minimum values of lat-lon-height    */
/* for the volume of interest, while the second row would have      */
/* the maximum values. If the range of values is too small along    */
/* any dimension, then a slightly bigger volume will be constructed */
/* in create_ecef_correspondences.                                  */

int rpc_to_wpp(int n_plh, double *PLH, int rpc_version, double *R, 
               double *m_adr, double *q_adr, double *tau_adr, 
               double *theta_adr, double *phi_adr, double *psi_adr, 
               double *bu_adr, double *bv_adr)

{
  int                    n_grid;
  double                 *grid_xyz, *grid_uv;
  double                 *P;
  /*  int                    i, j; */
  char                   infunc[] = "rpc_to_wpp";

  /*--------------------------------------------------------------*/
  if (rpc_version == RPC_VERSION_UNDEFINED) {
    fprintf(stderr, "ERROR (%s): cannot work with undefined RPC version\n", infunc);
    return(ERR); 
  }
  /* Use RPC to create grid of ECEF XYZ points covering volume of interest */
  create_ecef_correspondences(n_plh, PLH, rpc_version, R, &n_grid, &grid_xyz, &grid_uv);

  /* Set camera stuff */
  P = (double *) qmalloc(2*4, sizeof(double), 1, infunc, "P");
  estimate_affine_projection(n_grid, grid_xyz, grid_uv, P);

  recover_wpp_camera(P, m_adr, q_adr, tau_adr, theta_adr, phi_adr, psi_adr, bu_adr, bv_adr);

  free((void *) grid_xyz);
  free((void *) grid_uv);
  free((void *) P);

  return(OK);
}


/**************************************/
/* wpp_attitude_quaternion            */
/**************************************/
/* NOTE: theta, phi, and psi are in DEGREES! quat should be preallocated as (4 X 1) */

int wpp_attitude_quaternion(double theta, double phi, double psi, double *quat)
{
  double  wRc[3*3];

  camera_frame(theta, phi, psi, wRc);
  mat_to_quaternion(wRc, quat);

  return(OK);
}

/**************************************/
/* wpp_position_quaternion            */
/**************************************/

int wpp_position_quaternion(double alt, double u0, double v0, 
    double m, double q, double tau, double theta, double phi, double psi, 
    double bu, double bv, double *quat)
{
  ellipsoid_struct *ellipsoid;
  double           wRc[3*3];
  double           g0, g1;
  double           v[3], P[3], plh[3];
  double           ak, bk, ck;
  double           fak, fbk, fck;
  char             infunc[]= "wpp_position_quaternion";

  define_ellipsoid("WGS1984", &ellipsoid);
  camera_frame(theta, phi, psi, wRc);

  g0 = ((u0-bu) - q*(v0-bv)/tau)/m;
  g1 = (v0-bv)/m/tau;

  v[0] = wRc[0]*g0 + wRc[1]*g1;
  v[1] = wRc[3]*g0 + wRc[4]*g1;
  v[2] = wRc[6]*g0 + wRc[7]*g1;

  /* Pick a too low z value */
  ak = ellipsoid->a;
  P[0] = v[0] - ak * wRc[2];
  P[1] = v[1] - ak * wRc[5];
  P[2] = v[2] - ak * wRc[8];
  geodetic_from_ecef(plh, 1, P);
  fak = plh[2]-alt;

  /* Pick a too high z value */
  ck = 15.0*ellipsoid->a;
  P[0] = v[0] - ck * wRc[2];
  P[1] = v[1] - ck * wRc[5];
  P[2] = v[2] - ck * wRc[8];
  geodetic_from_ecef(plh, 1, P);
  fck = plh[2]-alt;

  if ( ((fak > 0) && (fck > 0)) || ((fak < 0) && (fck < 0)) ) {
    fprintf(stderr, "ERROR (%s): cannot solve by bisection method\n", infunc);
    return(ERR);
  }

  while ((ck-ak) > EPS) {
    /* Compute bisection point */
    bk = (ak + ck)/D_TWO;

    /* Compute value at bisection point */
    P[0] = v[0] - bk * wRc[2];
    P[1] = v[1] - bk * wRc[5];
    P[2] = v[2] - bk * wRc[8];
    geodetic_from_ecef(plh, 1, P);
    fbk = plh[2]-alt;

    if ( ((fak > 0) && (fbk > 0)) || ((fak < 0) && (fbk < 0)) ) {
      ak = bk;
      fak = fbk;
    }
    else {
      ck = bk;
      fck = fbk;
    }
  } 

  bk = (ak + ck)/D_TWO;
  quat[0] = D_ZERO;
  quat[1] = v[0] - bk * wRc[2];
  quat[2] = v[1] - bk * wRc[5];
  quat[3] = v[2] - bk * wRc[8];

  free_ellipsoid(ellipsoid);
  return(OK);
}

/**************************************/
/* create_ecef_correspondences        */
/**************************************/

int create_ecef_correspondences(int n_plh, double *PLH, int rpc_version, double *R, int *n_grid_adr, double **grid_xyz_adr, double **grid_uv_adr)
{
  int    i, j, k, l, m, n;
  int    g, gg, n_grid;
  double val, lat, lon, hgt;
  double *grid_plh, *grid_uv;
  double PLH_min[3], PLH_max[3];
  double *grid_xyz;
  double midpt[3], range[3], delta[3], min_range[3];
  char   infunc[] = "create_ecef_correspondences";

  /* Insure that the volume is not degenerate */
  min_range[0] = 8e-03; /* roughly +/- 400meters */
  min_range[1] = 8e-03; /* roughly +/- 400meters */
  min_range[2] = 800;   /* +/- 400meters */

  g = 7;
  gg = (g-1)/2;
  n_grid = g*g*g;
  *n_grid_adr = n_grid;

  /* Determine bounding volume for PLH */
  for (j = 0; j < 3; j++) {
    PLH_max[j] = PLH[j];
    PLH_min[j] = PLH[j];
  }
  for (i = 0; i < n_plh; i++) {
    for (j = 0; j < 3; j++) {
      val = PLH[i*3+j];
      if (val > PLH_max[j]) {
        PLH_max[j] = val;
      }
      if (val < PLH_min[j]) {
        PLH_min[j] = val;
      }
    }
  }
  
  /* Verify that the volume is sufficient */
  for (j = 0; j < 3;  j++) {
    range[j] = MAX((PLH_max[j] - PLH_min[j]), min_range[j]);
    midpt[j] = (PLH_max[j] + PLH_min[j])/D_TWO;
    delta[j] = range[j]/((double) g- D_ONE);
  }
  
  grid_plh = (double *) qmalloc(n_grid*3, sizeof(double), 0, infunc, "grid_plh");
  n = 0;
  for (k = -gg; k <= gg; k++) {
    lat = midpt[0] + k * delta[0];
    for (l = -gg; l <= gg; l++) {
      lon = midpt[1] + l * delta[1];
      for (m = -gg; m <= gg; m++) {
        hgt = midpt[2] + m * delta[2];
        grid_plh[n*3] = lat;
        grid_plh[n*3+1] = lon;
        grid_plh[n*3+2] = hgt;
        n++;
      }
    }
  }

  /* Transform geodetic lat-lon-height coords to metric ECEF coords */
  *grid_xyz_adr = grid_xyz = (double *) qmalloc(n_grid*3, sizeof(double), 0, infunc, "grid_xyz");
  ecef_from_geodetic(grid_xyz, n_grid, grid_plh);

  /* Also, Transform geodetic lat-lon-height coords to image plane u-v coords */
  grid_uv = (double *) qmalloc(n_grid*2, sizeof(double), 0, infunc, "grid_uv");
  *grid_uv_adr = grid_uv;
  rpc_forward(rpc_version, R, n_grid, grid_plh, grid_uv);

  free((void *) grid_plh);

  return(OK);
}


/**************************************/
/* print_plh                          */
/**************************************/
/* Minor utility for debugging */

int print_plh(int n_plh, double *PLH)
{
  int   i, j;
  /*  char  infunc[] = "print_plh"; */

  printf("\n");
  printf("PLH:\n");
  for (i = 0; i < n_plh; i++) {
    for (j = 0; j < 3; j++) {
      printf("%lf ", PLH[i*3+j]);
    }
    printf("\n");
  }
  printf("\n");

  return(OK);
}
