/*******************************************************************************
Title:    estimate_wpp_camera.c
Author:   Mike Burl
Date:     2005/01/03

Function: Estimate parameters in a weak perspective camera model. 
            INput and output angles are in DEGREES!
 
            The function recover_wpp_camera() takes a (2 X 4) affine projection
            matrix and backs out the physical parameters in the weak perspective
            camera model.

            The function estimate_wpp_camera() takes metric world
            coordinates (ENU) of four image corners, image width
            and height in pixels and the camera_zenith and camera_roll
            angles to determine the paramaeters

NOTE:      corners is a (4 X 3) matrix.

*******************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "estimate_wpp_camera.h"
#include "camera_frame.h"
#include "extract_azr.h"
#include "qmalloc.h"
#include "burl.h"

/**************************************/
/* GLOBAL DECLARATIONS                */
/**************************************/
#define EPS (1e-07)

/**************************************/
/* recover_wpp_camera                 */
/**************************************/

int recover_wpp_camera(double *P, double *m_adr, double *q_adr, double *tau_adr, 
      double *theta_adr,  double *phi_adr, double *psi_adr, double *bu_adr, double *bv_adr)
{ 
  int      j;
  double   m1[3], d[3], nu[3], r1[3];
  double   wRc[3*3];
  double   norm_m2, norm_nu, dTm1;

  *bu_adr = P[3];
  *bv_adr = P[7];

  for (j = 0; j < 3; j++) {
    m1[j] = P[j];
  }

  norm_m2 = sqrt(P[4]*P[4] + P[5]*P[5] + P[6]*P[6]);
  for (j = 0; j < 3; j++) {
    d[j] = P[4+j]/norm_m2;
  }

  dTm1 = d[0]*m1[0]+d[1]*m1[1] + d[2]*m1[2];
  for (j = 0; j < 3; j++) {
    nu[j] = m1[j] - dTm1 * d[j];
  }
  norm_nu = sqrt(nu[0]*nu[0] + nu[1]*nu[1] + nu[2]*nu[2]);
  *m_adr = norm_nu;

  for (j = 0; j < 3; j++) {
    r1[j] = nu[j]/norm_nu;
  }

  wRc[0] = r1[0];
  wRc[3] = r1[1];
  wRc[6] = r1[2];
  wRc[1] = d[0];
  wRc[4] = d[1];
  wRc[7] = d[2];
  wRc[2] = r1[1]*d[2] - r1[2] * d[1];
  wRc[5] = r1[2]*d[0] - r1[0] * d[2];
  wRc[8] = r1[0]*d[1] - r1[1] * d[0];

  extract_azr(wRc, theta_adr, phi_adr, psi_adr);

  *tau_adr = norm_m2/norm_nu;
  *q_adr = dTm1/norm_nu;
    
  return(OK);
}

/**************************************/
/* estimate_wpp_camera                */
/**************************************/

int estimate_wpp_camera(double phi0, double psi0, double *corners, int imwidth,
      int imheight, double *m_adr, double *q_adr, double *tau_adr, double *theta_adr, 
      double *phi_adr, double *psi_adr, double *bu_adr, double *bv_adr)
{ 
  double         m, q, tau;
  double         phi, psi, theta;
  double         bu, bv;
  double         GSD1;
  double         GSD2;
  double         rad_theta, rad_phi, rad_psi;
  double         ctheta, stheta, cphi, sphi, cpsi, spsi;
  double         wfac, hfac, tfac, mfac, detfac;
  double         P10x, P10y, P01x, P01y;
  double         A11, A12, A21, A22;
  double         tvec1, tvec2;
  double         qtauvec1, qtauvec2;
  /*  char           infunc[] = "estimate_wpp_camera"; */
  /*--------------------------------------------------------------*/
  wfac = D_ONE / ((double) imwidth);
  hfac = D_ONE / ((double) imheight);

  phi = phi0;
  psi = psi0;
  rad_phi = phi * DEG2RAD;
  rad_psi = psi * DEG2RAD;
  cphi = cos(rad_phi);
  sphi = sin(rad_phi);
  cpsi = cos(rad_psi);
  spsi = sin(rad_psi);

  P10x = (corners[3]-corners[0])*wfac;
  P10y = (corners[4]-corners[1])*hfac;
  P01x = (corners[9]-corners[0])*wfac;
  P01y = (corners[10]-corners[1])*hfac;

  /* Estimate GSD in across direction */
  GSD1 = sqrt(P10x*P10x + P10y*P10y);

  /* Estimate GSD in down direction */
  GSD2 = sqrt(P01x*P01x + P01y*P01y);
  printf("GSD1 = %lf, GSD2 = %lf, GSD = %lf\n", GSD1, GSD2, sqrt(GSD1 * GSD2));

  m = sqrt(cpsi*cpsi + (spsi*spsi)/(cphi*cphi))/GSD1;
  tfac  = (D_ONE/(m * GSD2*GSD2));
  tvec1 = tfac * (P10x * spsi/cphi + P10y * cpsi);
  tvec2 = tfac * (-P10x * cpsi + P10y * spsi/cphi); 
  rad_theta = atan2(tvec2, tvec1);
  theta = rad_theta * RAD2DEG;
  ctheta = cos(rad_theta);
  stheta = sin(rad_theta);

  mfac = D_ONE/m;
  A11 = mfac * (stheta*cpsi - ctheta*spsi/cphi);
  A12 = mfac * (stheta*spsi + ctheta*cpsi/cphi);
  A21 = mfac * (-ctheta*cpsi - stheta*spsi/cphi);
  A22 = mfac * (-ctheta*spsi + stheta*cpsi/cphi);

  detfac = D_ONE/(A11*A22-A21*A12);
  qtauvec1 = detfac * (A22 * P01x - A12 * P01y);
  qtauvec2 = detfac * (-A21 * P01x + A11 * P01y);

  tau = D_ONE/qtauvec2;
  q   = tau * qtauvec1;
  bu = ((double) imwidth)/D_TWO;
  bv = ((double) imheight)/D_TWO;
  theta = rad_theta * RAD2DEG;

  *m_adr = m;
  *q_adr = q;
  *tau_adr = tau;
  *theta_adr = theta;
  *phi_adr = phi;
  *psi_adr = psi;
  *bu_adr = bu;
  *bv_adr = bv;

  return(OK);
}

/**************************************/
/* wpp_projection_matrix              */
/**************************************/

/* Given the various paraemters in the weak-perspective camera model,
   return the corresponding 4 X 4 projection matrix M, which should
   be preallocated as a 16 element array of doubles */

int wpp_projection_matrix(double theta, double phi, double psi, 
			  double m, double q, double tau, double bu,
                          double bv, double *M)
{ 
  double         wRc[9];
  double         mq;
  double         mtau;
  int            i;
  /*  char           infunc[] = "wpp_projection_matrix"; */

  /*--------------------------------------------------------------*/
  
  camera_frame(theta, phi, psi, wRc);
  mq = m*q;
  mtau = m*tau;

  M[0] = m*wRc[0] + mq * wRc[1];
  M[1] = m*wRc[3] + mq * wRc[4];
  M[2] = m*wRc[6] + mq * wRc[7];
  M[3] = bu;
  M[4] = mtau * wRc[1];
  M[5] = mtau * wRc[4];
  M[6] = mtau * wRc[7];
  M[7] = bv;
  for (i = 8; i <= 14; i++) {
    M[i] = D_ZERO;
  }
  M[15] = D_ONE;

  return(OK);
}

