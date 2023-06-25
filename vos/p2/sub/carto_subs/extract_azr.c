/*******************************************************************************
Title:    extract_azr.c
Author:   Mike Burl
Date:     2005/03/01

Function: Extract camera azimuth-zenith-roll angles from a 3x3 matrix wRc. Note 
            that columns of wRc express camera axes in the world coordinate system.
            See B-0106. For nadir looking camera, there is an ambiguity which
            entangles theta and psi. We set theta to zero and choose psi
            accordingly. See also camera_frame.c for the inverse of this function
            (take angles and produce rotation matrix).

History: 

*******************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "burl.h"
#include "extract_azr.h"

/**************************************/
/* GLOBAL DECLARATIONS                */
/**************************************/

#define EXTRACT_AZR_EPS (1e-07)

/**************************************/
/* extract_azr                        */
/**************************************/

int extract_azr(double *wRc, double *theta_adr, double *phi_adr, double *psi_adr)
{
  double cth, sth;
  double cphi, sphi;
  double cpsi, spsi;
  /*  char   infunc[] = "extract_azr"; */

  cphi = -wRc[8];
  sphi = sin(acos(cphi));

  if (fabs(sphi) > EXTRACT_AZR_EPS) {
    cpsi = -wRc[7]/sphi;
    spsi = -wRc[6]/sphi;
    cth = -wRc[2]/sphi;
    sth = -wRc[5]/sphi;

    *theta_adr = atan2(sth, cth) * RAD2DEG;
    *phi_adr   = atan2(sphi, cphi) * RAD2DEG;
    *psi_adr   = atan2(spsi, cpsi) * RAD2DEG;
  }
  else {
    /* Cannot disentangle theta and psi in this situation */
    *theta_adr = D_ZERO;
    if (cphi > 0) {
      *phi_adr = D_ZERO;      
      *psi_adr = atan2(wRc[0], wRc[3]) * RAD2DEG;    
    }
    else {
      *phi_adr = M_PI;
      *psi_adr = atan2(-wRc[0], wRc[3]) * RAD2DEG;    
    }

  }

  return(OK);
}
