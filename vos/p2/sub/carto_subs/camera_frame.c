/*******************************************************************************
Title:    camera_frame.c
Author:   Mike Burl
Date:     2005/03/02

Function: Define wRc given the camera azimuth, zenith and roll angles in DEGREES.

Hisotry:  Factored out of estimate_wpp_camera.c

*******************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "camera_frame.h"
#include "burl.h"

/**************************************/
/* GLOBAL DECLARATIONS                */
/**************************************/

/**************************************/
/* camera_frame                       */
/**************************************/

/* Convert a specification for the camera orientation in terms of
   angles theta, phi, psi (in DEGREES!) to a (3 X 3) rotation matrix
   wRc. The COLUMNS of wRc express the camera system coordinate axes
   in the world coordinate frame. If you need cRw instead of wRc,
   note that the two are related by a transpose: cRw = wRc'. Observe that
   wRc must be preallocated as a 9-element array of doubles.
*/

int camera_frame(double theta, double phi, double psi, double *wRc)
{
  double  cth, sth;
  double  cphi, sphi;
  double  cpsi, spsi;
  /*  char    infunc[] = "camera_frame"; */

  /* Convert to radians */
  theta = theta * DEG2RAD;
  phi   = phi   * DEG2RAD;
  psi   = psi   * DEG2RAD;

  /* Precalculate */
  cth = cos(theta);
  sth = sin(theta);
  cphi = cos(phi);
  sphi = sin(phi);
  cpsi = cos(psi);
  spsi = sin(psi);

  /* Assign elements */
  wRc[0] = -sth*cpsi + cth*cphi*spsi;
  wRc[1] = spsi*sth+cth*cpsi*cphi;
  wRc[2] = -cth*sphi;
  wRc[3] = cth*cpsi+sth*cphi*spsi;
  wRc[4] = -cth*spsi+sth*cpsi*cphi;
  wRc[5] = -sth*sphi;
  wRc[6] = -spsi*sphi;
  wRc[7] = -sphi*cpsi;
  wRc[8] = -cphi;

  return(OK);
}
