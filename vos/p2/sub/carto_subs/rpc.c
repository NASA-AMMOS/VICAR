/*******************************************************************************
Title:    rpc.c
Author:   Mike Burl
Date:     2005/03/02

Function: A couple of functions for working with the RPC (rational polynomial camera)
           model.

History:  2005/03/11 (MCB) - modified to work with different RPC versions. Moved
            macro definitions into rpc.h

*******************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "rpc.h"
#include "burl.h"
#include "qmalloc.h"

/**************************************/
/* GLOBAL DECLARATIONS                */
/**************************************/

/**************************************/
/* rpc_forward                        */
/**************************************/
/* llh is a (n X 3) array containing the lat, lon, and height */
/* uv  is a (n X 2) array containing the corresponding SAMPLE (u) and LINE(v) */
/* There may be some half-pixel issues with uv that I haven't tried to resolve. */

int rpc_forward(int rpc_version, double *R, int n, double *llh, double *uv)
{
  int     i, ii;
  double  *plh;
  char    infunc[] = "rpc_forward";

  /* Convert llh to rescaled plh */
  plh = (double *) qmalloc(n*3, sizeof(double), 1, infunc, "plh");
  for (i = 0, ii= 0; i < n; i++, ii+=3) {
    plh[ii] = (llh[ii] - R[RPC_LAT_OFFSET]) / R[RPC_LAT_SCALE];
    plh[ii+1] = (llh[ii+1] - R[RPC_LON_OFFSET]) / R[RPC_LON_SCALE];
    plh[ii+2] = (llh[ii+2] - R[RPC_HGT_OFFSET]) / R[RPC_HGT_SCALE];
  }

  /* Apply rpc_n_forward */
  rpc_n_forward(rpc_version, R, n, plh, uv);

  /* Rescale the uv's */
  for (i = 0, ii = 0; i < n; i++, ii+=2) {
    uv[ii]   = R[RPC_SAMPLE_OFFSET] + uv[ii] * R[RPC_SAMPLE_SCALE];
    uv[ii+1] = R[RPC_LINE_OFFSET] + uv[ii+1] * R[RPC_LINE_SCALE];
  }

  free((void *) plh);

  return(OK);
}

/**************************************/
/* rpc_n_forward                      */
/**************************************/
/* plh is a (n x 3) double array containing the "normalized" lat, lon, and heights */
/* uv is a (n x 2) double array containing the "normalized" SAMPLE (u) and LINE (v) */

int rpc_n_forward(int rpc_version, double *R, int n, double *plh, double *uv)
{
  int     i, j, ii, kk;
  double  P, L, H;
  double  T[20];
  double  NL, DL, NS, DS;
  char    infunc[] = "rpc_n_forward";

  for (i = 0, ii = 0, kk= 0; i < n; i++, ii+=3, kk+=2) {
    P = plh[ii];
    L = plh[ii+1];
    H = plh[ii+2];
    if (rpc_version == RPC_VERSION_RPC00A) {
      T[0] = D_ONE;
      T[1] = L;
      T[2] = P;
      T[3] = H;
      T[4] = L*P;
      T[5] = L*H;
      T[6] = P*H;
      T[7] = L*L;
      T[8] = P*P;
      T[9] = H*H;
      T[10] = P*L*H;
      T[11] = L*L*L;
      T[12] = L*P*P;
      T[13] = L*H*H;
      T[14] = L*L*P;
      T[15] = P*P*P;
      T[16] = P*H*H;
      T[17] = L*L*H;
      T[18] = P*P*H;
      T[19] = H*H*H;
    }
    else if (rpc_version == RPC_VERSION_RPC00B) {
      T[0] = D_ONE;
      T[1] = L;
      T[2] = P;
      T[3] = H;
      T[4] = L*P;
      T[5] = L*H;
      T[6] = P*H;
      T[7] = P*L*H;
      T[8] = L*L;
      T[9] = P*P;
      T[10] = H*H;
      T[11] = L*L*L;
      T[12] = L*L*P;
      T[13] = L*L*H;
      T[14] = L*P*P;
      T[15] = P*P*P;
      T[16] = P*P*H;
      T[17] = L*H*H;
      T[18] = P*H*H;
      T[19] = H*H*H;
    }
    else {
      fprintf(stderr, "ERROR (%s): unsupported rpc_version = %d\n", infunc, rpc_version);
      return(ERR);
    }

    NS = D_ZERO;
    for (j = 0; j < 20; j++) {
      NS += R[RPC_NS_START+j] * T[j];
    }

    DS = D_ZERO;
    for (j = 0; j < 20; j++) {
      DS += R[RPC_DS_START+j] * T[j];
    }
    if (DS == D_ZERO) {
      fprintf(stderr, "ERROR (%s): DS = 0\n", infunc);
      return(ERR);
    }

    NL = D_ZERO;
    for (j = 0; j < 20; j++) {
      NL += R[RPC_NL_START+j] * T[j];
    }

    DL = D_ZERO;
    for (j = 0; j < 20; j++) {
      DL += R[RPC_DL_START+j] * T[j];
    }
    if (DL == D_ZERO) {
      fprintf(stderr, "ERROR (%s): DL = 0\n", infunc);
      return(ERR);
    }

    uv[kk] = NS/DS;
    uv[kk+1] = NL/DL;
  }

  return(OK);
}
