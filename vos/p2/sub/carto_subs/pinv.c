/*******************************************************************************

  Title:    pinv
  Author:   Mike Burl 
  Function: Compute psuedoinverse of a matrix M.
            Allocate A as (mc x mr) (dimensions equal to M')

            M = USV'
            A = V * pinv(S) * U';

*******************************************************************************/
#include <stdio.h>
#include <math.h>
#include "pinv.h"
#include "csvd.h"
#include "qmalloc.h"
#include "burl.h"

#define  EPS  1e-08;

int pinv(int mr, int mc, double *M, double *A)
{
  int    i, j, k, ind;
  int    ur, uc, sr, vr, vc, ar, ac;
  double *U, *S, *V;
  double Mnorm, thr;
  double s;
  char   infunc[] = "pinv";

  ur = mr;
  uc = MIN(mr,mc);
  sr = MIN(mr,mc);
  vr = mc;
  vc = mc;
  mc = mc;

  U = (double *) qmalloc(ur*uc, sizeof(double), 0, infunc, "U");
  S = (double *) qmalloc(sr, sizeof(double), 0, infunc, "S");
  V = (double *) qmalloc(vr*vc, sizeof(double), 0, infunc, "V");
  csvd(M, mr, mc, U, S, V);
  Mnorm = S[0];  /* Maximum singular value */
  thr = Mnorm * MAX(mr, mc) * EPS;

  ar = mc;
  ac = mr;
  for (i = 0; i < ar; i++) {
    for (j = 0; j < ac; j++) {
      ind = i*ac+j;
      A[ind] = D_ZERO;
      for (k = 0; k < sr; k++) {
        s = S[k];
        if (s > thr) {
          A[ind] += V[i*vc+k] * 1/s * U[j*uc+k];
	} 
      }
    }
  }

  return(OK);
}

