/*******************************************************************************

  Title:     csvd
  Author:    Mike Burl (translated from FORTRAN)
  Date:      Nov 17, 1998
  Function:  C version of the linpack dsvdc routine.

  History:   2003/07/03 (MCB) - removed some macro definitions that appear 
               in burl.h

  NOTES:     The FORTRAN routine dsvdc.f from which csvd.c was
               derived seems to require that V be dimensioned
               as p X p where p is the number of columns in X.
               However, V really only needs to be p X min(n,p). Thus, 
               this routine is memory-inefficient when p is large. 
               Changing the behavior may be possible but I haven't 
               looked closely enough at the FORTRAN code.

             Comments from the original FORTRAN routine are preserved 
               at the end of the C code

             The translation performed was fairly "vanilla", so the resulting
               C code may not be the most efficient possible. 

             Unlike the FROTRAN routine, we create a copy of the incoming 
               array so that it is not destroyed in the caller.

*******************************************************************************/
#include <stdio.h>
#include <math.h>
#include "cblas.h"
#include "burl.h"
#include "csvd.h"
#include "qmalloc.h"
/**************/
/* CSVD_SIZES */
/**************/

int csvd_sizes(int xr, int xc, int *ur_adr, int *uc_adr, int *sr_adr, int *sc_adr, int *vr_adr, int *vc_adr)
{
  *ur_adr = xr;
  *uc_adr = MIN(xr, xc);
  *sr_adr = MIN(xr, xc);
  *sc_adr = 1;
  *vr_adr = xc;
  *vc_adr = xc;

  return(OK);
}

/**************/
/* CSVD_ALLOC */
/**************/
int csvd_alloc(int n, int p, double **U_adr, double **S_adr, double **V_adr) {
/*
   Allocate space for the matrices needed to do SVD
   u  allocate as n x min(n,p)
   s  allocate as min(n,p)
   v  allocate as p x p
*/ 
  int   sr;
  char  infunc[] = "csvd_alloc";

  sr = MIN(n,p);
  *U_adr = (double *) qmalloc(n*sr, sizeof(double), 0, infunc, "*U_adr");
  *S_adr = (double *) qmalloc(sr, sizeof(double), 0, infunc, "*S_adr");
  *V_adr = (double *) qmalloc(p*p, sizeof(double), 0, infunc, "*V_adr");

  return(OK);
}

/**************/
/* CSVD       */
/**************/
int csvd(double *xx, int n, int p, double *u, double *s, double *v) { 

/*
   xx  is an n x p; the others are allocated as in csvd_alloc
*/

  int      i, iter, j, k, kase, r, rr, rm1, rp1, ru, m;
  int      maxit, mm, mm1, nct, nctp1, nrt, nrtp1, h;
  double   t;
  double   b, c, cs, ecr, emm1, f, g, scale, shift, scr, sm, sn;
  double   smm1, t1, test, ztest;
  int      xr, xc;
  int      ur, uc;
  int      vr, vc;
  int      info, rj;
  double   *x, *e, *work, *w, *ind;
  char     infunc[]  = "csvd";

/***********************************************************************/
  /* Copy xx into a temporary workspace array x */
  x = (double *) qmalloc(n*p, sizeof(double), 0, infunc, "xx");
  for (i = 0; i < n; i++) {
    for (j = 0; j < p; j++) {
      x[i*p+j] = xx[i*p+j];
    }
  }
  e    = (double *) qmalloc(p, sizeof(double), 1, infunc, "e");
  work = (double *) qmalloc(n, sizeof(double), 1, infunc, "work");

  /* FORTRAN routine wanted s to be allocated as MIN(n+1,p), with only
  the first MIN(n,p) entries being relevant. I thought this would be
  confusing to anyone calling the routine, so I internally allocate w
  to act like s, and at the end copy the relevant elements of w into s */
  w   = (double *) qmalloc(MIN(n+1,p), sizeof(double), 1, infunc, "w");

  /* set the maximum number of iterations. */
  maxit = 30;

  /* dimensions of the various arrays */
  xr = n;
  xc = p;
  ur = n;
  uc = MIN(n,p);
  vr = p;
  /*  vc = uc; */
  vc = p;
  
  /* Reduce x to bidiagonal form, storing the diagonal elements
     in w and the super-diagonal elements in e. */
  info = 0;
  nct = MIN(n-1, p);
  nrt = MAX(0, MIN(p-2,n));
  ru = MAX(nct, nrt);

  for (r = 1; r <= ru; r++) {
    rp1 = r+1;
    rm1 = r-1;
    rr = rm1*xc + rm1; /* index to x(r,r) */
    if (r <= nct) {
      /* compute the transformation for the r-th column and
	 place the r-th diagonal in w[r]. */
      w[rm1] = dnrm2(n-r+1,x+rr,xc);
      if (w[rm1] != D_ZERO) {
	if (x[rr] != D_ZERO) {
	  w[rm1] = dsign(w[rm1],x[rr]);
	}
	dscal(n-r+1,D_ONE/w[rm1],x+rr,xc);
	x[rr] = D_ONE + x[rr];
      } /* line 10 */
      w[rm1] = -w[rm1];
    } /* line 20 */

    for (j = rp1; j <= xc; j++) {
      rj = rm1*xc + (j-1);
      if (r <= nct) {
	if (w[rm1] != D_ZERO) {
	  /* apply the transformation. */
	  t = -ddot(n-r+1,x+rr,xc,x+rj,xc)/x[rr];
	  daxpy(n-r+1,t,x+rr,xc,x+rj,xc);
	}
      } /* line 30 */

      /* place the r-th row of x into  e for the
	 subsequent calculation of the row transformation. */
      e[j-1] = x[rj];
    } /* line 40 */

    if (r <= nct) {
      /* place the transformation in u for subsequent back
	 multiplication. */
      for (i = r; i <= n; i++) {
	u[(i-1)*uc+rm1] = x[(i-1)*xc+rm1];
      } /* line 60 */
    } /* line 70 */

    if (r <= nrt) {
      /* compute the r-th row transformation and place the
	 r-th super-diagonal in e[r]. */
      e[rm1] = dnrm2(p-r,e+r,1);
      if (e[rm1] != D_ZERO) {
	if (e[r] != D_ZERO) {
	  e[rm1] = dsign(e[rm1],e[r]);
	}
	dscal(p-r,D_ONE/e[rm1],e+r,1);
	e[r] = D_ONE + e[r];
      } /* line 80 */
      e[rm1] = -e[rm1];
      if ((r < n) && (e[rm1] != D_ZERO)) {
	/* apply the transformation. */
	for (i = rp1; i <= n; i++) {
	  work[i-1] = D_ZERO;
	} /* line 90 */
	for (j = rp1, ind = x+r*xc+r; j <= xc; j++, ind++) {
	  daxpy(n-r,e[j-1],ind,xc,work+r,1);
	} /* line 100 */
	for (j = rp1, ind = x+r*xc+r; j <= xc; j++, ind++) {
	  daxpy(n-r,-e[j-1]/e[r],work+r,1,ind,xc);
	} /* line 110 */
      } /* line 120 */

      /* place the transformation in v for subsequent
	 back multiplication. */
      for (i = rp1; i <= vr; i++) {
	v[(i-1)*vc+rm1] = e[i-1];
      } /* line 130 */
    } /* line 150 */
  } /* line 160 */

  /* set up the final bidiagonal matrix or order m. */
  m = MIN(p,n+1);
  nctp1 = nct + 1;
  nrtp1 = nrt + 1;
  if (nct < p) {
    w[nct] = x[nct*xc+nct];
  }
  if (n < m) {
    w[m-1] = D_ZERO;
  }
  if (nrtp1 < m) {
    e[nrt] = x[nrt*xc+(m-1)];
  }
  e[m-1] = D_ZERO;

  /* generate u. */
  for (j = nctp1; j <= uc; j++) {
    for (i = 1; i <= n; i++) {
      u[(i-1)*uc+(j-1)] = D_ZERO;
    } /* line 180 */
    u[(j-1)*uc+(j-1)] = D_ONE;
  } /* line 190 */

  for (r = nct; r >= 1; r--) {
    rm1 = r-1;
    rp1 = r+1;
    if (w[rm1] != D_ZERO) {
      rr = rm1*uc+rm1; /* index of u(r,r) */
      for (j = rp1; j <= uc; j++) {
	rj = rm1*uc+(j-1);
	t = -ddot(n-r+1,u+rr,uc,u+rj,uc)/u[rr];
	daxpy(n-r+1,t,u+rr,uc,u+rj,uc);
      } /* line 210 */
      dscal(n-r+1,-D_ONE,u+rr,uc);
      u[rr] = D_ONE + u[rr];
      for (i = 1; i <= rm1; i++) {
	u[(i-1)*uc+rm1] = D_ZERO;
      } /* line 230 */
    } /* line 250 */
    else {
      for (i = 1; i <= n; i++) {
	u[(i-1)*uc+rm1] = D_ZERO;
      } /* line 260 */
      u[rm1*uc+rm1] = D_ONE;
    } /* line 270 */
  } /* line 280 */

  /* generate v. */
  for (r = vr; r >= 1; r--) {
    rm1 = r-1;
    rp1 = r+1;
    rr = rm1*vc+rm1; /* index of v(r,r) */
    if (r <= nrt) {
      if (e[rm1] != D_ZERO) {
        for (j = rp1; j <= vc; j++) {
          t = -ddot(vr-r,v+r*vc+rm1,vc,v+r*vc+(j-1),vc)/v[r*vc+rm1];
          daxpy(vr-r,t,v+r*vc+rm1,vc,v+r*vc+(j-1),vc);
        } /* line 310 */
      }
    } /* line 320 */
    for (i = 1; i <= vr; i++) {
      v[(i-1)*vc+rm1] = D_ZERO;
    }
    v[rm1*vc+rm1] = D_ONE;
  } /* line 340 */

  /*----------------------------------------------------------*/

  /* main iteration loop for the singular values. */
  mm = m;
  iter = 0;
  /* line 360 */
  while ((m > 0) && (iter <= maxit)) {
    /* while not all singular values have been found. */
    /* This section of the program inspects for
       negligible elements in the w and e arrays.  On
       completion the variables kase and r are set as follows:

       kase = 1     if w[m] and e[r-1] are negligible and r<m
       kase = 2     if w[r] is negligible and r<m
       kase = 3     if e[r-1] is negligible, r<m, and
                    w[r], ..., w[m] are not negligible (qr step).
       kase = 4     if e[m-1] is negligible (convergence). */

    for (r = m-1; r >= 1; r--) {
      test = fabs(w[r-1]) + fabs(w[r]);
      ztest = test + fabs(e[r-1]);
      if (ztest == test) {
        e[r-1] = D_ZERO;
        break;
      }
    } /* line 390 */

    if (r != (m-1)) {
      for (h = m; h > r; h--) {
        test = D_ZERO;
        if (h != m) {
          test += fabs(e[h-1]);
        }
        if (h != (r+1)) {
          test += fabs(e[h-2]);
        }
        ztest = test + fabs(w[h-1]);
        if (ztest == test) {
          w[h-1] = D_ZERO;
          break;
        }
      } /* line 430 */

      if (h == r) {
        kase = 3;
      }
      else if (h == m) {
        kase = 1;
      }
      else {
        kase = 2;
        r = h;
      }
    }
    else {
      kase = 4;
    } /* line 480 */
    r = r + 1;
    /*---------------------------------------------------------*/

    /* perform the task indicated by kase. */
    if (kase == 1) {
      /* deflate negligible w[m].*/
      f = e[m-2];
      e[m-2] = D_ZERO;
      for (k = m-1; k >= r; k--) {
        t1 = w[k-1];
        drotg(&t1,&f,&cs,&sn);
        w[k-1] = t1;
        if (k != r) {
          f = -sn* e[k-2];
          e[k-2] = cs*e[k-2];
        } /* line 500 */
        drot(vr,v+k-1,vc,v+m-1,vc,cs,sn);
      } /* line 510 */
    }
    else if (kase == 2) {
      /* split at negligible w(r). */
      f = e[r-2];
      e[r-2] = D_ZERO;
      for (k = r; k <= m; k++) {
        t1 = w[k-1];
        drotg(&t1,&f,&cs,&sn);
        w[k-1] = t1;
        f = -sn*e[k-1];
        e[k-1] = cs*e[k-1];
        drot(n,u+k-1,uc,u+r-2,uc,cs,sn);
      } /* line 530 */
    }
    else if (kase == 3) {
      /* perform one qr step. */
      /* calculate the shift. */
      scale = MAX(MAX(fabs(w[m-1]),fabs(w[m-2])),MAX(MAX(fabs(e[m-2]),fabs(w[r-1])),fabs(e[r-1])));
      sm = w[m-1]/scale;
      smm1 = w[m-2]/scale;
      emm1 = e[m-2]/scale;
      scr = w[r-1]/scale;
      ecr = e[r-1]/scale;
      b = ((smm1 + sm)*(smm1 - sm) + emm1*emm1)/D_TWO;
      c = (sm*sm*emm1*emm1);
      shift = D_ZERO;
      if ((b != D_ZERO) || (c != D_ZERO)) {
        shift = sqrt(b*b+c);
        if (b < D_ZERO) {
          shift = -shift;
        }
        shift = c/(b + shift);
      } /* line 550 */
      f = (scr + sm)*(scr - sm) + shift;
      g = scr*ecr;

      /* chase zeros. */
      mm1 = m - 1;
      for (k = r; k <= mm1; k++) {
        drotg(&f,&g,&cs,&sn);
        if (k != r) {
          e[k-2] = f;
        }
        f = cs*w[k-1] + sn*e[k-1];
        e[k-1] = cs*e[k-1] - sn*w[k-1];
        g = sn*w[k];
        w[k] = cs*w[k];
        drot(vr,v+k-1,vc,v+k,vc,cs,sn);

        drotg(&f,&g,&cs,&sn);
        w[k-1] = f;
        f = cs*e[k-1] + sn*w[k];
        w[k] = -sn*e[k-1] + cs*w[k];
        g = sn*e[k];
        e[k] = cs*e[k];
        /* if (k < n) { */ /* ORIGINAL FORTRAN looks incorrect */
        if (k < uc) {
          drot(n,u+k-1,uc,u+k,uc,cs,sn);
        }
      } /* line 560 */
      e[m-2] = f;
      iter = iter + 1;
    }
    else if (kase == 4) {
      /* convergence. */
      /* make the singular value  positive */
      if (w[r-1] < D_ZERO) {
        w[r-1] = -w[r-1];
        dscal(vr,-D_ONE,v+r-1,vc);
      } /* line 580 */

      /* order the singular value. */
      while (r != mm) {
        /*...exit */
        if (w[r-1] < w[r]) {
          t = w[r-1];
          w[r-1] = w[r];
          w[r] = t;
          if (r < vr) {
	    dswap(vr,v+r-1,vc,v+r,vc);
          }
          if (r < n) {
	    dswap(n,u+r-1,uc,u+r,uc);
          }
          r = r + 1;
        }
        else {
          break;
        }
      } /* line 600 */
      iter = 0;
      m = m - 1;
    } 
    else { 
      printf("ERROR (%s): kase not set properly\n", infunc);
      return(ERR);
    } /* line 610 */
  } /* end of big while loop */
  if (iter > maxit) {
    info = m;
     printf("ERROR (%s): info = %d\n", infunc, info);
     free((void *) x);
     free((void *) e);
     free((void *) work);
     free((void *) w);
     return(ERR);
  }

  /* Copy w back into s */
  for (i = 0; i < uc; i++) {
    s[i] = w[i];
  }

  free((void *) x);
  free((void *) e);
  free((void *) work);
  free((void *) w);
  
  return(OK);
}

/******************************************************************************
c COMMENTS FROM THE ORIGINAL FORTRAN ROUTINE:
c
c     dsvdc is a subroutine to reduce a double precision nxp matrix x
c     by orthogonal transformations u and v to diagonal form.  the
c     diagonal elements s(i) are the singular values of x.  the
c     columns of u are the corresponding left singular vectors,
c     and the columns of v the right singular vectors.
c
c     on entry
c
c         x         double precision(ldx,p), where ldx.ge.n.
c                   x contains the matrix whose singular value
c                   decomposition is to be computed.  x is
c                   destroyed by dsvdc.
c
c         ldx       integer.
c                   ldx is the leading dimension of the array x.
c
c         n         integer.
c                   n is the number of rows of the matrix x.
c
c         p         integer.
c                   p is the number of columns of the matrix x.
c
c         ldu       integer.
c                   ldu is the leading dimension of the array u
c                   (see below).
c
c         ldv       integer.
c                   ldv is the leading dimension of the array v
c                   (see below).
c
c         work      double precision(n).
c                   work is a scratch array.
c
c         job       integer.
c                   job controls the computation of the singular
c                   vectors.  it has the decimal expansion ab
c                   with the following meaning
c
c                        a.eq.0    do not compute the left singular
c                                  vectors.
c                        a.eq.1    return the n left singular vectors
c                                  in u.
c                        a.ge.2    returns the first min(n,p)
c                                  left singular vectors in u.
c                        b.eq.0    do not compute the right singular
c                                  vectors.
c                        b.eq.1    return the right singular vectors
c                                  in v.
c
c     on return
c
c         s         double precision(mm), where mm=min(n+1,p).
c                   the first min(n,p) entries of s contain the
c                   singular values of x arranged in descending
c                   order of magnitude.
c
c         e         double precision(p), 
c                   e ordinarily contains zeros.  however see the
c                   discussion of info for exceptions.
c
c         u         double precision(ldu,k), where ldu.ge.n.  if
c                                   joba.eq.1 then k.eq.n, if joba.ge.2
c                                   then k.eq.min(n,p).
c                   u contains the matrix of left singular vectors.
c                   u is not referenced if joba.eq.0.  if n.le.p
c                   or if joba.eq.2, then u may be identified with x
c                   in the subroutine call.
c
c         v         double precision(ldv,p), where ldv.ge.p.
c                   v contains the matrix of right singular vectors.
c                   v is not referenced if job.eq.0.  if p.le.n,
c                   then v may be identified with x in the
c                   subroutine call.
c
c         info      integer.
c                   the singular values (and their corresponding
c                   singular vectors) s(info+1),s(info+2),...,s(m)
c                   are correct (here m=min(n,p)).  thus if
c                   info.eq.0, all the singular values and their
c                   vectors are correct.  in any event, the matrix
c                   b = trans(u)*x*v is the bidiagonal matrix
c                   with the elements of s on its diagonal and the
c                   elements of e on its super-diagonal (trans(u)
c                   is the transpose of u).  thus the singular
c                   values of x and b are the same.
c
c     linpack. this version dated 08/14/78 .
c              correction made to shift 2/84.
c     g.w. stewart, university of maryland, argonne national lab.
c
c     dsvdc uses the following functions and subprograms.
c
c     external drot
c     blas daxpy,ddot,dscal,dswap,dnrm2,drotg
c     fortran dabs,dmax1,max0,min0,mod,dsqrt
C
**************************************************************************/
