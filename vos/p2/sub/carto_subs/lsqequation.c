#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "lsqequation.h"
#include "gsl/gsl_matrix.h"
#include "gsl/gsl_vector.h"
#include "gsl/gsl_fit.h"
#include "gsl/gsl_linalg.h"
#include "gsl/gsl_multifit.h"

/*=========================================================*/
LsqEquation* LSQEQUATION_getEquation(int xcnt, int nRecs, int weight)
{
   int i;

   LsqEquation *eq;

   eq = (LsqEquation*)malloc(sizeof(LsqEquation));

   eq->nRecs = nRecs;
   eq->xcnt = xcnt;
   eq->ypar = (double*)calloc(nRecs, sizeof(double));
   eq->xpar = (double**)calloc(xcnt, sizeof(double*));
   for(i = 0; i < xcnt; i++)
      (eq->xpar)[i] = (double*)calloc(nRecs, sizeof(double));

   eq->res = (double*)calloc(nRecs, sizeof(double));
   eq->sol = (double*)calloc(nRecs, sizeof(double));
   if(weight > 0) eq->weight = 1;
   else eq->weight=0;
   if(weight)
   {
      eq->weights = (double*)calloc(nRecs, sizeof(double));

      eq->cov = (double**)malloc(xcnt*sizeof(double*));
      for(i = 0; i < xcnt; i++) (eq->cov)[i] = (double*)calloc(xcnt, sizeof(double));

   }

   return eq;
}

/*=========================================================*/
void LSQEQUATION_deleteEquation(LsqEquation* eq)
{
   int i;

   free(eq->sol);
   free(eq->res);
   for(i = 0; i < eq->xcnt; i++) free((eq->xpar)[i]);
   free(eq->xpar);
   free(eq->ypar);
   if(eq->weight)
   {
      free(eq->weights);

      for(i = 0; i < eq->xcnt; i++) free((eq->cov)[i]);
      free(eq->cov);
   }

   free(eq);
}

/*=========================================================*/
void LSQEQUATION_setYPar(LsqEquation *eq, double *buf, int nRecs)
{
   memcpy(eq->ypar, buf, sizeof(double)*nRecs);
}

/*=========================================================*/
void LSQEQUATION_setWeightPar(LsqEquation *eq, double *buf, int nRecs)
{
   assert(eq->weight);

   memcpy(eq->weights, buf, sizeof(double)*nRecs);
}

/*=========================================================*/
void LSQEQUATION_setXPar(LsqEquation *eq, double *buf, int ix, int nRecs)
{
   assert(ix < eq->xcnt);

   memcpy((eq->xpar)[ix], buf, sizeof(double)*nRecs);
}

/*=========================================================*/
void calcResiduals(LsqEquation *eq)
{
   int i;

   for(i = 0; i < eq->nRecs; i++)
   {
      int j;
      double result;

      result = 0;
      for(j = 0; j < eq->xcnt; j++)
         result += (eq->xpar)[j][i]*(eq->sol)[j];
      (eq->res)[i] = (eq->ypar)[i] - result;
   }

}

/*=========================================================*/
void solveOrig(LsqEquation *eq)
{
   int i;
   double *xbuf, *ybuf;

   xbuf = (double*)malloc(sizeof(double)*eq->xcnt*eq->nRecs);
   for(i = 0; i < eq->xcnt; i++)
      memcpy(xbuf+(i*eq->nRecs), (eq->xpar)[i], sizeof(double)*eq->nRecs);
   ybuf = (double*)malloc(sizeof(double)*eq->nRecs);
   memcpy(ybuf, eq->ypar, sizeof(double)*eq->nRecs);
   lsqfit(xbuf, ybuf, eq->nRecs, eq->xcnt, eq->sol, 1.e-7, &(eq->lsqErr));

   calcResiduals(eq);

   free(xbuf);
   free(ybuf);
}

/*=========================================================*/
void wsolve(LsqEquation *eq)
{
   int i;
   gsl_matrix *mX, *mCov;
   gsl_vector *vY, *vC, *vW;
   gsl_multifit_linear_workspace *work;

   work = gsl_multifit_linear_alloc(eq->nRecs, eq->xcnt);

   mX = gsl_matrix_alloc(eq->nRecs, eq->xcnt);
   vY = gsl_vector_alloc(eq->nRecs);
   vW = gsl_vector_alloc(eq->nRecs);
   vC = gsl_vector_alloc(eq->xcnt);
   mCov = gsl_matrix_alloc(eq->xcnt, eq->xcnt);

   /* set mX matrix */
   for(i = 0; i < eq->nRecs; i++)
   {
      int j;

      for(j = 0; j < eq->xcnt; j++)
         gsl_matrix_set(mX, i, j, (eq->xpar)[j][i]);

      gsl_vector_set(vY, i, (eq->ypar)[i]);
      gsl_vector_set(vW, i, (eq->weights)[i]);
      //      gsl_vector_set(vW, i, 1/(eq->xcnt - i));
   }

   /* solve */
   gsl_multifit_wlinear(mX, vW, vY, vC, mCov, &(eq->chisq), work);

   //   printf(" *** chisq estimate: %f\n", eq->chisq);

   for(i = 0; i < eq->xcnt; i++)
   {
      int j;

      (eq->sol)[i] = gsl_vector_get(vC, i);

      for(j = 0; j < eq->xcnt; j++)
         (eq->cov)[i][j] = gsl_matrix_get(mCov, i, j);
   }

   eq->lsqErr = 0;

   /* calculate residuals */
   calcResiduals(eq);

   gsl_multifit_linear_free(work);
   gsl_matrix_free(mX);
   gsl_matrix_free(mCov);
   gsl_vector_free(vY);
   gsl_vector_free(vW);
   gsl_vector_free(vC);
}

/*=========================================================*/
void LSQEQUATION_solve(LsqEquation *eq)
{
  //  solveOrig(eq);
   if(eq->weight) wsolve(eq);
   else solveOrig(eq);
}

