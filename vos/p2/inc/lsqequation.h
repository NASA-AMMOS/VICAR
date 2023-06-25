#ifndef LSQYPAR
#define LSQYPAR

#include "cartoLsqUtils.h"

typedef struct
{
   double *ypar, *sol, *res;
   double **xpar;
   int lsqErr, xcnt, nRecs;

   /* variables used by weighted fitting */
   int weight;
   double *weights, chisq, **cov;
}LsqEquation;
/*----------------*/

/* This function returns a pointer to a newly created */
/* LsqEquation struct with all the memory allocated   */
/* but uninitialized.                                 */
LsqEquation* LSQEQUATION_getEquation(int xcnt, int nRows, int weight);

/* This funtion frees up the LsqEquation struct       */
void LSQEQUATION_deleteEquation(LsqEquation* eq);

/* This copies the data in buf into eq->ypar          */
void LSQEQUATION_setYPar(LsqEquation *eq, double *buf, int nRecs);

/* This copies the data in buf into eq->weights       */
void LSQEQUATION_setWeightPar(LsqEquation *eq, double *buf, int nRecs);

/* This copies the data in buf into (eq->xpar)[ix]    */
void LSQEQUATION_setXPar(LsqEquation *eq, double *buf, int ix, int nRecs);

void LSQEQUATION_solve(LsqEquation *eq);

#endif
