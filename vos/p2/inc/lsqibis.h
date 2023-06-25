#ifndef LSQIBIS
#define LSQIBIS

#include "ibishelper.h"
#include "lsqequation.h"

/*----------------*/
typedef struct
{
   // print flag
   int noprint;

   // ibis column info
   int *ycols, *solcols, *xcols;
   int *rescols, concol, weightcol;
   int xcnt, ycnt, rescnt, solcnt;

   // data info
   double thresh;
   double **res, *dist;
   int *throwout;

   // data control tracking info
   // control2IBIS - maps control to a IBIS row
   //      control2IBIS[0][3] maps the fourth element of first control
   // IBIS2control - maps IBIS row to control ID index
   //      IBIS2control[0][0] maps the first record in IBIS to a control number
   //      IBIS2control[0][1] maps the first record in IBIS to a control record
   // controlID - contains control number
   // controlIDCnt - contains control number counts
   // controlCnt - number of different control numbers
   // controlSol - contains solution [control][iy][isol] - note: nsol = nx

   int **control2IBIS, **IBIS2control, *controlIDCnts, controlCnt;
   float *controlID;
   double ***controlSol;

   IBISStruct *ibisfile;
}LsqIbis;

/*----------------*/

LsqIbis* LSQIBIS_getLsqIbis();
void LSQIBIS_deleteLsqIbis(LsqIbis *ibis);
LsqEquation* LSQIBIS_getLsqEquation(LsqIbis *ibis, int icontrol, int iy, int includeOutliers);
void LSQIBIS_solveControl(LsqIbis *ibis, int icontrol, int includeOutliers);
void LSQIBIS_solveAll(LsqIbis *ibis, int includeOutliers);
int LSQIBIS_passesLocalFit(LsqIbis *ibis, int row, int npts, int includeThrowout);
void LSQIBIS_writeToIBIS(LsqIbis *ibis, int throwout);

#endif
