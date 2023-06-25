#include <zvproto.h>

#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include <math.h>

#include "lsqibis.h"
#include "ibishelper.h"
#include "ibisfile.h"

#include "cartoSortUtils.h"

#define MAXCOEFFCOLS 200
#define MAXRESCOLS   10

/*=========================================================*/
void setControlMapping(LsqIbis *ibis)
{
   int i, cnt;
   float control;
   float *pastControls;
   int *pastCnts;
   IBISStruct *ibisfile;

   ibisfile = ibis->ibisfile;

   // if no control number then set control mapping to row number
   if(!(ibis->concol))
   {
      ibis->IBIS2control = (int**)malloc(ibisfile->nr*sizeof(int*));
      for(i = 0; i < ibisfile->nr; i++)
      {
         (ibis->IBIS2control)[i] = (int*)malloc(2*sizeof(int));

         (ibis->IBIS2control)[i][0] = 0;
         (ibis->IBIS2control)[i][1] = i;
      }

      ibis->control2IBIS = (int**)malloc(sizeof(int*));
      (ibis->control2IBIS)[0] = (int*)malloc(sizeof(int)*ibisfile->nr);
      for(i = 0; i < ibisfile->nr; i++)(ibis->control2IBIS)[0][i] = i;

      ibis->controlID = (float*)malloc(sizeof(float));
      (ibis->controlID)[0] = 0;
      ibis->controlIDCnts = (int*)malloc(sizeof(int));
      (ibis->controlIDCnts)[0] = ibisfile->nr;

      ibis->controlSol = (double***)malloc(sizeof(double**));
      (ibis->controlSol)[0] = (double**)malloc(ibis->ycnt*sizeof(double*));
      for(i = 0; i < ibis->ycnt; i++)
         (ibis->controlSol)[0][i] = (double*)calloc(ibis->solcnt, sizeof(double));

      ibis->controlCnt = 1;
      return;
   }

   // to keep track of past control numbers
   pastControls = (float*)calloc(ibisfile->nr, sizeof(float));
   pastCnts = (int*)calloc(ibisfile->nr, sizeof(int));
   cnt = 0;
   for(i = 0; i < ibisfile->nr; i++)
   {
      int j, isNew;

      control = IBISHELPER_getFloat(ibisfile, ibis->concol - 1, i);
      if(i == 0)
      {
         pastControls[0] = control;
         pastCnts[0] = 1;
         ++cnt;
         continue;
      }

      isNew = 1;
      for(j = cnt - 1; j >= 0; j--)
      {
         if(control == pastControls[j])
         {
            isNew = 0;
            ++pastCnts[j];
            break;
         }
      }

      if(isNew)
      {
         pastControls[cnt] = control;
         ++pastCnts[cnt];
         ++cnt;
      }
   }

   ibis->controlCnt = cnt;

   // set control info array
   ibis->controlIDCnts = (int *)calloc(cnt, sizeof(int));
   ibis->controlID = (float *)malloc(sizeof(float)*cnt);
   for(i = 0; i < cnt; i++)
   {
      (ibis->controlID)[i] = pastControls[i];
   }

   // set control to ibis mapping
   ibis->control2IBIS = (int**)malloc(sizeof(int*)*cnt);
   for(i = 0; i < cnt; i++)
      (ibis->control2IBIS)[i] = (int*)calloc(pastCnts[i], sizeof(int));
   ibis->IBIS2control = (int**)malloc(sizeof(int*)*ibisfile->nr);

   for(i = 0; i < ibisfile->nr; i++)
   {
      int j;

      control = IBISHELPER_getFloat(ibisfile, ibis->concol - 1, i);
      for(j = 0; j < cnt; j++)
      {
         if(control == pastControls[j])
         {
            (ibis->control2IBIS)[j][(ibis->controlIDCnts)[j]++] = i;
            break;
         }
      }

      (ibis->IBIS2control)[i] = (int*)malloc(sizeof(int)*2);
      (ibis->IBIS2control)[i][0] = j;
      (ibis->IBIS2control)[i][1] = (ibis->controlIDCnts)[j] - 1;
   }

   // self check the number of controls
   for(i = 0; i < cnt; i++) assert(pastCnts[i] == (ibis->controlIDCnts)[i]);

   ibis->controlSol = (double***)malloc(sizeof(double**)*cnt);
   for(i = 0; i < cnt; i++)
   {
      int j;

      (ibis->controlSol)[i] = (double**)malloc(ibis->ycnt*sizeof(double*));

      for(j = 0; j < ibis->ycnt; j++)
         (ibis->controlSol)[i][j] = (double*)calloc(ibis->solcnt, sizeof(double));
   }

   free(pastControls);
   free(pastCnts);
}

/*=========================================================*/
LsqIbis* LSQIBIS_getLsqIbis()
{
   int i, status, dumdef, dumcnt;
   int temp[MAXCOEFFCOLS];
   LsqIbis *ibis;
   IBISStruct *ibisfile;

   ibis = (LsqIbis*)calloc(1, sizeof(LsqIbis));
   ibis->ibisfile = IBISHELPER_openIBIS("inp", 1, "read");
   ibisfile = ibis->ibisfile;

   // set print flag
   ibis->noprint = zvptst("noprint");

   // get dependent y columns
   status = zvpcnt("depcol", &(ibis->ycnt));
   assert(status == 1);
   if(ibis->ycnt == 0)
   {
      printf("Please specify depcols\n");
      zabend();
   }
   ibis->ycols = (int*)calloc(ibis->ycnt, sizeof(int));
   status = zvparm("depcol", ibis->ycols, &dumcnt, &dumdef, ibis->ycnt, 0);
   assert(status == 1);

   // get residual columns
   for(i = 0; i < MAXCOEFFCOLS; i++) temp[i] = 0;
   status = zvparm("rescol", temp, &dumcnt, &dumdef, MAXRESCOLS , 0);
   assert(status == 1);

   for(i = 0; i < MAXRESCOLS; i++) if(!(temp[i])) break;
   ibis->rescnt = i;
   if(ibis->rescnt != 0 && ibis->rescnt != ibis->ycnt)
   {
      printf("The number of rescol must either be 0 or equal to the number of depcol.\n");
      zabend();
   }

   if(ibis->rescnt)
   {
      ibis->rescols = (int*)calloc(ibis->rescnt, sizeof(int));
      for(i = 0; i < ibis->rescnt; i++) (ibis->rescols)[i] = temp[i];
   }

   // get independent x columns
   status = zvpcnt("indcol", &(ibis->xcnt));
   assert(status == 1);
   if(ibis->xcnt == 0)
   {
      printf("Please specify indcols\n");
      zabend();
   }
   ibis->xcols = (int*)calloc(ibis->xcnt, sizeof(int));
   status = zvparm("indcol", ibis->xcols, &dumcnt, &dumdef, ibis->xcnt, 0);
   assert(status == 1);

   // get solution coeff columns
   for(i = 0; i < MAXCOEFFCOLS; i++) temp[i] = 0;
   status = zvparm("coeffcol", temp, &dumcnt, &dumdef, MAXCOEFFCOLS, 0);
   assert(status == 1);

   for(i = 0; i < MAXCOEFFCOLS; i++)
   {
     /*      printf("%d %d\n", i, temp[i]);*/
      if(!(temp[i])) break;
   }
   ibis->solcnt = i;
   //   printf("!!!!!!TESTING!!!!!!!!! number of coeffcols: %d %d %d\n", ibis->solcnt, ibis->xcnt, ibis->ycnt);
   if(ibis->solcnt != 0 && ibis->solcnt != ibis->xcnt*ibis->ycnt)
   {
      printf("The number of coeffcol must be 0 or equal to indcol * depcol.\n");
      zabend();
   }

   if(ibis->solcnt)
   {
      ibis->solcols = (int*)calloc(ibis->solcnt, sizeof(int));
      for(i = 0; i < ibis->solcnt; i++) (ibis->solcols)[i] = temp[i];
   }

   // get threshold
   status = zvparmd("thresh", &(ibis->thresh), &dumcnt, &dumdef, 0, 0);
   assert(status == 1);

   // set control information
   status = zvp("concol", &(ibis->concol), &dumdef);
   assert(status == 1);

   // set weight information
   status = zvp("wghtcol", &(ibis->weightcol), &dumdef);
   assert(status == 1);

   setControlMapping(ibis);

   // set data info buffers
   ibis->dist = (double*)calloc(ibisfile->nr, sizeof(double));
   ibis->res = (double**)malloc(sizeof(double*)*ibis->ycnt);
   for(i = 0; i < ibis->ycnt; i++)
      ibis->res[i] = (double*)calloc(ibisfile->nr, sizeof(double));

   // throwout buffer
   ibis->throwout = (int*)calloc(ibisfile->nr, sizeof(int));

   return ibis;
}

/*=========================================================*/
void LSQIBIS_deleteLsqIbis(LsqIbis *ibis)
{
   int i;
   IBISStruct *ibisfile;

   ibisfile = ibis->ibisfile;

   free(ibis->ycols);
   if(ibis->solcnt) free(ibis->solcols);
   free(ibis->xcols);
   free(ibis->rescols);
   for(i = 0; i < ibis->ycnt; i++)
      free((ibis->res)[i]);
   free(ibis->res);
   free(ibis->dist);
   free(ibis->throwout);

   for(i = 0; i < ibis->controlCnt; i++)
   {
      int j;

      free((ibis->control2IBIS)[i]);

      for(j = 0; j < ibis->ycnt; j++)
         free((ibis->controlSol)[i][j]);
      free((ibis->controlSol)[i]);
   }
   free(ibis->control2IBIS);
   free(ibis->controlSol);

   for(i = 0; i < ibisfile->nr; i++)
      free((ibis->IBIS2control)[i]);
   free(ibis->IBIS2control);

   free(ibis->controlID);
   free(ibis->controlIDCnts);

   IBISHELPER_closeIBIS(&ibisfile);
   free(ibis);
}

/*=========================================================*/
/* control is the first index of control2IBIS              */
/* rec is the 2nd index of control2IBIS                    */
/*=========================================================*/
int isThrowout(LsqIbis *ibis, int control, int rec)
{
   assert(control >= 0 && control < ibis->controlCnt);
   assert(rec >= 0 && rec < (ibis->controlIDCnts)[control]);

   return (ibis->throwout)[(ibis->control2IBIS)[control][rec]];
}

/*=========================================================*/
int getControlNRecs(LsqIbis *ibis, int icontrol, int includeThrowout)
{
   int i, nRecs;

   if(includeThrowout) return (ibis->controlIDCnts)[icontrol];

   nRecs = 0;
   for(i = 0; i < (ibis->controlIDCnts)[icontrol]; i++)
     if(!isThrowout(ibis, icontrol, i)) ++nRecs;

   return nRecs;
}

/*=========================================================*/
int getOutfileNRecs(LsqIbis *ibis, int includeThrowout)
{
   int i, nr, nRecs;
   int *throwout;

   nr = ibis->ibisfile->nr;
   if(includeThrowout) return nr;

   throwout = ibis->throwout;

   nRecs = 0;
   for(i = 0; i < nr; i++)
      if(!throwout[i]) ++nRecs;

   return nRecs;
}

/*=========================================================*/
/* icontrol is the control index used in LsqIbis structure */
/* iy is the ycols index used by LsqIbis structure         */
/*=========================================================*/
void getControlYBuf(LsqIbis *ibis, double *buf, int icontrol, int iy, int includeThrowout)
{
   int i, ycol, ycnt;
   int controlCnt;

   ycnt = 0;
   ycol = (ibis->ycols)[iy] - 1;
   controlCnt = (ibis->controlIDCnts)[icontrol];
   for(i = 0; i < controlCnt; i++)
   {
      int rec;

      rec = (ibis->control2IBIS)[icontrol][i];

      if(!includeThrowout && isThrowout(ibis, icontrol, i)) continue;

      buf[ycnt++] = IBISHELPER_getDouble(ibis->ibisfile, ycol, rec);
   }
}

/*=========================================================*/
/* icontrol is the control index used in LsqIbis structure */
/* ix is the xcols index used by LsqIbis structure         */
/*=========================================================*/
void getControlXBuf(LsqIbis *ibis, double *buf, int icontrol, int ix, int includeThrowout)
{
   int i, xcol, xcnt;
   int controlCnt;

   xcnt = 0;
   xcol = (ibis->xcols)[ix] - 1;
   controlCnt = (ibis->controlIDCnts)[icontrol];
   for(i = 0; i < controlCnt; i++)
   {
      int rec;

      rec = (ibis->control2IBIS)[icontrol][i];

      if(!includeThrowout && isThrowout(ibis, icontrol, i)) continue;

      buf[xcnt++] = IBISHELPER_getDouble(ibis->ibisfile, xcol, rec);
   }
}

/*=========================================================*/
/* icontrol is the control index used in LsqIbis structure */
/* ix is the xcols index used by LsqIbis structure         */
/*=========================================================*/
void getControlWeightBuf(LsqIbis *ibis, double *buf, int icontrol, int includeThrowout)
{
   int i, wcol, wcnt;
   int controlCnt;

   wcnt = 0;
   wcol = ibis->weightcol - 1;
   controlCnt = (ibis->controlIDCnts)[icontrol];
   for(i = 0; i < controlCnt; i++)
   {
      int rec;

      rec = (ibis->control2IBIS)[icontrol][i];

      if(!includeThrowout && isThrowout(ibis, icontrol, i)) continue;

      buf[wcnt++] = IBISHELPER_getDouble(ibis->ibisfile, wcol, rec);
   }
}

/*=========================================================*/
LsqEquation* LSQIBIS_getLsqEquation(LsqIbis *ibis, int icontrol, int iy, int includeThrowout)
{
   double *buf;
   int i, nRecs;
   LsqEquation *eq;

   nRecs = getControlNRecs(ibis, icontrol, includeThrowout);
   eq = LSQEQUATION_getEquation(ibis->xcnt, nRecs, ibis->weightcol);

   buf = (double*)malloc(sizeof(double)*nRecs);

   getControlYBuf(ibis, buf, icontrol, iy, includeThrowout);
   LSQEQUATION_setYPar(eq, buf, nRecs);

   for(i = 0; i < ibis->xcnt; i++)
   {
      getControlXBuf(ibis, buf, icontrol, i, includeThrowout);
      LSQEQUATION_setXPar(eq, buf, i, nRecs);
   }

   if(ibis->weightcol)
   {
      getControlWeightBuf(ibis, buf, icontrol, includeThrowout);
      LSQEQUATION_setWeightPar(eq, buf, nRecs);
   }

   free(buf);

   return eq;
}

/*=========================================================*/
void getClosestPts(LsqIbis *ibis, int *pts, int row, int includeThrowout)
{
   int i;
   int control, controlRec, ncontrols;
   double *orig, *dist, max;

   orig = (double*)malloc(sizeof(double)*ibis->xcnt);
   for(i = 0; i < ibis->xcnt; i++)
      orig[i] = IBISHELPER_getDouble(ibis->ibisfile, (ibis->xcols)[i] - 1, row);
   control = (ibis->IBIS2control)[row][0];
   controlRec = (ibis->IBIS2control)[row][1];

   // get the distances
   max = -1;
   ncontrols = (ibis->controlIDCnts)[control];
   dist = (double*)calloc(ncontrols, sizeof(double));
   for(i = 0; i < ncontrols; i++)
   {
      int j;
      double x;

      for(j = 0; j < ibis->xcnt; j++)
      {
         x = IBISHELPER_getDouble(ibis->ibisfile, (ibis->xcols)[j] - 1, (ibis->control2IBIS)[control][i]);
         dist[i] += pow(orig[j] - x, 2.0);
      }

      dist[i] = pow(dist[i], 0.5);

      if(max < dist[i]) max = dist[i];
   }

   if(!includeThrowout)
      for(i = 0; i < ncontrols; i++)
         if(isThrowout(ibis, control, i)) dist[i] = max;

   getSelectionSortIndices(dist, pts, i, CART_DOUBLE);

   free(orig);
   free(dist);
}

/*=========================================================*/
/* row is IBIS file row number                             */
/* ibis->outliers must be set if outliers are excluded     */
/*=========================================================*/
int LSQIBIS_passesLocalFit(LsqIbis *ibis, int row, int npts, int includeThrowout)
{
   int i, j;
   int control, controlrec, controlcnt, *pts;
   double *buf, *errs, error;
   int **map;
   LsqEquation **eq;
   IBISStruct *ibisfile;

   ibisfile = ibis->ibisfile;
   map = ibis->control2IBIS;
   control = (ibis->IBIS2control)[row][0];
   controlrec = (ibis->IBIS2control)[row][1];
   controlcnt = (ibis->controlIDCnts)[control];
   if(npts > controlcnt) npts = controlcnt;

   eq = (LsqEquation**)malloc(sizeof(LsqEquation*)*ibis->ycnt);
   for(i = 0; i < ibis->ycnt; i++)
      eq[i] = LSQEQUATION_getEquation(ibis->xcnt, npts, ibis->weightcol);

   pts = (int*)calloc(controlcnt, sizeof(int));
   assert(pts);
   getClosestPts(ibis, pts, row, includeThrowout);

   buf = (double*)malloc(npts*sizeof(double));
   // set xpar
   for(i = 0; i < ibis->xcnt; i++)
   {
      for(j = 0; j < npts; j++)
         buf[j] = IBISHELPER_getDouble(ibisfile, (ibis->xcols)[i] - 1, map[control][pts[j]]);

      for(j = 0; j < ibis->ycnt; j++)
         LSQEQUATION_setXPar(eq[j], buf, i, npts);
   }

   // set weights
   if(ibis->weightcol)
   {
      for(j = 0; j < npts; j++)
         buf[j] = IBISHELPER_getDouble(ibisfile, ibis->weightcol - 1, map[control][pts[j]]);

      for(j = 0; j < ibis->ycnt; j++)
         LSQEQUATION_setWeightPar(eq[j], buf, npts);
   }

   // set ypar
   for(i = 0; i < ibis->ycnt; i++)
   {
      for(j = 0; j < npts; j++)
         buf[j] = IBISHELPER_getDouble(ibisfile, (ibis->ycols)[i] - 1, map[control][pts[j]]);
      LSQEQUATION_setYPar(eq[i], buf, npts);

      LSQEQUATION_solve(eq[i]);
   }

   error = 0.0;
   errs = (double*)calloc(ibis->ycnt, sizeof(double));
   for(i = 0; i < ibis->ycnt; i++)
      for(j = 0; j < ibis->xcnt; j++)
         errs[i] += IBISHELPER_getDouble(ibis->ibisfile, (ibis->xcols)[j] - 1, row)*((eq[i])->sol)[j];

   for(i = 0; i < ibis->ycnt; i++)
      error += pow(errs[i] - IBISHELPER_getDouble(ibis->ibisfile, (ibis->ycols)[i] - 1, row), 2.0);

   error = pow(error, 0.5);

   if(!(ibis->noprint))
   {
      printf("  ** local fitting point: %d error: %f --- ", row, error);
      (error < ibis->thresh) ? printf("passed") : printf("failed");
      printf("\n");
   }

   for(i = 0; i < ibis->ycnt; i++)
      LSQEQUATION_deleteEquation(eq[i]);
   free(eq);

   free(pts);
   free(buf);
   free(errs);
   if(error > ibis->thresh) return 0;
   return 1;
}

/*=========================================================*/
/* Calculates the residual, called by LSQIBIS_solveControl */
/* iy is the index of ycols not the ycol itself            */
/*=========================================================*/
void setControlResiduals(LsqIbis *ibis, LsqEquation *eq, int icontrol, int iy, int includeThrowout)
{
   int i, row, rescnt;

   rescnt = 0;
   for(i = 0; i < (ibis->controlIDCnts)[icontrol]; i++)
   {
      row = (ibis->control2IBIS)[icontrol][i];
      if(!includeThrowout && (ibis->throwout)[row]) continue;

      (ibis->res)[iy][row] = (eq->res)[rescnt++];
   }

   assert(rescnt == eq->nRecs);
}

/*=========================================================*/
void setControlDistance(LsqIbis *ibis, LsqEquation **eqs, int icontrol, int includeThrowout)
{
   int i, row, rescnt;

   for(i = 0; i < ibis->ycnt; i++)
   {
      int j;

      rescnt = 0;
      for(j = 0; j < (ibis->controlIDCnts)[icontrol]; j++)
      {
         row = (ibis->control2IBIS)[icontrol][j];
         if(!includeThrowout && (ibis->throwout)[row]) continue;

         if(i == 0) (ibis->dist)[row] = pow((eqs[i]->res)[rescnt++], 2.0);
         else (ibis->dist)[row] += pow((eqs[i]->res)[rescnt++], 2.0);
      }

      assert(rescnt == eqs[i]->nRecs);
   }

   for(i = 0; i < (ibis->controlIDCnts)[icontrol]; i++)
   {
      row = (ibis->control2IBIS)[icontrol][i];
      if(!includeThrowout && (ibis->throwout)[row]) continue;

      (ibis->dist)[row] = pow((ibis->dist)[row], 0.5);
   }
}

/*=========================================================*/
void printControlSol(LsqIbis *ibis, LsqEquation **eq, int icontrol)
{
   int i, j;

   printf("seq  concol  datcol    solution coefficient\n\n");
   for(i = 0; i < ibis->ycnt; i++)
   {
      for(j = 0; j < ibis->xcnt; j++)
         if(!(eq[i]->lsqErr))
            printf("%3d %7.2f %7d %24.10f\n",
                   i+1, (ibis->controlID)[icontrol],
                   (ibis->xcols)[j], ((eq[i])->sol)[j]);
         else
            printf("%3d %7.2f %7d %24.10f\n",
                   i+1, (ibis->controlID)[icontrol],
                   (ibis->xcols)[j], -999.0);
      printf("\n");
   }
}

/*=========================================================*/
void LSQIBIS_solveControl(LsqIbis *ibis, int icontrol, int includeThrowout)
{
   int i, erroredOut;
   LsqEquation **eq;

   erroredOut = 0;
   eq = (LsqEquation**)malloc(sizeof(LsqEquation*)*ibis->ycnt);
   for(i = 0; i < ibis->ycnt; i++)
   {
      eq[i] = LSQIBIS_getLsqEquation(ibis, icontrol, i, includeThrowout);
      LSQEQUATION_solve(eq[i]);

      if(eq[i]->lsqErr) erroredOut = 1;
   }

   for(i = 0; i < ibis->ycnt; i++)
   {
      int j;

      for(j = 0; j < ibis->solcnt; j++)
         if(!(eq[i]->lsqErr))
            (ibis->controlSol)[icontrol][i][j] = (eq[i]->sol)[j];
         else
            (ibis->controlSol)[icontrol][i][j] = -999.0;

      setControlResiduals(ibis, eq[i], icontrol, i, includeThrowout);
   }

   if(!erroredOut)
      setControlDistance(ibis, eq, icontrol, includeThrowout);

   /* printing if requested */
   if (!(ibis->noprint)) printControlSol(ibis, eq, icontrol);

   for(i = 0; i < ibis->ycnt; i++) LSQEQUATION_deleteEquation(eq[i]);
   free(eq);
}

/*=========================================================*/
void LSQIBIS_solveAll(LsqIbis *ibis, int includeThrowout)
{ 
   int i;

   for(i = 0; i < ibis->controlCnt; i++)
      LSQIBIS_solveControl(ibis, i, includeThrowout);
}

/*=========================================================*/
void writeIBIS(int unit, char *buf, char *format, int colNum, int sr, int nr)
{
   int status;

   assert(colNum > 0 && sr > 0);
   status = IBISColumnSet(unit, "U_FORMAT", format, colNum);
   assert(status == 1);
   status = IBISColumnWrite(unit, buf, colNum, sr, nr);
   assert(status == 1);
}

/*=========================================================*/
void getSolfout(LsqIbis *ibis, double *buf, int isol)
{
   int i, control, iy, bufind;

   iy = isol/(ibis->xcnt);
   isol %= (ibis->xcnt);

   bufind = 0;
   for(i = 0; i < ibis->ibisfile->nr; i++)
      if(!((ibis->throwout)[i]))
      {
         control = (ibis->IBIS2control)[i][0];
         buf[bufind++] = (ibis->controlSol)[control][iy][isol];
      }

   assert(bufind == getOutfileNRecs(ibis, 0));
}

/*=========================================================*/
void getResfout(LsqIbis *ibis, double *buf, int ires)
{
   int i, bufind;

   bufind = 0;
   for(i = 0; i < ibis->ibisfile->nr; i++)
      if(!((ibis->throwout)[i]))
         buf[bufind++] = (ibis->res)[ires][i];

   assert(bufind == getOutfileNRecs(ibis, 0));
}

/*=========================================================*/
void getColfout(LsqIbis *ibis, char *buf, int col)
{
   int i, bufind, colLen;
   IBISStruct *ibisfile;

   bufind = 0;
   ibisfile = ibis->ibisfile;
   colLen = (ibisfile->colLens)[col];
   for(i = 0; i < ibisfile->nr; i++)
      if(!(ibis->throwout[i]))
      {
         memcpy(buf+bufind, IBISHELPER_getBufPtr(ibisfile, col, i), colLen);
         bufind += colLen;
      }
}

/*=========================================================*/
void LSQIBIS_writeToIBIS(LsqIbis *ibis, int includeThrowout)
{
   int i, nRecs;
   int unit, handle, status, maxColLen;
   IBISStruct *ibisfile;
   double *buf;

   nRecs = getOutfileNRecs(ibis, includeThrowout);
   ibisfile = ibis->ibisfile;

   status = zvunit(&unit, "out", 1, NULL);
   assert(status == 1);
   status = IBISFileOpen(unit, &handle, "write", ibisfile->nc, nRecs, (char*) ibisfile->formats, NULL);
   if(status!=1) IBISSignalU(handle, status, 1);

   maxColLen = IBISHELPER_getMaxColLen(ibisfile);
   if(maxColLen < sizeof(double)) maxColLen = sizeof(double);
   buf = (double*)malloc(maxColLen*ibisfile->nr);
   for(i = 0; i < ibisfile->nc; i++)
   {
      int j, found;

      found = 0;
      for(j = 0; j < ibis->solcnt; j++)
         if(i + 1 == (ibis->solcols)[j])
         {
            getSolfout(ibis, buf, j);
            found = 1;
            break;
         }
      for(j = 0; !found && j < ibis->rescnt; j++)
         if(i + 1 == (ibis->rescols)[j])
         {
            getResfout(ibis, buf, j);
            found = 1;
            break;
         }

      if(found)
      {
         writeIBIS(handle, (char*)buf, "DOUB", i+1, 1, nRecs);
         continue;
      }

      getColfout(ibis, (char*)buf, i);
      status = IBISColumnWrite(handle, (char*)buf, i+1, 1, nRecs);
      assert(status == 1);
   }

   free(buf);
   status = zvclose(handle, NULL);
   assert(status == 1);
}

