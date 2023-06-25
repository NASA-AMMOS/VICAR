#include <math.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "vicmain_c.h"
#include "applic.h"
#include "defines.h"
#include "ibisfile.h"
#include "ibiserrs.h"

//#include "cartoVicarProtos.h"
#include "cartoLsqUtils.h"
#include "cartoSortUtils.h"
#include "cartoMemUtils.h"

/*       FREQUENTLY USED VARIABLES IN THIS PROGRAM         */
/*=========================================================*/
/*                                                         */
/* concol - control column number (0 if no control column  */
/*            is specified)                                */
/* concolv - buffer containing control numbers from IBIS   */
/*           file                                          */
/* xpar - independent dataset                              */
/* ypar - dependent dataset                                */
/* ypar2 - depenedent dataset. optional may be             */
/*         uninitialized                                   */
/* clen - number of rows in output IBIS file               */
/* indcount - number of dependent variables in the         */
/*            equation (xpar)                              */
/* rout - residual data for ypar                           */
/* rout3 - residual data for ypar2                         */
/* thresh - threshold for determining outliers             */
/* outliers - buffer holding outlier indices               */
/* outlierCnt - number of outliers                         */
/* depcol2 - column number where ypar2 is in the IBIS file */
/*=========================================================*/

/*=========================================================*/
/* This function performs a global least squares fitting.  */
/* This function calls least squares fitting after         */
/* separating the data by its control number. the least    */
/* squares fitting.                                        */
/*=========================================================*/
void doLstSq(concolv, xpar, ypar, clen, indcount, indcol, coeffcol, sout, noprint, rescol, rout)
   double **xpar, *ypar, **sout, *rout;
   float *concolv;
   int indcount, indcol[20], coeffcol[20], noprint, rescol;
{
   /* do the least squares */
   /* could reduce storage need by saving solutions more compactly, and
      then free the input data, and unpack the solutions */
   int uptr, i, j, lptr, igroup, ier, numlsq;
   float groupnbr;
   double sum, eps, csol[20];
   double *clsq, *clsqdep;

   mz_alloc1((unsigned char **)&clsq,indcount*clen,8);
   mz_alloc1((unsigned char **)&clsqdep,clen,8);

   uptr = 0;
   for (igroup=0;;igroup++)
   {
      /* Find the indices of the data points with same */
      /* control numbers.  The control numbers are     */
      /* assumed to be grouped.                        */
      lptr = uptr;
      if (lptr>=clen) break;
      groupnbr = concolv[lptr];
      for (uptr=lptr;uptr<=clen;uptr++)
         if (uptr==clen||concolv[uptr]!=groupnbr) break;

      /* put dependent and independent data into buffs */
      numlsq = uptr-lptr;
      for (i=0;i<indcount;i++)
         for (j=lptr;j<uptr;j++) clsq[i*numlsq+j-lptr] = xpar[i][j];
      for (j=lptr;j<uptr;j++) clsqdep[j-lptr] = ypar[j];

      /* perform least squares fitting here */
      eps = 1.e-7;
      lsqfit(clsq,clsqdep,numlsq,indcount,csol,eps,&ier);
      
      /* printing if requested */
      if (!noprint)
      {
         printf("seq  concol  datcol    solution coefficient\n\n");
         for (i=0;i<indcount;i++)
            if (ier==0) printf("%3d %7.2f %7d %24.10f\n",
                i+1,groupnbr,indcol[i],csol[i]);
            else printf("%3d %7.2f %7d %24.10f\n",
                i+1,groupnbr,indcol[i],-999.0);
         printf("\n");
      }

      /* calculate the output data */
      if (coeffcol[0]!=0)
      {
         for (i=0;i<indcount;i++)
            for (j=lptr;j<uptr;j++)
               if (ier==0) sout[i][j] = csol[i];
               else sout[i][j] = -999.0;
      }
      if (rescol!=0)
      {
         for (j=lptr;j<uptr;j++)
            if (ier==0)
            {
               sum = 0.0;
               for (i=0;i<indcount;i++)
                  sum += csol[i]*xpar[i][j];
               rout[j] = ypar[j]-sum;
            }
            else rout[j] = 0.0;
      }
   }

   free(clsq);
   free(clsqdep);
}

/*=========================================================*/
/* This function throws out the outlier from xpar, ypar    */
/* ypar2, concolv (control column), and adjusts the clen   */
/* (number of rows in output IBIS file) and outlierCnt.    */
/* It also adds the outlier index into outliers buffer.    */
/*=========================================================*/
void throwout(int *outliers, int outlierCnt, int indcount, float *concolv,
              double *ypar, double *ypar2, double **xpar, int depcol2, int *clen)
{
   int i, j;

   printf("  **Point %d thrown out.  outlier cnt: %d**\n\n", outliers[outlierCnt-1], outlierCnt);

   --(*clen);
   for(i = outliers[outlierCnt-1]; i < *clen; i++)
   {
      concolv[i] = concolv[i+1];
      ypar[i] = ypar[i+1];
      for(j = 0; j < indcount; j++)
         xpar[j][i] = xpar[j][i+1];
      if(depcol2 > 0)
	 ypar2[i] = ypar2[i+1];
   }
}

/*=========================================================*/
/* This function calculates the residual error as sqrt of  */
/* x'*x'+y'+y' and stores the errors in rout3.             */
/*=========================================================*/
void getAbsResiduals(rout1, rout2, rout3, clen, thresh, depcol2)
   double thresh, *rout1, *rout2, *rout3;
   int clen, depcol2;
{
   int i;
   
   for(i = 0; i < clen; i++)
   {
      if(depcol2 != 0)
         rout3[i] = pow(rout1[i]*rout1[i] + rout2[i]*rout2[i], 0.5);
      else
	 rout3[i] = fabs(rout1[i]);
   }
}

/*=========================================================*/
/* This function gets the distances for datapoints in      */
/* pos and stores it into distance buffer.  The distance   */
/* is measured from origin.                                */
/*                                                         */
/* pos - buffer that has the datapoints (input)            */
/* distance - buffer that the distances will be stored     */
/*            into (output)                                */
/* indcount - number of independent variables in pos and   */
/*            origin. (input)                              */
/* controlCnt - number of entities in pos. (input)         */
/* origin - the point from where the distances are         */
/*          calculated.                                    */
/*=========================================================*/
void getDistances(double **pos, double *distance, int indcount, int controlCnt, double *origin)
{
   int i, j;

   for(i = 0; i < controlCnt; i++)
   {
      double sum;

      sum = 0.0;
      for(j = 0; j < indcount; j++)
	 sum += pow(origin[j]-pos[j][i], 2);
      distance[i] = sqrt(sum);
   }
}

/*=========================================================*/
/* This function finds 30 closest datapoints for each      */
/* global outlier and calculates the local least squares.  */
/* If the outlier is within a threshold after a local      */
/* fitting, it will not be thrown away.                    */
/*                                                         */
/*=========================================================*/
void doLocalFit(concol, concolv, xpar, ypar, ypar2, clen, indcount, rout, rout3, thresh, outliers, outlierCnt, depcol2)
   float *concolv;
   double **xpar, *ypar, *ypar2, *rout, *rout3, thresh;
   int concol, *clen, indcount, *outliers, *outlierCnt, depcol2;
{
   int *routSorted, i, controlCnt, *distSorted, outlierFound, reBuffer;
   double **controlInds, *controlDeps, *controlDeps2, *origin, locRout, locRout2, *distances;
   float control, oldControl;

   /* data used for input parameters into lsqfit */
   int ier;
   double sum, eps, csol[20], csol2[20];
   double *clsq, *tmpclsq, *clsqdep, *clsqdep2;

   mz_alloc1((unsigned char **)&clsq,indcount*(*clen),sizeof(double));
   mz_alloc1((unsigned char **)&tmpclsq, indcount*(*clen), sizeof(double));
   mz_alloc1((unsigned char **)&clsqdep,(*clen),sizeof(double));
   mz_alloc1((unsigned char **)&clsqdep2,(*clen),sizeof(double));
   /**********************************************/

   controlDeps = malloc((*clen)*sizeof(double));
   controlDeps2 = malloc((*clen)*sizeof(double));
   mz_alloc2((unsigned char ***)&controlInds,indcount,(*clen),sizeof(double));
   origin = malloc(indcount*sizeof(double));
   distances = malloc((*clen)*sizeof(double));
   distSorted = malloc((*clen)*sizeof(int));
   routSorted = malloc((*clen)*sizeof(int));

   /* sort residual from lowest to greatest */
   getSelectionSortIndices(rout3, routSorted, *clen, CART_DOUBLE);

   oldControl = -999; /* Set to an *unlikely control number.  */
                      /* -999 means the buffers have not been */
                      /* initialized.                         */
                      /* -998 means that the buffers were     */
                      /* initialized and no control numbers   */
                      /* are specified.                       */

   outlierFound = 0;
   reBuffer = 1;
   eps = 1.e-7;
   i = *clen-1;
   while(i >= 0 && rout3[routSorted[i]] > thresh)
   {
      int j, k;

      printf("point %d has a global residual: %f\n", routSorted[i], rout3[routSorted[i]]);

      if(concol != 0)
         control = concolv[routSorted[i]];

      /* Copy data with same control entities into buffer. */
      /* Data is copied into buffer iff there is a control */
      /* column and the previous control number does not   */
      /* equal the current control number OR current loop  */
      /* is the initial loop and there are no data inside  */
      /* the buffers.                                      */
      if((concol != 0 && control != oldControl) || (concol != 0 && reBuffer))
      {
         controlCnt = 0;
         oldControl = control;

	 //	 printf("rebuffered\n");
	 for(j = 0; j < *clen; j++)
	 {
	    if(concolv[j] == concolv[routSorted[i]])
	    {
	       for(k = 0; k < indcount; k++)
		  controlInds[k][controlCnt] = xpar[k][j];
	       controlDeps[controlCnt] = ypar[j];
	       if(depcol2 != 0) controlDeps2[controlCnt] = ypar2[j];

	       ++controlCnt;
	    }
	 }
      }
      else if((concol == 0 && oldControl != -999) || (concol == 0 && reBuffer))
      {
	//	 printf("rebuffered\n");
	 oldControl = -998; /* Specify that the buffers have been initialized */
	                    /* and there are no control numbers.              */

	 for(j = 0; j < indcount; j++)
	    memcpy(controlInds[j], xpar[j], (*clen)*sizeof(double));

	 memcpy(controlDeps, ypar, (*clen)*sizeof(double));
	 if(depcol2 != 0) memcpy(controlDeps2, ypar2, (*clen)*sizeof(double));

	 controlCnt = *clen;
      }
   
      /* get 30 closest points */
      if(controlCnt > 30)
      {
	 for(j = 0; j < indcount; j++)
	    origin[j] = xpar[j][routSorted[i]];

         getDistances(controlInds, distances, indcount, controlCnt, origin);
      
	 getSelectionSortIndices(distances, distSorted, controlCnt, CART_DOUBLE);
	 for(j = 0; j < 30; j++)
	 {
	   //	   printf("j: %d point picked: %d distance: %f\n", j, distSorted[j], distances[distSorted[j]]);
	    /* get independent data into buffer */
	    for(k = 0; k < indcount; k++)
	       clsq[k*30+j] = controlInds[k][distSorted[j]];
	 
	    /* get dependent data into buffer */
	    clsqdep[j] = controlDeps[distSorted[j]];
	    if(depcol2 != 0)
               clsqdep2[j] = controlDeps2[distSorted[j]];
	 }
      }
      else /* if less than 30 points then just do a memcpy */
      {
	 for(j = 0; j < indcount; j++)
	    memcpy(clsq+j*controlCnt, controlInds[j], controlCnt*sizeof(double));
	 memcpy(clsqdep, controlDeps, controlCnt*sizeof(double));
	 if(depcol2 != 0) memcpy(clsqdep2, controlDeps2, controlCnt*sizeof(double));
      }

      /* perform local least squares fit on 30 points (29 closest + itself) */
      memcpy(tmpclsq, clsq, indcount*(*clen)*sizeof(double));

      lsqfit(clsq, clsqdep, (controlCnt < 30) ? controlCnt:30, indcount, csol, eps, &ier);
      if(ier != 0) printf("IBISLSQL error in lsqfit - error code: %d controlCnt: %d\n", ier, controlCnt);
      memcpy(clsq, tmpclsq, indcount*(*clen)*sizeof(double));

      if(depcol2 != 0)
         lsqfit(clsq, clsqdep2, (controlCnt < 30) ? controlCnt:30, indcount, csol2, eps, &ier);
      if(ier != 0) printf("IBISLSQL error in lsqfit - error code: %d controlCnt: %d", ier, controlCnt);

      /* calculate the local residual */
      sum = 0.0;
      for(j=0; j<indcount; j++)
         sum += csol[j]*origin[j];

      locRout = ypar[routSorted[i]]-sum;
      if(depcol2 != 0)
      {
	 sum = 0.0;
	 for(j = 0; j < indcount; j++)
	    sum += csol2[j]*origin[j];

	 locRout2 = ypar2[routSorted[i]]-sum;

         locRout = pow(locRout*locRout + locRout2*locRout2, 0.5);
      }
      else locRout = abs(locRout);

      printf("\t       local residual : %f\n", locRout);

      if(locRout > thresh)
      {
         outliers[*outlierCnt] = routSorted[i];
	 ++(*outlierCnt);
	 outlierFound = reBuffer = 1;

         throwout(outliers, *outlierCnt, indcount, concolv, ypar, ypar2, xpar, depcol2, clen);
	 for(j = i-1; j >= 0; j--)
	    if(routSorted[j] > routSorted[i])
	       routSorted[j]--;
      }
      else
      {
         printf("  **Point %d is not thrown out.\n\n", routSorted[i]);
	 reBuffer = 0;
      }

      --i;
   }

   /* If no outliers were thrown away by local fitting then tell main44 that it's done. */
   if(!outlierFound)
   {
      outliers[*outlierCnt] = -1;
      ++(*outlierCnt);
   }

   free(clsq);
   free(clsqdep);
   free(clsqdep2);
   free(controlDeps);
   free(controlDeps2);
   mz_free2((unsigned char**)controlInds, indcount);
   free(origin);
   free(distances);
   free(routSorted);
   free(distSorted);

   printf("local fit done.\n");
}

/*=========================================================*/
void main44(void)
{
   int i,indcol[20],coeffcol[20],unit,unit2,indcount,coeffcount,coldef;
   int coeffcol2[20],coeffcount2, coldef2;
   int ibis1,ibis2,status,clen,nc,nr,depcol,rescol,concol;
   int depcol2,rescol2, *outliers;
   int dummy,noprint, outlierCnt, cnt, def;
   double thresh;
   float *concolv;
   double **xpar,**sout,*ypar,*rout;
   double **sout2,*ypar2,*rout2,*rout3;
   double *inFile, *outFile;
   int attempt;

   zifmessage("ibislsql version 24-oct-02");
   
   /* get the basic parameters */
   
   status = zvp("depcol",&depcol,&dummy);
   status = zvp("depcol2", &depcol2, &dummy);
   status = zvp("rescol",&rescol,&dummy);
   status = zvp("rescol2", &rescol2, &dummy);
   status = zvp("concol",&concol,&dummy);
   status = zvparmd("thresh", &thresh, &cnt, &def, 0, 0);
   noprint = zvptst("noprint");

   zvparm("indcol",indcol,&indcount,&coldef,20,0);
   zvparm("coeffcol",coeffcol,&coeffcount,&coldef,20,0);
   if(depcol2 != 0)
      zvparm("coeffcol2",coeffcol2,&coeffcount2,&coldef2,20,0);
   if(coeffcount!=indcount && coldef==0)
      zmabend("Count for parameter COEFFCOL wrong");
   if(depcol2 != 0 && coeffcount2!=indcount && coldef2==0)
      zmabend("Count for parameter COEFFCOL2 wrong");

   /* read in data from the ibis interface file */
   /* open input files */
   status = zvunit(&unit,"inp",1, NULL);
   status = IBISFileOpen(unit,&ibis1,"read",0,0,0,0);
   if (status!=1) IBISSignalU(unit,status,1);
   IBISFileGet(ibis1,"nr",&nr,1,1,0);
   IBISFileGet(ibis1,"nc",&nc,1,1,0);
   clen=nr;

   /* allocate memories */
   mz_alloc1((unsigned char **)&outliers, clen, sizeof(int));
   outlierCnt = 0;
   
   mz_alloc2((unsigned char ***)&xpar,indcount,clen,8);
   mz_alloc1((unsigned char **)&ypar,clen,8);
   if(depcol2 != 0)   mz_alloc1((unsigned char **)&ypar2,clen,8);
   mz_alloc1((unsigned char **)&concolv,clen,8);
   
   if (coeffcol[0]!=0) mz_alloc2((unsigned char ***)&sout,indcount,clen,8);
   if (depcol2!=0 && coeffcol2[0]!=0)
      mz_alloc2((unsigned char ***)&sout2,indcount,clen,8);
   if (rescol!=0) mz_alloc1((unsigned char **)&rout,clen,8);
   if(rescol2 != 0)
      mz_alloc1((unsigned char**)&rout2, clen, 8);
   mz_alloc1((unsigned char**)&rout3, clen, 8);
   
   for (i=0;i<indcount;i++)
   {
      status = IBISColumnSet(ibis1,"U_FORMAT","DOUB",indcol[i]);
      if (status!=1) IBISSignal(ibis1,status,1);
      status = IBISColumnRead(ibis1,(char *)(xpar[i]),indcol[i],1,clen);
      if (status!=1) IBISSignal(ibis1,status,1);
   }

   status = IBISColumnSet(ibis1,"U_FORMAT","DOUB",depcol);
   if (status!=1) IBISSignal(ibis1,status,1);
   status = IBISColumnRead(ibis1,(char *)ypar,depcol,1,clen);
   if (status!=1) IBISSignal(ibis1,status,1);

   if(depcol2 != 0)
   {
      status = IBISColumnSet(ibis1,"U_FORMAT","DOUB",depcol2);
      if (status!=1) IBISSignal(ibis1,status,1);
      status = IBISColumnRead(ibis1,(char *)ypar2,depcol2,1,clen);
      if (status!=1) IBISSignal(ibis1,status,1);
   }
   
   if (concol>0)
   {
      status = IBISColumnSet(ibis1,"U_FORMAT","REAL",concol);
      if (status!=1) IBISSignal(ibis1,status,1);
      status = IBISColumnRead(ibis1,(char *)concolv,concol,1,clen);
      if (status!=1) IBISSignal(ibis1,status,1);
   }
   else for (i=0;i<clen;i++) concolv[i] = 1.0;

   /* perform least squares */
   printf("======================\n");
   printf("ATTEMPT: %d\n", (attempt = 1));
   doLstSq(concolv, xpar, ypar, clen, indcount, indcol, coeffcol, sout, noprint, rescol, rout);
   if(depcol2 != 0)
      doLstSq(concolv, xpar, ypar2, clen, indcount, indcol, coeffcol2, sout2, noprint, rescol2, rout2);

   /* find outlier */
   if(thresh > .0)
      getAbsResiduals(rout, rout2, rout3, clen, thresh, depcol2);

   if(thresh > .0)
      doLocalFit(concol, concolv, xpar, ypar, ypar2, &clen, indcount, rout, rout3, thresh, outliers, &outlierCnt, depcol2);

   while(thresh > .0 && outliers[outlierCnt-1] != -1)
   {
      /* perform least squares again */
      printf("======================\n");
      printf("ATTEMPT: %d\n", ++attempt);
      doLstSq(concolv, xpar, ypar, clen, indcount, indcol, coeffcol, sout, noprint, rescol, rout);
      if(depcol2 != 0)
      {
         doLstSq(concolv, xpar, ypar2, clen, indcount, indcol, coeffcol2, sout2, noprint, rescol2, rout2);
         getAbsResiduals(rout, rout2, rout3, clen, thresh, depcol2);
      }
      doLocalFit(concol, concolv, xpar, ypar, ypar2, &clen, indcount, rout, rout3, thresh, outliers, &outlierCnt, depcol2);
   }
   printf("  All points are under error threshold.**\n");

   /* Outlier indices are not what they were because of array shifting after
      throwing out each outlier.  We need to calculate the original index
      of the outliers                                                        */
   {
      int oi, oj;

      for(oi = outlierCnt-2; oi > 0; oi--)
      {
	 int actualIndex = outliers[oi];

	 for(oj = oi-1; oj >= 0; oj--)
	 {

	    if(actualIndex >= outliers[oj]) 
            {
               actualIndex++;
	    }
	 }
	 outliers[oi] = actualIndex;
      }
   }

   /* quick check */
   if(outlierCnt > 0 && (outlierCnt-1)+clen != nr)
      zmabend("Some entities were lost.\n");

   /* Output desired columns to the ibis interface file */
   /* Open output file */
   zvselpi(0);

   status = zvunit(&unit2,"out",1, NULL);
   if (status!=1) IBISSignalU(unit2,status,1);
   status = IBISFileUnit(unit2, &ibis2, "write", nc, clen, 0, 0);
   if (status!=1) IBISSignalU(unit2,status,1);
   status = IBISFileSet(ibis2, "fmt_default", "doub", 1);
   if (status!=1) IBISSignalU(unit2,status,1);
   status = IBISFileUnitOpen(ibis2);
   if (status!=1) IBISSignalU(unit2,status,1);

   mz_alloc1((unsigned char**)&inFile, nr, sizeof(double));
   mz_alloc1((unsigned char**)&outFile, clen, sizeof(double));

   /* output to file - inside this loop, columns that are
      unrelated to the least squares fitting are copied
      just to keep the data in the output file same as the
      data in the input file                               */
   for(i = 0; i < nc; i++)
   {
      int j, continueFlag, cpIndex;

      /* If i+1 equals a column that we handle later then skip for now */
      continueFlag = cpIndex = 0;
      for(j = 0; j < indcount; j++)
  	 if(i+1 == coeffcol[j] || (depcol2!=0 && coeffcol2[j] == i+1)
            || i+1 == indcol[j])
	 {
	    continueFlag = 1;
	    break;
	 }
      if(i+1 == rescol || i+1 == rescol2 || i+1 == depcol
         || i+1 == depcol2 || i+1 == concol)
	 continueFlag = 1;
      if(continueFlag) continue;

      status = IBISColumnSet(ibis1, "U_FORMAT", "DOUB", i+1);
      if(status != 1) IBISSignal(ibis1, status, 1);
      status = IBISColumnRead(ibis1, (char *)inFile, i+1, 1, nr);
      if(status != 1) IBISSignal(ibis1, status, 1);

      /* if there are no outliers then we can just copy the entire
         column... but if there are outliers then we need to not
         copy those                                               */
      if(outlierCnt > 0)
      {
         for(j = 0; j < nr; j++)
         {
	    int k, outFlag;

	    outFlag = 0;

	    /* check to see if current row is marked as an outlier */
	    for(k = 0; k < outlierCnt; k++)
	       if(j == outliers[k])
               {
                  outFlag = 1;
		  break;
	       }

	    /* check to make sure that the cpIndex (index of output
               column) is not greater than the length of output column */
	    if(cpIndex > clen) printf("error accessing beyond array cpIndex: %d clen: %d \n", cpIndex, clen);

	    /* if it's not an outlier then copy to output column */
	    if(!outFlag)
            {
               outFile[cpIndex++] = inFile[j];
	    }
         }
      }
      else memcpy(outFile, inFile, clen*sizeof(double));

      status = IBISColumnWrite(ibis2, (char *)outFile, i+1, 1, clen);
      if(status!=1) IBISSignal(ibis2, status, 0);

   }

   free(inFile);
   free(outFile);

   /* now copy over all the columns that were necessary in the least
      squares fitting                                                */
   if(indcol[0] != 0)
   {
      for(i = 0; i < indcount; i++)
      {
	 status = IBISColumnSet(ibis2, "U_FORMAT", "DOUB", indcol[i]);
	 if(status != 1) IBISSignal(ibis2, status, 1);
	 status = IBISColumnWrite(ibis2, (char *)(xpar[i]), indcol[i], 1, clen);
	 if(status != 1) IBISSignal(ibis2, status, 1);
      }
   }
   if(coeffcol[0]!=0)
   {
      for (i=0;i<indcount;i++)
      {
         status = IBISColumnSet(ibis2,"U_FORMAT","DOUB",coeffcol[i]);
         if (status!=1) IBISSignal(ibis2,status,1);
         status = IBISColumnWrite(ibis2,(char *)(sout[i]),coeffcol[i],1,clen);
         if (status!=1) IBISSignal(ibis2,status,1);
      }
      mz_free2((unsigned char**)sout, indcount);
   }
   if(depcol2 != 0 && coeffcol2[0]!=0)
   {
      for (i=0;i<indcount;i++)
      {
         status = IBISColumnSet(ibis2,"U_FORMAT","DOUB",coeffcol2[i]);
         if (status!=1) IBISSignal(ibis2,status,1);
         status = IBISColumnWrite(ibis2,(char *)(sout2[i]),coeffcol2[i],1,clen);
         if (status!=1) IBISSignal(ibis2,status,1);
      }
      mz_free2((unsigned char**)sout2, indcount);
   }
   if (rescol!=0)
   {
      status = IBISColumnSet(ibis2,"U_FORMAT","DOUB",rescol);
      if (status!=1) IBISSignal(ibis2,status,1);
      status = IBISColumnWrite(ibis2,(char *)rout,rescol,1,clen);
      if (status!=1) IBISSignal(ibis2,status,1);
      free(rout);
   }
   if (rescol2!=0)
   {
      status = IBISColumnSet(ibis2,"U_FORMAT","DOUB",rescol2);
      if (status!=1) IBISSignal(ibis2,status,1);
      status = IBISColumnWrite(ibis2,(char *)rout2,rescol2,1,clen);
      if (status!=1) IBISSignal(ibis2,status,1);
      free(rout2);
   }
   if(concol != 0)
   {
      status = IBISColumnSet(ibis2,"U_FORMAT","DOUB",concol);
      if (status!=1) IBISSignal(ibis2,status,1);
      status = IBISColumnWrite(ibis2,(char *)concolv,concol,1,clen);
      if (status!=1) IBISSignal(ibis2,status,1);
   }
   if(depcol != 0)
   {
      status = IBISColumnSet(ibis2,"U_FORMAT","DOUB",depcol);
      if (status!=1) IBISSignal(ibis2,status,1);
      status = IBISColumnWrite(ibis2,(char *)ypar,depcol,1,clen);
      if (status!=1) IBISSignal(ibis2,status,1);
   }
   if(depcol2 != 0)
   {
      status = IBISColumnSet(ibis2,"U_FORMAT","DOUB",depcol2);
      if (status!=1) IBISSignal(ibis2,status,1);
      status = IBISColumnWrite(ibis2,(char *)ypar2,depcol2,1,clen);
      if (status!=1) IBISSignal(ibis2,status,1);
      free(ypar2);
   }

   free(outliers);
   mz_free2((unsigned char**)xpar, indcount);
   free(ypar);
   free(concolv);

   status = IBISFileClose(ibis1,0);
   if (status!=1) IBISSignal(ibis1,status,1);
   status = IBISFileClose(ibis2,0);
   if (status!=1) IBISSignal(ibis2,status,1);
}












