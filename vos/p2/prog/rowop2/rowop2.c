#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "vicmain_c.h"
#include "applic.h"
#include "defines.h"
#include "ibisfile.h"
#include "ibiserrs.h"

#include "cartoStrUtils.h"
#include "cartoMemUtils.h"

#define MAXCOLS 500

/************************************************************************/
/* program rowop2                                                      */
/************************************************************************/
/*  03-08 ...alz... initial version                     */
/************************************************************************/

void main44(void)
{
   int i,j,icol,unit1,unit2,ibis1,ibis2,keycount,rangecount;
   int clen1,clen2,ncol1,ncol2,status,colwid,keycol[100],dummy;
   int selmode=0,parcount,outptr;
   short int *delvec;
   double *iodat,range[200],prec;
   char cformat[MAXCOLS][6],*iodatstr;
  
   zifmessage("rowop2 version 2015-12-16");
   
   zvparm("keycol",keycol,&keycount,&dummy,100,0);
   zvparmd("range",range,&rangecount,&dummy,200,0);
   if (rangecount!=2*keycount)
      zmabend("need two range values for each key column");
   if (zvptst("delete")) selmode = 1;
   if (zvptst("select")) selmode = 2;
   if (zvptst("pick")) selmode = 3;
   zvparmd("prec",&prec,&parcount,&dummy,200,0);
   
   /* open the input file and read in the control columns */
   
   status = zvunit(&unit1,"inp",1, NULL);
   status = IBISFileOpen(unit1,&ibis1,IMODE_READ,0,0,0,0);
   if (status!=1) IBISSignalU(unit1,status,1);
   IBISFileGet(ibis1,"nr",&clen1,1,1,0);
   IBISFileGet(ibis1,"nc",&ncol1,1,1,0);
   IBISFileGet(ibis1,"formats",cformat,1,MAXCOLS,6);
   
   mz_alloc1((unsigned char **)&iodat,clen1,8);
   mz_alloc1((unsigned char **)&delvec,clen1,2);
   
   /*  calculate a deletion vector and get clen2 */
   
   if (clen1==0) goto noin;
   for (i=0;i<clen1;i++) delvec[i] = 1;
   
   for (i=0;i<keycount;i++)
      {
      status = IBISColumnGet(ibis1,"FORMAT",cformat[i],keycol[i]);
      if (status!=1) IBISSignal(ibis1,status,1);
      if (cformat[i][0]=='A') zmabend("cannot use alphabetic column in rowop");
      status = IBISColumnSet(ibis1,"U_FORMAT","DOUB",keycol[i]);
      if (status!=1) IBISSignal(ibis1,status,1);
      status = IBISColumnRead(ibis1,(char*)iodat,keycol[i],1,clen1);
      if (status!=1) IBISSignal(ibis1,status,1);
      
      for (j=0;j<clen1;j++)
         {
         switch(selmode)
            {
            case 1: case 3:
               if ((range[i*2]-prec)<=iodat[j]&&
                   iodat[j]<=(range[i*2+1]+prec)) delvec[j] = 0;
               break;
            case 2:    
               if (!((range[i*2]-prec)<=iodat[j]&&
                   iodat[j]<=(range[i*2+1]+prec))) delvec[j] = 0;
            }
         }
      }
   if (selmode==3) for (i=0;i<clen1;i++) delvec[i] = 1-delvec[i];
   noin:
   clen2 = 0;
   for (i=0;i<clen1;i++) clen2 += delvec[i];
   
   /* open the output file */
   
   ncol2 = ncol1;
   for (icol=1;icol<=ncol1;icol++)
      {
      status = IBISColumnGet(ibis1,"FORMAT",cformat[icol-1],icol);
      if (status!=1) IBISSignal(ibis1,status,1);
     }
   status = zvunit(&unit2,"out",1, NULL);
   status = IBISFileUnit(unit2,&ibis2,"write",ncol2,clen2,(char*)cformat,"column");
   status = IBISFileUnitOpen(ibis2);
   for (icol=1;icol<=ncol1;icol++)
      {
      if (cformat[icol-1][0]!='A')
         {
         status = IBISColumnSet(ibis1,"U_FORMAT","DOUB",icol);
         if (status!=1) IBISSignal(ibis1,status,1);
         status = IBISColumnSet(ibis2,"U_FORMAT","DOUB",icol);
         if (status!=1) IBISSignal(ibis2,status,1);
         }
      }
   /*status = IBISFileOpen(ibis2,&ibis,"write",4,clen,0,0);*/
   
   /* read each selected column, delete according to delvec,
      then write out the column */
   
   if (clen2==0) goto noout;
   for (icol=1;icol<=ncol1;icol++)
      {
      if (cformat[icol-1][0]!='A')  /* numeric case */
         {
         status = IBISColumnRead(ibis1,(char*)iodat,icol,1,clen1);
         if (status!=1) IBISSignal(ibis1,status,1);
         
         for (i=0,outptr = 0;i<clen1;i++)
            if (delvec[i]==1) iodat[outptr++] = iodat[i];
         
         status = IBISColumnWrite(ibis2,(char*)iodat,icol,1,clen2);
         if (status!=1) IBISSignal(ibis2,status,1);
         }
      else     /* alphabetic case */
         {
         colwid = ms_num(&cformat[icol-1][1])+1;
         mz_alloc1((unsigned char **)&iodatstr,clen1,colwid);
         status = IBISColumnRead(ibis1,iodatstr,icol,1,clen1);
         if (status!=1) IBISSignal(ibis1,status,1);
         
         for (i=0,outptr = 0;i<clen1;i++)
            if (delvec[i]==1)
               strcpy(&iodatstr[(outptr++)*colwid],&iodatstr[i*colwid]);
         
         status = IBISColumnWrite(ibis2,iodatstr,icol,1,clen2);
         if (status!=1) IBISSignal(ibis2,status,1);
         
         free(iodatstr);
         }
      }
   noout:

   /* print in/out file lengths */
   
   printf("%d records in, %d records out\n",clen1,clen2);
   
   /* close files */
   
   status = IBISFileClose(ibis1,0);
   if (status!=1) IBISSignal(ibis1,status,1);
   status = IBISFileClose(ibis2,0);
   if (status!=1) IBISSignal(ibis2,status,1);
  
   return;
}
