#include "vicmain_c.h"
#include "applic.h"
#include <math.h>
#include <stdio.h>

#include "defines.h"
#include "ibisfile.h"
#include "ibiserrs.h"
#include "cartoMemUtils.h"

/************************************************************************/
/* program trnscol2                                                      */
/************************************************************************/
/*  02-06 ...alz... initial version                     */
/*  Fri Dec 28 2007 wlb switched to USES_ANSI_C AND LIB_CARTO; misc cleanup */
/************************************************************************/

void main44(void)
{
   int i,j,k,values[50],tocol[50],fromcol[50],valcount,tocount,coldef;
   int unit,unit2,ibis,ibis2,status,ncol,clen,fromcount,fclen,fncol;
   int datacol,indexcol,pcount,pdef,dummy,outhit,valsave,outncol,offset;
   int *indexvec;
   float *outvec,*toutvec;
   double *datavec,nullval;
           
   zifmessage("trnscol2 version Fri Dec 28 2007");
   
   /* get the basic parameters */
   
   status = zvp("datacol",&datacol,&dummy);
   status = zvp("indexcol",&indexcol,&dummy);
   status = zvp("ncol",&outncol,&dummy);
   zvparmd("null",&nullval,&pcount,&pdef,1,0);
   
   zvparm("values",values,&valcount,&coldef,50,0);
   zvparm("tocol",tocol,&tocount,&coldef,50,0);
   if (valcount!=tocount) zmabend("data values should equal destination cols");
   
   zvparm("fromcol",fromcol,&fromcount,&coldef,50,0);
   if (fromcol[0]>0) goto revcase;
   if (outncol>0&&outncol!=ncol) zmabend("can't change ncol for this case");
      
   /* read in data from the ibis interface file */

   status = zvunit(&unit,"inp",1, NULL);
   status = IBISFileOpen(unit,&ibis,"read",0,0,0,0);
   if (status!=1) IBISSignalU(unit,status,1);
   IBISFileGet(ibis,"nc",&ncol,1,1,0);
   IBISFileGet(ibis,"nr",&clen,1,1,0);
   
   mz_alloc1((unsigned char **)&indexvec,clen,4);
   mz_alloc1((unsigned char **)&datavec,clen,8);
   mz_alloc1((unsigned char **)&outvec,clen,4);
   
   status = IBISColumnSet(ibis,"U_FORMAT","DOUB",datacol);
   if (status!=1) IBISSignal(ibis,status,1);
   status = IBISColumnRead(ibis,(char*) datavec,datacol,1,clen);
   if (status!=1) IBISSignal(ibis,status,1);
   
   status = IBISColumnSet(ibis,"U_FORMAT","FULL",indexcol);
   if (status!=1) IBISSignal(ibis,status,1);
   status = IBISColumnRead(ibis,(char*) indexvec,indexcol,1,clen);
   if (status!=1) IBISSignal(ibis,status,1);
   
   /* Output desired columns to the 2nd ibis interface file */
   
   status = zvunit(&unit2,"out",1, NULL);
   status = IBISFileUnit(unit2,&ibis2,"write",ncol,clen,0,"column");
   status = IBISFileSet(ibis2,"fmt_default","real",0);
   status = IBISFileUnitOpen(ibis2);
   /*status = IBISFileOpen(ibisOut,&ibis2,"write",ncol,clen,0,0);*/
   
   valsave = 0;

   for (i=0;i<ncol;i++)
      {
      outhit = 0;
      for (j=0;j<tocount;j++)
         if ((i+1)==tocol[j])
            {
            outhit = i+1;
            valsave = values[j];
            }
      if (outhit>0)
         {
         for (j=0;j<clen;j++)
            {
            if (indexvec[j]==valsave) outvec[j] = datavec[j];
            else outvec[j] = nullval;
            }
         }
      else
         {
         status = IBISColumnSet(ibis,"U_FORMAT","REAL",i+1);
         if (status!=1) IBISSignal(ibis,status,1);
         status = IBISColumnRead(ibis,(char*) outvec,i+1,1,clen);
         if (status!=1) IBISSignal(ibis,status,1);
         }
      status = IBISColumnWrite(ibis2,(char*) outvec,i+1,1,clen);
      if (status!=1) IBISSignal(ibis2,status,0);
      }
   
   status = IBISFileClose(ibis,0);
   if (status!=1) IBISSignal(ibis,status,1);
   status = IBISFileClose(ibis2,0);
   if (status!=1) IBISSignal(ibis2,status,1);
   
   return;
   
   /* reverse case here */
   
   revcase:
   
   if (indexcol!=0) zmabend("don't allow indexcol for fromcol case");
   if (values[0]!=0) zmabend("don't allow values for fromcol case");
   if (tocol[0]!=0) zmabend("don't allow tocol for fromcol case");
   
   status = zvunit(&unit,"inp",1, NULL);
   status = IBISFileOpen(unit,&ibis,"read",0,0,0,0);
   if (status!=1) IBISSignalU(unit,status,1);
   IBISFileGet(ibis,"nc",&ncol,1,1,0);
   IBISFileGet(ibis,"nr",&clen,1,1,0);
   fclen = clen*fromcount;
   fncol = ncol;
   if (outncol>fncol) fncol = outncol;
   
   mz_alloc1((unsigned char **)&datavec,clen,8);
   mz_alloc1((unsigned char **)&outvec,fclen,4);
   mz_alloc1((unsigned char **)&toutvec,fclen,4);
   
   /* Output desired columns to the 2nd ibis interface file */
   
   status = zvunit(&unit2,"out",1, NULL);
   status = IBISFileUnit(unit2,&ibis2,"write",fncol,fclen,0,"column");
   status = IBISFileSet(ibis2,"fmt_default","real",0);
   status = IBISFileUnitOpen(ibis2);
   /*status = IBISFileOpen(ibisOut,&ibis2,"write",ncol,fclen,0,0);*/
   
   offset = 0;
   for (i=0;i<fncol;i++)
      {
      if ((i+1)<=ncol)
         {
         status = IBISColumnSet(ibis,"U_FORMAT","DOUB",i+1);
         if (status!=1) IBISSignal(ibis,status,1);
         status = IBISColumnRead(ibis,(char*) datavec,i+1,1,clen);
         if (status!=1) IBISSignal(ibis,status,1);
         }
      else
         for(j=0;j<clen;j++) datavec[j] = 0.0;
      
      for (j=0;j<clen;j++)
         for (k=0;k<fromcount;k++)
           toutvec[j*fromcount+k] = datavec[j];
      
      outhit = 0;
      for (j=0;j<fromcount;j++) if ((i+1)==fromcol[j]) outhit = i+1;
      if (outhit>0)
         {
         for (j=0;j<clen;j++) outvec[j*fromcount+offset] = datavec[j];
         offset++;
         }
      
      if ((i+1)==datacol) status = IBISColumnWrite(ibis2,(char*)outvec,i+1,1,fclen);
      else status = IBISColumnWrite(ibis2,(char*)toutvec,i+1,1,fclen);
      if (status!=1) IBISSignal(ibis2,status,0);
      }
      
   status = IBISFileClose(ibis,0);
   if (status!=1) IBISSignal(ibis,status,1);
   status = IBISFileClose(ibis2,0);
   if (status!=1) IBISSignal(ibis2,status,1);
   return;
}
