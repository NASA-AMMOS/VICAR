#include <math.h>
#include <stdio.h>
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
/* program icat                                                      */
/************************************************************************/
/*  03-08 ...alz... initial version                     */
/************************************************************************/

void main44(void)
{
   int i,j,icol,unit1,unit2,unit3,ibis1,ibis2,ibis3,ncol1,ncol2,ncol3;
   int clen1,clen2,clen3,filewid,status,vertmode,readptr,irec;
   char coltype1[MAXCOLS][6],coltype2[MAXCOLS][6],coltype3[MAXCOLS][6];
   char *filedat;
   double *numdat;
   char msgBuf[1000];
   
   zifmessage("icat version 2015-11-05");
   
   /* parameter, default is vertical */
   
   if (zvptst("h")) vertmode = 0; else vertmode = 1;
   
   /* open the ibis interface files */

   status = zvunit(&unit1,"inp",1, NULL);
   status = IBISFileOpen(unit1,&ibis1,IMODE_READ,0,0,0,0);
   if (status!=1) IBISSignalU(unit1,status,1);
   IBISFileGet(ibis1,"nr",&clen1,1,1,0);
   IBISFileGet(ibis1,"nc",&ncol1,1,1,0);
   IBISFileGet(ibis1,"formats",coltype1,1,MAXCOLS,6);
   
   status = zvunit(&unit2,"inp",2, NULL);
   status = IBISFileOpen(unit2,&ibis2,IMODE_READ,0,0,0,0);
   if (status!=1) IBISSignalU(unit2,status,1);
   IBISFileGet(ibis2,"nr",&clen2,1,1,0);
   IBISFileGet(ibis2,"nc",&ncol2,1,1,0);
   IBISFileGet(ibis2,"formats",coltype2,1,MAXCOLS,6);
   
   for (icol=1;icol<=ncol1;icol++)
      {
      status = IBISColumnGet(ibis1,"FORMAT",coltype3[icol-1],icol);
      if (status!=1) IBISSignal(ibis1,status,1);
      }
   if (vertmode)
      {
      if (ncol1!=ncol2) zmabend("number of columns don't match");
      for (icol=0;icol<ncol1;icol++)
         {
         if (strcmp(coltype1[icol],coltype2[icol])!=0) zmabend("column types don't match");
         }
      ncol3 = ncol1;
      clen3 = clen1+clen2;
      }
   else
      {
      ncol3 = ncol1+ncol2;
      if (clen1>clen2) clen3 = clen1; else clen3 = clen2;
      for (icol=1;icol<=ncol2;icol++)
         {
         status = IBISColumnGet(ibis2,"FORMAT",coltype3[ncol1+icol-1],icol);
         if (status!=1) IBISSignal(ibis2,status,1);
         }
      }
   status = zvunit(&unit3,"out",1, NULL);
   status = IBISFileUnit(unit3,&ibis3,"write",ncol3,clen3,(char*)coltype3,"column");
   status = IBISFileUnitOpen(ibis3);
   
   mz_alloc1((unsigned char **)&numdat,clen3,8);
   
   /* copy the data */
   
   for (icol=0;icol<ncol3;icol++)
      {
      if (coltype3[icol][0]!='A')     /* numeric column */
         {
         readptr = 0;
         if (vertmode||(!vertmode&&icol<ncol1))
            {
            if (clen1>0)
               {
               status = IBISColumnSet(ibis1,"U_FORMAT","DOUB",icol+1);
               if (status!=1) IBISSignal(ibis1,status,1);
               status = IBISColumnRead(ibis1,(char*)numdat,icol+1,1,clen1);
               if (status!=1) IBISSignal(ibis1,status,1);
               }
            if (!vertmode&&clen1<clen3)
               {
               for (i=clen1;i<clen3;i++) numdat[i] = 0.0;
               }
            readptr = clen1;
            }
         if (vertmode||(!vertmode&&icol>=ncol1))
            {
            if (!vertmode&&icol>=ncol1) irec = icol-ncol1+1; else irec = icol+1;
            if (clen2>0)
               {
               status = IBISColumnSet(ibis2,"U_FORMAT","DOUB",irec);
               if (status!=1) IBISSignal(ibis2,status,1);
               status = IBISColumnRead(ibis2,(char*)(&numdat[readptr]),irec,1,clen2);
               if (status!=1) IBISSignal(ibis2,status,1);
               }
            if (!vertmode&&clen2<clen3)
               {
               for (i=clen2;i<clen3;i++) numdat[i] = 0.0;
               }
            }
         status = IBISColumnSet(ibis3,"U_FORMAT","DOUB",icol+1);
         if (status!=1) IBISSignal(ibis3,status,1);
         status = IBISColumnWrite(ibis3,(char*)numdat,icol+1,1,clen3);
         if (status!=1) IBISSignal(ibis3,status,1);
         }
      else                           /* alpha column */
         {
         readptr = 0;
         filewid = ms_num(&coltype3[icol][1])+1;
         mz_alloc1((unsigned char **)&filedat,clen3,filewid);
         
         if (vertmode||(!vertmode&&icol<ncol1))
            {
               if (clen1>0)
               {
               status = IBISColumnRead(ibis1,filedat,icol+1,1,clen1);
               if (status!=1) IBISSignal(ibis1,status,1);
               }
            if (!vertmode&&clen1<clen3)
               {
               for (i=clen1;i<clen3;i++)
                  for (j=0;j<filewid;j++)
                     filedat[i*filewid+j] = ' ';
               }
            readptr = clen1;
            }
            
         if (vertmode||(!vertmode&&icol>=ncol1))
            {
            if (!vertmode&&icol>=ncol1) irec = icol-ncol1+1; else irec = icol+1;
            if (clen2>0)
               {
               status = IBISColumnRead(ibis2,&filedat[readptr*filewid],irec,1,clen2);
               if (status!=1) IBISSignal(ibis2,status,1);
               }
            if (!vertmode&&clen2<clen3)
               {
               for (i=clen2;i<clen3;i++)
                  for (j=0;j<filewid;j++)
                     filedat[i*filewid+j] = ' ';
               }
            }
            
         status = IBISColumnWrite(ibis3,filedat,icol+1,1,clen3);
         if (status!=1) IBISSignal(ibis3,status,1);
         }
      }
   
   /* print in/out file lengths */
   
   sprintf(msgBuf, "%d and %d records in, %d records out",clen1,clen2,clen3);
   zifmessage(msgBuf);
   
   /* close files */
   
   status = IBISFileClose(ibis1,0);
   if (status!=1) IBISSignal(ibis1,status,1);
   status = IBISFileClose(ibis2,0);
   if (status!=1) IBISSignal(ibis2,status,1);
   status = IBISFileClose(ibis3,0);
   if (status!=1) IBISSignal(ibis3,status,1);
  
   return;
}
