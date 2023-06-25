#include <math.h>

#include "vicmain_c.h"
#include "applic.h"
#include "defines.h"
#include "ibisfile.h"
#include "ibiserrs.h"

#include "cartoMemUtils.h"

#ifndef MAX
#define MAX(a,b)	(((a)>(b))?(a):(b))
#endif

#ifndef MIN
#define MIN(a,b)	(((a)<(b))?(a):(b))
#endif

/*  image geom routine   A. Zobrist    7/14/99   */


static char msgBuf[20000];

void main44(void)
{
   int i,j,cols[4],colcount,coldef,gridIn,ibis,ibis2,ibis3,status;
   int nrec,nrec2,grids=0,gridl,nah,nav,gridIn2;
   int ibisOut,ixl,ixs;
   double *outl,*outs,*inl,*ins,*inl2,*ins2,x,y,ldel,sdel;
   double xx1,yy1,xx2,yy2,xxx,yyy,fxl,fxs,gridck;
   
   /* initialize, fetch params */

   zifmessage("gridcomp version 2016-01-08");
   
   zvparm("acols",cols,&colcount,&coldef,4,0);
   status = zvunit(&gridIn,"inp",1, NULL);
   status = IBISFileOpen(gridIn,&ibis,IMODE_READ,0,0,0,0);
   if (status!=1) IBISSignalU(gridIn,status,1);
   status = IBISColumnSet(ibis,ICOLUMN_U_FORMAT,"DOUB",cols[0]);
   status = IBISColumnSet(ibis,ICOLUMN_U_FORMAT,"DOUB",cols[1]);
   status = IBISColumnSet(ibis,ICOLUMN_U_FORMAT,"DOUB",cols[2]);
   status = IBISColumnSet(ibis,ICOLUMN_U_FORMAT,"DOUB",cols[3]);
   IBISFileGet(ibis,"nr",&nrec,1,1,0);
   mz_alloc1((unsigned char **)&outl,nrec,8);
   mz_alloc1((unsigned char **)&outs,nrec,8);
   mz_alloc1((unsigned char **)&inl,nrec,8);
   mz_alloc1((unsigned char **)&ins,nrec,8);
   status = IBISColumnRead(ibis,(char*) outl,cols[0],1,nrec);
   if (status!=1) IBISSignal(ibis,status,0);
   status = IBISColumnRead(ibis,(char*) outs,cols[1],1,nrec);
   if (status!=1) IBISSignal(ibis,status,0);
   status = IBISColumnRead(ibis,(char*) inl,cols[2],1,nrec);
   if (status!=1) IBISSignal(ibis,status,0);
   status = IBISColumnRead(ibis,(char*) ins,cols[3],1,nrec);
   if (status!=1) IBISSignal(ibis,status,0);
   
   /* determine the grid geometry */
   /* the large loop finds min and max extremes in input */
   /* can't just do cell corners because output might be small 
      subset of large cell */

   gridck = outl[0];
   for (i=0;i<nrec;i++)
      {
      grids = i;
      if (outl[i]!=gridck) break;
      }
   gridl = nrec/grids;
   if (gridl*grids!=nrec||gridl<2||grids<2)
      zmabend("First input grid not rectangular-1");
   for (i=1;i<gridl;i++)
      for (j=0;j<grids;j++)
         {
         if (fabs(outs[i*grids+j]-outs[j])<.002) continue;
         zmabend("First input grid not rectangular-2");
         }
   for (i=0;i<gridl;i++)
      for (j=1;j<grids;j++)
         {
         if (fabs(outl[i*grids+j]-outl[i*grids])<.002) continue;
         zmabend("First input grid not rectangular-3");
         }
   ldel = (outl[nrec-1]-outl[0])/(double)(gridl-1);
   sdel = (outs[nrec-1]-outs[0])/(double)(grids-1);
   for (i=1;i<gridl;i++)
      if (fabs(outl[i*grids]-outl[(i-1)*grids]-ldel)>0.002)
         zmabend("First input grid not evenly spaced vertically");
   for (i=1;i<grids;i++)
      {
      if (fabs(outs[i]-outs[i-1]-sdel)>0.002)
         zmabend("First input grid not evenly spaced horizontally");
      }
   nah = grids-1;
   nav = gridl-1;
   sprintf(msgBuf, "First input grid grid OK: nah = %d nav = %d",nah,nav);
   zifmessage(msgBuf);
   
   /* now read the second grid (input only) */
   
   zvparm("bcols",cols,&colcount,&coldef,4,0);
   status = zvunit(&gridIn2,"inp",2, NULL);
   status = IBISFileOpen(gridIn2,&ibis2,IMODE_READ,0,0,0,0);
   if (status!=1) IBISSignalU(gridIn2,status,1);
   status = IBISColumnSet(ibis2,ICOLUMN_U_FORMAT,"DOUB",cols[0]);
   status = IBISColumnSet(ibis2,ICOLUMN_U_FORMAT,"DOUB",cols[1]);
   status = IBISColumnSet(ibis2,ICOLUMN_U_FORMAT,"DOUB",cols[2]);
   status = IBISColumnSet(ibis2,ICOLUMN_U_FORMAT,"DOUB",cols[3]);
   IBISFileGet(ibis2,"nr",&nrec2,1,1,0);
   mz_alloc1((unsigned char **)&inl2,nrec2,8);
   mz_alloc1((unsigned char **)&ins2,nrec2,8);
   status = IBISColumnRead(ibis2,(char*) inl2,cols[2],1,nrec2);
   if (status!=1) IBISSignal(ibis2,status,0);
   status = IBISColumnRead(ibis2,(char*) ins2,cols[3],1,nrec2);
   if (status!=1) IBISSignal(ibis2,status,0);
   
   /* transform bilinearly by the first input grid */
   
   for (i=0;i<nrec2;i++)
      {
      x = inl2[i];
      y = ins2[i];
      
      ixl = (int)((x-outl[0])/ldel);
      ixs = (int)((y-outs[0])/sdel);
      
      ixl = MAX(MIN(ixl,gridl-2),0);
      ixs = MAX(MIN(ixs,grids-2),0);
      
      fxl = (x-outl[ixl*grids+ixs])/ldel;
      fxs = (y-outs[ixl*grids+ixs])/sdel;
      
      xx1 = (1.0-fxl)*inl[ixl*grids+ixs]+fxl*inl[(ixl+1)*grids+ixs];
      yy1 = (1.0-fxl)*ins[ixl*grids+ixs]+fxl*ins[(ixl+1)*grids+ixs];
      
      xx2 = (1.0-fxl)*inl[ixl*grids+ixs+1]+fxl*inl[(ixl+1)*grids+ixs+1];
      yy2 = (1.0-fxl)*ins[ixl*grids+ixs+1]+fxl*ins[(ixl+1)*grids+ixs+1];
      
      xxx = (1.0-fxs)*xx1+fxs*xx2;
      yyy = (1.0-fxs)*yy1+fxs*yy2;
      
      inl2[i] = xxx;
      ins2[i] = yyy;
      if (i<0)
         {
         printf("x,y %f %f\n",x,y);
         printf("ixl,ixs %d %d\n",ixl,ixs);
         
         printf("ixl*grids+ixs %d\n",ixl*grids+ixs);
         printf("ixl*grids+ixs+1 %d\n",ixl*grids+ixs+1);
         printf("(ixl+1)*grids+ixs %d\n",(ixl+1)*grids+ixs);
         printf("(ixl+1)*grids+ixs+1 %d\n",(ixl+1)*grids+ixs+1);
         
         printf("fxl,fxs %f %f\n",fxl,fxs);
         
         printf("inl[ixl*grids+ixs],inl[ixl*grids+ixs+1] %f %f\n",
                 inl[ixl*grids+ixs],inl[ixl*grids+ixs+1]);
         printf("inl[(ixl+1)*grids+ixs],inl[(ixl+1)*grids+ixs+1] %f %f\n",
                 inl[(ixl+1)*grids+ixs],inl[(ixl+1)*grids+ixs+1]);
         
         printf("ins[ixl*grids+ixs],ins[ixl*grids+ixs+1] %f %f\n",
                 ins[ixl*grids+ixs],ins[ixl*grids+ixs+1]);
         printf("ins[(ixl+1)*grids+ixs],ins[(ixl+1)*grids+ixs+1] %f %f\n",
                 ins[(ixl+1)*grids+ixs],ins[(ixl+1)*grids+ixs+1]);
         
         printf("xx1,yy1 %f %f\n",xx1,yy1);
         printf("xx2,yy2 %f %f\n",xx2,yy2);
         printf("xxx,yyy %f %f\n",xxx,yyy);
         printf("\n");
         }
      }
   
   /* write the output ibis file, the output columns are copies of the
      second input file output columns */
 
   status = zvunit(&ibisOut,"out",1, NULL);
   status = IBISFileUnit(ibisOut,&ibis3,"write",4,nrec2,0,"column");
   status = IBISFileSet(ibis3,"fmt_default","doub",0);
   status = IBISFileUnitOpen(ibis3);
   /*status = IBISFileOpen(ibisOut,&ibis3,"write",4,nrec2,0,0);*/
  
   status = IBISColumnWrite(ibis3,(char*) inl2,3,1,nrec2);
   if (status!=1) IBISSignal(ibis3,status,0);
   status = IBISColumnWrite(ibis3,(char*) ins2,4,1,nrec2);
   if (status!=1) IBISSignal(ibis3,status,0);
   
   status = IBISColumnRead(ibis2,(char*) inl2,cols[0],1,nrec2);
   if (status!=1) IBISSignal(ibis2,status,0);
   status = IBISColumnRead(ibis2,(char*) ins2,cols[1],1,nrec2);
   if (status!=1) IBISSignal(ibis2,status,0);
   status = IBISColumnWrite(ibis3,(char*) inl2,1,1,nrec2);
   if (status!=1) IBISSignal(ibis3,status,0);
   status = IBISColumnWrite(ibis3,(char*) ins2,2,1,nrec2);
   if (status!=1) IBISSignal(ibis3,status,0);
   
   status = IBISFileClose(ibis,0);
   status = IBISFileClose(ibis2,0);
   status = IBISFileClose(ibis3,0);
   return;
}
