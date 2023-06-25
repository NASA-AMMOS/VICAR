#include <math.h>

#include "vicmain_c.h"
#include "applic.h"
#include "defines.h"
#include "ibisfile.h"
#include "ibiserrs.h"
#include "zifmessage.h"
#include "zmabend.h"

#include "cartoMemUtils.h"

/*  grid averaging routine   A. Zobrist    7/13/05   */

void main44(void)
{
   int i,j,cols[4],colcount,coldef,filcnt,gridIn,ibis,status;
   int nrec=0,nrec1=0,geomOut;
   double *newl=NULL,*news=NULL,*oldl=NULL,*olds=NULL,*ckl=NULL,*cks=NULL,*outl=NULL,*outs=NULL,denom;
   
   /* initialize, fetch params */

   zifmessage("GRIDAVG version 2021-11-12");
   
   zvparm("newcols",&cols[0],&colcount,&coldef,2,0);
   zvparm("oldcols",&cols[2],&colcount,&coldef,2,0);
   status = zvpcnt("inp",&filcnt);
   denom = (double)(1.0/(double)filcnt);

   /* loop over the files input, keeping average */
   
   for (i=0;i<filcnt;i++)
      {
      status = zvunit(&gridIn,"inp",i+1, NULL);
      status = IBISFileOpen(gridIn,&ibis,IMODE_READ,0,0,0,0);
      if (status!=1) IBISSignalU(gridIn,status,1);
      status = IBISColumnSet(ibis,ICOLUMN_U_FORMAT,"DOUB",cols[0]);
      status = IBISColumnSet(ibis,ICOLUMN_U_FORMAT,"DOUB",cols[1]);
      status = IBISColumnSet(ibis,ICOLUMN_U_FORMAT,"DOUB",cols[2]);
      status = IBISColumnSet(ibis,ICOLUMN_U_FORMAT,"DOUB",cols[3]);
      IBISFileGet(ibis,"nr",&nrec,1,1,0);

      if (i==0)
         {
         nrec1 = nrec;
         mz_alloc1((unsigned char **)&newl,nrec,8);
         mz_alloc1((unsigned char **)&news,nrec,8);
         mz_alloc1((unsigned char **)&oldl,nrec,8);
         mz_alloc1((unsigned char **)&olds,nrec,8);
         mz_alloc1((unsigned char **)&outl,nrec,8);
         mz_alloc1((unsigned char **)&outs,nrec,8);
         mz_alloc1((unsigned char **)&ckl,nrec,8);
         mz_alloc1((unsigned char **)&cks,nrec,8);
         for (j=0;j<nrec;j++) { outl[j] = 0.0; outs[j] = 0.0; }
         }
      else if (nrec!=nrec1) zmabend("input files of unequal length");
      
      status = IBISColumnRead(ibis,(char*) newl,cols[0],1,nrec);
      if (status!=1) IBISSignal(ibis,status,0);
      status = IBISColumnRead(ibis,(char*) news,cols[1],1,nrec);
      if (status!=1) IBISSignal(ibis,status,0);
      status = IBISColumnRead(ibis,(char*) oldl,cols[2],1,nrec);
      if (status!=1) IBISSignal(ibis,status,0);
      status = IBISColumnRead(ibis,(char*) olds,cols[3],1,nrec);
      if (status!=1) IBISSignal(ibis,status,0);
   
      if (i==0) for (j=0;j<nrec;j++) { ckl[j] = newl[j]; cks[j] = news[j]; }
      else for (j=0;j<nrec;j++) if(ckl[j]!=newl[j]||cks[j]!=news[j])
            zmabend("new part of grids not identical");
      
      for (j=0;j<nrec;j++) { outl[j] += oldl[j]; outs[j] += olds[j]; }
      status = IBISFileClose(ibis,0);
      }
   for (j=0;j<nrec;j++) { outl[j] *= denom; outs[j] *= denom; }

   /* write the output ibis file */
 
   status = zvunit(&geomOut,"out",1, NULL);
   status = IBISFileUnit(geomOut,&ibis,"write",4,nrec,0,"column");
   status = IBISFileSet(ibis,"fmt_default","doub",0);
   status = IBISFileUnitOpen(ibis);
   /*status = IBISFileOpen(geomOut,&ibis,"write",4,nrec,0,0);*/
  
   status = IBISColumnWrite(ibis,(char*) ckl,1,1,nrec);
   if (status!=1) IBISSignal(ibis,status,0);
   status = IBISColumnWrite(ibis,(char*) cks,2,1,nrec);
   if (status!=1) IBISSignal(ibis,status,0);
   
   status = IBISColumnWrite(ibis,(char*) outl,3,1,nrec);
   if (status!=1) IBISSignal(ibis,status,0);
   status = IBISColumnWrite(ibis,(char*) outs,4,1,nrec);
   if (status!=1) IBISSignal(ibis,status,0);
   
   status = IBISFileClose(ibis,0);
   return;
}
