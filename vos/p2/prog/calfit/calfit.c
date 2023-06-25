#include <stdlib.h>
#include <math.h>

#include "vicmain_c.h"
#include "applic.h"
#include "defines.h"
#include "zmabend.h"
#include "zifmessage.h"

#include "cartoMemUtils.h"
#include "cartoSortUtils.h"
#include "cartoLsqUtils.h"

/*  per pixel calibration using least squares fit A. Zobrist    02/12/06   */

void sortrec7(key,ptr,len)
   int *ptr,len;
   float *key;
{
   int i;
   float *temp;

   if (len<2) return;
   if ((temp=(float *)malloc(4*len))==NULL)
          zmabend("malloc failed");
   for (i=0;i<len;i++) temp[i] = key[i];
   for (i=0;i<len;i++) key[i] = temp[ptr[i]-1];
   free(temp);
   return;
}

void main44(void)
{
   int i,iout,jout,k,inpcnt,vunit[48],inl[48],ins[48],pixsiz,dummy,ptr;
   int ounit1,ounit2,ounit3,filcount,fildef,ier,vsize[4],mode,ntot;
   int *dnsort,inverse;
   short int **buf;
   char infil[48][99],outfil[48][99];
   float calval[48],**outa,**outb,**outq,c2,d2,*dn0,*dnn;
   double eps,csol[3],clsq[144],clsqdep[48],sum0=0,sumn=0,avg0,avgn,y0,yn;
   
   zifmessage("CALFIT version 2019-08-22");
   
   /* get some parms */
   
   zvparm("CALVAL",calval,&inpcnt,&dummy,48,0);
   zvparm("INP",infil,&filcount,&fildef,48,99);
   if (filcount!=inpcnt) zmabend("number of files and calval must be equal");
   zvparm("SIZE",vsize,&k,&dummy,4,0);
   if (vsize[0]==1) zvp("SL",&vsize[0],&dummy);
   if (vsize[1]==1) zvp("SS",&vsize[1],&dummy);
   if (vsize[2]==0) zvp("NL",&vsize[2],&dummy);
   if (vsize[3]==0) zvp("NS",&vsize[3],&dummy);
   mode = 0;
   if (zvptst("calibmed")) mode = 1;
   if (zvptst("fitonly")) mode = 2;
   inverse = zvptst("inverse");
   
   /* open and read the files */
   
   for (i=0;i<inpcnt;i++)
      {
      zvunit(&vunit[i],"INP",i+1, NULL);
      zvopen(vunit[i],"OPEN_ACT","SA",
                 "IO_ACT","SA","U_FORMAT","HALF", NULL);
      zvget(vunit[i],"NL",&inl[i],"NS",&ins[i],"PIX_SIZE",&pixsiz, NULL);
      if (i==0)
         {
         if (vsize[2]==0) vsize[2] = inl[0];
         if (vsize[3]==0) vsize[3] = ins[0];
         }
      else
         {
         if (vsize[2]!=inl[i]) zmabend("images must have identical size");
         if (vsize[3]!=ins[i]) zmabend("images must have identical size");
         }
      }
   
   zvparm("OUT",outfil,&filcount,&fildef,48,99);
   if (filcount<2||filcount>3)
      zmabend("must have 2 (linearfit) or 3 (quadratic) output files");
   
   zvunit(&ounit1,"OUT",1, NULL);
   zvopen(ounit1,"U_NL",vsize[2],"U_NS",vsize[3],"U_FORMAT","REAL",
	"OP","WRITE","OPEN_ACT","SA","IO_ACT","SA","O_FORMAT","REAL", NULL);
   zvunit(&ounit2,"OUT",2, NULL);
   zvopen(ounit2,"U_NL",vsize[2],"U_NS",vsize[3],"U_FORMAT","REAL",
	"OP","WRITE","OPEN_ACT","SA","IO_ACT","SA","O_FORMAT","REAL", NULL);
   if (filcount==3)
      {
      zvunit(&ounit3,"OUT",3, NULL);
      zvopen(ounit3,"U_NL",vsize[2],"U_NS",vsize[3],"U_FORMAT","REAL",
	   "OP","WRITE","OPEN_ACT","SA","IO_ACT","SA","O_FORMAT","REAL", NULL);
      }
   
   /* dynamically allocate the buffers */
   
   mz_alloc2((unsigned char ***)&outa,vsize[2],vsize[3],4);
   mz_alloc2((unsigned char ***)&outb,vsize[2],vsize[3],4);
   if (filcount==3) mz_alloc2((unsigned char ***)&outq,vsize[2],vsize[3],4);
   mz_alloc2((unsigned char ***)&buf,inpcnt,vsize[3],2);
   
   /* read the input lines and solve for linear fits per pixel */
   
   for (iout=0;iout<vsize[2];iout++)
      {
      for (jout=0;jout<inpcnt;jout++)
         {
         zvread(vunit[jout],buf[jout],
            "LINE",iout+vsize[0],"SAMP",vsize[1],
            "NSAMPS",vsize[3], NULL);
         }
      for (k=0;k<vsize[3];k++)
         {
         for (jout=0;jout<inpcnt;jout++)
            {
            if (inverse)
               {
               clsq[jout] = (float)buf[jout][k]; /* will be overwritten */
               clsq[jout+inpcnt] = 1.0;
               clsq[jout+2*inpcnt] = clsq[jout]*clsq[jout];
               clsqdep[jout] = calval[jout];
               }
            else
               {
               clsq[jout] = calval[jout]; /* will be overwritten */
               clsq[jout+inpcnt] = 1.0;
               clsq[jout+2*inpcnt] = clsq[jout]*clsq[jout];
               clsqdep[jout] = (float)buf[jout][k];
               }
            }
         eps = 1.e-7;
         if (filcount==3) lsqfit(clsq,clsqdep,inpcnt,3,csol,eps,&ier);
                     else lsqfit(clsq,clsqdep,inpcnt,2,csol,eps,&ier);
         if (ier!=0)
            {
            outa[iout][k] = 0.0;
            outb[iout][k] = 0.0;
            if (filcount==3) outq[iout][k] = 0.0;
            }
         else
            {
            outa[iout][k] = csol[0];
            outb[iout][k] = csol[1];
            if (filcount==3) outq[iout][k] = csol[2];
            }
         }
      }
   if (mode==2) goto alldone;
   
   /* calculate the average or median at end points over all pixels*/
   
   if (filcount==3) zmabend("quadratic fit can only be used in FITONLY mode");
   if (mode==0)
      {
      for (iout=0;iout<vsize[2];iout++)
         {
         sum0 = 0.0;
         sumn = 0.0;
         for (k=0;k<vsize[3];k++)
            {
            sum0 += (double)(outa[iout][k]*calval[0]+outb[iout][k]);
            sumn += (double)(outa[iout][k]*calval[inpcnt-1]+outb[iout][k]);
            }
         }
      avg0 = sum0/(double)inpcnt;
      avgn = sumn/(double)inpcnt;
      }
   else
      {
      ntot = vsize[2]*vsize[3];
      mz_alloc1((unsigned char **)&dnsort,ntot,4);
      mz_alloc1((unsigned char **)&dn0,ntot,4);
      mz_alloc1((unsigned char **)&dnn,ntot,4);
      for (iout=0;iout<ntot;iout++) dnsort[iout] = iout+1;
      ptr = 0;
      for (iout=0;iout<vsize[2];iout++)
         {
         for (k=0;k<vsize[3];k++)
            {
            dn0[ptr] = (double)(outa[iout][k]*calval[0]+outb[iout][k]);
            dnn[ptr++] = (double)(outa[iout][k]*calval[inpcnt-1]+outb[iout][k]);
            }
         }
      sort7(dnn,dnsort,ntot);
      sortrec7(dn0,dnsort,ntot);
      avg0 = dn0[ntot/2];
      avgn = dnn[ntot/2];
      }
 
   /* per pixel, resolve the outa and outb */
   
   for (iout=0;iout<vsize[2];iout++)
      {
      for (k=0;k<vsize[3];k++)
         {
         y0 = (double)(outa[iout][k]*calval[0]+outb[iout][k]);
         yn = (double)(outa[iout][k]*calval[inpcnt-1]+outb[iout][k]);
         if (y0!=yn)
            {
            c2 = (avg0-avgn)/(y0-yn);
            d2 = avg0-c2*y0;
            }
         else
            {
            c2 = 0.0;
            d2 = 0.0;
            }
         outa[iout][k] = c2;
         outb[iout][k] = d2;
         }
      }
   
   alldone:
   for (iout=0;iout<vsize[2];iout++)
      {
      zvwrit(ounit1,outa[iout],"LINE",iout+1,"SAMP",1,"NSAMPS",vsize[3], NULL);
      zvwrit(ounit2,outb[iout],"LINE",iout+1,"SAMP",1,"NSAMPS",vsize[3], NULL);
      if (filcount==3)
         zvwrit(ounit3,outq[iout],"LINE",iout+1,"SAMP",1,"NSAMPS",vsize[3], NULL);
      }
   
   for (i=0;i<inpcnt;i++) zvclose(vunit[i], NULL);
   zvclose(ounit1, NULL);
   zvclose(ounit2, NULL);
   if (filcount==3) zvclose(ounit3, NULL);
   return;
}
