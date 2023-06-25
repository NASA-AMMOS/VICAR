#include <math.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include "vicmain_c.h"
#include "defines.h"
#include "ibisfile.h"
#include "ibiserrs.h"
#include "zmabend.h"
#include "zifmessage.h"

#include "cartoMemUtils.h"

/*=========================================================*/
float doAdd2(f1, f2)
   float f1, f2;
{
   return f1+f2;
}

/*=========================================================*/
float doSub2(f1, f2)
   float f1, f2;
{
   return f1-f2;
}

/*=========================================================*/
float doMult2(f1, f2, f3, f4)
   float f1, f2, f3, f4;
{
   return f1*f3-f2*f4;
}

/*=========================================================*/
float doDiv2(f1, f2, f3, f4)
   float f1, f2, f3, f4;
{
   if(f3*f3+f4*f4 == 0) return 0;

   return (f1*f3+f2*f4)/(f3*f3+f4*f4);
}

/*=========================================================*/
float doCabs(f1, f2)
   float f1, f2;
{
   return sqrt(f1*f1+f2*f2);
}

/*=========================================================*/
void checkParams(sl, ss, nl, ns, nl1, ns1)
   int sl, ss, nl, ns, nl1, ns1;
{
  /*printf("sl: %d ss: %d nl: %d ns: %d nl1: %d ns1: %d\n", sl, ss, nl, ns, nl1, ns1);*/
   if(ss-1+ns > ns1)
      zmabend("The input ss + ns is greater than the image sample size.\n");
   if(sl-1+nl > nl1)
      zmabend("The input sl + nl is greater than the image line size.\n");
}

/*=========================================================*/
void makeComplex(inBuf, ns)
   unsigned char *inBuf;
   int ns;
{
   unsigned char* tmpBuf;
   int i;

   mz_alloc1(&tmpBuf, ns*2, sizeof(float));

   for(i = 0; i < ns; i++)
   {
      ((float*)tmpBuf)[i*2] = ((float*)inBuf)[i];
      ((float*)tmpBuf)[i*2+1] = 0.0;
   }

   memcpy(inBuf, tmpBuf, ns*2*sizeof(float));

   free(tmpBuf);
}

/*=========================================================*/
void main44(void)
{
   int units[2], outUnit, numOfInImages;
   int add, sub, mult, div, cabs, conj, comp, real2compreal, real2compimag, comp2real, comp2imag;
   int sl, ss, nl, ns, pixSize, status;
   int ns1=0, ns2=0, nl1=0, nl2=0;
   unsigned char *inBuf1=NULL, *inBuf2=NULL, *outBuf=NULL;
   int i, linCnt, size[4], cnt, imgSize, scalar;
   char fmt1[8], fmt2[8];

   zifmessage("F2COMP version 2019-08-22");

   /* getting and setting up the parameters */
   zvpcnt("inp", &numOfInImages);

   zvp("SL", &sl, &cnt);
   zvp("SS", &ss, &cnt);
   zvp("NL", &nl, &cnt);
   zvp("NS", &ns, &cnt);
   scalar = zvptst("SCALAR");
   zvp("SIZE", size, &cnt);
   if(sl == 1 && size[0] != 1) sl = size[0];
   if(ss == 1 && size[1] != 1) ss = size[1];
   if(nl == 0 && size[2] != 0) nl = size[2];
   if(ns == 0 && size[3] != 0) ns = size[3];
   if(sl == 1 && ss == 1 && nl == 0 && ns == 0) imgSize=1;
   else imgSize=0;

   status = zvunit(&units[0], "inp", 1, NULL);
   if(status != 1) IBISSignalU(units[0], status, 1);
   status = zvopen(units[0], "OP", "READ", "OPEN_ACT", "SA", "LAB_ACT", "SA", NULL);
   if(status != 1) IBISSignalU(units[0], status, 1);
   status = zvget(units[0], "PIX_SIZE", &pixSize, "NL", &nl1, "NS", &ns1, "FORMAT", fmt1, NULL);
   if(status != 1) IBISSignalU(units[0], status, 1);
   if(!imgSize) checkParams(sl, ss, nl, ns, nl1, ns1);
   mz_alloc1(&inBuf1, ns1*2, pixSize);

   if(numOfInImages == 2)
   {
      status = zvunit(&units[1], "inp", 2, NULL);
      if(status != 1) IBISSignalU(units[1], status, 1);
      status = zvopen(units[1], "OP", "READ", "OPEN_ACT", "SA", "LAB_ACT", "SA", NULL);
      if(status != 1) IBISSignalU(units[1], status, 1);
      status = zvget(units[1], "PIX_SIZE", &pixSize, "NL", &nl2, "NS", &ns2, "FORMAT", fmt2, NULL);
      if(status != 1) IBISSignalU(units[1], status, 1);
      if(!imgSize) checkParams(sl, ss, nl, ns, nl2, ns2);
      mz_alloc1(&inBuf2, ns2*2, pixSize);
   }

   add = zvptst("ADD");
   sub = zvptst("SUB");
   mult = zvptst("MULT");
   div = zvptst("DIV");
   cabs = zvptst("CABS");
   conj = zvptst("CONJ");
   comp = zvptst("COMP");
   real2compreal = zvptst("REAL2COMPREAL");
   real2compimag = zvptst("REAL2COMPIMAG");
   comp2real = zvptst("COMP2REAL");
   comp2imag = zvptst("COMP2IMAG");
   if(add+sub+mult+div+cabs+conj+comp+real2compreal+real2compimag+comp2real+comp2imag != 1)
      zmabend("Please choose one function to perform.\n");

   /*making sure the number of files is correct*/
   if(add || sub || mult || div || comp)
   {
      if(numOfInImages != 2)
         zmabend("The option specified requires 2 input images.\n");
   }
   else if(numOfInImages != 1)
      zmabend("The option specified requires 1 input image. \n");

   /*making sure the file types are correct*/
   if((add || sub || mult || div) && !strcmp(fmt1, "REAL") && !strcmp(fmt2, "REAL"))
      zmabend("At least 1 input file must be complex.\n");
   else if(comp)
   {
      if(strcmp(fmt1, "REAL") || strcmp(fmt2, "REAL"))
         zmabend("The option specified requires both input files to be real. \n");
   }
   else if(real2compreal || real2compimag)
   {  
      if(!strcmp(fmt1, "COMP"))
         zmabend("The option specified requires 1 real input file. \n");
   }
   else if(strcmp(fmt1, "COMP"))
      zmabend("The option specified requires 1 complex input file. \n");

   /*determining the image size*/
   if(imgSize)
   {
      sl = ss = 1;
      if(numOfInImages == 1) nl = nl1;
      else nl = (nl1 <= nl2) ? nl1 : nl2;
      if(numOfInImages == 1) ns = ns1;
      else ns = (ns1 <= ns2) ? ns1 : ns2;
   }

   status = zvunit(&outUnit, "out", 1, NULL);
   if(status != 1) IBISSignalU(units[1], status, 1);

   /*open files*/
   if(cabs || comp2real || comp2imag)
   {
      status = zvopen(outUnit, "OP", "WRITE", "O_FORMAT", "REAL", "U_FORMAT", "REAL", "U_NL", nl, "U_NS", ns, "OPEN_ACT", "SA", "LAB_ACT", "SA", NULL);
      if(status != 1) IBISSignalU(outUnit, status, 1);
      mz_alloc1(&outBuf, ns, pixSize);
   }
   else
   {
      status = zvopen(outUnit, "OP", "WRITE", "O_FORMAT", "COMP", "U_FORMAT", "COMP", "U_NL", nl, "U_NS", ns, "OPEN_ACT", "SA", "LAB_ACT", "SA", NULL);
      if(status != 1) IBISSignalU(outUnit, status, 1);
      mz_alloc1(&outBuf, ns*2, pixSize);
   }

   /*perform calculations*/
   linCnt = 1;
   for(i = sl; i < sl+nl; i++)
   {
      int j;

      status = zvread(units[0], inBuf1, "LINE", i, "SAMP", ss, "NSAMPS", ns, NULL);
      if(status != 1) IBISSignalU(units[0], status, 1);
      if(!strcmp(fmt1, "REAL") && !comp && !real2compreal && !real2compimag)
         makeComplex(inBuf1, ns1);

      if(numOfInImages == 2)
      {
         status = zvread(units[1], inBuf2, "LINE", i, "SAMP", ss, "NSAMPS", ns, NULL);
         if(status != 1) IBISSignalU(units[1], status, 1);
         if(!strcmp(fmt2, "REAL") && !comp)
            makeComplex(inBuf2, ns2);
      }

      /*      for(j = 0; j < ns*2; j++) printf("inBuf1: %f inBuf2: %f\n", ((float*)inBuf1)[j], ((float*)inBuf2)[j]);*/

      for(j = 0; j < ns*2; j++)
      {
	 if(numOfInImages == 2)
	 {
            if(add)
               ((float*)outBuf)[j] = doAdd2(((float*)inBuf1)[j], ((float*)inBuf2)[j]);
            else if(sub)
               ((float*)outBuf)[j] = doSub2(((float*)inBuf1)[j], ((float*)inBuf2)[j]);
            else if(mult)
	    {
	       if(!scalar)
	       {
                  ((float*)outBuf)[j] = doMult2(((float*)inBuf1)[j], ((float*)inBuf1)[j+1], ((float*)inBuf2)[j], ((float*)inBuf2)[j+1]);
	          ((float*)outBuf)[j+1] = doMult2(((float*)inBuf1)[j], -((float*)inBuf1)[j+1], ((float*)inBuf2)[j+1], ((float*)inBuf2)[j]);
	       }
	       else
	       {
                  ((float*)outBuf)[j] = ((float*)inBuf1)[j] * ((float*)inBuf2)[j];
                  ((float*)outBuf)[j+1] = ((float*)inBuf1)[j+1] * ((float*)inBuf2)[j];
	       }
	       j++;
	    }
            else if(div)
	    {
	       if(!scalar)
	       {
                  ((float*)outBuf)[j] = doDiv2(((float*)inBuf1)[j], ((float*)inBuf1)[j+1], ((float*)inBuf2)[j], ((float*)inBuf2)[j+1]);
                  ((float*)outBuf)[j+1] = doDiv2(((float*)inBuf1)[j+1], -((float*)inBuf1)[j], ((float*)inBuf2)[j], ((float*)inBuf2)[j+1]);
	       }
	       else
	       {
                  ((float*)outBuf)[j] = ((float*)inBuf1)[j] / ((float*)inBuf2)[j];
                  ((float*)outBuf)[j+1] = ((float*)inBuf1)[j+1] / ((float*)inBuf2)[j];
	       }
	       j++;
	    }
	    else if(comp)
	    {
 	       ((float*)outBuf)[j] = ((float*)inBuf1)[j/2];
	       ((float*)outBuf)[j+1] = ((float*)inBuf2)[j/2];
	       j++;
	    }
	 }
	 /*if one of the data sets is real*/
	 else if(real2compreal)
	 {
	    if(j%2 == 0) ((float*)outBuf)[j] = ((float*)inBuf1)[j/2];
	    else ((float*)outBuf)[j] = 0;
	 }
	 else if(real2compimag)
	 {
	    if(j%2 == 1) ((float*)outBuf)[j] = ((float*)inBuf1)[j/2];
	    else ((float*)outBuf)[j] = 0;
         }
	 else if(comp2real)
	 {
	    ((float*)outBuf)[j/2] = ((float*)inBuf1)[j];
	    j++;
	 }
	 else if(comp2imag)
	 {
	    ((float *)outBuf)[j/2] = ((float*)inBuf1)[j+1];
	    j++;
	 }
	 else if(cabs)
         {
            ((float*)outBuf)[j/2] = doCabs(((float*)inBuf1)[j], ((float*)inBuf1)[j+1]);
	    j++;
	 }
	 else if(conj)
	 {
	    ((float*)outBuf)[j] = ((float*)inBuf1)[j];
	    ((float*)outBuf)[j+1] = ((float*)inBuf1)[j+1] * -1;
	    j++;
	 }
      }

      status = zvwrit(outUnit, ((float*)outBuf), "LINE", linCnt++, NULL);
      if(status != 1) IBISSignalU(outUnit, status, 1);
   }

   /*free buffers and close files*/
   free(inBuf1);
   if(numOfInImages == 2) free(inBuf2);
   free(outBuf);
   zvclose(units[0], NULL);
   if(numOfInImages == 2) zvclose(units[1], NULL);
   zvclose(outUnit, NULL);
}
