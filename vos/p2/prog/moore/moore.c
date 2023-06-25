#include <math.h>
#include <stdio.h>
#include <string.h>

#include "vicmain_c.h"
#include "applic.h"
#include "defines.h"

#include "cartoMemUtils.h"

#ifndef MIN
#define MIN(a,b)	(((a)<(b))?(a):(b))
#endif

/************************************************************************/
/* program moore                                                      */
/************************************************************************/
/*  07-00 ...alz... initial version                     */
/************************************************************************/

int *pstack,*istack,*vstack,debug;

void propagate(ival,moorebuf,previx,i,dfeather,tns,currix)
   int ival,previx,i,dfeather,tns,currix;
   short int **moorebuf;
{
   int ptr,tp,ti,tv,upix;

   ptr = 1;
   pstack[0] = previx;
   istack[0] = i;
   vstack[0] = ival;
   
   /* only 3 directions needed for this Moore algorithm because of the
   downward sweep of the line read/write */
   
   do
      {
      ptr--;
      tp = pstack[ptr];
      ti = istack[ptr];
      tv = vstack[ptr];
      moorebuf[tp][ti] = tv;
      if (ti+1<tns)
         {
         if ((int)moorebuf[tp][ti+1]>tv+1)
            {
            pstack[ptr] = tp;
            istack[ptr] = ti+1;
            vstack[ptr++] = tv+1;
            }
         }
      if (ti>0)
         {
         if ((int)moorebuf[tp][ti-1]>tv+1)
            {
            pstack[ptr] = tp;
            istack[ptr] = ti-1;
            vstack[ptr++] = tv+1;
            }
         }
      upix = (tp-1+dfeather)%dfeather;
      if (upix!=currix)
         {
         if ((int)moorebuf[upix][ti]>tv+1)
            {
            pstack[ptr] = upix;
            istack[ptr] = ti;
            vstack[ptr++] = tv+1;
            }
         }
      }
   while (ptr>0);
   debug = 0;
   return;
}

void leftprop(ival,moorebuf,currix,i,tns)
   int ival,currix,i,tns;
   short int **moorebuf;
{
   int ptr,tp,ti,tv;

   ptr = 1;
   pstack[0] = currix;
   istack[0] = i;
   vstack[0] = ival;
            
   /* only 1 direction needed for leftward propagation, this could
   be more efficient, but it was easier to copy the code above */
   
   do
      {
      ptr--;
      tp = pstack[ptr];
      ti = istack[ptr];
      tv = vstack[ptr];
      if(ti < 0) zmabend("-1 detected.\n");
      moorebuf[tp][ti] = tv;
      if (ti>0)
         {
         if ((int)moorebuf[tp][ti-1]>tv+1)
            {
            pstack[ptr] = tp;
            istack[ptr] = ti-1;
            vstack[ptr++] = tv+1;
            }
         }
      }
   while (ptr>0);
   return;
}

void main44(void)
{
   int i,j,dmax,count,edgeval,outone,growone,i_unit,nl,ns;
   int o_unit,dmaxp1,iline,previx,currix,status;
   short int **inbuf,**moore,newmoore,prevmoore;
   char fmt_str[10];
   
   zifmessage("moore version 2015-12-09");
   
   /* fetch params */
   
   zvp("DMAX",&dmax,&count);
   edgeval = zvptst("edgeval");
   outone = zvptst("outone");
   growone = zvptst("growone");
   dmaxp1 = dmax+1; /* this is a shutoff value */
   
   /* open input image file */
   
   status = zvunit( &i_unit, "INP", 1, NULL);
   status = zvopen( i_unit, "OPEN_ACT", "SA", "IO_ACT", "SA",
			"U_FORMAT","HALF", NULL);
   zvget(i_unit,"FORMAT",fmt_str, NULL);
   if (strcmp(fmt_str,"BYTE")&&strcmp(fmt_str,"HALF")) 
      zmabend("Invalid input data format.  Use BYTE or HALF.");
   zvget(i_unit,"NL",&nl, NULL);
   zvget(i_unit,"NS",&ns, NULL);
   
   /* dynamically allocate storage */
   
   mz_alloc2((unsigned char ***)&inbuf,dmaxp1,ns,2);
   mz_alloc2((unsigned char ***)&moore,dmaxp1,ns,2);
   for (i=0;i<dmaxp1;i++)
      for (j=0;j<ns;j++)
         {
         inbuf[i][j] = 0;
         moore[i][j] = (short int)dmaxp1;
         }
   
   mz_alloc1((unsigned char **)&pstack,ns*dmaxp1,4);
   mz_alloc1((unsigned char **)&istack,ns*dmaxp1,4);
   mz_alloc1((unsigned char **)&vstack,ns*dmaxp1,4);
   
   /* open output image file */
   
   status=zvunit(&o_unit,"OUT",1, NULL);
   status=zvopen(o_unit,"U_NL",nl,"U_NS",ns,
	"OP","WRITE","OPEN_ACT","SA","IO_ACT","SA", NULL);
   
  
   /* roll the barrel, performing the Moore algorithm */
   
   debug = 1;
   currix = dmax;
   for (iline=0;iline<nl+dmaxp1;iline++)
      {
      previx = currix;
      currix = (currix+1)%dmaxp1;
      
      if (iline>=dmaxp1)
         {
         for (j=0;j<ns;j++) if (inbuf[currix][j]==0)
            {
            if (moore[currix][j]<dmaxp1)
               {
               if (growone) inbuf[currix][j] = 1;
                  else inbuf[currix][j] = moore[currix][j];
               }
            }
         else
            {
            if (outone) inbuf[currix][j] = 1;
            }
         zvwrit(o_unit,inbuf[currix],"LINE",iline-dmax, NULL);
         }
         
      if (iline>=nl) continue;
      status = zvread(i_unit,inbuf[currix],"LINE",iline+1, NULL);
      
      prevmoore = dmaxp1;
      for (j=0;j<ns;j++)
         {
         if (inbuf[currix][j]==0)
            {
            newmoore = MIN(dmaxp1,MIN(prevmoore,moore[previx][j])+1);
            }
         else
            {
            newmoore = edgeval*inbuf[currix][j];
            if (moore[previx][j]>newmoore+1)
               propagate(newmoore+1,moore,previx,j,dmaxp1,ns,currix);
            if ((prevmoore>newmoore+1)&&j>0)
               leftprop(newmoore+1,moore,currix,j-1,dmaxp1,ns);
            }
         moore[currix][j] = newmoore;
         prevmoore = newmoore;
         }
      }
   
   /* close and return */
   
   mz_free2((unsigned char **)inbuf,dmaxp1);
   mz_free2((unsigned char **)moore,dmaxp1);
   zvclose(i_unit, NULL);
   zvclose(o_unit, NULL);
   
   return;
}
