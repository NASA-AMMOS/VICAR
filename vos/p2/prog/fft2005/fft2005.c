#include <math.h>
#include <string.h>

#include "vicmain_c.h"
#include "applic.h"
#include "defines.h"

#include "my_fftw3.h"

#include "cartoMemUtils.h"

/*  fourier transform using fftw codes   A. Zobrist    09/21/05   */
/*  modified to perform quad swapping
    before doing fft                     P. Kim        11/29/07   */
/* prototypes */
int getCorrespondingIndex(int i, int ns);
void preQSwap(fftw_complex *in, int insect[]);
void normalize(fftw_complex *in, int insect[]);
void main44(void);
/*=========================================================*/
int getCorrespondingIndex(i, ns)
  int i, ns;
{
   if(i < ns/2) return (ns+1)/2 + i;

   return i-(ns/2);
}

/*=========================================================*/
void preQSwap(in, insect)
   fftw_complex *in;
   int insect[];
{
   int i, j;
   fftw_complex *tmp;

   tmp = fftw_malloc(sizeof(fftw_complex)*insect[0]*insect[1]);
   for(i = 0; i < insect[0]; i++)
      for(j = 0; j < insect[1]; j++)
      {
         int inI, inJ;
	 inI = getCorrespondingIndex(i, insect[0]);
	 inJ = getCorrespondingIndex(j, insect[1]);

         tmp[inI*insect[1]+inJ][0] = in[i*insect[1]+j][0];
      }

   for(i = 0; i < insect[0]; i++)
      for(j = 0; j < insect[1]; j++)
	in[i*insect[1]+j][0] = tmp[i*insect[1]+j][0];
}

/*=========================================================*/
void normalize(in, insect)
   fftw_complex *in;
   int insect[];
{
   int i, j;
   double sum;

   sum = 0.0;
   for(i=0;i<insect[0];i++)
      for (j=0;j<insect[1];j++) sum += in[i*insect[1]+j][0];

   if(sum == 0.0) return;

   for(i=0; i < insect[0]; i++)
      for(j = 0; j < insect[1]; j++) in[i*insect[1]+j][0] /= sum; 
}

/*=========================================================*/
void main44(void)
{
   int i,j,lnl,lns,i_unit,o_unit,vsize[4],sizepcnt,sizedef,dummy,status;
   int pixsiz,pcount,pdef,quadswap,lquad,squad,ii,jj,binsect,insect[2];
   int ishift,jshift;
   float *buf,fac,roundint;
   char i_fmt_str[10],o_fmt_str[10],modestr[10];
   int norm, preqswap;

   fftw_complex *in,*out;
   fftw_plan p;
   
   zifmessage("fft2005 version 2015-11-24");
   
   /* get some parms */
   
   zvparm("SIZE",vsize,&sizepcnt,&sizedef,4,0);
   if (vsize[0]==1) zvp("SL",&vsize[0],&dummy);
   if (vsize[1]==1) zvp("SS",&vsize[1],&dummy);
   if (vsize[2]==0) zvp("NL",&vsize[2],&dummy);
   if (vsize[3]==0) zvp("NS",&vsize[3],&dummy); 
   zvparm("format",o_fmt_str,&pcount,&pdef,4,0);
   zvparm("mode",modestr,&pcount,&pdef,7,0);
   if (strcmp(modestr,"FORWARD")!=0&&strcmp(modestr,"INVERSE")!=0)
       strcpy(modestr,"FORWARD");
   if (strcmp(modestr,"FORWARD")==0&&strcmp(o_fmt_str,"COMP")!=0)
       strcpy(o_fmt_str,"COMP");
   quadswap = zvptst("quadswap");
   zvparm("INSECT",insect,&pcount,&pdef,2,0);
   binsect = insect[0]!=0;
   preqswap = zvptst("preqswap");
   norm = zvptst("norm");

   /* open the files, input: read to complex */
      
   status = zvunit(&i_unit,"INP",1, NULL);
   status = zvopen(i_unit,"OP","READ","U_FORMAT","COMP",
      "OPEN_ACT","SA","IO_ACT","SA", NULL);
   zvget(i_unit,"FORMAT",i_fmt_str,"NL",&lnl,"NS",&lns,"PIX_SIZE",&pixsiz, NULL);
   if (vsize[2]==0) vsize[2] = lnl;
   if (vsize[3]==0) vsize[3] = lns;
   if (insect[0]==0) insect[0] = vsize[2];
   if (insect[1]==0) insect[1] = vsize[3];

/*    printf ("i_fmt_str = %s  modestr = %s\n",i_fmt_str,modestr);  */
      
   status=zvunit( &o_unit,"OUT",1, NULL);
   status=zvopen( o_unit,"U_NL",insect[0],"U_NS",insect[1],"U_FORMAT","COMP",
     "O_FORMAT",o_fmt_str,"OP","WRITE","OPEN_ACT","SA","IO_ACT","SA", NULL);
   if (strcmp(modestr,"INVERSE")==0&&strcmp(i_fmt_str,"COMP")!=0)
       zmabend("??E - Inverse transform must use complex image input");

   /* check preconditions on parameters */
   if(!strcmp("COMP", i_fmt_str) && norm)
      zmabend("??E - Normalization of complex input is not yet implemented.");
   if(!strcmp("INVERSE", modestr) && quadswap)
      zmabend("??E - Quadswap of inverse option is not yet implemented.");
   if(!strcmp("INVERSE", modestr) && preqswap)
      zmabend("??E -Preqswap of inverse option is not yet implemented.");
   
   /* dynamically allocate the buffers */
   
   mz_alloc1((unsigned char **)&buf,2*insect[1],4);
   in = fftw_malloc(sizeof(fftw_complex)*insect[0]*insect[1]);
   out = fftw_malloc(sizeof(fftw_complex)*insect[0]*insect[1]);
   
   /* read the input, for any VICAR type in, buf is r1,i1,r2,i2, */
   
   lquad = insect[0]/2;
   squad = insect[1]/2;
   if (binsect)
      {
      ishift = lquad-lnl/2;
      jshift = squad-lns/2;
      for (i=0;i<insect[0];i++)
         for (j=0;j<insect[1];j++)
            {
            in[i*insect[1]+j][0] = 0.0;
            in[i*insect[1]+j][1] = 0.0;
            }
      }
   else { ishift = 0; jshift = 0; }
   for (i=0;i<vsize[2];i++)
      {
      zvread(i_unit,buf,"LINE",i+vsize[0],"SAMP",vsize[1],
         "NSAMPS",vsize[3], NULL);
      for (j=0;j<vsize[3];j++)
         {
         ii = i+ishift;
         jj = j+jshift;
         in[ii*insect[1]+jj][0] = buf[j*2];
         in[ii*insect[1]+jj][1] = buf[j*2+1];
         }
      }
   
   /* print of in[] to test insect */
   /*
   printf("insect 0: %d insect 1: %d\n", insect[0], insect[1]);
   for (i=0;i<insect[0];i++)
      {
      printf("\n");
      for (j=0;j<insect[1];j++) printf(" %6.6f",in[i*insect[1]+j][0]);
      }
   printf("\n");
   */
   /* end - print of in[] to test insect */

   /* preqswap if user  specified */
   if(preqswap) preQSwap(in, insect);

   /* normalize if user-specified */
   if(norm) normalize(in, insect);

   /*
   for (i=0;i<insect[0];i++)
      {
      printf("\n");
      for (j=0;j<insect[1];j++) printf(" %6.6f",in[i*insect[1]+j][0]);
      }
   printf("\n");
   */

   /* call fftw */
   
   if (strcmp(modestr,"FORWARD")==0)
      p = fftw_plan_dft_2d(insect[0],insect[1],in,out,FFTW_FORWARD,
         FFTW_ESTIMATE);
   else
      p = fftw_plan_dft_2d(insect[0],insect[1],in,out,FFTW_BACKWARD,
         FFTW_ESTIMATE);
   fftw_execute(p);
   fftw_destroy_plan(p);
   
   /* write out the result */
   
   if (strcmp(modestr,"INVERSE")==0)
      {
      fac = 1.0/(insect[0]*insect[1]);
      if ((float)strcmp(o_fmt_str,"REAL")==0||strcmp(o_fmt_str,"COMP")==0)
         roundint = 0.0; else roundint = 0.5;
      }
   else { fac = 1.0; roundint = 0.0; }
   lquad = (insect[0]+1)/2;
   squad = (insect[1]+1)/2;
   for (i=0;i<insect[0];i++)
      {
      for (j=0;j<insect[1];j++)
         {
         if (quadswap)
            {
            ii = (i+lquad)%insect[0];
            jj = (j+squad)%insect[1];
            }
         else { ii = i; jj = j; }
         buf[j*2] = out[ii*insect[1]+jj][0]*fac+roundint;
         buf[j*2+1] = out[ii*insect[1]+jj][1]*fac+roundint;
         }
      zvwrit(o_unit,buf,"LINE",i+1,"SAMP",1,"NSAMPS",insect[1], NULL);
      }
   
   zvclose(i_unit, NULL);
   zvclose(o_unit, NULL);
   return;
}
