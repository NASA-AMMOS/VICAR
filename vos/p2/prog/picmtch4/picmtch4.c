#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "vicmain_c.h"
#include "applic.h"

#include "fftw3.h"
#include "defines.h"
#include "ibisfile.h"
#include "ibiserrs.h"
#include "zmabend.h"
#include "zifmessage.h"

#include "cartoMemUtils.h"
#include "cartoLsqUtils.h"
#include "cartoStrUtils.h"
#include "cartoGtUtils.h"
#include "cartoSortUtils.h"

#ifndef MAX
#define MAX(a,b)	(((a)>(b))?(a):(b))
#endif

#ifndef MIN
#define MIN(a,b)	(((a)<(b))?(a):(b))
#endif

/************************************************************************/
/* program picmtch4                                                     */
/*  image to image correlation, including gcp option  A. Zobrist 9/9/00 */
/************************************************************************/
/*  00-09 ...alz... initial version                                     */
/************************************************************************/

   int lnlg[2],lnsg[2],i_unit[2];
   float *chip1,*asrch;
   fftw_complex *afftin,*afftout,*bfftin,*bfftout;
   
   float rvecl[5] = {0.,1.,-1.,1.,-1.};
   float rvecs[5] = {0.,1.,-1.,-1.,1.};
   double ident[12] = {1.,0.,0.,0.,0.,0.,0.,1.,0.,0.,0.,0.};

/*=====================================================================

refine

refines the match produced by routine rfit

uses the 3 x 3 center of the fft correlation matrix and fits
a general quadratic (least squares), then sets the partial
derivatives in x and y to zero to find the max.  The max
must be within 0.7 pixels of the center to qualify (otherwise
the center itself is returned).

arguments:

     1. corr: input, float corr[3][3];
	center of the correlation matrix as
	returned by rfit
     2. vloff: input,output, float *vloff;
	the match produced by routine rfit at
	the exact center of the 3 x 3, output
	is the refined value
     3. vsoff: input,output, float *vsoff;
	the match produced by routine rfit at
	the exact center of the 3 x 3, output
	is the refined value
     4. ireferr: input,output, int *ireferr;
	error return, 0 if OK, 1 if error
*/

void refine(corr,vloff,vsoff,ireferr)
   int *ireferr;
   float corr[3][3],*vloff,*vsoff;
{
   int i,j,iq,ierror;
   double a[54],b[9],s[6],jvar,ivar,imx,jmx,eps;
   
   *ireferr = 0;
   for (i=0;i<3;i++)
      {
      ivar = (float)i-1.0;
      for (j=0;j<3;j++)
	 {
	 jvar = (float)j-1.0;
	 iq = i*3+j;
	 a[iq] = jvar*jvar;
	 a[iq+9] = jvar*ivar;
	 a[iq+18] = ivar*ivar;
	 a[iq+27] = jvar;
	 a[iq+36] = ivar;
	 a[iq+45] = 1.;
	 b[iq] = corr[i][j];
	 }
      }
   eps=1.e-7;
   lsqfit(a,b,9,6,s,eps,&ierror);
   if (ierror!=0 || s[0]==0 || (4.0*s[2]*s[0]==s[1]*s[1]))
      { printf("sing rfit"); *ireferr = 1; return; }
   imx = (s[1]*s[3]-2.0*s[4]*s[0])/(4.0*s[2]*s[0]-s[1]*s[1]);
   jmx = -(s[1]*imx+s[3])/(2.0*s[0]);
   if (imx*imx+jmx*jmx>=2.0) { *ireferr = 1; return; }
   *vloff = *vloff+imx;
   *vsoff = *vsoff+jmx;
   return;
}

/*=====================================================================

getzvl

gets a z value from a matrix by averaging over a window
and uses bilinear interpolation for fractional pixel
location

arguments:

     1. a: input, float *a;
	two dimensional square array stored as
	one dimensional (as in fortran)
     2. n: input, int n;
	array a is n x n
     3. coord: input, float coord[2];
	the pixel location to obtain z value
     4. nw: input, int nw;
	the window size to average over (must be odd)
     5. nr: input, int nr;
	the allowable number of zero values in the
	window; if exceeded -9999.0 is returned
*/

float getzvl(a,n,coord,nw,nr)
   float *a,coord[2];
   int n,nw,nr;
{
   int i,j,ill,jsl,ilu,jsu,ire,iline,isamp;
   float flu,fsu,fll,fsl,sum,val;

   iline = (int)(coord[0]-.5);
   isamp = (int)(coord[1]-.5);
   ill = iline-nw/2;
   jsl = isamp-nw/2;
   flu = coord[0]-(float)iline-.5;
   fsu = coord[1]-(float)isamp-.5;
   fll = 1.0-flu;
   fsl = 1.0-fsu;
   ilu = ill+nw;
   jsu = jsl+nw;
   if (ill<0||ilu>n) return(0.);
   if (jsl<0||jsu>n) return(0.);

   ire = 0;
   sum = 0.;
   for (i=ill;i<ilu;i++)
      for (j=jsl;j<jsu;j++)
	 {
	 val = a[i*n+j];
	 if (val<0.5) ire++;
	 if (i==ill) val *= fll;
	 if (i==(ilu-1)) val *= flu;
	 if (j==jsl) val *= fsl;
	 if (j==(jsu-1)) val *= fsu;
	 sum += val;
	 }
   if (ire<=nr) return(sum/(float)((nw-1)*(nw-1)));
	   else return(-9999.0);
}

/*=====================================================================

rfit

uses fft to compute correlation function on two images stored
in the specific global arrays chip1 and asrch.  Chip1 is a fixed
fftsize x fftsize array and asrch is a larger array for correlation
matching.  The correlation area is a srchdim x srchdim subset of the
larger array.

the method is fftinverse(fft(a) * conj(fft(b))

arguments:

     1. ilin: input, int ilin;
	line offset into asrch (the larger array)
     2. jsmp: input, int jsmp;
	sample offset into asrch (the larger array)
     3. vmax: output, float *vmax;
	the max correlation value
     4. vloff: output, float *vloff;
	the line offset of the peak match relative to the
	center of chip1 (16.0,16.0)
     5. vsoff: output, float *vsoff;
	the sample offset of the peak match relative to the
	center of chip1 (16.0,16.0)
     6. corr: output, float corr[3][3]
	the 3 x 3 part of the correlation centered at the
	peak, for use in subpixel refinement
     7. nohpf: input, int nohpf;
	if user sets to one, shuts off high pass filter
     8. srchdim: input, int srchdim;
	The current size of the search area in chip 2
     9. fftsize: input, int fftsiz
	the dimension of the fft. usually 32, 64, 128 or 256
    10. search: input, int search
        the dimension of the array containing the second
        chip

*/

void rfit(ilin,jsmp,vmax,vloff,vsoff,corr,nohpf,srchdim,fftsize,search)
   int ilin,jsmp,nohpf,srchdim,fftsize;
   float *vmax,*vloff,*vsoff,corr[3][3];
{
   int i,j,ixmax,jxmax,koff;
   int quadmark,mincor,maxcor;
   float t,v,bij,bij1,tvmax,rv,ttemp,avg;
   fftw_plan p;
   
   quadmark = srchdim/2;
   mincor = fftsize/6-1;
   maxcor = srchdim-mincor;
   
   avg = 0.0;
   for (i=0;i<fftsize;i++)
      for (j=0;j<fftsize;j++)
	 avg += chip1[j*fftsize+i];
   avg /= (float)(fftsize*fftsize);
   
   /*printf("ZERO DETECT CODE ON:ilin %d\n",ilin);  save for debugging*/
   for (i=0;i<srchdim;i++)
      for (j=0;j<srchdim;j++)
	 {
	 bfftin[j*srchdim+i][0] = asrch[(ilin+j)*search+(jsmp+i)];
         /*if (bfftin[j*srchdim+i][0]<120&&(i>56)&&(j>56)&&(i<72)&&(j<72))
            printf("B-ZERO:i,j,bfftin[j*srchdim+i][0] %d %d %7.1f\n",
               i,j,bfftin[j*srchdim+i][0]);*/
	 bfftin[j*srchdim+i][1] = 0.0;
	 afftin[j*srchdim+i][0] = avg;
	 afftin[j*srchdim+i][1] = 0.0;
	 }
   koff = (srchdim-fftsize)/2;
   for (i=0;i<fftsize;i++)
      for (j=0;j<fftsize;j++)
         {
	 afftin[(j+koff)*srchdim+i+koff][0] = chip1[j*fftsize+i];
         /*if (chip1[j*fftsize+i]==0) printf("A-ZERO:i,j %d %d\n",i,j);*/
         }
   
   p = fftw_plan_dft_2d(srchdim,srchdim,afftin,afftout,FFTW_FORWARD,FFTW_ESTIMATE);
   fftw_execute(p);
   fftw_destroy_plan(p);
   
   p = fftw_plan_dft_2d(srchdim,srchdim,bfftin,bfftout,FFTW_FORWARD,FFTW_ESTIMATE);
   fftw_execute(p);
   fftw_destroy_plan(p);
      
   afftout[0][0] = 0.;
   afftout[0][1] = 0.;
   if (!nohpf) for (i=1;i<srchdim;i++)
      {
      afftout[i][0] = 0.;
      afftout[i][1] = 0.;
      afftout[i*srchdim][0] = 0.;
      afftout[i*srchdim][1] = 0.;
      }
   
   for (i=0;i<srchdim;i++)
      for (j=0;j<srchdim;j++)
	 {
	 bij = afftout[j*srchdim+i][0]*bfftout[j*srchdim+i][0]+
               afftout[j*srchdim+i][1]*bfftout[j*srchdim+i][1];
	 bij1 = afftout[j*srchdim+i][0]*bfftout[j*srchdim+i][1]-
               afftout[j*srchdim+i][1]*bfftout[j*srchdim+i][0];
	 v = sqrt((double)(bij*bij+bij1*bij1));
	 if (v<1.e-6) v = 1.e-6;
	 bfftin[j*srchdim+i][0] = bij/v;
	 bfftin[j*srchdim+i][1] = bij1/v;
	 }
   p = fftw_plan_dft_2d(srchdim,srchdim,bfftin,bfftout,FFTW_BACKWARD,FFTW_ESTIMATE);
   fftw_execute(p);
   fftw_destroy_plan(p);

   /* quadrant swap */
   
   for (i=0;i<quadmark;i++)
      for (j=0;j<quadmark;j++)
	 {
	 t = bfftout[i*srchdim+j][0];
	 bfftout[i*srchdim+j][0] = bfftout[(i+quadmark)*srchdim+j+quadmark][0];
	 bfftout[(i+quadmark)*srchdim+j+quadmark][0] = t;
	 t = bfftout[(i+quadmark)*srchdim+j][0];
	 bfftout[(i+quadmark)*srchdim+j][0] = bfftout[i*srchdim+j+quadmark][0];
	 bfftout[i*srchdim+j+quadmark][0] = t;
	 }

   tvmax = -1.e20;
   ixmax = mincor; jxmax = mincor;
   for (i=mincor;i<maxcor;i++)
      for (j=mincor;j<maxcor;j++)
	 {
	 rv = bfftout[j*srchdim+i][0];
	 if (rv>tvmax) { tvmax = rv; ixmax = i; jxmax = j; }
	 }
   if (tvmax<0.0) tvmax = 0.0;
   
   /* normalized for varying footprints by three lines below */
   
   ttemp = log10((double)srchdim)/log10((double)2.0);
   ttemp = ttemp*ttemp;
   *vmax = tvmax*10.0/(srchdim*ttemp*ttemp);
   *vloff = (float)jxmax-quadmark;
   *vsoff = (float)ixmax-quadmark;
   /*printf("ilin,srchdim,*vloff,*vsoff,quadmark,koff %d %d %7.1f %7.1f %d %d\n",
           ilin,srchdim,*vloff,*vsoff,quadmark,koff);*/
   
   for (i=0;i<3;i++)
      for (j=0;j<3;j++)
	 corr[j][i] = bfftout[(jxmax+j-1)*srchdim+ixmax+i-1][0];

   return;
}

/*=====================================================================

getgrid

extracts a subimage from an image.  the subimage is specified by
a linear transformation and the point that (when transformed) becomes
the center of the subimage.  ndim can be used to obtain only the
central part of the array if the outer border is not needed, for
example when picmtch4 has narrowed a search.


arguments:

     1. filenum: input, int filenum;
	the filenumber returned by mc_open
     2. chip: output, float *chip;
	holds the ndim x ndim subimage as a
	one dimensional (fortran-like) array
	always floating format
     3. narray: input, int narray;
	the dimension of the chip array
     4. ndim: input, int ndim;
	how much data to put into the chip
	center; ndim <= narray, both are even
     5. trans: input, double trans[12];
	the quadratic transformation that shapes
	the grid.
     6. centrx: input, float centrx;
	trans(centrx,centry) will be the center
	of the sampled grid
     7. centry: input, float centry;
	trans(centrx,centry) will be the center
	of the sampled grid
     8. rmag: input, int rmag;
	applied to grid to obtain a larger (or
	smaller) area chip
     9. chop: output, int *chop;
	number of subimage points outside of
	input image plus zero points
*/

void getgrid(filenum,chip,narray,ndim,trans,centrx,centry,rmag,chop,pcl,pcs,zerothr)
   int filenum,narray,ndim,*chop,zerothr;
   float *chip,rmag[2],centrx,centry,pcl,pcs;
   double trans[12];
{
   float *gridx,*gridy;
   short int **imbuf;

   int i,j,lnl,lns,bord;
   int ndim2,iline,isamp,slbx,ssbx,elbx,esbx,nlbx,nsbx;
   float px,py,rline,rsamp,rlfac,rsfac,vl,vu;
   float xmin,xmax,ymin,ymax,fzerothr;
   double dvecl[2],dvecu[2];
   
   /* the line,samp calcs are based on a .5 centered pixel, however,
   the main program also passes a .5 centered request.  These cancel
   and the pixels are centered like VICAR pixels.  Since both arrays
   are offset the same amount, there is no error introduced - alz */
   
   /* get the file dimensions */

   if (narray%2==1||ndim%2==1) zmabend("getgrid error");
   lnl = lnlg[filenum-1];
   lns = lnsg[filenum-1];
   
   /* generate the grid */

   ndim2 = ndim*ndim;
   mz_alloc1((unsigned char **)&gridx,ndim2,4);
   mz_alloc1((unsigned char **)&gridy,ndim2,4);
   
   xmin = 99999999.0; xmax = -99999999.0;
   ymin = 99999999.0; ymax = -99999999.0;
   for (i=0;i<ndim;i++)
      for (j=0;j<ndim;j++)
	 {
	 px = centrx+rmag[0]*((float)(i-ndim/2)+1.0)+pcl;
	 py = centry+rmag[1]*((float)(j-ndim/2)+1.0)+pcs;
	 gridx[i*ndim+j] = trans[0]*px+trans[1]*py+trans[2]+trans[3]*px*px+
	     trans[4]*py*py+trans[5]*px*py;
	 gridy[i*ndim+j] = trans[6]*px+trans[7]*py+trans[8]+trans[9]*px*px+
	     trans[10]*py*py+trans[11]*px*py;
	 /*if (i==ndim/2&&j==ndim/2) printf("T/ZERO PT %7.1f %7.1f\n",
	      gridx[i*ndim+j],gridy[i*ndim+j]);*/
	 xmin = MIN(xmin,gridx[i*ndim+j]);
	 ymin = MIN(ymin,gridy[i*ndim+j]);
	 xmax = MAX(xmax,gridx[i*ndim+j]);
	 ymax = MAX(ymax,gridy[i*ndim+j]);
	 }
   
   /* malloc an input rectangle as bounding box, don't have to go outside image */

   slbx = MAX(0,MIN(lnl-2,(int)(xmin-1.0)));
   ssbx = MAX(0,MIN(lns-2,(int)(ymin-1.0)));
   elbx = MAX(1,MIN(lnl-1,(int)(xmax+1.0)));
   esbx = MAX(1,MIN(lns-1,(int)(ymax+1.0)));
   nlbx = elbx-slbx+1;
   nsbx = esbx-ssbx+1;
   mz_alloc2((unsigned char ***)&imbuf,nlbx,nsbx,2);
   
   /* read the image into the bounding box, outside immaterial */
   
   for (i=0;i<nlbx;i++)
      {
      zvread(i_unit[filenum-1],imbuf[i],
         "LINE",slbx+i+1,"SAMP",ssbx+1,"NSAMPS",nsbx,NULL);
      }
   
   /* now look up the image values, outside box is intercepted */

   *chop = 0;
   fzerothr = (float)zerothr+0.01;
   for (i=0;i<ndim2;i++)
      {
      rline = gridx[i]-0.5;
      rsamp = gridy[i]-0.5;
      iline = (int) (rline-0.5);
      isamp = (int) (rsamp-0.5);
      if (iline<0 || iline>=lnl-1 || isamp<0 || isamp>=lns-1)
	 { chip[i] = 0.; (*chop)++; }
      else
	 {
	 dvecl[0] = (double)(imbuf[iline-slbx][isamp-ssbx]);
	 dvecl[1] = (double)(imbuf[iline-slbx][isamp-ssbx+1]);
	 dvecu[0] = (double)(imbuf[iline-slbx+1][isamp-ssbx]);
	 dvecu[1] = (double)(imbuf[iline-slbx+1][isamp-ssbx+1]);
	 rlfac = rline-iline-0.5;
	 rsfac = rsamp-isamp-0.5;
	 vl = (1.0-rsfac)*dvecl[0]+rsfac*dvecl[1];
	 vu = (1.0-rsfac)*dvecu[0]+rsfac*dvecu[1];
	 chip[i] = (1.0-rlfac)*vl+rlfac*vu;
	 if (chip[i]<fzerothr) (*chop)++;
	 }
      }
   
   /* free storage */

   free(gridx);
   free(gridy);
   mz_free2((unsigned char **)imbuf,nlbx);
   
   /* now center in the grid, save the prints below for getgrid verif */

   /*printf("new getgrid: print raw center:\n");
   i = (narray/2-6)*narray+narray/2-6;
   for (k=0;k<11;k++)
      {
      for (j=0;j<11;j++) printf("%5.1f ",chip[i+j]);
      i += narray;
      printf("\n");
      }*/

   if (narray==ndim) return;
   bord = (narray-ndim)/2;
   for (i=ndim-1;i>=0;i--)
      for (j=0;j<ndim;j++)
         chip[(i+bord)*narray+bord+j] = chip[i*ndim+j];

   /*printf("print chip center:\n");
   i = (narray/2-6)*narray+narray/2-6;
   for (k=0;k<11;k++)
      {   
      for (j=0;j<11;j++) printf("%5.1f ",chip[i+j]);
      i += narray;
      printf("\n");
      }use first print section for most debugging*/

   return;
}


/*=====================================================================

throwout

throws out the worst point from the a,b arrays using a linear fit.


arguments:

     1. throwcount: input, int *throwcount;
	decremented by one for one throwout
     2. a: input/output, float **a;
	coefficients for the matched point
     3. b: input/output, float **b;
	coefficients for the matched point
     4. neq: input/output, int *neq;
	decremented by one for one throwout
     5. neqmax: input, int neqmax;
	the 2d dimension of the a and b arrays,
	the first dimension is fixed at 2
     
*/

void throwout(throwcount,a,b,neq,neqmax)
   int *throwcount,*neq,neqmax;
   float **a,**b;
{
   int ix,i,j,ierror,imax=0;
   float rx,ry,rmax;
   double *aa,*bb,eps,soln[12],*resid;
  
   mz_alloc1((unsigned char **)&aa,neqmax*6,8);
   mz_alloc1((unsigned char **)&bb,neqmax,8);
   mz_alloc1((unsigned char **)&resid,neqmax,8);
   
   /* solve lsq (linear), keeping quadratic form for consistency with main */
   
   for (ix=0;ix<2;ix++)
      {
      for (i=0;i<*neq;i++)
         {
         aa[i] = a[0][i];
         aa[i+(*neq)] = a[1][i];
         aa[i+2*(*neq)] = 1.;
         aa[i+3*(*neq)] = a[0][i]*a[0][i];
         aa[i+4*(*neq)] = a[1][i]*a[1][i];
         aa[i+5*(*neq)] = a[0][i]*a[1][i];
         bb[i] = b[ix][i];
         }
      eps = 1.e-7;
      lsqfit(aa,bb,(*neq),3,&soln[ix*6],eps,&ierror);
      for (j=3;j<6;j++) soln[ix*6+j] = 0.0;
      }
   
   /* calculate residuals */
   
   for (i=0;i<*neq;i++)
      {
      rx = b[0][i]-(soln[0]*a[0][i]+soln[1]*a[1][i]+soln[2]);
      ry = b[1][i]-(soln[6]*a[0][i]+soln[7]*a[1][i]+soln[8]);
      resid[i] = rx*rx+ry*ry;
      }
   
   /* discard the worst point */
   
   rmax = -1.0;
   for (i=0;i<*neq;i++)
      {
      if (resid[i]<=rmax) continue;
      rmax = resid[i];
      imax = i;
      }
   
   for (i=imax;i<(*neq)-1;i++)
      {
      a[0][i] = a[0][i+1];
      a[1][i] = a[1][i+1];
      b[0][i] = b[0][i+1];
      b[1][i] = b[1][i+1];
      }
   
   /* clean up and return */
   
   free(aa);
   free(bb);
   free(resid);
   (*throwcount)--;
   (*neq)--;
   return;
}

void main44(void)
{
   int   geocord1,geocord2,minsrch=0,nohpftae,subpix,getw,getr;
   int   nretry,gcpf,search,ffthalf,elvcor,zerothr;
   double itie[6],otie[6],rmagtae[2],rmagmin[2],rmcor,rretry,tretry,thr_res;
   float zerolim,zerolim2;
   
   float **c_data,**a,**b;
   double *aa,*bb;
   
   int iii,jjj,ie,ncol,tablen,ityp1,ityp2,solved;
   int neq,neqmax,ix,ierror,srchw,msrchw,ibig=0,jbig;
   int chop,ilin,jsmp,picout,ibigt,scount,srchdim;
   int retry,msrc,refinerr,nohpf,lastneq,ndim,itiecount,otiecount;
   int autofit,*vmaxix=NULL,autoix=0,gotthresh,neqmin,ifftsize,ffthset,throwcount;
   float getzvl(),xlt,xst,centr[2],pcntr[2];
   float vmax=0.,rmag[2];
   float vloff,vsoff,wl,ws,acentr[2],resl,ress,res,corr[3][3];
   float ocentr[2],*vmaxvec=NULL;
   double t[6],tinv[6],u[6],uinv[6],corner[4],dline,dsamp,eps,vmaxfac,thr_resp;
   double soln[12],solninv[12],angoff,rctl,rcts,rv1,rw1,rv2,rw2,rr,theta;
   
   int pcount,pdef,unit,status,ibis,parmcnt,o_unit,labnl=0,labns=0,len,fftsize;
   int choplimit1,choplimit2,bigchoplimit2,nredo,maxredo,ibigx,predfunc;   
   float retryparm[3];
   char *labelstr,magshrk[2];
   
   zifmessage("PICMTCH4 version 2019-09-06");
   
   /* get the basic parameters */
   
   gcpf = zvptst("gcpf");
   elvcor = zvptst("elvcor");
   zvparmd("itie",itie,&itiecount,&pdef,6,0);
   zvparmd("otie",otie,&otiecount,&pdef,6,0);
   if (itiecount!=otiecount) zmabend("itie count must equal otie count");
   if (itiecount!=0&&itiecount!=6) zmabend("itie/otie count must be 0 or 6");
   zvparmd("magnify",rmagtae,&pcount,&pdef,2,0);
   if (pcount==1) zmabend("magnify needs 2 inputs, or default = no inputs"); 
   zvparm("MAGSHRK",magshrk,&pcount,&pdef,1,2);
   zvparmd("magmin",rmagmin,&pcount,&pdef,2,0);
   zvparmd("thr_res",&thr_res,&pcount,&pdef,1,0);
   zvparmd("angoff",&angoff,&pcount,&pdef,1,0);
   angoff /= 57.29577;
   zvp("search",&search,&pcount);
   zvp("minsrch",&msrc,&pcount);
   zvp("fftsize",&fftsize,&pcount);
   zvp("zerothr",&zerothr,&pcount);
   if (search%2==1) search++;
   if (minsrch%2==1) minsrch++;
   if (fftsize%2==1) fftsize++;
   predfunc = zvptst("QUAD");
   zvp("redo",&nredo,&pcount);
   if (msrc<fftsize) msrc = fftsize;
   if (search<fftsize) search = fftsize;
   if (search<msrc) search = msrc;
   
   nohpftae = zvptst("nohpf");
   subpix = !zvptst("nosubpix");
   geocord1 = zvptst("geocord1");
   geocord2 = zvptst("geocord2");
   
   zvp("zwind",&getw,&pcount);
   zvp("zreject",&getr,&pcount);
   zvp("zerolim",&zerolim,&pcount);  zerolim *= 0.01;
   zvp("zerolim2",&zerolim2,&pcount);
   if (zerolim2<-0.5) zerolim2 = zerolim*100.0;
   zerolim2 *= 0.01;
   
   zvp("retry",&retryparm,&pcount);
   nretry = (int)retryparm[0]+1;
   rretry = (double)retryparm[1];
   tretry = (double)retryparm[2];
   zvp("ffthalf",&ffthalf,&pcount);
   if (ffthalf>0)
      {
      if (nretry>1) zmabend("can't use both RETRY and FFTHALF parameters");
      nretry = ffthalf+1;
      }
   
   zvp("fitmin",&rmcor,&pcount);
   zvp("autofit",&autofit,&pcount);
   if (autofit>0&&autofit<12)
      zmabend("autofit must be at least 12; or zero (autofit off)");
   if (autofit>0) 
      {
      mz_alloc1((unsigned char **)&vmaxvec,autofit,4);
      mz_alloc1((unsigned char **)&vmaxix,autofit,4);
      }
   
   rmag[0] = rmagtae[0]; rmag[1] = rmagtae[1];
   nohpf = nohpftae;
   if (getw%2==0) zmabend("zwind must be odd");
   retry = (nretry>1);
   if (retry&&gcpf) zmabend("can't retry gcpf");

   /* open the ibis interface file */

   status = zvunit(&unit,"inp",3,NULL);
   status = IBISFileOpen(unit,&ibis,"update",0,0,0,0);
   if (status!=1) IBISSignalU(unit,status,1);
   IBISFileGet(ibis,"nr",&tablen,1,1,0);
   IBISFileGet(ibis,"nc",&ncol,1,1,0);
   if (ncol<11) zmabend("ibis file needs 11 columns");
   
   /* malloc arrays, read in the cols (if not gcpf) */

   if (gcpf) zmabend("ground control point file not implemented yet");
   mz_alloc2((unsigned char ***)&c_data,15,tablen,4);
   neqmax = tablen+3+nredo;
   mz_alloc1((unsigned char **)&aa,neqmax*6,8);
   mz_alloc1((unsigned char **)&bb,neqmax,8);
   mz_alloc2((unsigned char ***)&a,2,neqmax,4);
   mz_alloc2((unsigned char ***)&b,2,neqmax,4);
   
   mz_alloc1((unsigned char **)&chip1,fftsize*fftsize,4);
   mz_alloc1((unsigned char **)&asrch,search*search,4);
   
   afftin = fftw_malloc(sizeof(fftw_complex)*search*search);
   afftout = fftw_malloc(sizeof(fftw_complex)*search*search);
   bfftin = fftw_malloc(sizeof(fftw_complex)*search*search);
   bfftout = fftw_malloc(sizeof(fftw_complex)*search*search);
   
   for (iii=0;iii<2;iii++)
      {
      status = IBISColumnSet(ibis,"U_FORMAT","REAL",iii+1);
      if (status!=1) IBISSignal(ibis,status,1);
      status = IBISColumnRead(ibis,(char*)(c_data[iii]),iii+1,1,tablen);
      if (status!=1) IBISSignal(ibis,status,1);
      }
   
   for (iii=11;iii<13;iii++)
      {
      if (elvcor)
         {
         status = IBISColumnSet(ibis,"U_FORMAT","REAL",iii+1);
         if (status!=1) IBISSignal(ibis,status,1);
         status = IBISColumnRead(ibis,(char*)(c_data[iii]),iii+1,1,tablen);
         if (status!=1) IBISSignal(ibis,status,1);
         }
      else for (jjj=0;jjj<tablen;jjj++) c_data[iii][jjj] = 0.0;
      }
   for (jjj=0;jjj<tablen;jjj++)
      {
      c_data[13][jjj] = 0.0;
      c_data[14][jjj] = 0.0;
      }
   
   /* is an output requested */
   
   status = zvpcnt("out",&parmcnt);
   picout = parmcnt>0;
   if (picout)
      {
      status=zvunit( &o_unit,"OUT",1,NULL);
      status=zvopen( o_unit,"U_NL",32,"U_NS",tablen*32,
	"OP","WRITE","OPEN_ACT","SA","IO_ACT","SA",NULL);
      }
   
   /* map points to line-samp according to image corners
      and parms, also convert itie */

   if (!geocord1)
      {
      for (iii=0;iii<6;iii++) { t[iii] = 0.; tinv[iii] = 0.; }
      t[0] = 1.; t[4] = 1.;
      tinv[0] = 1.; tinv[4] = 1.;
      }
   else
      {
      status = gtgetlab("inp",1,&labelstr,&labnl,&labns);
      if (status!=1) zmabend("Failed to get GeoTIFF label, first input");
      len = strlen(labelstr);
      for (iii=0;iii<len;iii++) labelstr[iii] = toupper(labelstr[iii]);
      status = geofix(labelstr,t,tinv,labnl,labns,corner);
      if (status!=1)
         zmabend("Failed to get mapping from GeoTIFF label, first input");
      }
   if (itiecount==0)
      {
      if (!geocord1) status = gtgetlab("inp",1,&labelstr,&labnl,&labns);
      if (!geocord1&&status!=1)
         zmabend("Failed to get GeoTIFF label, first input");
      itie[0] = 1.0;
      itie[1] = 1.0;
      itie[2] = (double)labnl;
      itie[3] = 1.0;
      itie[4] = 1.0;
      itie[5] = (double)labns;
      }
   
   for (iii=0;iii<tablen;iii++)
      {
      dline = tinv[0]*(double)(c_data[0][iii])+
		    tinv[1]*(double)(c_data[1][iii])+tinv[2];
      dsamp = tinv[3]*(double)(c_data[0][iii])+
		    tinv[4]*(double)(c_data[1][iii])+tinv[5];
      c_data[2][iii] = (float)dline;
      c_data[3][iii] = (float)dsamp;
      }
   for (iii=0;iii<6;iii+=2)
      {
      if (itiecount==6)
         {
         dline = tinv[0]*(double)(itie[iii])+tinv[1]*(double)(itie[iii+1])+tinv[2];
         dsamp = tinv[3]*(double)(itie[iii])+tinv[4]*(double)(itie[iii+1])+tinv[5];
         }
      else
         {
         dline = (double)itie[iii];
         dsamp = (double)itie[iii+1];
         }
      a[0][iii/2] = (float)dline;
      a[1][iii/2] = (float)dsamp;
      }
   
   /* convert otie */

   if (otiecount==6&&!geocord2)
      {
      for (iii=0;iii<6;iii++) { u[iii] = 0.; uinv[iii] = 0.; }
      u[0] = 1.; u[4] = 1.; uinv[0] = 1.; uinv[4] = 1.;
      }
   else
      {
      status = gtgetlab("inp",2,&labelstr,&labnl,&labns);
      if (status!=1) zmabend("Failed to get GeoTIFF label, second input");
      len = strlen(labelstr);
      for (iii=0;iii<len;iii++) labelstr[iii] = toupper(labelstr[iii]);
      status = geofix(labelstr,u,uinv,labnl,labns,corner);
      if (status!=1)
         zmabend("Failed to get mapping from GeoTIFF label, second input");
      }
   if (otiecount==0)
      {
      if (!geocord1)
         {
         status = gtgetlab("inp",1,&labelstr,&labnl,&labns);
         if (status!=1) zmabend("Failed to get GeoTIFF label, first input");
         len = strlen(labelstr);
         for (iii=0;iii<len;iii++) labelstr[iii] = toupper(labelstr[iii]);
         status = geofix(labelstr,t,tinv,labnl,labns,corner);
         if (status!=1)
            zmabend("Failed to get mapping from GeoTIFF label, first input");
         }
      for (iii=0;iii<6;iii+=2)
         {
         dline = t[0]*(double)(itie[iii])+t[1]*(double)(itie[iii+1])+t[2];
         dsamp = t[3]*(double)(itie[iii])+t[4]*(double)(itie[iii+1])+t[5];
         otie[iii] = uinv[0]*dline+uinv[1]*dsamp+uinv[2];
         otie[iii+1] = uinv[3]*dline+uinv[4]*dsamp+uinv[5];
         b[0][iii/2] = (float)otie[iii];
         b[1][iii/2] = (float)otie[iii+1];
         }
      }
   else for (iii=0;iii<6;iii+=2)
      {
      dline = uinv[0]*(double)(otie[iii])+uinv[1]*(double)(otie[iii+1])+uinv[2];
      dsamp = uinv[3]*(double)(otie[iii])+uinv[4]*(double)(otie[iii+1])+uinv[5];
      b[0][iii/2] = (float)dline;
      b[1][iii/2] = (float)dsamp;
      }
   
   /* open the first image file */
   
   status = zvunit(&i_unit[0],"INP",1,NULL);
   status = zvopen(i_unit[0],"OPEN_ACT","SA","IO_ACT","SA",
			"U_FORMAT","HALF",NULL);
   zvget(i_unit[0],"NL",&lnlg[0],"NS",&lnsg[0],"PIX_SIZE",&ityp1,NULL);
   
   /* open the second image file */

   status = zvunit(&i_unit[1],"INP",2,NULL);
   status = zvopen(i_unit[1],"OPEN_ACT","SA","IO_ACT","SA",
			"U_FORMAT","HALF",NULL);
   zvget(i_unit[1],"NL",&lnlg[1],"NS",&lnsg[1],"PIX_SIZE",&ityp2,NULL);
   rctl = 0.5*lnlg[1];
   rcts = 0.5*lnsg[1];
   
   /* outer loop over the tiepoints or gcp's */
   /* gcp option inactive for now... I need a sample gcp data set */

   neq = 3; lastneq = -1; autoix = 0; gotthresh = 0; throwcount = 4; scount = 0;
   srchw = search-fftsize; msrchw = msrc-fftsize; solved = 0; vmaxfac = 0.9;
   maxredo = MIN(nredo,tablen); thr_resp = 4.0*thr_res;
   neqmin = MAX(autofit/2+4,9);
   printf("  seq             point     srch       convergence     corr\n");
   for (ibigx=0;ibigx<(tablen+maxredo);ibigx++)
      {
      ibig = ibigx%tablen;
      c_data[4][ibig] = -9999.;
      c_data[5][ibig] = -9999.;
      c_data[6][ibig] = -9999.;
      c_data[7][ibig] = -9999.;
      c_data[8][ibig] = -9999.;
      c_data[9][ibig] = 0.;
      c_data[10][ibig] = -9999.;
      
      ifftsize = fftsize;  ffthset = 0;
      for (jbig=0;jbig<nretry;jbig++)
	 {
	 if (jbig>0&&ffthalf>0)
	    {
	    if (ibigx<autofit*2) continue;
	    if (ffthset==1) continue;
	    ifftsize /= 2;
	    }
	 ffthset = 0;
	 
	 /*  do linear fit of tiepoints and matches to predict match */
	 /*  inverse linear fit needed for getgrid outputs */
         /*  can't mix 3 initial points with updates */

	 if (((ibigx==0)||(neq>=neqmin))&&(neq!=lastneq))
	    {
	    if (ibigx>0&&throwcount>0)
	       {
	       solved = 1;
	       printf("***auto fit:neq = %8d ***\n",neq);
	       }
	    if (ibigx>0) for (iii=0;iii<4;iii++)
               {
               if (throwcount<=0) break;
               throwout(&throwcount,a,b,&neq,neqmax);
               }
	    if (scount%10==9) throwout(&throwcount,a,b,&neq,neqmax);
            scount++;
            lastneq = neq;
	    for (ix=0;ix<2;ix++)
	       {
	       for (iii=0;iii<neq;iii++)
	          {
	          aa[iii] = a[0][iii];
	          aa[iii+neq] = a[1][iii];
	          aa[iii+2*neq] = 1.;
	          aa[iii+3*neq] = a[0][iii]*a[0][iii];
	          aa[iii+4*neq] = a[1][iii]*a[1][iii];
	          aa[iii+5*neq] = a[0][iii]*a[1][iii];
                  if (fabs(angoff)>0.000001&&ibigx==0&&ix==0)
                     {
                     rv1 = b[0][iii]-rctl;
                     rw1 = b[1][iii]-rcts;
                     rr = sqrt(rv1*rv1+rw1*rw1+0.0000001);
                     theta = atan2(rw1,rv1)-angoff;
                     rv2 = rr*cos(theta);
                     rw2 = rr*sin(theta);
                     b[0][iii] = rv2+rctl;
                     b[1][iii] = rw2+rcts;
                     }
	          bb[iii] = b[ix][iii];
	          }
	       eps = 1.e-7;
	       if (predfunc&&neq>11)
	          lsqfit(aa,bb,neq,6,&soln[ix*6],eps,&ierror);
	       else
	          {
	          lsqfit(aa,bb,neq,3,&soln[ix*6],eps,&ierror);
	          for (jjj=3;jjj<6;jjj++) soln[ix*6+jjj] = 0.0;
	          }
	       }
	    }
	 if (ibigx==0) neq = 0;
         
         choplimit1 = (int)(ifftsize*ifftsize*zerolim);
	 choplimit2 = (int)(ifftsize*ifftsize*zerolim2);
	 bigchoplimit2 = search*search/2;   /* see zerolimit in rfit */
         xlt = c_data[2][ibig]+rretry*rvecl[jbig];
	 xst = c_data[3][ibig]+rretry*rvecs[jbig];
	 pcntr[0] = (int)(xlt+0.5);
	 pcntr[1] = (int)(xst+0.5);
	 centr[0] = xlt-pcntr[0]+ifftsize/2.0+.5;
	 centr[1] = xst-pcntr[1]+ifftsize/2.0+.5;
	 getgrid(1,chip1,ifftsize,ifftsize,ident,pcntr[0],pcntr[1],rmag,&chop,
               0.0,0.0,zerothr);
	 if (chop>choplimit1)
	    {
	    if (jbig==nretry-1) printf("point outside first image\n");
	    continue;
	    }

   /* get the second grid and do correlation, 2d grid can be larger */

         ndim = MIN(search,srchw+ifftsize+2);
	 if (ndim%2==1) ndim++;
	 getgrid(2,asrch,search,ndim,soln,pcntr[0],pcntr[1],rmag,&chop,
               c_data[11][ibig],c_data[12][ibig],zerothr);
	 if ((chop>bigchoplimit2)||((srchw==0)&&(chop>choplimit2)))
	    {
	    if (jbig==nretry-1) printf("point outside second image\n");
	    continue;
	    }
	 ffthset = 1;
         printf("%5d (%7.1f %7.1f)%5dx%3.1f",
            ibig+1,pcntr[0],pcntr[1],srchw+ifftsize,rmag[1]);
         
         if (srchw>0&&ifftsize<fftsize)
	    {
	    printf("#\n");
	    continue;
	    }
	 
	 if (ifftsize==fftsize) srchdim = fftsize+srchw; else srchdim = ifftsize;
	 ilin = (search-srchdim)/2;
	 jsmp = ilin;
	 
	 rfit(ilin,jsmp,&vmax,&vloff,&vsoff,corr,nohpf,srchdim,ifftsize,search);
         
         if (vmax<0.00001) { printf("refine err 1a\n"); continue; }
	 if (vmax<tretry) { printf("  low correlation\n"); continue; }
         
         /* save for future development
         if (srchw>0)
            {
            ilin += (int)(vloff+0.5);
            jsmp += (int)(vsoff+0.5);
            srchdim = ifftsize;
            rfit(ilin,jsmp,&vmax,&vloff,&vsoff,corr,nohpf,ifftsize,ifftsize,search);
            if (vmax<0.00001) { printf("refine err 1b\n"); continue; }
	    if (vmax<tretry) { printf("  low correlation\n"); continue; }
            }*/
         
	 /*if (jbig==0||ffthalf==0)
	    {
	    if (vmax<(vmaxfac*rmcor)&&(ibigx>(autofit*2)))
	       {
	       vmaxfac = MAX(0.50,vmaxfac*0.99825); 
	       printf("*\n");
	       continue;
	       }
	    }
	 else
	    {
	    stricter for half size fft, and no adaptation
	    if (vmax<((vmaxfac+0.5)*rmcor)&&(ibigx>(autofit*2)))
	       {
	       printf("*\n");
	       continue;
	       }
	    }*/
	 
	 if (jbig==0&&vmax<(vmaxfac*rmcor)&&(ibigx>(autofit*2)))
	    {
	    vmaxfac = MAX(0.50,vmaxfac*0.99825); 
	    printf("*\n");
	    continue;
	    }
	 
         refinerr = 0;
	 if (subpix) refine(corr,&vloff,&vsoff,&refinerr);
	 vloff *= rmag[0];
         vsoff *= rmag[1];
         /*printf("\nrefine:ilin,jsmp,vmax,vloff,vsoff %d %d %f %f %f\n",
                            ilin,jsmp,vmax,vloff,vsoff);*/
	 if (refinerr!=0) { printf("refine err 2\n"); continue; }
	 /*ocentr[0] = ((float)ilin-(search-ocentr[0])*0.5)*rmag[0]+vloff;*/
	 /*ocentr[1] = ((float)jsmp-(search-ifftsize)*0.5)*rmag[1]+vsoff;*/
	 /*printf("search,srchdim %d %d\n",search,srchdim);*/
         ocentr[0] = ((float)ilin-(search-srchdim)*0.5)*rmag[0]+vloff;
	 ocentr[1] = ((float)jsmp-(search-srchdim)*0.5)*rmag[1]+vsoff;
         wl = xlt+ocentr[0];
	 ws = xst+ocentr[1];
	 
	 /*printf("\nvloff,vsoff,ocentr[0],ocentr[1] %f %f %f %f\n",
	         vloff,vsoff,ocentr[0],ocentr[1]);
         printf("wl,ws,xlt,xst %f %f %f %f\n",wl,ws,xlt,xst);
         printf("ocentr[0],ocentr[1] %f %f\n",ocentr[0],ocentr[1]);
	 printf("pcntr[0],pcntr[1] %f %f\n",pcntr[0],pcntr[1]);*/

	 c_data[5][ibig] = soln[0]*wl+soln[1]*ws+soln[2]+soln[3]*wl*wl+
	    soln[4]*ws*ws+soln[5]*wl*ws;
	 c_data[6][ibig] = soln[6]*wl+soln[7]*ws+soln[8]+soln[9]*wl*wl+
	    soln[10]*ws*ws+soln[11]*wl*ws;
	 c_data[8][ibig] = vmax;
         
         wl += c_data[11][ibig];
	 ws += c_data[12][ibig];
	 c_data[13][ibig] = soln[0]*wl+soln[1]*ws+soln[2]+soln[3]*wl*wl+
	    soln[4]*ws*ws+soln[5]*wl*ws;
	 c_data[14][ibig] = soln[6]*wl+soln[7]*ws+soln[8]+soln[9]*wl*wl+
	    soln[10]*ws*ws+soln[11]*wl*ws;
	
         c_data[4][ibig] = getzvl(chip1,ifftsize,centr,getw,getr);
	 if (c_data[4][ibig]<-998.0) { printf("z val err\n"); continue; }
	 acentr[0] = search/2.0+0.5+vloff;
	 acentr[1] = search/2.0+0.5+vsoff;
	 /*acentr[0] = search/2.0+0.5;
	 acentr[1] = search/2.0+0.5;*/
	 c_data[7][ibig] = getzvl(asrch,search,acentr,getw,getr);
         if (c_data[7][ibig]<-998.0) { printf("z val err\n"); continue; }
	 
	 resl = fabs(ocentr[0]);
	 ress = fabs(ocentr[1]);
	 res = sqrt((double)(resl*resl+ress*ress));
	 if (res>thr_resp&&solved) 
            {
            printf(" (%7.1f,%7.1f)  %7.3f*\n",ocentr[0],ocentr[1],vmax);
            continue;
            }
         if (solved) thr_resp = MAX(thr_resp*0.95,thr_res);
	 c_data[9][ibig] = 1.;

	 if (jbig>0&&ffthalf==0) /* this is a resetting for the retry case */
	    {
	    c_data[2][ibig] = xlt;
	    c_data[3][ibig] = xst;
	    dline = t[0]*(double)xlt+t[1]*(double)xst+t[2];
	    dsamp = t[3]*(double)xlt+t[4]*(double)xst+t[5];
	    c_data[0][ibig] = (float)dline;
	    c_data[1][ibig] = (float)dsamp;
	    }

	 /*if (picout)  not implemented yet
	    {
	    }*/

	 printf(" (%7.1f,%7.1f)  %7.3f\n",ocentr[0],ocentr[1],vmax);
	 if (autoix<autofit)
	    {
	    vmaxvec[autoix] = vmax;
	    vmaxix[autoix++] = ibig+1;
            /*printf("save autofit,vmax,ibig,autoix,neq %d %f %d %d %d\n",
                         autofit,vmax,ibig,autoix-1,neq);*/
	    }
	 if (!gotthresh&&(autoix>=autofit)&&autofit>0)
	    {
	    sort7(vmaxvec,vmaxix,autoix);
	    rmcor = vmaxvec[autoix/2];
	    printf("***auto thresh  = %8.4f ***\n",rmcor);
	    gotthresh = 1;
	    /* recover points above newly set threshold */
	    for (ie=0;ie<autoix;ie++)
	       {
	       if (vmaxvec[ie]>rmcor)
	          {
	          ibigt = vmaxix[ie]-1;
	          c_data[9][ibigt] = 2.;
	          a[0][neq] = c_data[2][ibigt];
	          a[1][neq] = c_data[3][ibigt];
	          b[0][neq] = c_data[5][ibigt];
	          b[1][neq++] = c_data[6][ibigt];
	          /*printf("copy ie,ibigt,neq %d %d %d\n",ie,ibigt,neq);*/
	          }
	       }
	    break;
	    }
	 if (autoix<autofit) break;
	 if (vmax<rmcor) break;
	 c_data[9][ibig] = 2.;
	 a[0][neq] = xlt;
	 a[1][neq] = xst;
	 b[0][neq] = c_data[5][ibig];
	 b[1][neq++] = c_data[6][ibig];
	 if (neq<neqmin) break;
	 if (srchw>msrchw) vmaxfac = vmaxfac*1.05;
	 if (rmag[0]>rmagmin[0]) vmaxfac = MAX(0.50,vmaxfac*0.9825);
	 srchw = MIN(MAX(msrchw,(4*srchw+2*(int)res)/5),srchw);
	 if (srchw%2==1) srchw++;
	 if (10*srchw<(11*msrchw+ifftsize)) srchw = msrchw;
	 if (magshrk[0]=='y'&&rmag[0]>rmagmin[0]) rmag[0] = MAX(rmag[0]*0.9,rmagmin[0]);
	 if (magshrk[0]=='y'&&rmag[1]>rmagmin[1]) rmag[1] = MAX(rmag[1]*0.9,rmagmin[1]);
	 break;
	 }
      }

   /* print results of final fit */

   for (iii=0;iii<tablen;iii++)
      {
      if (c_data[9][iii]>0.5)
	 {
	 c_data[9][iii] = c_data[5][iii]-
		 (c_data[2][iii]*soln[0]+c_data[3][iii]*soln[1]+soln[2]+
		 c_data[2][iii]*c_data[2][iii]*soln[3]+
		 c_data[3][iii]*c_data[3][iii]*soln[4]+
		 c_data[2][iii]*c_data[3][iii]*soln[5]);
	 c_data[10][iii] = c_data[6][iii]-
		 (c_data[2][iii]*soln[6]+c_data[3][iii]*soln[7]+soln[8]+
		 c_data[2][iii]*c_data[2][iii]*soln[9]+
		 c_data[3][iii]*c_data[3][iii]*soln[10]+
		 c_data[2][iii]*c_data[3][iii]*soln[11]);
	 }
      else c_data[9][iii] = -9999.;
      }
   for (ix=0;ix<2;ix++)
      {
      for (iii=0;iii<neq;iii++)
	 {
	 aa[iii] = b[0][iii];
	 aa[iii+neq] = b[1][iii];
	 aa[iii+2*neq] = 1.;
	 aa[iii+3*neq] = b[0][iii]*b[0][iii];
	 aa[iii+4*neq] = b[1][iii]*b[1][iii];
	 aa[iii+5*neq] = b[0][iii]*b[1][iii];
	 bb[iii] = a[ix][iii];
	 }
      eps = 1.e-7;
      if (predfunc&&neq>11)
	 lsqfit(aa,bb,neq,6,&solninv[ix*6],eps,&ierror);
      else if (neq>=neqmin)
	 {
	 lsqfit(aa,bb,neq,3,&solninv[ix*6],eps,&ierror);
	 for (jjj=3;jjj<6;jjj++) solninv[ix*6+jjj] = 0.0;
	 }
      }
   printf("\n\nfinal line fit %12.6f x %12.6f y %12.6f %12.6f x2 %12.6f y2%12.6f xy\n",
	     soln[0],soln[1],soln[2],soln[3],soln[4],soln[5]);
   printf("final samp fit %12.6f x %12.6f y %12.6f %12.6f x2 %12.6f y2%12.6f xy\n",
	     soln[6],soln[7],soln[8],soln[9],soln[10],soln[11]);
   if (neq>=neqmin)
      {
      printf("inv line fit %12.6f x %12.6f y %12.6f %12.6f x2 %12.6f y2%12.6f xy\n",
	     solninv[0],solninv[1],solninv[2],solninv[3],solninv[4],solninv[5]);
      printf("inv samp fit %12.6f x %12.6f y %12.6f %12.6f x2 %12.6f y2%12.6f xy\n\n",
	     solninv[6],solninv[7],solninv[8],solninv[9],solninv[10],solninv[11]);
      }
   
   /* correct columns 6,7 for the predictor elevation offset (elvcor) */

   for (jjj=0;jjj<tablen;jjj++) 
      {
      c_data[5][jjj] = c_data[13][jjj];
      c_data[6][jjj] = c_data[14][jjj];
      }
   
   /* output the table columns */
   
   for (iii=0;iii<11;iii++)
      {
      status = IBISColumnSet(ibis,"U_FORMAT","REAL",iii+1);
      if (status!=1) IBISSignal(ibis,status,1);
      status = IBISColumnWrite(ibis,(char*)(c_data[iii]),iii+1,1,tablen);
      if (status!=1) IBISSignal(ibis,status,1);
      }
   
   status = IBISFileClose(ibis,0);
   return;
}
