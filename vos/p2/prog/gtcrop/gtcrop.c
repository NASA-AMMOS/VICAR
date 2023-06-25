#include <math.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>

#include "vicmain_c.h"
#include "applic.h"
#include "taeconf.inp"
#include "parblk.inc"
#include "defines.h"
#include "ibisfile.h"
#include "ibiserrs.h"

#include "cartoStrUtils.h"
#include "cartoMemUtils.h"
#include "cartoLsqUtils.h"
#include "cartoGtUtils.h"
#include "cartoTaeUtils.h"

/************************************************************************/
/* program gtcrop                                                      */
/************************************************************************/
/*  07-00 ...alz... initial version                     */
/************************************************************************/

void main44(void)
{
   int sl,ss,nl,ns;
   double *polybuf,*x,*y;
   short int *inbuf;
   
   int i,j,k,cols[2],clen,unit,ibis,coverpoly=0;
   int colcount,coldef,status,i_unit,o_unit,lnl,lns;
   int np,cross[200],pptr,kl,ku,cr,ul,us;
   int labnl,labns,gtfirst,len;
   double x0,y0,x1,y1,yc,dline,tt;
   double dnl2,dns2,minpl,minps,maxpl,maxps;
   double t[6],tinv[6],tout[6],toutinv[6],corner[4];
   double a,b,c,d,bcor,dcor,voff,scale1,scale2;
   char *p,*labelstr1,fmt_str[10],scalestr[50],transstr[133];
   
   zifmessage("gtcrop version 2015-10-15");
   
   /* fetch params */
   
   zvparm("cols",cols,&colcount,&coldef,2,0);
   if (colcount!=2) zmabend("gtcrop requires two columns");
   
   if (zvptst("COVERINP")) coverpoly = 0;
   else if (zvptst("COVERPOL")) coverpoly = 1;
   
   /* optional geotiff label in first input, save label in labelstr1 */
   /* also, don't need to do GeoTIFF unless coverpoly==1 */
   
   status = gtgetlab("inp",1,&labelstr1,&labnl,&labns);
   gtfirst = status==1&&coverpoly;
   if (gtfirst)
      {
      len = strlen(labelstr1);
      for (i=0;i<len;i++) labelstr1[i] = toupper(labelstr1[i]);
      status = geofix(labelstr1,t,tinv,labnl,labns,corner);
      if (status!=1)
         zmabend("Failed to get mapping from GeoTIFF label, first input");
      }
   
   /* read in data from the ibis interface file */
   
   status = zvunit(&unit,"inp",2, NULL);
   status = IBISFileOpen(unit,&ibis,IMODE_READ,0,0,0,0);
   if (status!=1) IBISSignalU(unit,status,1);
   IBISFileGet(ibis,"nr",&clen,1,1,0);
   
   mz_alloc1((unsigned char **)&x,clen,8);
   mz_alloc1((unsigned char **)&y,clen,8);
   mz_alloc1((unsigned char **)&polybuf,2*clen,8);
  
   status = IBISColumnSet(ibis,"U_FORMAT","DOUB",cols[0]);
   if (status!=1) IBISSignal(ibis,status,1);
   status = IBISColumnRead(ibis,(char*)x,cols[0],1,clen);
   if (status!=1) IBISSignal(ibis,status,1);
   status = IBISColumnSet(ibis,"U_FORMAT","DOUB",cols[1]);
   if (status!=1) IBISSignal(ibis,status,1);
   status = IBISColumnRead(ibis,(char*)y,cols[1],1,clen);
   if (status!=1) IBISSignal(ibis,status,1);
 
   /* open input image file */
   
   status = zvunit( &i_unit, "INP", 1, NULL);
   status = zvopen( i_unit, "OPEN_ACT", "SA", "IO_ACT", "SA",
			"U_FORMAT","HALF", NULL);
   zvget(i_unit,"FORMAT",fmt_str, NULL);
   if (strcmp(fmt_str,"BYTE")&&strcmp(fmt_str,"HALF")) 
      zmabend("Invalid input data format.  Use BYTE or HALF.");
      
   zvget(i_unit,"NL",&lnl, NULL);
   zvget(i_unit,"NS",&lns, NULL);
   sl = 1;
   ss = 1;
   nl = lnl;
   ns = lns;
   
   /* the line coordinates are varied to general position, and
   are also enlarged a tiny bit relative to image center so that
   poly lines that are dead on the center of a pixel will be
   included */
   
   np = clen*2;
   for (i=0;i<clen;i++)
      {
      polybuf[2*i] = x[i]-sl+1;
      polybuf[2*i+1] = y[i]-ss+1;
      }
   free(x);
   free(y);
   dnl2 = (double)(nl+1)*0.5;
   dns2 = (double)(ns+1)*0.5;
   minpl = 9999999.0; minps = 9999999.0;
   maxpl = -9999999.0; maxps = -9999999.0;
   for (i=0;i<np;i+=2)
      {
      polybuf[i] = (polybuf[i]-dnl2)*1.00000001+dnl2;
      polybuf[i+1] = (polybuf[i+1]-dns2)*1.00000001+dns2;
      if (!coverpoly) continue;
      minpl = MIN(minpl,polybuf[i]);
      minps = MIN(minps,polybuf[i+1]);
      maxpl = MAX(maxpl,polybuf[i]);
      maxps = MAX(maxps,polybuf[i+1]);
      }
   if (coverpoly)
      {
      sl = (int)minpl+1;
      ss = (int)minps+1;
      ul = (int)maxpl;
      us = (int)maxps;
      nl = ul-sl+1;
      ns = us-ss+1;
      }
   
   /* open output image file */
   
   status=zvunit(&o_unit,"OUT",1, NULL);
   status=zvopen(o_unit,"U_NL",nl,"U_NS",ns,
	"OP","WRITE","OPEN_ACT","SA","IO_ACT","SA", NULL);
   
   mz_alloc1((unsigned char **)&inbuf,ns*2,1);

   /* copy the selected part of the input to the output */
   
   for (i=0;i<nl;i++)
      {
      dline = (double)(sl+i); pptr = 2;
      cross[0] = -1; cross[1] = ns+2;
      for (j=0;j<np-2;j+=2)
	 {
	 x0 = polybuf[j];
	 x1 = polybuf[j+2];
	 if ((dline-x0)*(dline-x1)>0.) continue;
	 y0 = polybuf[j+1];
	 y1 = polybuf[j+3];
	 tt = (dline-x0)/(x1-x0);
	 yc = (y1-y0)*tt+y0;
	 cr = (int)yc-sl+1;
	 for (k=pptr;k>=0;k--)
	    {
	    if (k==0) { cross[k] = cr; break; }
	    if (cross[k-1]<=cr) { cross[k] = cr; break; }
	    cross[k] = cross[k-1];
	    }
	 pptr++;
	 }
      status = zvread(i_unit,inbuf,"LINE",sl+i,"SAMP",ss,"NSAMPS",ns, NULL);
      for (j=0;j<pptr-1;j+=2)
	 {
	 kl = MAX(cross[j],0);
	 ku = MIN(cross[j+1],ns);
	 for (k=kl;k<ku;k++) inbuf[k] = 0;
	 }
      zvwrit(o_unit,inbuf,"LINE",i+1,"SAMP",1,"NSAMPS",ns, NULL);
      }
   
   /* handling the geotiff label for the shift case */
   
   zvclose(o_unit, NULL);
   if (gtfirst)
      {
      a = 1.0; c = 1.0;
      b = 0.5+((double)sl-0.5)*a;
      d = 0.5+((double)ss-0.5)*c;
      bcor = t[0]*b+t[1]*d+t[2];
      dcor = t[3]*b+t[4]*d+t[5];
      p = ms_find(labelstr1,"GTRASTERTYPEGEOKEY=2");
      if (p!=0) voff = 1.0; else voff = 0.5;
   
      toutinv[0] = tinv[3]/c;
      toutinv[1] = tinv[4]/c;
      toutinv[2] = 1.0-voff-toutinv[0]*bcor-toutinv[1]*dcor;
      toutinv[3] = tinv[0]/a;
      toutinv[4] = tinv[1]/a;
      toutinv[5] = 1.0-voff-toutinv[3]*bcor-toutinv[4]*dcor;
      
      scale2 = -t[3]*a;
      scale1 = t[1]*c;
      
      invertmap(toutinv,tout);
      scalefmt(scalestr,scale1,scale2);
      trnsfmt(transstr,tout);
      gtreplab("OUT",1,labelstr1,0,1,toutinv,scalestr,transstr);
      }
   
   /* output the (sline,ssamp), close and return */
   
   mq_out_int("vsline",sl);
   mq_out_int("vssamp",ss);
   
   status = IBISFileClose(ibis,0);
   if (status!=1) IBISSignal(ibis,status,1);
   zvclose(i_unit, NULL);
   
   return;
}
