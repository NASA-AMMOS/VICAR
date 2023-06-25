#include <math.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>

#include "vicmain_c.h"
#include "applic.h"
#include "taeconf.inp"
#include "parblk.inc"
#include "defines.h"
#include "zifmessage.h"
#include "zmabend.h"

#include "cartoStrUtils.h"
#include "cartoMatUtils.h"
#include "cartoMemUtils.h"
#include "cartoLsqUtils.h"
#include "cartoGtUtils.h"
#include "cartoSortUtils.h"
#include "cartoTaeUtils.h"

/************************************************************************/
/* program imcorner                                                      */
/************************************************************************/
/*  04-00 ...alz... initial version                     */
/************************************************************************/

int insidecnvxqd(x,y,x1,y1,x2,y2,x3,y3,x4,y4)
   double x,y,x1,y1,x2,y2,x3,y3,x4,y4;
{
   if ((x-x1)*(y2-y1)-(y-y1)*(x2-x1)<0.) return(0);
   if ((x-x2)*(y3-y2)-(y-y2)*(x3-x2)<0.) return(0);
   if ((x-x3)*(y4-y3)-(y-y3)*(x4-x3)<0.) return(0);
   if ((x-x4)*(y1-y4)-(y-y4)*(x1-x4)<0.) return(0);
   return(1);
}

void main44(void)
{
   int i,j,len,status,pcount,pdef,inpcount,corrfoot;
   int nline1=0,nsamp1=0,nline2=0,nsamp2=0,thresh,labnl,labns;
   int i_unit1,i_unit2,iline,jsamp,inside,ibig,ibigskp,ibigskp0=0;
   int lineul0=0,lineur0=0,linell0=0,linelr0=0,bound2_1=0,bl1=0,bl2=0,bl3=0;
   int cptr,*candl=NULL,*cands=NULL,sfirst,slast,ptl[4],pts[4],ptix[4];
   double mintop,minleft,minbot,minrt,linetop=0;
   double lineleft,sampleft=0,linebot=0,linert,samprt=0;
   double linetop0=0,lineleft0=0,linebot0=0,linert0=0;
   double ptang[4],maxdist,d1,d2,d3,d4,maxsq,dp1,dp2,dxx;
   double cgl,cgs,fac;
   short int *inbuf=NULL;
   char fmt_str[10];
   char *labelstr=NULL;
   double t1[6],t1inv[6],t2[6],t2inv[6],corner[4];
   double dist,l2,s2;
   double lineul2=0,sampul2=0,lineul=0,sampul=0;
   double lineur2=0,sampur2=0,lineur=0,sampur=0;
   double linell2=0,sampll2=0,linell=0,sampll=0;
   double linelr2=0,samplr2=0,linelr=0,samplr=0;
   double triarea();
   
   zifmessage("IMCORNER version 2021-05-19");
   
   /* get the basic parameters */
   
   zvparm("thresh",&thresh,&pcount,&pdef,1,0);
   zvparm("inside",&inside,&pcount,&pdef,1,0);
   corrfoot = zvptst("corrfoot");
   
   /* get the mappings if there are two inputs */
   
   status = zvpcnt("inp",&inpcount);
   if (inpcount>1)
   {
   status = gtgetlab("inp",1,&labelstr,&labnl,&labns);
   /* geofix expects uppercase GeoTIFF names */
   len = strlen(labelstr);
   for (i=0;i<len;i++) labelstr[i] = toupper(labelstr[i]);
   status = geofix(labelstr,t1,t1inv,labnl,labns,corner);
   if (status!=1) zmabend("Failed to get mapping from primary GeoTIFF label");
   
   status = gtgetlab("inp",2,&labelstr,&labnl,&labns);
   len = strlen(labelstr);
   for (i=0;i<len;i++) labelstr[i] = toupper(labelstr[i]);
   status = geofix(labelstr,t2,t2inv,labnl,labns,corner);
   if (status!=1) zmabend("Failed to get mapping from secondary GeoTIFF label");
    
   /* do corners to a quadrilateral */
 
   status = zvunit( &i_unit2, "INP", 2, NULL);
   status = zvopen( i_unit2, "OPEN_ACT", "SA", "IO_ACT", "SA",
      "U_FORMAT","HALF", NULL);
   zvget(i_unit2,"FORMAT",fmt_str, NULL);
   zvget(i_unit2,"NL",&nline2, NULL);
   zvget(i_unit2,"NS",&nsamp2, NULL);
   if ( strcmp(fmt_str,"BYTE") && strcmp(fmt_str,"HALF"))
      zmabend("Invalid input data format.  Use BYTE or HALF.");
   mz_alloc1((unsigned char **)&inbuf,nsamp2,2);
   mz_alloc1((unsigned char **)&candl,2*nline2,4);
   mz_alloc1((unsigned char **)&cands,2*nline2,4);
   
   mintop = nline1+nsamp1;
   minleft = mintop;
   minbot = mintop;
   minrt = mintop;
   for (ibig=0;ibig<2;ibig++)
   {
   cptr = 0;
   if (ibig==0)
      {
      ibigskp = (int)(sqrt((double)nline2));
      ibigskp0 = ibigskp;
      }
      else ibigskp = 1;
   for (iline=0;iline<nline2;iline+=ibigskp)
      {
      if (ibig==1&&(!(  
           (((lineul0-iline-1)>(-ibigskp0))&&((lineul0-iline-1)<ibigskp0))||   
           (((lineur0-iline-1)>(-ibigskp0))&&((lineur0-iline-1)<ibigskp0))||   
           
           (((linetop0-iline-1)>(-ibigskp0))&&((linetop0-iline-1)<ibigskp0))||   
           (((lineleft0-iline-1)>(-ibigskp0))&&((lineleft0-iline-1)<ibigskp0))||   
           (((linebot0-iline-1)>(-ibigskp0))&&((linebot0-iline-1)<ibigskp0))||   
           (((linert0-iline-1)>(-ibigskp0))&&((linert0-iline-1)<ibigskp0))||   
           
           (((iline-linell0+1)>(-ibigskp0))&&((iline-linell0+1)<ibigskp0))||   
           (((iline-linelr0+1)>(-ibigskp0))&&((iline-linelr0+1)<ibigskp0))   
           ))) continue;
      status = zvread(i_unit2,inbuf,"LINE",iline+1, NULL);
      sfirst = nsamp2; slast = 0;
      for (jsamp=0;jsamp<nsamp2;jsamp++)
         {
         if (inbuf[jsamp]>thresh)
            {
            if (jsamp<sfirst) sfirst = jsamp;
            slast = jsamp;
            dist = (double)iline;
            if (dist<mintop)
               {
               mintop = dist;
               linetop = iline+1;
               if (ibig==0) linetop0 = linetop;
               /* samptop = jsamp+1; */
               }
            dist = (double)jsamp;
            if (dist<minleft)
               {
               minleft = dist;
               lineleft = iline+1;
               if (ibig==0) lineleft0 = lineleft;
               sampleft = jsamp+1;
               }
            dist = (double)(nline2-iline-1);
            if (dist<minbot)
               {
               minbot = dist;
               linebot = iline+1;
               if (ibig==0) linebot0 = linebot;
               /* sampbot = jsamp+1; */
               }
            dist = (double)(nsamp2-jsamp-1);
            if (dist<minrt)
               {
               minrt = dist;
               linert = iline+1;
               if (ibig==0) linert0 = linert;
               samprt = jsamp+1;
               }
            }
         } /* end of jsamp loop over line */
         if (slast>=sfirst)
            {
            candl[cptr] = iline+1;
            cands[cptr++] = sfirst+1;
            candl[cptr] = iline+1;
            cands[cptr++] = slast+1;
            }
      } /* end of iline loop */
    
   /* get the four points */
      
      switch(corrfoot)
         {
         case 0:
         maxdist = -1.0;  /* farthest from center */
         for (i=0;i<cptr;i++)
            {
            d1 = (double)(candl[i]-nline1/2);
            d2 = (double)(cands[i]-nsamp1/2);
            dist = d1*d1+d2*d2;
            if (dist>maxdist)
               {
               maxdist = dist;
               ptl[0] = candl[i];
               pts[0] = cands[i];
               }
            }
         
      maxdist = -1.0;  /* farthest from first point */
      for (i=0;i<cptr;i++)
         {
         d1 = (double)(candl[i]-ptl[0]);
         d2 = (double)(cands[i]-pts[0]);
         dist = d1*d1+d2*d2;
         if (dist>maxdist)
            {
            maxdist = dist;
            ptl[1] = candl[i];
            pts[1] = cands[i];
            }
         }
         
      maxdist = -1.0;  /* longest two legs */
      for (i=0;i<cptr;i++)
         {
         d1 = (double)(candl[i]-ptl[0]);
         d2 = (double)(cands[i]-pts[0]);
         dist = sqrt(d1*d1+d2*d2);
         d1 = (double)(candl[i]-ptl[1]);
         d2 = (double)(cands[i]-pts[1]);
         dist += sqrt(d1*d1+d2*d2);
         if (dist>maxdist)                                     
            {
            maxdist = dist;
            ptl[2] = candl[i];
            pts[2] = cands[i];
            }
         }
      
         maxdist = -1.0;  /* longest three legs */
         for (i=0;i<cptr;i++)
            {
            d1 = (double)(candl[i]-ptl[0]);
            d2 = (double)(cands[i]-pts[0]);
            dist = sqrt(d1*d1+d2*d2);
            d1 = (double)(candl[i]-ptl[1]);
            d2 = (double)(cands[i]-pts[1]);
            dist += sqrt(d1*d1+d2*d2);
            d1 = (double)(candl[i]-ptl[2]);
            d2 = (double)(cands[i]-pts[2]);
            dist += sqrt(d1*d1+d2*d2);
            if (dist>maxdist)                                     
               {
               maxdist = dist;
               ptl[3] = candl[i];
               pts[3] = cands[i];
               }
            }
         break;
         case 1:   /*finding max correlation footprint */
         maxsq = -1.0;
         for (i=0;i<cptr;i+=2)
            {
            printf("loop maxsq %f\n",maxsq);
            d1 = cands[i+1]-cands[i];
            if (d1<maxsq) continue;
            for (j=0;j<i;j+=2)
               {
               d2 = cands[j+1]-cands[j];
               if (d2<maxsq) continue;
               d3 = candl[i]-candl[j];
               if (d3<maxsq) continue;
               dp1 = max(cands[j],cands[i]);
               dp2 = min(cands[j+1],cands[i+1]);
               d4 = dp2-dp1;
               if (d4<maxsq) continue;
               printf("reset maxsq %f\n",maxsq);
               maxsq = min(d1,min(d2,min(d3,d4)));
               dxx = (d4-maxsq)*0.5;
               ptl[0] = candl[j];
               pts[0] = dp1+dxx;
               ptl[1] = ptl[0];
               pts[1] = dp2-dxx;
               ptl[2] = candl[i];
               pts[2] = pts[1];
               ptl[3] = ptl[2];
               pts[3] = pts[0];
               }
            }
         break;
         }
      lineul0 = ptl[0];   
      lineur0 = ptl[1];   
      linell0 = ptl[2];   
      linelr0 = ptl[3];   
   }
   zvclose(i_unit2, NULL);
   free(inbuf);

   /* sort the points in clockwise order */
   
   cgl = (double)(ptl[0]+ptl[1]+ptl[2]+ptl[3])*0.25;
   cgs = (double)(pts[0]+pts[1]+pts[2]+pts[3])*0.25;
   for (i=0;i<4;i++)
      {
      ptang[i] = 6.2832-atan2(pts[i]-cgs,ptl[i]-cgl);
      if (ptang[i]>7.8540) ptang[i] -= 7.8540;
      ptix[i] = i+1;
      }
   sort8(ptang,ptix,4);
   sortrec4(ptl,ptix,4);
   sortrec4(pts,ptix,4);
   
   lineul2 = ptl[0];
   sampul2 = pts[0];
   lineur2 = ptl[1];
   sampur2 = pts[1];
   linelr2 = ptl[2];
   samplr2 = pts[2];
   linell2 = ptl[3];
   sampll2 = pts[3];
   
   /*printf("Bminul2,lineul2,sampul2 %f %f %f\n",minul2,lineul2,sampul2);
   printf("Bminur2,lineur2,sampur2 %f %f %f\n",minur2,lineur2,sampur2);
   printf("Bminll2,linell2,sampll2 %f %f %f\n",minll2,linell2,sampll2);
   printf("Bminlr2,linelr2,samplr2 %f %f %f\n",minlr2,linelr2,samplr2);*/

   /* map the second image points to the first image */

   l2 = t2[0]*lineul2+t2[1]*sampul2+t2[2];
   s2 = t2[3]*lineul2+t2[4]*sampul2+t2[5];
   lineul2 = t1inv[0]*l2+t1inv[1]*s2+t1inv[2];
   sampul2 = t1inv[3]*l2+t1inv[4]*s2+t1inv[5];
  
   l2 = t2[0]*lineur2+t2[1]*sampur2+t2[2];
   s2 = t2[3]*lineur2+t2[4]*sampur2+t2[5];
   lineur2 = t1inv[0]*l2+t1inv[1]*s2+t1inv[2];
   sampur2 = t1inv[3]*l2+t1inv[4]*s2+t1inv[5];

   l2 = t2[0]*linell2+t2[1]*sampll2+t2[2];
   s2 = t2[3]*linell2+t2[4]*sampll2+t2[5];
   linell2 = t1inv[0]*l2+t1inv[1]*s2+t1inv[2];
   sampll2 = t1inv[3]*l2+t1inv[4]*s2+t1inv[5];

   l2 = t2[0]*linelr2+t2[1]*samplr2+t2[2];
   s2 = t2[3]*linelr2+t2[4]*samplr2+t2[5];
   linelr2 = t1inv[0]*l2+t1inv[1]*s2+t1inv[2];
   samplr2 = t1inv[3]*l2+t1inv[4]*s2+t1inv[5];

   /*printf("Cminul2,lineul2,sampul2 %f %f %f\n",minul2,lineul2,sampul2);
   printf("Cminur2,lineur2,sampur2 %f %f %f\n",minur2,lineur2,sampur2);
   printf("Cminll2,linell2,sampll2 %f %f %f\n",minll2,linell2,sampll2);
   printf("Cminlr2,linelr2,samplr2 %f %f %f\n",minlr2,linelr2,samplr2);*/

   /* enlarge the quadrilateral 0.5 to catch integral values */

   cgl = (lineul2+lineur2+linelr2+linell2)*0.25;
   cgs = (sampul2+sampur2+samplr2+sampll2)*0.25;

   d1 = (double)lineul2-cgl;
   d2 = (double)sampul2-cgs;
   dist = sqrt(d1*d1+d2*d2);
   lineul2 = lineul2+0.5*(lineul2-cgl)/dist;
   sampul2 = sampul2+0.5*(sampul2-cgs)/dist;

   d1 = (double)lineur2-cgl;
   d2 = (double)sampur2-cgs;
   dist = sqrt(d1*d1+d2*d2);
   lineur2 = lineur2+0.5*(lineur2-cgl)/dist;
   sampur2 = sampur2+0.5*(sampur2-cgs)/dist;

   d1 = (double)linelr2-cgl;
   d2 = (double)samplr2-cgs;
   dist = sqrt(d1*d1+d2*d2);
   linelr2 = linelr2+0.5*(linelr2-cgl)/dist;
   samplr2 = samplr2+0.5*(samplr2-cgs)/dist;

   d1 = (double)linell2-cgl;
   d2 = (double)sampll2-cgs;
   dist = sqrt(d1*d1+d2*d2);
   linell2 = linell2+0.5*(linell2-cgl)/dist;
   sampll2 = sampll2+0.5*(sampll2-cgs)/dist;
   
   free(candl);
   free(cands);
   
   } /* end of if (inpcnt>1) */

   /* read input 1 for corners */
      
   status = zvunit( &i_unit1, "INP", 1, NULL);
   status = zvopen( i_unit1, "OPEN_ACT", "SA", "IO_ACT", "SA",
			"U_FORMAT","HALF", NULL);
   zvget(i_unit1,"FORMAT",fmt_str, NULL);
   zvget(i_unit1,"NL",&nline1, NULL);
   zvget(i_unit1,"NS",&nsamp1, NULL);
   if ( strcmp(fmt_str,"BYTE") && strcmp(fmt_str,"HALF"))
      zmabend("Invalid input data format.  Use BYTE or HALF.");
   mz_alloc1((unsigned char **)&inbuf,nsamp1,2);
   mz_alloc1((unsigned char **)&candl,2*nline1,4);
   mz_alloc1((unsigned char **)&cands,2*nline1,4);
   
   mintop = nline1+nsamp1;
   minleft = mintop;
   minbot = mintop;
   minrt = mintop;
   bound2_1 = 0;
   for (ibig=0;ibig<2;ibig++)
   {
   cptr = 0;
   if (ibig==0)
      {
      ibigskp = (int)(sqrt((double)nline1));
      ibigskp0 = ibigskp;
      }
      else ibigskp = 1;
   for (iline=0;iline<nline1;iline+=ibigskp)
      {
      if (ibig==1&&(!(  
           (((lineul0-iline-1)>(-ibigskp0))&&((lineul0-iline-1)<ibigskp0))||   
           (((lineur0-iline-1)>(-ibigskp0))&&((lineur0-iline-1)<ibigskp0))||   
           
           (((linetop0-iline-1)>(-ibigskp0))&&((linetop0-iline-1)<ibigskp0))||   
           (((lineleft0-iline-1)>(-ibigskp0))&&((lineleft0-iline-1)<ibigskp0))||   
           (((linebot0-iline-1)>(-ibigskp0))&&((linebot0-iline-1)<ibigskp0))||   
           (((linert0-iline-1)>(-ibigskp0))&&((linert0-iline-1)<ibigskp0))||   
           
           (((iline-linell0+1)>(-ibigskp0))&&((iline-linell0+1)<ibigskp0))||   
           (((iline-linelr0+1)>(-ibigskp0))&&((iline-linelr0+1)<ibigskp0))   
           ))) continue;
      status = zvread(i_unit1,inbuf,"LINE",iline+1, NULL);
      sfirst = nsamp1; slast = 0;
      for (jsamp=0;jsamp<nsamp1;jsamp++)
         {
         bl1 = inbuf[jsamp]>thresh;
         bl2 = inpcount==1;
         if (!bl2) bl3 = insidecnvxqd((double)iline,(double)jsamp,lineul2,
           sampul2,lineur2,sampur2,linelr2,samplr2,linell2,sampll2);
         else bl3 = 1;
         if (bl1&&!bl3) bound2_1 = 1;
         if (bl1&&(bl2||bl3))
            {
            if (jsamp<sfirst) sfirst = jsamp;
            slast = jsamp;
            dist = (double)iline;
            if (dist<mintop)
               {
               mintop = dist;
               linetop = iline+1;
               if (ibig==0) linetop0 = linetop;
               /* samptop = jsamp+1; */
               }
            dist = (double)jsamp;
            if (dist<minleft)
               {
               minleft = dist;
               lineleft = iline+1;
               if (ibig==0) lineleft0 = lineleft;
               sampleft = jsamp+1;
               }
            dist = (double)(nline1-iline-1);
            if (dist<minbot)
               {
               minbot = dist;
               linebot = iline+1;
               if (ibig==0) linebot0 = linebot;
               /* sampbot = jsamp+1; */
               }
            dist = (double)(nsamp1-jsamp-1);
            if (dist<minrt)
               {
               minrt = dist;
               linert = iline+1;
               if (ibig==0) linert0 = linert;
               samprt = jsamp+1;
               }
            }
         } /* end of jsamp loop over line */
         if (slast>=sfirst)
            {
            candl[cptr] = iline+1;
            cands[cptr++] = sfirst+1;
            candl[cptr] = iline+1;
            cands[cptr++] = slast+1;
            }
      } /* end of iline loop */
      
      /* get the four points */
      
      switch(corrfoot)
         {
         case 0:
         maxdist = -1.0;  /* farthest from center */
         for (i=0;i<cptr;i++)
            {
            d1 = (double)(candl[i]-nline2/2);
            d2 = (double)(cands[i]-nsamp2/2);
            dist = d1*d1+d2*d2;
            if (dist>maxdist)
               {
               maxdist = dist;
               ptl[0] = candl[i];
               pts[0] = cands[i];
               }
            }
         
      maxdist = -1.0;  /* farthest from first point */
      for (i=0;i<cptr;i++)
         {
         d1 = (double)(candl[i]-ptl[0]);
         d2 = (double)(cands[i]-pts[0]);
         dist = d1*d1+d2*d2;
         if (dist>maxdist)
            {
            maxdist = dist;
            ptl[1] = candl[i];
            pts[1] = cands[i];
            }
         }
         
      maxdist = -1.0;  /* longest two legs */
      for (i=0;i<cptr;i++)
         {
         d1 = (double)(candl[i]-ptl[0]);
         d2 = (double)(cands[i]-pts[0]);
         dist = sqrt(d1*d1+d2*d2);
         d1 = (double)(candl[i]-ptl[1]);
         d2 = (double)(cands[i]-pts[1]);
         dist += sqrt(d1*d1+d2*d2);
         if (dist>maxdist)                                     
            {
            maxdist = dist;
            ptl[2] = candl[i];
            pts[2] = cands[i];
            }
         }
      
         maxdist = -1.0;  /* longest three legs */
         for (i=0;i<cptr;i++)
            {
            d1 = (double)(candl[i]-ptl[0]);
            d2 = (double)(cands[i]-pts[0]);
            dist = sqrt(d1*d1+d2*d2);
            d1 = (double)(candl[i]-ptl[1]);
            d2 = (double)(cands[i]-pts[1]);
            dist += sqrt(d1*d1+d2*d2);
            d1 = (double)(candl[i]-ptl[2]);
            d2 = (double)(cands[i]-pts[2]);
            dist += sqrt(d1*d1+d2*d2);
            if (dist>maxdist)                                     
               {
               maxdist = dist;
               ptl[3] = candl[i];
               pts[3] = cands[i];
               }
            }
         break;
         case 1:   /*finding max correlation footprint */
         maxsq = 0.0;
         for (i=0;i<cptr;i+=2)
            {
            d1 = cands[i+1]-cands[i];
            if (d1<maxsq) continue;
            for (j=0;j<i;j+=2)
               {
               d2 = cands[j+1]-cands[j];
               if (d2<maxsq) continue;
               d3 = candl[i]-candl[j];
               if (d3<maxsq) continue;
               dp1 = max(cands[j],cands[i]);
               dp2 = min(cands[j+1],cands[i+1]);
               d4 = dp2-dp1;
               if (d4<maxsq) continue;
               maxsq = min(d1,min(d2,min(d3,d4)));
               dxx = (d4-maxsq)*0.5;
               ptl[0] = candl[j];
               pts[0] = dp1+dxx;
               ptl[1] = ptl[0];
               pts[1] = dp2-dxx;
               ptl[2] = candl[i];
               pts[2] = pts[1];
               ptl[3] = ptl[2];
               pts[3] = pts[0];
               }
            }
         linetop = ptl[0];
         linebot = ptl[2];
         sampleft = pts[0];
         samprt = pts[1];
         break;
         }
      lineul0 = ptl[0];   
      lineur0 = ptl[1];   
      linell0 = ptl[2];   
      linelr0 = ptl[3];   
   }
   zvclose(i_unit1, NULL);
   free(inbuf);

   /* sort the points in clockwise order */
   
   cgl = (double)(ptl[0]+ptl[1]+ptl[2]+ptl[3])*0.25;
   cgs = (double)(pts[0]+pts[1]+pts[2]+pts[3])*0.25;
   for (i=0;i<4;i++)
      {
      ptang[i] = 6.2832-atan2(pts[i]-cgs,ptl[i]-cgl);
      if (ptang[i]>7.8540) ptang[i] -= 7.8540;
      ptix[i] = i+1;
      }
   sort8(ptang,ptix,4);
   sortrec4(ptl,ptix,4);
   sortrec4(pts,ptix,4);
   
   lineul = ptl[0];
   sampul = pts[0];
   lineur = ptl[1];
   sampur = pts[1];
   linelr = ptl[2];
   samplr = pts[2];
   linell = ptl[3];
   sampll = pts[3];
   
   /* perform the inside parameter */

   if (inside!=0)
      {
      d1 = (double)ptl[0]-cgl;
      d2 = (double)pts[0]-cgs;
      dist = sqrt(d1*d1+d2*d2);
      fac = (double)inside/dist;
      lineul = (int)((1.0-fac)*(double)lineul+fac*cgl+0.5);
      sampul = (int)((1.0-fac)*(double)sampul+fac*cgs+0.5);
      
      d1 = (double)ptl[1]-cgl;
      d2 = (double)pts[1]-cgs;
      dist = sqrt(d1*d1+d2*d2);
      fac = (double)inside/dist;
      lineur = (int)((1.0-fac)*(double)lineur+fac*cgl+0.5);
      sampur = (int)((1.0-fac)*(double)sampur+fac*cgs+0.5);
      
      d1 = (double)ptl[2]-cgl;
      d2 = (double)pts[2]-cgs;
      dist = sqrt(d1*d1+d2*d2);
      fac = (double)inside/dist;
      linelr = (int)((1.0-fac)*(double)linelr+fac*cgl+0.5);
      samplr = (int)((1.0-fac)*(double)samplr+fac*cgs+0.5);
      
      d1 = (double)ptl[3]-cgl;
      d2 = (double)pts[3]-cgs;
      dist = sqrt(d1*d1+d2*d2);
      fac = (double)inside/dist;
      linell = (int)((1.0-fac)*(double)linell+fac*cgl+0.5);
      sampll = (int)((1.0-fac)*(double)sampll+fac*cgs+0.5);
      
      linetop += inside;
      sampleft += inside;
      linebot -= inside;
      samprt -= inside;
      } 
   
   /* output the points to the TAE TCL variables of type real */
   
   /* taken out, new algorithm very good, may revive on some
   future bad case
   boxarea = (linebot-linetop)*(samprt-sampleft);
   gridh = MAX(0.0,MIN((linell-lineul),(linelr-lineur)));
   gridw = MAX(0.0,MIN((sampur-sampul),(samplr-sampll)));
   gridarea = gridh*gridw;
   if (gridarea<0.05*boxarea)
      {
      lineul = linetop; sampul = sampleft;
      lineur = linetop; sampur = samprt;
      linell = linebot; sampll = sampleft;
      linelr = linebot; samplr = samprt;
      }*/
   mq_out_real("line1",lineul);
   mq_out_real("samp1",sampul);
   mq_out_real("line2",lineur);
   mq_out_real("samp2",sampur);
   mq_out_real("line3",linell);
   mq_out_real("samp3",sampll);
   mq_out_real("line4",linelr);
   mq_out_real("samp4",samplr);
   mq_out_int("bound2_1",bound2_1);
   mq_out_int("sldat",(int)linetop);
   mq_out_int("ssdat",(int)sampleft);
   mq_out_int("nldat",(int)(linebot-linetop)+1);
   mq_out_int("nsdat",(int)(samprt-sampleft)+1);

   return;
}
