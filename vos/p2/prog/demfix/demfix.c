#include <math.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>

#include "vicmain_c.h"
#include "applic.h"
#include "defines.h"
#include "ibisfile.h"
#include "ibiserrs.h"
#include "zifmessage.h"
#include "zmabend.h"

#include "cartoLsqUtils.h"
#include "cartoStrUtils.h"
#include "cartoMemUtils.h"
#include "cartoGtUtils.h"
#include "cartoSortUtils.h"

/************************************************************************/
/* program demfix                                                      */
/************************************************************************/
/*  04-00 ...alz... initial version                     */
/************************************************************************/

void main44(void)
{
   int HOLEPIXNUM = 8000000;  /* max num of pixels in "holes" */
   int OCEANPIX = 12960000;   /* max num of pixels in "ocean" */
   int BANDWIDTH = 100;       /* half width of interpolation band */
   int EDGECOMPSIZE = 10;     /* size of edge pieces treated as holes if fixing edges */
   
   int i,j,k,irec,status,pcount,pdef,iroll,inpcount;
   int window,nline1,nsamp1,nline2=0,nsamp2=0,thresh,labnl,labns;
   int i_unit1,i_unit2,o_unit,*bptr,ll,lu,sl,su,llint,slint,icase;
   int len,ncomp,bigix,icur,iprev,pcomp,iline,jsamp,val1,lval,uval;
   int itemp,hptr,icomp,qcomp,notfound,ptrl,sptr,doedge;
   int ecount,becount,mcount,bmcount,d1,d2,dopow2,ocptr1,ocptr2;
   short int **inbuf=NULL,**inbuf2=NULL,*inbufr=NULL,*ptr=NULL,val,*hval2=NULL,*boost=NULL,*ocnl=NULL,*ocns=NULL;
   int *hline=NULL,*hsamp=NULL,*hcomp=NULL,*inbufcomp=NULL,*bsum1=NULL,*bsum2=NULL,*bcount=NULL,*hval=NULL;
   int *shline=NULL,*shsamp=NULL,*shcomp=NULL,*shresid=NULL,curline2,ocnthrsh,noneg;
   char fmt_str[10];
   float demfac;
   double woff,sum,wdiv,xfac,yfac,wmod,xfac2,yfac2,powparam;
   double fac=0,xval,yval,vlon,vlat,boosttemp,rsum,dist,dx,dy,rdiv;
   char *labelstr=NULL;
   double t1[6],t1inv[6],t2[6],t2inv[6],corner[4];
   char msgBuf[1000];
   
   zifmessage("DEMFIX version 2019-08-22");
   
   /* get the basic parameters */
   
   window = 2;
   woff = 0.5*(double)window-1.0;
   wmod = 0.5*(double)(window%2);
   zvparm("thresh",&thresh,&pcount,&pdef,1,0);
   zvparm("ocnthrsh",&ocnthrsh,&pcount,&pdef,1,0);
   zvparm("demfac",&demfac,&pcount,&pdef,1,0);
   zvparmd("pow",&powparam,&pcount,&pdef,1,0);
   if (powparam>1.99999&&powparam<2.00001) dopow2 = 1; else dopow2 = 0;
   powparam *= 0.5;
   doedge = -1;
   if (zvptst("edgeoff")) doedge = 0;
   if (zvptst("edgeon")) doedge = 1;
   noneg = zvptst("noneg");
   
   /* the auto case for the edge */
   
   if (doedge==(-1)||zvptst("edgeauto"))
      {
      status = zvunit( &i_unit1, "INP", 1, NULL);
      status = zvopen( i_unit1, "OPEN_ACT", "SA", "IO_ACT", "SA",
			"U_FORMAT","HALF", NULL);
      zvget(i_unit1,"NL",&nline1, NULL);
      zvget(i_unit1,"NS",&nsamp1, NULL);
      mz_alloc2((unsigned char ***)&inbuf,2,nsamp1,2);
      
      ecount = 0; becount = 0; mcount = 0; bmcount = 0;
      icur = 0;
      iprev = 1;
      for (iline=0;iline<nline1;iline++)
         {
         status = zvread(i_unit1,inbuf[icur],"LINE",iline+1, NULL);
         d1 = inbuf[icur][0]-inbuf[icur][1]; if (d1<0) d1 = -d1;
         d2 = inbuf[icur][1]-inbuf[icur][2]; if (d2<0) d2 = -d2;
         if (d1>4*(d2+5)&&inbuf[icur][0]>thresh&&inbuf[icur][1]>thresh&&
            inbuf[icur][2]>thresh) becount++;
         d1 = inbuf[icur][nsamp1-1]-inbuf[icur][nsamp1-2]; if (d1<0) d1 = -d1;
         d2 = inbuf[icur][nsamp1-2]-inbuf[icur][nsamp1-3]; if (d2<0) d2 = -d2;
         if (d1>4*(d2+5)&&inbuf[icur][nsamp1-1]>thresh&&
            inbuf[icur][nsamp1-2]>thresh&&inbuf[icur][nsamp1-3]>thresh) becount++;
         ecount +=2;
         for (i=1;i<nsamp1-3;i++)
            {
            d1 = inbuf[icur][i]-inbuf[icur][i+1]; if (d1<0) d1 = -d1;
            d2 = inbuf[icur][i+1]-inbuf[icur][i+2]; if (d2<0) d2 = -d2;
            if (d1>4*(d2+5)&&inbuf[icur][i]>thresh&&inbuf[icur][i+1]>thresh&&
               inbuf[icur][i+2]>thresh) bmcount++;
            mcount++;
            }
         if (iline==1||iline==nline1-1)
            {
            for (i=1;i<nsamp1-2;i++)
               {
               d1 = inbuf[icur][i]-inbuf[iprev][i]; if (d1<0) d1 = -d1;
               if (iline==1) d2 = inbuf[icur][i]-inbuf[icur][i+1];
                  else d2 = inbuf[iprev][i]-inbuf[iprev][i+1];
               if (d2<0) d2 = -d2;
               if (d1>4*(d2+5)&&inbuf[icur][i]>thresh&&inbuf[iprev][i]>thresh&&
                  inbuf[icur][i+1]>thresh) becount++;
               ecount++;
               }
            }
         itemp = iprev;
         iprev = icur;
         icur = itemp;
         }
      if (((float)becount/(float)ecount)>(4.0*(float)(bmcount+2)/(float)mcount))
         {
         doedge = 1;
         sprintf(msgBuf, "performing edge fix, statistics edge=%d/%d interior=%d/%d",
            becount,ecount,bmcount,mcount);
	 zifmessage(msgBuf);
         }
      else
         {
         doedge = 0;
         sprintf(msgBuf, "no edge fix, statistics edge=%d/%d interior=%d/%d",
            becount,ecount,bmcount,mcount);
	 zifmessage(msgBuf);
         }
      zvclose(i_unit1, NULL);
      }
   
   /* get the mappings */
   
   status = gtgetlab("inp",1,&labelstr,&labnl,&labns);
   len = strlen(labelstr);
   for (i=0;i<len;i++) labelstr[i] = toupper(labelstr[i]);
   status = geofix(labelstr,t1,t1inv,labnl,labns,corner);
   if (status!=1) zmabend("Failed to get mapping from primary GeoTIFF label");
   
   status = zvpcnt("inp",&inpcount);
   if (inpcount>1)
      {
      status = gtgetlab("inp",2,&labelstr,&labnl,&labns);
      len = strlen(labelstr);
      for (i=0;i<len;i++) labelstr[i] = toupper(labelstr[i]);
      status = geofix(labelstr,t2,t2inv,labnl,labns,corner);
      if (status!=1) zmabend("Failed to get mapping from secondary GeoTIFF label");
      }
   
   /* read the primary DEM file two lines at a time for connected component 
   analysis for the "holes"; open unit 2 for ocean processing */
   
   status = zvunit( &i_unit1, "INP", 1, NULL);
   status = zvopen( i_unit1, "OPEN_ACT", "SA", "IO_ACT", "SA",
			"U_FORMAT","HALF", NULL);
   zvget(i_unit1,"FORMAT",fmt_str, NULL);
   zvget(i_unit1,"NL",&nline1, NULL);
   zvget(i_unit1,"NS",&nsamp1, NULL);
   if ( strcmp(fmt_str,"BYTE") && strcmp(fmt_str,"HALF"))
      zmabend("Invalid input data format.  Use BYTE or HALF.");
   
   if (inpcount>1)
      {
      status = zvunit( &i_unit2, "INP", 2, NULL);
      status = zvopen( i_unit2, "OPEN_ACT", "SA", "IO_ACT", "SA",
			"U_FORMAT","HALF", NULL);
      zvget(i_unit2,"FORMAT",fmt_str, NULL);
      zvget(i_unit2,"NL",&nline2, NULL);
      zvget(i_unit2,"NS",&nsamp2, NULL);
      if ( strcmp(fmt_str,"BYTE") && strcmp(fmt_str,"HALF"))
         zmabend("Invalid input data format.  Use BYTE or HALF.");
      mz_alloc2((unsigned char ***)&inbuf2,window,nsamp2,2);
      mz_alloc1((unsigned char **)&ocnl,OCEANPIX,2);
      mz_alloc1((unsigned char **)&ocns,OCEANPIX,2);
      }
   
   mz_alloc1((unsigned char **)&inbufcomp,nsamp1,4);
   mz_alloc1((unsigned char **)&hline,HOLEPIXNUM,4);
   mz_alloc1((unsigned char **)&hsamp,HOLEPIXNUM,4);
   mz_alloc1((unsigned char **)&hcomp,HOLEPIXNUM,4);
   mz_alloc1((unsigned char **)&hval,HOLEPIXNUM,4); /* need 4 for sort */
   mz_alloc1((unsigned char **)&hval2,HOLEPIXNUM,2);
   mz_alloc1((unsigned char **)&bptr,HOLEPIXNUM,4);
   
   ncomp = 0;
   bigix = 0;
   icur = 0;
   iprev = 1;
   curline2 = -1;
   ocptr1 = 0;
   iroll = 0;
   for (jsamp=0;jsamp<nsamp1;jsamp++) inbufcomp[jsamp] = -1;
   for (iline=0;iline<nline1;iline++)
      {
	if (iline%100==0) {
	  sprintf(msgBuf, "line=%d pixels=%d component=%d",iline,bigix,ncomp);
	  zifmessage(msgBuf);
	}
      status = zvread(i_unit1,inbuf[icur],"LINE",iline+1, NULL);
      if (doedge)
         {
         inbuf[icur][0] = thresh;
         inbuf[icur][nsamp1-1] = thresh;
         if (iline==0||iline==nline1-1) for (i=1;i<nsamp1-1;i++) inbuf[icur][i] = thresh;
         }
      lval = 32767;
      pcomp = -2;
      for (jsamp=0;jsamp<nsamp1;jsamp++)
         {
         val1 = inbuf[icur][jsamp];
         if (val1<=ocnthrsh&&inpcount>1)
            {
            vlon = t1[0]*(double)iline+t1[1]*(double)jsamp+t1[2];
            vlat = t1[3]*(double)iline+t1[4]*(double)jsamp+t1[5];
            xval = t2inv[0]*vlon+t2inv[1]*vlat+t2inv[2];
            yval = t2inv[3]*vlon+t2inv[4]*vlat+t2inv[5];
            ll = (int)(xval-woff)-1;
            if (ll<0||ll>nline2-2) 
               {
               if (ll<-50||ll>nline2+50)
                  zmabend("input 1 not same area as input 2");
               else if (ll==-1) ll = 0;
               else if (ll==nline2-1) ll = nline2-2;
               else goto notocean;
               }
            llint = (int)(xval-wmod);
            lu = ll+1;
            
            if (ll>curline2)
               {
               status = zvread(i_unit2,inbuf2[iroll],"LINE",ll+1, NULL);
               iroll = 1-iroll;
               curline2 = ll;
               }
            if (lu>curline2)
               {
               status = zvread(i_unit2,inbuf2[iroll],"LINE",lu+1, NULL);
               iroll = 1-iroll;
               curline2 = lu;
               }
               
            sl = (int)(yval-woff)-1;
            slint = (int)(yval-wmod);
            if (sl<0||sl>nsamp2-2)
               {
               if (sl<-50||sl>nsamp2+50)
                  zmabend("input 1 not same area as input 2");
               else if (sl==-1) sl = 0;
               else if (sl==nsamp2-1) sl = nline2-2;
               else goto notocean;
               }
            su = sl+1;
            
            /* keep both sets of code here 1) bilinear==0 2) nearest nbr=0*/
            if (inbuf2[0][sl]!=0||inbuf2[1][sl]!=0) goto notocean;
            if (inbuf2[0][su]!=0||inbuf2[1][su]!=0) goto notocean;
            
            /*xfac = xval-wmod-(double)llint;
            yfac = yval-wmod-(double)slint;
            if (inbuf2[(int)(xfac>0.5)][su+(int)(yfac>0.5)]!=0) goto notocean;*/
            
            inbuf[icur][jsamp] = 32000;
            ocnl[ocptr1] = iline; ocns[ocptr1++] = jsamp;
            goto ocean;
            }
         
         notocean:
         if (val1<=thresh)
            {
            if (iline>0) uval = inbuf[iprev][jsamp]; else uval = 32767;
            if (lval<=thresh)
               {
               if (uval<=thresh)
                  {
                  if (pcomp==inbufcomp[jsamp])
                     {
                     /*this pixel connects both ways, but they are same*/
                     hline[bigix] = iline;
                     hsamp[bigix] = jsamp;
                     hcomp[bigix] = pcomp;
                     hval[bigix] = thresh;
                     }
                  else
                     {
                     /*have to join left and upper components*/
                     hline[bigix] = iline;
                     hsamp[bigix] = jsamp;
                     hcomp[bigix] = pcomp;
                     hval[bigix] = thresh;
                     /*if this ever gets slow, will have to do circ lnk list*/
                     qcomp = inbufcomp[jsamp];
                     for (k=0;k<=bigix;k++)
                        if (hcomp[k]==pcomp) hcomp[k] = qcomp;
                     for (k=0;k<=nsamp1;k++)
                        if (inbufcomp[k]==pcomp) inbufcomp[k] = qcomp;
                     if (ncomp==pcomp+1) ncomp--;
                     pcomp = qcomp;
                     }
                  }
               else
                  {
                  /*this pixel connects to the left only*/
                  if (doedge&&jsamp%EDGECOMPSIZE==0&&(iline==0||iline==nline1-1))
                     {
                     /*new component*/
                     hline[bigix] = iline;
                     hsamp[bigix] = jsamp;
                     hcomp[bigix] = ncomp++;
                     hval[bigix] = thresh;
                     }
                  else
                     {
                     hline[bigix] = iline;
                     hsamp[bigix] = jsamp;
                     hcomp[bigix] = pcomp;
                     hval[bigix] = thresh;
                     }
                  }
               }
            else
               {
               if (uval<=thresh)
                  {
                  /*this pixel connects above only*/
                  if (doedge&&iline%EDGECOMPSIZE==0&&(jsamp==0||jsamp==nsamp1-1))
                     {
                     /*new component*/
                     hline[bigix] = iline;
                     hsamp[bigix] = jsamp;
                     hcomp[bigix] = ncomp++;
                     hval[bigix] = thresh;
                     }
                  else
                     {
                     hline[bigix] = iline;
                     hsamp[bigix] = jsamp;
                     hcomp[bigix] = inbufcomp[jsamp];
                     hval[bigix] = thresh;
                     }
                  }
               else
                  {
                  /*new component*/
                  hline[bigix] = iline;
                  hsamp[bigix] = jsamp;
                  hcomp[bigix] = ncomp++;
                  hval[bigix] = thresh;
                  }
               }
            /*the component is set*/
            pcomp = hcomp[bigix];
            inbufcomp[jsamp] = hcomp[bigix++];
            /*process any non-0 nbrs*/
            if (jsamp>0&&lval>thresh)
               {
               hline[bigix] = iline;
               hsamp[bigix] = jsamp-1;
               hcomp[bigix] = pcomp;
               if (lval!=32000) hval[bigix++] = lval;
                  else hval[bigix++] = 0;
               }
            if (iline>0&&uval>thresh)
               {
               notfound = 1;
               for (k=0;k<bigix;k++)
                  if ((hline[k]==iline-1)&&hsamp[k]==jsamp) notfound = 0;
               if (notfound)
                  {
                  hline[bigix] = iline-1;
                  hsamp[bigix] = jsamp;
                  hcomp[bigix] = pcomp;
                  if (uval!=32000) hval[bigix++] = uval;
                     else hval[bigix++] = 0;
                  }
               }
            }
         else
            {
            /*look for adjacent 0 components, max of 1, ignore diag touch case*/
            if (pcomp>=0)
               {
               hline[bigix] = iline;
               hsamp[bigix] = jsamp;
               hcomp[bigix] = pcomp;
               hval[bigix++] = val1;
               }
            else if (inbufcomp[jsamp]>=0)
               {
               hline[bigix] = iline;
               hsamp[bigix] = jsamp;
               hcomp[bigix] = inbufcomp[jsamp];
               hval[bigix++] = val1;
               }
            inbufcomp[jsamp] = -1;
            pcomp = -2;
            }
         ocean:
         lval = inbuf[icur][jsamp];
         if (bigix>=HOLEPIXNUM) zmabend("too many pixels in holes");
         }
      itemp = iprev;
      iprev = icur;
      icur = itemp;
      }
   zvclose(i_unit1, NULL);
   if (inpcount>1) zvclose(i_unit2, NULL);
   
   /* sort the h-vectors into vertical order */
   
   for (i=0;i<bigix;i++) bptr[i] = i+1;
   sort4(hline,bptr,bigix);
   sortrec4(hsamp,bptr,bigix);
   sortrec4(hcomp,bptr,bigix);
   sortrec4(hval,bptr,bigix);
   free(bptr);
   
   /* open the secondary DEM file */
   
if (inpcount>1)
   {
     zifmessage("reading secondary dem file");

   /*for (i=0;i<bigix;i++) 
     printf("l,s,hcomp,hval %5d %5d %5d %6d\n",
             hline[i],hsamp[i],hcomp[i],hval[i]);*/
   
   mz_free2((unsigned char **)inbuf,2);
   status = zvunit( &i_unit2, "INP", 2, NULL);
   status = zvopen( i_unit2, "OPEN_ACT", "SA", "IO_ACT", "SA",
			"U_FORMAT","HALF", NULL);
   zvget(i_unit2,"FORMAT",fmt_str, NULL);
   zvget(i_unit2,"NL",&nline2, NULL);
   zvget(i_unit2,"NS",&nsamp2, NULL);
   if ( strcmp(fmt_str,"BYTE") && strcmp(fmt_str,"HALF"))
      zmabend("Invalid input data format.  Use BYTE or HALF.");
   mz_alloc2((unsigned char ***)&inbuf2,window,nsamp2,2);
   mz_alloc1((unsigned char **)&ptr,nline2,2);
   for (i=0;i<nline2;i++) ptr[i] = -1;
   
   /* get the z values */
   /* this will only read if needed and skips unneeded lines.  It also never 
   rereads a line. inbuf2 is a "rolling barrel" and ptr keeps track of line location */
   
   iroll = 0;
   for (irec=0;irec<bigix;irec++)
      {
      vlon = t1[0]*(double)hline[irec]+t1[1]*(double)hsamp[irec]+t1[2];
      vlat = t1[3]*(double)hline[irec]+t1[4]*(double)hsamp[irec]+t1[5];
      xval = t2inv[0]*vlon+t2inv[1]*vlat+t2inv[2];
      yval = t2inv[3]*vlon+t2inv[4]*vlat+t2inv[5];
      ll = (int)(xval-woff)-1;
      if (ll<0) ll = 0; if (ll>nline2-2) ll = nline2-2;
      llint = (int)(xval-wmod);
      lu = ll+window;
      if (ll<0||lu>nline2)
         {
         hval2[irec] = -32767;
         continue;
         }
      for (i=ll;i<lu;i++)
         {
         if (ptr[i]==(-1))
            {
            status = zvread(i_unit2,inbuf2[iroll],"LINE",i+1, NULL);
            ptr[i] = iroll++;
            if (iroll>=window) iroll = 0;
            }
         }
      sl = (int)(yval-woff)-1;
      slint = (int)(yval-wmod);
      if (sl<0) sl = 0; if (sl>nsamp2-2) sl = nsamp2-2;
      su = sl+window;
      xfac = xval-wmod-(double)llint;
      yfac = yval-wmod-(double)slint;
      xfac2 = 1.-xfac;
      yfac2 = 1.-yfac;
      
      if (ll<0||sl<0||lu>nline2||su>nsamp2)
         {
         hval2[irec] = -32767;
         continue;
         }
      sum = 0.0;
      wdiv = 0.0;
      for (i=ll;i<lu;i++)
         for (j=sl;j<su;j++)
            {
            val = inbuf2[ptr[i]][j];
            if (i==ll)
               {
               if (j==sl) icase = 1;
               else if (j==su-1) icase = 3;
               else icase = 2;
               }
            else if (i==lu-1)
               {
               if (j==sl) icase = 7;
               else if (j==su-1) icase = 9;
               else icase = 8;
               }
            else
               {
               if (j==sl) icase = 4;
               else if (j==su-1) icase = 6;
               else icase = 5;
               }
            switch (icase)
               {
               case 1: fac = xfac2*yfac2; break;
               case 2: fac = xfac2; break;
               case 3: fac = xfac2*yfac; break;
               case 4: fac = yfac2; break;
               case 5: fac = 1.0; break;
               case 6: fac = yfac; break;
               case 7: fac = xfac*yfac2; break;
               case 8: fac = xfac; break;
               case 9: fac = xfac*yfac;
               }
            sum += (double)val*fac;
            wdiv += fac;
            }
      hval2[irec] = (int)(sum*demfac/wdiv+0.5);
      } /* end of irec loop */
   mz_free2((unsigned char **)inbuf2,2);
   }
   else for (i=0;i<bigix;i++) hval2[i] = 0;
  
   /* printout for debugging */
   
   /*for (i=0;i<bigix;i++) if (hcomp[i]==37)
      printf("i,hline[i],hsamp[i],hcomp[i],hval[i],hval2[i] %d %d %d %d %d %d\n",
              i,hline[i],hsamp[i],hcomp[i],hval[i],hval2[i]);*/
   
   /* calculate the boosts for each component */
   
   mz_alloc1((unsigned char **)&bsum1,ncomp,4);
   mz_alloc1((unsigned char **)&bsum2,ncomp,4);
   mz_alloc1((unsigned char **)&bcount,ncomp,4);
   mz_alloc1((unsigned char **)&boost,ncomp,2);
   for (i=0;i<ncomp;i++)
      {
      bsum1[i] = 0;
      bsum2[i] = 0;
      bcount[i] = 0;
      }
   
   for (i=0;i<bigix;i++)
      {
      icomp = hcomp[i];
      if (hval[i]>thresh&&hval2[i]!=-32767)
         {
         bsum1[icomp] += hval[i];
         bsum2[icomp] += hval2[i];
         bcount[icomp]++;
         }
      }
   for (i=0;i<ncomp;i++)
      {
      if (bcount[i]>0)
         {
         boosttemp = (double)(bsum1[i]-bsum2[i])/(double)bcount[i];
         if (boosttemp<0.0) boosttemp -= 0.5; else boosttemp += 0.5;
         boost[i] = (int)boosttemp;
         }
      /*printf("i,bsum1[i],bsum2[i],bcount[i],boost[i] %d %d %d %d %d\n",
              i,bsum1[i],bsum2[i],bcount[i],boost[i]);*/
      }
   
   /* add boosts to array; residuals now val1-val2; malloc the secondary array */
   
   for (i=0;i<bigix;i++) hval2[i] += boost[hcomp[i]];
   mz_alloc1((unsigned char **)&shline,BANDWIDTH*nsamp1,4);
   mz_alloc1((unsigned char **)&shsamp,BANDWIDTH*nsamp1,4);
   mz_alloc1((unsigned char **)&shcomp,BANDWIDTH*nsamp1,4);
   mz_alloc1((unsigned char **)&shresid,BANDWIDTH*nsamp1,4);
   
   /* put values into the image */
   
   zifmessage("final update of primary file");
   if (inpcount>1) zvclose(i_unit2, NULL);
   status = zvunit( &i_unit1, "INP", 1, NULL);
   status = zvopen( i_unit1, "OPEN_ACT", "SA", "IO_ACT", "SA",
			"U_FORMAT","HALF", NULL);
   status=zvunit(&o_unit,"OUT",1, NULL);
   status=zvopen(o_unit,"U_NL",nline1,"U_NS",nsamp1,
	"OP","WRITE","OPEN_ACT","SA","IO_ACT","SA","U_FORMAT","HALF", NULL);
   mz_alloc1((unsigned char **)&inbufr,nsamp1,2);
   
   hptr = 0;
   ptrl = 0;
   ocptr2 = 0;
   for (iline=0;iline<nline1;iline++)
      {
      status = zvread(i_unit1,inbufr,"LINE",iline+1, NULL);
      if (doedge)
         {
         inbufr[0] = thresh;
         inbufr[nsamp1-1] = thresh;
         if (iline==0||iline==nsamp1-1) for (i=1;i<nsamp1-1;i++) inbufr[i] = thresh;
         }
      
      /* load the secondary array for interpolation of residuals */
      if (bigix==0) goto noholes;
      sptr = 0;
      for (i=ptrl;i<bigix;i++)
         {
         if (hline[i]<=iline+BANDWIDTH&&hval[i]>thresh)
            {
            shline[sptr] = hline[i];
            shsamp[sptr] = hsamp[i];
            shcomp[sptr] = hcomp[i];
            shresid[sptr++] = hval[i]-hval2[i];
            }
         if (hline[i]<iline-BANDWIDTH) ptrl = i+1;
         if (hline[i]>iline+BANDWIDTH) break;
         }
      if (sptr>=BANDWIDTH*nsamp1) zmabend("Too many edge pixels");
      
      while (hline[hptr]==iline)
         {
         if (inbufr[hsamp[hptr]]<=thresh)
            {
            /* calculate the 1/d interpolation of the residuals */
            rsum = 0.0;
            rdiv = 0.0;
            for (i=0;i<sptr;i++)
               {
               if (hcomp[hptr]!=shcomp[i]) continue;
               dx = (double)(hline[hptr]-shline[i]);
               dy = (double)(hsamp[hptr]-shsamp[i]);
               if (dopow2) dist = dx*dx+dy*dy;
                  else dist = pow(dx*dx+dy*dy,powparam);
               rsum += shresid[i]/dist;
               rdiv += 1.0/dist;
               }
            if (rdiv>0.0) rsum /= rdiv; else rsum = 0.0;
            
            /* the final calculation */ 
            inbufr[hsamp[hptr]] = hval2[hptr]+rsum;
            }
         hptr++;
         }
      noholes:
      while (ocptr2<ocptr1&&ocnl[ocptr2]==iline)
         inbufr[ocns[ocptr2++]] = 0;
      if (noneg) for (i=0;i<nsamp1;i++)
         if (inbufr[i]<0) inbufr[i] = 0;
      status = zvwrit(o_unit,inbufr,"LINE",iline+1, NULL);
      }
      
   return;
}
