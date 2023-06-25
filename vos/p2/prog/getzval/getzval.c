#include <math.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "vicmain_c.h"
#include "applic.h"
#include "defines.h"
#include "ibisfile.h"
#include "ibiserrs.h"
#include "zifmessage.h"
#include "zmabend.h"

#include "cartoMemUtils.h"
#include "cartoSortUtils.h"

/************************************************************************/
/* program getzval                                                      */
/************************************************************************/
/*  04-00 ...alz... initial version                     */
/*  Fri Dec 28 2007 wlb switched to USES_ANSI_C AND LIB_CARTO; misc cleanup */
/************************************************************************/

void main44(void)
{
   int i,j,k,irec,cols[4],unit,ibis,status,clen,pcount,pdef,iroll;
   int colcount,coldef,window,nreject,algor=0,nline,nsamp,igzero;
   int i_unit,*bptr=NULL,ll,lu,sl,su,llint,slint,icase,rejectcount,dted;
   int threject;
   short int **inbuf=NULL,*ptr=NULL,val,dval;
   char fmt_str[10];
   float spotfac;
   double *x=NULL,*y=NULL,*zval=NULL,*sigval=NULL;
   double woff,sum,wdiv,xfac,yfac,wmod,xfac2,yfac2,dx,dy,dist,rspot;
   double fac=0,val2;
   
   zifmessage("GETZVAL version 2019-09-04");
   
   /* get the basic parameters */
   
   zvparm("cols",cols,&colcount,&coldef,4,0);
   if (colcount<3) zmabend("Getzval requires three or four columns");
   if (colcount==3) cols[3] = 0;
   zvparm("window",&window,&pcount,&pdef,1,0);
   if (window>1001) zmabend("Window parameter must be 1001 or less");
   if (window<2) zmabend("Window parameter must be 2 or more");
   zvparm("reject",&nreject,&pcount,&pdef,1,0);
   zvparm("threject",&threject,&pcount,&pdef,1,0);
   zvparm("spotfac",&spotfac,&pcount,&pdef,1,0);
   igzero = zvptst("IGZERO");
   dted = zvptst("DTED");
   if (zvptst("bilin")) algor = 1;
   if (zvptst("noin")) algor = 2;
   if (zvptst("integral")) algor = 3;
   if (zvptst("spot")) algor = 4;
   if (zvptst("spot_r")) algor = 5;
   if (zvptst("spot_r2")) algor = 6;
   if (algor==2) window = 2;
   if (algor>2&&window%2==0)
      zmabend("INTEGRAL or SPOT algorithms require odd window size");
   rspot = (double)window*0.5;
   
   /* read in data from the ibis interface file */
   status = zvunit(&unit,"inp",2, NULL);
   status = IBISFileOpen(unit,&ibis,"update",0,0,0,0);
   if (status!=1) IBISSignalU(unit,status,1);
   IBISFileGet(ibis,"nr",&clen,1,1,0);
   
   mz_alloc1((unsigned char **)&x,clen,8);
   mz_alloc1((unsigned char **)&y,clen,8);
   mz_alloc1((unsigned char **)&bptr,clen,4);
   
   status = IBISColumnSet(ibis,"U_FORMAT","DOUB",cols[0]);
   if (status!=1) IBISSignal(ibis,status,1);
   status = IBISColumnRead(ibis,(char*)x,cols[0],1,clen);
   if (status!=1) IBISSignal(ibis,status,1);
   
   status = IBISColumnSet(ibis,"U_FORMAT","DOUB",cols[1]);
   if (status!=1) IBISSignal(ibis,status,1);
   status = IBISColumnRead(ibis,(char *)y,cols[1],1,clen);
   if (status!=1) IBISSignal(ibis,status,1);
   
   /* sort the (x,y) into x order so image read is sequential */
   
   for (i=0;i<clen;i++) bptr[i] = i+1;
   sort8(x,bptr,clen);
   sortrec8(y,bptr,clen);
   
   /* open the image file */
   status = zvunit( &i_unit, "INP", 1, NULL);
   status = zvopen( i_unit, "OPEN_ACT", "SA", "IO_ACT", "SA",
			"U_FORMAT","HALF", NULL);
   zvget(i_unit,"FORMAT",fmt_str, NULL);
   zvget(i_unit,"NL",&nline, NULL);
   zvget(i_unit,"NS",&nsamp, NULL);
   if ( strcmp(fmt_str,"BYTE") && strcmp(fmt_str,"HALF"))
      zmabend("Invalid input data format.  Use BYTE or HALF.");
   mz_alloc2((unsigned char ***)&inbuf,window,nsamp,2);
   mz_alloc1((unsigned char **)&zval,clen,8);
   if (cols[3]!=0) mz_alloc1((unsigned char **)&sigval,clen,8);
   mz_alloc1((unsigned char **)&ptr,nline,2);
   for (i=0;i<nline;i++) ptr[i] = -1;
   
   /* get the z values */
   /* this will only read if needed and skips unneeded lines.  It also never 
   rereads a line. inbuf is a "rolling barrel" and ptr keeps track of line location */
   
   woff = 0.5*(double)window-1.0;
   wmod = 0.5*(double)(window%2);
   iroll = 0;
   for (irec=0;irec<clen;irec++)
      {
      ll = (int)(x[irec]-woff)-1;
      llint = (int)(x[irec]-wmod);
      lu = ll+window;
      if (ll<0||lu>nline)
         {
         zval[irec] = -9999.0;
         if (cols[3]!=0) sigval[irec] = -9999.0;
         continue;
         }
      for (i=ll;i<lu;i++)
         {
         if (ptr[i]==(-1))
            {
            status = zvread(i_unit,inbuf[iroll],"LINE",i+1, NULL);
            if (dted) for (k=0;k<nsamp;k++)
               {
               dval = inbuf[iroll][k];
               if (dval<-5000) inbuf[iroll][k] = -(32768+dval);
               }
            ptr[i] = iroll++;
            if (iroll>=window) iroll = 0;
            }
         }
      sl = (int)(y[irec]-woff)-1;
      slint = (int)(y[irec]-wmod);
      su = sl+window;
      xfac = x[irec]-wmod-(double)llint;
      yfac = y[irec]-wmod-(double)slint;
      xfac2 = 1.-xfac;
      yfac2 = 1.-yfac;
   switch (algor)  /* calculate the mean */
      {
      case 2:
         {
         if (xfac<0.5&&(ll<0||ll>nline)) goto rejectexitn;
         if (xfac>0.5&&(ll+1<0||ll+1>nline)) goto rejectexitn;
         if (yfac<0.5&&(sl<0||sl>nsamp)) goto rejectexitn;
         if (yfac>0.5&&(sl+1<0||sl+1>nsamp)) goto rejectexitn;
         if (xfac<0.5)
            {
            if (yfac<0.5) zval[irec] = (double)inbuf[ptr[ll]][sl];
               else zval[irec] = (double)inbuf[ptr[ll]][sl+1];
            }
         else
            {
            if (yfac<0.5) zval[irec] = (double)inbuf[ptr[ll+1]][sl];
               else zval[irec] = (double)inbuf[ptr[ll+1]][sl+1];
            }
         break;
         rejectexitn: zval[irec] = -9999.0;
         break;
         } /* end of case 2 */
      case 1:
         {
         if (ll<0||sl<0||lu>nline||su>nsamp) goto rejectexitb;
         sum = 0.0;
         wdiv = 0.0;
         rejectcount = 0;
         for (i=ll;i<lu;i++)
            for (j=sl;j<su;j++)
               {
               val = inbuf[ptr[i]][j];
               if (val<=threject)
                  {
                  rejectcount++;
                  if (rejectcount>=nreject) goto rejectexitb;
                  if (igzero) continue;
                  }
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
               /*printf("i,j,icase %d %d %d\n",i,j,icase);*/
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
         if (wdiv>0.0) zval[irec] = sum/wdiv; else zval[irec] = 0.0;
         break;
         rejectexitb: zval[irec] = -9999.0;
         break;
         } /* end of case 1 */
      case 3:
         {
         if (ll<0||sl<0||lu>nline||su>nsamp) goto rejectexitc;
         sum = 0.0;
         wdiv = 0.0;
         rejectcount = 0;
         for (i=ll;i<lu;i++)
            for (j=sl;j<su;j++)
               {
               val = inbuf[ptr[i]][j];
               if (val<=threject)
                  {
                  rejectcount++;
                  if (rejectcount>=nreject) goto rejectexitc;
                  if (igzero) continue;
                  }
               sum += (double)val;
               wdiv += 1.0;
               }
         if (wdiv>0.0) zval[irec] = sum/wdiv; else zval[irec] = 0.0;
         break;
         rejectexitc: zval[irec] = -9999.0;
         break;
         } /* end of case 3 */
      case 4: case 5: case 6:
         {
         if (ll<0||sl<0||lu>nline||su>nsamp) goto rejectexitd;
         sum = 0.0;
         wdiv = 0.0;
         rejectcount = 0;
         for (i=ll;i<lu;i++)
            for (j=sl;j<su;j++)
               {
               dx = x[irec]-(double)(i+1);
               dy = y[irec]-(double)(j+1);
               dist = sqrt(dx*dx+dy*dy);
               if (dist>rspot) continue;
               if (algor==6) dist = dist*dist;
               dist = dist+spotfac;
               if (algor==4) dist = 1.0;
               val = inbuf[ptr[i]][j];
               if (val<=threject)
                  {
                  rejectcount++;
                  if (rejectcount>=nreject) goto rejectexitd;
                  if (igzero) continue;
                  }
               sum += (double)val/dist;
               wdiv += 1.0/dist;
               }
         if (wdiv>0.0) zval[irec] = sum/wdiv; else zval[irec] = 0.0;
         break;
         rejectexitd: zval[irec] = -9999.0;
         break;
         } /* end of case 4,5,6 */
      } /* end of switch(algor) for the mean*/
      
      if (cols[3]==0) goto nosig;
      if (zval[irec]==(-9999.0)) sigval[irec] = -9999.0;
      else switch (algor)  /* calculate the sigma */
      {
      case 2:
         {
         zmabend("Nearest neighbor case does not have a sigma");
         } /* end of case 2 */
      case 1:
         {
         sum = 0.0;
         wdiv = 0.0;
         for (i=ll;i<lu;i++)
            for (j=sl;j<su;j++)
               {
               val = inbuf[ptr[i]][j];
               if (val==0&&igzero) continue;
               val = val-zval[irec];
               val2 = (double)(val*val);
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
               /*printf("i,j,icase %d %d %d\n",i,j,icase);*/
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
               sum += val2*fac;
               wdiv += fac;
               }
         sigval[irec] = sqrt(sum/wdiv);
         if (wdiv>0.0) sigval[irec] = sqrt(sum/wdiv); else sigval[irec] = 0.0;
         break;
         } /* end of case 1 */
      case 3:
         {
         sum = 0.0;
         wdiv = 0.0;
         for (i=ll;i<lu;i++)
            for (j=sl;j<su;j++)
               {
               val = inbuf[ptr[i]][j];
               if (val==0&&igzero) continue;
               val = val-zval[irec];
               val2 = (double)(val*val); 
               sum += val2;
               wdiv += 1.0;
               }
         if (wdiv>0.0) sigval[irec] = sqrt(sum/wdiv); else sigval[irec] = 0.0;
         break;
         } /* end of case 3 */
      case 4: case 5: case 6:
         {
         sum = 0.0;
         wdiv = 0.0;
         for (i=ll;i<lu;i++)
            for (j=sl;j<su;j++)
               {
               val = inbuf[ptr[i]][j];
               if (val==0&&igzero) continue;
               val = val-zval[irec];
               val2 = (double)(val*val); 
               dx = x[irec]-(double)(i+1);
               dy = y[irec]-(double)(j+1);
               dist = sqrt(dx*dx+dy*dy);
               if (dist>rspot) continue;
               if (algor==6) dist = dist*dist;
               dist = dist+spotfac;
               if (algor==4) dist = 1.0;
               sum += val2/dist;
               wdiv += 1.0/dist;
               }
         if (wdiv>0.0) sigval[irec] = sqrt(sum/wdiv); else sigval[irec] = 0.0;
         break;
         } /* end of case 4,5,6 */
      } /* end of switch(algor) for the sigma*/
      nosig: continue;
      
      } /* end of irec loop */
      
   /* put the output column back into the original order */
   
   free(x);
   free(y);
   sortretn8(zval,bptr,clen);
   if (cols[3]!=0) sortretn8(sigval,bptr,clen);
   
   /* Output desired columns to the ibis interface file */
   
   status = IBISColumnSet(ibis,"U_FORMAT","DOUB",cols[2]);
   if (status!=1) IBISSignal(ibis,status,1);
   status = IBISColumnWrite(ibis,(char*)zval,cols[2],1,clen);
   if (status!=1) IBISSignal(ibis,status,1);
   if (cols[3]!=0)
      {
      status = IBISColumnSet(ibis,"U_FORMAT","DOUB",cols[3]);
      if (status!=1) IBISSignal(ibis,status,1);
      status = IBISColumnWrite(ibis,(char*) sigval,cols[3],1,clen);
      if (status!=1) IBISSignal(ibis,status,1);
      }
   /* close and return */
   
   status = IBISFileClose(ibis,0);
   if (status!=1) IBISSignal(ibis,status,1);
  
   return;
}
