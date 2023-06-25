#include "vicmain_c.h"
#include "applic.h"
#include <math.h>
#include <stdio.h>

#include "defines.h"
#include "ibisfile.h"
#include "ibiserrs.h"

#include "cartoMemUtils.h"
#include "cartoGridUtils.h"

/************************************************************************/
/* program bowtie                                                       */
/************************************************************************/
/*  3/23/03 ...alz... initial version                                   */
/*  Fri Dec 28 2007 wlb switched to USES_ANSI_C AND LIB_CARTO; misc cleanup */
/************************************************************************/

int above(xp,yp,x1,y1,x2,y2)
   double xp,yp,x1,y1,x2,y2;
{
   double areax2;
   areax2 = xp*y1-x1*yp+x1*y2-x2*y1+x2*yp-xp*y2;
   if (areax2<0.000001) return(0);
      else return(1);
}

double pdist(xp,yp,x1,y1,x2,y2)
   double xp,yp,x1,y1,x2,y2;
{
   return(fabs(xp*y1-x1*yp+x1*y2-x2*y1+x2*yp-xp*y2)/
      sqrt((x2-x1)*(x2-x1)+(y2-y1)*(y2-y1)));
}

static char msgBuf[10000];

void main44(void)
{
   double *gline,*gsamp,*ptx,*pty,*newptx,*newpty;
   int i,unit,colcount,coldef,ibis,clen,status,ntiepp;
   int null9,gridnah,gridnav,pcount,pdef,cols[4],ncol;
   int ioffset,i_unit,o_unit,inl,ins,pixsiz,nbow,inah,inav,nscan;
   int pointl,pointl2,pointa,pointb,pointu,pointu2,nint,iline;
   int jsamp,iband,ibandl,ibandu,iabove,ibelow,ibow,nahp1;
   double vpointl,vpointu,delx,dely,frac,xloc,yloc,vabove,vbelow;
   double dist1,dist2,totdist,xlocl,xlocr,ylocl,ylocr;
   short int **imbuf,*calcbuf;
        
   zifmessage("bowtie version 2016-01-13");
   
   zvparm("cols",cols,&colcount,&coldef,4,0);
   zvparm("nscan",&nscan,&pcount,&pdef,1,0);
   gridtol = 0.9;
      
   /*read in tiepoints from the ibis interface file, by columns */

   status = zvunit(&unit,"inp",2, NULL);
   status = IBISFileOpen(unit,&ibis,"update",0,0,0,0);
   if (status!=1) IBISSignalU(unit,status,1);
   IBISFileGet(ibis,"nr",&clen,1,1,0);
   IBISFileGet(ibis,"nc",&ncol,1,1,0);
   if (ncol<4) zmabend("need at least four columns");
   ncol = 4;
   mz_alloc1((unsigned char **)&gline,clen,8);
   mz_alloc1((unsigned char **)&gsamp,clen,8);
   mz_alloc1((unsigned char **)&ptx,clen,8);
   mz_alloc1((unsigned char **)&pty,clen,8);
   mz_alloc1((unsigned char **)&newptx,clen,8);
   mz_alloc1((unsigned char **)&newpty,clen,8);
   
   status = IBISColumnSet(ibis,"U_FORMAT","DOUB",cols[0]);
   if (status!=1) IBISSignal(ibis,status,1);
   status = IBISColumnRead(ibis,(char*) gline,cols[0],1,clen);
   if (status!=1) IBISSignal(ibis,status,1);
   
   status = IBISColumnSet(ibis,"U_FORMAT","DOUB",cols[1]);
   if (status!=1) IBISSignal(ibis,status,1);
   status = IBISColumnRead(ibis,(char*) gsamp,cols[1],1,clen);
   if (status!=1) IBISSignal(ibis,status,1);
   
   status = IBISColumnSet(ibis,"U_FORMAT","DOUB",cols[2]);
   if (status!=1) IBISSignal(ibis,status,1);
   status = IBISColumnRead(ibis,(char*) ptx,cols[2],1,clen);
   if (status!=1) IBISSignal(ibis,status,1);
   
   status = IBISColumnSet(ibis,"U_FORMAT","DOUB",cols[3]);
   if (status!=1) IBISSignal(ibis,status,1);
   status = IBISColumnRead(ibis,(char*) pty,cols[3],1,clen);
   if (status!=1) IBISSignal(ibis,status,1);
   
   for (i=0;i<clen;i++)
      {
      newptx[i] = ptx[i];
      newpty[i] = pty[i];
      }
   
   ntiepp = clen;
   
   /* detect grid here and set gridnah */
   
   cartogetline(gline,gsamp,0,1,ntiepp,ntiepp,&gridnah,&null9);
   if (null9==1) zmabend("problem with first line of grid");
   if (gridnah==0) zmabend("problem with first line of grid.");
   if (ntiepp%(gridnah+1)!=0) zmabend("problem with first line of grid..");
   gridnav = ntiepp/(gridnah+1)-1;
   if (ntiepp%(gridnav+1)!=0) zmabend("problem with first line of grid...");
   nahp1 = gridnah+1;
   sprintf(msgBuf, "gridnah,gridnav %d %d", gridnah, gridnav);
   zifmessage(msgBuf);
   
   /* determine offset for partial set of scan lines */
   
   ioffset = -1;
   for (i=1;i<nscan+1;i++)
      {
      if (ptx[i*nahp1]<ptx[(i-1)*nahp1]||
          ptx[i*nahp1+gridnah]<ptx[(i-1)*nahp1+gridnah]) continue;
      ioffset = i;
      break;
      }
   sprintf(msgBuf, "offset=%d", ioffset);
   zifmessage(msgBuf);
   
   /* open the image */  
   
   status = zvunit(&i_unit,"INP",1, NULL);
   status = zvopen(i_unit,"OPEN_ACT","SA","IO_ACT","SA","U_FORMAT","HALF", NULL);
   zvget(i_unit,"NL",&inl,"NS",&ins,"PIX_SIZE",&pixsiz, NULL);
   if (pixsiz>2) zmabend("only for byte or halfword images");
   
   status=zvunit(&o_unit,"OUT",1, NULL);
   status=zvopen(o_unit,"U_NL",inl,"U_NS",ins,
	"OP","WRITE","OPEN_ACT","SA","IO_ACT","SA", NULL);
   
   mz_alloc2((unsigned char ***)&imbuf,inl,ins,2);
   mz_alloc1((unsigned char **)&calcbuf,inl,2);
   
   /* read the image into imbuf */
   
   for (i=0;i<inl;i++) status = zvread(i_unit,imbuf[i],"LINE",i+1,
        "SAMP",1,"NSAMPS",ins,"U_FORMAT","HALF", NULL);
   
   if (ioffset==-1)
      {
      for (i=0;i<inl;i++)
         zvwrit(o_unit,imbuf[i],"LINE",i+1,"SAMP",1,"NSAMPS",ins, NULL);
      return;
      }
   
   /* big loop, nbow is number of overlaps */
   
   nbow = (inl+(nscan-ioffset)-1)/nscan;
   for (inah=0;inah<nahp1;inah++)
      {
      for (ibow=0;ibow<nbow;ibow++)
         {
         pointl = ibow*nscan+ioffset-1;
         pointl2 = MAX(0,pointl-nscan);
         vpointl = ptx[(pointl+1)*nahp1+inah];
         pointa = pointl;
         for (i=pointl;i>=pointl2;i--)
            {
            if (ptx[i*nahp1+inah]<vpointl) continue;
            pointa = i;
            break;
            }
         pointu = pointl+1;
         pointu2 = MIN(inl-1,pointu+nscan-1);
         vpointu = ptx[(pointu-1)*nahp1+inah];
         pointb = pointu;
         for (i=pointu;i<=pointu2;i++)
            {
            if (ptx[i*nahp1+inah]>vpointu) continue;
            pointb = i;
            break;
            }
         nint = pointb-pointa;
         if (nint>2)
            {
            delx = ptx[pointb*nahp1+inah]-ptx[pointa*nahp1+inah];
            dely = pty[pointb*nahp1+inah]-pty[pointa*nahp1+inah];
            for (i=1;i<nint;i++)
               {
               frac = (double)i/(double)nint;
               newptx[(pointa+i)*nahp1+inah] = ptx[pointa*nahp1+inah]+frac*delx;
               newpty[(pointa+i)*nahp1+inah] = pty[pointa*nahp1+inah]+frac*dely;
               }
            }
         }
      }
   
   /* output the new columns */
   
   status = IBISColumnSet(ibis,"U_FORMAT","DOUB",cols[2]);
   if (status!=1) IBISSignal(ibis,status,1);
   status = IBISColumnWrite(ibis,(char*) newptx,cols[2],1,clen);
   if (status!=1) IBISSignal(ibis,status,1);
         
   status = IBISColumnSet(ibis,"U_FORMAT","DOUB",cols[3]);
   if (status!=1) IBISSignal(ibis,status,1);
   status = IBISColumnWrite(ibis,(char*) newpty,cols[3],1,clen);
   if (status!=1) IBISSignal(ibis,status,1);
   
   /* interpolate the new values in the image from the moved grid points;
      interpolation is along verticals (one dimensional) so can be done
      in a line buffer then replaced back into the image */
   
   for (jsamp=0;jsamp<ins;jsamp++)
      {
      inah = gridnah-1;
      for (i=0;i<gridnah;i++)
         {
         if (gsamp[i]<((double)jsamp+0.99999)) continue;
         inah = MAX(0,i-1);
         break;
         }
      for (iline=0;iline<inl;iline++)
         {
         inav = gridnav;
         for (i=0;i<gridnav;i++)
            {
            if (gline[i*nahp1]<((double)iline+0.99999)) continue;
            inav = MAX(0,i);
            break;
            }
         /* find the scan, and its lower and upper indices */
         iband = (iline+(nscan-ioffset))/nscan+1;
         ibandl = MAX((iband-1)*nscan+ioffset-nscan,0);
         ibandu = MIN(iband*nscan+ioffset-nscan,inl)-1;
         if (ibandu<=ibandl) continue;
         
         /* find the left,right in newptx,newpty space */
         xlocl = newptx[inav*nahp1+inah];
         ylocl = newpty[inav*nahp1+inah];
         xlocr = newptx[inav*nahp1+inah+1];
         ylocr = newpty[inav*nahp1+inah+1];
         frac = (double)(jsamp+1-gsamp[inav*nahp1+inah])/
            (double)(gsamp[inav*nahp1+inah+1]-gsamp[inav*nahp1+inah]);
         xloc = (1.0-frac)*xlocl+frac*xlocr;
         yloc = (1.0-frac)*ylocl+frac*ylocr;
         
         /* find the above line in ptx,pty space, cannot be last line for interp */
         /*printf("xxx7:nscan,ioffset %d %d\n",nscan,ioffset);
         printf("xxx7:iband,ibandl,ibandu %d %d %d\n",iband,ibandl,ibandu);*/
         iabove = ibandu-1;
         for (i=ibandl;i<ibandu;i++)
            {
            if (above(xloc,yloc,ptx[i*nahp1+inah],pty[i*nahp1+inah],
                      ptx[i*nahp1+inah+1],pty[i*nahp1+inah+1])) continue;
            iabove = MAX(i-1,ibandl);
            break;
            }
         /* get the above and below pixel values */
         vabove = (double)(imbuf[iabove][jsamp]);
         vbelow = (double)(imbuf[iabove+1][jsamp]);
         /* get the fractional spacing and interpolate */
         ibelow = iabove+1;
         dist1 = pdist(xloc,yloc,ptx[iabove*nahp1+inah],pty[iabove*nahp1+inah],
                      ptx[iabove*nahp1+inah+1],pty[iabove*nahp1+inah+1]);
         dist2 = pdist(xloc,yloc,ptx[ibelow*nahp1+inah],pty[ibelow*nahp1+inah],
                      ptx[ibelow*nahp1+inah+1],pty[ibelow*nahp1+inah+1]);
         totdist = dist1+dist2;
         calcbuf[iline] = (short int)((dist2*vabove+dist1*vbelow)/totdist+0.5);
         }
      for (iline=0;iline<inl;iline++) imbuf[iline][jsamp] = calcbuf[iline];
      }
   
   /* write the image */
   
   for (i=0;i<inl;i++)
      zvwrit(o_unit,imbuf[i],"LINE",i+1,"SAMP",1,"NSAMPS",ins, NULL);
   
   status = IBISFileClose(ibis,0);
   zvclose(i_unit, NULL);
   zvclose(o_unit, NULL);
   return;
}
