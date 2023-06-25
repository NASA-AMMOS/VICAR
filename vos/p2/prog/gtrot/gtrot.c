#include <math.h>
#include <string.h>
#include <ctype.h>

#include "ms_defines.h"
#include "vicmain_c.h"
#include "applic.h"
#include "defines.h"

#include "cartoStrUtils.h"
#include "cartoMemUtils.h"
#include "cartoLsqUtils.h"
#include "cartoGtUtils.h"
#include "cartoTieUtils.h"

/*  GeoTIFF image rotate   A. Zobrist    10/28/99   */


/*********************************************************************
this routine is temporary, byte only until linking problem at JPL
resolved    alz 11/2/99 
12/4/99 resolved... vimake file needs #define LIB_FORTRAN
*********************************************************************/

/*void fakzmve(dcode,n,a,b,ainc,binc)
   int dcode,n,ainc,binc;
   char *a,*b;
{
   int i,j,k;
   
   if (dcode!=1) zmabend("PROGRAM TEMPORARILY BYTE ONLY, TILL MVE FIX");
   for (i=0,j=0,k=0;k<n;i+=ainc,j+=binc,k++) b[j] = a[i];
   
   return;
}*/

   static char msgBuf[20000];

void main44(void)
{
   int i,j,pixsiz,status,i_unit,o_unit,dummy,out_rot;
   int labnl,labns,len,in_rot,wrot=0,wcode=0,outnl,outns;
   int scaletype,ier;
   char *labelstr,**imbuf,*temp,fmt_str[33],scalestr[50];
   char *p,transstr[133];
   double t[6],tinv[6],corner[4],tie[8],map[6],voff;
   double img1[9],img2[9],xmain,xcross,xtot,mapinv[6];
   double ftie[8];
   
   zifmessage("gtrot version 2016-01-08");
   
   /* get the desired rotation from parm */
   
   zvp("ROT",&out_rot,&dummy);
    
   /* get the geotiff rotation */
   
   status = gtgetlab("inp",1,&labelstr,&labnl,&labns);
   if (status!=1) zmabend("Failed to get GeoTIFF label of input");
   len = strlen(labelstr);
   for (i=0;i<len;i++) labelstr[i] = toupper(labelstr[i]);
   status = geofix(labelstr,t,tinv,labnl,labns,corner);
   if (status!=1)
      zmabend("Failed to get mapping from GeoTIFF label of input");
   in_rot = gtgetrot(labelstr);
   
   /* figure the eight cases of transformation from the 64 combinations */
   
   sprintf(msgBuf, "Changing rotation %d to rotation %d",in_rot,out_rot);
   zifmessage(msgBuf);
   if (out_rot==1) zifmessage(" (the standard VICAR rotation)");
   
   if (out_rot<4&&in_rot<4)
      wrot = (out_rot-in_rot+8)%4;
   else if (out_rot>=4&&in_rot>=4)
      wrot = (in_rot-out_rot+8)%4;
   
   else if (in_rot<4&&out_rot>=4)
      wrot = (in_rot-out_rot+8)%4+4;
   else if (in_rot>=4&&out_rot<4)
      wrot = (out_rot-in_rot+8)%4+4;
   
   if (wrot%2==0)
      {
      outnl = labnl;
      outns = labns;
      }
   else
      {
      outnl = labns;
      outns = labnl;
      }
   
   /* This rotates the tiepoints for calculation of a new GeoTIFF label */
   
   if (wrot>=4) flip(tie);
   rot90(tie,wrot%4);
   
   /* read the entire image, an external proc will be used for large images */

   status = zvunit(&i_unit,"INP",1, NULL);
   status = zvopen(i_unit,"OPEN_ACT","SA","IO_ACT","SA", NULL);
   zvget(i_unit,"NL",&labnl,"NS",&labns,"PIX_SIZE",&pixsiz,
          "FORMAT",fmt_str, NULL);
   
   status=zvunit(&o_unit,"OUT",1, NULL);
   status=zvopen(o_unit,"U_NL",outnl,"U_NS",outns,
	"OP","WRITE","OPEN_ACT","SA","IO_ACT","SA", NULL);
   
   if (strcmp(fmt_str,"BYTE")==0) wcode = 1;
   else if (strcmp(fmt_str,"HALF")==0) wcode = 2;
   else if (strcmp(fmt_str,"FULL")==0) wcode = 4;
   else if (strcmp(fmt_str,"REAL")==0) wcode = 7;
   else if (strcmp(fmt_str,"DOUB")==0) wcode = 8;
   else zmabend("This image type not handled by gtrot");
   
   mz_alloc2((unsigned char ***)&imbuf,labnl,labns*pixsiz,1);
   mz_alloc1((unsigned char **)&temp,outns*pixsiz,1);

   for (i=0;i<labnl;i++)
      status = zvread(i_unit,imbuf[i],"LINE",i+1,"SAMP",1,
            "NSAMPS",labns, NULL);
      
   /* now compose output lines according to case */

   for (i=0;i<outnl;i++)
      {
      switch(wrot)
	 {
	 case 0: zmve(wcode,labns,imbuf[i],temp,1,1);
	         break;
	 
	 case 1: for (j=0;j<outns;j++)
	            zmve(wcode,1,&imbuf[j][outnl-i-1],&temp[j],1,1);
		 break;
	 
	 case 2: zmve(wcode,labns,imbuf[outnl-i-1],&temp[outns-pixsiz],1,-1);
	         break;
	 
	 case 3: for (j=0;j<outns;j++)
		    zmve(wcode,1,&imbuf[outns-j-1][i],&temp[j],1,1);
		 break;
	 
	 case 4: zmve(wcode,labns,imbuf[outnl-i-1],temp,1,1);
	         break;
	 
	 case 5: for (j=0;j<outns;j++)
		    zmve(wcode,1,&imbuf[outns-j-1][outnl-i-1],&temp[j],1,1);
		 break;
	 	 
	 case 6: zmve(wcode,labns,imbuf[i],&temp[outns-pixsiz],1,-1);
	         break;
	 
	 case 7: for (j=0;j<outns;j++)
		    zmve(wcode,1,&imbuf[j][i],&temp[j],1,1);
		 break;
	 }
      zvwrit(o_unit,temp,"LINE",i+1,"SAMP",1,"NSAMPS",outns, NULL);
      }
   
   zvclose(i_unit, NULL);
   zvclose(o_unit, NULL);
   
   /* now process the GeoTIFF label for the new rotation */
   /* four tiepoints are the esiest way to get rotated transformation */
   /* use corners in geotiff coord, I wish I know what the 90 degree */
   /* rotate matrix looks like for the (4 x 4) */
   
   p = ms_find(labelstr,"GTRASTERTYPEGEOKEY=2");
   if (p!=0) voff = 1.0; else voff = 0.5;
   ftie[0] = 0.5-voff;
   ftie[1] = 0.5-voff;
   ftie[2] = labns+0.5-voff;
   ftie[3] = 0.5-voff;
   ftie[4] = labns+0.5-voff;
   ftie[5] = labnl+0.5-voff;
   ftie[6] = 0.5-voff;
   ftie[7] = labnl+0.5-voff;
   
   for (i=0;i<8;i+=2) /* have to use vicar with t */
      {
      tie[i] = (ftie[i+1]+voff)*t[0]+(ftie[i]+voff)*t[1]+t[2];
      tie[i+1] = (ftie[i+1]+voff)*t[3]+(ftie[i]+voff)*t[4]+t[5];
      }
   
   if (wrot>=4) flip(tie);
   swp90(ftie,wrot%4);
   rot90(tie,wrot%4);
   
   for (i=0;i<3;i++)
      {
      img1[i] = ftie[2*i];
      img1[i+3] = ftie[2*i+1];
      img1[i+6] = 1.0;
      img2[i] = img1[i];
      img2[i+3] = img1[i+3];
      img2[i+6] = 1.0;
      map[i] = tie[2*i];
      map[i+3] = tie[2*i+1];
      }
   dgauss(img1,map,3,1.e-14,&ier);
   if (ier!=0) zmabend("Program error, tiepoints collinear");
   dgauss(img2,&map[3],3,1.e-14,&ier);
   if (ier!=0) zmabend("Program error, tiepoints collinear");
   
   invertmap(map,mapinv);
   
   xmain = fabs(map[0])+fabs(map[4]);
   xcross = fabs(map[1])+fabs(map[3]);
   xtot = xmain+xcross;
   scaletype = xcross/xtot<1.e-10;
   
   scalefmt(scalestr,map[0],-map[4]);
   trnsfmt(transstr,map);
   
   gtreplab("OUT",1,labelstr,0,1+wrot%2,mapinv,scalestr,transstr);
     
   /* units already closed */
   
   return  ;
}
