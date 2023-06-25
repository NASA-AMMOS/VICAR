#include <math.h>
#include <string.h>
#include <ctype.h>

#include "vicmain_c.h"
#include "applic.h"
#include "defines.h"
#include "ibisfile.h"
#include "ibiserrs.h"
#include "zifmessage.h"
#include "zmabend.h"

#include "cartoStrUtils.h"
#include "cartoMemUtils.h"
#include "cartoLsqUtils.h"
#include "cartoGtUtils.h"

/*  image resize routine   A. Zobrist    9/30/99   */

int dceiling(x) double x;
{
   /* not a true ceiling routine, need numbers near
   floor to go to floor*/
   
   if (x>0) return((int)(x+0.999999));
   else     return((int)(x-0.000001));
}

/*================================================================

int gtmapcom2

special version of mapcom for gtsize.

gtmapcom2 tests whether the two mappings are compatible.  This is
defined as having the same values for a list of attributes that
pertain to mappings.  The list is taken from the GeoTIFF spec
rev 1.0.  If the "value" contains parenthetical material, it is
not required to be the same.  Vector values are required to be
the same.  There is a tolerance of 1 part in 1e-12 on all numeric
values.

If one of the listed attributes is present in one label it must
also be present in the other label.  This prevents default values
from making two labels incompatible, or for alternate keywords
to do the same.

function return:
     int, 1 if mappings are compatible, else 0

arguments:
      1. labelstr1: char *labelstr;
	 (input) string containing the first GeoTIFF label
      1. labelstr2: char *labelstr2;
	 (input) string containing the second GeoTIFF label
*/

int gtmapcom2(labelstr1,labelstr2)
   char *labelstr1,*labelstr2;
{
#define numattrib 37
   int iattrib,status;
   char *p1,*p2;
   char attrib[numattrib][31] = {"GTRASTERTYPEGEOKEY","GTMODELTYPEGEOKEY",
     "GEOGRAPHICTXGEOKEY","GEOGGEODETICDATUMGEOKEY","GEOGPRIMEMERIDIANGEOKEY",
     "GEOGELLIPSOIDGEOKEY","GEOGSEMIMAJORAXISGEOKEY",
     "GEOGSEMIMINORAXISGEOKEY","GEOGINVFLATTENINGGEOKEY",
     "GEOGPRIMEMERIDIANLONGGEOKEY","PROJECTEDCSTYPEGEOKEY","PROJECTIONGEOKEY",
     "PROJCOORDTRANSGEOKEY",
     "PROJSTDPARALLEL1GEOKEY","PROJSTDPARALLEL2GEOKEY","PROJNATORIGINLONGGEOKEY",
     "PROJNATORIGINLATGEOKEY","PROJFALSEEASTINGGEOKEY","PROJFALSENORTHINGGEOKEY",
     "PROJFALSEORIGINLONGGEOKEY","PROJFALSEORIGINLATGEOKEY","PROJFALSEORIGINEASTINGGEOKEY",
     "PROJFALSEORIGINNORTHINGGEOKEY","PROJCENTERLONGGEOKEY","PROJCENTERLATGEOKEY",
     "PROJCENTEREASTINGGEOKEY","PROJCENTERNORTHINGGEOKEY","PROJSCALEATNATORIGINGEOKEY",
     "PROJSCALEATCENTERINGEOKEY","PROJAZIMUTHANGLEGEOKEY","PROJSTRAIGHTVERTPOLELONGGEOKEY",
     "PROJSTDPARALLELGEOKEY","PROJORIGINLONGGEOKEY","PROJORIGINLATGEOKEY",
     "PROJSCALEATORIGINGEOKEY","VERTICALCSTYPEGEOKEY","VERTICALDATUMGEOKEY"};
   /* dropped because made check too fussy, may have to re-add later
   "GEOGLINEARUNITSGEOKEY","GEOGLINEARUNITSIZEGEOKEY","GEOGANGULARUNITSGEOKEY",
   "GEOGANGULARUNITSIZEGEOKEY","GEOGAZIMUTHUNITSGEOKEY",
   "PROJLINEARUNITSGEOKEY","PROJLINEARUNITSIZEGEOKEY","VERTICALUNITSGEOKEY"
   */
     
     
   /*int numattrib = 45;*/
   
   /* loop over attributes in string 1, finding match in string 2,
   the second part of the loop does the reverse check */
   
   for (iattrib=0;iattrib<numattrib;iattrib++)
      {
      p1 = ms_find(labelstr1,attrib[iattrib]);
      if (p1!=0)
         {
         p2 = ms_find(labelstr2,attrib[iattrib]);
         if (p2==0)
            {
            printf("Missing attribute in label 2: %s\n",attrib[iattrib]);
            return 0;
            }
         status = gtcompval(p1,p2);
         if (status!=1)
            {
            printf("Disagreement in labels-1 for: %s\n",attrib[iattrib]);
            return 0;
            }
         }
      p2 = ms_find(labelstr2,attrib[iattrib]);
      if (p2!=0)
         {
         p1 = ms_find(labelstr1,attrib[iattrib]);
         if (p1==0)
            {
            printf("Missing attribute in label 1: %s\n",attrib[iattrib]);
            return 0;
            }
         status = gtcompval(p1,p2);
         if (status!=1)
            {
            printf("Disagreement in labels-2 for: %s\n",attrib[iattrib]);
            return 0;
            }
         }
      }
   
   return 1;
}

void main44(void)
{
   int sline=0, ssamp=0, nline=0, nsamp=0;	/* User specified size of output */
   int inpline=0, inpsamp=0;                  /* size of primary input */

   int interp,inpcnt,gttype=0,itiepar,sizepcnt;
   int itiepcnt,otiepcnt,sizepar,itiedef,otiedef,tsize[4];
   int sizedef,slpar,nlpar,dummy2,imred,redfac=0;
   double itie[4],otie[4],azoom,pzoom,zoom=0;
  
   short int **lines,*outbuf;
   int i,j,i1,j1,wline,cur,prv,halfdat,rot1,rot2;
   int iout,jout,lxl,lxu,lyl,lyu,tmp,gtfirst,len,ignoregt;
   int wsl=0,wss=0,wnl=0,wns=0,wul,wus,labnl,labns;
   int status,dummy,outcount,i_unit,o_unit;
   short int ntmp;
   char *labelstr1,*labelstr2,scalestr[50],transstr[133];
   char *p,fmt_str[10];
   double x,y,xl,xu,yl,yu,a=0,b=0,c=0,d=0,rl,ru,dval,rounder,voff;
   double t[6],r[6],rinv[6],tinv[6],tout[6],toutinv[6];
   double corner[4],rcorner[4],redfacsq;
   double vwsl,vwss,vwul,vwus,bcor,dcor,scale1,scale2,sum;
   
   /* initialize, fetch params */

   zifmessage("GTSIZE version 2019-09-05");
   
   zvparmd("AZOOM",&azoom,&dummy,&dummy2,0,0);
   zvparmd("PZOOM",&pzoom,&dummy,&dummy2,0,0);
   if (azoom<0.0) azoom = -1.0/azoom;
   if (pzoom<0.0) pzoom = -1.0/pzoom;
   imred = 0;
   
   status = gtgetlab("inp",1,&labelstr1,&labnl,&labns);
   gtfirst = status==1;
   ignoregt = zvptst("ignoregt");
   if (ignoregt) gtfirst = 0;
   if (gtfirst)
      {
      len = strlen(labelstr1);
      for (i=0;i<len;i++) labelstr1[i] = toupper(labelstr1[i]);
      status = geofix(labelstr1,t,tinv,labnl,labns,corner);
      if (status!=1)
         zmabend("Failed to get mapping from GeoTIFF label, first input");
      p = ms_find(labelstr1,"GTRASTERTYPEGEOKEY=2");
      if (p!=0&&azoom>0.0) zmabend("Applying area zoom to point type image");
      else if (p==0&&pzoom>0.0) zmabend("Applying point zoom to area type image");
      }
   status = zvpcnt("inp",&inpcnt);
   if (inpcnt>1)
      {
      status = gtgetlab("inp",2,&labelstr2,&labnl,&labns);
      if (status!=1)
         zmabend("Failed to get mapping from GeoTIFF label, second input");
      len = strlen(labelstr2);
      for (i=0;i<len;i++) labelstr2[i] = toupper(labelstr2[i]);
      status = gtrect(labelstr1,(double)1.0e-12);
      if (status!=1)
         zmabend("GTSIZE is restricted to rectangular mappings when 2d input");
      status = gtrect(labelstr2,(double)1.0e-12);
      if (status!=1)
         zmabend("GTSIZE is restricted to rectangular mappings when 2d input");
      status = geofix(labelstr2,r,rinv,labnl,labns,rcorner);
      if (status!=1)
         zmabend("Failed to get mapping from GeoTIFF label, second input");
      status = gtmapcom2(labelstr1,labelstr2);
      if (status!=1) zmabend("Mappings not compatible");
      rot1 = gtgetrot(labelstr1);
      rot2 = gtgetrot(labelstr2);
      if (rot1!=rot2)
         zmabend("Different rotations for two inputs, use GTROTATE");
      }
   
   status = zvunit( &i_unit, "INP", 1, NULL);
   status = zvopen( i_unit, "OPEN_ACT", "SA", "IO_ACT", "SA",
			"U_FORMAT","HALF", NULL);
   zvget(i_unit,"FORMAT",fmt_str, NULL);
   if (strcmp(fmt_str,"BYTE")&&strcmp(fmt_str,"HALF"))
       zmabend("Invalid input data format.  Use BYTE or HALF.");
   halfdat = !strcmp(fmt_str,"HALF");
   
   zvparmd("ITIE",itie,&itiepcnt,&itiedef,4,0);
   if ((!itiedef)&&(itiepcnt!=4)) zmabend("ITIE requires four values");
   if ((!itiedef)&&(inpcnt>1))
      zmabend("Cannot use itie parameters with GeoTIFF reference");
   zvparmd("OTIE",otie,&otiepcnt,&otiedef,4,0);
   if ((!otiedef)&&(otiepcnt!=4)) zmabend("OTIE requires four values");
   if (itiedef!=otiedef)
      zmabend("ITIE and OTIE must both be given, or neither");
   itiepar = !itiedef;
   if (itiepar)
      {
      if (itie[2]<=itie[0]||itie[3]<=itie[1]||
          otie[2]<=otie[0]||otie[3]<=otie[1])
          zmabend("Tiepoint rectangle must subtend positive area");
      }
   
   zvparm("SIZE",tsize,&sizepcnt,&sizedef,4,0);
   sizepar = !sizedef;
   if (sizepar&&(tsize[2]==0||tsize[3]==0))
      zmabend("Cannot set sl or ss without setting nl and ns");
   zvparm("SL",tsize,&sizepcnt,&sizedef,1,0);
   sizepar = sizepar||(!sizedef);
   slpar = !sizedef;
   zvparm("SS",tsize,&sizepcnt,&sizedef,1,0);
   sizepar = sizepar||(!sizedef);
   slpar = slpar||(!sizedef);
   zvparm("NL",tsize,&sizepcnt,&sizedef,1,0);
   sizepar = sizepar||(!sizedef);
   nlpar = !sizedef;
   zvparm("NS",tsize,&sizepcnt,&sizedef,1,0);
   sizepar = sizepar||(!sizedef);
   nlpar = nlpar||(!sizedef);
   if ((sizepar)&&(inpcnt>1))
      zmabend("Cannot use size parameters with GeoTIFF reference");
   if (slpar&&!nlpar)
      zmabend("Cannot set sl or ss without setting nl and ns");
   
   /*zvsize( &sline, &ssamp, &nline, &nsamp, &inpline, &inpsamp);*/
   /*zvsize no good for negative parameters: sline,ssamp*/
   zvget(i_unit,"NL",&inpline,"NS",&inpsamp, NULL);
   zvparm("SIZE",tsize,&sizepcnt,&sizedef,4,0);
   if (!sizedef)
      {
      sline = tsize[0];
      if (sizepcnt>=2) ssamp = tsize[1];
      if (sizepcnt>=3) nline = tsize[2];
      if (sizepcnt>=4) nsamp = tsize[3];
      }
   else
      {
      zvparm("SL",&sline,&sizepcnt,&sizedef,1,0);
      zvparm("SS",&ssamp,&sizepcnt,&sizedef,1,0);
      zvparm("NL",&nline,&sizepcnt,&sizedef,1,0);
      zvparm("NS",&nsamp,&sizepcnt,&sizedef,1,0);
      }
   if (nline==0) nline = inpline;
   if (nsamp==0) nsamp = inpsamp;
  
   if ((!itiepar)&&(sline!=1||ssamp!=1))
      zmabend("SL,SS cannot be used without ITIE,OTIE");
  
   if ((!sizepar)&&itiepar)
      {
      sline = (int)(otie[0]+0.50000001);
      ssamp = (int)(otie[1]+0.50000001);
      nline = (int)(otie[2]-otie[0]+0.49999999);
      nsamp = (int)(otie[3]-otie[1]+0.49999999);
      }
   
   if (zvptst("COVERREF")) gttype = 1;
   else if (zvptst("COVERINP")) gttype = 2;
   /*else gttype = 0;
   if (gttype>0&&inpcnt==1)
   zmabend("gttype parameter valid only if 2d input given");*/
      
   if (zvptst("NOIN")) interp = 2;
   else interp = 1;
   
   /* zoom cases */
   
   if (azoom>0.0||pzoom>0.0)
      {
      if (azoom>0.0&&pzoom>0.0)
         zmabend("Cannot use both azoom and pzoom");
      if (inpcnt==2) zmabend("Cannot use GeoTIFF reference with zoom");
      /*if (gttype>0) zmabend("Cannot use GeoTIFF reference with zoom");*/
      if (itiepar) zmabend("Cannot use itie-otie with zoom");
      if (zvptst("SIZE")) zmabend("Cannot use SIZE with ZOOM");
      if (zvptst("SL")||zvptst("SS")||zvptst("NL")||zvptst("NS"))
         zmabend("Cannot use SIZE with zoom");
      itiepar = 1;
      sline = 1;
      ssamp = 1;
      if (azoom>0.0)
         {
         zoom = azoom;
         if (zoom<0.51)
            {
            imred = 1;
            redfac = (int)(1.0/zoom+0.000001);
            if (fabs(1.0-redfac*zoom)>0.000001)
               zmabend("Zooms smaller than 0.5 must be integral for now");
            }
         if (imred)
            {
            itie[0] = (redfac+1)*0.5;
            itie[1] = (redfac+1)*0.5;
            nline = (int)(inpline*zoom+0.0001);
            nsamp = (int)(inpsamp*zoom+0.0001);
            otie[0] = 1.0;
            otie[1] = 1.0;
            }
         else
            {
            itie[0] = 0.5;
            itie[1] = 0.5;
            nline = (int)(inpline*zoom+0.0001);
            nsamp = (int)(inpsamp*zoom+0.0001);
            otie[0] = 0.5;
            otie[1] = 0.5;
            }
         }
      if (pzoom>0.0)
         {
         zoom = pzoom;
         if (zoom<0.51)
            {
            imred = 1;
            redfac = (int)(1.0/zoom+0.000001);
            if (fabs(1.0-redfac*zoom)>0.000001)
               zmabend("Zooms smaller than 0.5 must be integral for now");
            }
         if (imred)
            {
            itie[0] = (redfac+1)*0.5;
            itie[1] = (redfac+1)*0.5;
            nline = (int)(inpline*zoom+0.0001);
            nsamp = (int)(inpsamp*zoom+0.0001);
            }
         else
            {
            itie[0] = 1.0;
            itie[1] = 1.0;
            nline = (int)((inpline-1)*zoom+1.0001);
            nsamp = (int)((inpsamp-1)*zoom+1.0001);
            }
         otie[0] = 1.0;
         otie[1] = 1.0;
         }
      itie[2] = itie[0]+100.0;
      itie[3] = itie[1]+100.0;
      otie[2] = 100.0*zoom+otie[0];
      otie[3] = 100.0*zoom+otie[1];
      }
   
   /* process scale factors */
   
   if (inpcnt==1&&itiepar)
      {
      a = (itie[2]-itie[0])/(otie[2]-otie[0]);
      c = (itie[3]-itie[1])/(otie[3]-otie[1]);
      b = itie[0]+((double)sline-otie[0])*a;
      d = itie[1]+((double)ssamp-otie[1])*c;
      /* need the actual sized area in output, rest is gores, vwsl
         is vicar pixel space, wsl is 0 based (C indexing) */
      vwsl = (0.5-itie[0])/a+otie[0]-(double)(sline-1);
      wsl = MAX(1,(int)dceiling(vwsl))-1;
      vwss = (0.5-itie[1])/c+otie[1]-(double)(ssamp-1);
      wss = MAX(1,(int)dceiling(vwss))-1;
      vwul = ((double)inpline+0.5-itie[0])/a+otie[0]-(double)(sline-1);
      wul = MIN(nline,(int)vwul);
      wnl = wul-wsl;
      vwus = ((double)inpsamp+0.5-itie[1])/c+otie[1]-(double)(ssamp-1);
      wus = MIN(nsamp,(int)vwus);
      wns = wus-wss;
      }
   if (inpcnt==1&&!itiepar)
      {
      a = (double)inpline/(double)nline;
      c = (double)inpsamp/(double)nsamp;
      b = 0.5+((double)sline-0.5)*a;
      d = 0.5+((double)ssamp-0.5)*c;
      wsl = 0; wss = 0; wnl = nline; wns = nsamp;
      }
   
   if (inpcnt!=1)
      {
      vwsl = rinv[0]*corner[0]+rinv[1]*corner[1]+rinv[2];
      vwss = rinv[3]*corner[0]+rinv[4]*corner[1]+rinv[5];
      vwul = rinv[0]*corner[2]+rinv[1]*corner[3]+rinv[2];
      vwus = rinv[3]*corner[2]+rinv[4]*corner[3]+rinv[5];
      if (gttype==1)    /* this is coverref */
	 {
	 sline = 1;
         ssamp = 1;
         nline = labnl;
         nsamp = labns;
         wsl = MAX(1,(int)dceiling(vwsl))-1;
         wss = MAX(1,(int)dceiling(vwss))-1;
         wul = MIN(nline,(int)vwul);
         wus = MIN(nsamp,(int)vwus);
         wnl = wul-wsl;
         wns = wus-wss;
         }
      else             /* this is coverinp */
	 {
	 wsl = (int)dceiling(vwsl)-1;
         wss = (int)dceiling(vwss)-1;
         wul = (int)vwul;
         wus = (int)vwus;
         wnl = wul-wsl;
         wns = wus-wss;
         vwsl -= (double)wsl;
	 vwss -= (double)wss;
	 wsl = 0; wss = 0;
	 sline = 1; ssamp = 1;
         nline = wnl; nsamp = wns;
	 }
      a = (tinv[0]+tinv[1])/(rinv[0]+rinv[1]);
      c = (tinv[3]+tinv[4])/(rinv[3]+rinv[4]);
      b = 0.5+(1.0-vwsl)*a;
      d = 0.5+(1.0-vwss)*c;
      }
   
   status=zvunit( &o_unit, "OUT", 1, NULL);
   status=zvopen( o_unit, "U_NL", nline, "U_NS", nsamp, "U_FORMAT","HALF",
	"OP", "WRITE","OPEN_ACT", "SA",	"IO_ACT", "SA", NULL);
   
   /* first, dispense with the image reducing case */
   
   if (imred&&interp!=2)
      {
      mz_alloc2((unsigned char ***)&lines,redfac,inpsamp,2);
      mz_alloc1((unsigned char **)&outbuf,nsamp,2);
      redfacsq = 1.0/(redfac*redfac);
      outcount = 1;
   
      for (iout=0;iout<nline;iout++)
         {
         for (j=0;j<redfac;j++)
            {
            status = zvread(i_unit,lines[j],"LINE", iout*redfac+j+1,
  		"SAMP", 1, "NSAMPS", inpsamp, NULL);
            }
         for (j=0;j<nsamp;j++)
            {
            sum = 0.0;
            for (i1=0;i1<redfac;i1++)
               for (j1=0;j1<redfac;j1++)
                  sum += lines[i1][j*redfac+j1];
            outbuf[j] = (short int)(sum*redfacsq+0.5);
            }
         zvwrit(o_unit,outbuf,"LINE",outcount++,
               "SAMP",1,"NSAMPS", nsamp, NULL);
         }
      goto skipbilin;
      }
   
   /* now the other cases that use bilinear interpolation or nearest nbr */
   
   mz_alloc2((unsigned char ***)&lines,2,inpsamp,2);
   mz_alloc1((unsigned char **)&outbuf,nsamp,2);
   for (i=0;i<nsamp;i++) outbuf[i] = 0;
   outcount = 1;
   
   /* iterate over the output pixels calculating where the
      input center point comes from.  the input lines are
      read in on a demand basis and are stored alternately
      according to the cursors cur and prv.  rounder is
      needed for consistency when using different sl,ss
      and expecting the same pixel values (case where c is
      .3333333333) */
   
   cur = 1; prv = 0; wline = -2;
   rounder = 0.5+9.0e-14;
   for (iout=0;iout<wsl;iout++)
      zvwrit(o_unit,outbuf,"LINE",outcount++,"SAMP",1,"NSAMPS", nsamp, NULL);
   for (iout=wsl;iout<wsl+wnl;iout++)
      {
      y = ((double)iout)*a+b;
      lyl = (int)y-1;
      if (lyl<0) lyl = 0;
      if (lyl>inpline-2) lyl = inpline-2;
      lyu = lyl+1;
      yl = y-(double)lyl-1.0;
      yu = 1.-yl;
      if (lyl>wline)
	 {
	 status = zvread(i_unit,lines[prv],"LINE", lyl+1,
  		"SAMP", 1, "NSAMPS", inpsamp, NULL);
	 status = zvread(i_unit,lines[cur],"LINE", lyu+1,
  		"SAMP", 1, "NSAMPS", inpsamp, NULL);
	 wline = lyu;
	 }
      if (lyu>wline)
	 {
	 tmp = cur; cur = prv; prv = tmp;
	 status = zvread(i_unit,lines[cur],"LINE", lyu+1,
  		"SAMP", 1, "NSAMPS", inpsamp, NULL);
	 wline = lyu;
	 }
      for (jout=wss;jout<wss+wns;jout++)
	 {
	 x = ((double)jout)*c+d;
	 lxl = (int)x-1;
	 if (lxl<0) lxl = 0;
	 if (lxl>inpsamp-2) lxl = inpsamp-2;
	 lxu = lxl+1;
	 xl = x-(double)lxl-1.0;
	 xu = 1.-xl;
	 switch (interp)
	    {
case 1:     rl = xu*lines[prv][lxl]+xl*lines[prv][lxu];
	    ru = xu*lines[cur][lxl]+xl*lines[cur][lxu];
	    dval = yu*rl+yl*ru;
	    if (dval>=0.0) outbuf[jout] = (short int)(dval+rounder);
	    else
	       {
	       if (halfdat) outbuf[jout] = (short int)(dval-rounder);
	       else outbuf[jout] = 0;
	       }
	    break;
case 2:     if (xu<.5)
	       {
	       if (yu<.5) ntmp = lines[cur][lxu];
		     else ntmp = lines[prv][lxu];
	       }
	    else
	       {
	       if (yu<.5) ntmp = lines[cur][lxl];
		     else ntmp = lines[prv][lxl];
	       }
	    outbuf[jout] = ntmp;
	    break;
	    }
	 }
      zvwrit(o_unit,outbuf,"LINE",outcount++,
               "SAMP",1,"NSAMPS", nsamp, NULL);
      }
   for (i=0;i<nsamp;i++) outbuf[i] = 0;
   for (iout=wsl+wnl;iout<nline;iout++)
      status = zvwrit(o_unit,outbuf,"LINE",outcount++,
               "SAMP",1,"NSAMPS", nsamp, NULL);
   
   /* update the geotiff label, gtreplab reopens for update */
   /* the tout[] solutions are in GeoTIFF coordinates, I think
   that this will be the only program with this feature. */
   
   skipbilin:
   zvclose(o_unit, NULL);
   if (gtfirst)
      {
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
   
   zvclose(i_unit, NULL);
   return;
}   
