#include <math.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>

#include "vicmain_c.h"
#include "applic.h"
#include "defines.h"

#include "cartoMemUtils.h"
#include "cartoStrUtils.h"
#include "cartoLsqUtils.h"
#include "cartoGtUtils.h"


/*  image copy   A. Zobrist    11/29/01   */

/*================================================================

gtreplab2

gtreplab2 writes a GeoTIFF label into the property part of VICAR label
NOTE: similar to gtreplab, but unique to gtcopy (extra params)

function return : void

arguments:
      1. fileparm: input, char *fileparm;
         name of the file to put the label, usually "INP"
      2. nfile: input, int nfile;
         which file of fileparm, set to 1 if only one file
      3. labstr: input, char *labstr;
         string containing new GeoTIFF label, comma separated,
         see GTGEN document for format details, and TSTGTGEN.PDF
         for examples
      4. add: input, int add;
         0 - then the old label is deleted and labstr
             becomes the new label
         1 - then the old label is kept and added to,
      5. replflag: input, int replflag;
         0 - no processing of coord-pixel mapping
         1 - coord-pixel mapping replaced w/ next three params
         2 - coord-pixel mapping replaced w/ next three params, but
             MODELPIXELSCALETAG and MODELTRANSFORMATIONTAG type labels
             are swapped due to rotation
      6. tinv: input, double tinv[6];
         will recalculate every MODELTIEPOINTTAG using this transf
      7. scalestr: input, char *scalestr;
         will replace an occurrence of MODELPIXELSCALETAG with this
      8. transstr: input, char *transstr;
         will replace an occurrence of MODELTRANSFORMATIONTAG with this
      9. sl: input, int sl;
         starting line
     10. ss: input, int ss;
         starting sample
      
*/

void gtreplab2(fileparm,nfile,labstr,add,replflag,tinv,scalestr,transstr,sl,ss)
   char *fileparm,*labstr,*scalestr,*transstr;
   int nfile,add,replflag,sl,ss;
   double tinv[6];
{
   int geounit,nelement,maxlen,status,n,ival;
   char key[33],valformat[9],temp1[100],temp2[133],buf[133];
   char *p,*q,*p1,*q1,tbuf[30];
   double dummy,coord1,coord2,pline,psamp;
   
   status=zvunit(&geounit,fileparm,nfile, NULL);
   status=zvopen(geounit,"OP","UPDATE","OPEN_ACT","SA",
	"LAB_ACT","SA", NULL);
   if (!add) do
      {
      /*seems only way to delete properties, note ERR_ACT*/
      status=zlninfo(geounit,key,valformat,&maxlen,
      &nelement,"ERR_ACT"," ", NULL);
      if (status!=1) break;
      if (strcmp(key,"PROPERTY")==0) continue;
      status=zlinfo(geounit,"PROPERTY",key,valformat,
      &maxlen,&nelement,"PROPERTY","GEOTIFF",
      "ERR_ACT"," ", NULL);
      if (status!=1) continue;
      status=zldel(geounit,"PROPERTY",key,
      "ERR_ACT","SA","PROPERTY","GEOTIFF", NULL);
      }
   while (1);
   p = labstr;
   q = p+strlen(labstr);
   do
      {
      n = grab(p,'=',temp1);
      if (n==0) zmabend("syntax error in geotiff parameter");
      p += n;
      n = grab(p,'\n',temp2);
      if (n==0) zmabend("syntax error in geotiff parameter");
      p += n;
      
      if (replflag>0&&strcmp(temp1,"MODELPIXELSCALETAG")==0)
         {
         if (replflag==1) strcpy(temp2,scalestr);
         else
            {
            strcpy(temp1,"MODELTRANSFORMATIONTAG");
            strcpy(temp2,transstr);
            }
         }
      else if (replflag>0&&strcmp(temp1,"MODELTRANSFORMATIONTAG")==0)
         {
         if (replflag==1) strcpy(temp2,transstr);
         else
            {
            strcpy(temp1,"MODELPIXELSCALETAG");
            strcpy(temp2,scalestr);
            }
         }
      else if (replflag>0&&strcmp(temp1,"MODELTIEPOINTTAG")==0)
         {
         p1 = &temp2[1];
         dummy = ms_dnum(&p1); p1++;
         dummy = ms_dnum(&p1); q1 = p1; p1++;
         dummy = ms_dnum(&p1); p1++;
         coord1 = ms_dnum(&p1); p1++;
         coord2 = ms_dnum(&p1);
         psamp = tinv[0]*coord1+tinv[1]*coord2+tinv[2];
         pline = tinv[3]*coord1+tinv[4]*coord2+tinv[5];
         nicelen("(",psamp,buf);
         nicelen(",",pline,tbuf);
         strcat(buf,tbuf);
         strcat(buf,q1);
         strcpy(temp2,buf);
         }
      else if (replflag>0&&strcmp(temp1,"RPC_FIELD4")==0)
         {
         p1 = &temp2[0];
         dummy = ms_dnum(&p1); p1++;
         ival = (int)(dummy+0.5)-sl+1;  /* must replace with rpf solver */
         sprintf(buf,"%d",ival);
         /*while (strlen(buf)<6)
            {
            strcpy(tbuf,"0");
            strcat(tbuf,buf);
            strcpy(buf,tbuf);
            }*/
         strcpy(temp2,buf);
         }
      else if (replflag>0&&strcmp(temp1,"RPC_FIELD5")==0)
         {
         p1 = &temp2[0];
         dummy = ms_dnum(&p1); p1++;
         ival = (int)(dummy+0.5)-ss+1;  /* must replace with rpf solver */
         sprintf(buf,"%d",ival);
         /*while (strlen(buf)<6)
            {
            strcpy(tbuf,"0");
            strcat(tbuf,buf);
            strcpy(buf,tbuf);
            }*/
         strcpy(temp2,buf);
         }
      
      status=zladd(geounit,"PROPERTY",temp1,temp2,
         "ERR_ACT","SA","FORMAT","STRING","MODE","REPLACE",
         "PROPERTY","GEOTIFF", NULL);
      }
   while (p<q);
   zvclose(geounit, NULL);
   return;
}

void main44(void)
{
   int i,iline,i_unit,o_unit,inl,ins,pixsiz,status,dummy;
   int vsize[4],sizepcnt,sizedef;
   int labnl,labns,len,rline,ssimage,ssbuffer;
   int nsread,esimage,nbout;
   char *p,*labelstr1,scalestr[50],*transstr;
   unsigned char *outbuf;
   double t[6],tinv[6],*tout,toutinv[6],corner[4];
   double scale1,scale2,a,b,c,d,bcor,dcor,voff;
   
   transstr = (char*)calloc(133, sizeof(char));
   tout = (double*)calloc(6, sizeof(double));
   zifmessage("gtcopy version 2017-08-03");
   
   /* get the size parms */
   
   zvparm("SIZE",vsize,&sizepcnt,&sizedef,4,0);
   if (vsize[0]==1) zvp("SL",&vsize[0],&dummy);
   if (vsize[1]==1) zvp("SS",&vsize[1],&dummy);
   if (vsize[2]==0) zvp("NL",&vsize[2],&dummy);
   if (vsize[3]==0) zvp("NS",&vsize[3],&dummy);
   
   /* get the geotiff label */
   
   status = gtgetlab("inp",1,&labelstr1,&labnl,&labns);
   if (status!=1) zmabend("Failed to get mapping from input label");
   len = strlen(labelstr1);
   for (i=0;i<len;i++) labelstr1[i] = toupper(labelstr1[i]);
   status = geofix(labelstr1,t,tinv,labnl,labns,corner);
   if (status!=1)
         zmabend("Failed to get mapping from GeoTIFF label, first input");
   
   /* open the input and output files */
   
   status = zvunit(&i_unit,"INP",1, NULL);
   status = zvopen(i_unit,"OPEN_ACT","SA","IO_ACT","SA", NULL);
   zvget(i_unit,"NL",&inl,"NS",&ins,"PIX_SIZE",&pixsiz, NULL);
   
   if (vsize[2]==0) vsize[2] = inl-vsize[0]+1;
   if (vsize[3]==0) vsize[3] = ins-vsize[1]+1;
   ssimage = MAX(1,vsize[1]);
   ssbuffer = MAX(1,2-vsize[1]);
   esimage = MIN(ins,vsize[1]+vsize[3]-1);
   nsread = esimage-ssimage+1;
   nbout = vsize[3]*pixsiz;
   
   status=zvunit(&o_unit,"OUT",1, NULL);
   status=zvopen(o_unit,"U_NL",vsize[2],"U_NS",vsize[3],
	"OP","WRITE","OPEN_ACT","SA","IO_ACT","SA", NULL);
   
   mz_alloc1((unsigned char **)&outbuf,vsize[3]*pixsiz,1);
   
   /* read the lines, take subset or superset, write */
   
   for (iline=0;iline<vsize[2];iline++)
      {
      rline = vsize[0]+iline;
      for (i=0;i<nbout;i++) outbuf[i] = 0;
      if (rline>=1&&rline<=inl)
         status = zvread(i_unit,&outbuf[(ssbuffer-1)*pixsiz],"LINE",rline,
            "SAMP",ssimage,"NSAMPS",nsread,NULL);
      zvwrit(o_unit,outbuf,"LINE",iline+1,"SAMP",1,"NSAMPS",vsize[3], NULL);
      }
   
   /* put the geotiff label, then close the data sets */
   /* calculations from the more general case of gtsize */
   
   zvclose(o_unit, NULL);
   
   a = 1.0;
   c = 1.0;
   b = 0.5+((double)vsize[0]-0.5)*a;
   d = 0.5+((double)vsize[1]-0.5)*c;
   
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
   gtreplab2("OUT",1,labelstr1,0,1,toutinv,scalestr,transstr,vsize[0],vsize[1]);
   
   zvclose(i_unit, NULL);
   free(transstr);
   free(tout);
   return;
}
