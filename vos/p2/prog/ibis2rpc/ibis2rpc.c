#include <math.h>
#include <string.h>

#include "vicmain_c.h"
#include "applic.h"
#include "ibisfile.h"
#include "defines.h"
#include "zifmessage.h"
#include "zmabend.h"

#include "cartoMemUtils.h"
#include "cartoStrUtils.h"
#include "cartoGtUtils.h"

/*  IBIS formatted rpc into image label   A. Zobrist    11/4/05   */

void main44(void)
{
   int i,i_unit,o_unit,status,clen,ccol,scol,dummy,ibis,nlab,nix;
   int nelement,maxlen;
   short int imbuf[1];
   double coeff[80],scale[20],map[6];
   char *labelstr,buf[50],key[33],valformat[9];
   
   zifmessage("IBIS2RPC version 2019-07-31");
   
   /* initialize, fetch params */

   status = zvunit(&i_unit,"inp",1, NULL);
   status = IBISFileOpen(i_unit,&ibis,"read",0,0,0,0);
   if (status!=1) IBISSignalU(i_unit,status,1);
   IBISFileGet(ibis,"nr",&clen,1,1,0);
   if (clen!=80) zmabend("column length must be 80 for rpc ibis file");
   
   status = zvp("ccol",&ccol,&dummy);
   status = zvp("scol",&scol,&dummy);
   
   mz_alloc1((unsigned char **)&labelstr,5000,1);
   strcpy(labelstr,"");
   
   /* read the ibis columns */
   
   status = IBISColumnSet(ibis,"U_FORMAT","DOUB",scol);
   if (status!=1) IBISSignal(ibis,status,1);
   status = IBISColumnRead(ibis,(char*)scale,scol,1,15);
   if (status!=1) IBISSignal(ibis,status,1);
   
   status = IBISColumnSet(ibis,"U_FORMAT","DOUB",ccol);
   if (status!=1) IBISSignal(ibis,status,1);
   status = IBISColumnRead(ibis,(char*)coeff,ccol,1,clen);
   if (status!=1) IBISSignal(ibis,status,1);
   
   /* logic for linefeeds is:  before any item except first  */
   
   if (scale[0]<0.5)
     snprintf(buf, 50, "NITF_CETAG=RPC00A");
   else
     snprintf(buf, 50, "NITF_CETAG=RPC00B");

   strcat(labelstr,buf);
   
   for (i=0;i<13;i++)
      {
      strcat(labelstr,"\n");
      if (i==0)
	snprintf(buf, 50, "RPC_FIELD%d=%1.0f",i+1,scale[i+2]);
      else
	snprintf(buf, 50, "RPC_FIELD%d=%7.6f",i+1,scale[i+2]);
      strcat(labelstr,buf);
      }
   
   for (i=0;i<80;i++)
      {
      nlab = 14+i/20;
      nix = i%20+1;
      strcat(labelstr,"\n");
      if (coeff[i]>=0)
	snprintf(buf, 50, "RPC_FIELD%d%d=+%16.14f",nlab,nix,coeff[i]);
      else
	snprintf(buf, 50, "RPC_FIELD%d%d=%17.14f",nlab,nix,coeff[i]);
      strcat(labelstr,buf);
      }
   
   /* process the output, creating a new 1 x 1 */
   
   status = zvunit(&o_unit,"OUT",1, NULL);
   status=zvopen(o_unit,"U_NL",1,"U_NS",1,"OP","WRITE",
	"OPEN_ACT","SA","IO_ACT","SA","TYPE","IMAGE", NULL);
   
   status=zldel(o_unit,"PROPERTY","NR","ERR_ACT","SA","PROPERTY","IBIS", NULL);
   status=zldel(o_unit,"PROPERTY","NC","ERR_ACT","SA","PROPERTY","IBIS", NULL);
   
   do {
      /*seems only way to delete properties, note ERR_ACT*/
      status=zlninfo(o_unit,key,valformat,&maxlen,
         &nelement,"ERR_ACT"," ", NULL);
      if (status!=1) break;
      if (strcmp(key,"PROPERTY")==0) continue;
      status=zlinfo(o_unit,"PROPERTY",key,valformat,
         &maxlen,&nelement,"PROPERTY","IBIS",
         "ERR_ACT"," ", NULL);
      if (status!=1) continue;
      if (strcmp(key,"PROPERTY")==0) continue;
      status=zldel(o_unit,"PROPERTY",key,
         "ERR_ACT","SA","PROPERTY","IBIS", NULL);
      }
   while (1);
   
   imbuf[0] = 0;
   status = zvwrit(o_unit,imbuf,"LINE",1,"SAMP",1,"NSAMPS",1, NULL);
      
   /* now put labelstr in the state label under GeoTIFF, map param is a
      dummy here, is not used */
   
   zvclose(o_unit, NULL);
   gtreplab("OUT",1,labelstr,0,0,map,"","");
     
   return;
   
}
