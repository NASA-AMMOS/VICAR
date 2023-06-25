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
#include "cartoGtUtils.h"

/************************************************************************/
/* program rpc2ibis                                                      */
/************************************************************************/
/*  05-04 ...alz... initial version                     */
/************************************************************************/

void rpcrd(i,j,labelstr,val)
   int i,j;
   char *labelstr;
   double *val;
{
   char *p,rpcfield[15],numstr[5];
   
   strcpy(rpcfield,"RPC_FIELD");
   if (i>0)
      {
	snprintf(numstr,5,"%d",i);
	strcat(rpcfield,numstr);
      }
   snprintf(numstr,5,"%d=",j+1);
   strcat(rpcfield,numstr);
   p = ms_find(labelstr,rpcfield);
   val[j] = ms_dnum(&p);
   /*printf("rpcfield .%s. val[j] %25.18f\n",rpcfield,val[j]);*/

   return;
}

#if 0
double rpceval(isline,lon,lat,elv,rpck,rpcn,rpcd,rpctype)
   int isline,rpctype;
   double lon,lat,elv,rpck[13],rpcn[20],rpcd[20];
{
   double l,p,h,l2,l3,p2,p3,h2,h3,numer,denom;
   
   l = (lon-rpck[6])/rpck[11];
   p = (lat-rpck[5])/rpck[10];
   h = (elv-rpck[7])/rpck[12];
   l2 = l*l; l3 = l2*l;
   p2 = p*p; p3 = p2*p;
   h2 = h*h; h3 = h2*h;
   
   if (rpctype==0)       /* type RPC00A */
      {
      numer = rpcn[0]+rpcn[1]*l+rpcn[2]*p+rpcn[3]*h+
      rpcn[4]*l*p+rpcn[5]*l*h+rpcn[6]*p*h+rpcn[7]*p*l*h+
      rpcn[8]*l2+rpcn[9]*p2+rpcn[10]*h2+rpcn[11]*l3+
      rpcn[12]*l2*p+rpcn[13]*l2*h+rpcn[14]*l*p2+
      rpcn[15]*p3+rpcn[16]*p2*h+rpcn[17]*l*h2+
      rpcn[18]*p*h2+rpcn[19]*h3;
      denom = rpcd[0]+rpcd[1]*l+rpcd[2]*p+rpcd[3]*h+
      rpcd[4]*l*p+rpcd[5]*l*h+rpcd[6]*p*h+rpcd[7]*p*l*h+
      rpcd[8]*l2+rpcd[9]*p2+rpcd[10]*h2+rpcd[11]*l3+
      rpcd[12]*l2*p+rpcd[13]*l2*h+rpcd[14]*l*p2+
      rpcd[15]*p3+rpcd[16]*p2*h+rpcd[17]*l*h2+
      rpcd[18]*p*h2+rpcd[19]*h3;
      }
   else                   /* type RPC00B */
      {
      numer = rpcn[0]+rpcn[1]*l+rpcn[2]*p+rpcn[3]*h+
      rpcn[4]*l*p+rpcn[5]*l*h+rpcn[6]*p*h+rpcn[7]*l2+
      rpcn[8]*p2+rpcn[9]*h2+rpcn[10]*p*l*h+rpcn[11]*l3+
      rpcn[12]*l*p2+rpcn[13]*l*h2+rpcn[14]*l2*p+
      rpcn[15]*p3+rpcn[16]*p*h2+rpcn[17]*l2*h+
      rpcn[18]*p2*h+rpcn[19]*h3;
      denom = rpcd[0]+rpcd[1]*l+rpcd[2]*p+rpcd[3]*h+
      rpcd[4]*l*p+rpcd[5]*l*h+rpcd[6]*p*h+rpcd[7]*l2+
      rpcd[8]*p2+rpcd[9]*h2+rpcd[10]*p*l*h+rpcd[11]*l3+
      rpcd[12]*l*p2+rpcd[13]*l*h2+rpcd[14]*l2*p+
      rpcd[15]*p3+rpcd[16]*p*h2+rpcd[17]*l2*h+
      rpcd[18]*p2*h+rpcd[19]*h3;
      }

   
   if (isline) return(rpck[8]*numer/denom+rpck[3]);
      else     return(rpck[9]*numer/denom+rpck[4]);
}
#endif

void main44(void)
{
  int i,j,cols[5],unit,colcount,coldef;
   int ibis,status,clen,labnl,labns,len;
   char *labelstr,*p;
   double coeff[80],ancillary[15];
   
   zifmessage("RPC2IBIS version 2019-08-06");
   
   /* get the basic parameters */
   
   zvparm("cols",cols,&colcount,&coldef,2,0);
   if (colcount!=2) zmabend("Requires two columns");
   
   /* read the rpc's from the first input */
   
   status = gtgetlab("inp",1,&labelstr,&labnl,&labns);
   len = strlen(labelstr);
   for (i=0;i<len;i++) labelstr[i] = toupper(labelstr[i]);
   for (j=0;j<13;j++) rpcrd(0,j,labelstr,&ancillary[2]);
   for (j=0;j<20;j++) rpcrd(14,j,labelstr,&coeff[0]);
   for (j=0;j<20;j++) rpcrd(15,j,labelstr,&coeff[20]);
   for (j=0;j<20;j++) rpcrd(16,j,labelstr,&coeff[40]);
   for (j=0;j<20;j++) rpcrd(17,j,labelstr,&coeff[60]);
   
   p = ms_find(labelstr,"NITF_CETAG=");
   if (p!=0)
      {
      if (strncmp(p,"RPC00A",6)==0)
         {
         ancillary[0] = 0.0;
         ancillary[1] = 1041.0;
         zifmessage("processing RPC00A");
         }
      if (strncmp(p,"RPC00B",6)==0)
         {
         ancillary[0] = 1.0;
         ancillary[1] = 1041.0;
         zifmessage("processing RPC00B");
         }
      }
   else
      {
	zifmessage("processing RPC00A by default");
      }
      
   /* Output points to the ibis interface file */
  
   status = zvunit(&unit,"inp",2, NULL);
   status = IBISFileOpen(unit,&ibis,"update",0,0,0,0);
   if (status!=1) IBISSignalU(unit,status,1);
   IBISFileGet(ibis,"nr",&clen,1,1,0); 
   if (clen!=80) zmabend("Requires 80 record file");
   status = IBISColumnWrite(ibis,(char*)coeff,cols[0],1,clen);
   if (status!=1) IBISSignal(ibis,status,1);
   status = IBISColumnWrite(ibis,(char*)ancillary,cols[1],1,15);
   if (status!=1) IBISSignal(ibis,status,1);
   
   status = IBISFileClose(ibis,0);
   if (status!=1) IBISSignal(ibis,status,1);
  
   return;
}
