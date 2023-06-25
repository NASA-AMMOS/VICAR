#include <math.h>
#include <string.h>
#include <ctype.h>

#include "vicmain_c.h"
#include "applic.h"
#include "defines.h"
#include "ibisfile.h"
#include "ibiserrs.h"

#include "cartoStrUtils.h"
#include "cartoMemUtils.h"
#include "cartoGtUtils.h"
#include "cartoRpcUtils.h"

/************************************************************************/
/* program rpcscale                                                      */
/************************************************************************/
/*  99-08 ...alz... initial version                     */
/************************************************************************/

void main44(void)
{
   int i,j,cols[5],unit,colcount,coldef,rpccol,idef,icnt;
   int ibis,status,clen,labnl,labns,scale;
   char *labelstr;
   double *lon,*lat,*elev,*line,*samp;
   double rpck[13];
   
   zifmessage("rpcscale version Thu Jan  3 2008");
   
   /* get the basic parameters */
   
   zvparm("cols",cols,&colcount,&coldef,5,0);
   if (colcount!=5) zmabend("Requires five columns");
   scale = !zvptst("unscale");
   zvparm("rpccol",&rpccol,&icnt,&idef,1,0);
   
   /* read the rpc's (scale/offset only) from the first input */
   
   if (rpccol==0)
      {
      status = gtgetlab("inp",1,&labelstr,&labnl,&labns);
      for (j=0;j<13;j++) rpcrd(0,j,labelstr,rpck);
      }
   else
      {
      status = zvunit(&unit,"inp",1, NULL);
      status = IBISFileOpen(unit,&ibis,"read",0,0,0,0);
      if (status!=1) IBISSignalU(unit,status,1);
      status = IBISColumnSet(ibis,"U_FORMAT","DOUB",rpccol);
      if (status!=1) IBISSignal(ibis,status,1);
      status = IBISColumnRead(ibis,(char*)rpck,rpccol,3,13);
      if (status!=1) IBISSignal(ibis,status,1);
      status = IBISFileClose(ibis,0);
      if (status!=1) IBISSignal(ibis,status,1);
      }
   
   /* read in points from the ibis interface file (file 2) */

   status = zvunit(&unit,"inp",2, NULL);
   status = IBISFileOpen(unit,&ibis,"update",0,0,0,0);
   if (status!=1) IBISSignalU(unit,status,1);
   IBISFileGet(ibis,"nr",&clen,1,1,0);
   mz_alloc1((unsigned char **)&lon,clen,8);
   mz_alloc1((unsigned char **)&lat,clen,8);
   mz_alloc1((unsigned char **)&elev,clen,8);
   mz_alloc1((unsigned char **)&line,clen,8);
   mz_alloc1((unsigned char **)&samp,clen,8);
   
   for (i=0;i<5;i++)
      {
      status = IBISColumnSet(ibis,"U_FORMAT","DOUB",cols[i]);
      if (status!=1) IBISSignal(ibis,status,1);
      }
   
   status = IBISColumnRead(ibis,(char*)lon,cols[0],1,clen);
   if (status!=1) IBISSignal(ibis,status,1);
   status = IBISColumnRead(ibis,(char*)lat,cols[1],1,clen);
   if (status!=1) IBISSignal(ibis,status,1);
   status = IBISColumnRead(ibis,(char*)elev,cols[2],1,clen);
   if (status!=1) IBISSignal(ibis,status,1);
   status = IBISColumnRead(ibis,(char*)line,cols[3],1,clen);
   if (status!=1) IBISSignal(ibis,status,1);
   status = IBISColumnRead(ibis,(char*)samp,cols[4],1,clen);
   if (status!=1) IBISSignal(ibis,status,1);
   
   /* calculate the output data, line-samp in VICAR coord, rpc in area coord */
   /* note the 0.5 offset for VICAR to GeoTIFF area type */
   
   if (scale) for (i=0;i<clen;i++)
      {
      lon[i] = (lon[i]-rpck[6])/rpck[11];
      lat[i] = (lat[i]-rpck[5])/rpck[10];
      elev[i] = (elev[i]-rpck[7])/rpck[12];
      line[i] = (line[i]-rpck[3]-0.5)/rpck[8];
      samp[i] = (samp[i]-rpck[4]-0.5)/rpck[9];
      }
   else for (i=0;i<clen;i++)
      {
      lon[i] = lon[i]*rpck[11]+rpck[6];
      lat[i] = lat[i]*rpck[10]+rpck[5];
      elev[i] = elev[i]*rpck[12]+rpck[7];
      line[i] = line[i]*rpck[8]+rpck[3]+0.5;
      samp[i] = samp[i]*rpck[9]+rpck[4]+0.5;
      }
   
   /* Output points to the ibis interface file */
   
   status = IBISColumnWrite(ibis,(char*)lon,cols[0],1,clen);
   if (status!=1) IBISSignal(ibis,status,1);
   status = IBISColumnWrite(ibis,(char*)lat,cols[1],1,clen);
   if (status!=1) IBISSignal(ibis,status,1);
   status = IBISColumnWrite(ibis,(char*)elev,cols[2],1,clen);
   if (status!=1) IBISSignal(ibis,status,1);
   status = IBISColumnWrite(ibis,(char*)line,cols[3],1,clen);
   if (status!=1) IBISSignal(ibis,status,1);
   status = IBISColumnWrite(ibis,(char*)samp,cols[4],1,clen);
   if (status!=1) IBISSignal(ibis,status,1);
   
   status = IBISFileClose(ibis,0);
   if (status!=1) IBISSignal(ibis,status,1);
  
   return;
}
