#include <math.h>

#include "vicmain_c.h"
#include "applic.h"
#include "defines.h"
#include "ibisfile.h"
#include "ibiserrs.h"

#include "cartoMemUtils.h"

#ifndef MAX
#define MAX(a,b)	(((a)>(b))?(a):(b))
#endif

#ifndef MIN
#define MIN(a,b)	(((a)<(b))?(a):(b))
#endif

#define MAXCOMPONENTS       800000

void main44(void)
{
   int i,ix,cols[2],colcount,coldef,backgrnd,ct,iunit;
   int ibis,status,ncol,clen,*indx,i_unit,o_unit;
   int lnl,lns,sl,ss,nl,ns,pixsiz,dummy,dummy2;
   int *lookup,*buffer;
   float *val;
   char outformat[5];
   
   zifmessage("compject version 2015-10-23");
   
   /* initialize, fetch params */
   
   zvparm("cols",cols,&colcount,&coldef,2,0);
   if (colcount!=2) zmabend("requires two columns");
   zvp("backgrnd",&backgrnd,&ct);
   zvparm("FORMAT",outformat,&dummy,&dummy2,1,5);
   
   /* open the ibis file and read the two columns*/
   
   status = zvunit(&iunit,"inp",2, NULL);
   status = IBISFileOpen(iunit,&ibis,"read",0,0,0,0);
   if (status!=1) IBISSignalU(iunit,status,1);
   IBISFileGet(ibis,"nc",&ncol,1,1,0);
   IBISFileGet(ibis,"nr",&clen,1,1,0);
   if (ncol<cols[0]||ncol<cols[1])
       zmabend("column number exceeds last file column");
   mz_alloc1((unsigned char **)&indx,clen,4);
   mz_alloc1((unsigned char **)&val,clen,4);
   
   status = IBISColumnSet(ibis,"U_FORMAT","FULL",cols[0]);
   if (status!=1) IBISSignal(ibis,status,1);
   status = IBISColumnRead(ibis,(char*) indx,cols[0],1,clen);
   if (status!=1) IBISSignal(ibis,status,1);
   
   status = IBISColumnSet(ibis,"U_FORMAT","REAL",cols[1]);
   if (status!=1) IBISSignal(ibis,status,1);
   status = IBISColumnRead(ibis,(char*) val,cols[1],1,clen);
   if (status!=1) IBISSignal(ibis,status,1);
   
   status = IBISFileClose(ibis,0);
   if (status!=1) IBISSignal(ibis,status,1);
   
   /* compute the lookup table */
   
   mz_alloc1((unsigned char **)&lookup,MAXCOMPONENTS,4);
   for (i=0;i<MAXCOMPONENTS;i++) lookup[i] = backgrnd;
   for (i=0;i<clen;i++) lookup[indx[i]] = val[i]+0.5;
   
   /* open the image files */
   
   status = zvunit(&i_unit,"INP",1, NULL);
   status = zvopen(i_unit,"OPEN_ACT","SA","IO_ACT",
      "SA","U_FORMAT","FULL", NULL);
   zvget(i_unit,"NL",&lnl,"NS",&lns,"PIX_SIZE",&pixsiz, NULL);
   
   zvp("SL",&sl,&dummy);
   zvp("SS",&ss,&dummy);
   zvp("NL",&nl,&dummy);
   zvp("NS",&ns,&dummy);
   if (nl<1) nl = lnl;
   if (ns<1) ns = lns;
   if (sl<1) zmabend("parameter sl < 1");
   if (ss<1) zmabend("parameter ss < 1");
   if (nl>lnl) zmabend("parameter nl larger than image");
   if (ns>lns) zmabend("parameter ns larger than image");
   
   status=zvunit(&o_unit,"OUT",1, NULL);
   status=zvopen(o_unit,"U_NL",nl,"U_NS",ns,
        "U_FORMAT","FULL","O_FORMAT",outformat,
	"OP","WRITE","OPEN_ACT","SA","IO_ACT","SA", NULL);
   mz_alloc1((unsigned char **)&buffer,ns,4);
   
   /* read and write, substituting val for index */
   
   for (ix=0;ix<nl;ix++)
      {
      zvread(i_unit,buffer,"LINE",ix+1,"SAMP",1,"NSAMPS",ns, NULL);
      for (i=0;i<ns;i++) buffer[i] = lookup[buffer[i]];
      zvwrit(o_unit,buffer,"LINE",ix+1,"SAMP",1,"NSAMPS",ns, NULL);
      }

   zvclose(i_unit, NULL);
   zvclose(o_unit, NULL);
   
   return;
}
