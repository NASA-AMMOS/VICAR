#include <math.h>
#include <stdio.h>

#include "vicmain_c.h"
#include "applic.h"
#include "defines.h"
#include "ibisfile.h"
#include "ibiserrs.h"
#include "taeconf.inp"
#include "parblk.inc"
#include "defines.h"

#include "cartoTaeUtils.h"
#include "cartoMemUtils.h"
#include "cartoGridUtils.h"

/************************************************************************/
/* program gridck                                                       */
/************************************************************************/
/*  03-03 ...alz... initial version                                     */
/************************************************************************/

void main44(void)
{
   double *ptx,*pty;
   int i,unit,colcount,coldef,ibis,clen,status,ntiepp;
   int null9,gridnah,gridnav,k,pcount,pdef,cols[4],ncol;
        
   zifmessage("gridck version 2016-08-12");
   
   zvparmd("gridtol",&gridtol,&pcount,&pdef,1,0);
   zvparm("cols",cols,&colcount,&coldef,4,0);
   
   /*read in tiepoints from the ibis interface file, by columns */

   status = zvunit(&unit,"inp",1, NULL);
   status = IBISFileOpen(unit,&ibis,"read",0,0,0,0);
   if (status!=1) IBISSignalU(unit,status,1);
   IBISFileGet(ibis,"nr",&clen,1,1,0);
   IBISFileGet(ibis,"nc",&ncol,1,1,0);
   if (ncol<2) zmabend("need at least two columns");
   ncol = 2;
   mz_alloc1((unsigned char **)&ptx,clen,8);
   mz_alloc1((unsigned char **)&pty,clen,8);
   
   status = IBISColumnSet(ibis,"U_FORMAT","DOUB",cols[0]);
   if (status!=1) IBISSignal(ibis,status,1);
   status = IBISColumnRead(ibis,(char*)ptx,cols[0],1,clen);
   if (status!=1) IBISSignal(ibis,status,1);
   
   status = IBISColumnSet(ibis,"U_FORMAT","DOUB",cols[1]);
   if (status!=1) IBISSignal(ibis,status,1);
   status = IBISColumnRead(ibis,(char*)pty,cols[1],1,clen);
   if (status!=1) IBISSignal(ibis,status,1);
   
   status = IBISFileClose(ibis,0);
   if (status!=1) IBISSignal(ibis,status,1);
   ntiepp = clen;
   
   /* detect grid here and set gridnah */
   
   cartogetline(ptx,pty,0,1,ntiepp,ntiepp,&gridnah,&null9);
   if (null9==1) goto notagrid;
   if (gridnah==0) goto notagrid;
   if (ntiepp%(gridnah+1)!=0) goto notagrid;
   gridnav = ntiepp/(gridnah+1)-1;
   if (ntiepp%(gridnav+1)!=0) goto notagrid;
   if ((double)gridnah<(gridtol-2.0)) goto notagrid;
   if ((double)gridnav<(gridtol-2.0)) goto notagrid;
   for (i=1;i<=gridnav;i++)
      {
      cartogetline(ptx,pty,i*(gridnah+1),1,gridnah+1,ntiepp,&k,&null9);
      if (null9==1) goto notagrid;
      if (k!=gridnah) goto notagrid;
      }
   for (i=0;i<=gridnah;i++)
      {
      cartogetline(ptx,pty,i,gridnah+1,gridnav+1,ntiepp,&k,&null9);
      if (null9==1) goto notagrid;
      if (k!=gridnav) goto notagrid;
      }
   
   mq_out_int("valnah",gridnah);
   mq_out_int("valnav",gridnav);
   /*   printf(" grid OK: nah = %d, nav = %d\n",gridnah,gridnav);*/
   return;
   
   notagrid:
   mq_out_int("valnah",0);
   mq_out_int("valnav",0);
   printf(" not a grid\n");
   return;
   
   
   }
