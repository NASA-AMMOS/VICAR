#include <math.h>
#include <stdio.h>

#include "vicmain_c.h"
#include "applic.h"
#include "defines.h"
#include "ibisfile.h"
#include "ibiserrs.h"
#include "taeconf.inp"
#include "parblk.inc"

#include "cartoTaeUtils.h"
#include "cartoMemUtils.h"
#include "cartoMatUtils.h"
//#include "cartoVicarProtos.h"

/************************************************************************/
/* program grid3pt                                                       */
/************************************************************************/
/*  03-03 ...alz... initial version                                     */
/*  Sat Dec 29 2007 wlb switched to USES_ANSI_C AND LIB_CARTO; misc cleanup */
/************************************************************************/

double gridtol;

void main44(void)
{
  double *ptx,*pty,*ptlon,*ptlat;
  int i,unit,colcount,coldef,ibis,clen,status,ntiepp;
  int cols[4],ncol,isave1=0;
  int isave2=0,isave3=0;
  double xctr,yctr,x1,y1,x2,y2,x3,y3,dx,dy,dist,tdist;
   
  zifmessage("grid3pt version Thu Mar 05 2008");
   
  zvparm("cols",cols,&colcount,&coldef,4,0);
   
  /*read in tiepoints from the ibis interface file, by columns */

  status = zvunit(&unit,"inp",1, NULL);
  status = IBISFileOpen(unit,&ibis,"read",0,0,0,0);
  if (status!=1) IBISSignalU(unit,status,1);
  IBISFileGet(ibis,"nr",&clen,1,1,0);
  IBISFileGet(ibis,"nc",&ncol,1,1,0);
  if (ncol<4) zmabend("need at least four columns");
  ncol = 4;
  mz_alloc1((unsigned char **)&ptx,clen,8);
  mz_alloc1((unsigned char **)&pty,clen,8);
  mz_alloc1((unsigned char **)&ptlat,clen,8);
  mz_alloc1((unsigned char **)&ptlon,clen,8);
   
  status = IBISColumnSet(ibis,"U_FORMAT","DOUB",cols[0]);
  if (status!=1) IBISSignal(ibis,status,1);
  status = IBISColumnRead(ibis,(char*)ptx,cols[0],1,clen);
  if (status!=1) IBISSignal(ibis,status,1);
   
  status = IBISColumnSet(ibis,"U_FORMAT","DOUB",cols[1]);
  if (status!=1) IBISSignal(ibis,status,1);
  status = IBISColumnRead(ibis,(char*)pty,cols[1],1,clen);
  if (status!=1) IBISSignal(ibis,status,1);
   
  status = IBISColumnSet(ibis,"U_FORMAT","DOUB",cols[2]);
  if (status!=1) IBISSignal(ibis,status,1);
  status = IBISColumnRead(ibis,(char*)ptlon,cols[2],1,clen);
  if (status!=1) IBISSignal(ibis,status,1);
   
  status = IBISColumnSet(ibis,"U_FORMAT","DOUB",cols[3]);
  if (status!=1) IBISSignal(ibis,status,1);
  status = IBISColumnRead(ibis,(char*)ptlat,cols[3],1,clen);
  if (status!=1) IBISSignal(ibis,status,1);
   
  status = IBISFileClose(ibis,0);
  if (status!=1) IBISSignal(ibis,status,1);
  ntiepp = clen;
   
  /* find the center of gravity */
   
  xctr = 0.0; yctr = 0.0;
  for (i=0;i<clen;i++)
    {
      xctr += ptx[i];
      yctr += pty[i];
    }
  xctr /= (double)clen;
  yctr /= (double)clen;
   
  /* the furthest */
   
  dist = 0.0;
  for (i=0;i<clen;i++)
    {
      dx = ptx[i]-xctr;
      dy = pty[i]-yctr;
      tdist = dx*dx+dy*dy;
      if (tdist<=dist) continue;
      dist = tdist;
      isave1 = i; 
    }
  x1 = ptx[isave1];
  y1 = pty[isave1];
   
  /* largest area with center and furthest points */
   
  dist = 0.0;
  for (i=0;i<clen;i++)
    {
      tdist = fabs(triarea(xctr,yctr,x1,y1,ptx[i],pty[i]));
      if (tdist<=dist) continue;
      dist = tdist;
      isave2 = i; 
    }
  x2 = ptx[isave2];
  y2 = pty[isave2];
   
  /* largest area with other two points */
   
  dist = 0.0;
  for (i=0;i<clen;i++)
    {
      tdist = fabs(triarea(x1,y1,x2,y2,ptx[i],pty[i]));
      if (tdist<=dist) continue;
      dist = tdist;
      isave3 = i; 
    }
  x3 = ptx[isave3];
  y3 = pty[isave3];
   
  mq_out_real("v11",ptx[isave1]);
  mq_out_real("v21",pty[isave1]);
  mq_out_real("v31",ptlon[isave1]);
  mq_out_real("v41",ptlat[isave1]);
   
  mq_out_real("v12",ptx[isave2]);
  mq_out_real("v22",pty[isave2]);
  mq_out_real("v32",ptlon[isave2]);
  mq_out_real("v42",ptlat[isave2]);
   
  mq_out_real("v13",ptx[isave3]);
  mq_out_real("v23",pty[isave3]);
  mq_out_real("v33",ptlon[isave3]);
  mq_out_real("v43",ptlat[isave3]);
      
  mq_out_int("ix1",isave1+1);
  mq_out_int("ix2",isave2+1);
  mq_out_int("ix3",isave3+1);
      
  return;
   
}
