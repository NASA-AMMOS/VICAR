#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "vicmain_c.h"
#include "applic.h"
#include "defines.h"
#include "ibisfile.h"
#include "ibiserrs.h"
#include "zifmessage.h"
#include "zmabend.h"
#include "prnt.h"

#include "cartoMatUtils.h"
#include "cartoSortUtils.h"
#include "cartoLsqUtils.h"
#include "cartoMemUtils.h"
#include "cartoGridUtils.h"

#define VORMAXPOLY      1000   /* MAX EDGES IN A SINGLE VORONOI POLYGON */
#define MAXLSQ          400
#define MAXLSQD         MAXLSQ+1
#define MAXLSQ2         2*MAXLSQD
#define MAXLSQ10        10*MAXLSQD
#define MAXTIE          10000000

/************************************************************************/
/* program polygeov                                                      */
/************************************************************************/
/*  83-10 ...alz... initial version, new algorithm for thiessen         */
/*                  triangulation, converted to c,                      */
/************************************************************************/

short int *vgroup;
int bggset,bgg,ccjj[VORMAXPOLY],ccgrp[VORMAXPOLY];
double cc1x[VORMAXPOLY],cc1y[VORMAXPOLY],
      cc2x[VORMAXPOLY],cc2y[VORMAXPOLY],
      ccrx[VORMAXPOLY],ccry[VORMAXPOLY];
double gridtol;
typedef unsigned char  byte;

/*                                                  ALZ
   ct1, ct2, ct4, ct7, ct8, st1, st2, st4, st7, st8

   Subroutines used with tabular data set operations
   for type conversion and storing.  The unsigned char
   is for image handling only.

*/

unsigned char ct1(s) unsigned char *s; { return(*s); }
short int ct2(s) short int *s; { return(*s); }
int ct4(s) int *s; { return(*s); }
float ct7(s) float *s; { return(*s); }
double ct8(s) double *s; { return(*s); }
void st1(v,s) unsigned char v,*s; { *s = v; return; }
void st2(v,s) short int v,*s; { *s = v; return; }
void st4(v,s) int v,*s; { *s = v; return; }
void st7(v,s) float v,*s; { *s = v; return; }
void st8(v,s) double v,*s; { *s = v; return; }
char buff[2000];

void main44(void)
{
  double *rpar=NULL;
  int *con1=NULL,*con2=NULL,*con3=NULL;
   
  double tmaxx,tmaxy,tminx,tminy,x=0.,y=0.,*ptx=NULL,*pty=NULL;
  double clsq[MAXLSQ10], clsqxy[MAXLSQ2], elsqxy[MAXLSQ2];
  double csol[20],*optx=NULL,*opty=NULL;
  double tab[4],work[16];
  double **coeff=NULL,dx;
  double normlz[4];
  double *xin=NULL,*yin=NULL,*xout=NULL,*yout=NULL;
  float fac=0.;
  int cols[4],dcols[4];
  int zgeom=0,mgeom,abendl,abendg,lgeom,geomv,keystone,linear; /*booleans*/
  int quad,cubic,polynom; /*booleans*/
  int plot,printit;
  int found,null9;
  int npoint = 4,nrank = 6,nerr = 0;
  int lgeomlike = 0;
  char polystring[10];
   
  int i=0,j,k=0,n,ier=0,inpcnt,status=0,unit=0,colcount,ibis,clen,clen2,ptr=0,record;
  int irow,ntiepp,icnt,idef,nklsq=0,nlret;
  int ntri,gridnah,gridnav=0,pcount,pdef;
  int ttri,ttrj1,coldef,tiepdef;
  int ix,itri=0,tri,isign,ibisOut;
  double eps,skinny,reject;
        
  zifmessage("POLYGEOV version 2019-09-06");
   
  printit = zvptst("PRINT");
   
  zvparm("poly",polystring,&pcount,&pdef,4,0);
  keystone = strncmp(polystring,"KEYSTONE",8)==0;
  linear = strncmp(polystring,"LINEAR",6)==0;
  quad = strncmp(polystring,"QUAD",4)==0;
  cubic = strncmp(polystring,"CUBIC",5)==0;
  polynom = keystone||linear||quad||cubic;
  zvparmd("gridtol",&gridtol,&pcount,&pdef,1,0);
   
  /*	if inp file specified then read in tiepoints from
    the ibis interface file */

  status = zvpcnt("inp",&inpcnt);
  if (inpcnt>0)
    {
      status = zvunit(&unit,"inp",1, NULL);
     
      zvparm("dcols",dcols,&colcount,&coldef,4,0);
      if (colcount!=4) zmabend("only (x,y) mapping for now");
      zvparm("cols",cols,&colcount,&coldef,4,0);
      status = IBISFileOpen(unit,&ibis,"read",0,0,0,0);
      if (status!=1) IBISSignalU(unit,status,1);
      IBISFileGet(ibis,"nr",&clen,1,1,0);
      mz_alloc1((unsigned char **)&rpar,colcount*clen,8);
      ptr = 0;
      status = IBISRecordOpen(ibis,&record,0,cols,colcount,IFMT_DOUB);
      if (status!=1) IBISSignal(ibis,status,1);
      for (irow=1;irow<=clen;irow++)
	{
	  status = IBISRecordRead(record,(char*) &rpar[ptr],irow);
	  if (status!=1) IBISSignal(ibis,status,1);
	  ptr += colcount;
	}
      status = IBISFileClose(ibis,0);
      if (status!=1) IBISSignal(ibis,status,1);
      ntiepp = ptr;
    }
  else
    {
      mz_alloc1((unsigned char **)&rpar,1625,8);
      zvparm("tiepoint",&rpar,&ntiepp,&tiepdef,1625,0);
    }
  zvparm("gridnah",&gridnah,&icnt,&idef,1,0);

  abendl = zvptst("abend");
  abendg = zvptst("abendg");
  reject = 0.01;
  zvparm("reject",&reject,&icnt,&idef,1,0);
  reject = reject*reject;
  /* geoma = zvptst("geoma"); */
  geomv = zvptst("geomv");
  mgeom = zvptst("mgeom");
  lgeom = zvptst("lgeom");
  lgeomlike = mgeom||lgeom||geomv;
  zgeom = zvptst("geomz");
  plot = zvptst("plot");
  if (plot&&polynom)
    zmabend("can't plot with polynomial fit options");
  /* geoma = 1; */
  lgeomlike = 0;
  zgeom = 0;   /* these were used in tieconv, not here */

  if (zgeom) npoint = 3;
  if (zgeom) nrank = 3;
   
  n = ntiepp/npoint;
  if (n<3) zmabend("need 3 tiepoints");
  if (n<4&&keystone) zmabend("need 4 tiepoints for keystone option");
  if (n<6&&quad) zmabend("need 6 tiepoints for quadratic option");
  if (n<10&&cubic) zmabend("need 10 tiepoints for cubic option");
  if (n>MAXTIE) zmabend("maximum input tiepoints exceeded");
  if (lgeomlike)
    {
      for (i=0;i<ntiepp;i+=4)
	{
	  dx = rpar[i];
	  rpar[i] = rpar[i+2];
	  rpar[i+2] = dx;
	  dx = rpar[i+1];
	  rpar[i+1] = rpar[i+3];
	  rpar[i+3] = dx;
	}
    }
 
  tmaxx = 0.0;
  tmaxy = 0.0;
  tminx = 1.e20;
  tminy = 1.e20;
  if (n>MAXLSQ) nklsq = MAXLSQ; else nklsq = n;
   
  mz_alloc1((unsigned char **)&ptx,n+4,8);
  mz_alloc1((unsigned char **)&pty,n+4,8);
  mz_alloc1((unsigned char **)&optx,n+4,8);
  if (!zgeom) mz_alloc1((unsigned char **)&opty,n+4,8);
   
  /* transfer to vector format for thiessen; the slight random 
     perterbation was an experiment, can help with debugging the 
     rectangular grid input case */
   
  /*srand((unsigned)1);*/
  if (zgeom) ptr = 0; else ptr = 2;
  for (i=0;i<4;i++) normlz[i] = 0.0;
  for (i=0;i<n;i++)
    {
      /*  experimental code, also need srand() call above
	  randout = 1.0+1.5e-14*(double)(((unsigned int)rand())%32768)/32767.0;
	  ptx[i] = rpar[ptr]*randout;
	  randout = 1.0+1.5e-14*(double)(((unsigned int)rand())%32768)/32767.0;
	  pty[i] = rpar[ptr+1]*randout;*/
      
      ptx[i] = rpar[ptr];
      pty[i] = rpar[ptr+1];
      
      if (!zgeom)
	{
	  optx[i] = rpar[ptr-2];
	  opty[i] = rpar[ptr-1];
	}
      else optx[i] = rpar[ptr+2];
      
      normlz[0] += ptx[i];
      normlz[1] += pty[i];
      normlz[2] += optx[i];
      if (!zgeom) normlz[3] += opty[i];
      
      if (ptx[i]>tmaxx) tmaxx = ptx[i];
      if (ptx[i]<tminx) tminx = ptx[i];
      if (pty[i]>tmaxy) tmaxy = pty[i];
      if (pty[i]<tminy) tminy = pty[i];
      ptr += npoint;
    }
  free(rpar);
  for (i=0;i<4;i++) normlz[i] = (double)((int)(normlz[i]/(double)n+0.5));
  for (i=0;i<n;i++)
    {
      ptx[i] -= normlz[0];
      pty[i] -= normlz[1];
      optx[i] -= normlz[2];
      if (!zgeom) opty[i] -= normlz[3];
    }
  tmaxx -= normlz[0];
  tminx -= normlz[0];
  tmaxy -= normlz[1];
  tminy -= normlz[1];
   
  /* detect grid here and set gridnah, not necessary for polynom cases */
  /* some grids have null areas (-999,-999) for example off earth.  these */
  /* are required to be a grid, and the getlinef is applied to fill the */
  /* -999 areas with an approximate grid */
   
  if (!polynom)
    {
      cartogetline(ptx,pty,0,1,n,n,&gridnah,&null9);
      if (null9==1) goto nullarea;
      if (gridnah==0) goto endgrid;
      if (n%(gridnah+1)!=0) { gridnah = 0; goto endgrid; }
      gridnav = n/(gridnah+1)-1;
      if (n%(gridnav+1)!=0) { gridnah = 0; goto endgrid; }
      if ((double)gridnah<(gridtol-2.0)) { gridnah = 0; goto endgrid; }
      if ((double)gridnav<(gridtol-2.0)) { gridnah = 0; goto endgrid; }
      for (i=1;i<=gridnav;i++)
	{
	  cartogetline(ptx,pty,i*(gridnah+1),1,gridnah+1,n,&k,&null9);
	  if (null9==1) goto nullarea2;
	  if (k!=gridnah) { gridnah = 0; goto endgrid; }
	}
      for (i=0;i<=gridnah;i++)
	{
	  cartogetline(ptx,pty,i,gridnah+1,gridnav+1,n,&k,&null9);
	  if (null9==1) goto nullarea2;
	  if (k!=gridnav) { gridnah = 0; goto endgrid; }
	}
    }
  goto endgrid;
 nullarea:   /* try five more ways to find grid with null areas */
  cartogetline(ptx,pty,n-1,-1,n,n,&gridnah,&null9);
  if (null9==0)
    {
      if (n%(gridnah+1)!=0) zmabend("(nullarea) failure to find grid");
      gridnav = n/(gridnah+1)-1;
      if (n%(gridnav+1)!=0) zmabend("(nullarea) failure to find grid");
      goto nullarea2;
    }
  getline2(ptx,pty,0,1,n/2,n,&gridnah);
  if (gridnah>0) gridnav = n/(gridnah+1)-1;
  if (gridnah>0&&n%(gridnah+1)==0&&n%(gridnav+1)==0) goto nullarea2;
  getline2(ptx,pty,n-1,-1,n/2,n,&gridnah);
  if (gridnah>0) gridnav = n/(gridnah+1)-1;
  if (gridnah>0&&n%(gridnah+1)==0&&n%(gridnav+1)==0) goto nullarea2;
  getline3(ptx,pty,0,1,n/2,n,&gridnah);
  if (gridnah>0) gridnav = n/(gridnah+1)-1;
  if (gridnah>0&&n%(gridnah+1)==0&&n%(gridnav+1)==0) goto nullarea2;
  getline3(ptx,pty,n-1,-1,n/2,n,&gridnah);
  if (gridnah>0) gridnav = n/(gridnah+1)-1;
  if (gridnah>0&&n%(gridnah+1)==0&&n%(gridnav+1)==0) goto nullarea2;
  else zmabend("(nullarea) failure to find grid");
 nullarea2: 
  for (i=0;i<=gridnav;i++) gridfill(ptx,pty,i*(gridnah+1),1,gridnah);
  for (i=0;i<=gridnav;i++) gridfill(ptx,pty,(i+1)*(gridnah+1)-1,-1,gridnah);
  for (i=0;i<=gridnah;i++) gridfill(ptx,pty,i,gridnah+1,gridnav);
  for (i=0;i<=gridnah;i++) gridfill(ptx,pty,n-1-i,-gridnah-1,gridnav);
 endgrid:
      
  /* for the large case, the random formula scatters the points to be
     fitted across the area extended; will get duplicates but that is not
     a problem with a sample of 400; the sequence repeats for the the j 
     loop */
   
  for (j=0;j<npoint-2;j++)
    {
      k = 0;
      for (i=0;i<nklsq;i++)
	{
	  if (n>((3*MAXLSQ)/2)) k = (k*379+i*i)%n; else k = i;
	  clsq[i] = ptx[k];
	  clsq[i+nklsq] = pty[k];
	  clsq[i+nklsq*2] = 1.0;
	  clsq[i+nklsq*3] = ptx[k]*pty[k];
	  clsq[i+nklsq*4] = ptx[k]*ptx[k];
	  clsq[i+nklsq*5] = pty[k]*pty[k];
	  clsq[i+nklsq*6] = ptx[k]*ptx[k]*ptx[k];
	  clsq[i+nklsq*7] = ptx[k]*ptx[k]*pty[k];
	  clsq[i+nklsq*8] = ptx[k]*pty[k]*pty[k];
	  clsq[i+nklsq*9] = pty[k]*pty[k]*pty[k];
	  clsqxy[i] = optx[k];
	  if (!zgeom) clsqxy[i+nklsq] = opty[k];
	  elsqxy[i] = optx[k];
	  if (!zgeom) elsqxy[i+nklsq] = opty[k];
	}
      eps = 1.e-7;
      for (i=0;i<10;i++) csol[i+j*10] = 0.0;
      if (linear)
	lsqfit(clsq,&clsqxy[j*nklsq],nklsq,3,&csol[j*10],eps,&ier);
      else if (keystone)
	lsqfit(clsq,&clsqxy[j*nklsq],nklsq,4,&csol[j*10],eps,&ier);
      else if (quad)
	lsqfit(clsq,&clsqxy[j*nklsq],nklsq,6,&csol[j*10],eps,&ier);
      else if (cubic)
	lsqfit(clsq,&clsqxy[j*nklsq],nklsq,10,&csol[j*10],eps,&ier);
      else
	lsqfit(clsq,&clsqxy[j*nklsq],nklsq,3,&csol[j*10],eps,&ier);
    }
  if (ier!=0) zmabend("least squares fit failure");
  dx = (tmaxx-tminx+tmaxy-tminy)*5.0;

  /* open the update file before branching */

  status = zvunit(&ibisOut,"inp",2, NULL);
  status = IBISFileOpen(ibisOut,&ibis,"update",0,0,0,0);
  if (status!=1) IBISSignalU(unit,status,1);
  IBISFileGet(ibis,"nr",&clen2,1,1,0);
  mz_alloc1((unsigned char **)&xin,clen2,8);
  mz_alloc1((unsigned char **)&yin,clen2,8);
  mz_alloc1((unsigned char **)&xout,clen2,8);
  mz_alloc1((unsigned char **)&yout,clen2,8);
  for (i=0;i<4;i++)
    {
      status = IBISColumnSet(ibis,"U_FORMAT","DOUB",dcols[i]);
      if (status!=1) IBISSignal(ibis,status,1);
    }
  status = IBISColumnRead(ibis,(char*) xin,dcols[0],1,clen2);
  if (status!=1) IBISSignal(ibis,status,1);
  status = IBISColumnRead(ibis,(char*) yin,dcols[1],1,clen2);
  if (status!=1) IBISSignal(ibis,status,1);

  if (polynom) goto polyout;
   
  if (printit)
    {
      zprnt(8,3,csol,"lsq fit x'=ax+by+c.");
      if (!zgeom) zprnt(8,3,&csol[10],"lsq fit y'=dx+ey+f.");
      zprnt(8,nklsq*(npoint-2),elsqxy,"data.");
    }
      
  for (i=0;i<nklsq;i++)
    {
      if (n>((3*MAXLSQ)/2)) k = (i*i)%n; else k = i;
      elsqxy[i] = elsqxy[i]-ptx[k]*csol[0]-pty[k]*csol[1]-csol[2];
      if (!zgeom) elsqxy[i+nklsq] = elsqxy[i+nklsq]-
	ptx[k]*csol[10]-pty[k]*csol[11]-csol[12];
    }
  if (printit) zprnt(8,nklsq*(npoint-2),elsqxy,"residuals.");
     
  ptr = n*npoint;
  if (!zgeom) ptr = ptr+2;
  ptx[n] = tminx-dx;
  pty[n] = (tminy+tmaxy)*0.5;
  ptx[n+1] = (tminx+tmaxx)*0.5;
  pty[n+1] = tmaxy+dx;
  ptx[n+2] = (tminx+tmaxx)*0.5;
  pty[n+2] = tminy-dx;
  ptx[n+3] = tmaxx+dx;
  pty[n+3] = (tminy+tmaxy)*0.5;
  
  fac = 0.5;         /* 0 for linear, 1.0 for cubic, 0.5 for 1/2 each */
  for (i=0;i<4;i++)
    {
      x = ptx[n+i];
      y = pty[n+i];
      optx[n+i] = csol[0]*x+csol[1]*y+csol[2]+fac*(csol[3]*x*y+
						   csol[4]*x*x+csol[5]*y*y+csol[6]*x*x*x+csol[7]*x*x*y+
						   csol[8]*x*y*y+csol[9]*y*y*y);
      if (!zgeom) opty[n+i] = csol[10]*x+csol[11]*y+csol[12]+fac*(csol[13]*x*y+
								  csol[14]*x*x+csol[15]*y*y+csol[16]*x*x*x+csol[17]*x*x*y+
								  csol[18]*x*y*y+csol[19]*y*y*y);
    }
  n += 4;

  /* ready for the big triangulation routine, con1,con2,con3 are
     mallocked in the subroutine (type is **) */
      
  if (gridnah==0)
    {
      zifmessage("no grid detected");
      if (abendg&&n>1500)
	zmabend("user abend on no grid found");
      skinny = 0.0;
      thiessen(n,&nlret,reject,skinny,abendl,ptx,pty,&ntri,
	       &con1,&con2,&con3);
    }
  else
    {
      zifmessage("grid automatically detected");
      tgrid(n,&nlret,ptx,pty,&ntri,&con1,&con2,&con3,gridnah,csol,
	    optx,opty,zgeom);
    }
   
  if (printit)
    {
      zprnt(4,1,&n,"nodes.");
      zprnt(4,1,&nlret,"edges.");
      zprnt(4,1,&ntri,"triangles.");
    }
   
  /* solve triangles */
      
  mz_alloc2((unsigned char ***)&coeff,nrank,ntri,8);
  for (ix=0;ix<2;ix++)
    {
      if (zgeom&&ix>0) break;
      for (itri=0;itri<ntri;itri++)
	{
	  work[0] = ptx[con1[itri]-1];
	  work[1] = ptx[con2[itri]-1];
	  work[2] = ptx[con3[itri]-1];
	  work[3] = pty[con1[itri]-1];
	  work[4] = pty[con2[itri]-1];
	  work[5] = pty[con3[itri]-1];
	  work[6] = 1.;
	  work[7] = 1.;
	  work[8] = 1.;
	  if (ix==0)
            {
	      tab[0] = optx[con1[itri]-1];
	      tab[1] = optx[con2[itri]-1];
	      tab[2] = optx[con3[itri]-1];
	    }
	  else
	    {
	      tab[0] = opty[con1[itri]-1];
	      tab[1] = opty[con2[itri]-1];
	      tab[2] = opty[con3[itri]-1];
	    }
	  dgauss(work,tab,3,1.e-14,&ier);
	  for (j=0;j<3;j++) coeff[j+ix*3][itri] = tab[j];
	  if (ier!=0) coeff[0][itri] = 1.0E35;
	}
    }
         
  ntri = ntri-nerr;
     
  /* start the geom of the second input */
   
  ptr = 0;
  tri = 0;
  ttrj1 = 0;
  for (i=0;i<clen2;i++)
    {
      x = xin[i]-normlz[0];
      y = yin[i]-normlz[1];
      isign = -1;
      found = 0;
      ttri = tri+ntri;
         
      for (k=0;k<ntri;k++)
	{
	  tri = (ttri+((k+1)/2)*isign)%ntri;
	  isign = -isign;
	  if (coeff[0][tri]>1.0E34) continue;
	  if (insidetri(x,y,
			ptx[con1[tri]-1],pty[con1[tri]-1],
			ptx[con2[tri]-1],pty[con2[tri]-1],
			ptx[con3[tri]-1],pty[con3[tri]-1]))
            {
	      found = 1;
	      break;
            }
	}
      if (!found)
	{
	  tri = ttri-ntri;
	  if (j==1) tri = ttrj1;
	  if (printit){
	    sprintf(buff, "grid point %f,%f not in triangle",x,y);
	    zifmessage(buff);
	  }
	}
      if (j==1) ttrj1 = tri;
      
      xout[i] = coeff[0][tri]*x+coeff[1][tri]*y+coeff[2][tri]+normlz[2];
      yout[i] = coeff[3][tri]*x+coeff[4][tri]*y+coeff[5][tri]+normlz[3];
    }
  
  /* the next section is for output of polynomial fit cases */
   
 polyout:
  if (polynom)
    {
      if (printit)
	{
	  sprintf(buff, "x' lsq fit, coeff of x = %15.8f",csol[0]); zifmessage(buff);
	  sprintf(buff, "x' lsq fit, coeff of y = %15.8f",csol[1]); zifmessage(buff);
	  sprintf(buff, "x' lsq fit, coeff of 1 = %15.8f",csol[2]); zifmessage(buff);
	  sprintf(buff, "x' lsq fit, coeff of xy = %15.8f",csol[3]); zifmessage(buff);
	  sprintf(buff, "x' lsq fit, coeff of xx = %15.8f",csol[4]); zifmessage(buff);
	  sprintf(buff, "x' lsq fit, coeff of yy = %15.8f",csol[5]); zifmessage(buff);
	  sprintf(buff, "x' lsq fit, coeff of xxx = %15.8f",csol[6]); zifmessage(buff);
	  sprintf(buff, "x' lsq fit, coeff of xxy = %15.8f",csol[7]); zifmessage(buff);
	  sprintf(buff, "x' lsq fit, coeff of xyy = %15.8f",csol[8]); zifmessage(buff);
	  sprintf(buff, "x' lsq fit, coeff of yyy = %15.8f",csol[9]); zifmessage(buff);
         
	  if (!zgeom)
            {
	      sprintf(buff, "y' lsq fit, coeff of x = %15.8f",csol[10]); zifmessage(buff);
	      sprintf(buff, "y' lsq fit, coeff of y = %15.8f",csol[11]); zifmessage(buff);
	      sprintf(buff, "y' lsq fit, coeff of 1 = %15.8f",csol[12]); zifmessage(buff);
	      sprintf(buff, "y' lsq fit, coeff of xy = %15.8f",csol[13]); zifmessage(buff);
	      sprintf(buff, "y' lsq fit, coeff of xx = %15.8f",csol[14]); zifmessage(buff);
	      sprintf(buff, "y' lsq fit, coeff of yy = %15.8f",csol[15]); zifmessage(buff);
	      sprintf(buff, "y' lsq fit, coeff of xxx = %15.8f",csol[16]); zifmessage(buff);
	      sprintf(buff, "y' lsq fit, coeff of xxy = %15.8f",csol[17]); zifmessage(buff);
	      sprintf(buff, "y' lsq fit, coeff of xyy = %15.8f",csol[18]); zifmessage(buff);
	      sprintf(buff, "y' lsq fit, coeff of yyy = %15.8f",csol[19]); zifmessage(buff);
            }
	  zprnt(8,nklsq*(npoint-2),elsqxy,"data.");
	}
      
      for (i=0;i<nklsq;i++)
	{
	  if (n>((3*MAXLSQ)/2)) k = (i*i)%n; else k = i;
	  elsqxy[i] = elsqxy[i]
            -csol[0]*ptx[k]
            -csol[1]*pty[k]
            -csol[2]
            -csol[3]*ptx[k]*pty[k]
            -csol[4]*ptx[k]*ptx[k]
            -csol[5]*pty[k]*pty[k]
            -csol[6]*ptx[k]*ptx[k]*ptx[k]
            -csol[7]*ptx[k]*ptx[k]*pty[k]
            -csol[8]*ptx[k]*pty[k]*pty[k]
            -csol[9]*pty[k]*pty[k]*pty[k];
	  if (!zgeom) elsqxy[i+nklsq] = elsqxy[i+nklsq]
            -csol[10]*ptx[k]
            -csol[11]*pty[k]
            -csol[12]
            -csol[13]*ptx[k]*pty[k]
            -csol[14]*ptx[k]*ptx[k]
            -csol[15]*pty[k]*pty[k]
            -csol[16]*ptx[k]*ptx[k]*ptx[k]
            -csol[17]*ptx[k]*ptx[k]*pty[k]
            -csol[18]*ptx[k]*pty[k]*pty[k]
            -csol[19]*pty[k]*pty[k]*pty[k];
	}
      if (printit)
	zprnt(8,nklsq*(npoint-2),elsqxy,"residuals.");
      
      /* like above except use surface fit for all points */
      
      ptr = 0;
      for (i=0;i<clen2;i++)
	{
	  x = xin[i]-normlz[0];
	  y = yin[i]-normlz[1];
	  xout[i] = csol[0]*x
	    +csol[1]*y
	    +csol[2]
	    +csol[3]*x*y
	    +csol[4]*x*x
	    +csol[5]*y*y
	    +csol[6]*x*x*x
	    +csol[7]*x*x*y
	    +csol[8]*x*y*y
	    +csol[9]*y*y*y+normlz[2];
	  yout[i] = csol[10]*x
	    +csol[11]*y
	    +csol[12]
	    +csol[13]*x*y
	    +csol[14]*x*x
	    +csol[15]*y*y
	    +csol[16]*x*x*x
	    +csol[17]*x*x*y
	    +csol[18]*x*y*y
	    +csol[19]*y*y*y+normlz[3];
	}
    } 
      
  /* Output array to IBIS file in col_ordr */
    
  status = IBISColumnWrite(ibis,(char*) xout,dcols[2],1,clen2);
  if (status!=1) IBISSignal(ibis,status,1); 
  status = IBISColumnWrite(ibis,(char*) yout,dcols[3],1,clen2);
  if (status!=1) IBISSignal(ibis,status,1); 
  status = IBISFileClose(ibis,0);
  return;
}
