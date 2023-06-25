#include <math.h>
#include <string.h>

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

#define MAXCOLS 50
#define MAXCOMPONENTS       800000

/*  tabulate connected components to an IBIS file A. Zobrist    11/07/04   */

static char msgBuf[10000];

void main44(void)
{
  int i,j=0,iout,jout,inpcnt,vunit[48],inl[48],ins[48],pixsiz;
  int bignline=0,bignsamp,vsize[4],sizepcnt,sizedef,ncomp,inbr,nbr;
  int inptr[48],tpixsiz,status,dummy,icomp,unit2,ibis2,ncol2,clen2;
  int **stat,*i4out,outnsamp=0,outnline=0,statcol;
  float **pstat;
  double dl,ds;
  char cformat[MAXCOLS][6];
  int **inbuf;
   
  zifmessage("comptab version 2016-01-13");
   
  /* get some parms */
  
  status = zvpcnt("inp",&inpcnt);
    
  zvparm("SIZE",vsize,&sizepcnt,&sizedef,4,0);
  if (vsize[0]==1) zvp("SL",&vsize[0],&dummy);
  if (vsize[1]==1) zvp("SS",&vsize[1],&dummy);
  if (vsize[2]==0) zvp("NL",&vsize[2],&dummy);
  if (vsize[3]==0) zvp("NS",&vsize[3],&dummy);

  /* read the files one line at a time, the first file is connected component
     file and is always halfword */
   
  bignsamp = 0;
  for (i=0;i<inpcnt;i++)
    {
      status = zvunit(&vunit[i],"INP",i+1, NULL);
      status = zvopen(vunit[i],"OPEN_ACT","SA","IO_ACT",
		      "SA","U_FORMAT","FULL", NULL);
      zvget(vunit[i],"NL",&inl[i],"NS",&ins[i],"PIX_SIZE",&pixsiz, NULL);
      
      /* resolve input samples */
      if (vsize[3]!=0) outnsamp = MIN(vsize[3],ins[i]-vsize[1]+1);
      else outnsamp = ins[i];
      if (vsize[3]>ins[i]-vsize[1]+1) {
	sprintf(msgBuf, "Output samples truncated to match input size, input %d", i+1);
	zifmessage(msgBuf);
      }
      ins[i] = outnsamp;
      
      /* resolve input lines */
      if (vsize[2]!=0) outnline = MIN(vsize[2],inl[i]-vsize[0]+1);
      else outnline = inl[i];
      if (vsize[2]>inl[i]-vsize[0]+1) {
	sprintf(msgBuf, "Output lines truncated to match input size, input %d",i+1);
	zifmessage(msgBuf);
      }
      inl[i] = outnline;
      
      if (i==0)
	{
	  bignline = inl[0];
	  inptr[0] = 0;
	  tpixsiz = pixsiz;
	}
      else
	{
	  if (inl[i]!=bignline) zmabend("Images must have same NL");
	  /*if (pixsiz!=tpixsiz) zmabend("Images must have same pixel size");*/
	  inptr[i] = inptr[i-1]+ins[i-1]*pixsiz;
	}
      bignsamp += ins[i];
    }
   
  /* dynamically allocate the buffer and primary statistic array */
   
  statcol = 2*inpcnt+8;
  mz_alloc2((unsigned char ***)&inbuf,inpcnt,bignsamp,4);
  mz_alloc2((unsigned char ***)&stat,statcol,MAXCOMPONENTS,4);
  for (i=0;j<statcol;i++)
    for (j=0;j<MAXCOMPONENTS;j++) stat[i][j] = 0;
   
  /* read the input lines */
   
  ncomp = 0;
  for (iout=0;iout<outnline;iout++)
    {
      for (jout=0;jout<inpcnt;jout++)
	{
	  status = zvread(vunit[jout],inbuf[jout],
			  "LINE",iout+vsize[0],"SAMP",vsize[1],
			  "NSAMPS",ins[jout], NULL);
	}
      /* statistics for ibis file */
      for (i=0;i<outnsamp;i++)
	{
	  icomp = inbuf[0][i]-1;
	  if (icomp<0) continue;
	  if (i<=(outnsamp-3))
            { inbr = i+2; nbr = inbuf[0][i+1]; }
	  else
            { inbr = i-2; nbr = inbuf[0][i-1]; }
	  stat[0][icomp]++;
	  if (stat[0][icomp]==1)
            {
	      stat[1][icomp] = iout+1;
	      stat[2][icomp] = iout+1;
	      stat[3][icomp] = i+1;
	      stat[4][icomp] = i+1;
	      stat[5][icomp] = iout+1;
	      stat[6][icomp] = i+1;
            }
	  else
            {
	      stat[1][icomp] = MIN(iout+1,stat[1][icomp]);
	      stat[2][icomp] = MAX(iout+1,stat[2][icomp]);
	      stat[3][icomp] = MIN(i+1,stat[3][icomp]);
	      stat[4][icomp] = MAX(i+1,stat[4][icomp]);
            }
	  ncomp = MAX(ncomp,icomp+1);
	  for (jout=1;jout<inpcnt;jout++)
            {
	      stat[2*jout+6][icomp] += inbuf[jout][i];
	      if (nbr==0)
		{
		  if (jout==1) stat[7][icomp]++;
		  stat[2*jout+7][icomp] += inbuf[jout][inbr];
		}
            }
	}
    }

  /* process the statistics */

  mz_alloc1((unsigned char **)&i4out,ncomp,4);
  mz_alloc2((unsigned char ***)&pstat,2*inpcnt+4,ncomp,4);
   
  for (icomp=0;icomp<ncomp;icomp++)
    {
      i4out[icomp] = icomp+1;
      pstat[0][icomp] = (float)(stat[0][icomp]);
      dl = (double)(stat[2][icomp]-stat[1][icomp]);
      ds = (double)(stat[4][icomp]-stat[3][icomp]);
      pstat[3][icomp] = (float)sqrt(dl*dl+ds*ds+1.0);
      pstat[1][icomp] = (float)(stat[5][icomp]);
      pstat[2][icomp] = (float)(stat[6][icomp]);
      for (jout=1;jout<inpcnt;jout++)
	{
	  pstat[2*jout+2][icomp] =
            (float)(stat[2*jout+6][icomp])/(float)(stat[0][icomp]);
	  pstat[2*jout+3][icomp] =
            (float)(stat[2*jout+7][icomp])/(float)(stat[7][icomp]);
	}
    }
  mz_free2((unsigned char **)stat,statcol);

  for (i=0;i<inpcnt;i++) zvclose(vunit[i], NULL);

  /* output the pstat array to an ibis file */
  /* open the output file */

  ncol2 = 2*inpcnt+13; /* ten empty real columns are tacked on */
  clen2 = ncomp;
  for (i=0;i<4;i++) strcpy(cformat[i],"FULL");
  for (i=4;i<ncol2;i++) strcpy(cformat[i],"DOUB");
  status = zvunit(&unit2,"out",1, NULL);
  status = IBISFileUnit(unit2,&ibis2,"write",ncol2,clen2,(char*) cformat,"column");
  status = IBISFileUnitOpen(ibis2);
  for (i=0;i<4;i++)
    {
      status = IBISColumnSet(ibis2,"U_FORMAT","FULL",i+1);
      if (status!=1) IBISSignal(ibis2,status,1);
    }
  for (i=4;i<ncol2;i++)
    {
      status = IBISColumnSet(ibis2,"U_FORMAT","REAL",i+1);
      if (status!=1) IBISSignal(ibis2,status,1);
    }
  /*status = IBISFileOpen(ibis2,&ibis,"write",4,clen,0,0);*/

  status = IBISColumnWrite(ibis2,(char*)i4out,1,1,clen2);
  if (status!=1) IBISSignal(ibis2,status,1);
  for (i=1;i<4;i++)
    {
      for (j=0;j<clen2;j++) i4out[j] = (int)(pstat[i-1][j]);
      status = IBISColumnWrite(ibis2,(char*)i4out,i+1,1,clen2);
      if (status!=1) IBISSignal(ibis2,status,1);
    }
  for (i=4;i<ncol2-10;i++)
    {
      status = IBISColumnWrite(ibis2,(char*)(pstat[i-1]),i+1,1,clen2);
      if (status!=1) IBISSignal(ibis2,status,1);
    }

  status = IBISFileClose(ibis2,0);
  if (status!=1) IBISSignal(ibis2,status,1);
   
  return;
}
