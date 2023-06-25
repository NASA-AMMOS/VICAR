#include <math.h>
#include <stdio.h>

#include "vicmain_c.h"
#include "applic.h"
#include "defines.h"
#include "ibisfile.h"
#include "zifmessage.h"

#include "cartoStrUtils.h"
#include "cartoMemUtils.h"

#define MAXCOLS    100

/*  IBIS formatted data to plain ascii file   A. Zobrist    12/14/05   */

void main44(void)
{
   int i,j,k,i_unit,status,clen,ncol,nr=0,sr=0,ibis=0,dummy,parmct,parmdf,blnkout;
   int colcount,coldef,cquote,cols[MAXCOLS],wid[MAXCOLS],totwid,uwid=0;
   char infilename[256],sep[2],fmtstring[10],ctype[MAXCOLS],*cbuf=NULL,*pbuf=NULL;
   double **buf=NULL,penupval,testval;
   FILE *mifcb1=NULL;
   
   zifmessage("IBIS2ASC version 2019-09-05");
   
   /* initialize, fetch params */

   zvparm("cols",cols,&colcount,&coldef,100,0);
   status = zvp("sr",&sr,&dummy);
   status = zvp("nr",&nr,&dummy);
   cquote = zvptst("quote");
   zvparm("sep",sep,&parmct,&parmdf,1,2);
   zvparmd("penupval",&penupval,&parmct,&parmdf,1,0);
   if (fabs(penupval)<0.1) testval = 0.0001;
   else testval = fabs(.001*penupval);
   
   status = zvunit(&i_unit,"inp",1, NULL);
   status = IBISFileOpen(i_unit,&ibis,"read",0,0,0,0);
   if (status!=1) IBISSignalU(i_unit,status,1);
   IBISFileGet(ibis,"nr",&clen,1,1,0);
   IBISFileGet(ibis,"nc",&ncol,1,1,0);
   if (sr==0) sr = 1;
   if (nr==0) nr = clen-sr+1;
   
   zvparm("out",infilename,&parmct,&parmdf,1,255);
   mifcb1 = fopen(infilename,"w");
   
   /* read the ibis columns */
   
   mz_alloc2((unsigned char ***)&buf,colcount,nr,8);
   
   totwid = 0;
   for (i=0;i<colcount;i++)
      {
      status = IBISColumnGet(ibis,"FORMAT",fmtstring,cols[i]);
      if (fmtstring[0]=='A')
         {
         wid[i] = ms_num(&fmtstring[1])+1;
         totwid += wid[i];
         }
      ctype[i] = fmtstring[0];
      }
   
   if (totwid>0)
      {
      mz_alloc1((unsigned char **)&cbuf,totwid*nr,1);
      mz_alloc1((unsigned char **)&pbuf,totwid,1);
      }
   
   uwid = 0;
   for (i=0;i<colcount;i++)
      {
      if (ctype[i]=='A')
         {
         status = IBISColumnRead(ibis,&cbuf[uwid*nr],cols[i],sr,nr);
         if (status!=1) IBISSignal(ibis,status,1);
         uwid += wid[i];
         }
      else
         {
         status = IBISColumnSet(ibis,"U_FORMAT","DOUB",cols[i]);
         if (status!=1) IBISSignal(ibis,status,1);
         status = IBISColumnRead(ibis,(char*)(buf[i]),cols[i],sr,nr);
         if (status!=1) IBISSignal(ibis,status,1);
         }
      }
   
   /* write the file */
   
   for (j=0;j<nr;j++)
      {
      uwid = 0;
      for (i=0;i<colcount;i++)
         {
         blnkout = fabs(buf[i][j]-penupval)<testval;
         if (ctype[i]=='A')
            {
            for (k=0;k<wid[i];k++) pbuf[k] = cbuf[uwid*nr+wid[i]*j+k];
            pbuf[wid[i]-1] = (char)0;
            if (cquote) fprintf(mifcb1,"\"%s\"%s",pbuf,sep);
            else fprintf(mifcb1,"%s%s",pbuf,sep);
            uwid += wid[i];
            }
         else if (ctype[i]=='D')
            {
            if (blnkout) fprintf(mifcb1,"                   %s",sep);
            else fprintf(mifcb1,"%19.12e%s",buf[i][j],sep);
            }
         else if (ctype[i]=='R')
            {
            if (blnkout) fprintf(mifcb1,"               %s",sep);
            else fprintf(mifcb1,"%15.8e%s",buf[i][j],sep);
            }
         else if (ctype[i]=='F')
            {
            if (blnkout) fprintf(mifcb1,"           %s",sep);
            else fprintf(mifcb1,"%11.0f%s",buf[i][j],sep);
            }
         else
            {
            if (blnkout) fprintf(mifcb1,"      %s",sep);
            else fprintf(mifcb1,"%6.0f%s",buf[i][j],sep);
            }
         }
      fprintf(mifcb1,"\n");
      }
     
   mz_free2((unsigned char **)buf,colcount);
   return;
}
