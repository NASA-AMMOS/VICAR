#include <math.h>
#include <stdio.h>
#include <stdlib.h>

#include "vicmain_c.h"
#include "applic.h"
#include "defines.h"
#include "ibisfile.h"

#include "cartoStrUtils.h"
#include "cartoMemUtils.h"

void main44(void)
{
  int i,j,k,i_unit,o_unit,status,clen,ncol,nr,sr,ibis,dummy,parmct,parmdf,blnkout;
  int colcount,coldef,cquote;
   char outfilename[256],sep[2],fmtstring[10],*ctype=NULL,*cbuf,*pbuf;
   double buf,penupval,testval;
   FILE *mifcb1;
   int imgRow, imgCol;
   double *imgLine = NULL;

#define DOUB_TYPE 1
#define REAL_TYPE 2
#define FULL_TYPE 3
   int dataType = 0;
   
   zvmessage("ibis2img - version 2015-11-01"," ");

   /* initialize, fetch params */

   status = zvunit(&i_unit,"inp",1, NULL);
   status = IBISFileOpen(i_unit,&ibis,"read",0,0,0,0);
   if (status!=1) IBISSignalU(i_unit,status,1);
   IBISFileGet(ibis,"nr",&clen,1,1,0);
   IBISFileGet(ibis,"nc",&ncol,1,1,0);

   printf("copying from ibis nr %d nc %d\n", clen, ncol);
   
   if (! (imgLine = (double*) malloc(sizeof(double) * ncol))) {
     fprintf(stderr, "error allocating image line buffer\n");
     exit(1);
   }

   /* read the ibis column formats */
   for (i = 0; i < ncol; i ++) {
     status = IBISColumnGet(ibis, "FORMAT", fmtstring, i + 1);
     if (fmtstring[0] != 'R' && fmtstring[0] != 'D') {
       fprintf(stderr, "column format %c not supported\n", fmtstring[0]);
       exit(1);
     }
   }

   zvselpi(0);			/* don't copy ibis file label to output */
   status = zvunit(&o_unit, "out", 1, NULL);
   if (zvopen (o_unit, "U_NL", clen, "U_NS", ncol, "OP", "WRITE", "OPEN_ACT", "SA", "IO_ACT", "SA", "O_FORMAT", "DOUB", NULL) != 1)
     zmabend ("zvopen failed");

   /* copy the data row/line by row/line */
   for (imgRow=0; imgRow<clen; imgRow++) {
     for (imgCol=0; imgCol < ncol;imgCol++) {
       status = IBISColumnSet(ibis,"U_FORMAT","DOUB",imgCol+1);
       if (status!=1) IBISSignal(ibis,status,1);
       status = IBISColumnRead(ibis,(char*)(imgLine+imgCol),imgCol+1,imgRow+1,1);
       if (status!=1) IBISSignal(ibis,status,1);
     }

     zvwrit (o_unit, imgLine, "LINE", imgRow + 1, "SAMP", 1, "NSAMPS", ncol, NULL);
   }

   zvclose (o_unit, NULL);
}
