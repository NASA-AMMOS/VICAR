#include <math.h>
#include <stdio.h>

#include "vicmain_c.h"
#include "applic.h"
#include "defines.h"

/*  converts tcl strings to ascii file   A. Zobrist   12/14/05   */

void main44(void)
{
   int i,gtcount,gtdef,parmct,parmdf;
   char fileout[100],gtparms[100][200];
   FILE *mifcb1;
   
   /* initialize, fetch params */

   zifmessage("TCL2FILE version 2021-06-10");
   
   zvparm("out",fileout,&parmct,&parmdf,1,99);
   zvparm("tcltext",gtparms,&gtcount,&gtdef,100,200);
   
   /* open the output ascii file (not VICAR) */

   mifcb1 = fopen(fileout,"w");
   
   for (i=0;i<gtcount;i++) fprintf (mifcb1,"%s\n",gtparms[i]);

   fclose(mifcb1);
   return;
}
