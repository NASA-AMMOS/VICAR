#include <math.h>
#include <string.h>

#include "vicmain_c.h"
#include "applic.h"
#include "defines.h"

#include "cartoMemUtils.h"

/*  image box filter for standard deviation  A. Zobrist    12/14/03   */

void main44(void)
{
   int i,j,ibig,iline,i_unit1,o_unit,nline,nsamp,status,dummy;
   int window,win2,jsamp,iroll,iptr,count,ilim;
   int stat=0;
   float outfac;
   double sum,smean=0,sdev=0;
   short int **inbuf,*outbuf;
   int *linenbr;
   char fmt_str[10];
   
   zifmessage("gtstat version 2015-10-23");
   
   /* get the parms */
   
   zvp("WINDOW",&window,&dummy);
   if (window%2==0) zmabend("window size must be odd");
   win2 = window/2;
   zvp("FACTOR",&outfac,&dummy);
   if (zvptst("mean")) stat = 0;
   if (zvptst("sdev")) stat = 1;
   
   /* open the files */
   
   status = zvunit( &i_unit1, "INP", 1, NULL);
   status = zvopen( i_unit1, "OPEN_ACT", "SA", "IO_ACT", "SA",
			"U_FORMAT","HALF", NULL);
   zvget(i_unit1,"FORMAT",fmt_str, NULL);
   zvget(i_unit1,"NL",&nline, NULL);
   zvget(i_unit1,"NS",&nsamp, NULL);
   if ( strcmp(fmt_str,"BYTE") && strcmp(fmt_str,"HALF"))
      zmabend("Invalid input data format image 1.  Use BYTE or HALF.");
   
   status=zvunit( &o_unit,"OUT",1, NULL);
   status=zvopen( o_unit,"U_NL",nline,"U_NS",nsamp, "U_FORMAT","HALF",
	"OP","WRITE","OPEN_ACT","SA","IO_ACT","SA", NULL);
   ilim = nline-win2-1;
   
   mz_alloc2((unsigned char ***)&inbuf,window,nsamp,2);
   mz_alloc1((unsigned char **)&linenbr,window,4);
   mz_alloc1((unsigned char **)&outbuf,nsamp,2);
   for (i=0;i<window;i++) linenbr[i] = -99;
   
   /* start the barrel */
   
   iroll = 0;
   for (iline=0;iline<win2;iline++)
      {
      status = zvread(i_unit1,inbuf[iroll],"LINE",iline+1,
            "SAMP",1,"NSAMPS",nsamp, NULL);
      linenbr[iroll++] = iline;
      }
   
   /* read the new lines at iroll, calculate the std dev at iptr, write */
   
   iptr = 0;
   for (iline=0;iline<nline;iline++)
      {
      if (iline<nline-win2)
         {
         status = zvread(i_unit1,inbuf[iroll],"LINE",iline+win2+1,
               "SAMP",1,"NSAMPS",nsamp, NULL);
         linenbr[iroll] = iline+win2;
         }
      
      for (jsamp=0;jsamp<nsamp;jsamp++)
         {
         for (ibig=0;ibig<2;ibig++)
            {
            if (stat==0&&ibig==1) break;
            sum = 0.0; count = 0;
            for (i=0;i<window;i++)
               {
               if (iline<win2&&i>(iline+win2)) continue;
               if (iline>ilim&&linenbr[i]<iline-win2) continue;
               for (j=jsamp-win2;j<=jsamp+win2;j++)
                  {
                  if (j<0||j>=nsamp) continue;
                  if (ibig==0) sum += inbuf[i][j];
                     else sum += (inbuf[i][j]-smean)*(inbuf[i][j]-smean);
                  count++;
                  }
               }
            if (ibig==0) smean = sum/(double)count;
               else sdev = sqrt(sum/(double)(count-1));
            }
         switch (stat)
            { 
            case 0: outbuf[jsamp] = (short int)(smean*outfac+0.5);
                    break;
            case 1: outbuf[jsamp] = (short int)(sdev*outfac+0.5);
            }
         }
      zvwrit(o_unit,outbuf,"LINE",iline+1,"SAMP",1,"NSAMPS",nsamp, NULL);
      iroll = (iroll+1)%window;
      iptr = (iptr+1)%window;
      }
   
   zvclose(i_unit1, NULL);
   zvclose(o_unit, NULL);
   return;
}
