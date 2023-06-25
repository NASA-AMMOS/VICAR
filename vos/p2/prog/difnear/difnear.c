#include <math.h>
#include <string.h>

#include "vicmain_c.h"
#include "applic.h"
#include "defines.h"

#include "cartoMemUtils.h"

#ifndef MAX
#define MAX(a,b)	(((a)>(b))?(a):(b))
#endif

#ifndef MIN
#define MIN(a,b)	(((a)<(b))?(a):(b))
#endif

/*  image copy   A. Zobrist    11/29/01   */

void main44(void)
{
   int i,j,iline,i_unit1,i_unit2,o_unit,nline,nsamp,status,dummy;
   int window,win2,nline2,nsamp2,newval,outval,jsamp;
   int halfpix,add128,absval,iroll,minval,iix,centval=0;
   short int *buf1,**inbuf,*outbuf;
   char fmt_str[10];
   zifmessage("difnear version 2015-11-24");
   
   /* get the parms */
   
   zvp("WINDOW",&window,&dummy);
   if (window%2==0) zmabend("window size must be odd");
   win2 = window/2;
   halfpix = zvptst("HALFPIX");
   add128 = zvptst("ADD128");
   absval = zvptst("ABSVAL");
   if (halfpix&&window>3) zmabend("halfpix only for window=3");
   
   /* open the files */
   
   status = zvunit( &i_unit1, "INP", 1, NULL);
   status = zvopen( i_unit1, "OPEN_ACT", "SA", "IO_ACT", "SA",
			"U_FORMAT","HALF", NULL);
   zvget(i_unit1,"FORMAT",fmt_str, NULL);
   zvget(i_unit1,"NL",&nline, NULL);
   zvget(i_unit1,"NS",&nsamp, NULL);
   if ( strcmp(fmt_str,"BYTE") && strcmp(fmt_str,"HALF"))
      zmabend("Invalid input data format image 1.  Use BYTE or HALF.");
   
   status = zvunit( &i_unit2, "INP", 2, NULL);
   status = zvopen( i_unit2, "OPEN_ACT", "SA", "IO_ACT", "SA",
			"U_FORMAT","HALF", NULL);
   zvget(i_unit2,"FORMAT",fmt_str, NULL);
   zvget(i_unit2,"NL",&nline2, NULL);
   zvget(i_unit2,"NS",&nsamp2, NULL);
   if (nline2!=nline) zmabend("Input images must be the same size");
   if (nsamp2!=nsamp) zmabend("Input images must be the same size");
   if ( strcmp(fmt_str,"BYTE") && strcmp(fmt_str,"HALF"))
      zmabend("Invalid input data format image 2.  Use BYTE or HALF.");
   
   status=zvunit( &o_unit,"OUT",1, NULL);
   status=zvopen( o_unit,"U_NL",nline,"U_NS",nsamp, "U_FORMAT","HALF",
	"OP","WRITE","OPEN_ACT","SA","IO_ACT","SA", NULL);
   
   mz_alloc1((unsigned char **)&buf1,10,2);
   mz_alloc2((unsigned char ***)&inbuf,window,nsamp,2);
   mz_alloc1((unsigned char **)&outbuf,nsamp,2);
   
   /* start the barrel */
   
   for (iline=0;iline<win2;iline++)
      {
      status = zvread(i_unit2,inbuf[iline],"LINE",iline+1,
            "SAMP",1,"NSAMPS",nsamp, NULL);
      }
   
   /* read the lines, calculate the dif, write */
   
   iroll = 0;
   for (iline=0;iline<nline;iline++)
      {
      if (iline<nline-win2)
         {
         status = zvread(i_unit2,inbuf[(iline+win2)%window],
            "LINE",iline+win2+1,"SAMP",1,"NSAMPS",nsamp,NULL);
         }
      status = zvread(i_unit1,buf1,"LINE",iline+1,
            "SAMP",1,"NSAMPS",nsamp, NULL);
      
      /* calculate the dif for the line */
      
      for (jsamp=0;jsamp<nsamp;jsamp++)
         {
         minval = 123;
         outval = 999999;
         if (halfpix) centval = inbuf[iroll][jsamp];
         for (i=0;i<window;i++)
            {
            if (i<(win2-iline)) continue;
            if (i>=(nline-iline+win2)) continue;
            iix = (iroll+i-win2+window)%window;
            for (j=0;j<window;j++)
               {
               if (j<(win2-jsamp)) continue;
               if (j>=(nsamp-jsamp+win2)) continue;
               newval = buf1[jsamp]-inbuf[iix][jsamp+j-win2];
               if (fabs((double)newval)<(double)outval)
                  {
                  outval = (int)fabs((double)newval);
                  minval = newval;
                  }
               if (halfpix)
                  {
                  newval = buf1[jsamp]-(inbuf[iix][jsamp+j-win2]+centval)/2;
                  if (fabs((double)newval)<(double)outval)
                     {
                     outval = (int)fabs((double)newval);
                     minval = newval;
                     }
                  }
               }
            }
         if (add128) outbuf[jsamp] = minval+128;
         else if (absval) outbuf[jsamp] = outval;
         else outbuf[jsamp] = minval;
         }
      zvwrit(o_unit,outbuf,"LINE",iline+1,"SAMP",1,"NSAMPS",nsamp, NULL);
      iroll = (iroll+1)%window;
      }
   
   zvclose(i_unit1, NULL);
   zvclose(i_unit2, NULL);
   return;
}
