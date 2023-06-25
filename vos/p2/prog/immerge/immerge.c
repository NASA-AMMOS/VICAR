#include <math.h>

#include "vicmain_c.h"
#include "applic.h"
#include "defines.h"

#include "cartoMemUtils.h"

/*  reverse of imsplit   A. Zobrist    10/24/05   */

void main44(void)
{
   int j,iline,i_unit1,i_unit2,o_unit,nl,ns,nl2,ns2,nsout,status;
   short int *buf1,*buf2,*bufout;
   
   zifmessage("immerge version 2015-11-05");
   
   /* open the input and output files */
   
   status = zvunit(&i_unit1,"INP",1, NULL);
   status = zvopen(i_unit1,"OPEN_ACT","SA","IO_ACT","SA","U_FORMAT","HALF", NULL);
   zvget(i_unit1,"NL",&nl,"NS",&ns, NULL);
   status = zvunit(&i_unit2,"INP",2, NULL);
   status = zvopen(i_unit2,"OPEN_ACT","SA","IO_ACT","SA","U_FORMAT","HALF", NULL);
   zvget(i_unit2,"NL",&nl2,"NS",&ns2, NULL);
   if (nl2!=nl||ns2!=ns) zmabend("images must be same size");
   nsout = ns*2;
   
   status=zvunit(&o_unit,"OUT",1, NULL);
   status=zvopen(o_unit,"U_NL",nl,"U_NS",nsout,"U_FORMAT","HALF",
	"OP","WRITE","OPEN_ACT","SA","IO_ACT","SA", NULL);
   
   mz_alloc1((unsigned char **)&buf1,ns,2);
   mz_alloc1((unsigned char **)&buf2,ns,2);
   mz_alloc1((unsigned char **)&bufout,nsout,2);
   
   /* read the lines, take subset or superset, write */
   
   for (iline=0;iline<nl;iline++)
      {
      status = zvread(i_unit1,buf1,"LINE",iline+1,"NSAMPS",ns, NULL);
      status = zvread(i_unit2,buf2,"LINE",iline+1,"NSAMPS",ns, NULL);
      for (j=0;j<ns;j++)
         {
         bufout[j*2] = buf1[j];
         bufout[j*2+1] = buf2[j];
         }
      zvwrit(o_unit,bufout,"LINE",iline+1,"NSAMPS",nsout, NULL);
      }
   
   zvclose(i_unit1, NULL);
   zvclose(i_unit2, NULL);
   zvclose(o_unit, NULL);
   return;
}
