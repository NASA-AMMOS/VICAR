#include <math.h>

#include "vicmain_c.h"
#include "applic.h"
#include "defines.h"

#include "cartoMemUtils.h"

/*  splits even and odd columns of an image into two images   A. Zobrist    10/21/05   */

void main44(void)
{
   int j,iline,i_unit,o_unit1,o_unit2,nl,ns,nsout,status;
   short int *buf,*bufout1,*bufout2;
   
   zifmessage("imsplit version 2015-11-05");
   
   /* open the input and output files */
   
   status = zvunit(&i_unit,"INP",1, NULL);
   status = zvopen(i_unit,"OPEN_ACT","SA","IO_ACT","SA","U_FORMAT","HALF", NULL);
   zvget(i_unit,"NL",&nl,"NS",&ns, NULL);
   nsout = ns/2;
   
   status=zvunit(&o_unit1,"OUT",1, NULL);
   status=zvopen(o_unit1,"U_NL",nl,"U_NS",nsout,"U_FORMAT","HALF",
	"OP","WRITE","OPEN_ACT","SA","IO_ACT","SA", NULL);
   
   status=zvunit(&o_unit2,"OUT",2, NULL);
   status=zvopen(o_unit2,"U_NL",nl,"U_NS",nsout,"U_FORMAT","HALF",
	"OP","WRITE","OPEN_ACT","SA","IO_ACT","SA", NULL);
   
   mz_alloc1((unsigned char **)&buf,ns,2);
   mz_alloc1((unsigned char **)&bufout1,nsout,2);
   mz_alloc1((unsigned char **)&bufout2,nsout,2);
   
   /* read the lines, take subset or superset, write */
   
   for (iline=0;iline<nl;iline++)
      {
      status = zvread(i_unit,buf,"LINE",iline+1,"NSAMPS",ns, NULL);
      for (j=0;j<ns;j+=2)
         {
         bufout1[j/2] = buf[j];
         bufout2[j/2] = buf[j+1];
         }
      zvwrit(o_unit1,bufout1,"LINE",iline+1,"NSAMPS",nsout, NULL);
      zvwrit(o_unit2,bufout2,"LINE",iline+1,"NSAMPS",nsout, NULL);
      }
   
   zvclose(i_unit, NULL);
   zvclose(o_unit1, NULL);
   zvclose(o_unit2, NULL);
   return;
}
