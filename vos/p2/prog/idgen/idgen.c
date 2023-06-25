#include <math.h>
#include <string.h>
#include <time.h>

#include "vicmain_c.h"
#include "applic.h"
#include "taeconf.inp"
#include "parblk.inc"
#include "defines.h"

#include "cartoTaeUtils.h"

void main44(void)
{
  int ct, def;
#define MAX_PREFIX_LEN 100
#define MAX_ID_LEN 129
#define TIME_BUF_LEN 25
   char prefix [MAX_PREFIX_LEN + 1];
   char idBuf [MAX_ID_LEN + 1];
   char timeBuf [TIME_BUF_LEN + 1];
   time_t now;
   int i;
   int shorter=0, lower=0;
   
   zifmessage("idgen version Thu Jan  3 2008");
   
   shorter = zvptst("shorter");
   lower = zvptst("lower");

   /* generate the date/time stamp */
   
   if (time (& now) < 0)
     zmabend ("idgen: error reading current time");
   
   strcpy (timeBuf, ctime (& now));
   
   timeBuf [strlen (timeBuf) - 1] = 0; /* trim the trailing newline */
   
   if (shorter || lower) {
     int from, to;

     /* skip day of week and punctuation for shorter version */
     for (from = shorter?3:0, to = 0; from < strlen (timeBuf); from ++) {
       if (! shorter ||
	   (timeBuf [from] >= 'a' && timeBuf [from] <= 'z') ||
	   (timeBuf [from] >= 'A' && timeBuf [from] <= 'Z') ||
	   (timeBuf [from] >= '0' && timeBuf [from] <= '9'))
	 timeBuf [to ++] = lower ? timeBuf [from] | 32 : timeBuf [from];
     }
     timeBuf [to] = '\0';
   }

   /* get the prefix */
   zvparm ("prefix", prefix , &ct, &def, 1, 100);

   if (strlen (prefix) > 100)
     zmabend ("idlen: prefix > 100 chars");

   sprintf (idBuf, "%s%s", prefix, timeBuf);

   for (i = 0; i < strlen (idBuf); i ++)
     if ((idBuf [i] < 'a' || idBuf [i] > 'z') &&
	 (idBuf [i] < 'A' || idBuf [i] > 'Z') &&
	 (idBuf [i] < '0' || idBuf [i] > '9') &&
	 idBuf [i] != '-' && idBuf [i] != '.')
       idBuf [i] = '_';

   mq_out_string ("outvar", idBuf, MAX_ID_LEN + 1);

   return;
}
