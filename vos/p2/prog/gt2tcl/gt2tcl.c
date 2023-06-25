#include "vicmain_c.h"
#include "applic.h"
#include <math.h>
#include <string.h>
#include <ctype.h>

#include "taeconf.inp"
#include "parblk.inc"
#include "defines.h"

#include "cartoTaeUtils.h"
#include "cartoStrUtils.h"

#ifndef MIN
#define MIN(x,y) ((x) < (y) ? (x) : (y))
#endif


#define MAXTEXT    10000
#define MAXSTRPAR  130

void main44(void)
{
   int i,vtype,ct,def,unit,status,vfound,int_result;
   int maxlen,nelement,element,sequence;
   double real_result;
   char *p,*q,char_result[MAXTEXT];
   char keyword[100],key[33],valformat[9],vformat[9];
   
   zifmessage("gt2tcl version 2015-11-01");
   
   /* get the parameters */
   
   zvparm("keyword",keyword,&ct,&def,1,0);
   for (i=0;i<strlen(keyword);i++) keyword[i] = toupper(keyword[i]);
   zvp("vtype",&vtype,&ct);
   zvp("element",&element,&ct);
   zvp("sequence",&sequence,&ct);
    
   /* open the file, look for the keyword in the GeoTIFF property */
   
   status = zvunit(&unit,"inp",1, NULL);
   status = zvopen(unit,"OP","READ","OPEN_ACT","SA",
         "LAB_ACT","SA", NULL);
   
   vfound = 0;
   do
      {
      status=zlninfo(unit,key,valformat,&maxlen,
         &nelement,"ERR_ACT"," ", NULL);
      if (status!=1) break;
      if (strcmp(key,"PROPERTY")==0) continue;
      status=zlinfo(unit,"PROPERTY",key,vformat,
         &maxlen,&nelement,"ERR_ACT"," ",
         "PROPERTY","GEOTIFF", NULL);
      if (status!=1) continue;
      if (strcmp(key,"PROPERTY")==0) continue;
      /* now concatenate the string values / can be vector */
      
      for (i=1;i<=nelement;i++)
         {
         if (nelement==1)
            status=zlget(unit,"PROPERTY",key,char_result,
               "ERR_ACT","SA","FORMAT","STRING","NELEMENT",1,
               "PROPERTY","GEOTIFF","ULEN",133, NULL);
         else
            status=zlget(unit,"PROPERTY",key,char_result,"ELEMENT",i,
               "ERR_ACT","SA","FORMAT","STRING","NELEMENT",1,
               "PROPERTY","GEOTIFF","ULEN",133, NULL);
         if (strcmp(keyword,key)==0&&i==element) { vfound = 1; break; }
         }
      if (vfound) break;
      }
   while (1);
   status = zvclose(unit, NULL);
   p = char_result;
   
   /* handle vectors, and error if sequence off end of vector */
   
   if (vfound&&char_result[0]=='(')
      {
      p++;
      for (i=1;i<sequence;i++)
         {
         q = strpbrk(p,",)");
         if (*q==')') { vfound = 0; break; }
         p = q+1;
         }
      if (vfound)
         {
         q = strpbrk(p,",)");
         *q = (char)0;
         }
      }
   
   /* breaking out of read loop, result in char_result */
   
   if (!vfound) { strcpy(char_result,"-999"); p = char_result; }
   switch (vtype)
      {
      case 0: mq_out_string("val",p,MAXSTRPAR);
	      break;
      case 2: 
      case 4: int_result = ms_num(p);
              mq_out_int("val",int_result);
	      break;
      case 7: 
      case 8: real_result = ms_dnum(&p);
	      mq_out_real("val",real_result);
	      break;
      }

   return;
}
