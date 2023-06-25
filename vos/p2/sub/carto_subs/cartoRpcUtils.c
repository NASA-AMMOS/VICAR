#include <stdio.h>
#include <string.h>

#include "cartoRpcUtils.h"
#include "cartoStrUtils.h"

void rpcrd( int i, int j, char *labelstr, double *val )
{
   char *p,rpcfield[15],numstr[5];
   
   strcpy(rpcfield,"RPC_FIELD");
   if (i>0)
      {
      sprintf(numstr,"%d",i);
      strcat(rpcfield,numstr);
      }
   sprintf(numstr,"%d=",j+1);
   strcat(rpcfield,numstr);
   p = ms_find(labelstr,rpcfield);
   val[j] = ms_dnum(&p);
   /*printf("rpcfield .%s. val[j] %25.18f\n",rpcfield,val[j]);*/

   return;
}
