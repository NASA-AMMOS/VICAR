#include <math.h>

#include "vicmain_c.h"
#include "applic.h"
#include "defines.h"
#include "ibisfile.h"

//#include "cartoVicarProtos.h"
#include "cartoMemUtils.h"

#include "astroreference_camera.h"
#include "time_conversion.h"
#include "burl.h" /* for ERR and OK */

/*  compute rpc from spacecraft ephemeris, camera model   A. Zobrist    09/21/05   */

static char msgBuf[10000];
void main44(void)
{
   int i,j,urangecount,vrangecount,hrangecount,dummy,cols[5],colcount;
   int TOD_t_SV_count, TOD_q_SV_count;
   int SV_t_C_count, SV_q_C_count;
   int ACS_time_count;
   int   fucount,fvcount,gr,gc,kappacount;
   int qcount,u0count,v0count,status,unit,ibis,clen;
   double ACS_time;
   double TT, UT1;
   double **bufout;
   double urange[3],vrange[3],hrange[3];
   double TOD_t_SV[3], TOD_q_SV[4];
   double SV_t_C[3], SV_q_C[4];
   double fu,fv,q,u0,v0;
   double kappa[5];
   double *gout;
   char leapFile[100];
   int leapFileCount;
   
   zifmessage("ASTROREF version 2016-01-13");
   
   /* get some parms */
   zvparmd("ACS_time",&ACS_time,&ACS_time_count,&dummy,1,0);

   zvparmd("urange",urange,&urangecount,&dummy,3,0);
   zvparmd("vrange",vrange,&vrangecount,&dummy,3,0);
   zvparmd("hrange",hrange,&hrangecount,&dummy,3,0);
   zvparmd("TOD_t_SV",TOD_t_SV,&TOD_t_SV_count,&dummy,3,0);
   zvparmd("TOD_q_SV",TOD_q_SV,&TOD_q_SV_count,&dummy,4,0);
   zvparmd("SV_t_C",SV_t_C,&SV_t_C_count,&dummy,3,0);
   zvparmd("SV_q_C",SV_q_C,&SV_q_C_count,&dummy,4,0);
   zvparmd("fu",&fu,&fucount,&dummy,1,0);
   zvparmd("fv",&fv,&fvcount,&dummy,1,0);
   zvparmd("q",&q,&qcount,&dummy,1,0);
   zvparmd("u0",&u0,&u0count,&dummy,1,0);
   zvparmd("v0",&v0,&v0count,&dummy,1,0);
   zvparmd("kappa",kappa,&kappacount,&dummy,5,0);
   zvparm("cols",cols,&colcount,&dummy,5,0);
   zvparm("leapfile", leapFile, &leapFileCount, &dummy, 1, 99);

   /* call the function */
   
   /*mz_alloc1((unsigned char **)&gout,100000,8);*/

   if ( initialize_leap_second_table( leapFile ) == ERR )
     zmabend( "initialize_leap_second_table failed" );
   acs_to_tt_and_ut1(ACS_time, ((double) 0.0), &TT, &UT1);
   astroreference_camera_sv_c(urange,vrange,
       TOD_t_SV,TOD_q_SV,SV_t_C,SV_q_C,fu,fv,q,u0,v0,
       kappa,TT,UT1,&gr,&gc,&gout);
   
   sprintf(msgBuf, "gr,gc %d %d\n", gr, gc);
   zifmessage(msgBuf);
   if (gc>4) zmabend("number of data columns exceeds 4");
   
   /* convert grid in gout to ibis format, (lon,lat,elev,l,s) */
   
   mz_alloc2((unsigned char ***)&bufout,gc,gr,8);
   
   for (i=0;i<gr;i++)
      for (j=0;j<gc;j++)
         bufout[j][i] = gout[i*gc+j];
   
   /* Output points to the ibis interface file */
  
   status = zvunit(&unit,"inp",2, NULL);
   status = IBISFileOpen(unit,&ibis,"update",0,0,0,0);
   if (status!=1) IBISSignalU(unit,status,1);
   IBISFileGet(ibis,"nr",&clen,1,1,0);
   
   /* abend on missing output */
   
   if (gr<clen) zmabend("missing output");
   
   for (i=0;i<gc;i++)
      {
      if (status!=1) IBISSignal(ibis,status,1);
      status = IBISColumnWrite(ibis,(char*)bufout[i],cols[i],1,clen);
      if (status!=1) IBISSignal(ibis,status,1);
      }
      
   status = IBISFileClose(ibis,0);
   if (status!=1) IBISSignal(ibis,status,1);
   
   return;
}
