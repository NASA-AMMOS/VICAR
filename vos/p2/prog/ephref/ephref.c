#include <math.h>

#include "vicmain_c.h"
#include "applic.h"
#include "defines.h"
#include "ibisfile.h"

#include "cartoMemUtils.h"

#include "astroreference_camera.h"
#include "time_conversion.h"
#include "ephreference_camera.h"

/*  compute rpc from spacecraft ephemeris, camera model   A. Zobrist    09/21/05   */

void main44(void)
{
   int i,j,urangecount,vrangecount,hrangecount,dummy,cols[5],colcount;
   int TOD_t_SV_count, TOD_q_SV_count;
   int SV_t_C_count, SV_q_C_count;
   int ACS_time_count;
   int   fucount,fvcount,gr,gc,kappacount;
   int qcount,u0count,v0count,status,unit,ibis,clen;
   int calc_mode,calc_case;
   double ACS_time;
   double TT, UT1;
   double **bufout;
   double urange[3],vrange[3],hrange[3];
   double TOD_t_SV[3], TOD_q_SV[4];
   double SV_t_C[3], SV_q_C[4];
   double fu,fv,q,u0,v0;
   double kappa[5];
   double *gout;
   
   zifmessage("ephref version Thu Jan  3 2008");
   
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
   zvp("calc_mode",&calc_mode,&dummy);
   
   /* Open the ibis interface file */
  
      status = zvunit(&unit,"inp",2, NULL);
      status = IBISFileOpen(unit,&ibis,"update",0,0,0,0);
      if (status!=1) IBISSignalU(unit,status,1);
      IBISFileGet(ibis,"nr",&clen,1,1,0);
   
   /* call the function */
   
   /*mz_alloc1((unsigned char **)&gout,100000,8);*/

   acs_to_tt_and_ut1(ACS_time, ((double) 0.0), &TT, &UT1);
   
   ephreference_camera_sv_c(urange, vrange, hrange, 
       TOD_t_SV, TOD_q_SV, SV_t_C, SV_q_C, fu,
       fv, q, u0, v0, kappa, TT, UT1, &gr, &gc, &gout,
       clen, calc_mode,&calc_case);
   
   printf("gr,gc,calc_case %d %d %d\n",gr,gc,calc_case);
   
   if (calc_case==1) /* process earth coordinates */
      {
      /* convert grid in gout to ibis format, (lon,lat,elev,l,s) */
   
      mz_alloc2((unsigned char ***)&bufout,5,gr,8);
   
      for (i=0;i<gr;i++)
         {
         bufout[0][i] = gout[i*gc+3];
         bufout[1][i] = gout[i*gc+2];
         bufout[2][i] = gout[i*gc+4];
         bufout[3][i] = gout[i*gc+1];
         bufout[4][i] = gout[i*gc+0];
         }
   
      /* off Earth case sets an indicator into ibis file */
   
      if (gr<clen) bufout[0][0] = -9999.0;
   
      for (i=0;i<5;i++)
         {
         status = IBISColumnWrite(ibis,(char*)bufout[i],cols[i],1,clen);
         if (status!=1) IBISSignal(ibis,status,1);
         }
      
   status = IBISFileClose(ibis,0);
   if (status!=1) IBISSignal(ibis,status,1);
      }
   else if (calc_case==2)   /* process space coordinates */
      {
      /* convert grid in gout to ibis format, (phi,theta,l,s) */
      
      mz_alloc2((unsigned char ***)&bufout,gc,gr,8);
   
      for (i=0;i<gr;i++)
         for (j=0;j<gc;j++)
            bufout[j][i] = gout[i*gc+j];
   
      /* abend on missing output */
   
      if (gr<clen) zmabend("missing output");
   
      for (i=0;i<gc;i++)
         {
         status = IBISColumnWrite(ibis,(char*)bufout[i],cols[i],1,clen);
         if (status!=1) IBISSignal(ibis,status,1);
         }
      
      status = IBISFileClose(ibis,0);
      if (status!=1) IBISSignal(ibis,status,1);
      }
   
   return;
}
