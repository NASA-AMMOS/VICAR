#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "vicmain_c.h"
#include "applic.h"
#include "ibisfile.h"

#undef HAVE_INLINE
#include "gsl/gsl_math.h"
#include "gsl/gsl_test.h"
#include "gsl/gsl_multifit.h"
#include "gsl/gsl_multifit_nlin.h"
#include "gsl/gsl_blas.h"
#include "gsl/gsl_ieee_utils.h"
#include "gsl/gsl_version.h"

#include "cartoMemUtils.h"

/************************************************************************/
/* program ibisnlsq                                                      */
/************************************************************************/
/*  march 05 ...alz... initial version                     */
/*  Fri Dec 28 2007 wlb switched to USES_ANSI_C AND LIB_CARTO; misc cleanup */
/************************************************************************/

/* the following accessible to non-linear functions */
double **xpar,*clsqdep,oldfsq,diffsq,tguess[100],csol[100],*rout;
int n_ind_var,n_data_pt,solcol[5],n_guess,rpctype,rpc_linsmp;
char func_name[21];

/* the kirby2 routines (for testing) ***********************************/
int
kirby2_f (const gsl_vector * x, void *params, gsl_vector * f)
{
  double b[5];
  size_t i;

  for (i = 0; i < 5; i++)
    {
      b[i] = gsl_vector_get(x, i);
    }

  for (i = 0; i < n_data_pt; i++)
    {
      double x = xpar[0][i];
      double y = ((b[0] + x* (b[1]  + x * b[2]))
                  / (1 + x*(b[3]  + x *b[4])));
      gsl_vector_set (f, i, clsqdep[i] - y);
      rout[i] = clsqdep[i] - y;
    }

  return GSL_SUCCESS;
}

int
kirby2_df (const gsl_vector * x, void *params, gsl_matrix * df)
{
  double b[5];
  size_t i;

  for (i = 0; i < 5; i++)
    {
      b[i] = gsl_vector_get(x, i);
    }

  for (i = 0; i < n_data_pt; i++)
    {
      double x = xpar[0][i];
      double u = (b[0] + x*(b[1] + x*b[2]));
      double v = (1 + x*(b[3] + x*b[4]));
      gsl_matrix_set (df, i, 0, -1/v);
      gsl_matrix_set (df, i, 1, -x/v);
      gsl_matrix_set (df, i, 2, -x*x/v);
      gsl_matrix_set (df, i, 3, x*u/(v*v));
      gsl_matrix_set (df, i, 4, x*x*u/(v*v));
    }

  return GSL_SUCCESS;
}

int
kirby2_fdf (const gsl_vector * x, void *params,
           gsl_vector * f, gsl_matrix * df)
{
  kirby2_f (x, params, f);
  kirby2_df (x, params, df);

  return GSL_SUCCESS;
}

/* the RPF routines ******************************************/

void rpctran(buf1,buf2,offset)
   double *buf1,*buf2;
   int offset;
{
   if (rpctype==0)       /* type RPC00A */
      {
      buf1[0] = buf2[offset];
      buf1[1] = buf2[offset+1];
      buf1[2] = buf2[offset+2];
      buf1[3] = buf2[offset+4];
      buf1[4] = buf2[offset+8];
      buf1[5] = buf2[offset+9];
      buf1[6] = buf2[offset+11];
      buf1[7] = buf2[offset+12];
      buf1[8] = buf2[offset+14];
      buf1[9] = buf2[offset+15];
      }
   else                   /* type RPC00B */
      {
      buf1[0] = buf2[offset];
      buf1[1] = buf2[offset+1];
      buf1[2] = buf2[offset+2];
      buf1[3] = buf2[offset+4];
      buf1[4] = buf2[offset+7];
      buf1[5] = buf2[offset+8];
      buf1[6] = buf2[offset+11];
      buf1[7] = buf2[offset+12];
      buf1[8] = buf2[offset+14];
      buf1[9] = buf2[offset+15];
      }
   return;
}

void rpctran9(buf1,buf2,offset)
   double *buf1,*buf2;
   int offset;
{
   if (rpctype==0)       /* type RPC00A */
      {
      buf1[0] = buf2[offset+1];
      buf1[1] = buf2[offset+2];
      buf1[2] = buf2[offset+4];
      buf1[3] = buf2[offset+8];
      buf1[4] = buf2[offset+9];
      buf1[5] = buf2[offset+11];
      buf1[6] = buf2[offset+12];
      buf1[7] = buf2[offset+14];
      buf1[8] = buf2[offset+15];
      }
   else                   /* type RPC00B */
      {
      buf1[0] = buf2[offset+1];
      buf1[1] = buf2[offset+2];
      buf1[2] = buf2[offset+4];
      buf1[3] = buf2[offset+7];
      buf1[4] = buf2[offset+8];
      buf1[5] = buf2[offset+11];
      buf1[6] = buf2[offset+12];
      buf1[7] = buf2[offset+14];
      buf1[8] = buf2[offset+15];
      }
   return;
}

void rpcztran(buf1,buf2,offset)
   double *buf1,*buf2;
   int offset;
{
   if (rpctype==0)       /* type RPC00A */
      {
      buf1[0] = buf2[offset];
      buf1[1] = buf2[offset+3];
      buf1[2] = buf2[offset+5];
      buf1[3] = buf2[offset+6];
      buf1[4] = buf2[offset+7];
      buf1[5] = buf2[offset+10];
      buf1[6] = buf2[offset+13];
      buf1[7] = buf2[offset+16];
      buf1[8] = buf2[offset+17];
      buf1[9] = buf2[offset+18];
      buf1[10] = buf2[offset+19];
      }
   else                   /* type RPC00B */
      {
      buf1[0] = buf2[offset];
      buf1[1] = buf2[offset+3];
      buf1[2] = buf2[offset+5];
      buf1[3] = buf2[offset+6];
      buf1[4] = buf2[offset+9];
      buf1[5] = buf2[offset+10];
      buf1[6] = buf2[offset+13];
      buf1[7] = buf2[offset+16];
      buf1[8] = buf2[offset+17];
      buf1[9] = buf2[offset+18];
      buf1[10] = buf2[offset+19];
      }
   return;
}

void rpc2tran(buf1,buf2,offset)
   double *buf1,*buf2;
   int offset;
{
   if (rpctype==0)       /* type RPC00A */
      {
      buf1[0] = buf2[offset];
      buf1[1] = buf2[offset+3];
      }
   else                   /* type RPC00B */
      {
      buf1[0] = buf2[offset];
      buf1[1] = buf2[offset+3];
      }
   return;
}

void rpctranb(buf1,buf2,offset)
   double *buf1,*buf2;
   int offset;
{
   if (rpctype==0)       /* type RPC00A */
      {
      buf1[offset] = buf2[0];
      buf1[offset+1] = buf2[1];
      buf1[offset+2] = buf2[2];
      buf1[offset+4] = buf2[3];
      buf1[offset+8] = buf2[4];
      buf1[offset+9] = buf2[5];
      buf1[offset+11] = buf2[6];
      buf1[offset+12] = buf2[7];
      buf1[offset+14] = buf2[8];
      buf1[offset+15] = buf2[9];
      }
   else                   /* type RPC00B */
      {
      buf1[offset] = buf2[0];
      buf1[offset+1] = buf2[1];
      buf1[offset+2] = buf2[2];
      buf1[offset+4] = buf2[3];
      buf1[offset+7] = buf2[4];
      buf1[offset+8] = buf2[5];
      buf1[offset+11] = buf2[6];
      buf1[offset+12] = buf2[7];
      buf1[offset+14] = buf2[8];
      buf1[offset+15] = buf2[9];
      }
   return;
}

void rpctranb9(buf1,buf2,offset)
   double *buf1,*buf2;
   int offset;
{
   if (rpctype==0)       /* type RPC00A */
      {
      buf1[offset+1] = buf2[0];
      buf1[offset+2] = buf2[1];
      buf1[offset+4] = buf2[2];
      buf1[offset+8] = buf2[3];
      buf1[offset+9] = buf2[4];
      buf1[offset+11] = buf2[5];
      buf1[offset+12] = buf2[6];
      buf1[offset+14] = buf2[7];
      buf1[offset+15] = buf2[8];
      }
   else                   /* type RPC00B */
      {
      buf1[offset+1] = buf2[0];
      buf1[offset+2] = buf2[1];
      buf1[offset+4] = buf2[2];
      buf1[offset+7] = buf2[3];
      buf1[offset+8] = buf2[4];
      buf1[offset+11] = buf2[5];
      buf1[offset+12] = buf2[6];
      buf1[offset+14] = buf2[7];
      buf1[offset+15] = buf2[8];
      }
   return;
}

void rpcztranb(buf1,buf2,offset)
   double *buf1,*buf2;
   int offset;
{
   if (rpctype==0)       /* type RPC00A */
      {
      buf1[offset] = buf2[0];
      buf1[offset+3] = buf2[1];
      buf1[offset+5] = buf2[2];
      buf1[offset+6] = buf2[3];
      buf1[offset+7] = buf2[4];
      buf1[offset+10] = buf2[5];
      buf1[offset+13] = buf2[6];
      buf1[offset+16] = buf2[7];
      buf1[offset+17] = buf2[8];
      buf1[offset+18] = buf2[9];
      buf1[offset+19] = buf2[10];
      }
   else                   /* type RPC00B */
      {
      buf1[offset] = buf2[0];
      buf1[offset+3] = buf2[1];
      buf1[offset+5] = buf2[2];
      buf1[offset+6] = buf2[3];
      buf1[offset+9] = buf2[4];
      buf1[offset+10] = buf2[5];
      buf1[offset+13] = buf2[6];
      buf1[offset+16] = buf2[7];
      buf1[offset+17] = buf2[8];
      buf1[offset+18] = buf2[9];
      buf1[offset+19] = buf2[10];
      }
   return;
}

void rpc2tranb(buf1,buf2,offset)
   double *buf1,*buf2;
   int offset;
{
   if (rpctype==0)       /* type RPC00A */
      {
      buf1[offset] = buf2[0];
      buf1[offset+3] = buf2[1];
      }
   else                   /* type RPC00B */
      {
      buf1[offset] = buf2[0];
      buf1[offset+3] = buf2[1];
      }
   return;
}

int
rpc_f (const gsl_vector * x, void *params, gsl_vector * f)
{
  double rpcn[20],rpcd[20],l,p,h,l2,p2,h2,l3,p3,h3,numer,denom,y;
  size_t i;

   /* insert the solved parameters into the tguess and csol */

   if (strcmp(func_name,"RPCD20")==0) for (i=0;i<20;i++)
      {
      csol[i] = gsl_vector_get (x,i);
      tguess[i+20+rpc_linsmp*40] = csol[i];
      }
   if (strcmp(func_name,"RPCD19")==0) for (i=0;i<19;i++)
      {
      csol[i] = gsl_vector_get (x,i);
      tguess[i+21+rpc_linsmp*40] = csol[i];
      }
   if (strcmp(func_name,"RPCN20")==0) for (i=0;i<20;i++)
      {
      csol[i] = gsl_vector_get (x,i);
      tguess[i+rpc_linsmp*40] = csol[i];
      }
   if (strcmp(func_name,"RPCD10")==0)
      {
      for (i=0;i<10;i++) csol[i] = gsl_vector_get (x,i);
      rpctranb(tguess,csol,20+rpc_linsmp*40);
      }
   if (strcmp(func_name,"RPCD9")==0)
      {
      for (i=0;i<9;i++) csol[i] = gsl_vector_get (x,i);
      rpctranb9(tguess,csol,20+rpc_linsmp*40);
      }
   if (strcmp(func_name,"RPCN10")==0)
      {
      for (i=0;i<10;i++) csol[i] = gsl_vector_get (x,i);
      rpctranb(tguess,csol,rpc_linsmp*40);
      }
   if (strcmp(func_name,"RPC39")==0)
      {
      for (i=0;i<20;i++)
         {
         csol[i] = gsl_vector_get (x,i);
         tguess[i+rpc_linsmp*40] = csol[i];
         }
      for (i=21;i<40;i++)
         {
         csol[i-1] = gsl_vector_get (x,i);
         tguess[i+rpc_linsmp*40-1] = csol[i-1];
         }
      }
   if (strcmp(func_name,"RPCTEST")==0) for (i=0;i<1;i++)
      {
      csol[i] = gsl_vector_get (x,i);
      tguess[i+rpc_linsmp*40] = csol[i];
      }
   if (strcmp(func_name,"RPCDZT")==0)
      {
      for (i=0;i<11;i++) csol[i] = gsl_vector_get (x,i);
      rpcztranb(tguess,csol,20+rpc_linsmp*40);
      }
   if (strcmp(func_name,"RPCNZT")==0)
      {
      for (i=0;i<11;i++) csol[i] = gsl_vector_get (x,i);
      rpcztranb(tguess,csol,rpc_linsmp*40);
      }
   if (strcmp(func_name,"RPCDZ2")==0)
      {
      for (i=0;i<2;i++) csol[i] = gsl_vector_get (x,i);
      rpc2tranb(tguess,csol,20+rpc_linsmp*40);
      }
   if (strcmp(func_name,"RPCNZ2")==0)
      {
      for (i=0;i<2;i++) csol[i] = gsl_vector_get (x,i);
      rpc2tranb(tguess,csol,rpc_linsmp*40);
      }
   
  for (i=0;i<20;i++) rpcn[i] = tguess[i+rpc_linsmp*40];
  for (i=0;i<20;i++) rpcd[i] = tguess[i+20+rpc_linsmp*40];

  for (i=0;i<n_data_pt;i++)
    {
    l = xpar[0][i];
    p = xpar[1][i];
    h = xpar[2][i];
    l2 = l*l; l3 = l2*l;
    p2 = p*p; p3 = p2*p;
    h2 = h*h; h3 = h2*h;
    
    if (rpctype==0)       /* type RPC00A */
       {
       numer = rpcn[0]+rpcn[1]*l+rpcn[2]*p+rpcn[3]*h+
       rpcn[4]*l*p+rpcn[5]*l*h+rpcn[6]*p*h+rpcn[7]*p*l*h+
       rpcn[8]*l2+rpcn[9]*p2+rpcn[10]*h2+rpcn[11]*l3+
       rpcn[12]*l2*p+rpcn[13]*l2*h+rpcn[14]*l*p2+
       rpcn[15]*p3+rpcn[16]*p2*h+rpcn[17]*l*h2+
       rpcn[18]*p*h2+rpcn[19]*h3;
       denom = rpcd[0]+rpcd[1]*l+rpcd[2]*p+rpcd[3]*h+
       rpcd[4]*l*p+rpcd[5]*l*h+rpcd[6]*p*h+rpcd[7]*p*l*h+
       rpcd[8]*l2+rpcd[9]*p2+rpcd[10]*h2+rpcd[11]*l3+
       rpcd[12]*l2*p+rpcd[13]*l2*h+rpcd[14]*l*p2+
       rpcd[15]*p3+rpcd[16]*p2*h+rpcd[17]*l*h2+
       rpcd[18]*p*h2+rpcd[19]*h3;
       }
    else                   /* type RPC00B */
       {
       numer = rpcn[0]+rpcn[1]*l+rpcn[2]*p+rpcn[3]*h+
       rpcn[4]*l*p+rpcn[5]*l*h+rpcn[6]*p*h+rpcn[7]*l2+
       rpcn[8]*p2+rpcn[9]*h2+rpcn[10]*p*l*h+rpcn[11]*l3+
       rpcn[12]*l*p2+rpcn[13]*l*h2+rpcn[14]*l2*p+
       rpcn[15]*p3+rpcn[16]*p*h2+rpcn[17]*l2*h+
       rpcn[18]*p2*h+rpcn[19]*h3;
       denom = rpcd[0]+rpcd[1]*l+rpcd[2]*p+rpcd[3]*h+
       rpcd[4]*l*p+rpcd[5]*l*h+rpcd[6]*p*h+rpcd[7]*l2+
       rpcd[8]*p2+rpcd[9]*h2+rpcd[10]*p*l*h+rpcd[11]*l3+
       rpcd[12]*l*p2+rpcd[13]*l*h2+rpcd[14]*l2*p+
       rpcd[15]*p3+rpcd[16]*p*h2+rpcd[17]*l2*h+
       rpcd[18]*p2*h+rpcd[19]*h3;
       }

    y = numer/denom;
    gsl_vector_set (f, i, clsqdep[i] - y);
    rout[i] = clsqdep[i] - y;
    }
  
  return GSL_SUCCESS;
}

int
rpcd20_df (const gsl_vector * x, void *params, gsl_matrix * df)
{
  double rpcn[20],rpcd[20],l,p,h,l2,p2,h2,l3,p3,h3,numer,denom,ch;
  size_t i;

  for (i=0;i<20;i++) rpcn[i] = tguess[i+rpc_linsmp*40];
  for (i=0;i<20;i++) rpcd[i] = tguess[i+20+rpc_linsmp*40];

  for (i=0;i<n_data_pt;i++)
    {
    l = xpar[0][i];
    p = xpar[1][i];
    h = xpar[2][i];
    l2 = l*l; l3 = l2*l;
    p2 = p*p; p3 = p2*p;
    h2 = h*h; h3 = h2*h;

    if (rpctype==0)       /* type RPC00A */
       {
       numer = rpcn[0]+rpcn[1]*l+rpcn[2]*p+rpcn[3]*h+
       rpcn[4]*l*p+rpcn[5]*l*h+rpcn[6]*p*h+rpcn[7]*p*l*h+
       rpcn[8]*l2+rpcn[9]*p2+rpcn[10]*h2+rpcn[11]*l3+
       rpcn[12]*l2*p+rpcn[13]*l2*h+rpcn[14]*l*p2+
       rpcn[15]*p3+rpcn[16]*p2*h+rpcn[17]*l*h2+
       rpcn[18]*p*h2+rpcn[19]*h3;
       denom = rpcd[0]+rpcd[1]*l+rpcd[2]*p+rpcd[3]*h+
       rpcd[4]*l*p+rpcd[5]*l*h+rpcd[6]*p*h+rpcd[7]*p*l*h+
       rpcd[8]*l2+rpcd[9]*p2+rpcd[10]*h2+rpcd[11]*l3+
       rpcd[12]*l2*p+rpcd[13]*l2*h+rpcd[14]*l*p2+
       rpcd[15]*p3+rpcd[16]*p2*h+rpcd[17]*l*h2+
       rpcd[18]*p*h2+rpcd[19]*h3;
       }
    else                   /* type RPC00B */
       {
       numer = rpcn[0]+rpcn[1]*l+rpcn[2]*p+rpcn[3]*h+
       rpcn[4]*l*p+rpcn[5]*l*h+rpcn[6]*p*h+rpcn[7]*l2+
       rpcn[8]*p2+rpcn[9]*h2+rpcn[10]*p*l*h+rpcn[11]*l3+
       rpcn[12]*l*p2+rpcn[13]*l*h2+rpcn[14]*l2*p+
       rpcn[15]*p3+rpcn[16]*p*h2+rpcn[17]*l2*h+
       rpcn[18]*p2*h+rpcn[19]*h3;
       denom = rpcd[0]+rpcd[1]*l+rpcd[2]*p+rpcd[3]*h+
       rpcd[4]*l*p+rpcd[5]*l*h+rpcd[6]*p*h+rpcd[7]*l2+
       rpcd[8]*p2+rpcd[9]*h2+rpcd[10]*p*l*h+rpcd[11]*l3+
       rpcd[12]*l*p2+rpcd[13]*l*h2+rpcd[14]*l2*p+
       rpcd[15]*p3+rpcd[16]*p*h2+rpcd[17]*l2*h+
       rpcd[18]*p2*h+rpcd[19]*h3;
       }

    if (rpctype==0)       /* type RPC00A */
       {
       ch = numer/(denom*denom);
       gsl_matrix_set (df, i, 0, ch);
       gsl_matrix_set (df, i, 1, ch*l);
       gsl_matrix_set (df, i, 2, ch*p);
       gsl_matrix_set (df, i, 3, ch*h);
       gsl_matrix_set (df, i, 4, ch*l*p);
       gsl_matrix_set (df, i, 5, ch*l*h);
       gsl_matrix_set (df, i, 6, ch*p*h);
       gsl_matrix_set (df, i, 7, ch*p*l*h);
       gsl_matrix_set (df, i, 8, ch*l2);
       gsl_matrix_set (df, i, 9, ch*p2);
       gsl_matrix_set (df, i,10, ch*h2);
       gsl_matrix_set (df, i,11, ch*l3);
       gsl_matrix_set (df, i,12, ch*l2*p);
       gsl_matrix_set (df, i,13, ch*l2*h);
       gsl_matrix_set (df, i,14, ch*l*p2);
       gsl_matrix_set (df, i,15, ch*p3);
       gsl_matrix_set (df, i,16, ch*p2*h);
       gsl_matrix_set (df, i,17, ch*l*h2);
       gsl_matrix_set (df, i,18, ch*p*h2);
       gsl_matrix_set (df, i,19, ch*h3);
       }
    else                   /* type RPC00B */
       {
       ch = numer/(denom*denom);
       gsl_matrix_set (df, i, 0, ch);
       gsl_matrix_set (df, i, 1, ch*l);
       gsl_matrix_set (df, i, 2, ch*p);
       gsl_matrix_set (df, i, 3, ch*h);
       gsl_matrix_set (df, i, 4, ch*l*p);
       gsl_matrix_set (df, i, 5, ch*l*h);
       gsl_matrix_set (df, i, 6, ch*p*h);
       gsl_matrix_set (df, i, 7, ch*l2);
       gsl_matrix_set (df, i, 8, ch*p2);
       gsl_matrix_set (df, i, 9, ch*h2);
       gsl_matrix_set (df, i,10, ch*p*l*h);
       gsl_matrix_set (df, i,11, ch*l3);
       gsl_matrix_set (df, i,12, ch*l*p2);
       gsl_matrix_set (df, i,13, ch*l*h2);
       gsl_matrix_set (df, i,14, ch*l2*p);
       gsl_matrix_set (df, i,15, ch*p3);
       gsl_matrix_set (df, i,16, ch*p*h2);
       gsl_matrix_set (df, i,17, ch*l2*h);
       gsl_matrix_set (df, i,18, ch*p2*h);
       gsl_matrix_set (df, i,19, ch*h3);
       }
    }
  return GSL_SUCCESS;
}

int
rpcd20_fdf (const gsl_vector * x, void *params,
           gsl_vector * f, gsl_matrix * df)
{
  rpc_f (x, params, f);
  rpcd20_df (x, params, df);

  return GSL_SUCCESS;
}

int
rpcd19_df (const gsl_vector * x, void *params, gsl_matrix * df)
{
  double rpcn[20],rpcd[20],l,p,h,l2,p2,h2,l3,p3,h3,numer,denom,ch;
  size_t i;

  for (i=0;i<20;i++) rpcn[i] = tguess[i+rpc_linsmp*40];
  for (i=0;i<20;i++) rpcd[i] = tguess[i+20+rpc_linsmp*40];

  for (i=0;i<n_data_pt;i++)
    {
    l = xpar[0][i];
    p = xpar[1][i];
    h = xpar[2][i];
    l2 = l*l; l3 = l2*l;
    p2 = p*p; p3 = p2*p;
    h2 = h*h; h3 = h2*h;

    if (rpctype==0)       /* type RPC00A */
       {
       numer = rpcn[0]+rpcn[1]*l+rpcn[2]*p+rpcn[3]*h+
       rpcn[4]*l*p+rpcn[5]*l*h+rpcn[6]*p*h+rpcn[7]*p*l*h+
       rpcn[8]*l2+rpcn[9]*p2+rpcn[10]*h2+rpcn[11]*l3+
       rpcn[12]*l2*p+rpcn[13]*l2*h+rpcn[14]*l*p2+
       rpcn[15]*p3+rpcn[16]*p2*h+rpcn[17]*l*h2+
       rpcn[18]*p*h2+rpcn[19]*h3;
       denom = rpcd[0]+rpcd[1]*l+rpcd[2]*p+rpcd[3]*h+
       rpcd[4]*l*p+rpcd[5]*l*h+rpcd[6]*p*h+rpcd[7]*p*l*h+
       rpcd[8]*l2+rpcd[9]*p2+rpcd[10]*h2+rpcd[11]*l3+
       rpcd[12]*l2*p+rpcd[13]*l2*h+rpcd[14]*l*p2+
       rpcd[15]*p3+rpcd[16]*p2*h+rpcd[17]*l*h2+
       rpcd[18]*p*h2+rpcd[19]*h3;
       }
    else                   /* type RPC00B */
       {
       numer = rpcn[0]+rpcn[1]*l+rpcn[2]*p+rpcn[3]*h+
       rpcn[4]*l*p+rpcn[5]*l*h+rpcn[6]*p*h+rpcn[7]*l2+
       rpcn[8]*p2+rpcn[9]*h2+rpcn[10]*p*l*h+rpcn[11]*l3+
       rpcn[12]*l*p2+rpcn[13]*l*h2+rpcn[14]*l2*p+
       rpcn[15]*p3+rpcn[16]*p*h2+rpcn[17]*l2*h+
       rpcn[18]*p2*h+rpcn[19]*h3;
       denom = rpcd[0]+rpcd[1]*l+rpcd[2]*p+rpcd[3]*h+
       rpcd[4]*l*p+rpcd[5]*l*h+rpcd[6]*p*h+rpcd[7]*l2+
       rpcd[8]*p2+rpcd[9]*h2+rpcd[10]*p*l*h+rpcd[11]*l3+
       rpcd[12]*l*p2+rpcd[13]*l*h2+rpcd[14]*l2*p+
       rpcd[15]*p3+rpcd[16]*p*h2+rpcd[17]*l2*h+
       rpcd[18]*p2*h+rpcd[19]*h3;
       }

    if (rpctype==0)       /* type RPC00A */
       {
       ch = numer/(denom*denom);
       gsl_matrix_set (df, i, 0, ch*l);
       gsl_matrix_set (df, i, 1, ch*p);
       gsl_matrix_set (df, i, 2, ch*h);
       gsl_matrix_set (df, i, 3, ch*l*p);
       gsl_matrix_set (df, i, 4, ch*l*h);
       gsl_matrix_set (df, i, 5, ch*p*h);
       gsl_matrix_set (df, i, 6, ch*p*l*h);
       gsl_matrix_set (df, i, 7, ch*l2);
       gsl_matrix_set (df, i, 8, ch*p2);
       gsl_matrix_set (df, i, 9, ch*h2);
       gsl_matrix_set (df, i,10, ch*l3);
       gsl_matrix_set (df, i,11, ch*l2*p);
       gsl_matrix_set (df, i,12, ch*l2*h);
       gsl_matrix_set (df, i,13, ch*l*p2);
       gsl_matrix_set (df, i,14, ch*p3);
       gsl_matrix_set (df, i,15, ch*p2*h);
       gsl_matrix_set (df, i,16, ch*l*h2);
       gsl_matrix_set (df, i,17, ch*p*h2);
       gsl_matrix_set (df, i,18, ch*h3);
       }
    else                   /* type RPC00B */
       {
       ch = numer/(denom*denom);
       gsl_matrix_set (df, i, 0, ch*l);
       gsl_matrix_set (df, i, 1, ch*p);
       gsl_matrix_set (df, i, 2, ch*h);
       gsl_matrix_set (df, i, 3, ch*l*p);
       gsl_matrix_set (df, i, 4, ch*l*h);
       gsl_matrix_set (df, i, 5, ch*p*h);
       gsl_matrix_set (df, i, 6, ch*l2);
       gsl_matrix_set (df, i, 7, ch*p2);
       gsl_matrix_set (df, i, 8, ch*h2);
       gsl_matrix_set (df, i, 9, ch*p*l*h);
       gsl_matrix_set (df, i,10, ch*l3);
       gsl_matrix_set (df, i,11, ch*l*p2);
       gsl_matrix_set (df, i,12, ch*l*h2);
       gsl_matrix_set (df, i,13, ch*l2*p);
       gsl_matrix_set (df, i,14, ch*p3);
       gsl_matrix_set (df, i,15, ch*p*h2);
       gsl_matrix_set (df, i,16, ch*l2*h);
       gsl_matrix_set (df, i,17, ch*p2*h);
       gsl_matrix_set (df, i,18, ch*h3);
       }
    }
  return GSL_SUCCESS;
}

int
rpcd19_fdf (const gsl_vector * x, void *params,
           gsl_vector * f, gsl_matrix * df)
{
  rpc_f (x, params, f);
  rpcd19_df (x, params, df);

  return GSL_SUCCESS;
}

int
rpcn20_df (const gsl_vector * x, void *params, gsl_matrix * df)
{
  double rpcn[20],rpcd[20],l,p,h,l2,p2,h2,l3,p3,h3,numer,denom,ch;
  size_t i;

  for (i=0;i<20;i++) rpcn[i] = tguess[i+rpc_linsmp*40];
  for (i=0;i<20;i++) rpcd[i] = tguess[i+20+rpc_linsmp*40];

  for (i=0;i<n_data_pt;i++)
    {
    l = xpar[0][i];
    p = xpar[1][i];
    h = xpar[2][i];
    l2 = l*l; l3 = l2*l;
    p2 = p*p; p3 = p2*p;
    h2 = h*h; h3 = h2*h;

    if (rpctype==0)       /* type RPC00A */
       {
       numer = rpcn[0]+rpcn[1]*l+rpcn[2]*p+rpcn[3]*h+
       rpcn[4]*l*p+rpcn[5]*l*h+rpcn[6]*p*h+rpcn[7]*p*l*h+
       rpcn[8]*l2+rpcn[9]*p2+rpcn[10]*h2+rpcn[11]*l3+
       rpcn[12]*l2*p+rpcn[13]*l2*h+rpcn[14]*l*p2+
       rpcn[15]*p3+rpcn[16]*p2*h+rpcn[17]*l*h2+
       rpcn[18]*p*h2+rpcn[19]*h3;
       denom = rpcd[0]+rpcd[1]*l+rpcd[2]*p+rpcd[3]*h+
       rpcd[4]*l*p+rpcd[5]*l*h+rpcd[6]*p*h+rpcd[7]*p*l*h+
       rpcd[8]*l2+rpcd[9]*p2+rpcd[10]*h2+rpcd[11]*l3+
       rpcd[12]*l2*p+rpcd[13]*l2*h+rpcd[14]*l*p2+
       rpcd[15]*p3+rpcd[16]*p2*h+rpcd[17]*l*h2+
       rpcd[18]*p*h2+rpcd[19]*h3;
       }
    else                   /* type RPC00B */
       {
       numer = rpcn[0]+rpcn[1]*l+rpcn[2]*p+rpcn[3]*h+
       rpcn[4]*l*p+rpcn[5]*l*h+rpcn[6]*p*h+rpcn[7]*l2+
       rpcn[8]*p2+rpcn[9]*h2+rpcn[10]*p*l*h+rpcn[11]*l3+
       rpcn[12]*l*p2+rpcn[13]*l*h2+rpcn[14]*l2*p+
       rpcn[15]*p3+rpcn[16]*p*h2+rpcn[17]*l2*h+
       rpcn[18]*p2*h+rpcn[19]*h3;
       denom = rpcd[0]+rpcd[1]*l+rpcd[2]*p+rpcd[3]*h+
       rpcd[4]*l*p+rpcd[5]*l*h+rpcd[6]*p*h+rpcd[7]*l2+
       rpcd[8]*p2+rpcd[9]*h2+rpcd[10]*p*l*h+rpcd[11]*l3+
       rpcd[12]*l*p2+rpcd[13]*l*h2+rpcd[14]*l2*p+
       rpcd[15]*p3+rpcd[16]*p*h2+rpcd[17]*l2*h+
       rpcd[18]*p2*h+rpcd[19]*h3;
       }

    if (rpctype==0)       /* type RPC00A */
       {
       ch = -1.0/denom;
       gsl_matrix_set (df, i, 0, ch);
       gsl_matrix_set (df, i, 1, ch*l);
       gsl_matrix_set (df, i, 2, ch*p);
       gsl_matrix_set (df, i, 3, ch*h);
       gsl_matrix_set (df, i, 4, ch*l*p);
       gsl_matrix_set (df, i, 5, ch*l*h);
       gsl_matrix_set (df, i, 6, ch*p*h);
       gsl_matrix_set (df, i, 7, ch*p*l*h);
       gsl_matrix_set (df, i, 8, ch*l2);
       gsl_matrix_set (df, i, 9, ch*p2);
       gsl_matrix_set (df, i,10, ch*h2);
       gsl_matrix_set (df, i,11, ch*l3);
       gsl_matrix_set (df, i,12, ch*l2*p);
       gsl_matrix_set (df, i,13, ch*l2*h);
       gsl_matrix_set (df, i,14, ch*l*p2);
       gsl_matrix_set (df, i,15, ch*p3);
       gsl_matrix_set (df, i,16, ch*p2*h);
       gsl_matrix_set (df, i,17, ch*l*h2);
       gsl_matrix_set (df, i,18, ch*p*h2);
       gsl_matrix_set (df, i,19, ch*h3);
       }
    else                   /* type RPC00B */
       {
       ch = -1.0/denom;
       gsl_matrix_set (df, i, 0, ch);
       gsl_matrix_set (df, i, 1, ch*l);
       gsl_matrix_set (df, i, 2, ch*p);
       gsl_matrix_set (df, i, 3, ch*h);
       gsl_matrix_set (df, i, 4, ch*l*p);
       gsl_matrix_set (df, i, 5, ch*l*h);
       gsl_matrix_set (df, i, 6, ch*p*h);
       gsl_matrix_set (df, i, 7, ch*l2);
       gsl_matrix_set (df, i, 8, ch*p2);
       gsl_matrix_set (df, i, 9, ch*h2);
       gsl_matrix_set (df, i,10, ch*p*l*h);
       gsl_matrix_set (df, i,11, ch*l3);
       gsl_matrix_set (df, i,12, ch*l*p2);
       gsl_matrix_set (df, i,13, ch*l*h2);
       gsl_matrix_set (df, i,14, ch*l2*p);
       gsl_matrix_set (df, i,15, ch*p3);
       gsl_matrix_set (df, i,16, ch*p*h2);
       gsl_matrix_set (df, i,17, ch*l2*h);
       gsl_matrix_set (df, i,18, ch*p2*h);
       gsl_matrix_set (df, i,19, ch*h3);
       }
    }

  return GSL_SUCCESS;
}

int
rpcn20_fdf (const gsl_vector * x, void *params,
           gsl_vector * f, gsl_matrix * df)
{
  rpc_f (x, params, f);
  rpcn20_df (x, params, df);

  return GSL_SUCCESS;
}

int
rpcd10_df (const gsl_vector * x, void *params, gsl_matrix * df)
{
  double rpcn[20],rpcd[20],l,p,h,l2,p2,h2,l3,p3,h3,numer,denom,ch;
  size_t i;

  for (i=0;i<20;i++) rpcn[i] = tguess[i+rpc_linsmp*40];
  for (i=0;i<20;i++) rpcd[i] = tguess[i+20+rpc_linsmp*40];

  for (i=0;i<n_data_pt;i++)
    {
    l = xpar[0][i];
    p = xpar[1][i];
    h = xpar[2][i];
    l2 = l*l; l3 = l2*l;
    p2 = p*p; p3 = p2*p;
    h2 = h*h; h3 = h2*h;

    if (rpctype==0)       /* type RPC00A */
       {
       numer = rpcn[0]+rpcn[1]*l+rpcn[2]*p+rpcn[3]*h+
       rpcn[4]*l*p+rpcn[5]*l*h+rpcn[6]*p*h+rpcn[7]*p*l*h+
       rpcn[8]*l2+rpcn[9]*p2+rpcn[10]*h2+rpcn[11]*l3+
       rpcn[12]*l2*p+rpcn[13]*l2*h+rpcn[14]*l*p2+
       rpcn[15]*p3+rpcn[16]*p2*h+rpcn[17]*l*h2+
       rpcn[18]*p*h2+rpcn[19]*h3;
       denom = rpcd[0]+rpcd[1]*l+rpcd[2]*p+rpcd[3]*h+
       rpcd[4]*l*p+rpcd[5]*l*h+rpcd[6]*p*h+rpcd[7]*p*l*h+
       rpcd[8]*l2+rpcd[9]*p2+rpcd[10]*h2+rpcd[11]*l3+
       rpcd[12]*l2*p+rpcd[13]*l2*h+rpcd[14]*l*p2+
       rpcd[15]*p3+rpcd[16]*p2*h+rpcd[17]*l*h2+
       rpcd[18]*p*h2+rpcd[19]*h3;
       }
    else                   /* type RPC00B */
       {
       numer = rpcn[0]+rpcn[1]*l+rpcn[2]*p+rpcn[3]*h+
       rpcn[4]*l*p+rpcn[5]*l*h+rpcn[6]*p*h+rpcn[7]*l2+
       rpcn[8]*p2+rpcn[9]*h2+rpcn[10]*p*l*h+rpcn[11]*l3+
       rpcn[12]*l*p2+rpcn[13]*l*h2+rpcn[14]*l2*p+
       rpcn[15]*p3+rpcn[16]*p*h2+rpcn[17]*l2*h+
       rpcn[18]*p2*h+rpcn[19]*h3;
       denom = rpcd[0]+rpcd[1]*l+rpcd[2]*p+rpcd[3]*h+
       rpcd[4]*l*p+rpcd[5]*l*h+rpcd[6]*p*h+rpcd[7]*l2+
       rpcd[8]*p2+rpcd[9]*h2+rpcd[10]*p*l*h+rpcd[11]*l3+
       rpcd[12]*l*p2+rpcd[13]*l*h2+rpcd[14]*l2*p+
       rpcd[15]*p3+rpcd[16]*p*h2+rpcd[17]*l2*h+
       rpcd[18]*p2*h+rpcd[19]*h3;
       }

    if (rpctype==0)       /* type RPC00A */
       {
       ch = numer/(denom*denom);
       gsl_matrix_set (df, i, 0, ch);
       gsl_matrix_set (df, i, 1, ch*l);
       gsl_matrix_set (df, i, 2, ch*p);
       gsl_matrix_set (df, i, 3, ch*l*p);
       gsl_matrix_set (df, i, 4, ch*l2);
       gsl_matrix_set (df, i, 5, ch*p2);
       gsl_matrix_set (df, i, 6, ch*l3);
       gsl_matrix_set (df, i, 7, ch*l2*p);
       gsl_matrix_set (df, i, 8, ch*l*p2);
       gsl_matrix_set (df, i, 9, ch*p3);
       }
    else                   /* type RPC00B */
       {
       ch = numer/(denom*denom);
       gsl_matrix_set (df, i, 0, ch);
       gsl_matrix_set (df, i, 1, ch*l);
       gsl_matrix_set (df, i, 2, ch*p);
       gsl_matrix_set (df, i, 3, ch*l*p);
       gsl_matrix_set (df, i, 4, ch*l2);
       gsl_matrix_set (df, i, 5, ch*p2);
       gsl_matrix_set (df, i, 6, ch*l3);
       gsl_matrix_set (df, i, 7, ch*l*p2);
       gsl_matrix_set (df, i, 8, ch*l2*p);
       gsl_matrix_set (df, i, 9, ch*p3);
       }
    }

  return GSL_SUCCESS;
}

int
rpcd10_fdf (const gsl_vector * x, void *params,
           gsl_vector * f, gsl_matrix * df)
{
  rpc_f (x, params, f);
  rpcd10_df (x, params, df);

  return GSL_SUCCESS;
}

int
rpcd9_df (const gsl_vector * x, void *params, gsl_matrix * df)
{
  double rpcn[20],rpcd[20],l,p,h,l2,p2,h2,l3,p3,h3,numer,denom,ch;
  size_t i;

  for (i=0;i<20;i++) rpcn[i] = tguess[i+rpc_linsmp*40];
  for (i=0;i<20;i++) rpcd[i] = tguess[i+20+rpc_linsmp*40];

  for (i=0;i<n_data_pt;i++)
    {
    l = xpar[0][i];
    p = xpar[1][i];
    h = xpar[2][i];
    l2 = l*l; l3 = l2*l;
    p2 = p*p; p3 = p2*p;
    h2 = h*h; h3 = h2*h;

    if (rpctype==0)       /* type RPC00A */
       {
       numer = rpcn[0]+rpcn[1]*l+rpcn[2]*p+rpcn[3]*h+
       rpcn[4]*l*p+rpcn[5]*l*h+rpcn[6]*p*h+rpcn[7]*p*l*h+
       rpcn[8]*l2+rpcn[9]*p2+rpcn[10]*h2+rpcn[11]*l3+
       rpcn[12]*l2*p+rpcn[13]*l2*h+rpcn[14]*l*p2+
       rpcn[15]*p3+rpcn[16]*p2*h+rpcn[17]*l*h2+
       rpcn[18]*p*h2+rpcn[19]*h3;
       denom = rpcd[0]+rpcd[1]*l+rpcd[2]*p+rpcd[3]*h+
       rpcd[4]*l*p+rpcd[5]*l*h+rpcd[6]*p*h+rpcd[7]*p*l*h+
       rpcd[8]*l2+rpcd[9]*p2+rpcd[10]*h2+rpcd[11]*l3+
       rpcd[12]*l2*p+rpcd[13]*l2*h+rpcd[14]*l*p2+
       rpcd[15]*p3+rpcd[16]*p2*h+rpcd[17]*l*h2+
       rpcd[18]*p*h2+rpcd[19]*h3;
       }
    else                   /* type RPC00B */
       {
       numer = rpcn[0]+rpcn[1]*l+rpcn[2]*p+rpcn[3]*h+
       rpcn[4]*l*p+rpcn[5]*l*h+rpcn[6]*p*h+rpcn[7]*l2+
       rpcn[8]*p2+rpcn[9]*h2+rpcn[10]*p*l*h+rpcn[11]*l3+
       rpcn[12]*l*p2+rpcn[13]*l*h2+rpcn[14]*l2*p+
       rpcn[15]*p3+rpcn[16]*p*h2+rpcn[17]*l2*h+
       rpcn[18]*p2*h+rpcn[19]*h3;
       denom = rpcd[0]+rpcd[1]*l+rpcd[2]*p+rpcd[3]*h+
       rpcd[4]*l*p+rpcd[5]*l*h+rpcd[6]*p*h+rpcd[7]*l2+
       rpcd[8]*p2+rpcd[9]*h2+rpcd[10]*p*l*h+rpcd[11]*l3+
       rpcd[12]*l*p2+rpcd[13]*l*h2+rpcd[14]*l2*p+
       rpcd[15]*p3+rpcd[16]*p*h2+rpcd[17]*l2*h+
       rpcd[18]*p2*h+rpcd[19]*h3;
       }

    if (rpctype==0)       /* type RPC00A */
       {
       ch = numer/(denom*denom);
       gsl_matrix_set (df, i, 0, ch*l);
       gsl_matrix_set (df, i, 1, ch*p);
       gsl_matrix_set (df, i, 2, ch*l*p);
       gsl_matrix_set (df, i, 3, ch*l2);
       gsl_matrix_set (df, i, 4, ch*p2);
       gsl_matrix_set (df, i, 5, ch*l3);
       gsl_matrix_set (df, i, 6, ch*l2*p);
       gsl_matrix_set (df, i, 7, ch*l*p2);
       gsl_matrix_set (df, i, 8, ch*p3);
       }
    else                   /* type RPC00B */
       {
       ch = numer/(denom*denom);
       gsl_matrix_set (df, i, 0, ch*l);
       gsl_matrix_set (df, i, 1, ch*p);
       gsl_matrix_set (df, i, 2, ch*l*p);
       gsl_matrix_set (df, i, 3, ch*l2);
       gsl_matrix_set (df, i, 4, ch*p2);
       gsl_matrix_set (df, i, 5, ch*l3);
       gsl_matrix_set (df, i, 6, ch*l*p2);
       gsl_matrix_set (df, i, 7, ch*l2*p);
       gsl_matrix_set (df, i, 8, ch*p3);
       }
    }

  return GSL_SUCCESS;
}

int
rpcd9_fdf (const gsl_vector * x, void *params,
           gsl_vector * f, gsl_matrix * df)
{
  rpc_f (x, params, f);
  rpcd9_df (x, params, df);

  return GSL_SUCCESS;
}

int
rpcn10_df (const gsl_vector * x, void *params, gsl_matrix * df)
{
  double rpcn[20],rpcd[20],l,p,h,l2,p2,h2,l3,p3,h3,numer,denom,ch;
  size_t i;

  for (i=0;i<20;i++) rpcn[i] = tguess[i+rpc_linsmp*40];
  for (i=0;i<20;i++) rpcd[i] = tguess[i+20+rpc_linsmp*40];

  for (i=0;i<n_data_pt;i++)
    {
    l = xpar[0][i];
    p = xpar[1][i];
    h = xpar[2][i];
    l2 = l*l; l3 = l2*l;
    p2 = p*p; p3 = p2*p;
    h2 = h*h; h3 = h2*h;

    if (rpctype==0)       /* type RPC00A */
       {
       numer = rpcn[0]+rpcn[1]*l+rpcn[2]*p+rpcn[3]*h+
       rpcn[4]*l*p+rpcn[5]*l*h+rpcn[6]*p*h+rpcn[7]*p*l*h+
       rpcn[8]*l2+rpcn[9]*p2+rpcn[10]*h2+rpcn[11]*l3+
       rpcn[12]*l2*p+rpcn[13]*l2*h+rpcn[14]*l*p2+
       rpcn[15]*p3+rpcn[16]*p2*h+rpcn[17]*l*h2+
       rpcn[18]*p*h2+rpcn[19]*h3;
       denom = rpcd[0]+rpcd[1]*l+rpcd[2]*p+rpcd[3]*h+
       rpcd[4]*l*p+rpcd[5]*l*h+rpcd[6]*p*h+rpcd[7]*p*l*h+
       rpcd[8]*l2+rpcd[9]*p2+rpcd[10]*h2+rpcd[11]*l3+
       rpcd[12]*l2*p+rpcd[13]*l2*h+rpcd[14]*l*p2+
       rpcd[15]*p3+rpcd[16]*p2*h+rpcd[17]*l*h2+
       rpcd[18]*p*h2+rpcd[19]*h3;
       }
    else                   /* type RPC00B */
       {
       numer = rpcn[0]+rpcn[1]*l+rpcn[2]*p+rpcn[3]*h+
       rpcn[4]*l*p+rpcn[5]*l*h+rpcn[6]*p*h+rpcn[7]*l2+
       rpcn[8]*p2+rpcn[9]*h2+rpcn[10]*p*l*h+rpcn[11]*l3+
       rpcn[12]*l*p2+rpcn[13]*l*h2+rpcn[14]*l2*p+
       rpcn[15]*p3+rpcn[16]*p*h2+rpcn[17]*l2*h+
       rpcn[18]*p2*h+rpcn[19]*h3;
       denom = rpcd[0]+rpcd[1]*l+rpcd[2]*p+rpcd[3]*h+
       rpcd[4]*l*p+rpcd[5]*l*h+rpcd[6]*p*h+rpcd[7]*l2+
       rpcd[8]*p2+rpcd[9]*h2+rpcd[10]*p*l*h+rpcd[11]*l3+
       rpcd[12]*l*p2+rpcd[13]*l*h2+rpcd[14]*l2*p+
       rpcd[15]*p3+rpcd[16]*p*h2+rpcd[17]*l2*h+
       rpcd[18]*p2*h+rpcd[19]*h3;
       }

    if (rpctype==0)       /* type RPC00A */
       {
       ch = -1.0/denom;
       gsl_matrix_set (df, i, 0, ch);
       gsl_matrix_set (df, i, 1, ch*l);
       gsl_matrix_set (df, i, 2, ch*p);
       gsl_matrix_set (df, i, 3, ch*l*p);
       gsl_matrix_set (df, i, 4, ch*l2);
       gsl_matrix_set (df, i, 5, ch*p2);
       gsl_matrix_set (df, i, 6, ch*l3);
       gsl_matrix_set (df, i, 7, ch*l2*p);
       gsl_matrix_set (df, i, 8, ch*l*p2);
       gsl_matrix_set (df, i, 9, ch*p3);
       }
    else                   /* type RPC00B */
       {
       ch = -1.0/denom;
       gsl_matrix_set (df, i, 0, ch);
       gsl_matrix_set (df, i, 1, ch*l);
       gsl_matrix_set (df, i, 2, ch*p);
       gsl_matrix_set (df, i, 3, ch*l*p);
       gsl_matrix_set (df, i, 4, ch*l2);
       gsl_matrix_set (df, i, 5, ch*p2);
       gsl_matrix_set (df, i, 6, ch*l3);
       gsl_matrix_set (df, i, 7, ch*l*p2);
       gsl_matrix_set (df, i, 8, ch*l2*p);
       gsl_matrix_set (df, i, 9, ch*p3);
       }
    }

  return GSL_SUCCESS;
}

int
rpcn10_fdf (const gsl_vector * x, void *params,
           gsl_vector * f, gsl_matrix * df)
{
  rpc_f (x, params, f);
  rpcn10_df (x, params, df);

  return GSL_SUCCESS;
}

int
rpc39_df (const gsl_vector * x, void *params, gsl_matrix * df)
{
  double rpcn[20],rpcd[20],l,p,h,l2,p2,h2,l3,p3,h3,numer,denom,ch;
  size_t i;

  for (i=0;i<20;i++) rpcn[i] = tguess[i+rpc_linsmp*40];
  for (i=0;i<20;i++) rpcd[i] = tguess[i+20+rpc_linsmp*40];

  for (i=0;i<n_data_pt;i++)
    {
    l = xpar[0][i];
    p = xpar[1][i];
    h = xpar[2][i];
    l2 = l*l; l3 = l2*l;
    p2 = p*p; p3 = p2*p;
    h2 = h*h; h3 = h2*h;

    if (rpctype==0)       /* type RPC00A */
       {
       numer = rpcn[0]+rpcn[1]*l+rpcn[2]*p+rpcn[3]*h+
       rpcn[4]*l*p+rpcn[5]*l*h+rpcn[6]*p*h+rpcn[7]*p*l*h+
       rpcn[8]*l2+rpcn[9]*p2+rpcn[10]*h2+rpcn[11]*l3+
       rpcn[12]*l2*p+rpcn[13]*l2*h+rpcn[14]*l*p2+
       rpcn[15]*p3+rpcn[16]*p2*h+rpcn[17]*l*h2+
       rpcn[18]*p*h2+rpcn[19]*h3;
       denom = rpcd[0]+rpcd[1]*l+rpcd[2]*p+rpcd[3]*h+
       rpcd[4]*l*p+rpcd[5]*l*h+rpcd[6]*p*h+rpcd[7]*p*l*h+
       rpcd[8]*l2+rpcd[9]*p2+rpcd[10]*h2+rpcd[11]*l3+
       rpcd[12]*l2*p+rpcd[13]*l2*h+rpcd[14]*l*p2+
       rpcd[15]*p3+rpcd[16]*p2*h+rpcd[17]*l*h2+
       rpcd[18]*p*h2+rpcd[19]*h3;
       }
    else                   /* type RPC00B */
       {
       numer = rpcn[0]+rpcn[1]*l+rpcn[2]*p+rpcn[3]*h+
       rpcn[4]*l*p+rpcn[5]*l*h+rpcn[6]*p*h+rpcn[7]*l2+
       rpcn[8]*p2+rpcn[9]*h2+rpcn[10]*p*l*h+rpcn[11]*l3+
       rpcn[12]*l*p2+rpcn[13]*l*h2+rpcn[14]*l2*p+
       rpcn[15]*p3+rpcn[16]*p*h2+rpcn[17]*l2*h+
       rpcn[18]*p2*h+rpcn[19]*h3;
       denom = rpcd[0]+rpcd[1]*l+rpcd[2]*p+rpcd[3]*h+
       rpcd[4]*l*p+rpcd[5]*l*h+rpcd[6]*p*h+rpcd[7]*l2+
       rpcd[8]*p2+rpcd[9]*h2+rpcd[10]*p*l*h+rpcd[11]*l3+
       rpcd[12]*l*p2+rpcd[13]*l*h2+rpcd[14]*l2*p+
       rpcd[15]*p3+rpcd[16]*p*h2+rpcd[17]*l2*h+
       rpcd[18]*p2*h+rpcd[19]*h3;
       }

    if (rpctype==0)       /* type RPC00A */
       {
       ch = -1.0/denom;
       gsl_matrix_set (df, i, 0, ch);
       gsl_matrix_set (df, i, 1, ch*l);
       gsl_matrix_set (df, i, 2, ch*p);
       gsl_matrix_set (df, i, 3, ch*h);
       gsl_matrix_set (df, i, 4, ch*l*p);
       gsl_matrix_set (df, i, 5, ch*l*h);
       gsl_matrix_set (df, i, 6, ch*p*h);
       gsl_matrix_set (df, i, 7, ch*p*l*h);
       gsl_matrix_set (df, i, 8, ch*l2);
       gsl_matrix_set (df, i, 9, ch*p2);
       gsl_matrix_set (df, i,10, ch*h2);
       gsl_matrix_set (df, i,11, ch*l3);
       gsl_matrix_set (df, i,12, ch*l2*p);
       gsl_matrix_set (df, i,13, ch*l2*h);
       gsl_matrix_set (df, i,14, ch*l*p2);
       gsl_matrix_set (df, i,15, ch*p3);
       gsl_matrix_set (df, i,16, ch*p2*h);
       gsl_matrix_set (df, i,17, ch*l*h2);
       gsl_matrix_set (df, i,18, ch*p*h2);
       gsl_matrix_set (df, i,19, ch*h3);
       ch = numer/(denom*denom);
       gsl_matrix_set (df, i,20, ch*l);
       gsl_matrix_set (df, i,21, ch*p);
       gsl_matrix_set (df, i,22, ch*h);
       gsl_matrix_set (df, i,23, ch*l*p);
       gsl_matrix_set (df, i,24, ch*l*h);
       gsl_matrix_set (df, i,25, ch*p*h);
       gsl_matrix_set (df, i,26, ch*p*l*h);
       gsl_matrix_set (df, i,27, ch*l2);
       gsl_matrix_set (df, i,28, ch*p2);
       gsl_matrix_set (df, i,29, ch*h2);
       gsl_matrix_set (df, i,30, ch*l3);
       gsl_matrix_set (df, i,31, ch*l2*p);
       gsl_matrix_set (df, i,32, ch*l2*h);
       gsl_matrix_set (df, i,33, ch*l*p2);
       gsl_matrix_set (df, i,34, ch*p3);
       gsl_matrix_set (df, i,35, ch*p2*h);
       gsl_matrix_set (df, i,36, ch*l*h2);
       gsl_matrix_set (df, i,37, ch*p*h2);
       gsl_matrix_set (df, i,38, ch*h3);
       }
    else                   /* type RPC00B */
       {
       ch = -1.0/denom;
       gsl_matrix_set (df, i, 0, ch);
       gsl_matrix_set (df, i, 1, ch*l);
       gsl_matrix_set (df, i, 2, ch*p);
       gsl_matrix_set (df, i, 3, ch*h);
       gsl_matrix_set (df, i, 4, ch*l*p);
       gsl_matrix_set (df, i, 5, ch*l*h);
       gsl_matrix_set (df, i, 6, ch*p*h);
       gsl_matrix_set (df, i, 7, ch*l2);
       gsl_matrix_set (df, i, 8, ch*p2);
       gsl_matrix_set (df, i, 9, ch*h2);
       gsl_matrix_set (df, i,10, ch*p*l*h);
       gsl_matrix_set (df, i,11, ch*l3);
       gsl_matrix_set (df, i,12, ch*l*p2);
       gsl_matrix_set (df, i,13, ch*l*h2);
       gsl_matrix_set (df, i,14, ch*l2*p);
       gsl_matrix_set (df, i,15, ch*p3);
       gsl_matrix_set (df, i,16, ch*p*h2);
       gsl_matrix_set (df, i,17, ch*l2*h);
       gsl_matrix_set (df, i,18, ch*p2*h);
       gsl_matrix_set (df, i,19, ch*h3);
       ch = numer/(denom*denom);
       gsl_matrix_set (df, i,20, ch*l);
       gsl_matrix_set (df, i,21, ch*p);
       gsl_matrix_set (df, i,22, ch*h);
       gsl_matrix_set (df, i,23, ch*l*p);
       gsl_matrix_set (df, i,24, ch*l*h);
       gsl_matrix_set (df, i,25, ch*p*h);
       gsl_matrix_set (df, i,26, ch*l2);
       gsl_matrix_set (df, i,27, ch*p2);
       gsl_matrix_set (df, i,28, ch*h2);
       gsl_matrix_set (df, i,29, ch*p*l*h);
       gsl_matrix_set (df, i,30, ch*l3);
       gsl_matrix_set (df, i,31, ch*l*p2);
       gsl_matrix_set (df, i,32, ch*l*h2);
       gsl_matrix_set (df, i,33, ch*l2*p);
       gsl_matrix_set (df, i,34, ch*p3);
       gsl_matrix_set (df, i,35, ch*p*h2);
       gsl_matrix_set (df, i,36, ch*l2*h);
       gsl_matrix_set (df, i,37, ch*p2*h);
       gsl_matrix_set (df, i,38, ch*h3);
       }
    }

  return GSL_SUCCESS;
}

int
rpc39_fdf (const gsl_vector * x, void *params,
           gsl_vector * f, gsl_matrix * df)
{
  rpc_f (x, params, f);
  rpc39_df (x, params, df);

  return GSL_SUCCESS;
}

int
rpctest_df (const gsl_vector * x, void *params, gsl_matrix * df)
{
  double rpcn[20],rpcd[20],l,p,h,l2,p2,h2,l3,p3,h3,numer,denom,ch;
  size_t i;

  for (i=0;i<20;i++) rpcn[i] = tguess[i+rpc_linsmp*40];
  for (i=0;i<20;i++) rpcd[i] = tguess[i+20+rpc_linsmp*40];

  for (i=0;i<n_data_pt;i++)
    {
    l = xpar[0][i];
    p = xpar[1][i];
    h = xpar[2][i];
    l2 = l*l; l3 = l2*l;
    p2 = p*p; p3 = p2*p;
    h2 = h*h; h3 = h2*h;

    if (rpctype==0)       /* type RPC00A */
       {
       numer = rpcn[0]+rpcn[1]*l+rpcn[2]*p+rpcn[3]*h+
       rpcn[4]*l*p+rpcn[5]*l*h+rpcn[6]*p*h+rpcn[7]*p*l*h+
       rpcn[8]*l2+rpcn[9]*p2+rpcn[10]*h2+rpcn[11]*l3+
       rpcn[12]*l2*p+rpcn[13]*l2*h+rpcn[14]*l*p2+
       rpcn[15]*p3+rpcn[16]*p2*h+rpcn[17]*l*h2+
       rpcn[18]*p*h2+rpcn[19]*h3;
       denom = rpcd[0]+rpcd[1]*l+rpcd[2]*p+rpcd[3]*h+
       rpcd[4]*l*p+rpcd[5]*l*h+rpcd[6]*p*h+rpcd[7]*p*l*h+
       rpcd[8]*l2+rpcd[9]*p2+rpcd[10]*h2+rpcd[11]*l3+
       rpcd[12]*l2*p+rpcd[13]*l2*h+rpcd[14]*l*p2+
       rpcd[15]*p3+rpcd[16]*p2*h+rpcd[17]*l*h2+
       rpcd[18]*p*h2+rpcd[19]*h3;
       }
    else                   /* type RPC00B */
       {
       numer = rpcn[0]+rpcn[1]*l+rpcn[2]*p+rpcn[3]*h+
       rpcn[4]*l*p+rpcn[5]*l*h+rpcn[6]*p*h+rpcn[7]*l2+
       rpcn[8]*p2+rpcn[9]*h2+rpcn[10]*p*l*h+rpcn[11]*l3+
       rpcn[12]*l*p2+rpcn[13]*l*h2+rpcn[14]*l2*p+
       rpcn[15]*p3+rpcn[16]*p*h2+rpcn[17]*l2*h+
       rpcn[18]*p2*h+rpcn[19]*h3;
       denom = rpcd[0]+rpcd[1]*l+rpcd[2]*p+rpcd[3]*h+
       rpcd[4]*l*p+rpcd[5]*l*h+rpcd[6]*p*h+rpcd[7]*l2+
       rpcd[8]*p2+rpcd[9]*h2+rpcd[10]*p*l*h+rpcd[11]*l3+
       rpcd[12]*l*p2+rpcd[13]*l*h2+rpcd[14]*l2*p+
       rpcd[15]*p3+rpcd[16]*p*h2+rpcd[17]*l2*h+
       rpcd[18]*p2*h+rpcd[19]*h3;
       }

    if (rpctype==0)       /* type RPC00A */
       {
       ch = -1.0/denom;
       gsl_matrix_set (df, i, 0, ch);
       }
    else                   /* type RPC00B */
       {
       ch = -1.0/denom;
       gsl_matrix_set (df, i, 0, ch);
       }
    }
  return GSL_SUCCESS;
}

int
rpctest_fdf (const gsl_vector * x, void *params,
           gsl_vector * f, gsl_matrix * df)
{
  rpc_f (x, params, f);
  rpctest_df (x, params, df);

  return GSL_SUCCESS;
}

int
rpcdzt_df (const gsl_vector * x, void *params, gsl_matrix * df)
{
  double rpcn[20],rpcd[20],l,p,h,l2,p2,h2,l3,p3,h3,numer,denom,ch;
  size_t i;

  for (i=0;i<20;i++) rpcn[i] = tguess[i+rpc_linsmp*40];
  for (i=0;i<20;i++) rpcd[i] = tguess[i+20+rpc_linsmp*40];

  for (i=0;i<n_data_pt;i++)
    {
    l = xpar[0][i];
    p = xpar[1][i];
    h = xpar[2][i];
    l2 = l*l; l3 = l2*l;
    p2 = p*p; p3 = p2*p;
    h2 = h*h; h3 = h2*h;

    if (rpctype==0)       /* type RPC00A */
       {
       numer = rpcn[0]+rpcn[1]*l+rpcn[2]*p+rpcn[3]*h+
       rpcn[4]*l*p+rpcn[5]*l*h+rpcn[6]*p*h+rpcn[7]*p*l*h+
       rpcn[8]*l2+rpcn[9]*p2+rpcn[10]*h2+rpcn[11]*l3+
       rpcn[12]*l2*p+rpcn[13]*l2*h+rpcn[14]*l*p2+
       rpcn[15]*p3+rpcn[16]*p2*h+rpcn[17]*l*h2+
       rpcn[18]*p*h2+rpcn[19]*h3;
       denom = rpcd[0]+rpcd[1]*l+rpcd[2]*p+rpcd[3]*h+
       rpcd[4]*l*p+rpcd[5]*l*h+rpcd[6]*p*h+rpcd[7]*p*l*h+
       rpcd[8]*l2+rpcd[9]*p2+rpcd[10]*h2+rpcd[11]*l3+
       rpcd[12]*l2*p+rpcd[13]*l2*h+rpcd[14]*l*p2+
       rpcd[15]*p3+rpcd[16]*p2*h+rpcd[17]*l*h2+
       rpcd[18]*p*h2+rpcd[19]*h3;
       }
    else                   /* type RPC00B */
       {
       numer = rpcn[0]+rpcn[1]*l+rpcn[2]*p+rpcn[3]*h+
       rpcn[4]*l*p+rpcn[5]*l*h+rpcn[6]*p*h+rpcn[7]*l2+
       rpcn[8]*p2+rpcn[9]*h2+rpcn[10]*p*l*h+rpcn[11]*l3+
       rpcn[12]*l*p2+rpcn[13]*l*h2+rpcn[14]*l2*p+
       rpcn[15]*p3+rpcn[16]*p*h2+rpcn[17]*l2*h+
       rpcn[18]*p2*h+rpcn[19]*h3;
       denom = rpcd[0]+rpcd[1]*l+rpcd[2]*p+rpcd[3]*h+
       rpcd[4]*l*p+rpcd[5]*l*h+rpcd[6]*p*h+rpcd[7]*l2+
       rpcd[8]*p2+rpcd[9]*h2+rpcd[10]*p*l*h+rpcd[11]*l3+
       rpcd[12]*l*p2+rpcd[13]*l*h2+rpcd[14]*l2*p+
       rpcd[15]*p3+rpcd[16]*p*h2+rpcd[17]*l2*h+
       rpcd[18]*p2*h+rpcd[19]*h3;
       }

    if (rpctype==0)       /* type RPC00A */
       {
       ch = numer/(denom*denom);
       gsl_matrix_set (df, i, 0, ch);
       gsl_matrix_set (df, i, 1, ch*h);
       gsl_matrix_set (df, i, 2, ch*l*h);
       gsl_matrix_set (df, i, 3, ch*p*h);
       gsl_matrix_set (df, i, 4, ch*l*p*h);
       gsl_matrix_set (df, i, 5, ch*h2);
       gsl_matrix_set (df, i, 6, ch*l2*h);
       gsl_matrix_set (df, i, 7, ch*p2*h);
       gsl_matrix_set (df, i, 8, ch*l*h2);
       gsl_matrix_set (df, i, 9, ch*p*h2);
       gsl_matrix_set (df, i,10, ch*h3);
       }
    else                   /* type RPC00B */
       {
       ch = numer/(denom*denom);
       gsl_matrix_set (df, i, 0, ch);
       gsl_matrix_set (df, i, 1, ch*h);
       gsl_matrix_set (df, i, 2, ch*l*h);
       gsl_matrix_set (df, i, 3, ch*p*h);
       gsl_matrix_set (df, i, 4, ch*h2);
       gsl_matrix_set (df, i, 5, ch*p*l*h);
       gsl_matrix_set (df, i, 6, ch*l*h2);
       gsl_matrix_set (df, i, 7, ch*p*h2);
       gsl_matrix_set (df, i, 8, ch*l2*h);
       gsl_matrix_set (df, i, 9, ch*p2*h);
       gsl_matrix_set (df, i,10, ch*h3);
       }
    }

  return GSL_SUCCESS;
}

int
rpcdzt_fdf (const gsl_vector * x, void *params,
           gsl_vector * f, gsl_matrix * df)
{
  rpc_f (x, params, f);
  rpcdzt_df (x, params, df);

  return GSL_SUCCESS;
}

int
rpcnzt_df (const gsl_vector * x, void *params, gsl_matrix * df)
{
  double rpcn[20],rpcd[20],l,p,h,l2,p2,h2,l3,p3,h3,numer,denom,ch;
  size_t i;

  for (i=0;i<20;i++) rpcn[i] = tguess[i+rpc_linsmp*40];
  for (i=0;i<20;i++) rpcd[i] = tguess[i+20+rpc_linsmp*40];

  for (i=0;i<n_data_pt;i++)
    {
    l = xpar[0][i];
    p = xpar[1][i];
    h = xpar[2][i];
    l2 = l*l; l3 = l2*l;
    p2 = p*p; p3 = p2*p;
    h2 = h*h; h3 = h2*h;

    if (rpctype==0)       /* type RPC00A */
       {
       numer = rpcn[0]+rpcn[1]*l+rpcn[2]*p+rpcn[3]*h+
       rpcn[4]*l*p+rpcn[5]*l*h+rpcn[6]*p*h+rpcn[7]*p*l*h+
       rpcn[8]*l2+rpcn[9]*p2+rpcn[10]*h2+rpcn[11]*l3+
       rpcn[12]*l2*p+rpcn[13]*l2*h+rpcn[14]*l*p2+
       rpcn[15]*p3+rpcn[16]*p2*h+rpcn[17]*l*h2+
       rpcn[18]*p*h2+rpcn[19]*h3;
       denom = rpcd[0]+rpcd[1]*l+rpcd[2]*p+rpcd[3]*h+
       rpcd[4]*l*p+rpcd[5]*l*h+rpcd[6]*p*h+rpcd[7]*p*l*h+
       rpcd[8]*l2+rpcd[9]*p2+rpcd[10]*h2+rpcd[11]*l3+
       rpcd[12]*l2*p+rpcd[13]*l2*h+rpcd[14]*l*p2+
       rpcd[15]*p3+rpcd[16]*p2*h+rpcd[17]*l*h2+
       rpcd[18]*p*h2+rpcd[19]*h3;
       }
    else                   /* type RPC00B */
       {
       numer = rpcn[0]+rpcn[1]*l+rpcn[2]*p+rpcn[3]*h+
       rpcn[4]*l*p+rpcn[5]*l*h+rpcn[6]*p*h+rpcn[7]*l2+
       rpcn[8]*p2+rpcn[9]*h2+rpcn[10]*p*l*h+rpcn[11]*l3+
       rpcn[12]*l*p2+rpcn[13]*l*h2+rpcn[14]*l2*p+
       rpcn[15]*p3+rpcn[16]*p*h2+rpcn[17]*l2*h+
       rpcn[18]*p2*h+rpcn[19]*h3;
       denom = rpcd[0]+rpcd[1]*l+rpcd[2]*p+rpcd[3]*h+
       rpcd[4]*l*p+rpcd[5]*l*h+rpcd[6]*p*h+rpcd[7]*l2+
       rpcd[8]*p2+rpcd[9]*h2+rpcd[10]*p*l*h+rpcd[11]*l3+
       rpcd[12]*l*p2+rpcd[13]*l*h2+rpcd[14]*l2*p+
       rpcd[15]*p3+rpcd[16]*p*h2+rpcd[17]*l2*h+
       rpcd[18]*p2*h+rpcd[19]*h3;
       }

    if (rpctype==0)       /* type RPC00A */
       {
       ch = -1.0/denom;
       gsl_matrix_set (df, i, 0, ch);
       gsl_matrix_set (df, i, 1, ch*h);
       gsl_matrix_set (df, i, 2, ch*l*h);
       gsl_matrix_set (df, i, 3, ch*p*h);
       gsl_matrix_set (df, i, 4, ch*l*p*h);
       gsl_matrix_set (df, i, 5, ch*h2);
       gsl_matrix_set (df, i, 6, ch*l2*h);
       gsl_matrix_set (df, i, 7, ch*p2*h);
       gsl_matrix_set (df, i, 8, ch*l*h2);
       gsl_matrix_set (df, i, 9, ch*p*h2);
       gsl_matrix_set (df, i,10, ch*h3);
       }
    else                   /* type RPC00B */
       {
       ch = -1.0/denom;
       gsl_matrix_set (df, i, 0, ch*h);
       gsl_matrix_set (df, i, 1, ch*l*h);
       gsl_matrix_set (df, i, 2, ch*p*h);
       gsl_matrix_set (df, i, 3, ch*h2);
       gsl_matrix_set (df, i, 4, ch*p*l*h);
       gsl_matrix_set (df, i, 5, ch*l*h2);
       gsl_matrix_set (df, i, 6, ch*p*h2);
       gsl_matrix_set (df, i, 7, ch*l2*h);
       gsl_matrix_set (df, i, 8, ch*p2*h);
       gsl_matrix_set (df, i, 9, ch*h3);
       }
    }

  return GSL_SUCCESS;
}

int
rpcnzt_fdf (const gsl_vector * x, void *params,
           gsl_vector * f, gsl_matrix * df)
{
  rpc_f (x, params, f);
  rpcnzt_df (x, params, df);

  return GSL_SUCCESS;
}

int
rpcdz2_df (const gsl_vector * x, void *params, gsl_matrix * df)
{
  double rpcn[20],rpcd[20],l,p,h,l2,p2,h2,l3,p3,h3,numer,denom,ch;
  size_t i;

  for (i=0;i<20;i++) rpcn[i] = tguess[i+rpc_linsmp*40];
  for (i=0;i<20;i++) rpcd[i] = tguess[i+20+rpc_linsmp*40];

  for (i=0;i<n_data_pt;i++)
    {
    l = xpar[0][i];
    p = xpar[1][i];
    h = xpar[2][i];
    l2 = l*l; l3 = l2*l;
    p2 = p*p; p3 = p2*p;
    h2 = h*h; h3 = h2*h;

    if (rpctype==0)       /* type RPC00A */
       {
       numer = rpcn[0]+rpcn[1]*l+rpcn[2]*p+rpcn[3]*h+
       rpcn[4]*l*p+rpcn[5]*l*h+rpcn[6]*p*h+rpcn[7]*p*l*h+
       rpcn[8]*l2+rpcn[9]*p2+rpcn[10]*h2+rpcn[11]*l3+
       rpcn[12]*l2*p+rpcn[13]*l2*h+rpcn[14]*l*p2+
       rpcn[15]*p3+rpcn[16]*p2*h+rpcn[17]*l*h2+
       rpcn[18]*p*h2+rpcn[19]*h3;
       denom = rpcd[0]+rpcd[1]*l+rpcd[2]*p+rpcd[3]*h+
       rpcd[4]*l*p+rpcd[5]*l*h+rpcd[6]*p*h+rpcd[7]*p*l*h+
       rpcd[8]*l2+rpcd[9]*p2+rpcd[10]*h2+rpcd[11]*l3+
       rpcd[12]*l2*p+rpcd[13]*l2*h+rpcd[14]*l*p2+
       rpcd[15]*p3+rpcd[16]*p2*h+rpcd[17]*l*h2+
       rpcd[18]*p*h2+rpcd[19]*h3;
       }
    else                   /* type RPC00B */
       {
       numer = rpcn[0]+rpcn[1]*l+rpcn[2]*p+rpcn[3]*h+
       rpcn[4]*l*p+rpcn[5]*l*h+rpcn[6]*p*h+rpcn[7]*l2+
       rpcn[8]*p2+rpcn[9]*h2+rpcn[10]*p*l*h+rpcn[11]*l3+
       rpcn[12]*l*p2+rpcn[13]*l*h2+rpcn[14]*l2*p+
       rpcn[15]*p3+rpcn[16]*p*h2+rpcn[17]*l2*h+
       rpcn[18]*p2*h+rpcn[19]*h3;
       denom = rpcd[0]+rpcd[1]*l+rpcd[2]*p+rpcd[3]*h+
       rpcd[4]*l*p+rpcd[5]*l*h+rpcd[6]*p*h+rpcd[7]*l2+
       rpcd[8]*p2+rpcd[9]*h2+rpcd[10]*p*l*h+rpcd[11]*l3+
       rpcd[12]*l*p2+rpcd[13]*l*h2+rpcd[14]*l2*p+
       rpcd[15]*p3+rpcd[16]*p*h2+rpcd[17]*l2*h+
       rpcd[18]*p2*h+rpcd[19]*h3;
       }

    if (rpctype==0)       /* type RPC00A */
       {
       ch = numer/(denom*denom);
       gsl_matrix_set (df, i, 0, ch);
       gsl_matrix_set (df, i, 1, ch*h);
       }
    else                   /* type RPC00B */
       {
       ch = numer/(denom*denom);
       gsl_matrix_set (df, i, 0, ch);
       gsl_matrix_set (df, i, 1, ch*h);
       }
    }

  return GSL_SUCCESS;
}

int
rpcdz2_fdf (const gsl_vector * x, void *params,
           gsl_vector * f, gsl_matrix * df)
{
  rpc_f (x, params, f);
  rpcdz2_df (x, params, df);

  return GSL_SUCCESS;
}

int
rpcnz2_df (const gsl_vector * x, void *params, gsl_matrix * df)
{
  double rpcn[20],rpcd[20],l,p,h,l2,p2,h2,l3,p3,h3,numer,denom,ch;
  size_t i;

  for (i=0;i<20;i++) rpcn[i] = tguess[i+rpc_linsmp*40];
  for (i=0;i<20;i++) rpcd[i] = tguess[i+20+rpc_linsmp*40];

  for (i=0;i<n_data_pt;i++)
    {
    l = xpar[0][i];
    p = xpar[1][i];
    h = xpar[2][i];
    l2 = l*l; l3 = l2*l;
    p2 = p*p; p3 = p2*p;
    h2 = h*h; h3 = h2*h;

    if (rpctype==0)       /* type RPC00A */
       {
       numer = rpcn[0]+rpcn[1]*l+rpcn[2]*p+rpcn[3]*h+
       rpcn[4]*l*p+rpcn[5]*l*h+rpcn[6]*p*h+rpcn[7]*p*l*h+
       rpcn[8]*l2+rpcn[9]*p2+rpcn[10]*h2+rpcn[11]*l3+
       rpcn[12]*l2*p+rpcn[13]*l2*h+rpcn[14]*l*p2+
       rpcn[15]*p3+rpcn[16]*p2*h+rpcn[17]*l*h2+
       rpcn[18]*p*h2+rpcn[19]*h3;
       denom = rpcd[0]+rpcd[1]*l+rpcd[2]*p+rpcd[3]*h+
       rpcd[4]*l*p+rpcd[5]*l*h+rpcd[6]*p*h+rpcd[7]*p*l*h+
       rpcd[8]*l2+rpcd[9]*p2+rpcd[10]*h2+rpcd[11]*l3+
       rpcd[12]*l2*p+rpcd[13]*l2*h+rpcd[14]*l*p2+
       rpcd[15]*p3+rpcd[16]*p2*h+rpcd[17]*l*h2+
       rpcd[18]*p*h2+rpcd[19]*h3;
       }
    else                   /* type RPC00B */
       {
       numer = rpcn[0]+rpcn[1]*l+rpcn[2]*p+rpcn[3]*h+
       rpcn[4]*l*p+rpcn[5]*l*h+rpcn[6]*p*h+rpcn[7]*l2+
       rpcn[8]*p2+rpcn[9]*h2+rpcn[10]*p*l*h+rpcn[11]*l3+
       rpcn[12]*l*p2+rpcn[13]*l*h2+rpcn[14]*l2*p+
       rpcn[15]*p3+rpcn[16]*p*h2+rpcn[17]*l2*h+
       rpcn[18]*p2*h+rpcn[19]*h3;
       denom = rpcd[0]+rpcd[1]*l+rpcd[2]*p+rpcd[3]*h+
       rpcd[4]*l*p+rpcd[5]*l*h+rpcd[6]*p*h+rpcd[7]*l2+
       rpcd[8]*p2+rpcd[9]*h2+rpcd[10]*p*l*h+rpcd[11]*l3+
       rpcd[12]*l*p2+rpcd[13]*l*h2+rpcd[14]*l2*p+
       rpcd[15]*p3+rpcd[16]*p*h2+rpcd[17]*l2*h+
       rpcd[18]*p2*h+rpcd[19]*h3;
       }

    if (rpctype==0)       /* type RPC00A */
       {
       ch = -1.0/denom;
       gsl_matrix_set (df, i, 0, ch);
       gsl_matrix_set (df, i, 1, ch*h);
       }
    else                   /* type RPC00B */
       {
       ch = -1.0/denom;
       gsl_matrix_set (df, i, 0, ch*h);
       gsl_matrix_set (df, i, 1, ch*l*h);
       }
    }

  return GSL_SUCCESS;
}

int
rpcnz2_fdf (const gsl_vector * x, void *params,
           gsl_vector * f, gsl_matrix * df)
{
  rpc_f (x, params, f);
  rpcnz2_df (x, params, df);

  return GSL_SUCCESS;
}

/* end of user routines ************************************************/

gsl_multifit_function_fdf
make_fdf (int (* f) (const gsl_vector *, void *, gsl_vector *),
          int (* df) (const gsl_vector *, void *, gsl_matrix *),
          int (* fdf) (const gsl_vector *, void *, gsl_vector *, gsl_matrix *),
          size_t n,
          size_t p,
          void * params)
{
  gsl_multifit_function_fdf F_new;
  F_new.f = f;
  F_new.df = df;
  F_new.fdf = fdf;
  F_new.n = n;
  F_new.p = p;
  F_new.params = params;
  return F_new;
}

void
test_fdf (const char * name, gsl_multifit_function_fdf * f,
          double x0[], double x_final[],
          double f_sumsq, double sigma[])
{
  const gsl_multifit_fdfsolver_type *T;
  gsl_multifit_fdfsolver *s;

  const size_t n = f->n;
  const size_t p = f->p;

  int status;
  size_t iter = 0;

  gsl_vector_view x = gsl_vector_view_array (x0, p);

  T = gsl_multifit_fdfsolver_lmsder;
  s = gsl_multifit_fdfsolver_alloc (T, n, p);
  gsl_multifit_fdfsolver_set (s, f, &x.vector);

  do
    {
      status = gsl_multifit_fdfsolver_iterate (s);

#ifdef DEBUG
       printf("iter = %d  status = %d  |f| = %.18e x = \n",
         iter, status, gsl_blas_dnrm2 (s->f));

         gsl_vector_fprintf(stdout, s->x, "%.8e");
#endif
      
   diffsq = gsl_blas_dnrm2 (s->f)-oldfsq;
   printf("convergence = %.18e\n",diffsq);
   oldfsq = gsl_blas_dnrm2 (s->f);

   status = gsl_multifit_test_delta (s->dx, s->x, 0.0, 1e-7);
      iter++;
    }
  while (status == GSL_CONTINUE && iter < 1000);
  
   {
    size_t i;
    gsl_matrix * covar = gsl_matrix_alloc (p, p);
#if GSL_MAJOR_VERSION > 1
    gsl_matrix * J = gsl_matrix_alloc(n, p);
    gsl_multifit_fdfsolver_jac(s, J);
    gsl_multifit_covar (J, 0.0, covar);
    gsl_matrix_free (J);
#else    
    gsl_multifit_covar (s->J, 0.0, covar);
#endif    

    if (solcol[2]>0) for (i = 0 ; i < p; i++)
      {
        gsl_test_rel (gsl_vector_get (s->x, i), x_final[i], 1e-5,
                      "%s, lmsder, x%u", name, i);
      }


    {
      double s2 = pow(gsl_blas_dnrm2 (s->f), 2.0);

      if (solcol[4]>0) 
          gsl_test_rel (s2, f_sumsq, 1e-5, "%s, lmsder, |f|^2", name);

      if (solcol[3]>0) for (i = 0; i < p; i++)
        {
          double ei = sqrt(s2/(n-p))*sqrt(gsl_matrix_get(covar,i,i));
          gsl_test_rel (ei, sigma[i], 1e-4,
                        "%s, sigma(%d)", name, i) ;
        }
    }

    gsl_matrix_free (covar);
  }

  gsl_multifit_fdfsolver_free (s);
}

void main44(void)
{
   int i,j,datacol[20],coeffcol[100],unit,indcount,coeffcount,coldef;
   int ibis,status,clen,depcol,rescol,concol,numlsq,ier;
   int unit2,ibis2,clen2,solcount,soldef;
   int dummy,dct,noprint,uptr,lptr,igroup;
   int (*fptr)() = NULL,(*dfptr)() = NULL,(*fdfptr)() = NULL;
   float *concolv,groupnbr;
   double **sout,*ypar,eps,*guess,*solnck,*sigck;
   double sumsqck;
   char rpctypstr[2],rpclsstr[2];
   
   zifmessage("ibisnlsq version Fri Dec 28 2007");
   
   /* get the basic parameters */
   
   zvparm("func",func_name,&dct,&dummy,1,0);
   zvparm("rpctype",rpctypstr,&dct,&dummy,1,0);
   if (rpctypstr[0]=='B') rpctype = 1; else rpctype = 0;
   zvparm("rpcls",rpclsstr,&dct,&dummy,1,0);
   if (rpclsstr[0]=='S') rpc_linsmp = 1; else rpc_linsmp = 0;
   if (rpctype==0) printf("RPCTYPE=A\n");
      else printf("RPCTYPE=B\n");
   if (rpc_linsmp==0) printf("SOLVING FOR LINE\n");
      else printf("SOLVING FOR SAMP\n");
   status = zvp("depcol",&depcol,&dummy);
   status = zvp("rescol",&rescol,&dummy);
   status = zvp("concol",&concol,&dummy);
   if (concol!=0) zmabend("multiple solutions prohibited for now");
   noprint = zvptst("noprint");
   
   zvparm("datacol",datacol,&indcount,&coldef,20,0);
   zvparm("coeffcol",coeffcol,&coeffcount,&coldef,100,0);
   zvparm("solcol",solcol,&solcount,&soldef,5,0);
    
   /* read in data from the ibis interface file */

   status = zvunit(&unit,"inp",1, NULL);
   status = IBISFileOpen(unit,&ibis,"update",0,0,0,0);
   if (status!=1) IBISSignalU(unit,status,1);
   IBISFileGet(ibis,"nr",&clen,1,1,0);
   n_data_pt = clen;

   mz_alloc2((unsigned char ***)&xpar,indcount,clen,8);
   mz_alloc1((unsigned char **)&clsqdep,clen,8);
   mz_alloc1((unsigned char **)&ypar,clen,8);
   mz_alloc1((unsigned char **)&concolv,clen,8);
   
   if (rescol!=0) mz_alloc1((unsigned char **)&rout,clen,8);
   
   for (i=0;i<indcount;i++)
      {
      status = IBISColumnSet(ibis,"U_FORMAT","DOUB",datacol[i]);
      if (status!=1) IBISSignal(ibis,status,1);
      status = IBISColumnRead(ibis,(char *)xpar[i],datacol[i],1,clen);
      if (status!=1) IBISSignal(ibis,status,1);
      }
   status = IBISColumnSet(ibis,"U_FORMAT","DOUB",depcol);
   if (status!=1) IBISSignal(ibis,status,1);
   status = IBISColumnRead(ibis,(char *)ypar,depcol,1,clen);
   if (status!=1) IBISSignal(ibis,status,1);
   
   if (concol>0)
      {
      status = IBISColumnSet(ibis,"U_FORMAT","REAL",concol);
      if (status!=1) IBISSignal(ibis,status,1);
      status = IBISColumnRead(ibis,(char *)concolv,concol,1,clen);
      if (status!=1) IBISSignal(ibis,status,1);
      }
   else for (i=0;i<clen;i++) concolv[i] = 1.0;
   
   /* read in solution tguess/guess from the second ibis interface file */

   status = zvunit(&unit2,"inp",2, NULL);
   status = IBISFileOpen(unit2,&ibis2,"update",0,0,0,0);
   if (status!=1) IBISSignalU(unit2,status,1);
   IBISFileGet(ibis2,"nr",&clen2,1,1,0);
   n_guess = clen2;
   n_ind_var = clen2;   /* may be reset later */
   mz_alloc1((unsigned char **)&tguess,clen2,8);
   mz_alloc1((unsigned char **)&guess,clen2,8);
   mz_alloc1((unsigned char **)&solnck,clen2,8);
   mz_alloc1((unsigned char **)&sigck,clen2,8);
   printf("\n");
   status = IBISColumnSet(ibis2,"U_FORMAT","DOUB",solcol[0]);
   if (status!=1) IBISSignal(ibis2,status,1);
   status = IBISColumnRead(ibis2,(char *)tguess,solcol[0],1,clen2);
   if (status!=1) IBISSignal(ibis2,status,1);
   if (solcol[2]>0)
      {
      status = IBISColumnSet(ibis2,"U_FORMAT","DOUB",solcol[2]);
      if (status!=1) IBISSignal(ibis2,status,1);
      status = IBISColumnRead(ibis2,(char *)solnck,solcol[2],1,clen2);
      if (status!=1) IBISSignal(ibis2,status,1);
      }
   if (solcol[3]>0)
      {
      status = IBISColumnSet(ibis2,"U_FORMAT","DOUB",solcol[3]);
      if (status!=1) IBISSignal(ibis2,status,1);
      status = IBISColumnRead(ibis2,(char *)sigck,solcol[3],1,clen2);
      if (status!=1) IBISSignal(ibis2,status,1);
      }
   if (solcol[4]>0)
      {
      status = IBISColumnSet(ibis2,"U_FORMAT","DOUB",solcol[4]);
      if (status!=1) IBISSignal(ibis2,status,1);
      status = IBISColumnRead(ibis2,(char *)(&sumsqck),solcol[4],1,1);
      if (status!=1) IBISSignal(ibis2,status,1);
      }
   
   /* do the least squares */
   /* could reduce storage need by saving solutions more compactly, and
   then free the input data, and unpack the solutions */
   uptr = 0;
   for (igroup=0;;igroup++)
      {
      lptr = uptr;
      if (lptr>=clen) break;
      groupnbr = concolv[lptr];
      for (uptr=lptr;uptr<=clen;uptr++)
         if (uptr==clen||concolv[uptr]!=groupnbr) break;
      numlsq = uptr-lptr;
      for (j=lptr;j<uptr;j++) clsqdep[j-lptr] = ypar[j];
      eps = 1.e-7;
     
      /* set up function pointers and verify data columns */
      
      if (strcmp(func_name,"KIRBY2")==0)
         {
         n_ind_var = 5;
         if (n_guess!=5) zmabend("wrong length for guess vector");
         if (indcount!=1) zmabend("wrong number of input data columns");
         for (i=0;i<5;i++) guess[i] = tguess[i];
         fptr = &kirby2_f;
         dfptr = &kirby2_df;
         fdfptr = &kirby2_fdf;
         }
      if (strcmp(func_name,"RPCD20")==0)
         {
         n_ind_var = 20;
         if (n_guess!=80) zmabend("wrong length for guess vector");
         if (indcount!=3) zmabend("wrong number of input data columns");
         for (i=0;i<20;i++)
            {
            guess[i] = tguess[i+20+rpc_linsmp*40];
            solnck[i] = solnck[i+20+rpc_linsmp*40];
            sigck[i] = sigck[i+20+rpc_linsmp*40];
            }
         fptr = &rpc_f;
         dfptr = &rpcd20_df;
         fdfptr = &rpcd20_fdf;
         }
      if (strcmp(func_name,"RPCD19")==0)
         {
         n_ind_var = 19;
         if (n_guess!=80) zmabend("wrong length for guess vector");
         if (indcount!=3) zmabend("wrong number of input data columns");
         for (i=0;i<19;i++)
            {
            guess[i] = tguess[i+21+rpc_linsmp*40];
            solnck[i] = solnck[i+21+rpc_linsmp*40];
            sigck[i] = sigck[i+21+rpc_linsmp*40];
            }
         fptr = &rpc_f;
         dfptr = &rpcd19_df;
         fdfptr = &rpcd19_fdf;
         }
      if (strcmp(func_name,"RPCN20")==0)
         {
         n_ind_var = 20;
         if (n_guess!=80) zmabend("wrong length for guess vector");
         if (indcount!=3) zmabend("wrong number of input data columns");
         for (i=0;i<20;i++)
            {
            guess[i] = tguess[i+rpc_linsmp*40];
            solnck[i] = solnck[i+rpc_linsmp*40];
            sigck[i] = sigck[i+rpc_linsmp*40];
            }
         fptr = &rpc_f;
         dfptr = &rpcn20_df;
         fdfptr = &rpcn20_fdf;
         }
      if (strcmp(func_name,"RPCD10")==0)
         {
         n_ind_var = 10;
         if (n_guess!=80) zmabend("wrong length for guess vector");
         if (indcount!=3) zmabend("wrong number of input data columns");
         rpctran(guess,tguess,20+rpc_linsmp*40);
         rpctran(solnck,solnck,20+rpc_linsmp*40);
         rpctran(sigck,sigck,20+rpc_linsmp*40);
         fptr = &rpc_f;
         dfptr = &rpcd10_df;
         fdfptr = &rpcd10_fdf;
         }
      if (strcmp(func_name,"RPCD9")==0)
         {
         n_ind_var = 9;
         if (n_guess!=80) zmabend("wrong length for guess vector");
         if (indcount!=3) zmabend("wrong number of input data columns");
         rpctran9(guess,tguess,20+rpc_linsmp*40);
         rpctran9(solnck,solnck,20+rpc_linsmp*40);
         rpctran9(sigck,sigck,20+rpc_linsmp*40);
         fptr = &rpc_f;
         dfptr = &rpcd9_df;
         fdfptr = &rpcd9_fdf;
         }
      if (strcmp(func_name,"RPCN10")==0)
         {
         n_ind_var = 10;
         if (n_guess!=80) zmabend("wrong length for guess vector");
         if (indcount!=3) zmabend("wrong number of input data columns");
         rpctran(guess,tguess,rpc_linsmp*40);
         rpctran(solnck,solnck,rpc_linsmp*40);
         rpctran(sigck,sigck,rpc_linsmp*40);
         fptr = &rpc_f;
         dfptr = &rpcn10_df;
         fdfptr = &rpcn10_fdf;
         }
      if (strcmp(func_name,"RPC39")==0)
         {
         n_ind_var = 39;
         if (n_guess!=80) zmabend("wrong length for guess vector");
         if (indcount!=3) zmabend("wrong number of input data columns");
         for (i=0;i<20;i++)
            {
            guess[i] = tguess[i+rpc_linsmp*40];
            solnck[i] = solnck[i+rpc_linsmp*40];
            sigck[i] = sigck[i+rpc_linsmp*40];
            }
         for (i=21;i<40;i++)
            {
            guess[i-1] = tguess[i+rpc_linsmp*40];
            solnck[i-1] = solnck[i+rpc_linsmp*40];
            sigck[i-1] = sigck[i+rpc_linsmp*40];
            }
         fptr = &rpc_f;
         dfptr = &rpc39_df;
         fdfptr = &rpc39_fdf;
         }
      if (strcmp(func_name,"RPCTEST")==0)
         {
         n_ind_var = 1;
         if (n_guess!=80) zmabend("wrong length for guess vector");
         if (indcount!=3) zmabend("wrong number of input data columns");
         for (i=0;i<1;i++)
            {
            guess[i] = tguess[i+rpc_linsmp*40];
            solnck[i] = solnck[i+rpc_linsmp*40];
            sigck[i] = sigck[i+rpc_linsmp*40];
            }
         fptr = &rpc_f;
         dfptr = &rpctest_df;
         fdfptr = &rpctest_fdf;
         }
      if (strcmp(func_name,"RPCDZT")==0)
         {
         n_ind_var = 11;
         if (n_guess!=80) zmabend("wrong length for guess vector");
         if (indcount!=3) zmabend("wrong number of input data columns");
         rpcztran(guess,tguess,20+rpc_linsmp*40);
         rpcztran(solnck,solnck,20+rpc_linsmp*40);
         rpcztran(sigck,sigck,20+rpc_linsmp*40);
         fptr = &rpc_f;
         dfptr = &rpcdzt_df;
         fdfptr = &rpcdzt_fdf;
         }
      if (strcmp(func_name,"RPCNZT")==0)
         {
         n_ind_var = 11;
         if (n_guess!=80) zmabend("wrong length for guess vector");
         if (indcount!=3) zmabend("wrong number of input data columns");
         rpcztran(guess,tguess,rpc_linsmp*40);
         rpcztran(solnck,solnck,rpc_linsmp*40);
         rpcztran(sigck,sigck,rpc_linsmp*40);
         fptr = &rpc_f;
         dfptr = &rpcnzt_df;
         fdfptr = &rpcnzt_fdf;
         }
      if (strcmp(func_name,"RPCDZ2")==0)
         {
         n_ind_var = 2;
         if (n_guess!=80) zmabend("wrong length for guess vector");
         if (indcount!=3) zmabend("wrong number of input data columns");
         rpc2tran(guess,tguess,20+rpc_linsmp*40);
         rpc2tran(solnck,solnck,20+rpc_linsmp*40);
         rpc2tran(sigck,sigck,20+rpc_linsmp*40);
         fptr = &rpc_f;
         dfptr = &rpcdz2_df;
         fdfptr = &rpcdz2_fdf;
         }
      if (strcmp(func_name,"RPCNZ2")==0)
         {
         n_ind_var = 2;
         if (n_guess!=80) zmabend("wrong length for guess vector");
         if (indcount!=3) zmabend("wrong number of input data columns");
         rpc2tran(guess,tguess,rpc_linsmp*40);
         rpc2tran(solnck,solnck,rpc_linsmp*40);
         rpc2tran(sigck,sigck,rpc_linsmp*40);
         fptr = &rpc_f;
         dfptr = &rpcnz2_df;
         fdfptr = &rpcnz2_fdf;
         }
      
      /* the gnu non-linear solver */
      
      oldfsq = 0.0;
      ier = 0;
      
      gsl_ieee_env_setup();
      gsl_multifit_function_fdf f = make_fdf (fptr, dfptr,
         fdfptr, n_data_pt, n_ind_var, 0);
      test_fdf(func_name, &f, guess, solnck, sumsqck, sigck);
      
      /* printing if requested */
      
      if (!noprint)
         {
         printf("\nseq  concol    solution coefficient\n\n");
         for (i=0;i<n_ind_var;i++)
            if (ier==0) printf("%3d %7.2f %24.15e\n",
                i+1,groupnbr,csol[i]);
            else printf("%3d %7.2f %24.15e\n",
                i+1,groupnbr,-999.0);
         printf("\n");
         }
      
      /* calculate the output data, rout now calc in non-lin functions */
      
      if (coeffcol[0]!=0)
         {
         if (coeffcount!=n_ind_var&&coldef==0)
            zmabend("Count for parameter COEFFCOL wrong");
         mz_alloc2((unsigned char ***)&sout,n_ind_var,clen,8);
         for (i=0;i<n_ind_var;i++)
            for (j=lptr;j<uptr;j++)
               if (ier==0) sout[i][j] = csol[i];
               else sout[i][j] = -999.0;
         }
      }
     
   /* Output desired columns to the ibis interface files */
   
   if (coeffcol[0]!=0)
      {
      for (i=0;i<n_ind_var;i++)
         {
         status = IBISColumnSet(ibis,"U_FORMAT","DOUB",coeffcol[i]);
         if (status!=1) IBISSignal(ibis,status,1);
         status = IBISColumnWrite(ibis,(char*)(sout[i]),coeffcol[i],1,clen);
         if (status!=1) IBISSignal(ibis,status,1);
         }
      }
   if (rescol!=0)
      {
      status = IBISColumnSet(ibis,"U_FORMAT","DOUB",rescol);
      if (status!=1) IBISSignal(ibis,status,1);
      status = IBISColumnWrite(ibis,(char*)rout,rescol,1,clen);
      if (status!=1) IBISSignal(ibis,status,1);
      }

   status = IBISColumnSet(ibis2,"U_FORMAT","DOUB",solcol[1]);
   if (status!=1) IBISSignal(ibis2,status,1);
   status = IBISColumnWrite(ibis2,(char*) tguess,solcol[1],1,clen2);
   if (status!=1) IBISSignal(ibis2,status,1);
         
   status = IBISFileClose(ibis,0);
   if (status!=1) IBISSignal(ibis,status,1);
   status = IBISFileClose(ibis2,0);
   if (status!=1) IBISSignal(ibis2,status,1);
}
