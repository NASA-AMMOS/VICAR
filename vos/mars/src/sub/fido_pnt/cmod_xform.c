/*
  	cmod_xform.c

	This program transforms a camera model.
*/


#include <stdio.h>
#include "mat3.h"


void cmod_cahv_xform(r, t, c_i, a_i, h_i, v_i,
                	   c_f, a_f, h_f, v_f)
double r[3][3];         /* rotation */
double t[3];            /* translation */
double c_i[3];          /* input initial model center vector C */
double a_i[3];          /* input initial model axis   vector A */
double h_i[3];          /* input initial model horiz. vector H */
double v_i[3];          /* input initial model vert.  vector V */
double c_f[3];          /* output final model center vector C */
double a_f[3];          /* output final model axis   vector A */
double h_f[3];          /* output final model horiz. vector H */
double v_f[3];          /* output final model vert.  vector V */
{
     /* Rotate and translate the C vector */
     mult331(r, c_i, c_f);
     add3(c_f, t, c_f);

     /* Rotate the A, H, V vectors */
     mult331(r, a_i, a_f);
     mult331(r, h_i, h_f);
     mult331(r, v_i, v_f);
     }


void cmod_cahv_xform_cov(r, s_i, s_f)
double r[3][3];		/* input rotation */
double s_i[12][12];     /* input initial covariance */
double s_f[12][12];     /* output final covariance */
{
     int i, j, k;
     double d;
     static double r12[12][12], r12t[12][12], stemp[12][12];

     /* Contruct a matrix, R12, (and its transpose) to rotate the covariance
         |R   |
         | R  |
         |  R |
         |   R|
     */
     for (i=0; i<12; i++)
         for (j=0; j<12; j++)
             r12[i][j] = 0;
     for (i=0; i<3; i++) {
         for (j=0; j<3; j++) {
             r12[i+0][j+0] = r[i][j];
             r12[i+3][j+3] = r[i][j];
             r12[i+6][j+6] = r[i][j];
             r12[i+9][j+9] = r[i][j];
             }
         }
     for (i=0; i<12; i++)
         for (j=0; j<12; j++)
             r12t[i][j] = r12[j][i];

     /* Pre-multiply by the matrix */
     for (i=0; i<12; i++) {
         for (j=0; j<12; j++) {
             d = 0;
             for (k=0; k<12; k++)
                 d += r12[i][k] * s_i[k][j];
             stemp[i][j] = d;
             }
         }

     /* Post-multiply by the transpose */
     for (i=0; i<12; i++) {
         for (j=0; j<12; j++) {
             d = 0;
             for (k=0; k<12; k++)
                 d += stemp[i][k] * r12t[k][j];
             s_f[i][j] = d;
             }
         }
     }   


void cmod_cahvor_xform(r, t, c_i, a_i, h_i, v_i, o_i, r_i,
                              c_f, a_f, h_f, v_f, o_f, r_f)
double r[3][3];         /* input rotation */
double t[3];            /* input translation */
double c_i[3];          /* input initial model center vector C */
double a_i[3];          /* input initial model axis   vector A */
double h_i[3];          /* input initial model horiz. vector H */
double v_i[3];          /* input initial model vert.  vector V */
double o_i[3];          /* input initial model optical axis unit vector O */
double r_i[3];          /* input initial model radial-distortion terms  R */
double c_f[3];          /* output final model center vector C */
double a_f[3];          /* output final model axis   vector A */
double h_f[3];          /* output final model horiz. vector H */
double v_f[3];          /* output final model vert.  vector V */
double o_f[3];          /* output final model optical axis unit vector O */
double r_f[3];          /* output final model radial-distortion terms  R */
{
     /* Rotate and translate the C vector */
     mult331(r, c_i, c_f);     /* rotate */
     add3(c_f, t, c_f);        /* translate */

     /* Rotate the A, H, V, O vectors */
     mult331(r, a_i, a_f);
     mult331(r, h_i, h_f);
     mult331(r, v_i, v_f);
     mult331(r, o_i, o_f);

     /* Copy over the R "vector" unchanged */
     copy3(r_i, r_f);
     }



/******************************************************************************
********************************   CMOD_CAHVOR_XFORM_COV   *******************
*******************************************************************************

     This function rotates a CAHVOR model's covariance matrix to correspond to
     rotation of the model itself. Note that model translations do not affect
     the covariance matrix. */

void cmod_cahvor_xform_cov(r, s_i, s_f)
double r[3][3];         /* input rotation */
double s_i[18][18];     /* input initial covariance */
double s_f[18][18];     /* output final covariance */
{
     int i, j, k;
     double d;
     static double r18[18][18], r18t[18][18], stemp[18][18];

     /* Contruct a matrix, R18, (and its transpose) to rotate the covariance
         |R     |
         | R    |
         |  R   |
         |   R  |
         |    R |
         |     I|
     */ 
     for (i=0; i<18; i++)
         for (j=0; j<18; j++)
             r18[i][j] = 0;
     for (i=0; i<3; i++) {
         for (j=0; j<3; j++) {
             r18[i+ 0][j+ 0] = r[i][j];
             r18[i+ 3][j+ 3] = r[i][j];
             r18[i+ 6][j+ 6] = r[i][j];
             r18[i+ 9][j+ 9] = r[i][j];
             r18[i+12][j+12] = r[i][j];
             }
         }
     r18[15][15] = 1;
     r18[16][16] = 1;
     r18[17][17] = 1;
     for (i=0; i<18; i++)
         for (j=0; j<18; j++)
             r18t[i][j] = r18[j][i];

     /* Pre-multiply by the matrix */
     for (i=0; i<18; i++) {
         for (j=0; j<18; j++) {
             d = 0;
             for (k=0; k<18; k++)
                 d += r18[i][k] * s_i[k][j];
             stemp[i][j] = d;
             } 
         }

     /* Post-multiply by the transpose */
     for (i=0; i<18; i++) {
         for (j=0; j<18; j++) {
             d = 0;
             for (k=0; k<18; k++)
                 d += stemp[i][k] * r18t[k][j];
             s_f[i][j] = d;
             } 
         }
     }


void cmod_cahvore_xform(r, t, c_i, a_i, h_i, v_i, o_i, r_i, e_i,
                               c_f, a_f, h_f, v_f, o_f, r_f, e_f)
double r[3][3];         /* input rotation */
double t[3];            /* input translation */
double c_i[3];          /* input initial model center vector C */
double a_i[3];          /* input initial model axis   vector A */
double h_i[3];          /* input initial model horiz. vector H */
double v_i[3];          /* input initial model vert.  vector V */
double o_i[3];          /* input initial model optical axis unit vector O */
double r_i[3];          /* input initial model radial-distortion terms  R */
double e_i[3];          /* input initial model entrance-pupil    terms  E */
double c_f[3];          /* output final model center vector C */
double a_f[3];          /* output final model axis   vector A */
double h_f[3];          /* output final model horiz. vector H */
double v_f[3];          /* output final model vert.  vector V */
double o_f[3];          /* output final model optical axis unit vector O */
double r_f[3];          /* output final model radial-distortion terms  R */
double e_f[3];          /* output final model entrance-pupil    terms  E */
{
     /* Rotate and translate the C vector */
     mult331(r, c_i, c_f);     /* rotate */
     add3(c_f, t, c_f);        /* translate */

     /* Rotate the A, H, V, O vectors */
     mult331(r, a_i, a_f);
     mult331(r, h_i, h_f);
     mult331(r, v_i, v_f);
     mult331(r, o_i, o_f);

     /* Copy over the R & E "vectors" unchanged */
     copy3(r_i, r_f);
     copy3(e_i, e_f);
     }


void cmod_cahvore_xform_cov(r, s_i, s_f)
double r[3][3];         /* input rotation */
double s_i[21][21];     /* input initial covariance */
double s_f[21][21];     /* output final covariance */
{
     int i, j, k;
     double d;
     static double r21[21][21], r21t[21][21], stemp[21][21];

     /* Contruct a matrix, R21, (and its transpose) to rotate the covariance
         |R      |
         | R     |
         |  R    |
         |   R   |
         |    R  |
         |     I |
         |      I|
     */
     for (i=0; i<21; i++)
         for (j=0; j<21; j++)
             r21[i][j] = 0;
     for (i=0; i<3; i++) {
         for (j=0; j<3; j++) {
             r21[i+ 0][j+ 0] = r[i][j];
             r21[i+ 3][j+ 3] = r[i][j];
             r21[i+ 6][j+ 6] = r[i][j];
             r21[i+ 9][j+ 9] = r[i][j];
             r21[i+12][j+12] = r[i][j];
             }
         }
     r21[15][15] = 1;
     r21[16][16] = 1;
     r21[17][17] = 1;
     r21[18][18] = 1;
     r21[19][19] = 1;
     r21[20][20] = 1;
     for (i=0; i<21; i++)
         for (j=0; j<21; j++)
             r21t[i][j] = r21[j][i];

     /* Pre-multiply by the matrix */
     for (i=0; i<21; i++) {
         for (j=0; j<21; j++) {
             d = 0;
             for (k=0; k<21; k++)
                 d += r21[i][k] * s_i[k][j];
             stemp[i][j] = d;
             }
         }

     /* Post-multiply by the transpose */
     for (i=0; i<21; i++) {
         for (j=0; j<21; j++) {
             d = 0;
             for (k=0; k<21; k++)
                 d += stemp[i][k] * r21t[k][j];
             s_f[i][j] = d;
             } 
         }
     }
