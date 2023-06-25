#ifndef CMOD_XFORM_H
#define CMOD_XFORM_H

/****************************************************************************
  	cmod_xform.h

	This program transforms a camera model.
*****************************************************************************/



void cmod_cahv_xform(double r[3][3], double t[3], 
		   double c_i[3], double a_i[3], double h_i[3], double v_i[3], 
		   double c_f[3], double a_f[3], double h_f[3], double v_f[3]);

void cmod_cahv_xform_cov(double r[3][3],double s_i[12][12],double s_f[12][12]);


void cmod_cahvor_xform(double r[3][3], double t[3], 
		       double c_i[3], double a_i[3], double h_i[3], 
		       double v_i[3], double o_i[3], double r_i[3],
		       double c_f[3], double a_f[3], double h_f[3], 
		       double v_f[3], double o_f[3], double r_f[3]);

/******************************************************************************
********************************   CMOD_CAHVOR_XFORM_COV   *******************

     This function rotates a CAHVOR model's covariance matrix to correspond to
     rotation of the model itself. Note that model translations do not affect
     the covariance matrix. 
******************************************************************************/

void cmod_cahvor_xform_cov(double r[3][3], 
			   double s_i[18][18], double s_f[18][18]);

void cmod_cahvore_xform(double r[3][3], double t[3], 
			double c_i[3], double a_i[3], double h_i[3], 
			double v_i[3], double o_i[3], double r_i[3],
			double e_i[3],
			double c_f[3], double a_f[3], double h_f[3], 
			double v_f[3], double o_f[3], double r_f[3],
			double e_f[3]);

void cmod_cahvore_xform_cov(double r[3][3], 
			    double s_i[21][21], double s_f[21][21]);

#endif  /* CMOD_XFORM_H */

