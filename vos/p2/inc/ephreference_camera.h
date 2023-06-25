/*******************************************************************************

  Title:     ephreference_camera
  Author:    Al Zobrist 
  Date:      2007/01/25
  Function:  This is a combined function for earth or star coordinates.  See the
             individual programs for more details.
             
             clen        expected number of grid points
             calc_mode   0 = earth first, if off earth, then space
                         1 = earth only
                         2 = space only
             *calc_case  1 = calculated successfully for earth coordinates
                         2 = calculated successfully for space coordinates
                         3 = failed (requested earth coord, off earth)
 
*******************************************************************************/

int ephreference_camera_sv_c(double *urange, double *vrange, double *hrange,
			     double *TOD_t_SV, double *TOD_q_SV, double *SV_t_C, double *SV_q_C, 
			     double fu, double fv, double q, double u0, double v0, double *kappa, 
			     double TT, double UT1, int *gr_adr, int *gc_adr, double **G_adr,
			     int clen, int calc_mode,int *calc_case);
