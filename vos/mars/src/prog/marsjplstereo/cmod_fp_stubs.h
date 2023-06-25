/* Stubs for the cmod_*_fp routines, which Mark Maimone added to his	*/
/* version of the cmod package but Todd Litwin has not incorporated	*/
/* into the mainline.  They read and write camera models given a FILE	*/
/* pointer rather than a filename.  Since they are not needed by	*/
/* marsjplstereo, we simply stub them out here.				*/

#ifndef CMOD_FP_STUBS_H
#define CMOD_FP_STUBS_H

#ifdef __cplusplus
  extern "C" {
#endif

int cmod_cahv_read_fp(FILE *fp, double c[3], double a[3], double h[3],
		double v[3], double s[12][12], double *hs, double *hc,
		double *vs, double *vc, double *theta, double s_int[5][5])
{ printf("Attempt to call unimplemented function cmod_cahv_read_fp()!!");
  return 0; }

int cmod_cahv_read2_fp(FILE *fp, int *xdim, int *ydim, double c[3],
		double a[3], double h[3], double v[3], double s[12][12],
		double *hs, double *hc, double *vs, double *vc, double *theta,
		double s_int[5][5])
{ printf("Attempt to call unimplemented function cmod_cahv_read2_fp()!!");
  return 0; }

int cmod_cahv_write_fp(FILE *fp, char *comment, double c[3], double a[3],
		double h[3], double v[3], double s[12][12], double hs,
		double hc, double vs, double vc, double theta,
		double s_int[5][5])
{ printf("Attempt to call unimplemented function cmod_cahv_write_fp()!!");
  return 0; }

int cmod_cahv_write2_fp(FILE *fp, char *comment, int xdim, int ydim,
		double c[3], double a[3], double h[3], double v[3],
		double s[12][12], double hs, double hc, double vs, double vc,
		double theta, double s_int[5][5])
{ printf("Attempt to call unimplemented function cmod_cahv_write2_fp()!!");
  return 0; }

int cmod_cahvor_read_fp(FILE *fp, double c[3], double a[3], double h[3],
		double v[3], double o[3], double r[3], double s[18][18],
		double *hs, double *hc, double *vs, double *vc, double *theta,
		double s_int[5][5])
{ printf("Attempt to call unimplemented function cmod_cahvor_read_fp()!!");
  return 0; }

int cmod_cahvor_read2_fp(FILE *fp, int *xdim, int *ydim, double c[3],
		double a[3], double h[3], double v[3], double o[3],
		double r[3], double s[18][18], double *hs, double *hc,
		double *vs, double *vc, double *theta, double s_int[5][5])
{ printf("Attempt to call unimplemented function cmod_cahvor_read2_fp()!!");
  return 0; }

int cmod_cahvor_write_fp(FILE *fp, char *comment, double c[3], double a[3],
		double h[3], double v[3], double o[3], double r[3],
		double s[18][18], double hs, double hc, double vs, double vc,
		double theta, double s_int[5][5])
{ printf("Attempt to call unimplemented function cmod_cahvor_write_fp()!!");
  return 0; }

int cmod_cahvor_write2_fp(FILE *fp, char *comment, int xdim, int ydim,
		double c[3], double a[3], double h[3], double v[3],
		double o[3], double r[3], double s[18][18], double hs,
		double hc, double vs, double vc, double theta,
		double s_int[5][5])
{ printf("Attempt to call unimplemented function cmod_cahvor_write2_fp()!!");
  return 0; }

int cmod_cahvore_read_fp(FILE *fp, int *xdim, int *ydim, int *mtype,
		double *mparm, double c[3], double a[3], double h[3],
		double v[3], double o[3], double r[3], double e[3],
		double s[21][21], double *hs, double *hc, double *vs,
		double *vc, double *theta, double s_int[5][5])
{ printf("Attempt to call unimplemented function cmod_cahvore_read_fp()!!");
  return 0; }

int cmod_cahvore_write_fp(FILE *fp, char *comment, int xdim, int ydim,
		int mtype, double mparm, double c[3], double a[3],
		double h[3], double v[3], double o[3], double r[3],
		double e[3], double s[21][21], double hs, double hc,
		double vs, double vc, double theta, double s_int[5][5])
{ printf("Attempt to call unimplemented function cmod_cahvore_write_fp()!!");
  return 0; }

#ifdef __cplusplus
  }
#endif

#endif

