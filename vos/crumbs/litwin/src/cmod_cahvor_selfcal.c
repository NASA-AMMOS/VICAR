/******************************************************************************
*                                                                             *
*                     C M O D _ C A H V O R _ S E L F C A L                   *
*                                                                             *
*                                       Todd Litwin                           *
*                                       Written:  1 Apr 1998                  *
*                                       Updated: 17 May 1999                  *
*                                                                             *
*                                       Copyright (C) 1998, 1999              *
*                                       California Institute of Technology    *
*                                       All Rights Reserved                   *
*                                                                             *
*******************************************************************************


	This file contains functions for checking and updating a pair of
	CAHVOR models. The external functions are new interfaces to
	functions written by Yalin Xiong; see below. */


#include <stdio.h>
#include <math.h>
#include <mat3.h>

#define SUCCESS 0
#define FAILURE (-1)

#define TRUE 1
#define FALSE 0

#define NEW_MODEL 1
#define OLD_MODEL 2

static int calc_error;

static int self_calibration();
static int self_calibration_old();
static int out_of_calibration();


/******************************************************************************
********************************   CMOD_CAHVOR_CHKCAL   ***********************
*******************************************************************************

    This function checks the goodness of an existing calibration. It returns
    one of three possible values, depending upon the outcome of the check:

	0 = out of calibration
	1 = stereo calibration OK, but individual camera may be bad
	2 = both camera in good calibration

    */

int cmod_cahvor_chkcal(maxerr,
	n_match, match, n_point1, point1, n_point2, point2,
	xdim, ydim, c1, a1, h1, v1, o1, r1, c2, a2, h2, v2, o2, r2,
	verbose)
double maxerr;		/* input maximum RMS pixel error allowable */
double match[][7];	/* input matches: 0,1 = 2D #1; 2,3 = 2D #2; 4-6 = cov */
int n_point1;		/* input number of model 1 3D+2D positions */
double point1[][8];	/* input model 1 p: 0-2 = 3D; 3,4 = 2D; 5-7 = 2D cov */
int n_point2;		/* input number of model 2 3D+2D positions */
double point2[][8];	/* input model 2 p: 0-2 = 3D; 3,4 = 2D; 5-7 = 2D cov */
int xdim;		/* input X dimension of image */
int ydim;		/* input Y dimension of image */
double c1[3];		/* input model 1 center vector C */
double a1[3];		/* input model 1 axis   vector A */
double h1[3];		/* input model 1 horiz. vector H */
double v1[3];		/* input model 1 vert.  vector V */
double o1[3];		/* input model 1 axis   vector O */
double r1[3];		/* input model 1 dist.  terms  R */
double c2[3];		/* input model 2 center vector C */
double a2[3];		/* input model 2 axis   vector A */
double h2[3];		/* input model 2 horiz. vector H */
double v2[3];		/* input model 2 vert.  vector V */
double o2[3];		/* input model 2 axis   vector O */
double r2[3];		/* input model 2 dist.  terms  R */
int verbose;		/* input to printout intermediate results */
{
    static double cam[36];

    /* Setup the input camera models */
    copy3(c1, cam+ 0);
    copy3(a1, cam+ 3);
    copy3(h1, cam+ 6);
    copy3(v1, cam+ 9);
    copy3(o1, cam+12);
    copy3(r1, cam+15);
    copy3(c2, cam+18);
    copy3(a2, cam+21);
    copy3(h2, cam+24);
    copy3(v2, cam+27);
    copy3(o2, cam+30);
    copy3(r2, cam+33);

    /* Perform self calibration */
    return out_of_calibration(
		match, n_match, point1, n_point1, point2, n_point2,
		cam, OLD_MODEL, maxerr, verbose);
    }


/******************************************************************************
********************************   CMOD_CAHVOR_SELFCAL   **********************
*******************************************************************************

    This function updates a stereo pair of CAHVOR camera models from input
    data about points in the images. */

int cmod_cahvor_selfcal(priori,
	pos1, orient1, center1, scale1, dcenter1, dcoeff1,
	pos2, orient2, center2, scale2, dcenter2, dcoeff2,
	n_match, match, err_match,
	n_point1,  point1,  err_point1,
	n_point2,  point2,  err_point2,
	reject,
	xdim, ydim,
	c1i, a1i, h1i, v1i, o1i, r1i, s1i, c2i, a2i, h2i, v2i, o2i, r2i, s2i,
	c1o, a1o, h1o, v1o, o1o, r1o, s1o, c2o, a2o, h2o, v2o, o2o, r2o, s2o,
	so, verbose)

int priori;		/* input mode for updating/fixing: */
			/*   0 = hard, allowing zero change */
			/*   1 = soft, use apriori weights (with artficial) */
			/*   2 = soft, use apriori weights (w/o  artficial) */
int pos1;		/* input update model 1 3D position */
int orient1;		/* input update model 1 3D orientation */
int center1;		/* input update model 1 2D center */
int scale1;		/* input update model 1 2D scale */
int dcenter1;		/* input update model 1 distortion center */
int dcoeff1;		/* input update model 1 distortion coefficients */
int pos2;		/* input update model 2 3D position */
int orient2;		/* input update model 2 3D orientation */
int center2;		/* input update model 2 2D center */
int scale2;		/* input update model 2 2D scale */
int dcenter2;		/* input update model 2 distortion center */
int dcoeff2;		/* input update model 2 distortion coefficients */
int n_match;		/* input number of 2D matching points */
double match[][7];	/* input matches: 0,1 = 2D #1; 2,3 = 2D #2; 4-6 = cov */
double *err_match;	/* output RMS error of 2D points */
int n_point1;		/* input number of model 1 3D+2D positions */
double point1[][8];	/* input model 1 p: 0-2 = 3D; 3,4 = 2D; 5-7 = 2D cov */
double *err_point1;	/* output RMS error of model 1 positions */
int n_point2;		/* input number of model 2 3D+2D positions */
double point2[][8];	/* input model 2 p: 0-2 = 3D; 3,4 = 2D; 5-7 = 2D cov */
double *err_point2;	/* output RMS error of model 2 positions */
int reject;		/* input if to allow point rejection */
int xdim;		/* input X dimension of image */
int ydim;		/* input Y dimension of image */
double c1i[3];		/* input  model 1 center vector C */
double a1i[3];		/* input  model 1 axis   vector A */
double h1i[3];		/* input  model 1 horiz. vector H */
double v1i[3];		/* input  model 1 vert.  vector V */
double o1i[3];		/* input  model 1 axis   vector O */
double r1i[3];		/* input  model 1 dist.  terms  R */
double s1i[18][18];	/* input  model 1 covarinace of CAHVOR */
double c2i[3];		/* input  model 2 center vector C */
double a2i[3];		/* input  model 2 axis   vector A */
double h2i[3];		/* input  model 2 horiz. vector H */
double v2i[3];		/* input  model 2 vert.  vector V */
double o2i[3];		/* input  model 2 axis   vector O */
double r2i[3];		/* input  model 2 dist.  terms  R */
double s2i[18][18];	/* input  model 2 covarinace of CAHVOR */
double c1o[3];		/* output model 1 center vector C */
double a1o[3];		/* output model 1 axis   vector A */
double h1o[3];		/* output model 1 horiz. vector H */
double v1o[3];		/* output model 1 vert.  vector V */
double o1o[3];		/* output model 1 axis   vector O */
double r1o[3];		/* output model 1 dist.  terms  R */
double s1o[18][18];	/* output model 1 covarinace of CAHVOR, or NULL */
double c2o[3];		/* output model 2 center vector C */
double a2o[3];		/* output model 2 axis   vector A */
double h2o[3];		/* output model 2 horiz. vector H */
double v2o[3];		/* output model 2 vert.  vector V */
double o2o[3];		/* output model 2 axis   vector O */
double r2o[3];		/* output model 2 dist.  terms  R */
double s2o[18][18];	/* output model 2 covarinace of CAHVOR, or NULL */
double so[36][36];	/* output combined covariances, or NULL */
int verbose;		/* input to printout intermediate results */
{
    int i, j;
    static double apri_cam[36], apri_cov[36][36];
    static double post_cam[36], post_cov[36][36];
    static int fixing[30];

    /* Setup which components are to be updated and which fixed */
    fixing[ 0] = !pos1;
    fixing[ 1] = !pos1;
    fixing[ 2] = !pos1;
    fixing[ 3] = !orient1;
    fixing[ 4] = !orient1;
    fixing[ 5] = !orient1;
    fixing[ 6] = !center1;
    fixing[ 7] = !center1;
    fixing[ 8] = !scale1;
    fixing[ 9] = !scale1;
    fixing[10] = !dcenter1;
    fixing[11] = !dcenter1;
    fixing[12] = !dcoeff1;
    fixing[13] = !dcoeff1;
    fixing[14] = !dcoeff1;
    fixing[15] = !pos2;
    fixing[16] = !pos2;
    fixing[17] = !pos2;
    fixing[18] = !orient2;
    fixing[19] = !orient2;
    fixing[20] = !orient2;
    fixing[21] = !center2;
    fixing[22] = !center2;
    fixing[23] = !scale2;
    fixing[24] = !scale2;
    fixing[25] = !dcenter2;
    fixing[26] = !dcenter2;
    fixing[27] = !dcoeff2;
    fixing[28] = !dcoeff2;
    fixing[29] = !dcoeff2;

    /* Setup the input camera models */
    copy3(c1i, apri_cam+ 0);
    copy3(a1i, apri_cam+ 3);
    copy3(h1i, apri_cam+ 6);
    copy3(v1i, apri_cam+ 9);
    copy3(o1i, apri_cam+12);
    copy3(r1i, apri_cam+15);
    copy3(c2i, apri_cam+18);
    copy3(a2i, apri_cam+21);
    copy3(h2i, apri_cam+24);
    copy3(v2i, apri_cam+27);
    copy3(o2i, apri_cam+30);
    copy3(r2i, apri_cam+33);

    /* Setup the input covariance matrices */
    for (i=0; i<18; i++)
	for (j=0; j<18; j++)
	    apri_cov[i][j] = 0;
    for (i=0; i<18; i++)
	for (j=0; j<18; j++) {
	    apri_cov[i   ][j   ] = s1i[i][j];
	    apri_cov[i+18][j+18] = s2i[i][j];
	    }

    /* Prepare for optional output of combined covariance */
    if (so == NULL)
	so  = post_cov;

    /* Perform self calibration */
    if (self_calibration_old(
		match, n_match, err_match,
		point1,  n_point1,  err_point1,
		point2,  n_point2,  err_point2,
		apri_cam, apri_cov, priori,
		post_cam, so, fixing, reject, verbose) == FAILURE)
	return FAILURE;

    /* Extract the output camera models */
    copy3(post_cam+ 0, c1o);
    copy3(post_cam+ 3, a1o);
    copy3(post_cam+ 6, h1o);
    copy3(post_cam+ 9, v1o);
    copy3(post_cam+12, o1o);
    copy3(post_cam+15, r1o);
    copy3(post_cam+18, c2o);
    copy3(post_cam+21, a2o);
    copy3(post_cam+24, h2o);
    copy3(post_cam+27, v2o);
    copy3(post_cam+30, o2o);
    copy3(post_cam+33, r2o);

    /* Extract the output covariance matrices */
    for (i=0; i<18; i++)
	for (j=0; j<18; j++) {
	    s1o[i][j] = so[i   ][j   ];
	    s2o[i][j] = so[i+18][j+18];
	    }

    return SUCCESS;
    }


/******************************************************************************
*                                                                             *
*                              Support Functions                              *
*                                                                             *
*******************************************************************************


	The following support functions were taken from several source
	files written by Yalin Xiong. They contain the heart of the
	self-calibration algorithm.

	*/

#define mat_add_3x3d(a, b, c)	add33(a, b, c)
#define mat_copy_3x3d(a, b)	copy33(a, b)
#define mat_invert_d3(a, b)	inv33(a, b)
#define mat_invert_d2(a, b)	inv22(a, b)
#define mat_mul_3x3x3d(a, b, c)	mult333(a, b, c)
#define mat_scale_3x3d(s, a, b)	scale33(s, a, b)
#define vect_add_3d(a, b, c)	add3(a, b, c)
#define vect_copy_3d(a, b)	copy3(a, b)
#define vect_cross_3d(a, b, c)	cross3(a, b, c)
#define vect_dot_3d(a, b)	dot3(a, b)
#define vect_scale_3d(s, a, b)	scale3(s, a, b)
#define vect_sub_3d(a, b, c)	sub3(a, b, c)


typedef int stat_t;
typedef int bool_t;

extern char *malloc();

static double (*inv22())[2];


/* Routines used by self calibration */

#define LEFT 1
#define RIGHT 2

/*#define NEW_MODEL 1*/
/*#define OLD_MODEL 2*/

/* self_calibration.c */

static void fixing ();
/*...
static int self_calibration ();
static int self_calibration_old ();
static int out_of_calibration ();
...*/

/* LM_min.c */

static void gaussj ();
static void mrqmin ();

/* new_camera.c */

static void old2new ();
static void old2new2 ();
static void new2old ();
static void new2old2 ();
static void cov2old ();
static void cov2new ();

/* data_io.c */

static stat_t read_in_camera_parameters ();
static stat_t read_in_camera_parameters2 ();
static stat_t read_in_features ();
static stat_t read_in_features2 ();
static stat_t read_in_hood_points ();
static stat_t read_in_hood_points2 ();
static stat_t read_3d_points ();
static void fileprint2 ();
static void fileprint3 ();
static void print_cam ();
static void print_cam1 ();
static void print_cam2 ();
static void print_cam_new ();

/* imgio.c */

static void ReadImage1 ();
static void ReadImage2 ();
static void WriteGil1 ();
static void WriteGil2 ();
static void WriteGil3 ();

/* img2ray.c */

static double *vect2mat ();
static double *outer_prod ();
static double afun ();
static void img2ray ();

/* min_dist.c */
static void compute_dist ();
static void compute_dist2 ();
static double min_dist ();
static double min_dist2 ();

/* ray2img.c */

static double taofun ();
static double ufun ();
static double alphafun ();
static void ray2img ();

/* eigen_sym.c */

static void eigen_sym ();

/* Jacobian.c */

static double invt_matrixd ();
static void Jacobian ();

/* update_cam.c */

static void Jacobian_hood ();
static void Jacobian_hood2 ();
static void error_hood ();
static void error_feature ();
static void Jacobian_new ();
static void Jacobian_new2 ();
static void trans2old ();
static void calibrated_P ();
static void update_cam ();
static void apriori_weights_new ();

/* singular_mat.c */
static void singular_mat ();
static void inv_eigenvalues ();
static void singular_mat2 ();
static void general_inverse ();

/* para_scale.c */

static void para_scale ();

/* reject_outlier.c */

static void sort ();
static int X84 ();

/* blob_detection.c */

static void blob_detection ();
static void detect_hood_points ();

/* feature_match.c */

static int feature_match_one ();
static int feature_match_one1 ();
static int feature_match ();
static int feature_match1 ();

/* choose_features.c */

static void feature_detection ();

/* coarse2fine.c */

static void initial_pyramid ();
static void remove_pyramid ();
static void Generate_Pyramid ();
static void c2f_match ();
static void c2f_match1 ();

/* corimg.c */

static int corimg ();

/* int_forstner.c */

static int int_forstner();
static int int_forstner_best();

/* threshold.c */

static int getthreshold ();

/* evaluations.c */

static void evaluations ();

/* region.c */
static int region_find();
static int region_find8();
static int region_maxsize();
static int region_start();
static int region_stat();
static int region_stats();
static int region_stop();


/*****************************************************************
 *  C program:  Iterative Self Calibration Using Features and 3D Points
 *
 *                created by Yalin Xiong on ppt.ius.cs.cmu.edu
 *
 *             creation time:  Fri Mar  1 1996
 *             file     name:  /usr0/yx/jpl/self-calib/self_calibration.c
 *****************************************************************/

/*#include <math.h>*/
/*#include <stdio.h>*/
/*#include <strutil.h>*/

#define TYPICAL_DEPTH 5.0 /* typical depth used for rescaling */
#define MAX_ITERATION 200
#define EPS_RATIO 1e-2

/* priori_mode :
         0         default manual setting
	 1         all fixed except image origins and orientations
	 2         all fixed except image origins, orintations and 3D position
	 3         none is fixed
*/

static void fixing_pattern (fixing, priori_mode)
int fixing[30], priori_mode;
{
  int *ip, i;

  for (i = 0; i < 30; i++) fixing[i] = 1;

  ip = &(fixing[15]);

  if (priori_mode == 1) {

    fixing[3] = fixing[4] = fixing[5] = 0; /* Left orientation */
    ip[3] = ip[4] = ip[5] = 0; /* Right orientation */

    fixing[6] = fixing[7] = 0; /* Left origin */
    ip[6] = ip[7] = 0; /* Right origin */

  } else if (priori_mode == 2) {

    fixing[0] = fixing[1] = fixing[2] = 0; /* Left 3D position */
    ip[0] = ip[1] = ip[2] = 0; /* Right 3D position */

    fixing[3] = fixing[4] = fixing[5] = 0; /* Left orientation */
    ip[3] = ip[4] = ip[5] = 0; /* Right orientation */

    fixing[6] = fixing[7] = 0; /* Left origin */
    ip[6] = ip[7] = 0; /* Right origin */

  } else if (priori_mode == 3) {

    for (i = 0; i < 30; i++) fixing[i] = 0;
  } else {

    /* Left Camera */

    /* 3D Position of projective center */
    fixing[0] = 1;
    fixing[1] = 1;
    fixing[2] = 1;

    /* Camera orientation */
    fixing[3] = 0;
    fixing[4] = 0;
    fixing[5] = 0;

    /* Projective center */
    fixing[6] = 0;
    fixing[7] = 0;

    /* Horizontal scale factor */
    fixing[8] = 1;

    /* Aspect ratio */
    fixing[9] = 1;

    /* Distortion center */
    fixing[10] = fixing[6];
    fixing[11] = fixing[7];

    /* Distortion coefficients */
    fixing[12] = 0;
    fixing[13] = 0;
    fixing[14] = 0;

    /* Right Camera */

    /* 3D Position of projective center */
    ip[0] = 1;
    ip[1] = 1;
    ip[2] = 1;

    /* Camera orientation */
    ip[3] = 0;
    ip[4] = 0;
    ip[5] = 0;

    /* Projective center */
    ip[6] = 0;
    ip[7] = 0;

    /* Horizontal scale factor */
    ip[8] = 1;

    /* Aspect ratio */
    ip[9] = 1;

    /* Distortion center */
    ip[10] = ip[6];
    ip[11] = ip[7];

    /* Distortion coefficients */
    ip[12] = 0;
    ip[13] = 0;
    ip[14] = 0;
  }
}

/* Self Calibration procedure for updating camera parameters (new model) */

static int self_calibration (features, nof, ferr, left3D, left_nop, lerr,
		      right3D, right_nop, rerr, apri_cam, apri_cov, priori_mode,
		      post_cam, post_cov, fixing, outlier_reject, verbose)
double features[][7]; /* Feature matches and their covariance */
double left3D[][8]; /* 3D points and their covariance for left camera */
double right3D[][8]; /* 3D Points and their covariance for right camera */
double apri_cam[30], apri_cov[30][30]; /* apriori camera parameters */
double post_cam[30], post_cov[30][30]; /* posteriori camera parameters */
double *ferr, *lerr, *rerr; /* output RMS errors */
int nof, left_nop, right_nop, *fixing; /* number of features, 3D points */
int outlier_reject; /* Whether to reject outliers */
int verbose; /* For debug purpose only */
{
  static double scale[30], dcam[30], *current_depth, chisq, old_chisq;
  float *residuals;
  unsigned char *validity;
  int i, j, num_para, num_outlier;
  float maxRows, maxCols;

  maxRows = maxCols = 0;
  for (i = 0; i < nof; i++) {
    if (features[i][0] > maxCols) maxCols = features[i][0];
    if (features[i][1] > maxRows) maxRows = features[i][1];
  }

  calc_error = FALSE;

  for (i = 0; i < 30; i++)
    post_cam[i] = apri_cam[i];
  if ((current_depth = (double *) malloc (nof * sizeof (double))) == NULL ||
      (validity = (unsigned char *) malloc ((nof + left_nop + right_nop)
					    * sizeof (unsigned char)))
       == NULL ||
      (residuals = (float *) malloc ((nof + left_nop + right_nop) * 2 *
				   sizeof (float))) == NULL) {
    fprintf(stderr, "Fail to allocate memory %d bytes in self_caliberation\n",
	    nof * sizeof (double));
    return FAILURE;
  }
  for (i = 0; i < nof; i++) current_depth[i] = 1.0;
  for (i = 0; i < (nof + left_nop + right_nop); i++)
    validity[i] = 1;

  num_para = 30; /* Full matrix */
  for (i = 0; i < MAX_ITERATION; i++) {
    /* Obtains Scales for the parameters */
    para_scale (post_cam, scale, TYPICAL_DEPTH, (int) maxRows, (int) maxCols);

    if (calc_error)
      goto ABORT;

    update_cam (features, nof, ferr, current_depth, left3D, left_nop, lerr,
		right3D, right_nop, rerr, apri_cam, apri_cov, priori_mode,
		post_cam, post_cov, dcam, fixing, scale, num_para, 
		&chisq, residuals, validity, verbose);

    if (calc_error)
      goto ABORT;

    for (j = 0; j < 30; j++)
      post_cam[j] += dcam[j];

    if (i == 0) 
      old_chisq = chisq;
    else if (fabs((chisq - old_chisq) / chisq) < EPS_RATIO)
      break;
    else
      old_chisq = chisq;
  }

  if (outlier_reject) {

    num_outlier = X84 (residuals, 2, nof, validity);
    if (verbose)
	printf("\t %d features rejected\n", num_outlier);
    num_outlier = X84 (residuals, 2, left_nop, &(validity[nof]));
    if (verbose)
	printf("\t %d left 3D points rejected\n", num_outlier);
    num_outlier = X84 (residuals, 2, right_nop, &(validity[nof+left_nop]));
    if (verbose)
	printf("\t %d right 3D points rejected\n", num_outlier);

    for (i = 0; i < MAX_ITERATION; i++) {
      /* Obtains Scales for the parameters */
      para_scale (post_cam, scale, TYPICAL_DEPTH, (int) maxRows, (int) maxCols);

      if (calc_error)
	goto ABORT;

      update_cam (features, nof, ferr, current_depth, left3D, left_nop, lerr,
		  right3D, right_nop, rerr, apri_cam, apri_cov, priori_mode,
		  post_cam, post_cov, dcam, fixing, scale, num_para, 
		  &chisq, residuals, validity, verbose);

      if (calc_error)
	goto ABORT;

      for (j = 0; j < 30; j++)
	post_cam[j] += dcam[j];

      if (i == 0) 
	old_chisq = chisq;
      else if (fabs((chisq - old_chisq) / chisq) < EPS_RATIO)
	break;
      else
	old_chisq = chisq;
    }
  }
  if (i == MAX_ITERATION) {
    calc_error = TRUE;
    if (verbose)
      fprintf(stderr, "Self calibration fails to converge\n");
  }

  ABORT:
  free (current_depth);
  free (residuals);
  free (validity);
  return (calc_error ? FAILURE : SUCCESS);
}

/* Self calibration for updating camera parameters (OLD models) 
   Correlations between parameters of different camera are not considered */

static int self_calibration_old (features, nof, ferr, left3D, left_nop, lerr,
			  right3D, right_nop, rerr, apri_cam, apri_cov,
			  priori_mode, post_cam, post_cov, fixing,
			  outlier_reject, verbose)
double features[][7]; /* Feature matches and their covariance */
double left3D[][8]; /* 3D points and their covariance for left camera */
double right3D[][8]; /* 3D Points and their covariance for right camera */
double apri_cam[36], apri_cov[36][36]; /* apriori camera parameters */
double post_cam[36], post_cov[36][36]; /* posteriori camera parameters */
double *ferr, *lerr, *rerr; /* output RMS errors */
int nof, left_nop, right_nop, *fixing; /* number of features, 3D points */
int outlier_reject; /* Whether to reject outliers */
int verbose; /* For debug purpose only */
{
  static double tmp_cov[18][18], new_cov[15][15];
  static double new_apri_cam[30], new_apri_cov[30][30];
  static double new_post_cam[30], new_post_cov[30][30];
  int i, j, result;

  if (priori_mode != 0) {
    for (i = 0; i < 30; i++)
      for (j = 0; j < 30; j++)
	new_apri_cov[i][j] = 0.0;

    /* Converting left camera */
    for (i = 0; i < 18; i++)
      for (j = 0; j < 18; j++)
	tmp_cov[i][j] = apri_cov[i][j];
    cov2new (new_apri_cam, new_cov, apri_cam, tmp_cov);
    for (i = 0; i < 15; i++)
      for (j = 0; j < 15; j++)
	new_apri_cov[i][j] = new_cov[i][j];

    /* Converting right camera */
    for (i = 0; i < 18; i++)
      for (j = 0; j < 18; j++)
	tmp_cov[i][j] = apri_cov[i+18][j+18];
    cov2new (&(new_apri_cam[15]), new_cov, &(apri_cam[18]), tmp_cov);
    for (i = 0; i < 15; i++)
      for (j = 0; j < 15; j++)
	new_apri_cov[i+15][j+15] = new_cov[i][j];
  } else {
    old2new2 (apri_cam, new_apri_cam, NULL, TRUE);
    old2new2 (&(apri_cam[18]), &(new_apri_cam[15]), NULL, TRUE);
  }

  result = 
    self_calibration (features, nof, ferr, left3D, left_nop, lerr,
		      right3D, right_nop, rerr, new_apri_cam, new_apri_cov,
		      priori_mode, new_post_cam, new_post_cov, fixing,
		      outlier_reject, verbose);

  if (result == FAILURE) {
    fprintf(stderr, "Failed to perform self calibration\n");
    return FAILURE;
  }

  for (i = 0; i < 36; i++)
    for (j = 0; j < 36; j++)
      post_cov[i][j] = 0.0;

  /* Converting left camera */
  for (i = 0; i < 15; i++)
    for (j = 0; j < 15; j++)
      new_cov[i][j] = new_post_cov[i][j];
  cov2old (new_post_cam, new_cov, post_cam, tmp_cov);
  for (i = 0; i < 18; i++)
    for (j = 0; j < 18; j++)
      post_cov[i][j] = tmp_cov[i][j];

  /* Converting right camera */
  for (i = 0; i < 15; i++)
    for (j = 0; j < 15; j++)
      new_cov[i][j] = new_post_cov[i+15][j+15];
  cov2old (&(new_post_cam[15]), new_cov, &(post_cam[18]), tmp_cov);
  for (i = 0; i < 18; i++)
    for (j = 0; j < 18; j++)
      post_cov[i+18][j+18] = tmp_cov[i][j];

  return SUCCESS;
}

#define CALIB_THRE 0.5

/* Test whether current camera set is out of calibration

   return: 0   The stereo is out of calibration
           1   The stereo is OK, but individual camera may be out of
  	       calibration
	   2   Both cameras are calibrated.
*/ 

static int out_of_calibration (features, nof, left3D, left_nop, right3D,
			right_nop, camera, new_old, max_err, verbose)

double features[][7]; /* Feature matches and their covariance */
double left3D[][8]; /* 3D points and their covariance for left camera */
double right3D[][8]; /* 3D Points and their covariance for right camera */
int nof, left_nop, right_nop, verbose;
int new_old; /* Specify whether the camera parameters are in new or old model */
double *camera; /* Input camera parameters */
float max_err; /* Maximal pixel error allowed */
{
  float err_stat[6];
  int status;

  calc_error = FALSE;

  calibrated_P (features, nof, left3D, left_nop, right3D,
		right_nop, camera, new_old, err_stat, verbose);

  if (calc_error)
    return FAILURE;

  if (verbose) {
    printf("\t\t Before Rejection\t After Rejection\n");
    printf("Features:\t %.3f \t\t\t %.3f\n", err_stat[0], err_stat[3]);
    printf("Left 3D Points\t %.3f \t\t\t %.3f\n",
	   err_stat[1], err_stat[4]);
    printf("Right 3D Points\t %.3f \t\t\t %.3f\n",
	   err_stat[2], err_stat[5]);
  }

  if (err_stat[3] < max_err && err_stat[4] < max_err &&
      err_stat[5] < max_err) 
    status = 2;
  else if (err_stat[3] < max_err)
    status = 1;
  else
    status = 0;
  
  return (status);
}


/*****************************************************************
 *  C program:  A New Set of Camera Parameters
 *
 *                created by Yalin Xiong on PPT.IUS.CS.CMU.EDU
 *
 *             creation time:  Thu Nov 16 1995
 *             file     name:  /usr0/yx/jpl/self-calib/cam_para2.c
 *****************************************************************/

/*#include <math.h>*/
/*#include <stdio.h>*/
/*#include <mathutil.h>*/
/*#include <self_calib.h>*/

#define PI 3.141592653589

/* New Camera Parameters: 
   C[3]: Position of projective center.
   Q[3]: Orientation of the camera. Two choices: Quaternion or roll/pitch/yaw.
   Cp[2]: Projective image center
   Hs: Horizontal scale factor
   As: Aspect Ratio = Vs / Hs
   Co[2]: Distortion image center
   R[3]: Radial distortion parameters.
*/

/* Quaternion to Rotation Matrix */
static void quat2rot (q, rot)
double q[4], rot[3][3];
{
  rot[0][0] = 1.0 - 2.0 * (q[1] * q[1] + q[2] * q[2]);
  rot[0][1] = 2.0 * (q[0] * q[1] - q[2] * q[3]);
  rot[0][2] = 2.0 * (q[0] * q[2] + q[1] * q[3]);

  rot[1][0] = 2.0 * (q[0] * q[1] + q[2] * q[3]);
  rot[1][1] = 1.0 - 2.0 * (q[0] * q[0] + q[2] * q[2]);
  rot[1][2] = 2.0 * (q[1] * q[2] - q[0] * q[3]);

  rot[2][0] = 2.0 * (q[0] * q[2] - q[1] * q[3]);
  rot[2][1] = 2.0 * (q[1] * q[2] + q[0] * q[3]);
  rot[2][2] = 1.0 - 2.0 * (q[0] * q[0] + q[1] * q[1]);
}

/* Rotation Matrix to quaternion */
static void rot2quat (rot, q)
double q[4], rot[3][3];
{
  int i;

  q[3] = 0.5 * sqrt (1 + rot[0][0] + rot[1][1] + rot[2][2]);

  if (fabs(q[3]) < 1e-6) {
    fprintf(stderr, "Quaternion reachs singularity\n");
    /*exit(-1);*/
    q[0] = 0;
    q[1] = 0;
    q[2] = 0;
    q[3] = 1;
    return;
  }

  q[0] = (rot[2][1] - rot[1][2]) / (4.0 * q[3]);
  q[1] = (rot[0][2] - rot[2][0]) / (4.0 * q[3]);
  q[2] = (rot[1][0] - rot[0][1]) / (4.0 * q[3]);

  for (i = 0; i < 4; i++) 
    if (fabs(q[i]) > 1.0) {

      fprintf(stderr, "Incorrect Quaternion: (%e %e %e %e)\n",
	      q[0], q[1], q[2], q[3]);
    /*exit(-1);*/
    q[0] = 0;
    q[1] = 0;
    q[2] = 0;
    q[3] = 1;
    return;
    }
}

/* roll/pitch/yaw  to rotation matrix */

static void rpy2rot (rpy, rot, d_rot, fonly)
double rpy[3], rot[3][3], d_rot[3][3][3];
int fonly;
{
  double ca, cb, cr, sa, sb, sr;

  ca = cos(rpy[0]);
  sa = sin(rpy[0]);

  cb = cos(rpy[1]);
  sb = sin(rpy[1]);

  cr = cos(rpy[2]);
  sr = sin(rpy[2]);

  rot[0][0] = ca * cb;
  rot[0][1] = ca * sb * sr - sa * cr;
  rot[0][2] = ca * sb * cr + sa * sr;

  rot[1][0] = sa * cb;
  rot[1][1] = sa * sb * sr + ca * cr;
  rot[1][2] = sa * sb * cr - ca * sr;

  rot[2][0] = - sb;
  rot[2][1] = cb * sr;
  rot[2][2] = cb * cr;

  if (! fonly) {

    d_rot[0][0][0] = - sa * cb;
    d_rot[0][0][1] = - ca * sb;
    d_rot[0][0][2] = 0.0;

    d_rot[0][1][0] = - sa * sb * sr - ca * cr;
    d_rot[0][1][1] = ca * cb * sr;
    d_rot[0][1][2] = ca * sb * cr + sa * sr;

    d_rot[0][2][0] = - sa * sb * cr + ca * sr;
    d_rot[0][2][1] = ca * cb * cr;
    d_rot[0][2][2] = - ca * sb * sr + sa * cr;

    d_rot[1][0][0] = ca * cb;
    d_rot[1][0][1] = - sa * sb;
    d_rot[1][0][2] = 0.0;

    d_rot[1][1][0] = ca * sb * sr - sa * cr;
    d_rot[1][1][1] = sa * cb * sr;
    d_rot[1][1][2] = sa * sb * cr - ca * sr;

    d_rot[1][2][0] = ca * sb * cr + sa * sr;
    d_rot[1][2][1] = sa * cb * cr;
    d_rot[1][2][2] = - sa * sb * sr - ca * cr;

    d_rot[2][0][0] = 0.0;
    d_rot[2][0][1] = - cb;
    d_rot[2][0][2] = 0.0;

    d_rot[2][1][0] = 0.0;
    d_rot[2][1][1] = - sb * sr;
    d_rot[2][1][2] = cb * cr;

    d_rot[2][2][0] = 0.0;
    d_rot[2][2][1] = - sb * cr;
    d_rot[2][2][2] = - cb * sr;
  }
}

/* rotation matrix to roll/pitch/yaw */

static void rot2rpy (rot, rpy)
double rot[3][3], rpy[3];
{
  double cb;

  rpy[1] = atan2 (- rot[2][0],
		  sqrt(rot[0][0] * rot[0][0] + rot[1][0] * rot[1][0]));

  if (fabs(rpy[1] - PI / 2.0) < 1e-4) {

    fprintf(stderr, "Singular point in roll/pitch/yaw\n");

    /*exit(-1);*/
    rpy[0] = 0;
    rpy[1] = 0;
    rpy[2] = 0;
  }

  cb = cos (rpy[1]);

  rpy[0] = atan2 (rot[1][0] / cb, rot[0][0] / cb);

  rpy[2] = atan2 (rot[2][1] / cb, rot[2][2] / cb);
}

/* Old to New using quaternion */
 
static void old2new (A, H, V, O, q, Cp, Scale, Co)
double A[3], H[3], V[3], O[3];
double q[4], Cp[2], Scale[2], Co[2];
{
  static double AxH[3], AxV[3], T[3][3], tmp_vec[3], Hs, As, Vs;
  int i;

  Cp[0] = vect_dot_3d (A, H);
  Cp[1] = vect_dot_3d (A, V);

  vect_cross_3d (A, H, AxH);
  vect_cross_3d (A, V, AxV);

  Scale[0] = Hs = sqrt(vect_dot_3d (AxH, AxH));
  Vs = sqrt(vect_dot_3d (AxV, AxV));
  Scale[1] = As = Vs / Hs;

  Co[0] = 2.0 * vect_dot_3d (A, H) - vect_dot_3d (O, H) / vect_dot_3d (O, A);
  Co[1] = (1.0 + 1.0 / (As * As)) * vect_dot_3d (A, V)
    - vect_dot_3d (O, V) / vect_dot_3d (O, A) / (As * As);

  vect_sub_3d (H, vect_scale_3d (Cp[0], A, tmp_vec),
	       tmp_vec);

  for (i = 0; i < 3; i++)
    T[i][0] = tmp_vec[i] / Hs;

  for (i = 0; i < 3; i++)
    T[i][1] = A[i];

  vect_sub_3d (V, vect_scale_3d (Cp[1], A, tmp_vec),
	       tmp_vec);

  for (i = 0; i < 3; i++)
    T[i][2] = tmp_vec[i] / (- Vs);

  rot2quat (T, q);
}

/* Old to New using roll/pitch/yaw */
 
static void old2new2 (C_old, C_new, d_new, fonly)
double C_old[18], C_new[15], d_new[15][18];
int fonly;
{
  static double AxH[3], AxV[3], T[3][3], tmp_vec[3], Hs, As, Vs;
  double *A, *H, *V, *O, *rpy, *Cp, *Scale, *Co;
  int i, j;

  for (i = 0; i < 3; i++) {
    C_new[i] = C_old[i];
    C_new[i+12] = C_old[i+15];
  }

  A = &(C_old[3]);   H = &(C_old[6]);   
  V = &(C_old[9]);   O = &(C_old[12]);

  rpy = &(C_new[3]); Cp = &(C_new[6]);
  Scale = &(C_new[8]); Co = &(C_new[10]);

  Cp[0] = vect_dot_3d (A, H);
  Cp[1] = vect_dot_3d (A, V);

  vect_cross_3d (A, H, AxH);
  vect_cross_3d (A, V, AxV);

  Scale[0] = Hs = sqrt(vect_dot_3d (AxH, AxH));
  Vs = sqrt(vect_dot_3d (AxV, AxV));
  Scale[1] = As = Vs / Hs;

  Co[0] = 2.0 * vect_dot_3d (A, H) - vect_dot_3d (O, H) / vect_dot_3d (O, A);
  Co[1] = (1.0 + 1.0 / (As * As)) * vect_dot_3d (A, V)
    - vect_dot_3d (O, V) / vect_dot_3d (O, A) / (As * As);

  vect_sub_3d (H, vect_scale_3d (Cp[0], A, tmp_vec),
	       tmp_vec);

  for (i = 0; i < 3; i++)
    T[i][0] = tmp_vec[i] / Hs;

  for (i = 0; i < 3; i++)
    T[i][1] = A[i];

  vect_sub_3d (V, vect_scale_3d (Cp[1], A, tmp_vec),
	       tmp_vec);

  for (i = 0; i < 3; i++)
    T[i][2] = tmp_vec[i] / (- Vs);

  rot2rpy (T, rpy);

  if (! fonly) {
    static double tmp_vec1[3], tmp_vec2[3], tmp_mat[3][3];
    static double OdA, AdH, OdH, AdV, OdV;
    static double r11_A[3], r11_H[3], r21_A[3], r21_H[3], r31_A[3], r31_H[3],
           r33_A[3], r33_V[3], stmp, stmp2;
    
    OdH = vect_dot_3d (O, H);
    OdA = vect_dot_3d (O, A);
    OdV = vect_dot_3d (O, V);
    AdH = vect_dot_3d (A, H);
    AdV = vect_dot_3d (A, V);

    for (i = 0; i < 15; i++)
      for (j = 0; j < 18; j++)
	d_new[i][j] = 0.0;

    /* dC */

    for (i = 0; i < 3; i ++)
      d_new[i][i] = 1.0;

    /* dR */

    for (i = 0; i < 3; i++)
      d_new[i+12][i+15] = 1.0;
 
    /* dCp */

    for (i = 0; i < 3; i++) {
      d_new[6][3+i] = H[i]; /* Cp[0] w.r.t. A */
      d_new[6][6+i] = A[i]; /* Cp[0] w.r.t. H */
      d_new[7][3+i] = V[i]; /* Cp[1] w.r.t. A */
      d_new[7][9+i] = A[i]; /* Cp[1] w.r.t. V */
    }

    /* dHs, dAs */
    vect_cross_3d (A, H, tmp_vec1);
    vect_cross_3d (H, tmp_vec1, tmp_vec2);
    for (i = 0; i < 3; i++)
      d_new[8][3+i] = tmp_vec2[i] / Hs; /* Hs w.r.t. A */
    vect_cross_3d (A, tmp_vec1, tmp_vec2);
    for (i = 0; i < 3; i++)
      d_new[8][6+i] = - tmp_vec2[i] / Hs; /* Hs w.r.t. H */

    vect_cross_3d (A, V, tmp_vec1);
    vect_cross_3d (V, tmp_vec1, tmp_vec2);
    for (i = 0; i < 3; i++)
      d_new[9][3+i] = tmp_vec2[i] / Vs; /* Vs w.r.t. A */
    vect_cross_3d (A, tmp_vec1, tmp_vec2);
    for (i = 0; i < 3; i++)
      d_new[9][9+i] = - tmp_vec2[i] / Vs; /* Vs w.r.t. V */

    for (i = 0; i < 9; i++)
      d_new[9][3+i] = - Vs / (Hs * Hs) * d_new[8][3+i] +
	d_new[9][3+i] / Hs;  /* As w.r.t. A H V */

    /* dCo */
    vect_scale_3d (OdH / (OdA * OdA), O, tmp_vec1);
    for (i = 0; i < 3; i++)
      d_new[10][i+3] = 2.0 * H[i] + tmp_vec1[i]; /* Co[0] w.r.t. A */
    vect_scale_3d (-1.0 / OdA, O, tmp_vec1);
    for (i = 0; i < 3; i++)
      d_new[10][6+i] = 2.0 * A[i] + tmp_vec1[i]; /* Co[0] w.r.t. H */
    vect_scale_3d (- 1.0 / OdA, H, tmp_vec1);
    vect_scale_3d (OdH / (OdA * OdA), A, tmp_vec2);
    for (i = 0; i < 3; i++)
      d_new[10][12+i] = tmp_vec1[i] + tmp_vec2[i]; /* Co[0] w.r.t. O */

    AdV = vect_dot_3d (A, V);
    vect_scale_3d ((1.0 + 1.0 / (As * As)), V, tmp_vec1);
    vect_add_3d (vect_scale_3d ((AdV - OdV / OdA) * (- 2.0 / (As * As * As)),
				&(d_new[9][3]), tmp_vec2),
		 tmp_vec1, tmp_vec1);
    vect_add_3d (vect_scale_3d (OdA / (As * As * OdA * OdA), O,
				tmp_vec2),
		 tmp_vec1, tmp_vec1);
    vect_add_3d (vect_scale_3d (OdV / (OdA * OdA) / (As * As), O,
				tmp_vec2), tmp_vec1, tmp_vec1);
    for (i = 0; i < 3; i++)
      d_new[11][3+i] = tmp_vec1[i]; /* Co[1] w.r.t. A */
    vect_scale_3d ((AdV - OdV / OdA) * (-2.0 / (As * As * As)),
		   &(d_new[9][6]), tmp_vec1);
    for (i = 0; i < 3; i++)
      d_new[11][6+i] = tmp_vec1[i]; /* Co[1] w.r.t. H */
    vect_scale_3d ((1.0 + 1.0 / (As * As)), A, tmp_vec1);
    vect_add_3d (vect_scale_3d ((- 1.0 / (As * As)) / OdA, O, tmp_vec2),
		 tmp_vec1, tmp_vec1);
    vect_add_3d (vect_scale_3d ((AdV - OdV / OdA) * (-2.0 / (As * As * As)),
				&(d_new[9][9]), tmp_vec2),
		 tmp_vec1, tmp_vec1);
    for (i = 0; i < 3; i++)
      d_new[11][9+i] = tmp_vec1[i]; /* Co[1] w.r.t. V */
    vect_scale_3d (- OdA / (As * As), V, tmp_vec1);
    vect_scale_3d (OdV / (OdA * OdA) / (As * As), A, tmp_vec2);
    for (i = 0; i < 3; i++)
      d_new[11][12+i] = tmp_vec1[i] + tmp_vec2[i]; /* Co[1] w.r.t. O */

    /* d alpha, beta, gamma */
    
    /* dr11 w.r.t. A, H */
    vect_scale_3d (- A[0] / Hs, H, tmp_vec1);
    vect_scale_3d ((- H[0] + AdH * A[0]) / (Hs * Hs), &(d_new[8][3]), tmp_vec2);
    r11_A[0] = tmp_vec1[0] + tmp_vec2[0] - AdH / Hs;
    r11_A[1] = tmp_vec1[1] + tmp_vec2[1];
    r11_A[2] = tmp_vec1[2] + tmp_vec2[2];
    vect_scale_3d (- A[0] / Hs, A, tmp_vec1);
    vect_scale_3d ((- H[0] + AdH * A[0]) / (Hs * Hs), &(d_new[8][6]), tmp_vec2);
    r11_H[0] = tmp_vec1[0] + tmp_vec2[0] + 1.0 / Hs;
    r11_H[1] = tmp_vec1[1] + tmp_vec2[1];
    r11_H[2] = tmp_vec1[2] + tmp_vec2[2];

    /* dr21 w.r.t. A, H */
    vect_scale_3d (- A[1] / Hs, H, tmp_vec1);
    vect_scale_3d ((- H[1] + AdH * A[1]) / (Hs * Hs), &(d_new[8][3]), tmp_vec2);
    r21_A[0] = tmp_vec1[0] + tmp_vec2[0];
    r21_A[1] = tmp_vec1[1] + tmp_vec2[1] - AdH / Hs;
    r21_A[2] = tmp_vec1[2] + tmp_vec2[2];
    vect_scale_3d (- A[1] / Hs, A, tmp_vec1);
    vect_scale_3d ((- H[1] + AdH * A[1]) / (Hs * Hs), &(d_new[8][6]), tmp_vec2);
    r21_H[0] = tmp_vec1[0] + tmp_vec2[0];
    r21_H[1] = tmp_vec1[1] + tmp_vec2[1] + 1.0 / Hs;
    r21_H[2] = tmp_vec1[2] + tmp_vec2[2];

    vect_scale_3d (- A[2] / Hs, H, tmp_vec1);
    vect_scale_3d ((- H[2] + AdH * A[2]) / (Hs * Hs), &(d_new[8][3]), tmp_vec2);
    r31_A[0] = tmp_vec1[0] + tmp_vec2[0];
    r31_A[1] = tmp_vec1[1] + tmp_vec2[1];
    r31_A[2] = tmp_vec1[2] + tmp_vec2[2] - AdH / Hs;
    vect_scale_3d (- A[2] / Hs, A, tmp_vec1);
    vect_scale_3d ((- H[2] + AdH * A[2]) / (Hs * Hs), &(d_new[8][6]), tmp_vec2);
    r31_H[0] = tmp_vec1[0] + tmp_vec2[0];
    r31_H[1] = tmp_vec1[1] + tmp_vec2[1];
    r31_H[2] = tmp_vec1[2] + tmp_vec2[2] + 1.0 / Hs;

    vect_add_3d (vect_scale_3d (Hs, &(d_new[9][3]), tmp_vec1),
		 vect_scale_3d (As, &(d_new[8][3]), tmp_vec2),
		 tmp_vec2);
    vect_scale_3d ((V[2] - AdV * A[2]) / (Vs * Vs), 
		   tmp_vec2, tmp_vec2);
    vect_scale_3d (A[2] / Vs, V, tmp_vec1);
    r33_A[0] = tmp_vec1[0] + tmp_vec2[0];
    r33_A[1] = tmp_vec1[1] + tmp_vec2[1];
    r33_A[2] = tmp_vec1[2] + tmp_vec2[2] + AdV / Vs;

    vect_add_3d (vect_scale_3d (Hs, &(d_new[9][9]), tmp_vec1),
		 vect_scale_3d (As, &(d_new[8][9]), tmp_vec2),
		 tmp_vec2);
    vect_scale_3d ((V[2] - AdV * A[2]) / (Vs * Vs),
		    tmp_vec2, tmp_vec2);
    vect_scale_3d (A[2] / Vs, A, tmp_vec1);
    r33_V[0] = tmp_vec1[0] + tmp_vec2[0];
    r33_V[1] = tmp_vec1[1] + tmp_vec2[1];
    r33_V[2] = tmp_vec1[2] + tmp_vec2[2] - 1.0 / Vs;

    stmp2 = (T[0][0] * T[0][0] + T[1][0] * T[1][0]);
    stmp = sqrt(stmp2);
    vect_add_3d (vect_scale_3d (T[0][0] / stmp2, r21_A, tmp_vec1),
		 vect_scale_3d (- T[1][0] / stmp2, r11_A, tmp_vec2),
		 tmp_vec1);
    for (i = 0; i < 3; i++)
      d_new[3][i+3] = tmp_vec1[i]; /* dalpha w.r.t. A */
    vect_add_3d (vect_scale_3d (T[0][0] / stmp2, r21_H, tmp_vec1),
		 vect_scale_3d (- T[1][0] / stmp2, r11_H, tmp_vec2),
		 tmp_vec1);
    for (i = 0; i < 3; i++)
      d_new[3][i+6] = tmp_vec1[i]; /* dalpha w.r.t. H */

    vect_add_3d (vect_scale_3d (- stmp, r31_A, tmp_vec1),
		 vect_scale_3d (T[2][0] * T[0][0] / stmp, r11_A, tmp_vec2),
		 tmp_vec1);
    vect_add_3d (tmp_vec1, 
		 vect_scale_3d (T[2][0] * T[1][0] / stmp, r21_A, tmp_vec2),
		 tmp_vec1);
    for (i = 0; i < 3; i++)
      d_new[4][i+3] = tmp_vec1[i]; /* dbeta w.r.t. A */
    vect_add_3d (vect_scale_3d (- stmp, r31_H, tmp_vec1),
		 vect_scale_3d (T[2][0] * T[0][0] / stmp, r11_H, tmp_vec2),
		 tmp_vec1);
    vect_add_3d (tmp_vec1, 
		 vect_scale_3d (T[2][0] * T[1][0] / stmp, r21_H, tmp_vec2),
		 tmp_vec1);
    for (i = 0; i < 3; i++)
      d_new[4][i+6] = tmp_vec1[i]; /* dbeta w.r.t. H */

    stmp2 = T[2][1] * T[2][1] + T[2][2] * T[2][2];
    vect_scale_3d (- T[2][1] / stmp2, r33_A, tmp_vec1);
    d_new[5][3] = tmp_vec1[0]; /* dgamma w.r.t. A */
    d_new[5][4] = tmp_vec1[1];
    d_new[5][5] = tmp_vec1[2] + T[2][2] / stmp2;
    vect_scale_3d (- T[2][1] / stmp2, r33_V, tmp_vec1);
    for (i = 0; i < 3; i++)
      d_new[5][9+i] = tmp_vec1[i]; /* dgamma w.r.t. V */
  }
}
 
/* New to Old using quaternion */

static void new2old (A, H, V, O, q, Cp, Scale, Co)
double A[3], H[3], V[3], O[3];
double q[4], Cp[2], Scale[2], Co[2];
{
  static double T[3][3], O0[3], tmp, norm, Hs, As;
  int i;

  Hs = Scale[0];
  As = Scale[1];

  quat2rot (q, T);

  for (i = 0; i < 3; i++)
    A[i] = T[i][1];

  for (i = 0; i < 3; i++) 

    H[i] = T[i][0] * Hs + T[i][1] * Cp[0];

  for (i = 0; i < 3; i++)
    V[i] = T[i][1] * Cp[1] - Hs * As * T[i][2];

  for (i = 0; i < 3; i++)
    O0[i] = T[i][0] * (Cp[0] - Co[0]) + T[i][1] * Hs
      - T[i][2] * (Cp[1] - Co[1]) * As;

  norm = sqrt(vect_dot_3d (O0, O0));

  for (i = 0; i < 3; i++)
    O[i] = O0[i] / norm;
}

/* New to Old using roll/pitch/yaw */

static void new2old2 (C_old, C_new, d_old, fonly)
double C_old[18], C_new[15], d_old[18][15];
int fonly;
{
  static double T[3][3], O0[3], tmp, norm, Hs, As;
  int i, j, k;
  double *A, *H, *V, *O, *rpy, *Cp, *Scale, *Co;

  for (i = 0; i < 3; i++) {
    C_old[i] = C_new[i];
    C_old[i+15] = C_new[i+12];
  }

  A = &(C_old[3]);   H = &(C_old[6]);   
  V = &(C_old[9]);   O = &(C_old[12]);

  rpy = &(C_new[3]); Cp = &(C_new[6]);
  Scale = &(C_new[8]); Co = &(C_new[10]);

  Hs = Scale[0];
  As = Scale[1];

  rpy2rot (rpy, T, (double (*)[3][3])NULL, TRUE);

  for (i = 0; i < 3; i++)
    A[i] = T[i][1];

  for (i = 0; i < 3; i++) 

    H[i] = T[i][0] * Hs + T[i][1] * Cp[0];

  for (i = 0; i < 3; i++)
    V[i] = T[i][1] * Cp[1] - Hs * As * T[i][2];

  for (i = 0; i < 3; i++)
    O0[i] = T[i][0] * (Cp[0] - Co[0]) + T[i][1] * Hs
      - T[i][2] * (Cp[1] - Co[1]) * As;

  norm = sqrt(vect_dot_3d (O0, O0));

  for (i = 0; i < 3; i++)
    O[i] = O0[i] / norm;

  if (! fonly) {
    static double d_rot[3][3][3], rot[3][3];

    rpy2rot (rpy, rot, d_rot, FALSE);

    for (i = 0; i < 18; i++)
      for (j = 0; j < 15; j++)
	d_old[i][j] = 0.0;

    /* dC */

    for (i = 0; i < 3; i ++)
      d_old[i][i] = 1.0;

    /* dR */

    for (i = 0; i < 3; i++)
      d_old[i+15][i+12] = 1.0;

    /* dA */

    for (i = 0; i < 3; i++)
      for (j = 0; j < 3; j++)
	d_old[i+3][j+3] = d_rot[i][1][j];

    /* dH */

    for (i = 0; i < 3; i++) 
      for (j = 0; j < 3; j++) 
	d_old[i+6][j+3] = d_rot[i][0][j] * Hs + d_rot[i][1][j] * Cp[0];

    for (i = 0; i < 3; i++)
      d_old[i+6][6] = rot[i][1];

    for (i = 0; i < 3; i++)
      d_old[i+6][8] = rot[i][0];

    /* dV */

    for (i = 0; i < 3; i++) 
      for (j = 0; j < 3; j++) 
	d_old[i+9][j+3] = d_rot[i][1][j] * Cp[1] - d_rot[i][2][j] * Hs * As;

    for (i = 0; i < 3; i++)
      d_old[i+9][7] = rot[i][1];
    
    for (i = 0; i < 3; i++)
      d_old[i+9][8] = - rot[i][2] * As;

    for (i = 0; i < 3; i++)
      d_old[i+9][9] = - rot[i][2] * Hs;

    /* dO */

    for (i = 0; i < 3; i++) 
      for (j = 0; j < 3; j++) 
	d_old[i+12][j+3] = (d_rot[i][0][j] * (Cp[0] - Co[0]) +
			    d_rot[i][1][j] * Hs + 
			    d_rot[i][2][j] * (Co[1] - Cp[1]) * As) / norm;

    for (i = 0; i < 3; i ++) {

      d_old[i+12][6] = rot[i][0] / norm;         /* w.r.t. Xp */
      d_old[i+12][7] = - rot[i][2] * As / norm;  /* w.r.t. Yp */
      d_old[i+12][8] = rot[i][1] / norm;         /* w.r.t. Hs */
      d_old[i+12][9] = rot[i][2] * (Co[1] - Cp[1]) / norm; /* w.r.t. As */
      d_old[i+12][10] = - rot[i][0] / norm;       /* w.r.t. Xo */
      d_old[i+12][11] = rot[i][2] * As / norm;    /* w.r.t. Yo */
    }

    for (i = 0; i < 3; i++) {

      d_old[i+12][6] -= O[i] / (norm * norm) * (Cp[0] - Co[0]);
      d_old[i+12][7] -= O[i] / (norm * norm) * (Cp[1] - Co[1]) * As * As;
      d_old[i+12][8] -= O[i] / (norm * norm) * Hs;
      d_old[i+12][9] -= O[i] / (norm * norm) * (Cp[1] - Co[1]) * (Cp[1] -
								 Co[1]) * As;
      d_old[i+12][10] -= O[i] / (norm * norm) * (Co[0] - Cp[0]);
      d_old[i+12][11] -= O[i] / (norm * norm) * (Co[1] - Cp[1]) * As * As;
    }
  }
}

/* Converting both parameters and covariance matrix 
   for roll/pitch/yaw representation */

static void cov2old (C_new, cov_new, C_old, cov_old)
double C_new[15], cov_new[15][15], C_old[18], cov_old[18][18];
{
  static double d_old[18][15], tmp[18][15];
  int i, j, m, n;

  new2old2 (C_old, C_new, d_old, FALSE);

  for (i = 0; i < 18; i++)
    for (j = 0; j < 15; j++) {

      tmp[i][j] = 0.0;

      for (m = 0; m < 15; m++)
	tmp[i][j] += d_old[i][m] * cov_new[m][j];
    }

  for (i = 0; i < 18; i++)
    for (j = 0; j < 18; j++) {

      cov_old[i][j] = 0.0;

      for (m = 0; m < 15; m++)
	cov_old[i][j] += tmp[i][m] * d_old[j][m];
    }
}

static void cov2new (C_new, cov_new, C_old, cov_old)
double C_new[15], cov_new[15][15], C_old[18], cov_old[18][18];
{
  static double d_new[15][18], tmp[15][18];
  int i, j, m, n;

  old2new2 (C_old, C_new, d_new, FALSE);

  for (i = 0; i < 15; i++)
    for (j = 0; j < 18; j++) {

      tmp[i][j] = 0.0;

      for (m = 0; m < 18; m++)
	tmp[i][j] += d_new[i][m] * cov_old[m][j];
    }

  for (i = 0; i < 15; i++)
    for (j = 0; j < 15; j++) {

      cov_new[i][j] = 0.0;

      for (m = 0; m < 18; m++)
	cov_new[i][j] += tmp[i][m] * d_new[j][m];
    }
}

/*
static void test_cov_conversion (C_old)
double C_old[18];
{
  static double C_new[15], cov_old[18][18], cov_new[15][15];
  static double C2_old[18], C2_new[15], etmp;
  static double d_old[18][15], d_new[15][18], ident[18][18], delta[18];
  int i, j, k;

  old2new2 (C_old, C_new, d_new, FALSE);
  new2old2 (C_old, C_new, d_old, FALSE);
  old2new2 (C_old, C_new, d_new, FALSE);

  for (i = 0; i < 18; i++) delta[i] = 0.0;
  for (i = 12; i < 13; i++)
    delta[i] = 1e-3;

  for (i = 0; i < 18; i++)
    C2_old[i] = C_old[i] + delta[i];
  
  printf("Predicted Error:\n");
  for (i = 0; i < 15; i++) {

    etmp = 0.0;
    for (j = 0; j < 18; j++)
      etmp += d_new[i][j] * delta[j];
    printf("%e\n", etmp);
  }

  old2new2 (C2_old, C2_new, NULL, TRUE);
  printf("Actual Error:\n");
  for (i = 0; i < 15; i++)
    printf("%e\n", C2_new[i] - C_new[i]);
}

*/


/*****************************************************************
 *  C program:  Adjusting Camera Parameters to minimize the objective function
 *
 *                created by Yalin Xiong on PPT.IUS.CS.CMU.EDU
 *
 *             creation time:  Tue Oct 31 1995
 *             file     name:  /usr0/yx/jpl/self-calib/adjust_camera.c
 *****************************************************************/

/*#include <math.h>*/
/*#include <stdio.h>*/
/*#include <mathutil.h>*/
/*#include <self_calib.h>*/

/* deciding relative weights between features and 3D points*/
#define LAMBDA 0.005 /* Note that the covariance from correlation is
unusually large */

/* Compute Jacobian of 3D points on the hood */

static void Jacobian_hood (point, img_pos, disp, lorr, cam_para, J)
double point[3], img_pos[2], disp[2];
double J[2][30], cam_para[15];
int lorr;
{
  static double ray[3], k;
  static double *C, *A, *H, *V, *O, *R, C_old[18];
  static double dC[2][3], dr[2][3], dC0[2][3], dA[2][3], dH[2][3];
  static double dV[2][3], dO[2][3], dR[2][3], d_old[18][15];
  static double dk[2], tmp_J[2][18], correct_pos[2];
  int i, j, offset, l;

  new2old2 (C_old, cam_para, d_old, FALSE);
  C = &(C_old[0]); A = &(C_old[3]); H = &(C_old[6]);
  V = &(C_old[9]); O = &(C_old[12]); R = &(C_old[15]);

  k = 0.0;
  for (i = 0; i < 3; i++) 
    k += (point[i] - C[i]) * (point[i] - C[i]);
  k = sqrt(k);
  for (i = 0; i < 3; i++) ray[i] = (point[i] - C[i]) / k;

  ray2img (correct_pos, C, k, ray, C, A, H, V, O, R,
	   dC0, dk, dr, dC, dA, dH, dV, dO, dR, FALSE);

  for (i = 0; i < 2; i++)
    for (j = 0; j < 3; j++) {
      tmp_J[i][j] = dC[i][j];
      tmp_J[i][j+3] = dA[i][j];
      tmp_J[i][j+6] = dH[i][j];
      tmp_J[i][j+9] = dV[i][j];
      tmp_J[i][j+12] = dO[i][j];
      tmp_J[i][j+15] = dR[i][j];
    }

  for (i = 0; i < 2; i++)
    for (j = 0; j < 30; j++)
      J[i][j] = 0.0;

  if (lorr == LEFT) 
    offset = 0;
  else
    offset = 15;

  for (i = 0; i < 2; i++)
    for (j = 0; j < 15; j++) 
      for (l = 0; l < 18; l++)
	J[i][j+offset] += tmp_J[i][l] * d_old[l][j];

  for (i = 0; i < 2; i++) disp[i] = img_pos[i] - correct_pos[i];
}
  
static void Jacobian_hood2 (point, img_pos, disp, lorr, cam_old, trans, J)
double point[3], img_pos[2], disp[2];
double J[2][30], cam_old[18], trans[18][15];
int lorr;
{
  static double ray[3], k;
  double *C, *A, *H, *V, *O, *R;
  static double dC[2][3], dr[2][3], dC0[2][3], dA[2][3], dH[2][3];
  static double dV[2][3], dO[2][3], dR[2][3];
  static double dk[2], tmp_J[2][18], correct_pos[2];
  int i, j, offset, l;

  C = &(cam_old[0]); A = &(cam_old[3]); H = &(cam_old[6]);
  V = &(cam_old[9]); O = &(cam_old[12]); R = &(cam_old[15]);

  k = 0.0;
  for (i = 0; i < 3; i++) 
    k += (point[i] - C[i]) * (point[i] - C[i]);
  k = sqrt(k);
  for (i = 0; i < 3; i++) ray[i] = (point[i] - C[i]) / k;

  ray2img (correct_pos, C, k, ray, C, A, H, V, O, R,
	   dC0, dk, dr, dC, dA, dH, dV, dO, dR, FALSE);

  for (i = 0; i < 2; i++)
    for (j = 0; j < 3; j++) {
      tmp_J[i][j] = dC[i][j];
      tmp_J[i][j+3] = dA[i][j];
      tmp_J[i][j+6] = dH[i][j];
      tmp_J[i][j+9] = dV[i][j];
      tmp_J[i][j+12] = dO[i][j];
      tmp_J[i][j+15] = dR[i][j];
    }

  for (i = 0; i < 2; i++)
    for (j = 0; j < 30; j++)
      J[i][j] = 0.0;

  if (lorr == LEFT) 
    offset = 0;
  else
    offset = 15;

  for (i = 0; i < 2; i++)
    for (j = 0; j < 15; j++) 
      for (l = 0; l < 18; l++)
	J[i][j+offset] += tmp_J[i][l] * trans[l][j];

  for (i = 0; i < 2; i++) disp[i] = img_pos[i] - correct_pos[i];
}

static void error_hood (point, img_pos, disp, cam_old)
double point[3], img_pos[2], disp[2];
double cam_old[18];
{
  static double ray[3], k;
  double *C, *A, *H, *V, *O, *R;
  static double dk[2], correct_pos[2];
  int i, j;

  C = &(cam_old[0]); A = &(cam_old[3]); H = &(cam_old[6]);
  V = &(cam_old[9]); O = &(cam_old[12]); R = &(cam_old[15]);

  k = 0.0;
  for (i = 0; i < 3; i++) 
    k += (point[i] - C[i]) * (point[i] - C[i]);
  k = sqrt(k);
  for (i = 0; i < 3; i++) ray[i] = (point[i] - C[i]) / k;

  ray2img (correct_pos, C, k, ray, C, A, H, V, O, R,
	   NULL, dk, NULL, NULL, NULL, NULL, NULL, NULL, NULL, TRUE);

  for (i = 0; i < 2; i++) disp[i] = img_pos[i] - correct_pos[i];
}

static void error_feature (Cl, Cr, fl, fr, fr_cov, k, disp)
double Cl[18], Cr[18]; /* Left and Right camera parameters */
double fl[2], fr[2], fr_cov[2][2]; /* Features and their match */
double *k; /* depth */
double disp[2];
{
  static double r[3], dk[2], nearest[2];
  int i, j;

  img2ray (fl, &(Cl[3]), &(Cl[6]), &(Cl[9]), &(Cl[12]), &(Cl[15]),
	   r, NULL, NULL, NULL, NULL, NULL, TRUE);
  if (calc_error)
     return;
  min_dist2 (k, fr, fr_cov, &(Cl[0]), r, &(Cr[0]), &(Cr[3]), &(Cr[6]),
	     &(Cr[9]), &(Cr[12]), &(Cr[15]));

  ray2img (nearest, &(Cl[0]), *k, r, &(Cr[0]), &(Cr[3]), &(Cr[6]),
	   &(Cr[9]), &(Cr[12]), &(Cr[15]), NULL, dk, NULL,
	   NULL, NULL, NULL, NULL, NULL, NULL, TRUE);

  disp[0] = fr[0] - nearest[0];
  disp[1] = fr[1] - nearest[1];
}
  
/* Compute Jacobian w.r.t. the new explicit parametrization */

static void Jacobian_new (Cl, Cr, fl, fr, fr_cov, depth, disp, J)
double Cl[15], Cr[15];
double fl[2], fr[2], disp[2], fr_cov[2][2];
double J[2][31], *depth;
{
  static double Cl_old[18], Cr_old[18];
  double *C0, *A0, *H0, *V0, *O0, *R0;
  double *C1, *A1, *H1, *V1, *O1, *R1;
  static double d_old[18][15], trans[36][30], old_J[2][37];
  int i, j, k;

  for (i = 0; i < 36; i++)
    for (j = 0; j < 30; j++)
      trans[i][j] = 0.0;

  new2old2 (Cl_old, Cl, d_old, FALSE);

  for (i = 0; i < 18; i++)
    for (j = 0; j < 15; j++)
      trans[i][j] = d_old[i][j];

  new2old2 (Cr_old, Cr, d_old, FALSE);

  for (i = 0; i < 18; i++)
    for (j = 0; j < 15; j++)
      trans[i+18][j+15] = d_old[i][j];

  C0 = &(Cl_old[0]);  A0 = &(Cl_old[3]);   H0 = &(Cl_old[6]);
  V0 = &(Cl_old[9]);  O0 = &(Cl_old[12]);  R0 = &(Cl_old[15]);
  C1 = &(Cr_old[0]);  A1 = &(Cr_old[3]);   H1 = &(Cr_old[6]);
  V1 = &(Cr_old[9]);  O1 = &(Cr_old[12]);  R1 = &(Cr_old[15]);

  Jacobian (C0, A0, H0, V0, O0, R0, C1, A1, H1, V1, O1, R1, fl, fr, fr_cov, 
	    depth, disp, old_J);
  if (calc_error)
     return;

  for (i = 0; i < 2; i++) 
    for (j = 0; j < 30; j++) {

      J[i][j] = 0.0;

      for (k = 0; k < 36; k++)
	J[i][j] += old_J[i][k] * trans[k][j];
    }
    
  J[0][30] = old_J[0][36];
  J[1][30] = old_J[1][36];
}

static void Jacobian_new2 (Cl_old, Cr_old, trans, fl, fr, fr_cov, depth, disp, J)
double Cl_old[18], Cr_old[18], trans[36][30];
double fl[2], fr[2], disp[2], fr_cov[2][2];
double J[2][31], *depth;
{
  double *C0, *A0, *H0, *V0, *O0, *R0;
  double *C1, *A1, *H1, *V1, *O1, *R1;
  static double old_J[2][37];
  int i, j, k;

  C0 = &(Cl_old[0]);  A0 = &(Cl_old[3]);   H0 = &(Cl_old[6]);
  V0 = &(Cl_old[9]);  O0 = &(Cl_old[12]);  R0 = &(Cl_old[15]);
  C1 = &(Cr_old[0]);  A1 = &(Cr_old[3]);   H1 = &(Cr_old[6]);
  V1 = &(Cr_old[9]);  O1 = &(Cr_old[12]);  R1 = &(Cr_old[15]);

  Jacobian (C0, A0, H0, V0, O0, R0, C1, A1, H1, V1, O1, R1, fl, fr, fr_cov, 
	    depth, disp, old_J);
  if (calc_error)
     return;

  for (i = 0; i < 2; i++) 
    for (j = 0; j < 30; j++) {

      J[i][j] = 0.0;

      for (k = 0; k < 36; k++)
	J[i][j] += old_J[i][k] * trans[k][j];
    }
    
  J[0][30] = old_J[0][36];
  J[1][30] = old_J[1][36];
}

static void trans2old (Cl, Cr, Cl_old, Cr_old, trans, left_trans, right_trans)
double Cl[15], Cr[15];
double Cl_old[18], Cr_old[18], trans[36][30];
double left_trans[18][15], right_trans[18][15];
{
  int i, j;

  for (i = 0; i < 36; i++)
    for (j = 0; j < 30; j++)
      trans[i][j] = 0.0;

  new2old2 (Cl_old, Cl, left_trans, FALSE);

  for (i = 0; i < 18; i++)
    for (j = 0; j < 15; j++)
      trans[i][j] = left_trans[i][j];

  new2old2 (Cr_old, Cr, right_trans, FALSE);

  for (i = 0; i < 18; i++)
    for (j = 0; j < 15; j++)
      trans[i+18][j+15] = right_trans[i][j];
}

/* A function checking whether the current cameras are calibrated 
   camera parameters are in new model */

static void calibrated_P (features, nof, left3D, left_nop, right3D, right_nop,
		  current_cam, new_old, err_stat, verbose)
double features[][7]; /* Feature matches and their covariance */
double left3D[][8]; /* 3D points and their covariance for left camera */
double right3D[][8]; /* 3D Points and their covariance for right camera */
int nof, left_nop, right_nop, verbose;
int new_old; /* Specify whether the camera parameters are in new or old model */
double *current_cam; /* Input camera parameters */
float err_stat[6]; /* Output Error statistics:
		      0: feature square error
		      1: Left 3D point square error
		      2: Right 3D point square error

		      3-5: the same as 0-2 except that outliers are rejected
		           first.
		   */
{
  static double cov[2][2], camera_old[36], k, disp[2], *cam_p;
  float *errors, tmp;
  unsigned char *validity;
  int i, max_nop, feat_outlier, left_outlier, right_outlier;

  max_nop = ((nof > left_nop) ? nof : left_nop);
  max_nop = ((max_nop > right_nop) ? max_nop : right_nop);
  validity = (unsigned char *) malloc (max_nop * sizeof (unsigned char));
  errors = (float *) malloc (max_nop * 2 * sizeof (float));

  if (new_old == NEW_MODEL) {
    new2old2 (camera_old, current_cam, (double (*)[15])NULL, TRUE);
    cam_p = camera_old;
  } else
    cam_p = current_cam;

  for (i = 0; i < 6; i++) err_stat[i] = 0.0;

  if (nof > 0) {
    for (i = 0; i < nof; i++) {
      
      cov[0][0] = features[i][4];
      cov[0][1] = cov[1][0] = features[i][5];
      cov[1][1] = features[i][6];

      k = 1.0;
      error_feature (cam_p, &(cam_p[18]), &(features[i][0]),
		     &(features[i][2]), cov, &k, disp);
      if (calc_error)
	return;

      errors[i*2] = (float) disp[0];
      errors[i*2+1] = (float) disp[1];
      validity[i] = 1;
    }

    feat_outlier = X84 (errors, 2, nof, validity);

    for (i = 0; i < nof; i++) {

      tmp = errors[2*i] * errors[2*i] + errors[2*i+1] * errors[2*i+1];

      err_stat[0] += tmp;
      if (validity[i])
	err_stat[3] += tmp;
    }

    err_stat[0] = sqrt(err_stat[0] / (float) nof);
    if (feat_outlier < nof)
      err_stat[3] = sqrt(err_stat[3] / ((float) (nof - feat_outlier)));

    if (verbose) {
      printf("%d features are rejected \n", feat_outlier);
    }
  }

  if (left_nop > 0) {
    for (i = 0; i < left_nop; i++) {

      error_hood (&(left3D[i][0]), &(left3D[i][3]), disp, cam_p);
      errors[i*2] = (float) disp[0];
      errors[i*2+1] = (float) disp[1];
      validity[i] = 1;
    }

    left_outlier = X84 (errors, 2, left_nop, validity);

    for (i = 0; i < left_nop; i++) {

      tmp = errors[2*i] * errors[2*i] + errors[2*i+1] * errors[2*i+1];

      err_stat[1] += tmp;
      if (validity[i])
	err_stat[4] += tmp;
    }

    err_stat[1] = sqrt(err_stat[1] / (float) left_nop);
    if (left_outlier < left_nop)
      err_stat[4] = sqrt(err_stat[4] / ((float) (left_nop - left_outlier)));

    if (verbose) {
      printf("%d left 3D points are rejected \n", left_outlier);
    }
  }

  if (right_nop > 0) {
    for (i = 0; i < right_nop; i++) {

      error_hood (&(right3D[i][0]), &(right3D[i][3]), disp, &(cam_p[18]));
      errors[i*2] = (float) disp[0];
      errors[i*2+1] = (float) disp[1];
      validity[i] = 1;
    }

    right_outlier = X84 (errors, 2, right_nop, validity);

    for (i = 0; i < right_nop; i++) {

      tmp = errors[2*i] * errors[2*i] + errors[2*i+1] * errors[2*i+1];

      err_stat[2] += tmp;
      if (validity[i])
	err_stat[5] += tmp;
    }

    err_stat[2] = sqrt(err_stat[2] / (float) right_nop);
    if (right_outlier < right_nop)
      err_stat[5] = sqrt(err_stat[5] / ((float) (right_nop - right_outlier)));

    if (verbose) {
      printf("%d right 3D points are rejected \n", right_outlier);
    }
  }
}

static void update_cam (features, nof, ferr, depth, left3D, left_nop, lerr,
		 right3D, right_nop, rerr, apri_cam, apri_cov, priori_mode,
		 current_cam, post_cov, dcam, fixing, scale, num_para, 
		 chisq, residuals, validity, verbose)
double features[][7]; /* Feature matches and their uncertainty */
double *depth; /* Current estimation of features' depth values */
double apri_cam[30], apri_cov[30][30]; /* apriori camera parameters */
double current_cam[30], post_cov[30][30]; /* current camera parameters */
double dcam[30]; /* Update vector */
double left3D[][8], right3D[][8]; /* left and right 3D points*/
double scale[30]; /* Scale parameters */
double *chisq, *ferr, *lerr, *rerr; /* output error measures and chi-square */
int fixing[30], num_para;
int verbose; /* For debug purpose only */
float residuals[][2];
unsigned char *validity;
int nof, left_nop, right_nop, priori_mode;
/*Priori Mode: 0: Hard fixing, allowing zero change.
               1: Soft fixing, constrained by apriori weights.
	                     Artificial weights have already been included.
	       2: Soft fixing. constrained by apriori weights.
	                     Artificial weights are not included.
*/
{
  static double **A, B[30], tmp[2][31], **tmp2;
  static double cov[2][2], inv_cov[2][2], J[2][31], Jp[2][30], disp[2], N0[15][15];
  static double btmp[30], magnify;
  static double Cl_old[18], Cr_old[18], trans[36][30];
  static double left_trans[18][15], right_trans[18][15];
  double lambda;
  int i, j, k, row, col, dof, max_nop, outlier_nop;
  double **tmp_A, *tmp_B, *tmp_dx;
  FILE *aout;
  double left_chisq, right_chisq, priori_chisq;

  A = (double **) malloc (30 * sizeof (double *));
  for (i = 0; i < 30; i++)
    A[i] = (double *) malloc (30 * sizeof (double));

  for (i = 0; i < 30; i++) {
    B[i] = 0.0;
    for (j = 0; j < 30; j++) A[i][j] = 0.0;
  }

  if ((left_nop > 0 || right_nop > 0) && nof > 0)
    lambda = LAMBDA * ((double) nof / (left_nop + right_nop));
  else 
    lambda = 1.0;

  max_nop = ((nof > left_nop) ? nof : left_nop);
  max_nop = ((max_nop > right_nop) ? max_nop : right_nop);

  *ferr = *chisq = 0.0;
  if (verbose > 1) aout = fopen ("ferror.dat", "w");
  trans2old (&(current_cam[0]), &(current_cam[15]), 
	     Cl_old, Cr_old, trans, left_trans, right_trans);

  for (i = 0 ; i < nof; i ++) {

/*    printf("No. of features: %d\n", i); */

    if ( ! validity[i]) continue;

    cov[0][0] = features[i][4];
    cov[1][0] = cov[0][1] = features[i][5];
    cov[1][1] = features[i][6];
    mat_invert_d2(cov, inv_cov);
/*
    Jacobian_new (&(current_cam[0]), &(current_cam[15]),
		   &(features[i][0]), &(features[i][2]),
		   cov, &(depth[i]), disp, J);
*/
    Jacobian_new2 (Cl_old, Cr_old, trans,
		   &(features[i][0]), &(features[i][2]),
		   cov, &(depth[i]), disp, J);
    if (calc_error)
      goto ABORT1;

    residuals[i][0] = (float) disp[0];
    residuals[i][1] = (float) disp[1];
    if (verbose > 1) 
      fprintf(aout, "%f %f %f %f %f %f\n", 
	      features[i][0], features[i][1], features[i][2], features[i][3],
	      disp[0], disp[1]);

    if (verbose) {
      double rtmp[3];
      printf("Inside: %.2f (%.2f %.2f) (%.2f %.2f) depth: %f\n", 
	     sqrt(disp[0] * disp[0] + disp[1] * disp[1]),
	     features[i][0], features[i][1], features[i][2],features[i][3],
	     depth[i]);
      img2ray (&features[i][0], &Cl_old[3], &Cl_old[6],
	       &Cl_old[9], &Cl_old[12], &Cl_old[15], rtmp, 
	       NULL, NULL, NULL, NULL, NULL, TRUE);
      printf("\t Ray from left (%.4f %.4f %.4f)\n", rtmp[0],
	     rtmp[1], rtmp[2]);
      img2ray (&features[i][2], &Cr_old[3], &Cr_old[6],
	       &Cr_old[9], &Cr_old[12], &Cr_old[15], rtmp, 
	       NULL, NULL, NULL, NULL, NULL, TRUE);
      printf("\t Ray from right (%.4f %.4f %.4f)\n", rtmp[0],
	     rtmp[1], rtmp[2]);
    }

    tmp[0][0] = (double) 
      inv_cov[0][0] * disp[0] + inv_cov[0][1] * disp[1];
    tmp[0][1] = (double)
      inv_cov[1][0] * disp[0] + inv_cov[1][1] * disp[1];

    *ferr += disp[0] * disp[0] + disp[1] * disp[1];
    *chisq += disp[0] * tmp[0][0] + disp[1] * tmp[0][1];

    for (j = 0; j < 30; j++) 

      B[j] += (double) 
	J[0][j] * tmp[0][0] + J[1][j] * tmp[0][1];

    for (j = 0; j < 2; j++)
      for (k = 0; k < 30; k++)

	tmp[j][k] = (double)
	  inv_cov[j][0] * J[0][k] + inv_cov[j][1] * J[1][k];

    for (j = 0; j < 30; j++)
      for (k = 0; k < 30; k++)

	A[j][k] += (double)
	  J[0][j] * tmp[0][k] + J[1][j] * tmp[1][k];
  }

  if (nof > 0)
    *ferr = sqrt(*ferr / ((double) nof));

  if (verbose) {
    if (nof > 0)
      printf ("RMS distance between %d left and right features: %f\n",
	      nof, *ferr); 
    if (verbose > 1) {
      fclose (aout);
      aout = fopen ("lerror.dat", "w");
    }
  }

/*  printf("Constraining by Left hood points ...\n"); */

  *lerr = left_chisq = 0.0;
  for (i = 0; i < left_nop; i++) {

    if (! validity[i+nof]) continue;
    cov[0][0] = left3D[i][5];
    cov[0][1] = cov[1][0] = left3D[i][6];
    cov[1][1] = left3D[i][7];
    mat_invert_d2(cov, inv_cov);
/*
    Jacobian_hood (&(left3D[i][0]), &(left3D[i][3]),
		   disp, LEFT, &(current_cam[0]), Jp);
*/
    Jacobian_hood2 (&(left3D[i][0]), &(left3D[i][3]),
		    disp, LEFT, Cl_old, left_trans, Jp);
    if (calc_error)
      goto ABORT1;

    residuals[i+nof][0] = (float) disp[0];
    residuals[i+nof][1] = (float) disp[1];

    tmp[0][0] = (double) 
      inv_cov[0][0] * disp[0] + inv_cov[0][1] * disp[1];
    tmp[0][1] = (double)
      inv_cov[1][0] * disp[0] + inv_cov[1][1] * disp[1];

    if (verbose > 1) 
      fprintf(aout, "%f %f %f %f %f %f %f\n", left3D[i][0],
	      left3D[i][1], left3D[i][2], left3D[i][3], left3D[i][4], 
	      disp[0], disp[1]);
    *lerr += disp[0] * disp[0] + disp[1] * disp[1];
    left_chisq += disp[0] * tmp[0][0] + disp[1] * tmp[0][1];

    for (j = 0; j < 30; j++) 

      B[j] += (double) 
	(Jp[0][j] * tmp[0][0] + Jp[1][j] * tmp[0][1]) * lambda;

    for (j = 0; j < 2; j++)
      for (k = 0; k < 30; k++)

	tmp[j][k] = (double)
	  inv_cov[j][0] * Jp[0][k] + inv_cov[j][1] * Jp[1][k];

    for (j = 0; j < 30; j++)
      for (k = 0; k < 30; k++)

	A[j][k] += (double)
	  (Jp[0][j] * tmp[0][k] + Jp[1][j] * tmp[1][k]) * lambda;
  }

  if (left_nop > 0) 
    *lerr = sqrt(*lerr / ((double) left_nop));
  if (verbose) { 
    if (left_nop > 0) 
      printf(
	"RMS distance between %d predicted and current left 3D points: %f\n",
	     left_nop, *lerr);

    if (verbose > 1) {
      fclose (aout);
      aout = fopen ("rerror.dat", "w");
    }
  }

  *rerr = right_chisq = 0.0;
  for (i = 0; i < right_nop; i++) {

    if (! validity[i+left_nop+nof]) continue;
    cov[0][0] = right3D[i][5];
    cov[0][1] = cov[1][0] = right3D[i][6];
    cov[1][1] = right3D[i][7];
    mat_invert_d2(cov, inv_cov);
/*
    Jacobian_hood (&(right3D[i][0]), &(right3D[i][3]),
		   disp, RIGHT, &(current_cam[15]), Jp); 
*/
    Jacobian_hood2 (&(right3D[i][0]), &(right3D[i][3]),
		   disp, RIGHT, Cr_old, right_trans, Jp);
    if (calc_error)
      goto ABORT1;

    residuals[i+left_nop+nof][0] = (float) disp[0];
    residuals[i+left_nop+nof][1] = (float) disp[1];
    tmp[0][0] = (double) 
      inv_cov[0][0] * disp[0] + inv_cov[0][1] * disp[1];
    tmp[0][1] = (double)
      inv_cov[1][0] * disp[0] + inv_cov[1][1] * disp[1];

    if (verbose > 1) 
      fprintf(aout, "%f %f %f %f %f %f %f\n", right3D[i][0], right3D[i][1], 
	      right3D[i][2], right3D[i][3], right3D[i][4], disp[0], disp[1]);
    *rerr += disp[0] * disp[0] + disp[1] * disp[1];
    right_chisq += disp[0] * tmp[0][0] + disp[1] * tmp[0][1];

    for (j = 0; j < 30; j++) 

      B[j] += (double) 
	(Jp[0][j] * tmp[0][0] + Jp[1][j] * tmp[0][1]) * lambda;

    for (j = 0; j < 2; j++)
      for (k = 0; k < 30; k++)

	tmp[j][k] = (double)
	  inv_cov[j][0] * Jp[0][k] + inv_cov[j][1] * Jp[1][k];

    for (j = 0; j < 30; j++)
      for (k = 0; k < 30; k++)

	A[j][k] += (double)
	  (Jp[0][j] * tmp[0][k] + Jp[1][j] * tmp[1][k]) * lambda;
  }

  if (right_nop > 0)
    *rerr = sqrt(*rerr / ((double) right_nop));
  if (verbose) {

    if (verbose > 1) fclose (aout);
    if (right_nop > 0)
      printf(
	"RMS distance between %d predicted and current right 3D points: %f\n",
	     right_nop, *rerr);
  }

  /* Penalize bias from a priori parameters */

  priori_chisq = 0.0;
  for (i = 0; i < 30; i++) 
    tmp[0][i] = (double) apri_cam[i] - current_cam[i];

  if (priori_mode != 0) { /* i.e. apriori covar counts */

    tmp2 = (double **) malloc (30 * sizeof (double *));
    for (i = 0; i < 30; i++) 
      tmp2[i] = (double *) malloc (30 * sizeof (double));

    for (row = 0; row < 30; row ++)
      for (col = 0; col < 30; col ++)
	tmp2[row][col] = apri_cov[row][col];

    /* Inverse the matrix in double precision */

    invt_matrixd (tmp2, 30);
    
    /* Soft Fixing */
    magnify = 0.0; /* For non-fixed parameters */
    for (row = 0; row < 30; row ++)
      if (! fixing[row])
	for (col = 0; col < 30; col ++)
	  tmp2[row][col] *= magnify;
    for (col = 0; col < 30; col ++)
      if (! fixing[col])
	for (row = 0; row < 30; row ++)
	  tmp2[row][col] *= magnify;

    for (i = 0; i < 30; i++) {
      btmp[i] = 0.0;
      for (j = 0; j < 30; j++)
	btmp[i] += tmp2[i][j] * tmp[0][j];
    }

    for (i = 0; i < 30; i++)
      priori_chisq += btmp[i] * tmp[0][i];

    for (i = 0; i < 30; i++) {
      B[i] += btmp[i];
      for (j = 0; j < 30; j++)
	A[i][j] += tmp2[i][j];
    }

    for (i = 0; i < 30; i++) free(tmp2[i]);
    free (tmp2);
  }

  *chisq += left_chisq + right_chisq + priori_chisq;
  if (verbose) printf("Overall Chi-Square: %f\n", *chisq);

  /* Apriori Weights if the original covar doesn't include them */
  if (priori_mode == 2 || priori_mode == 0) {
    apriori_weights_new (&(current_cam[0]), N0);

    for (i = 0; i < 15; i++)
      for (j = 0; j < 15; j++) {
	A[i][j] += (double) N0[i][j];
      }

    for (i = 0; i < 15; i++) 
      for (j = 0; j < 15; j++) 
	B[i] += (double) N0[i][j] * tmp[0][j];

    apriori_weights_new (&(current_cam[15]), N0);
    for (i = 0; i < 15; i++)
      for (j = 0; j < 15; j++) {
	A[i+15][j+15] += (double) N0[i][j]; 
      }

    for (i = 0; i < 15; i++) 
      for (j = 0; j < 15; j++) 
	B[i+15] += (double) N0[i][j] * tmp[0][j+15];
  }

  if (priori_mode == 0) {
    dof = 0;
    for (i = 0; i < 30; i++)
      if (fixing[i] == 0) dof ++;
  } else 
    dof = 30;

  tmp_A = (double **) malloc (dof * sizeof (double *));
  for (i = 0; i < dof; i++)
    tmp_A[i] = (double *) malloc (dof * sizeof (double));
  tmp_B = (double *) malloc (dof * sizeof (double));
  tmp_dx = (double *) malloc (dof * sizeof (double));

  row = 0;
  for (i = 0; i < 30; i++) 
    if (fixing[i] == 0 || priori_mode != 0) {
      tmp_B[row] = B[i] * scale[i];
      
      col = 0;
      for (j = 0; j < 30; j++) 
	if (fixing[j] == 0 || priori_mode != 0) {
	  tmp_A[row][col] = A[i][j] * scale[i] * scale[j];
	  col ++;
	}
      row ++;
    }

  singular_mat2 (dof, tmp_A, tmp_B, tmp_dx, num_para);
/*
  singular_mat (dof, tmp_A, tmp_B, tmp_dx); */
  row = 0;
  for (i = 0; i < 30; i++)
    if (fixing[i] == 0 || priori_mode != 0) {
      dcam[i] = tmp_dx[row] * scale[i];
      row ++;
    } else {
      dcam[i] = 0.0;
    }

  general_inverse (dof, tmp_A, num_para);

  row = 0;
  for (i = 0; i < 30; i++) 
    if (fixing[i] == 0 || priori_mode != 0) {
      col = 0;
      for (j = 0; j < 30; j++)
	if (fixing[j] == 0 || priori_mode != 0) {
	  post_cov[i][j] = tmp_A[row][col] * scale[i] * scale[j];
	  col ++;
	} else
	  post_cov[i][j] = 0.0;
      row ++;
    } else 
      for (j = 0; j < 30; j++) post_cov[i][j] = 0.0;

  free (tmp_dx);
  free (tmp_B);
  for (i = 0; i < dof; i++) free(tmp_A[i]);
  free (tmp_A);

  ABORT1:
  for (i = 0; i < 30; i++) {
    free(A[i]);
  }

  free(A);
}
  
static void apriori_weights_new (C, weights)
double weights[15][15], C[15];
{
  double sigma_d = 5.0, sigma_0 = 0.01;
  double sigma_1 = 0.1, sigma_2 = 0.01, tmp;
  int i, j;

  for (i = 0; i < 15; i++)
    for (j = 0; j < 15; j++)
      weights[i][j] = 0.0;

  tmp = sigma_d * sigma_d;

  weights[6][6] = weights[7][7] = weights[10][10] = weights[11][11]
    = 1.0 / tmp;

  weights[6][10] = weights[10][6] = weights[7][11] = weights[11][7]
    = -1.0 / tmp;

  weights[12][12] = 1.0 / (sigma_0 * sigma_0); 
  weights[13][13] = 1.0 / (sigma_1 * sigma_1);
  weights[14][14] = 1.0 / (sigma_2 * sigma_2);

}


/*****************************************************************
 *  C program:  Adjusting Camera Parameters to minimize the objective function
 *
 *                created by Yalin Xiong on PPT.IUS.CS.CMU.EDU
 *
 *             creation time:  Tue Oct 31 1995
 *****************************************************************/

/*#include <math.h>*/
/*#include <stdio.h>*/
/*#include <mathutil.h>*/
/*#include <self_calib.h>*/

/* Matrix Inverse in double precision */

#define PERMBUFSIZE 100         /* Mat bigger than this requires calling malloc. */

#define abs(x) ((x)>=0 ? (x) : -(x))

static double    invt_matrixd (a, n)
double      **a;
int  n;
{
  register  i, j, k;
  double    det, biga, hold;
  int      *l,
           *m;                /* Row and column permutation vectors */
  int       permbuf[2 * PERMBUFSIZE];
  int       mallocflag;

    /* Allocate permutation vectors for l and m, with the same origin as the matrix. */
  if (n <= PERMBUFSIZE) {
    l = permbuf;
    mallocflag = 0;
  } else {
    l = (int *) malloc (2 * n * sizeof (int));
    if (l == 0) {
      fprintf (stderr, "matinvert: can't get working storage.\n");
      return 0.0;
    }
    mallocflag = 1;
  }

  m = &(l[n]);

  det = 1.0;

  for (k = 0; k < n; k++) {

    l[k] = k;
    m[k] = k;
    biga = a[k][k];

        /* Find the biggest element in the submatrix */
    for (i = k; i < n; i++)
      for (j = k; j < n; j++)
	
	if (abs (a[i][j]) > abs (biga)) {

	  biga = a[i][j];
	  l[k] = i;
	  m[k] = j;
	}

    /* Interchange rows */

    i = l[k];

    if (i > k)
      for (j = 0; j < n; j++) {

	hold = -a[k][j];
	a[k][j] = a[i][j];
	a[i][j] = hold;
      }

    /* Interchange columns */
    
    j = m[k];
    
    if (j > k)
      for (i = 0; i < n; i++) {

	hold = -a[i][k];
	a[i][k] = a[i][j];
	a[i][j] = hold;
      }

    /* Divide column by minus pivot (value of pivot element is contained in biga). */
    if (biga == 0.0)
      return (0.0);
    for (i = 0; i < n; i++)
      if (i != k)
	a[i][k] /= -biga;

    /* Reduce matrix */
    for (i = 0; i < n; i++)

      if (i != k) {

	hold = a[i][k];
	for (j = 0; j < n; j++)
	  if (j != k)
	    a[i][j] += hold * a[k][j];
      }
    /* Divide row by pivot */
    for (j = 0; j < n; j++)
      if (j != k)
	a[k][j] /= biga;

    det *= biga;            /* Product of pivots */
    a[k][k] = 1.0 / biga;
  }                           /* K loop */

  /* Final row & column interchanges */
  for (k = n - 2; k >= 0; k--) {
    i = l[k];
    if (i > k)
      for (j = 0; j < n; j++) {
	hold = a[j][k];
	a[j][k] = -a[j][i];
	a[j][i] = hold;
      }
    j = m[k];
    if (j > k)
      for (i = 0; i < n; i++) {
	hold = a[k][i];
	a[k][i] = -a[j][i];
	a[j][i] = hold;
      }
  }

  if (mallocflag)
    free (l);
  return det;
}

/* A Feature's error and its Jacobian matrix 
   w.r.t. camera parameter C0, A0, H0, V0, O0, R0, C1, A1, H1, V1, O1, R1, k

   Feature location: fl (left feature) and fr (right feature)
*/

static void Jacobian (C0, A0, H0, V0, O0, R0, 
	       C1, A1, H1, V1, O1, R1, fl, fr,
	       fr_cov, k, disp, J)
double C0[3], A0[3], H0[3], V0[3], O0[3], R0[3]; /* Left Camera Parameters */
double C1[3], A1[3], H1[3], V1[3], O1[3], R1[3]; /* Right Camera Parameters */
double fl[2], fr[2], disp[2]; /* Feature positions and residual displacement */
double fr_cov[2][2], J[2][37]; /* Matching uncertainty, Jacobian matrix */
double *k;
{
  static double dr_A0[3][3], dr_H0[3][3], dr_V0[3][3], dr_O0[3][3], dr_R0[3][3];
  static double r[3];
  static double dC0[2][3], dr[2][3], dC1[2][3], dA1[2][3], dH1[2][3], 
        dV1[2][3], dO1[2][3], dR1[2][3];
  static double nearest[2];
  static double distance, dk[2];
  int i, j;

  img2ray(fl, A0, H0, V0, O0, R0, r, dr_A0, dr_H0, dr_V0, dr_O0,
	  dr_R0, FALSE);
  if (calc_error)
     return;

  distance = min_dist2 (k, fr, fr_cov, C0, r, C1, A1, H1, V1, O1, R1);

  ray2img (nearest, C0, *k, r, C1, A1, H1, V1, O1, R1, 
	   dC0, dk, dr, dC1, dA1, dH1, dV1, dO1, dR1, FALSE);

  /* df / dA0 = dr/ dA0 * df / dr 
     (Be careful about row vs. column in matrix dr/dA0) */

  for (i = 0; i < 2; i++) {

    for (j = 0; j < 3; j++) {

      J[i][j] = dC0[i][j];

      J[i][j+3] = dr_A0[0][j] * dr[i][0] + dr_A0[1][j] * dr[i][1] +
	dr_A0[2][j] * dr[i][2];

      J[i][j+6] = dr_H0[0][j] * dr[i][0] + dr_H0[1][j] * dr[i][1] +
	dr_H0[2][j] * dr[i][2];

      J[i][j+9] = dr_V0[0][j] * dr[i][0] + dr_V0[1][j] * dr[i][1] +
	dr_V0[2][j] * dr[i][2];

      J[i][j+12] = dr_O0[0][j] * dr[i][0] + dr_O0[1][j] * dr[i][1] +
	dr_O0[2][j] * dr[i][2];

      J[i][j+15] = dr_R0[0][j] * dr[i][0] + dr_R0[1][j] * dr[i][1] +
	dr_R0[2][j] * dr[i][2];

      J[i][j+18] = dC1[i][j];
      J[i][j+21] = dA1[i][j];
      J[i][j+24] = dH1[i][j];
      J[i][j+27] = dV1[i][j];
      J[i][j+30] = dO1[i][j];
      J[i][j+33] = dR1[i][j];
    }

    J[i][36] = dk[i];

    disp[i] = fr[i] - nearest[i]; /* Displacement Error */
  }
}

  
/*****************************************************************
 *  C program:  Solving Rank Deficient Linear Symmetric Equations
 *
 *                created by Yalin Xiong on PPT.IUS.CS.CMU.EDU
 *
 *             creation time:  Thu Nov  9 1995
 *             file     name:  /usr0/yx/jpl/self-calib/singular_mat.c
 *****************************************************************/

/*#include <math.h>*/
/*#include <stdio.h>*/

#define ZERO_THRESHOLD 1e-9

/* Solving equation Ax = B when A is a rank-deficient symmetric matrix */

static void singular_mat (n, A, B, x)
int n;
double **A, *B, *x;
{
  double *eigenvalues, **eigenvectors, *ptr, *tmp, maxeigen;
  int i, j;

  eigenvalues = (double *) malloc (n * sizeof (double));
  eigenvectors = (double **) malloc (n * sizeof (double *));
  for (i = 0; i < n; i++)
    eigenvectors[i] = (double *) malloc (n * sizeof (double));
  tmp = (double *) malloc (n * sizeof (double));

  for (i = 0; i < n; i++)
    for (j = 0; j < n; j++)
      eigenvectors[i][j] = A[i][j];

  eigen_sym (n, eigenvectors, eigenvalues);

  /* Threshold Eigenvalues when taking inverse */

  maxeigen = fabs(eigenvalues[0]);
/*
  printf("Eigenvalues = [");
  for (i = 0; i < n; i++)
    printf(" %e ", eigenvalues[i]);
  printf("]\n");
  printf("Eigenvectors:\n[");
  for (i = 0; i < n; i++) {
    for (j = 0; j < n; j++)
      printf(" %e ", eigenvectors[j * n + i]);
    printf("\n");
  }
  printf("]\n");
*/
  for (i = 0; i < n; i ++) 
    if (fabs(eigenvalues[i]) > maxeigen)
      maxeigen = fabs(eigenvalues[i]);

  for (i = 0; i < n; i++)
    if ((fabs(eigenvalues[i]) / maxeigen) < ZERO_THRESHOLD) {
/*      printf(" Zero Eigenvalue: %e\n", eigenvalues[i]); */
      eigenvalues[i] = 0.0;
    } else
      eigenvalues[i] = 1.0 / eigenvalues[i];

  for (i = 0; i < n; i++) {
    
    tmp[i] = 0.0;
    for (j = 0; j < n; j++) {
      tmp[i] += eigenvectors[j][i] * B[j];
    }
    
/*    printf("tmp[%d] = %e\n", i, tmp[i]); */

    tmp[i] *= eigenvalues[i];
  }

  for (i = 0; i < n; i++) {

    x[i] = 0.0;

    for (j = 0; j < n; j++) 
      x[i] += eigenvectors[i][j] * tmp[j];
  }

  free(tmp);
  for (i = 0; i < n; i++) free(eigenvectors[i]);
  free(eigenvalues);
  free(eigenvectors);
}

static void inv_eigenvalues(eigen_values, n, num_eigen)
double *eigen_values;
int n, *num_eigen;
{
  int *done, i, j, k, max_index;
  double max_now, max_eig;
  
  done = (int *) malloc (n * sizeof (int));
  for (i = 0; i < n; i++)
    done[i] = 0;

  if (*num_eigen > n) *num_eigen = n;

  for (i = 0; i < *num_eigen; i++) {

    max_now = 0.0;

    for (j = 0; j < n; j++)
      if ((done[j] == 0) && (fabs(eigen_values[j]) > max_now)) {
	max_now = fabs(eigen_values[j]);
	max_index = j;
      }

    done[max_index] = i + 1;
    if (i == 0) max_eig = max_now;
  }

  for (i = 0; i < n; i++)
    if (done[i] == 0)
      eigen_values[i] = 0.0;
    else {
/*      printf("Eigenvalue = %e\n", eigen_values[i]); */
      if (fabs(eigen_values[i] / max_eig) > ZERO_THRESHOLD)
	eigen_values[i] = 1.0 / eigen_values[i];
      else {
	eigen_values[i] = 0.0;
	(*num_eigen) --;
      }
    }

  free (done);
/*  printf("Valid Eigenvalues: %d\n", *num_eigen); */
}

/* Pre-defined number of eigenvalues */

static void singular_mat2 (n, A, B, x, num_eigen)
int n, num_eigen;
double **A, *B, *x;
{
  double *eigenvalues, **eigenvectors, *ptr, *tmp, maxeigen;
  int i, j;

  eigenvalues = (double *) malloc (n * sizeof (double));
  eigenvectors = (double **) malloc (n * sizeof (double *));
  for (i = 0; i < n; i++)
    eigenvectors[i] = (double *) malloc (n * sizeof (double));
  tmp = (double *) malloc (n * sizeof (double));

  for (i = 0; i < n; i++)
    for (j = 0; j < n; j++)
      eigenvectors[i][j] = A[i][j];

  eigen_sym (n, eigenvectors, eigenvalues);

  /* Threshold Eigenvalues when taking inverse */

  maxeigen = fabs(eigenvalues[0]);
/*
  printf("Eigenvalues = [");
  for (i = 0; i < n; i++)
    printf(" %e ", eigenvalues[i]);
  printf("]\n");
  printf("Eigenvectors:\n[");
  for (i = 0; i < n; i++) {
    for (j = 0; j < n; j++)
      printf(" %e ", eigenvectors[j * n + i]);
    printf("\n");
  }
  printf("]\n");
*/
  inv_eigenvalues(eigenvalues, n, &num_eigen);

  for (i = 0; i < n; i++) {
    
    tmp[i] = 0.0;
    for (j = 0; j < n; j++) {
      tmp[i] += eigenvectors[j][i] * B[j];
    }
    
/*    printf("tmp[%d] = %e\n", i, tmp[i]); */

    tmp[i] *= eigenvalues[i];
  }

  for (i = 0; i < n; i++) {

    x[i] = 0.0;

    for (j = 0; j < n; j++) 
      x[i] += eigenvectors[i][j] * tmp[j];
  }

  free(tmp);
  for (i = 0; i < n; i++) free(eigenvectors[i]);
  free(eigenvalues);
  free(eigenvectors);
}

/* general inverse of a matrix */

static void general_inverse (n, A, num_eigen)
int n, num_eigen;
double **A;
{
  double *eigenvalues, **eigenvectors, *ptr, *tmp, maxeigen;
  int i, j, k;

  eigenvalues = (double *) malloc (n * sizeof (double));
  eigenvectors = (double **) malloc (n * sizeof (double *));
  for (i = 0; i < n; i++)
    eigenvectors[i] = (double *) malloc (n * sizeof (double));

  for (i = 0; i < n; i++)
    for (j = 0; j < n; j++)
      eigenvectors[i][j] = A[i][j];

  eigen_sym (n, eigenvectors, eigenvalues);

  /* Threshold Eigenvalues when taking inverse */

  inv_eigenvalues(eigenvalues, n, &num_eigen);

  for (i = 0; i < n; i++) {

    for (j = 0; j < n; j++) {
      A[i][j] = 0.0;

      for (k = 0; k < n; k++)
	A[i][j] += eigenvectors[i][k] * eigenvalues[k] * eigenvectors[j][k];
    }
  }

  for (i = 0; i < n; i++) free(eigenvectors[i]);
  free(eigenvalues);
  free(eigenvectors);
}

/*
main(argc, argv)
int argc;
char *argv[];
{
  static double a[3][3], B[3], x[3];
  int i;

  a[0][0] = 4.873188213514398e+03;
  a[0][1] = -4.941800119276201e+02;
  a[0][2] = -3.823149890147400e+03;
  a[1][0] = -4.941800119276201e+02;
  a[1][1] = 1.215350122186010e+02;
  a[1][2] = 7.421779760736700e+02;
  a[2][0] = -3.823149890147400e+03;
  a[2][1] = 7.421779760736700e+02;
  a[2][2] = 4.758733711620500e+03;

  B[0] = 2.7793e+01;
  B[1] = 1.8477e+01;
  B[2] = 8.3888e+01;
  
  singular_mat (3, a, B, x);

  printf("Solution x = [%e %e %e]\n",
	 x[0], x[1], x[2]);
}
*/


/*****************************************************************
 *  C program:  Compute 3D ray direction for given pixel location in image
 *
 *                created by Yalin Xiong on PPT.IUS.CS.CMU.EDU
 *
 *             creation time:  Tue Oct 24 1995
 *             file     name:  /usr0/yx/jpl/self-calib/img2ray.c
 *****************************************************************/

/*include <math.h>*/
/*include <stdio.h>*/
/*include <mathutil.h>*/
/*include <self_calib.h>*/

/* Compute the 3D ray direction for a given pixel location */
/* Also compute the derivative w.r.t. the given camera parameters */

/* Converting a vector to a matrix used in Cross -product */

static double *vect2mat (tmp1, mat1)
double tmp1[3], mat1[3][3];
{
  double *tmp;
  tmp = &(mat1[0][0]);

  mat1[0][0] = 0.0;
  mat1[0][1] = - tmp1[2];
  mat1[0][2] = tmp1[1];
  mat1[1][0] = tmp1[2];
  mat1[1][1] = 0.0;
  mat1[1][2] = - tmp1[0];
  mat1[2][0] = - tmp1[1];
  mat1[2][1] = tmp1[0];
  mat1[2][2] = 0.0;

  return(tmp);
}

/* Compute outer product of a * b' = c */

static double *outer_prod (a, b, c)
double a[3], b[3], *c;
{
  int i, j;

  for (i = 0; i < 3; i ++)
    for (j = 0; j < 3; j++)

      c[i*3+j] = a[i] * b[j];

  return (c);
}

/* Radial Distortion Coefficient function */

static double afun (r, O, R, dr, dO, dR, fonly)
double r[3], O[3], R[3], dr[3], dO[3], dR[3];
bool_t fonly;
{
  double result, tao, rdr, rdO;

  rdr = vect_dot_3d (r, r);
  rdO = vect_dot_3d (r, O);

  tao = rdr / (rdO * rdO) - 1.0;

  result = R[0] + R[1] * tao + R[2] * tao * tao;

  if ((fonly != TRUE) && ((dr == NULL) || (dO == NULL) || (dR == NULL))) {

    fprintf(stderr, "Memory Error: NULL pointer: afun");

    /*exit(-1);*/
    return result;
  }

  if (fonly != TRUE) {

    /* Also need to compute derivative */

    static double tmp1[3], tmp2[3];
    double coeff;

    /* Compute derivatives w.r.t. r */

    coeff = 2.0 * R[1] + 4.0 * R[2] * (rdr / (rdO * rdO) - 1.0);

    vect_scale_3d (1.0 / (rdO * rdO) * coeff, r, tmp1);
    
    vect_scale_3d (- rdr / (rdO * rdO * rdO) * coeff, O, tmp2);

    vect_add_3d (tmp1, tmp2, dr);

    /* Compute derivatives w.r.t. O */

    vect_scale_3d (- rdr / (rdO * rdO * rdO) * coeff, r, dO);

    /* Compute derivatives w.r.t. R */

    dR[0] = 1.0;
    dR[1] = tao;
    dR[2] = tao * tao;
  }

  return (result);
}

/* Camera Model: C A H V O R
   Ray Direction: r
   Derivatives: dC dA dH dV dO dR
   Pixel Location: pixel_pos
*/

#define MAX_r_ITERATION 50  /* Maximal number of iterations */

static void img2ray (pixel_pos, A, H, V, O, R, r, dA, dH, dV, dO, dR, fonly)
double pixel_pos[2];
double A[3], H[3], V[3], O[3], R[3], r[3];
double dA[3][3], dH[3][3], dV[3][3], dO[3][3], dR[3][3];
bool_t fonly;
{
  double coeff; /* coeff to make r unit and pointing in the right direction. */
  static double rnor[3]; /* ray direction when no distortion. */
  static double tmp1[3], tmp2[3];
  double rnorm, rdiffnorm, adist, rdO;
  int i, j, iteration;

  for (i = 0; i < 3; i++) {

    tmp1[i] = V[i] - pixel_pos[1] * A[i];

    tmp2[i] = H[i] - pixel_pos[0] * A[i];

  }

  vect_cross_3d (tmp1, tmp2, rnor);

  for (i = 0; i < 3; i++) r[i] = rnor[i];

  iteration = 0;

  while (iteration < MAX_r_ITERATION) {

    rnorm = sqrt(vect_dot_3d (r, r));

    if (rnorm > 1e10) {
      calc_error = TRUE;
      return;
    }

    adist = afun (r, O, R, NULL, NULL, NULL, TRUE);

    rdO = vect_dot_3d (r, O);

    vect_scale_3d (- rdO, O, tmp1);

    vect_add_3d (tmp1, r, tmp2);

    vect_scale_3d (-adist, tmp2, tmp1);

    /* tmp2 is the new r */

    vect_add_3d (rnor, tmp1, tmp2);

    /* Improvement: */

    vect_sub_3d (tmp2, r, tmp1);

    vect_copy_3d (tmp2, r);

    /* Test whether the iteration has converged */

    if ((sqrt(vect_dot_3d (tmp1, tmp1)) / rnorm) < 1e-5) break;

    iteration ++; 
  }

  if (iteration == MAX_r_ITERATION) {
    /*...
    fprintf(stderr, "The iteration to compute ray direction failed to converge.\n");
    fprintf(stderr, "Probably caused by wrong camera parameters.\n");
    fprintf(stderr, "  A = %f %f %f\n", A[0], A[1], A[2]);
    fprintf(stderr, "  H = %f %f %f\n", H[0], H[1], H[2]);
    fprintf(stderr, "  V = %f %f %f\n", V[0], V[1], V[2]);
    fprintf(stderr, "  O = %f %f %f\n", O[0], O[1], O[2]);
    fprintf(stderr, "  R = %f %f %f\n", R[0], R[1], R[2]);
    fprintf(stderr, "Pixel Position: (%f %f)\n", pixel_pos[0], pixel_pos[1]);
    fprintf(stderr, "Current Ray direction: (%e %e %e).\n",
	    r[0], r[1], r[2]);
    exit(-1);
    ...*/
    calc_error = TRUE;
    return;
  }

  coeff = sqrt(vect_dot_3d (r, r));

  /* Test whether the direction of r should be flipped */

  if (vect_dot_3d (r, A) < 0.0) coeff *= -1.0;

  if (fonly != TRUE &&
      ((dA == NULL) || (dH == NULL) || (dV == NULL) ||
       (dO == NULL) || (dR == NULL))) {

    fprintf(stderr, "Meomry Error: NULL pointer\n");
    /*exit(-1);*/
    calc_error = TRUE;
    return;
  }

  /* compute partial derivatives */

  if (fonly != TRUE) {

    static double dr_a[3], dO_a[3], dR_a[3], r_roo[3];
    static double r0_coeff[3][3], right_coeff[3][3], tmp_mat[3][3];

    adist = afun (r, O, R, dr_a, dO_a, dR_a, FALSE);

    /* r_roo = r - (r . O) O */

    vect_sub_3d (r, vect_scale_3d (vect_dot_3d(r, O), O, tmp1), r_roo);

    for (i = 0 ; i < 3; i++)
      for (j = 0; j < 3; j++)
	r0_coeff[i][j] = 0.0;

    for (i = 0; i < 3; i++) r0_coeff[i][i] = 1.0 + adist;

    for (i = 0; i < 3; i++)
      for (j = 0; j < 3; j++)

	r0_coeff[i][j] += r_roo[i] * dr_a[j] - adist * O[i] * O[j];

    mat_invert_d3(r0_coeff, right_coeff);

    mat_copy_3x3d (right_coeff, r0_coeff);

    /* Compute matrix dA */

    for (i = 0; i < 3; i++)
      tmp1[i] =  pixel_pos[1] * (H[i] - pixel_pos[0] * A[i])
	- pixel_pos[0] * (V[i] - pixel_pos[1] * A[i]);

    mat_mul_3x3x3d (r0_coeff, (double (*)[3])vect2mat (tmp1, right_coeff), dA);

    /* Compute matrix dH */

    vect_sub_3d (V, vect_scale_3d(pixel_pos[1], A, tmp2), tmp1);

    mat_mul_3x3x3d (r0_coeff, (double (*)[3])vect2mat (tmp1, right_coeff), dH);

    /* Compute matric dV */

    vect_sub_3d (H, vect_scale_3d(pixel_pos[0], A, tmp2), tmp1);

    vect_scale_3d (-1.0, tmp1, tmp1);

    mat_mul_3x3x3d (r0_coeff, (double (*)[3])vect2mat (tmp1, right_coeff), dV);

    /* Compute matrix dO */

    outer_prod (O, r, right_coeff);

    mat_scale_3x3d (adist, right_coeff, right_coeff);

    mat_scale_3x3d (-1.0,
		   (double (*)[3])outer_prod (r_roo, dO_a, tmp_mat),
		   tmp_mat);

    mat_add_3x3d (right_coeff, tmp_mat, right_coeff);

    for (i = 0; i < 3; i++)
      right_coeff[i][i] += adist * rdO;

    mat_mul_3x3x3d (r0_coeff, right_coeff, dO);

    /* Compute matrix dR */

    outer_prod (r_roo, dR_a, right_coeff);

    mat_scale_3x3d (-1.0, right_coeff, right_coeff);

    mat_mul_3x3x3d (r0_coeff, right_coeff, dR);

    /* Compute Normalized derivatives */

    mat_scale_3x3d (- 1.0 / (coeff * coeff * coeff),
		   (double (*)[3])outer_prod (r, r, tmp_mat),
		   tmp_mat);

    for (i = 0; i < 3; i++) tmp_mat[i][i] += 1.0 / coeff;

    mat_copy_3x3d (mat_mul_3x3x3d (tmp_mat, dA, right_coeff),
		  dA);

    mat_copy_3x3d (mat_mul_3x3x3d (tmp_mat, dH, right_coeff),
		  dH);

    mat_copy_3x3d (mat_mul_3x3x3d (tmp_mat, dV, right_coeff),
		  dV);

    mat_copy_3x3d (mat_mul_3x3x3d (tmp_mat, dO, right_coeff),
		  dO);

    mat_copy_3x3d (mat_mul_3x3x3d (tmp_mat, dR, right_coeff),
		  dR);

  }

  vect_scale_3d (1.0 / coeff, r, r);
}


/*****************************************************************
 *  C program:  Compute the epipolar curve corresponding to a given 3D ray
 *
 *                created by Yalin Xiong on PPT.IUS.CS.CMU.EDU
 *
 *             creation time:  Wed Oct 25 1995
 *             file     name:  /usr0/yx/jpl/self-calib/ray2img.c
 *****************************************************************/

/*include <math.h>*/
/*include <stdio.h>*/
/*include <mathutil.h>*/
/*include <self_calib.h>*/

/* Compute the projection of an arbitrary 3D ray on the camera */
/* Also compute the derivative w.r.t. given camera parameter and the
   depth of the 3D point in the ray */

/* Radial Distance */

static double taofun (k, r, B, O, dk, dr, dB, dO, fonly)
double k, *dk;
double *r, *B, *O, *dr, *dB, *dO;
bool_t fonly;
{
  static double den_vec[3], B_kr[3], r_roo[3], tmp_vec1[3];
  double denorm, norm, tao;

  /* B_kr = B + k r */

  vect_add_3d (B, vect_scale_3d (k, r, tmp_vec1), B_kr);

  norm = vect_dot_3d (B_kr, O);

  vect_sub_3d (B_kr, vect_scale_3d (norm, O, tmp_vec1), den_vec);

  denorm = vect_dot_3d (den_vec, den_vec);

  tao = denorm / (norm * norm);

  if (fonly != TRUE && ((dk == NULL) || (dr == NULL) || (dB == NULL) ||
			(dO == NULL))) {

    fprintf(stderr, "Memory Error: NULL pointer: taofun\n");
    /*exit(-1);*/
    return tao;
  }

  /* Compute derivatives w.r.t. k, B, r, O, R */

  vect_sub_3d (r, vect_scale_3d (vect_dot_3d (r, O), O, tmp_vec1),
	      r_roo);

  *dk = 2.0 * vect_dot_3d (den_vec, r_roo) / (norm * norm)
    - 2.0 * denorm / (norm * norm * norm) * vect_dot_3d (r, O);

  if (fonly != TRUE) {

    vect_sub_3d (den_vec, 
		vect_scale_3d (vect_dot_3d (O, den_vec), O, tmp_vec1),
		dB);

    vect_scale_3d (2.0 / (norm * norm), dB, dB);

    vect_sub_3d (dB, vect_scale_3d (2.0 * tao / norm, O, tmp_vec1),
		dB);

    vect_scale_3d (k, dB, dr);

    /* Compute dO */

    vect_scale_3d (- 2.0 * vect_dot_3d (B_kr, O) / (norm * norm),
		  den_vec, dO);

    vect_sub_3d (dO, 
		vect_scale_3d (2.0 / (norm * norm) * 
			      vect_dot_3d (den_vec, O), B_kr, tmp_vec1),
		dO);

    vect_sub_3d (dO,
		vect_scale_3d (2.0 * tao / norm, B_kr, tmp_vec1),
		dO);
  }

  return(tao);
}

/* Radial Lens Distortion Coefficient */

static double ufun(k, r, B, O, R, dk, dr, dB, dO, dR, fonly)
double k, *dk;
double *r, *B, *O, *R, *dr, *dB, *dO, *dR;
bool_t fonly;
{
  double tao, dk_tao, result, coeff;
  static double dB_tao[3], dr_tao[3], dO_tao[3], tmp_vec1[3], tmp_vec2[3];

  tao = taofun (k, r, B, O, &dk_tao, dr_tao, dB_tao, dO_tao, fonly);

  result = R[0] + R[1] * tao + R[2] * tao * tao;

  if (fonly != TRUE && ((dk == NULL) || (dr == NULL) || (dB == NULL) ||
			(dO == NULL) || (dR == NULL))) {

    fprintf(stderr, "Memory Error: NULL pointer: ufun\n");
    /*exit(-1);*/
    return result;
  }

  coeff = R[1] + 2.0 * R[2] * tao;

  *dk = coeff * dk_tao;

  if (fonly != TRUE) {

    vect_scale_3d (coeff, dr_tao, dr);

    dB[0] = coeff * dB_tao[0];
    vect_scale_3d (coeff, dB_tao, dB); 

    vect_scale_3d (coeff, dO_tao, dO);

    dR[0] = 1.0; dR[1] = tao; dR[2] = tao * tao;
  }

  return(result);
}

/* Alpha, Beta and Gamma are the same functions */

static double alphafun (k, r, B, A, O, R, dk, dr, dB, dA, dO, dR, fonly)
double k, *dk;
double *r, *B, *A, *O, *R, *dr, *dB, *dA, *dO, *dR;
bool_t fonly;
{
  static double dr_u[3], dB_u[3], dO_u[3], dR_u[3], B_kr[3];
  static double tmp_vec1[3], tmp_vec2[3];
  double udist, dk_u, result;
  double B_kro;

  udist = ufun (k, r, B, O, R, &dk_u, dr_u, dB_u, dO_u, dR_u, fonly);

  vect_add_3d (B, vect_scale_3d (k, r, tmp_vec1), B_kr);

  B_kro = vect_dot_3d (B_kr, O);

  vect_sub_3d (B_kr, 
	      vect_scale_3d (B_kro, O, tmp_vec1),
	      tmp_vec2);

  vect_scale_3d (udist, tmp_vec2, tmp_vec2);

  vect_add_3d (B_kr, tmp_vec2, tmp_vec1);

  /* alpha = (p' - c) . A */

  result = vect_dot_3d (tmp_vec1, A);

  if (fonly != TRUE && ((dk == NULL) || (dr == NULL) || (dB == NULL) ||
			(dA == NULL) || (dO == NULL) || (dR == NULL))) {

    fprintf(stderr, "Memory Error: NULL pointer: alpha\n");
    /*exit(-1);*/
    return result;
  }

  /* compute derivatives */

  if (fonly != TRUE) {

    /* Compute dA */

    vect_copy_3d (tmp_vec1, dA);

    /* Compute dB */

    vect_scale_3d ((1.0 + udist), A, dB);
    vect_add_3d (dB, vect_scale_3d (vect_dot_3d (A, B_kr), dB_u, tmp_vec1),
		 dB);

    vect_sub_3d (dB, vect_scale_3d (B_kro * vect_dot_3d (A, O), dB_u, 
				    tmp_vec1),
		 dB);

    vect_sub_3d (dB, vect_scale_3d (udist * vect_dot_3d (A, O), O, tmp_vec1),
		 dB);

    /* Compute dr */

    vect_scale_3d (k, dB, dr);

    /* Compute dO */

    vect_scale_3d (- udist * B_kro, A, dO);

    vect_sub_3d (dO, vect_scale_3d (udist * vect_dot_3d (O, A), B_kr, 
				    tmp_vec1),
		dO);

    vect_sub_3d (B_kr, vect_scale_3d (B_kro, O, tmp_vec1), tmp_vec1);
    vect_scale_3d (vect_dot_3d (tmp_vec1, A), dO_u, tmp_vec2);
    vect_add_3d (tmp_vec2, dO, dO);

    /* Compute dR */

    vect_scale_3d (vect_dot_3d (B_kr, A) - B_kro * vect_dot_3d (O, A),
		  dR_u, dR);
  }

  /* Compute dk */

  vect_scale_3d (dk_u, 
		vect_sub_3d (B_kr, vect_scale_3d (B_kro, O, tmp_vec1),
			    tmp_vec1),
		tmp_vec2);

  vect_add_3d (r, tmp_vec2, tmp_vec2);

  vect_scale_3d (udist, 
		vect_sub_3d (r, 
			    vect_scale_3d (vect_dot_3d (r, O), O, tmp_vec1),
			    tmp_vec1),
		tmp_vec1);

  vect_add_3d (tmp_vec2, tmp_vec1, tmp_vec1);

  *dk = vect_dot_3d (tmp_vec1, A);

  return(result);
}

/* Compute the image position and their derivatives */

/* The 3D ray is C0 + k r in the world coordinate */
/* The camera parameters are C, A, H, V, O, R */
/* The derivatives are dC0, dk, dr, dC, dA, dH, dV, dO, dR */

static void ray2img (pixel_pos, C0, k, r, C, A, H, V, O, R, dC0, dk, dr, dC, dA, dH,
	      dV, dO, dR, fonly)
double pixel_pos[2], k, dk[2];
double *C0, *r, *C, *A, *H, *V, *O, *R;
double dC0[2][3], dr[2][3], dC[2][3], dA[2][3], dH[2][3], dV[2][3], dO[2][3], dR[2][3];
bool_t fonly;
{
  static double B[3];

  /* partial derivatives of alpha, beta, gamma w.r.t. B, O, R, A, H, V */

  static double dB_abg[3][3], dr_abg[3][3], dO_abg[3][3], dR_abg[3][3], 
        dA_a[3], dH_b[3], dV_g[3];
  static double dk_abg[3], alpha, beta, gamma;
  double coeff1, coeff2, coeff3;

  /* B = C0 - C */

  vect_sub_3d (C0, C, B);

  alpha = alphafun (k, r, B, A, O, R, &(dk_abg[0]), &(dr_abg[0][0]), 
		    &(dB_abg[0][0]),
		    dA_a, &(dO_abg[0][0]), &(dR_abg[0][0]), fonly);

  beta = alphafun (k, r, B, H, O, R, &(dk_abg[1]), &(dr_abg[1][0]), 
		   &(dB_abg[1][0]),
		    dH_b, &(dO_abg[1][0]), &(dR_abg[1][0]), fonly);

  gamma = alphafun (k, r, B, V, O, R, &(dk_abg[2]), &(dr_abg[2][0]), 
		    &(dB_abg[2][0]),
		    dV_g, &(dO_abg[2][0]), &(dR_abg[2][0]), fonly);

  pixel_pos[0] = beta / alpha;
  pixel_pos[1] = gamma / alpha;

  if (fonly != TRUE && ((dC0 == NULL) || (dk == NULL) || (dr == NULL) ||
			(dC  == NULL) || (dA == NULL) || (dH == NULL) ||
			(dV  == NULL) || (dO == NULL) || (dR == NULL))) {

    fprintf(stderr, "Memory Error: NULL pointer: ray2img\n");
    /*exit(-1);*/
    return;
  }

  /* Compute partial derivatives */

  /* Compute dk */

  coeff1 = 1.0 / alpha;
  coeff2 = beta / (alpha * alpha);
  coeff3 = gamma / (alpha * alpha);
 
  dk[0] = coeff1 * dk_abg[1] - coeff2 * dk_abg[0];
  dk[1] = coeff1 * dk_abg[2] - coeff3 * dk_abg[0];

  if (fonly != TRUE) {

    static double tmp_vec1[3], tmp_vec2[3];

    /* Compute dC0 */
    vect_scale_3d (coeff1, &(dB_abg[1][0]), tmp_vec1);
    vect_scale_3d (coeff2, &(dB_abg[0][0]), tmp_vec2);

    vect_sub_3d (tmp_vec1, tmp_vec2, &(dC0[0][0]));

    vect_scale_3d (coeff1, &(dB_abg[2][0]), tmp_vec1);
    vect_scale_3d (coeff3, &(dB_abg[0][0]), tmp_vec2);

    vect_sub_3d (tmp_vec1, tmp_vec2, &(dC0[1][0]));

    /* Compute dC */

    vect_scale_3d (-1.0, &(dC0[0][0]), &(dC[0][0]));
    vect_scale_3d (-1.0, &(dC0[1][0]), &(dC[1][0]));

    /* Compute dr */

    vect_scale_3d (coeff1, &(dr_abg[1][0]), tmp_vec1);
    vect_scale_3d (coeff2, &(dr_abg[0][0]), tmp_vec2);

    vect_sub_3d (tmp_vec1, tmp_vec2, &(dr[0][0]));

    vect_scale_3d (coeff1, &(dr_abg[2][0]), tmp_vec1);
    vect_scale_3d (coeff3, &(dr_abg[0][0]), tmp_vec2);

    vect_sub_3d (tmp_vec1, tmp_vec2, &(dr[1][0]));

    /* Compute dA */

    vect_scale_3d (- coeff2, dA_a, &(dA[0][0]));
    vect_scale_3d (- coeff3, dA_a, &(dA[1][0]));

    /* Compute dH */

    vect_scale_3d (coeff1, dH_b, &(dH[0][0]));
    dH[1][0] = dH[1][1] = dH[1][2] = 0.0;

    /* Compute dV */

    dV[0][0] = dV[0][1] = dV[0][2] = 0.0;
    vect_scale_3d (coeff1, dV_g, &(dV[1][0]));

    /* Compute dO */

    vect_scale_3d (coeff1, &(dO_abg[1][0]), tmp_vec1);
    vect_scale_3d (coeff2, &(dO_abg[0][0]), tmp_vec2);

    vect_sub_3d (tmp_vec1, tmp_vec2, &(dO[0][0]));

    vect_scale_3d (coeff1, &(dO_abg[2][0]), tmp_vec1);
    vect_scale_3d (coeff3, &(dO_abg[0][0]), tmp_vec2);

    vect_sub_3d (tmp_vec1, tmp_vec2, &(dO[1][0]));

    /* Compute dR */

    vect_scale_3d (coeff1, &(dR_abg[1][0]), tmp_vec1);
    vect_scale_3d (coeff2, &(dR_abg[0][0]), tmp_vec2);

    vect_sub_3d (tmp_vec1, tmp_vec2, &(dR[0][0]));

    vect_scale_3d (coeff1, &(dR_abg[2][0]), tmp_vec1);
    vect_scale_3d (coeff3, &(dR_abg[0][0]), tmp_vec2);

    vect_sub_3d (tmp_vec1, tmp_vec2, &(dR[1][0]));
  }
}


/*****************************************************************
 *  C program:  Compute Eigenvalue and Eigenvector for Real Symmetric Matrix
 *
 *                created by Yalin Xiong on PPT.IUS.CS.CMU.EDU
 *
 *             creation time:  Thu Nov  9 1995
 *             file     name:  /usr0/yx/jpl/self-calib/eigen_sym.c
 *****************************************************************/

/*include <math.h>*/
/*include <stdio.h>*/

static void tred2(a,n,d,e)
double **a,d[],e[];
int n;
{
  int l,k,j,i;
  double scale,hh,h,g,f;

  for (i = (n-1); i >= 1; i--) {
    l=i-1;
    h=scale=0.0;
    
    if (l > 0) {
      for (k = 0; k <= l; k++)
	scale += fabs(a[i][k]);
      if (scale == 0.0)
	e[i]=a[i][l];
      else {
	for (k = 0; k <= l; k++) {
	  a[i][k] /= scale;
	  h += a[i][k]*a[i][k];
	}
	f=a[i][l];
	g = f>0 ? -sqrt(h) : sqrt(h);
	e[i]=scale*g;
	h -= f*g;
	a[i][l]=f-g;
	f=0.0;
	for (j = 0; j <= l; j++) {
	  /* Next statement can be omitted if eigenvectors not wanted */
	  a[j][i]=a[i][j]/h;
	  g=0.0;
	  for (k = 0; k <= j; k++)
	    g += a[j][k]*a[i][k];
	  for (k=j+1;k<=l;k++)
	    g += a[k][j]*a[i][k];
	  e[j]=g/h;
	  f += e[j]*a[i][j];
	}
	hh=f/(h+h);
	for (j = 0; j <= l; j++) {
	  f=a[i][j];
	  e[j]=g=e[j]-hh*f;
	  for (k = 0; k <= j; k++)
	    a[j][k] -= (f*e[k]+g*a[i][k]);
	}
      }
    } else
      e[i]=a[i][l];
    d[i]=h;
  }
  /* Next statement can be omitted if eigenvectors not wanted */
  d[0]=0.0;
  e[0]=0.0;
  /* Contents of this loop can be omitted if eigenvectors not
     wanted except for statement d[i]=a[i][i]; */
  for (i = 0;i < n; i++) {
    l=i-1;
    if (d[i]) {
      for (j = 0; j <= l; j++) {
	g=0.0;
	for (k = 0; k <= l; k++)
	  g += a[i][k]*a[k][j];
	for (k = 0; k <= l; k++)
	  a[k][j] -= g*a[k][i];
      }
    }
    d[i]=a[i][i];
    a[i][i]=1.0;
    for (j=0;j <= l; j++) a[j][i]=a[i][j]=0.0;
  }
}

#define SIGN(a,b) ((b)<0 ? -fabs(a) : fabs(a))

static void tqli(d,e,n,z)
double d[],e[],**z;
int n;
{
  int m,l,iter,i,k;
  double s,r,p,g,f,dd,c,b;

  for (i = 1; i < n; i++) e[i-1]=e[i];
  /*...e[n]=0.0;...*/
  e[n-1]=0.0;
  for (l = 0; l < n; l++) {
    iter=0;
    do {
      for (m=l;m < n-1; m++) {
	dd=fabs(d[m])+fabs(d[m+1]);
	if (fabs(e[m])+dd == dd) break;
      }
      if (m != l) {
	if (iter++ == 30) {
	  fprintf(stderr, "Too many iterations in TQLI");
	  /*exit(-1);*/
	  return;
	}
	g=(d[l+1]-d[l])/(2.0*e[l]);
	r=sqrt((g*g)+1.0);
	g=d[m]-d[l]+e[l]/(g+SIGN(r,g));
	s=c=1.0;
	p=0.0;
	for (i=m-1;i>=l;i--) {
	  f=s*e[i];
	  b=c*e[i];
	  if (fabs(f) >= fabs(g)) {
	    c=g/f;
	    r=sqrt((c*c)+1.0);
	    e[i+1]=f*r;
	    c *= (s=1.0/r);
	  } else {
	    s=f/g;
	    r=sqrt((s*s)+1.0);
	    e[i+1]=g*r;
	    s *= (c=1.0/r);
	  }
	  g=d[i+1]-p;
	  r=(d[i]-g)*s+2.0*c*b;
	  p=s*r;
	  d[i+1]=g+p;
	  g=c*r-b;
	  /* Next loop can be omitted if eigenvectors not wanted */
	  for (k = 0;k < n; k++) {
	    f=z[k][i+1];
	    z[k][i+1]=s*z[k][i]+c*f;
	    z[k][i]=c*z[k][i]-s*f;
	  }
	}
	d[l]=d[l]-p;
	e[l]=g;
	e[m]=0.0;
      }
    } while (m != l);
  }
}

static void eigen_sym (n, A, ev)
int n;
double **A, *ev;
{
  double *e;
  int i, j;

  e = (double *) malloc (n * sizeof (double));

  tred2 (A, n, ev, e);

  tqli (ev, e, n, A);

  free(e);
}


/*****************************************************************
 *  C program:  Scale camera parameters to the same scale
 *
 *                created by Yalin Xiong on PPT.IUS.CS.CMU.EDU
 *
 *             creation time:  Mon Dec 11 1995
 *             file     name:  /usr0/yx/jpl/self-calib/para_scale.c
 *****************************************************************/

/*include <math.h>*/
/*include <stdio.h>*/
/*include <mathutil.h>*/
/*include <self_calib.h>*/

/* Test Image size */
/*...
 define ROWS 480
 define COLS 512
...*/

/* Parameter Scale for new explicit camera parameters */

static void para_scale(cam, scale, typical_depth, rows, cols)
double cam[30], scale[30], typical_depth;
int rows, cols;
{
  static double cl_old[18], dl_old[18][15], cr_old[18], dr_old[18][15];
  static double p2d[5][2]; /* Four test image points */
  static double ray[3], k;
  static double dC0[2][3], dk[2], dC[2][3], dA[2][3];
  static double dH[2][3], dV[2][3], dO[2][3];
  static double dR[2][3], dr[2][3], offset[2];
  int i, j, m, l;

  /* Test image points */
  
  p2d[0][0] = (double) cols / 4.0; p2d[0][1] = (double) rows / 4.0;
  p2d[1][0] = (double) cols / 4.0; p2d[1][1] = (double) rows * 0.75;
  p2d[2][0] = (double) cols * 0.75; p2d[2][1] = (double) rows / 4.0;
  p2d[3][0] = (double) cols * 0.75; p2d[3][1] = (double) rows * 0.75;
  p2d[4][0] = (double) cols * 0.50; p2d[4][1] = (double) rows * 0.50;

  /* converting camera parameters */
  new2old2 (cl_old, cam, dl_old, FALSE);
  new2old2 (cr_old, &(cam[15]), dr_old, FALSE);

  for (i = 0; i < 30; i++) scale[i] = 0.0;

  /* for left camera */
  for (i = 0; i < 5; i++) {

/*    printf("Test Point: %d\n", i); */

    img2ray (&(p2d[i][0]), &(cl_old[3]), &(cl_old[6]), &(cl_old[9]),
	     &(cl_old[12]), &(cl_old[15]), ray, NULL, NULL, 
	     NULL, NULL, NULL, TRUE);
    if (calc_error)
      return;

    k = (typical_depth - cl_old[1]) / ray[1];

    ray2img (&(p2d[i][0]), cl_old, k, ray, cl_old, &(cl_old[3]), &(cl_old[6]),
	     &(cl_old[9]), &(cl_old[12]), &(cl_old[15]), dC0, dk, dr, 
	     dC, dA, dH, dV, dO, dR, FALSE);

    for (j = 0; j < 15; j++) {

      offset[0] = offset[1] = 0.0;
      for (m = 0; m < 2; m++) {
	
	/* dC */
	for (l = 0; l < 3; l ++)
	  offset[m] += dC[m][l] * dl_old[l][j];
	/* dA */
	for (l = 0; l < 3; l++)
	  offset[m] += dA[m][l] * dl_old[l+3][j];
	/* dH */
	for (l = 0; l < 3; l++)
	  offset[m] += dH[m][l] * dl_old[l+6][j];
	/* dV */
	for (l = 0; l < 3; l++)
	  offset[m] += dV[m][l] * dl_old[l+9][j];
	/* dO */
	for (l = 0; l < 3; l++)
	  offset[m] += dO[m][l] * dl_old[l+12][j];
	/* dR */
	for (l = 0; l < 3; l++)
	  offset[m] += dR[m][l] * dl_old[l+15][j];
      }

      scale[j] += sqrt(offset[0] * offset[0] + offset[1] * offset[1]);
    }
  }

  /* for right camera */
  for (i = 0; i < 5; i++) {

    img2ray (&(p2d[i][0]), &(cr_old[3]), &(cr_old[6]), &(cr_old[9]),
	     &(cr_old[12]), &(cr_old[15]), ray, NULL, NULL, 
	     NULL, NULL, NULL, TRUE);
    if (calc_error)
      return;

    k = (typical_depth - cr_old[1]) / ray[1];

    ray2img (&(p2d[i][0]), cr_old, k, ray, cr_old, &(cr_old[3]), &(cr_old[6]),
	     &(cr_old[9]), &(cr_old[12]), &(cr_old[15]), dC0, dk, dr, 
	     dC, dA, dH, dV, dO, dR, FALSE);

    for (j = 0; j < 15; j++) {

      offset[0] = offset[1] = 0.0;
      for (m = 0; m < 2; m++) {
	
	/* dC */
	for (l = 0; l < 3; l ++)
	  offset[m] += dC[m][l] * dr_old[l][j];
	/* dA */
	for (l = 0; l < 3; l++)
	  offset[m] += dA[m][l] * dr_old[l+3][j];
	/* dH */
	for (l = 0; l < 3; l++)
	  offset[m] += dH[m][l] * dr_old[l+6][j];
	/* dV */
	for (l = 0; l < 3; l++)
	  offset[m] += dV[m][l] * dr_old[l+9][j];
	/* dO */
	for (l = 0; l < 3; l++)
	  offset[m] += dO[m][l] * dr_old[l+12][j];
	/* dR */
	for (l = 0; l < 3; l++)
	  offset[m] += dR[m][l] * dr_old[l+15][j];
      }

      scale[15+j] += sqrt(offset[0] * offset[0] + offset[1] * offset[1]);
    }
  }

  for (i = 0; i < 30; i++)
    scale[i] = 5.0 / scale[i];
/*
  printf("Left Camera Scale Factors:\n");
  for (i = 0; i < 15; i++)
    printf(" %e ", scale[i]);
  printf("\nRightCamera Scale Factors:\n");
  for (i = 0; i < 15; i++)
    printf(" %e ", scale[i+15]);
  printf("\n");
*/
}


/*****************************************************************
 *  C program:  Outlier Rejection Functions
 *
 *                created by Yalin Xiong on ppt.ius.cs.cmu.edu
 *
 *             creation time:  Fri Mar 15 1996
 *             file     name:  /IUS/usrp1/yx/reject_outlier.c
 *****************************************************************/

/*include <math.h>*/
/*include <stdio.h>*/

static void sort(n,ra)
int n;
float ra[];
{
        int l,j,ir,i;
        float rra;

	if (n < 2) return;
        l=(n >> 1)+1;
        ir=n;
        for (;;) {
                if (l > 1)
                        rra=ra[--l];
                else {
                        rra=ra[ir];
                        ra[ir]=ra[1];
                        if (--ir == 1) {
                                ra[1]=rra;
                                return;
                        }
                }
                i=l;
                j=l << 1;
                while (j <= ir) {
                        if (j < ir && ra[j] < ra[j+1]) ++j;
                        if (rra < ra[j]) {
                                ra[i]=ra[j];
                                j += (i=j);
                        }
                        else j=ir+1;
                }
                ra[i]=rra;
        }
}

#define X84_THRESHOLD 10.0
#define MIN_NUMBER 5 /* minimal number of points to reject outliers */

/* Outlier rejection using X84 rule:
   
   From "Robust Statistics: the Approach Based on Influence Functions"
   by Hampel et al. pp. 56-71

   return value: -1 error
                 other integer: number of points rejected
*/

static int X84 (data, dim, length, validity)
float *data;
int dim, length;
unsigned char *validity;
{
  float median, MAD, *c_data, *residual;
  int i, j, mid, cardinality, reject_num;

  if (length < MIN_NUMBER) return (0);

  mid = length / 2;

  if ((residual = (float *) malloc (length * sizeof (float))) == NULL) {
    fprintf(stderr, "Failed to allocate memory in X84\n");
    return (-1);
  }

  c_data = data;
  reject_num = 0;
  for (i = 0; i < dim; i++) {

    cardinality = 0;
    for (j = 0; j < length; j++)
      if (validity[j])  
	residual[cardinality++] = c_data[j*dim];

    sort (cardinality, residual-1);
    median = residual[cardinality >> 1];

    cardinality = 0;
    for (j = 0; j < length; j++)
      if (validity[j])
	residual[cardinality++] = fabs(c_data[j*dim] - median);

    sort (cardinality, residual-1);
    MAD = residual[cardinality >> 1];

/*    printf("Median = %.3f, MAD = %.3f\n", median, MAD); */

    for (j = 0; j < length; j++) 
      if (validity[j] == 1 && fabs(c_data[j*dim] - median) > (X84_THRESHOLD *
							      MAD)) {
	  validity[j] = 0;
	  reject_num ++;
	}

    c_data ++;
  }
/*
  for (i = 0; i < length; i++)
    if (validity[i] == 0)
      printf("Rejected Error: (%.3f %.3f)\n", data[2*i], data[2*i+1]);
*/
  free (residual);
  return (reject_num);
}


/*****************************************************************
 *  C program:  Compute the minimal distance between a feature 
 *              and its epipolar curve
 *
 *                created by Yalin Xiong on PPT.IUS.CS.CMU.EDU
 *
 *             creation time:  Thu Oct 26 1995
 *             file     name:  /usr0/yx/jpl/self-calib/min_dist.c
 *****************************************************************/

/*include <math.h>*/
/*include <stdio.h>*/
/*include <mathutil.h>*/
/*include <self_calib.h>*/

/* Maximal and minimal distance of the 3D point along the ray */

#define MAX_DISTANCE 10000.0
#define MIN_DISTANCE 0.1

/* Given fixed camera parameters, find a point in the
   epipolar curve that is the closest to the feature. */

/* The ray is C0 + k r
   The feature point in the image is PIXEL_POS
   and the current camera parameters are C, A, H, V, O, R */

static double C0[3], r[3], C[3], A[3], H[3], V[3], O[3], R[3];
static double PIXEL_POS[2], POS_INV_COV[2][2];

/* Standard function interface for MRQCOF
   given k, compute the distance and its derivative w.r.t. k */

#undef  MAX_ITERATION
#define MAX_ITERATION 50

static void compute_dist (k, n, alpha, beta, chisq)
double *k, *beta, **alpha, *chisq;
int n;
{
  static double pos[2], dk[2];

  if (n != 1) {

    fprintf(stderr, "Wrong Arguments: distance\n");
    /*exit(-1);*/
    return;
  }

  if (*k > MAX_DISTANCE) *k = MAX_DISTANCE;
  if (*k < MIN_DISTANCE) *k = MIN_DISTANCE;

  ray2img (pos, C0, *k, r, C, A, H, V, O, R, NULL, dk, NULL, NULL, NULL, NULL,
	   NULL, NULL, NULL, TRUE);

  *chisq = (pos[0] - PIXEL_POS[0]) * (pos[0] - PIXEL_POS[0]) +
    (pos[1] - PIXEL_POS[1]) * (pos[1] - PIXEL_POS[1]);

  beta[0] = - (pos[0] - PIXEL_POS[0]) * dk[0] -
    (pos[1] - PIXEL_POS[1]) * dk[1];

  alpha[0][0] = dk[0] * dk[0] + dk[1] * dk[1];
}

/* When the uncertainty of feature position is considered. */

static void compute_dist2 (k, n, alpha, beta, chisq)
double *k, *beta, **alpha, *chisq;
int n;
{
  static double pos[2], dk[2], tmp[2], disp[2];

  if (n != 1) {

    fprintf(stderr, "Wrong Arguments: distance\n");
    /*exit(-1);*/
    return;
  }

  if (*k > MAX_DISTANCE) *k = MAX_DISTANCE;
  if (*k < MIN_DISTANCE) *k = MIN_DISTANCE;

  ray2img (pos, C0, *k, r, C, A, H, V, O, R, NULL, dk, NULL, NULL, NULL, NULL,
	   NULL, NULL, NULL, TRUE);

  disp[0] = PIXEL_POS[0] - pos[0];
  disp[1] = PIXEL_POS[1] - pos[1];

  tmp[0] = POS_INV_COV[0][0] * disp[0] + POS_INV_COV[0][1] * disp[1];
  tmp[1] = POS_INV_COV[1][0] * disp[0] + POS_INV_COV[1][1] * disp[1];

  *chisq = tmp[0] * disp[0] + tmp[1] * disp[1];

  beta[0] = dk[0]  * tmp[0] + dk[1] * tmp[1];

  tmp[0] = POS_INV_COV[0][0] * dk[0] + POS_INV_COV[0][1] * dk[1];
  tmp[1] = POS_INV_COV[1][0] * dk[0] + POS_INV_COV[1][1] * dk[1];

  alpha[0][0] = dk[0] * tmp[0] + dk[1] * tmp[1];
}

static double min_dist (init_k, pixel, local_C0, local_r, local_C, local_A, 
		local_H, local_V, local_O, local_R)
double *init_k, pixel[2];
double local_C0[3], local_r[3], local_C[3], local_A[3];
double local_H[3], local_V[3], local_O[3], local_R[3];
{
  double alamda, **alpha, **covar, distance, prev_dist;
  int i, count;

  /* Initilizing Camera parameters */

  for (i = 0; i < 2; i++) PIXEL_POS[i] = pixel[i];
  vect_copy_3d (local_C0, C0);
  vect_copy_3d (local_r, r);
  vect_copy_3d (local_C, C);
  vect_copy_3d (local_A, A);
  vect_copy_3d (local_H, H);
  vect_copy_3d (local_V, V);
  vect_copy_3d (local_O, O);
  vect_copy_3d (local_R, R);

  alpha = (double **) malloc (1 * sizeof (double *));
  covar = (double **) malloc (1 * sizeof (double *));
  for (i = 0; i < 1; i++){
    alpha[i] = (double *) malloc (1 * sizeof (double));
    covar[i] = (double *) malloc (1 * sizeof (double));
  }

  count = 0;

  alamda = -1.0;

  mrqmin (init_k, 1, covar, alpha, &distance, &alamda, compute_dist);

  while (count < MAX_ITERATION) {

    count ++;
    mrqmin (init_k, 1, covar, alpha, &distance, &alamda, compute_dist);

    if (count > 1 && distance < prev_dist &&
	(fabs(distance - prev_dist) / prev_dist < 1e-4))
        break;

    prev_dist = distance;

  }

  if (count == MAX_ITERATION) 
    fprintf(stderr, "Possible divergence in MIN_DIST\n");

  alamda = 0.0;

  mrqmin (init_k, 1, covar, alpha, &distance, &alamda, compute_dist);

  for (i = 0; i < 1; i++) {
    free(alpha[i]);
    free(covar[i]);
  }

  free(alpha);
  free(covar);

  return(distance);
}

/* When the uncertainty of feature location is considered. */

static double min_dist2 (init_k, pixel, pixel_cov, local_C0, local_r, local_C, local_A, 
		local_H, local_V, local_O, local_R)
double *init_k, pixel[2], pixel_cov[2][2];
double local_C0[3], local_r[3], local_C[3], local_A[3];
double local_H[3], local_V[3], local_O[3], local_R[3];
{
  double alamda, **alpha, **covar, distance, prev_dist;
  int i, count;

  /* Initilizing Camera parameters */

  for (i = 0; i < 2; i++) PIXEL_POS[i] = pixel[i];
  vect_copy_3d (local_C0, C0);
  vect_copy_3d (local_r, r);
  vect_copy_3d (local_C, C);
  vect_copy_3d (local_A, A);
  vect_copy_3d (local_H, H);
  vect_copy_3d (local_V, V);
  vect_copy_3d (local_O, O);
  vect_copy_3d (local_R, R);
  mat_invert_d2(pixel_cov, POS_INV_COV);

  alpha = (double **) malloc (1 * sizeof (double *));
  covar = (double **) malloc (1 * sizeof (double *));
  for (i = 0; i < 1; i++){
    alpha[i] = (double *) malloc (1 * sizeof (double));
    covar[i] = (double *) malloc (1 * sizeof (double));
  }

  count = 0;

  alamda = -1.0;

  mrqmin (init_k, 1, covar, alpha, &distance, &alamda, compute_dist2);

  while (count < MAX_ITERATION) {

    count ++;

    mrqmin (init_k, 1, covar, alpha, &distance, &alamda, compute_dist2);
    if (count > 1 && distance < prev_dist &&
	(fabs(distance - prev_dist) / prev_dist < 1e-4))
        break;

    prev_dist = distance;

  }
/*
  if (count == MAX_ITERATION) 
    fprintf(stderr, "Possible divergence in MIN_DIST\n");
*/
  alamda = 0.0;

  mrqmin (init_k, 1, covar, alpha, &distance, &alamda, compute_dist2);

  for (i = 0; i < 1; i++) {
    free(alpha[i]);
    free(covar[i]);
  }

  free(alpha);
  free(covar);

  return(distance);
}


/*include <stdio.h>*/
/*include <math.h>*/

/* Levenberg-Marduardt Method For Nonlinear Minimization.

   By Yalin Xiong

   Sept 23, 1994 */

/* Solving Linear Equation Ax = b using pivoting */

#define SWAP(a,b) {double temp=(a);(a)=(b);(b)=temp;}

static void gaussj(a,n,b)
double **a,*b;
int n;
{
  int *indxc,*indxr,*ipiv;
  int i,icol,irow,j,k,l,ll;
  double big,dum,pivinv;

  indxc = (int *) malloc (n * sizeof (int));
  indxr = (int *) malloc (n * sizeof (int));
  ipiv = (int *) malloc (n * sizeof (int));

  for (j = 0; j < n; j++) ipiv[j] = 0;

  for (i = 0; i < n; i++) {

    big=0.0;

    for (j = 0; j < n; j++)

      if (ipiv[j] != 1)
        for (k = 0; k < n; k++) {

          if (ipiv[k] == 0) {

            if (fabs(a[j][k]) >= big) {
              big=fabs(a[j][k]);
              irow=j;
              icol=k;
            }
          } else if (ipiv[k] > 1) {

            fprintf(stderr, "GAUSSJ: Singular Matrix-1");
            /*exit(1);*/
	    return;
          }
        }

    ++(ipiv[icol]);

    if (irow != icol) {
      for (l = 0; l < n;l++) SWAP(a[irow][l],a[icol][l])
      SWAP(b[irow],b[icol])
    }

    indxr[i]=irow;
    indxc[i]=icol;

    if (a[icol][icol] == 0.0) {
      fprintf(stderr, "GAUSSJ: Singular Matrix-2");
      fprintf(stderr, "MATRIX: \n\t%e \t%e \n\t%e \t%e\n",
		a[0][0], a[0][1], a[1][0], a[1][1]);
      /*exit(-1);*/
      return;
    }

    pivinv=1.0/a[icol][icol];

    a[icol][icol]=1.0;

    for (l = 0; l < n; l++) a[icol][l] *= pivinv;
    b[icol] *= pivinv;
    for (ll = 0; ll < n; ll++)
      if (ll != icol) {
        dum=a[ll][icol];
        a[ll][icol]=0.0;
        for (l = 0; l < n;l++) a[ll][l] -= a[icol][l]*dum;
        b[ll] -= b[icol]*dum;
      }
  }

  for (l = n-1; l >= 0; l--) {
    if (indxr[l] != indxc[l])
      for (k = 0; k < n; k++)
        SWAP(a[k][indxr[l]],a[k][indxc[l]]);
  }

  free(ipiv);
  free(indxr);
  free(indxc);
}

#undef SWAP

static double *da,*atry,*oneda,*beta,ochisq;

static void mrqmin(a,ma,covar,alpha,chisq,alamda, mrqcof)
double a[],**covar,**alpha,*chisq,*alamda;
int ma;
void (*mrqcof)();
{
  int k,kk,j,ihit;

  if (*alamda < 0.0) {

    oneda = (double *) malloc (ma * sizeof (double));
    atry = (double *) malloc (ma * sizeof (double));
    da = (double *) malloc (ma * sizeof (double));
    beta = (double *) malloc (ma * sizeof (double));

    *alamda=0.001;

    (*mrqcof) (a,ma,alpha,beta,chisq);
    ochisq=(*chisq);

  }

  for (j = 0; j < ma; j++) {

    for (k = 0; k < ma; k++) covar[j][k] = alpha[j][k];

    covar[j][j] = alpha[j][j] * (1.0+(*alamda));

    oneda[j] = beta[j];
  }
  gaussj (covar, ma, oneda);

  for (j = 0; j < ma; j++)

    da[j] = oneda[j];

  if (*alamda == 0.0) {

    free (beta);
    free (da);
    free (atry);
    free (oneda);

    return;
  }

  for (j = 0; j < ma; j++) atry[j] = a[j];

  for (j = 0; j < ma; j++)

    atry[j] = a[j] + da[j];

  (*mrqcof) (atry,ma,covar,da,chisq);

  if (*chisq < ochisq) {

    *alamda *= 0.1;

    ochisq=(*chisq);

    for (j = 0; j < ma; j++) {

      for (k = 0; k < ma; k++) alpha[j][k]=covar[j][k];

      beta[j]=da[j];

      a[j]=atry[j];
    }
  } else {

    *alamda *= 10.0;
    *chisq=ochisq;
  }

  return;
}




/******************************************************************************
********************************   INV22   ************************************
*******************************************************************************

    This function inverts any invertible 2x2 matrix. */

#undef  EPSILON
#define EPSILON 1e-15

static double (*inv22(a, b))[2]
double a[2][2];		/* input matrix */
double b[2][2];		/* output matrix */
{
    double det;

    /* Check for NULL inputs */
    if ((a == NULL) || (b == NULL))
	return NULL;

    /* Check for non-distinct output */
    if (a == b)
	return NULL;

    /* Compute the determinant */
    det = a[0][0] * a[1][1] - a[1][0] * a[0][1];
    if ((det < EPSILON) && (det > -EPSILON))
	return NULL;

    b[0][0] =  a[1][1] / det;
    b[0][1] = -a[0][1] / det;
    b[1][0] = -a[1][0] / det;
    b[1][1] =  a[0][0] / det;

    /* Return a pointer to the result */
    return b;
    }
