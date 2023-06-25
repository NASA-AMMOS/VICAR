/************************************************************************/
/* Gruen correlation algorithm.						*/
/************************************************************************/
/* The differences between gruen() and gruen2() are minor.  gruen() is
 * maintained for backwards compatibility; apps should use gruen2().
 * gruen() takes 3-element arrays for line_coef and samp_coef, while
 * gruen2() takes 4-element arrays.  Also, gruen2() adds the ftol parameter.
 *
 * gruen3() uses SimpleImage to pass around the images rather than arrays.
 * This cleans up the interface slightly.  Same functionality as gruen2
 * but because gruen2 constructs SimpleImage's and then calls gruen3,
 * gruen3 should be marginally faster.  Note though that gruen3 also supports
 * the use of coef_limits in the amoeba modes as well as the annealing modes
 * (via a flag parameter).
 *
 * Note that the extra parameter (an xy term which allows for perspective
 * transforms, not just affines) is NOT supported in the annealing modes
 * at this time.
 */
/*
   ARGUMENTS:

left = left image area template,[max_left_area][max_left_area]		double

nlw = # lines left area. odd number					int

nsw = # samples left area. odd number					int

max_left_area = inner physical dimension of "left" array		int

right = right image search area, [max_right_area][max_right_area]	double

nlw2 = # lines right area. odd number					int

nsw2 = # samples right area. odd number					int

max_right_area = inner physical dimension of "right" and "correl" arrays int

correl = correlation matrix, [max_right_area][max_right_area]		double
         Returned. Only used for linear correlation.

left_img = left image area template, as a SimpleImage.  For gruen3   SimpleImage
	   only; replaces left, nlw, nsw, and max_left_area.
right_img = right image area template, as a SimpleImage.  For gruen3 SimpleImage
	    only; replaces right, nlw, nsw, and max_left_area.
correl_img = Image to hold returned correlation matrix.  For gruen3 &SimpleImage
	     only; replaces correl.  Note that for gruen3 it can be
	     arbitrarily sized; it must be at least [nlw2-nlw+1][nsw2-nsw+1].
	     If a reference to null is passed in, an appropriate image
	     is allocated, only if needed.

line_offset = line shift to correct initial registration, returned	double
              Negative means shift upwards.

samp_offset = sample shift to correct initial registration, returned	double
              Negative means shift to the left.

line_coef = line equation coefficients,[4], returned			double
            Mapping polynomial from template to search area.
            rightline=line_coef[0]*leftsamp+line_coef[1]*leftline+line_coef[2]
			+ line_coef[3]*leftsamp*leftline
            On entry contains best estimate of line polynomial solution.
            Try: 0., 1., 0., 0.
            For gruen(), this is a 3-element array.

samp_coef = sample equation coefficients,[4], returned			double
            Mapping polynomial from template to search area.
            rightsamp=samp_coef[0]*leftsamp+samp_coef[1]*leftline+samp_coef[2]
			+ samp_coef[3]*leftsamp*leftline
            On entry contains best estimate of sample polynomial solution.
            Try: 1., 0., 0., 0.
            For gruen(), this is a 3-element array.

line_coef_limits = initial boundary conditions, [3][2]			double
                   Only used in modes 1 and 4.
                   [][0] is negative limit
                   [][1] is positive limit
                   These limit the possible range of mapping polynomial values.

samp_coef_limits = initial boundary conditions, [3][2]			double
                   Only used in modes 1 and 4.
                   [][0] is negative limit
                   [][1] is positive limit
                   These limit the possible range of mapping polynomial values.

line_temp = line equation coefficients temperature,[3]			double
            Only used in modes 1 and 4.
            The initial guessing range for solutions of polynomial values.
            Try: (line_coef_limits[][1]-line_coef_limits[][0])/12.

samp_temp = sample equation coefficients temperature,[3]		double
            Only used in modes 1 and 4.
            The initial guessing range for solutions of polynomial values.
            Try: (samp_coef_limits[][1]-samp_coef_limits[][0])/12.

percent = Percentage of the template to use in correlations. If percent=100
          then one correlation is computed with the entire area, otherwise
          five correlations are computed, beginning with the entire area. 
          The other four correlations utilize only:
          "percent" lines at the bottom,
          "percent" lines at the top,
          "percent" columns at the right,
          "percent" columns at the left.				double
          The purpose of this is to permit the exclusion of border points
          which for some reason are incompatible in intensity with the
          data being correlated. 
          For example if percent=80 then one of the correlation attempts
          will only use nlw*0.80 lines either at the top or bottom of the
          window.

limits = The number of iterations permitted in the annealing scheme.	int
         This should be several thousand at the least.

quality = correlation quality. Zero to one (if inv_flag is true), or -1 to 1
          (if inv_flag is false).  1=perfect.			      returned

mode = is the type of correlation desired. An integer from 0 to 8.	int
       (all modes are sub-pixel).
	If mode is negative, linear (mode 0) is performed before the
	otherwise specified -mode.  This avoids getting too many extra modes
	to support linear followed by all the amoebas.
        mode=0 linear least squares fit. 
             Fastest, on integral pixel borders. Translation only.
             Accuracy limited to 1/10 pixel.
        mode=1 annealing fit. Very slow. Able to search entire area.
             Accuracy adjustable but around 1/30 pixel.
             Handles rotation,skew,flip,scale,offset.
        mode=2 amoeba fit. deterministic simplex search method.
             Accuracy as good as it is possible to achieve(1/100 pixel).
             Initial estimate must be within 2 pixels or else the
             resulting tiepoint will be in error.
             Speed intermediate between modes 0 and 1.
             Handles rotation,skew,flip,scale,offset.
        mode=3 linear (mode=0) followed by amoeba (mode=2) fit
             Linear fit locates minimum and amoeba fit refines it.
             Handles rotation,skew,flip,scale,offset.
        mode=4 annealing (mode=1) followed by amoeba (mode=2) fit.
             Annealing locates minimum and amoeba refines it.
             Handles rotation,skew,flip,scale,offset.
        mode=5 amoeba fit with 2 degrees of freedom.  Like mode=2 except
             only two degrees of freedom (x/y translation) are allowed.
             line_coef[0] and [1], and samp_coef[0] and [1] are returned
             unchanged.
        mode=6 linear (mode=0) followed by amoeba w/2 degrees of freedom
             (mode=5) fit.  Similar to mode=3 except the amoeba step is
             limited to 2 degrees of freedom (x/y translation).  The other
             parameters are fixed by mode=0:  line_coef[0]=0, line_coef[1]=1,
             samp_coef[0]=1, samp_coef[1]=0.
        mode=7 amoeba fit with 8 degrees of freedom.  Like mode 2 but adds
             two additional degrees of freedom that handle perspective or
             trapezoidal (non-affine) transforms.  Only available in gruen2().
             The equations are thus:
             y' = ax + by + c + gxy
             x' = dx + ey + f + hxy
             where (a,b,c,g) maps to line_coef and (d,e,f,h) maps to samp_coef.
        mode=8 amoeba fit with 4 degrees of freedom.  This is a good model
             for in-situ cameras looking out at a plane.  Allows translation
             only in y, while translation, trapezoid, and shear are allowed
             in x.  So in the mode=7 equation, a==0, b==1, g==0, d==1, while
             c, e, f, h are allowed to vary.
        mode=9 amoeba fit with 5 degrees of freedom.  This is a better model
             for in-situ cameras looking out at a plane with epipolar-aligned
             cameras.  Allows translation only in y, while translation,
             trapezoid, scale, and shear are allowed in x.  So in the mode=7
             equation, a==0, b==1, g==0, while c, d, e, f, h are allowed to
             vary.
        Note: The annealing/amoeba-2 DOF mode (mode=1 followed by mode=5)
             is not implemented at this time.  Annealing will generate 6
             degrees of freedom, which are then ignored by amoeba-2 DOF.
             This can easily be implemented if necessary, however.
        Note: Combinations of mode=7 and mode=8 with linear (analogous to
             modes 3 and 6) are not implemented at this time.  They could
             easily be added if needed, however.

ftol = tolerance value for amoeba.  This tells amoeba when to stop.  When
       the difference in correlation quality obtained by varying the
       parameters becomes less than this value, it is assumed that the
       minimum as been found.  This value relates directly (but not simply)
       to the accuracy of the obtained result, and also has a tremendous
       impact on execution speed.  Smaller values mean more precision in
       the results, at the cost of increased run time.  Historically, this
       value has been .000001 (the value still used by gruen()) but for
       most cases, a value around .001 or slightly higher will be sufficient.
       input double.

inv_flag = flag indicating whether to accept inverse correlations or not.
           Setting this to true (non-0), which is the default for gruen(),
           and the traditional value) means that inverse correlations are
	   allowed as well as normal correlations, and the quality value
	   will always be non-negative.  Qualities < 0 get mapped to the
	   negative of the quality (thus -.98 is the same as +.98).  This
	   allows e.g. different spectral bands to be correlated, where
	   the features are the same but intensities get reversed.
	   Setting it to false (0) means that inverse correlations are NOT
	   allowed, and the returned quality value will range from -1 to +1.

Function return = return status, 0=OK, 1=unable to obtain a correlation. int
*/

/************************************************************************/

#include "gruen.h"
#include "amoeba.h"
#include "GruenTransform.h"

#include <math.h>
#include <stdlib.h>
#include <stdio.h>

/* Defines for accessing arrays */

#define correl(row,col) (params->correl_p[(row)*params->max_right_area + (col)])

/* Parameter structure for "cost" function. */

// Note: coef_limits indices are REVERSED w.r.t the caller.  [minmax][param]
struct CostParams {
	GruenTransform *xform;
	TransformType xform_type;
	double coef_limits[2][8];	// Note: REVERSED
	SimpleImage<double> *left_img;
	SimpleImage<double> *right_img;
	SimpleImage<double> *correl_img;	/* used only by search_area */
	double sumx[10];
	double left_sum[10];
	double percent;
	int inv_flag;
	int ind;		/* returned */
};

/* Function prototypes for this module */

static double cost(double p[], int ndim, void *func_args);
static void search_area(struct CostParams *params, double answer[2],
	double *quality, int inv_flag);
static void metropolis(struct CostParams *params, double answer[6],
	double range[6], int numten, int limits, int norm, double *quality);

/************************************************************************/
/* The gruen code itself */
/************************************************************************/

int gruen(double *left_p, int nlw, int nsw, int max_left_area,
	   double *right_p, int nlw2, int nsw2, int max_right_area,
	   double *correl_p,
	   double *line_offset, double *samp_offset,
	   double line_coef[3], double samp_coef[3],
	   double line_coef_limits[3][2], double samp_coef_limits[3][2],
	   double line_temp[3], double samp_temp[3],
	   double percent, int limits,
	   double *quality,
	   int mode)
{
    double line_coef4[4], samp_coef4[4];
    int i, status;
 
    if ((mode < -6) || (mode > 6)) {  /* modes 7,8,9 not supported in gruen() */
        return 1;
    }

    /* Transfer to 4-element arrays... */

    for (i=0; i<3; i++) {
	line_coef4[i] = line_coef[i];
	samp_coef4[i] = samp_coef[i];
    }
    line_coef4[3] = 0.0;
    samp_coef4[3] = 0.0;

    status = gruen2(left_p, nlw, nsw, max_left_area, right_p, nlw2, nsw2,
		max_right_area, correl_p, line_offset, samp_offset,
		line_coef4, samp_coef4, line_coef_limits, samp_coef_limits,
		line_temp, samp_temp, percent, limits,
		quality, mode, 0.00001, 1);

    /* and back again... */
    for (i=0; i<3; i++) {
	line_coef[i] = line_coef4[i];
	samp_coef[i] = samp_coef4[i];
    }
    return status;

}

int gruen2(double *left_p, int nlw, int nsw, int max_left_area,
	   double *right_p, int nlw2, int nsw2, int max_right_area,
	   double *correl_p,
	   double *line_offset, double *samp_offset,
	   double line_coef[4], double samp_coef[4],
	   double line_coef_limits[3][2], double samp_coef_limits[3][2],
	   double line_temp[3], double samp_temp[3],
	   double percent, int limits,
	   double *quality,
	   int mode, double ftol,
	   int inv_flag)
{
    SimpleImage<double> left_img(left_p, nlw, nsw, max_left_area);
    SimpleImage<double> right_img(right_p, nlw2, nsw2, max_right_area);
    SimpleImage<double> *correl_img = NULL;
    if (correl_p != NULL)
	correl_img = new SimpleImage<double>(correl_p,
		max_right_area, max_right_area, max_right_area);

    return gruen3(&left_img, &right_img,
		correl_img, line_offset, samp_offset,
		line_coef, samp_coef, line_coef_limits, samp_coef_limits,
		line_temp, samp_temp,
		percent, limits, quality, mode, ftol, inv_flag, 0);
    if (correl_img != NULL)
	delete correl_img;
}


int gruen3(SimpleImage<double> *left_img,
	   SimpleImage<double> *right_img,
	   SimpleImage<double> *&correl_img,
	   double *line_offset, double *samp_offset,
	   double line_coef[4], double samp_coef[4],
	   double line_coef_limits[3][2], double samp_coef_limits[3][2],
	   double line_temp[3], double samp_temp[3],
	   double percent, int limits,
	   double *quality,
	   int mode, double ftol,
	   int inv_flag, int use_limits)
{

    double range[8],answer[8];
    double P[9][8],Y[9];
    double sumx2,left_dn,rn=0;
    int numten,norm,i,j,ITER,kl=0,ks=0,num_areas,k,kll=0,kss=0;
    int num_DOF;
    double min;
    int ilo;

    struct CostParams params_struct;
    struct CostParams *params = &params_struct;

    params->left_img = left_img;
    params->right_img = right_img;
    params->correl_img = correl_img;
    params->percent = percent;
    params->inv_flag = inv_flag;

    params->ind = 0;

    int do_linear_first = 0;
    if (mode < 0) {
	mode = -mode;
	do_linear_first = 1;
    }
    if ((mode < 0) || (mode > 14)) {		/* bad mode */
        return 1;
    }

    params->xform = NULL;
    if (mode == 2 || mode == 3 || mode == 4)
	params->xform = GruenTransform::create(Amoeba6);
    if (mode == 5 || mode == 6)
	params->xform = GruenTransform::create(Amoeba2);
    if (mode == 7)
	params->xform = GruenTransform::create(Amoeba8);
    if (mode == 8)
	params->xform = GruenTransform::create(Amoeba4);
    if (mode == 9)
	params->xform = GruenTransform::create(Amoeba5);
    if (mode == 10)
	params->xform = GruenTransform::create(Amoeba2_bicubic);
    if (mode == 11)
	params->xform = GruenTransform::create(Amoeba4_bicubic);
    if (mode == 12)
	params->xform = GruenTransform::create(Amoeba5_bicubic);
    if (mode == 13)
	params->xform = GruenTransform::create(Amoeba6_bicubic);
    if (mode == 14)
	params->xform = GruenTransform::create(Amoeba8_bicubic);
    if (params->xform == NULL)
	params->xform_type = AmoebaNone;
    else
	params->xform_type = params->xform->getType();

    /* compute sub area limits if percent < 100 */
    num_areas=1;
    if (params->percent < 99.99) {
	num_areas=5;
	kl=(int)((left_img->getNL()*(params->percent/100.)) + .5);
	if (kl > left_img->getNL()) kl=left_img->getNL();
        if (kl < 1) kl=1;
        ks=(int)((left_img->getNS()*(params->percent/100.)) + .5);
        if (ks > left_img->getNL()) ks=left_img->getNL();
        if (ks < 1) ks=1;
        kll=left_img->getNL()-kl-1;
        kss=left_img->getNS()-ks-1;
    }

    /* loop on all sub areas */
    for (k=0; k < num_areas; k++) {

        /* compute area of sub area */
	if (k == 0) rn=left_img->getNL()*left_img->getNS();
	if (k == 1) rn=kl*left_img->getNS();
	if (k == 2) rn=kl*left_img->getNS();
	if (k == 3) rn=ks*left_img->getNL();
	if (k == 4) rn=ks*left_img->getNL();

        /* pre-compute left (template) sums for COST function */
	sumx2=0.0;
	params->sumx[k]=0.0;
	for (j=0; j < left_img->getNL(); j++) {
	    for (i=0; i < left_img->getNS(); i++) {
                if (k == 1) {
                    if (j <= kll) continue;
                }
                if (k == 2) {
                    if (j >= kl) continue;
                }
                if (k == 3) {
                    if (i <= kss) continue;
                }
                if (k == 4) {
                    if (i >= ks) continue;
                }
                left_dn=params->left_img->get(j,i);
                params->sumx[k] += left_dn;
                sumx2 += left_dn*left_dn;
             }
	}
	params->left_sum[k]=sumx2-params->sumx[k]*params->sumx[k]/rn;
    }

    if ((mode == 0) || (mode == 3) || (mode == 6) || do_linear_first) {

        /* Use conventional linear correlation scheme only */
	/* Result is in answer[0] (samp) and answer[1] (line) */

        search_area(params, answer, quality, inv_flag);

	// Put result back in line/samp_coef so it will be used below

	samp_coef[2] = answer[0];
	line_coef[2] = answer[1];
    }

    if ((mode == 1) || (mode == 4)) {

        /* use Annealing correlation */

        answer[0]=line_coef[0];
        answer[1]=line_coef[1];
        answer[2]=line_coef[2];
        answer[3]=samp_coef[0];
        answer[4]=samp_coef[1];
        answer[5]=samp_coef[2];
        range[0]=line_temp[0];
        range[1]=line_temp[1];
        range[2]=line_temp[2];
        range[3]=samp_temp[0];
        range[4]=samp_temp[1];
        range[5]=samp_temp[2];
        params->coef_limits[0][0]=line_coef_limits[0][0];
        params->coef_limits[1][0]=line_coef_limits[0][1];
        params->coef_limits[0][1]=line_coef_limits[1][0];
        params->coef_limits[1][1]=line_coef_limits[1][1];
        params->coef_limits[0][2]=line_coef_limits[2][0];
        params->coef_limits[1][2]=line_coef_limits[2][1];
        params->coef_limits[0][3]=samp_coef_limits[0][0];
        params->coef_limits[1][3]=samp_coef_limits[0][1];
        params->coef_limits[0][4]=samp_coef_limits[1][0];
        params->coef_limits[1][4]=samp_coef_limits[1][1];
        params->coef_limits[0][5]=samp_coef_limits[2][0];
        params->coef_limits[1][5]=samp_coef_limits[2][1];

        /* set iteration control */
        numten=limits/2;
        norm=numten/7;

        metropolis(params, answer, range,numten,limits,norm,quality);

	// Put results back in line/samp_coef

        line_coef[0] = answer[0];
        line_coef[1] = answer[1];
        line_coef[2] = answer[2];
        samp_coef[0] = answer[3];
        samp_coef[1] = answer[4];
        samp_coef[2] = answer[5];
    }

    if (params->xform != NULL) {	// any amoeba

	num_DOF = params->xform->getNumCoefs();

	// Stuff answer with parameters

	params->xform->setCoefs(answer,
				samp_coef[0], samp_coef[1], samp_coef[2],
				line_coef[0], line_coef[1], line_coef[2],
				samp_coef[3], line_coef[3]);

	/* Amoeba correlation */
	/* amoeba is used rather than amoeba2 because the "length	*/
	/* scales" (lambda) are different for each parameter.		*/

	if (use_limits) {
	    params->xform->setCoefs(params->coef_limits[0],
		samp_coef_limits[0][0], samp_coef_limits[1][0], -5000.0,
		line_coef_limits[0][0], line_coef_limits[1][0], -5000.0,
		samp_coef_limits[2][0], line_coef_limits[2][0]);
	    params->xform->setCoefs(params->coef_limits[1],
		samp_coef_limits[0][1], samp_coef_limits[1][1], 5000.0,
		line_coef_limits[0][1], line_coef_limits[1][1], 5000.0,
		samp_coef_limits[2][1], line_coef_limits[2][1]);
	} else {
	    params->xform->setCoefs(params->coef_limits[0],
				-1000., -1000., -1000., -1000.,
				-1000., -1000., -1000., -1000.);
	    params->xform->setCoefs(params->coef_limits[1],
				1000., 1000., 1000., 1000.,
				1000., 1000., 1000., 1000.);
	}

        /* precompute the matrix P of DOF+1 simplex starting points */
        /* and array Y of corresponding errors */

	// Note that there is one more precomputed solution than DOF's.
	// This extra solution is the original (i.e. unperturbed).
	// In amoeba2 the unperturbed is p[0] but here, for consistency
	// with the original code, the unperturbed is p[num_DOF].

	double lambda_array[9];
	params->xform->setCoefs(lambda_array,
			.07, .07, .3,
			.07, .07, .3,
			.01, .01);
	lambda_array[num_DOF] = 0.0;		// unperturbed solution

        for (j=0; j < num_DOF+1; j++) {
            for (i=0; i < num_DOF; i++) {
                P[j][i]=answer[i];
            }
	    // Perturb the diagonals (one per solution)
	    if (j < num_DOF)
	        P[j][j] = P[j][j] - lambda_array[j];

	    Y[j] = cost(P[j], num_DOF, params);
        }

        /* solve for the minimum using the simplex method */

        amoeba((double *)P, Y, num_DOF, 8, ftol, 5000, &ITER, cost, params);

	/* We used to pick the first answer arbitrarily, since all are	*/
	/* within FTOL.  However, with larger FTOL values the spread	*/
	/* gets larger.  So, we might as well pick the best of all the	*/
	/* possible answers.  It's cheap, and marginally improves the	*/
	/* results.							*/

	min = Y[0];
	ilo=0;
	for (i=1; i<num_DOF+1; i++) {
	    if (Y[i] < min) {
	        ilo = i;
		min = Y[i];
	    }
	}

        *quality = 1.0 - (Y[ilo] - 1.0);

	params->xform->getCoefs(P[ilo],
		&samp_coef[0], &samp_coef[1], &samp_coef[2],
		&line_coef[0], &line_coef[1], &line_coef[2],
		&samp_coef[3], &line_coef[3]);
    }

    /* return offset from initial estimate, Note other polynomial terms are
       zero because multiply by x=y=0 at template origin */

    *line_offset=line_coef[2];
    *samp_offset=samp_coef[2];
 
    correl_img = params->correl_img;	// in case it was created

    return params->ind;
}


/************************************************************************/
/* Objective function to be minimized.					*/
/* Polynomial is of the form:						*/
/* rightline = answer[0]*leftsamp + answer[1]*leftline + answer[2]	*/
/*           + answer[6]*leftsamp*leftline				*/
/* rightsamp = answer[3]*leftsamp + answer[4]*leftline + answer[5]	*/
/*           + answer[7]*leftsamp*leftline				*/
/*									*/
/* Note:  1.0 is added to the result before returning, to avoid some	*/
/* instabilities in the amoeba algorithm when the values are very close	*/
/* to 0.  Subtract 1.0 from the result to get a correlation quality.	*/
/* Also, the result is inverted so lower is better (since amoeba	*/
/* minimizes).  So, quality = 1.0 - (result - 1.0);			*/
/************************************************************************/

static double cost(double ans[], int ndim, void *func_args)
{
    struct CostParams *params = (struct CostParams *)func_args;

    int j,i,m,n,kl=0,ks=0,num_areas,k,kll=0,kss=0;
    double left_center_line,left_center_samp;
    double right_center_line,right_center_samp;
    double pre_x,pre_y,x,y,right_line,right_samp,wl,wr,wt,r2;
    double top,bot,rnlw2,rnsw2,best_r2;
    double sumy,sumy2,sumxy,rn=0,right_dn,denom,numer;
    double bad_return;
    double lll, lls, ull, uls;

    double dec, dec2, dec3, f1_, f0, f1, f2, b1_, b0, b1, b2;

    GruenTransform *xform = params->xform;
    TransformType type = params->xform_type;

    // Local variable copies only for consistency

    SimpleImage<double> *left_img = params->left_img;
    SimpleImage<double> *right_img = params->right_img;
    int nlw = left_img->getNL();
    int nsw = left_img->getNS();
    int nlw2 = right_img->getNL();
    int nsw2 = right_img->getNS();

    bad_return = (1.0 - 0.0) + 1.0; /* worst return value possible */

    /* check if solution limits are violated */
    for (i=0; i < ndim; i++) {
        if ((ans[i] < params->coef_limits[0][i]) || 
	   (ans[i] > params->coef_limits[1][i])) {
            params->ind=1;
            return bad_return;
        }
    }

    /* constants */
    left_center_line=(nlw-1)/2.0;
    left_center_samp=(nsw-1)/2.0;
    right_center_line=(nlw2-1)/2.0;
    right_center_samp=(nsw2-1)/2.0;

    /* check if corners of left area fall within right area */
    /* Account for interpolation margins */
    if (type == Amoeba2_bicubic || type == Amoeba4_bicubic ||
        type == Amoeba5_bicubic || type == Amoeba6_bicubic ||
        type == Amoeba8_bicubic) {
       lll = 1;
       lls = 1;
       ull = nlw2-2;
       uls = nsw2-2;
    }
    else {
       lll = 0;
       lls = 0;
       ull = nlw2-1;
       uls = nsw2-1;
   }

    y= -left_center_line;
    x= -left_center_samp;
    GruenTransformComputeXandY(type, ans, right_samp, right_line, x, y);
    right_samp += right_center_samp;
    right_line += right_center_line;
    if ((right_line < lll) || (right_line > ull) ||
       (right_samp < lls) || (right_samp > uls)) {
	params->ind=1;
        return bad_return;
    }
    // y= -left_center_line;		// already set
    x= (nsw-1)-left_center_samp;
    GruenTransformComputeXandY(type, ans, right_samp, right_line, x, y);
    right_samp += right_center_samp;
    right_line += right_center_line;
    if ((right_line < lll) || (right_line > ull) ||
       (right_samp < lls) || (right_samp > uls)) {
        params->ind=1;
        return bad_return;
    }
    y= (nlw-1)-left_center_line;
    x= -left_center_samp;
    GruenTransformComputeXandY(type, ans, right_samp, right_line, x, y);
    right_samp += right_center_samp;
    right_line += right_center_line;
    if ((right_line < lll) || (right_line > ull) ||
       (right_samp < lls) || (right_samp > uls)) {
	params->ind=1;
	return bad_return;
    }
    // y=(nlw-1)-left_center_line;	// already set
    x=(nsw-1)-left_center_samp;
    GruenTransformComputeXandY(type, ans, right_samp, right_line, x, y);
    right_samp += right_center_samp;
    right_line += right_center_line;
    if ((right_line < lll) || (right_line > ull) ||
       (right_samp < lls) || (right_samp > uls)) {
        params->ind=1;
        return bad_return;
    }

    /* compute sub area limits if percent < 100 */
    num_areas=1;
    if (params->percent < 99.99) {
        num_areas=5;
        kl=(int)((nlw*(params->percent/100.)) + .5);
        if (kl > nlw) kl=nlw;
        if (kl < 1) kl=1;
        ks=(int)((nsw*(params->percent/100.)) + .5);
        if (ks > nlw) ks=nlw;
        if (ks < 1) ks=1;
        kll=nlw-kl-1;
        kss=nsw-ks-1;
    }

    /* loop on all sub areas */
    best_r2=-1.0;

    for (k=0; k < num_areas; k++) {

        rn=nlw * nsw;

	/* compute coefficient of determination r^2 */
        sumy=0.0;
        sumy2=0.0;
        sumxy=0.0;

	int jstart = 0;
	int jstop = nlw;
	int istart = 0;
	int istop = nsw;
	if (k > 0) {
	    /* compute area and limits of sub area */
	    if (k == 1) {
		rn=kl * nsw;
		jstart = kll;
	    }
	    else if (k == 2) {
		rn=kl * nsw;
		jstop = kl;
	    }
	    else if (k == 3) {
		rn=ks * nlw;
		istart = kss;
	    }
	    else if (k == 4) {
		rn=ks * nlw;
		istop = ks;
	    }
	}


// This innermost of inner loops is implemented as a macro for efficiency.
// Each if statement makes a noticeable difference (several percent) in
// overall program run time!

#define GRUEN_INNER_LOOP(TYPE)					\
        for (j=jstart; j < jstop; j++) {			\
            y=j-left_center_line;				\
            /* store redundant terms */				\
	    GruenTransformPreComputeConstY##TYPE(ans, pre_x, pre_y, y); \
	    pre_x += right_center_samp;				\
	    pre_y += right_center_line;				\
            for (i=istart; i < istop; i++) {			\
                x=i-left_center_samp;				\
		GruenTransformComputeFromConstY##TYPE(ans, right_samp, right_line, pre_x, pre_y, x, y); \
								\
		/* Bilinear interpolation */			\
                m=(int)right_samp;				\
                n=(int)right_line;				\
                wl=right_samp-m;				\
                wr=1.0-wl;					\
                top=wl*right_img->get(n,m+1) +		\
		    wr*right_img->get(n,m);		\
		bot=wl*right_img->get(n+1,m+1) +	\
		    wr*right_img->get(n+1,m);		\
                wt=right_line-n;				\
                right_dn=bot*wt+top*(1.0-wt);			\
                                                                \
                sumy += right_dn; /* compute sums for least squares fit */ \
                sumy2 += right_dn*right_dn;			\
                sumxy += right_dn*left_img->get(j,i);	\
            }							\
        }




// This innermost of inner loops is implemented as a macro for efficiency.
// Each if statement makes a noticeable difference (several percent) in
// overall program run time!

#define GRUEN_INNER_LOOP_BICUBIC(TYPE)					\
        for (j=jstart; j < jstop; j++) {			\
            y=j-left_center_line;				\
            /* store redundant terms */				\
	    GruenTransformPreComputeConstY##TYPE(ans, pre_x, pre_y, y); \
	    pre_x += right_center_samp;				\
	    pre_y += right_center_line;				\
            for (i=istart; i < istop; i++) {			\
                x=i-left_center_samp;				\
		GruenTransformComputeFromConstY##TYPE(ans, right_samp, right_line, pre_x, pre_y, x, y); \
								\
	        /* Bicubic interpolation */			                               \
                m=(int)right_samp;                                                             \
                n=(int)right_line;                                                             \
                dec = right_samp - m;                                                          \
                dec2 = dec*dec;                                                                \
                dec3 = dec2*dec;                                                               \
                                                                                               \
                /* Computing coefficients in one direction */                                  \
                                                                                               \
                f1_ = right_img->get(n-1,m-1);                                                 \
                f0  = right_img->get(n-1,m  );                                                 \
                f1  = right_img->get(n-1,m+1);                                                 \
                f2  = right_img->get(n-1,m+2);                                                 \
                b1_ = 0.5 * ( 2*f0 + dec*(f1-f1_) + dec2*(2*f1_ - 5*f0 + 4*f1 - f2)            \
                              + dec3*(3*f0 - 3*f1 + f2 -f1_));                                 \
                                                                                               \
                f1_ = right_img->get(n,m-1);                                                   \
                f0  = right_img->get(n,m  );                                                   \
                f1  = right_img->get(n,m+1);                                                   \
                f2  = right_img->get(n,m+2);                                                   \
                b0  = 0.5 * ( 2*f0 + dec*(f1-f1_) + dec2*(2*f1_ - 5*f0 + 4*f1 - f2)            \
                              + dec3*(3*f0 - 3*f1 + f2 -f1_));                                 \
                                                                                               \
                f1_ = right_img->get(n+1,m-1);                                                 \
                f0  = right_img->get(n+1,m  );                                                 \
                f1  = right_img->get(n+1,m+1);                                                 \
                f2  = right_img->get(n+1,m+2);                                                 \
                b1  = 0.5 * ( 2*f0 + dec*(f1-f1_) + dec2*(2*f1_ - 5*f0 + 4*f1 - f2)            \
                              + dec3*(3*f0 - 3*f1 + f2 -f1_));                                 \
                                                                                               \
                f1_ = right_img->get(n+2,m-1);                                                 \
                f0  = right_img->get(n+2,m  );                                                 \
                f1  = right_img->get(n+2,m+1);                                                 \
                f2  = right_img->get(n+2,m+2);                                                 \
                b2  = 0.5 * ( 2*f0 + dec*(f1-f1_) + dec2*(2*f1_ - 5*f0 + 4*f1 - f2)            \
                              + dec3*(3*f0 - 3*f1 + f2 -f1_));                                 \
                                                                                               \
                /* Then combining in the other direction */                                    \
                                                                                               \
                dec = right_line - n;                                                          \
                right_dn  = 0.5 * ( 2*b0 + dec*(b1-b1_) + dec*dec*(2*b1_ - 5*b0 + 4*b1 - b2)   \
                              + dec*dec*dec*(3*b0 - 3*b1 + b2 -b1_));                          \
                                                                                               \
                                                                \
                sumy += right_dn; /* compute sums for least squares fit */ \
                sumy2 += right_dn*right_dn;			\
                sumxy += right_dn*left_img->get(j,i);	\
            }							\
        }

	switch (type) {
	    case Amoeba2:
		GRUEN_INNER_LOOP(Amoeba2)
		break;
	    case Amoeba4:
		GRUEN_INNER_LOOP(Amoeba4)
		break;
	    case Amoeba5:
		GRUEN_INNER_LOOP(Amoeba5)
		break;
	    case Amoeba6:
		GRUEN_INNER_LOOP(Amoeba6)
		break;
	    case Amoeba8:
		GRUEN_INNER_LOOP(Amoeba8)
		break;
	    case Amoeba2_bicubic:
		GRUEN_INNER_LOOP_BICUBIC(Amoeba2)
		break;
	    case Amoeba4_bicubic:
		GRUEN_INNER_LOOP_BICUBIC(Amoeba4)
		break;
	    case Amoeba5_bicubic:
		GRUEN_INNER_LOOP_BICUBIC(Amoeba5)
		break;
	    case Amoeba6_bicubic:
		GRUEN_INNER_LOOP_BICUBIC(Amoeba6)
		break;
	    case Amoeba8_bicubic:
		GRUEN_INNER_LOOP_BICUBIC(Amoeba8)
		break;
	}

        denom=params->left_sum[k]*(sumy2-sumy*sumy/rn);
        if (denom == 0.0) {
            params->ind=1;
            return bad_return;
        }
	numer = sumxy-params->sumx[k]*sumy/rn;
        r2=(numer*numer)/denom;
	if (numer < 0 && !params->inv_flag)  // Negative correlation... allowed?
	    r2 = - r2;			// nope... so q _is_ negative
        if (r2 > best_r2) best_r2=r2;
    }					/* end of areas loop */

    params->ind = 0;
    // logically:  (1.0 - best_r2) + 1.0;
    return 2.0 - best_r2;		/* invert so we can minimize */
}

/************************************************************************/
/* Routine to perform conventional correlation only.			*/
/* Correl is filled with the matrix of coefficient of determination values. */
/* Then the peak value is interpolated to sub pixel.			*/
/* Answer is returned with the offsets (samp in answer[0] and line in	*/
/* answer[1]).  Full affine transform is not supported here.		*/
/************************************************************************/

static void search_area(struct CostParams *params, double answer[2],
	double *quality, int inv_flag)
{
    double rn,right_dn,sumy,sumxy,sumy2;
    double r2,a,b,c,line_val[10],samp_val[10],qual[10];
    double numer,denom,sample,line;
    int i,j,m,n,kl,ks,num_areas,k,kll,kss;

    // Local variable copies only for consistency

    SimpleImage<double> *left_img = params->left_img;
    SimpleImage<double> *right_img = params->right_img;
    int nlw = left_img->getNL();
    int nsw = left_img->getNS();
    int nlw2 = right_img->getNL();
    int nsw2 = right_img->getNS();

    SimpleImage<double> *correl_img = params->correl_img;
    if (correl_img == NULL) {		// create it
	correl_img = new SimpleImage<double>(nlw2-nlw+1, nsw2-nsw+1);
	params->correl_img = correl_img;
    }
    if (correl_img->getNL() < (nlw2-nlw+1) ||
	correl_img->getNS() < (nsw2-nsw+1)) {
	zvmessage("Internal error: correl image not big enough in gruen linear mode","");
	zabend();
    }

    /* compute sub area limits if percent < 100 */
    num_areas=1;
    if (params->percent < 99.99) {
        num_areas=5;
        kl=(int)((nlw*(params->percent/100.)) + .5);
        if (kl > nlw) kl=nlw;
        if (kl < 1) kl=1;
        ks=(int)((nsw*(params->percent/100.)) + .5);
        if (ks > nlw) ks=nlw;
        if (ks < 1) ks=1;
        kll=nlw-kl-1;
        kss=nsw-ks-1;
    }

    /* loop on all sub areas */
    for (k=0; k < num_areas; k++) {

	/* compute area of sub area */
        if (k == 0) rn=nlw * nsw;
        if (k == 1) rn=kl * nsw;
        if (k == 2) rn=kl * nsw;
        if (k == 3) rn=ks * nlw;
        if (k == 4) rn=ks * nlw;

	/* fill correlation matrix */
        for (n=0; n < nlw2-nlw+1; n++) {
            for (m=0; m < nsw2-nsw+1; m++) {
                sumy=0.0;
                sumy2=0.0;
                sumxy=0.0;
                for (j=0; j < nlw; j++) {
                    for (i=0; i < nsw; i++) {
                        if (k == 1) {
                            if (j <= kll) continue;
                        }
                        if (k == 2) {
                            if (j >= kl) continue;
                        }
                        if (k == 3) {
                            if (i <= kss) continue;
                        }
                        if (k == 4) {
                            if (i >= ks) continue;
                        }
                        right_dn=right_img->get(j+n,i+m);
                        sumy += right_dn;
                        sumy2 += right_dn*right_dn;
                        sumxy += right_dn*left_img->get(j,i);
                    }
                }
		numer = sumxy-params->sumx[k]*sumy/rn;
		double denom = (params->left_sum[k]*(sumy2-sumy*sumy/rn));
		double correl_score = 0.0;
		if (denom != 0.0)
		    correl_score = numer*numer/denom;
		// If beyond the range, probably denom == 0 due to no variation
		// so we set correlation to lowest possible score
		if (correl_score > 1.0 || correl_score < -1.0)
		    correl_score = 0.0;
		correl_img->set(n,m, correl_score);
		if (numer < 0 && !inv_flag)	// allow negative correlation?
		    correl_img->set(n,m, - correl_score); // nope (so q _is_ negative)
            }
        }
  
	/* locate highest value */
        r2= -1.0;
        for (n=0; n < nlw2-nlw+1; n++) {
            for (m=0; m < nsw2-nsw+1; m++) {
                if (r2 < correl_img->get(n,m)) {
                    r2=correl_img->get(n,m);
                    j=n;
                    i=m;
                }
            }
        }

	/* reject point if on border */
	if ((j == 0) || (j == nlw2-nlw) ||
	   (i == 0) || (i == nsw2-nsw)) {
            if (num_areas == 1) {
		params->ind = 1;
		return;
            }
            else{
                qual[k]=0.;
                break;			/* exit this loop */
            }
        }

	/* compute sub-pixel location of best correlation.
	   See Numerical Recipes, eqn: 10.2.1, note b+1/2 should read b-1/2   */
        a=correl_img->get(j,i-1);
        b=2.0*correl_img->get(j,i);
        c=correl_img->get(j,i+1);
        denom=2.0*(b-c-a);
        if (denom != 0.0) {
            sample=(c-a)/denom+i+(nsw-1)/2;}
        else{
            sample=i+(nsw-1)/2;
        }
        a=correl_img->get(j-1,i);
        c=correl_img->get(j+1,i);
        denom=2.0*(b-c-a);
        if (denom != 0.0) {
            line=(c-a)/denom+j+(nlw-1)/2;}
        else{
            line=j+(nlw-1)/2;
        }

        line_val[k]=line;
        samp_val[k]=sample;
        qual[k]=r2;
    } /* end of areas loop */

    /* locate best area fit */
    if (num_areas > 1) {
        i= -1;
        r2=-1.0;
        for (k=0; k < num_areas; k++) {
            if (qual[k] > r2) {
                r2=qual[k];
                i=k;
            }
        }
        if (i == -1) {
            params->ind=1;
            return;
        }
        line=line_val[i];
        sample=samp_val[i];
        *quality=qual[i];
    }
    else
        *quality=correl_img->get(j,i);

    answer[0]=sample-(nsw2-1)/2;
    answer[1]=line-(nlw2-1)/2;

    params->ind = 0;
}

/************************************************************************/
/* Metropolis function to minimize by simulated annealing.		*/
/* This routine is a modified version of what's in r2lib (it's in C too)*/
/************************************************************************/

static void metropolis(struct CostParams *params, double answer[6],
	double range[6], int numten, int limits, int norm, double *quality)
{
      int fail1,fail2,success1,success2,limit,numreset,j,k,loop,narg,ind;
      unsigned int iseed;
      double temp[6],x[6],minx[6],mincost,pi,pi2,c1,c2,c3,scale,numtenf;
      double costsum,energy,boltzman,rand_max,ran,prob;

      pi=3.14159;
      pi2=pi/2.0;
      limit=limits;
      iseed=1;		/*!!!! p2 ones use zrangen() and a seed of 10109854 */
      srand(iseed);
      narg=6;
      numtenf=numten;
      fail1=0;
      fail2=0;
      success1=0;
      success2=0;
      numreset=numten/10;
      scale=exp((log(0.1))/numtenf);
      loop=0;
      rand_max=pow(2.0,31.0)-1.0;	/*!!!! Unix man page says 0..2^15-1! */

/*  Compute the cost at position ANSWER and assign to variable C1. */
      c1 = cost(answer, 6, params) - 1.0;
      if (params->ind != 0) {
         return;
      }

/*  Save the cost in case the user had good reason to inspect this
    solution position. */
      mincost=c1;
      for (j=0; j < narg; j++) {
         minx[j]=answer[j];
      }

/*  Set initial temperatures to the range estimates. */
      for (j=0; j < narg; j++) {
         temp[j]=range[j];
      }

/*   MAIN LOOP: loop on number of successful changes in solution space. */
      while(loop < limit) {

/*   Compute the delta_cost/temperature ratio for
     normalization of probabilities.
     Note that this is the Boltzmann constant for this 'system'.*/

        k=loop/norm;
        if (loop-k*norm == 0) {
           costsum=0.0;
           k=0;
           for (j=0; j < narg; j++) {
              x[j]=answer[j];
           }
           for (j=0; j < narg; j++) {
              x[j]=answer[j]-temp[j];
              c2 = cost(x, 6, params) - 1.0;
              if (params->ind == 0) {
                k=k+1;
                costsum=costsum+fabs(c1-c2)/temp[j];
              }
              x[j]=answer[j]+temp[j];
              c2 = cost(x, 6, params) - 1.0;
              if (ind == 0) {
                k=k+1;
                costsum=costsum+fabs(c1-c2)/temp[j];
              }
              x[j]=answer[j];
           }
           if (k == 0) {
              params->ind=2;
              return;
           }
           boltzman=5.0*(costsum/k);
        }

/*      Decrement the temperature according to the multiplicative
        cooling schedule.  */
        for (j=0; j < narg; j++) {
           temp[j]=temp[j]*scale;
        }
        energy=boltzman*(temp[0]+temp[1]+temp[2]+temp[3]+temp[4]
                        +temp[5])/6.0;

/*      Compute a solution space guess using a Cauchy-Lorentzian
        random probability distribution function. */
	do {
            for (j=0; j < narg; j++) {
               ran=rand()/rand_max;
               x[j]=temp[j]*tan(pi*ran+pi2)+answer[j];
            }
            c2 = cost(x, 6, params) - 1.0;
            if (params->ind != 0) {
               fail1 += 1;
            }
	} while (params->ind != 0);

        if (c2 < c1) {

/*          Accept lower cost position.
            We always accept a downhill cost route if offered.*/

            success1 += 1;
            c1=c2;
            for (j=0; j < narg; j++) {
               answer[j]=x[j];
            }
        }
        else{
/*          Compute probability of accepting higher cost position.
            This comes from the Boltzmann probability of our system 
            transitioning from energy state c1 to energy state c2.*/

            c3=(c2-c1)/energy;
            if (c3 > 50.) {
               goto A92;
            }
            prob=1.0/exp((double)c3);

/*          Evaluate the probability by comparing it against chance.*/

            ran=rand()/rand_max;
            if (prob > ran) {
/*              Accept higher cost position.*/
                success2 += 1;
                c1=c2;
                for (j=0; j < narg; j++) {
                   answer[j]=x[j];
                }
            }
            else{
/*              Reject higher cost position.*/
                fail2 += 1;
                goto A92;
            }
        }

/*       Save the minimum cost and associated solution as we go.*/

        if (c1 < mincost) {
            mincost=c1;
            for (j=0; j < narg; j++) {
               minx[j]=answer[j];
            }
        }
A92:
        loop=loop+1;

/*       Reset the solution pointer to the minimum cost
         location every numreset successful iterations. */

        k=loop/numreset;
        if (loop-k*numreset == 0) {
            c1=mincost;
            for (j=0; j < narg; j++) {
               answer[j]=minx[j];
            }
        }

      }  /*   END of MAIN WHILE LOOP  */

/*     Put minimum solution into ANSWER & it's cost into quality  */

      for (j=0; j < narg; j++) {
         answer[j]=minx[j];
      }
      *quality=1.0-mincost;

      params->ind = 0;
 /*printf("Initial ok=%d bad=%d, Probability ok=%d bad=%d\n",success1,
         fail1,success2,fail2);*/
}

