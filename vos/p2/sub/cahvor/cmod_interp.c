/******************************************************************************
*                                                                             *
*                            C M O D _ I N T E R P                            *
*                                                                             *
*                                       Todd Litwin                           *
*                                       Written:  9 Mar 2007                  *
*                                       Updated:  5 Jan 2012                  *
*                                                                             *
*                                       Copyright (C) 2007, 2008, 2009, 2010, *
*                                                     2011, 2012              *
*                                       California Institute of Technology    *
*                                       All Rights Reserved                   *
*                                                                             *
*******************************************************************************


	This file contains a function to interpolate camera models.

	The original version of this software used a general polynomial
	fitter as the basis of the interpolation, but this lead to
	undesirable results. The current version performs a linear
	interpolation between (or past) nearest neighbors. In the future
	it might make sense to consider a cubic spline fit. */


#include <math.h>
#include <mat3.h>

#include "cmod.h"
#include "cmod_interp.h"
#include "cmod_error.h"

enum {
    SUCCESS =  0,
    FAILURE = -1
    };

#ifndef EPSILON
#define EPSILON (1e-15)
#endif

cmod_float_t cmod_interp_get_cahv   (const cmod_t *cmod, cmod_int_t i);
cmod_float_t cmod_interp_get_cahvor (const cmod_t *cmod, cmod_int_t i);
cmod_float_t cmod_interp_get_cahvore(const cmod_t *cmod, cmod_int_t i);

cmod_float_t cmod_interp_get_scale(
    const cmod_float_t a[3], const cmod_float_t x[3]);

cmod_float_t cmod_interp_get_center(
    const cmod_float_t a[3], const cmod_float_t x[3]);

void cmod_interp_linear(
    cmod_int_t n, const cmod_float_t xi[], const cmod_float_t yi[],
    cmod_float_t x, cmod_float_t *y);

void cmod_interp_upd_vec(
    const cmod_float_t a[3], cmod_float_t x[3],
    cmod_float_t xs, cmod_float_t xc);


/******************************************************************************
********************************   CMOD_INTERP   ******************************
*******************************************************************************

    Interpolates a camera model. */

cmod_stat_t cmod_interp(
    cmod_int_t nmods,		/* input number of input camera models */
    const cmod_t cmodi[],	/* input list of camera models to fit */
    const cmod_float_t xi[],	/* input list of corresponding independent
					variables */
    cmod_float_t x,		/* input value of interpolation target */
    cmod_t *cmodo)		/* output camera model */
{
    cmod_int_t i;
    cmod_int_t j;
    cmod_int_t mtype;
    cmod_int_t nparms;
    cmod_int_t xdim;
    cmod_int_t ydim;

    cmod_float_t mparm;
    cmod_class_t mclass;
    cmod_float_t yi[CMOD_INTERP_MAX_MODELS];
    cmod_float_t yo[21+4];

    cmod_float_t *c = yo +  0;
    cmod_float_t *a = yo +  3;
    cmod_float_t *h = yo +  6;
    cmod_float_t *v = yo +  9;
    cmod_float_t *o = yo + 12;
    cmod_float_t *r = yo + 15;
    cmod_float_t *e = yo + 18;

    cmod_float_t *hs;
    cmod_float_t *hc;
    cmod_float_t *vs;
    cmod_float_t *vc;

    /* Check pointers */
    CMOD_ASSERT("cmod_interp", cmodi != NULL);
    CMOD_ASSERT("cmod_interp", xi    != NULL);
    CMOD_ASSERT("cmod_interp", cmodo != NULL);

    /* Check dimensions */
    CMOD_ASSERT("cmod_interp", nmods >  0);
    CMOD_ASSERT("cmod_interp", nmods <= CMOD_INTERP_MAX_MODELS);

    /* Verify that the models are consistent as a set */
    for (i=1; i<nmods; i++) {
	if ((cmodi[i].xdim   != cmodi[i-1].xdim) ||
	    (cmodi[i].ydim   != cmodi[i-1].ydim) ||
	    (cmodi[i].mclass != cmodi[i-1].mclass)) {
	    CMOD_ERROR_II("cmod_interp", "Models not consistent", i-1, i);
	    return FAILURE;
	    }
	}
    xdim   = cmodi[0].xdim;
    ydim   = cmodi[0].ydim;
    mclass = cmodi[0].mclass;
    mtype  = 0;
    mparm  = 0;

    /* Special checking for CAHVORE models */
    if (mclass == CMOD_CLASS_CAHVORE) {
	for (i=1; i<nmods; i++) {
	    if ((cmodi[i].u.cahvore.mtype != cmodi[i-1].u.cahvore.mtype) ||
		(cmodi[i].u.cahvore.mparm != cmodi[i-1].u.cahvore.mparm)) {
		CMOD_ERROR_II("cmod_interp",
			"CAHVORE models not consistent", i-1, i);
		return FAILURE;
		}
	    }
	mtype = cmodi[0].u.cahvore.mtype;
	mparm = cmodi[0].u.cahvore.mparm;
	}

    /* Setup for each model type */
    switch (mclass) {
	case CMOD_CLASS_CAHV:
	    nparms = 12;
	    break;
	case CMOD_CLASS_CAHVOR:
	    nparms = 18;
	    break;
	case CMOD_CLASS_CAHVORE:
	    nparms = 21;
	    break;
	default:
	    CMOD_ASSERT_1("cmod_interp", 0, mclass);
	    return FAILURE;
	}

    /* Add the internal model parameters to the end of the list */
    hs = yo + nparms++;
    hc = yo + nparms++;
    vs = yo + nparms++;
    vc = yo + nparms++;

    /* Fit each model parameter individually */
    for (i=0; i<nparms; i++) {

	/* Gather the i'th dependent variable for all models */
	switch (mclass) {
	    case CMOD_CLASS_CAHV:
		for (j=0; j<nmods; j++) {
		    yi[j] = cmod_interp_get_cahv(cmodi+j, i);	/* c .. v */
		    }
		break;
	    case CMOD_CLASS_CAHVOR:
		for (j=0; j<nmods; j++) {
		    yi[j] = cmod_interp_get_cahvor(cmodi+j, i);	/* c .. r */
		    }
		break;
	    case CMOD_CLASS_CAHVORE:
		for (j=0; j<nmods; j++) {
		    yi[j] = cmod_interp_get_cahvore(cmodi+j, i);/* c .. e */
		    }
		break;
	    default:
		CMOD_ASSERT_1("cmod_interp", 0, mclass);
		return FAILURE;
	    }

	/* Interpolate using a linear, piecewise fit */
	cmod_interp_linear(nmods, xi, yi, x, yo+i);
	}

    /* Add constraints for and complete the model */
    unit3(a, a);
    cmod_interp_upd_vec(a, h, *hs, *hc);
    cmod_interp_upd_vec(a, v, *vs, *vc);
    switch (mclass) {
	case CMOD_CLASS_CAHV:
	    cmod_init_cahv(xdim, ydim, c, a, h, v, cmodo);
	    break;
	case CMOD_CLASS_CAHVOR:
	    unit3(o, o);
	    cmod_init_cahvor(xdim, ydim, c, a, h, v, o, r, cmodo);
	    break;
	case CMOD_CLASS_CAHVORE:
	    unit3(o, o);
	    cmod_init_cahvore(xdim, ydim, mtype, mparm, c, a, h, v, o, r, e,
				cmodo);
	    break;
	default:
	    CMOD_ASSERT_1("cmod_interp", 0, mclass);
	    return FAILURE;
	}

    return SUCCESS;
    }


/******************************************************************************
*                                                                             *
*                           Internal Support Functions                        *
*                                                                             *
******************************************************************************/


/******************************************************************************
********************************   CMOD_INTERP_GET_CAHV   *********************
*******************************************************************************

    Gets the i'th CAHV parameter out of the given model. */

cmod_float_t cmod_interp_get_cahv(
    const cmod_t *cmod,	/* input camera models */
    cmod_int_t i)	/* input parameter index, 0-15 */
{
    CMOD_ASSERT  ("cmod_interp_get_cahv", cmod != NULL);
    CMOD_ASSERT_1("cmod_interp_get_cahv", i >= 0, i);
    CMOD_ASSERT_1("cmod_interp_get_cahv", i < 12+4, i);

    if (i < 3) {
	return cmod->u.cahv.c[i-0];
	}
    else if (i < 6) {
	return cmod->u.cahv.a[i-3];
	}
    else if (i < 9) {
	return cmod->u.cahv.h[i-6];
	}
    else if (i < 12) {
	return cmod->u.cahv.v[i-9];
	}
    else if (i < 13) {
	return cmod_interp_get_scale (cmod->u.cahv.a, cmod->u.cahv.h);
	}
    else if (i < 14) {
	return cmod_interp_get_center(cmod->u.cahv.a, cmod->u.cahv.h);
	}
    else if (i < 15) {
	return cmod_interp_get_scale (cmod->u.cahv.a, cmod->u.cahv.v);
	}
    else {
	return cmod_interp_get_center(cmod->u.cahv.a, cmod->u.cahv.v);
	}
    }


/******************************************************************************
********************************   CMOD_INTERP_GET_CAHVOR   *******************
*******************************************************************************

    Gets the i'th CAHVOR parameter out of the given model. */

cmod_float_t cmod_interp_get_cahvor(
    const cmod_t *cmod,	/* input camera models */
    cmod_int_t i)	/* input parameter index, 0-21 */
{
    CMOD_ASSERT  ("cmod_interp_get_cahvor", cmod != NULL);
    CMOD_ASSERT_1("cmod_interp_get_cahvor", i >= 0, i);
    CMOD_ASSERT_1("cmod_interp_get_cahvor", i < 18+4, i);

    if (i < 3) {
	return cmod->u.cahvor.c[i- 0];
	}
    else if (i < 6) {
	return cmod->u.cahvor.a[i- 3];
	}
    else if (i < 9) {
	return cmod->u.cahvor.h[i- 6];
	}
    else if (i < 12) {
	return cmod->u.cahvor.v[i- 9];
	}
    else if (i < 15) {
	return cmod->u.cahvor.o[i-12];
	}
    else if (i < 18) {
	return cmod->u.cahvor.r[i-15];
	}
    else if (i < 19) {
	return cmod_interp_get_scale (cmod->u.cahvor.a, cmod->u.cahvor.h);
	}
    else if (i < 20) {
	return cmod_interp_get_center(cmod->u.cahvor.a, cmod->u.cahvor.h);
	}
    else if (i < 21) {
	return cmod_interp_get_scale (cmod->u.cahvor.a, cmod->u.cahvor.v);
	}
    else {
	return cmod_interp_get_center(cmod->u.cahvor.a, cmod->u.cahvor.v);
	}
    }


/******************************************************************************
********************************   CMOD_INTERP_GET_CAHVORE   ******************
*******************************************************************************

    Gets the i'th CAHVORE parameter out of the given model. */

cmod_float_t cmod_interp_get_cahvore(
    const cmod_t *cmod,	/* input camera models */
    cmod_int_t i)	/* input parameter index, 0-24 */
{
    CMOD_ASSERT  ("cmod_interp_get_cahvore", cmod != NULL);
    CMOD_ASSERT_1("cmod_interp_get_cahvore", i >= 0, i);
    CMOD_ASSERT_1("cmod_interp_get_cahvore", i < 21+4, i);

    if (i < 3) {
	return cmod->u.cahvore.c[i- 0];
	}
    else if (i < 6) {
	return cmod->u.cahvore.a[i- 3];
	}
    else if (i < 9) {
	return cmod->u.cahvore.h[i- 6];
	}
    else if (i < 12) {
	return cmod->u.cahvore.v[i- 9];
	}
    else if (i < 15) {
	return cmod->u.cahvore.o[i-12];
	}
    else if (i < 18) {
	return cmod->u.cahvore.r[i-15];
	}
    else if (i < 21) {
	return cmod->u.cahvore.e[i-18];
	}
    else if (i < 22) {
	return cmod_interp_get_scale (cmod->u.cahvore.a, cmod->u.cahvore.h);
	}
    else if (i < 23) {
	return cmod_interp_get_center(cmod->u.cahvore.a, cmod->u.cahvore.h);
	}
    else if (i < 24) {
	return cmod_interp_get_scale (cmod->u.cahvore.a, cmod->u.cahvore.v);
	}
    else {
	return cmod_interp_get_center(cmod->u.cahvore.a, cmod->u.cahvore.v);
	}
    }


/******************************************************************************
********************************   CMOD_INTERP_GET_CENTER   *******************
*******************************************************************************

    Gets the center internal model parameter from the input vector. */

cmod_float_t cmod_interp_get_center(
    const cmod_float_t a[3],	/* input axis vector */
    const cmod_float_t x[3])	/* input horizontal or vertical vector */
{
    return dot3(a, x);
    }


/******************************************************************************
********************************   CMOD_INTERP_GET_SCALE   ********************
*******************************************************************************

    Gets the scale internal model parameter from the input vector. */

cmod_float_t cmod_interp_get_scale(
    const cmod_float_t a[3],	/* input axis vector */
    const cmod_float_t x[3])	/* input horizontal or vertical vector */
{
    cmod_float_t cross[3];
    return mag3(cross3(a, x, cross));
    }


/******************************************************************************
********************************   CMOD_INTERP_LINEAR   ***********************
*******************************************************************************

    Performs a linear interpolation. In this version no statistical fitting is
    done. Rather the two nearest neihbors are identified and a linear
    interpolation (or extrapolation) is performed. */

void cmod_interp_linear(
    cmod_int_t n,		/* input number of data points */
    const cmod_float_t xi[],	/* input array of independent variables */
    const cmod_float_t yi[],	/* input array of dependent variables */
    cmod_float_t x,		/* input value of interpolation target */
    cmod_float_t *y)		/* output interpolated value */
{
    cmod_int_t i;
    cmod_int_t j;
    cmod_int_t i0;
    cmod_int_t i1;
    cmod_int_t idx[CMOD_INTERP_MAX_MODELS];

    CMOD_ASSERT("cmod_interp_linear", n  >  0);
    CMOD_ASSERT("cmod_interp_linear", n  <= CMOD_INTERP_MAX_MODELS);
    CMOD_ASSERT("cmod_interp_linear", xi != NULL);
    CMOD_ASSERT("cmod_interp_linear", yi != NULL);
    CMOD_ASSERT("cmod_interp_linear", y  != NULL);

    /* Handle the special case of a single model */
    if (n == 1) {
	*y = yi[0];
	return;
	}

    /* Create a sorted index list for increasing xi[]. A bubble short is */
    /* good enough since efficiency doesn't matter for such short lists. */
    for (i=0; i<n; i++) {
	idx[i] = i;
	}
    for (j=1; j<n; j++) {
	for (i=1; i<n; i++) {
	    if (xi[idx[i]] < xi[idx[i-1]]) {
		i0 = idx[i];
		idx[i] = idx[i-1];
		idx[i-1] = i0;
		}
	    }
	}

    /* Find where x falls in the list */
    i0 = idx[0];
    i1 = idx[1];
    for (i=2; (i < n) && (xi[i1] < x); i++) {
	i0 = i1;
	i1 = idx[i];
	}

    /* Check for sufficient space between them to perform interpolation */
    if (fabs(xi[i0] - xi[i1]) < EPSILON) {
	*y = (yi[i0] + yi[i1]) / 2.0;
	return;
	}

    /* Interpolate */
    *y = yi[i0] + (yi[i1] - yi[i0]) * (x - xi[i0]) / (xi[i1] - xi[i0]);
    }


/******************************************************************************
********************************   CMOD_INTERP_UPD_VEC   **********************
*******************************************************************************

    Update the horizontal or vertical vector by correcting it with the
    scale and center. */

void cmod_interp_upd_vec(
    const cmod_float_t a[3],	/* input axis vector */
    cmod_float_t x[3],		/* input/output horizontal or vertical vector */
    cmod_float_t xs,		/* input scale */
    cmod_float_t xc)		/* input center */
{
    cmod_float_t t[3];

    /* Project x into the image plane and then make into a unit vector */
    scale3(cmod_interp_get_center(a, x), a, t);
    sub3(x, t, x);
    unit3(x, x);

    /* Apply the interpolated scale and center to reconstruct */
    scale3(xs, x, x);
    scale3(xc, a, t);
    add3(t, x, x);
    }
