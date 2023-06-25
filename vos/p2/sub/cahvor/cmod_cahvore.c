/******************************************************************************
*                                                                             *
*                            C M O D _ C A H V O R E                          *
*                                                                             *
*                                       Todd Litwin                           *
*                                       Written: 11 May 1998                  *
*                                       Updated: 17 Nov 2022                  *
*                                                                             *
*                                       Copyright (C) 1998, 2000, 2001, 2002, *
*                                                     2003, 2004, 2005, 2007, *
*                                                     2009, 2010, 2011, 2012, *
*                                                     2015, 2016, 2018, 2022  *
*                                       California Institute of Technology    *
*                                       All Rights Reserved                   *
*                                                                             *
*******************************************************************************


	This file contains functions for using the camera model known
	locally as CAHVORE. This model is an extension by Yalin Xiong
	and Donald Gennery of the earlier CAHVOR (Gennery) and CAHV
	(Yakimovsky & Cunningham) models. */


#include <math.h>
#include <mat3.h>

#include "cmod_psph.h"
#include "cmod_cahv.h"
#include "cmod_cahvor.h"
#include "cmod_cahvore.h"
#include "cmod_error.h"

enum {
    SUCCESS =  0,
    FAILURE = -1
    };

enum {
    MAX_NEWTON = 100
    };

#ifndef EPSILON
#define EPSILON (1e-15)
#endif

#define	PI (3.14159265358979323846)

#define MIN_PLANE_ANGLE (15.0 * PI / 180.0)

void cmod_cahvore_2d_to_3d_general(const cmod_float_t pos2[2],
    cmod_float_t linearity, const cmod_float_t c[3], const cmod_float_t a[3],
    const cmod_float_t h[3], const cmod_float_t v[3], const cmod_float_t o[3],
    const cmod_float_t r[3], const cmod_float_t e[3], cmod_bool_t approx,
    cmod_float_t pos3[3], cmod_float_t uvec3[3], cmod_float_t ppar[3][2],
    cmod_float_t upar[3][2]);

void cmod_cahvore_3d_to_2d_general(const cmod_float_t pos3[3],
    cmod_float_t linearity, const cmod_float_t c[3], const cmod_float_t a[3],
    const cmod_float_t h[3], const cmod_float_t v[3], const cmod_float_t o[3],
    const cmod_float_t r[3], const cmod_float_t e[3], cmod_bool_t approx,
    cmod_float_t *range, cmod_float_t pos2[2], cmod_float_t par[2][3]);

void cmod_cahvore_warp_limit(cmod_int_t mtype, cmod_float_t mparm,
    const cmod_float_t c[3], const cmod_float_t a[3], const cmod_float_t h[3],
    const cmod_float_t v[3], const cmod_float_t o[3], const cmod_float_t r[3],
    const cmod_float_t e[3], cmod_float_t limfov, cmod_bool_t minfov,
    const cmod_float_t nrm[3], const cmod_float_t dir[3],
    const cmod_float_t alt[3], cmod_float_t xcent, cmod_float_t ycent,
    cmod_float_t xmin, cmod_float_t ymin, cmod_float_t xmax, cmod_float_t ymax,
    cmod_float_t vect[3], cmod_float_t *dist);


/******************************************************************************
********************************   CMOD_CAHVORE_2D_TO_3D   ********************
*******************************************************************************

    This function projects a 2D image point out into 3D using the
    camera model parameters provided. In addition to the 3D projection,
    it outputs the partial-derivative matrices of the projection point
    and the unit vector with respect to the 2D image-plane point. If the
    parameter for either output partial matrix is passed as
    (cmod_float_t *)NULL, then it will not be calculated, note that it is
    necessary to calculate the partial for the unit vector if the one for
    the projection point is desired. */

void cmod_cahvore_2d_to_3d(
    const cmod_float_t pos2[2],	/* input 2D position */
    cmod_int_t mtype,		/* input type of model */
    cmod_float_t mparm,		/* input model parameter */
    const cmod_float_t c[3],	/* input model center position vector   C */
    const cmod_float_t a[3],	/* input model orthog. axis unit vector A */
    const cmod_float_t h[3],	/* input model horizontal vector        H */
    const cmod_float_t v[3],	/* input model vertical vector          V */
    const cmod_float_t o[3],	/* input model optical axis unit vector O */
    const cmod_float_t r[3],	/* input model radial-distortion terms  R */
    const cmod_float_t e[3],	/* input model entrance-pupil    terms  E */
    cmod_bool_t approx,		/* input flag to use fast approximation */
    cmod_float_t pos3[3],	/* output 3D origin of projection */
    cmod_float_t uvec3[3],	/* output unit vector ray of projection */
    cmod_float_t ppar[3][2],	/* output partial derivative of pos3  to pos2 */
    cmod_float_t upar[3][2])	/* output partial derivative of uvec3 to pos2 */
{
    cmod_float_t linearity = -1;

    switch (mtype) {

	case CMOD_CAHVORE_TYPE_PERSPECTIVE:	/* perspective projection */
	    linearity = 1;
	    break;

	case CMOD_CAHVORE_TYPE_FISHEYE:		/* fisheye */
	    linearity = 0;
	    break;

	case CMOD_CAHVORE_TYPE_GENERAL:		/* parametric */
	    linearity = mparm;
	    break;

	default:
	    CMOD_ASSERT_1("cmod_cahvore_2d_to_3d", 0, mtype);
	}

    cmod_cahvore_2d_to_3d_general(pos2, linearity, c, a, h, v, o, r, e, approx,
					pos3, uvec3, ppar, upar);
    }


/******************************************************************************
********************************   CMOD_CAHVORE_2D_TO_3D_GENERAL   ************
*******************************************************************************

    This function projects a 2D image point out into 3D using the
    camera model parameters provided. In addition to the 3D projection,
    it outputs the partial-derivative matrices of the projection point
    and the unit vector with respect to the 2D image-plane point. If the
    parameter for either output partial matrix is passed as
    (cmod_float_t *)NULL, then it will not be calculated, note that it is
    necessary to calculate the partial for the unit vector if the one for
    the projection point is desired. */

void cmod_cahvore_2d_to_3d_general(
    const cmod_float_t pos2[2],	/* input 2D position */
    cmod_float_t linearity,	/* input linearity parameter */
    const cmod_float_t c[3],	/* input model center position vector   C */
    const cmod_float_t a[3],	/* input model orthog. axis unit vector A */
    const cmod_float_t h[3],	/* input model horizontal vector        H */
    const cmod_float_t v[3],	/* input model vertical vector          V */
    const cmod_float_t o[3],	/* input model optical axis unit vector O */
    const cmod_float_t r[3],	/* input model radial-distortion terms  R */
    const cmod_float_t e[3],	/* input model entrance-pupil    terms  E */
    cmod_bool_t approx,		/* input flag to use fast approximation */
    cmod_float_t pos3[3],	/* output 3D origin of projection */
    cmod_float_t uvec3[3],	/* output unit vector ray of projection */
    cmod_float_t ppar[3][2],	/* output partial derivative of pos3  to pos2 */
    cmod_float_t upar[3][2])	/* output partial derivative of uvec3 to pos2 */
{
    cmod_float_t avh1;
    cmod_float_t chi;
    cmod_float_t chi2;
    cmod_float_t chi3;
    cmod_float_t chi4;
    cmod_float_t chi5;
    cmod_float_t chip;
    cmod_float_t cp[3];
    cmod_float_t dcpdrp[3][3];
    cmod_float_t dcpdx[3];
    cmod_float_t dcpdy[3];
    cmod_float_t drdrp[3][3];
    cmod_float_t drdx[3];
    cmod_float_t drdy[3];
    cmod_float_t drpdx[3];
    cmod_float_t drpdy[3];
    cmod_float_t lambdap;
    cmod_float_t lambdap3[3];
    cmod_float_t linchi;
    cmod_float_t m33[3][3];
    cmod_float_t n33[3][3];
    cmod_float_t ri[3];
    cmod_float_t rp[3];
    cmod_float_t theta;
    cmod_float_t theta2;
    cmod_float_t theta3;
    cmod_float_t theta4;
    cmod_float_t u3[3];
    cmod_float_t v3[3];
    cmod_float_t w3[3];
    cmod_float_t zetap;

    /* Check input */
    CMOD_ASSERT("cmod_cahvore_2d_to_3d_general", r    != NULL);
    CMOD_ASSERT("cmod_cahvore_2d_to_3d_general", e    != NULL);
    CMOD_ASSERT("cmod_cahvore_2d_to_3d_general", pos2 != NULL);

    /* In the following there is a mixture of nomenclature from several */
    /* versions of Gennery's write-ups and Litwin's software. Beware!   */

    chi = 0;
    chi3 = 0;
    theta = 0;
    theta2 = 0;
    theta3 = 0;
    theta4 = 0;

    /* Calculate initial terms */

    scale3(pos2[1], a, u3);
    sub3(v, u3, u3);
    scale3(pos2[0], a, v3);
    sub3(h, v3, v3);
    cross3(u3, v3, w3);
    cross3(v, h, u3);
    avh1 = dot3(a, u3);
    CMOD_ASSERT("cmod_cahvore_2d_to_3d_general", fabs(avh1) > EPSILON);
    avh1 = 1/avh1;
    scale3(avh1, w3, rp);

    zetap = dot3(rp, o);

    scale3(zetap, o, u3);
    sub3(rp, u3, lambdap3);

    lambdap = mag3(lambdap3);

    CMOD_ASSERT("cmod_cahvore_2d_to_3d_general", fabs(zetap) > EPSILON);
    chip = lambdap / zetap;

    /* Approximations for small angles */
    if (chip < 1e-8) {
	copy3(c, cp);
	copy3(o, ri);
	}

    /* Full calculations */
    else {
	cmod_int_t n;
	cmod_float_t dchi;
	cmod_float_t s;

	/* Calculate chi using Newton's Method */
	n = 0;
	chi = chip;
	dchi = 1;
	for (;;) {
	    cmod_float_t deriv;

	    /* Make sure we don't iterate forever */
	    if (++n > MAX_NEWTON) {
		CMOD_ERROR("cahvore_2d_to_3d_general", "too many iterations");
		break;
		}

	    /* Compute terms from the current value of chi */
	    chi2 = chi * chi;
	    chi3 = chi * chi2;
	    chi4 = chi * chi3;
	    chi5 = chi * chi4;

	    /* Check exit criterion from last update */
	    if (fabs(dchi) < 1e-8) {
		break;
		}

	    /* Update chi */
	    deriv = (1 + r[0]) + 3*r[1]*chi2 + 5*r[2]*chi4;
	    CMOD_ASSERT("cmod_cahvore_2d_to_3d_general", fabs(deriv) > EPSILON);
	    dchi = ((1 + r[0])*chi + r[1]*chi3 + r[2]*chi5 - chip) / deriv;
	    chi -= dchi;
	    }

	/* Compute the incoming ray's angle */
	linchi = linearity * chi;
	if (linearity < -EPSILON) {
	    theta = asin(linchi) / linearity;
	    }
	else if (linearity > EPSILON) {
	    theta = atan(linchi) / linearity;
	    }
	else {
	    theta = chi;
	    }

	theta2 = theta * theta;
	theta3 = theta * theta2;
	theta4 = theta * theta3;

	/* Compute the shift of the entrance pupil */
	s = sin(theta);
	CMOD_ASSERT("cmod_cahvore_2d_to_3d_general", fabs(s) > EPSILON);
	s = (theta/s - 1) * (e[0] + e[1]*theta2 + e[2]*theta4);

	/* The position of the entrance pupil */
	scale3(s, o, cp);
	add3(c, cp, cp);

	/* The unit vector along the ray */
	unit3(lambdap3, u3);
	scale3(sin(theta), u3, u3);
	scale3(cos(theta), o, v3);
	add3(u3, v3, ri);
	}

    copy3(cp, pos3);
    copy3(ri, uvec3);

    /* Optionally calculate the partial of pos3 & unit3 with respect to pos2 */
    if ((ppar == NULL) || (upar == NULL)) {
	return;
	}

    cross3(v, a, u3);
    scale3(-avh1, u3, drpdx);

    cross3(h, a, u3);
    scale3(avh1, u3, drpdy);

    /* Approximations for small angles */
    if (chip < 1e-8) {
	cmod_float_t x;

	zero33(dcpdrp);

	ident33(n33);
	mult313(o, o, m33);
	sub33(n33, m33, n33);
	x = mag3(rp) * (1 + r[0]);
	CMOD_ASSERT("cmod_cahvore_2d_to_3d_general", fabs(x) > EPSILON);
	scale33(1/x, n33, drdrp);
	}

    /* Full calculations */
    else {
	cmod_float_t costh;
	cmod_float_t linth;
	cmod_float_t nu;
	cmod_float_t psi;
	cmod_float_t omega;
	cmod_float_t sinth;
	cmod_float_t t;
	cmod_float_t x;

	sinth = sin(theta);
	costh = cos(theta);

	nu = 2*r[1]*chi + 4*r[2]*chi3;

	omega = nu*chi + chip/chi;

	CMOD_ASSERT("cmod_cahvore_2d_to_3d_general", fabs(sinth) > EPSILON);
	t = (1/sinth - theta*costh/(sinth*sinth))
				* (e[0] +   e[1]*theta2 +   e[2]*theta4)
	  + (theta/sinth - 1)	* (       2*e[1]*theta  + 4*e[2]*theta3);

	linth = linearity * theta;
	if (linearity < -EPSILON) {
	    psi = cos(linth);
	    }
	else if (linearity > EPSILON) {
	    psi = cos(linth);
	    CMOD_ASSERT("cmod_cahvore_2d_to_3d_general", fabs(psi) > EPSILON);
	    psi = 1/psi;
	    psi *= psi;
	    }
	else {
	    psi = 1;
	    }

	mult313(o, lambdap3, n33);
	x = psi * zetap * lambdap * omega;
	CMOD_ASSERT("cmod_cahvore_2d_to_3d_general", fabs(x) > EPSILON);
	scale33(t/x, n33, n33);
	mult313(o, o, m33);
	x = psi * zetap * omega;
	CMOD_ASSERT("cmod_cahvore_2d_to_3d_general", fabs(x) > EPSILON);
	scale33(t*chip/x, m33, m33);
	sub33(n33, m33, dcpdrp);

	ident33(n33);
	CMOD_ASSERT("cmod_cahvore_2d_to_3d_general", fabs(lambdap) > EPSILON);
	scale33(sinth/lambdap, n33, n33);
	mult313(o, o, m33);
	x = psi * zetap * zetap * omega;
	CMOD_ASSERT("cmod_cahvore_2d_to_3d_general", fabs(x) > EPSILON);
	scale33(lambdap*sinth/x - sinth/lambdap, m33, m33);
	add33(n33, m33, n33);
	mult313(lambdap3, o, m33);
	x = psi * zetap * zetap * omega;
	CMOD_ASSERT("cmod_cahvore_2d_to_3d_general", fabs(x) > EPSILON);
	scale33(costh/x, m33, m33);
	sub33(n33, m33, n33);
	mult313(o, lambdap3, m33);
	x = psi * zetap * lambdap * omega;
	CMOD_ASSERT("cmod_cahvore_2d_to_3d_general", fabs(x) > EPSILON);
	scale33(sinth/x, m33, m33);
	sub33(n33, m33, n33);
	mult313(lambdap3, lambdap3, m33);
	x = psi * zetap * lambdap * lambdap * omega;
	CMOD_ASSERT("cmod_cahvore_2d_to_3d_general", fabs(x) > EPSILON);
	scale33(costh/x - sinth/(lambdap*lambdap*lambdap), m33, m33);
	add33(n33, m33, drdrp);
	}

    /* Complete the partial derivatives */

    mult331(dcpdrp, drpdx, dcpdx);
    mult331(dcpdrp, drpdy, dcpdy);
    mult331(drdrp,  drpdx, drdx);
    mult331(drdrp,  drpdy, drdy);

    ppar[0][0] = dcpdx[0];
    ppar[1][0] = dcpdx[1];
    ppar[2][0] = dcpdx[2];
    ppar[0][1] = dcpdy[0];
    ppar[1][1] = dcpdy[1];
    ppar[2][1] = dcpdy[2];

    upar[0][0] = drdx[0];
    upar[1][0] = drdx[1];
    upar[2][0] = drdx[2];
    upar[0][1] = drdy[0];
    upar[1][1] = drdy[1];
    upar[2][1] = drdy[2];

    /* If requested, just use the approximations for speed */
    if (approx) {
	return;
	}

    /* Some day we might consider having approximate calculations */
    /*????*/
    }


/******************************************************************************
********************************   CMOD_CAHVORE_3D_TO_2D   ********************
*******************************************************************************

    This function projects a 3D point into the image plane using the
    camera model parameters provided. In addition to the 2D projection,
    it outputs the 3D perpendicular distance from the camera to the
    3D point, and the partial derivative matrix of the 2D point with respect
    to the 3D point. If the parameter for the output partial matrix is
    passed as (cmod_float_t (*)[3])NULL, then it will not be calculated. */

void cmod_cahvore_3d_to_2d(
    const cmod_float_t pos3[3],	/* input 3D position */
    cmod_int_t mtype,		/* input type of model */
    cmod_float_t mparm,		/* input model parameter */
    const cmod_float_t c[3],	/* input model center vector C */
    const cmod_float_t a[3],	/* input model axis   vector A */
    const cmod_float_t h[3],	/* input model horiz. vector H */
    const cmod_float_t v[3],	/* input model vert.  vector V */
    const cmod_float_t o[3],	/* input model optical axis  O */
    const cmod_float_t r[3],	/* input model radial-distortion terms R */
    const cmod_float_t e[3],	/* input model entrance-pupil    terms E */
    cmod_bool_t approx,		/* input flag to use fast approximation */
    cmod_float_t *range,	/* output range along A (same units as C) */
    cmod_float_t pos2[2],	/* output 2D image-plane projection */
    cmod_float_t par[2][3])	/* output partial derivative of pos2 to pos3 */
{
    cmod_float_t linearity = -1;

    switch (mtype) {

	case CMOD_CAHVORE_TYPE_PERSPECTIVE:	/* perspective projection */
	    linearity = 1;
	    break;

	case CMOD_CAHVORE_TYPE_FISHEYE:		/* fisheye */
	    linearity = 0;
	    break;

	case CMOD_CAHVORE_TYPE_GENERAL:		/* parametric */
	    linearity = mparm;
	    break;

	default:
	    CMOD_ASSERT_1("cmod_cahvore_3d_to_2d", 0, mtype);
	}

    cmod_cahvore_3d_to_2d_general(pos3, linearity, c, a, h, v, o, r, e, approx,
					range, pos2, par);
    }


/******************************************************************************
********************************   CMOD_CAHVORE_3D_TO_2D_GENERAL   ************
*******************************************************************************

    This function projects a 3D point into the image plane using the
    camera model parameters provided. In addition to the 2D projection,
    it outputs the 3D perpendicular distance from the camera to the
    3D point, and the partial derivative matrix of the 2D point with respect
    to the 3D point. If the parameter for the output partial matrix is
    passed as (cmod_float_t (*)[3])NULL, then it will not be calculated. */

void cmod_cahvore_3d_to_2d_general(
    const cmod_float_t pos3[3],	/* input 3D position */
    cmod_float_t linearity,	/* input linearity parameter */
    const cmod_float_t c[3],	/* input model center vector C */
    const cmod_float_t a[3],	/* input model axis   vector A */
    const cmod_float_t h[3],	/* input model horiz. vector H */
    const cmod_float_t v[3],	/* input model vert.  vector V */
    const cmod_float_t o[3],	/* input model optical axis  O */
    const cmod_float_t r[3],	/* input model radial-distortion terms R */
    const cmod_float_t e[3],	/* input model entrance-pupil    terms E */
    cmod_bool_t approx,		/* input flag to use fast approximation */
    cmod_float_t *range,	/* output range along A (same units as C) */
    cmod_float_t pos2[2],	/* output 2D image-plane projection */
    cmod_float_t par[2][3])	/* output partial derivative of pos2 to pos3 */
{
    cmod_int_t n;
    cmod_float_t alpha;
    cmod_float_t beta;
    cmod_float_t costh;
    cmod_float_t drpdp[3][3];
    cmod_float_t dtheta;
    cmod_float_t dxhdrp[3];
    cmod_float_t dyhdrp[3];
    cmod_float_t gamma;
    cmod_float_t lambda;
    cmod_float_t lambda3[3];
    cmod_float_t m33[3][3];
    cmod_float_t n33[3][3];
    cmod_float_t p_c[3];
    cmod_float_t rp[3];
    cmod_float_t sinth;
    cmod_float_t theta;
    cmod_float_t theta2;
    cmod_float_t theta3;
    cmod_float_t theta4;
    cmod_float_t u3[3];
    cmod_float_t upsilon;
    cmod_float_t v3[3];
    cmod_float_t xh;
    cmod_float_t yh;
    cmod_float_t zeta;

    /* Check input */
    CMOD_ASSERT("cmod_cahvore_3d_to_2d_general", r     != NULL);
    CMOD_ASSERT("cmod_cahvore_3d_to_2d_general", e     != NULL);
    CMOD_ASSERT("cmod_cahvore_3d_to_2d_general", range != NULL);
    CMOD_ASSERT("cmod_cahvore_3d_to_2d_general", pos2  != NULL);

    /* In the following there is a mixture of nomenclature from several */
    /* versions of Gennery's write-ups and Litwin's software. Beware!   */

    upsilon = 0;
    costh = 0;
    sinth = 0;

    /* Basic Computations */

    /* Calculate initial terms */
    sub3(pos3, c, p_c);
    zeta = dot3(p_c, o);
    scale3(zeta, o, u3);
    sub3(p_c, u3, lambda3);
    lambda = mag3(lambda3);

    /* Calculate theta using Newton's Method */
    n = 0;
    theta = atan2(lambda, zeta);
    dtheta = 1;
    for (;;) {

	/* Make sure we don't iterate forever */
	if (++n > MAX_NEWTON) {
	    CMOD_ERROR("cahvore_3d_to_2d", "too many iterations");
	    break;
	    }

	/* Compute terms from the current value of theta */
	costh = cos(theta);
	sinth = sin(theta);
	theta2 = theta * theta;
	theta3 = theta * theta2;
	theta4 = theta * theta3;
	upsilon = zeta*costh + lambda*sinth
		- (1     - costh) * (e[0] +  e[1]*theta2 +   e[2]*theta4)
		- (theta - sinth) * (      2*e[1]*theta  + 4*e[2]*theta3);

	/* Check exit criterion from last update */
	if (fabs(dtheta) < 1e-8) {
	    break;
	    }

	/* Update theta */
	dtheta = 0;
	if (fabs(upsilon) > EPSILON) {
	    dtheta = (
		zeta*sinth - lambda*costh
		- (theta - sinth) * (e[0] + e[1]*theta2 + e[2]*theta4)
		) / upsilon;
	    theta -= dtheta;
	    }
	}

    /* Check the value of theta */
    if ((theta * fabs(linearity)) >= PI/2 - EPSILON) {
	CMOD_ERROR("cahvore_3d_to_2d", "theta out of bounds");
	return;
	}

    /* Approximations for small theta */
    if (theta < 1e-8) {

	copy3(p_c, rp);

	if ((par != NULL) && !approx) {
	    ident33(m33);
	    scale33((1+r[0]), m33, m33);
	    mult313(o, o, n33);
	    scale33(r[0], n33, n33);
	    sub33(m33, n33, drpdp);
	    }
	}

    /* Full calculations */
    else {
	cmod_float_t chi;
	cmod_float_t chi2;
	cmod_float_t chi3;
	cmod_float_t chi4;
	cmod_float_t linth;
	cmod_float_t mu;
	cmod_float_t nu;
	cmod_float_t psi;
	cmod_float_t zetap;

	linth = linearity * theta;
	if (linearity < -EPSILON) {
	    chi = sin(linth) / linearity;
	    }
	else if (linearity > EPSILON) {
	    chi = tan(linth) / linearity;
	    }
	else {
	    chi = theta;
	    }

	chi2 = chi * chi;
	chi3 = chi * chi2;
	chi4 = chi * chi3;

	CMOD_ASSERT("cmod_cahvore_3d_to_2d_general", fabs(chi) > EPSILON);
	zetap = lambda / chi;

	mu = r[0] + r[1]*chi2 + r[2]*chi4;

	scale3(zetap, o, u3);
	scale3(1+mu, lambda3, v3);
	add3(u3, v3, rp);

	if ((par != NULL) && !approx) {

	    if (linearity < -EPSILON) {
		psi = cos(linth);
		}
	    else if (linearity > EPSILON) {
		CMOD_ASSERT("cmod_cahvore_3d_to_2d_general",
				fabs(cos(linth)) > EPSILON);
		psi = 1 / cos(linth);
		psi *= psi;
		}
	    else {
		psi = 1;
		}

	    nu = 2*r[1]*chi + 4*r[2]*chi3;

	    ident33(m33);
	    scale33(1+mu, m33, m33);
	    CMOD_ASSERT("cmod_cahvore_3d_to_2d_general",
				fabs(chi*upsilon) > EPSILON);
	    scale3(zetap*psi*sinth/(chi*upsilon) - (1+mu), o, u3);
	    mult313(u3, o, n33);
	    add33(m33, n33, m33);
	    CMOD_ASSERT("cmod_cahvore_3d_to_2d_general",
				fabs(upsilon) > EPSILON);
	    scale3(nu*psi*sinth/upsilon, lambda3, u3);
	    mult313(u3, o, n33);
	    sub33(m33, n33, m33);
	    CMOD_ASSERT("cmod_cahvore_3d_to_2d_general",
				fabs(chi*lambda) > EPSILON);
	    CMOD_ASSERT("cmod_cahvore_3d_to_2d_general",
				fabs(chi2*upsilon) > EPSILON);
	    scale3(1/(chi*lambda) - psi*costh/(chi2*upsilon), o, u3);
	    mult313(u3, lambda3, n33);
	    add33(m33, n33, m33);
	    CMOD_ASSERT("cmod_cahvore_3d_to_2d_general",
				fabs(lambda*upsilon) > EPSILON);
	    scale3(nu*psi*costh/(lambda*upsilon), lambda3, u3);
	    mult313(u3, lambda3, n33);
	    add33(m33, n33, drpdp);
	    }
	}

    /* Calculate the projection */
    alpha  = dot3(rp, a);
    beta   = dot3(rp, h);
    gamma  = dot3(rp, v);
    CMOD_ASSERT("cmod_cahvore_3d_to_2d_general", fabs(alpha) > EPSILON);
    pos2[0] = xh = beta  / alpha;
    pos2[1] = yh = gamma / alpha;
    *range = alpha;

    /* Only calculate the partial derivatives upon request */
    if (par == NULL) {
	return;
	}

    /* Calculate the approximate partial derivatives */

    scale3(xh, a, u3);
    sub3(h, u3, u3);
    scale3(1/alpha, u3, (approx ? par[0] : dxhdrp));

    scale3(yh, a, u3);
    sub3(v, u3, u3);
    scale3(1/alpha, u3, (approx ? par[1] : dyhdrp));

    /* If requested, just use the approximations for speed */
    if (approx) {
	return;
	}

    /* Complete the calculations for accuracy */
    mult133(dxhdrp, drpdp, par[0]);
    mult133(dyhdrp, drpdp, par[1]);
    }


/******************************************************************************
********************************   CMOD_CAHVORE_3D_TO_2D_POINT   **************
*******************************************************************************

    This function projects the vanishing point of any 3D line onto the image
    plane. In addition it calculates the partial-derivative matrix of the 2D
    point with respect to the 3D vector of the input ray. If the parameter for
    the output partial matrix is passed as (cmod_float_t (*)[3])NULL, then it
    will not be calculated. */

void cmod_cahvore_3d_to_2d_point(
    cmod_int_t mtype,		/* input type of model */
    cmod_float_t mparm,		/* input model parameter */
    const cmod_float_t c[3],	/* input model center vector C */
    const cmod_float_t a[3],	/* input model axis   vector A */
    const cmod_float_t h[3],	/* input model horiz. vector H */
    const cmod_float_t v[3],	/* input model vert.  vector V */
    const cmod_float_t o[3],	/* input model optical axis  O */
    const cmod_float_t r[3],	/* input model radial-distortion terms R */
    const cmod_float_t e[3],	/* input model entrance-pupil    terms E */
    cmod_bool_t approx,		/* input flag to use fast approximation */
    const cmod_float_t pos3[3],	/* input 3D position of line */
    const cmod_float_t uvec3[3],/* input 3D unit vector of line */
    cmod_float_t pos2[2],	/* output 2D image-plane projection */
    cmod_float_t par[2][3])	/* output derivative matrix of pos2 to uvec3 */
{
    const cmod_float_t e0[3] = {0, 0, 0};
    cmod_float_t p[3];
    cmod_float_t range;

    /* The vanishing point's projection will be the same as that of */
    /* a point a unit distance away in the proper direction. Since  */
    /* a finite motion of the entrance pupil becomes insignificant  */
    /* when compared to the infinity of the vanishing point's 3D    */
    /* position, we can set the E terms to zero, something we must  */
    /* do in this case since we are not moving to infinity.         */

    if (dot3(a, uvec3) >= 0) {
	copy3(uvec3, p);
	}
    else {
	scale3(-1.0, uvec3, p);
	}
    add3(c, p, p);
    cmod_cahvore_3d_to_2d(p, mtype, mparm, c, a, h, v, o, r, e0, approx,
			&range, pos2, par);

    /* I believe that the partial computed there should be correct */
    }


/******************************************************************************
********************************   CMOD_CAHVORE_ALIGN_MODELS   ****************
*******************************************************************************

    This function warps a pair of almost aligned camera models into a
    perfectly aligned stereo pair of virtual camera models. The virtual
    models will have their original C vectors, but will share newly
    computed A, H, V, O, R, and E terms, as well as internal model parameters.
    Note that image warping will be necessary in order to use the new models.
    If the E terms are non-zero, then the results will include some amount
    of error from internal parallax. */

void cmod_cahvore_align_models(
    cmod_int_t xdim1,		/* input number of columns */
    cmod_int_t ydim1,		/* input number of rows */
    cmod_int_t mtype1,		/* input type of model */
    cmod_float_t mparm1,	/* input model parameter */
    const cmod_float_t c1[3],	/* input model 1 center vector C */
    const cmod_float_t a1[3],	/* input model 1 axis   vector A */
    const cmod_float_t h1[3],	/* input model 1 horiz. vector H */
    const cmod_float_t v1[3],	/* input model 1 vert.  vector V */
    const cmod_float_t o1[3],	/* input model 1 axis   vector O */
    const cmod_float_t r1[3],	/* input model 1 dist.  terms  R */
    const cmod_float_t e1[3],	/* input model 1 pupil  terms  E */
    cmod_int_t xdim2,		/* input number of columns */
    cmod_int_t ydim2,		/* input number of rows */
    cmod_int_t mtype2,		/* input type of model */
    cmod_float_t mparm2,	/* input model parameter */
    const cmod_float_t c2[3],	/* input model 2 center vector C */
    const cmod_float_t a2[3],	/* input model 2 axis   vector A */
    const cmod_float_t h2[3],	/* input model 2 horiz. vector H */
    const cmod_float_t v2[3],	/* input model 2 vert.  vector V */
    const cmod_float_t o2[3],	/* input model 2 axis   vector O */
    const cmod_float_t r2[3],	/* input model 2 dist.  terms  R */
    const cmod_float_t e2[3],	/* input model 2 pupil  terms  E */
    cmod_float_t a[3],		/* output virtual model axis   vector A */
    cmod_float_t h[3],		/* output virtual model horiz. vector H */
    cmod_float_t v[3],		/* output virtual model vert.  vector V */
    cmod_float_t o[3],		/* output virtual model axis   vector O */
    cmod_float_t r[3],		/* output virtual model dist.  terms  R */
    cmod_float_t e[3],		/* output virtual model pupil  terms  E */
    cmod_float_t *hs,		/* output horizontal scale factor */
    cmod_float_t *hc,		/* output horizontal center */
    cmod_float_t *vs,		/* output vertical scale factor */
    cmod_float_t *vc,		/* output vertical center */
    cmod_float_t *theta)	/* output angle between axes */
{
    cmod_float_t dn[3];
    cmod_float_t hc1;
    cmod_float_t hc2;
    cmod_float_t hs1;
    cmod_float_t hs2;
    cmod_float_t p2[2];
    cmod_float_t pointing[3];
    cmod_float_t p3[3];
    cmod_float_t rt[3];
    cmod_float_t theta1;
    cmod_float_t theta2;
    cmod_float_t u3[3];
    cmod_float_t vc1;
    cmod_float_t vc2;
    cmod_float_t vec1[3];
    cmod_float_t vec2[3];
    cmod_float_t vs1;
    cmod_float_t vs2;

    /* Check input */
    CMOD_ASSERT("cmod_cahvore_align_models", hs    != NULL);
    CMOD_ASSERT("cmod_cahvore_align_models", hc    != NULL);
    CMOD_ASSERT("cmod_cahvore_align_models", vs    != NULL);
    CMOD_ASSERT("cmod_cahvore_align_models", vc    != NULL);
    CMOD_ASSERT("cmod_cahvore_align_models", theta != NULL);

    /* Check that models match */
    if ((mtype1 != mtype2) || (mparm1 != mparm2)) {
	CMOD_ERROR("cmod_cahvore_align_models", "different model types");
	return;
	}

    /* Due to moving entrance pupil, O must move as little as possible */
    add3(o1, o2, o);
    scale3(0.5, o, o);

    /* Set R to zero and compute mean E terms */
    zero3(r);
    add3(e1, e2, e);
    scale3(0.5, e, e);

    /* Maybe later we could try something more sophisticated. For  */
    /* example, should the E terms be the root mean of some higher */
    /* power than 1, different for each of the 3 terms?            */

    /* Compute pointing vector */
    p2[0] = (xdim1-1)/2.0;
    p2[1] = (ydim1-1)/2.0;
    cmod_cahvore_2d_to_3d(p2, mtype1, mparm1, c1, a1, h1, v1, o1, r1, e1, FALSE,
	p3, u3, (cmod_float_t (*)[2])NULL, (cmod_float_t (*)[2])NULL);
    p2[0] = (xdim2-1)/2.0;
    p2[1] = (ydim2-1)/2.0;
    cmod_cahvore_2d_to_3d(p2, mtype2, mparm2, c2, a2, h2, v2, o2, r2, e2, FALSE,
	p3, pointing, (cmod_float_t (*)[2])NULL, (cmod_float_t (*)[2])NULL);
    add3(u3, pointing, pointing);
    unit3(pointing, pointing);

    /* Compute right and down vectors */
    sub3(c2, c1, rt);		/* right vector */
    cross3(pointing, rt, dn);	/* down vector */
    unit3(dn, dn);
    unit3(rt, rt);

    /* Compute A so that both image planes are in the same plane:   */
    /* A must be perpendicular to the baseline between the cameras. */
    /* It would be nice if we could choose A so that it was very    */
    /* close to the input values, but this would cause problems for */
    /* looking at objects very close to the cameras.                */
    cross3(rt, dn, a);
    unit3(a, a);

    /* Extract internal parameters */
    cmod_cahv_internal(c1, a1, h1, v1, (cmod_float_t (*)[12])NULL,
		&hs1, &hc1, &vs1, &vc1, &theta1, (cmod_float_t (*)[5])NULL);
    cmod_cahv_internal(c2, a2, h2, v2, (cmod_float_t (*)[12])NULL,
		&hs2, &hc2, &vs2, &vc2, &theta2, (cmod_float_t (*)[5])NULL);

    /* Compute mean of the scales */
    *hs = (hs1 + hs2) / 2.0;
    *vs = (vs1 + vs2) / 2.0;

    /* Assign idealized image centers and coordinate angles */
    *hc = (xdim2 - 1) / 2.0;
    *vc = (ydim2 - 1) / 2.0;

    /* Adjust image centers so that they project   */
    /* out along the mean input pointing direction */
    *hc -= *hs * dot3(pointing, rt);
    *vc -= *vs * dot3(pointing, dn);

    /* Is the above calculation right? Shouldn't it be done so that    */
    /* the full CAHVORE projection, not just the CAHV one, is correct? */
    /* Maybe it's close enough, since it's near the image center?      */

    /* Assign idealized coordinate angles, which was assumed */
    /* in the calculations above                             */
    *theta = -PI / 2.0;

    /* Construct H and V */
    scale3(*hs, rt, vec1);
    scale3(*hc,  a, vec2);
    add3(vec1, vec2, h);
    scale3(*vs, dn, vec1);
    scale3(*vc,  a, vec2);
    add3(vec1, vec2, v);
    }


/******************************************************************************
********************************   CMOD_CAHVORE_MOVE   ************************
*******************************************************************************

    This function relocates a camera model, based on the initial and final
    positions and orientations of a camera platform reference point, a
    point which is rigidly connected to the camera, but is otherwise
    arbitrary. */

void cmod_cahvore_move(
    const cmod_float_t p_i[3],	/* input initial pos of camera ref pt */
    const cmod_float_t q_i[4],	/* input initial orientation of camera ref pt */
    const cmod_float_t c_i[3],	/* input initial model center vector C */
    const cmod_float_t a_i[3],	/* input initial model axis   vector A */
    const cmod_float_t h_i[3],	/* input initial model horiz. vector H */
    const cmod_float_t v_i[3],	/* input initial model vert.  vector V */
    const cmod_float_t o_i[3],	/* input initial model axis   vector O */
    const cmod_float_t r_i[3],	/* input initial model dist.   terms R */
    const cmod_float_t e_i[3],	/* input initial model pupil   terms E */
    const cmod_float_t p_f[3],	/* input final pos of camera ref pt */
    const cmod_float_t q_f[4],	/* input final orientation of camera ref pt */
    cmod_float_t c_f[3],	/* output final model center vector C */
    cmod_float_t a_f[3],	/* output final model axis   vector A */
    cmod_float_t h_f[3],	/* output final model horiz. vector H */
    cmod_float_t v_f[3],	/* output final model vert.  vector V */
    cmod_float_t o_f[3],	/* output final model axis   vector O */
    cmod_float_t r_f[3],	/* output final model dist.  terms  R */
    cmod_float_t e_f[3])	/* output final model pupil  terms  E */
{
    cmod_float_t d[3];
    cmod_float_t r[3][3];
    cmod_float_t rqf[3][3];
    cmod_float_t rqi[3][3];
    cmod_float_t rqit[3][3];

    /* Calculate the rotation from the initial to the final orientation */
    rotq(q_f, rqf);
    rotq(q_i, rqi);
    trans33(rqi, rqit);
    mult333(rqf, rqit, r);

    /* Rotate and translate the C vector */
    sub3(c_i, p_i, d);		/* delta vector from P_i to C_i */
    mult331(r, d, c_f);		/* rotate delta vector */
    add3(c_f, p_f, c_f);	/* reposition C_f from P_f */

    /* Rotate the A, H, V, O vectors */
    mult331(r, a_i, a_f);
    mult331(r, h_i, h_f);
    mult331(r, v_i, v_f);
    mult331(r, o_i, o_f);

    /* Copy over the R & E "vectors" unchanged */
    copy3(r_i, r_f);
    copy3(e_i, e_f);
    }


/******************************************************************************
********************************   CMOD_CAHVORE_REFLECT   *********************
*******************************************************************************

    This function relocates a camera model, based on the image reflecting
    from a mirror defined by a plane. If the initial model is the true
    model, then the final model will be a virtual model representing how
    the camera sees the reflected world. If the initial model is the
    virtual model, then the final model will be the true model. */

void cmod_cahvore_reflect(
    const cmod_float_t c_i[3],	/* input initial model center vector C */
    const cmod_float_t a_i[3],	/* input initial model axis   vector A */
    const cmod_float_t h_i[3],	/* input initial model horiz. vector H */
    const cmod_float_t v_i[3],	/* input initial model vert.  vector V */
    const cmod_float_t o_i[3],	/* input initial model axis   vector O */
    const cmod_float_t r_i[3],	/* input initial model dist.  terms  R */
    const cmod_float_t e_i[3],	/* input initial model pupil  terms  E */
    const cmod_float_t p[3],	/* input point on the reflecting plane */
    const cmod_float_t n[3],	/* input normal to the reflecting plane */
    cmod_float_t c_f[3],	/* output final model center vector C */
    cmod_float_t a_f[3],	/* output final model axis   vector A */
    cmod_float_t h_f[3],	/* output final model horiz. vector H */
    cmod_float_t v_f[3],	/* output final model vert.  vector V */
    cmod_float_t o_f[3],	/* output final model axis   vector O */
    cmod_float_t r_f[3],	/* output final model dist.  terms  R */
    cmod_float_t e_f[3],	/* output final model pupil  terms  E */
    cmod_bool_t *parallel,	/* output if camera view & plane are parallel */
    cmod_bool_t *behind)	/* output if camera behind reflecting plane */
{
    cmod_cahvor_reflect(c_i, a_i, h_i, v_i, o_i, r_i, p, n,
		c_f, a_f, h_f, v_f, o_f, r_f, parallel, behind);
    copy3(e_i, e_f);
    }


/******************************************************************************
********************************   CMOD_CAHVORE_REFLECT_COV   *****************
*******************************************************************************

    This function reflects the covariance matrix to correspond to the
    reflected model. */

void cmod_cahvore_reflect_cov(
    cmod_float_t s_i[21][21],	/* input initial covariance */
    const cmod_float_t n[3],	/* input normal to the reflecting plane */
    cmod_float_t s_f[21][21])	/* output final covariance */
{
    cmod_float_t nu[3];
    cmod_float_t nu0;
    cmod_float_t nu1;
    cmod_float_t nu2;
    cmod_float_t r[3][3];

    /* Make sure that the normal is a unit vector */
    unit3(n, nu);
    nu0 = nu[0];
    nu1 = nu[1];
    nu2 = nu[2];

    /* Construct the transformation matrix */
    r[0][0] = -2*nu0*nu0 + 1;
    r[0][1] = -2*nu0*nu1;
    r[0][2] = -2*nu0*nu2;
    r[1][0] = -2*nu1*nu0;
    r[1][1] = -2*nu1*nu1 + 1;
    r[1][2] = -2*nu1*nu2;
    r[2][0] = -2*nu2*nu0;
    r[2][1] = -2*nu2*nu1;
    r[2][2] = -2*nu2*nu2 + 1;

    /* Transform the covariance */
    cmod_cahvore_transform_cov(s_i, r, s_f);
    }


/******************************************************************************
********************************   CMOD_CAHVORE_ROT_COV   *********************
*******************************************************************************

    This function rotates a CAHVORE model's covariance matrix to correspond to
    rotation of the model itself. Note that model translations do not affect
    the covariance matrix. */

void cmod_cahvore_rot_cov(
    cmod_float_t r_i[3][3],	/* input initial orientation of camera ref pt */
    cmod_float_t s_i[21][21],	/* input initial covariance */
    cmod_float_t r_f[3][3],	/* input final orientation of camera ref pt */
    cmod_float_t s_f[21][21])	/* output final covariance */
{
    cmod_float_t r[3][3];
    cmod_float_t r_it[3][3];

    /* Calculate the rotation, R, from the initial to the final orientation */
    trans33(r_i, r_it);
    mult333(r_f, r_it, r);
    cmod_cahvore_transform_cov(s_i, r, s_f);
    }


/******************************************************************************
********************************   CMOD_CAHVORE_ROTATE_COV   ******************
*******************************************************************************

    This function rotates a CAHVORE model's covariance matrix to correspond to
    rotation of the model itself. Note that model translations do not affect
    the covariance matrix. */

void cmod_cahvore_rotate_cov(
    const cmod_float_t q_i[4],	/* input initial orientation of camera ref pt */
    cmod_float_t s_i[21][21],	/* input initial covariance */
    const cmod_float_t q_f[4],	/* input final orientation of camera ref pt */
    cmod_float_t s_f[21][21])	/* output final covariance */
{
    cmod_float_t r_f[3][3];
    cmod_float_t r_i[3][3];

    rotq(q_f, r_f);
    rotq(q_i, r_i);
    cmod_cahvore_rot_cov(r_i, s_i, r_f, s_f);
    }


/******************************************************************************
********************************   CMOD_CAHVORE_SCALE   ***********************
*******************************************************************************

    This function scales a camera model. The scale factors should be
    understood as the same scale factors that would be applied to a 2D
    coordinate in the original model to convert it to a coordinate in
    the resulting model. Note that any precomputed internal model
    parameters will be made obsolete by this function. */

void cmod_cahvore_scale(
    cmod_float_t hscale,	/* input horizontal scale factor */
    cmod_float_t vscale,	/* input vertical   scale factor */
    const cmod_float_t h1[3],	/* input  model horiz. vector H */
    const cmod_float_t v1[3],	/* input  model vert.  vector V */
    cmod_float_t s1[21][21],	/* input  covariance matrix, or NULL */
    cmod_float_t h2[3],		/* output model horiz. vector H */
    cmod_float_t v2[3],		/* output model vert.  vector V */
    cmod_float_t s2[21][21])	/* output covariance matrix, or NULL */
{
    cmod_int_t i;
    cmod_int_t j;

    /* Check input */
    CMOD_ASSERT("cmod_cahvore_scale", h1 != NULL);
    CMOD_ASSERT("cmod_cahvore_scale", v1 != NULL);
    CMOD_ASSERT("cmod_cahvore_scale", h2 != NULL);
    CMOD_ASSERT("cmod_cahvore_scale", v2 != NULL);

    /* Scale the model */
    h2[0] = hscale * h1[0];
    h2[1] = hscale * h1[1];
    h2[2] = hscale * h1[2];
    v2[0] = vscale * v1[0];
    v2[1] = vscale * v1[1];
    v2[2] = vscale * v1[2];

    /* Optionally scale the covariance */
    if ((s1 == NULL) || (s2 == NULL)) {
	return;
	}
    if (s1 != s2) {
	for (i=0; i<21; i++) {
	    for (j=0; j<21; j++) {
		s2[i][j] = s1[i][j];
		}
	    }
	}
    for (i=0; i<21; i++) {
	for (j=6; j<9; j++) {
	    s2[i][j] *= hscale;
	    s2[j][i] *= hscale;
	    }
	for (j=9; j<12; j++) {
	    s2[i][j] *= vscale;
	    s2[j][i] *= vscale;
	    }
	}
    }


/******************************************************************************
********************************   CMOD_CAHVORE_SHIFT   ***********************
*******************************************************************************

    This function shifts a camera model. The shift values should be
    understood as the coordinates of the old model where the origin
    will fall in the new one. Note that any precomputed internal model
    parameters will be made obsolete by this function. */

void cmod_cahvore_shift(
    cmod_float_t dx,		/* input horizontal shift */
    cmod_float_t dy,		/* input vertical   shift */
    const cmod_float_t a1[3],	/* input  model axis   vector A */
    const cmod_float_t h1[3],	/* input  model horiz. vector H */
    const cmod_float_t v1[3],	/* input  model vert.  vector V */
    cmod_float_t s1[21][21],	/* input  covariance matrix, or NULL */
    cmod_float_t h2[3],		/* output model horiz. vector H */
    cmod_float_t v2[3],		/* output model vert.  vector V */
    cmod_float_t s2[21][21])	/* output covariance matrix, or NULL */
{
    cmod_int_t i;
    cmod_int_t j;

    /* Check input */
    CMOD_ASSERT("cmod_cahvore_shift", a1 != NULL);
    CMOD_ASSERT("cmod_cahvore_shift", h1 != NULL);
    CMOD_ASSERT("cmod_cahvore_shift", v1 != NULL);
    CMOD_ASSERT("cmod_cahvore_shift", h2 != NULL);
    CMOD_ASSERT("cmod_cahvore_shift", v2 != NULL);

    /* Scale the model */
    h2[0] = h1[0]  -  dx * a1[0];
    h2[1] = h1[1]  -  dx * a1[1];
    h2[2] = h1[2]  -  dx * a1[2];
    v2[0] = v1[0]  -  dy * a1[0];
    v2[1] = v1[1]  -  dy * a1[1];
    v2[2] = v1[2]  -  dy * a1[2];

    /* Optionally scale the covariance */
    if ((s1 == NULL) || (s2 == NULL)) {
	return;
	}
    if (s1 != s2) {
	for (i=0; i<21; i++) {
	    for (j=0; j<21; j++) {
		s2[i][j] = s1[i][j];
		}
	    }
	}
    for (i=0; i<21; i++) {
	s2[ 6][i]  -=  dx * s2[3][i];
	s2[ 7][i]  -=  dx * s2[4][i];
	s2[ 8][i]  -=  dx * s2[5][i];
	s2[ 9][i]  -=  dy * s2[3][i];
	s2[10][i]  -=  dy * s2[4][i];
	s2[11][i]  -=  dy * s2[5][i];
	}
    for (i=0; i<21; i++) {
	s2[i][ 6]  -=  dx * s2[i][3];
	s2[i][ 7]  -=  dx * s2[i][4];
	s2[i][ 8]  -=  dx * s2[i][5];
	s2[i][ 9]  -=  dy * s2[i][3];
	s2[i][10]  -=  dy * s2[i][4];
	s2[i][11]  -=  dy * s2[i][5];
	}
    }


/******************************************************************************
********************************   CMOD_CAHVORE_TRANSFORM_COV   ***************
*******************************************************************************

    This function transform a CAHVORE model's covariance matrix according to a
    3x3 matrix that represents the transformation to the 3D coordinates of
    the model. Note that model translations do not affect the covariance
    matrix. */

void cmod_cahvore_transform_cov(
    cmod_float_t s_i[21][21],	/* input initial covariance */
    cmod_float_t r[3][3],	/* input transform matrix of camera ref pt */
    cmod_float_t s_f[21][21])	/* output final covariance */
{
    cmod_int_t i;
    cmod_int_t j;
    cmod_int_t k;
    cmod_float_t d;
    cmod_float_t r21[21][21];
    cmod_float_t r21t[21][21];
    cmod_float_t stemp[21][21];

    /* Check input */
    CMOD_ASSERT("cmod_cahvore_transform_cov", s_i != NULL);
    CMOD_ASSERT("cmod_cahvore_transform_cov", r   != NULL);
    CMOD_ASSERT("cmod_cahvore_transform_cov", s_f != NULL);

    /* Contruct a matrix, R21, (and its transpose) to rotate the covariance
	|R      |
	| R     |
	|  R    |
	|   R   |
	|    R  |
	|     I |
	|      I|
    */
    for (i=0; i<21; i++) {
	for (j=0; j<21; j++) {
	    r21[i][j] = 0;
	    }
	}
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
    for (i=0; i<21; i++) {
	for (j=0; j<21; j++) {
	    r21t[i][j] = r21[j][i];
	    }
	}

    /* Pre-multiply by the matrix */
    for (i=0; i<21; i++) {
	for (j=0; j<21; j++) {
	    d = 0;
	    for (k=0; k<21; k++) {
		d += r21[i][k] * s_i[k][j];
		}
	    stemp[i][j] = d;
	    }
	}

    /* Post-multiply by the transpose */
    for (i=0; i<21; i++) {
	for (j=0; j<21; j++) {
	    d = 0;
	    for (k=0; k<21; k++) {
		d += stemp[i][k] * r21t[k][j];
		}
	    s_f[i][j] = d;
	    }
	}
    }


/******************************************************************************
********************************   CMOD_CAHVORE_VALIDATE   ********************
*******************************************************************************

    This function validates the components of a camera model to determine
    whether or not the model makes some kind of sense. Only generic
    sanity-checking tests are performed. */

cmod_stat_t cmod_cahvore_validate(
    cmod_int_t mtype,		/* input type of model */
    cmod_float_t mparm,		/* input model parameter */
    const cmod_float_t c[3],	/* input model center vector C */
    const cmod_float_t a[3],	/* input model axis   vector A */
    const cmod_float_t h[3],	/* input model horiz. vector H */
    const cmod_float_t v[3],	/* input model vert.  vector V */
    const cmod_float_t o[3],	/* input model optical axis unit vector O */
    const cmod_float_t r[3],	/* input model radial-distortion terms  R */
    const cmod_float_t e[3])	/* input model entrance-pupil    terms  E */
{

    /* Check radial-distortion part of the model */
    if (cmod_cahvor_validate(c, a, h, v, o, r) < 0) {
	return FAILURE;
	}

    /* Check model type */
    if ((mtype < CMOD_CAHVORE_TYPE_PERSPECTIVE) ||
        (mtype > CMOD_CAHVORE_TYPE_GENERAL)) {
	CMOD_ERROR_I("cmod_cahvore_validate", "Bad model type", mtype);
        return FAILURE;
        }

    return SUCCESS;
    }


/******************************************************************************
********************************   CMOD_CAHVORE_WARP_FROM_CAHV   **************
*******************************************************************************

    This function takes an image coordinate which resulted from a camera
    modeled by CAHV and warps it into an image coordinate modeled by
    CAHVORE. */

void cmod_cahvore_warp_from_cahv(
    const cmod_float_t c1[3],	/* input initial model center vector C */
    const cmod_float_t a1[3],	/* input initial model axis   vector A */
    const cmod_float_t h1[3],	/* input initial model horiz. vector H */
    const cmod_float_t v1[3],	/* input initial model vert.  vector V */
    const cmod_float_t pos1[2],	/* input 2D position from CAHV */
    cmod_float_t rdist,		/* input radial distance to project */
    cmod_bool_t approx,		/* input flag to use fast approximation */
    cmod_int_t mtype2,		/* input final model type */
    cmod_float_t mparm2,	/* input model parameter */
    const cmod_float_t c2[3],	/* input final model center vector C */
    const cmod_float_t a2[3],	/* input final model axis   vector A */
    const cmod_float_t h2[3],	/* input final model horiz. vector H */
    const cmod_float_t v2[3],	/* input final model vert.  vector V */
    const cmod_float_t o2[3],	/* input final model axis   vector O */
    const cmod_float_t r2[3],	/* input final model dist.  terms  R */
    const cmod_float_t e2[3],	/* input final model pupil  terms  E */
    cmod_float_t pos2[2])	/* output 2D position for CAHVORE */
{
    cmod_float_t p3[3];
    cmod_float_t range;
    cmod_float_t u3[3];

    cmod_cahv_2d_to_3d(pos1, c1, a1, h1, v1, p3, u3, (cmod_float_t (*)[2])NULL);
    scale3(rdist, u3, u3);
    add3(p3, u3, p3);
    cmod_cahvore_3d_to_2d(p3, mtype2, mparm2, c2, a2, h2, v2, o2, r2, e2,
			approx, &range, pos2, (cmod_float_t (*)[3])NULL);
    }


/******************************************************************************
********************************   CMOD_CAHVORE_WARP_FROM_PSPH   **************
*******************************************************************************

    This function takes an image coordinate that resulted from a camera
    modeled by PSPH and warps it into an image coordinate modeled by
    CAHVORE. */

void cmod_cahvore_warp_from_psph(
    const cmod_psph_t *psph1,	/* input PSPH model */
    const cmod_float_t pos1[2],	/* input 2D position from PSPH */
    cmod_float_t rdist,		/* input radial distance to project */
    cmod_bool_t approx,		/* input flag to use fast approximation */
    cmod_int_t mtype2,		/* input final model type */
    cmod_float_t mparm2,	/* input model parameter */
    const cmod_float_t c2[3],	/* input final model center vector C */
    const cmod_float_t a2[3],	/* input final model axis   vector A */
    const cmod_float_t h2[3],	/* input final model horiz. vector H */
    const cmod_float_t v2[3],	/* input final model vert.  vector V */
    const cmod_float_t o2[3],	/* input final model axis   vector O */
    const cmod_float_t r2[3],	/* input final model dist.  terms  R */
    const cmod_float_t e2[3],	/* input final model pupil  terms  E */
    cmod_float_t pos2[2])	/* output 2D position for CAHVORE */
{
    cmod_float_t p3[3];
    cmod_float_t range;
    cmod_float_t u3[3];

    cmod_psph_2d_to_3d(pos1, psph1, p3, u3, (cmod_float_t (*)[2])NULL);
    scale3(rdist, u3, u3);
    add3(p3, u3, p3);
    cmod_cahvore_3d_to_2d(p3, mtype2, mparm2, c2, a2, h2, v2, o2, r2, e2,
			approx, &range, pos2, (cmod_float_t (*)[3])NULL);
    }


/******************************************************************************
********************************   CMOD_CAHVORE_WARP_LIMIT   ******************
*******************************************************************************

    This function selects a vector representing a linear field-of-view limit
    for one side of the given model. It is a support function for use in
    determining the FOV for warping one or more models. Its purpose is to find
    a projection vector corresponding to the minimum or maximum (as specified)
    field-of-view limit for a single side of the image. The given "center"
    point is where the image's horizontal or vertical center intersects the
    side, and does not refer to the geometrical center of the side. Note that
    the output distance measure may not be linear, and is useful for comparison
    purposes only. */

void cmod_cahvore_warp_limit(
    cmod_int_t mtype,		/* input type of model */
    cmod_float_t mparm,		/* input model parameter */
    const cmod_float_t c[3],	/* input model center vector C */
    const cmod_float_t a[3],	/* input model axis   vector A */
    const cmod_float_t h[3],	/* input model horiz. vector H */
    const cmod_float_t v[3],	/* input model vert.  vector V */
    const cmod_float_t o[3],	/* input model axis   vector O */
    const cmod_float_t r[3],	/* input model dist.  terms  R */
    const cmod_float_t e[3],	/* input model pupil  terms  E */
    cmod_float_t limfov,	/* input limit field of view: < Pi rad */
    cmod_bool_t minfov,		/* input if to minimize to common FOV */
    const cmod_float_t nrm[3],	/* input image-plane outward normal unit vec */
    const cmod_float_t dir[3],	/* input image-plane principle dir unit vec */
    const cmod_float_t alt[3],	/* input image-plane alternate dir unit vec */
    cmod_float_t xcent,		/* input X coord for side's "center" point */
    cmod_float_t ycent,		/* input Y coord for side's "center" point */
    cmod_float_t xmin,		/* input X coord for side's minimum  point */
    cmod_float_t ymin,		/* input Y coord for side's minimum  point */
    cmod_float_t xmax,		/* input X coord for side's maximum  point */
    cmod_float_t ymax,		/* input Y coord for side's maximum  point */
    cmod_float_t vect[3],	/* output limit vector */
    cmod_float_t *dist)		/* output distance measure */
{
    cmod_int_t i;
    cmod_int_t npts;
    cmod_float_t axis[3];
    cmod_float_t cmin;
    cmod_float_t d;
    cmod_float_t lim[3];
    cmod_float_t ortho[3];
    cmod_float_t p3[3];
    cmod_float_t pos2[3][2];
    cmod_float_t q[4];
    cmod_float_t r33[3][3];
    cmod_float_t sa;
    cmod_float_t t[3];
    cmod_float_t u3[3];

    /* Check input */
    CMOD_ASSERT("cmod_cahvore_warp_limit", dist != NULL);

    /* Make sure the the field-of-view limit is not unreasonable */
    if ((limfov > (PI - 2*MIN_PLANE_ANGLE)) || (limfov <= 0)) {
	limfov = PI - 2*MIN_PLANE_ANGLE;
	}

    /* Prepare the vector limiting how close we can get to the image plane */
    cross3(nrm, dir, axis);
    rotq(quatva(axis, limfov/2, q), r33);
    mult331(r33, nrm, lim);
    cmin = dot3(nrm, lim);

    /* Compute a unit vector in the image plane that is orthogonal to the */
    /* primary axis and in the general direction of the alternate axis    */
    cross3(nrm, dir, ortho);
    if (dot3(ortho, alt) < 0) {
	scale3(-1.0, ortho, ortho);
	}
    unit3(ortho, ortho);

    /* Calculate sine of the angle between the coordinate axes for use below */
    sa = mag3(cross3(dir, alt, t));
    CMOD_ASSERT("cmod_cahvore_warp_limit", fabs(sa) > EPSILON);

    /* To find the extremes of the field of view, we will only check the end */
    /* points of the side and the point opposite the image's horizontal or   */
    /* vertical center. For all reasonable models we will find the extreme   */
    /* point at one of these three places. While it is possible that a model */
    /* that is very twisted (rolled) with respect to the target rows and     */
    /* columns might have an extremum somewhere else, the likelihood of      */
    /* encountering such a geometry is very slight. The only defense against */
    /* such a possibility would be to search or analyze the entire side, but */
    /* we will not worry about the need to do so in this version.            */

    /* Build the list of 2D projection vectors, including the "center" point */
    /* only if it falls within the side's limits as defined by min and max   */
    pos2[0][0] = xmin;
    pos2[0][1] = ymin;
    pos2[1][0] = xmax;
    pos2[1][1] = ymax;
    pos2[2][0] = xcent;
    pos2[2][1] = ycent;
    npts = 3;
    if ((xcent < xmin) || (xcent > xmax) || (ycent < ymin) || (ycent > ymax)) {
	npts = 2;
	}

    /* Project each of the candidate points */
    *dist = 0;
    for (i=0; i<npts; i++) {

	/* Project the 2D point into 3D */
	cmod_cahvore_2d_to_3d(pos2[i], mtype, mparm, c, a, h, v, o, r, e,
				    FALSE, p3, u3, NULL, NULL);

	/* Skew the projection over to the principle axis */
	/* scale3(dot3(u3, alt), alt, t); -- only worked for orthogonal axes */
	scale3(dot3(u3, ortho)/sa, alt, t);
	sub3(u3, t, u3);
	unit3(u3, u3);

	/* Limit the projection if it is too close or beyond the image plane */
	if (dot3(nrm, u3) < cmin) {
	    copy3(lim, u3);
	    }

	/* Is this the extreme so far? */
	d = dot3(nrm, u3);
	CMOD_ASSERT("cmod_cahvore_warp_limit", fabs(d) > EPSILON);
	d = dot3(dir, u3) / d;
	if ((i == 0) || (minfov && (d < *dist)) || (!minfov && (d > *dist))) {
	    *dist = d;
	    copy3(u3, vect);
	    }
	}
    }


/******************************************************************************
********************************   CMOD_CAHVORE_WARP_MODEL   ******************
*******************************************************************************

    This function warps a camera model so that it is purely linear. The
    parameter C will not change. The parameters O (identical to A) and
    R and E (all terms zero) will not be output. Note that image warping
    will be necessary in order to use the new models. */

void cmod_cahvore_warp_model(
    cmod_int_t xdim,		/* input number of columns */
    cmod_int_t ydim,		/* input number of rows */
    cmod_int_t mtype,		/* input type of model */
    cmod_float_t mparm,		/* input model parameter */
    const cmod_float_t c[3],	/* input model center vector C */
    const cmod_float_t a[3],	/* input model axis   vector A */
    const cmod_float_t h[3],	/* input model horiz. vector H */
    const cmod_float_t v[3],	/* input model vert.  vector V */
    const cmod_float_t o[3],	/* input model axis   vector O */
    const cmod_float_t r[3],	/* input model dist.  terms  R */
    const cmod_float_t e[3],	/* input model pupil  terms  E */
    cmod_float_t limfov,	/* input limit field of view: < Pi rad */
    cmod_bool_t minfov,		/* input if to minimize to common FOV */
    cmod_int_t xdim2,		/* input number of columns of output model */
    cmod_int_t ydim2,		/* input number of rows    of output model */
    cmod_float_t a2[3],		/* output virtual model axis   vector A */
    cmod_float_t h2[3],		/* output virtual model horiz. vector H */
    cmod_float_t v2[3],		/* output virtual model vert.  vector V */
    cmod_float_t *hs,		/* output horizontal scale factor */
    cmod_float_t *hc,		/* output horizontal center */
    cmod_float_t *vs,		/* output vertical scale factor */
    cmod_float_t *vc,		/* output vertical center */
    cmod_float_t *theta)	/* output angle between axes */
{
    cmod_float_t c2[3];
    cmod_float_t d;
    cmod_float_t hc0;
    cmod_float_t hdir[3];
    cmod_float_t ndir[3];
    cmod_float_t ppnt[3];
    cmod_float_t u[3];
    cmod_float_t vc0;
    cmod_float_t vdir[3];
    cmod_float_t x[3];
    cmod_float_t xc1;
    cmod_float_t xc2;
    cmod_float_t xneg[3];
    cmod_float_t xv1[3];
    cmod_float_t xv2[3];
    cmod_float_t y[3];
    cmod_float_t yc1;
    cmod_float_t yc2;
    cmod_float_t yneg[3];
    cmod_float_t yv1[3];
    cmod_float_t yv2[3];

    /* Extract image-plane information from the input model, for use below */
    cmod_cahv_iplane(c, a, h, v, ppnt, ndir, hdir, vdir, &hc0, &vc0);

    /* Keep the same image plane as the input model */
    copy3(hdir, x);
    copy3(vdir, y);
    copy3(ndir, u);

    /* No longer need to force the Y dimension to be orthogonal to X...
    cross3(u, x, y);
    if (dot3(y, vdir) < 0) {
	scale3(-1.0, y, y);
	}
    unit3(y, y);
    ...*/

    /* Below we will look around the image perimeter and select vectors */
    /* representing the desired extremes in each dimension.             */

    /* Choose the lower horizontal vector and image coordinate */
    xc1 = 0;
    scale3(-1.0, x, xneg);
    cmod_cahvore_warp_limit(mtype, mparm, c, a, h, v, o, r, e, limfov, minfov,
	u, xneg, y, 0, vc0, 0, 0, 0, ydim-1, xv1, &d);

    /* Choose the upper horizontal vector and image coordinate */
    xc2 = xdim2 - 1;
    cmod_cahvore_warp_limit(mtype, mparm, c, a, h, v, o, r, e, limfov, minfov,
	u, x, y, xdim-1, vc0, xdim-1, 0, xdim-1, ydim-1, xv2, &d);

    /* Choose the lower vertical vector and image coordinate */
    yc1 = 0;
    scale3(-1.0, y, yneg);
    cmod_cahvore_warp_limit(mtype, mparm, c, a, h, v, o, r, e, limfov, minfov,
	u, yneg, x, hc0, 0, 0, 0, xdim-1, 0, yv1, &d);

    /* Choose the upper vertical vector and image coordinate */
    yc2 = ydim2 - 1;
    cmod_cahvore_warp_limit(mtype, mparm, c, a, h, v, o, r, e, limfov, minfov,
	u, y, x, hc0, ydim-1, 0, ydim-1, xdim-1, ydim-1, yv2, &d);

    /* Create a linear model with the geometry determined above */
    if (cmod_cahv_create(ppnt, x, xv1, xv2, xc1, xc2, y, yv1, yv2, yc1, yc2,
			c2, a2, h2, v2) == FAILURE) {
	return;
	}

    /* Compute internal model parameters */
    cmod_cahv_internal(c2, a2, h2, v2, NULL, hs, hc, vs, vc, theta, NULL);
    }


/******************************************************************************
********************************   CMOD_CAHVORE_WARP_MODELS   *****************
*******************************************************************************

    This function warps a pair of almost aligned camera models into a
    perfectly aligned stereo pair of virtual camera models. The virtual
    models will have their original C vectors, but will share newly
    computed A, H, and V vectors, as well as internal model parameters.
    Since the output models will be linear, the parameters O (identical to A)
    and R (all terms zero) will not be output. Note that image warping will
    be necessary in order to use the new models. */

void cmod_cahvore_warp_models(
    cmod_int_t xdim1,		/* input number of columns */
    cmod_int_t ydim1,		/* input number of rows */
    cmod_int_t mtype1,		/* input type of model */
    cmod_float_t mparm1,	/* input model parameter */
    const cmod_float_t c1[3],	/* input model 1 center vector C */
    const cmod_float_t a1[3],	/* input model 1 axis   vector A */
    const cmod_float_t h1[3],	/* input model 1 horiz. vector H */
    const cmod_float_t v1[3],	/* input model 1 vert.  vector V */
    const cmod_float_t o1[3],	/* input model 1 axis   vector O */
    const cmod_float_t r1[3],	/* input model 1 dist.  terms  R */
    const cmod_float_t e1[3],	/* input model 1 pupil  terms  E */
    cmod_int_t xdim2,		/* input number of columns */
    cmod_int_t ydim2,		/* input number of rows */
    cmod_int_t mtype2,		/* input type of model */
    cmod_float_t mparm2,	/* input model parameter */
    const cmod_float_t c2[3],	/* input model 2 center vector C */
    const cmod_float_t a2[3],	/* input model 2 axis   vector A */
    const cmod_float_t h2[3],	/* input model 2 horiz. vector H */
    const cmod_float_t v2[3],	/* input model 2 vert.  vector V */
    const cmod_float_t o2[3],	/* input model 2 axis   vector O */
    const cmod_float_t r2[3],	/* input model 2 dist.  terms  R */
    const cmod_float_t e2[3],	/* input model 2 pupil  terms  E */
    cmod_float_t limfov,	/* input limit field of view: < Pi rad */
    cmod_bool_t minfov,		/* input if to minimize to common FOV */
    cmod_int_t xdim,		/* input number of columns of output model */
    cmod_int_t ydim,		/* input number of rows    of output model */
    cmod_float_t a[3],		/* output virtual model axis   vector A */
    cmod_float_t h[3],		/* output virtual model horiz. vector H */
    cmod_float_t v[3],		/* output virtual model vert.  vector V */
    cmod_float_t *hs,		/* output horizontal scale factor */
    cmod_float_t *hc,		/* output horizontal center */
    cmod_float_t *vs,		/* output vertical scale factor */
    cmod_float_t *vc,		/* output vertical center */
    cmod_float_t *theta)	/* output angle between axes */
{
    cmod_float_t c[3];
    cmod_float_t d1;
    cmod_float_t d2;
    cmod_float_t hc1;
    cmod_float_t hc2;
    cmod_float_t hdir1[3];
    cmod_float_t hdir2[3];
    cmod_float_t ndir1[3];
    cmod_float_t ndir2[3];
    cmod_float_t ppnt1[3];
    cmod_float_t ppnt2[3];
    cmod_float_t t[3];
    cmod_float_t u[3];
    cmod_float_t vc1;
    cmod_float_t vc2;
    cmod_float_t vdir1[3];
    cmod_float_t vdir2[3];
    cmod_float_t x[3];
    cmod_float_t xc1;
    cmod_float_t xc2;
    cmod_float_t xneg[3];
    cmod_float_t xv1[3];
    cmod_float_t xv2[3];
    cmod_float_t y[3];
    cmod_float_t yc1;
    cmod_float_t yc2;
    cmod_float_t yneg[3];
    cmod_float_t yv1[3];
    cmod_float_t yv2[3];

    /* Extract image-plane information from both input models, for use below */
    cmod_cahv_iplane(c1, a1, h1, v1, ppnt1, ndir1, hdir1, vdir1, &hc1, &vc1);
    cmod_cahv_iplane(c2, a2, h2, v2, ppnt2, ndir2, hdir2, vdir2, &hc2, &vc2);

    /* Below we are assuming the input models form a left/right stereo pair. */
    /* This means we want row alignment. If there is demand in the future,   */
    /* we could make this function intelligent enough to decide if either    */
    /* row or column alignment is desired, based on the relative positions   */
    /* of the cameras. Or maybe a separate function would be better.         */

    /* The rows must be parallel to the difference in the camera positions */
    /* in order for the output models to be epipolar-aligned               */
    sub3(ppnt2, ppnt1, x);
    if (dot3(x, hdir1) < 0) {
	scale3(-1.0, x, x);
	}
    unit3(x, x);

    /* We will choose the direction for the columns to be as close to the */
    /* mean of the two input models as possible in order to minimize the  */
    /* warping distortion, but we will make it exactly orthogonal to the  */
    /* rows since that is what most users will require.                   */
    add3(vdir1, vdir2, t);
    cross3(x, t, u);
    cross3(u, x, y);
    unit3(y, y);

    /* Calculate the outward-facing unit normal to the new image plane */
    cross3(x, y, u);
    if (dot3(u, ndir1) < 0) {
	scale3(-1.0, u, u);
	}
    unit3(u, u);

    /* Below we will look around the image perimeters and select vectors */
    /* representing the desired extremes in each dimension.              */

    /* Choose the lower horizontal vector and image coordinate */
    xc1 = 0;
    scale3(-1.0, x, xneg);
    cmod_cahvore_warp_limit(mtype1, mparm1, c1, a1, h1, v1, o1, r1, e1,
	limfov, minfov, u, xneg, y, 0, vc1, 0, 0, 0, ydim1-1,
	xv1, &d1);
    cmod_cahvore_warp_limit(mtype2, mparm2, c2, a2, h2, v2, o2, r2, e2,
	limfov, minfov, u, xneg, y, 0, vc2, 0, 0, 0, ydim2-1,
	t, &d2);
    if ((minfov && (d2 < d1)) || (!minfov && (d2 > d1))) {
	copy3(t, xv1);
	}

    /* Choose the upper horizontal vector and image coordinate */
    xc2 = xdim - 1;
    scale3(-1.0, x, xneg);
    cmod_cahvore_warp_limit(mtype1, mparm1, c1, a1, h1, v1, o1, r1, e1,
	limfov, minfov, u, x, y, xdim1-1, vc1, xdim1-1, 0, xdim1-1, ydim1-1,
	xv2, &d1);
    cmod_cahvore_warp_limit(mtype2, mparm2, c2, a2, h2, v2, o2, r2, e2,
	limfov, minfov, u, x, y, xdim2-1, vc2, xdim2-1, 0, xdim2-1, ydim2-1,
	t, &d2);
    if ((minfov && (d2 < d1)) || (!minfov && (d2 > d1))) {
	copy3(t, xv2);
	}

    /* Choose the lower vertical vector and image coordinate */
    yc1 = 0;
    scale3(-1.0, y, yneg);
    cmod_cahvore_warp_limit(mtype1, mparm1, c1, a1, h1, v1, o1, r1, e1,
	limfov, minfov, u, yneg, x, hc1, 0, 0, 0, xdim1-1, 0,
	yv1, &d1);
    cmod_cahvore_warp_limit(mtype2, mparm2, c2, a2, h2, v2, o2, r2, e2,
	limfov, minfov, u, yneg, x, hc2, 0, 0, 0, xdim2-1, 0,
	t, &d2);
    if ((minfov && (d2 < d1)) || (!minfov && (d2 > d1))) {
	copy3(t, yv1);
	}

    /* Choose the upper vertical vector and image coordinate */
    yc2 = ydim - 1;
    cmod_cahvore_warp_limit(mtype1, mparm1, c1, a1, h1, v1, o1, r1, e1,
	limfov, minfov, u, y, x, hc1, ydim1-1, 0, ydim1-1, xdim1-1, ydim1-1,
	yv2, &d1);
    cmod_cahvore_warp_limit(mtype2, mparm2, c2, a2, h2, v2, o2, r2, e2,
	limfov, minfov, u, y, x, hc2, ydim2-1, 0, ydim2-1, xdim2-1, ydim2-1,
	t, &d2);
    if ((minfov && (d2 < d1)) || (!minfov && (d2 > d1))) {
	copy3(t, yv2);
	}

    /* Create a linear model with the geometry determined above */
    if (cmod_cahv_create(ppnt1, x, xv1, xv2, xc1, xc2, y, yv1, yv2, yc1, yc2,
			c, a, h, v) == FAILURE) {
	return;
	}

    /* Compute internal model parameters */
    cmod_cahv_internal(c, a, h, v, NULL, hs, hc, vs, vc, theta, NULL);
    }


/******************************************************************************
********************************   CMOD_CAHVORE_WARP_MODELS2   ****************
*******************************************************************************

    This function is a newer version of cmod_cahvore_warp_models(). It uses
    a row/normal-based way of computing things rather than a row/column-based
    way. The newer version is less sensitive to the left/right order in which
    the models are presented to the routine. */

void cmod_cahvore_warp_models2(
    cmod_int_t xdim1,		/* input number of columns */
    cmod_int_t ydim1,		/* input number of rows */
    cmod_int_t mtype1,		/* input type of model */
    cmod_float_t mparm1,	/* input model parameter */
    const cmod_float_t c1[3],	/* input model 1 center vector C */
    const cmod_float_t a1[3],	/* input model 1 axis   vector A */
    const cmod_float_t h1[3],	/* input model 1 horiz. vector H */
    const cmod_float_t v1[3],	/* input model 1 vert.  vector V */
    const cmod_float_t o1[3],	/* input model 1 axis   vector O */
    const cmod_float_t r1[3],	/* input model 1 dist.  terms  R */
    const cmod_float_t e1[3],	/* input model 1 pupil  terms  E */
    cmod_int_t xdim2,		/* input number of columns */
    cmod_int_t ydim2,		/* input number of rows */
    cmod_int_t mtype2,		/* input type of model */
    cmod_float_t mparm2,	/* input model parameter */
    const cmod_float_t c2[3],	/* input model 2 center vector C */
    const cmod_float_t a2[3],	/* input model 2 axis   vector A */
    const cmod_float_t h2[3],	/* input model 2 horiz. vector H */
    const cmod_float_t v2[3],	/* input model 2 vert.  vector V */
    const cmod_float_t o2[3],	/* input model 2 axis   vector O */
    const cmod_float_t r2[3],	/* input model 2 dist.  terms  R */
    const cmod_float_t e2[3],	/* input model 2 pupil  terms  E */
    cmod_float_t limfov,	/* input limit field of view: < Pi rad */
    cmod_bool_t minfov,		/* input if to minimize to common FOV */
    cmod_int_t xdim,		/* input number of columns of output model */
    cmod_int_t ydim,		/* input number of rows    of output model */
    cmod_float_t a[3],		/* output virtual model axis   vector A */
    cmod_float_t h[3],		/* output virtual model horiz. vector H */
    cmod_float_t v[3],		/* output virtual model vert.  vector V */
    cmod_float_t *hs,		/* output horizontal scale factor */
    cmod_float_t *hc,		/* output horizontal center */
    cmod_float_t *vs,		/* output vertical scale factor */
    cmod_float_t *vc,		/* output vertical center */
    cmod_float_t *theta)	/* output angle between axes */
{
    cmod_float_t c[3];
    cmod_float_t d1;
    cmod_float_t d2;
    cmod_float_t hc1;
    cmod_float_t hc2;
    cmod_float_t hdir1[3];
    cmod_float_t hdir2[3];
    cmod_float_t ndir1[3];
    cmod_float_t ndir2[3];
    cmod_float_t ppnt1[3];
    cmod_float_t ppnt2[3];
    cmod_float_t t[3];
    cmod_float_t u[3];
    cmod_float_t vc1;
    cmod_float_t vc2;
    cmod_float_t vdir1[3];
    cmod_float_t vdir2[3];
    cmod_float_t x[3];
    cmod_float_t xc1;
    cmod_float_t xc2;
    cmod_float_t xneg[3];
    cmod_float_t xv1[3];
    cmod_float_t xv2[3];
    cmod_float_t y[3];
    cmod_float_t yc1;
    cmod_float_t yc2;
    cmod_float_t yneg[3];
    cmod_float_t yv1[3];
    cmod_float_t yv2[3];

    /* Extract image-plane information from both input models, for use below */
    cmod_cahv_iplane(c1, a1, h1, v1, ppnt1, ndir1, hdir1, vdir1, &hc1, &vc1);
    cmod_cahv_iplane(c2, a2, h2, v2, ppnt2, ndir2, hdir2, vdir2, &hc2, &vc2);

    /* Below we are assuming the input models form a left/right stereo pair. */
    /* This means we want row alignment. If there is demand in the future,   */
    /* we could make this function intelligent enough to decide if either    */
    /* row or column alignment is desired, based on the relative positions   */
    /* of the cameras. Or maybe a separate function would be better. For now */
    /* we will make some effort to be able to handle images with any spatial */
    /* relationship to each other, but the results might be unexpected if    */
    /* they are not left/right.                                              */

    /* The rows must be parallel to the difference in the camera positions */
    /* in order for the output models to be epipolar-aligned               */
    sub3(ppnt2, ppnt1, x);
    add3(hdir1, hdir2, t);
    if (dot3(x, t) < 0) {
	scale3(-1.0, x, x);
	}
    unit3(x, x);

    /* Calculate the output normal as close to the inputs as possible while */
    /* still being orthogonal to the rows                                   */
    add3(ndir1, ndir2, t);
    cross3(x, t, y);
    cross3(y, x, u);
    unit3(u, u);

    /* We will choose the direction of the columns to be orthogonal to the  */
    /* rows since that is what most users will desire, and orthogonal to    */
    /* the normal since that is required by definition of the normal.       */
    cross3(x, u, y);
    add3(vdir1, vdir2, t);
    if (dot3(y, t) < 0) {
	scale3(-1.0, y, y);
	}
    unit3(y, y);

    /* Below we will look around the image perimeters and select vectors */
    /* representing the desired extremes in each dimension.              */

    /* Choose the lower horizontal vector and image coordinate */
    xc1 = 0;
    scale3(-1.0, x, xneg);
    cmod_cahvore_warp_limit(mtype1, mparm1, c1, a1, h1, v1, o1, r1, e1,
	limfov, minfov, u, xneg, y, 0, vc1, 0, 0, 0, ydim1-1,
	xv1, &d1);
    cmod_cahvore_warp_limit(mtype2, mparm2, c2, a2, h2, v2, o2, r2, e2,
	limfov, minfov, u, xneg, y, 0, vc2, 0, 0, 0, ydim2-1,
	t, &d2);
    if ((minfov && (d2 < d1)) || (!minfov && (d2 > d1))) {
	copy3(t, xv1);
	}

    /* Choose the upper horizontal vector and image coordinate */
    xc2 = xdim - 1;
    cmod_cahvore_warp_limit(mtype1, mparm1, c1, a1, h1, v1, o1, r1, e1,
	limfov, minfov, u, x, y, xdim1-1, vc1, xdim1-1, 0, xdim1-1, ydim1-1,
	xv2, &d1);
    cmod_cahvore_warp_limit(mtype2, mparm2, c2, a2, h2, v2, o2, r2, e2,
	limfov, minfov, u, x, y, xdim2-1, vc2, xdim2-1, 0, xdim2-1, ydim2-1,
	t, &d2);
    if ((minfov && (d2 < d1)) || (!minfov && (d2 > d1))) {
	copy3(t, xv2);
	}

    /* Choose the lower vertical vector and image coordinate */
    yc1 = 0;
    scale3(-1.0, y, yneg);
    cmod_cahvore_warp_limit(mtype1, mparm1, c1, a1, h1, v1, o1, r1, e1,
	limfov, minfov, u, yneg, x, hc1, 0, 0, 0, xdim1-1, 0,
	yv1, &d1);
    cmod_cahvore_warp_limit(mtype2, mparm2, c2, a2, h2, v2, o2, r2, e2,
	limfov, minfov, u, yneg, x, hc2, 0, 0, 0, xdim2-1, 0,
	t, &d2);
    if ((minfov && (d2 < d1)) || (!minfov && (d2 > d1))) {
	copy3(t, yv1);
	}

    /* Choose the upper vertical vector and image coordinate */
    yc2 = ydim - 1;
    cmod_cahvore_warp_limit(mtype1, mparm1, c1, a1, h1, v1, o1, r1, e1,
	limfov, minfov, u, y, x, hc1, ydim1-1, 0, ydim1-1, xdim1-1, ydim1-1,
	yv2, &d1);
    cmod_cahvore_warp_limit(mtype2, mparm2, c2, a2, h2, v2, o2, r2, e2,
	limfov, minfov, u, y, x, hc2, ydim2-1, 0, ydim2-1, xdim2-1, ydim2-1,
	t, &d2);
    if ((minfov && (d2 < d1)) || (!minfov && (d2 > d1))) {
	copy3(t, yv2);
	}

    /* Create a linear model with the geometry determined above */
    if (cmod_cahv_create(ppnt1, x, xv1, xv2, xc1, xc2, y, yv1, yv2, yc1, yc2,
			c, a, h, v) == FAILURE) {
	return;
	}

    /* Compute internal model parameters */
    cmod_cahv_internal(c, a, h, v, NULL, hs, hc, vs, vc, theta, NULL);
    }


/******************************************************************************
********************************   CMOD_CAHVORE_WARP_MODELS3   ****************
*******************************************************************************

    This function is similar to cmod_cahvore_warp_models2(), but it outputs
    a target pair of PSPH models. */

void cmod_cahvore_warp_models3(
    cmod_int_t xdim1,		/* input number of columns */
    cmod_int_t ydim1,		/* input number of rows */
    cmod_int_t mtype1,		/* input type of model */
    cmod_float_t mparm1,	/* input model parameter */
    const cmod_float_t c1[3],	/* input model 1 center vector C */
    const cmod_float_t a1[3],	/* input model 1 axis   vector A */
    const cmod_float_t h1[3],	/* input model 1 horiz. vector H */
    const cmod_float_t v1[3],	/* input model 1 vert.  vector V */
    const cmod_float_t o1[3],	/* input model 1 axis   vector O */
    const cmod_float_t r1[3],	/* input model 1 dist.  terms  R */
    const cmod_float_t e1[3],	/* input model 1 pupil  terms  E */
    cmod_int_t xdim2,		/* input number of columns */
    cmod_int_t ydim2,		/* input number of rows */
    cmod_int_t mtype2,		/* input type of model */
    cmod_float_t mparm2,	/* input model parameter */
    const cmod_float_t c2[3],	/* input model 2 center vector C */
    const cmod_float_t a2[3],	/* input model 2 axis   vector A */
    const cmod_float_t h2[3],	/* input model 2 horiz. vector H */
    const cmod_float_t v2[3],	/* input model 2 vert.  vector V */
    const cmod_float_t o2[3],	/* input model 2 axis   vector O */
    const cmod_float_t r2[3],	/* input model 2 dist.  terms  R */
    const cmod_float_t e2[3],	/* input model 2 pupil  terms  E */
    cmod_float_t limfov,	/* input limit field of view: < Pi rad */
    cmod_bool_t minfov,		/* input if to minimize to common FOV */
    cmod_bool_t vergence,	/* input if to converge or diverge */
    cmod_int_t xdim,		/* input number of columns of output model */
    cmod_int_t ydim,		/* input number of rows    of output model */
    cmod_psph_t *psph1,		/* output PSPH model 1 */
    cmod_psph_t *psph2)		/* output PSPH model 2 */
{
    cmod_float_t d1;
    cmod_float_t d2;
    cmod_float_t hc1;
    cmod_float_t hc2;
    cmod_float_t hdir1[3];
    cmod_float_t hdir2[3];
    cmod_float_t ndir1[3];
    cmod_float_t ndir2[3];
    cmod_float_t ppnt1[3];
    cmod_float_t ppnt2[3];
    cmod_float_t t[3];
    cmod_float_t u[3];
    cmod_float_t vc1;
    cmod_float_t vc2;
    cmod_float_t vdir1[3];
    cmod_float_t vdir2[3];
    cmod_float_t x[3];
    cmod_float_t xc1;
    cmod_float_t xc2;
    cmod_float_t xneg[3];
    cmod_float_t xv1a[3];
    cmod_float_t xv1b[3];
    cmod_float_t xv2a[3];
    cmod_float_t xv2b[3];
    cmod_float_t y[3];
    cmod_float_t yc1;
    cmod_float_t yc2;
    cmod_float_t yneg[3];
    cmod_float_t yv1[3];
    cmod_float_t yv2[3];

    /* Extract image-plane information from both input models, for use below */
    cmod_cahv_iplane(c1, a1, h1, v1, ppnt1, ndir1, hdir1, vdir1, &hc1, &vc1);
    cmod_cahv_iplane(c2, a2, h2, v2, ppnt2, ndir2, hdir2, vdir2, &hc2, &vc2);

    /* Below we are assuming the input models form a left/right stereo pair. */
    /* This means we want row alignment. If there is demand in the future,   */
    /* we could make this function intelligent enough to decide if either    */
    /* row or column alignment is desired, based on the relative positions   */
    /* of the cameras. Or maybe a separate function would be better. For now */
    /* we will make some effort to be able to handle images with any spatial */
    /* relationship to each other, but the results might be unexpected if    */
    /* they are not left/right.                                              */

    /* The rows must be parallel to the difference in the camera positions */
    /* in order for the output models to be epipolar-aligned               */
    sub3(ppnt2, ppnt1, x);
    add3(hdir1, hdir2, t);
    if (dot3(x, t) < 0) {
	scale3(-1.0, x, x);
	}
    unit3(x, x);

    /* Calculate the output normal as close to the inputs as possible while */
    /* still being orthogonal to the rows                                   */
    add3(ndir1, ndir2, t);
    cross3(x, t, y);
    cross3(y, x, u);
    unit3(u, u);

    /* We will choose the direction of the columns to be orthogonal to the  */
    /* rows since that is what most users will desire, and orthogonal to    */
    /* the normal since that is required by definition of the normal.       */
    cross3(x, u, y);
    add3(vdir1, vdir2, t);
    if (dot3(y, t) < 0) {
	scale3(-1.0, y, y);
	}
    unit3(y, y);

    /* Below we will look around the image perimeters and select vectors */
    /* representing the desired extremes in each dimension.              */

    /* Choose the lower horizontal vector and image coordinate */
    xc1 = 0;
    scale3(-1.0, x, xneg);
    cmod_cahvore_warp_limit(mtype1, mparm1, c1, a1, h1, v1, o1, r1, e1,
	limfov, minfov, u, xneg, y, 0, vc1, 0, 0, 0, ydim1-1,
	xv1a, &d1);
    cmod_cahvore_warp_limit(mtype2, mparm2, c2, a2, h2, v2, o2, r2, e2,
	limfov, minfov, u, xneg, y, 0, vc2, 0, 0, 0, ydim2-1,
	xv1b, &d2);
    if (!vergence) {
	if ((minfov && (d2 < d1)) || (!minfov && (d2 > d1))) {
	    copy3(xv1b, xv1a);
	    }
	else {
	    copy3(xv1a, xv1b);
	    }
	}

    /* Choose the upper horizontal vector and image coordinate */
    xc2 = xdim - 1;
    cmod_cahvore_warp_limit(mtype1, mparm1, c1, a1, h1, v1, o1, r1, e1,
	limfov, minfov, u, x, y, xdim1-1, vc1, xdim1-1, 0, xdim1-1, ydim1-1,
	xv2a, &d1);
    cmod_cahvore_warp_limit(mtype2, mparm2, c2, a2, h2, v2, o2, r2, e2,
	limfov, minfov, u, x, y, xdim2-1, vc2, xdim2-1, 0, xdim2-1, ydim2-1,
	xv2b, &d2);
    if (!vergence) {
	if ((minfov && (d2 < d1)) || (!minfov && (d2 > d1))) {
	    copy3(xv2b, xv2a);
	    }
	else {
	    copy3(xv2a, xv2b);
	    }
	}

    /* Choose the lower vertical vector and image coordinate */
    yc1 = 0;
    scale3(-1.0, y, yneg);
    cmod_cahvore_warp_limit(mtype1, mparm1, c1, a1, h1, v1, o1, r1, e1,
	limfov, minfov, u, yneg, x, hc1, 0, 0, 0, xdim1-1, 0,
	yv1, &d1);
    cmod_cahvore_warp_limit(mtype2, mparm2, c2, a2, h2, v2, o2, r2, e2,
	limfov, minfov, u, yneg, x, hc2, 0, 0, 0, xdim2-1, 0,
	t, &d2);
    if ((minfov && (d2 < d1)) || (!minfov && (d2 > d1))) {
	copy3(t, yv1);
	}

    /* Choose the upper vertical vector and image coordinate */
    yc2 = ydim - 1;
    cmod_cahvore_warp_limit(mtype1, mparm1, c1, a1, h1, v1, o1, r1, e1,
	limfov, minfov, u, y, x, hc1, ydim1-1, 0, ydim1-1, xdim1-1, ydim1-1,
	yv2, &d1);
    cmod_cahvore_warp_limit(mtype2, mparm2, c2, a2, h2, v2, o2, r2, e2,
	limfov, minfov, u, y, x, hc2, ydim2-1, 0, ydim2-1, xdim2-1, ydim2-1,
	t, &d2);
    if ((minfov && (d2 < d1)) || (!minfov && (d2 > d1))) {
	copy3(t, yv2);
	}

    /* Create a planospheric model with the geometry determined above */
    if ((cmod_psph_create(ppnt1, y, xv1a, xv2a, xc1, xc2,
			xneg, yv1, yv2, yc1, yc2, psph1) == FAILURE) ||
	(cmod_psph_create(ppnt2, y, xv1b, xv2b, xc1, xc2,
			xneg, yv1, yv2, yc1, yc2, psph2) == FAILURE)) {
	return;
	}

    /* The horizontal fields of view won't necessarily be identical above */
    /* if vergence is requested, but we need them to be; make them so     */
    if (( minfov && (psph1->sx > psph2->sx)) ||
	(!minfov && (psph1->sx < psph2->sx))) {
	psph1->sx = psph2->sx;
	}
    else {
	psph2->sx = psph1->sx;
	}
    }


/******************************************************************************
********************************   CMOD_CAHVORE_WARP_TO_CAHV   ****************
*******************************************************************************

    This function takes an image coordinate which resulted from a camera
    modeled by CAHVORE and warps it into an image coordinate modeled by
    CAHV. */

void cmod_cahvore_warp_to_cahv(
    cmod_int_t mtype,		/* input type of model */
    cmod_float_t mparm,		/* input model parameter */
    const cmod_float_t c1[3],	/* input initial model center vector C */
    const cmod_float_t a1[3],	/* input initial model axis   vector A */
    const cmod_float_t h1[3],	/* input initial model horiz. vector H */
    const cmod_float_t v1[3],	/* input initial model vert.  vector V */
    const cmod_float_t o1[3],	/* input initial model axis   vector O */
    const cmod_float_t r1[3],	/* input initial model dist.  terms  R */
    const cmod_float_t e1[3],	/* input initial model pupil  terms  E */
    const cmod_float_t pos1[2],	/* input 2D position from CAHVORE */
    cmod_float_t rdist,		/* input radial distance to project */
    cmod_bool_t approx,		/* input flag to use fast approximation */
    const cmod_float_t c2[3],	/* input final model center vector C */
    const cmod_float_t a2[3],	/* input final model axis   vector A */
    const cmod_float_t h2[3],	/* input final model horiz. vector H */
    const cmod_float_t v2[3],	/* input final model vert.  vector V */
    cmod_float_t pos2[2])	/* output 2D position for CAHV */
{
    cmod_float_t p3[3];
    cmod_float_t range;
    cmod_float_t u3[3];

    cmod_cahvore_2d_to_3d(pos1, mtype, mparm, c1, a1, h1, v1, o1, r1, e1,
	approx, p3, u3, (cmod_float_t (*)[2])NULL, (cmod_float_t (*)[2])NULL);
    scale3(rdist, u3, u3);
    add3(p3, u3, p3);
    cmod_cahv_3d_to_2d(p3, c2, a2, h2, v2, &range, pos2,
	(cmod_float_t (*)[3])NULL);
    }


/******************************************************************************
********************************   CMOD_CAHVORE_WARP_TO_CAHVORE   *************
*******************************************************************************

    This function takes an image coordinate which resulted from a camera
    modeled by CAHVORE and warps it into an image coordinate modeled by
    a different CAHVORE. */

void cmod_cahvore_warp_to_cahvore(
    cmod_int_t mtype,		/* input type of model */
    cmod_float_t mparm,		/* input model parameter */
    const cmod_float_t c1[3],	/* input initial model center vector C */
    const cmod_float_t a1[3],	/* input initial model axis   vector A */
    const cmod_float_t h1[3],	/* input initial model horiz. vector H */
    const cmod_float_t v1[3],	/* input initial model vert.  vector V */
    const cmod_float_t o1[3],	/* input initial model axis   vector O */
    const cmod_float_t r1[3],	/* input initial model dist   terms  R */
    const cmod_float_t e1[3],	/* input initial model pupil  terms  E */
    const cmod_float_t pos1[2],	/* input 2D position from CAHVORE */
    cmod_float_t rdist,		/* input radial distance to project */
    cmod_bool_t approx,		/* input flag to use fast approximation */
    const cmod_float_t c2[3],	/* input final model center vector C */
    const cmod_float_t a2[3],	/* input final model axis   vector A */
    const cmod_float_t h2[3],	/* input final model horiz. vector H */
    const cmod_float_t v2[3],	/* input final model vert.  vector V */
    const cmod_float_t o2[3],	/* input final model axis   vector O */
    const cmod_float_t r2[3],	/* input final model dist.  terms  R */
    const cmod_float_t e2[3],	/* input final model pupil  terms  E */
    cmod_float_t pos2[2])	/* output 2D position for CAHV */
{
    cmod_float_t p3[3];
    cmod_float_t range;
    cmod_float_t u3[3];

    cmod_cahvore_2d_to_3d(pos1, mtype, mparm, c1, a1, h1, v1, o1, r1, e1,
	approx, p3, u3, (cmod_float_t (*)[2])NULL, (cmod_float_t (*)[2])NULL);
    scale3(rdist, u3, u3);
    add3(p3, u3, p3);
    cmod_cahvore_3d_to_2d(p3, mtype, mparm, c2, a2, h2, v2, o2, r2, e2, approx,
	&range, pos2, (cmod_float_t (*)[3])NULL);
    }


/******************************************************************************
********************************   CMOD_CAHVORE_WARP_TO_PSPH   ****************
*******************************************************************************

    This function takes an image coordinate which resulted from a camera
    modeled by CAHVORE and warps it into an image coordinate modeled by
    PSPH. */

void cmod_cahvore_warp_to_psph(
    cmod_int_t mtype,		/* input type of model */
    cmod_float_t mparm,		/* input model parameter */
    const cmod_float_t c1[3],	/* input initial model center vector C */
    const cmod_float_t a1[3],	/* input initial model axis   vector A */
    const cmod_float_t h1[3],	/* input initial model horiz. vector H */
    const cmod_float_t v1[3],	/* input initial model vert.  vector V */
    const cmod_float_t o1[3],	/* input initial model axis   vector O */
    const cmod_float_t r1[3],	/* input initial model dist.  terms  R */
    const cmod_float_t e1[3],	/* input initial model pupil  terms  E */
    const cmod_float_t pos1[2],	/* input 2D position from CAHVORE */
    cmod_float_t rdist,		/* input radial distance to project */
    cmod_bool_t approx,		/* input flag to use fast approximation */
    const cmod_psph_t *psph2,	/* input final PSPH model */
    cmod_float_t pos2[2])	/* output 2D position for PSPH */
{
    cmod_float_t p3[3];
    cmod_float_t u3[3];

    cmod_cahvore_2d_to_3d(pos1, mtype, mparm, c1, a1, h1, v1, o1, r1, e1,
	approx, p3, u3, (cmod_float_t (*)[2])NULL, (cmod_float_t (*)[2])NULL);
    scale3(rdist, u3, u3);
    add3(p3, u3, p3);
    cmod_psph_3d_to_2d(p3, psph2, pos2, (cmod_float_t (*)[3])NULL);
    }
