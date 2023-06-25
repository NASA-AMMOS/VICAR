/******************************************************************************
*                                                                             *
*                            C M O D _ C A H V O R                            *
*                                                                             *
*                                       Todd Litwin                           *
*                                       Written:  3 Aug 1993                  *
*                                       Updated: 18 Sep 2018                  *
*                                                                             *
*                                       Copyright (C) 1993, 1994, 1995, 1996, *
*                                                     1997, 1998, 1999, 2000, *
*                                                     2001, 2002, 2003, 2005, *
*                                                     2007, 2009, 2010, 2011, *
*                                                     2012, 2013, 2015, 2018  *
*                                       California Institute of Technology    *
*                                       All Rights Reserved                   *
*                                                                             *
*******************************************************************************


	This file contains functions for using the camera model known
	locally as CAHVOR. This model is an extension by Donald Gennery
	into radial distortion of the linear model by Yakimovsky &
	Cunningham, known locally as CAHV. */


#include <math.h>
#include <mat3.h>

#include "cmod_cahv.h"
#include "cmod_cahvor.h"
#include "cmod_error.h"

enum {
    SUCCESS =  0,
    FAILURE = -1
    };

enum {
    MAXITER = 20	/* maximum number of iterations allowed */
    };

#ifndef EPSILON
#define EPSILON (1e-15)
#endif

#define	PI (3.14159265358979323846)

#define CONV   (1.0e-8)	/* covergence tolerance */

#define MIN_PLANE_ANGLE (15.0 * PI / 180.0)

void cmod_cahvor_warp_limit(const cmod_float_t c[3], const cmod_float_t a[3],
    const cmod_float_t h[3], const cmod_float_t v[3], const cmod_float_t o[3],
    const cmod_float_t r[3], cmod_bool_t minfov, const cmod_float_t nrm[3],
    const cmod_float_t dir[3], const cmod_float_t alt[3], cmod_float_t xcent,
    cmod_float_t ycent, cmod_float_t xmin, cmod_float_t ymin,
    cmod_float_t xmax, cmod_float_t ymax, cmod_float_t vect[3],
    cmod_float_t *dist);


/******************************************************************************
********************************   CMOD_CAHVOR_2D_TO_3D   *********************
*******************************************************************************

    This function projects a 2D image point out into 3D using the
    camera model parameters provided. In addition to the 3D projection,
    it outputs the partial-derivative matrix of the unit vector of the
    projection with respect to the 2D image-plane point.  If the parameter
    for the output partial matrix is passed as (cmod_float_t (*)[2])NULL,
    then it will not be calculated. */

void cmod_cahvor_2d_to_3d(
    const cmod_float_t pos2[2],	/* input 2D position */
    const cmod_float_t c[3],	/* input model center position vector   C */
    const cmod_float_t a[3],	/* input model orthog. axis unit vector A */
    const cmod_float_t h[3],	/* input model horizontal vector        H */
    const cmod_float_t v[3],	/* input model vertical vector          V */
    const cmod_float_t o[3],	/* input model optical axis unit vector O */
    const cmod_float_t r[3],	/* input model radial-distortion terms  R */
    cmod_bool_t approx,		/* input flag to use fast approximation */
    cmod_float_t pos3[3],	/* output 3D origin of projection */
    cmod_float_t uvec3[3],	/* output unit vector ray of projection */
    cmod_float_t par[3][2])	/* output partial derivative of uvec3 to pos2 */
{
    cmod_int_t i;
    cmod_int_t j;
    cmod_float_t deriv;
    cmod_float_t dldr[3][3];
    cmod_float_t drdx[3];
    cmod_float_t drdy[3];
    cmod_float_t drpdr[3][3];
    cmod_float_t drpdri[3][3];
    cmod_float_t drpdx[3];
    cmod_float_t drpdy[3];
    cmod_float_t du;
    cmod_float_t dudt;
    cmod_float_t f[3];
    cmod_float_t g[3];
    cmod_float_t k1;
    cmod_float_t k3;
    cmod_float_t k5;
    cmod_float_t irrt[3][3];
    cmod_float_t lambda[3];
    cmod_float_t m33[3][3];
    cmod_float_t magi;
    cmod_float_t magv;
    cmod_float_t mu;
    cmod_float_t n33[3][3];
    cmod_float_t omega;
    cmod_float_t omega_2;
    cmod_float_t poly;
    cmod_float_t pp[3];
    cmod_float_t rr[3];
    cmod_float_t sgn;
    cmod_float_t t[3];
    cmod_float_t tau;
    cmod_float_t u3[3];
    cmod_float_t u;
    cmod_float_t u_2;
    cmod_float_t v3[3];
    cmod_float_t w[3];
    cmod_float_t wo[3];

    /* Check input */
    CMOD_ASSERT("cmod_cahvor_2d_to_3d", r    != NULL);
    CMOD_ASSERT("cmod_cahvor_2d_to_3d", pos2 != NULL);

    /* The projection point is merely the C of the camera model. */
    copy3(c, pos3);

    /* Calculate the projection ray assuming normal vector directions, */
    /* neglecting distortion.                                          */
    scale3(pos2[1], a, f);
    sub3(v, f, f);
    scale3(pos2[0], a, g);
    sub3(h, g, g);
    cross3(f, g, rr);
    magi = mag3(rr);
    CMOD_ASSERT("cmod_cahvor_2d_to_3d", magi > EPSILON);
    magi = 1.0/magi;
    scale3(magi, rr, rr);

    /* Check and optionally correct for vector directions. */
    sgn = 1;
    cross3(v, h, t);
    if (dot3(t, a) < 0) {
	scale3(-1.0, rr, rr);
	sgn = -1;
	}

    /* Optionally compute partial for non-linear part of the model */
    if (par != NULL) {
	ident33(irrt);
	for (i=0; i<3; i++) {
	    for (j=0; j<3; j++) {
		irrt[i][j] -= rr[i] * rr[j];
		}
	    }
	cross3(f, a, t);
	mult331(irrt, t, w);
	drpdx[0] = par[0][0] = -sgn * w[0] * magi;
	drpdx[1] = par[1][0] = -sgn * w[1] * magi;
	drpdx[2] = par[2][0] = -sgn * w[2] * magi;
	cross3(g, a, t);
	mult331(irrt, t, w);
	drpdy[0] = par[0][1] = sgn * w[0] * magi;
	drpdy[1] = par[1][1] = sgn * w[1] * magi;
	drpdy[2] = par[2][1] = sgn * w[2] * magi;
	}

    /* Remove the radial lens distortion.  Preliminary values of omega,  */
    /* lambda, and tau are computed from the rr vector including         */
    /* distortion, in order to obtain the coefficients of the equation   */
    /* k5*u^5 + k3*u^3 + k1*u = 1, which is solved for u by means of     */
    /* Newton's method.  This value is used to compute the corrected rr. */
    omega = dot3(rr, o);
    omega_2 = omega * omega;
    CMOD_ASSERT("cmod_cahvor_2d_to_3d", fabs(omega_2) > EPSILON);
    scale3(omega, o, wo);
    sub3(rr, wo, lambda);
    tau = dot3(lambda, lambda) / omega_2;
    k1 = 1 + r[0];		/*  1 + rho0 */
    k3 = r[1] * tau;		/*  rho1*tau  */
    k5 = r[2] * tau*tau;	/*  rho2*tau^2  */
    mu = r[0] + k3 + k5;
    u = 1.0 - mu;	/* initial approximation for iterations */
    for (i=0; i<MAXITER; i++) {
	u_2 = u*u;
	poly  =  ((k5*u_2  +  k3)*u_2 + k1)*u - 1;
	deriv = (5*k5*u_2 + 3*k3)*u_2 + k1;
	if (deriv <= EPSILON) {
	    CMOD_ERROR("cmod_cahvor_2d_to_3d", "Distortion is too negative");
	    break;
	    }
	else {
	    du = poly/deriv;
	    u -= du;
	    if (fabs(du) < CONV) {
		break;
		}
	    }
	}
    if (i >= MAXITER) {
	CMOD_ERROR_I("cmod_cahvor_2d_to_3d", "Too many iterations", i);
	}
    mu = 1 - u;
    scale3(mu, lambda, pp);
    sub3(rr, pp, uvec3);
    magv = mag3(uvec3);
    CMOD_ASSERT("cmod_cahvor_2d_to_3d", fabs(magv) > EPSILON);
    scale3(1.0/magv, uvec3, uvec3);

    /* Note:  If partial derivatives are to be computed, corrected values */
    /* of omega, lambda, tau, and mu must be computed.                    */

    /* Optionally calculate the partial of uvec3 with respect to pos2 */
    if (par == NULL) {
	return;
	}

    /* Note that the approximate partial for non-linear part is above */

    /* If requested, just use the approximations for speed */
    if (approx) {
	return;
	}

    /* Recompute omega, lambda, tau, and mu */
    omega = dot3(uvec3, o);
    omega_2 = omega * omega;
    CMOD_ASSERT("cmod_cahvor_2d_to_3d", fabs(omega)   > EPSILON);
    CMOD_ASSERT("cmod_cahvor_2d_to_3d", fabs(omega_2) > EPSILON);
    scale3(omega, o, wo);
    sub3(uvec3, wo, lambda);
    tau = dot3(lambda, lambda) / omega_2;
    mu = r[0] + r[1]*tau + r[2]*tau*tau;

    /* Compute the partial derivatives for distortion */

    ident33(dldr);
    mult313(o, o, m33);
    sub33(dldr, m33, dldr);

    /*...
    ident33(dldo);
    scale33(-omega, dldo, dldo);
    mult313(o, uvec3, m33);
    sub33(dldo, m33, dldo);
    ...*/

    dudt = r[1] + (2 * r[2] * tau);

    /*...
    mult133(lambda, dldo, v3);
    scale3(2/omega_2, v3, v3);
    scale3((2 * tau / omega), uvec3, u3);
    sub3(v3, u3, v3);
    mult313(lambda, v3, m33);
    scale33(dudt, m33, m33);
    scale33(mu, dldo, n33);
    add33(m33, n33, drpdo);
    ...*/

    mult133(lambda, dldr, v3);
    scale3(2/omega_2, v3, v3);
    scale3((2 * tau / omega), o, u3);
    sub3(v3, u3, v3);
    mult313(lambda, v3, m33);
    scale33(dudt, m33, m33);
    scale33(mu, dldr, n33);
    add33(m33, n33, drpdr);
    ident33(m33);
    add33(m33, drpdr, drpdr);
    scale33(magv, drpdr, drpdr);

    /* Apply these partials to get the final result */

    inv33(drpdr, drpdri);
    mult331(drpdri, drpdx, drdx);
    mult331(drpdri, drpdy, drdy);

    par[0][0] = drdx[0];
    par[1][0] = drdx[1];
    par[2][0] = drdx[2];
    par[0][1] = drdy[0];
    par[1][1] = drdy[1];
    par[2][1] = drdy[2];
    }


/******************************************************************************
********************************   CMOD_CAHVOR_3D_TO_2D   *********************
*******************************************************************************

    This function projects a 3D point into the image plane using the
    camera model parameters provided. In addition to the 2D projection,
    it outputs the 3D perpendicular distance from the camera to the
    3D point, and the partial derivative matrix of the 2D point with respect
    to the 3D point. If the parameter for the output partial matrix is
    passed as (cmod_float_t (*)[3])NULL, then it will not be calculated. */

void cmod_cahvor_3d_to_2d(
    const cmod_float_t pos3[3],	/* input 3D position */
    const cmod_float_t c[3],	/* input model center vector C */
    const cmod_float_t a[3],	/* input model axis   vector A */
    const cmod_float_t h[3],	/* input model horiz. vector H */
    const cmod_float_t v[3],	/* input model vert.  vector V */
    const cmod_float_t o[3],	/* input model optical axis  O */
    const cmod_float_t r[3],	/* input model radial-distortion terms R */
    cmod_bool_t approx,		/* input flag to use fast approximation */
    cmod_float_t *range,	/* output range along A (same units as C) */
    cmod_float_t pos2[2],	/* output 2D image-plane projection */
    cmod_float_t par[2][3])	/* output partial derivative of pos2 to pos3 */
{
    cmod_float_t alpha;
    cmod_float_t beta;
    cmod_float_t dldp[3][3];
    cmod_float_t dppdp[3][3];
    cmod_float_t dudt;
    cmod_float_t dxhdpp[3];
    cmod_float_t dyhdpp[3];
    cmod_float_t gamma;
    cmod_float_t lambda[3];
    cmod_float_t m33[3][3];
    cmod_float_t mu;
    cmod_float_t n33[3][3];
    cmod_float_t omega;
    cmod_float_t omega_2;
    cmod_float_t p_c[3];
    cmod_float_t pp[3];
    cmod_float_t pp_c[3];
    cmod_float_t tau;
    cmod_float_t u3[3];
    cmod_float_t v3[3];
    cmod_float_t wo[3];
    cmod_float_t xh;
    cmod_float_t yh;

    /* Check input */
    CMOD_ASSERT("cmod_cahvor_3d_to_2d", r     != NULL);
    CMOD_ASSERT("cmod_cahvor_3d_to_2d", range != NULL);
    CMOD_ASSERT("cmod_cahvor_3d_to_2d", pos2  != NULL);

    /* Calculate p' and other necessary quantities */
    sub3(pos3, c, p_c);
    omega = dot3(p_c, o);
    omega_2 = omega * omega;
    CMOD_ASSERT("cmod_cahvor_3d_to_2d", fabs(omega_2) > EPSILON);
    scale3(omega, o, wo);
    sub3(p_c, wo, lambda);
    tau = dot3(lambda, lambda) / omega_2;
    mu = r[0] + (r[1] * tau) + (r[2] * tau * tau);
    scale3(mu, lambda, pp);
    add3(pos3, pp, pp);

    /* Calculate alpha, beta, gamma, which are (p' - c) */
    /* dotted with a, h, v, respectively                */
    sub3(pp, c, pp_c);
    alpha  = dot3(pp_c, a);
    beta   = dot3(pp_c, h);
    gamma  = dot3(pp_c, v);
    CMOD_ASSERT("cmod_cahvor_3d_to_2d", fabs(alpha) > EPSILON);

    /* Calculate the projection */
    pos2[0] = xh = beta  / alpha;
    pos2[1] = yh = gamma / alpha;
    *range = alpha;

    /* Only calculate the partial derivatives upon request */
    if (par == NULL) {
	return;
	}

    /* Calculate the approximate partial derivatives */

    scale3(xh, a, v3);
    sub3(h, v3, v3);
    scale3(1/alpha, v3, (approx ? par[0] : dxhdpp));

    scale3(yh, a, v3);
    sub3(v, v3, v3);
    scale3(1/alpha, v3, (approx ? par[1] : dyhdpp));

    /* If requested, just use the approximations for speed */
    if (approx) {
	return;
	}

    /* Complete the calculations for accuracy */
    CMOD_ASSERT("cmod_cahvor_3d_to_2d", fabs(omega) > EPSILON);

    ident33(dldp);
    mult313(o, o, m33);
    sub33(dldp, m33, dldp);

    dudt = r[1] + (2 * r[2] * tau);

    mult133(lambda, dldp, v3);
    scale3(2/omega_2, v3, v3);
    scale3((2 * tau / omega), o, u3);
    sub3(v3, u3, v3);
    mult313(lambda, v3, m33);
    scale33(dudt, m33, m33);
    scale33(mu, dldp, n33);
    add33(m33, n33, dppdp);
    ident33(m33);
    add33(m33, dppdp, dppdp);

    mult133(dxhdpp, dppdp, par[0]);
    mult133(dyhdpp, dppdp, par[1]);
    }


/******************************************************************************
********************************   CMOD_CAHVOR_3D_TO_2D_POINT   ***************
*******************************************************************************

    This function projects the vanishing point of any 3D line onto the image
    plane. In addition it calculates the partial-derivative matrix of the 2D
    point with respect to the 3D vector of the input ray. If the parameter for
    the output partial matrix is passed as (cmod_float_t (*)[3])NULL, then
    it will not be calculated. */

void cmod_cahvor_3d_to_2d_point(
    const cmod_float_t c[3],	/* input model center vector C */
    const cmod_float_t a[3],	/* input model axis   vector A */
    const cmod_float_t h[3],	/* input model horiz. vector H */
    const cmod_float_t v[3],	/* input model vert.  vector V */
    const cmod_float_t o[3],	/* input model optical axis  O */
    const cmod_float_t r[3],	/* input model radial-distortion terms R */
    cmod_bool_t approx,		/* input flag to use fast approximation */
    const cmod_float_t pos3[3],	/* input 3D position of line */
    const cmod_float_t uvec3[3],/* input 3D unit vector of line */
    cmod_float_t pos2[2],	/* output 2D image-plane projection */
    cmod_float_t par[2][3])	/* output derivative matrix of pos2 to uvec3 */
{
    cmod_float_t alpha;
    cmod_float_t beta;
    cmod_float_t gamma;
    cmod_float_t lambda[3];
    cmod_float_t mu;
    cmod_float_t omega;
    cmod_float_t omega_2;
    cmod_float_t pp_c[3];
    cmod_float_t tau;
    cmod_float_t wo[3];

    /* Check input */
    CMOD_ASSERT("cmod_cahvor_3d_to_2d_point", r    != NULL);
    CMOD_ASSERT("cmod_cahvor_3d_to_2d_point", pos2 != NULL);

    /* Calculate (p' - c). Note: p' is never computed directly, */
    /* but is understood to be a unit distance from c in the    */
    /* direction of the vanishing point.                        */
    omega = dot3(uvec3, o);
    CMOD_ASSERT("cmod_cahvor_3d_to_2d_point", fabs(omega) > EPSILON);
    omega_2 = omega * omega;
    scale3(omega, o, wo);
    sub3(uvec3, wo, lambda);
    tau = dot3(lambda, lambda) / omega_2;
    mu = r[0] + (r[1] * tau) + (r[2] * tau * tau);
    scale3(mu, lambda, pp_c);
    add3(uvec3, pp_c, pp_c);

    /* Calculate alpha, beta, gamma, which are (p' - c) */
    /* dotted with a, h, v, respectively                */
    alpha  = dot3(pp_c, a);
    beta   = dot3(pp_c, h);
    gamma  = dot3(pp_c, v);
    CMOD_ASSERT("cmod_cahvor_3d_to_2d_point", fabs(alpha) > EPSILON);

    /* Calculate the projection */
    pos2[0] = beta  / alpha;
    pos2[1] = gamma / alpha;

    /* Optionally calculate the partial-derivative matrix */
    if (par != NULL) {
	CMOD_ERROR("cmod_cahvor_3d_to_2d_point", "par not yet calculated");
	zero3(par[0]);
	zero3(par[1]);
	}
    }


/******************************************************************************
********************************   CMOD_CAHVOR_MOVE   *************************
*******************************************************************************

    This function relocates a camera model, based on the initial and final
    positions and orientations of a camera platform reference point, a
    point which is rigidly connected to the camera, but is otherwise
    arbitrary. */

void cmod_cahvor_move(
    const cmod_float_t p_i[3],	/* input initial pos of camera ref pt */
    const cmod_float_t q_i[4],	/* input initial orientation of camera ref pt */
    const cmod_float_t c_i[3],	/* input initial model center vector C */
    const cmod_float_t a_i[3],	/* input initial model axis   vector A */
    const cmod_float_t h_i[3],	/* input initial model horiz. vector H */
    const cmod_float_t v_i[3],	/* input initial model vert.  vector V */
    const cmod_float_t o_i[3],	/* input initial model optical axis  O */
    const cmod_float_t r_i[3],	/* input initial model radial terms  R */
    const cmod_float_t p_f[3],	/* input final pos of camera ref pt */
    const cmod_float_t q_f[4],	/* input final orientation of camera ref pt */
    cmod_float_t c_f[3],	/* output final model center vector C */
    cmod_float_t a_f[3],	/* output final model axis   vector A */
    cmod_float_t h_f[3],	/* output final model horiz. vector H */
    cmod_float_t v_f[3],	/* output final model vert.  vector V */
    cmod_float_t o_f[3],	/* output final model optical axis  O */
    cmod_float_t r_f[3])	/* output final model radial terms  R */
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

    /* Copy over the R "vector" unchanged */
    copy3(r_i, r_f);
    }


/******************************************************************************
********************************   CMOD_CAHVOR_REFLECT   **********************
*******************************************************************************

    This function relocates a camera model, based on the image reflecting
    from a mirror defined by a plane. If the initial model is the true
    model, then the final model will be a virtual model representing how
    the camera sees the reflected world. If the initial model is the
    virtual model, then the final model will be the true model. */

void cmod_cahvor_reflect(
    const cmod_float_t c_i[3],	/* input initial model center vector C */
    const cmod_float_t a_i[3],	/* input initial model axis   vector A */
    const cmod_float_t h_i[3],	/* input initial model horiz. vector H */
    const cmod_float_t v_i[3],	/* input initial model vert.  vector V */
    const cmod_float_t o_i[3],	/* input initial model optical axis  O */
    const cmod_float_t r_i[3],	/* input initial model radial terms  R */
    const cmod_float_t p[3],	/* input point on the reflecting plane */
    const cmod_float_t n[3],	/* input normal to the reflecting plane */
    cmod_float_t c_f[3],	/* output final model center vector C */
    cmod_float_t a_f[3],	/* output final model axis   vector A */
    cmod_float_t h_f[3],	/* output final model horiz. vector H */
    cmod_float_t v_f[3],	/* output final model vert.  vector V */
    cmod_float_t o_f[3],	/* output final model optical axis  O */
    cmod_float_t r_f[3],	/* output final model radial terms  R */
    cmod_bool_t *parallel,	/* output if camera view & plane are parallel */
    cmod_bool_t *behind)	/* output if camera behind reflecting plane */
{
    cmod_float_t d;
    cmod_float_t k;
    cmod_float_t nu[3];
    cmod_float_t u[3];

    cmod_float_t c[3];
    cmod_float_t a[3];
    cmod_float_t h[3];
    cmod_float_t v[3];
    cmod_float_t o[3];

    /* Check input */
    CMOD_ASSERT("cmod_cahvor_reflect", parallel != NULL);
    CMOD_ASSERT("cmod_cahvor_reflect", behind   != NULL);

    /* Check if the camera view and plane are parallel (or nearly so) */
    unit3(n, nu);
    k = dot3(a_i, nu);
    if (fabs(k) < EPSILON) {
	*parallel = TRUE;
	*behind   = FALSE;
	return;
	}
    *parallel = FALSE;

    /* Compute the reflected A vector */
    scale3(-2*k, nu, a);
    add3(a_i, a, a);

    /* Compute the reflected H vector */
    scale3(-2*dot3(h_i, nu), nu, h);
    add3(h_i, h, h);

    /* Compute the reflected V vector */
    scale3(-2*dot3(v_i, nu), nu, v);
    add3(v_i, v, v);

    /* Compute the reflected O vector */
    scale3(-2*dot3(o_i, nu), nu, o);
    add3(o_i, o, o);

    /* Calculate where A's extension intersects the plane */
    d = (dot3(p, nu) - dot3(c_i, nu)) / k;
    if (d < 0) {
	*behind = TRUE;
	return;
	}
    *behind = FALSE;
    scale3(d, a_i, u);
    add3(c_i, u, c);

    /* Compute the reflected model's position */
    scale3(-d, a, u);
    add3(u, c, c);

    /* Copy over the results */
    copy3(c,   c_f);
    copy3(a,   a_f);
    copy3(h,   h_f);
    copy3(v,   v_f);
    copy3(o,   o_f);
    copy3(r_i, r_f);
    }


/******************************************************************************
********************************   CMOD_CAHVOR_REFLECT_COV   ******************
*******************************************************************************

    This function reflects the covariance matrix to correspond to the
    reflected model. */

void cmod_cahvor_reflect_cov(
    cmod_float_t s_i[18][18],	/* input initial covariance */
    const cmod_float_t n[3],	/* input normal to the reflecting plane */
    cmod_float_t s_f[18][18])	/* output final covariance */
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
    cmod_cahvor_transform_cov(s_i, r, s_f);
    }


/******************************************************************************
********************************   CMOD_CAHVOR_ROT_COV   **********************
*******************************************************************************

    This function rotates a CAHVOR model's covariance matrix to correspond to
    rotation of the model itself. Note that model translations do not affect
    the covariance matrix. */

void cmod_cahvor_rot_cov(
    cmod_float_t r_i[3][3],	/* input initial orientation of camera ref pt */
    cmod_float_t s_i[18][18],	/* input initial covariance */
    cmod_float_t r_f[3][3],	/* input final orientation of camera ref pt */
    cmod_float_t s_f[18][18])	/* output final covariance */
{
    cmod_float_t r[3][3];
    cmod_float_t r_it[3][3];

    /* Calculate the rotation, R, from the initial to the final orientation */
    trans33(r_i, r_it);
    mult333(r_f, r_it, r);
    cmod_cahvor_transform_cov(s_i, r, s_f);
    }


/******************************************************************************
********************************   CMOD_CAHVOR_ROTATE_COV   *******************
*******************************************************************************

    This function rotates a CAHVOR model's covariance matrix to correspond to
    rotation of the model itself. Note that model translations do not affect
    the covariance matrix. */

void cmod_cahvor_rotate_cov(
    const cmod_float_t q_i[4],	/* input initial orientation of camera ref pt */
    cmod_float_t s_i[18][18],	/* input initial covariance */
    const cmod_float_t q_f[4],	/* input final orientation of camera ref pt */
    cmod_float_t s_f[18][18])	/* output final covariance */
{
    cmod_float_t r_f[3][3];
    cmod_float_t r_i[3][3];

    rotq(q_f, r_f);
    rotq(q_i, r_i);
    cmod_cahvor_rot_cov(r_i, s_i, r_f, s_f);
    }


/******************************************************************************
********************************   CMOD_CAHVOR_SCALE   ************************
*******************************************************************************

    This function scales a camera model. The scale factors should be
    understood as the same scale factors that would be applied to a 2D
    coordinate in the original model to convert it to a coordinate in
    the resulting model. Note that any precomputed internal model
    parameters will be made obsolete by this function. */

void cmod_cahvor_scale(
    cmod_float_t hscale,	/* input horizontal scale factor */
    cmod_float_t vscale,	/* input vertical   scale factor */
    const cmod_float_t h1[3],	/* input  model horiz. vector H */
    const cmod_float_t v1[3],	/* input  model vert.  vector V */
    cmod_float_t s1[18][18],	/* input  covariance matrix, or NULL */
    cmod_float_t h2[3],		/* output model horiz. vector H */
    cmod_float_t v2[3],		/* output model vert.  vector V */
    cmod_float_t s2[18][18])	/* output covariance matrix, or NULL */
{
    cmod_int_t i;
    cmod_int_t j;

    /* Check input */
    CMOD_ASSERT("cmod_cahvor_scale", h1 != NULL);
    CMOD_ASSERT("cmod_cahvor_scale", v1 != NULL);
    CMOD_ASSERT("cmod_cahvor_scale", h2 != NULL);
    CMOD_ASSERT("cmod_cahvor_scale", v2 != NULL);

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
	for (i=0; i<18; i++) {
	    for (j=0; j<18; j++) {
		s2[i][j] = s1[i][j];
		}
	    }
	}
    for (i=0; i<18; i++) {
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
********************************   CMOD_CAHVOR_SHIFT   ************************
*******************************************************************************

    This function shifts a camera model. The shift values should be
    understood as the coordinates of the old model where the origin
    will fall in the new one. Note that any precomputed internal model
    parameters will be made obsolete by this function. */

void cmod_cahvor_shift(
    cmod_float_t dx,		/* input horizontal shift */
    cmod_float_t dy,		/* input vertical   shift */
    const cmod_float_t a1[3],	/* input  model axis   vector A */
    const cmod_float_t h1[3],	/* input  model horiz. vector H */
    const cmod_float_t v1[3],	/* input  model vert.  vector V */
    cmod_float_t s1[18][18],	/* input  covariance matrix, or NULL */
    cmod_float_t h2[3],		/* output model horiz. vector H */
    cmod_float_t v2[3],		/* output model vert.  vector V */
    cmod_float_t s2[18][18])	/* output covariance matrix, or NULL */
{
    cmod_int_t i;
    cmod_int_t j;

    /* Check input */
    CMOD_ASSERT("cmod_cahvor_shift", a1 != NULL);
    CMOD_ASSERT("cmod_cahvor_shift", h1 != NULL);
    CMOD_ASSERT("cmod_cahvor_shift", v1 != NULL);
    CMOD_ASSERT("cmod_cahvor_shift", h2 != NULL);
    CMOD_ASSERT("cmod_cahvor_shift", v2 != NULL);

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
	for (i=0; i<18; i++) {
	    for (j=0; j<18; j++) {
		s2[i][j] = s1[i][j];
		}
	    }
	}
    for (i=0; i<18; i++) {
	s2[ 6][i]  -=  dx * s2[3][i];
	s2[ 7][i]  -=  dx * s2[4][i];
	s2[ 8][i]  -=  dx * s2[5][i];
	s2[ 9][i]  -=  dy * s2[3][i];
	s2[10][i]  -=  dy * s2[4][i];
	s2[11][i]  -=  dy * s2[5][i];
	}
    for (i=0; i<18; i++) {
	s2[i][ 6]  -=  dx * s2[i][3];
	s2[i][ 7]  -=  dx * s2[i][4];
	s2[i][ 8]  -=  dx * s2[i][5];
	s2[i][ 9]  -=  dy * s2[i][3];
	s2[i][10]  -=  dy * s2[i][4];
	s2[i][11]  -=  dy * s2[i][5];
	}
    }


/******************************************************************************
********************************   CMOD_CAHVOR_TRANSFORM_COV   ****************
*******************************************************************************

    This function transform a CAHVOR model's covariance matrix according to a
    3x3 matrix that represents the transformation to the 3D coordinates of
    the model. Note that model translations do not affect the covariance
    matrix. */

void cmod_cahvor_transform_cov(
    cmod_float_t s_i[18][18],	/* input initial covariance */
    cmod_float_t r[3][3],	/* input transform matrix of camera ref pt */
    cmod_float_t s_f[18][18])	/* output final covariance */
{
    cmod_int_t i;
    cmod_int_t j;
    cmod_int_t k;
    cmod_float_t d;
    cmod_float_t r18[18][18];
    cmod_float_t r18t[18][18];
    cmod_float_t stemp[18][18];

    /* Check input */
    CMOD_ASSERT("cmod_cahvor_transform_cov", s_i != NULL);
    CMOD_ASSERT("cmod_cahvor_transform_cov", r   != NULL);
    CMOD_ASSERT("cmod_cahvor_transform_cov", s_f != NULL);

    /* Contruct a matrix, R18, (and its transpose) to rotate the covariance
	|R     |
	| R    |
	|  R   |
	|   R  |
	|    R |
	|     I|
    */
    for (i=0; i<18; i++) {
	for (j=0; j<18; j++) {
	    r18[i][j] = 0;
	    }
	}
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
    for (i=0; i<18; i++) {
	for (j=0; j<18; j++) {
	    r18t[i][j] = r18[j][i];
	    }
	}

    /* Pre-multiply by the matrix */
    for (i=0; i<18; i++) {
	for (j=0; j<18; j++) {
	    d = 0;
	    for (k=0; k<18; k++) {
		d += r18[i][k] * s_i[k][j];
		}
	    stemp[i][j] = d;
	    }
	}

    /* Post-multiply by the transpose */
    for (i=0; i<18; i++) {
	for (j=0; j<18; j++) {
	    d = 0;
	    for (k=0; k<18; k++) {
		d += stemp[i][k] * r18t[k][j];
		}
	    s_f[i][j] = d;
	    }
	}
    }


/******************************************************************************
********************************   CMOD_CAHVOR_VALIDATE   *********************
*******************************************************************************

    This function validates the components of a camera model to determine
    whether or not the model makes some kind of sense. Only generic
    sanity-checking tests are performed. */

cmod_stat_t cmod_cahvor_validate(
    const cmod_float_t c[3],	/* input model center vector C */
    const cmod_float_t a[3],	/* input model axis   vector A */
    const cmod_float_t h[3],	/* input model horiz. vector H */
    const cmod_float_t v[3],	/* input model vert.  vector V */
    const cmod_float_t o[3],	/* input model optical axis unit vector O */
    const cmod_float_t r[3])	/* input model radial-distortion terms  R */
{
    cmod_float_t mag;
    const cmod_float_t ulower = 0.9; /* lower magnitude limit for unit vector */
    const cmod_float_t uupper = 1.1; /* upper magnitude limit for unit vector */

    /* Check linear part of the model */
    if (cmod_cahv_validate(c, a, h, v) < 0) {
	return FAILURE;
	}

    /* Check optical vector */
    mag = mag3(o);
    if ((mag < ulower) || (mag > uupper)) {
	CMOD_ERROR("cmod_cahvor_validate", "Bad O vector");
        return FAILURE;
        }

    return SUCCESS;
    }


/******************************************************************************
********************************   CMOD_CAHVOR_WARP_FROM_CAHV   ***************
*******************************************************************************

    This function takes an image coordinate which resulted from a camera
    modeled by CAHV and warps it into an image coordinate modeled by
    CAHVOR. */

void cmod_cahvor_warp_from_cahv(
    const cmod_float_t c1[3],	/* input initial model center vector C */
    const cmod_float_t a1[3],	/* input initial model axis   vector A */
    const cmod_float_t h1[3],	/* input initial model horiz. vector H */
    const cmod_float_t v1[3],	/* input initial model vert.  vector V */
    const cmod_float_t pos1[2],	/* input 2D position from CAHV */
    cmod_bool_t approx,		/* input flag to use fast approximation */
    const cmod_float_t c2[3],	/* input final model center vector C */
    const cmod_float_t a2[3],	/* input final model axis   vector A */
    const cmod_float_t h2[3],	/* input final model horiz. vector H */
    const cmod_float_t v2[3],	/* input final model vert.  vector V */
    const cmod_float_t o2[3],	/* input final model optical axis  O */
    const cmod_float_t r2[3],	/* input final model radial  terms R */
    cmod_float_t pos2[2])	/* output 2D position for CAHVOR */
{
    cmod_float_t p3[3];
    cmod_float_t range;
    cmod_float_t u3[3];

    cmod_cahv_2d_to_3d(pos1, c1, a1, h1, v1, p3, u3,
						(cmod_float_t (*)[2])NULL);
    add3(p3, u3, p3);
    cmod_cahvor_3d_to_2d(p3, c2, a2, h2, v2, o2, r2, approx, &range, pos2,
						(cmod_float_t (*)[3])NULL);
    }


/******************************************************************************
********************************   CMOD_CAHVOR_WARP_LIMIT   *******************
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

void cmod_cahvor_warp_limit(
    const cmod_float_t c[3],	/* input model center vector C */
    const cmod_float_t a[3],	/* input model axis   vector A */
    const cmod_float_t h[3],	/* input model horiz. vector H */
    const cmod_float_t v[3],	/* input model vert.  vector V */
    const cmod_float_t o[3],	/* input model axis   vector O */
    const cmod_float_t r[3],	/* input model dist.  terms  R */
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
    CMOD_ASSERT("cmod_cahvor_warp_limit", dist != NULL);

    /* Prepare the vector limiting how close we can get to the image plane */
    cross3(nrm, dir, axis);
    rotq(quatva(axis, (PI/2)-MIN_PLANE_ANGLE, q), r33);
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
    CMOD_ASSERT("cmod_cahvor_warp_limit", fabs(sa) > EPSILON);

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
	cmod_cahvor_2d_to_3d(pos2[i], c, a, h, v, o, r, FALSE, p3, u3, NULL);

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
	CMOD_ASSERT("cmod_cahvor_warp_limit", fabs(d) > EPSILON);
	d = dot3(dir, u3) / d;
	if ((i == 0) || (minfov && (d < *dist)) || (!minfov && (d > *dist))) {
	    *dist = d;
	    copy3(u3, vect);
	    }
	}
    }


/******************************************************************************
********************************   CMOD_CAHVOR_WARP_MODEL   *******************
*******************************************************************************

    This function warps a camera model so that it is purely linear. The
    parameter C will not change. The parameters O (identical to A) and
    R (all terms zero) will not be output. Note that image warping will
    be necessary in order to use the new models. */

void cmod_cahvor_warp_model(
    const cmod_float_t c[3],	/* input model center vector C */
    const cmod_float_t a[3],	/* input model axis   vector A */
    const cmod_float_t h[3],	/* input model horiz. vector H */
    const cmod_float_t v[3],	/* input model vert.  vector V */
    const cmod_float_t o[3],	/* input model optical axis  O */
    const cmod_float_t r[3],	/* input model radial terms  R */
    cmod_bool_t minfov,		/* input if to minimize to common FOV */
    const cmod_int_t idims[2],	/* input image dimensions of input  model */
    const cmod_int_t odims[2],	/* input image dimensions of output model */
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

    /* Check input */
    CMOD_ASSERT("cmod_cahvor_warp_model", idims != NULL);
    CMOD_ASSERT("cmod_cahvor_warp_model", odims != NULL);
    CMOD_ASSERT("cmod_cahvor_warp_model", hs    != NULL);
    CMOD_ASSERT("cmod_cahvor_warp_model", hc    != NULL);
    CMOD_ASSERT("cmod_cahvor_warp_model", vs    != NULL);
    CMOD_ASSERT("cmod_cahvor_warp_model", vc    != NULL);
    CMOD_ASSERT("cmod_cahvor_warp_model", theta != NULL);

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
    cmod_cahvor_warp_limit(c, a, h, v, o, r, minfov, u, xneg, y,
	0, vc0, 0, 0, 0, idims[1]-1, xv1, &d);

    /* Choose the upper horizontal vector and image coordinate */
    xc2 = odims[0] - 1;
    cmod_cahvor_warp_limit(c, a, h, v, o, r, minfov, u, x, y,
	idims[0]-1, vc0, idims[0]-1, 0, idims[0]-1, idims[1]-1, xv2, &d);

    /* Choose the lower vertical vector and image coordinate */
    yc1 = 0;
    scale3(-1.0, y, yneg);
    cmod_cahvor_warp_limit(c, a, h, v, o, r, minfov, u, yneg, x,
	hc0, 0, 0, 0, idims[0]-1, 0, yv1, &d);

    /* Choose the upper vertical vector and image coordinate */
    yc2 = odims[1] - 1;
    cmod_cahvor_warp_limit(c, a, h, v, o, r, minfov, u, y, x,
	hc0, idims[1]-1, 0, idims[1]-1, idims[0]-1, idims[1]-1, yv2, &d);

    /* Create a linear model with the geometry determined above */
    if (cmod_cahv_create(ppnt, x, xv1, xv2, xc1, xc2, y, yv1, yv2, yc1, yc2,
			c2, a2, h2, v2) == FAILURE) {
	zero3(a2);
	zero3(h2);
	zero3(v2);
	*hs = 0;
	*hc = 0;
	*vs = 0;
	*vc = 0;
	*theta = 0;
	return;
	}

    /* Compute internal model parameters */
    cmod_cahv_internal(c2, a2, h2, v2, NULL, hs, hc, vs, vc, theta, NULL);
    }


/******************************************************************************
********************************   CMOD_CAHVOR_WARP_MODELS   ******************
*******************************************************************************

    This function warps a pair of almost aligned camera models into a
    perfectly aligned stereo pair of virtual camera models. The virtual
    models will have their original C vectors, but will share newly
    computed A, H, and V vectors, as well as internal model parameters.
    Since the output models will be linear, the parameters O (identical to A)
    and R (all terms zero) will not be output. Note that image warping will
    be necessary in order to use the new models. */

void cmod_cahvor_warp_models(
    const cmod_float_t c1[3],	/* input model 1 center vector C */
    const cmod_float_t a1[3],	/* input model 1 axis   vector A */
    const cmod_float_t h1[3],	/* input model 1 horiz. vector H */
    const cmod_float_t v1[3],	/* input model 1 vert.  vector V */
    const cmod_float_t o1[3],	/* input model 1 axis   vector O */
    const cmod_float_t r1[3],	/* input model 1 dist.  terms  R */
    const cmod_float_t c2[3],	/* input model 2 center vector C */
    const cmod_float_t a2[3],	/* input model 2 axis   vector A */
    const cmod_float_t h2[3],	/* input model 2 horiz. vector H */
    const cmod_float_t v2[3],	/* input model 2 vert.  vector V */
    const cmod_float_t o2[3],	/* input model 2 axis   vector O */
    const cmod_float_t r2[3],	/* input model 2 dist.  terms  R */
    cmod_bool_t minfov,		/* input if to minimize to common FOV */
    const cmod_int_t idims[2],	/* input image dimensions of input  models */
    const cmod_int_t odims[2],	/* input image dimensions of output models */
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

    /* Check input */
    CMOD_ASSERT("cmod_cahvor_warp_models", idims != NULL);
    CMOD_ASSERT("cmod_cahvor_warp_models", odims != NULL);
    CMOD_ASSERT("cmod_cahvor_warp_models", hs    != NULL);
    CMOD_ASSERT("cmod_cahvor_warp_models", hc    != NULL);
    CMOD_ASSERT("cmod_cahvor_warp_models", vs    != NULL);
    CMOD_ASSERT("cmod_cahvor_warp_models", vc    != NULL);
    CMOD_ASSERT("cmod_cahvor_warp_models", theta != NULL);

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
    cmod_cahvor_warp_limit(c1, a1, h1, v1, o1, r1,
	minfov, u, xneg, y, 0, vc1, 0, 0, 0, idims[1]-1,
	xv1, &d1);
    cmod_cahvor_warp_limit(c2, a2, h2, v2, o2, r2,
	minfov, u, xneg, y, 0, vc2, 0, 0, 0, idims[1]-1,
	t, &d2);
    if ((minfov && (d2 < d1)) || (!minfov && (d2 > d1))) {
	copy3(t, xv1);
	}

    /* Choose the upper horizontal vector and image coordinate */
    xc2 = odims[0] - 1;
    scale3(-1.0, x, xneg);
    cmod_cahvor_warp_limit(c1, a1, h1, v1, o1, r1,
	minfov, u, x, y, idims[0]-1, vc1, idims[0]-1, 0, idims[0]-1, idims[1]-1,
	xv2, &d1);
    cmod_cahvor_warp_limit(c2, a2, h2, v2, o2, r2,
	minfov, u, x, y, idims[0]-1, vc2, idims[0]-1, 0, idims[0]-1, idims[1]-1,
	t, &d2);
    if ((minfov && (d2 < d1)) || (!minfov && (d2 > d1))) {
	copy3(t, xv2);
	}

    /* Choose the lower vertical vector and image coordinate */
    yc1 = 0;
    scale3(-1.0, y, yneg);
    cmod_cahvor_warp_limit(c1, a1, h1, v1, o1, r1,
	minfov, u, yneg, x, hc1, 0, 0, 0, idims[0]-1, 0,
	yv1, &d1);
    cmod_cahvor_warp_limit(c2, a2, h2, v2, o2, r2,
	minfov, u, yneg, x, hc2, 0, 0, 0, idims[0]-1, 0,
	t, &d2);
    if ((minfov && (d2 < d1)) || (!minfov && (d2 > d1))) {
	copy3(t, yv1);
	}

    /* Choose the upper vertical vector and image coordinate */
    yc2 = odims[1] - 1;
    cmod_cahvor_warp_limit(c1, a1, h1, v1, o1, r1,
	minfov, u, y, x, hc1, idims[1]-1, 0, idims[1]-1, idims[0]-1, idims[1]-1,
	yv2, &d1);
    cmod_cahvor_warp_limit(c2, a2, h2, v2, o2, r2,
	minfov, u, y, x, hc2, idims[1]-1, 0, idims[1]-1, idims[0]-1, idims[1]-1,
	t, &d2);
    if ((minfov && (d2 < d1)) || (!minfov && (d2 > d1))) {
	copy3(t, yv2);
	}

    /* Create a linear model with the geometry determined above */
    if (cmod_cahv_create(ppnt1, x, xv1, xv2, xc1, xc2, y, yv1, yv2, yc1, yc2,
			c, a, h, v) == FAILURE) {
	zero3(a);
	zero3(h);
	zero3(v);
	*hs = 0;
	*hc = 0;
	*vs = 0;
	*vc = 0;
	*theta = 0;
	return;
	}

    /* Compute internal model parameters */
    cmod_cahv_internal(c, a, h, v, NULL, hs, hc, vs, vc, theta, NULL);
    }


/******************************************************************************
********************************   CMOD_CAHVOR_WARP_MODELS_NODIMS   ***********
*******************************************************************************

    This function is the same as cmod_cahvor_warp_models(), but for cases
    where the image dimensions are not known precisely. This function fills
    in those dimensions with approximate values generated from the horizontal
    and vertical centers of the input models themselves. */

void cmod_cahvor_warp_models_nodims(
    const cmod_float_t c1[3],	/* input model 1 center vector C */
    const cmod_float_t a1[3],	/* input model 1 axis   vector A */
    const cmod_float_t h1[3],	/* input model 1 horiz. vector H */
    const cmod_float_t v1[3],	/* input model 1 vert.  vector V */
    const cmod_float_t o1[3],	/* input model 1 axis   vector O */
    const cmod_float_t r1[3],	/* input model 1 dist.  terms  R */
    const cmod_float_t c2[3],	/* input model 2 center vector C */
    const cmod_float_t a2[3],	/* input model 2 axis   vector A */
    const cmod_float_t h2[3],	/* input model 2 horiz. vector H */
    const cmod_float_t v2[3],	/* input model 2 vert.  vector V */
    const cmod_float_t o2[3],	/* input model 2 axis   vector O */
    const cmod_float_t r2[3],	/* input model 2 dist.  terms  R */
    cmod_bool_t minfov,		/* input if to minimize to common FOV */
    cmod_float_t a[3],		/* output virtual model axis   vector A */
    cmod_float_t h[3],		/* output virtual model horiz. vector H */
    cmod_float_t v[3],		/* output virtual model vert.  vector V */
    cmod_float_t *hs,		/* output horizontal scale factor */
    cmod_float_t *hc,		/* output horizontal center */
    cmod_float_t *vs,		/* output vertical scale factor */
    cmod_float_t *vc,		/* output vertical center */
    cmod_float_t *theta)	/* output angle between axes */
{
    cmod_int_t dims[2];
    cmod_float_t hc1;
    cmod_float_t hc2;
    cmod_float_t hs1;
    cmod_float_t hs2;
    cmod_float_t vc1;
    cmod_float_t vc2;
    cmod_float_t vs1;
    cmod_float_t vs2;

    /* Compute a reasonable set of dimensions from the model centers */
    cmod_cahv_internal(c1, a1, h1, v1, NULL,
				&hs1, &hc1, &vs1, &vc1, theta, NULL);
    cmod_cahv_internal(c2, a2, h2, v2, NULL,
				&hs2, &hc2, &vs2, &vc2, theta, NULL);
    dims[0] = (cmod_int_t)(hc1 + hc2 + 1);
    dims[1] = (cmod_int_t)(vc1 + vc2 + 1);

    /* Call the more general function with these values */
    cmod_cahvor_warp_models(c1, a1, h1, v1, o1, r1, c2, a2, h2, v2, o2, r2,
				minfov, dims, dims,
				a, h, v, hs, hc, vs, vc, theta);
    }


/******************************************************************************
********************************   CMOD_CAHVOR_WARP_MODELS2   *****************
*******************************************************************************

    This function is a newer version of cmod_cahvor_warp_models(). It uses
    a row/normal-based way of computing things rather than a row/column-based
    way. The newer version is less sensitive to the left/right order in which
    the models are presented to the routine. */

void cmod_cahvor_warp_models2(
    const cmod_float_t c1[3],	/* input model 1 center vector C */
    const cmod_float_t a1[3],	/* input model 1 axis   vector A */
    const cmod_float_t h1[3],	/* input model 1 horiz. vector H */
    const cmod_float_t v1[3],	/* input model 1 vert.  vector V */
    const cmod_float_t o1[3],	/* input model 1 axis   vector O */
    const cmod_float_t r1[3],	/* input model 1 dist.  terms  R */
    const cmod_float_t c2[3],	/* input model 2 center vector C */
    const cmod_float_t a2[3],	/* input model 2 axis   vector A */
    const cmod_float_t h2[3],	/* input model 2 horiz. vector H */
    const cmod_float_t v2[3],	/* input model 2 vert.  vector V */
    const cmod_float_t o2[3],	/* input model 2 axis   vector O */
    const cmod_float_t r2[3],	/* input model 2 dist.  terms  R */
    cmod_bool_t minfov,		/* input if to minimize to common FOV */
    const cmod_int_t idims[2],	/* input image dimensions of input  models */
    const cmod_int_t odims[2],	/* input image dimensions of output models */
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

    /* Check input */
    CMOD_ASSERT("cmod_cahvor_warp_models", idims != NULL);
    CMOD_ASSERT("cmod_cahvor_warp_models", odims != NULL);
    CMOD_ASSERT("cmod_cahvor_warp_models", hs    != NULL);
    CMOD_ASSERT("cmod_cahvor_warp_models", hc    != NULL);
    CMOD_ASSERT("cmod_cahvor_warp_models", vs    != NULL);
    CMOD_ASSERT("cmod_cahvor_warp_models", vc    != NULL);
    CMOD_ASSERT("cmod_cahvor_warp_models", theta != NULL);

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
    cmod_cahvor_warp_limit(c1, a1, h1, v1, o1, r1,
	minfov, u, xneg, y, 0, vc1, 0, 0, 0, idims[1]-1,
	xv1, &d1);
    cmod_cahvor_warp_limit(c2, a2, h2, v2, o2, r2,
	minfov, u, xneg, y, 0, vc2, 0, 0, 0, idims[1]-1,
	t, &d2);
    if ((minfov && (d2 < d1)) || (!minfov && (d2 > d1))) {
	copy3(t, xv1);
	}

    /* Choose the upper horizontal vector and image coordinate */
    xc2 = odims[0] - 1;
    scale3(-1.0, x, xneg);
    cmod_cahvor_warp_limit(c1, a1, h1, v1, o1, r1,
	minfov, u, x, y, idims[0]-1, vc1, idims[0]-1, 0, idims[0]-1, idims[1]-1,
	xv2, &d1);
    cmod_cahvor_warp_limit(c2, a2, h2, v2, o2, r2,
	minfov, u, x, y, idims[0]-1, vc2, idims[0]-1, 0, idims[0]-1, idims[1]-1,
	t, &d2);
    if ((minfov && (d2 < d1)) || (!minfov && (d2 > d1))) {
	copy3(t, xv2);
	}

    /* Choose the lower vertical vector and image coordinate */
    yc1 = 0;
    scale3(-1.0, y, yneg);
    cmod_cahvor_warp_limit(c1, a1, h1, v1, o1, r1,
	minfov, u, yneg, x, hc1, 0, 0, 0, idims[0]-1, 0,
	yv1, &d1);
    cmod_cahvor_warp_limit(c2, a2, h2, v2, o2, r2,
	minfov, u, yneg, x, hc2, 0, 0, 0, idims[0]-1, 0,
	t, &d2);
    if ((minfov && (d2 < d1)) || (!minfov && (d2 > d1))) {
	copy3(t, yv1);
	}

    /* Choose the upper vertical vector and image coordinate */
    yc2 = odims[1] - 1;
    cmod_cahvor_warp_limit(c1, a1, h1, v1, o1, r1,
	minfov, u, y, x, hc1, idims[1]-1, 0, idims[1]-1, idims[0]-1, idims[1]-1,
	yv2, &d1);
    cmod_cahvor_warp_limit(c2, a2, h2, v2, o2, r2,
	minfov, u, y, x, hc2, idims[1]-1, 0, idims[1]-1, idims[0]-1, idims[1]-1,
	t, &d2);
    if ((minfov && (d2 < d1)) || (!minfov && (d2 > d1))) {
	copy3(t, yv2);
	}

    /* Create a linear model with the geometry determined above */
    if (cmod_cahv_create(ppnt1, x, xv1, xv2, xc1, xc2, y, yv1, yv2, yc1, yc2,
			c, a, h, v) == FAILURE) {
	zero3(a);
	zero3(h);
	zero3(v);
	*hs = 0;
	*hc = 0;
	*vs = 0;
	*vc = 0;
	*theta = 0;
	return;
	}

    /* Compute internal model parameters */
    cmod_cahv_internal(c, a, h, v, NULL, hs, hc, vs, vc, theta, NULL);
    }


/******************************************************************************
********************************   CMOD_CAHVOR_WARP_TO_CAHV   *****************
*******************************************************************************

    This function takes an image coordinate which resulted from a camera
    modeled by CAHVOR and warps it into an image coordinate modeled by
    CAHV. */

void cmod_cahvor_warp_to_cahv(
    const cmod_float_t c1[3],	/* input initial model center vector C */
    const cmod_float_t a1[3],	/* input initial model axis   vector A */
    const cmod_float_t h1[3],	/* input initial model horiz. vector H */
    const cmod_float_t v1[3],	/* input initial model vert.  vector V */
    const cmod_float_t o1[3],	/* input initial model optical axis  O */
    const cmod_float_t r1[3],	/* input initial model radial  terms R */
    const cmod_float_t pos1[2],	/* input 2D position from CAHVOR */
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

    cmod_cahvor_2d_to_3d(pos1, c1, a1, h1, v1, o1, r1, approx, p3, u3,
						(cmod_float_t (*)[2])NULL);
    add3(p3, u3, p3);
    cmod_cahv_3d_to_2d(p3, c2, a2, h2, v2, &range, pos2,
						(cmod_float_t (*)[3])NULL);
    }


/******************************************************************************
********************************   CMOD_CAHVOR_WARP_TO_CAHVOR   ***************
*******************************************************************************

    This function takes an image coordinate which resulted from a camera
    modeled by CAHVOR and warps it into an image coordinate modeled by
    a different CAHVOR. */

void cmod_cahvor_warp_to_cahvor(
    const cmod_float_t c1[3],	/* input initial model center vector C */
    const cmod_float_t a1[3],	/* input initial model axis   vector A */
    const cmod_float_t h1[3],	/* input initial model horiz. vector H */
    const cmod_float_t v1[3],	/* input initial model vert.  vector V */
    const cmod_float_t o1[3],	/* input initial model optical axis  O */
    const cmod_float_t r1[3],	/* input initial model radial terms  R */
    const cmod_float_t pos1[2],	/* input 2D position from CAHVOR */
    cmod_bool_t approx,		/* input flag to use fast approximation */
    const cmod_float_t c2[3],	/* input final model center vector C */
    const cmod_float_t a2[3],	/* input final model axis   vector A */
    const cmod_float_t h2[3],	/* input final model horiz. vector H */
    const cmod_float_t v2[3],	/* input final model vert.  vector V */
    const cmod_float_t o2[3],	/* input final model optical axis  O */
    const cmod_float_t r2[3],	/* input final model radial terms  R */
    cmod_float_t pos2[2])	/* output 2D position for CAHV */
{
    cmod_float_t p3[3];
    cmod_float_t range;
    cmod_float_t u3[3];

    cmod_cahvor_2d_to_3d(pos1, c1, a1, h1, v1, o1, r1, approx, p3, u3,
	(cmod_float_t (*)[2])NULL);
    add3(p3, u3, p3);
    cmod_cahvor_3d_to_2d(p3, c2, a2, h2, v2, o2, r2, approx, &range, pos2,
	(cmod_float_t (*)[3])NULL);
    }
