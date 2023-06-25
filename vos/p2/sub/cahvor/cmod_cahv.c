/******************************************************************************
*                                                                             *
*                               C M O D _ C A H V                             *
*                                                                             *
*                                       Todd Litwin                           *
*                                       Written:  3 Aug 1993                  *
*                                       Updated: 24 May 2016                  *
*                                                                             *
*                                       Copyright (C) 1993, 1994, 1995, 1996, *
*                                                     1997, 1998, 1999, 2000, *
*                                                     2002, 2003, 2005, 2007, *
*                                                     2009, 2010, 2012, 2016  *
*                                       California Institute of Technology    *
*                                       All Rights Reserved                   *
*                                                                             *
*******************************************************************************


	This file contains functions for using the Yakimovsky & Cunningham
	camera model, known locally as CAHV. */


#include <math.h>
#include <mat3.h>

#include "cmod_cahv.h"
#include "cmod_error.h"

enum {
    SUCCESS =  0,
    FAILURE = -1
    };

#ifndef EPSILON
#define EPSILON (1e-15)
#endif

#define	PI (3.14159265358979323846)

#define MIN_PLANE_ANGLE (15.0 * PI / 180.0)

void cmod_cahv_create2_rot(const cmod_float_t fu[3], const cmod_float_t nu[3],
    const cmod_float_t xu[3], cmod_float_t angle, cmod_float_t dir[3]);


/******************************************************************************
********************************   CMOD_CAHV_2D_TO_3D   ***********************
*******************************************************************************

    This function projects a 2D image point out into 3D using the
    camera model parameters provided. In addition to the 3D projection,
    it outputs and the partial-derivative matrix of the unit vector of the
    projection with respect to the 2D image-plane point. If the parameter
    for the output partial matrix is passed as (cmod_float_t (*)[2])NULL,
    then it will not be calculated. */

void cmod_cahv_2d_to_3d(
    const cmod_float_t pos2[2],	/* input 2D position */
    const cmod_float_t c[3],	/* input model center vector C */
    const cmod_float_t a[3],	/* input model axis   vector A */
    const cmod_float_t h[3],	/* input model horiz. vector H */
    const cmod_float_t v[3],	/* input model vert.  vector V */
    cmod_float_t pos3[3],	/* output 3D origin of projection */
    cmod_float_t uvec3[3],	/* output unit vector ray of projection */
    cmod_float_t par[3][2])	/* output partial derivative of uvec3 to pos2 */
{
    cmod_int_t i;
    cmod_int_t j;
    cmod_float_t f[3];
    cmod_float_t g[3];
    cmod_float_t irrt[3][3];
    cmod_float_t magi;
    cmod_float_t sgn;
    cmod_float_t t[3];
    cmod_float_t u[3];

    /* Check input */
    CMOD_ASSERT("cmod_cahv_2d_to_3d", pos2  != NULL);
    CMOD_ASSERT("cmod_cahv_2d_to_3d", uvec3 != NULL);

    /* The projection point is merely the C of the camera model */
    copy3(c, pos3);

    /* Calculate the projection ray assuming normal vector directions */
    scale3(pos2[1], a, f);
    sub3(v, f, f);
    scale3(pos2[0], a, g);
    sub3(h, g, g);
    cross3(f, g, uvec3);
    magi = mag3(uvec3);
    CMOD_ASSERT("cmod_cahv_2d_to_3d", magi > EPSILON);
    magi = 1.0/magi;
    scale3(magi, uvec3, uvec3);

    /* Check and optionally correct for vector directions */
    sgn = 1;
    cross3(v, h, t);
    if (dot3(t, a) < 0) {
	scale3(-1.0, uvec3, uvec3);
	sgn = -1;
	}

    /* Optionally calculate the partial of uvec3 with respect to pos2 */
    if (par == NULL) {
	return;
	}
    ident33(irrt);
    for (i=0; i<3; i++) {
	for (j=0; j<3; j++) {
	    irrt[i][j] -= uvec3[i] * uvec3[j];
	    }
	}
    cross3(f, a, t);
    mult331(irrt, t, u);
    par[0][0] = -sgn * u[0] * magi;
    par[1][0] = -sgn * u[1] * magi;
    par[2][0] = -sgn * u[2] * magi;
    cross3(g, a, t);
    mult331(irrt, t, u);
    par[0][1] = sgn * u[0] * magi;
    par[1][1] = sgn * u[1] * magi;
    par[2][1] = sgn * u[2] * magi;
    }


/******************************************************************************
********************************   CMOD_CAHV_3D_TO_2D   ***********************
*******************************************************************************

    This function projects a 3D point into the image plane using the
    camera model parameters provided. In addition to the 2D projection,
    it outputs the 3D perpendicular distance from the camera to the
    3D point, and the partial derivative matrix of the 2D point with respect
    to the 3D point. If the parameter for the output partial matrix is
    passed as (cmod_float_t (*)[3])NULL, then it will not be calculated. */

void cmod_cahv_3d_to_2d(
    const cmod_float_t pos3[3],	/* input 3D position */
    const cmod_float_t c[3],	/* input model center vector C */
    const cmod_float_t a[3],	/* input model axis   vector A */
    const cmod_float_t h[3],	/* input model horiz. vector H */
    const cmod_float_t v[3],	/* input model vert.  vector V */
    cmod_float_t *range,	/* output range along A (same units as C) */
    cmod_float_t pos2[2],	/* output 2D image-plane projection */
    cmod_float_t par[2][3])	/* output partial derivative of pos2 to pos3 */
{
    cmod_float_t d[3];
    cmod_float_t r_1;

    /* Check input */
    CMOD_ASSERT("cmod_cahv_3d_to_2d", range != NULL);
    CMOD_ASSERT("cmod_cahv_3d_to_2d", pos2  != NULL);

    /* Calculate the projection */
    sub3(pos3, c, d);
    *range = dot3(d, a);
    CMOD_ASSERT("cmod_cahv_3d_to_2d", fabs(*range) > EPSILON);
    r_1 = 1.0 / *range;
    pos2[0] = dot3(d, h) * r_1;
    pos2[1] = dot3(d, v) * r_1;

    /* Optionally calculate the partial of pos2 with respect to pos3 */
    if (par != NULL) {
	scale3(pos2[0], a, par[0]);
	sub3(h, par[0], par[0]);
	scale3(r_1, par[0], par[0]);
	scale3(pos2[1], a, par[1]);
	sub3(v, par[1], par[1]);
	scale3(r_1, par[1], par[1]);
	}
    }


/******************************************************************************
********************************   CMOD_CAHV_3D_TO_2D_RAY   *******************
*******************************************************************************

    This function projects the vanishing point of any 3D line onto the image
    plane; the 2D back projection of the line from the vanishing point is
    calculated as well. In addition it calculates the partial-derivative
    matrix of the 2D point and vector with respect to the 3D vector of the
    input ray. If the parameter for the output partial matrix is passed as
    (cmod_float_t (*)[3])NULL, then it will not be calculated. If the output
    unit vector is (cmod_float_t *)NULL, then it will not be calculated. Note
    that there is a degenerate case where the 2D output vector is undefined;
    it will be set to (0,0) in this case. */

void cmod_cahv_3d_to_2d_ray(
    const cmod_float_t c[3],	/* input model center vector C */
    const cmod_float_t a[3],	/* input model axis   vector A */
    const cmod_float_t h[3],	/* input model horiz. vector H */
    const cmod_float_t v[3],	/* input model vert.  vector V */
    const cmod_float_t pos3[3],	/* input 3D position of line */
    const cmod_float_t uvec3[3],/* input 3D unit vector of line */
    cmod_float_t pos2[2],	/* output 2D image-plane projection */
    cmod_float_t uvec2[2],	/* output 2D unit vector back-projected line */
    cmod_float_t par[4][3])	/* output derivative of pos2,uvec2 to uvec3 */
{
    cmod_float_t d[3];
    cmod_float_t hp[3];
    cmod_float_t p[3];
    cmod_float_t vp[3];
    cmod_float_t x;

    /* Check input */
    CMOD_ASSERT("cmod_cahv_3d_to_2d_ray", pos2 != NULL);

    /* Calculate the projection of the vanishing point */
    x = dot3(uvec3, a);
    if (fabs(x) < EPSILON) {
	pos2[0] = 0;
	pos2[1] = 0;
	}
    else {
	pos2[0] = dot3(uvec3, h) / x;
	pos2[1] = dot3(uvec3, v) / x;
	}

    /* Calculate the back-projected ray in 3D */
    if (uvec2 == NULL) {
	return;
	}
    sub3(c, pos3, d);
    cross3(uvec3, d, d);
    cross3(a, d, d);
    unit3(d, d);

    /* Original algorithm to compute the 2D back-projected ray doesn't work. I
       think the problem is caused by making the H' and V' projections into the
       image plane unit length, thus destroying their relative scales...
    cmod_float_t uh[3], uv[3];
    cross3(h, a, uh);
    cross3(a, uh, uh);
    unit3(uh, uh);
    cross3(v, a, uv);
    cross3(a, uv, uv);
    unit3(uv, uv);
    uvec2[0] = dot3(d, uh);
    uvec2[1] = dot3(d, uv);
    ...*/

    /* Project H and V into the image plane */
    sub3(h, scale3(dot3(a, h), a, p), hp);
    sub3(v, scale3(dot3(a, v), a, p), vp);

    /* Use those projections to convert the back-projected ray to 2D */
    p[0] = dot3(d, hp);
    p[1] = dot3(d, vp);
    p[2] = 0;
    unit3(p, p);
    uvec2[0] = p[0];
    uvec2[1] = p[1];

    /* An alternative method, unused since it has a singularity...
    add3(uvec3, d, p);
    x = dot3(p, a);
    if (fabs(x) < EPSILON) {
	uvec2[0] = 0;
	uvec2[1] = 0;
	}
    else {
	uvec2[0] = dot3(p, h) / x;
	uvec2[1] = dot3(p, v) / x;
	d[0] = uvec2[0] - pos2[0];
	d[1] = uvec2[1] - pos2[1];
	d[2] = 0;
	unit3(d, d);
	uvec2[0] = d[0];
	uvec2[1] = d[1];
	}
    ...*/

    /* Optionally calculate the partial-derivative matrix */
    if (par != NULL) {
	CMOD_ERROR("cmod_cahv_3d_to_2d_ray", "par not yet calculated");
	zero3(par[0]);
	zero3(par[1]);
	zero3(par[2]);
	zero3(par[3]);
	}
    }


/******************************************************************************
********************************   CMOD_CAHV_CREATE   *************************
*******************************************************************************

    This function creates a camera model based on input geometric
    specifications. The X and Y vectors define the image plane. While
    nominally they would be orthogonal to each other, they do not need to
    to be. Nor do they need to have unit length, but may have any reasonable
    magnitude.

    The X and Y vectors should be constructed as if the image plane lies
    between the position point and the scene being viewed. While this is
    not the physical case for real cameras, whose image planes lie behind
    the lens and receive an inverted image from light that has passed
    through a lens, it simplifies thinking about the geometry. */

cmod_stat_t cmod_cahv_create(
    const cmod_float_t pos[3],	/* input 3D position */
    const cmod_float_t x[3],	/* input dir of increasing X image coordinate */
    const cmod_float_t xv1[3],	/* input X projection vector #1 */
    const cmod_float_t xv2[3],	/* input X projection vector #2 */
    cmod_float_t xc1,		/* input X image coord to match vector #1 */
    cmod_float_t xc2,		/* input X image coord to match vector #2 */
    const cmod_float_t y[3],	/* input dir of increasing Y image coordinate */
    const cmod_float_t yv1[3],	/* input Y projection vector #1 */
    const cmod_float_t yv2[3],	/* input Y projection vector #2 */
    cmod_float_t yc1,		/* input Y image coord to match vector #1 */
    cmod_float_t yc2,		/* input Y image coord to match vector #2 */
    cmod_float_t c[3],	  	/* output model center vector C */
    cmod_float_t a[3],		/* output model axis   vector A */
    cmod_float_t h[3],		/* output model horiz. vector H */
    cmod_float_t v[3])		/* output model vert.  vector V */
{
    cmod_float_t inv[3][3];
    cmod_float_t mat[3][3];
    cmod_float_t vec[3];

    /* C */
    copy3(pos, c);

    /* A */
    cross3(x, y, a);
    if (dot3(a, xv1) < 0) {
	scale3(-1.0, a, a);
	}
    unit3(a, a);

    /* Check that all vectors are reasonably in front of the plane */
    if ((dot3(a, xv1) < EPSILON) || (dot3(a, xv2) < EPSILON) ||
	(dot3(a, yv1) < EPSILON) || (dot3(a, yv2) < EPSILON)) {
	CMOD_ERROR("cmod_cahv_create",
		"vector(s) near or behind image plane");
	return FAILURE;
	}

    /* The approach for finding H and V uses the dot-product expressions     */
    /* for projecting a 3D world point into a 2D image point. Each such      */
    /* expression for a projection to a known image location represents an   */
    /* equation for the 3 unknown components of H or V. Three such equations */
    /* can be used to solve for those unknowns. Below we have used that      */
    /* approach to collect the values and coefficients in a vector and       */
    /* matrix. We solve for the unknowns by the matrix-inversion technique.  */

    /* We only input two projections for each dimension, yet three are       */
    /* needed to define a plane. We manufacture the third projection by      */
    /* offsetting one of the inputs along the other dimension, an operation  */
    /* that should preserve the coordinate value. Note that this not only    */
    /* satifies the requirement that the projections be non-coplanar, but it */
    /* also injects the information needed to specify non-orthogonal         */
    /* coordinate axes.                                                      */

    /* H */
    vec[0] = xc1 * dot3(xv1, a);
    vec[1] = xc1 * dot3(xv1, a);
    vec[2] = xc2 * dot3(xv2, a);
    copy3(xv1, mat[0]);
    copy3(xv1, mat[1]);
    copy3(xv2, mat[2]);
    add3(y, mat[0], mat[0]);
    if (inv33(mat, inv) == NULL) {
	CMOD_ERROR("cmod_cahv_create", "cannot invert matrix for h");
	return FAILURE;
	}
    mult331(inv, vec, h);

    /* V */
    vec[0] = yc1 * dot3(yv1, a);
    vec[1] = yc1 * dot3(yv1, a);
    vec[2] = yc2 * dot3(yv2, a);
    copy3(yv1, mat[0]);
    copy3(yv1, mat[1]);
    copy3(yv2, mat[2]);
    add3(x, mat[0], mat[0]);
    if (inv33(mat, inv) == NULL) {
	CMOD_ERROR("cmod_cahv_create", "cannot invert matrix for v");
	return FAILURE;
	}
    mult331(inv, vec, v);

    return SUCCESS;
    }


/******************************************************************************
********************************   CMOD_CAHV_CREATE2   ************************
*******************************************************************************

    This function creates a camera model based on input geometric
    specifications. The X and Y vectors define the image plane. While
    nominally they would be orthogonal to each other, they do not need to
    to be. Nor do they need to be orthogonal to the FWD vector. The input
    vectors to not need to have unit length, but may have any reasonable
    magnitude.

    The X and Y vectors should be constructed as if the image plane lies
    between the position point and the scene being viewed. While this is
    not the physical case for real cameras, whose image planes lie behind
    the lens and receive an inverted image from light that has passed
    through a lens, it simplifies thinking about the geometry.

    Note that the image dimensions are floating-point values. They may span
    any continuous set of values and do not need to represent integer pixels.
    If integer pixels are intended, be sure to pay attention to the assumed
    meaning of a coordinate location. Specifically consider that the field
    of view should normally be expressed to include the entirety of the edge
    pixels, and not merely to their centers. */

cmod_stat_t cmod_cahv_create2(
    const cmod_float_t pos[3],	/* input 3D position */
    const cmod_float_t fwd[3],	/* input forward-pointing vector: FOV center */
    const cmod_float_t x[3],	/* input dir of increasing X image coordinate */
    const cmod_float_t y[3],	/* input dir of increasing Y image coordinate */
    cmod_float_t xfov,	  	/* input X field of view (rad) */
    cmod_float_t yfov,		/* input Y field of view (rad) */
    cmod_float_t xdim,		/* input X image dimension */
    cmod_float_t ydim,		/* input Y image dimension */
    cmod_float_t xc,		/* input X coordinate of image center */
    cmod_float_t yc,		/* input Y coordinate of image center */
    cmod_float_t c[3],		/* output model center vector C */
    cmod_float_t a[3],		/* output model axis   vector A */
    cmod_float_t h[3],		/* output model horiz. vector H */
    cmod_float_t v[3])		/* output model vert.  vector V */
{
    cmod_float_t fu[3];
    cmod_float_t nu[3];
    cmod_float_t xu[3];
    cmod_float_t xmax[3];
    cmod_float_t xmin[3];
    cmod_float_t yu[3];
    cmod_float_t ymax[3];
    cmod_float_t ymin[3];

    /* Check that the forward vector does not lie too near the image plane */
    unit3(x, xu);
    unit3(y, yu);
    cross3(xu, yu, nu);
    unit3(fwd, fu);
    if (dot3(fu, nu) < 0) {
	scale3(-1.0, nu, nu);
	}
    if (dot3(fu, nu) < cos(PI/2 - MIN_PLANE_ANGLE)) {
	CMOD_ERROR("cmod_cahv_create2",
		"forward vector too near image plane");
	return FAILURE;
	}

    /* Find extreme pointings of the forward vector for both dimensions */
    cmod_cahv_create2_rot(fu, nu, xu, -xfov/2, xmin);
    cmod_cahv_create2_rot(fu, nu, xu,  xfov/2, xmax);
    cmod_cahv_create2_rot(fu, nu, yu, -yfov/2, ymin);
    cmod_cahv_create2_rot(fu, nu, yu,  yfov/2, ymax);

    /* Create the model */
    return cmod_cahv_create(pos,
		xu, xmin, xmax, xc-(xdim-1)/2, xc+(xdim-1)/2,
		yu, ymin, ymax, yc-(ydim-1)/2, yc+(ydim-1)/2,
		c, a, h, v);
    }


/******************************************************************************
********************************   CMOD_CAHV_CREATE2_ROT   ********************
*******************************************************************************

    This function rotates the forward vector along a given image dimension. */

void cmod_cahv_create2_rot(
    const cmod_float_t fu[3],	/* input forward unit vector: center of FOV */
    const cmod_float_t nu[3],	/* input outward unit normal to image plane */
    const cmod_float_t xu[3],	/* input unit dir of increasing image coord */
    cmod_float_t angle,		/* input rotation angle (rad) */
    cmod_float_t dir[3])	/* output rotation of forward vector */
{
    cmod_float_t amax;
    cmod_float_t amin;
    cmod_float_t axis[3];
    cmod_float_t quat[4];
    cmod_float_t rot[3][3];
    cmod_float_t sinamax;

    /* Axis for rotation across the row or column */
    cross3(fu, xu, axis);
    sinamax = mag3(axis);
    unit3(axis, axis);

    /* Calculate the range of allowable angles that do not */
    /* come too close to or cross the image plane          */
    amax = asin(sinamax);
    amin = amax - PI;
    amax -= MIN_PLANE_ANGLE;
    amin += MIN_PLANE_ANGLE;

    /* Clip the input angle to within the allowed range */
    if (angle > amax) {
	angle = amax;
	}
    if (angle < amin) {
	angle = amin;
	}

    /* Rotate the pointing vector */
    quatva(axis, angle, quat);
    rotq(quat, rot);
    mult331(rot, fu, dir);
    }


/******************************************************************************
********************************   CMOD_CAHV_INTERNAL   ***********************
*******************************************************************************

    This function calculates the internal camera model parameters for the
    CAHV camera model, deriving them from C, A, H, V, and their covariance
    matrix S. If either the input or output covariance matrices is NULL,
    then the covariance computation will be skipped. */

void cmod_cahv_internal(
    const cmod_float_t c[3],	/* input model center vector C */
    const cmod_float_t a[3],	/* input model axis   vector A */
    const cmod_float_t h[3],	/* input model horiz. vector H */
    const cmod_float_t v[3],	/* input model vert.  vector V */
    cmod_float_t s[12][12],	/* input covariance of CAHV */
    cmod_float_t *hs,		/* output horizontal scale factor */
    cmod_float_t *hc,		/* output horizontal center */
    cmod_float_t *vs,		/* output vertical scale factor */
    cmod_float_t *vc,		/* output vertical center */
    cmod_float_t *theta,	/* output angle between axes */
    cmod_float_t s_int[5][5])	/* output covariance matrix */
{
    cmod_int_t i;
    cmod_int_t j;
    cmod_int_t k;
    cmod_float_t cross[3];
    cmod_float_t cross1[3];
    cmod_float_t cross2[3];
    cmod_float_t jacobian[5][12];
    cmod_float_t jacobian_t[12][5];
    cmod_float_t m_5_12[5][12];
    cmod_float_t sin2th;
    cmod_float_t sinth_costh;
    cmod_float_t v_h_a;

    /* Check input */
    CMOD_ASSERT("cmod_cahv_internal", c     != NULL);
    CMOD_ASSERT("cmod_cahv_internal", a     != NULL);
    CMOD_ASSERT("cmod_cahv_internal", h     != NULL);
    CMOD_ASSERT("cmod_cahv_internal", v     != NULL);
    CMOD_ASSERT("cmod_cahv_internal", hs    != NULL);
    CMOD_ASSERT("cmod_cahv_internal", hc    != NULL);
    CMOD_ASSERT("cmod_cahv_internal", vs    != NULL);
    CMOD_ASSERT("cmod_cahv_internal", vc    != NULL);
    CMOD_ASSERT("cmod_cahv_internal", theta != NULL);

    /* Calculate hs, hc, vs, vc, theta */
    *hs = mag3(cross3(a, h, cross));
    *hc = dot3(a, h);
    *vs = mag3(cross3(a, v, cross));
    *vc = dot3(a, v);
    *theta = atan2(
	dot3(cross3(v, h, cross), a),
	dot3(cross3(a, v, cross1), cross3(a, h, cross2))
	);
    CMOD_ASSERT("cmod_cahv_internal", fabs(*hs) > EPSILON);
    CMOD_ASSERT("cmod_cahv_internal", fabs(*vs) > EPSILON);

    /* Calculate the Jacobian of those 5 quantities */
    if ((s == NULL) || (s_int == NULL)) {
	return;
	}
    for (i=0; i<5; i++) {
	for (j=0; j<12; j++) {
	    jacobian[i][j] = 0;
	    }
	}
    jacobian[0][ 3] = - *hc * h[0] / *hs;
    jacobian[0][ 4] = - *hc * h[1] / *hs;
    jacobian[0][ 5] = - *hc * h[2] / *hs;
    jacobian[0][ 6] = (h[0] - (*hc * a[0])) /
				*hs;
    jacobian[0][ 7] = (h[1] - (*hc * a[1])) /
				*hs;
    jacobian[0][ 8] = (h[2] - (*hc * a[2])) /
				*hs;
    jacobian[1][ 3] = h[0];
    jacobian[1][ 4] = h[1];
    jacobian[1][ 5] = h[2];
    jacobian[1][ 6] = a[0];
    jacobian[1][ 7] = a[1];
    jacobian[1][ 8] = a[2];
    jacobian[2][ 3] = - *vc * v[0] / *vs;
    jacobian[2][ 4] = - *vc * v[1] / *vs;
    jacobian[2][ 5] = - *vc * v[2] / *vs;
    jacobian[2][ 9] = (v[0] - (*vc * a[0])) /
				*vs;
    jacobian[2][10] = (v[1] - (*vc * a[1])) /
				*vs;
    jacobian[2][11] = (v[2] - (*vc * a[2])) /
				*vs;
    jacobian[3][ 3] = v[0];
    jacobian[3][ 4] = v[1];
    jacobian[3][ 5] = v[2];
    jacobian[3][ 9] = a[0];
    jacobian[3][10] = a[1];
    jacobian[3][11] = a[2];
    v_h_a = dot3(cross3(v, h, cross), a);
    CMOD_ASSERT("cmod_cahv_internal", fabs(v_h_a) > EPSILON);
    sin2th = sin(*theta);
    sin2th *= sin2th;
    sinth_costh = sin(*theta) * cos(*theta);
    cross3(v, h, cross);
    jacobian[4][ 3] = (cross[0] * sinth_costh
	+ (*hc * v[0] + *vc * h[0]) * sin2th)
				/ v_h_a;
    jacobian[4][ 4] = (cross[1] * sinth_costh
	+ (*hc * v[1] + *vc * h[1]) * sin2th)
				/ v_h_a;
    jacobian[4][ 5] = (cross[2] * sinth_costh
	+ (*hc * v[2] + *vc * h[2]) * sin2th)
				/ v_h_a;
    cross3(a, v, cross);
    jacobian[4][ 6] = (cross[0] * sinth_costh
	+ (*vc * a[0] - v[0]) * sin2th)
				/ v_h_a;
    jacobian[4][ 7] = (cross[1] * sinth_costh
	+ (*vc * a[1] - v[1]) * sin2th)
				/ v_h_a;
    jacobian[4][ 8] = (cross[2] * sinth_costh
	+ (*vc * a[2] - v[2]) * sin2th)
				/ v_h_a;
    cross3(a, h, cross);
    jacobian[4][ 9] = (-cross[0] * sinth_costh
	+ (*hc * a[0] - h[0]) * sin2th)
				/ v_h_a;
    jacobian[4][10] = (-cross[1] * sinth_costh
	+ (*hc * a[1] - h[1]) * sin2th)
				/ v_h_a;
    jacobian[4][11] = (-cross[2] * sinth_costh
	+ (*hc * a[2] - h[2]) * sin2th)
				/ v_h_a;

    /* Calculate the covariance matrix J S Jt */
    for (i=0; i<5; i++) {			/* tranpose */
	for (j=0; j<12; j++) {
	    jacobian_t[j][i] = jacobian[i][j];
	    }
	}
    for (i=0; i<5; i++) {			/* J S */
	for (k=0; k<12; k++) {
	    m_5_12[i][k] = 0;
	    for (j=0; j<12; j++) {
		m_5_12[i][k] += jacobian[i][j] * s[j][k];
		}
	    }
	}
    for (i=0; i<5; i++) {			/* (J S) Jt */
	for (k=0; k<5; k++) {
	    s_int[i][k] = 0;
	    for (j=0; j<12; j++) {
		s_int[i][k] += m_5_12[i][j] * jacobian_t[j][k];
		}
	    }
	}
    }


/******************************************************************************
********************************   CMOD_CAHV_IPLANE   *************************
*******************************************************************************

    This function computes information on the image plane and related
    quantities. It computes the unit outward-facing normal to the image plane
    along with in-plane unit vectors in the directions of increasing horizontal
    and vertical dimensions (which need not be mutually orthogonal). It
    provides the projection point through which image-plane points are
    projected, as well as the "center" image coorinate whose projection is
    along the image plane's normal.

    Note that the in-plane unit vectors are constructed as if the image plane
    lies between the projection point and the scene being viewed. While this
    is not the physical case for real cameras, whose image planes lie behind
    the lens and receive an inverted image from light that passes through it,
    this simplifies thinking about the geometry. */

void cmod_cahv_iplane(
    const cmod_float_t c[3],	/* input model center vector C */
    const cmod_float_t a[3],	/* input model axis   vector A */
    const cmod_float_t h[3],	/* input model horiz. vector H */
    const cmod_float_t v[3],	/* input model vert.  vector V */
    cmod_float_t ppnt[3],	/* output projection point */
    cmod_float_t ndir[3],	/* output normal direction */
    cmod_float_t hdir[3],	/* output horizontal direction */
    cmod_float_t vdir[3],	/* output vertical direction */
    cmod_float_t *hc,		/* output horizontal center */
    cmod_float_t *vc)		/* output vertical center */
{
    cmod_float_t p2[2];
    cmod_float_t p3[3];
    cmod_float_t p32[3][2];
    cmod_float_t u3[3];

    /* Check input */
    CMOD_ASSERT("cmod_cahv_iplane", hc != NULL);
    CMOD_ASSERT("cmod_cahv_iplane", vc != NULL);

    /* Report the projection point and the unit normal */
    copy3(c, ppnt);
    copy3(a, ndir);

    /* Compute the center coordinates */
    *hc = dot3(a, h);
    *vc = dot3(a, v);

    /* Compute the in-plane coordinate direction vectors */
    p2[0] = *hc;
    p2[1] = *vc;
    cmod_cahv_2d_to_3d(p2, c, a, h, v, p3, u3, p32);
    u3[0] = p32[0][0];
    u3[1] = p32[1][0];
    u3[2] = p32[2][0];
    unit3(u3, hdir);
    u3[0] = p32[0][1];
    u3[1] = p32[1][1];
    u3[2] = p32[2][1];
    unit3(u3, vdir);

    /* It is convenient to use the partial derivatives of the center-point */
    /* projection to 3D in order to compute the in-plane vectors. This is  */
    /* because the directions we want (of the rows and columns) are not as */
    /* simple as they seem. In particular, if the two dimensions are not   */
    /* orthogonal, then they will not lie along H' and V'. The horizontal  */
    /* direction will be perpendicular to V', and the vertical direction   */
    /* will be perpendicular to H'.                                        */
    }


/******************************************************************************
********************************   CMOD_CAHV_MOVE   ***************************
*******************************************************************************

    This function relocates a camera model, based on the initial and final
    positions and orientations of a camera platform reference point, a
    point which is rigidly connected to the camera, but is otherwise
    arbitrary. */

void cmod_cahv_move(
    const cmod_float_t p_i[3],	/* input initial pos of camera ref pt */
    const cmod_float_t q_i[4],	/* input initial orientation of camera ref pt */
    const cmod_float_t c_i[3],	/* input initial model center vector C */
    const cmod_float_t a_i[3],	/* input initial model axis   vector A */
    const cmod_float_t h_i[3],	/* input initial model horiz. vector H */
    const cmod_float_t v_i[3],	/* input initial model vert.  vector V */
    const cmod_float_t p_f[3],	/* input final pos of camera ref pt */
    const cmod_float_t q_f[4],	/* input final orientation of camera ref pt */
    cmod_float_t c_f[3],	/* output final model center vector C */
    cmod_float_t a_f[3],	/* output final model axis   vector A */
    cmod_float_t h_f[3],	/* output final model horiz. vector H */
    cmod_float_t v_f[3])	/* output final model vert.  vector V */
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

    /* Rotate the A, H, V vectors */
    mult331(r, a_i, a_f);
    mult331(r, h_i, h_f);
    mult331(r, v_i, v_f);
    }


/******************************************************************************
********************************   CMOD_CAHV_POSE   ***************************
*******************************************************************************

    This function returns the position and rotation matrix of orientation
    for the given model. The absolute orientation is based on a reference
    orientation of the camera pointing straight down the Y axis, with Z up.
    */

void cmod_cahv_pose(
    const cmod_float_t c[3],	/* input model center vector C */
    const cmod_float_t a[3],	/* input model axis   vector A */
    const cmod_float_t h[3],	/* input model horiz. vector H */
    const cmod_float_t v[3],	/* input model vert.  vector V */
    cmod_float_t p[3],		/* output position vector */
    cmod_float_t r[3][3])	/* output rotation matrix */
{
    cmod_int_t i;
    cmod_float_t av[3];
    cmod_float_t vc;
    cmod_float_t vp[3];
    cmod_float_t vpa[3];
    cmod_float_t vs;
    /*...
    cmod_float_t ah[3];
    cmod_float_t hc;
    cmod_float_t hp[3];
    cmod_float_t hpa[3];
    cmod_float_t hs;
    ...*/

    /* Check input */
    CMOD_ASSERT("cmod_cahv_pose", c != NULL);
    CMOD_ASSERT("cmod_cahv_pose", a != NULL);
    CMOD_ASSERT("cmod_cahv_pose", h != NULL);
    CMOD_ASSERT("cmod_cahv_pose", v != NULL);
    CMOD_ASSERT("cmod_cahv_pose", r != NULL);

    /* Copy over the position */
    copy3(c, p);

    /* Compute the orientation, forcing vertical to dominate */
    vs = mag3(cross3(a, v, av));	/* vertical scale & center */
    vc = dot3(a, v);
    CMOD_ASSERT("cmod_cahv_pose", fabs(vs) > EPSILON);
    vp[0] = (v[0] - vc * a[0]) / vs;	/* unit projection of V in img plane */
    vp[1] = (v[1] - vc * a[1]) / vs;
    vp[2] = (v[2] - vc * a[2]) / vs;
    cross3(vp, a, vpa);			/* right vector */
    for (i=0; i<3; i++) {		/* rotation matrix from orthogonal */
	r[i][0] = vpa[i];		/*   unit vectors                  */
	r[i][1] = a[i];
	r[i][2] = -vp[i];
	}

    /* Compute the orientation, forcing horizontal to dominate */
    /*...
    hs = mag3(cross3(a, h, ah));	/+ horizontal scale & center +/
    hc = dot3(a, h);
    CMOD_ASSERT("cmod_cahv_pose", fabs(hs) > EPSILON);
    hp[0] = (h[0] - hc * a[0]) / hs;	/+ unit projection of H in img plane +/
    hp[1] = (h[1] - hc * a[1]) / hs;
    hp[2] = (h[2] - hc * a[2]) / hs;
    cross3(hp, a, hpa);			/+ up vector +/
    for (i=0; i<3; i++) {		/+ rotation matrix from orthogonal +/
	r[i][0] = hp[i];		/+   unit vectors                  +/
	r[i][1] = a[i];
	r[i][2] = hpa[i];
	}
    ...*/
    }


/******************************************************************************
********************************   CMOD_CAHV_POSTURE   ************************
*******************************************************************************

    This function returns the rotation matrix of orientation for the given
    model. The absolute orientation is based on a reference orientation of
    the camera pointing straight down the Y axis, with Z up. The left- or
    right-handedness of the vectors is preserved, with the normal case being
    considered right-handed. */

void cmod_cahv_posture(
    const cmod_float_t a[3],	/* input model axis   vector A */
    const cmod_float_t h[3],	/* input model horiz. vector H */
    const cmod_float_t v[3],	/* input model vert.  vector V */
    cmod_float_t r[3][3])	/* output rotation matrix */
{
    cmod_int_t i;
    cmod_float_t ah[3];
    cmod_float_t av[3];
    cmod_float_t hc;
    cmod_float_t hp[3];
    cmod_float_t hs;
    cmod_float_t vc;
    cmod_float_t vp[3];
    cmod_float_t vs;

    /* Check input */
    CMOD_ASSERT("cmod_cahv_posture", a != NULL);
    CMOD_ASSERT("cmod_cahv_posture", h != NULL);
    CMOD_ASSERT("cmod_cahv_posture", v != NULL);
    CMOD_ASSERT("cmod_cahv_posture", r != NULL);

    /* Compute the orientation, preserved handedness of vectors */
    hs = mag3(cross3(a, h, ah));	/* horizontal scale & center */
    hc = dot3(a, h);
    vs = mag3(cross3(a, v, av));	/* vertical   scale & center */
    vc = dot3(a, v);
    CMOD_ASSERT("cmod_cahv_posture", fabs(hs) > EPSILON);
    CMOD_ASSERT("cmod_cahv_posture", fabs(vs) > EPSILON);
    hp[0] = (h[0] - hc * a[0]) / hs;	/* unit projection of H in img plane */
    hp[1] = (h[1] - hc * a[1]) / hs;
    hp[2] = (h[2] - hc * a[2]) / hs;
    vp[0] = (v[0] - vc * a[0]) / vs;	/* unit projection of V in img plane */
    vp[1] = (v[1] - vc * a[1]) / vs;
    vp[2] = (v[2] - vc * a[2]) / vs;
    for (i=0; i<3; i++) {		/* rotation matrix from orthogonal */
	r[i][0] = hp[i];		/*   unit vectors                  */
	r[i][1] = a[i];
	r[i][2] = -vp[i];
	}
    }


/******************************************************************************
********************************   CMOD_CAHV_REFLECT   ************************
*******************************************************************************

    This function relocates a camera model, based on the image reflecting
    from a mirror defined by a plane. If the initial model is the true
    model, then the final model will be a virtual model representing how
    the camera sees the reflected world. If the initial model is the
    virtual model, then the final model will be the true model. */

void cmod_cahv_reflect(
    const cmod_float_t c_i[3],	/* input initial model center vector C */
    const cmod_float_t a_i[3],	/* input initial model axis   vector A */
    const cmod_float_t h_i[3],	/* input initial model horiz. vector H */
    const cmod_float_t v_i[3],	/* input initial model vert.  vector V */
    const cmod_float_t p[3],	/* input point on the reflecting plane */
    const cmod_float_t n[3],	/* input normal to the reflecting plane */
    cmod_float_t c_f[3],	/* output final model center vector C */
    cmod_float_t a_f[3],	/* output final model axis   vector A */
    cmod_float_t h_f[3],	/* output final model horiz. vector H */
    cmod_float_t v_f[3],	/* output final model vert.  vector V */
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

    /* Check input */
    CMOD_ASSERT("cmod_cahv_reflect", parallel != NULL);
    CMOD_ASSERT("cmod_cahv_reflect", behind   != NULL);

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
    copy3(c, c_f);
    copy3(a, a_f);
    copy3(h, h_f);
    copy3(v, v_f);
    }


/******************************************************************************
********************************   CMOD_CAHV_REFLECT_COV   ********************
*******************************************************************************

    This function reflects the covariance matrix to correspond to the
    reflected model. */

void cmod_cahv_reflect_cov(
    cmod_float_t s_i[12][12],	/* input initial covariance */
    const cmod_float_t n[3],	/* input normal to the reflecting plane */
    cmod_float_t s_f[12][12])	/* output final covariance */
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
    cmod_cahv_transform_cov(s_i, r, s_f);
    }


/******************************************************************************
********************************   CMOD_CAHV_ROT_COV   ************************
*******************************************************************************

    This function rotates a CAHV model's covariance matrix to correspond to
    rotation of the model itself. Note that model translations do not affect
    the covariance matrix. */

void cmod_cahv_rot_cov(
    cmod_float_t r_i[3][3],	/* input initial orientation of camera ref pt */
    cmod_float_t s_i[12][12],	/* input initial covariance */
    cmod_float_t r_f[3][3],	/* input final orientation of camera ref pt */
    cmod_float_t s_f[12][12])	/* output final covariance */
{
    cmod_float_t r[3][3];
    cmod_float_t r_it[3][3];

    /* Calculate the rotation, R, from the initial to the final orientation */
    trans33(r_i, r_it);
    mult333(r_f, r_it, r);
    cmod_cahv_transform_cov(s_i, r, s_f);
    }


/******************************************************************************
********************************   CMOD_CAHV_ROTATE_COV   *********************
*******************************************************************************

    This function rotates a CAHV model's covariance matrix to correspond to
    rotation of the model itself. Note that model translations do not affect
    the covariance matrix. */

void cmod_cahv_rotate_cov(
    const cmod_float_t q_i[4],	/* input initial orientation of camera ref pt */
    cmod_float_t s_i[12][12],	/* input initial covariance */
    const cmod_float_t q_f[4],	/* input final orientation of camera ref pt */
    cmod_float_t s_f[12][12])	/* output final covariance */
{
    cmod_float_t r_f[3][3];
    cmod_float_t r_i[3][3];

    rotq(q_f, r_f);
    rotq(q_i, r_i);
    cmod_cahv_rot_cov(r_i, s_i, r_f, s_f);
    }


/******************************************************************************
********************************   CMOD_CAHV_SCALE   **************************
*******************************************************************************

    This function scales a camera model. The scale factors should be
    understood as the same scale factors that would be applied to a 2D
    coordinate in the original model to convert it to a coordinate in
    the resulting model. Note that any precomputed internal model
    parameters will be made obsolete by this function. */

void cmod_cahv_scale(
    cmod_float_t hscale,	/* input horizontal scale factor */
    cmod_float_t vscale,	/* input vertical   scale factor */
    const cmod_float_t h1[3],	/* input  model horiz. vector H */
    const cmod_float_t v1[3],	/* input  model vert.  vector V */
    cmod_float_t s1[12][12],	/* input  covariance matrix, or NULL */
    cmod_float_t h2[3],		/* output model horiz. vector H */
    cmod_float_t v2[3],		/* output model vert.  vector V */
    cmod_float_t s2[12][12])	/* output covariance matrix, or NULL */
{
    cmod_int_t i;
    cmod_int_t j;

    /* Check input */
    CMOD_ASSERT("cmod_cahv_scale", h1 != NULL);
    CMOD_ASSERT("cmod_cahv_scale", v1 != NULL);
    CMOD_ASSERT("cmod_cahv_scale", h2 != NULL);
    CMOD_ASSERT("cmod_cahv_scale", v2 != NULL);

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
	for (i=0; i<12; i++) {
	    for (j=0; j<12; j++) {
		s2[i][j] = s1[i][j];
		}
	    }
	}
    for (i=0; i<12; i++) {
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
********************************   CMOD_CAHV_SHIFT   **************************
*******************************************************************************

    This function shifts a camera model. The shift values should be
    understood as the coordinates of the old model where the origin
    will fall in the new one. Note that any precomputed internal model
    parameters will be made obsolete by this function. */

void cmod_cahv_shift(
    cmod_float_t dx,		/* input horizontal shift */
    cmod_float_t dy,		/* input vertical   shift */
    const cmod_float_t a1[3],	/* input  model axis   vector A */
    const cmod_float_t h1[3],	/* input  model horiz. vector H */
    const cmod_float_t v1[3],	/* input  model vert.  vector V */
    cmod_float_t s1[12][12],	/* input  covariance matrix, or NULL */
    cmod_float_t h2[3],		/* output model horiz. vector H */
    cmod_float_t v2[3],		/* output model vert.  vector V */
    cmod_float_t s2[12][12])	/* output covariance matrix, or NULL */
{
    cmod_int_t i;
    cmod_int_t j;

    /* Check input */
    CMOD_ASSERT("cmod_cahv_shift", a1 != NULL);
    CMOD_ASSERT("cmod_cahv_shift", h1 != NULL);
    CMOD_ASSERT("cmod_cahv_shift", v1 != NULL);
    CMOD_ASSERT("cmod_cahv_shift", h2 != NULL);
    CMOD_ASSERT("cmod_cahv_shift", v2 != NULL);

    /* Shift the model */
    h2[0] = h1[0]  -  dx * a1[0];
    h2[1] = h1[1]  -  dx * a1[1];
    h2[2] = h1[2]  -  dx * a1[2];
    v2[0] = v1[0]  -  dy * a1[0];
    v2[1] = v1[1]  -  dy * a1[1];
    v2[2] = v1[2]  -  dy * a1[2];

    /* Optionally shift the covariance */
    if ((s1 == NULL) || (s2 == NULL)) {
	return;
	}
    if (s1 != s2) {
	for (i=0; i<12; i++) {
	    for (j=0; j<12; j++) {
		s2[i][j] = s1[i][j];
		}
	    }
	}
    for (i=0; i<12; i++) {
	s2[ 6][i]  -=  dx * s2[3][i];
	s2[ 7][i]  -=  dx * s2[4][i];
	s2[ 8][i]  -=  dx * s2[5][i];
	s2[ 9][i]  -=  dy * s2[3][i];
	s2[10][i]  -=  dy * s2[4][i];
	s2[11][i]  -=  dy * s2[5][i];
	}
    for (i=0; i<12; i++) {
	s2[i][ 6]  -=  dx * s2[i][3];
	s2[i][ 7]  -=  dx * s2[i][4];
	s2[i][ 8]  -=  dx * s2[i][5];
	s2[i][ 9]  -=  dy * s2[i][3];
	s2[i][10]  -=  dy * s2[i][4];
	s2[i][11]  -=  dy * s2[i][5];
	}
    }


/******************************************************************************
********************************   CMOD_CAHV_TRANSFORM_COV   ******************
*******************************************************************************

    This function transform a CAHV model's covariance matrix according to a
    3x3 matrix that represents the transformation to the 3D coordinates of
    the model. Note that model translations do not affect the covariance
    matrix. */

void cmod_cahv_transform_cov(
    cmod_float_t s_i[12][12],	/* input initial covariance */
    cmod_float_t r[3][3],	/* input transform matrix of camera ref pt */
    cmod_float_t s_f[12][12])	/* output final covariance */
{
    cmod_int_t i;
    cmod_int_t j;
    cmod_int_t k;
    cmod_float_t d;
    cmod_float_t r12[12][12];
    cmod_float_t r12t[12][12];
    cmod_float_t stemp[12][12];

    /* Check input */
    CMOD_ASSERT("cmod_cahv_transform_cov", s_i != NULL);
    CMOD_ASSERT("cmod_cahv_transform_cov", r   != NULL);
    CMOD_ASSERT("cmod_cahv_transform_cov", s_f != NULL);

    /* Contruct a matrix, R12, (and its transpose) to rotate the covariance
	|R   |
	| R  |
	|  R |
	|   R|
    */
    for (i=0; i<12; i++) {
	for (j=0; j<12; j++) {
	    r12[i][j] = 0;
	    }
	}
    for (i=0; i<3; i++) {
	for (j=0; j<3; j++) {
	    r12[i+0][j+0] = r[i][j];
	    r12[i+3][j+3] = r[i][j];
	    r12[i+6][j+6] = r[i][j];
	    r12[i+9][j+9] = r[i][j];
	    }
	}
    for (i=0; i<12; i++) {
	for (j=0; j<12; j++) {
	    r12t[i][j] = r12[j][i];
	    }
	}

    /* Pre-multiply by the matrix */
    for (i=0; i<12; i++) {
	for (j=0; j<12; j++) {
	    d = 0;
	    for (k=0; k<12; k++) {
		d += r12[i][k] * s_i[k][j];
		}
	    stemp[i][j] = d;
	    }
	}

    /* Post-multiply by the transpose */
    for (i=0; i<12; i++) {
	for (j=0; j<12; j++) {
	    d = 0;
	    for (k=0; k<12; k++) {
		d += stemp[i][k] * r12t[k][j];
		}
	    s_f[i][j] = d;
	    }
	}
    }


/******************************************************************************
********************************   CMOD_CAHV_VALIDATE   ***********************
*******************************************************************************

    This function validates the components of a camera model to determine
    whether or not the model makes some kind of sense. Only generic
    sanity-checking tests are performed. */

cmod_stat_t cmod_cahv_validate(
    const cmod_float_t c[3],	/* input model center vector C */
    const cmod_float_t a[3],	/* input model axis   vector A */
    const cmod_float_t h[3],	/* input model horiz. vector H */
    const cmod_float_t v[3])	/* input model vert.  vector V */
{
    cmod_float_t mag;
    const cmod_float_t ulower = 0.9; /* lower magnitude limit for unit vector */
    const cmod_float_t uupper = 1.1; /* upper magnitude limit for unit vector */

    /* Check axis vector */
    mag = mag3(a);
    if ((mag < ulower) || (mag > uupper)) {
	CMOD_ERROR("cmod_cahv_validate", "Bad A vector");
        return FAILURE;
        }

    /* It would be nice to validate that the H and V vectors are not zero,  */
    /* but I'm not sure what minimum magnitude to insist upon. They include */
    /* components that represent the focal length and image center in units */
    /* of horizontal or vertical pixels, but a wide range of values is      */
    /* theoretically possible in the general case.                          */

    return SUCCESS;
    }


/******************************************************************************
********************************   CMOD_CAHV_WARP_MODELS   ********************
*******************************************************************************

    This function warps a pair of almost aligned camera models into a
    perfectly aligned stereo pair of virtual camera models. The virtual
    models will have their original C vectors, but will share newly
    computed A, H, and V vectors, as well as internal model parameters.
    Note that image warping will be necessary in order to use the new models.
    This function is based on the paper "Stereo images warping and
    triangulation," by Pierrick Grandjean, 20 Nov 1992. */

void cmod_cahv_warp_models(
    const cmod_float_t c1[3],	/* input model 1 center vector C */
    const cmod_float_t a1[3],	/* input model 1 axis   vector A */
    const cmod_float_t h1[3],	/* input model 1 horiz. vector H */
    const cmod_float_t v1[3],	/* input model 1 vert.  vector V */
    const cmod_float_t c2[3],	/* input model 2 center vector C */
    const cmod_float_t a2[3],	/* input model 2 axis   vector A */
    const cmod_float_t h2[3],	/* input model 2 horiz. vector H */
    const cmod_float_t v2[3],	/* input model 2 vert.  vector V */
    cmod_float_t a[3],		/* output virtual model axis   vector A */
    cmod_float_t h[3],		/* output virtual model horiz. vector H */
    cmod_float_t v[3],		/* output virtual model vert.  vector V */
    cmod_float_t *hs,		/* output horizontal scale factor */
    cmod_float_t *hc,		/* output horizontal center */
    cmod_float_t *vs,		/* output vertical scale factor */
    cmod_float_t *vc,		/* output vertical center */
    cmod_float_t *theta)	/* output angle between axes */
{
    cmod_float_t ap[3];
    cmod_float_t app[3];
    cmod_float_t f[3];
    cmod_float_t g[3];
    cmod_float_t hp[3];
    cmod_float_t vp[3];

    /* Check input */
    CMOD_ASSERT("cmod_cahv_shift", hc    != NULL);
    CMOD_ASSERT("cmod_cahv_shift", vc    != NULL);
    CMOD_ASSERT("cmod_cahv_shift", hs    != NULL);
    CMOD_ASSERT("cmod_cahv_shift", vs    != NULL);
    CMOD_ASSERT("cmod_cahv_shift", theta != NULL);

    /* Compute a common image center and scale for the two models */
    *hc = (dot3(h1, a1) + dot3(h2, a2)) / 2.0;
    *vc = (dot3(v1, a1) + dot3(v2, a2)) / 2.0;
    *hs = (mag3(cross3(a1, h1, f))		/* Pierrick's equation   */
	 + mag3(cross3(a2, h2, g))) / 2.0;	/*  was unclear:         */
    *vs = (mag3(cross3(a1, v1, f))		/*  clarified from       */
	 + mag3(cross3(a2, v2, g))) / 2.0;	/*  cmod_cahv_internal() */
    *theta = -PI / 2.0;

    /* Use common center and scale to construct common A, H, V */
    add3(a1, a2, app);
    sub3(c2, c1, f);
    /* breaks epipolar alignment...
    cross3(app, f, g);	/+ alter f (CxCy) to be         +/
    cross3(g, app, f);	/+   perpendicular to average A +/
    ...*/
    CMOD_ASSERT("cmod_cahv_warp_models", fabs(mag3(f)) > EPSILON);
    CMOD_ASSERT("cmod_cahv_warp_models", fabs(*hs)     > EPSILON);
    if (dot3(f, h1) > 0) {
	scale3((*hs)/mag3(f), f, hp);
	}
    else {
	scale3(-(*hs)/mag3(f), f, hp);
	}
    scale3(0.5, app, app);
    scale3(dot3(app, hp)/((*hs) * (*hs)), hp, g);
    sub3(app, g, ap);
    unit3(ap, a);
    cross3(a, hp, f);
    scale3((*vs)/(*hs), f, vp);
    scale3(*hc, a, f);
    add3(hp, f, h);
    scale3(*vc, a, f);
    add3(vp, f, v);
    }


/******************************************************************************
********************************   CMOD_CAHV_WARP_TO_CAHV   *******************
*******************************************************************************

    This function takes an image coordinate which resulted from a camera
    modeled by CAHV and warps it into an image coordinate modeled by
    different CAHV. */

void cmod_cahv_warp_to_cahv(
    const cmod_float_t c1[3],	/* input initial model center vector C */
    const cmod_float_t a1[3],	/* input initial model axis   vector A */
    const cmod_float_t h1[3],	/* input initial model horiz. vector H */
    const cmod_float_t v1[3],	/* input initial model vert.  vector V */
    const cmod_float_t pos1[2],	/* input 2D position from CAHV */
    const cmod_float_t c2[3],	/* input final model center vector C */
    const cmod_float_t a2[3],	/* input final model axis   vector A */
    const cmod_float_t h2[3],	/* input final model horiz. vector H */
    const cmod_float_t v2[3],	/* input final model vert.  vector V */
    cmod_float_t pos2[2])	/* output 2D position for CAHV */
{
    cmod_float_t p3[3];
    cmod_float_t range;
    cmod_float_t u3[3];

    cmod_cahv_2d_to_3d(pos1, c1, a1, h1, v1, p3, u3,
					(cmod_float_t (*)[2])NULL);
    add3(p3, u3, p3);
    cmod_cahv_3d_to_2d(p3, c2, a2, h2, v2, &range, pos2,
					(cmod_float_t (*)[3])NULL);
    }
