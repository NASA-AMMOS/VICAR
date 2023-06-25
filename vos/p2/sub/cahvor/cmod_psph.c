/******************************************************************************
*                                                                             *
*                                    P S P H                                  *
*                                                                             *
*                                       Todd Litwin                           *
*                                       Written:  5 Aug 2002                  *
*                                       Updated: 10 Aug 2016                  *
*                                                                             *
*                                       Copyright (C) 2002, 2016              *
*                                       California Institute of Technology    *
*                                       All Rights Reserved                   *
*                                                                             *
*******************************************************************************


	This file contains functions for using the Planospheric camera
	model. This model was designed by the author for the purpose of
	supporting one-dimensional searches for correlation matches between
	a pair of images from cameras with fisheye-like lenses. It is based
	on a pin-hole lens and a spherical image surface. The rows and
	columns are formed by intersecting planes with the sphere.

	The geometry behind this model has limitations. There are
	degenerate orientations of the planes such that projection from 3D
	to 2D will fail. These occur if the planes are rotated such that
	one plane contains the other one's rotation axis. With rotation
	axes that are orthonal to each other, and where the nominal forward
	or camera-boresight direction is to be taken as being at right
	angles to those axes, then the degenerate orientations occur when
	rotating plus or minus 90 degrees from forward. In such cases one
	should restrict the camera fields of view to less than 180 degrees.
	*/


#include <math.h>
#include <mat3.h>

#include "cmod_psph.h"
#include "cmod_error.h"

#ifndef EPSILON
#define EPSILON (1e-15)
#endif

enum {
    SUCCESS =  0,
    FAILURE = -1
    };


/******************************************************************************
********************************   CMOD_PSPH_2D_TO_3D   ***********************
*******************************************************************************

    This function projects a 2D image point out into 3D using the
    camera model parameters provided. In addition to the 3D projection,
    it outputs and the partial-derivative matrix of the unit vector of the
    projection with respect to the 2D image-plane point. If the parameter
    for the output partial matrix is passed as NULL, then it will not be
    calculated. */

void cmod_psph_2d_to_3d(
    const cmod_float_t pos2[2],	/* input 2D position */
    const cmod_psph_t *psph,	/* input camera model */
    cmod_float_t pos3[3],	/* output 3D origin of projection */
    cmod_float_t uvec3[3],	/* output unit vector ray of projection */
    cmod_float_t par[3][2])	/* output partial-derivative uvec3/pos2 */
{
    cmod_float_t m[3];
    cmod_float_t magv;
    cmod_float_t nx[3];
    cmod_float_t ny[3];
    /*...
    cmod_float_t q[4];
    cmod_float_t r[3][3];
    ...*/
    cmod_float_t v[3];
    cmod_float_t sgn;
    cmod_float_t t1[3];
    cmod_float_t t2[3];
    cmod_float_t theta;
    cmod_float_t vv;

    /* Check input */
    CMOD_ASSERT("cmod_psph_2d_to_3d", pos2  != NULL);
    CMOD_ASSERT("cmod_psph_2d_to_3d", psph  != NULL);
    CMOD_ASSERT("cmod_psph_2d_to_3d", pos3  != NULL);
    CMOD_ASSERT("cmod_psph_2d_to_3d", uvec3 != NULL);

    /* The spherical center is the projection point */
    copy3(psph->c, pos3);

    /* Rotate the planes for the given 2D point */
    /*...
    quatva(psph->ax, pos2[0] * psph->sx, q);
    rotq(q, r);
    mult331(r, psph->nx, nx);
    quatva(psph->ay, pos2[1] * psph->sy, q);
    rotq(q, r);
    mult331(r, psph->ny, ny);
    ...*/

    /* The above vector rotations about the axes can be done  */
    /* more efficiently by using Rodriguez' Rotation Formula, */
    /* especially after simplifying for our use of orthogonal */
    /* unit vectors.                                          */

    /* Rotate the column plane to the 2D point */
    cross3(psph->ax, psph->nx, m);
    theta = pos2[0] * psph->sx;
    scale3(cos(theta), psph->nx, t1);
    scale3(sin(theta), m, t2);
    add3(t1, t2, nx);

    /* Rotate the row plane to the 2D point */
    cross3(psph->ay, psph->ny, m);
    theta = pos2[1] * psph->sy;
    scale3(cos(theta), psph->ny, t1);
    scale3(sin(theta), m, t2);
    add3(t1, t2, ny);

    /* Compute the unit vector of projection */
    if (psph->sx > 0) {	// regular model
	sgn = 1.0;
	cross3(nx, ny, v);
	}
    else {		// reflected model
	sgn = -1.0;
	cross3(ny, nx, v);
	}
    vv = dot3(v, v);
    magv = sqrt(vv);
    CMOD_ASSERT("cmod_psph_2d_to_3d", magv > EPSILON);
    scale3(1.0/magv, v, uvec3);

    /* Optionally calculate the partial of uvec3 with respect to pos2 */
    if (par != NULL) {
	cmod_float_t dfdx[3];
	cmod_float_t dfdy[3];
	cmod_float_t dudx[3];
	cmod_float_t dudy[3];

	cross3(psph->ax, nx, t1);
	cross3(t1, ny, t2);
	scale3(psph->sx, t2, dfdx);

	cross3(psph->ay, ny, t1);
	cross3(nx, t1, t2);
	scale3(psph->sy, t2, dfdy);

	scale3(magv, dfdx, t1);
	scale3(dot3(dfdx, v), uvec3, t2);
	sub3(t1, t2, t2);
	scale3(1/vv, t2, dudx);

	scale3(magv, dfdy, t1);
	scale3(dot3(dfdy, v), uvec3, t2);
	sub3(t1, t2, t2);
	scale3(1/vv, t2, dudy);

	par[0][0] = sgn * dudx[0];
	par[1][0] = sgn * dudx[1];
	par[2][0] = sgn * dudx[2];
	par[0][1] = sgn * dudy[0];
	par[1][1] = sgn * dudy[1];
	par[2][1] = sgn * dudy[2];
	}
    }


/******************************************************************************
********************************   CMOD_PSPH_3D_TO_2D   ***********************
*******************************************************************************

    This function projects a 3D point into the image plane using the
    camera model parameters provided. In addition to the 2D projection,
    it outputs the 3D perpendicular distance from the camera to the
    3D point, and the partial derivative matrix of the 2D point with respect
    to the 3D point. If the parameter for the output partial matrix is
    passed as NULL, then it will not be calculated. */

void cmod_psph_3d_to_2d(
    const cmod_float_t pos3[3],	/* input 3D position */
    const cmod_psph_t *psph,	/* input camera model */
    cmod_float_t pos2[2],	/* output 2D image-plane projection */
    cmod_float_t par[2][3])	/* output partial-derivative pos2/pos3 */
{
    cmod_float_t mag;
    cmod_float_t nx[3];
    cmod_float_t ny[3];
    cmod_float_t p_c[3];
    cmod_float_t s;
    cmod_float_t sgn;
    cmod_float_t c;
    cmod_float_t u[3];

    /* Check input */
    CMOD_ASSERT("cmod_psph_3d_to_2d", pos3  != NULL);
    CMOD_ASSERT("cmod_psph_3d_to_2d", psph  != NULL);
    CMOD_ASSERT("cmod_psph_3d_to_2d", pos2  != NULL);
    CMOD_ASSERT("cmod_psph_3d_to_2d", (psph->sx >  EPSILON) ||
				      (psph->sx < -EPSILON));
    CMOD_ASSERT("cmod_psph_3d_to_2d", (psph->sy >  EPSILON) ||
				      (psph->sy < -EPSILON));

    /* Compute the normals */
    sub3(pos3, psph->c, p_c);
    if (psph->sx > 0) {	// regular model
	sgn = 1.0;
	cross3(psph->ax, p_c, nx);
	cross3(psph->ay, p_c, ny);
	}
    else {		// reflected model
	sgn = -1.0;
	cross3(p_c, psph->ax, nx);
	cross3(p_c, psph->ay, ny);
	}
    mag = mag3(nx);
    CMOD_ASSERT("cmod_psph_3d_to_2d", mag > EPSILON);
    scale3(1/mag, nx, nx);
    mag = mag3(ny);
    CMOD_ASSERT("cmod_psph_3d_to_2d", mag > EPSILON);
    scale3(1/mag, ny, ny);

    /* Compute the 2d coordinates */
    c = dot3(psph->nx, nx);
    s = dot3(cross3(psph->nx, nx, u), psph->ax);
    pos2[0] = atan2(s, c) / psph->sx;
    c = dot3(psph->ny, ny);
    s = dot3(cross3(psph->ny, ny, u), psph->ay);
    pos2[1] = atan2(s, c) / psph->sy;

    /* Optionally calculate the partial of pos2 with respect to pos3 */
    if (par != NULL) {

	scale3(dot3(p_c, psph->ax), psph->ax, u);
	sub3(p_c, u, u);
	mag = mag3(u);	/* orthogonal distance from p to ax */
	CMOD_ASSERT("cmod_psph_3d_to_2d", mag > EPSILON);
	scale3(sgn/(mag*psph->sx), nx, par[0]);

	scale3(dot3(p_c, psph->ay), psph->ay, u);
	sub3(p_c, u, u);
	mag = mag3(u);	/* orthogonal distance from p to ay */
	CMOD_ASSERT("cmod_psph_3d_to_2d", mag > EPSILON);
	scale3(sgn/(mag*psph->sy), ny, par[1]);
	}
    }


/******************************************************************************
********************************   CMOD_PSPH_CREATE   *************************
*******************************************************************************

    This function creates a camera model based on input geometric
    specifications. The XA and YA vectors define the rotation axes for the
    planes whose intersection selects the pixel on the sphere. While
    nominally they would be orthogonal to each other, they do not need to
    to be. Nor do they need to have unit length, but may have any
    reasonable magnitude.

    The XA and YA axis vectors should be oriented for increasing columns
    and row index when rotating by a positive angle, as defined by the
    right-hand rule. */

cmod_stat_t cmod_psph_create(
    const cmod_float_t pos[3],	/* input 3D position */
    const cmod_float_t xa[3],	/* input rotation axis for the columns */
    const cmod_float_t xv1[3],	/* input X projection vector #1 */
    const cmod_float_t xv2[3],	/* input X projection vector #2 */
    cmod_float_t xc1,		/* input X image coord to match vector #1 */
    cmod_float_t xc2,		/* input X image coord to match vector #2 */
    const cmod_float_t ya[3],	/* input rotation axis for the rows */
    const cmod_float_t yv1[3],	/* input Y projection vector #1 */
    const cmod_float_t yv2[3],	/* input Y projection vector #2 */
    cmod_float_t yc1,		/* input Y image coord to match vector #1 */
    cmod_float_t yc2,		/* input Y image coord to match vector #2 */
    cmod_psph_t *psph)		/* output camera model */
{
    cmod_float_t c;
    cmod_float_t mag;
    cmod_float_t n1[3];
    cmod_float_t n2[3];
    cmod_float_t q[4];
    cmod_float_t r[3][3];
    cmod_float_t s;
    cmod_float_t v[3];

    /* Check input */
    CMOD_ASSERT("cmod_psph_create", pos  != NULL);
    CMOD_ASSERT("cmod_psph_create", xa   != NULL);
    CMOD_ASSERT("cmod_psph_create", xv1  != NULL);
    CMOD_ASSERT("cmod_psph_create", xv2  != NULL);
    CMOD_ASSERT("cmod_psph_create", ya   != NULL);
    CMOD_ASSERT("cmod_psph_create", yv1  != NULL);
    CMOD_ASSERT("cmod_psph_create", yv2  != NULL);
    CMOD_ASSERT("cmod_psph_create", psph != NULL);
    if (fabs(xc1 - xc2) < EPSILON) {
	CMOD_ERROR("cmod_psph_create", "xc1 and xc2 too near each other");
	return FAILURE;
	}
    if (fabs(yc1 - yc2) < EPSILON) {
	CMOD_ERROR("cmod_psph_create", "yc1 and yc2 too near each other");
	return FAILURE;
	}

    /* Record the inputs that map directly to the output */
    copy3(pos, psph->c);
    unit3(xa,  psph->ax);
    unit3(ya,  psph->ay);

    /* Compute the X plane's normal vector at column #1 */
    cross3(xa, xv1, n1);
    mag = mag3(n1);
    if (mag < EPSILON) {
	CMOD_ERROR("cmod_psph_create", "xv1 too near rotation axis");
	return FAILURE;
	}
    scale3(1/mag, n1, n1);

    /* Compute the X plane's normal vector at column #2 */
    cross3(xa, xv2, n2);
    mag = mag3(n2);
    if (mag < EPSILON) {
	CMOD_ERROR("cmod_psph_create", "xv2 too near rotation axis");
	return FAILURE;
	}
    scale3(1/mag, n2, n2);

    /* Compute the X angular scale */
    c = dot3(n1, n2);
    s = dot3(cross3(n1, n2, v), psph->ax);
    psph->sx = atan2(s, c) / (xc2 - xc1);

    /* Compute the X plane's normal at reference column (0) */
    quatva(psph->ax, (-xc1 * psph->sx), q);
    rotq(q, r);
    mult331(r, n1, psph->nx);
    unit3(psph->nx, psph->nx);

    /* Compute the Y plane's normal vector at row #1 */
    cross3(ya, yv1, n1);
    mag = mag3(n1);
    if (mag < EPSILON) {
	CMOD_ERROR("cmod_psph_create", "yv1 too near rotation axis");
	return FAILURE;
	}
    scale3(1/mag, n1, n1);

    /* Compute the Y plane's normal vector at row #2 */
    cross3(ya, yv2, n2);
    mag = mag3(n2);
    if (mag < EPSILON) {
	CMOD_ERROR("cmod_psph_create", "yv2 too near rotation axis");
	return FAILURE;
	}
    scale3(1/mag, n2, n2);

    /* Compute the Y angular scale */
    c = dot3(n1, n2);
    s = dot3(cross3(n1, n2, v), psph->ay);
    psph->sy = atan2(s, c) / (yc2 - yc1);

    /* Compute the Y plane's normal at reference row (0) */
    quatva(psph->ay, (-yc1 * psph->sy), q);
    rotq(q, r);
    mult331(r, n1, psph->ny);
    unit3(psph->ny, psph->ny);

    return SUCCESS;
    }


/******************************************************************************
********************************   CMOD_PSPH_CREATE2   ************************
*******************************************************************************

    This function generates a planosperical camera model to match the input
    parameter values. The model will have the zero pixel in the upper, left
    corner. Note that the fwd, up, and rt vectors are with respect to the
    point described as the image center. */

cmod_stat_t cmod_psph_create2(
    const cmod_float_t pos[3],	/* input 3D projection point */
    const cmod_float_t fwd[3],	/* input forward-pointing vector */
    const cmod_float_t up[3],	/* input upward-pointing vector */
    const cmod_float_t rt[3],	/* input right-pointing vector */
    cmod_float_t xfov,	  	/* input X field of view (rad) */
    cmod_float_t yfov,		/* input Y field of view (rad) */
    cmod_float_t xdim,		/* input X image dimension */
    cmod_float_t ydim,		/* input Y image dimension */
    cmod_float_t xc,		/* input X coordinate of image center */
    cmod_float_t yc,		/* input Y coordinate of image center */
    cmod_psph_t *psph)		/* output camera model */
{
    cmod_float_t quat[4];
    cmod_float_t rot[3][3];
    cmod_float_t f[3];
    cmod_float_t xa[3];
    cmod_float_t xv1[3];
    cmod_float_t xv2[3];
    cmod_float_t xc1;
    cmod_float_t xc2;
    cmod_float_t ya[3];
    cmod_float_t yv1[3];
    cmod_float_t yv2[3];
    cmod_float_t yc1;
    cmod_float_t yc2;

    /* Check input */
    CMOD_ASSERT("cmod_psph_create2", pos  != NULL);
    CMOD_ASSERT("cmod_psph_create2", fwd  != NULL);
    CMOD_ASSERT("cmod_psph_create2", up   != NULL);
    CMOD_ASSERT("cmod_psph_create2", psph != NULL);
    CMOD_ASSERT("cmod_psph_create2", xfov > 0);
    CMOD_ASSERT("cmod_psph_create2", yfov > 0);
    CMOD_ASSERT("cmod_psph_create2", xdim > 0);
    CMOD_ASSERT("cmod_psph_create2", ydim > 0);

    /* Choose the rotation axes */
    scale3(-1, up, xa);
    scale3(-1, rt, ya);
    unit3(xa, xa);
    unit3(ya, ya);
    unit3(fwd, f);

    /* Select the X projections to describe the model */
    quatva(xa, -xfov/2, quat);
    rotq(quat, rot);
    mult331(rot, f, xv1);
    xc1 = xc - (xdim-1)/2;
    quatva(xa, xfov/2, quat);
    rotq(quat, rot);
    mult331(rot, f, xv2);
    xc2 = xc + (xdim-1)/2;

    /* Select the Y projections to describe the model */
    quatva(ya, -yfov/2, quat);
    rotq(quat, rot);
    mult331(rot, f, yv1);
    yc1 = yc - (ydim-1)/2;
    quatva(ya, yfov/2, quat);
    rotq(quat, rot);
    mult331(rot, f, yv2);
    yc2 = yc + (ydim-1)/2;

    /* Create the model */
    return cmod_psph_create(pos, xa, xv1, xv2, xc1, xc2,
				 ya, yv1, yv2, yc1, yc2, psph);
    }


/******************************************************************************
********************************   CMOD_PSPH_INTERNAL   ***********************
*******************************************************************************

    This function calculates the internal camera model parameters for the
    PSPH camera model. Note: the main purpose for providing this function
    is to follow the pattern established for the other model classes, not
    because there is much benefit to having it in and of itself. */

void cmod_psph_internal(
    const cmod_psph_t *psph,	/* input camera model */
    cmod_float_t *theta)	/* output angle between axes */
{
    cmod_float_t c;
    cmod_float_t s;
    cmod_float_t v[3];

    /* Check input */
    CMOD_ASSERT("cmod_psph_internal", psph  != NULL);
    CMOD_ASSERT("cmod_psph_internal", theta != NULL);

    /* Calculate theta */
    c = dot3(psph->ax, psph->ay);
    s = mag3(cross3(psph->ax, psph->ay, v));
    *theta = atan2(s, c);
    }


/******************************************************************************
********************************   CMOD_PSPH_IPLANE   *************************
*******************************************************************************

    This function computes information that roughly corresponds to the image
    plane and related quantities that are really only appropriate to the more
    traditional camera models. It does so by accepting an arbitrary "image
    center" point at which to place a ficticious "image plane" that is
    constructed tangent to this model's projection sphere. The function then
    computes the unit outward-facing normal to the plane along with in-plane
    unit vectors in the directions of increasing column and row dimensions
    (which need not be mutually orthogonal). It provides the projection point
    through which image-plane points are projected.

    Note that the in-plane unit vectors are constructed as if the "image" plane
    lies between the projection point and the scene being viewed. While this is
    not the physical case for real cameras, the image planes for which lie
    behind the lens and receive an inverted image from light that passes
    through it, this simplifies thinking about the geometry. */

void cmod_psph_iplane(
    const cmod_psph_t *psph,	/* input camera model */
    cmod_float_t xc,		/* input X coordinate of image center */
    cmod_float_t yc,		/* input Y coordinate of image center */
    cmod_float_t ppnt[3],	/* output projection point */
    cmod_float_t ndir[3],	/* output normal direction */
    cmod_float_t xdir[3],	/* output column direction */
    cmod_float_t ydir[3])	/* output row direction */
{
    cmod_float_t p2[2];
    cmod_float_t p32[3][2];
    cmod_float_t u[3];

    /* Check input */
    CMOD_ASSERT("cmod_psph_iplane", psph != NULL);
    CMOD_ASSERT("cmod_psph_iplane", ppnt != NULL);
    CMOD_ASSERT("cmod_psph_iplane", ndir != NULL);
    CMOD_ASSERT("cmod_psph_iplane", xdir != NULL);
    CMOD_ASSERT("cmod_psph_iplane", ydir != NULL);

    /* Compute the in-plane coordinate direction vectors */
    p2[0] = xc;
    p2[1] = yc;
    cmod_psph_2d_to_3d(p2, psph, ppnt, ndir, p32);
    u[0] = p32[0][0];
    u[1] = p32[1][0];
    u[2] = p32[2][0];
    unit3(u, xdir);
    u[0] = p32[0][1];
    u[1] = p32[1][1];
    u[2] = p32[2][1];
    unit3(u, ydir);
    }


/******************************************************************************
********************************   CMOD_PSPH_MOVE   ***************************
*******************************************************************************

    This function relocates a camera model, based on the initial and final
    positions and orientations of a camera platform reference point, a point
    that is rigidly connected to the camera, but is otherwise arbitrary. */

void cmod_psph_move(
    const cmod_float_t p_i[3],	/* input initial pos of camera ref pt */
    const cmod_float_t q_i[4],	/* input initial orientation of camera ref pt */
    const cmod_psph_t *psph_i,	/* input camera model */
    const cmod_float_t p_f[3],	/* input final pos of camera ref pt */
    const cmod_float_t q_f[4],	/* input final orientation of camera ref pt */
    cmod_psph_t *psph_f)	/* output camera model */
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
    sub3(psph_i->c, p_i, d);		/* delta vector from P_i to C_i */
    mult331(r, d, psph_f->c);		/* rotate delta vector */
    add3(psph_f->c, p_f, psph_f->c);	/* reposition C_f from P_f */

    /* Rotate the other vectors */
    mult331(r, psph_i->ax, psph_f->ax);
    mult331(r, psph_i->ay, psph_f->ay);
    mult331(r, psph_i->nx, psph_f->nx);
    mult331(r, psph_i->ny, psph_f->ny);

    /* Copy unchanged values */
    psph_f->sx = psph_i->sx;
    psph_f->sy = psph_i->sy;
    }


/******************************************************************************
********************************   CMOD_PSPH_POSE   ***************************
*******************************************************************************

    This function returns the position and rotation matrix of orientation for
    the given model. The absolute orientation is based on a reference
    orientation of the camera pointing straight down the Y axis, with Z up.
    Unlike for perspective-projection models, where the pointing direction is
    taken as the normal to the image plane (an approximation), some other
    reference must be used here. The input X and Y center coordinates fill
    that need here. */

void cmod_psph_pose(
    const cmod_psph_t *psph,	/* input camera model */
    cmod_float_t xc,		/* input X coordinate of image center */
    cmod_float_t yc,		/* input Y coordinate of image center */
    cmod_float_t p[3],		/* output position vector */
    cmod_float_t r[3][3])	/* output rotation matrix */
{
    cmod_int_t i;
    cmod_float_t ndir[3];
    cmod_float_t xdir[3];
    cmod_float_t ydir[3];

    /* Check input */
    CMOD_ASSERT("cmod_psph_pose", psph != NULL);
    CMOD_ASSERT("cmod_psph_pose", p    != NULL);
    CMOD_ASSERT("cmod_psph_pose", r    != NULL);

    /* Compute the position and orientation, forcing vertical to dominate */
    cmod_psph_iplane(psph, xc, yc, p, ndir, xdir, ydir);
    cross3(ydir, ndir, xdir);		/* force vertical to dominate */
    unit3(xdir, xdir);
    for (i=0; i<3; i++) {		/* rotation matrix from orthogonal */
	r[i][0] = xdir[i];		/*   unit vectors                  */
	r[i][1] = ndir[i];
	r[i][2] = -ydir[i];
	}
    }


/******************************************************************************
********************************   CMOD_PSPH_REFLECT   ************************
*******************************************************************************

    This function relocates a camera model, based on the image reflecting
    from a mirror defined by a plane. If the initial model is the true
    model, then the final model will be a virtual model representing how
    the camera sees the reflected world. If the initial model is the
    virtual model, then the final model will be the true model. */

void cmod_psph_reflect(
    const cmod_psph_t *psph_i,	/* input camera model */
    cmod_float_t xc,		/* input X coordinate of image center */
    cmod_float_t yc,		/* input Y coordinate of image center */
    const cmod_float_t p[3],	/* input point on the reflecting plane */
    const cmod_float_t n[3],	/* input normal to the reflecting plane */
    cmod_psph_t *psph_f,	/* output camera model */
    cmod_bool_t *parallel,	/* output if camera view & plane are parallel */
    cmod_bool_t *behind)	/* output if camera behind reflecting plane */
{
    cmod_float_t d;
    cmod_float_t k;
    cmod_float_t nu[3];
    cmod_float_t p2[2];
    cmod_float_t p3[3];
    cmod_float_t u3[3];
    cmod_float_t u3r[3];
    cmod_float_t u[3];

    /* Check input */
    CMOD_ASSERT("cmod_psph_reflect", psph_i   != NULL);
    CMOD_ASSERT("cmod_psph_reflect", p        != NULL);
    CMOD_ASSERT("cmod_psph_reflect", n        != NULL);
    CMOD_ASSERT("cmod_psph_reflect", psph_f   != NULL);
    CMOD_ASSERT("cmod_psph_reflect", parallel != NULL);
    CMOD_ASSERT("cmod_psph_reflect", behind   != NULL);

    /* Get the pointing direction */
    p2[0] = xc;
    p2[1] = yc;
    cmod_psph_2d_to_3d(p2, psph_i, p3, u3, NULL);

    /* Check if the camera view and plane are parallel (or nearly so) */
    unit3(n, nu);
    k = dot3(u3, nu);
    if (fabs(k) < EPSILON) {
	*parallel = TRUE;
	*behind   = FALSE;
	return;
	}
    *parallel = FALSE;

    /* Compute the reflected direction vector */
    scale3(-2*k, nu, u3r);
    add3(u3, u3r, u3r);

    /* Compute the reflected Ax vector */
    scale3(-2*dot3(psph_i->ax, nu), nu, u);
    add3(u, psph_i->ax, psph_f->ax);

    /* Compute the reflected Ay vector */
    scale3(-2*dot3(psph_i->ay, nu), nu, u);
    add3(u, psph_i->ay, psph_f->ay);

    /* Compute the reflected Nx vector */
    scale3(-2*dot3(psph_i->nx, nu), nu, u);
    add3(u, psph_i->nx, psph_f->nx);

    /* Compute the reflected Ny vector */
    scale3(-2*dot3(psph_i->ny, nu), nu, u);
    add3(u, psph_i->ny, psph_f->ny);

    /* Calculate where A's extension intersects the plane */
    d = (dot3(p, nu) - dot3(p3, nu)) / k;
    if (d < 0) {
	*behind = TRUE;
	return;
	}
    *behind = FALSE;
    scale3(d, u3, u);
    add3(p3, u, psph_f->c);

    /* Compute the reflected model's position */
    scale3(-d, u3r, u);
    add3(u, psph_f->c, psph_f->c);

    /* Reflect rotations */
    psph_f->sx = -psph_i->sx;
    psph_f->sy = -psph_i->sy;
    }


/******************************************************************************
********************************   CMOD_PSPH_SCALE   **************************
*******************************************************************************

    This function scales a camera model. The scale factors should be
    understood as the same scale factors that would be applied to a 2D
    coordinate in the original model to convert it to a coordinate in
    the resulting model. */

void cmod_psph_scale(
    cmod_float_t hscale,	/* input horizontal scale factor */
    cmod_float_t vscale,	/* input vertical   scale factor */
    const cmod_psph_t *psph_i,	/* input camera model */
    cmod_psph_t *psph_f)	/* output camera model */
{
    /* Check input */
    CMOD_ASSERT("cmod_psph_scale", psph_i != NULL);
    CMOD_ASSERT("cmod_psph_scale", psph_f != NULL);
    CMOD_ASSERT("cmod_psph_scale", hscale > EPSILON);
    CMOD_ASSERT("cmod_psph_scale", vscale > EPSILON);

    /* Copy the input to the output */
    *psph_f = *psph_i;

    /* Adjust the scale factors */
    psph_f->sx /= hscale;
    psph_f->sy /= vscale;
    }


/******************************************************************************
********************************   CMOD_PSPH_SHIFT   **************************
*******************************************************************************

    This function shifts a camera model. The shift values should be
    understood as the coordinates of the old model where the origin
    will fall in the new one. */

void cmod_psph_shift(
    cmod_float_t dx,		/* input horizontal shift */
    cmod_float_t dy,		/* input vertical   shift */
    const cmod_psph_t *psph_i,	/* input camera model */
    cmod_psph_t *psph_f)	/* output camera model */
{
    cmod_float_t q[4];
    cmod_float_t r[3][3];

    /* Check input */
    CMOD_ASSERT("cmod_psph_shift", psph_i != NULL);
    CMOD_ASSERT("cmod_psph_shift", psph_f != NULL);

    /* Copy the input to the output */
    *psph_f = *psph_i;

    /* Shift the model */
    quatva(psph_i->ax, dx * psph_i->sx, q);
    rotq(q, r);
    mult331(r, psph_i->nx, psph_f->nx);
    quatva(psph_i->ay, dy * psph_i->sy, q);
    rotq(q, r);
    mult331(r, psph_i->ny, psph_f->ny);
    }


/******************************************************************************
********************************   CMOD_PSPH_VALIDATE   ***********************
*******************************************************************************

    This function validates the components of a camera model to determine
    whether or not the model makes some kind of sense. Only generic
    sanity-checking tests are performed. */

cmod_stat_t cmod_psph_validate(
    const cmod_psph_t *psph)	/* input camera model */
{
    cmod_float_t mag;
    const cmod_float_t ulower  = 0.9;    /* lower magnitude for unit vector */
    const cmod_float_t uupper  = 1.1;    /* upper magnitude for unit vector */
    const cmod_float_t epsilon = 0.0001; /* upper magnitude close to zero */
    cmod_float_t v[3];

    /* Check input */
    CMOD_ASSERT("cmod_psph_validate", psph != NULL);

    /* Ax */
    mag = mag3(psph->ax);
    if ((mag < ulower) || (mag > uupper)) {
	CMOD_ERROR("cmod_psph_validate", "Bad Ax vector");
        return FAILURE;
        }

    /* Ay */
    mag = mag3(psph->ay);
    if ((mag < ulower) || (mag > uupper)) {
	CMOD_ERROR("cmod_psph_validate", "Bad Ay vector");
        return FAILURE;
        }

    /* Nx */
    mag = mag3(psph->nx);
    if ((mag < ulower) || (mag > uupper)) {
	CMOD_ERROR("cmod_psph_validate", "Bad Nx vector");
        return FAILURE;
        }

    /* Ny */
    mag = mag3(psph->ny);
    if ((mag < ulower) || (mag > uupper)) {
	CMOD_ERROR("cmod_psph_validate", "Bad Ny vector");
        return FAILURE;
        }

    /* sx */
    if (fabs(psph->sx) <= epsilon) {
	CMOD_ERROR("cmod_psph_validate", "Bad scale factor sx");
        return FAILURE;
        }

    /* sy */
    if (fabs(psph->sy) <= epsilon) {
	CMOD_ERROR("cmod_psph_validate", "Bad scale factor sy");
        return FAILURE;
        }

    /* sx and sy */
    if (psph->sx * psph->sy < 0) {
	CMOD_ERROR("cmod_psph_validate", "sx and sy have opposite signs");
        return FAILURE;
        }

    /* Ax and Ay */
    if (mag3(cross3(psph->ax, psph->ay, v)) < epsilon) {
	CMOD_ERROR("cmod_psph_validate", "Ax and Ay parallel");
        return FAILURE;
        }

    /* Ax and Nx */
    if (fabs(dot3(psph->ax, psph->nx)) > epsilon) {
	CMOD_ERROR("cmod_psph_validate", "Ax and Nx not orthogonal");
        return FAILURE;
        }

    /* Ay and Ny */
    if (fabs(dot3(psph->ay, psph->ny)) > epsilon) {
	CMOD_ERROR("cmod_psph_validate", "Ay and Ny not orthogonal");
        return FAILURE;
        }

    return SUCCESS;
    }
