/******************************************************************************
*                                                                             *
*                                    C M O D                                  *
*                                                                             *
*                                       Todd Litwin                           *
*                                       Written: 29 Aug 2003                  *
*                                       Updated: 21 Oct 2022                  *
*                                                                             *
*                                       Copyright (C) 2003, 2005, 2006, 2007, *
*                                                     2008, 2009, 2010, 2011, *
*                                                     2012, 2016, 2018, 2022  *
*                                       California Institute of Technology    *
*                                       All Rights Reserved                   *
*                                                                             *
*******************************************************************************


	This file contains composite camera-model utilities.

	This source code can be found here:

	    https://github.jpl.nasa.gov/telitwin/cmod

	Camera-calibration software, as well as descriptions of the model
	parameters and reference materials can be found here:

	    https://github.jpl.nasa.gov/telitwin/ccal

	*/


#include <mat3.h>
#include <string.h>

#include "cmod.h"
#include "cmod_cahv.h"
#include "cmod_cahvor.h"
#include "cmod_cahvore.h"
#include "cmod_psph.h"
#include "cmod_error.h"

#define SUCCESS 0
#define FAILURE (-1)


/******************************************************************************
********************************   CMOD_2D_TO_3D   ****************************
*******************************************************************************

    This function projects a 2D image point out into 3D using the
    camera model parameters provided. In addition to the 3D projection,
    it outputs and the partial-derivative matrix of the unit vector of the
    projection with respect to the 2D image-plane point. */

void cmod_2d_to_3d(
    const cmod_float_t pos2[2],	/* input 2D position */
    const cmod_t *cmod,		/* input core model */
    cmod_float_t pos3[3],	/* output 3D origin of projection */
    cmod_float_t uvec3[3],	/* output unit vector ray of projection */
    cmod_float_t ppar[3][2],	/* output partial-derivative matrix of
					pos3 to pos2, or NULL */
    cmod_float_t upar[3][2])	/* output partial-derivative matrix of
					uvec3 to pos2, or NULL */
{
    cmod_int_t i;
    cmod_int_t j;

    /* Check input */
    CMOD_ASSERT("cmod_2d_to_3d", cmod != NULL);

    /* Proceed based on underlying model class */
    switch (cmod->mclass) {

	/* CAHV */
	case CMOD_CLASS_CAHV:
	    cmod_cahv_2d_to_3d(pos2,
		cmod->u.cahv.c, cmod->u.cahv.a, cmod->u.cahv.h, cmod->u.cahv.v,
		pos3, uvec3, upar);
	    if (ppar != NULL) {
		for (i=0; i<3; i++) {
		    for (j=0; j<2; j++) {
			ppar[i][j] = 0;
			}
		    }
		}
	    break;

	/* CAHVOR */
	case CMOD_CLASS_CAHVOR:
	    cmod_cahvor_2d_to_3d(pos2,
		cmod->u.cahvor.c, cmod->u.cahvor.a, cmod->u.cahvor.h,
		cmod->u.cahvor.v, cmod->u.cahvor.o, cmod->u.cahvor.r,
		FALSE, pos3, uvec3, upar);
	    if (ppar != NULL) {
		for (i=0; i<3; i++) {
		    for (j=0; j<2; j++) {
			ppar[i][j] = 0;
			}
		    }
		}
	    break;

	/* CAHVORE */
	case CMOD_CLASS_CAHVORE:
	    cmod_cahvore_2d_to_3d(pos2,
		cmod->u.cahvore.mtype, cmod->u.cahvore.mparm,
		cmod->u.cahvore.c, cmod->u.cahvore.a, cmod->u.cahvore.h,
		cmod->u.cahvore.v, cmod->u.cahvore.o, cmod->u.cahvore.r,
		cmod->u.cahvore.e,
		FALSE, pos3, uvec3, ppar, upar);
	    break;

	/* PSPH */
	case CMOD_CLASS_PSPH:
	    cmod_psph_2d_to_3d(pos2,
		&cmod->u.psph, pos3, uvec3, upar);
	    if (ppar != NULL) {
		for (i=0; i<3; i++) {
		    for (j=0; j<2; j++) {
			ppar[i][j] = 0;
			}
		    }
		}
	    break;

	/* Unknown */
	default:
	    CMOD_ASSERT_1("cmod_2d_to_3d", 0, cmod->mclass);
	}
    }


/******************************************************************************
********************************   CMOD_2D_TO_3D_EXT   ************************
*******************************************************************************

    This function projects a 2D image point out into 3D using the
    camera model parameters provided. In addition to the 3D projection,
    it outputs and the partial-derivative matrix of the unit vector of the
    projection with respect to the 2D image-plane point. */

void cmod_2d_to_3d_ext(
    const cmod_float_t pos2[2],	/* input 2D position */
    const cmod_ext_t *cmod,	/* input extended model */
    cmod_float_t pos3[3],	/* output 3D origin of projection */
    cmod_float_t uvec3[3],	/* output unit vector ray of projection */
    cmod_float_t ppar[3][2],	/* output partial-derivative matrix of
					pos3 to pos2, or NULL */
    cmod_float_t upar[3][2])	/* output partial-derivative matrix of
					uvec3 to pos2, or NULL */
{
    CMOD_ASSERT("cmod_2d_to_3d_ext", cmod != NULL);
    cmod_2d_to_3d(pos2, &(cmod->core), pos3, uvec3, ppar, upar);
    }


/******************************************************************************
********************************   CMOD_3D_TO_2D   ****************************
*******************************************************************************

    This function projects a 3D point into the image plane using the camera
    model parameters provided. In addition to the 2D projection, it outputs
    the partial derivative matrix of the 2D point with respect to the 3D
    point. */

void cmod_3d_to_2d(
    const cmod_float_t pos3[3],	/* input 3D position */
    const cmod_t *cmod,		/* input core model */
    cmod_float_t pos2[2],	/* output 2D image-plane projection */
    cmod_float_t par[2][3])	/* output partial-derivative matrix of
					pos2 to pos3, or NULL */
{
    cmod_float_t range;

    /* Check input */
    CMOD_ASSERT("cmod_3d_to_2d", cmod != NULL);

    /* Proceed based on underlying model class */
    switch (cmod->mclass) {

	/* CAHV */
	case CMOD_CLASS_CAHV:
	    cmod_cahv_3d_to_2d(pos3,
		cmod->u.cahv.c, cmod->u.cahv.a, cmod->u.cahv.h, cmod->u.cahv.v,
		&range, pos2, par);
	    break;

	/* CAHVOR */
	case CMOD_CLASS_CAHVOR:
	    cmod_cahvor_3d_to_2d(pos3,
		cmod->u.cahvor.c, cmod->u.cahvor.a, cmod->u.cahvor.h,
		cmod->u.cahvor.v, cmod->u.cahvor.o, cmod->u.cahvor.r,
		FALSE, &range, pos2, par);
	    break;

	/* CAHVORE */
	case CMOD_CLASS_CAHVORE:
	    cmod_cahvore_3d_to_2d(pos3,
		cmod->u.cahvore.mtype, cmod->u.cahvore.mparm,
		cmod->u.cahvore.c, cmod->u.cahvore.a, cmod->u.cahvore.h,
		cmod->u.cahvore.v, cmod->u.cahvore.o, cmod->u.cahvore.r,
		cmod->u.cahvore.e,
		FALSE, &range, pos2, par);
	    break;

	/* PSPH */
	case CMOD_CLASS_PSPH:
	    cmod_psph_3d_to_2d(pos3,
		&cmod->u.psph, pos2, par);
	    break;

	/* Unknown */
	default:
	    CMOD_ASSERT_1("cmod_3d_to_2d", 0, cmod->mclass);
	}
    }


/******************************************************************************
********************************   CMOD_3D_TO_2D_EXT   ************************
*******************************************************************************

    This function projects a 3D point into the image plane using the camera
    model parameters provided. In addition to the 2D projection, it outputs
    the partial derivative matrix of the 2D point with respect to the 3D
    point. */

void cmod_3d_to_2d_ext(
    const cmod_float_t pos3[3],	/* input 3D position */
    const cmod_ext_t *cmod,	/* input extended model */
    cmod_float_t pos2[2],	/* output 2D image-plane projection */
    cmod_float_t par[2][3])	/* output partial-derivative matrix of
					pos2 to pos3, or NULL */
{
    CMOD_ASSERT("cmod_3d_to_2d_ext", cmod != NULL);
    cmod_3d_to_2d(pos3, &(cmod->core), pos2, par);
    }


/******************************************************************************
********************************   CMOD_EXT   *********************************
*******************************************************************************

    This function converts a core model into an extended one. It returns a
    pointer to the output model. */

cmod_ext_t *cmod_ext(
    const cmod_t *cmodcore,	/* input core model */
    cmod_ext_t *cmodext)	/* output extended model */
{
    cmod_int_t i;
    cmod_int_t j;

    /* Check input */
    CMOD_ASSERT("cmod_ext", cmodcore != NULL);
    CMOD_ASSERT("cmod_ext", cmodext  != NULL);

    /* Start by copying the core model */
    cmodext->core = *cmodcore;

    /* Proceed with extended data based on underlying model class */
    switch (cmodcore->mclass) {

	/* CAHV */
	case CMOD_CLASS_CAHV:
	    for (i=0; i<12; i++) {
		for (j=0; j<12; j++) {
		    cmodext->ext.cahv.s[i][j] = 0;
		    }
		}
	    for (i=0; i<5; i++) {
		for (j=0; j<5; j++) {
		    cmodext->ext.cahv.s_int[i][j] = 0;
		    }
		}
	    cmod_cahv_internal(
		cmodcore->u.cahv.c, cmodcore->u.cahv.a,
		cmodcore->u.cahv.h, cmodcore->u.cahv.v, NULL,
		&cmodext->ext.cahv.hs, &cmodext->ext.cahv.hc,
		&cmodext->ext.cahv.vs, &cmodext->ext.cahv.vc,
		&cmodext->ext.cahv.theta, NULL);
	    break;

	/* CAHVOR */
	case CMOD_CLASS_CAHVOR:
	    for (i=0; i<18; i++) {
		for (j=0; j<18; j++) {
		    cmodext->ext.cahvor.s[i][j] = 0;
		    }
		}
	    for (i=0; i<5; i++) {
		for (j=0; j<5; j++) {
		    cmodext->ext.cahvor.s_int[i][j] = 0;
		    }
		}
	    cmod_cahv_internal(
		cmodcore->u.cahvor.c, cmodcore->u.cahvor.a,
		cmodcore->u.cahvor.h, cmodcore->u.cahvor.v, NULL,
		&cmodext->ext.cahvor.hs, &cmodext->ext.cahvor.hc,
		&cmodext->ext.cahvor.vs, &cmodext->ext.cahvor.vc,
		&cmodext->ext.cahvor.theta, NULL);
	    break;

	/* CAHVORE */
	case CMOD_CLASS_CAHVORE:
	    for (i=0; i<21; i++) {
		for (j=0; j<21; j++) {
		    cmodext->ext.cahvore.s[i][j] = 0;
		    }
		}
	    for (i=0; i<5; i++) {
		for (j=0; j<5; j++) {
		    cmodext->ext.cahvore.s_int[i][j] = 0;
		    }
		}
	    cmod_cahv_internal(
		cmodcore->u.cahvore.c, cmodcore->u.cahvore.a,
		cmodcore->u.cahvore.h, cmodcore->u.cahvore.v, NULL,
		&cmodext->ext.cahvore.hs, &cmodext->ext.cahvore.hc,
		&cmodext->ext.cahvore.vs, &cmodext->ext.cahvore.vc,
		&cmodext->ext.cahvore.theta, NULL);
	    break;

	/* PSPH */
	case CMOD_CLASS_PSPH:
	    cmod_psph_internal(
		&cmodcore->u.psph,
		&cmodext->ext.psph.theta);
	    break;

	/* Unknown */
	default:
	    CMOD_ASSERT_1("cmod_ext", 0, cmodcore->mclass);
	}

    return cmodext;
    }


/******************************************************************************
********************************   CMOD_INIT_CAHV   ***************************
*******************************************************************************

    This function initializes a model from the parameters of a CAHV model. */

void cmod_init_cahv(
    cmod_int_t xdim,		/* input number of columns */
    cmod_int_t ydim,		/* input number of rows */
    const cmod_float_t c[3],	/* input model center vector C */
    const cmod_float_t a[3],	/* input model axis   vector A */
    const cmod_float_t h[3],	/* input model horiz. vector H */
    const cmod_float_t v[3],	/* input model vert.  vector V */
    cmod_t *cmod)		/* output core model */
{
    /* Check input */
    CMOD_ASSERT("cmod_init_cahv", cmod != NULL);
    CMOD_ASSERT("cmod_init_cahv", c    != NULL);
    CMOD_ASSERT("cmod_init_cahv", a    != NULL);
    CMOD_ASSERT("cmod_init_cahv", h    != NULL);
    CMOD_ASSERT("cmod_init_cahv", v    != NULL);

    /* Initialize model */
    cmod->mclass = CMOD_CLASS_CAHV;
    cmod->xdim = xdim;
    cmod->ydim = ydim;
    copy3(c, cmod->u.cahv.c);
    copy3(a, cmod->u.cahv.a);
    copy3(h, cmod->u.cahv.h);
    copy3(v, cmod->u.cahv.v);
    }


/******************************************************************************
********************************   CMOD_INIT_CAHVOR   *************************
*******************************************************************************

    This function initializes a model from the parameters of a CAHVOR model. */

void cmod_init_cahvor(
    cmod_int_t xdim,		/* input number of columns */
    cmod_int_t ydim,		/* input number of rows */
    const cmod_float_t c[3],	/* input model center vector C */
    const cmod_float_t a[3],	/* input model axis   vector A */
    const cmod_float_t h[3],	/* input model horiz. vector H */
    const cmod_float_t v[3],	/* input model vert.  vector V */
    const cmod_float_t o[3],	/* input model optical axis unit vector O */
    const cmod_float_t r[3],	/* input model radial-distortion terms  R */
    cmod_t *cmod)		/* output core model */
{
    /* Check input */
    CMOD_ASSERT("cmod_init_cahvor", cmod != NULL);
    CMOD_ASSERT("cmod_init_cahvor", c    != NULL);
    CMOD_ASSERT("cmod_init_cahvor", a    != NULL);
    CMOD_ASSERT("cmod_init_cahvor", h    != NULL);
    CMOD_ASSERT("cmod_init_cahvor", v    != NULL);
    CMOD_ASSERT("cmod_init_cahvor", o    != NULL);
    CMOD_ASSERT("cmod_init_cahvor", r    != NULL);

    /* Initialize model */
    cmod->mclass = CMOD_CLASS_CAHVOR;
    cmod->xdim = xdim;
    cmod->ydim = ydim;
    copy3(c, cmod->u.cahvor.c);
    copy3(a, cmod->u.cahvor.a);
    copy3(h, cmod->u.cahvor.h);
    copy3(v, cmod->u.cahvor.v);
    copy3(o, cmod->u.cahvor.o);
    copy3(r, cmod->u.cahvor.r);
    }


/******************************************************************************
********************************   CMOD_INIT_CAHVORE   ************************
*******************************************************************************

    This function initializes a model from the parameters of a CAHVORE model. */

void cmod_init_cahvore(
    cmod_int_t xdim,		/* input number of columns */
    cmod_int_t ydim,		/* input number of rows */
    cmod_int_t mtype,		/* input type of model */
    cmod_float_t mparm,		/* input model parameter */
    const cmod_float_t c[3],	/* input model center vector C */
    const cmod_float_t a[3],	/* input model axis   vector A */
    const cmod_float_t h[3],	/* input model horiz. vector H */
    const cmod_float_t v[3],	/* input model vert.  vector V */
    const cmod_float_t o[3],	/* input model optical axis unit vector O */
    const cmod_float_t r[3],	/* input model radial-distortion terms  R */
    const cmod_float_t e[3],	/* input model entrance-pupil    terms  E */
    cmod_t *cmod)		/* output core model */
{
    /* Check input */
    CMOD_ASSERT("cmod_init_cahvore", cmod != NULL);
    CMOD_ASSERT("cmod_init_cahvore", c    != NULL);
    CMOD_ASSERT("cmod_init_cahvore", a    != NULL);
    CMOD_ASSERT("cmod_init_cahvore", h    != NULL);
    CMOD_ASSERT("cmod_init_cahvore", v    != NULL);
    CMOD_ASSERT("cmod_init_cahvore", o    != NULL);
    CMOD_ASSERT("cmod_init_cahvore", r    != NULL);
    CMOD_ASSERT("cmod_init_cahvore", e    != NULL);

    /* Initialize model */
    cmod->mclass = CMOD_CLASS_CAHVORE;
    cmod->xdim = xdim;
    cmod->ydim = ydim;
    cmod->u.cahvore.mtype = mtype;
    cmod->u.cahvore.mparm = mparm;
    copy3(c, cmod->u.cahvore.c);
    copy3(a, cmod->u.cahvore.a);
    copy3(h, cmod->u.cahvore.h);
    copy3(v, cmod->u.cahvore.v);
    copy3(o, cmod->u.cahvore.o);
    copy3(r, cmod->u.cahvore.r);
    copy3(e, cmod->u.cahvore.e);
    }


/******************************************************************************
********************************   CMOD_INIT_PSPH   ***************************
*******************************************************************************

    This function initializes a model from the parameters of a PSPH model. */

void cmod_init_psph(
    cmod_int_t xdim,		/* input number of columns */
    cmod_int_t ydim,		/* input number of rows */
    cmod_float_t c[3],		/* input sphere center */
    cmod_float_t ax[3],		/* input column rotation axis */
    cmod_float_t ay[3],		/* input row    rotation axis */
    cmod_float_t nx[3],		/* input column-plane normal vector at col 0 */
    cmod_float_t ny[3],		/* input row   -plane normal vector at row 0 */
    cmod_float_t sx,		/* input column scale factor (rad/pixel) */
    cmod_float_t sy,		/* input row    scale factor (rad/pixel) */
    cmod_t *cmod)		/* output core model */
{
    /* Check input */
    CMOD_ASSERT("cmod_init_cahv", cmod != NULL);
    CMOD_ASSERT("cmod_init_cahv", c    != NULL);
    CMOD_ASSERT("cmod_init_cahv", ax   != NULL);
    CMOD_ASSERT("cmod_init_cahv", ay   != NULL);
    CMOD_ASSERT("cmod_init_cahv", nx   != NULL);
    CMOD_ASSERT("cmod_init_cahv", ny   != NULL);

    /* Initialize model */
    cmod->mclass = CMOD_CLASS_PSPH;
    cmod->xdim = xdim;
    cmod->ydim = ydim;
    copy3(c,  cmod->u.psph.c);
    copy3(ax, cmod->u.psph.ax);
    copy3(ay, cmod->u.psph.ay);
    copy3(nx, cmod->u.psph.nx);
    copy3(ny, cmod->u.psph.ny);
    cmod->u.psph.sx = sx;
    cmod->u.psph.sy = sy;
    }


/******************************************************************************
********************************   CMOD_IPLANE   ******************************
*******************************************************************************

    This function computes information on the image plane and related
    quantities. It computes the unit outward-facing normal to the image plane
    along with in-plane unit vectors in the directions of increasing
    dimensions for the two coordinates (which need not be mutually orthogonal).
    It provides the 3D projection point through which image-plane points are
    projected, as well as the 2D "center" image coorinate whose projection is
    along the image plane's normal.

    Note that the in-plane unit vectors are constructed as if the image plane
    lies between the projection point and the scene being viewed. While this
    is not the physical case for real cameras, whose image planes lie behind
    the lens and receive an inverted image from light that passes through it,
    this simplifies thinking about the geometry. */

void cmod_iplane(
    const cmod_t *cmod,		/* input core model */
    cmod_float_t ndir[3],	/* output normal direction */
    cmod_float_t xdir[3],	/* output X (0th) coordinate direction */
    cmod_float_t ydir[3],	/* output Y (1st) coordinate direction */
    cmod_float_t pos3[3],	/* output 3D projection point */
    cmod_float_t pos2[2])	/* output 2D projection center */
{
    /* Check input */
    CMOD_ASSERT("cmod_iplane", cmod != NULL);
    CMOD_ASSERT("cmod_iplane", pos2 != NULL);

    /* Proceed based on underlying model class */
    switch (cmod->mclass) {

	/* CAHV */
	case CMOD_CLASS_CAHV:
	    cmod_cahv_iplane(
		cmod->u.cahv.c, cmod->u.cahv.a,
		cmod->u.cahv.h, cmod->u.cahv.v,
		pos3, ndir, xdir, ydir, pos2+0, pos2+1);
	    break;

	/* CAHVOR */
	case CMOD_CLASS_CAHVOR:
	    cmod_cahv_iplane(
		cmod->u.cahvor.c, cmod->u.cahvor.a,
		cmod->u.cahvor.h, cmod->u.cahvor.v,
		pos3, ndir, xdir, ydir, pos2+0, pos2+1);
	    break;

	/* CAHVORE */
	case CMOD_CLASS_CAHVORE:
	    cmod_cahv_iplane(
		cmod->u.cahvore.c, cmod->u.cahvore.a,
		cmod->u.cahvore.h, cmod->u.cahvore.v,
		pos3, ndir, xdir, ydir, pos2+0, pos2+1);
	    break;

	/* PSPH */
	case CMOD_CLASS_PSPH:
	    CMOD_ASSERT("cmod_iplane", cmod->xdim > 0);
	    CMOD_ASSERT("cmod_iplane", cmod->ydim > 0);
	    pos2[0] = (cmod->xdim - 1) / 2.0;
	    pos2[1] = (cmod->ydim - 1) / 2.0;
	    cmod_psph_iplane(
		&cmod->u.psph, pos2[0], pos2[1],
		pos3, ndir, xdir, ydir);
	    break;

	/* Unknown */
	default:
	    CMOD_ASSERT_1("cmod_iplane", 0, cmod->mclass);
	}
    }


/******************************************************************************
********************************   CMOD_IPLANE_EXT   **************************
*******************************************************************************

    This function is the same as cmod_iplane(), but for an extended model */

void cmod_iplane_ext(
    const cmod_ext_t *cmod,	/* input extended model */
    cmod_float_t ndir[3],	/* output normal direction */
    cmod_float_t xdir[3],	/* output X (0th) coordinate direction */
    cmod_float_t ydir[3],	/* output Y (1st) coordinate direction */
    cmod_float_t pos3[3],	/* output 3D projection point */
    cmod_float_t pos2[2])	/* output 2D projection center */
{
    CMOD_ASSERT("cmod_iplane_ext", cmod != NULL);
    cmod_iplane(&(cmod->core), ndir, xdir, ydir, pos3, pos2);
    }


/******************************************************************************
********************************   CMOD_MOVE   ********************************
*******************************************************************************

    This function relocates a camera model, based on the initial and final
    positions and orientations of a camera platform reference point, a
    point which is rigidly connected to the camera, but is otherwise
    arbitrary. */

void cmod_move(
    const cmod_float_t p_i[3],	/* input initial camera ref pt position */
    const cmod_float_t q_i[4],	/* input initial camera ref pt orientation */
    const cmod_t *cmod_i,	/* input initial core model */
    const cmod_float_t p_f[3],	/* input final camera ref pt position */
    const cmod_float_t q_f[4],	/* input final camera ref pt orientation */
    cmod_t *cmod_f)		/* output final core model */
{
    /* Check input */
    CMOD_ASSERT("cmod_move", cmod_i != NULL);
    CMOD_ASSERT("cmod_move", cmod_f != NULL);
    CMOD_ASSERT("cmod_move", cmod_i != cmod_f);	/* models must be distinct */

    /* The output model will be the same class as the input model */
    cmod_f->xdim   = cmod_i->xdim;
    cmod_f->ydim   = cmod_i->ydim;
    cmod_f->mclass = cmod_i->mclass;

    /* Proceed based on underlying model class */
    switch (cmod_i->mclass) {

	/* CAHV */
	case CMOD_CLASS_CAHV:
	    cmod_cahv_move(
		p_i, q_i,
		cmod_i->u.cahv.c, cmod_i->u.cahv.a, cmod_i->u.cahv.h,
		cmod_i->u.cahv.v,
		p_f, q_f,
		cmod_f->u.cahv.c, cmod_f->u.cahv.a, cmod_f->u.cahv.h,
		cmod_f->u.cahv.v);
	    break;

	/* CAHVOR */
	case CMOD_CLASS_CAHVOR:
	    cmod_cahvor_move(
		p_i, q_i,
		cmod_i->u.cahvor.c, cmod_i->u.cahvor.a, cmod_i->u.cahvor.h,
		cmod_i->u.cahvor.v, cmod_i->u.cahvor.o, cmod_i->u.cahvor.r,
		p_f, q_f,
		cmod_f->u.cahvor.c, cmod_f->u.cahvor.a, cmod_f->u.cahvor.h,
		cmod_f->u.cahvor.v, cmod_f->u.cahvor.o, cmod_f->u.cahvor.r);
	    break;

	/* CAHVORE */
	case CMOD_CLASS_CAHVORE:
	    cmod_cahvore_move(
		p_i, q_i,
		cmod_i->u.cahvore.c, cmod_i->u.cahvore.a, cmod_i->u.cahvore.h,
		cmod_i->u.cahvore.v, cmod_i->u.cahvore.o, cmod_i->u.cahvore.r,
		cmod_i->u.cahvore.e,
		p_f, q_f,
		cmod_f->u.cahvore.c, cmod_f->u.cahvore.a, cmod_f->u.cahvore.h,
		cmod_f->u.cahvore.v, cmod_f->u.cahvore.o, cmod_f->u.cahvore.r,
		cmod_f->u.cahvore.e);
	    cmod_f->u.cahvore.mtype = cmod_i->u.cahvore.mtype;
	    cmod_f->u.cahvore.mparm = cmod_i->u.cahvore.mparm;
	    break;

	/* PSPH */
	case CMOD_CLASS_PSPH:
	    cmod_psph_move(
		p_i, q_i, &cmod_i->u.psph,
		p_f, q_f, &cmod_f->u.psph);
	    break;

	/* Unknown */
	default:
	    CMOD_ASSERT_1("cmod_move", 0, cmod_i->mclass);
	}
    }


/******************************************************************************
********************************   CMOD_MOVE_EXT   ****************************
*******************************************************************************

    This function relocates a camera model, based on the initial and final
    positions and orientations of a camera platform reference point, a
    point which is rigidly connected to the camera, but is otherwise
    arbitrary. */

void cmod_move_ext(
    const cmod_float_t p_i[3],	/* input initial camera ref pt position */
    const cmod_float_t q_i[4],	/* input initial camera ref pt orientation */
    const cmod_ext_t *cmod_i,	/* input initial extended model */
    const cmod_float_t p_f[3],	/* input final camera ref pt position */
    const cmod_float_t q_f[4],	/* input final camera ref pt orientation */
    cmod_ext_t *cmod_f)		/* output final extended model */
{
    /* Check input */
    CMOD_ASSERT("cmod_move_ext", cmod_i != NULL);
    CMOD_ASSERT("cmod_move_ext", cmod_f != NULL);
    CMOD_ASSERT("cmod_move_ext", cmod_i != cmod_f);	/* must be distinct */

    /* Compute the transform of the core model */
    cmod_move(p_i, q_i, &(cmod_i->core), p_f, q_f, &(cmod_f->core));

    /* Compute the transform of the extended part of the model */
    switch (cmod_i->core.mclass) {

	/* CAHV */
	case CMOD_CLASS_CAHV:
	    cmod_cahv_rotate_cov(
		q_i, ((cmod_ext_t *)cmod_i)->ext.cahv.s,
		q_f, cmod_f->ext.cahv.s);
	    cmod_f->ext.cahv.hs    = cmod_i->ext.cahv.hs;
	    cmod_f->ext.cahv.hc    = cmod_i->ext.cahv.hc;
	    cmod_f->ext.cahv.vs    = cmod_i->ext.cahv.vs;
	    cmod_f->ext.cahv.vc    = cmod_i->ext.cahv.vc;
	    cmod_f->ext.cahv.theta = cmod_i->ext.cahv.theta;
	    memcpy(&(cmod_f->ext.cahv.s_int),
		   &(cmod_i->ext.cahv.s_int),
		   sizeof(cmod_i->ext.cahv.s_int));
	    break;

	/* CAHVOR */
	case CMOD_CLASS_CAHVOR:
	    cmod_cahvor_rotate_cov(
		q_i, ((cmod_ext_t *)cmod_i)->ext.cahvor.s,
		q_f, cmod_f->ext.cahvor.s);
	    cmod_f->ext.cahvor.hs    = cmod_i->ext.cahvor.hs;
	    cmod_f->ext.cahvor.hc    = cmod_i->ext.cahvor.hc;
	    cmod_f->ext.cahvor.vs    = cmod_i->ext.cahvor.vs;
	    cmod_f->ext.cahvor.vc    = cmod_i->ext.cahvor.vc;
	    cmod_f->ext.cahvor.theta = cmod_i->ext.cahvor.theta;
	    memcpy(&(cmod_f->ext.cahvor.s_int),
		   &(cmod_i->ext.cahvor.s_int),
		   sizeof(cmod_i->ext.cahvor.s_int));
	    break;

	/* CAHVORE */
	case CMOD_CLASS_CAHVORE:
	    cmod_cahvore_rotate_cov(
		q_i, ((cmod_ext_t *)cmod_i)->ext.cahvore.s,
		q_f, cmod_f->ext.cahvore.s);
	    cmod_f->ext.cahvore.hs    = cmod_i->ext.cahvore.hs;
	    cmod_f->ext.cahvore.hc    = cmod_i->ext.cahvore.hc;
	    cmod_f->ext.cahvore.vs    = cmod_i->ext.cahvore.vs;
	    cmod_f->ext.cahvore.vc    = cmod_i->ext.cahvore.vc;
	    cmod_f->ext.cahvore.theta = cmod_i->ext.cahvore.theta;
	    memcpy(&(cmod_f->ext.cahvore.s_int),
		   &(cmod_i->ext.cahvore.s_int),
		   sizeof(cmod_i->ext.cahvore.s_int));
	    break;

	/* PSPH */
	case CMOD_CLASS_PSPH:
	    cmod_f->ext.cahv.theta = cmod_i->ext.cahv.theta;
	    break;

	/* Unknown */
	default:
	    CMOD_ASSERT_1("cmod_move_ext", 0, cmod_i->core.mclass);
	}
    }


/******************************************************************************
********************************   CMOD_POSE   ********************************
*******************************************************************************

    This function returns the position and rotation matrix of orientation
    for the given model. The absolute orientation is based on a reference
    orientation of the camera pointing straight down the Y axis, with Z up.
    */

void cmod_pose(
    const cmod_t *cmod,		/* input core model */
    cmod_float_t p[3],		/* output position vector */
    cmod_float_t r[3][3])	/* output rotation matrix */
{
    /* Check input */
    CMOD_ASSERT("cmod_pose", cmod != NULL);

    /* Proceed based on underlying model class */
    switch (cmod->mclass) {

	/* CAHV */
	case CMOD_CLASS_CAHV:
	    cmod_cahv_pose(cmod->u.cahv.c, cmod->u.cahv.a,
			   cmod->u.cahv.h, cmod->u.cahv.v, p, r);
	    break;

	/* CAHVOR */
	case CMOD_CLASS_CAHVOR:
	    cmod_cahv_pose(cmod->u.cahvor.c, cmod->u.cahvor.a,
			   cmod->u.cahvor.h, cmod->u.cahvor.v, p, r);
	    break;

	/* CAHVORE */
	case CMOD_CLASS_CAHVORE:
	    cmod_cahv_pose(cmod->u.cahvore.c, cmod->u.cahvore.a,
			   cmod->u.cahvore.h, cmod->u.cahvore.v, p, r);
	    break;

	/* PSPH */
	case CMOD_CLASS_PSPH:
	    cmod_psph_pose(&cmod->u.psph,
			   (cmod->xdim-1)/2.0, (cmod->ydim-1)/2.0, p, r);
	    break;

	/* Unknown */
	default:
	    CMOD_ASSERT_1("cmod_pose", 0, cmod->mclass);
	}
    }


/******************************************************************************
********************************   CMOD_POSE_EXT   ****************************
*******************************************************************************

    This function returns the position and rotation matrix of orientation
    for the given model. The absolute orientation is based on a reference
    orientation of the camera pointing straight down the Y axis, with Z up.
    */

void cmod_pose_ext(
    const cmod_ext_t *cmod,	/* input extended model */
    cmod_float_t p[3],		/* output position vector */
    cmod_float_t r[3][3])	/* output rotation matrix */
{
    CMOD_ASSERT("cmod_pose_ext", cmod != NULL);
    cmod_pose(&(cmod->core), p, r);
    }


/******************************************************************************
********************************   CMOD_REFLECT   *****************************
*******************************************************************************

    This function relocates a camera model, based on the image reflecting
    from a mirror defined by a plane. If the initial model is the true
    model, then the final model will be a virtual model representing how
    the camera sees the reflected world. If the initial model is the
    virtual model, then the final model will be the true model. */

void cmod_reflect(
    const cmod_t *cmod1,	/* input initial core model */
    const cmod_float_t p[3],	/* input point on the reflecting plane */
    const cmod_float_t n[3],	/* input normal to the reflecting plane */
    cmod_t *cmod2,		/* output final core model */
    cmod_bool_t *parallel,	/* output if camera view & plane are parallel */
    cmod_bool_t *behind)	/* output if camera is behind reflecting plane*/
{
    /* Check input */
    CMOD_ASSERT("cmod_reflect", cmod1    != NULL);
    CMOD_ASSERT("cmod_reflect", cmod2    != NULL);
    CMOD_ASSERT("cmod_reflect", parallel != NULL);
    CMOD_ASSERT("cmod_reflect", behind   != NULL);
    CMOD_ASSERT("cmod_reflect", cmod1    != cmod2);	/* must be distinct */

    /* The output model will be the same class as the input model */
    cmod2->xdim   = cmod1->xdim;
    cmod2->ydim   = cmod1->ydim;
    cmod2->mclass = cmod1->mclass;

    /* Proceed based on underlying model class */
    switch (cmod1->mclass) {

	/* CAHV */
	case CMOD_CLASS_CAHV:
	    cmod_cahv_reflect(
		cmod1->u.cahv.c, cmod1->u.cahv.a, cmod1->u.cahv.h,
		cmod1->u.cahv.v,
		p, n,
		cmod2->u.cahv.c, cmod2->u.cahv.a, cmod2->u.cahv.h,
		cmod2->u.cahv.v,
		parallel, behind);
	    if (*parallel || *behind) {
		cmod2->mclass = CMOD_CLASS_NONE;
		break;
		}
	    break;

	/* CAHVOR */
	case CMOD_CLASS_CAHVOR:
	    cmod_cahvor_reflect(
		cmod1->u.cahvor.c, cmod1->u.cahvor.a, cmod1->u.cahvor.h,
		cmod1->u.cahvor.v, cmod1->u.cahvor.o, cmod1->u.cahvor.r,
		p, n,
		cmod2->u.cahvor.c, cmod2->u.cahvor.a, cmod2->u.cahvor.h,
		cmod2->u.cahvor.v, cmod2->u.cahvor.o, cmod2->u.cahvor.r,
		parallel, behind);
	    if (*parallel || *behind) {
		cmod2->mclass = CMOD_CLASS_NONE;
		break;
		}
	    break;

	/* CAHVORE */
	case CMOD_CLASS_CAHVORE:
	    cmod_cahvore_reflect(
		cmod1->u.cahvore.c, cmod1->u.cahvore.a, cmod1->u.cahvore.h,
		cmod1->u.cahvore.v, cmod1->u.cahvore.o, cmod1->u.cahvore.r,
		cmod1->u.cahvore.e,
		p, n,
		cmod2->u.cahvore.c, cmod2->u.cahvore.a, cmod2->u.cahvore.h,
		cmod2->u.cahvore.v, cmod2->u.cahvore.o, cmod2->u.cahvore.r,
		cmod2->u.cahvore.e,
		parallel, behind);
	    if (*parallel || *behind) {
		cmod2->mclass = CMOD_CLASS_NONE;
		break;
		}
	    cmod2->u.cahvore.mtype = cmod1->u.cahvore.mtype;
	    cmod2->u.cahvore.mparm = cmod1->u.cahvore.mparm;
	    break;

	/* PSPH */
	case CMOD_CLASS_PSPH:
	    cmod_psph_reflect(
		&cmod1->u.psph,
		(cmod1->xdim-1)/2.0, (cmod1->ydim-1)/2.0,
		p, n,
		&cmod2->u.psph,
		parallel, behind);
	    if (*parallel || *behind) {
		cmod2->mclass = CMOD_CLASS_NONE;
		break;
		}
	    break;

	/* Unknown */
	default:
	    CMOD_ASSERT_1("cmod_reflect", 0, cmod1->mclass);
	}
    }


/******************************************************************************
********************************   CMOD_REFLECT_EXT   *************************
*******************************************************************************

    This function relocates a camera model, based on the image reflecting
    from a mirror defined by a plane. If the initial model is the true
    model, then the final model will be a virtual model representing how
    the camera sees the reflected world. If the initial model is the
    virtual model, then the final model will be the true model. */

void cmod_reflect_ext(
    const cmod_ext_t *cmod1,	/* input initial extended model */
    const cmod_float_t p[3],	/* input point on the reflecting plane */
    const cmod_float_t n[3],	/* input normal to the reflecting plane */
    cmod_ext_t *cmod2,		/* output final extended model */
    cmod_bool_t *parallel,	/* output if camera view & plane are parallel */
    cmod_bool_t *behind)	/* output if camera is behind reflecting plane*/
{
    cmod_int_t i;
    cmod_int_t j;
    cmod_float_t s12[12][12];

    /* Check input */
    CMOD_ASSERT("cmod_reflect_ext", cmod1    != NULL);
    CMOD_ASSERT("cmod_reflect_ext", cmod2    != NULL);
    CMOD_ASSERT("cmod_reflect_ext", parallel != NULL);
    CMOD_ASSERT("cmod_reflect_ext", behind   != NULL);
    CMOD_ASSERT("cmod_reflect_ext", cmod1    != cmod2);	/* must be distinct */

    /* Compute the transform of the core model */
    cmod_reflect(&(cmod1->core), p, n, &(cmod2->core), parallel, behind);
    if (*parallel || *behind) {
	return;
	}

    /* Compute the transform of the extended part of the model */
    switch (cmod1->core.mclass) {

	/* CAHV */
	case CMOD_CLASS_CAHV:
	    cmod_cahv_reflect_cov(((cmod_ext_t *)cmod1)->ext.cahv.s, n,
					cmod2->ext.cahv.s);
	    cmod_cahv_internal(
		cmod2->core.u.cahv.c, cmod2->core.u.cahv.a,
		cmod2->core.u.cahv.h, cmod2->core.u.cahv.v,
		cmod2->ext.cahv.s,
		&(cmod2->ext.cahv.hs), &(cmod2->ext.cahv.hc),
		&(cmod2->ext.cahv.vs), &(cmod2->ext.cahv.vc),
		&(cmod2->ext.cahv.theta), (cmod2->ext.cahv.s_int));
	    break;

	/* CAHVOR */
	case CMOD_CLASS_CAHVOR:
	    cmod_cahvor_reflect_cov(((cmod_ext_t *)cmod1)->ext.cahvor.s, n,
					cmod2->ext.cahvor.s);
	    for (i=0; i<12; i++) {
		for (j=0; j<12; j++) {
		    s12[i][j] = cmod2->ext.cahvor.s[i][j];
		    }
		}
	    cmod_cahv_internal(
		cmod2->core.u.cahvor.c, cmod2->core.u.cahvor.a,
		cmod2->core.u.cahvor.h, cmod2->core.u.cahvor.v,
		s12,
		&(cmod2->ext.cahvor.hs), &(cmod2->ext.cahvor.hc),
		&(cmod2->ext.cahvor.vs), &(cmod2->ext.cahvor.vc),
		&(cmod2->ext.cahvor.theta), (cmod2->ext.cahvor.s_int));
	    break;

	/* CAHVORE */
	case CMOD_CLASS_CAHVORE:
	    cmod_cahvore_reflect_cov(((cmod_ext_t *)cmod1)->ext.cahvore.s, n,
					cmod2->ext.cahvore.s);
	    for (i=0; i<12; i++) {
		for (j=0; j<12; j++) {
		    s12[i][j] = cmod2->ext.cahvore.s[i][j];
		    }
		}
	    cmod_cahv_internal(
		cmod2->core.u.cahvore.c, cmod2->core.u.cahvore.a,
		cmod2->core.u.cahvore.h, cmod2->core.u.cahvore.v,
		s12,
		&(cmod2->ext.cahvore.hs), &(cmod2->ext.cahvore.hc),
		&(cmod2->ext.cahvore.vs), &(cmod2->ext.cahvore.vc),
		&(cmod2->ext.cahvore.theta), (cmod2->ext.cahvore.s_int));
	    break;

	/* PSPH */
	case CMOD_CLASS_PSPH:
	    cmod_psph_internal(&cmod2->core.u.psph, &(cmod2->ext.psph.theta));
	    break;

	/* Unknown */
	default:
	    CMOD_ASSERT_1("cmod_reflect_ext", 0, cmod1->core.mclass);
	}
    }


/******************************************************************************
********************************   CMOD_SCALE   *******************************
*******************************************************************************

    This function scales a camera model. The scale factors should be
    understood as the same scale factors that would be applied to a 2D
    coordinate in the original model to convert it to a coordinate in
    the resulting model. The image dimensions are left undefined in the
    scaled image. */

void cmod_scale(
    cmod_float_t hscale,	/* input horizontal scale factor */
    cmod_float_t vscale,	/* input vertical   scale factor */
    const cmod_t *cmod1,	/* input initial core model */
    cmod_t *cmod2)		/* output final core model */
{
    /* Check input */
    CMOD_ASSERT("cmod_scale", cmod1 != NULL);
    CMOD_ASSERT("cmod_scale", cmod2 != NULL);

    /* Some of the model stays the same */
    if (cmod2 != cmod1) {
	*cmod2 = *cmod1;
	}

    /* Leave the image dimensions undefined */
    cmod2->xdim = -1;
    cmod2->ydim = -1;

    /* Proceed based on underlying model class */
    switch (cmod1->mclass) {

	/* CAHV */
	case CMOD_CLASS_CAHV:
	    cmod_cahv_scale(hscale, vscale,
		cmod1->u.cahv.h, cmod1->u.cahv.v, NULL,
		cmod2->u.cahv.h, cmod2->u.cahv.v, NULL);
	    break;

	/* CAHVOR */
	case CMOD_CLASS_CAHVOR:
	    cmod_cahvor_scale(hscale, vscale,
		cmod1->u.cahvor.h, cmod1->u.cahvor.v, NULL,
		cmod2->u.cahvor.h, cmod2->u.cahvor.v, NULL);
	    break;

	/* CAHVORE */
	case CMOD_CLASS_CAHVORE:
	    cmod_cahvore_scale(hscale, vscale,
		cmod1->u.cahvore.h, cmod1->u.cahvore.v, NULL,
		cmod2->u.cahvore.h, cmod2->u.cahvore.v, NULL);
	    break;

	/* PSPH */
	case CMOD_CLASS_PSPH:
	    cmod_psph_scale(hscale, vscale,
		&cmod1->u.psph, &cmod2->u.psph);
	    break;

	/* Unknown */
	default:
	    CMOD_ASSERT_1("cmod_scale", 0, cmod1->mclass);
	}
    }


/******************************************************************************
********************************   CMOD_SCALE_EXT   ***************************
*******************************************************************************

    This function scales a camera model. The scale factors should be
    understood as the same scale factors that would be applied to a 2D
    coordinate in the original model to convert it to a coordinate in
    the resulting model. The image dimensions are left undefined in the
    scaled image. */

void cmod_scale_ext(
    cmod_float_t hscale,	/* input horizontal scale factor */
    cmod_float_t vscale,	/* input vertical   scale factor */
    const cmod_ext_t *cmod1,	/* input initial extended model */
    cmod_ext_t *cmod2)		/* output final extended model */
{
    cmod_int_t i;
    cmod_int_t j;
    cmod_float_t s12[12][12];

    /* Check input */
    CMOD_ASSERT("cmod_scale_ext", cmod1 != NULL);
    CMOD_ASSERT("cmod_scale_ext", cmod2 != NULL);

    /* Some of the core model stays the same */
    if (cmod2 != cmod1) {
	cmod2->core = cmod1->core;
	}

    /* Leave the image dimensions undefined */
    cmod2->core.xdim = -1;
    cmod2->core.ydim = -1;

    /* Compute the transform of the core and extended parts of the model */
    switch (cmod1->core.mclass) {

	/* CAHV */
	case CMOD_CLASS_CAHV:
	    cmod_cahv_scale(hscale, vscale,
		cmod1->core.u.cahv.h, cmod1->core.u.cahv.v,
					((cmod_ext_t *)cmod1)->ext.cahv.s,
		cmod2->core.u.cahv.h, cmod2->core.u.cahv.v,
					cmod2->ext.cahv.s);
	    cmod_cahv_internal(
		cmod2->core.u.cahv.c, cmod2->core.u.cahv.a,
		cmod2->core.u.cahv.h, cmod2->core.u.cahv.v,
		cmod2->ext.cahv.s,
		&(cmod2->ext.cahv.hs), &(cmod2->ext.cahv.hc),
		&(cmod2->ext.cahv.vs), &(cmod2->ext.cahv.vc),
		&(cmod2->ext.cahv.theta), (cmod2->ext.cahv.s_int));
	    break;

	/* CAHVOR */
	case CMOD_CLASS_CAHVOR:
	    cmod_cahvor_scale(hscale, vscale,
		cmod1->core.u.cahvor.h, cmod1->core.u.cahvor.v,
					((cmod_ext_t *)cmod1)->ext.cahvor.s,
		cmod2->core.u.cahvor.h, cmod2->core.u.cahvor.v,
					cmod2->ext.cahvor.s);
	    for (i=0; i<12; i++) {
		for (j=0; j<12; j++) {
		    s12[i][j] = cmod2->ext.cahvor.s[i][j];
		    }
		}
	    cmod_cahv_internal(
		cmod2->core.u.cahvor.c, cmod2->core.u.cahvor.a,
		cmod2->core.u.cahvor.h, cmod2->core.u.cahvor.v,
		s12,
		&(cmod2->ext.cahvor.hs), &(cmod2->ext.cahvor.hc),
		&(cmod2->ext.cahvor.vs), &(cmod2->ext.cahvor.vc),
		&(cmod2->ext.cahvor.theta), (cmod2->ext.cahvor.s_int));
	    break;

	/* CAHVORE */
	case CMOD_CLASS_CAHVORE:
	    cmod_cahvore_scale(hscale, vscale,
		cmod1->core.u.cahvore.h, cmod1->core.u.cahvore.v,
					((cmod_ext_t *)cmod1)->ext.cahvore.s,
		cmod2->core.u.cahvore.h, cmod2->core.u.cahvore.v,
					cmod2->ext.cahvore.s);
	    for (i=0; i<12; i++) {
		for (j=0; j<12; j++) {
		    s12[i][j] = cmod2->ext.cahvore.s[i][j];
		    }
		}
	    cmod_cahv_internal(
		cmod2->core.u.cahvore.c, cmod2->core.u.cahvore.a,
		cmod2->core.u.cahvore.h, cmod2->core.u.cahvore.v,
		s12,
		&(cmod2->ext.cahvore.hs), &(cmod2->ext.cahvore.hc),
		&(cmod2->ext.cahvore.vs), &(cmod2->ext.cahvore.vc),
		&(cmod2->ext.cahvore.theta), (cmod2->ext.cahvore.s_int));
	    break;

	/* PSPH */
	case CMOD_CLASS_PSPH:
	    cmod_psph_scale(hscale, vscale,
		&cmod1->core.u.psph, &cmod2->core.u.psph);
	    cmod_psph_internal(&cmod2->core.u.psph, &(cmod2->ext.psph.theta));
	    break;

	/* Unknown */
	default:
	    CMOD_ASSERT_1("cmod_scale_ext", 0, cmod1->core.mclass);
	}
    }


/******************************************************************************
********************************   CMOD_SHIFT   *******************************
*******************************************************************************

    This function shifts a camera model. The shift values should be
    understood as the coordinates of the old model where the origin
    will fall in the new one. */

void cmod_shift(
    cmod_float_t dx,		/* input horizontal shift */
    cmod_float_t dy,		/* input vertical   shift */
    const cmod_t *cmod1,	/* input initial core model */
    cmod_t *cmod2)		/* output final core model */
{
    /* Check input */
    CMOD_ASSERT("cmod_shift", cmod1 != NULL);
    CMOD_ASSERT("cmod_shift", cmod2 != NULL);

    /* Some of the model stays the same */
    if (cmod2 != cmod1) {
	*cmod2 = *cmod1;
	}

    /* Proceed based on underlying model class */
    switch (cmod1->mclass) {

	/* CAHV */
	case CMOD_CLASS_CAHV:
	    cmod_cahv_shift(dx, dy,
		cmod1->u.cahv.a,
		cmod1->u.cahv.h, cmod1->u.cahv.v, NULL,
		cmod2->u.cahv.h, cmod2->u.cahv.v, NULL);
	    break;

	/* CAHVOR */
	case CMOD_CLASS_CAHVOR:
	    cmod_cahvor_shift(dx, dy,
		cmod1->u.cahvor.a,
		cmod1->u.cahvor.h, cmod1->u.cahvor.v, NULL,
		cmod2->u.cahvor.h, cmod2->u.cahvor.v, NULL);
	    break;

	/* CAHVORE */
	case CMOD_CLASS_CAHVORE:
	    cmod_cahvore_shift(dx, dy,
		cmod1->u.cahvore.a,
		cmod1->u.cahvore.h, cmod1->u.cahvore.v, NULL,
		cmod2->u.cahvore.h, cmod2->u.cahvore.v, NULL);
	    break;

	/* PSPH */
	case CMOD_CLASS_PSPH:
	    cmod_psph_shift(dx, dy,
		&cmod1->u.psph, &cmod2->u.psph);
	    break;

	/* Unknown */
	default:
	    CMOD_ASSERT_1("cmod_shift", 0, cmod1->mclass);
	}
    }


/******************************************************************************
********************************   CMOD_SHIFT_EXT   ***************************
*******************************************************************************

    This function shifts a camera model. The shift values should be
    understood as the coordinates of the old model where the origin
    will fall in the new one. */

void cmod_shift_ext(
    cmod_float_t dx,		/* input horizontal shift */
    cmod_float_t dy,		/* input vertical   shift */
    const cmod_ext_t *cmod1,	/* input initial extended model */
    cmod_ext_t *cmod2)		/* output final extended model */
{
    cmod_int_t i;
    cmod_int_t j;
    cmod_float_t s12[12][12];

    /* Check input */
    CMOD_ASSERT("cmod_shift_ext", cmod1 != NULL);
    CMOD_ASSERT("cmod_shift_ext", cmod2 != NULL);

    /* Some of the core model stays the same */
    if (cmod2 != cmod1) {
	cmod2->core = cmod1->core;
	}

    /* Proceed based on underlying model class */
    switch (cmod1->core.mclass) {

	/* CAHV */
	case CMOD_CLASS_CAHV:
	    cmod_cahv_shift(dx, dy,
			cmod1->core.u.cahv.a,
			cmod1->core.u.cahv.h,
			cmod1->core.u.cahv.v,
			((cmod_ext_t *)cmod1)->ext.cahv.s,
			cmod2->core.u.cahv.h,
			cmod2->core.u.cahv.v,
			cmod2->ext.cahv.s);
	    cmod_cahv_internal(
		cmod2->core.u.cahv.c, cmod2->core.u.cahv.a,
		cmod2->core.u.cahv.h, cmod2->core.u.cahv.v,
		cmod2->ext.cahv.s,
		&(cmod2->ext.cahv.hs), &(cmod2->ext.cahv.hc),
		&(cmod2->ext.cahv.vs), &(cmod2->ext.cahv.vc),
		&(cmod2->ext.cahv.theta), (cmod2->ext.cahv.s_int));
	    break;

	/* CAHVOR */
	case CMOD_CLASS_CAHVOR:
	    cmod_cahvor_shift(dx, dy,
			cmod1->core.u.cahvor.a,
			cmod1->core.u.cahvor.h,
			cmod1->core.u.cahvor.v,
			((cmod_ext_t *)cmod1)->ext.cahvor.s,
			cmod2->core.u.cahvor.h,
			cmod2->core.u.cahvor.v,
			cmod2->ext.cahvor.s);
	    for (i=0; i<12; i++) {
		for (j=0; j<12; j++) {
		    s12[i][j] = cmod2->ext.cahvor.s[i][j];
		    }
		}
	    cmod_cahv_internal(
		cmod2->core.u.cahvor.c, cmod2->core.u.cahvor.a,
		cmod2->core.u.cahvor.h, cmod2->core.u.cahvor.v,
		s12,
		&(cmod2->ext.cahvor.hs), &(cmod2->ext.cahvor.hc),
		&(cmod2->ext.cahvor.vs), &(cmod2->ext.cahvor.vc),
		&(cmod2->ext.cahvor.theta), (cmod2->ext.cahvor.s_int));
	    break;

	/* CAHVORE */
	case CMOD_CLASS_CAHVORE:
	    cmod_cahvore_shift(dx, dy,
			cmod1->core.u.cahvore.a,
			cmod1->core.u.cahvore.h,
			cmod1->core.u.cahvore.v,
			((cmod_ext_t *)cmod1)->ext.cahvore.s,
			cmod2->core.u.cahvore.h,
			cmod2->core.u.cahvore.v,
			cmod2->ext.cahvore.s);
	    for (i=0; i<12; i++) {
		for (j=0; j<12; j++) {
		    s12[i][j] = cmod2->ext.cahvore.s[i][j];
		    }
		}
	    cmod_cahv_internal(
		cmod2->core.u.cahvore.c, cmod2->core.u.cahvore.a,
		cmod2->core.u.cahvore.h, cmod2->core.u.cahvore.v,
		s12,
		&(cmod2->ext.cahvore.hs), &(cmod2->ext.cahvore.hc),
		&(cmod2->ext.cahvore.vs), &(cmod2->ext.cahvore.vc),
		&(cmod2->ext.cahvore.theta), (cmod2->ext.cahvore.s_int));
	    break;

	/* PSPH */
	case CMOD_CLASS_PSPH:
	    cmod_psph_shift(dx, dy,
		&cmod1->core.u.psph, &cmod2->core.u.psph);
	    cmod_psph_internal(&cmod2->core.u.psph, &(cmod2->ext.psph.theta));
	    break;

	/* Unknown */
	default:
	    CMOD_ASSERT_1("cmod_shift_ext", 0, cmod1->core.mclass);
	}
    }


/******************************************************************************
********************************   CMOD_VALIDATE   ****************************
*******************************************************************************

    This function validates the components of a camera model to determine
    whether or not the model makes some kind of sense. Only generic
    sanity-checking tests are performed. It is an option whether or not the
    "none" class is considered acceptable; if so and if the model's class is
    indeed none, then no other checks are performed. It is also an option
    whether or not it is acceptable for the image dimensions to be missing. */

cmod_stat_t cmod_validate(
    const cmod_t *cmod,		/* input core model */
    cmod_bool_t noclass,	/* input if class "none" (no model) is ok */
    cmod_bool_t nodims)		/* input if missing dimensions is ok */
{
    /* Check input */
    CMOD_ASSERT("cmod_validate", cmod != NULL);

    /* Check for model class "none" */
    if (noclass && (cmod->mclass == CMOD_CLASS_NONE)) {
	return SUCCESS;
	}

    /* Check model dimensions if requested */
    if (!nodims && ((cmod->xdim <= 0) || (cmod->ydim <= 0))) {
	CMOD_ERROR_II("cmod_validate", "Bad model dimensions",
			cmod->xdim, cmod->ydim);
	return FAILURE;
	}

    /* Proceed based on underlying model class */
    switch (cmod->mclass) {

	/* CAHV */
	case CMOD_CLASS_CAHV:
	    return cmod_cahv_validate(
		cmod->u.cahv.c, cmod->u.cahv.a, cmod->u.cahv.h, cmod->u.cahv.v);

	/* CAHVOR */
	case CMOD_CLASS_CAHVOR:
	    return cmod_cahvor_validate(
		cmod->u.cahvor.c, cmod->u.cahvor.a, cmod->u.cahvor.h,
		cmod->u.cahvor.v, cmod->u.cahvor.o, cmod->u.cahvor.r);

	/* CAHVORE */
	case CMOD_CLASS_CAHVORE:
	    return cmod_cahvore_validate(
		cmod->u.cahvore.mtype, cmod->u.cahvore.mparm,
		cmod->u.cahvore.c, cmod->u.cahvore.a, cmod->u.cahvore.h,
		cmod->u.cahvore.v, cmod->u.cahvore.o, cmod->u.cahvore.r,
		cmod->u.cahvore.e);

	/* PSPH */
	case CMOD_CLASS_PSPH:
	    return cmod_psph_validate(&cmod->u.psph);

	/* Unknown */
	default:
	    CMOD_ERROR_I("cmod_validate", "Bad model class", cmod->mclass);
	    return FAILURE;
	}
    }
