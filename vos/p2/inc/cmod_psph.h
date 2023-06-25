/******************************************************************************
*                                                                             *
*                                    P S P H                                  *
*                                                                             *
*                                       Todd Litwin                           *
*                                       Written:  5 Aug 2002                  *
*                                       Updated: 24 May 2016                  *
*                                                                             *
*                                       Copyright (C) 2002, 2003, 2016        *
*                                       California Institute of Technology    *
*                                       All Rights Reserved                   *
*                                                                             *
*******************************************************************************


	This file contains declarations for the Planospheric camera
	model. */

#ifndef CMOD_PSPH_H
#define CMOD_PSPH_H

#include "cmod.h"

#ifdef	__cplusplus
extern "C" {
#endif

#ifdef __STDC__

/******************************************************************************

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
    cmod_float_t par[3][2]);	/* output partial-derivative uvec3/pos2 */

/******************************************************************************

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
    cmod_float_t par[2][3]);	/* output partial-derivative pos2/pos3 */

/******************************************************************************

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
    cmod_psph_t *psph);		/* output camera model */

/******************************************************************************

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
    cmod_psph_t *psph);		/* output camera model */

/******************************************************************************

    This function calculates the internal camera model parameters for the
    PSPH camera model. Note: the main purpose for providing this function
    is to follow the pattern established for the other model classes, not
    because there is much benefit to having it in and of itself. */

void cmod_psph_internal(
    const cmod_psph_t *psph,	/* input camera model */
    cmod_float_t *theta);	/* output angle between axes */

/******************************************************************************

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
    cmod_float_t ydir[3]);	/* output row direction */

/******************************************************************************

    This function relocates a camera model, based on the initial and final
    positions and orientations of a camera platform reference point, a point
    that is rigidly connected to the camera, but is otherwise arbitrary. */

void cmod_psph_move(
    const cmod_float_t p_i[3],	/* input initial pos of camera ref pt */
    const cmod_float_t q_i[4],	/* input initial orientation of camera ref pt */
    const cmod_psph_t *psph_i,	/* input camera model */
    const cmod_float_t p_f[3],	/* input final pos of camera ref pt */
    const cmod_float_t q_f[4],	/* input final orientation of camera ref pt */
    cmod_psph_t *psph_f);	/* output camera model */

/******************************************************************************

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
    cmod_float_t r[3][3]);	/* output rotation matrix */

/******************************************************************************

    This function reads a PSPH model from a text file. Note that image
    dimensions might be missing from the file, in which case xdim and ydim will
    be less than zero. */

cmod_stat_t cmod_psph_read(
    const char *filename,	/* input filename */
    cmod_int_t *xdim,		/* output number of columns */
    cmod_int_t *ydim,		/* output number of rows */
    cmod_psph_t *psph);		/* output camera model */

/******************************************************************************

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
    cmod_bool_t *behind);	/* output if camera behind reflecting plane */

/******************************************************************************

    This function scales a camera model. The scale factors should be
    understood as the same scale factors that would be applied to a 2D
    coordinate in the original model to convert it to a coordinate in
    the resulting model. */

void cmod_psph_scale(
    cmod_float_t hscale,	/* input horizontal scale factor */
    cmod_float_t vscale,	/* input vertical   scale factor */
    const cmod_psph_t *psph_i,	/* input camera model */
    cmod_psph_t *psph_f);	/* output camera model */

/******************************************************************************

    This function shifts a camera model. The shift values should be
    understood as the coordinates of the old model where the origin
    will fall in the new one. */

void cmod_psph_shift(
    cmod_float_t dx,		/* input horizontal shift */
    cmod_float_t dy,		/* input vertical   shift */
    const cmod_psph_t *psph_i,	/* input camera model */
    cmod_psph_t *psph_f);	/* output camera model */

/******************************************************************************

    This function validates the components of a camera model to determine
    whether or not the model makes some kind of sense. Only generic
    sanity-checking tests are performed. */

cmod_stat_t cmod_psph_validate(
    const cmod_psph_t *psph);	/* input camera model */

/******************************************************************************

    This function writes a PSPH model to a text file. */

cmod_stat_t cmod_psph_write(
    const char *filename,	/* input filename */
    const char *comment,	/* input one-line comment to record in file */
    cmod_int_t xdim,		/* input number of columns, or < 0 */
    cmod_int_t ydim,		/* input number of rows, or < 0 */
    const cmod_psph_t *psph);	/* input camera model */

/*****************************************************************************/

#else

void        cmod_psph_2d_to_3d();
void        cmod_psph_3d_to_2d();
cmod_stat_t cmod_psph_create();
cmod_stat_t cmod_psph_create2();
void        cmod_psph_internal();
void        cmod_psph_iplane();
void        cmod_psph_move();
void        cmod_psph_pose();
cmod_stat_t cmod_psph_read();
void        cmod_psph_reflect();
void        cmod_psph_scale();
void        cmod_psph_shift();
cmod_stat_t cmod_psph_validate();
cmod_stat_t cmod_psph_write();

#endif

#ifdef	__cplusplus
}
#endif

#endif
