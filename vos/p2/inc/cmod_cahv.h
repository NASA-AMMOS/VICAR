/******************************************************************************
*                                                                             *
*                                    C A H V                                  *
*                                                                             *
*                                       Todd Litwin                           *
*                                       Written:  5 Jun 2003                  *
*                                       Updated: 24 May 2016                  *
*                                                                             *
*                                       Copyright (C) 2003, 2005, 2009, 2010, *
*                                                     2012, 2016              *
*                                       California Institute of Technology    *
*                                       All Rights Reserved                   *
*                                                                             *
*******************************************************************************


	This file contains declarations for the Yakimovsky & Cunningham
	camera model. */

#ifndef CMOD_CAHV_H
#define CMOD_CAHV_H

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
    cmod_float_t par[3][2]);	/* output partial derivative of uvec3 to pos2 */

/******************************************************************************

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
    cmod_float_t par[2][3]);	/* output partial derivative of pos2 to pos3 */

/******************************************************************************

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
    cmod_float_t par[4][3]);	/* output derivative of pos2,uvec2 to uvec3 */

/******************************************************************************

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
    cmod_float_t c[3],		/* output model center vector C */
    cmod_float_t a[3],		/* output model axis   vector A */
    cmod_float_t h[3],		/* output model horiz. vector H */
    cmod_float_t v[3]);		/* output model vert.  vector V */

/******************************************************************************

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
    cmod_float_t xfov,		/* input X field of view (rad) */
    cmod_float_t yfov,		/* input Y field of view (rad) */
    cmod_float_t xdim,		/* input X image dimension */
    cmod_float_t ydim,		/* input Y image dimension */
    cmod_float_t xc,		/* input X coordinate of image center */
    cmod_float_t yc,		/* input Y coordinate of image center */
    cmod_float_t c[3],		/* output model center vector C */
    cmod_float_t a[3],		/* output model axis   vector A */
    cmod_float_t h[3],		/* output model horiz. vector H */
    cmod_float_t v[3]);		/* output model vert.  vector V */

/******************************************************************************

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
    cmod_float_t s_int[5][5]);	/* output covariance matrix */

/******************************************************************************

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
    cmod_float_t *vc);		/* output vertical center */

/******************************************************************************

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
    cmod_float_t v_f[3]);	/* output final model vert.  vector V */

/******************************************************************************

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
    cmod_float_t r[3][3]);	/* output rotation matrix */

/******************************************************************************

    This function returns the rotation matrix of orientation for the given
    model. The absolute orientation is based on a reference orientation of
    the camera pointing straight down the Y axis, with Z up. The left- or
    right-handedness of the vectors is preserved, with the normal case being
    considered right-handed. */

void cmod_cahv_posture(
    const cmod_float_t a[3],	/* input model axis   vector A */
    const cmod_float_t h[3],	/* input model horiz. vector H */
    const cmod_float_t v[3],	/* input model vert.  vector V */
    cmod_float_t r[3][3]);	/* output rotation matrix */

/******************************************************************************

    This function reads a CAHV model from a text file, whose format is
    compatible with what is put out by the program CCALADJ. */

cmod_stat_t cmod_cahv_read(
    const char *filename,	/* input filename */
    cmod_float_t c[3],		/* output model center vector C */
    cmod_float_t a[3],		/* output model axis   vector A */
    cmod_float_t h[3],		/* output model horiz. vector H */
    cmod_float_t v[3],		/* output model vert.  vector V */
    cmod_float_t s[12][12],	/* output covariance of CAHV, or NULL */
    cmod_float_t *hs,		/* output horizontal scale factor */
    cmod_float_t *hc,		/* output horizontal center */
    cmod_float_t *vs,		/* output vertical scale factor */
    cmod_float_t *vc,		/* output vertical center */
    cmod_float_t *theta,	/* output angle between axes */
    cmod_float_t s_int[5][5]);	/* output covariance matrix, or NULL */

/******************************************************************************

    This function reads a CAHV model from a text file, whose format is
    compatible with what is put out by the program CCALADJ. Note that some
    older model files do not contain the X and Y dimensions of the image;
    in this case negative values will be returned. */

cmod_stat_t cmod_cahv_read2(
    const char *filename,	/* input filename */
    cmod_int_t *xdim,		/* output number of columns */
    cmod_int_t *ydim,		/* output number of rows */
    cmod_float_t c[3],		/* output model center vector C */
    cmod_float_t a[3],		/* output model axis   vector A */
    cmod_float_t h[3],		/* output model horiz. vector H */
    cmod_float_t v[3],		/* output model vert.  vector V */
    cmod_float_t s[12][12],	/* output covariance of CAHV, or NULL */
    cmod_float_t *hs,		/* output horizontal scale factor */
    cmod_float_t *hc,		/* output horizontal center */
    cmod_float_t *vs,		/* output vertical scale factor */
    cmod_float_t *vc,		/* output vertical center */
    cmod_float_t *theta,	/* output angle between axes */
    cmod_float_t s_int[5][5]);	/* output covariance matrix, or NULL */

/******************************************************************************

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
    cmod_bool_t *behind);	/* output if camera behind reflecting plane */

/******************************************************************************

    This function reflects the covariance matrix to correspond to the
    reflected model. */

void cmod_cahv_reflect_cov(
    cmod_float_t s_i[12][12],	/* input initial covariance */
    const cmod_float_t n[3],	/* input normal to the reflecting plane */
    cmod_float_t s_f[12][12]);	/* output final covariance */

/******************************************************************************

    This function rotates a CAHV model's covariance matrix to correspond to
    rotation of the model itself. Note that model translations do not affect
    the covariance matrix. */

void cmod_cahv_rot_cov(
    cmod_float_t r_i[3][3],	/* input initial orientation of camera ref pt */
    cmod_float_t s_i[12][12],	/* input initial covariance */
    cmod_float_t r_f[3][3],	/* input final orientation of camera ref pt */
    cmod_float_t s_f[12][12]);	/* output final covariance */

/******************************************************************************

    This function rotates a CAHV model's covariance matrix to correspond to
    rotation of the model itself. Note that model translations do not affect
    the covariance matrix. */

void cmod_cahv_rotate_cov(
    const cmod_float_t q_i[4],	/* input initial orientation of camera ref pt */
    cmod_float_t s_i[12][12],	/* input initial covariance */
    const cmod_float_t q_f[4],	/* input final orientation of camera ref pt */
    cmod_float_t s_f[12][12]);	/* output final covariance */

/******************************************************************************

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
    cmod_float_t s2[12][12]);	/* output covariance matrix, or NULL */

/******************************************************************************

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
    cmod_float_t s2[12][12]);	/* output covariance matrix, or NULL */

/******************************************************************************

    This function transform a CAHV model's covariance matrix according to a
    3x3 matrix that represents the transformation to the 3D coordinates of
    the model. Note that model translations do not affect the covariance
    matrix. */

void cmod_cahv_transform_cov(
    cmod_float_t s_i[12][12],	/* input initial covariance */
    cmod_float_t r[3][3],	/* input transform matrix of camera ref pt */
    cmod_float_t s_f[12][12]);	/* output final covariance */

/******************************************************************************

    This function validates the components of a camera model to determine
    whether or not the model makes some kind of sense. Only generic
    sanity-checking tests are performed. */

cmod_stat_t cmod_cahv_validate(
    const cmod_float_t c[3],	/* input model center vector C */
    const cmod_float_t a[3],	/* input model axis   vector A */
    const cmod_float_t h[3],	/* input model horiz. vector H */
    const cmod_float_t v[3]);	/* input model vert.  vector V */

/******************************************************************************

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
    cmod_float_t *theta);	/* output angle between axes */

/******************************************************************************

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
    cmod_float_t pos2[2]);	/* output 2D position for CAHV */

/******************************************************************************

    This function writes a CAHV model to a text file, whose format is
    compatible with what is put out by the program CCALADJ. */

cmod_stat_t cmod_cahv_write(
    const char *filename,	/* input filename */
    const char *comment,	/* input one-line comment to record in file */
    const cmod_float_t c[3],	/* input model center vector C */
    const cmod_float_t a[3],	/* input model axis   vector A */
    const cmod_float_t h[3],	/* input model horiz. vector H */
    const cmod_float_t v[3],	/* input model vert.  vector V */
    cmod_float_t s[12][12],	/* input covariance of CAHV, or NULL */
    cmod_float_t hs,		/* input horizontal scale factor */
    cmod_float_t hc,		/* input horizontal center */
    cmod_float_t vs,		/* input vertical scale factor */
    cmod_float_t vc,		/* input vertical center */
    cmod_float_t theta,		/* input angle between axes */
    cmod_float_t s_int[5][5]);	/* input covariance matrix, or NULL */

/******************************************************************************

    This function writes a CAHV model to a text file, whose format is
    compatible with what is put out by the program CCALADJ. */

cmod_stat_t cmod_cahv_write2(
    const char *filename,	/* input filename */
    const char *comment,	/* input one-line comment to record in file */
    cmod_int_t xdim,		/* input number of columns */
    cmod_int_t ydim,		/* input number of rows */
    const cmod_float_t c[3],	/* input model center vector C */
    const cmod_float_t a[3],	/* input model axis   vector A */
    const cmod_float_t h[3],	/* input model horiz. vector H */
    const cmod_float_t v[3],	/* input model vert.  vector V */
    cmod_float_t s[12][12],	/* input covariance of CAHV, or NULL */
    cmod_float_t hs,		/* input horizontal scale factor */
    cmod_float_t hc,		/* input horizontal center */
    cmod_float_t vs,		/* input vertical scale factor */
    cmod_float_t vc,		/* input vertical center */
    cmod_float_t theta,		/* input angle between axes */
    cmod_float_t s_int[5][5]);	/* input covariance matrix, or NULL */

/*****************************************************************************/

#else

void        cmod_cahv_2d_to_3d();
void        cmod_cahv_3d_to_2d();
void        cmod_cahv_3d_to_2d_ray();
cmod_stat_t cmod_cahv_create();
cmod_stat_t cmod_cahv_create2();
void        cmod_cahv_internal();
void        cmod_cahv_iplane();
void        cmod_cahv_move();
void        cmod_cahv_pose();
void        cmod_cahv_posture();
cmod_stat_t cmod_cahv_read();
cmod_stat_t cmod_cahv_read2();
void        cmod_cahv_reflect();
void        cmod_cahv_reflect_cov();
void        cmod_cahv_rot_cov();
void        cmod_cahv_rotate_cov();
void        cmod_cahv_scale();
void        cmod_cahv_shift();
void        cmod_cahv_transform_cov();
cmod_stat_t cmod_cahv_validate();
void        cmod_cahv_warp_models();
void        cmod_cahv_warp_to_cahv();
cmod_stat_t cmod_cahv_write();
cmod_stat_t cmod_cahv_write2();

#endif

#ifdef	__cplusplus
}
#endif

#endif
