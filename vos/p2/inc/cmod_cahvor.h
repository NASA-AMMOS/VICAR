/******************************************************************************
*                                                                             *
*                                  C A H V O R                                *
*                                                                             *
*                                       Todd Litwin                           *
*                                       Written:  5 Jun 2003                  *
*                                       Updated: 24 May 2016                  *
*                                                                             *
*                                       Copyright (C) 2003, 2006, 2009, 2010, *
*                                                     2012, 2015, 2016        *
*                                       California Institute of Technology    *
*                                       All Rights Reserved                   *
*                                                                             *
*******************************************************************************


	This file contains declarations for the CAHVOR camera model. */

#ifndef CMOD_CAHVOR_H
#define CMOD_CAHVOR_H

#include "cmod.h"

#ifdef	__cplusplus
extern "C" {
#endif

#ifdef __STDC__

/******************************************************************************

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
    cmod_float_t par[3][2]);	/* output partial derivative of uvec3 to pos2 */

/******************************************************************************

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
    cmod_float_t par[2][3]);	/* output partial derivative of pos2 to pos3 */

/******************************************************************************

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
    cmod_float_t par[2][3]);	/* output derivative matrix of pos2 to uvec3 */

/******************************************************************************

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
    cmod_float_t r_f[3]);	/* output final model radial terms  R */

/******************************************************************************

    This function reads a CAHVOR model from a text file, whose format is
    compatible with what is put out by the program CCALADJ. */

cmod_stat_t cmod_cahvor_read(
    const char *filename,	/* input filename */
    cmod_float_t c[3],		/* output model center vector C */
    cmod_float_t a[3],		/* output model axis   vector A */
    cmod_float_t h[3],		/* output model horiz. vector H */
    cmod_float_t v[3],		/* output model vert.  vector V */
    cmod_float_t o[3],		/* output model optical axis unit vector O */
    cmod_float_t r[3],		/* output model radial-distortion terms  R */
    cmod_float_t s[18][18],	/* output covariance of CAHVOR, or NULL */
    cmod_float_t *hs,		/* output horizontal scale factor */
    cmod_float_t *hc,		/* output horizontal center */
    cmod_float_t *vs,		/* output vertical scale factor */
    cmod_float_t *vc,		/* output vertical center */
    cmod_float_t *theta,	/* output angle between axes */
    cmod_float_t s_int[5][5]);	/* output covariance matrix, or NULL */

/******************************************************************************

    This function reads a CAHVOR model from a text file, whose format is
    compatible with what is put out by the program CCALADJ. Note that some
    older model files do not contain the X and Y dimensions of the image;
    in this case negative values will be returned. */

cmod_stat_t cmod_cahvor_read2(
    const char *filename,	/* input filename */
    cmod_int_t *xdim,		/* output number of columns */
    cmod_int_t *ydim,		/* output number of rows */
    cmod_float_t c[3],		/* output model center vector C */
    cmod_float_t a[3],		/* output model axis   vector A */
    cmod_float_t h[3],		/* output model horiz. vector H */
    cmod_float_t v[3],		/* output model vert.  vector V */
    cmod_float_t o[3],		/* output model optical axis unit vector O */
    cmod_float_t r[3],		/* output model radial-distortion terms  R */
    cmod_float_t s[18][18],	/* output covariance of CAHVOR, or NULL */
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
    cmod_bool_t *behind);	/* output if camera behind reflecting plane */

/******************************************************************************

    This function reflects the covariance matrix to correspond to the
    reflected model. */

void cmod_cahvor_reflect_cov(
    cmod_float_t s_i[18][18],	/* input initial covariance */
    const cmod_float_t n[3],	/* input normal to the reflecting plane */
    cmod_float_t s_f[18][18]);	/* output final covariance */

/******************************************************************************

    This function rotates a CAHVOR model's covariance matrix to correspond to
    rotation of the model itself. Note that model translations do not affect
    the covariance matrix. */

void cmod_cahvor_rot_cov(
    cmod_float_t r_i[3][3],	/* input initial orientation of camera ref pt */
    cmod_float_t s_i[18][18],	/* input initial covariance */
    cmod_float_t r_f[3][3],	/* input final orientation of camera ref pt */
    cmod_float_t s_f[18][18]);	/* output final covariance */

/******************************************************************************

    This function rotates a CAHVOR model's covariance matrix to correspond to
    rotation of the model itself. Note that model translations do not affect
    the covariance matrix. */

void cmod_cahvor_rotate_cov(
    const cmod_float_t q_i[4],	/* input initial orientation of camera ref pt */
    cmod_float_t s_i[18][18],	/* input initial covariance */
    const cmod_float_t q_f[4],	/* input final orientation of camera ref pt */
    cmod_float_t s_f[18][18]);	/* output final covariance */

/******************************************************************************

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
    cmod_float_t s2[18][18]);	/* output covariance matrix, or NULL */

/******************************************************************************

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
    cmod_float_t s2[18][18]);	/* output covariance matrix, or NULL */

/******************************************************************************

    This function transform a CAHVOR model's covariance matrix according to a
    3x3 matrix that represents the transformation to the 3D coordinates of
    the model. Note that model translations do not affect the covariance
    matrix. */

void cmod_cahvor_transform_cov(
    cmod_float_t s_i[18][18],	/* input initial covariance */
    cmod_float_t r[3][3],	/* input transform matrix of camera ref pt */
    cmod_float_t s_f[18][18]);	/* output final covariance */

/******************************************************************************

    This function validates the components of a camera model to determine
    whether or not the model makes some kind of sense. Only generic
    sanity-checking tests are performed. */

cmod_stat_t cmod_cahvor_validate(
    const cmod_float_t c[3],	/* input model center vector C */
    const cmod_float_t a[3],	/* input model axis   vector A */
    const cmod_float_t h[3],	/* input model horiz. vector H */
    const cmod_float_t v[3],	/* input model vert.  vector V */
    const cmod_float_t o[3],	/* input model optical axis unit vector O */
    const cmod_float_t r[3]);	/* input model radial-distortion terms  R */

/******************************************************************************

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
    cmod_float_t pos2[2]);	/* output 2D position for CAHVOR */

/******************************************************************************

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
    cmod_float_t *theta);	/* output angle between axes */

/******************************************************************************

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
    cmod_float_t *theta);	/* output angle between axes */

/******************************************************************************

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
    cmod_float_t *theta);	/* output angle between axes */

/******************************************************************************

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
    cmod_float_t *theta);	/* output angle between axes */

/******************************************************************************

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
    cmod_float_t pos2[2]);	/* output 2D position for CAHV */

/******************************************************************************

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
    cmod_float_t pos2[2]);	/* output 2D position for CAHV */

/******************************************************************************

    This function writes a CAHVOR model to a text file, whose format is
    compatible with what is put out by the program CCALADJ. */

cmod_stat_t cmod_cahvor_write(
    const char *filename,	/* input filename */
    const char *comment,	/* input one-line comment to record in file */
    const cmod_float_t c[3],	/* input model center vector C */
    const cmod_float_t a[3],	/* input model axis   vector A */
    const cmod_float_t h[3],	/* input model horiz. vector H */
    const cmod_float_t v[3],	/* input model vert.  vector V */
    const cmod_float_t o[3],	/* input model optical axis unit vector O */
    const cmod_float_t r[3],	/* input model radial-distortion terms  R */
    cmod_float_t s[18][18],	/* input covariance of CAHVOR, or NULL */
    cmod_float_t hs,		/* input horizontal scale factor */
    cmod_float_t hc,		/* input horizontal center */
    cmod_float_t vs,		/* input vertical scale factor */
    cmod_float_t vc,		/* input vertical center */
    cmod_float_t theta,		/* input angle between axes */
    cmod_float_t s_int[5][5]);	/* input covariance matrix, or NULL */

/******************************************************************************

    This function writes a CAHVOR model to a text file, whose format is
    compatible with what is put out by the program CCALADJ. */

cmod_stat_t cmod_cahvor_write2(
    const char *filename,	/* input filename */
    const char *comment,	/* input one-line comment to record in file */
    cmod_int_t xdim,		/* input number of columns */
    cmod_int_t ydim,		/* input number of rows */
    const cmod_float_t c[3],	/* input model center vector C */
    const cmod_float_t a[3],	/* input model axis   vector A */
    const cmod_float_t h[3],	/* input model horiz. vector H */
    const cmod_float_t v[3],	/* input model vert.  vector V */
    const cmod_float_t o[3],	/* input model optical axis unit vector O */
    const cmod_float_t r[3],	/* input model radial-distortion terms  R */
    cmod_float_t s[18][18],	/* input covariance of CAHVOR, or NULL */
    cmod_float_t hs,		/* input horizontal scale factor */
    cmod_float_t hc,		/* input horizontal center */
    cmod_float_t vs,		/* input vertical scale factor */
    cmod_float_t vc,		/* input vertical center */
    cmod_float_t theta,		/* input angle between axes */
    cmod_float_t s_int[5][5]);		/* input covariance matrix, or NULL */

/*****************************************************************************/

#else

void        cmod_cahvor_2d_to_3d();
void        cmod_cahvor_3d_to_2d();
void        cmod_cahvor_3d_to_2d_point();
void        cmod_cahvor_move();
cmod_stat_t cmod_cahvor_read();
cmod_stat_t cmod_cahvor_read2();
void        cmod_cahvor_reflect();
void        cmod_cahvor_reflect_cov();
void        cmod_cahvor_rot_cov();
void        cmod_cahvor_rotate_cov();
void        cmod_cahvor_scale();
void        cmod_cahvor_shift();
void        cmod_cahvor_transform_cov();
cmod_stat_t cmod_cahvor_validate();
void        cmod_cahvor_warp_from_cahv();
void        cmod_cahvor_warp_model();
void        cmod_cahvor_warp_models();
void        cmod_cahvor_warp_models_nodims();
void        cmod_cahvor_warp_models2();
void        cmod_cahvor_warp_to_cahv();
void        cmod_cahvor_warp_to_cahvor();
cmod_stat_t cmod_cahvor_write();
cmod_stat_t cmod_cahvor_write2();

#endif

#ifdef	__cplusplus
}
#endif

#endif
