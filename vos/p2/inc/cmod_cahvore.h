/******************************************************************************
*                                                                             *
*                                 C A H V O R E                               *
*                                                                             *
*                                       Todd Litwin                           *
*                                       Written:  5 Jun 2003                  *
*                                       Updated: 22 Jul 2016                  *
*                                                                             *
*                                       Copyright (C) 2003, 2005, 2006, 2009, *
*                                                     2010, 2012, 2015, 2016  *
*                                       California Institute of Technology    *
*                                       All Rights Reserved                   *
*                                                                             *
*******************************************************************************


	This file contains declarations for the CAHVORE camera model. */

#ifndef CMOD_CAHVORE_H
#define CMOD_CAHVORE_H

#include "cmod.h"

/* CAHVORE model type */
typedef enum {
    CMOD_CAHVORE_TYPE_NONE,
    CMOD_CAHVORE_TYPE_PERSPECTIVE,
    CMOD_CAHVORE_TYPE_FISHEYE,
    CMOD_CAHVORE_TYPE_GENERAL
    } cmod_cahvore_type_t;

#ifdef	__cplusplus
extern "C" {
#endif

#ifdef __STDC__

/******************************************************************************

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
    cmod_float_t upar[3][2]);	/* output partial derivative of uvec3 to pos2 */

/******************************************************************************

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
    cmod_float_t par[2][3]);	/* output partial derivative of pos2 to pos3 */

/******************************************************************************

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
    cmod_float_t par[2][3]);	/* output derivative matrix of pos2 to uvec3 */

/******************************************************************************

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
    cmod_float_t *theta);	/* output angle between axes */

/******************************************************************************

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
    cmod_float_t e_f[3]);	/* output final model pupil  terms  E */

/******************************************************************************

    This function reads a CAHVORE model from a text file, whose format is
    compatible with what is put out by the program CCALADJ. */

cmod_stat_t cmod_cahvore_read(
    const char *filename,	/* input filename */
    cmod_int_t *xdim,		/* output number of columns */
    cmod_int_t *ydim,		/* output number of rows */
    cmod_int_t *mtype,		/* output type of model */
    cmod_float_t *mparm,	/* output model parameter */
    cmod_float_t c[3],		/* output model center vector C */
    cmod_float_t a[3],		/* output model axis   vector A */
    cmod_float_t h[3],		/* output model horiz. vector H */
    cmod_float_t v[3],		/* output model vert.  vector V */
    cmod_float_t o[3],		/* output model optical axis unit vector O */
    cmod_float_t r[3],		/* output model radial-distortion terms  R */
    cmod_float_t e[3],		/* output model entrance-pupil    terms  E */
    cmod_float_t s[21][21],	/* output covariance of CAHVORE, or NULL */
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
    cmod_bool_t *behind);	/* output if camera behind reflecting plane */

/******************************************************************************

    This function reflects the covariance matrix to correspond to the
    reflected model. */

void cmod_cahvore_reflect_cov(
    cmod_float_t s_i[21][21],	/* input initial covariance */
    const cmod_float_t n[3],	/* input normal to the reflecting plane */
    cmod_float_t s_f[21][21]);	/* output final covariance */

/******************************************************************************

    This function rotates a CAHVORE model's covariance matrix to correspond to
    rotation of the model itself. Note that model translations do not affect
    the covariance matrix. */

void cmod_cahvore_rot_cov(
    cmod_float_t r_i[3][3],	/* input initial orientation of camera ref pt */
    cmod_float_t s_i[21][21],	/* input initial covariance */
    cmod_float_t r_f[3][3],	/* input final orientation of camera ref pt */
    cmod_float_t s_f[21][21]);	/* output final covariance */

/******************************************************************************

    This function rotates a CAHVORE model's covariance matrix to correspond to
    rotation of the model itself. Note that model translations do not affect
    the covariance matrix. */

void cmod_cahvore_rotate_cov(
    const cmod_float_t q_i[4],	/* input initial orientation of camera ref pt */
    cmod_float_t s_i[21][21],	/* input initial covariance */
    const cmod_float_t q_f[4],	/* input final orientation of camera ref pt */
    cmod_float_t s_f[21][21]);	/* output final covariance */

/******************************************************************************

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
    cmod_float_t s2[21][21]);	/* output covariance matrix, or NULL */

/******************************************************************************

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
    cmod_float_t s2[21][21]);	/* output covariance matrix, or NULL */

/******************************************************************************

    This function transform a CAHVORE model's covariance matrix according to a
    3x3 matrix that represents the transformation to the 3D coordinates of
    the model. Note that model translations do not affect the covariance
    matrix. */

void cmod_cahvore_transform_cov(
    cmod_float_t s_i[21][21],	/* input initial covariance */
    cmod_float_t r[3][3],	/* input transform matrix of camera ref pt */
    cmod_float_t s_f[21][21]);	/* output final covariance */

/******************************************************************************

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
    const cmod_float_t e[3]);	/* input model entrance-pupil    terms  E */

/******************************************************************************

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
    cmod_float_t pos2[2]);	/* output 2D position for CAHVORE */

/******************************************************************************

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
    cmod_float_t pos2[2]);	/* output 2D position for CAHVORE */

/******************************************************************************

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
    cmod_float_t *theta);	/* output angle between axes */

/******************************************************************************

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
    cmod_float_t *theta);	/* output angle between axes */

/******************************************************************************

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
    cmod_float_t *theta);	/* output angle between axes */

/******************************************************************************

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
    cmod_psph_t *psph2);	/* output PSPH model 2 */

/******************************************************************************

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
    cmod_float_t pos2[2]);	/* output 2D position for CAHV */

/******************************************************************************

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
    cmod_float_t pos2[2]);	/* output 2D position for CAHV */

/******************************************************************************

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
    cmod_float_t pos2[2]);	/* output 2D position for PSPH */

/******************************************************************************

    This function writes a CAHVORE model to a text file, whose format is
    compatible with what is put out by the program CCALADJ. */

cmod_stat_t cmod_cahvore_write(
    const char *filename,	/* input filename */
    const char *comment,	/* input one-line comment to record in file */
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
    cmod_float_t s[21][21],	/* input covariance of CAHVORE, or NULL */
    cmod_float_t hs,		/* input horizontal scale factor */
    cmod_float_t hc,		/* input horizontal center */
    cmod_float_t vs,		/* input vertical scale factor */
    cmod_float_t vc,		/* input vertical center */
    cmod_float_t theta,		/* input angle between axes */
    cmod_float_t s_int[5][5]);	/* input covariance matrix, or NULL */

/*****************************************************************************/

#else

void        cmod_cahvore_2d_to_3d();
void        cmod_cahvore_3d_to_2d();
void        cmod_cahvore_3d_to_2d_point();
void        cmod_cahvore_align_models();
void        cmod_cahvore_move();
cmod_stat_t cmod_cahvore_read();
void        cmod_cahvore_reflect();
void        cmod_cahvore_reflect_cov();
void        cmod_cahvore_rot_cov();
void        cmod_cahvore_rotate_cov();
void        cmod_cahvore_scale();
void        cmod_cahvore_shift();
void        cmod_cahvore_transform_cov();
cmod_stat_t cmod_cahvore_validate();
void        cmod_cahvore_warp_from_cahv();
void        cmod_cahvore_warp_from_psph();
void        cmod_cahvore_warp_model();
void        cmod_cahvore_warp_models();
void        cmod_cahvore_warp_models2();
void        cmod_cahvore_warp_models3();
void        cmod_cahvore_warp_to_cahv();
void        cmod_cahvore_warp_to_cahvore();
void        cmod_cahvore_warp_to_psph();
cmod_stat_t cmod_cahvore_write();

#endif

#ifdef	__cplusplus
}
#endif

#endif
