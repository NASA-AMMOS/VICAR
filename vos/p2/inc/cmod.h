/******************************************************************************
*                                                                             *
*                                    C M O D                                  *
*                                                                             *
*                                       Todd Litwin                           *
*                                       Written: 27 Aug 2003                  *
*                                       Updated: 21 Oct 2022                  *
*                                                                             *
*                                       Copyright (C) 2003, 2005, 2006, 2009, *
*                                                     2010, 2012, 2016, 2022  *
*                                       California Institute of Technology    *
*                                       All Rights Reserved                   *
*                                                                             *
*******************************************************************************


	This file contains declarations for the composite camera-model
	utilities.

	This source code can be found here:

	    https://github.jpl.nasa.gov/telitwin/cmod

	Camera-calibration software, as well as descriptions of the model
	parameters and other reference materials, can be found here:

	    https://github.jpl.nasa.gov/telitwin/ccal

	*/

#ifndef CMOD_H
#define CMOD_H

/* Floating-point type, double by default */
#ifdef  CMOD_FLOAT
typedef CMOD_FLOAT cmod_float_t;
#else
typedef double     cmod_float_t;
#endif

/* Integer type, int by default */
#ifdef  CMOD_INTEGER
typedef CMOD_INTEGER cmod_int_t;
#else
typedef int          cmod_int_t;
#endif

/* Other integer types */
typedef cmod_int_t cmod_bool_t;	/* boolean */
typedef cmod_int_t cmod_stat_t;	/* status  */

/* Model classes */
typedef enum {
    CMOD_CLASS_NONE,
    CMOD_CLASS_CAHV,
    CMOD_CLASS_CAHVOR,
    CMOD_CLASS_CAHVORE,
    CMOD_CLASS_PSPH
    } cmod_class_t;

/* CAHV core model */
typedef struct cmod_cahv_t {
    cmod_float_t c[3];		/* model center vector C */
    cmod_float_t a[3];		/* model axis   vector A */
    cmod_float_t h[3];		/* model horiz. vector H */
    cmod_float_t v[3];		/* model vert.  vector V */
    } cmod_cahv_t;

/* CAHV extended model elements */
typedef struct cmod_cahv_ext_t {
    cmod_float_t s[12][12];	/* covariance of CAHV */
    cmod_float_t hs;		/* horizontal scale factor */
    cmod_float_t hc;		/* horizontal center */
    cmod_float_t vs;		/* vertical scale factor */
    cmod_float_t vc;		/* vertical center */
    cmod_float_t theta;		/* angle between axes */
    cmod_float_t s_int[5][5];	/* covariance matrix */
    } cmod_cahv_ext_t;

/* CAHVOR core model */
typedef struct cmod_cahvor_t {
    cmod_float_t c[3];		/* model center vector C */
    cmod_float_t a[3];		/* model axis   vector A */
    cmod_float_t h[3];		/* model horiz. vector H */
    cmod_float_t v[3];		/* model vert.  vector V */
    cmod_float_t o[3];		/* model optical axis unit vector O */
    cmod_float_t r[3];		/* model radial-distortion terms  R */
    } cmod_cahvor_t;

/* CAHVOR extended model elements */
typedef struct cmod_cahvor_ext_t {
    cmod_float_t s[18][18];	/* covariance of CAHVOR */
    cmod_float_t hs;		/* horizontal scale factor */
    cmod_float_t hc;		/* horizontal center */
    cmod_float_t vs;		/* vertical scale factor */
    cmod_float_t vc;		/* vertical center */
    cmod_float_t theta;		/* angle between axes */
    cmod_float_t s_int[5][5];	/* covariance matrix */
    } cmod_cahvor_ext_t;

/* CAHVORE core model */
typedef struct cmod_cahvore_t {
    cmod_int_t mtype;		/* type of model */
    cmod_float_t mparm;		/* model parameter */
    cmod_float_t c[3];		/* model center vector C */
    cmod_float_t a[3];		/* model axis   vector A */
    cmod_float_t h[3];		/* model horiz. vector H */
    cmod_float_t v[3];		/* model vert.  vector V */
    cmod_float_t o[3];		/* model optical axis unit vector O */
    cmod_float_t r[3];		/* model radial-distortion terms  R */
    cmod_float_t e[3];		/* model entrance-pupil    terms  E */
    } cmod_cahvore_t;

/* CAHVORE extended model elements */
typedef struct cmod_cahvore_ext_t {
    cmod_float_t s[21][21];	/* covariance of CAHVORE */
    cmod_float_t hs;		/* horizontal scale factor */
    cmod_float_t hc;		/* horizontal center */
    cmod_float_t vs;		/* vertical scale factor */
    cmod_float_t vc;		/* vertical center */
    cmod_float_t theta;		/* angle between axes */
    cmod_float_t s_int[5][5];	/* covariance matrix */
    } cmod_cahvore_ext_t;

/* PSPH core model */
typedef struct cmod_psph_t {
    cmod_float_t c[3];		/* sphere center */
    cmod_float_t ax[3];		/* column rotation axis */
    cmod_float_t ay[3];		/* row    rotation axis */
    cmod_float_t nx[3];		/* column-plane normal vector at column zero */
    cmod_float_t ny[3];		/* row   -plane normal vector at row    zero */
    cmod_float_t sx;		/* column scale factor (rad/pixel) */
    cmod_float_t sy;		/* row    scale factor (rad/pixel) */
    } cmod_psph_t;

/* PSPH extended model elements */
typedef struct cmod_psph_ext_t {
    cmod_float_t theta;		/* angle between axes */
    } cmod_psph_ext_t;

/* Composite core model */
typedef struct cmod_t {
    cmod_int_t xdim;		/* number of image columns */
    cmod_int_t ydim;		/* number of image rows */
    cmod_class_t mclass;	/* model class */
    union {			/* union of core models by class */
	cmod_cahv_t    cahv;
	cmod_cahvor_t  cahvor;
	cmod_cahvore_t cahvore;
	cmod_psph_t    psph;
	} u;
    } cmod_t;

/* Composite extended model */
typedef struct cmod_ext_t {
    cmod_t core;		/* core model */
    union {			/* union of extended model elements by class */
	cmod_cahv_ext_t    cahv;
	cmod_cahvor_ext_t  cahvor;
	cmod_cahvore_ext_t cahvore;
	cmod_psph_ext_t    psph;
	} ext;
    } cmod_ext_t;

#ifdef	__cplusplus
extern "C" {
#endif

#ifdef __STDC__

/******************************************************************************

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
    cmod_float_t upar[3][2]);	/* output partial-derivative matrix of
					uvec3 to pos2, or NULL */

/******************************************************************************

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
    cmod_float_t upar[3][2]);	/* output partial-derivative matrix of
					uvec3 to pos2, or NULL */

/******************************************************************************

    This function projects a 3D point into the image plane using the camera
    model parameters provided. In addition to the 2D projection, it outputs
    the partial derivative matrix of the 2D point with respect to the 3D
    point. */

void cmod_3d_to_2d(
    const cmod_float_t pos3[3],	/* input 3D position */
    const cmod_t *cmod,		/* input core model */
    cmod_float_t pos2[2],	/* output 2D image-plane projection */
    cmod_float_t par[2][3]);	/* output partial-derivative matrix of
					pos2 to pos3, or NULL */

/******************************************************************************

    This function projects a 3D point into the image plane using the camera
    model parameters provided. In addition to the 2D projection, it outputs
    the partial derivative matrix of the 2D point with respect to the 3D
    point. */

void cmod_3d_to_2d_ext(
    const cmod_float_t pos3[3],	/* input 3D position */
    const cmod_ext_t *cmod,	/* input extended model */
    cmod_float_t pos2[2],	/* output 2D image-plane projection */
    cmod_float_t par[2][3]);	/* output partial-derivative matrix of
                                        pos2 to pos3, or NULL */

/******************************************************************************

    This function converts a core model into an extended one. It returns a
    pointer to the output model. */

cmod_ext_t *cmod_ext(
    const cmod_t *cmodcore,	/* input core model */
    cmod_ext_t *cmodext);	/* output extended model */

/******************************************************************************

    This function initializes a model from the parameters of a CAHV model. */

void cmod_init_cahv(
    cmod_int_t xdim,		/* input number of columns */
    cmod_int_t ydim,		/* input number of rows */
    const cmod_float_t c[3],	/* input model center vector C */
    const cmod_float_t a[3],	/* input model axis   vector A */
    const cmod_float_t h[3],	/* input model horiz. vector H */
    const cmod_float_t v[3],	/* input model vert.  vector V */
    cmod_t *cmod);		/* output core model */

/******************************************************************************

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
    cmod_t *cmod);		/* output core model */

/******************************************************************************

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
    cmod_t *cmod);		/* output core model */

/******************************************************************************

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
    cmod_t *cmod);		/* output core model */

/******************************************************************************

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
    cmod_float_t pos2[2]);	/* output 2D projection center */

/******************************************************************************

    This function is the same as cmod_iplane(), but for an extended model */

void cmod_iplane_ext(
    const cmod_ext_t *cmod,	/* input extended model */
    cmod_float_t ndir[3],	/* output normal direction */
    cmod_float_t xdir[3],	/* output X (0th) coordinate direction */
    cmod_float_t ydir[3],	/* output Y (1st) coordinate direction */
    cmod_float_t pos3[3],	/* output 3D projection point */
    cmod_float_t pos2[2]);	/* output 2D projection center */

/******************************************************************************

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
    cmod_t *cmod_f);		/* output final core model */

/******************************************************************************

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
    cmod_ext_t *cmod_f);	/* output final extended model */

/******************************************************************************

    This function returns the position and rotation matrix of orientation
    for the given model. The absolute orientation is based on a reference
    orientation of the camera pointing straight down the Y axis, with Z up.
    */

void cmod_pose(
    const cmod_t *cmod,		/* input core model */
    cmod_float_t p[3],		/* output position vector */
    cmod_float_t r[3][3]);	/* output rotation matrix */

/******************************************************************************

    This function returns the position and rotation matrix of orientation
    for the given model. The absolute orientation is based on a reference
    orientation of the camera pointing straight down the Y axis, with Z up.
    */

void cmod_pose_ext(
    const cmod_ext_t *cmod,	/* input extended model */
    cmod_float_t p[3],		/* output position vector */
    cmod_float_t r[3][3]);	/* output rotation matrix */

/******************************************************************************

    This function reads a camera model from a text file, whose format is
    compatible with what is put out by the program CCALADJ. Note that some
    older model files do not contain the X and Y dimensions of the image;
    in this case negative values will be returned. */

cmod_stat_t cmod_read(
    const char *filename,	/* input filename */
    cmod_t *cmod);		/* output core model */

/******************************************************************************

    This function reads a camera model from a text file, whose format is
    compatible with what is put out by the program CCALADJ. Note that some
    older model files do not contain the X and Y dimensions of the image;
    in this case negative values will be returned. */

cmod_stat_t cmod_read_ext(
    const char *filename,	/* input filename */
    cmod_ext_t *cmod);		/* output extended model */

/******************************************************************************

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
    cmod_bool_t *behind);	/* output if camera is behind reflecting plane*/

/******************************************************************************

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
    cmod_bool_t *behind);	/* output if camera is behind reflecting plane*/

/******************************************************************************

    This function scales a camera model. The scale factors should be
    understood as the same scale factors that would be applied to a 2D
    coordinate in the original model to convert it to a coordinate in
    the resulting model. The image dimensions are left undefined in the
    scaled image. */

void cmod_scale(
    cmod_float_t hscale,	/* input horizontal scale factor */
    cmod_float_t vscale,	/* input vertical   scale factor */
    const cmod_t *cmod1,	/* input initial core model */
    cmod_t *cmod2);		/* output final core model */

/******************************************************************************

    This function scales a camera model. The scale factors should be
    understood as the same scale factors that would be applied to a 2D
    coordinate in the original model to convert it to a coordinate in
    the resulting model. The image dimensions are left undefined in the
    scaled image. */

void cmod_scale_ext(
    cmod_float_t hscale,	/* input horizontal scale factor */
    cmod_float_t vscale,	/* input vertical   scale factor */
    const cmod_ext_t *cmod1,	/* input initial extended model */
    cmod_ext_t *cmod2);		/* output final extended model */

/******************************************************************************

    This function shifts a camera model. The shift values should be
    understood as the coordinates of the old model where the origin
    will fall in the new one. */

void cmod_shift(
    cmod_float_t dx,		/* input horizontal shift */
    cmod_float_t dy,		/* input vertical   shift */
    const cmod_t *cmod1,	/* input initial core model */
    cmod_t *cmod2);		/* output final core model */

/******************************************************************************

    This function shifts a camera model. The shift values should be
    understood as the coordinates of the old model where the origin
    will fall in the new one. */

void cmod_shift_ext(
    cmod_float_t dx,		/* input horizontal shift */
    cmod_float_t dy,		/* input vertical   shift */
    const cmod_ext_t *cmod1,	/* input initial extended model */
    cmod_ext_t *cmod2);		/* output final extended model */

/******************************************************************************

    This function validates the components of a camera model to determine
    whether or not the model makes some kind of sense. Only generic
    sanity-checking tests are performed. It is an option whether or not the
    "none" class is considered acceptable; if so and if the model's class is
    indeed none, then no other checks are performed. It is also an option
    whether or not it is acceptable for the image dimensions to be missing. */

cmod_stat_t cmod_validate(
    const cmod_t *cmod,		/* input core model */
    cmod_bool_t noclass,	/* input if class "none" is ok */
    cmod_bool_t nodims);	/* input if missing dimensions is ok */

/******************************************************************************

    This function writes a camera model to a text file, whose format is
    compatible with what is put out by the program CCALADJ. */

cmod_stat_t cmod_write(
    const char *filename,	/* input filename */
    const char *comment,	/* input one-line comment to record in file */
    const cmod_t *cmod);	/* input core model */

/******************************************************************************

    This function writes a camera model to a text file, whose format is
    compatible with what is put out by the program CCALADJ. */

cmod_stat_t cmod_write_ext(
    const char *filename,	/* input filename */
    const char *comment,	/* input one-line comment to record in file */
    const cmod_ext_t *cmod);	/* input extended model */

/*****************************************************************************/

#else

void        cmod_2d_to_3d();
void        cmod_2d_to_3d_ext();
void        cmod_3d_to_2d();
void        cmod_3d_to_2d_ext();
cmod_ext_t *cmod_ext();
void        cmod_init_cahv();
void        cmod_init_cahvor();
void        cmod_init_cahvore();
void        cmod_iplane();
void        cmod_iplane_ext();
void        cmod_move();
void        cmod_move_ext();
void        cmod_pose();
void        cmod_pose_ext();
cmod_stat_t cmod_read();
cmod_stat_t cmod_read_ext();
void        cmod_reflect();
void        cmod_reflect_ext();
void        cmod_scale();
void        cmod_scale_ext();
void        cmod_shift();
void        cmod_shift_ext();
cmod_stat_t cmod_validate();
cmod_stat_t cmod_write();
cmod_stat_t cmod_write_ext();

#endif

#ifdef	__cplusplus
}
#endif

#endif
