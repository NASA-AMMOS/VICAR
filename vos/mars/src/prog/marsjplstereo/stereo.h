/******************************************************************************
*                                                                             *
*		JPL Stereo Vision code					      *
*									      *
*		Developed by the Machine Vision and Tracking Sensors Group    *
*		at the Jet Propulsion Laboratory			      *
*								  	      *
*									      *
*	For information contact:					      *
*									      *
*			Larry Matthies	lhm@robotics.jpl.nasa.gov	      *
*                      	Todd Litwin     litwin@robotics.jpl.nasa.gov          *
*			Mark Maimone	mark.maimone@jpl.nasa.gov	      *
*			Yalin Xiong	yx@robotics.jpl.nasa.gov	      *
*								  	      *
*                                       Updated: 16 Nov 1998                  *
*                                                                             *
*                                       Copyright (C) 1993, 1994, 1995, 1996, *
*                                                     1997, 1998              *
*                                       California Institute of Technology    *
*                                       All Rights Reserved                   *
*                                                                             *
******************************************************************************/

/******************************************************************************
*                                                                             *
*                                 S T E R E O                                 *
*                                                                             *
*                                       Todd Litwin                           *
*                                       Written: 16 Nov 1992                  *
*                                       Updated:  8 Aug 2002                  *
*                                                                             *
*                                       Copyright (C) 1992, 1993, 1994, 1995, *
*                                                     1996                    *
*                                       California Institute of Technology    *
*                                       All Rights Reserved                   *
*                                                                             *
*******************************************************************************


	This header file contains definitions used by the stereo
	functions. */

#ifndef __STEREO_HEAD__
#define __STEREO_HEAD__

#ifdef __cplusplus
   extern "C" {
#endif

#include "viscommon.h"

#define FOV_LINEAR	0	/* field of view based on linear parameters */
#define FOV_INTERSECT	1	/* intersection of fields of view */
#define FOV_UNION	2	/* union of fields of view */
#define FOV_DEFAULT	FOV_INTERSECT

#define DISP_SCALE_SHIFT	8
#define I_DISP_SCALE (1<<DISP_SCALE_SHIFT)	/* scaling factor for fixed point disparity */

#define NO_B_DISP  0xFF		/* flag for no disparity in byte image */
#define NO_S_CORR  0x7FFF	/* flag for no correlation in short image */
#define NO_SI_DISP 0x0FFFFFFF	/* flag for no disparity in scaled int image */
#define NO_F_DISP  100000.0	/* flag for no disparity in float image */
#define NO_S2_DISP 0x7FFF   /* flag for no disparity in 16-bit disparity map */

#define NO_RANGE  -100000.0	/* flag for no range point */

typedef struct st_cahv_t_tag {
    double c[3];	/* model center vector C */
    double a[3];	/* model axis   vector A */
    double h[3];	/* model horiz. vector H */
    double v[3];	/* model vert.  vector V */
    /*...
    double s[12][12];	/+ covariance of CAHV +/
    ...*/
    double hs;		/* horizontal scale factor */
    double hc;		/* horizontal center */
    double vs;		/* vertical scale factor */
    double vc;		/* vertical center */
    double theta;	/* angle between axes */
    /*...
    double s_int[5][5];	/+ covariance matrix +/
    ...*/
    } st_cahv_t;
     
typedef struct st_cahvor_t_tag {
    double c[3];	/* model center vector C */
    double a[3];	/* model axis   vector A */
    double h[3];	/* model horiz. vector H */
    double v[3];	/* model vert.  vector V */
    double o[3];	/* model optical axis  O */
    double r[3];	/* model radial-distortion terms R */
    /*...
    double s[18][18];	/+ covariance of CAHVOR +/
    ...*/
    double hs;		/* horizontal scale factor */
    double hc;		/* horizontal center */
    double vs;		/* vertical scale factor */
    double vc;		/* vertical center */
    double theta;	/* angle between axes */
    /*...
    double s_int[5][5];	/+ covariance matrix +/
    ...*/
} st_cahvor_t;

     typedef struct {
       double c[3];      /* model center vector C */
       double a[3];      /* model axis   vector A */
       double h[3];      /* model horiz. vector H */
       double v[3];      /* model vert.  vector V */
       double o[3];      /* model optical axis  O */
       double r[3];      /* model radial-distortion terms R */
       double e[3];      /* model entrance-pupile   terms E */
       /*...
	 double s[21][21]; /+ covariance of CAHVORE +/
	 ...*/
       double hs;                /* horizontal scale factor */
       double hc;                /* horizontal center */
       double vs;                /* vertical scale factor */
       double vc;                /* vertical center */
       double theta;     /* angle between axes */
       /*...
	 double s_int[5][5];       /+ covariance matrix +/
	 ...*/
       double mparm;     /* model parameter (for mtype 3 only) */
       int mtype;                /* model type: 1=perspective, 2=fisheye, 3=general */
       int xdim;         /* model horizontal dimension */
       int ydim;         /* model vertical   dimension */
     } st_cahvore_t;


     /* Rectification Warp Map */

#define WARP_INT	1
#define WARP_DOUBLE	2
#define WARP_I24F44	3
#define WARP_I24F44C	4
#define WARP_FLOAT	5

typedef struct {
    int datatype;
    int xdim;
    int ydim;
    int addr[2];
    } warp_int_t;

typedef struct {
    int datatype;
    int xdim;
    int ydim;
    double addr[2];
    } warp_double_t;

typedef struct {
    int datatype;
    int xdim;
    int ydim;
    float addr[2];
    } warp_float_t;

typedef struct {
    int datatype;
    int frame;
    int xdim;
    int ydim;
    long addr[2];
    } warp_i24f44_t;

typedef struct {
    int datatype;
    int frame;
    int xdim;
    int ydim;
    int x0;
    int y0;
    int nx;
    int ny;
    unsigned short addr[2];
    } warp_i24f44c_t;



#ifdef __cplusplus
  }   /*  extern "C" */
#endif

#endif
