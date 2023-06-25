/******************************************************************************
*                                                                             *
*                            C M O D _ I N T E R P                            *
*                                                                             *
*                                       Todd Litwin                           *
*                                       Written:  9 Mar 2007                  *
*                                       Updated: 24 May 2016                  *
*                                                                             *
*                                       Copyright (C) 2007, 2008, 2009, 2010, *
*                                                     2012, 2016              *
*                                       California Institute of Technology    *
*                                       All Rights Reserved                   *
*                                                                             *
*******************************************************************************


	This file contains declarations for the camera-model interpolator.
	*/

#ifndef CMOD_INTERP_H
#define CMOD_INTERP_H

#include "cmod.h"

enum {
    CMOD_INTERP_MAX_MODELS = 32	/* maximum number of camera models */
    };

#ifdef	__cplusplus
extern "C" {
#endif

#ifdef __STDC__

/******************************************************************************

    Interpolates a camera model. */

cmod_stat_t cmod_interp(
    cmod_int_t nmods,           /* input number of input camera models */
    const cmod_t cmodi[],       /* input list of camera models to fit */
    const cmod_float_t xi[],    /* input list of corresponding independent
                                        variables */
    cmod_float_t x,             /* input value of interpolation target */
    cmod_t *cmodo);             /* output camera model */

/*****************************************************************************/

#else

cmod_stat_t cmod_interp();

#endif

#ifdef	__cplusplus
}
#endif

#endif
