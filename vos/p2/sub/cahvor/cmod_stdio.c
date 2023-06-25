/******************************************************************************
*                                                                             *
*                              C M O D _ S T D I O                            *
*                                                                             *
*                                       Todd Litwin                           *
*                                       Written: 29 Aug 2003                  *
*                                       Updated: 20 May 2016                  *
*                                                                             *
*                                       Copyright (C) 2003, 2005, 2006, 2007, *
*                                                     2008, 2009, 2016        *
*                                       California Institute of Technology    *
*                                       All Rights Reserved                   *
*                                                                             *
*******************************************************************************


	This file contains standard I/O functions for the composite
	camera-model utilities. */


#include <stdio.h>

#include "cmod.h"
#include "cmod_cahv.h"
#include "cmod_cahvor.h"
#include "cmod_cahvore.h"
#include "cmod_psph.h"
#include "cmod_error.h"

#define SUCCESS 0
#define FAILURE (-1)

cmod_stat_t cmod_read_class(const char *filename);
cmod_stat_t cmod_read_scanstr(FILE *fp, const char *str);


/******************************************************************************
********************************   CMOD_READ   ********************************
*******************************************************************************

    This function reads a camera model from a text file, whose format is
    compatible with what is put out by the program CCALADJ. Note that some
    older model files do not contain the X and Y dimensions of the image;
    in this case negative values will be returned. */

cmod_stat_t cmod_read(
    const char *filename,	/* input filename */
    cmod_t *cmod)		/* output core model */
{
    cmod_float_t hs, hc, vs, vc, theta;

    /* Proceed based on underlying model class */
    switch (cmod_read_class(filename)) {

	/* CAHV */
	case CMOD_CLASS_CAHV:
	    if (cmod_cahv_read2(filename,
			&(cmod->xdim),    &(cmod->ydim),
			  cmod->u.cahv.c,   cmod->u.cahv.a,
			  cmod->u.cahv.h,   cmod->u.cahv.v,
			NULL, &hs, &hc, &vs, &vc, &theta, NULL) == FAILURE)
		break;
	    cmod->mclass = CMOD_CLASS_CAHV;
	    return SUCCESS;

	/* CAHVOR */
	case CMOD_CLASS_CAHVOR:
	    if (cmod_cahvor_read2(filename,
			&(cmod->xdim),      &(cmod->ydim),
			  cmod->u.cahvor.c,   cmod->u.cahvor.a,
			  cmod->u.cahvor.h,   cmod->u.cahvor.v,
			  cmod->u.cahvor.o,   cmod->u.cahvor.r,
			NULL, &hs, &hc, &vs, &vc, &theta, NULL) == FAILURE)
		break;
	    cmod->mclass = CMOD_CLASS_CAHVOR;
	    return SUCCESS;

	/* CAHVORE */
	case CMOD_CLASS_CAHVORE:
	    if (cmod_cahvore_read(filename,
			&(cmod->xdim),            &(cmod->ydim),
			&(cmod->u.cahvore.mtype), &(cmod->u.cahvore.mparm),
			  cmod->u.cahvore.c,        cmod->u.cahvore.a,
			  cmod->u.cahvore.h,        cmod->u.cahvore.v,
			  cmod->u.cahvore.o,        cmod->u.cahvore.r,
			  cmod->u.cahvore.e,
			NULL, &hs, &hc, &vs, &vc, &theta, NULL) == FAILURE)
		break;
	    cmod->mclass = CMOD_CLASS_CAHVORE;
	    return SUCCESS;

	/* PSPH */
	case CMOD_CLASS_PSPH:
	    if (cmod_psph_read(filename,
			&(cmod->xdim), &(cmod->ydim), &cmod->u.psph) == FAILURE)
		break;
	    cmod->mclass = CMOD_CLASS_PSPH;
	    return SUCCESS;

	/* Error */
	case FAILURE:
	    break;

	/* Unknown */
	default:
	    CMOD_ERROR_S("cmod_read", "could not determine model class: ",
		filename);
	    break;
	}

    /* Not successful */
    cmod->mclass = CMOD_CLASS_NONE;

    return FAILURE;
    }


/******************************************************************************
********************************   CMOD_READ_CLASS   **************************
*******************************************************************************

    This function examines the named file and determines what type of
    camera-model file it is. Note that structuring the code with a separate
    function like this means that each input file will be opened and closed
    twice, once to determine its type, and once more to read it in. */

cmod_stat_t cmod_read_class(
    const char *filename)	/* input filename */
{
    FILE *fp;

    /* Open the model file */
    if ((fp = fopen(filename, "r")) == NULL) {
	CMOD_ERROR_S("cmod_read_class", "Error opening camera-model file: ",
		filename);
	return FAILURE;
	}

    /* Look for parameters that must be present for PSPH models */
    if ((cmod_read_scanstr(fp, "C  =") == SUCCESS) &&
	(cmod_read_scanstr(fp, "Ax =") == SUCCESS) &&
	(cmod_read_scanstr(fp, "Ay =") == SUCCESS) &&
	(cmod_read_scanstr(fp, "Nx =") == SUCCESS) &&
	(cmod_read_scanstr(fp, "Ny =") == SUCCESS) &&
	(cmod_read_scanstr(fp, "sx =") == SUCCESS) &&
	(cmod_read_scanstr(fp, "sy =") == SUCCESS))
	return CMOD_CLASS_PSPH;
    else
	rewind(fp);

    /* Look for parameters that must be present for CAHV models and beyond */
    if ((cmod_read_scanstr(fp, "C =") == FAILURE) ||
	(cmod_read_scanstr(fp, "A =") == FAILURE) ||
	(cmod_read_scanstr(fp, "H =") == FAILURE) ||
	(cmod_read_scanstr(fp, "V =") == FAILURE)) {
	fclose(fp);
	return CMOD_CLASS_NONE;
	}

    /* Look for parameters that must be present for CAHVOR models and beyond */
    if (cmod_read_scanstr(fp, "O =") == FAILURE) {
	fclose(fp);
	return CMOD_CLASS_CAHV;
	}
    if (cmod_read_scanstr(fp, "R =") == FAILURE) {
	fclose(fp);
	return CMOD_CLASS_NONE;
	}

    /* Look for final parameter that must be present for CAHVORE models */
    if (cmod_read_scanstr(fp, "E =") == FAILURE) {
	fclose(fp);
	return CMOD_CLASS_CAHVOR;
	}

    /* No more types defined at present */
    fclose(fp);
    return CMOD_CLASS_CAHVORE;
    }


/******************************************************************************
********************************   CMOD_READ_EXT   ****************************
*******************************************************************************

    This function reads a camera model from a text file, whose format is
    compatible with what is put out by the program CCALADJ. Note that some
    older model files do not contain the X and Y dimensions of the image;
    in this case negative values will be returned. */

cmod_stat_t cmod_read_ext(
    const char *filename,	/* input filename */
    cmod_ext_t *cmod)		/* output extended model */
{
    /* Proceed based on underlying model class */
    switch (cmod_read_class(filename)) {

	/* CAHV */
	case CMOD_CLASS_CAHV:
	    if (cmod_cahv_read2(filename,
		   &(cmod->core.xdim),    &(cmod->core.ydim),
		     cmod->core.u.cahv.c,   cmod->core.u.cahv.a,
		     cmod->core.u.cahv.h,   cmod->core.u.cahv.v,
		     cmod->ext.cahv.s,
		   &(cmod->ext.cahv.hs),      &(cmod->ext.cahv.hc),
		   &(cmod->ext.cahv.vs),      &(cmod->ext.cahv.vc),
		   &(cmod->ext.cahv.theta),
		     cmod->ext.cahv.s_int) == FAILURE)
		break;
	    cmod->core.mclass = CMOD_CLASS_CAHV;
	    return SUCCESS;

	/* CAHVOR */
	case CMOD_CLASS_CAHVOR:
	    if (cmod_cahvor_read2(filename,
		   &(cmod->core.xdim),      &(cmod->core.ydim),
		     cmod->core.u.cahvor.c,   cmod->core.u.cahvor.a,
		     cmod->core.u.cahvor.h,   cmod->core.u.cahvor.v,
		     cmod->core.u.cahvor.o,   cmod->core.u.cahvor.r,
		     cmod->ext.cahvor.s,
		   &(cmod->ext.cahvor.hs),      &(cmod->ext.cahvor.hc),
		   &(cmod->ext.cahvor.vs),      &(cmod->ext.cahvor.vc),
		   &(cmod->ext.cahvor.theta),
		     cmod->ext.cahvor.s_int) == FAILURE)
		break;
	    cmod->core.mclass = CMOD_CLASS_CAHVOR;
	    return SUCCESS;

	/* CAHVORE */
	case CMOD_CLASS_CAHVORE:
	    if (cmod_cahvore_read(filename,
		   &(cmod->core.xdim),            &(cmod->core.ydim),
		   &(cmod->core.u.cahvore.mtype), &(cmod->core.u.cahvore.mparm),
		     cmod->core.u.cahvore.c,        cmod->core.u.cahvore.a,
		     cmod->core.u.cahvore.h,        cmod->core.u.cahvore.v,
		     cmod->core.u.cahvore.o,        cmod->core.u.cahvore.r,
		     cmod->core.u.cahvore.e,
		     cmod->ext.cahvore.s,
		   &(cmod->ext.cahvore.hs),      &(cmod->ext.cahvore.hc),
		   &(cmod->ext.cahvore.vs),      &(cmod->ext.cahvore.vc),
		   &(cmod->ext.cahvore.theta),
		     cmod->ext.cahvore.s_int) == FAILURE)
		break;
	    cmod->core.mclass = CMOD_CLASS_CAHVORE;
	    return SUCCESS;

	/* PSPH */
	case CMOD_CLASS_PSPH:
	    if (cmod_psph_read(filename,
			&(cmod->core.xdim),	&(cmod->core.ydim),
			&(cmod->core.u.psph)) == FAILURE)
		break;
	    cmod->core.mclass = CMOD_CLASS_PSPH;
	    return SUCCESS;

	/* Error */
	case FAILURE:
	    break;

	/* Unknown */
	default:
	    CMOD_ERROR_S("cmod_read_ext", "could not determine model class: ",
		filename);
	    break;
	}

    /* Not successful */
    cmod->core.mclass = CMOD_CLASS_NONE;

    return FAILURE;
    }


/******************************************************************************
********************************   CMOD_READ_SCANSTR   ************************
*******************************************************************************

    This function scans the input for the given string. It return SUCCESS or
    FAILURE. */

cmod_stat_t cmod_read_scanstr(
    FILE *fp,		/* input file pointer */
    const char *str)	/* input target string */
{
    const char *s;
    cmod_int_t c;

    s = str;
    for (;;) {
	c = getc(fp);
	if (c == EOF)
	    return FAILURE;
	if (c == *s) {
	    s++;
	    if (*s == '\0')
		return SUCCESS;
	    }
	else
	    s = str;
	}
    }


/******************************************************************************
********************************   CMOD_WRITE   *******************************
*******************************************************************************

    This function writes a camera model to a text file, whose format is
    compatible with what is put out by the program CCALADJ. */

cmod_stat_t cmod_write(
    const char *filename,	/* input filename */
    const char *comment,	/* input one-line comment to record in file */
    const cmod_t *cmod)		/* input core model */
{
    cmod_float_t hs, hc, vs, vc, theta;

    /* Proceed based on the underlying model class */
    switch (cmod->mclass) {

	/* CAHV */
	case CMOD_CLASS_CAHV:
	    cmod_cahv_internal(
		cmod->u.cahv.c, cmod->u.cahv.a, cmod->u.cahv.h,
		cmod->u.cahv.v, NULL,
		&hs, &hc, &vs, &vc, &theta, NULL);
	    return cmod_cahv_write2(filename, comment,
			cmod->xdim,     cmod->ydim,
			cmod->u.cahv.c, cmod->u.cahv.a,
			cmod->u.cahv.h, cmod->u.cahv.v,
			NULL,
			hs, hc, vs, vc, theta, NULL);

	/* CAHVOR */
	case CMOD_CLASS_CAHVOR:
	    cmod_cahv_internal(
		cmod->u.cahvor.c, cmod->u.cahvor.a, cmod->u.cahvor.h,
		cmod->u.cahvor.v, NULL,
		&hs, &hc, &vs, &vc, &theta, NULL);
	    return cmod_cahvor_write2(filename, comment,
			cmod->xdim,       cmod->ydim,
			cmod->u.cahvor.c, cmod->u.cahvor.a,
			cmod->u.cahvor.h, cmod->u.cahvor.v,
			cmod->u.cahvor.o, cmod->u.cahvor.r,
			NULL,
			hs, hc, vs, vc, theta, NULL);

	/* CAHVORE */
	case CMOD_CLASS_CAHVORE:
	    cmod_cahv_internal(
		cmod->u.cahvore.c, cmod->u.cahvore.a, cmod->u.cahvore.h,
		cmod->u.cahvore.v, NULL,
		&hs, &hc, &vs, &vc, &theta, NULL);
	    return cmod_cahvore_write(filename, comment,
			cmod->xdim,            cmod->ydim,
			cmod->u.cahvore.mtype, cmod->u.cahvore.mparm,
			cmod->u.cahvore.c,     cmod->u.cahvore.a,
			cmod->u.cahvore.h,     cmod->u.cahvore.v,
			cmod->u.cahvore.o,     cmod->u.cahvore.r,
			cmod->u.cahvore.e,     NULL,
			hs, hc, vs, vc, theta, NULL);

	/* PSPH */
	case CMOD_CLASS_PSPH:
	    return cmod_psph_write(filename, comment,
			cmod->xdim, cmod->ydim, &cmod->u.psph);

	/* Unknown */
	default:
	    CMOD_ERROR_I("cmod_write", "bad camera-model type", cmod->mclass);
	    break;
	}

    return FAILURE;
    }


/******************************************************************************
********************************   CMOD_WRITE_EXT   ***************************
*******************************************************************************

    This function writes a camera model to a text file, whose format is
    compatible with what is put out by the program CCALADJ. */

cmod_stat_t cmod_write_ext(
    const char *filename,	/* input filename */
    const char *comment,	/* input one-line comment to record in file */
    const cmod_ext_t *cmod)	/* input extended model */
{
    /* Proceed based on the underlying model class */
    switch (cmod->core.mclass) {

	/* CAHV */
	case CMOD_CLASS_CAHV:
	    return cmod_cahv_write2(filename, comment,
			cmod->core.xdim,     cmod->core.ydim,
			cmod->core.u.cahv.c, cmod->core.u.cahv.a,
			cmod->core.u.cahv.h, cmod->core.u.cahv.v,
			(cmod_float_t (*)[12])cmod->ext.cahv.s,
			cmod->ext.cahv.hs,   cmod->ext.cahv.hc,
			cmod->ext.cahv.vs,   cmod->ext.cahv.vc,
			cmod->ext.cahv.theta,
			(cmod_float_t (*)[5])cmod->ext.cahv.s_int);

	/* CAHVOR */
	case CMOD_CLASS_CAHVOR:
	    return cmod_cahvor_write2(filename, comment,
			cmod->core.xdim,       cmod->core.ydim,
			cmod->core.u.cahvor.c, cmod->core.u.cahvor.a,
			cmod->core.u.cahvor.h, cmod->core.u.cahvor.v,
			cmod->core.u.cahvor.o, cmod->core.u.cahvor.r,
			(cmod_float_t (*)[18])cmod->ext.cahvor.s,
			cmod->ext.cahvor.hs,   cmod->ext.cahvor.hc,
			cmod->ext.cahvor.vs,   cmod->ext.cahvor.vc,
			cmod->ext.cahvor.theta,
			(cmod_float_t (*)[5])cmod->ext.cahvor.s_int);

	/* CAHVORE */
	case CMOD_CLASS_CAHVORE:
	    return cmod_cahvore_write(filename, comment,
			cmod->core.xdim,            cmod->core.ydim,
			cmod->core.u.cahvore.mtype, cmod->core.u.cahvore.mparm,
			cmod->core.u.cahvore.c,     cmod->core.u.cahvore.a,
			cmod->core.u.cahvore.h,     cmod->core.u.cahvore.v,
			cmod->core.u.cahvore.o,     cmod->core.u.cahvore.r,
			cmod->core.u.cahvore.e,
			(cmod_float_t (*)[21])cmod->ext.cahvore.s,
			cmod->ext.cahvore.hs,       cmod->ext.cahvore.hc,
			cmod->ext.cahvore.vs,       cmod->ext.cahvore.vc,
			cmod->ext.cahvore.theta,
			(cmod_float_t (*)[5])cmod->ext.cahvore.s_int);

	/* PSPH */
	case CMOD_CLASS_PSPH:
	    return cmod_psph_write(filename, comment,
			cmod->core.xdim, cmod->core.ydim, &cmod->core.u.psph);

	/* Unknown */
	default:
	    CMOD_ERROR_I("cmod_write_ext", "bad camera-model type",
		cmod->core.mclass);
	    break;
	}

    return FAILURE;
    }
