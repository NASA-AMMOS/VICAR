/******************************************************************************
*                                                                             *
*                          C M O D _ C A H V _ S T D I O                      *
*                                                                             *
*                                       Todd Litwin                           *
*                                       Written:  3 Aug 1993                  *
*                                       Updated: 24 May 2016                  *
*                                                                             *
*                                       Copyright (C) 1993, 1994, 1995, 1996, *
*                                                     1997, 1998, 1999, 2000, *
*                                                     2002, 2003, 2005, 2007, *
*                                                     2009, 2013, 2016        *
*                                       California Institute of Technology    *
*                                       All Rights Reserved                   *
*                                                                             *
*******************************************************************************


	This file contains standard I/O functions for the Yakimovsky &
	Cunningham camera model, known locally as CAHV. */


#include <stdio.h>

#include "cmod_cahv.h"
#include "cmod_error.h"

#define SUCCESS 0
#define FAILURE (-1)

#define	PI (3.14159265358979323846)

#define scanstr_(fp, str) \
    if (cmod_cahv_read2_scanstr(fp, str) == FAILURE) { \
	CMOD_ERROR_S("cmod_cahv_read2", \
			"Error looking for \"" str "\" in file ", filename); \
	fclose(fp); \
	return FAILURE; \
	}

#define scan_(fp, num, args) \
    if (fscanf args != num) { \
	CMOD_ERROR_S("cmod_cahv_read2", \
		"Error reading input data in file ", filename); \
	fclose(fp); \
	return FAILURE; \
	}

cmod_stat_t cmod_cahv_read2_scanstr(FILE *fp, const char *str);


/******************************************************************************
********************************   CMOD_CAHV_READ   ***************************
*******************************************************************************

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
    cmod_float_t s_int[5][5])	/* output covariance matrix, or NULL */
{
    return cmod_cahv_read2(filename, (cmod_int_t *)NULL, (cmod_int_t *)NULL,
				c, a, h, v, s, hs, hc, vs, vc, theta, s_int);
    }


/******************************************************************************
********************************   CMOD_CAHV_READ2   **************************
*******************************************************************************

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
    cmod_float_t s_int[5][5])	/* output covariance matrix, or NULL */
{
    cmod_int_t i;
    cmod_int_t j;
    FILE *fp;

    /* Open the CAHV file */
    if ((fp = fopen(filename, "r")) == NULL) {
	CMOD_ERROR_S("cmod_cahv_read2", "Error opening CAHV file: ", filename);
	return FAILURE;
	}

    /* Optionally look for image dimensions */
    if ((xdim != NULL) && (ydim != NULL)) {
	*xdim = -1;
	*ydim = -1;
	if (cmod_cahv_read2_scanstr(fp, "Dimensions =") == FAILURE)
	    rewind(fp);
	else
	    scan_(fp, 2, (fp, " %d %d ", xdim, ydim));
	}

    /* Read C, A, H, V vectors */
    scanstr_(fp, "C =");
    scan_(fp, 3, (fp, "%lf %lf %lf\n", &c[0], &c[1], &c[2]));
    scanstr_(fp, "A =");
    scan_(fp, 3, (fp, "%lf %lf %lf\n", &a[0], &a[1], &a[2]));
    scanstr_(fp, "H =");
    scan_(fp, 3, (fp, "%lf %lf %lf\n", &h[0], &h[1], &h[2]));
    scanstr_(fp, "V =");
    scan_(fp, 3, (fp, "%lf %lf %lf\n", &v[0], &v[1], &v[2]));

    /* Read covariance matrix for C, A, H, V */
    /*scanstr_(fp, "S =");*/
    if (s != NULL) {
	if (cmod_cahv_read2_scanstr(fp, "S =") == FAILURE) {
	    for (i=0; i<12; i++)
		for (j=0; j<12; j++)
		    s[i][j] = 0;
	    }
	else {
	    for (i=0; i<12; i++) {
		scan_(fp, 12, (fp,
		    "%lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf %lf",
		    &s[i][ 0], &s[i][ 1], &s[i][ 2], &s[i][ 3],
		    &s[i][ 4], &s[i][ 5], &s[i][ 6], &s[i][ 7],
		    &s[i][ 8], &s[i][ 9], &s[i][10], &s[i][11]
		    ));
		scanstr_(fp, "\n"); /* skip extra elements if reading CAHVOR */
		}
	    }
	}

    /* Read internal model parameters */
    /*scanstr_(fp, "Hs    =");*/
    if (cmod_cahv_read2_scanstr(fp, "Hs    =") == FAILURE)
	cmod_cahv_internal(c, a, h, v, NULL, hs, hc, vs, vc, theta, NULL);
    else {
	scan_(fp, 1, (fp, "%lf \n", hs));
	scanstr_(fp, "Hc    =");
	scan_(fp, 1, (fp, "%lf \n", hc));
	scanstr_(fp, "Vs    =");
	scan_(fp, 1, (fp, "%lf \n", vs));
	scanstr_(fp, "Vc    =");
	scan_(fp, 1, (fp, "%lf \n", vc));
	scanstr_(fp, "Theta =");
	scan_(fp, 1, (fp, "%lf \n", theta));
	}

    /* Read covariance matrix for internal model parameters */
    /*scanstr_(fp, "S internal =");*/
    if (s_int != NULL) {
	if (cmod_cahv_read2_scanstr(fp, "S internal =") == FAILURE) {
	    for (i=0; i<5; i++)
		for (j=0; j<5; j++)
		    s_int[i][j] = 0;
	    }
	else {
	    for (i=0; i<5; i++) {
		scan_(fp, 5, (fp, "%lf %lf %lf %lf %lf\n",
		    &s_int[i][ 0], &s_int[i][ 1],
		    &s_int[i][ 2], &s_int[i][ 3],
		    &s_int[i][ 4]
		    ));
		}
	    }
	}

    /* Close the CAHV file */
    fclose(fp);

    return SUCCESS;
    }


/******************************************************************************
********************************   CMOD_CAHV_READ2_SCANSTR   ******************
*******************************************************************************

    This function scans the input for the given string. It return SUCCESS or
    FAILURE. */

cmod_stat_t cmod_cahv_read2_scanstr(
    FILE *fp,			/* input file pointer */
    const char *str)		/* input target string */
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
********************************   CMOD_CAHV_WRITE   **************************
*******************************************************************************

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
    cmod_float_t s_int[5][5])	/* input covariance matrix, or NULL */
{
    return cmod_cahv_write2(filename, comment, -1, -1,
			c, a, h, v, s, hs, hc, vs, vc, theta, s_int);
    }


/******************************************************************************
********************************   CMOD_CAHV_WRITE2   *************************
*******************************************************************************

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
    cmod_float_t s_int[5][5])	/* input covariance matrix, or NULL */
{
    cmod_int_t i, j;
    FILE *fp;

    /* Open the CAHV file */
    if ((fp = fopen(filename, "w")) == NULL) {
	CMOD_ERROR_S("cmod_cahv_write2", "Error creating CAHV file: ",
		filename);
	return FAILURE;
	}

    /* Write out the comment */
    fprintf(fp, "# %s\n", comment);

    /* Write out model type */
    fprintf(fp, "\n");
    fprintf(fp, "Model = CAHV = perspective, linear\n");

    /* Write out image dimensions */
    if ((xdim >= 0) && (ydim >= 0)) {
	fprintf(fp, "\n");
	fprintf(fp, "Dimensions = %d %d\n", xdim, ydim);
	}

    /* Write C, A, H, V vectors */
    fprintf(fp, "\n");
    fprintf(fp, "C = %16.9f %16.9f %16.9f\n", c[0], c[1], c[2]);
    fprintf(fp, "A = %16.9f %16.9f %16.9f\n", a[0], a[1], a[2]);
    fprintf(fp, "H = %16.9f %16.9f %16.9f\n", h[0], h[1], h[2]);
    fprintf(fp, "V = %16.9f %16.9f %16.9f\n", v[0], v[1], v[2]);

    /* Write covariance matrix for C, A, H, V */
    fprintf(fp, "\n");
    fprintf(fp, "S =\n");
    for (i=0; i<12; i++) {
	for (j=0; j<12; j++)
	    fprintf(fp, " %16.9e", (s == NULL) ? 0.0 : s[i][j]);
	fprintf(fp, "\n");
	}

    /* Write internal model parameters */
    fprintf(fp, "\n");
    fprintf(fp, "Hs    = %16.9f\n", hs);
    fprintf(fp, "Hc    = %16.9f\n", hc);
    fprintf(fp, "Vs    = %16.9f\n", vs);
    fprintf(fp, "Vc    = %16.9f\n", vc);
    fprintf(fp, "Theta = %16.9f (%f deg)\n", theta, (theta * 180 / PI));

    /* Write covariance matrix for internal model parameters */
    fprintf(fp, "\n");
    fprintf(fp, "S internal =\n");
    for (i=0; i<5; i++) {
	for (j=0; j<5; j++)
	    fprintf(fp, " %16.9e", (s_int == NULL) ? 0.0 : s_int[i][j]);
	fprintf(fp, "\n");
	}
    fprintf(fp, "\n");

    /* Close file */
    fclose(fp);

    return SUCCESS;
    }
