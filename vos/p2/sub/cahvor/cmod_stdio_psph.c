/******************************************************************************
*                                                                             *
*                          C M O D _ P S P H _ S T D I O                      *
*                                                                             *
*                                       Todd Litwin                           *
*                                       Written: 20 May 2016                  *
*                                       Updated: 12 Aug 2016                  *
*                                                                             *
*                                       Copyright (C) 2016                    *
*                                       California Institute of Technology    *
*                                       All Rights Reserved                   *
*                                                                             *
*******************************************************************************


	This file contains standard I/O functions for the Planospheric
	camera model. */


#include <stdio.h>

#include "cmod_psph.h"
#include "cmod_error.h"

#define SUCCESS 0
#define FAILURE (-1)

#define scanstr_(fp, str) \
    if (cmod_psph_read_scanstr(fp, str) == FAILURE) { \
	CMOD_ERROR_S("cmod_psph_read", \
			"Error looking for \"" str "\" in file ", filename); \
	fclose(fp); \
	return FAILURE; \
	}

#define scan_(fp, num, args) \
    if (fscanf args != num) { \
	CMOD_ERROR_S("cmod_psph_read", \
		"Error reading input data in file ", filename); \
	fclose(fp); \
	return FAILURE; \
	}

cmod_stat_t cmod_psph_read_scanstr(FILE *fp, const char *str);


/******************************************************************************
********************************   CMOD_PSPH_READ   ***************************
*******************************************************************************

    This function reads a PSPH model from a text file. Note that image
    dimensions might be missing from the file, in which case xdim and ydim will
    be less than zero. */

cmod_stat_t cmod_psph_read(
    const char *filename,	/* input filename */
    cmod_int_t *xdim,		/* output number of columns */
    cmod_int_t *ydim,		/* output number of rows */
    cmod_psph_t *psph)		/* output camera model */
{
    FILE *fp;

    /* Open the PSPH file */
    if ((fp = fopen(filename, "r")) == NULL) {
	CMOD_ERROR_S("cmod_psph_read", "Error opening PSPH file: ", filename);
	return FAILURE;
	}

    /* Optionally look for image dimensions */
    if ((xdim != NULL) && (ydim != NULL)) {
	*xdim = -1;
	*ydim = -1;
	if (cmod_psph_read_scanstr(fp, "Dimensions =") == FAILURE)
	    rewind(fp);
	else
	    scan_(fp, 2, (fp, " %d %d ", xdim, ydim));
	}

    /* Read the model parameters */
    scanstr_(fp, "C  =");
    scan_(fp, 3, (fp, "%lf %lf %lf\n", psph->c +0, psph->c +1, psph->c +2));
    scanstr_(fp, "Ax =");
    scan_(fp, 3, (fp, "%lf %lf %lf\n", psph->ax+0, psph->ax+1, psph->ax+2));
    scanstr_(fp, "Ay =");
    scan_(fp, 3, (fp, "%lf %lf %lf\n", psph->ay+0, psph->ay+1, psph->ay+2));
    scanstr_(fp, "Nx =");
    scan_(fp, 3, (fp, "%lf %lf %lf\n", psph->nx+0, psph->nx+1, psph->nx+2));
    scanstr_(fp, "Ny =");
    scan_(fp, 3, (fp, "%lf %lf %lf\n", psph->ny+0, psph->ny+1, psph->ny+2));
    scanstr_(fp, "sx =");
    scan_(fp, 1, (fp, "%lf\n", &psph->sx));
    scanstr_(fp, "sy =");
    scan_(fp, 1, (fp, "%lf\n", &psph->sy));

    /* Close the PSPH file */
    fclose(fp);

    return SUCCESS;
    }


/******************************************************************************
********************************   CMOD_PSPH_READ_SCANSTR   *******************
*******************************************************************************

    This function scans the input for the given string. It return SUCCESS or
    FAILURE. */

cmod_stat_t cmod_psph_read_scanstr(
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
********************************   CMOD_PSPH_WRITE   **************************
*******************************************************************************

    This function writes a PSPH model to a text file. */

cmod_stat_t cmod_psph_write(
    const char *filename,	/* input filename */
    const char *comment,	/* input one-line comment to record in file */
    cmod_int_t xdim,		/* input number of columns, or < 0 */
    cmod_int_t ydim,		/* input number of rows, or < 0 */
    const cmod_psph_t *psph)	/* input camera model */
{
    FILE *fp;

    /* Open the PSPH file */
    if ((fp = fopen(filename, "w")) == NULL) {
	CMOD_ERROR_S("cmod_psph_write", "Error creating PSPH file: ",
		filename);
	return FAILURE;
	}

    /* Write out the comment */
    fprintf(fp, "# %s\n", comment);

    /* Write out model type */
    fprintf(fp, "\n");
    fprintf(fp, "Model = PSPH = planospheric\n");

    /* Write out image dimensions */
    if ((xdim >= 0) && (ydim >= 0)) {
	fprintf(fp, "\n");
	fprintf(fp, "Dimensions = %d %d\n", xdim, ydim);
	}

    /* Write model parameters */
    fprintf(fp, "\n");
    fprintf(fp, "C  = %16.9f %16.9f %16.9f\n",
					psph->c [0], psph->c [1], psph->c [2]);
    fprintf(fp, "Ax = %16.9f %16.9f %16.9f\n",
					psph->ax[0], psph->ax[1], psph->ax[2]);
    fprintf(fp, "Ay = %16.9f %16.9f %16.9f\n",
					psph->ay[0], psph->ay[1], psph->ay[2]);
    fprintf(fp, "Nx = %16.9f %16.9f %16.9f\n",
					psph->nx[0], psph->nx[1], psph->nx[2]);
    fprintf(fp, "Ny = %16.9f %16.9f %16.9f\n",
					psph->ny[0], psph->ny[1], psph->ny[2]);
    fprintf(fp, "sx = %20.9e\n", psph->sx);
    fprintf(fp, "sy = %20.9e\n", psph->sy);

    /* Close file */
    fclose(fp);

    return SUCCESS;
    }
