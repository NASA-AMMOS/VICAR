// color.C 1.7 02/07/10 15:02:51
/** \file
 ** Color specification handling
 **/
#include <stdlib.h>
#include "grape/color.h"

#define END_COLOR_LIST "END_COLORSPEC"
#define BAND_SPEC "BANDS"

/// convenience function to get right type of ColorSpec object when parsing in
ColorSpec *get_new_colorspec(Dataport *fp)
{
	int	num_bands;
	char	token[4096];

	if(!get_next_token(fp, token)) {
		fprintf(stderr, " Whoops - Unexpected EOF while parsing ColorSpec object\n");
		return(NULL);			// error
	} else if(!strcmp(token, "COLOR_SPEC_V1")) {
		// skip it
		if(!get_next_token(fp, token)) {
			fprintf(stderr, " Whoops - Unexpected EOF while parsing ColorSpec object\n");
			return(NULL);			// error
		}
	}
	// better be NUM_BANDS
	if(strcmp(token, "NUM_BANDS")) {
		fprintf(stderr, " Whoops - Expected NUM_BANDS as next token in ColorSpec object being parsed.\n");
		return(NULL);
	}
	if(!get_next_token(fp, token)) {
		fprintf(stderr, " Whoops - Unexpected EOF while parsing ColorSpec object\n");
		return(NULL);
	}
	num_bands = atoi(token);
	if(num_bands <= 0) {
		fprintf(stderr, " Whoops - Number of bands detected in ColorSpec object <= 0 (token = %s)\n", token);
		return(NULL);
	}
	if(num_bands == 3) {
		return(new RgbColorSpec);
	} else {
		return(new ColorSpec(num_bands));
	}
}
	

int ColorSpec::parse_in(Dataport *fp)
{
	//double dtemp;				// read-in double
//	int ii = 0;					// band counter
	char    token[4096];

	if (nBands < 1)				// valid number of bands?
		return FALSE;			// nope

								// account for alpha channel, if any

								// Assumptions:
								//   - we know that we are parsing a color
								//   - the number of bands has
								// already been established

	do {
		if(!get_next_token(fp, token)) {
			fprintf(stderr,
					" Whoops - Unexpected EOF while parsing ColorSpec object\n");
			return(FALSE);			// error
		} else if(!strcmp(token, "COLOR")) {
ColorVals.parse_in(fp);
/*
			do {

				if(!get_next_token(fp, token)) {
					fprintf(stderr,
							" Whoops - Unexpected EOF while parsing ColorSpec object\n");
					return(FALSE);			// error
				} else if(!strcmp(token, "DATA")) {
					// parse in enveloped color
					if(!ColorVals[ii].parse_in(fp)) {
						fprintf(stderr," Whoops - Problem parsing a band value in ColorSpec\n");
						return(FALSE);
					}
				} else {
					// assume the value is numeric
					dtemp = atof( token);
					ColorVals.set_value( ii, dtemp);
				}
		
				ii++;
		
			} while (ii < nBands);
*/
		} else if(!strcmp(token, "ALPHA")) {
								// see fi we need to handle alpha values
			if (!alpha.parse_in( fp)) {
				fprintf(stderr,
					" Whoops - Problem while parsing alpha for ColorSpec object\n");
				return(FALSE);
			}
			bGetAlpha = TRUE;
		}
	} while (strcmp(token, "END_COLORSPEC"));

	return TRUE;

}

int ColorSpec::parse_out( Dataport *fp, int expand)
{
	char	token[4096];
//	int	ii;

	if (fp == NULL)
		return FALSE;

	put_token(fp, "\nCOLOR_SPEC_V1");

//	put_token(fp, "\nVERSION");
//	put_token(fp, "1.0");

	put_token(fp, "\nNUM_BANDS");
	sprintf(token, " %d", nBands);
	put_token(fp, token);

	put_token(fp, "\nCOLOR ");
//	for(ii=0; ii<nBands; ii++) ColorVals[ii].parse_out(fp, expand);
	ColorVals.parse_out(fp, expand);

//	fprintf(stderr, "In ColorSpec::parse_out, bGetAlpha = %d\n", bGetAlpha);
	if(bGetAlpha) {
		put_token(fp, "\nALPHA ");
		alpha.parse_out(fp, expand);
	}
	
	put_token(fp, "\nEND_COLORSPEC");

	return(TRUE);
}

void ColorSpec::dump( void)
{
	int i;

	if (get_num_bands() > 0) {
		for (i = 0; i < get_num_bands(); i++) {
			fprintf( stderr, "%f ", get_color( i));
		}
		fprintf( stderr, "\n");
	}
}

void ColorSpec::set_color( double *pColorVector) {
	int i;
	if (pColorVector != NULL) {
		for (i = 0; i < nBands; i++, pColorVector++)
			ColorVals.set_value( i, *pColorVector);
		if (using_alpha() == TRUE)
			set_alpha( *pColorVector);
	}
}

void ColorSpec::get_color( double *pColorVector, double dIntensity) {
	int i;
	if (pColorVector != NULL) {
		for (i = 0; i < nBands; i++, pColorVector++)
			*pColorVector = ColorVals.get_value( i) * dIntensity;
		if (using_alpha() == TRUE)
			*pColorVector = get_alpha();
	}
	
}

void ColorSpec::set_color( float *pColorVector) {
	int i;
	if (pColorVector != NULL) {
		for (i = 0; i < nBands; i++, pColorVector++)
			ColorVals.set_value( i, (double)(*pColorVector));
		if (using_alpha() == TRUE)
			set_alpha( (double)(*pColorVector));
	}
}

void ColorSpec::get_color( float *pColorVector, double dIntensity) {
	int i;
	if (pColorVector != NULL) {
		for (i = 0; i < nBands; i++, pColorVector++)
			*pColorVector = (float)(ColorVals.get_value( i) *
									dIntensity);
		if (using_alpha() == TRUE)
			*pColorVector = (float)get_alpha();
	}
	
}

void ColorList::dump( void)
{
	int ii;
	ColorSpec *pCS;

	fprintf( stderr, "ColorList: colors=%d bands=%d\n",
			nColors, nBands);
	for (ii = 0; ii < get_num_colors(); ii++) {
		pCS = get_color_by_ndx( ii);
		fprintf( stderr, "%05d : ", ii);
		if (pCS != NULL)
			pCS->dump();
		else
			fprintf( stderr, "nil\n");
	}
	fprintf( stderr, "ColorList: end\n\n");
}

ColorSpec *ColorList::get_color_by_ndx( int ndx) {

								/* NOTE: ndx is zero-based! */

	if (ppColorList == NULL)
		return NULL;

	if (ndx < nColors && ndx >= 0)
		return *(ppColorList + ndx);
	else
		return NULL;
}


int ColorList::set_color_by_ndx( int ndx, ColorSpec *pNewColorSpec) {
	ColorSpec **ppC;

	if (pNewColorSpec == NULL)
		return FALSE;

								/* watch for improper number of bands */
	if (nBands != 0)		/* all entries have same # bands if nonzero */
		if (pNewColorSpec->get_num_bands() != nBands)
			return FALSE;

	if (ppColorList != NULL && ndx < nColors && ndx >= 0) {
		ppC = ppColorList + ndx; /* ndx-th element in list */
		if (*ppC != NULL)	/* in use? */
			delete *ppC;	/* throw away */
		*ppC = pNewColorSpec; /* assign new color */

		return TRUE;		/* success */
	}
	else
		return FALSE;		/* failure */
		
}


int ColorList::parse_in(Dataport *fp)
{
	int ii = 0;					// band counter
	ColorSpec *pC;

#if 0
	cleanup();					// memory
	init();						// initialize
#endif

	if (get_num_colors() < 1)	// valid number of colors
		return FALSE;			// nope


	if (!get_num_bands()) {	// number of bands not specified: assume 3
		fprintf(stderr,
				" Whoops - unable to parse ColorSpec object: no bands specified\n");
		return(FALSE);
	}


								// Assumptions:
								//   - we know that we are parsing a color list
								//   - the number of colors has been established
								//   - the number of bands has been established
								// i.e. someone else before must parse:
								//   COLORS n [COLOR_BANDS m]
								//
								//   if COLOR_BANDS not specified, must be set
								//   to 3 for RGB

	do {


								// get pointer to ColorSPec object

		if ((pC = get_color_by_ndx( ii)) == NULL) { // if not yet alocated
			pC = new ColorSpec( get_num_bands()); // allocate it

			set_color_by_ndx( ii, pC); // set the ii-th color object (assigning
									   // a ptr!)
		}

								// parse in the color

		if (pC->parse_in( fp) == FALSE) {
			fprintf(stderr,
					" Whoops - unable to parse ColorSpec object %d while parsing ColorList\n", ii);
			return(FALSE);
		}

		ii++;					// next band
		
	} while (ii < get_num_colors());

	return TRUE;

}

int ColorList::parse_out(Dataport *fp, int expand)
{

	if (fp == NULL)
		return FALSE;
	if (expand)
		return FALSE;			// not yet supported

	return FALSE;				// not yet supported
}

int ColorList::set_num_bands( int n)
{
	
	if (!nBands && n > 0) {		// not yet set AND valid numbet of bands
		nBands = n;
		return TRUE;
	}
	else
		return FALSE;

		
}

int ColorList::using_alpha( void)
{
	ColorSpec *p;

	if ((p = get_color_by_ndx( 0)) != NULL)
		return p->using_alpha();
	else
		return FALSE;
}
