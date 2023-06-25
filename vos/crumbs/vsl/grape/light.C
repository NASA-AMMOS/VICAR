#include "grape/light.h"

inline int get_next( Dataport *fp, const char *szObjectName, char *token)
{
	if(!get_next_token(fp, token)) {
		fprintf(stderr,
				" Whoops - Unexpected EOF in %s file\n",
				szObjectName);
		return(FALSE);			// error
	}
	return TRUE;				// all is well
	
}

int LightObject::parse_in(Dataport *fp)
{
	char	token[4096];
	int	curr_color = -1, nColorBands = 0, num_colors = -1;



								// parse in the common object portion:
								//   <type>
								//   OBJECT_NAME <name>
								//   CENTROID <xyz> <xyz> <xyz>
	if(!Obj::parse_in(fp)) {
		return(FALSE);
	}


								// get the light-specific portion
	do {
		if (get_next( fp, "LightObject", token) == FALSE)
			return(FALSE);

		if(!strcmp(token, "VERSION")) {
			if (get_next( fp, "LightObject", token) == FALSE)
				return(FALSE);

			if(strcmp(token, "1.0")) {
				fprintf(stderr," Whoops - Unexpected version number encountered in parsing polygon object\n");
				fprintf(stderr,"   Token=>%s<  Expecting >1.0<\n", token);
				return(FALSE);
			}
			version = strdup(token);
		} else if(!strcmp(token, INTENSITY_TOKEN)) {

			if (!intensity.parse_in( fp)) {
				fprintf( stderr,
						"Error while parsing in light intensity\n");
				return FALSE;
			}

		} else if(!strcmp(token, BANDS_TOKEN)) {
			if (get_next( fp, "LightObject", token) == FALSE)
				return(FALSE);

			if (num_colors != -1) {
				fprintf( stderr,
			"Error: %s token must come before %s: parsing LightObject\n", 
						BANDS_TOKEN, COLORS_TOKEN);
				return FALSE;
			}
			nColorBands = atoi( token);

		} else if(!strcmp(token, ALPHA_TOKEN)) {

			if (num_colors != -1) {
				fprintf( stderr,
			"Error: %s token must come before %s: parsing LightObject\n", 
						ALPHA_TOKEN, COLORS_TOKEN);
				return FALSE;
			}
			bGetAlpha = TRUE;	// color alpha channel for each color, too

		} else if(!strcmp(token, COLOR_SELECTOR)) {
			if (get_next( fp, "LightObject", token) == FALSE)
				return(FALSE);

								// select active color

			curr_color = atoi( token);
		} else if(!strcmp(token, AMBIENT_TOKEN)) {

			if (pColors == NULL) { // no colors specified yet
				fprintf( stderr,
						"Can't read ambient color: no color list specified yet: while parsing LightObject\n");
				return FALSE;	// error
			}

			set_amb( curr_color);
			
		} else if(!strcmp(token, DIFFUSE_TOKEN)) {

			if (pColors == NULL) { // no colors specified yet
				fprintf( stderr,
						"Can't read diffuse color: no color list specified yet: while parsing LightObject\n");
				return FALSE;	// error
			}

			set_diff( curr_color);
			
		} else if(!strcmp(token, SPECULAR_TOKEN)) {

			if (pColors == NULL) { // no colors specified yet
				fprintf( stderr,
						"Can't read specular color: no color list specified yet: while parsing LightObject\n");
				return FALSE;	// error
			}

			set_spec( curr_color);
			
		} else if(!strcmp(token, COLORS_TOKEN)) {

			if (!nColorBands)	// bands not specified
				nColorBands = 3; // default is RGB

			set_num_bands( nColorBands); // assign number of bands to object

			if (get_next( fp, "LightObject", token) == FALSE)
				return(FALSE);

			num_colors = atoi(token);	// number of colors in object

								// make room for color list
			if ((pColors = new ColorList( num_colors, nColorBands, bGetAlpha))
				== NULL) {
				fprintf( stderr, 
						" Unable to alloc pColors while parsing in LightObject\n");
				return FALSE;	// error
			}

								// get the colorlist
			if (pColors->parse_in( fp) == FALSE)
				return FALSE;	// error
				
		} else if(strcmp(token, LIGHT_OBJ_END)) {
			fprintf(stderr," Whoops - Unexpected token encountered in parsing LightObject object\n");
			fprintf(stderr,"   Token=>%s<  Expecting one of COLORS POINTS EDGES FACES THAT'S_ALL_FOLKS\n", token);
			return(FALSE);
		}
	} while(strcmp(token, LIGHT_OBJ_END));

								// List of colors is _required_
	if (pColors == NULL) {
		fprintf( stderr,
				"Error while parsing LightObject: no color list specified\n");
		return FALSE;
	}

								// at least 1 color from the colorlist must
								// be selected
	else if (curr_color == -1) {
		fprintf( stderr,
				"Error while parsing LightObject: no color specified\n");
		return FALSE;
	}

								// deal with lack of specification of
								// light properties
	else if (diff_ndx == -1 && amb_ndx == -1 && spec_ndx == -1)
		diff_ndx = amb_ndx = spec_ndx = 0; // choose 1st color
	else if (amb_ndx == -1 && spec_ndx == -1)
		amb_ndx = spec_ndx = diff_ndx;
	else if (diff_ndx == -1 && spec_ndx == -1)
		diff_ndx = spec_ndx = amb_ndx;
	else if (diff_ndx == -1 && amb_ndx == -1)
		diff_ndx = amb_ndx = spec_ndx;
	else if (diff_ndx == -1)
		diff_ndx = amb_ndx;
	else if (amb_ndx == -1)
		amb_ndx = diff_ndx;
	else if (spec_ndx == -1)
		spec_ndx = diff_ndx;
	

	return(TRUE);
	
}


int LightObject::parse_out(Dataport *fp, int expand)
{
	if (fp == NULL)
		return FALSE;
	if (expand)
		return FALSE;			// not yet supported

	return FALSE;				// not yet supported
}

int LightObject::using_alpha( void)
{
	if (pColors == NULL)
		return FALSE;
	else
		return pColors->using_alpha();
}
