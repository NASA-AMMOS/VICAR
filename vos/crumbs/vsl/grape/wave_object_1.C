// wave_object_1.C 1.12 01/10/11 15:05:13
#include "dataport.h"
#include "grape/object_types.h"
#include "grape/wave_object_1.h"

void WaveObject1::init(void)
{
	if(version)free(version);
	if(file_type)free(file_type);
	if(group_name)free(group_name);

	// set centroid
	x.set_value(0.0);
	y.set_value(0.0);
	z.set_value(0.0);
	xrot.set_value(0.0);
	yrot.set_value(0.0);
	zrot.set_value(0.0);
	xscale.set_value(1.0);
	yscale.set_value(1.0);
	zscale.set_value(1.0);
}

void WaveObject1::skip_line(Dataport *fp)
{
	while(fp->pgetc() != '\n');
}

void WaveObject1::dump_texture_map( void)
{
	int i, j, n, k;

	for (i = 0; i < text_verts.get_num(); i++) {
		n = text_verts.get_size( i);
		fprintf( stderr, "%4d :\n", i);
		for (j = 0; j < n; j++) {
			k = text_verts.get_value( i, j);
			fprintf( stderr, 
					"\t%7.2f %7.2f %7.2f\n",
					(double)x_text[k],
					(double)y_text[k],
					(double)z_text[k]);
		}
	}
}

int WaveObject1::parse_in(Dataport *fp)
{
	char	token[4096];
	char	*ttoken;
	char	**mtl_names=NULL;
	int	i, j, curr_color=0, num_colors=0;
	FILE_Dataport	*tfdp;

	do {
		if(!get_next_token(fp, token)) {
//			fprintf(stderr," Whoops - Unexpected EOF in polygon file\n");
//			return(FALSE);
			return(TRUE);	// files without ending keyword are okay
		}
		if(!strcmp(token, "#")) {
			skip_line(fp);
		} else if(!strcmp(token, "OBJECT_NAME")) {
			if(!get_next_token(fp, token)) {
				fprintf(stderr," Whoops - Unexpected EOF in polygon file\n");
				return(FALSE);
			}
			if(name)free(name);
			name = strdup(token);
		} else if (!strcmp( token, "GRAPE_SURFACE")) {
			if (!surface.parse_in( fp)) {
				fprintf(stderr," Whoops - Unable to parse in object's surface\n");
				return FALSE;
			}
		} else if(!strcmp(token, "CENTROID")) {
			x.parse_in(fp);
			y.parse_in(fp);
			z.parse_in(fp);
			xrot.parse_in(fp);
			yrot.parse_in(fp);
			zrot.parse_in(fp);
			xscale.parse_in(fp);
			yscale.parse_in(fp);
			zscale.parse_in(fp);
		} else if(!strcmp(token, "OBJECT_TYPE")) {
			if(!get_next_token(fp, token)) {
				fprintf(stderr," Whoops - Unexpected EOF in polygon file\n");
				return(FALSE);
			}
			object_type = PolyObject1::poly_object_from_string(token);
			if(object_type == BadPolyObj) {
				fprintf(stderr," Whoops - Unknown object type %s\n", token);
				return(FALSE);
			}
			else if (object_type == TextureMapped) {

								// get the texture file name
				if(!get_next_token(fp, token)) {
					fprintf(stderr," Whoops - Unexpected EOF in polygon file\n");
					return(FALSE);
				}
				PolyObject1::set_texture_name( token);
			}
		} else if(!strcmp(token, "FILE")) {
			if(!get_next_token(fp, token)) {
				fprintf(stderr," Whoops - Unexpected EOF in polygon file\n");
				return(FALSE);
			}
			tfdp = new FILE_Dataport();
			if (!tfdp->ropen(token)) {
				fprintf(stderr," Whoops - unable to open wavefront object file '%s'\n",
						token);
				return FALSE;	// error
			}
			parse_in(tfdp);
		} else if(!strcmp(token, "vt")) { // texture coords
			if(!get_next_token(fp, token)) {
				fprintf(stderr," Whoops - Unexpected EOF in polygon file\n");
				return(FALSE);
			}
			x_text.add_value(atof(token));
			if(!get_next_token(fp, token)) {
				fprintf(stderr," Whoops - Unexpected EOF in polygon file\n");
				return(FALSE);
			}
			y_text.add_value(atof(token));
			if(!get_next_token(fp, token)) {
				fprintf(stderr," Whoops - Unexpected EOF in polygon file\n");
				return(FALSE);
			}
			z_text.add_value(atof(token));
			skip_line(fp);
#if 0
		} else if (!strcmp(token, "vn")) { // normal vectors
			if(!get_next_token(fp, token)) {
				fprintf(stderr," Whoops - Unexpected EOF in polygon file\n");
				return(FALSE);
			}
			x_norm.add_value(atof(token));
			if(!get_next_token(fp, token)) {
				fprintf(stderr," Whoops - Unexpected EOF in polygon file\n");
				return(FALSE);
			}
			y_norm.add_value(atof(token));
			if(!get_next_token(fp, token)) {
				fprintf(stderr," Whoops - Unexpected EOF in polygon file\n");
				return(FALSE);
			}
			z_norm.add_value(atof(token));
			skip_line(fp);
#endif
		} else if(!strcmp(token, "v")) { // vertex coords
			if(!get_next_token(fp, token)) {
				fprintf(stderr," Whoops - Unexpected EOF in polygon file\n");
				return(FALSE);
			}
			x_vert.add_value(atof(token));
			if(!get_next_token(fp, token)) {
				fprintf(stderr," Whoops - Unexpected EOF in polygon file\n");
				return(FALSE);
			}
			y_vert.add_value(atof(token));
			if(!get_next_token(fp, token)) {
				fprintf(stderr," Whoops - Unexpected EOF in polygon file\n");
				return(FALSE);
			}
			z_vert.add_value(atof(token));
			point_colors.add_value(curr_color);
			skip_line(fp);
		} else if(!strcmp(token, "f")) {
			i = 0;
			j = fp->pgetc();
			while(j != '\n' && j != EOF && i < 4096) {
				token[i++] = j;
				j = fp->pgetc();
			}
			token[i] = '\0';
			poly_colors.add_value(curr_color);
			poly_verts.add_list();
			text_verts.add_list();

			// int num_verts = text_verts.get_num();

			ttoken = strtok(token, " \t");
			char *pStr, *pSlash;
			int ndx;
			while(ttoken) {
				pStr = ttoken;	// start at beg of token
				pSlash = strchr( pStr, '/');
				if (pSlash != NULL) {
					*pSlash = '\0';
					pStr = pSlash + 1; // skip past slash
				}
				else
					pStr += strlen( pStr);
				
								// set vertex index
				ndx = atoi(ttoken)-1;
				poly_verts.add_value(poly_verts.get_num()-1, ndx);

				
				if (*pStr == '/') {
					*pSlash = '\0';
					pStr = pSlash + 1; // skip past slash
				}

				else if (*pStr != '\0') {
						// not yet at end of original ttoken

								// set texture index
					pSlash = strchr( pStr, '/'); // find next slash
					if (pSlash != NULL) {
						*pSlash = '\0';
//						text_verts.set_value( ndx, atoi( pStr)-1);
						text_verts.add_value( text_verts.get_num()-1, 
											 atoi( pStr)-1);
						pStr = pSlash + 1; // skip past slash
					}
					else {
//						text_verts.set_value( ndx, atoi( pStr)-1);
						text_verts.add_value( text_verts.get_num()-1, 
											 atoi( pStr)-1);
						pStr += strlen( pStr);					
					}
				}

#if 0
								// deal with normals
				if (*pStr != '\0') { // not yet at end of original ttoken

					if (atoi( pStr) > point_normals.get_num())
						point_normals.set_num( atoi( pStr));
					point_normals[ ndx ] = point_normals.get_num() - 1;
					
				}
#endif

				ttoken = strtok(NULL, " \t");
			}
		} else if(!strcmp(token, "usemtl")) {
			if(!get_next_token(fp, token)) {
				fprintf(stderr," Whoops - Unexpected EOF in polygon file\n");
				return(FALSE);
			}
			for(curr_color=0; curr_color<num_colors; curr_color++) {
				if(!strcmp(token, mtl_names[curr_color])) {
					break;
				}
			}
			if(curr_color >= num_colors) {
				fprintf(stderr,"Whoops - Material %s not found in library.\n", token);
				curr_color = 0;
			}
			skip_line(fp);
		} else if(!strcmp(token, "mtllib")) {
			if(!get_next_token(fp, token)) {
				fprintf(stderr," Whoops - Unexpected EOF in polygon file\n");
				return(FALSE);
			}
			tfdp = new FILE_Dataport();
			if (!tfdp->ropen(token)) {
				fprintf(stderr," Whoops - Unable to open material library %s\n",
						token);
			}
			while(get_next_token(tfdp, token)) {
				if(!strcmp(token, "newmtl")) {
					if(!get_next_token(tfdp, token)) {
						fprintf(stderr," Whoops - Unexpected EOF in mtllib file\n");
						return(FALSE);
					}
					num_colors++;
					mtl_names = (char **)realloc((void *)mtl_names, num_colors * sizeof(char *));
					mtl_names[num_colors-1] = strdup(token);
				} else if(!strcmp(token, "Kd")) {
					if(!get_next_token(tfdp, token)) {
						fprintf(stderr," Whoops - Unexpected EOF in mtllib file\n");
						return(FALSE);
					}
					r.add_value(atof(token));
					if(!get_next_token(tfdp, token)) {
						fprintf(stderr," Whoops - Unexpected EOF in mtllib file\n");
						return(FALSE);
					}
					g.add_value(atof(token));
					if(!get_next_token(tfdp, token)) {
						fprintf(stderr," Whoops - Unexpected EOF in mtllib file\n");
						return(FALSE);
					}
					b.add_value(atof(token));
				}
			} 
			tfdp->close();
			delete(tfdp);
			skip_line(fp);
		} else if(strcmp(token, "#END_OBJ")) {
			skip_line(fp);
		}
	} while(strcmp(token, "#END_OBJ"));

	if(mtl_names) {
		for(i=0; i<num_colors; i++) {
			free(mtl_names[i]);
		}
		free(mtl_names);
	}


	int  dbg = 0;
	if (dbg)
		dump_texture_map( );

	return(TRUE);
	
}

