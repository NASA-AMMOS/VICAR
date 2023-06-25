// playback.C
// contains functions for playback object and renderer

#include "grape/playback.h"

char *PB_Obj::get_file_name_at_time(void)
{
	char	temp[4096];
	int	frame;
	double	curr_time;

	curr_time = get_time();
	if(loop) {
		if(curr_time < 0)curr_time -= ((int)(curr_time/(end_time-start_time))-1) * (end_time-start_time);
		curr_time -= start_time;
		curr_time = fmod(curr_time, end_time-start_time) + start_time;
	} else if(hard_limit) {
		if(curr_time < start_time || curr_time > end_time) return(NULL);
	}
	if(curr_time > end_time) curr_time = end_time;
	if(curr_time < start_time) curr_time = start_time;
	frame = (int)((curr_time - start_time + delta_time/2) / delta_time);

	sprintf(temp, file_root, frame, frame, frame, frame, frame);
	if(file_name)free(file_name);
	file_name = strdup(temp);

	return(file_name);
}

char *PB_Obj::get_z_file_name_at_time(void)
{
	char	temp[4096];
	int	frame;
	double	curr_time;

	if(!z_file_root)return(NULL);
	curr_time = get_time();
	if(loop) {
		if(curr_time < 0)curr_time -= ((int)(curr_time/(end_time-start_time))-1) * (end_time-start_time);
		curr_time -= start_time;
		curr_time = fmod(curr_time, end_time-start_time) + start_time;
	} else if(hard_limit) {
		if(curr_time < start_time || curr_time > end_time) return(NULL);
	}
	if(curr_time > end_time) curr_time = end_time;
	if(curr_time < start_time) curr_time = start_time;
	frame = (int)((curr_time - start_time + delta_time/2) / delta_time);

	sprintf(temp, z_file_root, frame, frame, frame, frame, frame);
	if(z_file_name)free(z_file_name);
	z_file_name = strdup(temp);

	return(z_file_name);
}

int PB_Obj::parse_in(Dataport *fp)
{
	char	token[4096];

	do {
		if(!get_next_token(fp, token)) {
			fprintf(stderr," Whoops - Unexpected EOF in playback file\n");
			return(FALSE);
		}
		if(!strcmp(token, "NAME")) {
			if(!get_next_token(fp, token)) {
				fprintf(stderr," Whoops - Unexpected EOF in playback file\n");
				return(FALSE);
			}
			set_name(token);
		} else if(!strcmp(token, "VERSION")) {
			if(!get_next_token(fp, token)) {
				fprintf(stderr," Whoops - Unexpected EOF in playback file\n");
				return(FALSE);
			}
			if(strcmp(token, "1.0")) {
				 fprintf(stderr," Whoops - Unexpected version (%s) in playback file != 1.0\n",token);
				return(FALSE);
			}
			version = strdup(token);
		} else if(!strcmp(token, "IMAGE_FILE")) {
			if(!get_next_token(fp, token)) {
				fprintf(stderr," Whoops - Unexpected EOF in playback file\n");
				return(FALSE);
			}
			file_root = strdup(token);
		} else if(!strcmp(token, "Z_FILE")) {
			if(!get_next_token(fp, token)) {
				fprintf(stderr," Whoops - Unexpected EOF in playback file\n");
				return(FALSE);
			}
			z_file_root = strdup(token);
		} else if(!strcmp(token, "START")) {
			if(!get_next_token(fp, token)) {
				fprintf(stderr," Whoops - Unexpected EOF in playback file\n");
				return(FALSE);
			}
			start_time = atof(token);
		} else if(!strcmp(token, "END")) {
			if(!get_next_token(fp, token)) {
				fprintf(stderr," Whoops - Unexpected EOF in playback file\n");
				return(FALSE);
			}
			end_time = atof(token);
		} else if(!strcmp(token, "DELTA")) {
			if(!get_next_token(fp, token)) {
				fprintf(stderr," Whoops - Unexpected EOF in playback file\n");
				return(FALSE);
			}
			delta_time = atof(token);
		} else if(!strcmp(token, "LOOP")) {
			loop = TRUE;
		} else if(!strcmp(token, "INTERPOLATE")) {
			interpolate = TRUE;
		} else if(!strcmp(token, "X_OFFSET")) {
			x_offset.parse_in(fp);
		} else if(!strcmp(token, "Y_OFFSET")) {
			y_offset.parse_in(fp);
		} else if(!strcmp(token, "X_SCALE")) {
			x_scale.parse_in(fp);
		} else if(!strcmp(token, "Y_SCALE")) {
			y_scale.parse_in(fp);
		} else if(!strcmp(token, "SCALE")) {
			x_scale.parse_in(fp);
			set_scale(get_x_scale(), get_x_scale());
		} else if(!strcmp(token, "DEFAULT_ALPHA")) {
			default_alpha.parse_in(fp);
		} else if(strcmp(token, "END_PLAYBACK")) {
			fprintf(stderr," Whoops - Unexpected token in playback object file = %s\n", token);
			return(FALSE);
		}
	} while(strcmp(token, "END_PLAYBACK"));

	return(TRUE);
}

int PB_Obj::parse_out(Dataport *fp, int flag)
{
	char	token[4096];

	put_token(fp, "\nPLAY_V1");
	if(get_name()) {
		put_token(fp, "\nNAME");
		put_token(fp, get_name());
	}
	if(version) {
		put_token(fp, "\nVERSION");
		put_token(fp, version);
	}
	if(file_root) {
		put_token(fp, "\nIMAGE_FILE");
		put_token(fp, file_root);
	}
	if(z_file_root) {
		put_token(fp, "\nZ_FILE");
		put_token(fp, z_file_root);
	}
	put_token(fp, "\nSTART");
	sprintf(token, "%f", start_time);
	put_token(fp, token);
	put_token(fp, "\nEND");
	sprintf(token, "%f", end_time);
	put_token(fp, token);
	put_token(fp, "\nDELTA");
	sprintf(token, "%f", delta_time);
	put_token(fp, token);
	if(loop)put_token(fp, "\nLOOP");
	if(interpolate)put_token(fp, "\nINTERPOLATE");
	put_token(fp, "\nX_OFFSET");
	x_offset.parse_out(fp, flag);
	put_token(fp, "\nY_OFFSET");
	y_offset.parse_out(fp, flag);
	put_token(fp, "\nX_SCALE");
	x_offset.parse_out(fp, flag);
	put_token(fp, "\nY_SCALE");
	y_offset.parse_out(fp, flag);
	put_token(fp, "\nDEFAULT_ALPHA");
	default_alpha.parse_out(fp, flag);
	put_token(fp, "\nEND_PLAYBACK");

	return(TRUE);
}

// ****************************************  Renderer functions for playback objects *******

int PBrenderer::set_scene(Scene *s, ImgInfo *)
{
	int	i;

	scene = s;
//	if(!pb_obj) {
		// search thru scene to find first pb object
		for(i=0; i<s->get_num_objects(); i++) {
			if(s->get_obj(i)->get_object()->get_type() == PLAY_V1) {
				pb_obj = (PB_Obj *)(s->get_obj(i)->get_object());
				return(TRUE);
			}
		}
		return(FALSE);	// didn't find one
//	}

//	return(0);	// already have one
}

int PBrenderer::render(ObjNode *, ImgInfo *img)
{
	Image	*timg;
	Image	*outimg;
	int	zband=0;
	int	temp_rla_mode;
	int	i, j;
	int	width, height, bands;
	uchar	r,g,b,a;

	double	ti,tj;

	if(!pb_obj) {
		fprintf(stderr," Whoops - Unable to render playback without object.\n");
		return(FALSE);
	}

	img->prepare();

	img->get_image(0)->set_default_alpha();

	if(img->get_image_flag() || img->get_alpha_flag()) {
		if(file_name)free(file_name);
		file_name = strdup(pb_obj->get_file_name_at_time());
		if(file_name) {
			temp_rla_mode = RLA_Load;
			RLA_Load = RLA_NO_AUX;
fprintf(stderr,"playing back frame from file %s\n", file_name);
			if(outimg = img->get_image(0)) {
				timg = new Image(file_name);
				/* copy to outimg */
				outimg->get_res(&width, &height, &bands);
				if(pb_obj->interpolation()) {
fprintf(stderr,"playback with interpolation not functional yet\n");
				} else {
				    //for(k=0; k<bands; k++) {
					for(i=0; i<height; i++) {
						for(j=0; j<width; j++) {
							ti = i+(pb_obj->get_y_offset());
							tj = j+(pb_obj->get_x_offset());
							timg->get_data()->iget_color(tj,ti,&r,&g,&b,&a);
//							timg->get_color((int)tj,(int)ti,&r,&g,&b,&a);
							outimg->set_color(j,i,r,g,b,a);
						}
					}
				    //}
				}
				delete(timg);
			}
			RLA_Load = temp_rla_mode;
		}
		zband++;
	}


	if(img->get_z_flag()) {
		if(file_name)free(file_name);
		file_name = NULL;
		if(pb_obj->get_z_file_name_at_time())file_name = strdup(pb_obj->get_z_file_name_at_time());
		if(file_name) {
			temp_rla_mode = RLA_Load;
			RLA_Load = RLA_AUX_ONLY;
fprintf(stderr,"playing back z from file %s\n", file_name);
			if(timg = img->get_image(zband)) timg->read(file_name);
			RLA_Load = temp_rla_mode;
		}
	}

	img->dispose();
fprintf(stderr,"Just disposed of image in playback render.\n");

	return(TRUE);
}
