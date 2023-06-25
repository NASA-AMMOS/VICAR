// animate.C

#include "grape/animate.h"
#include "grape/metarender.h"

int Shot::parse_in(Dataport *fp)
{
        char    token[4096];
	char	*rtemp;
        Dataport        *tfp;
	int		cam_num = 0;

        do {
                if(!get_next_token(fp, token)) {
                        fprintf(stderr," Whoops - Unexpected EOF in shot file\n");
                        return(FALSE);
                }
		if(!strcmp(token, "SHOT_V1")) {
			// eat it
                } else if(!strcmp(token, "NAME")) {
                        if(!get_next_token(fp, token)) {
                                fprintf(stderr," Whoops - Unexpected EOF in shot file\n");
                                return(FALSE);
                        }
                        name = strdup(token);
                } else if(!strcmp(token, "VERSION")) {
                        if(!get_next_token(fp, token)) {
                                fprintf(stderr," Whoops - Unexpected EOF in shot file\n");
                                return(FALSE);
                        }
                        if(strcmp(token, "1.0")) {
                                fprintf(stderr," Whoops - Unexpected version number encountered in parsing shot object\n");
                                fprintf(stderr,"   Token=>%s<  Expecting >1.0<\n", token);
                                return(FALSE);
                        }
                        version = strdup(token);
                } else if(!strcmp(token, "START")) {
                        if(!get_next_token(fp, token)) {
                                fprintf(stderr," Whoops - Unexpected EOF in shot file\n");
                                return(FALSE);
                        }
                        start_time = atof(token);
                } else if(!strcmp(token, "END")) {
                        if(!get_next_token(fp, token)) {
                                fprintf(stderr," Whoops - Unexpected EOF in shot file\n");
                                return(FALSE);
                        }
                        end_time = atof(token);
                } else if(!strcmp(token, "SCENE")) {
                        if(!get_next_token(fp, token)) {
                                fprintf(stderr," Whoops - Unexpected EOF in shot file\n");
                                return(FALSE);
                        }
			if(!strcmp(token, "{")) {	// inline object reference
				// call object creator with current fp
				scene = new Scene();
				if(!scene->parse_in(fp)) {
					fprintf(stderr,"Whoops - Problems parsing inline scene\n");
					return(FALSE);
				}
				if(!get_next_token(fp, token)) {
					fprintf(stderr," Whoops - Unexpected EOF in shot file\n");
					return(FALSE);
				}
				if(strcmp(token, "}")) {
					fprintf(stderr," Whoops - Missing } after inline scene\n");
					return(FALSE);
				}
			} else {	// must be external reference
				rtemp = strdup(token);
				tfp = dataport_create(fp->get_type());
				if(tfp && tfp->ropen(token)) {
					scene = new Scene();
					if(!scene->parse_in(tfp)) {
						fprintf(stderr,"Whoops - Problems parsing referenced scene %s\n",token);
						return(FALSE);
					}
					tfp->close();
					scene->set_reference(rtemp);
				} else {
					fprintf(stderr,"Whoops - Unable to open referenced scene file %s\n", token);
					return(FALSE);
				}
				free(rtemp);
			}
                } else if(!strcmp(token, "SCENE_CAM")) {
                        if(!get_next_token(fp, token)) {
                                fprintf(stderr," Whoops - Unexpected EOF in shot file\n");
                                return(FALSE);
                        }
                        cam_num = atoi(token);
                } else if(!strcmp(token, "CAMERA")) {
                        if(!get_next_token(fp, token)) {
                                fprintf(stderr," Whoops - Unexpected EOF in shot file\n");
                                return(FALSE);
                        }
			camera = new ObjNode();
			if(!camera->parse_in(fp)) {
				fprintf(stderr,"Whoops - Problems parsing camera in shot\n");
				return(FALSE);
			}
                } else if(strcmp(token, "END_SHOT")) {
                        fprintf(stderr," Whoops - Unexpected token encountered in parsing shot object\n");
                        fprintf(stderr,"   Token=>%s<  Expecting >END_SHOT<\n", token);
                        return(FALSE);
                }
        } while (strcmp(token, "END_SHOT"));

	if(cam_num > 0 && scene) {
		set_camera(scene->get_camera(cam_num));
	}

        return(TRUE);
}

int Shot::parse_out(Dataport *fp, int expand)
{
        char    token[4096];

	put_token(fp, "SHOT_V1");
	if(name) {
		put_token(fp, "\nNAME");
		put_token(fp, name);
	}
	put_token(fp, "\nVERSION");
	put_token(fp, "1.0");
	put_token(fp, "\nSTART");
	sprintf(token, "%f", start_time);
	put_token(fp, token);
	put_token(fp, "\nEND");
	sprintf(token, "%f", end_time);
	put_token(fp, token);
	if(scene) {
		put_token(fp, "\nSCENE");
		if(expand || !(scene->get_reference())) {
			put_token(fp, "{");
			scene->parse_out(fp, expand);
			put_token(fp, "}");
		} else {
			put_token(fp, scene->get_reference());
		}
	}
	if(camera) {
		if(scene) {
			if(scene->get_camera_num(camera)) {
				put_token(fp, "\nSCENE_CAM");
				sprintf(token, "%d", scene->get_camera_num(camera));
				put_token(fp, token);
			} else {
				put_token(fp, "\nCAMERA");
				get_camera()->parse_out(fp, expand);
			}
		} else {
			put_token(fp, "\nCAMERA");
			get_camera()->parse_out(fp, expand);
		}
	}
	put_token(fp, "\nEND_SHOT");
	return(TRUE);
}

// convenience function to read dataport and create correct transition
Transition	*create_transition(Dataport *fp)
{
	char	token[4096];


        if(!get_next_token(fp, token)) {
                fprintf(stderr," Whoops - Unexpected EOF in parsing transition file\n");
                return(FALSE);
        }
        if(!strcmp(token,"CUT_V1")) {
                return(new Cut_Transition());
	} else if(!strcmp(token,"FADE_V1")) {
                return(new Fade_Transition());
	} else if(!strcmp(token,"WIPE_V1")) {
                return(new Wipe_Transition());
	} else {
		fprintf(stderr,"Whoops - Unrecognized transition type in creator = %s\n", token);
        }
        return(NULL);
}


void Transition::render(ImgInfo *)
{
}

int Transition::parse_in(Dataport *)
{
        return(FALSE);
}

int Transition::parse_out(Dataport *, int )
{
	return(FALSE);
}

int Transition::set_scene(ImgInfo *inimg) 
{
	scene_set = FALSE;
	if(shot && rndr) {
		if(shot->get_scene()) {
			if(inimg)((Camera *)(shot->get_camera()->get_object()))->set_img_info(inimg->duplicate(TRUE));	// TRUE so Xwindow and Xdisplay ptrs copied
//			((Camera *)(shot->get_camera()->get_object()))->get_img_info()->set_image_fill();	// why is this being set?
			rndr->set_scene(shot->get_scene(), ((Camera *)(shot->get_camera()->get_object()))->get_img_info());
			scene_set = TRUE;
			return(TRUE);
		}
	}
	return(FALSE);
}

void Cut_Transition::render(ImgInfo *img)
{
	double	curr_time;
	int	i, j, k, w, h, b, z;
	Image	*timg;
	ImageData	*td1, *td2;

	curr_time = get_time();
	if(curr_time >= get_start_time() && curr_time <= get_end_time()) {
		if(!scene_is_set()) {
			set_scene(img);
		}
		// set time to shot local
		set_time((curr_time - time_offset)/time_scale);
		shot->get_scene()->set_camera(shot->get_camera());
// fprintf(stderr,"Rendering cut transition.\n");
		if(rndr) {
			rndr->render(shot->get_camera());
		} else {
			fprintf(stderr," Whoops - Need to set renderer in transition.\n");
			return;
		}
		// replace old image with new one
		// img->set_data(shot->get_camera()->get_img_info()->get_image(0)->get_data());
		z = 0;
		if(img->get_image_flag() || img->get_alpha_flag()) {
			z = 1;
			timg = img->get_image(0);
			timg->get_res(&w, &h, &b);
			td1 = timg->get_data();
			td2 = ((Camera *)(shot->get_camera()->get_object()))->get_img_info()->get_image(0)->get_data();
			for(i=0; i<w; i++) {
				for(j=0; j<h; j++) {
					for(k=0; k<b; k++) {
						// copy pixel values for all bands
						td1->set_uchar(td2->get_uchar(i,j,k),i,j,k);
// fprintf(stderr,"Set pixel x=%d y=%d b=%d to value %d from %d in cut.\n",i,j,k,td1->get_uchar(i,j,k),td2->get_uchar(i,j,k));
					}
				}
			}
		}
		if(img->get_z_flag()) {
			timg = img->get_image(z);
			timg->get_res(&w, &h, &b);
			td1 = timg->get_data();
			td2 = ((Camera *)(shot->get_camera()->get_object()))->get_img_info()->get_image(0)->get_data();
			for(i=0; i<w; i++) {
				for(j=0; j<h; j++) {
					for(k=0; k<b; k++) {
						// copy pixel values for all bands
						td1->set_float(td2->get_float(i,j,k),i,j,k);
					}
				}
			}
		}
		// set time back to global clock time
		set_time(curr_time);
	}
}

int Cut_Transition::parse_in(Dataport *fp)
{
        char    token[4096];
	char	*rtemp;
        Dataport        *tfp;

        do {
                if(!get_next_token(fp, token)) {
                        fprintf(stderr," Whoops - Unexpected EOF in transition file\n");
                        return(FALSE);
                }
		if(!strcmp(token, "CUT_V1")) {
			// eat it
                } else if(!strcmp(token, "NAME")) {
                        if(!get_next_token(fp, token)) {
                                fprintf(stderr," Whoops - Unexpected EOF in transition file\n");
                                return(FALSE);
                        }
                        name = strdup(token);
                } else if(!strcmp(token, "VERSION")) {
                        if(!get_next_token(fp, token)) {
                                fprintf(stderr," Whoops - Unexpected EOF in transition file\n");
                                return(FALSE);
                        }
                        if(strcmp(token, "1.0")) {
                                fprintf(stderr," Whoops - Unexpected version number encountered in parsing transition object\n");
                                fprintf(stderr,"   Token=>%s<  Expecting >1.0<\n", token);
                                return(FALSE);
                        }
                        version = strdup(token);
                } else if(!strcmp(token, "START")) {
                        if(!get_next_token(fp, token)) {
                                fprintf(stderr," Whoops - Unexpected EOF in transition file\n");
                                return(FALSE);
                        }
                        start_time = atof(token);
                } else if(!strcmp(token, "END")) {
                        if(!get_next_token(fp, token)) {
                                fprintf(stderr," Whoops - Unexpected EOF in transition file\n");
                                return(FALSE);
                        }
                        end_time = atof(token);
                } else if(!strcmp(token, "OFFSET")) {
                        if(!get_next_token(fp, token)) {
                                fprintf(stderr," Whoops - Unexpected EOF in transition file\n");
                                return(FALSE);
                        }
                        time_offset = atof(token);
                } else if(!strcmp(token, "SCALE")) {
                        if(!get_next_token(fp, token)) {
                                fprintf(stderr," Whoops - Unexpected EOF in transition file\n");
                                return(FALSE);
                        }
                        time_scale = atof(token);
                } else if(!strcmp(token, "SHOT")) {
                        if(!get_next_token(fp, token)) {
                                fprintf(stderr," Whoops - Unexpected EOF in transition file\n");
                                return(FALSE);
                        }
			if(!strcmp(token, "{")) {	// inline object reference
				shot = new Shot();
				if(!shot->parse_in(fp)) {
					fprintf(stderr,"Whoops - Problems parsing inline shot\n");
					return(FALSE);
				}
				if(!get_next_token(fp, token)) {
					fprintf(stderr," Whoops - Unexpected EOF in transition file\n");
					return(FALSE);
				}
				if(strcmp(token, "}")) {
					fprintf(stderr," Whoops - Missing } after inline shot\n");
					return(FALSE);
				}
			} else {	// must be external reference
				rtemp = strdup(token);
				tfp = dataport_create(fp->get_type());
				if(tfp && tfp->ropen(token)) {
					shot = new Shot();
					if(!shot->parse_in(tfp)) {
						fprintf(stderr,"Whoops - Problems parsing referenced shot %s\n",token);
						return(FALSE);
					}
					tfp->close();
					shot->set_reference(rtemp);
				} else {
					fprintf(stderr,"Whoops - Unable to open referenced shot file %s\n", token);
					return(FALSE);
				}
				free(rtemp);
			}
                } else if(strcmp(token, "END_TRANSITION")) {
                        fprintf(stderr," Whoops - Unexpected token encountered in parsing transition object\n");
                        fprintf(stderr,"   Token=>%s<  Expecting >END_TRANSITION<\n", token);
                        return(FALSE);
                }
        } while (strcmp(token, "END_TRANSITION"));

        return(TRUE);
}

int Cut_Transition::parse_out(Dataport *fp, int expand)
{
        char    token[4096];

	put_token(fp, "CUT_V1");
	if(name) {
		put_token(fp, "\nNAME");
		put_token(fp, name);
	}
	put_token(fp, "\nVERSION");
	put_token(fp, "1.0");
	put_token(fp, "\nSTART");
	sprintf(token, "%f", start_time);
	put_token(fp, token);
	put_token(fp, "\nEND");
	sprintf(token, "%f", end_time);
	put_token(fp, token);
	put_token(fp, "\nOFFSET");
	sprintf(token, "%f", time_offset);
	put_token(fp, token);
	put_token(fp, "\nSCALE");
	sprintf(token, "%f", time_scale);
	put_token(fp, token);
	if(shot) {
		put_token(fp, "\nSHOT");
		if(expand || !(shot->get_reference())) {
			put_token(fp, "{");
			shot->parse_out(fp, expand);
			put_token(fp, "}");
		} else {
			put_token(fp, shot->get_reference());
		}
	}
	put_token(fp, "\nEND_TRANSITION");
	return(TRUE);
}

void Wipe_Transition::render(ImgInfo *img)
{
	double	curr_time;

	curr_time = get_time();
	if(curr_time >= get_start_time() && curr_time <= get_end_time()) {
		if(!scene_is_set()) {
			set_scene(img);
		}
		// set time to shot local
		set_time((curr_time - time_offset)/time_scale);
		shot->get_scene()->set_camera(shot->get_camera());
// fprintf(stderr,"Rendering wipe transition.\n");
		if(rndr) {
			rndr->render(shot->get_camera());
		} else {
			fprintf(stderr," Whoops - Need to set renderer in transition.\n");
			return;
		}
		// replace old image with new one if after duration
		// else blend
		// set time back to global clock time
		set_time(curr_time);
	}
}

int Wipe_Transition::parse_in(Dataport *)
{
	return(FALSE);
}

int Wipe_Transition::parse_out(Dataport *, int )
{
	return(FALSE);
}

void Fade_Transition::render(ImgInfo *img)
{
	double	curr_time, ratio;
	int	i, j, k, w, h, b, z;
	Image	*timg;
	ImageData	*td1, *td2;

	curr_time = get_time();
	if(curr_time >= get_start_time() && curr_time <= get_end_time()) {
		if(!scene_is_set()) {
			set_scene(img);
		}
		// set time to shot local
		set_time((curr_time - time_offset)/time_scale);
		shot->get_scene()->set_camera(shot->get_camera());
// fprintf(stderr,"Rendering fade transition.\n");
		if(rndr) {
			rndr->render(shot->get_camera());
		} else {
			fprintf(stderr," Whoops - Need to set renderer in transition.\n");
			return;
		}
		// replace old image with new one if after duration
		if(curr_time > start_time + duration) {
		z = 0;
		if(img->get_image_flag() || img->get_alpha_flag()) {
			z = 1;
			timg = img->get_image(0);
			timg->get_res(&w, &h, &b);
			td1 = timg->get_data();
			td2 = ((Camera *)(shot->get_camera()->get_object()))->get_img_info()->get_image(0)->get_data();
			for(i=0; i<w; i++) {
				for(j=0; j<h; j++) {
					for(k=0; k<b; k++) {
						// copy pixel values for all bands
						td1->set_uchar(td2->get_uchar(i,j,k),i,j,k);
					}
				}
			}
		}
		if(img->get_z_flag()) {
			timg = img->get_image(z);
			timg->get_res(&w, &h, &b);
			td1 = timg->get_data();
			td2 = ((Camera *)(shot->get_camera()->get_object()))->get_img_info()->get_image(0)->get_data();
			for(i=0; i<w; i++) {
				for(j=0; j<h; j++) {
					for(k=0; k<b; k++) {
						// copy pixel values for all bands
						td1->set_float(td2->get_float(i,j,k),i,j,k);
					}
				}
			}
		}
		} else {	// else blend
		z = 0;
		ratio = (curr_time - start_time) / duration;
		if(img->get_image_flag() || img->get_alpha_flag()) {
			z = 1;
			timg = img->get_image(0);
			timg->get_res(&w, &h, &b);
			td1 = timg->get_data();
			td2 = ((Camera *)(shot->get_camera()->get_object()))->get_img_info()->get_image(0)->get_data();
			for(i=0; i<w; i++) {
				for(j=0; j<h; j++) {
					for(k=0; k<b; k++) {
						// copy pixel values for all bands
						td1->set_uchar((uchar)(td2->get_uchar(i,j,k)*ratio + td1->get_uchar(i,j,k)*(1.0-ratio)),i,j,k);
					}
				}
			}
		}
		if(img->get_z_flag()) {
			timg = img->get_image(z);
			timg->get_res(&w, &h, &b);
			td1 = timg->get_data();
			td2 = ((Camera *)(shot->get_camera()->get_object()))->get_img_info()->get_image(0)->get_data();
			for(i=0; i<w; i++) {
				for(j=0; j<h; j++) {
					for(k=0; k<b; k++) {
						// copy pixel values for all bands
						td1->set_float(td2->get_float(i,j,k)*ratio + td1->get_float(i,j,k)*(1.0-ratio),i,j,k);
					}
				}
			}
		}
		}
		// set time back to global clock time
		set_time(curr_time);
	}
}

int Fade_Transition::parse_in(Dataport *fp)
{
        char    token[4096];
	char	*rtemp;
        Dataport        *tfp;

        do {
                if(!get_next_token(fp, token)) {
                        fprintf(stderr," Whoops - Unexpected EOF in transition file\n");
                        return(FALSE);
                }
		if(!strcmp(token, "FADE_V1")) {
			// eat it
                } else if(!strcmp(token, "NAME")) {
                        if(!get_next_token(fp, token)) {
                                fprintf(stderr," Whoops - Unexpected EOF in transition file\n");
                                return(FALSE);
                        }
                        name = strdup(token);
                } else if(!strcmp(token, "VERSION")) {
                        if(!get_next_token(fp, token)) {
                                fprintf(stderr," Whoops - Unexpected EOF in transition file\n");
                                return(FALSE);
                        }
                        if(strcmp(token, "1.0")) {
                                fprintf(stderr," Whoops - Unexpected version number encountered in parsing transition object\n");
                                fprintf(stderr,"   Token=>%s<  Expecting >1.0<\n", token);
                                return(FALSE);
                        }
                        version = strdup(token);
                } else if(!strcmp(token, "START")) {
                        if(!get_next_token(fp, token)) {
                                fprintf(stderr," Whoops - Unexpected EOF in transition file\n");
                                return(FALSE);
                        }
                        start_time = atof(token);
                } else if(!strcmp(token, "END")) {
                        if(!get_next_token(fp, token)) {
                                fprintf(stderr," Whoops - Unexpected EOF in transition file\n");
                                return(FALSE);
                        }
                        end_time = atof(token);
                } else if(!strcmp(token, "OFFSET")) {
                        if(!get_next_token(fp, token)) {
                                fprintf(stderr," Whoops - Unexpected EOF in transition file\n");
                                return(FALSE);
                        }
                        time_offset = atof(token);
                } else if(!strcmp(token, "SCALE")) {
                        if(!get_next_token(fp, token)) {
                                fprintf(stderr," Whoops - Unexpected EOF in transition file\n");
                                return(FALSE);
                        }
                        time_scale = atof(token);
                } else if(!strcmp(token, "TYPE")) {
                        if(!get_next_token(fp, token)) {
                                fprintf(stderr," Whoops - Unexpected EOF in transition file\n");
                                return(FALSE);
                        }
                        type = atoi(token);
                } else if(!strcmp(token, "LENGTH")) {
                        if(!get_next_token(fp, token)) {
                                fprintf(stderr," Whoops - Unexpected EOF in transition file\n");
                                return(FALSE);
                        }
                        duration = atof(token);
                } else if(!strcmp(token, "SHOT")) {
                        if(!get_next_token(fp, token)) {
                                fprintf(stderr," Whoops - Unexpected EOF in transition file\n");
                                return(FALSE);
                        }
			if(!strcmp(token, "{")) {	// inline object reference
				shot = new Shot();
				if(!shot->parse_in(fp)) {
					fprintf(stderr,"Whoops - Problems parsing inline shot\n");
					return(FALSE);
				}
				if(!get_next_token(fp, token)) {
					fprintf(stderr," Whoops - Unexpected EOF in transition file\n");
					return(FALSE);
				}
				if(strcmp(token, "}")) {
					fprintf(stderr," Whoops - Missing } after inline shot\n");
					return(FALSE);
				}
			} else {	// must be external reference
				rtemp = strdup(token);
				tfp = dataport_create(fp->get_type());
				if(tfp && tfp->ropen(token)) {
					shot = new Shot();
					if(!shot->parse_in(tfp)) {
						fprintf(stderr,"Whoops - Problems parsing referenced shot %s\n",token);
						return(FALSE);
					}
					tfp->close();
					shot->set_reference(rtemp);
				} else {
					fprintf(stderr,"Whoops - Unable to open referenced shot file %s\n", token);
					return(FALSE);
				}
				free(rtemp);
			}
                } else if(strcmp(token, "END_TRANSITION")) {
                        fprintf(stderr," Whoops - Unexpected token encountered in parsing transition object\n");
                        fprintf(stderr,"   Token=>%s<  Expecting >END_TRANSITION<\n", token);
                        return(FALSE);
                }
        } while (strcmp(token, "END_TRANSITION"));

        return(TRUE);
}

int Fade_Transition::parse_out(Dataport *fp, int expand)
{
        char    token[4096];

	put_token(fp, "FADE_V1");
	if(name) {
		put_token(fp, "\nNAME");
		put_token(fp, name);
	}
	put_token(fp, "\nVERSION");
	put_token(fp, "1.0");
	put_token(fp, "\nSTART");
	sprintf(token, "%f", start_time);
	put_token(fp, token);
	put_token(fp, "\nEND");
	sprintf(token, "%f", end_time);
	put_token(fp, token);
	put_token(fp, "\nOFFSET");
	sprintf(token, "%f", time_offset);
	put_token(fp, token);
	put_token(fp, "\nSCALE");
	sprintf(token, "%f", time_scale);
	put_token(fp, token);
	put_token(fp, "\nTYPE");
	sprintf(token, "%d", type);
	put_token(fp, token);
	put_token(fp, "\nLENGTH");
	sprintf(token, "%f", duration);
	put_token(fp, token);
	if(shot) {
		put_token(fp, "\nSHOT");
		if(expand || !(shot->get_reference())) {
			put_token(fp, "{");
			shot->parse_out(fp, expand);
			put_token(fp, "}");
		} else {
			put_token(fp, shot->get_reference());
		}
	}
	put_token(fp, "\nEND_TRANSITION");
	return(TRUE);
}

int Animation::parse_in(Dataport *fp)
{
        char    token[4096];
	char	*rtemp;
        Dataport        *tfp;
	Transition	*transition;

        do {
                if(!get_next_token(fp, token)) {
                        fprintf(stderr," Whoops - Unexpected EOF in animation file\n");
                        return(FALSE);
                }
		if(!strcmp(token, "ANIM_V1")) {
			// eat it
                } else if(!strcmp(token, "NAME")) {
                        if(!get_next_token(fp, token)) {
                                fprintf(stderr," Whoops - Unexpected EOF in animation file\n");
                                return(FALSE);
                        }
                        name = strdup(token);
                } else if(!strcmp(token, "VERSION")) {
                        if(!get_next_token(fp, token)) {
                                fprintf(stderr," Whoops - Unexpected EOF in animation file\n");
                                return(FALSE);
                        }
                        if(strcmp(token, "1.0")) {
                                fprintf(stderr," Whoops - Unexpected version number encountered in parsing animation object\n");
                                fprintf(stderr,"   Token=>%s<  Expecting >1.0<\n", token);
                                return(FALSE);
                        }
                        version = strdup(token);
                } else if(!strcmp(token, "START")) {
                        if(!get_next_token(fp, token)) {
                                fprintf(stderr," Whoops - Unexpected EOF in animation file\n");
                                return(FALSE);
                        }
                        start_time = atof(token);
                } else if(!strcmp(token, "END")) {
                        if(!get_next_token(fp, token)) {
                                fprintf(stderr," Whoops - Unexpected EOF in animation file\n");
                                return(FALSE);
                        }
                        end_time = atof(token);
                } else if(!strcmp(token, "DELTA")) {
                        if(!get_next_token(fp, token)) {
                                fprintf(stderr," Whoops - Unexpected EOF in animation file\n");
                                return(FALSE);
                        }
                        delta_time = atof(token);
                } else if(!strcmp(token, "NOTIFY")) {
                        notify = TRUE;
                } else if(!strcmp(token, "IMGINFO")) {
                        if(!get_next_token(fp, token)) {
                                fprintf(stderr," Whoops - Unexpected EOF in animation file\n");
                                return(FALSE);
                        }
			if(!strcmp(token, "{")) {	// inline object reference
				img_info = create_img_info(fp);
				if(!img_info->parse_in(fp)) {
					fprintf(stderr,"Whoops - Problems parsing inline img_info\n");
					return(FALSE);
				}
				if(!get_next_token(fp, token)) {
					fprintf(stderr," Whoops - Unexpected EOF in animation file\n");
					return(FALSE);
				}
				if(strcmp(token, "}")) {
					fprintf(stderr," Whoops - Missing } after inline img_info\n");
					return(FALSE);
				}
			} else {	// must be external reference
				rtemp = strdup(token);
				tfp = dataport_create(fp->get_type());
				if(tfp && tfp->ropen(token)) {
					img_info = create_img_info(tfp);
					if(!img_info->parse_in(tfp)) {
						fprintf(stderr,"Whoops - Problems parsing referenced img_info %s\n",token);
						return(FALSE);
					}
					tfp->close();
					img_info->set_reference(rtemp);
				} else {
					fprintf(stderr,"Whoops - Unable to open referenced img_info file %s\n", token);
					return(FALSE);
				}
				free(rtemp);
			}
                } else if(!strcmp(token, "TRANSITION")) {
                        if(!get_next_token(fp, token)) {
                                fprintf(stderr," Whoops - Unexpected EOF in animation file\n");
                                return(FALSE);
                        }
			if(!strcmp(token, "{")) {	// inline object reference
				transition = create_transition(fp);
				if(!transition->parse_in(fp)) {
					fprintf(stderr,"Whoops - Problems parsing inline transition\n");
					return(FALSE);
				}
				if(!get_next_token(fp, token)) {
					fprintf(stderr," Whoops - Unexpected EOF in animation file\n");
					return(FALSE);
				}
				if(strcmp(token, "}")) {
					fprintf(stderr," Whoops - Missing } after inline transition\n");
					return(FALSE);
				}
			} else {	// must be external reference
				rtemp = strdup(token);
				tfp = dataport_create(fp->get_type());
				if(tfp && tfp->ropen(token)) {
					transition = create_transition(tfp);
					if(!transition->parse_in(tfp)) {
						fprintf(stderr,"Whoops - Problems parsing referenced transition %s\n",token);
						return(FALSE);
					}
					tfp->close();
					transition->set_reference(rtemp);
				} else {
					fprintf(stderr,"Whoops - Unable to open referenced transition file %s\n", token);
					return(FALSE);
				}
				free(rtemp);
			}
			add_transition(transition);
                } else if(strcmp(token, "END_ANIMATION")) {
                        fprintf(stderr," Whoops - Unexpected token encountered in parsing animation object\n");
                        fprintf(stderr,"   Token=>%s<  Expecting >END_ANIMATION<\n", token);
                        return(FALSE);
                }
        } while (strcmp(token, "END_ANIMATION"));

        return(TRUE);
}

int Animation::parse_out(Dataport *fp, int expand)
{
        char    token[4096];
	int	i;

	put_token(fp, "ANIM_V1");
	if(name) {
		put_token(fp, "\nNAME");
		put_token(fp, name);
	}
	put_token(fp, "\nVERSION");
	put_token(fp, "1.0");
	put_token(fp, "\nSTART");
	sprintf(token, "%f", start_time);
	put_token(fp, token);
	put_token(fp, "\nEND");
	sprintf(token, "%f", end_time);
	put_token(fp, token);
	put_token(fp, "\nDELTA");
	sprintf(token, "%f", delta_time);
	put_token(fp, token);
	if(notify)put_token(fp, "\nNOTIFY");
	for(i=0; i<tran_count; i++) {
		put_token(fp, "\nTRANSITION");
		if(expand || !(tran_list[i]->get_reference())) {
			put_token(fp, "{");
			tran_list[i]->parse_out(fp, expand);
			put_token(fp, "}");
		} else {
			put_token(fp, tran_list[i]->get_reference());
		}
	}
	if(img_info) {
		put_token(fp, "\nIMGINFO");
		if(expand || !(img_info->get_reference())) {
			put_token(fp, "{");
			img_info->parse_out(fp, expand);
			put_token(fp, "}");
		} else {
			put_token(fp, img_info->get_reference());
		}
	}
	put_token(fp, "\nEND_ANIMATION");
	return(TRUE);
}

int Animation::get_frame_number(double t)
{
	int	fn;

	if(t > end_time) t = end_time;
	if(t < start_time) t = start_time;
	fn = (int)((t - start_time + delta_time/2.0) / delta_time);
	return(fn);
}

void Animation::add_transition(Transition *tran)
{
	int	i;

	if(tran_list) {
		tran_count++;
		tran_list = (Transition **)realloc((void *)tran_list, (tran_count+1) * sizeof(Transition *));
		tran_list[tran_count] = NULL;
		for(i=tran_count-1; i>0; i--) {
			if(tran_list[i-1]->get_start_time() > tran->get_start_time()) {
				tran_list[i] = tran_list[i-1];
			} else {
				break;
			}
		}
		tran_list[i] = tran;
	} else {
		tran_count = 1;
		tran_list = (Transition **)malloc((tran_count+1) * sizeof(Transition *));
		tran_list[0] = tran;
		tran_list[1] = NULL;
	}
	// should there be set_renderer routines in transition::parse_in?
	if(!(tran->get_rndr()))tran->set_rndr(new MetaRenderer());
}

void Animation::render(void)
{
	double		curr_time;
	Transition	**temp_tran;
	Transition	*curr_tran;
	int		i;

	curr_time = start_time + frame_number * delta_time;
	// initialize img_info & generate default (black?) frame
	img_info->prepare();
	for(i=0; i<img_info->get_num_images(); i++) {
		img_info->get_image(i)->clear();
	}
	// search thru tran list for applicable trans & process applicable trans
	if(tran_list) {
		temp_tran = tran_list;
		for(curr_tran = *temp_tran;  curr_tran;  curr_tran = *(++temp_tran)) {
			if(curr_time >= curr_tran->get_start_time() &&
			   curr_time <= curr_tran->get_end_time()) {
				// have tran generate image and combine with current
				curr_tran->render(img_info);
			}
		}
	}
	img_info->dispose(frame_number);
}

void Animation::animate(void)
{
	double	curr_time;

	// make sure initialization is done
	if(!img_info) {
		fprintf(stderr," Whoops - Unable to animate until image info is set.\n");
		return;
	}
	// then render the frames
	for(curr_time = start_time; curr_time <= end_time; curr_time += delta_time) {
		set_frame_number(curr_time);
		if(notify)fprintf(stderr,"Rendering animation frame %d\n", frame_number);
		set_time(curr_time);
		render();
	}
}
