// img_info.C

#include "grape/object.h"
#include "grape/img_info.h"
#ifdef DO_GL
#include <gl/gl.h>
#endif

#define RGB_BLACK 0x000000

char	*output_method_master_list[NUM_METHODS] =
	{
		 "OUT_NONE" ,
		 "OUT_FILL_IMG" ,
		 "OUT_FILE" ,
		 "OUT_XWIN" ,
		 "OUT_GLWIN" 
	};

int	output_method_from_type(char *type)
{
	int	i;

	for(i=0; i<NUM_METHODS; i++) {
		if(!strcmp(output_method_master_list[i], type)) {
			return(i);
		}
	}
	return(-1);
}

void    ImgInfo::prepare(void) // open unopened windows, etc.
{
	int	windowx, windowy, wbands, zband;
#ifdef DO_GL
	double	near, far;
#endif


	if(get_output_method() == OUT_XWIN && !win) {
		if(image_flag || alpha_flag) {
                	get_res(&windowx, &windowy);
			if(!img) {
				add_image(new Image());
			} 
			if(!(img[0]->get_data())) {
				img[0]->set_data(new ucharData());
			}
			img[0]->get_data()->get_res(&windowx, &windowy, &wbands);
			if(windowx <= 0 || windowy <= 0 || wbands <= 0) {
				if(alpha_flag) {
					img[0]->get_data()->allocate(xres, yres, bands+1);
					img[0]->get_data()->set_alpha_band(bands);
				} else {
					img[0]->get_data()->allocate(xres, yres, bands);
				}
			}
		}
/*
		XVisualInfo     vinfo;  // contains pointer to visual
		Display         *disp;
		disp = XOpenDisplay(NULL);
		if(XMatchVisualInfo(disp,DefaultScreen(disp),24,TrueColor,&vinfo)) {
			ImageDisp *tdisp;
			tdisp = new ImageDisp();
			tdisp->setup(disp, vinfo.visual);
			tdisp->set_image(img[0]);
			img[0]->set_disp(tdisp);
		}
*/
		if(z_flag) {
                	get_res(&windowx, &windowy);
			if(!img) {
				add_image(new Image());
				zband = 0;
			} else if(!(image_flag || alpha_flag)) {
				zband = 0;
			} else if(num_images < 2) {
				add_image(new Image());
				zband = 1;
			} else {
				zband = 1;
			} 
			if(!(img[zband]->get_data())) {
				img[zband]->set_data(new floatData());
			}
			img[zband]->get_data()->get_res(&windowx, &windowy, &wbands);
			if(windowx <= 0 || windowy <= 0 || wbands <= 0) {
				img[zband]->get_data()->allocate(xres, yres, 1);
			}
		}
#ifdef DO_GL
	} else if(get_output_method() == OUT_GLWIN && !gl_win) {
		GL_WIN	temp_gl_win;

		temp_gl_win = winget();
		winset(temp_gl_win);
fprintf(stderr,"Creating gl window in img_info because method = %d and windowid = %d.\n", get_output_method() , get_GLwindow());
                near = 1.0;
                far = getgdesc(GD_ZMAX);

                get_res(&windowx, &windowy);

                prefsize(windowx,windowy);     /* the size of the window */
                foreground();      /* keep GL from exiting this process      */
                glcompat(GLC_ZRANGEMAP,1);
                gl_win = winopen("GL Renderer");     /* Give title of window */
                subpixel(TRUE);    /* added for subpixel accuracy. So polys don't wabble*/
                RGBmode();         /* Set to RGB mode instead of default Color Map mode */
                doublebuffer();    /* we will double buffer for smooth animations */
                gconfig();         /* Let system know of change */
                zbuffer(1);        /* enable ZBuffer */
                lsetdepth((long)near, far); /* set clip planes to min,max depths */
                mmode(MVIEWING);   /* a matrix mode */
                cpack(RGB_BLACK);  /* clear both buffers. */
                czclear(0,far);
                swapbuffers();
                czclear(0,far);

                viewport(0,windowx-1,0,windowy-1);
                window(-(windowx-1)/2.0, (windowx-1)/2.0,
                       -(windowy-1)/2.0, (windowy-1)/2.0, near, far);

                czclear(0,far);    /* clear the Z and image buffers */
#endif
	} else if(get_output_method() == OUT_FILL_IMG) {
		if(image_flag || alpha_flag) {
                	get_res(&windowx, &windowy);
			if(!img) {
				add_image(new Image());
			} 
			if(!(img[0]->get_data())) {
				img[0]->set_data(new ucharData());
			}
			img[0]->get_data()->get_res(&windowx, &windowy, &wbands);
			if(windowx <= 0 || windowy <= 0 || wbands <= 0) {
				if(alpha_flag) {
					img[0]->get_data()->allocate(xres, yres, bands+1);
					img[0]->get_data()->set_alpha_band(bands);
				} else {
					img[0]->get_data()->allocate(xres, yres, bands);
				}
			}
		}
		if(z_flag) {
                	get_res(&windowx, &windowy);
			if(!img) {
				add_image(new Image());
				zband = 0;
			} else if(!(image_flag || alpha_flag)) {
				zband = 0;
			} else if(num_images < 2) {
				add_image(new Image());
				zband = 1;
			} else {
				zband = 1;
			} 
			if(!(img[zband]->get_data())) {
				img[zband]->set_data(new floatData());
			}
			img[zband]->get_data()->get_res(&windowx, &windowy, &wbands);
			if(windowx <= 0 || windowy <= 0 || wbands <= 0) {
				img[zband]->get_data()->allocate(xres, yres, 1);
			}
		}
	} else if(get_output_method() == OUT_FILE && filename) {
		if(image_flag || alpha_flag) {
                	get_res(&windowx, &windowy);
			if(!img) {
				add_image(new Image());
			} 
			if(!(img[0]->get_data())) {
				img[0]->set_data(new ucharData());
			}
			img[0]->get_data()->get_res(&windowx, &windowy, &wbands);
			if(windowx <= 0 || windowy <= 0 || wbands <= 0) {
				if(alpha_flag) {
					img[0]->get_data()->allocate(xres, yres, bands+1);
					img[0]->get_data()->set_alpha_band(bands);
				} else {
					img[0]->get_data()->allocate(xres, yres, bands);
				}
			}
		}
		img[0]->set_file(new_file_by_type(file_type));
		if(z_flag) {
                	get_res(&windowx, &windowy);
			if(!img) {
				add_image(new Image());
				zband = 0;
			} else if(!(image_flag || alpha_flag)) {
				zband = 0;
			} else if(num_images < 2) {
				add_image(new Image());
				zband = 1;
			} else {
				zband = 1;
			} 
			if(!(img[zband]->get_data())) {
				img[zband]->set_data(new floatData());
			}
			img[zband]->get_data()->get_res(&windowx, &windowy, &wbands);
			if(windowx <= 0 || windowy <= 0 || wbands <= 0) {
				img[zband]->get_data()->allocate(xres, yres, 1);
			}
		}
	} else {
	}
}

void	ImgInfo::dispose(int fnum)
{
	char	fname[4096];
        int outnum = (output_filenum >= 0) ? output_filenum : fnum;

// fprintf(stderr,"Output method=%d in dispose\n", get_output_method());
	if(get_output_method() == OUT_XWIN && win) {
		// tell to redisplay
	} else if(get_output_method() == OUT_GLWIN && gl_win) {
		// who knows
	} else if(get_output_method() == OUT_FILL_IMG && img) {
		if (filename) {
			sprintf(fname, filename, outnum);
			img[0]->write(fname, file_type);
		}
	} else if(get_output_method() == OUT_FILE && filename) {
		// write image to file
		sprintf(fname, filename, outnum);
// fprintf(stderr,"Writing image to file %s\n", fname);
		img[0]->write(fname, file_type);
	} else {
		// complain?
	}
}

void	ImgInfo::get_res(int *x, int *y, int *b)
{
#ifdef DO_GL
	GL_WIN	temp_gl_win;
	long	lx, ly;
#endif
	int	lb;

	if(get_output_method() == OUT_XWIN && win) {
		*x=xres;
		*y=yres;
		if(b) *b=bands;
#ifdef DO_GL
	} else if(get_output_method() == OUT_GLWIN && gl_win) {
		temp_gl_win = winget();
		winset(gl_win);
		getsize(&lx, &ly);
		*x = lx;
		*y = ly;
		if(b) *b = 3;
		winset(temp_gl_win);
#endif
	} else if(get_output_method() == OUT_FILL_IMG && img) {
		*x=xres;
		*y=yres;
		if(b) *b=bands;
		if(img[0]) {
			img[0]->get_res(x, y, &lb);
			if(*x <= 0 || *y <= 0 || lb<= 0) {
				*x=xres;
				*y=yres;
				if(b) *b=bands;
			}
		}
	} else if(get_output_method() == OUT_FILE && filename) {
		*x=xres;
		*y=yres;
		if(b) *b=bands;
	} else {
		*x=xres;
		*y=yres;
		if(b) *b=bands;
	}
}


ImgInfo	*create_img_info(Dataport *fp)
{
	char	token[4096];

	if(!get_next_token(fp, token)) {
		fprintf(stderr," Whoops - Unexpected EOF in parsing img_info file\n");
		return(FALSE);
	}
	if(!strcmp(token,"IMGINFO_V1")) {
		return(new ImgInfo());
	}
	return(NULL);
}


int	ImgInfo::parse_in(Dataport *fp)
{
	char	token[4096];

	do {
		if(!get_next_token(fp, token)) {
			fprintf(stderr," Whoops - Unexpected EOF in image info file\n");
			return(FALSE);
		}
		if(!strcmp(token, "VERSION")) {
			if(!get_next_token(fp, token)) {
				fprintf(stderr," Whoops - Unexpected EOF in image info file\n");
				return(FALSE);
			}
			if(strcmp(token, "1.0")) {
				fprintf(stderr," Whoops - Unexpected version number encountered in parsing image info object\n");
				fprintf(stderr,"   Token=>%s<  Expecting >1.0<\n", token);
				return(FALSE);
			}
			version = strdup(token);
		} else if(!strcmp(token, "RESOLUTION")) {
			if(!get_next_token(fp, token)) {
				fprintf(stderr," Whoops - Unexpected EOF in image info file\n");
				return(FALSE);
			}
			xres = atoi(token);
			if(!get_next_token(fp, token)) {
				fprintf(stderr," Whoops - Unexpected EOF in image info file\n");
				return(FALSE);
			}
			yres = atoi(token);
			if(!get_next_token(fp, token)) {
				fprintf(stderr," Whoops - Unexpected EOF in image info file\n");
				return(FALSE);
			}
			bands = atoi(token);
		} else if(!strcmp(token, "OUTPUT_METHOD")) {
			if(!get_next_token(fp, token)) {
				fprintf(stderr," Whoops - Unexpected EOF in image info file\n");
				return(FALSE);
			}
			output_method = output_method_from_type(token);
			if(output_method == OUT_FILE) {
				if(!get_next_token(fp, token)) {
					fprintf(stderr," Whoops - Unexpected EOF in image info file\n");
					return(FALSE);
				}
				if(filename)free(filename);
				filename = strdup(token);
				if(!get_next_token(fp, token)) {
					fprintf(stderr," Whoops - Unexpected EOF in image info file\n");
					return(FALSE);
				}
				file_type = atoi(token);
			}
		} else if(!strcmp(token, "ALPHA_RENDER")) {
			alpha_flag = TRUE;
		} else if(!strcmp(token, "Z_RENDER")) {
			z_flag = TRUE;
		} else if(!strcmp(token, "IMAGE_RENDER")) {
			image_flag = TRUE;
		} else if(!strcmp(token, "FIELD_RENDER")) {
			if(!get_next_token(fp, token)) {
				fprintf(stderr," Whoops - Unexpected EOF in image info file\n");
				return(FALSE);
			}
			field_render = strcmp(token, "OFF");
		} else if(strcmp(token, "NO_MO_INFO")) {
			fprintf(stderr," Whoops - Unexpected token encountered in parsing image info object\n");
			fprintf(stderr,"   Token=>%s<  Expecting >NO_MO_INFO<\n", token);
			return(FALSE);
		}
	} while (strcmp(token, "NO_MO_INFO"));

	return(TRUE);
	
}

int ImgInfo::parse_out(Dataport *fp, int )
{
	char	token[4096];

	put_token(fp, "IMGINFO_V1");
	put_token(fp, "\nVERSION");
	put_token(fp, "1.0");
	put_token(fp, "\nRESOLUTION");
	sprintf(token, "%d", xres);
	put_token(fp, token);
	sprintf(token, "%d", yres);
	put_token(fp, token);
	sprintf(token, "%d", bands);
	put_token(fp, token);
	put_token(fp, "\nOUTPUT_METHOD");
	put_token(fp, output_method_master_list[output_method]);
	if(output_method == OUT_FILE && filename) {
		put_token(fp, filename);
		sprintf(token, " %d", file_type);
		put_token(fp, token);
	}
	if(image_flag)put_token(fp, "\nIMAGE_RENDER");
	if(alpha_flag)put_token(fp, "\nALPHA_RENDER");
	if(z_flag)put_token(fp, "\nZ_RENDER");
	put_token(fp, "\nFIELD_RENDER");
	if(field_render) {
		put_token(fp, "ON");
	} else {
		put_token(fp, "OFF");
	}

	put_token(fp, "\nNO_MO_INFO\n");

	return(TRUE);
}

void	ImgInfo::copy(ImgInfo *inimg, int winflag)
{
        int x,y,b;

        if(inimg->get_version())version = strdup(inimg->get_version());
//		if(inimg->get_reference())reference = strdup(inimg->get_reference());
        inimg->get_res(&x, &y, &b);
        set_res(x,y,b);
        set_image_flag(inimg->get_image_flag());
        set_alpha_flag(inimg->get_alpha_flag());
        set_z_flag(inimg->get_z_flag());
        set_field_render(inimg->get_field_render());
        output_method = inimg->get_output_method();
        if(inimg->get_filename())filename = strdup(inimg->get_filename());
        file_type = inimg->get_file_type();
        if(winflag) {
                win = inimg->get_xwindow();
                gl_win = inimg->get_GLwindow();
				pXDisplay = inimg->get_xdisplay();
        }
}

ImgInfo	*ImgInfo::duplicate(int winflag)
{
	ImgInfo *temp;

	temp = new ImgInfo();
	temp->copy(this, winflag);
	return(temp);
}

