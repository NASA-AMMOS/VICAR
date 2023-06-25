// img_info.h

#ifndef	_IMG_INFO_H_
#define _IMG_INFO_H_

#include <stdlib.h>

#include "dataport.h"

#include "image/image.h"
#include "image/datatypes.h"
#include "image/filetypes.h"
#ifdef _NO_IMAGEDISP_
#include <X11/X.h>
#include <X11/Xlib.h>
#include <X11/Intrinsic.h>
#endif

#define GL_WIN	long

// Output methods:

#define	NUM_METHODS		5

#define	OUT_NONE	0
#define	OUT_FILL_IMG	1
#define	OUT_FILE	2
#define	OUT_XWIN	3
#define	OUT_GLWIN	4

extern	char	*output_method_master_list[NUM_METHODS];


// Class for storing output image information for rendering
// (could be written to handle multiple types at once)

class ImgInfo {

    private:

    protected:

	char	*version;
	char	*reference;

	int	xres,yres,bands;
	int	image_flag;
	int	alpha_flag;
	int	z_flag;

	int	field_render;


	int	output_method;		// 1=fill image, 2=file, 3=X window
					// 4=GL window
	Image	**img;
	int	num_images;

	char	*filename;
	int	file_type;

	int     output_filenum;	  // if non-negative, dispose will use this

	Window	win;				/* X window ID, if any */
	Display *pXDisplay;			/* ptr to X display, if any */
	GL_WIN	gl_win;

	void	init() { xres=0; yres=0; bands=0; output_method=0;version=NULL;
			img=NULL; num_images=0; win=0;
			filename=NULL; output_filenum = -1; file_type=0; 
			gl_win=0; reference = NULL; alpha_flag = FALSE; 
			image_flag = FALSE; z_flag = FALSE; field_render=FALSE;
			pXDisplay = NULL;
		       }

    public:

	void	copy(ImgInfo *inimg, int winflag=FALSE);

	ImgInfo	*duplicate(int winflag=FALSE);

	void	prepare(void);
	void	dispose(int fnum=0);

	void	set_res(int x=0, int y=0, int b=3) { xres=x; yres=y; bands=b; }

	void	get_res(int *x, int *y, int *b=NULL);

	void	add_image(Image *i) { 
		img = (Image **)realloc((void *)img, ++num_images * 
							sizeof(Image *));
		img[num_images-1] = i; 
	}

	void	set_image_fill(void) { output_method=OUT_FILL_IMG; }

	void	set_file_name(char *name, int type=0) {
			if(filename)free(filename);
			filename=strdup(name);
			file_type=type; }


	void	set_file(char *name, int type=0) { output_method=OUT_FILE;
			if(filename)free(filename);
			filename=strdup(name);
			file_type=type; }

	void    set_file_type(int type) { file_type = type; }

	void    set_output_filenum(int num) { output_filenum = num; }
	

	void	set_xwindow(Window w) { output_method=OUT_XWIN; win=w; }

	void	set_GLwindow(GL_WIN w) { output_method=OUT_GLWIN; gl_win=w; }

	int	get_output_method() { return output_method; }
	void	set_output_method(int mth) { output_method=mth; }

	Image	**get_image_list() { return img; }
	Image	*get_image(int idx) { if(img) return(img[idx]); else return(NULL); }
	int	get_num_images(void) { return (num_images); }

	char	*get_filename() { return filename; }

	int	get_file_type() { return file_type; }

	Window	get_xwindow() { return win; }

	GL_WIN	get_GLwindow() { return gl_win; }

	void set_xdisplay( Display *p) { pXDisplay = p; }
	Display *get_xdisplay( void) { return pXDisplay; }
	
	void	set_image_flag(int f) { image_flag=f; }
	int	get_image_flag() { return image_flag; }

	void	set_alpha_flag(int f) { alpha_flag=f; }
	int	get_alpha_flag() { return alpha_flag; }

	void	set_z_flag(int f) { z_flag=f; }
	int	get_z_flag() { return z_flag; }

	void	set_field_render(int f) { field_render=f; }
	int	get_field_render() { return field_render; }

	void	set_reference(char *ref) {
		if(reference)free(reference);
		reference = strdup(ref);
	}
	char	*get_reference(void) { return(reference); }

	char	*get_version(void) { return(version); }

	virtual	int	parse_in(Dataport *fp);
	virtual	int	parse_out(Dataport *fp, int expand=FALSE);

	ImgInfo() { init(); }
	};

ImgInfo *create_img_info(Dataport *fp);

#endif
