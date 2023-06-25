// image.h
//
// Written by Dave Kagels 10/3/94

#ifndef _IMAGE_H_
#define _IMAGE_H_


#include "image/imageclasstypes.h"
#include "image/imagedata.h"
#include "image/imagefile.h"
#include "image/imagedisp.h"



// This class combines three aspects of images: data access (in memory),
// data storage on disk, and data display (on a workstation monitor).
// It contains pointers to three objects, one for each aspect.
// Many member functions are included to operate on those objects.

class Image {

    protected:

	ImageData	*data;	    // Pointer to image data object
	ImageFile	*file;	    // Pointer to image file storage object
	ImageDisp	*disp;	    // Pointer to image display interface object

	// Initialize member variables:
	void	init() { data=NULL; file=NULL; disp=NULL; clean=True; }

    public:

	int		clean;	    // Flag for deleting objects in destructor

	virtual	int	image_class_type(void) { return(BASEIMAGE); }	// Base class type

	// Return the pointers to the objects:
	ImageData	*get_data() { return data; }
	ImageFile	*get_file() { return file; }
	ImageDisp	*get_disp() { return disp; }

	// Set the pointers to the objects, and set back-pointers if appropriate:
	void	set_data(ImageData *d) { data=d; }
	void	set_file(ImageFile *f) { file=f; if(f) f->set_image(this); }
	void	set_disp(ImageDisp *id) { disp=id; if(id) id->set_image(this); }

	// Delete the objects:
	void	free_data() { if(data) delete data; data=NULL; }
	void	free_file() { if(file) delete file; file=NULL; }
	void	free_disp() { if(disp) delete disp; disp=NULL; }

	// Typecast operators to return pointers to the objects:
	operator ImageData*() { return data; }
	operator ImageFile*() { return file; }
	operator ImageDisp*() { return disp; }

	// Overloaded assignment operators to set the pointers to the objects:
	void	operator=(ImageData *d) { set_data(d); }
	void	operator=(ImageFile *f) { set_file(f); }
	void	operator=(ImageDisp *id) { set_disp(id); }


	// ImageData object operations: (See ImageData class)
	ImageData	&operator()(int x, int y=0, int b=DEFAULT_BAND) {
				data->cx=x; data->cy=y; data->cband=b;
				return *data; }
	int	allocate(int x=0, int y=1, int b=1) { if(!data) return 0;
					else return data->allocate(x,y,b); }
	void	free() { if(data) data->free(); }
	int	clear(int b=ALL_BANDS) { if(!data) return 0;
					 else return data->clear(b); }
	// made get_res virtual so derived classes may do other things
	virtual int	get_res(int *x=NULL, int *y=NULL, int *b=NULL) {
				if(data) return data->get_res(x,y,b);
				else if(file) return file->get_res(x,y,b);
				else return -1;
		}
	int	get_bands() { if(!data) return -1; else return data->get_bands(); }
	void	set_default_band(int b=0) { if(data) data->set_default_band(b); }
	void	set_red_band(int b=0) { if(data) data->set_red_band(b); }
	void	set_green_band(int b=1) { if(data) data->set_green_band(b); }
	void	set_blue_band(int b=2) { if(data) data->set_blue_band(b); }
	void	set_alpha_band(int b=-1) { if(data) data->set_alpha_band(b); }
	void	set_default_alpha(uchar a=255) { if(data) data->set_default_alpha(a); }
	void	set_rgb_bands(int r=0, int g=1, int b=2) {
				if(data) data->set_rgb_bands(r,g,b); }
	int	get_default_band() { if(!data) return -1;
					else return data->get_default_band(); }
	int	get_red_band() { if(!data) return -1;
					else return data->get_red_band(); }
	int	get_green_band() { if(!data) return -1;
					else return data->get_green_band(); }
	int	get_blue_band() { if(!data) return -1;
					else return data->get_blue_band(); }
	int	get_alpha_band() { if(!data) return -1;
					else return data->get_alpha_band(); }
	uchar	get_default_alpha() { if(!data) return 0;
					else return data->get_default_alpha(); }
	void	set_level(double l) { if(data) data->set_level(l); }
	void	set_spacing(double s) { if(data) data->set_spacing(s); }
	double	get_level() { if(!data) return 0.0;
				else return data->get_level(); }
	Image	*get_map() { if(!data) return NULL; else return data->get_map(); }
	void	set_map(Image *m) { if(data) data->set_map(m); }
	int	access() { if(!data) return 0; else return data->access(); }
	int	get_data_type() { if(!data) return 0; else return data->type(); }
	char	*data_type_name() { if(!data) return (char *)"None";
				    else return data->type_name(); }
	int	preferred_data_type() { if(!data) return NO_DATA;
					else return data->preferred_data_type(); }
	int	preferred_file_type() { if(!data) return 0;
					else return data->preferred_file_type(); }
	void	get_color(int x, int y, uchar *r, uchar *g, uchar *b,
				  uchar *a=NULL) { data->get_color(x,y,r,g,b,a); }
	void    get_color_row(int x, int y, int length, uchar *r, uchar *g, uchar *b,
	                         uchar *a=NULL) { data->get_color_row(x,y,length,r,g,b,a); }
	void	set_color(int x, int y, uchar r, uchar g, uchar b,
				  uchar a=255) { data->set_color(x,y,r,g,b,a); }
	uchar	get_alpha(int x, int y) { return data->get_alpha(x,y); }
	void	set_alpha(int x, int y, uchar a=255) { data->set_alpha(x,y,a); }
	ushort	get_red16(int x, int y=0) { return data->get_red16(x,y); }
	ushort	get_green16(int x, int y=0) { return data->get_green16(x,y); }
	ushort	get_blue16(int x, int y=0) { return data->get_blue16(x,y); }
	void	set_red16(ushort c, int x, int y=0) { data->set_red16(c,x,y); }
	void	set_green16(ushort c, int x, int y=0) { data->set_green16(c,x,y); }
	void	set_blue16(ushort c, int x, int y=0) { data->set_blue16(c,x,y); }

	// Create a new ImageData object by type number, return status:
	int	create_data_type(int t) { if(clean) free_data();
			set_data(new_data_by_type(t));
			if(data) return 1; else return 0; }

	// ImageFile object operations: (See ImageFile class)
	void	set_filename(char *fname) { if(file) file->set_filename(fname); }
	int	get_file_type() { if(!file) return 0; else return file->type(); }
	char	*file_type_name() { if(!file) return (char *)"None";
				    else return file->type_name(); }
	char	*get_filename(char *fname=NULL) { if(!file) return NULL;
					else return file->get_filename(fname); }
	char	*get_comments() { if(!file) return NULL;
				  else return file->get_comments(); }
	char	*get_flags() { if(!file) return NULL;
			       else return file->get_flags(); }
	int	set_comments(char *str=NULL) { if(!file) return 0;
					else return file->set_comments(str); }
	int	set_flags(char *str=NULL) { if(!file) return 0;
					else return file->set_flags(str); }

	// Read file fname (of type t) from disk, return status:
	// This is for quickly reading a disk file to get
	// info about it without reading the entire image
	// No ImageData is created
	// Corresponding ImageFile functions must be created
	int	read_header(char *fname=NULL, int t=0);

	// Read file fname (of type t) from disk, return status:
	int	read(char *fname=NULL, int t=0);

	// Write image to file fname (of type t), return status:
	int	write(char *fname=NULL, int t=0);

	// Find the file type of file fname, but do not load, return status:
	int	find_file_type(char *fname);

	// Create an ImageFile object of type t (with name fname), return status:
	int	create_file_type(int t=0, char *fname=NULL);


	// ImageDisp operations: (See ImageDisp class)
#ifndef _NO_IMAGEDISP_
	Display		*get_display() { if(!disp) return NULL;
					 else return disp->get_display(); }
	Visual		*get_visual() { if(!disp) return NULL;
					else return disp->get_visual(); }
	Colormap	get_colormap() { if(!disp) return 0;
					 else return disp->get_colormap(); }
#endif // _NO_IMAGEDISP_
	int		get_depth() { if(!disp) return 0;
					 else return disp->get_depth(); }
#ifndef _NO_IMAGEDISP_
	void	set_display(Display *d) { if(disp) disp->set_display(d); }
	void	set_visual(Visual *v) { if(disp) disp->set_visual(v); }
	void	set_colormap(Colormap c) { if(disp) disp->set_colormap(c); }
	int	set_window_colormap(Window w=0) { if(!disp) return 0;
				else return disp->set_window_colormap(w); }
	int	disp_initialize() { if(!disp) return 0;
				    else return disp->initialize(); }
	int	disp_setup(Display *d, Visual *v=NULL, Colormap c=0) {
				if(!disp) return create_disp(d,v,c);
				else return disp->setup(d,v,c); }
	int	disp_setup(Display *d, Colormap c) {
				if(!disp) return create_disp(d,c);
				else return disp->setup(d,c); }
	Pixel	pixel(int x, int y=0) { if(!disp) return 0;
					else return disp->pixel(x,y); }
	int	fill_ximage(XImage *ximg, int xx=0, int xy=0, int ix=0, int iy=0,
			    int w=0, int h=0) { if(!disp) return 0;
			else return disp->fill_ximage(ximg,xx,xy,ix,iy,w,h); }
	XImage	*create_ximage(int ix=0, int iy=0, int w=0, int h=0) {
				if(!disp) return NULL;
				else return disp->create_ximage(ix,iy,w,h); }
	int	fill_drawable(Drawable draw=0, int dx=0, int dy=0, int ix=0,
			      int iy=0, int w=0, int h=0) { if(!disp) return 0;
			else return disp->fill_drawable(draw,dx,dy,ix,iy,w,h); }
	Pixmap	create_pixmap(Drawable draw=0, int ix=0, int iy=0, int w=0, int h=0) {
				if(!disp) return (Pixmap)0;
				else return disp->create_pixmap(draw,ix,iy,w,h); }

	// Create an appropriate ImageDisp object given the display, and
	// optional visual and/or colormap, return status:
	int	create_disp(Display *d, Visual *v=NULL, Colormap c=0);
	int	create_disp(Display *d, Colormap c) {
				return create_disp(d,(Visual *)NULL,c); }

	// Same as above, but do not initialize:
	int	create_disp_noinit(Display *d, Visual *v=NULL, Colormap c=0);
	int	create_disp_noinit(Display *d, Colormap c) {
				return create_disp_noinit(d,(Visual *)NULL,c); }
#endif // _NO_IMAGEDISP_

	// Constructors:
	Image() { init(); }
	Image(char *fname, int t=0) { init(); read(fname,t); }
#ifndef _NO_IMAGEDISP_
	Image(char *fname, Display *d) { init(); read(fname); create_disp(d); }
#endif // _NO_IMAGEDISP_
	Image(ImageData *d, ImageFile *f=NULL, ImageDisp *id=NULL) { init();
		set_data(d); set_file(f); set_disp(id); }
	Image(ImageFile *f, ImageDisp *id=NULL) { init(); set_file(f);
		set_disp(id); }
	Image(ImageData *d, ImageDisp *id) { init(); set_data(d);
		set_disp(id); }
	Image(ImageDisp *id) { init(); set_disp(id); }

	virtual ~Image() { if(clean) { free_data(); free_file(); free_disp(); } }
	};


// Global flag for creating DismMap object if image has a colormap:
extern	int	DISP_USE_MAP;

#endif // _IMAGE_H_
