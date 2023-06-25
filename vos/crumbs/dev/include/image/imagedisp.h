// imagedisp.h
//
// Written by Dave Kagels 10/11/94

#ifndef _IMAGEDISP_H_
#define _IMAGEDISP_H_

#include <stdio.h>
#ifndef _NO_IMAGEDISP_
#include <X11/X.h>
#include <X11/Xlib.h>
#include <X11/Intrinsic.h>
#else
#define True 1
#define False 0
#endif // _NO_IMAGEDISP_
#include "image/imagedata.h"

//extern class	Image;
class	Image;	// removed extern to conform to new compiler std for forward declarations


// Flags to control private colormap creation (PseudoColor visuals only)
#define	PRIVATE_NEVER		0
#define	PRIVATE_IF_NEEDED	1
#define PRIVATE_ALWAYS		2


// This class is the base class for interfacing with workstation displays.
// It has a pointer to the image, and information about the display.
// It can use a pre-defined colormap, or make one automatically.
// Member functions are provided to return a Pixel (colormap index) value
// at any x,y location in the image, with versions for bilinear interpolation
// and dithering.  The Pixel value can be used to set the current drawing
// color in X, or placed in an XImage, which can be copied to the screen.
// Derived classes can be made to handle pseudocolor images, dithering, etc.

class ImageDisp {

    protected:

	Image		*img;			// pointer to image
#ifndef _NO_IMAGEDISP_
	Display		*display;		// display pointer for X
	Visual		*visual;		// visual pointer for X
	Colormap	colormap;		// colormap ID for X
	Window		window;			// for screen info
#endif // _NO_IMAGEDISP_
	int		depth;			// depth of colormap

	void	init();				// initialize member variables

    public:

	int	create_private;			// flag for colormap creation
	int	interpolate;			// flag for using interpolation

	// Functions to get information:
	Image		*get_image() { return img; }
#ifndef _NO_IMAGEDISP_
	Display 	*get_display() { return display; }
	Visual		*get_visual() { return visual; }
	Colormap	get_colormap() { return colormap; }
	Window		get_window() { return window; }
#endif // _NO_IMAGEDISP_
	int		get_depth() { return depth; }

	// Functions to set information:
	virtual	void	set_image(Image *i) { img=i; }
#ifndef _NO_IMAGEDISP_
	virtual	void	set_display(Display *d) { display=d; }
	virtual	void	set_visual(Visual *v) { visual=v; }
	virtual	void	set_colormap(Colormap c) { colormap=c; }
	virtual	void	set_window(Window w) { window=w; }
#endif // _NO_IMAGEDISP_

	// Typecast operators to get information:
	operator Image*() { return get_image(); }
#ifndef _NO_IMAGEDISP_
	operator Display *() { return get_display(); }
	operator Visual*() { return get_visual(); }
	operator Colormap() { return get_colormap(); }
#endif // _NO_IMAGEDISP_

	// Overloaded assignment operators to set information:
	void	operator=(Image *i) { set_image(i); }
#ifndef _NO_IMAGEDISP_
	void	operator=(Display *d) { set_display(d); }
	void	operator=(Visual *v) { set_visual(v); }
	void	operator=(Colormap c) { set_colormap(c); }
#endif // _NO_IMAGEDISP_

	// Initialize the necessary information for drawing images:
	virtual	int	initialize() { return 0; }

	// Functions to set the information and perform initialization:
#ifndef _NO_IMAGEDISP_
	int	setup(Display *d, Visual *v=NULL, Colormap c=0) {
			set_display(d);
			if(v) set_visual(v);
			if(c) set_colormap(c);
			return initialize(); }
	int	setup(Display *d, Colormap c) {
			set_display(d); set_colormap(c);
			return initialize(); }

	// Set the colormap into a window:
	virtual	int	set_window_colormap(Window w=0);

	// Functions to get a Pixel (colormap index) value at x,y:
	virtual	Pixel	pixel(int x, int y=0)=0;
	virtual	Pixel	ipixel(double x, double y=0.0)=0;

	// Functions to get a Pixel (colormap index) value at x,y with dithering:
	virtual	Pixel	dpixel(int x, int y=0, int sx=0, int sy=0);
	virtual	Pixel	idpixel(double x, double y=0.0, int sx=0, int sy=0);

	// Functions to get an area of pixels: (for XImage objects)
	virtual	void	get_area8(uchar *buf, int ix, int iy, int w, int h=1, int ppl=0);
	virtual	void	iget_area8(uchar *buf, double x, double y, double dx, double dy, int w, int h=1, int sx=0, int sy=0, int ppl=0);
	virtual	void	get_area32(ulong *buf, int ix, int iy, int w, int h=1, int ppl=0);
	virtual	void	iget_area32(ulong *buf, double x, double y, double dx, double dy, int w, int h=1, int sx=0, int sy=0, int ppl=0);

	// Fill part of an XImage with an area of the image:
	virtual int	fill_ximage(XImage *ximg, int xx=0, int xy=0, int ix=0, int iy=0, int w=0, int h=0);
	virtual int	ifill_ximage(XImage *ximg, double x=0.0, double y=0.0, double dx=1.0, double dy=1.0, int xx=0, int xy=0, int w=0, int h=0, int sx=0, int sy=0);

	// Create an XImage from part of the image:
	virtual	XImage	*create_ximage(int ix=0, int iy=0, int w=0, int h=0);
	virtual	XImage	*icreate_ximage(double x=0.0, double y=0.0, double dx=1.0, double dy=1.0, int w=0, int h=0, int sx=0, int sy=0);

	// Fill part of a drawable (window or pixmap) with an area of the image:
	virtual int	fill_drawable(Drawable draw=0, int dx=0, int dy=0, int ix=0, int iy=0, int w=0, int h=0);
	virtual int	ifill_drawable(Drawable draw=0, double x=0.0, double y=0.0, double dx=1.0, double dy=1.0, int wx=0, int wy=0, int w=0, int h=0, int sx=0, int sy=0);

	// Create a pixmap from part of the image:
	virtual Pixmap	create_pixmap(Drawable draw=0, int ix=0, int iy=0, int w=0, int h=0);
	virtual Pixmap	icreate_pixmap(Drawable draw=0, double x=0.0, double y=0.0, double dx=1.0, double dy=1.0, int w=0, int h=0, int sx=0, int sy=0);
#endif // _NO_IMAGEDISP_

	// Constructor:
	ImageDisp() { init(); }
	virtual ~ImageDisp() { ; }
	};


// Colormap index reference count functions:

#ifndef _NO_IMAGEDISP_
void RFreeColor(Display *disp, Colormap cmap, Pixel pix);
void RFreeColors(Display *disp, Colormap cmap, Pixel *pix, int npix);
int RAllocColor(Display *disp, Colormap cmap, XColor *xcol);
int RAllocColor(Display *disp, Colormap cmap, Pixel *pix,
	ushort red, ushort green, ushort blue);
int RAllocColor(Display *disp, Colormap cmap, Pixel *pix,
	int red, int green, int blue);
int RAllocColor(Display *disp, Colormap cmap, Pixel *pix,
	double red, double green, double blue);
#endif // _NO_IMAGEDISP_

#endif // _IMAGEDISP_H_
