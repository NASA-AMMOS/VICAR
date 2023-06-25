// imagedata.h 1.16 03/05/09 15:16:38
//
// Written by Dave Kagels 10/3/94

#ifndef _IMAGEDATA_H_
#define _IMAGEDATA_H_

#include <stdio.h>
#include <math.h>
#include <string>
#include <complex>
#define COMPLEX_TYPE    complex<double>

//#include "machine_setup.h"

// need deprecated non-template complex library?
#ifdef _NON_TEMPLATE_COMPLEX
#include <complex.h>
#else
#include <complex>
using std::complex;
static const COMPLEX_TYPE complex_zero(0,0);
#endif

#include "image/geodata.h"

class Image;
extern int data_type_size[];
extern int data_type_bits[];

typedef	unsigned long	ulong;			// more convenient names
typedef unsigned short	ushort;
typedef unsigned char	uchar;

#define	DEFAULT_BAND	-1			// image band indirection
#define	RED_BAND	-2
#define	GREEN_BAND	-3
#define	BLUE_BAND	-4
#define	ALPHA_BAND	-5
#define	LAST_BAND	-6
#define ALL_BANDS	-7

#define	DATA_READ	1			// read/write status bits
#define	DATA_WRITE	2

#define	NO_DATA		0			// data types
#define	BIT_DATA	1
#define	CHAR_DATA	2
#define UCHAR_DATA	3
#define SHORT_DATA	4
#define USHORT_DATA	5
#define LONG_DATA	6
#define ULONG_DATA	7
#define FLOAT_DATA	8
#define DOUBLE_DATA	9
#define COMPLEX_DATA	10
#define	RLE_LONG_DATA	50

#define XIMAGE_DATA 1000


typedef enum {
	IMAGE_WRAP_STATE_UNINIT = -1,
	IMAGE_WRAP_STATE_NONE = 0,
	IMAGE_WRAP_STATE_X,
	IMAGE_WRAP_STATE_Y,
	IMAGE_WRAP_STATE_XY,

    IMAGE_WRAP_STATE_NUMBER // must be last!
} Image_Wrap_State_Type;


// This class is the base class for accessing and storing image data.
// The image can consist of any number of bands (planes) of two dimensional
// (or one dimensional) data in any type.  The data can be accessed using
// any of the eight standard types: char, unsigned char (uchar), short,
// unsigned short (ushort), long, unsigned long (ulong), float, or double,
// and also as single bit values or complex (double precision) values.
// Member functions are provided for allocating the data at a particular
// resolution, adding or removing bands, freeing the data, clearing it, etc.
// The data may have a color map, which is actually another image that is
// assumed to be one dimensional (number of colors by one resolution).
// To allow for color display, the color components red, green, and blue
// may be assigned to any of the bands.  A band for alpha can also be included.
// Member functions are provided for accessing all these bands at once for
// efficiency.  For allocation colors using 16 bit-per-component XColor
// structures, special member functions are provided.
// There are also functions for accessing the data using bilinear interpolation.


class ImageData {

    protected:

	int	xres,yres;			// resolution (size) of image
	int	bands;				// number of image data bands
	int	levels;				// number of pyramid levels

	int	default_band;			// if no band is given
	int	red_band,green_band,blue_band;	// color component bands
	int	alpha_band;			// transparency band
	uchar	default_alpha;			// default transparency value

	double	clev;				// current pyramid level
	double	levmax;				// maximum value for clev

	Image	*map;				// for storing colormap info

	void	init();				// initialize member variables

	// Macro to get a valid band number:
	inline	int	check_band(int &b) const {
				if(b<0) {
					switch(b) {
						case DEFAULT_BAND: b=default_band; break;
						case RED_BAND: b=red_band; break;
						case GREEN_BAND: b=green_band; break;
						case BLUE_BAND: b=blue_band; break;
						case ALPHA_BAND: b=alpha_band; break;
						case LAST_BAND: b=bands-1; break;
						}
					if(b<0) return 1;
					}
				return(b>=bands);
				}

	// Macros to check x,y coordinates against image boundaries:

	inline	int	check_bounds(const int x, const int y) const {
				return(x<0 || x>=xres || y<0 || y>=yres); }

	inline	int	check_bounds_x(const int x) const {
				return(x<0 || x>=xres); }

	inline	int	check_bounds_y(const int y) const {
				return(y<0 || y>=yres); }

	inline	int	check_bounds(const double x, const double y) const {
				return(x<0.0 || x>=(double)xres || 
				       y<0.0 || y>=(double)yres); }

	inline	int	check_bounds_x(const double x) const {
				return(x<0.0 || x>=(double)xres); }

	inline	int	check_bounds_y(const double y) const {
				return(y<0.0 || y>=(double)yres); }


	inline	double	pyr_lev(const double l) const {
				if(l<0.0) return 0.0;
				else if(l>levmax) return levmax;
				else return l; }

	inline int check_bounds_and_apply_wrap(int &x, int &y,
										   uchar *r, uchar *g, uchar *b, uchar *a
										   ) const {
	    if (flgWrapState == IMAGE_WRAP_STATE_NONE) {
			if(check_bounds(x,y)) { 
				*r=0; *g=0; *b=0; 
				if(a) *a=0;
				return 1;
			}
			else {
				return 0;
			}
		}
		else if (flgWrapState == IMAGE_WRAP_STATE_X) {
			if(check_bounds_y(y)) { 
				*r=0; *g=0; *b=0; 
				if(a) *a=0; 
				return 1; 
			}
			else {
				x = apply_wrap_x( x);
				return 0;
			}
		}
		else if (flgWrapState == IMAGE_WRAP_STATE_Y) {
			if(check_bounds_x(x)) { 
				*r=0; *g=0; *b=0;
				if(a) *a=0; 
				return 1;
			}
			else {
				y = apply_wrap_y( y);
				return 0;
			}
		}
		else if (flgWrapState == IMAGE_WRAP_STATE_XY) {
			x = apply_wrap_x( x);
			y = apply_wrap_y( y);
			return 0;
		}
		else {
			return 0;
		}
	}

	inline int check_bounds_and_apply_wrap(int &x, int &y
										   ) const {
	    if (flgWrapState == IMAGE_WRAP_STATE_NONE) {
			if(check_bounds(x,y)) { return 1; }
			else { return 0;}
		}
		else if (flgWrapState == IMAGE_WRAP_STATE_X) {
			if(check_bounds_y(y)) { return 1; }
			else { x = apply_wrap_x( x); return 0; }
		}
		else if (flgWrapState == IMAGE_WRAP_STATE_Y) {
			if(check_bounds_x(x)) { return 1; }
			else { y = apply_wrap_y( y); return 0;}
		}
		else if (flgWrapState == IMAGE_WRAP_STATE_XY) {
			x = apply_wrap_x( x);
			y = apply_wrap_y( y);
			return 0;
		}
		else {
			return 0;
		}

	}

	inline int check_bounds_and_apply_wrap(double &x, double &y,
										   uchar *r, uchar *g, uchar *b, uchar *a
										   ) const {

	    if (flgWrapState == IMAGE_WRAP_STATE_NONE) {
			if(x<0.0 || y<0.0 || x>=(double)(xres-1) || y>=(double)(yres-1)) { *r=0; *g=0; *b=0; if(a) *a=0; return 1; }
			else { return 0;}
		}
		else if (flgWrapState == IMAGE_WRAP_STATE_X) {
			if(y<0.0 || y>=(double)(yres-1)) { *r=0; *g=0; *b=0; if(a) *a=0; return 1; }
			else { x = apply_wrap_x( x); return 0;}
		}
		else if (flgWrapState == IMAGE_WRAP_STATE_Y) {
			if(x<0.0 || x>=(double)(xres-1)) { *r=0; *g=0; *b=0; if(a) *a=0; return 1; }
			else { y = apply_wrap_y( y); return 0;}
		}
		else if (flgWrapState == IMAGE_WRAP_STATE_XY) {
			x = apply_wrap_x( x);
			y = apply_wrap_y( y);
			return 0;
		}
		else {
			return 0;
		}

   }

	inline int check_bounds_and_apply_wrap(double &x, double &y
										   ) const {

	    if (flgWrapState == IMAGE_WRAP_STATE_NONE) {
			if(x<0.0 || y<0.0 || x>=(double)(xres-1) || y>=(double)(yres-1)) { 
				return 1;
			}
			else {
				return 0;
			}
		}
		else if (flgWrapState == IMAGE_WRAP_STATE_X) {
			if(y<0.0 || y>=(double)(yres-1)) { return 1; }
			else { x = apply_wrap_x( x); return 0;}
		}
		else if (flgWrapState == IMAGE_WRAP_STATE_Y) {
			if(x<0.0 || x>=(double)(xres-1)) { return 1; }
			else { y = apply_wrap_y( y); return 0; }
		}
		else if (flgWrapState == IMAGE_WRAP_STATE_XY) {
			x = apply_wrap_x( x);
			y = apply_wrap_y( y);
			return 0;
		}
		else {
			return 0;
		}

   }

	inline int apply_wrap_x( int x) const {
		while (x < 0) x += xres;
		while (x >= xres) x -= xres;
		return x;
	}
	inline int apply_wrap_y( int y) const {
		while (y < 0) y += yres;
		while (y >= yres) y -= yres;
		return y;
	}

	inline double apply_wrap_x( double x) const {
		while (x < 0) x += (double)xres;
		while (x >= (double)xres) x -= (double)xres;
		return x;
	}
	inline double apply_wrap_y( double y) const {
		while (y < 0) y += (double)yres;
		while (y >= (double)yres) y -= (double)yres;
		return y;
	}

    public:

	int	cx,cy,cband;				// current location

	int	clean;		// flag to delete other data in destructor

	Image_Wrap_State_Type flgWrapState;  // flag for wrap-around state
	virtual void set_wrap_state( Image_Wrap_State_Type st = IMAGE_WRAP_STATE_NONE) {
		flgWrapState = st;
	}
	virtual  Image_Wrap_State_Type  get_wrap_state() { return flgWrapState;}


	// Storage control functions:
	virtual	int	allocate(int =0, int =1, int =1) { return 0; }
	virtual void	free() { ; }
	virtual	int	add_band() { return 0; }
	virtual int	remove_band(int =LAST_BAND) { return 0; }
	virtual int	clear(int b=ALL_BANDS);

	// Default & band functions:
	virtual	void	set_default_band(int b=0) { default_band=b; }
	virtual	void	set_red_band(int b=0) { red_band=b; }
	virtual	void	set_green_band(int b=1) { green_band=b; }
	virtual	void	set_blue_band(int b=2) { blue_band=b; }
	virtual	void	set_alpha_band(int b=-1) { alpha_band=b; }
	virtual void	set_default_alpha(uchar a=255) { default_alpha=a; }
	virtual void	set_rgb_bands(int r=0, int g=1, int b=2) {
				set_red_band(r);
				set_green_band(g);
				set_blue_band(b); }
	virtual int	get_default_band() { return default_band; }
	virtual int	get_red_band() { return red_band; }
	virtual int	get_green_band() { return green_band; }
	virtual int	get_blue_band() { return blue_band; }
	virtual int	get_alpha_band() { return alpha_band; }
	virtual uchar	get_default_alpha() { return default_alpha; }

	// Pyramid level functions:
	virtual	void	set_level(double l) { clev=pyr_lev(l);  }
	virtual void	set_spacing(double s) { if(s<=0.0) clev=0.0;
				else set_level(log(s)*1.442695041); }
	virtual double	get_level() { return clev; }

	// Color map functions:
	virtual Image	*get_map() { return map; }
	virtual void	set_map(Image *m) { map=m; }
	virtual void	free_map();

	// Image information functions:
	virtual	int	get_res(int *x=NULL, int *y=NULL, int *b=NULL);
	virtual	int	get_bands() { return bands; }
	virtual	int	get_levels() { return levels; }
	virtual	int	access() { return 0; }		// r/w access
	virtual int	type() { return 0; }		// class id
	virtual char	*type_name() { return (char *)"ImageData"; }
	virtual int	preferred_data_type() { return NO_DATA; }
	virtual	int	preferred_file_type() { return 0; }
	virtual GeoData	*get_geo_data(void) { return((GeoData *)NULL); }

	// For 24 bit color display:
	// get a single pixel color
	virtual	void	get_color(int x, int y, uchar *r, uchar *g, uchar *b,
				  uchar *a=NULL);
	virtual	void	get_color_row(int x, int y, int length, uchar *r, uchar *g, uchar *b,
				  uchar *a=NULL) {
				if(a) while(length--) get_color(x++, y, r++, g++, b++, a++);
				else  while(length--) get_color(x++, y, r++, g++, b++);
			}
	virtual	void	iget_color_row(double x, double y, double xstep, int length, uchar *r, uchar *g, uchar *b,
				  uchar *a=NULL) {
				x -= xstep;	// subtract xstep first so first access is at input x
				if(a) while(length--) iget_color(x+=xstep, y, r++, g++, b++, a++);
				else  while(length--) iget_color(x+=xstep, y, r++, g++, b++);
			}
	virtual	void	iget_color(double x, double y, uchar *r, uchar *g, uchar *b,
				  uchar *a=NULL);
	virtual	void	set_color(int x, int y, uchar r, uchar g, uchar b,
				  uchar a=255);
	virtual	void	iset_color(double x, double y, uchar r, uchar g, uchar b,
				  uchar a=255);
	virtual	uchar	get_alpha(int x, int y);
	virtual uchar	iget_alpha(double x, double y);
	virtual	void	set_alpha(int x, int y, uchar a=255);
	virtual	void	iset_alpha(double x, double y, uchar a=255);

	// For 16 bit XColor info:
	virtual ushort	get_red16(int x, int y=0);
	virtual	ushort	get_green16(int x, int y=0);
	virtual ushort	get_blue16(int x, int y=0);

	virtual ushort	iget_red16(double x, double y=0.0);
	virtual	ushort	iget_green16(double x, double y=0.0);
	virtual ushort	iget_blue16(double x, double y=0.0);

	virtual void	set_red16(ushort c, int x, int y=0);
	virtual	void	set_green16(ushort c, int x, int y=0);
	virtual void	set_blue16(ushort c, int x, int y=0);

	virtual void	iset_red16(ushort c, double x, double y=0.0);
	virtual	void	iset_green16(ushort c, double x, double y=0.0);
	virtual void	iset_blue16(ushort c, double x, double y=0.0);

	// Data types:

	// Get value functions: (integer pixel)
	virtual	int	get_bit(int x, int y=0, int b=DEFAULT_BAND);
	virtual	char	get_char(int , int =0, int =DEFAULT_BAND) { return 0; }
	virtual	uchar	get_uchar(int , int =0, int =DEFAULT_BAND) { return 0; }
	virtual	short	get_short(int , int =0, int =DEFAULT_BAND) { return 0; }
	virtual	ushort	get_ushort(int , int =0, int =DEFAULT_BAND) { return 0; }
	virtual	long	get_long(int , int =0, int =DEFAULT_BAND) { return 0; }
	virtual	ulong	get_ulong(int , int =0, int =DEFAULT_BAND) { return 0; }
	virtual	float	get_float(int , int =0, int =DEFAULT_BAND) { return 0.0; }
	virtual	double	get_double(int , int =0, int =DEFAULT_BAND) { return 0.0; }
	virtual	COMPLEX_TYPE	get_complex(int x, int y=0, int b=DEFAULT_BAND);

	// Get value functions: (bilinear interpolation)
	virtual	int	iget_bit(double x, double y=0.0, int b=DEFAULT_BAND);
	virtual	char	iget_char(double x, double y=0.0, int b=DEFAULT_BAND);
	virtual	uchar	iget_uchar(double x, double y=0.0, int b=DEFAULT_BAND);
	virtual	short	iget_short(double x, double y=0.0, int b=DEFAULT_BAND);
	virtual	ushort	iget_ushort(double x, double y=0.0, int b=DEFAULT_BAND);
	virtual	long	iget_long(double x, double y=0.0, int b=DEFAULT_BAND);
	virtual	ulong	iget_ulong(double x, double y=0.0, int b=DEFAULT_BAND);
	virtual	float	iget_float(double x, double y=0.0, int b=DEFAULT_BAND);
	virtual	double	iget_double(double x, double y=0.0, int b=DEFAULT_BAND);
	virtual	COMPLEX_TYPE	iget_complex(double x, double y=0.0, int b=DEFAULT_BAND);

	// Set value functions: (integer pixel)
	virtual	void	set_bit(int v, int x, int y=0, int b=DEFAULT_BAND);
	virtual	void	set_char(char , int , int =0, int =DEFAULT_BAND) { ; }
	virtual	void	set_uchar(uchar , int , int =0, int =DEFAULT_BAND) { ; }
	virtual	void	set_short(short , int , int =0, int =DEFAULT_BAND) { ; }
	virtual	void	set_ushort(ushort , int , int =0, int =DEFAULT_BAND) { ; }
	virtual	void	set_long(long , int , int =0, int =DEFAULT_BAND) { ; }
	virtual	void	set_ulong(ulong , int , int =0, int =DEFAULT_BAND) { ; }
	virtual	void	set_float(float , int , int =0, int =DEFAULT_BAND) { ; }
	virtual	void	set_double(double , int , int =0, int =DEFAULT_BAND) { ; }
	virtual	void	set_complex(COMPLEX_TYPE v, int x, int y=0, int b=DEFAULT_BAND);

	// Set value functions: (floating point coordinates)
	virtual	void	iset_bit(int v, double x, double y=0.0, int b=DEFAULT_BAND);
	virtual	void	iset_char(char v, double x, double y=0.0, int b=DEFAULT_BAND);
	virtual	void	iset_uchar(uchar v, double x, double y=0.0, int b=DEFAULT_BAND);
	virtual	void	iset_short(short v, double x, double y=0.0, int b=DEFAULT_BAND);
	virtual	void	iset_ushort(ushort v, double x, double y=0.0, int b=DEFAULT_BAND);
	virtual	void	iset_long(long v, double x, double y=0.0, int b=DEFAULT_BAND);
	virtual	void	iset_ulong(ulong v, double x, double y=0.0, int b=DEFAULT_BAND);
	virtual	void	iset_float(float v, double x, double y=0.0, int b=DEFAULT_BAND);
	virtual	void	iset_double(double v, double x, double y=0.0, int b=DEFAULT_BAND);
	virtual	void	iset_complex(COMPLEX_TYPE v, double x, double y=0.0, int b=DEFAULT_BAND);

	// Overloaded set value function:
	void	set(char v, int x, int y=0, int b=DEFAULT_BAND) {	set_char(v,x,y,b); }
	void	set(uchar v, int x, int y=0, int b=DEFAULT_BAND) {	set_uchar(v,x,y,b); }
	void	set(short v, int x, int y=0, int b=DEFAULT_BAND) {	set_short(v,x,y,b); }
	void	set(ushort v, int x, int y=0, int b=DEFAULT_BAND) {	set_ushort(v,x,y,b); }
	void	set(long v, int x, int y=0, int b=DEFAULT_BAND) {	set_long(v,x,y,b); }
	void	set(ulong v, int x, int y=0, int b=DEFAULT_BAND) {	set_ulong(v,x,y,b); }
	void	set(float v, int x, int y=0, int b=DEFAULT_BAND) {	set_float(v,x,y,b); }
	void	set(double v, int x, int y=0, int b=DEFAULT_BAND) {	set_double(v,x,y,b); }
	void	set(COMPLEX_TYPE v, int x, int y=0, int b=DEFAULT_BAND) {	set_complex(v,x,y,b); }
	void	set(int v, int x, int y=0, int b=DEFAULT_BAND) {	set_long((long)v,x,y,b); }
	void	set(char v, double x, double y=0.0, int b=DEFAULT_BAND) {	iset_char(v,x,y,b); }
	void	set(uchar v, double x, double y=0.0, int b=DEFAULT_BAND) {	iset_uchar(v,x,y,b); }
	void	set(short v, double x, double y=0.0, int b=DEFAULT_BAND) {	iset_short(v,x,y,b); }
	void	set(ushort v, double x, double y=0.0, int b=DEFAULT_BAND) {	iset_ushort(v,x,y,b); }
	void	set(long v, double x, double y=0.0, int b=DEFAULT_BAND) {	iset_long(v,x,y,b); }
	void	set(ulong v, double x, double y=0.0, int b=DEFAULT_BAND) {	iset_ulong(v,x,y,b); }
	void	set(float v, double x, double y=0.0, int b=DEFAULT_BAND) {	iset_float(v,x,y,b); }
	void	set(double v, double x, double y=0.0, int b=DEFAULT_BAND) {	iset_double(v,x,y,b); }
	void	set(COMPLEX_TYPE v, double x, double y=0.0, int b=DEFAULT_BAND) {	iset_complex(v,x,y,b); }
	void	set(int v, double x, double y=0.0, int b=DEFAULT_BAND) {	iset_long((long)v,x,y,b); }

	// Operator for easy (but less efficient) image data access:
	ImageData	&operator()(int x, int y=0, int b=DEFAULT_BAND) {
					cx=x; cy=y; cband=b; return *this; }

	// Overloaded assignment operators: (for setting values)
	void	operator=(char v) { set_char(v,cx,cy,cband); }
	void	operator=(uchar v) { set_uchar(v,cx,cy,cband); }
	void	operator=(short v) { set_short(v,cx,cy,cband); }
	void	operator=(ushort v) { set_ushort(v,cx,cy,cband); }
	void	operator=(long v) { set_long(v,cx,cy,cband); }
	void	operator=(ulong v) { set_ulong(v,cx,cy,cband); }
	void	operator=(float v) { set_float(v,cx,cy,cband); }
	void	operator=(double v) { set_double(v,cx,cy,cband); }
	void	operator=(COMPLEX_TYPE v) { set_complex(v,cx,cy,cband); }
	void	operator=(int v) { set_long((long)v,cx,cy,cband); }

	// Typecast operators: (for getting values)
	operator	char() { return get_char(cx,cy,cband); }
	operator	uchar() { return get_uchar(cx,cy,cband); }
	operator	short() { return get_short(cx,cy,cband); }
	operator	ushort() { return get_ushort(cx,cy,cband); }
	operator	long() { return get_long(cx,cy,cband); }
	operator	ulong() { return get_ulong(cx,cy,cband); }
	operator	float() { return get_float(cx,cy,cband); }
	operator	double() { return get_double(cx,cy,cband); }
	operator	COMPLEX_TYPE() { return get_complex(cx,cy,cband); }
	operator	int() { return (int)get_long(cx,cy,cband); }

	// Constructor:
	ImageData() { init(); }

	// Destructor: Free the data:
	virtual ~ImageData() { if(clean) free_map(); }
	};


typedef ImageData *(*DE_FUNC)(int);
extern	DE_FUNC	Data_Extension_Func;
ImageData *new_data_by_type(int type);

#endif // _IMAGEDATA_H_
