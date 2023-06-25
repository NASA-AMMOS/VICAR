// datamod.h
//
// Written by Dave Kagels 1/12/95

#ifndef _DATAMOD_H_
#define _DATAMOD_H_

#include "string.h"
#include "image/geoimage.h"
#include "image/imagedata.h"


// ****************************** blackData *******************************

class blackData : public ImageData {

    public:

	int	allocate(int x=0, int y=1, int b=1) { xres=x; yres=y; bands=b;
							return 1;}
	void	free() { xres=0; yres=0; bands=0; }
	int	add_band() { bands++; return 1; }
	int	remove_band(int) { if(bands>0) bands--; return 1; }
	int	clear(int) { return 1; }

	int	access() { return(DATA_READ); }
	int	type() { return 100; }		// class id
	char	*type_name() { return (char *)"blackData"; }
	int	preferred_data_type() { return UCHAR_DATA; }

	// For 24 bit color display:
	void	get_color(int, int, uchar *r, uchar *g, uchar *b,
			  uchar *a=NULL) { *r=0; *g=0; *b=0; if(a) *a=default_alpha; }
	void	iget_color(double, double, uchar *r, uchar *g, uchar *b,
			  uchar *a=NULL) { *r=0; *g=0; *b=0; if(a) *a=default_alpha; }
	void    get_color_row(int x, int y, int length, uchar *r, uchar *g, uchar *b,
			  uchar *a=NULL) {
			memset(r, 0, length);
			memset(g, 0, length);
			memset(b, 0, length);
			if(a) memset(a, default_alpha, length);
		}
	void    iget_color_row(double x, double y, double xstep, int length, uchar *r, uchar *g, uchar *b,
			  uchar *a=NULL) {
			memset(r, 0, length);
			memset(g, 0, length);
			memset(b, 0, length);
			if(a) memset(a, default_alpha, length);
		}
	void	set_color(int, int, uchar, uchar, uchar, uchar) { ; }
	uchar	get_alpha(int, int) { return default_alpha; }
	uchar	iget_alpha(double, double) { return default_alpha; }
	void	set_alpha(int, int, uchar) { ; }

	// For 16 bit XColor info:
	ushort	get_red16(int, int) { return 0; }
	ushort	get_green16(int, int) { return 0; }
	ushort	get_blue16(int, int) { return 0; }

	ushort	iget_red16(double, double) { return 0; }
	ushort	iget_green16(double, double) { return 0; }
	ushort	iget_blue16(double, double) { return 0; }

	void	set_red16(ushort, int, int) { ; }
	void	set_green16(ushort, int, int) { ; }
	void	set_blue16(ushort, int, int) { ; }

	// Data types:
	int	get_bit(int, int, int) { return 0; }
	char	get_char(int, int, int) { return 0; }
	uchar	get_uchar(int, int, int) { return 0; }
	short	get_short(int, int, int) { return 0; }
	ushort	get_ushort(int, int, int) { return 0; }
	long	get_long(int, int, int) { return 0; }
	ulong	get_ulong(int, int, int) { return 0; }
	float	get_float(int, int, int) { return 0.0; }
	double	get_double(int, int, int) { return 0.0; }
	COMPLEX_TYPE get_complex(int, int, int) { return complex_zero; }

	int	iget_bit(double, double, int) { return 0; }
	char	iget_char(double, double, int) { return 0; }
	uchar	iget_uchar(double, double, int) { return 0; }
	short	iget_short(double, double, int) { return 0; }
	ushort	iget_ushort(double, double, int) { return 0; }
	long	iget_long(double, double, int) { return 0; }
	ulong	iget_ulong(double, double, int) { return 0; }
	float	iget_float(double, double, int) { return 0.0; }
	double	iget_double(double, double, int) { return 0.0; }
	COMPLEX_TYPE	iget_complex(double, double, int) { return complex_zero; }

	void	set_bit(int, int, int, int) { ; }
	void	set_char(char, int, int, int) { ; }
	void	set_uchar(uchar, int, int, int) { ; }
	void	set_short(short, int, int, int) { ; }
	void	set_ushort(ushort, int, int, int) { ; }
	void	set_long(long, int, int, int) { ; }
	void	set_ulong(ulong, int, int, int) { ; }
	void	set_float(float, int, int, int) { ; }
	void	set_double(double, int, int, int) { ; }
	void	set_complex(COMPLEX_TYPE, int, int, int) { ; }
	};


class blackRect : public blackData {

    public:

	int	type() { return 103; }		// class id
	char	*type_name() { return (char *)"blackRect"; }

	void	get_color(int x, int y, uchar *r, uchar *g, uchar *b,
			  uchar *a=NULL) { *r=0; *g=0; *b=0;
			if(a) { if(check_bounds(x,y)) *a=0; else *a=default_alpha; } }
	void	iget_color(double x, double y, uchar *r, uchar *g, uchar *b,
			  uchar *a=NULL) { *r=0; *g=0; *b=0;
			if(a) { if(x<0.0 || y<0.0 || x>(double)(xres-1) ||
				   y>(double)(yres-1)) *a=0; else *a=default_alpha; } }
	uchar	get_alpha(int x, int y) { if(check_bounds(x,y)) return 0;
			else return default_alpha; }
	uchar	iget_alpha(double x, double y) {
			if(x<0.0 || y<0.0 || x>(double)(xres-1) || y>(double)(yres-1))
				return 0; else return default_alpha; }
	blackRect() { ; }
	blackRect(int x, int y=1, int b=1) { xres=x; yres=y; bands=b; }
	};


// ****************************** rgbData *******************************

class rgbData : public ImageData {

    protected:

	uchar	cr,cg,cb;

	void	init() { cr=0; cg=0; cb=0; }

    public:

	int	allocate(int x=0, int y=1, int =3) { xres=x; yres=y; bands=3;
							return 1;}
	void	free() { xres=0; yres=0; bands=0; }
	int	add_band() { return 0; }
	int	remove_band(int) { return 0; }
	int	clear(int) { init(); return 1; }

	int	access() { return(DATA_READ); }
	int	type() { return 105; }		// class id
	char	*type_name() { return (char *)"rgbData"; }
	int	preferred_data_type() { return UCHAR_DATA; }

	// For 24 bit color display:
	void	get_color(int, int, uchar *r, uchar *g, uchar *b,
			  uchar *a=NULL) { *r=cr; *g=cg; *b=cb; if(a) *a=default_alpha; }
	void	iget_color(double, double, uchar *r, uchar *g, uchar *b,
			  uchar *a=NULL) { *r=cr; *g=cg; *b=cb; if(a) *a=default_alpha; }
	void    get_color_row(int x, int y, int length, uchar *r, uchar *g, uchar *b,
			  uchar *a=NULL) {
			memset(r, cr, length);
			memset(g, cg, length);
			memset(b, cb, length);
			if(a) memset(a, default_alpha, length);
		}
	void    iget_color_row(double x, double y, double xstep, int length, uchar *r, uchar *g, uchar *b,
			  uchar *a=NULL) {
			memset(r, cr, length);
			memset(g, cg, length);
			memset(b, cb, length);
			if(a) memset(a, default_alpha, length);
		}
	void	set_color(int, int, uchar r, uchar g, uchar b, uchar =255) {
			cr=r; cg=g; cb=b; }
	uchar	get_alpha(int, int) { return default_alpha; }
	uchar	iget_alpha(double, double) { return default_alpha; }
	void	set_alpha(int, int, uchar) { ; }

	// For 16 bit XColor info:
	ushort	get_red16(int, int) { return (ushort)cr<<8; }
	ushort	get_green16(int, int) { return (ushort)cg<<8; }
	ushort	get_blue16(int, int) { return (ushort)cb<<8; }

	ushort	iget_red16(double, double) { return (ushort)cr<<8; }
	ushort	iget_green16(double, double) { return (ushort)cg<<8; }
	ushort	iget_blue16(double, double) { return (ushort)cb<<8; }

	void	set_red16(ushort, int, int) { ; }
	void	set_green16(ushort, int, int) { ; }
	void	set_blue16(ushort, int, int) { ; }

	rgbData() { init(); }
	};


class rgbRect : public rgbData {

    public:

	int	type() { return 106; }		// class id
	char	*type_name() { return (char *)"rgbRect"; }

	void	get_color(int x, int y, uchar *r, uchar *g, uchar *b, uchar *a=NULL) {
			if(check_bounds(x,y)) { *r=0; *g=0; *b=0; if(a) *a=0; }
			else { *r=cr; *g=cg; *b=cb; if(a) *a=default_alpha; } }
	void	iget_color(double x, double y, uchar *r, uchar *g, uchar *b, uchar *a=NULL) {
			if(x<0.0 || y<0.0 || x>(double)(xres-1) || y>(double)(yres-1)) {
				*r=0; *g=0; *b=0; if(a) *a=0; }
			else { *r=cr; *g=cg; *b=cb; if(a) *a=default_alpha; } }
	uchar	get_alpha(int x, int y) { if(check_bounds(x,y)) return 0;
			else return default_alpha; }
	uchar	iget_alpha(double x, double y) {
			if(x<0.0 || y<0.0 || x>(double)(xres-1) || y>(double)(yres-1))
				return 0; else return default_alpha; }

	rgbRect() { ; }
	rgbRect(int x, int y=1) { xres=x; yres=y; bands=3; }
	};


// ****************************** transData *******************************

class transData : public ImageData {

    protected:

	void	init();
	void	t2i();				// translate t -> i
	void	i2t();				// translate i -> t

	Image	*img;

	double	tcx,tcy,txx,txy,tyx,tyy;
	double	icx,icy,ixx,ixy,iyx,iyy;
	double	lev_off;

    public:

	int	allocate(int x=0, int y=1, int b=1);
	void	free();
	int	add_band();
	int	remove_band(int b=LAST_BAND);
	int	clear(int b=ALL_BANDS);

	void	set_default_band(int b=0) { if(img) img->set_default_band(b); }
	void	set_red_band(int b=0) { if(img) img->set_red_band(b); }
	void	set_green_band(int b=1) { if(img) img->set_green_band(b); }
	void	set_blue_band(int b=2) { if(img) img->set_blue_band(b); }
	void	set_alpha_band(int b=-1) { if(img) img->set_alpha_band(b); }
	void	set_default_alpha(uchar a=255) { if(img) img->set_default_alpha(a); }
	void	set_rgb_bands(int r=0, int g=1, int b=2) {
				if(img) img->set_rgb_bands(r,g,b); }
	int	get_default_band() { if(!img) return -1;
					else return img->get_default_band(); }
	int	get_red_band() { if(!img) return -1;
					else return img->get_red_band(); }
	int	get_green_band() { if(!img) return -1;
					else return img->get_green_band(); }
	int	get_blue_band() { if(!img) return -1;
					else return img->get_blue_band(); }
	int	get_alpha_band() { if(!img) return -1;
					else return img->get_alpha_band(); }
	uchar	get_default_alpha() { if(!img) return 0;
					else return img->get_default_alpha(); }

	void	set_level(double l) { if(img) img->set_level(l+lev_off); }

	int	access() { if(!img) return 0; else return img->access(); }
	int	type() { return 101; }		// class id
	char	*type_name() { return (char *)"transData"; }
	int	preferred_data_type() { if(!img) return NO_DATA;
				else return img->preferred_data_type(); }

	int	get_bands() { if(img) return img->get_bands(); else return 0; }
	Image	*get_map() { if(img) return img->get_map(); else return NULL; }

	// For 24 bit color display:
	void	get_color(int x, int y, uchar *r, uchar *g, uchar *b,
			  uchar *a=NULL);
	void	iget_color(double x, double y, uchar *r, uchar *g, uchar *b,
			  uchar *a=NULL);
	void	set_color(int x, int y, uchar r, uchar g, uchar b,
			  uchar a=255);
	uchar	get_alpha(int x, int y);
	uchar	iget_alpha(double x, double y);
	void	set_alpha(int x, int y, uchar a=255);

	// For 16 bit XColor info:
//	ushort	get_red16(int x, int y=0);
//	ushort	get_green16(int x, int y=0);
//	ushort	get_blue16(int x, int y=0);

//	ushort	iget_red16(double x, double y=0.0);
//	ushort	iget_green16(double x, double y=0.0);
//	ushort	iget_blue16(double x, double y=0.0);

//	void	set_red16(ushort c, int x, int y=0);
//	void	set_green16(ushort c, int x, int y=0);
//	void	set_blue16(ushort c, int x, int y=0);

	// Data types:
	int	get_bit(int x, int y=0, int b=DEFAULT_BAND) { return (img->get_data())->get_bit((int)(icx+x*ixx+y*iyx),(int)(icy+x*ixy+y*iyy),b); }
	char	get_char(int x, int y=0, int b=DEFAULT_BAND) { return (img->get_data())->get_char((int)(icx+x*ixx+y*iyx),(int)(icy+x*ixy+y*iyy),b); }
	uchar	get_uchar(int x, int y=0, int b=DEFAULT_BAND) { return (img->get_data())->get_uchar((int)(icx+x*ixx+y*iyx),(int)(icy+x*ixy+y*iyy),b); }
	short	get_short(int x, int y=0, int b=DEFAULT_BAND) { return (img->get_data())->get_short((int)(icx+x*ixx+y*iyx),(int)(icy+x*ixy+y*iyy),b); }
	ushort	get_ushort(int x, int y=0, int b=DEFAULT_BAND) { return (img->get_data())->get_ushort((int)(icx+x*ixx+y*iyx),(int)(icy+x*ixy+y*iyy),b); }
	long	get_long(int x, int y=0, int b=DEFAULT_BAND) { return (img->get_data())->get_long((int)(icx+x*ixx+y*iyx),(int)(icy+x*ixy+y*iyy),b); }
	ulong	get_ulong(int x, int y=0, int b=DEFAULT_BAND) { return (img->get_data())->get_ulong((int)(icx+x*ixx+y*iyx),(int)(icy+x*ixy+y*iyy),b); }
	float	get_float(int x, int y=0, int b=DEFAULT_BAND) { return (img->get_data())->get_float((int)(icx+x*ixx+y*iyx),(int)(icy+x*ixy+y*iyy),b); }
	double	get_double(int x, int y=0, int b=DEFAULT_BAND) { return (img->get_data())->get_double((int)(icx+x*ixx+y*iyx),(int)(icy+x*ixy+y*iyy),b); }
	COMPLEX_TYPE get_complex(int x, int y=0, int b=DEFAULT_BAND) { return (img->get_data())->get_complex((int)(icx+x*ixx+y*iyx),(int)(icy+x*ixy+y*iyy),b); }

	int	iget_bit(double x, double y=0.0, int b=DEFAULT_BAND) { return (img->get_data())->iget_bit(icx+x*ixx+y*iyx,icy+x*ixy+y*iyy,b); }
	char	iget_char(double x, double y=0.0, int b=DEFAULT_BAND) { return (img->get_data())->iget_char(icx+x*ixx+y*iyx,icy+x*ixy+y*iyy,b); }
	uchar	iget_uchar(double x, double y=0.0, int b=DEFAULT_BAND) { return (img->get_data())->iget_uchar(icx+x*ixx+y*iyx,icy+x*ixy+y*iyy,b); }
	short	iget_short(double x, double y=0.0, int b=DEFAULT_BAND) { return (img->get_data())->iget_short(icx+x*ixx+y*iyx,icy+x*ixy+y*iyy,b); }
	ushort	iget_ushort(double x, double y=0.0, int b=DEFAULT_BAND) { return (img->get_data())->iget_ushort(icx+x*ixx+y*iyx,icy+x*ixy+y*iyy,b); }
	long	iget_long(double x, double y=0.0, int b=DEFAULT_BAND) { return (img->get_data())->iget_long(icx+x*ixx+y*iyx,icy+x*ixy+y*iyy,b); }
	ulong	iget_ulong(double x, double y=0.0, int b=DEFAULT_BAND) { return (img->get_data())->iget_ulong(icx+x*ixx+y*iyx,icy+x*ixy+y*iyy,b); }
	float	iget_float(double x, double y=0.0, int b=DEFAULT_BAND) { return (img->get_data())->iget_float(icx+x*ixx+y*iyx,icy+x*ixy+y*iyy,b); }
	double	iget_double(double x, double y=0.0, int b=DEFAULT_BAND) { return (img->get_data())->iget_double(icx+x*ixx+y*iyx,icy+x*ixy+y*iyy,b); }
	COMPLEX_TYPE	iget_complex(double x, double y=0.0, int b=DEFAULT_BAND) { return (img->get_data())->iget_complex(icx+x*ixx+y*iyx,icy+x*ixy+y*iyy,b); }

//	void	set_bit(int v, int x, int y=0, int b=DEFAULT_BAND);
//	void	set_char(char v, int x, int y=0, int b=DEFAULT_BAND);
//	void	set_uchar(uchar v, int x, int y=0, int b=DEFAULT_BAND);
//	void	set_short(short v, int x, int y=0, int b=DEFAULT_BAND);
//	void	set_ushort(ushort v, int x, int y=0, int b=DEFAULT_BAND);
//	void	set_long(long v, int x, int y=0, int b=DEFAULT_BAND);
//	void	set_ulong(ulong v, int x, int y=0, int b=DEFAULT_BAND);
//	void	set_float(float v, int x, int y=0, int b=DEFAULT_BAND);
//	void	set_double(double v, int x, int y=0, int b=DEFAULT_BAND);
//	void	set_complex(COMPLEX_TYPE v, int x, int y=0, int b=DEFAULT_BAND);

	void	set_image(Image *i) { img=i; 
			if(img) { bands=img->get_bands();
					  flgWrapState=img->get_data()->flgWrapState;
				if(!xres||!yres) img->get_res(&xres,&yres); } }
	void	set_image(Image *i, double lcx, double lcy) {
			set_image(i); set_corner(lcx,lcy); }
	void	set_image(Image *i, double lcx, double lcy, double xx, double xy,
							  double yx, double yy) {
			set_image(i); set_trans(lcx,lcy,xx,xy,yx,yy); }
	void	set_corner(double lcx=0.0, double lcy=0.0) {
			tcx=lcx; tcy=lcy; t2i(); }
	void	set_trans(double lcx=0.0, double lcy=0.0, double xx=1.0, double xy=0.0,
							double yx=0.0, double yy=1.0) {
			tcx=lcx; tcy=lcy; txx=xx; txy=xy; tyx=yx; tyy=yy; t2i(); }
	void	set_icorner(double lcx=0.0, double lcy=0.0) {
			icx=lcx; icy=lcy; i2t(); }
	void	set_itrans(double lcx=0.0, double lcy=0.0, double xx=1.0, double xy=0.0,
							double yx=0.0, double yy=1.0) {
			icx=lcx; icy=lcy; ixx=xx; ixy=xy; iyx=yx; iyy=yy; i2t(); }
	Image	*get_image() { return img; }
	void	get_trans(double *lcx, double *lcy, double *xx=NULL, double *xy=NULL,
						  double *yx=NULL, double *yy=NULL) {
			if(lcx) *lcx=tcx;	if(lcy) *lcy=tcy;
			if(xx) *xx=txx;	if(xy) *xy=txy;
			if(yx) *yx=tyx;	if(yy) *yy=tyy; }
	void	get_itrans(double *lcx, double *lcy, double *xx=NULL, double *xy=NULL,
						  double *yx=NULL, double *yy=NULL) {
			if(lcx) *lcx=icx;	if(lcy) *lcy=icy;
			if(xx) *xx=ixx;	if(xy) *xy=ixy;
			if(yx) *yx=iyx;	if(yy) *yy=iyy; }

	GeoData	*get_geo_data(void) {
			GeoData	*gd=NULL;
			if(img) {
				if(img->image_class_type() == GEOIMAGE) {
					gd = ((GeoImage *)img)->get_geo_data();
				}
			}
			return(gd);
		}


	void set_wrap_state( Image_Wrap_State_Type st = IMAGE_WRAP_STATE_NONE);
	Image_Wrap_State_Type  get_wrap_state();

	transData() { init(); }
	transData(Image *i) { init(); set_image(i); }
	transData(Image *i, double lcx, double lcy, double xx=1.0, double xy=0.0,
						  double yx=0.0, double yy=1.0) {
		init(); set_image(i); set_trans(lcx,lcy,xx,xy,yx,yy); }
	~transData() { free(); }
	};


// ****************************** compData *******************************

class compData : public ImageData {

    protected:

	void	init();

	Image	**img;
	int	num;

	int	crossmap(Image *, Image *, double *, double *);

    public:

	int	allocate(int x=0, int y=1, int b=1);
	void	free();
	int	add_band() { return 0; }
	int	remove_band(int) { return 0; }
	int	clear(int) { return 0; }

	int	access() { return(DATA_READ); }
	int	type() { return 102; }		// class id
	char	*type_name() { return (char *)"compData"; }
	int	preferred_data_type() { return UCHAR_DATA; }

	// For 24 bit color display:
	void	get_color(int x, int y, uchar *r, uchar *g, uchar *b,
			  uchar *a=NULL);
	void	iget_color(double x, double y, uchar *r, uchar *g, uchar *b,
			  uchar *a=NULL);
	void	set_color(int, int, uchar, uchar, uchar, uchar) { ; }
	uchar	get_alpha(int x, int y);
	uchar	iget_alpha(double x, double y);
	void	set_alpha(int, int, uchar) { ; }

	void	set_level(double);

	// For 16 bit XColor info:
//	ushort	get_red16(int x, int y=0);
//	ushort	get_green16(int x, int y=0);
//	ushort	get_blue16(int x, int y=0);

//	ushort	iget_red16(double x, double y=0.0);
//	ushort	iget_green16(double x, double y=0.0);
//	ushort	iget_blue16(double x, double y=0.0);

	void	set_red16(ushort, int, int) { ; }
	void	set_green16(ushort, int, int) { ; }
	void	set_blue16(ushort, int, int) { ; }

	// Data types:
	int	get_bit(int x, int y=0, int b=DEFAULT_BAND);
	char	get_char(int x, int y=0, int b=DEFAULT_BAND);
	uchar	get_uchar(int x, int y=0, int b=DEFAULT_BAND);
	short	get_short(int x, int y=0, int b=DEFAULT_BAND);
	ushort	get_ushort(int x, int y=0, int b=DEFAULT_BAND);
	long	get_long(int x, int y=0, int b=DEFAULT_BAND);
	ulong	get_ulong(int x, int y=0, int b=DEFAULT_BAND);
	float	get_float(int x, int y=0, int b=DEFAULT_BAND);
	double	get_double(int x, int y=0, int b=DEFAULT_BAND);
	COMPLEX_TYPE get_complex(int x, int y=0, int b=DEFAULT_BAND);

	int	iget_bit(double x, double y=0.0, int b=DEFAULT_BAND);
	char	iget_char(double x, double y=0.0, int b=DEFAULT_BAND);
	uchar	iget_uchar(double x, double y=0.0, int b=DEFAULT_BAND);
	short	iget_short(double x, double y=0.0, int b=DEFAULT_BAND);
	ushort	iget_ushort(double x, double y=0.0, int b=DEFAULT_BAND);
	long	iget_long(double x, double y=0.0, int b=DEFAULT_BAND);
	ulong	iget_ulong(double x, double y=0.0, int b=DEFAULT_BAND);
	float	iget_float(double x, double y=0.0, int b=DEFAULT_BAND);
	double	iget_double(double x, double y=0.0, int b=DEFAULT_BAND);
	COMPLEX_TYPE	iget_complex(double x, double y=0.0, int b=DEFAULT_BAND);

	void	set_bit(int, int, int, int) { ; }
	void	set_char(char, int, int, int) { ; }
	void	set_uchar(uchar, int, int, int) { ; }
	void	set_short(short, int, int, int) { ; }
	void	set_ushort(ushort, int, int, int) { ; }
	void	set_long(long, int, int, int) { ; }
	void	set_ulong(ulong, int, int, int) { ; }
	void	set_float(float, int, int, int) { ; }
	void	set_double(double, int, int, int) { ; }
	void	set_complex(COMPLEX_TYPE, int, int, int) { ; }

	void	add_image(Image *i, int p=-1);
	void	remove_image(int p=-1);
	int	get_count() { return num; }
	Image	*get_image(int p=-1) { if(p==-1) p=num-1;
			if(p<0 || p>=num) return NULL; else return img[p]; }
	Image	**get_image_list(int *n=NULL) { if(n) *n=num; return img; }

	GeoData	*get_geo_data(void) {
			GeoData	*gd=NULL;
			if(img) {
				if(img[0]) {
					if(img[0]->image_class_type() == GEOIMAGE) {
						gd = ((GeoImage *)img[0])->get_geo_data();
					}
				}
			}
			return(gd);
		}

	compData() { init(); }
	compData(Image *i1, Image *i2=NULL) { init(); add_image(i1);
			if(i2) add_image(i2); }
	compData(Image **i);
	compData(Image **i, int n);
	~compData();
	};


// ****************************** multiBand *******************************

class multiBand : public ImageData {

    protected:

	void	init();

	Image	**img;

    public:

	int	allocate(int x=0, int y=1, int b=1);
	void	free();
	int	add_band() { return 0; }
	int	remove_band(int) { return 0; }
	int	clear(int) { return 0; }

	int	access() { return(DATA_READ|DATA_WRITE); }
	int	type() { return 107; }		// class id
	char	*type_name() { return (char *)"multiBand"; }
	int	preferred_data_type() { return UCHAR_DATA; }

	// For 24 bit color display:
//	void	get_color(int x, int y, uchar *r, uchar *g, uchar *b,
//			  uchar *a=NULL);
//	void	iget_color(double x, double y, uchar *r, uchar *g, uchar *b,
//			  uchar *a=NULL);
//	void	set_color(int, int, uchar, uchar, uchar, uchar);
//	uchar	get_alpha(int x, int y);
//	uchar	iget_alpha(double x, double y);
//	void	set_alpha(int, int, uchar);

	void	set_level(double);

	// For 16 bit XColor info:
//	ushort	get_red16(int x, int y=0);
//	ushort	get_green16(int x, int y=0);
//	ushort	get_blue16(int x, int y=0);

//	ushort	iget_red16(double x, double y=0.0);
//	ushort	iget_green16(double x, double y=0.0);
//	ushort	iget_blue16(double x, double y=0.0);

//	void	set_red16(ushort, int, int);
//	void	set_green16(ushort, int, int);
//	void	set_blue16(ushort, int, int);

	// Data types:
//	int	get_bit(int x, int y=0, int b=DEFAULT_BAND);
//	char	get_char(int x, int y=0, int b=DEFAULT_BAND);
	uchar	get_uchar(int x, int y=0, int b=DEFAULT_BAND);
//	short	get_short(int x, int y=0, int b=DEFAULT_BAND);
//	ushort	get_ushort(int x, int y=0, int b=DEFAULT_BAND);
//	long	get_long(int x, int y=0, int b=DEFAULT_BAND);
//	ulong	get_ulong(int x, int y=0, int b=DEFAULT_BAND);
//	float	get_float(int x, int y=0, int b=DEFAULT_BAND);
//	double	get_double(int x, int y=0, int b=DEFAULT_BAND);
//	COMPLEX_TYPE get_complex(int x, int y=0, int b=DEFAULT_BAND);

//	int	iget_bit(double x, double y=0.0, int b=DEFAULT_BAND);
//	char	iget_char(double x, double y=0.0, int b=DEFAULT_BAND);
	uchar	iget_uchar(double x, double y=0.0, int b=DEFAULT_BAND);
//	short	iget_short(double x, double y=0.0, int b=DEFAULT_BAND);
//	ushort	iget_ushort(double x, double y=0.0, int b=DEFAULT_BAND);
//	long	iget_long(double x, double y=0.0, int b=DEFAULT_BAND);
//	ulong	iget_ulong(double x, double y=0.0, int b=DEFAULT_BAND);
//	float	iget_float(double x, double y=0.0, int b=DEFAULT_BAND);
//	double	iget_double(double x, double y=0.0, int b=DEFAULT_BAND);
//	COMPLEX_TYPE	iget_complex(double x, double y=0.0, int b=DEFAULT_BAND);

//	void	set_bit(int, int, int, int);
//	void	set_char(char, int, int, int);
	void	set_uchar(uchar, int, int, int);
//	void	set_short(short, int, int, int);
//	void	set_ushort(ushort, int, int, int);
//	void	set_long(long, int, int, int);
//	void	set_ulong(ulong, int, int, int);
//	void	set_float(float, int, int, int);
//	void	set_double(double, int, int, int);
//	void	set_complex(COMPLEX_TYPE, int, int, int);

	void	add_image(Image *i, int p=-1);
	void	remove_image(int p=-1);
	Image	*get_image(int p=-1) { if(p==-1) p=bands-1;
			if(p<0 || p>=bands) return NULL; else return img[p]; }
	Image	**get_image_list(int *n=NULL) { if(n) *n=bands; return img; }
	GeoData	*get_geo_data(void) {
			GeoData	*gd=NULL;
			if(img) {
				if(img[0]) {
					if(img[0]->image_class_type() == GEOIMAGE) {
						gd = ((GeoImage *)img[0])->get_geo_data();
					}
				}
			}
			return(gd);
		}


	multiBand() { init(); }
	multiBand(Image *i1, Image *i2=NULL) { init(); add_image(i1);
			if(i2) add_image(i2); }
	multiBand(Image **i);
	multiBand(Image **i, int n);
	~multiBand();
	};


// ****************************** fadeBorder *******************************

class fadeBorder : public ImageData {

    protected:

	void	init() { img=NULL; bwidth=0; cropwidth=0;}

	Image	*img;

	int	bwidth;
	int	cropwidth;

    public:

	int	allocate(int x=0, int y=1, int b=1);
	void	free();
	int	add_band();
	int	remove_band(int b=LAST_BAND);
	int	clear(int b=ALL_BANDS);

	void	set_default_band(int b=0) { if(img) img->set_default_band(b); }
	void	set_red_band(int b=0) { if(img) img->set_red_band(b); }
	void	set_green_band(int b=1) { if(img) img->set_green_band(b); }
	void	set_blue_band(int b=2) { if(img) img->set_blue_band(b); }
	void	set_alpha_band(int b=-1) { if(img) img->set_alpha_band(b); }
	void	set_default_alpha(uchar a=255) { if(img) img->set_default_alpha(a); }
	void	set_rgb_bands(int r=0, int g=1, int b=2) {
				if(img) img->set_rgb_bands(r,g,b); }
	int	get_default_band() { if(!img) return -1;
					else return img->get_default_band(); }
	int	get_red_band() { if(!img) return -1;
					else return img->get_red_band(); }
	int	get_green_band() { if(!img) return -1;
					else return img->get_green_band(); }
	int	get_blue_band() { if(!img) return -1;
					else return img->get_blue_band(); }
	int	get_alpha_band() { if(!img) return -1;
					else return img->get_alpha_band(); }
	uchar	get_default_alpha() { if(!img) return 0;
					else return img->get_default_alpha(); }

	void	set_level(double l) { if(img) img->set_level(l); }

	int	access() { if(!img) return 0; else return img->access(); }
	int	type() { return 110; }		// class id
	char	*type_name() { return (char *)"fadeBorder"; }
	int	preferred_data_type() { if(!img) return NO_DATA;
				else return img->preferred_data_type(); }

	int	get_bands() { if(img) return img->get_bands(); else return 0; }
	Image	*get_map() { if(img) return img->get_map(); else return NULL; }

	// For 24 bit color display:
	void	get_color(int x, int y, uchar *r, uchar *g, uchar *b,
			  uchar *a=NULL);
	void	iget_color(double x, double y, uchar *r, uchar *g, uchar *b,
			  uchar *a=NULL);
	void	set_color(int x, int y, uchar r, uchar g, uchar b,
			  uchar a=255);
	uchar	get_alpha(int x, int y);
	uchar	iget_alpha(double x, double y);
	void	set_alpha(int x, int y, uchar a=255);

	// For 16 bit XColor info:
//	ushort	get_red16(int x, int y=0);
//	ushort	get_green16(int x, int y=0);
//	ushort	get_blue16(int x, int y=0);

//	ushort	iget_red16(double x, double y=0.0);
//	ushort	iget_green16(double x, double y=0.0);
//	ushort	iget_blue16(double x, double y=0.0);

//	void	set_red16(ushort c, int x, int y=0);
//	void	set_green16(ushort c, int x, int y=0);
//	void	set_blue16(ushort c, int x, int y=0);

	// Data types:
	int	get_bit(int x, int y=0, int b=DEFAULT_BAND) {
			return (img->get_data())->get_bit(x,y,b); }
	char	get_char(int x, int y=0, int b=DEFAULT_BAND) {
			return (img->get_data())->get_char(x,y,b); }
	uchar	get_uchar(int x, int y=0, int b=DEFAULT_BAND) {
			return (img->get_data())->get_uchar(x,y,b); }
	short	get_short(int x, int y=0, int b=DEFAULT_BAND) {
			return (img->get_data())->get_short(x,y,b); }
	ushort	get_ushort(int x, int y=0, int b=DEFAULT_BAND) {
			return (img->get_data())->get_ushort(x,y,b); }
	long	get_long(int x, int y=0, int b=DEFAULT_BAND) {
			return (img->get_data())->get_long(x,y,b); }
	ulong	get_ulong(int x, int y=0, int b=DEFAULT_BAND) {
			return (img->get_data())->get_ulong(x,y,b); }
	float	get_float(int x, int y=0, int b=DEFAULT_BAND) {
			return (img->get_data())->get_float(x,y,b); }
	double	get_double(int x, int y=0, int b=DEFAULT_BAND) {
			return (img->get_data())->get_double(x,y,b); }
	COMPLEX_TYPE get_complex(int x, int y=0, int b=DEFAULT_BAND) {
			return (img->get_data())->get_complex(x,y,b); }

	int	iget_bit(double x, double y=0.0, int b=DEFAULT_BAND) {
			return (img->get_data())->iget_bit(x,y,b); }
	char	iget_char(double x, double y=0.0, int b=DEFAULT_BAND) {
			return (img->get_data())->iget_char(x,y,b); }
	uchar	iget_uchar(double x, double y=0.0, int b=DEFAULT_BAND) {
			return (img->get_data())->iget_uchar(x,y,b); }
	short	iget_short(double x, double y=0.0, int b=DEFAULT_BAND) {
			return (img->get_data())->iget_short(x,y,b); }
	ushort	iget_ushort(double x, double y=0.0, int b=DEFAULT_BAND) {
			return (img->get_data())->iget_ushort(x,y,b); }
	long	iget_long(double x, double y=0.0, int b=DEFAULT_BAND) {
			return (img->get_data())->iget_long(x,y,b); }
	ulong	iget_ulong(double x, double y=0.0, int b=DEFAULT_BAND) {
			return (img->get_data())->iget_ulong(x,y,b); }
	float	iget_float(double x, double y=0.0, int b=DEFAULT_BAND) {
			return (img->get_data())->iget_float(x,y,b); }
	double	iget_double(double x, double y=0.0, int b=DEFAULT_BAND) {
			return (img->get_data())->iget_double(x,y,b); }
	COMPLEX_TYPE	iget_complex(double x, double y=0.0, int b=DEFAULT_BAND) {
			return (img->get_data())->iget_complex(x,y,b); }

//	void	set_bit(int v, int x, int y=0, int b=DEFAULT_BAND);
//	void	set_char(char v, int x, int y=0, int b=DEFAULT_BAND);
//	void	set_uchar(uchar v, int x, int y=0, int b=DEFAULT_BAND);
//	void	set_short(short v, int x, int y=0, int b=DEFAULT_BAND);
//	void	set_ushort(ushort v, int x, int y=0, int b=DEFAULT_BAND);
//	void	set_long(long v, int x, int y=0, int b=DEFAULT_BAND);
//	void	set_ulong(ulong v, int x, int y=0, int b=DEFAULT_BAND);
//	void	set_float(float v, int x, int y=0, int b=DEFAULT_BAND);
//	void	set_double(double v, int x, int y=0, int b=DEFAULT_BAND);
//	void	set_complex(COMPLEX_TYPE v, int x, int y=0, int b=DEFAULT_BAND);

	void	set_image(Image *i) { img=i; 
			if(img) { bands=img->get_bands();
					  flgWrapState=img->get_data()->flgWrapState;
					  img->get_res(&xres,&yres); } }

	void	set_border_width(int w) { bwidth=w; }
	void	set_crop_width(int w) { cropwidth=w; }
	GeoData	*get_geo_data(void) {
			GeoData	*gd=NULL;
			if(img) {
				if(img->image_class_type() == GEOIMAGE) {
					gd = ((GeoImage *)img)->get_geo_data();
				}
			}
			return(gd);
		}


	fadeBorder() { init(); }
	fadeBorder(Image *i) { init(); set_image(i); }
	fadeBorder(Image *i, int w, int cw=0) {
		init(); set_image(i); set_border_width(w); set_crop_width(cw); }
	~fadeBorder() { free(); }
	};


// ****************************** cropData *******************************

class cropData : public ImageData {

    protected:

	void	init() { img=NULL; set_borders(0,0,0,0); }

	Image	*img;

	int	xmin,ymin,xmax,ymax;
	double	dxmin,dymin,dxmax,dymax;

    public:

	int	allocate(int x=0, int y=1, int b=1);
	void	free();
	int	add_band();
	int	remove_band(int b=LAST_BAND);
	int	clear(int b=ALL_BANDS);

	int	get_res(int *x, int *y, int *b) {	// reimplemented to reflect cropping
			if(x) *x = xmax - xmin + 1;
			if(y) *y = ymax - ymin + 1;
			if(b) *b=bands;
			return(xres);
		}

	void	set_default_band(int b=0) { if(img) img->set_default_band(b); }
	void	set_red_band(int b=0) { if(img) img->set_red_band(b); }
	void	set_green_band(int b=1) { if(img) img->set_green_band(b); }
	void	set_blue_band(int b=2) { if(img) img->set_blue_band(b); }
	void	set_alpha_band(int b=-1) { if(img) img->set_alpha_band(b); }
	void	set_default_alpha(uchar a=255) { if(img) img->set_default_alpha(a); }
	void	set_rgb_bands(int r=0, int g=1, int b=2) {
				if(img) img->set_rgb_bands(r,g,b); }
	int	get_default_band() { if(!img) return -1;
					else return img->get_default_band(); }
	int	get_red_band() { if(!img) return -1;
					else return img->get_red_band(); }
	int	get_green_band() { if(!img) return -1;
					else return img->get_green_band(); }
	int	get_blue_band() { if(!img) return -1;
					else return img->get_blue_band(); }
	int	get_alpha_band() { if(!img) return -1;
					else return img->get_alpha_band(); }
	uchar	get_default_alpha() { if(!img) return 0;
					else return img->get_default_alpha(); }

	void	set_level(double l) { if(img) img->set_level(l); }

	int	access() { if(!img) return 0; else return img->access(); }
	int	type() { return 111; }		// class id
	char	*type_name() { return (char *)"cropData"; }
	int	preferred_data_type() { if(!img) return NO_DATA;
				else return img->preferred_data_type(); }

	int	get_bands() { if(img) return img->get_bands(); else return 0; }
	Image	*get_map() { if(img) return img->get_map(); else return NULL; }

	// For 24 bit color display:
	void	get_color(int x, int y, uchar *r, uchar *g, uchar *b,
			  uchar *a=NULL);
	void	iget_color(double x, double y, uchar *r, uchar *g, uchar *b,
			  uchar *a=NULL);
	void	set_color(int x, int y, uchar r, uchar g, uchar b,
			  uchar a=255);
	uchar	get_alpha(int x, int y);
	uchar	iget_alpha(double x, double y);
	void	set_alpha(int x, int y, uchar a=255);

	// For 16 bit XColor info:
//	ushort	get_red16(int x, int y=0);
//	ushort	get_green16(int x, int y=0);
//	ushort	get_blue16(int x, int y=0);

//	ushort	iget_red16(double x, double y=0.0);
//	ushort	iget_green16(double x, double y=0.0);
//	ushort	iget_blue16(double x, double y=0.0);

//	void	set_red16(ushort c, int x, int y=0);
//	void	set_green16(ushort c, int x, int y=0);
//	void	set_blue16(ushort c, int x, int y=0);

	// Data types:
	int	get_bit(int x, int y=0, int b=DEFAULT_BAND);
	char	get_char(int x, int y=0, int b=DEFAULT_BAND);
	uchar	get_uchar(int x, int y=0, int b=DEFAULT_BAND);
	short	get_short(int x, int y=0, int b=DEFAULT_BAND);
	ushort	get_ushort(int x, int y=0, int b=DEFAULT_BAND);
	long	get_long(int x, int y=0, int b=DEFAULT_BAND);
	ulong	get_ulong(int x, int y=0, int b=DEFAULT_BAND);
	float	get_float(int x, int y=0, int b=DEFAULT_BAND);
	double	get_double(int x, int y=0, int b=DEFAULT_BAND);
	COMPLEX_TYPE get_complex(int x, int y=0, int b=DEFAULT_BAND);

	int	iget_bit(double x, double y=0.0, int b=DEFAULT_BAND);
	char	iget_char(double x, double y=0.0, int b=DEFAULT_BAND);
	uchar	iget_uchar(double x, double y=0.0, int b=DEFAULT_BAND);
	short	iget_short(double x, double y=0.0, int b=DEFAULT_BAND);
	ushort	iget_ushort(double x, double y=0.0, int b=DEFAULT_BAND);
	long	iget_long(double x, double y=0.0, int b=DEFAULT_BAND);
	ulong	iget_ulong(double x, double y=0.0, int b=DEFAULT_BAND);
	float	iget_float(double x, double y=0.0, int b=DEFAULT_BAND);
	double	iget_double(double x, double y=0.0, int b=DEFAULT_BAND);
	COMPLEX_TYPE	iget_complex(double x, double y=0.0, int b=DEFAULT_BAND);

//	void	set_bit(int v, int x, int y=0, int b=DEFAULT_BAND);
//	void	set_char(char v, int x, int y=0, int b=DEFAULT_BAND);
//	void	set_uchar(uchar v, int x, int y=0, int b=DEFAULT_BAND);
//	void	set_short(short v, int x, int y=0, int b=DEFAULT_BAND);
//	void	set_ushort(ushort v, int x, int y=0, int b=DEFAULT_BAND);
//	void	set_long(long v, int x, int y=0, int b=DEFAULT_BAND);
//	void	set_ulong(ulong v, int x, int y=0, int b=DEFAULT_BAND);
//	void	set_float(float v, int x, int y=0, int b=DEFAULT_BAND);
//	void	set_double(double v, int x, int y=0, int b=DEFAULT_BAND);
//	void	set_complex(COMPLEX_TYPE v, int x, int y=0, int b=DEFAULT_BAND);

	void	set_image(Image *i) { img=i; 
			if(img) { bands=img->get_bands();
					  flgWrapState=img->get_data()->flgWrapState;
					  img->get_res(&xres,&yres); } }

	void	set_borders(int x1, int y1, int x2, int y2) {
			xmin=x1; ymin=y1; xmax=x2; ymax=y2;
			dxmin=(double)xmin; dymin=(double)ymin;
			dxmax=(double)xmax; dymax=(double)ymax; }

	void	set_borders(double x1, double y1, double x2, double y2) {
			dxmin=x1; dymin=y1; dxmax=x2; dymax=y2;
			xmin=(int)dxmin; ymin=(int)dymin;
			xmax=(int)dxmax; ymax=(int)dymax; }
	GeoData	*get_geo_data(void) {
			GeoData	*gd=NULL;
			if(img) {
				if(img->image_class_type() == GEOIMAGE) {
					gd = ((GeoImage *)img)->get_geo_data();
				}
			}
			return(gd);
		}


	cropData() { init(); }
	cropData(Image *i) { init(); set_image(i); }
	cropData(Image *i, int x1, int y1, int x2, int y2) {
		init(); set_image(i); set_borders(x1,y1,x2,y2); }
	cropData(Image *i, double x1, double y1, double x2, double y2) {
		init(); set_image(i); set_borders(x1,y1,x2,y2); }
	~cropData() { free(); }
	};


// ****************************** pyrpixData *******************************

class pyrpixData : public ImageData {

    protected:

	void	init() { img=NULL; }

	Image	*img;

    public:

	int	allocate(int x=0, int y=1, int b=1);
	void	free();
	int	add_band();
	int	remove_band(int b=LAST_BAND);
	int	clear(int b=ALL_BANDS);

	void	set_default_band(int b=0) { if(img) img->set_default_band(b); }
	void	set_red_band(int b=0) { if(img) img->set_red_band(b); }
	void	set_green_band(int b=1) { if(img) img->set_green_band(b); }
	void	set_blue_band(int b=2) { if(img) img->set_blue_band(b); }
	void	set_alpha_band(int b=-1) { if(img) img->set_alpha_band(b); }
	void	set_default_alpha(uchar a=255) { if(img) img->set_default_alpha(a); }
	void	set_rgb_bands(int r=0, int g=1, int b=2) {
				if(img) img->set_rgb_bands(r,g,b); }
	int	get_default_band() { if(!img) return -1;
					else return img->get_default_band(); }
	int	get_red_band() { if(!img) return -1;
					else return img->get_red_band(); }
	int	get_green_band() { if(!img) return -1;
					else return img->get_green_band(); }
	int	get_blue_band() { if(!img) return -1;
					else return img->get_blue_band(); }
	int	get_alpha_band() { if(!img) return -1;
					else return img->get_alpha_band(); }
	uchar	get_default_alpha() { if(!img) return 0;
					else return img->get_default_alpha(); }

	void	set_level(double l) { clev=l; }

	int	access() { if(!img) return 0; else return img->access(); }
	int	type() { return 112; }		// class id
	char	*type_name() { return (char *)"pyrpixData"; }
	int	preferred_data_type() { if(!img) return NO_DATA;
				else return img->preferred_data_type(); }

	int	get_bands() { if(img) return img->get_bands(); else return 0; }
	Image	*get_map() { if(img) return img->get_map(); else return NULL; }

	// For 24 bit color display:
	void	get_color(int x, int y, uchar *r, uchar *g, uchar *b,
			  uchar *a=NULL);
	void	iget_color(double x, double y, uchar *r, uchar *g, uchar *b,
			  uchar *a=NULL);
	void	set_color(int x, int y, uchar r, uchar g, uchar b,
			  uchar a=255);
	uchar	get_alpha(int x, int y);
	uchar	iget_alpha(double x, double y);
	void	set_alpha(int x, int y, uchar a=255);

	// For 16 bit XColor info:
//	ushort	get_red16(int x, int y=0);
//	ushort	get_green16(int x, int y=0);
//	ushort	get_blue16(int x, int y=0);

//	ushort	iget_red16(double x, double y=0.0);
//	ushort	iget_green16(double x, double y=0.0);
//	ushort	iget_blue16(double x, double y=0.0);

//	void	set_red16(ushort c, int x, int y=0);
//	void	set_green16(ushort c, int x, int y=0);
//	void	set_blue16(ushort c, int x, int y=0);

	// Data types:
//	int	get_bit(int x, int y=0, int b=DEFAULT_BAND);
//	char	get_char(int x, int y=0, int b=DEFAULT_BAND);
	uchar	get_uchar(int x, int y=0, int b=DEFAULT_BAND);
//	short	get_short(int x, int y=0, int b=DEFAULT_BAND);
//	ushort	get_ushort(int x, int y=0, int b=DEFAULT_BAND);
//	long	get_long(int x, int y=0, int b=DEFAULT_BAND);
//	ulong	get_ulong(int x, int y=0, int b=DEFAULT_BAND);
//	float	get_float(int x, int y=0, int b=DEFAULT_BAND);
//	double	get_double(int x, int y=0, int b=DEFAULT_BAND);
//	COMPLEX_TYPE get_complex(int x, int y=0, int b=DEFAULT_BAND);

//	int	iget_bit(double x, double y=0.0, int b=DEFAULT_BAND);
//	char	iget_char(double x, double y=0.0, int b=DEFAULT_BAND);
	uchar	iget_uchar(double x, double y=0.0, int b=DEFAULT_BAND);
//	short	iget_short(double x, double y=0.0, int b=DEFAULT_BAND);
//	ushort	iget_ushort(double x, double y=0.0, int b=DEFAULT_BAND);
//	long	iget_long(double x, double y=0.0, int b=DEFAULT_BAND);
//	ulong	iget_ulong(double x, double y=0.0, int b=DEFAULT_BAND);
//	float	iget_float(double x, double y=0.0, int b=DEFAULT_BAND);
//	double	iget_double(double x, double y=0.0, int b=DEFAULT_BAND);
//	COMPLEX_TYPE	iget_complex(double x, double y=0.0, int b=DEFAULT_BAND);

//	void	set_bit(int v, int x, int y=0, int b=DEFAULT_BAND);
//	void	set_char(char v, int x, int y=0, int b=DEFAULT_BAND);
//	void	set_uchar(uchar v, int x, int y=0, int b=DEFAULT_BAND);
//	void	set_short(short v, int x, int y=0, int b=DEFAULT_BAND);
//	void	set_ushort(ushort v, int x, int y=0, int b=DEFAULT_BAND);
//	void	set_long(long v, int x, int y=0, int b=DEFAULT_BAND);
//	void	set_ulong(ulong v, int x, int y=0, int b=DEFAULT_BAND);
//	void	set_float(float v, int x, int y=0, int b=DEFAULT_BAND);
//	void	set_double(double v, int x, int y=0, int b=DEFAULT_BAND);
//	void	set_complex(COMPLEX_TYPE v, int x, int y=0, int b=DEFAULT_BAND);

	void	set_image(Image *i) { img=i; 
			if(img) { bands=img->get_bands();
					  flgWrapState=img->get_data()->flgWrapState;
					  img->get_res(&xres,&yres); } }

	GeoData	*get_geo_data(void) {
			GeoData	*gd=NULL;
			if(img) {
				if(img->image_class_type() == GEOIMAGE) {
					gd = ((GeoImage *)img)->get_geo_data();
				}
			}
			return(gd);
		}

	pyrpixData() { init(); }
	pyrpixData(Image *i) { init(); set_image(i); }
	~pyrpixData() { free(); }
	};


// ****************************** doubleMap *******************************

class doubleMap : public ImageData {

    public:

	// Node in the value search tree:
	typedef struct	valnode {
			double	val;		// cutoff value
			struct valnode	*less;		// node for less than
			struct valnode	*geq;		// node for greater than or equal
			}valnode;

	// Cell for storing color info at a cutoff value:
	typedef struct	colcell {
			double	val;		// cutoff value
			uchar	lr,lg,lb;	// less than color
			uchar	gr,gg,gb;	// greater than color
			double	sr,sg,sb;	// stored double greater than col
			double	dr,dg,db;	// color multipliers
			valnode	*vnode;		// corresponding valnode
			}colcell;

    protected:

	Image	*img;
	int	num;
	colcell	*col;
	valnode	*root;

	void	init();
	void	alloc_col();
	void	free_col();
	void	free_tree();
	void	build_tree();
	valnode	*build_range(int n1, int n2);
	void	comp_col(int n);

    public:

	int	allocate(int x=0, int y=1, int b=3);
	void	free();
	int	add_band() { return 0; }
	int	remove_band(int) { return 0; }
	int	clear(int) { return 0; }

	int	access() { return(DATA_READ); }
	int	type() { return 104; }		// class id
	char	*type_name() { return (char *)"doubleMap"; }
	int	preferred_data_type() { if(!img) return NO_DATA;
				else return img->preferred_data_type(); }

	int	get_res(int *x=NULL, int *y=NULL, int *b=NULL) {
			if(img) return img->get_res(x,y,b);
			else { if(x) *x=0; if(y) *y=0; if(b) *b=0; return 0; } }
	int	get_bands() { if(img) return img->get_bands(); else return 0; }
	Image	*get_map() { if(img) return img->get_map(); else return NULL; }
	GeoData	*get_geo_data(void) {
			GeoData	*gd=NULL;
			if(img) {
				if(img->image_class_type() == GEOIMAGE) {
					gd = ((GeoImage *)img)->get_geo_data();
				}
			}
			return(gd);
		}


	void	set_default_band(int b=0) { if(img) img->set_default_band(b); }
	void	set_red_band(int b=0) { if(img) img->set_red_band(b); }
	void	set_green_band(int b=1) { if(img) img->set_green_band(b); }
	void	set_blue_band(int b=2) { if(img) img->set_blue_band(b); }
	void	set_alpha_band(int b=-1) { if(img) img->set_alpha_band(b); }
	void	set_default_alpha(uchar a=255) { if(img) img->set_default_alpha(a); }
	void	set_rgb_bands(int r=0, int g=1, int b=2) {
				if(img) img->set_rgb_bands(r,g,b); }
	int	get_default_band() { if(!img) return -1;
					else return img->get_default_band(); }
	int	get_red_band() { if(!img) return -1;
					else return img->get_red_band(); }
	int	get_green_band() { if(!img) return -1;
					else return img->get_green_band(); }
	int	get_blue_band() { if(!img) return -1;
					else return img->get_blue_band(); }
	int	get_alpha_band() { if(!img) return -1;
					else return img->get_alpha_band(); }
	uchar	get_default_alpha() { if(!img) return 0;
					else return img->get_default_alpha(); }

	void	set_level(double l) { if(img) img->set_level(l); }

	// For 24 bit color display:
	void	get_color(int x, int y, uchar *r, uchar *g, uchar *b,
			  uchar *a=NULL);
	void	iget_color(double x, double y, uchar *r, uchar *g, uchar *b,
			  uchar *a=NULL);
	void	set_color(int, int, uchar, uchar, uchar, uchar) { ; }
	uchar	get_alpha(int x, int y) { return img->get_alpha(x,y); }
	uchar	iget_alpha(double x, double y) { 
			return (img->get_data())->iget_alpha(x,y); }
	void	set_alpha(int, int, uchar) { ; }

	// For 16 bit XColor info:
//	ushort	get_red16(int x, int y=0);
//	ushort	get_green16(int x, int y=0);
//	ushort	get_blue16(int x, int y=0);

//	ushort	iget_red16(double x, double y=0.0);
//	ushort	iget_green16(double x, double y=0.0);
//	ushort	iget_blue16(double x, double y=0.0);

	void	set_red16(ushort, int, int) { ; }
	void	set_green16(ushort, int, int) { ; }
	void	set_blue16(ushort, int, int) { ; }

	// Data types: (implement as "see thru" functions)
	int	get_bit(int x, int y=0, int b=DEFAULT_BAND) {
			return (img->get_data())->get_bit(x,y,b); }
	char	get_char(int x, int y=0, int b=DEFAULT_BAND) {
			return (img->get_data())->get_char(x,y,b); }
	uchar	get_uchar(int x, int y=0, int b=DEFAULT_BAND) {
			return (img->get_data())->get_uchar(x,y,b); }
	short	get_short(int x, int y=0, int b=DEFAULT_BAND) {
			return (img->get_data())->get_short(x,y,b); }
	ushort	get_ushort(int x, int y=0, int b=DEFAULT_BAND) {
			return (img->get_data())->get_ushort(x,y,b); }
	long	get_long(int x, int y=0, int b=DEFAULT_BAND) {
			return (img->get_data())->get_long(x,y,b); }
	ulong	get_ulong(int x, int y=0, int b=DEFAULT_BAND) {
			return (img->get_data())->get_ulong(x,y,b); }
	float	get_float(int x, int y=0, int b=DEFAULT_BAND) {
			return (img->get_data())->get_float(x,y,b); }
	double	get_double(int x, int y=0, int b=DEFAULT_BAND) {
			return (img->get_data())->get_double(x,y,b); }
	COMPLEX_TYPE get_complex(int x, int y=0, int b=DEFAULT_BAND) {
			return (img->get_data())->get_complex(x,y,b); }

	int	iget_bit(double x, double y=0.0, int b=DEFAULT_BAND) {
			return (img->get_data())->iget_bit(x,y,b); }
	char	iget_char(double x, double y=0.0, int b=DEFAULT_BAND) {
			return (img->get_data())->iget_char(x,y,b); }
	uchar	iget_uchar(double x, double y=0.0, int b=DEFAULT_BAND) {
			return (img->get_data())->iget_uchar(x,y,b); }
	short	iget_short(double x, double y=0.0, int b=DEFAULT_BAND) {
			return (img->get_data())->iget_short(x,y,b); }
	ushort	iget_ushort(double x, double y=0.0, int b=DEFAULT_BAND) {
			return (img->get_data())->iget_ushort(x,y,b); }
	long	iget_long(double x, double y=0.0, int b=DEFAULT_BAND) {
			return (img->get_data())->iget_long(x,y,b); }
	ulong	iget_ulong(double x, double y=0.0, int b=DEFAULT_BAND) {
			return (img->get_data())->iget_ulong(x,y,b); }
	float	iget_float(double x, double y=0.0, int b=DEFAULT_BAND) {
			return (img->get_data())->iget_float(x,y,b); }
	double	iget_double(double x, double y=0.0, int b=DEFAULT_BAND) {
			return (img->get_data())->iget_double(x,y,b); }
	COMPLEX_TYPE	iget_complex(double x, double y=0.0, int b=DEFAULT_BAND) {
			return (img->get_data())->iget_complex(x,y,b); }

	void	set_bit(int, int, int, int) { ; }
	void	set_char(char, int, int, int) { ; }
	void	set_uchar(uchar, int, int, int) { ; }
	void	set_short(short, int, int, int) { ; }
	void	set_ushort(ushort, int, int, int) { ; }
	void	set_long(long, int, int, int) { ; }
	void	set_ulong(ulong, int, int, int) { ; }
	void	set_float(float, int, int, int) { ; }
	void	set_double(double, int, int, int) { ; }
	void	set_complex(COMPLEX_TYPE, int, int, int) { ; }

	void	set_image(Image *i) { img=i; }
	Image	*get_image() { return img; }

	int	get_num() { return num; }
	double	get_val(int n) { if(n>=0 && n<num) return col[n].val; else return 0.0; }
	void	set_val(int n, double v);
	void	add_val(double val);
	void	add_val(double val, uchar lr, uchar lg, uchar lb,
				    uchar gr, uchar gg, uchar gb);
	void	set_lcolor(int n, uchar r, uchar g, uchar b);
	void	set_gcolor(int n, uchar r, uchar g, uchar b);
	void	set_data(int n, double v, uchar lr, uchar lg, uchar lb,
					  uchar gr, uchar gg, uchar gb);
	void	get_lcolor(int n, uchar *r, uchar *g, uchar *b) {
			if(n>=0 && n<num) {
				*r=col[n].lr; *g=col[n].lg; *b=col[n].lb; } }
	void	get_gcolor(int n, uchar *r, uchar *g, uchar *b) {
			if(n>=0 && n<num) {
				*r=col[n].gr; *g=col[n].gg; *b=col[n].gb; } }
	void	get_data(int n, double *v, uchar *lr, uchar *lg, uchar *lb,
					   uchar *gr, uchar *gg, uchar *gb) {
			if(n>=0 && n<num) { *v=col[n].val;
				*lr=col[n].lr; *lg=col[n].lg; *lb=col[n].lb;
				*gr=col[n].gr; *gg=col[n].gg; *gb=col[n].gb; } }
	void	get_vcolor(double v, uchar *r, uchar *g, uchar *b);
	void	remove(int n);
	void	remove_all();

	void	copy(doubleMap &dm);
	void	operator=(doubleMap &dm) { copy(dm); }

	doubleMap() { init(); }
	doubleMap(Image *i) { init(); set_image(i); }
	~doubleMap() { remove_all(); free(); }
	};


// ****************************** XData *******************************

class XData : public ImageData {

    public:

	int	allocate(int x=0, int y=1, int b=1) { xres=x; yres=y; bands=b;
							return 1;}
	void	free() { xres=0; yres=0; bands=0; }
	int	add_band() { bands++; return 1; }
	int	remove_band(int) { if(bands>0) bands--; return 1; }
	int	clear(int) { return 0; }

	int	access() { return(DATA_READ); }
	int	type() { return 108; }		// class id
	char	*type_name() { return (char *)"XData"; }
	int	preferred_data_type() { return DOUBLE_DATA; }

	// For 24 bit color display:
	void	set_color(int, int, uchar, uchar, uchar, uchar) { ; }
	uchar	get_alpha(int, int) { return default_alpha; }
	uchar	iget_alpha(double, double) { return default_alpha; }
	void	set_alpha(int, int, uchar) { ; }

	// For 16 bit XColor info:
	void	set_red16(ushort, int, int) { ; }
	void	set_green16(ushort, int, int) { ; }
	void	set_blue16(ushort, int, int) { ; }

	// Data types:
	int	get_bit(int x, int, int) { return x&1; }
	char	get_char(int x, int, int) { return (char)x; }
	uchar	get_uchar(int x, int, int) { return (uchar)x; }
	short	get_short(int x, int, int) { return (short)x; }
	ushort	get_ushort(int x, int, int) { return (ushort)x; }
	long	get_long(int x, int, int) { return (long)x; }
	ulong	get_ulong(int x, int, int) { return (ulong)x; }
	float	get_float(int x, int, int) { return (float)x; }
	double	get_double(int x, int, int) { return (double)x; }
	COMPLEX_TYPE get_complex(int x, int y, int) { return COMPLEX_TYPE((double)x,(double)y); }

	int	iget_bit(double x, double, int) { return (int)x&1; }
	char	iget_char(double x, double, int) { return (char)x; }
	uchar	iget_uchar(double x, double, int) { return (uchar)x; }
	short	iget_short(double x, double, int) { return (short)x; }
	ushort	iget_ushort(double x, double, int) { return (ushort)x; }
	long	iget_long(double x, double, int) { return (long)x; }
	ulong	iget_ulong(double x, double, int) { return (ulong)x; }
	float	iget_float(double x, double, int) { return (float)x; }
	double	iget_double(double x, double, int) { return x; }
	COMPLEX_TYPE	iget_complex(double x, double y, int) { return COMPLEX_TYPE(x,y); }

	void	set_bit(int, int, int, int) { ; }
	void	set_char(char, int, int, int) { ; }
	void	set_uchar(uchar, int, int, int) { ; }
	void	set_short(short, int, int, int) { ; }
	void	set_ushort(ushort, int, int, int) { ; }
	void	set_long(long, int, int, int) { ; }
	void	set_ulong(ulong, int, int, int) { ; }
	void	set_float(float, int, int, int) { ; }
	void	set_double(double, int, int, int) { ; }
	void	set_complex(COMPLEX_TYPE, int, int, int) { ; }
	};


// ****************************** pyrData *******************************

class pyrData : public ImageData {

    protected:

	void	init();

	Image	**img;

	int	lev_int;
	double	lev_frac;

    public:

	int	allocate(int x=0, int y=1, int b=1);
	void	free();
	int	add_band() { return 0; }
	int	remove_band(int) { return 0; }
	int	clear(int) { return 0; }

	int	access() { return(DATA_READ|DATA_WRITE); }
	int	type() { return 109; }		// class id
	char	*type_name() { return (char *)"pyrData"; }
	int	preferred_data_type() { return UCHAR_DATA; }
	GeoData	*get_geo_data(void) {
			GeoData	*gd=NULL;
			if(img) {
				if(img[0]) {
					if(img[0]->image_class_type() == GEOIMAGE) {
						gd = ((GeoImage *)img[0])->get_geo_data();
					}
				}
			}
			return(gd);
		}


	void	set_level(double l) { clev=pyr_lev(l);
			lev_int=(int)clev; lev_frac=clev-(double)lev_int; }

	// For 24 bit color display:
	void	get_color(int x, int y, uchar *r, uchar *g, uchar *b,
			  uchar *a=NULL);
	void	iget_color(double x, double y, uchar *r, uchar *g, uchar *b,
			  uchar *a=NULL);
	void	set_color(int, int, uchar, uchar, uchar, uchar);
//	uchar	get_alpha(int x, int y);
//	uchar	iget_alpha(double x, double y);
//	void	set_alpha(int, int, uchar);

	// For 16 bit XColor info:
//	ushort	get_red16(int x, int y=0);
//	ushort	get_green16(int x, int y=0);
//	ushort	get_blue16(int x, int y=0);

//	ushort	iget_red16(double x, double y=0.0);
//	ushort	iget_green16(double x, double y=0.0);
//	ushort	iget_blue16(double x, double y=0.0);

//	void	set_red16(ushort, int, int);
//	void	set_green16(ushort, int, int);
//	void	set_blue16(ushort, int, int);

	// Data types:
	int	get_bit(int x, int y=0, int b=DEFAULT_BAND) { return (img[lev_int]->get_data())->get_bit(x>>lev_int,y>>lev_int,b); }
	char	get_char(int x, int y=0, int b=DEFAULT_BAND) { return (img[lev_int]->get_data())->get_char(x>>lev_int,y>>lev_int,b); }
	uchar	get_uchar(int x, int y=0, int b=DEFAULT_BAND) { return (img[lev_int]->get_data())->get_uchar(x>>lev_int,y>>lev_int,b); }
	short	get_short(int x, int y=0, int b=DEFAULT_BAND) { return (img[lev_int]->get_data())->get_short(x>>lev_int,y>>lev_int,b); }
	ushort	get_ushort(int x, int y=0, int b=DEFAULT_BAND) { return (img[lev_int]->get_data())->get_ushort(x>>lev_int,y>>lev_int,b); }
	long	get_long(int x, int y=0, int b=DEFAULT_BAND) { return (img[lev_int]->get_data())->get_long(x>>lev_int,y>>lev_int,b); }
	ulong	get_ulong(int x, int y=0, int b=DEFAULT_BAND) { return (img[lev_int]->get_data())->get_ulong(x>>lev_int,y>>lev_int,b); }
	float	get_float(int x, int y=0, int b=DEFAULT_BAND) { return (img[lev_int]->get_data())->get_float(x>>lev_int,y>>lev_int,b); }
	double	get_double(int x, int y=0, int b=DEFAULT_BAND) { return (img[lev_int]->get_data())->get_double(x>>lev_int,y>>lev_int,b); }
	COMPLEX_TYPE get_complex(int x, int y=0, int b=DEFAULT_BAND) { return (img[lev_int]->get_data())->get_complex(x>>lev_int,y>>lev_int,b); }

	int	iget_bit(double x, double y=0.0, int b=DEFAULT_BAND);
	char	iget_char(double x, double y=0.0, int b=DEFAULT_BAND);
	uchar	iget_uchar(double x, double y=0.0, int b=DEFAULT_BAND);
	short	iget_short(double x, double y=0.0, int b=DEFAULT_BAND);
	ushort	iget_ushort(double x, double y=0.0, int b=DEFAULT_BAND);
	long	iget_long(double x, double y=0.0, int b=DEFAULT_BAND);
	ulong	iget_ulong(double x, double y=0.0, int b=DEFAULT_BAND);
	float	iget_float(double x, double y=0.0, int b=DEFAULT_BAND);
	double	iget_double(double x, double y=0.0, int b=DEFAULT_BAND);
	COMPLEX_TYPE	iget_complex(double x, double y=0.0, int b=DEFAULT_BAND);

	void	set_bit(int v, int x, int y=0, int b=DEFAULT_BAND) { (img[lev_int]->get_data())->set_bit(v,x>>lev_int,y>>lev_int,b); }
	void	set_char(char v, int x, int y=0, int b=DEFAULT_BAND) { (img[lev_int]->get_data())->set_char(v,x>>lev_int,y>>lev_int,b); }
	void	set_uchar(uchar v, int x, int y=0, int b=DEFAULT_BAND) { (img[lev_int]->get_data())->set_uchar(v,x>>lev_int,y>>lev_int,b); }
	void	set_short(short v, int x, int y=0, int b=DEFAULT_BAND) { (img[lev_int]->get_data())->set_short(v,x>>lev_int,y>>lev_int,b); }
	void	set_ushort(ushort v, int x, int y=0, int b=DEFAULT_BAND) { (img[lev_int]->get_data())->set_ushort(v,x>>lev_int,y>>lev_int,b); }
	void	set_long(long v, int x, int y=0, int b=DEFAULT_BAND) { (img[lev_int]->get_data())->set_long(v,x>>lev_int,y>>lev_int,b); }
	void	set_ulong(ulong v, int x, int y=0, int b=DEFAULT_BAND) { (img[lev_int]->get_data())->set_ulong(v,x>>lev_int,y>>lev_int,b); }
	void	set_float(float v, int x, int y=0, int b=DEFAULT_BAND) { (img[lev_int]->get_data())->set_float(v,x>>lev_int,y>>lev_int,b); }
	void	set_double(double v, int x, int y=0, int b=DEFAULT_BAND) { (img[lev_int]->get_data())->set_double(v,x>>lev_int,y>>lev_int,b); }
	void	set_complex(COMPLEX_TYPE v, int x, int y=0, int b=DEFAULT_BAND) { (img[lev_int]->get_data())->set_complex(v,x>>lev_int,y>>lev_int,b); }

	void	add_image(Image *i, int p=-1);
	void	remove_image(int p=-1);
	Image	*get_image(int p=-1) { if(p==-1) p=bands-1;
			if(p<0 || p>=levels) return NULL; else return img[p]; }
	Image	**get_image_list(int *n=NULL) { if(n) *n=levels; return img; }

	pyrData() { init(); }
	pyrData(Image *i1, Image *i2=NULL) { init(); add_image(i1);
			if(i2) add_image(i2); }
	pyrData(Image **i);
	pyrData(Image **i, int n);
	~pyrData();
	};


// ****************************** interpData *******************************

class interpData : public ImageData {

    protected:

	void	init() { img=NULL; }

	Image	*img;

    public:

	int	allocate(int x=0, int y=1, int b=1);
	void	free();
	int	add_band();
	int	remove_band(int b=LAST_BAND);
	int	clear(int b=ALL_BANDS);

	void	set_default_band(int b=0) { if(img) img->set_default_band(b); }
	void	set_red_band(int b=0) { if(img) img->set_red_band(b); }
	void	set_green_band(int b=1) { if(img) img->set_green_band(b); }
	void	set_blue_band(int b=2) { if(img) img->set_blue_band(b); }
	void	set_alpha_band(int b=-1) { if(img) img->set_alpha_band(b); }
	void	set_default_alpha(uchar a=255) { if(img) img->set_default_alpha(a); }
	void	set_rgb_bands(int r=0, int g=1, int b=2) {
				if(img) img->set_rgb_bands(r,g,b); }
	int	get_default_band() { if(!img) return -1;
					else return img->get_default_band(); }
	int	get_red_band() { if(!img) return -1;
					else return img->get_red_band(); }
	int	get_green_band() { if(!img) return -1;
					else return img->get_green_band(); }
	int	get_blue_band() { if(!img) return -1;
					else return img->get_blue_band(); }
	int	get_alpha_band() { if(!img) return -1;
					else return img->get_alpha_band(); }
	uchar	get_default_alpha() { if(!img) return 0;
					else return img->get_default_alpha(); }

	void	set_level(double l) { if(img) img->set_level(l); }

	int	access() { if(!img) return 0; else return img->access(); }
	int	type() { return 113; }		// class id
	char	*type_name() { return (char *)"interpData"; }
	int	preferred_data_type() { if(!img) return NO_DATA;
				else return img->preferred_data_type(); }

	int	get_bands() { if(img) return img->get_bands(); else return 0; }
	Image	*get_map() { if(img) return img->get_map(); else return NULL; }
	GeoData	*get_geo_data(void) {
			GeoData	*gd=NULL;
			if(img) {
				if(img->image_class_type() == GEOIMAGE) {
					gd = ((GeoImage *)img)->get_geo_data();
				}
			}
			return(gd);
		}


	// For 24 bit color display:
	void	get_color(int x, int y, uchar *r, uchar *g, uchar *b,
			  uchar *a=NULL);
	void	iget_color(double x, double y, uchar *r, uchar *g, uchar *b,
			  uchar *a=NULL);
	void	set_color(int x, int y, uchar r, uchar g, uchar b,
			  uchar a=255);
	uchar	get_alpha(int x, int y);
	uchar	iget_alpha(double x, double y);
	void	set_alpha(int x, int y, uchar a=255);

	// For 16 bit XColor info:
//	ushort	get_red16(int x, int y=0);
//	ushort	get_green16(int x, int y=0);
//	ushort	get_blue16(int x, int y=0);

//	ushort	iget_red16(double x, double y=0.0);
//	ushort	iget_green16(double x, double y=0.0);
//	ushort	iget_blue16(double x, double y=0.0);

//	void	set_red16(ushort c, int x, int y=0);
//	void	set_green16(ushort c, int x, int y=0);
//	void	set_blue16(ushort c, int x, int y=0);

	// Data types:
	int	get_bit(int x, int y=0, int b=DEFAULT_BAND) { return (img->get_data())->iget_bit((double)x,(double)y,b); }
	char	get_char(int x, int y=0, int b=DEFAULT_BAND) { return (img->get_data())->iget_char((double)x,(double)y,b); }
	uchar	get_uchar(int x, int y=0, int b=DEFAULT_BAND) { return (img->get_data())->iget_uchar((double)x,(double)y,b); }
	short	get_short(int x, int y=0, int b=DEFAULT_BAND) { return (img->get_data())->iget_short((double)x,(double)y,b); }
	ushort	get_ushort(int x, int y=0, int b=DEFAULT_BAND) { return (img->get_data())->iget_ushort((double)x,(double)y,b); }
	long	get_long(int x, int y=0, int b=DEFAULT_BAND) { return (img->get_data())->iget_long((double)x,(double)y,b); }
	ulong	get_ulong(int x, int y=0, int b=DEFAULT_BAND) { return (img->get_data())->iget_ulong((double)x,(double)y,b); }
	float	get_float(int x, int y=0, int b=DEFAULT_BAND) { return (img->get_data())->iget_float((double)x,(double)y,b); }
	double	get_double(int x, int y=0, int b=DEFAULT_BAND) { return (img->get_data())->iget_double((double)x,(double)y,b); }
	COMPLEX_TYPE get_complex(int x, int y=0, int b=DEFAULT_BAND) { return (img->get_data())->iget_complex((double)x,(double)y,b); }

	int	iget_bit(double x, double y=0.0, int b=DEFAULT_BAND) { return (img->get_data())->iget_bit(x,y,b); }
	char	iget_char(double x, double y=0.0, int b=DEFAULT_BAND) { return (img->get_data())->iget_char(x,y,b); }
	uchar	iget_uchar(double x, double y=0.0, int b=DEFAULT_BAND) { return (img->get_data())->iget_uchar(x,y,b); }
	short	iget_short(double x, double y=0.0, int b=DEFAULT_BAND) { return (img->get_data())->iget_short(x,y,b); }
	ushort	iget_ushort(double x, double y=0.0, int b=DEFAULT_BAND) { return (img->get_data())->iget_ushort(x,y,b); }
	long	iget_long(double x, double y=0.0, int b=DEFAULT_BAND) { return (img->get_data())->iget_long(x,y,b); }
	ulong	iget_ulong(double x, double y=0.0, int b=DEFAULT_BAND) { return (img->get_data())->iget_ulong(x,y,b); }
	float	iget_float(double x, double y=0.0, int b=DEFAULT_BAND) { return (img->get_data())->iget_float(x,y,b); }
	double	iget_double(double x, double y=0.0, int b=DEFAULT_BAND) { return (img->get_data())->iget_double(x,y,b); }
	COMPLEX_TYPE	iget_complex(double x, double y=0.0, int b=DEFAULT_BAND) { return (img->get_data())->iget_complex(x,y,b); }

//	void	set_bit(int v, int x, int y=0, int b=DEFAULT_BAND);
//	void	set_char(char v, int x, int y=0, int b=DEFAULT_BAND);
//	void	set_uchar(uchar v, int x, int y=0, int b=DEFAULT_BAND);
//	void	set_short(short v, int x, int y=0, int b=DEFAULT_BAND);
//	void	set_ushort(ushort v, int x, int y=0, int b=DEFAULT_BAND);
//	void	set_long(long v, int x, int y=0, int b=DEFAULT_BAND);
//	void	set_ulong(ulong v, int x, int y=0, int b=DEFAULT_BAND);
//	void	set_float(float v, int x, int y=0, int b=DEFAULT_BAND);
//	void	set_double(double v, int x, int y=0, int b=DEFAULT_BAND);
//	void	set_complex(COMPLEX_TYPE v, int x, int y=0, int b=DEFAULT_BAND);

	void	set_image(Image *i) { img=i; 
			if(img) { bands=img->get_bands();
		      flgWrapState=img->get_data()->flgWrapState;
					  if(!xres||!yres) img->get_res(&xres,&yres); } }
	Image	*get_image() { return img; }

	interpData() { init(); }
	interpData(Image *i) { init(); set_image(i); }
	~interpData() { free(); }
	};


#endif // _DATAMOD_H_
