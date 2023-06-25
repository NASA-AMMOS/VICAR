// datatypes.h
//
// Written by Dave Kagels 10/11/94

#ifndef _DATATYPES_H_
#define _DATATYPES_H_

#include "image/imagedata.h"

#ifndef _NO_IMAGEDISP_
//
// This conditionally-compiled code is only to support the ximageData class
//
#include <X11/X.h>
#include <X11/Xlib.h>
#include <X11/Intrinsic.h>
#endif // _NO_IMAGEDISP_


// ****************************** bitData *******************************

class bitData : public ImageData {

    protected:

	uchar	**data;
	int	xlen;

	void	init();

	uchar	gbit(int x, int y, int b) {
			return (data[b][(x>>3)+y*xlen]>>(x&7))&1; }
	void	sbit(int v, int x, int y, int b) {
			if(v)	data[b][(x>>3)+y*xlen]|=1<<(x&7);
			else	data[b][(x>>3)+y*xlen]&=~(1<<(x&7));
			}

    public:

	int	allocate(int x=0, int y=1, int b=1);
	void	free();
	int	add_band();
	int	remove_band(int b=LAST_BAND);
//	int	clear(int b=ALL_BANDS);

	int	access() { return(DATA_READ|DATA_WRITE); }
	int	type() { return 1; }		// class id
	char	*type_name() { return (char *)"bitData"; }
	int	preferred_data_type() { return BIT_DATA; }

	virtual	uchar	*get_data(int b=DEFAULT_BAND);

	// For 24 bit color display:
	void	get_color(int x, int y, uchar *r, uchar *g, uchar *b,
			  uchar *a=NULL);
//	void	iget_color(double x, double y, uchar *r, uchar *g, uchar *b,
//			  uchar *a=NULL);
	void	set_color(int x, int y, uchar r, uchar g, uchar b,
			  uchar a=255);
	uchar	get_alpha(int x, int y);
//	uchar	iget_alpha(double x, double y);
	void	set_alpha(int x, int y, uchar a=255);

	// For 16 bit XColor info:
	ushort	get_red16(int x, int y=0);
	ushort	get_green16(int x, int y=0);
	ushort	get_blue16(int x, int y=0);

//	ushort	iget_red16(double x, double y=0.0);
//	ushort	iget_green16(double x, double y=0.0);
//	ushort	iget_blue16(double x, double y=0.0);

	void	set_red16(ushort c, int x, int y=0);
	void	set_green16(ushort c, int x, int y=0);
	void	set_blue16(ushort c, int x, int y=0);

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

//	int	iget_bit(double x, double y=0.0, int b=DEFAULT_BAND);
//	char	iget_char(double x, double y=0.0, int b=DEFAULT_BAND);
//	uchar	iget_uchar(double x, double y=0.0, int b=DEFAULT_BAND);
//	short	iget_short(double x, double y=0.0, int b=DEFAULT_BAND);
//	ushort	iget_ushort(double x, double y=0.0, int b=DEFAULT_BAND);
//	long	iget_long(double x, double y=0.0, int b=DEFAULT_BAND);
//	ulong	iget_ulong(double x, double y=0.0, int b=DEFAULT_BAND);
//	float	iget_float(double x, double y=0.0, int b=DEFAULT_BAND);
//	double	iget_double(double x, double y=0.0, int b=DEFAULT_BAND);
//	COMPLEX_TYPE	iget_complex(double x, double y=0.0, int b=DEFAULT_BAND);

	void	set_bit(int v, int x, int y=0, int b=DEFAULT_BAND);
	void	set_char(char v, int x, int y=0, int b=DEFAULT_BAND);
	void	set_uchar(uchar v, int x, int y=0, int b=DEFAULT_BAND);
	void	set_short(short v, int x, int y=0, int b=DEFAULT_BAND);
	void	set_ushort(ushort v, int x, int y=0, int b=DEFAULT_BAND);
	void	set_long(long v, int x, int y=0, int b=DEFAULT_BAND);
	void	set_ulong(ulong v, int x, int y=0, int b=DEFAULT_BAND);
	void	set_float(float v, int x, int y=0, int b=DEFAULT_BAND);
	void	set_double(double v, int x, int y=0, int b=DEFAULT_BAND);
	void	set_complex(COMPLEX_TYPE v, int x, int y=0, int b=DEFAULT_BAND);

	bitData() { init(); }
	bitData(int x, int y=1, int b=1) { init(); allocate(x,y,b); }
	~bitData() { if(clean) free(); }
	};


// ****************************** charData ********************************

class charData : public ImageData {

    protected:

	char	**data;

	void	init();

    public:

	int	allocate(int x=0, int y=1, int b=1);
	void	free();
	int	add_band();
	int	remove_band(int b=LAST_BAND);
//	int	clear(int b=ALL_BANDS);

	int	access() { return(DATA_READ|DATA_WRITE); }
	int	type() { return 2; }		// class id
	char	*type_name() { return (char *)"charData"; }
	int	preferred_data_type() { return CHAR_DATA; }

	virtual	char	*get_data(int b=DEFAULT_BAND);

	// For 24 bit color display:
	void	get_color(int x, int y, uchar *r, uchar *g, uchar *b,
			  uchar *a=NULL);
//	void	iget_color(double x, double y, uchar *r, uchar *g, uchar *b,
//			  uchar *a=NULL);
	void	set_color(int x, int y, uchar r, uchar g, uchar b,
			  uchar a=255);
	uchar	get_alpha(int x, int y);
//	uchar	iget_alpha(double x, double y);
	void	set_alpha(int x, int y, uchar a=255);

	// For 16 bit XColor info:
	ushort	get_red16(int x, int y=0);
	ushort	get_green16(int x, int y=0);
	ushort	get_blue16(int x, int y=0);

//	ushort	iget_red16(double x, double y=0.0);
//	ushort	iget_green16(double x, double y=0.0);
//	ushort	iget_blue16(double x, double y=0.0);

	void	set_red16(ushort c, int x, int y=0);
	void	set_green16(ushort c, int x, int y=0);
	void	set_blue16(ushort c, int x, int y=0);

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

//	int	iget_bit(double x, double y=0.0, int b=DEFAULT_BAND);
//	char	iget_char(double x, double y=0.0, int b=DEFAULT_BAND);
//	uchar	iget_uchar(double x, double y=0.0, int b=DEFAULT_BAND);
//	short	iget_short(double x, double y=0.0, int b=DEFAULT_BAND);
//	ushort	iget_ushort(double x, double y=0.0, int b=DEFAULT_BAND);
//	long	iget_long(double x, double y=0.0, int b=DEFAULT_BAND);
//	ulong	iget_ulong(double x, double y=0.0, int b=DEFAULT_BAND);
//	float	iget_float(double x, double y=0.0, int b=DEFAULT_BAND);
//	double	iget_double(double x, double y=0.0, int b=DEFAULT_BAND);
//	COMPLEX_TYPE	iget_complex(double x, double y=0.0, int b=DEFAULT_BAND);

	void	set_bit(int v, int x, int y=0, int b=DEFAULT_BAND);
	void	set_char(char v, int x, int y=0, int b=DEFAULT_BAND);
	void	set_uchar(uchar v, int x, int y=0, int b=DEFAULT_BAND);
	void	set_short(short v, int x, int y=0, int b=DEFAULT_BAND);
	void	set_ushort(ushort v, int x, int y=0, int b=DEFAULT_BAND);
	void	set_long(long v, int x, int y=0, int b=DEFAULT_BAND);
	void	set_ulong(ulong v, int x, int y=0, int b=DEFAULT_BAND);
	void	set_float(float v, int x, int y=0, int b=DEFAULT_BAND);
	void	set_double(double v, int x, int y=0, int b=DEFAULT_BAND);
	void	set_complex(COMPLEX_TYPE v, int x, int y=0, int b=DEFAULT_BAND);

	charData() { init(); }
	charData(int x, int y=1, int b=1) { init(); allocate(x,y,b); }
	~charData() { if(clean) free(); }
	};


// ****************************** ucharData *******************************

class ucharData : public ImageData {

    protected:

	uchar	**data;

	void	init();

    public:

	int	allocate(int x=0, int y=1, int b=1);
	void	free();
	int	add_band();
	int	remove_band(int b=LAST_BAND);
//	int	clear(int b=ALL_BANDS);

	int	access() { return(DATA_READ|DATA_WRITE); }
	int	type() { return 3; }		// class id
	char	*type_name() { return (char *)"ucharData"; }
	int	preferred_data_type() { return UCHAR_DATA; }

	virtual	uchar	*get_data(int b=DEFAULT_BAND);

	// For 24 bit color display:
	void	get_color(int x, int y, uchar *r, uchar *g, uchar *b,
			  uchar *a=NULL);
	void	get_color_row(int x, int y, int length, uchar *r, uchar *g, uchar *b,
			  uchar *a=NULL);
	void	iget_color(double x, double y, uchar *r, uchar *g, uchar *b,
			  uchar *a=NULL);
	void	iget_color_row(double x, double y, double xstep, int length, uchar *r, uchar *g, uchar *b,
			  uchar *a=NULL);
	void	set_color(int x, int y, uchar r, uchar g, uchar b,
			  uchar a=255);
	uchar	get_alpha(int x, int y);
//	uchar	iget_alpha(double x, double y);
	void	set_alpha(int x, int y, uchar a=255);

	// For 16 bit XColor info:
	ushort	get_red16(int x, int y=0);
	ushort	get_green16(int x, int y=0);
	ushort	get_blue16(int x, int y=0);

//	ushort	iget_red16(double x, double y=0.0);
//	ushort	iget_green16(double x, double y=0.0);
//	ushort	iget_blue16(double x, double y=0.0);

	void	set_red16(ushort c, int x, int y=0);
	void	set_green16(ushort c, int x, int y=0);
	void	set_blue16(ushort c, int x, int y=0);

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

//	int	iget_bit(double x, double y=0.0, int b=DEFAULT_BAND);
//	char	iget_char(double x, double y=0.0, int b=DEFAULT_BAND);
//	uchar	iget_uchar(double x, double y=0.0, int b=DEFAULT_BAND);
//	short	iget_short(double x, double y=0.0, int b=DEFAULT_BAND);
//	ushort	iget_ushort(double x, double y=0.0, int b=DEFAULT_BAND);
//	long	iget_long(double x, double y=0.0, int b=DEFAULT_BAND);
//	ulong	iget_ulong(double x, double y=0.0, int b=DEFAULT_BAND);
//	float	iget_float(double x, double y=0.0, int b=DEFAULT_BAND);
//	double	iget_double(double x, double y=0.0, int b=DEFAULT_BAND);
//	COMPLEX_TYPE	iget_complex(double x, double y=0.0, int b=DEFAULT_BAND);

	void	set_bit(int v, int x, int y=0, int b=DEFAULT_BAND);
	void	set_char(char v, int x, int y=0, int b=DEFAULT_BAND);
	void	set_uchar(uchar v, int x, int y=0, int b=DEFAULT_BAND);
	void	set_short(short v, int x, int y=0, int b=DEFAULT_BAND);
	void	set_ushort(ushort v, int x, int y=0, int b=DEFAULT_BAND);
	void	set_long(long v, int x, int y=0, int b=DEFAULT_BAND);
	void	set_ulong(ulong v, int x, int y=0, int b=DEFAULT_BAND);
	void	set_float(float v, int x, int y=0, int b=DEFAULT_BAND);
	void	set_double(double v, int x, int y=0, int b=DEFAULT_BAND);
	void	set_complex(COMPLEX_TYPE v, int x, int y=0, int b=DEFAULT_BAND);

	ucharData() { init(); }
	ucharData(int x, int y=1, int b=1) { init(); allocate(x,y,b); }
	~ucharData() { if(clean) free(); }
	};


// ****************************** shortData ********************************

class shortData : public ImageData {

    protected:

	short	**data;

	void	init();

    public:

	int	allocate(int x=0, int y=1, int b=1);
	void	free();
	int	add_band();
	int	remove_band(int b=LAST_BAND);
//	int	clear(int b=ALL_BANDS);

	int	access() { return(DATA_READ|DATA_WRITE); }
	int	type() { return 4; }		// class id
	char	*type_name() { return (char *)"shortData"; }
	int	preferred_data_type() { return SHORT_DATA; }

	virtual	short	*get_data(int b=DEFAULT_BAND);

	// For 24 bit color display:
	void	get_color(int x, int y, uchar *r, uchar *g, uchar *b,
			  uchar *a=NULL);
//	void	iget_color(double x, double y, uchar *r, uchar *g, uchar *b,
//			  uchar *a=NULL);
	void	set_color(int x, int y, uchar r, uchar g, uchar b,
			  uchar a=255);
	uchar	get_alpha(int x, int y);
//	uchar	iget_alpha(double x, double y);
	void	set_alpha(int x, int y, uchar a=255);

	// For 16 bit XColor info:
	ushort	get_red16(int x, int y=0);
	ushort	get_green16(int x, int y=0);
	ushort	get_blue16(int x, int y=0);

//	ushort	iget_red16(double x, double y=0.0);
//	ushort	iget_green16(double x, double y=0.0);
//	ushort	iget_blue16(double x, double y=0.0);

	void	set_red16(ushort c, int x, int y=0);
	void	set_green16(ushort c, int x, int y=0);
	void	set_blue16(ushort c, int x, int y=0);

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

//	int	iget_bit(double x, double y=0.0, int b=DEFAULT_BAND);
//	char	iget_char(double x, double y=0.0, int b=DEFAULT_BAND);
//	uchar	iget_uchar(double x, double y=0.0, int b=DEFAULT_BAND);
//	short	iget_short(double x, double y=0.0, int b=DEFAULT_BAND);
//	ushort	iget_ushort(double x, double y=0.0, int b=DEFAULT_BAND);
//	long	iget_long(double x, double y=0.0, int b=DEFAULT_BAND);
//	ulong	iget_ulong(double x, double y=0.0, int b=DEFAULT_BAND);
//	float	iget_float(double x, double y=0.0, int b=DEFAULT_BAND);
//	double	iget_double(double x, double y=0.0, int b=DEFAULT_BAND);
//	COMPLEX_TYPE	iget_complex(double x, double y=0.0, int b=DEFAULT_BAND);

	void	set_bit(int v, int x, int y=0, int b=DEFAULT_BAND);
	void	set_char(char v, int x, int y=0, int b=DEFAULT_BAND);
	void	set_uchar(uchar v, int x, int y=0, int b=DEFAULT_BAND);
	void	set_short(short v, int x, int y=0, int b=DEFAULT_BAND);
	void	set_ushort(ushort v, int x, int y=0, int b=DEFAULT_BAND);
	void	set_long(long v, int x, int y=0, int b=DEFAULT_BAND);
	void	set_ulong(ulong v, int x, int y=0, int b=DEFAULT_BAND);
	void	set_float(float v, int x, int y=0, int b=DEFAULT_BAND);
	void	set_double(double v, int x, int y=0, int b=DEFAULT_BAND);
	void	set_complex(COMPLEX_TYPE v, int x, int y=0, int b=DEFAULT_BAND);

	shortData() { init(); }
	shortData(int x, int y=1, int b=1) { init(); allocate(x,y,b); }
	~shortData() { if(clean) free(); }
	};


// ****************************** ushortData ********************************

class ushortData : public ImageData {

    protected:

	ushort	**data;

	void	init();

    public:

	int	allocate(int x=0, int y=1, int b=1);
	void	free();
	int	add_band();
	int	remove_band(int b=LAST_BAND);
//	int	clear(int b=ALL_BANDS);

	int	access() { return(DATA_READ|DATA_WRITE); }
	int	type() { return 5; }		// class id
	char	*type_name() { return (char *)"ushortData"; }
	int	preferred_data_type() { return USHORT_DATA; }

	virtual	ushort	*get_data(int b=DEFAULT_BAND);

	// For 24 bit color display:
	void	get_color(int x, int y, uchar *r, uchar *g, uchar *b,
			  uchar *a=NULL);
//	void	iget_color(double x, double y, uchar *r, uchar *g, uchar *b,
//			  uchar *a=NULL);
	void	set_color(int x, int y, uchar r, uchar g, uchar b,
			  uchar a=255);
	uchar	get_alpha(int x, int y);
//	uchar	iget_alpha(double x, double y);
	void	set_alpha(int x, int y, uchar a=255);

	// For 16 bit XColor info:
	ushort	get_red16(int x, int y=0);
	ushort	get_green16(int x, int y=0);
	ushort	get_blue16(int x, int y=0);

//	ushort	iget_red16(double x, double y=0.0);
//	ushort	iget_green16(double x, double y=0.0);
//	ushort	iget_blue16(double x, double y=0.0);

	void	set_red16(ushort c, int x, int y=0);
	void	set_green16(ushort c, int x, int y=0);
	void	set_blue16(ushort c, int x, int y=0);

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

//	int	iget_bit(double x, double y=0.0, int b=DEFAULT_BAND);
//	char	iget_char(double x, double y=0.0, int b=DEFAULT_BAND);
//	uchar	iget_uchar(double x, double y=0.0, int b=DEFAULT_BAND);
//	short	iget_short(double x, double y=0.0, int b=DEFAULT_BAND);
//	ushort	iget_ushort(double x, double y=0.0, int b=DEFAULT_BAND);
//	long	iget_long(double x, double y=0.0, int b=DEFAULT_BAND);
//	ulong	iget_ulong(double x, double y=0.0, int b=DEFAULT_BAND);
//	float	iget_float(double x, double y=0.0, int b=DEFAULT_BAND);
//	double	iget_double(double x, double y=0.0, int b=DEFAULT_BAND);
//	COMPLEX_TYPE	iget_complex(double x, double y=0.0, int b=DEFAULT_BAND);

	void	set_bit(int v, int x, int y=0, int b=DEFAULT_BAND);
	void	set_char(char v, int x, int y=0, int b=DEFAULT_BAND);
	void	set_uchar(uchar v, int x, int y=0, int b=DEFAULT_BAND);
	void	set_short(short v, int x, int y=0, int b=DEFAULT_BAND);
	void	set_ushort(ushort v, int x, int y=0, int b=DEFAULT_BAND);
	void	set_long(long v, int x, int y=0, int b=DEFAULT_BAND);
	void	set_ulong(ulong v, int x, int y=0, int b=DEFAULT_BAND);
	void	set_float(float v, int x, int y=0, int b=DEFAULT_BAND);
	void	set_double(double v, int x, int y=0, int b=DEFAULT_BAND);
	void	set_complex(COMPLEX_TYPE v, int x, int y=0, int b=DEFAULT_BAND);

	ushortData() { init(); }
	ushortData(int x, int y=1, int b=1) { init(); allocate(x,y,b); }
	~ushortData() { if(clean) free(); }
	};


// ****************************** longData ********************************

class longData : public ImageData {

    protected:

	long	**data;

	void	init();

    public:

	int	allocate(int x=0, int y=1, int b=1);
	void	free();
	int	add_band();
	int	remove_band(int b=LAST_BAND);
//	int	clear(int b=ALL_BANDS);

	int	access() { return(DATA_READ|DATA_WRITE); }
	int	type() { return 6; }		// class id
	char	*type_name() { return (char *)"longData"; }
	int	preferred_data_type() { return LONG_DATA; }

	virtual	long	*get_data(int b=DEFAULT_BAND);

	// For 24 bit color display:
	void	get_color(int x, int y, uchar *r, uchar *g, uchar *b,
			  uchar *a=NULL);
//	void	iget_color(double x, double y, uchar *r, uchar *g, uchar *b,
//			  uchar *a=NULL);
	void	set_color(int x, int y, uchar r, uchar g, uchar b,
			  uchar a=255);
	uchar	get_alpha(int x, int y);
//	uchar	iget_alpha(double x, double y);
	void	set_alpha(int x, int y, uchar a=255);

	// For 16 bit XColor info:
	ushort	get_red16(int x, int y=0);
	ushort	get_green16(int x, int y=0);
	ushort	get_blue16(int x, int y=0);

//	ushort	iget_red16(double x, double y=0.0);
//	ushort	iget_green16(double x, double y=0.0);
//	ushort	iget_blue16(double x, double y=0.0);

	void	set_red16(ushort c, int x, int y=0);
	void	set_green16(ushort c, int x, int y=0);
	void	set_blue16(ushort c, int x, int y=0);

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

//	int	iget_bit(double x, double y=0.0, int b=DEFAULT_BAND);
//	char	iget_char(double x, double y=0.0, int b=DEFAULT_BAND);
//	uchar	iget_uchar(double x, double y=0.0, int b=DEFAULT_BAND);
//	short	iget_short(double x, double y=0.0, int b=DEFAULT_BAND);
//	ushort	iget_ushort(double x, double y=0.0, int b=DEFAULT_BAND);
//	long	iget_long(double x, double y=0.0, int b=DEFAULT_BAND);
//	ulong	iget_ulong(double x, double y=0.0, int b=DEFAULT_BAND);
//	float	iget_float(double x, double y=0.0, int b=DEFAULT_BAND);
//	double	iget_double(double x, double y=0.0, int b=DEFAULT_BAND);
//	COMPLEX_TYPE	iget_complex(double x, double y=0.0, int b=DEFAULT_BAND);

	void	set_bit(int v, int x, int y=0, int b=DEFAULT_BAND);
	void	set_char(char v, int x, int y=0, int b=DEFAULT_BAND);
	void	set_uchar(uchar v, int x, int y=0, int b=DEFAULT_BAND);
	void	set_short(short v, int x, int y=0, int b=DEFAULT_BAND);
	void	set_ushort(ushort v, int x, int y=0, int b=DEFAULT_BAND);
	void	set_long(long v, int x, int y=0, int b=DEFAULT_BAND);
	void	set_ulong(ulong v, int x, int y=0, int b=DEFAULT_BAND);
	void	set_float(float v, int x, int y=0, int b=DEFAULT_BAND);
	void	set_double(double v, int x, int y=0, int b=DEFAULT_BAND);
	void	set_complex(COMPLEX_TYPE v, int x, int y=0, int b=DEFAULT_BAND);

	longData() { init(); }
	longData(int x, int y=1, int b=1) { init(); allocate(x,y,b); }
	~longData() { if(clean) free(); }
	};


// ****************************** ulongData ********************************

class ulongData : public ImageData {

    protected:

	ulong	**data;

	void	init();

    public:

	int	allocate(int x=0, int y=1, int b=1);
	void	free();
	int	add_band();
	int	remove_band(int b=LAST_BAND);
//	int	clear(int b=ALL_BANDS);

	int	access() { return(DATA_READ|DATA_WRITE); }
	int	type() { return 7; }		// class id
	char	*type_name() { return (char *)"ulongData"; }
	int	preferred_data_type() { return ULONG_DATA; }

	virtual	ulong	*get_data(int b=DEFAULT_BAND);

	// For 24 bit color display:
	void	get_color(int x, int y, uchar *r, uchar *g, uchar *b,
			  uchar *a=NULL);
//	void	iget_color(double x, double y, uchar *r, uchar *g, uchar *b,
//			  uchar *a=NULL);
	void	set_color(int x, int y, uchar r, uchar g, uchar b,
			  uchar a=255);
	uchar	get_alpha(int x, int y);
//	uchar	iget_alpha(double x, double y);
	void	set_alpha(int x, int y, uchar a=255);

	// For 16 bit XColor info:
	ushort	get_red16(int x, int y=0);
	ushort	get_green16(int x, int y=0);
	ushort	get_blue16(int x, int y=0);

//	ushort	iget_red16(double x, double y=0.0);
//	ushort	iget_green16(double x, double y=0.0);
//	ushort	iget_blue16(double x, double y=0.0);

	void	set_red16(ushort c, int x, int y=0);
	void	set_green16(ushort c, int x, int y=0);
	void	set_blue16(ushort c, int x, int y=0);

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

//	int	iget_bit(double x, double y=0.0, int b=DEFAULT_BAND);
//	char	iget_char(double x, double y=0.0, int b=DEFAULT_BAND);
//	uchar	iget_uchar(double x, double y=0.0, int b=DEFAULT_BAND);
//	short	iget_short(double x, double y=0.0, int b=DEFAULT_BAND);
//	ushort	iget_ushort(double x, double y=0.0, int b=DEFAULT_BAND);
//	long	iget_long(double x, double y=0.0, int b=DEFAULT_BAND);
//	ulong	iget_ulong(double x, double y=0.0, int b=DEFAULT_BAND);
//	float	iget_float(double x, double y=0.0, int b=DEFAULT_BAND);
//	double	iget_double(double x, double y=0.0, int b=DEFAULT_BAND);
//	COMPLEX_TYPE	iget_complex(double x, double y=0.0, int b=DEFAULT_BAND);

	void	set_bit(int v, int x, int y=0, int b=DEFAULT_BAND);
	void	set_char(char v, int x, int y=0, int b=DEFAULT_BAND);
	void	set_uchar(uchar v, int x, int y=0, int b=DEFAULT_BAND);
	void	set_short(short v, int x, int y=0, int b=DEFAULT_BAND);
	void	set_ushort(ushort v, int x, int y=0, int b=DEFAULT_BAND);
	void	set_long(long v, int x, int y=0, int b=DEFAULT_BAND);
	void	set_ulong(ulong v, int x, int y=0, int b=DEFAULT_BAND);
	void	set_float(float v, int x, int y=0, int b=DEFAULT_BAND);
	void	set_double(double v, int x, int y=0, int b=DEFAULT_BAND);
	void	set_complex(COMPLEX_TYPE v, int x, int y=0, int b=DEFAULT_BAND);

	ulongData() { init(); }
	ulongData(int x, int y=1, int b=1) { init(); allocate(x,y,b); }
	~ulongData() { if(clean) free(); }
	};


// ****************************** floatData *******************************

class floatData : public ImageData {

    protected:

	float	**data;

	void	init();

    public:

	int	allocate(int x=0, int y=1, int b=1);
	void	free();
	int	add_band();
	int	remove_band(int b=LAST_BAND);
//	int	clear(int b=ALL_BANDS);

	int	access() { return(DATA_READ|DATA_WRITE); }
	int	type() { return 8; }		// class id
	char	*type_name() { return (char *)"floatData"; }
	int	preferred_data_type() { return FLOAT_DATA; }

	virtual	float	*get_data(int b=DEFAULT_BAND);

	// For 24 bit color display:
	void	get_color(int x, int y, uchar *r, uchar *g, uchar *b,
			  uchar *a=NULL);
//	void	iget_color(double x, double y, uchar *r, uchar *g, uchar *b,
//			  uchar *a=NULL);
	void	set_color(int x, int y, uchar r, uchar g, uchar b,
			  uchar a=255);
	uchar	get_alpha(int x, int y);
//	uchar	iget_alpha(double x, double y);
	void	set_alpha(int x, int y, uchar a=255);

	// For 16 bit XColor info:
	ushort	get_red16(int x, int y=0);
	ushort	get_green16(int x, int y=0);
	ushort	get_blue16(int x, int y=0);

//	ushort	iget_red16(double x, double y=0.0);
//	ushort	iget_green16(double x, double y=0.0);
//	ushort	iget_blue16(double x, double y=0.0);

	void	set_red16(ushort c, int x, int y=0);
	void	set_green16(ushort c, int x, int y=0);
	void	set_blue16(ushort c, int x, int y=0);

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

//	int	iget_bit(double x, double y=0.0, int b=DEFAULT_BAND);
//	char	iget_char(double x, double y=0.0, int b=DEFAULT_BAND);
//	uchar	iget_uchar(double x, double y=0.0, int b=DEFAULT_BAND);
//	short	iget_short(double x, double y=0.0, int b=DEFAULT_BAND);
//	ushort	iget_ushort(double x, double y=0.0, int b=DEFAULT_BAND);
//	long	iget_long(double x, double y=0.0, int b=DEFAULT_BAND);
//	ulong	iget_ulong(double x, double y=0.0, int b=DEFAULT_BAND);
//	float	iget_float(double x, double y=0.0, int b=DEFAULT_BAND);
//	double	iget_double(double x, double y=0.0, int b=DEFAULT_BAND);
//	COMPLEX_TYPE	iget_complex(double x, double y=0.0, int b=DEFAULT_BAND);

	void	set_bit(int v, int x, int y=0, int b=DEFAULT_BAND);
	void	set_char(char v, int x, int y=0, int b=DEFAULT_BAND);
	void	set_uchar(uchar v, int x, int y=0, int b=DEFAULT_BAND);
	void	set_short(short v, int x, int y=0, int b=DEFAULT_BAND);
	void	set_ushort(ushort v, int x, int y=0, int b=DEFAULT_BAND);
	void	set_long(long v, int x, int y=0, int b=DEFAULT_BAND);
	void	set_ulong(ulong v, int x, int y=0, int b=DEFAULT_BAND);
	void	set_float(float v, int x, int y=0, int b=DEFAULT_BAND);
	void	set_double(double v, int x, int y=0, int b=DEFAULT_BAND);
	void	set_complex(COMPLEX_TYPE v, int x, int y=0, int b=DEFAULT_BAND);

	floatData() { init(); }
	floatData(int x, int y=1, int b=1) { init(); allocate(x,y,b); }
	~floatData() { if(clean) free(); }
	};


// ****************************** doubleData *******************************

class doubleData : public ImageData {

    protected:

	double	**data;

	void	init();

    public:

	int	allocate(int x=0, int y=1, int b=1);
	void	free();
	int	add_band();
	int	remove_band(int b=LAST_BAND);
//	int	clear(int b=ALL_BANDS);

	int	access() { return(DATA_READ|DATA_WRITE); }
	int	type() { return 9; }		// class id
	char	*type_name() { return (char *)"doubleData"; }
	int	preferred_data_type() { return DOUBLE_DATA; }

	virtual	double	*get_data(int b=DEFAULT_BAND);

	// For 24 bit color display:
	void	get_color(int x, int y, uchar *r, uchar *g, uchar *b,
			  uchar *a=NULL);
//	void	iget_color(double x, double y, uchar *r, uchar *g, uchar *b,
//			  uchar *a=NULL);
	void	set_color(int x, int y, uchar r, uchar g, uchar b,
			  uchar a=255);
	uchar	get_alpha(int x, int y);
//	uchar	iget_alpha(double x, double y);
	void	set_alpha(int x, int y, uchar a=255);

	// For 16 bit XColor info:
	ushort	get_red16(int x, int y=0);
	ushort	get_green16(int x, int y=0);
	ushort	get_blue16(int x, int y=0);

//	ushort	iget_red16(double x, double y=0.0);
//	ushort	iget_green16(double x, double y=0.0);
//	ushort	iget_blue16(double x, double y=0.0);

	void	set_red16(ushort c, int x, int y=0);
	void	set_green16(ushort c, int x, int y=0);
	void	set_blue16(ushort c, int x, int y=0);

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

//	int	iget_bit(double x, double y=0.0, int b=DEFAULT_BAND);
//	char	iget_char(double x, double y=0.0, int b=DEFAULT_BAND);
//	uchar	iget_uchar(double x, double y=0.0, int b=DEFAULT_BAND);
//	short	iget_short(double x, double y=0.0, int b=DEFAULT_BAND);
//	ushort	iget_ushort(double x, double y=0.0, int b=DEFAULT_BAND);
//	long	iget_long(double x, double y=0.0, int b=DEFAULT_BAND);
//	ulong	iget_ulong(double x, double y=0.0, int b=DEFAULT_BAND);
//	float	iget_float(double x, double y=0.0, int b=DEFAULT_BAND);
//	double	iget_double(double x, double y=0.0, int b=DEFAULT_BAND);
//	COMPLEX_TYPE	iget_complex(double x, double y=0.0, int b=DEFAULT_BAND);

	void	set_bit(int v, int x, int y=0, int b=DEFAULT_BAND);
	void	set_char(char v, int x, int y=0, int b=DEFAULT_BAND);
	void	set_uchar(uchar v, int x, int y=0, int b=DEFAULT_BAND);
	void	set_short(short v, int x, int y=0, int b=DEFAULT_BAND);
	void	set_ushort(ushort v, int x, int y=0, int b=DEFAULT_BAND);
	void	set_long(long v, int x, int y=0, int b=DEFAULT_BAND);
	void	set_ulong(ulong v, int x, int y=0, int b=DEFAULT_BAND);
	void	set_float(float v, int x, int y=0, int b=DEFAULT_BAND);
	void	set_double(double v, int x, int y=0, int b=DEFAULT_BAND);
	void	set_complex(COMPLEX_TYPE v, int x, int y=0, int b=DEFAULT_BAND);

	doubleData() { init(); }
	doubleData(int x, int y=1, int b=1) { init(); allocate(x,y,b); }
	~doubleData() { if(clean) free(); }
	};


// ****************************** complexData *******************************

class complexData : public ImageData {

    protected:

	COMPLEX_TYPE	**data;

	void	init();

    public:

	int	allocate(int x=0, int y=1, int b=1);
	void	free();
	int	add_band();
	int	remove_band(int b=LAST_BAND);
//	int	clear(int b=ALL_BANDS);

	int	access() { return(DATA_READ|DATA_WRITE); }
	int	type() { return 10; }		// class id
	char	*type_name() { return (char *)"complexData"; }
	int	preferred_data_type() { return COMPLEX_DATA; }

	virtual	COMPLEX_TYPE	*get_data(int b=DEFAULT_BAND);

	// For 24 bit color display:
	void	get_color(int x, int y, uchar *r, uchar *g, uchar *b,
			  uchar *a=NULL);
//	void	iget_color(double x, double y, uchar *r, uchar *g, uchar *b,
//			  uchar *a=NULL);
	void	set_color(int x, int y, uchar r, uchar g, uchar b,
			  uchar a=255);
	uchar	get_alpha(int x, int y);
//	uchar	iget_alpha(double x, double y);
	void	set_alpha(int x, int y, uchar a=255);

	// For 16 bit XColor info:
	ushort	get_red16(int x, int y=0);
	ushort	get_green16(int x, int y=0);
	ushort	get_blue16(int x, int y=0);

//	ushort	iget_red16(double x, double y=0.0);
//	ushort	iget_green16(double x, double y=0.0);
//	ushort	iget_blue16(double x, double y=0.0);

	void	set_red16(ushort c, int x, int y=0);
	void	set_green16(ushort c, int x, int y=0);
	void	set_blue16(ushort c, int x, int y=0);

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

//	int	iget_bit(double x, double y=0.0, int b=DEFAULT_BAND);
//	char	iget_char(double x, double y=0.0, int b=DEFAULT_BAND);
//	uchar	iget_uchar(double x, double y=0.0, int b=DEFAULT_BAND);
//	short	iget_short(double x, double y=0.0, int b=DEFAULT_BAND);
//	ushort	iget_ushort(double x, double y=0.0, int b=DEFAULT_BAND);
//	long	iget_long(double x, double y=0.0, int b=DEFAULT_BAND);
//	ulong	iget_ulong(double x, double y=0.0, int b=DEFAULT_BAND);
//	float	iget_float(double x, double y=0.0, int b=DEFAULT_BAND);
//	double	iget_double(double x, double y=0.0, int b=DEFAULT_BAND);
//	COMPLEX_TYPE	iget_complex(double x, double y=0.0, int b=DEFAULT_BAND);

	void	set_bit(int v, int x, int y=0, int b=DEFAULT_BAND);
	void	set_char(char v, int x, int y=0, int b=DEFAULT_BAND);
	void	set_uchar(uchar v, int x, int y=0, int b=DEFAULT_BAND);
	void	set_short(short v, int x, int y=0, int b=DEFAULT_BAND);
	void	set_ushort(ushort v, int x, int y=0, int b=DEFAULT_BAND);
	void	set_long(long v, int x, int y=0, int b=DEFAULT_BAND);
	void	set_ulong(ulong v, int x, int y=0, int b=DEFAULT_BAND);
	void	set_float(float v, int x, int y=0, int b=DEFAULT_BAND);
	void	set_double(double v, int x, int y=0, int b=DEFAULT_BAND);
	void	set_complex(COMPLEX_TYPE v, int x, int y=0, int b=DEFAULT_BAND);

	complexData() { init(); }
	complexData(int x, int y=1, int b=1) { init(); allocate(x,y,b); }
	~complexData() { if(clean) free(); }
	};

// ****************************** RLEData *******************************

class RLEData : public ImageData {

    protected:

	long	***data;
	long	**line_lengths;
	long	curr_line, *curr_posn, curr_len;

	void	init();

    public:

	int	allocate(int x=0, int y=1, int b=1);
	void	free();
	int	add_band();
	int	remove_band(int b=LAST_BAND);
//	int	clear(int b=ALL_BANDS);

	int	access() { return(DATA_READ|DATA_WRITE); }
	int	type() { return 50; }		// class id
	char	*type_name() { return (char *)"RLEData"; }
	int	preferred_data_type() { return UCHAR_DATA; }

	virtual	long	**get_data(int b=DEFAULT_BAND);

	// For 24 bit color display:
	void	get_color(int x, int y, uchar *r, uchar *g, uchar *b,
			  uchar *a=NULL);
	void	get_color_row(int x, int y, int length, uchar *r, uchar *g, uchar *b,
			  uchar *a=NULL);
	void	iget_color(double x, double y, uchar *r, uchar *g, uchar *b,
			  uchar *a=NULL);
//	void	iget_color_row(double x, double y, double xstep, int length, uchar *r, uchar *g, uchar *b,
//			  uchar *a=NULL);
	void	set_color(int x, int y, uchar r, uchar g, uchar b,
			  uchar a=255);
	uchar	get_alpha(int x, int y);
//	uchar	iget_alpha(double x, double y);
	void	set_alpha(int x, int y, uchar a=255);

	// For 16 bit XColor info:
	ushort	get_red16(int x, int y=0);
	ushort	get_green16(int x, int y=0);
	ushort	get_blue16(int x, int y=0);

//	ushort	iget_red16(double x, double y=0.0);
//	ushort	iget_green16(double x, double y=0.0);
//	ushort	iget_blue16(double x, double y=0.0);

	void	set_red16(ushort c, int x, int y=0);
	void	set_green16(ushort c, int x, int y=0);
	void	set_blue16(ushort c, int x, int y=0);

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

//	int	iget_bit(double x, double y=0.0, int b=DEFAULT_BAND);
//	char	iget_char(double x, double y=0.0, int b=DEFAULT_BAND);
//	uchar	iget_uchar(double x, double y=0.0, int b=DEFAULT_BAND);
//	short	iget_short(double x, double y=0.0, int b=DEFAULT_BAND);
//	ushort	iget_ushort(double x, double y=0.0, int b=DEFAULT_BAND);
//	long	iget_long(double x, double y=0.0, int b=DEFAULT_BAND);
//	ulong	iget_ulong(double x, double y=0.0, int b=DEFAULT_BAND);
//	float	iget_float(double x, double y=0.0, int b=DEFAULT_BAND);
//	double	iget_double(double x, double y=0.0, int b=DEFAULT_BAND);
//	COMPLEX_TYPE	iget_complex(double x, double y=0.0, int b=DEFAULT_BAND);

	void	set_bit(int v, int x, int y=0, int b=DEFAULT_BAND);
	void	set_char(char v, int x, int y=0, int b=DEFAULT_BAND);
	void	set_uchar(uchar v, int x, int y=0, int b=DEFAULT_BAND);
	void	set_short(short v, int x, int y=0, int b=DEFAULT_BAND);
	void	set_ushort(ushort v, int x, int y=0, int b=DEFAULT_BAND);
	void	set_long(long v, int x, int y=0, int b=DEFAULT_BAND);
	void	set_ulong(ulong v, int x, int y=0, int b=DEFAULT_BAND);
	void	set_float(float v, int x, int y=0, int b=DEFAULT_BAND);
	void	set_double(double v, int x, int y=0, int b=DEFAULT_BAND);
	void	set_complex(COMPLEX_TYPE v, int x, int y=0, int b=DEFAULT_BAND);

// High speed fill routines for pre run-length-encoded data
	void	line_append(long v, long length, int y=0, int b=DEFAULT_BAND);
	void	band_append(long v, long length, int b=DEFAULT_BAND); // Not Implemented Yet

// High speed fetch routine for run-length-encoded data
	long	*get_line(int line, int band=DEFAULT_BAND);

	RLEData() { init(); }
	RLEData(int x, int y=1, int b=1) { init(); allocate(x,y,b); }
	~RLEData() { if(clean) free(); }
	};


#ifndef _NO_IMAGEDISP_


//  ###########################  ximageData class ######################
//
// This class makes it easy to put a X pixmap into an image
//



class ximageData : public ImageData {

 protected:

	Pixmap pixmap;
	Display *display;
	Visual *visual;
	XImage *ximg;
	int depth;

	void init (void);
	void copy_pixmap_to_ximage( int xx, int xy, int ix, int iy, int w, int h);

 public:

	int	allocate(int x=0, int y=1, int b=1);
	void free( void);
	int	add_band() { return 0;}
	int	remove_band(int) { return 0;}

    int	access() { return(DATA_READ|DATA_WRITE); }
	int	type() { return XIMAGE_DATA; }		// class id
		char	*type_name() { return (char *)"ximageData"; }
	int	preferred_data_type() { return XIMAGE_DATA; }

	virtual	XImage	*get_data(int b=DEFAULT_BAND);

	
	void set_pixmap( Pixmap pm);
	void set_display( Display *pd) { display = pd; }
	void set_visual( Visual *pv) { visual = pv; }
	void set_depth( int d) { depth = d; }

	// For 24 bit color display:
	void	get_color(int x, int y, uchar *r, uchar *g, uchar *b,
					  uchar *a=NULL);
	void	set_color(int x, int y, uchar r, uchar g, uchar b,
					  uchar a=255);
	uchar	get_alpha(int x, int y);
	void	set_alpha(int x, int y, uchar a=255);

	// For 16 bit XColor info:
	ushort	get_red16(int x, int y=0) ;
	ushort	get_green16(int x, int y=0) ;
	ushort	get_blue16(int x, int y=0) ;
	//
	void	set_red16(ushort c, int x, int y=0) ;
	void	set_green16(ushort c, int x, int y=0) ;
	void	set_blue16(ushort c, int x, int y=0) ;

	// Data types:

	int	get_bit(int x, int y=0, int b=DEFAULT_BAND) ;
	char	get_char(int x, int y=0, int b=DEFAULT_BAND) ;
	uchar	get_uchar(int x, int y=0, int b=DEFAULT_BAND) ;
	short	get_short(int x, int y=0, int b=DEFAULT_BAND) ;
	ushort	get_ushort(int x, int y=0, int b=DEFAULT_BAND) ;
	long	get_long(int x, int y=0, int b=DEFAULT_BAND) ;
	ulong	get_ulong(int x, int y=0, int b=DEFAULT_BAND) ;
	float	get_float(int x, int y=0, int b=DEFAULT_BAND) ;
	double	get_double(int x, int y=0, int b=DEFAULT_BAND) ;
	COMPLEX_TYPE get_complex(int x, int y=0, int b=DEFAULT_BAND) ;

	void	set_bit(int v, int x, int y=0, int b=DEFAULT_BAND) ;
	void	set_char(char v, int x, int y=0, int b=DEFAULT_BAND) ;
	void	set_uchar(uchar v, int x, int y=0, int b=DEFAULT_BAND) ;
	void	set_short(short v, int x, int y=0, int b=DEFAULT_BAND) ;
	void	set_ushort(ushort v, int x, int y=0, int b=DEFAULT_BAND) ;
	void	set_long(long v, int x, int y=0, int b=DEFAULT_BAND) ;
	void	set_ulong(ulong v, int x, int y=0, int b=DEFAULT_BAND) ;
	void	set_float(float v, int x, int y=0, int b=DEFAULT_BAND) ;
	void	set_double(double v, int x, int y=0, int b=DEFAULT_BAND) ;
	void	set_complex(COMPLEX_TYPE v, int x, int y=0, int b=DEFAULT_BAND) ;

	ximageData();
//	ximageData( int x, int y=1, int b=1) { init(); }

//	ximageData( Pixmap pm, Display *pd, Visual *pv, int depth=0) { 
//		init(); 
//		set_pixmap( pm);
//		set_display( pd);
//		set_visual( pv);
//		set_depth( depth);
//	}


	~ximageData();

};


// Convenience routines:
Image	*color_image(int x=0, int y=0);
Image	*gray_image(int x=0, int y=0);
Image	*pseudocolor_image(int x=0, int y=0, int n=256);
Image	*color_alpha_image(int x=0, int y=0);
Image	*gray_alpha_image(int x=0, int y=0);
Image	*pseudocolor_alpha_image(int x=0, int y=0, int n=256);

#endif // _NO_IMAGEDISP_


#endif // _DATATYPES_H_
