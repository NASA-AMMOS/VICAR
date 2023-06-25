// datads.h
//
// Written by Dave Kagels 10/31/95

#ifndef _DATADS_H_
#define _DATADS_H_

#ifndef _NO_DATASERVER_
#include "image/image.h"
#include "image/imagedata.h"


// ****************************** dataDS *******************************
//
// This is the base class for image data classes that use the data server.

class dataDS : public ImageData {

    protected:

	// socket handle:
	int	sock;

	int	lev_int;
	double	lev_frac;

	int	yflip;		// yres-1, used for flipping image

	void	init();

    public:

	// Storage control functions:
	int	allocate(int =0, int =1, int =1);
	void	free();
	int	add_band() { return 0; }
	int	remove_band(int =LAST_BAND) { return 0; }
	int	clear(int =ALL_BANDS) { return 0; }

	// Image information functions:
	int	access() { return 0; }
	int	type() { return 30; }		// class id
	char	*type_name() { return (char *)"dataDS"; }
	int	preferred_data_type() { return NO_DATA; }

	void	set_level(double l) { clev=pyr_lev(l);
			lev_int=(int)clev; lev_frac=clev-(double)lev_int; }

	// For 24 bit color display:
	void	set_color(int , int , uchar , uchar , uchar ,
				  uchar =255) { ; }
	void	iset_color(double , double , uchar , uchar , uchar ,
				  uchar =255) { ; }
	void	set_alpha(int , int , uchar =255) { ; }
	void	iset_alpha(double , double , uchar =255) { ; }

	void	set_red16(ushort , int , int =0) { ; }
	void	set_green16(ushort , int , int =0) { ; }
	void	set_blue16(ushort , int , int =0) { ; }
	void	iset_red16(ushort , double , double =0.0) { ; }
	void	iset_green16(ushort , double , double =0.0) { ; }
	void	iset_blue16(ushort , double , double =0.0) { ; }

	// Set value functions: (integer pixel)
	void	set_bit(int , int , int =0, int =DEFAULT_BAND) { ; }
	void	set_char(char , int , int =0, int =DEFAULT_BAND) { ; }
	void	set_uchar(uchar , int , int =0, int =DEFAULT_BAND) { ; }
	void	set_short(short , int , int =0, int =DEFAULT_BAND) { ; }
	void	set_ushort(ushort , int , int =0, int =DEFAULT_BAND) { ; }
	void	set_long(long , int , int =0, int =DEFAULT_BAND) { ; }
	void	set_ulong(ulong , int , int =0, int =DEFAULT_BAND) { ; }
	void	set_float(float , int , int =0, int =DEFAULT_BAND) { ; }
	void	set_double(double , int , int =0, int =DEFAULT_BAND) { ; }
	void	set_complex(COMPLEX_TYPE , int , int =0, int =DEFAULT_BAND) { ; }

	// Set value functions: (floating point coordinates)
	void	iset_bit(int , double , double =0.0, int =DEFAULT_BAND) { ; }
	void	iset_char(char , double , double =0.0, int =DEFAULT_BAND) { ; }
	void	iset_uchar(uchar , double , double =0.0, int =DEFAULT_BAND) { ; }
	void	iset_short(short , double , double =0.0, int =DEFAULT_BAND) { ; }
	void	iset_ushort(ushort , double , double =0.0, int =DEFAULT_BAND) { ; }
	void	iset_long(long , double , double =0.0, int =DEFAULT_BAND) { ; }
	void	iset_ulong(ulong , double , double =0.0, int =DEFAULT_BAND) { ; }
	void	iset_float(float , double , double =0.0, int =DEFAULT_BAND) { ; }
	void	iset_double(double , double , double =0.0, int =DEFAULT_BAND) { ; }
	void	iset_complex(COMPLEX_TYPE , double , double =0.0, int =DEFAULT_BAND) { ; }

	void	set_socket(int s) { sock=s; }

	int	get_socket() { return sock; }

	void	set_levels(int l) { levels=l; levmax=(double)(l-1); }

	dataDS() { init(); }
	~dataDS() { if(clean) free(); }
	};


// ****************************** ucharDS *******************************

class ucharDS : public dataDS {

    public:

	// Image information functions:
	int	access() { return DATA_READ; }
	int	type() { return 33; }		// class id
	char	*type_name() { return (char *)"ucharDS"; }
	int	preferred_data_type() { return UCHAR_DATA; }

	// For 24 bit color display:
	void	get_color(int x, int y, uchar *r, uchar *g, uchar *b,
				  uchar *a=NULL);
	void	iget_color(double x, double y, uchar *r, uchar *g, uchar *b,
				  uchar *a=NULL);
	uchar	get_alpha(int x, int y);
	uchar	iget_alpha(double x, double y);

	// Data types:

	// Get value functions: (integer pixel)
//	int	get_bit(int x, int y=0, int b=DEFAULT_BAND);
//	char	get_char(int , int =0, int =DEFAULT_BAND);
	uchar	get_uchar(int , int =0, int =DEFAULT_BAND);
//	short	get_short(int , int =0, int =DEFAULT_BAND);
//	ushort	get_ushort(int , int =0, int =DEFAULT_BAND);
//	long	get_long(int , int =0, int =DEFAULT_BAND);
//	ulong	get_ulong(int , int =0, int =DEFAULT_BAND);
//	float	get_float(int , int =0, int =DEFAULT_BAND);
//	double	get_double(int , int =0, int =DEFAULT_BAND);
//	COMPLEX_TYPE	get_complex(int x, int y=0, int b=DEFAULT_BAND);

	// Get value functions: (bilinear interpolation)
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

	~ucharDS() { ; }
	};

#endif // _NO_DATASERVER_
#endif // _DATADS_H_
