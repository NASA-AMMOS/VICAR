// datammap.h
//
// Written by Dave Kagels 2/14/95

#ifndef _DATAMMAP_H_
#define _DATAMMAP_H_

#include "image/image.h"
#include "image/imagedata.h"
#include "image/datatypes.h"


// ****************************** ucharMMap *******************************

class ucharMMap : public ucharData {

    protected:

	int	data_access;
	uchar	*base,*rbase;
	int	page_size,tlen;
	uchar	**rdata;

	void	init() { data_access=0; base=NULL; rbase=NULL;
			 page_size=1; rdata=NULL; tlen=0; }

    public:

	int	allocate(int =0, int =1, int =1) { return 0; }
	void	free();
	int	add_band() { return 0; }
	int	remove_band(int =LAST_BAND) { return 0; }
//	int	clear(int b=ALL_BANDS);

	int	access() { return data_access; }
	int	type() { return 13; }		// class id
	char	*type_name() { return (char *)"ucharMMap"; }

	virtual	int	mmap_setup(int a, int x, int y=1, int b=1);
	virtual	int	mmap_band(int b, int fd, int off);
	virtual	int	mmap_data(int fd, int off, int len=0);
	virtual	int	set_off(int b, int off);

	ucharMMap() { init(); }
	~ucharMMap() { if(clean) free(); }
	};


class ucharMMapFV : public ucharMMap {

    public:

	void	free();

	int	type() { return 23; }		// class id
	char	*type_name() { return (char *)"ucharMMapFV"; }

	int	mmap_band(int b, int fd, int off);
	int	mmap_data(int fd, int off, int len=0);
	int	set_off(int b, int off);

	void	get_color(int x, int y, uchar *r, uchar *g, uchar *b, uchar *a=NULL);
	void	iget_color(double x, double y, uchar *r, uchar *g, uchar *b, uchar *a=NULL);
	uchar	get_uchar(int x, int y=0, int b=DEFAULT_BAND);
	long	get_long(int x, int y=0, int b=DEFAULT_BAND);
	double	get_double(int x, int y=0, int b=DEFAULT_BAND);

	~ucharMMapFV() { if(clean) free(); }
	};


// ****************************** shortMMap *******************************

class shortMMap : public shortData {

    protected:

	int	data_access;
	short	*base,*rbase;
	int	page_size,tlen;
	short	**rdata;

	void	init() { data_access=0; base=NULL; rbase=NULL;
			 page_size=2; rdata=NULL; tlen=0; }

    public:

	int	allocate(int =0, int =1, int =1) { return 0; }
	void	free();
	int	add_band() { return 0; }
	int	remove_band(int =LAST_BAND) { return 0; }
//	int	clear(int b=ALL_BANDS);

	int	access() { return data_access; }
	int	type() { return 14; }		// class id
	char	*type_name() { return (char *)"shortMMap"; }

	virtual	int	mmap_setup(int a, int x, int y=1, int b=1);
	virtual	int	mmap_band(int b, int fd, int off);
	virtual	int	mmap_data(int fd, int off, int len=0);
	virtual	int	set_off(int b, int off);

	short	get_short(int x, int y=0, int b=DEFAULT_BAND);
	double	get_double(int x, int y=0, int b=DEFAULT_BAND);

	shortMMap() { init(); }
	~shortMMap() { if(clean) free(); }
	};


class shortMMapFV : public shortMMap {

    public:

	void	free();

	int	type() { return 24; }		// class id
	char	*type_name() { return (char *)"shortMMapFV"; }

	int	mmap_band(int b, int fd, int off);
	int	mmap_data(int fd, int off, int len=0);
	int	set_off(int b, int off);

	short	get_short(int x, int y=0, int b=DEFAULT_BAND);
	double	get_double(int x, int y=0, int b=DEFAULT_BAND);

	~shortMMapFV() { if(clean) free(); }
	};


// ****************************** ushortMMap *******************************

class ushortMMap : public ushortData {

    protected:

	int	data_access;
	ushort	*base,*rbase;
	int	page_size,tlen;
	ushort	**rdata;

	void	init() { data_access=0; base=NULL; rbase=NULL;
			 page_size=2; rdata=NULL; tlen=0; }

    public:

	int	allocate(int =0, int =1, int =1) { return 0; }
	void	free();
	int	add_band() { return 0; }
	int	remove_band(int =LAST_BAND) { return 0; }
//	int	clear(int b=ALL_BANDS);

	int	access() { return data_access; }
	int	type() { return 15; }		// class id
	char	*type_name() { return (char *)"ushortMMap"; }

	virtual	int	mmap_setup(int a, int x, int y=1, int b=1);
	virtual	int	mmap_band(int b, int fd, int off);
	virtual	int	mmap_data(int fd, int off, int len=0);
	virtual	int	set_off(int b, int off);

	ushort	get_ushort(int x, int y=0, int b=DEFAULT_BAND);
	double	get_double(int x, int y=0, int b=DEFAULT_BAND);

	ushortMMap() { init(); }
	~ushortMMap() { if(clean) free(); }
	};


class ushortMMapFV : public ushortMMap {

    public:

	void	free();

	int	type() { return 25; }		// class id
	char	*type_name() { return (char *)"ushortMMapFV"; }

	int	mmap_band(int b, int fd, int off);
	int	mmap_data(int fd, int off, int len=0);
	int	set_off(int b, int off);

	ushort	get_ushort(int x, int y=0, int b=DEFAULT_BAND);
	double	get_double(int x, int y=0, int b=DEFAULT_BAND);

	~ushortMMapFV() { if(clean) free(); }
	};


// ****************************** floatMMap *******************************

class floatMMap : public floatData {

    protected:

	int	data_access;
	float	*base,*rbase;
	int	page_size,tlen;
	float	**rdata;

	void	init() { data_access=0; base=NULL; rbase=NULL;
			 page_size=4; rdata=NULL; tlen=0; }

    public:

	int	allocate(int =0, int =1, int =1) { return 0; }
	void	free();
	int	add_band() { return 0; }
	int	remove_band(int =LAST_BAND) { return 0; }
//	int	clear(int b=ALL_BANDS);

	int	access() { return data_access; }
	int	type() { return 18; }		// class id
	char	*type_name() { return (char *)"floatMMap"; }

	virtual	int	mmap_setup(int a, int x, int y=1, int b=1);
	virtual	int	mmap_band(int b, int fd, int off);
	virtual	int	mmap_data(int fd, int off, int len=0);
	virtual	int	set_off(int b, int off);

	float	get_float(int x, int y=0, int b=DEFAULT_BAND);
	double	get_double(int x, int y=0, int b=DEFAULT_BAND);

	floatMMap() { init(); }
	~floatMMap() { if(clean) free(); }
	};


class floatMMapFV : public floatMMap {

    public:

	void	free();

	int	type() { return 28; }		// class id
	char	*type_name() { return (char *)"floatMMapFV"; }

	int	mmap_band(int b, int fd, int off);
	int	mmap_data(int fd, int off, int len=0);
	int	set_off(int b, int off);

	float	get_float(int x, int y=0, int b=DEFAULT_BAND);
	double	get_double(int x, int y=0, int b=DEFAULT_BAND);

	~floatMMapFV() { if(clean) free(); }
	};

#endif // _DATAMMAP_H_
