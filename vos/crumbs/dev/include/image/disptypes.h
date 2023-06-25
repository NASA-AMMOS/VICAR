// disptypes.h
//
// Written by Dave Kagels 10/24/94

#ifndef _DISPTYPES_H_
#define _DISPTYPES_H_

#ifndef _NO_IMAGEDISP_

#include "image/imagedisp.h"


// Disp24  -  For 24 bit TrueColor displays, any type of data:

class Disp24 : public ImageDisp {

    protected:

	int	rshift,gshift,bshift;

	void	init() { rshift=0; gshift=0; bshift=0; }

    public:

	int	initialize();

	Pixel	pixel(int x, int y=0);
	Pixel	ipixel(double x, double y=0.0);
	Pixel	dpixel(int x, int y=0, int sx=0, int sy=0);
	Pixel	idpixel(double x, double y=0.0, int sx=0, int sy=0);

	void	get_area32(ulong *buf, int ix, int iy, int w, int h=1, int ppl=0);
	void	iget_area32(ulong *buf, double x, double y, double dx, double dy, int w, int h=1, int sx=0, int sy=0, int ppl=0);

	Disp24(Image *i=NULL) { init(); if(i) set_image(i); }
	Disp24(Image *i, Display *d, Visual *v=NULL, Colormap c=0) {
			init(); set_image(i); setup(d,v,c); }
	Disp24(Image *i, Display *d, Colormap c) {
			init(); set_image(i); setup(d,c); }
	};


// DispMap  -  For PseudoColor or TrueColor displays, data with a colormap:

class DispMap : public ImageDisp {

    protected:

	Pixel	*pixel_map;
	int	map_size;

	void	init() { pixel_map=NULL; map_size=0; }

	virtual	void	free_map();

    public:

	void	set_display(Display *d) { free_map(); display=d; }
	void	set_visual(Visual *v) { free_map(); visual=v; }
	void	set_colormap(Colormap c) { free_map(); colormap=c; }
	int	initialize();

	Pixel	pixel(int x, int y=0);
	Pixel	ipixel(double x, double y=0.0);
	Pixel	dpixel(int x, int y=0, int sx=0, int sy=0);
	Pixel	idpixel(double x, double y=0.0, int sx=0, int sy=0);

	void	get_area8(uchar *buf, int ix, int iy, int w, int h=1, int ppl=0);
	void	iget_area8(uchar *buf, double x, double y, double dx, double dy, int w, int h=1, int sx=0, int sy=0, int ppl=0);
	void	get_area32(ulong *buf, int ix, int iy, int w, int h=1, int ppl=0);
	void	iget_area32(ulong *buf, double x, double y, double dx, double dy, int w, int h=1, int sx=0, int sy=0, int ppl=0);

	DispMap(Image *i=NULL) { init(); if(i) set_image(i); }
	DispMap(Image *i, Display *d, Visual *v=NULL, Colormap c=0) {
			init(); set_image(i); setup(d,v,c); }
	DispMap(Image *i, Display *d, Colormap c) {
			init(); set_image(i); setup(d,c); }
	~DispMap() { free_map(); }
	};


// Disp8  -  For 8 bit PseudoColor displays, any type of data:

class Disp8 : public DispMap {

    protected:

	int	*tri;					// table of triples to try
	int	num_tri;				// number of entries
	int	rnum,gnum,bnum;				// shades of components
	int	rcomp[256],gcomp[256],bcomp[256];	// table of comp values
	Pixel	opix_map[4096];				// optimized pixel map

	void	init();

	virtual	void	comp_shade();	// compute shade reduction map table
	virtual void	comp_opmap();	// compute optimized 3comp->Pixel mapping

    public:

	int	initialize();

	Pixel	pixel(int x, int y=0);
	Pixel	ipixel(double x, double y=0.0);
	Pixel	dpixel(int x, int y=0, int sx=0, int sy=0);
	Pixel	idpixel(double x, double y=0.0, int sx=0, int sy=0);

	virtual	void	set_tri_table(int *tab, int num);
	virtual int	*get_tri_table(int *num=NULL) { if(num) *num=num_tri;
				return tri; }

	void	get_area8(uchar *buf, int ix, int iy, int w, int h=1, int ppl=0);
	void	iget_area8(uchar *buf, double x, double y, double dx, double dy, int w, int h=1, int sx=0, int sy=0, int ppl=0);
	void	get_area32(ulong *buf, int ix, int iy, int w, int h=1, int ppl=0);
	void	iget_area32(ulong *buf, double x, double y, double dx, double dy, int w, int h=1, int sx=0, int sy=0, int ppl=0);

	Disp8(Image *i=NULL) { init(); if(i) set_image(i); }
	Disp8(Image *i, Display *d, Visual *v=NULL, Colormap c=0) {
			init(); set_image(i); setup(d,v,c); }
	Disp8(Image *i, Display *d, Colormap c) {
			init(); set_image(i); setup(d,c); }
	};


// DispDither  -  8 bit PseudoColor dithered, any type of data:

class DispDither : public Disp8 {

    protected:

	int	rcomp1[256],gcomp1[256],bcomp1[256];	// table values incremented
	int	rind[256],gind[256],bind[256];		// dither index tables
	int	*xmodtab,*ymodtab;			// modulo table for x&y
	int	*dtab;					// dither table stored
	int	dxres;					// dither x resolution
	Image	*dither_img;				// dither pattern image

	void	init();

	void	comp_shade();		// compute shade reduction map table

    public:

	Pixel	pixel(int x, int y=0);
	Pixel	ipixel(double x, double y=0.0);
	Pixel	dpixel(int x, int y=0, int sx=0, int sy=0);
	Pixel	idpixel(double x, double y=0.0, int sx=0, int sy=0);

	virtual	int	set_dither(Image *img);
	virtual	Image	*get_dither() { return dither_img; }

	void	get_area8(uchar *buf, int ix, int iy, int w, int h=1, int ppl=0);
	void	iget_area8(uchar *buf, double x, double y, double dx, double dy, int w, int h=1, int sx=0, int sy=0, int ppl=0);
	void	get_area32(ulong *buf, int ix, int iy, int w, int h=1, int ppl=0);
	void	iget_area32(ulong *buf, double x, double y, double dx, double dy, int w, int h=1, int sx=0, int sy=0, int ppl=0);

	DispDither(Image *i=NULL) { init(); if(i) set_image(i); }
	DispDither(Image *i, Display *d, Visual *v=NULL, Colormap c=0) {
			init(); set_image(i); setup(d,v,c); }
	DispDither(Image *i, Display *d, Colormap c) {
			init(); set_image(i); setup(d,c); }
	DispDither(Image *i, Image *di, Display *d=NULL, Visual *v=NULL, Colormap c=0) {
			init(); set_image(i); set_dither(di);
			if(d) setup(d,v,c); }
	DispDither(Image *i, Image *di, Display *d, Colormap c) {
			init(); set_image(i); set_dither(di); setup(d,c); }
	~DispDither();
	};

#endif // _NO_IMAGEDISP_

#endif // _DISPTYPES_H_
