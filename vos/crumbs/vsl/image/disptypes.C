// disptypes.C
//
// Written by Dave Kagels 10/24/94

#ifndef _NO_IMAGEDISP_

#include "image/disptypes.h"
#include "image/image.h"
#include "image/datatypes.h"


// Disp24  -  For 24 bit TrueColor displays, any type of data:


int Disp24::initialize()
{
XVisualInfo	vinfo,*v;
XWindowAttributes xwa;
int		screen,i;

if(!display) return 0;

depth=24;

if(!visual) {
	if(window) {
		i=XGetWindowAttributes(display,window,&xwa);
		if(!i) return 0;
		screen=XScreenNumberOfScreen(xwa.screen);
		}
	else	screen=DefaultScreen(display);
	if(!XMatchVisualInfo(display,screen,depth,TrueColor,&vinfo)) return 0;
	visual=vinfo.visual;
	}
else {	vinfo.visualid=XVisualIDFromVisual(visual);
	vinfo.depth=depth;
	vinfo.c_class=TrueColor;
	v=XGetVisualInfo(display,VisualIDMask|VisualDepthMask|VisualClassMask,
			 &vinfo,&i);
	if(!v) return 0;
	vinfo=*v;
	XFree(v);
	}
if(!colormap) {
	if(window) colormap=XCreateColormap(display,window,visual,AllocNone);
	else	colormap=XCreateColormap(display,RootWindow(display,
				DefaultScreen(display)),visual,AllocNone);
	if(!colormap) return 0;
	}
for(rshift=0;!(vinfo.red_mask&1) && rshift<32; rshift++) vinfo.red_mask>>=1;
for(gshift=0;!(vinfo.green_mask&1) && gshift<32; gshift++) vinfo.green_mask>>=1;
for(bshift=0;!(vinfo.blue_mask&1) && bshift<32; bshift++) vinfo.blue_mask>>=1;

return 1;
}


Pixel Disp24::pixel(int x, int y)
{
unsigned char	r,g,b;

img->get_color(x,y,&r,&g,&b);
return (((Pixel)r)<<rshift)|(((Pixel)g)<<gshift)|(((Pixel)b)<<bshift);
}


Pixel Disp24::ipixel(double x, double y)
{
unsigned char	r,g,b;

(img->get_data())->iget_color(x,y,&r,&g,&b);
return (((Pixel)r)<<rshift)|(((Pixel)g)<<gshift)|(((Pixel)b)<<bshift);
}


Pixel Disp24::dpixel(int x, int y, int, int)
{
unsigned char	r,g,b;

img->get_color(x,y,&r,&g,&b);
return (((Pixel)r)<<rshift)|(((Pixel)g)<<gshift)|(((Pixel)b)<<bshift);
}


Pixel Disp24::idpixel(double x, double y, int, int)
{
unsigned char	r,g,b;

(img->get_data())->iget_color(x,y,&r,&g,&b);
return (((Pixel)r)<<rshift)|(((Pixel)g)<<gshift)|(((Pixel)b)<<bshift);
}


void Disp24::get_area32(ulong *buf, int ix, int iy, int w, int h, int ppl)
{
register int	ex,x,ey,y;
#ifndef _FAST_PIXELS_
unsigned char	r,g,b;
#else
unsigned char	*tbuf;
#endif
ImageData	*id;

if(ppl) ppl-=w;
ex=ix+w;
ey=iy+h;

if(interpolate) {
    id=img->get_data();
#ifndef _FAST_PIXELS_
    for(y=iy;y<ey;y++) {
	for(x=ix;x<ex;x++) {
		id->iget_color((double)x,(double)y,&r,&g,&b);
		*(buf++)=(((ulong)r)<<rshift)|(((ulong)g)<<gshift)|(((ulong)b)<<bshift);
		}
	buf+=ppl;
	}
#else
	uchar   *red = (uchar *)malloc(3 * w * sizeof(uchar));
	uchar   *grn = red + w;
	uchar   *blu = grn + w;
    for(y=iy;y<ey;y++) {
	uchar 	*tred=red, *tgrn=grn, *tblu=blu;
	id->iget_color_row((double)ix, (double)y, 1.0, w, red, grn, blu);
	for(x=ix;x<ex;x++) {
		tbuf = (unsigned char *)buf;
		tbuf++;
		*tbuf++ = *tblu++;
		*tbuf++ = *tgrn++;
		*tbuf++ = *tred++;
		buf+=1;
		}
		buf+=ppl;
	}
	free(red);
#endif
    }
else {
#ifndef _FAST_PIXELS_
    for(y=iy;y<ey;y++) {
	for(x=ix;x<ex;x++) {
		img->get_color(x,y,&r,&g,&b);
		*(buf++)=(((ulong)r)<<rshift)|(((ulong)g)<<gshift)|(((ulong)b)<<bshift);
		}
	buf+=ppl;
	}
#else
	uchar   *red = (uchar *)malloc(3 * w * sizeof(uchar));
	uchar   *grn = red + w;
	uchar   *blu = grn + w;
    for(y=iy;y<ey;y++) {
	uchar 	*tred=red, *tgrn=grn, *tblu=blu;
	img->get_color_row(ix, y, w, red, grn, blu);
	for(x=ix;x<ex;x++) {
		tbuf = (unsigned char *)buf;
		//*(buf++)=(((ulong)*(tred++))<<rshift)|(((ulong)*(tgrn++))<<gshift)|(((ulong)*(tblu++))<<bshift);
		tbuf++;
		*tbuf++ = *tblu++;
		*tbuf++ = *tgrn++;
		*tbuf++ = *tred++;
		buf+=1;
		}
		buf+=ppl;
	}
	free(red);
#endif
    }
}


void Disp24::iget_area32(ulong *buf, double x, double y, double dx, double dy,
			   int w, int h, int sx, int sy, int ppl)
{
register int	ex,ix,ey,iy,intx,inty,t;
register ulong	val=0;
unsigned char	r,g,b;
double		cx;
ulong		*ptr=NULL;
ImageData	*id;

if(ppl) ppl-=w;
ex=sx+w;
ey=sy+h;
id=img->get_data();

if(interpolate) {
    for(iy=sy;iy<ey;iy++) {
	cx=x;
	for(ix=sx;ix<ex;ix++) {
		id->iget_color(cx,y,&r,&g,&b);
		*(buf++)=(((ulong)r)<<rshift)|(((ulong)g)<<gshift)|(((ulong)b)<<bshift);
		cx+=dx;
		}
	buf+=ppl;
	y+=dy;
	}
    }
else {
  x+=0.500001; y+=0.500001;
  if(dx>=1.0) {
    for(iy=sy;iy<ey;iy++) {
	cx=x;
	inty=(int)y;
	for(ix=sx;ix<ex;ix++) {
		id->get_color((int)cx,inty,&r,&g,&b);
		*(buf++)=(((ulong)r)<<rshift)|(((ulong)g)<<gshift)|(((ulong)b)<<bshift);
		cx+=dx;
		}
	buf+=ppl;
	y+=dy;
	}
    }
  else {
    inty=-123456789;
    for(iy=sy;iy<ey;iy++) {
	if((t=(int)y)==inty) {
	    memcpy(buf,ptr,w<<2);
	    buf+=w+ppl;
	    }
	else {
	    inty=t; ptr=buf;
	    cx=x; intx=-123456789;
	    for(ix=sx;ix<ex;ix++) {
		if((t=(int)cx)==intx)
			*(buf++)=val;
		else {	intx=t;
			id->get_color(intx,inty,&r,&g,&b);
			val=*(buf++)=(((ulong)r)<<rshift)|(((ulong)g)<<gshift)|(((ulong)b)<<bshift);
			}
		cx+=dx;
		}
	    buf+=ppl;
	    }
	y+=dy;
	}
    }
  }
}



// DispMap  -  For PseudoColor or TrueColor displays, data with a colormap:


void DispMap::free_map()
{
if(map_size && pixel_map && display && colormap)
	RFreeColors(display,colormap,pixel_map,map_size);
map_size=0;
if(pixel_map) delete[] pixel_map;
pixel_map=NULL;
}


int DispMap::initialize()
{
XVisualInfo	vinfo,*v;
XWindowAttributes xwa;
int		screen,i,try_again;
Image		*map;

if(!display || !img) return 0;

free_map();

map=img->get_map();
if(!map) return 0;

try_again=0;

if(window) {
	i=XGetWindowAttributes(display,window,&xwa);
	if(!i) return 0;
	screen=XScreenNumberOfScreen(xwa.screen);
	}
else	screen=DefaultScreen(display);

if(!visual) {
	if(!colormap) {
		visual=DefaultVisual(display,screen);
		depth=DefaultDepth(display,screen);
		}
	}
else {	vinfo.visualid=XVisualIDFromVisual(visual);
	v=XGetVisualInfo(display,VisualIDMask,&vinfo,&i);
	if(!v) return 0;
	depth=v->depth;
	XFree(v);
	}
if(!colormap) {
	if(create_private==PRIVATE_ALWAYS || visual!=DefaultVisual(display,screen)) {
		if(window) colormap=XCreateColormap(display,window,visual,AllocNone);
		else	colormap=XCreateColormap(display,RootWindow(display,
				DefaultScreen(display)),visual,AllocNone);
		if(!colormap) return 0;
		}
	else {	colormap=DefaultColormap(display,screen);
		try_again=1;
		}
	}
else	if(create_private==PRIVATE_IF_NEEDED) try_again=1;

// Allocate colors:
map_size=map->get_res();
if(!map_size) return 0;
pixel_map=new Pixel[map_size];
for(i=0;i<map_size;i++) {
	if(!RAllocColor(display,colormap,&pixel_map[i],
	   map->get_red16(i),map->get_green16(i),map->get_blue16(i))) {
		if(try_again && visual) {
			RFreeColors(display,colormap,pixel_map,i);
			try_again=0;
			i=-1;
			if(window) colormap=XCreateColormap(display,
						window,visual,AllocNone);
			else	colormap=XCreateColormap(display,
					RootWindow(display,DefaultScreen(
					display)),visual,AllocNone);
			if(!colormap) { delete[] pixel_map; pixel_map=NULL;
				return 0;
				}
			}
		else {	map_size=i;
			free_map();
			return 0;
			}
		}
	}

return 1;
}


Pixel DispMap::pixel(int x, int y)
{
long	i;

i=(img->get_data())->get_long(x,y);
if(i>=map_size || i<0) i=0;
return pixel_map[i];
}


Pixel DispMap::ipixel(double x, double y)
{
long	i;

i=(img->get_data())->iget_long(x,y);
if(i>=map_size || i<0) i=0;
return pixel_map[i];
}


Pixel DispMap::dpixel(int x, int y, int, int)
{
long	i;

i=(img->get_data())->get_long(x,y);
if(i>=map_size || i<0) i=0;
return pixel_map[i];
}


Pixel DispMap::idpixel(double x, double y, int, int)
{
long	i;

i=(img->get_data())->iget_long(x,y);
if(i>=map_size || i<0) i=0;
return pixel_map[i];
}


void DispMap::get_area8(uchar *buf, int ix, int iy, int w, int h, int ppl)
{
register int	ex,x,ey,y;
long		i;
ImageData	*id;

if(ppl) ppl-=w;
ex=ix+w;
ey=iy+h;
id=img->get_data();

if(interpolate) {
    for(y=iy;y<ey;y++) {
	for(x=ix;x<ex;x++) {
		i=id->iget_long((double)x,(double)y);
		if(i>=map_size || i<0) i=0;
		*(buf++)=(uchar)pixel_map[i];
		}
	buf+=ppl;
	}
    }
else {
    for(y=iy;y<ey;y++) {
	for(x=ix;x<ex;x++) {
		i=id->get_long(x,y);
		if(i>=map_size || i<0) i=0;
		*(buf++)=(uchar)pixel_map[i];
		}
	buf+=ppl;
	}
    }

}


void DispMap::iget_area8(uchar *buf, double x, double y, double dx, double dy,
			   int w, int h, int sx, int sy, int ppl)
{
register int	ex,ix,ey,iy,intx,inty,t;
register uchar	val=0;
long		i;
double		cx;
ImageData	*id;

if(ppl) ppl-=w;
ex=sx+w;
ey=sy+h;
id=img->get_data();

if(interpolate) {
    for(iy=sy;iy<ey;iy++) {
	cx=x;
	for(ix=sx;ix<ex;ix++) {
		i=id->iget_long(cx,y);
		if(i>=map_size || i<0) i=0;
		*(buf++)=(uchar)pixel_map[i];
		cx+=dx;
		}
	buf+=ppl;
	y+=dy;
	}
    }
else {
  x+=0.500001; y+=0.500001;
  if(dx>=1.0) {
    for(iy=sy;iy<ey;iy++) {
	cx=x;
	inty=(int)y;
	for(ix=sx;ix<ex;ix++) {
		i=id->get_long((int)cx,inty);
		if(i>=map_size || i<0) i=0;
		*(buf++)=(uchar)pixel_map[i];
		cx+=dx;
		}
	buf+=ppl;
	y+=dy;
	}
    }
  else {
    for(iy=sy;iy<ey;iy++) {
	cx=x;
	inty=(int)y; intx=-123456789;
	for(ix=sx;ix<ex;ix++) {
		if((t=(int)cx)==intx)
			*(buf++)=val;
		else {	intx=t;
			i=id->get_long(intx,inty);
			if(i>=map_size || i<0) i=0;
			val=*(buf++)=(uchar)pixel_map[i];
			}
		cx+=dx;
		}
	buf+=ppl;
	y+=dy;
	}
    }
  }
}


void DispMap::get_area32(ulong *buf, int ix, int iy, int w, int h, int ppl)
{
register int	ex,x,ey,y;
long		i;
ImageData	*id;

if(ppl) ppl-=w;
ex=ix+w;
ey=iy+h;
id=img->get_data();

if(interpolate) {
    for(y=iy;y<ey;y++) {
	for(x=ix;x<ex;x++) {
		i=id->iget_long((double)x,(double)y);
		if(i>=map_size || i<0) i=0;
		*(buf++)=(ulong)pixel_map[i];
		}
	buf+=ppl;
	}
    }
else {
    for(y=iy;y<ey;y++) {
	for(x=ix;x<ex;x++) {
		i=id->get_long(x,y);
		if(i>=map_size || i<0) i=0;
		*(buf++)=(ulong)pixel_map[i];
		}
	buf+=ppl;
	}
    }
}


void DispMap::iget_area32(ulong *buf, double x, double y, double dx, double dy,
			   int w, int h, int sx, int sy, int ppl)
{
register int	ex,ix,ey,iy,intx,inty,t;
register ulong	val=0;
long		i;
double		cx;
ImageData	*id;

if(ppl) ppl-=w;
ex=sx+w;
ey=sy+h;
id=img->get_data();

if(interpolate) {
    for(iy=sy;iy<ey;iy++) {
	cx=x;
	for(ix=sx;ix<ex;ix++) {
		i=id->iget_long(cx,y);
		if(i>=map_size || i<0) i=0;
		*(buf++)=(ulong)pixel_map[i];
		cx+=dx;
		}
	buf+=ppl;
	y+=dy;
	}
    }
else {
  x+=0.500001; y+=0.500001;
  if(dx>=1.0) {
    for(iy=sy;iy<ey;iy++) {
	cx=x;
	inty=(int)y;
	for(ix=sx;ix<ex;ix++) {
		i=id->get_long((int)cx,inty);
		if(i>=map_size || i<0) i=0;
		*(buf++)=(ulong)pixel_map[i];
		cx+=dx;
		}
	buf+=ppl;
	y+=dy;
	}
    }
  else {
    for(iy=sy;iy<ey;iy++) {
	cx=x;
	inty=(int)y; intx=-123456789;
	for(ix=sx;ix<ex;ix++) {
		if((t=(int)cx)==intx)
			*(buf++)=val;
		else {	intx=t;
			i=id->get_long(intx,inty);
			if(i>=map_size || i<0) i=0;
			val=*(buf++)=(ulong)pixel_map[i];
			}
		cx+=dx;
		}
	buf+=ppl;
	y+=dy;
	}
    }
  }
}



// Disp8  -  For PseudoColor displays, any type of data:

// Default color component shade count table:
int	default_tri[36]={ 6,8,5, 6,7,5, 6,7,4, 5,6,4, 4,5,4, 4,5,3,
			  4,4,3, 3,4,3, 3,3,3, 3,3,2, 2,3,2, 2,2,2 };
#define	default_num_tri	12


void Disp8::init()
{
rnum=0; gnum=0; bnum=0;

set_tri_table(default_tri,default_num_tri);
}


void Disp8::set_tri_table(int *tab, int num)
{
tri=tab;
num_tri=num;
}


int Disp8::initialize()
{
XVisualInfo	vinfo,*v;
XWindowAttributes xwa;
int		screen,i,j,r,g,b,try_again;

if(!display) return 0;

free_map();

try_again=0;

if(window) {
	i=XGetWindowAttributes(display,window,&xwa);
	if(!i) return 0;
	screen=XScreenNumberOfScreen(xwa.screen);
	}
else	screen=DefaultScreen(display);

if(!visual) {
	if(!colormap) {
		visual=DefaultVisual(display,screen);
		depth=DefaultDepth(display,screen);
		}
	}
else {	vinfo.visualid=XVisualIDFromVisual(visual);
	v=XGetVisualInfo(display,VisualIDMask,&vinfo,&i);
	if(!v) return 0;
	depth=v->depth;
	XFree(v);
	}
if(!colormap) {
	if(create_private==PRIVATE_ALWAYS || visual!=DefaultVisual(display,screen)) {
		if(window) colormap=XCreateColormap(display,window,visual,AllocNone);
		else	colormap=XCreateColormap(display,RootWindow(display,
				DefaultScreen(display)),visual,AllocNone);
		if(!colormap) return 0;
		}
	else {	colormap=DefaultColormap(display,screen);
		try_again=1;
		}
	}
else	if(create_private==PRIVATE_IF_NEEDED) try_again=1;

// Allocate colors:
for(j=0;j<num_tri;j++) {
  rnum=tri[j*3]; gnum=tri[j*3+1]; bnum=tri[j*3+2];
  map_size=rnum*gnum*bnum;
  if(!map_size) return 0;
  pixel_map=new Pixel[map_size];
  i=0;
  for(b=0;b<bnum;b++) {
    for(g=0;g<gnum;g++) {
      for(r=0;r<rnum;r++) {
	if(!RAllocColor(display,colormap,&pixel_map[i],
	   (double)r/(rnum-1.0),(double)g/(gnum-1.0),(double)b/(bnum-1.0))) {
		map_size=i;
		free_map();
		r=rnum; g=gnum; b=bnum;
		if(j==num_tri-1) {
		    if(try_again && visual) {
			try_again=0;
			j=-1;
			if(window) colormap=XCreateColormap(display,
						window,visual,AllocNone);
			else	colormap=XCreateColormap(display,
					RootWindow(display,DefaultScreen(
					display)),visual,AllocNone);
			if(!colormap) return 0;
			}
		    else return 0;
		    }
		}
	i++;
	}
      }
    }
  if(pixel_map) j=num_tri;	// suceeded
  }

comp_shade();
comp_opmap();

return 1;
}


void Disp8::comp_shade()
{
int	i;

for(i=0;i<256;i++) {
	rcomp[i]=(int)((double)i*0.003921568*(double)(rnum-1)+0.5);
	gcomp[i]=(int)((double)i*0.003921568*(double)(gnum-1)+0.5)<<4;
	bcomp[i]=(int)((double)i*0.003921568*(double)(bnum-1)+0.5)<<8;
	}
}


void Disp8::comp_opmap()
{
int	r,g,b,i;

i=0;
for(b=0;b<bnum;b++)
    for(g=0;g<gnum;g++)
      	for(r=0;r<rnum;r++)
		opix_map[r|(g<<4)|(b<<8)]=pixel_map[i++];
}


Pixel Disp8::pixel(int x, int y)
{
unsigned char	r,g,b;

img->get_color(x,y,&r,&g,&b);
return opix_map[rcomp[r]|gcomp[g]|bcomp[b]];
}


Pixel Disp8::ipixel(double x, double y)
{
unsigned char	r,g,b;

(img->get_data())->iget_color(x,y,&r,&g,&b);
return opix_map[rcomp[r]|gcomp[g]|bcomp[b]];
}


Pixel Disp8::dpixel(int x, int y, int, int)
{
unsigned char	r,g,b;

img->get_color(x,y,&r,&g,&b);
return opix_map[rcomp[r]|gcomp[g]|bcomp[b]];
}


Pixel Disp8::idpixel(double x, double y, int, int)
{
unsigned char	r,g,b;

(img->get_data())->iget_color(x,y,&r,&g,&b);
return opix_map[rcomp[r]|gcomp[g]|bcomp[b]];
}


void Disp8::get_area8(uchar *buf, int ix, int iy, int w, int h, int ppl)
{
register int	ex,x,ey,y;
unsigned char	r,g,b;
ImageData	*id;

if(ppl) ppl-=w;
ex=ix+w;
ey=iy+h;

if(interpolate) {
    id=img->get_data();
    for(y=iy;y<ey;y++) {
	for(x=ix;x<ex;x++) {
		id->iget_color((double)x,(double)y,&r,&g,&b);
		*(buf++)=(uchar)opix_map[rcomp[r]|gcomp[g]|bcomp[b]];
		}
	buf+=ppl;
	}
    }
else {
    for(y=iy;y<ey;y++) {
	for(x=ix;x<ex;x++) {
		img->get_color(x,y,&r,&g,&b);
		*(buf++)=(uchar)opix_map[rcomp[r]|gcomp[g]|bcomp[b]];
		}
	buf+=ppl;
	}
    }
}


void Disp8::iget_area8(uchar *buf, double x, double y, double dx, double dy,
			   int w, int h, int sx, int sy, int ppl)
{
register int	ex,ix,ey,iy,intx,inty,t;
register uchar	val=0;
unsigned char	r,g,b;
double		cx;
ImageData	*id;

if(ppl) ppl-=w;
ex=sx+w;
ey=sy+h;
id=img->get_data();

if(interpolate) {
    for(iy=sy;iy<ey;iy++) {
	cx=x;
	for(ix=sx;ix<ex;ix++) {
		id->iget_color(cx,y,&r,&g,&b);
		*(buf++)=(uchar)opix_map[rcomp[r]|gcomp[g]|bcomp[b]];
		cx+=dx;
		}
	buf+=ppl;
	y+=dy;
	}
    }
else {
  x+=0.500001; y+=0.500001;
  if(dx>=1.0) {
    for(iy=sy;iy<ey;iy++) {
	cx=x;
	inty=(int)y;
	for(ix=sx;ix<ex;ix++) {
		id->get_color((int)cx,inty,&r,&g,&b);
		*(buf++)=(uchar)opix_map[rcomp[r]|gcomp[g]|bcomp[b]];
		cx+=dx;
		}
	buf+=ppl;
	y+=dy;
	}
    }
  else {
    for(iy=sy;iy<ey;iy++) {
	cx=x;
	inty=(int)y; intx=-123456789;
	for(ix=sx;ix<ex;ix++) {
		if((t=(int)cx)==intx)
			*(buf++)=val;
		else {	intx=t;
			id->get_color(intx,inty,&r,&g,&b);
			val=*(buf++)=(uchar)opix_map[rcomp[r]|gcomp[g]|bcomp[b]];
			}
		cx+=dx;
		}
	buf+=ppl;
	y+=dy;
	}
    }
  }
}


void Disp8::get_area32(ulong *buf, int ix, int iy, int w, int h, int ppl)
{
register int	ex,x,ey,y;
unsigned char	r,g,b;
ImageData	*id;

if(ppl) ppl-=w;
ex=ix+w;
ey=iy+h;

if(interpolate) {
    id=img->get_data();
    for(y=iy;y<ey;y++) {
	for(x=ix;x<ex;x++) {
		id->iget_color((double)x,(double)y,&r,&g,&b);
		*(buf++)=(ulong)opix_map[rcomp[r]|gcomp[g]|bcomp[b]];
		}
	buf+=ppl;
	}
    }
else {
    for(y=iy;y<ey;y++) {
	for(x=ix;x<ex;x++) {
		img->get_color(x,y,&r,&g,&b);
		*(buf++)=(ulong)opix_map[rcomp[r]|gcomp[g]|bcomp[b]];
		}
	buf+=ppl;
	}
    }
}


void Disp8::iget_area32(ulong *buf, double x, double y, double dx, double dy,
			   int w, int h, int sx, int sy, int ppl)
{
register int	ex,ix,ey,iy,intx,inty,t;
register ulong	val=0;
unsigned char	r,g,b;
double		cx;
ImageData	*id;

if(ppl) ppl-=w;
ex=sx+w;
ey=sy+h;
id=img->get_data();

if(interpolate) {
    for(iy=sy;iy<ey;iy++) {
	cx=x;
	for(ix=sx;ix<ex;ix++) {
		id->iget_color(cx,y,&r,&g,&b);
		*(buf++)=(ulong)opix_map[rcomp[r]|gcomp[g]|bcomp[b]];
		cx+=dx;
		}
	buf+=ppl;
	y+=dy;
	}
    }
else {
  x+=0.500001; y+=0.500001;
  if(dx>=1.0) {
    for(iy=sy;iy<ey;iy++) {
	cx=x;
	inty=(int)y;
	for(ix=sx;ix<ex;ix++) {
		id->get_color((int)cx,inty,&r,&g,&b);
		*(buf++)=(ulong)opix_map[rcomp[r]|gcomp[g]|bcomp[b]];
		cx+=dx;
		}
	buf+=ppl;
	y+=dy;
	}
    }
  else {
    for(iy=sy;iy<ey;iy++) {
	cx=x;
	inty=(int)y; intx=-123456789;
	for(ix=sx;ix<ex;ix++) {
		if((t=(int)cx)==intx)
			*(buf++)=val;
		else {	intx=t;
			id->get_color(intx,inty,&r,&g,&b);
			val=*(buf++)=(ulong)opix_map[rcomp[r]|gcomp[g]|bcomp[b]];
			}
		cx+=dx;
		}
	buf+=ppl;
	y+=dy;
	}
    }
  }
}



// DispDither  -  For PseudoColor displays, any type of data, dithered:

// Data for default dither pattern:
//#define	default_dither_xres	4
//#define	default_dither_yres	4
//uchar	default_dither_data[default_dither_yres][default_dither_xres]=
//		{ {0,8,6,14},{12,2,10,4},{7,15,1,9},{13,3,11,5} };

// Semi-ordered:
#define	default_dither_xres	8
#define	default_dither_yres	8
uchar	default_dither_data[default_dither_yres][default_dither_xres]={
		{  0,32,24,56, 2,34,26,58 },
		{ 48, 8,40,16,50,10,42,18 },
		{ 28,60, 4,36,30,62, 6,38 },
		{ 52,12,44,20,54,14,46,22 },
		{  3,35,27,59, 1,33,25,57 },
		{ 51,11,43,19,49, 9,41,17 },
		{ 31,63, 7,39,29,61, 5,37 },
		{ 55,15,47,23,53,13,45,21 } };

Image	*default_dither_image=NULL;


void DispDither::init()
{
int		x,y;

dtab=NULL;
dither_img=NULL;
xmodtab=NULL;
ymodtab=NULL;
dxres=0;

if(!default_dither_image) {
	default_dither_image=gray_image(default_dither_xres,default_dither_yres);
	for(y=0;y<default_dither_yres;y++)
		for(x=0;x<default_dither_xres;x++)
			(*default_dither_image)(x,y)=default_dither_data[y][x];
	}
set_dither(default_dither_image);
}


void DispDither::comp_shade()
{
int	i,xres,yres,n;
double	n2;

dither_img->get_res(&xres,&yres);
n=xres*yres;
n2=0.5/(double)n;

for(i=0;i<256;i++) {
	rcomp[i]=(int)((double)i*0.0039215687*(double)(rnum-1)+n2);
	gcomp[i]=(int)((double)i*0.0039215687*(double)(gnum-1)+n2)<<4;
	bcomp[i]=(int)((double)i*0.0039215687*(double)(bnum-1)+n2)<<8;
	rcomp1[i]=rcomp[i]+1;
	gcomp1[i]=gcomp[i]+16;
	bcomp1[i]=bcomp[i]+256;
	rind[i]=(int)((double)i*0.0039215687*(double)(rnum-1)*n+0.5)%n;
	gind[i]=(int)((double)i*0.0039215687*(double)(gnum-1)*n+0.5)%n;
	bind[i]=(int)((double)i*0.0039215687*(double)(bnum-1)*n+0.5)%n;
	}
rcomp1[255]=rcomp[255];
gcomp1[255]=gcomp[255];
bcomp1[255]=bcomp[255];
}


int DispDither::set_dither(Image *limg)
{
int	xres,yres,x,y,*iptr;
ImageData	*id;

dither_img=limg;
if(dtab) { free((char *)dtab); dtab=NULL; }
id=dither_img->get_data();
if(!id) return 0;

dither_img->get_res(&xres,&yres);
if(!xres||!yres) return 0;

dtab=(int *)malloc(xres*yres*sizeof(int));
if(!dtab) return 0;

if(!xmodtab) xmodtab=new int[16384];
if(!ymodtab) ymodtab=new int[16384];
for(x=0;x<16384;x++) xmodtab[x]=(x%xres);
for(y=0;y<16384;y++) ymodtab[y]=(y%yres)*xres;

iptr=dtab;
for(y=0;y<yres;y++) {
	for(x=0;x<xres;x++) *(iptr++)=(int)id->get_long(x,y);
	}
dxres=xres;

if(pixel_map && rnum && gnum && bnum) comp_shade();

return 1;
}


Pixel DispDither::pixel(int x, int y)
{
unsigned char	r,g,b;
int	dval;

img->get_color(x,y,&r,&g,&b);
dval=dtab[ymodtab[y&16383]+xmodtab[x&16383]];
return opix_map[(rind[r]>dval?rcomp1[r]:rcomp[r])|
		(gind[g]>dval?gcomp1[g]:gcomp[g])|
		(bind[b]>dval?bcomp1[b]:bcomp[b])];
}


Pixel DispDither::ipixel(double x, double y)
{
unsigned char	r,g,b;
int	dval;

(img->get_data())->iget_color(x,y,&r,&g,&b);
dval=dtab[ymodtab[(int)y&16383]+xmodtab[(int)x&16383]];
return opix_map[(rind[r]>dval?rcomp1[r]:rcomp[r])|
		(gind[g]>dval?gcomp1[g]:gcomp[g])|
		(bind[b]>dval?bcomp1[b]:bcomp[b])];
}


Pixel DispDither::dpixel(int x, int y, int dx, int dy)
{
unsigned char	r,g,b;
int	dval;

img->get_color(x,y,&r,&g,&b);
dval=dtab[ymodtab[dy&16383]+xmodtab[dx&16383]];
return opix_map[(rind[r]>dval?rcomp1[r]:rcomp[r])|
		(gind[g]>dval?gcomp1[g]:gcomp[g])|
		(bind[b]>dval?bcomp1[b]:bcomp[b])];
}


Pixel DispDither::idpixel(double x, double y, int dx, int dy)
{
unsigned char	r,g,b;
int	dval;

(img->get_data())->iget_color(x,y,&r,&g,&b);
dval=dtab[ymodtab[dy&16383]+xmodtab[dx&16383]];
return opix_map[(rind[r]>dval?rcomp1[r]:rcomp[r])|
		(gind[g]>dval?gcomp1[g]:gcomp[g])|
		(bind[b]>dval?bcomp1[b]:bcomp[b])];
}


void DispDither::get_area8(uchar *buf, int ix, int iy, int w, int h, int ppl)
{
register int	ex,x,ey,y,*dval,*d1;
unsigned char	r,g,b;
ImageData	*id;

if(ppl) ppl-=w;
ex=ix+w;
ey=iy+h;

if(interpolate) {
    id=img->get_data();
    for(y=iy;y<ey;y++) {
	d1=dtab+ymodtab[y&16383];
	dval=d1+xmodtab[ix&16383];
	d1+=dxres;
	for(x=ix;x<ex;x++) {
		id->iget_color((double)x,(double)y,&r,&g,&b);
		*(buf++)=(uchar)opix_map[(rind[r]>*dval?rcomp1[r]:rcomp[r])|
					 (gind[g]>*dval?gcomp1[g]:gcomp[g])|
					 (bind[b]>*dval?bcomp1[b]:bcomp[b])];
		dval++; if(dval==d1) dval-=dxres;
		}
	buf+=ppl;
	}
    }
else {
    for(y=iy;y<ey;y++) {
	d1=dtab+ymodtab[y&16383];
	dval=d1+xmodtab[ix&16383];
	d1+=dxres;
	for(x=ix;x<ex;x++) {
		img->get_color(x,y,&r,&g,&b);
		*(buf++)=(uchar)opix_map[(rind[r]>*dval?rcomp1[r]:rcomp[r])|
					 (gind[g]>*dval?gcomp1[g]:gcomp[g])|
					 (bind[b]>*dval?bcomp1[b]:bcomp[b])];
		dval++; if(dval==d1) dval-=dxres;
		}
	buf+=ppl;
	}
    }
}


void DispDither::iget_area8(uchar *buf, double x, double y, double dx, double dy,
			   int w, int h, int sx, int sy, int ppl)
{
register int	ex,ix,ey,iy,intx,inty,t,*dval,*d1;
unsigned char	r,g,b;
double		cx;
ImageData	*id;

if(ppl) ppl-=w;
ex=sx+w;
ey=sy+h;
id=img->get_data();

if(interpolate) {
    for(iy=sy;iy<ey;iy++) {
	d1=dtab+ymodtab[iy&16383];
	dval=d1+xmodtab[sx&16383];
	d1+=dxres;
	cx=x;
	for(ix=sx;ix<ex;ix++) {
		id->iget_color(cx,y,&r,&g,&b);
		*(buf++)=(uchar)opix_map[(rind[r]>*dval?rcomp1[r]:rcomp[r])|
					 (gind[g]>*dval?gcomp1[g]:gcomp[g])|
					 (bind[b]>*dval?bcomp1[b]:bcomp[b])];
		dval++; if(dval==d1) dval-=dxres;
		cx+=dx;
		}
	buf+=ppl;
	y+=dy;
	}
    }
else {
  x+=0.500001; y+=0.500001;
  if(dx>=1.0) {
    for(iy=sy;iy<ey;iy++) {
	d1=dtab+ymodtab[iy&16383];
	dval=d1+xmodtab[sx&16383];
	d1+=dxres;
	cx=x;
	inty=(int)y;
	for(ix=sx;ix<ex;ix++) {
		id->get_color((int)cx,inty,&r,&g,&b);
		*(buf++)=(uchar)opix_map[(rind[r]>*dval?rcomp1[r]:rcomp[r])|
					 (gind[g]>*dval?gcomp1[g]:gcomp[g])|
					 (bind[b]>*dval?bcomp1[b]:bcomp[b])];
		dval++; if(dval==d1) dval-=dxres;
		cx+=dx;
		}
	buf+=ppl;
	y+=dy;
	}
    }
  else {
    for(iy=sy;iy<ey;iy++) {
	d1=dtab+ymodtab[iy&16383];
	dval=d1+xmodtab[sx&16383];
	d1+=dxres;
	cx=x;
	inty=(int)y; intx=-123456789;
	for(ix=sx;ix<ex;ix++) {
		if((t=(int)cx)!=intx) {
			intx=t;
			id->get_color(intx,inty,&r,&g,&b);
			}
		*(buf++)=(uchar)opix_map[(rind[r]>*dval?rcomp1[r]:rcomp[r])|
					 (gind[g]>*dval?gcomp1[g]:gcomp[g])|
					 (bind[b]>*dval?bcomp1[b]:bcomp[b])];
		dval++; if(dval==d1) dval-=dxres;
		cx+=dx;
		}
	buf+=ppl;
	y+=dy;
	}
    }
  }
}


void DispDither::get_area32(ulong *buf, int ix, int iy, int w, int h, int ppl)
{
register int	ex,x,ey,y,*dval,*d1;
unsigned char	r,g,b;
ImageData	*id;

if(ppl) ppl-=w;
ex=ix+w;
ey=iy+h;

if(interpolate) {
    id=img->get_data();
    for(y=iy;y<ey;y++) {
	d1=dtab+ymodtab[y&16383];
	dval=d1+xmodtab[ix&16383];
	d1+=dxres;
	for(x=ix;x<ex;x++) {
		id->iget_color((double)x,(double)y,&r,&g,&b);
		*(buf++)=(ulong)opix_map[(rind[r]>*dval?rcomp1[r]:rcomp[r])|
					 (gind[g]>*dval?gcomp1[g]:gcomp[g])|
					 (bind[b]>*dval?bcomp1[b]:bcomp[b])];
		dval++; if(dval==d1) dval-=dxres;
		}
	buf+=ppl;
	}
    }
else {
    for(y=iy;y<ey;y++) {
	d1=dtab+ymodtab[y&16383];
	dval=d1+xmodtab[ix&16383];
	d1+=dxres;
	for(x=ix;x<ex;x++) {
		img->get_color(x,y,&r,&g,&b);
		*(buf++)=(ulong)opix_map[(rind[r]>*dval?rcomp1[r]:rcomp[r])|
					 (gind[g]>*dval?gcomp1[g]:gcomp[g])|
					 (bind[b]>*dval?bcomp1[b]:bcomp[b])];
		dval++; if(dval==d1) dval-=dxres;
		}
	buf+=ppl;
	}
    }
}


void DispDither::iget_area32(ulong *buf, double x, double y, double dx, double dy,
			   int w, int h, int sx, int sy, int ppl)
{
register int	ex,ix,ey,iy,intx,inty,t,*dval,*d1;
unsigned char	r,g,b;
double		cx;
ImageData	*id;

if(ppl) ppl-=w;
ex=sx+w;
ey=sy+h;
id=img->get_data();

if(interpolate) {
    for(iy=sy;iy<ey;iy++) {
	d1=dtab+ymodtab[iy&16383];
	dval=d1+xmodtab[sx&16383];
	d1+=dxres;
	cx=x;
	for(ix=sx;ix<ex;ix++) {
		id->iget_color(cx,y,&r,&g,&b);
		*(buf++)=(ulong)opix_map[(rind[r]>*dval?rcomp1[r]:rcomp[r])|
					 (gind[g]>*dval?gcomp1[g]:gcomp[g])|
					 (bind[b]>*dval?bcomp1[b]:bcomp[b])];
		dval++; if(dval==d1) dval-=dxres;
		cx+=dx;
		}
	buf+=ppl;
	y+=dy;
	}
    }
else {
  x+=0.500001; y+=0.500001;
  if(dx>=1.0) {
    for(iy=sy;iy<ey;iy++) {
	d1=dtab+ymodtab[iy&16383];
	dval=d1+xmodtab[sx&16383];
	d1+=dxres;
	cx=x;
	inty=(int)y;
	for(ix=sx;ix<ex;ix++) {
		id->get_color((int)cx,inty,&r,&g,&b);
		*(buf++)=(ulong)opix_map[(rind[r]>*dval?rcomp1[r]:rcomp[r])|
					 (gind[g]>*dval?gcomp1[g]:gcomp[g])|
					 (bind[b]>*dval?bcomp1[b]:bcomp[b])];
		dval++; if(dval==d1) dval-=dxres;
		cx+=dx;
		}
	buf+=ppl;
	y+=dy;
	}
    }
  else {
    for(iy=sy;iy<ey;iy++) {
	d1=dtab+ymodtab[iy&16383];
	dval=d1+xmodtab[sx&16383];
	d1+=dxres;
	cx=x;
	inty=(int)y; intx=-123456789;
	for(ix=sx;ix<ex;ix++) {
		if((t=(int)cx)!=intx) {
			intx=t;
			id->get_color(intx,inty,&r,&g,&b);
			}
		*(buf++)=(ulong)opix_map[(rind[r]>*dval?rcomp1[r]:rcomp[r])|
					 (gind[g]>*dval?gcomp1[g]:gcomp[g])|
					 (bind[b]>*dval?bcomp1[b]:bcomp[b])];
		dval++; if(dval==d1) dval-=dxres;
		cx+=dx;
		}
	buf+=ppl;
	y+=dy;
	}
    }
  }
}


DispDither::~DispDither()
{
if(xmodtab) delete[] xmodtab;
if(ymodtab) delete[] ymodtab;
if(dtab) free((char *)dtab);
}

#endif // _NO_IMAGEDISP_
