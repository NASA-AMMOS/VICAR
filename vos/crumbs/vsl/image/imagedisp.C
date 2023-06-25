// imagedisp.C
//
// Written by Dave Kagels 10/11/94

#include <stdint.h>
#include "image/imagedisp.h"
#include "image/image.h"


void ImageDisp::init()
{
img=NULL;

#ifndef _NO_IMAGEDISP_
display=NULL;
visual=NULL;
colormap=0;
window=0;
#endif // _NO_IMAGEDISP_
depth=0;
create_private=PRIVATE_IF_NEEDED;
interpolate=1;
}

#ifndef _NO_IMAGEDISP_
int ImageDisp::set_window_colormap(Window w)
{
if(w) set_window(w);
if(!window || !display || !colormap) return 0;

XSetWindowColormap(display,window,colormap);
return 1;
}


Pixel ImageDisp::dpixel(int x, int y, int, int)
{
return pixel(x,y);
}


Pixel ImageDisp::idpixel(double x, double y, int, int)
{
return ipixel(x,y);
}


void ImageDisp::get_area8(uchar *buf, int ix, int iy, int w, int h, int ppl)
{
register int	ex,x,ey,y;

if(ppl) ppl-=w;
ex=ix+w;
ey=iy+h;

for(y=iy;y<ey;y++) {
	for(x=ix;x<ex;x++) *(buf++)=(uchar)pixel(x,y);
	buf+=ppl;
	}
}


void ImageDisp::iget_area8(uchar *buf, double x, double y, double dx, double dy,
			   int w, int h, int sx, int sy, int ppl)
{
register int	ex,ix,ey,iy,inty;
double	cx;

if(ppl) ppl-=w;
ex=sx+w;
ey=sy+h;

if(interpolate) {
    x-=0.5; y-=0.5;
    for(iy=sy;iy<ey;iy++) {
	cx=x;
	for(ix=sx;ix<ex;ix++) {
		*(buf++)=(uchar)idpixel(cx,y,ix,iy);
		cx+=dx;
		}
	buf+=ppl;
	y+=dy;
	}
    }
else {
    x+=0.000001; y+=0.000001;
    for(iy=sy;iy<ey;iy++) {
	cx=x;
	inty=(int)y;
	for(ix=sx;ix<ex;ix++) {
		*(buf++)=(uchar)dpixel((int)cx,inty,ix,iy);
		cx+=dx;
		}
	buf+=ppl;
	y+=dy;
	}
    }
}


void ImageDisp::get_area32(ulong *buf, int ix, int iy, int w, int h, int ppl)
{
register int	ex,x,ey,y;

if(ppl) ppl-=w;
ex=ix+w;
ey=iy+h;

for(y=iy;y<ey;y++) {
	for(x=ix;x<ex;x++) *(buf++)=(ulong)pixel(x,y);
	buf+=ppl;
	}
}


void ImageDisp::iget_area32(ulong *buf, double x, double y, double dx, double dy,
			   int w, int h, int sx, int sy, int ppl)
{
register int	ex,ix,ey,iy,inty;
double	cx;

if(ppl) ppl-=w;
ex=sx+w;
ey=sy+h;

if(interpolate) {
    x-=0.5; y-=0.5;
    for(iy=sy;iy<ey;iy++) {
	cx=x;
	for(ix=sx;ix<ex;ix++) {
		*(buf++)=(ulong)idpixel(cx,y,ix,iy);
		cx+=dx;
		}
	buf+=ppl;
	y+=dy;
	}
    }
else {
    x+=0.000001; y+=0.000001;
    for(iy=sy;iy<ey;iy++) {
	cx=x;
	inty=(int)y;
	for(ix=sx;ix<ex;ix++) {
		*(buf++)=(ulong)dpixel((int)cx,inty,ix,iy);
		cx+=dx;
		}
	buf+=ppl;
	y+=dy;
	}
    }
}


int ImageDisp::fill_ximage(XImage *ximg, int xx, int xy, int ix, int iy, int w, int h)
{
if(!img || ximg->format!=ZPixmap || !ximg->data) return 0;

if(!w)	w=ximg->width-xx;
if(!h)	h=ximg->height-xy;
if(w<=0 || xx+w>ximg->width || h<=0 || xy+h>ximg->height) return 0;

if(ximg->bits_per_pixel==8) {
	get_area8((uchar *)ximg->data+ximg->xoffset+xy*ximg->bytes_per_line+xx,ix,iy,
			w,h,ximg->bytes_per_line);
	return 1;
	}
if(ximg->bits_per_pixel==32) {
	get_area32((ulong *)(ximg->data+xy*ximg->bytes_per_line)+ximg->xoffset+xx,ix,iy,
			w,h,ximg->bytes_per_line>>2);
	return 1;
	}
return 0;
}


int ImageDisp::ifill_ximage(XImage *ximg, double x, double y, double dx, double dy,
			    int xx, int xy, int w, int h, int sx, int sy)
{
if(!img || ximg->format!=ZPixmap || !ximg->data) return 0;

if(!w)	w=ximg->width-xx;
if(!h)	h=ximg->height-xy;
if(w<=0 || xx+w>ximg->width || h<=0 || xy+h>ximg->height) return 0;

if(ximg->bits_per_pixel==8) {
	iget_area8((uchar *)ximg->data+ximg->xoffset+xy*ximg->bytes_per_line+xx,x,y,
			dx,dy,w,h,sx,sy,ximg->bytes_per_line);
	return 1;
	}
if(ximg->bits_per_pixel==32) {
	iget_area32((ulong *)(ximg->data+xy*ximg->bytes_per_line)+ximg->xoffset+xx,
			x,y,dx,dy,w,h,sx,sy,ximg->bytes_per_line>>2);
	return 1;
	}
return 0;
}


XImage *ImageDisp::create_ximage(int ix, int iy, int w, int h)
{
int	xr,yr;
XImage	*ximg;
char	*imgdata;

if(!img || !depth || !display || !visual) return NULL;

img->get_res(&xr,&yr);
if(!w)	w=xr-ix;
if(!h)	h=yr-iy;
if(w<=0 || h<=0) return NULL;

if(depth==8) imgdata=(char *)malloc(((w+3)&0xfffc)*h);
else if(depth==24) imgdata=(char *)malloc(w*h*4);
else	return NULL;
if(!imgdata) return NULL;
ximg=XCreateImage(display,visual,depth,ZPixmap,0,imgdata,w,h,BitmapPad(display),0);
if(ximg) fill_ximage(ximg,0,0,ix,iy,w,h);
return ximg;
}


XImage *ImageDisp::icreate_ximage(double x, double y, double dx, double dy,
				  int w, int h, int sx, int sy)
{
int	xr,yr;
XImage	*ximg;
char	*imgdata;

if(!img || !depth || !display || !visual) return NULL;

img->get_res(&xr,&yr);
if(!w && dx) w=(int)(((double)xr-x)/dx);
if(!h && dy) h=(int)(((double)yr-y)/dy);
if(w<=0 || h<=0) return NULL;

if(depth==8) imgdata=(char *)malloc(((w+3)&0xfffc)*h);
else if(depth==24) imgdata=(char *)malloc(w*h*4);
else	return NULL;
if(!imgdata) return NULL;
ximg=XCreateImage(display,visual,depth,ZPixmap,0,imgdata,w,h,BitmapPad(display),0);
if(ximg) ifill_ximage(ximg,x,y,dx,dy,0,0,w,h,sx,sy);
return ximg;
}


int ImageDisp::fill_drawable(Drawable draw, int dx, int dy, int ix, int iy,
								int w, int h)
{
XImage	*ximg;
int	rxr,ryr;
unsigned int	dw,dh,bwr,dr;
Window	rr;
GC	gc;

if(!draw) { if(window) draw=window; else return 0; }

if(!w||!h) {
	if(!XGetGeometry(display,draw,&rr,&rxr,&ryr,&dw,&dh,&bwr,&dr)) return 0;
	if(!w) w=dw-dx;
	if(!h) h=dh-dy;
	}
if(w<=0 || h<=0) return 0;

ximg=create_ximage(ix,iy,w,h);
if(!ximg) return 0;

gc=XCreateGC(display,draw,0,NULL);
XPutImage(display,draw,gc,ximg,0,0,dx,dy,w,h);
XDestroyImage(ximg);
XFreeGC(display,gc);
return 1;
}


int ImageDisp::ifill_drawable(Drawable draw, double x, double y, double dx, double dy,
			      int wx, int wy, int w, int h, int sx, int sy)
{
XImage	*ximg;
int	rxr,ryr;
unsigned int	dw,dh,bwr,dr;
Window	rr;
GC	gc;

if(!draw) { if(window) draw=window; else return 0; }

if(!w||!h) {
	if(!XGetGeometry(display,draw,&rr,&rxr,&ryr,&dw,&dh,&bwr,&dr)) return 0;
	if(!w) w=dw-wx;
	if(!h) h=dh-wy;
	}
if(w<=0 || h<=0) return 0;

ximg=icreate_ximage(x,y,dx,dy,w,h,sx,sy);
if(!ximg) return 0;

gc=XCreateGC(display,draw,0,NULL);
XPutImage(display,draw,gc,ximg,0,0,wx,wy,w,h);
XDestroyImage(ximg);
XFreeGC(display,gc);
return 1;
}


Pixmap ImageDisp::create_pixmap(Drawable draw, int ix, int iy, int w, int h)
{
Pixmap	pix;
int	xr,yr;

if(!img || !depth || !display || !visual) return (Pixmap)0;
if(!draw) { if(window) draw=window; else return (Pixmap)0; }

img->get_res(&xr,&yr);
if(!w)	w=xr-ix;
if(!h)	h=yr-iy;
if(w<=0 || h<=0) return (Pixmap)0;

pix=XCreatePixmap(display,draw,w,h,depth);
fill_drawable(pix,0,0,ix,iy,w,h);
return pix;
}


Pixmap ImageDisp::icreate_pixmap(Drawable draw, double x, double y, double dx, double dy,
				 int w, int h, int sx, int sy)
{
Pixmap	pix;
int	xr,yr;

if(!img || !depth || !display || !visual) return (Pixmap)0;
if(!draw) { if(window) draw=window; else return (Pixmap)0; }

img->get_res(&xr,&yr);
if(!w && dx) w=(int)(((double)xr-x)/dx);
if(!h && dy) h=(int)(((double)yr-y)/dy);
if(w<=0 || h<=0) return (Pixmap)0;

pix=XCreatePixmap(display,draw,w,h,depth);
ifill_drawable(pix,x,y,dx,dy,0,0,w,h,sx,sy);
return pix;
}




// Colormap index reference count code: ***********************************


int	R_NUM_MAPS=0;
int	*R_CODE_ARRAY=NULL;
int	*R_ACCESS_ARRAY=NULL;
int	**R_REFERENCE_ARRAY=NULL;


int R_get_index(Display *disp, Colormap cmap, int create)
{
int	code,i,j;

code=(int)(uintptr_t)disp+17*(int)(uintptr_t)cmap;
for(i=0;i<R_NUM_MAPS;i++)
	if(R_CODE_ARRAY[i]==code) break;
if(i==R_NUM_MAPS) {
	if(!create) return -1;
	R_NUM_MAPS++;
	R_CODE_ARRAY=(int *)realloc(R_CODE_ARRAY,R_NUM_MAPS*sizeof(int));
	R_ACCESS_ARRAY=(int *)realloc(R_ACCESS_ARRAY,R_NUM_MAPS*sizeof(int));
	R_REFERENCE_ARRAY=(int **)realloc(R_REFERENCE_ARRAY,R_NUM_MAPS*sizeof(int *));
	R_REFERENCE_ARRAY[i]=(int *)malloc(256*sizeof(int));
	R_CODE_ARRAY[i]=code;
	R_ACCESS_ARRAY[i]=1;
	for(j=0;j<256;j++) R_REFERENCE_ARRAY[i][j]=0;
	}
if(R_ACCESS_ARRAY[i]) return i;
return -1;
}


void RFreeColor(Display *disp, Colormap cmap, Pixel pix)
{
int	i;

i=R_get_index(disp,cmap,0);
if(i<0) return;
if(pix>255) { R_ACCESS_ARRAY[i]=0; free((char *)R_REFERENCE_ARRAY[i]); return; }
if(!(--R_REFERENCE_ARRAY[i][(int)pix])) XFreeColors(disp,cmap,&pix,1,0);
if(R_REFERENCE_ARRAY[i][(int)pix]<0) R_REFERENCE_ARRAY[i][(int)pix]=0;
}


void RFreeColors(Display *disp, Colormap cmap, Pixel *pix, int npix)
{
int	i,j;

i=R_get_index(disp,cmap,0);
if(i<0) return;
for(j=0;j<npix;j++) {
	if(pix[j]>255) { R_ACCESS_ARRAY[i]=0; free((char *)R_REFERENCE_ARRAY[i]); return; }
	if(!(--R_REFERENCE_ARRAY[i][(int)pix[j]])) XFreeColors(disp,cmap,&pix[j],1,0);
	if(R_REFERENCE_ARRAY[i][(int)pix[j]]<0) R_REFERENCE_ARRAY[i][(int)pix[j]]=0;
	}
}


int RAllocColor(Display *disp, Colormap cmap, XColor *xcol)
{
int	i;

if(!XAllocColor(disp,cmap,xcol)) return 0;
i=R_get_index(disp,cmap,1);
if(i<0) return 1;
if(xcol->pixel>255) { R_ACCESS_ARRAY[i]=0; free((char *)R_REFERENCE_ARRAY[i]); return 1; }
R_REFERENCE_ARRAY[i][(int)(xcol->pixel)]++;
return 1;
}


int RAllocColor(Display *disp, Colormap cmap, Pixel *pix,
	ushort red, ushort green, ushort blue)
{
XColor	xcol;
int	r;

xcol.red=red; xcol.green=green; xcol.blue=blue;
r=RAllocColor(disp,cmap,&xcol);
if(!r) return 0;
*pix=xcol.pixel;
return r;
}


int RAllocColor(Display *disp, Colormap cmap, Pixel *pix,
	int red, int green, int blue)
{
XColor	xcol;
int	r;

xcol.red=(unsigned short)(red<<8);
xcol.green=(unsigned short)(green<<8);
xcol.blue=(unsigned short)(blue<<8);
r=RAllocColor(disp,cmap,&xcol);
if(!r) return 0;
*pix=xcol.pixel;
return r;
}


int RAllocColor(Display *disp, Colormap cmap, Pixel *pix,
	double red, double green, double blue)
{
XColor	xcol;
int	r;

xcol.red=(unsigned short)(red*65535.0+0.5);
xcol.green=(unsigned short)(green*65535.0+0.5);
xcol.blue=(unsigned short)(blue*65535.0+0.5);
r=RAllocColor(disp,cmap,&xcol);
if(!r) return 0;
*pix=xcol.pixel;
return r;
}

#endif // _NO_IMAGEDISP_
