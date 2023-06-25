// imagedata.C
//
// Written by Dave Kagels 10/3/94

#include "image/imagedata.h"
#include "image/image.h"


int	data_type_size[11]={ 0,1,1,1,2,2,4,4,sizeof(float),sizeof(double),sizeof(COMPLEX_TYPE) };
int	data_type_bits[11]={ 0,1,8,8,16,16,32,32,8*sizeof(float),8*sizeof(double),8*sizeof(COMPLEX_TYPE) };


void ImageData::init()
{
xres=0; yres=0;
bands=0;
map=NULL;
default_band=0;
red_band=0;
green_band=1;
blue_band=2;
alpha_band=-1;
default_alpha=(uchar)255;
cx=0; cy=0; cband=0;
levels=1;
clev=0.0; levmax=0.0;
clean=True;
flgWrapState=IMAGE_WRAP_STATE_NONE;
}


int ImageData::clear(int b)
{
int	eb,x,y;

if(!(access()&DATA_WRITE)) return 0;

if(b==ALL_BANDS) {
	if(bands>0) { b=0; eb=bands-1; }
	else return 0;
	}
else {	if(check_band(b)) return 0;
	else	eb=b;
	}
for(;b<=eb;b++)
	for(y=0;y<yres;y++)
		for(x=0;x<xres;x++) set_long(0,x,y,b);
return 1;
}


int ImageData::get_res(int *x, int *y, int *b)
{
if(x) *x=xres;
if(y) *y=yres;
if(b) *b=bands;

return xres;
}


void ImageData::free_map()
{
if(map) delete map;
map=NULL;
}


void ImageData::get_color(int x, int y, uchar *r, uchar *g, uchar *b, uchar *a)
{
if(map) (map->get_data())->get_color((int)get_long(x,y),0,r,g,b);
else {	*r=get_uchar(x,y,red_band);
	*g=get_uchar(x,y,green_band);
	*b=get_uchar(x,y,blue_band);
	}
if(a) *a=get_alpha(x,y);
}


void ImageData::iget_color(double x, double y, uchar *r, uchar *g, uchar *b, uchar *a)
{
double	fx,fy,fx1,fy1;
int	ix,iy;
uchar	r1,r2,r3,r4,g1,g2,g3,g4,b1,b2,b3,b4;

ix=(int)x; if(x<0.0) ix--; fx=x-ix; fx1=1.0-fx;
iy=(int)y; if(y<0.0) iy--; fy=y-iy; fy1=1.0-fy;

get_color(ix,iy,&r1,&g1,&b1);
get_color(ix+1,iy,&r2,&g2,&b2);
get_color(ix,iy+1,&r3,&g3,&b3);
get_color(ix+1,iy+1,&r4,&g4,&b4);

*r=(uchar)(r1*fx1*fy1+r2*fx*fy1+r3*fx1*fy+r4*fx*fy);
*g=(uchar)(g1*fx1*fy1+g2*fx*fy1+g3*fx1*fy+g4*fx*fy);
*b=(uchar)(b1*fx1*fy1+b2*fx*fy1+b3*fx1*fy+b4*fx*fy);

if(a) *a=iget_alpha(x,y);
}


void ImageData::set_color(int x, int y, uchar r, uchar g, uchar b, uchar a)
{
if(map) return;
set_uchar(r,x,y,red_band);
set_uchar(g,x,y,green_band);
set_uchar(b,x,y,blue_band);
set_alpha(x,y,a);
}


void ImageData::iset_color(double x, double y, uchar r, uchar g, uchar b, uchar a)
{
set_color((int)(x<0.0?x-0.5:x+0.5),(int)(y<0.0?y-0.5:y+0.5),r,g,b,a);
}


uchar ImageData::get_alpha(int x, int y)
{
if(alpha_band>=0) return get_uchar(x,y,alpha_band);

if (check_bounds_and_apply_wrap( x, y)) return (uchar)0;
return default_alpha;
}


uchar ImageData::iget_alpha(double x, double y)
{
if(alpha_band>=0) return iget_uchar(x,y,alpha_band);

if (check_bounds_and_apply_wrap( x, y)) return (uchar)0;

return default_alpha;
}


void ImageData::set_alpha(int x, int y, uchar a)
{
if(alpha_band>=0) set_uchar(a,x,y,alpha_band);
}


void ImageData::iset_alpha(double x, double y, uchar a)
{
set_alpha((int)(x<0.0?x-0.5:x+0.5),(int)(y<0.0?y-0.5:y+0.5),a);
}


ushort ImageData::get_red16(int x, int y)
{
if(map) return (map->get_data())->get_red16((int)get_long(x,y));
return get_ushort(x,y,red_band);
}


ushort ImageData::get_green16(int x, int y)
{
if(map) return (map->get_data())->get_green16((int)get_long(x,y));
return get_ushort(x,y,green_band);
}


ushort ImageData::get_blue16(int x, int y)
{
if(map) return (map->get_data())->get_blue16((int)get_long(x,y));
return get_ushort(x,y,blue_band);
}


ushort ImageData::iget_red16(double x, double y)
{
double	fx,fy,fx1,fy1;
int	ix,iy;

ix=(int)x; if(x<0.0) ix--; fx=x-ix; fx1=1.0-fx;
iy=(int)y; if(y<0.0) iy--; fy=y-iy; fy1=1.0-fy;

return((ushort)(get_red16(ix,iy)*fx1*fy1+get_red16(ix+1,iy)*fx*fy1+
		get_red16(ix,iy+1)*fx1*fy+get_red16(ix+1,iy+1)*fx*fy));
}


ushort ImageData::iget_green16(double x, double y)
{
double	fx,fy,fx1,fy1;
int	ix,iy;

ix=(int)x; if(x<0.0) ix--; fx=x-ix; fx1=1.0-fx;
iy=(int)y; if(y<0.0) iy--; fy=y-iy; fy1=1.0-fy;

return((ushort)(get_green16(ix,iy)*fx1*fy1+get_green16(ix+1,iy)*fx*fy1+
		get_green16(ix,iy+1)*fx1*fy+get_green16(ix+1,iy+1)*fx*fy));
}


ushort ImageData::iget_blue16(double x, double y)
{
double	fx,fy,fx1,fy1;
int	ix,iy;

ix=(int)x; if(x<0.0) ix--; fx=x-ix; fx1=1.0-fx;
iy=(int)y; if(y<0.0) iy--; fy=y-iy; fy1=1.0-fy;

return((ushort)(get_blue16(ix,iy)*fx1*fy1+get_blue16(ix+1,iy)*fx*fy1+
		get_blue16(ix,iy+1)*fx1*fy+get_blue16(ix+1,iy+1)*fx*fy));
}


void ImageData::set_red16(ushort c, int x, int y)
{
if(!map) set_ushort(c,x,y,red_band);
}


void ImageData::set_green16(ushort c, int x, int y)
{
if(!map) set_ushort(c,x,y,green_band);
}


void ImageData::set_blue16(ushort c, int x, int y)
{
if(!map) set_ushort(c,x,y,blue_band);
}


void ImageData::iset_red16(ushort c, double x, double y)
{
set_red16(c,(int)(x<0.0?x-0.5:x+0.5),(int)(y<0.0?y-0.5:y+0.5));
}


void ImageData::iset_green16(ushort c, double x, double y)
{
set_green16(c,(int)(x<0.0?x-0.5:x+0.5),(int)(y<0.0?y-0.5:y+0.5));
}


void ImageData::iset_blue16(ushort c, double x, double y)
{
set_blue16(c,(int)(x<0.0?x-0.5:x+0.5),(int)(y<0.0?y-0.5:y+0.5));
}


int ImageData::get_bit(int x, int y, int b)
{
return (int)(get_long(x,y,b)&1);
}


void ImageData::set_bit(int v, int x, int y, int b)
{
set_long((long)(v&1),x,y,b);
}


COMPLEX_TYPE ImageData::get_complex(int x, int y, int b)
{
return (COMPLEX_TYPE)get_double(x,y,b);
}


void ImageData::set_complex(COMPLEX_TYPE v, int x, int y, int b)
{
set_double(real(v),x,y,b);
}


int ImageData::iget_bit(double x, double y, int b)
{
double	fx,fy,fx1,fy1;
int	ix,iy;

ix=(int)x; if(x<0.0) ix--; fx=x-ix; fx1=1.0-fx;
iy=(int)y; if(y<0.0) iy--; fy=y-iy; fy1=1.0-fy;

return((int)(get_bit(ix,iy,b)*fx1*fy1+get_bit(ix+1,iy,b)*fx*fy1+
		get_bit(ix,iy+1,b)*fx1*fy+get_bit(ix+1,iy+1,b)*fx*fy+0.5));
}


char ImageData::iget_char(double x, double y, int b)
{
double	fx,fy,fx1,fy1;
int	ix,iy;

ix=(int)x; if(x<0.0) ix--; fx=x-ix; fx1=1.0-fx;
iy=(int)y; if(y<0.0) iy--; fy=y-iy; fy1=1.0-fy;

return((char)(get_char(ix,iy,b)*fx1*fy1+get_char(ix+1,iy,b)*fx*fy1+
		get_char(ix,iy+1,b)*fx1*fy+get_char(ix+1,iy+1,b)*fx*fy+0.5));
}


uchar ImageData::iget_uchar(double x, double y, int b)
{
double	fx,fy,fx1,fy1;
int	ix,iy;

ix=(int)x; if(x<0.0) ix--; fx=x-ix; fx1=1.0-fx;
iy=(int)y; if(y<0.0) iy--; fy=y-iy; fy1=1.0-fy;

return((uchar)(get_uchar(ix,iy,b)*fx1*fy1+get_uchar(ix+1,iy,b)*fx*fy1+
		get_uchar(ix,iy+1,b)*fx1*fy+get_uchar(ix+1,iy+1,b)*fx*fy+0.5));
}


short ImageData::iget_short(double x, double y, int b)
{
double	fx,fy,fx1,fy1;
int	ix,iy;

ix=(int)x; if(x<0.0) ix--; fx=x-ix; fx1=1.0-fx;
iy=(int)y; if(y<0.0) iy--; fy=y-iy; fy1=1.0-fy;

return((short)(get_short(ix,iy,b)*fx1*fy1+get_short(ix+1,iy,b)*fx*fy1+
		get_short(ix,iy+1,b)*fx1*fy+get_short(ix+1,iy+1,b)*fx*fy+0.5));
}


ushort ImageData::iget_ushort(double x, double y, int b)
{
double	fx,fy,fx1,fy1;
int	ix,iy;

ix=(int)x; if(x<0.0) ix--; fx=x-ix; fx1=1.0-fx;
iy=(int)y; if(y<0.0) iy--; fy=y-iy; fy1=1.0-fy;

return((ushort)(get_ushort(ix,iy,b)*fx1*fy1+get_ushort(ix+1,iy,b)*fx*fy1+
		get_ushort(ix,iy+1,b)*fx1*fy+get_ushort(ix+1,iy+1,b)*fx*fy+0.5));
}


long ImageData::iget_long(double x, double y, int b)
{
double	fx,fy,fx1,fy1;
int	ix,iy;

ix=(int)x; if(x<0.0) ix--; fx=x-ix; fx1=1.0-fx;
iy=(int)y; if(y<0.0) iy--; fy=y-iy; fy1=1.0-fy;

return((long)(get_long(ix,iy,b)*fx1*fy1+get_long(ix+1,iy,b)*fx*fy1+
		get_long(ix,iy+1,b)*fx1*fy+get_long(ix+1,iy+1,b)*fx*fy+0.5));
}


ulong ImageData::iget_ulong(double x, double y, int b)
{
double	fx,fy,fx1,fy1;
int	ix,iy;

ix=(int)x; if(x<0.0) ix--; fx=x-ix; fx1=1.0-fx;
iy=(int)y; if(y<0.0) iy--; fy=y-iy; fy1=1.0-fy;

return((ulong)(get_ulong(ix,iy,b)*fx1*fy1+get_ulong(ix+1,iy,b)*fx*fy1+
		get_ulong(ix,iy+1,b)*fx1*fy+get_ulong(ix+1,iy+1,b)*fx*fy+0.5));
}


float ImageData::iget_float(double x, double y, int b)
{
double	fx,fy,fx1,fy1;
int	ix,iy;

ix=(int)x; if(x<0.0) ix--; fx=x-ix; fx1=1.0-fx;
iy=(int)y; if(y<0.0) iy--; fy=y-iy; fy1=1.0-fy;

return((float)(get_float(ix,iy,b)*fx1*fy1+get_float(ix+1,iy,b)*fx*fy1+
		get_float(ix,iy+1,b)*fx1*fy+get_float(ix+1,iy+1,b)*fx*fy));
}


double ImageData::iget_double(double x, double y, int b)
{
double	fx,fy,fx1,fy1;
int	ix,iy;

ix=(int)x; if(x<0.0) ix--; fx=x-ix; fx1=1.0-fx;
iy=(int)y; if(y<0.0) iy--; fy=y-iy; fy1=1.0-fy;

return(get_double(ix,iy,b)*fx1*fy1+get_double(ix+1,iy,b)*fx*fy1+
		get_double(ix,iy+1,b)*fx1*fy+get_double(ix+1,iy+1,b)*fx*fy);
}


COMPLEX_TYPE ImageData::iget_complex(double x, double y, int b)
{
double	fx,fy,fx1,fy1;
int	ix,iy;

ix=(int)x; if(x<0.0) ix--; fx=x-ix; fx1=1.0-fx;
iy=(int)y; if(y<0.0) iy--; fy=y-iy; fy1=1.0-fy;

return(get_complex(ix,iy,b)*fx1*fy1+get_complex(ix+1,iy,b)*fx*fy1+
		get_complex(ix,iy+1,b)*fx1*fy+get_complex(ix+1,iy+1,b)*fx*fy);
}


void ImageData::iset_bit(int v, double x, double y, int b)
{
set_bit(v,(int)(x<0.0?x-0.5:x+0.5),(int)(y<0.0?y-0.5:y+0.5),b);
}


void ImageData::iset_char(char v, double x, double y, int b)
{
set_char(v,(int)(x<0.0?x-0.5:x+0.5),(int)(y<0.0?y-0.5:y+0.5),b);
}


void ImageData::iset_uchar(uchar v, double x, double y, int b)
{
set_uchar(v,(int)(x<0.0?x-0.5:x+0.5),(int)(y<0.0?y-0.5:y+0.5),b);
}


void ImageData::iset_short(short v, double x, double y, int b)
{
set_short(v,(int)(x<0.0?x-0.5:x+0.5),(int)(y<0.0?y-0.5:y+0.5),b);
}


void ImageData::iset_ushort(ushort v, double x, double y, int b)
{
set_ushort(v,(int)(x<0.0?x-0.5:x+0.5),(int)(y<0.0?y-0.5:y+0.5),b);
}


void ImageData::iset_long(long v, double x, double y, int b)
{
set_long(v,(int)(x<0.0?x-0.5:x+0.5),(int)(y<0.0?y-0.5:y+0.5),b);
}


void ImageData::iset_ulong(ulong v, double x, double y, int b)
{
set_ulong(v,(int)(x<0.0?x-0.5:x+0.5),(int)(y<0.0?y-0.5:y+0.5),b);
}


void ImageData::iset_float(float v, double x, double y, int b)
{
set_float(v,(int)(x<0.0?x-0.5:x+0.5),(int)(y<0.0?y-0.5:y+0.5),b);
}


void ImageData::iset_double(double v, double x, double y, int b)
{
set_double(v,(int)(x<0.0?x-0.5:x+0.5),(int)(y<0.0?y-0.5:y+0.5),b);
}


void ImageData::iset_complex(COMPLEX_TYPE v, double x, double y, int b)
{
set_complex(v,(int)(x<0.0?x-0.5:x+0.5),(int)(y<0.0?y-0.5:y+0.5),b);
}


