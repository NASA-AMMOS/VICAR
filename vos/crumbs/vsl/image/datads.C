// datads.C
//
// Written by Dave Kagels 10/31/95

#ifndef _NO_DATASERVER_
#include "image/datads.h"
#include "ds/client.h"


// ****************************** dataDS *******************************

extern	double	pyr_fraction[32];


void dataDS::init()
{
int	i;

sock=-1;

if(pyr_fraction[0]==0.0) { pyr_fraction[0]=1.0;
	for(i=1;i<32;i++) pyr_fraction[i]=pyr_fraction[i-1]*0.5; }

lev_int=0;
lev_frac=0.0;
}


int dataDS::allocate(int x, int y, int b)
{
xres=x; yres=y; bands=b;
yflip=yres-1;

return 1;
}


void dataDS::free()
{
if(sock>=0) dsClose(sock); sock=-1;
}


// ****************************** ucharDS *******************************


void ucharDS::get_color(int x, int y, uchar *r, uchar *g, uchar *b, uchar *a)
{
uchar	*data;

if(check_bounds(x,y)) { *r=0; *g=0; *b=0; if(a) *a=0; return; }

if(requestPixel(sock,lev_int,(yflip-y)>>lev_int,x>>lev_int,(void **)&data)<0) {
	*r=0; *g=0; *b=0; if(a) *a=0; return; }
*r=data[red_band];
*g=data[green_band];
*b=data[blue_band];
if(a) {	if(alpha_band>=0) *a=data[alpha_band];
	else	*a=default_alpha;
	}
}


uchar ucharDS::get_alpha(int x, int y)
{
uchar	*a;

if(check_bounds(x,y)) return 0;

if(alpha_band>=0) {
	if(requestValue(sock,alpha_band,lev_int,(yflip-y)>>lev_int,x>>lev_int,(void **)&a)<0) return 0;
	return *a;
	}
return default_alpha;
}


uchar ucharDS::get_uchar(int x, int y, int b)
{
uchar	*v;

if(check_band(b) || check_bounds(x,y)) return 0;
if(requestValue(sock,b,lev_int,(yflip-y)>>lev_int,x>>lev_int,(void **)&v)<0) return 0;
return *v;
}


uchar	*d_0=(uchar *)"\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0";


void ucharDS::iget_color(double x, double y, uchar *r, uchar *g, uchar *b, uchar *a)
{
uchar	*data1,*data2,*data3,*data4;
double	f,fx,fy,fx1,fy1;
int	ix,iy;

if(check_bounds(x,y)) { *r=0; *g=0; *b=0; if(a) *a=0; return; }

if(a) *a=default_alpha;

if(lev_int) { x*=pyr_fraction[lev_int]; y=((double)yflip-y)*pyr_fraction[lev_int]; }
else	y=(double)yflip-y;
ix=(int)x; fx=x-ix; fx1=1.0-fx;
iy=(int)y; fy=y-iy; fy1=1.0-fy;

if(requestPixel(sock,lev_int,iy,ix,(void **)&data1)<0) data1=d_0;
if(requestPixel(sock,lev_int,iy,ix+1,(void **)&data2)<0) data2=d_0;
if(requestPixel(sock,lev_int,iy+1,ix,(void **)&data3)<0) data3=d_0;
if(requestPixel(sock,lev_int,iy+1,ix+1,(void **)&data4)<0) data4=d_0;
*r=(uchar)(data1[red_band]*fx1*fy1+data2[red_band]*fx*fy1+data3[red_band]*fx1*fy+data4[red_band]*fx*fy);
*g=(uchar)(data1[green_band]*fx1*fy1+data2[green_band]*fx*fy1+data3[green_band]*fx1*fy+data4[green_band]*fx*fy);
*b=(uchar)(data1[blue_band]*fx1*fy1+data2[blue_band]*fx*fy1+data3[blue_band]*fx1*fy+data4[blue_band]*fx*fy);
if(a && alpha_band>=0) *a=(uchar)(data1[alpha_band]*fx1*fy1+data2[alpha_band]*fx*fy1+data3[alpha_band]*fx1*fy+data4[alpha_band]*fx*fy);

if(lev_frac<0.001) return;
f=1.0-lev_frac;
x*=0.5; y*=0.5;
ix=(int)x; fx=x-ix; fx1=1.0-fx;
iy=(int)y; fy=y-iy; fy1=1.0-fy;

if(requestPixel(sock,lev_int+1,iy,ix,(void **)&data1)<0) data1=d_0;
if(requestPixel(sock,lev_int+1,iy,ix+1,(void **)&data2)<0) data2=d_0;
if(requestPixel(sock,lev_int+1,iy+1,ix,(void **)&data3)<0) data3=d_0;
if(requestPixel(sock,lev_int+1,iy+1,ix+1,(void **)&data4)<0) data4=d_0;
*r=(uchar)(*r*f+lev_frac*(data1[red_band]*fx1*fy1+data2[red_band]*fx*fy1+data3[red_band]*fx1*fy+data4[red_band]*fx*fy));
*g=(uchar)(*g*f+lev_frac*(data1[green_band]*fx1*fy1+data2[green_band]*fx*fy1+data3[green_band]*fx1*fy+data4[green_band]*fx*fy));
*b=(uchar)(*b*f+lev_frac*(data1[blue_band]*fx1*fy1+data2[blue_band]*fx*fy1+data3[blue_band]*fx1*fy+data4[blue_band]*fx*fy));
if(a && alpha_band>=0) *a=(uchar)(*a*f+lev_frac*(data1[alpha_band]*fx1*fy1+data2[alpha_band]*fx*fy1+data3[alpha_band]*fx1*fy+data4[alpha_band]*fx*fy));
}


uchar ucharDS::iget_alpha(double x, double y)
{
uchar	*v1,*v2,*v3,*v4,a;
double	fx,fy,fx1,fy1;
int	ix,iy;

if(check_bounds(x,y)) return 0;
if(alpha_band<0) return default_alpha;

if(lev_int) { x*=pyr_fraction[lev_int]; y=((double)yflip-y)*pyr_fraction[lev_int]; }
else	y=(double)yflip-y;
ix=(int)x; fx=x-ix; fx1=1.0-fx;
iy=(int)y; fy=y-iy; fy1=1.0-fy;

if(requestValue(sock,alpha_band,lev_int,iy,ix,(void **)&v1)<0) v1=d_0;
if(requestValue(sock,alpha_band,lev_int,iy,ix+1,(void **)&v2)<0) v2=d_0;
if(requestValue(sock,alpha_band,lev_int,iy+1,ix,(void **)&v3)<0) v3=d_0;
if(requestValue(sock,alpha_band,lev_int,iy+1,ix+1,(void **)&v4)<0) v4=d_0;
a=(uchar)(*v1*fx1*fy1+*v2*fx*fy1+*v3*fx1*fy+*v4*fx*fy);

if(lev_frac<0.001) return a;
x*=0.5; y*=0.5;
ix=(int)x; fx=x-ix; fx1=1.0-fx;
iy=(int)y; fy=y-iy; fy1=1.0-fy;

if(requestValue(sock,alpha_band,lev_int+1,iy,ix,(void **)&v1)<0) v1=d_0;
if(requestValue(sock,alpha_band,lev_int+1,iy,ix+1,(void **)&v2)<0) v2=d_0;
if(requestValue(sock,alpha_band,lev_int+1,iy+1,ix,(void **)&v3)<0) v3=d_0;
if(requestValue(sock,alpha_band,lev_int+1,iy+1,ix+1,(void **)&v4)<0) v4=d_0;
return (uchar)(a*(1.0-lev_frac)+lev_frac*(*v1*fx1*fy1+*v2*fx*fy1+*v3*fx1*fy+*v4*fx*fy));
}


uchar ucharDS::iget_uchar(double x, double y, int b)
{
uchar	*v1,*v2,*v3,*v4,v;
double	fx,fy,fx1,fy1;
int	ix,iy;

if(check_band(b) || check_bounds(x,y)) return 0;

if(lev_int) { x*=pyr_fraction[lev_int]; y=((double)yflip-y)*pyr_fraction[lev_int]; }
else	y=(double)yflip-y;
ix=(int)x; fx=x-ix; fx1=1.0-fx;
iy=(int)y; fy=y-iy; fy1=1.0-fy;

if(requestValue(sock,b,lev_int,iy,ix,(void **)&v1)<0) v1=d_0;
if(requestValue(sock,b,lev_int,iy,ix+1,(void **)&v2)<0) v2=d_0;
if(requestValue(sock,b,lev_int,iy+1,ix,(void **)&v3)<0) v3=d_0;
if(requestValue(sock,b,lev_int,iy+1,ix+1,(void **)&v4)<0) v4=d_0;
v=(uchar)(*v1*fx1*fy1+*v2*fx*fy1+*v3*fx1*fy+*v4*fx*fy);

if(lev_frac<0.001) return v;
x*=0.5; y*=0.5;
ix=(int)x; fx=x-ix; fx1=1.0-fx;
iy=(int)y; fy=y-iy; fy1=1.0-fy;

if(requestValue(sock,b,lev_int+1,iy,ix,(void **)&v1)<0) v1=d_0;
if(requestValue(sock,b,lev_int+1,iy,ix+1,(void **)&v2)<0) v2=d_0;
if(requestValue(sock,b,lev_int+1,iy+1,ix,(void **)&v3)<0) v3=d_0;
if(requestValue(sock,b,lev_int+1,iy+1,ix+1,(void **)&v4)<0) v4=d_0;
return (uchar)(v*(1.0-lev_frac)+lev_frac*(*v1*fx1*fy1+*v2*fx*fy1+*v3*fx1*fy+*v4*fx*fy));
}

#endif // _NO_DATASERVER_
