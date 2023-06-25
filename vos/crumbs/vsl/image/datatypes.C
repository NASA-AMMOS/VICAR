// datatypes.C
//
// Written by Dave Kagels 10/11/94

#include "image/datatypes.h"
#include "image/geoimage.h"


// ****************************** bitData *******************************

void bitData::init()
{
data=NULL;
xlen=0;
}


int bitData::allocate(int x, int y, int b)
{
int	i;

free();

if(x<0 || y<0 || b<0) return 0;

xres=x; yres=y; bands=b;

data=(uchar **)malloc(bands*sizeof(uchar *));
if(!data) return 0;
xlen=(xres+7)>>3;
for(i=0;i<bands;i++) {
	data[i]=(uchar *)malloc(xlen*yres);
	if(!data[i]) return 0;
	}
return 1;
}


void bitData::free()
{
int	i;

if(data) {
	for(i=0;i<bands;i++) ::free((char *)data[i]);
	::free((char *)data);
	data=NULL;
	}
xres=0; yres=0; bands=0; xlen=0;
}


int bitData::add_band()
{
if(!data) return 0;

bands++;
data=(uchar **)realloc(data,bands*sizeof(uchar *));
if(!data) return 0;
data[bands-1]=(uchar *)malloc(xlen*yres);
if(!data[bands-1]) return 0;
return 1;
}


int bitData::remove_band(int b)
{
int	i;

if(!data || check_band(b)) return 0;

bands--;

::free((char *)data[b]);
for(i=b;i<bands;i++) data[i]=data[i+1];
data=(uchar **)realloc(data,bands*sizeof(uchar *));
if(!data) return 0;
return 1;
}


uchar *bitData::get_data(int b)
{
if(check_band(b) || !data) return NULL;
return data[b];
}


void bitData::get_color(int x, int y, uchar *r, uchar *g, uchar *b, uchar *a)
{
if (check_bounds_and_apply_wrap( x, y, r, g, b, a))
	return;

if(map) {
	if(default_band>=bands || default_band<0) { *r=0; *g=0; *b=0; }
	else	(map->get_data())->get_color((int)gbit(x,y,default_band),0,r,g,b);
	}
else {	if(red_band>=bands || red_band<0) *r=0;
	else	*r=gbit(x,y,red_band)?(uchar)255:(uchar)0;
	if(green_band>=bands || green_band<0) *g=0;
	else	*g=gbit(x,y,green_band)?(uchar)255:(uchar)0;
	if(blue_band>=bands || blue_band<0) *b=0;
	else	*b=gbit(x,y,blue_band)?(uchar)255:(uchar)0;
	}
if(a) {	if(alpha_band<0) *a=default_alpha;
	else	if(alpha_band>=bands) *a=0;
	else	*a=gbit(x,y,alpha_band)?(uchar)255:(uchar)0;
	}
}


void bitData::set_color(int x, int y, uchar r, uchar g, uchar b, uchar a)
{
if(check_bounds(x,y) || map) return;
if(red_band<bands && red_band>=0) sbit((int)r>>7,x,y,red_band);
if(green_band<bands && green_band>=0) sbit((int)g>>7,x,y,green_band);
if(blue_band<bands && blue_band>=0) sbit((int)b>>7,x,y,blue_band);
if(alpha_band>=0 && alpha_band<bands) sbit((int)a>>7,x,y,alpha_band);
}


uchar bitData::get_alpha(int x, int y)
{
if(check_bounds_and_apply_wrap(x,y) || alpha_band>=bands) return 0;
if(alpha_band<0) return default_alpha;
return gbit(x,y,alpha_band)?(uchar)255:(uchar)0;
}


void bitData::set_alpha(int x, int y, uchar a)
{
if(check_bounds(x,y) || alpha_band<0 || alpha_band>=bands) return;
sbit((int)a>>7,x,y,alpha_band);
}


ushort bitData::get_red16(int x, int y)
{
if(map) {
	if(check_bounds_and_apply_wrap(x,y) || default_band>=bands || default_band<0) return 0;
	return (map->get_data())->get_red16((int)gbit(x,y,default_band));
	}
if(check_bounds_and_apply_wrap(x,y) || red_band>=bands || red_band<0) return 0;
return gbit(x,y,red_band)?(ushort)65535:(ushort)0;
}


ushort bitData::get_green16(int x, int y)
{
if(map) {
	if(check_bounds_and_apply_wrap(x,y) || default_band>=bands || default_band<0) return 0;
	return (map->get_data())->get_green16((int)gbit(x,y,default_band));
	}
if(check_bounds_and_apply_wrap(x,y) || green_band>=bands || green_band<0) return 0;
return gbit(x,y,green_band)?(ushort)65535:(ushort)0;
}


ushort bitData::get_blue16(int x, int y)
{
if(map) {
	if(check_bounds_and_apply_wrap(x,y) || default_band>=bands || default_band<0) return 0;
	return (map->get_data())->get_blue16((int)gbit(x,y,default_band));
	}
if(check_bounds_and_apply_wrap(x,y) || blue_band>=bands || blue_band<0) return 0;
return gbit(x,y,blue_band)?(ushort)65535:(ushort)0;
}


void bitData::set_red16(ushort c, int x, int y)
{
if(check_bounds(x,y) || red_band>=bands || red_band<0 || map) return;
sbit((int)c>>15,x,y,red_band);
}


void bitData::set_green16(ushort c, int x, int y)
{
if(check_bounds(x,y) || green_band>=bands || green_band<0 || map) return;
sbit((int)c>>15,x,y,green_band);
}


void bitData::set_blue16(ushort c, int x, int y)
{
if(check_bounds(x,y) || blue_band>=bands || blue_band<0 || map) return;
sbit((int)c>>15,x,y,blue_band);
}


int bitData::get_bit(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return 0;
return (int)gbit(x,y,b);
}


char bitData::get_char(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return 0;
return (char)gbit(x,y,b);
}


uchar bitData::get_uchar(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return 0;
return gbit(x,y,b);
}


short bitData::get_short(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return 0;
return (short)gbit(x,y,b);
}


ushort bitData::get_ushort(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return 0;
return (ushort)gbit(x,y,b);
}


long bitData::get_long(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return 0;
return (long)gbit(x,y,b);
}


ulong bitData::get_ulong(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return 0;
return (ulong)gbit(x,y,b);
}


float bitData::get_float(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return 0.0;
return (float)gbit(x,y,b);
}


double bitData::get_double(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return 0.0;
return (double)gbit(x,y,b);
}


COMPLEX_TYPE bitData::get_complex(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return complex_zero;
return (COMPLEX_TYPE)((double)gbit(x,y,b));
}


void bitData::set_bit(int v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
sbit(v&1,x,y,b);
}


void bitData::set_char(char v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
sbit((int)v&1,x,y,b);
}


void bitData::set_uchar(uchar v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
sbit((int)v&1,x,y,b);
}


void bitData::set_short(short v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
sbit((int)v&1,x,y,b);
}


void bitData::set_ushort(ushort v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
sbit((int)v&1,x,y,b);
}


void bitData::set_long(long v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
sbit((int)v&1,x,y,b);
}


void bitData::set_ulong(ulong v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
sbit((int)v&1,x,y,b);
}


void bitData::set_float(float v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
sbit(v!=0.0,x,y,b);
}


void bitData::set_double(double v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
sbit(v!=0.0,x,y,b);
}


void bitData::set_complex(COMPLEX_TYPE v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
sbit(v!=complex_zero,x,y,b);
}


// ****************************** charData *******************************

void charData::init()
{
data=NULL;
}


int charData::allocate(int x, int y, int b)
{
int	i;

free();

if(x<0 || y<0 || b<0) return 0;

xres=x; yres=y; bands=b;

data=(char **)malloc(bands*sizeof(char *));
if(!data) return 0;
for(i=0;i<bands;i++) {
	data[i]=(char *)malloc(xres*yres);
	if(!data[i]) return 0;
	}
return 1;
}


void charData::free()
{
int	i;

if(data) {
	for(i=0;i<bands;i++) ::free(data[i]);
	::free((char *)data);
	data=NULL;
	}
xres=0; yres=0; bands=0;
}


int charData::add_band()
{
if(!data) return 0;

bands++;
data=(char **)realloc(data,bands*sizeof(char *));
if(!data) return 0;
data[bands-1]=(char *)malloc(xres*yres);
if(!data[bands-1]) return 0;
return 1;
}


int charData::remove_band(int b)
{
int	i;

if(!data || check_band(b)) return 0;

bands--;

::free(data[b]);
for(i=b;i<bands;i++) data[i]=data[i+1];
data=(char **)realloc(data,bands*sizeof(char *));
if(!data) return 0;
return 1;
}


char *charData::get_data(int b)
{
if(check_band(b) || !data) return NULL;
return data[b];
}


void charData::get_color(int x, int y, uchar *r, uchar *g, uchar *b, uchar *a)
{
if (check_bounds_and_apply_wrap( x, y, r, g, b, a))
	return;

if(map) {
	if(default_band>=bands || default_band<0) { *r=0; *g=0; *b=0; }
	else	(map->get_data())->get_color((int)data[default_band][x+y*xres],0,r,g,b);
	}
else {	if(red_band>=bands || red_band<0) *r=0;
	else	*r=(uchar)data[red_band][x+y*xres];
	if(green_band>=bands || green_band<0) *g=0;
	else	*g=(uchar)data[green_band][x+y*xres];
	if(blue_band>=bands || blue_band<0) *b=0;
	else	*b=(uchar)data[blue_band][x+y*xres];
	}
if(a) {	if(alpha_band<0) *a=default_alpha;
	else	if(alpha_band>=bands) *a=0;
	else	*a=(uchar)data[alpha_band][x+y*xres];
	}
}


void charData::set_color(int x, int y, uchar r, uchar g, uchar b, uchar a)
{
if(check_bounds(x,y) || map) return;
if(red_band<bands && red_band>=0) data[red_band][x+y*xres]=(char)r;
if(green_band<bands && green_band>=0) data[green_band][x+y*xres]=(char)g;
if(blue_band<bands && blue_band>=0) data[blue_band][x+y*xres]=(char)b;
if(alpha_band>=0 && alpha_band<bands) data[alpha_band][x+y*xres]=(char)a;
}


uchar charData::get_alpha(int x, int y)
{
if(check_bounds_and_apply_wrap(x,y) || alpha_band>=bands) return 0;
if(alpha_band<0) return default_alpha;
return (uchar)data[alpha_band][x+y*xres];
}


void charData::set_alpha(int x, int y, uchar a)
{
if(check_bounds(x,y) || alpha_band<0 || alpha_band>=bands) return;
data[alpha_band][x+y*xres]=(char)a;
}


ushort charData::get_red16(int x, int y)
{
if(map) {
	if(check_bounds_and_apply_wrap(x,y) || default_band>=bands || default_band<0) return 0;
	return (map->get_data())->get_red16((int)data[default_band][x+y*xres]);
	}
if(check_bounds_and_apply_wrap(x,y) || red_band>=bands || red_band<0) return 0;
return ((ushort)data[red_band][x+y*xres])<<8;
}


ushort charData::get_green16(int x, int y)
{
if(map) {
	if(check_bounds_and_apply_wrap(x,y) || default_band>=bands || default_band<0) return 0;
	return (map->get_data())->get_green16((int)data[default_band][x+y*xres]);
	}
if(check_bounds_and_apply_wrap(x,y) || green_band>=bands || green_band<0) return 0;
return ((ushort)data[green_band][x+y*xres])<<8;
}


ushort charData::get_blue16(int x, int y)
{
if(map) {
	if(check_bounds_and_apply_wrap(x,y) || default_band>=bands || default_band<0) return 0;
	return (map->get_data())->get_blue16((int)data[default_band][x+y*xres]);
	}
if(check_bounds_and_apply_wrap(x,y) || blue_band>=bands || blue_band<0) return 0;
return ((ushort)data[blue_band][x+y*xres])<<8;
}


void charData::set_red16(ushort c, int x, int y)
{
if(check_bounds(x,y) || red_band>=bands || red_band<0 || map) return;
data[red_band][x+y*xres]=(char)(c>>8);
}


void charData::set_green16(ushort c, int x, int y)
{
if(check_bounds(x,y) || green_band>=bands || green_band<0 || map) return;
data[green_band][x+y*xres]=(char)(c>>8);
}


void charData::set_blue16(ushort c, int x, int y)
{
if(check_bounds(x,y) || blue_band>=bands || blue_band<0 || map) return;
data[blue_band][x+y*xres]=(char)(c>>8);
}


int charData::get_bit(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return 0;
return (int)data[b][x+y*xres]&1;
}


char charData::get_char(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return 0;
return data[b][x+y*xres];
}


uchar charData::get_uchar(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return 0;
return (uchar)data[b][x+y*xres];
}


short charData::get_short(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return 0;
return (short)data[b][x+y*xres];
}


ushort charData::get_ushort(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return 0;
return (ushort)data[b][x+y*xres];
}


long charData::get_long(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return 0;
return (long)data[b][x+y*xres];
}


ulong charData::get_ulong(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return 0;
return (ulong)data[b][x+y*xres];
}


float charData::get_float(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return 0.0;
return (float)data[b][x+y*xres];
}


double charData::get_double(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return 0.0;
return (double)data[b][x+y*xres];
}


COMPLEX_TYPE charData::get_complex(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return complex_zero;
return (COMPLEX_TYPE)((double)data[b][x+y*xres]);
}


void charData::set_bit(int v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
if(v&1) data[b][x+y*xres]=(char)-1;
else	data[b][x+y*xres]=(char)0;
}


void charData::set_char(char v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
data[b][x+y*xres]=v;
}


void charData::set_uchar(uchar v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
data[b][x+y*xres]=(char)v;
}


void charData::set_short(short v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
data[b][x+y*xres]=(char)v;
}


void charData::set_ushort(ushort v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
data[b][x+y*xres]=(char)v;
}


void charData::set_long(long v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
data[b][x+y*xres]=(char)v;
}


void charData::set_ulong(ulong v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
data[b][x+y*xres]=(char)v;
}


void charData::set_float(float v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
data[b][x+y*xres]=(char)v;
}


void charData::set_double(double v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
data[b][x+y*xres]=(char)v;
}


void charData::set_complex(COMPLEX_TYPE v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
data[b][x+y*xres]=(char)real(v);
}


// ****************************** ucharData *******************************

void ucharData::init()
{
data=NULL;
}


int ucharData::allocate(int x, int y, int b)
{
int	i;

free();

if(x<0 || y<0 || b<0) return 0;

xres=x; yres=y; bands=b;

data=(uchar **)malloc(bands*sizeof(uchar *));
if(!data) return 0;
for(i=0;i<bands;i++) {
	data[i]=(uchar *)malloc(xres*yres);
	if(!data[i]) return 0;
	}
return 1;
}


void ucharData::free()
{
int	i;

if(data) {
	for(i=0;i<bands;i++) ::free((char *)data[i]);
	::free((char *)data);
	data=NULL;
	}
xres=0; yres=0; bands=0;
}


int ucharData::add_band()
{
if(!data) return 0;

bands++;
data=(uchar **)realloc(data,bands*sizeof(uchar *));
if(!data) return 0;
data[bands-1]=(uchar *)malloc(xres*yres);
if(!data[bands-1]) return 0;
return 1;
}


int ucharData::remove_band(int b)
{
int	i;

if(!data || check_band(b)) return 0;

bands--;

::free((char *)data[b]);
for(i=b;i<bands;i++) data[i]=data[i+1];
data=(uchar **)realloc(data,bands*sizeof(uchar *));
if(!data) return 0;
return 1;
}


uchar *ucharData::get_data(int b)
{
if(check_band(b) || !data) return NULL;
return data[b];
}


void ucharData::get_color(int x, int y, uchar *r, uchar *g, uchar *b, uchar *a)
{

if (check_bounds_and_apply_wrap( x, y, r, g, b, a))
	return;

if(map) {
	if(default_band>=bands || default_band<0) { *r=0; *g=0; *b=0; }
	else	(map->get_data())->get_color((int)data[default_band][x+y*xres],0,r,g,b);
	}
else {	if(red_band>=bands || red_band<0) *r=0;
	else	*r=data[red_band][x+y*xres];
	if(green_band>=bands || green_band<0) *g=0;
	else	*g=data[green_band][x+y*xres];
	if(blue_band>=bands || blue_band<0) *b=0;
	else	*b=data[blue_band][x+y*xres];
	}
if(a) {	if(alpha_band<0) *a=default_alpha;
	else	if(alpha_band>=bands) *a=0;
	else	*a=data[alpha_band][x+y*xres];
	}
}

void ucharData::get_color_row(int x, int y, int length, uchar *r, uchar *g, uchar *b, uchar *a)
{

int	i, lx, llength;

// initialize output arrays to 0
memset(r, 0, length);
memset(g, 0, length);
memset(b, 0, length);
if(a) memset(a, 0, length);

if ( x < 0 ) {
	if(flgWrapState != IMAGE_WRAP_STATE_X && flgWrapState != IMAGE_WRAP_STATE_XY) {
		if(x + length < 0) return;
		r -= x;
		g -= x;
		b -= x;
		if(a) a -= x;
		length += x;
		if(length > xres) length = xres;
		x = 0;
	}
}

if (check_bounds_and_apply_wrap( x, y, r, g, b, a)) {
	return;
}

uchar	*red = &(data[red_band][x+y*xres]);
uchar	*green = &(data[green_band][x+y*xres]);
uchar	*blue = &(data[blue_band][x+y*xres]);
uchar	*alpha =  &(data[alpha_band][y*xres]);

if(map) {
	if(default_band>=bands || default_band<0) {
	} else {
		for(i=0; i<length; i++,x++) (map->get_data())->get_color((int)data[default_band][x+y*xres],0,r++,g++,b++);
	}
} else {	
	if(red_band>=bands || red_band<0) {
	} else	{
		lx = x;
		llength = length;
		while(lx+llength >= xres) {
			memcpy(r, red, xres-lx);
			r += xres-lx;
			red = &(data[red_band][y*xres]);
			if(flgWrapState != IMAGE_WRAP_STATE_X && flgWrapState != IMAGE_WRAP_STATE_XY) break;
			llength -= xres-lx;
			lx = 0;
		}
		if(x+length < xres || flgWrapState == IMAGE_WRAP_STATE_X || flgWrapState == IMAGE_WRAP_STATE_XY) {
			memcpy(r, red, llength);
		}
	}
	if(green_band>=bands || green_band<0) for(i=0; i<length; i++) { *(g++) = 0; }
	else	{
		lx = x;
		llength = length;
		while(lx+llength >= xres) {
			memcpy(g, green, xres-lx);
			g += xres-lx;
			green = &(data[green_band][y*xres]);
			if(flgWrapState != IMAGE_WRAP_STATE_X && flgWrapState != IMAGE_WRAP_STATE_XY) break;
			llength -= xres-lx;
			lx = 0;
		}
		if(x+length < xres || flgWrapState == IMAGE_WRAP_STATE_X || flgWrapState == IMAGE_WRAP_STATE_XY) {
			memcpy(g, green, llength);
		}
	}
	if(blue_band>=bands || blue_band<0) for(i=0; i<length; i++) { *(b++) = 0; }
	else	{
		lx = x;
		llength = length;
		while(lx+llength >= xres) {
			memcpy(b, blue, xres-lx);
			b += xres-lx;
			blue = &(data[blue_band][y*xres]);
			if(flgWrapState != IMAGE_WRAP_STATE_X && flgWrapState != IMAGE_WRAP_STATE_XY) break;
			llength -= xres-lx;
			lx = 0;
		}
		if(x+length < xres || flgWrapState == IMAGE_WRAP_STATE_X || flgWrapState == IMAGE_WRAP_STATE_XY) {
			memcpy(b, blue, llength);
		}
	}
	}
if(a) {	if(alpha_band<0)  for(i=0; i<length; i++) *a++ =default_alpha;
	else	if(alpha_band>=bands) for(i=0; i<length; i++) { *a++ = 0; }
	else	{
		lx = x;
		llength = length;
		while(lx+llength >= xres) {
			memcpy(a, alpha, xres-lx);
			a += xres-lx;
			alpha = &(data[alpha_band][y*xres]);
			if(flgWrapState != IMAGE_WRAP_STATE_X && flgWrapState != IMAGE_WRAP_STATE_XY) break;
			llength -= xres-lx;
			lx = 0;
		}
		if(x+length < xres || flgWrapState == IMAGE_WRAP_STATE_X || flgWrapState == IMAGE_WRAP_STATE_XY) {
			memcpy(a, alpha, llength);
		}
	}
	}
}



void ucharData::iget_color(double x, double y, uchar *r, uchar *g, uchar *b, uchar *a)
{
double	fx,fy,fx1,fy1,m1,m2,m3,m4;
int	ix,iy,off;
uchar	*ptr;

if (check_bounds_and_apply_wrap( x, y, r, g, b, a)) 
	return;

ix=(int)x; fx=x-ix; fx1=1.0-fx;
iy=(int)y; fy=y-iy; fy1=1.0-fy;
m1=fx1*fy1; m2=fx*fy1; m3=fx1*fy; m4=fx*fy;

off=ix+iy*xres;

if(map) {
	uchar	r1,r2,r3,r4,g1,g2,g3,g4,b1,b2,b3,b4;
	if(default_band>=bands || default_band<0) { *r=0; *g=0; *b=0; }
	ptr=data[default_band]+off;
	(map->get_data())->get_color(*ptr,0,&r1,&g1,&b1);
	(map->get_data())->get_color(*(ptr+1),0,&r2,&g2,&b2);
	(map->get_data())->get_color(*(ptr+xres),0,&r3,&g3,&b3);
	(map->get_data())->get_color(*(ptr+xres+1),0,&r4,&g4,&b4);
	*r=(uchar)(r1*m1+r2*m2+r3*m3+r4*m4);
	*g=(uchar)(g1*m1+g2*m2+g3*m3+g4*m4);
	*b=(uchar)(b1*m1+b2*m2+b3*m3+b4*m4);
	}
else {
	if(red_band>=bands || red_band<0) *r=0;
	else {	ptr=data[red_band]+off;
		*r=(uchar)(*ptr*m1+*(ptr+1)*m2+*(ptr+xres)*m3+*(ptr+xres+1)*m4); }
	if(green_band>=bands || green_band<0) *g=0;
	else {	ptr=data[green_band]+off;
		*g=(uchar)(*ptr*m1+*(ptr+1)*m2+*(ptr+xres)*m3+*(ptr+xres+1)*m4); }
	if(blue_band>=bands || blue_band<0) *b=0;
	else {	ptr=data[blue_band]+off;
		*b=(uchar)(*ptr*m1+*(ptr+1)*m2+*(ptr+xres)*m3+*(ptr+xres+1)*m4); }
	}

if(a) {	if(alpha_band<0) *a=default_alpha;
	else	if(alpha_band>=bands) *a=0;
	else {	ptr=data[alpha_band]+off;
		*a=(uchar)(*ptr*m1+*(ptr+1)*m2+*(ptr+xres)*m3+*(ptr+xres+1)*m4); }
	}
}


void ucharData::iget_color_row(double x, double y, double xstep, int length, uchar *r, uchar *g, uchar *b, uchar *a)
{
int	i, lx, llength;
int	min_x, max_x, min_y, max_y;
uchar	*top_r, *top_g, *top_b, *top_a;
uchar	*bot_r, *bot_g, *bot_b, *bot_a;
double	left_wt, right_wt, bot_wt, top_wt;

max_x = (int)floor(x + (length-1)*xstep) + 1;
min_x = (int)floor(x);
min_y = (int)floor(y);
max_y = min_y + 1;

llength = max_x - min_x + 1;

bot_r = (uchar *)malloc(8 * llength * sizeof(uchar));
bot_g = bot_r + llength;
bot_b = bot_g + llength;
bot_a = bot_b + llength;
top_r = bot_a + llength;
top_g = top_r + llength;
top_b = top_g + llength;
top_a = top_b + llength;

get_color_row(min_x, min_y, llength, bot_r, bot_g, bot_b, bot_a);
get_color_row(min_x, max_y, llength, top_r, top_g, top_b, top_a);

bot_wt = max_y - y;
top_wt = y - min_y;

for(i=0; i<length; i++) {
	lx = (int)floor(x);
	left_wt = (double)(lx + 1) - x;
	right_wt = x - (double)(lx);
	lx = lx - min_x;
	*r++ = (uchar)((bot_r[lx]*left_wt + bot_r[lx+1]*right_wt) * bot_wt +
	      (top_r[lx]*left_wt + top_r[lx+1]*right_wt) * top_wt);
	*g++ = (uchar)((bot_g[lx]*left_wt + bot_g[lx+1]*right_wt) * bot_wt +
	      (top_g[lx]*left_wt + top_g[lx+1]*right_wt) * top_wt);
	*b++ = (uchar)((bot_b[lx]*left_wt + bot_b[lx+1]*right_wt) * bot_wt +
	      (top_b[lx]*left_wt + top_b[lx+1]*right_wt) * top_wt);
	if(a) *a++ = (uchar)((bot_a[lx]*left_wt + bot_a[lx+1]*right_wt) * bot_wt +
	      (top_a[lx]*left_wt + top_a[lx+1]*right_wt) * top_wt);
	x += xstep;
}

::free(bot_r);

}


void ucharData::set_color(int x, int y, uchar r, uchar g, uchar b, uchar a)
{
if(check_bounds(x,y) || map) return;
if(red_band<bands && red_band>=0) data[red_band][x+y*xres]=r;
if(green_band<bands && green_band>=0) data[green_band][x+y*xres]=g;
if(blue_band<bands && blue_band>=0) data[blue_band][x+y*xres]=b;
if(alpha_band>=0 && alpha_band<bands) data[alpha_band][x+y*xres]=a;
}


uchar ucharData::get_alpha(int x, int y)
{
if (check_bounds_and_apply_wrap( x, y) || alpha_band>=bands) return 0;

if(alpha_band<0) return default_alpha;
return data[alpha_band][x+y*xres];
}


void ucharData::set_alpha(int x, int y, uchar a)
{
if(check_bounds(x,y) || alpha_band<0 || alpha_band>=bands) return;
data[alpha_band][x+y*xres]=a;
}


ushort ucharData::get_red16(int x, int y)
{
if(map) {
	if(check_bounds_and_apply_wrap(x,y) || default_band>=bands || default_band<0) return 0;
	return (map->get_data())->get_red16((int)data[default_band][x+y*xres]);
	}
if(check_bounds_and_apply_wrap(x,y) || red_band>=bands || red_band<0) return 0;
return ((ushort)data[red_band][x+y*xres])<<8;
}


ushort ucharData::get_green16(int x, int y)
{
if(map) {
	if(check_bounds_and_apply_wrap(x,y) || default_band>=bands || default_band<0) return 0;
	return (map->get_data())->get_green16((int)data[default_band][x+y*xres]);
	}
if(check_bounds_and_apply_wrap(x,y) || green_band>=bands || green_band<0) return 0;
return ((ushort)data[green_band][x+y*xres])<<8;
}


ushort ucharData::get_blue16(int x, int y)
{
if(map) {
	if(check_bounds_and_apply_wrap(x,y) || default_band>=bands || default_band<0) return 0;
	return (map->get_data())->get_blue16((int)data[default_band][x+y*xres]);
	}
if(check_bounds_and_apply_wrap(x,y) || blue_band>=bands || blue_band<0) return 0;
return ((ushort)data[blue_band][x+y*xres])<<8;
}


void ucharData::set_red16(ushort c, int x, int y)
{
if(check_bounds(x,y) || red_band>=bands || red_band<0 || map) return;
data[red_band][x+y*xres]=(uchar)(c>>8);
}


void ucharData::set_green16(ushort c, int x, int y)
{
if(check_bounds(x,y) || green_band>=bands || green_band<0 || map) return;
data[green_band][x+y*xres]=(uchar)(c>>8);
}


void ucharData::set_blue16(ushort c, int x, int y)
{
if(check_bounds(x,y) || blue_band>=bands || blue_band<0 || map) return;
data[blue_band][x+y*xres]=(uchar)(c>>8);
}


int ucharData::get_bit(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return 0;
return (int)data[b][x+y*xres]&1;
}


char ucharData::get_char(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return 0;
return (char)data[b][x+y*xres];
}


uchar ucharData::get_uchar(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return 0;
return data[b][x+y*xres];
}


short ucharData::get_short(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return 0;
return (short)data[b][x+y*xres];
}


ushort ucharData::get_ushort(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return 0;
return (ushort)data[b][x+y*xres];
}


long ucharData::get_long(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return 0;
return (long)data[b][x+y*xres];
}


ulong ucharData::get_ulong(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return 0;
return (ulong)data[b][x+y*xres];
}


float ucharData::get_float(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return 0.0;
return (float)data[b][x+y*xres];
}


double ucharData::get_double(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return 0.0;
return (double)data[b][x+y*xres];
}


COMPLEX_TYPE ucharData::get_complex(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return complex_zero;
return (COMPLEX_TYPE)((double)data[b][x+y*xres]);
}


void ucharData::set_bit(int v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
if(v&1) data[b][x+y*xres]=(uchar)255;
else	data[b][x+y*xres]=(uchar)0;
}


void ucharData::set_char(char v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
data[b][x+y*xres]=(uchar)v;
}


void ucharData::set_uchar(uchar v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
data[b][x+y*xres]=v;
}


void ucharData::set_short(short v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
data[b][x+y*xres]=(uchar)v;
}


void ucharData::set_ushort(ushort v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
data[b][x+y*xres]=(uchar)v;
}


void ucharData::set_long(long v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
data[b][x+y*xres]=(uchar)v;
}


void ucharData::set_ulong(ulong v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
data[b][x+y*xres]=(uchar)v;
}


void ucharData::set_float(float v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
data[b][x+y*xres]=(uchar)v;
}


void ucharData::set_double(double v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
data[b][x+y*xres]=(uchar)v;
}


void ucharData::set_complex(COMPLEX_TYPE v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
data[b][x+y*xres]=(uchar)real(v);
}


// ****************************** shortData *******************************

void shortData::init()
{
data=NULL;
}


int shortData::allocate(int x, int y, int b)
{
int	i;

free();

if(x<0 || y<0 || b<0) return 0;

xres=x; yres=y; bands=b;

data=(short **)malloc(bands*sizeof(short *));
if(!data) return 0;
for(i=0;i<bands;i++) {
	data[i]=(short *)malloc(xres*yres*2);
	if(!data[i]) return 0;
	}
return 1;
}


void shortData::free()
{
int	i;

if(data) {
	for(i=0;i<bands;i++) ::free((char *)data[i]);
	::free((char *)data);
	data=NULL;
	}
xres=0; yres=0; bands=0;
}


int shortData::add_band()
{
if(!data) return 0;

bands++;
data=(short **)realloc(data,bands*sizeof(short *));
if(!data) return 0;
data[bands-1]=(short *)malloc(xres*yres*2);
if(!data[bands-1]) return 0;
return 1;
}


int shortData::remove_band(int b)
{
int	i;

if(!data || check_band(b)) return 0;

bands--;

::free((char *)data[b]);
for(i=b;i<bands;i++) data[i]=data[i+1];
data=(short **)realloc(data,bands*sizeof(short *));
if(!data) return 0;
return 1;
}


short *shortData::get_data(int b)
{
if(check_band(b) || !data) return NULL;
return data[b];
}


void shortData::get_color(int x, int y, uchar *r, uchar *g, uchar *b, uchar *a)
{
if (check_bounds_and_apply_wrap(x,y,r,g,b,a)) return;

if(map) {
	if(default_band>=bands || default_band<0) { *r=0; *g=0; *b=0; }
	else	(map->get_data())->get_color((int)data[default_band][x+y*xres],0,r,g,b);
	}
else {	if(red_band>=bands || red_band<0) *r=0;
	else	*r=(uchar)data[red_band][x+y*xres];
	if(green_band>=bands || green_band<0) *g=0;
	else	*g=(uchar)data[green_band][x+y*xres];
	if(blue_band>=bands || blue_band<0) *b=0;
	else	*b=(uchar)data[blue_band][x+y*xres];
	}
if(a) {	if(alpha_band<0) *a=default_alpha;
	else	if(alpha_band>=bands) *a=0;
	else	*a=(uchar)data[alpha_band][x+y*xres];
	}
}


void shortData::set_color(int x, int y, uchar r, uchar g, uchar b, uchar a)
{
if(check_bounds(x,y) || map) return;
if(red_band<bands && red_band>=0) data[red_band][x+y*xres]=(short)r;
if(green_band<bands && green_band>=0) data[green_band][x+y*xres]=(short)g;
if(blue_band<bands && blue_band>=0) data[blue_band][x+y*xres]=(short)b;
if(alpha_band>=0 && alpha_band<bands) data[alpha_band][x+y*xres]=(short)a;
}


uchar shortData::get_alpha(int x, int y)
{
if(check_bounds_and_apply_wrap(x,y) || alpha_band>=bands) return 0;
if(alpha_band<0) return default_alpha;
return (uchar)data[alpha_band][x+y*xres];
}


void shortData::set_alpha(int x, int y, uchar a)
{
if(check_bounds(x,y) || alpha_band<0 || alpha_band>=bands) return;
data[alpha_band][x+y*xres]=(short)a;
}


ushort shortData::get_red16(int x, int y)
{
if(map) {
	if(check_bounds_and_apply_wrap(x,y) || default_band>=bands || default_band<0) return 0;
	return (map->get_data())->get_red16((int)data[default_band][x+y*xres]);
	}
if(check_bounds_and_apply_wrap(x,y) || red_band>=bands || red_band<0) return 0;
return (ushort)data[red_band][x+y*xres];
}


ushort shortData::get_green16(int x, int y)
{
if(map) {
	if(check_bounds_and_apply_wrap(x,y) || default_band>=bands || default_band<0) return 0;
	return (map->get_data())->get_green16((int)data[default_band][x+y*xres]);
	}
if(check_bounds_and_apply_wrap(x,y) || green_band>=bands || green_band<0) return 0;
return (ushort)data[green_band][x+y*xres];
}


ushort shortData::get_blue16(int x, int y)
{
if(map) {
	if(check_bounds_and_apply_wrap(x,y) || default_band>=bands || default_band<0) return 0;
	return (map->get_data())->get_blue16((int)data[default_band][x+y*xres]);
	}
if(check_bounds_and_apply_wrap(x,y) || blue_band>=bands || blue_band<0) return 0;
return (ushort)data[blue_band][x+y*xres];
}


void shortData::set_red16(ushort c, int x, int y)
{
if(check_bounds(x,y) || red_band>=bands || red_band<0 || map) return;
data[red_band][x+y*xres]=(short)c;
}


void shortData::set_green16(ushort c, int x, int y)
{
if(check_bounds(x,y) || green_band>=bands || green_band<0 || map) return;
data[green_band][x+y*xres]=(short)c;
}


void shortData::set_blue16(ushort c, int x, int y)
{
if(check_bounds(x,y) || blue_band>=bands || blue_band<0 || map) return;
data[blue_band][x+y*xres]=(short)c;
}


int shortData::get_bit(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return 0;
return (int)data[b][x+y*xres]&1;
}


char shortData::get_char(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return 0;
return (char)data[b][x+y*xres];
}


uchar shortData::get_uchar(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return 0;
return (uchar)data[b][x+y*xres];
}


short shortData::get_short(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return 0;
return data[b][x+y*xres];
}


ushort shortData::get_ushort(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return 0;
return (ushort)data[b][x+y*xres];
}


long shortData::get_long(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return 0;
return (long)data[b][x+y*xres];
}


ulong shortData::get_ulong(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return 0;
return (ulong)data[b][x+y*xres];
}


float shortData::get_float(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return 0.0;
return (float)data[b][x+y*xres];
}


double shortData::get_double(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return 0.0;
return (double)data[b][x+y*xres];
}


COMPLEX_TYPE shortData::get_complex(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return complex_zero;
return (COMPLEX_TYPE)((double)data[b][x+y*xres]);
}


void shortData::set_bit(int v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
if(v&1) data[b][x+y*xres]=(short)-1;
else	data[b][x+y*xres]=(short)0;
}


void shortData::set_char(char v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
data[b][x+y*xres]=(short)v;
}


void shortData::set_uchar(uchar v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
data[b][x+y*xres]=(short)v;
}


void shortData::set_short(short v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
data[b][x+y*xres]=v;
}


void shortData::set_ushort(ushort v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
data[b][x+y*xres]=(short)v;
}


void shortData::set_long(long v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
data[b][x+y*xres]=(short)v;
}


void shortData::set_ulong(ulong v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
data[b][x+y*xres]=(short)v;
}


void shortData::set_float(float v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
data[b][x+y*xres]=(short)v;
}


void shortData::set_double(double v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
data[b][x+y*xres]=(short)v;
}


void shortData::set_complex(COMPLEX_TYPE v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
data[b][x+y*xres]=(short)real(v);
}


// ****************************** ushortData *******************************

void ushortData::init()
{
data=NULL;
}


int ushortData::allocate(int x, int y, int b)
{
int	i;

free();

if(x<0 || y<0 || b<0) return 0;

xres=x; yres=y; bands=b;

data=(ushort **)malloc(bands*sizeof(ushort *));
if(!data) return 0;
for(i=0;i<bands;i++) {
	data[i]=(ushort *)malloc(xres*yres*2);
	if(!data[i]) return 0;
	}
return 1;
}


void ushortData::free()
{
int	i;

if(data) {
	for(i=0;i<bands;i++) ::free((char *)data[i]);
	::free((char *)data);
	data=NULL;
	}
xres=0; yres=0; bands=0;
}


int ushortData::add_band()
{
if(!data) return 0;

bands++;
data=(ushort **)realloc(data,bands*sizeof(ushort *));
if(!data) return 0;
data[bands-1]=(ushort *)malloc(xres*yres*2);
if(!data[bands-1]) return 0;
return 1;
}


int ushortData::remove_band(int b)
{
int	i;

if(!data || check_band(b)) return 0;

bands--;

::free((char *)data[b]);
for(i=b;i<bands;i++) data[i]=data[i+1];
data=(ushort **)realloc(data,bands*sizeof(ushort *));
if(!data) return 0;
return 1;
}


ushort *ushortData::get_data(int b)
{
if(check_band(b) || !data) return NULL;
return data[b];
}


void ushortData::get_color(int x, int y, uchar *r, uchar *g, uchar *b, uchar *a)
{
if (check_bounds_and_apply_wrap( x, y, r, g, b, a))
	return;

if(map) {
	if(default_band>=bands || default_band<0) { *r=0; *g=0; *b=0; }
	else	(map->get_data())->get_color((int)data[default_band][x+y*xres],0,r,g,b);
	}
else {	if(red_band>=bands || red_band<0) *r=0;
	else	*r=(uchar)data[red_band][x+y*xres];
	if(green_band>=bands || green_band<0) *g=0;
	else	*g=(uchar)data[green_band][x+y*xres];
	if(blue_band>=bands || blue_band<0) *b=0;
	else	*b=(uchar)data[blue_band][x+y*xres];
	}
if(a) {	if(alpha_band<0) *a=default_alpha;
	else	if(alpha_band>=bands) *a=0;
	else	*a=(uchar)data[alpha_band][x+y*xres];
	}
}


void ushortData::set_color(int x, int y, uchar r, uchar g, uchar b, uchar a)
{
if(check_bounds(x,y) || map) return;
if(red_band<bands && red_band>=0) data[red_band][x+y*xres]=(ushort)r;
if(green_band<bands && green_band>=0) data[green_band][x+y*xres]=(ushort)g;
if(blue_band<bands && blue_band>=0) data[blue_band][x+y*xres]=(ushort)b;
if(alpha_band>=0 && alpha_band<bands) data[alpha_band][x+y*xres]=(ushort)a;
}


uchar ushortData::get_alpha(int x, int y)
{
if(check_bounds_and_apply_wrap(x,y) || alpha_band>=bands) return 0;
if(alpha_band<0) return default_alpha;
return (uchar)data[alpha_band][x+y*xres];
}


void ushortData::set_alpha(int x, int y, uchar a)
{
if(check_bounds(x,y) || alpha_band<0 || alpha_band>=bands) return;
data[alpha_band][x+y*xres]=(ushort)a;
}


ushort ushortData::get_red16(int x, int y)
{
if(map) {
	if(check_bounds_and_apply_wrap(x,y) || default_band>=bands || default_band<0) return 0;
	return (map->get_data())->get_red16((int)data[default_band][x+y*xres]);
	}
if(check_bounds_and_apply_wrap(x,y) || red_band>=bands || red_band<0) return 0;
return data[red_band][x+y*xres];
}


ushort ushortData::get_green16(int x, int y)
{
if(map) {
	if(check_bounds_and_apply_wrap(x,y) || default_band>=bands || default_band<0) return 0;
	return (map->get_data())->get_green16((int)data[default_band][x+y*xres]);
	}
if(check_bounds_and_apply_wrap(x,y) || green_band>=bands || green_band<0) return 0;
return data[green_band][x+y*xres];
}


ushort ushortData::get_blue16(int x, int y)
{
if(map) {
	if(check_bounds_and_apply_wrap(x,y) || default_band>=bands || default_band<0) return 0;
	return (map->get_data())->get_blue16((int)data[default_band][x+y*xres]);
	}
if(check_bounds_and_apply_wrap(x,y) || blue_band>=bands || blue_band<0) return 0;
return data[blue_band][x+y*xres];
}


void ushortData::set_red16(ushort c, int x, int y)
{
if(check_bounds(x,y) || red_band>=bands || red_band<0 || map) return;
data[red_band][x+y*xres]=c;
}


void ushortData::set_green16(ushort c, int x, int y)
{
if(check_bounds(x,y) || green_band>=bands || green_band<0 || map) return;
data[green_band][x+y*xres]=c;
}


void ushortData::set_blue16(ushort c, int x, int y)
{
if(check_bounds(x,y) || blue_band>=bands || blue_band<0 || map) return;
data[blue_band][x+y*xres]=c;
}


int ushortData::get_bit(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return 0;
return (int)data[b][x+y*xres]&1;
}


char ushortData::get_char(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return 0;
return (char)data[b][x+y*xres];
}


uchar ushortData::get_uchar(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return 0;
return (uchar)data[b][x+y*xres];
}


short ushortData::get_short(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return 0;
return (short)data[b][x+y*xres];
}


ushort ushortData::get_ushort(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return 0;
return data[b][x+y*xres];
}


long ushortData::get_long(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return 0;
return (long)data[b][x+y*xres];
}


ulong ushortData::get_ulong(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return 0;
return (ulong)data[b][x+y*xres];
}


float ushortData::get_float(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return 0.0;
return (float)data[b][x+y*xres];
}


double ushortData::get_double(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return 0.0;
return (double)data[b][x+y*xres];
}


COMPLEX_TYPE ushortData::get_complex(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return complex_zero;
return (COMPLEX_TYPE)((double)data[b][x+y*xres]);
}


void ushortData::set_bit(int v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
if(v&1) data[b][x+y*xres]=(ushort)65535;
else	data[b][x+y*xres]=(ushort)0;
}


void ushortData::set_char(char v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
data[b][x+y*xres]=(ushort)v;
}


void ushortData::set_uchar(uchar v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
data[b][x+y*xres]=(ushort)v;
}


void ushortData::set_short(short v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
data[b][x+y*xres]=(ushort)v;
}


void ushortData::set_ushort(ushort v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
data[b][x+y*xres]=v;
}


void ushortData::set_long(long v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
data[b][x+y*xres]=(ushort)v;
}


void ushortData::set_ulong(ulong v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
data[b][x+y*xres]=(ushort)v;
}


void ushortData::set_float(float v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
data[b][x+y*xres]=(ushort)v;
}


void ushortData::set_double(double v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
data[b][x+y*xres]=(ushort)v;
}


void ushortData::set_complex(COMPLEX_TYPE v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
data[b][x+y*xres]=(ushort)real(v);
}


// ****************************** longData *******************************

void longData::init()
{
data=NULL;
}


int longData::allocate(int x, int y, int b)
{
int	i;

free();

if(x<0 || y<0 || b<0) return 0;

xres=x; yres=y; bands=b;

data=(long **)malloc(bands*sizeof(long *));
if(!data) return 0;
for(i=0;i<bands;i++) {
	data[i]=(long *)malloc(xres*yres*4);
	if(!data[i]) return 0;
	}
return 1;
}


void longData::free()
{
int	i;

if(data) {
	for(i=0;i<bands;i++) ::free((char *)data[i]);
	::free((char *)data);
	data=NULL;
	}
xres=0; yres=0; bands=0;
}


int longData::add_band()
{
if(!data) return 0;

bands++;
data=(long **)realloc(data,bands*sizeof(long *));
if(!data) return 0;
data[bands-1]=(long *)malloc(xres*yres*4);
if(!data[bands-1]) return 0;
return 1;
}


int longData::remove_band(int b)
{
int	i;

if(!data || check_band(b)) return 0;

bands--;

::free((char *)data[b]);
for(i=b;i<bands;i++) data[i]=data[i+1];
data=(long **)realloc(data,bands*sizeof(long *));
if(!data) return 0;
return 1;
}


long *longData::get_data(int b)
{
if(check_band(b) || !data) return NULL;
return data[b];
}


void longData::get_color(int x, int y, uchar *r, uchar *g, uchar *b, uchar *a)
{
if (check_bounds_and_apply_wrap( x, y, r, g, b, a))
	return;

if(map) {
	if(default_band>=bands || default_band<0) { *r=0; *g=0; *b=0; }
	else	(map->get_data())->get_color((int)data[default_band][x+y*xres],0,r,g,b);
	}
else {	if(red_band>=bands || red_band<0) *r=0;
	else	*r=(uchar)data[red_band][x+y*xres];
	if(green_band>=bands || green_band<0) *g=0;
	else	*g=(uchar)data[green_band][x+y*xres];
	if(blue_band>=bands || blue_band<0) *b=0;
	else	*b=(uchar)data[blue_band][x+y*xres];
	}
if(a) {	if(alpha_band<0) *a=default_alpha;
	else	if(alpha_band>=bands) *a=0;
	else	*a=(uchar)data[alpha_band][x+y*xres];
	}
}


void longData::set_color(int x, int y, uchar r, uchar g, uchar b, uchar a)
{
if(check_bounds(x,y) || map) return;
if(red_band<bands && red_band>=0) data[red_band][x+y*xres]=(long)r;
if(green_band<bands && green_band>=0) data[green_band][x+y*xres]=(long)g;
if(blue_band<bands && blue_band>=0) data[blue_band][x+y*xres]=(long)b;
if(alpha_band>=0 && alpha_band<bands) data[alpha_band][x+y*xres]=(long)a;
}


uchar longData::get_alpha(int x, int y)
{
if(check_bounds_and_apply_wrap(x,y) || alpha_band>=bands) return 0;
if(alpha_band<0) return default_alpha;
return (uchar)data[alpha_band][x+y*xres];
}


void longData::set_alpha(int x, int y, uchar a)
{
if(check_bounds(x,y) || alpha_band<0 || alpha_band>=bands) return;
data[alpha_band][x+y*xres]=(long)a;
}


ushort longData::get_red16(int x, int y)
{
if(map) {
	if(check_bounds_and_apply_wrap(x,y) || default_band>=bands || default_band<0) return 0;
	return (map->get_data())->get_red16((int)data[default_band][x+y*xres]);
	}
if(check_bounds_and_apply_wrap(x,y) || red_band>=bands || red_band<0) return 0;
return (ushort)data[red_band][x+y*xres];
}


ushort longData::get_green16(int x, int y)
{
if(map) {
	if(check_bounds_and_apply_wrap(x,y) || default_band>=bands || default_band<0) return 0;
	return (map->get_data())->get_green16((int)data[default_band][x+y*xres]);
	}
if(check_bounds_and_apply_wrap(x,y) || green_band>=bands || green_band<0) return 0;
return (ushort)data[green_band][x+y*xres];
}


ushort longData::get_blue16(int x, int y)
{
if(map) {
	if(check_bounds_and_apply_wrap(x,y) || default_band>=bands || default_band<0) return 0;
	return (map->get_data())->get_blue16((int)data[default_band][x+y*xres]);
	}
if(check_bounds_and_apply_wrap(x,y) || blue_band>=bands || blue_band<0) return 0;
return (ushort)data[blue_band][x+y*xres];
}


void longData::set_red16(ushort c, int x, int y)
{
if(check_bounds(x,y) || red_band>=bands || red_band<0 || map) return;
data[red_band][x+y*xres]=(long)c;
}


void longData::set_green16(ushort c, int x, int y)
{
if(check_bounds(x,y) || green_band>=bands || green_band<0 || map) return;
data[green_band][x+y*xres]=(long)c;
}


void longData::set_blue16(ushort c, int x, int y)
{
if(check_bounds(x,y) || blue_band>=bands || blue_band<0 || map) return;
data[blue_band][x+y*xres]=(long)c;
}


int longData::get_bit(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return 0;
return (int)data[b][x+y*xres]&1;
}


char longData::get_char(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return 0;
return (char)data[b][x+y*xres];
}


uchar longData::get_uchar(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return 0;
return (uchar)data[b][x+y*xres];
}


short longData::get_short(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return 0;
return (short)data[b][x+y*xres];
}


ushort longData::get_ushort(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return 0;
return (ushort)data[b][x+y*xres];
}


long longData::get_long(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return 0;
return data[b][x+y*xres];
}


ulong longData::get_ulong(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return 0;
return (ulong)data[b][x+y*xres];
}


float longData::get_float(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return 0.0;
return (float)data[b][x+y*xres];
}


double longData::get_double(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return 0.0;
return (double)data[b][x+y*xres];
}


COMPLEX_TYPE longData::get_complex(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return complex_zero;
return (COMPLEX_TYPE)((double)data[b][x+y*xres]);
}


void longData::set_bit(int v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
if(v&1) data[b][x+y*xres]=(long)-1;
else	data[b][x+y*xres]=(long)0;
}


void longData::set_char(char v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
data[b][x+y*xres]=(long)v;
}


void longData::set_uchar(uchar v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
data[b][x+y*xres]=(long)v;
}


void longData::set_short(short v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
data[b][x+y*xres]=(long)v;
}


void longData::set_ushort(ushort v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
data[b][x+y*xres]=(long)v;
}


void longData::set_long(long v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
data[b][x+y*xres]=v;
}


void longData::set_ulong(ulong v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
data[b][x+y*xres]=(long)v;
}


void longData::set_float(float v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
data[b][x+y*xres]=(long)v;
}


void longData::set_double(double v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
data[b][x+y*xres]=(long)v;
}


void longData::set_complex(COMPLEX_TYPE v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
data[b][x+y*xres]=(long)real(v);
}


// ****************************** ulongData *******************************

void ulongData::init()
{
data=NULL;
}


int ulongData::allocate(int x, int y, int b)
{
int	i;

free();

if(x<0 || y<0 || b<0) return 0;

xres=x; yres=y; bands=b;

data=(ulong **)malloc(bands*sizeof(ulong *));
if(!data) return 0;
for(i=0;i<bands;i++) {
	data[i]=(ulong *)malloc(xres*yres*4);
	if(!data[i]) return 0;
	}
return 1;
}


void ulongData::free()
{
int	i;

if(data) {
	for(i=0;i<bands;i++) ::free((char *)data[i]);
	::free((char *)data);
	data=NULL;
	}
xres=0; yres=0; bands=0;
}


int ulongData::add_band()
{
if(!data) return 0;

bands++;
data=(ulong **)realloc(data,bands*sizeof(ulong *));
if(!data) return 0;
data[bands-1]=(ulong *)malloc(xres*yres*4);
if(!data[bands-1]) return 0;
return 1;
}


int ulongData::remove_band(int b)
{
int	i;

if(!data || check_band(b)) return 0;

bands--;

::free((char *)data[b]);
for(i=b;i<bands;i++) data[i]=data[i+1];
data=(ulong **)realloc(data,bands*sizeof(ulong *));
if(!data) return 0;
return 1;
}


ulong *ulongData::get_data(int b)
{
if(check_band(b) || !data) return NULL;
return data[b];
}


void ulongData::get_color(int x, int y, uchar *r, uchar *g, uchar *b, uchar *a)
{
if (check_bounds_and_apply_wrap( x, y, r, g, b, a))
	return;

if(map) {
	if(default_band>=bands || default_band<0) { *r=0; *g=0; *b=0; }
	else	(map->get_data())->get_color((int)data[default_band][x+y*xres],0,r,g,b);
	}
else {	if(red_band>=bands || red_band<0) *r=0;
	else	*r=(uchar)data[red_band][x+y*xres];
	if(green_band>=bands || green_band<0) *g=0;
	else	*g=(uchar)data[green_band][x+y*xres];
	if(blue_band>=bands || blue_band<0) *b=0;
	else	*b=(uchar)data[blue_band][x+y*xres];
	}
if(a) {	if(alpha_band<0) *a=default_alpha;
	else	if(alpha_band>=bands) *a=0;
	else	*a=(uchar)data[alpha_band][x+y*xres];
	}
}


void ulongData::set_color(int x, int y, uchar r, uchar g, uchar b, uchar a)
{
if(check_bounds(x,y) || map) return;
if(red_band<bands && red_band>=0) data[red_band][x+y*xres]=(ulong)r;
if(green_band<bands && green_band>=0) data[green_band][x+y*xres]=(ulong)g;
if(blue_band<bands && blue_band>=0) data[blue_band][x+y*xres]=(ulong)b;
if(alpha_band>=0 && alpha_band<bands) data[alpha_band][x+y*xres]=(ulong)a;
}


uchar ulongData::get_alpha(int x, int y)
{
if(check_bounds_and_apply_wrap(x,y) || alpha_band>=bands) return 0;
if(alpha_band<0) return default_alpha;
return (uchar)data[alpha_band][x+y*xres];
}


void ulongData::set_alpha(int x, int y, uchar a)
{
if(check_bounds(x,y) || alpha_band<0 || alpha_band>=bands) return;
data[alpha_band][x+y*xres]=(ulong)a;
}


ushort ulongData::get_red16(int x, int y)
{
if(map) {
	if(check_bounds_and_apply_wrap(x,y) || default_band>=bands || default_band<0) return 0;
	return (map->get_data())->get_red16((int)data[default_band][x+y*xres]);
	}
if(check_bounds_and_apply_wrap(x,y) || red_band>=bands || red_band<0) return 0;
return (ushort)data[red_band][x+y*xres];
}


ushort ulongData::get_green16(int x, int y)
{
if(map) {
	if(check_bounds_and_apply_wrap(x,y) || default_band>=bands || default_band<0) return 0;
	return (map->get_data())->get_green16((int)data[default_band][x+y*xres]);
	}
if(check_bounds_and_apply_wrap(x,y) || green_band>=bands || green_band<0) return 0;
return (ushort)data[green_band][x+y*xres];
}


ushort ulongData::get_blue16(int x, int y)
{
if(map) {
	if(check_bounds_and_apply_wrap(x,y) || default_band>=bands || default_band<0) return 0;
	return (map->get_data())->get_blue16((int)data[default_band][x+y*xres]);
	}
if(check_bounds_and_apply_wrap(x,y) || blue_band>=bands || blue_band<0) return 0;
return (ushort)data[blue_band][x+y*xres];
}


void ulongData::set_red16(ushort c, int x, int y)
{
if(check_bounds(x,y) || red_band>=bands || red_band<0 || map) return;
data[red_band][x+y*xres]=(ulong)c;
}


void ulongData::set_green16(ushort c, int x, int y)
{
if(check_bounds(x,y) || green_band>=bands || green_band<0 || map) return;
data[green_band][x+y*xres]=(ulong)c;
}


void ulongData::set_blue16(ushort c, int x, int y)
{
if(check_bounds(x,y) || blue_band>=bands || blue_band<0 || map) return;
data[blue_band][x+y*xres]=(ulong)c;
}


int ulongData::get_bit(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return 0;
return (int)data[b][x+y*xres]&1;
}


char ulongData::get_char(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return 0;
return (char)data[b][x+y*xres];
}


uchar ulongData::get_uchar(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return 0;
return (uchar)data[b][x+y*xres];
}


short ulongData::get_short(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return 0;
return (short)data[b][x+y*xres];
}


ushort ulongData::get_ushort(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return 0;
return (ushort)data[b][x+y*xres];
}


long ulongData::get_long(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return 0;
return (long)data[b][x+y*xres];
}


ulong ulongData::get_ulong(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return 0;
return data[b][x+y*xres];
}


float ulongData::get_float(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return 0.0;
return (float)data[b][x+y*xres];
}


double ulongData::get_double(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return 0.0;
return (double)data[b][x+y*xres];
}


COMPLEX_TYPE ulongData::get_complex(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return complex_zero;
return (COMPLEX_TYPE)((double)data[b][x+y*xres]);
}


void ulongData::set_bit(int v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
if(v&1) data[b][x+y*xres]=(ulong)0xffffffff;
else	data[b][x+y*xres]=(ulong)0;
}


void ulongData::set_char(char v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
data[b][x+y*xres]=(ulong)v;
}


void ulongData::set_uchar(uchar v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
data[b][x+y*xres]=(ulong)v;
}


void ulongData::set_short(short v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
data[b][x+y*xres]=(ulong)v;
}


void ulongData::set_ushort(ushort v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
data[b][x+y*xres]=(ulong)v;
}


void ulongData::set_long(long v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
data[b][x+y*xres]=(ulong)v;
}


void ulongData::set_ulong(ulong v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
data[b][x+y*xres]=v;
}


void ulongData::set_float(float v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
data[b][x+y*xres]=(ulong)v;
}


void ulongData::set_double(double v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
data[b][x+y*xres]=(ulong)v;
}


void ulongData::set_complex(COMPLEX_TYPE v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
data[b][x+y*xres]=(ulong)real(v);
}


// ****************************** floatData *******************************

void floatData::init()
{
data=NULL;
}


int floatData::allocate(int x, int y, int b)
{
int	i;

free();

if(x<0 || y<0 || b<0) return 0;

xres=x; yres=y; bands=b;

data=(float **)malloc(bands*sizeof(float *));
if(!data) return 0;
for(i=0;i<bands;i++) {
	data[i]=(float *)malloc(xres*yres*sizeof(float));
	if(!data[i]) return 0;
	}
return 1;
}


void floatData::free()
{
int	i;

if(data) {
	for(i=0;i<bands;i++) ::free((char *)data[i]);
	::free((char *)data);
	data=NULL;
	}
xres=0; yres=0; bands=0;
}


int floatData::add_band()
{
if(!data) return 0;

bands++;
data=(float **)realloc(data,bands*sizeof(float *));
if(!data) return 0;
data[bands-1]=(float *)malloc(xres*yres*sizeof(float));
if(!data[bands-1]) return 0;
return 1;
}


int floatData::remove_band(int b)
{
int	i;

if(!data || check_band(b)) return 0;

bands--;

::free((char *)data[b]);
for(i=b;i<bands;i++) data[i]=data[i+1];
data=(float **)realloc(data,bands*sizeof(float *));
if(!data) return 0;
return 1;
}


float *floatData::get_data(int b)
{
if(check_band(b) || !data) return NULL;
return data[b];
}


void floatData::get_color(int x, int y, uchar *r, uchar *g, uchar *b, uchar *a)
{
if (check_bounds_and_apply_wrap( x, y, r, g, b, a))
	return;

if(map) {
	if(default_band>=bands || default_band<0) { *r=0; *g=0; *b=0; }
	else	(map->get_data())->get_color((int)data[default_band][x+y*xres],0,r,g,b);
	}
else {	if(red_band>=bands || red_band<0) *r=0;
	else	*r=(uchar)(255.0*data[red_band][x+y*xres]+0.5);
	if(green_band>=bands || green_band<0) *g=0;
	else	*g=(uchar)(255.0*data[green_band][x+y*xres]+0.5);
	if(blue_band>=bands || blue_band<0) *b=0;
	else	*b=(uchar)(255.0*data[blue_band][x+y*xres]+0.5);
	}
if(a) {	if(alpha_band<0) *a=default_alpha;
	else	if(alpha_band>=bands) *a=0;
	else	*a=(uchar)(255.0*data[alpha_band][x+y*xres]+0.5);
	}
}


void floatData::set_color(int x, int y, uchar r, uchar g, uchar b, uchar a)
{
if(check_bounds(x,y) || map) return;
if(red_band<bands && red_band>=0) data[red_band][x+y*xres]=(float)r*0.0039215686;
if(green_band<bands && green_band>=0) data[green_band][x+y*xres]=(float)g*0.0039215686;
if(blue_band<bands && blue_band>=0) data[blue_band][x+y*xres]=(float)b*0.0039215686;
if(alpha_band>=0 && alpha_band<bands) data[alpha_band][x+y*xres]=(float)a*0.0039215686;
}


uchar floatData::get_alpha(int x, int y)
{
if(check_bounds_and_apply_wrap(x,y) || alpha_band>=bands) return 0;
if(alpha_band<0) return default_alpha;
return (uchar)(255.0*data[alpha_band][x+y*xres]+0.5);
}


void floatData::set_alpha(int x, int y, uchar a)
{
if(check_bounds(x,y) || alpha_band<0 || alpha_band>=bands) return;
data[alpha_band][x+y*xres]=(float)a*0.0039215686;
}


ushort floatData::get_red16(int x, int y)
{
if(map) {
	if(check_bounds_and_apply_wrap(x,y) || default_band>=bands || default_band<0) return 0;
	return (map->get_data())->get_red16((int)data[default_band][x+y*xres]);
	}
if(check_bounds_and_apply_wrap(x,y) || red_band>=bands || red_band<0) return 0;
return (ushort)(65535.0*data[red_band][x+y*xres]+0.5);
}


ushort floatData::get_green16(int x, int y)
{
if(map) {
	if(check_bounds_and_apply_wrap(x,y) || default_band>=bands || default_band<0) return 0;
	return (map->get_data())->get_green16((int)data[default_band][x+y*xres]);
	}
if(check_bounds_and_apply_wrap(x,y) || green_band>=bands || green_band<0) return 0;
return (ushort)(65535.0*data[green_band][x+y*xres]+0.5);
}


ushort floatData::get_blue16(int x, int y)
{
if(map) {
	if(check_bounds_and_apply_wrap(x,y) || default_band>=bands || default_band<0) return 0;
	return (map->get_data())->get_blue16((int)data[default_band][x+y*xres]);
	}
if(check_bounds_and_apply_wrap(x,y) || blue_band>=bands || blue_band<0) return 0;
return (ushort)(65535.0*data[blue_band][x+y*xres]+0.5);
}


void floatData::set_red16(ushort c, int x, int y)
{
if(check_bounds(x,y) || red_band>=bands || red_band<0 || map) return;
data[red_band][x+y*xres]=(float)c*0.00001525902;
}


void floatData::set_green16(ushort c, int x, int y)
{
if(check_bounds(x,y) || green_band>=bands || green_band<0 || map) return;
data[green_band][x+y*xres]=(float)c*0.00001525902;
}


void floatData::set_blue16(ushort c, int x, int y)
{
if(check_bounds(x,y) || blue_band>=bands || blue_band<0 || map) return;
data[blue_band][x+y*xres]=(float)c*0.00001525902;
}


int floatData::get_bit(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return 0;
return(data[b][x+y*xres]!=0.0);
}


char floatData::get_char(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return 0;
return (char)data[b][x+y*xres];
}


uchar floatData::get_uchar(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return 0;
return (uchar)data[b][x+y*xres];
}


short floatData::get_short(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return 0;
return (short)data[b][x+y*xres];
}


ushort floatData::get_ushort(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return 0;
return (ushort)data[b][x+y*xres];
}


long floatData::get_long(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return 0;
return (long)data[b][x+y*xres];
}


ulong floatData::get_ulong(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return 0;
return (ulong)data[b][x+y*xres];
}


float floatData::get_float(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return 0.0;
return data[b][x+y*xres];
}


double floatData::get_double(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return 0.0;
return (double)data[b][x+y*xres];
}


COMPLEX_TYPE floatData::get_complex(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return complex_zero;
return (COMPLEX_TYPE)data[b][x+y*xres];
}


void floatData::set_bit(int v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
data[b][x+y*xres]=(float)(v&1);
}


void floatData::set_char(char v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
data[b][x+y*xres]=(float)v;
}


void floatData::set_uchar(uchar v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
data[b][x+y*xres]=(float)v;
}


void floatData::set_short(short v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
data[b][x+y*xres]=(float)v;
}


void floatData::set_ushort(ushort v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
data[b][x+y*xres]=(float)v;
}


void floatData::set_long(long v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
data[b][x+y*xres]=(float)v;
}


void floatData::set_ulong(ulong v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
data[b][x+y*xres]=(float)v;
}


void floatData::set_float(float v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
data[b][x+y*xres]=v;
}


void floatData::set_double(double v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
data[b][x+y*xres]=(float)v;
}


void floatData::set_complex(COMPLEX_TYPE v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
data[b][x+y*xres]=(float)real(v);
}


// ****************************** doubleData *******************************

void doubleData::init()
{
data=NULL;
}


int doubleData::allocate(int x, int y, int b)
{
int	i;

free();

if(x<0 || y<0 || b<0) return 0;

xres=x; yres=y; bands=b;

data=(double **)malloc(bands*sizeof(double *));
if(!data) return 0;
for(i=0;i<bands;i++) {
	data[i]=(double *)malloc(xres*yres*sizeof(double));
	if(!data[i]) return 0;
	}
return 1;
}


void doubleData::free()
{
int	i;

if(data) {
	for(i=0;i<bands;i++) ::free((char *)data[i]);
	::free((char *)data);
	data=NULL;
	}
xres=0; yres=0; bands=0;
}


int doubleData::add_band()
{
if(!data) return 0;

bands++;
data=(double **)realloc(data,bands*sizeof(double *));
if(!data) return 0;
data[bands-1]=(double *)malloc(xres*yres*sizeof(double));
if(!data[bands-1]) return 0;
return 1;
}


int doubleData::remove_band(int b)
{
int	i;

if(!data || check_band(b)) return 0;

bands--;

::free((char *)data[b]);
for(i=b;i<bands;i++) data[i]=data[i+1];
data=(double **)realloc(data,bands*sizeof(double *));
if(!data) return 0;
return 1;
}


double *doubleData::get_data(int b)
{
if(check_band(b) || !data) return NULL;
return data[b];
}


void doubleData::get_color(int x, int y, uchar *r, uchar *g, uchar *b, uchar *a)
{
if (check_bounds_and_apply_wrap( x, y, r, g, b, a))
	return;

if(map) {
	if(default_band>=bands || default_band<0) { *r=0; *g=0; *b=0; }
	else	(map->get_data())->get_color((int)data[default_band][x+y*xres],0,r,g,b);
	}
else {	if(red_band>=bands || red_band<0) *r=0;
	else	*r=(uchar)(255.0*data[red_band][x+y*xres]+0.5);
	if(green_band>=bands || green_band<0) *g=0;
	else	*g=(uchar)(255.0*data[green_band][x+y*xres]+0.5);
	if(blue_band>=bands || blue_band<0) *b=0;
	else	*b=(uchar)(255.0*data[blue_band][x+y*xres]+0.5);
	}
if(a) {	if(alpha_band<0) *a=default_alpha;
	else	if(alpha_band>=bands) *a=0;
	else	*a=(uchar)(255.0*data[alpha_band][x+y*xres]+0.5);
	}
}


void doubleData::set_color(int x, int y, uchar r, uchar g, uchar b, uchar a)
{
if(check_bounds(x,y) || map) return;
if(red_band<bands && red_band>=0) data[red_band][x+y*xres]=(double)r*0.0039215686;
if(green_band<bands && green_band>=0) data[green_band][x+y*xres]=(double)g*0.0039215686;
if(blue_band<bands && blue_band>=0) data[blue_band][x+y*xres]=(double)b*0.0039215686;
if(alpha_band>=0 && alpha_band<bands) data[alpha_band][x+y*xres]=(double)a*0.0039215686;
}


uchar doubleData::get_alpha(int x, int y)
{
if(check_bounds_and_apply_wrap(x,y) || alpha_band>=bands) return 0;
if(alpha_band<0) return default_alpha;
return (uchar)(255.0*data[alpha_band][x+y*xres]+0.5);
}


void doubleData::set_alpha(int x, int y, uchar a)
{
if(check_bounds(x,y) || alpha_band<0 || alpha_band>=bands) return;
data[alpha_band][x+y*xres]=(double)a*0.0039215686;
}


ushort doubleData::get_red16(int x, int y)
{
if(map) {
	if(check_bounds_and_apply_wrap(x,y) || default_band>=bands || default_band<0) return 0;
	return (map->get_data())->get_red16((int)data[default_band][x+y*xres]);
	}
if(check_bounds_and_apply_wrap(x,y) || red_band>=bands || red_band<0) return 0;
return (ushort)(65535.0*data[red_band][x+y*xres]+0.5);
}


ushort doubleData::get_green16(int x, int y)
{
if(map) {
	if(check_bounds_and_apply_wrap(x,y) || default_band>=bands || default_band<0) return 0;
	return (map->get_data())->get_green16((int)data[default_band][x+y*xres]);
	}
if(check_bounds_and_apply_wrap(x,y) || green_band>=bands || green_band<0) return 0;
return (ushort)(65535.0*data[green_band][x+y*xres]+0.5);
}


ushort doubleData::get_blue16(int x, int y)
{
if(map) {
	if(check_bounds_and_apply_wrap(x,y) || default_band>=bands || default_band<0) return 0;
	return (map->get_data())->get_blue16((int)data[default_band][x+y*xres]);
	}
if(check_bounds_and_apply_wrap(x,y) || blue_band>=bands || blue_band<0) return 0;
return (ushort)(65535.0*data[blue_band][x+y*xres]+0.5);
}


void doubleData::set_red16(ushort c, int x, int y)
{
if(check_bounds(x,y) || red_band>=bands || red_band<0 || map) return;
data[red_band][x+y*xres]=(double)c*0.00001525902;
}


void doubleData::set_green16(ushort c, int x, int y)
{
if(check_bounds(x,y) || green_band>=bands || green_band<0 || map) return;
data[green_band][x+y*xres]=(double)c*0.00001525902;
}


void doubleData::set_blue16(ushort c, int x, int y)
{
if(check_bounds(x,y) || blue_band>=bands || blue_band<0 || map) return;
data[blue_band][x+y*xres]=(double)c*0.00001525902;
}


int doubleData::get_bit(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return 0;
return(data[b][x+y*xres]!=0.0);
}


char doubleData::get_char(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return 0;
return (char)data[b][x+y*xres];
}


uchar doubleData::get_uchar(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return 0;
return (uchar)data[b][x+y*xres];
}


short doubleData::get_short(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return 0;
return (short)data[b][x+y*xres];
}


ushort doubleData::get_ushort(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return 0;
return (ushort)data[b][x+y*xres];
}


long doubleData::get_long(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return 0;
return (long)data[b][x+y*xres];
}


ulong doubleData::get_ulong(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return 0;
return (ulong)data[b][x+y*xres];
}


float doubleData::get_float(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return 0.0;
return (float)data[b][x+y*xres];
}


double doubleData::get_double(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return 0.0;
return data[b][x+y*xres];
}


COMPLEX_TYPE doubleData::get_complex(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return complex_zero;
return (COMPLEX_TYPE)data[b][x+y*xres];
}


void doubleData::set_bit(int v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
data[b][x+y*xres]=(double)(v&1);
}


void doubleData::set_char(char v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
data[b][x+y*xres]=(double)v;
}


void doubleData::set_uchar(uchar v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
data[b][x+y*xres]=(double)v;
}


void doubleData::set_short(short v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
data[b][x+y*xres]=(double)v;
}


void doubleData::set_ushort(ushort v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
data[b][x+y*xres]=(double)v;
}


void doubleData::set_long(long v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
data[b][x+y*xres]=(double)v;
}


void doubleData::set_ulong(ulong v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
data[b][x+y*xres]=(double)v;
}


void doubleData::set_float(float v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
data[b][x+y*xres]=(double)v;
}


void doubleData::set_double(double v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
data[b][x+y*xres]=v;
}


void doubleData::set_complex(COMPLEX_TYPE v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
data[b][x+y*xres]=real(v);
}


// ****************************** complexData *******************************

void complexData::init()
{
data=NULL;
}


int complexData::allocate(int x, int y, int b)
{
int	i;

free();

if(x<0 || y<0 || b<0) return 0;

xres=x; yres=y; bands=b;

data=(COMPLEX_TYPE **)malloc(bands*sizeof(COMPLEX_TYPE *));
if(!data) return 0;
for(i=0;i<bands;i++) {
	data[i]=(COMPLEX_TYPE *)malloc(xres*yres*sizeof(COMPLEX_TYPE));
	if(!data[i]) return 0;
	}
return 1;
}


void complexData::free()
{
int	i;

if(data) {
	for(i=0;i<bands;i++) ::free((char *)data[i]);
	::free((char *)data);
	data=NULL;
	}
xres=0; yres=0; bands=0;
}


int complexData::add_band()
{
if(!data) return 0;

bands++;
data=(COMPLEX_TYPE **)realloc(data,bands*sizeof(COMPLEX_TYPE *));
if(!data) return 0;
data[bands-1]=(COMPLEX_TYPE *)malloc(xres*yres*sizeof(COMPLEX_TYPE));
if(!data[bands-1]) return 0;
return 1;
}


int complexData::remove_band(int b)
{
int	i;

if(!data || check_band(b)) return 0;

bands--;

::free((char *)data[b]);
for(i=b;i<bands;i++) data[i]=data[i+1];
data=(COMPLEX_TYPE **)realloc(data,bands*sizeof(COMPLEX_TYPE *));
if(!data) return 0;
return 1;
}


COMPLEX_TYPE *complexData::get_data(int b)
{
if(check_band(b) || !data) return NULL;
return data[b];
}


void complexData::get_color(int x, int y, uchar *r, uchar *g, uchar *b, uchar *a)
{
if (check_bounds_and_apply_wrap( x, y, r, g, b, a))
	return;

if(map) {
	if(default_band>=bands || default_band<0) { *r=0; *g=0; *b=0; }
	else	(map->get_data())->get_color((int)real(data[default_band][x+y*xres]),0,r,g,b);
	}
else {	if(red_band>=bands || red_band<0) *r=0;
	else	*r=(uchar)(255.0*real(data[red_band][x+y*xres])+0.5);
	if(green_band>=bands || green_band<0) *g=0;
	else	*g=(uchar)(255.0*real(data[green_band][x+y*xres])+0.5);
	if(blue_band>=bands || blue_band<0) *b=0;
	else	*b=(uchar)(255.0*real(data[blue_band][x+y*xres])+0.5);
	}
if(a) {	if(alpha_band<0) *a=default_alpha;
	else	if(alpha_band>=bands) *a=0;
	else	*a=(uchar)(255.0*real(data[alpha_band][x+y*xres])+0.5);
	}
}


void complexData::set_color(int x, int y, uchar r, uchar g, uchar b, uchar a)
{
if(check_bounds(x,y) || map) return;
if(red_band<bands && red_band>=0) data[red_band][x+y*xres]=(double)r*0.0039215686;
if(green_band<bands && green_band>=0) data[green_band][x+y*xres]=(double)g*0.0039215686;
if(blue_band<bands && blue_band>=0) data[blue_band][x+y*xres]=(double)b*0.0039215686;
if(alpha_band>=0 && alpha_band<bands) data[alpha_band][x+y*xres]=(double)a*0.0039215686;
}


uchar complexData::get_alpha(int x, int y)
{
if(check_bounds_and_apply_wrap(x,y) || alpha_band>=bands) return 0;
if(alpha_band<0) return default_alpha;
return (uchar)(255.0*real(data[alpha_band][x+y*xres])+0.5);
}


void complexData::set_alpha(int x, int y, uchar a)
{
if(check_bounds(x,y) || alpha_band<0 || alpha_band>=bands) return;
data[alpha_band][x+y*xres]=(double)a*0.0039215686;
}


ushort complexData::get_red16(int x, int y)
{
if(map) {
	if(check_bounds_and_apply_wrap(x,y) || default_band>=bands || default_band<0) return 0;
	return (map->get_data())->get_red16((int)real(data[default_band][x+y*xres]));
	}
if(check_bounds_and_apply_wrap(x,y) || red_band>=bands || red_band<0) return 0;
return (ushort)(65535.0*real(data[red_band][x+y*xres])+0.5);
}


ushort complexData::get_green16(int x, int y)
{
if(map) {
	if(check_bounds_and_apply_wrap(x,y) || default_band>=bands || default_band<0) return 0;
	return (map->get_data())->get_green16((int)real(data[default_band][x+y*xres]));
	}
if(check_bounds_and_apply_wrap(x,y) || green_band>=bands || green_band<0) return 0;
return (ushort)(65535.0*real(data[green_band][x+y*xres])+0.5);
}


ushort complexData::get_blue16(int x, int y)
{
if(map) {
	if(check_bounds_and_apply_wrap(x,y) || default_band>=bands || default_band<0) return 0;
	return (map->get_data())->get_blue16((int)real(data[default_band][x+y*xres]));
	}
if(check_bounds_and_apply_wrap(x,y) || blue_band>=bands || blue_band<0) return 0;
return (ushort)(65535.0*real(data[blue_band][x+y*xres])+0.5);
}


void complexData::set_red16(ushort c, int x, int y)
{
if(check_bounds(x,y) || red_band>=bands || red_band<0 || map) return;
data[red_band][x+y*xres]=(double)c*0.00001525902;
}


void complexData::set_green16(ushort c, int x, int y)
{
if(check_bounds(x,y) || green_band>=bands || green_band<0 || map) return;
data[green_band][x+y*xres]=(double)c*0.00001525902;
}


void complexData::set_blue16(ushort c, int x, int y)
{
if(check_bounds(x,y) || blue_band>=bands || blue_band<0 || map) return;
data[blue_band][x+y*xres]=(double)c*0.00001525902;
}


int complexData::get_bit(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return 0;
return(data[b][x+y*xres]!=complex_zero);
}


char complexData::get_char(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return 0;
return (char)real(data[b][x+y*xres]);
}


uchar complexData::get_uchar(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return 0;
return (uchar)real(data[b][x+y*xres]);
}


short complexData::get_short(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return 0;
return (short)real(data[b][x+y*xres]);
}


ushort complexData::get_ushort(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return 0;
return (ushort)real(data[b][x+y*xres]);
}


long complexData::get_long(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return 0;
return (long)real(data[b][x+y*xres]);
}


ulong complexData::get_ulong(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return 0;
return (ulong)real(data[b][x+y*xres]);
}


float complexData::get_float(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return 0.0;
return (float)real(data[b][x+y*xres]);
}


double complexData::get_double(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return 0.0;
return real(data[b][x+y*xres]);
}


COMPLEX_TYPE complexData::get_complex(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return complex_zero;
return data[b][x+y*xres];
}


void complexData::set_bit(int v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
data[b][x+y*xres]=(double)(v&1);
}


void complexData::set_char(char v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
data[b][x+y*xres]=(double)v;
}


void complexData::set_uchar(uchar v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
data[b][x+y*xres]=(double)v;
}


void complexData::set_short(short v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
data[b][x+y*xres]=(double)v;
}


void complexData::set_ushort(ushort v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
data[b][x+y*xres]=(double)v;
}


void complexData::set_long(long v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
data[b][x+y*xres]=(double)v;
}


void complexData::set_ulong(ulong v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
data[b][x+y*xres]=(double)v;
}


void complexData::set_float(float v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
data[b][x+y*xres]=(double)v;
}


void complexData::set_double(double v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
data[b][x+y*xres]=v;
}


void complexData::set_complex(COMPLEX_TYPE v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
data[b][x+y*xres]=v;
}


// ****************************** RLEData *******************************
// RLEData is an image data class for Run Length Encoded images.
// It is being implemented on a line compressed basis such that
// each line of each band is separately malloced and referenced.
// Each line consists of n pairs of (long length, long pixel)
// where the sum of length values for a line equals xres.  the
// pixel value may be an intensity or a color map reference.

void RLEData::init()
{
data=NULL;
line_lengths = NULL;
curr_line = 0;
curr_posn = NULL;
curr_len = 0;
}


int RLEData::allocate(int x, int y, int b)
{			// allocates pointer structures but not data space
int	i;

free();
curr_line = 0;
curr_posn = NULL;
curr_len = 0;

if(x<0 || y<0 || b<0) return 0;

xres=x; yres=y; bands=b;

data=(long ***)malloc(bands*sizeof(long **));
if(!data) return 0;
line_lengths = (long **)malloc(bands*sizeof(long *));
if(!line_lengths) return 0;
for(i=0;i<bands;i++) {
	data[i]=(long **)calloc(yres, sizeof(long *));
	if(!data[i]) return 0;
	line_lengths[i]=(long *)calloc(yres, sizeof(long));
	if(!line_lengths[i]) return 0;
	}
return 1;
}


void RLEData::free()
{
int	i,j;

if(data) {
	for(i=0;i<bands;i++) {
		for(j=0; j<yres; j++) {
			::free((char *)(data[i][j]));
		}
		::free((char *)data[i]);
		::free((char *)line_lengths[i]);
	}
	::free((char *)data);
	::free((char *)line_lengths);
	data=NULL;
	line_lengths=NULL;
	}
xres=0; yres=0; bands=0;
}


int RLEData::add_band()
{
if(!data) return 0;

bands++;
data=(long ***)realloc(data,bands*sizeof(long **));
if(!data) return 0;
data[bands-1]=(long **)calloc(yres, sizeof(long *));
if(!data[bands-1]) return 0;

line_lengths=(long **)realloc(line_lengths,bands*sizeof(long *));
if(!line_lengths) return 0;
line_lengths[bands-1]=(long *)calloc(yres, sizeof(long));
if(!line_lengths[bands-1]) return 0;
return 1;
}


int RLEData::remove_band(int b)
{
int	i;

if(!data || check_band(b)) return 0;

bands--;

for(i=0; i<yres; i++) {
	::free((char *)data[b][i]);
}
::free((char *)data[b]);
for(i=b;i<bands;i++) data[i]=data[i+1];
data=(long ***)realloc(data,bands*sizeof(long **));
if(!data) return 0;

::free((char *)line_lengths[b]);
for(i=b;i<bands;i++) line_lengths[i]=line_lengths[i+1];
line_lengths=(long **)realloc(line_lengths,bands*sizeof(long *));
if(!line_lengths) return 0;
return 1;
}


long **RLEData::get_data(int b)
{
if(check_band(b) || !data) return NULL;
return data[b];
}


void RLEData::get_color(int x, int y, uchar *r, uchar *g, uchar *b, uchar *a)
{
if (check_bounds_and_apply_wrap( x, y, r, g, b, a))
	return;

if(map) {
	if(default_band>=bands || default_band<0) { *r=0; *g=0; *b=0; }
	else {
		(map->get_data())->get_color((int)get_long(x,y,default_band), 0, r,g,b,a);
	}
	}
else {	if(red_band>=bands || red_band<0) *r=0;
	else {
		*r = (uchar)get_long(x,y,red_band);
	}
	if(green_band>=bands || green_band<0) *g=0;
	else {
		*g = (uchar)get_long(x,y,green_band);
	}
	if(blue_band>=bands || blue_band<0) *b=0;
	else {
		*b = (uchar)get_long(x,y,blue_band);
	}
	}
if(a) {	if(alpha_band<0) *a=default_alpha;
	else	if(alpha_band>=bands) *a=0;
	else {
		*a = (uchar)get_long(x,y,alpha_band);
	}
	}
}

void RLEData::get_color_row(int x, int y, int length, uchar *r, uchar *g, uchar *b, uchar *a)
{
int	i=0, lx, tx, map_w, map_h;
long	*index;

// if x is off left of image and no wrap, fill leading portion with 0
// if length is not long enough to reach the edge then return
if ( x < 0 ) {
	if(flgWrapState != IMAGE_WRAP_STATE_X && flgWrapState != IMAGE_WRAP_STATE_XY) {
		for(i=0; i<length && x < 0; i++) { 
			*(r++) = 0; *(g++) = 0; *(b++) = 0; 
			if(a) *(a++) = 0;
			x++;
		}
		if(x < 0) return;
	}
}

// check for valid data.  If not,then fill with 0 and return
index = data[red_band][y];

if (!index) {
	for(i=0; i<length; i++) { *(r++) = 0; *(g++) = 0; *(b++) = 0; }
	if(a) for(i=0; i<length; i++) { *(a++) = 0; }
	return;
}

// check for valid xy.  If not,then fill with 0 and return
if (check_bounds_and_apply_wrap( x, y, r, g, b, a)) {
	for(i=0; i<length; i++) { *(r++) = 0; *(g++) = 0; *(b++) = 0; }
	if(a) for(i=0; i<length; i++) { *(a++) = 0; }
	return;
}

// if color map which is typical of RLE images
if(map) {
	if(default_band>=bands || default_band<0) for(i=0; i<length; i++) { *(r++) = 0; *(g++) = 0; *(b++) = 0; }
	else {
		// fetch the entire colormap into arrays
		(map->get_data())->get_res(&map_w, &map_h);
		uchar *red_map = (uchar *)malloc(map_w * sizeof(uchar));
		uchar *grn_map = (uchar *)malloc(map_w * sizeof(uchar));
		uchar *blu_map = (uchar *)malloc(map_w * sizeof(uchar));
		uchar *alp_map = (uchar *)malloc(map_w * sizeof(uchar));
		(map->get_data())->get_color_row(0, 0, map_w, red_map, grn_map, blu_map, alp_map);

		// find the run-length-encoded block within which x falls
		// data is stored as long (length,value) pairs
		tx = x;
		tx -= (int)(*index);	// subtract the first length
		while(tx >= 0) tx -= (int)(*(index += 2));  // keep subtracting lengths
		// negate the value to find how many values are left in last block 
		tx = -tx;

		
		lx = x;
		for(; i<length; i++) {
			if(lx >= xres) {
				if(flgWrapState == IMAGE_WRAP_STATE_X || flgWrapState == IMAGE_WRAP_STATE_XY) {
					index = data[red_band][y];
					tx = (int)(*index);
					lx = 0;
				} else {
					for(; i<length; i++) { *(r++) = 0; }
					break;
				}
			}
			if(tx <= 0) {
				tx -= (int)(*(index += 2));
				tx = -tx;
			}
			*(r++) = *(red_map + *(index+1));
			*(g++) = *(grn_map + *(index+1));
			*(b++) = *(blu_map + *(index+1));
			if(a) *(a++) = *(alp_map + *(index+1));
			lx++;
			tx--;
		}

		// free the temp colormap arrays
		::free(red_map);
		::free(grn_map);
		::free(blu_map);
		::free(alp_map);
	}
	}
else {	if(red_band>=bands || red_band<0) for(i=0; i<length; i++) { *(r++) = 0; }
	else {
		*r = (uchar)get_long(x,y,red_band);
	}
	if(green_band>=bands || green_band<0) for(i=0; i<length; i++) { *(g++) = 0; }
	else {
		*g = (uchar)get_long(x,y,green_band);
	}
	if(blue_band>=bands || blue_band<0) for(i=0; i<length; i++) { *(b++) = 0; }
	else {
		*b = (uchar)get_long(x,y,blue_band);
	}
	}
if(a) {	if(alpha_band<0) *a=default_alpha;
	else	if(alpha_band>=bands) *a=0;
	else {
		*a = (uchar)get_long(x,y,alpha_band);
	}
	}
}


void RLEData::iget_color(double x, double y, uchar *r, uchar *g, uchar *b, uchar *a)
{
double	fx,fy,fx1,fy1,m1,m2,m3,m4;
int	ix,iy;
long	pix_val;
uchar	r1,r2,r3,r4,g1,g2,g3,g4,b1,b2,b3,b4;

if(x<0.0 || y<0.0 || x>=(double)(xres-1) || y>=(double)(yres-1)) { *r=0; *g=0; *b=0; if(a) *a=0; return; }
ix=(int)x; fx=x-ix; fx1=1.0-fx;
iy=(int)y; fy=y-iy; fy1=1.0-fy;
m1=fx1*fy1; m2=fx*fy1; m3=fx1*fy; m4=fx*fy;


if(map) {
	if(default_band>=bands || default_band<0) { 
		*r=0; *g=0; *b=0; 
	} else {
		pix_val = get_long(ix, iy, default_band);
		(map->get_data())->get_color((int)pix_val,0,&r1,&g1,&b1);
		pix_val = get_long(ix+1, iy, default_band);
		(map->get_data())->get_color((int)pix_val,0,&r2,&g2,&b2);
		pix_val = get_long(ix, iy+1, default_band);
		(map->get_data())->get_color((int)pix_val,0,&r3,&g3,&b3);
		pix_val = get_long(ix+1, iy+1, default_band);
		(map->get_data())->get_color((int)pix_val,0,&r4,&g4,&b4);
		*r=(uchar)(r1*m1+r2*m2+r3*m3+r4*m4);
		*g=(uchar)(g1*m1+g2*m2+g3*m3+g4*m4);
		*b=(uchar)(b1*m1+b2*m2+b3*m3+b4*m4);
	}
} else {
	if(red_band>=bands || red_band<0) *r=0;
	else {	
		r1 = get_uchar(ix, iy, red_band);
		r2 = get_uchar(ix+1, iy, red_band);
		r3 = get_uchar(ix, iy+1, red_band);
		r4 = get_uchar(ix+1, iy+1, red_band);
		*r=(uchar)(r1*m1+r2*m2+r3*m3+r4*m4); 
	}
	if(green_band>=bands || green_band<0) *g=0;
	else {
		r1 = get_uchar(ix, iy, green_band);
		r2 = get_uchar(ix+1, iy, green_band);
		r3 = get_uchar(ix, iy+1, green_band);
		r4 = get_uchar(ix+1, iy+1, green_band);
		*g=(uchar)(r1*m1+r2*m2+r3*m3+r4*m4); 
	}
	if(blue_band>=bands || blue_band<0) *b=0;
	else {	
		r1 = get_uchar(ix, iy, blue_band);
		r2 = get_uchar(ix+1, iy, blue_band);
		r3 = get_uchar(ix, iy+1, blue_band);
		r4 = get_uchar(ix+1, iy+1, blue_band);
		*b=(uchar)(r1*m1+r2*m2+r3*m3+r4*m4); 
	}
}

if(a) {	
	if(alpha_band<0) *a=default_alpha;
	else	if(alpha_band>=bands) *a=0;
		else {
			r1 = get_uchar(ix, iy, alpha_band);
			r2 = get_uchar(ix+1, iy, alpha_band);
			r3 = get_uchar(ix, iy+1, alpha_band);
			r4 = get_uchar(ix+1, iy+1, alpha_band);
			*a=(uchar)(r1*m1+r2*m2+r3*m3+r4*m4); 
		}
	}
}


void RLEData::set_color(int x, int y, uchar r, uchar g, uchar b, uchar a)
{
if(check_bounds(x,y) || map) return;
if(red_band<bands && red_band>=0) set_long((long)r, x, y, red_band);
if(green_band<bands && green_band>=0) set_long((long)g, x, y, green_band);
if(blue_band<bands && blue_band>=0) set_long((long)b, x, y, blue_band);
if(alpha_band>=0 && alpha_band<bands) set_long((long)a, x, y, alpha_band);
}


uchar RLEData::get_alpha(int x, int y)
{
if(check_bounds_and_apply_wrap(x,y) || alpha_band>=bands) return 0;
if(alpha_band<0) return default_alpha;
return (uchar)get_long(x,y,alpha_band);
}


void RLEData::set_alpha(int x, int y, uchar a)
{
if(check_bounds(x,y) || alpha_band<0 || alpha_band>=bands) return;
set_long((long)a, x, y, alpha_band);
}


ushort RLEData::get_red16(int x, int y)
{
if(map) {
	if(check_bounds_and_apply_wrap(x,y) || default_band>=bands || default_band<0) return 0;
	return (map->get_data())->get_red16((long)data[default_band][x+y*xres]);
	}
if(check_bounds_and_apply_wrap(x,y) || red_band>=bands || red_band<0) return 0;
return ((ushort)get_long(x, y, red_band));
}


ushort RLEData::get_green16(int x, int y)
{
if(map) {
	if(check_bounds_and_apply_wrap(x,y) || default_band>=bands || default_band<0) return 0;
	return (map->get_data())->get_green16((long)data[default_band][x+y*xres]);
	}
if(check_bounds_and_apply_wrap(x,y) || green_band>=bands || green_band<0) return 0;
return ((ushort)get_long(x, y, green_band));
}


ushort RLEData::get_blue16(int x, int y)
{
if(map) {
	if(check_bounds_and_apply_wrap(x,y) || default_band>=bands || default_band<0) return 0;
	return (map->get_data())->get_blue16((long)data[default_band][x+y*xres]);
	}
if(check_bounds_and_apply_wrap(x,y) || blue_band>=bands || blue_band<0) return 0;
return ((ushort)get_long(x, y, blue_band));
}


void RLEData::set_red16(ushort c, int x, int y)
{
if(check_bounds(x,y) || red_band>=bands || red_band<0 || map) return;
set_long((long)c, x, y, red_band);
}


void RLEData::set_green16(ushort c, int x, int y)
{
if(check_bounds(x,y) || green_band>=bands || green_band<0 || map) return;
set_long((long)c, x, y, green_band);
}


void RLEData::set_blue16(ushort c, int x, int y)
{
if(check_bounds(x,y) || blue_band>=bands || blue_band<0 || map) return;
set_long((long)c, x, y, blue_band);
}


int RLEData::get_bit(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return 0;
return (int)get_long(x,y,b)&1;
}


char RLEData::get_char(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return 0;
return (char)get_long(x,y,b);
}


uchar RLEData::get_uchar(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return 0;
return (uchar)get_long(x,y,b);
}


short RLEData::get_short(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return 0;
return (short)get_long(x,y,b);
}


ushort RLEData::get_ushort(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return 0;
return (ushort)get_long(x,y,b);
}


long RLEData::get_long(int x, int y, int b)
{
static	int	yl=-1, bl=-1;
static	long	value;
static	long	xmin=-2, xmax=-2;
long	*index;

if(b == bl && y == yl && x >= xmin && x <= xmax) return(value);
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return 0;

yl = y;
bl = b;
xmin = x;

index = data[b][y];
if(!index) return((long)0);
x -= (int)(*index);
while(x >= 0) x -= (int)(*(index += 2));

xmin -= *index + x;
xmax = xmin + *index - 1;
value = *(index+1);

return value;
}


ulong RLEData::get_ulong(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return 0;
return (ulong)get_long(x,y,b);
}


float RLEData::get_float(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return 0.0;
return (float)get_long(x,y,b);
}


double RLEData::get_double(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return 0.0;
return (double)get_long(x,y,b);
}


COMPLEX_TYPE RLEData::get_complex(int x, int y, int b)
{
if(check_bounds_and_apply_wrap(x,y) || check_band(b)) return complex_zero;
return (COMPLEX_TYPE)((double)get_long(x,y,b));
}


void RLEData::set_bit(int v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
if(v&1) set_long((long)255, x, y, b);
else	set_long((long)0, x, y, b);
}


void RLEData::set_char(char v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
set_long((long)v, x, y, b);
}


void RLEData::set_uchar(uchar v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
set_long((long)v, x, y, b);
}


void RLEData::set_short(short v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
set_long((long)v, x, y, b);
}


void RLEData::set_ushort(ushort v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
set_long((long)v, x, y, b);
}


void RLEData::set_long(long v, int x, int y, int b)
{
long	*index, *tindex, *t2index, *t3index;
long	posn;

if(check_bounds(x,y) || check_band(b)) return;
if(!data[b][y]) {		// no existing line yet
	line_lengths[b][y] = 2;
	index = (long *)malloc((unsigned int)line_lengths[b][y] * sizeof (long));
	data[b][y] = index;
	*index++ = xres;
	*index++ = v;
} else {
	index = data[b][y];
	posn = *index;
	while(posn <= x) posn += *(index += 2);
	if( v != *(index+1)) {				// must insert new value
		if(*index == 1) {			// if length = 1 can just replace
			*(index+1) = v;
		} else {
			if( posn-1 == x) {			// insert at end of block
				if(v == *(index+3) && posn < xres) {  // maybe matches next block
					(*index)--;
					(*(index+2))++;
				} else {
					t2index = (long *)malloc((unsigned int)(line_lengths[b][y] + 2) * sizeof (long));
					line_lengths[b][y] += 2;
					tindex = data[b][y];
					t3index = t2index;
					while(tindex < index) *t3index++ = *tindex++;
					*t3index++ = *tindex++ - 1;
					*t3index++ = *tindex++;
					*t3index++ = 1;
					*t3index++ = v;
					while(posn < xres) {
						posn += *tindex;
				 		*t3index++ = *tindex++;
				 		*t3index++ = *tindex++;
					}
					::free(data[b][y]);
					data[b][y] = t2index;
				}
			} else if(posn - *index == x) {		// insert at beginning of block
				if(v == *(index-1) && index > data[b][y]) {  // maybe matches previous block
					(*index)--;
					(*(index-2))++;
				} else {
					t2index = (long *)malloc((unsigned int)(line_lengths[b][y] + 2) * sizeof (long));
					line_lengths[b][y] += 2;
					tindex = data[b][y];
					t3index = t2index;
					while(tindex < index) *t3index++ = *tindex++;
					*t3index++ = 1;
					*t3index++ = v;
					*t3index++ = *tindex++ - 1;
					*t3index++ = *tindex++;
					while(posn < xres) {
						posn += *tindex;
				 		*t3index++ = *tindex++;
				 		*t3index++ = *tindex++;
					}
					::free(data[b][y]);
					data[b][y] = t2index;
				}
			} else {				// insert in middle of block
				t2index = (long *)malloc((unsigned int)(line_lengths[b][y] + 4) * sizeof (long));
				line_lengths[b][y] += 4;
				tindex = data[b][y];
				t3index = t2index;
				while(tindex < index) *t3index++ = *tindex++;
				*t3index++ = *tindex++ - (posn - x);
				*t3index++ = *tindex;
				*t3index++ = 1;
				*t3index++ = v;
				*t3index++ = posn - x - 1;
				*t3index++ = *tindex++;
				while(posn < xres) {
					posn += *tindex;
				 	*t3index++ = *tindex++;
				 	*t3index++ = *tindex++;
				}
				::free(data[b][y]);
				data[b][y] = t2index;
			}
		}
	}
}
}


void RLEData::set_ulong(ulong v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
set_long((long)v, x, y, b);
}


void RLEData::set_float(float v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
set_long((long)v, x, y, b);
}


void RLEData::set_double(double v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
set_long((long)v, x, y, b);
}


void RLEData::set_complex(COMPLEX_TYPE v, int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return;
set_long((long)real(v), x, y, b);
}

void RLEData::line_append(long v, long length, int y, int b)
{
	long	*index;
	if(check_bounds(0,y) || check_band(b)) return;
	if(!data) return;
	if(!data[b][y]) {
		line_lengths[b][y] = 2;
		index = (long *)malloc((int)line_lengths[b][y] * sizeof(long));
		data[b][y] = index;
		*index++ = length;
		*index++ = v;
	} else {
		line_lengths[b][y] += 2;
		index = (long *)realloc(data[b][y], (int)line_lengths[b][y] * sizeof(long));
		data[b][y] = index;
		*(index + line_lengths[b][y] - 2) = length;
		*(index + line_lengths[b][y] - 1) = v;
	}
}

void RLEData::band_append(long, long, int b)
{

	if(check_band(b)) return;
	if(!data) return;
	if(!data[b][curr_line]) {
	}
}

long *RLEData::get_line(int y, int b)
{
	if(check_bounds(0,y) || check_band(b)) return(NULL);
	if(!data) return(NULL);
	return(data[b][y]);
}

#ifndef _NO_IMAGEDISP_  // compile only if X windows is present!

// ****************************** ximageData *******************************

void ximageData::init(void)
{
	display = NULL;
	visual = NULL;
	pixmap = (Pixmap)NULL;
	depth = 0;
	xres = 0;
	yres = 0;


	ximg = NULL;				// this actually gets allocated within the class
}

void ximageData::set_pixmap( Pixmap pm)
{

	int	rxr,ryr;
	unsigned int	dw,dh,bwr,dr;
	Window	rr;

	pixmap = pm;

	if(!XGetGeometry(display, (Drawable)pm,&rr,&rxr,&ryr,&dw,&dh,&bwr,&dr)) {
		fprintf( stderr, "ximageData::set_pixmap(): XGetGeometry() failed");
		return;
	}
	else {
		fprintf( stderr, "pixmap geometry is (%d,%d), depth=%d\n", dw, dh, dr);
	}

	xres = dw;
	yres = dh;
	
	copy_pixmap_to_ximage( 0, 0, 0, 0, xres, yres);
}

int ximageData::allocate(int, int, int)
{
	return 0;
}
							 
XImage  *ximageData::get_data(int )
{
	return ximg;
}

void ximageData::get_color(int x, int y, uchar *r, uchar *g, uchar *b, uchar *a)
{
								// get pixel from ximage
	unsigned long pix;

	if (check_bounds_and_apply_wrap( x, y, r, g, b, a))
		return;
	
	
	pix = XGetPixel( ximg, x, y);

	*r = (unsigned char)(pix & 0x000000ff);
	*g = (unsigned char)((pix & 0x0000ff00) >> 8);
	*b = (unsigned char)((pix & 0x00ff0000) >> 16);
	if (a)
		*a = (unsigned char)((pix & 0xff000000) >> 24);
}

void	ximageData::set_color(int, int, uchar, uchar, uchar, uchar)
{
}


uchar	ximageData::get_alpha(int x, int y)
{

	uchar r, g, b, a;

	get_color( x, y, &r, &g, &b, &a);

	return a;
}


void	ximageData::set_alpha(int, int, uchar)
{
	;
}



//void ximageData::copy_pixmap_to_ximage( int xx, int xy, int ix, int iy, int w, int h)
void ximageData::copy_pixmap_to_ximage( int, int, int, int, int, int)
//
// xx,xy: exclude this much data in x and in y (???)
// ix,iy: starting x and y offsets in pixmap
//
{

	if (!depth || !display || !visual || !pixmap) {
		fprintf( stderr, "allocate_ximage: ptrs");
		return;
	}
	
	if (!xres || !yres) {
		fprintf( stderr, "allocate_ximage: x,y res");
		return;
	}

								// get the whole pixmap contents into ximage
	ximg = XGetImage( display, pixmap, 0, 0, xres, yres, AllPlanes, ZPixmap);

	if (!ximg || ximg->format!=ZPixmap || !ximg->data) {
		fprintf( stderr, "copy_pixmap_to_ximage: ptrs");
		return;
	}

			 
}

								// 
								// 
								// FYI : None of these are implemented yet -
								//   Sorry!
								// !!! NYI !!!
								// 

ushort	ximageData::get_red16(int, int) { return (ushort)0; }
ushort	ximageData::get_green16(int, int) { return (ushort)0; }
ushort	ximageData::get_blue16(int, int) { return (ushort)0; }
	//
void	ximageData::set_red16(ushort, int, int) { ; }
void	ximageData::set_green16(ushort, int, int) { ; }
void	ximageData::set_blue16(ushort, int, int) { ; }

	// Data types:

int	ximageData::get_bit(int, int, int) { return (int)0; }
char	ximageData::get_char(int, int, int) { return (char)0; }
uchar	ximageData::get_uchar(int, int, int) { return (uchar)0; }
short	ximageData::get_short(int, int, int) { return (short)0; }
ushort	ximageData::get_ushort(int, int, int) { return (ushort)0; }
long	ximageData::get_long(int, int, int) { return (long)0; }
ulong	ximageData::get_ulong(int, int, int) { return (ulong)0; }
float	ximageData::get_float(int, int, int) { return (float)0; }
double	ximageData::get_double(int, int, int) { return (double)0; }
COMPLEX_TYPE ximageData::get_complex(int, int, int) { return (COMPLEX_TYPE)0; }

void	ximageData::set_bit(int, int, int, int) { ; }
void	ximageData::set_char(char, int, int, int) { ; }
void	ximageData::set_uchar(uchar, int, int, int) { ; }
void	ximageData::set_short(short, int, int, int) { ; }
void	ximageData::set_ushort(ushort, int, int, int) { ; }
void	ximageData::set_long(long, int, int, int) { ; }
void	ximageData::set_ulong(ulong, int, int, int) { ; }
void	ximageData::set_float(float, int, int, int) { ; }
void	ximageData::set_double(double, int, int, int) { ; }
void	ximageData::set_complex(COMPLEX_TYPE, int, int, int) { ; }

								// 
								// end of NYI code
								// 


void ximageData::free( void)
{
	if (ximg) {
		XDestroyImage( ximg);
	}
}

ximageData::ximageData()
{
	init();
}

ximageData::~ximageData()
{
	if( clean) free();
}

#endif // _NO_IMAGEDISP_


// Convenience routines:   ************************************************

Image *color_image(int x, int y)
{
Image *i=new GeoImage(new ucharData(x,y,3));
i->clear();
return i;
}


Image *gray_image(int x, int y)
{
Image *i=new GeoImage(new ucharData(x,y));
i->clear();
return i;
}


Image *pseudocolor_image(int x, int y, int n)
{
Image *i=new GeoImage(new ucharData(x,y));
Image *m=new Image(new ucharData(n,1,3));
i->clear();
m->clear();
i->set_map(m);
return i;
}


Image *color_alpha_image(int x, int y)
{
Image *i=new GeoImage(new ucharData(x,y,4));
i->clear();
i->set_alpha_band(3);
return i;
}


Image *gray_alpha_image(int x, int y)
{
Image *i=new GeoImage(new ucharData(x,y,2));
i->clear();
i->set_alpha_band(1);
return i;
}


Image *pseudocolor_alpha_image(int x, int y, int n)
{
Image *i=new GeoImage(new ucharData(x,y,2));
Image *m=new Image(new ucharData(n,1,3));
i->clear();
m->clear();
i->set_map(m);
i->set_alpha_band(1);
return i;
}

#ifndef _NO_IMAGEDISP_
Image *pixmap_image( Pixmap pm, Display *pd, Visual *pv, int depth)
{
//Image *i=new GeoImage(new ximageData( pm, pd, pv, depth));

Image *i=new GeoImage(new ximageData( ));
ximageData *xid = (ximageData *)i->get_data();

xid->set_display( pd);
xid->set_visual( pv);
xid->set_depth( depth);
xid->set_pixmap( pm);

i->clear();
return i;
}
#endif // _NO_IMAGEDISP_

