// datammap.C
//
// Written by Dave Kagels 2/14/95

#ifndef _NO_MMAP_

#include "image/datammap.h"
#include <sys/types.h>
#include <sys/mman.h>
#include <unistd.h>


// ****************************** ucharMMap *******************************

void ucharMMap::free()
{
int	i;

if(!data) return;

if(base) munmap(
#ifdef SUN4
		(caddr_t)
#endif
#ifdef SOLARIS
		(char *)
#endif
			 rbase,tlen);
else	for(i=0;i<bands;i++) if(rdata[i]) munmap(
#ifdef SUN4
				(caddr_t)
#endif
#ifdef SOLARIS
				(char *)
#endif
					 rdata[i],xres*yres+(data[i]-rdata[i]));

::free((char *)data);
::free((char *)rdata);
data=NULL;
xres=0; yres=0; bands=0;
init();
}


int ucharMMap::mmap_setup(int a, int x, int y, int b)
{
int	i;

free();

if(x<0 || y<0 || b<0 || !(a&3)) return 0;

data_access=a;
xres=x; yres=y; bands=b;

#ifdef SUN4
page_size=4096;	// SHOULD CALL getpagesize(), but it doesn't seem to exist
#else
page_size=(int)sysconf(_SC_PAGESIZE);
#endif

data=(uchar **)malloc(bands*sizeof(uchar *));
if(!data) return 0;
rdata=(uchar **)malloc(bands*sizeof(uchar *));
if(!rdata) return 0;
for(i=0;i<bands;i++) { data[i]=NULL; rdata[i]=NULL; }

return 1;
}


int ucharMMap::mmap_band(int b, int fd, int off)
{
int	a,r;

if(!data || b<0 || b>=bands) return 0;
if(base || data[b]) return 0;

a=0;
if(data_access&DATA_READ) a=PROT_READ;
if(data_access&DATA_WRITE) a|=PROT_WRITE;

r=off%page_size; off-=r;

rdata[b]=(uchar *)mmap(NULL,xres*yres+r,a,MAP_SHARED,fd,off);
if(rdata[b]==(void *)-1) return 0;
data[b]=rdata[b]+r;
return 1;
}


int ucharMMap::mmap_data(int fd, int off, int len)
{
int	a,i,r;

if(!data || base) return 0;
for(i=0;i<bands;i++) if(data[i]) return 0;

a=0;
if(data_access&DATA_READ) a=PROT_READ;
if(data_access&DATA_WRITE) a|=PROT_WRITE;

tlen=len;
if(!tlen) tlen=xres*yres*bands;
if(tlen<xres*yres*bands) return 0;

r=off%page_size; off-=r; tlen+=r;

rbase=(uchar *)mmap(NULL,tlen,a,MAP_SHARED,fd,off);
if(rbase==(void *)-1) return 0;
base=rbase+r;

for(i=0;i<bands;i++) data[i]=base+i*xres*yres;
return 1;
}


int ucharMMap::set_off(int b, int off)
{
if(!data || b<0 || b>=bands || !base || off<0 || off>tlen-xres*yres) return 0;

data[b]=base+off;
return 1;
}


void ucharMMapFV::free()
{
int	i;

if(!data) return;

if(base) munmap(
#ifdef SUN4
		(caddr_t)
#endif
#ifdef SOLARIS
		(char *)
#endif
			 rbase,tlen);
else	for(i=0;i<bands;i++) if(rdata[i]) munmap(
#ifdef SUN4
				(caddr_t)
#endif
#ifdef SOLARIS
				(char *)
#endif
					 rdata[i],xres+(data[i]-rdata[i]));

::free((char *)data);
::free((char *)rdata);
data=NULL;
xres=0; yres=0; bands=0;
init();
}


int ucharMMapFV::mmap_band(int b, int fd, int off)
{
int	a,r;

if(!data || b<0 || b>=bands) return 0;
if(base || data[b]) return 0;

a=0;
if(data_access&DATA_READ) a=PROT_READ;
if(data_access&DATA_WRITE) a|=PROT_WRITE;

r=off%page_size; off-=r;

rdata[b]=(uchar *)mmap(NULL,xres*yres+r,a,MAP_SHARED,fd,off);
if(rdata[b]==(void *)-1) return 0;
data[b]=rdata[b]+r+xres*(yres-1);
return 1;
}


int ucharMMapFV::mmap_data(int fd, int off, int len)
{
int	a,i,r;

if(!data || base) return 0;
for(i=0;i<bands;i++) if(data[i]) return 0;

a=0;
if(data_access&DATA_READ) a=PROT_READ;
if(data_access&DATA_WRITE) a|=PROT_WRITE;

tlen=len;
if(!tlen) tlen=xres*yres*bands;
if(tlen<xres*yres*bands) return 0;

r=off%page_size; off-=r; tlen+=r;

rbase=(uchar *)mmap(NULL,tlen,a,MAP_SHARED,fd,off);
if(rbase==(void *)-1) return 0;
base=rbase+r;

for(i=0;i<bands;i++) data[i]=base+(i+1)*xres*yres-xres;
return 1;
}


int ucharMMapFV::set_off(int b, int off)
{
if(!data || b<0 || b>=bands || !base || off<0 || off>tlen-xres*yres) return 0;

data[b]=base+off+xres*(yres-1);
return 1;
}


void ucharMMapFV::get_color(int x, int y, uchar *r, uchar *g, uchar *b, uchar *a)
{
if(check_bounds(x,y)) { *r=0; *g=0; *b=0; if(a) *a=0; return; }
if(map) {
	if(default_band>=bands || default_band<0) { *r=0; *g=0; *b=0; }
	else	(map->get_data())->get_color((int)data[default_band][x-y*xres],0,r,g,b);
	}
else {	if(red_band>=bands || red_band<0) *r=0;
	else	*r=data[red_band][x-y*xres];
	if(green_band>=bands || green_band<0) *g=0;
	else	*g=data[green_band][x-y*xres];
	if(blue_band>=bands || blue_band<0) *b=0;
	else	*b=data[blue_band][x-y*xres];
	}
if(a) {	if(alpha_band<0) *a=default_alpha;
	else	if(alpha_band>=bands) *a=0;
	else	*a=data[alpha_band][x-y*xres];
	}
}


void ucharMMapFV::iget_color(double x, double y, uchar *r, uchar *g, uchar *b, uchar *a)
{
double	fx,fy,fx1,fy1,m1,m2,m3,m4;
int	ix,iy,off;
uchar	*ptr;

if(x<0.0 || y<0.0 || x>=(double)(xres-1) || y>=(double)(yres-1)) { *r=0; *g=0; *b=0; if(a) *a=0; return; }
ix=(int)x; fx=x-ix; fx1=1.0-fx;
iy=(int)y; fy=y-iy; fy1=1.0-fy;
m1=fx1*fy1; m2=fx*fy1; m3=fx1*fy; m4=fx*fy;

off=ix-iy*xres;

if(map) {
	uchar	r1,r2,r3,r4,g1,g2,g3,g4,b1,b2,b3,b4;
	if(default_band>=bands || default_band<0) { *r=0; *g=0; *b=0; }
	ptr=data[default_band]+off;
	(map->get_data())->get_color(*ptr,0,&r1,&g1,&b1);
	(map->get_data())->get_color(*(ptr+1),0,&r2,&g2,&b2);
	(map->get_data())->get_color(*(ptr-xres),0,&r3,&g3,&b3);
	(map->get_data())->get_color(*(ptr-xres+1),0,&r4,&g4,&b4);
	*r=(uchar)(r1*m1+r2*m2+r3*m3+r4*m4);
	*g=(uchar)(g1*m1+g2*m2+g3*m3+g4*m4);
	*b=(uchar)(b1*m1+b2*m2+b3*m3+b4*m4);
	}
else {
	if(red_band>=bands || red_band<0) *r=0;
	else {	ptr=data[red_band]+off;
		*r=(uchar)(*ptr*m1+*(ptr+1)*m2+*(ptr-xres)*m3+*(ptr-xres+1)*m4); }
	if(green_band>=bands || green_band<0) *g=0;
	else {	ptr=data[green_band]+off;
		*g=(uchar)(*ptr*m1+*(ptr+1)*m2+*(ptr-xres)*m3+*(ptr-xres+1)*m4); }
	if(blue_band>=bands || blue_band<0) *b=0;
	else {	ptr=data[blue_band]+off;
		*b=(uchar)(*ptr*m1+*(ptr+1)*m2+*(ptr-xres)*m3+*(ptr-xres+1)*m4); }
	}

if(a) {	if(alpha_band<0) *a=default_alpha;
	else	if(alpha_band>=bands) *a=0;
	else {	ptr=data[alpha_band]+off;
		*a=(uchar)(*ptr*m1+*(ptr+1)*m2+*(ptr-xres)*m3+*(ptr-xres+1)*m4); }
	}
}


uchar ucharMMapFV::get_uchar(int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return 0;

return data[b][x-y*xres];
}


long ucharMMapFV::get_long(int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return 0;

return (long)data[b][x-y*xres];
}


double ucharMMapFV::get_double(int x, int y, int b)
{
if(check_bounds(x,y) || check_band(b)) return 0.0;

return (double)data[b][x-y*xres];
}


// ****************************** shortMMap *******************************

void shortMMap::free()
{
int	i;

if(!data) return;

if(base) munmap(
#ifdef SUN4
		(caddr_t)
#endif
#ifdef SOLARIS
		(char *)
#endif
			 rbase,tlen);
else	for(i=0;i<bands;i++) if(rdata[i]) munmap(
#ifdef SUN4
				(caddr_t)
#endif
#ifdef SOLARIS
				(char *)
#endif
					 rdata[i],xres*yres*sizeof(short)+((char *)data[i]-(char *)rdata[i]));

::free((char *)data);
::free((char *)rdata);
data=NULL;
xres=0; yres=0; bands=0;
init();
}


int shortMMap::mmap_setup(int a, int x, int y, int b)
{
int	i;

free();

if(x<0 || y<0 || b<0 || !(a&3)) return 0;

data_access=a;
xres=x; yres=y; bands=b;

#ifdef SUN4
page_size=4096;	// SHOULD CALL getpagesize(), but it doesn't seem to exist
#else
page_size=(int)sysconf(_SC_PAGESIZE);
#endif

data=(short **)malloc(bands*sizeof(short *));
if(!data) return 0;
rdata=(short **)malloc(bands*sizeof(short *));
if(!rdata) return 0;
for(i=0;i<bands;i++) { data[i]=NULL; rdata[i]=NULL; }

return 1;
}


int shortMMap::mmap_band(int b, int fd, int off)
{
int	a,r;

if(!data || b<0 || b>=bands) return 0;
if(base || data[b]) return 0;

a=0;
if(data_access&DATA_READ) a=PROT_READ;
if(data_access&DATA_WRITE) a|=PROT_WRITE;

r=off%page_size; off-=r;

rdata[b]=(short *)mmap(NULL,xres*yres*sizeof(short)+r,a,MAP_SHARED,fd,off);
if(rdata[b]==(void *)-1) return 0;
data[b]=(short *)((char *)rdata[b]+r);
return 1;
}


int shortMMap::mmap_data(int fd, int off, int len)
{
int	a,i,r;

if(!data || base) return 0;
for(i=0;i<bands;i++) if(data[i]) return 0;

a=0;
if(data_access&DATA_READ) a=PROT_READ;
if(data_access&DATA_WRITE) a|=PROT_WRITE;

tlen=len;
if(!tlen) tlen=xres*yres*bands*sizeof(short);
if(tlen<xres*yres*bands*sizeof(short)) return 0;

r=off%page_size; off-=r; tlen+=r;

rbase=(short *)mmap(NULL,tlen,a,MAP_SHARED,fd,off);
if(rbase==(void *)-1) return 0;
base=(short *)((char *)rbase+r);

for(i=0;i<bands;i++) data[i]=base+i*xres*yres;
return 1;
}


int shortMMap::set_off(int b, int off)
{
if(!data || b<0 || b>=bands || !base || off<0 || off>tlen-xres*yres*sizeof(short)) return 0;

data[b]=(short *)((char *)base+off);
return 1;
}


short shortMMap::get_short(int x, int y, int b)
{
static short	temp;

if(check_bounds(x,y) || check_band(b)) return 0;

memcpy((char *)&temp,(char *)&data[b][x+y*xres],sizeof(short));
return temp;
}


double shortMMap::get_double(int x, int y, int b)
{
static short	temp;

if(check_bounds(x,y) || check_band(b)) return 0.0;

memcpy((char *)&temp,(char *)&data[b][x+y*xres],sizeof(short));
return (double)temp;
}


void shortMMapFV::free()
{
int	i;

if(!data) return;

if(base) munmap(
#ifdef SUN4
		(caddr_t)
#endif
#ifdef SOLARIS
		(char *)
#endif
			 rbase,tlen);
else	for(i=0;i<bands;i++) if(rdata[i]) munmap(
#ifdef SUN4
				(caddr_t)
#endif
#ifdef SOLARIS
				(char *)
#endif
					 rdata[i],xres*sizeof(short)+((char *)data[i]-(char *)rdata[i]));

::free((char *)data);
::free((char *)rdata);
data=NULL;
xres=0; yres=0; bands=0;
init();
}


int shortMMapFV::mmap_band(int b, int fd, int off)
{
int	a,r;

if(!data || b<0 || b>=bands) return 0;
if(base || data[b]) return 0;

a=0;
if(data_access&DATA_READ) a=PROT_READ;
if(data_access&DATA_WRITE) a|=PROT_WRITE;

r=off%page_size; off-=r;

rdata[b]=(short *)mmap(NULL,xres*yres*sizeof(short)+r,a,MAP_SHARED,fd,off);
if(rdata[b]==(void *)-1) return 0;
data[b]=(short *)((char *)rdata[b]+r)+xres*(yres-1);
return 1;
}


int shortMMapFV::mmap_data(int fd, int off, int len)
{
int	a,i,r;

if(!data || base) return 0;
for(i=0;i<bands;i++) if(data[i]) return 0;

a=0;
if(data_access&DATA_READ) a=PROT_READ;
if(data_access&DATA_WRITE) a|=PROT_WRITE;

tlen=len;
if(!tlen) tlen=xres*yres*bands*sizeof(short);
if(tlen<xres*yres*bands*sizeof(short)) return 0;

r=off%page_size; off-=r; tlen+=r;

rbase=(short *)mmap(NULL,tlen,a,MAP_SHARED,fd,off);
if(rbase==(void *)-1) return 0;
base=(short *)((char *)rbase+r);

for(i=0;i<bands;i++) data[i]=base+(i+1)*xres*yres-xres;
return 1;
}


int shortMMapFV::set_off(int b, int off)
{
if(!data || b<0 || b>=bands || !base || off<0 || off>tlen-xres*yres*sizeof(short)) return 0;

data[b]=(short *)((char *)base+off)+xres*(yres-1);
return 1;
}


short shortMMapFV::get_short(int x, int y, int b)
{
static short	temp;

if(check_bounds(x,y) || check_band(b)) return 0;

memcpy((char *)&temp,(char *)&data[b][x-y*xres],sizeof(short));
return temp;
}


double shortMMapFV::get_double(int x, int y, int b)
{
static short	temp;

if(check_bounds(x,y) || check_band(b)) return 0.0;

memcpy((char *)&temp,(char *)&data[b][x-y*xres],sizeof(short));
return (double)temp;
}


// ****************************** ushortMMap *******************************

void ushortMMap::free()
{
int	i;

if(!data) return;

if(base) munmap(
#ifdef SUN4
		(caddr_t)
#endif
#ifdef SOLARIS
		(char *)
#endif
			 rbase,tlen);
else	for(i=0;i<bands;i++) if(rdata[i]) munmap(
#ifdef SUN4
				(caddr_t)
#endif
#ifdef SOLARIS
				(char *)
#endif
					 rdata[i],xres*yres*sizeof(ushort)+((char *)data[i]-(char *)rdata[i]));

::free((char *)data);
::free((char *)rdata);
data=NULL;
xres=0; yres=0; bands=0;
init();
}


int ushortMMap::mmap_setup(int a, int x, int y, int b)
{
int	i;

free();

if(x<0 || y<0 || b<0 || !(a&3)) return 0;

data_access=a;
xres=x; yres=y; bands=b;

#ifdef SUN4
page_size=4096;	// SHOULD CALL getpagesize(), but it doesn't seem to exist
#else
page_size=(int)sysconf(_SC_PAGESIZE);
#endif

data=(ushort **)malloc(bands*sizeof(ushort *));
if(!data) return 0;
rdata=(ushort **)malloc(bands*sizeof(ushort *));
if(!rdata) return 0;
for(i=0;i<bands;i++) { data[i]=NULL; rdata[i]=NULL; }

return 1;
}


int ushortMMap::mmap_band(int b, int fd, int off)
{
int	a,r;

if(!data || b<0 || b>=bands) return 0;
if(base || data[b]) return 0;

a=0;
if(data_access&DATA_READ) a=PROT_READ;
if(data_access&DATA_WRITE) a|=PROT_WRITE;

r=off%page_size; off-=r;

rdata[b]=(ushort *)mmap(NULL,xres*yres*sizeof(ushort)+r,a,MAP_SHARED,fd,off);
if(rdata[b]==(void *)-1) return 0;
data[b]=(ushort *)((char *)rdata[b]+r);
return 1;
}


int ushortMMap::mmap_data(int fd, int off, int len)
{
int	a,i,r;

if(!data || base) return 0;
for(i=0;i<bands;i++) if(data[i]) return 0;

a=0;
if(data_access&DATA_READ) a=PROT_READ;
if(data_access&DATA_WRITE) a|=PROT_WRITE;

tlen=len;
if(!tlen) tlen=xres*yres*bands*sizeof(ushort);
if(tlen<xres*yres*bands*sizeof(ushort)) return 0;

r=off%page_size; off-=r; tlen+=r;

rbase=(ushort *)mmap(NULL,tlen,a,MAP_SHARED,fd,off);
if(rbase==(void *)-1) return 0;
base=(ushort *)((char *)rbase+r);

for(i=0;i<bands;i++) data[i]=base+i*xres*yres;
return 1;
}


int ushortMMap::set_off(int b, int off)
{
if(!data || b<0 || b>=bands || !base || off<0 || off>tlen-xres*yres*sizeof(ushort)) return 0;

data[b]=(ushort *)((char *)base+off);
return 1;
}


ushort ushortMMap::get_ushort(int x, int y, int b)
{
static ushort	temp;

if(check_bounds(x,y) || check_band(b)) return 0;

memcpy((char *)&temp,(char *)&data[b][x+y*xres],sizeof(ushort));
return temp;
}


double ushortMMap::get_double(int x, int y, int b)
{
static ushort	temp;

if(check_bounds(x,y) || check_band(b)) return 0.0;

memcpy((char *)&temp,(char *)&data[b][x+y*xres],sizeof(ushort));
return (double)temp;
}


void ushortMMapFV::free()
{
int	i;

if(!data) return;

if(base) munmap(
#ifdef SUN4
		(caddr_t)
#endif
#ifdef SOLARIS
		(char *)
#endif
			 rbase,tlen);
else	for(i=0;i<bands;i++) if(rdata[i]) munmap(
#ifdef SUN4
				(caddr_t)
#endif
#ifdef SOLARIS
				(char *)
#endif
					 rdata[i],xres*sizeof(ushort)+((char *)data[i]-(char *)rdata[i]));

::free((char *)data);
::free((char *)rdata);
data=NULL;
xres=0; yres=0; bands=0;
init();
}


int ushortMMapFV::mmap_band(int b, int fd, int off)
{
int	a,r;

if(!data || b<0 || b>=bands) return 0;
if(base || data[b]) return 0;

a=0;
if(data_access&DATA_READ) a=PROT_READ;
if(data_access&DATA_WRITE) a|=PROT_WRITE;

r=off%page_size; off-=r;

rdata[b]=(ushort *)mmap(NULL,xres*yres*sizeof(ushort)+r,a,MAP_SHARED,fd,off);
if(rdata[b]==(void *)-1) return 0;
data[b]=(ushort *)((char *)rdata[b]+r)+xres*(yres-1);
return 1;
}


int ushortMMapFV::mmap_data(int fd, int off, int len)
{
int	a,i,r;

if(!data || base) return 0;
for(i=0;i<bands;i++) if(data[i]) return 0;

a=0;
if(data_access&DATA_READ) a=PROT_READ;
if(data_access&DATA_WRITE) a|=PROT_WRITE;

tlen=len;
if(!tlen) tlen=xres*yres*bands*sizeof(ushort);
if(tlen<xres*yres*bands*sizeof(ushort)) return 0;

r=off%page_size; off-=r; tlen+=r;

rbase=(ushort *)mmap(NULL,tlen,a,MAP_SHARED,fd,off);
if(rbase==(void *)-1) return 0;
base=(ushort *)((char *)rbase+r);

for(i=0;i<bands;i++) data[i]=base+(i+1)*xres*yres-xres;
return 1;
}


int ushortMMapFV::set_off(int b, int off)
{
if(!data || b<0 || b>=bands || !base || off<0 || off>tlen-xres*yres*sizeof(ushort)) return 0;

data[b]=(ushort *)((char *)base+off)+xres*(yres-1);
return 1;
}


ushort ushortMMapFV::get_ushort(int x, int y, int b)
{
static ushort	temp;

if(check_bounds(x,y) || check_band(b)) return 0;

memcpy((char *)&temp,(char *)&data[b][x-y*xres],sizeof(ushort));
return temp;
}


double ushortMMapFV::get_double(int x, int y, int b)
{
static ushort	temp;

if(check_bounds(x,y) || check_band(b)) return 0.0;

memcpy((char *)&temp,(char *)&data[b][x-y*xres],sizeof(ushort));
return (double)temp;
}


// ****************************** floatMMap *******************************

void floatMMap::free()
{
int	i;

if(!data) return;

if(base) munmap(
#ifdef SUN4
		(caddr_t)
#endif
#ifdef SOLARIS
		(char *)
#endif
			 rbase,tlen);
else	for(i=0;i<bands;i++) if(rdata[i]) munmap(
#ifdef SUN4
				(caddr_t)
#endif
#ifdef SOLARIS
				(char *)
#endif
					 rdata[i],xres*yres*sizeof(float)+((char *)data[i]-(char *)rdata[i]));

::free((char *)data);
::free((char *)rdata);
data=NULL;
xres=0; yres=0; bands=0;
init();
}


int floatMMap::mmap_setup(int a, int x, int y, int b)
{
int	i;

free();

if(x<0 || y<0 || b<0 || !(a&3)) return 0;

data_access=a;
xres=x; yres=y; bands=b;

#ifdef SUN4
page_size=4096;	// SHOULD CALL getpagesize(), but it doesn't seem to exist
#else
page_size=(int)sysconf(_SC_PAGESIZE);
#endif

data=(float **)malloc(bands*sizeof(float *));
if(!data) return 0;
rdata=(float **)malloc(bands*sizeof(float *));
if(!rdata) return 0;
for(i=0;i<bands;i++) { data[i]=NULL; rdata[i]=NULL; }

return 1;
}


int floatMMap::mmap_band(int b, int fd, int off)
{
int	a,r;

if(!data || b<0 || b>=bands) return 0;
if(base || data[b]) return 0;

a=0;
if(data_access&DATA_READ) a=PROT_READ;
if(data_access&DATA_WRITE) a|=PROT_WRITE;

r=off%page_size; off-=r;

rdata[b]=(float *)mmap(NULL,xres*yres*sizeof(float)+r,a,MAP_SHARED,fd,off);
if(rdata[b]==(void *)-1) return 0;
data[b]=(float *)((char *)rdata[b]+r);
return 1;
}


int floatMMap::mmap_data(int fd, int off, int len)
{
int	a,i,r;

if(!data || base) return 0;
for(i=0;i<bands;i++) if(data[i]) return 0;

a=0;
if(data_access&DATA_READ) a=PROT_READ;
if(data_access&DATA_WRITE) a|=PROT_WRITE;

tlen=len;
if(!tlen) tlen=xres*yres*bands*sizeof(float);
if(tlen<xres*yres*bands*sizeof(float)) return 0;

r=off%page_size; off-=r; tlen+=r;

rbase=(float *)mmap(NULL,tlen,a,MAP_SHARED,fd,off);
if(rbase==(void *)-1) return 0;
base=(float *)((char *)rbase+r);

for(i=0;i<bands;i++) data[i]=base+i*xres*yres;
return 1;
}


int floatMMap::set_off(int b, int off)
{
if(!data || b<0 || b>=bands || !base || off<0 || off>tlen-xres*yres*sizeof(float)) return 0;

data[b]=(float *)((char *)base+off);
return 1;
}


float floatMMap::get_float(int x, int y, int b)
{
static float	temp;

if(check_bounds(x,y) || check_band(b)) return 0.0;

memcpy((char *)&temp,(char *)&data[b][x+y*xres],sizeof(float));
return temp;
}


double floatMMap::get_double(int x, int y, int b)
{
static float	temp;

if(check_bounds(x,y) || check_band(b)) return 0.0;

memcpy((char *)&temp,(char *)&data[b][x+y*xres],sizeof(float));
return (double)temp;
}


void floatMMapFV::free()
{
int	i;

if(!data) return;

if(base) munmap(
#ifdef SUN4
		(caddr_t)
#endif
#ifdef SOLARIS
		(char *)
#endif
			 rbase,tlen);
else	for(i=0;i<bands;i++) if(rdata[i]) munmap(
#ifdef SUN4
				(caddr_t)
#endif
#ifdef SOLARIS
				(char *)
#endif
					 rdata[i],xres*sizeof(float)+((char *)data[i]-(char *)rdata[i]));

::free((char *)data);
::free((char *)rdata);
data=NULL;
xres=0; yres=0; bands=0;
init();
}


int floatMMapFV::mmap_band(int b, int fd, int off)
{
int	a,r;

if(!data || b<0 || b>=bands) return 0;
if(base || data[b]) return 0;

a=0;
if(data_access&DATA_READ) a=PROT_READ;
if(data_access&DATA_WRITE) a|=PROT_WRITE;

r=off%page_size; off-=r;

rdata[b]=(float *)mmap(NULL,xres*yres*sizeof(float)+r,a,MAP_SHARED,fd,off);
if(rdata[b]==(void *)-1) return 0;
data[b]=(float *)((char *)rdata[b]+r)+xres*(yres-1);
return 1;
}


int floatMMapFV::mmap_data(int fd, int off, int len)
{
int	a,i,r;

if(!data || base) return 0;
for(i=0;i<bands;i++) if(data[i]) return 0;

a=0;
if(data_access&DATA_READ) a=PROT_READ;
if(data_access&DATA_WRITE) a|=PROT_WRITE;

tlen=len;
if(!tlen) tlen=xres*yres*bands*sizeof(float);
if(tlen<xres*yres*bands*sizeof(float)) return 0;

r=off%page_size; off-=r; tlen+=r;

rbase=(float *)mmap(NULL,tlen,a,MAP_SHARED,fd,off);
if(rbase==(void *)-1) return 0;
base=(float *)((char *)rbase+r);

for(i=0;i<bands;i++) data[i]=base+(i+1)*xres*yres-xres;
return 1;
}


int floatMMapFV::set_off(int b, int off)
{
if(!data || b<0 || b>=bands || !base || off<0 || off>tlen-xres*yres*sizeof(float)) return 0;

data[b]=(float *)((char *)base+off)+xres*(yres-1);
return 1;
}


float floatMMapFV::get_float(int x, int y, int b)
{
static float	temp;

if(check_bounds(x,y) || check_band(b)) return 0.0;

memcpy((char *)&temp,(char *)&data[b][x-y*xres],sizeof(float));
return temp;
}


double floatMMapFV::get_double(int x, int y, int b)
{
static float	temp;

if(check_bounds(x,y) || check_band(b)) return 0.0;

memcpy((char *)&temp,(char *)&data[b][x-y*xres],sizeof(float));
return (double)temp;
}

#endif // _NO_MMAP_
