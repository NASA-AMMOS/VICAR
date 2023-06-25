// asvcfile.C
//
// Written by Dave Kagels 10/18/94

#include "image/types/asvcfile.h"
#include "image/image.h"
#include "image/datatypes.h"


int ASVCFile::read_data(FILE *f)
{
int		xres,yres,c,x,y;
uchar		r,g,b,a;
ImageData	*idat;
char		buf[256];

if(!img) return error=ImageFileErr_NoImage;		// check for an Image

if(f) attach(f);					// set file pointer

// Read header:
fread(buf,4);
if(strncmp(buf,"ASVC",4))
	return error=ImageFileErr_BadFile;

xres = getc(fp);
c = getc(fp);
xres = (xres << 8) + c;

yres = getc(fp);
c = getc(fp);
yres = (yres << 8) + c;

if(xres<1 || yres<1)
	return error=ImageFileErr_BadFile;

// Get an image data pointer:
idat=*img;
if(!idat) {
	idat=new ucharData();
	*img=idat;
	}
if(!(idat->access() & DATA_WRITE))
	return error=ImageFileErr_BadImageType;

// Allocate memory:
if(!idat->allocate(xres,yres,4))
	return error=ImageFileErr_AllocFailed;

// Read the image:
idat->set_rgb_bands(0,1,2);
idat->set_alpha_band(3);
idat->free_map();
for(y=0;y<yres;y++) {
	for(x=0;x<xres;x++) {
		a=(uchar)getc(fp); b=(uchar)getc(fp); g=(uchar)getc(fp); r=(uchar)getc(fp);
		idat->set_color(x,y,r,g,b,a);
		}
	}

return error;
}


int ASVCFile::write_data(FILE *f)
{
int		xres,yres,x,y;
uchar		r,g,b,a;
ImageData	*idat;

if(!img) return error=ImageFileErr_NoImage;		// check for an Image

if(f) attach(f);					// set file pointer

// Get an image data pointer:
idat=*img;
if(!idat)
	return error=ImageFileErr_BadImageType;
if(!(idat->access() & DATA_READ))
	return error=ImageFileErr_BadImageType;

// Get resolution:
idat->get_res(&xres,&yres);
if(xres<1 || yres<1)
	return error=ImageFileErr_BadImageData;

// Write the header:
fprintf(fp,"ASVC");
putc((xres>>8) & 255, fp); putc(xres & 255, fp);
putc((yres>>8) & 255, fp); putc(yres & 255, fp);

// Write the image:
for(y=0;y<yres;y++) {
	for(x=0;x<xres;x++) {
		idat->get_color(x,y,&r,&g,&b,&a);
		putc((int)a,fp); putc((int)b,fp); putc((int)g,fp); putc((int)r,fp);
		}
	}

return error;
}
