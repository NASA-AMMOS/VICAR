// ppmfile.C
//
// Written by Dave Kagels 10/18/94

#include "image/types/ppmfile.h"
#include "image/image.h"
#include "image/datatypes.h"


int PPMFile::read_data(FILE *f)
{
int		xres,yres,max,c,x,y;
uchar		r,g,b;
ImageData	*idat;
char		buf[256];

if(!img) return error=ImageFileErr_NoImage;		// check for an Image

if(f) attach(f);					// set file pointer

// Read header:
fread(buf,3);
if(strncmp(buf,"P6",2))
	return error=ImageFileErr_BadFile;
c=getc(fp);
buf[1]=(char)0;
if(c=='#') {
	fgets(buf,256,fp);
	if(strlen(buf)) buf[strlen(buf)-1]=(char)0;	// strip newline
	set_comments(buf+1);				// skip space
	}
else	ungetc(c,fp);
fscanf(fp,"%d%d%d",&xres,&yres,&max);
getc(fp);
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
if(!idat->allocate(xres,yres,3))
	return error=ImageFileErr_AllocFailed;

// Read the image:
idat->set_rgb_bands(0,1,2);
idat->set_alpha_band(-1);
idat->free_map();
for(y=0;y<yres;y++) {
	for(x=0;x<xres;x++) {
		r=(uchar)getc(fp); g=(uchar)getc(fp); b=(uchar)getc(fp);
		idat->set_color(x,y,r,g,b);
		}
	}

return error;
}


int PPMFile::write_data(FILE *f)
{
int		xres,yres,x,y;
uchar		r,g,b;
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
fprintf(fp,"P6\n");
if(comments) fprintf(fp,"# %s\n",comments);
fprintf(fp,"%d %d\n255\n",xres,yres);

// Write the image:
for(y=0;y<yres;y++) {
	for(x=0;x<xres;x++) {
		idat->get_color(x,y,&r,&g,&b);
		putc((int)r,fp); putc((int)g,fp); putc((int)b,fp);
		}
	}

return error;
}
