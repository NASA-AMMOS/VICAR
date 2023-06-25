// pgmfile.C
//
// Written by John Wright 12/02/96

#include "image/types/pgmfile.h"
#include "image/image.h"
#include "image/datatypes.h"


int PGMFile::read_data(FILE *f)
{
int		xres,yres,max,c,x,y;
uchar		r;
ImageData	*idat;
char		buf[256];

if(!img) return error=ImageFileErr_NoImage;		// check for an Image

if(f) attach(f);					// set file pointer

// Read header:
fread(buf,3);
if(strncmp(buf,"P5",2))
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
if(!idat->allocate(xres,yres,1))
	return error=ImageFileErr_AllocFailed;

// Read the image:
idat->set_rgb_bands(0,0,0);
idat->set_alpha_band(-1);
idat->free_map();
for(y=0;y<yres;y++) {
	for(x=0;x<xres;x++) {
		r=(uchar)getc(fp);
		idat->set_color(x,y,r,r,r);
		}
	}

return error;
}


int PGMFile::write_data(FILE *f)
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
fprintf(fp,"P5\n");
if(comments) fprintf(fp,"# %s\n",comments);
fprintf(fp,"%d %d\n255\n",xres,yres);

// Write the image:
for(y=0;y<yres;y++) {
	for(x=0;x<xres;x++) {
		idat->get_color(x,y,&r,&g,&b);
		putc((int)r,fp);
		}
	}

return error;
}
