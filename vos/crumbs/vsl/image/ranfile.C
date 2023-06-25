// ranfile.C
// Reader can handle XYZ file data as float or double,
// native or swapped endian.
// Internal and written data is always float format, native endian.

#include "image/types/ranfile.h"
#include "image/image.h"
#include "image/datatypes.h"
#include "machine_setup.h"

// swap bytes for n float (4-byte) values at p to native endian order
#define SWAP(x,y)	a=bp[x]; bp[x]=bp[y]; bp[y]=a;

static void swap_floats(char *bp, int n)
{
	char a;
	for (; --n >= 0; bp+=4) {
		SWAP(0,3);
		SWAP(1,2);
	}
}

// swap bytes for n double (8-byte) values at p to native endian order
static void swap_doubles(char *bp, int n)
{
	char a;
	for (; --n >= 0; bp+=8) {
		SWAP(0,7);
		SWAP(1,6);
		SWAP(2,5);
		SWAP(3,4);
	}
}

int RANFile::read_data(FILE *f)
{
	if(!img) 
		return error=ImageFileErr_NoImage;

	if(f) 
		attach(f);

	// Read header: 4-byte #rows, 4-byte #cols
	unsigned char buf[8];
	int xres, yres;
	int swap = 0;
	if (fread((char *)buf, 8) != 8)
		return error=ImageFileErr_BadFile;
	// determine endian-ness, assuming max size of 64K x 64K
	if ((buf[0] | buf[1] | buf[4] | buf[5]) == 0) {
		// valid big-endian
		yres = (buf[2]<<8) + buf[3];	// rows
		xres = (buf[6]<<8) + buf[7];	// cols
		if (!REAL_BIGENDIAN)		// not the native format?
			swap = 1;
	} else if ((buf[2] | buf[3] | buf[6] | buf[7]) == 0) {
		// valid little-endian
		yres = (buf[1]<<8) + buf[0];	// rows
		xres = (buf[5]<<8) + buf[4];	// cols
		if (REAL_BIGENDIAN)		// not the native format?
			swap = 1;
	} else {
		return error=ImageFileErr_BadFile;
	}

	// Determine whether file data is float or double format
	// (based on file size)
	int dsize = sizeof(float);
	if (get_size() >= xres * yres * 3 * sizeof(double) + 8)
		dsize = sizeof(double);

	// Get an image data pointer:
	ImageData *idat = *img;
	if(!idat)
		*img = idat = new floatData();
	if(!(idat->access() & DATA_WRITE))
		return error=ImageFileErr_BadImageType;

	// Allocate memory:
	if(!idat->allocate(xres,yres,3))
		return error=ImageFileErr_AllocFailed;

	// Read the image:
	int rowbytes = xres * 3 * dsize;
	char *fbuf = (char *)malloc(rowbytes);
	if (fbuf == NULL)
		return error=ImageFileErr_AllocFailed;

	for(int y=0;y<yres;y++) {
		if (fread(fbuf, rowbytes) != rowbytes) {
			free(fbuf);
			return error=ImageFileErr_BadFile;
		}
		if (dsize == sizeof(float)) {
			if (swap)
				swap_floats(fbuf, 3*xres);
			float *pf = (float *)fbuf;
			for(int x=0;x<xres;x++) {
	        	        idat->set_float(*pf++, x, y, 0);
	                	idat->set_float(*pf++, x, y, 1);
		                idat->set_float(*pf++, x, y, 2);
			}
		} else {
			if (swap)
				swap_doubles(fbuf, 3*xres);
			double *pd = (double *)fbuf;
			for(int x=0;x<xres;x++) {
	        	        idat->set_float(*pd++, x, y, 0);
	                	idat->set_float(*pd++, x, y, 1);
		                idat->set_float(*pd++, x, y, 2);
			}
		}
	}
	free(fbuf);

	return error;
}


int RANFile::write_data(FILE *f)
{
	if(!img) 
		return error=ImageFileErr_NoImage;	// check for an Image

	if(f) 
		attach(f);				// set file pointer

	// Get an image data pointer:
	ImageData *idat = *img;
	if(!idat || !(idat->access() & DATA_READ))
		return error=ImageFileErr_BadImageType;

	// Get resolution:
	int xres, yres;
	idat->get_res(&xres,&yres);
	if(xres<1 || yres<1)
		return error=ImageFileErr_BadImageData;

	// Write the header:
	fwrite(&yres, 4);
	fwrite(&xres, 4);

	// Write the image:
	for(int y=0;y<yres;y++) {
		for(int x=0;x<xres;x++) {
			float xyz[3];
			xyz[0] = idat->get_float(x,y,0);
			xyz[1] = idat->get_float(x,y,1);
			xyz[2] = idat->get_float(x,y,2);
			fwrite(xyz, 3*sizeof(float));
		}
	}

	return error;
}
