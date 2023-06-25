// rgbfile.C 1.4 03/03/27 07:58:44
// SGI RGB image format.
// Only supports uncompressed 8-bit-per-pixel color or monochrome.
// (Doesn't use SGI-specific library with same name as VSL libimage.a)

#include "image/types/rgbfile.h"
#include "image/image.h"
#include "image/datatypes.h"

// really ought to use <gl/image.h>.
// using uchar[] for endian-neutral code (file is big-endian)
#define RGB_MAGIC	0x01DA	// or 0732
struct RGB_header {
	uchar magic[2];		// magic number
	uchar type[2];		// must be 0001 = 8-bit uncompressed
	uchar dim[2];
	uchar xres[2], yres[2];
	uchar zres[2];		// must be 3=color or 1=mono
	uchar pmin[4], pmax[4];
	uchar waste[4];
	char name[512-24];
};

// endian-neutral data access
static int get16(uchar *p)
{
	return (p[0]<<8) + p[1];
}

static void put16(uchar *p, int val)
{
	p[0] = val>>8;
	p[1] = val;
}

int RGBFile::read_data(FILE *f)
{
	int		xres,yres,bands,x,y;
	ImageData	*idat;

	if(!img) 		// check for an Image
		return error=ImageFileErr_NoImage;

	if(f) attach(f);	// set file pointer

	// Read header:
	RGB_header hdr;
	fread(&hdr,sizeof(hdr));
	if (get16(hdr.magic) != RGB_MAGIC)
		return error=ImageFileErr_BadFile;
	bands = get16(hdr.zres);
	if (get16(hdr.type) != 1 || (bands != 1 && bands != 3)) {
		fprintf(stderr, "Currently unsupported RGB file format\n");
		return error=ImageFileErr_BadFile;
	}
	xres = get16(hdr.xres);
	yres = get16(hdr.yres);
	set_comments(hdr.name);

	// Get an image data pointer:
	idat=*img;
	if(!idat) {
		idat=new ucharData();
		*img=idat;
		}
	if(!(idat->access() & DATA_WRITE))
		return error=ImageFileErr_BadImageType;

	// Allocate memory:
	if(!idat->allocate(xres,yres,bands))
		return error=ImageFileErr_AllocFailed;

	// Read the image:
	if (bands == 1)
		idat->set_rgb_bands(0,0,0);
	else
		idat->set_rgb_bands(0,1,2);
	idat->set_alpha_band(-1);
	idat->free_map();
	for (int band=0; band<bands; band++) {
		for(y=yres-1;y>=0;y--) {
			for(x=0;x<xres;x++) {
				idat->set_uchar((uchar)getc(fp), x, y, band);
			}
		}
	}

	return error;
}

int RGBFile::write_data(FILE *f)
{
	int		xres,yres,bands,x,y;
	ImageData	*idat;

	// check for an Image
	if(!img) 		// check for an Image
		return error=ImageFileErr_NoImage;

	if(f) attach(f);	// set file pointer

	// Get an image data pointer:
	idat=*img;
	if(!idat)
		return error=ImageFileErr_BadImageType;
	if(!(idat->access() & DATA_READ))
		return error=ImageFileErr_BadImageType;

	// Get resolution:
	idat->get_res(&xres,&yres,&bands);
	if(xres<1 || yres<1)
		return error=ImageFileErr_BadImageData;

	// Write the header:
	RGB_header hdr;
	memset(&hdr, 0, sizeof(hdr));
	put16(hdr.magic, RGB_MAGIC);
	put16(hdr.type, 1);
	put16(hdr.dim, 3);
	put16(hdr.xres, xres);
	put16(hdr.yres, yres);
	put16(hdr.zres, bands);
	hdr.pmax[3] = 255;
	if (comments)
		strncpy(hdr.name, comments, sizeof(hdr.name)-1);
	fwrite(&hdr, sizeof(hdr));

	// Write the image:
	for (int band=0; band<bands; band++) {
		int b;		// allow unusual band mappings
		if (band == 0) 		b = idat->get_red_band();
		else if (band == 1)	b = idat->get_green_band();
		else			b = idat->get_blue_band();
		for(y=yres-1;y>=0;y--) {
			for(x=0;x<xres;x++) {
				putc(idat->get_uchar(x, y, b), fp);
			}
		}
	}

	return error;
}
