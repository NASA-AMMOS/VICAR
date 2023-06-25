// vicarfile.C
//
// Written by Dave Kagels 2/15/95
// Probably ought to use templates to avoid all the duplicated code...

#include "image/types/vicarfile.h"
#include "image/image.h"
#include "image/datatypes.h"
#include "image/datammap.h"
#include <string.h>
#include "machine_setup.h"

#ifndef _NO_MMAP_
#define CHARDATA	ucharMMap
#else
#define CHARDATA	ucharData
#endif

// Write VICAR format label (band sequential, native endian)	
// psize: 1=byte, 2=halfword, 4=real.
// Returns <= 0 on error.
// If image has a "comments" field and the field does NOT start
// with "LBLSIZE", the comments are added after the standard label stuff.
static int vicar_label(FILE *f, int xres, int yres, int bands, 
				int psize, char *comments)
{
	// bytes per line
	int recsize = xres * psize;

	// include comments as user-specified label fields?
	int cmt_len = 0;
	if (comments && strncmp(comments, "LBLSIZE", 7))
		cmt_len = strlen(comments);
	
	// header must be multiple of recsize, and enough for required fields 
	int hdrsize = ((199 + cmt_len + recsize) / recsize) * recsize;

	// allocate header
	char *hdr = (char *)malloc(hdrsize);
	memset(hdr, 0, hdrsize);
	
	// format header
	static const char *sizename[] = { 
		"BOGUS", "BYTE", "HALF", "BOGUS", "REAL" };
		
	sprintf(hdr, "LBLSIZE=%d  BUFSIZ=%d  RECSIZE=%d  "
		"FORMAT='%s'  TYPE='IMAGE'  ORG='BSQ'  NBB=0  DIM=3  "
		"INTFMT='%s'  REALFMT='%s'  "
		"NS=%d  N1=%d  NL=%d  N2=%d  NB=%d  N3=%d %s",
		hdrsize, recsize, recsize, sizename[psize],
		INT_BIGENDIAN ? "HIGH" : "LOW",
		REAL_BIGENDIAN ? "IEEE" : "RIEEE",
		xres, xres, yres, yres, bands, bands,
		cmt_len ? comments : "");

	int e = ::fwrite(hdr, 1, hdrsize, f);
	free(hdr);
	return e;
}

// parse VICAR label, get xres, yres, number of bands
static int parse_label(char *buf, int &x, int &y, int &nb)
{
	char *ptr;

	ptr=strstr(buf," NS=");
	if(!ptr) return ImageFileErr_BadFile;
	sscanf(ptr+4,"%d",&x);
	if(x<0 || x>1048576) return ImageFileErr_BadFile;

	ptr=strstr(buf," NL=");
	if(!ptr) return ImageFileErr_BadFile;
	sscanf(ptr+4,"%d",&y);
	if(y<0 || y>1048576) return ImageFileErr_BadFile;

	ptr=strstr(buf," NB=");
	//if(!ptr) return ImageFileErr_BadFile;
	if(ptr) {
		sscanf(ptr+4,"%d",&nb);
		if(nb<0 || nb>16) return ImageFileErr_BadFile;
	} else {
		nb = 1;
	}

	return 0;
}

// write one image band's data as specified type (stolen from pdsfile.C)
template<class T> void write_img_band(int xres, int yres, int band, 
					FILE *f, ImageData *idat, T dummy)
{
	T local;
	idat->cband = band;
	for(int j=0; j<yres; j++) {
		idat->cy = j;
		for(int i=0; i<xres; i++) {
			idat->cx = i; 
			local = (T)(*idat);
			::fwrite(&local, sizeof(T), 1, f);
		}
	}
}

// split comma-separated filename list in buf into separate strings.
// return count, modifies buf.
static int split_filename(char *buf, char *fn[])
{
	int n;
	for(n=1,fn[0]=buf; n<16 && (fn[n]=strchr(fn[n-1],',')); n++)
		*(fn[n]++)=0;
	return n;
}

// ****************************** VicByte *****************************

// Currently supports either multiple files with one band per file
// (e.g. "foo.r,foo.g,foo.b"), or a single file with one or more bands.
// Does NOT work with multiple files with multiple bands (since mmaps don't
// currently provide add_band() methods, and I don't want to scan all
// the files an extra time just to count bands).
int VicByte::read_image(char *fname)
{
	char	buf[2048],rbuf[2048],*fn[16];
	int	i,n;

	reset_error();
	if(fname) set_filename(fname);
	get_filename(buf);
	strcpy(rbuf,buf);
	n = split_filename(buf, fn);

	for(i=0;i<n;i++) {
		set_filename(fn[i]);
		if(open(FILE_READ)) break;
		error=read_band(i,n);
		close();
		if(error) break;
		}
	set_filename(rbuf);
	return error;
}

// read one or more image bands from vicar file.
// Byte data is always memory-mapped where mmap is supported.
int VicByte::read_band(int b, int n)
{
	char	buf[2048],*ptr;
	int	ls,x,y,rx,ry;
	int	i,nb;

	if(!img) return ImageFileErr_NoImage;		// check for an Image

	// Read header:
	read(buf,2047);
	if(strncmp(buf,"LBLSIZE=",8))
		return ImageFileErr_BadFile;
	buf[2047]=(char)0;
	sscanf(buf+8,"%d",&ls);

	// storing label in imagefile comments
	ptr = (char *)malloc(ls * sizeof(char) + 1);
	if(ls < 2048) {
		strncpy(ptr, buf, ls);
		ptr[ls] = '\0';
	} else {
		strncpy(ptr, buf, 2047);
		read(&(ptr[2048]), ls - 2047);
		ptr[ls] = '\0';
	}
	set_comments(ptr);
	free(ptr);

	rx = parse_label(buf, x, y, nb);
	if (rx) return rx;

	CHARDATA *idat;
#ifndef _NO_MMAP_		/* mmap version */
	method=0;
#else
	uchar 		*pBuf;
	if ((pBuf = (uchar *)malloc( x * y * sizeof(uchar))) == NULL)
		return ImageFileErr_AllocFailed;
#endif

	// adjust total number of bands
	// (unfortunately, "n" is only used for b=0)
	if (b+nb > n)
		n = b+nb;

	// for each band in file
	for (i=0; i<nb; i++, b++) {

		if(b==0) {		// first band

			idat=new CHARDATA();
#ifndef _NO_MMAP_
			idat->mmap_setup(DATA_READ,x,y,n);
#else
			if(!idat->allocate(x,y,n))
				return ImageFileErr_AllocFailed;
#endif
			*img=(ImageData *)idat;
			idat->set_rgb_bands(0,0,0);
			idat->set_alpha_band(-1);
			idat->set_default_band(0);

		} else {
			idat=(CHARDATA *)img->get_data();
			idat->get_res(&rx,&ry);
			if(x!=rx || y!=ry) return ImageFileErr_BadFile;

			if(b==1) idat->set_rgb_bands(0,1,1);
			else if(b==2) idat->set_blue_band(2);
			else if(b==3) idat->set_alpha_band(3);
		}

		long offset = ls + i * x * y * sizeof(uchar);
#ifndef _NO_MMAP_
		idat->mmap_band(b,fd,offset);
#else
		// seek to beg of data
		if (::fseek( fp, offset, SEEK_SET))
			return ImageFileErr_NotImplemented;
	
		// read band's data
		if (fread( pBuf, x*y*sizeof(uchar)) != x*y*sizeof(uchar))
			return ImageFileErr_NotImplemented;

		// store in imagedata
		int cx, cy;
		uchar *pv = pBuf;
		for (cy=0;cy<y;cy++) {
			for (cx=0;cx<x;cx++) {
				idat->set_uchar( *pv++, cx, cy, b);
			}
		}
#endif
	}	// end band loop

#ifdef _NO_MMAP_
	free( pBuf);
#endif

return error;
}

// Write as one multiband file, or to set of single-band files
// if fname = comma-separated list of names.
int VicByte::write_image(char *fname)
{
	// check/access image data and attributes
	if (!img)
		return error = ImageFileErr_NoImage;
	ImageData *idat = *img;
	if (!idat)
		return error = ImageFileErr_BadImageType;
	int xres, yres, bands;
	idat->get_res(&xres, &yres, &bands);
	if (xres<1 || yres<1)
		return error=ImageFileErr_BadImageData;

	// handle filename list
	char buf[2048], rbuf[2048], *fn[16];
	if(fname) set_filename(fname);
	get_filename(buf);
	strcpy(rbuf,buf);
	int n = split_filename(buf, fn);
	if (n > 1 && n != bands)	// wrong number of single-band files?
		return error=ImageFileErr_NotImplemented;

	// write each band
	for (int b=0; b<bands; b++) {
		if (b==0 || n>1) {	// first file, or one file per band
			// open output file
			close();
			set_filename(fn[b]);
            //make sure output file will have proper permissions
            umask(002);
			if (fopen(FILE_WRITE))
				return error = FileErr_CouldNotOpen;

			// write header
			vicar_label(get_fp(), xres, yres, n>1 ? 1 : bands, 
				1, get_comments());
		}
	
		// write band data
		write_img_band(xres, yres, b, get_fp(), idat, (uchar)0);
	}
	set_filename(rbuf);
	return error;
}


// ****************************** VicHalf *****************************

int VicHalf::read_image(char *fname)
{
	char	buf[2048],rbuf[2048],*fn[16];
	int	i,n;

	reset_error();
	if(fname) set_filename(fname);
	get_filename(buf);
	strcpy(rbuf,buf);
	n = split_filename(buf, fn);

	for(i=0;i<n;i++) {
		set_filename(fn[i]);
		if(open(FILE_READ)) break;
		error=read_band(i,n);
		close();
		if(error) break;
		}

	set_filename(rbuf);
	return error;
}

// read one or more image bands from vicar file.
// If mmap is supported and file is in native byte ordering, just
// map file, else read to buffer (and swap if needed).
int VicHalf::read_band(int b, int n)
{
	char	buf[2048],*ptr;
	int	ls,x,y,rx,ry;
	int	i,nb;

	if(!img) return ImageFileErr_NoImage;		// check for an Image

	// Read header:
	read(buf,2047);
	if(strncmp(buf,"LBLSIZE=",8))
		return ImageFileErr_BadFile;
	buf[2047]=(char)0;
	sscanf(buf+8,"%d",&ls);

	// check for non-native byte ordering
	// (if INTFMT isn't specified, assume it's native!)
	int swap = 0;
	ptr = strstr(buf, " INTFMT=");
	if (ptr) {
		ptr += 9;
		if (*ptr == '\'')	// skip optional single quote
			ptr++;
		if (INT_BIGENDIAN && *ptr=='L')		// LOW
			swap = 1;
		else if (!INT_BIGENDIAN && *ptr=='H')	// HIGH
			swap = 1;
	}
	
	// storing label in imagefile comments
	ptr = (char *)malloc(ls * sizeof(char) + 1);
	if(ls < 2048) {
		strncpy(ptr, buf, ls);
		ptr[ls] = '\0';
	} else {
		strncpy(ptr, buf, 2047);
		read(&(ptr[2048]), ls - 2047);
		ptr[ls] = '\0';
	}
	set_comments(ptr);
	free(ptr);

	rx = parse_label(buf, x, y, nb);
	if (rx) return rx;
	
	int mapped = 0;
#ifndef _NO_MMAP_
	if (!swap)		// okay to memory map?
		mapped = 1;
#endif

	shortData *idat;
	short *pBuf;
	int nbytes = x * y * sizeof(short);
	if (mapped)
		method=0;
	else if ((pBuf = (short *)malloc(nbytes)) == NULL)
		return ImageFileErr_AllocFailed;

	// adjust total number of bands 
	// (unfortunately, "n" is only used for b=0)
	if (b+nb > n)
		n = b+nb;

	// for each band in file
	for (i=0; i<nb; i++, b++) {

		if(b==0) {		// first band

			if (mapped) {
				idat = new shortMMap();
				((shortMMap *)idat)->mmap_setup(DATA_READ,x,y,n);
			} else {
				idat = new shortData();
				if(!idat->allocate(x,y,n))
					return ImageFileErr_AllocFailed;
			}
			*img=(ImageData *)idat;
			idat->set_rgb_bands(0,0,0);
			idat->set_alpha_band(-1);
			idat->set_default_band(0);

		} else {
			idat=(shortData *)img->get_data();
			idat->get_res(&rx,&ry);
			if(x!=rx || y!=ry) return ImageFileErr_BadFile;

			if(b==1) idat->set_rgb_bands(0,1,1);
			else if(b==2) idat->set_blue_band(2);
			else if(b==3) idat->set_alpha_band(3);
		}

		long offset = ls + i * nbytes;
		if (mapped) {
			((shortMMap *)idat)->mmap_band(b,fd,offset);
		} else {
			// seek to beg of data
			if (::fseek( fp, offset, SEEK_SET))
				return ImageFileErr_NotImplemented;
	
			// read band's data
			if (fread( pBuf, nbytes) != nbytes)
				return ImageFileErr_NotImplemented;

			// swap bytes if necessary
			if (swap) {
				char *bp = (char *)pBuf;
				for (i=0; i<nbytes; i+=2, bp+=2) {
					char a = bp[0];
					bp[0] = bp[1];
					bp[1] = a;
				}
			}

			// store in imagedata
			short *pv = pBuf;
			int cx, cy;
			for (cy=0;cy<y;cy++) {
				for (cx=0;cx<x;cx++) {
					idat->set_short( *pv++, cx, cy, b);
				}
			}
		}
	}	// end band loop

	if (!mapped)
		free(pBuf);

	return error;
}


int VicHalf::write_image(char *fname)
{
	// check/access image data and attributes
	if (!img)
		return error = ImageFileErr_NoImage;
	ImageData *idat = *img;
	if (!idat)
		return error = ImageFileErr_BadImageType;
	int xres, yres, bands;
	idat->get_res(&xres, &yres, &bands);
	if (xres<1 || yres<1)
		return error=ImageFileErr_BadImageData;

	// handle filename list
	char buf[2048], rbuf[2048], *fn[16];
	if(fname) set_filename(fname);
	get_filename(buf);
	strcpy(rbuf,buf);
	int n = split_filename(buf, fn);
	if (n > 1 && n != bands)	// wrong number of single-band files?
		return error=ImageFileErr_NotImplemented;

	// write each band
	for (int b=0; b<bands; b++) {
		if (b==0 || n>1) {	// first file, or one file per band
			// open output file
			close();
			set_filename(fn[b]);
			if (fopen(FILE_WRITE))
				return error = FileErr_CouldNotOpen;

			// write header
			vicar_label(get_fp(), xres, yres, n>1 ? 1 : bands, 
				2, get_comments());
		}
	
		// write band data
		write_img_band(xres, yres, b, get_fp(), idat, (short)0);
	}
	set_filename(rbuf);
	return error;
}


// ****************************** VicReal[MMap] *****************************

int VicReal::read_image(char *fname)
{
	char	buf[2048],rbuf[2048],*fn[16];
	int	i,n;

	reset_error();
	if(fname) set_filename(fname);
	get_filename(buf);
	strcpy(rbuf,buf);
	n = split_filename(buf, fn);

	for(i=0;i<n;i++) {
		set_filename(fn[i]);
		if(open(FILE_READ)) break;
		error=read_band(i,n);
		close();
		if(error) break;
		}

	set_filename(rbuf);
	return error;
}

// Read one or more image bands from vicar file.
// If mmap is supported and file is in native byte ordering, just
// map file, else read to buffer (and swap if needed).
int VicReal::read_band(int b, int n)
{
	char	buf[2048],*ptr;
	int	ls,x,y,rx,ry;
	int	i,nb;
	
	if(!img) return ImageFileErr_NoImage;		// check for an Image

	// Read header:
	read(buf,2047);
	if(strncmp(buf,"LBLSIZE=",8))
		return ImageFileErr_BadFile;
	buf[2047]=(char)0;
	sscanf(buf+8,"%d",&ls);

	// check for non-native byte ordering
	// (if REALFMT isn't specified, assume it's native!)
	int swap = 0;
	ptr = strstr(buf, " REALFMT=");
	if (ptr) {
		ptr += 10;
		if (*ptr == '\'')	// skip optional single quote
			ptr++;
		if (REAL_BIGENDIAN && *ptr=='R')	// RIEEE
			swap = 1;
		else if (!REAL_BIGENDIAN && *ptr=='I')	// IEEE
			swap = 1;
	}
	
	// storing label in imagefile comments
	ptr = (char *)malloc(ls * sizeof(char) + 1);
	if(ls < 2048) {
		strncpy(ptr, buf, ls);
		ptr[ls] = '\0';
	} else {
		strncpy(ptr, buf, 2047);
		read(&(ptr[2048]), ls - 2047);
		ptr[ls] = '\0';
	}
	set_comments(ptr);
	free(ptr);

	rx = parse_label(buf, x, y, nb);
	if (rx) return rx;

	int mapped = 0;
#ifndef _NO_MMAP_
	if (!swap)		// okay to memory map?
		mapped = 1;
#endif

	floatData *idat;
	float *pBuf;
	int nbytes = x * y * sizeof(float);
	if (mapped)
		method=0;
	else if ((pBuf = (float *)malloc(nbytes)) == NULL)
		return ImageFileErr_AllocFailed;

	// adjust total number of bands
	// (unfortunately, "n" is only used for b=0)
	if (b+nb > n)
		n = b+nb;

	// for each band in file
	for (i=0; i<nb; i++, b++) {

		if(b==0) {		// first band

			if (mapped) {
				idat = new floatMMap();
				((floatMMap *)idat)->mmap_setup(DATA_READ,x,y,n);
			} else {
				idat = new floatData();
				if(!idat->allocate(x,y,n))
					return ImageFileErr_AllocFailed;
			}
			*img=(ImageData *)idat;
			idat->set_rgb_bands(0,0,0);
			idat->set_alpha_band(-1);
			idat->set_default_band(0);

		} else {
			idat=(floatData *)img->get_data();
			idat->get_res(&rx,&ry);
			if(x!=rx || y!=ry) return ImageFileErr_BadFile;

			if(b==1) idat->set_rgb_bands(0,1,1);
			else if(b==2) idat->set_blue_band(2);
			else if(b==3) idat->set_alpha_band(3);
		}

		long offset = ls + i * nbytes;
		if (mapped) {
			((floatMMap *)idat)->mmap_band(b,fd,offset);
		} else {
			// seek to beg of data
			if (::fseek( fp, offset, SEEK_SET))
				return ImageFileErr_NotImplemented;
	
			// read band's data
			if (fread( pBuf, nbytes) != nbytes)
				return ImageFileErr_NotImplemented;

			// swap bytes if necessary
			if (swap) {
				char *bp = (char *)pBuf;
				for (i=0; i<nbytes; i+=4, bp+=4) {
					char a = bp[0];
					bp[0] = bp[3];
					bp[3] = a;
					a = bp[1];
					bp[1] = bp[2];
					bp[2] = a;
				}
			}
			
			// store in imagedata
			int cx, cy;
			float *pv = pBuf;
			for (cy=0;cy<y;cy++) {
				for (cx=0;cx<x;cx++) {
					idat->set_float( *pv++, cx, cy, b);
				}
			}
		}
	}	// end band loop

	if (!mapped)
		free( pBuf);

	return error;
}

int VicReal::write_image(char *fname)
{
	// check/access image data and attributes
	if (!img)
		return error = ImageFileErr_NoImage;
	ImageData *idat = *img;
	if (!idat)
		return error = ImageFileErr_BadImageType;
	int xres, yres, bands;
	idat->get_res(&xres, &yres, &bands);
	if (xres<1 || yres<1)
		return error=ImageFileErr_BadImageData;

	// handle filename list
	char buf[2048], rbuf[2048], *fn[16];
	if(fname) set_filename(fname);
	get_filename(buf);
	strcpy(rbuf,buf);
	int n = split_filename(buf, fn);
	if (n > 1 && n != bands)	// wrong number of single-band files?
		return error=ImageFileErr_NotImplemented;

	// write each band
	for (int b=0; b<bands; b++) {
		if (b==0 || n>1) {	// first file, or one file per band
			// open output file
			close();
			set_filename(fn[b]);
			if (fopen(FILE_WRITE))
				return error = FileErr_CouldNotOpen;

			// write header
			vicar_label(get_fp(), xres, yres, n>1 ? 1 : bands, 
				4, get_comments());
		}
	
		// write band data
		write_img_band(xres, yres, b, get_fp(), idat, (float)0);
	}
	set_filename(rbuf);
	return error;
}
