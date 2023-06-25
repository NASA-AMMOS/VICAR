// vicarfile.h
//
// Written by Dave Kagels 2/15/95

#ifndef _VICARFILE_H_
#define _VICARFILE_H_

#include "image/imagefile.h"
#include <string.h>

class Image;

// ****************************** VicByte *****************************

class VicByte : public ImageFile {

    public:

	int	read_image(char *fname=NULL);
	int	write_image(char *fname=NULL);

	int	read_data(FILE * =NULL) { return ImageFileErr_NotImplemented; }
	int	write_data(FILE * =NULL) { return ImageFileErr_NotImplemented; }

	int	read_band(int b, int n);

	int	type() { return VIC_BYTE_FILE_ID; }			// class id

	VicByte(char *fname=NULL) : ImageFile(fname) { ; }
	};


// ****************************** VicHalf *****************************

class VicHalf : public ImageFile {

    public:

	int	read_image(char *fname=NULL);
	int	write_image(char *fname=NULL);

	int	read_data(FILE * =NULL) { return ImageFileErr_BadFileType; }
	int	write_data(FILE * =NULL) { return ImageFileErr_BadFileType; }

	int	read_band(int b, int n);

	int	type() { return VIC_HALF_FILE_ID; }			// class id

	VicHalf(char *fname=NULL) : ImageFile(fname) { ; }
	};


// ****************************** VicReal *****************************

class VicReal : public ImageFile {

    public:

	int	read_image(char *fname=NULL);
	int	write_image(char *fname=NULL);

	int	read_data(FILE * =NULL) { return ImageFileErr_BadFileType; }
	int	write_data(FILE * =NULL) { return ImageFileErr_BadFileType; }

	int	read_band(int b, int n);

	int	type() { return VIC_REAL_FILE_ID; }			// class id

	VicReal(char *fname=NULL) : ImageFile(fname) { ; }
	};

#endif // _VICARFILE_H_
