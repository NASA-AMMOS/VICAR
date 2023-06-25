// rgbfile.h
// SGI RGB color image (only 8-bit uncompressed format supported)

#ifndef _RGBFILE_H_
#define _RGBFILE_H_

#include "image/imagefile.h"

class Image;

class RGBFile : public ImageFile {

    public:

	int	read_data(FILE *f=NULL);
	int	write_data(FILE *f=NULL);

	int	type() { return RGB_FILE_ID; }			// class id

	RGBFile(char *fname=NULL) : ImageFile(fname) { ; }
	};

#endif // _RGBFILE_H_
