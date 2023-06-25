// ranfile.h
// 3D XYZ data from range map

#ifndef _RANFILE_H_
#define _RANFILE_H_

#include "image/imagefile.h"

class Image;

class RANFile : public ImageFile {

    public:

	int	read_data(FILE *f=NULL);
	int	write_data(FILE *f=NULL);

	int	type() { return RAN_FILE_ID; }			// class id

	RANFile(char *fname=NULL) : ImageFile(fname) { ; }
	};

#endif // _RANFILE_H_
