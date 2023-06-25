// ppmfile.h
//
// Written by Dave Kagels 10/18/94

#ifndef _PPMFILE_H_
#define _PPMFILE_H_

#include "image/imagefile.h"

class Image;


class PPMFile : public ImageFile {

    public:

	int	read_data(FILE *f=NULL);
	int	write_data(FILE *f=NULL);

	int	type() { return PPM_FILE_ID; }			// class id

	int	set_comments(char *str=NULL) {		// comments implemented
			ImageFile::set_comments(str); return 1; }
	int	add_comments(char *str=NULL) {
			ImageFile::add_comments(str); return 1; }

	PPMFile(char *fname=NULL) : ImageFile(fname) { ; }
	};

#endif // _PPMFILE_H_
