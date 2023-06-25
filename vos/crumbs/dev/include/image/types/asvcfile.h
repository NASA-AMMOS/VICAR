// asvcfile.h
//
// Written by John Wright  05/22/2000

#ifndef _ASVCFILE_H_
#define _ASVCFILE_H_

#include "image/imagefile.h"

class Image;


class ASVCFile : public ImageFile {

    public:

	int	read_data(FILE *f=NULL);
	int	write_data(FILE *f=NULL);

	int	type() { return ASVC_FILE_ID; }			// class id

	int	set_comments(char *str=NULL) {		// comments implemented
			ImageFile::set_comments(str); return 1; }
	int	add_comments(char *str=NULL) {
			ImageFile::add_comments(str); return 1; }

	ASVCFile(char *fname=NULL) : ImageFile(fname) { ; }
	};

#endif // _ASVCFILE_H_
