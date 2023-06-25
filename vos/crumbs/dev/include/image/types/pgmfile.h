// ppmfile.h
//
// Written by Dave Kagels 10/18/94
// Adapted by John Wright 12/02/96

#ifndef _PGMFILE_H_
#define _PGMFILE_H_

#include "image/imagefile.h"

class Image;


class PGMFile : public ImageFile {

    public:

	int	read_data(FILE *f=NULL);
	int	write_data(FILE *f=NULL);

	int	type() { return PGM_FILE_ID; }			// class id

	int	set_comments(char *str=NULL) {		// comments implemented
			ImageFile::set_comments(str); return 1; }
	int	add_comments(char *str=NULL) {
			ImageFile::add_comments(str); return 1; }

	PGMFile(char *fname=NULL) : ImageFile(fname) { ; }
	};

#endif // _PGMFILE_H_
