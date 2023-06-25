// kapfile.h
//
// Written by John Wright 03/12/97

#ifndef _KAPFILE_H_
#define _KAPFILE_H_

#include "image/imagefile.h"
#include "image/geoimage.h"
#include "image/types/kap_geodata.h"

class Image;


class KAPFile : public ImageFile {

    protected:

	int	read_head(FILE *f=NULL);

    public:

	int	read_header(char *fname=NULL);
	int	read_data(FILE *f=NULL);
	int	write_data(FILE *f=NULL);

	int	type() { return KAP_FILE_ID; }			// class id

	int	set_comments(char *str=NULL) {		// comments implemented
			ImageFile::set_comments(str); return 1; }
	int	add_comments(char *str=NULL) {
			ImageFile::add_comments(str); return 1; }

	KAPFile(char *fname=NULL) : ImageFile(fname) { ; }
	};

#endif // _KAPFILE_H_
