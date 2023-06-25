// dsfile.h
//
// Written by Dave Kagels 10/31/95

#ifndef _DSFILE_H_
#define _DSFILE_H_

#ifndef _NO_DATASERVER_

#include "image/imagefile.h"
#include "image/image.h"


class DSFile : public ImageFile {

   public:

	int	read_image(char *fname=NULL);
	int	write_image(char * =NULL) { return ImageFileErr_NotImplemented; }

	int	read_data(FILE * =NULL) { return ImageFileErr_NotImplemented; }
	int	write_data(FILE * =NULL) { return ImageFileErr_NotImplemented; }

	int	type() { return DS_FILE_ID; }			// class id

	DSFile(char *fname=NULL) : ImageFile(fname) { ; }
	};

#endif // _NO_DATASERVER_
#endif // _DSFILE_H_
