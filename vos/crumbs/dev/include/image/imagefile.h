// imagefile.h
//
// Written by Dave Kagels 10/11/94

#ifndef _IMAGEFILE_H_
#define _IMAGEFILE_H_

#include "file.h"
#include "image/filetypes.h"

class Image;

#define	ImageFileErr_NoImage		101		// error codes
#define ImageFileErr_BadFile		102
#define ImageFileErr_AllocFailed	103
#define	ImageFileErr_BadImageType	104
#define	ImageFileErr_BadImageData	105
#define ImageFileErr_BadFileType	106
#define ImageFileErr_GetNewTypeFailed	107
#define ImageFileErr_NotImplemented	108

// Flag for compressing images with .Z file extensions
extern int IMAGE_COMPRESSION;
// Possible values:
#define	DISABLE_COMPRESSION	0	// Don't compress
#define	ENABLE_COMPRESSION	1	// Compress without warning
#define	WARN_COMPRESSION	2	// Compress, but print a warning


// This class is the base class for image file storage objects.
// It contains a pointer to the image, and comments and flags information.
// It is a derived class of File, so it has all the necessary file information.
// Member functions are provided for reading and writing the image,
// accessing the comments and flags, and getting image file type information.
// Derived classes can be made for each specific file type.

class ImageFile : public File {

    protected:

	Image	*img;				// pointer to image
	char	*comments;			// pointer to comments string
	char	*flags;				// pointer to flags string

	// these image size float.have been added to support the
	// read_header and get_res functions - when read_header
	// reads an image header, it should set these values for
	// later use by get_res.  Normally, get_res will get the
	// values from the imagedata object but read_header does
	// not create such an object
	int file_xres;
	int file_yres;
	int file_bands;

	// Initialize member variables:
	void	init() { img=NULL; comments=NULL; flags=NULL; 
			file_xres = file_yres = file_bands = 0; }

    public:

	// Read/Write functions (take filename, return status):
	virtual	int	read_image(char *fname=NULL);
	virtual	int	read_header(char *fname=NULL) { return read_image(fname); }
	virtual int	write_image(char *fname=NULL);

	virtual int	read_data(FILE * =NULL) { return ImageFileErr_BadFileType; }
	virtual int	write_data(FILE * =NULL) { return ImageFileErr_BadFileType; }

	Image	*get_image() { return img; }		// get image pointer

	virtual	void	set_image(Image *i) { img=i; }	// set image pointer

	// Get image type information:
	virtual int	type() { return BASE_FILE_ID; }			// class id
	virtual char	*type_name() { return file_type_name[type()]; }

	// Functions for manipulationg comments and flags:
	void		free_comments() { if(comments) free(comments);
					  comments=NULL; }
	void		free_flags() { if(flags) free(flags);
					  flags=NULL; }
	virtual char	*get_comments() { return comments; }
	virtual char	*get_flags() { return flags; }
	virtual int	set_comments(char *str=NULL) { free_comments();
				if(str) { comments=(char *)malloc(strlen(str)+1);
					  strcpy(comments,str); }
				return 0; }
	virtual int	set_flags(char *str=NULL) { free_flags();
				if(str) { flags=(char *)malloc(strlen(str)+1);
					  strcpy(flags,str); }
				return 0; }
	virtual int	add_comments(char *s) { int i;
				if(comments) i=strlen(comments); else i=0;
				comments=(char *)realloc(comments,strlen(s)+i+1);
				strcpy(comments+i,s); return 0; }
	virtual int	add_flags(char *s) { int i;
				if(flags) i=strlen(flags); else i=0;
				flags=(char *)realloc(flags,strlen(s)+i+1);
				strcpy(flags+i,s); return 0; }

	virtual int	get_res(int *x=NULL, int *y=NULL, int *b=NULL) {
				if(x) *x = file_xres;
				if(y) *y = file_yres;
				if(b) *b = file_bands;
				return(file_xres);
			}

	// Typecast operator to get image pointer:
	operator Image*() { return img; }

	// Constructor:
	ImageFile(char *fname=NULL) : File(fname) { init(); }

	virtual	~ImageFile() { free_comments(); free_flags(); }
	};

#endif // _IMAGEFILE_H_
