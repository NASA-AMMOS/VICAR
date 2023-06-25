// rlafile.h
//
// Written by Dave Kagels 10/09/95

#ifndef _RLAFILE_H_
#define _RLAFILE_H_

#include "image/imagefile.h"
#include "image/image.h"


// Global variable for loading auxiliary channel of RLA files:
extern int	RLA_Load;

#define	RLA_NO_AUX	0
#define	RLA_LOAD_AUX	1
#define RLA_AUX_ONLY	2


class RLAFile : public ImageFile {

	Image	*aux_img;

	int	clean;

    public:

	// Set the auxiliary channel image:
	void	set_aux_img(Image *i) { aux_img=i; clean=False; }

	// Get the auxiliary channel image:
	Image	*get_aux_image() { return aux_img; }

	// Free the auxiliary channel image:
	void	free_aux_img() { if(aux_img) delete aux_img; aux_img=NULL; clean=True; }

	int	read_data(FILE *f=NULL);
	int	write_data(FILE *f=NULL);

	int	type() { return RLA_FILE_ID; }			// class id

	int	set_comments(char *str=NULL) {		// comments implemented
			ImageFile::set_comments(str); return 1; }
	int	add_comments(char *str=NULL) {
			ImageFile::add_comments(str); return 1; }

	RLAFile(char *fname=NULL) : ImageFile(fname) { aux_img=NULL; clean=True; }
	~RLAFile() { if(clean) free_aux_img(); }
	};

#endif // _RLAFILE_H_
