// modfile.h
//
// Written by Dave Kagels 1/26/95

#ifndef _MODFILE_H_
#define _MODFILE_H_

#include "image/imagefile.h"

class Image;


class MODFile : public ImageFile {

    protected:

	Image	*root;

	char	**name_list;
	Image	**img_list;
	int	num;

	Image	*scan_image();
	Image	*find_image(char *);
	void	new_name(char *, Image *);
	void	free_list();

	int	find_char(int ch);
	int	read_block(Image *i);
	int	read_params(Image *i);

	int	block_file(Image *i);
	int	block_black(Image *i);
	int	block_transform(Image *i);
	int	block_composite(Image *i);
	int	block_color(Image *i);
	int	block_multiband(Image *i);
	int	block_ramp(Image *i);
	int	block_pyramid(Image *i);
	int	block_fade(Image *i);
	int	block_crop(Image *i);
	int	block_pyrpix(Image *i);

    public:

	int	read_data(FILE *f=NULL);
	int	write_data(FILE *f=NULL);

	int	type() { return MOD_FILE_ID; }			// class id

	MODFile(char *fname=NULL) : ImageFile(fname) {
		name_list=NULL; img_list=NULL; root=NULL; num=0; }
	};

#endif // _MODFILE_H_
