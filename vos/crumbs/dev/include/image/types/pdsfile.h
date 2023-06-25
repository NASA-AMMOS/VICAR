// Class PDSFile
// Created by John Wright
// Created Mon Feb  9 16:50:47 1998


#ifndef _PDSFile_H_
#define _PDSFile_H_

#include "image/imagefile.h"
#include "image/geoimage.h"
#include "image/types/lablib3.h"	// symbolic link in image/types to cots sw location
#include "image/types/pds_geodata.h"   //will be new geodata type for pds
#include "image/types/camera_geodata.h"   //will be new geodata type for camera models

//extern class Image;
class Image;	// removed extern to conform to new compiler std for forward declarations

class PDSFile:public ImageFile {

    private:

    protected:

	OBJDESC	*label;			// PDS object pointer for label
	OBJDESC	*image_obj;		// PDS object pointer for image
	OBJDESC	*georef_obj;		// PDS object pointer for georeferencing block
	char	*err_file;		// filename of error log file (/dev/null)
	char	*external_file;		// filename of detached data file

	char	*get_key_value(OBJDESC *obj, const char *key_name) {
		if(!obj)return(NULL);
		KEYWORD *key=OdlFindKwd(obj,(char *)key_name,
						NULL,0,ODL_RECURSIVE_DOWN);
		char *local=OdlGetKwdValue(key);
		return(local);
	}

    public:

	int	read_image(char *fname=NULL);
	int	write_image(char *fname=NULL);
	int	read_header(char *fname=NULL);
	int	read_data(FILE *f=NULL);
	int	write_data(FILE *f=NULL);

	int	type() { return PDS_FILE_ID; }			// class id

	int	set_comments(char *str=NULL) {		// comments implemented
			ImageFile::set_comments(str); return 1; }
	int	add_comments(char *str=NULL) {
			ImageFile::add_comments(str); return 1; }
	void	set_err_file(char *fn) {
			if(err_file)free(err_file);
			err_file = strdup(fn);
		}
	char	*get_err_file(void) { return(err_file); }

	// lookup keyword value in image label with optional block specified
	char	*get_value(const char *key_name, const char *block_name=NULL) {
		if(!block_name) {
			return get_key_value(label, (char *)key_name);
		} else {
			OBJDESC *local_obj = OdlFindObjDesc (label, 
				(char *)block_name, NULL, NULL, 1, 
				ODL_RECURSIVE_DOWN);
			if(!local_obj) return(NULL);
			return(get_key_value(local_obj, (char *)key_name));
		}
	}

    // Constructors

	PDSFile(char *fname=NULL) : ImageFile(fname) { 
		err_file = strdup("/dev/null"); 
		external_file = NULL;
		label = NULL;
		image_obj = NULL;
	}
	PDSFile(char *fname, char *efname) : ImageFile(fname) { 
		if(efname) {
			err_file = strdup(efname);
		} else {
			err_file = strdup("/dev/null"); 
		}
		external_file = NULL;
		label = NULL;
		image_obj = NULL;
	}

    // Destructor

	~PDSFile() { 
		if(err_file)free(err_file); 
		if(label)OdlFreeTree(label);
	}

};


#endif
