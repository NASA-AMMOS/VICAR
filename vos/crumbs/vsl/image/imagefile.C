// imagefile.C
//
// Written by Dave Kagels 11/1/95

#include "image/imagefile.h"

int	IMAGE_COMPRESSION = WARN_COMPRESSION;	    // image compression flag


int ImageFile::read_image(char *fname)
{
int	e;
char  buf[256];

reset_error();
if(fname) set_filename(fname);
free_comments(); free_flags();
if(open(FILE_READ)) return error;
if(strlen(filename)>2) {
#ifndef _NO_PIPES_
	if(!strcmp(filename+strlen(filename)-2,".Z")) {	    // Compressed file
		close();
		sprintf(buf,"zcat %s",filename);
		e=read_data(popen(buf,"r"));
		if(fp) pclose(fp);
		return error=e;
		}
#endif // _NO_PIPES_
	}
e=read_data();
close();
return error=e;
}


int ImageFile::write_image(char *fname)
{
int e;
char  buf[256];

reset_error();
if(fname) set_filename(fname);
if(open(FILE_WRITE)) return error;
if(strlen(filename)>2 && IMAGE_COMPRESSION) {
#ifndef _NO_PIPES_
	if(!strcmp(filename+strlen(filename)-2,".Z")) {	    // Compressed file
		close();
		if(IMAGE_COMPRESSION==WARN_COMPRESSION)
			fprintf(stderr,"Warning:  Image file %s is being saved as a compressed file.\n",filename);
		sprintf(buf,"compress -f > %s",filename);
		e=write_data(popen(buf,"w"));
		if(fp) pclose(fp);
		return error=e;
		}
#endif // _NO_PIPES_
	}
e=write_data();
close();
return error=e;
}
