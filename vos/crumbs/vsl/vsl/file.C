// file.C
//
// Written by Dave Kagels 10/17/94

#include <stdlib.h>
#include "file.h"


// Delete file fname (I)
// Return success status
int File::delete_file(char *fname)
{
if(fname) set_filename(fname);
#ifdef SUN4
return 0;				// NOT YET IMPLEMENTED
#else
return (!remove(filename));
#endif
}


// Open file with specified mode (I) using file pointers
// Return error code
int File::fopen(int m)
{
error=FileErr_None;
mode=m;
method=1;
fd=-1;

if(!filename) { error=FileErr_NoFilename; fp=NULL; return error; }

switch(mode) {
    //make sure file permissions are set right
    umask(002);
	case FILE_READ:		fp=::fopen(filename,"r");	break;
	case FILE_WRITE:	fp=::fopen(filename,"w");	break;
	case FILE_APPEND:	fp=::fopen(filename,"a");	break;
	case FILE_READ_PLUS:	fp=::fopen(filename,"r+");	break;
	case FILE_WRITE_PLUS:	fp=::fopen(filename,"w+");	break;
	case FILE_APPEND_PLUS:	fp=::fopen(filename,"a+");	break;
    	default:
		error=FileErr_BadMode;
		fp=NULL;
		return error;
	}
if(!fp) error=FileErr_CouldNotOpen;

return error;
}


// Open file with specified mode (I) using integer file descriptors
// Return error code
int File::iopen(int m)
{
error=FileErr_None;
mode=m;
method=0;
fp=NULL;

if(!filename) { error=FileErr_NoFilename; fd=-1; return error; }

switch(mode) {
	case FILE_READ:		fd=::open(filename,O_RDONLY);					break;
	case FILE_WRITE:	fd=::open(filename,O_WRONLY|O_TRUNC|O_CREAT,(mode_t)420);	break;
	case FILE_APPEND:	fd=::open(filename,O_WRONLY|O_APPEND|O_CREAT,(mode_t)420);	break;
	case FILE_READ_PLUS:	fd=::open(filename,O_RDWR);					break;
	case FILE_WRITE_PLUS:	fd=::open(filename,O_RDWR|O_TRUNC|O_CREAT,(mode_t)420);		break;
	case FILE_APPEND_PLUS:	fd=::open(filename,O_RDWR|O_APPEND|O_CREAT,(mode_t)420);	break;
	default:
		error=FileErr_BadMode;
		fd=-1;
		return error;
	}
if(fd==-1) error=FileErr_CouldNotOpen;

return error;
}


int File::open(int m)
{
fp=NULL;
iopen(m);
method=1;
if(error) return error;

switch(mode) {
	case FILE_READ:		fp=::fdopen(fd,"r");	break;
	case FILE_WRITE:	fp=::fdopen(fd,"w");	break;
	case FILE_APPEND:	fp=::fdopen(fd,"a");	break;
	case FILE_READ_PLUS:	fp=::fdopen(fd,"r+");	break;
	case FILE_WRITE_PLUS:	fp=::fdopen(fd,"w+");	break;
	case FILE_APPEND_PLUS:	fp=::fdopen(fd,"a+");	break;
    	default:
		error=FileErr_BadMode;
		fp=NULL;
		return error;
	}
if(!fp) { error=FileErr_CouldNotOpen; ::close(fd); fd=-1; }

return error;
}


int File::fdopen(int m)
{
if(fd==-1) { error=FileErr_CouldNotOpen; return error; }
if(mode && m) { error=FileErr_BadMode; return error; }
if(m) mode=m;

error=FileErr_None;

switch(mode) {
	case FILE_READ:		fp=::fdopen(fd,"r");	break;
	case FILE_WRITE:	fp=::fdopen(fd,"w");	break;
	case FILE_APPEND:	fp=::fdopen(fd,"a");	break;
	case FILE_READ_PLUS:	fp=::fdopen(fd,"r+");	break;
	case FILE_WRITE_PLUS:	fp=::fdopen(fd,"w+");	break;
	case FILE_APPEND_PLUS:	fp=::fdopen(fd,"a+");	break;
    	default:
		error=FileErr_BadMode;
		fp=NULL;
		return error;
	}
if(!fp) error=FileErr_CouldNotOpen;
else	method=1;

return error;
}


void File::attach(int ifd, FILE *ifp)
{
fd=ifd;
fp=ifp;
mode=0;
if(ifp)	method=1;
else	method=0;
}


void File::attach(FILE *ifp)
{
fd=-1;
fp=ifp;
mode=0;
method=1;
}


// Destructor: close file if it's open
File::~File() { close(); }
