// file.h
//
// Written by Dave Kagels 10/17/94

#ifndef _FILE_H_
#define _FILE_H_

#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <string>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <stdlib.h>
// !!!! -ozp
// This is a change needed for building on a mac. 
// Mac platform doesn't have malloc.h. The change is to include stdlib.h. 
// That's where malloc() should be defined according to the man pages.
//#include <malloc.h>
//#include "machine_setup.h"


// Error codes:

#define	FileErr_None			0
#define FileErr_Overload		1
#define	FileErr_CouldNotOpen		2
#define	FileErr_CouldNotClose		3
#define FileErr_BadMode			4
#define	FileErr_NoFilename		5


// Read/Write mode flags:

// -ozp /usr/local/vicar/dev/rtl/inc/errdefs.h:36:0: note: this is the location of the previous definition
// #define FILE_NOT_OPEN -25
///#define FILE_NOT_OPEN	0
#define FILE_NOT_OPEN		-25
#define FILE_READ		1
#define FILE_WRITE		2
#define FILE_APPEND		3
#define FILE_READ_PLUS		4
#define FILE_WRITE_PLUS		5
#define	FILE_APPEND_PLUS	6


// This is the base class for accessing files on disk.
// It has member functions for opening, closing, and deleting files.
// Member functions are provided for accessing the file data.
// Both UNIX file descriptors and C FILE pointers can be used.
// An error code is kept for later access if a problem is encountered.

class File {

    protected:

	char	*filename;		// stores the filename
	int	error;			// stores the error status
	int	fd;			// stores the file descriptor
	FILE	*fp;			// stores the file pointer
	int	mode;			// mode used for opening
	int	method;			// file access method (fd=0, fp=1)

	// Initialize member variables:
	void	init() {	filename=NULL;
				fd=-1; fp=NULL;
				reset_error();
				mode=FILE_NOT_OPEN;
				method=1; }

    public:

	// Return the error code
	int	get_error() { return error; }

	// Reset the error code to zero
	void	reset_error() { error=FileErr_None; }

	// Free storage used by the filename
	void	free_filename() { if(filename) free(filename); filename=NULL; }

	// Set the filename to fname
	void	set_filename(char *fname) { free_filename();
			filename=(char *)malloc(strlen(fname)+1);
			strcpy(filename,fname); }

	// Append to the filename (extensions)
	void	append_filename(char *s) { int i;
			if(filename) i=strlen(filename); else i=0;
			filename=(char *)realloc(filename,strlen(s)+i+1);
			strcpy(filename+i,s); }

	// Return the filename, copy into fname if given
	char	*get_filename(char *fname=NULL) {
			if(fname && filename) strcpy(fname,filename);
			return filename; }

	// Return the file descriptor
	int	get_fd() { return fd; }

	// Return the file pointer
	FILE	*get_fp() { return fp; }

	// Get open mode/method
	int	get_mode() { return mode; }
	int	get_method() { return method; }

	int	get_size(void) {
			struct stat stat_struct;
			if(fd && mode != FILE_NOT_OPEN) {
				fstat(fd, &stat_struct);
				return(stat_struct.st_size);
			} else {
				error = FileErr_BadMode;
				return(0);
			}
		}

	long int get_size64(void) {
#ifdef LINUX
			struct stat stat_struct;
#else
			struct stat64 stat_struct;
#endif
			if(fd && mode != FILE_NOT_OPEN) {
#ifdef LINUX
				fstat(fd, &stat_struct);
#else
				fstat64(fd, &stat_struct);
#endif
				return(stat_struct.st_size);
			} else {
				error = FileErr_BadMode;
				return(0);
			}
		}

	// Close the file
	virtual void	close() { if(method) { fclose(); if(fd>=0) iclose(); }
				  else	iclose(); }

	// Close the file using file pointer
	void	fclose() { 	if(fp) { ::fclose(fp); error=FileErr_None;
					 fp=NULL; mode=FILE_NOT_OPEN; }
				else error=FileErr_CouldNotClose; }

	// Close the file using file descriptor
	void	iclose() {	if(fd>=0) { ::close(fd); error=FileErr_None;
					    fd=-1; mode=FILE_NOT_OPEN; }
				else error=FileErr_CouldNotClose; }

	// Read n characters into buf
	virtual int	read(void *buf, int n) { if(method) return fread(buf,n);
						 else	return iread(buf,n); }

	// Write n chars from buf
	virtual int	write(void *buf, int n) { if(method) return fwrite(buf,n);
						  else	return iwrite(buf,n); }

	// Read n characters into buf using file pointer,
	// return number of chars read
	int	fread(void *buf, int n) { return ::fread((char *)buf,1,n,fp); }

	// Write n chars from buf using file pointer,
	// return number of chars written
	int	fwrite(void *buf, int n) { return ::fwrite((char *)buf,1,n,fp); }

	// Read n characters into buf using file descriptor,
	// return number of chars read
	int	iread(void *buf, int n) { return ::read(fd,(char *)buf,n); }

	// Write n chars from buf using file descriptor,
	// return number of chars written
	int	iwrite(void *buf, int n) { return ::write(fd,(char *)buf,n); }

	// Seek off chars into the file according to whence, return position
	virtual int	seek(int off, int whence=SEEK_CUR) {
				if(method) return fseek(off,whence);
				else	return iseek(off,whence); }

	// Seek off chars into the file according to whence using
	// file pointer, return position
	int	fseek(int off, int whence=SEEK_CUR) { if(::fseek(fp,off,whence))
			return -1; else return ::ftell(fp); }

	// Seek off chars into the file according to whence using
	// file descriptor, return position
	int	iseek(int off, int whence=SEEK_CUR) { 
			return lseek(fd,off,whence); }

	// Return position in file
	virtual int	tell() { if(method) return ftell();
				 else	return itell(); }

	// Return position in file using file pointer
	int	ftell() { return ::ftell(fp); }

	// Return position in file using file descriptor
	int	itell() { return iseek(0,SEEK_CUR); }

	// Reset the file position to the beginning
	virtual void	rewind() { if(method) frewind(); else irewind(); }

	// Reset the file position to the beginning using file pointer
	void	frewind() { fseek(0,SEEK_SET); }

	// Reset the file position to the beginning using file descriptor
	void	irewind() { iseek(0,SEEK_SET); }

	// Flush the buffer
	virtual int	flush() { if(method) return fflush();
				  else	return iflush(); }

	// Flush the buffer using file pointer
	int	fflush() { return ::fflush(fp); }

	// Flush the buffer using file descriptor
	int	iflush() { return fsync(fd); }

	// Open using mode m
	virtual	int	open(int m);

	// Open using mode m with pointer/descriptor
	int	fopen(int m);
	int	iopen(int m);

	// Open using file pointer when already opened using file descriptor
	int	fdopen(int m=0);

	// Attach to an already opened file
	void	attach(int ifd, FILE *ifp=NULL); // ifd && ifp should be same file
	void	attach(FILE *ifp);

	// Delete file fname
	int	delete_file(char *fname=NULL);

	// Conversion (typecast) operators:
	operator	int() { return fd; }
#ifdef SUN4
	operator	_iobuf*() { return fp; }
#else
	operator	FILE*() { return fp; }
#endif
	operator	char*() { return filename; }

	// Overloaded assignment operators:
	void	operator=(char *fname) { set_filename(fname); }
	void	operator=(int ifd) { attach(ifd); }
	void	operator=(FILE *ifp) { attach(ifp); }

	// Constructor: set filename to fname if given, initialize
	File(char *fname=NULL) { init(); if(fname) set_filename(fname); }

	// Constructor: set filename to fname, open with mode m
	File(char *fname, int m) { init(); set_filename(fname); open(m); }

	// Constructor: attach to open file
	File(int ifd, FILE *ifp=NULL) { init(); attach(ifd,ifp); }
	File(FILE *ifp) { init(); attach(ifp); }

	// Destructor:
	virtual ~File();
	};

#endif // _FILE_H_
