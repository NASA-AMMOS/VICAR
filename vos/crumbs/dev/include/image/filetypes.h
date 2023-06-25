// filetypes.h
//
// Written by Dave Kagels 10/18/94

#ifndef _FILETYPES_H_
#define _FILETYPES_H_

#include <stdio.h>

//extern class ImageFile;
class ImageFile;	// removed extern to conform to new compiler std for forward declarations


// Bit flags for file access:	( FILE_READ & FILE_WRITE defined in file.h )

//	FILE_READ	1	// File type read supported
//	FILE_WRITE	2	// File type write supported
#define	FILE_ONLY	4	// File not loaded into memory (ex: mmap)

// id's of all supported file types
#define BASE_FILE_ID 0
#define PPM_FILE_ID 1
#define MOD_FILE_ID 2
#define VIC_BYTE_FILE_ID 3
#define VIC_HALF_FILE_ID 4
#define VIC_REAL_FILE_ID 5
#define RLA_FILE_ID 6
#define DS_FILE_ID 7
#define PGM_FILE_ID 8
#define KAP_FILE_ID 9
#define BSB_FILE_ID 10
#define PDS_FILE_ID 11
#define ASVC_FILE_ID 12
#define RAN_FILE_ID 13
#define RGB_FILE_ID 14

// Number of currently supported file types:
#define	NUM_FILE_TYPES	15

// Array of file type names:
extern char *file_type_name[];

// Array of file type access flags:
extern int file_type_access[];

ImageFile *new_file_by_type(int type, char *fname=NULL);

ImageFile *get_new_file_type(char *fname);

#endif // _FILETYPES_H_
