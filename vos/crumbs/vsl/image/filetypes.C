// filetypes.C
//
// Written by Dave Kagels 10/18/94

#include "image/filetypes.h"
#include "image/imagefile.h"

#include "image/types/all.h"

#include <string.h>
#include <stdlib.h>

char *file_type_name[NUM_FILE_TYPES] = {
	(char *)"Base Class",
	(char *)"PPM",
	(char *)"MOD",
	(char *)"Vicar Byte",
	(char *)"Vicar Half",
	(char *)"Vicar Real",
	(char *)"RLA",
	(char *)"Data Server",
	(char *)"PGM",
	(char *)"KAP",
	(char *)"BSB (unimplemented)",
	(char *)"PDS",
	(char *)"ASVC",
	(char *)"RAN",
	(char *)"RGB",
	};

int file_type_access[NUM_FILE_TYPES] = {
	0,					// Base Class
	FILE_READ|FILE_WRITE,			// PPM
	FILE_READ,				// MOD
#ifndef _NO_MMAP_
	FILE_READ|FILE_ONLY|FILE_WRITE,		// Vicar Byte (mmap)
	FILE_READ|FILE_ONLY|FILE_WRITE,		// Vicar Half (mmap)
	FILE_READ|FILE_ONLY|FILE_WRITE,		// Vicar Real (mmap)
#else
	FILE_READ|FILE_WRITE,			// Vicar Byte (no mmap)
	FILE_READ|FILE_WRITE,			// Vicar Half (no mmap)
	FILE_READ|FILE_WRITE,			// Vicar Real (no mmap)
#endif
	FILE_READ|FILE_WRITE,			// RLA
	FILE_READ|FILE_ONLY,			// Data Server
	FILE_READ|FILE_WRITE,			// PGM
	FILE_READ|FILE_WRITE,			// KAP
	FILE_READ|FILE_WRITE,			// BSB
	FILE_READ|FILE_WRITE,			// PDS
	FILE_READ|FILE_WRITE,			// ASVC
	FILE_READ|FILE_WRITE,			// RAN
	FILE_READ|FILE_WRITE,			// SGI RGB
	};


ImageFile *new_file_by_type(int type, char *fname)
{
switch(type) {
	case	PPM_FILE_ID:	return new PPMFile(fname);
	case	MOD_FILE_ID:	return new MODFile(fname);
	case	VIC_BYTE_FILE_ID:	return new VicByte(fname);
	case	VIC_HALF_FILE_ID:	return new VicHalf(fname);
	case	VIC_REAL_FILE_ID:	return new VicReal(fname);

	case	RLA_FILE_ID:	return new RLAFile(fname);
#ifndef _NO_DATASERVER_
	case	DS_FILE_ID:	return new DSFile(fname);
#else
	case	DS_FILE_ID:	return NULL;
#endif // _NO_DATASERVER_
	case	PGM_FILE_ID:	return new PGMFile(fname);
	case	KAP_FILE_ID:	return new KAPFile(fname);
	case	PDS_FILE_ID:	return new PDSFile(fname);
	case	ASVC_FILE_ID:	return new ASVCFile(fname);
	case	RAN_FILE_ID:	return new RANFile(fname);
	case	RGB_FILE_ID:	return new RGBFile(fname);
	}
return NULL;
}


ImageFile *get_vicar_type(char *buf, char *fname)
{
char	 *ptr;

ptr=strstr(buf,"FORMAT=");
if(!ptr) return NULL;
if(!strncmp(ptr+7,"'BYTE'",6)) return new VicByte(fname);
if(!strncmp(ptr+7,"'HALF'",6)) return new VicHalf(fname);
if(!strncmp(ptr+7,"'REAL'",6)) return new VicReal(fname);

return NULL;
}


ImageFile *get_new_file_type(char *fname)
{
char	buf[256],*ptr;
File	f(fname);
short	*s;
int	i;
double	d;

if(f.open(FILE_READ)) {	// open failed:
#ifndef _NO_DATASERVER_
	if(!strncmp(fname,"ds:",3))				// Data Server
		return new DSFile(fname);
#endif // _NO_DATASERVER_
	if(ptr=strchr(fname,',')) {				// Multiple VICAR
		*ptr=(char)0;
		f.set_filename(fname);
		if(f.open(FILE_READ)) { *ptr=','; return NULL; }
		f.read(buf,256);
		if(!strncmp(buf,"LBLSIZE=",8)) { *ptr=',';
			buf[255]=(char)0;
			return get_vicar_type(buf,fname); }
		*ptr=',';
		}
	return NULL;			// couldn't open
	}

f.read(buf,256);
f.close();

s=(short *)buf;
if(*s==8093 && strlen(fname)>2) {
#ifndef _NO_PIPES_
	if(!strcmp(fname+strlen(fname)-2,".Z")) {	    // Compressed file
		sprintf(buf,"zcat %s",fname);
		f.attach(popen(buf,"r"));
		if(!f.get_fp()) return NULL;
		f.read(buf,256);
		pclose(f.get_fp());
		f.attach((FILE *)NULL);
		}
#endif // _NO_PIPES_
	}
	
if (!strcasecmp(fname+strlen(fname)-4,".ran"))
	return new RANFile(fname);

if(!strncmp(buf,"P6",2)) return new PPMFile(fname);			// PPM
if(!strncmp(buf,"P5",2)) return new PGMFile(fname);			// PGM
if(!strncmp(buf,"ASVC",4)) return new ASVCFile(fname);			// ASVC
int p=0;  while(p<246 && buf[p] == ' ')p++;  // skip leading spaces
//!!!!ozp if(!strncmp(buf,"CCSD",4) || !strncmp(buf,"PDS_",4)) return new PDSFile(fname);			// PDS
if(!strncmp(buf,"CCSD",4) || !strncmp(buf, "PDS_", 4) || !strncmp(buf,"ODL_",4)) return new PDSFile(fname);			// PDS
if(!strncmp(&(buf[p]),"!Copyright",10)) {	// KAP or BSB
	if(strstr(fname,"CHT")) {			// BSB
	} else {					// KAP
		return new KAPFile(fname);
	}
}

if(!strncmp(buf,"I~",2)) return new MODFile(fname);			// MOD

if(!strncmp(buf,"LBLSIZE=",8)) {					// VICAR
	buf[255]=(char)0;
	return get_vicar_type(buf,fname);
	}

if ((uchar)buf[0] == 0x01 && (uchar)buf[1] == 0xDA) return new RGBFile(fname);	// RGB

s=(short *)(buf+20);							// RLA
if(*s>0 && *s<4) {
	s=(short *)(buf+22);
		if(*s>=0 && *s<4) {
		for(i=29;i<44;i++) if(buf[i]==(char)0) break;
		if(i<44) {
			d=atof(buf+28);
			if(d>0.0 && d<20.0) return new RLAFile(fname);
			}
		}
	}

return NULL;
}
