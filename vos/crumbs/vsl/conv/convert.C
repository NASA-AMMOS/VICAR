// convert.C	converts image file formats
//
// Written by Dave Kagels 10/27/95

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "image/geoimage.h"
#include "image/filetypes.h"
#include "image/types/all.h"
#include "image/datamod.h"


char *dtype[11]={
	"Unknown", "Bit", "Signed Byte", "Byte", "Signed Half", "Half",
	"Signed Word", "Word", "Real", "Double", "Complex" };


int main(int argc, char **argv)
{
int	inum,flag,i,err,x,y,b,type;
char	*oname;
int	info=0,wx=0,wy=0,write=0,inter=0;
double	cx=0.0,cy=0.0,iz=1.0,ar=1.0;

RLA_Load=RLA_NO_AUX;

if(argc==2) {
	if(!strncmp(argv[1],"-sup",4)) {
		printf("File types supported for read (input):\n");
		for(i=1;i<NUM_FILE_TYPES;i++) {
			if(file_type_access[i]&FILE_READ)
				printf("\t%2d. %s\n",i,file_type_name[i]);
			}
		printf("\nFile types supported for write (output):\n");
		for(i=1;i<NUM_FILE_TYPES;i++) {
			if(file_type_access[i]&FILE_WRITE)
				printf("\t%2d. %s\n",i,file_type_name[i]);
			}
		exit(0);
		}
	}

inum=1;
while(inum<argc && argv[inum][0]=='-') {
	if(!strncmp(argv[inum],"-aux",4))
		RLA_Load=RLA_AUX_ONLY;
	else if(!strncmp(argv[inum],"-inf",4))
		info=1;
	else if(!strncmp(argv[inum],"-c",2) && inum+2<argc) {
		cx=atof(argv[++inum]); cy=atof(argv[++inum]); }
	else if(!strncmp(argv[inum],"-z",2) && inum+1<argc) {
		iz=atof(argv[++inum]); if(iz<=0.0) iz=1.0; }
	else if(!strncmp(argv[inum],"-s",2) && inum+2<argc) {
		wx=atoi(argv[++inum]); wy=atoi(argv[++inum]); }
	else if(!strncmp(argv[inum],"-a",2) && inum+1<argc) {
		ar=atof(argv[++inum]); if(ar<=0.0) ar=1.0; }
	else if(!strncmp(argv[inum],"-i",2))
		inter=1;
	else fprintf(stderr,"Skipping unknown option: %s\n",argv[inum]);
	inum++;
	}

if(inum==argc-1 || inum==argc-3) flag=0;
else	flag=1;
if(inum==argc-3) write=1;

if(flag) {
	printf("Usage: %s [options] in_image [out_image file_type_number]\n",argv[0]);
	printf("       %s -supported\n\n",argv[0]);
	printf("Options:\n\t-info\t\tshow image information\n\t-corner X Y\tplace image position X,Y at corner\n\t-zoom Z\t\tresample image with a zoom factor of Z\n\t-size X Y\tset size of output image to X by Y pixels\n\t-aspect X\thorizontal aspect ratio (magnification)\n\t-inter\t\tturn image pixel interpolation on\n\t-aux\t\tuse auxiliary channels of RLA image\n\n");
	exit(1);
	}

if(write) {
	type=atoi(argv[argc-1]);
	if(type<1 || type>=NUM_FILE_TYPES) {
		fprintf(stderr,"Invalid file type number: %s\n",argv[argc-1]);
		exit(3);
		}
	if(!(file_type_access[type]&FILE_WRITE)) {
		fprintf(stderr,"Cannot write files of type %d (%s)\n",type,file_type_name[type]);
		exit(3);
		}
	}

if(info) printf("Reading input file %s\n",argv[inum]);
Image *iptr = new Image;
err=iptr->read(argv[inum]);
if(err) {
	fprintf(stderr,"Error reading input file %s\n",argv[inum]);
	exit(2);
	}

iptr->get_res(&x,&y,&b);
if(info || !write) {
	printf("File type: %d. %s\n",iptr->get_file_type(),file_type_name[iptr->get_file_type()]);
	printf("Image size: %d x %d\n",x,y);
	printf("Number of bands: %d\n",b);
	printf("Data type: %s\n",dtype[iptr->preferred_data_type()]);
	}

if(!write) exit(0);

if(wx || wy || cx || cy || iz!=1.0 || ar!=1.0) {
	Image *timg = new Image;
	*timg = new transData(iptr,-cx*iz*ar,-cy*iz,iz*ar,0.0,0.0,iz);
	if(!wx) wx=(int)(((double)x-cx)*iz*ar);
	if(!wy) wy=(int)(((double)y-cy)*iz);
	timg->allocate(wx,wy,b);
	timg->set_level(0.0);
	iptr=timg;
	}

if(inter) {
	Image *intimg = new Image;
	*intimg = new interpData(iptr);
	iptr=intimg;
	}

oname=argv[inum+1];

if(info) printf("Writing output file %s of type %d (%s)\n",oname,type,file_type_name[type]);
err=iptr->write(oname,type);
if(err) {
	fprintf(stderr,"Error writing output file %s\n",oname);
	exit(4);
	}
exit(0);
}
