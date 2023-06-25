// tile.C	cuts images up into subimages
//
// Written by John Wright 02/14/00

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include "image/geoimage.h"
#include "image/filetypes.h"
#include "image/types/all.h"
#include "image/datamod.h"


char *dtype[11]={
	"Unknown", "Bit", "Signed Byte", "Byte", "Signed Half", "Half",
	"Signed Word", "Word", "Real", "Double", "Complex" };


main(int argc, char **argv)
{
GeoImage	img;
GeoImage	timg,intimg;
ImageData	*id;
int	inum,flag,i,j,k,l,err,x,y,b,tilesx=0, tilesy=0;
char	*oname, *iname;
int	guess=0,overlap=0,info=0,wx=0,wy=0,inter=0,datatype=0;
int	extra_x, extra_y, twx, twy, trim=0;
double	cx=0.0,cy=0.0,iz=1.0,ar=1.0;
FILE	*ofp;
int	tlblsize, lblsize;
char	*header;
//   \t-guess\t\tsuggest appropriate tiling parameters\n\

char	*usage = {"Options:\n\
\t-info\t\tshow image information\n\
\t-corner X Y\tplace image position X,Y at corner\n\
\t-zoom Z\t\tresample image with a zoom factor of Z (NOT Implemented)\n\
\t-size X Y\tset size of output tiles to X by Y pixels\n\
\t-tiles w h\tset number of tiles across width and height\n\
\t-pixels n\tnumber of pixels overlap between tiles(default=0)\n\
\t-trim    \toutput smaller tiles at ends of rows and columns rather than blank fill\n\
\t-data type\toutput data type to convert to (see supported)\n\
\t-aspect X\thorizontal aspect ratio (magnification) (NOT Implemented)\n\
\t-inter\t\tturn image pixel interpolation on (NOT Implemented)\n\
\t-aux\t\tuse auxiliary channels of RLA image (NOT Implemented)\n\
\n"};
char	*description = {"\
This tile program was developed to assist in tiling large image files into\n\
smaller rectangular tiles.  The tiles may overlap in pixel space.  Tiles may\n\
be specified by the desired number across the image, horizontally and vertically,\n\
or by tile size given width and height.  The specified overlap will be applied in\n\
either case.  It is possible to cut out subportions of the image by specifying\n\
both tile size and number of tiles with an optional specification of the upper\n\
left corner of the subregion to extract.\n\
\n\
Normally, this function will blank fill any tiles which exceed the boundaries of\n\
the original image.  Specifying the trim option will prevent that.  The info\n\
option may be specified to get info about the image and how the tiling will\n\
proceed.\n\
\n\
Output filenames will be of the form input_filename.xxxx.yyyy where xxxx and yyyy\n\
are four digit values, with leading zeroes, specifying the tile's position within\n\
the extracted block.  These values range from 0 to number of tiles specified in\n\
each direction.  If the user needs more than 10000 tiles in each direction, too bad.\n\
\n\
For comments, suggestions, and bug reports: john.r.wright@jpl.nasa.gov\n\
"};



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
		printf("  Actually only VICAR halfword and byte output supported!!!\n");
		printf("\nOutput data types supported for conversion to:\n");
		for(i=1; i<11; i++) {
			printf("\t%2d. %s\n",i,dtype[i]);
		}
		exit(0);
		}
	}

inum=1;
while(inum<argc && argv[inum][0]=='-') {
	if(!strncmp(argv[inum],"-aux",4))
		RLA_Load=RLA_AUX_ONLY;
	else if(!strncmp(argv[inum],"-g",2)) {
		guess=1;
		info=1; }
	else if(!strncmp(argv[inum],"-inf",4))
		info=1;
	else if(!strncmp(argv[inum],"-trim",5))
		trim=1;
	else if(!strncmp(argv[inum],"-c",2) && inum+2<argc) {
		cx=atof(argv[++inum]); cy=atof(argv[++inum]); }
	else if(!strncmp(argv[inum],"-z",2) && inum+1<argc) {
		iz=atof(argv[++inum]); if(iz<=0.0) iz=1.0; }
	else if(!strncmp(argv[inum],"-s",2) && inum+2<argc) {
		wx=atoi(argv[++inum]); wy=atoi(argv[++inum]); }
	else if(!strncmp(argv[inum],"-p",2) && inum+1<argc) {
		overlap=atoi(argv[++inum]); if(overlap<0) overlap=0; }
	else if(!strncmp(argv[inum],"-a",2) && inum+1<argc) {
		ar=atof(argv[++inum]); if(ar<=0.0) ar=1.0; }
	else if(!strncmp(argv[inum],"-t",2) && inum+2<argc) {
		tilesx = atoi(argv[++inum]); tilesy = atoi(argv[++inum]); }
	else if(!strncmp(argv[inum],"-i",2))
		inter=1;
	else if(!strncmp(argv[inum],"-d",2) && inum+1<argc) {
		datatype = atoi(argv[++inum]); }
	else fprintf(stderr,"Skipping unknown option: %s\n",argv[inum]);
	inum++;
	}

if(inum==argc-1) flag=0;
else	flag=1;

if(flag) {
	printf("Usage: %s [options] in_image\n",argv[0]);
	printf("       %s -supported\n\n",argv[0]);
	printf(usage);
	printf("\n");
	printf(description);
	exit(1);
	}

if(info) printf("Reading input file %s\n",argv[inum]);
err=img.read(argv[inum]);
iname = strdup(argv[inum]);
if(err) {
	fprintf(stderr,"Error reading input file %s\n",argv[inum]);
	exit(2);
	}

img.get_res(&x,&y,&b);
if(info) {
	printf("File type: %d %s\n",img.get_file_type(),file_type_name[img.get_file_type()]);
	printf("Image size: %d x %d\n",x,y);
	printf("Number of bands: %d\n",b);
	printf("Data type: %d = %s\n",img.preferred_data_type(), dtype[img.preferred_data_type()]);
	}

id = img.get_data();
if(!id) exit(10);


if(wx > 0 && tilesx <= 0) {	// use tile width for sampling in X
	tilesx = (int)ceil((double)(x-cx-overlap)/(double)(wx-overlap));
} else if(tilesx > 0 && wx <= 0) {
	wx = (int)ceil((double)(x-cx-overlap)/(double)tilesx)+overlap;
} else if(wx <= 0 && tilesx <= 0) {
	printf(usage);
	printf("\n");
	printf(description);
	exit(10);
}
if(wy > 0 && tilesy <= 0) {	// use tile width for sampling in Y
	tilesy = (int)ceil((double)(y-cy-overlap)/(double)(wy-overlap));
} else if(tilesy > 0 && wy <= 0) {
	wy = (int)ceil((double)(y-cy-overlap)/(double)tilesy)+overlap;
} else if(wy <= 0 && tilesy <= 0) {
	printf(usage);
	printf("\n");
	printf(description);
	exit(10);
}
extra_x = (tilesx*(wx-overlap) + overlap + cx) - x;
extra_y = (tilesy*(wy-overlap) + overlap + cy) - y;

if(info) {
	printf("\nInput tiling parameters will have the following characteristics:\n");
	printf("Number of tiles across image = %d\n", tilesx);
	printf("Width of tiles across image  = %d\n", wx);
	printf("Number of tiles down image = %d\n", tilesy);
	printf("Height of tiles down image = %d\n", wy);
	printf("Overlap area of tiles = %d pixels\n", overlap);
	if(!trim) {
		printf("Number of extra pixels added to fill right side of last tiles in row = %d\n", extra_x);
		printf("Number of extra pixels added to fill bottom of last tiles in column  = %d\n", extra_y);
	}
}

if(guess) {
	if(extra_x) {
		printf("For the same number of tiles across the image:\n");
		for(i=1; i<1000; i++) {
			if(tilesx*((wx+i)-(overlap+2*i)) + overlap + 2*i + cx -x == 0) {
				printf("A tile width of %d and an overlap of %d will avoid extra fill pixels.\n",
					wx+i, overlap+2*i);
				break;
			}
		}
		for(i=-1; i>-wx; i--) {
			if(tilesx*((wx+i)-(overlap+2*i)) + overlap + 2*i +cx -x == 0) {
				printf("A tile width of %d and an overlap of %d will avoid extra fill pixels.\n",
					wx+i, overlap+2*i);
				break;
			}
		}
	} else {
		printf("The width tiling parameters are good.\n");
	}
	if(extra_y) {
	} else {
		printf("The height tiling parameters are good.\n");
	}
	exit(0);
}
		

if(datatype <= 0 || datatype >= 11) datatype = img.preferred_data_type();
oname = (char *)malloc(strlen(iname) + 64);
header = (char *)calloc(2048, 1);
lblsize=9999;
sprintf(header,"LBLSIZE=%d  FORMAT='INTEGER' TYPE='IMAGE' NL=%d NS=%d NB=1",lblsize,wy,wx);
lblsize = wx * (int)ceil(strlen(header)/(double)wx);
for(i=0; i<tilesy; i++) {
	for(j=0; j<tilesx; j++) {
		// open file
		sprintf(oname,"%s.%04d.%04d",iname,j,i);
		ofp = fopen(oname,"w");
		if(!ofp) {
			fprintf(stderr,"Whoops - Unable to open output file %s\n",oname);
			exit(10);
		}
		// test for reduced size of last tile
		if(trim && j==tilesx-1) {
			twx = wx - extra_x;
		} else {
			twx = wx;
		}
		if(trim && i==tilesy-1) {
			twy = wy - extra_y;
		} else {
			twy = wy;
		}
		

		// process according to output data type
		switch ( datatype ) {

		case 3:		// unsigned bytes
			tlblsize = twx * (int)ceil(strlen(header)/(double)twx);
			bzero(header,2048);
			sprintf(header,"LBLSIZE=%d  FORMAT='BYTE' TYPE='IMAGE' NL=%d NS=%d NB=1",tlblsize,twy,twx);
			fwrite(header, tlblsize, 1, ofp);
			{	// need block for local vars in switch
			unsigned char *btmp = (unsigned char *)malloc(wx * sizeof(unsigned char));
			for(k=cy+i*(wy-overlap); k<cy+i*(wy-overlap)+twy; k++) {
				for(l=0; l<twx; l++) {
					// output pixel value
					btmp[l] = id->get_uchar(l+(int)cx+j*(wx-overlap),k);
				}
				fwrite(btmp, twx, sizeof(unsigned char), ofp);
			}
			free(btmp);
			}
			break;
			

		case 4:		// signed short values
			// output header
			tlblsize = twx * (int)ceil(strlen(header)/(double)twx);
			bzero(header,2048);
			sprintf(header,"LBLSIZE=%d  FORMAT='HALF' TYPE='IMAGE' NL=%d NS=%d NB=1",tlblsize,twy,twx);
			fwrite(header, tlblsize, 1, ofp);
			{	// need block for local vars in switch
			short *stmp = (short *)malloc(wx * sizeof(short));
			for(k=cy+i*(wy-overlap); k<cy+i*(wy-overlap)+twy; k++) {
				for(l=0; l<twx; l++) {
					// output pixel value
					stmp[l] = id->get_short(l+(int)cx+j*(wx-overlap),k);
				}
				fwrite(stmp, twx, sizeof(short), ofp);
			}
			free(stmp);
			}
			break;

		default:
			fprintf(stderr,"Whoops - Unable to output data of type %d or %s\n", datatype, dtype[datatype]);

		};
		// close file
		fclose(ofp);
	}
}

}
