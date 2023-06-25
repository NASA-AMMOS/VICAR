// range2xyz.C 1.13 02/05/15 13:56:44
/** \file
This utility coverts a range image to 3 separate images files: x, y, and z
It depends on the GRAPE image class.

(Normally, rectified XYZ range images will be provided instead.)

Input range data is assumed to be left-handed (Y=0 at top).
Output XYZ maps are also left-handed (to correspond to RGB images).

Author:  Kathy Sturdevant 5-19-99
         Math code stolen from John Wright's "warper" utility.

*/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <fstream.h>
#include <float.h>
#include <math.h>
#include <malloc.h>
#include "image/geoimage.h"
#include "image/datatypes.h"

static const char usage[] = 
"Usage: %s [-v] [-t camera_x camera_y camera_z]\n"
"  [-f fov ] [-r roll pitch yaw] [-s rangescale]\n"
"  -i rangefile -o ranfile\n\n"
"Converts range image to cartesian maps (ranfile).x, .y, and .z.\n"
"-v = verbose output\n"
"-t = camera position (default=0,0,0)\n"
"-r = camera orientation in degrees (default=0,0,0)\n"
"-f = vertical field of view in degrees (default=20)\n"
"-s = optional scale factor for input range data.\n";

#define PI	3.14159265358979

int	verbose;
FILE 	*create_vicar_file( char *);
void	write_vicar_header(FILE *, int, int);
char 	*progname;

int main(int argc, char **argv)
{
	char    *rangename=NULL, *imagename=NULL;
	char    *imagex, *imagey, *imagez;
	GeoImage *rangeimg;
	float	xeye=0.0, yeye=0.0, zeye=0.0, pitch=0.0, roll=0.0, yaw=0.0;
	float	theta, gamma, xscr, yscr, range, ground_range;
	float	temp;
	int     xres, yres;
	float	H, vfov = 20.0*PI/180.0;  // vfov is in radians
	float	k1, k2, k3;
	float	tan_pitch, cos_pitch, sin_pitch;
	float	cos_roll, sin_roll, x_center, y_center;
	float	delta_x, delta_y, delta_z, theta_prime;
	float	rscale = 1.0;
	int	i, j;
	FILE	*fpx, *fpy, *fpz;
	float	*ximg, *yimg, *zimg;

	progname = argv[0];
        for(i=1; i<argc; i++) {
                if(!strcmp(argv[i], "-t")) {
                        xeye = atof(argv[++i]);
                        yeye = atof(argv[++i]);
                        zeye = atof(argv[++i]);
                } else if(!strcmp(argv[i], "-f")) {
                        // Convert fov to radians
                        vfov = atof(argv[++i])*PI/180.0;
                } else if(!strcmp(argv[i], "-r")) {
                        roll  = atof(argv[++i])*PI/180.0;
                        pitch = atof(argv[++i])*PI/180.0;
                        yaw   = atof(argv[++i])*PI/180.0;
		} else if(!strcmp(argv[i], "-s")) {
			rscale = atof(argv[++i]);
                } else if(!strcmp(argv[i], "-i")) {
                        rangename = argv[++i];
                } else if(!strcmp(argv[i], "-o")) {
                        imagename = argv[++i];
                } else if(!strcmp(argv[i], "-v")) {
                        verbose = TRUE;
		} else {
			fprintf(stderr, "Unknown argument %s\n", argv[i]);
			fprintf(stderr, usage, argv[0]);
			return 1;
		}
	}

	if (rangename == NULL || imagename == NULL) {
		fprintf(stderr, "%s: Need to specify -i and -o names\n",
			argv[0]);
		return 1;
	}
	
        // Open range file
        if ((rangeimg = new GeoImage (rangename)) == (GeoImage *) NULL)
        {
                fprintf(stderr, "%s: Could not open range file %s\n", 
                	argv[0], rangename);
                exit(1);
        }

	// Create the image output files <imgfile>.x, <imgfile>.y, <imgfile>.z
	int filelen = strlen(imagename) + 3;
	imagex = (char *)malloc(filelen);
	imagey = (char *)malloc(filelen);
	imagez = (char *)malloc(filelen);
	if (imagex==NULL || imagey==NULL || imagez==NULL) {
                fprintf(stderr, "%s: Could not allocate space for filenames\n",
                	argv[0]);
                exit(1);
	}
	sprintf(imagex, "%s.x", imagename);
	sprintf(imagey, "%s.y", imagename);
	sprintf(imagez, "%s.z", imagename);

	// Get the x and y resolutions from the range file
        if (rangeimg->get_res(&xres, &yres) == -1)
        {
            fprintf(stderr, "%s: Could not get resolution from range file\n",
            	argv[0]);
            exit(1);
        }

	if (verbose)
		fprintf(stderr, "Resolution is %d %d\n", xres, yres);

	// Allocate data space
	ximg = (float *)malloc(xres*yres*sizeof(float));
	yimg = (float *)malloc(xres*yres*sizeof(float));
	zimg = (float *)malloc(xres*yres*sizeof(float));
	if (ximg==NULL || yimg==NULL || zimg==NULL) {
                fprintf(stderr, "%s: Could not allocate space for XYZ data\n",
                	argv[0]);
                exit(1);
	}

	fpx = create_vicar_file(imagex);
	write_vicar_header(fpx, xres, yres);

	fpy = create_vicar_file(imagey);
	write_vicar_header(fpy, xres, yres);
	
	fpz = create_vicar_file(imagez);
	write_vicar_header(fpz, xres, yres);

	// Calculate the x and y centers for roll
	x_center = xres/2.0;
	y_center = yres/2.0;

	// Calculate constants to save time inside the image processing loop
	sin_pitch = sin(pitch);
	cos_pitch = cos(pitch);
	tan_pitch = tan(pitch);
	sin_roll = sin(-roll);	// negative to *remove* the roll
	cos_roll = cos(-roll);

	H = (yres/2.0) / tan(vfov/2.0);
	k1 = tan_pitch * sin_pitch + cos_pitch;
	k2 = H * tan_pitch;
	k3 = 0.0;  /* used later */

	// j=input row (0=top), i=column (0=left)
    	for(j=0; j<yres; j++) {
	    int jxres = j * xres;		// output row offset

    	    for(i=0; i<xres; i++) {

		range = rangeimg->get_data()->get_float(i, j, 0);
		if (range < -10000.0) {		// invalid cell
			ximg[i+jxres] = yimg[i+jxres] = zimg[i+jxres] = range;
			continue;
		}

		range *= rscale;		// apply scale factor

		xscr = (float)i;
		yscr = (float)(yres-1 - j);	// compensate for left-handed

		/* remove roll */
		temp = (xscr-x_center) * cos_roll - (yscr-y_center) * sin_roll;
		yscr = (yscr-y_center) * cos_roll + (xscr-x_center) * sin_roll;
		xscr = temp;

		/* compute theta and gamma */
		k3 = yscr*tan_pitch + H;
		theta = atan2((xscr * k1) , k3);
		gamma = atan(cos(theta) * (k2 - yscr) / k3);
		
		/* compute impact point */
		theta_prime = yaw - theta;
		ground_range = cos(gamma) * range;

		delta_x = cos(theta_prime) * ground_range;
		ximg[i+jxres] = xeye + delta_x;

		delta_y = sin(theta_prime) * ground_range;
		yimg[i+jxres] = yeye + delta_y;

		delta_z = sin(gamma) * range;
		zimg[i+jxres] = zeye - delta_z;

		} //for i

	} // for j

	fwrite(ximg, sizeof(float), xres*yres, fpx);
	fwrite(yimg, sizeof(float), xres*yres, fpy);
	fwrite(zimg, sizeof(float), xres*yres, fpz);

        fclose(fpx);
        fclose(fpy);
        fclose(fpz);

	exit(0);
}	

FILE *create_vicar_file( char * filename)
{
    FILE * fp;

    //make sure output file will have proper permissions
    umask(002);
    if ((fp = fopen (filename, "w")) == NULL) {
	fprintf(stderr, "%s: create_vicar_file() unable to open file %s\n",
		progname, filename);
	exit(1);
    }

    return (fp);
}

void write_vicar_header(FILE *fp, int xres, int yres)
{
    static const char fixed[]="LBLSIZE=1200            FORMAT='REAL'  TYPE='IMAGE'  BUFSIZ=20480  DIM=3  EOL=0  RECSIZE=1200  ORG='BSQ'  NB=1  N3=1  N4=0  NBB=0  NLB=0  BLTYPE=''  ";

	char header[1200];  
	int  i;

    strcpy(header, fixed);
    sprintf(header+sizeof(fixed)-1, 
	"INTFMT='%s'  REALFMT='%s'  NS=%d  N1=%d  NL=%d  N2=%d  ",
	INT_BIGENDIAN ? "HIGH" : "LOW",
	REAL_BIGENDIAN ? "IEEE" : "RIEEE",
	xres, xres, yres, yres);

    // fill with nulls?
    for (i=strlen(header); i < 1200; i++)
	header[i] = ' ';

    fwrite(header, 1200, 1, fp);
}
		
