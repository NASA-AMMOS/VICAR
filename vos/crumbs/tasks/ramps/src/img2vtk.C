/* This utility converts an image file to a vtk structured points object */
/* The image is considered an elevation file for a 2.5D model            */
/* John Wright		06 March, 1998                                   */

#include <fstream.h>
#include <float.h>
#include "vtk.h"
#include "image/image.h"

void main(int argc, char **argv)
{
	Image	*img;
	int	i,j,k, xres, yres, zval;
	char	*ifname=NULL, *ofname=NULL;
	float	min, max, temp, thck=1.0, bottom;
	vtkStructuredPoints	*vol;


	if(argc < 2 || !strcmp(argv[1],"-") || !strcmp(argv[1],"-h") || !strcmp(argv[1],"-help")) {
		fprintf(stderr,"Usage is: %s [-t thickness] imgfile vtkfile\n", argv[0]);
		exit(1);
	}

	for(i=1; i<argc; i++) {
		if(!strcmp(argv[i], "-t")) {
			i++;
			thck = atof(argv[i]);
		} else if(!ifname) {
			ifname = strdup(argv[i]);
		} else {
			ofname = strdup(argv[i]);
		}
	}

	// open image file
	img = new Image(ifname);

	// get resolution
	img->get_res(&xres, &yres);

	// find min and max elevations in image
	min = FLT_MAX;
	max = -FLT_MAX;
	for(i=0; i<xres; i++) {
		for(j=0; j<yres; j++) {
			temp = img->get_data()->get_float(i, j);
			if(temp < min)min = temp;
			if(temp > max)max = temp;
		}
	}
fprintf(stderr,"Min=%f  max=%f\n",min,max);

	// create grid
	vol = vtkStructuredPoints::New();
	vol->SetDimensions(xres, yres, (int)(max-min)+1);
	vol->SetOrigin(0.0,0.0,0.0);
	vol->SetSpacing(1.0,1.0,1.0);

	// create temp area for stuffing grid
	vtkUnsignedCharScalars *scalars = vtkUnsignedCharScalars::New();

	// output stack of voxels for each (x,y)
	for(i=0; i<xres; i++) {
		for(j=0; j<yres; j++) {
			temp = img->get_data()->get_float(i, j);
			temp -= min;
			bottom = temp-thck;
/*
			for(zval = 0.0; zval < bottom; zval+=1.0) {
				scalars->InsertScalar((i*yres+j)*(int)(max-min+1)+(int)zval, 0.0);  // unknown
			}
			for(; zval < temp+thck; zval+=1.0) {
				scalars->InsertScalar((i*yres+j)*(int)(max-min+1)+(int)zval, 10.0);  // known
			}
			for(; zval <= max; zval+=1.0) {
				scalars->InsertScalar((i*yres+j)*(int)(max-min+1)+(int)zval, -1.0);  // empty
			}
*/
			for(zval = 0.0; zval < bottom; zval+=1.0) {
				scalars->InsertScalar(((int)zval*yres+j)*xres+i, (unsigned char)0);  // unknown
			}
			for(; zval < temp+thck; zval+=1.0) {
				scalars->InsertScalar(((int)zval*yres+j)*xres+i, (unsigned char)255);  // known
			}
			for(; zval <= max; zval+=1.0) {
				scalars->InsertScalar(((int)zval*yres+j)*xres+i, (unsigned char)5);  // empty
			}
		}
	}

	vol->GetPointData()->SetScalars(scalars);

	// give volume to writer to output
	vtkStructuredPointsWriter *writer = vtkStructuredPointsWriter::New();
	writer->SetInput(vol);
	writer->SetFileName(ofname);
	writer->Write();

	scalars->Delete();

	exit(0);
}

