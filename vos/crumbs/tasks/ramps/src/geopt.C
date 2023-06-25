// extract georef points from range map to temp range map

#include <stdio.h>
#include <math.h>
#include "image/image.h"
#include "image/datatypes.h"
#include "image/types/all.h"
#include "grape/range.h"

float xyz[100][3];
int nr = 0;

int main(int argc, char **argv)
{
	int i;
	
	if (argc != 2) {
		fprintf(stderr, "usage: %s inmap > outmap\n", argv[0]);
		return 1;
	}
	
	// load input range map
        Image *ranfile;
        if ((ranfile = new Image(argv[1])) == NULL) {
                fprintf(stderr, "%s: Can't read range file %s\n", 
                        argv[0], argv[1]);
                return 1;
        }
        ImageData *imap = ranfile->get_data();
        if (!imap) {
                fprintf(stderr, "%s: No range file data in %s\n",
                        argv[0], argv[1]);
                return 1;
        }
        int xres, yres;
        imap->get_res(&xres, &yres);
	
	// get list of reference points in original 4Kx4K image
	char buf[256];
	while (fgets(buf, sizeof(buf), stdin)) {
		float x, y;
		if (sscanf(buf, "%f %f", &x, &y) != 2)
			continue;
			
		// convert to 488x488 180-rotated coordinates,
		int ix = 447 - int(x/8.0 - 32);
		int iy = 447 - int(y/8.0 - 32);
		if (ix < 0 || ix >= xres || iy < 0 || iy >= yres) {
			fprintf(stderr, "Invalid point %d,%d\n", ix, iy);
			continue;
		}

		// save corresponding XYZ point from input range map
		for (i=0; i<3; i++)
			xyz[nr][i] = imap->get_float(ix, iy, i);
		nr++;
	}
	
	// write map of copied XYZ points
	int ny = 1;
	write(1, &ny, 4);	
	write(1, &nr, 4);
	write(1, xyz, 4*3*nr);
}
	