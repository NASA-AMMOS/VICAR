// Generate random-perturbed range map
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <limits.h>

static const char usage[] = 
"Randomized range map generation\n"
"Output is a .RAN range file with values based on an XY grid\n"
"at constant Z, which are then perturbed a random amount.\n"
"usage: %s -x min delta count randvar -y min delta count randvar\n"
" -z min randvar > ranfile\n"
"min = minimum grid value\n"
"delta = increment for grid values\n"
"count = number of grid values\n"
"randvar = limit of random variation (output values = grid +/- randvar)\n";

double x0, dx, xvar;
double y0, dy, yvar;
double z0, zvar;
int nx, ny;

// return floating point random value between min and max
static double randr(double vmin, double vmax)
{
	//return random()/(double)INT_MAX * (vmax - vmin) + vmin;
	return drand48() * (vmax - vmin) + vmin;
}

int main(int argc, char **argv)
{
	int i;

	for (i=1; i<argc; i++) {
		if (!strcmp(argv[i], "-x")) {
			x0 = atof(argv[++i]);
			dx = atof(argv[++i]);
			nx = atoi(argv[++i]);
			xvar = atof(argv[++i]);
		} else if (!strcmp(argv[i], "-y")) {
			y0 = atof(argv[++i]);
			dy = atof(argv[++i]);
			ny = atoi(argv[++i]);
			yvar = atof(argv[++i]);
		} else if (!strcmp(argv[i], "-z")) {
			z0 = atof(argv[++i]);
			zvar = atof(argv[++i]);
		} else {
			fprintf(stderr, usage, argv[0]);
			return 1;
		}
	}

	if (nx==0 || ny==0) {
		fprintf(stderr, usage, argv[0]);
		return 1;
	}
	
	// write RAN file header - nrows, ncols
	// (assuming native endian-ness is okay)
	write(1, &ny, 4);
	write(1, &nx, 4);

	double yt = y0 + (ny-1)*dy;	
	for (int gy=0; gy<ny; gy++, yt -= dy) {
		for (int gx=0; gx<nx; gx++) {
			float xyz[3];
			xyz[0] = x0 + gx * dx + randr(-xvar, xvar);
			xyz[1] = yt + randr(-yvar, yvar);
			xyz[2] = z0 + randr(-zvar, zvar);
			write(1, xyz, 4*3);
		}
	}
}

