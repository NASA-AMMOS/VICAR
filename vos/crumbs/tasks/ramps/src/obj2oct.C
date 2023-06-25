// obj2oct.C 1.2 01/02/26 07:40:00
/*
This utility converts an obj file to a grape octree file.
It treats the vertices as samples, not voxels.
*/

#include "grape/octree.h"
#include <math.h>

static const char usage[] = 
"Usage: %s [edge_length] > output_tree\n"
"Creates octree on stdout from Wavefront .OBJ file, with vertices\n"
"used as samples, not voxels.\n"
"Default edge_length is 0.023.\n";

int main (int argc, char **argv)
{
	char	buff[4096];
	NodeSpec *ns;
	double	cntr[OCTREE_DIMS] = { 0.0 };
	double	edge = 0.023;

	if (argc == 2 && argv[1][0] != '-') {
		edge = atof(argv[1]);
	} else if (argc != 1) {
		fprintf(stderr, usage, argv[0]);
		return 1;
	}		

	// make an octree shape
	Octree	*octree = new Octree(10);	// allow 10 levels

	octree->x = -2.5;
	octree->y = 0.5;
	octree->z = 0.0;
	octree->xrot = 0.0;
	octree->yrot = 0.0;
	octree->zrot = 0.0;
	octree->xscale = 1.0;
	octree->yscale = 1.0;
	octree->zscale = 1.0;

	// add a large cube of zero
	ns = new NodeSpec();
	ns->set_color(0.0, 0.0, 0.0);
	cntr[0] = 0.0; cntr[1] = 0.0; cntr[2] = 0.0;
	ns->set_global_center(cntr);
	ns->edge_length = 10.00;
	octree->add_voxel(ns);

	while(fgets(buff, 4095, stdin)) {
		if(!strncmp(buff, "v ", 2)) {
			ns = new NodeSpec();
			ns->set_color(1.0, 0.0, 0.0);
			sscanf(buff,"v %lf %lf %lf", &cntr[0], &cntr[1], &cntr[2]);
			ns->set_global_center(cntr);
			ns->edge_length = edge;
			//octree->add_voxel(ns);
			octree->add_sample(ns);
		}
	}

	// add a large cube of zero
	ns = new NodeSpec();
	ns->set_color(0.0, 0.0, 0.0);
	cntr[0] = 0.0; cntr[1] = 0.0; cntr[2] = 0.0;
	ns->set_global_center(cntr);
	ns->edge_length = 10.00;
	octree->add_voxel(ns);

	FILE_Dataport *fp2 = new FILE_Dataport();
	fp2->open(stdout);
	octree->parse_out(fp2);
	return 0;
}


