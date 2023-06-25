// octreeextents.C 1.5 01/10/17 10:58:21
/** \file
* Determine the limits of an octree (or surface model)'s voxel coordinates
*/
#include <math.h>
#include <float.h>
#include "grape/octree.h"

static const char usage[] = 
"Usage: %s [-v] [-i octree_file]\n"
"Computes the range of voxel coordinates of an octree (default = stdin)\n"
"in model and object space.\n"
"-v = verbose output.\n";

int verbose = FALSE;

long	voxel_count;
long	non_red_count;
ZMatrix	xform;

double min_x_mdl = FLT_MAX;
double min_y_mdl = FLT_MAX;
double min_z_mdl = FLT_MAX;
double max_x_mdl = -FLT_MAX;
double max_y_mdl = -FLT_MAX;
double max_z_mdl = -FLT_MAX;
double min_x_obj = FLT_MAX;
double min_y_obj = FLT_MAX;
double min_z_obj = FLT_MAX;
double max_x_obj = -FLT_MAX;
double max_y_obj = -FLT_MAX;
double max_z_obj = -FLT_MAX;

/// Recursive scan
static void parse_octree(Octree_Data *oct)
{
    NodeSpec	*ns;
    double	center[3], tcenter[3];
    int	ref;
    double	r,g,b;

	// recursively process each valid child of node
	for(ref=UPPER_LEFT_BACK; ref<NUMBER_OF_SPACE_PARTITIONS; ref++) {
		Octree_Data *kid = oct->get_child((Octree_Child_Ref)ref);
		if (kid)
			parse_octree(kid);
	}

	// check voxels at this node
	ns = oct->get_node_data();
	while (ns) {
		if (verbose) {
			voxel_count++;
			ns->get_color(&r, &g, &b);
			if(r == 0 || g != 0 || b != 0) non_red_count++;
		}

		ns->get_global_center(center);
		if(center[0] < min_x_mdl) min_x_mdl = center[0];
		if(center[1] < min_y_mdl) min_y_mdl = center[1];
		if(center[2] < min_z_mdl) min_z_mdl = center[2];
		if(center[0] > max_x_mdl) max_x_mdl = center[0];
		if(center[1] > max_y_mdl) max_y_mdl = center[1];
		if(center[2] > max_z_mdl) max_z_mdl = center[2];

		MultPoints(center, xform, tcenter);
		if(tcenter[0] < min_x_obj) min_x_obj = tcenter[0];
		if(tcenter[1] < min_y_obj) min_y_obj = tcenter[1];
		if(tcenter[2] < min_z_obj) min_z_obj = tcenter[2];
		if(tcenter[0] > max_x_obj) max_x_obj = tcenter[0];
		if(tcenter[1] > max_y_obj) max_y_obj = tcenter[1];
		if(tcenter[2] > max_z_obj) max_z_obj = tcenter[2];

		ns=ns->next;
	} 
}

main (int argc, char **argv)
{
	char	*fname=NULL;
	int	i;

	for(i=1; i<argc; i++) {
		if(!strcmp(argv[i],"-i")) {
			fname = argv[++i];
		} else if(!strcmp(argv[i],"-v")) {
			verbose = TRUE;
		} else {
			fprintf(stderr, "Unknown argument %s\n", argv[i]);
			fprintf(stderr, usage, argv[0]);
			exit(1);
		}
	}

	// read in octree
        FILE_Dataport *fp = new FILE_Dataport();
        // make an octree shape
	Octree  *octree = new Octree(10);       // allow 10 levels
	if(fname) {
		fp->open(fname);
	} else {
		fp->open(stdin);
	}
	char    token[4096];
	get_next_token(fp, token);      // get file type
	if (!strcmp(token, "SFC_MODEL_V1"))
		get_next_token(fp, token);	// OCTREE_V1
	octree->parse_in(fp);

	if (verbose)
		printf("Octree levels = %d\n", octree->get_max_levels());
			
        // call recursive parser
	Octree_Data *od = octree->get_data();
	if (!od) {
		fprintf(stderr,"Octree is empty\n");
		exit(1);
	}
	octree->GetModelToObjectTransform(xform);
	parse_octree(od);

	if (verbose) {
		fprintf(stdout,"Found %d voxels\n", voxel_count);
		fprintf(stdout,"Found %d non-red voxels\n", non_red_count);
	}

	fprintf(stdout,"\nOctree Extents in Model Space:\n");
	fprintf(stdout,"Min X=%14g  Min Y=%14g  Min Z=%14g\n",
		min_x_mdl, min_y_mdl, min_z_mdl);
	fprintf(stdout,"Max X=%14g  Max Y=%14g  Max Z=%14g\n",
		max_x_mdl, max_y_mdl, max_z_mdl);
	fprintf(stdout,"\nOctree Extents in Object Space:\n");
	fprintf(stdout,"Min X=%14g  Min Y=%14g  Min Z=%14g\n",
		min_x_obj, min_y_obj, min_z_obj);
	fprintf(stdout,"Max X=%14g  Max Y=%14g  Max Z=%14g\n",
		max_x_obj, max_y_obj, max_z_obj);
	exit(0);
}
