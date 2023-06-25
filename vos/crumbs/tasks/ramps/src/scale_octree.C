// %M% %I% %E% %U%
/** \file
// Scale octree (in model or world space). 
// Doesn't change model-to-object transform,
// so object space is scaled also.
*/
#include "summitt_func.h"

static const char usage[] = 
"Usage: %s [-v] [-i input_tree] [-o output_tree]\n"
"  -s xscale yscale zscale [-t x y z xr yr zr]\n\n"
"Creates output octree (default=stdout) with voxels from the input octree\n"
"(default=stdin), scaled as specified.\n"
"-t = transform to world space - translation and rotation\n"
"  (if not specified, voxels are scaled in model space)\n"
"-v = verbose output.\n";

double xscale = 1.0;
double yscale = 1.0;
double zscale = 1.0;
ObjNode node;
Octree_Data * od_out;
ZMatrix m2world, world2m;
int world_scale;
int verbose;

/// Recursive scan
void scale_octree(Octree_Data *od)
{
	// get voxels at this node
	NodeSpec *ns1 = od->get_node_data();
	while (ns1) {
		double center[OCTREE_DIMS];
		ns1->get_global_center(center);

		if (world_scale) {
			// transform to world coordinates
			double wcenter[3];
			MultPoints(center, m2world, wcenter);
			// scale there
			wcenter[0] *= xscale;
			wcenter[1] *= yscale;
			wcenter[2] *= zscale;
			// transform back to model
			MultPoints(wcenter, world2m, center);
		} else {
			// apply scaling in model coordinates
			center[0] *= xscale;
			center[1] *= yscale;
			center[2] *= zscale;
		}
		ns1->set_global_center(center);
		// ** modify edge_scale?
		
		/* add to output octree */
		NodeSpec *nextptr = ns1->next;
		ns1->next = NULL;

		od_out->add_voxel(ns1);
		ns1 = nextptr;
	}

	// recursively handle children
	for (int ref=UPPER_LEFT_BACK; ref<NUMBER_OF_SPACE_PARTITIONS; ref++) {
		Octree_Data *kid = od->get_child((Octree_Child_Ref)ref);
		if (kid)
			scale_octree(kid);
	}
}

int main (int argc, char **argv)
{
	Octree * oct_in;  // the input octree
	Octree * oct_out;  // the output octree
	Octree_Data * od_in;
	FILE_Dataport  *fp_in; 
	FILE_Dataport  *fp_out;
	int i;
	
	char *infile = NULL;
	char *outfile = NULL;
	
	for(i=1; i<argc; i++) {
		if (!strcmp(argv[i], "-i")) {
			infile = argv[++i];
		} else if (!strcmp(argv[i], "-o")) {
			outfile = argv[++i];
		} else if (!strcmp(argv[i], "-v")) {
			verbose = TRUE;
		} else if (!strcmp(argv[i], "-s")) {
			xscale = atof(argv[++i]);
			yscale = atof(argv[++i]);
			zscale = atof(argv[++i]);
		} else if (!strcmp(argv[i], "-t")) {
			node.x = atof(argv[++i]);
			node.y = atof(argv[++i]);
			node.z = atof(argv[++i]);
			node.xrot = atof(argv[++i]);
			node.yrot = atof(argv[++i]);
			node.zrot = atof(argv[++i]);
			node.set_object(oct_in);
			world_scale = TRUE;
		} else {
			fprintf(stderr, "Unknown argument %s\n", argv[i]);
			fprintf(stderr, usage, argv[0]);
			return 1;
		}
	}
	
    fp_in = new FILE_Dataport();
    fp_out = new FILE_Dataport();

    /* Open input octree file and skip header token */
    if (infile) {
	if (!fp_in->ropen(infile)) {
		fprintf(stderr, "%s: unable to open %s for reading\n", 
			argv[0], infile);
		return 1;
	}
    } else {
    	fp_in->open(stdin);
    }

    char token[4096];
    get_next_token(fp_in, token);
    if (!strcmp(token, "SFC_MODEL_V1"))
	get_next_token(fp_in, token);	// OCTREE_V1

    // Create new octree for output
    if (outfile) {
	if (!fp_out->wopen(outfile)) {
		fprintf(stderr, "%s: unable to open %s for writing\n", 
			argv[0], outfile);
		return 1;
	}
    } else {
    	fp_out->open(stdout);
    }

    // Create new octrees, and populate oct_in with the input octree
    oct_in = new Octree();
    oct_in->parse_in(fp_in);

    long levels = oct_in->get_max_levels();
    if (verbose)
    	fprintf(stderr, "input octree = %ld levels\n", levels);
    oct_out= new Octree( levels );  
    od_out = new Octree_Data( levels - 1);
    if ((od_out = oct_out->init_data()) == (Octree_Data *)NULL)
    {
	fprintf(stderr, "%s: could not create octree data object.\n", argv[0]);
	return 1;
    }

    // copy transform parameters. 
    // Need to scale the offsets due to transform ordering
    oct_out->x.set_value(oct_in->x.get_value() /* * xscale */); 
    oct_out->y.set_value(oct_in->y.get_value() /* * yscale */);
    oct_out->z.set_value(oct_in->z.get_value() /* * zscale */);
    oct_out->xrot.set_value(oct_in->xrot.get_value());
    oct_out->yrot.set_value(oct_in->yrot.get_value());
    oct_out->zrot.set_value(oct_in->zrot.get_value());
    oct_out->xscale.set_value(oct_in->xscale.get_value());
    oct_out->yscale.set_value(oct_in->yscale.get_value());
    oct_out->zscale.set_value(oct_in->zscale.get_value());

    // Get Octree_Data object for input octree
    if ((od_in = oct_in->get_data()) == NULL) {
	fprintf(stderr, "%s: Unable to get Octree_Data for input octree.\n",
		argv[0]);
	return 1;
    }
    
    // If transforming in world space, get matrices
    if (world_scale) {
    	node.GetObjToWorldTransform(m2world);
    	MatInvert(m2world, world2m);
    }

   // freeze transforms to speed up voxel adding
   oct_out->freeze_xform();

   // Traverse input octree, scaling points and adding them 
   // to the output octree.
   scale_octree(od_in);

   // Write out the results
   oct_out->parse_out(fp_out);
   fp_out->close();
   return 0;
}
