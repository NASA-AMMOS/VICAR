// clip_octree.C 1.7 02/02/24 16:37:57
/** \file
** Given and octree and range information, "clip" the octree, taking
** only those voxels that fall within the range and put them in
** an output octree file.
*/
#include "summitt_func.h"

static Summitt_range range;

static const char usage[] = 
"Usage: %s [-v] [-i input_tree] [-o output_tree]\n"
"  -ro|rm xmin xmax ymin ymax zmin zmax\n\n"
"Creates output octree (default=stdout) with voxels inside specified range\n"
"(in object or model space) of the input octree (default=stdin).\n"
"-v = verbose output.\n";

int main (int argc, char **argv)
{
	Octree * oct_in;  // the input octree
	Octree * oct_out;  // the output octree
	Octree_Data * od_in;
	Octree_Data * od_out;
	FILE_Dataport  *fp_in; 
	FILE_Dataport  *fp_out;
	ZMatrix modelobj1;
	int clip_counter = 0;
	int i;

	char *infile = NULL;
	char *outfile = NULL;
	range.space = -1;
	
	for(i=1; i<argc; i++) {
		if (!strcmp(argv[i], "-i")) {
			infile = argv[++i];
		} else if (!strcmp(argv[i], "-o")) {
			outfile = argv[++i];
		} else if (!strcmp(argv[i], "-v")) {
			verbose = TRUE;
		} else if (argv[i][0]=='-' && argv[i][1]=='r') {
			if (argv[i][2] == 'o')
				range.space = SUMMITT_OBJECT_SPACE;
			else
				range.space = SUMMITT_MODEL_SPACE;
			range.xmin = atof(argv[++i]);
			range.xmax = atof(argv[++i]);
			range.ymin = atof(argv[++i]);
			range.ymax = atof(argv[++i]);
			range.zmin = atof(argv[++i]);
			range.zmax = atof(argv[++i]);
		} else {
			fprintf(stderr, "Unknown argument %s\n", argv[i]);
			fprintf(stderr, usage, argv[0]);
			return 1;
		}
	}

	if (range.space < 0) {	// range not specified
		fprintf(stderr, usage, argv[0]);
		return 1;
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
    oct_out = new Octree( levels );  
    if ((od_out = oct_out->init_data()) == (Octree_Data *)NULL)
    {
	fprintf(stderr, "%s: could not create octree data object.\n", argv[0]);
	return 1;
    }

    oct_out->x.set_value(oct_in->x.get_value()); 
    oct_out->y.set_value(oct_in->y.get_value());
    oct_out->z.set_value(oct_in->z.get_value());
    oct_out->xrot.set_value(oct_in->xrot.get_value());
    oct_out->yrot.set_value(oct_in->yrot.get_value());
    oct_out->zrot.set_value(oct_in->zrot.get_value());
    oct_out->xscale.set_value(oct_in->xscale.get_value());
    oct_out->yscale.set_value(oct_in->yscale.get_value());
    oct_out->zscale.set_value(oct_in->zscale.get_value());

    if (verbose) {
	fprintf(stderr, "Setting values of oct_out to: "
		"%f, %f, %f, %f, %f, %f, %f, %f, %f\n",
		oct_out->x.get_value(),oct_out->y.get_value(),
			oct_out->z.get_value(),
		oct_out->xrot.get_value(),oct_out->yrot.get_value(),
			oct_out->zrot.get_value(),
		oct_out->xscale.get_value(),oct_out->yscale.get_value(),
			oct_out->zscale.get_value());
    }
    
    // Get Octree_Data object for input octree
    if ((od_in = oct_in->get_data()) == NULL) {
	fprintf(stderr, "%s: Unable to get Octree_Data for input octree.\n",
		argv[0]);
	return 1;
    }

   // Get the model to object transform
   oct_in->GetModelToObjectTransform( modelobj1 );
   if (verbose)
   	MatDump(stderr, "Modelobj1", modelobj1);

   // freeze transforms to speed up voxel adding
   oct_out->freeze_xform();

   // Traverse input octree, comparing points to input range.  Points that lie
   // within the range are then added to the output octree.
   clip_counter = summitt_clip_octree(od_in, &range, modelobj1, od_out);

   if (verbose)
	fprintf(stderr, "Resulting octree contains %d voxels\n", clip_counter);

   // Write out the results
   oct_out->parse_out(fp_out);
   fp_out->close();
   return 0;
}
