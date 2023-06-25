// summitt_match.C 1.19 02/08/07 15:56:56
/** \file
** Given an octree forest, use the iterative closest point algorithm
** in summitt_func to match a "source" model to a subset of the other
** models. This is a top-level user interface to the match function
** in summitt_funcs.C.
**
** The source model (and its initial transform) can be specified 
** explicitly rather than loaded from the forest. The advantage of
** doing so is that a bounding volume test can be applied during
** forest input, bypassing the loading of models outside the 
** region of interest. In any case, the bvfactor - which corresponds
** to an error bound on the initial transform) is used to scale the
** source bounding volume for clipping voxels for the merged destination 
** octree, and also to scale the destination bounding volume for
** limiting source voxel matches.
**
** (A future production version of this application might keep destination
** data in memory and process a series of source models...
*/
#include <ctype.h>
#include "summitt_func.h"

static const char usage[] = 
"Usage: %s [-v] [-it #iterations] [-e t-epsilon r-epsilon] [-t tolerance]\n"
" [-s src] [-d dest] [-z stretch] [-b factor] [-n distance]\n"
" [-x tx ty tz rx ry rz sx sy sz] -i forest_file\n"
"Match source model to forest using iterative closest point algorithm.\n"
"-v  = verbose output (repeat for extra-verbose)\n"
"-it = max iterations (default 100)\n"
"-e  = epsilon (max absolute change) completion threshold (default 0 0)\n"
"      t_epsilon = translation units, r_epsilon = rotation degrees\n"
"-t  = tolerance (max relative change) completion threshold (default 0)\n"
"-s  = index (0-n) of source model in forest to adjust (default=last one),\n"
"      or filename of octree or surface model file\n"
"-d  = index (0-n) of destination model to match against,\n"
"      or 'a' to match all other models,\n"
"      or 'p' to match all preceding models 0 to src-1 (default)\n"
"-z  = stretch destination in model space for better matching (default=1)\n"
"-b  = bounding volume scale factor (default=1)\n"
"-n  = neighborhood error bound (src mdl space) to reject edges (default 0)\n"
"-x  = object-to-world transform parameters if source model is named\n"
"      with -s instead of being taken from the forest\n"
"-i  = input forest file (list of octrees with transforms)\n";

int main (int argc, char **argv)
{
    char *forest_file = NULL;	// only you can prevent them :-O
    char *src_file = NULL;
    double tepsilon = 0.0;  
    double repsilon = 0.0;
    double tolerance = 0.0; 
    double bvfactor = 1.0;
    double err_radius = 0.0;
    double zstretch = 1.0;
    int max_iterations = 100;
    int src = -1;		// default is last
    int dest = MATCH_PREV;
    ObjNode *node = new ObjNode; // for explicit source model (must be dyn)

    for(int i=1; i<argc; i++) {
       	if(!strcmp(argv[i], "-i")) {
       	    forest_file = argv[++i];
 	} else if(!strcmp(argv[i], "-v")) {
 	    verbose++;
 	} else if(!strcmp(argv[i], "-it")) {
            max_iterations = atoi(argv[++i]);
	} else if(!strcmp(argv[i], "-e")) {
            tepsilon = atof(argv[++i]);
            repsilon = atof(argv[++i]);
	} else if(!strcmp(argv[i], "-t")) {
            tolerance = atof(argv[++i]);
    	} else if(!strcmp(argv[i], "-b")) {
            bvfactor = atof(argv[++i]);
    	} else if(!strcmp(argv[i], "-n")) {
            err_radius = atof(argv[++i]);
      	} else if(!strcmp(argv[i], "-s")) {
	    if (isdigit(argv[++i][0]))	// index in forest
		src = atoi(argv[i]);
	    else			// explicit file
	    	src_file = argv[i];
	} else if(!strcmp(argv[i], "-d")) {
	    if (argv[++i][0] == 'a')
	    	dest = MATCH_ALL;
	    else if (argv[i][0] == 'p')
	    	dest = MATCH_PREV;
	    else
	    	dest = atoi(argv[i]);
	} else if(!strcmp(argv[i], "-z")) {
            zstretch = atof(argv[++i]);
	} else if(!strcmp(argv[i], "-x") && i+9<argc) {
		node->x = atof(argv[++i]);
		node->y = atof(argv[++i]);
		node->z = atof(argv[++i]);
		node->xrot = atof(argv[++i]);
		node->yrot = atof(argv[++i]);
		node->zrot = atof(argv[++i]);
		node->xscale = atof(argv[++i]);
		node->yscale = atof(argv[++i]);
		node->zscale = atof(argv[++i]);
	} else {
	    fprintf(stderr, "Unknown argument %s\n", argv[i]);
	    fprintf(stderr, usage, argv[0]);
	    return 1;
	}
    }
    if (forest_file == NULL) {
        fprintf(stderr, "%s: input forest file required\n", argv[0]);
	return 1;
    }

    FILE_Dataport *fp = new FILE_Dataport();
    char token[4096];

    // if explicit source file named, load it first
    if (src_file) {
    	if (!fp->ropen(src_file)) {
		fprintf(stderr, "%s: unable to open source %s for reading\n", 
			argv[0], src_file);
		return 1;
	}
	do {			// skip header tokens
	    get_next_token(fp, token);
	} while (!strcmp(token, "SFC_MODEL_V1"));

	Octree *oct = new Octree;
	if (!oct->parse_in(fp)) {
		fprintf(stderr, "%s: unable to read source octree %s\n",
			argv[0], src_file);
		return 1;
	}
	node->set_object((Obj *)oct);
    }

    // load forest
    if (!fp->ropen(forest_file)) {
	fprintf(stderr, "%s: unable to open forest %s for reading\n", 
		argv[0], forest_file);
	return 1;
    }
    Forest forest;
    get_next_token(fp, token);	// skip GRP_V1

    if (src_file) {
#if 0
	// get a bounding volume around source, in world space,
	// and add some margin
	Summitt_range src_bvol;
	summitt_bounding_volume(node, &src_bvol);
	double margin = (src_bvol.xmax - src_bvol.xmin) * (bvfactor - 1.0)/2.0;
	src_bvol.xmin -= margin;
	src_bvol.xmax += margin;
	margin = (src_bvol.ymax - src_bvol.ymin) * (bvfactor - 1.0)/2.0;
	src_bvol.ymin -= margin;
	src_bvol.ymax += margin;
	margin = (src_bvol.zmax - src_bvol.zmin) * (bvfactor - 1.0)/2.0;
	src_bvol.zmin -= margin;
	src_bvol.zmax += margin;

	if (verbose)
		src_bvol.dump(stderr, "Forest input limits");

	// load forest models that overlap this volume
    	forest.parse_in(fp, &src_bvol);
#else
    	forest.parse_in(fp);
#endif

	// add source model to end of forest (in order to use merge func)
    	forest.add_child(node);
    	src = -1;		// use last forest model
    } else {
	forest.parse_in(fp);
    }

    // validate source and destination indices
    int ntrees = forest.get_num_children();
    if (ntrees < 2) {
    	fprintf(stderr, "%s: need at least two trees in forest\n", argv[0]);
    	exit(1);
    }
    if (src < 0)		// means use last model
    	src = ntrees-1;
    if (src == dest) {
   	fprintf(stderr, "%s: source and dest are the same\n", argv[0]);
    	exit(1);
    }
    if (src==0 && dest==MATCH_PREV) {
   	fprintf(stderr, "%s: no previous destination trees\n", argv[0]);
    	exit(1);
    }
    if (verbose) {
	fprintf(stderr, "read %d trees\n", ntrees);
	for (int i=0; i<ntrees; i++) {
		Octree *oct = (Octree *)forest.get_child(i)->get_object();
		fprintf(stderr, "tree %d = %d levels\n", i, 
			oct->get_max_levels());
	}
	fprintf(stderr, "matching tree %d against ", src);
	if (dest == MATCH_PREV)
		fprintf(stderr, "trees 0-%d\n", src-1);
	else if (dest == MATCH_ALL)
		fprintf(stderr, "all other trees\n");
	else
		fprintf(stderr, "tree %d\n", dest);
    }

    // merge destination octrees into single octree. Don't stretch yet
    Octree *mdo = matcher_merge(&forest, src, dest, bvfactor, 1.0);

    // Setup source and destination objnodes for matcher.
    // Destination (now a single octree) uses base forest model objnode
    if (dest == MATCH_PREV)
	dest = 0;
    else if (dest == MATCH_ALL)
	dest = src==0 ? 1 : 0;
    ObjNode *dn = forest.get_child(dest);
    dn->set_object((Obj *)mdo);	// replace base octree with merged model
    ObjNode *sn = forest.get_child(src);
   
    if (zstretch != 1.0) {
	// First pass on unstretch model to get basic orientation.
    	// Clean up allocated memory.
    	summitt_icp_match_octrees(sn, dn, 2, 
		tepsilon, repsilon, tolerance, bvfactor, err_radius, TRUE);

    	// rebuild destination octree with Z-stretch
    	Octree *mdo2 = matcher_merge(&forest, src, dest, 1.0, zstretch);
    	if (verbose > 3) { 	// debug - dump merged octrees
    		FILE_Dataport fp;
	
    		fp.wopen("merge1.oct");
    		mdo->parse_out(&fp);
    		fp.close();
  	 
    		fp.wopen("merge2.oct");
    		mdo2->parse_out(&fp);
    		fp.close();
    	}

    	dn->set_object((Obj *)mdo2);
    }

    // Continue matching against stretched model.
    // Don't bother with proper memory cleanup since we're just gonna exit...
    summitt_icp_match_octrees(sn, dn, max_iterations, 
		tepsilon, repsilon, tolerance, bvfactor, err_radius, FALSE);

    // other cleanup we're not bothering with :)
    // delete mdo;
    // delete mdo2;

    // write result transform (updated in source objnode) to stdout
    printf("-t %f %f %f -r %f %f %f -s %f %f %f\n", 
	sn->x.get_value(), sn->y.get_value(), sn->z.get_value(),
    	sn->xrot.get_value(), sn->yrot.get_value(), sn->zrot.get_value(), 
    	sn->xscale.get_value(), sn->yscale.get_value(), sn->zscale.get_value());
    
    exit(0);
}
