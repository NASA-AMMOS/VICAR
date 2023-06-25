// summitt_merge.C 1.10 02/06/14 08:23:33
/** \file
// Merge a source "unkown" octree into a destination "known" octree.
*/
#include "summitt_func.h"

static const char usage[] = 
"Usage: %s [-v] [-acc] [-grow] [-t xeye yeye zeye]\n"
"  [-r roll pitch yaw] [-s scale]\n"
"  -i src_oct_1_file src_oct_2_file > dest_oct_file\n\n"
"Merges octree src_oct_1 into octree src_oct_2, writing the merged tree\n"
"to standard output.\n"
"-v = verbose output.\n"
"-acc = enable nodespec accumulation\n"
"-grow = grow levels as needed\n"
"-t = eyepoint translation\n"
"-r = eyepoint rotation in degrees\n"
"-s = XY scale factor\n";

#define PI      3.14159265358979

Octree * oct1;  /* unknown, to be translated */
Octree * oct2;  /* known */

/* Create ObjNodes to correspond to the octrees.  
** Needed for transformations */ 
ObjNode * objn1;
ObjNode * objn2;  

int main (int argc, char **argv)
{
	FILE_Dataport  *fp1;  /* unknown */
	FILE_Dataport  *fp2;  /* known */
	FILE_Dataport  *fp2_out;
	Boolean accumulation_on = FALSE;
	Boolean level_grow = FALSE;

	int i;
	double  scaler=1.0;
	float   xeye=0.0, yeye=0.0, zeye=0.0, pitch=0.0, roll=0.0, yaw=0.0;
	char *infile1 = NULL;
	char *infile2 = NULL;
	
   for(i=1; i<argc; i++) {
        if(!strcmp(argv[i], "-acc")) {
	        accumulation_on = TRUE;
	} else if(!strcmp(argv[i], "-grow")) {
	        level_grow = TRUE;
        } else if(!strcmp(argv[i], "-t")) {
            xeye = atof(argv[++i]);
            yeye = atof(argv[++i]);
            zeye = atof(argv[++i]);
	} else if (!strcmp(argv[i], "-r")) {
	    roll  = atof(argv[++i]);  	// (keep in degrees)
	    pitch = atof(argv[++i]);
            yaw   = atof(argv[++i]);
	} else if (!strcmp(argv[i], "-s")) {
            scaler = atof(argv[++i]);
        } else if (!strcmp(argv[i], "-i")) {
            infile1 = argv[++i];
            infile2 = argv[++i];
        } else if (!strcmp(argv[i], "-v")) {
            verbose = TRUE;
	} else {
	    fprintf(stderr, "argument %s unrecognized.\n", argv[i]);
	    fprintf(stderr, usage, argv[0]);
	    exit(1);
        }
    }

    if (infile2 == NULL) {
    	fprintf(stderr, "Need to specify input filenames\n");
 	fprintf(stderr, usage, argv[0]);
	exit(1);
    }

    fp1 = new FILE_Dataport();
    fp2 = new FILE_Dataport();
    fp2_out = new FILE_Dataport();

    /* Open octree files and skip leading
    ** OCTREE_V1 token 
    */
    if (!fp1->ropen(infile1)) {
	fprintf(stderr, "%s: unable to open %s for reading\n", 
		argv[0], infile1);
	exit(1);
    }

    if (!fp2->ropen(infile2)) {
	fprintf(stderr, "%s: unable to open %s for reading\n", 
		argv[0], infile2);
	exit(1);
    }

    char token[4096];
    get_next_token(fp1, token);
    get_next_token(fp2, token);

    /*
    ** Create new octrees and populate them
    */
    oct1 = new Octree();
    oct2 = new Octree();

    oct1->parse_in(fp1);
    oct2->parse_in(fp2);

    /*
    ** Create new object nodes
    */
    objn1 = new ObjNode();
    objn2 = new ObjNode();

    /*
	Set the object to world transforms
    */
    objn1->x.set_value(xeye); 
    objn1->y.set_value(yeye);
    objn1->z.set_value(zeye);
    objn1->xrot.set_value(roll);  // set_value expects degrees
    objn1->yrot.set_value(pitch);  // set_value expects degrees
    objn1->zrot.set_value(yaw);  // set_value expects degrees
    objn1->xscale.set_value(1.0);
    objn1->yscale.set_value(1.0);
    objn1->zscale.set_value(1.0);

    objn2->x.set_value(0.0); 
    objn2->y.set_value(0.0);
    objn2->z.set_value(0.0);
    objn2->xrot.set_value(0.0);
    objn2->yrot.set_value(0.0);
    objn2->zrot.set_value(0.0);
    objn2->xscale.set_value(1.0/scaler);
    objn2->yscale.set_value(1.0/scaler);
    objn2->zscale.set_value(1.0);

    /*
    ** Connect Octrees to ObjNodes.
    */
   objn2->set_object( oct2);
   objn1->set_object( oct1);

   // do the merge
   summitt_merge_octrees(objn1, objn2, accumulation_on, level_grow);

   if (verbose)
	fprintf(stderr, "Before parse_out, oct2 get_levels()= %d\n", 
		oct2->get_max_levels());

   /* output the merged tree */
    fp2_out->open(stdout);
    oct2->parse_out(fp2_out);
    fp2_out->close();
    return 0;
}

