// accum_tree.C 1.6 02/02/15 07:08:24
/** \file
 ** Utility program to accumulate octree nodes below a specified level 
 ** into a single voxel.
 **/
#include "grape/octree.h"

static const char usage[] = 
"Usage: %s [-v] [-a] src.oct dest.oct nlevels\n"
"Accumulate bottom nlevels of src.oct octree, write to dest.oct\n"
"-v = verbose output\n"
"-a = accumulate to nlevels (n=absolute, not relative)\n"; 

int verbose = 0;
int abs_acc = 0;

int main(int argc, char **argv)
{
	char *progname = argv[0];
	while (argc > 1 && argv[1][0] == '-') {
		if (argv[1][1] == 'v')
			verbose = 1;
		else if (argv[1][1] == 'a')
			abs_acc = 1;
		else
			break;
		argv++;
		argc--;
	}
	if (argc != 4) {
		fprintf(stderr, usage, progname);
		exit(1);
	}

	FILE_Dataport *fp = new FILE_Dataport();
	if (!fp->ropen(argv[1])) {
		fprintf(stderr, "%s: Can't open %s for reading\n", 
			progname, argv[1]);
		exit(1);
	}

	// skip input headers (OCTREE_V1)
	char token[4096];
	get_next_token(fp, token);
	if (!strcmp(token, "SFC_MODEL_V1"))
		get_next_token(fp, token);
	Octree oct;
	oct.parse_in(fp);
	printf("Old depth = %d\n", oct.get_max_levels());

	int levels = atoi(argv[3]);
	if (!abs_acc)
		levels = oct.get_max_levels() - levels;

	// accumulate the lower levels
	levels = oct.set_max_levels(levels, TRUE);
	if (verbose)
		printf("New depth = %d\n", levels);

	// write it back out
	if (!fp->wopen(argv[2])) {
		fprintf(stderr, "%s: Can't open %s for writing\n",
			progname, argv[2]);
		exit(1);
	}
	oct.parse_out(fp);
	fp->close();
	return 0;
}
