// octreestats.C 1.8 05/01/25 10:18:06
/** \file
* Count up octree (or surface model) nodes by level
*/
#include "grape/octree.h"
#include <math.h>
#include <strings.h>

#define MAXLEVELS 32

static const char usage[] =
"Usage: %s [-v] [-i oct_file]\n"
"Prints statistics by level of the octree 'oct_file' (default stdin)\n"
"including #nodespecs, #nodes, and a breakdown of the nodespec\n"
"distribution per node.\n"
"-v = verbose output\n";

int verbose = FALSE;

// Arrays for statistics
long nodespecs[MAXLEVELS];
long numnodes[MAXLEVELS];

// Count the number of nodes at each level that have X (0 to 5)
// nodespecs at that level
long tally[MAXLEVELS][6];  //Shortcut--assume no more than MAXLEVELS

// max octree level
long toplevel;

/// Recursive scan
void parse_octree(Octree_Data *oct)
{
    NodeSpec	*ns;
    int	ref;
    long	level;
    long	nscount = 0;

	// Get level
	level = oct->get_level();

	if (level > 0)
		numnodes[level-1] += oct->any_children();

	// If there are children, process them recursively
	for(ref=UPPER_LEFT_BACK; ref<NUMBER_OF_SPACE_PARTITIONS; ref++) {
		Octree_Data *kid = oct->get_child((Octree_Child_Ref)ref);
		if (kid)
			parse_octree(kid);
	}
	
	// Count the nodespecs at this level
	ns = oct->get_node_data();
	while (ns) {
		nscount++;
		ns=ns->next;
	}

	// Add the nscount to the nodespec count for this level
	nodespecs[level] += nscount;

	// Fill in the tally information
	if (nscount < 5)
		tally[level][nscount]++;
	else
		tally[level][5]++;
}

main (int argc, char **argv)
{
        FILE_Dataport  *fp;
	char    token[4096];
	int i;

	char *infile = NULL;
	for(i=1; i<argc; i++) {
		if (!strcmp(argv[i], "-i")) {
			infile = argv[++i];
		} else if (!strcmp(argv[i], "-v")) {
			verbose = TRUE;
		} else {
			fprintf(stderr, "Unknown argument %s\n", argv[i]);
			fprintf(stderr, usage, argv[0]);
			return 1;
		}
	}

        fp = new FILE_Dataport();
	Octree *octree = new Octree();
	if (infile) {
		if (!fp->ropen(infile)) {
			fprintf(stderr, "%s: unable to open file %s\n", 
				argv[0], infile);
			exit(1);
		}
	} else {
		fp->open(stdin);
	}

	get_next_token(fp, token);      // get file type
 	if (!strcmp(token, "SFC_MODEL_V1"))
		get_next_token(fp, token);
		
	octree->parse_in(fp);
	fp->close();

	// call recursive parser
	Octree_Data *od = octree->get_data();
	if (!od) {
		fprintf(stderr,"%s: octree's data pointer is null\n", argv[0]);
		exit(1);
	}

	toplevel = octree->get_max_levels();
	if (verbose)
		fprintf(stderr, "toplevel = %d\n", toplevel);
	numnodes[toplevel-1] = 1; // hard-code toplevel, levels go 0 to top-1

	if (toplevel > MAXLEVELS) {
		fprintf(stderr, "%s: sorry, programmer shortcut limits\n"
			" the number of levels (MAXLEVELS) to %d"
			" (tree has %d)\n", 
			argv[0], MAXLEVELS, toplevel);
		exit(1);
		// (The right way: allocate storage based on toplevel)
	}

	parse_octree(od);

	// print out results
	long tnodes = 0;		// totals
	long tnodespecs = 0;

	printf("                                                       -- Number of nodes with this many nodespecs -- \n");
	printf("Level  Nodespecs          MaxNodes  NumNodes           0           1           2           3           4           5+\n");
	printf("---------------------------------------------------------------------------------------------------------------------\n");

	for (i=toplevel-1; i>=0; i--)
	{
		double maxnodes = pow( 8.0, double(toplevel - i - 1));
		// Want to do some formatting of strings here.. kfs

		printf("%2d  %8ld  %20.f  %8ld  %10ld  %10ld  %10ld  %10ld  %10ld  %10ld\n", 
			i, nodespecs[i], maxnodes, numnodes[i], 
			tally[i][0], tally[i][1], tally[i][2], tally[i][3], tally[i][4], tally[i][5]);
		tnodes += numnodes[i];
		tnodespecs += nodespecs[i];
	}

	printf("Total nodes = %ld, total nodespecs = %ld\n",
		tnodes, tnodespecs);
	exit(0);
}
