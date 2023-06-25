// test voxel edge info
#include <stdio.h>
#include "summitt_func.h"

static Octree octree;		// the input octree
double min_edge = FLT_MAX;
double max_edge = -FLT_MAX;
double min_bot = FLT_MAX;
double max_bot = -FLT_MAX;
int bottom;

static void scan_tree(Octree_Data *od, int depth)
{
	// recurse on subtrees
	for (int ref=UPPER_LEFT_BACK; ref<NUMBER_OF_SPACE_PARTITIONS; ref++) {
		Octree_Data *kid = od->get_child((Octree_Child_Ref)ref);
		if (kid)
			scan_tree(kid, depth+1);
	}

	// check voxels
	for (NodeSpec *ns = od->get_node_data(); ns; ns = ns->next) {
		double e = ns->edge_length;
		if (e < min_edge) {
			printf("New min edge = %f at depth %d\n", e, depth);
			min_edge = e;
		}
		if (e > max_edge) {
			printf("New max edge = %f at depth %d\n", e, depth);
			max_edge = e;
		}

		if (depth != bottom)
			continue;
		if (e < min_bot) {
			printf("New min bot = %f\n", e);
			min_bot = e;
		}
		if (e > max_bot) {
			printf("New max bot = %f\n", e);
			max_bot = e;
		}
	}          
}

int main(int argc, char **argv)
{
	FILE_Dataport ifp;
	ifp.open(stdin);

 	// check input file type
 	char token[4096];
 	get_next_token(&ifp, token);
	if (!strcmp(token, "SFC_MODEL_V1"))
	 	get_next_token(&ifp, token);
	if (strcmp(token, "OCTREE_V1")) {
		fprintf(stderr, "%s: Whoops - unknown input "
			" type token >%d<\n", argv[0], token);
		exit(1);
	}
	// read in octree (ignore any mesh)
	if (!octree.parse_in(&ifp))
		exit(1);
	ifp.close();

	printf("Octree levels = %d\n", octree.get_max_levels());
	bottom = octree.get_max_levels() - 1;
	scan_tree(octree.get_data(), 0);
	return 0;
}
