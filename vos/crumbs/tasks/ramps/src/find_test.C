// Test octree search functions
#include "summitt_func.h"
#include <stdlib.h>
#include <math.h>
#include <time.h>

#define NUM_REP	100	// # repetitions for timing

double p[3], r;		// point and radius
NodeSpec *match;	// matched point
NodeSpec_List_Element *list;	// list of matched points
double m[3];		// matched point coordinates
int nmatch;		// number of matched points

void show_search()
{
	if (nmatch == 0) {
		printf("No matches\n");
		return;
	}
	printf("%d match(es), starting with:\n", nmatch);
	int show = nmatch;
	if (show > 4) show = 4;
	NodeSpec_List_Element *nle = list;
	int i;
	for (i=0; i<show; i++) {
		nle->nodespec->get_global_center(m);
		printf("  dist=%f, point=%08X, coords=%f,%f,%f\n",
			distance(m, p), nle->nodespec, m[0], m[1], m[2]);
		nle = nle->next;
	}

	// check all distances
	for (i=0, nle=list; i<nmatch; i++) {
		nle->nodespec->get_global_center(m);
		if (distance(m, p) > r)
			printf("  point %d is past radius!\n", i);
		nle = nle->next;
	}
}

// scan node list for nodes within radius_squared
int find_nodes(NodeSpec *node, double pt[], double rsq,
		NodeSpec_List_Element **list)
{
	double node_center[OCTREE_DIMS];
	int count = 0;

        while (node) {

                node->get_global_center(node_center);

                // Calculate the distance d2 = (xdiff + ydiff + zdiff)
		double dsq;
		dsq = (node_center[0] - pt[0]) * (node_center[0] - pt[0]) +
			(node_center[1] - pt[1]) * (node_center[1] - pt[1]) +
			(node_center[2] - pt[2]) * (node_center[2] - pt[2]);

                if (dsq <= rsq) {
			count++;
			NodeSpec_List_Element * node_dup;
			node_dup = new NodeSpec_List_Element(node);
			node_dup->next = *list;
			*list = node_dup;
                }

        	node = node->next;
        }
	return count;
}

// exhaustive search for points within radius_squared
int search_radius(Octree_Data *od, double p[], double rsq, 
		NodeSpec_List_Element **list)
{
	Octree_Data * child;
	int	i;

	// Loop through nodespecs at this level 
	int count = find_nodes(od->get_node_data(), p, rsq, list);

	// Recursively search all children
	for (i=UPPER_LEFT_BACK; i< NUMBER_OF_SPACE_PARTITIONS; i++) {
		if ((child = od->get_child( Octree_Child_Ref (i))) != NULL) {
                	count += search_radius(child, p, rsq, list);
		}
	}

	return count;
}

void main (int argc, char **argv)
{
	if (argc != 2) {
		printf("usage: %s octree_file\n", argv[0]);
		exit(1);
	}
	
	// read in octree
	FILE_Dataport *fp_in = new FILE_Dataport();
	if (!fp_in->ropen(argv[1])) {
		printf("Can't read input %s\n", argv[1]);
		exit(1);
	}
 
	char token[4096];
	get_next_token(fp_in, token);
	if (!strcmp(token, "SFC_MODEL_V1"))
		get_next_token(fp_in, token);
	Octree *oct_in = new Octree();
	oct_in->parse_in(fp_in);
	int levels = oct_in->get_max_levels();
	printf("Loaded input, %d levels\n", levels);
	Octree_Data *od = oct_in->get_data();
	

	for (;;) {
		printf("Enter model point X,Y,Z and radius (-1 for random): ");
		gets(token);
		if (sscanf(token, "%lf %lf %lf %lf", 
				&p[0], &p[1], &p[2], &r) != 4)
			continue;

		if (r < 0.0) {
			// random values +/- 2.0
			p[0] = 2.0 - 4.0*drand48();
			p[1] = 2.0 - 4.0*drand48();
			p[2] = 2.0 - 4.0*drand48();
			r = 2.0*drand48();
			printf("Using random point %f %f %f, r=%f\n",
				p[0], p[1], p[2], r);
		}
		printf("Exhaustive search for closest point...\n");
		double d2 = od->search_model_space(p, &match);
		match->get_global_center(m);
		printf("  dist=%f, point=%08X, coords=%f,%f,%f\n",
			sqrt(d2), match, m[0], m[1], m[2]);
	
		printf("Fast search for closest point...\n");
clock_t t0 = clock();
for (int rep=0; rep<NUM_REP; rep++) {
		d2 = od->fast_find_closest(levels, FLT_MAX, p, p, &match);
}
clock_t t1 = clock();
		match->get_global_center(m);
		printf("  dist=%f, point=%08X, coords=%f,%f,%f time=%f\n",
			sqrt(d2), match, m[0], m[1], m[2],
			(t1 - t0) / (float)CLOCKS_PER_SEC);

		printf("Old fast search for closest point...\n");
		d2 = od->fastest_find_closest(levels, FLT_MAX, p, p, &match);
		match->get_global_center(m);
		printf("  dist=%f, point=%08X, coords=%f,%f,%f\n",
			sqrt(d2), match, m[0], m[1], m[2]);

		
		printf("Exhaustive search for points within radius...\n");
		list = NULL;
		nmatch = search_radius(od, p, r*r, &list);
		show_search();

		printf("Fast search for points within radius...\n");
		list = NULL;
		nmatch = od->find_points_within_radius(levels, 
				p, p, r*r, &list);
		show_search();
	}
}

// old code for verification

// fastest_find_closest()
// Given a point from the small model (already converted to model space), 
// do a "smart" search in the large model to find the closest point.
// Also search neighboring cells, if they have the opportunity to
// find a closer match.

// The method's return value is the __square__ of the smallest distance in octree data/model space.
// Octree's fastest_find_closest() will take the sqrt to get "d," the distance.
//
// If the point isn't matched, fastest_find_closest() will return -1.
//
// K. Sturdevant 10-5-99

double
Octree_Data::fastest_find_closest(long top_level, double in_distance_sq, 
double glob_input_point[OCTREE_DIMS], double loc_input_point[OCTREE_DIMS],
NodeSpec **matched_node)
{
double distance_sq;
Octree_Data * child;
NodeSpec * node;
int	i,j,k;
int  subvol;
double local_center[OCTREE_DIMS];
double abs_x, abs_y, abs_z, test_dist, curr_d_sq, node_d_sq, child_d_sq;
static int xor_masks[] = {0, 1, 2, 4, 3, 5, 6, 7};
int child_num;

	// Check to see if there is a chance of improvement

	abs_x = fabs(loc_input_point[0]);
	abs_y = fabs(loc_input_point[1]);
	abs_z = fabs(loc_input_point[2]);

	//fprintf(stdout, "abs_x = %f, abs_y = %f, abs_z = %f\n", abs_x, abs_y, abs_z);

	// Use local distance to calculate distances to current voxel borders,
	// then convert to global space for comparison to in_distance_sq.
	// Top_level came from octree, and level is from the current Octree_Data.

	if (abs_x > 1.0)
	{
		abs_x = abs_x - 1.0;
		abs_x /= pow (2.0, (double)(top_level - level));
	}
	else
		abs_x = 0;

	if (abs_y > 1.0)
	{
		abs_y = abs_y - 1.0;
		abs_y /= pow (2.0, (double)(top_level - level));
	}
	else
		abs_y = 0;

	if (abs_z > 1.0)
	{
		abs_z = abs_z - 1.0;
		abs_z /= pow (2.0, (double)(top_level - level));
	}
	else
		abs_z = 0;

	// Calculate and compare distances in global units

	test_dist = pow(abs_x, 2.0) + pow(abs_y, 2.0) + pow(abs_z, 2.0);

	if ( test_dist > in_distance_sq )
	{
		//fprintf(stdout, "fastest_.. Could not improve in_distance_sq returning %f\n", in_distance_sq);
		return in_distance_sq;
	}
	else
	{
		// Get a child to test
		// Subvol chosen like it was in add_voxel(); want to narrow
		// in on most likely area.

		subvol = 0;
		for(i=0; i<OCTREE_DIMS ; i++) 
		{
			subvol <<= 1;
			if( loc_input_point[i] < 0.0)
				subvol++;
		}

		// Set the current distance to the input value, but do not edit the
		// input value.

		curr_d_sq = in_distance_sq;
		node_d_sq = FLT_MAX;

		for (i=0; i < NUMBER_OF_SPACE_PARTITIONS ; i++)
		{
			child_num = subvol ^ xor_masks[i];
 
			if ((child = get_child( (Octree_Child_Ref)(child_num))) != NULL )
        		{
				// Transform the input point into this local octree space
				// want to look at z, y, x, which are 0, 1, 2, so we
				// have to look k "backwards" to see them in the right
				// order.
				for(k=0; k < 3 ; k++) 
				{
					if ( (2-k) != 0)
					    j = (child_num >> (2-k) ) & 1;	
					else
					    j = child_num & 1;

					if (j) 
						local_center[k] = 2.0 * loc_input_point[k] + 1.0;
					else 
						local_center[k] = 2.0 * loc_input_point[k] - 1.0;
				}

				//fprintf(stdout, "Checking new center: %f %f %f\n", local_center[0],
				//	local_center[1], local_center[2]);

				if ((child_d_sq = 
				  child->fastest_find_closest(top_level, curr_d_sq, glob_input_point, 
                                        local_center, matched_node)) == -1)
				{
					fprintf(stderr, "Problem in fastest_find_closest().\n");
					fprintf(stderr, "Child %i returned -1.\n", i );
					return -1;
				}

				if (child_d_sq < curr_d_sq)
				{
					curr_d_sq = child_d_sq;
				}
			}
		}

		// Check Nodespecs, if it has them, and return the closest one.
		NodeSpec *n_match_node;
		if ((node = get_node_data()) != NULL)
		{
			//fprintf(stdout, "fastest_.. testing node\n");
			// Find the closest nodespec
			if ((node_d_sq = find_closest_nodespec(node, glob_input_point, &n_match_node)) == -1)
			{
				fprintf(stderr, "Problem in fastest_find_closest().\n");
				fprintf(stderr, "find_closest_nodespec() could not match point.\n" );
				return -1;
			}
			//fprintf(stdout, "fastest_.. node distance is %f\n", node_d_sq);
		}

		// Test if node is closer than children
		if (node_d_sq < curr_d_sq)
		{
			//fprintf(stdout, "fastest_.. node is closer than children\n");
                        *matched_node = n_match_node;
			distance_sq = node_d_sq;
		}
		else
		{
			//fprintf(stdout, "fastest_.. child is closer\n");
			distance_sq = curr_d_sq;
		}
        }

	//fprintf(stdout, "fastest_.. returning distance %f\n", distance_sq);

	// match_point should be set, so return the distance_sq
	return distance_sq;

}  // end fastest_find_closest()
