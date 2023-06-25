// octree.C 1.38 02/12/03 13:17:21
/** \file
 ** Implements functions for octree and nodespec classes
 ** John Wright	11/6/97
 **/

#include <float.h>
#include "grape/octree.h"
#include "grape/vector_ops.h"

/// Track node ID during file I/O to associate face vertices
static int node_id;

/* ***************************************************
NodeSpec methods
****************************************************/
void NodeSpec::set_color(int r, int g, int b, int a)
{
	set_color(r/255.0, g/255.0, b/255.0, a/255.0);
}

void NodeSpec::set_color(double r, double g, double b, double a)
{
#ifdef FLEXCOLOR
	if(nodecolor) free(nodecolor);
	nodecolor = new RgbColorSpec();
	((RgbColorSpec *)nodecolor)->set_color(r, g, b);
	nodecolor->set_alpha(a);
#else
	color[0] = r;
	color[1] = g;
	color[2] = b;
	color[3] = a;
#endif
}

int NodeSpec::get_color(double *r, double *g, double *b, double *a)
{
#ifdef FLEXCOLOR
	if(!nodecolor || nodecolor->get_num_bands() <= 0) {
		if(r) *r = 0.0;
		if(g) *g = 0.0;
		if(b) *b = 0.0;
		if(a) *a = 0.0;
		return(FALSE);
	}

	double lr, lg, lb;
	if(nodecolor->get_num_bands() >= 3) {
		((RgbColorSpec *)nodecolor)->get_color(lr, lg, lb);
	} else {
		lr = nodecolor->get_color(RGB_NDX_RED);
		if(nodecolor->get_num_bands() > 1) {
			lg = nodecolor->get_color(RGB_NDX_GREEN);
			lb = nodecolor->get_color(RGB_NDX_GREEN);
		} else {
			lg = nodecolor->get_color(RGB_NDX_RED);
			lb = nodecolor->get_color(RGB_NDX_RED);
		}
	}
	if(r) *r = lr;
	if(g) *g = lg;
	if(b) *b = lb;
	if(a) *a = nodecolor->get_alpha();
#else
	if(r) *r = color[0];
	if(g) *g = color[1];
	if(b) *b = color[2];
	if(a) *a = color[3];
#endif
	return(TRUE);
}

int NodeSpec::get_color(int *r, int *g, int *b, int *a)
{
	double lr, lg, lb, la;
	int status = get_color(&lr, &lg, &lb, &la);
	if(r) *r = (int)(lr*255.0 + 0.4);
	if(g) *g = (int)(lg*255.0 + 0.4);
	if(b) *b = (int)(lb*255.0 + 0.4);
	if(a) *a = (int)(la*255.0 + 0.4);
	return(status);
}

/// Generate a copy of this nodespec and return a pointer to it.
// (ought to be const function)
NodeSpec * NodeSpec::duplicate(NodeSpec *dup) 
{
	// allocate new nodespec if necessary
	if (dup == NULL)
		dup = new NodeSpec();

	// start with bitwise copy, then adjust
	// (note: don't try "*dup = *this", as that invokes copy
	// constructor which calls this method which ...)
	memcpy(dup, this, sizeof(NodeSpec));
	dup->next = NULL;

#ifdef FLEXCOLOR
	dup->nodecolor = NULL;
	double red, green, blue, alpha;
	get_color(&red, &green, &blue, &alpha);
        dup->set_color(red, green, blue, alpha);
#endif

        return dup;
}

// Parse in a double vector
static int read_vector(int n, double *vec, Dataport *fp)
{
	char token[4096];

	// for compatibility with old ldparam code, skip "LIST N "
	get_next_token(fp, token);
	get_next_token(fp, token);

	for (int i=0; i<n; i++) {
		if (!get_next_token(fp, token)) {
			fprintf(stderr," Whoops - Unexpected EOF in vector\n");
			return(FALSE);
		}
		vec[i] = atof(token);
	}
	return TRUE;
}

/// Load octree node from file
int NodeSpec::parse_in(Dataport *fp)
{
	char    token[4096];

	do {
		if(!get_next_token(fp, token)) {
			fprintf(stderr," Whoops - Unexpected EOF in octree nodespec entry\n");
			return(FALSE);
		}
		if(!strcmp(token, "OCTREE_NODE_V1")) {
			// do nothing
		} else if(!strcmp(token, "CENTER")) {
			if (!read_vector(OCTREE_DIMS, global_center, fp))
				return FALSE;
		} else if(!strcmp(token, "WEIGHT")) {
			if(!get_next_token(fp, token)) {
				fprintf(stderr," Whoops - Unexpected EOF in octree nodespec entry\n");
				return(FALSE);
			}
			weight = atof(token);
		} else if(!strcmp(token, "LENGTH")) {
			if(!get_next_token(fp, token)) {
				fprintf(stderr," Whoops - Unexpected EOF in octree nodespec entry\n");
				return(FALSE);
			}
			edge_length = atof(token);
		} else if(!strcmp(token, "COLOR")) {
			get_next_token(fp, token);	// assume "COLOR_SPEC_V1"
#ifdef FLEXCOLOR
			nodecolor = get_new_colorspec(fp);
			nodecolor->parse_in(fp);
#else
			// for compatibility, assume "NUM_BANDS 3 COLOR"
			int i;
			for (i=0; i<3; i++)
				get_next_token(fp, token);
			double c[3];
			if (!read_vector(3, c, fp))
				return(FALSE);
			for (i=0; i<3; i++)	// convert to floats
				color[i] = c[i];
			while (get_next_token(fp, token)) {
				if (!strcmp(token, "ALPHA")) {
					get_next_token(fp, token);
					color[3] = atof(token);
				} else if (!strcmp(token, "END_COLORSPEC")) {
					break;
				}
			}
#endif
		} else if(!strcmp(token, "NORMAL")) {
			if (!read_vector(OCTREE_DIMS, normal, fp))
				return FALSE;
		} else if(strcmp(token, "END_NODE")) {
			fprintf(stderr," Whoops - Unexpected token encountered in parsing octree nodespec entry\n");
			fprintf(stderr,"   Token=>%s<  Expecting one of CENTER LENGTH COLOR END_NODE\n", token);
			 return(FALSE);
		}
	} while(strcmp(token, "END_NODE"));
	id = node_id++;		// assign next node ID
	return(TRUE);
}

/// Save octree node to file
int NodeSpec::parse_out(Dataport *fp, int expand)
{
	char    token[4096];

	put_token(fp, "OCTREE_NODE_V1");

	sprintf(token, "CENTER LIST 3 %f %f %f", 
		global_center[0], global_center[1], global_center[2]);
	put_token(fp, token);

	sprintf(token, "LENGTH %f WEIGHT %f", edge_length, weight);
	put_token(fp, token);

	if (has_normal()) {
		sprintf(token, "\nNORMAL LIST 3 %f %f %f",
			normal[0], normal[1], normal[2]);
		put_token(fp, token);
	}

#ifdef FLEXCOLOR
	if(nodecolor) {
		put_token(fp, "COLOR");
		nodecolor->parse_out(fp, expand);
	}
#else
	put_token(fp, "COLOR COLOR_SPEC_V1 NUM_BANDS 3 COLOR LIST 3");
	
	sprintf(token, "%f %f %f ALPHA %f END_COLORSPEC",
		color[0], color[1], color[2], color[3]);
	put_token(fp, token);
#endif

	put_token(fp, "END_NODE\n");
	id = node_id++;		// assign next node ID

	return(TRUE);
}

/// Save voxel node in binary format (note, no expand option)	
void NodeSpec::write_binary(Dataport *fp)
{
	BRHeader hdr;
	int r, g, b;

	// indicate proper endianness for floats
#if REAL_BIGENDIAN
	hdr.flags = BRVOXEL_FLAGS_BENDIAN;
#else
	hdr.flags = BRVOXEL_FLAGS_LENDIAN;
#endif
	hdr.nbytes = 4*sizeof(float);
#ifdef FLEXCOLOR
	if (nodecolor) {
#else
	{
#endif
		get_color(&r, &g, &b);
		if (r==g && r==b) {
			hdr.flags |= BRVOXEL_FLAGS_GRAY;
			hdr.nbytes += sizeof(uchar);
		} else {
			hdr.flags |= BRVOXEL_FLAGS_RGB;
			hdr.nbytes += 3*sizeof(uchar);
		}
	}

	if (weight != 1.0) {	// not default
		hdr.flags |= BRVOXEL_FLAGS_WEIGHT;
		hdr.nbytes += sizeof(float);
	}
	if (has_normal()) {
		hdr.flags |= BRVOXEL_FLAGS_NORMAL;
		hdr.nbytes += 3*sizeof(float);
	}
		
	fp->write(&hdr, sizeof(hdr));
				
	// always write center and length
	float fbuf[4];
	fbuf[0] = global_center[0];
	fbuf[1] = global_center[1];
	fbuf[2] = global_center[2];
	fbuf[3] = edge_length;
	fp->write(fbuf, 4*sizeof(float));

	if (hdr.flags & BRVOXEL_FLAGS_RGB) {
		uchar rgb[3];
		rgb[0] = r;
		rgb[1] = g;
		rgb[2] = b;
		fp->write(rgb, 3);
	}

	if (hdr.flags & BRVOXEL_FLAGS_WEIGHT) {
		fbuf[0] = weight;
		fp->write(fbuf, sizeof(float));
	}

	if (hdr.flags & BRVOXEL_FLAGS_NORMAL) {
		fbuf[0] = normal[0];
		fbuf[1] = normal[1];
		fbuf[2] = normal[2];
		fp->write(fbuf, 3*sizeof(float));
	}

	if (hdr.flags & BRVOXEL_FLAGS_GRAY) {
		uchar gray = r;
		fp->write(&gray, 1);
	}

	id = node_id++;		// assign next node ID
}

// swap 4-byte values from big-endian to little or vice-versa
static void swap4(void *p, int n)
{
	char *cp = (char *)p;
	while (--n >= 0) {
		char t = cp[0];
		cp[0] = cp[3];
		cp[3] = t;
		t = cp[1];
		cp[1] = cp[2];
		cp[2] = t;
		cp += 4;
	}
}

/// Load voxel node in binary format, record header already read in
void NodeSpec::read_binary(BRHeader *hdr, Dataport *fp)
{
	int nread = 0;	// record data read so far
	float fbuf[4];

	// need to swap bytes on floats?
#if REAL_BIGENDIAN
	int doswap = (hdr->flags & 0xC0) == BRVOXEL_FLAGS_LENDIAN;
#else
	int doswap = (hdr->flags & 0xC0) == BRVOXEL_FLAGS_BENDIAN;
#endif

	// always starts with global center (model coords) 
	// and edge length
	fp->read(fbuf, 4*sizeof(float));
	nread = 4*sizeof(float);
	if (doswap) swap4(fbuf, 4);
	global_center[0] = fbuf[0];
	global_center[1] = fbuf[1];
	global_center[2] = fbuf[2];
		
	edge_length = fbuf[3];
	id = node_id++;		// track file index for mesh mapping

	// optional color (alpha assumed to be 1.0)
	if (hdr->flags & BRVOXEL_FLAGS_RGB) {
		uchar rgb[3];
		fp->read(rgb, 3);
		nread += sizeof(rgb);
		set_color(rgb[0], rgb[1], rgb[2]);
	}

	// optional node weight
	if (hdr->flags & BRVOXEL_FLAGS_WEIGHT) {
		fp->read(fbuf, sizeof(float));
		nread += sizeof(float);
		if (doswap) swap4(fbuf, 1);
		weight = fbuf[0];
	}

	// optional surface normal
	if (hdr->flags & BRVOXEL_FLAGS_NORMAL) {
		fp->read(fbuf, 3*sizeof(float));
		nread += 3*sizeof(float);
		if (doswap) swap4(fbuf, 3);
		normal[0] = fbuf[0];
		normal[1] = fbuf[1];
		normal[2] = fbuf[2];
	}

	// optional grayscale (alpha assumed to be 1.0)
	if (hdr->flags & BRVOXEL_FLAGS_GRAY) {
		uchar gray;
		fp->read(&gray, 1);
		nread++;
		set_color(gray, gray, gray);
	}


	// skip any unread record data (upward compatibility)
	if (nread < hdr->nbytes) {
		uchar junk[256];
		fp->read(junk, hdr->nbytes - nread);
	}
}

/* ***************************************************
Octree_Data methods
****************************************************/

/// Does octree cell have any children (i.e. is it subdivided)?
int Octree_Data::any_children(void)
{
	int	i,status=0;

	for(i=UPPER_LEFT_BACK; i<NUMBER_OF_SPACE_PARTITIONS; i++) {
		if(child[i]) status++;
	}
	return(status);
}

/// Get coordinates of the node in list closest to given coordinates
/**
// Given a pointer to a nodespec, run down its list and
// calculate the distance to each node.  Return the coordinates of the closest
// point and the distance squared.
*/
double
Octree_Data::find_closest_nodespec(NodeSpec * node, double input_point[OCTREE_DIMS], 
					double match_point[OCTREE_DIMS])
{
	double distance_sq = FLT_MAX;
	double temp_dsq;
	double node_center[OCTREE_DIMS];

        while (node) {
        	node->get_global_center(node_center);

                // Calculate the distance d2 = (xdiff + ydiff + zdiff)
		temp_dsq = (node_center[0] - input_point[0]) * (node_center[0] - input_point[0]) +
			(node_center[1] - input_point[1]) * (node_center[1] - input_point[1]) +
			(node_center[2] - input_point[2]) * (node_center[2] - input_point[2]);

                // If the node distance is less than the current, replace it.
                if (temp_dsq < distance_sq) {
                        distance_sq = temp_dsq;
                        match_point[0] = node_center[0];
                        match_point[1] = node_center[1];
                        match_point[2] = node_center[2];
                }

        	node = node->next;
        }
	return distance_sq;
}

/// Get pointer to the node in list closest to given coordinates
// same as above, but return closest nodespec
double
Octree_Data::find_closest_nodespec(NodeSpec * node, 
		double input_point[OCTREE_DIMS], NodeSpec **match_node)
{
	double distance_sq = FLT_MAX;
	double temp_dsq;
	double node_center[OCTREE_DIMS];

        while (node) {

                node->get_global_center(node_center);

                // Calculate the distance d2 = (xdiff + ydiff + zdiff)
		temp_dsq = (node_center[0] - input_point[0]) * (node_center[0] - input_point[0]) +
			(node_center[1] - input_point[1]) * (node_center[1] - input_point[1]) +
			(node_center[2] - input_point[2]) * (node_center[2] - input_point[2]);

                 // If the node distance is less than the current, replace it.
                if (temp_dsq < distance_sq) {
                        distance_sq = temp_dsq;
			*match_node = node;
                }

        	node = node->next;
        }
	return distance_sq;
}

/// Exhaustive recursive search of octree for closest point. 
/**
// This works even if the octree partitioning of voxels isn't right.
// Returns the square of the smallest distance, and the matched octree point.
*/
double
Octree_Data::search_model_space(double input_point[OCTREE_DIMS], 
		NodeSpec **matched_node)
{
	Octree_Data * kid;
	NodeSpec *temp_node;
	double 	temp_dsq;
	int	i;

	// Loop through nodespecs at this level and check their distances,
	// keeping the shortest one.
	double distance_sq = find_closest_nodespec(get_node_data(), 
					input_point, matched_node);

	// Recursively test all children
	for (i=UPPER_LEFT_BACK; i< NUMBER_OF_SPACE_PARTITIONS; i++) {
		if ((kid = get_child( Octree_Child_Ref (i))) != NULL) {
                	temp_dsq = kid->search_model_space(input_point, 
						&temp_node);

			// Compare child's best to current minimum.
			if (temp_dsq < distance_sq) {
				distance_sq = temp_dsq;
				*matched_node = temp_node;
			}
		}
	}

	// Return the square of the shortest distance
	return distance_sq;
}

// test distance (squared) to edge of octree cell (?)
static double local_dist(double loc_input_point[], int dlevel)
{
	double edge = 1.0 / (1 << dlevel);
	double test_dist = 0.0;
	
	double abs_x = fabs(loc_input_point[0]);
	if (abs_x > 1.0) {
		abs_x = (abs_x - 1.0) * edge;
		test_dist = abs_x * abs_x;
	}

	double abs_y = fabs(loc_input_point[1]);
	if (abs_y > 1.0) {
		abs_y = (abs_y - 1.0) * edge;
		test_dist += abs_y * abs_y;
	}

	double abs_z = fabs(loc_input_point[2]);
	if (abs_z > 1.0) {
		abs_z = (abs_z - 1.0) * edge;
		test_dist += abs_z * abs_z;
	}
	return test_dist;
}

/// Smart octree search for point closest to specified coordinates.
/**
// Do a "smart" search in the model to find the closest point to specified
// coordinates. Also search neighboring cells, if they have the opportunity 
// to find a closer match.
// The method's return value is the square of the smallest distance in 
// octree data/model space. The matching point coordinates are also returned.
//
// K. Sturdevant 10-5-99
*/
double
Octree_Data::fast_find_closest(long top_level, double in_distance_sq, 
	double glob_input_point[OCTREE_DIMS], double loc_input_point[OCTREE_DIMS], 
	double match_point[OCTREE_DIMS])
{
	NodeSpec *ns = NULL;
	double d2 = fast_find_closest(top_level, in_distance_sq, glob_input_point,
			loc_input_point, &ns);
	if (ns == NULL)		// hmm, must be empty tree
		return -1.0;
	ns->get_global_center(match_point);
	return d2;
}

/// Smart octree search for node closest to specified coordinates.
// Same as above, but return the closest nodespec instead of coordinates.
double
Octree_Data::fast_find_closest(long top_level, double in_distance_sq, 
	double glob_input_point[OCTREE_DIMS], double loc_input_point[OCTREE_DIMS], 
	NodeSpec **matched_node)
{
	Octree_Data * kid;
	NodeSpec * node, *n_match_node;
	int	i,j,k;
	int  	subvol;
	double 	local_center[OCTREE_DIMS];
	double 	curr_d_sq, node_d_sq, child_d_sq;
	static 	const int xor_masks[] = {0, 1, 2, 4, 3, 5, 6, 7};
	int 	child_num;

	// Check to see if there is a chance of improvement.
	// Use local distance to calculate distances to current voxel borders,
	// then convert to global space for comparison to in_distance_sq.
	// Top_level is from octree, and level is from the current Octree_Data.
	if (local_dist(loc_input_point, top_level - level) > in_distance_sq )
		return in_distance_sq;

	// Get a child to test
	// Subvol chosen like it was in add_voxel(); want to narrow
	// in on most likely area.

	subvol = 0;
	for(i=0; i<OCTREE_DIMS ; i++) {
		subvol <<= 1;
		if( loc_input_point[i] < 0.0)
			subvol++;
	}

	// Set the current distance to the input value, but do not edit the
	// input value. (?)
	curr_d_sq = in_distance_sq;

	for (i=0; i < NUMBER_OF_SPACE_PARTITIONS ; i++) {
		child_num = subvol ^ xor_masks[i];
 
		if ((kid = get_child( (Octree_Child_Ref)(child_num))) != NULL ) {
			// Transform the input point into this local octree space
			// want to look at z, y, x, which are 0, 1, 2, so we
			// have to look k "backwards" to see them in the right
			// order.
			for(k=0; k < 3 ; k++) {
				if ( (2-k) != 0)
				    j = (child_num >> (2-k) ) & 1;	
				else
				    j = child_num & 1;

				if (j) 
					local_center[k] = 2.0 * loc_input_point[k] + 1.0;
				else 
					local_center[k] = 2.0 * loc_input_point[k] - 1.0;
			}

			child_d_sq = kid->fast_find_closest(top_level, curr_d_sq, 
					glob_input_point, local_center, matched_node);

			if (child_d_sq < curr_d_sq)
				curr_d_sq = child_d_sq;
		}
	}

	// Check Nodespecs, if it has them, and return the closest one.
	if ((node = get_node_data()) != NULL) {
		// Find the closest nodespec
		node_d_sq = find_closest_nodespec(node, glob_input_point, &n_match_node);

		// Test if node is closer than children
		if (node_d_sq < curr_d_sq) {
			*matched_node = n_match_node;
			curr_d_sq = node_d_sq;
		}
        }

	// match_point should be set, so return the distance_sq
	return curr_d_sq;

}  // end fast_find_closest()

/// Search for nodes within specified distance of coordinates
/*
// Returns a list of Nodespecs within radius "r" of the given point.
// Also, returns the number of points found.
// NOTE: argument "radius" is really the square of the radius.
*/
int
Octree_Data::find_points_within_radius(long top_level, double glob_input_point[OCTREE_DIMS], 
   double loc_input_point[OCTREE_DIMS], double radius, NodeSpec_List_Element ** node_list)
{
	Octree_Data * kid;
	NodeSpec * node;
	int	i;
	int  subvol;
	double local_center[OCTREE_DIMS];
	static const int xor_masks[] = {0, 1, 2, 4, 3, 5, 6, 7};
	int child_num;
	NodeSpec_List_Element * node_dup;
	int node_counter = 0;

	// Check to see if there is a chance of improvement
	// Use local distance to calculate distances to current voxel borders,
	// then convert to global space for comparison to radius.
	// Top_level is from octree, and level is from the current Octree_Data.
	if (local_dist(loc_input_point, top_level - level) > radius)
		return 0;

        subvol = 0;
        if (loc_input_point[0] < 0.0)
        	subvol = 4;
        if (loc_input_point[1] < 0.0)
        	subvol += 2;
        if (loc_input_point[2] < 0.0)
        	subvol += 1;

        for (i=0; i < NUMBER_OF_SPACE_PARTITIONS ; i++) {
                child_num = subvol ^ xor_masks[i];

		if ((kid = get_child( (Octree_Child_Ref)(child_num))) != NULL ) {
 			// Transform the input point into this local octree space
 			// want to look at z, y, x, which are 0, 1, 2, so we
                        // have to look k "backwards" to see them in the right order.

			double dctr = (child_num&4) ? 1.0 : -1.0;
			local_center[0] = 2.0 * loc_input_point[0] + dctr;

			dctr = (child_num&2) ? 1.0 : -1.0;
			local_center[1] = 2.0 * loc_input_point[1] + dctr;

			dctr = (child_num&1) ? 1.0 : -1.0;
			local_center[2] = 2.0 * loc_input_point[2] + dctr;

                        node_counter += kid->find_points_within_radius(top_level, 
                        	glob_input_point, local_center, radius, node_list);
                }
	}

	// Loop through nodespecs at this level and compare their distances to the radius
	node = get_node_data();
	while (node) {
		double dctr[OCTREE_DIMS];
		node->get_global_center(dctr);
		vector_diff(dctr, glob_input_point, dctr);

		// Calculate the distance squared;
		// If the distance to the point is less than or equal to
		// the given radius, add it to the list of points.
		if (vector_magnitude_sqr(dctr) <= radius) {
			node_counter++;
			node_dup = new NodeSpec_List_Element(node);

			node_dup->next = *node_list;
			*node_list = node_dup;
		}

		node = node->next;
	}

	// Return the number of nodes added to the nodelist. 
	return node_counter;
}

/*
** match_rgb() is an extention to get_rgb().  Rather than looking for an
** exact match, it will return a "weighted" value to help to determine
** how "good" the match is.
** K. Sturdevant 7-8-99
*/

float Octree_Data::match_rgb(double input_center[OCTREE_DIMS], double *r, double *g, double *b, double *a)
{
	double	local_center[OCTREE_DIMS];
	long	i, subvol;
	int	status;
	float	tally = 0.0;
	NodeSpec * nsptr;
	

	if(!in_volume(input_center)) {
		return(0);
	}

	if (level == 0) 			// get from this level
        {
	    nsptr = nodespec;
	    while(nsptr) 
            {
		nsptr->get_color(r, g, b, a);

		// If alpha is > 0, the voxel is known and the
		// value should be tallied.
		// If alpha is < 0, the voxel is unknown and the
		// value should be talled.
		// If alpha == 0, we "add 0" to the running tally.
		if (*a != 0)
		{
		    tally += *a;
		}
		nsptr = nsptr->next;
	    }
	} else {
		subvol = 0;
		for(i=0; i<OCTREE_DIMS ; i++) {
			subvol <<= 1;
			if(input_center[i] < 0.0) {
				subvol++;
				local_center[i] = 2.0 * input_center[i] + 1.0;
			} else {
				local_center[i] = 2.0 * input_center[i] - 1.0;
			}
		}

		// Get the child's value, and tally the value of its nodes.

		if ( child[subvol] ) 
		{
			tally = child[subvol]->match_rgb(local_center, r, g, b, a);

			// check for errors? kfs

	    		nsptr = nodespec;
			while (nsptr) 
			{
			    status = nodespec->get_color(r, g, b, a);

			    // what do if can't get color? kfs
			    if (!status)
			    {
				fprintf(stderr, "get_color() failed, continuing\n");
				continue;
                            }
		
			    // Add alpha to tally for known and empty voxels.	
			    if (*a != 0.0)
			    {
		    		tally += *a;
			    }
			    nsptr = nsptr->next;
			}
	        }
	}

	return(tally);
}

int Octree_Data::get_rgb(double input_center[OCTREE_DIMS], int *r, int *g, int *b, int *a)
{
	double	local_center[OCTREE_DIMS];
	long	i, subvol;
	int	status;
	
	if(!in_volume(input_center)) {
		return(FALSE);
	}

	if(level == 0) {		// get from this level
		if(nodespec) {
			nodespec->get_color(r, g, b, a);
			return(TRUE);
		} else {
			return(FALSE);
		}
	} else {
		subvol = 0;
		for(i=0; i<OCTREE_DIMS ; i++) {
			subvol <<= 1;
			if(input_center[i] < 0.0) {
				subvol++;
				local_center[i] = 2.0 * input_center[i] + 1.0;
			} else {
				local_center[i] = 2.0 * input_center[i] - 1.0;
			}
		}
		status = FALSE;
		if(child[subvol]) {
			status = child[subvol]->get_rgb(local_center, r, g, b, a);
		} 
		if(!status) {
			if(nodespec) {
				status = nodespec->get_color(r, g, b, a);
			}
		}
		return(status);
	}
}


int Octree_Data::get_rgb(double input_center[OCTREE_DIMS], double *r, double *g, double *b, double *a)
{
	double	local_center[OCTREE_DIMS];
	long	i, subvol;
	int	status;
	
	if(!in_volume(input_center)) {
		return(FALSE);
	}

	if(level == 0) {		// get from this level
		if(nodespec) {
			nodespec->get_color(r, g, b, a);
			return(TRUE);
		} else {
			return(FALSE);
		}
	} else {
		subvol = 0;
		for(i=0; i<OCTREE_DIMS ; i++) {
			subvol <<= 1;
			if(input_center[i] < 0.0) {
				subvol++;
				local_center[i] = 2.0 * input_center[i] + 1.0;
			} else {
				local_center[i] = 2.0 * input_center[i] - 1.0;
			}
		}
		status = FALSE;
		if(child[subvol]) {
			status = child[subvol]->get_rgb(local_center, r, g, b, a);
		} 
		if(!status) {
			if(nodespec) {
				status = nodespec->get_color(r, g, b, a);
			}
		}
		return(status);
	}
}

#ifdef FLEXCOLOR
ColorSpec *Octree_Data::get_color(double input_center[OCTREE_DIMS])
{
	double	local_center[OCTREE_DIMS];
	long	i, subvol;
	ColorSpec	*cspec;


	if(!in_volume(input_center)) {
		return(NULL);
	}

	if(level == 0) {		// get from this level
		if(nodespec) {
			return(nodespec->get_color());
		} else {
			return(NULL);
		}
	} else {
		subvol = 0;
		for(i=0; i<OCTREE_DIMS ; i++) {
			subvol <<= 1;
			if(input_center[i] < 0.0) {
				subvol++;
				local_center[i] = 2.0 * input_center[i] + 1.0;
			} else {
				local_center[i] = 2.0 * input_center[i] - 1.0;
			}
		}
		cspec = NULL;
		if(child[subvol]) {
			cspec = child[subvol]->get_color(local_center);
		} 
		if(!cspec) {
			if(nodespec) {
				cspec = nodespec->get_color();
			}
		}
		return(cspec);
	}
}
#endif

/// Add sample point to octree
long Octree_Data::add_sample(NodeSpec *ns, double input_center[OCTREE_DIMS])
{
	double	local_center[OCTREE_DIMS], dummy[OCTREE_DIMS];
	long	i, subvol;
	NodeSpec	*tns1, *tns2;
	
	if(!input_center) {
		input_center = dummy;
		ns->get_global_center(input_center);
	}

	if(level == 0) {		// at bottom, add at this level
		add_data(ns, input_center);
	} else {			// push all nodes to lower level
		// push old node list to lower level
		tns1 = nodespec;
		while(tns1) {
			// remove each node from list
			tns2 = tns1;
			tns1 = tns2->next;
			tns2->next = NULL;
			// push down to child node
			subvol = 0;
			tns2->get_local_center(local_center);
			for(i=0; i<OCTREE_DIMS ; i++) {
				subvol <<= 1;
				if(local_center[i] < 0.0) {
					subvol++;
					local_center[i] = 2.0 * local_center[i] + 1.0;
				} else {
					local_center[i] = 2.0 * local_center[i] - 1.0;
				}
			}
			if(!child[subvol]) {
				child[subvol] = new Octree_Data(level-1);
			}
			child[subvol]->add_sample(tns2, local_center);
		}
		nodespec = NULL;

		if(any_children()) {
			// push new node to lower level
			subvol = 0;
			for(i=0; i<OCTREE_DIMS ; i++) {
				subvol <<= 1;
				if(input_center[i] < 0.0) {
					subvol++;
					local_center[i] = 2.0 * input_center[i] + 1.0;
				} else {
					local_center[i] = 2.0 * input_center[i] - 1.0;
				}
			}
			if(!child[subvol]) {
				child[subvol] = new Octree_Data(level-1);
			}
			child[subvol]->add_sample(ns, local_center);
		} else {
			add_data(ns, input_center);
		}

	}
	return(level);
}

/// Add voxel node to octree
long Octree_Data::add_voxel(NodeSpec *ns, double input_center[OCTREE_DIMS], double input_size)
{
	double	local_center[OCTREE_DIMS], dummy[OCTREE_DIMS];
	long	i, subvol;
	
	if(ns->edge_length <= 0.0 && input_size <= 0.0) {
		return(add_sample(ns, input_center));
	}

	if(input_size < 0.0) input_size = ns->edge_length;
	if(!input_center) {
		input_center = dummy;
		ns->get_global_center(input_center);
	}

	if(level == 0) {		// add at this level
		add_data(ns, input_center);
		return(level);
	} else if(input_size >= 1.50) {		// size is right
		add_data(ns, input_center);
		return(level);
	} else {
		subvol = 0;
		for(i=0; i<OCTREE_DIMS ; i++) {
			subvol <<= 1;
			if(input_center[i] < 0.0) {
				subvol++;
				local_center[i] = 2.0 * input_center[i] + 1.0;
			} else {
				local_center[i] = 2.0 * input_center[i] - 1.0;
			}
		}

		if(!child[subvol]) {
			child[subvol] = new Octree_Data(level-1);
		}

		// transform voxel to node space
		return(child[subvol]->add_voxel(ns, local_center, input_size*2.0));
	}
}

/// Perform weighted accumulation of specified nodespec and this node
// edge length is not affected
void
Octree_Data::accumulate_values(NodeSpec *ns, double weight)
{
	double cur_red, cur_green, cur_blue, cur_alpha;
	double ns_red, ns_green, ns_blue, ns_alpha;

	NodeSpec *cur = nodespec;
	cur->use_alpha(TRUE);  //needed?

	double total_weight = cur->get_weight() + weight;
	if (total_weight == 0)
		return;
	double cweight = cur->get_weight() / total_weight;
	double nweight = weight / total_weight;

	// Adjust color
	cur->get_color(&cur_red, &cur_green, &cur_blue, &cur_alpha);
	ns->get_color(&ns_red, &ns_green, &ns_blue, &ns_alpha);
	nodespec->set_color(
		cur_red * cweight + ns_red * nweight,
		cur_green * cweight + ns_green * nweight,
		cur_blue * cweight + ns_blue * nweight,
		cur_alpha * cweight + ns_alpha * nweight);

	double cvec[OCTREE_DIMS], nvec[OCTREE_DIMS], acc_vec[OCTREE_DIMS];
	// Adjust normal
	int i;
	if (cur->get_normal(cvec) && ns->get_normal(nvec)) {
		for (i=0; i < OCTREE_DIMS; i++)
    			acc_vec[i] = cvec[i] * cweight + nvec[i] * nweight;
		normalize_vector(acc_vec);
	        nodespec->set_normal(acc_vec);
	}

	// Adjust local and global centers
	cur->get_local_center(cvec);
	ns->get_local_center(nvec);
	for (i=0; i < OCTREE_DIMS; i++)
    		acc_vec[i] = cvec[i] * cweight + nvec[i] * nweight;
	nodespec->set_local_center(acc_vec);

	cur->get_global_center(cvec);
	ns->get_global_center(nvec);
	for (i=0; i < OCTREE_DIMS; i++)
    		acc_vec[i] = cvec[i] * cweight + nvec[i] * nweight;
	nodespec->set_global_center(acc_vec);

	nodespec->set_weight(total_weight);
}

/// Accumulate node, or add node if no existing node at this cell
void Octree_Data::add_or_acc(NodeSpec *ns, double input_center[OCTREE_DIMS],double weight)
{
	double r,g,b,a;
	ns->get_color(&r,&g,&b,&a);

        // Check if data already exists here, if not add.
        // If so, merge the information.
        if (nodespec == NULL)
        {
                add_data(ns, input_center);
        }
        else
        {
                // Could add weight to nodespec rather than sending it 
                // as arg. John's suggestion. kfs
                accumulate_values(ns, weight);
        }
}

long Octree_Data::accumulate_voxel(NodeSpec *ns, double input_center[OCTREE_DIMS], double input_size, double weight)
{
	double	local_center[OCTREE_DIMS], dummy[OCTREE_DIMS];
	long	i, subvol;
	
	if(input_size < 0.0) input_size = ns->edge_length;

	if(!input_center) {
		input_center = dummy;
		ns->get_global_center(input_center);
	}

	if (level == 0) {		// add at this level
		add_or_acc(ns, input_center);
		return(level);
	} else if(input_size >= 1.50) {		// size is right
		add_or_acc(ns, input_center);
		return(level);
	} else {
		subvol = 0;
		for(i=0; i<OCTREE_DIMS ; i++) {
			subvol <<= 1;
			if(input_center[i] < 0.0) {
				subvol++;
				local_center[i] = 2.0 * input_center[i] + 1.0;
			} else {
				local_center[i] = 2.0 * input_center[i] - 1.0;
			}
		}

		if(!child[subvol]) {
			child[subvol] = new Octree_Data(level-1);
		}

		// transform voxel to node space
		return(child[subvol]->accumulate_voxel(ns, local_center, input_size*2.0, weight));
	}
}

long Octree_Data::accumulate_at_leaf(NodeSpec *ns, double input_center[OCTREE_DIMS], double input_size)
{
	double  local_center[OCTREE_DIMS], global_center[OCTREE_DIMS], dummy[OCTREE_DIMS];
	long i, subvol;
	NodeSpec * ns_in_copy, * node_cur_copy;
	long ret_level;
	double quarter_edge;
	double new_global[OCTREE_DIMS];

        if(input_size < 0.0)
                input_size = ns->edge_length;

        if(!input_center) {
                input_center = dummy;
                ns->get_global_center(input_center);
        }

        if(level == 0) {                // add at this level
		add_or_acc(ns, input_center);
                return level;
        } else if(input_size >= 1.50) {         // size is right
                // If there are no children, add here
                if (any_children() == 0)
                {
			add_or_acc(ns, input_center);
                        return level;
                }
                else
                {
                        for (i=0; i < NUMBER_OF_SPACE_PARTITIONS; i++)
                        {
                                // Copy incoming nodespec, unless we're the last child, then use orig.
				if ( i == (OCTREE_DIMS -1))
					ns_in_copy = ns;
				else
                                        ns_in_copy = ns->duplicate();

                                // If child[i] is null, create new child[i]
                                if ((get_child( Octree_Child_Ref(i))) == NULL )
                                        child[i] = new Octree_Data(level-1);

                                ret_level = child[i]->accumulate_at_leaf(ns_in_copy, input_center, input_size);
                        }
			return ret_level;
                }

        } else {
                // Subdivide this nodespec to go farther down the tree

		// If have > 1 nodespec, may want to accumulate them together, then
		// divide and push down as 1. kfs
                if (nodespec)
                {
			nodespec->get_global_center(global_center);

                        for( i=0; i < NUMBER_OF_SPACE_PARTITIONS; i++)
                        {
                                // Copy current nodespec, unless we're the last child, then use orig.
				if ( i == (NUMBER_OF_SPACE_PARTITIONS -1))
					node_cur_copy = nodespec;
				else
                                        node_cur_copy = nodespec->duplicate();

                                // If child[i] is null, create new child[i]
                                if ((get_child( Octree_Child_Ref(i))) == NULL )
                                        child[i] = new Octree_Data(level-1);
	
				quarter_edge = 0.25 * nodespec->edge_length;

				// Modify the global center-- +/- .25*edge_length

				new_global[0] = global_center[0] + quarter_edge*X_Offset[i];
				new_global[1] = global_center[1] + quarter_edge*Y_Offset[i];
				new_global[2] = global_center[2] + quarter_edge*Z_Offset[i];

				node_cur_copy->set_global_center(new_global);
				node_cur_copy->edge_length /= 2.0;

				fprintf(stderr, "global_center = %f %f %f\n", new_global[0],new_global[1],new_global[2]);

                                ret_level = child[i]->accumulate_at_leaf(node_cur_copy, NULL, 2.0);
                        }

			// Since nodespec was pushed down, release it from this level.
                        release_nodespec();
                }

		subvol = 0;
		for(i=0; i<OCTREE_DIMS ; i++) {
			subvol <<= 1;
			if(input_center[i] < 0.0) {
				subvol++;
				local_center[i] = 2.0 * input_center[i] + 1.0;
			} else {
				local_center[i] = 2.0 * input_center[i] - 1.0;
			}
		}

		if(!child[subvol])
			child[subvol] = new Octree_Data(level-1);

		// transform voxel to node space
		return(child[subvol]->accumulate_at_leaf(ns, local_center, input_size*2.0));

	}
}

/// Accumulate all voxels (NodeSpecs) at this node below into a single NodeSpec
void Octree_Data::accumulate_node()
{
	// if nothing to accumulate to, create a weightless node
	if (!nodespec) {
		static double xyz[] = {0,0,0};
		static double xy1[] = {0,0,1};
		nodespec = new NodeSpec;
		nodespec->set_color(0,0,0,0);
		nodespec->set_local_center(xyz);
		nodespec->set_global_center(xyz);
		nodespec->set_normal(xy1);
		nodespec->set_weight(0.0);
		// edge length set per first accumulated child
	}

	// accumulate any children (recursively)
	for (int i=UPPER_LEFT_BACK; i<NUMBER_OF_SPACE_PARTITIONS; i++) {
		Octree_Data *kid = child[i];
		if (kid) {
			kid->accumulate_node();
			accumulate_values(kid->get_node_data());
			if (nodespec->edge_length == 0.0)
				nodespec->edge_length = 2.0 * 
					kid->get_node_data()->edge_length;
		}
	}

	// accumulate any additional voxels at this node
	NodeSpec *next_ns;
	for (next_ns = nodespec->next; next_ns; next_ns = next_ns->next)
		accumulate_values(next_ns);
	delete nodespec->next;
	nodespec->next = NULL;
}

/// Determine deepest instantiated level of octree
long Octree_Data::get_levels(void)
{
	int  i;
	long tlvl, lvl = 0;

	if(level > 0) {
		for(i=UPPER_LEFT_BACK; i<NUMBER_OF_SPACE_PARTITIONS; i++) {
			if(child[i]) {
				tlvl = child[i]->get_levels();
				if(tlvl > lvl) lvl = tlvl;
			}
		}
	}
	lvl++;
	return(lvl);
}

/// Set (reduce) octree depth, removing or accumulating, lower-level nodes
long Octree_Data::set_level(long lvl, int accum)
{
	int	i;

	// if(lvl > level)
	// 	may want to push multiple nodespecs down the tree

	if(lvl > 0) {	// set level for all children
		for(i=UPPER_LEFT_BACK; i<NUMBER_OF_SPACE_PARTITIONS; i++) {
			if(child[i]) 
				child[i]->set_level(lvl-1, accum);
		}
	} else if(lvl == 0) {	// delete all children
		if (accum)
			accumulate_node();
		for(i=UPPER_LEFT_BACK; i<NUMBER_OF_SPACE_PARTITIONS; i++) {
			if(child[i]) {
				delete(child[i]);
				child[i] = NULL;
			}
		}
	}
	level = lvl;
	return(level);
}

/// Condense the octree
/**
//    When all of an octree's children are the same, the octree
//    can be condensed (pruned) to contain just the parent.  
//    If no compare function is provided, the method will default.
//
//    K. Sturdevant 10-16-98
// Note:  may set this up to take an input to indicate which
// type of comparison is to be made.  Then, switch on the
// type and call the appropriate comparison method.  This way,
// the higher level does not have to know the method name, 
// should Octree_Data provide more than one.  The current
// method format assumes the user will provide a different
// method, should the default be unsuitable.
*/
int Octree_Data:: condense(int (* compare_func)())
{
	Octree_Data  *childA;  // Save first child
	NodeSpec     *nodeA;

	Octree_Data  *childX;  // Generic child
	NodeSpec     *nodeX;
	int i;
        int no_condense = 0;
	double      glob_ctr[OCTREE_DIMS];
	double      new_glob_ctr[OCTREE_DIMS] = { 0.0 };

	// Test if child is set and all are present.  If any are 
	// missing, return.  Returning 1 means the value of this
        // branch will not negatively affect the condensing.  No
	// children is OK, but not enough or mismatching children
	// indicates condensing cannot occur.

	if (any_children() == 0)		
	{
		return 1;
	}

	// Recursively condense children
	// If any of the children cannot be condensed, then the parent
        // cannot be condensed.  Condense all that can be.  Let level
        // above know if one or more failed.

	for (i=UPPER_LEFT_BACK; i< NUMBER_OF_SPACE_PARTITIONS; i++)
	{
		// The return value matters here, because we cannot prune
		// the level above, unless the levels below are pruneable.

		if ((childX = get_child( Octree_Child_Ref (i))) != NULL)
		{
                	if (!childX->condense(compare_func))
			{
			    no_condense = 1;
			}
		}
	}

	// Since one of the children failed to condense, we know
        // this level cannot be condensed, so we return.
	if (no_condense)
        {
            return 0;
	}

	// Compare the children to each other.  If they match,
	// return 1, if not, return 0.

	if ( compare_child_list(compare_func) == 0)
	{
		return 0;
	}

        // Get first child and its data
        if ((childA = get_child( Octree_Child_Ref (0))) == NULL)
        {
                fprintf(stderr, "Octree_Data::condense(): could not get first child.");
                return 0;
        }

        if ((nodeA = childA->get_node_data()) == NULL)
	{
                fprintf(stderr, "Octree_Data::condense(): could not get node data for first child");
                return 0;
	}

	// Reset edge length and center in nodeA to reflect the
        // fact that it now takes up room for 8 nodes.
	// Double the edge length
	nodeA->edge_length *= 2.0;

	// For now, average all of the global centers to come
	// up with the global center for the condensed node.
	// nodeA will become the representative node.

	new_glob_ctr[0] = new_glob_ctr[1] = new_glob_ctr[2] = 0.0;

	for (i=UPPER_LEFT_BACK; i< NUMBER_OF_SPACE_PARTITIONS; i++)
	{
		if ((childX = get_child( Octree_Child_Ref (i))) != NULL)
		{
			if ((nodeX = childX->get_node_data()) != NULL)
			{
				nodeX->get_global_center(glob_ctr);
			        new_glob_ctr[0] += glob_ctr[0];
				new_glob_ctr[1] += glob_ctr[1];
				new_glob_ctr[2] += glob_ctr[2];
			}
		}
	}

	new_glob_ctr[0] /= 8.0;
	new_glob_ctr[1] /= 8.0;
	new_glob_ctr[2] /= 8.0;

	nodeA->set_global_center( new_glob_ctr);

	// Move the child's data node to the parent.
	add_data(nodeA);
	childA->release_nodespec();

	// Prune the octree by deleting the children.
	for(i=UPPER_LEFT_BACK; i<NUMBER_OF_SPACE_PARTITIONS; i++) 
	{
		if (child[i])
		{
			delete child[i];
			child[i] = NULL;
		}
	}

	return 1;
}

/// Compares the children to each other based on the compare function given.  
/**
// Returns 1 if they match, 0 if they don't.
*/
int Octree_Data:: compare_child_list( int (* compare_func)() )
{
	Octree_Data  *childA;
	Octree_Data  *childB;
	NodeSpec  *nodeA;
	NodeSpec  *nodeB;
	int i;
	int comp_val = 1;

	// Get first child and its data
	childA = get_child( Octree_Child_Ref (0) );

	// This should be redundant because we checked earlier.
	if (!childA)
	{
		return 0;
	}
	nodeA = childA->get_node_data();

	if (!nodeA)
	{
		return 0;
	}

	// Does the parent's color matter?  kfs
	// If so, compare the parent's color to the first child here.

	// Loop through the rest of the children to compare to the first.
	// If any do not match, set return val to 0 (false) break out of loop.

	for (i=UPPER_LEFT_BACK+1; i < NUMBER_OF_SPACE_PARTITIONS; i++)
	{
		childB = get_child( Octree_Child_Ref (i) );

		// This should be redudant, since we checked earlier.
		if (!childB)
		{
			comp_val = 0;
			break;
		}

		nodeB = childB->get_node_data();

		if (!nodeB)
		{
			fprintf(stderr, "Octree_Data::compare_child_list(): node of child %d NULL\n", i);
			comp_val = 0;
			break;
		}

		// Use the default compare function if none was
		// specified.
		if (compare_func == NULL)
		{
			if (compare_color(nodeA, nodeB) == 0)
			{
				comp_val = 0;
				break;
			}
		}
		//else
		//{
		// Hmmmmm--what will args be? kfs
		//	if (compare_func() == 0)
		//	{
		//		comp_val = 0;
		//		break;
		//	}
		//}
	}

	return comp_val;
}

/// Default NodeSpec comparison method.  
/**
// Compare by color as integer.
// If the nodes match, return 1, else return 0.
*/
int Octree_Data:: compare_color( NodeSpec *node1, NodeSpec *node2)
{
	int 	node1_red, node1_green, node1_blue, node1_alpha;
	int 	node2_red, node2_green, node2_blue, node2_alpha;

	// If they don't match, or you can't get the color, return 0.
	// Otherwise, return 1.

	if ( node1->get_color(&node1_red, &node1_green, &node1_blue, &node1_alpha) == 0)
	{
		fprintf(stderr, "compare_color(): could not get node 1 color\n");
		return 0;
	}

	if ( node2->get_color(&node2_red, &node2_green, &node2_blue, &node2_alpha) == 0)
	{
		fprintf(stderr, "compare_color(): could not get node 2 color\n");
		return 0;
	}

	if ( (node1_red == node2_red) && 
	     (node1_green == node2_green) && 
	     (node1_blue == node2_blue) &&
	     (node1_alpha == node2_alpha))
	{
		return 1;
	}
	else
	{
		return 0;
	}
}

/**
//   Runs through the octree creating NodeSpecs at parent levels with alpha values 
//   that will  be used as "weights" to determine how good a match is.
//   The farther up the tree the match is, the less "good" it will be.
//
// K. Sturdevant 7-6-99
*/
double Octree_Data::setup_weighted_match(float weight)
{
	double		red, green, blue, alpha;
	int		i;
	Octree_Data  	*kid;
	NodeSpec	*ns;
	NodeSpec	*weighted_node;
	double		max_alpha = 0.0, max_red = 0.0, max_green = 0.0, max_blue = 0.0;
	double		max_edge = 0.0;
	double          max_ctr[OCTREE_DIMS] = { 0.0 };

	// Check for a known voxel at this level.
	// If one exists, return its alpha value.

	// This could be a list--traverse the list? kfs
	if ((ns = get_node_data()) != (NodeSpec *)NULL)
	{
		// Get the alpha value and test for alpha >0
		// to indicate "known."
		ns->get_color(&red, &green, &blue, &alpha);
	        if ( alpha > 0.0 )	
		{
			return alpha;
		}
	}

	// Traverse the children, getting the alpha value for each one 
	// and saving the largest value.
        for (i=UPPER_LEFT_BACK; i < NUMBER_OF_SPACE_PARTITIONS; i++)
        {
		// Get the children, but avoid nulls.  is this enough?  kfs
                if ((kid = get_child( Octree_Child_Ref (i) )) == (Octree_Data *)NULL)
		{
			continue;
		}

		// Setup weighted match for this child
		// Do what with return value? kfs
		kid->setup_weighted_match(weight);

		// Traverse the nodespec list. 
		// Do anything with return value? kfs
                ns = kid->get_node_data();

		while (ns)
		{
			ns->get_color(&red, &green, &blue, &alpha);

			if (alpha > max_alpha)
			{
				max_alpha = alpha;
				max_red = red;
				max_green = green;
				max_blue = blue;
				max_edge = ns->edge_length;
				ns->get_global_center(max_ctr);
			}

			ns = ns->next;

			if (ns)
				fprintf(stderr, "Found second node!\n");
		}
	}

	// Create a nodespec for the octree at this level which 
	// contains the largest alpha value found for the children,
	// but multiplied by the weight.
	weighted_node = new NodeSpec();
	weighted_node->set_color(max_red, max_green, max_blue, weight*max_alpha);
	weighted_node->edge_length = max_edge;
	weighted_node->use_alpha(TRUE);
	weighted_node->set_global_center(max_ctr);

	// Adding the node here.
	//add_data( weighted_node, max_ctr ); kfs?
	add_data( weighted_node );

	return (weight*max_alpha);
}

/// Recursively discard any voxels outside specified volume
void Octree_Data::clip(Range *volume, ZMatrix m2w)
{
	// check voxels at this level
	NodeSpec *ns, *ns_prev, *ns_next;
	ns_prev = NULL;
	for (ns = nodespec; ns; ns=ns_next) {
		ns_next = ns->next;

		// transform voxel to world frame
		double center[OCTREE_DIMS], center_trans[OCTREE_DIMS];
		ns->get_global_center(center);
		MultPoints(center, m2w, center_trans);

		// test against clip limits
		if (!volume->in_range(center_trans)) {
			// remove from chain
			if (ns_prev)		// not first one
				ns_prev->next = ns_next;
			else			// first one
				nodespec = ns_next;
			ns->next = NULL;	// don't delete rest of chain
			delete ns;
		} else {
			ns_prev = ns;		// keep it
		}
	}

	// recurse and conquer
	for(int i=UPPER_LEFT_BACK; i<NUMBER_OF_SPACE_PARTITIONS; i++) {
		if(child[i])
			child[i]->clip(volume, m2w);
	}
}

void Octree_Data:: ascii_dump(FILE *fs, long lvl)
{
	int i,j,r,g,b,a;
	NodeSpec *ns;

	for(i=0; i<lvl*4; i++) fprintf(fs," ");
	fprintf(fs,"|%4d>",lvl);
	ns = nodespec;
	while(ns) {
		ns->get_color(&r, &g, &b, &a);
		fprintf(fs,"(R=%3d  G=%3d  B=%3d  A=%3d LEN=%f) ", r,g,b,a,ns->edge_length);
		ns = ns->next;
	}
	fprintf(fs,"\n");
	for(i=UPPER_LEFT_BACK; i<NUMBER_OF_SPACE_PARTITIONS; i++) {
		if(child[i]) {
			child[i]->ascii_dump(fs, lvl+1);
		} else {
			for(j=0; j<(lvl+1)*4; j++) fprintf(fs," ");
			fprintf(fs,"|%4d>",(lvl+1));
			fprintf(fs,"NULL Child\n");
		}
	}
}	

/// Load octree node from file
int Octree_Data::parse_in(Dataport *fp)
{
	char    token[4096];
	NodeSpec *ns;
	int	chld;
	Octree_Data	*octree;

	do {
		if(!get_next_token(fp, token)) {
			fprintf(stderr," Whoops - Unexpected EOF in octree data entry\n");
			return(FALSE);
		}
		if(!strcmp(token, "OCTREE_DATA_V1")) {
			// do nothing
		} else if(!strcmp(token, "NODE")) {
			ns = new NodeSpec();
			ns->parse_in(fp);
			add_data(ns);
		} else if(!strcmp(token, "CHILD")) {
			if(!get_next_token(fp, token)) {
				fprintf(stderr," Whoops - Unexpected EOF in octree entry\n");
				return(FALSE);
			}
			chld = atoi(token);
			octree = new Octree_Data(level-1);
			octree->parse_in(fp);
			child[chld] = octree;

		} else if(strcmp(token, "END_OCTREE_DATA")) {
			fprintf(stderr," Whoops - Unexpected token encountered in parsing octree data entry\n");
			fprintf(stderr,"   Token=>%s<  Expecting one of NODE END_OCTREE_DATA\n", token);
			 return(FALSE);
		}
	} while(strcmp(token, "END_OCTREE_DATA"));
	return(TRUE);
}

/// Save octree node to file
int Octree_Data::parse_out(Dataport *fp, int expand)
{
	char    token[4096];
	int	i;
	NodeSpec *ns;

	put_token(fp, "\nOCTREE_DATA_V1");
	ns = get_node_data();
	while(ns) {
		put_token(fp, "\nNODE");
		ns->parse_out(fp, expand);
		ns = ns->next;
	}

	for(i=UPPER_LEFT_BACK; i<NUMBER_OF_SPACE_PARTITIONS; i++) {
		if(child[i]) {
			sprintf(token,"\nCHILD %d",i);
			put_token(fp, token);
			child[i]->parse_out(fp, expand);
		}
	}

	put_token(fp, "\nEND_OCTREE_DATA");

	return(TRUE);
}

/// Save octree data in binary format (note, no expand option)
void Octree_Data::write_binary(Dataport *fp)
{
	// write this cell's voxels
	for (NodeSpec *ns = get_node_data(); ns; ns = ns->next)
		ns->write_binary(fp);
	
	// recursively write children
	for(int i=UPPER_LEFT_BACK; i<NUMBER_OF_SPACE_PARTITIONS; i++) {
		if(child[i])
			child[i]->write_binary(fp);
	}
}


/****************************************************
Octree methods
****************************************************/

/// Create an octree data and initialize the octree attribute
Octree_Data * Octree::init_data()
{
	if (!octree)
		octree = new Octree_Data(levels-1);
	return octree;
}

#ifdef FLEXCOLOR
ColorSpec *Octree::get_color(double  local_center[OCTREE_DIMS])
{
	double	temp[OCTREE_DIMS];
	ZMatrix	xform;

	// transform sample center into model space
	GetObjectToModelTransform(xform);
	MultPoints(local_center, xform, temp);
	// set sample center to model space in sample
	if(octree) {
		return(octree->get_color(temp));
	} else {
		return(NULL);
	}
}
#endif

int Octree::get_rgb(double local_center[OCTREE_DIMS], double *r, double *g, double *b, double *a)
{
	double	temp[OCTREE_DIMS];
	ZMatrix	xform;

	// transform sample center into model space
	GetObjectToModelTransform(xform);
	MultPoints(local_center, xform, temp);
	// set sample center to model space in sample
	if(octree) {
		return(octree->get_rgb(temp,r,g,b,a));
	} else {
		return(FALSE);
	}
}

int Octree::get_rgb(double local_center[OCTREE_DIMS], int *r, int *g, int *b, int *a)
{
	double	temp[OCTREE_DIMS];
	ZMatrix	xform;

	// transform sample center into model space
	GetObjectToModelTransform(xform);
	MultPoints(local_center, xform, temp);
	// set sample center to model space in sample
	if(octree) {
		return(octree->get_rgb(temp,r,g,b,a));
	} else {
		return(FALSE);
	}
}

/// Transform and add sample to octree
void Octree::add_sample(NodeSpec *ns)
{
	double	local_center[OCTREE_DIMS], temp[OCTREE_DIMS];
	ZMatrix	xform;

	// transform sample center into model space
	GetObjectToModelTransform(xform);
	ns->get_global_center(temp);
	MultPoints(temp, xform, local_center);
	// set sample center to model space in sample
	ns->set_global_center(local_center);
	ns->set_local_center(local_center);
	// add to octree
	if(!octree) {
		octree = new Octree_Data(levels-1);
	}
	octree->add_sample(ns, local_center);
}

/// Transform and add voxel to octree
void Octree::add_voxel(NodeSpec *ns)
{
	double	local_center[OCTREE_DIMS], local_edge, temp[OCTREE_DIMS];

	// scale edge length
	local_edge = ns->edge_length ; // * xscale;

	// Get transform to map object coords to internal octree coords.
	// Hopefully, freeze_xform() has been called so this matrix
	// is cached.
	ZMatrix xform;
	GetObjectToModelTransform(xform);
	//MatDump(stderr, "xform", xform);

	// transform nodes center from world(object) coords to internal octree coords
	ns->get_global_center(temp);
	MultPoints(temp, xform, local_center);

	ns->set_global_center(local_center);
	ns->set_local_center(local_center);
	ns->edge_length = local_edge;

	// transform the normal to model space
	if (ns->has_normal()) {
		double xnorm[OCTREE_DIMS];
		GetNormalToModelTransform(xform);
		ns->get_normal(temp);
		MultPoints(temp, xform, xnorm);
		normalize_vector(xnorm);	// needed?
		ns->set_normal(xnorm);
	}

	// add to octree
	if(!octree) {
		octree = new Octree_Data(levels-1);
	}
	octree->add_voxel(ns, local_center, local_edge);
}

void Octree::accumulate_voxel(NodeSpec *ns, double weight)
{
	double	local_center[OCTREE_DIMS], local_edge, temp[OCTREE_DIMS];
	ZMatrix	xform;

	// scale edge length
	local_edge = ns->edge_length ; // * xscale;

	// get transform to map object coords to internal octree coords
	GetObjectToModelTransform(xform);

	// transform nodes center from world(object) coords to internal octree coords
	ns->get_global_center(temp);

	MultPoints(temp, xform, local_center);

	ns->set_global_center(local_center);
	ns->set_local_center(local_center);
	ns->edge_length = local_edge;
	// add to octree
	if(!octree) {
		octree = new Octree_Data(levels-1);
	}
	octree->accumulate_voxel(ns, local_center, local_edge, weight);
}

void Octree::OctreeLimits(double mins[OCTREE_DIMS], double maxs[OCTREE_DIMS])
{
	int	i, j;
	double	local[OCTREE_DIMS], corner[OCTREE_DIMS];
	ZMatrix xform;

	corner[0] = X_Offset[0];
	corner[1] = Y_Offset[0];
	corner[2] = Z_Offset[0];
	GetModelToObjectTransform(xform);
	MultPoints(corner, xform, mins);
	for(j=0; j<OCTREE_DIMS; j++) {
		maxs[j] = mins[j];
	}
	for(i=1; i<NUMBER_OF_SPACE_PARTITIONS; i++) {
		corner[0] = X_Offset[i];
		corner[1] = Y_Offset[i];
		corner[2] = Z_Offset[i];
		MultPoints(corner, xform, local);
		for(j=0; j<OCTREE_DIMS; j++) {
			if(local[j] < mins[j]) mins[j] = local[j];
			if(local[j] > maxs[j]) maxs[j] = local[j];
		}
	}
}

/// Condense octree levels
/**
//  Entry method for the condense operation.  The major work is
//  in the Octree_Data class.
*/
int Octree::condense(int (*comp_func)())
{
	// If octree data is not null, try to condense it
	if (octree != NULL)
	{
		// If a comparison function is defined, pass it along.
		// Call the condense method in the Octree_Data class
		if (comp_func == NULL)
			return ( octree->condense() );
		else
			return ( octree->condense(comp_func) );
	}
	else
		return 0;
}

/// Pre-processor method for weighted match.
// K. Sturdevant 7-7-99
double Octree::setup_weighted_match(float weight)
{
	if (octree != NULL)
		return( octree->setup_weighted_match(weight));
	else
		return 0;
}

/// Exhaustively search octree for point nearest given coordinates
/**
// Given a point in object space, convert it 
// to model space and then call search_model_space() to find the point 
// in the octree that is closest to this point.
// The method return value is the distance, and the matching point
// K. Sturdevant 8-19-99
*/
double
Octree::search_object_space(double input_point[], double closest_point[])
{
	ZMatrix xform, xform_inv;
	double	model_point[OCTREE_DIMS], closest_model_point[OCTREE_DIMS];
	NodeSpec *matched_node;

	// get transform to map object coords to internal octree coords
	GetObjectToModelTransform(xform);
	MatInvert(xform, xform_inv);

	// convert point to model space
	MultPoints(input_point, xform, model_point);

	// call Octree_Data's search_model_space() w/ converted value
	octree->search_model_space(model_point, &matched_node);
        matched_node->get_global_center(closest_model_point);

	//Transform closest_point from model space back to object space
	MultPoints(closest_model_point, xform_inv, closest_point);

	// determine distance in object space
	return distance(closest_point, input_point);
}

/// Smart search for octree point nearest coordinates
/**
// Given a point in object space from the smaller octree model, convert it 
// to model space and then call fast_find_closest() to find the point 
// in the larger dataset that is closest to this point.
// The method return value is the distance, and the matching point's x, y, z 
// are returned as arguments.
// Returns -1 if problems with the data.
// K. Sturdevant 11-3-99
*/
double
Octree::fast_find_closest(double input_point[], double closest_point[])
{
	ZMatrix xform, xform_inv;
	double	loc_model_point[OCTREE_DIMS], glob_model_point[OCTREE_DIMS];
	double	closest_model_point[OCTREE_DIMS];
	long	top_level;

	if (!octree) {
		fprintf(stderr, "Octree::fast_find_closest() could not find octree data\n");
		return -1;
	}

	// get transform to map object coords to internal octree coords
	GetObjectToModelTransform(xform);
	MatInvert(xform, xform_inv);

	// convert point to model space
	// Set global model point equal to local model point at this point.  Global
	// will remain constant during iterations, while the local will be changed to
	// match the current space.  Global will be used to calculate all distances.

	MultPoints(input_point, xform, loc_model_point);
	MultPoints(input_point, xform, glob_model_point);

	top_level = octree->get_level();

	// call Octree_Data's fast_find_closest() w/ converted value
	octree->fast_find_closest(top_level, (double)FLT_MAX, glob_model_point, 
			loc_model_point, closest_model_point);

	//Transform closest_point from model space back to object space
	MultPoints(closest_model_point, xform_inv, closest_point);

	// Compute distance in object space
	return distance(closest_point, input_point);
}

#if 0
// Given a point x, y, z and a radius r, return a list of points which are within the radius
// of the given point.  This method converts the points to model space before sending them
// to the Octree_data method for processing.  It also converts them back to object space
// before returning them to the caller.
int Octree::find_points_within_radius(double input_point[OCTREE_DIMS], double radius, NodeSpec **node_list)
{
	ZMatrix xform, xform_inv;
	double loc_model_point[OCTREE_DIMS];
	double glob_model_point[OCTREE_DIMS];
	double glob_object_point[OCTREE_DIMS];
	double rad_model_point[OCTREE_DIMS];
	double model_radius;
	long	top_level;
	double rad_point[OCTREE_DIMS];
	int i;
	int od_points_added;
	NodeSpec * ptr, * save, *prev, *del;
	double obj_dist, rad_sq;

        if (!octree)
        {
                fprintf(stderr, "Octree::find_points_within_radius() could not find octree data\n");
                return -1;
        }

	// get transform to map object coords to internal octree coords
	GetObjectToModelTransform(xform);
	MatInvert(xform, xform_inv);

	// convert point to model space
	// Set global model point equal to local model point at this point.  Global
	// will remain constant during iterations, while the local will be changed to
	// match the current space.  Global will be used to calculate all distances.

	MultPoints(input_point, xform, loc_model_point);
	MultPoints(input_point, xform, glob_model_point);

	// Convert the radius to model space
	for (i=0; i <OCTREE_DIMS; i++)
		rad_point[i] = radius;

	MultPoints(rad_point, xform, rad_model_point);

	// Set model_radius to the largest translated value.
	model_radius = fabs(rad_model_point[0]);

	for (i=0; i <OCTREE_DIMS; i++)
	{
		if (fabsf(rad_model_point[i]) > model_radius)
			model_radius = fabsf(rad_model_point[i]);
	}

	// Get the level
	top_level = octree->get_level();

	// call Octree_Data's find_points_within_radius() w/ converted value
	// The method will return the # of points it found in addition to a linked list of NodeSpecs.

	od_points_added = octree->find_points_within_radius(top_level, glob_model_point, loc_model_point, 
		model_radius, node_list);

	// Due to radius ellipsoid-type changes in the conversion from object to model space,
	// run through the points one more time to verify they are indeed within the radius
	// before returning the list.  Also, convert the global values back to OBJECT space 
	// coordinates while running through the list.
	// Subtract from the counter any points which are removed.

	save = ptr = *node_list;
	prev = NULL;

	rad_sq = radius*radius;

	while (ptr)
	{
                ptr->get_global_center(glob_model_point);

		// translate, calculate distance, and compare to radius	
		MultPoints(glob_model_point, xform_inv, glob_object_point);

		obj_dist  = sqrt((glob_object_point[0] - input_point[0]) *
				(glob_object_point[0] - input_point[0]) + 
				(glob_object_point[1] - input_point[1]) *
				(glob_object_point[1] - input_point[1]) +
				(glob_object_point[2] - input_point[2]) *
				(glob_object_point[2] - input_point[2]));

		// If the distance is more than the radius, remove the point from the list
		if (obj_dist > rad_sq)
		{
			if (prev == NULL)
			{
				// Skip the first nodespec
				del = *node_list;

				*node_list = (*node_list)->next;

				// Delete the node
				del->next = NULL;
				delete del;  

				// Reset the list pointer
				save = ptr = *node_list;
				prev = NULL;
			}
			else
			{
				// Skip the current nodespec
				del = ptr;
				ptr = ptr->next;
				prev->next = ptr;

				del->next = NULL;
				delete del;
			}

			od_points_added--;
		}
		// Otherwise, set the global center value to OBJECT space
		else
		{
			ptr->set_global_center(glob_object_point);
			prev = ptr;
			ptr = ptr->next;
		}
	}

	// Reset node list before returning
	*node_list = save;

	return od_points_added;
}
#endif

/// Get node count accumulated during parse_in().
// Count is invalid after modifying octree.
int Octree::get_node_count()
{
	return node_id;
}

/// Load octree from file
int Octree::parse_in(Dataport *fp)
{
	char    token[4096];

	node_id = 0;		// initialize

	// initial OCTREE_V1 token assumed to already be read in
	// (e.g. to determine that this *is* an octree)

	if(!Obj::parse_in(fp)) {
		return(FALSE);
	}

	do {
		if(!get_next_token(fp, token)) {
			fprintf(stderr," Whoops - Unexpected EOF in octree entry\n");
			return(FALSE);
		}
		if(!strcmp(token, "OCTREE_V1")) {
			// do nothing
		} else if(!strcmp(token, "LEVELS")) {
			if(!get_next_token(fp, token)) {
				fprintf(stderr," Whoops - Unexpected EOF in octree entry\n");
				return(FALSE);
			}
			levels = atoi(token);
		} else if(!strcmp(token, "OCTREE_STUFF")) {
			// text format voxels
			if(!get_next_token(fp, token)) {
				fprintf(stderr," Whoops - Unexpected EOF in octree entry\n");
				return(FALSE);
			}
			while(strcmp(token, "END_OCTREE_STUFF")) {
				if(!octree) octree = new Octree_Data(levels-1);
				octree->parse_in(fp);
				if(!get_next_token(fp, token)) {
					fprintf(stderr," Whoops - Unexpected EOF in octree entry\n");
					return(FALSE);
				}
			}
		} else if(!strcmp(token, "OCTREE_BIN_STUFF")) {
			// binary format voxels
			if (!octree) octree = new Octree_Data(levels-1);
			BRHeader hdr;
			while (fp->read(&hdr, sizeof(hdr)) > 0 && hdr.nbytes) {
				NodeSpec *ns = new NodeSpec;
				ns->read_binary(&hdr, fp);

				// add node to tree
				octree->add_voxel(ns, NULL, -1.0);
			}
		} else if(strcmp(token, "END_OCTREE")) {
			fprintf(stderr," Whoops - Unexpected token encountered in parsing octree data entry\n");
			fprintf(stderr,"   Token=>%s<  Expecting one of NODE END_OCTREE\n", token);
			 return(FALSE);
		}
	} while(strcmp(token, "END_OCTREE"));
	return(TRUE);
}

/// Save octree to file
int Octree::parse_out(Dataport *fp, int expand)
{
	char    token[4096];

	put_token(fp, "\nOCTREE_V1");
	node_id = 0;		// initialize

	Obj::parse_out(fp, expand);

	sprintf(token, "\nLEVELS %d", levels);
	put_token(fp, token);

	if(octree) {
		if (getenv("GRAPE_WRITE_TEXT")) {	// write text format
			put_token(fp, "\nOCTREE_STUFF");
			octree->parse_out(fp, expand);
			put_token(fp, "\nEND_OCTREE_STUFF");
		} else {				// write binary format
			put_token(fp, "\nOCTREE_BIN_STUFF");
			octree->write_binary(fp);
			// add end record
			BRHeader hdr;
			hdr.nbytes = hdr.flags = 0;
			fp->write(&hdr, sizeof(hdr));
		}
	}

	put_token(fp, "\nEND_OCTREE");

	return(TRUE);
}
