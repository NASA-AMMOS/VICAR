// summitt_func.C 1.37 03/05/01 07:58:36
/** \file
** SUMMITT application services.
** Function definitions particular to the SUMMITT approach of 
** evaluating octrees, including the infamous matcher.
*/

#include <float.h>
#include <stdlib.h> 
#include "time.h" 
#include "stddef.h"
#include "strings.h"
#include "grape/aej.h"
#include "grape/aejError.h"
#include "grape/aejMatrix.h"
#include "grape/aejTransform.h"
#include "summitt_func.h"

#ifdef MATRIXDEBUG
#define DBGMATDUMP(fp,msg,mat)	MatDump(fp,msg,mat)
#else
#define DBGMATDUMP(fp,msg,mat)
#endif

// use squared distance for ICP cutoff distance histogram?
// faster, may or may not do better
#define HIST_DSQUARED

// using nodespec ID field for matcher "valid match" bit flags
#define valid	ns->id

// octree voxels for ICP matcher source object list
struct Source_voxel {
	double ctr[OCTREE_DIMS];	// position in object coordinates
	NodeSpec *ns;			// pointer back to octree structure
		// (to support rejecting neighbors of discarded matches)
};

int verbose;	///< set this for trace output to stderr

// secret flags for experimental code
// 1 = aggresive histogram cutoff
// 2 = no histogram cutoff
// 4 = vector magnitude cutoff
// 8 = vector magnitude weighting
// 16 = accumulate WDO 2 levels
// 32 = match vector dot normal cutoff (sliding)
// 64 = match vector dot normal weighting (sliding)
// 128 = project match to surface along dest normal
// 256 = dump intermediate octrees
// 512 = 
int icp_flags;

// ** TEMP
double vsum_factor = 0.75;
double sfc_cos_limit = 0.1;
double min_dist2;

enum { MAX_REPEAT = 10 };	// max match repeats for weighting

/// Count voxels in an octree
/**
// Count the number of voxel nodes in an octree.
// "Known" voxels have positive alpha, "empty" voxels have alpha = -1
*/
void
count_voxels( Octree_Data * oct, int *known, int *empty, int *non_leaf = NULL)
{
        double r,g,b,a;
        NodeSpec *ns;
        int ref;

        if (!oct) return;

        if ( !oct->any_children() ) {
                if((ns = oct->get_node_data()) != NULL) {
		    while(ns){
		        ns->use_alpha(TRUE);	

                        if (ns->get_color(&r, &g, &b, &a) == FALSE){
			    fprintf(stderr, "count_voxels(): failed to get node color.\n");
			    return;
		        }

                        if(a > 0.0)
                              (*known)++;
                        else if(a == -1.0)   // Empties are alpha = -1
                             (*empty)++;

		        ns = ns->next;
		    }
                } else {
                    fprintf(stderr, "count_voxels(): Found a leaf node w/ no node data\n");
                }
        } else {
                //Count the number of nodes that do not have data, e.g.
                //the non-leaf nodes

                if ((ns = oct->get_node_data()) == NULL)
                        (*non_leaf)++;

                for (ref=UPPER_LEFT_BACK; ref<NUMBER_OF_SPACE_PARTITIONS; ref++) {
                        count_voxels(oct->get_child((Octree_Child_Ref)ref),
                                known, empty, non_leaf);
                }
        }
}

/// Count the known voxels in an octree (alpha > 0)
int count_known_voxels( Octree_Data * oct)
{
    int known = 0, empty = 0, non_leaf = 0;

    count_voxels( oct, &known, &empty, &non_leaf);
    return known;
}

/// Count the empty voxels in an octree (alpha = -1)
int count_empty_voxels(Octree_Data *oct)
{
    int known = 0, empty = 0, non_leaf = 0;

    count_voxels( oct, &known, &empty, &non_leaf);
    return empty;
}


/// The work horse of summitt_match_octrees()
void traverse_data (Octree_Data * od1, Octree_Data *od2, 
	ZMatrix transmat, float * tally)
{
	double  r1, g1, b1, alpha1;
	double  r2, g2, b2, alpha2;
	int  ref;
	NodeSpec *ns1;
	double  center[OCTREE_DIMS];
	double  center_trans[OCTREE_DIMS];

    if (!od1) return;

    // Currently checking the first nodespec only.  May have to change
    // it to loop over all nodespecs. kfs
    if ((ns1 = od1->get_node_data()) != NULL) {
	    ns1->use_alpha(TRUE);	
	
	    // Get the alpha value of octree1's node 
	    if (ns1->get_color(&r1, &g1, &b1, &alpha1) == FALSE)
	        fprintf(stderr, "traverse_data failed to get color for node 1\n");

	    // Check that alpha1 > 0, which means the voxel is known.  
            // An alpha value of -1 means it is empty on 3-11-99.
            // No guarantees of its value after that. :-)
            //
	    if (alpha1 > 0.0) {

		// Get the x,y,z of octree 1 and compare to nodes in octree 2
		ns1->get_global_center(center);

		// apply transformation matrix to point 
		MultPoints(center, transmat, center_trans);

		// Center is _new_ center, after transformation 
		// use a weighted match to find a close match instead of exact.
		*tally += od2->match_rgb(center_trans, &r2, &g2, &b2, &alpha2);
        }
    }

    for (ref=UPPER_LEFT_BACK; ref<NUMBER_OF_SPACE_PARTITIONS; ref++)
	    traverse_data(od1->get_child((Octree_Child_Ref)ref), 
	    	od2, transmat, tally);

}  /*end traverse_data */

/// Clip octree by object or model coordinate range
/**
// Fills in output octree with voxels from input tree that are within
// the specified range. MODEL_SPACE range means use voxel coordinates
// as-is; OBJECT_SPACE range means apply given transform for clipping.
// In either case, in-range voxels are added directly to the output
// tree (same model coords).
//
// ** There is now an Octree::clip() method, recommended instead.
*/
int
summitt_clip_octree(Octree_Data *od_in, Summitt_range *range, 
			ZMatrix modelobj1, Octree_Data *od_out)
{
	double r,g,b,a;
	double   center[OCTREE_DIMS];
	double   center_trans[OCTREE_DIMS];
	int      clip_counter = 0;

    if (!od_in || !od_out) return -1;

    // If no children, we have a leaf node.
    // Distinguish between empties and ones with data.

    if ( !(od_in->any_children()) ) {
            for (NodeSpec *ns = od_in->get_node_data(); ns; ns = ns->next) {
            
		// SUMMITT relies on alpha being set to determine when a voxel is "known."
	        ns->use_alpha(TRUE);	
                if (ns->get_color(&r, &g, &b, &a) == FALSE) {
		    fprintf(stderr, "summitt_clip_octree(): failed to get node color.\n");
		    return -1;
	        }

                if (a > 0.0) {
		    ns->get_global_center(center);

		   // The model to object transform must be used for comparison.
		   // However, if space is OBJECT space, and using octree data's
		   // add_voxel, the regular, non-transformed voxel should be added.
		   // If in MODEL space, add the transformed voxel.
		   // If range is set to compare model space, check here to see if the
		   // point is within range.  If it is, put it in the new octree.
		   // If it isn't, skip to check the next nodespec.

		   if (range->space == SUMMITT_MODEL_SPACE) {
			if (range->in_range(center)) {
				NodeSpec *ns_add = ns->duplicate();
				ns_add->use_alpha(TRUE);	// ** is this needed?
                		od_out->add_voxel(ns_add, center, ns_add->edge_length);
				clip_counter++;
			}
		   } else { 	// SUMMITT_OBJECT_SPACE
                   	MultPoints(center, modelobj1, center_trans);
			if (range->in_range(center_trans)) {
				NodeSpec *ns_add = ns->duplicate();
				ns_add->use_alpha(TRUE);
                		od_out->add_voxel(ns_add, center, ns_add->edge_length);
				clip_counter++;
			}
		   }
                }
	    }
    }

    for (int ref=UPPER_LEFT_BACK; ref<NUMBER_OF_SPACE_PARTITIONS; ref++) {
	int ret_clips = summitt_clip_octree(od_in->get_child((Octree_Child_Ref)ref), 
						range, modelobj1, od_out);
	if (ret_clips > 0)
            clip_counter += ret_clips;
    }

    return clip_counter;

}  // end summitt_clip_octree()

/// extract and transform octree points for icp matcher
/**
** For each octree point, transform with the specified matrix
** and add it to the output list
*/
void summitt_transform_od(Octree_Data *od, ZMatrix transmat, 
	int *odcounter, Source_voxel *od_list)
{
    // If no children, we have a leaf node.
    // Distinguish between empties and ones with data.
    if (!od->any_children()) {
            for (NodeSpec *ns = od->get_node_data(); ns; ns = ns->next) {
		double r,g,b,a;
	        ns->use_alpha(TRUE);	
                if (ns->get_color(&r, &g, &b, &a) == FALSE) {
		    fprintf(stderr, "summitt_transform_od: no node color.\n");
		    return;
	        }

                if (a > 0.0) {
		  double center[OCTREE_DIMS];
		  ns->get_global_center(center);

		  // save object coordinates and link back to the node
		  Source_voxel *v = &od_list[*odcounter];
		  MultPoints(center, transmat, v->ctr);
		  v->ns = ns;
		  ns->id = 0;	// flag as not yet valid
		  (*odcounter)++;
               }
	    }
    }

    for (int ref=UPPER_LEFT_BACK; ref<NUMBER_OF_SPACE_PARTITIONS; ref++) {
	Octree_Data *kid = od->get_child((Octree_Child_Ref)ref);
	if (kid)
        	summitt_transform_od(kid, transmat, odcounter, od_list);
    }
}  // end summitt_transform_od()


// Error tolerance determination for icp_match_octrees()
static double change_ratio(double prev, double newval)
{
	// Avoid divide by zero on tolerance test.  
	// Determine appropriate denominator.
	double diff = prev - newval;
	if (prev == 0) {
		if (newval == 0)
			return 0.0;
		return fabs(diff / newval);
	}
	return fabs(diff / prev);
}

// Determine a point match cutoff distance for the ICP matcher,
// by cutting off the distance histogram tail.
// Only check every (step) slot in dist array.
// Limit cutoff to discard at most max_disc % of the points
// [old: Limit the tail cutoff bin to at least bin (min_bin) of 100.]
// Input array and output value are actually distance squared.
// ** maybe speed this up using squared distances in histogram
static double cutoff_distance(double *dist, int ndist, int step, int max_disc)
{
	enum { NUM_BINS = 100, LAST_BIN = 99 };
	// find match distance extremes to scale histogram bin width
	int i;
	int nvalid = 0;				// count valid matches
	double dmin = FLT_MAX;
	double dmax = 0.0;
	for (i=0; i<ndist; i+=step) {
		double d = dist[i];
		if (d == FLT_MAX)		// rejected match
			continue;
		nvalid++;
		if (d < dmin)
			dmin = d;
		if (d > dmax)
			dmax = d;
	}
	dmin = sqrt(dmin);
	dmax = sqrt(dmax);
	if (verbose > 1)
		fprintf(stderr, "cutoff dist range %f to %f\n", dmin, dmax);

	// insufficient variation?
	if (dmax - dmin < 0.0001)
		return dmax * dmax;

	// build histogram covering distance range
	long hist[NUM_BINS];
	memset(hist, 0, sizeof(hist));
	double scale = (NUM_BINS - 0.001) / (dmax - dmin);
	for (i=0; i<ndist; i+=step) {
		if (dist[i] < FLT_MAX)
			hist[int((sqrt(dist[i]) - dmin) * scale)]++;
	}

// ** test for min cutoff
min_dist2 = dmin + 1.0/scale;
min_dist2 *= min_dist2;

	if (verbose > 2) {
		// dump histogram
		fprintf(stderr, "HIST: ");
		for (i=0; i<NUM_BINS; i++) 
			fprintf(stderr, "%d ", hist[i]);
		fprintf(stderr, "\n");
	}

	// find (last) max bin count, and percent cutoff limit 
	long maxcnt = 0;
	int  maxbin = 0;
	long limit = (100 - max_disc) * nvalid / 100; 
	int  min_bin = NUM_BINS;
	for (i=0; i<NUM_BINS; i++) {
		if (hist[i] >= maxcnt) {
			maxcnt = hist[i];	// new max bin
			maxbin = i;
		}
		if (min_bin == NUM_BINS) {	// limit not reached yet?
			limit -= hist[i];	// accumulate
			if (limit < 0)		// reached threshold
				min_bin = i;
		}
	}
	if (verbose > 1)
		fprintf(stderr, "hist max=%d at bin %d of %d, lim at %d", 
			maxcnt, maxbin, NUM_BINS, min_bin);

	// find next bin with less than 1/10 height of max bin
	maxcnt /= 10;
	for (i=maxbin+1; i<NUM_BINS; i++) {
		if (hist[i] <= maxcnt)
			break;
	}
	if (verbose > 1)
		fprintf(stderr, " tail at %d ", i);
	if (i < min_bin)	// if tail too early, apply cutoff limit
		i = min_bin;
	dmin += i/scale;
	return dmin*dmin;
}

// Improve ICP matching by finding match point on dest surface rather
// than closest dest sample point.
// Given a source point and the closest dest point,
// project source point into the plane defined by the dest point and
// its surface normal (all done in dest model space).
static void match_surface(double *src, NodeSpec *dest, double *match)
{
	// Destination surface plane at dest is ax+by+cz+d = 0
	// where (a,b,c) is the unit vector normal.
	// d = - dest . normal
	double dpt[3], dnorm[3];
	dest->get_global_center(dpt);
	dest->get_normal(dnorm);

	// Compute signed distance from src to plane
	// dist = src . normal + d
	//      = (src . normal) - (dest . normal)
	//      = (src - dest) . normal
	double dist = (src[0] - dpt[0]) * dnorm[0] + 
		      (src[1] - dpt[1]) * dnorm[1] +
		      (src[2] - dpt[2]) * dnorm[2];

	// Projection is dist units along normal from src
	match[0] = src[0] - dist*dnorm[0];
	match[1] = src[1] - dist*dnorm[1];
	match[2] = src[2] - dist*dnorm[2];
}

/// Match octrees using iterative closest point algorithm
/**
**   The iterative closest point match to (re)align a source
**   surface model to a destination model (possibly merged from
**   an octree forest).
**
**   Make lists of source point centers in object space.
**   Find the closest destination point for each source
**   point and store in a list.  Perform least squares (Andrew
**   Johnson's methods) and get a metric.  Throw out the outlying 
**   points and redo least squares until a satisfactory
**   metric is obtained.
**
**   The least squares algorithm doesn't handle a scaling component,
**   so any scaling factor in the initial guess for the source's
**   object-to-world transform is separated out, and its inverse
**   applied to the destination world space for matching.
**
**   max_iterations, epsilon, and tolerance are values used to terminate the
**   least squares algorithm.  Epsilon compares the difference between the
**   current guesses and the previous guesses.  Tolerance is the percentage
**   of change.
**
**   bvfactor scales the destination octree bounding volume - source
**   points mapping outside this scaled volume are rejected.
**   A bvfactor less than one shrinks the volume; 
**   a bvfactor greater than one expands the volume.
**
**   err_radius sets an error bound (in source model space) around 
**   "edge" points - source points whose match distance is too large.
**   Points within this radius are also ignored in least squares update.
**
**   Source octree nodespec "id" fields are overwritten here as
**   temporary storage for a "valid" indicator (bit 0 = previously valid,
**   bit 1 = valid this iteration).
**
**   The optional cleanup argument determines whether the (large amount
**   of) data allocated by this function is freed before returning.
**   If you're only calling the function once and don't need to do
**   much afterward, it's a bit faster to not bother cleaning up.
**
**   On successful return, the source ObjNode is updated with the 
**   new object-world transform.
*/

float summitt_icp_match_octrees(ObjNode *objn1, ObjNode *objn2, 
	int max_iterations, double tepsilon, double repsilon, 
	double tolerance, double bvfactor, double err_radius, Boolean cleanup)
{
Octree *oct1, *oct2;		// 1=source, 2=dest
Octree_Data *od1, *od2;
int pt_increment;		// The sample increment for the search
Boolean first_time = TRUE;      // Boolean for the first time tolerance is reached.
int i, u;
aejTransform resultTR;
aejVector trans;
double x_toltest, y_toltest, z_toltest, xrot_toltest, yrot_toltest, zrot_toltest;

    // get source octree data
    if (!(oct1 = (Octree *)objn1->get_object())) {
        fprintf (stderr, "summitt_icp_match_octrees: source octree is null\n");
        return -1.0;
    }
    if (!(od1 = oct1->get_data())) {
        fprintf (stderr, "summitt_icp_match_octrees: no source octree data\n");
        return -1.0;
    }

    // get destination octree data
    if (!(oct2 = (Octree *)objn2->get_object())) {
        fprintf (stderr, "summitt_icp_match_octrees: dest octree is null\n");
        return -1.0;
    }
    if (!(od2 = oct2->get_data())) {
        fprintf (stderr, "summitt_icp_match_octrees: no dest octree data\n");
        return -1.0;
    }

    // get destination bounding volume in model space, and adjust its size
    Summitt_range bvol;
    summitt_octree_extents(oct2, &bvol);
    if (verbose) {
	fprintf(stderr, "ICP flags = %d\n", icp_flags);
	bvol.dump(stderr, "Matcher destination bvol");
    }

    double d = (bvol.xmax - bvol.xmin) * (bvfactor - 1.0)/2.0;
    bvol.xmin -= d;
    bvol.xmax += d;
    d = (bvol.ymax - bvol.ymin) * (bvfactor - 1.0)/2.0;
    bvol.ymin -= d;
    bvol.ymax += d;
    d = (bvol.zmax - bvol.zmin) * (bvfactor - 1.0)/2.0;
    bvol.zmin -= d;
    bvol.zmax += d;

    // count source voxels, make sure there *are* some
    int od1_size = count_known_voxels(od1);
    if (od1_size <= 0) {
	fprintf(stderr, "The source octree has no known voxels, "
		"od1_size = %d\n", od1_size);
	return -1;
    }

    // Get source's model-to-object transformation matrix
    ZMatrix modelobj1;
    oct1->GetModelToObjectTransform( modelobj1 );
    DBGMATDUMP(stderr, "modelobj1", modelobj1);

    // Build list of source points in object space
    Source_voxel *od1_list = new Source_voxel[od1_size];
    od1_size = 0;
    summitt_transform_od(od1, modelobj1, &od1_size, od1_list);

    // Remove any scaling component from source object-to-world 
    // initial guess transform, since the matcher can't deal with it.
    // Obj-to-world transform is rotate, then scale, then translate, so
    // we can't move the scaling between source model-to-object and
    // source object-to-world. Instead, the inverse scaling is applied
    // to the destination, so matching is in an interim world space.
    double xscale = objn1->xscale.get_value();
    double yscale = objn1->yscale.get_value();
    double zscale = objn1->zscale.get_value();
    objn1->xscale.set_value(1.0);
    objn1->yscale.set_value(1.0);
    objn1->zscale.set_value(1.0);
    objn1->x.set_value(objn1->x.get_value() / xscale);
    objn1->y.set_value(objn1->y.get_value() / yscale);
    objn1->z.set_value(objn1->z.get_value() / zscale);
    ZMatrix wscale;	// interim world space scaling
    MakeScaleMatrix(wscale, 1.0/xscale, 1.0/yscale, 1.0/zscale);

    // setup transform from world to destination model space
    ZMatrix dest_m2w, dest_w2m;
    objn2->GetTransformationMatrix(dest_m2w);
    // adjust transform for source scaling removed above
    MatPostMult(dest_m2w, wscale);
    // get inverse transform (world to model)
    MatInvert(dest_m2w, dest_w2m);

    // convert error radius to squared radius for find_points...()
    err_radius *= err_radius;

    // Setup match_list array
    // od2_match_list holds destination points 
    // that "match" corresponding source point (in world space).
    // Allocate for number of source points
    NodeSpec **od2_match_list = new NodeSpec *[od1_size];

    // Pool of match vector sums
    double *match_sum = NULL;
    int *match_cnt = NULL;
if (icp_flags & (4 | 8))
    match_sum = new double[3 * od1_size];
if (icp_flags & (4 | 8 | 64)) 
    match_cnt = new int[od1_size];

    // allocate corresponding list to track match distance
    double *distance = new double[od1_size];

    // Define aej containers
aejVector *matchpts, *sourcepts;
if (icp_flags & (8 | 64)) {	// extras for duplicating for weighting 
    matchpts = new aejVector[MAX_REPEAT*od1_size];
    sourcepts = new aejVector[MAX_REPEAT*od1_size];
} else {
    matchpts = new aejVector[od1_size];
    sourcepts = new aejVector[od1_size];
}

#if 0
    if (verbose) {	// all allocated, monitor dynamic memory
		struct mallinfo mi = mallinfo();
		fprintf(stderr, "Mem: cur=%8ldK @ ICP match\n",
				(mi.usmblks + mi.uordblks) / 1024);
    }
#endif

    // Iteration starts here
    int loop;
    double xrot, yrot, zrot;

    // Save previous values for epsilon and tolerance tests
    double prev_x, prev_y, prev_z;
    double prev_xrot, prev_yrot, prev_zrot;
    // compiler thinks they don't get initialized...
    prev_x = prev_y = prev_z = prev_xrot = prev_yrot = prev_zrot = 0.0;

    // To speed things up, start by testing only a sampling of points 
    // (every pt_increment point).
    // For final search, pt_increment is set to 1.
    if (od1_size >= 2000)
    	pt_increment = od1_size / 1000;
    else
    	pt_increment = 1;
    int pt_inc0 = pt_increment;		// save initial increment for histogram

    for (loop = 0; loop < max_iterations; loop++) {
	int pt_inc = pt_increment;	// save loop's start value for diag
        // Get the source object to world transform--
        // this is the one to be adjusted iteratively.
        ZMatrix objworld1;
        objn1->GetObjToWorldTransform( objworld1 );
	DBGMATDUMP(stderr, "orig objworld1", objworld1);

	// transmat maps source object to destination model
	ZMatrix transmat;
	MatCopy(dest_w2m, transmat);
        MatPreMult(transmat, objworld1);
	DBGMATDUMP(stderr, "Transmat", transmat);

       	// For each source point in od1_list (obj space), 
       	// find match in destination model space and store in match list.
	// Search every pt_increment point.
	for (i=0; i<od1_size; i+= pt_increment) {
		Source_voxel *od1_ptr = &od1_list[i];

    		// Transform source point to dest model space
		double find_point_mdl[3];
    		MultPoints(od1_ptr->ctr, transmat, find_point_mdl);

		// if outside the destination bounding volume, no match
		if (!bvol.in_range(find_point_mdl)) {
			distance[i] = FLT_MAX;
			continue;
		}
		
		// search destination in model space
    		distance[i] = od2->fast_find_closest(od2->get_level(),
    			(double)FLT_MAX, find_point_mdl, 
    			find_point_mdl, &od2_match_list[i]);

		// destination point coordinates
		double dpt[3];
if (icp_flags & 128) {
		// find surface near destination, not just the sample point
		match_surface(find_point_mdl, od2_match_list[i], dpt);
} else {
		od2_match_list[i]->get_global_center(dpt);
}

if (icp_flags & (4 | 8)) {
		// record vector from source to destination
		vector_diff(dpt, find_point_mdl, match_sum+3*i);
		match_cnt[i] = 1;
}

if (icp_flags & (32 | 64)) {
		// ignore or unweight matches where the match vector 
		// is mostly parallel to the surface, 
		// to allow surface sliding and favor
		// features perpendicular to the surface

		// unit vector in dest model space from dest pt to src pt
		double match_vec[3];
		vector_diff(find_point_mdl, dpt, match_vec);
		normalize_vector(match_vec);	

		// surface normal at matched dest pt
		double snorm[3];
		od2_match_list[i]->get_normal(snorm);

		// dot product = cosine of angle
		double dot = dot_product(snorm, match_vec);

if (icp_flags & 32) {
		// ignore match if parallel to surface = perp to normal
		// = angle around 90 degrees = cosine around zero
		if (fabs(dot) < sfc_cos_limit)
			distance[i] = FLT_MAX;
} else {
		// weight match lower if cosine smaller
		match_cnt[i] = int(fabs(dot) * MAX_REPEAT);
}
}

#if 0
// ** temp test for large-distance matches - check up on fast_find
	if (distance[i] > 0.01*0.01) {
		NodeSpec *ns;
	    	double brute_dist = od2->search_model_space(
				find_point_mdl, &ns);
		if (brute_dist != distance[i]) {
			fprintf(stderr, "YIKES: fast find source "
				"%f %f %f found %f %f %f at D2=%f\n",
				find_point_mdl[0], find_point_mdl[1],
				find_point_mdl[2], match_point[0],
				match_point[1], match_point[2],
				distance[i]);
			double brute_pt[3];
			ns->get_global_center(brute_pt);
			fprintf(stderr, "but exhaustive search finds "
				"%f %f %f at D2=%f\n",
				brute_pt[0], brute_pt[1], brute_pt[2],
					brute_dist);
		}
	}
// ** end test
#endif

		// deep diagnostics for visualizing matcher
		if (verbose > 3 && od1_size/pt_increment <= 2000)
			fprintf(stderr, "M %f %f %f  %f %f %f %d %g\n",
				find_point_mdl[0], find_point_mdl[1], 
				find_point_mdl[2], dpt[0], dpt[1], dpt[2], 
				i, distance[i]);

	} // end for each point in od1_list

        // match distance cutoff (squared) for outlier rejection
	// various strategies...
        double max_dist2;

	// max discard is 0%, 1%, ...
	//max_dist2 = cutoff_distance(distance, od1_size, pt_inc0, loop);

	// OR: limit cutoff to 50%, 25%, ...
if (icp_flags & 1)
	max_dist2 = cutoff_distance(distance, od1_size, pt_inc0, 50/(loop+1));

	// OR: no cutoff
else if (icp_flags & 2)
	max_dist2 = FLT_MAX/2.0;

	// OR: cutoff = multiple of destination model resolution
else if (icp_flags & 1024) {
	max_dist2 = 10.0 * 4.0 / (1 << oct2->get_max_levels());
	max_dist2 *= max_dist2;

	// OR: max discard is 0%, 2%, ... (default logic)
} else
	max_dist2 = cutoff_distance(distance, od1_size, pt_inc0, 2*loop);

	if (verbose > 1)
		fprintf(stderr, "cutoff dist = %g\n", sqrt(max_dist2));

	double sum_cutoff = 0.0;
if (icp_flags & (4 | 8)) {
	// To increase weight of features outside the mostly flat
	// parts of the terrain, find the vector sum of each match
	// at the destination nodes (assumed to be lower res than
	// the source), and discard matches with small vector sum
	// lengths which presumably indicate matches from scattered
	// data distributed in the same plane
        for (i=0; i<od1_size; i+=pt_increment) {
        	// for each match, see if any other match found the
        	// same destination point. This search could be avoided
        	// by storing match vectors in the nodespecs, although
		// logic to clear them out before summing is needed.
		NodeSpec *ns = od2_match_list[i];
		int j;
		for (j=0; j<i; j++) {
			if (ns == od2_match_list[j]) {
				// found previous match to this dest node
				double *tn = match_sum + 3*i;
				double *pn = match_sum + 3*j;
				vector_sum(pn, tn, pn);
				// count number of vectors summed to j
				match_cnt[j]++;
				// link i to j
				match_cnt[i] = -j;
				break;
			}
		}
	}

	// now convert vector sums to average length
	// and choose a cutoff
	int sum_count = 0;
        for (i=0; i<od1_size; i+=pt_increment) {
		if (match_cnt[i] < 1)	// skip links
			continue;
        	double *ms = match_sum + 3*i;
        	ms[0] = vector_magnitude(ms) / match_cnt[i];
		if (icp_flags & 4) {	// find average
        		sum_cutoff += ms[0];
        		sum_count++;
		} else {		// find max
			if (ms[0] > sum_cutoff)
				sum_cutoff = ms[0];
		}
 	}
	if (sum_count) {
		// cutoff = 3/4 of mean?
		sum_cutoff = vsum_factor * (sum_cutoff / sum_count);
	}
}

	// We want to reject matches that are too far from their closest
	// destination point, and other source points within an error
	// radius of too-far matches.  (To ignore outliers).

	// mark bad matches and their neighbors as invalid for current loop
	int mag_rejects = 0;
        for (i=0; i<od1_size; i+=pt_increment) {
		Source_voxel *od1_ptr = &od1_list[i];

// ignore really close matches?
if ((icp_flags & 512) && distance[i] < min_dist2) {
			od1_ptr->valid &= (~2);	// mark invalid
			continue;
		}

		if (distance[i] <= max_dist2) {	// close match, keep it
if (icp_flags & 4) {
			// also check vector sum
			int j = match_cnt[i];
			double vsum;
			if (j < 1)	// link
				vsum = match_sum[-3*j];
			else
				vsum = match_sum[3*i];
			if (vsum < sum_cutoff) {
				mag_rejects++;
				od1_ptr->valid &= (~2);	// mark invalid
				if (verbose > 3 && 
					  od1_size/pt_increment <= 2000) {
					fprintf(stderr, "VMR %d\n", i);
					continue;
				}
			}
}

			od1_ptr->valid |= 2;	// mark as valid
			continue;
		}

		if (err_radius) {
			double mctr[OCTREE_DIMS];	// model coords
			od1_ptr->ns->get_global_center(mctr);
			NodeSpec_List_Element *nodelist = NULL;
			od1->find_points_within_radius(oct1->get_max_levels(), 
				mctr, mctr, err_radius, &nodelist);
			NodeSpec_List_Element *tmp_node, *next_node;
			for (tmp_node=nodelist; tmp_node; tmp_node=next_node) {
				tmp_node->nodespec->id &= (~2);
				next_node = tmp_node->next;
				delete tmp_node;	// clean up
			}

		} else {
			// just mark the current point invalid
			od1_ptr->valid &= (~2);
		}
	}

	if (verbose && (icp_flags & 12))
		fprintf(stderr, 
			"Match vector mag cutoff = %f, %d rejected\n",
				sum_cutoff, mag_rejects);

        // Convert the valid vectors to aej format for least squares routine.
	double dsum = 0.0;			// match quality diagnostic

int rep_cnt[MAX_REPEAT+1];
memset(rep_cnt, 0, sizeof(rep_cnt));

        for (i=0,u=0; i<od1_size; i+=pt_increment) {
	    Source_voxel *od1_ptr = &od1_list[i];

#ifdef KEEP_POINTS_FOREVER
	    // to accept points that were valid in a previous iteration,
	    // merge "currently valid" with "previously valid" (either is ok)
	    od1_ptr->valid |= (od1_ptr->valid >> 1);
	    if (od1_ptr->valid & 1) {
#else
	    if (od1_ptr->valid & 2) {	// just look at "currently valid"
#endif
		double mpt_mdl[3], mpt_wld[3];
		// get matched point location (dest model space)
if (icp_flags & 128) {
		// find surface near destination, not just the sample point
		// (maybe store this above?)
		double find_point_mdl[3];
    		MultPoints(od1_list[i].ctr, transmat, find_point_mdl);
		match_surface(find_point_mdl, od2_match_list[i], mpt_mdl);
} else {
		od2_match_list[i]->get_global_center(mpt_mdl);
}
		// convert to interim world space
       		MultPoints(mpt_mdl, dest_m2w, mpt_wld);

if (icp_flags & 8) {
		// weight this match based on vector sum,
		// by repeating it in the match list
		int j = match_cnt[i];
		double vsum;
		if (j < 1)	// link
			vsum = match_sum[-3*j];
		else
			vsum = match_sum[3*i];
#if 0
		// max vector repeats MAX_REPEAT times, min is used one time
		int repeats = int(MAX_REPEAT * vsum / sum_cutoff + 0.999);
#else
		// max vector repeats MAX_REPEAT times, min is discarded
		int repeats = int(MAX_REPEAT * vsum / sum_cutoff + 0.001);
#endif
		rep_cnt[repeats]++;
		for (int r=0; r<repeats; r++) {
		  matchpts[u]  = aejVector(mpt_wld[0], mpt_wld[1], mpt_wld[2]);
		  sourcepts[u] = aejVector(od1_ptr->ctr[0],
					od1_ptr->ctr[1], od1_ptr->ctr[2]);
		  u++;
		  dsum += distance[i];
		}

} else if (icp_flags & 64) {
		// weighted match, weight is in match_cnt[]
		int repeats = match_cnt[i];
		rep_cnt[repeats]++;
		for (int r=0; r<repeats; r++) {
		  matchpts[u]  = aejVector(mpt_wld[0], mpt_wld[1], mpt_wld[2]);
		  sourcepts[u] = aejVector(od1_ptr->ctr[0],
					od1_ptr->ctr[1], od1_ptr->ctr[2]);
		  u++;
		  dsum += distance[i];
		}

} else {	// normal case, single match
		matchpts[u]  = aejVector(mpt_wld[0], mpt_wld[1], mpt_wld[2]);
		sourcepts[u] = aejVector(od1_ptr->ctr[0],
					od1_ptr->ctr[1], od1_ptr->ctr[2]);
		u++;
		dsum += distance[i];
}
	    }
	}
if (verbose && (icp_flags & (8 | 64))) {
	for (int i=0; i<=MAX_REPEAT; i++)
		printf("%d@%dX ", rep_cnt[i], i);
	printf("\n");
}

	// need at least a few matches!
	// ** maybe if at least one match, do translate-only match
	if (u < 4) {
        	fprintf (stderr, "summitt_icp_match_octrees: "
					"not enough match points\n");
		if (pt_increment > 1 && loop < max_iterations-1) {
			pt_increment /= 10;
			if (pt_increment < 10)
				pt_increment = 1;
			continue;
		}
		if (cleanup) {	// abort, clean up the mess
			delete[] sourcepts;
			delete[] matchpts;
			delete[] distance;
			delete[] match_cnt;
			delete[] match_sum;
			delete[] od2_match_list;
	    		delete[] od1_list;
		}
        	return -1.0;
    	}

        // Run least squares on match
        resultTR = TransformationFromMatchedPoints(u,sourcepts,matchpts);
        trans = resultTR.trans();
	aejMatrix rot = resultTR.rot();
	
        aejVector row0 = rot.getRow(0);
        aejVector row1 = rot.getRow(1);
        aejVector row2 = rot.getRow(2);

	// rotation 1, Rz*Ry*Rx
        yrot = - asin(row2[0]);
        xrot =   asin(row2[1]/cos(yrot));
        zrot =   acos(row0[0]/cos(yrot));

	// row2[2] and row1[0] need to be the same sign
#if 1
	double check1 = cos(yrot)*cos(xrot); 
	if ((check1 * row2[2]) < 0.0) {
		if (verbose)
			fprintf(stderr, "Changing angle of xrot\n");
		xrot = 3.14159265358979 - xrot; 
	}

	double check2 = sin(zrot)*cos(yrot); 
	if ((check2 * row1[0]) < 0.0) {
		zrot = -zrot;
	}
#endif

	//Reset parms for objworld1 matrix based on these results 

        objn1->x.set_value(trans[0]);
        objn1->y.set_value(trans[1]);
        objn1->z.set_value(trans[2]);
        objn1->xrot.set_value(xrot*TODEGREES);  // set_value expects degrees
        objn1->yrot.set_value(yrot*TODEGREES);
        objn1->zrot.set_value(zrot*TODEGREES);

	// Test epsilon and tolerance to see if we can break out of the least squares loop
	//double tol2 =100.0 * tolerance;	// near tolerance for pt_inc>1
	double tol2 = 5.0 * tolerance;	// near tolerance for pt_inc>1
	if (loop > 0) {

	   x_toltest = change_ratio(prev_x, trans[0]);
	   y_toltest = change_ratio(prev_y, trans[1]);
	   z_toltest = change_ratio(prev_z, trans[2]);
	   xrot_toltest = change_ratio(prev_xrot, objn1->xrot.get_value());
	   yrot_toltest = change_ratio(prev_yrot, objn1->yrot.get_value());
	   zrot_toltest = change_ratio(prev_zrot, objn1->zrot.get_value());

	   if ((x_toltest <= tolerance || fabs(prev_x-trans[0]) <= tepsilon) &&
	       (y_toltest <= tolerance || fabs(prev_y-trans[1]) <= tepsilon) &&
	       (z_toltest <= tolerance || fabs(prev_z-trans[2]) <= tepsilon) &&
	       (xrot_toltest <= tolerance ||
			fabs(prev_xrot-objn1->xrot.get_value()) <= repsilon) &&
	       (yrot_toltest <= tolerance ||
			fabs(prev_yrot-objn1->yrot.get_value()) <= repsilon) &&
	       (zrot_toltest <= tolerance ||
			fabs(prev_zrot-objn1->zrot.get_value()) <= repsilon)) {

		// If we have reached tolerance the first time, up the pt_increment to 1
		// to test with every point to be sure the calculation is as close
		// as we can get.

		if (first_time) {
		   first_time = FALSE;	
		   pt_increment = 1;
		   if (verbose)
			fprintf(stderr, "pt_inc = 1 after iter %d\n", loop);
		} else {
	           fprintf(stderr, "TOLERANCE REACHED at iteration %d!!, tolerance = %f\n", loop, tolerance);
		   break;
		}

	   // See if we are closing i
	   // and can increase the number of points sampled.
	   } else if (pt_increment > 10 &&
		 (x_toltest <= tol2) && (y_toltest <= tol2) &&
	         (z_toltest <= tol2) && (xrot_toltest <= tol2) &&
	         (yrot_toltest <= tol2) && (zrot_toltest <= tol2)) {
			pt_increment /= 10;
			if (pt_increment < 10)
				pt_increment = 1;	// get it over with
			if (verbose)
				fprintf(stderr, "pt_inc = %d after iter %d\n",
					pt_increment, loop);
	   }

	   // ** else failed tolerance; if pt_increment = 1 and
	   // worse than last time, maybe stop?

	   // monitor progress
	   if (verbose>1 || (verbose && (loop % 10) == 0)) {	
		double maxtol = x_toltest;
		char maxd = 'X';
		if (y_toltest > maxtol) {	maxtol = y_toltest; maxd = 'Y'; }
		if (z_toltest > maxtol) { 	maxtol = z_toltest; maxd = 'Z'; }
		if (xrot_toltest > maxtol) { 	maxtol = xrot_toltest; maxd ='r'; }
		if (yrot_toltest > maxtol) { 	maxtol = yrot_toltest; maxd ='p'; }
		if (zrot_toltest > maxtol) { 	maxtol = zrot_toltest; maxd = 'w'; }
		fprintf(stderr, "Loop %3d: pt_inc=%3d, #match=%d, "
				"maxtol=%f on %c avgdist=%f\n",
			loop, pt_inc, u, maxtol, maxd, sqrt(dsum/u));
		fprintf(stderr, "  x=%f y=%f z=%f r=%f p=%f y=%f\n", 
			trans[0], trans[1], trans[2],
	   		objn1->xrot.get_value(), objn1->yrot.get_value(),
			objn1->zrot.get_value());
	    }

	} else if (verbose > 1) {	// loop 0 diagnostic
		fprintf(stderr, "Loop %3d: pt_inc=%3d, #match=%d avgdist=%f\n",
			loop, pt_inc, u, sqrt(dsum/u));
		fprintf(stderr, "  x=%f y=%f z=%f r=%f p=%f y=%f\n", 
			trans[0], trans[1], trans[2],
	   		objn1->xrot.get_value(), objn1->yrot.get_value(),
			objn1->zrot.get_value());
	}

	// Set previous values
        prev_x = objn1->x.get_value(); 
        prev_y = objn1->y.get_value();
        prev_z = objn1->z.get_value();
        prev_xrot = objn1->xrot.get_value();  // stored in degrees
        prev_yrot = objn1->yrot.get_value();  // stored in degrees
        prev_zrot = objn1->zrot.get_value();  // stored in degrees
    }

    // If loop ran out because of max_iterations...
    if (loop == max_iterations)
	fprintf(stderr, "Reached max iterations %d!!\n", max_iterations);

    // cleanup source transform with final results, restoring scale factor
    objn1->x.set_value(trans[0]*xscale);
    objn1->y.set_value(trans[1]*yscale);
    objn1->z.set_value(trans[2]*zscale);
    objn1->xscale.set_value(xscale);
    objn1->yscale.set_value(yscale);
    objn1->zscale.set_value(zscale);

    // free up all the stuff we allocated
    // (conditional, to save time if we don't care)
    if (cleanup) {
	delete[] sourcepts;
	delete[] matchpts;
	delete[] distance;
	delete[] match_cnt;
	delete[] match_sum;
	delete[] od2_match_list;
    	delete[] od1_list;
    }

    return 10.0;	// ** stub

} // end summitt_icp_match_octrees

/// alpha (color?)-based octree matching
/**
**    This is the comparison method for evaluating octrees
**    from the SUMMITT project perspective.  
**    Nodes are compared in one tree vs. the other using
**    the alpha values.  The alphas are multiplied, and the 
**    cumulative sum is tallied and returned as a method of 
**    evaluating how "good" the match is.
**
**    objn1 is source ObjNode, objn2 is destination.
*/

float summitt_match_octrees(ObjNode *objn1, ObjNode *objn2)
{
	Octree *oct1;
	Octree *oct2;
	Octree_Data *od1;
	Octree_Data *od2;
	float 	tally = 0.0;
	ZMatrix mat1, mat2, transmat;

    /*
    ** Get the octrees from the ObjNodes
    */
    if (!(oct1 = (Octree *)objn1->get_object())) {
	fprintf (stderr, "summitt_match_octrees:  octree1 is null\n");
	return -1.0;
    }

    if (!(oct2 = (Octree *)objn2->get_object())) {
	fprintf (stderr, "summitt_match_octrees:  octree2 is null\n");
	return -1.0;
    }

    if (!(od1 = oct1->get_data())) {
	fprintf (stderr, "summitt_match_octrees:  failed to get Octree_Data for octree 1\n");
	return -1.0;
    }

    if (!(od2 = oct2->get_data())) {
	fprintf (stderr, "summitt_match_octrees:  failed to get Octree_Data for octree 2\n");
	return -1.0;
    }

    /*
    ** Build transformation matrix to convert from octree1 space
    ** to octree2 space.
    ** Matrix should be mult of oct 1: model to object space, oct 1 to world 
    ** space, oct 2 world space to object, oct 2 object to octree/model space
    */

    objn1->GetTransformationMatrix( mat1 );
    // MatDump(stdout, "trans matrix oct 1", mat1);

    /* Note: the obj->world matrix for mat2 will default to identity */
    objn2->GetTransformationMatrix( mat2 );
    // MatDump(stdout, "trans matrix oct 2", mat2);

    /* Invert trans matrix 2 */
    MatInvert(mat2, transmat);
    // MatDump(stdout, "inverse matrix 2", transmat);

	   /* mult trans for mat1 by transmat to get the trans. matrix.
           ** Store resultng matrix in mat1 */
    MatMult(transmat, mat1);
    // MatDump(stdout, "trans matrix final", transmat);

    /* Get octree_1's data nodes one at a time and compare to octree2 
    ** For efficiency, it is expected that octree1 is the smaller, 
    ** "unknown" octree.
    */
    traverse_data (od1, od2, transmat, &tally);

    if (verbose)
	    fprintf(stderr, "summitt_match returning tally %f\n", -tally);

    return( -tally );

} // end summitt_match_octrees

// Transform a point from octree 1 model space to octree 2 model space.
// If geo1 is not NULL, the octrees are matched in geo space, so
// the sequence is pt -> m2w1 -> lat/lon/el -> w2m2 -> newpt.
// Otherwise, octrees are matched in world frame, and m2w1 is 
// really the combined transform w2m2*m2w1, so the sequence 
// is pt -> m2w1 -> newpt.
static void xform_point(double pt[], double newpt[],
	ZMatrix m2w1, ZMatrix w2m2, GeoData *geo1, GeoData *geo2)
{
	MultPoints(pt, m2w1, newpt);
	if (geo1) {
		double lat, lon, el, tpt[3];
		geo1->xyztolle(newpt[0], newpt[1], newpt[2], lat, lon, el);
		geo2->lletoxyz(lat, lon, el, tpt[0], tpt[1], tpt[2]);
		MultPoints(tpt, w2m2, newpt);
	}
}

/// merge octrees
/**
merge_data will traverse all the nodes in od1, transform
those voxels to od2's model space, then call add_voxel
to add them to od2's model space.  The optional edge_scale
argument is used to scale the size of the voxels if needed.

** NOTE: Since voxels are currently moved rather than duplicated,
** this function leaves the od1 source octree with no voxels but
** leaves the tree linkage.
*/
void merge_data (Octree_Data * od1, Octree_Data *od2, ZMatrix m2w1,
	ZMatrix w2m2, GeoData *geo1, GeoData *geo2,
	Boolean accumulate_on, double edge_scale=1.0)
{
	int  ref;
	NodeSpec *ns1;
	NodeSpec *nextptr;
	double  center[OCTREE_DIMS];
	double  center_trans[OCTREE_DIMS];

    if (!od1 || !od2) return;

    ns1 = od1->get_node_data();
    while (ns1) {
		/* Get the x,y,z of octree 1 */
		ns1->get_global_center(center);

		/* transform to octree 2 model space */
		xform_point(center, center_trans, m2w1, w2m2, geo1, geo2);

		/* Center is _new_ center, after transformation */
		ns1->set_global_center(center_trans);
		ns1->edge_length *= edge_scale;
		/* add to od2 */
		nextptr = ns1->next;
		ns1->next = NULL;

		// Either add or accumulate the voxel
		if (accumulate_on == FALSE)
			od2->add_voxel(ns1);
		else
			od2->accumulate_voxel(ns1);	// ** may leak ns1??
			
		ns1 = nextptr;
    }
    // remove linkage from source octree to moved voxels
    od1->release_nodespec();

    for (ref=UPPER_LEFT_BACK; ref<NUMBER_OF_SPACE_PARTITIONS; ref++)
	    merge_data(od1->get_child((Octree_Child_Ref)ref), od2, 
		m2w1, w2m2, geo1, geo2, accumulate_on, edge_scale);

}

/// add levels to octree as needed for merging
/**
** Check points in source octree.  If necessary, add levels to the
** destination octree, either above or below the current octree.
*/
void summitt_verify_level( Octree *oct1, Octree *oct2, 
	ZMatrix m2w1, ZMatrix w2m2, GeoData *geo1, GeoData *geo2)
{
	long level1, level2;
	double res1, res2;
	double p1[3], p2[3], mp1[3], mp2[3];
	double dist;
	int count;

	// Test for need to add levels at the bottom of the tree
	// In this case, the destination octree is not as deep as the
	// incoming tree.
	level1 = oct1->get_max_levels();
	level2 = oct2->get_max_levels();

	if (verbose)
		fprintf(stderr, "octree1 level: %d, and octree2 level: %d\n", 
			level1, level2);

	res1 = 2.0/ pow(2.0, (double)level1);
	res2 = 2.0/ pow(2.0, (double)level2);

	if (verbose)
		fprintf(stderr, "Res1 = %f and res2 = %f\n", res1, res2);

        // Transform two points p1 and p2 from model space 1 to model space 2
        p1[0] = p1[1] = p1[2] = 0.0; // p1 = (0,0,0)
        p2[0] = res1;
        p2[1] = p2[2] = 0.0;         // p2 = (res1,0,0)

	xform_point(p1, mp1, m2w1, w2m2, geo1, geo2);
	xform_point(p2, mp2, m2w1, w2m2, geo1, geo2);

        if (verbose) {
                fprintf(stderr, "mp1 = (%f, %f, %f)\n", mp1[0], mp1[1], mp1[2]);
                fprintf(stderr, "mp2 = (%f, %f, %f)\n", mp2[0], mp2[1], mp2[2]);
        }

        dist = distance(mp1, mp2);
        if (verbose)
                fprintf(stderr, "dist = %f;  res2*0.75 = %f\n", 
                        dist, res2*0.75);

        if ( dist < res2*0.75 ) {
                count = 0;  // Count the number of new levels needed
                while (dist < res2*0.75) {
                        count++;
                        res2 /= 2;
                        if (verbose)
                                fprintf(stderr, "In loop: dist = %f;  res2*0.75 = %f\n", 
                                        dist, res2*0.75);
                }

                if (verbose) {
                        fprintf(stderr, "Adding %d levels to bottom of octree 2\n", count);
                        fprintf(stderr, "Setting level = %d\n", level2+count);
                }
                oct2->set_max_levels(level2+count);
        }

#ifdef ADDING_AT_TOP
        // Transform all 8 corners of oct1 from model1 to model2 space
        // Multiply each vertex by the transformation
        double trans_vertex[NUMBER_OF_SPACE_PARTITIONS][3];
        for( i=0; i < NUMBER_OF_SPACE_PARTITIONS ; i++) {
                vertex[0] = X_Offset[i];
                vertex[1] = Y_Offset[i];
                vertex[2] = Z_Offset[i];

                // Writing 3 values at a time, x, y, and z
		xform_point(vertex, trans_vertex[i], m2w1, w2m2, geo1, geo2);
        }

        // Find min and max for x, y, and z of all 8 transformed corners.
        double xmin, xmax, ymin, ymax, zmin, zmax;
        xmin = xmax = trans_vertex[0][0];
        ymin = ymax = trans_vertex[0][1];
        zmin = zmax = trans_vertex[0][2];

        fprintf(stderr, "Find min and max Z for vertices\n");

        for( i=1; i < NUMBER_OF_SPACE_PARTITIONS ; i++) {
                // Test the x's
                if (trans_vertex[i][0] < xmin)
                        xmin = trans_vertex[i][0];
                else if (trans_vertex[i][0] > xmax)
                        xmax = trans_vertex[i][0];

                // Test the y's
                if (trans_vertex[i][1] < ymin)
                        ymin = trans_vertex[i][1];
                else if (trans_vertex[i][1] > ymax)
                        ymax = trans_vertex[i][1];

                // Test the z's
                if (trans_vertex[i][2] < zmin)
                        zmin = trans_vertex[i][2];
                else if (trans_vertex[i][2] > zmax)
                        zmax = trans_vertex[i][2];
        }

        if (verbose) {
                fprintf(stderr, "Xmin = %f, Xmax = %f\n", xmin, xmax);
                fprintf(stderr, "Ymin = %f, Ymax = %f\n", ymin, ymax);
                fprintf(stderr, "Zmin = %f, Zmax = %f\n", zmin, zmax);
        }
        
        // If all 6 values are between -1 and 1, then we don't need new levels
        // If one or more is out of bounds, we need to figure out what
        // child to attach to.
        while( xmin < -1.0 || xmax > 1.0 || ymin < -1.0 || ymax > 1.0 || 
               zmin < -1.0 || zmax > 1.0 )
        {
                // figure out which child
                // attach previous top to correct child of new node

                // set level of new top node, get_level, increment, set_level
                level = oct2->get_levels();
                level++;
                oct2->set_max_levels(level);
        }
#endif // ADDING_AT_TOP
}

// merge octrees - top-level interface
/**
**      This is a method for merging octrees
**      Each node will be extracted from objn1,
**      transformed to objn2 object space, then added
**      to objn2 (actually its Octree object)

To be more descriptive, this algorithm is an in memory merge
without resize if parts of A (objn1) extend outside the box
of B (objn2).  The algorithm is as follows:
        read A
        read B
        build xform matrix which xforms from A model space to
                A object space, A object space to world space,
                world space to B object space, then from B
                object space to B model space
		(or setup georeferenced match transforms)
        traverse all voxels in A
                xform to B model space
                add_voxel to B

NOTE: This function leaves source octree objn1 in a funny state!
(see merge_data)
*/

float summitt_merge_octrees(ObjNode *objn1, ObjNode *objn2, 
        Boolean accumulate_on, Boolean level_grow)
{
        Octree *oct1;
        Octree *oct2;
        Octree_Data *od1;
        Octree_Data *od2;

        if (verbose) {  // show octree names
                char *n1 = objn1->get_name();
                char *n2 = objn2->get_name();
                if (n1 && n2) {
                        char *r1 = objn1->get_reference();
                        char *r2 = objn2->get_reference();
                        fprintf(stderr, "Merging octree %s/%s to %s/%s\n",
                                n1, r1 ? r1 : "", n2, r2 ? r2 : "");
                }
        }

    /*
    ** Get the octrees from the ObjNodes
    */
    if (!(oct1 = (Octree *)objn1->get_object())) {
        fprintf (stderr, "summitt_merge_octrees:  octree1 is null\n");
        return -1.0;
    }

    if (!(oct2 = (Octree *)objn2->get_object())) {
        fprintf (stderr, "summitt_merge_octrees:  octree2 is null\n");
        return -1.0;
    }

    if (!(od1 = oct1->get_data())) {
        fprintf (stderr, "summitt_merge_octrees:  failed to get Octree_Data for octree 1\n");
        return -1.0;
    }

    if (!(od2 = oct2->get_data())) {
        fprintf (stderr, "summitt_merge_octrees:  failed to get Octree_Data for octree 2\n");
        return -1.0;
    }

    /*
    ** Build transformation matrix to convert from octree1 space
    ** to octree2 space. If both models are georeferenced, the matching
    ** is through geo coordinates and must be done in steps.
    */
    GeoData *geo1 = NULL;
    GeoData *geo2 = objn2->get_geo_data();
    if (geo2)
        geo1 = objn1->get_geo_data();
    // now geo1!=NULL means geo matched (both octrees are georeferenced)

    ZMatrix m2w1;       // model-to-world for octree 1
    objn1->GetTransformationMatrix( m2w1 );
    DBGMATDUMP(stderr, "merge(): trans matrix oct 1", m2w1);

    /* Note: the obj->world matrix for mat2 may default to identity */
    ZMatrix m2w2;       // model-to-world for octree 2
    objn2->GetTransformationMatrix( m2w2 );
    DBGMATDUMP(stderr, "merge(): trans matrix oct 2", m2w2);

    /* Invert trans matrix 2 (world-to-model) */
    ZMatrix w2m2;
    MatInvert(m2w2, w2m2);
    DBGMATDUMP(stderr, "merge(): inverse of matrix 2", w2m2);

    if (!geo1) {
        // not geo matched, build combined transform w2m2*m2w1 into m2w1
        MatMult(w2m2, m2w1);
        MatCopy(w2m2, m2w1);
        DBGMATDUMP(stderr, "merge(): trans matrix final", m2w1);
    }

    /*
    ** If necessary, adjust the octree level in the destination octree
    ** to properly accomodate the new octree.
    */
    if (level_grow == TRUE)     // ** fix for geo!
        summitt_verify_level(oct1, oct2, m2w1, w2m2, geo1, geo2);

    /* Get octree_1's data nodes one at a time and merge into octree2 
    ** For efficiency, it is expected that octree1 is the smaller, 
    ** "unknown" octree.
    */
    merge_data(od1, od2, m2w1, w2m2, geo1, geo2, accumulate_on);

    return(1.0);
}

// Recursive part of scan for octree extent.
static void octree_extents(Octree_Data *oct, Summitt_range *r)
{
	// recursively process each valid child of node
	for(int ref=UPPER_LEFT_BACK; ref<NUMBER_OF_SPACE_PARTITIONS; ref++) {
		Octree_Data *kid = oct->get_child((Octree_Child_Ref)ref);
		if (kid)
			octree_extents(kid, r);
	}

	// check all voxels at this node
	for (NodeSpec *ns = oct->get_node_data(); ns; ns = ns->next) {
		double center[3];
		ns->get_global_center(center);
		r->include(center);
	} 
}

/// Find min/max coordinates of octree voxels in model space.
void summitt_octree_extents(Octree *octree, Summitt_range *r)
{
	r->init();
	Octree_Data *od = octree->get_data();
	if (od)
		octree_extents(od, r);
}

/// transform bounding volume in r from node's model space to world space
void world_volume(ObjNode *node, Summitt_range *r)
{
	ZMatrix m2w;
	node->GetTransformationMatrix(m2w);
	r->transform(m2w);
}

/// Find a bounding volume of octree in world space
// (not necessarily the smallest one)
void summitt_bounding_volume(ObjNode *node, Summitt_range *r)
{
	// find bounding cube in model space
	Octree *oct = (Octree *)node->get_object();
	summitt_octree_extents(oct, r);
	
	// convert to world space
	world_volume(node, r);
}
