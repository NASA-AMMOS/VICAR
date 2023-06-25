/* marscorr */
#include "vicmain_c"

#include "mars_support.h"
#include "gruen.h"
#include "mars_tiepoints.h"

#include "PigMission.h"
#include "PigLabelModel.h"
#include "PigFileModel.h"

#include <math.h>

/* buffer sizes in main program */
#define MAX_INPUTS 2
/* Should be MARS_MAX_NS but that busts memory on 32-bit machines. */
#define MAX_NS 7000
#define MAX_LEFT_AREA 31
#define MAX_RIGHT_AREA 81
#define MAX_TIEPOINTS 10000

#define EMPTY -32767
#define FAILED -32766		// Failed for unspecified reason
#define GOOD_CORR 0		// Really, anything >0 is good
#define QUAL_FACTOR 10000.	/* scaling for qual array */

#ifndef TRUE
#define TRUE 1
#endif
#ifndef FALSE
#define FALSE 0
#endif

// Structure to hold various things needed by the correlator

struct CorrelParams {
	short int qual[MAX_NS][MAX_NS];		// correl quality of each point
	double line_mask[MAX_NS][MAX_NS];	// matching line # in right img
	double samp_mask[MAX_NS][MAX_NS];	// matching line # in right img
	double left_dn[MAX_NS][MAX_NS];		// entire left image
	double right_dn[MAX_NS][MAX_NS];	// entire right image
	int nlin[2], nsin[2];			// [0]=left, [1]=right
	int nlw, nsw;				// left window size (template)
	int nlw2, nsw2;				// right window size (search)
	int max_left_area, max_right_area;	// max areas
	double min_quality;			// minimum acceptable corr qual
	int npts;				// number of tiepoints found
	int print_interval;			// how often to print status msg
};


static int get_best_neighbor(short int qual[MAX_NS][MAX_NS], int line, int samp,
	int &max_qual_line, int &max_qual_samp);
static int do_correlation(int left_line, int left_samp, int mode,
		int seed, int seed_line, int seed_samp,
		CorrelParams *p);
static int get_next_point(int &line, int &samp,
		int &sl_window, int &el_window, int &ss_window, int &es_window,
		int &border_passes, int nl_image, int ns_image);

////////////////////////////////////////////////////////////////////////

void main44()
{
    int i,j,s;
    int count, def;
    int status;
    int unit_in[MAX_INPUTS];
    int nids;
	const size_t msgLen = 256;
    char msg[256];

    // User parameters

    int band;
    int max_area[2];
    int templ[2], search[2];
    int mode;
    int limit_pts;
    int seed[4], seed_count;
    char seed_file[256];
    int seed_file_count;		// != 0 if seed file given
    int pair[2];
    struct TiePoint tiepoints[MAX_TIEPOINTS];

    struct CorrelParams *p;
    int left_line, left_samp;

    int border_passes;
    int sl_window, ss_window, el_window, es_window;

    LblImageData_typ ImageData;

    p = new CorrelParams;	// maybe too big for auto variable on Solaris?
    if (p == NULL) {
	zvmessage("Fatal memory error in initialization!", "");
	zabend();
    }

    zvpcnt("INP", &nids);
    if (nids > MAX_INPUTS) {
	zvmessage("Too many Input images.", "");
	zabend();
    }

    /* open input files */

    for (i=0; i < nids; i++) {
	zvunit(&unit_in[i], "INP", i+1, NULL);
	zvopen(unit_in[i], "OP", "READ",
		"U_FORMAT", "DOUB", "OPEN_ACT", "AS", NULL);
    }

    /* get input image dimensions */

    for (i=0; i < nids; i++) {
	zvget(unit_in[i], "NL", &p->nlin[i], "NS", &p->nsin[i], NULL);
	if (p->nsin[i] > MAX_NS) {
	    zvmessage("Input images exceed buffer limit sizes", "");
	    zabend();
	}
    }

    /* read input(s) into memory */

    zvp("BAND", &band, &count);
    for (j=0; j < p->nlin[0]; j++) {
	zvread(unit_in[0], &p->left_dn[j][0], "BAND", band, "LINE", j+1, NULL);
    }
    for (j=0; j < p->nlin[1]; j++) {
        zvread(unit_in[1], &p->right_dn[j][0], "BAND", band, "LINE", j+1, NULL);
    }

    /* get parameter overrides if any */

    zvparmd("QUALITY", &p->min_quality, &count, &def, 1, 0);

    zvp("TPTLIMIT", &limit_pts, &count); 
    if (limit_pts == -1)
	limit_pts = p->nlin[0] * p->nsin[0];	// accept all points
    // Print about 100 times if we get full coverage.  Round off to the
    // nearest 100 to make it look better.
    p->print_interval = ((int)((limit_pts/100) / 100)) * 100;
    if (p->print_interval < 100)
	p->print_interval = 100;

    // Determine the gruen mode to use.  See comments at top of gruen.com
    // for details of each mode.

    mode = 2;			/* default: only use amoeba in gruen */
    if (zvptst("linear"))		mode = 0;
    if (zvptst("annealing"))		mode = 1;
    if (zvptst("amoeba"))		mode = 2;
    if (zvptst("linear_amoeba"))	mode = 3;
    if (zvptst("annealing_amoeba"))	mode = 4;
    if (zvptst("amoeba2"))		mode = 5;
    if (zvptst("linear_amoeba2"))	mode = 6;

    if (mode == 1 || mode == 4) {
	zvmessage("WARNING!!!! Annealing modes not set up currently", "");
	zvmessage("Extra parameters must be given to gruen() correlator", "");
	zabend();
    }

    zvp("TEMPLATE", templ, &count);		// Size of area to correlate
    if (count == 1)
	templ[1] = templ[0];			// make it square
    p->nlw = templ[0];
    p->nsw = templ[1];
    if ((p->nlw % 2) == 0)			// make sure it's odd-sized
	p->nlw += 1;
    if ((p->nsw % 2) == 0)
	p->nsw += 1;

    zvp("SEARCH", search, &count);		// Size of area to search
    if (count == 1)
	search[1] = search[0];
    p->nlw2 = search[0];
    p->nsw2 = search[1];
    if ((p->nlw2 % 2) == 0)			// make sure it's odd-sized
	p->nlw2 += 1;
    if ((p->nsw2 % 2) == 0)
	p->nsw2 += 1;

    zvparm("AMAX", &max_area, &count, &def, 2, 0);
    if (count == 1)
	max_area[1] = max_area[0];
    if (count != 0) {
	p->max_left_area = max_area[0];
	p->max_right_area = max_area[1];
    }
    else {
	p->max_left_area = MAX_LEFT_AREA;
	p->max_right_area = MAX_RIGHT_AREA;
    }

    if ((p->nlw > p->max_left_area) || (p->nsw > p->max_left_area)) {
	zvmessage("Template too large", "");
	zabend();
    }
    if ((p->nlw2 > p->max_right_area) || (p->nsw2 > p->max_right_area)) {
	zvmessage("Search area too large", "");
	zabend();
    }

    /* clear correlation quality array */

    for (j=0; j < p->nlin[0]; j++) {
	for (i=0; i < p->nsin[0]; i++) {
	    p->qual[j][i] = EMPTY;
	}
    }
    p->npts=0;

    // Check for seed point, or an input tiepoint file of seeds

    zvparm("SEED", seed, &count, &def, 4, 0);
    seed_count = 1;

    zvp("SEEDFILE", seed_file, &seed_file_count);
    if (seed_file_count != 0) {	// Tiepoint file with seeds (ignore SEED)

	mars_load_tiepoints(seed_file, tiepoints, seed_count);

	zvp("SEEDPAIR", pair, &count);		// which image pair to get
    }

    /**************** Loop over each seed point *****************/

    int good_seeds = 0;
    for (s = 0; s < seed_count; s++) {

	if (seed_file_count != 0) {		// find next pair in file

	    if ((tiepoints[s].left_image == pair[0]) &&		// l,r order
		(tiepoints[s].right_image == pair[1])) {

		seed[0] = (int)(tiepoints[s].left_line + 0.5);
		seed[1] = (int)(tiepoints[s].left_sample + 0.5);
		seed[2] = (int)(tiepoints[s].corrected_line + 0.5);
		seed[3] = (int)(tiepoints[s].corrected_sample + 0.5);
	    }
	    else if ((tiepoints[s].left_image == pair[1]) &&	// r,l order
		     (tiepoints[s].right_image == pair[0])) {

		seed[0] = (int)(tiepoints[s].corrected_line + 0.5);
		seed[1] = (int)(tiepoints[s].corrected_sample + 0.5);
		seed[2] = (int)(tiepoints[s].left_line + 0.5);
		seed[3] = (int)(tiepoints[s].left_sample + 0.5);
	    }
	    else
		continue;			// it doesn't match
	}

	/********** GET SEED LOCATION ******************/

	left_line = seed[0];
	left_samp = seed[1];

	if (p->qual[left_line][left_samp] < GOOD_CORR) //if seed failed earlier,
	    p->qual[left_line][left_samp] = EMPTY;    // force a re-do

	// Do the initial correlation

	/* Mode 3 (linear followed by amoeba) is always used for seed points */
	status = do_correlation(left_line, left_samp, 3,
			TRUE, seed[2], seed[3], p);
 
	if (status == 1) {
	    snprintf(msg, msgLen, "Seed #%d already done", s);
	    zvmessage(msg, "");
	    continue;
	}
	if (status == -1) {
	    snprintf(msg, msgLen, "Correlation failure for seed #%d patch", s);
	    zvmessage(msg, "");
	    continue;
	} 
	if (status == -2) {
	    snprintf(msg, msgLen, "Correlation quality for seed #%d patch too low", s);
	    zvmessage(msg, "");
	    p->qual[left_line][left_samp] = EMPTY;	// try again later
	    continue;
	}

	good_seeds++;

	zvmessage("Seed point:", "");
	zvmessage("left l&s , initial right l&s, final right l&s, quality", ""); 
	snprintf(msg, msgLen, "%d %d %d %d %f %f %f", left_line, left_samp,
      		seed[2], seed[3],
      		p->line_mask[left_line][left_samp],
		p->samp_mask[left_line][left_samp],
		p->qual[left_line][left_samp]/QUAL_FACTOR);
	zvmessage(msg, "");

	/* set window */
	sl_window = left_line-1;
	el_window = left_line+1;
	ss_window = left_samp-1;
	es_window = left_samp+1;

	/*******************ACQUIRE TIEPOINTS ******************************/

	border_passes = 0;
	left_line = sl_window;
	left_samp = ss_window - 1;
 
	int done = FALSE;
	while (!done) {

	    done = get_next_point(left_line, left_samp, sl_window, el_window,
		ss_window, es_window, border_passes, p->nlin[0], p->nsin[0]);

	    if (!done) {

		status = do_correlation(left_line, left_samp, mode,
						FALSE, 0, 0, p);

		if (status == 1)
		    continue;			/* skip point */

		if (status < 0)	{ /* bad correlation, try again in gore code */
		    if (status == -2)
	    		p->qual[left_line][left_samp] = EMPTY;// try again later
		    continue;
		}

		if (p->npts >= limit_pts)
		    done = TRUE;		/* ran out of points */
	    }
	}

	/***********CLEAN UP MISSING POINTS IN ENTIRE IMAGE AREA************/

	zvmessage("Filling in gores", "");

	do {
	    count = 0;

	    for (j=sl_window; j < el_window; j++) {
		for (i=ss_window; i < es_window; i++) {

		    left_line = j;
		    left_samp = i;

		    status = do_correlation(left_line, left_samp, mode,
					FALSE, 0, 0, p);

		    if (status == 1)
			continue;
		    if (status < 0) {
			if (status == -2)	// failed due to quality setting
			    p->qual[left_line][left_samp] =
						- p->qual[left_line][left_samp];
			else			// failed for other reason
			    p->qual[left_line][left_samp] = FAILED;
			continue;
		    }

		   count += 1;
		}
	    }
	    snprintf(msg, msgLen, "gores found in last pass: %d", count);
	    zvmessage(msg, "");

	} while (count > 0);		/* find more missing points */

	snprintf(msg, msgLen, "%d tiepoints acquired", p->npts);
	zvmessage(msg, "");

    }			// end of seed point loop

    if (good_seeds == 0) {
	zvmessage("No valid seed points found!", "");
	zabend();
    }

    /****************WRITE DATA FILES************************************/

    int unit = 0, unit_out = 0;
    char filename[PIG_MAX_FILENAME_SIZE+1];
    PigMission *m = NULL;

    // Create PIG mission object based on the first input file
    status = zvpone("INP",filename,1,sizeof(filename));
    if (status) //success
        m = PigMission::getMissionObject(filename, &unit);
    if (m)  
        zvclose(unit,"CLOS_ACT","FREE",NULL);

    // write line offset

    band = 1;
    int num_out_files;
    zvpcnt("OUT", &num_out_files);
    int num_bands = 3 - num_out_files;		// 2 files == 1 band each

    zvunit(&unit_out, "OUT", 1, NULL);
    zvopen(unit_out, "OP", "WRITE", "U_FORMAT", "DOUB", "O_FORMAT", "REAL",
               "U_NS", p->nsin[0], "U_NL", p->nlin[0], "OPEN_ACT", "AS",
               "U_NB", num_bands, "U_ORG", "BSQ", NULL);
    zvplabel(unit_out, 0, 1);

    // Write output label
    zvpcnt("INP", &nids);
    PigFileModel *file_models[MAX_INPUTS];
    for (int i = 0; i < MAX_INPUTS; i++)
        file_models[i] = NULL;
    if (m)  {// if we were able to create Mission object
        for (int i = 0; i < nids; i++)
            file_models[i] = m->createFileModel("", unit_in[i]);

        PigLabelModel *labelModel_0 = NULL;
	labelModel_0 = m->createLabelModel(unit_out);

	if (labelModel_0) {
	    if (num_bands == 2)  // single two-banded image
	        labelModel_0->setDisparity(file_models, file_models[1], nids+1, 
                                           "DISPARITY_MAP");
	    else  // two 1-banded images
	       labelModel_0->setDisparity(file_models, file_models[1], nids+1, 
                                          "DISPARITY_LINE_MAP");
	}
    }

    for (j=0; j < p->nlin[0]; j++) {
	for (i=0; i < p->nsin[0]; i++) {
	    if (p->qual[j][i] < GOOD_CORR)
		p->line_mask[j][i] = 0.;
	    else
		p->line_mask[j][i] += 1.0;		// make coords 1-based
	}
	zvwrit(unit_out, p->line_mask[j], "LINE", j+1, "BAND", band, NULL);
    }

    /* write samp offset */

    if (num_out_files == 1) {		// 2 bands in one file
	band = 2;
    }
    else {				// one band in each of 2 files
	zvclose(unit_out, "CLOS_ACT", "FREE", NULL);
	zvunit(&unit_out, "OUT", 2, NULL);
	zvopen(unit_out, "OP", "WRITE", "U_FORMAT", "DOUB", "O_FORMAT", "REAL",
               "U_NS", p->nsin[0], "U_NL", p->nlin[0], "OPEN_ACT", "AS",
               "U_NB", num_bands, "U_ORG", "BSQ", NULL);
	zvplabel(unit_out, 0, 1);

	// Write output label for the second 1-banded file
	if (m) {
	    PigLabelModel *labelModel_1 = NULL;
	    labelModel_1 = m->createLabelModel(unit_out);
	    if (labelModel_1)
	        labelModel_1->setDisparity(file_models, file_models[1], nids+1, 
                                           "DISPARITY_SAMPLE_MAP");
	}
    }

    for (j=0; j < p->nlin[0]; j++) {
	for (i=0; i < p->nsin[0]; i++) {
	    if (p->qual[j][i] < GOOD_CORR)
		p->samp_mask[j][i] = 0.;
	    else
		p->samp_mask[j][i] += 1.0;		// make coords 1-based
	}
	zvwrit(unit_out, p->samp_mask[j], "LINE", j+1, "BAND", band, NULL);
    }
    zvclose(unit_out, "CLOS_ACT", "FREE", NULL);

    /* write mask */

    zvpcnt("MASK", &count);
    if (count > 0) {				// only if desired

	unsigned char bbuf[MAX_NS];
	char name[256];

	zvp("MASK", name, &count);
	zvunit(&unit_out, "MASK", 3, "U_NAME", name, NULL);
	zvopen(unit_out, "OP", "WRITE", "U_FORMAT", "BYTE", "O_FORMAT", "BYTE",
               "U_NS", p->nsin[0], "U_NL", p->nlin[0], "OPEN_ACT", "AS",
               "U_NB", 1, "U_ORG", "BSQ", NULL);
	zvplabel(unit_out, 0, 1);

	for (j=0; j < p->nlin[0]; j++) {
	    for (i=0; i < p->nsin[0]; i++) {
		if (p->qual[j][i] == EMPTY) bbuf[i]=0;		// unreachable
		else if (p->qual[j][i] < GOOD_CORR) bbuf[i]=255;// bad point
		else bbuf[i] = 128;				// good point
	    }
	    zvwrit(unit_out, bbuf, "LINE", j+1, NULL);
	}
	zvclose(unit_out, "CLOS_ACT", "FREE", NULL);
    }

    /* write quality image */

    zvpcnt("OUT_QUALITY", &count);
    if (count > 0) {				// only if desired

	float fbuf[MAX_NS];
	char name[256];

	zvp("OUT_QUALITY", name, &count);
	zvunit(&unit_out, "OUT_QUALITY", 1, "U_NAME", name, NULL);
	zvopen(unit_out, "OP", "WRITE", "U_FORMAT", "REAL", "O_FORMAT", "REAL",
               "U_NS", p->nsin[0], "U_NL", p->nlin[0], "OPEN_ACT", "AS",
               "U_NB", 1, "U_ORG", "BSQ", NULL);
	zvplabel(unit_out, 0, 1);

	for (j=0; j < p->nlin[0]; j++) {
	    for (i=0; i < p->nsin[0]; i++) {
		int q = p->qual[j][i];
		if (q == EMPTY)				// unreachable
		    fbuf[i] = 0.0;
		else if (q == FAILED)			// corr. failure
		    fbuf[i] = 0.0;
		else if (q < GOOD_CORR)			// didn't meet quality
		    fbuf[i] = - q / QUAL_FACTOR;
		else					// good point
		    fbuf[i] = q / QUAL_FACTOR;
	    }
	    zvwrit(unit_out, fbuf, "LINE", j+1, NULL);
	}
	zvclose(unit_out, "CLOS_ACT", "FREE", NULL);
    }
}


////////////////////////////////////////////////////////////////////////
// Find the neighboring point with the best quality.  Returns the
// quality of that maximum point, or -1 if no neighbors are valid.
// The line and samp of the max point are also returned in max_qual_line
// and max_qual_samp.
////////////////////////////////////////////////////////////////////////

static int get_best_neighbor(short int qual[MAX_NS][MAX_NS], int line, int samp,
	int &max_qual_line, int &max_qual_samp)
{

    int max_qual = -1;
    if (qual[line+1][samp] > max_qual) {
	max_qual_line = line+1;
	max_qual_samp = samp;
	max_qual = qual[max_qual_line][max_qual_samp];
    }
    if (qual[line-1][samp] > max_qual) {
	max_qual_line = line-1;
	max_qual_samp = samp;
	max_qual = qual[max_qual_line][max_qual_samp];
    }
    if (qual[line][samp-1] > max_qual) {
	max_qual_line = line;
	max_qual_samp = samp-1;
	max_qual = qual[max_qual_line][max_qual_samp];
    }
    if (qual[line][samp+1] > max_qual) {
	max_qual_line = line;
	max_qual_samp = samp+1;
	max_qual = qual[max_qual_line][max_qual_samp];
    }
    if (qual[line+1][samp+1] > max_qual) {
	max_qual_line = line+1;
	max_qual_samp = samp+1;
	max_qual = qual[max_qual_line][max_qual_samp];
    }
    if (qual[line-1][samp+1] > max_qual) {
	max_qual_line = line-1;
	max_qual_samp = samp+1;
	max_qual = qual[max_qual_line][max_qual_samp];
    }
    if (qual[line+1][samp-1] > max_qual) {
	max_qual_line = line+1;
	max_qual_samp = samp-1;
	max_qual = qual[max_qual_line][max_qual_samp];
    }
    if (qual[line-1][samp-1] > max_qual) {
	max_qual_line = line-1;
	max_qual_samp = samp+1;
	max_qual = qual[max_qual_line][max_qual_samp];
    }

    return max_qual;
}


////////////////////////////////////////////////////////////////////////
// Do a correlation for a single point.  This includes finding the best
// neighbor as a starting point, and updating the mask and quality
// arrays upon success.  Certain things are slightly different if we're
// correlating a seed point (in which case seed_line and seed_samp are
// the initial values for the right line/samp... if seed is false, these
// parameters are ignored).
//
// Return values:
// -2 = correlation quality below minimum
// -1 = bad correlation
//  0 = success
//  1 = point skipped (already done or no neighbors)
////////////////////////////////////////////////////////////////////////

int do_correlation(int left_line, int left_samp, int mode,
		int seed, int seed_line, int seed_samp,
		CorrelParams *p)
{
    int max_qual = 0, max_qual_line = 0, max_qual_samp = 0;
    int right_line = 0, right_samp = 0;
    double right_liner = 0.0, right_sampr = 0.0;
    int start_line = 0, start_samp = 0;
    double *left = NULL, *right = NULL;
    int nlw_right = p->nlw2;
    int nsw_right = p->nsw2;
	const size_t msgLen = 256;
    char msg[msgLen];

    // Params for gruen

    double line_offset = 0.0, samp_offset = 0.0;
    double line_coef[3], samp_coef[3];
    double quality;
    double percent = 100.;
    int limits = 10000;

    // the rest of the gruen params are not used
    static double correl[MAX_NS][MAX_NS];
    double line_coef_limits[3][2], samp_coef_limits[3][2];
    double line_temp[3], samp_temp[3];


    // Skip this point if it's already been done

    if (p->qual[left_line][left_samp] != EMPTY)
	return 1;

    if (seed) {				// seed points skip neighbor checks
	right_line = seed_line;
	right_samp = seed_samp;

	// Set the window size to be the max.  If we're too close to the
	// edge, adjust it down to the given search space.

	nlw_right = p->max_right_area;
	if (right_line <= nlw_right/2 + 1)
	    nlw_right = right_line*2 - 1;
	if (right_line >= p->nlin[1] - nlw_right/2 - 1)
	    nlw_right = (p->nlin[1] - right_line) * 2 - 1;
	if (nlw_right < p->nlw2)
	    nlw_right = p->nlw2;

	nsw_right = p->max_right_area;
	if (right_samp <= nsw_right/2 + 1)
	    nsw_right = right_samp*2 - 1;
	if (right_samp >= p->nsin[1] - nsw_right/2 - 1)
	    nsw_right = (p->nsin[1] - right_samp) * 2 - 1;
	if (nsw_right < p->nsw2)
	    nsw_right = p->nsw2;

    }
    else {

	// Find the neighbor with the best correlation to use as starting point

	max_qual = get_best_neighbor(p->qual, left_line, left_samp,
		max_qual_line, max_qual_samp);

	if (max_qual == -1)
	    return 1;			// no neighbors, skip it

	right_liner = p->line_mask[max_qual_line][max_qual_samp];
	right_sampr = p->samp_mask[max_qual_line][max_qual_samp];

	right_line = (int)(right_liner - max_qual_line + left_line + 0.5);
	right_samp = (int)(right_sampr - max_qual_samp + left_samp + 0.5);
    }

    // Prepare to correlate the point

    start_samp = left_samp - p->nsw/2;
    start_line = left_line - p->nlw/2;
    if (start_samp <= 0) return -1;
    if (start_line <= 0) return -1;
    if (start_samp + p->nsw - 1 >= p->nsin[0]) return -1;
    if (start_line + p->nlw - 1 >= p->nlin[0]) return -1;
    left = &p->left_dn[start_line][start_samp];

    start_samp = right_samp - nsw_right/2;
    start_line = right_line - nlw_right/2;
    if (start_samp <= 0) return -1;
    if (start_line <= 0) return -1;
    if (start_samp + nsw_right - 1 >= p->nsin[1]) return -1;
    if (start_line + nlw_right - 1 >= p->nlin[1]) return -1;
    right = &p->right_dn[start_line][start_samp];

    /* set initial coefficient values */
    line_coef[0]=0.;
    line_coef[1]=1.;
    if (seed)
	line_coef[2] = 0.;
    else
	line_coef[2]=right_liner - max_qual_line + left_line - right_line;
    samp_coef[0]=1.;
    samp_coef[1]=0.;
    if (seed)
	samp_coef[2] = 0.;
    else
	samp_coef[2]=right_sampr - max_qual_samp + left_samp - right_samp;
 
    /* correct candidate tiepoints by correlation */
    int status = gruen(left, p->nlw, p->nsw, MAX_NS,
		right, nlw_right, nsw_right, MAX_NS,
		&correl[0][0], &line_offset, &samp_offset,
		line_coef, samp_coef, line_coef_limits, samp_coef_limits,
		line_temp, samp_temp, percent, limits, &quality, mode);

    if (status > 0)
	return -1;

    p->qual[left_line][left_samp] = (short int)(quality*QUAL_FACTOR);

    if (quality < p->min_quality)
	return -2;

    if (p->npts < 10) {
	snprintf(msg, msgLen, "%d %d %d %d %f %f %f", left_line, left_samp,
		right_line, right_samp,
		right_line+line_offset, right_samp+samp_offset, quality);
	zvmessage(msg, "");
    }

    /* compute right image coordinates from offsets */

    p->line_mask[left_line][left_samp] = right_line + line_offset;
    p->samp_mask[left_line][left_samp] = right_samp + samp_offset;
    p->npts += 1;

    if ((p->npts % p->print_interval) == 0) {
	snprintf(msg, msgLen, "%d tiepoints gathered so far", p->npts);
	zvmessage(msg, "");
    }

    return 0;
}

////////////////////////////////////////////////////////////////////////
// Determine the next point to try.  Points are tried at the edge of an
// ever-expanding window around the seed.  First, the edges are tried
// top-to-bottom, then bottom-to-top, then the window is expanded by 1
// in all directions and the process is repeated, until we run out of image.
//
// The line and samp parameters are modified to hold the new location.
// In addition, the window parameters may also be updated, if the window
// was expanded.  Border_passes should be set to 0 before the initial call
// and left alone after that; it is an internal flag which is modified.
// The function returns TRUE when we run out of points (i.e. the window
// reaches the edge of the image).  FALSE means the point is good.
//
// Initialization before calling this the first time:
//     border_passes = 0;
//     line = sl_window;
//     samp = ss_window - 1;
////////////////////////////////////////////////////////////////////////

int get_next_point(int &line, int &samp,
		int &sl_window, int &el_window, int &ss_window, int &es_window,
		int &border_passes, int nl_image, int ns_image)
{

    /* move from upper left to lower right */

    if (border_passes == 0) {
	if (line == sl_window) {		/* top line of window */
	    if (samp < es_window)
		samp += 1;
	    else {
		line += 1;
		samp = ss_window;
	    }
	}
	else if (line < el_window) {		/* central part of window */
	    if (samp == ss_window)
		samp = es_window;
	    else {
		line += 1;
		samp = ss_window;
	    }
	}
        else {					/* last line of window */
	    if (samp < es_window)
		samp += 1;
	    else {
		border_passes = 1;		/* now reverse direction */
		samp += 1;
		/* This falls through into the border_passes==1 test below. */
	    }
	}
    }

    /* move from lower right to upper left */

    if (border_passes == 1) {
	if (line == sl_window) {		/* top line of window */
	    if (samp > ss_window)
		samp -= 1;
	    else {
		if (ss_window > 1)
		    ss_window -= 1;		/* expand window by 1 */
		if (sl_window > 1)
		    sl_window -= 1;
		if (el_window < nl_image-2)
		    el_window += 1; 
		if (es_window < ns_image-2)
		    es_window += 1;
		if ((ss_window == 1) && (es_window == ns_image-2) &&
		    (sl_window == 1) && (el_window == nl_image-2)) 

		    return TRUE;		/* Nothing more to do */

		/* Image not done, so start over with the new window size */
		/* Note that we set samp to ss_window, NOT ss_window-1 as */
		/* the initialization instructions say, because we are    */
		/* including in this the first iteration of the           */
		/* border_passes==0 case above, in order to avoid a loop. */

		border_passes = 0;
		line = sl_window;
		samp = ss_window;
	    }
	}
	else if (line < el_window) {	/* central part of window */
	    if (samp == es_window)
		samp = ss_window;
	    else {
		line -= 1;
		samp = es_window;
	    }
	}
        else {					/* last line of window */
	    if (samp > ss_window)
		samp -= 1;
	    else {
		line -= 1;
		samp = es_window;
	    }
	}
    }

    return FALSE;				/* not done yet */
}

