////////////////////////////////////////////////////////////////////////
// marsautotie - multimission tiepoint gathering program: automated
////////////////////////////////////////////////////////////////////////
//
// TO DO LIST:
// * Use moving box for busyness calculation for efficiency.
// * Consider using Gary Yagi's horizon mask routines.
// * More parameter tuning!!
// * Fix rover filter to compensate for CAHVOR bug (see method comments).
// * Use PIG to find filter file in the config dir automatically.
// * Compute appropriate SEP_ANGLE based on camera FOV.
// * Once parameter sets stabilize, perhaps have "sets" you can choose from
//   for different cameras (rather than setting dozens of params).
// * More adaptable algorithms for parameters... for example, increasing
//   window size when there's not enough information to correlate properly.
// * Do something more to avoid line problems (e.g. horizon, shadows) where
//   the correlation is unconstrained in one direction.
// * Add some "slop" for on-image checks when projecting to the right side.
// * Determine if the "correlate again using min search window" step is really
//   useful any more, now that the search window shaping and adjustment is done.
// * ...


#include "vicmain_c"

#include "mars_support.h"
#include "mars_tiepoints.h"

#include "gruen.h"

#include "PigMission.h"
#include "PigFileModel.h"
#include "PigCameraModel.h"
#include "PigPointingModel.h"
#include "PigSurfaceModel.h"
#include "PigVector.h"
#include "RadiometryModel.h"
#include "PigCoordSystem.h"
#include "FilterShapes.h"

#include "SimpleImage.h"

#include <string.h>

#ifdef _OPENMP
#include <omp.h>
#endif

/* buffer sizes in main program */
#define MAX_INPUTS 2000
#define MAX_NS MARS_MAX_NS
#define MAX_NL MARS_MAX_NL

#define MAX_GRID_X 300
#define MAX_GRID_Y 300

#define MAX_THREADS 256



// 0 == no debug
// 1 == info about window sizes, culling
// 2 == 1 + info about tiepoints and param filtering
// 3 == 2 + saving intermediate (temp) images (don't do with more than one
//						or two pairs!)

int debug = 0;

// Expanded version of TiePoint that includes extra info for filtering, etc.

struct TiePointPlus {
  struct TiePoint tie;

  double rot_angle;           // rotation of rt rel to left, rad, +=countercl
  double scale_factor;        // scale of rt rel to left, rt=left*scale
  double coefs[8];		// coefficients from amoeba correlation
				// line->0..3, samp->4..7
};



void generate_tiepoints(TiePoint *&tiepoints, int &n_tiepoints,
			int nids,
			PigPointingModel *pointing_in[], PigCameraModel *camera_in[],
			PigSurfaceModel *surface,
			PigFileModel *file_models[], int band,
			int tmpl_area[2], int search_area[2], int busy_area[2],
			double min_quality, double linear_quality, PigCoordSystem *cs,
			int refimg[]);
int read_area(double *area, int line, int samp,
	      int array_height, int array_width,
	      PigFileModel *file_model, int band, int height, int width);
int rotate_area(SimpleImage<double> &image,
		SimpleImage<double> &out_area,
                int line, int samp,
		int window_height, int window_width,
		double rotation, double scale);

int get_tiepoint_grid(
		      TiePoint grid[][MAX_GRID_X],
		      int &n_grid_x,
		      int &n_grid_y,
		      int grid_spacing,
		      int nsw,
		      int nlw,
		      int border,
		      PigFileModel *file_model);

int project_tiepoints(
		      TiePoint *left_tiepoints,
		      int n_left_tpts,
		      TiePointPlus *tiepoints,
		      int &n_tpts,
		      PigCameraModel *camera_left,
		      PigCameraModel *camera_right,
		      PigCoordSystem *cs,
		      PigSurfaceModel *surface,
		      PigFileModel *left_file,
		      PigFileModel *right_file,
		      int left_img, int right_img,
		      PigPointingModel *point_right,
		      double fov_override);

int project_point(
		  TiePointPlus *point,
		  PigCameraModel *camera_left,
		  PigCameraModel *camera_right,
		  PigCoordSystem *cs,
		  PigSurfaceModel *surface,
		  PigFileModel *left_model,
		  PigFileModel *right_model,
		  double FOV,
		  PigPoint pos, PigVector orient);

void plot_ties(SimpleImage<double> &image, TiePoint *ties, int n_ties,
	       int which);
void plot_ties(SimpleImage<double> &image, TiePointPlus *ties, int n_ties,
	       int which, int rotate);

int read_image(PigFileModel *file_model,
	       SimpleImage<double> &image,
	       int band);

int compute_busyness(SimpleImage<double> &image,
		     SimpleImage<double> &busyness_image, int nlw, int nsw);

int get_busyest_from_grid(
			  TiePoint grid[][MAX_GRID_X],
			  int n_grid_x,
			  int n_grid_y,
			  TiePoint **tptlist,
			  int &n_tpts,
			  SimpleImage<double> &busyness_image,
			  double busy_thresh);

void save_temp_image(SimpleImage<double> &image, int n);

int correlate_candidates(
			 SimpleImage<double> &left_image,
			 SimpleImage<double> &right_image,
			 TiePointPlus *tptlist,
			 int n_tpts,
			 int nlw, int nsw,		// template size
			 int nlw_rt, int nsw_rt,		// search size
			 double min_quality,
			 double linear_quality,
			 float search_edge,
			 float search_factor,
			 int search_min[2],
			 int search_max[2],
			 int omp_on,
			 SimpleImage<double> *left_template[],	// per-thread temps
			 SimpleImage<double> *correl_img[]);

int filter_corr_params(TiePointPlus *tptlist, int n_tpts,
		       int param_filt_w, int param_filt_h,
		       double param_filt_sigma[],
		       double param_filt_sigma_min[],
		       double param_filt_abs_limit[]);

int geometric_scatter(TiePointPlus *tptlist, int n_tpts,
		      int *&candidates, int &n_candidates,
		      int density, int max_iterations,
		      int nl, int ns, float center_weight);

int filter_points_using_mask(TiePoint *tptlist, int n_tpts,
			     PigFileModel *file_model, PigCameraModel *camera_model,
			     PigCoordSystem *cs, char *mask_filename,
			     PigSurfaceModel *surface);

////////////////////////////////////////////////////////////////////////
// MarsTIE program
////////////////////////////////////////////////////////////////////////

void main44()
{

  int i, status;
  int count, def;
  const int BUF_SIZE = 150;
  char msg[BUF_SIZE];

  // Inputs

  int nids;
  PigFileModel *file_models[MAX_INPUTS];
  PigCameraModel *camera_in[MAX_INPUTS];
  PigPointingModel *pointing_in[MAX_INPUTS];
  PigSurfaceModel *surface_model;
  int homogeneous_inputs = TRUE;
  PigCoordSystem *cs, *dummy_cs;
  int refimg[MAX_INPUTS];

  // Tiepoints

  struct TiePoint *tiepoints;
  int n_tiepoints;

  // User Parameters

  char outfilename[150];
  int band;
  char mission[64], instrument[64];
  int tmpl_area[2], search_area[2], busy_area[2];
  double min_quality, linear_quality;

  zvmessage("MARSAUTOTIE version 2020-02-18", "");

  // Note that the cs returned here is NOT used.

  mars_setup(nids, file_models, camera_in, pointing_in, NULL, dummy_cs,
	     mission, instrument, homogeneous_inputs,
	     MAX_NL, MAX_NS, MAX_INPUTS);

  // get output parameter file name

  zvp("OUT", outfilename, &count);

  // get parameter overrides if any

  zvp("DEBUG", &debug, &count);

  zvp("BAND", &band, &count);

  zvp("TEMPLATE", &tmpl_area, &count);
  if (count == 1)
    tmpl_area[1] = tmpl_area[0];
  zvp("SEARCH", &search_area, &count);
  if (count == 1)
    search_area[1] = search_area[0];
  // Check that Search is larger than Template
  if (search_area[0] < tmpl_area[0] || search_area[1] < tmpl_area[1]) {
     zvmessage("TEMPLATE cannot be larger than SEARCH","");
     zabend();
  }


  zvp("BUSY_WINDOW", &busy_area, &count);
  if (count == 1)
    busy_area[1] = busy_area[0];

  zvparmd("QUALITY", &min_quality, &count, &def, 1, 0);
  zvparmd("LINEAR_QUALITY", &linear_quality, &count, &def, 1, 0);

  // Set up "reference" images (those we don't bother tiepointing)

  for (i=0; i<nids; i++)
    refimg[i] = FALSE;

  int refimg_array[MAX_INPUTS];
  status = zvparm("REFIMAGE", refimg_array, &count, &def, MAX_INPUTS, 0);
  if (status == 1 && count > 0) {
    for (i=0; i < count; i++) {
      if (refimg_array[i] == -1)
	continue;			// set nothing in the array
      if (refimg_array[i] > nids || refimg_array[i] < -nids) {
	zvmessage("REFIMAGE value bigger than # of inputs!", "");
	zabend();
      }
      if (refimg_array[i] <= 0) {
	if (i < 1 || refimg_array[i-1] <= 0 ||
	    refimg_array[i-1] >= -refimg_array[i]) {
	  zvmessage("REFIMAGE value less than 0 and prior ref not correct (neg or > this)", "");
	  zabend();
	}
	for (int j = refimg_array[i-1]; j <= -refimg_array[i]; j++) {
	  refimg[j-1] = TRUE;
	}
      }
      else {
	refimg[refimg_array[i]-1] = TRUE;
      }
    }
  }

  // Create surface model based on the first input.  Looks at user
  // parameters.

  surface_model = PigSurfaceModel::create(file_models[0]);

  cs = surface_model->getCoordSystem();

  // Print out input status from labels

  mars_print_inputs(nids, pointing_in, camera_in, file_models,
		    homogeneous_inputs, mission, instrument);

  if (nids == 1) {
    zvmessage("Only one input file; nothing to do!", "");
    return;
  }

  // Generate the tiepoints!

  generate_tiepoints(tiepoints, n_tiepoints, nids, pointing_in, camera_in,
		     surface_model, file_models, band,
		     tmpl_area, search_area, busy_area,
		     min_quality, linear_quality, cs, refimg);



  // save tiepoints

  int start_key;
  zvp("START_KEY", &start_key, &count);

  if (zvptst("OLD"))
    status = mars_save_tiepoints(outfilename, tiepoints, n_tiepoints);
  else
    status = mars_save_tiepoints(outfilename, tiepoints, n_tiepoints,
				 file_models, nids, start_key, cs);
  if (status != 0) {
    snprintf(msg, BUF_SIZE, "Error saving tiepoints!! code=%d", status);
    zvmessage(msg, "");
  }

  // print out tiepoints

  zvmessage("Final tiepoint list (Camera coordinates)", "");
  mars_print_tiepoint_list(tiepoints, n_tiepoints);


  // determine which pictures are represented (when present[i] is 1)

  int present[MAX_INPUTS];

  for (i=0; i < nids; i++)
    present[i] = FALSE;
  for (i=0; i < n_tiepoints; i++) {
    present[tiepoints[i].left_image] = TRUE;
    present[tiepoints[i].right_image] = TRUE;
  }

  for (i=0; i < nids; i++) {
    if (!present[i]) {
      snprintf(msg, BUF_SIZE, "Input image %d not represented by tiepoints", i+1);
      zvmessage(msg, "");
    }
  }

}

////////////////////////////////////////////////////////////////////////
// Generate a set of tiepoints to use, if we didn't read them from a file.
// Note that the tiepoint list is allocated here.
////////////////////////////////////////////////////////////////////////

void generate_tiepoints(TiePoint *&tiepoints, int &n_tiepoints, int nids,
			PigPointingModel *pointing_in[], PigCameraModel *camera_in[],
			PigSurfaceModel *surface,
			PigFileModel *file_models[], int band,
			int tmpl_area[2], int search_area[2], int busy_area[2],
			double min_quality, double linear_quality, PigCoordSystem *cs,
			int refimg[])
{

  int i, j, k;
  const int BUF_SIZE = 255;
  char msg[BUF_SIZE];

  int nlw, nsw, nlw2, nsw2, border;
  int count, def;
  float busy_threshold;
  int grid_spacing;
  float sep_angle;
  double sep_cos;
  float search_edge, search_factor;
  int search_min[2], search_max[2], param_filt_win[2];
  int density;
  int max_iterations;
  float center_weight;
  double param_filt_sigma[8];
  double param_filt_sigma_min[8];
  double param_filt_abs_limit[8];
  double fov_override;
  int nominal_height;		// used to change params based on image size

  SimpleImage<double> left_image;
  SimpleImage<double> busyness_image;
  SimpleImage<double> right_image;

  TiePointPlus *tptlist = NULL;	// pairwise tiepoints
  int n_tpts_alloc = 0;
  int n_tpts = 0;

  tiepoints = NULL;			// overall tiepoints
  int n_tiepoints_alloc = 0;
  n_tiepoints = 0;

  // compute max length/width of correlations

  if (tmpl_area[0] % 2 == 0)
    tmpl_area[0] += 1;				// make sure it's odd
  if (tmpl_area[1] % 2 == 0)
    tmpl_area[1] += 1;				// make sure it's odd
  if (search_area[0] % 2 == 0)
    search_area[0] += 1;				// make sure it's odd
  if (search_area[1] % 2 == 0)
    search_area[1] += 1;				// make sure it's odd

  nlw = tmpl_area[0];
  nsw = tmpl_area[1];
  nlw2 = search_area[0];
  nsw2 = search_area[1];

  zvp("BORDER", &border, &count);
  zvp("BUSY", &busy_threshold, &count);
  zvp("GRID_SPACING", &grid_spacing, &count);
  zvp("SEP_ANGLE", &sep_angle, &count);	// max sep angle for frames
  sep_cos = cos(PigDeg2Rad(sep_angle));
  zvp("SEARCH_EDGE", &search_edge, &count);
  zvp("SEARCH_FACTOR", &search_factor, &count);
  zvp("SEARCH_MIN", search_min, &count);
  if (count == 1)
    search_min[1] = search_min[0];
  zvp("SEARCH_MAX", search_max, &count);
  if (count == 1)
    search_max[1] = search_max[0];
  // Check that Search is larger than Template
  if (search_min[0] < tmpl_area[0] || search_min[1] < tmpl_area[1]) {
     zvmessage("TEMPLATE cannot be larger than SEARCH_MIN","");
     zabend();
  }
  zvp("PARAM_WIN", param_filt_win, &count);
  if (count == 1)
    param_filt_win[1] = param_filt_win[0];
  zvp("DENSITY", &density, &count);
  zvp("MAX_ITER", &max_iterations, &count);
  zvp("CENTER_WEIGHT", &center_weight, &count);

  zvparmd("PARAM_SIGMA", param_filt_sigma, &count, &def, 8, 0);
  zvparmd("PARAM_MIN", param_filt_sigma_min, &count, &def, 8, 0);
  zvparmd("PARAM_ABS", param_filt_abs_limit, &count, &def, 8, 0);

  zvparmd("FOV", &fov_override, &count, &def, 1, 0);
  if (count == 0)
      fov_override = 0.0;

  zvp("NOMINAL_HEIGHT", &nominal_height, &count);

  int use_mask = zvptst("DO_MASK");
  char mask_fn[PIG_MAX_FILENAME_SIZE];
  char *mask_filename = NULL;

  if (use_mask) {
    zvp("MASK", mask_fn, &count);
    if (count == 0)
      mask_filename = NULL;
    else
      mask_filename = mask_fn;
  }

  int omp_on = zvptst("OMP_ON");
  int num_thr = 1;
#ifdef _OPENMP
  if (omp_on)
    num_thr = omp_get_max_threads();
#endif
  if (num_thr > MAX_THREADS) {
    snprintf(msg, BUF_SIZE, "Number of threads %d exceeds the maximum %d.  Reduce threads or increase limit.", num_thr, MAX_THREADS);
    zvmessage(msg, "");
    zabend();
  }

  // Create pool of left_template and correl_img images (one per thread)
  // so we don't have to continually re-allocate them.  Each thread does
  // need a separate one.

  SimpleImage<double> *left_template[MAX_THREADS];
  SimpleImage<double> *correl_img[MAX_THREADS];

  for (int i=0; i < num_thr; i++) {
    left_template[i] = new SimpleImage<double>(nlw, nsw);
    if (left_template[i] == NULL) {
      snprintf(msg, BUF_SIZE, "Memory allocation error in left_template");
      zvmessage(msg, "");
      zabend();
    }
    // It's nlw2 for any given iteration, but window shaping says it
    // could go as high as search_max.  So use that for sizing.
    correl_img[i] = new SimpleImage<double>(search_max[0]-nlw+1,
					    search_max[1]-nsw+1);
    if (correl_img[i] == NULL) {
      snprintf(msg, BUF_SIZE, "Memory allocation error in correl_img");
      zvmessage(msg, "");
      zabend();
    }
  }

  // ref or left picture loop

  for (i=0; i < nids-1; i++) {

    snprintf(msg, BUF_SIZE, "reading left image %d", i+1);
    zvmessage(msg, "");

    read_image(file_models[i], left_image, band);

    zvmessage("computing busyness", "");

    compute_busyness(left_image, busyness_image, busy_area[0],busy_area[1]);
    if (debug >= 3) {
      save_temp_image(busyness_image, 0);
    }

    // Get candidate tiepoint grid

    zvmessage("getting tiepoint grid", "");

    TiePoint (*grid)[MAX_GRID_X];
    grid = new TiePoint[MAX_GRID_Y][MAX_GRID_X];
    if (grid == NULL) {
      zvmessage("error allocating tiepoint grid!","");
      zabend();
    }
    int n_grid_x, n_grid_y;

    int gsp = grid_spacing;
    if (nominal_height != 0)
      gsp = grid_spacing * file_models[i]->getNL() / nominal_height;
    get_tiepoint_grid(grid, n_grid_x, n_grid_y, gsp, nsw, nlw,
		      border, file_models[i]);

    // Use busyness to get most interesting point in each box

    zvmessage("getting busyest points", "");
    TiePoint *left_points = NULL;
    int n_left_tpts = 0;

    get_busyest_from_grid(grid, n_grid_x, n_grid_y,
			  &left_points, n_left_tpts,
			  busyness_image, busy_threshold);

    if (debug >= 3) {
      plot_ties(left_image, left_points, n_left_tpts, 0);
    }

    // Filter out points which are on the rover mask.

    if (use_mask) {
      zvmessage("Filtering using mask...", "");
      filter_points_using_mask(left_points, n_left_tpts,
			       file_models[i], camera_in[i], cs, mask_filename,
			       surface);
      if (debug >= 3) {
	plot_ties(left_image, left_points, n_left_tpts, 0);
      }
    }

    // Allocate the pairwise working array (tptlist) if needed.
    // Can't have more pairwise tiepoints than the # of left-side
    // candidates... but alloc it at 2x if needed to allow for expansion.

    if (tptlist == NULL || n_tpts_alloc < n_left_tpts) {
      if (tptlist != NULL)
	delete tptlist;
      tptlist = new TiePointPlus[n_left_tpts*2];
      n_tpts_alloc = n_left_tpts*2;
    }
    n_tpts = 0;

    // Right picture loop.  Go through each one looking for overlaps.
    // Don't bother looking at images numbered lower than this one; we
    // should have picked all those up already.

    zvmessage("entering right loop", "");
    for (j=i+1; j < nids; j++) {		// right picture loop

      if (refimg[i] && refimg[j]) {
	snprintf(msg, BUF_SIZE, "Skipping %d,%d: both are ref images", i+1,j+1);
	zvmessage(msg, "");
	continue;
      }

      n_tpts = 0;

      // If angle between the images is too great, skip this pair

      if (debug >= 1) {
	printf("Sep angle %d,%d = %f, %f degrees, sep=%f, %f degrees\n",
	       i+1, j+1, (pointing_in[i]->getCameraOrientation(cs) %
			  pointing_in[j]->getCameraOrientation(cs)),
	       PigRad2Deg(acos((pointing_in[i]->getCameraOrientation(cs) %
				pointing_in[j]->getCameraOrientation(cs)))),
	       sep_cos, PigRad2Deg(acos(sep_cos)));
      }
      if ((pointing_in[i]->getCameraOrientation(cs) %
	   pointing_in[j]->getCameraOrientation(cs)) < sep_cos)
	continue;

      // Project all the left image candidate tiepoints into the
      // right image.  tptlist contains the candidates for this pair.

      snprintf(msg, BUF_SIZE, "PROJECTING FROM %d to %d", i+1, j+1);
      zvmessage(msg, "");

      project_tiepoints(left_points, n_left_tpts, tptlist, n_tpts,
			camera_in[i], camera_in[j], cs, surface,
			file_models[i], file_models[j], i, j,
			pointing_in[j], fov_override);


      if (n_tpts == 0)		// no overlap, so bail out
	continue;

      // left-side, before correlation, with rot
      if (debug >= 3) {
	plot_ties(left_image, tptlist, n_tpts, 0, TRUE);
      }
      // Read in right image

      zvmessage("reading right image", "");
      read_image(file_models[j], right_image, band);

      // left and right-side projected, before corr
      if (debug >= 3) {
	plot_ties(left_image, tptlist, n_tpts, 0, FALSE);
	plot_ties(right_image, tptlist, n_tpts, 1, FALSE);
      }

      // Correlate all the candidates

      int nn = 0;
      int kk;
      for (kk=0; kk<n_tpts; kk++) {
	if (tptlist[kk].tie.active)
	  nn++;
      }
      snprintf(msg, BUF_SIZE, "correlating %d candidates", nn);
      zvmessage(msg, "");
      correlate_candidates(left_image,
			   right_image,
			   tptlist, n_tpts,
			   nlw, nsw,
			   nlw2, nsw2,
			   min_quality, linear_quality,
			   search_edge, search_factor,
			   search_min, search_max,
			   omp_on, left_template, correl_img);

      // If we got nothing, could be the search window is too big for
      // the overlap.  While that's necessary to deal with big offsets,
      // the overlap has to be at least that big to work.  So, if we
      // got nothing, try again with the minimum size for the search
      // window.

      nn=0;
      for (kk=0; kk<n_tpts; kk++) {
	if (tptlist[kk].tie.active)
	  nn++;
      }
      if (nn == 0) {
	zvmessage("No candidates, try again with min seach window", "");
	n_tpts = 0;
	project_tiepoints(left_points, n_left_tpts, tptlist, n_tpts,
			  camera_in[i], camera_in[j], cs, surface,
			  file_models[i], file_models[j], i, j,
			  pointing_in[j], fov_override);

	correlate_candidates(left_image,
			     right_image,
			     tptlist, n_tpts,
			     nlw, nsw,
			     search_min[0], search_min[1],
			     min_quality, linear_quality,
			     search_edge, search_factor,
			     search_min, search_max,
			     omp_on, left_template, correl_img);
      }

      // left and right sides, after corr
      if (debug >= 3) {
	plot_ties(left_image, tptlist, n_tpts, 0, FALSE);
	plot_ties(right_image, tptlist, n_tpts, 2, FALSE);
      }

      //!!!!
      // Assumptions:  Rotation difference no more than 20 deg from
      // mean, scale no more than 10% from mean.  For l0 and s1
      // (sin terms) the abs error is thus sin(20)/0.9 = 0.38.  For
      // l1 and s0 (cos terms) it is 1.0-(cos(20)*0.9) = 0.15.
      // Offsets are much more generous; 5 lines off mean and 50 samples.
      // Nominal rotation has been removed but not rot difference so
      // that becomes 5/sin(20)=14 lines and 50/cos(20)=53 samps.
      // Trapezoid terms are empirically limited to 0.05.
      //
      // Sigma values are rather arbitrarily set to 1.0.  And the sigma
      // min values are (again, rather arbitrarily) set to 1/5 of the
      // abs_limit values... except for the shift terms, which are set
      // to 1 pixel.

      // Pancam liked this set:
      //	    double param_filt_sigma[8] =     {1.0, 1.0, 1.2, 1.0,
      //					     1.0, 1.0, 1.2, 1.0 };
      // Navcam liked this set:
      //	    double param_filt_sigma[8] =     {1.5, 1.5, 1.2, 1.0,
      //					     1.5, 1.5, 1.2, 1.0 };
      //	    double param_filt_sigma_min[8] = {.076, .030, 1.0, .01,
      //					     .030, .076, 1.0, .01 };
      //old	    double param_filt_sigma_min[8] = {.0038, .0015, 1.0, .00025,
      //					     .0015, .0038, 1.0, .00025 };
      //	    double param_filt_abs_limit[8] = {0.38, 0.15, 14, 0.05,
      //					     0.15, 0.38, 53, 0.05 };
      //!!!!

      // Count remaining candidates
      nn = 0;
      for (kk=0; kk<n_tpts; kk++) {
	if (tptlist[kk].tie.active)
	  nn++;
      }
      snprintf(msg, BUF_SIZE, "filtering %d candidates", nn);
      zvmessage(msg, "");

      filter_corr_params(tptlist, n_tpts,
			 param_filt_win[1], param_filt_win[0],
			 param_filt_sigma,
			 param_filt_sigma_min,
			 param_filt_abs_limit);

      if (debug >= 3) {
	plot_ties(left_image, tptlist, n_tpts, 0, FALSE);
	plot_ties(right_image, tptlist, n_tpts, 2, FALSE);
      }

      int *candidates;		// array of indices into tptlist
      int n_candidates;

      int den = density;
      if (nominal_height != 0)
	den = density * file_models[i]->getNL() / nominal_height;
      geometric_scatter(tptlist, n_tpts,
			candidates, n_candidates, den, max_iterations,
			left_image.getNL(), left_image.getNS(), center_weight);

      if (debug >= 3) {
	//!!!! TEMP ONLY FOR PLOTTING
	TiePointPlus tptc[100];					//!!!!
	for (int iii=0; iii<n_candidates; iii++)		//!!!!
	  tptc[iii] = tptlist[candidates[iii]];		//!!!!
	plot_ties(left_image, tptc, n_candidates, 0, FALSE);	//!!!!
	plot_ties(right_image, tptc, n_candidates, 2, FALSE);	//!!!!
      }

      // Copy the remaining tiepoints over to the main array to save them

      // Count the tiepoints

      int n = n_candidates;

      // Re-alloc the tiepoint array if necessary.  Make it big enough
      // that we shouldn't have to do this again.

      if (tiepoints == NULL || n_tiepoints + n > n_tiepoints_alloc) {
	if (debug >= 1)
	  printf("re-alloc tiepoints array\n");
	int nleft = nids - i;	// remaining images
	// Assume 4 tpts per overlap min (in case this is a corner).
	// Most images have 4 overlaps plus corners (thus *5)
	int nn = n_tiepoints_alloc + (nleft*5 * (n<4 ? 4 : n));
	TiePoint *new_tiepoints = new TiePoint[nn];
	if (tiepoints != NULL) {
	  for (k=0; k < n_tiepoints_alloc; k++) {
	    new_tiepoints[k] = tiepoints[k];
	  }
	  delete tiepoints;
	}
	tiepoints = new_tiepoints;
	n_tiepoints_alloc = nn;
      }

      // Copy the new tiepoints into the main list

      int total_n = 0;
      for (k=0; k < n_candidates; k++) {
	if (tptlist[candidates[k]].tie.active) {
	  tiepoints[n_tiepoints++] = tptlist[candidates[k]].tie;
	  total_n++;
	}
      }
      snprintf(msg, BUF_SIZE, "Found %d tiepoints for (%d,%d)\n", total_n, i+1, j+1);
      zvmessage(msg, "");

    }		// right picture loop

  }		 	// left picture loop

  delete tptlist;

}

////////////////////////////////////////////////////////////////////////
// Defines the grid intersection points; these become the corners of the
// cells that contain tiepoints.
////////////////////////////////////////////////////////////////////////

int get_tiepoint_grid(
		      TiePoint grid[][MAX_GRID_X],
		      int &n_grid_x,
		      int &n_grid_y,
		      int grid_spacing,
		      int nsw,
		      int nlw,
		      int border,
		      PigFileModel *file_model)
{
  const int BUF_SIZE = 256;
  char msg[BUF_SIZE];

  // Get hard borders of image (we can't go outside these, period)

  double sl, ss, el, es;
  file_model->getImageBorders(sl, ss, el, es);
  sl -= file_model->getYOffset();
  if (sl < 0) sl = 0;
  el -= file_model->getYOffset();
  if (el >= file_model->getNL()) el = file_model->getNL() - 1;
  ss -= file_model->getXOffset();
  if (ss < 0) ss = 0;
  es -= file_model->getXOffset();
  if (es >= file_model->getNS()) es = file_model->getNS() - 1;

  // Soft borders on each side are template/2 (so we get a full template),
  // and border (extra slop for rotation or whatever)

  int start_x = (int)(ss + nsw/2 + border);
  int end_x = (int)(es - nsw/2 - border);
  int start_y = (int)(sl + nlw/2 + border);
  int end_y = (int)(el - nlw/2 - border);

  n_grid_x = (end_x - start_x) / grid_spacing;
  n_grid_y = (end_y - start_y) / grid_spacing;
  double grid_spacing_x = (end_x - start_x) / (double)n_grid_x;
  double grid_spacing_y = (end_y - start_y) / (double)n_grid_y;

  if (n_grid_x > MAX_GRID_X) {
    snprintf(msg, BUF_SIZE, "Too many X grid points: %d, max is %d\n",
	     n_grid_x, MAX_GRID_X);
    zvmessage(msg, "");
    zabend();
  }
  if (n_grid_y > MAX_GRID_Y) {
    snprintf(msg, BUF_SIZE, "Too many Y grid points: %d, max is %d\n",
	     n_grid_y, MAX_GRID_Y);
    zvmessage(msg, "");
    zabend();
  }

  double y = start_y;
  n_grid_y = 0;

  while ((int)(y+0.5) <= end_y) {
    double x = start_x;
    n_grid_x = 0;
    while ((int)(x+0.5) <= end_x) {
      memset(&grid[n_grid_y][n_grid_x], 0, sizeof(struct TiePoint));
      grid[n_grid_y][n_grid_x].type = TIEPOINT_TRADITIONAL;
      grid[n_grid_y][n_grid_x].left_line = (int)(y+0.5);
      grid[n_grid_y][n_grid_x].left_sample = (int)(x+0.5);
      grid[n_grid_y][n_grid_x].cs = NULL;

      x += grid_spacing_x;
      n_grid_x++;
    }
    y += grid_spacing_y;
    n_grid_y++;
  }

  return 0;
}

////////////////////////////////////////////////////////////////////////
// Get the "best" (most busy) tiepoint from each grid cell and make a
// new list of them.  The tiepoint list is allocated here; caller is
// responsible for free-ing.  Note that the tpt list might be over-allocated,
// meaning bigger than we need, because of the busyness threshold.
////////////////////////////////////////////////////////////////////////

int get_busyest_from_grid(
			  TiePoint grid[][MAX_GRID_X],
			  int n_grid_x,
			  int n_grid_y,
			  TiePoint **tptlist,
			  int &n_tpts,
			  SimpleImage<double> &busyness_image,
			  double busy_thresh)
{
  n_tpts = (n_grid_x-1) * (n_grid_y-1);
  *tptlist = new TiePoint[n_tpts];
  if (*tptlist == NULL) {
    zvmessage("Not enough memory to allocate tiepoint list!", "");
    zabend();
  }

  n_tpts = 0;

  for (int i = 0; i < n_grid_y-1; i++) {
    for (int j = 0; j < n_grid_x-1; j++) {

      // within grid (i,j) look for busyest location.  Must be above
      // the busyness threshold.

      double max_busy = 0.0;

      for (int m = (int)grid[i][j].left_line;
	   m < (int)grid[i+1][j].left_line; m++) {
	for (int n = (int)grid[i][j].left_sample;
	     n < (int)grid[i][j+1].left_sample; n++) {
	  if (busyness_image.get(m,n) > max_busy) {
	    max_busy = busyness_image.get(m,n);
	    memset(&(*tptlist)[n_tpts], 0, sizeof(struct TiePoint));
	    (*tptlist)[n_tpts].left_line = m;
	    (*tptlist)[n_tpts].left_sample = n;
	    (*tptlist)[n_tpts].active = FALSE;
	    (*tptlist)[n_tpts].type = TIEPOINT_TRADITIONAL;
	    (*tptlist)[n_tpts].cs = NULL;
	  }
	}
      }
      if (max_busy > busy_thresh) {
	(*tptlist)[n_tpts].active = TRUE;
	n_tpts++;		// keep the point
      }
    }
  }
  return 0;
}

////////////////////////////////////////////////////////////////////////
// Projects all the tiepoints from the left to the right image, using
// the camera models.  Determines if the point is in the right image.
// Also determines the relative rotation between frames for that point.
// Copies all found points from left_tiepoints (which is unmodified)
// into tiepoints (which is valid for this pair).  It is assumed that
// tiepoints has been allocated.
//
// We compute the diagonal FOV here of the right image, as in marsmap.
// We make sure each point is within that FOV before back-projecting.
// This gets around a problem with the CAHVOR back-projection math where
// a point off the image can sometimes be projected back into the image,
// due to the R terms behaving badly when extrapolated outside the image
// calibration area.  Todd Litwin is aware of this problem but is unsure
// how to properly solve it (rgd, 7/06).
////////////////////////////////////////////////////////////////////////

int project_tiepoints(
		      TiePoint *left_tiepoints,
		      int n_left_tpts,
		      TiePointPlus *tiepoints,
		      int &n_tpts,
		      PigCameraModel *camera_left,
		      PigCameraModel *camera_right,
		      PigCoordSystem *cs,
		      PigSurfaceModel *surface,
		      PigFileModel *left_file,
		      PigFileModel *right_file,
		      int left_img, int right_img,	// only to fill out TiePoint structure
		      PigPointingModel *point_right,
		      double fov_override)
{
  n_tpts = 0;

  // (this section cribbed from marsmap)
  // Calculate the approximate Field Of View for the right input.
  // Adding H and V FOV's covers the diagonals, plus provides some slop.
  // We divide by 2 because the H and V FOV's cover from edge to edge, while
  // we need only from the center to the edge.  The cosine is stored for
  // easy comparison with a dot product.  We limit the FOV to 90 degrees
  // (180 total) to avoid any back-projection (MER hazcam H+V is > 90).
  // But we make sure we have at least 30 degrees to accomodate very tiny
  // subframes.
  // We also compute and save the camera position and orientation here
  // once, rather than rederiving it for each pixel.

  double FOV = cos((right_file->getFOV(camera_right, 0) +
		    right_file->getFOV(camera_right, 1)) / 2);
  if (debug >= 2)
    printf("calculated FOV = %f = %f degrees\n", FOV,PigRad2Deg(acos(FOV)));
  if (FOV < 0.0)
    FOV = 0.0;			// Limit to 90 degrees
  if (FOV > .866)
    FOV = .866;			// but at least 30 degrees

  if (fov_override > 0) {
    FOV = cos(PigDeg2Rad(fov_override));
    if (debug >= 2)
      printf("override FOV = %f = %f degrees\n", FOV,PigRad2Deg(acos(FOV)));
  }

  PigPoint pos = point_right->getCameraPosition(cs);
  PigVector orient = point_right->getCameraOrientation(cs);

  if (debug >= 2)
    printf("left tiepoint count = %d\n", n_left_tpts);
  for (int i = 0; i < n_left_tpts; i++) {

    if (!left_tiepoints[i].active)		// masked out
      continue;

    tiepoints[n_tpts].tie = left_tiepoints[i];

    project_point(&tiepoints[n_tpts], camera_left, camera_right, cs,
		  surface, left_file, right_file, FOV, pos, orient);
	
    if (tiepoints[n_tpts].tie.active) {
      tiepoints[n_tpts].tie.left_image = left_img;
      tiepoints[n_tpts].tie.right_image = right_img;
      n_tpts++;			// keep it
    }
  }
  return 0;
}

////////////////////////////////////////////////////////////////////////
// Projects one tiepoint from the left to the right image, using
// the camera models.  Determines if the point is in the right image.
// Also determines the relative rotation and scale between frames for
// that point.
////////////////////////////////////////////////////////////////////////

int project_point(
		  TiePointPlus *point,
		  PigCameraModel *camera_left,
		  PigCameraModel *camera_right,
		  PigCoordSystem *cs,
		  PigSurfaceModel *surface,
		  PigFileModel *left_file,
		  PigFileModel *right_file,
		  double FOV,
		  PigPoint pos, PigVector orient)
{
  double samp;
  double line;
  PigPoint origin;
  PigVector look;
  PigPoint surf_pt;
  int hits;

  // Project point into right image.  Convert from File to Camera
  // coords.

  samp = point->tie.left_sample + left_file->getXOffset();
  line = point->tie.left_line + left_file->getYOffset();
  camera_left->LStoLookVector(line, samp, origin, look, cs);

  // printf("origin=(%f,%f,%f), look=(%f,%f,%f)\n", origin.getX(), origin.getY(), origin.getZ(), look.getX(), look.getY(), look.getZ());	//!!!!

  hits=surface->intersectRay(origin, look, surf_pt);

  if (hits <= 0) {		// if (infinity) {
    if ((look % orient) < FOV) {
      point->tie.active = 0;
      // printf("INFINITY CASE REJECT\n");
      return 0;
    }
  }
  else {
    PigVector new_look = surf_pt - pos;
    new_look.normalize();
    if ((new_look % orient) < FOV) {
      point->tie.active = 0;
      // printf("NON-INFINITY CASE REJECT\n");
      return 0;
    }
  }


  // printf("projection: orig l=%f, s=%f, hits=%d, (%f,%f,%f)\n", line, samp, hits, surf_pt.getX(), surf_pt.getY(), surf_pt.getZ());	//!!!!

  camera_right->XYZtoLS(surf_pt, (hits<=0), &line, &samp, cs);

  // printf("projection: new l=%f, s=%f\n", line, samp);	//!!!!

  // camera_right->LStoLookVector(line, samp, origin, look, cs);	//!!!!
  // printf("origin=(%f,%f,%f), look=(%f,%f,%f)\n", origin.getX(), origin.getY(), origin.getZ(), look.getX(), look.getY(), look.getZ());	//!!!!
  // hits=surface->intersectRay(origin, look, surf_pt);	//!!!!
  // printf("re-projection: hits=%d, (%f,%f,%f)\n",  hits, surf_pt.getX(), surf_pt.getY(), surf_pt.getZ());	//!!!!
  // char msg[256];	//!!!!
  // camera_left->writeToString(msg, 256);	//!!!!
  // printf("LCM=%s\n", msg);		//!!!!
  // camera_right->writeToString(msg, 256);	//!!!!
  // printf("RCM=%s\n", msg);		//!!!!



  // Convert back to File coords and save

  point->tie.right_sample = samp - right_file->getXOffset();
  point->tie.right_line = line - right_file->getYOffset();

  // Check if the point is inside the image  (Camera coords)

  point->tie.active = (right_file->testPixelLocation(line, samp) == 0);

  //!!!!  Should there be a slop factor here??  Allow it to project slightly off the image????

  // Move over a little and project again, to get the rotation angle and
  // scale.  Don't bother if we're inactive.

  if (point->tie.active) {
    samp = point->tie.left_sample + left_file->getXOffset() + 10;
    line = point->tie.left_line + left_file->getYOffset();
    camera_left->LStoLookVector(line, samp, origin, look, cs);
    hits=surface->intersectRay(origin, look, surf_pt);
    camera_right->XYZtoLS(surf_pt, (hits<=0), &line, &samp, cs);
    line -= right_file->getYOffset();
    samp -= right_file->getXOffset();

    // rotation of right image rel to left image, +=countercl
    point->rot_angle = atan2(point->tie.right_line - line,
			     samp - point->tie.right_sample);

    // For scale we need to compare the projected vector lengths.
    // We know the left side is 10 because that's how much we perturbed
    // it above.
    PigVector p1(point->tie.right_sample, point->tie.right_line, 0.0);
    PigVector p2(samp, line, 0.0);
    PigVector v = p1 - p2;
    point->scale_factor = v.magnitude() / 10.0;
  }
  else {
    point->tie.active = 0;
  }

  return 0;
}



////////////////////////////////////////////////////////////////////////
// Reads an image into a a double-precision buffer.  The buffer is allocated
// and managed by this routine (and re-used, if possible).  Pixels will
// be packed based on the file model's getNS().
//
// Returns 0 for success, 1 for error.
////////////////////////////////////////////////////////////////////////

int read_image(PigFileModel *file_model,
	       SimpleImage<double> &image,
	       int band)
{
  if (file_model->isFileOpen())
    file_model->closeFile();

  zvopen(file_model->getUnit(), "OP", "READ",
	 "U_FORMAT", "DOUB", "OPEN_ACT", "SA", "IO_ACT","SA", NULL);
  file_model->setFileOpen(TRUE);

  int nl = file_model->getNL();
  int ns = file_model->getNS();
  int nb = file_model->getNB();

  if (band > nb)
    band = nb;		// peg it to the last available band

  image.alloc(nl, ns);

  for (int i=0; i < nl; i++) {
    zvread(file_model->getUnit(), image.linePtr(i), "LINE", i+1,
	   "BAND", band, NULL);
  }

  file_model->closeFile();

  return 0;
}

////////////////////////////////////////////////////////////////////////
// Compute the "busyness" measure across the image.  This algorithm is
// courtesy of Jean Lorre and amounts to comparing each pixel with its
// immediate right and down neighbors (via absolute value of the difference)
// and summing that across the entire template window.
//
// !!!! TBD: for efficiency, use a sliding-sum mechanism here!!!!
////////////////////////////////////////////////////////////////////////

int compute_busyness(SimpleImage<double> &image,
		     SimpleImage<double> &busyness_image, int nlw, int nsw)
{
  int nl = image.getNL();
  int ns = image.getNS();

  busyness_image.alloc(nl, ns);

  // Only a modest gain but doesn't hurt to be parallel here
#pragma omp parallel for
  for (int i=0; i < nl; i++) {
    for (int j=0; j < ns; j++) {

      // too close to edge, no measure. -2 because we omit last
      // row/column of template so we don't go outside it

      if (i < nlw/2 || i > (nl-nlw/2-2) ||
	  j < nsw/2 || j > (ns-nsw/2-2)) {

	busyness_image.set(i, j, 0.0);

      } else {

	double busyness = 0.0;
	for (int m=i-nlw/2; m <= i+nlw/2; m++) {
	  for (int n=j-nsw/2; n <= j+nsw/2; n++) {
	    busyness += fabs(image.get(m,n) - image.get(m,n+1));
	    busyness += fabs(image.get(m,n) - image.get(m+1,n));
	  }
	}
	busyness_image.set(i, j, busyness/((nsw-1)*(nlw-1)));
      }
    }
  }

  return 0;
}

////////////////////////////////////////////////////////////////////////
// Rotate a portion of the image and store the result in out_area.
// Center of rotation is (line,samp), which ends up in the center of
// the output window (which must be odd sized).  "rotation" is in radians.
// + rotation is counterclockwise.  Returns 0 on success, or 1 if the
// area needed goes off the input image.  Also scales the image about
// the same center.
////////////////////////////////////////////////////////////////////////

int rotate_area(SimpleImage<double> &image,
		SimpleImage<double> &out_area,
                int line, int samp,
		int window_height, int window_width,
		double rotation, double scale)
{

  int i,j,ii,jj;
  double costh,sinth,xcenarea,ycenarea,x,y,wr,wl,wt,wb;
  double top,bot;
  
  out_area.alloc(window_height, window_width);

  // Precompute the rotation sin/cos.  We include scale here too.

  costh=cos(rotation) / scale;
  sinth=sin(rotation) / scale;

  xcenarea=(window_width-1)/2;
  ycenarea=(window_height-1)/2;

  // Check corners for going out of bounds
  // Note that the high-end check is >= n-1 because of interpolation;
  // both int(x) and int(x+1) must be valid.

  x = (0-xcenarea)*costh - (0-ycenarea)*sinth + samp;
  y = (0-ycenarea)*costh + (0-xcenarea)*sinth + line;
  if (x < 0 || x >= image.getNS()-1 ||
      y < 0 || y >= image.getNL()-1)
    return 1;
  x = (window_width-1-xcenarea)*costh - (0-ycenarea)*sinth + samp;
  y = (0-ycenarea)*costh + (window_width-1-xcenarea)*sinth + line;
  if (x < 0 || x >= image.getNS()-1 ||
      y < 0 || y >= image.getNL()-1)
    return 1;
  x = (0-xcenarea)*costh - (window_height-1-ycenarea)*sinth + samp;
  y = (window_height-1-ycenarea)*costh + (0-xcenarea)*sinth + line;
  if (x < 0 || x >= image.getNS()-1 ||
      y < 0 || y >= image.getNL()-1)
    return 1;
  x = (window_width-1-xcenarea)*costh-(window_height-1-ycenarea)*sinth + samp;
  y = (window_height-1-ycenarea)*costh+(window_width-1-xcenarea)*sinth + line;
  if (x < 0 || x >= image.getNS()-1 ||
      y < 0 || y >= image.getNL()-1)
    return 1;

  for (i=0; i < window_height; i++){
    for (j=0; j < window_width; j++){
      x = (j-xcenarea)*costh - (i-ycenarea)*sinth + samp;
      y = (i-ycenarea)*costh + (j-xcenarea)*sinth + line;
      jj=(int)x;
      ii=(int)y;
      // bilinear interpolation
      wr=x-jj;
      wl=1.0-wr;
      wb=y-ii;
      wt=1.0-wb;
      top=wl*image.get(ii,  jj) + wr*image.get(ii  ,jj+1);
      bot=wl*image.get(ii+1,jj) + wr*image.get(ii+1,jj+1);
      out_area.set(i, j, top*wt + bot*wb);
    }
  }

  return 0;
}


////////////////////////////////////////////////////////////////////////
// DEBUG method to plot tiepoints on an image from original TiePoint
// "which" says which part of the point to project:
//	0 = left_
//	1 = right_
//	2 = corrected_
////////////////////////////////////////////////////////////////////////

void plot_ties(SimpleImage<double> &image, TiePoint *ties, int n_ties,
	       int which)
{
  char filename[256];
  const int BUF_SIZE = 256;
  char msg[BUF_SIZE];
  SimpleImage<double> out_image;
  static int count = 0;
  snprintf(filename, BUF_SIZE, "tmpx.%d", count);
  count++;

  out_image.alloc(image.getNL(), image.getNS());
  for (int line = 0; line < image.getNL(); line++) {
    memcpy(out_image.linePtr(line), image.linePtr(line),
	   image.getNS() * sizeof(double));
  }

  int active_count = 0;
  for (int index = 0; index < n_ties; index++ ) {
    if (ties[index].active) {
      double l, s;
      if (which == 0) {
	l = ties[index].left_line;
	s = ties[index].left_sample;
      } else if (which == 1) {
	l = ties[index].right_line;
	s = ties[index].right_sample;
      } else {
	l = ties[index].corrected_line;
	s = ties[index].corrected_sample;
      }
      int line = (int)(l + 0.5);
      int samp = (int)(s + 0.5);
      if (line < 0 || line >= out_image.getNL() ||
	  samp < 0 || samp >= out_image.getNS()) {
	snprintf(msg, BUF_SIZE, "BAD POINT: line=%f, samp=%f", l, s);
	zvmessage(msg, "");
	continue;
      }
      //!!!! out_image.set(line, samp, 20000+active_count);
      out_image.set(line, samp, 20000+index);

      active_count++;
    }
  }
  int out_unit;
  zvunit(&out_unit, "plot_ties", count, "u_name", filename, NULL);
  zvopen(out_unit, "op", "write",
	 "u_nl", out_image.getNL(), "u_ns", out_image.getNS(), "u_nb", 1,
	 "format", "half", "u_format", "doub", NULL);
  zvplabel(out_unit, 0, 1);
  for (int line = 0; line < out_image.getNL(); line++) {
    zvwrit(out_unit, out_image.linePtr(line), "line", line+1, NULL);
  }
  zvclose(out_unit, NULL);
}

////////////////////////////////////////////////////////////////////////
// DEBUG method to plot tiepoints on an image using the expanded TiePointPlus
// Rotate is a flag to indicate whether or not to show the rotation
// "which" says which part of the point to project:
//	0 = left_
//	1 = right_
//	2 = corrected_
////////////////////////////////////////////////////////////////////////

void plot_ties(SimpleImage<double> &image, TiePointPlus *ties, int n_ties,
	       int which, int rotate)
{
  char filename[256];
  const int BUF_SIZE = 256;
  char msg[BUF_SIZE];
  SimpleImage<double> out_image;
  static int count = 0;
  snprintf(filename, BUF_SIZE, "tmp.%d", count);
  count++;

  out_image.alloc(image.getNL(), image.getNS());
  for (int line = 0; line < image.getNL(); line++) {
    memcpy(out_image.linePtr(line), image.linePtr(line),
	   image.getNS() * sizeof(double));
  }

  int active_count = 0;
  for (int index = 0; index < n_ties; index++ ) {
    if (ties[index].tie.active) {
      double l, s;
      if (which == 0) {
	l = ties[index].tie.left_line;
	s = ties[index].tie.left_sample;
      } else if (which == 1) {
	l = ties[index].tie.right_line;
	s = ties[index].tie.right_sample;
      } else {
	l = ties[index].tie.corrected_line;
	s = ties[index].tie.corrected_sample;
      }
      int line = (int)(l + 0.5);
      int samp = (int)(s + 0.5);
      if (line < 0 || line >= out_image.getNL() ||
	  samp < 0 || samp >= out_image.getNS()) {
	snprintf(msg, BUF_SIZE, "BAD POINT: line=%f, samp=%f", l, s);
	zvmessage(msg, "");
	continue;
      }
      //!!!!out_image.set(line, samp, 20000+active_count);
      out_image.set(line, samp, 20000+index);

      if (rotate) {
	for (int rot_len = 1; rot_len < 10; rot_len++) {
	  line = (int)(l + sin(ties[index].rot_angle)*rot_len + 0.5);
	  samp = (int)(s + cos(ties[index].rot_angle)*rot_len + 0.5);
	  if (line >= 0 && line < out_image.getNL() &&
	      samp >= 0 && samp < out_image.getNS()) {

	    out_image.set(line, samp, 10000+active_count);
	  }
	}
      }
      active_count++;
    }
  }
  int out_unit;
  zvunit(&out_unit, "plot_ties", count, "u_name", filename, NULL);
  zvopen(out_unit, "op", "write",
	 "u_nl", out_image.getNL(), "u_ns", out_image.getNS(), "u_nb", 1,
	 "format", "half", "u_format", "doub", NULL);
  zvplabel(out_unit, 0, 1);
  for (int line = 0; line < out_image.getNL(); line++) {
    zvwrit(out_unit, out_image.linePtr(line), "line", line+1, NULL);
  }
  zvclose(out_unit, NULL);
}

////////////////////////////////////////////////////////////////////////
// DEBUG method to save a temp image.  If n==0, file is named ".count"
// where count is auto-incremented.  If n!=0, file is named "_n".
////////////////////////////////////////////////////////////////////////

void save_temp_image(SimpleImage<double> &image, int n)
{
  static int count = 0;
  const int BUF_SIZE = 256;
  char filename[BUF_SIZE];
  if (n == 0) {
    snprintf(filename, BUF_SIZE, "tmpimg.%d", count);
    count++;
  }
  else {
    snprintf(filename, BUF_SIZE, "tmpimg_%d", n);
  }

  int out_unit;
  zvunit(&out_unit, "save_temp_image", count, "u_name", filename, NULL);
  zvopen(out_unit, "op", "write",
	 "u_nl", image.getNL(), "u_ns", image.getNS(), "u_nb", 1,
	 "format", "doub", "u_format", "doub", NULL);
  zvplabel(out_unit, 0, 1);

  for (int line = 0; line < image.getNL(); line++) {
    zvwrit(out_unit, image.linePtr(line), NULL);
  }
  zvclose(out_unit, NULL);
}



////////////////////////////////////////////////////////////////////////
// Correlate all the candidate tiepoints for an image pair
// We first rotate and scale the template by the amount the camera model says
// it "should" be, and do a linear correlation.  Then we refine the result
// of that using amoeba8.  But, the refinement uses the ORIGINAL image as
// the template, not the rotated one.  The rotation is built in to the
// initial coefficients.
////////////////////////////////////////////////////////////////////////

int correlate_candidates(
			 SimpleImage<double> &left_image,
			 SimpleImage<double> &right_image,
			 TiePointPlus *tptlist,
			 int n_tpts,
			 int nlw, int nsw,		// template size
			 int param_nlw_rt, int param_nsw_rt,		// search size
			 double min_quality,
			 double linear_quality,
			 float search_edge, float search_factor,
			 int search_min[2], int search_max[2],
			 int omp_on,
			 SimpleImage<double> *left_template[],
			 SimpleImage<double> *correl_img[])
{

  int next_nlw_rt = param_nlw_rt;
  int next_nsw_rt = param_nsw_rt;

#pragma omp parallel for schedule(dynamic) if (omp_on)
  for (int i=0; i<n_tpts; i++) {
    double line_coef_limits[3][2],samp_coef_limits[3][2];
    double line_coef[4],samp_coef[4];
    double line_temp[3],samp_temp[3];
    double line_offset = 0;
    double samp_offset = 0;
    double lin_quality = 0;
    double quality = 0;
    double percent = 100.;
    int limits = 10000;
    int status;

    int nlw_rt = next_nlw_rt;
    int nsw_rt = next_nsw_rt;

    // Get the right thread-pool temp images
    int thr_idx = 0;
#ifdef _OPENMP
    if (omp_on) 
      thr_idx = omp_get_thread_num();
#endif

    if (!tptlist[i].tie.active)
      continue;
    tptlist[i].tie.active = FALSE;	// in case we bail on the point

    int left_line = (int)(tptlist[i].tie.left_line + 0.5);
    int left_samp = (int)(tptlist[i].tie.left_sample + 0.5);
    int right_line = (int)(tptlist[i].tie.right_line + 0.5);
    int right_samp = (int)(tptlist[i].tie.right_sample + 0.5);

    // Find template (left) area.

    if (left_samp - nsw/2 <= 0) continue;		// sanity checks
    if (left_line - nlw/2 <= 0) continue;
    if (left_samp + nsw/2 >= left_image.getNS()) continue;
    if (left_line + nlw/2 >= left_image.getNL()) continue;

    // (un)rotate it to match right side

    status = rotate_area(left_image, *left_template[thr_idx],
			 (int)tptlist[i].tie.left_line,
			 (int)tptlist[i].tie.left_sample,
			 nlw, nsw, tptlist[i].rot_angle,
			 tptlist[i].scale_factor);

    if (status != 0) {
      if (debug >= 2) {
	printf("Rotate outside image: i=%d (%d,%d)\n", i, left_line,left_samp);
      }
      continue;			// went outside the image
    }

    // Find search (right) area.  This is complicated because we
    // want to allow the search window to be skewed, not always
    // centered on the projected location, in order to be able to
    // find matches closer to the edge.  (there is a limit as to how
    // far to go, we use search_min for that).  Gruen itself returns
    // a location relative to the center of the supplied window
    // (internally it subtracts off half the window size to be
    // center-relative).  That is easily compensated for when calculating
    // corrected_line/samp.  However, the transform filtering assumes
    // that offsets are relative to the projection point.  Therefore the
    // returned transform must be adjusted do be relative to that point,
    // so the stdev measures of the offset make sense.  Fortunately this
    // affects only the offset, not the other transform parameters.  The
    // upshot of all this is that we have to remember both the "nominal"
    // search window (which could go off the image), and the actual one
    // (which doesn't).
    // right_line: center of window based on projection
    // nominal_right_start_line: start of window based on projection
    // actual_right_start_line: start of window adjusted to be on image

    int nominal_right_start_line = right_line - nlw_rt/2;
    int nominal_right_start_samp = right_samp - nsw_rt/2;
    int actual_right_start_line = nominal_right_start_line;
    int actual_right_start_samp = nominal_right_start_samp;

    if (actual_right_start_line < 0) {
      actual_right_start_line = 0;
      if (right_line - search_min[0]/2 < 0)
	continue;			// really too close to edge
    }
    if (actual_right_start_line + nlw_rt >= right_image.getNL()) {
      actual_right_start_line = right_image.getNL() - nlw_rt - 1;
      if (actual_right_start_line < 0) continue;
      if (right_line + search_min[0]/2 >= right_image.getNL())
	continue;			// really too close to edge
    }
    if (actual_right_start_samp < 0) {
      actual_right_start_samp = 0;
      if (right_samp - search_min[1]/2 < 0)
	continue;			// really too close to edge
    }
    if (actual_right_start_samp + nsw_rt >= right_image.getNS()) {
      actual_right_start_samp = right_image.getNS() - nsw_rt - 1;
      if (actual_right_start_samp < 0) continue;	// sanity check
      if (right_samp + search_min[1]/2 >= right_image.getNS())
	continue;			// really too close to edge
    }

    SimpleImage<double> right_img(right_image,
				  actual_right_start_line, actual_right_start_samp,
				  nlw_rt, nsw_rt);

    // set initial coefficient values.  Actually irrelevant for mode 0.

    line_coef[0]=0.;
    line_coef[1]=1.;
    line_coef[2]=0.;
    line_coef[3]=0.;
    samp_coef[0]=1.;
    samp_coef[1]=0.;
    samp_coef[2]=0.;
    samp_coef[3]=0.;
 
    // correct candidate tiepoints by correlation

    // Mode 0 is linear correlation

    status = gruen3(left_template[thr_idx], &right_img,
		    correl_img[thr_idx],
		    &line_offset, &samp_offset,
		    line_coef, samp_coef, line_coef_limits, samp_coef_limits,
		    line_temp, samp_temp, percent, limits, &lin_quality, 0,
		    .00001, 0, 0);	//!!!! ftol, inv_flag, use_limits
    //!!!! TBD: should inv_flag be controllable, e.g. for cross-band tiepointing?

    if (status != 0) {
      if (debug >= 2) {
	printf("no result: i=%d, (%d,%d)\n", i, left_line, left_samp);
      }
      continue;			// no result
    }
 
    if (lin_quality < linear_quality) {
      if (debug >= 2) {
	printf("discard lq: i=%d, lq=%f, (%f,%f) (%d,%d)->(%d,%d) rot=%f scale=%f\n",
	       i, lin_quality, line_offset, samp_offset,
	       left_line, left_samp, right_line, right_samp,
	       PigRad2Deg(tptlist[i].rot_angle),
	       tptlist[i].scale_factor);
      }
      continue;			// discard result for low quality
    }

    // Correlate again using amoeba8 to refine the point.  Use original
    // left image (not rotated) and include rotation in initial
    // line_coef/samp_coef values.

    samp_coef[0] = cos(tptlist[i].rot_angle) * tptlist[i].scale_factor;
    samp_coef[1] = sin(tptlist[i].rot_angle) * tptlist[i].scale_factor;
    samp_coef[2] = samp_offset;
    samp_coef[3] = 0.0;
    line_coef[0] = - samp_coef[1];		// - sin(tptlist[i].rot_angle)
    line_coef[1] = samp_coef[0];		// cos(tptlist[i].rot_angle)
    line_coef[2] = line_offset;
    line_coef[3] = 0.0;

    SimpleImage<double> left_img(left_image,
				 left_line-nlw/2, left_samp-nsw/2,
				 nlw, nsw);

    // Mode 7 is amoeba8

    status = gruen3(&left_img,
		    &right_img,
		    correl_img[thr_idx],
		    &line_offset, &samp_offset,
		    line_coef, samp_coef, line_coef_limits, samp_coef_limits,
		    line_temp, samp_temp, percent, limits, &quality, 7,
		    .00001, 0, 0);	//!!!! ftol, inv_flag, use_limits
    //!!!! TBD: should inv_flag be controllable, e.g. for cross-band tiepointing?

    if (quality < min_quality) {
      if (debug >= 2) {
	printf("discard q: i=%d, lq=%f, q=%f, (%f,%f) (%d,%d) rot=%f scale=%f\n",
	       i, lin_quality, quality, line_offset, samp_offset,
	       left_line, left_samp,
	       PigRad2Deg(tptlist[i].rot_angle),
	       tptlist[i].scale_factor);
      }
      continue;			// discard result for low quality
    }

    // compute right image coordinates from offsets.  Since gruen adjusts
    // to the center of the search area, we must take that into account.
    // See large comment block before first gruen call.

    tptlist[i].tie.corrected_line = (actual_right_start_line + nlw_rt/2)
      + line_offset;
    tptlist[i].tie.corrected_sample = (actual_right_start_samp + nsw_rt/2)
      + samp_offset;
    tptlist[i].tie.quality = quality;		// quality 0 to 1

    // Unrotate the coefficients based on the nominal rotation angle
    // (i.e. take out the rotation specified by that angle).
    // This makes it easier to filter based on the coefficients later.
    //!!!! ... at least I think it does... may not matter? ...

    line_coef[0] *= sin(tptlist[i].rot_angle) / tptlist[i].scale_factor;
    line_coef[1] *= cos(tptlist[i].rot_angle) / tptlist[i].scale_factor;
    samp_coef[0] *= cos(tptlist[i].rot_angle) / tptlist[i].scale_factor;
    samp_coef[1] *= -sin(tptlist[i].rot_angle) / tptlist[i].scale_factor;

    // Remove the offset between the nominal and actual window starts from
    // the translation terms, so the filtering will work right.  See large
    // comment block before the first gruen call.

    line_coef[2] += actual_right_start_line - nominal_right_start_line;
    samp_coef[2] += actual_right_start_samp - nominal_right_start_samp;

    for (int j=0; j<4; j++) {
      tptlist[i].coefs[j] = line_coef[j];
      tptlist[i].coefs[j+4] = samp_coef[j];
    }

    tptlist[i].tie.active = TRUE;	// found a good one

    if (debug >= 2) {
      printf("i=%d lq=%f q=%f l0=%9f l1=%9f l2=%9f l3=%9f s0=%9f s1=%9f s2=%9f s3=%9f, rot=%9f, scale=%5f\n",
	     i, lin_quality, quality,
	     line_coef[0], line_coef[1], line_coef[2], line_coef[3],
	     samp_coef[0], samp_coef[1], samp_coef[2], samp_coef[3],
	     PigRad2Deg(tptlist[i].rot_angle),
	     tptlist[i].scale_factor);	//!!!!
    }

    // Finally, check how far off the predicted location we are.
    // Compare the distance to the (usable) size of the search
    // area.  If we're getting too close to the edge of the search
    // area (defined by search_edge) then the search area is
    // expanded by an amount defined by search_factor (multiplicative).
    // Similarly, if we end up closer to the center, we reduce the
    // search area (for efficiency).  A min and max value keep it from
    // getting too ridiculous.
    //
    // This works because we go through the tiepoints in order, top-down.
    // In most in-situ images the geometry is similar as you go left to
    // right but changes as you go top to bottom (because you're going far
    // to near).  Thus if we are getting close to the edge, it's likely
    // that nearby tiepoints are similarly getting close to the edge.
    // It turns out this makes a huge difference in run time; in one
    // test it was 2:39 with vs. 31:07 (!) without.
    //
    // However, this is problematic for parallel processing.  Using the
    // strict algorithm means you could not do pixels in parallel, since
    // each one could potentially affect the next's search window.  So, we
    // modify the algorithm slightly.  We do the same window size
    // computation, but then we save the value in a shared variable so the
    // next thread that happens to start will pick it up.  To avoid too
    // much thrashing, we look at the current value of the shared variable
    // before adjusting, rather than whatever value was used for the actual
    // computation.  For omp off, this degenerates to the serial algorithm.
    // With omp on, it means that a change in window size will affect some
    // future pixels but not the immediately neighboring ones.  This seems
    // to be an acceptable compromise.  However, it does introduce
    // non-deterministic behavior, because it cannot be predicted which
    // future pixel will pick up the change.  C'est la vie!
    //
    // This may not *need* to be a critical section, since it's not that
    // important which specific value gets used (as long as the overall
    // trend is right).  But it feels safer, and forces cache updates.

#pragma omp critical(window_size)
    {
      int old = next_nlw_rt;		// pick up the current value
      int my_nlw_rt = old;

      float diff = fabs(tptlist[i].tie.right_line -
			tptlist[i].tie.corrected_line);
      if (diff > ((my_nlw_rt - nlw) / 2) * (1.0-search_edge)) {
	my_nlw_rt = (int)(my_nlw_rt * search_factor);
	if (my_nlw_rt % 2 == 0)
	  my_nlw_rt++;		// keep it odd
	if (my_nlw_rt > search_max[0])
	  my_nlw_rt = search_max[0];
	if (debug >= 1) {
	  if (my_nlw_rt != old)
	    printf("INCREASE nlw_rt to %d on iter %d\n", my_nlw_rt, i);
	}
      }
      else if (diff < ((my_nlw_rt - nlw) / 2) * search_edge) {
	my_nlw_rt = (int)(my_nlw_rt / search_factor);
	if (my_nlw_rt % 2 == 0)
	  my_nlw_rt--;		// keep it odd
	if (my_nlw_rt < search_min[0])
	  my_nlw_rt = search_min[0];
	if (debug >= 1) {
	  if (my_nlw_rt != old)
	    printf("DECREASE nlw_rt to %d on iter %d\n", my_nlw_rt, i);
	}
      }

      // Set the value for the next iteration
      next_nlw_rt = my_nlw_rt;

      // Same thing in the Sample direction

      old = next_nsw_rt;
      int my_nsw_rt = old;
      diff = fabs(tptlist[i].tie.right_sample -
		  tptlist[i].tie.corrected_sample);
      if (diff > ((my_nsw_rt - nsw) / 2) * (1.0-search_edge)) {
	my_nsw_rt = (int)(my_nsw_rt * search_factor);
	if (my_nsw_rt % 2 == 0)
	  my_nsw_rt++;		// keep it odd
	if (my_nsw_rt > search_max[1])
	  my_nsw_rt = search_max[1];
	if (debug >= 1) {
	  if (my_nsw_rt != old)
	    printf("INCREASE nsw_rt to %d on iter %d\n", my_nsw_rt, i);
	}
      }
      if (diff < ((my_nsw_rt - nsw) / 2) * search_edge) {
	my_nsw_rt = (int)(my_nsw_rt / search_factor);
	if (my_nsw_rt % 2 == 0)
	  my_nsw_rt--;		// keep it odd
	if (my_nsw_rt < search_min[1])
	  my_nsw_rt = search_min[1];
	if (debug >= 1) {
	  if (my_nsw_rt != old)
	    printf("DECREASE nsw_rt to %d on iter %d\n", my_nsw_rt, i);
	}
      }

      // Set the value for the next iteration
      next_nsw_rt = my_nsw_rt;

    }		// end pragma omp critical

  }		// end tiepoint loop

  return 0;
}

////////////////////////////////////////////////////////////////////////
// Filter the correlated points.  This is done by considering an area
// around the point (in pixel space) defined by param_filt_w/h, and finding
// all the active points within that region.  The mean and standard
// deviations of each transform coefficient are then computed.  Then the
// values for the each coefficient are compared against the statstics.  If
// the std dev is less than the corresponding entry in sigma_min[], it is
// passed.  Otherwise, the value must be within sigma*stdev of the mean
// in order to pass.  Finally, the point must also be within mean +/- abs_limit
// in order to pass.  If all coefficients pass these checks, the point is
// accepted.
// Note that we never actually compute the stdev... no need for the sqrt.
// Instead of |(x-mean)| < sigma*stdev, we compare (x-mean)^2 < sigma^2*var
// where stdev = sqrt(var).
//
// When computing statistics, we consider all points that were active at
// the time we started... i.e. throwing one out doesn't exclude it from
// later statistics.  This should help with the problem of running out of
// points with which to do statstics at the bottom of the image, when stuff
// is being rejected all over the place.  It also helps with the bimodal
// problem; when there are two distributions, originally we were filtering
// out all of the first, then all the second passed because there was nothing
// left from the first to skew the statistics.  Since the second is typically
// the worst (e.g. points on the deck, vs. on the ground), considering all
// points may help.  In order to effect this, we set active to negative to
// mean "filtered out but still in for now", and then fix this up at the end.
////////////////////////////////////////////////////////////////////////


int filter_corr_params(TiePointPlus *tptlist, int n_tpts,
		       int param_filt_w, int param_filt_h,
		       double param_filt_sigma[],
		       double param_filt_sigma_min[],
		       double param_filt_abs_limit[])
{
  int i, j, k;
  double sumx[8], sumx2[8], var[8], mean[8];
  int n;
  int reject_count_abs[8];
  int reject_count_sigma[8];
  int reject_count_n;
  double sigma_min_sq[8], sigma_sq[8];
  const int BUF_SIZE = 256;
  char msg[BUF_SIZE];

  memset(reject_count_abs, 0, sizeof(reject_count_abs));
  memset(reject_count_sigma, 0, sizeof(reject_count_sigma));
  reject_count_n = 0;

  for (k=0; k<8; k++) {
    sigma_min_sq[k] = param_filt_sigma_min[k] * param_filt_sigma_min[k];
    sigma_sq[k] = param_filt_sigma[k] * param_filt_sigma[k];
  }

  for (i=0; i < n_tpts; i++) {
    if (!tptlist[i].tie.active)
      continue;			// inactive point, skip it

    int ctr_x = (int)tptlist[i].tie.left_sample;
    int ctr_y = (int)tptlist[i].tie.left_line;

    memset(sumx, 0, sizeof(sumx));
    memset(sumx2, 0, sizeof(sumx2));
    n = 0;

    for (j=0; j < n_tpts; j++) {
      if (tptlist[j].tie.active == 0)
	continue;			// inactive point, skip it

      if (tptlist[j].tie.left_line < ctr_y-param_filt_h)
	continue;
      if (tptlist[j].tie.left_line > ctr_y+param_filt_h)
	continue;
      if (tptlist[j].tie.left_sample < ctr_x-param_filt_w)
	continue;
      if (tptlist[j].tie.left_sample > ctr_x+param_filt_w)
	continue;

      // Found a good point within range.  Gather the stats.

      for (k=0; k<8; k++) {
	double x = tptlist[j].coefs[k];
	sumx[k] += x;
	sumx2[k] += x*x;
      }
      n++;
    }

    if (n == 0) {	// shouldn't happen! since the point itself is included
      snprintf(msg, BUF_SIZE, "INTERNAL ERROR in filter_corr_params:n==0! (i=%d)",i);
      zvmessage(msg, "");
      tptlist[i].tie.active = -1;
      continue;
    }

    // Compute mean and stdev

    if (debug >= 2) {
      printf("i=%d n=%d q=%.2f ", i, n, tptlist[i].tie.quality);	//!!!!
    }

    for (k=0; k<8; k++) {
      mean[k] = sumx[k] / n;
      var[k] = (sumx2[k] - (sumx[k]*sumx[k]/n)) / n;

      if (debug >= 2) {
	printf("%.5f %.5f, ", mean[k], sqrt(var[k]));	//!!!!
      }
    }

    //!!!! Okay so what do we do if n is too small??  Probably should
    //!!!! do the absolute check against mean anyway... but does not
    //!!!! having enough points rule it in, or out?  And how many are
    //!!!! enough?

    // Do filtering: absolute difference from mean

    for (k=0; k<8; k++) {

      if (tptlist[i].coefs[k] < mean[k] - param_filt_abs_limit[k]) {
	reject_count_abs[k]++;
	tptlist[i].tie.active = -1;
	if (debug >= 2)
	  printf(" A%d\n",k);	//!!!!
	break;
      }
      if (tptlist[i].coefs[k] > mean[k] + param_filt_abs_limit[k]) {
	reject_count_abs[k]++;
	tptlist[i].tie.active = -1;
	if (debug >= 2)
	  printf(" A%d\n",k);	//!!!!
	break;
      }
    }

    if (tptlist[i].tie.active != TRUE)
      continue;			// rejected above, so stop here

    // n check
    //!!!! what do we do if n is too low??!!!! And what is too low?
    //!!!! Reject for now.

    if (n < 3) {
      if (debug >= 2)
	printf("Rn\n");	//!!!!
      tptlist[i].tie.active = -1;
      reject_count_n++;
      continue;			//!!!! reject it, for now...
    }

    // Sigma check

    // Do filtering: stdev limits

    for (k=0; k<8; k++) {

      if (var[k] < sigma_min_sq[k])
	continue;		// sigma too small; don't filter on it

      double diff = tptlist[i].coefs[k] - mean[k];
      if (diff*diff >= sigma_sq[k]*var[k]) {
	reject_count_sigma[k]++;
	tptlist[i].tie.active = -1;
	if (debug >= 2)
	  printf(" R%d",k);	//!!!!
	break;
      }
    }
    if (debug >= 2)
      printf("\n");		//!!!!
  }

  for (k=0; k<8; k++) {
    snprintf(msg, BUF_SIZE, "Rejected for coef[%d]: abs=%d, sigma=%d", k,
	     reject_count_abs[k], reject_count_sigma[k]);
    zvmessage(msg, "");
  }
  snprintf(msg, BUF_SIZE, "Rejected for too few points: %d", reject_count_n);
  zvmessage(msg, "");

  n=0;					//!!!!

  // Turn all filtered-out tiepoints (active==-1) to FALSE (inactive) now

  for (i=0; i<n_tpts; i++) {
    if (tptlist[i].tie.active == -1)
      tptlist[i].tie.active = FALSE;
    if (tptlist[i].tie.active)		//!!!!
      n++;				//!!!!
  }
  printf("active count=%d\n", n);		//!!!!


  return 0;
}

////////////////////////////////////////////////////////////////////////
// Implement geometric scattering of the points.  Find the set of points
// that meet certain geometric criteria, ignoring any actual quality measure
// of those point.
//
// First, the number of points to gather is determined based on the desired
// density, and the size of the overlap area.
//
// Algorithm:
//   Set initial candidates to geometric center of tiepoints
//   iterate...
//     loop over candidate points
//       find tiepoint that maximizes distance from all candidates
//         (include a weighting away from the center of the image)
//       set candidate to that point
//     end loop
//   until we converge, or iteration limit reached
//
// "candidates" is allocated by this routine (once the # of them is
// known) and returned.  Caller must free.
//
// "density" is a measure of how many candidates to choose.  This depends
// on how big the overlap area is.  Because we are trying to find tiepoints
// on the edges of the image, not in the middle, we need a measure of the
// length of the overlap.  To do this we simply find the min and max coord
// in each dimension across all tiepoints, treat that as a bounding box for
// the tiepoint area, and get the diagonal distance of that box.  Not terribly
// rigorous (and sensitive to spurious points) but the number of tiepoints to
// gather is not THAT critical... we just want to have few for a corner overlap
// and more for a side overlap.
////////////////////////////////////////////////////////////////////////

int geometric_scatter(TiePointPlus *tptlist, int n_tpts,
		      int *&candidates, int &n_candidates,
		      int density, int max_iterations,
		      int nl, int ns, float center_weight)
{
  int i, j, k, n;

  // Compute geometric center of available tiepoints.  Also compute
  // stdev of the scatter in line and sample, so we can estimate the
  // density and thus the # of candidates to choose

  double sum_l = 0, sum_s = 0;
  double min_l = 1e20, max_l = -1e20, min_s = 1e20, max_s = -1e20;

  n = 0;
  for (i=0; i < n_tpts; i++) {
    if (!tptlist[i].tie.active)
      continue;

    register double line = tptlist[i].tie.left_line;
    register double samp = tptlist[i].tie.left_sample;
    sum_l += line;
    sum_s += samp;
    if (line < min_l) min_l = line;
    if (line > max_l) max_l = line;
    if (samp < min_s) min_s = samp;
    if (samp > max_s) max_s = samp;
    n++;
  }

  if (n == 0) {				// nothing active!!
    n_candidates = 0;
    candidates = NULL;
    return 0;
  }

  double center_l = sum_l / n;
  double center_s = sum_s / n;

  double overlap_length = sqrt((max_l-min_l) * (max_l-min_l) +
			       (max_s-min_s) * (max_s-min_s));
  n_candidates = (int)((overlap_length / density) + 0.5);	// round it
  if (n_candidates < 1)
    n_candidates = 1;
  if (n_candidates > n)
    n_candidates = n;

  printf("n_candidates = %d, length=%f\n", n_candidates, overlap_length);

  // Find the tiepoint closest to the center, to start with
  // We deal exclusively with distance-squared, to avoid the sqrt

  double min_dist = 1.0e20;
  int min_idx = 0;

  for (i=0; i < n_tpts; i++) {
    if (!tptlist[i].tie.active)
      continue;
    double y_d = tptlist[i].tie.left_line - center_l;
    double x_d = tptlist[i].tie.left_sample - center_s;
    double dist = y_d * y_d + x_d * x_d;

    if ((dist) < min_dist) {
      min_idx = i;
      min_dist = dist;
    }
  }

  if (debug >= 1) {
    printf("min dist=%f, min_idx=%d\n", min_dist, min_idx);	//!!!!
  }

  // Set the candidates to this value

  candidates = new int[n_candidates];

  for (j=0; j < n_candidates; j++)
    candidates[j] = min_idx;

  // HERE is the big iteration loop...

  int n_iterations = 0;
  int n_changes = 0;

  do {
    n_iterations++;
    n_changes = 0;

    // Loop over each candidate

    for (j=0; j < n_candidates; j++) {

      min_dist = 1e20;
      min_idx = 0;

      // Loop over each tiepoint

      for (i=0; i < n_tpts; i++) {
	if (!tptlist[i].tie.active)
	  continue;

	// Compute distance from all other candidates
	// We want to overemphasize close distances, to make
	// sure things spread out.  So instead of max(sum(sqrt(d2)))
	// (where d2 is the distance squared) we do min(sum(1/d2)).
	// This avoids the sqrt as well.
	// We exclude the current candidate, since we probably want
	// to end up near it.
	// Finally, we include the distance to the center of the
	// image as an extra point.

	// Start with distance to center

	double y_d = (tptlist[i].tie.left_line - nl/2);
	double x_d = (tptlist[i].tie.left_sample - ns/2);
	if (x_d == 0.0 && y_d == 0.0) x_d = .001;    // avoid div by 0
	double dist = center_weight/(y_d*y_d + x_d*x_d);

	for (k=0; k < n_candidates; k++) {
	  if (k == j)
	    continue;	// exclude current candidate
	  y_d = tptlist[i].tie.left_line -
	    tptlist[candidates[k]].tie.left_line;
	  x_d = tptlist[i].tie.left_sample -
	    tptlist[candidates[k]].tie.left_sample;
	  if (x_d == 0.0 && y_d == 0.0) x_d = .001; // avoid div by 0
	  dist += 1.0/(y_d*y_d + x_d*x_d);
	}

	if (dist < min_dist) {
	  min_dist = dist;
	  min_idx = i;
	}
      }

      // did we change?

      if (min_idx != candidates[j]) {
	candidates[j] = min_idx;
	n_changes++;
      }
      if (debug >= 1) {
	printf("min dist = %f, idx=%d\n", min_dist, min_idx);
      }
    }

    if (debug >= 1) {
      printf("changes this round: %d\n", n_changes);		//!!!!
    }
  } while (n_iterations < max_iterations && n_changes != 0);

  if (debug >= 1) {
    printf("iterations required: %d\n", n_iterations);	//!!!!
  }
  return 0;
}

////////////////////////////////////////////////////////////////////////
// Filter out points for this image that are in the rover mask.  We re-read
// the mask file for each left image for two reasons:  1) the instrument
// (and thus the mask) could change over the mosaic, and 2) the mask is
// actually modified by the filter mechanism for projected shapes (which
// is fixable, but this is much easier).
//
// There is a serious bug in the CAHVOR math that allows points well outside
// the FOV to be projected back onto the image, depending on the O/R terms.
// Or if not into the image, at least they project to bad places.  This
// causes no end of grief for the filter routines, which project triangles
// into image space and expect the triangles to behave somewhat linearly.
// In order to get around this, we create a linear camera model (CAHV) and
// provide *that* to the filters.  We also have to project the tested points
// into this model, of course.
//
// One could make a very good argument that this should be done inside the
// filter routines themselves.  That probably should be done, so marsfilter
// will also work (now it must use linearized images, due to this).  Doing it
// here is just for expediency at the moment.  (rgd 8/20/06).
// CHANGE THIS LATER!!!!
////////////////////////////////////////////////////////////////////////

int filter_points_using_mask(TiePoint *tptlist, int n_tpts,
			     PigFileModel *file_model, PigCameraModel *camera_model,
			     PigCoordSystem *cs, char *mask_filename,
			     PigSurfaceModel *surface)
{
  FilterShape **filters;
  int num_filters;
  int i, j;

  int nl = file_model->getNL();
  int ns = file_model->getNS();

  int rmc[PIG_MAX_CS_INDEX];
  int num_rmc = 0;
  file_model->getRoverMotionCounter(rmc, num_rmc);

  PigMission *m = PigMission::getMissionObject(file_model);
  int status = FilterShape::readFilters(m, mask_filename,
					"autotie_filter",
					camera_model->getInstrumentName(),
					filters, num_filters, NULL, rmc, num_rmc);
  if (status != 0)
    return status;		// message already printed

  // Create the linear camera model

  PigCameraModel *linear_camera =
    camera_model->alignStereoCameras(ns, nl, ns, nl, ns, nl, NULL);

  for (j=0; j < num_filters; j++) {

    PigCoordSystem *site_cs = m->getCoordSystem(file_model, "SITE");

    //!!!! Note... should add parameter support for filters eventually...

    filters[j]->setSourceImage(file_model, linear_camera, cs, site_cs,
			       NULL, nl, ns, NULL);
  }

  // Now loop through all points, and all filters, and do the deed

  for (i=0; i < n_tpts; i++) {

    if (!tptlist[i].active)
      continue;				// next!

    // Project tiepoint into the linear model.

    double samp = tptlist[i].left_sample + file_model->getXOffset();
    double line = tptlist[i].left_line + file_model->getYOffset();

    PigPoint origin;
    PigVector look;
    PigPoint surf_pt;
    int hits;

    camera_model->LStoLookVector(line, samp, origin, look, cs);
    hits=surface->intersectRay(origin, look, surf_pt);
    linear_camera->XYZtoLS(surf_pt, (hits<=0), &line, &samp, cs);
    int ll = (int)(line - file_model->getYOffset() + 0.5);
    int ss = (int)(samp - file_model->getXOffset() + 0.5);

    for (j=0; j < num_filters; j++) {
      if (filters[j]->isPointFiltered(ll, ss)) {		// bzzzt!
	tptlist[i].active = FALSE;
	break;
      }
    }
  }

  delete linear_camera;
  for (j=0; j<num_filters; j++)
    if (filters[j]) delete filters[j];
  delete filters;

  return 0;
}
