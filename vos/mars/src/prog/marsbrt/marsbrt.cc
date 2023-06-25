/////////////////////////////////////////////////////////////////////////
// marsbrt - Brightness correction program
////////////////////////////////////////////////////////////////////////

#include "vicmain_c"

#include "mars_support.h"
#include "mars_overlaps.h"

#include "amoeba.h"

#include "PigMission.h"
#include "PigFileModel.h"
#include "PigCameraModel.h"
#include "PigPointingModel.h"
#include "PigSurfaceModel.h"
#include "PigVector.h"
#include "RadiometryModel.h"
#include "PigCoordSystem.h"
#include "PigCAHV.h"
#include "PigCAHVOR.h"
#include "PigCAHVORE.h"
#include "PigCSReference.h"
#include "PigMER.h"
#include "PigSurfacePlane.h"
#include "PigBrtCorrModel.h"
#include "PigBrtCorrLinear.h"

#include <string.h>

/* buffer sizes in main program */
#define MAX_INPUTS 2000
#define MAX_NL MARS_MAX_NL
#define MAX_NS MARS_MAX_NS

#define MAX_NUM_PARAMS	2	// just mean, stdev currently

#ifdef SQR
#undef SQR
#endif
#define SQR(x) ((x)*(x))

// Structure for storing the results.

struct Results {
  double mult;		// multiplicative factor (applied first)
  double add;			// additive factor
};

// Parameter structure for "objective" cost function (the function that
// evaluates pointing solutions).  Note that several of these are just
// pointers to the main program's arrays.

struct ObjectiveParams {
  int nids;
  PigPointingModel **pointing_in;
  PigCameraModel **camera_in;
  PigSurfaceModel *surface_model;
  PigCoordSystem *cs;

  int *n_overlaps;
  Overlap *overlaps;
  Results *results;
  Results *orig_results;

  double (*inertia)[MAX_NUM_PARAMS];	// wt to keep images in place
  double *drift_wt;	// weight to keep solution from drifting

  int refimg[MAX_INPUTS];

  double max_error;		// biggest error on any overlap (output)
  int who;			// which overlap was biggest (output)

  int adjust_linear;		// Do linear (add + mult)
  int adjust_add;		// Additive factor only
  int adjust_mult;		// Multiplicative factor only

  int use_mean;		// True if mean should be used
  int use_stdev;		// True if stdev should be used

  int use_normal;		// True if normal overlaps should be used
  int use_overall;		// True if overall overlaps shoudl be used

  double overall_mean;	// Target for overall mean
  double overall_stdev;	// Target for overall stdev

  double lambda_mult;		// Length scale for mult
  double lambda_add;		// Length scale for add
};

extern "C" double Objective(double p[], int ndim, void *func_args);
void save_brightness(char *name, int n, PigFileModel *file_models[],
		     Results results[], char *solution_id,
		     int do_add, int do_mult, int use_hsi);
void pack_params(double Pzero[], double lambda[], int &ndim,
		 ObjectiveParams *params);
double unpack_params(double Pzero[], int ndim,
		     ObjectiveParams *params);

////////////////////////////////////////////////////////////////////////
// MarsBRT program
////////////////////////////////////////////////////////////////////////

void main44()
{

  int i, j, status;
  int count, count2, def;
  const int BUF_LEN = 150;
  char msg[BUF_LEN];

  // Inputs

  int nids;
  PigFileModel *file_models[MAX_INPUTS];
  PigCameraModel *camera_in[MAX_INPUTS];
  PigPointingModel *pointing_in[MAX_INPUTS];
  PigSurfaceModel *surface_model;
  int homogeneous_inputs = TRUE;
  PigCoordSystem *cs, *output_cs;
  PigBrtCorrModel *brt_corr[MAX_INPUTS];

  // Overlaps

  Overlap *overlaps;
  int n_overlaps;
  int refimg[MAX_INPUTS];

  Results results[MAX_INPUTS];	// results for each image
  Results orig_results[MAX_INPUTS];	// Original results (for inertia)

  double residual;

  // User Parameters

  char out_filename[PIG_MAX_FILENAME_SIZE];
  char ovr_filename[PIG_MAX_FILENAME_SIZE];
  char out_ovr_filename[PIG_MAX_FILENAME_SIZE];
  int out_ovr_count;
  char mission[64], instrument[64];
  char solution_id[64];
  int recycle;
  double ftol;
  double inertia[MAX_NUM_PARAMS];
  double drift_wt[2];
  int start_key;
  int adjust_linear, adjust_mult, adjust_add;
  double lambda_mult, lambda_add;
  int use_mean, use_stdev;
  int normalize;
  int iter_outlier;		// iterations for removing outliers
  double outlier_sigma;
  int use_normal, use_overall, use_pre_overall;
  double overall_mean, overall_stdev=0.0;

  zvmessage("MARSBRT version 2020-02-18", "");

  mars_setup(nids, file_models, camera_in, pointing_in, 
	     surface_model, NULL, brt_corr, output_cs, mission, instrument,
	     homogeneous_inputs,
	     MAX_NL, MAX_NS, MAX_INPUTS);

  cs = surface_model->getCoordSystem();
 
  // Load overlap file

  n_overlaps = MAX_OVERLAPS;
  overlaps = new Overlap[n_overlaps];
  if (overlaps == NULL) {
    zvmessage("Unable to allocate memory for Overlaps!", "");
    zabend();
  }

  zvp("in_ovr", ovr_filename, &count);

  mars_load_overlaps(ovr_filename, overlaps, n_overlaps, file_models, nids);
  snprintf(msg, BUF_LEN, "%d overlaps read from %s", n_overlaps, ovr_filename);
  zvmessage(msg, "");
 
  zvp("out_ovr", out_ovr_filename, &out_ovr_count);

  // get parameter overrides if any

  zvp("RECYCLE", &recycle, &count);
  if (recycle <= 0)
    recycle = 1;

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

  if (zvptst("UNTIL")) {
    if (count < 1 || refimg_array[0] <= 0) {
      zvmessage("Invalid entry for REFIMAGE with UNTIL specified", "");
      zabend();
    }
    for (i=0; i < refimg_array[0]; i++)
      refimg[i] = TRUE;
  }

  // Look at the ignore array.  Any overlaps using an image in this list
  // will be inactivated.

  int ignore_set = FALSE;
  int ignore_array[MAX_INPUTS];
  int ignore[MAX_INPUTS];
  for (i=0; i<nids; i++)
    ignore[i] = FALSE;
  int ignore_partial = zvptst("IGNORE_PARTIAL");
  status = zvparm("IGNORE", ignore_array, &count, &def, MAX_INPUTS, 0);
  if (status == 1 && count > 0) {
    ignore_set = TRUE;
    for (i=0; i < count; i++) {
      if ((ignore_array[i] > nids) | (ignore_array[i] < -nids)) {
	zvmessage("IGNORE value bigger than # of inputs!", "");
	zabend();
      }
      if (ignore_array[i] <= 0) {
	if (i < 1 || ignore_array[i-1] <= 0 ||
	    ignore_array[i-1] >= -ignore_array[i]) {
	  zvmessage("IGNORE value less than 0 and prior ignore not correct (neg or > this)", "");
	  zabend();
	}
	for (int j = ignore_array[i-1]; j <= -ignore_array[i]; j++) {
	  ignore[j-1] = TRUE;
	}
      }
      else {
	ignore[ignore_array[i]-1] = TRUE;
      }
    }
  }

  if (ignore_set) {
    int n_ignored = 0;
    for (i=0; i < n_overlaps; i++) {
      Overlap *ov = &overlaps[i];
      if (!ov->active)
	continue;

      for (int j=0; j < ov->n_images; j++) {
	int im = ov->stats[j].image;
	if (ignore[im]) {
	  if (!ignore_partial) {
	    ov->active = FALSE;	// Eliminate whole ov if ignored
	    n_ignored++;
	    break;
	  }
	  else {		// Eliminate only this image from the ov
	    for (int k=j; k < ov->n_images-1; k++) {
	      ov->stats[k] = ov->stats[k+1];
	    }
	    ov->n_images--;
	    j--;			// look again at the new j
	  }
	}
      }
      if (ov->n_images < 2) {	// got rid of everything in here
	ov->active = FALSE;
	n_ignored++;
      }
    }
    snprintf(msg, BUF_LEN, "%d overlaps ignored due to IGNORE parameter", n_ignored);
    zvmessage(msg, "");
  }

  // Ignore intra-set matches.  In other words, if an overlap has two
  // or more images from the non-reference (active) set, ignore the
  // overlap.  This allows you to run a bunch of images at once, ignoring
  // connections between them, and only looking at how they connect to
  // their reference-image neighbors.  Use case: making navcams match the
  // mastcams in a gigapan.

  if (zvptst("IGNORE_INTRA")) {
    int n_ignored = 0;
    for (i=0; i < n_overlaps; i++) {
      Overlap *ov = &overlaps[i];
      if (!ov->active)
	continue;

      int n_in_nonref = 0;
      for (int j=0; j < ov->n_images; j++) {
	int im = ov->stats[j].image;
	if (!refimg[im]) {
	  n_in_nonref++;
	  if (n_in_nonref > 1) {
	    ov->active = FALSE;
	    n_ignored++;
	    break;
	  }
	}
      }
    }
    snprintf(msg, BUF_LEN, "%d overlaps ignored due to IGNORE_INTRA parameter",
	     n_ignored);
    zvmessage(msg, "");
  }

  // Check on which statistics to use

  use_mean = zvptst("USE_MEAN");
  use_stdev = zvptst("USE_STDEV");

  int num_params = 0;
  if (use_mean)
    num_params++;
  if (use_stdev)
    num_params++;

  if (num_params == 0) {
    zvmessage("Must have either USE_MEAN or USE_STDEV (or both) enabled!","");
    zabend();
  }

  use_normal = zvptst("NORMAL") || zvptst("BOTH");
  use_overall = zvptst("OVERALL") || zvptst("BOTH");
  use_pre_overall = zvptst("PRE_OVERALL");

  // Compute overall target statistics if needed.  Averaging the stdevs
  // is probably not statistically correct but it should be good enough
  // for this purpose.
  //!!!! We don't distinguish between OVERAL_OVERALL and OVERLAP_OVERALL_HSI.
  //!!!! Do we need to?  Don't think so...

  zvparmd("OVERALL_MEAN", &overall_mean, &count, &def, 1, 0);
  zvparmd("OVERALL_STDEV", &overall_mean, &count2, &def, 1, 0);
  if ((count == 0 || count2 == 0) && (use_overall || use_pre_overall)) {
    // Compute average of means and stdevs as target
    double mean_sum = 0.0;
    double stdev_sum = 0.0;
    int n = 0;
    for (i=0; i < n_overlaps; i++) {
      if (!overlaps[i].active)
	continue;
      if (overlaps[i].type == OVERLAP_OVERALL ||
	  overlaps[i].type == OVERLAP_OVERALL_HSI) {
	mean_sum += overlaps[i].stats[0].mean;
	stdev_sum += overlaps[i].stats[0].stdev;
	n++;
      }
    }
    if (n == 0) {
      zvmessage("Overall turned on but no overall overlaps found","");
      n = 1;
    }
    if (count == 0)
      overall_mean = mean_sum / n;
    if (count2 == 0)
      overall_stdev = stdev_sum / n;
  }
  if (use_overall || use_pre_overall) {
    snprintf(msg, BUF_LEN, "Overall target: mean=%f stdev=%f", overall_mean,
	     overall_stdev);
    zvmessage(msg, "");
  }

  for (i=0; i < num_params; i++)
    inertia[i] = 0.0;
  status = zvparmd("INERTIA", inertia, &count, &def,MAX_NUM_PARAMS,0);

  status = zvparmd("DRIFT_WT", drift_wt, &count, &def,2,0);

  zvp("START_KEY", &start_key, &count);

  // Check on what to adjust...

  adjust_linear = zvptst("DO_LINEAR");
  adjust_add = zvptst("DO_ADD");
  adjust_mult = zvptst("DO_MULT");

  zvparmd("LAMBDA_MULT", &lambda_mult, &count, &def,1,0);
  zvparmd("LAMBDA_ADD", &lambda_add, &count, &def,1,0);

  normalize = zvptst("NORMALIZE");

#if 0
  // Make sure there are enough degrees of freedom
  //!!!! this may not be relevant!
  if (adjust_linear && !(use_mean && use_stdev)) {
    zvmessage("Can't do LINEAR adjustment without both USE_MEAN and USE_STDEV","");
    zabend();
  }
#endif

  zvparmd("FTOL", &ftol, &count, &def, 1, 0);

  zvp("OUT_SOLUTION_ID", solution_id, &count);
  if (count != 1) {
    zvmessage("Output Solution ID required!", "");
    zabend();
  }

  zvpone("OUT", out_filename, 1, sizeof(out_filename));

  zvp("ITER_OUTLIER", &iter_outlier, &count);

  zvparmd("OUTLIER", &outlier_sigma, &count, &def, 1, 0);

  int use_hsi = zvptst("HSI");
  if (zvptst("PER_OVERLAPS")) {
    // Look for any active overlaps using HSI.  If any, turn it on.
    // If not PER_OVERLAPS then we force the issue, which may or may not
    // be what the user really wants.
    use_hsi = FALSE;		// just in case
    for (i=0; i < n_overlaps; i++) {
      if (!overlaps[i].active)
	continue;
      if (overlaps[i].type == OVERLAP_HSI ||
	  overlaps[i].type == OVERLAP_OVERALL_HSI) {
	use_hsi = TRUE;
	break;
      }
    }
  }
  snprintf(msg, BUF_LEN, "Using %s colorspace", (use_hsi?"HSI":"RGB"));
  zvmessage(msg, "");

  // Print out input status from labels

  mars_print_inputs(nids, pointing_in, camera_in, file_models,
		    homogeneous_inputs, mission, instrument);

  int refimg_set = FALSE;
  for (i=0; i < nids; i++) {
    if (refimg[i]) {
      snprintf(msg, BUF_LEN, "Reference image: %d", i+1);
      zvmessage(msg, "");
      refimg_set = TRUE;
    }
  }
  if (!refimg_set) {
    snprintf(msg, BUF_LEN, "No reference image");
    zvmessage(msg, "");
  }

  //////////////////////////
  // Now compute the actual solutions
  //////////////////////////

  // Initialize the results.  If we have an input brt corr file, that
  // becomes the original results.

  for (i=0; i < nids; i++) {
    orig_results[i].mult = 1.0;
    orig_results[i].add = 0.0;

    // HsiLin is a subclass of Linear
    if ((brt_corr[i] != NULL) &&
	((strcmp(brt_corr[i]->getModelName(), "PigBrtCorrLinear") == 0) ||
	 (strcmp(brt_corr[i]->getModelName(), "PigBrtCorrHsiLin") == 0))) {

      PigBrtCorrLinear *bcl = (PigBrtCorrLinear *)brt_corr[i];
      orig_results[i].add = bcl->getAdd();
      orig_results[i].mult = bcl->getMult();
      snprintf(msg, BUF_LEN, "Setting image %d to init cond from brt file mult=%f add=%f",
	       i, orig_results[i].mult, orig_results[i].add);
      zvmessage(msg, "");
    }
  }

  // If we have pre-condition overlaps turned on, calculate the initial
  // conditions from them.  Note that this overrides the brt inputs because
  // you can always remove the overall ties from the ovr file or just turn
  // off pre if you don't want them.  Plus you can disable via ref image.

  if (use_pre_overall) {
    for (i=0; i < n_overlaps; i++) {
      if (!overlaps[i].active)
	continue;
      if (overlaps[i].type == OVERLAP_OVERALL ||
	  overlaps[i].type == OVERLAP_OVERALL_HSI) {
	int im = overlaps[i].stats[0].image;
	if (refimg[im])
	  continue;		// don't modify a ref

	double mult = overall_stdev / overlaps[i].stats[0].stdev;
	orig_results[im].add = overall_mean -
	  overlaps[i].stats[0].mean * mult;
	orig_results[im].mult = mult;
	snprintf(msg, BUF_LEN, "setting image %d to init cond mult=%f add=%f",
		 im, mult, orig_results[im].add);
	zvmessage(msg, "");
      }
    }
  }

  for (i=0; i < nids; i++) {
    results[i] = orig_results[i];
  }

  double Pzero[MAX_INPUTS*MAX_NUM_PARAMS];
  double lambda[MAX_INPUTS*MAX_NUM_PARAMS];
  int ndim, iter=0;

  // Fill in the parameters for the cost function

  ObjectiveParams params;

  params.nids = nids;
  params.pointing_in = pointing_in;
  params.camera_in = camera_in;
  params.surface_model = surface_model;
  params.n_overlaps = &n_overlaps;
  params.overlaps = overlaps;
  params.results = results;
  params.orig_results = orig_results;
  memcpy(params.refimg, refimg, sizeof(refimg));
  params.inertia = NULL;		// only set if something is non-0
  for (i=0; i < num_params; i++) {
    if (inertia[i] != 0.0)
      params.inertia = &inertia;
  }
  params.drift_wt = drift_wt;
  params.cs = cs;
  params.adjust_linear = adjust_linear;
  params.adjust_add = adjust_add;
  params.adjust_mult = adjust_mult;
  params.use_mean = use_mean;
  params.use_stdev = use_stdev;
  params.lambda_mult = lambda_mult;
  params.lambda_add = lambda_add;
  params.use_normal = use_normal;
  params.use_overall = use_overall;
  params.overall_mean = overall_mean;
  params.overall_stdev = overall_stdev;

  for (int recycle_count = 0; recycle_count < recycle + iter_outlier;
       recycle_count++) {

    zvmessage("", "");

    // Get initial guess

    pack_params(Pzero, lambda, ndim, &params);

    snprintf(msg, BUF_LEN, "Initial error metric (unweighted): %f",
	     sqrt(Objective(Pzero, ndim, &params)));
    zvmessage(msg, "");
    snprintf(msg, BUF_LEN, "Overlap %d has an initial residual (weighted) of %f (%d pix)",
	     params.who, sqrt(params.max_error),
	     overlaps[params.who].n_pixels);
    zvmessage(msg, "");

    // solve for all pointing params at once using simplex method

    residual = amoeba4(Pzero, lambda, ndim, ftol, 9000000, &iter,
		       Objective, &params, 100000);

    // retrieve solutions

    unpack_params(Pzero, ndim, &params);

    // If we're normalizing the results, do it now.  This changes
    // the corrections so that the average of all the multiplicative
    // corrections is 1.0 and the average of all additive corrections
    // is 0.0.  This prevents the solution from "drifting" in brightness
    // or contrast.

    if (normalize) {

      residual = Objective(Pzero, ndim, &params);
      snprintf(msg, BUF_LEN, "Pre-normalize (unweighted) error metric: %f",
	       sqrt(residual));
      zvmessage(msg, "");

      int do_mult = adjust_linear || adjust_mult;
      int do_add = adjust_linear || adjust_add;

      double mean_mult = 0.0;
      for (i=0; i < nids; i++) {
	mean_mult += results[i].mult;
      }
      mean_mult /= nids;

      // Divide all the mults, and the adds, by mean_mult.  This
      // makes the average mult correction be 1.0.

      if (do_mult && mean_mult != 0.0) {
	for (i=0; i < nids; i++) {
	  results[i].mult /= mean_mult;
	  results[i].add /= mean_mult;
	}
      }

      // Now we use the post-adjust add factors to ensure the
      // average add is 0.

      double mean_add = 0.0;
      for (i=0; i < nids; i++) {
	mean_add += results[i].add;
      }
      mean_add /= nids;

      // subtract the mean from the adds only.  The mults are not
      // re-modified here.

      if (do_add && mean_add != 0.0) {
	for (i=0; i < nids; i++) {
	  results[i].add -= mean_add;
	}
      }

      snprintf(msg, BUF_LEN, "Normalization, mean_mult = %f, mean_add = %f",
	       mean_mult, mean_add);
      zvmessage(msg, "");
      pack_params(Pzero, lambda, ndim, &params);
    }

    // Check solution for residual.  We calculate it again just to get
    // max_error and who right.

    residual = Objective(Pzero, ndim, &params);

    snprintf(msg, BUF_LEN, "Solution (unweighted) error metric: %f after %d iterations",
	     sqrt(residual), iter);
    zvmessage(msg, "");
    snprintf(msg, BUF_LEN, "Overlap %d has a (weighted) residual of %f (%d pix)",
	     params.who, sqrt(params.max_error),
	     overlaps[params.who].n_pixels);
    zvmessage(msg, "");

    // Remove outliers if requested

    //!!!! comment: todo - make sure removed points don't create islands

    double stdev = 0.0;
    if (n_overlaps >= 5) {

      // Compute stdev of errors
      // Note that errors should approximate a half-gaussian, peaked
      // at 0.  To make it a gaussian distribution we add both +err and
      // -err to the statistics (forcing the mean to 0).  This
      // simplifies to sqrt(sum(err^2)/N).

      //double sum = 0.0;
      double sum_sq = 0.0;
      for (i=0; i < n_overlaps; i++) {
	if (!params.overlaps[i].active)
	  continue;
	double err = params.overlaps[i].error;
	sum_sq += err*err;
      }
      stdev = sqrt(sum_sq / n_overlaps);

      snprintf(msg, BUF_LEN, "Residual (weighted) stdev = %f n=%d",
	       stdev, n_overlaps);
      zvmessage(msg, "");
    }

    if ((recycle_count < iter_outlier) && (stdev != 0.0)) {

      // Reject points.  Oddly, we want to keep inactive points...
      // they haven't been rejected, just are not participating at
      // the moment (they may in future runs).

      int k = 0;
      for (i=0; i < n_overlaps; i++) {
	double err = params.overlaps[i].error;
	if ((!params.overlaps[i].active) ||
	    err <= outlier_sigma * stdev) {		// good
	  params.overlaps[k] = params.overlaps[i];
	  k++;
	}
      }
      int gone = n_overlaps - k;
      snprintf(msg, BUF_LEN, "Removed %d overlaps", gone);
      zvmessage(msg, "");

      n_overlaps = k;

      if (k == 0) {
	recycle_count = iter_outlier-1;	// skip remaining iters
	if (recycle != 0) {
	  zvmessage("Skipping remaining iter_outliers; restarting with last soln as starting point","");
	}
      }
      else {
	zvmessage("Resetting solution to initial conditions", "");
	for (i=0; i < nids; i++) {
	  results[i] = orig_results[i];
	}
      }
    }

    else if (recycle_count != (iter_outlier + recycle-1))
      zvmessage("Restarting with last solution as starting point","");

    // Save results so far, in case user quits early
    save_brightness(out_filename, nids, file_models, results, solution_id,
		    (adjust_linear || adjust_add),
		    (adjust_linear || adjust_mult),
		    use_hsi);
  }

#if 0 //!!!!
  for (i=0; i<n_overlaps; i++) {
    char msg[256];
    snprintf(msg, 256, "overlap %i err=%f npix=%d imgs=", i, params.overlaps[i].error, params.overlaps[i].n_pixels);
    for (int j=0; j<params.overlaps[i].n_images; j++) {
      char msg2[100];
      snprintf(msg2, 100, "%d ", params.overlaps[i].stats[j].image);
      strcat(msg, msg2);
    }
    printf("%s\n", msg);
  }
#endif

  // save modified overlaps -- XML format only

  if (out_ovr_count != 0) {
    mars_save_overlaps(out_ovr_filename, overlaps, n_overlaps,
		       file_models, nids, start_key);
  }

#if 0
  // print out tiepoints

  zvmessage("Final tiepoint list (Camera coordinates)", "");
  mars_print_tiepoint_list(tiepoints, n_overlaps);
#endif

  // determine which pictures are represented (when present[i] is 1)

  int present[MAX_INPUTS];

  for (i=0; i < nids; i++)
    present[i] = FALSE;
  for (i=0; i < n_overlaps; i++) {
    Overlap *ov = &overlaps[i];
    if (!ov->active)
      continue;
    for (j=0; j < ov->n_images; j++)
      present[ov->stats[j].image] = TRUE;
  }

  for (i=0; i < nids; i++) {
    if (!present[i]) {
      snprintf(msg, BUF_LEN, "Input image %d not represented by overlaps", i+1);
      zvmessage(msg, "");
    }
  }

  for (i=0; i < nids; i++) {
    if ((refimg[i]) && !(present[i])) {
      snprintf(msg, BUF_LEN, "Reference image %d not connected to rest of mosaic.",
	       i+1);
      zvmessage(msg, "");
    }
  }

  // re-check normalized solution to be certain

  pack_params(Pzero, lambda, ndim, &params);

  residual = Objective(Pzero, ndim, &params);

  snprintf(msg, BUF_LEN, "Final solution error metric: %f", sqrt(residual));
  zvmessage(msg, "");

  snprintf(msg, BUF_LEN, "%d iterations required", iter);
  zvmessage(msg, "");

  zvmessage("Image   MultCorr        AddCorr", "");

  for (i=0; i<nids; i++) {
    snprintf(msg, BUF_LEN, "%7d %15.10lf %15.10lf",i, results[i].mult, results[i].add);
    zvmessage(msg, "");
  }

  // Write out correction file

  save_brightness(out_filename, nids, file_models, results, solution_id,
		  (adjust_linear || adjust_add),
		  (adjust_linear || adjust_mult),
		  use_hsi);

}

////////////////////////////////////////////////////////////////////////
// Objective function for brightness solution
////////////////////////////////////////////////////////////////////////

extern "C"
double Objective(double p[], int ndim, void *func_args)
{
  const int BUF_LEN = 256;
  char msg[BUF_LEN];
  ObjectiveParams *params = (ObjectiveParams *)func_args;

  // Unpack parameters into params->results to make it easier to deal with.

  double inertia_wt = unpack_params(p, ndim, params);

  // Compute metric
  // We can ignore the "adjust_*" here because both add and mult in result
  // are initialized to no-op status, and never changed if their adjust is
  // not on.

  double sum = 0.0;
  params->max_error = 0.0;
  params->who = 0;

  // The errors are weighted by the number of pixels involved.  So
  // large overlaps count more than small ones.

  long total_pixels = 0;

  for (int k=0; k < *params->n_overlaps; k++) {
    Overlap *ov = &params->overlaps[k];
    if (!ov->active)
      continue;

    double err = 0.0;
    int nthis;

    switch (ov->type) {

      // Both standard and HSI overlaps look and act the same.  The only
      // difference is in how we label the output.

    case OVERLAP_MEAN_STDEV:
    case OVERLAP_HSI:
      {
	if (!params->use_normal)
	  continue;			// skip

	// Compare each image in the overlap with every other

	nthis = 0;
	for (int i=0; i < ov->n_images-1; i++) {
	  for (int j=i+1; j < ov->n_images; j++) {
	    int ik = ov->stats[i].image;
	    int jk = ov->stats[j].image;

	    // Compute adjusted mean and stdev from results

	    double imean = 0.0, jmean = 0.0;
	    if (params->use_mean) {
	      imean = ov->stats[i].mean *
		params->results[ik].mult +
		params->results[ik].add;
	      jmean = ov->stats[j].mean *
		params->results[jk].mult +
		params->results[jk].add;
	    }
	    double istd = 1.0, jstd = 1.0;
	    if (params->use_stdev) {
	      istd = ov->stats[i].stdev *
		params->results[ik].mult;
	      jstd = ov->stats[j].stdev *
		params->results[jk].mult;
	    }


	    // Combine them into a single error metric
	    //!!!! DO THIS CORRECTLY, WHATEVER THAT IS!!!!
	    // Make sure to avoid divide by zero, which would
	    // also indicate a degererate solution (0 stdev means
	    // 0 contrast and a constant image).
	    // Square each of the terms.

	    double thiserr = 0.0;
	    double avgmean = (imean+jmean)/2.0;
	    double avgstd = (istd+jstd)/2.0;
	    if (fabs(avgmean) < 1e-6)
	      avgmean = 1e-6;		// avoid divide by 0
	    if (fabs(avgstd) < 1e-6)
	      avgstd = 1e-6;
	    thiserr = SQR((imean - jmean) / avgmean) +
	      SQR((istd - jstd) / avgstd);
	    err += thiserr;
	    nthis++;
	  }
	}
	if (nthis != 0)		// should never be 0
	  err /= nthis;	// adjust for # of images in overlap

	err *= ov->n_pixels;		// weight by size of overlap
	total_pixels += ov->n_pixels;
	break;
      }

    case OVERLAP_OVERALL:
    case OVERLAP_OVERALL_HSI:
      {
	if (!params->use_overall)
	  continue;			// skip

	// Compare the overall stats with the target ones

	double imean = 0.0;
	double jmean = params->overall_mean;
	int ik = ov->stats[0].image;
	if (params->use_mean) {
	  imean = ov->stats[0].mean *
	    params->results[ik].mult +
	    params->results[ik].add;
	}
	double istd = 1.0;
	double jstd = params->overall_stdev;
	if (params->use_stdev) {
	  istd = ov->stats[0].stdev *
	    params->results[ik].mult;
	}

	// Combine them into a single error metric
	//!!!! DO THIS CORRECTLY, WHATEVER THAT IS!!!!
	// Make sure to avoid divide by zero, which would
	// also indicate a degererate solution (0 stdev means
	// 0 contrast and a constant image).
	// Square each of the terms.
	// We use targets as the "average" since they don't move.

	double avgmean = jmean;
	double avgstd = jstd;
	if (fabs(avgmean) < 1e-6)
	  avgmean = 1e-6;		// avoid divide by 0
	if (fabs(avgstd) < 1e-6)
	  avgstd = 1e-6;
	err = SQR((imean - jmean) / avgmean) +
	  SQR((istd - jstd) / avgstd);

	//!!!! err *= ov->n_pixels;		// weight by size of overlap
	//!!!! total_pixels += ov->n_pixels;
	total_pixels += 1;
	break;
      }

    default:
      snprintf(msg, BUF_LEN, "Warning: Unknown overlap type %d. Ignored", ov->type);
      zvmessage(msg, "");
      err = 0.0;
      break;
    }

    if (err > 0.0) {
      if (err > params->max_error) {
	params->max_error = err;
	params->who = k;
      }
    }
    else {
      err = 0.0;
    }

    //!!!!params->overlaps[k].error = err/ov->n_pixels;
    params->overlaps[k].error = err;
    sum += err;

  }

  if (total_pixels != 0)
    sum /= total_pixels;	// adjust weighting

  // Now add in a factor to keep the overall solution from drifing.
  // The idea is that the average of all multiplicative corrections
  // should be 1.0, and the average of all adds should be 0.0.  If
  // that's not the case, we're "drifting" overall in brightness.

  if (params->drift_wt[0] != 0.0 || params->drift_wt[1] != 0.0) {
    double avg_mult = 0.0;
    double avg_add = 0.0;
    for (int i = 0; i < params->nids; i++) {
      avg_mult += params->results[i].mult;
      avg_add += params->results[i].add;
    }
    if (params->nids != 0) {
      avg_mult /= params->nids;
      avg_add /= params->nids;
    }

    double mult_drift = 0.0;
    if (params->adjust_linear || params->adjust_mult) {
      mult_drift = SQR(fabs(avg_mult - 1.0)) * params->drift_wt[0];
    }
    double add_drift = 0.0;
    if (params->adjust_linear || params->adjust_add) {
      add_drift = SQR(fabs(avg_add - 0.0)) * params->drift_wt[1];
    }

    sum += (mult_drift + add_drift);
  }

  sum += inertia_wt;

  return (sum+.0001);
}

////////////////////////////////////////////////////////////////////////
// Write out the brightness corrections table in XML format.
////////////////////////////////////////////////////////////////////////
void save_brightness(char *name, int n, PigFileModel *files[],
		     Results results[], char *solution_id,
		     int do_add, int do_mult, int use_hsi)
{
  FILE *fout;
  int i, count;
  const int BUF_LEN = 256;
  char msg[BUF_LEN];

  PigMission *m = PigMission::getMissionObject(files[0]);

  const char *mission = m->getMissionName();

  // Check to see if we want to use UniqueId2 in the file instead
  int use_uniqueid2 = false;
  char point_method[256];
  char *value;
  zvp("POINT_METHOD", point_method, &count);
  if (count != 0) {
    value = PigModelBase::parseParamString(point_method, "USE_UNIQUEID2");
    if (value != NULL)
      use_uniqueid2 = true;
  }
  
  if ((fout = fopen(name, "w")) == NULL) {
    snprintf(msg, BUF_LEN, "Error opening pointing corrections file %s\n", name);
    zvmessage(msg, "");
    zabend();
  }

  fprintf(fout, "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");
  fprintf(fout, "<brightness_correction"); 
  if (mission != NULL)
    fprintf(fout, " mission=\"%s\"", mission);
  fprintf(fout, " version=\"1.0\">\n");
  fprintf(fout, "  <origination");
  fprintf(fout, " id=\"%s\"", solution_id);
  fprintf(fout, " institution=\"%s\" program=\"%s\">\n", "mipl", "marsbrt");

  fprintf(fout, "    <purpose>Brightness correction file for a mosaic</purpose>\n");
  fprintf(fout, "  </origination>\n");

  fprintf(fout, "  <priority>\n");
  fprintf(fout, "    <entry solution_id=\"%s\"/>\n", solution_id);
  fprintf(fout, "  </priority>\n"); 

  for (i=0; i < n; i++) {
    fprintf(fout, "  <brt_solution solution_id=\"%s\">\n", solution_id);

    const char *inst_id = files[i]->getInstrumentId();
    const char *image_id = files[i]->getImageId();
    const char *frame_id = files[i]->getFrameId();
    const char *filter = files[i]->getFilterNumber();
    char unique_id[33];
    strcpy(unique_id, "");
    if (use_uniqueid2)
      files[i]->getUniqueId2(unique_id);
    else
      files[i]->getUniqueId(unique_id);

    fprintf(fout, "    <image");
    if (filter != NULL)
      fprintf(fout, " filter=\"%s\"", filter);
    if (frame_id != NULL)
      fprintf(fout, " frame_id=\"%s\"", frame_id);
    if (image_id != NULL)
      fprintf(fout, " image_id=\"%s\"", image_id);
    if (inst_id != NULL)
      fprintf(fout, " instrument=\"%s\"", inst_id);
    if (strcmp(unique_id, "") != 0)
      fprintf(fout, " unique_id=\"%s\"", unique_id);
    fprintf(fout, "/>\n");

    //!!!! Only one correction type currently supported....

    if (use_hsi)
      fprintf(fout, "    <correction type=\"HSI_LIN\">\n");
    else
      fprintf(fout, "    <correction type=\"LINEAR\">\n");

    if (do_add) {
      fprintf(fout, "      <parameter id=\"ADD\" value=\"%lf\"/>\n",
	      results[i].add);
    }
    if (do_mult) {
      fprintf(fout, "      <parameter id=\"MULT\" value=\"%lf\"/>\n",
	      results[i].mult);
    }

    fprintf(fout, "    </correction>\n");
    fprintf(fout, "  </brt_solution>\n");
  }
  fprintf(fout, "</brightness_correction>\n");
  fclose(fout);
}

////////////////////////////////////////////////////////////////////////
// Take the parameters from Results and pack them into a single Pzero array
// for use by amoeba.  Also put the "length scale" into lambda.  Skip
// reference images, so they are not adjusted.
////////////////////////////////////////////////////////////////////////

void pack_params(double Pzero[], double lambda[], int &ndim,
		 ObjectiveParams *params)
{

  ndim = 0;

  if (params->adjust_linear || params->adjust_mult) {
    for (int i=0; i < params->nids; i++) {
      if (params->refimg[i])
	continue;		// skip this one
      Pzero[ndim] = params->results[i].mult;
      lambda[ndim] = params->lambda_mult;
      ndim++;
    }
  }
  if (params->adjust_linear || params->adjust_add) {
    for (int i=0; i < params->nids; i++) {
      if (params->refimg[i])
	continue;		// skip this one
      Pzero[ndim] = params->results[i].add;
      lambda[ndim] = params->lambda_add;
      ndim++;
    }
  }
}

////////////////////////////////////////////////////////////////////////
// Take the a packed array Pzero and extract results from it.  Skip
// reference images.
//
// If inertia is given, then the deltas between the current and orig
// params are computed, multiplied by inertia, and summed to become the
// function return.  This provides an incentive for the parameters to
// "stay in place", or an inertia.  It is computed here simply because
// this is where we have the parameters readily available.
////////////////////////////////////////////////////////////////////////

double unpack_params(double Pzero[], int ndim, ObjectiveParams *params)
{
  double weight = 0.0;
  int nctr = 0;
  int j = 0;			// inertia counter

  if (params->adjust_linear || params->adjust_mult) {
    for (int i = 0; i < params->nids; i++) {
      if (params->refimg[i])
	continue;		// skip this one
      if (nctr >= ndim) {		// oops!  Ran out of params!
	zvmessage("Internal error:  Not enough parameters in unpack_params (mult)!", "");
	zabend();
      }
      params->results[i].mult = Pzero[nctr++];

      // Compute the inertia weights.  Inertia[0] is always mult.

      if (params->inertia != NULL) {
	double wt = (*params->inertia)[0] *
	  (params->results[i].mult - params->orig_results[i].mult);
	weight += (wt * wt);
      }
    }
    j++;
  }

  if (params->adjust_linear || params->adjust_add) {
    for (int i = 0; i < params->nids; i++) {
      if (params->refimg[i])
	continue;		// skip this one
      if (nctr >= ndim) {		// oops!  Ran out of params!
	zvmessage("Internal error:  Not enough parameters in unpack_params (add)!", "");
	zabend();
      }
      params->results[i].add = Pzero[nctr++];

      // Compute the inertia weights.  Inertia[1] is always add.

      if (params->inertia != NULL) {
	double wt = (*params->inertia)[1] *
	  (params->results[i].add - params->orig_results[i].add);
	weight += (wt * wt);
      }
    }
    j++;
  }

  if (nctr != ndim) {			// oops!  Too many params!
    zvmessage("Internal error:  Too many parameters in unpack_params!", "");
    zabend();
  }

  return weight;
}
