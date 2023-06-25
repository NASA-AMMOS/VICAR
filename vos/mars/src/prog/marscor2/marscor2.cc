/* marscor2 */

#include "vicmain_c"

// mpi_id and mpi_numprocs are set up by vicmain_c if ENABLE_MPI is on.
// However, we use them below in some cases if MPI is off, too.  So,
// we define them here if MPI is not enabled.

#ifdef ENABLE_MPI
#include "mpi.h"
#else
int mpi_id = 0;
int mpi_numprocs = 1;
#include "mpi_fake.h"
#endif

int mpi_masterid=0;

#include "mars_support.h"

#include "gruen.h"
#include "mars_tiepoints.h"

#include "PigMission.h"
#include "PigFileModel.h"
#include "PigLabelModel.h"
#include "PigCameraModel.h"
#include "PigPointingModel.h"
#include "PigSurfaceModel.h"
#include "PigVector.h"
#include "RadiometryModel.h"
#include "PigCoordSystem.h"

#include "zvproto.h"

#include "mvec.h"		// some dynamic vector and matrix stuff

#include <math.h>
#include <time.h>

/* buffer sizes in main program */
#define MAX_INPUTS 2
#define MAX_NL 65536			/* doesn't matter, not used */
/* Should be MARS_MAX_NS but that busts memory on 32-bit machines. */
#define MAX_NS 7000
#define MAX_LEFT_AREA 31
#define MAX_RIGHT_AREA 81

#define SEED_TRIAL_MAX 100	// Max # of tries by a single call to get_seed
#define SEGMENT_MIN_SIZE 20	// Min size of a segment in pixels
#define SEED_AWAY_FROM_EDGE 4	// Keep seeds away from the edge by this much

#define EMPTY -32767
#define FAILED -32766		// Failed for unspecified reason
#define GOOD_CORR 0		// Really, anything > 0 is good
#define QUAL_FACTOR 10000.	/* scaling for qual array */

#ifndef TRUE
#define TRUE 1
#endif
#ifndef FALSE
#define FALSE 0
#endif

#ifndef MAX
#define MAX(A,B) ((A) > (B) ? (A) : (B))
#endif
#ifndef MIN
#define MIN(A,B) ((A) < (B) ? (A) : (B))
#endif

#ifdef DEBUG
#define DPR(x) { char msg[256]; x; zvmessage(msg,""); }
#else
#define DPR(x)
#endif

#include "mpi_timing_macro.h"
TIME_DEFINE(total);
TIME_DEFINE(setup);
TIME_DEFINE(main_work);
TIME_DEFINE(comm);
TIME_DEFINE(wait_barr);
TIME_DEFINE(write);
TIME_DEFINE(cor_seed);
TIME_DEFINE(cor_main);
TIME_DEFINE(cor_gore);
TIME_ARRAY_DEFINE(cor,20);

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
                          int &sl_window, int &el_window,
                          int &ss_window, int &es_window,
                          int &border_passes,
                          int nl_start_image, int nl_width_image,
                          int ns_start_image, int ns_width_image);


////////////////////////////////////////////////////////////////////////

int get_tiepoints(TiePoint *tiepoints, PigCameraModel **camera_in,
		  PigFileModel **file_models, PigCoordSystem *coord_sys,
		  PigSurfaceModel *surface, int &n_ties)
{
  int i;

  for (i=0; i < n_ties; i++) {
    tiepoints[i].left_image = 0;
    tiepoints[i].right_image = 1;
    tiepoints[i].quality = 0.0;
    tiepoints[i].interactive = 0;

    PigPoint origin, surf_pt;
    PigVector look;
    int hits;
    double line, samp;

    camera_in[0]->LStoLookVector(tiepoints[i].left_line,
				 tiepoints[i].left_sample, origin, look, coord_sys);
    hits = surface->intersectRay(origin, look, surf_pt);
    camera_in[1]->XYZtoLS(surf_pt, (hits<=0), &line, &samp, coord_sys);

    // Check to make sure projected point is inside right image

    if (file_models[1]->testPixelLocation(line, samp) == 0) {
      tiepoints[i].right_line = line;
      tiepoints[i].right_sample = samp;
      tiepoints[i].corrected_line = line;
      tiepoints[i].corrected_sample = samp;
    }
    else {			// nope, remove the point
      tiepoints[i].quality = -10.0;
    }
  }

  // Remove bad points (that didn't overlap the right image)

  mars_remove_bad_points(tiepoints, n_ties, -1.0);

  return 0;
}

////////////////////////////////////////////////////////////////////////
// Function to return the coordinates of the segment of the original image
// to be worked on for a given CPU number.
////////////////////////////////////////////////////////////////////////

int get_segment(int cpu_id, int proc_total,
		int nl_start_image, int nl_width_image,
		int ns_start_image, int ns_width_image,
		int &nl_start_segment, int &nl_width_segment,
		int &ns_start_segment, int &ns_width_segment,
		int segment_leave_out_border, int outside_border)
{

  int ns=0, nl=0, is, il=0, cpu=0;
  int del_s, del_l;
  int ncpu=proc_total - cpu_id;

  char msg[512];

  switch (proc_total){
  case 1:
    ns=1, nl=1; break;
  case 2:
    ns=2, nl=1; break;
  case 3:
    ns=3, nl=1; break;
  case 4:
    ns=2, nl=2; break;
  case 5:
    ns=5, nl=1; break;
  case 6:
    ns=3, nl=2; break;
  case 8:
    ns=4, nl=2; break;
  case 9:
    ns=3, nl=3; break;
  case 10:
    ns=5, nl=2; break;
  case 12:
    ns=4, nl=3; break;
  case 15:
    ns=5, nl=3; break;
  case 16:
    ns=4, nl=4; break;
  case 18:
    ns=6, nl=3; break;
  case 20:
    ns=5, nl=4; break;
  case 24:
    ns=6, nl=4; break;
  case 25:
    ns=5, nl=5; break;
  case 28:
    ns=7, nl=4; break;
  case 32:
    ns=8, nl=4; break;
  case 36:
    ns=6, nl=6; break;
  case 40:
    ns=8, nl=5; break;
  case 44:
    ns=11, nl=4; break;
  case 45:
    ns=9, nl=5; break;
  case 48:
    ns=12, nl=4; break;
  case 49:
    ns=7, nl=7; break;
  case 50:
    ns=10, nl=5; break;
  default:
    snprintf(msg, 512, "Support only 1,2,3,4,5,6,9,10,12,15,16,18,20,24,25,28,32,36,40,44,45,48,49,50 CPU subdivisions right now.");
    zvmessage(msg, "");
    zabend();
    break;
  }

  // Double check on the number of CPU's available
  if (ns*nl != proc_total) {
    snprintf(msg, 512, "Error in get_segment: There are %d CPUs and the subdivision is computes as %d x %d = %d", proc_total, ns, nl, nl*ns);
    zvmessage(msg, "");
    zabend();
  }

  // Figure out from the cpu_id which segment to pick

  for (is=0; (is<ns && cpu<ncpu); is++) {
    for (il=0; (il<nl && cpu<ncpu); il++) {
      cpu++;
    }
  }
  is--;
  il--;

  del_s=(ns_width_image-(2*ns*segment_leave_out_border))/ns;
  del_l=(nl_width_image-(2*nl*segment_leave_out_border))/nl;

  ns_start_segment = is * del_s + (2*is+1)*segment_leave_out_border;
  ns_width_segment = del_s;

  nl_start_segment = il * del_l + (2*il+1)*segment_leave_out_border;
  nl_width_segment = del_l;

  if (is==0) {
    ns_start_segment += outside_border;
    ns_width_segment -= outside_border;
  }
  if (is==ns-1) {
    ns_width_segment -= outside_border;
  }
  if (il==0) {
    nl_start_segment += outside_border;
    nl_width_segment -= outside_border;
  }
  if (il==nl-1) {
    nl_width_segment -= outside_border;
  }

  if (ns_width_segment < SEGMENT_MIN_SIZE) {
    snprintf(msg, 512, "Error in get_segment: The image is too small to be divided in into is=%d segments on the sample axis", is);
    zvmessage(msg, "");
    zabend();
  }

  if (nl_width_segment < SEGMENT_MIN_SIZE) {
    snprintf(msg, 512, "Error in get_segment: The image is too small to be divided in into il=%d segments on the line axis", il);
    zvmessage(msg, "");
    zabend();
  }

  DPR(snprintf(msg,256,"CPU=%d generates coordinates is=%d,il=%d with segment (ns_start_segment,nl_start_segment)=(%d,%d) width (ns_width,nl_width)=(%d,%d) inclusive corner (%d,%d)",
	       cpu_id, is, il, ns_start_segment, nl_start_segment,
	       ns_width_segment, nl_width_segment,
	       ns_start_segment+ns_width_segment-1,
	       nl_start_segment+nl_width_segment-1));

  return 0;
}


////////////////////////////////////////////////////////////////////////
// Function to return the next seed given a particular CPU.
// The first seed is in the center of the segment.  Later seeds are
// randomly distributed within the segment.
//!!!! TBD: improve this; random is not particularly good...
////////////////////////////////////////////////////////////////////////

int get_seed(int &seed_trial, int nl_start_segment, int nl_width_segment,
	     int ns_start_segment, int ns_width_segment, int *seed,
	     TiePoint *tiepoints, PigCameraModel **camera_in,
	     PigFileModel **file_models, PigCoordSystem *coord_sys,
	     PigSurfaceModel *surface)
{
  int left_line, left_samp;

  DPR(snprintf(msg, 256,"CPU=%d nl_start_segment=%d, nl_width_segment=%d ns_start_segment=%d ns_width_frame=%d\n",
	       mpi_id, nl_start_segment, nl_width_segment,
	       ns_start_segment, ns_width_segment));

  // treat the first guess special

  if (seed_trial > SEED_TRIAL_MAX) {
    return 1;					// too many

  } else if (seed_trial>0) {
    //!!!! do something more systematic here eventually...
    double x = (1.0*rand())/RAND_MAX;
    double y = (1.0*rand())/RAND_MAX;
    left_line = nl_start_segment + SEED_AWAY_FROM_EDGE +
      (int) (x*(nl_width_segment-2*SEED_AWAY_FROM_EDGE));
    left_samp = ns_start_segment + SEED_AWAY_FROM_EDGE +
      (int) (y*(ns_width_segment-2*SEED_AWAY_FROM_EDGE));
    DPR(snprintf(msg, 256,"CPU=%d left_line=%d left_samp=%d random",
		 mpi_id, left_line, left_samp));

  } else {
    // pick the middle of the segment
    left_line = nl_start_segment + nl_width_segment / 2;
    left_samp = ns_start_segment + ns_width_segment / 2;
    DPR(snprintf(msg, 256,"CPU=%d left_line=%d left_samp=%d middle of segment",
		 mpi_id, left_line, left_samp));
  }
  seed_trial++;

  tiepoints[0].left_sample = left_samp;
  tiepoints[0].left_line   = left_line;

  int n_ties=1;
  get_tiepoints(tiepoints, camera_in, file_models, coord_sys, surface, n_ties);
  // mars_print_tiepoint_list(tiepoints, n_ties);

  seed[0] = (int)(tiepoints[0].left_line + 0.5);
  seed[1] = (int)(tiepoints[0].left_sample + 0.5);
  seed[2] = (int)(tiepoints[0].corrected_line + 0.5);
  seed[3] = (int)(tiepoints[0].corrected_sample + 0.5);
  DPR(snprintf(msg,256,"CPU=%d t.l=%g t.s=%g t.cl=%g t.cs=%g seed[0]=%d seed[1]=%d",
	       mpi_id, tiepoints[0].left_line, tiepoints[0].left_sample,
	       tiepoints[0].corrected_line, tiepoints[0].corrected_sample,
	       seed[0], seed[1]));

  return 0;
}

////////////////////////////////////////////////////////////////////////
// Do the correlation for a seed point
////////////////////////////////////////////////////////////////////////

int do_seed_correlation(int left_line, int left_samp, CorrelParams *p,
			int *seed, int seed_use)
{
  int status=0;
  // char msg[256];

  if (p->qual[left_line][left_samp] < GOOD_CORR)  // if seed failed earlier,
    p->qual[left_line][left_samp] = EMPTY;    // force a re-do

  // Do the initial correlation

  if (p->qual[left_line][left_samp] != EMPTY) {
    status = 1;
  }
  else {
    TIME_ARRAY_INCR_CNT(cor);
    TIME_START(cor_seed);

    /* Mode 3 (linear followed by amoeba) is always used for seed points */
    status = do_correlation(left_line, left_samp, 3,
			    TRUE, seed[2], seed[3], p);
    TIME_EVAL(cor_seed);
  }

  if (status == 1) {
    // snprintf(msg, 256, "Seed #%d already done", seed_use);
    // zvmessage(msg, "");
  }
  if (status == -1) {
    // snprintf(msg, 256, "Correlation failure for seed #%d patch", seed_use);
    // zvmessage(msg, "");
  }
  if (status == -2) {
    // snprintf(msg, 256, "Correlation quality for seed #%d patch too low", seed_use);
    // zvmessage(msg, "");
    p->qual[left_line][left_samp] = EMPTY;	// don't count it out for later
  }

  if (status == 0) {
    // snprintf(msg, 256, "CPU %d: %d iterations to find seed point:", mpi_id, 
    // zvmessage("Seed point:", "");
    // zvmessage("left l&s , initial right l&s, final right l&s, quality", "");
    // snprintf(msg, 256, "%d %d %d %d %f %f %f", left_line, left_samp,
    //       seed[2], seed[3],
    //       p->line_mask[left_line][left_samp],
    //       p->samp_mask[left_line][left_samp],
    //       p->qual[left_line][left_samp]/QUAL_FACTOR);
    // zvmessage(msg, "");
  }

  return status;
}

////////////////////////////////////////////////////////////////////////
// Do the correlation for every pixel in the window that we can.
////////////////////////////////////////////////////////////////////////

int do_main_correlation(int &left_line, int &left_samp,
			int &sl_window, int &el_window,
			int &ss_window, int &es_window,
			int &border_passes,
			int nl_start_segment, int nl_width_segment,
			int ns_start_segment, int ns_width_segment,
			CorrelParams *p, int mode, int limit_pts)
{
  int status=0;

  int done = FALSE;
  while (!done) {

    done = get_next_point(left_line, left_samp, sl_window, el_window,
			  ss_window, es_window, border_passes,
			  nl_start_segment, nl_width_segment,
			  ns_start_segment, ns_width_segment);

    if (!done) {

      if (p->qual[left_line][left_samp] != EMPTY) {
	status = 1;
      }
      else {
	TIME_ARRAY_INCR_CNT(cor);
	TIME_START(cor_main);
	status = do_correlation(left_line, left_samp, mode,
				FALSE, 0, 0, p);

	TIME_EVAL(cor_main);
      }
      if (status == 1)
	continue;			/* skip point */

      if (status < 0) {	/* bad correlation, try again in gore code */
	if (status == -2)	// don't count it out for later
	  p->qual[left_line][left_samp] = EMPTY;
	continue;
      }

      if (p->npts >= limit_pts)
	done = TRUE;		/* ran out of points */
    }
  }
  return status;
}

////////////////////////////////////////////////////////////////////////
// Go through the window and fill up any gores we can
////////////////////////////////////////////////////////////////////////

int do_gore_correlation(int &sl_window, int &el_window,
			int &ss_window, int &es_window, CorrelParams *p, int mode)
{
  int status=0;
  int count;
  int left_line, left_samp;
  int i, j;
  char msg[256];

  do {
    count = 0;

    for (j=sl_window; j < el_window; j++) {
      for (i=ss_window; i < es_window; i++)  {

	left_line = j;
	left_samp = i;

	if (p->qual[left_line][left_samp] != EMPTY) {
	  status = 1;
	  continue;
	}
	else {
	  TIME_ARRAY_INCR_CNT(cor);
	  TIME_START(cor_gore);
	  status = do_correlation(left_line, left_samp, mode,
				  FALSE, 0, 0, p);
	  TIME_EVAL(cor_gore);
	}

	if (status == 1)
	  continue;
	if (status < 0) {
	  if (status == -2)		// failed due to quality setting
	    p->qual[left_line][left_samp] =
	      - p->qual[left_line][left_samp];
	  else			// failed for other reason
	    p->qual[left_line][left_samp] = FAILED;
	  continue;
	}

	count += 1;
      }
    }
#if 0
    if (mpi_numprocs > 1)
      snprintf(msg, 256, "CPU %d: gores found in last pass: %d", mpi_id, count);
    else
      snprintf(msg, 256, "gores found in last pass: %d", count);
    zvmessage(msg, "");
#endif

  } while (count > 0);		/* find more missing points */

  if (mpi_numprocs > 1)
    snprintf(msg, 256,"CPU %d: %d tiepoints acquired after gore filling",
	     mpi_id, p->npts);
  else
    snprintf(msg, 256,"%d tiepoints acquired after gore filling", p->npts);
  zvmessage(msg, "");

  return status;
}


////////////////////////////////////////////////////////////////////////
// Transfer correlation data to arrays for transmission to the master
////////////////////////////////////////////////////////////////////////

int copy_data_to_communicator_matrices(mve_simatrix *qual_ptr,
				       mve_dmatrix *line_mask_ptr, mve_dmatrix *samp_mask_ptr,
				       CorrelParams *p,
				       int nl_start_segment, int nl_width_segment,
				       int ns_start_segment, int ns_width_segment)
{
  mve_simatrix  qual     =*qual_ptr;
  mve_dmatrix   line_mask=*line_mask_ptr;
  mve_dmatrix   samp_mask=*samp_mask_ptr;
  int l,s,l_shift,s_shift;
  // char msg[256];

  DPR(snprintf(msg,256,"CPU=%d tries to perform mve_check_simatrix with (nl*ns)=(%d,%d)=%d",
	       mpi_id, nl_width_segment, ns_width_segment,
	       nl_width_segment*ns_width_segment));

  qual = mve_check_simatrix (qual, nl_width_segment,ns_width_segment);
  line_mask = mve_check_dmatrix(line_mask, nl_width_segment,ns_width_segment);
  samp_mask = mve_check_dmatrix(samp_mask, nl_width_segment,ns_width_segment);

  for (l=0, l_shift=nl_start_segment; l<nl_width_segment; l++, l_shift++) {
    for (s=0,s_shift=ns_start_segment; s<ns_width_segment; s++,s_shift++) {

      qual[l][s]      = p->qual     [l_shift][s_shift];
      samp_mask[l][s] = p->samp_mask[l_shift][s_shift];
      line_mask[l][s] = p->line_mask[l_shift][s_shift];
    }
  }
  *qual_ptr      = qual;
  *line_mask_ptr = line_mask;
  *samp_mask_ptr = samp_mask;

  return 0;
}

////////////////////////////////////////////////////////////////////////
// Transfer correlation data from arrays after reception by the master
////////////////////////////////////////////////////////////////////////

int copy_communicator_matrices_to_data(mve_simatrix qual,
				       mve_dmatrix line_mask, mve_dmatrix samp_mask,
				       CorrelParams *p,
				       int nl_start_segment, int nl_width_segment,
				       int ns_start_segment, int ns_width_segment)
{
  int l, s, l_shift, s_shift;
  mve_sivectr  qv = qual[0];
  mve_dvectr   sv = samp_mask[0];
  mve_dvectr   lv = line_mask[0];

  for (l=0, l_shift=nl_start_segment; l<nl_width_segment; l++, l_shift++) {
    for (s=0,s_shift=ns_start_segment; s<ns_width_segment; s++,s_shift++) {

      p->qual     [l_shift][s_shift] = *qv; qv++;
      p->samp_mask[l_shift][s_shift] = *sv; sv++;
      p->line_mask[l_shift][s_shift] = *lv; lv++;
    }
  }

  return 0;
}

////////////////////////////////////////////////////////////////////////
// Write all the data to the output files
////////////////////////////////////////////////////////////////////////

int marscorr_write_data(PigFileModel* file_models[], int nids, CorrelParams *p, const char* mission)
{
  // int status;
  int j,i,count;
  // LblImageData_typ ImageData;
  int band;

  double buffer[MAX_NS];

  // write line offset

  int unit_out;
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
  // We need mission object to write out output label.
  PigMission *m = PigMission::getMissionObject(mission);
  if (m) {
    PigLabelModel *labelModel_0 = NULL;
    labelModel_0 = m->createLabelModel(unit_out);

    // Write output label
    if (labelModel_0) {
      if (num_bands == 2){  // single two-banded image
	labelModel_0->setDisparity(file_models, file_models[1], nids, 
				   "DISPARITY_MAP");
      }else  // two 1-banded images
	labelModel_0->setDisparity(file_models, file_models[1], nids, 
				   "DISPARITY_LINE_MAP");
    }
  }

  // Write number of CPUs to history label

  zladd(unit_out, "history", "num_cpus", &mpi_numprocs, "format", "int",NULL);

  // Originally this modified p->line_mask in place.  But since the
  // write routine may be called twice, that's not a very good idea...

  for (j=0; j < p->nlin[0]; j++) {
    for (i=0; i < p->nsin[0]; i++) {
      if (p->qual[j][i] < GOOD_CORR)
	buffer[i] = 0.;
      else
	buffer[i] = p->line_mask[j][i] + 1.0;	// make coords 1-based
    }
    zvwrit(unit_out, buffer, "LINE", j+1, "BAND", band, NULL);
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
	labelModel_1->setDisparity(file_models, file_models[1], nids, 
				   "DISPARITY_SAMPLE_MAP");
    }
  }

  // Originally this modified p->samp_mask in place.  But since the
  // write routine may be called twice, that's not a very good idea...

  for (j=0; j < p->nlin[0]; j++) {
    for (i=0; i < p->nsin[0]; i++) {
      if (p->qual[j][i] < GOOD_CORR)
	buffer[i] = 0.;
      else
	buffer[i] = p->samp_mask[j][i] + 1.0;	// make coords 1-based
    }
    zvwrit(unit_out, buffer, "LINE", j+1, "BAND", band, NULL);
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
  return 0;
}

////////////////////////////////////////////////////////////////////////
// The entire parallel correlation algorithm...
////////////////////////////////////////////////////////////////////////

int perform_correlation(int proc_total, int nids, CorrelParams *p, TiePoint *tiepoints,
			PigCameraModel **camera_in, PigFileModel **file_models,
			PigCoordSystem *coord_sys, PigSurfaceModel *surface,
			const char* mission, int mode, int limit_pts, 
			int segment_leave_out_border, int outside_border, 
			int outer_trial_max, double fill_fraction)
{
  int nl_start_segment, nl_width_segment, ns_start_segment, ns_width_segment;
  int nl_start_image=0;
  int ns_start_image=0;
  int ns_width_image=p->nsin[0];
  int nl_width_image=p->nlin[0];
  int seed[4];
  int left_line, left_samp;
  int sl_window, ss_window, el_window, es_window;
  int border_passes;
  char msg[256];

  TIME_START(main_work);

  // get the particular segment of the overall image that this particular
  // CPU will work with

  get_segment(mpi_id, proc_total, nl_start_image, nl_width_image,
	      ns_start_image, ns_width_image,
	      nl_start_segment, nl_width_segment,
	      ns_start_segment, ns_width_segment,
	      segment_leave_out_border, outside_border);

  int done = FALSE;
  int done_outer = FALSE;
  int seed_trial = 0;
  int seed_use=0;
  int outer_trial=0;

  while (!done_outer) {
    TIME_ARRAY_START(cor);

    // int seeds_tried = 0;

    while (!done) {
      // need to get a seed for this particular segment
      done = get_seed(seed_trial, nl_start_segment, nl_width_segment,
		      ns_start_segment, ns_width_segment, seed, tiepoints,
		      camera_in,file_models,coord_sys, surface);

      left_line = seed[0];
      left_samp = seed[1];

      DPR(snprintf(msg, 256,"CPU=%d start the seed correlation", mpi_id));

      if (do_seed_correlation(left_line, left_samp, p, seed, seed_use)) {
	DPR(snprintf(msg, 256,"CPU=%d could not get a good correlation on the seed #%d",
		     mpi_id, seed_trial));
	continue;
      }

      // Got a good seed...

      snprintf(msg, 256,"CPU %d: %d iterations to find seed point:",
	       mpi_id, seed_trial);
      zvmessage(msg, "");
      snprintf(msg, 256,"L l/s=(%d,%d), init R l/s=(%d,%d), final R l/s=(%f,%f), quality=%f",
	       left_line, left_samp,
	       seed[2], seed[3],
	       p->line_mask[left_line][left_samp],
	       p->samp_mask[left_line][left_samp],
	       p->qual[left_line][left_samp]/QUAL_FACTOR);
      zvmessage(msg, "");

      // Set window
      sl_window = left_line-1;
      el_window = left_line+1;
      ss_window = left_samp-1;
      es_window = left_samp+1;

      /*******************ACQUIRE TIEPOINTS ****************************/

      border_passes = 0;
      left_line = sl_window;
      left_samp = ss_window - 1;

      DPR(snprintf(msg, 256,"CPU=%d start the main correlation",mpi_id));

      do_main_correlation(left_line, left_samp, sl_window, el_window,
			  ss_window, es_window, border_passes,
			  nl_start_segment, nl_width_segment,
			  ns_start_segment, ns_width_segment,
			  p, mode, limit_pts);

      DPR(snprintf(msg,256,"CPU=%d finished main correlation with %d tiepoints",
		   mpi_id, p->npts));
      DPR(snprintf(msg, 256,"CPU=%d start the gore correlation", mpi_id));

      do_gore_correlation(sl_window, el_window, ss_window, es_window,
			  p, mode);

      DPR(snprintf(msg, 256,"CPU=%d finished gore correlation with %d tiepoints of (nl*ns)=(%d * %d)=%d total",
		   mpi_id, p->npts, nl_width_segment,ns_width_segment,
		   nl_width_segment*ns_width_segment));

      done = TRUE;
    }
    TIME_ARRAY_EVAL(cor);

    outer_trial++;
    if ((((int)(fill_fraction*nl_width_segment*ns_width_segment)) < p->npts)
	|| (outer_trial > outer_trial_max)) {

      DPR(snprintf(msg, 256,"CPU=%d finished with the %d iterations of maximum %d loops over the segments, achieved %g%%of required %g%% coverage",
		   mpi_id, outer_trial, OUTER_TRIAL_MAX,
		   100.0*p->npts/(nl_width_segment*ns_width_segment),
		   100.0*fill_fraction));

      done_outer = TRUE;
    }
    else {
      DPR(snprintf(msg, 256,"CPU=%d still running with the %d iterations of maximum %d loops over the segments, achieved %g%% of required %g%% coverage",
		   mpi_id, outer_trial, OUTER_TRIAL_MAX,
		   100.0*p->npts/(nl_width_segment*ns_width_segment),
		   100.0*fill_fraction));
    }
    seed_trial=0;
    done=FALSE;
  }

  DPR(snprintf(msg, 256,"CPU=%d done with the processing of an individual window",
	       mpi_id));
  TIME_EVAL_START(main_work,comm);

  // Communication protocol tags...
#define mpi_tag_qual 2
#define mpi_tag_line 4
#define mpi_tag_samp 8
#define mpi_tag_pnts 16

  if (mpi_id != mpi_masterid) {		// we're a slave...

    // need to package up the data of the local images and send it to
    // the master

    snprintf(msg, 256,"CPU %d: done, ready to send data.  %d tiepoints acquired (%g%% of segment)",
	     mpi_id, p->npts,
	     100.0*p->npts/(ns_width_segment*nl_width_segment));
    zvmessage(msg, "");

    mve_simatrix qual=NULL;
    mve_dmatrix line_mask=NULL, samp_mask=NULL;
    int ns = ns_width_segment;
    int nl = nl_width_segment;
    DPR(snprintf(msg, 256,"CPU=%d copy data to communicator matrices", mpi_id));

    copy_data_to_communicator_matrices(&qual, &line_mask, &samp_mask, p,
				       nl_start_segment, nl_width_segment,
				       ns_start_segment, ns_width_segment);

    DPR(snprintf(msg,256,"CPU=%d start sending data dimension (nl,ns)=(%d,%d)",
		 mpi_id, nl, ns));
    DPR(snprintf(msg,256,"CPU=%d start sending of data", mpi_id));

    MPI_Send(&qual[0][0], ns*nl, MPI_SHORT, mpi_masterid,
	     mpi_tag_qual, MPI_COMM_WORLD);

    MPI_Send(&line_mask[0][0], ns*nl, MPI_DOUBLE, mpi_masterid,
	     mpi_tag_line, MPI_COMM_WORLD);

    MPI_Send(&samp_mask[0][0], ns*nl, MPI_DOUBLE, mpi_masterid,
	     mpi_tag_samp, MPI_COMM_WORLD);

    MPI_Send(&p->npts, 1, MPI_INT, mpi_masterid,
	     mpi_tag_pnts, MPI_COMM_WORLD);

    DPR(snprintf(msg,256,
		 "CPU=%d done sending.  Has a total of %d tiepoints corresponding to %g%%",
		 mpi_id, p->npts,
		 100.0*p->npts/(ns_width_segment*nl_width_segment)));

    TIME_EVAL(comm);

  }
  else {					// we're the master...

    // The master needs to receive the data and copy them into the correct
    // location.  It must set aside the memory for the communicated
    // matrices.  So, we need to figure out the largest possible size
    // of these data matrices.

#ifdef MPI_TIMING
    mve_ivectr npoints = mve_Ivectr(mpi_numprocs);
    npoints[mpi_id] = p->npts;
#endif

    int cpu;
    int nl_max=0, ns_max=0;
    for (cpu=0; cpu < proc_total; cpu++) {
      get_segment(cpu, proc_total, nl_start_image, nl_width_image,
		  ns_start_image, ns_width_image,
		  nl_start_segment, nl_width_segment,
		  ns_start_segment, ns_width_segment,
		  segment_leave_out_border, outside_border);
      nl_max = MAX(nl_max, nl_width_segment);
      ns_max = MAX(ns_max, ns_width_segment);
    }

    DPR(snprintf(msg, 256,"CPU=%d tries to make an mve_Simatrix with (nl*ns)=(%d,%d)=%d",
		 mpi_id, nl_max, ns_max, nl_max*ns_max));
    mve_simatrix  qual=     mve_Simatrix(nl_max,ns_max);
    mve_dmatrix   line_mask=mve_Dmatrix (nl_max,ns_max);
    mve_dmatrix   samp_mask=mve_Dmatrix (nl_max,ns_max);

    int maxdatalen = nl_max * ns_max;
    int actualdatalength = 0;
    int data_rcvd = 1;
    int source = 0;
    int more_points = 0;
    MPI_Status mpi_status;

    snprintf(msg, 256,"CPU %d: ready to receive data from slaves", mpi_id);
    if (proc_total > 1)
      zvmessage(msg, "");

#define MAX_CPUS 2048
    int done_list[MAX_CPUS];			// max # of cpu's
    for (int i=1; i < MIN(proc_total, MAX_CPUS); i++)
      done_list[i] = 0;
    done_list[0] = 1;			// Master is done!

    while (data_rcvd < proc_total) {

      MPI_Recv(&qual[0][0], maxdatalen, MPI_SHORT, MPI_ANY_SOURCE,
	       mpi_tag_qual, MPI_COMM_WORLD, &mpi_status);
      source = mpi_status.MPI_SOURCE;

      if (source < MAX_CPUS)
	done_list[source] = 1;

      DPR(snprintf(msg, 256,"CPU=%d received qual data from CPU=%d",
		   mpi_id, source));

      MPI_Recv(&line_mask[0][0], maxdatalen, MPI_DOUBLE, source,
	       mpi_tag_line, MPI_COMM_WORLD, &mpi_status);

      MPI_Recv(&samp_mask[0][0], maxdatalen, MPI_DOUBLE, source,
	       mpi_tag_samp, MPI_COMM_WORLD, &mpi_status);

      MPI_Get_count(&mpi_status, MPI_DOUBLE, &actualdatalength);

      MPI_Recv(&more_points, 1, MPI_INT, source,
	       mpi_tag_pnts, MPI_COMM_WORLD, &mpi_status);

      snprintf(msg, 256,"CPU=%d done receiving from CPU=%d", mpi_id, source);
      zvmessage(msg, "");

      p->npts += more_points;
      data_rcvd++;

      snprintf(msg, 256,"Still running: %d CPU's", proc_total - data_rcvd);
      if (proc_total - data_rcvd < 10 && proc_total != data_rcvd) {
	strcat(msg, ", ID=");
	for (int i=0; i < proc_total; i++) {
	  if (!done_list[i]) {
	    char msg2[20];
	    snprintf(msg2, 20, "%d ", i);
	    strcat(msg, msg2);
	  }
	}
      }
      zvmessage(msg, "");

#ifdef MPI_TIMING
      npoints[mpi_status.MPI_SOURCE] = more_points;
#endif

      // get the correct segment addresses to place them properly

      get_segment(source, proc_total, nl_start_image, nl_width_image,
		  ns_start_image, ns_width_image,
		  nl_start_segment, nl_width_segment,
		  ns_start_segment, ns_width_segment,
		  segment_leave_out_border, outside_border);

      // check for the correct datalength

      if (ns_width_segment * nl_width_segment != actualdatalength) {

	snprintf(msg, 256, "CPU=%d received the wrong datalength set from CPU=%d!!!!!!",
		 mpi_id, source);
	zvmessage(msg, "");
	snprintf(msg, 256, "CPU=%d expected to get (nl*ns)=(%d * %d)=%d; did receive total=%d!!!!\n",
		 mpi_id, ns_width_segment, nl_width_segment,
		 ns_width_segment * nl_width_segment, actualdatalength);
	zvmessage(msg, "");
      }

      copy_communicator_matrices_to_data(qual, line_mask, samp_mask, p,
					 nl_start_segment, nl_width_segment,
					 ns_start_segment, ns_width_segment);
      DPR(snprintf(msg, 256,"CPU=%d is done with copying data to the target",
		   mpi_id));
    }

    if (proc_total > 1)
      zvmessage("Master done receiving data", "");

    TIME_EVAL_START(comm,write);

    marscorr_write_data(file_models, nids, p, mission);

    DPR(snprintf(msg, 256,"CPU=%d has a total of %d tiepoints corresponding to %g%%",
		 mpi_id, p->npts,100.0*p->npts/(ns_width_image*nl_width_image)));

#ifdef MPI_TIMING
    {
      char npointsfilename[100];
      int ini;
      snprintf(npointsfilename, 100, "timing_npoints_%d", mpi_numprocs);
      FILE *npoints_fp = fopen(npointsfilename, "a");
      fprintf(npoints_fp, "CPUs %d maxseed %d fillfract %g second_border %d total %d/%d fraction %g%%\n",
	      mpi_numprocs, outer_trial_max, fill_fraction,
	      segment_leave_out_border, p->npts,
	      ns_width_image * nl_width_image,
	      100.0*p->npts/(ns_width_image*nl_width_image));
      for (ini=0; ini<mpi_numprocs; ini++) {
	fprintf(npoints_fp," %d", npoints[ini]);
      }
      fprintf(npoints_fp, "\n");
      fclose(npoints_fp);
    }
    mve_rm_ivectr(&npoints);
#endif

  }

  return 0;
}

////////////////////////////////////////////////////////////////////////
// A PigMsgFunction that suppresses all printout less than Fatal severity.
// Used by slave nodes during parallel processing.
////////////////////////////////////////////////////////////////////////

void pig_msg_suppressor(char *msg, PigMsgSeverity severity, void *clientData)
{
  if (severity == PigMsgFatal)
    zvmessage(msg, "");
}

////////////////////////////////////////////////////////////////////////
// Main program
////////////////////////////////////////////////////////////////////////

void main44()
{
  int i, j /* , s */;
  int count, def;
  // int status;
  int unit_in[MAX_INPUTS];
  int nids;
  char msg[256];

  // User parameters

  int band;
  int max_area[2];
  int templ[2], search[2];
  int mode;
  int limit_pts;
  TiePoint *tiepoints = new (std::nothrow) TiePoint[MARS_MAX_TIEPOINTS];

  zvmessage("MARSCOR2 version 2020-03-18", "");

  if(tiepoints == NULL){
    zvmessage("Error in allocating memory for tiepoints array","");
    zabend();
  }

  int second_pass;
  int segment_leave_out_border;
  int outside_border;
  int outer_trial_max;
  double fill_fraction;

  int do_print;

  struct CorrelParams *p;
  // int left_line, left_samp;

  // int border_passes;
  // int sl_window, ss_window, el_window, es_window;

  // LblImageData_typ ImageData;

  // Pig stuff, needed for seed point generation

  char mission[64], instrument[64];

  // Inputs

  PigFileModel *file_models[MAX_INPUTS];
  PigCameraModel *camera_in[MAX_INPUTS];
  PigPointingModel *pointing_in[MAX_INPUTS];
  PigSurfaceModel *surface;
  int homogeneous_inputs = TRUE;
  // char segment_name[256];
  PigCoordSystem *coord_sys;

  TIME_START(total);
  TIME_START(setup);

  // Since all parallel machines' stdout/stderr get jumbled together on
  // the master, do_print is used to control whether or not that stuff
  // gets printed.  We want to do the printing only on the master node.
  // If you want the prints for debug reasons, set the flag.

  do_print = (mpi_id == 0);
  if (!do_print)
    PigModelBase::setDefaultMsgFunction(pig_msg_suppressor, NULL);

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

  zvp("MAX_TRIALS", &outer_trial_max, &count);    // Maximum number of trials
  zvparmd("FILL_FRACTION", &fill_fraction, &count, &def, 1, 0); 
  zvp("SECOND_PASS", &second_pass, &count);
  zvp("SEGMENT_BORDER", &segment_leave_out_border, &count);
  zvp("OUTSIDE_BORDER", &outside_border, &count);

  // Set the random-number seed, if desired

  int rand_seed;
  zvp("RAND_SEED", &rand_seed, &count);
  if (count > 0) {
    if (rand_seed != 0)
      srand((unsigned int)rand_seed);
    else
      srand((unsigned int)time(NULL));
  }

  ///////////////////////////////////////////////////////////////////////
  // Compute seed points on the fly (stolen from marsseedgen).
  // Get the input file list, and set up initial camera/pointing models
  // for each input.  Although we accept two and only two inputs, mars_setup
  // does lots of other nice things for us.

  mars_setup(nids, file_models, camera_in, pointing_in, surface,
	     NULL, coord_sys, mission, instrument, homogeneous_inputs,
	     MAX_NL, MAX_NS, MAX_INPUTS);

  // Get the coordinate system to use.
  coord_sys = surface->getCoordSystem();

  // end of seed generation set-up

  TIME_EVAL(setup);

  ///////////////////////////////////////////////////////////////////////

  perform_correlation(mpi_numprocs, nids, p, tiepoints,
		      camera_in, file_models, coord_sys, surface, mission, 
		      mode, limit_pts, segment_leave_out_border, outside_border,
		      outer_trial_max, fill_fraction); 
   
  TIME_START(wait_barr);
  MPI_Barrier(MPI_COMM_WORLD);
  TIME_EVAL(wait_barr);

  if (second_pass) {
    if (mpi_id == mpi_masterid) {
      zvmessage("Starting another single pass over the whole image", "");

      perform_correlation(1, nids, p, tiepoints,
			  camera_in, file_models, coord_sys, surface, mission, 
			  mode, limit_pts, segment_leave_out_border, outside_border,
			  outer_trial_max, fill_fraction);

      snprintf(msg, 256, "CPU=%d is done with the second pass through perform_correlation; got %d points",
	       mpi_id, p->npts);
      zvmessage(msg, "");
    }
    TIME_START(wait_barr);
    MPI_Barrier(MPI_COMM_WORLD);
    TIME_EVAL(wait_barr);

#ifdef MPI_TIMING
    {
      char npointsfilename[100];
      snprintf(npointsfilename, 100,"timing_npoints_%d", mpi_numprocs);
      FILE *npoints_fp = fopen(npointsfilename, "a");
      fprintf(npoints_fp, "total %d 2nd pass\n", p->npts);
      fclose(npoints_fp);
    }
#endif
  }

  TIME_ARRAY_REPORT(cor);

  TIME_REPORT_MPIID;
  TIME_REPORT_wCOUNT(cor_seed);
  TIME_REPORT_wCOUNT(cor_main);
  TIME_REPORT_wCOUNT(cor_gore);
  TIME_REPORT_END;

  TIME_EVAL(total);
  TIME_REPORT_MPIID;
  TIME_REPORT(total);
  TIME_REPORT(setup);
  TIME_REPORT(main_work);
  TIME_REPORT(comm);
  TIME_REPORT(write);
  TIME_REPORT(wait_barr);
  TIME_REPORT_END;
       
#ifdef MPI_TIMING
  if (mpi_id == mpi_masterid) {
    char npointsfilename[100];
    snprintf(npointsfilename, 100,"timing_summary");
    FILE *npoints_fp = fopen(npointsfilename, "a");
    fprintf(npoints_fp, "CPUs %d maxseed %d fillfract %g second_pass %d second_border %d mode %d totalpoints %d totaltime %g\n ",
	    mpi_numprocs, outer_trial_max, fill_fraction, second_pass,
	    segment_leave_out_border, mode, p->npts, auxtime_total);
    fclose(npoints_fp);
  }
#endif

  TIME_GATHER_REPORT(total);
  TIME_GATHER_REPORT(setup);
  TIME_GATHER_REPORT(main_work);
  TIME_GATHER_REPORT(comm);
  TIME_GATHER_REPORT(write);
  TIME_GATHER_REPORT(wait_barr);
    
  delete[] tiepoints;
   
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
  int max_qual, max_qual_line=0.0, max_qual_samp=0.0;
  int right_line, right_samp;
  double right_liner=0.0, right_sampr=0.0;
  int start_line, start_samp;
  double *left, *right;
  int nlw_right = p->nlw2;
  int nsw_right = p->nsw2;
  char msg[256];

  // Params for gruen

  double line_offset, samp_offset;
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

    right_line = (int) (right_liner - max_qual_line + left_line + 0.5);
    right_samp = (int) (right_sampr - max_qual_samp + left_samp + 0.5);
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

  p->qual[left_line][left_samp] = (short int) (quality*QUAL_FACTOR);

  if (quality < p->min_quality)
    return -2;

  if (p->npts < 10 && !seed) {
    snprintf(msg,256,"%d %d %d %d %f %f %f", left_line, left_samp,
	     right_line, right_samp,
	     right_line+line_offset, right_samp+samp_offset, quality);
    zvmessage(msg, "");
  }

  /* compute right image coordinates from offsets */

  p->line_mask[left_line][left_samp] = right_line + line_offset;
  p->samp_mask[left_line][left_samp] = right_samp + samp_offset;
  p->npts += 1;

  if ((p->npts % p->print_interval) == 0) {
    if (mpi_numprocs > 1)
      snprintf(msg,256,"CPU %d: %d tiepoints gathered so far",mpi_id,p->npts);
    else
      snprintf(msg, 256,"%d tiepoints gathered so far", p->npts);
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
		   int &sl_window, int &el_window,
                   int &ss_window, int &es_window,
		   int &border_passes,
		   int nl_start_image, int nl_width_image,
		   int ns_start_image, int ns_width_image)
{

  /* move from upper left to lower right */
  int ns_end_image = ns_start_image + ns_width_image+1;
  int nl_end_image = nl_start_image + nl_width_image+1;

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
	if (ss_window > ns_start_image)
	  ss_window -= 1;		/* expand window by 1 */
	if (sl_window > nl_start_image)
	  sl_window -= 1;
	if (el_window < nl_end_image - 1)
	  el_window += 1;
	if (es_window < ns_end_image - 1)
	  es_window += 1;

	if ((ss_window == ns_start_image) &&
	    (es_window == ns_end_image - 1) &&
	    (sl_window == nl_start_image) &&
	    (el_window == nl_end_image - 1))

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

  if ((samp == ns_start_image) ||
      (samp == ns_end_image - 1) ||
      (line == nl_start_image) ||
      (line == nl_end_image - 1)) {

    return get_next_point(line, samp, sl_window, el_window,
			  ss_window, es_window, border_passes,
			  nl_start_image, nl_width_image,
			  ns_start_image,  ns_width_image);
  }

  return FALSE;				/* not done yet */
}
