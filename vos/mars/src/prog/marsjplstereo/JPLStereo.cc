/******************************************************************************
*                                                                             *
*		JPL Stereo Vision code					      *
*									      *
*		Developed by the Machine Vision and Tracking Sensors Group    *
*		at the Jet Propulsion Laboratory			      *
*								  	      *
*									      *
*	For information contact:					      *
*									      *
*			Larry Matthies	lhm@robotics.jpl.nasa.gov	      *
*                      	Todd Litwin     litwin@robotics.jpl.nasa.gov          *
*			Mark Maimone	mark.maimone@jpl.nasa.gov	      *
*			Yalin Xiong	yx@robotics.jpl.nasa.gov	      *
*								  	      *
*                                       Updated: 16 Nov 1998                  *
*                                                                             *
*                                       Copyright (C) 1993, 1994, 1995, 1996, *
*                                                     1997, 1998              *
*                                       California Institute of Technology    *
*                                       All Rights Reserved                   *
*                                                                             *
******************************************************************************/

#include <math.h>
#include <stdlib.h>
#include <string.h>	// for memset
#include "cahvor.h"
#include "ErrHandle.h"
#include "JPLStereo.h"
#include "MDTypes.h"
#include "stereo.h"
/* #include "stereo_ppc.h" */
#include "float_matrix.h"
#include "mat3.h"
#include <netinet/in.h>	// for htonl()

#ifdef MSP 
#define ERR(x) (0x6050+(x))
#endif

#ifdef TIMING__
#include "Timing.h"
#define SHOW_TIME(str) (show_timing ? TIME(str) : 0)
#endif

// MEM_DEBUG was useful in diagnosing memory leaks on a VxWorks system
//#define MEM_DEBUG
//#define BLOB_DEBUG
//#define CMOD_DEBUG

#ifndef ABS
#define ABS(x)	((x) < 0 ? -(x) : (x))
#endif

// netinet/in.h redefines INT_MAX :-P

#ifdef INT_MAX
#undef INT_MAX
#endif
#define INT_MAX(x,y) (((x) > (y)) ? (x) : (y))

// You never want to check floating point numbers for equality directly,
// so use this epsilon value instead.

#ifndef EPSILON
#define EPSILON 1e-5
#endif

#include "real_helpers.h"

#ifndef PI
#define PI 3.1415926535897932384626433
#endif

// VxWorks has a special "logMsg" function that tags each message with a
// task-identifying string.  Fortunately its syntax is like printf, so
// just use printf on non-VxWorks systems.

#if defined(RTI_VXWORKS) || defined (__VXWORKS__) || defined (RTS_VXWORKS)
#include "logLib.h"
#define ZEROS6 ,0,0,0,0,0,0
#define ZEROS5 ,0,0,0,0,0
#define ZEROS4 ,0,0,0,0
#define ZEROS3 ,0,0,0
#define ZEROS2 ,0,0
#define ZEROS1 ,0
#else
#define logMsg printf
#define ZEROS6
#define ZEROS5
#define ZEROS4
#define ZEROS3
#define ZEROS2
#define ZEROS1
#endif


//////////////////////////////////////////////////////////////////////////////
//
//	CONSTRUCTORS
//
//////////////////////////////////////////////////////////////////////////////

JPLStereo::JPLStereo ()
{
  init ((unsigned char) 7, 0, 63, NULL);
}

JPLStereo::JPLStereo (JMemoryManager *mgr)
{
  init ((unsigned char) 7, 0, 63, mgr);
}

JPLStereo::JPLStereo (unsigned char corrWinSize)
{
  init (corrWinSize, 0, 63, NULL);
}

JPLStereo::JPLStereo (int minDis, int maxDis)
{
  init ((unsigned char) 7, minDis, maxDis, NULL);
}


JPLStereo::JPLStereo (unsigned char corrWinSize, int minDisparity,
			     int maxDisparity, JMemoryManager *mgr)
{
  init (corrWinSize, minDisparity, maxDisparity, mgr);
}


void JPLStereo::init (unsigned char corrWinSize, int minDisparity,
			     int maxDisparity, JMemoryManager *mgr)
{
	fixed_blob_filter_size = -4;	// Use the default 4% of image size
	subpixelBits = 4;	// 4-bit subpixel 
	rangeBits = 8;
	show_timing = 0;
	minDisp = minDisparity;
	maxDisp = maxDisparity; // pixel search range in full resolution
	rightHandDisparity = 0;	// Use left-hand by default; right-hand is
				// implemented in a space/time-expensive way
	mergeSqThreshold = 0.2;
	mergePixelThreshold = 1;
	uncertainty_threshold = 128;
	mm = mgr;

	// generating all maps

	// IMCOMPLETE_SEARCH means we're allowed to consider image
	// borders, even if the max disparity won't fit (as long as
	// some disparity values do fit within the image)

	options = SUBPIXEL_DISPARITY | REJECT_LRLOS | REJECT_BLOB 
	  | INCOMPLETE_SEARCH;
//	  | POST_WARP | THRESHOLD_UNCERTAINTY; 

#ifdef MEM_DEBUG
printf ( "Constructing JPLStereo: win=%d, disp=[%d:%d], options=%lx\n",
	 corrWinSize, minDisparity, maxDisparity, options);
#endif

	range_params_good = 0;

	use_sub = 0;

	// Window Sizes
	smoothWinSize = 3;
	dogWinSize1 = 9;
	dogWinSize2 = 1;
	matchWinSize = matchWinSizeY = corrWinSize;
	lrlosLimit = 1;
	blob_region_fraction = DEFAULT_BLOB_REGION_FRACTION;
	min_blob_regions_to_allocate = DEFAULT_MIN_BLOB_REGIONS_TO_ALLOCATE;
	xGradThreshold = (I_DISP_SCALE >> 1);
	yGradThreshold = (I_DISP_SCALE >> 1);
	overhangWindowFraction = DEFAULT_OVERHANG_WINDOW_FRACTION;
	overhangSigmaFraction = DEFAULT_OVERHANG_SIGMA_FRACTION;
	overhang_world_up[0] = overhang_world_up[1] =
	  overhang_world_up[2] = 0.0;

	// number of quadratic curves in warping approximation
	numQuadraCurves = 10;

	subDispPic = NULL;
	rangePic = NULL;
	obsPic = NULL;
	maskPic = NULL;

	leftRectPic = NULL;
	rightRectPic = NULL;
	leftRawRectPic = NULL;
	rightRawRectPic = NULL;
	leftRectCam = NULL;
	rightRectCam = NULL;
	leftOriginalCam = NULL;
	rightOriginalCam = NULL;
	rows_to_search = NULL;
	use_laplacian_in_disparity_space_image = 1;

	baseline = center_X = 0.0;
	MM[0] = MM[1] = MM[2] = MM[3] = MM[4] = 0.0;
	MM[5] = MM[6] = MM[7] = MM[8] = 0.0;
	AA[0] = AA[1] = AA[2] = CC[0] = CC[1] = CC[2] = 0.0;

	#ifdef MAC_PLATFORM
	debugWindow = NULL;
	#endif
#ifdef MEM_DEBUG
printf ( "  End of constructor:  options = 0x%lx\n", options);
printf ( "%s", params2s());
#endif
}



long JPLStereo::SetRowsToSearch (char *buff)
{
  if (buff == NULL)
    return PARAM_ERR;

#ifdef __unix
  unsigned int chars_parsed =
    s2range (buff, dflt_s2index, 0, 2048, &rows_to_search);
#else
  unsigned int chars_parsed = 0;
#endif

  if (chars_parsed < strlen (buff))
    printf ( "Only parsed first %d chars of \"%s\"!\n",
	     chars_parsed, buff);

  return NO_ERR;
} // JPLStereo::SetRowsToSearch



void JPLStereo::ShowRowsToSearch (FILE *fp)
{
  char buff[2000];

  if (fp == NULL)
    return;

#ifdef __unix
  (void) range2s (rows_to_search, buff, dflt_index2s);
#else
  strcpy (buff, "[Row specification only available on unix]");
#endif

  fprintf (fp, "%s\n", buff);
} // JPLStereo::ShowRowsToSearch





JPLStereo::~JPLStereo ()
{
  // We used to delete the static buffers here (and arguably still should
  // delete them under unix), but to accomodate VxWorks we just let them
  // linger on the heap between calls.

  if (leftRectCam) DELETE (mm, leftRectCam);
  if (rightRectCam) DELETE (mm, rightRectCam);
}



void
JPLStereo::SetOptions (long newopt)
{
  if (newopt & ~ALL_STEREO_OPTIONS)
    printf ( "SetOptions:  unknown options 0x%lx\n", newopt);
  else
    options |= newopt;
}



void
JPLStereo::ClearOptions (long newopt)
{
#ifdef MEM_DEBUG
printf ( "ClearOptions: starting with 0x%lx, ", options);
#endif
  if (newopt & ~ALL_STEREO_OPTIONS)
    printf ( "ClearOptions:  unknown options 0x%lx\n", newopt);
  else
    options &= ~newopt;
#ifdef MEM_DEBUG
printf ( "ending with 0x%lx\n", options);
#endif
}



long JPLStereo::EnableMaskPic (void)
{
  if (maskPic)
    return NO_ERR;

  maskPic = NEW(mm, "Stereo Pixel Mask Image") JPLPic(mm);

  if (maskPic)
    return NO_ERR;

  Warning ("Failed to enable mask picture!!\n");

  return INTERNAL_ERR;
}

JPLPic *JPLStereo::GetMaskPic (void)
{
  return maskPic;
}

JPLPic	*
JPLStereo::GetRectPic(int leftOrRight) 
{
  return (leftOrRight == LEFT_VIEW ? leftRectPic : rightRectPic);
}



JPLPic  *
JPLStereo::GetRawRectPic (int leftOrRight)
{
  return (leftOrRight == LEFT_VIEW) ? leftRawRectPic : rightRawRectPic;
}



JPLCamera *
JPLStereo::GetRectCam (int leftOrRight)
{
  return (leftOrRight == LEFT_VIEW ? leftRectCam : rightRectCam);
}



void
JPLStereo::SetRawRectPic (JPLPic *pic, int leftOrRight)
{
	if (leftOrRight == LEFT_VIEW)
		leftRawRectPic = pic;
	else
		rightRawRectPic = pic;
}



void
JPLStereo::SetRectPic (JPLPic *pic, int leftOrRight)
{
	if (leftOrRight == LEFT_VIEW)
		leftRectPic = pic;
	else
		rightRectPic = pic;
}



void
JPLStereo::SetRectCam (JPLCamera *cam, int leftOrRight)
{
	if (leftOrRight == LEFT_VIEW)
		leftRectCam = cam;
	else
		rightRectCam = cam;
}



// Set the uncertainty threshold, which will only be used if the
// THRESHOLD_UNCERTAINTY option has been set.

void
JPLStereo::SetUncertainty (long u) {
  	if (u < 0) u = 0;
	if (u > 256) u = 256;
 	uncertainty_threshold = u;
}



void
JPLStereo::ClearSubwindow ()
{
  JPLCamera *lcam = NULL, *rcam = NULL;

  if (use_sub == 0)
    return;

  if ((lcam = GetRectCam(LEFT_VIEW)) == NULL ||
      (rcam = GetRectCam(RIGHT_VIEW)) == NULL) {
    logMsg ("ClearSubwindow:  Missing rectified camera model, "
	    "cannot clear subwindow\n" ZEROS6);
    return;
  }

  // Remove the correction term for image coordinate translation from
  // the current camera models.

  lcam->TransformImageCoordinate(1.0, 1.0, (float) sub_up_left_col,
				 (float) sub_up_left_row);
  rcam->TransformImageCoordinate(1.0, 1.0, (float) sub_up_left_col,
				 (float) sub_up_left_row);

  // No need to reset sub_rows, sub_cols, because they're not used
  
  use_sub = 0;
  return;
}



int
JPLStereo::SetSubwindow (int upper_left_row, int upper_left_col, int s_rows,
			 int s_cols)
{
  JPLCamera *lcam= NULL, *rcam = NULL;

  if ((lcam = GetRectCam(LEFT_VIEW)) == NULL ||
      (rcam = GetRectCam(RIGHT_VIEW)) == NULL) {
    logMsg ("SetSubwindow:  Missing rectified camera model, "
	    "cannot set subwindow\n" ZEROS6);
    return INIT_ERR;
  }

  // Setting of the subwindow means you have to update the camera model
  // to reflect the new interpretation of image coordinates

  lcam->TransformImageCoordinate(1.0, 1.0, (float) -upper_left_col,
				 (float) -upper_left_row);
  rcam->TransformImageCoordinate(1.0, 1.0, (float) -upper_left_col,
				 (float) -upper_left_row);
  use_sub = 1;

  sub_up_left_row = upper_left_row;
  sub_up_left_col = upper_left_col;
  sub_rows = s_rows;
  sub_cols = s_cols;
  return NO_ERR;
}


long
JPLStereo::AlignCameras (JPLCamera *leftCam, JPLCamera *rightCam,
			 long rows, long cols, long warp_rows, long warp_cols)
{
  // first align the camera
  double A[3], H[3], V[3], center[2], scale[2], theta;

#ifdef CMOD_DEBUG
  fprintf (stdout, "JPLStereo: INPUT LEFT Camera Model:  ");
  leftCam->PrintCameraModelSummary();
  fprintf (stdout, "JPLStereo: INPUT RIGHT Camera Model:  ");
  rightCam->PrintCameraModelSummary();
#endif

  /* Fixes PR #Z79871 */
  if (leftCam == NULL || rightCam == NULL ||
      EQ (mag3(leftCam->A), 0.0) || EQ(mag3(rightCam->A), 0.0)) {
    fprintf (stderr, "AlignCameras:  Cannot align bogus camera models!\n");
    if (leftCam == NULL)
      fprintf (stderr, "   leftCam is NULL\n");
    else
      fprintf (stderr, "   leftCam->A is {%f, %f, %f}\n",
	       leftCam->A[0], leftCam->A[1], leftCam->A[2]);
    if (rightCam == NULL)
      fprintf (stderr, "   rightCam is NULL\n");
    else
      fprintf (stderr, "   rightCam->A is {%f, %f, %f}\n",
	       rightCam->A[0], rightCam->A[1], rightCam->A[2]);

    return PARAM_ERR;
  }

  if (leftCam->modelType == CAHV_MODEL) {
    cmod_cahv_warp_models
      ( leftCam->C,  leftCam->A,  leftCam->H,  leftCam->V,
       rightCam->C, rightCam->A, rightCam->H, rightCam->V, A, H, V,
	&scale[0], &center[0],
	&scale[1], &center[1], &theta);
  } else if (leftCam->modelType == CAHVOR_MODEL) {
    int dims[2], warped_dims[2];
    dims[0] = cols;
    dims[1] = rows;
    /* HACK!!!  No longer using the warp_rows / cols params -- Aug 2000 */
    warped_dims[0] = warp_cols;
    warped_dims[1] = warp_rows;
    warp_cols = warped_dims[0];
    warp_rows = warped_dims[1];

    // Debugging, so eliminated the warped_dims parameter.

    cmod_cahvor_warp_models (leftCam->C, leftCam->A, leftCam->H,
				 leftCam->V, leftCam->O, leftCam->R,
				 rightCam->C, rightCam->A, rightCam->H,
				 rightCam->V, rightCam->O, rightCam->R,
				 true, dims, dims, A, H, V,
				 &scale[0], &center[0],
				 &scale[1], &center[1], &theta);
  } else if (leftCam->modelType == CAHVORE_MODEL) {

    cmod_cahvore_warp_models (cols, rows,
				  (int) *leftCam->e_type,
				  *leftCam->linearity_parm,
				  leftCam->C, leftCam->A, leftCam->H,
				  leftCam->V, leftCam->O, leftCam->R,
				  leftCam->E,
				  cols, rows,
				  (int) *rightCam->e_type,
				  *rightCam->linearity_parm,
				  rightCam->C, rightCam->A, rightCam->H,
				  rightCam->V, rightCam->O, rightCam->R,
				  rightCam->E,
				  /* Use 80% of 180degrees as max fov */
				  0.8 * M_PI, FOV_DEFAULT,
				  /* output rows/cols in xelts, yelts order */
				  cols, rows,
				  A, H, V,
				  &scale[0], &center[0],
				  &scale[1], &center[1], &theta);
  } else {
    printf ( "*** Unknown camera model %ld, cannot rectify\n",
	     leftCam->modelType);
    return PARAM_ERR;
  }

  if (leftRectCam == NULL)
    leftRectCam = NEW(mm, "leftRectCam") JPLCamera(mm);
  if (rightRectCam == NULL)
    rightRectCam = NEW(mm, "rightRectCam") JPLCamera(mm);
  
  leftRectCam->InitJPLCamera (CAHV_MODEL, &rows, &cols, leftCam->C, A,
			      H, V, NULL, NULL, center, scale, &theta,
			      NULL, NULL);
  rightRectCam->InitJPLCamera (CAHV_MODEL, &rows, &cols, rightCam->C, A,
			       H, V, NULL, NULL, center, scale, &theta,
			       NULL, NULL);

#ifdef CMOD_DEBUG
  fprintf (stdout, "JPLStereo: RECTIFIED LEFT Camera Model:  ");
  leftRectCam->PrintCameraModelSummary();
  fprintf (stdout, "JPLStereo: RECTIFIED RIGHT Camera Model:  ");
  rightRectCam->PrintCameraModelSummary();
#endif

  return NO_ERR;
} // JPLStereo::AlignCameras


long JPLStereo::ComputeDisparityRange (double min_range, double max_range,
				       int *min_disp, int *max_disp)
{
  return ComputeDisparityRange (leftRectCam, rightRectCam,
				min_range, max_range,
				min_disp, max_disp);
} // JPLStereo::ComputeDisparityRange



long JPLStereo::ComputeDisparityRange (JPLCamera *lrectcam,
				       JPLCamera *rrectcam,
				       JPLPic *leftpic, JPLPic *rightpic,
				       double min_range, double max_range,
				       int *min_disp, int *max_disp)
{
  float rscale = 1.0, cscale = 1.0;
  JPLCamera leftCam(mm), rightCam(mm);

  if (lrectcam == NULL || rrectcam == NULL)
    return PARAM_ERR;
  if (leftpic == NULL || rightpic == NULL) {
    Warning ("ComputeDisparityRange: NULL pic, just using camera model\n");
    return ComputeDisparityRange (lrectcam, rrectcam, min_range, max_range,
				  min_disp, max_disp);
  }
  lrectcam->CopyCamera (&leftCam);
  rrectcam->CopyCamera (&rightCam);

  if (leftCam.rows > 0 && leftpic->rows > 0) {
    rscale = ((float) leftpic->rows) / ((float) leftCam.rows);
  }
  if (leftCam.cols > 0 && leftpic->cols > 0) {
    cscale = ((float) leftpic->cols) / ((float) leftCam.cols);
  }
  leftCam.ResizeImageCoordinate (rscale, cscale);

  if (rightCam.rows > 0 && rightpic->rows > 0) {
    rscale = ((float) rightpic->rows) / ((float) rightCam.rows);
  }
  if (rightCam.cols > 0 && rightpic->cols > 0) {
    cscale = ((float) rightpic->cols) / ((float) rightCam.cols);
  }
  rightCam.ResizeImageCoordinate (rscale, cscale);

  return ComputeDisparityRange (&leftCam, &rightCam, min_range, max_range,
				min_disp, max_disp);
}

  

long JPLStereo::ComputeDisparityRange (JPLCamera *lrectcam,
				       JPLCamera *rrectcam,
				       double min_range, double max_range,
				       int *min_disp, int *max_disp)
{
  if (lrectcam == NULL || rrectcam == NULL)
    return PARAM_ERR;


  if (lrectcam->modelType != CAHV_MODEL ||
      rrectcam->modelType != CAHV_MODEL)
    fprintf (stderr, "WARNING!! ComputeDisparityRange given %ld and %ld "
	     "models (not CAHV %d)\n",
	     lrectcam->modelType, rrectcam->modelType, CAHV_MODEL);

  double baseline_dist = sqrt (SQ (lrectcam->C[0] - rrectcam->C[0]) +
			       SQ (lrectcam->C[1] - rrectcam->C[1]) +
			       SQ (lrectcam->C[2] - rrectcam->C[2]));
  double focal_length = lrectcam->scale[0];

  if (!EQ (lrectcam->scale[0], rrectcam->scale[0]))
    fprintf (stderr,
	     "WARNING!! Left and Right focallengths (%g,%g) "
	     "differ, using left\n", lrectcam->scale[0], rrectcam->scale[0]);

  return ComputeDisparityRange (baseline_dist, focal_length,
				min_range, max_range,
				min_disp, max_disp);
} // JPLStereo::ComputeDisparityRange



long JPLStereo::ComputeDisparityRange (double baseline_dist,
				       double focal_length,
				       double min_range, double max_range,
				       int *min_disp, int *max_disp)
{
  int mind, maxd;

  mind = EQ(max_range, 0.0) ? 0 :
    (int) (baseline_dist * focal_length / max_range);

  // Need to search at least one more disparity, else the sanity check will
  // throw out a disparity at the extreme end of the search range.

  mind--;
  if (mind < 0)
    mind = 0;

  maxd = EQ(min_range, 0.0) ? 255 :
    (int) (0.99 + baseline_dist * focal_length / min_range);

  // This version of the code only computes 8 bit disparities, so
  // don't allow the returned value to exceed this maximum

  if (maxd > 255) {
    fprintf (stderr,
	     "WARNING!! Near range of %gm implies disparity %d; using 255!\n",
	     min_range, maxd);
    maxd = 255;
  }

  if (min_disp)
    *min_disp = mind;
  if (max_disp)
    *max_disp = maxd;

  return NO_ERR;
} // JPLStereo::ComputeDisparityRange




/* VxWorks allow maximal of seven parameters when timing */

long
JPLStereo::TimingMatchStereo (JPLPic *leftPic, JPLPic *rightPic,
			      JPLCamera *leftCam, JPLCamera *rightCam,
			      int pyramidLevel, int fullMinDisp,
			      int fullMaxDisp)
{
  return MatchStereo (leftPic, rightPic, leftCam, rightCam,
		      pyramidLevel, fullMinDisp, fullMaxDisp, 0);
}




long			
JPLStereo::MatchStereo (JPLPic *leftPic, JPLPic *rightPic, 
			JPLCamera *leftCam, JPLCamera *rightCam,
			int pyramidLevel, int fullMinDisp,
			int fullMaxDisp, unsigned char bound)
{
  range_params_good = 0;
  if (leftPic)
    return MatchStereo (leftPic, rightPic, leftCam, rightCam,
			pyramidLevel, leftPic->cols, leftPic->rows,
			fullMinDisp, fullMaxDisp, bound);
  else {
    printf ( "MatchStereo:  empty leftPic!\n");
    return INIT_ERR;
  }
}





// To save space, we avoid creating the complete range map.  Instead, we
// call this function to compute some often-used parameters, which can be
// used to generate range values for each needed pixel on the fly.

long JPLStereo::CreateRangeParams ()
{
  if (leftRectCam == NULL || rightRectCam == NULL) {
    FatalErr ("No camera models loaded, cannot generate range\n");
    return INIT_ERR;
  } /* if */

  float x_scale = 0.5 * (leftRectCam->scale[0] + rightRectCam->scale[0]);
  // Compute baseline
  
  int i;
  baseline = 0.0;
  for (i = 3; i--; )
    baseline += SQ(leftRectCam->C[i] - rightRectCam->C[i]);
  baseline = sqrt(baseline) * x_scale * ((float) I_DISP_SCALE);
  
  // Now "baseline" is really baseline times horizontal focal length
  // (in pixels), times the subpixel scale factor required in the
  // calculation below.
  
  if (ABS(baseline) < 0.00001)
    printf (
	     "WARNING!! (Baseline*focal length) near zero (%g)\n",
	     baseline);

  center_X = (rightRectCam->center[0] - leftRectCam->center[0]) *
    ((float) I_DISP_SCALE);

  if (rightHandDisparity) {
    rightRectCam->M2D_3D(MM);
    for (i = 0; i < 3; i++) {
      AA[i] = rightRectCam->A[i];
      CC[i] = rightRectCam->C[i];
    } // for
  } else {
    leftRectCam->M2D_3D(MM);
    for (i = 0; i < 3; i++) {
      AA[i] = leftRectCam->A[i];
      CC[i] = leftRectCam->C[i];
    }
  }

  // Set the flag indicating that these cached coefficient values are good

  range_params_good = 1;

  return NO_ERR;
}



// Convert the disparity value at a given pixel into float X,Y,Z values.
// Note the row and col are the required inputs, whereas the disparity
// is assumed to lie within the current subDispPic at that location.

long
JPLStereo::Disparity2FloatXYZRange (long row, long col, float *x,
				    float *y, float *z)
{
  float tx, ty, tz;
  float scale;

  if (subDispPic == NULL) {
    FatalErr ("Subpixel Disparity Map not initialized\n");
    return INIT_ERR;
  }

  // Currently, disparities are stored as shorts
  unsigned short *disp = subDispPic->GetUShortPixelAddress (row, col);

  if (disp == NULL)
    return INIT_ERR;

  if (!range_params_good)
    CreateRangeParams();

  if (*disp == NO_S2_DISP || *disp <= center_X) {
    if (x) *x = NO_RANGE;
    if (y) *y = NO_RANGE;
    if (z) *z = NO_RANGE;
  } else {
    tx = MM[0] * col + MM[1] * row + MM[2];
    ty = MM[3] * col + MM[4] * row + MM[5];
    tz = MM[6] * col + MM[7] * row + MM[8];
    scale = tx * AA[0] + ty * AA[1] + tz * AA[2];
    if (fabs ((((float) *disp) - center_X) * scale) < 0.00001) {
      scale = 1.0;
    }
    scale = baseline / ((((float) *disp) - center_X) * scale);

    if (x) *x = scale * tx + CC[0];
    if (y) *y = scale * ty + CC[1];
    if (z) *z = scale * tz + CC[2];
  }
  return NO_ERR;
}



// If you really need the full-up range image, calling this routine will
// create it in a public class variable.  But please note this requires a
// LOT of space; each disparity pixel is a short (2 bytes), but each range
// pixel is sizeof(float)*3 (12 bytes).

long			
JPLStereo::GenerateFloatXYZRangeImage ()
{

  long rows, cols, err = 0, row, col;
  long dispRB, rangeRB;	// Number of bytes per row in each image
  AnythingT disp, dst;
  unsigned short *d;
  float *range;
  register float x0, y_0, z0, x, y, z, scale;
  
  JPLPic *pic10 = GetPic10 ();

  if (subDispPic == NULL) {
    FatalErr ("Subpixel Disparity Map not initialized\n");
    return INIT_ERR;
  }
  
#ifdef TIMING__
  Timing (timeStart);
#endif
  
  // Prepare internal matrices for range generation
  
  CreateRangeParams();
  
  rows = leftRectPic->rows;
  cols = leftRectPic->cols;
  
  // pic10 is the static space for the rangePic.
  
  if (pic10 == NULL && rangePic == NULL) {
    pic10 = NEW(mm, "rangePic") JPLPic(mm);
    if (pic10 == NULL ||
	pic10->Init (rows, cols, XYZ_FLOAT_PIXEL) != NO_ERR) {
      FatalErr ("Cannot allocate pic10 temp space\n");
      return MEM_ERR;
    }
  }
  
  rangePic = pic10;
  if (rangePic == NULL || 
      (err = rangePic->Init (rows, cols, XYZ_FLOAT_PIXEL)) != NO_ERR) {
    FatalErr ("Cannot allocate the range pic\n");
    return err;
  }
  
  
  dispRB = subDispPic->GetRowBytes ();
  rangeRB = rangePic->GetRowBytes ();
  disp.uc = subDispPic->GetPixelAddress ();
  dst.uc = rangePic->GetPixelAddress ();
  
#ifdef INTERFACE_DEBUG
  double testmax = -1.0, testmin = 100000.0;
  double scalemax = -1.0, scalemin = 100000.0;
#endif
  int scale_zeros = 0;
  
  for (row = 0; row < rows; row ++, disp.uc += dispRB, dst.uc += rangeRB) {
    x0 = MM[1] * row + MM[2];
    y_0 = MM[4] * row + MM[5];
    z0 = MM[7] * row + MM[8];
    for (col = 0, d = disp.uh, range = dst.f; col < cols;
	 col ++, d++, range+=3) {

#if 0
      // You can either use this code, or the code below.  Code below will run
      // slightly faster since it does some precomputations and doesn't 
      // require a function call for each pixel.  Advantage of using this
      // shorter, but less run-time efficient code is that it tests out the
      // disparity-map-only calculations.

      if (Disparity2FloatXYZRange
	  (row, col, &range[0], &range[1], &range[2]) != NO_ERR)
	printf ( "Range failed at [%ld,%ld]\n", row, col);
      continue;
#else
      if (*d == NO_S2_DISP || *d <= center_X) {
	range[0] = range[1] = range[2] = NO_RANGE;
	continue;
      }
      x = x0 + MM[0] * col;
      y = y_0 + MM[3] * col;
      z = z0 + MM[6] * col;
      scale = x * AA[0] + y * AA[1] + z * AA[2];
      if (fabs((((float) *d) - center_X) * scale) < 0.00001) {
	scale_zeros++;
	scale = 1.0;
      }
      scale = baseline / ((((float) *d) - center_X) * scale);

      range[0] = scale * x + CC[0];
      range[1] = scale * y + CC[1];
      range[2] = scale * z + CC[2];
#endif
      
#ifdef INTERFACE_DEBUG
      if (scalemin == 100000.0 || scale < scalemin)
	scalemin = scale;
      if (scalemin == 100000.0 || scale > scalemax)
	scalemax = scale;
      if (testmin == 100000.0 || range[1] < testmin)
	testmin = range[1];
      if (testmin == 100000.0 || range[1] > testmax)
	testmax = range[1];
#endif
    }
  }
  
  if (scale_zeros > 0)
    printf ( "WARNING!  There were %d near-zero scalings\n",
	     scale_zeros);
  
#ifdef INTERFACE_DEBUG
  printf ( "Y ranges from [%g:%g]\n", testmin, testmax);
  printf ( "Scale ranges from [%g:%g]\n", scalemin, scalemax);
#endif
  
#ifdef TIMING__
  SHOW_TIME ("Time for generating float range XYZ map");
#endif

  return NO_ERR;
}



// GenerateRangeImage -- convert the subpixel disparity results stored
// in subDispPic into range data using the camera model.

long			
JPLStereo::GenerateRangeImage ()
{
  return GenerateFloatXYZRangeImage ();
}





void
JPLStereo::RemoveStaticPointers ()
{
#ifdef MEM_DEBUG
  printf ( "Zeroing out Static pointers %lx %lx %lx %lx\n",
	   (long) leftRectPic, (long) rightRectPic, (long) subDispPic,
	   (long) rangePic);
#endif
//  if (leftRectPic) leftRectPic = NULL;
//  if (rightRectPic) rightRectPic = NULL;
//  if (subDispPic) subDispPic = NULL;
//  if (rangePic) rangePic = NULL;
}



void
JPLStereo::SetLrlosLimit (int limit)
{
  limit = (limit < 0 ? -limit : limit);
  lrlosLimit = limit;
}


void
JPLStereo::SetXGradThreshold (float threshold)
{
  int tmp;

  tmp = (int) (I_DISP_SCALE * fabs (threshold));
  xGradThreshold = tmp;
}

void
JPLStereo::SetYGradThreshold (float threshold)
{
  int tmp;

  tmp = (int) (I_DISP_SCALE * fabs (threshold));
  yGradThreshold = tmp;
}

void JPLStereo::SetOverhangWindowFraction (float new_fraction)
{
  if (EQ(new_fraction, 0.0))
    overhangWindowFraction = 0.0;
  else if (EQ(new_fraction, 1.0))
    overhangWindowFraction = 1.0;
  else if (LT (0.0, new_fraction) && LT (new_fraction, 1.0))
    overhangWindowFraction = new_fraction;
}

void JPLStereo::SetOverhangSigmaFraction (float new_fraction)
{
  overhangSigmaFraction = new_fraction;
}



void JPLStereo::SetOverhangWorldUp (float x, float y, float z)
{
  overhang_world_up[0] = x;
  overhang_world_up[1] = y;
  overhang_world_up[2] = z;
}


#ifdef __unix

int 
JPLStereo::InitCameras (char *leftName, char *rightName,
			float leftNodal[3], float leftToRight[3],
			float leftPointing[3], int rows, int cols)
{
  JPLCamera left(mm), right(mm);
  int result;

  if (leftOriginalCam == NULL && leftName != NULL)
    leftOriginalCam = NEW(mm, "leftOriginalCam") JPLCamera(mm);
  if (rightOriginalCam == NULL && rightName != NULL)
    rightOriginalCam = NEW(mm, "rightOriginalCam") JPLCamera (mm);

  if (leftName != NULL) {
    if (left.ReadCameraModel (leftName) != NO_ERR) {
      FatalErr ("Cannot read the left camera model\n");
      return FILE_ERR;
    }
  }

  if (rightName != NULL && right.ReadCameraModel (rightName) != NO_ERR) {
      FatalErr ("Cannot read the right camera model\n");
      return FILE_ERR;
  }

  if (leftName == NULL || rightName == NULL) {
    Warning ("No rectification will be perfomed. Only one camera is specified\n");
    left.CopyCamera (leftOriginalCam);
    right.CopyCamera (rightOriginalCam);
    return NO_ERR;
  }

  /* transform camera models*/
  result = left.TransformStereoCameras (&right, leftOriginalCam,
					rightOriginalCam, leftNodal,
					leftToRight, leftPointing);
  if (result != NO_ERR)
    return result;

  result = AlignCameras (leftOriginalCam, rightOriginalCam,
			 rows, cols, rows, cols);
  return result;
}

#endif /* __unix */

int
JPLStereo::StereoPixelRay (int leftOrRight, int rectify,
			 double imgPos[2], double nodal[3], double ray[3])
{
  JPLCamera *cam;

  if (leftOrRight == LEFT_VIEW) {
    if (rectify) 
      cam = leftRectCam;
    else
      cam = leftOriginalCam;
  } else {
    if (rectify)
      cam = rightRectCam;
    else
      cam = rightOriginalCam;
  }

  if (cam == NULL) {
    FatalErr ("Camera is NULL\n");
    return INIT_ERR;
  }

  return cam->Image2DToRay3D (imgPos, nodal, ray, NULL);
}

