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

#ifndef __STEREO_PAIR__
#define __STEREO_PAIR__

#include "JPLPic.h"
#include "Camera.h"
#ifdef __unix
#include "range.h"
#else
#define RangeT char*
#endif
#include "nav_memory.h"

#define LEFT_VIEW	0
#define RIGHT_VIEW	1

#define ORIGINAL_PIC	0
#define RECTIFIED_PIC	1

// This fraction of the image size defines the MAX number of blobs to allow
#define DEFAULT_BLOB_REGION_FRACTION 0.153	// yields 40k in 512x512
#define DEFAULT_MIN_BLOB_REGIONS_TO_ALLOCATE 10000
#define DEFAULT_OVERHANG_WINDOW_FRACTION 0.05
#define DEFAULT_OVERHANG_SIGMA_FRACTION 0.1

#define SUBPIXEL_DISPARITY		0x00000001
#define REJECT_LRLOS			0x00000002
#define REJECT_BLOB				0x00000004
#define SMOOTH_DISPARITY		0x00000008
#define RANGE_MAP				0x00000010
#define POST_WARP				0x00000020
#define THRESHOLD_UNCERTAINTY	0x00000040
#define INCOMPLETE_SEARCH       0x00000080
#define REMOVE_OVERHANGS	0x00000100
#define ALL_STEREO_OPTIONS (SUBPIXEL_DISPARITY | REJECT_LRLOS | REJECT_BLOB | \
		 SMOOTH_DISPARITY | RANGE_MAP | POST_WARP | \
		 THRESHOLD_UNCERTAINTY | INCOMPLETE_SEARCH | REMOVE_OVERHANGS)

// Structure for counting the number of pixels excluded according to
// which filter flagged them for removal

// ACCEPT	Number of pixels accepted as having good ranges
// BLOB		 ... pixels filtered as parts of freestanding blobs
// BORDER	 ... by having disparity at one of the input limits
// BOUND	 ... by being near the edge of the image
// The width of the outer band that contributes to this count is:
//     Input value (typically 0)
//	 Each Pyramid-level but last add smoothWinSize>>1 (3>>1),  divide by 2
//	 If full-res add dogWinSize1>>1 (9>>1),
//	 else last pyr level add (dogWinSize1+smoothWinSize-1)>>1, divide by 2
//     Finally, add half the correlation window (7>>1).
// FLAT		 ... by having adjacent flat correlation scores
// LRLOS	 ... by mismatching left and right best disparities
// OVERHANG	 ... by hanging over the vehicle
// THRESH	 ... by correlation curvature threshold
// VEHICLE	 ... by being considered as part of the vehicle; this test
//		     is not implement in JPLStereo, but the flag is available
//		     for outside code to use as a marker.

typedef enum {
  NO_FLAG = 0,
  ACCEPT,
  BLOB,
  BORDER,
  BOUND,
  FLAT,
  LRLOS,
  OVERHANG,
  THRESH,
  VEHICLE,
  REJ_FLAGS		// Total number of flags in this enum
} RejFlagT;

typedef struct {
  long rows, cols;	// Image dimensions at which MatchStereo was run
  long f[REJ_FLAGS];	// Counts associated with each RejFlagT filter
} StereoFilterCountT;

#ifdef MAC_PLATFORM
#define DEBUG_STEREO
#endif

#ifdef DEBUG_STEREO
#include <QDOffscreen.h>
#endif

#define OBS_NEGOTIABLE_BIT    5
#define OBS_NONNEGOTIABLE_BIT 7

#define SAD(x, y)	((x) > (y) ? ((x) - (y)) : ((y) - (x)))

//#define __TIMING__

typedef struct MinFullRecord 
{
  unsigned char d;	//  disparity.
  short m, m0, m2, prev;	// minimal correlations and their neighbors.
				// rm is for lrlos
} MinFullRecord, *MinFullRecordPtr;
typedef struct MinRecord 
{
  unsigned char d;	//  disparity.
  short m; // minimal correlations and their neighbor. rm is for lrlos
} MinRecord, *MinRecordPtr;




/*! class

  JPLStereo

  Caller must remove the memory associated with leftRectCam and rightRectCam
*/

class JPLStereo {

protected:

	JPLPic	*leftRectPic, *rightRectPic;
	JPLPic  *leftRawRectPic, *rightRawRectPic;
	JPLCamera *leftRectCam, *rightRectCam;
	/* these cameras are set in InitCameras() and are *not* used
	   by anyone except GetStereoRay */
	JPLCamera *leftOriginalCam, *rightOriginalCam;
	JMemoryManager *mm;

#ifdef MAC_PLATFORM
	WindowPtr		debugWindow;
#endif
 private:



	int lrlosLimit;
	float blob_region_fraction;
	int min_blob_regions_to_allocate;
	int xGradThreshold, yGradThreshold;
	float overhangWindowFraction;
	float overhangSigmaFraction;
	float overhang_world_up[3];
	int verbosity;

	StereoFilterCountT stats;

	// 256 corresponds to 1.0 pixel uncertainty in disparity value.
	long uncertainty_threshold;

	RangeT *rows_to_search;
	int use_laplacian_in_disparity_space_image;

	void init (unsigned char corrWinSize, int minDisparity,
		   int maxDisparity, JMemoryManager *mgr);
public:
	JPLStereo ();
	JPLStereo (JMemoryManager *mgr);
        JPLStereo (unsigned char corrWinSize);
        JPLStereo (int minDisp, int maxDisp);
	JPLStereo (unsigned char corrWinSize, int minDisparity,
			     int maxDisparity, JMemoryManager *mgr);
	~JPLStereo ();
	
	void	FreeStaticBuffers (void);
	void	FreeFilterStaticBuffersOnly (void);

	// set inputs
	void   SetOptions (long newopt);
	void   ClearOptions (long newopt);
	JPLPic  *GetRawRectPic (int leftOrRight);
	void    SetRawRectPic (JPLPic *pic, int leftOrRight);

  	JPLPic	*GetRectPic(int leftOrRight);

	void	SetRectPic (JPLPic *pic, int leftOrRight);

	JPLCamera *GetRectCam (int leftOrRight);

	void	SetRectCam (JPLCamera *cam, int leftOrRight);
	long    EnableMaskPic (void);
	JPLPic *GetMaskPic (void);

	void	SetUncertainty (long u);
	void SetVerbosity (int v)
	  { verbosity = v; }
	int GetVerbosity ()
	  { return verbosity; }
	void SetShowTiming (int newshow)
	  { show_timing = newshow; }
	int GetShowTiming ()
	  { return show_timing; }
	long SetRowsToSearch (char *buff);
	void ShowRowsToSearch (FILE *fp);
	void	SetUseLaplacianInDisparitySpace (int new_use)
	  { use_laplacian_in_disparity_space_image = new_use; }
	int	GetUseLaplacianInDisparitySpace (void)
	  { return use_laplacian_in_disparity_space_image; }

	char *params2s(char *buff);
	char *params2s() { return params2s(NULL); };

	int ResizeImageBuffers (long rows, long cols, int pyrLevel);
	void ClearSubwindow();
	int SetSubwindow(int upper_left_row, int upper_left_col, int rows,
			 int cols);

	long MergeRangeMaps (JPLPic *dstPic, JPLPic *srcPic, float max_sep);
	long MergeDisparityMaps (JPLPic *dstPic, JPLPic *srcPic, 
				 float disp_ratio, float pixel_threshold);

	long TrinocularStereo (JPLPic *centerPic, JPLPic *lowerPic,
			       JPLPic *leftPic, JPLCamera *centerCam,
			       JPLCamera *lowerCam, JPLCamera *leftCam,
			       int pyrLevel);


	/* Image processing
	Convention: If I use a new StereoPair pointer as the last parameter,
	it means that the processed images are going to be stored in
	the new StereoPair. Otherwise, the processed images will 
	replace the old one.
	
	long	DecimateByHalf ();
	long	DoG ();
	long	SmoothUsingSlidingSum (long flag, unsigned char winSizeX,
					unsigned char winSizeY);
	long	SmoothUsingSlidingSum (long flag, unsigned char winSizeX,
				unsigned char winSizeY, StereoPair *newPair);
	long	SmoothAndDecimateUsingSlidingSum (long flag,
						  unsigned char winSize);
	long	SmoothAndDecimateUsingSlidingSum (long flag,
						  unsigned char winSize,
						  StereoPair *newPair);
	long	DoGUsingSlidingSum (long flag, unsigned char winSize1X,
				    unsigned char winSize1Y,
				    unsigned char winSize2X,
				    unsigned char winSize2Y);
	long	DoGUsingSlidingSum (long flag, unsigned char winSize1X,
				    unsigned char winSize1Y,
				    unsigned char winSize2X,
				    unsigned char winSize2Y,
				    StereoPair *newPair);
	long	DoGAndDecimateUsingSlidingSum (long flag, 
			unsigned char winSize1, unsigned char winSize2);
	long	DoGAndDecimateUsingSlidingSum (long flag,
					       unsigned char winSize1, 
					       unsigned char winSize2,
					       StereoPair *newPair);
	*/
	
	// set options

//long	outputMaps; // bits are turned on/off for generating various maps
	int 		fixed_blob_filter_size;  // negative numbers mean use
						//  abs(num) percent of image

	unsigned char	subpixelBits;
			// how many bits to represent subpixel disparity.
	unsigned char	rangeBits;
			// scale in range.
	int		minDisp, maxDisp;
			// search range in pixels.
	long	options;// algorithm options
	int		show_timing;
	int		rightHandDisparity;

	float mergeSqThreshold;	// Max Squared Distance to allow
	// when merging horiztonal and vertical range maps for trinocular

	float mergePixelThreshold; // Max number of pixels by which disparities
	// can differ when merging for trinocular

  // Cache the parameters needed to compute range data from disparity
        int range_params_good;
	float baseline, center_X;
  	float MM[9], AA[3], CC[3];

	// Handle subimages

	long sub_up_left_row, sub_up_left_col, sub_rows, sub_cols, use_sub;

	// various window sizes used in filtering and matching:
	unsigned char	smoothWinSize, dogWinSize1, dogWinSize2,
	  		matchWinSize, matchWinSizeY, numQuadraCurves;
	
	// main actions
	long	AlignCameras (JPLCamera *leftCam, JPLCamera *rightCam,
			      long rows, long cols,
			      long warp_rows, long warp_cols);

	long ComputeDisparityRange (double min_range, double max_range,
				    int *min_disp, int *max_disp);
	long ComputeDisparityRange (JPLCamera *lrectcam, JPLCamera *rrectcam,
				    JPLPic *leftpic, JPLPic *rightpic,
				    double min_range, double max_range,
				    int *min_disp, int *max_disp);
	long ComputeDisparityRange (JPLCamera *lrectcam, JPLCamera *rrectcam,
				    double min_range, double max_range,
				    int *min_disp, int *max_disp);
	long ComputeDisparityRange (double baseline, double focal_length,
				    double min_range, double max_range,
				    int *min_disp, int *max_disp);
	  

	long RemoveOverhangsFromDisparityImage ()
	  { return RemoveOverhangsFromDisparityImage (NULL, NULL); }

	long RemoveOverhangsFromDisparityImage (float *world_up)
	  { return RemoveOverhangsFromDisparityImage (NULL, world_up); }

	long RemoveOverhangsFromDisparityImage (JPLPic *disparity_pic,
						float *world_up);

	void ClearFilterCounts (void);
	void UpdateFilterCount (RejFlagT cnt, long offset);
	StereoFilterCountT GetFilterCounts (void);
	long    ExtractCountsFromMask (JPLPic *mask,
				       StereoFilterCountT *count);

	long BlobFiltering  (short *pixel, long rows, long cols, long srcRB,
			     long maxNumRegions, long minRegSize, short dh,
			     short dv, long *numReg, long *numSize);
	long BlobFiltering  (unsigned char *pixel, long rows, long cols,
			     long srcRB,long maxNumRegions, long minRegSize,
			     unsigned char dh, unsigned char dv,
			     long *numReg, long *numSize);

// full stereo: warping, decimating, filtering, correlation, postprocessing
	long	MatchStereo (JPLPic *leftPic, JPLPic *rightPic, 
			     JPLCamera *leftCam, JPLCamera *rightCam,
			     int pyramidLevel, long warp_rows, long warp_cols,
			     int fullMinDisp, int fullMaxDisp,
			     unsigned char bound, long s_up_l_row,
			     long s_up_l_col, long s_rows, long s_cols);

	long	MatchStereo (JPLPic *leftPic, JPLPic *rightPic, 
			     JPLCamera *leftCam, JPLCamera *rightCam,
			     int pyramidLevel, long warp_rows, long warp_cols,
			     int fullMinDisp, int fullMaxDisp,
			     unsigned char bound) {
	  return MatchStereo (leftPic, rightPic, leftCam, rightCam,
			      pyramidLevel, warp_rows, warp_cols,
			      fullMinDisp, fullMaxDisp, bound, 0L, 0L, 0L,
			      0L); }

	long	MatchStereo (JPLPic *leftPic, JPLPic *rightPic, 
			     JPLCamera *leftCam, JPLCamera *rightCam,
			     int pyramidLevel, int fullMinDisp,
			     int fullMaxDisp, unsigned char bound);
	long	TimingMatchStereo (JPLPic *leftPic, JPLPic *rightPic, 
			     JPLCamera *leftCam, JPLCamera *rightCam,
			     int pyramidLevel, int fullMinDisp,
			     int fullMaxDisp);

	// filtering, decimating, correlation, postprocessing. 
	// (No camera model involved)

	long	MatchStereo (JPLPic *leftPic, JPLPic *rightPic, 
			     int pyramidLevel, int fullMinDisp,
			     int fullMaxDisp, unsigned char bound);

	// correlation, postprocessing. (bare-bone stereo)
	long	MatchStereo (unsigned char bound);
	long	MatchStereo2 (unsigned char bound);
#if 0
	long    MatchStereoHoropter (JPLPic *leftPic, JPLPic *rightPic,
				     JPLCamera *leftCam, JPLCamera *rightCam,
				     int pyramidLevel, float downAngle,
				     float upAngle, float planeCoeff[4],
				     unsigned char bound, float warpM[9]);
	long    Horopter (float minSlope, float maxSlope, float *groundPlane,
			  float M[9], long rows, long cols);
#endif
	// generate the range map
	long	GenerateRangeImage ();
	long	GenerateFloatXYZRangeImage ();
	long CreateRangeParams();
	long Disparity2FloatXYZRange (long row, long col, float *x,
				      float *y, float *z);
	JPLPic *MakeSubpixelElevation();
	JPLPic *MakeSubpixelElevation(JPLPic *rangePic);
	JPLPic *MakeSubpixelElevation(double origin[3], double h_axis[3],
				      double min_height, double max_height,
				      double height_scale);
	JPLPic *MakeSubpixelElevation(JPLPic *rangePic,
				      double origin[3], double h_axis[3],
				      double min_height, double max_height,
				      double height_scale);

	// Generate obstacle map
	/* run the obstacle detection, the result will set the 
           corresponding bit in the obstacle map */
	long    DetectPosObs (float up[3], float height, float threshold,
			      float maxRange, int numBit);
	long    DetectHeightObs (float *up_vec, float minHeight, float maxHeight,
				 float maxRange, int numBits);
	long    DetectNegObs (float gapSize, float ground[3], float up[3],
			      float forward[3], float minSlope,
			      float maxRange);
	long    DetectObs (float *up_vec, float maxRange);
	long    WriteOverlayImage (FILE *fp);
	long    PlaneBoundary (float warpM[9], float planeCoeff[4],
			       float maxRange, float minHeigth,
			       float maxHeight, int numBits);
	
	// write outputs
	long	WriteSubpixelDisparity (char *filename);
	long	WriteSubpixelDisparity (FILE *fp);
	long	WriteRangeAsHeightField (FILE *fp);
	long	WriteRangeMap (FILE *fp);
	long	WriteRangeMap (char *filename);
	long	WriteSubpixelElevation (char *filename);
	long	WriteSubpixelElevation (FILE *fp);
	long	WriteSubpixelElevation (FILE *fp,
					double origin[3], double h_axis[3],
					double min_h, double max_h,
					double h_scale);
	void 	WriteDisparitySpace (unsigned short *disp_vals, long startCol,
				     long dispCols, long pixelCols, long row,
				     unsigned char bound, int use_laplacian);
	void	WriteFilterLabels (JPLPic *result, long ulrow, long ulcol,
				   long rows, long cols,
				   StereoFilterCountT *counts);
	JPLPic *AnnotateMask (JPLPic *result, JPLPic *mask,
			      StereoFilterCountT *counts);

	void	WriteStats (char *filename);
	void	WriteStats (FILE *fp);
	void	WriteFilterCounts (void);
	void	WriteFilterCounts (FILE *fp, char *prefix,
				   StereoFilterCountT *s);
	void	WriteShortStats (char *filename);
	void	WriteShortStats (FILE *fp);

	/* extract planar surfaces */
	long    SurfaceOrientation (unsigned char lsize,
				    JPLPic *vx, JPLPic *vy, JPLPic *vz);
	
	void	RemoveStaticPointers (void);

#ifdef MAC_PLATFORM
	void	SetWindow (WindowPtr	win) {debugWindow = win;};
	long	ShowStereoPair ();
	long 	ShowIntegerDisparity ();
	long	ShowSubpixelDisparity (long flag);
	long	ShowRangeMap ();
#endif
	
#undef INT_MIN
#define INT_MIN(x,y) (((x) < (y)) ? (x) : (y))
#undef INT_MAX
#define INT_MAX(x,y) (((x) > (y)) ? (x) : (y))

	/* utility functions */
	void    SetLrlosLimit (int limit);
	void	SetBlobRegionFraction (float fraction)
	  { blob_region_fraction = INT_MIN(1, INT_MAX(0, fraction)); }
	void	SetMinBlobRegionsToAllocate (int size)
	  { min_blob_regions_to_allocate = INT_MAX(0, size); }
	void    SetMinBlobSize (int minSize);
	void    SetXGradThreshold (float threshold);
	void    SetYGradThreshold (float threshold);
	void	SetOverhangWindowFraction (float new_fraction);
	void	SetOverhangSigmaFraction (float new_fraction);
	void	SetOverhangWorldUp (float x, float y, float z);
	void	SetOverhangWorldUp (float *up)
	  { if (up) { SetOverhangWorldUp (up[0], up[1], up[2]); } }

	int    GetLrlosLimit ()
	  { return lrlosLimit; }
	float	GetBlobRegionFraction ()
	  { return blob_region_fraction; }
	int	GetMinBlobRegionsToAllocate ()
	  { return min_blob_regions_to_allocate; }
	int    GetXGradThreshold ()
	  { return xGradThreshold; }
	int    GetYGradThreshold ()
	  { return yGradThreshold; }
	float  GetOverhangWindowFraction ()
	  { return overhangWindowFraction; }
	float    GetOverhangSigmaFraction ()
	  { return overhangSigmaFraction; }

	/* Access static buffers */
	MinRecordPtr GetMinRecordBuffer (int size);
	MinFullRecordPtr GetMinFullRecordBuffer (int size);
	unsigned char *GetUCBuffer (int size);
	short *GetColumnsBuffer (int size);
	short *GetDisparitySpaceBuffer (int size);
	unsigned char *GetFilterColor (RejFlagT flag);
	RejFlagT *GetRejFlagBuffer (int size);
	char *RejFlag2s (RejFlagT flag);
	long *GetSameBuffer (int size);
	int *GetRegionBuffer (int size);
	float *GetFwdColSumBuffer (int size);
	float *GetFwdColSumSqBuffer (int size);
	JPLPic *GetPic1 (void);
	JPLPic *GetPic2 (void);
	JPLPic *GetPic3 (void);
	JPLPic *GetPic4 (void);
	JPLPic *GetPic5 (void);
	JPLPic *GetPic6 (void);
	JPLPic *GetPic7 (void);
	JPLPic *GetPic8 (void);
	JPLPic *GetPic9 (void);
	JPLPic *GetPic10 (void);


	/* get the ray direction */
	int 
	  InitCameras (char *leftName, char *rightName,
			float leftNodal[3], float leftToRight[3],
			float leftPointing[3], int rows, int cols);
	int     StereoPixelRay (int leftOrRight, int rectify, double imagePos[2],
			      double nodal[3], double ray[3]);


//	JPLPic	*dispPic;	// for pixel disparity map.
	JPLPic	*subDispPic;	// for subpixel disparity.
	JPLPic	*rangePic;	// for range map.
	JPLPic  *obsPic;        // obstacle map
	JPLPic  *maskPic;	// pixel-by-pixel RejFlagT tags or NULL
	
//	static JPLPic *pic1 = NULL, *pic2 = NULL, *pic3 = NULL, *pic4 = NULL;
//	static JPLPic *pic5 = NULL, *pic6 = NULL, *pic7 = NULL, *pic8 = NULL;
//	static long *same = NULL;
//	static short *region = NULL, *columns = NULL;
//	static long pic_size = 0L, same_size = 0L, region_size = 0L;
//	static long columns_size = 0L;
//	static MinRecordPtr rightRecord = NULL;
//	static MinFullRecordPtr leftRecord = NULL;
//	static long rightRecord_size = 0L, leftRecord_size = 0L;
//	static unsigned char *uc_buffer = NULL;
//	static long uc_buffer_size = 0L;

};

extern "C" {
  void FreeJPLStereoStaticBuffers (JMemoryManager *mm);
}

#endif /* __STEREO_PAIR__ */
