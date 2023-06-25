#include "JPLStereo.h"

#define SAVE_RECTIFIED_IMAGES

#ifdef TIMING__
#include "Timing.h"
#define SHOW_TIME(str) (show_timing ? TIME(str) : 0)
#endif


/*!
  \param bound Total number of pixels to be skipped on one side of the
  image. 
*/

long			
JPLStereo::MatchStereo (JPLPic *leftPic, JPLPic *rightPic, 
			JPLCamera *leftCam, JPLCamera *rightCam,
			int pyramidLevel, long warp_rows, long warp_cols,
			int fullMinDisp, int fullMaxDisp, unsigned char bound,
			long s_up_l_row, long s_up_l_col, long s_rows,
			long s_cols)
{

#ifdef MEM_DEBUG
  printf ( "*** Entering MatchStereo\n");
#endif
  float s = (1L << pyramidLevel);
  long trows = 1, tcols = 1;
#ifdef TIMING__
  int save_pyrlevel = pyramidLevel;
#endif

  // Grab copies of the static buffers
  JPLPic *pic1 = GetPic1 ();
  JPLPic *pic2 = GetPic2 ();
  JPLPic *pic3 = GetPic3 ();
  JPLPic *pic4 = GetPic4 ();
  JPLPic *pic5 = GetPic5 ();
  JPLPic *pic6 = GetPic6 ();
#ifdef SAVE_RECTIFIED_IMAGES
  JPLPic *pic7 = GetPic7 ();
  JPLPic *pic8 = GetPic8 ();
#endif
  range_params_good = 0;
  if (leftPic) {
    trows = leftPic->rows;
    tcols = leftPic->cols;
  }
  if (pyramidLevel < 0) {
    FatalErr ("Cannot have negative pyramid level\n");
    return INIT_ERR;
  }
  if (leftPic == NULL || rightPic == NULL ||
      leftCam == NULL || rightCam == NULL ||
      leftCam->modelType != rightCam->modelType) {
    FatalErr ("Cameras or Pictures not initialized properly\n");
    return INIT_ERR;
  }
  
  ResizeImageBuffers (trows, tcols, pyramidLevel);

#ifdef MEM_DEBUG
  printf ( "*** Done checking MatchStereo params\n");
#endif

#ifdef TIMING__
	Timing (timeStart);
#endif
  
  // filter/decimate images and cameras
  minDisp = (fullMinDisp) >> pyramidLevel;
  maxDisp = (fullMaxDisp) >> pyramidLevel;
  JPLPic *initLeftPic, *initRightPic, *tmpPic;
  unsigned char addBound = bound;
  long rows, cols;
  
#ifdef MEM_DEBUG
  printf ( "*** Done initializing scalars\n");
#endif
  
  // if (leftRectPic) DELETE (mm, leftRectPic);
  //	if (rightRectPic) DELETE (mm, rightRectPic);
  initLeftPic = leftPic;
#ifdef MEM_DEBUG
  fprintf(stderr, "Input Left Size = (%ld, %ld), Right Input Size = (%ld, %ld)\n",
	  leftPic->rows, leftPic->cols, rightPic->rows, rightPic->cols);
  printf ( "*** initLeftPic=%lx\n", (long) initLeftPic);
#endif
  initRightPic = rightPic;

  /*  unsigned char usingOriginal = true; NOT USING THIS ANYMORE */
  
#ifdef MEM_DEBUG
  printf (
	   "*** Done setting up initial picture pointers\n    Options: 0x%lx\n",
	   options);
#endif
  
  //	initLeftPic->ShowOnScreen (&debugWindow, "\pLeft Rectified",
  //					false, false, 0, 0);
  if (!(options & POST_WARP)) { // pre-warp

    // Compute the rectification parameters needed for these camera
    // models, at the resolution defined by warp_rows and warp_cols at
    // the given pyramidLevel.

    if (AlignCameras (leftCam, rightCam, leftPic->rows, leftPic->cols,
		      warp_rows >> pyramidLevel,
		      warp_cols >> pyramidLevel) != NO_ERR)
      return INTERNAL_ERR;
    
    rows = initLeftPic->rows;
    cols = initLeftPic->cols;
    tmpPic = pic1;
    if (tmpPic->Init(rows, cols, UC8_PIXEL) != NO_ERR)
      return MEM_ERR;
    
    // Rectification should work as long as the camera model is
    // a full cahvor file.  If not, just plow ahead with the
    // original image and see what happens.

#ifdef TIMING__
    SHOW_TIME ("Done aligning cameras, about to rectify left image");
#endif

    if (leftCam->RectifyImage (initLeftPic, tmpPic, leftRectCam,
			       numQuadraCurves, verbosity) == NO_ERR)
      initLeftPic = tmpPic;
#ifdef TIMING__
    SHOW_TIME ("Done rectifying left image, starting right image");
#endif


    rows = initRightPic->rows;
    cols = initRightPic->cols;
    tmpPic = pic2;
    if (tmpPic->Init(rows, cols, UC8_PIXEL) != NO_ERR)
      return MEM_ERR;
    if (rightCam->RectifyImage (initRightPic, tmpPic,
				rightRectCam, numQuadraCurves, verbosity)
	== NO_ERR)
      initRightPic = tmpPic;

#ifdef TIMING__
    SHOW_TIME ("Time for rectification");
#endif

    /*    usingOriginal = false;  NOT USING THIS ANYMORE */
    // scale camera models
    leftRectCam->ResizeImageCoordinate (1.0 / s);
    rightRectCam->ResizeImageCoordinate (1.0 / s);
  } 
  
  // Now initLeftPic and initRightPic point to the initial images,
  // either as originally passed in, or as allocated dynamically for
  // pre-warping.
  
#ifdef MEM_DEBUG
  printf ( "*** Done (or skipped) warping step; now decimating\n");
#endif
  
  //	initLeftPic->ShowOnScreen (&debugWindow, "\pLeft Rectified",
  //					false, false, 0, 0);

  // Use these pointers to manage the alternating between two buffer
  // storage locations (init*Pic and pic#).

  JPLPic *left_use = pic3, *left_store = initLeftPic, *pic_swap;
  JPLPic *right_use = pic4, *right_store = initRightPic;

  while (pyramidLevel > 1) {
    rows = initLeftPic->rows;
    cols = initLeftPic->cols;
    tmpPic = left_use;
    if (tmpPic->Init ((rows + 1) >> 1, (cols + 1) >> 1,
		      UC8_PIXEL) != NO_ERR)
      return MEM_ERR;
    
    initLeftPic->SmoothAndDecimateBySlidingSum (tmpPic, smoothWinSize, 0);
    //		if (! usingOriginal && initLeftPic) DELETE (mm, initLeftPic);
    
    initLeftPic = tmpPic;
    pic_swap = left_store;
    left_store = left_use;
    left_use = pic_swap;

    rows = initRightPic->rows;
    cols = initRightPic->cols;
    tmpPic = right_use;
    if (tmpPic->Init((rows+1)>>1, (cols+1)>>1, UC8_PIXEL)
	!= NO_ERR)
      return MEM_ERR;
    
    initRightPic->SmoothAndDecimateBySlidingSum (tmpPic, smoothWinSize, 0);
    //		if (! usingOriginal && initRightPic) DELETE (mm, initRightPic);
    //		else usingOriginal = false;
    initRightPic = tmpPic;
    pic_swap = right_store;
    right_store = right_use;
    right_use = pic_swap;

    pyramidLevel --;

    // Reduce the bound around the image with each pyramid level, but
    // take into account the smoothing window that was used to do the
    // reduction.  After this while loop addBound will be in pixels at
    // the reduced resolution; before the loop it was at full
    // resolution.

    addBound = (addBound + (smoothWinSize >> 1) + 1) >> 1;
  }
  
  // So now pic1/pic2 might have been used to prewarp, and pic3/pic4
  // might have  been used to decimate to achieve the necessary
  // pyramid level
  
  //	initLeftPic->ShowOnScreen (&debugWindow, "\pLeft Rectified",
  //					false, false, 0, 0);
#ifdef MEM_DEBUG
  printf ( "*** Done preparing for decimating MatchStereo\n");
#endif
  
#ifdef SAVE_RECTIFIED_IMAGES
  // SAVE a copy of the warped original image.  First allocate 
  // storage if needed.  We expect pyramidLevel to be either 0 or 1
  // at this point.
  if (leftRawRectPic == NULL)
    leftRawRectPic = pic7;
  leftRawRectPic->Init
    ((initLeftPic->rows + pyramidLevel) >> pyramidLevel,
     (initLeftPic->cols + pyramidLevel) >> pyramidLevel,
     UC8_PIXEL);

  if (rightRawRectPic == NULL)
    rightRawRectPic = pic8;
  rightRawRectPic->Init
    ((initRightPic->rows + pyramidLevel) >> pyramidLevel,
     (initRightPic->cols + pyramidLevel) >> pyramidLevel,
     UC8_PIXEL);
  
  
  // Now copy the image, reducing the pyramid level first if needed.
  // HACK!!  This does NOT give us rectified images if post-warp is set.
  // Need to generate them in a temp image somehow.

  if (pyramidLevel == 1) {
    initLeftPic->SmoothAndDecimateBySlidingSum (leftRawRectPic,
						smoothWinSize, 0);
    initRightPic->SmoothAndDecimateBySlidingSum (rightRawRectPic,
						 smoothWinSize, 0);
  } else {
    if (initLeftPic->Copy(leftRawRectPic) != NO_ERR)
      fprintf (stderr, "ERROR!!! Failed to create a copy of "
	      "the original left image!\n");
    if (initRightPic->Copy(rightRawRectPic) != NO_ERR)
      fprintf (stderr, "ERROR!!! Failed to create a copy of "
	      "the original right image!\n");
  }
#endif /* SAVE_RECTIFIED_IMAGES */
  
  if (pyramidLevel == 0) {	// DoG without decimation
    
    // We can reuse pic3/pic4 here, since they will only have been
    // used before for decimation.
    
    rows = initLeftPic->rows;
    cols = initLeftPic->cols;
    tmpPic = pic5;
    if (tmpPic->Init (rows, cols, UC8_PIXEL) != NO_ERR)
      return MEM_ERR;
    
    initLeftPic->DoGBySlidingSum (tmpPic, dogWinSize1, dogWinSize1, dogWinSize2, 
				  dogWinSize2, 0);
#ifdef MEM_DEBUG
    fprintf(stderr, "rows = %ld, cols = %ld, initLeftPic=%lx\n", rows, cols,(long)  initLeftPic);
#endif
    // if (!usingOriginal && initLeftPic) DELETE (mm, initLeftPic);
    initLeftPic = tmpPic;
    
    rows = initRightPic->rows;
    cols = initRightPic->cols;
    tmpPic = pic6;
    if (tmpPic->Init (rows, cols, UC8_PIXEL) != NO_ERR)
      return MEM_ERR;
    
    initRightPic->DoGBySlidingSum (tmpPic, dogWinSize1, dogWinSize1, dogWinSize2, 
				   dogWinSize2, 0);
    //		if (!usingOriginal && initRightPic) DELETE (mm, initRightPic);
    initRightPic = tmpPic;
    
    // Grow the no-data boundary to account for the DoG filter
    addBound = (addBound + (dogWinSize1 >> 1));
  } else { // DoG and Decimation
    rows = initLeftPic->rows;
    cols = initLeftPic->cols;
    tmpPic = pic5;
    if (tmpPic->Init ((rows+1)>>1, (cols+1)>>1,
			    UC8_PIXEL) != NO_ERR)
      return MEM_ERR;
    
    initLeftPic->DoGAndDecimateBySlidingSum (tmpPic, dogWinSize1 + smoothWinSize - 1, 
					     dogWinSize2 + smoothWinSize - 1, 0);
#ifdef MEM_DEBUG
    fprintf(stderr, "rows = %ld, cols = %ld, initLeftPic=%lx\n", rows, cols,(long)  initLeftPic);
#endif
    //		if (!usingOriginal && initLeftPic) DELETE (mm, initLeftPic);
    initLeftPic = tmpPic;
    
    //	initLeftPic->ShowOnScreen (&debugWindow, "\pLeft Rectified",
    //					false, false, 0, 0);
    rows = initRightPic->rows;
    cols = initRightPic->cols;
    tmpPic = pic6;
    if (tmpPic->Init ((rows+1)>>1, (cols+1)>>1, UC8_PIXEL)
	!= NO_ERR)
      return MEM_ERR;
    initRightPic->DoGAndDecimateBySlidingSum (tmpPic, dogWinSize1 + smoothWinSize - 1, 
					      dogWinSize2 + smoothWinSize - 1, 0);
    //		if (!usingOriginal&& initRightPic ) DELETE (mm, initRightPic);
    //		else usingOriginal = false;
    initRightPic = tmpPic;

    // Grow the no-data boundary to account for the DoG filter and
    // smoothing window, but then shrink it because we've gone down a
    // pyramid level.
    addBound = (addBound + ((dogWinSize1 + smoothWinSize - 1) >> 1) + 1) >> 1; 

  }
  
  //	initLeftPic->Write ("leftRect.pic");
  //	initRightPic->Write ("rightRect.pic");
  //	initLeftPic->ShowOnScreen (&debugWindow, "\pLeft Rectified",
  //					false, false, 0, 0);
#ifdef MEM_DEBUG
  printf ( "*** Done decimating MatchStereo\n");
#endif
#ifdef TIMING__
  { char buf[1000];
    snprintf (buf, 1000, "Time for DoG and pyrlevel %d downsample",
	     save_pyrlevel);
    SHOW_TIME (buf);
  }
#endif

  
  if (options & POST_WARP) { // post-warp
    // scale the original camera parameters
    leftCam->ResizeImageCoordinate (1.0 / s);
    rightCam->ResizeImageCoordinate (1.0 / s);

    // NOT A HACK: By calling AlignCameras, we're constantly
    // recomputing the rectification parameters from the original
    // camera models.  It might make sense to cache the original
    // models, then only recompute the rectification info when the
    // input camera models differ from those used to originally
    // compute it.

    if (AlignCameras (leftCam, rightCam,
		      initLeftPic->rows, initLeftPic->cols,
		      warp_rows, warp_cols) != NO_ERR)
      return INTERNAL_ERR;

    rows = initLeftPic->rows;
    cols = initLeftPic->cols;
    tmpPic = pic1;
    if (tmpPic->Init (rows, cols, UC8_PIXEL) != NO_ERR)
      return MEM_ERR;
#ifdef MEM_DEBUG
    fprintf(stderr, "rows = %ld, cols = %ld, initLeftPic=%lx\n", rows, cols,(long)  initLeftPic);
#endif

#ifdef TIMING__
    SHOW_TIME ("Done aligning cameras, about to rectify left image");
#endif

    leftCam->RectifyImage (initLeftPic, tmpPic, leftRectCam, numQuadraCurves,
			   verbosity);
#ifdef MEM_DEBUG
    printf ( "*** about to delete initLeftPic; initLeftPic=%lx\n",
	     (long) initLeftPic);
#endif
    //                initLeftPic->Write ("test.pic");
#if 0
    if (!usingOriginal && initLeftPic) {
      //if (initLeftPic->clut) free (initLeftPic->clut);
      if (initLeftPic->GetPixelAddress()) free (initLeftPic->GetPixelAddress());
      free (initLeftPic);
    }
#endif
    //delete initLeftPic;
#ifdef MEM_DEBUG
    printf ( "*** done deleting initLeftPic\n");
#endif
    
    initLeftPic = tmpPic;
    
    //	initLeftPic->ShowOnScreen (&debugWindow, "\pLeft Rectified",
    //					false, false, 0, 0);
    rows = initRightPic->rows;
    cols = initRightPic->cols;
    tmpPic = pic2;
    if (tmpPic->Init (rows, cols, UC8_PIXEL) != NO_ERR)
      return MEM_ERR;

#ifdef TIMING__
    SHOW_TIME ("Done rectifying left image, starting right image");
#endif

    rightCam->RectifyImage (initRightPic, tmpPic, rightRectCam,
			    numQuadraCurves, verbosity);
#ifdef MEM_DEBUG
    printf ( "*** about to delete initRightPic=%lx\n",
	     (long) initRightPic);
#endif
    
#if 0
    if (!usingOriginal && initRightPic) {
      //  if (initRightPic->clut) free (initRightPic->clut);
      if (initRightPic->GetPixelAddress()) free (initRightPic->GetPixelAddress());
      free (initRightPic);
    }
#endif
    //delete initRightPic;
#ifdef MEM_DEBUG
    printf ( "*** done deleting initRightPic\n");
#endif
    
    initRightPic = tmpPic;
    //		addBound = 0; // We don't know how many pixels are invalid anymore
    // scale them back
    leftCam->ResizeImageCoordinate (s);
    rightCam->ResizeImageCoordinate (s);

#ifdef TIMING__
    SHOW_TIME ("Time for rectification");
#endif
  }
  leftRectPic = initLeftPic;
  rightRectPic = initRightPic;


  // Handle a request for right-handed disparity generation
  // The core of the code is fixed to generate the LEFT handed disparity.
  // But we can trick it by feeding it the mirror images in reverse order;
  // instead of <left, right> we use <mirror-right, mirror-left>.  We'll
  // have to mirror the results too, but down below after the call to
  // MatchStereo.

  JPLPic *leftRectMirror = NULL, *rightRectMirror = NULL, *tmp = NULL;

  if (rightHandDisparity) {
    rightRectMirror = rightRectPic->HorizontalMirrorImage();
    leftRectMirror = leftRectPic->HorizontalMirrorImage();
    tmp = rightRectPic; rightRectPic = leftRectMirror; leftRectMirror = tmp;
    tmp = leftRectPic; leftRectPic = rightRectMirror; rightRectMirror = tmp;
#ifdef TIMING__
    SHOW_TIME ("Time for mirror-image reflection of both images");
#endif
  }

  //#define __DEBUG_RECTIFY__
#ifdef __DEBUG_RECTIFY__
  leftRectPic->Write ("l_rect.pic");
  rightRectPic->Write ("r_rect.pic");
#endif

  // Handle a request for a subimage of the warped image here.
  
  JPLPic *subLeftRectPic = NULL, *subRightRectPic = NULL;
  
  if (s_rows > 0 && s_cols > 0) {
    subLeftRectPic = leftRectPic->SubImage (s_up_l_row, s_up_l_col,
					     s_rows, s_cols);
    subRightRectPic = rightRectPic->SubImage (s_up_l_row, s_up_l_col,
					       s_rows, s_cols);
    leftRectPic = subLeftRectPic;
    rightRectPic = subRightRectPic;
    SetSubwindow (s_up_l_row, s_up_l_col, s_rows, s_cols);
  }
#ifdef MEM_DEBUG
  printf ( "*** Done modifying MatchStereo image coords\n");
#endif
  
  //	initLeftPic->ShowOnScreen (&debugWindow, "\pLeft Rectified",
  //					false, false, 0, 0);
#ifdef TIMING__
  SHOW_TIME ("Time for pre-processing");
#endif
  
#ifdef MEM_DEBUG
  printf ( "*** Calling primary stereo matching\n");
#endif
  int status = MatchStereo (addBound);
  
  if (subLeftRectPic) {
    leftRectPic = initLeftPic;
    DELETE (mm, subLeftRectPic);
    subLeftRectPic = NULL;
  }
  if (subRightRectPic) {
    rightRectPic = initRightPic;
    DELETE (mm, subRightRectPic);
    subRightRectPic = NULL;
  }
  
  // Handle post-processing for right-handed disparity generation

  if (rightHandDisparity) {
    tmp = subDispPic->HorizontalMirrorImage();
    tmp->Copy (subDispPic);
    DELETE (mm, tmp);
    tmp = rightRectPic; rightRectPic = leftRectMirror; leftRectMirror = tmp;
    tmp = leftRectPic; leftRectPic = rightRectMirror; rightRectMirror = tmp;
    DELETE (mm, leftRectMirror);
    DELETE (mm, rightRectMirror);
  }

  ClearSubwindow();
  return status;
}


long			
JPLStereo::MatchStereo (JPLPic *leftPic, JPLPic *rightPic, 
			int pyramidLevel, int fullMinDisp,
			int fullMaxDisp, unsigned char bound)
{
  long rows, cols;
  JPLPic *initLeftPic, *initRightPic, *tmpPic;
  /*  unsigned char usingOriginal; HACK not using this anymore */
  unsigned char addBound;
  long trows = 1, tcols = 1;
  
  // Grab copies of the static buffers
  JPLPic *pic1 = GetPic1 ();
  JPLPic *pic2 = GetPic2 ();
  JPLPic *pic3 = GetPic3 ();
  JPLPic *pic4 = GetPic4 ();

  range_params_good = 0;
  if (leftPic) {
    trows = leftPic->rows;
    tcols = leftPic->cols;
  }
  if (pyramidLevel < 0) {
    FatalErr ("Cannot have negative pyramid level\n");
    return INIT_ERR;
  }
  if (leftPic == NULL || rightPic == NULL) {
    FatalErr ("Pictures not initialized properly\n");
    return INIT_ERR;
  }
  
  ResizeImageBuffers (trows, tcols, pyramidLevel);
  
#ifdef TIMING__
  Timing (timeStart);
#endif
  
  minDisp = (fullMinDisp) >> pyramidLevel;
  maxDisp = (fullMaxDisp) >> pyramidLevel;
  
  if (leftRectPic) DELETE (mm, leftRectPic);
  if (rightRectPic) DELETE (mm, rightRectPic);
  initLeftPic = leftPic;
  initRightPic = rightPic;
  /*  usingOriginal = true; HACK not using this anymore */
  addBound = bound;
  
  while (pyramidLevel > 1) {
    rows = initLeftPic->rows;
    cols = initLeftPic->cols;
    tmpPic = pic1;
    if (tmpPic->Init ((rows + 1) >> 1, (cols + 1) >> 1,
			    UC8_PIXEL) != NO_ERR) {
      FatalErr ("Cannot allocate temporary memory\n");
      return MEM_ERR;
    }
    initLeftPic->SmoothAndDecimateBySlidingSum (tmpPic, smoothWinSize, 0);
    // if (! usingOriginal && initLeftPic) DELETE (mm, initLeftPic);
    initLeftPic = tmpPic;
    
    rows = initRightPic->rows;
    cols = initRightPic->cols;
    tmpPic = pic2;
    if (tmpPic->Init ((rows + 1) >> 1, (cols + 1) >> 1,
			    UC8_PIXEL) != NO_ERR) {
      FatalErr ("Cannot allocate temporary memory\n");
      return MEM_ERR;
    }
    initRightPic->SmoothAndDecimateBySlidingSum (tmpPic, smoothWinSize, 0);
    // if (! usingOriginal && initRightPic) DELETE (mm, initRightPic);
    // else usingOriginal = false;
    initRightPic = tmpPic;
    pyramidLevel --;
    addBound = (addBound + (smoothWinSize >> 1) + 1) >> 1; // how many boundary pixels are invalid
  }
  
  //	initLeftPic->ShowOnScreen (&debugWindow, "\pLeft Rectified",
  //					false, false, 0, 0);
  if (pyramidLevel == 0) {	// DoG without decimation
    rows = initLeftPic->rows;
    cols = initLeftPic->cols;
    tmpPic = pic3;
    if (tmpPic->Init (rows, cols, UC8_PIXEL) != NO_ERR) {
      FatalErr ("Cannot allocate temporary memory\n");
      return MEM_ERR;
    }
    initLeftPic->DoGBySlidingSum (tmpPic, dogWinSize1, dogWinSize1, dogWinSize2, 
				  dogWinSize2, 0);
    // if (!usingOriginal && initLeftPic) DELETE (mm, initLeftPic);
    initLeftPic = tmpPic;
    
    rows = initRightPic->rows;
    cols = initRightPic->cols;
    tmpPic = pic4;
    if (tmpPic->Init (rows, cols, UC8_PIXEL) != NO_ERR) {
      FatalErr ("Cannot allocate temporary memory\n");
      return MEM_ERR;
    }
    initRightPic->DoGBySlidingSum (tmpPic, dogWinSize1, dogWinSize1, dogWinSize2, 
				   dogWinSize2, 0);
    // if (!usingOriginal && initRightPic) DELETE (mm, initRightPic);
    initRightPic = tmpPic;
    
    addBound = (addBound + (dogWinSize1 >> 1));
  } else { // DoG and Decimation
    rows = initLeftPic->rows;
    cols = initLeftPic->cols;
    tmpPic = pic3;
    if (tmpPic->Init ((rows+1) >> 1, (cols+1) >> 1,
			    UC8_PIXEL) != NO_ERR) {
      FatalErr ("Cannot allocate temporary memory\n");
      return MEM_ERR;
    }
    initLeftPic->DoGAndDecimateBySlidingSum
      (tmpPic, dogWinSize1 + smoothWinSize - 1, 
       dogWinSize2 + smoothWinSize - 1, 0);
    // if (!usingOriginal && initLeftPic) DELETE (mm, initLeftPic);
    initLeftPic = tmpPic;
    
    rows = initRightPic->rows;
    cols = initRightPic->cols;
    tmpPic = pic4;
    if (tmpPic->Init ((rows+1) >> 1, (cols+1) >> 1,
			    UC8_PIXEL) != NO_ERR) {
      FatalErr ("Cannot allocate temporary memory\n");
      return MEM_ERR;
    }
    initRightPic->DoGAndDecimateBySlidingSum
      (tmpPic, dogWinSize1 + smoothWinSize - 1, 
       dogWinSize2 + smoothWinSize - 1, 0);
    // if (!usingOriginal && initRightPic) DELETE (mm, initRightPic);
    // else usingOriginal = false;
    initRightPic = tmpPic;
    addBound = (addBound + ((dogWinSize1 + smoothWinSize - 1) >> 1) + 1) >> 1; // how many boundary pixels are invalid
  }
  leftRectPic = initLeftPic;
  rightRectPic = initRightPic;
  
  //	initLeftPic->ShowOnScreen (&debugWindow, "\pLeft Rectified",
  //					false, false, 0, 0);
#ifdef TIMING__
  SHOW_TIME ("Time for pre-processing");
#endif

  return MatchStereo (addBound);
}

				
