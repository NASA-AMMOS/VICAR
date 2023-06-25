#include <math.h>
#include <string.h>	// for memcpy
#include "JPLStereo.h"
#include "Timing.h"


#ifdef TIMING__
#include "Timing.h"
#define SHOW_TIME(str) (show_timing ? TIME(str) : 0)
#endif





// THIS IS THE GUTS OF THE STEREO CODE; HERE'S WHERE THE CORRELATION
// ACTUALLY GETS DONE
//
//   INPUTS:
//	bound		-- Deadband of pixels (both horiz and vert) around
//			   the border.  This is redundant, it could have
//			   been implemented by JPLPic aliasing instead.
//	leftRectPic	-- Left/Right rectified images; expected to have
//	rightRectPic	--   identical sizes (and already pyramid-reduced)
//	minDisp		-- Min/Max disparity range, expressed in rectified
//	maxDisp		--   pixels (i.e. may not be at full res)
//	pic9 (nee subDispPic)  -- holds 16bit subpixel disparities
//	matchWinSize	-- Horizontal correlation window size
//	matchWinSizeY	-- Vertical   correlation window size


//   OUTPUTS:
//	range_params_good
//	
// bound   is the width of bad pixels around the boundary

/*!
  Perform the low-level stereo matching efficiently using sliding windows.

  \param bound User-given symmetric image boundary that should be ignored
  when computing disparities.  This is given in pixels at the
  resolution of the input leftRectPic.
*/

long			
JPLStereo::MatchStereo (unsigned char bound)
{
  // Total number of integer disparities
  unsigned char disparityRange = maxDisp - minDisp + 1;
  long nrows = 0, ncols = 0;
#if defined(STUPID_TRINOCULAR_BUG) || defined(IMG_DEBUG)
  const size_t bufLen = 100;
  char buf[bufLen];
#endif

#ifdef MEM_DEBUG
  printf ( "*** Entering MatchStereo Guts (bound=%u)\n", bound);
#endif

  JPLPic *pic9 = GetPic9 ();

  if (maxDisp - minDisp + 1 > 255) {
    FatalErr ("Number of disparities > 255!  Failing!\n");
    return PARAM_ERR;
  }

  if (maxDisp > 255) {
    FatalErr ("Max disparity > 255!  Failing!\n");
    return PARAM_ERR;
  }

  // As a safety check, we wipe out the cached range-computing matrices.
  // Maybe we don't need to do this here, but what the heck.
  range_params_good = 0;

  // This correlator needs to be run on rectified images
  if (leftRectPic== NULL || rightRectPic == NULL ||
      (nrows = leftRectPic->rows) != rightRectPic->rows ||
      (ncols = leftRectPic->cols) != rightRectPic->cols) {
    FatalErr ("Rectified images are not initialized properly\n");
    return INIT_ERR;
  }
  
  // allocate subpixel disparity map
  
  // pic9 will be the subDispPic.
  if (pic9 == NULL)
    pic9 = NEW(mm, "subDispPic") JPLPic(mm);
  if (pic9 == NULL ||
      pic9->Init (nrows, ncols, INT16_PIXEL) != NO_ERR) {
    FatalErr ("Cannot allocate pic9 temp space\n");
    return MEM_ERR;
  }

  subDispPic = pic9;
  if (subDispPic->Init (nrows, ncols, INT16_PIXEL) != NO_ERR) {
    FatalErr ("Cannot allocate subpixel map\n");
    return MEM_ERR;
  }
  if (maskPic && maskPic->Init (nrows, ncols, UC8_PIXEL) != NO_ERR) {
    Warning ("MatchStereo:Cannot Initialize maskPic\n");
    maskPic = NULL;
  }

  //	leftRectPic->ShowOnScreen (&debugWindow, "\pLeft Rectified",
  //					false, false, 0, 0);
  //	rightRectPic->ShowOnScreen (&debugWindow, "\pLeft Rectified",
  //					false, false, 0, 0);
  
#ifdef MEM_DEBUG
  printf ( "*** Done allocating subpixel map\n");
#endif

  // Reset values in the stats fields
  ClearFilterCounts ();
  stats.rows = nrows;
  stats.cols = ncols;

#ifdef TIMING__
  Timing (timeStart);
#endif
  
  unsigned char winSize = matchWinSize;
  unsigned char winSizeY = matchWinSizeY;
  long leftRB, rightRB, subDispRB;	// Number of bytes per row
  long startFullCol, endFullCol;		// start and end cols in LEFT image,
					//  measured wrt corr window center.
  long startRow = (winSizeY >> 1) + bound, endRow, row;
  register long col, k;
  long dispCols, pixelCols;	// Actual # cols in DISP img, total image pixel
				// cols processed (spanning corr window too)
  AnythingT dst;		// Always points to LEFTMOST COLUMN in subDispP
  unsigned char *ll, *rr, firstScan;
  unsigned short *sd;		// Index into   subDispPic
  register unsigned char *l, *r, *b;
  register short *c;		// Pointer into    columns   static buffer
  register unsigned char cd, cd_1;	// Current disparity
  register short *firstWinCol, *lastWinCol, curr;
  register MinFullRecord *lrec;
  register MinRecord *rrec;
  register signed char lrlos;
  register long ia, ib;		// Parameters for quadratic fit of sqrt
  register long iscale;		// Scale factor for certainty thresholding
  register long ddd;		// subpixel disparity offset within a pixel
#ifdef POWER_PC
  register long leftPixel, rightPixel;
#endif
  
#ifdef MEM_DEBUG
  printf ( "*** Done allocating local variable\n");
#endif

  // 1 more than the last row index of subDispPic that should be written
  endRow = nrows - (winSizeY >> 1) - bound; // non-inclusive

  // startFullCol is the first possible search column within the LEFT image.
  // This is the column at which the CENTER of the correlation window
  // will be placed.

  if (options & INCOMPLETE_SEARCH) {
    startFullCol = (winSize >> 1) + bound; // 1/2 correlation window + bound
    //    endFullCol = ncold - (winSize >> 1) - bound;
  } else {
    startFullCol = maxDisp + (winSize >> 1) + bound;
    //    endFullCol = ncols - minDisp - (winSize >> 1) - bound;
  }

  // endFullCol is 1 more than the last possible search column index in 
  // the LEFT image.
  endFullCol = ncols - (winSize >> 1) - bound;

  // Total number of columns containing useful data in the disparity
  // image, exactly (not counting the 2*(bound+(winSize>>1)) border pixels
  // and room for the correlation window)
  dispCols = endFullCol - startFullCol;

#ifdef MIN_DEBUG
  printf ( "START and END COLUMN INDEXES:  %d %d\n",
	   startFullCol, endFullCol-1);
#endif

  if (verbosity > 0)
    printf ( "USEFUL RANGE BOUNDS: Columns:  %ld, Rows:  %ld\n",
	     endFullCol - startFullCol, endRow - startRow);

  // Total number of pixels considered in this row (total less the
  // left and right    bound   borders)
  pixelCols = dispCols + winSize - 1;
  
  // Using ia and ib, we perform a quadratic fit to approximate a sqrt:
  // 256*sqrt(6 x^2 + 2), where x is in [-0.5, 0.5].
  // So ia x^2 + ib is the approximation.  Seems to be within 5-10%
  
  ia = (long) ((sqrt(3.5) - sqrt(2.0)) * 4.0 * 256.0);
  ib = (long) (sqrt(2.0) * 256.0);

  // iscale is the area of the correlation window (in pixels)
  iscale = winSize * winSizeY;
  
  // temporary memory (one scan line) 
  // *** may need to be adaptive to the scanline length
  //    P_S_B (typeof(obj), obj, buffer_current_size, desired_size)

  // rightRecord -- disparity and correlation score for all disp columns
  //    plus 0:maxDisp (*not* minDisp; why?  HACK)
  // leftRecord  -- disparity, 3 surrounding correlation values, only for
  //    available number of disp columns

  MinRecord *rightRecord = GetMinRecordBuffer (dispCols + maxDisp + 1);
  MinFullRecord *leftRecord = GetMinFullRecordBuffer
    (dispCols + ((options & INCOMPLETE_SEARCH) ? minDisp : 0));
  unsigned char *uc_buffer = GetUCBuffer
    (pixelCols * winSizeY * disparityRange);

  if (leftRecord == NULL || rightRecord == NULL || uc_buffer == NULL) {
    FatalErr ("Cannot allocate correlation temporary space\n");
    return MEM_ERR;
  }
  
#ifdef MEM_DEBUG
  printf ( "*** Done allocating scanline temp memory; initing disp matrix\n");
#endif
  
  leftRB = leftRectPic->GetRowBytes();
  rightRB = rightRectPic->GetRowBytes();
  subDispRB = subDispPic->GetRowBytes ();
  
  short *columns = GetColumnsBuffer (disparityRange * pixelCols);
  short *disparity_space = GetDisparitySpaceBuffer (disparityRange * pixelCols);
  RejFlagT *rejected_flag = GetRejFlagBuffer (pixelCols);

  if (columns == NULL) {
    printf ( "Unable to allocate scanline temp columns memory!\n");
    return MEM_ERR;
  } 

  // The main result here is   subDispPic   , which will store the
  // short-encoded subpixel disparity values.  Note that there is a black
  // border around this image, since we don't allow the correlator to
  // run off the edge of the image.  Initialize the top border here, all
  // the way down to halfway within the correlation window.

  dst.uc = subDispPic->GetPixelAddress ();
  for (row = 0; row < startRow; row ++, dst.uc += subDispRB)
    for (col = ncols, sd = dst.uh; col--; sd ++)
      *sd = (short) NO_S2_DISP;
  stats.f[BOUND] += MAX(0,startRow) * MAX(0,ncols);
  if (maskPic)
    maskPic->FillBox (0, 0, startRow, ncols, (unsigned char) BOUND);

  if (options & INCOMPLETE_SEARCH) {
    ll = leftRectPic->GetPixelAddress() + bound * (1 + leftRB);
    rr = rightRectPic->GetPixelAddress() + bound * (1 + rightRB);
  } else {
    ll = leftRectPic->GetPixelAddress() + maxDisp + bound * (1 + leftRB);
    rr = rightRectPic->GetPixelAddress() + maxDisp + bound * (1 + rightRB);
  }

  // Now   ll and rr   point to the BEGINING OF THE ROW, offset vertically
  // and horizontally by the input bound, *NOT* by any fraction of the
  // correlation window.  If we disallow INCOMPLETE_SEARCH, forcing us
  // to search only a narrow band of pixels by always starting MaxDisp
  // pixels in from the left, then there is an additional horizontal
  // offset to compensate for maxDisp.

#ifdef MEM_DEBUG
  printf ( "*** Done initializing top rows of disparity image\n");
#endif
  
  // initialize the buffer zone; all disparities within one vertical window

  if (verbosity > 0)
    printf ("RECTIFIED DISP RANGE:  %d %d\n", minDisp, maxDisp);

  // We want to allow as many as 255 disparities, but because we're
  // using an unsigned char for the current disparity, we need to
  // check for minDisp here too.  If we didn't, we'd just loop forever
  // when cd goes from 255 to 0.
  for (cd = minDisp; cd >= minDisp && cd <= maxDisp; cd ++) {
    for (row = 0; row < winSizeY; row ++) {

      // cd is the current disparity, row is counted WITHIN THE
      // CORRELATION WINDOW (so it's relative, varying only from 0 to
      // 7 or whatever).

      c = columns + (cd - minDisp) * pixelCols;
      b = uc_buffer + (cd - minDisp) * winSizeY * pixelCols + row * pixelCols;
      l = ll + row * leftRB;
      r = rr - cd + row * rightRB;
      col = pixelCols;

      // c (columns) points to a row of pixels, one row for each disparity
      // b (uc_buffer) points to a row of pixels, as a function of disparity
      //   and row within the correlation window.
      // Starting just below the bound-ed top subframe, l points into
      //   the LEFT image at the start of the current ROW of the corr window.
      // r points into the RIGHT image, matching l, except that we've
      //   applied the current disparity level by subtracting it from the
      //   starting index.  Note that at the beginnings of rows THIS IS BAD!!
      // col is the number of useful columns that it will be searched,
      //   GIVEN THIS DISPARITY LEVEL.

      if (options & INCOMPLETE_SEARCH) {
	col -= cd;

	// A little inefficient, this just passes over some memory.
	// 0.5 * (minDisp+maxDisp)*disparityRange in columns,
	// 0.5 * (minDisp+maxDisp)*winSizeY*disparityRange in uc_buffer.
	// But it doesn't hurt the algorithm.

	c += cd;
	b += cd;
	l += cd;
	r += cd;

	// During this "incomplete" search (poorly named since it actually
	// results in *more* disparity values), we start searching at cd
	// in the left image, and at the bound border in the right.

	// Skip if the current disparity is larger than the number of
	// available columns (i.e. those not in "bound")

	if (col <= 0) continue;
      }

      if (row == 0) {

	// The first time through this row of the window, initialize
	// columns and uc_buffer with SAD values.

#ifdef POWER_PC
	PPCSADInit (col, l, r, b, c, leftPixel, rightPixel);
#else
	for (; col--; l++, r++, b++, c++) {
	  *c = *b = SAD (*l, *r);
	}
#endif
      } else {

	// Most times through this row of the window, uc_buffer (b) gets
	// a single SAD value.  columns (c) accumulates the SUM of the SAD
	// errors throughout the correlation window (i.e. for all "row"s)

#ifdef POWER_PC
	PPCSADAccumulate (col, l, r, b, c, leftPixel, rightPixel);
#else
	for (; col--; l++, r++, b++, c++) {
	  *c += *b = SAD (*l, *r);
	}
#endif
      }
    }
  }	
  
#ifdef MEM_DEBUG
  printf ( "*** Done pre-loading scanline buffer zone\n");
#endif
  
  // Now that we've preloaded the SAD sums into   columns and uc_buffer
  // from the topmost usable rows in the images, move the left and right
  // image pixel pointers down below the correlation window.

  ll += winSizeY * leftRB;
  rr += winSizeY * rightRB;	
  firstScan = 0;
  
  // FOREACH ROW IN THE ORIGINAL IMAGE:
  // row is in the CENTER OF THE CORRELATION WINDOW

  for (row = startRow; row < endRow;
       row ++, dst.uc += subDispRB, ll += leftRB, rr += rightRB) {

    unsigned char *mask_ptr = maskPic ?
      maskPic->GetPixelAddress (row, startFullCol) : NULL;

    // Initialize NO_DISP pixels on the LEFT border of this scanline
    sd = dst.uh;
    for (col = startFullCol; col > 0; col--, sd++)
      *sd = (short) NO_S2_DISP;
    stats.f[BOUND] += MAX(0,startFullCol);
    if (maskPic)
      maskPic->FillBox (row, 0, 1, startFullCol, (unsigned char) BOUND);

    // COMPUTE OPTIMAL DISPARITY AT EACH COLUMN IN leftRecord and rightRecord
    // -- lR->d is the best 8bit disparity match found so far
    // -- lR->m is the correlation score for the best disparity match
    // -- lR->m0 is either -1 or the correlation score for the prior
    //    disparity (i.e. lR->d - 1).
    // -- lR->m2 is either -1 or the score that follows an optimal disparity
    //    score
    // -- lR->prev is either -1 or the previous score (independent of
    //    the previous score's optimality).  This is just temp storage
    //    to hold what gets copied into m0.

    // Compute correlation values for all disparities within this row
    //
    // We want to allow as many as 255 disparities, but because we're
    // using an unsigned char for the current disparity, we need to
    // check for minDisp here too.  If we didn't, we'd just loop forever
    // when cd goes from 255 to 0.
    for (cd = minDisp; cd >= minDisp && cd <= maxDisp; cd ++) {
      
      // Load up leftRecord and rightRecord
      if (options & INCOMPLETE_SEARCH) {
	rrec = rightRecord;
	lrec = leftRecord + cd;
	col = 0;
	if (cd == minDisp) {

	  // If minDisp is positive, initialize leftmost leftRecord elts
	  // and rightmost rightRecord elements the first time through.

	  for (col = 0; col < minDisp; col++) {
	    leftRecord[col].m = 0x7FFF;
	    leftRecord[col].d = 0;
	    leftRecord[col].m0 = -1;
	    leftRecord[col].m2 = -1;
	    rightRecord[dispCols - col - 1].m = 0x7FFF;
	    rightRecord[dispCols - col - 1].d = 0;
	  }
	} // if (cd == minDisp)

	// compute number of columns remaining to be processed in this
	// scanline.

	col = dispCols - cd;	// HACK!!! Want to add minDisp-col back in 9/1/02
	if (col <= 0) continue;
	firstWinCol = columns + (cd - minDisp) * pixelCols + cd;
      } else {
	rrec = rightRecord;
	/* initialize the head */
	if (cd == minDisp) {
	  for (col = disparityRange; col--; rrec++) {
	    rrec->m = 0x7FFF;
	    rrec->d = cd;
	  }
	}
	// the length
	col = dispCols;		// Number of cols left on the line; this
				// only counts from one window center to
				// another
	lrec = leftRecord;		
	rrec = &rightRecord[maxDisp - cd];
	firstWinCol = columns + (cd - minDisp) * pixelCols;
      }
      // Now lrec, rrec point to the first usable records, firstWinCol
      // points to the first usable column sum

      // ramp-up
      // firstWinCol points to the start of the correlation window in   columns
      // lastWinCol  points to the end of the correlation window in   columns

      lastWinCol = firstWinCol + winSize;

      // curr is the SAD sum throughout the 2D correlation window
      curr = 0;

      for (k = winSize; k--; )
	curr += firstWinCol[k];	// Add Vertical sums over a horizontal window.

      if (cd == minDisp) {
	// FIRST TIME THROUGH:
	// Initialize lrec at all usable columns for this minimum disparity
	// using the previously-computed column sums.
	// m0, m2 are set to -1 to indicate the value stands alone (isn't a
	// local minimum)

#ifdef MIN_DEBUG
	printf ( "%ld,%d  ", row, lrec - leftRecord);
#endif
	for (; col--;
	     lrec++, rrec++, curr += *lastWinCol++, curr -= *firstWinCol++) {
	  lrec->d = rrec->d = cd;

	  // The first time through (at min disparity) we do not use
	  // the "prev" score, but we have to initialize it here so it
	  // can be used as part of the next pass.

	  lrec->m = lrec->prev = rrec->m = curr;
	  lrec->m0 = lrec->m2 = -1;

	  if (disparity_space)
	    disparity_space[(cd - minDisp) * pixelCols + dispCols - (col+1)] =
	      curr;
	  if (rejected_flag)
	    rejected_flag[dispCols - (col+1)] = NO_FLAG;
	}
      } else {
	// EVERY TIME BUT THE FIRST TIME:
	// Compare the current correlation score against the previous
	// best score.  Save whichever is better.
	//	rrec = &rightRecord[maxDisp - cd];
	//	col = dispCols;
	/*#ifdef POWER_PC
	  PPCInnerLoop (col, lrec, rrec, curr, firstWinCol, lastWinCol,
	  cd, leftPixel);
	  #else*/
	cd_1 = cd - 1;
	for (; col--; lrec++, rrec++) {

	  if (disparity_space)
	    disparity_space[(cd - minDisp) * pixelCols + dispCols - (col+1)] =
	      curr;
	  if (rejected_flag)
	    rejected_flag[dispCols - (col+1)] = NO_FLAG;
	  if (mask_ptr)
	    *mask_ptr = NO_FLAG;

	  // Save the MINIMUM "curr" correlation value, at the SMALLEST
	  // disparity where it occurs.

	  if (curr < lrec->m) { // Compare with "best" value from smaller disp
	    lrec->d = cd;
	    lrec->m = curr;
	    lrec->m0 = lrec->prev;
	    lrec->m2 = -1;	// Wipe out any prior "next" score
	  } else if (cd_1 == lrec->d) {
	    // If previous disp was a minimum, save current corr value
	    // for peak-checking and subpixel computation
	    lrec->m2 = curr;
	  }
	  // Save the correlation value in case the next disparity is a
	  // minimum.  Note this is INDEPENDENT from any previously guessed
	  // minimum, it's just used to buffer values until the next pass.
	  lrec->prev = curr;
	  if (curr < rrec->m) {
	    // Save only one correlation value for right image, no need to
	    // interpolate in that image.
	    rrec->d = cd;
	    rrec->m = curr;
	  }
	  // ONLY UPDATE CURR IF THERE ARE VALID DATA PRESENT
	  if (col) {
	    curr += *lastWinCol++;
	    curr -= *firstWinCol++;
	  } /* if */
	} /* for ; col-- */
	//#endif
      } // else
    } // for cd = minDisp

    // Now leftRecord has the values need to output all subDispPic pixels:
    // ->d  is best integer disparity
    // ->m  is best correlation value
    // ->m0 is correlation value at previous disparity
    // ->m2 is correlation value at next disparity
    // ->prev is ignored

    // COMPUTE SUBPIXEL DISPARITY OR RANGE MAP

    // (m0, m, m2) are the three correlation scores around the minimum
    // at d, i.e. the selected integer disparity.  ddd is the subpixel
    // offset, computed by polynomial fit of a quadratic to the three
    // correlation values:
    // a x^2 + b x + c = y, so:
    //   a - b + c = m0   [-1, m0]
    //           c = m	  [ 0, m ]
    //   a + b + c = m2	  [ 1, m2]
    // thus:
    //   a = (m0 + m2 - 2 m)/2
    //   b = (m2 - m0) / 2
    //   c = m
    // The quadratic is minimized when the derivative is zero, i.e.
    //   2ax + b == 0, or   x = -b / 2a
    // This gives us the expression for the subpixel offset

#define SINGLE_IMAGE_THRESHOLD_DEBUG 0

    // Perform the low level subpixel disparity assignment.  We're
    // filtering values stored in the leftRecord buffer, using
    // rightRecord only in the LRLOS check step.  

    lrec = leftRecord;
    rrec = rightRecord;

#define FILTER_SCANLINE(check_rejected,check_threshold) \
    for (col = dispCols; \
	 col--; sd++, lrec++, rrec++) /* Iterate thru leftRecord */ { \
      if (lrec->m0 < 0 || lrec->m2 < 0) { \
	*sd = (short) NO_S2_DISP; \
	stats.f[BORDER]++; \
	if (check_rejected && rejected_flag) \
	  rejected_flag[dispCols - (col+1)] = BORDER; \
	if (mask_ptr) \
	  *mask_ptr = BORDER; \
      } else { \
	if (options & INCOMPLETE_SEARCH) { \
	  lrlos = lrec->d - rrec[-lrec->d].d; \
	} else { \
	  lrlos = lrec->d - rrec[maxDisp - lrec->d].d; \
	} \
	if (lrlos > lrlosLimit || lrlos < -lrlosLimit) { \
	  *sd = (short) NO_S2_DISP; \
	  stats.f[LRLOS]++; \
	  if (check_rejected && rejected_flag) \
	    rejected_flag[dispCols - (col+1)] = LRLOS; \
	  if (mask_ptr) \
	    *mask_ptr = LRLOS; \
	} else { \
	  curr = lrec->m0 + lrec->m2 - (lrec->m << 1); \
	  if (!curr || !(lrec->m0-lrec->m) || !(lrec->m2-lrec->m)) { \
	    /* straight line, no obvious central peak */ \
	    *sd = (short) NO_S2_DISP; \
	    stats.f[FLAT]++; \
	    if (check_rejected && rejected_flag) \
	      rejected_flag[dispCols - (col+1)] = FLAT; \
	    if (mask_ptr) \
	      *mask_ptr = FLAT; \
	  } else { \
	    /* No need to check second derivative, we know this is a global minimum \
	       from how it was assigned. */ \
	    ddd = ((lrec->m0 - lrec->m2) << (DISP_SCALE_SHIFT - 1)) / curr; \
	    if (check_threshold && \
		(iscale * (((ia * ddd * ddd) >> \
			    (2 * DISP_SCALE_SHIFT)) + ib)) \
		>= uncertainty_threshold * curr) { \
	      *sd = (short) NO_S2_DISP; \
	      stats.f[THRESH]++; \
	      if (check_rejected && rejected_flag) \
		rejected_flag[dispCols - (col+1)] = THRESH; \
	      if (mask_ptr) \
		*mask_ptr = THRESH; \
	      if (SINGLE_IMAGE_THRESHOLD_DEBUG) \
	        printf ("%d %d %d ==> %ld >= %ld\n", lrec->m0, lrec->m, \
		        lrec->m2,  (iscale * (((ia * ddd * ddd) >> \
					       (2 * DISP_SCALE_SHIFT)) + ib)), \
		        uncertainty_threshold * curr); \
	    } else { \
	      *sd = (lrec->d << DISP_SCALE_SHIFT) + ddd; \
	      stats.f[ACCEPT]++; \
	      if (mask_ptr) \
		*mask_ptr = ACCEPT; \
	    } \
	  } \
	} \
      } \
      if (mask_ptr) mask_ptr++; \
    }
    
    // We use a macro so the compiler can optimize away the
    // uncertainty threshold internal check.

    if ((options & THRESHOLD_UNCERTAINTY) != 0) {
      FILTER_SCANLINE (1, 1);
    } else {
      FILTER_SCANLINE (1, 0);
    }

#ifdef __unix
    if (rows_to_search &&
	!range_empty (range_intersect
		      (mk_range (row, row),
		       rows_to_search))) {
      WriteDisparitySpace (dst.uh, startFullCol, dispCols, pixelCols,
			   row, bound,
			   use_laplacian_in_disparity_space_image);
    }
#endif

    // Initialize NO_DISP pixels on the RIGHT border of this scanline

    for (col = endFullCol; col < ncols; col++, sd++)
      *sd = (short) NO_S2_DISP;
    stats.f[BOUND] += MAX(0, ncols - endFullCol);
    if (maskPic)
      maskPic->FillBox (row, endFullCol, 1, ncols - endFullCol,
			(unsigned char) BOUND);

    // prepare next row
    // We want to allow as many as 255 disparities, but because we're
    // using an unsigned char for the current disparity, we need to
    // check for minDisp here too.  If we didn't, we'd just loop forever
    // when cd goes from 255 to 0.
    for (cd = minDisp; cd >= minDisp && cd <= maxDisp; cd ++) {
      // subtract the first scan
      b = uc_buffer + (cd - minDisp) * winSizeY * pixelCols +
	firstScan * pixelCols;
      c = columns + (cd - minDisp) * pixelCols;
      col = pixelCols;
      if (options & INCOMPLETE_SEARCH) {
	col -= cd;
	c += cd;
	b += cd;
	if (col <= 0) continue;
      }
      for (; col --; b++, c++)
	*c -= *b;
      // compute & add the new scan
      b = uc_buffer + (cd - minDisp) * winSizeY * pixelCols +
	firstScan * pixelCols;
      c = columns + (cd - minDisp) * pixelCols;
      col = pixelCols;
      l = ll;
      r = rr - cd;
      if (options & INCOMPLETE_SEARCH) {
	col -= cd;
	c += cd;
	b += cd;
	l += cd;
	r += cd;
      }
#ifdef POWER_PC
      PPCSADAccumulate (col, l, r, b, c, leftPixel, rightPixel);
#else
      for (; col --; b++, c++, l++, r++)
	*c += *b = SAD (*l, *r);
#endif
    }
    
    firstScan++;
    if (firstScan == winSizeY) firstScan = 0;
  }
  
  for (row = endRow; row < nrows; row ++, dst.uc += subDispRB) 	
    for (col = 0, sd = dst.uh; col < ncols;
	 col ++, sd ++)
      *sd = (short) NO_S2_DISP;
  stats.f[BOUND] += MAX (0, nrows - endRow) * MAX (0, ncols);
  if (maskPic)
    maskPic->FillBox (endRow, 0, nrows - endRow, ncols,
		       (unsigned char) BOUND);
  
#ifdef IMG_DEBUG
  static int sdp_no = 0;

  snprintf (buf, bufLen, "out%d.pic", sdp_no++);
  subDispPic->Write (buf);
#endif

#ifdef TIMING__
  SHOW_TIME ("Time for correl, minim, subpix, lrlos");
#endif
  /*	
	SmoothSubpixelDisparityMap ((short *) subDispPic->GetPixelAddress (), 
	subDispPic->rows, subDispPic->cols, subDispPic->GetRowBytes ());
#ifdef TIMING__
  SHOW_TIME ("Time for smoothing");
#endif
#ifdef MEM_DEBUG
  printf ( "*** Done smoothing disparities\n");
#endif
  */	
  
  long nr, ns;

  if (options & REMOVE_OVERHANGS)
      if (RemoveOverhangsFromDisparityImage (subDispPic,
					     overhang_world_up) != NO_ERR)
	fprintf (stderr, "*** Failed to remove overhangs, plodding on\n");

  BlobFiltering (subDispPic->GetShortPixelAddress (), 
		 subDispPic->rows, subDispPic->cols,
		 subDispPic->GetRowBytes (), 
		 INT_MAX (min_blob_regions_to_allocate,
			  (int) (subDispPic->rows * subDispPic->cols *
				 blob_region_fraction)),
		 ((fixed_blob_filter_size < 0) ?
		  (int) ((float) -fixed_blob_filter_size * 0.01 *
			 subDispPic->rows * subDispPic->cols) : 
		  fixed_blob_filter_size),
		 I_DISP_SCALE >> 2, I_DISP_SCALE >> 2, &nr, &ns);
  /*		 minBlobSize, xGradThreshold, yGradThreshold,
		 &nr, &ns); */

#ifdef BLOB_DEBUG
  printf ( "nr=%ld, ns=%ld\n", nr, ns);
#endif
#ifdef IMG_DEBUG
  snprintf (buf, bufLen, "out%d.pic", sdp_no++);
  subDispPic->Write (buf);
#endif

#ifdef TIMING__
  SHOW_TIME ("Time for blob filtering");
#endif
  
#ifdef MEM_DEBUG
  printf ( "*** Finished with MatchStereo; freeing mem\n");
#endif
  
  //	if (buffer) DELETEV (mm, buffer);
  //	if (columns) DELETEV (mm, columns);
  //	if (leftRecord) DELETEV (mm, leftRecord);
  //	if (rightRecord) DELETEV (mm, rightRecord);
  
#ifdef MEM_DEBUG
  printf ( "*** All done; returning\n");
#endif
  
  return NO_ERR;
}

