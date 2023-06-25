#include "JPLPic.h"
#include "JPLStereo.h"
#include "Timing.h"

#ifdef TIMING__
#include "Timing.h"
#define SHOW_TIME(str) (show_timing ? TIME(str) : 0)
#endif

/*
  This version reduces the temp memory by 90% comparing to MatchStereo().
  But it has increased the amount of computations.
  
  This version suits those machines with limited cache size.
  
  It is particularly germane to SIMD-style parallization.
  
  But on G3, it is about 6% slower than MatchStereo().
*/

long			
JPLStereo::MatchStereo2 (unsigned char bound)
{
  unsigned char disparityRange = maxDisp - minDisp + 1;
  long nrows = 0, ncols = 0;
  
  range_params_good = 0;
  if (leftRectPic== NULL || rightRectPic == NULL ||
      (nrows = leftRectPic->rows) != rightRectPic->rows ||
      (ncols = leftRectPic->cols) != rightRectPic->cols) {
    FatalErr ("Rectified images are not initialized properly\n");
    return INIT_ERR;
  }
  
  JPLPic *pic1 = GetPic1 ();

  if (pic1 == NULL) {
    pic1 = NEW(mm, "pic1") JPLPic(mm);
    if (pic1 == NULL ||
	pic1->Init (nrows, ncols, INT16_PIXEL) != NO_ERR) {
      FatalErr ("Cannot allocate pic1 temp space\n");
      return MEM_ERR;
    }
  }
  
  // allocate subpixel disparity map
  // if (subDispPic) DELETE (mm, subDispPic);
  
  JPLPic *pic9 = GetPic9 ();

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
  
  //	leftRectPic->ShowOnScreen (&debugWindow, "\pLeft Rectified",
  //					false, false, 0, 0);
  //	rightRectPic->ShowOnScreen (&debugWindow, "\pLeft Rectified",
  //					false, false, 0, 0);
  
#ifdef TIMING__
  Timing (timeStart);
#endif
  
  unsigned char winSize = matchWinSize;
  long leftRB, rightRB;
  long subDispRB, startCol, endCol;
  long startRow = (winSize >> 1) + bound, endRow, row;
  register long col;
  long dispCols, pixelCols;
  AnythingT dst;
  unsigned char *ll, *rr, firstScan;
  unsigned short *sd;
  register unsigned char *l, *r;
  register short *c;
  register unsigned char cd, cd_1;
  register short *firstCol, *lastCol, curr;
  register MinFullRecord *lrec;
  register MinRecord *rrec;
  register char lrlos;
#ifdef POWER_PC
  register long leftPixel, rightPixel;	// temporary registers for assembly code.
#endif
  
  endRow = nrows - (winSize >> 1) - bound; // non-inclusive
  startCol = maxDisp + (winSize >> 1) + bound;
  endCol = ncols - minDisp - (winSize >> 1) - bound;
  dispCols = endCol - startCol;
  pixelCols = dispCols + winSize - 1;
  
  // temporary memory (one scan line) 
  // *** may need to be adaptive to the scanline length
  
  MinRecord *rightRecord = GetMinRecordBuffer (dispCols + maxDisp + 1);
  MinFullRecord *leftRecord = GetMinFullRecordBuffer (dispCols);
  
  if (leftRecord == NULL || rightRecord == NULL) {
    FatalErr ("Cannot allocate correlation temporary space\n");
    // if (rightRecord) DELETEV (mm, rightRecord);
    // if (leftRecord) DELETEV (mm, leftRecord);
    return MEM_ERR;
  }
  
  leftRB = leftRectPic->GetRowBytes();
  rightRB = rightRectPic->GetRowBytes();
  subDispRB = subDispPic->GetRowBytes ();
  
  short *columns = GetColumnsBuffer (disparityRange * pixelCols);
  if (columns == NULL) {
    printf ( "Unable to allocate temp columns memory!\n");
    return MEM_ERR;
  } 
  
  dst.uc = subDispPic->GetPixelAddress ();
  for (row = 0; row < startRow; row ++, dst.uc += subDispRB)
    for (col = ncols, sd = dst.uh; col--; sd ++)
      *sd = NO_S2_DISP;
  
  ll = leftRectPic->GetPixelAddress() + maxDisp + bound;
  rr = rightRectPic->GetPixelAddress() + maxDisp + bound;
  
  // initialize the buffer zone
  // We want to allow as many as 255 disparities, but because we're
  // using an unsigned char for the current disparity, we need to
  // check for minDisp here too.  If we didn't, we'd just loop forever
  // when cd goes from 255 to 0.
  for (cd = minDisp; cd >= minDisp && cd <= maxDisp; cd ++) {
    for (row = 0; row < winSize; row ++) {
      c = columns + (cd - minDisp) * pixelCols;
      l = ll + row * leftRB;
      r = rr - cd + row * rightRB;
      col = pixelCols;
      if (row == 0) {
#ifdef POWER_PC
	PPCSADInit2(col, l, r, c, leftPixel, rightPixel);
#else
	for (; col--; l++, r++, c++) {
	  *c = SAD (*l, *r);
	}
#endif
      } else {
#ifdef POWER_PC
	PPCSADAccumulate2(col, l, r, c, leftPixel, rightPixel);
#else
	for (; col--; l++, r++, c++) {
	  *c += SAD (*l, *r);
	}
#endif
      }
    }
  }	
  
  ll += winSize * leftRB;
  rr += winSize * rightRB;	
  firstScan = 0;
  
  for (row = startRow; row < endRow; row ++, dst.uc += subDispRB, ll += leftRB, rr += rightRB) {
    
    for (col = 0, sd = dst.uh; col < startCol; col ++, sd++)
      *sd = NO_S2_DISP;
    
    // main actions: computing correlation values
    // We want to allow as many as 255 disparities, but because we're
    // using an unsigned char for the current disparity, we need to
    // check for minDisp here too.  If we didn't, we'd just loop forever
    // when cd goes from 255 to 0.
    for (cd = minDisp; cd >= minDisp && cd <= maxDisp; cd ++) {
      
      lrec = leftRecord;		
      
      firstCol = columns + (cd - minDisp) * pixelCols;
      lastCol = firstCol + winSize;
      curr = 0;
      for (col = winSize; col--; )
	curr += firstCol[col];
      if (cd == minDisp) {
	rrec = rightRecord;
	for (col = disparityRange; col--; rrec ++) {
	  rrec->m = 0x7FFF;
	  rrec->d = cd;
	}
	for (col = dispCols; col--; lrec++, rrec++, curr += *lastCol++, curr -= *firstCol++) {
	  lrec->d = rrec->d = cd;
	  lrec->m = lrec->prev = rrec->m = curr;
	}
      } else {
	rrec = &rightRecord[maxDisp - cd];
	col = dispCols;
	/*#ifdef POWER_PC
	  PPCInnerLoop (col, lrec, rrec, curr, firstCol, lastCol,
	  cd, leftPixel);
	  #else*/
	cd_1 = cd - 1;
	for (; col--; lrec++, rrec++, curr += *lastCol++, curr -= *firstCol++) {
	  if (curr < lrec->m) {
	    lrec->d = cd;
	    lrec->m = curr;
	    lrec->m0 = lrec->prev;
	  } else if (cd_1 == lrec->d) {
	    lrec->m2 = curr;
	  }
	  lrec->prev = curr;
	  if (curr < rrec->m) {
	    rrec->d = cd;
	    rrec->m = curr;
	  }
	}
	//#endif
      }
    }
    // compute subpixel disparity or range map
    lrec = leftRecord;
    rrec = rightRecord;
    for (col = dispCols; col--; sd++, lrec++, rrec++) {
      if (lrec->d == minDisp || lrec->d == maxDisp) {
	*sd = NO_S2_DISP;
      } else {
				// LRLOS
	lrlos = lrec->d - rrec[maxDisp - lrec->d].d;
	if (!(lrlos & 0xFE) || ((lrlos & 0xff) == 0xff)) {
	  curr = lrec->m0 + lrec->m2 - (lrec->m << 1);
	  if (!curr) 
	    *sd = NO_S2_DISP;
	  else 
	    *sd = (lrec->d << DISP_SCALE_SHIFT) +
	      ((lrec->m0 - lrec->m2) << (DISP_SCALE_SHIFT - 1)) / curr;
	} else 
	  *sd = NO_S2_DISP;
      }
    }
    
    for (col = endCol; col < ncols; col++, sd++)
      *sd = NO_S2_DISP;
    
    // prepare next row
    // We want to allow as many as 255 disparities, but because we're
    // using an unsigned char for the current disparity, we need to
    // check for minDisp here too.  If we didn't, we'd just loop forever
    // when cd goes from 255 to 0.
    for (cd = minDisp; cd >= minDisp && cd <= maxDisp; cd ++) {
      // subtract the first scan
      c = columns + (cd - minDisp) * pixelCols;
      l = ll - winSize * leftRB;
      r = rr - winSize * rightRB - cd;
      col = pixelCols;
#ifdef POWER_PC
      PPCSADSubtract2(col, l, r, c, leftPixel, rightPixel);
#else
      for (; col --; c++, l++, r++)
	*c -= SAD (*l, *r);
#endif
    }
    // We want to allow as many as 255 disparities, but because we're
    // using an unsigned char for the current disparity, we need to
    // check for minDisp here too.  If we didn't, we'd just loop forever
    // when cd goes from 255 to 0.
    for (cd = minDisp; cd >= minDisp && cd <= maxDisp; cd ++) {
      // compute & add the new scan
      c = columns + (cd - minDisp) * pixelCols;
      col = pixelCols;
      l = ll;
      r = rr - cd;
#ifdef POWER_PC
      PPCSADAccumulate2(col, l, r, c, leftPixel, rightPixel);
#else
      for (; col --; c++, l++, r++)
	*c += SAD (*l, *r);
#endif
    }
    
    firstScan++;
    if (firstScan == winSize) firstScan = 0;
  }
  
  for (row = endRow; row < nrows; row ++, dst.uc += 	subDispRB) 	
    for (col = 0, sd = dst.uh; col < ncols; col ++, sd ++)
      *sd = NO_S2_DISP;
  
  
#ifdef TIMING__
  SHOW_TIME ("Time for correlation, minimization, subpixel, lrlos");
#endif
  /*	
	SmoothSubpixelDisparityMap ((short *) subDispPic->GetPixelAddress (), 
	subDispPic->rows, subDispPic->cols, subDispPic->GetRowBytes ());
  */	
#ifdef TIMING__
  SHOW_TIME ("Time for smoothing");
#endif
  
  long nr, ns;
  BlobFiltering (subDispPic->GetShortPixelAddress (), 
		 subDispPic->rows, subDispPic->cols,
		 subDispPic->GetRowBytes (),
		 INT_MAX (min_blob_regions_to_allocate,
			  (int) (subDispPic->rows * subDispPic->cols *
				 blob_region_fraction)),
		 (int) (0.04 * subDispPic->rows *subDispPic->cols),
		 I_DISP_SCALE >> 2, I_DISP_SCALE >> 2, &nr, &ns);
  
#ifdef TIMING__
  SHOW_TIME ("Time for blob filtering");
#endif
  
  // 	if (columns) DELETEV (mm, columns);
  //	if (leftRecord) DELETEV (mm, leftRecord);
  //	if (rightRecord) DELETEV (mm, rightRecord);
  	
  return NO_ERR;
} /* MatchStereo2 */

