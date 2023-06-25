#include "nav_memory.h"
#include "JPLPic.h"




#if defined(VXWORKS)
#define PREPARE_STATIC_BUFFER(type,buf,sizevar,newsize) \
	if ((buf) == NULL || (sizevar) < (newsize)) { \
	  int heap_avail = memPartFindMax (memSysPartId); \
	  if (buf) DELETEV (mm, buf); \
	  sizevar = (newsize); \
	 /* error(ERR(0), ERR_INFO, ERF_COMMAND, ">>> Allocating %s: %ld %ss\n", # buf, newsize, # type);*/ \
	  if (heap_avail < (int) (sizevar * sizeof(type))) { \
	    error(ERR(1), ERR_MAJOR, ERF_COMMAND, "### Not Enough Memory for Static Buffer "); \
	    error(ERR(2), ERR_MAJOR, ERF_COMMAND, "%s\n### %d < %ld\n", # buf, heap_avail, (long) sizevar * sizeof(type)); \
	    sizevar = 0; buf = NULL; \
	  } else { \
	    if (heap_avail < (int) (2*sizevar * sizeof(type))) { \
	      error(ERR(3), ERR_MINOR, ERF_COMMAND, "*** WARNING! Heap free space dangerously "); \
	      error(ERR(4), ERR_MINOR, ERF_COMMAND, "low\n*** Was at %d, about to allocate %ld\n", \
		heap_avail, sizevar * sizeof(type)); \
	    } \
	    buf = NEW(mm, #buf "(" #type ")") type[sizevar]; \
	  } \
	}
#else
#define PREPARE_STATIC_BUFFER(type,buf,sizevar,newsize) \
	if ((buf) == NULL || (sizevar) < (newsize)) { \
	  if (buf) DELETEV (mm, buf); \
	  sizevar = (newsize); \
	 /* error(ERR(5), ERR_INFO, ERF_COMMAND, ">>> Allocating %s: %ld bytes\n", # buf, newsize);*/ \
	  buf = NEW(mm, #buf "(" #type ")") type[sizevar]; \
	}
#endif






typedef unsigned char BlobPixelT;
static long *same = NULL;
static short *region = NULL;
static long same_size = 0L, region_size = 0L, intensities_size = 0L;
static unsigned char *intensities = NULL;
static short *slidingSum1 = NULL, *slidingSum2 = NULL;
static long slide_size1 = 0, slide_size2 = 0;



long JPLPic::BlobFiltering (long maxNumRegions, long minRegionSize,
			    long *numRegions, long *pointsFiltered,
			    unsigned char background_pixel,
			    long *max_region_row, long *max_region_col,
			    long *max_region_pixels)
{
  long *equiv, *regsize;

  if (GetPixelType() != UC8_PIXEL) {
    error(ERR(14), ERR_MINOR, ERF_COMMAND, "ERROR: Can only run blob filter on 8bit image, "
	     "not type %ld\n", GetPixelType());
    return PARAM_ERR;
  }
  PREPARE_STATIC_BUFFER (long, same, same_size, 3*(maxNumRegions+1));
  if (same == NULL) {
    error(ERR(15), ERR_MAJOR, ERF_COMMAND, "Cannot allocate temp memory for   same\n");
    return MEM_ERR;
  }
  PREPARE_STATIC_BUFFER (short, region, region_size, rows * cols);
  if (region == NULL) {
    error(ERR(16), ERR_MAJOR, ERF_COMMAND, "Cannot allocate temp memory for   region\n");
    // if (same) DELETEV (mm, same);
    return MEM_ERR;
  }
  if (max_region_row && max_region_col) {
    PREPARE_STATIC_BUFFER (unsigned char, intensities, intensities_size,
			   maxNumRegions+1);
    if (same == NULL) {
      error(ERR(17), ERR_MAJOR, ERF_COMMAND, "Cannot allocate temp memory for   intensities\n");
      return MEM_ERR;
    }
  }

  equiv = same + maxNumRegions + 1;
  regsize = equiv + maxNumRegions + 1;	// Number of pixels per region

  long points_filtered = 0;
  long region_id = 1;
  int reg_u, reg_l;
  int up = -cols;
  short *reg = region;
  int i, j, last_i, last_j;
  int iy;
  BlobPixelT *image;
  BlobPixelT *lastRow;

  // Label the different regions
  same[0] = same[1] = 0;
  regsize[0] = 0;
  regsize[1] = 1;

  for (iy = 0; iy < rows; iy++) {
    image = (BlobPixelT *) GetPixelAddress(iy,0);
    int ix;

    for (ix = 0, lastRow = image - GetRowBytes();
	 ix < cols; ix++, image++, lastRow++, reg++) {

      BlobPixelT this_pix = image[0];

      reg[0] = 0;

      int x_l = (ix > 0) && (reg[-1] > 0) && (this_pix == image[-1]);
      int x_u = (iy > 0) && (reg[up] > 0) && (this_pix == *lastRow);

      // Connect to any neighbors
      if (x_u && !x_l) {
	reg_u = reg[up];	// Same as UP pixel only - set region ID
	reg[0] = reg_u;
	regsize[reg_u]++;
      } else if (x_l && !x_u) {
	reg_l = reg[-1];	// Same as LEFT pixel only - set region ID
	reg[0] = reg_l;
	regsize[reg_l]++;
      } else if (x_l && x_u) {
	reg_l = reg[-1];	// Same as both LEFT and UP Pixels
	reg_u = reg[up];
	reg[0] = reg_l;
	regsize[reg_l]++;
	if (reg_l != reg_u) {
	  i = reg_l, last_i = -1;

	  // Walk down the list of same regions, till you hit 0
	  while (i != 0) {
	    last_i = i;
	    i = same[i];
	  }
	  j = reg_u, last_j = -1;

	  while (j != 0) {
	    last_j = j;
	    j = same[j];
	  }

	  // Merge formerly unconnected regions; lower indices win
	  if (last_i > last_j)
	    same[last_i] = last_j;
	  else if (last_j > last_i)
	    same[last_j] = last_i;
	}
      } else {
	// Starting a new region; !x_l && !x_u
	reg[0] = region_id++;
	if (region_id > maxNumRegions) {
	  error(ERR(18), ERR_MINOR, ERF_COMMAND, "BlobFiltering:  too many regions");
	  error(ERR(19), ERR_MINOR, ERF_COMMAND, ", >%ld at row %d col %d\n", maxNumRegions, iy, ix);
	  return MEM_ERR;
	}
	same[region_id] = 0;
	regsize[region_id] = 1;
      }
      if (max_region_row && max_region_col)
	intensities[reg[0]] = this_pix;
    }
  }

  // Flatten the equivalence lists
  for (j = 0; j < region_id; j++)
    equiv[j] = 0;

  for ( j = 0; j < region_id; j++) {
    i = j;
    last_i = 0;
    int merged_size = 0;
    while (i != 0) {
      merged_size += regsize[i];
      regsize[i] = 0;
      last_i = i;
      i = same[i];
    }
    equiv[j] = last_i;
    regsize[last_i] = merged_size;
  }

  // Compress the region IDs to the smallest numbers possible

  for (j = 0; j < region_id; j++)
    same[j] = 0;
  int unused_index = 0;
  for (j = 1; j < region_id; j++) {
    long equiv_j = equiv[j];
    if (!same[equiv_j]) {
      same[equiv_j] = ++unused_index;
      regsize[unused_index] = regsize[equiv_j];
    }
  }
  for (j = 1; j < region_id; j++)
    equiv[j] = same[equiv[j]];

  reg = region;
  image = GetPixelAddress(0,0);

  for (iy = rows; iy--; ) {
    for (int ix = cols; ix--; image++, reg++) {
      if ((*reg == 0) || (regsize[equiv[*reg]] < minRegionSize)) {
	*image = background_pixel;
	if (max_region_row && max_region_col)
	  intensities[equiv[*reg]] = background_pixel;
	points_filtered++;
      }
    }
  }

  if (numRegions) *numRegions = region_id;
  if (pointsFiltered) *pointsFiltered = points_filtered;


  // return a pixel contained inside a largest region that doesn't
  // use the background_pixel value (note we're overloading that
  // parameter here; up above it's used to plug into holes in the
  // image, now we're skipping its value). 

  if (max_region_row && max_region_col) {
    int max_region_id = -1, max_region_size = 0;

    for (j = 0; j < region_id; j++)
      if (regsize[equiv[j]] > max_region_size &&
	  intensities[equiv[j]] != background_pixel) {
	max_region_id = equiv[j];
	max_region_size = regsize[max_region_id];
      }

    *max_region_row = *max_region_col = -1;
    if (max_region_size > 0) {
      reg = region;

      for (long y = 0; *max_region_row == -1 && y < rows; y++) {
	for (long x = 0; x < cols; x++, reg++) {
	  if (equiv[*reg] == max_region_id) {
	    *max_region_row = y;
	    *max_region_col = x;
	    break;
	  }
	}
      }
      if (max_region_pixels)
	*max_region_pixels = max_region_size;
    }
  }
  return NO_ERR;
}



#define SCALE_BITS	12

// Fast smooth using sliding sum

long 
JPLPic::SmoothBySlidingSum (JPLPic *newPic, unsigned char winSizeX, 
			    unsigned char winSizeY, unsigned char bound_pixel)
{
  long srcRB, dstRB, bufferCols,  pixelCols;
  register long scale;
  unsigned char *src, *dst, *firstScan;
  register unsigned char *s, *d;
  register short *c, *first;
  long row, startRow, endRow, startCol, endCol, totalRows;
  register long col, curr;
  
  if (field) {
    FatalErr("Cannot smooth without decimating an interlaced frame\n");
    return INIT_ERR;
  }
  
  dstRB = newPic->GetRowBytes ();
  dst = newPic->GetPixelAddress ();
  
  bufferCols = cols;
  startCol = winSizeX >> 1;
  startRow = winSizeY >> 1;
  endRow = rows - (winSizeY >> 1);
  endCol = cols - (winSizeX >> 1);
  pixelCols = endCol - startCol;
  totalRows = endRow - startRow;
  
  scale = (1L << SCALE_BITS) / (winSizeX * winSizeY);
  
  PREPARE_STATIC_BUFFER(short, slidingSum1, slide_size1, bufferCols);
  if (slidingSum1 == NULL) {
    FatalErr ("Cannot allocate temp memory\n");
    return MEM_ERR;
  }
  
  for (row = 0; dst && row < startRow; row ++, dst += dstRB)
    for (col = cols, d = dst; col --; d++)
      *d = bound_pixel;
  
  // prepare for the first scanline
  src = pixels;
  srcRB = rowBytes;
  for (row = 0; row < winSizeY ; row ++, src += srcRB)
    if (row == 0)
      for (col = bufferCols, s = src, c = slidingSum1; col--; s++, c++) 
	*c = *s;
    else
      for (col = bufferCols, s = src, c = slidingSum1; col--; s++, c++) 
	*c += *s;
  
  for (row = totalRows; dst && row --; src += srcRB, dst += dstRB) {
    firstScan = src - winSizeY * srcRB;
    
    for (col = startCol, d = dst; col--; d ++)
      *d = bound_pixel;
    
    curr = 0;	
    first = slidingSum1;
    for (col = winSizeX, c = slidingSum1; col--; c++)
      curr += *c;
    
    for (col = pixelCols; col--; curr += *c++, curr -= *first++, d++)
      *d = (curr * scale) >> SCALE_BITS;
    for (col = endCol; col < cols; col++, d++)	
      *d = bound_pixel;
    
    // prepare for the next scanline
    if (row == 0) continue;
    if (winSizeY > 1) {
      for (col = bufferCols, s = src, c = slidingSum1; col --; s++, c++)
	*c += *s;
      for (col = bufferCols, s = firstScan, c = slidingSum1; col--; s++, c++)
	*c -= *s;
    } else
      for (col = bufferCols, s = src, c = slidingSum1; col --; s++, c++)
	*c = *s;
  }
  for (row = endRow; dst && row < rows; row++, dst+= dstRB)
    for (col = cols, d = dst; col--; d++)
      *d = bound_pixel;
  
  return NO_ERR;
}



// HACK HACK  This method is overly complicated; clean it up.  In particular
// there was an off by one error that caused the sliding sum buffer to
// be read one short beyond its end.  Although that's been fixed for cols=512,
// it would be better to recode the whole thing.

long 
JPLPic::SmoothAndDecimateBySlidingSum (JPLPic *newPic, unsigned char winSizeX, 
						unsigned char bound_pixel)
{
  long srcRB, dstRB, bufferCols, pixelCols;
  register long scale;
  unsigned char *src, *dst, *firstScan;
  register unsigned char *s, *d;
  register short *c, *first;
  long row, startRow, endRow, startCol, endCol, totalRows;
  unsigned char winSizeY;
  register long col, curr, evenOddOffsetX, evenOddOffsetY;
  long newRows, newCols;
  
  if (newPic == NULL)
    return PARAM_ERR;
  
  if (pixelType != UC8_PIXEL) {
    Warning ("SmoothAndDecimateBySlidingSum: Input not 8 bit image!");
    return PARAM_ERR;
  }
  
  if ((newPic->rows != (rows+1) >> 1) ||
      (newPic->cols != (cols+1) >> 1)) {
    Warning ("SmoothAndDecimateBySlidingSum:  Bad Decimated image size!");
    return PARAM_ERR;
  }
  
  // Set the smoothing window vertical size
  
  if (field) {
    winSizeY = winSizeX >> 2;
    winSizeY = (winSizeY << 1) + 1;
  } else {
    winSizeY = winSizeX;
  }
  
  dstRB = newPic->GetRowBytes ();
  dst = newPic->GetPixelAddress ();
  
  // The buffer is exactly as wide as the original image.
  
  bufferCols = cols;
  
  // startRow and startcol   skip over half of the smoothing window
  
  startRow = winSizeY >> 1;
  if (startRow & 0x01) { // odd
    evenOddOffsetY = 1;
    startRow ++; // need to start on an even row
  } else {
    evenOddOffsetY = 0;
  }
  startCol = winSizeX >> 1;
  if (startCol & 0x01) {  // odd
    evenOddOffsetX = 1;
    startCol ++;
  } else
    evenOddOffsetX = 0;
  
  // endRow and endCol   stop 1/2 smoothing window shy of the 
  // actual end of the ORIGINAL image
  
  if (field) 
    endRow = ((rows + 1) >> 1) - (winSizeY >> 1);
  else
    endRow = rows - (winSizeY >> 1);
  endCol = cols - (winSizeX >> 1);
  
  // New image will be half the size of the original
  
  newRows = (rows + 1) >> 1;
  newCols = (cols + 1) >> 1;
  
  // Number of rows & cols in original image that will be decimated
  
  pixelCols = endCol - startCol;
  totalRows = endRow - startRow;
  
  // Scale factor used for summing values according to window
  // size.
  
  scale = (1L << SCALE_BITS) / (winSizeX * winSizeY);
  
  // Allocate the number of columns in the original image
  
  PREPARE_STATIC_BUFFER(short, slidingSum1, slide_size1, bufferCols);
  if (slidingSum1 == NULL) {
    FatalErr ("Cannot allocate temp memory\n");
    return MEM_ERR;
  }
  
  // Zero out a few top rows in the destination image; just those
  // fitting in the top HALF of the DECIMATED smoothing window
  
  for (row = (field ? startRow : (startRow >> 1)); dst && row--; dst += dstRB)
    for (col = newCols, d = dst; col --; d++)
      *d = bound_pixel;
  
  // prepare for the first scanline
  if (field)	
    srcRB = rowBytes << 1;
  else
    srcRB = rowBytes;
  src = pixels + evenOddOffsetY * srcRB;
  
  // Initialize the buffer memory with the full sum from a column.
  
  for (row = 0; row < winSizeY ; row ++, src += srcRB)
    if (row == 0)
      for (col = bufferCols, s = src, c = slidingSum1; col--; s++, c++) 
	*c = *s;
    else
      for (col = bufferCols, s = src, c = slidingSum1; col--; s++, c++) 
	*c += *s;
  
  // Now   src   points at the top row AFTER a window-size
  // and   dst   points after a QUARTER window-size
  
  // FILL IN THE DESTINATION IMAGE
  
  for (row = 0; dst && row < totalRows; row++, src += srcRB) {
    
    // firstScan   points back to the BEGINNING of the original
    // image area, at the EARLIEST part that contributed to the
    // running sums in the buffer area.
    
    firstScan = src - winSizeY * srcRB;
    
    if (field || (!(row & 0x01))) { // even row
      
      // Zero out the LEFT HAND PART of the destination image row
      
      for (col = startCol >> 1, d = dst; col--; d ++)
	*d = bound_pixel;
      
      // Sum up the column sums in the leftmost window
      
      curr = 0;	
      first = slidingSum1 + evenOddOffsetX;
      for (col = 0, c = first; col < winSizeX; c++, col++)
	curr += *c;
      
      // Apply the column sums to write the destination
      // image
      
      for (col = 0; col< pixelCols - evenOddOffsetX;
	   col++, curr += *c++, curr -= *first++) {
	if (!(col & 0x01)) {
	  *d++ = (curr * scale) >> SCALE_BITS;
	}
      }
      
      // Zero out the RIGHT HAND PART of the destination image row
      
      for (col = d - dst; col < newCols; col++)
	*d++ = bound_pixel;
      
      dst += dstRB;
    }
    
    // prepare for the next scanline
    if (row == (totalRows-1)) continue;
    if (winSizeY > 1) {
      for (col = bufferCols, s = firstScan, c = slidingSum1; col--; s++, c++)
	*c -= *s;
      for (col = bufferCols, s = src, c = slidingSum1; col --; s++, c++)
	*c += *s;
    } else { // one row only. No need to calculate
      for (col = bufferCols, s = src, c = slidingSum1; col--; s++, c++) 
	*c = *s;
    }
  }
  
  // Pad the bottom part of the destination image with zeros
  
  for (row = (field ? endRow : ((endRow+1) >> 1)); dst && row < newRows; row++, dst+= dstRB)
    for (col = newCols, d = dst; col--; d++)
      *d = bound_pixel;
  
  newPic->field = false;
  return NO_ERR;
}



/* same as above except the result image is 16-bit*/

long 
JPLPic::SmoothAndDecimateBySlidingSumB (JPLPic *newPic, unsigned char winSizeX, 
					unsigned char bound_pixel)
{
  long srcRB, dstRB, bufferCols, pixelCols;
  unsigned char *src, *firstScan;
  AnythingT dst;
  register unsigned char *s;
  register unsigned short *d;
  register short *c, *first;
  long row, startRow, endRow, startCol, endCol, totalRows;
  unsigned char winSizeY;
  register long col, curr, evenOddOffsetX, evenOddOffsetY;
  long newRows, newCols;
  
  if (field) {
    winSizeY = winSizeX >> 2;
    //		if (winSizeY == 0) winSizeY = 1;
    winSizeY = (winSizeY << 1) + 1;
  } else {
    winSizeY = winSizeX;
  }
  
  dstRB = newPic->GetRowBytes ();
  dst.uc = newPic->GetPixelAddress ();
  
  bufferCols = cols;
  startRow = winSizeY >> 1;
  if (startRow & 0x01) { // odd
    evenOddOffsetY = 1;
    startRow ++; // need to start on an even row
  } else {
    evenOddOffsetY = 0;
  }
  startCol = winSizeX >> 1;
  if (startCol & 0x01) {  // odd
    evenOddOffsetX = 1;
    startCol ++;
  } else
    evenOddOffsetX = 0;
  
  if (field) 
    endRow = ((rows + 1) >> 1) - (winSizeY >> 1);
  else
    endRow = rows - (winSizeY >> 1);
  endCol = cols - (winSizeX >> 1);
  newRows = (rows + 1) >> 1;
  newCols = (cols + 1) >> 1;
  // decimated rows & cols
  pixelCols = endCol - startCol;
  totalRows = endRow - startRow;
  
  //	scale = (1L << SCALE_BITS) / (winSizeX * winSizeY);
  
  PREPARE_STATIC_BUFFER(short, slidingSum1, slide_size1, bufferCols);
  if (slidingSum1 == NULL) {
    FatalErr ("Cannot allocate temp memory\n");
    return MEM_ERR;
  }
  
  for (row = (field ? startRow : (startRow >> 1)); row--;
       dst.uc += dstRB)
    for (col = newCols, d = dst.uh; col --; d++)
      *d = bound_pixel;
  
  // prepare for the first scanline
  if (field)	
    srcRB = rowBytes << 1;
  else
    srcRB = rowBytes;
  src = pixels + evenOddOffsetY * srcRB;
  for (row = 0; row < winSizeY ; row ++, src += srcRB)
    if (row == 0)
      for (col = bufferCols, s = src, c = slidingSum1; col--; s++, c++) 
	*c = *s;
    else
      for (col = bufferCols, s = src, c = slidingSum1; col--; s++, c++) 
	*c += *s;
  
  for (row = 0; row < totalRows; row++, src += srcRB) {
    firstScan = src - winSizeY * srcRB;
    
    if (field || (!(row & 0x01))) { // even row
      for (col = startCol >> 1, d = dst.uh; col--; d++)
	*d = bound_pixel;
      
      curr = 0;	
      first = slidingSum1 + evenOddOffsetX;
      for (col = winSizeX, c = first; col--; c++)
	curr += *c;
      
      for (col = 0; col< pixelCols; col++, curr += *c++, curr -= *first++) {
	if (!(col & 0x01)) {
	  *d++ = curr;
	}
      }
      for (col = (endCol+1)>>1; col < newCols; col++, d++)	
	*d = bound_pixel;
      
      dst.uc += dstRB;
    }
    
    // prepare for the next scanline
    if (row == (totalRows-1)) continue;
    if (winSizeY > 1) {
      for (col = bufferCols, s = firstScan, c = slidingSum1; col--; s++, c++)
	*c -= *s;
      for (col = bufferCols, s = src, c = slidingSum1; col --; s++, c++)
	*c += *s;
    } else { // one row only. No need to calculate
      for (col = bufferCols, s = src, c = slidingSum1; col--; s++, c++) 
	*c = *s;
    }
  }
  for (row = (field ? endRow : ((endRow+1) >> 1)); row < newRows; 
       row++, dst.uc += dstRB)
    for (col = newCols, d = dst.uh; col--; d++)
      *d = bound_pixel;
  
  newPic->field = false;
  return NO_ERR;
}




/* Difference of average :
	Assume winSize1 > winSize2
*/

#define OFFSET_SCALED	(126L << SCALE_BITS)

long 
JPLPic::DoGBySlidingSum (JPLPic *newPic, unsigned char winSize1X,
			 unsigned char winSize1Y, unsigned char winSize2X,
			 unsigned char winSize2Y, unsigned char bound_pixel)
{
  long srcRB, dstRB, bufferCols, pixelCols;
  register long scale1, scale2;
  unsigned char *src1, *src2, *dst, *firstScan1, *firstScan2;
  register unsigned char *s, *d;
  register short *c1, *c2, *first1, *first2;
  long row, startRow, endRow, startCol, endCol, totalRows;
  unsigned char rowDiff, colDiff;
  register long col, curr1, curr2, newPixel;
  
  if (field) {
    FatalErr ("Cannot DoG without decimating an interlaced frame\n");
    return INIT_ERR;
  }
  
  rowDiff = (winSize1Y >> 1) - (winSize2Y >> 1);
  colDiff = (winSize1X >> 1) - (winSize2X >> 1);
	
  dstRB = newPic->GetRowBytes ();
  dst = newPic->GetPixelAddress ();
  
  bufferCols = cols;
  startRow = winSize1X >> 1;
  startCol = winSize1Y >> 1;
  endCol = cols - (winSize1X >> 1);
  endRow = rows - (winSize1Y >> 1);
  pixelCols = endCol - startCol;
  totalRows = endRow - startRow;
  
  scale1 = (1L << SCALE_BITS) / (winSize1X * winSize1Y);
  scale2 = (1L << SCALE_BITS) / (winSize2X * winSize2Y);
  
  
  PREPARE_STATIC_BUFFER(short, slidingSum1, slide_size1, bufferCols);
  PREPARE_STATIC_BUFFER(short, slidingSum2, slide_size2, bufferCols);
  
  if (slidingSum1 == NULL || slidingSum2 == NULL) {
    FatalErr ("Cannot allocate temp memory\n");
    return MEM_ERR;
  }
  
  if (dst)
    for (row = 0; row < startRow; row ++, dst += dstRB)
      for (col = cols, d = dst; col --; d++)
	*d = bound_pixel;
  
  // prepare for the first scanline
  src1 = pixels;
  srcRB = rowBytes;
  // for window 1
  for (row = 0; row < winSize1Y ; row ++, src1 += srcRB)
    if (row == 0)
      for (col = bufferCols, s = src1, c1 = slidingSum1; col--; s++, c1++) 
	*c1 = *s;
    else
      for (col = bufferCols, s = src1, c1 = slidingSum1; col--; s++, c1++) 
	*c1 += *s;
  // for window 2
  src2 = pixels + rowDiff * srcRB;
  for (row = 0; row < winSize2Y; row++, src2 += srcRB)
    if (row == 0)
      for (col = bufferCols, s = src2, c2 = slidingSum2; col--; s++, c2++) 
	*c2 = *s;
    else
      for (col = bufferCols, s = src2, c2 = slidingSum2; col--; s++, c2++) 
	*c2 += *s;
  
  if (dst)
    for (row = totalRows; row --; src1 += srcRB, src2 += srcRB, dst += dstRB) {
      firstScan1 = src1 - winSize1Y * srcRB;
      firstScan2 = src2 - winSize2Y * srcRB;
      
      for (col = startCol, d = dst; col--; d ++)
	*d = bound_pixel;
      
      curr1 = 0;	
      first1 = slidingSum1;
      for (col = winSize1X, c1 = first1; col--; c1++)
	curr1 += *c1;
      curr2 = 0;
      first2 = slidingSum2 + colDiff;
      for (col = winSize2X, c2 = first2; col--; c2++)
	curr2 += *c2;
      
      for (col = pixelCols; col--; d++) {
	newPixel = (OFFSET_SCALED - curr1 * scale1 + curr2 * scale2);
	if (newPixel < 0)
	  *d = 0;
	else if (newPixel > (255L << SCALE_BITS))
	  *d = 255;
	else
	  *d = newPixel >> SCALE_BITS;
	if (col) {
	  curr1 += *c1++;
	  curr1 -= *first1++;
	  curr2 += *c2++;
	  curr2 -= *first2++;
	}
      }
      for (col = endCol; col < cols; col++, d++)	
	*d = bound_pixel;
      
      // prepare for the next scanline
      if (row == 0) continue;
      for (col = bufferCols, s = firstScan1, c1 = slidingSum1; col--; s++, c1++)
	*c1 -= *s;
      for (col = bufferCols, s = src1, c1 = slidingSum1; col --; s++, c1++)
	*c1 += *s;
      if (winSize2Y > 1) {
	for (col = bufferCols, s = firstScan2, c2 = slidingSum2; col--; s++, c2++)
	  *c2 -= *s;
	for (col = bufferCols, s = src2, c2 = slidingSum2; col --; s++, c2++)
	  *c2 += *s;
      } else {
	for (col = bufferCols, s = src2, c2 = slidingSum2; col --; s++, c2++)
	  *c2 = *s;
      }
    }
  if (dst)
    for (row = endRow; row < rows; row++, dst+= dstRB)
      for (col = cols, d = dst; col--; d++)
	*d = bound_pixel;
  
  return NO_ERR;
	
}



long 
JPLPic::DoGAndDecimateBySlidingSum (JPLPic *newPic, unsigned char winSize1X, 
				    unsigned char winSize2X, unsigned char bound_pixel)
{
  long srcRB, dstRB, bufferCols, pixelCols;
  register long scale1, scale2;
  unsigned char *src1, *src2, *dst, *firstScan1, *firstScan2;
  register unsigned char *s, *d;
  register short *c1, *c2, *first1, *first2;
  long row, startRow, endRow, startCol, endCol, totalRows;
  unsigned char rowDiff, colDiff;
  unsigned char winSize1Y, winSize2Y;
  register long col, curr1, curr2, newPixel, evenOddOffsetX, evenOddOffsetY;
  long newRows, newCols;
  
  if (field) {
    winSize1Y = winSize1X >> 2;
    winSize1Y = (winSize1Y << 1) + 1;
    winSize2Y = winSize2X >> 2;
    winSize2Y = (winSize2Y << 1) + 1;
  } else {
    winSize1Y = winSize1X;
    winSize2Y = winSize2X;
  }
  
  rowDiff = (winSize1Y >> 1) - (winSize2Y >> 1);
  colDiff = (winSize1X >> 1) - (winSize2X >> 1);
  
  dstRB = newPic->GetRowBytes ();
  dst = newPic->GetPixelAddress ();
  
  bufferCols = cols;
  startRow = winSize1Y >> 1;
  if (startRow & 0x01) { // odd
    evenOddOffsetY = 1;
    startRow ++; // need to start on an even row
  } else {
    evenOddOffsetY = 0;
  }
  startCol = winSize1X >> 1;
  if (startCol & 0x01) {  // odd
    evenOddOffsetX = 1;
    startCol ++;
  } else
    evenOddOffsetX = 0;
  
  if (field) 
    endRow = ((rows + 1) >> 1) - (winSize1Y >> 1);
  else
    endRow = rows - (winSize1Y >> 1);
  endCol = cols - (winSize1X >> 1);
  newRows = (rows + 1) >> 1;
  newCols = (cols + 1) >> 1;
  // decimated rows & cols
  pixelCols = endCol - startCol;
  totalRows = endRow - startRow;
  
  scale1 = (1L << SCALE_BITS) / (winSize1X * winSize1Y);
  scale2 = (1L << SCALE_BITS) / (winSize2X * winSize2Y);
  
  
  PREPARE_STATIC_BUFFER(short, slidingSum1, slide_size1, bufferCols);
  PREPARE_STATIC_BUFFER(short, slidingSum2, slide_size2, bufferCols);
  
  if (slidingSum1 == NULL || slidingSum2 == NULL) {
    FatalErr ("Cannot allocate temp memory\n");
    return MEM_ERR;
  }
  
  if (dst)
    for (row = (field ? startRow : (startRow >> 1)); row--; dst += dstRB)
      for (col = newCols, d = dst; col --; d++)
	*d = bound_pixel;
  
  // prepare for the first scanline
  if (field)	
    srcRB = rowBytes << 1;
  else
    srcRB = rowBytes;
  src1 = pixels + evenOddOffsetY * srcRB;
  // for window 1
  for (row = 0; row < winSize1Y ; row ++, src1 += srcRB)
    if (row == 0)
      for (col = bufferCols, s = src1, c1 = slidingSum1; col--; s++, c1++) 
	*c1 = *s;
    else
      for (col = bufferCols, s = src1, c1 = slidingSum1; col--; s++, c1++) 
	*c1 += *s;
  // for window 2
  src2 = pixels + (rowDiff + evenOddOffsetY) * srcRB;
  for (row = 0; row < winSize2Y; row++, src2 += srcRB)
    if (row == 0)
      for (col = bufferCols, s = src2, c2 = slidingSum2; col--; s++, c2++) 
	*c2 = *s;
    else
      for (col = bufferCols, s = src2, c2 = slidingSum2; col--; s++, c2++) 
	*c2 += *s;
  
  if (dst)
    for (row = 0; row < totalRows; row++, src1 += srcRB, src2 += srcRB) {
      firstScan1 = src1 - winSize1Y * srcRB;
      firstScan2 = src2 - winSize2Y * srcRB;
      
      if (field || (!(row & 0x01))) { // even row
	for (col = startCol >> 1, d = dst; col--; d ++)
	  *d = bound_pixel;
	
	curr1 = 0;	
	first1 = slidingSum1 + evenOddOffsetX;
	for (col = winSize1X, c1 = first1; col--; c1++)
	  curr1 += *c1;
	curr2 = 0;
	first2 = slidingSum2 + colDiff + evenOddOffsetX;
	for (col = winSize2X, c2 = first2; col--; c2++)
	  curr2 += *c2;
	
	for (col = 0; col< pixelCols; col++, curr1 += *c1++, curr1 -= *first1++, 
	       curr2 += *c2++, curr2 -= *first2++) {
	  if (!(col & 0x01)) {
	    newPixel = (OFFSET_SCALED - curr1 * scale1 + curr2 * scale2);
	    if (newPixel < 0)
	      *d++ = 0;
	    else if (newPixel > (255L << SCALE_BITS))
	      *d++ = 255;
	    else
	      *d++ = newPixel >> SCALE_BITS;
	  }
	}
	for (col = (endCol+1)>>1; col < newCols; col++, d++)	
	  *d = bound_pixel;
	
	dst += dstRB;
      }
      
      // prepare for the next scanline
      if (row == (totalRows-1)) continue;
      for (col = bufferCols, s = firstScan1, c1 = slidingSum1; col--; s++, c1++)
	*c1 -= *s;
      for (col = bufferCols, s = src1, c1 = slidingSum1; col --; s++, c1++)
	*c1 += *s;
      if (winSize2Y > 1) {
	for (col = bufferCols, s = firstScan2, c2 = slidingSum2; col--; s++, c2++)
	  *c2 -= *s;
	for (col = bufferCols, s = src2, c2 = slidingSum2; col --; s++, c2++)
	  *c2 += *s;
      } else { // one row only. No need to calculate
	for (col = bufferCols, s = src2, c2 = slidingSum2; col--; s++, c2++) 
	  *c2 = *s;
      }
    }
  if (dst)
    for (row = (field ? endRow : ((endRow+1) >> 1)); row < newRows; row++, dst+= dstRB)
      for (col = newCols, d = dst; col--; d++)
	*d = bound_pixel;
  
  
  newPic->field = false;
  return NO_ERR;
	
}



#define GRADIENT_MAX_COLS   2048 

long
JPLPic::Gradients (JPLPic *gradx, JPLPic *grady, 
		   unsigned char winSizeX, unsigned char winSizeY,
		   unsigned char shiftBits, int bound_pixel)
{
  long srcRB, dstXRB, dstYRB, bufferCols, pixelCols;
  unsigned char *src, *firstScan;
  AnythingT dstX, dstY;
  register unsigned char *s;
  register short *dx, *dy;
  register short *c, *first;
  long row, startRow, endRow, startCol, endCol, totalRows;
  register long col, curr;
  long currHist[3*GRADIENT_MAX_COLS];
  long *cp, *cp_2, cRow;
	
  if (field) {
    FatalErr("Cannot deal with an interlaced frame\n");
    return INIT_ERR;
  }
	
  dstXRB = gradx->GetRowBytes ();
  dstX.uc = gradx->GetPixelAddress ();
  dstYRB = grady->GetRowBytes ();
  dstY.uc = grady->GetPixelAddress ();
	
  bufferCols = cols;
  startCol = winSizeX >> 1;
  startRow = winSizeY >> 1;
  endRow = rows - (winSizeY >> 1);
  endCol = cols - (winSizeX >> 1);
  pixelCols = endCol - startCol;
  totalRows = endRow - startRow;
	
  //  scale = (1L << SCALE_BITS) / (winSizeX * winSizeY);
	
  PREPARE_STATIC_BUFFER(short, slidingSum1, slide_size1, bufferCols);
  if (slidingSum1 == NULL) {
    FatalErr ("Cannot allocate temp memory\n");
    return MEM_ERR;
  }
	
  for (row = 0; row < startRow; row ++, dstX.uc += dstXRB, dstY.uc += dstYRB)
    for (col = cols, dx = dstX.h, dy = dstY.h; col --; dx++, dy++)
      *dx = *dy = bound_pixel;
	
  // prepare for the first scanline
  src = pixels;
  srcRB = rowBytes;
  for (row = 0; row < winSizeY ; row ++, src += srcRB)
    if (row == 0)
      for (col = bufferCols, s = src, c = slidingSum1; col--; s++, c++) 
	*c = *s;
    else
      for (col = bufferCols, s = src, c = slidingSum1; col--; s++, c++) 
	*c += *s;
	
  cRow = 0;
  dstY.uc -= dstYRB;
  for (row = 0; row < totalRows; row ++,
	 src += srcRB, dstX.uc += dstXRB, dstY.uc += dstYRB) {
    firstScan = src - winSizeY * srcRB;
		
    for (col = startCol, dx = dstX.h, dy = dstY.h; col--; dx ++, dy++)
      *dx = *dy = bound_pixel;
    *dx ++ = bound_pixel;
		
    curr = 0;	
    first = slidingSum1;
    for (col = winSizeX, c = slidingSum1; col--; c++)
      curr += *c;
	
    cp = &currHist[cRow * GRADIENT_MAX_COLS];
    for (col = 0; col < pixelCols; col++, 
	   curr += *c++, curr -= *first++) {
      *cp++ = curr;
    }

    // gradientX
    cp = &currHist[cRow * GRADIENT_MAX_COLS + 2];
    for (col = pixelCols - 2; col --; cp++, dx++) {
       *dx = ((cp[0] - cp[-2]) >> (shiftBits + 1));
    }
    *dx++ = bound_pixel;

    // gradY
    if (row == 1) {
      for (col = pixelCols; col --; dy++)
	*dy = bound_pixel;
    } else if (row >= 2) {
      cp = &currHist[cRow * GRADIENT_MAX_COLS];
      cp_2 = &currHist[((cRow + 1) % 3) * GRADIENT_MAX_COLS];
      for (col = pixelCols; col --; dy++, cp++, cp_2++)
	*dy = (*cp - *cp_2) >> (shiftBits + 1);
    } else {
      dy += pixelCols;
    }
			
    for (col = endCol; col < cols; col++, dx++, dy++)
      *dx = *dy = bound_pixel;
    cRow ++;
    if (cRow >= 3) cRow = 0;
    // prepare for the next scanline
    if (row == (totalRows - 1)) continue;
    if (winSizeY > 1) {
      for (col = bufferCols, s = src, c = slidingSum1; col --; s++, c++)
	*c += *s;
      for (col = bufferCols, s = firstScan, c = slidingSum1; col--; s++, c++)
	*c -= *s;
    } else
      for (col = bufferCols, s = src, c = slidingSum1; col --; s++, c++)
	*c = *s;
  }

  for (col = cols, dy = dstY.h; col--; dy++)
    *dy = bound_pixel;
  dstY.uc += dstYRB;
    
  for (row = endRow; row < rows; row++, dstX.uc += dstXRB, dstY.uc += dstYRB)
    for (col = cols, dx = dstX.h, dy = dstY.h; col--; 
	 dx++, dy++)
      *dx = *dy = bound_pixel;
			
  return NO_ERR;
}



// Gradient in x direction using [-1/2 0 1/2]
// the result is offset by 128

long 
JPLPic::GradientX (JPLPic *gradImg)
{
  long row, col;
  unsigned char *src, *dst;
  register unsigned char *s, *d;
  register long p;
  long srcRB, dstRB;
  
  if (pixelType != UC8_PIXEL ||
      gradImg->GetPixelType () != UC8_PIXEL ||
      rows !=  gradImg->rows ||
      cols !=  gradImg->cols ||
      cols <= 2) {
    FatalErr ("Image type or size not consistent\n");
    return INIT_ERR;
  }
  
  src = (unsigned char *) pixels;
  dst = (unsigned char *) gradImg->GetPixelAddress ();
  srcRB = rowBytes;
  dstRB = gradImg->GetRowBytes ();
  
  for (row = 0; row < rows; row ++, src += srcRB, dst +=dstRB) {
    
    s = src;
    d = dst;
    // the first column
    *d++ = 0;
    
    for (col = cols - 2, d++; col--; s++, d++) {
      p = *s;
      p = *(s+2) - p + 255;
      *d = p >> 1;
    }
    
    // last column
    *d = 0;
  }
  
  return NO_ERR;
}



long 
JPLPic::GradientXB (JPLPic *gradImg, int offset)
{
  long row, col;
  AnythingT src, dst;
  register unsigned short *s, *d;
  register long p;
  long srcRB, dstRB;
  
  if (pixelType != INT16_PIXEL ||
      gradImg->GetPixelType () != INT16_PIXEL ||
      rows !=  gradImg->rows ||
      cols !=  gradImg->cols ||
      cols <= 2) {
    FatalErr ("Image type or size not consistent\n");
    return INIT_ERR;
  }
  
  src.uc = pixels;
  dst.uc = gradImg->GetPixelAddress ();
  srcRB = rowBytes;
  dstRB = gradImg->GetRowBytes ();
  
  for (row = 0; row < rows; row ++, src.uc += srcRB, dst.uc +=dstRB) {
    
    s = src.uh;
    d = dst.uh;
    // the first column
    *d++ = 0;
    
    for (col = cols - 2, d++; col--; s++, d++) {
      p = *s;
      p = *(s+2) - p;
      *d = (p + offset);
    }
    
    // last column
    *d = 0;
  }
  
  return NO_ERR;
}



// Gradient in y direction

long
JPLPic::GradientY (JPLPic *gradImg)
{
  long row, col;
  unsigned char *src, *dst, *prevRow, *nextRow;
  register unsigned char *s1, *s2, *d;
  register long p;
  long srcRB, dstRB;
  
  if (pixelType != UC8_PIXEL ||
      gradImg->GetPixelType () != UC8_PIXEL ||
      rows !=  gradImg->rows ||
      cols !=  gradImg->cols || rows <= 2 || cols <= 0) {
    FatalErr ("Image type or size not consistent\n");
    return INIT_ERR;
  }
  
  src = (unsigned char *) pixels;
  dst = (unsigned char *) gradImg->GetPixelAddress ();
  srcRB = rowBytes;
  dstRB = gradImg->GetRowBytes ();
  
  // the first row
  for (d = dst, col = cols; col --; d++)
    *d = 0;
  prevRow = src;
  nextRow = src + 2 * srcRB;
  
  dst += dstRB;
  
  for (row = 1; row < rows - 1; row ++, 
	 prevRow += srcRB, nextRow += srcRB, dst +=dstRB) {
    
    s1 = prevRow;
    s2 = nextRow;
    d = dst;
    for (col = cols; col--; s1++, s2++, d++) {
      p = *s1;
      p = *s2 - p + 255;
      *d = p >> 1;
    }
  }
  // the last row
  for (d = dst, col = cols; col --; d++)
    *d = 0;
  
  return NO_ERR;
}




long
JPLPic::GradientYB (JPLPic *gradImg, int offset)
{
  long row, col;
  AnythingT src, dst, prevRow, nextRow;
  register unsigned short *s1, *s2, *d;
  register long p;
  long srcRB, dstRB;
  
  if (pixelType != INT16_PIXEL ||
      gradImg->GetPixelType () != INT16_PIXEL ||
      rows !=  gradImg->rows ||
      cols !=  gradImg->cols || rows <= 2 || cols <= 0) {
    FatalErr ("Image type or size not consistent\n");
    return INIT_ERR;
  }
  
  src.uc = pixels;
  dst.uc = gradImg->GetPixelAddress ();
  srcRB = rowBytes;
  dstRB = gradImg->GetRowBytes ();
  
  // the first row
  for (d = dst.uh, col = cols; col --; d++)
    *d = 0;
  prevRow.uc = src.uc;
  nextRow.uc = src.uc + 2 * srcRB;
  
  dst.uc += dstRB;
  
  for (row = 1; row < rows - 1; row ++, 
	 prevRow.uc += srcRB, nextRow.uc += srcRB, dst.uc +=dstRB) {
    
    s1 = prevRow.uh;
    s2 = nextRow.uh;
    d = dst.uh;
    for (col = cols; col--; s1++, s2++, d++) {
      p = *s1;
      p = *s2 - p;
      *d = p + offset;
    }
  }
  // the last row
  for (d = dst.uh, col = cols; col --; d++)
    *d = 0;
  
  return NO_ERR;
}


int JPLPic::BV_GXGY(int x, int y,  double *bv, double *gx, double *gy)
{
  /*this check point can be removed when the code is finalized*/
  unsigned char *src, *s;
  register long row_bytes;
  register long p00, p01, p02, p10, p12, p20, p21, p22;
  
  src = GetPixelAddress();
  row_bytes = GetRowBytes(); 
  s = src + (y-1) * row_bytes + x -1;
  
  p00 = *s++;
  p01 = *s++;
  p02 = *s;
  s += row_bytes;
  p12 = *s--;
  *bv = *s--;
  p10 = *s;
  s += row_bytes;
  p20 = *s++;
  p21 = *s++;
  p22 = *s;
  
  *gx = (float)(p02 - p00)/2.0 + (float)(p12 - p10)/2.0*1.414 + (float)(p22 - p20)/2.0;
  *gx = (*gx)/3.414;
  *gy = (float)(p20 - p00)/2.0 + (float)(p21 - p01)/2.0*1.414 + (float)(p22 - p02)/2.0;
  *gy = (*gy)/3.414;
  return NO_ERR;
}


void JPLPic::FreeStaticBuffers (void)
{
  FreeJPLPicStaticBuffers (mm);
}



void FreeJPLPicStaticBuffers (JMemoryManager *mm)
{
  if (slidingSum1) { DELETEV (mm, slidingSum1); }
  if (slidingSum2) { DELETEV (mm, slidingSum2); }
  if (same) { DELETEV (mm, same); }
  if (region) { DELETEV (mm, region); }
  if (intensities) { DELETEV (mm, intensities); }
}
