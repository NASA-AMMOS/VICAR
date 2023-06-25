#include <string.h>	// for memcpy
#include "JPLStereo.h"


char *JPLStereo::RejFlag2s (RejFlagT flag)
{
  switch (flag) {
    // If there is no flag associated with a given disparity, that's
    // because it passed all the local tests, but failed some later
    // global test (e.g. Blob Filter, Overhang Filter).
  case NO_FLAG:  return "NOFLAG";
  case ACCEPT: return "ACCEPT";
  case BLOB:  return "BLOB";
  case BOUND:  return "BOUND";
  case BORDER:  return "BORDER";
  case FLAT:  return "FLAT";
  case LRLOS:  return "LRLOS";
  case OVERHANG: return "OVRHNG";
  case THRESH:  return "THRESH";
  case VEHICLE: return "VEHCLE";
  default: break;
  }

  return "UNKNOWN";
} // RejFlag2s



/*!
  Return a color for the given flag.  The color is an address of 4
  unsigned char bytes that encode the ARGB pixel value.

  \param flag Filter flag

  \return Valid address of a 4-byte color encoding, Alpha, Red, Green,
  Blue.  Guaranteed not to be NULL.
*/

unsigned char *JPLStereo::GetFilterColor (RejFlagT flag)
{
  static unsigned char none[4] = { 0, 0, 0, 0 };	// black
  static unsigned char accept[4] = { 0, 0, 255, 0 };	// green
  static unsigned char blob[4] = { 0, 0, 0, 128 };	// deep blue
  static unsigned char border[4] = { 0, 0, 0, 255 };	// blue
  static unsigned char bound[4] = { 0, 192, 192, 192 }; // light grey
  static unsigned char flat[4] = { 0, 255, 0, 255 };	// lavender
  static unsigned char lrlos[4] = { 0, 0, 255, 255 };	// aqua
  static unsigned char overhang[4] = { 0, 128, 0, 0 };	// deep red
  static unsigned char thresh[4] = { 0, 255, 255, 0 };	// yellow
  static unsigned char vehicle[4] = { 0, 255, 128, 128 }; // light red

  switch (flag) {
  case ACCEPT: return accept;
  case BLOB:  return blob;
  case BOUND:  return bound;
  case BORDER:  return border;
  case FLAT:  return flat;
  case LRLOS:  return lrlos;
  case OVERHANG: return overhang;
  case THRESH:  return thresh;
  case VEHICLE: return vehicle;
  default:
  case NO_FLAG:
    break;
  }

  return none;
}


void JPLStereo::WriteFilterLabels (JPLPic *image, long ulrow, long ulcol,
				   long rows, long cols,
				   StereoFilterCountT *counts)
{
  int char_indent = 1;
  int filters_used = REJ_FLAGS;
  int ii;
  const size_t bufLen = 300;
  char buf[bufLen];
  int cw = 7;	// width of a character (incl space) in pixels
  int ch = 10;  // height of a character (incl space) in pixels

  if (counts) {
    for (ii = 0; ii < REJ_FLAGS; ii++)
      if (counts->f[ii] == 0)
	filters_used--;
  }

  int prefix_len = MAX(1, cols / (cw * filters_used) - 1);
  int out_rows = 1;

  if (image == NULL ||
      ulrow > image->rows || ulcol > image->cols ||
      ulrow+rows < 0 || ulcol + cols < 0)
    return;

  if (prefix_len < 4 && cols >= ch * 2) {
    out_rows = rows / ch;

    prefix_len = MAX (1,
		      cols /
		      (cw * ((filters_used + out_rows - 1) / out_rows))-1);
  }

  // Write a label for each Reject flag
  for (ii = 0; ii < REJ_FLAGS; ii++) {
    if (counts && counts->f[ii]) {
      snprintf (buf, bufLen, "%.*s", prefix_len, RejFlag2s ((RejFlagT) ii));
      image->LoadColorMapEntry (0, GetFilterColor((RejFlagT) ii), 4);
      if ((int) (char_indent + strlen(buf)) * cw > cols && out_rows > 1) {
	ulrow += ch;
	char_indent = 1;
      }
      image->Text (ulrow, ulcol + char_indent * cw,
		   buf, 0, 0, 1);
      char_indent += strlen (buf) + 1;
    } // if
  } // for
} // JPLStereo::WriteFilterLabels



void JPLStereo::WriteDisparitySpace (unsigned short *disp_vals, long startCol,
				     long dispCols, long pixelCols, long row,
				     unsigned char bound, int use_laplacian)
{
  const size_t bufLen = 1000;
  char buf[bufLen];
  int dsrows = maxDisp + 1 - minDisp;
  int dscols = pixelCols;
  
  short mins = NO_S2_DISP, maxs = NO_S2_DISP;
  int ii, end = dsrows * dscols;
  
  // Get access to the static buffers, but no need to specify
  // the allocation amount, just use zero
  short *disparity_space = GetDisparitySpaceBuffer (0);
  RejFlagT *rejected_flag = GetRejFlagBuffer (0);
  MinFullRecord *leftRecord = GetMinFullRecordBuffer (0);
  
  // Compute the min and max short-valued SAD scores
  
  for (ii = 0; ii < end; ii++) {
    if (disparity_space[ii] != NO_S2_DISP && disparity_space[ii]) {
      if (mins == NO_S2_DISP || disparity_space[ii] < mins)
	mins = disparity_space[ii];
      if (maxs == NO_S2_DISP || disparity_space[ii] > maxs)
	maxs = disparity_space[ii];
    }
  }
  
  // Desired approx size of the whole image (with labels)
  long dsimage_rows = 512, dsimage_cols = 512;
  // Total Size of the text labels around the image (top + bottom)
  long dsimage_text_rows = 40;
  long dsimage_text_left_cols = 4 + 7 * 3;
  long dsimage_text_right_cols = 4 + 7 * 8;
  // Size of the scanlines and surrounding context
  long dsimage_window_rows = matchWinSizeY * 4 + 15;

  // Compute an image scale that will fit within the desired bounds
  int rscale = MAX (1,
		    (dsimage_rows -
		     (dsimage_text_rows + dsimage_window_rows)) / dsrows);
  int cscale = MAX (1,
		    (dsimage_cols -
		     (dsimage_text_left_cols +
		      dsimage_text_right_cols)) / dscols);
  
  JPLPic dispspace(mm), subpic(mm);
  
  // Allocate space for the whole annotated image
  dispspace.Init (dsrows * rscale +
		  dsimage_text_rows + dsimage_window_rows,
		  dscols * cscale + dsimage_text_left_cols +
		  dsimage_text_right_cols,
		  ARGB32_PIXEL);

  // Initialize background to white
  dispspace.LoadGreyColorMap();
  dispspace.FillBox (0, 0, dispspace.rows, dispspace.cols, 255);

  // Extract just the image part of the annotated image
  if (dispspace.SubImage (&subpic, dsimage_text_rows / 2,
			  dsimage_text_left_cols,
			  dsrows * rscale, dscols * cscale) != NO_ERR)
    Warning ("Failed to create disparity space subimage!");
  else {
    int tr, tc;
    // Figure out where to put the left and right scanlines
    int bottom_table_row = dispspace.rows - dsimage_window_rows;
    
    // Label the disparity space image
    snprintf (buf, bufLen, "%ldx%ld, %ld cols, [%d:%d], Row %ld",
	     leftRectPic->rows, leftRectPic->cols, pixelCols,
	     mins, maxs, row);
    dispspace.TextBoundingBox (buf, 1, &tr, &tc);
    dispspace.Text (4, MAX(0, dispspace.cols - tc) / 2, buf, 0, 0, 1);
    
    // Label vertical Disparity axis
    snprintf (buf, bufLen, "%3d", minDisp);
    dispspace.Text (dsimage_text_rows / 2 + rscale / 2 - 5, 4,
		    buf, 0, 0, 1);
    snprintf (buf,bufLen, "%3d", maxDisp);
    dispspace.Text (bottom_table_row - (dsimage_text_rows / 2 + rscale / 2 +
					5), 4,
		    buf, 0, 0, 1);
    snprintf (buf, bufLen, "Lo-\nres\n\nDsp");
    dispspace.TextBoundingBox (buf, 1, &tr, &tc);
    dispspace.Text ((bottom_table_row - tr) / 2, 4,
		    buf, 0, 0, 1);
    
    // Label vertical Range axis
    
    if (CreateRangeParams () == NO_ERR) {
      int rinc = 1 + 10 / rscale;
      int rval;
      
      for (rval = 0; rval <= maxDisp - minDisp; rval += rinc) {
	float rng;
	
	if (rval + minDisp < 1)
	  rng = 1e6;
	else
	  rng = baseline / ((rval + minDisp) * I_DISP_SCALE);
	
	snprintf (buf, bufLen,"%.3gm", rng);
	dispspace.Text (dsimage_text_rows / 2 + rval * rscale +
			rscale / 2 - 5,
			dispspace.cols + 4 - dsimage_text_right_cols,
			buf, 0, 0, 1);
      }
    }
    
    // Label horizontal Column axis
    snprintf (buf, bufLen, "%d", bound);
    dispspace.Text (bottom_table_row + 2 - (dsimage_text_rows / 2),
		    dsimage_text_left_cols,
		    buf, 0, 0, 1);
    snprintf (buf, bufLen, "%3ld", leftRectPic->cols-(bound+1));
    dispspace.Text (bottom_table_row + 2 - (dsimage_text_rows / 2),
		    dsimage_text_left_cols + subpic.cols - 3 * 7,
		    buf, 0, 0, 1);
    snprintf (buf, bufLen, "Columns");
    dispspace.TextBoundingBox (buf, 1, &tr, &tc);	
    dispspace.Text (bottom_table_row + 2 - (dsimage_text_rows / 2),
		    (dsimage_text_left_cols + subpic.cols - tc) / 2,
		    buf, 0, 0, 1);
    
    // Deal with the colormap
    
    unsigned char save[4];

    memcpy (save, subpic.GetColorMapEntry (0),
	    subpic.BytesPerPixel());
    
    int ds_disp, ds_col;
    short *cp = disparity_space;

    JPLPic dspic(mm);

    const size_t bufLen = 50;
    char buf[bufLen];
    snprintf (buf, bufLen, "ds%ld.pic", row);
    if (dspic.LoadFromMemory ((unsigned char *) disparity_space,
			      dsrows, dscols, INT16_PIXEL) == NO_ERR)
      dspic.Write (buf);

    unsigned char missing[4] = { 0, 255, 0, 0 };	// red
    unsigned char chosen[4];

    memcpy (chosen, GetFilterColor(ACCEPT), 4);

    subpic.LoadColorMapEntry (0, missing, sizeof(missing));
    subpic.FillBox (0, 0, subpic.rows, subpic.cols, 0);
    subpic.LoadColorMapEntry (0, save, sizeof(save));
    
    if (mins < maxs) {
      // Copy the disparity space values for display
      
      for (ds_disp = 0; ds_disp < dsrows; ds_disp++) {
	for (ds_col = 0; ds_col < dscols; ds_col++) {

	  if (*cp == 0) {

	    // Highlight any SAD value of 0 by coloring it specially
	    // This will happen only where no value was computed
	    // (e.g. at unrealizable disparities, or in synthetic images
	    // with an absolutely plain background)

	    subpic.LoadColorMapEntry (0, missing, sizeof(missing));
	    subpic.FillBox (ds_disp * rscale, ds_col * cscale,
			    rscale, cscale, 0);
	    subpic.LoadColorMapEntry (0, save, sizeof(save));
	  } else {
	    // Add an offset (63)  here to avoid rendering at the
	    // same color as "missing" data, which is 0.
	    unsigned char val = 63 + (unsigned char)
	      (192.0 * ((float) (*cp - mins) /
			(float) (maxs - mins)));
	    subpic.FillBox (ds_disp * rscale, ds_col * cscale,
			    rscale, cscale, val);
	  }

	  cp++;
	}
      }

      // Color the boxes that correspond to the chosen disparities

      subpic.LoadColorMapEntry (0, chosen, sizeof(chosen));
      
      unsigned short *this_line = disp_vals + startCol; // sd - (dispCols)
      
      for (ds_col = 0; ds_col < dispCols; ds_col++) {
	if (this_line[ds_col] != NO_S2_DISP) {
	  long bestr = ((this_line[ds_col] >> DISP_SCALE_SHIFT) - minDisp)
	    * rscale;
	  long bestc = ds_col * cscale;
	  unsigned char *optr = subpic.GetPixelAddress (bestr, bestc);
	  
	  // Color the best value green, but keep its intensity
	  chosen[2] = optr ? (255 + *optr) / 2 : 255;
	  subpic.LoadColorMapEntry (0, chosen, sizeof(chosen));
	  subpic.FillBox (bestr, bestc,
			  rscale, cscale, 0);
	  
#ifdef PRINTING_DISPARITY_SPACE_DETAILS
	  printf ("Row %ld Col %ld: bestdisp = %g\n", row, ds_col+startCol,
		  (this_line[ds_col] >> DISP_SCALE_SHIFT) +
		  (double) (this_line[ds_col] & ((1 << DISP_SCALE_SHIFT) - 1))
		  / (1 << DISP_SCALE_SHIFT));
#endif
	  // Indicate subpixel precision only if there's room
	  if (rscale > 2) {
	    long subpix_row = bestr +
	      (this_line[ds_col] & ((1 << DISP_SCALE_SHIFT) - 1)) *
	      rscale / (1 << DISP_SCALE_SHIFT);
	    subpic.Line (subpix_row, bestc,
			 subpix_row, bestc + cscale - 1, 255, 0);
	  }
	}
      }
    }
    
    // Color those values that were omitted based on the reason for
    // their omission.
    
    RejFlagT *rp = rejected_flag;
    
    for (ds_col = 0; ds_col < dispCols; ds_col++) {
      unsigned char *ptr;
      short best_disp = leftRecord[ds_col].d;

      if (best_disp >= minDisp && best_disp <= maxDisp) {
	if (disp_vals[startCol + ds_col] == NO_S2_DISP) {
	  // GetFilterColor is Guaranteed not to be NULL
	  ptr = GetFilterColor (*rp);
	  
#ifdef PRINTING_DISPARITY_SPACE_DETAILS
	  printf ("Row %ld Col %ld bestdisp %d REJ=%s\n", row,
		  ds_col+startCol, best_disp,
		  RejFlag2s (*rp));
#endif
	  subpic.LoadColorMapEntry (0, ptr, 4);
	  subpic.FillBox ((best_disp-minDisp) * rscale, ds_col * cscale,
			  rscale, cscale, 0);
	  subpic.LoadColorMapEntry (0, save, sizeof(save));
	}
      } else if (0) {
	fprintf (stderr, 
		 "BOGUS best_disp %d !in %d:%d\n",
		 best_disp, minDisp, maxDisp);
      }
      rp++;
    }
    

    dispspace.FillBox (bottom_table_row + 2 - (dsimage_text_rows / 4),
		       0,
		       dsimage_text_rows,
		       dispspace.cols, 255);


    WriteFilterLabels (&dispspace,
		       bottom_table_row + 2 - (dsimage_text_rows / 4),
		       0,
		       dsimage_text_rows,
		       dispspace.cols, &stats);
    
    // add copies of the scanlines to the bottom of the image
    
    unsigned char text_color[4] = {0, 0, 0, 0};

    dispspace.LoadColorMapEntry (0, text_color, sizeof(text_color));
    int iscale = MAX(dsimage_window_rows / (matchWinSizeY * 5), 1);
    int tcol = dsimage_text_left_cols;
    JPLPic s(mm), o(mm), *src = use_laplacian ? leftRectPic : leftRawRectPic;
    
    if (src->SubImage (&s, row - (matchWinSizeY+bound), 0,
		       2*matchWinSizeY + 1, leftRectPic->cols)
	== NO_ERR &&
	(s.Resample (&o, iscale * s.rows, cscale * s.cols) == NO_ERR)) {
      dispspace.Overlay (&o, dispspace.rows - dsimage_window_rows,
			 tcol - cscale * bound, 0);
      strcpy (buf, "Left");
      dispspace.TextBoundingBox (buf, 1, &tr, &tc);
      dispspace.Text (dispspace.rows - 3 * dsimage_window_rows / 4 - tr/2,
		      dispspace.cols + 4 - dsimage_text_right_cols + bound,
		      buf, 0, 0, 1);
    }
    src = use_laplacian ? rightRectPic : rightRawRectPic;
    if (src->SubImage (&s, row - (matchWinSizeY+bound), 0,
		       2*matchWinSizeY + 1, rightRectPic->cols)
	== NO_ERR &&
	(s.Resample (&o, iscale * s.rows, cscale * s.cols) == NO_ERR)) {
      dispspace.Overlay (&o, dispspace.rows - dsimage_window_rows/2,
			 tcol - cscale * bound, 0);
      strcpy (buf, "Right");
      dispspace.TextBoundingBox (buf, 1, &tr, &tc);
      dispspace.Text (dispspace.rows - 1 * dsimage_window_rows / 4 - tr/2,
		      dispspace.cols + 4 - dsimage_text_right_cols + bound,
		      buf, 0, 0, 1);
    }
  }
  snprintf (buf, bufLen, "row%03ld.png", row);
  dispspace.WriteGenericImage (buf);
}



JPLPic *JPLStereo::AnnotateMask (JPLPic *result, JPLPic *mask,
				 StereoFilterCountT *counts)
{
  long r, c;
  int text_rows = 20;
  int text_offset = 2;
  unsigned char zero[4] = {0, 0, 0, 0};

  if (mask == NULL)
    return NULL;

  if (result == NULL)
    result = NEW(mm, "Colorized Mask Image") JPLPic(mm);

  result->Init (mask->rows + text_rows + text_offset,
		mask->cols, ARGB32_PIXEL);

  for (r = 0; r < REJ_FLAGS; r++) {
    result->LoadColorMapEntry ((unsigned char) r,
			       GetFilterColor ((RejFlagT) r), 4);
  }
  for (; r < 256; r++) {
    result->LoadColorMapEntry ((unsigned char) r, zero, 4);
  }

  for (r = 0; r < mask->rows; r++) {
    unsigned char *src = mask->GetPixelAddress (r, 0);

    if (src) {
      for (c = 0; c < mask->cols; c++) {
	result->SetPixel (r, c, *src, 0);
	src++;
      }
    }
  }

  result->LoadGreyColorMap();
  result->FillBox (mask->rows, 0, text_offset + text_rows, result->cols, 255);
  WriteFilterLabels (result, mask->rows + text_offset, 0,
		     text_rows, result->cols, counts);
  return result;
} // AnnotateMask
