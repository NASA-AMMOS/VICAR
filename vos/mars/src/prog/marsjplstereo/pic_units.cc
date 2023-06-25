#include <math.h>
#include "nav_memory.h"
#include "JPLPic.h"
#include <string.h>



/*!
  Initialize the floating point Units associated with this image object
  by copying the appropriate values from the input object.

  If a unit description string exists in src and must be freed, then
  the string will be copied into new memory (which will also be tagged
  as needed to be freed).  Otherwise just the pointer to the string
  will be used, no additional memory will be allocated.

  \param src Image from which the floating point Units will be extracted

  \return NO_ERR if successful, PARAM_ERR on NULL input or if the row
  or column increment is zero.
*/

long	JPLPic::SetUnits (JPLPic *src)
{
  char *ru, *cu;
  int fr, fc;

  if (src == NULL)
    return PARAM_ERR;

  ru = src->row_units;
  cu = src->col_units;
  fr = src->free_row_units;
  fc = src->free_col_units;

  if (fr && ru) {
    ru = NEW(mm, "Duplicate Row Units String") char[strlen(ru)+1];
    strcpy (ru, src->row_units);
  }
  if (fc && cu) {
    cu = NEW(mm, "Duplicate Col Units String") char[strlen(cu)+1];
    strcpy (cu, src->col_units);
  }

  return SetUnits (ru, fr, src->row_offset, src->row_inc,
		   cu, fc, src->col_offset, src->col_inc);
}



/*!
  Initialize the floating point Units associated with this image object
  

  If a unit description string exists in src and must be freed, then
  the string will be copied into new memory (which will also be tagged
  as needed to be freed).  Otherwise just the pointer to the string
  will be used, no additional memory will be allocated.

  \param new_row_units String describing the ROW Units
  (e.g. "meters") or NULL.
  \param new_free_row If nonzero, the contents of new_row_units should
  be deleted by this image destructor.  If zero, then new_row_units
  points to memory that need not be free'd (e.g. a string constant).
  \param new_row_offset Set the floating point row value associated
  with the pixel at (0,0), i.e. the upper left corner of the upper
  left pixel in the image.
  \param new_row_inc Linear increment of the Units from row to row.
  \param new_col_units String describing the COL Units
  (e.g. "meters") or NULL.
  \param new_free_col If nonzero, the contents of new_col_units should
  be deleted by this image destructor.  If zero, then new_col_units
  points to memory that need not be free'd (e.g. a string constant).
  \param new_col_offset Set the floating point col value associated
  with the pixel at (0,0), i.e. the upper left corner of the upper
  left pixel in the image.
  \param new_col_inc Linear increment of the Units from col to col.


  \return NO_ERR if successful, PARAM_ERR if the row
  or column increment is zero.
*/

long	JPLPic::SetUnits (char *new_row_units, int new_free_row,
		  float new_row_offset, float new_row_inc, 
		  char *new_col_units, int new_free_col,
		  float new_col_offset, float new_col_inc)
{
  if (EQ (new_row_inc, 0.0) ||
      EQ (new_col_inc, 0.0))
    return PARAM_ERR;

  row_units = new_row_units;
  free_row_units = new_free_row;
  row_offset = new_row_offset;
  row_inc = new_row_inc;

  col_units = new_col_units;
  free_col_units = new_free_col;
  col_offset = new_col_offset;
  col_inc = new_col_inc;

  return NO_ERR;
} // JPLPic::SetUnits


/*!
  Scale the floating point Units associated with this image

  \param row_scale Linear scale factor for the row increment
  \param col_scale Linear scale factor for the column increment
*/

void JPLPic::ScaleUnits (float row_scale, float col_scale)
{
  row_inc *= row_scale;
  col_inc *= col_scale;
}


/*!
  Create a copy of this image that has been rescaled to have the Unit
  increments given as input.

  \param result Storage for the resulting rescaled image or NULL.
  \param new_row_inc Desired Linear increment of the Units from row to
  row.  The actual increment will not necessarily equal this amount,
  because the final value will be determined by the integer number of
  new rows that result from the rescaling.
  \param new_col_inc Linear increment of the Units from column to
  column.  The actual increment will not necessarily equal this amount,
  because the final value will be determined by the integer number of
  new columns that result from the rescaling.
  \param force_odd_dimensions If true, the size of the resulting
  rescaled image must be odd in each dimension.
  \param desc Description of the new image, used only for diagnostics
  if the resampling operation fails.

  \result Image whose row and column increments are close to the
  desired values, and whose pixel contents have been appropriately
  rescaled from the original object.  NULL if either input increment
  is zero or the resampling operation failed.
*/

JPLPic *JPLPic::ResampleUsingUnits (JPLPic *result,
				    float new_row_inc,
				    float new_col_inc,
				    int force_odd_dimensions,
				    char *desc)
{
  JPLPic *free_me = NULL;
  char buf[80];

  snprintf (buf, 80, "ResampleUsingUnits %.50s", desc ? desc : "");
  if (result == NULL)
    free_me = result = NEW(mm, buf) JPLPic(mm);

  if (EQ(new_row_inc, 0.0) ||
      EQ(new_col_inc, 0.0))
    return NULL;

  long new_rows = (long) floor (rows * row_inc / new_row_inc + 0.5);
  long new_cols = (long) floor (cols * col_inc / new_col_inc + 0.5);

  // Make sure the dimensions are odd
  if (force_odd_dimensions) {
    new_rows |= 1;
    new_cols |= 1;
  }

  if (new_rows == 0 || new_cols == 0 ||
      Resample (result, new_rows, new_cols) != NO_ERR) {
    DBG(("WARNING!! Failed to resample %s\n", desc));
    if (free_me) DELETE(mm, free_me);
    return NULL;
  }
  result->SetUnits (row_units, 0, row_offset, row_inc * rows / new_rows,
		    col_units, 0, col_offset, col_inc * cols / new_cols);
  return result;
}


/*!
  Return the address of the pixel value at the given floating point
  Units coordinates.  All float values within a given pixel map to the same
  single address.

  \param frow Floating point row Units coordinate
  \param fcol Floating point col Units coordinate
  \param wrap If nonzero, the input coordinates will wrap around in the
  image.  If zero, only those values within offset + length *
  increment (for both row and column values) will return a valid address.

  \return Address of the pixel at those coordinates, or NULL if the
  inputs are out of bounds or no Units mapping has been defined.
*/

unsigned char *JPLPic::GetPixelUnitsAddress (float frow, float fcol, int wrap)
{
  long row, col;

  if (GetPixelUnitsRowCol (frow, fcol, 1, wrap, &row, &col) != NO_ERR)
    return NULL;

  return GetPixelAddress (row, col, wrap);
} // JPLPic::GetPixelAddress


/*! Compute the pixel row and column using the floating point
  linearly-spaced Units ranges associated with this image.  Returns
  INIT_ERR if no such mapping has been defined, or PARAM_ERR if something
  went wrong with the computation.

  \param frow Value in the Pixel Units row range.
  \param fcol Value in the Pixel Units col range.
  \param scale Scale factor for pixel range; higher scales imply
  higher resolution pixels.
  \param wrap Should values outside the range be wrapped?
  \param rowp Row number return value
  \param colp Column number return value
  \return NO_ERR if successful, else INIT_ERR  
*/

long
JPLPic::GetPixelUnitsRowCol (float frow, float fcol, int scale, int wrap,
			     long *rowp, long *colp)
{
  long result = 1;

  if (row_units == NULL || col_units == NULL)
    return INIT_ERR;

  if (rowp)
    result = discretize_value (frow, row_offset, rows, row_inc, scale,
			       wrap, rowp);

  if (result == 0)
    return INIT_ERR;

  if (colp)
    result = discretize_value (fcol, col_offset, cols, col_inc, scale,
			       wrap, colp);

  return result ? NO_ERR : INIT_ERR;
}


/*!
  Returns the Units associated with the given pixel.  The Units
  returned are those from the upper left corner of the pixel.  In
  images that wrap around, the given anchor point is assumed to be
  in the range of values desired, and determines what the final value
  will be.  Specifically, the minimum wrapped distance from the anchor
  point to the given cell will be used to determine the offsets.

  \param row Pixel row
  \param col Pixel column
  \param wrap If nonzero, the input coordinates will wrap around in the
  image.  If zero, only those values within offset + length *
  increment (for both row and column values) will return a valid address.
  \param anchor_units_row Row Units value from somewhere in the
  image.  This is used to disambiguate the desired value when wrap is true.
  \param anchor_units_col Column Units value from somewhere in the
  image.  This is used to disambiguate the desired value when wrap is
  true.
  \param result_cell_units_row Computed Row Units coordinate
  \param result_cell_units_col Computed Column Units coordinate

  \return NO_ERR on success, INIT_ERR if no units were defined,
  PARAM_ERR if the inputs do not describe a legal pixel value
*/

long JPLPic::GetUpperLeftPixelUnitsFromRowCol (long row, long col, int wrap,
					       float anchor_units_row,
					       float anchor_units_col,
					       float *result_cell_units_row,
					       float *result_cell_units_col)
{
  // This only works if the units have been initialized

  if (row_units == NULL || col_units == NULL)
    return INIT_ERR;

  // Find the anchor point's row and column

  long arow, acol;

  if (GetPixelUnitsRowCol (anchor_units_row, anchor_units_col, 1, wrap,
			   &arow, &acol) != NO_ERR)
    return PARAM_ERR;

  // Compute the absolute delta needed from the anchor point, taking
  // any wraparound into account.
  long delta_row, delta_col;

  delta_row = wraparound_min_delta (arow, row, rows);
  delta_col = wraparound_min_delta (acol, col, cols);

  // Scale the absolute delta and apply it to the input.  As written
  // this will compute an integer number of cells offset from the
  // continuous-valued anchor point.  It might make more sense to map
  // the anchor point to its upper left value first instead.

  if (result_cell_units_row)
    *result_cell_units_row = anchor_units_row + delta_row * row_inc;
  if (result_cell_units_col)
    *result_cell_units_col = anchor_units_col + delta_col * col_inc;

  return NO_ERR;
} // JPLPic::GetUpperLeftPixelUnitsFromRowCol




/*!
  Returns a short string representing the floating point value.  The
  string is stored in static storage, so this function's return value
  will only be valid for a single invocation; don't call this twice in
  an expression and expect it to work.

  This function displays at most 3 significant digits after the
  decimal, but eliminates the decimal part of the display if it can be
  represeneted as an integer.

  \param val Floating point value to be rendered in ascii.

  \return Pointer to static stoage holding the float value.
*/

static char *MakeThermometerLabel (float val)
{
  static char buf[100];
  if (EQ(val, floor(val)))
    snprintf (buf, 100, "%d", (int) val);
  else
    snprintf (buf, 100, "%.3g", val);

  return buf;
}




/*! Create an annotated color image illustrating the physical units
  associated with this image.  The right and bottom of the image will
  have the units labelled, a repeating set of grid lines with the
  given period will be overlaid, and the image itself will be scaled
  in size by some integer offset.  If the scaling is sufficiently
  large, a border will be added around each pixel value.

  \param result Final color image; must have been previously
  allocated, but need not have been Init-ialized.
  \param scale Integer zoom factor, must be >= 1.  If it is more than
  4, a small border will be added to each scaled pixel by XORing.
  \param roff Original image row number of the cell you want in
  the upper left of the generated image.  
  \param coff Original image column number of the cell you want in
  the upper left of the generated image.  
  \param rperiod Row period, sampling rate to use for overlaid grid
  lines along the rows.
  \param rfixed Row fixed value, reference value that anchors the
  overlaid grid along the rows.  If (rfixed,cfixed) appears in the
  image it will be highlighted.
  \param cperiod Column period, sampling rate to use for overlaid grid
  lines along the rows.
  \param rfixed Column fixed value, reference value that anchors the
  overlaid grid along the rows.  If (rfixed,cfixed) appears in the
  image it will be highlighted.
  \param gamma Gamma value used to scale original pixel intensities,
  see JPLPic::LoadGreyColorMap for details.
  \param special 8-bit colormap index value that has a special color
  associated with it (if special_color is not NULL).
  \param special_color Vector used to initialize an ARGB 32-bit
  colormap with a highlight color for colormap index value special.
  \param special_color_size Size of special_color in bytes
  \param text_color Vector used to initialize an ARGB 32-bit
  colormap for text operations.
  \param text_color_size Size of text_color in bytes
  \param bg_color Vector used to initialize an ARGB 32-bit
  colormap for drawing the background of the annotated image.
  \param bg_color_size Size of bg_color in bytes
  \param period_color Vector used to initialize an ARGB 32-bit
  colormap for the overlaid grid lines.
  \param period_color_size Size of period_color in bytes
  \param origin_color Vector used to initialize an ARGB 32-bit
  colormap for highlighting the (rfixed, cfixed) origin
  \param origin_color_size Size of origin_color in bytes
  \param wrap_origin Should the origin be displayed even if it has to
  wrap around?
  \param band Which band of the image should be rendered (default 0)
  \param force_8bit_scaling All images are auto-scaled to 8 bits, but images
  that start with 8bit pixels are <em>not</em> scaled unless this is true.
  \param low_8bit_value Continuous value that corresponds to pixel value of 0
  \param mid_8bit_value Continuous value that corresponds to pixel value of 127
  \param high_8bit_value Continous value that corresponds to pixel value of 255
  \param pixel_type String identifying pixel intensity units
  \param therm_start_row Top row for drawing a pixel-intensity thermometer
  scale, or -1 if none desired
  \param therm_end_row Bottom row for drawing a pixel-intensity thermometer,
  or -1 to auto-scale it.
  scale
  \return NO_ERR if successful, else PARAM_ERR
*/

long
JPLPic::AnnotateUnits (JPLPic *result, char *title, long roff, long coff,
		       int scale, float rperiod,
		       float rfixed, float cperiod, float cfixed,
		       float new_gamma, unsigned char special,
		       unsigned char *special_color, int special_color_size,
		       unsigned char *text_color, int text_color_size,
		       unsigned char *bg_color, int bg_color_size,
		       unsigned char *period_color, int period_color_size,
		       unsigned char *origin_color, int origin_color_size,
		       int wrap_origin, int band, int force_8bit_scaling,
		       char *pixel_type, long therm_start_row,
		       long therm_end_row)
{
  const char *ru = row_units ? row_units : "pixels";
  const char *cu = col_units ? col_units : "pixels";
  float ro = row_units ? row_offset : 0.0;
  float co = col_units ? col_offset : 0.0;
  float ri = row_units ? row_inc : 1.0;
  float ci = col_units ? col_inc : 1.0;
  int pixel_text_pad = 3;

  if (result == NULL || EQ(rperiod, 0.0) || EQ(cperiod, 0.0))
    return PARAM_ERR;

  float a_min = 0.0, a_mid = 0.0, a_max = 0.0;

  // Poorly named,   "my8bit"   is either an 8bit-reduced version of
  // the input image, or the original image itself if it was a color image.

  JPLPic *my8bit = force_8bit_scaling ?
    MakeScaled8BitImage(band, &a_min, &a_mid, &a_max) :
    (pixelType != ARGB32_PIXEL ? 
     Make8BitImage(band, &a_min, &a_mid, &a_max) :
     this);

  scale = MAX (scale, 1);

  // Compute how many extra columns to add
  const size_t bufLen = 256;
  char buf[bufLen];
  int text_rows, text_cols, c2;

  // Leave space for the labels.  Assume that all labels fit into the
  // space required for just the first one and the pixel label

  snprintf (buf, bufLen, "%g %s", ro, ru);
  TextBoundingBox (buf, 1, &text_rows, NULL);
  snprintf (buf, bufLen, "%g\n%s", ro, ru);
  TextBoundingBox (buf, 1, &c2, &text_cols);
  if (therm_end_row < 0)
    therm_end_row = rows * scale - c2 - pixel_text_pad;
  if (pixel_type) {
    TextBoundingBox (pixel_type, 1, NULL, &c2);
    if (c2 > text_cols)
      text_cols = c2;
  }


  // Also check the spacing needed for thermometer labels 

  int show_therm = (therm_start_row >= 0 &&
		    therm_end_row - (therm_start_row + pixel_text_pad) > 30);

  if (show_therm) {
    TextBoundingBox (MakeThermometerLabel (a_min), 1, NULL, &c2);
    text_cols = MAX (text_cols, c2+15);
    TextBoundingBox (MakeThermometerLabel (a_mid), 1, NULL, &c2);
    text_cols = MAX (text_cols, c2+15);
    TextBoundingBox (MakeThermometerLabel (a_max), 1, NULL, &c2);
    text_cols = MAX (text_cols, c2+15);
  }

  result->Init (rows * scale + text_rows + pixel_text_pad,
		cols * scale + text_cols + pixel_text_pad,
		ARGB32_PIXEL);

  // Give it a background color; white if none given in the argument list

  unsigned char white[3] = { 255, 255, 255 };
  if (bg_color == NULL) {
    bg_color = white;
    bg_color_size = sizeof(white);
  }

  if (bg_color)
    result->LoadColorMapEntry (255, bg_color, bg_color_size);

  result->FillBox (0, 0, result->rows, result->cols, 255);

  // Copy the raw image at the given scale, using new_gamma to resample the
  // intensities

  result->LoadGreyColorMap (new_gamma, 0);

  // Allow one greyscale value to be colored specially.  Nominally an
  // "unknown" or "don't care" value, but in practise anything.

  if (special_color)
    result->LoadColorMapEntry (special, special_color, special_color_size);

  // Extract just the grid area, so we don't draw into the surrounding
  // text.  Note that SubImage copies the colormap for us, so we don't
  // have to copy it explicitly.

  JPLPic *grid_only = result->SubImage (0, 0, rows * scale, cols * scale);

  long r, c;

  if (my8bit) {
    int bytespp = my8bit->BytesPerPixel();

    for (r = 0; r < rows; r++) {
      unsigned char *ptr = my8bit->GetPixelAddress (r + roff, coff, 1);
      unsigned char *row_start = my8bit->GetPixelAddress (r + roff, 0, 1);
      unsigned char *row_end = my8bit->GetPixelAddress (r + roff,
							my8bit->cols-1, 1);

      for (c = 0; c < cols; c++) {
	if (bytespp == 1)
	  result->FillBox (r * scale, c * scale, scale, scale, *ptr);
	else {
	  result->LoadColorMapEntry (0, ptr, bytespp);
	  result->FillBox (r * scale, c * scale, scale, scale, 0);
	}
	if (scale > 4) {
	  
	  // Highlight each cell - just do the north and east borders, the
	  // effect will be that all sides have been filled in.
	  
	  grid_only->Line (r * scale, c * scale,
			   r * scale + scale-1, c * scale);
	  grid_only->Line (r * scale + scale-1, c * scale,
			   r * scale + scale-1, c * scale + scale-1);
	}
	ptr += bytespp;
	if (ptr > row_end)
	  ptr = row_start;
      }
    }
    if (my8bit != this)
      DELETE (mm, my8bit);
  }

  // Remove the special highlight color, we're done with cell values
  result->LoadGreyColorMap (new_gamma, 0);
  grid_only->LoadGreyColorMap (new_gamma, 0);

  // Add row highlights at the given period
  // rfixed is a row (in world units) that is part of the grid.
  // ro is the row (in world units) that is the upper left corner.
  
  rperiod = ABS (rperiod);
  if (period_color)
    grid_only->LoadColorMapEntry (0, period_color, period_color_size);

  long multiple = (long) ((rfixed - ro) / rperiod);
  float val = rfixed - SGN(ri) * (multiple + SGN(ri)) * rperiod;
  float end = val + (rows * 2) * ri;
  while (LT(SGN(ri) * val, SGN(ri) * end)) {
    long scaled_r;

    if (GetPixelUnitsRowCol (val, co + ci * (cols / 2), scale, 0,
			     &scaled_r, NULL) == NO_ERR) {
      grid_only->Line (scaled_r, 0, scaled_r, cols * scale - 1, 0, 0);
    }
    val += SGN(ri) * rperiod;
  }

  // Add col highlights at the given period
  cperiod = ABS (cperiod);
  multiple = (long) ((cfixed - co) / cperiod);
  val =  cfixed - SGN(ci) * (multiple + 1) * cperiod;
  end = val + (cols * 2) * ci;
  while (LT(SGN(ci) * val, SGN(ci) * end)) {
    long scaled_c;

    if (GetPixelUnitsRowCol (ro + ri * (rows/2), val, scale, 0, NULL,
			     &scaled_c)	== NO_ERR) {
      grid_only->Line (0, scaled_c, rows * scale - 1, scaled_c, 0, 0);
    }
    val += SGN(ci) * cperiod;
  }

  // Add text labels
  val = ro + rows * ri;
  if (EQ (val, 0.0))
    val = 0.0;

  if (text_color)
    result->LoadColorMapEntry (0, text_color, text_color_size);
  result->Text (pixel_text_pad, cols * scale + pixel_text_pad, buf, 0, 0, 1, 1);
  snprintf (buf, bufLen, "%g\n%s", val, ru);
  TextBoundingBox (buf, 1, &text_rows, &text_cols);
  result->Text (rows * scale - text_rows, cols * scale + pixel_text_pad, buf, 0,
		0, 1, 1);
  snprintf (buf, bufLen, "%g %s", co, cu);
  result->Text (rows * scale + pixel_text_pad, pixel_text_pad, buf, 0, 0, 1, 1);
  val = co + cols * ci;
  if (EQ (val, 0.0))
    val = 0.0;
  snprintf (buf, bufLen, "%g %s", val, cu);
  TextBoundingBox (buf, 1, &text_rows, &text_cols);
  result->Text (rows * scale + pixel_text_pad,
		cols * scale - (pixel_text_pad + text_cols),
		buf, 0, 0, 1, 1);
  if (title) {
    TextBoundingBox (title, 1, &text_rows, &text_cols);
    result->Text (rows * scale + pixel_text_pad,
		  (cols * scale - text_cols) / 2,
		  title, 0, 0, 1, 1);
  }

  // Add ticks next to the text labels
  result->Line (0, cols * scale, 0, cols * scale + pixel_text_pad, 0, 0);
  result->Line (rows * scale - 1, cols * scale, rows * scale - 1,
		cols * scale + pixel_text_pad, 0, 0);
  result->Line (rows * scale, 0, rows * scale + pixel_text_pad - 1, 0, 0, 0);
  result->Line (rows * scale, cols * scale - 1,
		rows * scale + pixel_text_pad - 1, cols * scale - 1, 0, 0);
  
  // Highlight the origin, if present and if a color was given
  if (origin_color) {
    result->LoadColorMapEntry (0, origin_color, origin_color_size);
    if (GetPixelUnitsRowCol (0.0, 0.0, scale, wrap_origin, &r, &c)
	== NO_ERR) {

      // Draw cross-hairs
      result->Line (r, 0, r, cols * scale - 1, 0, 0);
      result->Line (0, c, rows * scale - 1, c, 0, 0);

      // Draw a circle the same size as a cell
      result->Circle (r, c, MAX(1, scale/2), 1, 0, 0, 0);
    }
  }

  // Restore default greyscale colormap
  result->LoadGreyColorMap (new_gamma, 0);

  // Draw a thermometer for pixel values

  if (pixelType != ARGB32_PIXEL) {
    int therm_text_col = cols * scale + pixel_text_pad + 15;

    if (pixel_type) {
      result->Text (therm_start_row, therm_text_col - 15,
		    pixel_type, 1, 0, 1);
      TextBoundingBox (pixel_type, 1, &text_rows, &text_cols);
      therm_start_row += text_rows;
    }

    if (show_therm) {
      int start_row = therm_start_row + 10;
      int i;
      float pixel_inc = 255.0 / (therm_end_row - start_row);

      for (i = 0; i < therm_end_row - start_row; i++)
	result->Line (therm_end_row - i, therm_text_col - 15,
		      therm_end_row - i, therm_text_col - 5,
		      (unsigned char) (i * pixel_inc), 0);

      if (special_color) {
	i = therm_end_row -
	  (int) (special * (therm_end_row - start_row) / 255.0);
	result->LoadColorMapEntry (special, special_color, special_color_size);
	result->Line (i, therm_text_col - 15,
		      i, therm_text_col - 5,
		      special, 0);
      }

      result->Text (therm_end_row - 8, therm_text_col,
		    MakeThermometerLabel (a_min), 1, 0, 1);
      result->Text ((start_row + therm_end_row) / 2 - 4, therm_text_col,
		    MakeThermometerLabel (a_mid), 1, 0, 1);
      result->Text (start_row, therm_text_col,
		    MakeThermometerLabel (a_max), 1, 0, 1);
    }
  }

  if (grid_only)
    DELETE (mm, grid_only);

  return NO_ERR;
} // JPLPic::AnnotateUnits

