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


#include <stdio.h>
#include <math.h>
#include <string.h>	// For memcpy
#include "JPLPic.h"
#include "mwm.h"	// cistrcmp()
#include "nav_memory.h"
#include "MDTypes.h" // machine depedent type def
#include "ErrHandle.h"
#include "real_helpers.h"
#include "good_fopen.h"
#include <netinet/in.h>	// htonl()

#ifdef __unix__

#include <unistd.h>
#include <fcntl.h>    // For open()
#endif


#ifdef VXWORKS

#include <memLib.h>	// Check heap memory before allocating
// #include "Timing.h"
#endif


#ifdef MSP
#define ERR(x) (0x6050+(x))
#else
#ifndef DBG
#define DBG(x) printf x
#endif
#endif


//#define INTERFACE_DEBUG

#ifndef MAX
#define MAX(a,b)  (((a) > (b)) ? (a) : (b))
#endif
#ifndef MIN
#define MIN(a,b)  (((a) < (b)) ? (a) : (b))
#endif



/*!
  Initialize the class variables associated with this JPLPic object.

  Set the pixel type to UC8_PIXEL.

  This also loads the colormap with default greyscale values.

  \param mgr Memory pool manager
  \param new_flags Flags associated with this image
*/

void JPLPic::InitVars (JMemoryManager *mgr, unsigned long new_flags)
{
	rows = cols = 0;
	field = false;
	flags = new_flags;

	mm = mgr;

	cmapbuf = cmap_dflt;

	row_offset = col_offset = 0.0;
	row_inc = col_inc = 1.0;
	row_units = col_units = NULL;
	free_row_units = free_col_units = 0;

	pixelType = UC8_PIXEL;
	actualBytes = 0L;
	pixels = NULL;
	rowBytes = 0;
	releaseMem = false;
	clut = NULL;

	LoadGreyColorMap (0.0, 0);

}

/*!
  Default Constructor for a JPLPic object

  Set the pixel type to UC8_PIXEL.

  This also loads the colormap with default greyscale values.
*/

JPLPic::JPLPic ()
{
  InitVars (NULL, 0);
}


/*!
  Constructor for a JPLPic object

  Set the pixel type to UC8_PIXEL.

  This also loads the colormap with default greyscale values.

  \param mgr Memory pool manager

*/


JPLPic::JPLPic (JMemoryManager *mgr)
{
  InitVars (mgr, 0);
}


/*!
  Constructor for a JPLPic object

  Set the pixel type to UC8_PIXEL.

  This also loads the colormap with default greyscale values.

  \param new_flags Flags associated with this image
  \param mgr Memory pool manager

*/

JPLPic::JPLPic (unsigned long new_flags, JMemoryManager *mgr)
{
  InitVars (mgr, new_flags);
}

/*!
  Destructor for JPLPic object.

  Frees up any additional memory associated with this image, including:

  clut - mac-based code colormap lookup table
  row_units - string describing the Units associated with image rows
  col_units - string describing the Units associated with image columns
  pixels - raw image memory
  cmapbuf - color map
*/

JPLPic::~JPLPic ()
{
  if (clut) DELETEV (mm, clut);
  if (free_row_units && row_units) DELETEV (mm, row_units);
  if (free_col_units && col_units) DELETEV (mm, col_units);
  if (releaseMem && pixels) DELETEV (mm, pixels);
  if (cmapbuf && cmapbuf != cmap_dflt) DELETEV (mm, cmapbuf);
}


//
// Table of Pixel Type strings and tags.  The FIRST string for a given tag
// must be the canonical representation.
//

typedef struct {
  char *str;
  enum PixelTag tag;
} S2PixType;

static S2PixType s2pixel_table[] =
{
  { "UC8", UC8_PIXEL },
  { "char", UC8_PIXEL },
  { "c", UC8_PIXEL },

  { "INT16", INT16_PIXEL },
  { "short", INT16_PIXEL },
  { "h", INT16_PIXEL },

  { "INT32", INT32_PIXEL },
  { "int", INT32_PIXEL },
  { "long", INT32_PIXEL },
  { "i", INT32_PIXEL },
  { "l", INT32_PIXEL },

  { "FLOAT", FLOAT_PIXEL },
  { "f", FLOAT_PIXEL },

  { "ARGB32", ARGB32_PIXEL },
  { "color", ARGB32_PIXEL },
  { "rgb", ARGB32_PIXEL },
  { "a", ARGB32_PIXEL },
  { "r", ARGB32_PIXEL },

  { "DOUBLE", DOUBLE_PIXEL },
  { "d", DOUBLE_PIXEL },

  { "XYZ_FLOAT", XYZ_FLOAT_PIXEL },
  { "XYZ_DOUBLE", XYZ_DOUBLE_PIXEL }
};

#define S2PIXEL_ENTRIES (sizeof(s2pixel_table) / sizeof(S2PixType))


/*!
  Print a list of available pixel type tags to the output stream.

  \param fp Output Stream.

  \sa S2PixelTag
*/

void PrintPixelTags (FILE *fp)
{
  int i;
  PixelTag last = UNKNOWN_PIXEL;

  if (fp == NULL) return;

  fprintf (fp, "Available Pixel Tags:");
  for (i = 0; i < (int) S2PIXEL_ENTRIES; i++) {
    if (s2pixel_table[i].tag != last)
      fprintf (fp, "\n");
    if (s2pixel_table[i].str)
      fprintf (fp, " %s", s2pixel_table[i].str);
    last = s2pixel_table[i].tag;
  }
 
 fprintf (fp, "\n");

} /* PrintPixelTags */



/*!
  Parse the input string for a valid Pixel Tag specification.  The
  first valid specification that completely matches the input string
  will be returned.  So you can use prefix matching, but nothing is
  done to ensure that the prefix is unique.

  \param str String containing the pixel tag spec at its beginning, or
  NULL.

  \return The Pixel Tag corresponding to the input string, or
  UNKNOWN_PIXEL.
*/

enum PixelTag JPLPic::S2PixelTag (char *str)
{
  enum PixelTag result = UNKNOWN_PIXEL;
  int i;

  if (str == NULL)
    return result;

  for (i = 0; i < (int) S2PIXEL_ENTRIES; i++)
    if (cistrcmp (s2pixel_table[i].str, str) == 0) {
      result = s2pixel_table[i].tag;
      break;
    }
  
  return result;
} // JPLPic::S2PixelTag



/*!
  Generate a human-readable string describing the input pixel tag.

  \param pt Pixel Tag to be described
  \param buf Optional storage for the pixel tag description, or NULL.

  \return description of the input pixel tag.  If an invalid pixel tag
  is somehow input, a string will still be returned.  The returned
  pointer value is <em>not</em> the same as the input buf.
*/

char *JPLPic::PixelTag2s (enum PixelTag pt, char *buf)
{
  char *res = "Invalid pixel tag value!";
  int i;

  for (i = 0; i < (int) S2PIXEL_ENTRIES; i++)
    if (s2pixel_table[i].tag == pt) {
      res = s2pixel_table[i].str;
      break;
    }

  if (buf && res) {
    strcpy(buf, res);
    res = buf;
  }
  return res;
} // JPLPic::PixelTag2s

/*!
  Initialize a JPLPic object

  This will automatically reallocate image memory if the requested
  size is larger than the current size.  It will <em>not</em> free up
  existing, larger memory.

  A default greyscale colormap will also be initialized.

  \param totalRows Number of rows requested for the image
  \param totalCols Number of columns requested for the image
  \param type Type of pixels in this image.  See enum PixelTag for a
  list of values.
  \param mgr Memory pool Manager

  \return NO_ERR on success.  Right now this always succeeds.
*/

long JPLPic::Init (long totalRows, long totalCols, long type,
			  JMemoryManager *mgr)
{
	int newbytes = BytesPerPixel(type) * totalRows * totalCols;

	rows = totalRows;
	cols = totalCols;
	pixelType = type;
	
	if (actualBytes < newbytes) {
	  if (releaseMem && pixels) DELETEV (mgr, pixels);
	  pixels = NEW(mgr, "pixel memory") unsigned char[newbytes];
	  actualBytes = newbytes;
	  releaseMem = true;

	  // Have to do this here because we want to be able to Init
	  // to a subimage region.  And if the rowBytes is always
	  // reset, we'll lose the ability to re-init subimages.
	  rowBytes = BytesPerPixel (pixelType) * cols;
	}
	
	if (pixels == NULL) return MEM_ERR;
	LoadGreyColorMap (0.0, 0);
	
	return NO_ERR;
}



/*!
  Place a copy of this image object into a pre-allocated destination
  image.  The destination must have been initialized with the proper
  pixel type and sufficient memory to hold the specified copy, or an
  error will result.

  This copies associated data structures too, e.g. the Pixel Units
  specifications, but <em>not</em> the colormap.

  \param dstPic Destination image.  This must have already been
  initialized with the problem Pixel Tag and enough space to hold the
  copy of the image.
  \param start_row Starting row from which to extract rows for the
  copy.  Must be a row within this image, it will not wrap around.
  \param start_col Starting column from which to extract columns for the
  copy.  Must be a column within this image, it will not wrap around.

  \bug Does not copy the colormap.

  \return NO_ERR if successful, PARAM_ERR if the destination pointer
  is NULL, TYPE_MATCH_ERR if the destination image is not large enough
  to hold the copy or has a different Pixel Tag.
*/
long
JPLPic::Copy (JPLPic *dstPic, long start_row, long start_col)
{
  if (dstPic == NULL)
    return PARAM_ERR;
  if (dstPic->rows < rows - start_row ||
      dstPic->cols < cols - start_col ||
      start_row < 0 || start_col < 0 ||
      dstPic->GetPixelType () != pixelType) {
    return (TYPE_MATCH_ERR);
  }
	
  unsigned char *src, *dst, *s, *d;
  long  row, col;
	
  src = pixels;
  dst = dstPic->GetPixelAddress (start_row, start_col);
  long bytesPerRow = BytesPerPixel (pixelType) * cols;
  long newRowBytes = dstPic->GetRowBytes ();
	
  // copy byte by byte, even if pixels are larger
  for (row = rows; row --; src+= rowBytes, dst += newRowBytes)
    for (col = bytesPerRow, s= src, d = dst; col --; s++, d++)
      *d = *s;
  
  // HACK -- should copy the colormap too

  // Copy unit strings, freeing up any that had been there already

  dstPic->row_offset = row_offset;
  dstPic->col_offset = col_offset;
  dstPic->row_inc = row_inc;
  dstPic->col_inc = col_inc;

  if (row_units) {
    if (dstPic->row_units && dstPic->free_row_units)
      DELETEV (mm, dstPic->row_units);
    if (free_row_units) {
      dstPic->row_units = NEW(mm, "row unit string") char[strlen(row_units)+1];
      strcpy (dstPic->row_units, row_units);
      dstPic->free_row_units = 1;
    } else {
      dstPic->row_units = row_units;
      dstPic->free_row_units = 0;
    }
  } else {
    if (dstPic->free_row_units)
      DELETEV (mm, dstPic->row_units);
    dstPic->row_units = NULL;
    dstPic->free_row_units = 0;
  }
  if (col_units) {
    if (dstPic->col_units && dstPic->free_col_units)
      DELETEV (mm, dstPic->col_units);
    if (free_col_units) {
      dstPic->col_units = NEW(mm, "col unit string") char[strlen(col_units)+1];
      strcpy (dstPic->col_units, col_units);
      dstPic->free_col_units = 1;
    } else {
      dstPic->col_units = col_units;
      dstPic->free_col_units = 0;
    }
  } else {
    if (dstPic->free_col_units)
      DELETEV (mm, dstPic->col_units);
    dstPic->col_units = NULL;
    dstPic->free_row_units = 0;
  }

  return NO_ERR;
}


/*!
  Returns the address within the pixels array that corresponds to the
  input pixel coordiantes.

  \param row Row number of the desired pixel
  \param col Column number of the desired pixel
  \param wrap If nonzero, wrap the input row and col numbers so that
  they lie within the image.  If zero, values outside the range
  [0:rows-1] or [0:cols-1] will cause NULL to be returned.

  \return Address of the specified pixel
*/

unsigned char *
JPLPic::GetPixelAddress (int row, int col, int wrap)
{
  unsigned char *result = GetPixelAddress();

  if (wrap && rows > 0 && cols > 0) {
    row = (row + rows) % rows;
    col = (col + cols) % cols;
    if (row < 0) row += rows;
    if (col < 0) col += cols;
  }
    
  if (0 <= row && row < rows && 0 <= col && col < cols)
    result += rowBytes * row + col * BytesPerPixel();
  else
    result = NULL;

  return result;
}



/*!
  Given an XYZ_FLOAT_PIXEL or XYZ_DOUBLE_PIXEL image, find the pixel
  coordinates of the point whose X and Y values are nearest to the input
  values, within some threshold.  

  Currently this returns just the integer pixel coordiantes neaest the
  point, in the future we might want to allow subpixel interpolation.

  \param x X coordinate to be compared against X component of the
  pixel values
  \param y Y coordinate to be compared against Y component of the
  pixel values
  \param threshold Threshold for finding pixels.  The Euclidean
  distance from the pixel XY to the input XY must be strictly less
  than this amount to be considered close enough.
  \param ignore_value Special pixel value that should be ignored.
  Useful for specifying invalid pixels, e.g. NO_RANGE.
  \param row Row coordinate of the pixel that lies closest to the
  input XY location.
  \param col Column coordinate of the pixel that lies closest to the
  input XY location.

  \return NO_ERR if successful, PARAM_ERR if there was a problem with
  the input, CONTENT_ERR if no such pixel exists.

*/

long JPLPic::GetRowColNearestXY (double x, double y, double threshold,
				 double ignore_value, double *row, double *col)
{
  long r, c, bestr=0, bestc=0;
  double best_sq_dist = threshold * threshold;

  if (!IS_VECTOR_PIXEL (pixelType) || pixelType == ARGB32_PIXEL)
    return PARAM_ERR;

  switch (pixelType) {
  case XYZ_FLOAT_PIXEL:
    for (r = 0; r < rows; r++) {
      float *f = GetFloatPixelAddress (r, 0);

      for (c = 0; c < cols; c++, f += 3) {
	if (!EQ(f[0], ignore_value) &&
	    !EQ(f[1], ignore_value) &&
	    !EQ(f[2], ignore_value)) {
	  double this_sq_dist = SQ(f[0] - x) + SQ(f[1] - y);

	  if (LT (this_sq_dist, best_sq_dist)) {
	    best_sq_dist = this_sq_dist;
	    bestr = r;
	    bestc = c;
	  }
	}
      }
    }
    break;
  case XYZ_DOUBLE_PIXEL:
    for (r = 0; r < rows; r++) {
      double *d = GetDoublePixelAddress (r, 0);

      for (c = 0; c < cols; c++, d += 3) {
	if (!EQ(d[0], ignore_value) &&
	    !EQ(d[1], ignore_value) &&
	    !EQ(d[2], ignore_value)) {
	  double this_sq_dist = SQ(d[0] - x) + SQ(d[1] - y);

	  if (LT (this_sq_dist, best_sq_dist)) {
	    best_sq_dist = this_sq_dist;
	    bestr = r;
	    bestc = c;
	  }
	}
      }
    }
    break;
  default:
    return PARAM_ERR;
  }

  // Currently we just return the best integer pixel, but in the future we
  // might interpolate to get the best subpixel result

  if (LT (best_sq_dist, SQ(threshold))) {
    if (row) *row = (double) bestr;
    if (col) *col = (double) bestc;
    return NO_ERR;
  }

  return CONTENT_ERR;
}


/*!
  Find the shortest row and column deltas that connect two pixels,
  allowing wraparound at the edges.

  \param from_row Row coordinate of the first pixel
  \param from_col Column coordinate of the first pixel
  \param to_row Row coordinate of the second pixel
  \param to_col Column coordinate of the second pixel
  \param delta_row Signed delta offset that relates the input row
  coordinates
  \param delta_col Signed delta offset that relates the input column
  coordinates
*/

void JPLPic::GetBestWraparoundDeltaBetween (long from_row, long from_col,
					    long to_row, long to_col,
					    long *delta_row, long *delta_col)
{
  if (delta_row)
    *delta_row = wraparound_min_delta (from_row, to_row, rows);
  if (delta_col)
    *delta_col = wraparound_min_delta (from_col, to_col, cols);
} // JPLPic::GetBestWraparoundDeltaBetween


/*!
  Allocate a new image and initialize its pixel memory so it is just
  large enough to hold the current image object.

  At present this does <em>not</em> copy the image pixels, just
  allocates a sufficiently large structure.

  \return Newly allocated image object, or NULL if no memory was
  available for either the image object or its component pixels.
*/

JPLPic *
JPLPic::Clone ()
{
	JPLPic *newPic = NEW(mm, "cloned image") JPLPic(mm);
	
	if (newPic == NULL || newPic->Init (rows, cols, pixelType)
	    != NO_ERR) {
		if (newPic != NULL) DELETE (mm, newPic);
		Warning ("Clone:  Cannot allocate new pic\n");
		return NULL;
	}
	
//	Copy (newPic);
	return newPic;
}



/*!
  Reset the parameters of an already-allocated image object so that it
  points to an alias of the current object's pixel memory.

  Creating an alias is error-prone, because now you need to worry
  about which object will be responsible for maintaining the memory
  associated with the pixels.  Set the swap_release_mem parameter if
  you want the aliased image to take over responsibility for freeing
  up the pixel memory when that image is deleted.

  \param newPic Already-allocated image object, whose parameters will
  be reset to become an alias of the current object's pixel memory.
  \param swap_release_mem If nonzero, force the new input image to
  inherit the releaseMem flag from this object.  If zero, this object
  maintains the releaseMem flag.

  \bug Doesn't modify the aliased image's colormap at all.

  \sa Copy, Clone
  \return NO_ERR if successful, PARAM_ERR if the input new image is NULL.
*/

long
JPLPic::Alias (JPLPic *newPic, int swap_release_mem)
{
  if (newPic == NULL)
    return PARAM_ERR;
  newPic->SetPixelAddress (pixels, actualBytes, releaseMem);
  newPic->rows = rows;
  newPic->cols = cols;
  newPic->SetPixelType (pixelType);
  newPic->SetRowBytes (rowBytes);
  newPic->field = field;
  newPic->clut = clut;
  newPic->releaseMem = false;
  if (swap_release_mem) {
    char tmp = newPic->releaseMem;
    newPic->releaseMem = releaseMem;
    releaseMem = tmp;
  }
	  
  // HACK deal with colormap

  newPic->SetUnits (row_units, free_row_units, row_offset, row_inc,
		    col_units, free_col_units, col_offset, col_inc);
  return NO_ERR;
}

/*!
  Allocate a new image object that
  points to an alias of the current object's pixel memory.

  Creating an alias is error-prone, because now you need to worry
  about which object will be responsible for maintaining the memory
  associated with the pixels.  This version of the function will leave
  the pixel memory destruction in place with the original object, it
  will not pass through to the newly-allocated alias.

  \bug Doesn't copy the original image's colormap into the alias

  \sa Copy, Clone

  \return Newly allocated image or NULL if it could not be allocated.
*/


JPLPic *
JPLPic::Alias ()
{
	JPLPic *newPic = NEW(mm, "aliased image") JPLPic(mm);

	if (newPic == NULL) {
		Warning ("Cannot allocate new pic in Alias\n");
		return NULL;
	}
	
	Alias (newPic);
	return newPic;
}


/*!
  Create an aliased view into a sub-rectangle of the current image's
  pixels.

  \param startRow Row coordinate of the upper left pixel in the
  subwindow
  \param startCol Column coordinate of the upper left pixel in the
  subwindow
  \param numRows Number of rows in the subwindow
  \param numCols Number of columns in the subwindow

  \return Newly allocated subimage or NULL if it could not be
  allocated or properly initialized.
*/

JPLPic *
JPLPic::SubImage (long startRow, long startCol, long numRows, long numCols)
{
  long result = NO_ERR;
  JPLPic *newPic = NEW(mm, "SubImage") JPLPic(mm);
	
  if (newPic == NULL) {
    Warning ("Cannot allocate new pic in Subing\n");
    return NULL;
  }
	
  result = SubImage (newPic, startRow, startCol, numRows, numCols);

  if (result != NO_ERR) {
    DELETE (mm, newPic);
    return NULL;
  }

  return newPic;
} // JPLPic::SubImage



/*!
  Establish an aliased view into a sub-rectangle of the current image's
  pixels in an already-allocated destination image object.

  \param dstPic Destination image, expected to have been allocated
  already.  Must not be NULL.
  \param startRow Row coordinate of the upper left pixel in the
  subwindow
  \param startCol Column coordinate of the upper left pixel in the
  subwindow
  \param numRows Number of rows in the subwindow
  \param numCols Number of columns in the subwindow

  \return NO_ERR if successful, PARAM_ERR if dstPic is NULL or the
  subwindow is ill-defined.
*/
  
long
JPLPic::SubImage (JPLPic *dstPic,
		   long startRow, long startCol, long numRows, long numCols)
{
	if (dstPic == NULL) {
		Warning ("Destination JPLPic is NULL\n");
		return PARAM_ERR;
	}
	
	if (startRow + numRows > rows) {
		Warning ("Rows exceeds limit\n");
		numRows = rows - startRow;
	}
	if (startCol + numCols > cols) {
		Warning("cols exceeds limit\n");
		numCols = cols - startCol;
	}
	
	if (numRows <= 0 || numCols <= 0) {
		FatalErr ("Ill-defined sub-rectangle\n");
		return PARAM_ERR;
	}
	
	dstPic->SetPixelAddress
	  (pixels + startRow * rowBytes + startCol * BytesPerPixel(),
	   actualBytes - (startRow * rowBytes + startCol * BytesPerPixel()),
	   false );
	dstPic->rows = numRows;
	dstPic->cols = numCols;
	dstPic->SetPixelType (pixelType);
	dstPic->SetRowBytes (rowBytes);
	dstPic->releaseMem = false;
	dstPic->DefineColorMapMem (256 * dstPic->BytesPerPixel());
	memcpy (dstPic->cmapbuf, cmapbuf, 256 * dstPic->BytesPerPixel());
	dstPic->cmap.uc = dstPic->cmapbuf;

	dstPic->SetUnits (row_units, free_row_units,
			  row_offset + row_inc * startRow, row_inc,
			  col_units, free_col_units,
			  col_offset + col_inc * startCol, col_inc);

	return NO_ERR;
}



/*!
  Construct a new ARBG32_PIXEL image given component Red, Green,
  Blue UC8_PIXEL images.  All bands must have matching numbers of rows
  and columns, and have pixel type UC8_PIXEL.

  The alpha channel will be initialized as the averge value of the 3
  color bands.

  \param red UC8_PIXEL image that will become the RED band.
  \param green UC8_PIXEL image that will become the GREEN band.
  \param blue UC8_PIXEL image that will become the BLUE band.

  \return Newly-allocated ARGB32_PIXEL image comprised of pixels from
  the three input bands, or NULL if any parameters have been
  misspecified.
*/

JPLPic *JPLPic::BuildARGB (JPLPic *red, JPLPic *green, JPLPic *blue)
{
  JPLPic *result = NULL;
  
  long r, c;

  if (red == NULL || green == NULL || blue == NULL)
    return NULL;

  if (red->rows != green->rows || red->rows != blue->rows ||
      red->cols != green->cols || red->cols != blue->cols)
    return NULL;

  if (red->GetPixelType() != UC8_PIXEL || green->GetPixelType() != UC8_PIXEL ||
      blue->GetPixelType() != UC8_PIXEL)
    return NULL;
  
  result = NEW(mm, "ARGB32 image") JPLPic(mm);
  result->Init (red->rows, red->cols, ARGB32_PIXEL);

  for (r = 0; r < red->rows; r++) {
    unsigned char *ptr = result->GetPixelAddress (r, 0);
    unsigned char *rptr = red->GetPixelAddress (r, 0);
    unsigned char *gptr = green->GetPixelAddress (r, 0);
    unsigned char *bptr = blue->GetPixelAddress (r, 0);
  
    if (ptr && rptr && gptr && bptr) {
      for (c = 0; c < red->cols; c++, ptr += 4, rptr++, gptr++, bptr++) {
	ptr[0] = (*rptr + *gptr + *bptr) / 3;
	ptr[1] = *rptr;
	ptr[2] = *gptr;
	ptr[3] = *bptr;
      } // for
    } // if
  } // for

  return result;

} // JPLPic::BuildARGB


/*!
  Construct a new ARBG32_PIXEL image given component Red, Green,
  Blue in-memory images.  All bands are assumed to have matching
  numbers of rows and columns, and have unsigned char pixels.

  The alpha channel will be initialized as the averge value of the 3
  color bands.

  \param alpha Start of memory for the ALPHA channel or NULL.  If
  NULL, the new image will assign the average of the three color bands
  as the alpha value.
  \param red Memory for the image that will become the RED band.  Must
  <em>not</em> be NULL.
  \param green Memory for the image that will become the GREEN band. Must
  <em>not</em> be NULL.
  \param blue Memory for the image that will become the BLUE band. Must
  <em>not</em> be NULL.

  \return Newly-allocated ARGB32_PIXEL image comprised of pixels from
  the three input bands, or NULL if any parameters have been
  misspecified.
*/

JPLPic *JPLPic::BuildARGB (long nrows, long ncols, unsigned char *alpha,
			   unsigned char *red,
			   unsigned char *green, unsigned char *blue)
{
  JPLPic *result;
  long r, c;

  if (red == NULL || green == NULL || blue == NULL)
    return NULL;

  result = NEW(mm, "ARGB32 from channels image") JPLPic(mm);
  result->Init (nrows, ncols, ARGB32_PIXEL);

  for (r = 0; r < nrows; r++) {
    unsigned char *ptr = result->GetPixelAddress (r, 0);
    if (ptr) {
      for (c = 0; c < ncols; c++, ptr += 4, red++, green++, blue++) {
	ptr[0] = alpha ? *alpha : (*red + *green + *blue) / 3;
	ptr[1] = *red;
	ptr[2] = *green;
	ptr[3] = *blue;
	if (alpha) alpha++;
      } // for
    } // if
  } // for

  return result;
} // JPLPic::BuildARGB



/*!
  Create aliases for any of the two fields in the current image object.
  Although this operation could in theory be applied to any image,
  here we insist that this operation only be allowed when the field
  attribute of the current image has been set previously.

  \param evenField Pointer to an already-allocated image that will
  become an alias to the even fields of the current image, or NULL.
  \param oddField Pointer to an already-allocated image that will
  become an alias to the odd fields of the current image, or NULL.

  \return NO_ERR if successful, INIT_ERR if this object does not have
  its field attribute set.
*/

long
JPLPic::SeparateFields (JPLPic *evenField, JPLPic *oddField)
{
  if (! field) {
    FatalErr ("Cannot seprate a non-interlaced frame\n");
    return INIT_ERR;
  }
	
  if (evenField) {
    Alias(evenField);
    // even field
    evenField->rows = (rows + 1) >> 1;
    evenField->field = false;
    evenField->SetRowBytes (rowBytes << 1);
  }
	
  if (oddField) {
    Alias(oddField);
    // odd field
    oddField->rows = ((rows & 0x01) ? ((rows - 1) >> 1) : (rows >> 1));
    oddField->field = false;
    oddField->SetPixelAddress (pixels + rowBytes, actualBytes-rowBytes, false); 
    oddField->SetRowBytes (rowBytes << 1);
    oddField->releaseMem = false;
  }
	
  return NO_ERR;
}



/*!
  Compute the subpixel intensity value by bilinear interpolation.  The
  current image must have UC8_PIXEL pixels.

  \param p 2-element array of Image coordinates in (x,y) (not row,col)
  order. 
  \param bv Memory to hold the interpolated value, or NULL.

  \return NO_ERR if successful, PARAM_ERR if no image coordinates are
  given or this image is not UC8_PIXEL, INIT_ERR if the requested
  image coordinates are outside the image bounds.
*/

int JPLPic::InterpolateBV(double p[2], double *bv)
{
  unsigned char *src;
  register long ix, iy;
  register double dx, dy, dx0, dy0;
  register long p00, p01, p11, p10;

  if (p == NULL || pixelType != UC8_PIXEL)
    return PARAM_ERR;

  ix = (long)p[0];
  iy = (long)p[1];

  src = GetPixelAddress(iy, ix);

  if (src == NULL)
    return INIT_ERR;

  dx = (p[0] - ix);
  dy = (p[1] - iy);
  dx0 = 1.0 - dx;
  dy0 = 1.0 - dy;

  p00 = *src++;
  p01 = *src;
  src += rowBytes;
  p11 = *src--;
  p10 = *src;

  if (bv)
    *bv = dy0 * (dx0 * p00 + dx * p01) +
           dy * (dx0 * p10 + dx * p11);

  return NO_ERR;
}

