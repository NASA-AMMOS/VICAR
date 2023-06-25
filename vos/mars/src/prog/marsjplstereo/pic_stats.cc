#include <math.h>
#include <string.h>
#include "JPLPic.h"


/*!
  Compute pixel statistics for arbitrary image types.

*/
void JPLPic::ComputePixelStats (double *mean_p, double *stddev_p,
				double *min_p, double *max_p,
				unsigned long *empties_p,
				unsigned long *saturated_p,
				unsigned long *hi_mask, unsigned long *lo_mask)
{
  ComputePixelStats (0, 0.0, 0.0, 0,
		     mean_p, stddev_p, min_p, max_p,
		     empties_p, saturated_p, hi_mask, lo_mask);
}


/*!
  Compute pixel statistics for arbitrary image types.

*/
void JPLPic::ComputePixelStats (int use_bounds, double low, double high,
				int band,
				double *mean_p, double *stddev_p,
				double *min_p, double *max_p,
				unsigned long *empties_p,
				unsigned long *saturated_p,
				unsigned long *hi_mask, unsigned long *lo_mask)
{
  long pixt = GetPixelType();
  double sat_val = 0.0;

  switch (pixt) {
  case ARGB32_PIXEL:
  case UC8_PIXEL: sat_val = 255.0; break;
  case INT16_PIXEL: sat_val = 65535.0; break;
  case INT32_PIXEL: sat_val = 65536.0 * 65536.0 - 1; break;
  case XYZ_FLOAT_PIXEL:
  case FLOAT_PIXEL: sat_val = 1e32; break;
  case XYZ_DOUBLE_PIXEL:
  case DOUBLE_PIXEL: sat_val = 1e300; break;
  }

  ComputePixelStats (use_bounds, low, high, band,
		     mean_p, stddev_p, min_p, max_p,
		     empties_p, sat_val,
		     saturated_p, hi_mask, lo_mask);
} // ComputePixelStats



/*!
  Compute pixel statistics for arbitrary image types.

  If use_bounds is defined, pixel intesities outside the range from
  low to high will be considered "empty".  The specific definition of
  an empty pixel is:

  empty <=> pixels are SHORTS and the value is UNDEF_DISPARITY (7FFF) or
  	    use_bounds is true and the current value not in [low, high] or
	    pixels are XYZ and value is UNDEF_3D (this band < -10000.0)
  valid <=> not empty

  \param use_bounds If nonzero, use the low and high parameters to
  bound the pixel intensities that are considered valid.  This is
  useful for filtering out "NO_RANGE" or "NO_S2_DISP" pixels from
  contributing to the totals.
  \param low Lowest valid pixel intensity, only used if use_bounds is
  nonzero.  Intensities below this are considered "empty".
  \param high Highest valid pixel intensity, only used if use_bounds is
  nonzero.  Intensities below this are considered "empty".
  \param band Only one band of an image contributes to the statistics,
  this selects the band.  Bands are numbered starting from 0, so for
  scalar pixel-valued images this should be 0.

  \param mean_p Pointer to memory that will hold the mean valid
  intensity, or NULL. Set to -1.0 if there are no valid pixels.
  \param stddev_p Pointer to memory that will hold the standard
  deviation of the valid intensities, or NULL. Set to -1.0 if there
  are no valid pixels.
  \param min_p Pointer to memory that will hold the minimum valid
  pixel value, or NULL. Set to -1.0 if there are no valid pixels.
  \param max_p Pointer to memory that will hold the maximum valid
  pixel value, or NULL. Set to -1.0 if there are no valid pixels.
  \param empties_p Pointer to memory that will hold the number of
  empty pixels, or NULL.
  \param saturated_cutoff Values equal to or greater than this are
  considered saturated
  \param saturated_p Pointer to memory that will hold the number of
  saturated pixels or NULL.
  \param hi_mask Mask that holds the OR of the high 32 bits of ALL
  non-empty pixel values.
  \param lo_mask Mask that holds the OR of the low 32 bits of ALL
  non-empty pixel values.
*/

void JPLPic::ComputePixelStats (int use_bounds, double low, double high,
				int band,
				double *mean_p, double *stddev_p,
				double *min_p, double *max_p,
				unsigned long *empties_p,
				double saturated_cutoff,
				unsigned long *saturated_p,
				unsigned long *hi_mask, unsigned long *lo_mask)
{
  long pixt = GetPixelType();
  long r, c;
  double sum = 0.0, sqsum = 0.0, val;
  double pixmin = -1.0, pixmax = -1.0;
  union {
    unsigned char *uc8;
    unsigned short *int16;
    unsigned int   *int32;
    float *f;
    double *d;
  } u;
  unsigned long empties = 0L;
  unsigned long saturated = 0L;

  long mask1 = 0, mask2 = 0;
  
  int starting = 1;

  for (r = 0; r < rows; r++) {
    u.uc8 = GetPixelAddress (r, 0);

    for (c = 0; c < cols; c++) {
      switch (pixt) {
      case UC8_PIXEL:
	mask1 |= *u.uc8;
	val = *u.uc8++;
	break;
      case INT16_PIXEL:
	if (!IS_UNDEF_DISPARITY(*u.int16))
	  mask1 |= *u.int16;
	val = *u.int16++;
	break;
      case INT32_PIXEL:
	mask1 |= *u.int32;
	val = *u.int32++;
	break;
      case FLOAT_PIXEL:
	mask1 |= *u.int32;
	val = *u.f++;
	break;
      case DOUBLE_PIXEL:
	mask2 |= *u.int32;
	mask1 |= *(u.int32+1);
	val = *u.d++;
	break;
      case XYZ_FLOAT_PIXEL:
	u.f += band;
	mask1 |= *u.int32;
	val = *u.f;
	u.f += 3 - band;
	break;
      case XYZ_DOUBLE_PIXEL:
	u.d += band;
	mask2 |= *u.int32;
	mask1 |= u.int32[1];
	val = *u.d;
	u.d += 3 - band;
	break;
      case ARGB32_PIXEL:
	u.uc8 += band;
	mask1 |= *u.uc8;
	val = *u.uc8;
	u.uc8 += 4 - band;
	break;
      default:
	val = 0;
	break;
      }

      if ((pixt == INT16_PIXEL && IS_UNDEF_DISPARITY(*(u.int16-1))) ||
	  (use_bounds && (LE (val, low) || GE (val, high))) ||
	  (IS_VECTOR_PIXEL(pixt) && pixt != ARGB32_PIXEL &&
	   IS_UNDEF_3D_POINT (val, val, val))) {
	empties++;
      } else {
	sum += val;
	sqsum += val * val;

	if (starting)
	  pixmin = pixmax = val;

	saturated += GE(val, saturated_cutoff);
	pixmin = (val < pixmin) ? val : pixmin;
	pixmax = (val > pixmax) ? val : pixmax;
	starting = 0;
      }
    } // for c
  } // for r

  double mean = -1.0;
  double variance = -1.0;
  double count = (double) rows * cols;

  if (GT(count, 0.0) && GT(count, empties)) {
    mean = sum / (count - empties);
    variance = (sqsum / (count - empties)) - (mean * mean);
  }

  if (mean_p)
    *mean_p = mean;
  if (stddev_p)
    *stddev_p = EQ(variance,0.0) ? 0.0 :
      (LT(variance, 0.0) ? -1.0 : sqrt(variance));
  if (min_p)
    *min_p = pixmin;
  if (max_p)
    *max_p = pixmax;
  if (empties_p)
    *empties_p = empties;
  if (saturated_p)
    *saturated_p = saturated;
  if (hi_mask)
    *hi_mask = mask2;
  if (lo_mask)
    *lo_mask = mask1;  
} // JPLPic::ComputePixelStats



/*!
  Write separate statistics for each image band to stdout
*/

void JPLPic::WriteStats ()
{
  WriteStats (stdout, "");
} // JPLPic::WriteStats


/*!
  Write separate statistics for each image band to the named file
*/
void JPLPic::WriteStats (char *filename)
{
  FILE *fp = filename ? fopen (filename, "w") : NULL;

  if (fp) {
    WriteStats (fp, "");
    fclose (fp);
  } else
    DBG(("WriteStats:  cannot open \"%s\"\n", filename));
}

/*!
  Write statistics about just the given band in this image to stdout
*/

void JPLPic::WriteStats (int band)
{
  WriteStats (stdout, "", 0, 0.0, 0.0, band);
}

/*!
  Write separate statistics for each image band to the output stream

  \param fp Output stream
  \param prefix Optional string to print at the beginning of each
  line, or NULL.
*/

void JPLPic::WriteStats (FILE *fp, const char *prefix)
{
  WriteStats (fp, prefix, 0, 0.0, 0.0);
}



/*!
  Write separate statistics for each image band to the output stream.
  Optionally specify a bound on those pixel values that will
  contribute to the statistics.

  \param fp Output stream
  \param prefix Optional string to print at the beginning of each
  line, or NULL.
  \param use_bounds If nonzero, use the low and high parameters to
  bound the pixel intensities that are considered valid.  This is
  useful for filtering out "NO_RANGE" or "NO_S2_DISP" pixels from
  contributing to the totals.
  \param low Lowest valid pixel intensity, only used if use_bounds is
  nonzero.  Intensities below this are considered "empty".
  \param high Highest valid pixel intensity, only used if use_bounds is
  nonzero.  Intensities below this are considered "empty".

*/



void JPLPic::WriteStats (FILE *fp, const char *prefix,
			 int use_bounds, float low, float high)
{
  int b;
  size_t bufLen = 0;
  char *buf = NULL;;

  if (prefix && BandsPerPixel() > 1) {
    bufLen = strlen(prefix) + 15;
    buf = NEW(mm, "WriteStats Band-based prefix buffer")
      char[bufLen];
    fprintf (fp, "%sMulti-band image: Pixels are %d bytes split "
	     "into %d bands\n",
	     prefix, BytesPerPixel(), BandsPerPixel());
  }

  for (b = 0; b < BandsPerPixel(); b++) {
    if (buf)
      snprintf (buf, bufLen, "%s band %d: ", prefix, b);
    WriteStats (fp, buf ? buf : prefix, use_bounds, low, high, b);
  }
}


/*!
  Write statistics just for the given image band to the output stream.
  Optionally specify a bound on those pixel values that will
  contribute to the statistics.

  empty <=> pixels are SHORTS and the value is UNDEF_DISPARITY (7FFF) or
  	    use_bounds is true and the current value not in [low, high] or
	    pixels are XYZ and value is UNDEF_3D (this band < -10000.0)
  valid <=> not empty

  \param fp Output stream
  \param prefix Optional string to print at the beginning of each
  line, or NULL.
  \param use_bounds If nonzero, use the low and high parameters to
  bound the pixel intensities that are considered valid.  This is
  useful for filtering out "NO_RANGE" or "NO_S2_DISP" pixels from
  contributing to the totals.
  \param low Lowest valid pixel intensity, only used if use_bounds is
  nonzero.  Intensities below this are considered "empty".
  \param high Highest valid pixel intensity, only used if use_bounds is
  nonzero.  Intensities below this are considered "empty".

*/



void JPLPic::WriteStats (FILE *fp, const char *prefix,
			 int use_bounds, float low, float high,
			 int band)
{
  double mean, stddev;
  double pixmin, pixmax;
  unsigned long empties;
  unsigned long saturated;
  unsigned long mask_lo, mask_hi;

  if (fp == NULL) return;
  if (prefix == NULL)
    prefix = "";

  fprintf (fp, "%s%ld rows, %ld cols, %d-bit pixels", prefix,
	   rows, cols, BytesPerPixel() * 8 / BandsPerPixel());

  ComputePixelStats (use_bounds, low, high, band,
		     &mean, &stddev, &pixmin, &pixmax,
		     &empties, &saturated, &mask_hi, &mask_lo);

  if (mask_lo || mask_hi) {
    int i, showall = 0;
    fprintf (fp, " (actual mask: ");
    for (i = sizeof(mask_hi) * 8; i > 0; i--)
      if ((mask_hi & (1 << (i-1))) || showall) {
	fprintf (fp, "%s", (mask_hi & (1 << (i-1))) ? "1" : "0");
	if (!showall)
	  showall = 32 + i;
      }
    for (i = sizeof(mask_lo) * 8; i > 0; i--)
      if ((mask_lo & (1 << (i-1))) || showall) {
	fprintf (fp, "%s", (mask_lo & (1 << (i-1))) ? "1" : "0");
	if (!showall)
	  showall = i;
      }
    fprintf (fp, ", or %d bits)", showall);
  }
  fprintf (fp, "\n");

  unsigned long count = rows * cols;

  if (EQ (count, 0.0)) {
    fprintf (fp, "%sTHERE ARE NO PIXEL VALUES AT ALL!\n", prefix);
  } else if (EQ (count, empties)) {
    fprintf (fp, "%sALL PIXELS ARE EMPTY!\n", prefix);
  } else {

    fprintf (fp,
	     "%s%sPixels are %g +/- %g in [%g:%g]\n", prefix,
	     GT (empties, 0.0) ? "Non-empty " : "",
	     mean, stddev, pixmin, pixmax);
    if (GT (empties, 0.0)) {
      fprintf (fp,
	       "%s%ld%% of the image has GOOD RANGE pixels (%ld of %ld pixels)\n",
	       prefix, 100 - (((100 * empties) + (count >> 1)) / count),
	       count - empties, count);
    }
    if (GT (saturated, 0.0)) {
      fprintf (fp,
	       "%s%ld%% of the image is SATURATED (%ld of %ld pixels)\n",
	       prefix, ((100 * saturated) + (count >> 1)) / count,
	       saturated, count);
    }
  }
}
