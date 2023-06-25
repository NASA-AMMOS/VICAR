#include <math.h>
#include "JPLStereo.h"
#include <netinet/in.h>	// for htonl()

#ifdef __unix__
#undef __unix__
#endif

#ifdef __unix__
#include "st_diag.h"	// st_diag_range_color
#include "cmap.h"	// cmap_false()
#endif


// netinet/in.h redefines INT_MAX :-P

#ifdef INT_MAX
#undef INT_MAX
#endif
#define INT_MAX(x,y) (((x) > (y)) ? (x) : (y))



long			
JPLStereo::WriteSubpixelDisparity (char *filename)
{
	if (subDispPic == NULL) {
		FatalErr("No subpixel disparity\n");
		return INIT_ERR;
	}

	return (subDispPic->Write (filename));
}



long			
JPLStereo::WriteSubpixelDisparity (FILE *fp)
{
	if (subDispPic == NULL) {
		FatalErr("No subpixel disparity\n");
		return INIT_ERR;
	}

	return (subDispPic->Write (fp));
}

long
JPLStereo::WriteRangeAsHeightField (FILE *fp)
{
  long side, r, c;
  union { long l; float f; } u;
  float pad = -10000;		/* HF_UNSET from libray/libobj/hf.h */
  float min_range = 1e6, max_range = -1e6;

  if (subDispPic == NULL || fp == NULL) {
    FatalErr ("No subpixel disparity\n");
    return INIT_ERR;
  }

  u.f = pad;
  u.l = htonl(u.l);
  pad = u.f;

  side = INT_MAX(subDispPic->rows,subDispPic->cols);
  r = htonl(side);
  fwrite (&r, sizeof(long), 1, fp);

  for (r = 0; r < side; r++) {
    float x, y, z, rng;

    if (r < subDispPic->rows) {
      short *ptr = subDispPic->GetShortPixelAddress (r, 0);

      for (c = 0; c < subDispPic->cols; c++) {
	if (*ptr++ == NO_S2_DISP) {
	  rng = pad;
	} else {
	  if (Disparity2FloatXYZRange (r, c, &x, &y, &z) != NO_ERR)
	    return INIT_ERR;
	  rng = sqrt (SQ(x - leftRectCam->C[0]) +
		      SQ(y - leftRectCam->C[1]) +
		      SQ(z - leftRectCam->C[2]));

	  if (min_range < 0 || LT(rng, min_range))
	    min_range = rng;
	  if (max_range < 0 || GT(rng, max_range))
	    max_range = rng;

	  u.f = rng;
	  u.l = htonl (u.l);
	  rng = u.f;
	}
	fwrite (&rng, sizeof(rng), 1, fp);
      }
      for (c = subDispPic->cols < side; c > 0; c--) {
	fwrite (&pad, sizeof(pad), 1, fp);
      }
    } else {
      for (c = side; c > 0; c--) {
	fwrite (&pad, sizeof(pad), 1, fp);
      }
    }
  }

  printf ("Wrote Heightfield values from %g to %g\n", min_range, max_range);

  return NO_ERR;
}


#ifndef OMIT_IMAGE_OPERATORS



long
JPLStereo::WriteSubpixelElevation (char *filename)
{
	FILE *fp;
	long result = NO_ERR;

	if (subDispPic == NULL) {
		FatalErr("No subpixel disparity\n");
		return INIT_ERR;
	}

	if ((fp = fopen (filename, "w")) == NULL) {
	  FatalErr ("Cannot open Subpixel Elevation output file\n");
	  return PARAM_ERR;
	}

	result =  WriteSubpixelElevation (fp);
	fclose (fp);
	return result;
}




long
JPLStereo::WriteSubpixelElevation (FILE *fp)
{
  double origin[3] = {0.0, 0.0, 0.0};
  double h_axis[3] = {0.0, 0.0, 1.0};

  return WriteSubpixelElevation (fp, origin, h_axis, 0.0, 0.0, 1.5);
}



// For parameter definitions see MakeSubpixelElevation comments.

long
JPLStereo::WriteSubpixelElevation (FILE *fp,
				   double origin[3], double h_axis[3],
				   double min_h, double max_h, double h_scale)
{
  JPLPic *e = MakeSubpixelElevation (origin, h_axis, min_h, max_h, h_scale);

  if (e == NULL) {
    Warning ("Failed to create Elevation Image!\n");
    return INTERNAL_ERR;
  }

  if (e->WritePPM (fp) != NO_ERR) {
    Warning ("Failed to write PPM image");
    return INTERNAL_ERR;
  }

  DELETE (mm, e);

  return NO_ERR;
} // JPLStereo::WriteSubpixelElevation




long			
JPLStereo::WriteRangeMap (FILE *fp)
{
	if (rangePic == NULL) {
		FatalErr("No Range Map\n");
		return INIT_ERR;
	}

	return (rangePic->Write (fp));
}



long			
JPLStereo::WriteRangeMap (char *filename)
{
	if (rangePic == NULL) {
		FatalErr("No Range Map\n");
		return INIT_ERR;
	}

	return (rangePic->Write (filename));
}







#ifdef __unix__

static long ComputeHeightBounds (JPLPic *rangePic, double origin[3],
				 double h_axis[3],
				 double *min_height, double *max_height,
				 double height_scale)
{
  long r, c, rows, cols;
  int first_pixel = 1;
  double p0 = origin[0], p1 = origin[1], p2 = origin[2];
  double d0 = h_axis[0], d1 = h_axis[1], d2 = h_axis[2];
  double this_min = 0, this_max = 0;
  double dsum = 0.0, dsqsum = 0.0, count = 0;

  if (rangePic == NULL || min_height == NULL || max_height == NULL)
    return PARAM_ERR;

  if (rangePic->GetPixelType() != XYZ_FLOAT_PIXEL &&
      rangePic->GetPixelType() != XYZ_DOUBLE_PIXEL) {
    Warning ("ComputeHeightBounds:  not a range image!");
    return PARAM_ERR;
  }

  rows = rangePic->rows;
  cols = rangePic->cols;
  for (r = 0; r < rows; r++)
    for (c = 0; c < cols; c++) {
      union { double *d; float *f; unsigned char *u; } x;
      double r0, r1, r2, d;

      x.u = rangePic->GetPixelAddress(r,c);
      if (rangePic->GetPixelType() == XYZ_FLOAT_PIXEL) {
	r0 = *(x.f); r1 = *(x.f + 1); r2 = *(x.f + 2);
      } else {
	r0 = *(x.d); r1 = *(x.d + 1); r2 = *(x.d + 2);
      }
      if (EQ(r0, NO_RANGE) || EQ (r1, NO_RANGE) || EQ(r2, NO_RANGE))
	continue;
      d = (r0 - p0) * d0 + (r1 - p1) * d1 + (r2 - p2) * d2;
      if (first_pixel)
	this_min = this_max = d;
      else if (LT (d, this_min))
	this_min = d;
      else if (LT (this_max, d))
	this_max = d;
      first_pixel = 0;
      dsum += d;
      dsqsum += d * d;
      count++;
    }


  double mean = dsum / count, var = dsqsum / count - mean * mean;
  double stddev = 0.0;

  if (LT (0.0, var))
    stddev = sqrt (var);

  if (LT (this_min, mean - height_scale * stddev))
    this_min = mean - height_scale * stddev;
  if (LT (mean + height_scale * stddev, this_max))
    this_max = mean + height_scale * stddev;

  if (min_height)
    *min_height = this_min;
  if (max_height)
    *max_height = this_max;

  return NO_ERR;
} // ComputeHeightBounds

#endif


JPLPic *
JPLStereo::MakeSubpixelElevation ()
{
  double origin[3] = {0.0, 0.0, 0.0};
  double h_axis[3] = {0.0, 0.0, 1.0};

  return MakeSubpixelElevation (origin, h_axis, 0.0, 0.0, 1.5);
} // JPLStereo::MakeSubpixelElevation


JPLPic *
JPLStereo::MakeSubpixelElevation(double origin[3], double h_axis[3],
				 double min_height, double max_height,
				 double height_scale)
{
  if (subDispPic == NULL) {
    FatalErr("No subpixel disparity\n");
    return NULL;
  }

  if (GenerateFloatXYZRangeImage() != NO_ERR) {
    FatalErr ("Failed to generate float range image\n");
    return NULL;
  }

  if (rangePic == NULL) {
    FatalErr ("MakeSubpixelElevation:  NULL generated range pic\n");
    return NULL;
  }

  return MakeSubpixelElevation (rangePic, origin, h_axis, min_height,
				max_height, height_scale);
} // JPLStereo::MakeSubpixelElevation




JPLPic *
JPLStereo::MakeSubpixelElevation (JPLPic *rangePicture)
{
  double origin[3] = {0.0, 0.0, 0.0};
  double h_axis[3] = {0.0, 0.0, 1.0};

  return MakeSubpixelElevation (rangePicture, origin, h_axis, 0.0, 0.0, 1.5);
}



// This routine is grossly inefficient.  It creates a float XYZ range
// image instead of just using subDispPic.  But that's the interface
// required by Todd's code, so I guess I've little choice.
//
//    origin -- Some point on the ground plane (or arbitrary reference
//		plane)
//    h_axis -- Unit vector normal to the reference plane
//    min_height, max_height -- Set bounds for the colormap
//		explicitly with these.  To autoscale, set them equal.
//    height_scale -- if autoscaling, grow to this number of standard
//		deviations.


JPLPic *
JPLStereo::MakeSubpixelElevation(JPLPic *ranPic,
				 double origin[3], double h_axis[3],
				 double min_height, double max_height,
				 double height_scale)
{
#ifndef __unix__
  // Don't bother with full color elevation images on non-unix platforms

  if (ranPic == NULL) {
    FatalErr ("Empty range image\n");
    return NULL;
  }

  return ranPic->Make8BitImage();
#else
  JPLPic by_color(mm), dblRange(mm);
  JPLPic *by_pixel = NULL;
  JPLPic *deleteme = NULL;	// st_diag_range_color needs float pixels
  long rows, cols;
  long r, c;
  unsigned char *fptr, *tptr;

  if (ranPic == NULL) {
    FatalErr ("Empty range image\n");
    return NULL;
  }
  if (ranPic->GetPixelType() != XYZ_FLOAT_PIXEL &&
      ranPic->GetPixelType() != XYZ_DOUBLE_PIXEL) {
    FatalErr ("Bad format for range image pixels, expected XYZ\n");
    return NULL;
  }
  rows = ranPic->rows;
  cols = ranPic->cols;

  by_color.Init (rows, cols + 70, ARGB32_PIXEL);

  by_pixel = NEW(mm, "ARGB32 elevation image") JPLPic(mm);

  if (by_pixel == NULL) {
    Warning ("Failed to allocate JPLPic for Elevation image!\n");
    return NULL;
  }

  by_pixel->Init (by_color.rows, by_color.cols, ARGB32_PIXEL);
  by_color.ClearImage();

  min_height = -.2; max_height = 1.0;

  unsigned char *red = by_color.GetPixelAddress(0,0);

  // Need to create an image with XYZ_FLOAT_PIXELS to use st_diag_range_color

  if (ranPic->GetPixelType() == XYZ_DOUBLE_PIXEL) {
    long tr, tc;

    deleteme = NEW(mm, "temp float pixel range image") JPLPic(mm);
    deleteme->Init (rows, cols, XYZ_FLOAT_PIXEL);
    for (tr = 0; tr < rows; tr++) {
      double *dptr;
      float *flptr;

      flptr = deleteme->GetFloatPixelAddress(tr, 0);
      dptr = ranPic->GetDoublePixelAddress(tr, 0);

      for (tc = 0; tc < cols * 3; tc++) {
	*flptr++ = (float) *dptr++;
      }
    }
    ranPic = deleteme;
  } // if ranPic->GetPixelType() == XYZ_FLOAT_PIXEL
    
  if (ComputeHeightBounds (ranPic, origin, h_axis, &min_height,
			   &max_height, height_scale) != NO_ERR) {
    Warning ("Failed to compute Height Bounds!\n");
  }
  if (ranPic->GetDoublePixelAddress() == NULL) {
    Warning ("Cannot generate Elevation image, DoublePixelAddress NULL!\n");
  } else if (st_diag_range_color ((double (*)[3])
				  ranPic->GetDoublePixelAddress(),
				  cols, rows,
				  0, 0, cols, rows,
				  0, origin, h_axis,
				  min_height, max_height,
				  by_color.cols, by_color.rows, 0, 0, 
				  cols, rows,
				  red, red + by_color.rows * by_color.cols,
				  red + 2 * by_color.rows * by_color.cols)
	     == FAILURE) {
    Warning ("Failed to generate Elevation color image!");
    if (deleteme)
      DELETE (mm, deleteme);
    return NULL;
  }

  // Color image data is written in separate color bands; first RED in
  // memory, then GREEN, then BLUE.  We have to interlace the RGB
  // values to conform to the ARGB image format.

  fptr = by_color.GetPixelAddress(0, 0);
  for (r = 0; r < by_color.rows; r++) {
    tptr = by_pixel->GetPixelAddress(r, 0);

    for (c = 0; c < by_color.cols; c++, tptr += 4, fptr++) {
      tptr[1] = fptr[0];
      tptr[2] = fptr[by_color.rows*by_color.cols];
      tptr[3] = fptr[by_color.rows*by_color.cols*2];
    }
  }

  // Add information about the height map coloring to the far right of the
  // output image.  Recall we left room for it above when we allocated
  // by_pixel.  Write it from (30 to rows-20)

  if (rows > 50) {
    int i, steps = rows / 25;
    char buf[100];
    char rmap[256], gmap[256], bmap[256];
    int need_minus = LT(min_height, 0.0);

    cmap_false (rmap, gmap, bmap);
    if (steps > 4)
      steps = 4;

    by_pixel->Text (15, cols + 10, "Elev");
    for (i = 0; i <= steps; i++) {
      int rr = 30 + ((rows - 50) * i) / steps - 4;
      int minus_space;

      snprintf (buf, 100, "%-3.3g",
	       max_height - i*(max_height - min_height) / steps);
      minus_space = (!need_minus || buf[0] == '-') ? 0 : 7;
      by_pixel->Text (rr, cols + 20 + minus_space, buf);
    }

    int pixmin = 160, pixmax = 252;	 /* from st_diag.c */

    for (i = 30; i < rows - 20; i++) {
      tptr = by_pixel->GetPixelAddress (i, cols + 4);
      tptr[1] = tptr[2] = tptr[3] = 255;
      tptr += 4;
      for (int j = 0; j < 10; j++, tptr += 4) {
	int off = pixmax - (i-30) * (pixmax-pixmin) / (rows-50);
	tptr[1] = rmap[off];
	tptr[2] = gmap[off];
	tptr[3] = bmap[off];
      }
      tptr[1] = tptr[2] = tptr[3] = 255;
    }
  } // if (rows > 50) 

  if (deleteme)
    DELETE (mm, deleteme);
  return by_pixel;
#endif
}




#endif /* ! OMIT_IMAGE_OPERATORS */
