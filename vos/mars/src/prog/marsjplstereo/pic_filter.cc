#include <math.h>
#include <string.h>	// For memcpy

#include "nav_memory.h"
#include "JPLPic.h"
#include "stereo.h"	// NO_S2_DISP


#ifndef OMIT_IMAGE_OPERATORS

long JPLPic::MiniMax (double d, int use_min)
{
  long r, c, maxrows, maxcols;
  union {
    unsigned char *uc;
    short *s;
    long *l;
    float *f;
    double *d;
  } u;

  maxrows = rows;
  maxcols = cols;
  use_min = use_min ? 1 : 0;

  for (r = 0; r < maxrows; r++) {
    u.uc = GetPixelAddress (r, 0);
    for (c = 0; c < maxcols; c++) {
      int dest_is_less, i;
      switch (pixelType) {
      case UC8_PIXEL:
	dest_is_less = *u.uc < (unsigned char) d;

	if (dest_is_less ^ use_min)
	  *u.uc = (unsigned char) d;
	u.uc++;
	break;
      case INT16_PIXEL:
	dest_is_less = *u.s < (short) d;

	if (dest_is_less ^ use_min)
	  *u.s = (short) d;
	u.s++;
	break;
      case INT32_PIXEL:
	dest_is_less = *u.l < (long) d;

	if (dest_is_less ^ use_min)
	  *u.l = (long) d;
	u.l++;
	break;
      case FLOAT_PIXEL:
	dest_is_less = *u.f < (float) d;

	if (dest_is_less ^ use_min)
	  *u.f = (float) d;
	u.f++;
	break;
      case XYZ_FLOAT_PIXEL:
	for (i = 0; i < 3; i++) {
	  dest_is_less = *u.f < (float) d;

	  if (dest_is_less ^ use_min)
	    *u.f = (float) d;
	  u.f++;
	}
	break;
      case DOUBLE_PIXEL:
	dest_is_less = *u.d < d;

	if (dest_is_less ^ use_min)
	  *u.d = d;
	u.d++;
	break;
      case XYZ_DOUBLE_PIXEL:
	for (i = 0; i < 3; i++) {
	  dest_is_less = *u.d < d;

	  if (dest_is_less ^ use_min)
	    *u.d = d;
	  u.d++;
	}
	break;
      case ARGB32_PIXEL:
	for (i = 0; i < 4; i++) {
	  dest_is_less = *u.uc < (unsigned char) d;
	  
	  if (dest_is_less ^ use_min)
	    *u.uc = (unsigned char) d;
	  u.uc++;
	}
	break;

      default:
#if 0
	EPRINT1 (EWARNING, 0, 0, MINIMAX_UNKNOWN_PIXEL,
		 "MiniMax:  Unknown pixelType %ld!\n",
		 pixelType);
#endif
	return PARAM_ERR;
      } // switch
    }
  }

  return NO_ERR;
} // JPLPic::MiniMax




long JPLPic::MiniMax (JPLPic *other, int use_min)
{
  long r, c, maxrows, maxcols;
  union {
    unsigned char *uc;
    short *s;
    long *l;
    float *f;
    double *d;
  } u, o;

  if (other == NULL || pixelType != other->GetPixelType())
    return PARAM_ERR;

  maxrows = MIN (rows, other->rows);
  maxcols = MIN (cols, other->cols);
  use_min = use_min ? 1 : 0;

  for (r = 0; r < maxrows; r++) {
    u.uc = GetPixelAddress (r, 0);
    o.uc = other->GetPixelAddress (r, 0);
    for (c = 0; c < maxcols; c++) {
      int dest_is_less, i;
      switch (pixelType) {
      case UC8_PIXEL:
	dest_is_less = *u.uc < *o.uc;

	if (dest_is_less ^ use_min)
	  *u.uc = *o.uc;
	u.uc++;
	o.uc++;
	break;
      case INT16_PIXEL:
	dest_is_less = *u.s < *o.s;

	if (dest_is_less ^ use_min)
	  *u.s = *o.s;
	u.s++;
	o.s++;
	break;
      case INT32_PIXEL:
	dest_is_less = *u.l < *o.l;

	if (dest_is_less ^ use_min)
	  *u.l = *o.l;
	u.l++;
	o.l++;
	break;
      case FLOAT_PIXEL:
	dest_is_less = *u.f < *o.f;

	if (dest_is_less ^ use_min)
	  *u.f = *o.f;
	u.f++;
	o.f++;
	break;
      case XYZ_FLOAT_PIXEL:
	for (i = 0; i < 3; i++) {
	  dest_is_less = *u.f < *o.f;

	  if (dest_is_less ^ use_min)
	    *u.f = *o.f;
	  u.f++;
	  o.f++;
	}
	break;
      case DOUBLE_PIXEL:
	dest_is_less = *u.d < *o.d;

	if (dest_is_less ^ use_min)
	  *u.d = *o.d;
	u.d++;
	o.d++;
	break;
      case XYZ_DOUBLE_PIXEL:
	for (i = 0; i < 3; i++) {
	  dest_is_less = *u.d < *o.d;

	  if (dest_is_less ^ use_min)
	    *u.d = *o.d;
	  u.d++;
	  o.d++;
	}
	break;
      case ARGB32_PIXEL:
	for (i = 0; i < 4; i++) {
	  dest_is_less = *u.uc < *o.uc;
	  
	  if (dest_is_less ^ use_min)
	    *u.uc = *o.uc;
	  u.uc++;
	  o.uc++;
	}
	break;

      default:
	fprintf (stderr, "MiniMax:  Unknown pixelType %ld!\n",
		 pixelType);
	return PARAM_ERR;
      } // switch
    }
  }

  return NO_ERR;
} // JPLPic::MiniMax




// Arbitrary linear scaling -- modifies pixels (of any type) in the JPLPic
// according to this linear scaling.

long JPLPic::LinearOperator (double mult, double add)
{
  long r, c, colmax = cols;
  union {
    unsigned char *uc;
    short *s;
    long *l;
    float *f;
    double *d;
  } u;

  if (pixelType == XYZ_FLOAT_PIXEL || pixelType == XYZ_DOUBLE_PIXEL)
    colmax = cols * 3;

  for (r = 0; r < rows; r++) {
    u.uc = GetPixelAddress(r, 0);
    for (c = 0; c < colmax; c++) {
      switch (pixelType) {
      case UC8_PIXEL:
	*u.uc = (unsigned char) (((double) *u.uc) * mult + add);
	u.uc++;
	break;
      case INT16_PIXEL:
	*u.s = (short) (((double) *u.s) * mult + add);
	u.s++;
	break;
      case INT32_PIXEL:
	*u.l = (long) (((double) *u.l) * mult + add);
	u.l++;
	break;
      case FLOAT_PIXEL:
      case XYZ_FLOAT_PIXEL:
	*u.f = (float) (((double) *u.f) * mult + add);
	u.f++;
	break;
      case DOUBLE_PIXEL:
      case XYZ_DOUBLE_PIXEL:
	*u.d = *u.d * mult + add;
	u.d++;
	break;
      case ARGB32_PIXEL:
	u.uc[0] = (unsigned char) (((double) u.uc[0]) * mult + add);
	u.uc[1] = (unsigned char) (((double) u.uc[1]) * mult + add);
	u.uc[2] = (unsigned char) (((double) u.uc[2]) * mult + add);
	u.uc[3] = (unsigned char) (((double) u.uc[3]) * mult + add);
	u.uc += 4;
	break;

      default:
	fprintf (stderr, "LinearOperator:  Unknown pixelType %ld!\n",
		 pixelType);
	return PARAM_ERR;
      } // switch
    }
  }

  return NO_ERR;
} // JPLPic::LinearOperator




// Use values found inside the images to do the linear scaling.  Either
// image pointer can be NULL.

long JPLPic::LinearOperator (JPLPic *multpic, JPLPic *addpic)
{
  long r, c, rowmax = rows, colmax = cols;
  long mult_pixelType = -1, add_pixelType = -1;
  long mult_offset = 0, mult_inc = 1;
  long  add_offset = 0,  add_inc = 1;
  double mult[4] = {1.0, 1.0, 1.0, 1.0}, add[4] = {0.0, 0.0, 0.0, 0.0};
  union {
    unsigned char *uc;
    short *s;
    long *l;
    float *f;
    double *d;
  } dst, msrc, asrc;

  dst.uc = NULL;
  msrc.uc=NULL;
  asrc.uc=NULL;

  if (multpic == NULL && addpic == NULL)
    return NO_ERR;

  if (multpic) {
    rowmax = MIN (rowmax, multpic->rows);
    colmax = MIN (colmax, multpic->cols);
    mult_pixelType = multpic->GetPixelType();
  }
  if (addpic) {
    rowmax = MIN (rowmax, addpic->rows);
    colmax = MIN (colmax, addpic->cols);
    add_pixelType = addpic->GetPixelType();
  }

  if (IS_SCALAR_PIXEL (pixelType) &&
      (mult_pixelType == XYZ_FLOAT_PIXEL ||
       mult_pixelType == XYZ_DOUBLE_PIXEL)) {
    mult_offset = 2 * BytesPerPixel (mult_pixelType);
    mult_inc = 3;
  } else if (mult_pixelType == ARGB32_PIXEL) {
    mult_offset = 0;
    mult_inc = 4;
  }

  if (IS_SCALAR_PIXEL (pixelType) &&
      (add_pixelType == XYZ_FLOAT_PIXEL ||
       add_pixelType == XYZ_DOUBLE_PIXEL)) {
    add_offset = 2 * BytesPerPixel (add_pixelType);
    add_inc = 3;
  } else if (add_pixelType == ARGB32_PIXEL) {
    add_offset = 0;
    add_inc = 4;
  }

  for (r = 0; r < rowmax; r++) {
    dst.uc = GetPixelAddress(r, 0);
    if (mult_pixelType > -1)
      msrc.uc = multpic->GetPixelAddress(r, 0) + mult_offset;
    if (add_pixelType > -1)
      asrc.uc =  addpic->GetPixelAddress(r, 0) + add_offset;

    for (c = 0; c < colmax; c++) {

      // Grab the value to multiply from the source image

      switch (mult_pixelType) {
      case -1:
	mult[0] = mult[1] = mult[2] = mult[3] = 1.0;
	break;
      case UC8_PIXEL:
	mult[0] = mult[1] = mult[2] = mult[3] = (double) *msrc.uc;
	msrc.uc += mult_inc;
	break;
      case INT16_PIXEL:
	mult[0] = mult[1] = mult[2] = mult[3] = (double) *msrc.s;
	msrc.s += mult_inc;
	break;
      case INT32_PIXEL:
	mult[0] = mult[1] = mult[2] = mult[3] = (double) *msrc.l;
	msrc.l += mult_inc;
	break;
      case FLOAT_PIXEL:
	mult[0] = mult[1] = mult[2] = mult[3] = (double) *msrc.f;
	msrc.f += mult_inc;
	break;
      case XYZ_FLOAT_PIXEL:
	mult[1] = (double) *msrc.f++;
	mult[2] = (double) *msrc.f++;
	mult[3] = (double) *msrc.f++;
	mult[0] = (mult[1] > -10000.0) && (mult[2] > -10000.0) && 
	  (mult[3] > -10000.0);
	break;
      case DOUBLE_PIXEL:
	mult[0] = mult[1] = mult[2] = mult[3] = *msrc.d;
	msrc.d += mult_inc;
	break;
      case XYZ_DOUBLE_PIXEL:
	mult[1] = *msrc.d++;
	mult[2] = *msrc.d++;
	mult[3] = *msrc.d++;
	mult[0] = (mult[1] > -10000.0) && (mult[2] > -10000.0) && 
	  (mult[3] > -10000.0);
	break;
      case ARGB32_PIXEL:
	mult[0] = (double) msrc.uc[0];
	mult[1] = (double) msrc.uc[1];
	mult[2] = (double) msrc.uc[2];
	mult[3] = (double) msrc.uc[3];
	msrc.uc += mult_inc;
	break;
      default:
	fprintf (stderr, "LinearOperator:  Unknown pixelType %ld!\n",
		 pixelType);
	return PARAM_ERR;
      } // switch

      // Grab the value to add from the source image

      switch (add_pixelType) {
      case -1:
	add[0] = add[1] = add[2] = add[3] = 0.0;
	break;
      case UC8_PIXEL:
	add[0] = add[1] = add[2] = add[3] = (double) *asrc.uc;
	asrc.uc += add_inc;
	break;
      case INT16_PIXEL:
	add[0] = add[1] = add[2] = add[3] = (double) *asrc.s;
	asrc.s += add_inc;
	break;
      case INT32_PIXEL:
	add[0] = add[1] = add[2] = add[3] = (double) *asrc.l;
	asrc.l += add_inc;
	break;
      case FLOAT_PIXEL:
	add[0] = add[1] = add[2] = add[3] = (double) *asrc.f;
	asrc.f += add_inc;
	break;
      case XYZ_FLOAT_PIXEL:
	add[1] = asrc.f[0];
	add[2] = asrc.f[1];
	add[3] = asrc.f[2];
	add[0] = (add[1] > -10000.0) && (add[2] > -10000.0) && 
	  (add[3] > -10000.0);
	asrc.f += add_inc;
	break;
      case DOUBLE_PIXEL:
	add[0] = add[1] = add[2] = add[3] = *asrc.d;
	asrc.d += add_inc;
	break;
      case XYZ_DOUBLE_PIXEL:
	add[1] = (double) asrc.d[0];
	add[2] = (double) asrc.d[1];
	add[3] = (double) asrc.d[2];
	add[0] = (add[1] > -10000.0) && (add[2] > -10000.0) && 
	  (add[3] > -10000.0);
	asrc.d += add_inc;
	break;
      case ARGB32_PIXEL:
	add[0] = (double) asrc.uc[0];
	add[1] = (double) asrc.uc[1];
	add[2] = (double) asrc.uc[2];
	add[3] = (double) asrc.uc[3];
	asrc.uc += add_inc;
	break;
      default:
	fprintf (stderr, "LinearOperator:  Unknown pixelType %ld!\n",
		 pixelType);
	return PARAM_ERR;
      } // switch

      // Write the new values into the destination image

      double dval;

      switch (pixelType) {
      case UC8_PIXEL:
	dval = (((double) *dst.uc) * mult[0] + add[0]);
	dval = MIN (255, MAX (0, dval));
	*dst.uc = (unsigned char) dval;
	dst.uc++;
	break;
      case INT16_PIXEL:
	dval = (((double) *dst.uc) * mult[0] + add[0]);
	dval = MIN (32767, MAX (-32768, dval));
	*dst.s = (short) (((double) *dst.s) * mult[0] + add[0]);
	dst.s++;
	break;
      case INT32_PIXEL:
	*dst.l = (long) (((double) *dst.l) * mult[0] + add[0]);
	dst.l++;
	break;
      case FLOAT_PIXEL:
      case XYZ_FLOAT_PIXEL:
	*dst.f = (float) (((double) *dst.f) * mult[1] + add[1]);
	dst.f++;
	if (pixelType == XYZ_FLOAT_PIXEL) {
	  dst.f[0] = (float) (((double) dst.f[0]) * mult[2] + add[3]);
	  dst.f[1] = (float) (((double) dst.f[0]) * mult[3] + add[3]);
	  dst.f += 2;
	}
	break;
      case DOUBLE_PIXEL:
      case XYZ_DOUBLE_PIXEL:
	*dst.d = *dst.d * mult[1] + add[1];
	dst.d++;
	if (pixelType == XYZ_DOUBLE_PIXEL) {
	  dst.d[0] = dst.d[0] * mult[2] + add[2];
	  dst.d[1] = dst.d[1] * mult[3] + add[3];
	  dst.d += 2;
	}
	break;
      case ARGB32_PIXEL:
	dval = (((double) dst.uc[0]) * mult[0] + add[0]);
	dval = MIN (255, MAX (0, dval));
	dst.uc[0] = (unsigned char) dval;
	dval = (((double) dst.uc[1]) * mult[1] + add[1]);
	dval = MIN (255, MAX (0, dval));
	dst.uc[1] = (unsigned char) dval;
	dval = (((double) dst.uc[2]) * mult[2] + add[2]);
	dval = MIN (255, MAX (0, dval));
	dst.uc[2] = (unsigned char) dval;
	dval = (((double) dst.uc[3]) * mult[3] + add[3]);
	dval = MIN (255, MAX (0, dval));
	dst.uc[3] = (unsigned char) dval;
	dst.uc += 4;
	break;
      default:
	fprintf (stderr, "LinearOperator:  Unknown pixelType %ld!\n",
		 pixelType);
	return PARAM_ERR;
      } // switch
    }
  }
  
  return NO_ERR;
} // JPLPic::LinearOperator



long JPLPic::AbsSub (JPLPic *other)

{
  long r, c, rowmax = rows, colmax = cols;
  long other_pixelType = -1;
  long other_offset = 0, other_inc = 1;
  double other_pix;
  union {
    unsigned char *uc;
    short *s;
    long *l;
    float *f;
    double *d;
  } dst, src;

  if (other == NULL)
    return NO_ERR;

  rowmax = MIN (rowmax, other->rows);
  colmax = MIN (colmax, other->cols);
  other_pixelType = other->GetPixelType();

  if (pixelType == XYZ_FLOAT_PIXEL || pixelType == XYZ_DOUBLE_PIXEL) {
    colmax *= 3;
  } else if (pixelType == ARGB32_PIXEL) {
    colmax *= 4;
  } else {
    if (other_pixelType == XYZ_FLOAT_PIXEL ||
	other_pixelType == XYZ_DOUBLE_PIXEL) {
      other_offset = 2 * BytesPerPixel (other_pixelType);
      other_inc = 3;
    } else if (other_pixelType == ARGB32_PIXEL) {
      other_offset = 1;	// Use RED channel if comparing ARGB32 to scalar
      other_inc = 4;
    }
  }

  for (r = 0; r < rowmax; r++) {
    dst.uc = GetPixelAddress(r, 0);
    src.uc = other->GetPixelAddress(r, 0) + other_offset;

    for (c = 0; c < colmax; c++) {

      // HACK -- this doesn't handle XYZ_ to ARGB32 or vice versa

      // Grab the value to difference from the source image

      switch (other_pixelType) {
      case -1:
	other_pix = 0.0;
	break;
      case UC8_PIXEL:
      case ARGB32_PIXEL:
	other_pix = (double) *src.uc;
	src.uc += other_inc;
	break;
      case INT16_PIXEL:
	other_pix = (double) *src.s;
	src.s += other_inc;
	break;
      case INT32_PIXEL:
	other_pix = (double) *src.l;
	src.l += other_inc;
	break;
      case FLOAT_PIXEL:
      case XYZ_FLOAT_PIXEL:
	other_pix = (double) *src.f;
	src.f += other_inc;
	break;
      case DOUBLE_PIXEL:
      case XYZ_DOUBLE_PIXEL:
	other_pix = *src.d;
	src.d += other_inc;
	break;
      default:
	fprintf (stderr, "AbsSub:  Unknown other pixelType %ld!\n", pixelType);
	return PARAM_ERR;
      } // switch

      // Grab the original value from the destination image, save difference

      switch (pixelType) {
      case -1:
	dst.uc += 1;
	break;
      case ARGB32_PIXEL:
	if (other_pixelType != ARGB32_PIXEL) {
	  dst.uc[0] = (unsigned char) ABS(other_pix - (double) dst.uc[0]);
	  dst.uc[1] = (unsigned char) ABS(other_pix - (double) dst.uc[1]);
	  dst.uc[2] = (unsigned char) ABS(other_pix - (double) dst.uc[2]);
	  dst.uc[3] = (unsigned char) ABS(other_pix - (double) dst.uc[3]);
	  dst.uc += 4;
	  break;
	} // else fall-through
      case UC8_PIXEL:
	*dst.uc = (unsigned char) ABS(other_pix - (double) *dst.uc);
	dst.uc += 1;
	break;
      case INT16_PIXEL:
	*dst.s = (short) ABS(other_pix - (double) *dst.s);
	dst.s += 1;
	break;
      case INT32_PIXEL:
	*dst.l = (long) ABS(other_pix - (double) *dst.l);
	dst.l += 1;
	break;
      case FLOAT_PIXEL:
      case XYZ_FLOAT_PIXEL:
	*dst.f = (float) ABS(other_pix - (double) *dst.f);
	dst.f += 1;
	break;
      case DOUBLE_PIXEL:
      case XYZ_DOUBLE_PIXEL:
	*dst.d = ABS(other_pix - *dst.d);
	dst.d += 1;
	break;
      default:
	fprintf (stderr, "AbsSub:  Unknown source pixelType %ld!\n", pixelType);
	return PARAM_ERR;
      } // switch
    }
  }

  return NO_ERR;
} // JPLPic::AbsSub




// Convolve an image with an arbitrary filter

JPLPic *JPLPic::ApplyFilter (JPLPic *filter)
{
  JPLPic *result = NULL, *full_image = NULL;
  long r, c, i, j, rowmax, max_cols;

  if (filter == NULL)
    return NULL;

  rowmax = 1 + rows - filter->rows;
  max_cols = 1 + cols - filter->cols;
  if (rowmax < 0 || max_cols < 0)
    return NULL;

  // Create a new JPLPic of the same type as this one to hold the results.
  // There will be a zero band surrounding the image whose size is dictated
  // by the dimensions of the filter.

  full_image = NEW(mm, "ApplyFilter full_image") JPLPic(mm);
  full_image->Init (rows, cols, pixelType);
  full_image->ClearImage ();
  result = full_image->SubImage ((filter->rows+1) / 2, (filter->cols+1) / 2,
				 rowmax, max_cols);


  for (r = 0; r < rowmax; r++) {
    for (c = 0; c < max_cols; c++) {
      JPLPic *sub_alias = SubImage (r, c, filter->rows, filter->cols);
      JPLPic *sub = NEW(mm, "ApplyFilter sub") JPLPic(mm);
      union {
	unsigned char *uc;
	short *s;
	long *l;
	float *f;
	double *d;
      } ptr, dst;
      double sum = 0.0, sum1 = 0.0, sum2 = 0.0;

      if (sub == NULL) {
	fprintf (stderr, "Applyfilter:  failed to allocate subimage\n");
	return NULL;
      }
      sub->Init (sub_alias->rows, sub_alias->cols, sub_alias->GetPixelType());
      if (sub_alias->Copy(sub) != NO_ERR) {
	fprintf (stderr, "ApplyFilter: failed to copy subimage at (%ld,%ld)\n",
		 r, c);
	return NULL;
      }

      sub->LinearOperator (filter, NULL);
      dst.uc = result->GetPixelAddress(r, c);
      ptr.uc = sub->GetPixelAddress(0,0);

      switch (sub->pixelType) {
      case UC8_PIXEL:
	for (i = 0; i < filter->rows; i++)
	  for (j = 0; j < filter->cols; j++)
	    sum += *ptr.uc++;
	*dst.uc = (unsigned char) (sum);
	break;
      case INT16_PIXEL:
	for (i = 0; i < filter->rows; i++)
	  for (j = 0; j < filter->cols; j++)
	    sum += *ptr.s++;
	*dst.s = (short) (sum);
	break;
      case INT32_PIXEL:
	for (i = 0; i < filter->rows; i++)
	  for (j = 0; j < filter->cols; j++)
	    sum += *ptr.l++;
	*dst.l = (long) (sum);
	break;
      case FLOAT_PIXEL:
	for (i = 0; i < filter->rows; i++)
	  for (j = 0; j < filter->cols; j++)
	    sum += *ptr.f++;
	*dst.f = (float) (sum);
	break;
      case DOUBLE_PIXEL:
	for (i = 0; i < filter->rows; i++)
	  for (j = 0; j < filter->cols; j++)
	    sum += *ptr.d++;
	*dst.d = (sum);
	break;
      case XYZ_FLOAT_PIXEL:
	for (i = 0; i < filter->rows; i++)
	  for (j = 0; j < filter->cols; j++) {
	    sum += *ptr.f++;
	    sum1 += *ptr.f++;
	    sum2 += *ptr.f++;
	  }
	*dst.f++ = (float) (sum);
	*dst.f++ = (float) (sum1);
	*dst.f++ = (float) (sum2);
	break;
      case XYZ_DOUBLE_PIXEL:
	for (i = 0; i < filter->rows; i++)
	  for (j = 0; j < filter->cols; j++) {
	    sum += *ptr.d++;
	    sum1 += *ptr.d++;
	    sum2 += *ptr.d++;
	  }
	*dst.d++ = (sum);
	*dst.d++ = (sum1);
	*dst.d++ = (sum2);
	break;
      default:
	fprintf (stderr, "ApplyFilter:  Unknown pixelType %ld!\n", pixelType);
	return NULL;
      } // switch

      DELETE (mm, sub_alias);
      DELETE (mm, sub);
    }
  }

  return full_image;
} // JPLPic::ApplyFilter

#endif /* ! OMIT_IMAGE_OPERATORS */



JPLPic *JPLPic::Make8BitImage (void)
{
  return Make8BitImage (0, NULL, NULL, NULL);
}

long JPLPic::Make8BitImage (JPLPic *result, int bits_to_shift)
{
  return Make8BitImage (result, 0, bits_to_shift, NULL, NULL, NULL);
}



JPLPic *JPLPic::Make8BitImage (int band, float *actual_min,
			       float *actual_mid,
			       float *actual_max)
{
  JPLPic *result = NEW(mm, "8 bit image") JPLPic(mm);

  if (IS_INT_PIXEL (pixelType)) {
    if (Make8BitImage (result, band, 8 * BytesPerPixel() - 8, actual_min,
		       actual_mid, actual_max) == NO_ERR)
      return result;
  } else
    if (Make8BitImage (result, band, 0, actual_min, actual_mid, actual_max)
	== NO_ERR)
      return result;

  DELETE (mm, result);
  return NULL;
  
}



// Create an 8bit image by shifting by an input amount, then
// truncating at the remaining low-order bits

long JPLPic::Make8BitImage (JPLPic *result, int band, int bits_to_shift,
			    float *actual_min, float *actual_mid,
			    float *actual_max)
{
  long row, col;

  if (result == NULL)
    return PARAM_ERR;

  result->Init (rows, cols, UC8_PIXEL, result->mm ? result->mm : mm);

  if (result->rows * result->cols == 0)
    return PARAM_ERR;

  for (row = 0; row < rows; row++) {
    AnythingT src;
    unsigned char *dest = result->GetPixelAddress (row, 0);

    src.uc = GetPixelAddress(row, 0);
    

    for (col = 0; col < cols; col++) {
      switch (pixelType) {
      case UC8_PIXEL:
	*dest++ = (unsigned char) ((*src.uc >> bits_to_shift) & 0xFF);
	src.uc += 1;
	break;
      case INT16_PIXEL:
	*dest++ = (unsigned char) ((*src.h >> bits_to_shift) & 0xFF);
	src.h += 1;
	break;
      case INT32_PIXEL:
	*dest++ = (unsigned char) ((*src.i >> bits_to_shift) & 0xFF);
	src.i += 1;
	break;
      case FLOAT_PIXEL:
	*dest++ = (unsigned char) ((((unsigned long) *src.f) >> bits_to_shift) & 0xFF);
	src.f += 1;
	break;
      case DOUBLE_PIXEL:
	*dest++ = (unsigned char) ((((unsigned long) *src.d) >> bits_to_shift) & 0xFF);
	src.d += 1;
	break;
      case XYZ_FLOAT_PIXEL:
	*dest++ = (unsigned char) ((((unsigned long) src.f[band]) >> bits_to_shift) & 0xFF);
	src.f += 3;
	break;
      case XYZ_DOUBLE_PIXEL:
	*dest++ = (unsigned char) ((((unsigned long) src.d[band]) >> bits_to_shift) & 0xFF);
	src.d += 3;
	break;
      case ARGB32_PIXEL:
	*dest++ = (unsigned char) ((src.uc[band] >> bits_to_shift) & 0xFF);
	src.uc += 4;
	break;
      default:
	error(ERR(8), ERR_MAJOR, ERF_COMMAND, "Make8BitImage: Unknown pixel type %ld\n",
	      pixelType);
	return PARAM_ERR;
      }
    }
  }

  if (actual_min)
    *actual_min = 0.0;
  if (actual_mid)
    *actual_mid = (float) ((unsigned long) 1 << (bits_to_shift+7)) - 1.0;
  if (actual_max)
    *actual_max = (float) ((unsigned long) 1 << (bits_to_shift+7)) * 2.0 - 1.0;
  return NO_ERR;
  
}



// Generates a new JPLPic with 8 bit pixels, with min/max scaling.
// Uses the last component of vector pixels (e.g., "Z" value from XYZ triples).

JPLPic *JPLPic::MakeScaled8BitImage (void)
{
  return MakeScaled8BitImage (0, NULL, NULL, NULL);
} // JPLPic::MakeScaled8BitImage


long JPLPic::MakeScaled8BitImage(JPLPic *result)
{
  return MakeScaled8BitImage (result, 0, 0.0, 0.0, 0);
}



JPLPic *JPLPic::MakeScaled8BitImage(int band, float *actual_min,
				    float *actual_mid,
				    float *actual_max)
{
  JPLPic *result = NEW(mm, "8 bit scaled image") JPLPic(mm);

  if (MakeScaled8BitImage(result, 0, 0.0, 0.0, band, actual_min, actual_mid,
			  actual_max) == NO_ERR)
    return result;
  return NULL;
}



long JPLPic::MakeScaled8BitImage(JPLPic *result,
				 int use_bounds, float low, float high,
				 int band)
{
  return MakeScaled8BitImage (result, use_bounds, low, high, band,
			      NULL, NULL, NULL);
}


long JPLPic::MakeScaled8BitImage(JPLPic *result,
				 int use_bounds, float low, float high,
				 int band, float *actual_min,
				 float *actual_mid, float *actual_max)
{
  long row, col;
  AnythingT ptr;

  if (result == NULL)
    return PARAM_ERR;

  result->Init(rows, cols, UC8_PIXEL, result->mm ? result->mm : mm);

  if (result->rows * result->cols == 0)
    return PARAM_ERR;

  double pixmin = 0.0, pixmax = 0.0, diff, offset;
  int starting = 1;

  // RESCALE_TYPE includes a HACK to skip over S2_NO_RANGE pixels, 0x7fff
  // in short images, and very negative values in FLOAT images

#define RESCALE_TYPE(sourceType, field, numPerPixel) \
	{for (row = 0; row < result->rows; row++) { \
	  ptr.uc = GetPixelAddress (row, 0); \
	  ptr.field += band; \
	  for (col = 0; col < result->cols; col++, ptr.field += numPerPixel) { \
	    if (sizeof(sourceType) == sizeof(short) && (*ptr.field) == 0x7fff) \
	      continue; \
	    if ((sizeof(sourceType) == sizeof(float) || \
		 sizeof(sourceType) == sizeof(double)) && (*ptr.field) <= -1e5) \
	      continue; \
	    if (use_bounds && \
		(LE((double) *ptr.field, low) || \
		 GE((double) *ptr.field, high))) \
	      continue; \
	    if (starting || LT(((double) *ptr.field),pixmin)) \
	      pixmin = (double)*ptr.field;\
	    if (starting || GT(((double) *ptr.field),pixmax)) \
	      pixmax = (double)*ptr.field;\
	    starting = 0; \
	  } } \
	if (pixmax > pixmin) { double range = (pixmax - pixmin); \
  	   if (actual_min)   *actual_min = pixmin; \
	   if (actual_max)   *actual_max = pixmax; \
	   if (actual_mid)   *actual_mid = 0.5 * (pixmax + pixmin); \
	   pixmin -= range / 512; pixmax += range / 512; } \
	diff = (pixmax - pixmin > 0) ? 1.0 / (pixmax - pixmin) : 0.0; \
	offset = LE(diff, 0.0) ? 128.0 : (-255.0 * pixmin * diff); \
	for (row = 0; row < result->rows; row++) { \
	  ptr.uc = GetPixelAddress (row, 0); \
	  ptr.field += band; \
	  unsigned char *resptr = result->GetPixelAddress(row, 0); \
	  for (col = 0; col < result->cols; \
	       resptr++, col++, ptr.field += numPerPixel) { \
	    *resptr = (unsigned char) (255 * ((double) *ptr.field) \
				       * diff + offset); \
       } } }

  switch (pixelType) {
  case UC8_PIXEL:
    RESCALE_TYPE (unsigned char, uc, 1);
    break;
  case INT16_PIXEL:
    RESCALE_TYPE (short, h, 1);
    break;
  case INT32_PIXEL:
    RESCALE_TYPE (long, i, 1);
    break;
  case FLOAT_PIXEL:
    RESCALE_TYPE (float, f, 1);
    break;
  case DOUBLE_PIXEL:
    RESCALE_TYPE (double, d, 1);
    break;
  case XYZ_FLOAT_PIXEL:
    RESCALE_TYPE (float, f, 3);
    break;
  case XYZ_DOUBLE_PIXEL:
    RESCALE_TYPE (double, d, 3);
    break;
  case ARGB32_PIXEL:
    RESCALE_TYPE (unsigned char, uc, 4);
    break;
  default:
    error(ERR(8), ERR_MAJOR, ERF_COMMAND, "MakeScaled8BitImage: Unknown pixel type %ld\n",
	     pixelType);
    DELETE (mm, result);
    break;
  }
#ifdef INTERFACE_DEBUG
  DBG(("Scaled INT pixels [%ld:%ld] using "
	   "diff=%ld, offset=%ld\n", pixmin, pixmax, diff, offset));
#endif

  return NO_ERR;
} // JPLPic::MakeScaled8BitImage



/* pixType -- primitive type of pixel contents
   accumType -- primitive type of accumulator
   getPixAdd -- appropriately typed GetPixelAddress method name
   div4 -- code to use to perform a division by 4
   div16 -- code to use to perform a division by 16
   div64 -- code to use to perform a division by 64
   cm -- number of scalars at each pixel (e.g., XYZ has 3) (column multiple)
   mask_expr -- either (sum & mask) or just (sum), depending on pixel type
*/


#define PYRLEVEL_GOOD_LOWBITS(pixType,accumType,getPixAdd,div4,div16,div64,cm,mask_expr) \
for (r = 0; r < nrows; r++) { \
  pixType *resultptr, *ptr0, *ptr1, *ptr2, *ptr3, *ptr4, *ptr5, *ptr6, *ptr7; \
 \
  ptr0 = getPixAdd (r << rpyrlevel, 0); \
  ptr1 = ptr0 + cols * cm; \
  ptr2 = ptr1 + cols * cm; \
  ptr3 = ptr2 + cols * cm; \
  ptr4 = ptr3 + cols * cm; \
  ptr5 = ptr4 + cols * cm; \
  ptr6 = ptr5 + cols * cm; \
  ptr7 = ptr6 + cols * cm; \
  resultptr = result->getPixAdd (r, 0); \
 \
  for (c = 0; c < ncols; c++) { \
    accumType sum; \
    unsigned int p, p1, p2; \
 \
    for (p = 0; p < cm; p++) { \
      if (rpyrlevel == 3 && cpyrlevel == 1) { \
	sum = (ptr0[0*cm+p] + ptr0[1*cm+p] + \
	       ptr1[0*cm+p] + ptr1[1*cm+p] + \
	       ptr2[0*cm+p] + ptr2[1*cm+p] + \
	       ptr3[0*cm+p] + ptr3[1*cm+p] + \
	       ptr4[0*cm+p] + ptr4[1*cm+p] + \
	       ptr5[0*cm+p] + ptr5[1*cm+p] + \
	       ptr6[0*cm+p] + ptr6[1*cm+p] + \
	       ptr7[0*cm+p] + ptr7[1*cm+p]) div16; \
	*resultptr++ = mask_expr; \
      } else if (rpyrlevel == 1 && cpyrlevel == 3) { \
	  sum = (ptr0[0*cm+p] + ptr0[1*cm+p] + ptr0[2*cm+p] + ptr0[3*cm+p] + \
	         ptr0[4*cm+p] + ptr0[5*cm+p] + ptr0[6*cm+p] + ptr0[7*cm+p] + \
	         ptr1[0*cm+p] + ptr1[1*cm+p] + ptr1[2*cm+p] + ptr1[3*cm+p] + \
	         ptr1[4*cm+p] + ptr1[5*cm+p] + ptr1[6*cm+p] + ptr1[7*cm+p]) div16; \
	  *resultptr++ = mask_expr; \
      } else if (rpyrlevel == cpyrlevel && rpyrlevel >= 0 && rpyrlevel <= 3) { \
        switch (rpyrlevel) { \
        case 0: \
	  believe (0 && "PyramidLevel:  Impossible case pyrlevel == 0!!"); \
        case 1: \
	  sum = (ptr0[0*cm+p] + ptr0[1*cm+p] + ptr1[0*cm+p] + ptr1[1*cm+p]) div4; \
	  *resultptr++ = mask_expr; \
	  break; \
        case 2: \
	  sum = (ptr0[0*cm+p] + ptr0[1*cm+p] + ptr0[2*cm+p] + ptr0[3*cm+p] + \
	         ptr1[0*cm+p] + ptr1[1*cm+p] + ptr1[2*cm+p] + ptr1[3*cm+p] + \
	         ptr2[0*cm+p] + ptr2[1*cm+p] + ptr2[2*cm+p] + ptr2[3*cm+p] + \
	         ptr3[0*cm+p] + ptr3[1*cm+p] + ptr3[2*cm+p] + ptr3[3*cm+p]) div16; \
	  *resultptr++ = mask_expr; \
	  break; \
        case 3: \
	  sum = (ptr0[0*cm+p] + ptr0[1*cm+p] + ptr0[2*cm+p] + ptr0[3*cm+p] + \
	         ptr0[4*cm+p] + ptr0[5*cm+p] + ptr0[6*cm+p] + ptr0[7*cm+p] + \
	         ptr1[0*cm+p] + ptr1[1*cm+p] + ptr1[2*cm+p] + ptr1[3*cm+p] + \
	         ptr1[4*cm+p] + ptr1[5*cm+p] + ptr1[6*cm+p] + ptr1[7*cm+p] + \
	         ptr2[0*cm+p] + ptr2[1*cm+p] + ptr2[2*cm+p] + ptr2[3*cm+p] + \
	         ptr2[4*cm+p] + ptr2[5*cm+p] + ptr2[6*cm+p] + ptr2[7*cm+p] + \
	         ptr3[0*cm+p] + ptr3[1*cm+p] + ptr3[2*cm+p] + ptr3[3*cm+p] + \
	         ptr3[4*cm+p] + ptr3[5*cm+p] + ptr3[6*cm+p] + ptr3[7*cm+p] + \
	         ptr4[0*cm+p] + ptr4[1*cm+p] + ptr4[2*cm+p] + ptr4[3*cm+p] + \
	         ptr4[4*cm+p] + ptr4[5*cm+p] + ptr4[6*cm+p] + ptr4[7*cm+p] + \
	         ptr5[0*cm+p] + ptr5[1*cm+p] + ptr5[2*cm+p] + ptr5[3*cm+p] + \
	         ptr5[4*cm+p] + ptr5[5*cm+p] + ptr5[6*cm+p] + ptr5[7*cm+p] + \
	         ptr6[0*cm+p] + ptr6[1*cm+p] + ptr6[2*cm+p] + ptr6[3*cm+p] + \
	         ptr6[4*cm+p] + ptr6[5*cm+p] + ptr6[6*cm+p] + ptr6[7*cm+p] + \
	         ptr7[0*cm+p] + ptr7[1*cm+p] + ptr7[2*cm+p] + ptr7[3*cm+p] + \
	         ptr7[4*cm+p] + ptr7[5*cm+p] + ptr7[6*cm+p] + ptr7[7*cm+p]) div64; \
	  *resultptr++ = mask_expr; \
	  break; \
        } /* switch */ \
      } else { \
	pixType rmaxp = 1 << rpyrlevel, cmaxp = 1 << cpyrlevel; \
	 \
	sum = 0; \
	 \
	for (p1 = 0; p1 < cmaxp; p1++) \
	  for (p2 = 0; p2 < rmaxp; p2++) { \
	    sum += ptr0[p + p1 * cm  + p2 * cols * cm]; \
	  } \
	sum /= (1 << (rpyrlevel + cpyrlevel)); \
	*resultptr++ = mask_expr; \
      } \
    } /* for p = 0 */ \
    if (rpyrlevel == 3 && cpyrlevel == 1) { \
      ptr0 += 2*cm; ptr1 += 2*cm; ptr2 += 2*cm; ptr3 += 2*cm; \
      ptr4 += 2*cm; ptr5 += 2*cm; ptr6 += 2*cm; ptr7 += 2*cm; \
    } else if (rpyrlevel == 1 && cpyrlevel == 3) { \
      ptr0 += 8*cm; ptr1 += 8*cm; \
    } else { \
      switch (cpyrlevel) { \
      case 1: \
        ptr0 += 2*cm; ptr1 += 2*cm; \
        break; \
      case 2: \
        ptr0 += 4*cm; ptr1 += 4*cm; ptr2 += 4*cm; ptr3 += 4*cm; \
        break; \
      case 3: \
        ptr0 += 8*cm; ptr1 += 8*cm; ptr2 += 8*cm; ptr3 += 8*cm; \
        ptr4 += 8*cm; ptr5 += 8*cm; ptr6 += 8*cm; ptr7 += 8*cm; \
        break; \
      default: \
        ptr0 += (1 << cpyrlevel)*cm; \
        break; \
      } /* switch */ \
    } /* if */ \
  } /* for c = 0 */ \
} /* for r = 0 */



long JPLPic::PyramidLevel (JPLPic *result, int rpyrlevel, int cpyrlevel)
{
  unsigned long nrows, ncols, r, c;
  
  if (rpyrlevel < 0 || cpyrlevel < 0 || result == NULL)
    return PARAM_ERR;

  if (rpyrlevel == 0 && cpyrlevel == 0) {
    result->Init (rows, cols, pixelType);
    return this->Copy (result);
  }

  nrows = rows >> rpyrlevel;
  ncols = cols >> cpyrlevel;

  if (result->Init (nrows, ncols, pixelType, result->mm ? result->mm : mm)
      != NO_ERR) {
    DBG(("PyramidLevel: Failed to Initialize result!\n"));
    return INIT_ERR;
  }

  switch (pixelType) {
  case UC8_PIXEL:
    PYRLEVEL_GOOD_LOWBITS (unsigned char, unsigned long, GetPixelAddress,
			   >> 2, >> 4, >> 6, 1, 
			   sum & ((1 << (8*sizeof(char))) - 1));
    break;
  case INT16_PIXEL:
    PYRLEVEL_GOOD_LOWBITS (unsigned short, unsigned long,
			   GetUShortPixelAddress, >> 2, >> 4, >> 6, 1,
			   sum & ((1 << (8*sizeof(short))) - 1));
    break;
  case INT32_PIXEL:
    PYRLEVEL_GOOD_LOWBITS (unsigned long, unsigned long,
			   GetULongPixelAddress,
			   >> 2, >> 4, >> 6, 1, sum);
    break;
  case FLOAT_PIXEL:
    PYRLEVEL_GOOD_LOWBITS (float, float, GetFloatPixelAddress,
			   / 4.0, / 16.0, / 64.0, 1, sum);
    break;
  case DOUBLE_PIXEL:
    PYRLEVEL_GOOD_LOWBITS (double, double, GetDoublePixelAddress,
			   / 4.0, / 16.0, / 64.0, 1, sum);
    break;
  case XYZ_FLOAT_PIXEL:
    PYRLEVEL_GOOD_LOWBITS (float, float, GetFloatPixelAddress,
			   / 4.0, / 16.0, / 64.0, 3, sum);
    break;
  case XYZ_DOUBLE_PIXEL:
    PYRLEVEL_GOOD_LOWBITS (double, double, GetDoublePixelAddress,
			   / 4.0, / 16.0, / 64.0, 3, sum);
    break;
  case ARGB32_PIXEL:
    PYRLEVEL_GOOD_LOWBITS (unsigned char, unsigned long, GetPixelAddress,
			   >> 2, >> 4, >> 6, 4,
			   sum & ((1 << (8*sizeof(char))) - 1));
    break;
  default:
    DBG(("PyramidLevel: Invalid pixelType %ld\n", pixelType));
    return PARAM_ERR;
  } // switch

  return NO_ERR;
} // JPLPic::PyramidLevel



JPLPic *JPLPic::SubsamplePyrLevel (int rpyrlevel, int cpyrlevel)
{
  JPLPic *retval = NULL;
  long new_rows, new_cols;
  long rscale = 1 << rpyrlevel;
  long cscale = 1 << cpyrlevel;

  if (rpyrlevel < 1 || rpyrlevel > 3 ||
      cpyrlevel < 1 || cpyrlevel > 3)
    return NULL;

  new_rows = rows >> rpyrlevel;
  new_cols = cols >> cpyrlevel;

  retval = NEW(mm, "PyrLevel image") JPLPic(mm);
  if (retval == NULL)
    return NULL;

  retval->Init(new_rows, new_cols, UC8_PIXEL);
  for (long r = 0; r < new_rows; r++) {
    unsigned char *dest = retval->GetPixelAddress(r, 0);
    unsigned char *src = GetPixelAddress(r * rscale, 0);

    if (dest && src)
      for (long c = 0; c < new_cols; c++, dest++, src += cscale)
	*dest = *src;
  }

  return retval;
} // JPLPic::SubsamplePyrLevel


/*!
  Extract a copy of the named band from the input image.  Bands
  outside the range of what is defined in the image result in NULL.

  \param band Band number, starting at 0.

  \param result Pre-allocated storage for the result, must not be NULL

  \return NO_ERR if successful, PARAM_ERR if that band could not be
  accessed
*/

long JPLPic::ExtractBand (JPLPic *result, int band)
{
  long r, c;
  int bands = BandsPerPixel();

  if (band < 0 || band >= bands)
    return PARAM_ERR;

  if (result == NULL)
    result = NEW(mm, "ExtractBand new image") JPLPic(mm);

  switch (pixelType) {
  case UC8_PIXEL:
  case INT16_PIXEL:
  case INT32_PIXEL:
  case FLOAT_PIXEL:
  case DOUBLE_PIXEL:
    return this->Copy (result);
  case XYZ_FLOAT_PIXEL:
    result->Init (rows, cols, FLOAT_PIXEL);

    for (r = 0; r < rows; r++) {
      float *src = GetFloatPixelAddress (r, 0) + band;
      float *dest = result->GetFloatPixelAddress (r, 0);
      if (src && dest)
	for (c = 0; c < cols; c++) {
	  *dest++ = *src;
	  src += bands;
	}
    }
    // Copy the relevant bits of the colormap
    {
      int band_bytes = result->BytesPerPixel();

      for (r = 0; r < 256; r++)
	result->LoadColorMapEntry (r,
				   GetColorMapEntry (r) + band * band_bytes,
				   band_bytes);
    }
    break;
  case XYZ_DOUBLE_PIXEL:
    result->Init (rows, cols, DOUBLE_PIXEL);

    for (r = 0; r < rows; r++) {
      double *src = GetDoublePixelAddress (r, 0) + band;
      double *dest = result->GetDoublePixelAddress (r, 0);
      if (src && dest)
	for (c = 0; c < cols; c++) {
	  *dest++ = *src;
	  src += bands;
	}
    }
    // Copy the relevant bits of the colormap
    {
      int band_bytes = result->BytesPerPixel();

      for (r = 0; r < 256; r++)
	result->LoadColorMapEntry (r,
				   GetColorMapEntry (r) + band * band_bytes,
				   band_bytes);
    }
    break;
  case ARGB32_PIXEL:
    result->Init (rows, cols, UC8_PIXEL);

    for (r = 0; r < rows; r++) {
      unsigned char *src = GetPixelAddress (r, 0) + band;
      unsigned char *dest = result->GetPixelAddress (r, 0);
      if (src && dest)
	for (c = 0; c < cols; c++) {
	  *dest++ = *src;
	  src += bands;
	}
    // Copy the relevant bits of the colormap
    {
      int band_bytes = result->BytesPerPixel();

      for (r = 0; r < 256; r++)
	result->LoadColorMapEntry (r,
				   GetColorMapEntry (r) + band * band_bytes,
				   band_bytes);
    }
    }
  default:
    DBG(("ExtractBand: Unknown pixel type %ld\n", pixelType));
    break;
  } // switch

  result->SetUnits (this);

  return NO_ERR;
} // JPLPic::ExtractBand




long JPLPic::Rotate (JPLPic *result, float degrees_ccw,
			    long src_center_row, long src_center_col,
			    long dest_center_row, long dest_center_col)
{
  if (result == NULL)
    return PARAM_ERR;

  return INIT_ERR;

} // JPLPic::Rotate




long JPLPic::Resample (JPLPic *result, long nrows, long ncols)
{
  long r, c;
  int pyrlevel = 0;
  float o_r, oc, rinc, cinc;
  long rs, cs, re, ce;
  AnythingT ptr;

  if (result == NULL)
    return PARAM_ERR;

  r = rows; c = cols;
  while (r > nrows && c > ncols) {
    pyrlevel++;
    r >>= 1;
    c >>= 1;
  }

  if (r == nrows && c == ncols)
    return PyramidLevel (result, pyrlevel);

  if (result->Init (nrows, ncols, pixelType, result->mm ? result->mm : mm)
      != NO_ERR) {
    DBG(("Resample: Failed to Initialize result!\n"));
    return INIT_ERR;
  }

  rinc = ((float) rows) / ((float) nrows);
  cinc = ((float) cols) / ((float) ncols);
  ptr.uc = result->GetPixelAddress();

  double count_sum = 0.0, count_sumsq = 0.0;

  for (o_r = 0.0, r = 0; r < nrows; r++, o_r += rinc)
    for (oc = 0.0, c = 0; c < ncols; c++, oc += cinc) {
      AnythingT sptr;
      float rlcoeff, rmcoeff, rrcoeff;
      float clcoeff, cmcoeff, crcoeff;
      long ir, ic;

      rs = (long) o_r;
      re = (long) (o_r + rinc);
      cs = (long) oc;
      ce = (long) (oc + cinc);

      // Compute relative contributions of first ("leftmost") pixel, last
      // ("rightmost") pixel, and those in-between ("middle").  Note that
      // thanks to round-off error, we often get middle or ending values
      // of nearly-0, but it's okay.  Just be careful not to try to multiply
      // them by pixels that don't exist in the source image.

      rlcoeff = MIN(rinc, ((float) rs + 1) - o_r) / rinc;
      rrcoeff = MIN(o_r + rinc - (float) re, re - rs) / rinc;
      rmcoeff = 0.0;
      if (re > rs + 1)
	rmcoeff = (1.0 - (rlcoeff + rrcoeff)) / (re - (rs+1));

      // Bi-linear interpolation is separable; this code mimics that above

      clcoeff = MIN(cinc, ((float) cs + 1) - oc) / cinc;
      crcoeff = MIN(oc + cinc - (float) ce, ce - cs) / cinc;
      cmcoeff = 0.0;
      if (ce > cs + 1)
	cmcoeff = (1.0 - (clcoeff + crcoeff)) / (ce - (cs+1));

      int crows = 1 + re - rs, mrows;
      int ccols = 1 + ce - cs, mcols;
      float M[3][3];

      mrows = (crows < 3) ? crows : 3;
      mcols = (ccols < 3) ? ccols : 3;
      M[0][0] = M[0][1] = M[0][2] = rlcoeff;
      M[1][1] = M[1][2] = M[2][1] = M[2][2] = 1.0;
      M[0][0] *= clcoeff;
      M[1][0] = M[2][0] = clcoeff;
      if (re > rs) {
	M[mrows-1][0] *= rrcoeff;
	M[mrows-1][1] *= rrcoeff;
	M[mrows-1][2] *= rrcoeff;
	if (re > rs+1) {
	  M[1][0] *= rmcoeff;
	  M[1][1] *= rmcoeff;
	  M[1][2] *= rmcoeff;
	}
      }
      if (ce > cs) {
	M[0][mcols-1] *= crcoeff;
	M[1][mcols-1] *= crcoeff;
	M[2][mcols-1] *= crcoeff;
	if (ce > cs+1) {
	  M[0][1] *= cmcoeff;
	  M[1][1] *= cmcoeff;
	  M[2][1] *= cmcoeff;
	}	
      }

#if 0
      // We often end up with "virtually zero" in the final column.  Check
      // here to avoid extra sampling.  For now we won't add these percentages
      // (less than 0.1% total) back into the terms that get used.

      if (mrows > 1 &&
	  LT(M[mrows-1][0] + M[mrows-1][1] + M[mrows-1][2], 0.001))
	mrows--, re--;
      if (mcols > 1 &&
	  LT(M[0][mcols-1] + M[1][mcols-1] + M[2][mcols-1], 0.001))
	mcols--, ce--;
#endif

      double sum = 0.0, sum2 = 0.0, sum3 = 0.0, sum4 = 0.0;
      double count = 0.0;

      for (ir = rs; ir <= re && ir < rows; ir++) {
	for (ic = cs; ic <= ce && ic < cols; ic++) {
	  int mr = (ir == rs) ? 0 : ((ir == re) ? (mrows-1) : 1);
	  int mc = (ic == cs) ? 0 : ((ic == ce) ? (mcols-1) : 1);

	  sptr.uc = GetPixelAddress (ir, ic);
	  if (sptr.uc == NULL) {
	    DBG(("Resample: Failed to lookup %ld,%ld at %ld,%ld!!\n", ir, ic,
		 r, c));
	    return PARAM_ERR;
	  }
	  count++;

	  switch (pixelType) {
	  case UC8_PIXEL:
	    sum += ((double) *sptr.uc) * M[mr][mc];
	    break;
	  case INT16_PIXEL:
	    // SPECIAL CASE:  NO_S2_DISP is a background pixel that doesn't
	    // get blended with the others
	    if (*sptr.uh == NO_S2_DISP)
	      count--;
	    else
	      sum += ((double) *sptr.uh) * M[mr][mc];
	    break;
	  case INT32_PIXEL:
	    sum += ((double) *sptr.ul) * M[mr][mc];
	    break;
	  case FLOAT_PIXEL:
	    sum += ((double) *sptr.f) * M[mr][mc];
	    break;
	  case DOUBLE_PIXEL:
	    sum += ((double) *sptr.d) * M[mr][mc];
	    break;
	  case XYZ_FLOAT_PIXEL:
	    // SPECIAL CASE: NO_RANGE*3 is a background pixel that doesn't
	    // get blended with the others
	    if (LE (sptr.f[0], NO_RANGE) &&
		LE (sptr.f[1], NO_RANGE) &&
		LE (sptr.f[2], NO_RANGE)) {
	      count--;
	      sptr.f += 2;
	    } else {
	      sum += ((double) *sptr.f++) * M[mr][mc];
	      sum2 += ((double) *sptr.f++) * M[mr][mc];
	      sum3 += ((double) *sptr.f) * M[mr][mc];
	    }
	    break;
	  case XYZ_DOUBLE_PIXEL:
	    // SPECIAL CASE: NO_RANGE*3 is a background pixel that doesn't
	    // get blended with the others
	    if (LE (sptr.d[0], NO_RANGE) &&
		LE (sptr.d[1], NO_RANGE) &&
		LE (sptr.d[2], NO_RANGE)) {
	      count--;
	      sptr.d += 2;
	    } else {
	      sum += ((double) *sptr.d++) * M[mr][mc];
	      sum2 += ((double) *sptr.d++) * M[mr][mc];
	      sum3 += ((double) *sptr.d) * M[mr][mc];
	    }
	    break;
	  case ARGB32_PIXEL:
	    sum += ((double) *sptr.uc++) * M[mr][mc];
	    sum2 += ((double) *sptr.uc++) * M[mr][mc];
	    sum3 += ((double) *sptr.uc++) * M[mr][mc];
	    sum4 += ((double) *sptr.uc) * M[mr][mc];
	    break;
	  default:
	    DBG(("Resample: Invalid pixelType %ld\n", pixelType));
	    return PARAM_ERR;
	  } // switch
	} // for (ic = cs;
      } // for (ir = rs;

      count_sum += count;
      count_sumsq += count * count;

      switch (pixelType) {
      case UC8_PIXEL:
	*ptr.uc++ = (unsigned char) sum;
	break;
      case INT16_PIXEL:
	// SPECIAL CASE:  NO_S2_DISP is a background pixel that doesn't
	// get blended with the others
	if (EQ(count, 0.0))
	  *ptr.uh++ = NO_S2_DISP;
	else
	  *ptr.uh++ = (unsigned short) sum;
	break;
      case INT32_PIXEL:
	*ptr.ul++ = (unsigned long) sum;
	break;
      case FLOAT_PIXEL:
	*ptr.f++ = (float) sum;
	break;
      case DOUBLE_PIXEL:
	*ptr.d++ = (double) sum;
	break;
      case XYZ_FLOAT_PIXEL:
	// SPECIAL CASE: NO_RANGE*3 is a background pixel that doesn't
	// get blended with the others
	if (EQ(count, 0.0)) {
	  *ptr.f++ = NO_RANGE;
	  *ptr.f++ = NO_RANGE;
	  *ptr.f++ = NO_RANGE;
	} else {
	  *ptr.f++ = (float) sum;
	  *ptr.f++ = (float) sum2;
	  *ptr.f++ = (float) sum3;
	}
	break;
      case XYZ_DOUBLE_PIXEL:
	// SPECIAL CASE: NO_RANGE*3 is a background pixel that doesn't
	// get blended with the others
	if (EQ(count, 0.0)) {
	  *ptr.d++ = NO_RANGE;
	  *ptr.d++ = NO_RANGE;
	  *ptr.d++ = NO_RANGE;
	} else {
	  *ptr.d++ = (double) sum;
	  *ptr.d++ = (double) sum2;
	  *ptr.d++ = (double) sum3;
	}
	break;
      case ARGB32_PIXEL:
	*ptr.uc++ = (unsigned char) sum;
	*ptr.uc++ = (unsigned char) sum2;
	*ptr.uc++ = (unsigned char) sum3;
	*ptr.uc++ = (unsigned char) sum4;
	break;
      default:
	DBG(("Resample: Impossible pixelType %ld\n", pixelType));
	return PARAM_ERR;
      } // switch
    } // for (c = 0;

  double mean = count_sum / (nrows * ncols);
  double var = count_sumsq / (nrows * ncols) - mean * mean;
  DBG(("Resample:  On average, %g pixels +/- %g contributed to one pixel\n",  mean, sqrt(var)));

  return NO_ERR;
} // JPLPic::Resample



long
JPLPic::ClearImage (unsigned char cmap_val)
{
  // FillBox checks to see if memset can be used, and is thus quite fast
  FillBox (0, 0, rows, cols, 0, cmap_val, 0);

  return NO_ERR;
}



JPLPic *JPLPic::HorizontalMirrorImage ()
{
  return HorizontalMirrorImage ((JPLPic *) NULL);
}




JPLPic *JPLPic::HorizontalMirrorImage (JPLPic *result)
{
  long row, col;

  if (result == NULL)
    result = NEW(mm, "mirror image") JPLPic(mm);

  if (result == NULL)
    return NULL;

  if (result->Init (rows, cols, pixelType) != NO_ERR) {
    Warning ("Cannot resize new pic in HorizontalMirrorImage\n");
    return NULL;
  }

  // Inefficient - copy everything over, include the image, even though we're
  // going to recopy the image below

  Copy (result);

  unsigned char *s, *src = GetPixelAddress();
  unsigned char *d, *dst = result->GetPixelAddress();
  int b, bpp = BytesPerPixel();

  for (row = rows; src && dst && row--;
       src += rowBytes, dst += result->GetRowBytes()) {
    s = src;
    d = dst + (cols - 1) * bpp;
    for (col = cols; col--; d -= bpp, s += bpp)
      for (b = 0; b < bpp; b++)
	d[b] = s[b];
  } // for

  // Rescale the offset / increment bounds if units have been set

  if (col_units)
    result->SetUnits (row_units, free_row_units, row_offset, row_inc,
		      col_units, free_col_units, col_offset + cols * col_inc,
		      -col_inc);

  return result;
}




#ifndef OMIT_IMAGE_OPERATORS

long JPLPic::Overlay (JPLPic *ontopPic, long row, long col, int wrap)
{
  long ontop_row = MAX(0, -row);
  long ontop_col = MAX(0, -col);

  if (ontopPic == NULL)
    return PARAM_ERR;
  if (wrap == 0 && (row >= rows || col >= cols ||
		    ontop_row >= ontopPic->rows ||
		    ontop_col >= ontopPic->cols))
    return PARAM_ERR;

#ifdef DEBUG_OVERLAY
  fprintf (stderr, "OVERLAY:  %ldx%ld %s at (%ld,%ld) in %ldx%ld %s\n",
	   ontopPic->rows, ontopPic->cols,
	   PixelTag2s((enum PixelTag) ontopPic->GetPixelType()),
	   row, col, rows, cols,
	   PixelTag2s((enum PixelTag) pixelType));
#endif
  if (wrap == 0) {
    row = MAX(0, row);
    col = MAX(0, col);
  }

  /* Make a special case out of including a range image in a color image.
     Call the elevation-colorizer to include it */
  /* HACK HACK incomplete */

  if (wrap == 0 && pixelType == ontopPic->GetPixelType()) {
    for (int r = row; r < rows && ontop_row < ontopPic->rows;
	 r++, ontop_row++) {
      unsigned char *ptr = GetPixelAddress (r, col);
      unsigned char *tptr = ontopPic->GetPixelAddress (ontop_row, ontop_col);
      if (ptr && tptr) {
	long size = MIN(cols-col, ontopPic->cols-ontop_col);
	memcpy (ptr, tptr, size * ontopPic->BytesPerPixel());
      }
    }
  } else {
    int oc, c, large_pixels = (pixelType != UC8_PIXEL) &&
      (ontopPic->GetPixelType() != UC8_PIXEL);
    long lastdestrow = rows, lastdestcol = cols;

    if (wrap) {
      lastdestrow = row + ontopPic->rows;
      lastdestcol = col + ontopPic->cols;
    }

    for (int r = row; r < lastdestrow && ontop_row < ontopPic->rows;
	 r++, ontop_row++) {
      for (c = col, oc = ontop_col;
	   c < lastdestcol && oc < ontopPic->cols;
	   c++, oc++) {
	if (large_pixels) {
	  LoadColorMapEntry (0,
			     ontopPic->GetPixelAddress(ontop_row, oc),
			     ontopPic->BytesPerPixel());
	  SetPixel (r, c, wrap, 0, 0);
	} else {
	  unsigned char *first_byte = ontopPic->GetPixelAddress(ontop_row,
								oc);
	  unsigned char t[3];
	  t[0] = t[1] = t[2] = first_byte ? *first_byte : 0;
	  LoadColorMapEntry (t[0], t, sizeof(t));
	  SetPixel (r, c, wrap, t[0], 0);
	}
      }
    }
  }

  return NO_ERR;
}

#endif /* ! OMIT_IMAGE_OPERATORS */



#define HISTOGRAM_BIN_MAG	255

long     
JPLPic::ComputeThreshold (int *lowT, int *highT, float fraction)
{
  unsigned char *px; 
  long xRB;
  long sum, gpth, buckets[HISTOGRAM_BIN_MAG];
  register long indx;
  long row, col;
	
  if (fraction <= 0.0 || fraction >= 1.0) return INIT_ERR;
	
  px = this->GetPixelAddress ();
  xRB = this->GetRowBytes ();
	
  for (row = 0; row < HISTOGRAM_BIN_MAG; row ++)
    buckets[row] = 0;
	
  if (pixelType == UC8_PIXEL) {
    unsigned char *ppx;
    for (row = 0; row < rows; row ++, px += xRB) {
      ppx = (unsigned char *) px;
      for (col = 0; col < cols; col ++, ppx++) {
	indx = *ppx;
	if (indx >= HISTOGRAM_BIN_MAG)	
	  indx = HISTOGRAM_BIN_MAG - 1;
	buckets[indx] ++;
      }
    }	
  } else if (pixelType == INT16_PIXEL) {
    unsigned short *ppx;
    for (row = 0; row < rows; row ++, px += xRB) {
      AnythingT aptr;
      aptr.uc = px;

      ppx = aptr.uh;
      for (col = 0; col < cols; col ++, ppx++) {
	indx = (*ppx);
	if (indx >= HISTOGRAM_BIN_MAG)	
	  indx = HISTOGRAM_BIN_MAG - 1;
	buckets[indx] ++;
      }
    }	
  } else if (pixelType == INT32_PIXEL) {
    unsigned long *ppx;
    for (row = 0; row < rows; row ++, px += xRB) {
      AnythingT aptr;
      aptr.uc = px;
      ppx = aptr.ul;
      for (col = 0; col < cols; col ++, ppx++) {
	indx = (*ppx);
	if (indx >= HISTOGRAM_BIN_MAG)	
	  indx = HISTOGRAM_BIN_MAG - 1;
	buckets[indx] ++;
      }
    }	
  } else {
    FatalErr("Threshold finding for the pixel type not implemented\n");
    return INIT_ERR;
  }
	
  gpth = (int) (rows * cols * fraction + 0.5);
  sum = 0;
  for (row = 0; row < HISTOGRAM_BIN_MAG; row ++) {
    //printf("(%ld %ld) ", row, buckets[row]);
    sum += buckets[row];
    if (sum >= gpth) break;
  }
	
  *lowT = row;
  *highT = (*lowT) * 2;
	
  return NO_ERR;
}

long
JPLPic::Threshold (JPLPic *dstPic, int threshold)
{
  unsigned char *src = pixels;
  unsigned char *dst = dstPic->GetPixelAddress ();
  long dstRB = dstPic->GetRowBytes ();
  register unsigned char *d;
  long row, col;
	
  if (pixelType != UC8_PIXEL && pixelType != INT16_PIXEL &&
      pixelType != INT32_PIXEL) {
    FatalErr("Thresholding for the pixel type not implemented\n");
    return INIT_ERR;
  }
  if (dstPic->pixelType != UC8_PIXEL) {
    FatalErr("Incorrect binary image format in threshold\n");
    return INIT_ERR;
  }
  if (dstPic->rows != rows || dstPic->cols != cols) {
    FatalErr("Image size not consistent in threshold\n");
    return INIT_ERR;
  }
  if (pixelType == UC8_PIXEL) {
    register unsigned char *s;
    for (row = 0; row < rows; row ++, src += rowBytes, dst += dstRB) {
      s = (unsigned char *) src;
      d = (unsigned char *) dst;
      for (col = cols; col--; s++, d++) {
	if ((*s) > threshold) 
	  *d = 255;
	else
	  *d = 0;
      }
    }
  } else if (pixelType == INT16_PIXEL) {
    register unsigned short *s;
    for (row = 0; row < rows; row ++, src += rowBytes, dst += dstRB) {
      AnythingT aptr;
      aptr.uc = src;
      s = aptr.uh;
      d = (unsigned char *) dst;
      for (col = cols; col--; s++, d++) {
	if ((*s) > threshold) 
	  *d = 255;
	else
	  *d = 0;
      }
    }
  } else if (pixelType == INT32_PIXEL){
    register unsigned long *s;
    for (row = 0; row < rows; row ++, src += rowBytes, dst += dstRB) {
      AnythingT aptr;
      aptr.uc = src;
      s = aptr.ul;
      d = (unsigned char *) dst;
      for (col = cols; col--; s++, d++) {
	if ((signed int) (*s) > threshold) 
	  *d = 255;
	else
	  *d = 0;
      }
    }
  }

  return NO_ERR;
}



#ifndef OMIT_IMAGE_OPERATORS

// HACK HACK -- this uses recursion to mark pixels; the stack could
// overflow it you're not careful.
static void mark_blob (JPLPic *pic, long start_row, long start_col,
                 unsigned char old_value, unsigned char new_pixel_value)
{
  unsigned char *this_ptr = pic->GetPixelAddress(start_row, start_col);
  long bytes_per_row = pic->GetRowBytes();

  if (this_ptr == NULL)
    return;

  if (*this_ptr == old_value) {
    *this_ptr = new_pixel_value;
    if (start_row > 0 && *(this_ptr - bytes_per_row) == old_value)
      mark_blob (pic, start_row - 1, start_col, old_value, new_pixel_value);
    if (start_col > 0 && *(this_ptr - 1) == old_value)
      mark_blob (pic, start_row, start_col - 1, old_value, new_pixel_value);
    if (start_row < pic->rows-1 && *(this_ptr + bytes_per_row) == old_value)
      mark_blob (pic, start_row + 1, start_col, old_value, new_pixel_value);
    if (start_col < pic->cols-1 && *(this_ptr + 1) == old_value)
      mark_blob (pic, start_row, start_col + 1, old_value, new_pixel_value);
  }
} // mark_blob



long JPLPic::FindSun (float *row_ptr, float *col_ptr)
{
  long retval = NO_ERR;

  // Map the input image down into an 8 bit image
  JPLPic *image = MakeScaled8BitImage();

  long hist[256];
  long r, c, sum = 0;
  unsigned char *ptr;
  unsigned char threshold, max_intense = 0;

  // Compute a histogram of image intensities

  for (r = 0; r < 256; r++)
    hist[r] = 0;

  for (r = 0; r < rows; r++) {
    ptr = (unsigned char *) image->GetPixelAddress (r, 0);
    if (ptr) {
      for (c = 0; c < cols; c++, ptr++)
	hist[*ptr]++;
    }
  }

  // find the brightest 5% of the image

  for (r = 255; r >= 0; r--) {
    if (max_intense == 0 && hist[r] > 0)
      max_intense = r;
    sum += hist[r];
    if (sum > rows * cols / 20)
      break;
  }

#ifndef MAX
#define MAX(a,b) (((a) < (b)) ? (b) : (a))
#endif

  // Threshold off the larger of:  The brightest 5% of the image, or:
  // 90% of the brightest pixel.  So if the sun is really-really bright
  // (yet occupies very few pixels), it'll still find it.  If you knew
  // the Sun was the brightest thing, 90% of the max would be ok.  But
  // since you can get into funky cases, the "brightest 5%" is a useful
  // fallback.

  threshold = MAX(r, 9 * max_intense / 10);

  // Check for uniform image
  if (threshold == 0) {
    DBG (("FindSun: Uniform Image!\n"));
    return PARAM_ERR;
  }


  // threshold the image
  for (r = 0; r < rows; r++) {
    ptr = (unsigned char *) image->GetPixelAddress (r, 0);
    if (ptr) {
      for (c = 0; c < cols; c++, ptr++)
	*ptr = (*ptr < threshold) ? 0 : 127;
    }
  }

  image->Write ("sun_pre.pic");

  // Find a pixel inside the largest blob
  
  long row, col, size;

  if (image->BlobFiltering (2000, 1, NULL, NULL, 0, &row, &col, &size)
      != NO_ERR) {
    DBG (("Cannot find max blob!!!\n"));
    retval = INTERNAL_ERR;
  } else if (size > 15 * row * col / 100) {
    // More than 15% of the image is above the threshold; that must
    // mean we've saturated some pixels
    DBG (("Image is saturated!!!\n"));
    retval = INTERNAL_ERR;
  } else {
    double rsum = 0.0, csum = 0.0, count = 0.0;
    ptr = image->GetPixelAddress (row, col);

    // Mark the largest blob with a special value

#ifdef DEBUG
    DBG(("Found Sun blob starting point at %ld,%ld (intense %d)\n", row, col, *ptr));
#endif

    mark_blob (image, row, col, *ptr, 255);

    image->Write ("sun_thresh.pic");

    // Compute the centroid of the largest blob

    for (r = 0; r < rows; r++) {
      ptr = image->GetPixelAddress (r, 0);

      for (c = 0; c < cols; c++, ptr++)
	if (*ptr == 255) {
	  rsum += r;
	  csum += c;
	  count += 1;
	}
    }
#ifdef DEBUG
    DBG(("Sun Row = %g / %g = %g, Col = %g / %g = %g\n",
	 rsum, count, (EQ(count, 0.0) ? 0.0 : (rsum/count)),
	 csum, count, (EQ(count, 0.0) ? 0.0 : (csum/count))));
#endif

    if (GT(count, 0.0)) {
      if (row_ptr)
	*row_ptr = rsum / count;
      if (col_ptr)
	*col_ptr = csum / count;
    } else
      retval = INTERNAL_ERR;
  }
  DELETE (mm, image);

  return retval;
} // JPLPic::FindSun



#endif /* ! OMIT_IMAGE_OPERATORS */

