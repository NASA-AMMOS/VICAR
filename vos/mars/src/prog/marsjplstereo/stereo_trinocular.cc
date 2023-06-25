#include <math.h>
#include "JPLStereo.h"
#include "mat3.h"
#include "cahvor.h"

#ifndef OMIT_IMAGE_OPERATORS


long
JPLStereo::TrinocularStereo (JPLPic *centerPic, JPLPic *lowerPic,
			     JPLPic *leftPic,
			     JPLCamera *centerCam, JPLCamera *lowerCam,
			     JPLCamera *leftCam, int pyramidLevel)
{
	JPLPic centerRectPicH(mm), centerRectPicV(mm);
	JPLPic leftRectPicH(mm);
	JPLPic lowerRectPicV(mm);
	JPLCamera centerRectCamH(mm), centerRectCamV(mm);
	JPLCamera leftRectCamH(mm), lowerRectCamV(mm);
	double hvec[3], vvec[3], norm, pos[3], ray1[3], ray2[3];
	long i, rows, cols, newRows, newCols;
	double A[3], H[3], V[3], imgPos[2];
	double hfov, vfov, hscale, vscale;
	double newCenter[2], newScale[2], tmp;
#ifdef DEBUG_MAC_TRINOCULAR
	static WindowPtr	win1 = NULL, win2 = NULL, win3 = NULL, win4 = NULL;
#endif
	
	/* Find the two baseline vectors and normalize them */
	norm = 0.0;
	for (i = 0; i < 3; i++) {
		hvec[i] = leftCam->C[i] - centerCam->C[i];
		norm += hvec[i] * hvec[i];
	}
	if (EQ(norm, 0.0)) {
		FatalErr ("Zero norm vector from horizontal baseline\n");
		return INIT_ERR;
	}
	norm = 1.0 / sqrt(norm);
	for (i = 0; i < 3; i++) hvec[i] *= norm;
	// now   hvec   is a unit vector along the horizontal baseline
	norm = 0.0;
	for (i = 0; i < 3; i++) {
		vvec[i] = lowerCam->C[i] - centerCam->C[i];
		norm += vvec[i] * vvec[i];
	}
	if (EQ(norm, 0.0)) {
		FatalErr ("Zero norm vector from vertical baseline\n");
		return INIT_ERR;
	}
	norm = 1.0 / sqrt(norm);
	for (i = 0; i < 3; i++) vvec[i] *= norm;
	// now   vvec   is a unit vector along the vertical baseline

	/* Construct the new rectified camera models */
	rows = centerPic->rows;
	cols = centerPic->cols;
	 /* The new A vector - normal to the horiz and vert baselines */
	cross3 (vvec, hvec, A);
	norm = 0.0;
	for (i = 0; i < 3; i++) norm += A[i] * A[i];
	if (EQ(norm, 0.0)) {
		FatalErr ("Zero norm vector from new A vector\n");
		return INIT_ERR;
	}
	norm = 1.0 / sqrt(norm);

	// Sanity check on magnitude of new pointing direction.  E.g.,
	// it might point the wrong way if left and right cameras are
	// reversed.

	if (dot3(A, centerCam->A) < 0.0) norm = -norm;
	for (i = 0; i < 3; i++)
		A[i] *= norm;
	// now   A   is a unit vector in the new pointing direction

	// Correct for a non-right angle in the horiz/vert baselines
        cross3 (A, vvec, ray1);
        cross3 (hvec, A, vvec);
        for (i = 0; i < 3; i++) hvec[i] = ray1[i];
	// now vvec is normal to A and the horizontal baseline,
	// and hvec is normal to A and the vertical baseline

	// calculate the horizontal and vertical fov -- center[0] is horizontal
	// center (cols), center[1] is vertical center (rows)
	imgPos[0] = 0.0;
	imgPos[1] = centerCam->center[1];
	// ray1 points at the west border of the image
	cmod_cahvor_2d_to_3d (imgPos, centerCam->C, centerCam->A,
				  centerCam->H, centerCam->V, centerCam->O,
				  centerCam->R, false, pos, ray1, NULL);
	imgPos[0] = cols - 1;
	imgPos[1] = centerCam->center[1];
	// ray2 points at the east border of the image
	cmod_cahvor_2d_to_3d (imgPos, centerCam->C, centerCam->A,
				  centerCam->H, centerCam->V, centerCam->O,
				  centerCam->R, false, pos, ray2, NULL);
	// hfov is the horizontal field of view of the center camera
	hfov = acos (dot3 (ray1, ray2));
	for (i = 0; i < 3; i++) ray1[i] -= ray2[i];
	// now ray1 points in the horizontal direction of the original
	// center image; but it's the difference of two unit vectors, so
	// I'm not sure what its magnitude represents --mwm
	hscale = mag3(ray1) / fabs(dot3(ray1, hvec));
	// Now hscale is the ratio of how much larger the horizontal part
	// of the image is compared to its projection on the new horizontal
	// axis
	imgPos[0] = centerCam->center[0];
	imgPos[1] = 0.0;
	// ray1 points at the north border of the image
	cmod_cahvor_2d_to_3d (imgPos, centerCam->C, centerCam->A,
		centerCam->H, centerCam->V, centerCam->O, centerCam->R,
		false, pos, ray1, NULL);
	imgPos[0] = centerCam->center[0];
	imgPos[1] = rows - 1;
	// ray2 points at the south border of the image
	cmod_cahvor_2d_to_3d (imgPos, centerCam->C, centerCam->A,
		centerCam->H, centerCam->V, centerCam->O, centerCam->R,
		false, pos, ray2, NULL);
	// vfov is the vertical field of view of the center camera
	vfov = acos (dot3 (ray1, ray2));
	for (i = 0; i < 3; i++) ray1[i] -= ray2[i];
	// now ray1 points in the vertical direction of the original
	// center image; but it's the difference of two unit vectors, so
	// I'm not sure what its magnitude represents  --mwm
	vscale = mag3 (ray1) / fabs (dot3 (ray1, vvec));
	// Now vscale is the ratio of how much larger the vertical part
	// of the image is compared to its projection on the new vertical
	// axis

	// Compute new rows, cols, centers and scales
	add3 (A, centerCam->C, pos);
	// now   pos   is a point along the optical axis of the new
	// camera model
	cmod_cahvor_3d_to_2d (pos, centerCam->C, centerCam->A,
		centerCam->H, centerCam->V, centerCam->O, centerCam->R,
		false, &tmp, imgPos, NULL);
	// now   imgPos   is the image coordinate in the center image that
	// will be the center of the new camera model
	imgPos[0] -= (float) (cols - 1) * 0.5;
	imgPos[1] -= (float) (rows - 1) * 0.5;
	// now   imgPos   is the image coordinate in the center image that
	// will be the upper left origin, assuming the number of rows and
	// columns from the original image
	newRows = (long) ((float) rows * vscale + 0.5);
	newCols = (long) ((float) cols * hscale + 0.5);
	// newRows and newCols   have been increased by the skew between
	// the image plane and horizontal and vertical baselines.
	// Question:  why increased, shouldn't it have shrunk? --mwm
	newCenter[0] = ((float) newCols - 1) * 0.5;
	newCenter[1] = ((float) newRows - 1) * 0.5;
	// newCenter is the center of the new camera model's center image
	newScale[0] = newCenter[0] / tan (hfov * 0.5);
	newScale[1] = newCenter[1] / tan (vfov * 0.5);
	// newScale[0] is the new horizontal FOV in pixels
	// newScale[1] is the new vertical FOV in pixels
	newCenter[0] += imgPos[0] * hscale;
	newCenter[1] += imgPos[1] * vscale;
	// newCenter now includes an offset to bring the "actual" origin
	// into the upper left corner.

	/* Construct H and V vectors; why is H negated? --mwm */
	for (i = 0; i < 3; i++) {
		H[i] = -newScale[0] * hvec[i] + newCenter[0] * A[i];
		V[i] =  newScale[1] * vvec[i] + newCenter[1] * A[i];
	}
	/* Construct the new camera models */
	/* left/right pair*/
	centerRectCamH.InitJPLCamera (CAHV_MODEL, &newRows, &newCols,
				      centerCam->C, 
		A, H, V, NULL, NULL, newCenter, newScale, NULL, 
		NULL, NULL);
	leftRectCamH.InitJPLCamera (CAHV_MODEL, &newRows, &newCols,
				    leftCam->C, 
		A, H, V, NULL, NULL, newCenter, newScale, NULL, 
		NULL, NULL);
	// up/down pair
	tmp = newCenter[0]; newCenter[0] = newCenter[1]; newCenter[1] = tmp;
	tmp = newScale[0];   newScale[0] = newScale[1];   newScale[1] = tmp;

	centerRectCamV.InitJPLCamera (CAHV_MODEL, &newRows, &newCols,
				      centerCam->C, 
		A, V, H, NULL, NULL, newCenter, newScale, NULL, 
		NULL, NULL);
	lowerRectCamV.InitJPLCamera (CAHV_MODEL, &newRows, &newCols,
				     lowerCam->C, 
		A, V, H, NULL, NULL, newCenter, newScale, NULL, 
		NULL, NULL);
		
	/* Allocate rectified images */
	centerRectPicH.Init (newRows, newCols, UC8_PIXEL);
	centerRectPicV.Init (newCols, newRows, UC8_PIXEL);
	leftRectPicH.Init (newRows, newCols, UC8_PIXEL);
	lowerRectPicV.Init (newCols, newRows, UC8_PIXEL);
	
	/* Rectify Images */
	centerCam->RectifyImage(centerPic, &centerRectPicH,
			&centerRectCamH, 10, 0);
#ifdef DEBUG_MAC_TRINOCULAR
	centerRectPicH.ShowOnScreen(&win1, "\pCenter H",
					  true, false, 0, 0);
#endif
	centerCam->RectifyImage(centerPic, &centerRectPicV,
				&centerRectCamV, 10, 0);
#ifdef DEBUG_MAC_TRINOCULAR
	centerRectPicV.ShowOnScreen(&win2, "\pCenter V",
					  true, false, 0, 0);
#endif
	leftCam->RectifyImage(leftPic, &leftRectPicH,
			      &leftRectCamH, 10, 0);
#ifdef DEBUG_MAC_TRINOCULAR
	leftRectPicH.ShowOnScreen(&win3, "\pleftRectPicH",
					true, false, 0, 0);
#endif
	lowerCam->RectifyImage(lowerPic, &lowerRectPicV,
			&lowerRectCamV, 10, 0);
#ifdef DEBUG_MAC_TRINOCULAR
	lowerRectPicV.ShowOnScreen(&win4, "\plowerRectPicV",
		true, false, 0, 0);
#endif
	leftRectPicH.Write ("horiz.l-rect.pic");
	centerRectPicH.Write ("horiz.r-rect.pic");
	centerRectPicV.Write ("vert.l-rect.pic");
	lowerRectPicV.Write ("vert.r-rect.pic");
	leftRectCamH.WriteCameraModel ("horiz.l-rect.cahv");
	centerRectCamH.WriteCameraModel ("horiz.r-rect.cahv");
	centerRectCamV.WriteCameraModel ("vert.l-rect.cahv");
	lowerRectCamV.WriteCameraModel ("vert.r-rect.cahv");

	/* Run stereo on these two pairs and merge results */

	int save_min_disp = minDisp, save_max_disp = maxDisp;

	MatchStereo (&centerRectPicV, &lowerRectPicV, &centerRectCamV, 
		     &lowerRectCamV, pyramidLevel, minDisp, maxDisp, 0);

	WriteSubpixelDisparity ("result.v.pic");

	GenerateRangeImage();

	JPLPic *vertRangeMap = rangePic->Clone();
	rangePic->Copy (vertRangeMap);
	WriteRangeMap("result.v.ran");

	JPLPic *vertDispMap = subDispPic->Clone();
	subDispPic->Copy (vertDispMap);

	
	if (ResizeImageBuffers (leftRectPicH.rows, leftRectPicH.cols,
				pyramidLevel) != NO_ERR)
	  printf (
		   "WARNING!!  Failed to resize static image buffers\n");

	rightHandDisparity = 1;
	MatchStereo (&leftRectPicH, &centerRectPicH, &leftRectCamH, 
		     &centerRectCamH, pyramidLevel, save_min_disp,
		     save_max_disp, 0);
	WriteSubpixelDisparity ("result.h.pic");

	GenerateRangeImage();
	WriteRangeMap("result.h.ran");


#ifdef MERGING_RANGE_MAPS
	if (MergeRangeMaps (rangePic, vertRangeMap, mergeSqThreshold)
	    != NO_ERR)
	  printf (
		   "ERROR!! Unable to merge horizontal and vertical maps!\n");
#else
	int ind;
	float horiz_baseline = 0.0, vert_baseline = 0.0;

	for (ind = 3; ind--; ) {
	  horiz_baseline += SQ(leftRectCamH.C[ind] - centerRectCamH.C[ind]);
	  vert_baseline  += SQ(centerRectCamV.C[ind] - lowerRectCamV.C[ind]);
	}
	if (EQ(vert_baseline, 0.0) || EQ(horiz_baseline, 0.0)) {
	  printf ( "WARNING!! Horiz baseline %g or Vert baseline %g "
		   "close to zero!\n", horiz_baseline, vert_baseline);
	  horiz_baseline = 1.0;
	  vert_baseline = 1.0;
	} else {
	  horiz_baseline = sqrt(horiz_baseline);
	  vert_baseline = sqrt(vert_baseline);
	}

	if (MergeDisparityMaps (subDispPic, vertDispMap,
				vert_baseline / horiz_baseline,
				mergePixelThreshold)
	    != NO_ERR)
	  printf ( "ERROR!! Unable to merge disparity maps!\n");
	else {
	  long nr, ns;

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
	}

	GenerateRangeImage();
#endif
	WriteRangeMap("result.m.ran");
	WriteSubpixelDisparity ("result.m.pic");
	rightHandDisparity = 0;

	return NO_ERR;
}


float float_triple_sq_diff (float *f1, float *f2) {
  return     (f1[2]-f2[2])*(f1[2]-f2[2]);
  //  return (f1[0]-f2[0])*(f1[0]-f2[0]) + (f1[1]-f2[1])*(f1[1]-f2[1]) +
  //    (f1[2]-f2[2])*(f1[2]-f2[2]);
}


long
JPLStereo::MergeDisparityMaps (JPLPic *dstPic, JPLPic *srcPic,
			       float disp_ratio, float pixel_threshold)
{
  long row, col, dst_RB;
  long sgood = 0l, dgood = 0l, mgood = 0l;
  AnythingT dst_pixels;
  float dsum = 0.0, dsum_sq = 0.0;
  long dcount = 0l;

  if (dstPic == NULL || srcPic == NULL ||
      (dstPic->rows != srcPic->cols) ||
      (dstPic->cols != srcPic->rows) ||
      (dstPic->GetPixelType() != srcPic->GetPixelType())) {
    return (TYPE_MATCH_ERR);
  }
  if (disp_ratio < 0.5 || disp_ratio > 2.0)
    printf ( "WARNING!!! Disp_ratio %g should be approx [0.5:2.0]\n",
	     disp_ratio);

  dst_pixels.uc = dstPic -> GetPixelAddress();
  dst_RB = dstPic -> GetRowBytes ();

  for (row = 0; row < dstPic->rows; row++, dst_pixels.uc += dst_RB) {
    short *dst_ptr = dst_pixels.h;

    for (col = dstPic->cols; col-- > 0;) {
      short *src_ptr = srcPic -> GetShortPixelAddress(col, row);

      int src_empty = src_ptr[0] == NO_S2_DISP;
      int dst_empty = dst_ptr[col] == NO_S2_DISP;

      if (!src_empty) sgood++;
      if (!dst_empty) dgood++;

      float diff = ABS(((float) (src_ptr[0] >> DISP_SCALE_SHIFT)) -
		       disp_ratio *
		       ((float) (dst_ptr[col] >> DISP_SCALE_SHIFT)));
#if ULTRA_CONSERVATIVE
      if (src_empty || dst_empty || diff > pixel_threshold)
	dst_ptr[col] = NO_S2_DISP;
#else
      if (dst_empty)
	dst_ptr[col] = src_empty ? NO_S2_DISP : src_ptr[0];
      else if (!src_empty)
	dst_ptr[col] = (diff > pixel_threshold) ? NO_S2_DISP
	  : (dst_ptr[col] + src_ptr[0]) / 2;
#endif
	
      if (!src_empty && !dst_empty) {
	dcount++;
	dsum += diff;
	dsum_sq += diff * diff;
#ifdef USE_TO_MAKE_DIFFERENCE_IMAGE_ONLY
	dst_ptr[col] = (short int) diff;
	dst_ptr[col] <<= DISP_SCALE_SHIFT;
#endif
      }
      if (dst_ptr[col] != NO_S2_DISP) mgood++;
    }
  }
  printf ( "Trinocular merge stats: %ld total, %ld vert, %ld horiz, "
	   "%ld both\n", dstPic->rows * dstPic->cols, sgood, dgood, mgood);
  float mean_diff = dsum / (float) dcount;
  float diff_var = dsum_sq / (float) dcount - mean_diff * mean_diff;
  if (diff_var < 0.0 || EQ(diff_var, 0.0)) {
    printf ( "ERROR!!  Negative variance %g?\n", diff_var);
    diff_var = 1.0;
  }
  printf ( "Of the %ld valid differences:  %g +/- %g\n",
	   dcount, mean_diff, diff_var);
  return NO_ERR;

}


long
JPLStereo::MergeRangeMaps (JPLPic *dstPic, JPLPic *srcPic, float max_sq_sep)
{
  long row, col, dst_RB;
  AnythingT dst_pixels;

  if (dstPic == NULL || srcPic == NULL ||
      (dstPic->rows != srcPic->cols) ||
      (dstPic->cols != srcPic->rows) ||
      (dstPic->GetPixelType() != srcPic->GetPixelType())) {
    return (TYPE_MATCH_ERR);
  }
  dst_pixels.uc = dstPic -> GetPixelAddress();
  dst_RB = dstPic -> GetRowBytes ();

  for (row = 0; row < dstPic->rows; row++, dst_pixels.uc += dst_RB) {
    float *dst_ptr = dst_pixels.f;

    for (col = dstPic->cols; col-- > 0;) {
      float *src_ptr = srcPic -> GetFloatPixelAddress(col, row);

      int src_empty = EQ(src_ptr[0],NO_RANGE);
      int dst_empty = EQ(dst_ptr[3*col],NO_RANGE);
      if (src_empty || dst_empty || 
	  float_triple_sq_diff (src_ptr, dst_ptr+3*col) > max_sq_sep)
	dst_ptr[3*col] = dst_ptr[3*col+1] = dst_ptr[3*col+2] = NO_RANGE;
    }
  }
  return NO_ERR;
}




long
JPLStereo::SurfaceOrientation (unsigned char lsize, 
			       JPLPic *vx, JPLPic *vy, JPLPic *vz)
{
  unsigned char *xSrc, *ySrc, *zSrc;
  AnythingT rangeSrc;
  float *r, *range;
  unsigned char *nx, *ny, *nz;
  long xRB, yRB, zRB, rangeRB;
  double sx, sy, sz, sxx, sxy, sxz, syy, syz, szz;
  int row, col, half, i, j, rows, cols;

  if (rangePic == NULL) {
    FatalErr ("Range image must be initialized before surface orientation\n");
    return INIT_ERR;
  }

  if (lsize < 3) lsize = 3;
  half = lsize >> 1;
  lsize = 2 * half + 1;
  printf("Window size for local plane fitting: %d\n", lsize);

  xSrc = ySrc = zSrc = rangeSrc.uc = NULL;
  xRB = yRB = zRB = rangeRB = 0;
  rows = leftRectPic->rows;
  cols = leftRectPic->cols;

  if (vx) {
    vx->ClearImage ();
    xSrc = vx->GetPixelAddress();
    xRB = vx->GetRowBytes ();
  }
  if (vy) {
    vy->ClearImage ();
    ySrc = vy->GetPixelAddress();
    yRB = vy->GetRowBytes ();
  }
  if (vz) {
    vz->ClearImage ();
    zSrc = vz->GetPixelAddress();
    zRB = vz->GetRowBytes ();
  }

  rangeSrc.uc = rangePic->GetPixelAddress();
  rangeRB = rangePic->GetRowBytes ();
  //  rangeSrc += half * rangeRB + half * 3 * sizeof (float);
  xSrc += half * xRB + half;
  ySrc += half * yRB + half;
  zSrc += half * zRB + half;
  for (row = half; row < (rows - half); 
       row ++, rangeSrc.uc += rangeRB, xSrc += xRB, ySrc += yRB, zSrc += zRB) {
    range = rangeSrc.f;
    nx = xSrc;
    ny = ySrc;
    nz = zSrc;
    for (col = half; col < (cols - half); 
	 col++, range += 3, nx++, ny++, nz++) {

      sx = sy = sz = sxx = sxy = sxz = syy = syz = szz = 0.0;

      for (i = 0; i < lsize; i++) {
	r = range + (rangeRB / sizeof (float));
	for (j = 0; j < lsize; j++, r+=3) {
	  sx += r[0];
	  sy += r[1];
	  sz += r[2];
	  sxx += r[0] * r[0];
	  sxy += r[0] * r[1];
	  sxz += r[0] * r[2];
	  syy += r[1] * r[1];
	  syz += r[1] * r[2];
	  szz += r[2] * r[2];
	}
      }

      /* fit a plane */
    }
  }

  return NO_ERR;
}



#endif /* ! OMIT_IMAGE_OPERATORS */
