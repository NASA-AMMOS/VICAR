#include <math.h>
#include "JPLStereo.h"
#include <string.h>	// for memset

/* blob filtering for 16-bit image */
long
JPLStereo::BlobFiltering (short *pixel, long rows, long cols, long srcRB,
			  long maxNumRegions,long minRegSize, short dh,
			  short dv, long *numReg, long *numSize)
{
  long *equiv, *regsize;

  // Allocate memory
  long *same = GetSameBuffer (3*(maxNumRegions+1));
  if (same == NULL) {
    fprintf(stderr, "Cannot allocate temp memory for   same\n");
    return MEM_ERR;
  }
  int *region = GetRegionBuffer (rows * cols);
  if (region == NULL) {
    fprintf(stderr, "Cannot allocate temp memory for   region\n");
    return MEM_ERR;
  }
#ifdef BLOB_DEBUG
printf ( "minRegSize=%ld\n", minRegSize);
#endif
  equiv = same + maxNumRegions + 1;
  regsize = equiv + maxNumRegions + 1;

  int i, j, ix, iy, reg_id, i_u, x_u, x_l, reg_u, reg_l;
  int last_i = 0, last_j = 0;
  int size, neg_dv, neg_dh, equiv_j, img_c, diff = 0, nr, ns;
  short *img, *lastRow;
  int *reg;
  AnythingT subDisp;

  /* Initialize for statistics */
  nr = 0;
  ns = 0;

  /* Label the different regions which are above the intensity threshold */
  reg_id = 1;
  same[0] = same[1] = 0;
  regsize[0] = 0;
  regsize[1] = 1;
  i_u = -cols;
  neg_dh = -dh;
  neg_dv = -dv;
    
  reg = region;
  subDisp.h = pixel;
  for (iy=0; iy<rows; iy++, subDisp.uc += srcRB) {
    for (ix=0, img = subDisp.h,
	 lastRow = (subDisp.h - (srcRB / sizeof (short)));
	 ix<cols; ix++,img++, lastRow++, reg++) {

      /* Initialize */
      reg[0] = 0;
      
      /* Note the membership values */
      if ((img_c = img[0]) == (short) NO_S2_DISP)
	continue;
      nr++;
      x_l = ((ix > 0) && (reg[-1] > 0) &&
	     (diff = img_c - img[-1]) < dh) && (diff > neg_dh);
      x_u = ((iy > 0) && (reg[i_u] > 0) &&
	     (diff = img_c - (*lastRow)) < dv) && (diff > neg_dv);

      /* Connect to any neighbors */
      if (x_u && !x_l) {
	reg_u = reg[i_u];
	reg[0] = reg_u;
	regsize[reg_u]++;
      }
      else if (x_l && !x_u) {
	reg_l = reg[-1];
	reg[0] = reg_l;
	regsize[reg_l]++;
      }
      else if (x_l && x_u) {
	reg_l = reg[-1];
	reg_u = reg[i_u];
	reg[0] = reg_l;
	regsize[reg_l]++;
	if (reg_l != reg_u) {	/* equivalence of 2 regions: */
	  
	  /* See if the 2 regions' lists have elements in common */
	  /*...
	    i = reg_l;
	    while (i != 0) {
	    j = reg_u;
	    while ((j != 0) && (j != i))
	    j = same[j];
	    if (j == i)
	    break;
	    last_i = i;
	    i = same[i];
	    }
	    ...*/
	  i = reg_l;
	  while (i != 0) {
	    last_i = i;
	    i = same[i];
	  }
	  j = reg_u;
	  while (j != 0) {
	    last_j = j;
	    j = same[j];
	  }

	  /* If they don't already, then equate the regions */
	  /*...
	    if (i == 0)
	    same[last_i] = reg_u;
	    ...*/
	  if (last_i > last_j)
	    same[last_i] = last_j;
	  else if (last_j > last_i)
	    same[last_j] = last_i;
	}
      }

      /* Start a new region */
      else /*if (!x_l && !x_u)*/ {
	reg[0] = reg_id;
	reg_id++;
	if (reg_id > maxNumRegions) {
	  fprintf(stderr, "region_find(): too many regions");
	  fprintf(stderr, ", >%ld, at row %d, column %d\n",
		  maxNumRegions, iy, ix);
	  //		    if (same) DELETEV (mm, same);
	  // if (region) DELETEV (mm, region);
	  return MEM_ERR;
	}
	same[reg_id] = 0;
	regsize[reg_id] = 1;
      }
    }
    
  }

  /* Simplify (flatten) the equivalance lists */
  for (j=0; j<reg_id; j++)
    equiv[j] = 0;
  for (j=0; j<reg_id; j++) {
    i = j;
    last_i = 0;
    size = 0;
    while (i != 0) {
      size += regsize[i];
      regsize[i] = 0;
      last_i = i;
      i = same[i];
    }
    equiv[j] = last_i;
    regsize[last_i] = size;
  }

  /* Compress the region IDs to smallest numbers possible */
  for (j=0; j<reg_id; j++)
    same[j] = 0;
  i = 0;
  for (j=1; j<reg_id; j++) {
    equiv_j = equiv[j];
    if (!same[equiv_j]) {
      same[equiv_j] = ++i;
      regsize[i] = regsize[equiv_j];
    }
  }
  for (j=1; j<reg_id; j++)
    equiv[j] = same[equiv[j]];

#ifdef BLOB_DEBUG
printf ( "minRegSize = %ld\n", minRegSize);
double rs_cnt = 0.0l, reg_cnt = 0.0l;
for (int ind = 0; ind < maxNumRegions + 1; ind++)
  rs_cnt += regsize[ind], reg_cnt += region[ind];
printf ( "Average regsize = %g, average region = %g\n",
	 rs_cnt / (maxNumRegions + 1), reg_cnt / (maxNumRegions + 1));
#endif
 
  /* Eliminate small regions */
  subDisp.h = pixel;
  reg = region;
  for (iy=rows; iy--; subDisp.uc += srcRB) {
    for (ix=cols, img = subDisp.h; ix--; img++,reg++) {
      if ((*reg == 0) || (regsize[equiv[*reg]] < minRegSize)) {
	if (*img != NO_S2_DISP) {
	  stats.f[ACCEPT]--;
	  stats.f[BLOB]++;
	  if (maskPic)
	    maskPic->SetPixel (rows-(iy+1), cols-(ix+1), BLOB, 0);
	}
	*img = (short) NO_S2_DISP;
      } else
	ns++;
      /* *reg = equiv[*reg]; */
    }
  }

  /* Return the statistics */
  *numReg  = nr;
  *numSize = ns;
  
  // We used to de-allocate   same and region   , but no more
  // if (same) DELETEV (mm, same);
  // if (region) DELETEV (mm, region);

  return NO_ERR;
}


long
JPLStereo::BlobFiltering (unsigned char *pixel, long rows, long cols, long srcRB,
			  long maxNumRegions,long minRegSize, unsigned char dh,
			  unsigned char dv, long *numReg, long *numSize)
{
  long *equiv, *regsize;

  // Allocate memory
  long *same = GetSameBuffer (3*(maxNumRegions+1));
  if (same == NULL) {
    fprintf(stderr, "Cannot allocate temp memory for   same\n");
    return MEM_ERR;
  }
  int *region = GetRegionBuffer (rows * cols);
  if (region == NULL) {
    fprintf(stderr, "Cannot allocate temp memory for   region\n");
    // if (same) DELETEV (mm, same);
    return MEM_ERR;
  }
#ifdef BLOB_DEBUG
printf ( "minRegSize=%ld\n", minRegSize);
#endif
  equiv = same + maxNumRegions + 1;
  regsize = equiv + maxNumRegions + 1;

  int i, j, ix, iy, reg_id, i_u, x_u, x_l, reg_u, reg_l;
  int last_i = 0, last_j = 0;
  int size, neg_dv, neg_dh, equiv_j, img_c, diff = 0, nr, ns;
  unsigned char *img, *lastRow;
  int *reg;
  AnythingT subDisp;

  /* Initialize for statistics */
  nr = 0;
  ns = 0;

  /* Label the different regions which are above the intensity threshold */
  reg_id = 1;
  same[0] = same[1] = 0;
  regsize[0] = 0;
  regsize[1] = 1;
  i_u = -cols;
  neg_dh = -dh;
  neg_dv = -dv;
    
  reg = region;
  subDisp.uc = pixel;
  for (iy=0; iy<rows; iy++, subDisp.uc += srcRB) {
    for (ix=0, img = subDisp.uc, lastRow = (subDisp.uc - srcRB); 
	 ix<cols; ix++,img++, lastRow++, reg++) {

      /* Initialize */
      reg[0] = 0;
      
      /* Note the membership values */
      if ((img_c = img[0]) == 0)
	continue;
      nr++;
      x_l = ((ix > 0) && (reg[-1] > 0) &&
	     (diff = img_c - img[-1]) < dh) && (diff > neg_dh);
      x_u = ((iy > 0) && (reg[i_u] > 0) &&
	     (diff = img_c - (*lastRow)) < dv) && (diff > neg_dv);

      /* Connect to any neighbors */
      if (x_u && !x_l) {
	reg_u = reg[i_u];
	reg[0] = reg_u;
	regsize[reg_u]++;
      }
      else if (x_l && !x_u) {
	reg_l = reg[-1];
	reg[0] = reg_l;
	regsize[reg_l]++;
      }
      else if (x_l && x_u) {
	reg_l = reg[-1];
	reg_u = reg[i_u];
	reg[0] = reg_l;
	regsize[reg_l]++;
	if (reg_l != reg_u) {	/* equivalence of 2 regions: */
	  
	  /* See if the 2 regions' lists have elements in common */
	  /*...
	    i = reg_l;
	    while (i != 0) {
	    j = reg_u;
	    while ((j != 0) && (j != i))
	    j = same[j];
	    if (j == i)
	    break;
	    last_i = i;
	    i = same[i];
	    }
	    ...*/
	  i = reg_l;
	  while (i != 0) {
	    last_i = i;
	    i = same[i];
	  }
	  j = reg_u;
	  while (j != 0) {
	    last_j = j;
	    j = same[j];
	  }

	  /* If they don't already, then equate the regions */
	  /*...
	    if (i == 0)
	    same[last_i] = reg_u;
	    ...*/
	  if (last_i > last_j)
	    same[last_i] = last_j;
	  else if (last_j > last_i)
	    same[last_j] = last_i;
	}
      }

      /* Start a new region */
      else /*if (!x_l && !x_u)*/ {
	reg[0] = reg_id;
	reg_id++;
	if (reg_id > maxNumRegions) {
	  fprintf(stderr, "region_find(): too many regions");
	  fprintf(stderr, ", >%ld, at row %d, column %d\n",
		  maxNumRegions, iy, ix);
	  //		    if (same) DELETEV (mm, same);
	  // if (region) DELETEV (mm, region);
	  return MEM_ERR;
	}
	same[reg_id] = 0;
	regsize[reg_id] = 1;
      }
    }
    
  }

  /* Simplify (flatten) the equivalance lists */
  for (j=0; j<reg_id; j++)
    equiv[j] = 0;
  for (j=0; j<reg_id; j++) {
    i = j;
    last_i = 0;
    size = 0;
    while (i != 0) {
      size += regsize[i];
      regsize[i] = 0;
      last_i = i;
      i = same[i];
    }
    equiv[j] = last_i;
    regsize[last_i] = size;
  }

  /* Compress the region IDs to smallest numbers possible */
  for (j=0; j<reg_id; j++)
    same[j] = 0;
  i = 0;
  for (j=1; j<reg_id; j++) {
    equiv_j = equiv[j];
    if (!same[equiv_j]) {
      same[equiv_j] = ++i;
      regsize[i] = regsize[equiv_j];
    }
  }
  for (j=1; j<reg_id; j++)
    equiv[j] = same[equiv[j]];

#ifdef BLOB_DEBUG
printf ( "minRegSize = %ld\n", minRegSize);
double rs_cnt = 0.0l, reg_cnt = 0.0l;
for (int ind = 0; ind < maxNumRegions + 1; ind++)
  rs_cnt += regsize[ind], reg_cnt += region[ind];
printf ( "Average regsize = %g, average region = %g\n",
	 rs_cnt / (maxNumRegions + 1), reg_cnt / (maxNumRegions + 1));
#endif
 
  /* Eliminate small regions */
  subDisp.uc = pixel;
  reg = region;
  for (iy=rows; iy--; subDisp.uc += srcRB) {
    for (ix=cols, img = subDisp.uc; ix--; img++,reg++) {
      if ((*reg == 0) || (regsize[equiv[*reg]] < minRegSize)) {
	if (*img) {
	  stats.f[ACCEPT]--;
	  stats.f[BLOB]++;
	  if (maskPic)
	    maskPic->SetPixel (rows-(iy+1), cols-(ix+1), BLOB, 0);
	}
	*img = 0; //(short) NO_S2_DISP;
      } else
	ns++;
      /* *reg = equiv[*reg]; */
    }
  }

  /* Return the statistics */
  if (numReg) *numReg  = nr;
  if (numSize) *numSize = ns;
  
  // We used to de-allocate   same and region   , but no more
  // if (same) DELETEV (mm, same);
  // if (region) DELETEV (mm, region);

  return NO_ERR;
}




long JPLStereo::RemoveOverhangsFromDisparityImage (JPLPic *disparity_pic,
						   float *world_up)
{
  long input_rows = 0;
  long r, c;
  int rowBytes = 0;
  float x, y, z;
  double cam_fwd[3];
  int window_size = 0;
  float sigma_fraction = 1.0;

  if (disparity_pic == NULL) {
    VDBG(0,("RemoveOverhangsFromDisparityImage: Empty disparity map!"));
    return PARAM_ERR;
  }
  if (CreateRangeParams() != NO_ERR) {
    VDBG(0,("RemoveOverhangsFromDisparityImage:  Failed to cache disp->range matrix"));
    return INIT_ERR;
  }

  if (leftRectCam == NULL) {
    fprintf (stderr, "RemoveOverhangs: Missing left rectified camera model\n");
    return INIT_ERR;
  }

  input_rows = disparity_pic->rows;
  window_size = (int) (GetOverhangWindowFraction() * input_rows);
  sigma_fraction = GetOverhangSigmaFraction();

  // Don't perform the overhead filter if we don't know which way is up

  if (world_up == NULL ||
      (EQ(world_up[0], 0.0) &&
       EQ(world_up[1], 0.0) &&
       EQ(world_up[2], 0.0)))
    window_size = 0;

  if (window_size < 1)
    return INIT_ERR;

  // Maintain a windowed buffer of FWD values within each column of the
  // disparity image.  We use this buffer to filter out range pixels that
  // "overhang" above the ground plane.
  //
  // fwd_col_sum and fwd_col_sum_sq buffer the sum of FWD and sum of squared
  // FWD values respectively.  fwd_count is the number of valid values
  // that have been added to the buffer, and window_index is where the
  // *next* FWD value should go.
  //
  //		    window_size
  //		|_________________|
  //		    ^
  //		    |__ window_index
  //
  // Since this is a ring buffer and we're storing sums, we can keep
  // the bounds checking to a minimum by simply initializing
  // everything to zero.  Thus the windowed sum corresponding to the
  // most recent 1:window_size values is always:
  //
  // 		sum[PREV_INDEX(window_index)] - sum[window_index]

  float *fwd_col_sum = GetFwdColSumBuffer (window_size);
  if (fwd_col_sum == NULL) {
    fprintf (stderr, "Cannot allocate memory for RemoveOverhangs sum\n");
    window_size = 0;
  }
  float *fwd_col_sum_sq = GetFwdColSumSqBuffer (window_size);
  if (fwd_col_sum_sq == NULL) {
    fprintf (stderr, "Cannot allocate memory for RemoveOverhangs sum_sq\n");
    window_size = 0;
  }
  rowBytes = disparity_pic->GetRowBytes();

  if (leftRectCam->GetPointingDirections (cam_fwd, NULL, NULL) != NO_ERR) {
    cam_fwd[0] = leftRectCam->A[0];
    cam_fwd[1] = leftRectCam->A[1];
    cam_fwd[2] = leftRectCam->A[2];
  }

  // Prepare to flag pixels that get classified as overhangs
  // These will get added to the maskPic and the stats counts

  for (c = 0; c < disparity_pic->cols; c++) {
    union { unsigned short *ptr; unsigned char *c; } u;
    int fwd_count = 0;
    int window_index = 0;

    u.c = disparity_pic->GetPixelAddress(input_rows-1,c);
    memset (fwd_col_sum,    0, window_size * sizeof(float));
    memset (fwd_col_sum_sq, 0, window_size * sizeof(float));

    // Have to run through the each column from BOTTOM to TOP, so we can
    // properly filter overhangs by comparing them to what's underneath.

    for (r = input_rows - 1; r >= 0; r--, u.c -= rowBytes) {

      // Check for bad range values filtered out by the correlator
      if (*(u.ptr) != NO_S2_DISP) {

	// Map integer disparity to 3D (x,y,z) point location
	if (Disparity2FloatXYZRange (r, c, &x, &y, &z) == NO_ERR) {
	  int filter_out = 0;
	  float fwd = -1.0;

	  // Compute the component of this range point in the
	  // camera-forward direction.  You might be tempted to make
	  // this forward in the world frame instead, but don't; the
	  // overhang effect is something that happens with respect to
	  // the camera, not the world.

	  fwd = x * cam_fwd[0] + y * cam_fwd[1] + z * cam_fwd[2];

	  // When possible, only worry about points above the horizon
	  float value = world_up ?
	    (x * world_up[0] + y * world_up[1] + z * world_up[2]) : 1.0;

	  if (GT(value, 0.0)) {

#define PREV_INDEX(x) (((x) + window_size - 1) % window_size)
#define NEXT_INDEX(x) (((x) + 1) % window_size)

	    if (fwd_count > 0) {

	      // Filter out overhanging points when enabled

	      float local_mean =
		fwd_col_sum[PREV_INDEX(window_index)];
	      float local_variance =
		fwd_col_sum_sq[PREV_INDEX(window_index)];
	      float last_fwd =
		fwd_col_sum[PREV_INDEX(window_index)];

	      // We do these local_* assignments in two steps to allow a
	      // window size of 1 to work properly

	      if (fwd_count > 1) {
		local_mean -=  fwd_col_sum[window_index];
		local_mean /= fwd_count;
		local_variance -= fwd_col_sum_sq[window_index];
		last_fwd -=
		  fwd_col_sum[PREV_INDEX(PREV_INDEX(window_index))];
	      }
	      local_variance = local_variance / fwd_count - SQ(local_mean);

	      float cutoff = last_fwd -
		(GT(local_variance, 0.0) ?
		 (sqrt (local_variance) * sigma_fraction) : 0.0);
		  
	      filter_out = LT(fwd, cutoff);
	    }
	  }

	  if (filter_out) {
	    *u.ptr = NO_S2_DISP;
	    stats.f[ACCEPT]--;
	    stats.f[OVERHANG]++;
	    if (maskPic)
	      maskPic->SetPixel (r, c, OVERHANG, 0);
	  }

	  // Keep running sums of fwd and fwd^2

	  if (filter_out == 0 && GE(fwd, 0.0)) {
	    fwd_col_sum[window_index] = 0.0;
	    fwd_col_sum[window_index] =
	      fwd_col_sum[PREV_INDEX(window_index)]    + fwd;
	    fwd_col_sum_sq[window_index] = 0.0;
	    fwd_col_sum_sq[window_index] =
	      fwd_col_sum_sq[PREV_INDEX(window_index)] + SQ(fwd);
	    if (fwd_count < window_size)
	      fwd_count++;
	    window_index = NEXT_INDEX(window_index);
	  }

#undef PREV_INDEX
#undef NEXT_INDEX

	}
      }
    }
  }

  return NO_ERR;
} // JPLStereo::RemoveOverhangsFromDisparityImage



void JPLStereo::ClearFilterCounts (void)
{
  int i;

  stats.rows = 0;
  stats.cols = 0;

  for (i = 0; i < REJ_FLAGS; i++)
    stats.f[i] = 0;
} // JPLStereo::ClearFilterCounts


void JPLStereo::UpdateFilterCount (RejFlagT cnt, long offset)
{
  if (cnt >= 0 && cnt < REJ_FLAGS)
    stats.f[cnt] += offset;
} // JPLStereo::UpdateFilterCount



StereoFilterCountT JPLStereo::GetFilterCounts (void)
{
  return stats;
} // JPLStereo::GetFilterCounts

long JPLStereo::ExtractCountsFromMask (JPLPic *mask,
				       StereoFilterCountT *count)
{
  long row, col;
  long bogus = 0;

  if (mask == NULL || count == NULL || mask->GetPixelType() != UC8_PIXEL)
    return PARAM_ERR;

  for (row = 0; row < REJ_FLAGS; row++)
    count->f[row] = 0;

  for (row = 0; row < mask->rows; row++) {
    unsigned char *ptr = mask->GetPixelAddress (row, 0);

    if (ptr)
      for (col = 0; col < mask->cols; col++, ptr++) {
	if (*ptr < REJ_FLAGS)
	  count->f[*ptr]++;
	else
	  bogus++;
      }
  }

  if (bogus > 0)
    return PARAM_ERR;

  return NO_ERR;
} // JPLStereo::ExtractCountsFromMask

