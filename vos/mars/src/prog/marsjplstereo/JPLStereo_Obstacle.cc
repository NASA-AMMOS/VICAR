#include <math.h>
#include <stdlib.h>
#include "ErrHandle.h"
#include "JPLStereo.h"
#include "MDTypes.h"
#include "stereo.h"
#include "nav_memory.h"

#ifdef MAC_PLATFORM
	#ifdef __TIMING__
	#include "Timing.h"
	#endif
#endif

/* globals for obstacle detection */
float gMinHeight = 0.09; /* for obs lower than 9cm, drive straight */
float gMinAngle = 30; /* 30 degree for slope threshold */
float gClimbHeight = 0.20; /* the maixmal climbable height is 20cm */
float gClimbAngle = 30;

void 
PrintObstacleHeights ()
{
  printf("Climbable height: %f, Non-climbale height: %f\n",
	 gMinHeight, gClimbHeight);
}

void
SetObstacleHeight (double minHeight, double climbHeight)
{
  gMinHeight = minHeight;
  gClimbHeight = climbHeight;

  PrintObstacleHeights ();
}

long 
JPLStereo::DetectPosObs (float *up_vec, float minObsSize,
			 float slopeThreshold, float maxRange, int numBits)
{
	float M[9], M0[9], A[3];
	unsigned char *obstaclePixel, *obstacle;
	AnythingT dispPixel, dispSrcPixel;
	//unsigned char *unSrcPixel;
	long dispRB, obstacleRB ;
	long row, col, rows, cols, i;
	unsigned short *disp, newDisp;
	//	unsigned char newUncertainty, *uncertainty;
	float nbaseline, center, step[3], x_scale;
	float x0, y_0, z0, x, y, z, xi, yi, zi, xr, yr, zr, sign;
	float range, scale, cosTheta, a2, b2;
	long newRow, newCol;
	unsigned char mask;
	
	mask = (1 << numBits);

	if (leftRectCam == NULL || rightRectCam == NULL) {
		FatalErr ("No Camera info \n");
		return INIT_ERR;
	}
	
	if (subDispPic == NULL) {
		FatalErr ("No disparity or uncertainty info\n");
		return INIT_ERR;
	}
	
	leftRectCam->M3D_2D(M);
	leftRectCam->M2D_3D(M0);
	for (i = 0; i < 3; i++)
		A[i] = leftRectCam->A[i];
	
	dispSrcPixel.uc = dispPixel.uc = subDispPic->GetPixelAddress ();
	dispRB = subDispPic->GetRowBytes ();
	//	unSrcPixel = uncertaintyPixel = uncertaintyMap->GetPixelAddress ();
	//	uncertaintyRB = uncertaintyMap->GetRowBytes ();
	rows = subDispPic->rows;
	cols = subDispPic->cols;
	
	if (obsPic == NULL) {
	  obsPic = new JPLPic(mm);
	  if (obsPic->Init (rows, cols, UC8_PIXEL) != NO_ERR) {
	    FatalErr ("Cannot allocate osbtacle map\n");
	    return MEM_ERR;
	  }
	  obsPic->ClearImage ();
	}

	obstaclePixel = obsPic->GetPixelAddress ();
	obstacleRB = obsPic->GetRowBytes ();
	
	x_scale = 0.5 * (leftRectCam->scale[0] + rightRectCam->scale[0]);
	nbaseline = 0.0;
	for (i = 3; i--; )
		nbaseline += SQ(leftRectCam->C[i] - rightRectCam->C[i]);
	nbaseline = sqrt(nbaseline) * x_scale * ((float) I_DISP_SCALE);
	center = (rightRectCam->center[0] / rightRectCam->scale[0] -
		leftRectCam->center[0] / leftRectCam->scale[0]) * ((float) I_DISP_SCALE);
	//	minValidDisp = nbaseline / maxRange + center;
	
	slopeThreshold = cos(slopeThreshold * 3.141592653589 / 180.0);
	
	for (i = 0; i < 3; i++)
		step[i] = minObsSize * up_vec[i];
	
	for (row = 0; row < rows; row++, dispPixel.uc += dispRB,
	       obstaclePixel += obstacleRB)
	  {
	  disp = dispPixel.uh;
	  obstacle = (unsigned char *) obstaclePixel;
		
	  x0 = M0[1] * row + M0[2];
	  y_0 = M0[4] * row + M0[5];
	  z0 = M0[7] * row + M0[8];
		
	  for (col = 0; col < cols; col++, disp ++, obstacle++) {
		
	    //  *obstacle = 0;
	    
	    if (*disp == NO_S2_DISP) continue;  // no disparity 
	    //			if (*disp <= minValidDisp) continue; // out of range
			
	    range = nbaseline / (((float) *disp) - center);
	    if (range > maxRange) continue;
	    x = x0 + M0[0] * col;
	    y = y_0 + M0[3] * col;
	    z = z0 + M0[6] * col;
			
	    scale = range / (x * A[0] + y * A[1] + z * A[2]);
			
	    // imaginary point
	    x *= scale;
	    y *= scale;
	    z *= scale;
	    xi = x + step[0];
	    yi = y + step[1];
	    zi = z + step[2];
			
	    scale = 1.0 / (M[6] * xi + M[7] * yi + M[8] * zi);
	    newCol = (int) ((M[0] * xi + M[1] * yi + M[2] * zi) * scale + 0.5);
	    newRow = (int) ((M[3] * xi + M[4] * yi + M[5] * zi) * scale + 0.5);
			
	    if (newCol < 0 || newCol >= cols) continue; // out of bound
	    if (newRow < 0 || newRow >= rows) continue; // out of bound
	    newDisp = *(dispSrcPixel.uh +
			(newRow * dispRB / sizeof(unsigned short)) + newCol);
		       
	    /*
	    newUncertainty = *(((unsigned char *) (unSrcPixel + newRow * uncertaintyRB))
			       + newCol);
	    */
	    
	    if (newDisp == NO_S2_DISP/* || newDisp <= minValidDisp*/) continue;
	    //if (newUncertainty > uncertaintyThreshold) continue;
			
	    // real point
	    range = nbaseline / (((float) newDisp) - center);
	    scale = range / (xi * A[0] + yi * A[1] + zi * A[2]);
	    xr = xi * scale;
	    yr = yi * scale;
	    zr = zi * scale;
			
	    // slope estimate
	    b2 = SQ(xr - x) + SQ(yr - y) + SQ(zr - z);
	    a2 = SQ(xr - xi) + SQ(yr - yi) + SQ(zr - zi);
	    cosTheta = (b2 + SQ(minObsSize) - a2) / (2.0 * sqrt(b2) * minObsSize);
	    sign = (xr - xi) * A[0] + (yr - yi) * A[1] + (zr - zi) * A[2];
			
	    if (sign <= 0.0 || cosTheta > slopeThreshold) { // obstacle detected
	      *obstacle |= mask;
	    } else { // no obstacle
	    }
	  }
	}
	
	#ifdef __TIMING__
	{
		float time = TimingStats(ReturnElapsed) * TO_SECS;
		char str[100];
		snprintf(str, 100, "Time for Detecting in one direction: %f seconds\n", time);
		Message (str);
	}
	#endif
	return NO_ERR;
}

long 
JPLStereo::DetectHeightObs (float *up_vec, float minHeight, float maxHeight,
			 float maxRange, int numBits)
{
	float M[9], M0[9], A[3], height;
	AnythingT dispPixel;
	unsigned char *obstaclePixel, *obstacle;
	//unsigned char *unSrcPixel;
	long dispRB, obstacleRB ;
	long row, col, rows, cols, i;
	unsigned short *disp;
	//	unsigned char newUncertainty, *uncertainty;
	float nbaseline, center, x_scale;
	float x0, y_0, z0, x, y, z;
	float range, scale;
	unsigned char mask;
	
	mask = (1 << numBits);

	if (leftRectCam == NULL || rightRectCam == NULL) {
		FatalErr ("No Camera info \n");
		return INIT_ERR;
	}
	
	if (subDispPic == NULL) {
		FatalErr ("No disparity or uncertainty info\n");
		return INIT_ERR;
	}
	
	leftRectCam->M3D_2D(M);
	leftRectCam->M2D_3D(M0);
	for (i = 0; i < 3; i++)
		A[i] = leftRectCam->A[i];
	
	dispPixel.uc = subDispPic->GetPixelAddress ();
	dispRB = subDispPic->GetRowBytes ();
	//	unSrcPixel = uncertaintyPixel = uncertaintyMap->GetPixelAddress ();
	//	uncertaintyRB = uncertaintyMap->GetRowBytes ();
	rows = subDispPic->rows;
	cols = subDispPic->cols;
	
	if (obsPic == NULL) {
	  obsPic = new JPLPic(mm);
	  if (obsPic->Init (rows, cols, UC8_PIXEL) != NO_ERR) {
	    FatalErr ("Cannot allocate osbtacle map\n");
	    return MEM_ERR;
	  }
	  obsPic->ClearImage ();
	}

	obstaclePixel = obsPic->GetPixelAddress ();
	obstacleRB = obsPic->GetRowBytes ();
	
	x_scale = 0.5 * (leftRectCam->scale[0] + rightRectCam->scale[0]);
	nbaseline = 0.0;
	for (i = 3; i--; )
		nbaseline += SQ(leftRectCam->C[i] - rightRectCam->C[i]);
	nbaseline = sqrt(nbaseline) * x_scale * ((float) I_DISP_SCALE);
	center = (rightRectCam->center[0] / rightRectCam->scale[0] -
		leftRectCam->center[0] / leftRectCam->scale[0]) * ((float) I_DISP_SCALE);
	//	minValidDisp = nbaseline / maxRange + center;
	
	for (row = 0; row < rows; row++, dispPixel.uc += dispRB,
	       obstaclePixel += obstacleRB)
	  {
	  disp = dispPixel.uh;
	  obstacle = (unsigned char *) obstaclePixel;
		
	  x0 = M0[1] * row + M0[2];
	  y_0 = M0[4] * row + M0[5];
	  z0 = M0[7] * row + M0[8];
		
	  for (col = 0; col < cols; col++, disp ++, obstacle++) {
		
	    //  *obstacle = 0;
	    
	    if (*disp == NO_S2_DISP) continue;  // no disparity 
	    //			if (*disp <= minValidDisp) continue; // out of range
			
	    range = nbaseline / (((float) *disp) - center);
	    if (range > maxRange) continue;
	    x = x0 + M0[0] * col;
	    y = y_0 + M0[3] * col;
	    z = z0 + M0[6] * col;
			
	    scale = range / (x * A[0] + y * A[1] + z * A[2]);
			
	    // imaginary point
	    x *= scale;
	    y *= scale;
	    z *= scale;

	    height = (x + leftRectCam->C[0]) * up_vec[0] + 
	      (y + leftRectCam->C[1]) * up_vec[1] + 
	      (z + leftRectCam->C[2]) * up_vec[2];
			
	    if (height < minHeight || height > maxHeight) { // obstacle detected
	      *obstacle |= mask;
	    } else { // no obstacle
	    }
	  }
	}
	
	#ifdef __TIMING__
	{
		float time = TimingStats(ReturnElapsed) * TO_SECS;
		char str[100];
		snprintf(str, 100, "Time for Detecting in one direction: %f seconds\n", time);
		Message (str);
	}
	#endif
	return NO_ERR;
}

/* Todd Litwin's st_negobs5.c: variation: using disparity map */

long
JPLStereo::DetectNegObs (float gapSize, float ground[3], float up[3],
			 float forward[3],
			 float minSlope, float maxRange)
{
  float M[9], M0[9];
  unsigned char *obstaclePixel, *obs, *lobs;	
  AnythingT dispPixel, dispSrcPixel;
  long dispRB, obstacleRB;
  long row, col, rows, cols, i, j, n;
  unsigned short *disp;
  float x_scale, nbaseline, center, sum_rh, sum_r2;
  float x0, y_0, z0, x1, y_1, z1, x2, y2, z2, range, x, y, z;
  float d0, d1, d2, dgr, dgh, slope, slopeg, dr, dh, dr0;
	
  leftRectCam->M3D_2D(M);
  leftRectCam->M2D_3D(M0);
	
  dispSrcPixel.uc = dispPixel.uc = subDispPic->GetPixelAddress ();
  dispRB = subDispPic->GetRowBytes ();
  rows = subDispPic->rows;
  cols = subDispPic->cols;
	
  if (obsPic == NULL) {
    obsPic = new JPLPic(mm);
    if (obsPic->Init (rows, cols, UC8_PIXEL) != NO_ERR) {
      FatalErr ("Cannot allocate osbtacle map\n");
      return MEM_ERR;
    }
    obsPic->ClearImage ();
  }
  obstaclePixel = obsPic->GetPixelAddress ();
  obstacleRB = obsPic->GetRowBytes ();
	
  obstaclePixel += (rows - 1) * obstacleRB;
  dispPixel.uc += (rows - 1) * dispRB;
	
  x_scale = 0.5 * (leftRectCam->scale[0] + rightRectCam->scale[0]);
  nbaseline = 0.0;
  for (i = 3; i--; )
    nbaseline += SQ(leftRectCam->C[i] - rightRectCam->C[i]);
  nbaseline = sqrt(nbaseline) * x_scale * ((float) I_DISP_SCALE);
  center = (rightRectCam->center[0] / rightRectCam->scale[0] -
	    leftRectCam->center[0] / leftRectCam->scale[0]) 
    * ((float) I_DISP_SCALE);
  minSlope = minSlope * 3.1415926535 / 180.0;
		
  for (col = 0 ; col < cols; col++, obstaclePixel += 1, dispPixel.uc += 2) {
		
    sum_rh = 0;
    sum_r2 = 0;
		
    obs = obstaclePixel;
    dispSrcPixel.uc = dispPixel.uc;
		
    x0 = M0[0] * col + M0[2];
    y_0 = M0[3] * col + M0[5];
    z0 = M0[6] * col + M0[7];
		
    for (row = rows; row--; dispSrcPixel.uc -= dispRB, obs -= obstacleRB) {
		
      disp = dispSrcPixel.uh;
      *obs = 0;
			
      if ((*disp) == NO_S2_DISP) continue;
			
      range = nbaseline / (((float) * disp) - center);			
      x = (x0 + M0[1] * row) * range;
      y = (y_0 + M0[4] * row) * range;
      z = (z0 + M0[7] * row) * range;
			
      d0 = x - ground[0];
      d1 = y - ground[1];
      d2 = z - ground[2];
			
      dgr = d0 * forward[0] + d1 * forward[1] + d2 * forward[2];
      dgh = d0 * up[0] + d1 * up[1] + d2 * up[2];
			
      sum_rh += dgr * dgh;
      sum_r2 += dgr * dgr;
			
      if (dgr > maxRange) continue;
			
      slopeg = dgh / dgr;
      slope = sum_rh / sum_r2;
      if (slope > slopeg) 
	slope = slopeg;
				
      n = 0;
      lobs = obs;
      x2 = x;
      y2 = y;
      z2 = z;
      for (i = row; i--; ) {
	
        disp = (dispSrcPixel.uh - ((n+1) * dispRB / sizeof(unsigned short)));
	lobs -= obstacleRB;
	*lobs = 0;
	if (*disp == NO_S2_DISP) continue;
				
	range = nbaseline / (((float) * disp) - center);	
					
	x1 = (x0 + M0[1] * i) * range;
	y_1 = (y_0 + M0[4] * i) * range;
	z1 = (z0 + M0[7] * i) * range;
			
	d0 = x1 - x;
	d1 = y_1 - y;
	d2 = z1 - z;
	dr = d0 * forward[0] + d1 * forward[1] + d2 * forward[2];
	dh = d0 * up[0] + d1 * up[1] + d2 * up[2];
				
	if (dr <= 0) break;
	if ((slope - dh / dr) < minSlope) break;
	n++;
				
	x2 = x1;
	y2 = y_1;
	z2 = z1;
      }
			
      if (i < (row - 1)) { /* negative obstacle potential */
	d0 = x2 - x;
	d1 = y2 - y;	
	d2 = z2 - z;
	dr0 = (d0 * forward[0]) + (d1 * forward[1])
	  + (d2 * forward[2]);
	if (dr0 >= gapSize) { /* neg obstacle detected */
	  for (j = (row - i - 1); j--;  obs -= obstacleRB) {
	    *obs = 255;
	  }
	}
           	 	
	dispSrcPixel.uc -= (row - i - 1) * dispRB;
	row = i + 1;
      }
    }
  }
	
  return NO_ERR;
}

long
JPLStereo::WriteOverlayImage (FILE *fp)
{
  static JPLPic cImg;
  int rows, cols, row, col;
  unsigned char *src1, *src2, *dst;
  unsigned char *s1, *s2, *d;
  long src1RB, src2RB, dstRB;

  rows = obsPic->rows;
  cols = obsPic->cols;
  
  if (cImg.GetPixelAddress() == NULL) {
    if (cImg.Init (rows, cols, ARGB32_PIXEL) != NO_ERR) {
      FatalErr ("Cannot allocate color overlay image\n");
      return MEM_ERR;
    }
  }

  if (obsPic == NULL || leftRectPic == NULL) {
    FatalErr ("Grayscale Image & obstacle image not initialized\n");
    return INIT_ERR;
  } 

  if (cImg.rows != obsPic->rows || cImg.cols != obsPic->cols ||
      cImg.rows != leftRectPic->rows || cImg.cols != leftRectPic->cols) {
    FatalErr ("Image size inconsistent in Overlay\n");
    return INIT_ERR;
  }

  src1 = leftRectPic->GetPixelAddress ();
  src1RB = leftRectPic->GetRowBytes();
  src2 = obsPic->GetPixelAddress ();
  src2RB = obsPic->GetRowBytes();
  dst = cImg.GetPixelAddress ();
  dstRB = cImg.GetRowBytes();
  for (row = 0; row < rows; row ++, src1 += src1RB, src2 += src2RB, dst += dstRB) {
    s1 = src1;
    s2 = src2;
    d = dst;

    for (col = 0; col < cols; col++, s1++, s2++, d += 4) {
      d[0] = *s1;
      d[1] = d[2] = d[3] = *s1;
      if (*s2 != 0) {
	//	printf("obs = %d ", *s2);
	d[1] = 255; //*s2;
      }
    }
  }

  return (cImg.WritePPM (fp));
}

/* This function generates a obstacle map with labels
   of negotiable and non-negotiable obstacles */

long
JPLStereo::DetectObs (float *up_vec, float maxRange)
{
  long result = NO_ERR;
  int rows, cols;

  if (subDispPic == NULL) {
    FatalErr ("No disparity or uncertainty info\n");
    return INIT_ERR;
  }
  rows = subDispPic->rows;
  cols = subDispPic->cols;
  if (obsPic == NULL) {
    obsPic = new JPLPic(mm);
    if (obsPic->Init (rows, cols, UC8_PIXEL) != NO_ERR) {
      FatalErr ("Cannot allocate obstacle map\n");
      return MEM_ERR;
    }
  }
  obsPic->ClearImage ();

  if ((result = DetectPosObs (up_vec, gMinHeight, gMinAngle, maxRange,
		OBS_NEGOTIABLE_BIT)) != NO_ERR)
    return result;

  if ((result = DetectPosObs (up_vec, gClimbHeight, gClimbAngle, maxRange,
			      OBS_NONNEGOTIABLE_BIT)) != NO_ERR)
    return result;

  /* need to add negative obstacle detection here */

  /* blob filtering */
  BlobFiltering ((unsigned char *) obsPic->GetPixelAddress(),
		 obsPic->rows, obsPic->cols, obsPic->GetRowBytes (),
		 10000, 15, 20, 20, NULL, NULL);

  return result;
}

/* assuming the vehicle is on a plane and
   the stereo matching was done by a horopter warping,
   find out the boundary of the the plane in the image.

   The boundary is used as obstacle

   Note that the disparity map is w.r.t. the horopter.
*/

int stairPlaneBlob = 1;

long
JPLStereo::PlaneBoundary (float warpM[9], float planeCoeff[4],
			  float maxRange, float minHeight, float maxHeight,
			  int numBits)
{
  float M[9], M0[9];
  unsigned char *obstaclePixel, *obstacle;
  AnythingT dispPixel, dispSrcPixel;
  long dispRB, obstacleRB;
  unsigned short *disp;
  int row, col, rows, cols, i, minRow, offset;
  unsigned char mask;
  float x0, y_0, z0, planeOffset[3], dist, range;
  float nbaseline, center, x_scale;

  mask = (1 << numBits);

  if (leftRectCam == NULL || rightRectCam == NULL) {
    FatalErr ("No Camera info \n");
    return INIT_ERR;
  }
	
  if (subDispPic == NULL) {
    FatalErr ("No disparity or uncertainty info\n");
    return INIT_ERR;
  }
	
  rows = subDispPic->rows;
  cols = subDispPic->cols;
  if (obsPic == NULL) {
    obsPic = new JPLPic(mm);
    if (obsPic->Init (rows, cols, UC8_PIXEL) != NO_ERR) {
      FatalErr ("Cannot allocate osbtacle map\n");
      return MEM_ERR;
    }
    obsPic->ClearImage ();
  }

  leftRectCam->M3D_2D(M);
  leftRectCam->M2D_3D(M0);
  dispSrcPixel.uc = dispPixel.uc = subDispPic->GetPixelAddress ();
  dispRB = subDispPic->GetRowBytes ();
  obstaclePixel = obsPic->GetPixelAddress ();
  obstacleRB = obsPic->GetRowBytes ();
	
  x_scale = 0.5 * (leftRectCam->scale[0] + rightRectCam->scale[0]);
  nbaseline = 0.0;
  for (i = 3; i--; )
    nbaseline += SQ(leftRectCam->C[i] - rightRectCam->C[i]);
  nbaseline = sqrt(nbaseline) * x_scale * ((float) I_DISP_SCALE);
  center = (rightRectCam->center[0] / rightRectCam->scale[0] -
	    leftRectCam->center[0] / leftRectCam->scale[0]) * 
    ((float) I_DISP_SCALE);
 
  for (row = 0; row < rows; row++, dispPixel.uc += dispRB) {
    planeOffset[0] = row * warpM[1] + warpM[2];
    planeOffset[1] = row * warpM[4] + warpM[5];
    planeOffset[2] = row * warpM[7] + warpM[8];
    disp = dispPixel.uh;
    for (col = 0; col < cols; col ++, disp++) {
      planeOffset[0] += warpM[0];
      planeOffset[1] += warpM[3];
      planeOffset[2] += warpM[6];

      if (*disp != NO_S2_DISP) {
	offset = (int) ((col - planeOffset[0] / planeOffset[2]) *
			I_DISP_SCALE);
	*disp += offset;
      }
    }
  }

  /* find the boundary column-wise */
  for (col = 0; col < cols; col++) {
    disp = dispSrcPixel.uh + ((rows - 1) * dispRB / sizeof(unsigned short)) +
	    col;
    dispPixel.uh = disp;
    minRow = -1;
    for (row = rows; row --; dispPixel.uc -= dispRB) {
      disp = dispPixel.uh;
      if (*disp == NO_S2_DISP) continue;
      x0 = M0[0] * col + M0[1] * row + M0[2];
      y_0 = M0[3] * col + M0[4] * row + M0[5];
      z0 = M0[6] * col + M0[7] * row + M0[8];

      range = nbaseline / (((float) *disp) - center);
      if (range > maxRange) continue;
      x0 = x0 * range + leftRectCam->C[0];
      y_0 = y_0 * range + leftRectCam->C[1];
      z0 = z0 * range + leftRectCam->C[2];
      
      dist = (x0 * planeCoeff[0] + y_0 * planeCoeff[1] +
	      z0 * planeCoeff[2] + planeCoeff[3]);
      //printf("Dist = %8.4f. Row = %4d, Col = %4d\n",
      //     dist, row, col);

      if (dist < maxHeight && dist > minHeight) {
	minRow = row;
      }
    }

    if (minRow >= 0) { // found it
      
      obstacle = (unsigned char *) (obstaclePixel + minRow * 
				    obstacleRB + col);
      *obstacle |= mask;
      //      printf("Final rows = %4d, pixel val %3d\n",
      //     minRow, *obstacle);
    }
  }

  /* blob filtering */
  BlobFiltering ((unsigned char *) obsPic->GetPixelAddress (),
		 obsPic->rows, obsPic->cols, obsPic->GetRowBytes (),
		 10000, stairPlaneBlob, 20, 20, NULL, NULL);

  return NO_ERR;
}
