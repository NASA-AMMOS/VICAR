#include <math.h>
#include "JPLStereo.h"



// For debugging, print ALL stereo parameters to a buffer

char *JPLStereo::params2s(char *buff)
{
  if (buff == NULL)
    buff = NEW(mm, "JPLStereo param string") char[1000];

  sprintf (buff,
	   "fixed_blob_filter_size=%d,\n"
	   "subpixelBits=%d, rangeBits=%d, disp=[%d:%d], options=0x%lx,\n"
	   "rightHandDisparity=%d,\n"
	   "mergeSqThreshold=%g, mergePixelThreshold=%g \n"
	   "range_params_good=%d, baseline=%g, center=%g\n"
	   "MM=[%g %g %g ; %g %g %g; %g %g %g ],\n"
	   "AA = [%g %g %g], CC = [%g %g %g], smoothWinSize=%d,\n"
	   "dogWinSize1=%d, dogWinSize2=%d, "
	   "matchWinSize=%d, numQuadraCurves=%d\n",
	   fixed_blob_filter_size,
	   subpixelBits, rangeBits, minDisp, maxDisp, options,
	   rightHandDisparity,
	   mergeSqThreshold, mergePixelThreshold,
	   range_params_good, baseline, center_X,
	   MM[0], MM[1], MM[2], MM[3], MM[4], MM[5], MM[6], MM[7], MM[8],
	   AA[0], AA[1], AA[2], CC[0], CC[1], CC[2], smoothWinSize,
	   dogWinSize1, dogWinSize2, matchWinSize, numQuadraCurves);
  return buff;
}





void JPLStereo::WriteStats (char *filename)
{
  FILE *fp;

  if ((fp = fopen (filename, "w")) == NULL) {
    printf ( "WriteStats: Failed to open \"%s\" for writing!\n",
	     filename ? filename : "[NULL]");
    return;
  }
  WriteStats (fp);
  fclose (fp);
} // JPLStereo::WriteStats

void
JPLStereo::WriteStats (FILE *fp)
{
  char *prefix = "   ";

  fprintf (fp, "STEREO OBJECT:\n");
  fprintf (fp, "  lrlosLimit: %d\n", lrlosLimit);
  fprintf (fp, "  Blob regions:  %g%% of image size, at least %d regions\n",
	   blob_region_fraction * 100.0, min_blob_regions_to_allocate);
  fprintf (fp, "  Cached range params are %s\n",
	   range_params_good ? "GOOD" : "BAD");
  fprintf (fp, "  Search window is %dx%d pixels\n",
	   matchWinSize, matchWinSizeY);
  fprintf (fp, " LEFT RECTIFIED CAMERA MODEL:\n");
  if (leftRectCam == NULL) 
    fprintf (fp, "%sNULL!\n", prefix);
  else 
    leftRectCam->PrintCameraModel (fp, prefix);

  fprintf (fp, " RIGHT RECTIFIED CAMERA MODEL:\n");
  if (rightRectCam == NULL) 
    fprintf (fp, "%sNULL!\n", prefix);
  else 
    rightRectCam->PrintCameraModel (fp, prefix);
  
  fprintf (fp, " LEFT RAW RECTIFIED IMAGE:\n");
  if (leftRawRectPic == NULL) 
    fprintf (fp, "%sNULL!\n", prefix);
  else 
    leftRawRectPic->WriteStats (fp, prefix);
  
  fprintf (fp, " RIGHT RAW RECTIFIED IMAGE:\n");
  if (rightRawRectPic == NULL) 
    fprintf (fp, "%sNULL!\n", prefix);
  else 
    rightRawRectPic->WriteStats (fp, prefix);
  
  fprintf (fp, " DISPARITY IMAGE (subpixel resolution):\n");
  if (subDispPic == NULL) 
    fprintf (fp, "%sNULL!\n", prefix);
  else 
    subDispPic->WriteStats (fp, prefix);
  
  fprintf (fp, " RANGE IMAGE (x,y,z triples):\n");
  if (rangePic == NULL) 
    fprintf (fp, "%sNULL!\n", prefix);
  else 
    rangePic->WriteStats (fp, prefix);
  
  fprintf (fp, " OBSTACLE IMAGE (computed from RANGE, not elevation):\n");
  if (obsPic == NULL) 
    fprintf (fp, "%sNULL!\n", prefix);
  else 
    obsPic->WriteStats (fp, prefix);

  fprintf (fp, " FILTER PIXEL COUNTS:\n");
  WriteFilterCounts (fp, prefix, &stats);
} // JPLStereo::WriteStats



/*!
  Write information about the most-recently computed stereo filter pixel
  counts to stdout.
*/

void JPLStereo::WriteFilterCounts (void)
{
  WriteFilterCounts (stdout, "", &stats);
} // JPLStereo::WriteFilterCounts


/*!
  Write information about the stereo filter pixel counts to the output stream.

  \param fp Output stream

  \param prefix Optional string prefix to use on each line or NULL.

  \param s Pointer to the structure contianing the statistics.
*/

void JPLStereo::WriteFilterCounts (FILE *fp, char *prefix,
				   StereoFilterCountT *s)
{
  long total, all;
  char buf[200];
  int cols = 36;

  if (fp == NULL || prefix == NULL)
    prefix = "";

  if (s == NULL) {
    fprintf (fp, "%s[null filter counts]\n", prefix);
    return;
  }

  all = total = s->rows * s->cols;
  int digits = (int) (log ((double) total) / log ((double) 10.0)) + 1;
  int rdigits = (int) (log ((double) s->rows) / log ((double) 10.0)) + 1;
  int cdigits = (int) (log ((double) s->cols) / log ((double) 10.0)) + 1;

  digits = MAX(digits, rdigits + cdigits + 1);

#define ONE_LINE(fmt,str,a1,a2,sub) \
  snprintf (buf, 200, fmt, prefix, cols, str, a1, a2); \
  total -= sub; \
  fprintf (fp, "%s   %3d%%    %*ld pixels\n", buf, \
	   (int) ((sub * 100 + (all >> 1)) / all), digits, total);


  ONE_LINE ("%s%-*s  %ldx%ld", "Rectified dimensions:",
	    s->rows, s->cols, 0);
  ONE_LINE ("%s%-*s  %*ld", "Pixels ACCEPTED:",
	    digits, s->f[ACCEPT], s->f[ACCEPT]);
  ONE_LINE ("%s%-*s  %*ld", "Pixels rejected by BLOB FILTER:",
	    digits, s->f[BLOB], s->f[BLOB]);
  ONE_LINE ("%s%-*s  %*ld",  "Pixels rejected by BORDER check:",
	    digits, s->f[BORDER], s->f[BORDER]);
  ONE_LINE ("%s%-*s  %*ld",  "Pixels in Dog,Smooth,CorrWin BOUND:",
	    digits, s->f[BOUND], s->f[BOUND]);
  ONE_LINE ("%s%-*s  %*ld",  "Pixels rejected by FLAT FILTER:",
	    digits, s->f[FLAT], s->f[FLAT]);
  ONE_LINE ("%s%-*s  %*ld",  "Pixels rejected by LEFT/RIGHT check:",
	    digits, s->f[LRLOS], s->f[LRLOS]);
  ONE_LINE ("%s%-*s  %*ld",  "Pixels rejected by OVERHANG check:",
	    digits, s->f[OVERHANG], s->f[OVERHANG]);
  ONE_LINE ("%s%-*s  %*ld",  "Pixels rejected by THRESHOLD curv:",
	    digits, s->f[THRESH], s->f[THRESH]);
  ONE_LINE ("%s%-*s  %*ld",  "Pixels rejected by VEHICLE filter:",
	    digits, s->f[VEHICLE], s->f[VEHICLE]);

#undef ONE_LINE

} // JPLStereo::WriteFilterCounts


void JPLStereo::WriteShortStats (char *filename)
{
  FILE *fp;

  if ((fp = fopen (filename, "w")) == NULL) {
    printf ( "WriteShortStats: Failed to open \"%s\" for writing!\n",
	     filename ? filename : "[NULL[");
    return;
  }
  WriteShortStats (fp);
  fclose (fp);
} // JPLStereo::WriteShortStats



void
JPLStereo::WriteShortStats (FILE *fp)
{
  char *prefix = "   ";

  fprintf (fp, "STEREO OBJECT:\n");
  fprintf (fp, "  lrlosLimit: %d\n", lrlosLimit);
  fprintf (fp, "  Blob region:  %g%% of image size, at least %d bytes\n",
	   blob_region_fraction * 100.0, min_blob_regions_to_allocate);
  fprintf (fp, "  Cached range params are %s\n",
	   range_params_good ? "GOOD" : "BAD");
  fprintf (fp, "  Search window is %dx%d pixels\n",
	   matchWinSize, matchWinSizeY);
  fprintf (fp, " LEFT RECTIFIED CAMERA MODEL:\n");
  if (leftRectCam == NULL) 
    fprintf (fp, "%sNULL!\n", prefix);
  else 
    leftRectCam->PrintCameraModelSummary (fp, prefix);

  fprintf (fp, " RIGHT RECTIFIED CAMERA MODEL:\n");
  if (rightRectCam == NULL) 
    fprintf (fp, "%sNULL!\n", prefix);
  else
    rightRectCam->PrintCameraModelSummary (fp, prefix);
  
  fprintf (fp, " LEFT RAW RECTIFIED IMAGE:\n");
  if (leftRawRectPic == NULL) 
    fprintf (fp, "%sNULL!\n", prefix);
  else 
    leftRawRectPic->WriteStats (fp, prefix);
  
  fprintf (fp, " RIGHT RAW RECTIFIED IMAGE:\n");
  if (rightRawRectPic == NULL) 
    fprintf (fp, "%sNULL!\n", prefix);
  else 
    rightRawRectPic->WriteStats (fp, prefix);
  
  fprintf (fp, " DISPARITY IMAGE (subpixel resolution):\n");
  if (subDispPic == NULL) 
    fprintf (fp, "%sNULL!\n", prefix);
  else 
    subDispPic->WriteStats (fp, prefix);
  
  fprintf (fp, " RANGE IMAGE (x,y,z triples):\n");
  if (rangePic == NULL) 
    fprintf (fp, "%sNULL!\n", prefix);
  else 
    rangePic->WriteStats (fp, prefix);
  
  fprintf (fp, " OBSTACLE IMAGE (computed from RANGE, not elevation):\n");
  if (obsPic == NULL) 
    fprintf (fp, "%sNULL!\n", prefix);
  else 
    obsPic->WriteStats (fp, prefix);
  
} // JPLStereo::WriteShortStats

