#include <stdio.h>
#include <assert.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <float.h>
#include "vicmain_c"
#include "zifmessage.h"
#include "cartoTaeUtils.h"

void main44(void)
{
  int iunit;
  float *inBuf = NULL;
  int sl, ss, nl, ns, line, samp;
  int nli, nsi;
  int cnt;
  int binCount;
  int *bins = NULL;
  float min = FLT_MAX, max = - FLT_MAX;
  float binWidth;
  int index, halfNumPix, sum;
  char outBuf[100];
  float median;

  zifmessage("MEDVAL version 2020-12-21");

  assert(zvunit(&iunit, "INP", 1, NULL) == 1);

  assert(zvopen(iunit, "OPEN_ACT", "SA", "IO_ACT", "SA", "U_FORMAT", "REAL", NULL) == 1);

  zvsize(&sl, &ss, &nl, &ns, &nli, &nsi);
  /* assert(zvget(iunit, "NL", &nl, "NS", &ns, NULL) == 1); */
  halfNumPix = (nl * ns) / 2;
  assert((inBuf = (float*) malloc(sizeof(float) * ns)));
  assert(zvp("BINCNT", &binCount, &cnt) == 1);
  assert((bins = (int*) malloc(sizeof(int) * binCount)));

  /* find min, max */
  for (line = sl; line < sl + nl; ++line) {
    assert(zvread(iunit, inBuf, "LINE", line, "SAMP", ss, "NSAMPS", ns, NULL) == 1);
    for (samp = 0; samp < ns; ++samp) {
      if (min > inBuf[samp])
	min = inBuf[samp];
      if (max < inBuf[samp]) {
	max = inBuf[samp];
      }
    }
  }

  binWidth = (max - min) / binCount;

  /* initialize bins */
  memset(bins, 0, sizeof(int) * binCount);

  /* fill bins */
  for (line = sl; line < sl + nl; ++line) {
    assert(zvread(iunit, inBuf, "LINE", line, "SAMP", ss, "NSAMPS", ns, NULL) == 1);
    for (samp = 0; samp < ns; ++samp) {
      index = (int) ((inBuf[samp] - min) / binWidth);
      if (index < 0)
	index = 0;
      if (index >= binCount)
	index = binCount - 1;
	
      ++ bins[index];
    }
  }

  assert(zvclose(iunit, NULL) == 1);

  /* find median */
  sum = 0;
  for (index = 0; index < binCount; ++ index) {
    sum += bins[index];
    if (sum >= halfNumPix)
      break;
  }

  median = min + binWidth * index;

  sprintf(outBuf, "Median: %10.5f", median);
  zifmessage(outBuf);

  mq_out_real("val", (double) median);
}
