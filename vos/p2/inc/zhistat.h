#ifndef _HISTAT_H_
#define _HISTAT_H_

void zhistat(void   *ohist,
	     int    npts,
	     float  *mean,
	     float  *sigma,
	     int    *maxfreq);
void zhistat2(void   *hist,
	      int    npts,
	      float  *mean,
	      float  *sigma,
	      int    *mindn,
	      int    *maxdn,
	      int    *maxfreq);

#endif
