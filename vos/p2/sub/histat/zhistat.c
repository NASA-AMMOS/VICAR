#include "xvmaininc.h"
#include "ftnbridge.h"
#include "zhistat.h"

/************************************************************************/
/* C-Callable Version                                                   */
/************************************************************************/

void zhistat(void   *ohist,
	     int    npts,
	     float  *mean,
	     float  *sigma,
	     int    *maxfreq)
#if 0
void   zhistat(ohist,npts,mean,sigma,maxfreq)
void   *ohist;	
int    npts;
float  *mean;	
float  *sigma;	
int    *maxfreq;	
#endif
{
FTN_NAME2(histat, HISTAT) (ohist, &npts, mean, sigma, maxfreq);
}

void zhistat2(void   *hist,
	      int    npts,
	      float  *mean,
	      float  *sigma,
	      int    *mindn,
	      int    *maxdn,
	      int    *maxfreq)
#if 0
void   zhistat2(hist,npts,mean,sigma,mindn,maxdn,maxfreq)
void   *hist;	
int    npts;
float  *mean;	
float  *sigma;	
int    *mindn,*maxdn,*maxfreq;	
#endif
{
FTN_NAME2(histat2, HISTAT2) (hist, &npts, mean, sigma,mindn,maxdn, maxfreq);
}
