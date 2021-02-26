/*  This is the PHIST C Subroutine for the phist.f program  */
/*  which produces a histogram.                             */

#include "xvmaininc.h"
#include "ftnbridge.h"
#include "zphist.h"

void zphist(int *freq, int ns, int ilow, int ihigh, int ispike, int imode)
#if 0
int *freq,ns,ilow,ihigh,ispike,imode;
#endif
{
     FTN_NAME(phist)(freq,&ns,&ilow,&ihigh,&ispike,&imode);
}
