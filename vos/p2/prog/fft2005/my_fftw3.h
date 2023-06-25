#include <stdlib.h>
typedef    double       fftw_complex  [2] ;
typedef struct   fftw_plan_s     *  fftw_plan    ;
void *  fftw_malloc    (size_t n);
fftw_plan       fftw_plan_dft_1d    (int n,   fftw_complex  *in,   fftw_complex  *out, int sign,	unsigned flags);
fftw_plan       fftw_plan_dft_2d    (int nx, int ny,	  fftw_complex  *in,   fftw_complex  *out, int sign, unsigned flags);
void   fftw_execute    (fftw_plan     p);
void   fftw_destroy_plan    (  fftw_plan     p);
#define FFTW_FORWARD (-1)
#define FFTW_BACKWARD (+1)
#define FFTW_ESTIMATE (1U << 6)
