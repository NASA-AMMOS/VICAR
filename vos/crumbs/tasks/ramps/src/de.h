#include "stdio.h"
/* #include "conio.h" */ /* conio.h not available on all platforms */
#include "stdlib.h"
#include "math.h"
#include "memory.h"
#include "string.h"

/* orig 3/18/99
#define MAXPOP  500
#define MAXDIM  35
*/
#define MAXPOP  500000
#define MAXDIM  10

/*------Constant for data parameters --------------------------------------*/
#define MAXINIPARMS 1024

/*------Constants for rnd_uni()--------------------------------------------*/

#define IM1 2147483563
#define IM2 2147483399
#define AM (1.0/IM1)
#define IMM1 (IM1-1)
#define IA1 40014
#define IA2 40692
#define IQ1 53668
#define IQ2 52774
#define IR1 12211
#define IR2 3791
#define NTAB 32
#define NDIV (1+IMM1/NTAB)
#define EPS 1.2e-7
#define RNMX (1.0-EPS)

