#ifndef __REAL_HELPERS__
#define __REAL_HELPERS__


#ifdef __cplusplus
extern "C" {
#endif
#include <stdio.h>	/* For printf() in believe() macros */
#include <math.h>	/* If we don't do this, including math.h later will
			   cause several symbols to be redefined */
#ifdef __cplusplus
}
#endif

#ifndef REAL_CMP_EPSILON
#define REAL_CMP_EPSILON 1e-6
#endif
#define EQe(x,y,eps) (((x)-(y)) < (eps) && ((x)-(y)) > -(eps))
#define EQ(x,y) EQe((x),(y),REAL_CMP_EPSILON)
#define LT(x,y) (!EQ((x),(y)) && (x) < (y))
#define GT(x,y) (!EQ((x),(y)) && (x) > (y))
#define LE(x,y) (EQ((x),(y)) || (x) < (y))
#define GE(x,y) (EQ((x),(y)) || (x) > (y))
#define IN(x,lo,hi)  (LE(lo, x) && LE (x, hi))
#define INe(x,lo,hi,e) (IN(x,lo,hi) || EQe(x,lo,e) || EQe(x,hi,e))

#define MOD_EQe(x,y,mod,e) EQe(MY_FMOD ((x) - (y), (mod)), 0.0, e)
#define MOD_EQ(x,y,mod) EQ(MY_FMOD ((x) - (y), (mod)), 0.0)
#define MOD_LT(x,y,mod) LT(MY_FMOD ((x) - (y), (mod)), 0.0)
#define MOD_GT(x,y,mod) GT(MY_FMOD ((x) - (y), (mod)), 0.0)
#define MOD_LE(x,y,mod) LE(MY_FMOD ((x) - (y), (mod)), 0.0)
#define MOD_GE(x,y,mod) GE(MY_FMOD ((x) - (y), (mod)), 0.0)

#ifndef ABS
#define ABS(x) (LT(x,0.0) ? -(x) : (x))
#endif
#ifndef INT_ABS
#define INT_ABS(x) (((x) < 0) ? -(x) : (x))
#endif
#ifndef SQ
#define SQ(x) ((x)*(x))
#endif
#ifndef SGN
#define SGN(x) (EQ((x), 0.0) ? 0 : (((x) < 0.0) ? -1 : 1))
#endif
#ifndef MAX
#define MAX(a,b) (((a) > (b)) ? (a) : (b))
#endif
#ifndef MIN
#define MIN(a,b) (((a) < (b)) ? (a) : (b))
#endif


/*! Modifies fmod slightly to ensure that it never returns a value that
  approximately equals (EQ) the modulus.

  \return The remainder
  \sa fmod POS_FMOD
*/
#define MY_FMOD(x,mod) ((_my_fmod_tmp = fmod ((x), (mod))), \
			 _my_fmod_tmp = EQ(ABS(_my_fmod_tmp),(mod)) ? 0.0 : \
					_my_fmod_tmp)

/*! Modifies fmod to always return a <em>positive</em> remainder.  fmod is
  antisymmetric around 0, but this is consistently positive.  For example:
  <pre>
  fmod (2, 5) = fmod (7, 5) = 2
  fmod (-3, 5) = -3
  POS_FMOD (2, 5) = POS_FMOD (7,5) = POS_FMOD (-3, 5) = 2
  </pre>

  \return The positive remainder after dividing x by the modulus
  \sa fmod  MY_FMOD
*/
#define POS_FMOD(x,mod) ((_my_fmod_tmp = fmod ((x), (mod))), \
			 _my_fmod_tmp + ((_my_fmod_tmp < 0.0) ? ABS(mod) : 0.0))

static float _my_fmod_tmp = 0.0;

#ifdef __cplusplus
#define INLINE inline
#else
#define INLINE
#endif

/*! Map a floating point value into an integer using a scaling function.
  Handle float roundoff gracefully. The nominal range starts from range_start
  and goes to range_start + steps * res.  

  \param val Value to be mapped
  \param range_start Start of the range
  \param steps Number of steps desired. Must be >= 0.
  \param res Step size of the discretization.  Must not EQ 0.0.
  \param scale Scale factor for the resolution.  Larger scale values imply a
  smaller effective step size.  Must be > 0.
  \param wrap Should values outside the range be wrapped?
  \param ival The resulting integer offset of val in chunks of (res / scale).
  \return Status of the mapping; 1 implies success, zero implies failure.
*/

static INLINE int discretize_value (float val, float range_start,
					   int steps, float res, int scale,
					   int wrap, long *lval)
{
  if (EQ (res, 0.0) || scale < 1 || steps < 0)
    return 0;

  /* Handle the non-wrapped case, when a return value outside the range is
     legal. */
  if (wrap == 0) {
    float range_end = range_start + steps * res;

#if 0
    /* This assymetry (use of "LE" in the second comparison) keeps us
       from incorrectly wrapping values on the boundary back to zero */
    if (LT (SGN(res) * val, SGN(res) * range_start) ||
	LE (SGN(res) * range_end, SGN(res) * val))
      return 0;
#endif

    /* Handle float roundoff at the edges of the range */
    if (EQ (val, range_start))
      val = range_start;
    if (EQ (val, range_end))
      val = range_end;
    if (EQ (val, 0.0))
      val = 0.0;
  }

  /* Compute the wrapped value. */
  if (lval) {
    float tmp_fmod = (float) (scale *
			      (val + 0.5 * res / scale - range_start));

    if (wrap)
      tmp_fmod = fmod (tmp_fmod, (float) (res * steps * scale));

    /* Use floor() to force consistent rounding */
    *lval = (long) floor (tmp_fmod / res);
    if (wrap && *lval < 0)
      *lval += steps * scale;
  }

  return 1;
}

#ifndef M_PI
#ifdef PI
#define M_PI  PI
#else
#define M_PI  3.1415926535897932384626433
#define PI M_PI
#endif
#endif

#ifndef ROOT2
#define ROOT2 1.4142135623731
#endif

#ifndef DEG2RAD
#define DEG2RAD(x) ((x) * M_PI / 180.0)
#endif
#ifndef RAD2DEG
#define RAD2DEG(x) ((x) * 180.0 / M_PI)
#endif

/* These used to be ASSERT calls, but for now we'll just "believe" them
   and return, without dumping core */

#ifndef PARAM_ERR
#define PARAM_ERR 4		/* HACK HACK should include from ErrHandle.h*/
#endif

#undef believe
#undef believev
#undef believez
#undef believe_ret
#define believe_ret(x,r) if (!(x)) { printf ("BELIEVE FAILED! %s\n", # x); return r;}
#define believe(x) believe_ret(x,INTERNAL_ERR)
#define believev(x) if (!(x)) { printf ("BELIEVE FAILED! %s\n", # x); return;}
#define believez(x) believe_ret(x,0)


/* ZeroToTwoPi -- returns radians normalized to lie in [ 0, 2Pi ) */
static float ZeroToTwoPi (float x)
{
  register float TwoPi = 2 * M_PI;
  while (LT(x, 0.0))
    x += TwoPi;
  while (GE(x, TwoPi))
    x -= TwoPi;

  /* Just for grins, force anything near zero to be exactly zero. */

  if (EQ(x, 0.0))
    x = 0.0;
  return x;
} /* ZeroToTwoPi */

/* MinusPiToPi -- returns radians normalized to lie in ( -Pi, Pi ] */
static float MinusPiToPi (float x)
{
  register float TwoPi = 2 * M_PI;
  while (LE(x, -M_PI))
    x += TwoPi;
  while (GT(x, M_PI))
    x -= TwoPi;

  /* Just for grins, force anything near PI to be exactly PI. */

  if (EQ(x, M_PI) || EQ(x, -M_PI))
    x = M_PI;
  return x;
} /* MinusPiToPi */


/******************************************************************************
 **   wraparound_min_dist -- Compute the minimum distance between indices
 ** taking wraparound and negative numbers into account.
 *****************************************************************************/

static
long wraparound_min_dist (long i1, long i2, long period)
{
  long di = (i2 - i1) % period;

  di = ABS (di);
  di = MIN (di, period - di);

  return di;
} /* wraparound_min_dist */


/*!
 * wraparound_min_delta computes the delta needed to change from_val
 * to to_val in a wraparound sense.  We assume these values already
 * lie within the period.
 */

static
long wraparound_min_delta (long from_val, long to_val, long period)
{
  long di;

  /* Avoid an exception */
  if (period == 0)
    return 0;

  di = (to_val - from_val) % period;

  if (ABS(di) > ABS((di + period)) % period)
    return di + period;
  if (ABS(di) > ABS((di - period)) % period)
    return di - period;

  return di;
} /* wraparound_min_delta */




#endif

