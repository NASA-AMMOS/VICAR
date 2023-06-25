#include <math.h>
#include "nav_memory.h"
#include <string.h>	// For memset
#include "JPLPic.h"



#define SET_PIXEL_AND_INC(anyp,pix_type,val,just_shift,errval) \
      switch (pix_type) { \
      case UC8_PIXEL: \
	*anyp.uc = (just_shift ? (*anyp.uc ^ 0x80) : (cmap.uc[val])); \
	anyp.uc++; \
	break; \
      case INT16_PIXEL: \
	*anyp.uh = (just_shift ? (*anyp.uh ^ 0x8000) : cmap.uh[val]); \
	anyp.uh++; \
	break; \
      case INT32_PIXEL: \
	*anyp.ul = (just_shift ? (*anyp.ul ^ 0x80000000) : cmap.ul[val]); \
	anyp.ul++; \
	break; \
      case FLOAT_PIXEL:  *anyp.f++  = cmap.f[val];  break; \
      case DOUBLE_PIXEL: *anyp.d++  = cmap.d[val];  break; \
      case ARGB32_PIXEL: \
	{ unsigned char *cmptr = cmap.uc + BytesPerPixel() * (unsigned char) val; \
	anyp.uc[0] = (just_shift ? (anyp.uc[0] ^ 0x80) : \
		      cmptr[0]); \
	anyp.uc[1] = (just_shift ? (anyp.uc[1] ^ 0x80) : \
		      cmptr[1]);\
	anyp.uc[2] = (just_shift ? (anyp.uc[2] ^ 0x80) : \
		      cmptr[2]);\
	anyp.uc[3] = (just_shift ? (anyp.uc[3] ^ 0x80) : \
		      cmptr[3]);\
	anyp.uc += 4; } \
	break; \
      case XYZ_FLOAT_PIXEL: \
	anyp.f[0] = cmap.f[val*3]; \
	anyp.f[1] = cmap.f[val*3 + 1]; \
	anyp.f[2] = cmap.f[val*3 + 2]; \
	anyp.f += 3; \
	break; \
      case XYZ_DOUBLE_PIXEL: \
	anyp.d[0] = cmap.d[val*3]; \
	anyp.d[1] = cmap.d[val*3 + 1]; \
	anyp.d[2] = cmap.d[val*3 + 2]; \
	anyp.d += 3; \
	break; \
      default: \
	fprintf (stderr, "SET_PIXEL_AND_inc:  unknown pixelType %ld!\n", \
		 pixelType); \
	return errval; \
      }



long
JPLPic::ResetBorder (int up, int down, int left, int right,
		     unsigned char cmap_val)
{
  int row, col;
  AnythingT src, ptr;

  // Make sure the input parameters lie in the bounds of the image

  if (up > rows) up = rows;
  else if (up < 0) up = 0;

  if (down > rows) down = rows;
  else if (down < 0) down = 0;

  if (left > cols) left = cols;
  else if (left < 0) left = 0;

  if (right > cols) right = cols;
  else if (right < 0) right = 0;

  /* up border */
  src.uc = pixels;
  for (row = 0; row < up; row++, src.uc += rowBytes) {
    ptr.uc = src.uc;
    for (col = 0; col < cols; col++)
      SET_PIXEL_AND_INC(ptr, pixelType, cmap_val, 0, INIT_ERR);
  }

  /* bottom border */
  src.uc = pixels + (rows - down) * rowBytes;
  for (row = rows - down; row < rows; row++, src.uc += rowBytes) {
    ptr.uc = src.uc;
    for (col = 0; col < cols; col ++)
      SET_PIXEL_AND_INC(ptr, pixelType, cmap_val, 0, INIT_ERR);
  }

  /* left & right border */
  src.uc = pixels + up * rowBytes;
  for (row = up; row < (rows - down); row++, src.uc += rowBytes) {
    
    /* left border */
    ptr.uc = src.uc;
    for (col = 0; col < left; col++)
      SET_PIXEL_AND_INC(ptr, pixelType, cmap_val, 0, INIT_ERR);
    
    /* right border */
    ptr.uc = src.uc + (cols - right);
    for (col = cols - right; col < cols; col++)
      SET_PIXEL_AND_INC (ptr, pixelType, cmap_val, 0, INIT_ERR);
  }

  return NO_ERR;
}



#ifndef OMIT_IMAGE_OPERATORS

#undef INT_IN
#undef IN
#define INT_IN(x,b0,b1) (((x) <= (b1) && (x) >= (b0)) || \
			 ((x) <= (b0) && (x) >= (b1)))
#define IN(x,b0,b1) \
     ((LE((double) (x),(double) (b1)) && GE((double) (x),(double) (b0))) || \
      (LE((double) (x),(double) (b0)) && GE((double) (x),(double) (b1))))

#define IntersectGivenX(m,b,x) ((m) * (x) + (b))
#define IntersectGivenY(m,b,y) (((y) - (b)) / (m))

// BoundLineWithinImage -- Given two points that define a line, recompute
// them after truncating any points outside the image bounds.  Returns 0
// on failure to compute bounds inside the image, else 1.

inline int BoundLineWithinImage (long *r0ptr, long *c0ptr, long *r1ptr, long *c1ptr,
			 long rows, long cols)
{
  if (r0ptr == NULL || c0ptr == NULL || r1ptr == NULL || c1ptr == NULL)
    return 0;

  long r0 = *r0ptr, c0 = *c0ptr, r1 = *r1ptr, c1 = *c1ptr;

  // Trivially inside the image

  if (INT_IN(r0, 0, rows-1) && INT_IN(r1, 0, rows-1) &&
      INT_IN(c0, 0, cols-1) && INT_IN(c1, 0, cols-1))
    return 1;

  if (c0 == c1) {

    // Handle a vertical line

    if (!INT_IN(c0, 0, cols-1))
      return 0;

    long nr0, nr1;

    nr0 = MIN(r0, r1);
    nr1 = MAX(r0, r1);
    nr0 = MAX(0, nr0);
    nr1 = MIN(rows-1, nr1);
    if (nr0 >= rows || nr1 < 0)
      return 0;
    r0 = nr0;
    r1 = nr1;
  } else if (r0 == r1) {

    // Handle a horizontal line

    if (!INT_IN(r0, 0, rows-1))
      return 0;

    long nc0, nc1;

    nc0 = MIN(c0, c1);
    nc1 = MAX(c0, c1);
    nc0 = MAX(0, nc0);
    nc1 = MIN(cols-1, nc1);
    if (nc0 >= cols || nc1 < 0)
      return 0;
    c0 = nc0;
    c1 = nc1;
  } else {

    // Handle arbitrary non-vertical, non-horizontal lines.  Compute the
    // parameters of the infinite line described by the points, and
    // determine the extent of that infinite line lying in the image.
    // Only worry about the infinite line, we'll truncate to fit the
    // requested bounds in a simple final step.

    double m = (((double) r1) - ((double) r0)) /
      (((double) c1) - ((double) c0));			// Slope
    double b = ((double) r0) - ((double) c0) * m;	// Intercept
    double nr0, nr1, nc0, nc1;	// New row and column values
    double pc0, pc1;		// Potential new column values

    // First compute the part of the line that lies within the column bounds.

    nc0 = 0.0;			// Leftmost image point column and row
    nr0 = IntersectGivenX (m, b, nc0);
    nc1 = (double) (cols-1);	// Rightmost image point column and row
    nr1 = IntersectGivenX (m, b, nc1);
    pc0 = IntersectGivenY (m, b, 0.0);
    pc1 = IntersectGivenY (m, b, (double) (rows-1));

    // We're done if the line hits both the left and right of the image.
    // However, if the line hits the top or bottom border, we need to
    // recompute.  Handle the leftmost point first, then the rightmost
    // point.

    // (nc0, nr0) is the leftmost point of the line segment within the
    // column bounds.  If the point lies above or below the image, find
    // the intersection with the top or bottom of the image.

    if (!IN(nr0, 0, rows-1)) {
      // Fail if both top and bottom intersect outside the image.  
      if ((!IN(pc0, 0, cols-1)) && !IN(pc1, 0, cols-1))
	return 0;
      if (IN(pc0, 0, cols-1)) {
	if (IN(pc1, 0, cols-1)) {
	  // Both hit the image, pick the minimum
	  if (LT (pc0, pc1)) {
	    nc0 = pc0;
	    nr0 = 0.0;
	  } else {
	    nc0 = pc1;
	    nr0 = (double) (rows-1);
	  }
	} else {
	  // Only pc0 hits the image (not pc1)
	  nc0 = pc0;
	  nr0 = 0.0;
	}
      } else {
	// Only pc1 hits the image (not pc0)
	nc0 = pc1;
	nr0 = (double) (rows-1);
      }
    }
    believez (IN(nc0, 0, cols-1) && IN(nr0, 0, rows-1));

    // (nr1, nc1) is the rightmost point of the line segment within the
    // column bounds.  If the point lies above or below the image, find
    // the intersection with the top or bottom of the image.

    if (!IN(nr1, 0, rows-1)) {
      // Fail if both top and bottom intersect outside the image.  
      if ((!IN(pc0, 0, cols-1)) && !IN(pc1, 0, cols-1))
	return 0;
      if (IN(pc0, 0, cols-1) && IN(pc1, 0, cols-1)) {
	// Both top and bottom hit the image, pick the maximum
	if (GT(pc0, pc1)) {
	  nc1 = pc0;
	  nr1 = 0.0;
	} else {
	  nc1 = pc1;
	  nr1 = (double) (rows-1);
	}
      } else {
	// Must have hit the left column of the image
	believez (EQ(nc0, 0.0));
	if (IN(pc0, 0, cols-1)) {
	  // Only pc0 hits the image (not pc1)
	  nc1 = pc0;
	  nr1 = 0.0;
	} else {
	  believez (IN(pc1, 0, cols-1));
	  // Only pc1 hits the image (not pc0)
	  nc1 = pc1;
	  nr1 = (double) (rows-1);
	}
      }
    }
    believez (IN(nc1, 0, cols-1) && IN(nr1, 0, rows-1));
#ifdef DEBUG_LINE_BOUNDS
    printf ("(nr0,nc0) (%g,%g)     (nr1,nc1) (%g,%g)\n", nr0, nc0, nr1, nc1);
#endif

    // Now we have the exact extent of the infinite line inside the image.
    // The leftmost point is (nr0,nc0), the rightmost point is (nr1,nc1).

    // Make (r0,c0) the leftmost of the input points.
    if (c0 > c1) {
      long t;

      t = r0; r0 = r1; r1 = t;
      t = c0; c0 = c1; c1 = t;
    }

    // Fail if the input segment lies outside the part of the line known
    // to be in the image.
    if (LT((double) c1, nc0) || GT((double) c0, nc1))
      return 0;

    // Replace the input bounds if they extend beyond that part of the
    // line known to be in the image.
    if (LT((double) c0, nc0)) {
      c0 = (long) nc0;
      r0 = (long) nr0;
    }
    if (GT((double) c1, nc1)) {
      c1 = (long) nc1;
      r1 = (long) nr1;
    }
    believez (INT_IN(c0,-1,cols) && INT_IN(r0,-1,rows) &&
	     INT_IN(c1,-1,cols) && INT_IN(r1,-1,rows));
  
    // Handle off-by-one errors
    c0 = MIN(cols-1,c0); c0 = MAX(0,c0);
    c1 = MIN(cols-1,c1); c1 = MAX(0,c1);
    r0 = MIN(rows-1,r0); r0 = MAX(0,r0);
    r1 = MIN(rows-1,r1); r1 = MAX(0,r1);
  }

#ifdef DEBUG_LINE_BOUNDS
  printf ("(r0,c0) (%ld,%ld)     (r1,c1) (%ld,%ld)\n", r0, c0, r1, c1);
#endif
  believez (IN(c0,0,cols-1) && IN(r0,0,rows-1) &&
	  IN(c1,0,cols-1) && IN(r1,0,rows-1));
  *c0ptr = c0; *c1ptr = c1; *r0ptr = r0; *r1ptr = r1;
  return 1;
}

#endif /* ! OMIT_IMAGE_OPERATORS */



#ifndef ABS
#define ABS(x) (((x) < 0) ? -(x) : (x))
#endif

long JPLPic::SetPixel (long r, long c, int wrap,
		       unsigned char val, int just_shift)
{
  AnythingT u;

  u.uc = GetPixelAddress (r, c, wrap);

  if (u.uc == NULL)
    return PARAM_ERR;

  SET_PIXEL_AND_INC (u, pixelType, val, just_shift, INIT_ERR);

  return NO_ERR;
} // JPLPic::SetPixel





#ifndef OMIT_IMAGE_OPERATORS


// JPLPic::Line -- Draw a line in an image, using Bresenham's algorithm
// (Computer Graphics, Hearn & Baker, 1986, pp 60-61).

void JPLPic::Line (long r0, long c0, long r1, long c1, int wrap,
			  unsigned char val, int just_shift)
{
  // Make sure arguments are in bounds

  if (wrap == 0) {

    // Truncate the line segment down to just the part that appears in
    // the image.

    if (BoundLineWithinImage (&r0, &c0, &r1, &c1, rows, cols) == 0)
      return;

    if (r0 >= rows) r0 = rows-1;
    if (c0 >= cols) c0 = cols-1;
    if (r1 >= rows) r1 = rows-1;
    if (c1 >= cols) c1 = cols-1;

    if (r0 < 0) r0 = 0;
    if (c0 < 0) c0 = 0;
    if (r1 < 0) r1 = 0;
    if (c1 < 0) c1 = 0;
  }

  long dr, dc, r, c, p, con1, con2;

  dc = ABS (c1-c0);
  dr = ABS (r1-r0);

  if (dc == 0) {
    if (r0 < r1)
      for (r = r0; r <= r1; r++)
	SetPixel (r, c0, wrap, val, just_shift);
    else
      for (r = r1; r <= r0; r++)
	SetPixel (r, c0, wrap, val, just_shift);
  } else if (dc > dr) {
    long c_end, r_inc = 1;

    if (c0 > c1) {
      c = c1; r = r1;
      c_end = c0;
      r_inc = (r1 > r0) ? -1 : 1;	// Hearn&Baker left out this test
    } else {
      c = c0; r = r0;
      c_end = c1;
      r_inc = (r0 > r1) ? -1 : 1;
    }
    p = 2 * dr - dc;
    con1 = 2 * dr;
    con2 = 2 * (dr - dc);
    SetPixel (r, c, wrap, val, just_shift);

    while (c < c_end) {
      c++;
      if (p < 0)
	p += con1;
      else {
	r += r_inc;
	p += con2;
      }
      SetPixel (r, c, wrap, val, just_shift);
    }
  } else {
    long r_end, c_inc;

    if (r0 > r1) {
      c = c1; r = r1;
      r_end = r0;
      c_inc = (c1 > c0) ? -1 : 1;
    } else {
      c = c0; r = r0;
      r_end = r1;
      c_inc = (c0 > c1) ? -1 : 1;
    }
    p = 2 * dc - dr;
    con1 = 2 * dc;
    con2 = 2 * (dc - dr);
    SetPixel (r, c, wrap, val, just_shift);

    while (r < r_end) {
      r++;
      if (p < 0)
	p += con1;
      else {
	c += c_inc;
	p += con2;
      }
      SetPixel (r, c, wrap, val, just_shift);
    }
  }
}



#endif /* ! OMIT_IMAGE_OPERATORS */


//////////////////////////////////////////////////////////////////////////////
//  INTEGER-BASED CIRCLE AND ARC DRAWING CODE
//
//  The Circle and Arc methods both use variants of Bresenham's
//  integer-based drawing algorithms.  Arc is in fact the same as
//  circle, with the addition of a range check to ensure that each
//  pixel lies in the desired arc bounds.  So it's not very efficient,
//  since it actually computes all points on the circle, but it is
//  effective.
//
//  At a coarse level, Bresenham's approach is to compute the points
//  needed to plot just 1/8th of a circle.  The rest of the points
//  follow directly by symmetry.  You just need to be careful not to
//  plot the same point more than once, because our "SetPixel" method
//  has an XOR option; so multiple plots would result in no apparent
//  change.
//
//  How is the 1/8th of a circle generated?  There is a nice
//  explanation on the web at
//  http://www.cc.gatech.edu/gvu/multimedia/nsfmmedia/cware/graphics/notes/ragra/sccir/sccir04.html
//  and related pages, modulo a few math errors and an
//  instance of backwards-reasoning.  Here's a hopefully-more-logical
//  summary, albeit without the nice image illustrations from the web site.
//  
//  We're only interested points lying on any 1/8th of the circle, so
//  we choose to focus on the lower half of quadrant 1:
//
//	Y ^
//	  | 
//	  | 
//	  | 
//	  | 
//	  |	    .
//	  | 	  ....
//	  | 	.......
//	  |   .........
//	  | ...........
//	  ._______________> X
//	  
//	  | <-- R --> |
//
//  Fixing our attention to 1/8th of the circle allows us to make
//  certain useful assumptions.  For instance, we know that in this
//  quadrant, to draw the arc starting at the X axis and move up, Y
//  always increases more quickly than X decreases.  Also note that in
//  this area, values of X lying on the circle are always positive
//  numbers greater than 1 (in fact X >= R / sqrt(2)).  In what
//  follows we're only concerned with drawing the outer edge (i.e. the
//  circumference) of the circle; the extention that also fills in the
//  circle is relatively trivial.
//
//  As a starting point, consider the formula for the equation of
//  points on a circle centered at (0,0):
//
//	f(X, Y) = X^2 + Y^2 - R^2 = 0
//
//  Think of drawing the arc spanning 1/8th of a circle starting from
//  the point (R, 0) on the X axis, going up to (R/sqrt(2),
//  R/sqrt(2)).  Y will always increase by 1, and we will decrease X
//  by 1 only when the edge of the continuously-valued circle crosses
//  the halfway mark between X and X-1.  We want to model this change
//  in X using integer-only operations, but before we worry about
//  that, simply consider the value of f() at the point of interest:
//  the halfway mark between X and X-1 in the row above the current
//  row (Y+1).  We want to know, for a given Y+1 row, whether the
//  corresponding X is greater or less than X-1/2, where X is the
//  integer value at the current row.  We'll take advantage of
//  properties of this particular arc in formulating this expression.
//
//	THERE EXISTS eps (-1/2 < eps < 1/2) SUCH THAT
//	f(X - 1/2 + eps, Y + 1) = 0
//	   = (X - 1/2 + eps)^2 + (Y + 1)^2 - R^2
//	   = (X - 1/2)^2 + 2 * eps * (X - 1/2) + eps^2 + (Y + 1)^2 - R^2
//	   = f(X - 1/2, Y + 1) + 2 * eps * (X - 1/2) + eps^2 = 0
//
//  The value of eps could be computed using float numbers and the
//  sqrt() function, but it will turn out that we don't need to use
//  floating point math at all.  It is enough to care simply about the
//  sign of "eps".  Note that when eps is < 0, we should subtract 1
//  from X.  When eps > 0, we should keep X the same.
//
//  Although we do not compute eps precisely, it turns out we can use
//  part of the equation above to reliably compute its sign.  That is,
//  we're going to compute some other function, whose sign is reliably
//  related to the sign of eps, and which thus will allow us to
//  determine when to change the value of X.  Got it?
//
//  The mysterious expression is contained in the formula above.
//  We'll call the expression d_i, and it will represent the subpixel
//  offset of the edge of the circle, in integer arithmetic modulo
//  some multiple of the radius (but don't worry about that yet).
//
//	d_i = f(X - 1/2, Y + 1)
//	    = eps * ( -eps - 2*X + 1)
//
//  To understand the relation between d_i and eps, consider the sign
//  of d_i.
//
//      When d_i >= 0,
//	    eps * (-eps - 2*X + 1) >= 0
//	    eps * (eps + 2*X - 1) <= 0
//          so  eps^2 + 2 * eps * (X - 1/2) <= 0
//	    so  eps^2 <= -2 * eps * (X - 1/2)
//
//	and since X is guaranteed to be >= R/sqrt(2), we must have eps <= 0
//	for the equation to hold.
//
//	When d_i <= 0,
//	    eps^2 >= -2 * eps * (X - 1/2)
//
//	and if eps were negative this could never be true because X is
//	so much larger than eps, so we must have eps >= 0.
//
//  To summarize,
//	When d_i >= 0, eps <= 0
//	When d_i <= 0, eps >= 0
//
//  But the question remains, can we compute d_i using integer only
//  operations?  Luckily, yes.  It's not immediately obvious, but it
//  follows from the derivation of d_(i+1) in the two cases:
//
//  When d_i >= 0 (i.e. eps <= 0)
//	d_(i+1) = f(X - 1 - 1/2, Y + 1 + 1)
//		= (X-1/2)^2 - 2*(X-1/2) + 1 + (Y+1)^2 + 2*Y + 1 - R^2
//		= f(X - 1/2, Y + 1) - 2*(X-1/2) + 1 + 2*Y + 1
//		= d_i - 2*(X-1/2) + 1 + 2*Y + 1
//		= d_i - 2*(Y - X) + 3
//  When d_i <= 0 (i.e. eps >= 0)
//	d_(i+1) = f(X - 1/2, Y + 1 + 1)
//		= (X-1/2)^2 + (Y+1)^2 + 2*Y + 1 - R^2
//		= f(X - 1/2, Y + 1) + 2*Y + 1
//		= d_i + 2*Y + 1
//
//  Luckily, both possibilities for d_(i+1) can be performed using
//  integer-only operations.
//
//  The only remaining step is to set an initial value for d_i, which
//  represents the crossing point.  We can do this simply by
//  evaluating d_0 at f (R - 1/2, 1).  So the complete recursive
//  definition of d_i is:
//
//	d_0 = f (R - 1/2, 1)
//	    = (R - 1/2)^2 + 1^2 - R^2
//	    = R^2 - R + 1/4 + 1 - R^2
//	    = -R + 5/4
//
//	and the change from d_i to d_(i+1) is:
//	   2*Y + 1		when d_i < 0
//	   2(Y-X) + 3		when d_i > 0
//
//  Of course "5/4" is not an integer, but we can compensate by
//  multiplying all these expressions by 4.  
//
//  What's a little odd is that published versions of this algorithm
//  seem to use slightly different numbers.  E.g., Hearn and Baker use
//  d_0 = 3 - 2 * R, with change 4*Y+6 when d_i < 0, and change
//  4*(Y-X)+10 when d_i > 0.  The web page mentioned above uses d_0 =
//  1 - R, with change 2*Y+1 when d_i < 0, and change 2*(Y-X)+1 when
//  d_i > 0.  I've found that the numbers that come from my analysis
//  tend to result in very boxy circles when the radius is small, so
//  some tweaking is warranted, though I can't say I've found a
//  mathematical justification for any particular tweaks yet.
//
//	-- Mark Maimone 25 October 2002
//
//////////////////////////////////////////////////////////////////////////////


// ARC_SET_PIXEL -- this gets complicated, because we have to check two things
// for each ARC_SET_PIXEL.  First we have to confirm that the pixel we're
// trying to set lies in the fraction of the circle whose arc is being drawn.
// This gets complicated by the phase wraparound.  Then, having determined it's
// part of the arc that was really requested, we have to verify that the
// pixel we're trying to set is really within the image bounds.

#define LIES_OUTSIDE_ARC(r,c) ((tmp = atan2((double) -(r),(double) (c))), \
			       (LT(tmp,0.0) ? (tmp += 2.0*M_PI) : 0), \
			       (!EQ(tmp,end_theta) && !EQ(start_theta,tmp) && \
				((LE(tmp,end_theta) && LE(start_theta,tmp)) ^ \
				 !swap_p)))

// We call LIES_OUTSIDE_ARC often because we run the risk of omitting the
// endpoints of the arc if we only test one corner of the pixel.

#define ARC_SET_PIXEL(r,c) (LIES_OUTSIDE_ARC(r,  c  ) && \
			    LIES_OUTSIDE_ARC(r+1,c  ) && \
			    LIES_OUTSIDE_ARC(r  ,c+1) && \
			    LIES_OUTSIDE_ARC(r+1,c+1))? 0 : \
			     ((pixelType == UC8_PIXEL) ? \
			      ((ptr = GetPixelAddress(center_row+(r), \
						      center_col+(c), wrap)), \
			       (ptr ? (*ptr = (just_shift) ? (*ptr ^ 0x80) \
				       : cmap.uc[byte_val]) : 0)) : \
			      SetPixel (center_row+(r), \
					center_col+(c), \
					wrap, byte_val, just_shift))


// ARC_SET_ALL -- Use Bresenham's algorithm to draw a complete circle.
// But use a test in ARC_SET_PIXEL to only attempt to draw those pixels
// that comprise the requested arc.
#define ARC_SET_HALF(r,c)  (ARC_SET_PIXEL( (r),  (c)), \
		       (r ? ARC_SET_PIXEL(-(r),  (c)):0), \
		       (c ? ARC_SET_PIXEL( (r), -(c)):0), \
		       ((r && c) ? ARC_SET_PIXEL(-(r), -(c)):0))

#define ARC_SET_ALL(r,c) (ARC_SET_HALF (r,c), \
		      (r != c) ? ARC_SET_HALF (c,r) : 0)

// Although this circle-drawing code is integer-based, we pass in a float
// radius to allow better modelling.  Use EXTRA_BITS to define how many
// extra binary bits you want to use to increase the resolution fidelity
// of the generated circle or arc.

#define EXTRA_BITS 0

// JPLPic::Arc -- Draw an arc in an image, using Bresenham's circle algorithm
// (Computer Graphics, Hearn & Baker, 1986, pp 68-69).
// Draw an arc of a circle, centered at given row/col, starting at location
// theta (given in radians), continuing for delta_theta radians.

void JPLPic::Arc (long center_row, long center_col,
			 float radius, float start_theta, float delta_theta,
			 int wrap, unsigned char byte_val, int just_shift)
{
  long p, r, c;
  unsigned char *ptr;

  // Add a fudge factor based on the angle subtended by a pixel at the
  // distance of 1 radius

  float fudge_factor = GT(ABS(radius), 2.56) ?
    atan2 (1.0, ABS(radius)-1.0) : 0.5;
  fudge_factor = 0.0;

  start_theta -= SGN(delta_theta) * ABS(fudge_factor);

  // Map the start and end values into the range [0 .. 2pi)

  while (LT(start_theta,0.0))
    start_theta += 2.0*M_PI;
  while (LE(2.0*M_PI,start_theta))
    start_theta -= 2.0*M_PI;
  // Now start_theta is in [0..2PI)

  float end_theta = start_theta+delta_theta;

  end_theta += SGN(delta_theta) * ABS(fudge_factor);

  while (LT(end_theta,0.0))
    end_theta += 2.0*M_PI;
  while (LE(2.0*M_PI,end_theta))
    end_theta -= 2.0*M_PI;
  // Now end_theta is in [0..2PI)

  if (LT(delta_theta,0.0)) {
    float swap_t = end_theta;
    end_theta = start_theta;
    start_theta = swap_t;
  }

  // Now the increment from start_theta to end_theta is guaranteed
  // positive, but it might wrap around.

  int swap_p = start_theta > end_theta;
  float tmp;
  if (swap_p) {
    float swap_t = end_theta;
    end_theta = start_theta;
    start_theta = swap_t;
  }

  // Now start_theta is definitely less than end_theta.  swap_p is set
  // iff you want the wrap-around range (the larger end_theta up
  // through start_theta mod 2pi).

  if (LE(2.0*M_PI,ABS(delta_theta)+ABS(fudge_factor*2.0))) {
    Circle (center_row, center_col, radius, 0, wrap, byte_val, just_shift);
    return;
  }
  c = 0;
  r = (long) (radius+0.5);
  //#define OLD
#ifdef OLD
  p = 3 - 2 * radius;

  while (c < r) {
    ARC_SET_ALL(r,c);

    if (p < 0) {
      p += 4 * c + 6;
    } else {
      p += 4 * (c - r) + 10;
      r--;
    }
    c++;
  }
  if (r == c) {
    ARC_SET_ALL(r,c);
  }
#else
  p = (long) (((float) (1 << EXTRA_BITS)) * (5 - 4 * radius));
  while (c < r) {
    ARC_SET_ALL(r,c);

    if (p < 0) {
      p += (8 * c + 4) << EXTRA_BITS;
    } else {
      p += (8 * (c - r) + 12) << EXTRA_BITS;
      r--;
    }
    c++;
  }
#endif
  if (r == c) {
    ARC_SET_ALL(r,c);
  }
}


#define PIX_XOR (*ptr ^ 0x80)
#define SET_PIXEL(r,c) ((pixelType == UC8_PIXEL) ?  \
			((ptr = GetPixelAddress((r),(c), wrap)) ? \
			 (*ptr = (just_shift ? PIX_XOR : cmap.uc[byte_val])) : 0) \
			: SetPixel(r, c, wrap, byte_val, just_shift))

#define FILL_ALL(r,c) { \
  if (r != lastr) { \
    Line (center_row + (r), center_col - (c), \
	  center_row + (r), center_col + (c), wrap, byte_val, just_shift); \
    if (r) Line (center_row - (r), center_col - (c), \
	  center_row - (r), center_col + (c), wrap, byte_val, just_shift); \
  } else { \
    SET_HALF (r, c); \
  } \
  if (c != lastc && c != r) { \
    Line (center_row + (c), center_col - (r), \
          center_row + (c), center_col + (r), wrap, byte_val, just_shift); \
    if (c) Line (center_row - (c), center_col - (r), \
	  center_row - (c), center_col + (r), wrap, byte_val, just_shift); \
  } else if (c != r) { \
    SET_HALF (c, r); }}

#define SET_HALF(r,c) (SET_PIXEL(center_row + (r), center_col + (c)), \
		       (r ? SET_PIXEL(center_row - (r), center_col + (c)):0), \
		       (c ? SET_PIXEL(center_row + (r), center_col - (c)):0), \
		       ((r&&c) ? SET_PIXEL(center_row - (r), center_col - (c)):0))

#define SET_ALL(r,c) (SET_HALF (r,c), \
		      (r != c) ? SET_HALF (c,r) : 0)

// JPLPic::Circle -- Draw an arc in an image, using Bresenham's algorithm
// (Computer Graphics, Hearn & Baker, 1986, pp 68-69).

void JPLPic::Circle (long center_row, long center_col, float radius,
			    int fill, int wrap, unsigned char byte_val,
			    int just_shift)
{
  long p, r, c, lastr, lastc;
  unsigned char *ptr;

  c = 0;
  r = (long) (radius+0.5);
  lastr = lastc = -1;
#ifdef OLD
  p = 3 - 2 * radius;

  while (c < r) {
    if (fill) {
      FILL_ALL(r,c);
    } else {
      SET_ALL(r,c);
    }
    lastr = r;
    lastc = c;
    if (p < 0) {
      p += 4 * c + 6;
    } else {
      p += 4 * (c - r) + 10;
      r--;
    }
    c++;
  }
#else
  //  p = 5 - 4 * radius;
  p = (long) (((float) (1 << EXTRA_BITS)) * (10 - 4 * radius));

  while (c < r) {
    if (fill) {
      FILL_ALL(r,c);
    } else {
      SET_ALL(r,c);
    }
    lastr = r;
    lastc = c;
    if (p < 0) {
      p += (8 * c + 4) << EXTRA_BITS;
    } else {
      p += (8 * (c - r) + 12) << EXTRA_BITS;
      r--;
    }
    c++;
  }
#endif
  if (r == c) {
    if (fill) {
      FILL_ALL(r,c);
    } else {
      SET_ALL(r,c);
    }
  }
} // JPLPic::Circle



#undef PIX_XOR
#define PIX_XOR (*ptr + byte_val)


// JPLPic::AntiAliasedCircle -- Draw a circle with anti-aliased borders
// so you don't see any jaggies.  Algorithm by Xiaolin Wu, in Graphics Gems II
// pp 446-450

void JPLPic::AntiAliasedCircle (long center_row, long center_col,
				long radius, int wrap,
				unsigned char max_val, int just_shift)
{
  unsigned char last_intense = 0;
  unsigned char this_intense = 0;
  unsigned char byte_val = max_val;
  unsigned char *ptr;
  long i = radius;
  long j = 0;

  // The mapping to aliased values is nonlinear; compute the mapping once

  float Gamma = 3.0;
  static float current_gamma = 0;
  static unsigned char colormap[256];

  if (current_gamma != Gamma) {
    for (int v = 0; v < 256; v++)
      colormap[v] =
	(unsigned char) (255.0 * pow (((float) v) / 255.0, 1.0 / Gamma));
    current_gamma = Gamma;
  }

  if (just_shift)
    max_val = 128;

  SET_ALL(i, j);

  while (j < i) {
    float delta = sqrt ((float) (radius * radius - j * j));
    float intense_ratio = ((float) max_val) * (ceil (delta) - delta) + 0.5;

    j++;
    this_intense = (unsigned char) intense_ratio;
    if (last_intense > this_intense)
      i--;
    byte_val = colormap[max_val - this_intense];
    SET_ALL(i, j);
    byte_val = colormap[this_intense];
    SET_ALL(i-1, j);
    last_intense = this_intense;
  }
  if (j == i) {
    byte_val = max_val;
    SET_ALL(i, j);
  }
} // JPLPic::AntiAliasedCircle

// JPLPic::AntiAliasedArc -- Draw an arc in an image, using Wu's algorithm
// Draw an arc of a circle, centered at given row/col, starting at location
// theta (given in radians), continuing for delta_theta radians.

void JPLPic::AntiAliasedArc (long center_row, long center_col, float radius,
			     float start_theta, float delta_theta,
			     int wrap, unsigned char max_val, int just_shift)
{
  unsigned char last_intense = 0;
  unsigned char this_intense = 0;
  unsigned char byte_val = max_val;
  unsigned char *ptr;
  long i = (long) (radius+0.5);
  long j = 0;

  // The mapping to aliased values is nonlinear; compute the mapping once

  float Gamma = 3.0;
  static float current_gamma = 0;
  static unsigned char colormap[256];

  if (current_gamma != Gamma) {
    for (int v = 0; v < 256; v++)
      colormap[v] =
	(unsigned char) (255.0 * pow (((float) v) / 255.0, 1.0 / Gamma));
    current_gamma = Gamma;
  }

  // Add a fudge factor based on the angle subtended by a pixel at the
  // distance of 1 radius

  float fudge_factor = GT(ABS(radius), 2.56) ?
    atan2 (1.0, ABS(radius)-1.0) : 0.5;
  fudge_factor = 0.0;

  start_theta -= SGN(delta_theta) * ABS(fudge_factor);

  // Map the start and end values into the range [0 .. 2pi)

  while (LT(start_theta,0.0))
    start_theta += 2.0*M_PI;
  while (LE(2.0*M_PI,start_theta))
    start_theta -= 2.0*M_PI;
  // Now start_theta is in [0..2PI)

  float end_theta = start_theta+delta_theta;

  end_theta += SGN(delta_theta) * ABS(fudge_factor);

  while (LT(end_theta,0.0))
    end_theta += 2.0*M_PI;
  while (LE(2.0*M_PI,end_theta))
    end_theta -= 2.0*M_PI;
  // Now end_theta is in [0..2PI)

  if (LT(delta_theta,0.0)) {
    float swap_t = end_theta;
    end_theta = start_theta;
    start_theta = swap_t;
  }

  // Now the increment from start_theta to end_theta is guaranteed
  // positive, but it might wrap around.

  int swap_p = start_theta > end_theta;
  float tmp;
  if (swap_p) {
    float swap_t = end_theta;
    end_theta = start_theta;
    start_theta = swap_t;
  }

  // Now start_theta is definitely less than end_theta.  swap_p is set
  // iff you want the wrap-around range (the larger end_theta up
  // through start_theta mod 2pi).

  if (LE(2.0*M_PI,ABS(delta_theta)+ABS(fudge_factor*2.0))) {
    Circle (center_row, center_col, radius, 0, wrap, max_val, just_shift);
    return;
  }

  if (just_shift)
    max_val = 128;

  ARC_SET_ALL(i, j);

  while (j < i) {
    float delta = sqrt ((float) (radius * radius - j * j));
    float intense_ratio = ((float) max_val) * (ceil (delta) - delta) + 0.5;

    j++;
    this_intense = (unsigned char) intense_ratio;
    if (last_intense > this_intense)
      i--;
    byte_val = colormap[max_val - this_intense];
    ARC_SET_ALL(i, j);
    byte_val = colormap[this_intense];
    ARC_SET_ALL(i-1, j);
    last_intense = this_intense;
  }
  if (j == i) {
    byte_val = max_val;
    ARC_SET_ALL(i, j);
  }
}



#undef PIX_XOR




void JPLPic::Box (long top_row, long top_col, long row_size,
			 long col_size, int wrap, unsigned char byte_val,
			 int just_shift)
{
  Line (top_row, top_col,
	top_row + row_size - 2, top_col,
	wrap, byte_val, just_shift);
  Line (top_row + row_size - 1, top_col,
	top_row + row_size - 1, top_col + col_size - 2,
	wrap, byte_val, just_shift);
  Line (top_row + row_size - 1, top_col + col_size - 1,
	top_row + 1, top_col + col_size - 1,
	wrap, byte_val, just_shift);
  Line (top_row, top_col + col_size - 1,
	top_row, top_col + 1,
	wrap, byte_val, just_shift);
}


int JPLPic::ColorMapEntryRepeatsOneByte (unsigned char byte_val,
					 unsigned char *mem_val)
{
  unsigned char memset_arg = '\0';
  AnythingT ptr;
  int retval = 0;

  switch (pixelType) {
  case UC8_PIXEL:
    retval = 1;
    memset_arg = cmap.uc[byte_val];
    break;
  case INT16_PIXEL: 
    ptr.uh = &cmap.uh[byte_val];
    memset_arg = ptr.uc[0];
    retval = memset_arg == ptr.uc[1];
    break;
  case INT32_PIXEL:
    ptr.ul = &cmap.ul[byte_val];
    memset_arg = ptr.uc[0];
    retval = memset_arg == ptr.uc[1] &&
      memset_arg == ptr.uc[2] &&
      memset_arg == ptr.uc[3];
    break;
  case FLOAT_PIXEL:
    ptr.f = &cmap.f[byte_val];
    memset_arg = ptr.uc[0];
    retval = memset_arg == ptr.uc[1] &&
      memset_arg == ptr.uc[2] &&
      memset_arg == ptr.uc[3];
    break;
  case DOUBLE_PIXEL:
    ptr.d = &cmap.d[byte_val];
    memset_arg = ptr.uc[0];
    retval = memset_arg == ptr.uc[1] && memset_arg == ptr.uc[2] &&
      memset_arg == ptr.uc[3] && memset_arg == ptr.uc[4] &&
      memset_arg == ptr.uc[5] && memset_arg == ptr.uc[6] &&
      memset_arg == ptr.uc[7];
    break;
  case XYZ_FLOAT_PIXEL:
    ptr.f = &cmap.f[byte_val];
    memset_arg = ptr.uc[0];
    retval = memset_arg == ptr.uc[1] && memset_arg == ptr.uc[2] &&
      memset_arg == ptr.uc[3] && memset_arg == ptr.uc[4] &&
      memset_arg == ptr.uc[5] && memset_arg == ptr.uc[6] &&
      memset_arg == ptr.uc[7] && memset_arg == ptr.uc[8] &&
      memset_arg == ptr.uc[9] && memset_arg == ptr.uc[10] &&
      memset_arg == ptr.uc[11];
    break;
  case XYZ_DOUBLE_PIXEL:
    ptr.d = &cmap.d[byte_val];
    memset_arg = ptr.uc[0];
    retval = memset_arg == ptr.uc[1] && memset_arg == ptr.uc[2] &&
      memset_arg == ptr.uc[3] && memset_arg == ptr.uc[4] &&
      memset_arg == ptr.uc[5] && memset_arg == ptr.uc[6] &&
      memset_arg == ptr.uc[7] && memset_arg == ptr.uc[8] &&
      memset_arg == ptr.uc[9] && memset_arg == ptr.uc[10] &&
      memset_arg == ptr.uc[11] && memset_arg == ptr.uc[12] &&
      memset_arg == ptr.uc[13] && memset_arg == ptr.uc[14] &&
      memset_arg == ptr.uc[15] && memset_arg == ptr.uc[16] &&
      memset_arg == ptr.uc[17] && memset_arg == ptr.uc[18] &&
      memset_arg == ptr.uc[19] && memset_arg == ptr.uc[20] &&
      memset_arg == ptr.uc[21] && memset_arg == ptr.uc[22] &&
      memset_arg == ptr.uc[23];
    break;
  case ARGB32_PIXEL:
    ptr.uc = &cmap.uc[byte_val*4];
    memset_arg = ptr.uc[0];
    retval = memset_arg == ptr.uc[1] && memset_arg == ptr.uc[2] &&
      memset_arg == ptr.uc[3];
    break;    
  default:
    retval = 0;
  }

  if (retval && mem_val)
    *mem_val = memset_arg;
  return retval;
}



void JPLPic::FillBox (long top_row, long top_col, long row_size,
			     long col_size, int wrap, unsigned char byte_val,
			     int just_shift)
{
  unsigned char memval = '\0';
  int can_memset_each_row = 0;
  int pixBytes = BytesPerPixel();

  if (top_row < 0) {
    row_size += top_row;
    top_row = 0;
  }

  if (top_col < 0) {
    col_size += top_col;
    top_col = 0;
  }

  if (top_col + col_size >= cols)
    col_size = cols - top_col;

  if (top_row + row_size >= rows)
    row_size = rows - top_row;

  if (col_size < 1 || row_size < 1)
    return;

  can_memset_each_row = (just_shift == 0 &&
			 ColorMapEntryRepeatsOneByte (byte_val, &memval));

  if (can_memset_each_row && col_size == cols)
    memset (GetPixelAddress (top_row, top_col), memval,
	    row_size * cols * pixBytes);
  else
    for (long row = 0; row < row_size; row++) {
      AnythingT ptr;

      ptr.uc = GetPixelAddress (top_row+row, top_col);
      if (can_memset_each_row)
	memset (ptr.uc, memval, col_size * pixBytes);
      else
	for (long col = 0; col < col_size; col++) {
    
#ifdef _GREEN_TOOL
#pragma ghs nowarning 76	/* argument to macro is empty */
#endif

	  if (ptr.uc)
	    SET_PIXEL_AND_INC (ptr, pixelType, byte_val, just_shift, );
	}
    }
}


#ifndef OMIT_IMAGE_OPERATORS

/* Number -- Write an integer into an image; num is any
   nonnegative integer, pixel is the color to use; background pixels
   will be scaled down so they're darker.  Row and column specify the
   upper left corner of the first digit.  We will have 2 pixels
   between adjacent digits.  Handle image boundaries robustly, and
   only darken the bounding box if darken_background flag is set.
   Numbers will be written in the given base (can be between 2 and
   MAX_BASE) */

void JPLPic::Number (int r, int c, long num, long base,
		     unsigned char pixel, int darken_background, int wrap,
		     int scale)
{
  /* I tried to use const int here, but the compiler doesn't accept
      them as array bounds.  Numbers may be expressed in a base from 2
      up to MAX_BASE.  MAX_BASE tells you how many entries you have in
      nums5x7data). */

#define CHAR_ROWS 7		/* Number of rows per character */
#define CHAR_COLS 5		/* Number of cols per character */
#define SKIP_COLS 2		/* Number of cols between characters */
#define MAX_BASE 20		/* Number of digits in nums5x7data table */

  /* Basic 5x7 numeric font.  To keep it legible, we store it in a
     funky order in this table, then build an index pointing properly
     into it the first time this function is called. */

  static const char *nums5x7data[CHAR_ROWS * (MAX_BASE/5)][5] = {
    { " *** ", " **  ", " *** ", " *** ", "*   *" },
    { "*   *", "* *  ", "*   *", "*   *", "*   *" },
    { "*   *", "  *  ", "   * ", "    *", "*   *" },
    { "*   *", "  *  ", "  *  ", " *** ", " ****" },
    { "*   *", "  *  ", " *   ", "    *", "    *" },
    { "*   *", "  *  ", "*    ", "*   *", "    *" },
    { " *** ", "*****", "*****", " *** ", "    *" },

    { "*****", " *** ", "*****", " *** ", " *** " },
    { "*    ", "*   *", "    *", "*   *", "*   *" },
    { "*    ", "*    ", "   * ", "*   *", "*   *" },
    { "**** ", "**** ", "  *  ", " *** ", " ****" },
    { "    *", "*   *", " *   ", "*   *", "    *" },
    { "*   *", "*   *", "*    ", "*   *", "*   *" },
    { " *** ", " *** ", "*    ", " *** ", " *** " },

    { "  *  ", "**** ", " *** ", "**** ", "*****" },
    { " * * ", "*   *", "*   *", "*   *", "*    " },
    { "*   *", "*   *", "*    ", "*   *", "*    " },
    { "*****", "**** ", "*    ", "*   *", "***  " },
    { "*   *", "*   *", "*    ", "*   *", "*    " },
    { "*   *", "*   *", "*   *", "*   *", "*    " },
    { "*   *", "**** ", " *** ", "**** ", "*****" },

    { "*****", " *** ", "*   *", "*****", "*****" },
    { "*    ", "*   *", "*   *", "  *  ", "   * " },
    { "*    ", "*    ", "*   *", "  *  ", "   * " },
    { "***  ", "* ***", "*****", "  *  ", "   * " },
    { "*    ", "*   *", "*   *", "  *  ", "   * " },
    { "*    ", "*   *", "*   *", "  *  ", "*  * " },
    { "*    ", " *** ", "*   *", "*****", " **  " },
  };

  long log_base = 0, tmp_num = 0;
  static const char *nums_font[MAX_BASE][CHAR_ROWS];
  int i, j;

  /* Initialize font table first time through */
  if (nums_font[0][0] == NULL) {
    int n, r;

    /* There are MAX_BASE+1 digits to initialize (including 0) */
    for (n = 0; n <= MAX_BASE; n++)
      for (r = 0; r < CHAR_ROWS; r++)
	nums_font[n][r] = nums5x7data[CHAR_ROWS * (n/5) + r][n%5];
  }

  /* Perform basic sanity checking */
  if (base > MAX_BASE || base < 2) {
    DBG(("Number: Impossible base %ld not in [2:%d]\n",
	     base, MAX_BASE));
    return;
  }

  if (pixelType != UC8_PIXEL && pixelType != ARGB32_PIXEL) {
    DBG(("Number: cannot handle pixelType %ld\n", pixelType));
    return;
  }

  if (num < 0) {
    DBG(("Number: cannot draw negative number %ld!\n",
	     num));
    return;
  }

  /* Count the number of additional digits that must be printed */
  log_base = 0;
  for (tmp_num = num; tmp_num >= base; tmp_num /= base)
    log_base++;

  /* Write all the low-order digits recursively (only one level deep)
     in this loop, from least to greatest */
  while (log_base > 0) {
    int this_digit = num % base;

    /* Bounds checking happens inside recursive call, so don't bother
       checking here */
    Number (r, c + log_base * (CHAR_COLS + SKIP_COLS) * scale,
		this_digit, base, pixel, darken_background, wrap, scale);
    num /= base;
    log_base--;
  }

  /* Now we just need to display one digit.  */

  /* Don't do anything if it won't even be visible */
  if (r <= -CHAR_ROWS * scale || r >= rows ||
      c <= -CHAR_COLS * scale || c >= cols) {
    return;
  }

  /* Make sure it's what we expect */
  if (num < 0 || num >= base) {
    DBG(("Number: Num %ld out of bounds [0:9]!\n", num));
    return;
  }

  /* Darken background pixels, including those just outside the bounding
     box */
  if (darken_background) {
    int min_row, max_row, min_col, max_col;

    /* Compute the part of the bounding box that lies within the image,
       so we don't need to check index values in the for loops */
    min_row = MAX (r-1, 0);
    max_row = MIN (rows-1, r + CHAR_ROWS * scale);
    min_col = MAX (c-1, 0);
    max_col = MIN (cols-1, c + CHAR_COLS * scale);

    for (i = min_row; i <= max_row; i++) {

      for (j = min_col; j <= max_col; j++) {
	unsigned char *pix_ptr = GetPixelAddress (i, j, wrap);

	if (pix_ptr) {
	  switch (pixelType) {
	  case ARGB32_PIXEL:
	    pix_ptr[1] >>= 1;
	    pix_ptr[2] >>= 1;
	    pix_ptr[3] >>= 1;
	    pix_ptr += 4;
	    break;
	  case UC8_PIXEL:
	  default:
	    *pix_ptr++ >>= 1;
	    break;
	  }
	}
      }
    }
  }

  /* Fill in the pixels required to draw this numeral */

  for (i = 0; i < CHAR_ROWS; i++)
    for (j = 0; j < CHAR_COLS; j++) {
	register char this_pix = *(nums_font[num][i]+j);

	if (this_pix != ' ')
	  for (int ii = 0; ii < scale; ii++)
	    for (int jj = 0; jj < scale; jj++) {
	      register unsigned char *pix_ptr =
		GetPixelAddress(r+i*scale+ii, c+j*scale+jj, wrap);

	      if (pix_ptr)
		switch (pixelType) {
		case ARGB32_PIXEL:
		  pix_ptr[1] = pix_ptr[2] = pix_ptr[3] = pixel;
		  break;
		case UC8_PIXEL:
		default:
		  *pix_ptr = pixel;
		  break;
		}
	    }
    }
}

/* Text -- Write 5x8 text into an image.  Pixel is the color to use;
   background pixels can be scaled down so they're darker.  Row and
   column specify the upper left corner of the first character.  We
   will have 2 pixels between adjacent letters.  Handle image
   boundaries robustly, and only darken the bounding box if
   darken_background is set. */

void JPLPic::Text (int r, int c, const char *str, unsigned char pixel,
		       int darken_background, int wrap, int scale)
{
  /* I tried to use const int here, but the compiler doesn't accept
     them as array bounds. */

#undef CHAR_ROWS
#define CHAR_ROWS 8		/* Now need 8 rows for descenders */
#define NUM_CHARS 95		/* Number of characters in ascii5x8data;
				   must be a multiple of 5 */

  /* Basic 5x7 font, with descenders (so really 5x8).  To keep it
     legible, we store it in a funky order in this table, then build
     an index pointing properly into it the first time this function
     is called. */

  const unsigned char first_char = 33;	// ASCII value of first character

  // HACK HACK -- Cannot use asciiNx8data until I store it in a class
  // variable.  Otherwise, TextBoundingBox cannot compute the actual
  // text size.  Also need to change the layout engine to make the
  // horizontal intercharacter spacing optional.
#if 0
  static const char *asciiNx8data[CHAR_ROWS * (NUM_CHARS/5)][5] = {
    
    { "  *  ", " * * ", "  * *  ", "   **   ", " **  * " },
    { " *** ", " * * ", "  * *  ", "  ***** ", " **  * " },
    { " *** ", " * * ", " ***** ", " * **   ", "    *  " },
    { " *** ", " * * ", "  * *  ", "  ****  ", "   *   " },
    { "  *  ", "     ", "  * *  ", "   ** * ", "  *    " },
    { "     ", "     ", " ***** ", " *****  ", " *  ** " },
    { " *** ", "     ", "  * *  ", "   **   ", " *  ** " },
    { "  *  ", "     ", "  * *  ", "        ", "       " },

    { "  **   ", "   * ", "   * ", " *   ", "       " },
    { " *  *  ", "  *  ", "  *  ", "  *  ", " * * * " },
    { " *  *  ", " *   ", " **  ", "  ** ", "  ***  " },
    { "  **   ", "     ", " **  ", "  ** ", " ***** " },
    { " *  ** ", "     ", " **  ", "  ** ", "  ***  " },
    { " *  *  ", "     ", "  *  ", "  *  ", " * * * " },
    { "  ** * ", "     ", "   * ", " *   ", "       " },
    { "       ", "     ", "     ", "     ", "       " },

    { "       ", "     ", "       ", "     ", "       " },
    { "   *   ", "     ", "       ", "     ", "     * " },
    { "   *   ", "     ", "       ", "     ", "    *  " },
    { " ***** ", "     ", " ***** ", "     ", "   *   " },
    { "   *   ", "     ", "       ", "     ", "  *    " },
    { "   *   ", "  ** ", "       ", "  *  ", " *     " },
    { "       ", "  ** ", "       ", " *** ", "       " },
    { "       ", " *   ", "       ", "  *  ", "       " },

    { "   **   ", "  ***   ", "  ****  ", "  ****  ", " **  ** " },
    { "  *  *  ", " ****   ", " **  ** ", " **  ** ", " **  ** " },
    { " **  ** ", "   **   ", "     ** ", "     ** ", " **  ** " },
    { " **  ** ", "   **   ", "    **  ", "  ****  ", "  ***** " },
    { " **  ** ", "   **   ", "   **   ", "     ** ", "     ** " },
    { "  *  *  ", "   **   ", "  **    ", " **  ** ", "     ** " },
    { "   **   ", " ****** ", " ****** ", "  ****  ", "     ** " },
    { "        ", "        ", "        ", "        ", "        " },

    { " ****** ", "  ****  ", " ****** ", "  ****  ", "  ****  " },
    { " **     ", " **  ** ", "     ** ", " **  ** ", " **  ** " },
    { " **     ", " **     ", "    **  ", " **  ** ", " **  ** " },
    { " *****  ", " *****  ", "   **   ", "  ****  ", "  ***** " },
    { "     ** ", " **  ** ", "  **    ", " **  ** ", "     ** " },
    { " **  ** ", " **  ** ", " **     ", " **  ** ", " **  ** " },
    { "  ****  ", "  ****  ", " *      ", "  ****  ", "  ****  " },
    { "        ", "        ", "        ", "        ", "        "  },

    { "    ", "     ", "    ** ", "       ", "  *    " },
    { "    ", "     ", "   **  ", "       ", "   *   " },
    { " ** ", "  ** ", "  **   ", " ***** ", "    *  " },
    { " ** ", "  ** ", " **    ", "       ", "    ** " },
    { "    ", "     ", "  *    ", " ***** ", "   **  " },
    { " ** ", "  ** ", "   *   ", "       ", "  **   " },
    { " ** ", "  ** ", "    *  ", "       ", " **    " },
    { "    ", " *   ", "       ", "       ", "       " },

    { "  ****  ", "  ****  ", "       **       ", " *****  ", "  ****  " },
    { " **  ** ", " **  ** ", "      * **      ", " **  ** ", " **  ** " },
    { "    **  ", " ** *** ", "     *   **     ", " **  ** ", " **     " },
    { "   **   ", " ** * * ", "    ********    ", " *****  ", " **     " },
    { "   **   ", " ** *** ", "   *       **   ", " **  ** ", " **     " },
    { "        ", " **     ", "  *         **  ", " **  ** ", " **  ** " },
    { "   **   ", "  ***** ", " **          ** ", " *****  ", "  ****  " },
    { "   **   ", "        ", "                ", "        ", "        " },

    { " ******   ", " ****** ", " ****** ", "  ****   ", " ***   ** " },
    { "  **   *  ", " **   * ", " **   * ", " **  **  ", "  **   *  " },
    { "  **    * ", " **     ", " **     ", " **      ", "  **   *  " },
    { "  **    * ", " ****   ", " ****   ", " **  *** ", "  ******  " },
    { "  **    * ", " **     ", " **     ", " **   *  ", "  **   *  " },
    { "  **   *  ", " **   * ", " **     ", " **   *  ", "  **   *  " },
    { " ******   ", " ****** ", " **     ", "  ****   ", " ***   ** " },
    { "          ", "        ", "        ", "         ", "          " },

    { " ****** ", "  ***** ", " ***   ** ", " **     ", " **       **  " },
    { "   **   ", "    **  ", "  **  *   ", " **     ", " ***     ***  " },
    { "   **   ", "    **  ", "  ** *    ", " **     ", " ** *   ** *  " },
    { "   **   ", "    **  ", "  ***     ", " **     ", " **  * **  *  " },
    { "   **   ", " ** **  ", "  ** *    ", " **     ", " **   **   *  " },
    { "   **   ", " *  **  ", "  **  *   ", " **   * ", " **        *  " },
    { " ****** ", "  ***   ", " ***   ** ", " ****** ", " **        ** " },
    { "        ", "        ", "          ", "        ", "              " },

    { "  **    ** ", "  *****  ", " *****  ", "  *****   ", " *****  " },
    { "  ***    * ", " **    * ", " **   * ", " **    *  ", " **   * " },
    { "  ** *   * ", " **    * ", " **   * ", " **    *  ", " **   * " },
    { "  **  *  * ", " **    * ", " *****  ", " **    *  ", " *****  " },
    { "  **   * * ", " **    * ", " **     ", " **  * *  ", " ** *   " },
    { "  **    ** ", " **    * ", " **     ", " **   *   ", " **  *  " },
    { " ***     * ", "  *****  ", " **     ", "  **** ** ", " **   * " },
    { "           ", "         ", "        ", "          ", "        " },

    { "  ****  ", " ******** ", " **   * ", " **      * ", " **          ** "},
    { " **  ** ", " *  **  * ", " **   * ", " **      * ", " **           * "},
    { " **     ", "    **    ", " **   * ", "  **    *  ", "  **         *  "},
    { "  ****  ", "    **    ", " **   * ", "  **    *  ", "  **    *    *  "},
    { "     ** ", "    **    ", " **   * ", "   **  *   ", "   **  **   *   "},
    { " **  ** ", "    **    ", " **   * ", "    ***    ", "    *** ** *    "},
    { "  ****  ", "    **    ", "  ****  ", "     *     ", "     *   **     "},
    { "        ", "          ", "        ", "           ", "                "},

    { " **    *  ", " **     * ", " ******* ", " **** ", "         " },
    { "  **  *   ", "  **   *  ", "     **  ", " **   ", " **      " },
    { "   ***    ", "   ** *   ", "    **   ", " **   ", "  **     " },
    { "    **    ", "    **    ", "   **    ", " **   ", "   **    " },
    { "   * **   ", "    **    ", "  **     ", " **   ", "    **   " },
    { "  *   **  ", "    **    ", " **    * ", " **   ", "     **  " },
    { " *     ** ", "    **    ", " ******* ", " **** ", "      ** " },
    { "          ", "          ", "         ", "      ", "         " },

    { " ****  ", "   *   ", "        ", " *   ", "        " },
    { "   **  ", "  ***  ", "        ", "  *  ", "        " },
    { "   **  ", " *  ** ", "        ", "   * ", "  ***   " },
    { "   **  ", "       ", "        ", "     ", " **  *  " },
    { "   **  ", "       ", "        ", "     ", "  ****  " },
    { "   **  ", "       ", "        ", "     ", " **  *  " },
    { " ****  ", "       ", "        ", "     ", " ****** " },
    { "       ", "       ", " ****** ", "     ", "        " },

    { " **     ", "        ", "      * ", "        ", "   *** " },
    { " **     ", "        ", "      * ", "        ", "  **   " },
    { " **     ", "  ****  ", "      * ", "  ****  ", "  **   " },
    { " *****  ", " **   * ", "  ***** ", " **  ** ", " ***** " },
    { " **   * ", " **     ", " **   * ", " *****  ", "  **   " },
    { " **   * ", " **   * ", " **   * ", " **     ", "  **   " },
    { " *****  ", "  ****  ", "  ******", "  ***** ", " **    " },
    { "        ", "        ", "        ", "        ", "       " },

    { "        ", " **     ", "    ", "       ", " **    " },
    { "        ", " **     ", " ** ", "    ** ", " **    " },
    { "  ****  ", " **     ", "    ", "       ", " **  * " },
    { " **  ** ", " *****  ", " ** ", "    ** ", " ** *  " },
    { " **  ** ", " **   * ", " ** ", "    ** ", " ***   " },
    { "  ***** ", " **   * ", " ** ", "    ** ", " ** *  " },
    { "     ** ", " **   * ", "  **", " *  ** ", " **  * " },
    { " *****  ", "        ", "    ", "  ***  ", "       " },

    { " **   ", "          ", "        ", "        ", "        " },
    { "  **  ", "          ", "        ", "        ", "        " },
    { "  **  ", " ** * **  ", " ** **  ", "  ****  ", " *****  " },
    { "  **  ", " *** *  * ", " ***  * ", " **   * ", " **   * " },
    { "  **  ", " **  *  * ", " **   * ", " **   * ", " **   * " },
    { "  **  ", " **  *  * ", " **   * ", " **   * ", " *****  " },
    { " **** ", " **  *  * ", " **   * ", "  ****  ", " **     " },
    { "      ", "          ", "        ", "        ", " **     " },

    { "        ", "        ", "       ", "       ", "         " },
    { "        ", "        ", "       ", "  **   ", "         " },
    { "  ****  ", " ** **  ", "  **** ", " ***** ", " **  **  " },
    { " *   ** ", " ***  * ", " **    ", "  **   ", " **  **  " },
    { " *   ** ", " **     ", "  ***  ", "  **   ", " **  **  " },
    { "  ***** ", " **     ", "    ** ", "  **   ", " **  **  " },
    { "     ** ", " **     ", " ***** ", "   *** ", "  *** ** " },
    { "     ** ", "        ", "       ", "       ", "         " },

    { "        ", "            ", "        ", "       ", "        " },
    { "        ", "            ", "        ", "       ", "        " },
    { " **   * ", " **       * ", " **   * ", " *  ** ", " ****** " },
    { " **   * ", " **   *   * ", "  ** *  ", " *  ** ", "    **  " },
    { "  ** *  ", "  ** *** *  ", "   **   ", " *  ** ", "   **   " },
    { "  ** *  ", "  ***  ***  ", "  * **  ", "  **** ", "  **    " },
    { "   **   ", "   **   *   ", " *   ** ", "    ** ", " ****** " },
    { "        ", "            ", "        ", "  ***  ", "        " },

    { "    **  ", "   **   ", "  **    ", "  **  * ", "   " },
    { "  **    ", "   **   ", "    **  ", " * ***  ", "   " },
    { "  **    ", "   **   ", "    **  ", "        ", "   " },
    { " **     ", "   **   ", "     ** ", "        ", "   " },
    { "  **    ", "   **   ", "    **  ", "        ", "   " },
    { "  **    ", "   **   ", "    **  ", "        ", "   " },
    { "    **  ", "   **   ", "  **    ", "        ", "   " },
    { "        ", "        ", "        ", "        ", "   " },
};
#endif

  static const char *ascii5x8data[CHAR_ROWS * (NUM_CHARS/5)][5] = {

    { "  *  ", " * * ", " * * ", "  *  ", "**  *" },
    { "  *  ", " * * ", "*****", " ****", "**  *" },
    { "  *  ", " * * ", " * * ", "* *  ", "   * " },
    { "  *  ", "     ", " * * ", " *** ", "  *  " },
    { "  *  ", "     ", "*****", "  * *", " *   " },
    { "     ", "     ", " * * ", "**** ", "*  **" },
    { "  *  ", "     ", "     ", "  *  ", "*  **" },
    { "     ", "     ", "     ", "     ", "     " },

    { " **  ", "   * ", "   * ", " *   ", "     " },
    { "*  * ", "  *  ", "  *  ", "  *  ", "* * *" },
    { "*  * ", " *   ", " *   ", "   * ", " *** " },
    { " **  ", "     ", " *   ", "   * ", "*****" },
    { "*  **", "     ", " *   ", "   * ", " *** " },
    { "*  * ", "     ", "  *  ", "  *  ", "* * *" },
    { " ** *", "     ", "   * ", " *   ", "     " },
    { "     ", "     ", "     ", "     ", "     " },

    { "     ", "     ", "     ", "     ", "     " },
    { "  *  ", "     ", "     ", "     ", "    *" },
    { "  *  ", "     ", "     ", "     ", "   * " },
    { "*****", "     ", "*****", "     ", "  *  " },
    { "  *  ", "     ", "     ", "     ", " *   " },
    { "  *  ", "  ** ", "     ", "  *  ", "*    " },
    { "     ", "  ** ", "     ", " *** ", "     " },
    { "     ", " *   ", "     ", "  *  ", "     " },

    { "  *  ", " **  ", " *** ", " *** ", "*   *" },
    { " * * ", "* *  ", "*   *", "*   *", "*   *" },
    { "*   *", "  *  ", "   * ", "    *", "*   *" },
    { "*   *", "  *  ", "  *  ", " *** ", " ****" },
    { "*   *", "  *  ", " *   ", "    *", "    *" },
    { " * * ", "  *  ", "*    ", "*   *", "    *" },
    { "  *  ", "*****", "*****", " *** ", "    *" },
    { "     ", "     ", "     ", "     ", "     " },

    { "*****", " *** ", "*****", " *** ", " *** " },
    { "*    ", "*   *", "    *", "*   *", "*   *" },
    { "*    ", "*    ", "   * ", "*   *", "*   *" },
    { "**** ", "**** ", "  *  ", " *** ", " ****" },
    { "    *", "*   *", " *   ", "*   *", "    *" },
    { "*   *", "*   *", "*    ", "*   *", "*   *" },
    { " *** ", " *** ", "*    ", " *** ", " *** " },
    { "     ", "     ", "     ", "     ", "     " },

    { "     ", "     ", "   * ", "     ", " *   " },
    { "     ", "     ", "  *  ", "     ", "  *  " },
    { "  ** ", "  ** ", " *   ", "*****", "   * " },
    { "  ** ", "  ** ", "*    ", "     ", "    *" },
    { "     ", "     ", " *   ", "*****", "   * " },
    { "  ** ", "  ** ", "  *  ", "     ", "  *  " },
    { "  ** ", "  ** ", "   * ", "     ", " *   " },
    { "     ", " *   ", "     ", "     ", "     " },

    { " *** ", " *** ", "  *  ", "**** ", " *** " },
    { "*   *", "*   *", " * * ", "*   *", "*   *" },
    { "    *", "* ***", "*   *", "*   *", "*    " },
    { "  ** ", "* * *", "*****", "**** ", "*    " },
    { "  *  ", "* ***", "*   *", "*   *", "*    " },
    { "     ", "*    ", "*   *", "*   *", "*   *" },
    { "  *  ", " *** ", "*   *", "**** ", " *** " },
    { "     ", "     ", "     ", "     ", "     " },

    { "**** ", "*****", "*****", " *** ", "*   *" },
    { "*   *", "*    ", "*    ", "*   *", "*   *" },
    { "*   *", "*    ", "*    ", "*    ", "*   *" },
    { "*   *", "***  ", "***  ", "* ***", "*****" },
    { "*   *", "*    ", "*    ", "*   *", "*   *" },
    { "*   *", "*    ", "*    ", "*   *", "*   *" },
    { "**** ", "*****", "*    ", " *** ", "*   *" },
    { "     ", "     ", "     ", "     ", "     " },

    { "*****", "*****", "*   *", "*    ", "*   *" },
    { "  *  ", "   * ", "*  * ", "*    ", "** **" },
    { "  *  ", "   * ", "* *  ", "*    ", "** **" },
    { "  *  ", "   * ", "**   ", "*    ", "* * *" },
    { "  *  ", "   * ", "* *  ", "*    ", "*   *" },
    { "  *  ", "*  * ", "*  * ", "*    ", "*   *" },
    { "*****", " **  ", "*   *", "*****", "*   *" },
    { "     ", "     ", "     ", "     ", "     " },

    { "*   *", " *** ", "**** ", " *** ", "**** " },
    { "**  *", "*   *", "*   *", "*   *", "*   *" },
    { "**  *", "*   *", "*   *", "*   *", "*   *" },
    { "* * *", "*   *", "**** ", "*   *", "**** " },
    { "*  **", "*   *", "*    ", "* * *", "* *  " },
    { "*  **", "*   *", "*    ", "*  * ", "*  * " },
    { "*   *", " *** ", "*    ", " ** *", "*   *" },
    { "     ", "     ", "     ", "     ", "     " },

    { " *** ", "*****", "*   *", "*   *", "*   *" },
    { "*   *", "  *  ", "*   *", "*   *", "*   *" },
    { "*    ", "  *  ", "*   *", "*   *", "*   *" },
    { " *** ", "  *  ", "*   *", " * * ", "* * *" },
    { "    *", "  *  ", "*   *", " * * ", "* * *" },
    { "*   *", "  *  ", "*   *", " * * ", "** **" },
    { " *** ", "  *  ", " *** ", "  *  ", " * * " },
    { "     ", "     ", "     ", "     ", "     " },

    { "*   *", "*   *", "*****", " *** ", "     " },
    { "*   *", "*   *", "    *", " *   ", "*    " },
    { " * * ", " * * ", "   * ", " *   ", " *   " },
    { "  *  ", "  *  ", "  *  ", " *   ", "  *  " },
    { " * * ", "  *  ", " *   ", " *   ", "   * " },
    { "*   *", "  *  ", "*    ", " *   ", "    *" },
    { "*   *", "  *  ", "*****", " *** ", "     " },
    { "     ", "     ", "     ", "     ", "     " },

    { " *** ", "  *  ", "     ", " *   ", "     " },
    { "   * ", " * * ", "     ", "  *  ", "     " },
    { "   * ", "*   *", "     ", "   * ", " *** " },
    { "   * ", "     ", "     ", "     ", "*  * " },
    { "   * ", "     ", "     ", "     ", " *** " },
    { "   * ", "     ", "     ", "     ", "*  * " },
    { " *** ", "     ", "     ", "     ", "*****" },
    { "     ", "     ", "*****", "     ", "     " },

    { "*    ", "     ", "    *", "     ", "  ***" },
    { "*    ", "     ", "    *", "     ", " *   " },
    { "*    ", " *** ", "    *", " *** ", " *   " },
    { "**** ", "*   *", " ****", "*   *", "**** " },
    { "*   *", "*    ", "*   *", "**** ", " *   " },
    { "*   *", "*   *", "*   *", "*    ", " *   " },
    { "**** ", " *** ", " ****", " ****", " *   " },
    { "     ", "     ", "     ", "     ", "     " },

    { "     ", "*    ", "     ", "     ", " *   " },
    { "     ", "*    ", "  *  ", "    *", " *   " },
    { " *** ", "*    ", "     ", "     ", " *  *" },
    { "*   *", "**** ", "  *  ", "    *", " * * " },
    { "*   *", "*   *", "  *  ", "    *", " **  " },
    { " ****", "*   *", "  *  ", "    *", " * * " },
    { "    *", "*   *", "  *  ", " *  *", " *  *" },
    { "**** ", "     ", "     ", "  ** ", "     " },

    { " **  ", "     ", "     ", "     ", "     " },
    { "  *  ", "     ", "     ", "     ", "     " },
    { "  *  ", "** * ", "* *  ", " *** ", "**** " },
    { "  *  ", "* * *", "** * ", "*   *", "*   *" },
    { "  *  ", "* * *", "*  * ", "*   *", "*   *" },
    { "  *  ", "* * *", "*  * ", "*   *", "**** " },
    { " *** ", "* * *", "*  * ", " *** ", "*    " },
    { "     ", "     ", "     ", "     ", "*    " },

    { "     ", "     ", "     ", "     ", "     " },
    { "     ", "     ", "     ", "  *  ", "     " },
    { " *** ", "* ** ", " ****", " *** ", "*  * " },
    { "*   *", "**  *", "*    ", "  *  ", "*  * " },
    { "*   *", "*    ", " *** ", "  *  ", "*  * " },
    { " ****", "*    ", "    *", "  *  ", "*  * " },
    { "    *", "*    ", "**** ", "   **", " ** *" },
    { "    *", "     ", "     ", "     ", "     " },

    { "     ", "     ", "     ", "     ", "     " },
    { "     ", "     ", "     ", "     ", "     " },
    { "*   *", "* * *", "*   *", " *  *", "*****" },
    { "*   *", "* * *", " * * ", " *  *", "   * " },
    { " * * ", "* * *", "  *  ", " *  *", "  *  " },
    { " * * ", "** **", " * * ", "  ***", " *   " },
    { "  *  ", " * * ", "*   *", "    *", "*****" },
    { "     ", "     ", "     ", "  ** ", "     " },

    { "  ** ", "  *  ", " **  ", " *  *", "     " },
    { " *   ", "  *  ", "   * ", "* ** ", "     " },
    { " *   ", "  *  ", "   * ", "     ", "     " },
    { "*    ", "  *  ", "    *", "     ", "     " },
    { " *   ", "  *  ", "   * ", "     ", "     " },
    { " *   ", "  *  ", "   * ", "     ", "     " },
    { "  ** ", "  *  ", " **  ", "     ", "     " },
    { "     ", "     ", "     ", "     ", "     " },
};

  static const char *ascii_font[NUM_CHARS][CHAR_ROWS] = {{0}};
  int i, j;
  int start_col = c;

  // Initialize the font table the first time through

  if (ascii_font[0][0] == NULL) {
    int ch, r;

    for (ch = 0; ch < NUM_CHARS; ch++)
      for (r = 0; r < CHAR_ROWS; r++)
	ascii_font[ch][r] = ascii5x8data[CHAR_ROWS * (ch/5) + r][ch%5];
  }

  // Perform basic sanity checking

  if (str == NULL)
    return;

  if (!IS_INT_PIXEL (pixelType) && pixelType != ARGB32_PIXEL) {
    DBG(("Text: cannot handle pixelType %ld\n", pixelType));
    return;
  }

  // Iterate over the entire string

  for (; *str; str++, c += (CHAR_COLS + SKIP_COLS) * scale) {

    // Display one character

    // Don't do anything if it won't even be visible
    if (r <= -CHAR_ROWS * scale || r >= rows ||
	c <= -CHAR_COLS * scale || c >= cols) {
      if (*str == '\n') {
	c = start_col - (CHAR_COLS + SKIP_COLS) * scale;
	r += scale * (CHAR_ROWS+2);
      }
      continue;
    }

    // Darken background pixels, including those just outside the
    // bounding box
    if (darken_background) {
      int min_row, max_row, min_col, max_col;
      
      // Compute the part of the bounding box that lies within the
      // image, so we don't need to check index values in the for
      // loops
      
      min_row = MAX (r-1, 0);
      max_row = MIN (rows-1, r + CHAR_ROWS * scale);
      min_col = MAX ((c) - ((c != start_col) ? (SKIP_COLS * scale-1) : 1), 0);
      max_col = MIN (cols-1, c + CHAR_COLS * scale);
      
      for (i = min_row; i <= max_row; i++) {
	for (j = min_col; j <= max_col; j++) {
	  unsigned char *pix_ptr = GetPixelAddress (i, j, wrap);
	
	  if (pix_ptr) {
	    switch (pixelType) {
	    case ARGB32_PIXEL:
	      pix_ptr[1] >>= 1;
	      pix_ptr[2] >>= 1;
	      pix_ptr[3] >>= 1;
	      pix_ptr += 4;
	      break;
	    case INT32_PIXEL:
	      *((unsigned int *) pix_ptr) >>= 1;
	      break;
	    case INT16_PIXEL:
	      *((unsigned short *) pix_ptr) >>= 1;
	      break;
	    case UC8_PIXEL:
	    default:
	      *pix_ptr++ >>= 1;
	      break;
	    }
	  }
	}
      }
    }

    // Fill in the pixels required to draw this character; else leave it
    // blank

    unsigned char this_c = *((unsigned const char *) str);

    if (this_c >= first_char && this_c < first_char + NUM_CHARS)
      for (i = 0; i < CHAR_ROWS; i++)
	for (j = 0; j < CHAR_COLS; j++) {
	  register char this_pix = *(ascii_font[this_c-first_char][i]+j);

	  if (this_pix != ' ')
	    for (int ii = 0; ii < scale; ii++)
	      for (int jj = 0; jj < scale; jj++) {
	      
		SetPixel (r+i*scale+ii, c+j*scale+jj, wrap, pixel, 0);
	      } // for jj
	} // for j
    else if (this_c == '\n') {
      c = start_col - (CHAR_COLS + SKIP_COLS) * scale;
      r += scale * (CHAR_ROWS+2);
    }
  }
} // JPLPic::Text



void JPLPic::TextBoundingBox (const char *str, int scale,
			      int *trows, int *tcols)
{
  int max_len = 0, this_len = 0;
  int out_rows = 1;
  const char *ptr;

  for (ptr = str; ptr && *ptr; ptr++) {
    if (*ptr == '\n') {
      out_rows++;
      this_len = 0;
    } else
      this_len++;
    if (this_len > max_len)
      max_len = this_len;
  }

  if (trows) {
    *trows = scale * (max_len ? (CHAR_ROWS+2) * out_rows : 0);
  }
  if (tcols) {
    *tcols = scale * max_len * (CHAR_COLS+2);
  }
} // JPLPic::TextBoundingBox


#undef CHAR_ROWS
#undef CHAR_COLS
#undef SKIP_COLS
#undef MAX_BASE


#endif /* ! OMIT_IMAGE_OPERATORS */
