/******************************************************************************
*                                                                             *
*                                     L O S                                   *
*                                                                             *
*                                       Todd Litwin                           *
*                                       Written: 23 Aug 1995                  *
*                                       Updated:  8 Nov 2003                  *
*                                                                             *
*                                       Copyright (C) 1995, 1999, 2002, 2003  *
*                                       California Institute of Technology    *
*                                       All Rights Reserved                   *
*                                                                             *
*******************************************************************************

	This file contains functions to find line-of-sight visible pixels
	surrounding a point. */

// Modified 11/06/03 Jack Morrison for height map hole filling

enum { SUCCESS = 1, FAILURE = 0 };

#define abs(x) (((x) >= 0) ? (x) : -(x))
#define sgn(x) (((x) > 0) ? 1 : (((x) < 0) ? -1 : 0))

#define x_0 0		// upper-left region of interest
#define y_0 0

static int			x_dim;		/* size of X dimension */
static int			x_1;		/* right-most X coordinate */
static int			y_1;		/* bottom-most Y coordinate */
static int			x_center;	/* central X coordinate */
static int			y_center;	/* central Y coordinate */
static const unsigned char *	p_center;	/* pointer to center position */
static int			x_current;	/* X of current search coord */
static int			y_current;	/* Y of current search coord */
static int			half_way;	/* at least half way around */


/******************************************************************************
********************************   LOS_NEXT   *********************************
*******************************************************************************

    This function finds the next pixel in the search. It returns FAILURE after
    finding the first point for a second time. */

int los_next(
    int *xout,			/* output X coordinate of first point found */
    int *yout)			/* output Y coordinate of first point found */
{
    int d, x, y, dx, dy, ax, ay, sx, sy, sxdim;
    const unsigned char *p;

    /* Search in a clockwise direction until the next point is found */
    for (;;) {

	/* Move over clockwise to the next potential target point */
	dx = y_center  - y_current;
	dy = x_current - x_center;
	ax = abs(dx);
	ay = abs(dy);
	sx = sgn(dx);
	sy = sgn(dy);
	if (ax >= ay) {
	    dx = sx;
	    dy = (ay >= ax/2) ? sy : 0;
	    }
	else {
	    dy = sy;
	    dx = (ax >= ay/2) ? sx : 0;
	    }
	x_current += dx;
	y_current += dy;

	/* See if we have come full circle */
	if (half_way) {
	    if (x_current >= x_center)
	    	return FAILURE;
	    }
	else if (x_current < x_center)
	    half_way = 1;

	/* Start at center and search outward through current point, */
	/* using Bresenham's line algorithm for choosing the pixels  */
	dx = x_current - x_center;
	dy = y_current - y_center;
	ax = abs(dx) << 1;
	ay = abs(dy) << 1;
	sx = sgn(dx);
	sy = sgn(dy);
	x  = x_center;
	y  = y_center;
	p  = p_center;
	sxdim = sy * x_dim;
	if (ax > ay) {	/* X dominant */
	    d = ay - (ax >> 1);
	    for (;;) {
		if (d >= 0) {
		    y += sy;
		    p += sxdim;
		    d -= ax;
		    }
		x += sx;
		p += sx;
		d += ay;
		if ((x < x_0) || (x > x_1) || (y < y_0) || (y > y_1))
		    break;
		if (*p) {
		    *xout = x_current = x;
		    *yout = y_current = y;
		    return SUCCESS;
		    }
		}
	    }
	else {		/* Y dominant */
	    d = ax - (ay >> 1);
	    for (;;) {
		if (d >= 0) {
		    x += sx;
		    p += sx;
		    d -= ay;
		    }
		y += sy;
		p += sxdim;
		d += ax;
		if ((x < x_0) || (x > x_1) || (y < y_0) || (y > y_1))
		    break;
		if (*p) {
		    *xout = x_current = x;
		    *yout = y_current = y;
		    return SUCCESS;
		    }
		}
	    }
	}
    }


/******************************************************************************
********************************   LOS_START   ********************************
*******************************************************************************

    This function begins the search for the nearest pixels to a point which
    falls within the given range, and which are line-of-sight visible to the
    point. Call this function once, followed by los_next(). The function
    returns FAILURE if there are no points to find. */

int los_start(
    const unsigned char *image,	/* input image: 0=hole, 1=valid */
    int xdim,			/* input size of X dimension */
    int ydim,			/* input size of Y dimension */
    int x,			/* input central X coordinate */
    int y,			/* input central Y coordinate */
    int *xout,			/* output X coordinate of first point found */
    int *yout)			/* output Y coordinate of first point found */
{
    const unsigned char *p;

    /* Store the relevant status data */
    x_dim	= xdim;
    x_1		= x_0 + x_dim - 1;
    y_1		= y_0 + ydim - 1;
    x_center	= x;
    y_center	= y;

    /* Note the starting point */
    p_center  = image + (y_center * x_dim) + x_center;
    x_current = x_center;
    y_current = y_center;
    half_way  = 0;

    /* Look straight up for the first point */
    y_current--;
    p = p_center - x_dim;
    while (y_current >= y_0) {
	if (*p) {
	    *xout = x_current;
	    *yout = y_current;
	    return SUCCESS;
	    }
	y_current--;
	p -= x_dim;
	}

    /* If not found, then pretend one was found, and hunt for next */
    return los_next(xout, yout);
    }

