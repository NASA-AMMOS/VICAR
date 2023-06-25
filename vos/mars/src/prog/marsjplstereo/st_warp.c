/******************************************************************************
*                                                                             *
*                                 S T _ W A R P                               *
*                                                                             *
*                                       Todd Litwin                           *
*                                       Written: 27 Oct 1993                  *
*                                       Updated: 12 Aug 2002                  *
*                                                                             *
*                                       Copyright (C) 1993, 1995, 1996, 1998, *
*                                                     2001, 2002              *
*                                       California Institute of Technology    *
*                                       All Rights Reserved                   *
*                                                                             *
*******************************************************************************


	This file contains functions to perform image warping. Both
	integer (nearest-neighbor) and floating-point (bilinear
	interpolation) are supported forms of warping. Warping is
	performed in two stages. The first stage is to compute a
	map of warping addresses; this is time-consuming, and should
	be done once, probably at program initialization. The second
	step is to use the map to perform warping; this step may be
	repeated as often as desired. For integer warping, these two
	steps are performed with the functions

		st_warp_nearest_init_cahvor()
		st_warp_nearest()

	and for floating-point warping, they are performed with the
	functions

		st_warp_bilinear_init_cahvor()
		st_warp_bilinear()

	Well, actually, a lot more functions are now in here, but I
	haven't gotten around to listing up top, yet.

	*/


#include <stdio.h>
#include <math.h>	/* sqrt() */
#include "st_warp.h"
#if defined(__unix__) || defined(__APPLE__)
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#endif

#ifdef __STDC__
#include <unistd.h>
#else
extern long lseek();
#endif
#include "cahvor.h"
#define SUCCESS CAHVOR_SUCCESS

#include "stereo.h"

#ifndef PMODE
#define PMODE 0666
#endif

#define ADDR_OVERFLOW	-1
#define ADDR_UNDERFLOW	-2

#define WARP_INT	1
#define WARP_DOUBLE	2
#define WARP_I24F44	3
#define WARP_I24F44C	4
#define WARP_FLOAT	5

/******************************************************************************
********************************   ST_WARP_BILINEAR   *************************
*******************************************************************************

    This function uses a previously defined warping map to warp an image. */

int st_warp_bilinear(warpmap, xdim, ydim, x0, y0, nx, ny, image0, image1)
char *warpmap;		/* input warping map */
int xdim;		/* input size in X dimension */
int ydim;		/* input size in Y dimension */
int x0;			/* input left-most X coordinate */
int y0;			/* input upper-most Y coordinate */
int nx;			/* input number of columns */
int ny;			/* input number of rows */
unsigned char *image0;	/* input image to warp */
unsigned char *image1;	/* output warped image */
{
    int ix, iy, row_offset, start_offset;
    warp_float_t *warp;
    int intx, inty, intoff;
    float *addrx, *addry, fracx, fracy;
    unsigned char i00, i01, i10, i11;

#ifdef WARP_DEBUG
    float ixsum = 0, ixsumsq = 0, ixcount = 0;
    float iysum = 0, iysumsq = 0, iycount = 0;
    float fxsum = 0, fxsumsq = 0, fxcount = 0;
    float fysum = 0, fysumsq = 0, fycount = 0;

    printf ("=== st_warp_bilinear (0x%x, %dx%d overall, (%d,%d) upper left\n",
	    (void *) warpmap, xdim, ydim, x0, y0);
    printf ("===     %dx%d cols/rows, 0x%x input, 0x%x output\n",
	    nx, ny, image0, image1);
#endif

    /* Make sure that the map is the correct size */
    warp = (warp_float_t *)warpmap;
    if ((warp->datatype != WARP_FLOAT) ||
	(warp->xdim != xdim) || (warp->ydim != ydim))
	return FAILURE;

    /* Compute how far we advance between rows */
    row_offset = xdim - nx;

    /* Move to the beginning of the first row to process horizontally */
    start_offset = (xdim * y0) + x0;
    image1 += start_offset;
    addrx = warp->addr + start_offset;
    addry = warp->addr + start_offset + (xdim * ydim);

    /* Process each row */
    for (iy=0; iy<ny; iy++) {

	/* Process each column in the row */
	for (ix=0; ix<nx; ix++,image1++,addrx++,addry++) {

	    /* Check for out-of-bounds error */
	    if ((*addrx < 0) || (*addry < 0)) {
		*image1 = 0;
		continue;
		}

	    /* Get the address of the upper, left pixel */
	    intx = (int)*addrx;
	    inty = (int)*addry;

	    /* Compute the fractional addresses */
	    fracx = *addrx - intx;
	    fracy = *addry - inty;

	    /* Check for boundary cases */
	    if (intx == xdim - 1) {
		intx  = xdim - 2;
		fracx = 1.0;
		}
	    if (inty == ydim - 1) {
		inty  = ydim - 2;
		fracy = 1.0;
		}

#ifdef WARP_DEBUG
	    /* Keep stats on the offsets used */

	    ixsum   += (float) intx;
	    ixsumsq += (float) intx * intx;
	    ixcount += 1.0;
	    iysum   += (float) inty;
	    iysumsq += (float) inty * inty;
	    iycount += 1.0;
	    fxsum   += fracx;
	    fxsumsq += fracx * fracx;
	    fxcount += 1.0;
	    fysum   += fracy;
	    fysumsq += fracy * fracy;
	    fycount += 1.0;
#endif

	    /* Get the intensity values of the four nearest neighbors */
	    intoff = intx + (xdim * inty);
	    i00 = image0[intoff];
	    i01 = image0[intoff + 1];
	    i10 = image0[intoff + xdim];
	    i11 = image0[intoff + xdim + 1];

	    /* Compute the target intensity using a bilinear interpolation */
	    *image1 =
		(((i00 * (1.0 - fracx)) + (i01 * fracx)) * (1.0 - fracy)) +
		(((i10 * (1.0 - fracx)) + (i11 * fracx)) * fracy);
	    }

	/* Move to the start of the next row */
	image1 += row_offset;
	addrx  += row_offset;
	addry  += row_offset;
	}

#ifdef WARP_DEBUG
#define SHOW_STATS(sum,sq,count,label) \
   if (count > 0) { \
     float mean = sum / count, var = sq / count - mean * mean; \
     float stddev = (var > 0) ? sqrt(var) : var; \
     printf ("  === %s :  %f +/- %f   (%f values)\n", label, mean, stddev, count); }

    SHOW_STATS(ixsum, ixsumsq, ixcount, "Integer X Offset");
    SHOW_STATS(iysum, iysumsq, iycount, "Integer Y Offset");
    SHOW_STATS(fxsum, fxsumsq, fxcount, "Float X Offset  ");
    SHOW_STATS(fysum, fysumsq, fycount, "Float Y Offset  ");
#endif

    return SUCCESS;
    }



/******************************************************************************
********************************   ST_WARP_BILINEAR_FIELD   *******************
*******************************************************************************

    This function is just like st_warp_bilinear(), except that it operates on
    odd and even fields separately. */

int st_warp_bilinear_field(warpmap, xdim, ydim, x0, y0, nx, ny, image0, image1)
char *warpmap;		/* input warping map */
int xdim;		/* input size in X dimension */
int ydim;		/* input size in Y dimension */
int x0;			/* input left-most X coordinate */
int y0;			/* input upper-most Y coordinate */
int nx;			/* input number of columns */
int ny;			/* input number of rows */
unsigned char *image0;	/* input image to warp */
unsigned char *image1;	/* output warped image */
{
    int ix, iy, row_offset, start_offset;
    warp_float_t *warp;
    int intx, inty, intoff;
    float *addrx, *addry, fracx, fracy;
    unsigned char i00, i01, i10, i11, ipix;

    /* Make sure that the map is the correct size */
    warp = (warp_float_t *)warpmap;
    if ((warp->datatype != WARP_FLOAT) ||
	(warp->xdim != xdim) || (warp->ydim != ydim))
	return FAILURE;

    /* Compute how far we advance between rows */
    row_offset = xdim - nx;

    /* Move to the beginning of the first row to process horizontally */
    start_offset = (xdim * y0) + x0;
    image1 += start_offset;
    addrx = warp->addr + start_offset;
    addry = warp->addr + start_offset + (xdim * ydim);

    /* Process each row */
    for (iy=0; iy<ny; iy++) {

	/* Process each column in the row */
	for (ix=0; ix<nx; ix++,image1++,addrx++,addry++) {

	    /* Check for out-of-bounds error */
	    if ((*addrx < 0) || (*addry < 0)) {
		*image1 = 0;
		continue;
		}

	    /* Get the address of the upper, left pixel */
	    intx = (int)*addrx;
	    inty = (int)*addry;

	    /* Compute the fractional addresses */
	    fracx = *addrx - intx;
	    fracy = *addry - inty;

	    /* Check for boundary cases */
	    if (intx == xdim - 1) {
		intx  = xdim - 2;
		fracx = 1.0;
		}
	    if (inty == ydim - 1) {
		inty  = ydim - 2;
		fracy = 1.0;
		}
	    /*...
	    if (intx == xdim - 2) {
		intx  = xdim - 3;
		fracx += 1.0;
		}
	    ...*/
	    if (inty == ydim - 2) {
		inty  = ydim - 3;
		fracy += 1.0;
		}

	    /* Check for being on the correct field */
	    if ((inty & 1) != ((y0+iy) & 1)) {
		inty--;
		fracy += 1.0;
		if (inty == -1) {
		    inty  = 1;
		    fracy = 0.0;
		    }
		}

	    /* Adjust bilinear weighting for skipping rows */
	    fracy /= 2.0;
	    if (fracy > 1.0)
		fracy = 1.0;

	    /* Get the intensity values of the four nearest usable neighbors */
	    intoff = intx + (xdim * inty);
	    i00 = image0[intoff];
	    i01 = image0[intoff + 1];
	    i10 = image0[intoff + 2*xdim];
	    i11 = image0[intoff + 2*xdim + 1];

	    /* Compute the target intensity using a bilinear interpolation */
	    /* SunOS's cc has trouble with this...
	    *image1 =
		(((i00 * (1.0 - fracx)) + (i01 * fracx)) * (1.0 - fracy)) +
		(((i10 * (1.0 - fracx)) + (i11 * fracx)) * fracy);
	    ...*/
	    ipix  = (((i00 * (1.0 - fracx)) + (i01 * fracx)) * (1.0 - fracy));
	    ipix += (((i10 * (1.0 - fracx)) + (i11 * fracx)) * fracy);
	    *image1 = ipix;
	    }

	/* Move to the start of the next row */
	image1 += row_offset;
	addrx  += row_offset;
	addry  += row_offset;
	}

    return SUCCESS;
    }


#ifndef MER

/******************************************************************************
********************************   ST_WARP_BILINEAR_INIT_CAHVOR   *************
*******************************************************************************

    This function initializes for image warping from a CAHVOR-modeled image
    to a CAHV-modeled image using float-precision bilinear-interpolation
    mapping. The memory for the output map is allocated internally, and it
    is the responsibility of the caller to free the map when it is no longer
    needed. */

int st_warp_bilinear_init_cahvor(cahvor, cahv, xdim, ydim, warpmap)
st_cahvor_t *cahvor;	/* input CAHVOR camera model warping from */
st_cahv_t *cahv;	/* input CAHV camera model warping to */
int xdim;		/* input size in X dimension */
int ydim;		/* input size in Y dimension */
char **warpmap;		/* output warping map */
{
    union { char *c; warp_float_t *f; } u;
    unsigned neededsize;
    int ix, iy;
    float *addrx, *addry, xlim, ylim;
    warp_float_t *warp;
    double pos[2];

    /* Allocate the memory for the map */
    neededsize = sizeof(warp_float_t) + (2 * sizeof(float) * (xdim * ydim - 1));
    if ((warp = (warp_float_t *)malloc(neededsize)) == NULL)
      return FAILURE;
    u.f = warp;
    *warpmap = u.c;
    warp->datatype = WARP_FLOAT;
    warp->xdim = xdim;
    warp->ydim = ydim;

    /* Process each row */
    addrx = warp->addr;
    addry = warp->addr + (xdim * ydim);
    xlim = xdim - 1;
    ylim = ydim - 1;
    for (iy=0; iy<ydim; iy++) {

	/* Process each column in the row */
	for (ix=0; ix<xdim; ix++,addrx++,addry++) {

	    /* Compute the warped address for this point */
	    pos[0] = ix;
	    pos[1] = iy;
	    cmod_cahvor_warp_from_cahv(
		cahv->c, cahv->a, cahv->h, cahv->v,
		pos, FALSE,
		cahvor->c, cahvor->a, cahvor->h, cahvor->v,
		cahvor->o, cahvor->r,
		pos
		);

	    /* Clip to the image boundaries */
	    if (pos[0] < 0)
		pos[0] = ADDR_UNDERFLOW;	/* was: 0 */
	    if (pos[1] < 0)
		pos[1] = ADDR_UNDERFLOW;	/* was: 0 */
	    if (pos[0] > xlim)
		pos[0] = ADDR_OVERFLOW;		/* was: xlim */
	    if (pos[1] > ylim)
		pos[1] = ADDR_OVERFLOW;		/* was: ylim */

	    /* Record the address */
	    *addrx = pos[0];
	    *addry = pos[1];
	    }
	}

    return SUCCESS;
    }

#endif /* MER */

/******************************************************************************
********************************   ST_WARP_BILINEAR_INIT_CAHVORE   ************
*******************************************************************************

    This function initializes for image warping from a CAHVORE-modeled image
    to a CAHV-modeled image using float-precision bilinear-interpolation
    mapping. The memory for the output map must be passed in via the warpmap
    parameter (which for historical reasons is a pointer to the buffer pointer
    instead of just the buffer pointer). */

int st_warp_bilinear_init_cahvore(cahvore, cahv, rdist, xdim, ydim, warpmap,
				  mapsize)
st_cahvore_t *cahvore;	/* input CAHVORE camera model warping from */
st_cahv_t *cahv;	/* input CAHV camera model warping to */
double rdist;		/* input radial distance to project */
int xdim;		/* input size in X dimension */
int ydim;		/* input size in Y dimension */
char **warpmap;		/* output warping map */
unsigned int mapsize;	/* size of pre-allocated map */
{
    union { char *c; warp_float_t *f; } u;
    unsigned neededsize;
    int ix, iy;
    float *addrx, *addry, xlim, ylim;
    warp_float_t *warp;
    double pos[2];

    /* Allocate the memory for the map */
    neededsize = sizeof(warp_float_t) + (2 * sizeof(float) * (xdim * ydim - 1));
    if (neededsize > mapsize || warpmap == NULL || *warpmap == NULL)
      return FAILURE;
    /*    if ((warp = (warp_float_t *)malloc(neededsize)) == NULL)
	return FAILURE;
	*warpmap = (char *)warp;*/
    u.c = *warpmap;
    warp = u.f;
    warp->datatype = WARP_FLOAT;
    warp->xdim = xdim;
    warp->ydim = ydim;

    /* Process each row */
    addrx = warp->addr;
    addry = warp->addr + (xdim * ydim);
    xlim = xdim - 1;
    ylim = ydim - 1;
    for (iy=0; iy<ydim; iy++) {

	/* Process each column in the row */
	for (ix=0; ix<xdim; ix++,addrx++,addry++) {

	    /* Compute the warped address for this point */
	    pos[0] = ix;
	    pos[1] = iy;
	    cmod_cahvore_warp_from_cahv(
		cahv->c, cahv->a, cahv->h, cahv->v,
		pos, rdist, FALSE,
		cahvore->mtype, cahvore->mparm,
		cahvore->c, cahvore->a, cahvore->h, cahvore->v,
		cahvore->o, cahvore->r, cahvore->e,
		pos
		);

	    /* Clip to the image boundaries */
	    if (pos[0] < 0)
		pos[0] = ADDR_UNDERFLOW;	/* was: 0 */
	    if (pos[1] < 0)
		pos[1] = ADDR_UNDERFLOW;	/* was: 0 */
	    if (pos[0] > xlim)
		pos[0] = ADDR_OVERFLOW;		/* was: xlim */
	    if (pos[1] > ylim)
		pos[1] = ADDR_OVERFLOW;		/* was: ylim */

	    /* Record the address */
	    *addrx = pos[0];
	    *addry = pos[1];
	    }
	}

    return SUCCESS;
    }

#if 0

/******************************************************************************
********************************   ST_WARP_CFIXED   ***************************
*******************************************************************************

    This function uses a previously defined warping map to warp an image. */

int st_warp_cfixed(warpmap, xdim, ydim, x0, y0, nx, ny, image0, image1)
char *warpmap;		/* input warping map */
int xdim;		/* input size in X dimension */
int ydim;		/* input size in Y dimension */
int x0;			/* input left-most X coordinate */
int y0;			/* input upper-most Y coordinate */
int nx;			/* input number of columns */
int ny;			/* input number of rows */
unsigned char *image0;	/* input image to warp */
unsigned char *image1;	/* output warped image */
{
    int row_offset, start_offset;
    warp_i24f44c_t *warp;
    int ix, iy;
    unsigned short *addr, addr16;
    long addr32, intoff, rowoff0, rowoff1, ifracx, ifracy, xdiff, ydiff;
    unsigned char i00, i01, i10, i11, *img0, *img1;

    /* Check data types (size and complement) */
    if ((sizeof(long) != 4) || (sizeof(short) != 2) || (-1 != ~0)) {
	printf("st_warp_cfixed(): bad data types\n");
	return FAILURE;
	}

    /* Make sure that the map is the correct size */
    warp = (warp_i24f44c_t *)warpmap;
    if ((warp->datatype != WARP_I24F44C) ||
	(warp->xdim != xdim) || (warp->ydim != ydim) ||
	(warp->x0 != x0) || (warp->y0 != y0) ||
	(warp->nx != nx) || (warp->ny != ny)) {
	printf("st_warp_cfixed(): size or window mismatch\n");
	return FAILURE;
	}

    /* Compute how far we advance between rows */
    row_offset = xdim - nx;
    if (warp->frame) {
	rowoff0 = xdim;
	rowoff1 = xdim + 1;
	}
    else {
	rowoff0 = 2*xdim;
	rowoff1 = 2*xdim + 1;
	}

    /* Move to the beginning of the first row to process horizontally */
    start_offset = (xdim * y0) + x0;
    image1 += start_offset;
    addr = warp->addr;
    img0 = image0;
    img1 = image1;
    addr32 = 0;	/* first cycle won't use this */

    /* Process each row */
    for (iy=ny; iy>0; iy--) {

	/* Process each column in the row */
	for (ix=nx; ix>0; ix--,img1++) {

	    /* Get the next address code */
	    if ((addr16 = *(addr++)) == 0x8000) {
		addr16 = *(addr++);
		addr32 = (addr16 << 16) | *(addr++);
		}
	    else {
		xdiff = ((addr16 >> 12) & 0x0F);	/* extract data */
		ydiff = ((addr16 >>  8) & 0x0F);
		if (xdiff & 0x08)			/* sign extend */
		    xdiff |= 0xFFFFFFF0;
		if (ydiff & 0x08)
		    ydiff |= 0xFFFFFFF0;
		addr32 = ((addr32 & 0xFFFFFF00)
				+ (((ydiff * xdim) + xdiff) << 8))
			    | (addr16 & 0xFF);
		}

	    /* Check for out-of-bounds error */
	    if (addr32 == 0xFFFFFFFF) {
		*img1 = 0;
		continue;
		}

	    /* Get the intensity values of the four nearest neighbors */
	    intoff = addr32 >> 8;
	    i00 = img0[intoff];
	    i01 = img0[intoff + 1];
	    i10 = img0[intoff + rowoff0];
	    i11 = img0[intoff + rowoff1];

	    /* Extract the fractional parts */
	    ifracx = (addr32 >> 4) & 0x0F;
	    ifracy =  addr32       & 0x0F;

	    /* Compute the target intensity using a bilinear interpolation */
	    *img1 = ((
		(((i00 * (16L - ifracx)) + (i01 * ifracx)) * (16L - ifracy)) +
		(((i10 * (16L - ifracx)) + (i11 * ifracx)) * ifracy)
		) + 128) >> 8;
	    }

	/* Move to the start of the next row */
	img1 += row_offset;
	}

    return SUCCESS;
    }

#endif

#ifndef MER

/******************************************************************************
********************************   ST_WARP_CFIXED_INIT_CAHVOR   ***************
*******************************************************************************

    This function initializes for image warping from a CAHVOR-modeled image
    to a CAHV-modeled image using fixed-point bilinear-interpolation
    mapping. The memory for the output map is allocated internally, and it
    is the responsibility of the caller to free the map when it is no longer
    needed.

    Note that the data encoded here is a layer on top of the one produced by
    st_warp_fixed_init_cahvor. It works be updating the previous value. The
    lower-order byte merely replaces the low-order byte of the previous value.
    The high-order byte is broken in 2 4-bit fields, each representing an
    update to the X and Y coordinates implicit in the integer offset. */

int st_warp_cfixed_init_cahvor(cahvor, cahv, frame, xdim, ydim, x0, y0, nx, ny,
			warpmap)
st_cahvor_t *cahvor;	/* input CAHVOR camera model warping from */
st_cahv_t *cahv;	/* input CAHV camera model warping to */
int frame;		/* input if a full frame (vs. a field) is to be used */
int xdim;		/* input size in X dimension */
int ydim;		/* input size in Y dimension */
int x0;			/* input left-most X coordinate */
int y0;			/* input upper-most Y coordinate */
int nx;			/* input number of columns */
int ny;			/* input number of rows */
char **warpmap;		/* output warping map */
{
    unsigned neededsize;
    int ix, iy, nshorts;
    unsigned short *addr;
    long /*ipos0, ipos1,*/ ifracx, ifracy, intx, inty, intoff, iaddr, iaddr0;
    long idiff, xdiff, ydiff;
    double pos[2], xlim, ylim, fracx, fracy;
    warp_i24f44c_t *warp;

    /* Check data types (size and complement) */
    if ((sizeof(long) != 4) || (sizeof(short) != 2) || (-1 != ~0)) {
	printf("st_warp_cfixed_init_cahvor(): bad data types\n");
	return FAILURE;
	}

    /* Allocate the memory for the map */
    nshorts = (3 * xdim * ydim) / 2;
    neededsize = sizeof(warp_i24f44c_t) + (sizeof(short) * nshorts);
    if ((warp = (warp_i24f44c_t *)malloc(neededsize)) == NULL)
	return FAILURE;
    *warpmap = (char *)warp;
    warp->datatype = WARP_I24F44C;
    warp->xdim = xdim;
    warp->ydim = ydim;
    warp->x0 = x0;
    warp->y0 = y0;
    warp->nx = nx;
    warp->ny = ny;
    warp->frame = frame;

    /* Process each row */
    addr = warp->addr;
    xlim = xdim - 1;
    ylim = ydim - 1;
    for (iy=0; iy<ny; iy++) {

	/* Process each column in the row */
	for (ix=0; ix<nx; ix++) {

	    /* Check that there is more room */
	    if ((addr - warp->addr) > (nshorts - 3)) {
		free((char *)warp);
		return FAILURE;
		}

	    /* Compute the warped address for this point */
	    pos[0] = ix + x0;
	    pos[1] = iy + y0;
	    cmod_cahvor_warp_from_cahv(
		cahv->c, cahv->a, cahv->h, cahv->v,
		pos, FALSE,
		cahvor->c, cahvor->a, cahvor->h, cahvor->v,
		cahvor->o, cahvor->r,
		pos
		);

	    /* Clip to the image boundaries */
	    if ((pos[0] < 0) || (pos[0] > xlim) ||
		(pos[1] < 0) || (pos[1] > ylim)) {
		iaddr = 0xFFFFFFFF;
		goto DIFF;
		}

	    /* Get the address of the upper, left pixel */
	    intx = (long)pos[0];
	    inty = (long)pos[1];

	    /* Compute the fractional addresses */
	    fracx = pos[0] - intx;
	    fracy = pos[1] - inty;

	    /* Adjust for field-only processing */
	    if (!frame) {

		/* Check for being on the correct field */
		if ((inty & 1) != ((iy + y0) & 1)) {
		    inty--;
		    fracy += 1.0;
		    if (inty < 0) {
			iaddr = 0xFFFFFFFF;
			goto DIFF;
			}
		    }

		/* Adjust bilinear weighting for skipping rows */
		fracy /= 2.0;
		if (fracy > 1.0)
		    fracy = 1.0;
		}

	    /* Compute the 4-bit scaled fractional addresses */
	    ifracx = (long)(fracx * 16 + 0.5);
	    ifracy = (long)(fracy * 16 + 0.5);
	    if (ifracx >= 16) {
		intx++;
		ifracx = 0;
		}
	    if (ifracy >= 16) {
		inty += (frame ? 1 : 2);
		ifracy = 0;
		}

	    /* Check for boundary cases */
	    if ((intx < 0) || (intx >= xdim - 1) ||
		(inty < 0) || (inty >= ydim - 1) ||
		(!frame    && (inty >= ydim - 2))) {
		iaddr = 0xFFFFFFFF;
		goto DIFF;
		}

	    /* Get the offset to the upper-left corner */
	    intoff = intx + (xdim * inty);
	    if (intoff > 0x00FFFFFF) {
		iaddr = 0xFFFFFFFF;
		goto DIFF;
		}

	    /* Encode the address */
	    iaddr = (intoff << 8) | (ifracx << 4) | ifracy;

	    /* Compute the difference */
	    DIFF:
	    idiff = (iaddr >> 8) - (iaddr0 >> 8);
	    iaddr0 = iaddr;
	    if (idiff >= 0) {
		xdiff = idiff % xdim;
		ydiff = idiff / xdim;
		}
	    else {
		xdiff = -((-idiff) % xdim);
		ydiff = -((-idiff) / xdim);
		}
	    if (((ix == 0) && (iy == 0))
		|| (xdiff < -7) || (xdiff > 7)
		|| (ydiff < -7) || (ydiff > 7)) {
		*(addr++) = 0x8000;
		*(addr++) = iaddr >> 16;
		*(addr++) = iaddr  & 0xFFFF;
		}
	    else
		*(addr++) = ((xdiff & 0x0F) << 12)
			  | ((ydiff & 0x0F) <<  8)
			  |  (iaddr & 0xFF);

	    }
	}

    return SUCCESS;
    }


/******************************************************************************
********************************   ST_WARP_FIXED   ****************************
*******************************************************************************

    This function uses a previously defined warping map to warp an image. */

int st_warp_fixed(warpmap, xdim, ydim, x0, y0, nx, ny, image0, image1)
char *warpmap;		/* input warping map */
int xdim;		/* input size in X dimension */
int ydim;		/* input size in Y dimension */
int x0;			/* input left-most X coordinate */
int y0;			/* input upper-most Y coordinate */
int nx;			/* input number of columns */
int ny;			/* input number of rows */
unsigned char *image0;	/* input image to warp */
unsigned char *image1;	/* output warped image */
{
    int row_offset, start_offset;
    warp_i24f44_t *warp;
    /*register*/ int ix, iy;
    /*register*/ long *addr, addr32, intoff, rowoff0, rowoff1, ifracx, ifracy;
    /*register*/ unsigned char i00, i01, i10, i11, *img0, *img1;

    /* Check size of data type */
    if (sizeof(long) < 4)
	return FAILURE;

    /* Make sure that the map is the correct size */
    warp = (warp_i24f44_t *)warpmap;
    if ((warp->datatype != WARP_I24F44) ||
	(warp->xdim != xdim) || (warp->ydim != ydim))
	return FAILURE;

    /* Compute how far we advance between rows */
    row_offset = xdim - nx;
    if (warp->frame) {
	rowoff0 = xdim;
	rowoff1 = xdim + 1;
	}
    else {
	rowoff0 = 2*xdim;
	rowoff1 = 2*xdim + 1;
	}

    /* Move to the beginning of the first row to process horizontally */
    start_offset = (xdim * y0) + x0;
    image1 += start_offset;
    addr = warp->addr + start_offset;
    img0 = image0;
    img1 = image1;

    /* Process each row */
    for (iy=ny; iy>0; iy--) {

	/* Process each column in the row */
	for (ix=nx; ix>0; ix--,img1++,addr++) {

	    /* Check for out-of-bounds error */
	    if ((addr32 = *addr) == 0xFFFFFFFF) {
		*img1 = 0;
		continue;
		}

	    /* Get the intensity values of the four nearest neighbors */
	    intoff = addr32 >> 8;
	    i00 = img0[intoff];
	    i01 = img0[intoff + 1];
	    i10 = img0[intoff + rowoff0];
	    i11 = img0[intoff + rowoff1];

	    /* Extract the fractional parts */
	    ifracx = (addr32 >> 4) & 0x0F;
	    ifracy =  addr32       & 0x0F;

	    /* Compute the target intensity using a bilinear interpolation */
	    *img1 = ((
		(((i00 * (16L - ifracx)) + (i01 * ifracx)) * (16L - ifracy)) +
		(((i10 * (16L - ifracx)) + (i11 * ifracx)) * ifracy)
		) + 128) >> 8;
	    }

	/* Move to the start of the next row */
	img1 += row_offset;
	addr += row_offset;
	}

    return SUCCESS;
    }

#if 0

/******************************************************************************
********************************   ST_WARP_FIXED_1FIELD   *********************
*******************************************************************************

    This function uses a previously defined warping map to warp a single
    field of an image. The input warping map may have been initialized as
    either a field or frame. Note that the output is not compact, but is
    spaced every other line. */

int st_warp_fixed_1field(warpmap, field,
			xdim, ydim, x0, y0, nx, ny, image0, image1)
char *warpmap;		/* input warping map */
int field;		/* input field selection: 0 or 1 */
int xdim;		/* input size in X dimension */
int ydim;		/* input size in Y dimension */
int x0;			/* input left-most X coordinate */
int y0;			/* input upper-most Y coordinate */
int nx;			/* input number of columns */
int ny;			/* input number of rows */
unsigned char *image0;	/* input image to warp */
unsigned char *image1;	/* output warped image */
{
    int row_offset, start_offset;
    warp_i24f44_t *warp;
    /*register*/ int ix, iy;
    /*register*/ long *addr, addr32, intoff, rowoff0, rowoff1, ifracx, ifracy;
    /*register*/ unsigned char i00, i01, i10, i11, *img0, *img1;

    /* Check size of data type */
    if (sizeof(long) < 4)
	return FAILURE;

    /* Make sure that the map is the correct size */
    warp = (warp_i24f44_t *)warpmap;
    if ((warp->datatype != WARP_I24F44) ||
	(warp->xdim != xdim) || (warp->ydim != ydim))
	return FAILURE;

    /* Compute how far we advance between rows */
    row_offset = 2*xdim - nx;
    if (warp->frame) {
	rowoff0 = xdim;
	rowoff1 = xdim + 1;
	}
    else {
	rowoff0 = 2*xdim;
	rowoff1 = 2*xdim + 1;
	}

    /* Make sure that we start on an appropriate row */
    if ((field & 1) != (y0 & 1)) {
	y0++;
	ny--;
	}

    /* Move to the beginning of the first row to process horizontally */
    start_offset = (xdim * y0) + x0;
    image1 += start_offset;
    addr = warp->addr + start_offset;
    img0 = image0;
    img1 = image1;

    /* Process each row */
    for (iy=ny; iy>0; iy-=2) {

	/* Process each column in the row */
	for (ix=nx; ix>0; ix--,img1++,addr++) {

	    /* Check for out-of-bounds error */
	    if ((addr32 = *addr) == 0xFFFFFFFF) {
		*img1 = 0;
		continue;
		}

	    /* Get the intensity values of the four nearest neighbors */
	    intoff = addr32 >> 8;
	    i00 = img0[intoff];
	    i01 = img0[intoff + 1];
	    i10 = img0[intoff + rowoff0];
	    i11 = img0[intoff + rowoff1];

	    /* Extract the fractional parts */
	    ifracx = (addr32 >> 4) & 0x0F;
	    ifracy =  addr32       & 0x0F;

	    /* Compute the target intensity using a bilinear interpolation */
	    *img1 = ((
		(((i00 * (16L - ifracx)) + (i01 * ifracx)) * (16L - ifracy)) +
		(((i10 * (16L - ifracx)) + (i11 * ifracx)) * ifracy)
		) + 128) >> 8;
	    }

	/* Move to the start of the next row */
	img1 += row_offset;
	addr += row_offset;
	}

    return SUCCESS;
    }


/******************************************************************************
********************************   ST_WARP_FIXED_INIT_CAHVOR   ****************
*******************************************************************************

    This function initializes for image warping from a CAHVOR-modeled image
    to a CAHV-modeled image using fixed-point bilinear-interpolation
    mapping. The memory for the output map is allocated internally, and it
    is the responsibility of the caller to free the map when it is no longer
    needed.

    Each 4-byte quantity is made of of 3 fields: 24 bits of integer offset
    from the beginning of the source image of the upper left pixel to be used
    for bilinear interpolation, plus 4 bits of X fraction and 4 bits of Y
    fraction. */

int st_warp_fixed_init_cahvor(cahvor, cahv, frame, xdim, ydim, warpmap)
st_cahvor_t *cahvor;	/* input CAHVOR camera model warping from */
st_cahv_t *cahv;	/* input CAHV camera model warping to */
int frame;		/* input if a full frame (vs. a field) is to be used */
int xdim;		/* input size in X dimension */
int ydim;		/* input size in Y dimension */
char **warpmap;		/* output warping map */
{
    unsigned neededsize;
    int ix, iy;
    long *addr, ipos0, ipos1, ifracx, ifracy, intx, inty, intoff;
    double pos[2], xlim, ylim, fracx, fracy;
    warp_i24f44_t *warp;

    /* Check size of data type */
    if (sizeof(long) < 4)
	return FAILURE;

    /* Allocate the memory for the map */
    neededsize = sizeof(warp_i24f44_t) + (sizeof(long) * (xdim * ydim - 2));
    if ((warp = (warp_i24f44_t *)malloc(neededsize)) == NULL)
	return FAILURE;
    *warpmap = (char *)warp;
    warp->datatype = WARP_I24F44;
    warp->xdim = xdim;
    warp->ydim = ydim;
    warp->frame = frame;

    /* Process each row */
    addr = warp->addr;
    xlim = xdim - 1;
    ylim = ydim - 1;
    for (iy=0; iy<ydim; iy++) {

	/* Process each column in the row */
	for (ix=0; ix<xdim; ix++,addr++) {

	    /* Compute the warped address for this point */
	    pos[0] = ix;
	    pos[1] = iy;
	    cmod_cahvor_warp_from_cahv(
		cahv->c, cahv->a, cahv->h, cahv->v,
		pos, FALSE,
		cahvor->c, cahvor->a, cahvor->h, cahvor->v,
		cahvor->o, cahvor->r,
		pos
		);

	    /* Clip to the image boundaries */
	    if ((pos[0] < 0) || (pos[0] > xlim) ||
		(pos[1] < 0) || (pos[1] > ylim)) {
		*addr = 0xFFFFFFFF;
		continue;
		}

	    /* Get the address of the upper, left pixel */
	    intx = (long)pos[0];
	    inty = (long)pos[1];

	    /* Compute the fractional addresses */
	    fracx = pos[0] - intx;
	    fracy = pos[1] - inty;

	    /* Adjust for field-only processing */
	    if (!frame) {

		/* Check for being on the correct field */
		if ((inty & 1) != (iy & 1)) {
		    inty--;
		    fracy += 1.0;
		    if (inty < 0) {
			*addr = 0xFFFFFFFF;
			continue;
			}
		    }

		/* Adjust bilinear weighting for skipping rows */
		fracy /= 2.0;
		if (fracy > 1.0)
		    fracy = 1.0;
		}

	    /* Compute the 4-bit scaled fractional addresses */
	    ifracx = (long)(fracx * 16 + 0.5);
	    ifracy = (long)(fracy * 16 + 0.5);
	    if (ifracx >= 16) {
		intx++;
		ifracx = 0;
		}
	    if (ifracy >= 16) {
		inty += (frame ? 1 : 2);
		ifracy = 0;
		}

	    /* Check for boundary cases */
	    if ((intx < 0) || (intx >= xdim - 1) ||
		(inty < 0) || (inty >= ydim - 1) ||
		(!frame    && (inty >= ydim - 2))) {
		*addr = 0xFFFFFFFF;
		continue;
		}

	    /* Get the offset to the upper-left corner */
	    intoff = intx + (xdim * inty);
	    if (intoff > 0x00FFFFFF) {
		*addr = 0xFFFFFFFF;
		continue;
		}

	    /* Encode the address */
	    *addr = (intoff << 8) | (ifracx << 4) | ifracy;
	    }
	}

    return SUCCESS;
    }

#endif

/******************************************************************************
********************************   ST_WARP_NEAREST   **************************
*******************************************************************************

    This function uses a previously defined warping map to warp an image. */

int st_warp_nearest(warpmap, xdim, ydim, x0, y0, nx, ny, image0, image1)
char *warpmap;		/* input warping map */
int xdim;		/* input size in X dimension */
int ydim;		/* input size in Y dimension */
int x0;			/* input left-most X coordinate */
int y0;			/* input upper-most Y coordinate */
int nx;			/* input number of columns */
int ny;			/* input number of rows */
unsigned char *image0;	/* input image to warp */
unsigned char *image1;	/* output warped image */
{
    int i, ix, iy, row_offset, start_offset;
    warp_int_t *warp;
    int *addr;

    /* Make sure that the map is the correct size */
    warp = (warp_int_t *)warpmap;
    if ((warp->datatype != WARP_INT) ||
	(warp->xdim != xdim) || (warp->ydim != ydim))
	return FAILURE;

    /* Compute how far we advance between rows */
    row_offset = xdim - nx;

    /* Move to the beginning of the first row to process horizontally */
    start_offset = (xdim * y0) + x0;
    image1 += start_offset;
    addr = warp->addr + start_offset;

    /* Process each row */
    for (iy=0; iy<ny; iy++) {

	/* Process each column in the row */
	for (ix=0; ix<nx; ix++,image1++,addr++) {
	    i = *addr;
	    if (i >= 0)
		*image1 = image0[i];
	    else
		*image1 = 0;
	    }

	/* Move to the start of the next row */
	image1 += row_offset;
	addr   += row_offset;
	}

    return SUCCESS;
    }


/******************************************************************************
********************************   ST_WARP_NEAREST_INIT_CAHVOR   **************
*******************************************************************************

    This function initializes for image warping from a CAHVOR-modeled image
    to a CAHV-modeled image using integer-only nearest-neighbor mapping. The
    memory for the output map is allocated internally, and it is the
    responsibility of the caller to free the map when it is no longer needed.
    */

int st_warp_nearest_init_cahvor(cahvor, cahv, xdim, ydim, warpmap)
st_cahvor_t *cahvor;	/* input CAHVOR camera model warping from */
st_cahv_t *cahv;	/* input CAHV camera model warping to */
int xdim;		/* input size in X dimension */
int ydim;		/* input size in Y dimension */
char **warpmap;		/* output warping map */
{
    unsigned neededsize;
    int ix, iy, *addr, addrx, addry, berr;
    double pos[2];
    warp_int_t *warp;

    /* Allocate the memory for the map */
    neededsize = sizeof(warp_int_t) + (sizeof(int) * (xdim * ydim - 2));
    if ((warp = (warp_int_t *)malloc(neededsize)) == NULL)
	return FAILURE;
    *warpmap = (char *)warp;
    warp->datatype = WARP_INT;
    warp->xdim = xdim;
    warp->ydim = ydim;

    /* Process each row */
    addr = warp->addr;
    for (iy=0; iy<ydim; iy++) {

	/* Process each column in the row */
	for (ix=0; ix<xdim; ix++,addr++) {

	    /* Compute the warped address for this point */
	    pos[0] = ix;
	    pos[1] = iy;
	    cmod_cahvor_warp_from_cahv(
		cahv->c, cahv->a, cahv->h, cahv->v,
		pos, FALSE,
		cahvor->c, cahvor->a, cahvor->h, cahvor->v,
		cahvor->o, cahvor->r,
		pos
		);

	    /* Find the nearest neighbor */
	    addrx = (int)(pos[0] + 0.5);
	    addry = (int)(pos[1] + 0.5);

	    /* Clip to the image boundaries */
	    berr = FALSE;
	    if (addrx < 0) {
		addrx = 0;
		berr = TRUE;
		}
	    if (addry < 0) {
		addry = 0;
		berr = TRUE;
		}
	    if (addrx >= xdim) {
		addrx  = xdim - 1;
		berr = TRUE;
		}
	    if (addry >= ydim) {
		addry  = ydim - 1;
		berr = TRUE;
		}

	    /* Record the nearest-neighbor address */
	    *addr = addrx + (xdim * addry);
	    if (berr)
		*addr = -(*addr) - 1;
	    }
	}

    return SUCCESS;
    }


/******************************************************************************
********************************   ST_WARP_NEAREST_INIT_CAHVOR_FIELD   ********
*******************************************************************************

    This function is just like st_warp_nearest_init_cahvor(), except that
    it initializes for operation on odd and even fields separately.
    */

int st_warp_nearest_init_cahvor_field(cahvor, cahv, xdim, ydim, warpmap)
st_cahvor_t *cahvor;	/* input CAHVOR camera model warping from */
st_cahv_t *cahv;	/* input CAHV camera model warping to */
int xdim;		/* input size in X dimension */
int ydim;		/* input size in Y dimension */
char **warpmap;		/* output warping map */
{
    unsigned neededsize;
    int ix, iy, *addr, addrx, addry, berr;
    double pos[2];
    warp_int_t *warp;

    /* Allocate the memory for the map */
    neededsize = sizeof(warp_int_t) + (sizeof(int) * (xdim * ydim - 2));
    if ((warp = (warp_int_t *)malloc(neededsize)) == NULL)
	return FAILURE;
    *warpmap = (char *)warp;
    warp->datatype = WARP_INT;
    warp->xdim = xdim;
    warp->ydim = ydim;

    /* Process each row */
    addr = warp->addr;
    for (iy=0; iy<ydim; iy++) {

	/* Process each column in the row */
	for (ix=0; ix<xdim; ix++,addr++) {

	    /* Compute the warped address for this point */
	    pos[0] = ix;
	    pos[1] = iy;
	    cmod_cahvor_warp_from_cahv(
		cahv->c, cahv->a, cahv->h, cahv->v,
		pos, FALSE,
		cahvor->c, cahvor->a, cahvor->h, cahvor->v,
		cahvor->o, cahvor->r,
		pos
		);

	    /* Reduce scale of Y dimension */
	    if ((iy & 1) == 1)
		pos[1] -= 1.0;
	    pos[1] /= 2;

	    /* Find the nearest neighbor */
	    addrx = (int)(pos[0] + 0.5);
	    addry = (int)(pos[1] + 0.5);

	    /* Clip to the image boundaries */
	    berr = FALSE;
	    if (addrx < 0) {
		addrx = 0;
		berr = TRUE;
		}
	    if (addry < 0) {
		addry = 0;
		berr = TRUE;
		}
	    if (addrx >= xdim) {
		addrx  = xdim - 1;
		berr = TRUE;
		}
	    if (addry >= ydim/2) {
		addry  = ydim/2 - 1;
		berr = TRUE;
		}

	    /* Adjust Y dimension back into proper scale and field */
	    addry *= 2;
	    if ((iy & 1) == 1)
		addry++;

	    /* Record the nearest-neighbor address */
	    *addr = addrx + (xdim * addry);
	    if (berr)
		*addr = -(*addr) - 1;
	    }
	}

    return SUCCESS;
    }


/******************************************************************************
********************************   ST_WARP_READ   *****************************
*******************************************************************************

    This function reads in a warping map from a file. It allocates the memory
    internally to hold the data. It is the caller's responsibility to free
    the memory when it is no longer needed. */

int st_warp_read(filename, warpmap, dtype, xdim, ydim)
char *filename;		/* input name of warp file */
char **warpmap;		/* output pointer to warping map */
int *dtype;		/* output data type: 1=nearest, 2=bilinear */
int *xdim;		/* output X dimension */
int *ydim;		/* output Y dimension */
{
    int fd, nbytes, n;
    warp_int_t header;

    /* Open the warp file */
    if ((fd = open(filename, 0)) == -1) {
	fprintf(stderr, "Error opening warp file: %s\n", filename);
	return FAILURE;
	}

    /* Read in the header of the file */
    if (read(fd, &header, sizeof header) != sizeof header) {
	fprintf(stderr, "Error reading warp-file header: %s\n", filename);
	close(fd);
	return FAILURE;
	}
    lseek(fd, 0L, 0);

    /* See what type of data we have */
    if (header.datatype == WARP_INT)
	nbytes = sizeof(warp_int_t)
		+ (sizeof(int) * (header.xdim * header.ydim - 2));
    else if (header.datatype == WARP_FLOAT)
	nbytes = sizeof(warp_float_t)
		+ (2 * sizeof(float) * (header.xdim * header.ydim - 1));
    else {
	fprintf(stderr, "Error decoding data type: %d\n", header.datatype);
	return FAILURE;
	}
    *dtype = header.datatype;
    *xdim  = header.xdim;
    *ydim  = header.ydim;

    /* Allocate enough memory to hold the warp data */
    if ((*warpmap = (char *)malloc(nbytes)) == NULL) {
	fprintf(stderr, "Error allocating %d bytes for warp file: %s\n",
		nbytes, filename);
	close(fd);
	return FAILURE;
	}

    /* Read the image into local memory */
    if ((n = read(fd, *warpmap, nbytes)) != nbytes) {
	fprintf(stderr, "Error reading data from warp file: %s, %d vs %d\n",
		filename, n, nbytes);
	free(*warpmap);
	close(fd);
	return FAILURE;
	}

    /* Close the warp file */
    close(fd);

    return SUCCESS;
    }


/******************************************************************************
********************************   ST_WARP_WRITE   ****************************
*******************************************************************************

    This function writes warping map to a file. */

int st_warp_write(filename, warpmap)
char *filename;		/* input name of warp file */
char *warpmap;		/* input warping map */
{
    int fd, n, datatype, xdim, ydim;
    unsigned neededsize;

    /* See what type of data we have */
    datatype	= ((warp_int_t *)warpmap)->datatype;
    xdim	= ((warp_int_t *)warpmap)->xdim;
    ydim	= ((warp_int_t *)warpmap)->ydim;
    if (datatype == WARP_INT)
	neededsize = sizeof(warp_int_t) + (sizeof(int) * (xdim * ydim - 2));
    else if (datatype == WARP_FLOAT)
	neededsize = sizeof(warp_float_t) + (2*sizeof(float) * (xdim*ydim - 1));
    else {
	fprintf(stderr, "Error decoding data type: %d\n", datatype);
	return FAILURE;
	}

    /* Open the warp file */
    if ((fd = creat(filename, PMODE)) == -1) {
	fprintf(stderr, "Error creating warp file: %s\n", filename);
	return FAILURE;
	}

    /* Write the warping map to the file */
    if ((n = write(fd, warpmap, neededsize)) != neededsize) {
	fprintf(stderr,
		"Error writing map to warp file: %s, %d bytes written\n",
		filename, n);
	close(fd);
	return FAILURE;
	}

    /* Close the warp file */
    if (close(fd) < 0) {
	fprintf(stderr, "Error closing warp file: %s\n", filename);
	return FAILURE;
	}

    return SUCCESS;
    }

#endif /* MER */
