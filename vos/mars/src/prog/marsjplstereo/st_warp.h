#ifndef __ST_WARP__
#define __ST_WARP__

#include "stereo.h"

#ifdef __cplusplus
extern "C" {
#endif

int st_warp_bilinear(
		     char *warpmap,	/* input warping map */
		     int xdim,		/* input size in X dimension */
		     int ydim,		/* input size in Y dimension */
		     int x0,		/* input left-most X coordinate */
		     int y0,		/* input upper-most Y coordinate */
		     int nx,		/* input number of columns */
		     int ny,		/* input number of rows */
		     unsigned char *image0,	/* input image to warp */
		     unsigned char *image1	/* output warped image */
		     );

int st_warp_bilinear_init_cahvor
	(st_cahvor_t *cahvor,	/* input CAHVOR camera model warping from */
	 st_cahv_t *cahv,	/* input CAHV camera model warping to */
	 int xdim,		/* input size in X dimension */
	 int ydim,		/* input size in Y dimension */
	 char **warpmap		/* output warping map */
	 );

int st_warp_bilinear_init_cahvore
	(st_cahvore_t *cahvore,	/* input CAHVORE camera model warping from */
	 st_cahv_t *cahv,	/* input CAHV camera model warping to */
	 double rdist,		/* input radial distance to project */
	 int xdim,		/* input size in X dimension */
	 int ydim,		/* input size in Y dimension */
	 char **warpmap,	/* input warping map buffer space (2 floats
				   per pixel) */
	 unsigned int mapsize   /* size of input warping map in bytes */
	 );


int st_warp_fixed(
		  char *warpmap,		/* input warping map */
		  int xdim,		/* input size in X dimension */
		  int ydim,		/* input size in Y dimension */
		  int x0,			/* input left-most X coordinate */
		  int y0,			/* input upper-most Y coordinate */
		  int nx,			/* input number of columns */
		  int ny,			/* input number of rows */
		  unsigned char *image0,	/* input image to warp */
		  unsigned char *image1	/* output warped image */
		  );


int st_warp_nearest(
		    char *warpmap,		/* input warping map */
		    int xdim,		/* input size in X dimension */
		    int ydim,		/* input size in Y dimension */
		    int x0,		/* input left-most X coordinate */
		    int y0,		/* input upper-most Y coordinate */
		    int nx,			/* input number of columns */
		    int ny,			/* input number of rows */
		    unsigned char *image0,	/* input image to warp */
		    unsigned char *image1	/* output warped image */
		    );

int st_warp_nearest_init_cahvor(
				st_cahvor_t *cahvor,	/* input CAHVOR camera model warping from */
				st_cahv_t *cahv, /* input CAHV camera model warping to */
				int xdim,      /* input size in X dimension */
				int ydim,      /* input size in Y dimension */
				char **warpmap		/* output warping map */
				);


int st_warp_nearest_init_cahvor_field(
				      st_cahvor_t *cahvor, /* input CAHVOR camera model warping from */
				      st_cahv_t *cahv,	/* input CAHV camera model warping to */
				      int xdim,		/* input size in X dimension */
				      int ydim,		/* input size in Y dimension */
				      char **warpmap	/* output warping map */
				      );

int st_warp_read(
	     char *filename,	/* input name of warp file */
	     char **warpmap,	/* output pointer to warping map */
	     int *dtype,	/* output data type: 1=nearest, 2=bilinear */
	     int *xdim,		/* output X dimension */
	     int *ydim		/* output Y dimension */
	     );

int st_warp_write(
	      char *filename,		/* input name of warp file */
	      char *warpmap		/* input warping map */
	      );

#ifdef __cplusplus
}
#endif

#endif
