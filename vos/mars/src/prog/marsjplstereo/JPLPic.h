/******************************************************************************
*                                                                             *
*		JPL Stereo Vision code					      *
*									      *
*		Developed by the Machine Vision and Tracking Sensors Group    *
*		at the Jet Propulsion Laboratory			      *
*								  	      *
*									      *
*	For information contact:					      *
*									      *
*			Larry Matthies	lhm@robotics.jpl.nasa.gov	      *
*                      	Todd Litwin     litwin@robotics.jpl.nasa.gov          *
*			Mark Maimone	mark.maimone@jpl.nasa.gov	      *
*			Yalin Xiong	yx@robotics.jpl.nasa.gov	      *
*								  	      *
*                                       Updated: 16 Nov 1998                  *
*                                                                             *
*                                       Copyright (C) 1993, 1994, 1995, 1996, *
*                                                     1997, 1998              *
*                                       California Institute of Technology    *
*                                       All Rights Reserved                   *
*                                                                             *
******************************************************************************/

#ifndef __JPL_PIC__
#define __JPL_PIC__

#ifndef __INTEL__
#ifndef __unix
#if !defined(RTI_VXWORKS) && !defined (__VXWORKS__) && !defined (RTS_VXWORKS)
#if !defined(__GNUC__) && !defined(VXWORKS) && !defined(unix)
#define MAC_PLATFORM
#endif
#endif
#endif
#endif

#if defined(RTI_VXWORKS) || defined (__VXWORKS__) || defined (RTS_VXWORKS)
#undef VXWORKS
#define VXWORKS
#endif


#include <stdio.h>
#include <stdint.h>
#include "nav_memory.h"
#include "ErrHandle.h"
/* #ifdef __unix */
#if 0
#include "image_parse.h"	// for IS_TRUNCATE
#else
enum ImageScaleT {IS_TRUNCATE, IS_LINEAR, IS_3SIGMA,
		  IS_SCALE_ABOVE_8BITS, IS_HISTEQ};
#endif

#if defined(__ghs__)
#undef NULL
#define NULL 0
#endif

#ifdef MAC_PLATFORM

#include <QDOffscreen.h>
#endif



// Pixel Types
enum PixelTag {
  UC8_PIXEL = 0, // unsigned char 8-bits (must be 0 for backward compatibility)
  START_PIXEL = UC8_PIXEL,
  INT16_PIXEL = 1,	// unsigned int  16-bits
  INT32_PIXEL = 2,	// unsigned long 32-bits
  FLOAT_PIXEL = 3,	// float	 32-bits
  ARGB32_PIXEL = 4,	// 32-bits ARGB Color pixels
  DOUBLE_PIXEL = 5,	// double        64-bits
  XYZ_FLOAT_PIXEL = 6,
  XYZ_DOUBLE_PIXEL = 7,
  UNKNOWN_PIXEL = 8,
  END_PIXEL = UNKNOWN_PIXEL
};

#define IS_INT_PIXEL(x)		((x) == UC8_PIXEL || (x) == INT16_PIXEL || \
				 (x) == INT32_PIXEL)
#define IS_FLOAT_PIXEL(x)	((x) == FLOAT_PIXEL || (x) == XYZ_FLOAT_PIXEL)
#define IS_DOUBLE_PIXEL(x)	((x) == DOUBLE_PIXEL || (x) == XYZ_DOUBLE_PIXEL)
#define IS_SCALAR_PIXEL(x)	(!IS_VECTOR_PIXEL(x))
#define IS_VECTOR_PIXEL(x)	((x) == ARGB32_PIXEL || (x) == XYZ_FLOAT_PIXEL \
				 || (x) == XYZ_DOUBLE_PIXEL)

#define IS_UNDEF_3D_POINT(x,y,z) \
     ((x) < -10000.0 || (y) < -10000.0 || (z) < -10000.0)
#define IS_UNDEF_DISPARITY(x)  ((x) == 0x7fff)

#define COLOR_CODED		1
#define GRAY_CODED		2

// MASKS for JPLPic Flags
#define WRITE_AS_SCALED_8BIT_IMAGE 0x1
#define WRITE_FLOATS_AS_DOUBLES    0x2
#define WRITE_REALS_SWAPPING_BYTES 0x4

#define MAX_CMAP_SIZE (256 * 3 * sizeof(double))
#define DFLT_CMAP_SIZE (256 * 4 * sizeof(char)) /* ARGB needs 4 bytes/pixel */

typedef union {
	unsigned char *uc;
	char *c;
	const char *cc;
	short *h;
	unsigned short *uh;
	int *i;
	long *l;
	unsigned int *ui;
	unsigned long *ul, ulv;
	float *f;
	double *d;
} AnythingT;


//////////////////////////////////////////////////////////////////////////////
//  JPLPic Class
//
//	The JPLPic class is a multipurpose two dimensional image
// format.  It supports a variety of pixel types useful for Machine
// Vision applications, in particular:
//
// UC8_PIXEL	"unsigned char",  8-bit greyscale pixels
// INT16_PIXEL	"unsigned int",   16-bit greyscale pixels
// INT32_PIXEL	"unsigned long",  32-bit greyscale pixels
// FLOAT_PIXEL	"floating point", 32-bit floating point native pixels
// ARGB32_PIXEL	"Alpha/Red/Green/Blue", 32-bit color pixels with alpha channel
// DOUBLE_PIXEL	"double",         64-bit double native pixels
// XYZ_FLOAT_PIXEL "float triple", 96-bit triple of 3 floating point values
// XYZ_DOUBLE_PIXEL "double triple", 96-bit triple of 3 double values
//
//	While some methods require that their input images be of a
// certain type, most are designed to work on any of these pixel
// types.
//
//	Images are stored uncompressed in rows.  Contents of adjacent
// rows need not be immediately adjacent in memory, but they must have
// a consistent (and appropriately word-aligned) offset between them.
// That property means it's easy to create a rectangular subwindow
// alias within a given JPLPic object, but also means you should never
// assume that the number of columns in an image is the same as the
// spacing between rows; always use GetRowBytes() if you want to
// explicitly move from row-to-row using pointer arithmetic.  Or even
// better, use the GetPixelAddress() macro to find the appropriate
// start of each row.
//
//	Several primitive graphical object drawing methods are
// provided; Line, Box, Polygon, Arc, Circle, Text for instance.
// These all invoke the SetPixel() method, which allows them to write
// equally well into images with any type of pixel.  These methods
// also make use of a colormap in order to effectively represent
// arbitrary pixel types.  A colormap converts an 8 bit index into a
// value appropriate for the image's pixel type.  Thus each of these
// functions accepts only an 8 bit index to specify the new intensity
// at a given pixel.  But you are free to load the colormap ahead of
// time with the correct pixel values.  This is a bit confusing, but
// allows us to use a single prototype for each drawing method, and
// allows us to avoid having to specify the pixel type at compile
// time.  That's useful when you want to read an image from a file,
// but don't know the type of the image a priori.
//
//	The SetPixel() method used by all of the primitive graphical
// object drawing methods, accepts three parameters in addition to the
// row/col specification:
//
//	wrap -- Wrap any row/col values that are outside the defined
//	range of 0 .. (rows-1) and 0 .. (cols-1).
//
//	val -- Index value into the current colormap
//
//	just_shift -- When applied to any integer-type pixels, ignore
//	the "val" parameter, take whatever value is already at that
//	pixel and shift it by half the valid range.  Useful for
//	highlighting part of an image while still preserving some
//	information about it.
//
//	When compiled with the NEW_READ switch, this code makes use of
// the src/fileio "libimg.a" library for parsing many types of image
// formats.  When built against that library, the ReadGenericImage()
// and WriteGenericImage() methods allow you to read images of
// arbitrary formats without needing to figure out their formats on
// your own.
//
//	Although one could use templates to implement an image class
// such as this, there are two main reasons for avoiding them here:
//
//	1. Templates are deprecated from flight code, since they can
//	result in tremendous code bloat.
//
//	2. You would have to write a wrapper around templated images
//	anyway, if you wanted to support the capability we have (in
//	LinearOperator() and other methods) of being able to blend
//	data between image types, like overlaying a greyscale image on
//	top of a color image.

//////////////////////////////////////////////////////////////////////////////


#ifdef __cplusplus

class JPLPic {
public:

  // PUBLIC METHODS

	JPLPic ();
	JPLPic (JMemoryManager *mm);
	JPLPic (unsigned long flags, JMemoryManager *mm);
	~JPLPic ();
	
	void InitVars (JMemoryManager *mgr, unsigned long new_flags);

	long	Init (long totalRows, long totalCols, long type)
	  { return Init (totalRows, totalCols, type, mm); }
	long	Init (long totalRows, long totalCols, long type,
			      JMemoryManager *mm);
	long	SetUnits (JPLPic *src);
	long	SetUnits (char *new_row_units, int new_free_row,
				  float new_row_offset, float new_row_inc, 
				  char *new_col_units, int new_free_col,
				  float new_col_offset, float new_col_inc);
	char *	GetRowUnits ()
	  { return row_units; }
	char *	GetColUnits ()
	  { return col_units; }

	void	ScaleUnits (float row_scale, float col_scale);

	void	FreeStaticBuffers (void);
	// Image I/O
	long	Read (char *filename, unsigned char fieldOnly);
	long	Read (FILE *picFile, unsigned char fieldOnly);
	long	ReadGenericImage (char *filename)
	  { return ReadGenericImage (filename, -1, -1, IS_TRUNCATE); }
	long	ReadGenericImage (char *filename, int bitsppb,
					  int bands)
	  { return ReadGenericImage (filename, bitsppb, bands, IS_TRUNCATE); }
	long	ReadGenericImage (char *filename, int bitsppb,
					  int bands, enum ImageScaleT scale);
	long	AliasFromMemory (unsigned char *start, long rows,
					 long cols, long type);
	long	LoadFromMemory (unsigned char *start, long rows,
					long cols, long type);
	long	Write (char *filename);
	long    Write (char *dir, char *filename);
	long	Write (FILE *picFile);
	long	WriteFD (char *filename);
	long	WriteFD (int fd);
#ifdef EXTRA_JPLPIC_WRITES
	long    WriteGIF (char *filename);
#endif
	long    WritePPM (FILE *ppmFile, JPLPic *mask);
	long    WritePPM (char *filename, JPLPic *mask);
	long    WritePPM (FILE *ppmFile);

	enum PixelTag FilePixelType (char *filename, FILE *mesgfp);
	long	WriteGenericImage (char *filename)
	  { return WriteGenericImage (filename, NULL); }
	long	WriteGenericImage (char *filename, FILE *mesgfp);
	long	WriteHeightField (char *filename);
	JPLPic *HorizontalMirrorImage (void);
	JPLPic *HorizontalMirrorImage (JPLPic *result);
	JPLPic	*Clone (void);
	long	Overlay (JPLPic *ontopPic, long row, long col,
				 int wrap);
	long	Copy (JPLPic *dstPic)
	  { return Copy (dstPic, 0, 0); }
	long	Copy (JPLPic *dstPic, long start_row, long start_col);
	long	Alias (JPLPic *dstPic, int swap_release_mem);
	long	Alias (JPLPic *dstPic)
	  { return Alias (dstPic, 0); }
	JPLPic	*Alias (void);
	JPLPic	*SubImage (long startRow, long startCol,
				   long numRows, long nCols);
	JPLPic *SubsamplePyrLevel (int pyrlevel)
	  { return SubsamplePyrLevel (pyrlevel, pyrlevel); }
	JPLPic	*SubsamplePyrLevel (int rpyrlevel, int cpyrlevel);
	long ExtractBand (JPLPic *result, int band);

	long    MiniMax (double d, int use_min);
	long    MiniMax (JPLPic *other, int use_min);

	long	LinearOperator (double mult, double add);
	long	LinearOperator (JPLPic *multpic, JPLPic *addpic);
	long	AbsSub (JPLPic *other);
	JPLPic *ApplyFilter (JPLPic *filter);
	long PyramidLevel (JPLPic *result,
				   int rpyrlevel, int cpyrlevel);
	long PyramidLevel (JPLPic *result, int pyrlevel)
	  { return PyramidLevel (result, pyrlevel, pyrlevel); }
	long Resample (JPLPic *result, long nrows, long ncols);
	JPLPic *ResampleUsingUnits (JPLPic *result,
				    float new_row_inc,
				    float new_col_inc,
				    int force_odd_dimensions,
				    char *desc);

	long Rotate (JPLPic *result, float degrees_ccw,
			     long src_center_row, long src_center_col,
			     long dest_center_row, long dest_center_col);
	long Rotate (JPLPic *result, float degrees_ccw)
	  { return Rotate (result, degrees_ccw, rows/2, cols/2,
			   result ? result->rows/2 : 0,
			   result ? result->cols/2 : 0); }
	JPLPic  *Make8BitImage (void);
	JPLPic  *Make8BitImage (int band, float *actual_min,
				float *actual_mid,
				float *actual_max);
	  long  Make8BitImage (JPLPic *result, int bits_to_shift);
	  long  Make8BitImage (JPLPic *result, int band, int bits_to_shift,
			       float *actual_min, float *actual_mid,
			       float *actual_max);
	JPLPic	*MakeScaled8BitImage (void);
	long	MakeScaled8BitImage (JPLPic *result);
	JPLPic *MakeScaled8BitImage (int band, float *actual_min,
				     float *actual_mid,
				     float *actual_max);
	long	MakeScaled8BitImage (JPLPic *result,
				     int use_bounds, float low, float high,
				     int band);
	long	MakeScaled8BitImage (JPLPic *result,
				     int use_bounds, float low, float high,
				     int band, float *actual_min,
				     float *actual_mid, float *actual_max);
	long    SubImage (JPLPic *dstPic, 
				  long startRow, long startCol,
				  long numRows, long nCols);
	long            ClearImage (unsigned char cmap_val);
	long		ClearImage () { return ClearImage(0); }

	// Image Processing

	long            ResetBorder (int up, int bottom, int left, int right,
				     unsigned char cmap_val);
	long            ComputeThreshold (int *lowT, int *highT,
					  float percent);
	long            Threshold (JPLPic *resultBytePic, int threshold);
	long		SeparateFields (JPLPic *evenField, JPLPic *oddField);

	JPLPic		*BuildARGB (JPLPic *greyscale)
		{ return BuildARGB (greyscale, greyscale, greyscale); }
	JPLPic		*BuildARGB (JPLPic *red, JPLPic *green, JPLPic *blue);
	JPLPic		*BuildARGB (long nrows, long ncols, unsigned char *red,
				    unsigned char *green, unsigned char *blue)
	  { return BuildARGB (nrows, ncols, NULL, red, green, blue); }
	JPLPic		*BuildARGB (long nrows, long ncols, unsigned char *alpha,
				    unsigned char *red,
				    unsigned char *green, unsigned char *blue);

#ifdef 		MAC_PLATFORM
	long	ShowOnScreen (WindowPtr *win, Str255 title,
					    Boolean resize, Boolean scale,
					    float offset, float scaleFactor);
	long 		ShowSubpixelDisparity (WindowPtr *win, Str255 title, 
					       long minDisp, long maxDisp,
					       long flag);
#endif
	
	long		DOG (JPLPic *resPic); 
	long BlobFiltering (long maxNumRegions,
			    long minRegionSize, long *numRegions,
			    long *pointsFiltered,
			    unsigned char background_pixel,
			    long *max_region_row, long *max_region_col,
			    long *max_region_pixels);
	long BlobFiltering (long maxNumRegions,
			    long minRegionSize, long *numRegions,
			    long *pointsFiltered,
			    unsigned char background_pixel)
	  { return BlobFiltering (maxNumRegions, minRegionSize, numRegions,
				  pointsFiltered, background_pixel,
				  (long *) NULL, (long *) NULL,
				  (long *) NULL); }

	long FindSun (float *row, float *col);

	// Gaussian Decimation
	long	DecimateByHalf (JPLPic *gauss, JPLPic *laplacian);

	// Fast processing using sliding sum
	long	SmoothBySlidingSum (JPLPic *newPic, unsigned char winSizeX, 
				    unsigned char winSizeY,
				    unsigned char bound_pixel);
	long 	SmoothAndDecimateBySlidingSum (JPLPic *newPic,
					       unsigned char winSizeX, 
					       unsigned char bound_pixel);
	// 16-bit pixel result
	long 	SmoothAndDecimateBySlidingSumB (JPLPic *newPic,
					       unsigned char winSizeX, 
					       unsigned char bound_pixel);
	long	DoGBySlidingSum (JPLPic *newPic, unsigned char winSize1X, 
				 unsigned char winSize1Y,
				 unsigned char winSize2X,
				 unsigned char winSize2Y,
				 unsigned char bound_pixel);
	long	DoGAndDecimateBySlidingSum (JPLPic *newPic,
					    unsigned char winSize1,
					    unsigned char winSize2,
					    unsigned char bound_pixel);
	long 	GradientX (JPLPic *gradImg);
	long    GradientY (JPLPic *gradImg);
	// 16-bit images: These two functions are not tested yet!!
	long	GradientXB (JPLPic *gradImg, int offset);
	long	GradientYB (JPLPic *gradImg, int offset);

	// 16-bit 
	long    Gradients (JPLPic *gradx, JPLPic *grady,
			   unsigned char winSizeX, unsigned char winSizeY,
			   unsigned char shiftBits, int bound_pixel);

	int BV_GXGY(int x, int y,  double *bv, double *gx, double *gy);
	int InterpolateBV(double p[2], double *bv);

	int	BytesPerPixel ()
	  { return BytesPerPixel (pixelType); }
	int	BytesPerPixel (long type)
	  {  static int bpp[END_PIXEL] = {1, 2, 4, 4, 4, 8, 12, 24};
	  return (type < 0 || type >= END_PIXEL) ? 0 : bpp[type];
	  }

	int	BandsPerPixel ()
	  { return BandsPerPixel (pixelType); }
	int	BandsPerPixel (long type)
	  {  static int bpp[END_PIXEL] = {1, 1, 1, 1, 4, 1, 3, 3};
	  return (type < 0 || type >= END_PIXEL) ? 1 : bpp[type];
	  }

	// utilities
	JMemoryManager   *GetMM () { return mm; };

	// Return the address of the upper left pixel, casting the return
	// value to the appropriate type
	unsigned char	*GetPixelAddress () { return pixels; };
	short 	 	*GetShortPixelAddress ()
	  { return GetShortPixelAddress (0, 0); }
	unsigned short 	*GetUShortPixelAddress ()
	  { return GetUShortPixelAddress (0, 0); }
	unsigned int 	*GetUIntPixelAddress ()
	  { return GetUIntPixelAddress (0, 0); }
	unsigned long 	*GetULongPixelAddress ()
	  { return GetULongPixelAddress (0, 0); }
	float 	 	*GetFloatPixelAddress ()
	  { return GetFloatPixelAddress (0, 0); }
	double 	 	*GetDoublePixelAddress ()
	  { return GetDoublePixelAddress (0, 0); }
	
	// Return the address of any pixel in the image, casting the return
	// value to the appropriate type.  Ensures that the address is
	// word aligned; if not, returns NULL
	unsigned char	*GetPixelAddress (int row, int col)
	  { return GetPixelAddress (row, col, 0); }
	short 	 	*GetShortPixelAddress (int row, int col)
	  { AnythingT p; p.uc = GetPixelAddress(row, col);
	    return (((uintptr_t) p.h) & 1) ? ((short *) NULL) : p.h; }
	unsigned short 	 *GetUShortPixelAddress (int row, int col)
	  { AnythingT p; p.uc = GetPixelAddress (row, col);
	    return (p.ulv & 1) ? ((unsigned short *) NULL) : p.uh; }
	unsigned int 	 *GetUIntPixelAddress (int row, int col)
	  { AnythingT p; p.uc = GetPixelAddress (row, col);
	    return (p.ulv & 3) ? ((unsigned int *) NULL) : p.ui; }
	unsigned long	 *GetULongPixelAddress (int row, int col)
	  { AnythingT p; p.uc = GetPixelAddress (row, col);
	    return (p.ulv & 3) ? ((unsigned long *) NULL) : p.ul; }
	float 	 	*GetFloatPixelAddress (int row, int col)
	  { AnythingT p; p.uc = GetPixelAddress (row, col);
	    return (p.ulv & 3) ? ((float *) NULL) : p.f;}
	double 	 	*GetDoublePixelAddress (int row, int col)
	  { AnythingT p; p.uc = GetPixelAddress (row, col);
	    return (p.ulv & 3) ? ((double *) NULL) : p.d;}

	// Return the address of any pixel in the image, wrapping pixel
	// coordinates if they lie outside the immediate bounds, and
	// casting the return value to the appropriate type.  Ensures that
	// the address is word aligned; if not, returns NULL
	unsigned char	*GetPixelAddress (int row, int col, int wrap);
	short 	 	*GetShortPixelAddress (int row, int col, int wrap)
		{ AnythingT p; p.uc = GetPixelAddress(row, col, wrap);
		 return p.h; }
	unsigned short 	 *GetUShortPixelAddress (int row, int col, int wrap)
		{ AnythingT p; p.uc = GetPixelAddress (row, col, wrap);
		  return (p.ulv & 1) ? ((unsigned short *) NULL) : p.uh; }
	unsigned int 	 *GetUIntPixelAddress (int row, int col, int wrap)
		{ AnythingT p; p.uc = GetPixelAddress (row, col, wrap);
		  return (p.ulv & 3) ? ((unsigned int *) NULL) : p.ui; }
	unsigned long	 *GetULongPixelAddress (int row, int col, int wrap)
		{ AnythingT p; p.uc = GetPixelAddress (row, col, wrap);
		  return (p.ulv & 3) ? ((unsigned long *) NULL) : p.ul; }
	float 	 	*GetFloatPixelAddress (int row, int col, int wrap)
		{ AnythingT p; p.uc = GetPixelAddress (row, col, wrap);
		  return (p.ulv & 3) ? ((float *) NULL) : p.f;}
	double 	 	*GetDoublePixelAddress (int row, int col, int wrap)
		{ AnythingT p; p.uc = GetPixelAddress (row, col, wrap);
		  return (p.ulv & 3) ? ((double *) NULL) : p.d;}


	// Return the address of any pixel in the image, casting the
	// return value to the appropriate type.  Ensures that the
	// address is word aligned; if not, returns NULL.  Uses the
	// linearly scaled float values associated with this image
	// instead of raw row and column numbers.

	unsigned char	*GetPixelUnitsAddress (float frow, float fcol)
	  { return GetPixelUnitsAddress (frow, fcol, 0); }
	short 	 	*GetShortPixelUnitsAddress (float frow, float fcol)
		{ AnythingT p; p.uc = GetPixelUnitsAddress(frow, fcol); return p.h; }
	unsigned short 	 *GetUShortPixelUnitsAddress (float frow, float fcol)
		{ AnythingT p; p.uc = GetPixelUnitsAddress (frow, fcol);
		  return (p.ulv & 1) ? ((unsigned short *) NULL) : p.uh; }
	unsigned int 	 *GetUIntPixelUnitsAddress (float frow, float fcol)
		{ AnythingT p; p.uc = GetPixelUnitsAddress (frow, fcol);
		  return (p.ulv & 3) ? ((unsigned int *) NULL) : p.ui; }
	unsigned long	 *GetULongPixelUnitsAddress (float frow, float fcol)
		{ AnythingT p; p.uc = GetPixelUnitsAddress (frow, fcol);
		  return (p.ulv & 3) ? ((unsigned long *) NULL) : p.ul; }
	float 	 	*GetFloatPixelUnitsAddress (float frow, float fcol)
		{ AnythingT p; p.uc = GetPixelUnitsAddress (frow, fcol);
		  return (p.ulv & 3) ? ((float *) NULL) : p.f;}
	double 	 	*GetDoublePixelUnitsAddress (float frow, float fcol)
		{ AnythingT p; p.uc = GetPixelUnitsAddress (frow, fcol);
		  return (p.ulv & 3) ? ((double *) NULL) : p.d;}


	// Return the address of any pixel in the image, wrapping
	// pixel coordinates if they lie outside the immediate bounds,
	// and casting the return value to the appropriate type.
	// Ensures that the address is word aligned; if not, returns
	// NULL.  Uses the linearly scaled float values associated
	// with this image instead of raw row and column numbers.

	unsigned char	*GetPixelUnitsAddress (float frow, float fcol, int wrap);
	short 	 	*GetShortPixelUnitsAddress (float frow, float fcol, int wrap)
	  { AnythingT p; p.uc = GetPixelUnitsAddress(frow, fcol, wrap);
	    return p.h; }
	unsigned short 	 *GetUShortPixelUnitsAddress (float frow, float fcol, int wrap)
	  { AnythingT p; p.uc = GetPixelUnitsAddress (frow, fcol, wrap);
	    return (p.ulv & 1) ? ((unsigned short *) NULL) : p.uh; }
	unsigned int 	 *GetUIntPixelUnitsAddress (float frow, float fcol, int wrap)
	  { AnythingT p; p.uc = GetPixelUnitsAddress (frow, fcol, wrap);
	    return (p.ulv & 3) ? ((unsigned int *) NULL) : p.ui; }
	unsigned long	 *GetULongPixelUnitsAddress (float frow, float fcol, int wrap)
	  { AnythingT p; p.uc = GetPixelUnitsAddress (frow, fcol, wrap);
	    return (p.ulv & 3) ? ((unsigned long *) NULL) : p.ul; }
	float 	 	*GetFloatPixelUnitsAddress (float frow, float fcol, int wrap)
	  { AnythingT p; p.uc = GetPixelUnitsAddress (frow, fcol, wrap);
	    return (p.ulv & 3) ? ((float *) NULL) : p.f;}
	double 	 	*GetDoublePixelUnitsAddress (float frow, float fcol, int wrap)
	  { AnythingT p; p.uc = GetPixelUnitsAddress (frow, fcol, wrap);
	    return (p.ulv & 3) ? ((double *) NULL) : p.d;}


	// Perform the mapping from the linearly scaled float values
	// into row and column values

	long  GetPixelUnitsRowCol (float frow, float fcol, int scale,
			      int wrap, long *rowp, long *colp);
	// Perform the mapping from row/col into scaled float values.
	// The float value returned is at the upper left corner of the cell.

	long GetUpperLeftPixelUnitsFromRowCol (long row, long col, int wrap,
					       float anchor_units_row,
					       float anchor_units_col,
					       float *row_units,
					       float *col_units);
	void GetBestWraparoundDeltaBetween (long from_row, long from_col,
					    long to_row, long to_col,
					    long *delta_row, long *delta_col);

	float GetPixelUnitsRowInc ()
	  { return row_inc; }
	float GetPixelUnitsColInc ()
	  { return col_inc; }
	float GetPixelUnitsRowOffset ()
	  { return row_offset; }
	float GetPixelUnitsColOffset ()
	  { return col_offset; }

	long AnnotateUnits (JPLPic *result, char *title,
			    int scale, float rperiod,
			    float rfixed, float cperiod, float cfixed,
			    float new_gamma)
	  { return AnnotateUnits (result, title, 0, 0, scale, rperiod, rfixed,
				  cperiod, cfixed, new_gamma, 11,
				  NULL, 0, NULL, 0, NULL, 0, NULL, 0, NULL, 0,
				  0, 0, 0, NULL, -1, -1); }
	long AnnotateUnits (JPLPic *result, char *title,
			    long row_offset, long col_offset,
			    int scale, float rperiod,
			    float rfixed, float cperiod, float cfixed,
			    float new_gamma, unsigned char special,
			    unsigned char *special_color,
			    int special_color_size,
			    unsigned char *text_color,
			    int text_color_size, unsigned char *bg_color,
			    int bg_color_size, unsigned char *period_color,
			    int period_color_size, unsigned char *origin_color,
			    int origin_color_size, int wrap_origin, int band,
			    int force_8bit_scaling, char *pixel_type,
			    long therm_start_row, long therm_end_row);

	long GetRowColNearestXY (double x, double y, double threshold,
				 double ignore_value, double *row, double *col);

	void SetPixelAddress (unsigned char *c, long size, char ok_to_delete)
	  {pixels = c; actualBytes = size; releaseMem = ok_to_delete;};
	long		GetPixelType () {return pixelType;};
	void		SetPixelType (long type) {pixelType = type;};
	long		GetRowBytes () {return rowBytes;};
	void		SetRowBytes (long rb) {rowBytes = rb;};
	unsigned long	GetFlags () {return flags;}
	void		SetFlags (unsigned long new_flag)
	  { flags |= new_flag; }
	void		ClearFlags (unsigned long new_flag)
	  { flags &= ~new_flag; }
	void		ZeroFlags () {flags = 0;}
	char 		*PixelTag2s ()
	  { return PixelTag2s ((enum PixelTag) pixelType); }
	char		*PixelTag2s (enum PixelTag pt)
	  { return PixelTag2s (pt, NULL); }
	char	        *PixelTag2s (enum PixelTag pt, char *buf);
	enum PixelTag	S2PixelTag (char *str);

	//  The next routines draw simple graphics into an image.  Each
	//  has colormap "val" and "just_shift" parameters.  If just_shift
	//  is nonzero, then the current contents of the pixel will be
	//  shifted by 128 to hilight it.  If just_shift is zero, then
	//  the val'th entry in the colormap will be used to set each
	//  pixel.  Even if the pixelType is UC8_PIXEL,
	//  the unsigned char values will be treated as indices
	//  into the current color map.

	long		SetPixel (long r, long c,
				  unsigned char val, int just_shift)
	  { return SetPixel (r, c, 0, val, just_shift); }
	long		SetPixel (long r, long c, int wrap,
				  unsigned char val, int just_shift);
	void 		DefineColorMapMem (int size);
	void		LoadColorMap (unsigned char *map, int size);
	void		LoadGreyColorMap ()
		{ LoadGreyColorMap (0.0, 0); }
	void		LoadGreyColorMap (float Gamma, int contour_period);
	void		LoadRainbowColorMap (void);
	void 		LoadInterpolatedColorMap (unsigned char *background,
						  unsigned char *foreground,
						  int size,
						  float gamma);
	void		LoadColorMapEntry (unsigned char entry,
					   unsigned char *ptr, int size);
	unsigned char *GetColorMapEntry (unsigned char entry);
	int ColorMapEntryRepeatsOneByte (unsigned char byte_val,
					 unsigned char *mem_val);

	void 		PrintColorMap (FILE *fp);
	JPLPic *BuildARGBFromColormap (JPLPic *input, unsigned char *clrmap,
				       int clrmap_size);

	void		Line (long r0, long c0, long r1, long c1)
	  { Line (r0, c0, r1, c1, 0, 1); }
	void 		Line (long r0, long c0, long r1, long c1,
			      unsigned char byte_val, int just_shift)
	  { Line (r0, c0, r1, c1, 0, byte_val, just_shift); }
	void 		Line (long r0, long c0, long r1, long c1, int wrap,
			      unsigned char byte_val, int just_shift);

	void		Polygon (long *prows, long *pcols, long count)
	  { Polygon (prows, pcols, count, 0, 1); }
	void		Polygon (long *prows, long *pcols, long count,
				 unsigned char byte_val, int just_shift)
	  { Polygon (prows, pcols, count, 0, byte_val, just_shift); }
	void		Polygon (long *prows, long *pcols, long count,
				 int wrap, unsigned char byte_val,
				 int just_shift);
	void		PolygonOutline (long *prows, long *pcols, long count)
	  { PolygonOutline (prows, pcols, count, 0, 1); }
	void		PolygonOutline (long *prows, long *pcols, long count,
					unsigned char byte_val, int just_shift)
	  { PolygonOutline (prows, pcols, count, 0, byte_val, just_shift); }
	void		PolygonOutline (long *prows, long *pcols, long count,
					int wrap, unsigned char byte_val,
					int just_shift);

	void 		AntiAliasedCircle (long center_row, long center_col,
					   long radius, unsigned char byte_val,
					   int just_shift)
	  { AntiAliasedCircle (center_row, center_col, radius, 0,
			       byte_val, just_shift); }
	void 		AntiAliasedCircle (long center_row, long center_col,
					   long radius, int wrap_around,
					   unsigned char byte_val,
					   int just_shift);

	// When specifying arcs, theta=0 when pointing due east, theta=PI/2
	// when pointing due north.  Positive delta_theta is counterclockwise.

	void AntiAliasedArc (long center_row, long center_col, float radius,
			     float start_theta, float delta_theta,
			     int wrap, unsigned char byte_val, int just_shift);

	void		Arc (long center_row, long center_col, float radius,
			     float start_theta, float delta_theta,
			     unsigned char byte_val, int just_shift)
	  { Arc (center_row, center_col, radius, start_theta, delta_theta, 0,
		 byte_val, just_shift); }
	void		Arc (long center_row, long center_col, float radius,
			     float start_theta, float delta_theta,
			     int wrap_around,
			     unsigned char byte_val, int just_shift);
	void 		Circle (long center_row, long center_col,
				float radius, unsigned char byte_val,
				int just_shift)
	  { Circle (center_row, center_col, radius, 0,
		    0, byte_val, just_shift); }
	void 		Circle (long center_row, long center_col,
				float radius, int fill, int wrap_around,
				unsigned char byte_val, int just_shift);
	void            Box (long top_row, long top_col, long row_size,
			     long col_size, int wrap,
			     unsigned char byte_val, int just_shift);
	void            Box (long top_row, long top_col, long row_size,
			     long col_size, unsigned char byte_val,
			     int just_shift)
	  { Box(top_row, top_col, row_size, col_size,
		0, byte_val, just_shift); }
	void 		Box (long top_row, long top_col, long row_size,
			     long col_size, unsigned char byte_val)
	  { Box(top_row, top_col, row_size, col_size, byte_val, 0); }
	void		Box (long top_row, long top_col, long row_size,
			     long col_size)
	  { Box(top_row, top_col, row_size, col_size, 0, 1); };
	void            FillBox (long top_row, long top_col, long row_size,
				 long col_size, int wrap,
				 unsigned char byte_val, int just_shift);
	void            FillBox (long top_row, long top_col, long row_size,
				 long col_size, unsigned char byte_val,
				 int just_shift)
	  { FillBox(top_row, top_col, row_size, col_size,
		    0, byte_val, just_shift); }
	void 		FillBox (long top_row, long top_col, long row_size,
				 long col_size, unsigned char byte_val)
	  { FillBox(top_row, top_col, row_size, col_size, byte_val, 0); }
	void		FillBox (long top_row, long top_col, long row_size,
				 long col_size)
	  { FillBox(top_row, top_col, row_size, col_size, 0, 1); };
	void		FillCircle (long center_row, long center_col,
				    float radius, int wrap,
				    unsigned char byte_val, int just_shift)
	  { Circle (center_row, center_col, radius, 1,
		    wrap, byte_val, just_shift); }
	void Number (int r, int c, long num)
	  { Number (r, c, num, 10, 255, 1, 1); }
	void Number (int r, int c, long num, int scale)
	  { Number (r, c, num, 10, 255, 1, scale); }
	void Number (int r, int c, long num, unsigned char pixel,
		     int scale)
	  { Number (r, c, num, 10, pixel, 1, scale); }
	void Number (int r, int c, long num, long base,
		     unsigned char pixel, int scale)
	  { Number (r, c, num, base, pixel, 1, scale); }
	void Number (int r, int c, long num, long base, unsigned char pixel,
		     int darken_background, int scale)
	  { Number (r, c, num, base, pixel, darken_background, 0, scale); }
	void Number (int r, int c, long num, long base, unsigned char pixel,
		     int darken_background, int wrap, int scale);

	void TextBoundingBox (const char *str, int scale,
			      int *rows, int *cols);
	void Text (int r, int c, const char *str)
	  { Text (r, c, str, 255, 1, 1); }
	void Text (int r, int c, const char *str, int scale)
	  { Text (r, c, str, 255, 1, scale); }
	void Text (int r, int c, const char *str, unsigned char pixel,
		   int scale)
	  { Text (r, c, str, pixel, 1, scale); }
	void Text (int r, int c, const char *str, unsigned char pixel,
		   int darken_background, int scale)
	  { Text (r, c, str, pixel, darken_background, 0, scale); }
	void Text (int r, int c, const char *str, unsigned char pixel,
		   int darken_background, int wrap, int scale);

	// Generate ASCII-readable interpretations of the image

	int        ExposureStats (int x0, int y0, int nx,int ny,
				  int underRef, int overRef,
				  float *underExpo, float *overExpo,
				  float *mean, float *var, long *numPixels);

	void ComputePixelStats (double *mean_p, double *stddev_p,
				double *min_p, double *max_p,
				unsigned long *empties_p,
				unsigned long *saturated_p,
				unsigned long *hi_mask,
				unsigned long *lo_mask);
	void ComputePixelStats (int use_bounds, double low, double high,
				int band,
				double *mean_p, double *stddev_p,
				double *min_p, double *max_p,
				unsigned long *empties_p,
				unsigned long *saturated_p,
				unsigned long *hi_mask,
				unsigned long *lo_mask);
	void ComputePixelStats (int use_bounds, double low, double high,
				int band,
				double *mean_p, double *stddev_p,
				double *min_p, double *max_p,
				unsigned long *empties_p,
				double saturated_cutoff,
				unsigned long *saturated_p,
				unsigned long *hi_mask,
				unsigned long *lo_mask);

	void WriteStats (void);
	void WriteStats (char *filename);
	void WriteStats (int band);
	void WriteStats (FILE *fp, const char *prefix);
	void WriteStats (FILE *fp, const char *prefix,
			 int use_bounds, float low, float high);
	void WriteStats (FILE *fp, const char *prefix,
			 int use_bounds, float low, float high, int band);

	unsigned char *RenderAsCode (void);
	unsigned char *RenderAsCode (unsigned char *buf, unsigned long
				     *bytes_required);
	// Returns number of characters parsed, or negative number on error
	long ParseCodeString (char *input, long input_length,
			      unsigned char *output, long *output_length);
	// Returns number of characters parsed, or negative number on error
	long ParseCodeFile (char *filename,
			    unsigned char *output, long *output_length);
	long LoadCodeFromMemory (unsigned char *buf, unsigned long length);

	void RenderInASCII (void);
	void RenderInASCII (int max_dim);
	void RenderInASCII (int screen_max_rows, int screen_max_cols);
	void RenderInASCII (FILE *fp, int screen_max_rows,
			    int screen_max_cols, int scale);
	long **GetClutAddr ()
	  { return &clut; }

	// Class variables

	long 	rows, cols;	// Should be protected, but are used all
	// over the place in existing code
	char	field;	// whether to use only a field or the whole frame.
	unsigned long	flags;

	//protected:

	JMemoryManager *mm;

// Here we allocate enough space to store an RGB colormap (768 bytes).
// However, the max possible storage (one that would be needed for
// an XYZ_DOUBLE_PIXEL image) takes about 6K (MAX_CMAP_SIZE), which is 100
// times the size of the parameters alone. cmapbuf will point to a
// dynamically allocated buffer of that size if anything larger than
// the default size is requested (see DefineColorMapMem).

	unsigned char cmap_dflt[DFLT_CMAP_SIZE];	// Default colormap
	unsigned char *cmapbuf; // Generalized ColorMap
	AnythingT cmap;	// Points into cmapbuf

	// Associate a linear scaling of Units with both row and
	// column indices.  Row 0 and Col 0 start in the upper left of
	// the image, *and* in the upper left corner of that pixel
	// These are only considered valid if the _units string is not
	// NULL.

	float row_offset, row_inc;
	float col_offset, col_inc;
	char *row_units, *col_units;
	char free_row_units, free_col_units;

	long 		pixelType; // pixel type
	long 		actualBytes; // num bytes allocated in   pixels
	unsigned char 	*pixels;	// pointer to pixel memory
	long 	rowBytes;  // how many bytes to jump from one row to the next
	char	releaseMem;	// whether to release memory when destroyed.
	long	*clut;		// color lookup table
};


extern "C" {
#endif /* __cplusplus */

void PrintPixelTags (FILE *fp);
void FreeJPLPicStaticBuffers (JMemoryManager *mm);

#ifdef __cplusplus
}
#endif /* __cplusplus */


#endif // __JPL_PIC__
