#include "nav_memory.h"

#include <string.h>	// For memcpy
#include <netinet/in.h>	// htonl()
#include <ctype.h>	// isprint(), isspace()

#include "JPLPic.h"
#include "good_fopen.h"
#include "MDTypes.h"

/* #ifdef __unix__ */
#if 1
#include <unistd.h>
#include <fcntl.h>    // For open()
#else
#include <ioLib.h>
#endif


#ifdef EXTRA_JPLPIC_WRITES
#include "GifUtils.h"
#endif

#if 0
#ifndef RSC
#ifdef __unix__
#ifndef MSP
#define NEW_READ -- NOTE!!! This will require you to link with -limg -lmwm
#include "image_parse.h"
#endif
#endif
#endif
#endif


#ifdef NEW_READ
#include "image_parse.h"
#else
#define SUCCESS (0)
#define FAILURE (1)
#endif



long JPLPic::LoadFromMemory (unsigned char *start,
					  long nrows, long ncols, long type)
{
  long newsize = nrows * ncols;
#ifdef MEM_DEBUG
  int bytes = BytesPerPixel(pixelType);
#endif
  long status = NO_ERR;

#ifdef MEM_DEBUG
  DBG(("*** LoadFromMemory; newsize=0x%lx, oldsize=0x%lx\n",
	   newsize * BytesPerPixel(type), (long) rows * cols * bytes));
  DBG(("rows=0x%lx cols=0x%lx bytes=%d\n", rows, cols, bytes));
  if (((long) pixels) == 0xfc2780) DBG(("Init: pixels=%lx\n", (long)pixels));
  if (((long) start) == 0xfc2780) DBG(("Init: start=%lx\n", (long)start));

#endif

  // Init will only allocate more memory when required
  status = Init (nrows, ncols, type, mm);
  
  if (status != NO_ERR)
    return status;

  memcpy (pixels, start, newsize * BytesPerPixel (type));

  field = 0;
  if (clut) DELETEV (mm, clut);
  clut = (long *) NULL;

#ifdef MEM_DEBUG
  DBG(("=== Leaving LoadFromMemory\n"));
#endif


  return status;

} /* LoadFromMemory */




long JPLPic::AliasFromMemory (unsigned char *start,
					   long nrows, long ncols, long type)
{
  SetPixelAddress (start, nrows * ncols * BytesPerPixel(type), false);
  rows = nrows;
  cols = ncols;
  SetPixelType (type);
  SetRowBytes (ncols * BytesPerPixel(type));
  field = false;
  clut = NULL;
  return NO_ERR;
}




long JPLPic::Read (char *filename, unsigned char fieldOnly)
{
  FILE *fp;
  long status;

  if (filename == NULL) {
    FatalErr ("Read:  empty filename\n");
    return FILE_ERR;
  }

  if ((fp = fopen (filename, "rb")) == NULL) {
    error(ERR(6), ERR_MINOR, ERF_COMMAND, "Read:  cannot open \"%s\"\n", filename);
    return FILE_ERR;
  }
  status = Read (fp, fieldOnly);
  fclose (fp);
  return status;
}


long JPLPic::Read (FILE *picFile, unsigned char fieldOnly)
{
	Int32 firstWord, secondWord;
	unsigned char *c;
	Int16	*type;
	long row;
	long newType;
	int swap_bytes = 0;

	if ((fread((void *) &firstWord, 1, 4, picFile) != 4) || 
		(fread((void *) &secondWord, 1, 4, picFile) != 4)) {
		 return (FILE_ERR);
	}

	c = (unsigned char *) &firstWord;
	if (c[0] != 'Y' || c[1] != 'X') {
	  // Warning ("JPL old pic format\n");
	}
	
	rows = firstWord & 0x0000FFFF;
	cols = secondWord & 0x0000FFFF;
	// HACK!!  Handle sparc-generated pics on linux
	if (rows == 0 && cols == 0) {
	  swap_bytes = 1;
	  rows = ((firstWord & 0xff000000) >> 24) |
	    ((firstWord & 0x00ff0000) >> 8);
	  cols = ((secondWord & 0xff000000) >> 24) |
	    ((secondWord & 0xff0000) >> 8);
	}
	
	type = (short *) &secondWord;
	newType = (long) *type;
	int status = Init(rows, cols, newType);

	if (status != NO_ERR)
	  return status;

	// default to interlaced signal
	field = fieldOnly;
	
	if (rows * rowBytes > actualBytes) {
	  error(ERR(7), ERR_MAJOR, ERF_COMMAND,
		   "Read:  ERROR!!!  Trying to load %ld bytes into %ld!\n",
		   (long) rows * rowBytes, actualBytes);
	}

	unsigned char sw;
	int pixSize = BytesPerPixel (newType);
	int pixHalfSize = pixSize / 2;

	for (row = rows, c = pixels; row--; c += rowBytes) {
	  if ((unsigned long) fread(c, 1, rowBytes, picFile) !=
	      (unsigned long) rowBytes) {
	    return (FILE_ERR);	
	  } else {
	    if (swap_bytes && pixHalfSize) {
	      for (unsigned char *swapc = c; swapc < c + rowBytes;
		   swapc += pixSize)
		for (int i = 0; i < pixHalfSize; i++) {
		  sw = c[i]; c[i] = c[pixSize-(i+1)]; c[pixSize-(i+1)] = sw;
		}
	    }
	  }
	}
	
	return (NO_ERR);
}



long JPLPic::WriteFD (int fd)
{
  Int32 firstWord, secondWord;
  unsigned char *c;
  long row, bytesPerRow;
  
  firstWord = rows;
  secondWord = cols;

  if ((write(fd, (char *) &firstWord, 4) != 4) || 
      (write(fd, (char *) &secondWord, 4) != 4)) {
    return (FILE_ERR);
  }
  
  if (pixels == NULL || rows * cols == 0)
    return NO_ERR;
  
  bytesPerRow = BytesPerPixel (pixelType) * cols;
  
  // Certain JPLPics can be written out quickly.  Fast track writing:

  if (pixelType == UC8_PIXEL && (flags & WRITE_AS_SCALED_8BIT_IMAGE) == 0
      && rowBytes == bytesPerRow) {

    // Write the whole image as one chunk
    if (write (fd, (char *) pixels, rows * cols) != (rows * cols)) {
      error(ERR(9), ERR_MINOR, ERF_COMMAND, "Failed to write complete JPLPic\n");
      return FILE_ERR;
    }
    
    return NO_ERR;
  }

  JPLPic *image = this;

  // Map pixels into 8 bits?

  if (flags & WRITE_AS_SCALED_8BIT_IMAGE)
    image = MakeScaled8BitImage();

  // Some UNIX tools can only read double triplets (not floats).  Optionally
  // write all FLOAT-type images as DOUBLES

  if ((flags & WRITE_FLOATS_AS_DOUBLES) && IS_FLOAT_PIXEL (image->pixelType)) {
    int floats = cols * BytesPerPixel (pixelType) /
      BytesPerPixel (FLOAT_PIXEL);

    for (row = 0; row < rows; row++) {
      AnythingT aptr;
      float *fptr;

      aptr.uc = image->GetPixelAddress(row, 0);
      fptr = aptr.f;
      for (long i = 0; i < floats; i++) {
	double d = (double) *fptr++;
	if (write (fd, (char *) &d, sizeof(double)) != sizeof(double)) {
	  FatalErr ("Failed to write a float\n");
	  if (image != this) DELETE (mm, image);
	  return FILE_ERR;
	}
      }
    }
  } else if (IS_FLOAT_PIXEL (image->pixelType) ||
	     IS_DOUBLE_PIXEL (image->pixelType)) {

    // Handle FLOAT and DOUBLE writing (with optional byte swapping) here

    int floats = cols * BytesPerPixel (pixelType) /
      BytesPerPixel (FLOAT_PIXEL);

    for (row = 0; row < rows; row++) {
      AnythingT aptr;
      float *fptr;

      aptr.uc = image->GetPixelAddress(row, 0);
      fptr = aptr.f;
      for (long i = 0; i < floats; i++, fptr++) {
	float ftmp = *fptr;

	if (flags & WRITE_REALS_SWAPPING_BYTES) {
	  unsigned char *aa, *pp;

	  aa = (unsigned char *) fptr;
	  pp = (unsigned char *) &ftmp;
	  pp[0] = aa[3];
	  pp[1] = aa[2];
	  pp[2] = aa[1];
	  pp[3] = aa[0];
	}

	if (write (fd, (char *) &ftmp, sizeof (float)) != sizeof(float)) {
	  FatalErr ("Failed to write a float\n");
	  if (image != this) DELETE (mm, image);
	  return (FILE_ERR);
	}
      }
    }
  } else {
    // Handle all other pixel types here.

    for (row = rows, c = image->pixels; row --; c += image->rowBytes)
      if (write(fd, (char *) c, bytesPerRow) != 
	  bytesPerRow) {
	if (image != this) DELETE (mm, image);
	return (FILE_ERR);
      }
  }

  if (image != this) DELETE (mm, image);
  return NO_ERR;
}



long JPLPic::WriteFD (char *filename)
{
  int fd;
  //#define __TIMING_SPEED
#ifdef __TIMING_SPEED
  float t1, t2, t3;

  Timing (timeStart);
#endif
  fd = open (filename, O_CREAT | O_RDWR | O_TRUNC, 0644);
  if (fd == 0) {
    error(ERR(10), ERR_MINOR, ERF_COMMAND, "Error opening file: %s\n",
	    filename);
    return FILE_ERR;
  }

#ifdef __TIMING_SPEED
  t1 = Timing(timeElapsed);
#endif

  WriteFD (fd);

#ifdef __TIMING_SPEED
  t2 = Timing(timeElapsed);
#endif

  close (fd);

#ifdef __TIMING_SPEED
  t3 = Timing(timeElapsed);
  error(ERR(11), ERR_INFO, ERF_COMMAND, "Open: %f sec; Write: %f sec; Close: %f sec\n",
	 t1, t2, t3);
#endif

  return NO_ERR;
}



long JPLPic::Write (char *dir, char *filename)
{
  static char *buf = NULL;
  static int buf_len = 0;

  if (dir == NULL)
    return Write (filename);
  else if (filename == NULL)
    return Write (dir);
  else {
    int len = strlen (dir) + strlen (filename) + 2;

    if (len > buf_len) {
      if (buf) DELETEV (mm, buf);
      len = 1024 * ((len + 1023) / 1024);
      buf = NEW(mm, "JPLPic::Write pathname") char[len];
      buf_len = len;
    }
  }
  snprintf (buf, buf_len, "%s%s%s", dir, (dir[strlen(dir)-1] == '/') ? "" : "/",
	   filename);
  return Write (buf);
}



long JPLPic::Write (char *filename)
{
  FILE *fp;
  long l;
  
  if ((fp = good_fopen (filename, "wb")) == NULL)
    return FILE_ERR;

  l = Write (fp);
  fclose(fp);
  return l;
}


long JPLPic::Write (FILE *picFile)
{
  Int32 firstWord, secondWord;
  unsigned char *c;
  long row, bytesPerRow;
  
  firstWord = rows;
  secondWord = cols;
  
  if ((fwrite((void *) &firstWord, 1, 4, picFile) != 4) || 
      (fwrite((void *) &secondWord, 1, 4, picFile ) != 4)) {
    return (FILE_ERR);
  }
	
  bytesPerRow = BytesPerPixel (pixelType) * cols;

  // Certain JPLPics can be written out quickly.  Fast track writing:

  if (pixelType == UC8_PIXEL && (flags & WRITE_AS_SCALED_8BIT_IMAGE) == 0
      && rowBytes == bytesPerRow) {
    
    // Write the whole image as one chunk
    if ((long) fwrite ((void *) pixels, 1, rows * cols, picFile) !=
	(rows * cols)) {
      error(ERR(12), ERR_MINOR, ERF_COMMAND, "Failed to write complete JPLPic\n");
      return FILE_ERR;
    }
    
    return NO_ERR;
  }


#ifdef INTERFACE_DEBUG
  DBG(("Pixel type %ld, bytes per pixel %d\n",
	   pixelType, BytesPerPixel (pixelType)));
#endif

  JPLPic *image = this;

  // Map pixels into 8 bits?

  if (flags & WRITE_AS_SCALED_8BIT_IMAGE) {
    image = MakeScaled8BitImage();
    bytesPerRow = BytesPerPixel(image->GetPixelType()) * cols;
  }

  // Some UNIX tools can only read double triplets (not floats).  Optionally
  // write all FLOAT-type images as DOUBLES

  if ((flags & WRITE_FLOATS_AS_DOUBLES) && IS_FLOAT_PIXEL (image->pixelType)) {
    int floats = cols * BytesPerPixel (pixelType) /
      BytesPerPixel (FLOAT_PIXEL);

    for (row = 0; row < rows; row++) {
      AnythingT aptr;
      float *fptr;

      aptr.uc = image->GetPixelAddress(row, 0);
      fptr = aptr.f;

      for (long i = 0; i < floats; i++) {
	double d = (double) *fptr++;
	if (fwrite ((char *) &d, sizeof(double), 1, picFile) != 1) {
	  FatalErr ("Failed to write a float as double\n");
	  if (image != this) DELETE (mm, image);
	  return FILE_ERR;
	}
      }
    }
  } else if (IS_FLOAT_PIXEL (image->pixelType) ||
	     IS_DOUBLE_PIXEL (image->pixelType)) {

    // Handle FLOAT and DOUBLE writing (with optional byte swapping) here

    int floats = cols * BytesPerPixel (pixelType) /
      BytesPerPixel (FLOAT_PIXEL);

    for (row = 0; row < rows; row++) {
      AnythingT aptr;
      float *fptr;

      aptr.uc = image->GetPixelAddress(row, 0);
      fptr = aptr.f;

      for (long i = 0; i < floats; i++, fptr++) {
	float ftmp = *fptr;

	if (flags & WRITE_REALS_SWAPPING_BYTES) {
	  unsigned char *aa, *pp;

	  aa = (unsigned char *) fptr;
	  pp = (unsigned char *) &ftmp;
	  pp[0] = aa[3];
	  pp[1] = aa[2];
	  pp[2] = aa[1];
	  pp[3] = aa[0];
	}

	if (fwrite ((void *) &ftmp, sizeof (float), 1, picFile) != 1) {
	  FatalErr ("Failed to write a float\n");
	  if (image != this) DELETE (mm, image);
	  return (FILE_ERR);
	}
      }
    }
  } else {
    // Handle all other pixel types here.

    for (row = rows, c = image->pixels; row --; c += image->rowBytes)
      if ((long) fwrite((void *) c, 1, bytesPerRow, picFile) != 
	  bytesPerRow) {
	if (image != this) DELETE (mm, image);
	return (FILE_ERR);
      }
  }

  if (image != this) DELETE (mm, image);
  return NO_ERR;
}


// Returns the PIC PixelType most likely to be assigned to the file.  Does
// *not* guarantee anything; in particular, ARGB32_PIXEL is returned even
// for files with red channel/green channel/blue channel.

enum PixelTag
JPLPic::FilePixelType (char *filename, FILE *mesgfp)
{
  enum PixelTag result = UNKNOWN_PIXEL;
#ifdef NEW_READ
  PicHypothesisT h;

  if (find_pic_params (filename, mesgfp, &h) == 0)
    return UNKNOWN_PIXEL;
  if (h.bands == 1)
    switch (h.bits_per_pixel) {
    case 8: result = UC8_PIXEL; break;
    case 16: result = INT16_PIXEL; break;
    case 24: result = ARGB32_PIXEL; break;
    case 32: result = FLOAT_PIXEL; break;
    case 64: result = DOUBLE_PIXEL; break;
    case 96: result = XYZ_FLOAT_PIXEL; break;
    case 192: result = XYZ_DOUBLE_PIXEL; break;
    default: result = UNKNOWN_PIXEL; break;
    } // switch
  else if (h.bands == 3)
    switch (h.bits_per_pixel) {
    case 8: result = ARGB32_PIXEL; break;
    case 32: result = XYZ_FLOAT_PIXEL; break;
    case 64: result = XYZ_DOUBLE_PIXEL; break;
    default: result = UNKNOWN_PIXEL; break;
    } // switch
  else
    result = UNKNOWN_PIXEL;
#endif
  return result;
} // JPLPic::FilePixelType




long


JPLPic::ReadGenericImage (char *filename, int bitsppb, int bands,
			  enum ImageScaleT scale_func)
{
#ifdef NEW_READ
  ImageT info;
  unsigned char *rptr = NULL, *gptr = NULL, *bptr = NULL;
  int status;
  long pix_type = UC8_PIXEL;

  // HACK HACK HACK -- These routines don't handle .RAN images yet.

  if (bands == 1) {
    status = grey_image_read (NULL, filename, (ImageMemoryT *) &rptr,
			      bitsppb, IF_PIC, scale_func, &info, NULL);
    gptr = bptr = rptr;
  } else
    status = color_image_read (NULL, filename, (ImageMemoryT *) &rptr,
			       (ImageMemoryT *) &gptr, (ImageMemoryT *) &bptr, 
			       bitsppb, IF_PIC, scale_func, &info, NULL);
  if (status == 0)
    return FAILURE;
  switch (info.bitsppb) {
  case 8: pix_type = UC8_PIXEL; break;
  case 16: pix_type = INT16_PIXEL; break;
  case 32: pix_type = FLOAT_PIXEL; break;
  case 64: pix_type = DOUBLE_PIXEL; break;
  case 96: pix_type = XYZ_FLOAT_PIXEL; break;
  case 192: pix_type = XYZ_DOUBLE_PIXEL; break;
  default:
    DBG(("WARNING!! Found %d bpp, %d bands, Assuming 8 bit pixels in %s\n",
	 info.bitsppb, info.bands, filename ? filename : "[null]"));
    break;
  }

  int current_bands = info.bands;

  if (info.bands == 1 && (bands == 1 || bands < 0))
    status = LoadFromMemory (rptr, info.rows, info.cols, pix_type);
  else if (info.bands == 3 || bands == 3) {
    JPLPic *result = BuildARGB (info.rows, info.cols, rptr, gptr, bptr);

    Init (info.rows, info.cols, ARGB32_PIXEL);
    result->Copy (this);
    if (bands == 3 || bands == 4)
      bands = 1;
    current_bands = 1;
    DELETE (mm, result);
  } else if (info.bands == 4) {
    long block = (info.bitsppb >> 3) * info.rows * info.cols;

    JPLPic *result = BuildARGB (info.rows, info.cols, rptr,
				rptr + block, rptr + 2 * block,
				rptr + 3 * block);
    Init (info.rows, info.cols, ARGB32_PIXEL);
    result->Copy (this);
    current_bands = 4;
    DELETE (mm, result);
  } else {
    DBG (("WARNING!!  Only loading first band of %d in image file!\n",
	  info.bands));
    status = LoadFromMemory (rptr, info.rows, info.cols, pix_type);
  }
  free (rptr);

  // Now we've read the image generically.  Here we have to force it
  // into the desired format.

  if (bands > 0 && bands != current_bands) {
    if (bands == 1) {
      JPLPic *result = NEW(mm, "GenericImage 8bit image") JPLPic(mm);
      long status;

      switch (pixelType) {
      case UC8_PIXEL:
	status = Make8BitImage (result, 0);
	break;
      case INT16_PIXEL:
      case INT32_PIXEL:
      case FLOAT_PIXEL:
      case DOUBLE_PIXEL:
      case XYZ_FLOAT_PIXEL:
      case XYZ_DOUBLE_PIXEL:
	status = MakeScaled8BitImage (result);
	break;
      case ARGB32_PIXEL:
	status = Make8BitImage (result, 0);
	break;
      default:
	fprintf (stderr,
		 "ReadGenericImage:  Cannot cast pixtype %ld to 8 bits!\n",
		 pixelType);
	return PARAM_ERR;
      }
      if (status != NO_ERR) {
	DBG (("ReadGenericImage:  Failed to map %s to 8 bit pixels!\n",
	      PixelTag2s ()));
	return status;
      }
      pixelType = UC8_PIXEL;
      result->Copy (this);
      current_bands = 1;
      DELETE (mm, result);
    } else {
      DBG (("Warning! Cannot map %d band %s image into %d bands!\n",
	    current_bands, PixelTag2s (), bands));
    }
  }
	
  return status;
#else
  if (filename) {
    int l = strlen (filename);

    bitsppb += l;	// Dummy assignment; not used 

    if (l > 4 && strcmp (filename + l - 3, "pic") != 0)
      DBG(("WARNING!!! Reading \"%s\" as PIC file\n", filename));
  }
  return Read (filename, 0);
#endif
}



long
JPLPic::WriteGenericImage (char *filename, FILE *mesgfp)
{
  int status = 0;
#ifdef NEW_READ
  ImageT info;

  info.rows = rows;
  info.cols = cols;
  if (flags & WRITE_AS_SCALED_8BIT_IMAGE)
    info.bitsppb = 8;
  else
    info.bitsppb = BytesPerPixel() * 8;

  if (pixelType == ARGB32_PIXEL) {
    JPLPic red(mm), green(mm), blue(mm);
    long r, c;
    ImageMemoryT ptr, rptr, gptr, bptr;

    red.Init (rows, cols, UC8_PIXEL);
    green.Init (rows, cols, UC8_PIXEL);
    blue.Init (rows, cols, UC8_PIXEL);
    rptr.uc = red.GetPixelAddress();
    gptr.uc = green.GetPixelAddress();
    bptr.uc = blue.GetPixelAddress();

    for (r = 0; r < rows; r++) {
      ptr.uc = GetPixelAddress (r, 0);
      for (c = 0; c < cols; c++) {
	*rptr.uc++ = ptr.uc[1];
	*gptr.uc++ = ptr.uc[2];
	*bptr.uc++ = ptr.uc[3];
	ptr.uc += 4;
      }
    }

    rptr.uc = red.GetPixelAddress();
    gptr.uc = green.GetPixelAddress();
    bptr.uc = blue.GetPixelAddress();
    info.bitsppb = 8;

    status = color_image_write (NULL, filename, rptr, gptr, bptr, 8,
				guess_format_from_suffix (filename),
				IS_TRUNCATE, &info, mesgfp);
  } else
    status = grey_image_write (NULL, filename,
			       (unsigned char *) GetPixelAddress(),
			       info.bitsppb,
			       guess_format_from_suffix (filename),
			       IS_TRUNCATE, &info, mesgfp);
#else
  DBG(("WARNING!!! Writing \"%s\" as PIC file\n", filename));
  status = Write (filename);
#endif

  return status;
}

#ifndef NO_RANGE
#define NO_RANGE  -100000.0
#endif


long
JPLPic::WriteHeightField (char *outfile)
{
  long side, r, c;
  union { long l; float f; } u;
  float pad = -10000;		/* HF_UNSET from libray/libobj/hf.h */
  float min_range = -1e6, max_range = -1e6;
  FILE *fp;

  if (outfile == NULL) {
    FatalErr ("No destination file!\n");
    return PARAM_ERR;
  }

  if ((fp = fopen (outfile, "wb")) == NULL) {
    Warning ("WriteHeightField: Failed to open output file for writing");
    return PARAM_ERR;
  }

  if (pixelType != FLOAT_PIXEL && pixelType != DOUBLE_PIXEL &&
      pixelType != UC8_PIXEL && pixelType != INT16_PIXEL &&
      pixelType != INT32_PIXEL && pixelType != ARGB32_PIXEL) {
    fprintf (stderr, "*** Don't know how to convert %s pixels\n",
	     PixelTag2s ());
    fclose (fp);
    return PARAM_ERR;
  }    

  u.f = pad;
  u.l = htonl(u.l);
  pad = u.f;

  side = MAX(rows,cols);
  r = htonl(side);
  fwrite (&r, sizeof(long), 1, fp);

  for (r = side-1; r >= 0; r--) {
    float rng;

    if (r > side - (rows+1)) {
      AnythingT ptr;

      ptr.f = GetFloatPixelAddress (r, 0);

      for (c = 0; c < cols; c++) {
	float val;

	if (ptr.uc == NULL)
	  val = NO_RANGE;
	else
	  switch (pixelType) {
	  case FLOAT_PIXEL:
	    val = *(ptr.f++);
	    break;
	  case DOUBLE_PIXEL:
	    val = (float) *(ptr.d++);
	    break;
	  case UC8_PIXEL:
	    val = ((float) *ptr.uc++) / 255.0;
	    break;
	  case INT16_PIXEL:
	    val = ((float) *ptr.uh++) / 65536.0;
	    break;
	  case INT32_PIXEL:
	    val = ((float) *ptr.ul++) / 65536.0; 	/* HACK - just guessing */
	    break;
	  case ARGB32_PIXEL:
	    val = (((float) ptr.uc[1]) + (float) ptr.uc[2] + (float) ptr.uc[3])
	      / 768.0;
	    ptr.uc += 4;
	    break;
	  default:
	    DBG (("WriteHeightField:  Can't parse %s pixels!\n", 
		  PixelTag2s()));
	    return PARAM_ERR;
	  }

	if (LE (val, NO_RANGE)) {
	  rng = pad;
	} else {
	  rng = val;

	  if (min_range < 0 || LT(rng, min_range))
	    min_range = rng;
	  if (max_range < 0 || GT(rng, max_range))
	    max_range = rng;

	  u.f = rng;
	  u.l = htonl (u.l);
	  rng = u.f;
	}
	fwrite (&rng, sizeof(rng), 1, fp);
      }
      for (c = side - cols; c > 0; c--) {
	fwrite (&pad, sizeof(pad), 1, fp);
      }
    } else {
      for (c = side; c > 0; c--) {
	fwrite (&pad, sizeof(pad), 1, fp);
      }
    }
  }

  fclose (fp);

  printf ("Wrote Heightfield values from %g to %g\n", min_range, max_range);

  return NO_ERR;
}

/* write a 24-bit color PPM image */

long
JPLPic::WritePPM (FILE *fp)
{
  char header[64];
  long *p;
  AnythingT src;
  long row, col;

  if (fp == NULL) {
    FatalErr ("Invalid file pointer in WritePPM\n");
    return INIT_ERR;
  }

  if (pixelType != ARGB32_PIXEL) {
    FatalErr ("The image is not color for PPM\n");
    return INIT_ERR;
  }

  snprintf(header, 64, "P6\n%d %d\n255\n", (int) cols, (int) rows);
  
  if ((int) fwrite ((void *) header, 1, strlen(header), fp) !=
      (int) strlen(header)) {
    FatalErr ("Cannot write the header in PPM\n");
    return FILE_ERR;
  }

  src.uc = pixels;
  for (row = 0; row < rows; row++, src.uc += rowBytes)
    for (col = 0, p = src.l; col < cols; col++, p++) {
      if (fwrite((void *) (((char *) p) + 1), 1, 3, fp) != 3) {
	FatalErr ("Cannot write a color pixel\n");
	return FILE_ERR;
      }
    }

  return NO_ERR;
}

/* write a 8-bit image with a colored mask */
long
JPLPic::WritePPM (FILE *fp, JPLPic *mask)
{
  char header[64];
  unsigned char *p, *p1, colorPixel[3];
  unsigned char *src, *src1;
  long row, col, src1RB;

  if (fp == NULL || mask == NULL) {
    FatalErr ("Invalid file pointer or mask image in WritePPM\n");
    return INIT_ERR;
  }

  if (pixelType != UC8_PIXEL ||
      mask->GetPixelType () != UC8_PIXEL) {
    FatalErr ("The images are 8-bit for PPM\n");
    return INIT_ERR;
  }

  if (mask->rows != rows || mask->cols != cols) {
    FatalErr ("Images are not the same size for PPM\n");
    return INIT_ERR;
  }

  snprintf(header, 64, "P6\n%d %d\n255\n", (int) cols, (int) rows);
  
  if ((int) fwrite ((void *) header, 1, strlen(header), fp) !=
      (int) strlen(header)) {
    FatalErr ("Cannot write the header in PPM\n");
    return FILE_ERR;
  }

  src = pixels;
  src1 = mask->GetPixelAddress();
  src1RB = mask->GetRowBytes ();
  for (row = 0; row < rows; row++, src += rowBytes, src1 += src1RB)
    for (col = 0, p = src, p1 = src1;
	 col < cols; col++, p++, p1++) {
      if ((*p1) != 0)
	colorPixel[0] = *p1;
      else
	colorPixel[0] = *p;
      colorPixel[1] = colorPixel[2] = *p;
      if (fwrite((void *) colorPixel, 1, 3, fp) != 3) {
	FatalErr ("Cannot write a color pixel\n");
	return FILE_ERR;
      }
    }

  return NO_ERR;
}

long
JPLPic::WritePPM (char *filename, JPLPic *mask)
{
  FILE *fp;
  long l;
  
  if (mask == NULL) return INIT_ERR;

  if ((fp = good_fopen (filename, "wb")) == NULL)
    return FILE_ERR;

  l = WritePPM (fp, mask);
  fclose(fp);
  return l;
}

#ifndef OMIT_IMAGE_OPERATORS

#ifdef EXTRA_JPLPIC_WRITES

long 
JPLPic::WriteGIF (char *filename) 
{
  int result = GIF_SUCCESS;
  GifUtils gu(mm);

  if (pixelType == UC8_PIXEL)
    //result = gif_write (filename, rows, cols, (char*) pixels);
    result = gu.WriteStillGif (filename, this);
  else if (pixelType == ARGB32_PIXEL) 
    result = gu.WriteARGBasGif (filename, this, 256);

  else {
    FatalErr ("Not implemented yet\n");
  }

  if (result == GIF_SUCCESS)
    return NO_ERR;
  else
    return FILE_ERR;
}

#endif /* EXTRA_JPLPIC_WRITES */
#endif /* ! OMIT_IMAGE_OPERATORS */



/*!
  Convert one byte to an equivalent representation for an ANSI C
  string-style string.
*/

static void Byte2Code (unsigned char byte, unsigned char *result,
		       unsigned long *bytes_used)
{
  if (byte == '\\') {
    if (result) *result++ = '\\';
    if (result) *result++ = '\\';
    if (bytes_used) (*bytes_used) = 2;
  } else if (byte == '\0') {
    if (result) *result++ = '\\';
    if (result) *result++ = '0';
    if (bytes_used) (*bytes_used) = 2;
  } else if (isprint (byte)) {
    if (result) *result++ = byte;
    if (bytes_used) (*bytes_used) = 1;
  } else {
    if (result) sprintf ((char *) result, "\\%03o", byte);
    if (bytes_used) (*bytes_used) = 4;
  }
}

/*!
  Write out the start of an ANSI C string-based representation of an
  image, an opening double quote
*/

static void CodeStart (int *indent, unsigned char *result,
		       unsigned long *bytes_used)
{
  if (result) strcpy ((char *) result, "\"");
  if (bytes_used) *bytes_used = 1;
  if (indent) *indent += 1;
} // CodeStart


/*!
  Write out the end of an ANSI C string-based representation of an
  image, a closing double quote and newline
*/

static void CodeEnd (int *indent, unsigned char *result,
		     unsigned long *bytes_used)
{
  if (result) strcpy ((char *) result, "\"\n");
  if (bytes_used) *bytes_used = 2;
  if (indent) *indent = 0;
} // CodeEnd


/*!
  Insert a break into an ANSI C string-based representation of an
  image, a close quote, newline, open quote.
*/

static void InsertCodeBreak (int *indent, unsigned char *result,
			     unsigned long *bytes_used)
{
  if (result) strcpy ((char *) result, "\"\n\"");
  if (bytes_used) *bytes_used = 3;
  if (indent) *indent = 1;
} // InsertCodeBreak


/*!
  Convert bytes stored in memory into an ANSI C string-based
  representation.

\param indent optional count of the number of characters indented on
this line.
\param start Start of memory to be re-expressed as code.
\param len Number of memory of memory to re-express as code.
\param tab_every_n_bytes Maximum length of ANSI C string component
before inserting a break.
\param result Buffer into which the ANSI C-based string will be
written.
\param bytes_used Number of bytes written into result.
*/

static void Mem2Code (int *indent, unsigned char *start, unsigned long len,
		      int tab_every_n_bytes, unsigned char *result,
		      unsigned long *bytes_used)
{
  unsigned long l, char_bytes;

  if (bytes_used)
    *bytes_used = 0;

  if (start == NULL || len == 0)
    return;

  for (l = 0; l < len; l++, start++) {
    if ((indent && *indent > 74) ||
	(tab_every_n_bytes > 0 && l && (l % tab_every_n_bytes) == 0)) {
      InsertCodeBreak (indent, result, &char_bytes);
      if (result) result += char_bytes;
      if (bytes_used) *bytes_used += char_bytes;
    }
    Byte2Code (*start, result, &char_bytes);
    if (result) result += char_bytes;
    if (bytes_used) *bytes_used += char_bytes;
    if (indent) (*indent) += (int) char_bytes;
  }

} // Mem2Code


unsigned char *JPLPic::RenderAsCode (void) {
  unsigned char *buf = NEW(mm, "Code Image Buffer")
    unsigned char[rows * cols * BytesPerPixel() * 4 + 100];
  
  return RenderAsCode (buf, NULL);
}


// Creates a C-parsable string that encodes this image. Either
// parameter may be NULL.

unsigned char *JPLPic::RenderAsCode (unsigned char *buf, unsigned long
				     *bytes_required)
{ unsigned long bytes, total_bytes;
  unsigned long r, c, pt;
  int indent = 0;

  r = htonl(rows);
  c = htonl(cols);
  pt = htonl (pixelType);

  total_bytes = 0;

  CodeStart (&indent, buf, &bytes);
  total_bytes += bytes;
  if (buf) buf += bytes;

  Mem2Code (&indent, (unsigned char *) &r, sizeof(long), -1, buf, &bytes);
  total_bytes += bytes;
  if (buf) buf += bytes;

  Mem2Code (&indent, (unsigned char *) &c, sizeof(long), -1, buf, &bytes);
  total_bytes += bytes;
  if (buf) buf += bytes;

  Mem2Code (&indent, (unsigned char *) &pt, sizeof(long), -1, buf, &bytes);
  total_bytes += bytes;
  if (buf) buf += bytes;

  Mem2Code (&indent, GetPixelAddress(), rows * cols * BytesPerPixel(),
	    cols * BytesPerPixel(), buf, &bytes);
  total_bytes += bytes;
  if (buf) buf += bytes;

  CodeEnd (&indent, buf, &bytes);
  total_bytes += bytes;
  if (buf) buf += bytes;

  // We're done creating the string contents, which are guaranteed to
  // be representable in ASCII.  Now tack on a trailing 0.

  if (buf) *buf = '\0';
  total_bytes++;

  if (bytes_required) *bytes_required = total_bytes;

  return buf;
} // JPLPic::RenderAsCode


// Returns number of characters parsed, or negative number on error
long JPLPic::ParseCodeString (char *input, long input_length,
			      unsigned char *output, long *output_length)
{
  typedef enum {LOOK_FOR_DOUBLE_QUOTE, PIXEL_START, PARSE_ERROR} StateT;
  StateT parse_state = LOOK_FOR_DOUBLE_QUOTE;
  long bytes_parsed = 0, output_count = 0;

  if (input == NULL)
    return 0;

  while (input_length > 0 && parse_state != PARSE_ERROR) {
    long these_bytes = 1;

    switch (parse_state) {
    case LOOK_FOR_DOUBLE_QUOTE:
      if (*input == '"')
	parse_state = PIXEL_START;
      else if (!isspace(*input))
	parse_state = PARSE_ERROR;
      break;

    case PIXEL_START:
      if (*input == '"')
	parse_state = LOOK_FOR_DOUBLE_QUOTE;
      else if (*input == '\\') {
	if (input_length > 3 && input[1] >= '0' && input[1] <= '3' &&
	    input[2] >= '0' && input[2] <= '7' &&
	    input[3] >= '0' && input[3] <= '7') {
	  these_bytes = 4;
	  if (output) output[output_count++] = (input[1] - '0') * 64 +
			(input[2] - '0') * 8 +
			(input[3] - '0');
	} else if (input_length > 1) {
	  these_bytes = 2;
	  switch (input[1]) {
	  case '0':
	    if (output) output[output_count++] = '\0';
	    break;
	  case 'n':
	    if (output) output[output_count++] = '\n';
	    break;
	  case 't':
	    if (output) output[output_count++] = '\t';
	    break;
	  default:
	    if (output) output[output_count++] = input[1];
	    break;
	  }
	} else
	  parse_state = PARSE_ERROR;
      } else {
	if (output) output[output_count++] = *input;
      }
      break;
    default:
      break;
    }

    if (parse_state != PARSE_ERROR) {
      bytes_parsed += these_bytes;
      input += these_bytes;
      input_length -= these_bytes;
    }
  }

  if (output_length) *output_length = output_count;

  if (input_length == 0 && parse_state == LOOK_FOR_DOUBLE_QUOTE)
    return bytes_parsed;
  else
    return -1;
}

// Returns number of characters parsed, or negative number on error
long JPLPic::ParseCodeFile (char *filename,
			    unsigned char *output, long *output_length)
{
  char linebuf[1000];
  long output_bytes = 0;
  long chars_parsed = 0;

  FILE *fp;

  if (filename == NULL)
    return -1;

  if ((fp = fopen (filename, "r")) == NULL)
    return -1;
  
  while (!feof(fp)) {
    long this_len = 0, this_parsed = 0;

    if (fgets (linebuf, 999, fp) == NULL)
      break;
    linebuf[999] = '\0';
    this_parsed = ParseCodeString (linebuf, strlen (linebuf),
				   output, &this_len);
    if (this_parsed >= 0) {
      output_bytes += this_len;
      if (output)
	output += this_len;
      chars_parsed += this_parsed;
    } else {
      chars_parsed = -1;
      break;
    }
  }
  fclose (fp);
  if (output_length)
    *output_length = output_bytes;

  return chars_parsed;
}

long JPLPic::LoadCodeFromMemory (unsigned char *buf, unsigned long length)
{
  long r, c, pt, *ptr;

  if (buf == NULL || length < 3 * sizeof(long))
    return PARAM_ERR;

  ptr = (long *) buf;
  r = ntohl (*ptr);
  ptr++;
  c = ntohl (*ptr);
  ptr++;
  pt = ntohl (*ptr);
  ptr++;

  return LoadFromMemory ((unsigned char *) ptr, r, c, pt);
}



// Even though this could be inlined away, keep it here so we can call it
// from a debugger
void JPLPic::RenderInASCII (void)
{
  RenderInASCII (24, 80);
} // JPLPic::RenderInASCII

void JPLPic::RenderInASCII (int max_dim)
{
  RenderInASCII (max_dim, max_dim);
}


void JPLPic::RenderInASCII (int screen_max_rows, int screen_max_cols)
{
  RenderInASCII (stdout, screen_max_rows, screen_max_cols, 1);
}


void JPLPic::RenderInASCII (FILE *fp, int screen_max_rows, int screen_max_cols,
			    int scale)
{
   char toasc[256];
   char increasing[] = {' ', '.', ',', ':', ';', '^', '<', '/',
			'|', '+', '=', '%', '$', '*', '@', '#'};
   int max_row_pixels = screen_max_rows < rows ? screen_max_rows : rows;
   int max_col_pixels = screen_max_cols < cols ? screen_max_cols : cols;
   JPLPic *image;
   JPLPic resized(mm);

   if (fp == NULL || rows < 1 || cols < 1)
     return;

   if (Resample (&resized, max_row_pixels, max_col_pixels) != NO_ERR)
     fprintf (fp, "*** Could not rescale image, so just subsampling\n");

   // Create the 8bit pixel -> ASCII char mapping

   for (int i = 0; i < 256; i++)
     toasc[i] = increasing[(255-i) * sizeof(increasing) / 256];

   // Map pixel down to 8 bits; use the last component of vector pixels
   // (e.g., "Z" value from XYZ triples)

   if (pixelType != UC8_PIXEL || scale == 1)
     image = resized.MakeScaled8BitImage();
   else
     image = &resized;

   if (image == NULL) {
     DBG(("RenderInASCII:  NULL intermediate image!\n"));
     return;
   }

   for (int row = 0; row < max_row_pixels; row++) {
     for (int col = 0; col < max_col_pixels; col++) {
       int actual_row = row * image->rows / max_row_pixels;
       int actual_col = col * image->cols / max_col_pixels;
       unsigned char *intensity;
       intensity = image->GetPixelAddress (actual_row, actual_col);
       if (intensity == NULL)
	 continue;
       fputc (toasc[*intensity], fp);
     }
     fputc ('\n', fp);
   }
   fflush (fp);
   if (image != this)
     DELETE (mm, image);
} // RenderInASCII
