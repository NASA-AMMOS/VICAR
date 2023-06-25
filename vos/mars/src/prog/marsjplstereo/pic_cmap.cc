#include "nav_memory.h"

#include <string.h>	// For memset

#include "JPLPic.h"
#include <math.h>


/*!
  There are only two sizes a colormap can be; either it points to cmap_dflt
  which handles most cases, or it's dynamically allocated once to be
  MAX_CMAP_SIZE bytes and stays that way */

void JPLPic::DefineColorMapMem (int size)
{
  // cmapbuf == cmap_dflt the first time through
  cmap.uc = cmapbuf;

  if (size <= (int) DFLT_CMAP_SIZE)
    return;

  if (cmapbuf == NULL || cmapbuf == cmap_dflt) {
    cmapbuf = NEW(mm,"ColorMap large buffer") unsigned char[MAX_CMAP_SIZE];
  }

  cmap.uc = cmapbuf;
} // JPLPic::DefineColorMapMem



void JPLPic::LoadColorMap (unsigned char *map, int size)
{
  int pixBytes = BytesPerPixel();

  DefineColorMapMem (256 * pixBytes);

  if (size > (int) MAX_CMAP_SIZE)
    size = (int) MAX_CMAP_SIZE;

  if (256 * pixBytes > (int) MAX_CMAP_SIZE) {
    fprintf (stderr, "LoadColorMap:  256 * Bpp %d exceeds MAX_CMAP_SIZE %ld!\n",
	      pixBytes, MAX_CMAP_SIZE);
    return;
  }

  if (size == 256 * pixBytes)
    memcpy (cmap.uc, map, size);
  else {
    int i, elt_size = size / 256, offset;
    unsigned char *map_ptr = cmap.uc;

    offset = pixBytes - elt_size;
    if (offset < 0)
      offset = 0;

    // Load up the colormap memory.  We assume that the map is complete,
    // i.e. that there are 256 entries of some size.  That size can be
    // larger than the pixel size (in which case only the first
    // BytesPerPixel() bytes are read from each entry), or they can be
    // smaller than the pixel size (in which case they're zero-padded
    // at the front in the colormap table).  This latter property lets
    // you initialize ARGB32_PIXEL pixels (which technically require 4
    // bytes) from 3-byte colormap triples.

    if (elt_size > 0) {
      int to_copy = (elt_size <= pixBytes) ? elt_size : pixBytes;

      (void) memset (map_ptr, 0, 256 * pixBytes);
      for (i = 0; i < 256; i++) {
	memcpy (map_ptr + offset, map, to_copy);
	map_ptr += pixBytes;
	map += elt_size;
      }
    }
  }
} // JPLPic::LoadColorMap




//////////////////////////////////////////////////////////////////////////////
///  LoadInterpolatedColorMap -- Take a background and foreground color,
///  whose pixeltype matches that of this image (though the number of
///  scalars may differ), and fill the colormap
///  by interpolating between them using the given gamma.
//////////////////////////////////////////////////////////////////////////////

void JPLPic::LoadInterpolatedColorMap (unsigned char *background,
				       unsigned char *foreground,
				       int size,
				       float gamma)
{
  AnythingT b, f, e;

  if (size < 1 || size > 2048)
    return;

  if (background == NULL || foreground == NULL)
    return;

  unsigned char *entry =
    NEW(mm, "InterpolatedColorMap Entry") unsigned char[size];
  float *coeff = NEW (mm, "InterpolatedColorMap Coefficients") float[size];
  float *offset = NEW (mm, "InterpolatedColorMap Offsets") float[size];
  unsigned int i, j;
  unsigned int co;

  // Start by generating appropriate gamma-corrected greyscales ranging
  // from 0 to 255

  LoadGreyColorMap (gamma, 0);
  PrintColorMap (stdout);
  b.uc = background;
  f.uc = foreground;
  e.uc = entry;

#define SET_COEFF_OFFSET(field,type) \
    for (j = 0; j < size / sizeof(type); j++) { \
      coeff[j]  = (float) (f.field[j] - b.field[j]) / 255.0; \
      offset[j] = (float) b.field[j]; \
    }

#define SET_ENTRY(field,type,i) \
    co = BytesPerPixel() / sizeof(type) - 1; \
    for (j = 0; j < size / sizeof(e.field[0]); j++) { \
      e.field[j] = (type) (coeff[j] * \
			   cmap.field[i * BytesPerPixel() / \
				     sizeof(type) + co] \
			   + offset[j]); \
    }

  switch (pixelType) {
  case UC8_PIXEL:
  case ARGB32_PIXEL:
    SET_COEFF_OFFSET(uc, unsigned char);
    for (i = 0; i <= 255; i++) {
      SET_ENTRY(uc, unsigned char, i);
      fprintf (stdout, "%d: %d %d %d\n", i, entry[0], entry[1], entry[2]);
      LoadColorMapEntry (i, entry, size);
    }
    break;
    
  case INT16_PIXEL:
    SET_COEFF_OFFSET(uh, unsigned short);
    for (i = 0; i <= 255; i++) {
      SET_ENTRY(uh, unsigned short, i);
      LoadColorMapEntry (i, entry, size);
    }
    break;
    
  case INT32_PIXEL:
    SET_COEFF_OFFSET(ul, unsigned long);
    for (i = 0; i <= 255; i++) {
      SET_ENTRY(ul, unsigned long, i);
      LoadColorMapEntry (i, entry, size);
    }
    break;
    
  case FLOAT_PIXEL:
  case XYZ_FLOAT_PIXEL:
    SET_COEFF_OFFSET(f, float);
    for (i = 0; i <= 255; i++) {
      SET_ENTRY(f, float, i);
      LoadColorMapEntry (i, entry, size);
    }
    break;
    
  case DOUBLE_PIXEL:
  case XYZ_DOUBLE_PIXEL:
    SET_COEFF_OFFSET(d, double);
    for (i = 0; i <= 255; i++) {
      SET_ENTRY(d, double, i);
      LoadColorMapEntry (i, entry, size);
    }
    break;
    
  default:
    fprintf (stderr, "LoadInterpolatedColorMap:  Unknown pixelType %ld\n",
	     pixelType);
    break;
  } // switch

  DELETEV (mm, entry);
  DELETEV (mm, offset);
  DELETEV (mm, coeff);
} // JPLPic::LoadInterpolatedColorMap


// ptr data type is expected to match pixelType

void JPLPic::LoadColorMapEntry (unsigned char entry, unsigned char *ptr,
				int size)
{
  AnythingT a;
  int pixBytes = BytesPerPixel();
  unsigned char buf[MAX_CMAP_SIZE / 256];

  DefineColorMapMem (256 * pixBytes);

  if (cmap.uc == NULL || ptr == NULL) return;
  a.uc = ptr;

  if (size < pixBytes) {
    memset (buf, 0, MAX_CMAP_SIZE / 256);
    memcpy (buf + (pixBytes - size), ptr, size);
    a.uc = buf;
  }
  
  switch (pixelType) {
  case UC8_PIXEL:    cmap.uc[entry] = *a.uc;  break;
  case INT16_PIXEL:  cmap.uh[entry] = *a.uh; break;			
  case INT32_PIXEL:  cmap.l[entry]  = *a.l;  break;
  case FLOAT_PIXEL:  cmap.f[entry]  = *a.f;  break;
  case DOUBLE_PIXEL: cmap.d[entry]  = *a.d;  break;
  case XYZ_FLOAT_PIXEL:
  case XYZ_DOUBLE_PIXEL:
  case ARGB32_PIXEL: memcpy (cmap.uc + pixBytes * entry, a.uc,
			     pixBytes);
    break;
  default: fprintf (stderr, "LoadColorMapEntry:  Unknown pixelType %ld\n",
		    pixelType);
    break;
  }
} // JPLPic::LoadColorMapEntry


unsigned char *JPLPic::GetColorMapEntry (unsigned char val)
{
  return cmap.uc ? (cmap.uc + BytesPerPixel() * val) : NULL;
} // JPLPic::GetColorMapEntry



/*! Loads the colormap with a greyscale range appropriate for its pixeltype */

void JPLPic::LoadGreyColorMap (float Gamma, int contour_period)
{
  unsigned char newmap[256*3];
  float f[256];
  double d[256];
  int i;


  switch (pixelType) {
  case UC8_PIXEL:
  case INT16_PIXEL:
  case INT32_PIXEL:
    for (i = 0; i < 256; i++)
      newmap[i] = i;
    if (contour_period)
      for (i = contour_period; i < 256; i += contour_period)
	newmap[i] = 0;
    LoadColorMap (newmap, 256);
    break;
  case FLOAT_PIXEL:
  case XYZ_FLOAT_PIXEL:
    for (i = 0; i < 256; i++)
      f[i] = (float) i;
    if (contour_period)
      for (i = contour_period; i < 256; i += contour_period)
	f[i] = 0.0;
    LoadColorMap ((unsigned char *) f, sizeof(f));
    break;
  case ARGB32_PIXEL:
    if (!EQ(Gamma, 0.0))
      for (i = 0; i < 256; i++)
	newmap[i*3] = newmap [i*3 + 1] = newmap[i*3 + 2] =
	  (unsigned char) (255.0 *
			   pow (((float) i) / 255.0, 1.0 / Gamma));
    else
      for (i = 0; i < 256; i++)
	newmap[i*3] = newmap [i*3 + 1] = newmap[i*3 + 2] = i;

    if (contour_period)
      for (i = contour_period; i < 256; i += contour_period)
	newmap[i*3] = newmap[i*3 + 1] = newmap[i*3 + 2] = 0;

    LoadColorMap (newmap, 256*3);
    break;
  case DOUBLE_PIXEL:
  case XYZ_DOUBLE_PIXEL:
    for (i = 0; i < 256; i++)
      d[i] = (float) i;
    if (contour_period)
      for (i = contour_period; i < 256; i += contour_period)
	d[i] = 0.0;
    LoadColorMap ((unsigned char *) d, sizeof(d));
    break;
  default:
    Warning ("LoadGreyColorMap: Unknown pixel type!!\n");
    break;
  }
} // JPLPic::LoadGreyColorMap


void JPLPic::LoadRainbowColorMap ()
{
  unsigned char newmap[256*3] = {
     0, 0, 0,     45, 0, 36,     56, 0, 46,     60, 0, 49,
     67, 0, 54,     70, 0, 59,     71, 0, 61,     75, 0, 68,
     74, 0, 73,     74, 0, 77,     73, 0, 81,     71, 0, 87,
     69, 1, 90,     68, 2, 94,     66, 3, 97,     63, 6, 102,
     61, 7, 106,     58, 10, 109,     56, 12, 113,     53, 15, 116,
     48, 18, 119,     47, 20, 121,     44, 23, 124,     41, 27, 128,
     40, 28, 129,     37, 32, 132,     34, 36, 134,     29, 43, 137,
     25, 52, 138,     24, 57, 139,     24, 62, 141,     24, 64, 142,
     23, 65, 142,     23, 69, 143,     23, 71, 142,     23, 71, 142,
     23, 73, 142,     23, 75, 142,     23, 75, 142,     23, 78, 142,
     23, 80, 142,     23, 80, 142,     23, 82, 141,     23, 85, 141,
     23, 85, 141,     23, 87, 140,     23, 87, 140,     24, 90, 140,
     24, 90, 140,     24, 93, 139,     24, 93, 139,     24, 93, 139,
     24, 93, 139,     24, 97, 139,     24, 97, 139,     25, 101, 138,
     25, 101, 138,     25, 104, 137,     25, 104, 137,     25, 104, 137,
     26, 108, 137,     26, 108, 137,     27, 111, 136,     27, 111, 136,
     27, 111, 136,     27, 115, 135,     27, 115, 135,     28, 118, 134,
     28, 118, 134,     29, 122, 133,     29, 122, 133,     29, 122, 133,
     29, 122, 133,     29, 125, 132,     29, 125, 132,     30, 128, 131,
     30, 128, 131,     31, 131, 130,     31, 131, 130,     31, 131, 130,
     32, 134, 128,     32, 134, 128,     33, 137, 127,     33, 137, 127,
     33, 137, 127,     34, 140, 125,     34, 140, 125,     35, 142, 123,
     35, 142, 123,     36, 145, 121,     36, 145, 121,     36, 145, 121,
     37, 147, 118,     37, 147, 118,     38, 150, 116,     38, 150, 116,
     40, 152, 113,     40, 152, 113,     41, 154, 111,     41, 154, 111,
     42, 156, 108,     42, 156, 108,     43, 158, 106,     43, 158, 106,
     43, 158, 106,     45, 160, 104,     45, 160, 104,     46, 162, 101,
     46, 162, 101,     48, 164, 99,     48, 164, 99,     50, 166, 97,
     50, 166, 97,     51, 168, 95,     53, 170, 93,     53, 170, 93,
     53, 170, 93,     55, 172, 91,     55, 172, 91,     57, 174, 88,
     57, 174, 88,     59, 175, 86,     62, 177, 84,     64, 178, 82,
     64, 178, 82,     67, 180, 80,     67, 180, 80,     69, 181, 79,
     72, 183, 77,     72, 183, 77,     72, 183, 77,     75, 184, 76,
     77, 186, 74,     80, 187, 73,     83, 189, 72,     87, 190, 72,
     91, 191, 71,     95, 192, 70,     99, 193, 70,     103, 194, 70,
     107, 195, 70,     111, 196, 70,     111, 196, 70,     115, 196, 70,
     119, 197, 70,     123, 197, 70,     130, 198, 71,     133, 199, 71,
     137, 199, 72,     140, 199, 72,     143, 199, 73,     143, 199, 73,
     147, 199, 73,     150, 199, 74,     153, 199, 74,     156, 199, 75,
     160, 200, 76,     167, 200, 78,     170, 200, 79,     173, 200, 79,
     173, 200, 79,     177, 200, 80,     180, 200, 81,     183, 199, 82,
     186, 199, 82,     190, 199, 83,     196, 199, 85,     199, 198, 85,
     199, 198, 85,     203, 198, 86,     206, 197, 87,     212, 197, 89,
     215, 196, 90,     218, 195, 91,     224, 194, 94,     224, 194, 94,
     230, 193, 96,     233, 192, 98,     236, 190, 100,     238, 189, 104,
     240, 188, 106,     240, 188, 106,     242, 187, 110,     244, 185, 114,
     245, 184, 116,     247, 183, 120,     248, 182, 123,     248, 182, 123,
     250, 181, 125,     251, 180, 128,     252, 180, 130,     253, 180, 133,
     253, 180, 133,     254, 180, 134,     254, 179, 138,     255, 179, 142,
     255, 179, 145,     255, 179, 145,     255, 179, 152,     255, 180, 161,
     255, 180, 164,     255, 180, 167,     255, 180, 167,     255, 181, 169,
     255, 181, 170,     255, 182, 173,     255, 183, 176,     255, 183, 176,
     255, 184, 179,     255, 185, 179,     255, 185, 182,     255, 186, 182,
     255, 186, 182,     255, 187, 185,     255, 188, 185,     255, 189, 188,
     255, 189, 188,     255, 190, 188,     255, 191, 191,     255, 192, 191,
     255, 194, 194,     255, 194, 194,     255, 197, 197,     255, 198, 198,
     255, 200, 200,     255, 201, 201,     255, 201, 201,     255, 202, 202,
     255, 203, 203,     255, 205, 205,     255, 206, 206,     255, 206, 206,
     255, 208, 208,     255, 209, 209,     255, 211, 211,     255, 215, 215,
     255, 216, 216,     255, 216, 216,     255, 218, 218,     255, 219, 219,
     255, 221, 221,     255, 223, 223,     255, 226, 226,     255, 228, 228,
     255, 230, 230,     255, 230, 230,     255, 232, 232,     255, 235, 235,
     255, 237, 237,     255, 240, 240,     255, 243, 243,     255, 246, 246,
     255, 249, 249,     255, 251, 251,     255, 253, 253,     255, 255, 255 };

  LoadColorMap (newmap, 256*3);

} // JPLPic::LoadRainbowColorMap



void JPLPic::PrintColorMap (FILE *fp)
{
  if (fp == NULL)
    return;

  if (cmap.uc == NULL) {
    fprintf (fp, "NULL Colormap\n");
    return;
  }

  fprintf (fp, "256 %s Colormap entries, each of size %d\n",
	   PixelTag2s((enum PixelTag) pixelType), BytesPerPixel());

  unsigned int i;
  for (i = 0; i < 256; i++) {
    AnythingT ptr;
    ptr.uc = GetColorMapEntry (i);
    switch (pixelType) {
    case UC8_PIXEL:
      fprintf (fp, "%d.  %d\n", i, *ptr.uc);
      break;
    case INT16_PIXEL:
      fprintf (fp, "%d.  %d\n", i, *ptr.uh);
      break;
    case INT32_PIXEL:
      fprintf (fp, "%d.  %ld\n", i, *ptr.ul);
      break;
    case FLOAT_PIXEL:
      fprintf (fp, "%d.  %f\n", i, *ptr.f);
      break;
    case DOUBLE_PIXEL:
      fprintf (fp, "%d.  %f\n", i, *ptr.d);
      break;
    case XYZ_FLOAT_PIXEL:
      fprintf (fp, "%d.  %f %f %f\n", i, ptr.f[0], ptr.f[1], ptr.f[2]);
      break;
    case XYZ_DOUBLE_PIXEL:
      fprintf (fp, "%d.  %f %f %f\n", i, ptr.d[0], ptr.d[1], ptr.d[2]);
      break;
    case ARGB32_PIXEL:
      fprintf (fp, "%d.  %d %d %d %d\n", i,
	       ptr.uc[0], ptr.uc[1], ptr.uc[2], ptr.uc[3]);
      break;
    default:
      if (i == 0)
	fprintf (fp, "PrintColorMap:  Unknown pixeltype %ld!\n", pixelType);
      break;
    }
  }    
} // JPLPic::PrintColorMap


JPLPic *JPLPic::BuildARGBFromColormap (JPLPic *input, unsigned char *clrmap,
				       int clrmap_size)
{
  JPLPic *pic = input;
  long r, c;

  if (pixelType != UC8_PIXEL) {
    fprintf (stderr, "BuildARGBFromARGBColormap: Can't handle %s pixelType\n",
	     PixelTag2s ((enum PixelTag) pixelType));
    return NULL;
  }
  if (pic == NULL)
    pic = NEW(mm, "ARGB32 applied colormap image") JPLPic(mm);
  if (pic == NULL || clrmap == NULL)
    return NULL;
  if (clrmap_size > (int) MAX_CMAP_SIZE) {
    fprintf (stderr, "BuildARGBFromColormap: Cmap size %d > %ld", clrmap_size,
	     MAX_CMAP_SIZE);
    return NULL;
  }
  pic->Init(rows, cols, ARGB32_PIXEL, pic->mm ? pic->mm : mm);
  pic->LoadColorMap (clrmap, clrmap_size);

  for (r = 0; r < rows; r++) {
    unsigned char *ptr = GetPixelAddress (r, 0);

    for (c = 0; c < cols; c++)
      pic->SetPixel (r, c, *ptr++, 0);
  }
  
  return pic;
} // JPLPic::BuildARGBFromColormap

