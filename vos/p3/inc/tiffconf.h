/* $Header: /usr/people/sam/tiff/libtiff/RCS/tiffconf.h,v 1.13 1996/04/05 17:36:53 sam Rel $ */
/*
 * Copyright (c) 1988-1996 Sam Leffler
 * Copyright (c) 1991-1996 Silicon Graphics, Inc.
 *
 * Permission to use, copy, modify, distribute, and sell this software and 
 * its documentation for any purpose is hereby granted without fee, provided
 * that (i) the above copyright notices and this permission notice appear in
 * all copies of the software and related documentation, and (ii) the names of
 * Sam Leffler and Silicon Graphics may not be used in any advertising or
 * publicity relating to the software without the specific, prior written
 * permission of Sam Leffler and Silicon Graphics.
 * 
 * THE SOFTWARE IS PROVIDED "AS-IS" AND WITHOUT WARRANTY OF ANY KIND, 
 * EXPRESS, IMPLIED OR OTHERWISE, INCLUDING WITHOUT LIMITATION, ANY 
 * WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.  
 * 
 * IN NO EVENT SHALL SAM LEFFLER OR SILICON GRAPHICS BE LIABLE FOR
 * ANY SPECIAL, INCIDENTAL, INDIRECT OR CONSEQUENTIAL DAMAGES OF ANY KIND,
 * OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
 * WHETHER OR NOT ADVISED OF THE POSSIBILITY OF DAMAGE, AND ON ANY THEORY OF 
 * LIABILITY, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE 
 * OF THIS SOFTWARE.
 */

#ifndef _TIFFCONF_
#define	_TIFFCONF_
/*
 * Library Configuration Definitions.
 *
 * This file defines the default configuration for the library.
 * If the target system does not have make or a way to specify
 * #defines on the command line, this file can be edited to
 * configure the library.  Otherwise, one can override portability
 * and configuration-related definitions from a Makefile or command
 * line by defining FEATURE_SUPPORT and COMPRESSION_SUPPORT (see below).
 */

/*
 * General portability-related defines:
 *
 * HAVE_IEEEFP		define as 0 or 1 according to the floating point
 *			format suported by the machine
 * BSDTYPES		define this if your system does NOT define the
 *			usual 4BSD typedefs u_int et. al.
 * HAVE_MMAP		enable support for memory mapping read-only files;
 *			this is typically deduced by the configure script
 * HOST_FILLORDER	native cpu bit order: one of FILLORDER_MSB2LSB
 *			or FILLODER_LSB2MSB; this is typically set by the
 *			configure script
 * HOST_BIGENDIAN	native cpu byte order: 1 if big-endian (Motorola)
 *			or 0 if little-endian (Intel); this may be used
 *			in codecs to optimize code
 */
#ifndef HAVE_IEEEFP
#define	HAVE_IEEEFP	1
#endif
#ifndef HOST_FILLORDER
#define	HOST_FILLORDER	FILLORDER_MSB2LSB
#endif
#ifndef	HOST_BIGENDIAN
#define	HOST_BIGENDIAN	1
#endif

#ifndef FEATURE_SUPPORT
/*
 * Feature support definitions:
 *
 *    COLORIMETRY_SUPPORT enable support for 6.0 colorimetry tags
 *    YCBCR_SUPPORT	enable support for 6.0 YCbCr tags
 *    CMYK_SUPPORT	enable support for 6.0 CMYK tags
 */
#define	COLORIMETRY_SUPPORT
#define	YCBCR_SUPPORT
#define	CMYK_SUPPORT
#endif /* FEATURE_SUPPORT */

#ifndef COMPRESSION_SUPPORT
/*
 * Compression support defines:
 *
 *    CCITT_SUPPORT	enable support for CCITT Group 3 & 4 algorithms
 *    PACKBITS_SUPPORT	enable support for Macintosh PackBits algorithm
 *    LZW_SUPPORT	enable support for LZW algorithm
 *    THUNDER_SUPPORT	enable support for ThunderScan 4-bit RLE algorithm
 *    NEXT_SUPPORT	enable support for NeXT 2-bit RLE algorithm
 *    OJPEG_SUPPORT	enable support for 6.0-style JPEG DCT algorithms
 *			(no builtin support, only a codec hook)
 *    JPEG_SUPPORT	enable support for post-6.0-style JPEG DCT algorithms
 *			(requires freely available IJG software, see tif_jpeg.c)
 *    ZIP_SUPPORT	enable support for Deflate algorithm
 *			(requires freely available zlib software, see tif_zip.c)
 *    PIXARLOG_SUPPORT	enable support for Pixar log-format algorithm
 */
#define	CCITT_SUPPORT
#define	PACKBITS_SUPPORT
#define	LZW_SUPPORT
#define	THUNDER_SUPPORT
#define	NEXT_SUPPORT
#endif /* COMPRESSION_SUPPORT */

/*
 * If JPEG compression is enabled then we must also include
 * support for the colorimetry and YCbCr-related tags.
 */
#ifdef JPEG_SUPPORT
#ifndef YCBCR_SUPPORT
#define	YCBCR_SUPPORT
#endif
#ifndef COLORIMETRY_SUPPORT
#define	COLORIMETRY_SUPPORT
#endif
#endif /* JPEG_SUPPORT */

/*
 * ``Orthogonal Features''
 *
 * STRIPCHOP_SUPPORT	automatically convert single-strip uncompressed images
 *			to mutiple strips of ~8Kb (for reducing memory use)
 * SUBIFD_SUPPORT	enable support for SubIFD tag (thumbnails and such)
 */
#ifndef STRIPCHOP_SUPPORT
#define	STRIPCHOP_SUPPORT	1	/* enable strip chopping */
#endif
#ifndef SUBIFD_SUPPORT
#define	SUBIFD_SUPPORT		1	/* enable SubIFD tag (330) support */
#endif
#endif /* _TIFFCONF_ */
