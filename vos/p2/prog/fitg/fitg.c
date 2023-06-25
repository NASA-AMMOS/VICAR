/* compile with gcc -DSTANDALONE -Wall -o fit fit.c -lm */

#include <stdlib.h>
#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <math.h>

#include "zifmessage.h"
#include "zmabend.h"

#ifndef STANDALONE
#include "vicmain_c.h"
#endif

#define _fitg_version_ "2019-06-11"

#define DEBUGMSG(MSG) if (debug) printf("%s on line %d\n", MSG, __LINE__)

int HistToCurve(unsigned char * lut, /* out: lookup table to modify              */
		int * hist,          /* in: histogram of image to match to curve */
		float * curve,       /* in: curve to be matched against hist     */
		int low, int high);  /* in: low/high entries in hist to use      */
int GaussLut(unsigned char * lut, /* The lookup table to modify     */
	     int * hist,          /* The corresponding histogram    */
	     float gsigma,        /* no. of sigmas across 1/2 curve */
	     float mean,          /* mean of curve                  */
	     int low,             /* low/high hist vals to exclude  */
	     int high);

/* Lifted from vids jstretch.c */
/************************************************************************/
/* HistToCurve will create a lookup table which gives a good match
 * of the histogram given by hist to the curve described by the array
 * curve.
 */
#define HSIZE 256
#undef SUCCESS
#define SUCCESS 0
#define FAIL 1
#define BlockMove(src,dst,len) memcpy((dst),(src),(len))
#define BlockFill(val,dst,len) memset((dst),(val),(len))
int HistToCurve(unsigned char * lut, /* out: lookup table to modify              */
		int * hist,          /* in: histogram of image to match to curve */
		float * curve,       /* in: curve to be matched against hist     */
		int low, int high)   /* in: low/high entries in hist to use      */
{
  int		i,cdfPos;		/* increment variables		*/
  float	       cumSum,cumSumNext,scale;	/* Cumulative sum, scaling factor*/
  int		locHist[HSIZE];		/* local copy of histogram	*/
  float		cdf[HSIZE];		/* cumulative distribution func.*/

  BlockMove(curve, cdf, HSIZE * sizeof(float));	 /* Make local copies of  */
  BlockMove(hist, locHist, HSIZE * sizeof(int));/* arrays to be modified */

  BlockFill(0, locHist, low * sizeof(int));	    /* Zero out values 	*/
  for (i = high + 1; i < HSIZE; i++) locHist[i] = 0; /* to be excluded	*/

/* Now make the cdf table contain an actual cumulative distribution
 * function (cdf); ie, each value is the sum of the values below it.
 * scale is modified to hold a scaling factor to adjust the gaussian
 * to the actual number of pixels we have, and each value in cdf
 * is scaled and then added to the point below it, to give
 * an actual cumulative distribution function.
 */
  cumSum = 0.0;			/* Total area under the curve to be fit	*/
  for (i = 0; i < HSIZE; i++)
    cumSum += cdf[i];
  scale = 0;
  for (i = low; i <= high; i++)		/* scale is the total number of	*/
    scale += (float) locHist[i];	/* pixels in our histogram 	*/
  scale = scale / cumSum;		/* divided by the total in gauss*/
  cdf[0] *= scale;
  for (i = 1; i < HSIZE; i++)
    cdf[i] = cdf[i] * scale + cdf[i - 1];

/* Now generate the lookup table based on the shape of the 
 * cumulative distribution function.  Each output DN is that DN (location)
 * in the cdf[] array that has the accumulation (value) that is
 * closest to the accumulation (sum of values) of the input DN in
 * our histogram.
 */
  cumSum=0.0;
  i = cdfPos = 0;
  cumSumNext = locHist[0];
  while ((i < HSIZE) && (cdfPos < HSIZE))
  {
    while (fabs(cdf[cdfPos] - cumSum) >= fabs(cdf[cdfPos] - cumSumNext))
    {				/* cumulative sum not yet big enough,	*/
      lut[i] = cdfPos;		/* so store the cdf position and try	*/
      i++;			/* the next spot in the histogram.	*/
      if (i > 255) break;
      cumSum = cumSumNext;
      cumSumNext = cumSum + locHist[i];
    }
    cdfPos++;			/* match next spot in cdf	*/
  }
  for (; i <= 255; i++) lut[i] = 255;	/* saturate out rest of lut	*/
  return SUCCESS;
}

/* Lifted from vids jstretch.c */
/************************************************************************/
/* GaussLut will force the given histogram into roughly a gaussian 
 * curve with the given mean and sigma by tweaking the given lookup
 * table.  The histogram is ignored below "low" and above "high".
 * "This arrangement is by D. Stanfill, from an original composition
 * by J. Addington entitled 'RDISPLAY' (in D minor)".
 */
int GaussLut(unsigned char * lut, /* The lookup table to modify     */
	     int * hist,          /* The corresponding histogram    */
	     float gsigma,        /* no. of sigmas across 1/2 curve */
	     float mean,          /* mean of curve                  */
	     int low,             /* low/high hist vals to exclude  */
	     int high)
{
  int		i;			/* increment variable		*/
  float		sig2;			/* sigma squared		*/
  float		cdf[HSIZE];

  if (gsigma == 0.0) return FAIL;
  sig2 = 127.5 / gsigma;			/* sigma		*/
  sig2 = 2.0 * sig2 * sig2;			/* 2 * sigma^2		*/

/* Now fill up the cumulative distribution function (cdf) table with the 
 * proper values so that we have a gaussian histogram.  Each value
 * in cdf[] contains the gaussian formula evaluated at that point.
 * (y = e^(-(x-mean)^2 / (2*sigma^2)) )
 */
  for (i = 0; i < HSIZE; i++)
    cdf[i] = exp(-(i - mean) * (i - mean) / sig2);
  HistToCurve(lut, hist, cdf, low, high);
  return SUCCESS;
}

#ifdef STANDALONE
int main(int argc, char* argv[]) {
  int c;
  char * optstring = "p:l:r:bHhvVsn:x:m:S:g";
#else
void main44(void) {
#endif
  double percent = 0.0, leftPercent = 0.0, rightPercent = 0.0;
  unsigned int halfWord = 0, lines32, samples32;
  unsigned long lines, samples;
  unsigned int histogram[ 65536 ];
  int firstIndex, lastIndex;
  void * voidBuf = 0;
  int verbose = 0, Verbose = 0, debug = 0;
  int halfToByte = 0;
  int swapInput = 0;
  int outMin=999999, outMax=999999;
  int gauss = 0;
  double mean = 127.5;
  double sigma = 2.5;
#ifdef STANDALONE
  FILE * infile, * outfile;
#else
  char progVersionID [100];
  int parmct, parmdf;
  char inpfilename [99];
  char outfilename [99];
  char msgBuf [1000];
  int inpunit, outunit;
  char fmtStr [100];
  unsigned int line;
  int status;
#endif

#ifdef STANDALONE
  printf("fitg version %s\n", _fitg_version_);
#else
  sprintf (progVersionID, "fitg version %s", _fitg_version_);
  zifmessage (progVersionID);
#endif

#ifndef STANDALONE
  /* fetch params */
  zvparm ("inp", inpfilename, &parmct, &parmdf, 1, 99);
  zvparm ("out", outfilename, &parmct, &parmdf, 1, 99);
  zvparmd ("percent", & percent, & parmct, & parmdf, 1, 0);
  if (! parmdf) /* if the default wasn't used, because the user provided a value */
    leftPercent = rightPercent = percent;
  zvparmd ("left", & leftPercent, & parmct, & parmdf, 1, 0);
  zvparmd ("right", & rightPercent, & parmct, & parmdf, 1, 0);
  zvp ("outmin", & outMin, & parmct);
  zvp ("outmax", & outMax, & parmct);
  gauss = zvptst("gauss");
  zvparmd ("mean", & mean, & parmct, & parmdf, 1, 0);
  if (! parmdf)
    gauss = 1;
  zvparmd ("sigma", & sigma, & parmct, & parmdf, 1, 0);
  if (! parmdf)
    gauss = 1;
  halfToByte = zvptst("byte");
  swapInput = zvptst("swap");
  verbose = zvptst("verbose");
  debug = zvptst("debug");

  if (zvunit (& inpunit, "INP", 1, NULL) != 1) {
    sprintf (msgBuf, "zvunit failed on input \"%s\"", inpfilename);
    zmabend (msgBuf);	/* die */
  }

  if (zvopen (inpunit, "OPEN_ACT", "SA", "IO_ACT", "SA",NULL) != 1) {
    sprintf (msgBuf, "zvopen failed on input \"%s\"", inpfilename);
    zmabend (msgBuf);	/* die */
  }

  zvget (inpunit, "NL", & lines32, NULL);
  lines = lines32;
  zvget (inpunit, "NS", & samples32, NULL);
  samples = samples32;
  zvget (inpunit, "FORMAT", fmtStr, NULL);

  if (strcmp (fmtStr, "BYTE") && strcmp (fmtStr, "HALF"))
    zmabend ("Input must be BYTE or HALF word files");

  if (strcmp (fmtStr, "BYTE"))	/* then it must be HALF */
    halfWord = 1;
  else {			/* else it must be BYTE */
    halfWord = 0;
    halfToByte = 0;
    swapInput = 0;
  }

  if (zvunit (& outunit, "OUT", 1, NULL) != 1) {
    sprintf (msgBuf, "zvunit failed on output \"%s\"", outfilename);
    zmabend (msgBuf);	/* die */
  }

  if (zvopen (outunit, "U_NL", lines32, "U_NS", samples32, "O_FORMAT", (halfWord && ! halfToByte) ? "HALF" : "BYTE",
	      "OP", "WRITE", "OPEN_ACT", "SA", "IO_ACT", "SA",NULL) != 1) {
    sprintf (msgBuf, "zvopen failed on output \"%s\"", outfilename);
    zmabend (msgBuf);	/* die */
  }

#else
  /* parse command line options */

  while ((c = getopt(argc, argv, optstring)) != -1) {
    switch(c) {
    case 's':
      swapInput = 1;
      halfWord = 1;
      break;
    case 'V':
      Verbose = 1;
    case 'v':
      verbose = 1;
      break;
    case '?':
    case 'H':
      fprintf(stderr, "usage: fit [-p percent] [-l leftPercent] [-r rightPercent] [-n outMin] [-x outMax] [-m mean] [-S sigma] [-HVvhbsg] lines samples infile outfile\n");
      fprintf(stderr, "       don't use -p with either -l or -r;\n");
      fprintf(stderr, "       -h processes half-word input;\n");
      fprintf(stderr, "       -b converts half-word input to byte output (forces -h)\n");
      fprintf(stderr, "       -s byte swaps half-word input before stretching (forces -h)\n");
      fprintf(stderr, "       -g adds a Gaussian stretch following the linear stretch (with -h forces -b)\n");
      fprintf(stderr, "       -m sets Gaussian mean, defaults to 127.5 (forces -g)\n");
      fprintf(stderr, "       -S sets Gaussian sigma, defaults to 2.5 (forces -g)\n");
      fprintf(stderr, "       -v is verbose, -V is very verbose\n");
      return 1;
    case 'n':
      if (sscanf(optarg, "%d", & outMin) != 1) {
	fprintf(stderr, "fit: error parsing output minimum value (integer) from \"%s\"\n", optarg);
	return 1;
      }
      break;
    case 'x':
      if (sscanf(optarg, "%d", & outMax) != 1) {
	fprintf(stderr, "fit: error parsing output maximum value (integer) from \"%s\"\n", optarg);
	return 1;
      }
      break;
    case 'p':
      if (sscanf(optarg, "%lf", & percent) != 1  || percent < 0.0 || percent > 100.0) {
	fprintf(stderr, "fit: error parsing percent value (real in range 0..100) from \"%s\"\n", optarg);
	return 1;
      }
      leftPercent = rightPercent = percent;
      break;
    case 'l':
      if (percent >= 0) {
	fprintf(stderr, "fit: cannot use both p and l options\n");
	return 1;
      }
      if (sscanf(optarg, "%lf", & leftPercent) != 1  || leftPercent < 0.0 || leftPercent > 100.0) {
	fprintf(stderr, "fit: error parsing left percent value (real in range 0..100) from \"%s\"\n", optarg);
	return 1;
      }
      break;
    case 'r':
      if (percent >= 0) {
	fprintf(stderr, "fit: cannot use both p and r options\n");
	return 1;
      }
      if (sscanf(optarg, "%lf", & rightPercent) != 1  || rightPercent < 0.0 || rightPercent > 100.0) {
	fprintf(stderr, "fit: error parsing right percent value (real in range 0..100) from \"%s\"\n", optarg);
	return 1;
      }
      break;
    case 'm':
      if (sscanf(optarg, "%lf", & mean) != 1  || mean < 0.0 || mean > 255.0) {
	fprintf(stderr, "fit: error parsing mean value (real in range 0..255) from \"%s\"\n", optarg);
	return 1;
      }
      gauss = 1;
      break;
    case 'S':
      if (sscanf(optarg, "%lf", & sigma) != 1  || sigma < 0.0) {
	fprintf(stderr, "fit: error parsing sigma value (non-negative real) from \"%s\"\n", optarg);
	return 1;
      }
      gauss = 1;
      break;
    case 'g':
      gauss = 1;
      break;
    case 'b':
      halfToByte = 1;
    case 'h':
      halfWord = 1;
      break;
    default:
      fprintf(stderr, "fit: error in optstring\n");
    }
  }
#endif

  if (gauss && halfWord)
    halfToByte = 1;    

  /* check optional min/max output values */

  if (outMin == 999999)
    if (! halfWord || halfToByte)
      outMin = 0;
    else
      outMin = -32768;
  else
    if (! halfWord || halfToByte) {
      if (outMin < 0 || outMin > 255) {
	fprintf(stderr, "fit: min output value (%d) out of range (for unsigned byte); forced to 0\n", outMin);
	outMin = 0;
      }
    } else {
      if (outMin < -32768 || outMin > 32767) {
	fprintf(stderr, "fit: min output value (%d) out of range (for signed half-word); forced to -32768\n", outMin);
	outMin = -32768;
      }
    }

  if (outMax == 999999)
    if (! halfWord || halfToByte)
      outMax = 255;
    else
      outMax = 32767;
  else
    if (! halfWord || halfToByte) {
      if (outMax < 0 || outMax > 255) {
	fprintf(stderr, "fit: max output value (%d) out of range (for unsigned byte); forced to 255\n", outMax);
	outMax = 255;
      }
    } else {
      if (outMax < -32768 || outMax > 32767) {
	fprintf(stderr, "fit: max output value (%d) out of range (for signed half-word); forced to 32767\n", outMax);
	outMax = 32767;
      }
    }

#ifdef STANDALONE
  /* get required line/sample parameters */

  if (argc <= optind) {
    fprintf(stderr, "fit: missing parameter \"lines\" -- use \"-H\" for help\n");
    return 1;
  }

  if (sscanf(argv[ optind ], "%d", & lines32) != 1 || lines32 < 1) {
    fprintf(stderr, "fit: error parsing line count (positive integer) from \"%s\"\n", argv[ optind ]);
    return 1;
  }
  lines = lines32;

  optind ++;

  if (argc <= optind) {
    fprintf(stderr, "fit: missing parameter \"samples\" -- use \"-H\" for help\n");
    return 1;
  }

  if (sscanf(argv[ optind ], "%d", & samples32) != 1 || samples32 < 1) {
    fprintf(stderr, "fit: error parsing sample count (positive integer) from \"%s\"\n", argv[ optind ]);
    return 1;
  }
  samples = samples32;

  /* get/open required in/out file parameters */

  optind ++;

  if (argc <= optind) {
    fprintf(stderr, "fit: missing parameter \"infile\" -- use \"-H\" for help\n");
    return 1;
  }

  if (! (infile = fopen(argv[ optind ], "r"))) {
    fprintf(stderr, "fit: error opening input \"%s\"\n", argv[ optind ]);
    return 1;
  }

  optind ++;

  if (argc <= optind) {
    fprintf(stderr, "fit: missing parameter \"outfile\" -- use \"-H\" for help\n");
    return 1;
  }

  if (! (outfile = fopen(argv[ optind ], "w"))) {
    fprintf(stderr, "fit: error opening output \"%s\"\n", argv[ optind ]);
    return 1;
  }
#endif

  /* clear histogram */

  DEBUGMSG("clearing histogram");

  memset(histogram, 0, sizeof(int) * 65536);

  /* read image and calculate histogram */

  if (halfWord) {		/* halfWord input */
    signed short * buf;

    DEBUGMSG("mallocing for half-word image");

    if (! (voidBuf = buf = malloc(sizeof(signed short) * lines * samples))) {
      fprintf(stderr, "fit: error mallocing read buffer\n");
      return;
    }

    DEBUGMSG("reading half-word image");

#ifdef STANDALONE
    if (fread (buf, sizeof(signed short), lines * samples, infile) != lines * samples) {
      fprintf(stderr, "fit: error reading input\n");
      return;
    }
#else
    for (line = 0; line < lines; line ++) {
      /* read in a line */
      if ((status = zvread (inpunit, buf + (line * samples), "LINE", line + 1, "SAMP", 1, "NSAMPS", samples32, NULL)) != 1)
	zmabend ("zvread failed on input");
    }
#endif

    unsigned long i;

    if (swapInput) {
      char * byteBuf = (char *) buf;
      char tmp;
      for (i = 0; i < 2 * lines * samples; i += 2) {
	tmp = byteBuf[ i ];
	byteBuf[ i ] = byteBuf[ i + 1 ];
	byteBuf[ i + 1 ] = tmp;
      }
    }

    DEBUGMSG("calculating half-word image");

    for (i = 0; i < lines * samples; i ++)
      histogram[ 32768 + buf[ i ] ] ++;

    DEBUGMSG("done reading half-word image");

  } else {			/* byte input */
#ifndef STANDALONE
    unsigned char * buf;
#else
    /* remove this */
    signed short * buf;
#endif

    DEBUGMSG("reading byte image");

#ifndef STANDALONE
    if (! (voidBuf = buf = malloc(sizeof(unsigned char) * lines * samples))) {
#else
      /* remove this */
    if (! (voidBuf = buf = malloc(sizeof(signed short) * lines * samples))) {
#endif
      fprintf(stderr, "fit: error mallocing read buffer\n");
      return;
    }

#ifdef STANDALONE
    if (fread (buf, sizeof(unsigned char), lines * samples, infile) != lines * samples) {
      fprintf(stderr, "fit: error reading input\n");
      return 1;
    }
#else
    for (line = 0; line < lines; line ++)
      /* read in a line */
      if ((status = zvread (inpunit, buf + (line * samples), "LINE", line + 1, "SAMP", 1, "NSAMPS", samples32, NULL)) != 1)
	zmabend ("zvread failed on input");
#endif

    unsigned long i;
    for (i = 0; i < lines * samples; i ++)
      histogram[ buf[ i ] ] ++;

    DEBUGMSG("done reading byte image");
  }

  /* done with input file */

#ifdef STANDALONE
  fclose(infile);
#else
  zvclose(inpunit, NULL);
#endif

  /* display histogram */

  int i;
  if (verbose) {
    printf("histogram before stretch:\n");
    for (i = 0; i < 65536; i ++)
      if (histogram[ i ])
	printf("%5d: %d\n", i - (halfWord?32768:0), histogram[ i ]);
  }

  /* find selected histogram range */

  unsigned long sum = 0;

  for (i = 0; i < 65536; i ++)
    if (histogram[ i ])
      break;

  for (; i < 65536; i ++) {
    sum += histogram[ i ];
    if (sum >= lines * samples * leftPercent / 100)
      break;
  }

  firstIndex = i;

  sum = 0;

  for (i = 65535; i >= 0; i --)
    if (histogram[ i ])
      break;

  for (; i >= 0; i --) {
    sum += histogram[ i ];
    if (sum >= lines * samples * rightPercent / 100)
      break;
  }

  lastIndex = i;
  
  if (Verbose)
    printf("first index %d last index %d\n", firstIndex, lastIndex);
  
  /* stretch image */

  DEBUGMSG("start linear stretch");

  if (halfToByte) {
    signed short * buf = (short *) voidBuf;
    signed short loVal = firstIndex - 32768;
    signed short hiVal = lastIndex - 32768;
    int inRange = hiVal - loVal;
#ifdef STANDALONE
    unsigned char * byteBuf = 0;
#endif

    DEBUGMSG("entering halfToByte branch");

#ifdef STANDALONE
    if (! (byteBuf = malloc(sizeof(unsigned char) * lines * samples))) {
      fprintf(stderr, "fit: error mallocing conversion buffer\n");
      return 1;
    }
#endif

    if (verbose)
      printf ("loVal: %d hiVal: %d\n", loVal, hiVal);

#ifdef STANDALONE
    unsigned long i;
    for (i = 0; i < lines * samples; i ++)
      if (buf[ i ] < loVal) {
	if (Verbose)
	  printf ("forcing %d to %d\n", buf[ i ], outMin);
	byteBuf[ i ] = outMin;
      } else if (buf[ i ] > hiVal) {
	if (Verbose)
	  printf ("forcing %d to %d\n", buf[ i ], outMax);
	byteBuf[ i ] = outMax;
      } else {
	if (Verbose)
	  printf ("stretching %d to ", buf[ i ]);
	byteBuf[ i ] = (int) (0.5 + ((buf[ i ] - loVal) * (float) (outMax - outMin) / inRange)) + outMin;
	if (Verbose)
	  printf ("%d\n", byteBuf[ i ]);
      }    

    voidBuf = byteBuf;

    /* buf is the half word buffer holding the input image, which was
       copied to a new buffer byteBuf. */
    free(buf);
#else
    unsigned long i;
    for (i = 0; i < lines * samples; i ++)
      if (buf[ i ] < loVal) {
	if (Verbose)
	  printf ("forcing %d to %d\n", buf[ i ], outMin);
	buf[ i ] = outMin;
      } else if (buf[ i ] > hiVal) {
	if (Verbose)
	  printf ("forcing %d to %d\n", buf[ i ], outMax);
	buf[ i ] = outMax;
      } else {
	if (Verbose)
	  printf ("stretching %d to ", buf[ i ]);
	buf[ i ] = (int) (0.5 + ((buf[ i ] - loVal) * (float) (outMax - outMin) / inRange)) + outMin;
	if (Verbose)
	  printf ("%d\n", buf[ i ]);
      }    
#endif

    halfWord = 0;

    DEBUGMSG("leaving halfToByte branch");

    /* end of halfToByte branch */

  } else if (halfWord) {
    signed short * buf = (short *) voidBuf;
    signed short loVal = firstIndex - 32768;
    signed short hiVal = lastIndex - 32768;
    int inRange = hiVal - loVal;

    DEBUGMSG("entering halfWord branch");

    if (Verbose)
      printf ("loVal: %d hiVal: %d\n", loVal, hiVal);

    unsigned long i;
    for (i = 0; i < lines * samples; i ++)
      if (buf[ i ] < loVal) {
	if (Verbose)
	  printf ("forcing %d to %d\n", buf[ i ], outMin);
	buf[ i ] = outMin;
      } else if (buf[ i ] > hiVal) {
	if (Verbose)
	  printf ("forcing %d to %d\n", buf[ i ], outMax);
	buf[ i ] = outMax;
      } else {
	if (Verbose)
	  printf ("stretching %d to ", buf[ i ]);

	buf[ i ] = (int) (0.5 + ((buf[ i ] - loVal) * (float) (outMax - outMin) / inRange)) + outMin;
	if (Verbose)
	  printf ("%d\n", buf[ i ]);
      }    

    DEBUGMSG("leaving halfWord branch");

  } else {			/* byte to byte */
    unsigned char * buf = (unsigned char *) voidBuf;
    unsigned char loVal = firstIndex;
    unsigned char hiVal = lastIndex;
    int inRange = hiVal - loVal;

    DEBUGMSG("entering byte branch");

    if (verbose)
      printf ("loVal: %d hiVal: %d\n", loVal, hiVal);

    unsigned long i;
    for (i = 0; i < lines * samples; i ++)
      if (buf[ i ] < loVal) {
	if (Verbose)
	  printf ("forcing %d to %d\n", buf[ i ], outMin);
	buf[ i ] = outMin;
      } else if (buf[ i ] > hiVal) {
	if (Verbose)
	  printf ("forcing %d to %d\n", buf[ i ], outMax);
	buf[ i ] = outMax;
      } else {
	if (Verbose)
	  printf ("stretching %d to ", buf[ i ]);
	buf[ i ] = (int) (0.5 + ((buf[ i ] - loVal) * (float) (outMax - outMin) / inRange)) + outMin;
	if (Verbose)
	  printf ("%d\n", buf[ i ]);
      }    

    DEBUGMSG("leaving byte branch");
  }

  if (gauss) {
    DEBUGMSG("starting gauss stretch");

    if (verbose)
      printf("Linear stretch complete, performing Gaussian stretch\n");

    memset(histogram, 0, 256);
#ifdef STANDALONE
    unsigned char * buf = (unsigned char *) voidBuf;
#else
    signed short * buf = (signed short *) voidBuf;
#endif
    unsigned long i;
    for (i = 0; i < lines * samples; i ++)
      histogram[ buf[ i ] ] ++;

    unsigned char lut[ 256 ];

    GaussLut(lut, (int*) histogram, sigma, mean, outMin, outMax);
    
    for (i = 0; i < lines * samples; i ++)
      buf[ i ] = lut[ buf[ i ] ];

    DEBUGMSG("done with gauss stretch");
  }

  /* calculate/display new histogram */

  if (verbose) {
    memset(histogram, 0, sizeof(int) * 65536);
    unsigned long i;
    if (halfWord) {
      signed short * buf = (signed short *) voidBuf;
      for (i = 0; i < lines * samples; i ++)
	histogram[ 32768 + buf[ i ] ] ++;
    } else {
      unsigned char * buf = (unsigned char *) voidBuf;
      for (i = 0; i < lines * samples; i ++)
	histogram[ buf[ i ] ] ++;
    }

    printf("histogram after stretch:\n");
    for (i = 0; i < 65536; i ++)
      if (histogram[ i ])
	printf("%5ld: %d\n", i - (halfWord?32768:0), histogram[ i ]);
  }

  /* write new image */

  if (halfWord) {
    signed short * buf = (signed short *) voidBuf;

    DEBUGMSG("entering halfWord branch of write");

#ifdef STANDALONE
    if (fwrite (buf, sizeof(signed short), lines * samples, outfile) != lines * samples) {
      fprintf(stderr, "fit: error writing output\n");
      return 1;
    }
#else
    for (line = 0; line < lines; line ++)
      zvwrit (outunit, buf + line * samples,
	      "LINE", line + 1, /* LINE is one based; line is zero based */
	      "SAMP", 1,
	      "NSAMPS", samples32, NULL);
#endif

    DEBUGMSG("leaving halfWord branch of write");

  } else {
    unsigned char * buf = (unsigned char *) voidBuf;

    DEBUGMSG("entering byte branch of write");

#ifdef STANDALONE
    if (fwrite (buf, sizeof(unsigned char), lines * samples, outfile) != lines * samples) {
      fprintf(stderr, "fit: error writing output\n");
      return 1;
    }
#else
    for (line = 0; line < lines; line ++)
      zvwrit (outunit, buf + (halfToByte?2:1) * line * samples,
	      "LINE", line + 1, /* LINE is one based; line is zero based */
	      "SAMP", 1,
	      "NSAMPS", samples32, NULL);
#endif

    DEBUGMSG("leaving byte branch of write");
  }

  /* done with output file */

#ifdef STANDALONE
  fclose(outfile);
#else
  zvclose(outunit, NULL);
#endif

  return;
}
