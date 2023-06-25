// Routine marsjplstereo
// Purpose :
//   Creates a disparity map and a range map from stereo images. 
// Implemented for VICAR : Anton B. Ivanov (abi@mipl7.jpl.nasa.gov)
// 
// ------------------------------------------------------------------------

#include "vicmain_c"
#include "zvprintf.h"

#include "mars_support.h"

#include "PigMission.h"
#include "PigFileModel.h"
#include "PigLabelModel.h"
#include "PigPointingModel.h"
#include "PigCameraModel.h"
#include "PigCAHV.h"
#include "cahvor.h"
#include "SimpleImage.h"

#include <stdio.h>
#include <string.h>

// Includes for JPL Stereo algorithm
// Note that they define their own Camera and Image classes.
#include "JPLStereo.h"
#include "Camera.h"
#include "stereo.h"

// buffer sizes in main program - these should be made dynamic, eventually!
#define MAX_INPUTS 3            // Left and right camera images 
#define MAX_OUTPUTS 2           // Disparity maps (2)
#define MAX_NL 10000		// Only used for error check
#define MAX_NS 10000		// Also used for histogram calculation
#define HIST_MAX 32768		// size of histogram (<=0 is ignored)

#define MAX_PIXELS 2048		// max size that JPLStereo can handle.  If the
				// image is bigger than this, it is pre-
				// downsampled and the pyramid level adjusted.

// Utility routines for Vicar to JPLStereo conversion. 

// This routine pre-reads the files and determines the stretch to use to
// convert to byte.

void DetermineStretch (PigFileModel *f1, PigFileModel *f2, PigFileModel *f3,
		       double percent, int separate, int &isByte,
		       unsigned char lut0[HIST_MAX],
		       unsigned char lut1[HIST_MAX],
		       unsigned char lut2[HIST_MAX],
		       int pyrlevel, int &nl, int &ns, int band);

// Calculate a LUT and low/high limits given a histogram and percent value.

void calculateLUT(long histogram[HIST_MAX], unsigned char lut[HIST_MAX],
		  double percent, int &low, int &high);

// This routine reads in Vicar file and returns a pointer to JPLPic structure
// that is supposed to be allocated already. 

void ReadVicar2JPLPic( PigFileModel *file_model, JPLPic* jplpic, int isByte,
		       unsigned char lut[HIST_MAX], int orig_nl, int orig_ns, int nl, int ns,
		       int downsample_count, int band);

// Copies Camera model from Vicar lalbel to JPLPic
void ReadVicar2JPLCam( PigCameraModel *camera_model, JPLCamera* jplcam,
		       int which, int downsample_count);

char *this_program = "marsjplstereo";	// for range.c

static void downsample(SimpleImage<short int> *img_in,
                       SimpleImage<short int> *&img_out);

////////////////////////////////////////////////////////////////////////

void main44()
{
  int i, j;
  int count;
  //int status;

  // Inputs

  int nids;
  char mission[64], instrument[64];
    
  PigFileModel *file_models[MAX_INPUTS];
  PigCameraModel *camera_in[MAX_INPUTS];
  PigPointingModel *pointing_in[MAX_INPUTS];
  int homogeneous_inputs = TRUE;
  PigCoordSystem *cs;

  // Left and right cameras with CAHV vectors
  //PigCAHV      *leftcam, *rightcam; 

  // Outputs

  int num_out_files;
  int out_unit[MAX_OUTPUTS], out_band[MAX_OUTPUTS];

  // Correlator objects

  JPLCamera *leftCam = NULL, *rightCam = NULL, *lowerCam = NULL;
  JPLPic    *leftpic = NULL, *rightpic = NULL, *lowerpic = NULL;
  JPLStereo *stereo = NULL; 

  // Input Parameters for the JPL Stereo algorithm. 
  // The default values are assigned here. 
  // But in fact VICAR PDF can handle the assignment of default values. 
  // These values will not be used, but those in PDF file will 

  int pyrlevel    =  0; 
  int corrWinSize =  7;
  int minDisp     =  0;
  int maxDisp     = 63;
  int blobSize    = -1;
  int uncertainty = 0;
  int uncertainty_count = 0;
 
  float separation      = 0.2f;
  float pixel_threshold = 1.0f; 

  float percent   = 0.4;		// percent stretch
  int separate = 0;			// true==separate stretch, f==combined

  int downsample_count = 0;		// # of 2x downsamples for MAX_PIXELS

  zvmessage("MARSJPLSTEREO version 2020-03-24", "");

  // Get the input file list, and set up initial camera/pointing models
  // for each input.  Although we accept two or three inputs only,
  // mars_setup() does lots of other nice things for us.

  mars_setup(nids, file_models, camera_in, pointing_in, NULL, cs,
	     mission, instrument, homogeneous_inputs,
	     MAX_NL, MAX_NS, MAX_INPUTS);

  if (nids < 2 || nids > 3) {
    zvmessage("MARSJPLSTEREO requires 2 inputs (or 3 for trinocular)", "");
    zabend();
  }

  if (nids == 2)
    file_models[2] = NULL;		// flag as not available

  // Print out input status from labels

  mars_print_inputs(nids, pointing_in, camera_in, file_models,
		    homogeneous_inputs, mission, instrument);

  // Check input files for consistency
  if ( file_models[0] -> getNL() != file_models[1] -> getNL() ) {
    zvmessage( "MARSJPLSTEREO: Number of lines must be same in LEFT and RIGHT images", " ");
    zabend();
  }

  if ( file_models[0] -> getNS() != file_models[1] -> getNS() ) {
    zvmessage( "MARSJPLSTEREO: Number of samples must be same in LEFT and RIGHT images", " ");
    zabend();
  }

  // Check BAND parameter
  int band = 0;
  zvp("BAND", &band, &count);
  if (band > file_models[0]->getNB()) {
    zvnprintf(255, "Input band (%d) is greater than number of bands in input "
	      "image. Band set to 1.", band);
    band = 1;
  }

  // Get parameters for the stereo correlation algorithm

  // Pyramid level. 
  zvp( "PYRLEVEL", &pyrlevel, &count); 

  // Correlation window size
  zvp( "WINDOWSIZE", &corrWinSize, &count);

  // Minium and maximum allowable disparity
  zvp ( "MINDISP", &minDisp, &count); 
  zvp ( "MAXDISP", &maxDisp, &count); 
    
  if ( minDisp > maxDisp ) {
    zvnabend( 255, "Minimum dispary [%d] must be less than maximum disparity [%d]", minDisp, maxDisp);
  }

  // Blob size
  zvp( "BLOBSIZE", &blobSize, &count); 

  // Uncertainty filtering
  zvp("UNCERTAINTY", &uncertainty, &uncertainty_count);

  // Two trinocular parameters. 
  zvp( "SEPARATION", &separation, &count); 
  zvp( "THRESHOLD",  &pixel_threshold, &count); 

  // Check input parameters and print them out
  zvnprintf( 255, "Pyrlevel %d\nWindow size %d\nDisparity range : [%d %d]\nBlob size %d\nThreshold %f\nSeparation %f", 
	     pyrlevel,    corrWinSize,    minDisp, maxDisp,          blobSize,     separation,   pixel_threshold );

  if (uncertainty_count > 0) {
    zvnprintf(255, "Uncertainty filter enabled, value = %d", uncertainty);
  }

  // Percent stretch limit
  zvp("PERCENT", &percent, &count);
  // Whether to do separate or combined stretches
  separate = zvptst("SEPARATE");

  // Determine the stretch to use

  int orig_nl=0, orig_ns=0;
  int nl=0, ns=0, isByte=FALSE;
  unsigned char lut0[HIST_MAX], lut1[HIST_MAX], lut2[HIST_MAX];

  DetermineStretch (file_models[0], file_models[1], file_models[2],
		    percent, separate, isByte, lut0, lut1, lut2,  
		    pyrlevel, nl, ns, band);

  // Adjust pyramid level for any required pre-downsamples

  orig_nl = nl;
  orig_ns = ns;
  while (nl > MAX_PIXELS || ns > MAX_PIXELS) {
    pyrlevel--;
    if (pyrlevel < 0)
      zvnabend(255, "Not enough pyramid levels to fit within %d pixels",
	       MAX_PIXELS);
    nl /= 2;
    ns /= 2;
    downsample_count++;
    maxDisp /= 2;
  }

  int res = 1 << pyrlevel;
  unsigned long int outrows = nl; 
  unsigned long int outcols = ns;
        
  // Now initialize JPL stereo images with the camera information and run the stereo algorithm. 

  // New stereo object initialized with the correlation window size and 
  // values for minimum and maxumum disparity values. 

  stereo = new JPLStereo(corrWinSize, minDisp, maxDisp, NULL);

  // Allocate space for the Images and camera models

  leftpic  = new JPLPic;
  rightpic = new JPLPic;
  leftCam  = new JPLCamera;
  rightCam = new JPLCamera;
    
  // Initialize some options for the JPL Stereo
  stereo->ClearOptions( THRESHOLD_UNCERTAINTY );
  stereo->fixed_blob_filter_size = blobSize;
  stereo->mergeSqThreshold = separation * separation;
  stereo->mergePixelThreshold = pixel_threshold;

  if (uncertainty_count > 0) {
    stereo->SetUncertainty((long)uncertainty);
    stereo->SetOptions(THRESHOLD_UNCERTAINTY);
  }

  JPLPic *disparityPic = NULL;

  // Check that the image dimensions are large enough
  // relative to correlation window size.  We do
  // correlation only if both dimensions are at least
  // 3 times the corr. window size.
  if (((nl/res) >= (3*corrWinSize)) && 
      ((ns/res) >= (3*corrWinSize))) {

    ReadVicar2JPLPic( file_models[0], leftpic, isByte, 
		      lut0, orig_nl, orig_ns, nl, ns, downsample_count, 
		      band);
    ReadVicar2JPLPic( file_models[1], rightpic, isByte, 
		      lut1, orig_nl, orig_ns, nl, ns, downsample_count, 
		      band); 

    
    // Initialize cameras with values from PIG.
    // First, set their coord systems to be consistent.  This is a no-op
    // for standard stereo (the CS's are already identical) but is
    // critical for long-baseline stereo.
    // We probably should do the setCoordSystem unconditionally, but I'm
    // nervous about changing it for the bulk of standard stereo products.

    if (camera_in[0]->getCoordSystem() != camera_in[1]->getCoordSystem()) {
      camera_in[0]->setCoordSystem(cs);
      camera_in[1]->setCoordSystem(cs);
    }

    ReadVicar2JPLCam( camera_in[0], leftCam, 1, downsample_count); 
    ReadVicar2JPLCam( camera_in[1], rightCam, 2, downsample_count); 

    zvmessage("Correlating...", "");
	
    //Decide whether to do trinocular or binocular stereo. 
    // Trinocular stereo was never tested beacuse of absence of test images. 
    if ( nids == 3 ) {
      lowerCam = new JPLCamera;
      lowerpic = new JPLPic; 
	  
      ReadVicar2JPLPic( file_models[2], lowerpic, isByte, lut2,
			orig_nl, orig_ns, nl, ns, downsample_count, band); 
      camera_in[2]->setCoordSystem(cs);
      ReadVicar2JPLCam( camera_in[2], lowerCam, 3, downsample_count);
	  
      stereo->TrinocularStereo (rightpic, lowerpic, leftpic,
				rightCam, lowerCam, leftCam, pyrlevel);
    } else {
      // Do binocular stereo
	  
      stereo->MatchStereo (leftpic, rightpic, leftCam, rightCam, pyrlevel,
			   minDisp, maxDisp, 0);
    }

    // Write out disparity map in Vicar format
	
    // Shortcut for the disparity picture
    disparityPic = stereo -> subDispPic; 

    if (disparityPic) {
      outrows = disparityPic -> rows; 
      outcols = disparityPic -> cols;
    }
  }
  // If image is too small, just set the right output dimensions
  // without doing any correlation.
  else {   
    outrows = outrows/(1 << pyrlevel); 
    outcols = outcols/(1 << pyrlevel);    
  }

  zvpcnt("OUT", &num_out_files);
  int num_bands = 2;			// assume same file, bands 1 and 2
  out_band[0] = 1;
  out_band[1] = 2;
  if (num_out_files > 1) {		// separate files, band 1 for each
    num_bands = 1;
    out_band[1] = 1;
  }

  // Open the output file (or line disparity file if two)

  zvunit(&out_unit[0], "OUT", 1, NULL);
  zvopen(out_unit[0], "op", "write",
	 "u_ns", outcols, "u_nl", outrows, "u_nb", num_bands,
	 "open_act", "sa", "u_org", "bsq",
	 "u_format", "real", "o_format", "real", NULL);
  zvplabel(out_unit[0], 0, 1);

  // We need mission object to write out output label.
  PigMission *m = PigMission::getMissionObject(mission);
  PigLabelModel *labelModel_0 = NULL;
  if (m)
    labelModel_0 = m->createLabelModel(out_unit[0]);

  // Open the sample disparity file if needed

  if (num_out_files <= 1) {
    out_unit[1] = out_unit[0];		// share the file

    // write output label for 2-banded image
    if (labelModel_0)
      labelModel_0->setDisparity(file_models, file_models[1], nids, 
				 "DISPARITY_MAP");
	
  }
  else {
    zvunit(&out_unit[1], "OUT", 2, NULL);
    zvopen(out_unit[1], "op", "write",
	   "u_ns", outcols, "u_nl", outrows, "u_nb", num_bands,
	   "open_act", "sa", "u_org", "bsq",
	   "u_format", "real", "o_format", "real", NULL);
    zvplabel(out_unit[1], 0, 1);

    // Write output label here for two 1-banded images
    if (m) {
      PigLabelModel *labelModel_1 = NULL;
      labelModel_1 = m->createLabelModel(out_unit[1]);
      if (labelModel_0)
	labelModel_0->setDisparity(file_models, file_models[1], nids, 
				   "DISPARITY_LINE_MAP");
      if (labelModel_1)
	labelModel_1->setDisparity(file_models, file_models[1], nids, 
				   "DISPARITY_SAMPLE_MAP");
    }	
  }

  // Form VICAR file line by line. 

  float *linedisp = new float[outcols];
  float *sampdisp = new float[outcols];
  if (linedisp == NULL || sampdisp == NULL) {
    zvmessage("Insufficient memory allocating linedisp or sampdisp!", "");
    zabend();
  }

  zvmessage( "Writing disparity maps", " "); 

  if (disparityPic) {
    for ( j = 0; j < outrows; j++) {
	    
      unsigned char *disp = disparityPic -> GetPixelAddress( j, 0);
	    
      // Convert from JPL Stereo INT16_PIXEL to REAL pixel. 
      for ( i = 0; i < outcols; i++ ) {
		
	unsigned short int value = *((unsigned short int *) (disp + i * 2));
		
	// if disparity is 0 it is 0 in both line and samples
		
	if ( value == 32767 ) 
	  linedisp[i] = sampdisp[i] = 0.0;
	else {
	  // Make up the line disparity map  it is all just the line#
	  linedisp[i] = j + 1;
		    
	  // Add sample offset and reverse sense of the disparity map
	  sampdisp[i] = i - value / 256.0  + 1;
	}
      }
      zvwrit(out_unit[0], linedisp, "LINE", j+1, 
	     "BAND", out_band[0], NULL);
      zvwrit(out_unit[1], sampdisp, "LINE", j+1, 
	     "BAND", out_band[1], NULL);
    }
  }
  else {
    for (int cnt = 0; cnt < outcols; cnt++) {
      linedisp[cnt] = 0.0;
    }
    for (j = 0; j < outrows; j++) {
      zvwrit(out_unit[0], linedisp, "LINE", j+1, 
	     "BAND", out_band[0], NULL);
      zvwrit(out_unit[1], linedisp, "LINE", j+1, 
	     "BAND", out_band[1], NULL);
    }
  }

  // Close output files 
  zvclose(out_unit[0], NULL);
  if (num_out_files > 1)
    zvclose(out_unit[1], NULL);

  // De-allocate memory. 
  delete linedisp;
  delete sampdisp; 

  // Done!

}

// ------------------------------------------------------------------------
// Accumulate the histogram of this file.  Histogram is added to, not reset.
// ------------------------------------------------------------------------

void GatherHist(int unit, long histogram[], int nl, int ns, int band)
{
  static short int buf[MAX_NS];

  for (int line=0; line < nl; line++) {
    zvread(unit, buf, "line", line+1, "BAND", band, NULL);

    for (int samp=0; samp < ns; samp++) {
      short int pixel = buf[samp];
      if (pixel < 0) pixel = 0;		// trim off 0 and negative
      histogram[pixel]++;
    }
  }
}

// ------------------------------------------------------------------------
// Calculate the histograms of the input files (2 or 3) as a combined unit.
// Do a percent stretch and return the halfword lookup table to use.  This
// is used to scale all the files to byte consistently.
//
// Note:  if the files are already byte, isByte is set to TRUE and we
// immediately return.  No need to do a stretch in that case.
//
// The files are closed for BYTE data so they can be re-opened properly.
// Otherwise, they are left open since the same read must be done again.
//
// The "percent" parameter is a percentage... 0.0 to 100.0, not 0.0 to 1.0.
// It should normally be *very* low.
// ------------------------------------------------------------------------

void DetermineStretch (PigFileModel *f1, PigFileModel *f2, PigFileModel *f3,
		       double percent, int separate, int &isByte,
		       unsigned char lut0[HIST_MAX],
		       unsigned char lut1[HIST_MAX],
		       unsigned char lut2[HIST_MAX],
		       int pyrlevel, int &nl, int &ns, int band)
{
  int i;

  memset(lut0, 0, HIST_MAX);
  memset(lut1, 0, HIST_MAX);
  memset(lut2, 0, HIST_MAX);

  zveaction("SA", "");		// die on error

  f1->closeFile();
  f2->closeFile();
  if (f3) f3->closeFile();

  // Re-open the files using U_FORMAT HALF for ease of histogram calculation.

  zvopen(f1->getUnit(), "U_FORMAT", "HALF", NULL);
  f1->setFileOpen(TRUE);
  zvopen(f2->getUnit(), "U_FORMAT", "HALF", NULL);
  f2->setFileOpen(TRUE);
  if (f3) {
    zvopen(f3->getUnit(), "U_FORMAT", "HALF", NULL);
    f3->setFileOpen(TRUE);
  }

  isByte = FALSE;
  char format[32], iformat[32];	// i versions are temporaries
  int inl, ins;

  zvget(f1->getUnit(), "format", format, "nl", &nl, "ns", &ns, NULL);
  zvget(f2->getUnit(), "format", iformat, "nl", &inl, "ns", &ins, NULL);

  if (strcmp(format, iformat) != 0) {
    zvmessage("Inputs 1 and 2 must have the same data type", "");
    zabend();
  }
  if (nl != inl || ns != ins) {
    zvmessage("Inputs 1 and 2 must have the same image size", "");
    zabend();
  }

  if (f3) {
    zvget(f3->getUnit(), "format", iformat, "nl", &inl, "ns", &ins, NULL);

    if (strcmp(format, iformat) != 0) {
      zvmessage("Inputs 1 and 3 must have the same data type", "");
      zabend();
    }
    if (nl != inl || ns != ins) {
      zvmessage("Inputs 1 and 3 must have the same image size", "");
      zabend();
    }
  }

  // truncate size of the image if necessary
  // to accomodate pyrlevel
  int res = 1 << pyrlevel;
  int trunc_nl = ((int)(nl/res)) * res;
  int trunc_ns = ((int)(ns/res)) * res;
  if (trunc_nl != nl || trunc_ns != ns) {
    zvmessage("Truncating input image size to be a max. multiple of pyramid level...", "");
    nl = trunc_nl;
    ns = trunc_ns;

    // make sure we don't end up with 0 image
    if (nl < 1) {
      zvmessage("Assigning number of lines to the min. valid value nl=1", "");
      nl = 1;
    }
    if (ns < 1) {
      zvmessage("Assigning number of samples to the min. valid value ns=1", "");
      ns = 1;
    }
  }

  if (strcmp(format, "BYTE") == 0) {		// Byte, do nothing
    f1->closeFile();
    f2->closeFile();
    if (f3) f3->closeFile();
    for (i=0; i<255; i++) {
      lut0[i] = i;			// just in case
      lut1[i] = i;
      lut2[i] = i;
    }
    isByte = TRUE;

    return;
  }

  isByte = FALSE;

  // Gather the combined histogram for the two (or three) files

  long histogram0[HIST_MAX];
  long histogram1[HIST_MAX];
  long histogram2[HIST_MAX];
  memset(histogram0, 0, HIST_MAX * sizeof(long));
  memset(histogram1, 0, HIST_MAX * sizeof(long));
  memset(histogram2, 0, HIST_MAX * sizeof(long));

  GatherHist(f1->getUnit(), histogram0, nl, ns, band);
  GatherHist(f2->getUnit(), histogram1, nl, ns, band);
  if (f3)
    GatherHist(f3->getUnit(), histogram2, nl, ns, band);

  int low,  high;

  zvnprintf(256, "Applying %f percent stretch", percent);

  if (separate) {			// Separate stretch per image

    calculateLUT(histogram0, lut0, percent, low, high);
    zvnprintf(256, "Stretching image 1 from %d to %d", low, high);

    calculateLUT(histogram1, lut1, percent, low, high);
    zvnprintf(256, "Stretching image 2 from %d to %d", low, high);

    if (f3) {
      calculateLUT(histogram2, lut2, percent, low, high);
      zvnprintf(256, "Stretching image 3 from %d to %d", low, high);
    }
  }
  else {				// Combined stretch
    for (int i=0; i < HIST_MAX; i++) {
      histogram0[i] += histogram1[i];
      if (f3)
	histogram0[i] += histogram2[i];
    }

    calculateLUT(histogram0, lut0, percent, low, high);
    memcpy(lut1, lut0, HIST_MAX * sizeof(lut0[0]));
    if (f3)
      memcpy(lut2, lut0, HIST_MAX * sizeof(lut0[0]));

    zvnprintf(256, "Stretching all images from %d to %d", low, high);
  }
}

// ------------------------------------------------------------------------
// Calculate a LUT and low/high limits given a histogram and percent value.
// ------------------------------------------------------------------------

void calculateLUT(long histogram[HIST_MAX], unsigned char lut[HIST_MAX],
		  double percent, int &low, int &high)
{
  int i;

  // Calculate the total of the histogram, excluding 0

  double total = 0.0;
  for (i=1; i < HIST_MAX; i++)
    total += (double)histogram[i];
  if (total == 0.0)
    total = 0.001;		// avoid divide by 0

  // Now compute the stretch limits.  This is all shamelessly ripped off
  // from VIDS.

  double lTotal = 0.0;
  double hTotal = 0.0;
  percent /= 100.0;		// make it 0..1

  low = 1;
  high = HIST_MAX-1;

  for (i = 1; i < HIST_MAX; i++) {
    lTotal += (double)histogram[i];

    if ((lTotal / total) > percent)
      break;
    low = i;
  }

  for (i = HIST_MAX-1; i >= 1; i--) {
    hTotal += (double)histogram[i];

    if ((hTotal / total) > percent)
      break;
    high = i;
  }

  // Fill up the LUT.  Since 0 indicates transparent, we _should_ map
  // anything 0<pixel<lowStretch to 1.  But, the correlator doesn't know
  // anything about 0 being transparent, so there's no need to bother.
  // Note that the LUT is for positive numbers only (0-32767).

  if (high == low) low--;		// avoid divide by 0
  if (high < low) low = high-1;	// shouldn't happen

  double m = 255.0 / (high - low);
  double b = (-m) * low + 0.5;

  for (i=0; i<HIST_MAX; i++) {
    int j = (int) (m * i + b);
    if (j < 0) j = 0;
    if (j > 255) j = 255;
    lut[i] = j;
  }
}

// ------------------------------------------------------------------------
// This routine reads in Vicar file and returns a pointer to JPLPic structure
// that is supposed to be allocated already. 
// ------------------------------------------------------------------------

void ReadVicar2JPLPic( PigFileModel *file_model, JPLPic* jplpic, int isByte,
		       unsigned char lut[HIST_MAX], int orig_nl, int orig_ns, int nl, int ns,
		       int downsample_count, int band)
{
  int unit = file_model->getUnit();

  // It's pretty inefficient to read into an array just so
  // LoadFromMemory can copy it into its own array... but that's how
  // the routine is written.

  SimpleImage<short int> *half_img=NULL, *temp_img=NULL;

  half_img = new SimpleImage<short int>(orig_nl, orig_ns);
  if (half_img == NULL) {
    zvmessage("Error allocating memory in ReadVicar2JPLPic half_img!","");
    zabend();
  }

  if (file_model->isFileOpen())
    file_model->closeFile();
  zvopen(unit, "U_FORMAT", "HALF", NULL);
  file_model->setFileOpen(TRUE);

  for (int line = 0; line < orig_nl; line++) {
    zvread(unit, half_img->linePtr(line), "line", line+1, "nsamps", orig_ns,
	   "BAND", band, NULL);
  }
  file_model->closeFile();

  // Downsample as needed

  for (int i=0; i < downsample_count; i++) {
    downsample(half_img, temp_img);
    delete half_img;
    half_img = temp_img;
    temp_img = NULL;
  }

  // Image should now be nl, ns

  SimpleImage<unsigned char> *byte_img=new SimpleImage<unsigned char>(nl, ns);
  if (byte_img == NULL) {
    zvmessage("Error allocating memory in ReadVicar2JPLPic byte_img!","");
    zabend();
  }

  // Non-byte case requires stretching

  for (int line = 0; line < nl; line++) {
    for (int samp = 0; samp < ns; samp++) {
      short int pixel = half_img->get(line,samp);
      if (pixel < 0) pixel = 0;
      unsigned char byte_pixel;
      if (isByte)
	byte_pixel = (unsigned char)pixel;
      else
	byte_pixel = lut[pixel];
      byte_img->set(line, samp, byte_pixel);
    }
  }
  delete half_img;

  jplpic->LoadFromMemory(byte_img->linePtr(0), nl, ns, UC8_PIXEL);

  delete byte_img;

}

// ------------------------------------------------------------------------
// Converts Camera model from PIG format to JPLCamera format
// ------------------------------------------------------------------------

void ReadVicar2JPLCam(PigCameraModel *camera_model, JPLCamera* jplcam,
		      int which, int downsample_count)
{
  int i;

  // Check to make sure we have a CAHV and cast it

  if (strcmp(camera_model->getModelName(), "CAHV") != 0)
    zvnabend(80, "Camera model must be of CAHV type.  Got %s instead for input %d",
	     camera_model->getModelName(), which);

  PigCAHV *cm = (PigCAHV *)camera_model;

  // Now get the component values

  PigPoint c;
  PigVector a, h, v;
  double  inC[3], inA[3], inH[3], inV[3];  

  cm->getCurrentCAHV(c, a, h, v);

  // Adjust the camera model if we had to pre-downsample

  for (i=0; i < downsample_count; i++) {
    cm->scaleCamera(0.5, 0.5);
  }

  c.getXYZ(inC);
  a.getXYZ(inA);
  h.getXYZ(inH);
  v.getXYZ(inV);

  // Set the OR vectors, just in case...

  double  inO[3] = { inA[0], inA[1], inA[2] }; 
  double  inR[3] = { 0.0, 0.0, 0.0}; 

  // Calculate all the other silly parameters.  These are probably not
  // needed, but we might as well...  Note that we just punt on s and
  // s_int since they're not available nor are they used.

  double incenter[2], inscale[2], intheta ;
  double ins[18*18], ins_int[5*5];

  cmod_cahv_internal(inC, inA, inH, inV, NULL, &inscale[0], &incenter[0],
		     &inscale[1], &incenter[1], &intheta, NULL);

  // Set both covariance matrix and s_int matrix to 0s. 
  for ( i = 0; i< 18 * 18; i++ ) ins[i] = 0.0;
  for ( i = 0; i< 5 * 5; i++ )   ins_int[i] = 0.0; 
  
  jplcam ->InitJPLCamera( CAHV_MODEL, inC, inA, inH, inV, inO, inR, 
			  incenter, inscale, &intheta, 
			  ins, ins_int);

}

////////////////////////////////////////////////////////////////////////
// Downsample the given image by a factor of two.  The output image is
// re-allocated if non-null, and then the original output is deleted
// *after* the operation.  This allows the input to be the same as the
// output on entry (in which case the input will be deleted).
// Uses simple averaging.  Output is sized nl/2, ns/2.
// Cribbed from marscor3 (converted to short int)
////////////////////////////////////////////////////////////////////////

static void downsample(SimpleImage<short int> *img_in,
                       SimpleImage<short int> *&img_out)
{
  int nl = img_in->getNL();
  int ns = img_in->getNS();
  SimpleImage<short int> *save_output = img_out;
  img_out = new SimpleImage<short int>(nl/2, ns/2);
  if (img_out == NULL) {
    zvmessage("Error allocating memory in downsample!", "");
    zabend();
  }

  for (int j=0; j < nl-1; j+=2) {
    int jj = j / 2;
    for (int i=0; i < ns-1; i+=2) {
      int ii = i / 2;

      double sum = img_in->get(j,i) + img_in->get(j+1,i) +
	img_in->get(j,i+1) + img_in->get(j+1,i+1);
      img_out->set(jj,ii, sum / 4.0);
    }
  }
  if (save_output != NULL)
    delete save_output;
}
