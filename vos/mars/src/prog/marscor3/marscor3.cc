/* marscorr */
#include "vicmain_c"

#include "mars_support.h"
#include "gruen.h"
#include "GruenTransform.h"
#include "mars_tiepoints.h"

#include "PigMission.h"
#include "PigLabelModel.h"
#include "PigFileModel.h"

#include <math.h>
#include <stdlib.h>
#include <valarray>
#include <set>
#include <vector>
#include <algorithm>
#include <utility>
#include <numeric>
#include <iostream>

#include <Eigen/Dense>
#include <Eigen/SVD>

/* buffer sizes in main program */
#define MAX_INPUTS 2
#define MAX_NL MARS_MAX_NL
#define MAX_NS MARS_MAX_NS

#define EMPTY -32767
#define FAILED -32766		// Failed for unspecified reason
#define GOOD_CORR 0		// Really, anything >0 is good
#define QUAL_FACTOR 10000.	/* scaling for qual array */

#ifndef TRUE
#define TRUE 1
#endif
#ifndef FALSE
#define FALSE 0
#endif

#define BOX_FILTER 1
#define GAUSS_FILTER 2



using namespace std;
using namespace Eigen;
#define	PI 3.14159265358979323846

// Container related to the Gaussian profile used to prefilter+sample an
// image using the Ellipsoid Weighted Averag (ewa)
struct gaussProfile{float sigma; int numSigmas; int numSamplesPerSigma;
                    std::vector<float> gauss;};


// Structure to hold various things needed by the correlator
struct CorrelParams {
	SimpleImage<short int> *qual;		// correl quality of each point
	SimpleImage<double> *line_mask;		// matching line # in right img
	SimpleImage<double> *samp_mask;		// matching line # in right img
	SimpleImage<double> *left_img;		// current entire left image
	SimpleImage<double> *right_img;		// current entire right image
        SimpleImage<double> *in_disp_line;	// input line disp
        SimpleImage<double> *in_disp_samp;	// input samp disp
	int nlw, nsw;				// left window size (template)
	int nlw2, nsw2;				// right window size (search)
	double min_quality;			// minimum acceptable corr qual
	int npts;				// number of tiepoints found
	double ftol;				// tolerance for qual in amoeba
	double check;				// threshold for reverse check
	double check_quality;			// quality for reverse check
        SimpleImage<double> *orig_left_img;	// entire original left image
        SimpleImage<double> *orig_right_img;	// entire original right image
	double initial_line_coef[4];		// Initial gruen coefficients
	double initial_samp_coef[4];		// Initial gruen coefficients
	double line_coef_limits[3][2];		// Coefficient limits (prevents
	double samp_coef_limits[3][2];		// walking too far)
	int use_limits;				// True to use the limits
	int use_input_coefs;
	int feedback_coefs;
	SimpleImage<double> *in_coef_a;		// input coefficients
	SimpleImage<double> *in_coef_b;
	SimpleImage<double> *in_coef_d;
	SimpleImage<double> *in_coef_e;
	SimpleImage<double> *in_coef_g;
	SimpleImage<double> *in_coef_h;
	int save_output_coefs;			// True if we use the below
	SimpleImage<double> *out_coef_a;	// output coefficients
	SimpleImage<double> *out_coef_b;
	SimpleImage<double> *out_coef_d;
	SimpleImage<double> *out_coef_e;
	SimpleImage<double> *out_coef_g;
	SimpleImage<double> *out_coef_h;

        int scaling;                       // True is patches are to be scaled 
        float scalingThreshold;            // Threshold to activate scaling 
        int tiling;                        // True if accounting for tiling 
        int decimation;                    // True if subsampling scaled patches

        int pyr; //Pyramid level of input image   
        float maxResRatio;
        float localCoefLimit;
        double coefLimits[6];

        SimpleImage<int> *left_tile;
        SimpleImage<int> *right_tile;

        int statFilter;                         // True if apply stat filter

        std::vector<std::vector<SimpleImage<double> *> > mipmapL;  
        std::vector<std::vector<SimpleImage<double> *> > mipmapR;
        gaussProfile gaussP;
};


static int get_best_neighbor(SimpleImage<short int> *qual, int line, int samp,
	int &max_qual_line, int &max_qual_samp);
static int do_correlation(int left_line, int left_samp, int mode,
		int seed, double seed_line, double seed_samp, double *seed_coef,
		CorrelParams *p);



std::vector<float> getGaussianProfile(float sigma, 
                                      int numSigmas, 
                                      int numSamplesPerSigma);
 
SimpleImage<double> * ewa(SimpleImage<double> *imgIn,
                  int nbX, int nbY,
                  const std::valarray<float> &matX, 
                  const std::valarray<float> &matY,
                  const gaussProfile &gaussP,
                  const MatrixXf &U,
                  const VectorXf &S,
                  const MatrixXf &V );





static void downsample(SimpleImage<double> *img_in,
		       SimpleImage<double> *&img_out);




static void decimateImage(SimpleImage<double> *img_in,
		          SimpleImage<double> *&img_out,
                          int downX, int downY);
static void decimateImage(SimpleImage<double> *img_in,
		          SimpleImage<double> *&img_out);
static void downsampleImage(SimpleImage<double> *img_in,
		            SimpleImage<double> *&img_out,
                            int down);
static void downsampleImage(SimpleImage<double> *img_in,
		            SimpleImage<double> *&img_out,
                            int downX, int downY);
static void downsampleTiling(SimpleImage<int> *img_in,
		             SimpleImage<int> *&img_out);
static void downsampleTiling(SimpleImage<int> *tile_in,
		             SimpleImage<int> *&tile_out, 
                             int downIndex);
static void lowpassBox(SimpleImage<double> *img_in,
		       SimpleImage<double> *&img_out, 
                       int window);
static void lowpassGauss(SimpleImage<double> *imageIn, 
                         SimpleImage<double> *&imageOut, 
                         double sigmaX, double sigmaY);
static void lowpassGauss(SimpleImage<double> *img_in,
		         SimpleImage<double> *&img_out, 
                         double sigma); 

static int getTilingInformation(int unit, SimpleImage<int> *& tileSampling,
                                std::set<int> &tilingLevel);
static int read_image(char *param, int instance,
		SimpleImage<double> *&image, int band, int close);
void set_up_limits(struct CorrelParams *p, double rot, double rot_delta,
			double xscale, double yscale, double scale_factor);
static void ConvBufferFast(double *buffer, double *kernel, int rsize, int ksize);
static void ConvHorizontal(SimpleImage<double> *image, double *kernel, int ksize, int omp);
static void ConvVertical(SimpleImage<double> *image, double *kernel, int ksize, int omp);

void applyLocalCoherenceFilter(SimpleImage<double> * input, 
                               SimpleImage<short> * mask); 
template<class T>
void applyLocalCoherenceFilter(SimpleImage<T> * input, 
                               SimpleImage<short> * mask, 
                               T value);



void localCoherenceFilter(SimpleImage<short> * corrScores,    
                          SimpleImage<double> * locX,     
                          SimpleImage<double> * locY, 
                          SimpleImage<double> * coefs_a, 
                          SimpleImage<double> * coefs_b, 
                          SimpleImage<double> * coefs_d, 
                          SimpleImage<double> * coefs_e, 
                          SimpleImage<double> * coefs_g, 
                          SimpleImage<double> * coefs_h, 
                          int corrNL,
                          int corrNS,
                          int statFilterExtent, 
                          double statFilterScalerThreshold, 
                          int statFilterThresholdN,
                          int omp_on,
                          SimpleImage<short> *& mask);

template<class T>
void saveImg(const std::string name, SimpleImage<T> * img);

////////////////////////////////////////////////////////////////////////

void main44()
{
    // int s;
    int count, def, status;
    int unit_in[MAX_INPUTS], band;
    int nids;
    const size_t msgLen = 256;
    char msg[msgLen];
    int fwd_count, rev_count, total_count;

    char filename[PIG_MAX_FILENAME_SIZE+1];

    // User parameters


    int templ[2], search[2];
    int mode;
    int disp_zoom, disp_pyramid, stop_pyramid;
    int do_gores, gore_reverse;
    double gore_quality;
    int max_gore_passes;
    int multipass;
    int apply_filter;
    double filter_window_l=0.0, filter_window_r=0.0;

    struct CorrelParams *p;

    // LblImageData_typ ImageData;

    zvmessage("MARSCOR3 version 2019-10-02", "");

    p = new CorrelParams;	// maybe too big for auto variable on Solaris?
    if (p == NULL) {
	zvmessage("Fatal memory error in initialization!", "");
	zabend();
    }
    memset(p, 0, sizeof(*p));


    /**** Read input images ****/

    zvpcnt("INP", &nids);
    if (nids > MAX_INPUTS) {
	zvmessage("Too many Input images.", "");
	zabend();
    }

    zvp("BAND", &band, &count);
    unit_in[0] = read_image("INP", 1, p->orig_left_img, band, false);
    unit_in[1] = read_image("INP", 2, p->orig_right_img, band, false);

    // Get input image sizes
    int nll = p->orig_left_img->getNL();
    int nsl = p->orig_left_img->getNS();
    int nlr = p->orig_right_img->getNL();
    int nsr = p->orig_right_img->getNS();


    multipass = zvptst("MULTIPASS");


    /**** Read input disparity image ****/

    zvp("DISP_PYRAMID", &disp_pyramid, &count);
    disp_zoom = 1 << disp_pyramid;		// convert to zoom factor
    snprintf(msg, msgLen,  "Zooming disparity by %d (pyramid level %d)",
			disp_zoom, disp_pyramid);
    zvmessage(msg, "");

    zvp("STOP_PYRAMID", &stop_pyramid, &count);
    if (stop_pyramid != 0 && stop_pyramid > disp_pyramid-1) {
	stop_pyramid = disp_pyramid-1;
	zvmessage("STOP_PYRAMID truncated to max of DISP_PYRAMID-1", "");
    }
    if (stop_pyramid != 0 && !multipass) {
	zvmessage("STOP_PYRAMID cannot be used if MULTIPASS is not on", "");
	zabend();
    }
    if (stop_pyramid < 0) {
	zvmessage("Invalid STOP_PYRAMID value", "");
	zabend();
    }

    zvpcnt("IN_DISP", &nids);
    if (nids > 2) {
        zvmessage("Too many Input disparity images.", "");
        zabend();
    }

    int disp_unit = read_image("IN_DISP", 1, p->in_disp_line, 1, false);
    int disp_unit2 = -1;
    if (nids == 1) {
	zvclose(disp_unit, NULL);		// so read can re-open it
	disp_unit = read_image("IN_DISP", 1, p->in_disp_samp, 2, false);  // file 1, band 2
    }
    else						// file 2, band 1
	disp_unit2 = read_image("IN_DISP", 2, p->in_disp_samp, 1, false);

    /* check input image dimensions */

    int orig_nlin = ((int)(nll/disp_zoom)) * disp_zoom;
    int orig_nsin = ((int)(nsl/disp_zoom)) * disp_zoom;

    if ((orig_nlin != 0) && (orig_nsin != 0)) {
	if (p->in_disp_line->getNL() * disp_zoom != orig_nlin ||
	    p->in_disp_line->getNS() * disp_zoom != orig_nsin) {
	    zvmessage("Input line disparity size doesn't match image size", "");
	    zabend();
	}
	if (p->in_disp_samp->getNL() * disp_zoom != orig_nlin ||
	    p->in_disp_samp->getNS() * disp_zoom != orig_nsin) {
	    zvmessage("Input samp disparity size doesn't match image size", "");
	    zabend();
	}
    }

    // Check for coefficients input file

    p->in_coef_a = NULL;
    p->in_coef_b = NULL;
    p->in_coef_d = NULL;
    p->in_coef_e = NULL;
    p->in_coef_g = NULL;
    p->in_coef_h = NULL;
    p->use_input_coefs = FALSE;
    int n;
    zvpcnt("IN_COEFS", &n);
    if (n != 0) {
	// It's inefficient to close each time but a lot easier
	read_image("IN_COEFS", 1, p->in_coef_a, 1, true);
	read_image("IN_COEFS", 1, p->in_coef_b, 2, true);
	read_image("IN_COEFS", 1, p->in_coef_d, 3, true);
	read_image("IN_COEFS", 1, p->in_coef_e, 4, true);
	read_image("IN_COEFS", 1, p->in_coef_g, 5, true);
	read_image("IN_COEFS", 1, p->in_coef_h, 6, true);
	p->use_input_coefs = TRUE;
    }
    p->feedback_coefs = zvptst("FEEDBACK_COEFS");
    if (p->feedback_coefs)
	zvmessage("Coefficient feedback between pyramid levels is ON","");

    // Set primary input so labels come from disparity image (so we don't lose 
    // the history of the 1-D correlator run).
    if (zvptst("PI_FROM_DISP"))
        zvselpiu(disp_unit);

    // Allocate temporary images
    p->qual = new SimpleImage<short int>(nll, nsl);
    p->line_mask = new SimpleImage<double>(nll, nsl);
    p->samp_mask = new SimpleImage<double>(nll, nsl);


    // Gget parameter overrides if any
    zvparmd("QUALITY", &p->min_quality, &count, &def, 1, 0);
    zvparmd("FTOL", &p->ftol, &count, &def, 1, 0);
    zvparmd("GORE_QUALITY", &gore_quality, &count, &def, 1, 0);
    if (count == 0)
	gore_quality = p->min_quality;
    zvp("GORE_PASSES", &max_gore_passes, &count);
    do_gores = zvptst("GORES");
    gore_reverse = zvptst("GORE_REVERSE");


    // Determine the gruen mode to use.  See comments at top of gruen.com
    // for details of each mode.
    mode = 2;			// default: only use amoeba in gruen
    if (zvptst("linear_only"))		mode = 0;
    if (zvptst("annealing"))		mode = 1;
    if (zvptst("amoeba"))		mode = 2;
    if (zvptst("linear_amoeba"))	mode = 3;
    if (zvptst("annealing_amoeba"))	mode = 4;
    if (zvptst("amoeba2"))		mode = 5;
    if (zvptst("linear_amoeba2"))	mode = 6;
    if (zvptst("amoeba8"))		mode = 7;
    if (zvptst("amoeba4"))		mode = 8;
    if (zvptst("amoeba5"))		mode = 9;
    // If using cubic interpolator (instead of linear) in gruen
    if (zvptst("cubicgruen")) {
       if (mode == 5 || mode == 6)
          mode = 10;
       else if (mode == 8)
          mode = 11;
       else if (mode == 9)
          mode = 12;
       else if (mode == 2 || mode == 3 || mode == 4)
          mode = 13;
       else if (mode == 7)
          mode = 14;
       else {}
    }

    if (mode == 1 || mode == 4) {
	zvmessage("WARNING!!!! Annealing modes not set up currently", "");
	zvmessage("Extra parameters must be given to gruen() correlator", "");
	zabend();
    }

    if (zvptst("linear"))		// Do linear before the main mode
	mode = -mode;

    zvparmd("CHECK", &p->check, &count, &def, 1, 0);
    if (count == 0)
	p->check = 0.0;			// No reverse check

    // Min quality to run reverse check
    zvparmd("CHECK_QUALITY", &p->check_quality, &count, &def, 1, 0);


    // Filtering

    apply_filter = 0;  // Default no filter
    if (zvptst("filter")) {   // Box filtering
       apply_filter = BOX_FILTER;
       filter_window_l = filter_window_r = 3; //default values for box
    }
    if (zvptst("gaussfilter")) {  // Gaussian filtering
       apply_filter = GAUSS_FILTER;
       filter_window_l = filter_window_r = 0.9; //default values for gauss
    }
    float fw[2];
    zvp("FILTER_SIZE", fw, &count);
    if (count != 0) {
       filter_window_l = fw[0];
       filter_window_r = fw[0];
    }
    if (count == 2)
	filter_window_r = fw[1];

    if (apply_filter == BOX_FILTER && 
          (((int)(filter_window_l + 0.5) % 2) != 1 ||
           ((int)(filter_window_r + 0.5) % 2) != 1)) {

       zvmessage("FILTER_SIZE must be integer odd numbers with box filter","");
       
       if (((int)(filter_window_l + 0.5) % 2) != 1) {
          snprintf(msg, msgLen, "Left box filter size changed from %d pixels to %d pixels.",
               (int)(filter_window_l+0.5), (int)(filter_window_l+0.5) + 1);
          zvmessage(msg, "");
          filter_window_l = (int)(filter_window_l+0.5) + 1;
       }

       if (((int)(filter_window_r + 0.5) % 2) != 1) {
          snprintf(msg, msgLen, "Right box filter size changed from %d pixels to %d pixels.",
               (int)(filter_window_r+0.5), (int)(filter_window_r+0.5) + 1);
          zvmessage(msg, "");
          filter_window_r = (int)(filter_window_r+0.5) + 1;
       }
    }



    // Correlation boxes

    zvp("TEMPLATE", templ, &count);		// Size of area to correlate
    if (count == 1)
	templ[1] = templ[0];			// make it square
    p->nlw = templ[0];
    p->nsw = templ[1];
    if ((p->nlw % 2) == 0)			// make sure it's odd-sized
	p->nlw += 1;
    if ((p->nsw % 2) == 0)
	p->nsw += 1;

    zvp("SEARCH", search, &count);		// Size of area to search
    if (count == 1)
	search[1] = search[0];
    p->nlw2 = search[0];
    p->nsw2 = search[1];
    if ((p->nlw2 % 2) == 0)			// make sure it's odd-sized
	p->nlw2 += 1;
    if ((p->nsw2 % 2) == 0)
	p->nsw2 += 1;

    // Is local coherence filter applied after matching
    p->statFilter = zvptst("STAT_FILTER");

    p->save_output_coefs = FALSE;
    p->out_coef_a = NULL;
    p->out_coef_b = NULL;
    p->out_coef_d = NULL;
    p->out_coef_e = NULL;
    p->out_coef_g = NULL;
    p->out_coef_h = NULL;
    zvpcnt("OUT_COEFS", &n);
    if (n != 0) {
	p->save_output_coefs = TRUE;
    }
    p->out_coef_a = new SimpleImage<double>(nll, nsl);
    p->out_coef_b = new SimpleImage<double>(nll, nsl);
    p->out_coef_d = new SimpleImage<double>(nll, nsl);
    p->out_coef_e = new SimpleImage<double>(nll, nsl);
    p->out_coef_g = new SimpleImage<double>(nll, nsl);
    p->out_coef_h = new SimpleImage<double>(nll, nsl);




    // Check if SCALING is ON.
    // If on, then TEMPLATE/SEARCH values are meant to expressed "real" pixels,
    // that is, non-oversampled pixels. We'll adjust TEMPLATE and SEARCH such 
    // that TEMPLATE pixels are correlated which have not been oversampled, due
    // either to geometric scaling and/or tiling.
    // Check also if the enlarged patches have to be decimated to get only 
    // TEMPLATE pixels that are correlated. The actual footprint of the covered
    // patches is larger, but the number of pixels correlated is the same as
    // TEMPLATE. This is to speed the correlation of scaled patches.
    // Similarly, the SEARCH area will be enlarged if needed
    p->scaling = zvptst("SCALING");
    p->tiling = zvptst("TILING");
    p->decimation = zvptst("DECIMATION");

    float scalingThreshold = 1;
    zvp("SCALING_THRESH", &scalingThreshold, &count);
    p->scalingThreshold = scalingThreshold;


    // Convenience functions
    // Backward compatibility function with DECILEFT
    if (zvptst("DECILEFT")) {
       p->scaling = 1;
       p->tiling = 0;
       p->decimation = 1;
    }


    // For now, neither of SCALING/DECIMATION/TILING are compatible with CHECK. 
    // Verify that both are not turned ON
    if ((p->decimation && p->check) || (p->tiling && p->check) || 
        (p->scaling && p->check)) {
       zvmessage("CHECK cannot be used with SCALING or DECIMATION or TILING.","");
       zabend();
    }


    if (p->scaling == 0 && (p->decimation || p->tiling)) {
       zvmessage("DECIMATION and TILING needs SCALING on. Turning SCALING ON","");
       p->scaling = 1;
    }



   // What is the allowed maximum resolution difference between L and R?
   zvp("MAX_RATIO", &p->maxResRatio, &count);
   if (p->maxResRatio < 1) p->maxResRatio = 1;

    
   // Generate the lookup table on the Gaussian Profile for EWA resampling
   float kewa_sigma = 0.5;
   zvp("KEWA_SIGMA", &kewa_sigma, &count);
   p->gaussP.sigma = kewa_sigma;

   int kewa_nsigma = 3;
   zvp("KEWA_NSIGMA", &kewa_nsigma, &count);
   p->gaussP.numSigmas = kewa_nsigma;

   int kewa_nsamples = 10000;
   zvp("KEWA_NSAMPLES", &kewa_nsamples, &count);
   p->gaussP.numSamplesPerSigma = kewa_nsamples;

   p->gaussP.gauss = getGaussianProfile(p->gaussP.sigma, 
                                        p->gaussP.numSigmas, 
                                        p->gaussP.numSamplesPerSigma); 




    // Prefilter the Left and Right images
    // For performance, we construct a series of lowpass filtered and 
    // downsampled images with various X and Y downsampling factors (X and Y
    // factors are powers of 2 (i.e., 1, 2, 4, 8 etc) but are not necessarily
    // the same). See diagram below representing the 2D container of the 
    // downsampled image. Along the line, the image is reduced in size in the 
    // X direction, and along the column, it is downsampled in the Y direction. 
    // Then, during processing, we'd hand pick in that list of downsampled
    // images, the one that is the closest to what the local transform 
    // between the Left and Right requires.
    // Note that this list of downsampled images does not cover all the affine 
    // transforms. For instance diagonally oriented transforms are not covered,
    // so we'll still need some amount of prefilter+resampling.
    //  ________  ____  __
    // |        ||    ||  |
    // |        ||    ||  |
    // |        ||    ||  |
    // |________||____||__|
    //  ________  ____  __
    // |        ||    ||  |
    // |________||____||__|
    //  ________  ____  __
    // |________||____||__|    
    //


    zvmessage("Generating pyramid of images...","");

    // Left image

    // Number of downsampling levels in X and Y
    int nbLevelY = (int)log2(nll) - (int)log2(p->nlw);
    int nbLevelX = (int)log2(nsl) - (int)log2(p->nsw);

    // Container to store all the downsampled Left images
    auto & mipmapL = p->mipmapL;
    //std::vector< std::vector < SimpleImage<double> * > > mipmapL;
    mipmapL.resize(nbLevelY);
    for (auto & m:mipmapL)
       m.resize(nbLevelX);

    // Set the input raw image to the first location in the container. 
    mipmapL[0][0] = p->orig_left_img;
    for (int j=0; j<nbLevelY; j++) { 
       if (j != 0) 
          downsampleImage(mipmapL[j-1][0], mipmapL[j][0], 0, 2);
       for (int i=1; i<nbLevelX; i++)       
          downsampleImage(mipmapL[j][i-1], mipmapL[j][i], 2, 0);
    }


    // Right image

    // Number of downsampling levels in X and Y
    nbLevelY = (int)log2(nlr) - (int)log2(p->nlw2);
    nbLevelX = (int)log2(nsr) - (int)log2(p->nsw2);


    // Container to store all the downsampled Right images
    //std::vector< std::vector < SimpleImage<double> * > > mipmapR;
    auto & mipmapR = p->mipmapR;
    mipmapR.resize(nbLevelY);
    for (auto & m:mipmapR)
       m.resize(nbLevelX);

    // Set the input raw image to the first location in the container. 
    mipmapR[0][0] = p->orig_right_img;
    for (int j=0; j<nbLevelY; j++) { 
       if (j != 0) 
          downsampleImage(mipmapR[j-1][0], mipmapR[j][0], 0, 2);
       for (int i=1; i<nbLevelX; i++)       
          downsampleImage(mipmapR[j][i-1], mipmapR[j][i], 2, 0);
    }


    // If user asked for FILTER, then lowpass the first (raw image) accordingly.
    // It's done after the prefiltering/downsampling at various levels to avoid 
    // over filtering (FILTER + filter for downsampling)
    if (apply_filter == BOX_FILTER) {
       mipmapL[0][0] = nullptr;
       mipmapR[0][0] = nullptr;
       lowpassBox(p->orig_left_img, mipmapL[0][0], (int)(filter_window_l+0.5));
       lowpassBox(p->orig_right_img, mipmapR[0][0], (int)(filter_window_r+0.5));
    }
    else if(apply_filter == GAUSS_FILTER) {
       mipmapL[0][0] = nullptr;
       mipmapR[0][0] = nullptr;
       lowpassGauss(p->orig_left_img, mipmapL[0][0], filter_window_l);
       lowpassGauss(p->orig_right_img, mipmapR[0][0], filter_window_r);
    }     
    else {}

    zvmessage("Done generating pyramid of images...","");







    float overall_xscale = 1; // Default
    float overall_yscale = 1;

    // Does the input disparity file contains a scale factor?
    // That scale factor is w.r.t. the input images and enbodies geometric
    // scaling difference AND any tiling (downsampling/upsampling).
    double correlationAvgScale = 0;
    status = zlget(disp_unit, "PROPERTY", "CORRELATION_AVERAGE_SCALE", 
                   &correlationAvgScale, "FORMAT", "DOUB", "ERR_ACT", "", 
                   "PROPERTY", "DERIVED_IMAGE_PARMS", NULL);
    if (correlationAvgScale) {
       overall_xscale = correlationAvgScale;
       overall_yscale = correlationAvgScale;
    }

    // Did the user supplied scaling factor? 
    // Retrieve the X/Y scaling factor between L and R if supplied by the user.
    // These scale factors represent nominal resolution difference assuming
    // both images are at full resolution, i.e., no downsampling due to Tiling,
    // which will be accounted for from the labels. These parameters Override 
    // the average scale factor from the label (if any)
    int countXScale = 0;
    int countYScale = 0;
    zvp("XSCALE", &overall_xscale, &countXScale);
    zvp("YSCALE", &overall_yscale, &countYScale);
    if ( (countXScale + countYScale) % 2 ) {  // ~XOR
       zvmessage("Error: Both XSCALE and YSCALE must be supplied","");
       zabend();
    }
 
    // In case the user supplied X/YSCALE, check if the image is tiled, and if 
    // so, retrieve the downsampling values due to tiling to adjust the initial
    // transform as X/YSCALE are expressed w.r.t. full resolution. 
    // This is done irrespective of the TILING parameter, which is used for 
    // scaling the correlation patches (SCALING). If the image is not tiled, no 
    // harm done. 
    if (countXScale) {
       // Get pixel averaging in X of Left image
       int avgWL = 1; //default
       status = zlget(unit_in[0], "PROPERTY", "PIXEL_AVERAGING_WIDTH", 
                       &avgWL, "FORMAT", "INT", "ERR_ACT", "", "PROPERTY",
                       "INSTRUMENT_STATE_PARMS", NULL);

       // Get pixel averaging in Y of Left image
       int avgHL = 1; //default
       status = zlget(unit_in[0], "PROPERTY", "PIXEL_AVERAGING_HEIGHT", 
                      &avgHL, "FORMAT", "INT", "ERR_ACT", "", "PROPERTY",
                      "INSTRUMENT_STATE_PARMS", NULL);

       // Get pixel averaging in X of Right image
       int avgWR = 1; //default
       status = zlget(unit_in[1], "PROPERTY", "PIXEL_AVERAGING_WIDTH", 
                      &avgWR, "FORMAT", "INT", "ERR_ACT", "", "PROPERTY",
                      "INSTRUMENT_STATE_PARMS", NULL);

       // Get pixel averaging in Y of Right image
       int avgHR = 1; //default
       status = zlget(unit_in[1], "PROPERTY", "PIXEL_AVERAGING_HEIGHT", 
                      &avgHR, "FORMAT", "INT", "ERR_ACT", "", "PROPERTY",
                      "INSTRUMENT_STATE_PARMS", NULL);


       // Adjust scale difference in X and Y
       overall_xscale *= pow(2, log2(avgWL)-log2(avgWR));
       overall_yscale *= pow(2, log2(avgHL)-log2(avgHR));
    }


    // Did the user supplied a rotation angle between the two cameras
    float overall_rotation = 0;
    zvp("ROTATION", &overall_rotation, &count);

    // Compute the initial transform parameters between Left and Right from
    // the supplied scaling factor and rotation angle

    overall_rotation = PigDeg2Rad(overall_rotation);

    // Default case:  line = 0 1 0 0, samp = 1 0 0 0 

    // Rotation
    p->initial_samp_coef[0] = cos(overall_rotation);
    p->initial_samp_coef[1] = sin(overall_rotation);
    p->initial_samp_coef[2] = 0.0;		// updated by do_correlation
    p->initial_samp_coef[3] = 0.0;
    p->initial_line_coef[0] = - p->initial_samp_coef[1];	// - sin(angle)
    p->initial_line_coef[1] = p->initial_samp_coef[0];		// cos(angle)
    p->initial_line_coef[2] = 0.0;		// updated by do_correlation
    p->initial_line_coef[3] = 0.0;


    // Scale.  Although scale alone is just a,e, compositing it with rotation
    // impacts all 4: a,b,d,e.
    p->initial_samp_coef[0] = p->initial_samp_coef[0] * overall_xscale;
    p->initial_samp_coef[1] = p->initial_samp_coef[1] * overall_yscale;
    p->initial_line_coef[0] = p->initial_line_coef[0] * overall_xscale;
    p->initial_line_coef[1] = p->initial_line_coef[1] * overall_yscale;









    // If limits are in use, set them up.  

    // Default, no limits on the coefficients
    p->use_limits = FALSE;
    p->localCoefLimit = FALSE;



    // Case where limits are specified as a scale range, and a rotation range 
    // (which are deltas around the overall scale and rotation).  We thus figure
    // out the coefs for all 4 combinations (of min/max scale and min/max 
    // rotation) and find the min and max across all.
    float rot_range[2];
    rot_range[0] = -5.0; rot_range[1] = 5.0;	// in case both aren't given
    zvp("ROT_RANGE", rot_range, &count);
    if (count != 0)
	p->use_limits = TRUE;
    rot_range[0] =  PigDeg2Rad(rot_range[0]);
    rot_range[1] =  PigDeg2Rad(rot_range[1]);

    float scale_range[2];
    scale_range[0] = 0.9; scale_range[1] = 1.1;	// in case both aren't given
    zvp("SCALE_RANGE", scale_range, &count);
    if (count != 0)
	p->use_limits = TRUE;

    
    // Adjust the limits
    if (p->use_limits) {
	p->samp_coef_limits[0][0] = p->initial_samp_coef[0];
	p->samp_coef_limits[0][1] = p->initial_samp_coef[0];
	p->samp_coef_limits[1][0] = p->initial_samp_coef[1];
	p->samp_coef_limits[1][1] = p->initial_samp_coef[1];
	p->samp_coef_limits[2][0] = -1000.0;	// XY terms not limited
	p->samp_coef_limits[2][1] = +1000.0;	// XY terms not limited

	p->line_coef_limits[0][0] = p->initial_line_coef[0];
	p->line_coef_limits[0][1] = p->initial_line_coef[0];
	p->line_coef_limits[1][0] = p->initial_line_coef[1];
	p->line_coef_limits[1][1] = p->initial_line_coef[1];
	p->line_coef_limits[2][0] = -1000.0;	// XY terms not limited
	p->line_coef_limits[2][1] = +1000.0;	// XY terms not limited

	set_up_limits(p, overall_rotation, rot_range[0],
			 overall_xscale, overall_yscale, scale_range[0]);
	set_up_limits(p, overall_rotation, rot_range[1],
			 overall_xscale, overall_yscale, scale_range[0]);
	set_up_limits(p, overall_rotation, rot_range[0],
			 overall_xscale, overall_yscale, scale_range[1]);
	set_up_limits(p, overall_rotation, rot_range[1],
			 overall_xscale, overall_yscale, scale_range[1]);
    }

    
    // Case where limits are specified from the IN_COEFS file
    if (zvptst("COEFS_RANGE_ON")) {
       if (p->use_input_coefs) {

          // Retrieve the min/max of each affine parameter over the entire image
          // to setup the coeficient limits
          double a_min, a_max, b_min, b_max, d_min, d_max, e_min, e_max, g_min,
                 g_max, h_min, h_max;
          a_min = b_min = d_min = e_min = g_min = h_min = 10000000.0;
          a_max = b_max = d_max = e_max = g_max = h_max = -10000000.0;
          for (int j=0; j<p->in_disp_line->getNL(); j++) {
             for (int i=0; i<p->in_disp_line->getNS(); i++) {
                if (p->in_disp_line->get(j,i) != 0) {
                   double a = p->in_coef_a->get(j,i);
                   double b = p->in_coef_b->get(j,i);
                   double d = p->in_coef_d->get(j,i);
                   double e = p->in_coef_e->get(j,i);
                   double g = p->in_coef_g->get(j,i);
                   double h = p->in_coef_h->get(j,i);
                   if(a_min > a) a_min=a;
                   if(a_max < a) a_max=a;
                   if(b_min > b) b_min=b;
                   if(b_max < b) b_max=b;
                   if(d_min > d) d_min=d;
                   if(d_max < d) d_max=d;
                   if(e_min > e) e_min=e;
                   if(e_max < e) e_max=e;
                   if(g_min > g) g_min=g;
                   if(g_max < g) g_max=g;
                   if(h_min > h) h_min=h;
                   if(h_max < h) h_max=h;
                }
             }
          }
          // Setup the limits according to min/max found
          p->samp_coef_limits[0][0] = a_min;
          p->samp_coef_limits[0][1] = a_max; 
          p->samp_coef_limits[1][0] = b_min;
          p->samp_coef_limits[1][1] = b_max;
          p->samp_coef_limits[2][0] = g_min;
          p->samp_coef_limits[2][1] = g_max;

          p->line_coef_limits[0][0] = d_min;
          p->line_coef_limits[0][1] = d_max;
          p->line_coef_limits[1][0] = e_min;
          p->line_coef_limits[1][1] = e_max;
          p->line_coef_limits[2][0] = h_min;
          p->line_coef_limits[2][1] = h_max;

          p->use_limits = TRUE;
       }
       else
          zvmessage("COEFS_RANGE_ON needs IN_COEFS. Ignoring...","");
    }



    // Are the coefs limit set above to be enlarged/reduced
    float coefsScale[6];
    zvp("COEFS_SCALE", coefsScale, &count);
    if (count == 1) { 
       p->coefLimits[0] = coefsScale[0] / 100;     
       p->coefLimits[1] = coefsScale[0] / 100;     
       p->coefLimits[2] = coefsScale[0] / 100;     
       p->coefLimits[3] = coefsScale[0] / 100;     
       p->coefLimits[4] = coefsScale[0] / 100;     
       p->coefLimits[5] = coefsScale[0] / 100;     
    }
    else if (count == 6) {
       p->coefLimits[0] = coefsScale[0] / 100;     
       p->coefLimits[1] = coefsScale[1] / 100;     
       p->coefLimits[2] = coefsScale[2] / 100;     
       p->coefLimits[3] = coefsScale[3] / 100;     
       p->coefLimits[4] = coefsScale[4] / 100;     
       p->coefLimits[5] = coefsScale[5] / 100;     
    }
    else {
       if (count != 0) {
          zvmessage("Limit on transform coefficients (COEFS_SCALE) must be either 1 or 6 values","");
          zabend();
       }
    }
    
    // Update coefficient limit if necessary with the user supplied limit 
    // extension computed above.
    if (count != 0  && p->use_limits) {

      double bracket, min, max;
      bracket = abs(p->samp_coef_limits[0][0]) * p->coefLimits[0];
      min = p->samp_coef_limits[0][0] - bracket; 
      bracket = abs(p->samp_coef_limits[0][1]) * p->coefLimits[0];
      max = p->samp_coef_limits[0][1] + bracket; 
      if (min < max) {
         p->samp_coef_limits[0][0] = min;
         p->samp_coef_limits[0][1] = max;
      }

      bracket = abs(p->samp_coef_limits[1][0]) * p->coefLimits[1];
      min = p->samp_coef_limits[1][0] - bracket; 
      bracket = abs(p->samp_coef_limits[1][1]) * p->coefLimits[1];
      max = p->samp_coef_limits[1][1] + bracket; 
      if (min < max) {
         p->samp_coef_limits[1][0] = min;
         p->samp_coef_limits[1][1] = max;
      }

      bracket = abs(p->samp_coef_limits[2][0]) * p->coefLimits[2];
      min = p->samp_coef_limits[2][0] - bracket; 
      bracket = abs(p->samp_coef_limits[2][1]) * p->coefLimits[2];
      max = p->samp_coef_limits[2][1] + bracket; 
      if (min < max) {
         p->samp_coef_limits[2][0] = min;
         p->samp_coef_limits[2][1] = max;
      }


      bracket = abs(p->line_coef_limits[0][0]) * p->coefLimits[3];
      min = p->line_coef_limits[0][0] - bracket; 
      bracket = abs(p->line_coef_limits[0][1]) * p->coefLimits[3];
      max = p->line_coef_limits[0][1] + bracket; 
      if (min < max) {
         p->line_coef_limits[0][0] = min;
         p->line_coef_limits[0][1] = max;
      }

      bracket = abs(p->line_coef_limits[1][0]) * p->coefLimits[4];
      min = p->line_coef_limits[1][0] - bracket; 
      bracket = abs(p->line_coef_limits[1][1]) * p->coefLimits[4];
      max = p->line_coef_limits[1][1] + bracket; 
      if (min < max) {
         p->line_coef_limits[1][0] = min;
         p->line_coef_limits[1][1] = max;
      }

      bracket = abs(p->line_coef_limits[2][0]) * p->coefLimits[5];
      min = p->line_coef_limits[2][0] - bracket; 
      bracket = abs(p->line_coef_limits[2][1]) * p->coefLimits[5];
      max = p->line_coef_limits[2][1] + bracket; 
      if (min < max) {
         p->line_coef_limits[2][0] = min;
         p->line_coef_limits[2][1] = max;
      }


    } 


    // Lastly, if use_limit is ON, we check that the limit brackets for the
    // coefficient are not too small.
    // This is based on the perturbation brought to the initial coefficients by
    // Gruen to generate the simplex.
    // By default in the code the a, b, d, e coefficients are perturbated by 
    // 0.07 and g, h by 0.01. If the limit bracket is smaller than 4 times these
    // values, the limit are enlarged
    if (p->use_limits) {
       if ((p->samp_coef_limits[0][1] - p->samp_coef_limits[0][0]) < 4 * 0.07) {
          p->samp_coef_limits[0][0] -= 2*0.07;
          p->samp_coef_limits[0][1] += 2*0.07;
       } 
       if ((p->samp_coef_limits[1][1] - p->samp_coef_limits[1][0]) < 4 * 0.07) {
          p->samp_coef_limits[1][0] -= 2*0.07;
          p->samp_coef_limits[1][1] += 2*0.07;
       } 
       if ((p->samp_coef_limits[2][1] - p->samp_coef_limits[2][0]) < 4 * 0.01) {
          p->samp_coef_limits[2][0] -= 2*0.01;
          p->samp_coef_limits[2][1] += 2*0.01;
       } 
       if ((p->line_coef_limits[0][1] - p->line_coef_limits[0][0]) < 4 * 0.07) {
          p->line_coef_limits[0][0] -= 2*0.07;
          p->line_coef_limits[0][1] += 2*0.07;
       } 
       if ((p->line_coef_limits[1][1] - p->line_coef_limits[1][0]) < 4 * 0.07) {
          p->line_coef_limits[1][0] -= 2*0.07;
          p->line_coef_limits[1][1] += 2*0.07;
       } 
       if ((p->line_coef_limits[2][1] - p->line_coef_limits[2][0]) < 4 * 0.01) {
          p->line_coef_limits[2][0] -= 2*0.01;
          p->line_coef_limits[2][1] += 2*0.01;
       } 
    }


    // Are the coef limits defined locally?
    // If yes, then all limits set above are invalidated and will be set
    // on a pixel-by-pixel basis prior to correlation
    p->localCoefLimit = zvptst("LOCAL_LIMIT_ON");

    if (p->localCoefLimit && count == 0) {
       zvmessage("LOCAL_LIMIT _ON requires COEF_RANGE use","");
       zabend();
    }


    if (p->use_limits) {
       if( p->localCoefLimit)
          zvmessage("Use of local transform coefficients limits.","");
       else
          zvmessage("Use of global transform coefficients limits.","");
    }
    else
          zvmessage("No transform coefficients limits.","");

    // Initialization of variables related to tiled processing. A "tiled" image
    // is an image (same dimensions as the image to correlate) that contains the
    // tiling index (1, 2, 4, or 8) which indicates the level of downsampling 
    // sustained by each pixel (in terms of frequency content) w.r.t. the pixel
    // size. A level of 1 is equivalent to a "non-tiled" image.
    SimpleImage<int> * orig_left_tile = nullptr;
    SimpleImage<int> * orig_right_tile = nullptr;
    std::set<int> orig_left_tiling_level;
    std::set<int> orig_right_tiling_level;

    // If tiled processing is required, retrieve the tiling information of both
    // the left and right image. Essentially, a "mask" for both images 
    // indicating for each pixel the amount of on-board downsampling it 
    // sustained. Tile maps are relative to the highest resolution tile in the 
    // image.
    if (p->tiling) {

       orig_left_tile = new SimpleImage<int>(nll, nsl);
       orig_right_tile = new SimpleImage<int>(nlr, nsr);

       // Set default tiling to 1 (i.e., no tiling). Then updates the tiling
       // according to label. It's set by default to 1, bc if some tiles are
       // missing, we don't want tiling level to be at 0.
       std::fill(orig_left_tile->linePtr(0), orig_left_tile->linePtr(0)+nll*nsl, 1);
       std::fill(orig_right_tile->linePtr(0), orig_right_tile->linePtr(0)+nlr*nsr, 1);

       // Retrieve tiling information from label
       int statusL = getTilingInformation(unit_in[0], orig_left_tile, orig_left_tiling_level); 
       int statusR = getTilingInformation(unit_in[1], orig_right_tile, orig_right_tiling_level); 


       if (statusL < 1 && statusR < 1) {
          zvmessage("Invalid tiling information on both inputs; Turning tiling off","");
          p->tiling = 0;
          orig_left_tile->free();       
          orig_right_tile->free();
          orig_left_tile = nullptr;
          orig_right_tile = nullptr;
          orig_left_tiling_level.clear();
          orig_right_tiling_level.clear();
       }
       else {
          // No valid information on tiling for left image. Assume full res
          if (statusL < 1) {
             zvmessage("No valid tiling info for INP 1; Assuming no tiling","");
             std::fill(orig_left_tile->linePtr(0), orig_left_tile->linePtr(0) + 
                       nll*nsl, 1);
             orig_left_tiling_level.clear();
             orig_left_tiling_level.insert(1);
          }
          // No valid information on tiling for right image. Assume full res
          if (statusR < 1) {
             zvmessage("No valid tiling info for INP 2; Assuming no tiling","");
             std::fill(orig_right_tile->linePtr(0), orig_right_tile->linePtr(0)+
                       nlr*nsr, 1);
             orig_right_tiling_level.clear();
             orig_right_tiling_level.insert(1);
          }
       }
    }



    /* clear correlation quality array */

    for (int j=0; j < p->qual->getNL(); j++) {
	for (int i=0; i < p->qual->getNS(); i++) {
	    p->qual->set(j,i, EMPTY);
	}
    }
    p->npts=0;

    int omp_on = zvptst("OMP_ON");



   // Set up transform to adjust Right pixel estimate from reading disparity at
   // a superior zoom level.
   // Traditionally we just added one pixel to the coordinate, but that's not 
   // right if there are differential zooms in effect.  This is not a critical 
   // code section so we always use an amoeba8 transform for simplicity.
   GruenTransform *xform = GruenTransform::create(Amoeba8);
   double xform_buf[8];




    /************* Loop through the pyramid levels if requested **************/

    int pyr_passes = 1;
    if (multipass && disp_pyramid > 0)
	pyr_passes = disp_pyramid;

    for (int pyr_pass = pyr_passes; pyr_pass > stop_pyramid; pyr_pass--) {


//TODO Looks like the following is enough
/*
/////////////////////////////////////////////////////////////////////////////////////////
	if (multipass && pyr_pass > 1) 
	   disp_zoom = 2;		// Only a factor of 2 each time

        // Get the Left and Right image for the current pyramid level. It's one
        // pyramid level down from current pyr_pass
        p->left_img  = mipmapL[pyr_pass-1][pyr_pass-1];
        p->right_img = mipmapR[pyr_pass-1][pyr_pass-1];

        // Get corresponding Left and Right tiling maps
        downsampleTiling(orig_left_tile, p->left_tile, pyr_pass-1);
        downsampleTiling(orig_right_tile, p->right_tile, pyr_pass-1);

        // Update the "position" of the Left and Right image w.r.t the images
        // container
        p->pyr = pyr_pass - 1;
/////////////////////////////////////////////////////////////////////////////////////////
*/

	if (multipass && pyr_pass > 1) {

           // Get the Left and Right image for the current pyramid level 
           // It's one pyramid level down
           p->left_img  = mipmapL[pyr_pass-1][pyr_pass-1];
           p->right_img = mipmapR[pyr_pass-1][pyr_pass-1];

           // Compute the Left and Right tiling map at the current pyramid level
           downsampleTiling(orig_left_tile, p->left_tile, pyr_pass-1);
           downsampleTiling(orig_right_tile, p->right_tile, pyr_pass-1);

	   disp_zoom = 2;		// Only a factor of 2 each time
        }
        else {

           // This is full resolution Left and Right images
           p->left_img  = mipmapL[0][0];
           p->right_img = mipmapR[0][0];

           // Compute the Left and Right tiling map 
           p->left_tile  = orig_left_tile;
           p->right_tile = orig_right_tile;

        }

        // Update the "position" of the Left and Right image w.r.t the images
        // container
        p->pyr = pyr_pass - 1;




	if (multipass && pyr_pass == 1)
	    zvmessage("Computing last pyramid pass", "");
 

        /**************** Loop through the image *****************/

#pragma omp parallel for schedule(dynamic) if (omp_on)
        for (int j = 0; j < p->left_img->getNL(); j++) {
            if (j % 100 == 0 && j != 0) {
               snprintf(msg, msgLen, "line %d, gathered %d tiepoints (%f%% coverage)",
                       j, p->npts,
                       (((float)p->npts) / (j*p->left_img->getNS())) * 100.0 );
	       zvmessage(msg, "");
	    }

	    int jj = j / disp_zoom;
	    int jj_rem = j % disp_zoom; // Remainders used to offset results
	    int left_line = j;

            // Out of bound could happen depending on pyr level vs image size.
            if (jj >= p->in_disp_line->getNL())
               continue;

	    for (int i = 0; i < p->left_img->getNS(); i++) {

	        // Get coordinates in disparity image

	        int ii = i / disp_zoom;
	        int ii_rem = i % disp_zoom;
	        int left_samp = i;

                // Out of bound could happen depending on pyr level vs
                // image size.
                if (ii >= p->in_disp_line->getNS())
                   continue;

                if ((int)(p->in_disp_line->get(jj,ii) + 0.5) == 0 ||
                    (int)(p->in_disp_samp->get(jj,ii) + 0.5) == 0) 
		    continue;			// no input disparity
                

	        // Do the initial correlation

	        // -1 makes it 0-based; +.5 rounds to int
	        double right_line = (p->in_disp_line->get(jj,ii)-1)*disp_zoom;
	        double right_samp = (p->in_disp_samp->get(jj,ii)-1)*disp_zoom;

//	        // The value actually represents the upper-left pixel in the
//	        // zoom box, once it is zoomed back up.
//	        right_line = right_line + jj_rem;
//	        right_samp = right_samp + ii_rem;

		// Seeds come from the input coefficients file if given,
		// or the feedback mechanism if other than the first pass
		double *seed_coefs_p  = NULL;
		double seed_coefs[6];
		if (p->use_input_coefs ||
			(p->feedback_coefs && (pyr_pass != pyr_passes))) {
		   seed_coefs[0] = p->in_coef_a->get(jj,ii);
		   seed_coefs[1] = p->in_coef_b->get(jj,ii);
		   seed_coefs[2] = p->in_coef_d->get(jj,ii);
		   seed_coefs[3] = p->in_coef_e->get(jj,ii);
		   seed_coefs[4] = p->in_coef_g->get(jj,ii);
		   seed_coefs[5] = p->in_coef_h->get(jj,ii);
		   seed_coefs_p = seed_coefs;

                   xform->setCoefs(xform_buf, 
                      seed_coefs[0], seed_coefs[1], 0.0,
                      seed_coefs[2], seed_coefs[3], 0.0,
                      seed_coefs[4], seed_coefs[5]);

		}
                else {
                   xform->setCoefs(xform_buf, 
                      p->initial_samp_coef[0], p->initial_samp_coef[1], p->initial_samp_coef[2],
                      p->initial_line_coef[0], p->initial_line_coef[1], p->initial_line_coef[2],
                      p->initial_samp_coef[3], p->initial_line_coef[3]);
                }
        
                // The values right_samp/line actually represents the upper-left
                // pixel in the zoom box, once it is zoomed back up.
                // Need to add the offset to get the correct Right pixel location.
                right_samp += xform->computeX(xform_buf, ii_rem, jj_rem);
                right_line += xform->computeY(xform_buf, ii_rem, jj_rem);


	        int status = do_correlation(left_line, left_samp, mode,
		       TRUE, right_line, right_samp, seed_coefs_p, p);


	        if (status < 0) {	// bad correl, try again in gore code
		    if (status == -2) {	// failed due to quality setting
		        // Quality shouldn't be negative here, but just in case
		        if (p->qual->get(left_line,left_samp) > 0)
		            p->qual->set(left_line,left_samp,
					- p->qual->get(left_line,left_samp));
		    }
		    else			// failed for other reason
		        p->qual->set(left_line,left_samp, FAILED);
	        }
	    }
        }

        /***********CLEAN UP MISSING POINTS IN ENTIRE IMAGE AREA************/

// Gore passes are simple without parallelization: loop over lines, loop over
// samps, and look at all neighbors.  With parallelization, however, this is
// complicated because we have to get the neighbors above/right computed before
// we can compute ourselves.  This is inherently non-parallel.
// However, we can exploit a trick: go diagonally.  We really only need the
// neighbors above +1 in the sample direction... so if we're 2 behind that
// row, we have all the info we need.  We thus proceed diagonally, at a 2:1
// pitch.  We cannot parallize the outer loop (which diagonal we're on) but
// we CAN parallelize the inner loop (which item within the diagonal).  Looking
// at it graphically, we have:
//  0  1  2  3  4  5  6  7
//  2  3  4  5  6  7  8  9
//  4  5  6  7  8  9 10 11
//  6  7  8  9 10 11 12 13
//  8  9 10 11 12 13 14 15
// where each number shows the pixels process on a given diagonal (first
// diag, second diag, etc).  Turns out the math is slightly different based
// on whether the number of columns is even or odd, but that only affects
// start_samp.
// Within a diagonal, we get the starting line and samp (highest one in the
// diagram), then go down one and left 2 to get the next... this is what is
// done in parallel.
// All this gives us the same algorithm we had before parallelization... which
// greatly eases testing.

        if (do_gores) {

	    // Replace the quality with gore_quality so we don't do too much

	    p->min_quality = gore_quality;

	    zvmessage("Filling in gores", "");

	    int nl = p->left_img->getNL();
	    int ns = p->left_img->getNS();

	    int gore_passes = 0;
	    do {
	        fwd_count = 0;
		rev_count = 0;
		total_count = 0;

		// integer math with truncation is assumed here
		int num_diags = ns + (nl*2) - 2;
		int max_in_diag = (ns+1)/2;

		// Outer loop, not parallelized

		for (int j=0; j < num_diags; j++) {
		    int start_line = 0;
		    int start_samp = j;
		    if (j >= ns) {
			start_line = (j-ns+2)/2;
			start_samp = ns-2+(j-ns%2)%2;
		    }

		    // Inner loop, parallelized
#pragma omp parallel for schedule(dynamic) if (omp_on) reduction(+:fwd_count) reduction(+:total_count)
		    for (int i=0; i < max_in_diag; i++) {
			int line = start_line + i;
			int samp = start_samp - (i*2);
			if (line >= nl || samp < 0)
			    continue;		// off the image

			total_count++;
		        int left_line = line;
		        int left_samp = samp;

		        // Re-try all points, even those that failed last pass
		        if (p->qual->get(left_line,left_samp) > GOOD_CORR)
			    continue;

		        int status = do_correlation(left_line, left_samp, mode,
					FALSE, 0, 0, NULL, p);

		        if (status == 0)		// good point
			    fwd_count += 1;
		        else if (status < 0) {
			    if (status == -2) {	// failed due to quality setting
			        if (p->qual->get(left_line,left_samp) > 0)
			            p->qual->set(left_line,left_samp,
					   - p->qual->get(left_line,left_samp));
			    }
			    else		// failed for other reason
			        p->qual->set(left_line,left_samp, FAILED);
			    continue;
		        }
		        // status==1: already done, ignore
		    }
	        }
	        snprintf(msg, msgLen, "Fwd gore pass %d: found %d new (%f%% total coverage)",
		    gore_passes+1, fwd_count,
		    (((float)p->npts) / (total_count) * 100.0 ));
	        zvmessage(msg, "");

		// The code block below duplicates that above.  But it's
		// short enough, this is easier than doing complicated
		// loop control

		if (gore_reverse) {
	            rev_count = 0;
		    total_count = 0;

		    // Outer loop, not parallelized

		    for (int j=0; j < num_diags; j++) {
			int start_line = 0;
			int start_samp = j;
			if (j >= ns) {
			    start_line = (j-ns+2)/2;
			    start_samp = ns-2+(j-ns%2)%2;
			}

			// Inner loop, parallelized
#pragma omp parallel for schedule(dynamic) if (omp_on) reduction(+:rev_count) reduction(+:total_count)
		        for (int i=0; i < max_in_diag; i++) {
			    int line = start_line + i;
			    int samp = start_samp - (i*2);
			    if (line >= nl || samp < 0)
				continue;		// off the image

			    total_count++;

			    // This is what reverses it...
		            int left_line = nl - line - 1;
		            int left_samp = ns - samp - 1;

		            // Re-try all pts, even those that failed last pass
		            if (p->qual->get(left_line,left_samp) > GOOD_CORR)
			        continue;

		            int status = do_correlation(left_line, left_samp, mode,
					FALSE, 0, 0, NULL, p);

		            if (status == 0)		// good point
			        rev_count += 1;
		            else if (status < 0) {
			        if (status == -2) {	// failed due to qual
			            if (p->qual->get(left_line,left_samp) > 0)
			                p->qual->set(left_line,left_samp,
					   - p->qual->get(left_line,left_samp));
			        }
			        else		// failed for other reason
			            p->qual->set(left_line,left_samp, FAILED);
			        continue;
		            }
		            // status==1: already done, ignore
		        }
	            }
	            snprintf(msg, msgLen, "Rev gore pass %d: found %d new (%f%% total coverage)",
		        gore_passes+1, rev_count,
		        (((float)p->npts) / total_count) * 100.0);
	            zvmessage(msg, "");
		}

	        gore_passes++;

	    } while (fwd_count+rev_count > 0 &&
		((gore_passes < max_gore_passes) || (max_gore_passes == 0)));

        }			// end of gore loop

        snprintf(msg, msgLen, "Gathered %d tiepoints total (%f%% coverage)",
		p->npts,
		(((float)p->npts) / (p->left_img->getNL()*p->left_img->getNS()))
			* 100.0);
        zvmessage(msg, "");

	// Now... if we're doing a multipass, reshuffle the results and
	// clean up for next time
	if (multipass && pyr_pass > (stop_pyramid+1)) {

	    // The input coefs file is no longer valid... we're either going
	    // to wipe it out with feedback, or if not it's the wrong size
	    // for further passes.  So we turn off input coefs after the
	    // first pass.
	    p->use_input_coefs = FALSE;

	    // Copy results back to correlation array for new starting point
	    // We have to add 1 because the in_disp is 1-based but the
	    // line/samp_mask's are 0-based...
	    // We also copy the coefs array from output back to input, if
	    // feedback_coefs is on.

	    // Realloc to the next size up
	    p->in_disp_line->alloc(p->left_img->getNL(), p->left_img->getNS());
	    p->in_disp_samp->alloc(p->left_img->getNL(), p->left_img->getNS());
	    if (p->feedback_coefs) {
		if (p->in_coef_a == NULL)
		    p->in_coef_a = new SimpleImage<double>();
		if (p->in_coef_b == NULL)
		    p->in_coef_b = new SimpleImage<double>();
		if (p->in_coef_d == NULL)
		    p->in_coef_d = new SimpleImage<double>();
		if (p->in_coef_e == NULL)
		    p->in_coef_e = new SimpleImage<double>();
		if (p->in_coef_g == NULL)
		    p->in_coef_g = new SimpleImage<double>();
		if (p->in_coef_h == NULL)
		    p->in_coef_h = new SimpleImage<double>();
		p->in_coef_a->alloc(p->left_img->getNL(), p->left_img->getNS());
		p->in_coef_b->alloc(p->left_img->getNL(), p->left_img->getNS());
		p->in_coef_d->alloc(p->left_img->getNL(), p->left_img->getNS());
		p->in_coef_e->alloc(p->left_img->getNL(), p->left_img->getNS());
		p->in_coef_g->alloc(p->left_img->getNL(), p->left_img->getNS());
		p->in_coef_h->alloc(p->left_img->getNL(), p->left_img->getNS());
		p->in_coef_a->zero();
		p->in_coef_b->zero();
		p->in_coef_d->zero();
		p->in_coef_e->zero();
		p->in_coef_g->zero();
		p->in_coef_h->zero();
	    }

	    for (int j=0; j < p->left_img->getNL(); j++) {
		for (int i=0; i < p->left_img->getNS(); i++) {
	            if (p->qual->get(j,i) <= GOOD_CORR) {
		        p->in_disp_line->set(j,i, 0.0);
		        p->in_disp_samp->set(j,i, 0.0);
		    }
		    else {
		        p->in_disp_line->set(j,i, p->line_mask->get(j,i) + 1.0);
		        p->in_disp_samp->set(j,i, p->samp_mask->get(j,i) + 1.0);
			if (p->feedback_coefs) {
			    p->in_coef_a->set(j,i, p->out_coef_a->get(j,i));
			    p->in_coef_b->set(j,i, p->out_coef_b->get(j,i));
			    p->in_coef_d->set(j,i, p->out_coef_d->get(j,i));
			    p->in_coef_e->set(j,i, p->out_coef_e->get(j,i));
			    p->in_coef_g->set(j,i, p->out_coef_g->get(j,i));
			    p->in_coef_h->set(j,i, p->out_coef_h->get(j,i));
			}
		    }
		}
	    }

	    /* clear correlation quality array and disparities */

	    p->line_mask->zero();
	    p->samp_mask->zero();

	    for (int j=0; j < p->qual->getNL(); j++) {
	        for (int i=0; i < p->qual->getNS(); i++) {
	            p->qual->set(j,i, EMPTY);
	        }
	    }

	    // Reset count for next pass...
	    p->npts=0;
	}
 
    }				// end multipass loop



    // If applying the local coherence filter
    if (p->statFilter) {

        zvmessage("Applying local coherence filter","");

        int statFilterExtent, statFilterThresholdN;
        float statFilterScalerThreshold;

        zvp("STAT_EXTENT", &statFilterExtent, &count);
        zvp("STAT_STHRESHOLD", &statFilterScalerThreshold, &count);
        zvp("STAT_NTHRESHOLD", &statFilterThresholdN, &count);


        SimpleImage<short> *maskCoherence;

        localCoherenceFilter(p->qual,
                             p->samp_mask,
                             p->line_mask,
                             p->out_coef_a,
                             p->out_coef_b,
                             p->out_coef_d,
                             p->out_coef_e,
                             nullptr, //p->out_coef_g, //Not used for now
                             nullptr, //p->out_coef_h,
                             p->nlw, 
                             p->nsw,
                             statFilterExtent,
                             statFilterScalerThreshold,
                             statFilterThresholdN,
                             omp_on, 
                             maskCoherence); //output

        applyLocalCoherenceFilter(p->qual, maskCoherence, (short)FAILED);
        applyLocalCoherenceFilter(p->samp_mask, maskCoherence);
        applyLocalCoherenceFilter(p->line_mask, maskCoherence);

        applyLocalCoherenceFilter(p->out_coef_a, maskCoherence);
        applyLocalCoherenceFilter(p->out_coef_b, maskCoherence);
        applyLocalCoherenceFilter(p->out_coef_d, maskCoherence);
        applyLocalCoherenceFilter(p->out_coef_e, maskCoherence);
        applyLocalCoherenceFilter(p->out_coef_g, maskCoherence);
        applyLocalCoherenceFilter(p->out_coef_h, maskCoherence);

        if (maskCoherence != nullptr) {
           maskCoherence->free();
           delete maskCoherence;
        }

    }

    // Count the number of valid correlations
    short int * pt = p->qual->linePtr(0);
    int nbCorrValid = std::count_if(pt, pt+nll*nsl, [](short i) {return i > GOOD_CORR;});
    double overlapCheck = (double) nbCorrValid / (nll*nsl) * 100.0; 

    // Compute average scaling between the two images.
    // This is approximate scaling between the two images. Really approximate
    // Only meant for output label. 
    double * ptr = p->out_coef_a->linePtr(0);
    double avg_a = std::accumulate(ptr, ptr+nll*nsl, 0.0) / nbCorrValid;
    ptr = p->out_coef_b->linePtr(0);
    double avg_b = std::accumulate(ptr, ptr+nll*nsl, 0.0) / nbCorrValid;
    ptr = p->out_coef_d->linePtr(0);
    double avg_d = std::accumulate(ptr, ptr+nll*nsl, 0.0) / nbCorrValid;
    ptr = p->out_coef_e->linePtr(0);
    double avg_e = std::accumulate(ptr, ptr+nll*nsl, 0.0) / nbCorrValid;

    // Average scaling defaults to the one form IN_DISP if available
    double avgScaling = correlationAvgScale;
    double det = avg_a * avg_e - avg_d * avg_b;

    if (det > 0)
       avgScaling = sqrt(det);
    else 
       zvmessage("Determinant of average transform negative. Suspicious!","");
    


    /****************WRITE DATA FILES************************************/



    int unit_inp=0, unit_out;
    char filename_inp[PIG_MAX_FILENAME_SIZE+1];
    PigMission *m = NULL;

    // Create PIG mission object based on the first input file
    status = zvpone("INP",filename_inp,1,sizeof(filename_inp));
    if (status) //success
        m = PigMission::getMissionObject(filename_inp, &unit_inp);
    if (m)  
        zvclose(unit_inp,"CLOS_ACT","FREE",NULL);


    /* write line offset */

    band = 1;
    int num_out_files;
    zvpcnt("OUT", &num_out_files);
    int num_bands = 3 - num_out_files;		// 2 files == 1 band each

    zvunit(&unit_out, "OUT", 1, NULL);
    zvopen(unit_out, "OP", "WRITE", "U_FORMAT", "DOUB", "O_FORMAT", "REAL",
               "U_NS", p->left_img->getNS(), "U_NL", p->left_img->getNL(),
		"OPEN_ACT", "AS", "U_NB", num_bands, "U_ORG", "BSQ", NULL);
    zvplabel(unit_out, 0, 1);

    // Write output label
    zvpcnt("INP", &nids);
    PigFileModel *file_models[MAX_INPUTS+2];
    for (int i = 0; i < MAX_INPUTS + 2; i++)
        file_models[i] = NULL; 
    if (m)  {// if we were able to create Mission object
        for (int i = 0; i < nids; i++)
            file_models[i] = m->createFileModel("", unit_in[i]);
        file_models[nids] = m->createFileModel("", disp_unit);
	nids++;
	if (disp_unit2 != -1) {
	    file_models[nids] = m->createFileModel("", disp_unit2);
	    nids++;
	}

        PigLabelModel *labelModel_0 = NULL;
	labelModel_0 = m->createLabelModel(unit_out);

	if (labelModel_0) {
	    if (num_bands == 2) { // single two-banded image
	        labelModel_0->setDisparity(file_models, file_models[1], nids, 
                                           "DISPARITY_MAP");
                labelModel_0->setDisparityExtra(nbCorrValid, 
                                                avgScaling,
                                                -1,  // Keep overlap value from input disparity
                                                stop_pyramid);
            }
	    else  // two 1-banded images
	        labelModel_0->setDisparity(file_models, file_models[1], nids, 
                                           "DISPARITY_LINE_MAP");
	}
    }

    for (int j=0; j < p->left_img->getNL(); j++) {
	for (int i=0; i < p->left_img->getNS(); i++) {
	    if (p->qual->get(j,i) <= GOOD_CORR)
		p->line_mask->set(j,i, 0.);
	    else
		p->line_mask->addto(j,i, 1.0);		// make coords 1-based
	}
	zvwrit(unit_out, p->line_mask->linePtr(j), "LINE", j+1,
						   "BAND", band, NULL);
    }

    /* write samp offset */

    if (num_out_files == 1) {		// 2 bands in one file
	band = 2;
    }
    else {				// one band in each of 2 files
	zvclose(unit_out, "CLOS_ACT", "FREE", NULL);
	zvunit(&unit_out, "OUT", 2, NULL);
	zvopen(unit_out, "OP", "WRITE", "U_FORMAT", "DOUB", "O_FORMAT", "REAL",
               "U_NS", p->left_img->getNS(), "U_NL", p->left_img->getNL(),
		"OPEN_ACT", "AS", "U_NB", num_bands, "U_ORG", "BSQ", NULL);
	zvplabel(unit_out, 0, 1);

	// Write output label for the second 1-banded file
	if (m) {
	    PigLabelModel *labelModel_1 = NULL;
	    labelModel_1 = m->createLabelModel(unit_out);
	    if (labelModel_1)
	        labelModel_1->setDisparity(file_models, file_models[1], nids, 
                                           "DISPARITY_SAMPLE_MAP");
	}
    }

    for (int j=0; j < p->left_img->getNL(); j++) {
	for (int i=0; i < p->left_img->getNS(); i++) {
	    if (p->qual->get(j,i) <= GOOD_CORR)
		p->samp_mask->set(j,i, 0.);
	    else
		p->samp_mask->addto(j,i, 1.0);		// make coords 1-based
	}
	zvwrit(unit_out, p->samp_mask->linePtr(j), "LINE", j+1,
						   "BAND", band, NULL);
    }
    zvclose(unit_out, "CLOS_ACT", "FREE", NULL);

    /* write mask */

    zvpcnt("MASK", &count);
    if (count > 0) {				// only if desired

	unsigned char bbuf[MAX_NS];
	char name[256];

	zvp("MASK", name, &count);
	zvunit(&unit_out, "MASK", 3, "U_NAME", name, NULL);
	zvopen(unit_out, "OP", "WRITE", "U_FORMAT", "BYTE", "O_FORMAT", "BYTE",
               "U_NS", p->left_img->getNS(), "U_NL", p->left_img->getNL(),
		"OPEN_ACT", "AS", "U_NB", 1, "U_ORG", "BSQ", NULL);
	zvplabel(unit_out, 0, 1);
        if (m) {
            PigLabelModel *labelModel = m->createLabelModel(unit_out);
            labelModel->setIdentification(NULL);
            labelModel->setDerivedImageType("CORR_MASK");
        }

	for (int j=0; j < p->left_img->getNL(); j++) {
	    for (int i=0; i < p->left_img->getNS(); i++) {
		if (p->qual->get(j,i) == EMPTY) bbuf[i]=0;	// unreachable
		else if (p->qual->get(j,i) <= GOOD_CORR) bbuf[i]=255;//bad point
		else bbuf[i] = 128;				// good point
	    }
	    zvwrit(unit_out, bbuf, "LINE", j+1, NULL);
	}
	zvclose(unit_out, "CLOS_ACT", "FREE", NULL);
    }

    /* write quality image */

    zvpcnt("OUT_QUALITY", &count);
    if (count > 0) {				// only if desired

	float fbuf[MAX_NS];
	char name[256];

	zvp("OUT_QUALITY", name, &count);
	zvunit(&unit_out, "OUT_QUALITY", 1, "U_NAME", name, NULL);
	zvopen(unit_out, "OP", "WRITE", "U_FORMAT", "REAL", "O_FORMAT", "REAL",
               "U_NS", p->left_img->getNS(), "U_NL", p->left_img->getNL(),
		"OPEN_ACT", "AS", "U_NB", 1, "U_ORG", "BSQ", NULL);
	zvplabel(unit_out, 0, 1);
        if (m) {
            PigLabelModel *labelModel = m->createLabelModel(unit_out);
            labelModel->setIdentification(NULL);
            labelModel->setDerivedImageType("QUALITY");
        }

	for (int j=0; j < p->left_img->getNL(); j++) {
	    for (int i=0; i < p->left_img->getNS(); i++) {
		int q = p->qual->get(j,i);
		if (q == EMPTY)				// unreachable
		    fbuf[i] = 0.0;
		else if (q == FAILED)			// corr. failure
		    fbuf[i] = 0.0;
		else if (q <= GOOD_CORR)		// didn't meet quality
		    fbuf[i] = - q / QUAL_FACTOR;
		else					// good point
		    fbuf[i] = q / QUAL_FACTOR;
	    }
	    zvwrit(unit_out, fbuf, "LINE", j+1, NULL);
	}
	zvclose(unit_out, "CLOS_ACT", "FREE", NULL);
    }

    // Write coefficients file

    if (p->save_output_coefs) {

	char name[256];

	zvp("OUT_COEFS", name, &count);
	zvunit(&unit_out, "OUT_COEFS", 1, "U_NAME", name, NULL);
	zvopen(unit_out, "OP", "WRITE", "U_FORMAT", "DOUB", "O_FORMAT", "REAL",
               "U_NS", p->left_img->getNS(), "U_NL", p->left_img->getNL(),
		"OPEN_ACT", "AS", "U_NB", 6, "U_ORG", "BSQ", NULL);
	zvplabel(unit_out, 0, 1);
        if (m) {
            PigLabelModel *labelModel = m->createLabelModel(unit_out);
            labelModel->setIdentification(NULL);
            labelModel->setDerivedImageType("COEFFICIENTS");
        }




	for (int band=0; band < 6; band++) {

	    SimpleImage<double> *coef = NULL;
	    switch (band) {
		case 0: coef = p->out_coef_a; break;
		case 1: coef = p->out_coef_b; break;
		case 2: coef = p->out_coef_d; break;
		case 3: coef = p->out_coef_e; break;
		case 4: coef = p->out_coef_g; break;
		case 5: coef = p->out_coef_h; break;
	    }
	    for (int j=0; j < p->left_img->getNL(); j++) {
		zvwrit(unit_out, coef->linePtr(j),
			"LINE", j+1, "BAND", band+1, NULL);
	    }
	}
	zvclose(unit_out, "CLOS_ACT", "FREE", NULL);
    }
}


////////////////////////////////////////////////////////////////////////
// Find the neighboring point with the best quality.  Returns the
// quality of that maximum point, or -1 if no neighbors are valid.
// The line and samp of the max point are also returned in max_qual_line
// and max_qual_samp.
////////////////////////////////////////////////////////////////////////

static int get_best_neighbor(SimpleImage<short int> *qual, int line, int samp,
	int &max_qual_line, int &max_qual_samp)
{

    int max_qual = -1;

    // If we're on the edge, don't bother with a neighbor.  There won't be
    // enough correlation window anyway.  Simpler and faster than checking
    // each case.

    if (line <= 0 || samp <= 0 || line >= (qual->getNL()-1) ||
				  samp >= (qual->getNS()-1))
	return max_qual;
    if (line <= 0 || samp <= 0 || line >= MAX_NL-1 ||
				  samp >= MAX_NS-1)
	return max_qual;

    if (qual->get(line+1,samp) > max_qual) {
	max_qual_line = line+1;
	max_qual_samp = samp;
	max_qual = qual->get(max_qual_line,max_qual_samp);
    }
    if (qual->get(line-1,samp) > max_qual) {
	max_qual_line = line-1;
	max_qual_samp = samp;
	max_qual = qual->get(max_qual_line,max_qual_samp);
    }
    if (qual->get(line,samp-1) > max_qual) {
	max_qual_line = line;
	max_qual_samp = samp-1;
	max_qual = qual->get(max_qual_line,max_qual_samp);
    }
    if (qual->get(line,samp+1) > max_qual) {
	max_qual_line = line;
	max_qual_samp = samp+1;
	max_qual = qual->get(max_qual_line,max_qual_samp);
    }
    if (qual->get(line+1,samp+1) > max_qual) {
	max_qual_line = line+1;
	max_qual_samp = samp+1;
	max_qual = qual->get(max_qual_line,max_qual_samp);
    }
    if (qual->get(line-1,samp+1) > max_qual) {
	max_qual_line = line-1;
	max_qual_samp = samp+1;
	max_qual = qual->get(max_qual_line,max_qual_samp);
    }
    if (qual->get(line+1,samp-1) > max_qual) {
	max_qual_line = line+1;
	max_qual_samp = samp-1;
	max_qual = qual->get(max_qual_line,max_qual_samp);
    }
    if (qual->get(line-1,samp-1) > max_qual) {
	max_qual_line = line-1;
	max_qual_samp = samp+1;
	max_qual = qual->get(max_qual_line,max_qual_samp);
    }

    return max_qual;
}


////////////////////////////////////////////////////////////////////////
// Do a correlation for a single point.  This includes finding the best
// neighbor as a starting point, and updating the mask and quality
// arrays upon success.  Certain things are slightly different if we're
// correlating a seed point (in which case seed_line and seed_samp are
// the initial values for the right line/samp... if seed is false, these
// parameters are ignored).
//
// If p->check is non-0, a reverse correlation (right->left) is performed on
// the result.  If that correlation fails, the point is rejected.  Then
// the result is verified to make sure the new left result falls within
// "check" pixels of the original left location... if not, the point is also
// rejected.  The check is only performed if the quality is <= check_quality.
// The theory is if the quality is good enough, a reverse check is not needed.
//
// Return values:
// -4 = reverse corelation not within "check"
// -3 = reverse correlation failed
// -2 = correlation quality below minimum, or reverse below minimum
// -1 = bad correlation
//  0 = success
//  1 = point skipped (already done or no neighbors)
//
//!!!! TBD: The find-a-neighbor stuff really should go in the caller...
//!!!! it would be cleaner that way.  Provide a seed, period.
////////////////////////////////////////////////////////////////////////

int do_correlation(int left_line, int left_samp, int mode,
		int seed, double seed_line, double seed_samp, double *seed_coef,
		CorrelParams *p)
{
    int max_qual, max_qual_line, max_qual_samp;
    int right_line, right_samp;
    double right_liner, right_sampr;
    const size_t msgLen = 256;
    char msg[msgLen];
    short int first_quality;

    SimpleImage<double> * left_area;
    SimpleImage<double> * right_area;

    // Params for gruen

    double line_offset, samp_offset;
    double line_offset2, samp_offset2;
    double delta_line=0.0, delta_samp=0.0;
    double line_coef[4], samp_coef[4];
    double line_coef_limits[3][2];
    double samp_coef_limits[3][2];
    double rev_line_coef[4], rev_samp_coef[4];
    double quality;
    double percent = 100.;
    int limits = 10000;

    // The rest of the gruen params are not used
    static SimpleImage<double> *correl_img;
    double line_temp[3], samp_temp[3];


    // Get local handles of some input parameters
    int nlw_left = p->nlw;
    int nsw_left = p->nsw;
    int nlw_right = p->nlw2;
    int nsw_right = p->nsw2;
    SimpleImage<double> * left_img = p->left_img;
    SimpleImage<double> * right_img = p->right_img;

    // Skip this point if it's already been done

    if (p->qual->get(left_line,left_samp) > GOOD_CORR)
	return 1;

    int left_nl = left_img->getNL();
    int left_ns = left_img->getNS();
    int right_nl = right_img->getNL();
    int right_ns = right_img->getNS();


    /* set initial coefficient values */
    // This used to be line = 0 1 0 0, samp = 1 0 0 0.  Now, we use the
    // initial_*_coef arrays, seed_coefs, or in_coef images to get us started
    // in the right place.

    // Default case
    for (int i=0; i<4; i++) {
       line_coef[i] = p->initial_line_coef[i];
       samp_coef[i] = p->initial_samp_coef[i];
    }

    if (seed) {				// seed points skip neighbor checks
       right_line = (int)seed_line;
       right_samp = (int)seed_samp;
       right_liner = seed_line;
       right_sampr = seed_samp;

       // Remember that samp_coef is [a,b,c,g] and line_coef is [d,e,f,h].
       if (seed_coef != NULL) {
          samp_coef[0] = seed_coef[0];	// a
          samp_coef[1] = seed_coef[1];	// b
          samp_coef[2] = 0.0;		// c
          samp_coef[3] = seed_coef[4];	// g
          line_coef[0] = seed_coef[2];	// d
          line_coef[1] = seed_coef[3];	// e
          line_coef[2] = 0.0;		// f
          line_coef[3] = seed_coef[5];	// h
       }
    }
    else {

	// Find the neighbor with the best correlation to use as starting point
	max_qual = get_best_neighbor(p->qual, left_line, left_samp,
		max_qual_line, max_qual_samp);

	if (max_qual == -1)
	    return 1;			// no neighbors, skip it

	right_liner = p->line_mask->get(max_qual_line,max_qual_samp);
	right_sampr = p->samp_mask->get(max_qual_line,max_qual_samp);

	delta_line = left_line - max_qual_line;
	delta_samp = left_samp - max_qual_samp;

	// Get coefs from neighbor too (they don't need to be adjusted).
	// We actually get it from out_coef because that's where the
	// neighbor's data has been put.
	// If no feedback coef, the defaults transform parameters are used, 
	// which may contain X/Yscale and rotation.

	if (p->feedback_coefs) {
	    samp_coef[0] = p->out_coef_a->get(max_qual_line,max_qual_samp);
	    samp_coef[1] = p->out_coef_b->get(max_qual_line,max_qual_samp);
	    samp_coef[2] = 0.0;
	    samp_coef[3] = p->out_coef_g->get(max_qual_line,max_qual_samp);
	    line_coef[0] = p->out_coef_d->get(max_qual_line,max_qual_samp);
	    line_coef[1] = p->out_coef_e->get(max_qual_line,max_qual_samp);
	    line_coef[2] = 0.0;
	    line_coef[3] = p->out_coef_h->get(max_qual_line,max_qual_samp);
        }

        // Traditionally we just add/sub one to the coordinate, but that's not 
        // right if there are differential zooms in effect.  This is not a 
        // critical code section so we always use an amoeba8 transform for 
        // simplicity.
	
        GruenTransform *xform = GruenTransform::create(Amoeba8);
        double xform_buf[8];
        xform->setCoefs(xform_buf,
                        samp_coef[0], samp_coef[1], samp_coef[2],
                        line_coef[0], line_coef[1], line_coef[2],
                        samp_coef[3], line_coef[3]);
        double right_delta_samp = xform->computeX(xform_buf,
                                                delta_samp, delta_line);
        double right_delta_line = xform->computeY(xform_buf,
						delta_samp, delta_line);
       
        // Now, delta contains offset in the right image
        delta_samp = right_delta_samp;
        delta_line = right_delta_line;

        // Rounding to the closest integer pixel for template patch central
        // pixel
	right_line = (int)(right_liner + right_delta_line + 0.5);	
	right_samp = (int)(right_sampr + right_delta_samp + 0.5);	

    }


  // Adjust translation terms
  // if seed==0, need to add delta. If seed!=0 delta=0. So no need for
  // a if statement on seed value.
  line_coef[2] = right_liner + delta_line - right_line; 
  samp_coef[2] = right_sampr + delta_samp - right_samp; 




  // Now that we have the initial transform coefficients, set the limit on 
  // these coefficients.
  // There are two options. 1) same limit applied to all pixel (global) and
  // 2) limit defined locally from the coefficients
  if (p->use_limits) {
     if (!p->localCoefLimit) {
        samp_coef_limits[0][0] = p->samp_coef_limits[0][0];
        samp_coef_limits[0][1] = p->samp_coef_limits[0][1];
        samp_coef_limits[1][0] = p->samp_coef_limits[1][0];
        samp_coef_limits[1][1] = p->samp_coef_limits[1][1];
        samp_coef_limits[2][0] = p->samp_coef_limits[2][0];
        samp_coef_limits[2][1] = p->samp_coef_limits[2][1];

        line_coef_limits[0][0] = p->line_coef_limits[0][0];
        line_coef_limits[0][1] = p->line_coef_limits[0][1];
        line_coef_limits[1][0] = p->line_coef_limits[1][0];
        line_coef_limits[1][1] = p->line_coef_limits[1][1];
        line_coef_limits[2][0] = p->line_coef_limits[2][0];
        line_coef_limits[2][1] = p->line_coef_limits[2][1];
     }
     else {
        double bracket = abs(samp_coef[0] * p->coefLimits[0]);
        samp_coef_limits[0][0] =  samp_coef[0] - bracket; 
        samp_coef_limits[0][1] =  samp_coef[0] + bracket;
        //See comment in global limit part (in main marscor3) regarding these
        //limits based on gruen default values
        if ((p->samp_coef_limits[0][1] - p->samp_coef_limits[0][0]) < 4 * 0.07) {
           p->samp_coef_limits[0][0] -= 2*0.07;
           p->samp_coef_limits[0][1] += 2*0.07;
        } 

        bracket = abs(samp_coef[1] * p->coefLimits[1]);
        samp_coef_limits[1][0] =  samp_coef[1] - bracket; 
        samp_coef_limits[1][1] =  samp_coef[1] + bracket;
        if ((p->samp_coef_limits[1][1] - p->samp_coef_limits[1][0]) < 4 * 0.07) {
           p->samp_coef_limits[1][0] -= 2*0.07;
           p->samp_coef_limits[1][1] += 2*0.07;
        } 
 
        bracket = abs(samp_coef[2] * p->coefLimits[2]);
        samp_coef_limits[2][0] =  samp_coef[3] - bracket; 
        samp_coef_limits[2][1] =  samp_coef[3] + bracket;
        if ((p->samp_coef_limits[2][1] - p->samp_coef_limits[2][0]) < 4 * 0.01) {
           p->samp_coef_limits[2][0] -= 2*0.01;
           p->samp_coef_limits[2][1] += 2*0.01;
        } 

        bracket = abs(line_coef[0] * p->coefLimits[3]);
        line_coef_limits[0][0] =  line_coef[0] - bracket; 
        line_coef_limits[0][1] =  line_coef[0] + bracket;
        if ((p->line_coef_limits[0][1] - p->line_coef_limits[0][0]) < 4 * 0.07) {
           p->line_coef_limits[0][0] -= 2*0.07;
           p->line_coef_limits[0][1] += 2*0.07;
        } 

        bracket = abs(line_coef[1] * p->coefLimits[4]);
        line_coef_limits[1][0] =  line_coef[1] - bracket; 
        line_coef_limits[1][1] =  line_coef[1] + bracket;
        if ((p->line_coef_limits[1][1] - p->line_coef_limits[1][0]) < 4 * 0.07) {
           p->line_coef_limits[1][0] -= 2*0.07;
           p->line_coef_limits[1][1] += 2*0.07;
        } 

        bracket = abs(line_coef[2] * p->coefLimits[5]);
        line_coef_limits[2][0] =  line_coef[3] - bracket; 
        line_coef_limits[2][1] =  line_coef[3] + bracket;
        if ((p->line_coef_limits[2][1] - p->line_coef_limits[2][0]) < 4 * 0.01) {
           p->line_coef_limits[2][0] -= 2*0.01;
           p->line_coef_limits[2][1] += 2*0.01;
        } 

     }
  } 

//TODO
//- Account for THRESHOLD
//- If decimation not worth it (THRESHOLD?) no ewa
//- Limit for x,y alignement.
  
  // The objective of the following section is to define:
  // [1] The size of the Left and Right patches to extract, 
  // [2] from which pre-downsampled images.
  // Ideally you'd want to extract a TEMPLATE patch around the current Left 
  // pixel from the input Left image and a SEARCH patch around the estimated 
  // matching pixel location from the input Right image. 
  // However, because of possibly a strong geometric transform between Left and
  // Right, of the image tiling, and of the trade off between matching quality 
  // and processing speed, there are several strategies to extract these patches
  // which are controlled by user parameters. 
  //
  // This is the goal of this section. Use chosen strategy to define [1] and [2]
  //
  // [1] To define the Left/Right patches, 2 variables needs to be defined (for 
  // each the Left and Right):
  // - The top-left corner of the patch (pixel location)
  // - The spread of the patch (its size) which may be different (increased) 
  //   from the TEMPLATE/SEARCH parameters because of various reasons (the whole
  //   purpose behind the following section)
  //
  // [2] The selection of which image the patch is to be extracted from, depends
  // on the geometric transform between Left and Right, the tiling, and (of 
  // course) some user parameters.
  //


  // Initialize the list of variable that will define the patches footprint.

  // Initialize the image index and image downsampling factor. 
  // The image index represents the level in the image pyramid (i.e., which 
  // downsampled image will be used) to extract the L or R patches.
  // The dowsampling factor corresponds to the actual downsampling corresponding
  // to the index. Downsampling factors are x1, x2, x4, x8, for indices 0,1,2,3.
  // We do that for performance. Instead of extracting a huge patch from the
  // full resolution image, we extract a smaller patch from a downsampled image.
  // Patch ground footprint stays the same.
  // Downsampling can be different in X and Y.
  double scaleLx = 1;
  double scaleLy = 1;
  int xIndexImgL = 0;
  int yIndexImgL = 0;
  double downLx  = 1;
  double downLy  = 1;
  int decimationL = p->decimation;
  int lTileScale = 1;
  int tIndexImgL = 0;

  double scaleRx = 1;
  double scaleRy = 1;
  int xIndexImgR = 0;
  int yIndexImgR = 0; 
  double downRx  = 1;
  double downRy  = 1;
  int decimationR = p->decimation;
  int rTileScale = 1;
  int tIndexImgR = 0;

  // Singular Value Decomposition variables
  JacobiSVD<MatrixXf> svd;
  MatrixXf U, V;
  VectorXf SR, SL;


  // The simplest case is if SCALING is turn off by the user. In that case, 
  // the left and right patches are extracted from the nominal input images,
  // with a size that is equal to TEMPLATE and SEARCH.
  // In that case we can jump directly to patches extraction.
  // If scaling is ON, that means that the user want to insure that at least
  // TEMPLATE real pixels are present in both Left and Right patches for 
  // correlation. We need to look more closely at the apriori transform between
  // the images and the tiling to make sure that the extracted patches satisfy
  // the requirement.
  if (p->scaling) {

     // If the user wants to account for any TILING:
     // Check if the image are tiled (i.e., images downsampled onboard and then 
     // upsampled on ground). Get the tiling index of the left and right images
     // at the current pixel. Only the center pixel of the correlation window is
     // checked.
     // The tiling is accounted for now instead of later while analysing the
     // geometric transform between Left and Right (simpler this way). Pick the
     // corresponding pyramid Left and Right, and adapt the transform matrix. 
     // So, e.g., if Left image tiling=2 (i.e., 2x downsampled onboard, compared
     // to what we have now), set current Left image one up in the pyramid, and
     // adapt the initial transform
     if (p->tiling) {
        lTileScale = (p->left_tile)->get(left_line, left_samp);
        rTileScale = (p->right_tile)->get(right_line, right_samp);

        if (lTileScale != 1 || rTileScale != 1) {

           // Get the indices of the image to use in the pyramid container
           tIndexImgL = log2(lTileScale);
           tIndexImgR = log2(rTileScale);

           //Adjust transform to account for the tiling
           double tileRatio = (double)lTileScale / rTileScale;
           samp_coef[0] *= tileRatio;
           samp_coef[1] *= tileRatio;
           line_coef[0] *= tileRatio;
           line_coef[1] *= tileRatio;
           samp_coef[3] *= tileRatio * tileRatio;
           line_coef[3] *= tileRatio * tileRatio;


           // Adjust coefficients limits if needed
           if (p->use_limits) {
              samp_coef_limits[0][0] *= tileRatio;
              samp_coef_limits[0][1] *= tileRatio;
              samp_coef_limits[1][0] *= tileRatio; 
              samp_coef_limits[1][1] *= tileRatio;

              line_coef_limits[0][0] *= tileRatio;
              line_coef_limits[0][1] *= tileRatio;
              line_coef_limits[1][0] *= tileRatio;
              line_coef_limits[1][1] *= tileRatio;

              // No limit on the xy terms (line/samp_coef[3]). Historical 
              // default behavior.
           }

           // Adjust translation terms
           line_coef[2] /= (double) rTileScale; 
           samp_coef[2] /= (double) rTileScale; 
        }

     }



     // Get the transform in a matrix layout
     // We're going to express the affine transform between L and R with a
     // Singular Value Decomposition (SVD), which will render the analysis of
     // the transform easier
     MatrixXf aMat(2,2);
     aMat(0,0) = samp_coef[0];
     aMat(0,1) = samp_coef[1];
     aMat(1,0) = line_coef[0];
     aMat(1,1) = line_coef[1];


     // Sanity check.
     // Determinant of affine matrice must be strictly positive. 
     // Otherwise the transform involves a "reflection" which 
     // shouldn't happen.
     if (aMat.determinant() <= 0)
        return -1;

     // Compute the SVD of the affine transform. It should be the
     // simple composition of rotations and shear
     svd = JacobiSVD<MatrixXf>(aMat, ComputeThinU | ComputeThinV);
     U = svd.matrixU();
     V = svd.matrixV();
     SR = svd.singularValues();

     // Some sanity/limits checks on the transform.

     // Determinant of both rot matrices (U and V) should be either positive or 
     // negative
     if (U.determinant() * V.determinant() < 0)
        return -1;
 
     // Determinant amplitude of U and V should be ~ 1
     if (fabs(fabs(U.determinant()) - 1) > 1e-5  || 
         fabs(fabs(V.determinant()) - 1) > 1e-5)
        return -1;

     // Thresholding the singular value if larger than user allowable limit.
     // Other option is to bail out if limit is reached
     if ((SR[0] > p->maxResRatio) || (SR[0] < 1.0/p->maxResRatio) ||
         (SR[1] > p->maxResRatio) || (SR[1] < 1.0/p->maxResRatio)) {

        if (SR[0] > p->maxResRatio) SR[0] = p->maxResRatio; 
        if (SR[0] < 1./p->maxResRatio) SR[0] = 1./p->maxResRatio; 
        if (SR[1] > p->maxResRatio) SR[1] = p->maxResRatio; 
        if (SR[1] < 1./p->maxResRatio) SR[1] = 1./p->maxResRatio; 

        // Need now to update the transform matrix
        aMat = U * SR.asDiagonal() * V.transpose();

        // and the corresponding coef. 
        samp_coef[0] = aMat(0,0);
        samp_coef[1] = aMat(0,1);
        line_coef[0] = aMat(1,0);
        line_coef[1] = aMat(1,1);

        //TODO Should update limits?
     }


     // Compute the pseudo-inverse to get the transform from R to L
     SL = SR;
     SL[0] = 1./SR[0];
     SL[1] = 1./SR[1];





     // If decimation is activated, analyse the possible use of downsampled 
     // input images.
      
     if (decimationL) {

        // The transform expressed as SVD
        //
        //      V1                                   / \
        //    /                M             S[1]U1 /   \
        //   /        ------------------->               \
        //   \                                            \
        //    \V2                                          \
        //                                                  \S[0]U2
        //   Left                                   Right
        //
        // (V1,V2) (U1,U2) are perpendicular unit vectors - schema doesn't show
        // M is the transform matrix
        // M.V1 = S[0].U1
        // M.V2 = S[1].U2

        // The singular values (SR[0] and SR[1]) represent the ellipse (in Right
        // image) semi-major and semi-minor axis corresponding of a unit circle 
        // in Left that have been transformed by M.


        // Is the transform such that we could use a downsampled version of the 
        // image (Left and/or Right) to speed up the resampling process?


        // First, look at the Right image:
        // This is the logic to define the downsampling in x and y:
        // - First singular value needs to be > 2 for potential downsampling 
        //   use. A necessary condiction but not sufficient.
        // - If S[0] > 2, then we look at the amplitude and orientation of S[1]:
        //   Look at projection of S[1] on X and Y. Whatever axis gets the 
        //   larger value is constrained (for downsampling in the axis 
        //   direction) by the projection value.
        // - Then compare the projection of S[1] on the other axis with the 
        //   projection of S[0] on that axis. If the former is larger, then it
        //   controls the downsampling in that axis too. Otherwise it is 
        //   constrained by the latter. 
//TODO add a threshold on the SR[0] value or SR1UX...
        if (SR[0] > 2) {
           float SR0Ux = fabs(SR[0] * U(0,0));
           float SR0Uy = fabs(SR[0] * U(1,0));
           float SR1Ux = fabs(SR[1] * U(0,1));
           float SR1Uy = fabs(SR[1] * U(1,1));
           if (SR1Ux > SR1Uy) {
              xIndexImgR = (int) log2(SR1Ux);
              if (SR1Ux < SR0Ux) 
                 yIndexImgR = (int) log2(SR1Uy);
              else 
                 yIndexImgR = (int) log2(SR0Uy);

              if (xIndexImgR < 0) xIndexImgR = 0;
              if (yIndexImgR < 0) yIndexImgR = 0;
              downRx = std::pow(2, xIndexImgR);
              downRy = std::pow(2, yIndexImgR);
              SR[0] /= downRy;
              SR[1] /= downRx;
           }
           else {
              yIndexImgR = (int) log2(SR1Uy);
              if (SR1Uy < SR0Uy) 
                 xIndexImgR = (int) log2(SR1Ux);
              else 
                 xIndexImgR = (int) log2(SR0Ux);
   
              if (xIndexImgR < 0) xIndexImgR = 0;
              if (yIndexImgR < 0) yIndexImgR = 0;
              downRx = std::pow(2, xIndexImgR);
              downRy = std::pow(2, yIndexImgR);
              SR[0] /= downRx;
              SR[1] /= downRy;
           }
        }



        // Update the pseudo-inverse of the singular value matrix to get the 
        // transform from R to L
        SL[0] = 1./SR[0];
        SL[1] = 1./SR[1];

     
        // Same thing as above to define the possible downsampled Left image.
        // Note: SR values are intially ordered in decreasing order, which means
        // that SL values should be ordered in increasing order. However, the 
        // Right downsampling adjustment above might change this. But, if either
        // SL[0] or SL[1] is greater than 1, the larger value should still be 
        // SL[1]. This makes the if statement below always correct, no matter 
        // any dowsampling adjustment made above. 
//TODO add a threshold on the SR[0] value or SR1UX...
        if (SL[1] > 2) {
           float SL0Vx = fabs(SL[0] * V(0,0));
           float SL0Vy = fabs(SL[0] * V(1,0));
           float SL1Vx = fabs(SL[1] * V(0,1));
           float SL1Vy = fabs(SL[1] * V(1,1));
           if (SL1Vx > SL1Vy) {
              xIndexImgL = (int) log2(SL1Vx);
              if (SL1Vx < SL0Vx) 
                 yIndexImgL = (int) log2(SL1Vy);
              else 
                 yIndexImgL = (int) log2(SL0Vy);

              if (xIndexImgL < 0) xIndexImgL = 0;
              if (yIndexImgL < 0) yIndexImgL = 0;
              downLx = std::pow(2, xIndexImgL);
              downLy = std::pow(2, yIndexImgL);
              SL[0] /= downLy;
              SL[1] /= downLx;
           }
           else {
              yIndexImgL = (int) log2(SL1Vy);
              if (SL1Vy < SL0Vy) 
                 xIndexImgL = (int) log2(SL1Vx);
              else 
                 xIndexImgL = (int) log2(SL0Vx);

              if (xIndexImgL < 0) xIndexImgL = 0;
              if (yIndexImgL < 0) yIndexImgL = 0;
              downLx = std::pow(2, xIndexImgL);
              downLy = std::pow(2, yIndexImgL);
              SL[0] /= downLx;
              SL[1] /= downLy;
           }
        }


        // Update SR with changes brought to SL
        SR[0] = 1/SL[0];
        SR[1] = 1/SL[1];



        // At this stage, the Left and Right downsampled images to use for 
        // extracting the Left and Right patches have been identified.

        // Update the transform coefficients, so they express the transform 
        // between the L and R in the newly selected downsampled images.
        // In the native images we had this relation (in matrice notation)
        // |X| = M |x|     with M (affine 2x2 matrices), upper case in R, 
        // |Y|     |y|                                   lower case in L
        // Now in the downsampled L and R images that we use, we have:
        // |dRx 0| |X'| = M |dLx 0||x'| 
        // |0 dRy| |Y'|     |0 dLy||y'|
        // with dRx, dRy, dLx, dLy the downsampling factor of the R in x and 
        // y and L in x and y, respectively. The "prime" refers to the 
        // coordinates in the downsampled images.
        // with |X| = |dRx 0||X'| and equivalently for the L coordinates.
        //      |Y|   |0 dRy||Y'|
        // Rearranging we get:
        // |X'| = |1/dRx 0|M|dLx 0||x'|
        // |Y'| = |0 1/dRy| |0 dLy||y'|
        // So the conversion of the initial parameters is:
        samp_coef[0] *= (double)downLx / downRx;
        samp_coef[1] *= (double)downLy / downRx;
        line_coef[0] *= (double)downLx / downRy;
        line_coef[1] *= (double)downLy / downRy;
        samp_coef[3] *= (double)downLx / downRx * (double)downLy / downRx;
        line_coef[3] *= (double)downLx / downRy * (double)downLy / downRy;

        // Update the coefficients limit accordingly
        // Note that we need to account for any tiling. This is already done for
        // the transform coef, but not for the limit
        if (p->use_limits) {
           samp_coef_limits[0][0] *= (double)downLx / downRx;
           samp_coef_limits[0][1] *= (double)downLx / downRx;
           samp_coef_limits[1][0] *= (double)downLy / downRx;
           samp_coef_limits[1][1] *= (double)downLy / downRx;

           line_coef_limits[0][0] *= (double)downLx / downRy;
           line_coef_limits[0][1] *= (double)downLx / downRy;
           line_coef_limits[1][0] *= (double)downLy / downRy;
           line_coef_limits[1][1] *= (double)downLy / downRy;

           // No limit on the xy terms (line/samp_coef[3]). Default behavior.
        }

        // Adjust translation terms
        line_coef[2] /= (double) downRy; 
        samp_coef[2] /= (double) downRx; 


     } //End if statement on decimation


     // Compute the "scaling" in X and Y of both the Left and Right images that
     // needs to be applied to the Left and/or Right patches to guarantee that
     // at least TEMPLATE real pixels are correlated and/or enough SEARCH pixels
     // are present in the left and right patches.

     // Define how to scale the Left patch, according to the transform (possibly
     // updated if decimation is on and downsampled images have been selected).
     scaleLx = std::max(std::fabs(SL[0]*V(0,0)), std::fabs(SL[1]*V(0,1)));
     if (scaleLx < 1) scaleLx = 1;
     scaleLy = std::max(std::fabs(SL[0]*V(1,0)), std::fabs(SL[1]*V(1,1)));
     if (scaleLy <1 ) scaleLy = 1;

     // Same with the Right patch
     scaleRx = std::max(std::fabs(SR[0]*U(0,0)), std::fabs(SR[1]*U(0,1)));
     if (scaleRx < 1) scaleRx = 1;
     scaleRy = std::max(std::fabs(SR[0]*U(1,0)), std::fabs(SR[1]*U(1,1)));
     if (scaleRy < 1) scaleRy = 1;


     // If the scaling in X and Y are equal to 1, the patch can be a direct
     // subset of the Left/Right image instead of having to resample the image
     // to get the patch, which is more efficient. Otherwise, the patch must be
     // scaled and the patch extraction will involve the more expensive EWA 
     // resampler. To avoid jittering between subset and resampling when the
     // scaling is close to 1, there's the possibility to force the subset
     // approach if the scaling is "close" to 1 (user value).
      
     // Get the threshold at which an image is to be subset instead of
     // resampled. 
     float threshold = 1 - p->scalingThreshold; //Legacy of scalingThreshold!
     
     if ( abs(1.0 - abs(scaleLx)) < threshold && 
          abs(1.0 - abs(scaleLy)) < threshold ) {
        scaleLx = 1;
        scaleLy = 1;
        decimationL = 0;
     }

     if ( abs(1.0 - abs(scaleRx)) < threshold && 
          abs(1.0 - abs(scaleRy)) < threshold ) {
        scaleRx = 1;
        scaleRy = 1;
        decimationR = 0;
     }


  } // End of if (p->scaling) statement




  // There's a bit of logic branching here, which depends on scaling and
  // decimation parameters
  // - scaling=0 (decimation=0): 
  // Goes to subset with tIndexImg = x/yIndexImg = 0. So first pyramid level 
  // image is selected
  // - scaling=1, decimation=0:
  // If decimation=0 is user choice, then x/yIndexImg=0 by construction. The 
  // patch is retrieve from the pyramid first level image (or a downsampled if
  // tiling is not null - this is the exception of the decimation logic!)
  // If decimation=0 because of scaling threshold, then x/yIndexImg might be 
  // different than 0 and a downsampled image might have been selected. In that 
  // case, the patch is subset from the downsampled image.
  // - scaling=1, decimation=1: force resampling

  if (decimationL) {
     int halfX = (int)(nsw_left/2 * scaleLx );
     int halfY = (int)(nlw_left/2 * scaleLy );
     float centerX = (float)left_samp / (downLx*lTileScale);
     float centerY =  (float)left_line / (downLy*lTileScale);
     if ( (centerX - halfX < 0) || (centerY - halfY < 0) || 
          (centerX + halfX >= left_ns/(downLx*lTileScale)) ||
          (centerY + halfY >= left_nl/(downLy*lTileScale)) )
        return -1;

     int szLeft = (halfX * 2 + 1) * (halfY * 2 + 1);
     std::valarray<float> matX(szLeft);
     std::valarray<float> matY(szLeft);
     int index = 0;
     for (int i=-halfY; i<=halfY; i++) {
        for (int j=-halfX; j<=halfX; j++) {
           matX[index] = centerX + j;
           matY[index] = centerY + i;
           index++;
        }
     }
     left_area = ewa(p->mipmapL[tIndexImgL + yIndexImgL + p->pyr][tIndexImgL + xIndexImgL + p->pyr],
                     halfX * 2 + 1, halfY * 2 + 1, matX, matY, p->gaussP, V, SL, U);

  }
  else { 
     int halfx = (int)((nsw_left/2) * (scaleLx * downLx * lTileScale) + 0.5);
     int halfy = (int)((nlw_left/2) * (scaleLy * downLy * lTileScale) + 0.5);
     int startx = left_samp - halfx;
     int starty = left_line - halfy;
     if ( (startx < 0) || (starty < 0) || 
          (left_samp + halfx >= left_ns) || (left_line + halfy >= left_nl) )
        return -1;

     //TODO May be best to use what decimation would have been and use a 
     //adequately (full res) low passed image
     left_area = new SimpleImage<double>(*(p->mipmapL[tIndexImgL + yIndexImgL + p->pyr]
                                                     [tIndexImgL + xIndexImgL + p->pyr]),
                                         starty, startx, 
                                         2*halfy+1, 2*halfx+1);
  }



  if (decimationR) {
     int halfX = (int)(nsw_right/2 * scaleRx);
     int halfY = (int)(nlw_right/2 * scaleRy);
     float centerX = right_samp / (downRx*rTileScale);
     float centerY = right_line / (downRy*rTileScale);
     if ( (centerX - halfX < 0) || (centerY - halfY < 0) || 
          (centerX + halfX >= right_ns/(downRx*rTileScale)) ||
          (centerY + halfY >= right_nl/(downRy*rTileScale)) )
        return -1;

     int szRight = (halfX * 2 + 1) * (halfY * 2 + 1);
     std::valarray<float> matX(szRight);
     std::valarray<float> matY(szRight);
     int index = 0;
     for (int i=-halfY; i<=halfY; i++) {
        for (int j=-halfX; j<=halfX; j++) {
           matX[index] = centerX +  j;
           matY[index] = centerY +  i;
           index++;
        }
     }
     right_area = ewa(p->mipmapR[tIndexImgR + yIndexImgR + p->pyr][tIndexImgR + xIndexImgR + p->pyr],
                     halfX * 2 + 1, halfY * 2 + 1, matX, matY, p->gaussP, U, SR, V);

  }
  else { 
     int halfx = (int)((nsw_right/2) * (scaleRx * downRx * rTileScale) + 0.5);
     int halfy = (int)((nlw_right/2) * (scaleRy * downRy * rTileScale) + 0.5);
     int startx = right_samp - halfx;
     int starty = right_line - halfy;
     if ( (startx <= 0) || (starty <= 0) || 
          (right_samp + halfx >= right_ns) || (right_line + halfy >= right_nl) )
        return 1;

     right_area = new SimpleImage<double>(*(p->mipmapR[tIndexImgR + yIndexImgR + p->pyr]
                                                      [tIndexImgR + xIndexImgR + p->pyr]),
                                         starty, startx, 
                                         2*halfy+1, 2*halfx+1);
  }







  // correct candidate tiepoints by correlation
  int status = gruen3(left_area, right_area,
                      correl_img, &line_offset, &samp_offset,
                      line_coef, samp_coef, line_coef_limits, samp_coef_limits,
                      line_temp, samp_temp, percent, limits, &quality, mode,
                      p->ftol, 0, p->use_limits);


  left_area->free();
  right_area->free();
  delete left_area;
  delete right_area;

  if (status > 0)
  return -1;

  first_quality = (short)(quality*QUAL_FACTOR);
  if (first_quality <= 0)
     first_quality = (short)1;

  if (quality < p->min_quality) {
     p->qual->set(left_line,left_samp, first_quality);
     return -2;
  }

  right_liner = right_line + line_offset * downRy * rTileScale;
  right_sampr = right_samp + samp_offset * downRx * rTileScale;
	

  // Correct X and Y coef back to pre-decimation, to keep the coefficients 
  // consistent with the calling function.
  if (decimationL || decimationR) {
     samp_coef[0] /= ((double)(downLx * lTileScale) / (rTileScale * downRx));
     samp_coef[1] /= ((double)(downLy * lTileScale) / (rTileScale * downRx));
     line_coef[0] /= ((double)(downLx * lTileScale) / (rTileScale * downRy));
     line_coef[1] /= ((double)(downLy * lTileScale) / (rTileScale * downRy));
     samp_coef[3] /= ((double)(downLx * downLx * lTileScale * lTileScale) / (downRx * downRx * rTileScale * rTileScale));
     line_coef[3] /= ((double)(downLx * downLx * lTileScale * lTileScale) / (downRy * downRy * rTileScale * rTileScale));
  }
  // No need to readjust the coef limits, as the values are not kept







    //---------------------------------
    // Do reverse correlation for check
    // WARNING: Don't do it with SCALING on.

    if (p->check != 0 && quality <= p->check_quality) {

        // Prepare to correlate the point.  Same as above but we switch the
	// sides.
        int start_samp = (int)(right_sampr+0.5) - (nsw_left/2);
        int start_line = (int)(right_liner+0.5) - (nlw_left/2);
        if (start_samp <= 0) return -1;
        if (start_line <= 0) return -1;
        if (start_samp + (nsw_left - 1) >= right_ns) return -1;
        if (start_line + (nlw_left - 1) >= right_nl) return -1;

        // Extract the subset image for the template
        right_area = new SimpleImage<double>(*right_img,
                                             start_line, start_samp,
                                             nlw_left, nsw_left);


        start_samp = left_samp - (nsw_right/2);
        start_line = left_line - (nlw_right/2);
        if (start_samp <= 0) return -1;
        if (start_line <= 0) return -1;
        if (start_samp + (nsw_right - 1) >= left_ns) return -1;
        if (start_line + (nlw_right - 1) >= left_nl) return -1;

        // Extract the subset image for the search
        left_area = new SimpleImage<double>(*left_img,
                                            start_line, start_samp,
                                            nlw_right, nsw_right);



        // Set initial coefficient values
        //
        // Warning: This does not account for rotation/scaling coefficients.
        // A simple translation is assumed.
        rev_line_coef[0]=0.;
        rev_line_coef[1]=1.;
        rev_line_coef[2] = 0.0;
        rev_line_coef[3]=0.;
        rev_samp_coef[0]=1.;
        rev_samp_coef[1]=0.;
        rev_samp_coef[2] = 0.0;
        rev_samp_coef[3]=0.;
 

        // Perform the reverse correlation
       
        status = gruen3(right_area, left_area,
		        correl_img, &line_offset2, &samp_offset2,
		        rev_line_coef, rev_samp_coef,
		        p->line_coef_limits, p->samp_coef_limits,
		        line_temp, samp_temp, percent, limits, &quality, mode,
		        p->ftol, 0, p->use_limits);

        left_area->free();
        right_area->free();
        delete left_area;
        delete right_area;


        if (status > 0)
	    return -3;

        if (quality < p->min_quality) {
            p->qual->set(left_line,left_samp, first_quality);
	    return -2;
	}

        if (line_offset2 < -p->check || line_offset2 > p->check ||
	    samp_offset2 < -p->check || samp_offset2 > p->check)
	    return -4;				// failed to find same point

    }			// end of reverse correlation





    p->qual->set(left_line,left_samp, first_quality);

    if (p->npts < 10) {
	snprintf(msg, msgLen, "%d %d %d %d %f %f %f", left_line, left_samp,
		right_line, right_samp,
		right_line+line_offset, right_samp+samp_offset, quality);
	zvmessage(msg, "");
    }

    // Save right image coordinates from offsets 
    p->line_mask->set(left_line,left_samp, right_liner);
    p->samp_mask->set(left_line,left_samp, right_sampr);


#pragma omp critical(npts)
    {
        p->npts += 1;
    }

    // Save coefficients 
    // Remember that samp_coef is [a,b,c,g] and line_coef is [d,e,f,h].
    // No need to save the translation terms (c,f) since that's done above.
    p->out_coef_a->set(left_line,left_samp, samp_coef[0]);
    p->out_coef_b->set(left_line,left_samp, samp_coef[1]);
    // c is the translational term, dealt with above
    p->out_coef_d->set(left_line,left_samp, line_coef[0]);
    p->out_coef_e->set(left_line,left_samp, line_coef[1]);
    // f is the translational term, dealt with above
    p->out_coef_g->set(left_line,left_samp, samp_coef[3]);
    p->out_coef_h->set(left_line,left_samp, line_coef[3]);

    return 0;

}








////////////////////////////////////////////////////////////////////////
// Read an image into memory.  Returns the unit number (closed).
////////////////////////////////////////////////////////////////////////

static int read_image(char *param, int instance,
		SimpleImage<double> *&image, int band, int close)
{
    char filename[PIG_MAX_FILENAME_SIZE+1];
    const size_t msgLen = 150;
    char msg[msgLen];
    int unit;

    zvpone(param, filename, instance, PIG_MAX_FILENAME_SIZE);
    zvunit(&unit, param, instance, "u_name", filename, NULL);
    zvopen(unit, "OP", "READ", "U_FORMAT", "DOUB", "OPEN_ACT", "AS", NULL);

    /* get input image dimensions */

    int nl, ns, nb;

    zvget(unit, "NL", &nl, "NS", &ns, "NB", &nb, NULL);
    if (ns > MAX_NS) {
	zvmessage("Input images exceed buffer limit sizes", "");
	zabend();
    }
    if (band > nb) {
        snprintf(msg, msgLen, "Input band (%d) is greater than number of bands in input "
                "image. Band set to 1.", band);
        zvmessage(msg, "");
        band = 1;
    }

    /* read input(s) into memory */

    if (image != NULL)
	delete image;
    image = new SimpleImage<double>(nl, ns);

    for (int j=0; j < nl; j++) {
	zvread(unit, image->linePtr(j), "BAND", band, "LINE", j+1, NULL);
    }

    if (close)
        zvclose(unit, NULL);

    return unit;
}





////////////////////////////////////////////////////////////////////////
// Set up the limits created by the given rotation angle and (additive)
// delta angle, and the x/y scale and (multiplicative) scale factor.
////////////////////////////////////////////////////////////////////////

void set_up_limits(struct CorrelParams *p, double rot, double rot_delta,
			double xscale, double yscale, double scale_factor)
{
    double a = cos(rot + rot_delta);
    double b = sin(rot + rot_delta);
    double d = -b;			// -sin(angle)
    double e = a;			// cos(angle)

    a = a * (xscale * scale_factor);
    b = b * (yscale * scale_factor);
    d = d * (xscale * scale_factor);
    e = e * (yscale * scale_factor);

    if (a < p->samp_coef_limits[0][0]) p->samp_coef_limits[0][0] = a;
    if (a > p->samp_coef_limits[0][1]) p->samp_coef_limits[0][1] = a;
    if (b < p->samp_coef_limits[1][0]) p->samp_coef_limits[1][0] = b;
    if (b > p->samp_coef_limits[1][1]) p->samp_coef_limits[1][1] = b;

    if (d < p->line_coef_limits[0][0]) p->line_coef_limits[0][0] = d;
    if (d > p->line_coef_limits[0][1]) p->line_coef_limits[0][1] = d;
    if (e < p->line_coef_limits[1][0]) p->line_coef_limits[1][0] = e;
    if (e > p->line_coef_limits[1][1]) p->line_coef_limits[1][1] = e;
}








////////////////////////////////////////////////////////////////////////
// Downsample the given image by a factor of two.  The output image is
// re-allocated if non-null, and then the original output is deleted
// *after* the operation.  This allows the input to be the same as the
// output on entry (in which case the input will be deleted).
// Uses simple averaging.  Output is sized nl/2, ns/2.
////////////////////////////////////////////////////////////////////////

static void downsample(SimpleImage<double> *img_in,
		       SimpleImage<double> *&img_out)
{
    int nl = img_in->getNL();
    int ns = img_in->getNS();
    SimpleImage<double> *save_output = img_out;
    img_out = new SimpleImage<double>(nl/2, ns/2);

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









////////////////////////////////////////////////////////////////////////////////
// Downsample the given image by a factor of two.  Similar to function  above 
// but with a gaussian low pass filtering instead of a 2x2 boxcar filter. The 
// output image is re-allocated if non-null, and then the original output is 
// deleted *after* the operation.  This allows the input to be the same as the 
// output on entry (in which case the input is deleted). Output is (nl/2, ns/2).
////////////////////////////////////////////////////////////////////////////////


static void decimateImage(SimpleImage<double> *img_in,
		          SimpleImage<double> *&img_out,
                          int downX, int downY)
{


    // Get input image size
    int nb = img_in->getNB();
    int nl = img_in->getNL();
    int ns = img_in->getNS();

    // Check that the decimation factor are valid
    if (downX < 1) downX = 1;
    if (downY < 1) downY = 1;
    if (downX > ns) downX = ns;
    if (downY > nl) downY = nl;

    // Save output image and allocate container for decimated image
    SimpleImage<double> *save_output = img_out;
    img_out = new SimpleImage<double>(nb, nl/downY, ns/downX);

    for (int k=0; k < nb; k++) {
       for (int j = 0; j < nl-downY+1; j += downY) {
	  int jj = j / downY;
	  for (int i = 0; i < ns-downX+1; i += downX) {
	     int ii = i / downX;
	     img_out->set(k, jj, ii, img_in->get(k,j,i));
	  }
       }
    }
 
    if (save_output != NULL) {
	save_output->free();
        delete save_output;
    }
}


static void decimateImage(SimpleImage<double> *img_in,
		          SimpleImage<double> *&img_out)
{
   decimateImage(img_in, img_out, 2, 2);
}



static void downsampleImage(SimpleImage<double> *img_in,
		            SimpleImage<double> *&img_out,
                            int down)
{
    downsampleImage(img_in, img_out, down, down);
}



static void downsampleImage(SimpleImage<double> *img_in,
		            SimpleImage<double> *&img_out,
                            int downX, int downY)
{
   // Some hard-coded value (need parametrization eventually)
   double cst = 0.8;

   // Define the Gaussian sigma needed for the given downsampling factor
   double sigmaX = (downX > 1) ? cst * sqrt(downX * downX - 1.0) : 0.0;
   double sigmaY = (downY > 1) ? cst * sqrt(downY * downY - 1.0) : 0.0;
 
   // [1] Low pass the image with a Gaussian filter
   lowpassGauss(img_in, img_out, sigmaX, sigmaY);

   // [2] Decimate the image according to the downsampling factor
   decimateImage(img_out, img_out, downX, downY);

}



// This is a specific function to downsample the tiling factor of a given image
// tiling map.
// The input tiling map size is reduced by a factor of 2. The actual tiling DN,
// are "downsampled" accordingly. Tiling factors of 1, 2, 4, 8 are downsampled
// to 1, 1, 2, 4 respectively
static void downsampleTiling(SimpleImage<int> *tile_in,
		             SimpleImage<int> *&tile_out)
{

   if (tile_in == nullptr) return;

   // Adjust the tile image accordingly. That is, if image is upsample at a
   // given pixel, lower the tile index by 1 level (factor of 2). Otherwise
   // set tile index level to 1 (full frequency reslution w.r.t spatial 
   // resolution)
   int nl = tile_in->getNL();
   int ns = tile_in->getNS();
   SimpleImage<int> *save_output = tile_out;
   tile_out = new SimpleImage<int>(nl/2, ns/2);

   for (int j=0; j < nl-1; j+=2) {
      int jj = j / 2;
      for (int i=0; i < ns-1; i+=2) {
         int ii = i / 2;
         if (tile_in->get(j,i) > 2)
            tile_out->set(jj, ii, pow(2, log2(tile_in->get(j,i))-1));
         else
	    tile_out->set(jj, ii, 1);
      }
   }

   if (save_output != NULL) {
      save_output->free();
      delete save_output;
   }

}




// This is a specific function to downsample the tiling factor map of a given 
// image.
// The actual tiling DN values represents the lowest downsampling level a pixel
// was subject to on-board (before being upsampled again). The DN are power of
// 2 values, i.e., 1,2,4,8.
// Downsampling this map requires specific DN treatment.
// The input dowsampling index represents the level of downsampling (e.g., 
// downindex=2 <-> map downsampled by x4)
// The actual tiling DN, are "downsampled" accordingly. Tiling factors of 1, 2, 
// 4, 8 are downsampled to 1, 1, 2, 4 respectively.
static void downsampleTiling(SimpleImage<int> *tile_in,
		             SimpleImage<int> *&tile_out, 
                             int downIndex)
{

   if (tile_in == nullptr) return;

   int nl = tile_in->getNL();
   int ns = tile_in->getNS();
   int downFactor = pow(2, downIndex); //downIndex=0,1,2,3 <-> downFactor=1,2,4,8

   SimpleImage<int> *save_output = tile_out;
   tile_out = new SimpleImage<int>(nl/downFactor, ns/downFactor);

   for (int j=0; j < nl-downFactor+1; j += downFactor) {
      int jj = j / downFactor;
      for (int i=0; i < ns-downFactor+1; i += downFactor) {
         int ii = i / downFactor;
         int dnIndex = log2(tile_in->get(j,i));
         if (dnIndex > downIndex)
            tile_out->set(jj, ii, pow(2, log2(dnIndex-downIndex)));
         else
	    tile_out->set(jj, ii, 1);
      }
   }

   if (save_output != NULL) {
      save_output->free();
      delete save_output;
   }

}










/*
static void downsampleGauss(SimpleImage<double> *img_in,
                            SimpleImage<int> *tile_in,
		            SimpleImage<double> *&img_out,
		            SimpleImage<int> *&tile_out)
{
   // Downsample input image
   downsampleGauss(img_in, img_out);

   // Adjust the tile image accordingly. That is, if image is upsample at a
   // given pixel, lower the tile index by 1 level (factor of 2). Otherwise
   // set tile index level to 1 (full frequency reslution w.r.t spatial 
   // resolution)
   int nl = tile_in->getNL();
   int ns = tile_in->getNS();
   SimpleImage<int> *save_output = tile_out;
   tile_out = new SimpleImage<int>(nl/2, ns/2);

   for (int j=0; j < nl-1; j+=2) {
      int jj = j / 2;
      for (int i=0; i < ns-1; i+=2) {
         int ii = i / 2;
         if (tile_in->get(j,i) > 2)
            tile_out->set(jj, ii, pow(2, log2(tile_in->get(j,i))-1));
         else
	    tile_out->set(jj, ii, 1);
      }
   }

   if (save_output != NULL) {
      save_output->free();
      delete save_output;
   }

}

*/



////////////////////////////////////////////////////////////////////////
// Low-pass filters the given image, copying it to a new location as we go.
// Simple box filter; pixel is replaced with average of nxn surrounding pixels.
// window must be odd.
////////////////////////////////////////////////////////////////////////

static void lowpassBox(SimpleImage<double> *img_in,
		    SimpleImage<double> *&img_out, int window)
{
    int i, j, k;
    int nl = img_in->getNL();
    int ns = img_in->getNS();

    SimpleImage<double> *sum_img = new SimpleImage<double>(nl, ns);
    sum_img->zero();

    if (img_out != NULL)
	delete img_out;
    img_out = new SimpleImage<double>(nl, ns);
    img_out->zero();

    int nw = window/2;			// distance either side of center

    // Create image containing horizontal sums for each pixel

    for (j=0; j < nl; j++) {
	for (i=0; i < ns; i++) {
	    if (j < nw || j >= nl-nw || i < nw || i >= ns-nw)
		continue;			// border case
	    else {
		for (k=-nw; k <= nw; k++) {
		    sum_img->addto(j, i, img_in->get(j,i+k));
		}
	    }
	}
    }

    // Create output image by summing the columns and dividing by the # pixels

    int npix = window * window;

    for (j=0; j < nl; j++) {
	for (i=0; i < ns; i++) {
	    if (j < nw || j >= nl-nw || i < nw || i >= ns-nw)
		continue;			// border case
	    else {
		for (k=-nw; k <= nw; k++) {
		    img_out->addto(j, i, sum_img->get(j+k, i));
		}
	    }
	    img_out->set(j,i, img_out->get(j,i) / npix);
	}
    }

    delete sum_img;
}







// Convolution of buffer (one line or column of the image) with the gaussian
// kernel. This is a time intensive routine, so deserves attention for 
// efficiency.
static void ConvBufferFast(double *buffer, double *kernel, int rsize, int ksize)
{
   int i;
   double *bp, *kp, *endkp, *mp;
   double sum, sum2;
   double mask[rsize+ksize-1];
   int central = ksize/2;

   // Locate pixels in buffer whose values are 0
   for (i=0; i<(rsize+ksize-1); i++) {
      mask[i] = 1.0;
      if (buffer[i]==0.0) 
         mask[i] = 0.0;
   }
      
   // Main convolution
   for (i = 0; i < rsize; i++) {
     
      if (buffer[i+central]==0.0) {
         buffer[i] = 0.0;
         continue;
      }

      sum = 0.0;
      sum2 = 0.0;
      bp = &buffer[i];
      kp = &kernel[0];
      endkp = &kernel[ksize];
      mp = &mask[i];

      while (kp < endkp) { 
        sum += *bp++ * *kp;
        sum2 += *mp++ * *kp++;
      }			  

      if (sum2 == 0.0)
         buffer[i]=0.0;
      else
         buffer[i] = sum/sum2;
   }
}




// Convolve image with the 1-D kernel vector along image rows.  
// Pixels outside the image are set to the value of the closest image pixel.
static void ConvHorizontal(SimpleImage<double> *image, double *kernel, int ksize, int omp_on)
{
  int r, c, i, halfsize;
  int width = image->getNS();
  int height = image->getNL();
  halfsize = ksize / 2;


#pragma omp parallel private (r, c, i) if (omp_on)
{
  double buffer[width+ksize-1];

  #pragma omp for
  for (r = 0; r < height; r++) {
    // Copy the row into buffer with pixels at ends replicated for
    // half the mask size.  This avoids need to check for ends
    // within inner loop. 
    for (i = 0; i < halfsize; i++)
      buffer[i] = image->get(r,0);
    for (i = 0; i < width; i++)
      buffer[halfsize + i] = image->get(r, i);
    for (i = 0; i < halfsize; i++)
      buffer[halfsize + width + i] = image->get(r, width-1);

    ConvBufferFast(buffer, kernel, width, ksize);
    for (c = 0; c < width; c++)
      image->set(r, c, buffer[c]);
  }

} //end pragma

}




// Same as ConvHorizontal, but apply to vertical columns of image.
static void ConvVertical(SimpleImage<double> *image, double *kernel, int ksize, int omp_on)
{
  int r, c, i, halfsize;
  int width = image->getNS();
  int height = image->getNL();
//  float buffer[height+ksize-1];
  halfsize = ksize / 2;

#pragma omp parallel private(r, c, i) if (omp_on)
{
  double buffer[height+ksize-1];

  #pragma omp for
  for (c = 0; c < width; c++) {
    for (i = 0; i < halfsize; i++)
      buffer[i] = image->get(0,c);
    for (i = 0; i < height; i++)
      buffer[halfsize + i] = image->get(i,c);
    for (i = 0; i < halfsize; i++)
      buffer[halfsize + height + i] = image->get(height - 1, c);

    ConvBufferFast(buffer, kernel, height, ksize);
    for (r = 0; r < height; r++)
      image->set(r,c,buffer[r]);
  }

} // end pragma

}






// Low pass filter based on Gaussian convolution
// Width of Gaussian kernel is given by the X and Y sigma parameters.
// A sigma of 0 means no low pass filtering (i.e., no convolution) in that direction
static void lowpassGauss(SimpleImage<double> *imageIn, SimpleImage<double> *&imageOut, double sigmaX, double sigmaY)
{

  // Some hard-coded values (should be parameterized eventually)
  int omp_on = 1;
  int nbSigmas = 3;

  int i;
  int ksizeX = 2*nbSigmas*sigmaX+1;
  int ksizeY = 2*nbSigmas*sigmaY+1;
  double x, kernelX[ksizeX], kernelY[ksizeY], sum = 0.0;

  // Fill in kernel values for X.
  if (sigmaX != 0.0) {
     for (i = 0; i < ksizeX; i++) {
        x = i - ksizeX / 2.0;
        kernelX[i] = exp(- x * x / (2.0 * sigmaX * sigmaX));
        sum += kernelX[i];
     }
     // Normalize kernel values to sum to 1.0.
     for (i = 0; i < ksizeX; i++)
        kernelX[i] /= sum;
  }

  if (sigmaY != 0.0) {
     sum = 0.0;
     // Fill in kernel values for Y.
     for (i = 0; i < ksizeY; i++) {
        x = i - ksizeY / 2;
        kernelY[i] = exp(- x * x / (2.0 * sigmaY * sigmaY));
        sum += kernelY[i];
     }
     // Normalize kernel values to sum to 1.0.
     for (i = 0; i < ksizeY; i++)
        kernelY[i] /= sum;
  } 


  // If imageOut and imageIn are the same, then it's an inplace
  // low pass filtering. Nothing to do.
  // Otherwise, make a copy of the original data into the output
  // location and filter in place. 
  // This is not optimum, but the original convolution functions
  // below are  inplace.
  if (imageIn != imageOut) {
     if (imageOut != NULL) {
        imageOut->free();
        delete imageOut;
     }
     imageOut = new SimpleImage<double>(imageIn->getNB(),
                                        imageIn->getNL(),
                                        imageIn->getNS());
     for (int k = 0; k<imageIn->getNB(); k++)
        for (int j = 0; j<imageIn->getNL(); j++)
           for (int i = 0; i<imageIn->getNS(); i++)
              imageOut->set(k, j, i, imageIn->get(k, j, i));

  }

  // If sigmas are zero, no low pass filtering. Check made here in
  // case output image is supplied
  if (sigmaX == 0.0 && sigmaY == 0.0)
     return;

  // Convolve image with gaussian kernel in the horizontal direction
  if (sigmaX != 0.0)
     ConvHorizontal(imageOut, kernelX, ksizeX, omp_on);

  // Convolve image with gaussian kernel in the vertical direction
  if (sigmaY != 0.0)
     ConvVertical(imageOut, kernelY, ksizeY, omp_on);

}




static void lowpassGauss(SimpleImage<double> *img_in,
		    SimpleImage<double> *&img_out, double sigma) 
{

   lowpassGauss(img_in, img_out, sigma, sigma);

}






// compute the profile of a gaussian curve.
// This will return a gaussian low pass kernel
//
std::vector<float> getGaussianProfile(float sigma, 
                                      int numSigmas, 
                                      int numSamplesPerSigma) {

   float maxRadius = sigma * numSigmas;
   float maxRadius2 = maxRadius * maxRadius;

   // Output container for the gaussian profile
   int numSamples = numSigmas * numSamplesPerSigma;
   std::vector<float> out(numSamples);
   std::iota(out.begin(), out.end(), 0);

   // Evalute gaussian
   float coef = 1.0 / (sqrt(2.0*PI) * sigma);
   float coefExp = - 1.0/(2.0 * pow(sigma, 2)) * maxRadius2 / numSamples;
   std::transform(out.begin(), out.end(), out.begin(), 
                  [coef, coefExp](float ind){return coef * exp(coefExp * ind);}); //warped on r^2

   return out;

}




// Elliptical Weighted Average resampler.
// Based on Heckbert, 1989
// "Fundamental of Texture Mapping and Resampling"
// The following version use the finite difference approach for speed
// improvement
SimpleImage<double> * ewa(SimpleImage<double> *imgIn,
                  int nbX, int nbY,
                  const std::valarray<float> &matX, 
                  const std::valarray<float> &matY,
                  const gaussProfile &gaussP,
                  const MatrixXf &U,
                  const VectorXf &S,
                  const MatrixXf &V ) {



   // Gather information on input and allocate output image
   int nsi = imgIn->getNS();
   int nli = imgIn->getNL();

   int ntot = matX.size();

   // Check input matrices size and output image size are the same
   if (ntot != (nbX*nbY)) {
      zvmessage("input mapping matrices and output image size are different!","");
      zabend();
   }

   // Allocate output image
   SimpleImage<double> * out = nullptr;
   out = new (std::nothrow) SimpleImage<double>(nbY, nbX);
   if (out == nullptr) {
      zvmessage("Output image allocation failed!","");
      zabend();
   }
   out->zero();


   // Construct affine transform matrix
   // A singular value <1 means that the image is to be oversampled in that
   // direction. In that case the corresponding gaussian kernel width is less 
   // than 1. This is in practice incorrect as it does not account for the
   // discrete-to-continuous kernel involved in the resampling process. To 
   // solve for that, we force the singular value to 1.
   VectorXf S2 = S;
   // Check that singular value are not less than 1. Otherwise that means 
   // oversampling in that direction. If that is the case, set to 1 to avoid
   // too skinny gaussian kernel that will slip through the pixels
   if (S2[0] < 1.0) S2[0] = 1.0;
   if (S2[1] < 1.0) S2[1] = 1.0;
   MatrixXf M(2,2);
   M = U * S2.asDiagonal() * V.transpose();


   // To define the resampling kernel shape in the image to resample, we 
   // transform a reconstruction kernel in the destination image (the Left image)
   // according to the mapping (L to R). The reconstruction kernel "support" in
   // the destination image is a circle whose radius is sigma*numSigmas. The
   // reconstruction kernel "profile" is a gaussian
   float kernelRadius = gaussP.sigma * gaussP.numSigmas;
   const std::vector<float> &gauss = gaussP.gauss;

   // The footprint of the resampling kernel is an ellipse whose function is
   // AX^2 + BXY + CY^2 = F
   // According to the main paper, A, B, C, and F are defined as:
   // Note that F is scale with the source circle radius as is it different 
   // from the paper assumption of 1
   float A = M(1,0) * M(1,0) + M(1,1) * M(1,1);
   float B = -2 * (M(0,0) * M(1,0) + M(0,1) * M(1,1));
   float C = M(0,0) * M(0,0) + M(0,1) * M(0,1);
   float F = pow(M(0,0) * M(1,1) - M(0,1)*M(1,0), 2) * pow(kernelRadius, 2); 


   // Check on the discriminant of the ellipsoid.
   // D = A*C - B^2/4
   // if D > 0 ellispoid -- > good
   // else something went bad. Should not happen!
   if ((A*C - B*B/4.) <= 0) {
      zvmessage("EWA - Ellipsoid discriminant <=0", "");
      return out;
   }  

   // Scale A, B, C, F equally such that F = num elements in gauss array.
   // This way, the value of AX^2 + BXY + CY^2 (= F) directly corresponds to
   // the index in the gauss array to get the kernel value
   int szGauss = gaussP.gauss.size();
   A *= szGauss / F;
   B *= szGauss / F;
   C *= szGauss / F;
   F  = szGauss;


   double * ptrOut = out->linePtr(0);

   // Get the largest singular value. Normally, SVD order singular values from
   // largest to smallest. However, this function might be used on the pseudo
   // inverse of the SVD in which case the order will be reversed
   float Smax = (S2[0] > S2[1]) ? S2[0] : S2[1];



   for (int i=0 ; i<ntot; i++) {

      double rsamp = matX[i];
      double rline = matY[i];


      // Check that x,y coordinates are not inf/nan/subnormal. If proper work is
      // done before, that shouldn't happen, but if "non-normal" values slip 
      // through, it's a segfault
      if (!std::isnormal(rsamp) || !std::isnormal(rline)) 
         continue;

      // if out of bound matrice values, move to next pixel. 
      if (rsamp < 0  || rsamp >= nsi || rline < 0 || rline >= nli) 
         continue;


      // Find box around the ellipse in image. It is defined using the 
      // singular values, as they express the "stetch" of the unit circle
      // in the source image into the destination image.
      // TODO for first implementation, we use the max singular value to
      // define a square box. This will be awfully inefficient if the 
      // transform is a long and skinny ellipse at 45 degrees. We'll have
      // a huge box to scan.
      int umin = floor(rsamp - Smax * kernelRadius);
      int umax = ceil(rsamp + Smax * kernelRadius);
      int vmin = floor(rline - Smax * kernelRadius);
      int vmax = ceil(rline + Smax * kernelRadius);


      // Main loop. Iteration over the box and either skip pixel if out of 
      // the ellipse footprint or get img value + gauss profile at that 
      // location and cumulate
      double num = 0;
      double den = 0;
      double DDQ = 2 * A;
      double U = umin - rsamp;
      
      //Out-of-loop computation
      double DA = A * (2 * U + 1); 
      double AU2 = A * U * U;

      for (int v = vmin; v <= vmax; v++) {

         if (v < 0 || v >= nli)
            continue;

         double V = v - rline;
         double DQ = DA + B * V;  //finite difference
         double Q = (C * V + B * U) * V + AU2;

         for (int u = umin; u <= umax; u++) {

            if ((Q < F) && (u >= 0) && (u < nsi)) { //inside ellipse and inside image   
               double gaussWeight = gauss[(long)(Q)];
               num += gaussWeight * imgIn->get(v,u);
               den += gaussWeight;
            }

            Q += DQ;
            DQ += DDQ;
         }
      } 

      if (den != 0) // Ellipse should cover at least a pixel. Should not be empty!
         ptrOut[i] = num/den;

   }

 

   return out;

}







////////////////////////////////////////////////////////////////////////
// Post-process step consisting in filtering out pixels that are deemed 
// outliers. The filter is a median-of-sort where, for each pixel, the
// consistency in the surrounding pixels is above a threshold. 
// The output is a binary mask flagging pixel as valid/nonvalid. Size of
// mask is the same as the correlation score / locX / locY images.
////////////////////////////////////////////////////////////////////////
void localCoherenceFilter(SimpleImage<short int> * corrScores,    
                          SimpleImage<double> * locX,     
                          SimpleImage<double> * locY, 
                          SimpleImage<double> * coefs_a, 
                          SimpleImage<double> * coefs_b, 
                          SimpleImage<double> * coefs_d, 
                          SimpleImage<double> * coefs_e, 
                          SimpleImage<double> * coefs_g, 
                          SimpleImage<double> * coefs_h, 
                          int corrNL,
                          int corrNS,
                          int statFilterExtent, 
                          double statFilterScalerThreshold, 
                          int statFilterThresholdN,
                          int omp_on,
                          SimpleImage<short> *& mask) { // output


   // Check the validity of mandatory inputs/outputs
   if (corrScores == nullptr || locX == nullptr || locY == nullptr) {
      zvmessage("Invalid mandatory input images for local coherence filter, returning...","");
      return;
   }

   // Get the size of the diparity maps to filter
   int ns = corrScores->getNS();
   int nl = corrScores->getNL();
   
   // Check validitiy of pointers to coefs_a(b)(d)(e)(g)(h)
   int coefs_a_valid = 1;
   if (coefs_a == nullptr) {
      coefs_a_valid = 0;
      coefs_a = new SimpleImage<double>(nl, ns);
      coefs_a->zero();
   }

   int coefs_b_valid = 1;
   if (coefs_b == nullptr) {
      coefs_b_valid = 0;
      coefs_b = new SimpleImage<double>(nl, ns);
      coefs_b->zero();
   }

   int coefs_d_valid = 1;
   if (coefs_d == nullptr) {
      coefs_d_valid = 0;
      coefs_d = new SimpleImage<double>(nl, ns);
      coefs_d->zero();
   }

   int coefs_e_valid = 1;
   if (coefs_e == nullptr) {
      coefs_e_valid = 0;
      coefs_e = new SimpleImage<double>(nl, ns);
      coefs_e->zero();
   }

   int coefs_g_valid = 1;
   if (coefs_g == nullptr) {
      coefs_g_valid = 0;
      coefs_g = new SimpleImage<double>(nl, ns);
      coefs_g->zero();
   }

   int coefs_h_valid = 1;
   if (coefs_h == nullptr) {
      coefs_h_valid = 0;
      coefs_h = new SimpleImage<double>(nl, ns);
      coefs_h->zero();
   }


   // Initialize the output mask
   mask = new SimpleImage<short>(nl, ns);
   mask->zero();

   // Loop over all the pixel of the disparity map and check the validity of the
   // disparity with respect to the disparity of the neigboring pixels
   #pragma omp parallel for if (omp_on)
   for (int loc = 0 ; loc < ns*nl; loc++) {

      int i = loc % ns;
      int j = loc / ns;

      // If current pixel is invalid, nothing to check, move to next one.
      if (corrScores->get(j, i) <= GOOD_CORR) 
         continue;

      // Get the transform coefficients of the queried pixel. This is needed to
      // "center" the disparity with respect to the queried one and to define 
      // the neighborood area to check for consistency
      double _coef_a = coefs_a->get(j,i);
      double _coef_b = coefs_b->get(j,i);
      double _coef_d = coefs_d->get(j,i);
      double _coef_e = coefs_e->get(j,i);
      double _coef_g = coefs_g->get(j,i);
      double _coef_h = coefs_h->get(j,i);


      // Compute the "area change" of the transform. This is an appoximation, 
      // obtained by computing the determinant of the linear transform (does
      // not account for the g/h factor). This gives an idea of the "scale" 
      // factor between the left and right image (it assumes same scaling in X 
      // and Y, and minimal g/h factors). Both the "neighborhood" and allowed 
      // "pixel distance" depend on it.
      // May be another option is to look at the nominal scale difference
      // between Left and Right only.
      double det = std::abs(_coef_a * _coef_e - _coef_d*_coef_b);

      // Neighborood size around the pixel of interest to check for consistency 
      int hly, hlx;  
      // and threshold on pixel distance to check for.
      float statFilterThreshold;
         

      // Determinant < 1 means that the left image is high resolution compared 
      // to the right. The correlation window has therefore been increased to 
      // keep number of actual pixels in both L and R same as nominal template 
      // size. The neighborood to check consistency for must be increased 
      // accordingly. Similarly the expected precision of the correlation is 
      // dependant on that scaling factor and the threshold must be accounted 
      // for as well.
      float scaler = 10; // Limit scaling to 10. Arbitrary for now
      if (det < 1) {
         if (det > 0.01)
            scaler = 1.f / sqrt(det);
         hlx = (int)(scaler * (corrNS - 1)/2) + statFilterExtent;
         hly = (int)(scaler * (corrNL - 1)/2) + statFilterExtent;
         statFilterThreshold = scaler * statFilterScalerThreshold;
      }
      else {
         // In that case, L low res compared to R, only the correlation 
         // precision must be scaled. The correlation window size was not 
         // affected.
         if (det < 100)
            scaler = sqrt(det);
         hlx = (corrNS - 1)/2 + statFilterExtent;
         hly = (corrNL - 1)/2 + statFilterExtent;
         //TODO Investigate if statFilterThreshold really need to be scaled
         //here
         statFilterThreshold = scaler * statFilterScalerThreshold;
      }

      // Number of pixel in the neighborood
      int nbN = (hly*2+1) * (hlx*2+1);         

      // Get the offset of the affine transform of the queried pixel
      // For now, g/h are not used. Not clear how to account for them
      float offsetX = locX->get(j, i) - (_coef_a*i + _coef_b*j);
      float offsetY = locY->get(j, i) - (_coef_d*i + _coef_e*j);
      //float offsetX = locX->get(j, i) - (_coef_a*i + _coef_b*j + _coef_g*i*j);
      //float offsetY = locY->get(j, i) - (_coef_d*i + _coef_e*j + _coef_h*i*j);
   
      // Compute difference in disparity for neighboring pixels and queried one
      std::valarray<float> arrX((std::numeric_limits<float>::max)(), nbN);
      std::valarray<float> arrY((std::numeric_limits<float>::max)(), nbN);
      unsigned int pos = 0;
      for (int l = j-hly; l < j+hly+1; l++) {
         for (int s = i-hlx; s < i+hlx+1; s++) {
            if (l>=0 && l<nl && s>=0 && s<ns) {
               arrX[pos] = _coef_a*s + _coef_b*l + offsetX - locX->get(l,s);
               arrY[pos] = _coef_d*s + _coef_e*l + offsetY - locY->get(l,s);
               //arrX[pos] = _coef_a*s + _coef_b*l + _coef_g*s*l + offsetX - locX->get(l,s);
               //arrY[pos] = _coef_d*s + _coef_e*l + _coef_h*s*l + offsetY - locY->get(l,s);
            }
            pos++;
         }
      }

      // Threshold on the number of pixels in the neigborood that need to have a
      // disparity "similar" to the queried pixel
      float thresholdN = (int)((nbN * statFilterThresholdN)/100.0) ;

      // Abs value as we're only interested in the "distance" between pixels 
      arrX = std::abs(arrX);
      arrY = std::abs(arrY);

      // Count the number of pixels whose "distance" in column direction w.r.t.
      // queried one is less than threshold 
      std::valarray<bool> distX = arrX < statFilterThreshold;

      // Are there enough of them?
      if (std::count(std::begin(distX), std::end(distX), true) < thresholdN)
         continue;

      // Count the number of pixels whose "distance" in row direction w.r.t 
      // queried one is less than threshold 
      std::valarray<bool> distY = arrY < statFilterThreshold;
      // Are there enough of them?
      if (std::count(std::begin(distY), std::end(distY), true) < thresholdN)
         continue;

      // A valid pixel must have both column and row "distance" thresholds
      // satisfied
      std::valarray<bool> both = distX * distY;
      int totalGood = std::count(std::begin(both), std::end(both), true);
      // Are there enough of them?
      if (totalGood < thresholdN)
         continue;

      // Queried pixel seems good, keep it
      mask->set(j, i, 1);

   }

   
   // Deallocate coef images if necessary
   if (!coefs_a_valid) {
      coefs_a->free();
      delete coefs_a;
   }

   if (!coefs_b_valid){
      coefs_b->free();
      delete coefs_b;
   }

   if (!coefs_d_valid){
      coefs_d->free();
      delete coefs_d;
   }

   if (!coefs_e_valid){
      coefs_e->free();
      delete coefs_e;
   }

   if (!coefs_g_valid){
      coefs_g->free();
      delete coefs_g;
   }

   if (!coefs_h_valid){
      coefs_h->free();
      delete coefs_h;
   }

   return;
}


////////////////////////////////////////////////////////////////////////////
// Apply a binary mask to an input file. Usually this should be called after
// the previous function where the mask is defined
////////////////////////////////////////////////////////////////////////////


void applyLocalCoherenceFilter(SimpleImage<double> * input, 
                               SimpleImage<short> * mask)  {
                              
   applyLocalCoherenceFilter<double>(input, mask, 0);
}

template<class T>
void applyLocalCoherenceFilter(SimpleImage<T> * input, 
                               SimpleImage<short> * mask,
                               T value) {

   if (mask == nullptr) {
      zvmessage("mask is NULL. Can't apply local coherence filter","");
      return;
   }

   int nsl = mask->getNS();
   int nll = mask->getNL();

   // Mask input image with value according to the mask.
   for (int j=0 ; j<nll; j++) {
      for (int i=0; i<nsl; i++) {
         if(mask->get(j, i) == 0) {
            input->set(j, i, value);
         }
      }
   }

}


////////////////////////////////////////////////////////////////////////////
// Read an image tiling labels and constructs a tiling map corresponding to 
// the current image downsampling residual. Output tiling image is the same
// size as the input image.
////////////////////////////////////////////////////////////////////////////
int getTilingInformation(int unit, SimpleImage<int> *& tI, 
                         std::set<int> &tilingLevel) {

   int status;
   int sameNbElements = 1;


   ////////////////////////////////////////////////////////////////////////////
   // Extract relevant information about tiling parameters from the image label
   ////////////////////////////////////////////////////////////////////////////


   // Pixel averaging in X
   int pixelAvgWidth = 1; //default
   status = zlget(unit, "PROPERTY", "PIXEL_AVERAGING_WIDTH", &pixelAvgWidth,
                  "FORMAT", "INT", "ERR_ACT", "", "PROPERTY", 
                  "INSTRUMENT_STATE_PARMS", NULL);
   if (status <= 0) 
      return status;


   // Pixel averaging in Y
   int pixelAvgHeight = 1; //default
   status = zlget(unit, "PROPERTY", "PIXEL_AVERAGING_HEIGHT", &pixelAvgHeight,
                  "FORMAT", "INT", "ERR_ACT", "", "PROPERTY", 
                  "INSTRUMENT_STATE_PARMS", NULL);
   if (status <= 0) 
      return status;

   
   // Downsampling factor in X of each tiles
   int downsamplingX[20]; //max should be 16 per M2020 Engineering cam spec
   int nret = 0; // number of returned elements
   status = zlget(unit, "PROPERTY", "TILE_DOWNSAMPLE_X", downsamplingX,
                  "FORMAT", "INT", "ERR_ACT", "", "NELEMENT", -1, "NRET", &nret,
                  "PROPERTY", "INSTRUMENT_STATE_PARMS", NULL);
   if (status <= 0) 
      return status;

   if (nret == 0) {
      zvmessage("TILE_DOWNSAMPLE_X label parameter returned 0 elements","");
      return 0;
   }

   
   // Downsampling factor in X of each tiles
   int downsamplingY[20]; //max should be 16 per M2020 Engineering cam spec
   int nret2 = 0; // number of returned elements
   status = zlget(unit, "PROPERTY", "TILE_DOWNSAMPLE_Y", downsamplingY,
                  "FORMAT", "INT", "ERR_ACT", "", "NELEMENT", -1, "NRET", &nret2,
                  "PROPERTY", "INSTRUMENT_STATE_PARMS", NULL);
   if (status <= 0) 
      return status;

   if (nret != nret2) 
      sameNbElements = 0;


   // Starting position of each tile in X direction
   int startX[20]; //max should be 16 per M2020 Engineering cam spec
   status = zlget(unit, "PROPERTY", "TILE_FIRST_LINE_SAMPLE", startX,
                  "FORMAT", "INT", "ERR_ACT", "", "NELEMENT", -1, "NRET", &nret2,
                  "PROPERTY", "INSTRUMENT_STATE_PARMS", NULL);
   if (status <= 0) 
      return status;

   if (nret != nret2) 
      sameNbElements = 0;



   // Starting position of each tile in Y direction
   int startY[20]; //max should be 16 per M2020 Engineering cam spec
   status = zlget(unit, "PROPERTY", "TILE_FIRST_LINE", startY,
                  "FORMAT", "INT", "ERR_ACT", "", "NELEMENT", -1, "NRET", &nret2,
                  "PROPERTY", "INSTRUMENT_STATE_PARMS", NULL);
   if (status <= 0) 
      return status;

   if (nret != nret2) 
      sameNbElements = 0;


   // Number of samples of each tile
   int nbX[20]; //max should be 16 per M2020 Engineering cam spec
   status = zlget(unit, "PROPERTY", "TILE_NUM_LINE_SAMPLES", nbX,
                  "FORMAT", "INT", "ERR_ACT", "", "NELEMENT", -1, "NRET", &nret2,
                  "PROPERTY", "INSTRUMENT_STATE_PARMS", NULL);
   if (status <= 0) 
      return status;

   if (nret != nret2) 
      sameNbElements = 0;



   // Number of lines of each tile
   int nbY[20]; //max should be 16 per M2020 Engineering cam spec
   status = zlget(unit, "PROPERTY", "TILE_NUM_LINES", nbY,
                  "FORMAT", "INT", "ERR_ACT", "", "NELEMENT", -1, "NRET", &nret2,
                  "PROPERTY", "INSTRUMENT_STATE_PARMS", NULL);
   if (status <= 0) 
      return status;

   if (nret != nret2) 
      sameNbElements = 0;


   // If there is a discrepency between the number of tiles elements read from
   // the file, notify, and exit
   if (!sameNbElements) {
      zvmessage("Different number of elements read for TILE_DOWNSAMPLE_X","");
      zvmessage("or TILE_DOWNSAMPLE_Y or TILE_FIRST_LINE or TILE_FIRST_LINE_SAMPLE","");
      zvmessage("or TILE_NUM_LINES or TILE_NUM_LINE_SAMPLES","");
      return 0;
   }


   ////////////////////////////////////////////////////////////////////////////
   // Construct tile map for the input image
   ////////////////////////////////////////////////////////////////////////////


   // Tiles of different downsampling factor can overlap. For instance, a x4
   // downsampling tile can cover all the image, while other x2 tiles cover 
   // only part of it. x2 has precedence over x4 (we keep the highest 
   // resolution). The output will be x4 where there is no coverage with x2 
   // tiles, and x2 otherwise.
   // To achieve this, we fill the output downsampling factor image starting 
   // with the coarser tiles, down to the higher resolution ones.

   // Get the smaller average pixel size. Most of the time it will be the same
   int minAvg = pixelAvgHeight > pixelAvgWidth ? pixelAvgWidth : pixelAvgHeight; 


   // Generate a list of pair relating position in the label array (i.e., tile 
   // index) and downsampling factor. This will be used to order the tiles by
   // downsampling factor.
   std::vector<std::pair<int, int> > dsVect;
   for (int i=0; i<nret; i++)
      dsVect.push_back(std::make_pair(i,downsamplingX[i] > downsamplingY[i] ? 
                                        downsamplingX[i] : downsamplingY[i]));

   // Sort from coarser to higher resolution
   std::sort(dsVect.begin(), dsVect.end(),
             [](const std::pair<int,int> & d1, const std::pair<int,int> & d2)
                                               {return d1.second > d2.second;});


   // Image a crop of the full frame. Need to identify the line/sample of the
   // most top-left tile used in this image. This will provide the general
   // line/sample offset to apply when filling the tile map
   int minX = startX[0];
   for (int i=0; i<nret; i++) 
      if (startX[i] < minX) minX = startX[i];

   int minY = startY[0];
   for (int i=0; i<nret; i++) 
      if (startY[i] < minY) minY = startY[i];


   // Tiles origin and number of lines are expressed in the full 
   // resolution frame (raw frame). Scale the values with the average 
   // downsampling and offset with crop origin
   for (int i=0; i<nret; i++) startX[i] = (startX[i] - minX) / minAvg;
   for (int i=0; i<nret; i++) nbX[i] /= minAvg;
   for (int i=0; i<nret; i++) startY[i] = (startY[i] - minY) / minAvg;
   for (int i=0; i<nret; i++) nbY[i] /= minAvg;


   // Go over each tile and fill in the output
   for (const auto &p: dsVect) {
    
      // Get position in label output array and downsampling factor 
      int pos = p.first; 
      int dsFactor = pow(2, log2(p.second) - log2(minAvg));

      // If dsFactor < 1, that means that the averaging of pixel is larger than
      // the downsampling of the tiles. This would be equivalent of a simple
      // box or linear downsampling. dsFactor should be set to 1 as the 
      // frequency resolution is the one expected from the spatial resolution.
      if (dsFactor < 1)
         dsFactor = 1;
     
      // Save current tiling level in output list
      tilingLevel.insert(dsFactor);

      // Update the output tile accordingly to current tile
      for (int l = startY[pos]; l < startY[pos] + nbY[pos]; l++) 
         for (int s = startX[pos]; s < startX[pos] + nbX[pos]; s++) 
            tI->set(l,s,dsFactor);
   } 

   return 1;
}


template<class T>
void saveImg(const std::string name, SimpleImage<T> * img) {

   int unit_out;

   const char *cstr = name.c_str();
   zvunit(&unit_out, "", 1, "u_name", cstr, NULL);
   zvopen(unit_out, "OP", "WRITE", "U_FORMAT", img->getVicarTypeString(), 
          "O_FORMAT", img->getVicarTypeString(), "U_NS", img->getNS(), "U_NL", 
          img->getNL(), "OPEN_ACT", "AS", "U_NB", img->getNB(), "U_ORG", "BSQ",
          NULL);
   zvplabel(unit_out, 0, 1);
   if (img->getNB() > 1)
      for (int i=0; i<img->getNB(); i++)
         for (int j=0; j<img->getNL(); j++)
            zvwrit(unit_out, img->linePtr(i,j), "LINE", j+1, "BAND", i+1,NULL);
   else
      for (int j=0; j<img->getNL(); j++)
         zvwrit(unit_out, img->linePtr(j), "LINE", j+1, NULL);
   zvclose(unit_out, "CLOS_ACT", "FREE", NULL);
}

