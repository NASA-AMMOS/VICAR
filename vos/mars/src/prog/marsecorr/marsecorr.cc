#include "vicmain_c"

#include "mars_support.h"

#include "PigMission.h"
#include "PigFileModel.h"
#include "PigLabelModel.h"
#include "PigCameraModel.h"
#include "PigPointingModel.h"
#include "PigSurfaceModel.h"

#ifdef _OPENMP
#include <omp.h>
#endif

#include "xvector.h"
#include <iostream>
#include <iomanip> //testing
#include <fstream>
#include "SimpleImage.h"
#include <cmath>
#include <vector>
#include <set>
#include <map>
#include <valarray>
#include <array>
#include <limits>
#include <numeric>
#include <chrono> 
#include <random> 
#include <Eigen/Dense>
#include <Eigen/SVD>


#include "meshman.h"

// For output parameter
#include "return_status.h"
#include "taeconf.inp"
#include "parblk.inc"
#include "pgminc.inc"

// buffer sizes in main program - these should be made dynamic, eventually!
#define MAX_INPUTS 2
#define MAX_NL MARS_MAX_NL
#define MAX_NS MARS_MAX_NS

// Maximum of DN values to filter out
#define MAX_DNS 200

#define EPSILON 0.00000001 
#define	PI 3.14159265358979323846


using namespace std::chrono;  
using namespace Eigen;




// For vicar output variable (correlation score)
extern "C" {
    int q_real(struct PARBLK *p, char *name, int count, double *real, int mode);
    int q_init(struct PARBLK *p, int pool_size, int mode);
}



// Tile definition
// A tile is a subset of the Left image for which the pinhole model is assumed
struct tile {
   int xs;    // tile start X (top-left of tile) 
   int ys;    // tile start Y
   int xe;    // tile end X (bottom right of tile)
   int ye;    // tile end Y
   int xc;    // tile center X
   int yc;    // tile center Y
   float radius; // Dist in pixel between tile center (xc, yc) and image center

   // Tile center pixel look angle
   PigVector look;


   // Raw ranges. Estimates of depths of planes prior to refining them to move 
   // "along" the epipolar line with a given step and verifying that ranges ends
   // (i.e., XYZ point) is in both cam FOV. These ranges are usually the same as
   // the rawRangesPrior, except in case where the progressive approach is used 
   // (neighbor to neighbor range seeding) in which case the rawRanges are 
   // updated with values from the neighbors.
   std::vector<float> rawRanges;

   // Container for storing the tile neighbors, ie, the indices of the tiles
   // that are direct neighbor to this one (above, below, left, right)
   std::set<int> N;

   // Container for storing the seeds(s) that led to the unsuccessfull 
   // processing of the current tile:
   // -1: full_sweep or prior was used
   // [set]: tile indices that were used to seed the unsuccessfull processing of
   //         the tile. Can only be direct neighbors. 
   std::set<int> An;

   // List of neighbor tiles to be used for seeding the current tile.
   std::set<int> B;

   // Plane normal onto which project the Right image then back to Left image 
   PigVector planeNorm;
 
   // List of plane normals
   std::vector<PigVector> planeNormArr;

   // Depth of planes for projecting Right to Left once refined from rawRanges
   // list
   std::vector<float> ranges; 

   // X(pix) of projected tile center on plane in R (for each ranges). Can have
   // a max of 4 epipolar "segments" for fisheye et al. types 
   std::vector<float> epiX;  

   // Same for Y(pix)
   std::vector<float> epiY;

   // Ground Sampling Distance (GSD) ratio between the tile center pixel 
   // (xc, yc) and the image center. For linear camera that ratio is about
   // constant (~1). For non-linear it will vary depending on the 
   // distance between (xc, yc) to the image center  
   //float relResFromCenter;   
                            
   // GSD ratio between the Left current tile center (xc, yc) and the Right
   // epipolar point for a given range. 
   //std::vector<float> relResFromStereo; 

   // Tiling factor of the (Left) tile center 
   int tilingL;

   // Tiling factor of the (Right) pixel corresponding for a given range
   std::vector<int> tilingRs;

   // Homography transform coefficients between the L and R at given range
   std::vector<std::array<float, 9> > H; 

   // Affine transform coefficients between L and R at given range
   std::vector< std::array<float, 6> > A;

   //Perpendicular-epipolar direction of tile center between ranges
   //min/max. If cam cahvore this is not correct, but a good approximation
   //if min/max ranges are not too far apart
   float perpEpi[2];

};  


// A tiler is a container that contains the list of tiles and some general
// information about the tiled image as well as on the processing of these tiles
struct tiler {

   // List of tiles  
   std::vector<tile> tiles;

   // Number of samples of the entire image
   int nsl;

   // Number of lines of the entire image
   int nll;

   // Total number of tiles
   unsigned int nbTiles;  // == tiles.size()

   // Number of tiles in the X direction
   unsigned int nbTilesX;

   // Number of tiles in the Y direction
   unsigned int nbTilesY;

};



// Tie points definition. Just a pairing of 2 pixels between L and R images
struct tiePoint{float xL; float yL; float xR; float yR;};


// Container related to ranges bracket definition and seeding. Mainly for ease
// of moving this information around
struct planeSpace{int rangeSampling; double rangeStep; 
                  float minRange; float maxRange;
                  PigVector planeNormal;
                  int multiPlane; int numTilts;};

// Container related to camera model, image and precomputed values. Mainly for 
// ease of moving the information around
struct camGeometry{PigCameraModel * cm;
                   PigCoordSystem * cs;
                   int ns; int nl;
                   PigPoint pos;
                   PigPoint look;
                   // How close can be a XYZ to optical center - mainly used
                   // for Right image
                   float closenessLimit;
                   float minCosine;
                   float GSDcenterAt1m;
                   std::vector<double> relativeGSD;
                   SimpleImage<int> *downsamplingTile;};

// Container related to the Gaussian profile used to prefilter+sample an
// image using the Ellipsoid Weighted Averag (ewa)
struct gaussProfile{float sigma; int numSigmas; int numSamplesPerSigma;
                    std::vector<float> gauss;};




// Functions

void printScales(const std::set<float> &scaleSet); 

void printProgress(int nbTiles = 0);


// Tiler functions

tiler initializeTiler(camGeometry & leftC, int nll, int nsl, 
                      int tileSize[2], int corrSize[2]);

std::set<int> getNeighbors(int index, int nbTilesX, int nbTilesY);



template<class T1, class T2>
float getRadius(T1 x, T1 y, T2 nbX, T2 nbY);

std::vector<tiePoint> getTies(camGeometry & leftC, camGeometry & rightC, 
                              tile &tile, const int &gridX, const int &gridY, 
                              const float &range, const float &minIncidence);

void getDepthList(camGeometry &leftC,
                  camGeometry &rightC,
                  const float xLeft,
                  const float yLeft,
                  const int stepEpipolar,
                  const std::vector<float> rawRanges,
                  std::array<std::vector<float>,4>& ranges,    // output
                  std::array<std::vector<float>,4>& epiPixX,   // output
                  std::array<std::vector<float>,4>& epiPixY);   // output

void getTileDepths(camGeometry & leftC,
                   camGeometry & rightC,
                   const int stepEpipolar,
                   tile &tile);// output: tile.ranges


void computeRelativeGSDfromCenterPixel(camGeometry &camG);

void getTilingPerRange (camGeometry & leftC, camGeometry & rightC, tile &t);

int segmentFrameIntersect(const float &xmin, const float &ymin, 
                          const float &xmax, const float &ymax,
                          const float &x0, const float &y0, 
                          const float &x1, const float &y1,
                          float &xout0, float &yout0, 
                          float &xout1, float &yout1) ;


void getMinMaxLookAngle(PigCameraModel *cm, int ns, int nl, PigCoordSystem *cs, 
                         float & minCosine, float &maxCosine);


// Left image tiling utilities

void getAndCheckTileSize(int tileSize[2], int nll, int nsl, int corrSz[2]);

SimpleImage<float> * getTile(SimpleImage<float> * imgIn,
                             const int xstart,
                             const int ystart,
                             const int nbX,
                             const int nbY);


// Projection plane definition (Range and normal) functions

void getRangeNormalPrior(tiler &tiler, camGeometry &leftC,
                         const planeSpace &planeInfo);

void getRangeFromPyr(tiler &tiler, camGeometry &leftC,
                     const planeSpace &planeInfo, 
                     SimpleImage<float> *ranges,
                     SimpleImage<PigVector> * normals);

std::vector<float> getRangeFromDefault(const planeSpace &planeInfo);

std::vector<float> getRangeFromDefault(const int samplingMode, 
                                       const double samplingStep, 
                                       const float minRange,
                                       const float maxRange);

void getRangeAndNormalFromMesh(MeshMan * mesh, PigCameraModel * cam, tile & tile,
                               int multiplane);

void getRangeFromFile(SimpleImage<float> * rangeImg, const tile &tile);

void getNormalFromFile(SimpleImage<float> * normalImg, const tile &tile, 
                       int multiplane);

//void interpolateRangeAndNormal(std::vector<tile> &tiles, RBFMode mode); 

void getPlaneOrientationList(const PigVector look, const int numTilts,
                             std::vector<PigVector> &outList) ;

void updateEpiShift(tiler &tiler, camGeometry &leftC, camGeometry &rightC, 
                    SimpleImage<float> *epiOffX, SimpleImage<float> *epiOffY);

void adjustPlaneNorm(camGeometry &camG, const float minHitAngle, tile &tile);



// NCC using integral image

inline double sumIntegralImage(SimpleImage<double>* img, 
                               int sX, int sY, int nbX, int nbY);

template<class T>
void computeIntegralImage(SimpleImage<T> *imgIn,
                          SimpleImage<double> *& imgOut);

void integralImageCorrelation( SimpleImage<float> * tL,
                               SimpleImage<float> * tR,
                               int corrX, int corrY,
                               int searchX, int searchY,
                               int searchStepX, int searchStepY,
                               SimpleImage<float> *& scores,
                               SimpleImage<int> *& locX, 
                               SimpleImage<int> *& locY);


// Homography/Affine transform utilities

int computeAffineTransform(const std::vector<tiePoint> & ties, 
                           std::array<float, 6> & outA);

int computeHomography(const std::vector<tiePoint> & ties, 
                      std::array<float, 9> & H);

void applyHomography(const std::array<float,9> &H, 
                     const std::valarray<float> &xarr,
                     const std::valarray<float> &yarr,
                     std::valarray<float> &matX,
                     std::valarray<float> &matY);

void applyAffine(const std::array<float,6> &A, 
                 const std::valarray<float> &xarr,
                 const std::valarray<float> &yarr,
                 std::valarray<float> &matX,
                 std::valarray<float> &matY);

void computeRtoLprojectionMatrices(const std::array<float,9> &H, 
                                   const int &xtlR, const int &ytlR,
                                   const int &xbrR, const int &ybrR,
                                   std::valarray<float> &matX,
                                   std::valarray<float> &matY);

// Low-pass filtering and interpolation utilities
 
std::vector<float> getGaussianProfile(float sigma, int numSigmas, 
                                      int numSamplesPerSigma); 

SimpleImage<float> * ewa(SimpleImage<float> *imgIn,
                  int nbX, int nbY,
                  const std::valarray<float> &matX, 
                  const std::valarray<float> &matY,
                  const gaussProfile &gaussP,
                  const MatrixXf &U,
                  const VectorXf &S,
                  const MatrixXf &V );

void test_ewa(std::string outName, SimpleImage<float> *img, 
              float angle, float sX, float sY, 
              float sigma, int numSigmas) ;



void ConvBufferFast(float *buffer, float *kernel, int rsize, int ksize);
void ConvHorizontal(SimpleImage<float> *image, float *kernel, int ksize, int omp);
void ConvVertical(SimpleImage<float> *image, float *kernel, int ksize, int omp);
void GaussianBlur(SimpleImage<float> *image, float sigmaX, float sigmaY, int omp);

float getValidBlurLimit(const int corrLargeSide, const int imgSmallSide, 
                        const float userLimit, const float resFactor);

void blurImage(SimpleImage<float> *imgIn, 
               std::vector<SimpleImage<float>*> &imgArr, 
               std::set<float> &resRatio, 
               int numScale,
               float minRes, 
               float maxRes);

inline int getImageIndex(std::set<float> &resSet, float res);

SimpleImage<float> * resampleImageBicubic(SimpleImage<float> *img, 
                                          int nbX, int nbY, 
                                          std::valarray<float> &matX, 
                                          std::valarray<float> &matY); 

void downsample(SimpleImage<float> *imgIn, SimpleImage<float> *&imgOut,
                const int &downFactor, int omp);

void downsampleTiling(SimpleImage<int> *tile_in, SimpleImage<int> *&tile_out,
                      const int &downFactor);


// Output correlation filtering (post-process filter) functions

int simpleCorrelationFilter(SimpleImage<float> * corrScores, 
                            SimpleImage<float> * locX, 
                            SimpleImage<float> * locY, 
                            int nlr, int nsr,
                            int minScore);


void localCoherenceFilter(SimpleImage<float> * corrScores,    
                          SimpleImage<float> * locX,     
                          SimpleImage<float> * locY, 
                          SimpleImage<double> * coefs_a, 
                          SimpleImage<double> * coefs_b, 
                          SimpleImage<double> * coefs_d, 
                          SimpleImage<double> * coefs_e, 
                          int corrSz[2],
                          int statFilterExtent, 
                          float statFilterScalerThreshold, 
                          int statFilterThresholdN,
                          int omp_on,
                          SimpleImage<int> *& mask);

void applyLocalCoherenceFilter(SimpleImage<float> * input, 
                               SimpleImage<int> * mask); 
template<class T>
void applyLocalCoherenceFilter(SimpleImage<T> * input, 
                               SimpleImage<int> * mask, 
                               T value);


int getTilingInformation(PigFileModel * file_model, SimpleImage<int> *& tI, 
                         std::set<int> &tilingLevel);

template<class T>
SimpleImage<int> * getMask(SimpleImage<T> *&img, std::vector<double> &maskDN,
                           int maskExtentX, int maskExtentY);


// Loading/saving  vicar images functions

SimpleImage<float> * loadImage(PigFileModel * file_model, int band = 1); 

static void read_image(char *param, SimpleImage<float> *&image);

template<class T>
void saveImg(const std::string name, SimpleImage<T> * img);


// Drawing function

void drawEpipolarPoints(SimpleImage<float> *&leftImage,
                        SimpleImage<float> *&rightImage,
                        camGeometry &leftC,
                        camGeometry &rightC,
                        const planeSpace &planeInfo, 
                        const int epiStep, 
                        const std::vector<int> &Xarr,
                        const std::vector<int> &Yarr,
                        const std::string &fileName);







////////////////////////////////////////////////////////////////////////

void main44()
{

   int status, count, omp_on;
   int nids, corrSz[2], search, outCoefs, outQuality, rangeSampling, 
       apply_filter, epiStep, pyrlevel, downFactor, tileSize[2],// numScales,
       gridTile[2], statFilter, statFilterExtent, 
       statFilterThresholdN, bands[2], nbCorrValid; 
   double filter_window_l, filter_window_r, overlapCheck, avgScale;
   float minRange, maxRange, rangeStep, minScore, minScore2, limitCosine,// angleMax,
         maxResRatioUser, statFilterScalerThreshold, rightClosenessLimit, 
         minHitAngle;
   char msg[150], mission[64], instrument[64];

   SimpleImage<float> corrScores, locX, locY;
   SimpleImage<float> *imgL, *imgR;
   SimpleImage<double> *coefs_a, *coefs_b, *coefs_d, *coefs_e, *coefs_g, *coefs_h;

   PigPoint centerRight, centerLeft;
   PigVector oriR, oriL;
   PigSurfaceModel *surface_model;



   struct PARBLK out_parb;

   // Inputs

   PigFileModel *file_models[MAX_INPUTS];
   PigCameraModel *camera_in[MAX_INPUTS];
   PigPointingModel *pointing_in[MAX_INPUTS];
   int homogeneous_inputs = TRUE;
   RadiometryModel *radiometric[MAX_INPUTS];
   PigCoordSystem *output_cs;


   // Timer stamp
   auto t1 = high_resolution_clock::now();
   

   // User Parameters

   zvmessage("MARSECORR version 2022-10-12", "");

   // Get the input file name, and set up initial camera/pointing model
   // for it.  Although we accept only single file input, mars_setup
   // does lots of other nice things for us.

   mars_setup(nids, file_models, camera_in, pointing_in, surface_model,
              radiometric, output_cs, mission, instrument,homogeneous_inputs,
              MAX_NL, MAX_NS, MAX_INPUTS);


   // Check that we have only two files (L and R)
   if (nids != 2) {
      zvmessage("Two files are expected (Left and Right)","");
      zabend();
   }

   if (file_models[0] == nullptr || file_models[1] == nullptr) {
      zvmessage("Necessary file models could not be created.","");
      zabend();
   }


   // Retrieve maximum closeness limit for the right image. That is, when 
   // sampling different ranges, the XYZ point can end up being very close
   // to the Right camera center (even behind, but that is checked in the code)
   // This parameter sets a minimum distance, below which the range is not
   // considered. This is to avoid divergence for large FOV camera when 
   // computing XYZ-to-LS.
   // This should eventually be a camera model param and not a program param.
   zvp("RIGHT_MAX_DIST", &rightClosenessLimit, &count);

   // Retrieve downsampling factor for sampling the epipolar curve
   zvp("EPI_STEP", &epiStep, &count);

   // Retrieve first pass range sampling. A first pass of projecting L image
   // samples out in XYZ world then backprojecting them in R image is done
   // at a limited number of ranges. There are two possible ways to sample the
   // range: Linear or power sampling
   rangeSampling = zvptst("LINEAR");

   // Get the sampling parameter.
   if (rangeSampling) {
      float tmp;
      zvp("LINEAR_STEP", &tmp, &count);
      rangeStep = (double)tmp;
   }
   else {
      int tmp;
      zvp("POWER_STEP", &tmp, &count);
      rangeStep = (double)tmp;
   }

   // Retrieve range bracket
   zvp("MIN_RANGE", &minRange, &count);
   zvp("MAX_RANGE", &maxRange, &count);



   // Get the size of the correlation window
   zvp("TEMPLATE", corrSz, &count);             // Size of area to correlate
   if (count == 1)
       corrSz[1] = corrSz[0];                   // Make it square if needed
   if ((corrSz[0] % 2) == 0)                    // make sure it's odd-sized
       corrSz[0] += 1;
   if ((corrSz[1] % 2) == 0)
       corrSz[1] += 1; 
   
 
   // Get the search size (nb pixel) along the perpendicular-epipolar curve to
   // search for best correlation. This is to account for camera model error.
   zvp("SEARCH", &search, &count);

 
   // Do we need to save the affine parameters?
   count = 0;
   outCoefs = 0;
   zvpcnt("OUT_COEFS", &count);
   if (count != 0)
      outCoefs = 1;

   // Do we need to save the correlation score?
   count = 0;
   outQuality = 0;
   zvpcnt("OUT_QUALITY", &count);
   if (count != 0)
      outQuality = 1;

  
   // Is there a minimum acceptable score? 
   zvp("SCORE_MIN", &minScore, &count);
   if (count == 0)
      minScore = 0.0;
   minScore2 = minScore * minScore;


   // Pyramid level?
   zvp( "PYRLEVEL", &pyrlevel, &count); 
   downFactor = 1;
   if (pyrlevel)
      downFactor = pow(2, pyrlevel);

   // Number of low-pass filtering scales per octave. An octave is a change of 
   // scale by a factor of 2.
   //zvp("NUM_SCALES", &numScales, &count);
   

   // Are the tiles gridded to define the homography?
   // These numbers are the approximate number of samples to take in row and
   // col. Minimum to define a homography is 4 point, hence minimum for X and Y
   // is 2, which boils down to (approximately) the 4 corners
   zvp("GRID_TILE", gridTile, &count); 
   if (count == 1) {
      if (gridTile[0] < 2) 
         gridTile[0] = 2;
      gridTile[1] = gridTile[0];
   }
   else {
      if (gridTile[0] < 2) gridTile[0]=2; 
      if (gridTile[1] < 2) gridTile[1]=2; 
   }


   // Does user want to activate the EWA resampler? It can be used on both L 
   // and R, or one or the other, or on none. This choice is mostly because
   // it's a new feature and the actual effect on correlation quality is not
   // yet proven. Theoretically, this is the best approach, but it practice
   // it might have limited improvement (if not detrimental)
   int ewaL = 0;
   int ewaR = 0;
   if (zvptst("EWA_BOTH"))
      ewaL = ewaR = 1;
   else if (zvptst("EWA_LEFT"))
      ewaL = 1;
   else if (zvptst("EWA_RIGHT"))
      ewaR = 1;
   else {}

   // Get the threshold on the singular value of the affine transform between
   // Left and Right above which the EWA resampler will be used. If below,
   // a simpler bicubic interpolator is used
   float ewaT, ewaTi;
   zvp("EWA_THRESHOLD", &ewaT, &count);
   if (ewaT < 1) ewaT = 1;
   ewaTi = 1.0/ewaT; // Inverse for the other image. 


   // Generate the lookup table on the Gaussian Profile 
   // for EWA resampling
   float kewa_sigma = 0.5;
   zvp("KEWA_SIGMA", &kewa_sigma, &count);
   int kewa_nsigma = 3;
   zvp("KEWA_NSIGMA", &kewa_nsigma, &count);
   int kewa_nsamples = 10000;
   zvp("KEWA_NSAMPLES", &kewa_nsamples, &count);
   gaussProfile gaussP;
   gaussP.sigma = kewa_sigma;
   gaussP.numSigmas = kewa_nsigma;
   gaussP.numSamplesPerSigma = kewa_nsamples;
   gaussP.gauss = getGaussianProfile(gaussP.sigma, gaussP.numSigmas, 
                                      gaussP.numSamplesPerSigma);





   // Did the user provided a plane to project the image onto? If not, then 
   // each tile will project on a plane perpendicular to the look vector of the
   // tile center (fronto-looking plane)
   float planeNorm[3];
   int userPlaneNorm = 0;
   planeNorm[0] = planeNorm[1] = planeNorm[2] = 0;
   zvp("PLANENORM", planeNorm, &userPlaneNorm);  
   if (userPlaneNorm != 0 && userPlaneNorm != 3) {
      zvmessage("PLANENORM must be a 3-element vector","");
      zabend();
   }

   // Did the user activate the multi-plane option. In a multi-plane situation,
   // instead of projecting the image on a unique plane, it is projected on a 
   // list of planes with different orientations. 
   int userMultiPlane = 0;
   userMultiPlane = zvptst("MULTI_PLANE");

   // Get number of tilts for plane orientation. This is not the number of
   // planes orientations, but the number of tilts (latitudes). Then for each 
   // tilts there  will be a number of rotations (longitude) which depends on 
   // the tilt value.
   int numTiltsPlane = 0;
   zvp("MULTI_NUM", &numTiltsPlane, &count);


   // At this stage, multi plane is not compatible with the use of PLANENORM
   if (userPlaneNorm && userMultiPlane) {
      zvmessage("PLANENORM and MULTI_PLANE are not compatible. Disabling MULTI_PLANE","");
      userMultiPlane = 0;
   }

   // Pack the ranges information into a structure for convenience
   planeSpace planeInfo = {rangeSampling, rangeStep, minRange, maxRange, 
                           planeNorm, userMultiPlane, numTiltsPlane};



   // Minimum angle for the ray (camera to plane surface) to hit the surface.
   // This is to avoid super grazing angle for which projection on the plane 
   // does not make sense (from a correlation quality expectation point of 
   // view). 
   zvp("HIT_MIN_ANGLE", &minHitAngle, &count);
   if ((minHitAngle < 0) || (minHitAngle > 90)) {
      zvmessage("HIT_MIN_ANGLE must be between 0 and 90","");
      zabend();
   }
   // Convert to radian
   minHitAngle *= PI / 180.0;

   // If a projection plane does not satisfy the minimum incidence angle
   // (HIT_MIN_ANGLE), should the plane be adjusted to meet the
   // requirement
   int userAdjustPlane = 0;
   userAdjustPlane = zvptst("ADJUST_PLANE");


   // Filter output by statistical analysis of disparities wrt to the
   // template size
   statFilter = 0;
   statFilter = zvptst("STAT_FILTER");

   // Get extent from template size
   zvp("STAT_EXTENT", &statFilterExtent, &count);

   // Get filter scaler 
   zvp("STAT_STHRESHOLD", &statFilterScalerThreshold, &count);

   // Get stat filter number of pixel necessary (in percentage) to validate
   // a queried pixel
   zvp("STAT_NTHRESHOLD", &statFilterThresholdN, &count);


   // Multithreading?
   int numThreads = 1;
   omp_on = zvptst("OMP_ON");
#ifdef _OPENMP
   if (omp_on)
      numThreads = omp_get_max_threads();
#endif

   // Low-pass pre-filtering (Gaussian filter)?
   apply_filter = 0;  // Default no filter
   if (zvptst("FILTER")) { 
      apply_filter = 1;
      filter_window_l = filter_window_r = 1.1; //default values for gauss
   }
   float fw[2];
   zvp("FILTER_SIZE", fw, &count);
   if (count != 0) {
      filter_window_l = fw[0];
      filter_window_r = fw[0];
   }
   if (count == 2)
      filter_window_r = fw[1];


   // Check if user want a max acceptable angle. If set, and if the cameras
   // pointing angles difference is larger than parameter, exit.
   float sep_angle = 360; //default value for no filtering
   zvp("SEP_ANGLE", &sep_angle, &count);       // max sep angle for frames


   // Maximum allowed resolution ratio between L and R
   maxResRatioUser = (std::numeric_limits<float>::max)();
   zvp("RES_RATIO_MAX", &maxResRatioUser, &count);
   if (maxResRatioUser < 1) {
      zvmessage("Maximum relative resolution ratio cannot be < 1; Adjusting to 1.", "");
      maxResRatioUser = 1; 
   }

   // Band to process? 
   zvp("BANDS", bands, &count);    
   if (count == 1)
       bands[1] = bands[0];     
   // Check that band number is ok w.r.t image. Chosen behavior is to select
   // last band if input is greater than image' number of bands
   if (bands[0] > file_models[0]->getNB())
      bands[0] = file_models[0]->getNB();
   if (bands[1] > file_models[1]->getNB())
      bands[1] = file_models[1]->getNB();



   // Tiling (down/up-sampling) support needed?
   // This is refering to the downsampling/upsampling of 
   // image tile onboard/onground that started with M2020
   int downsamplingTiling = 0;
   downsamplingTiling = zvptst("TILING");



   // Pyramid approach?
   int pyramid = 0;
   pyramid = zvptst("RUN_PYR");

   // Pyramid max size
   int pyrSizeMax = 500;
   zvp("MAX_PYR_SIZE", &pyrSizeMax, &count);    

   // Correct for offset in epipolar line between pyramids?
   int epiShift = 0;
   epiShift = zvptst("SHIFT_EPI");
   if (!pyramid && epiShift) {
      zvmessage("Correction of epipolar offset only possible when RUN_PYR is on","");
      epiShift = 0;
   }



   // Get Left image dimensions
   int nsl_original = file_models[0]->getNS();
   int nll_original = file_models[0]->getNL();

   // Get position and orientation of the L camera. This is done by projecting
   // the center pixel out in the XYZ world. It could (should?) also be done
   // by using the camera pointing (PigPointing->getCameraPosition/Orientation):
   // PigPoint centerLeft = pointing_in[0]->getCameraPosition(output_cs);
   // PigVector oriL = pointing_in[0]->getCameraOrientation(output_cs);
   camera_in[0]->LStoLookVector((float)(nll_original-1)/2.0, 
                                (float)(nsl_original-1)/2.0, 
                                centerLeft, oriL, output_cs); 



   // Get Right image dimensions
   int nsr_original = file_models[1]->getNS();
   int nlr_original = file_models[1]->getNL();

   // Get position and orientation of the R camera
   camera_in[1]->LStoLookVector((float)(nlr_original-1)/2.0, 
                                (float)(nsr_original-1)/2.0, 
                                centerRight, oriR, output_cs); 


   zvmessage("","");
   zvmessage("************ Image information **************","");

   // Print out some information about the images
   sprintf(msg, "Left Image original size: %dx%d pixels", nsl_original, 
                                                          nll_original);
   zvmessage(msg,"");

   sprintf(msg, "Right Image original size: %dx%d pixels", nsr_original, 
                                                           nlr_original);
   zvmessage(msg,"");

   float magDist = (centerLeft-centerRight).magnitude();
   sprintf(msg,"Distance between cameras: %10.5f meters", magDist);
   zvmessage(msg, "");

   float frontDist = (centerLeft-centerRight) % oriL;
   if (frontDist < 0)
      sprintf(msg,"Right camera in \"front\" of Left camera by %10.6f meters", 
               -frontDist);
   else
      sprintf(msg,"Left camera in \"front\" of Right camera by %10.6f meters", 
               frontDist);
   zvmessage(msg, "");


   // Need to make the distinction if look vector are close to identical as 
   // small numerical error could cause oriL%oriR >1 in case the vectors are //
   // with subsequent acos returning nan
   double lookDiffAngle = oriL%oriR;
   if (abs(lookDiffAngle) >= 1.0)
      lookDiffAngle = 0;
   else
      lookDiffAngle = acos(lookDiffAngle)*180.0/PI;

   sprintf(msg,"Cameras look direction difference:  %5.3f degrees", 
           lookDiffAngle);
   zvmessage(msg, "");


   // Check for maximum allowable diff angle
   if (lookDiffAngle > sep_angle) {
      sprintf(msg, "Look direction difference above limit (%5.3f degrees). Returning", sep_angle);
      zvmessage(msg, "");
      // Setting output parameter in case it is checked
      overlapCheck = 0.0;
      q_init(&out_parb, 50, P_ABORT);
      q_real(&out_parb, "OVERLAP_CHECK", 1, &overlapCheck, P_ADD);
      zvq_out(&out_parb);
      return;
   }

   float leftMinCosine, leftMaxCosine;
   getMinMaxLookAngle(camera_in[0], nsl_original, nll_original, output_cs, 
                      leftMinCosine, leftMaxCosine);
   float rightMinCosine, rightMaxCosine;
   getMinMaxLookAngle(camera_in[1], nsr_original, nlr_original, output_cs,
                      rightMinCosine, rightMaxCosine);
   sprintf(msg,"Left image largest look angle from camera orientation: %8.3f degrees",
               acos(leftMinCosine) * 180.0 / PI);
   zvmessage(msg, "");
   sprintf(msg,"Right image largest look angle from camera orientation: %8.3f degrees",
               acos(rightMinCosine) * 180.0 / PI);
   zvmessage(msg, "");



   zvmessage("","");
   zvmessage("********* Processing information ************","");

   // Print out some information about processing
   if (pyrlevel) {
      sprintf(msg, "Output pyramid level: %d", pyrlevel);
      zvmessage(msg, "");
      sprintf(msg, "Left Image downsampled size: %dx%d pixels", 
                   nsl_original/downFactor, nll_original/downFactor);
      zvmessage(msg,"");
      sprintf(msg, "Right Image downsampled size: %dx%d pixels", 
                   nsr_original/downFactor, nlr_original/downFactor);
      zvmessage(msg,"");
   }
   else zvmessage("Output pyramid level: 0 (full size)", "");


   if (downsamplingTiling)
      zvmessage("Image tiling information turned on", "");




   
   // Load L and R images 
   imgL = loadImage(file_models[0], bands[0]);
   imgR = loadImage(file_models[1], bands[1]);


   // Retrieve L and R tiling if TILING is ON
   SimpleImage<int> *downsamplingTilingL = nullptr;
   SimpleImage<int> *downsamplingTilingR = nullptr;
   if (downsamplingTiling) {
      
      // List of tiling levels in the image
      std::set<int> tileLevels;
 
      downsamplingTilingL = new SimpleImage<int>(imgL->getNL(), imgL->getNS());
      std::fill(downsamplingTilingL->linePtr(0), downsamplingTilingL->linePtr(0) + 
                       imgL->getNL() * imgL->getNS(), 1);
      int statusL = getTilingInformation(file_models[0], downsamplingTilingL, 
                                         tileLevels);

      // Display the tiling levels in the Left image
      if (statusL == 1) {
        char *pos = msg;
        pos += sprintf(pos, "Left tiling levels: ");
        for (const auto &t: tileLevels)
           pos += sprintf(pos, "%4d", t);
        zvmessage(msg, ""); 
      }

      tileLevels.clear();
      downsamplingTilingR = new SimpleImage<int>(imgR->getNL(), imgR->getNS());
      std::fill(downsamplingTilingR->linePtr(0), downsamplingTilingR->linePtr(0)+
                       imgR->getNL() * imgR->getNS(), 1);
      int statusR = getTilingInformation(file_models[1], downsamplingTilingR, 
                                         tileLevels);

      // Display the tiling levels in the Right image
      if (statusR == 1) {
        char *pos = msg;
        pos += sprintf(pos, "Right tiling levels: ");
        for (const auto &t: tileLevels)
           pos += sprintf(pos, "%4d", t);
        zvmessage(msg, ""); 
      }

       if (statusL < 1 && statusR < 1) {
          zvmessage("Invalid tiling information on both inputs; Turning TILING off","");
          downsamplingTilingL->free();       
          downsamplingTilingR->free();       
          downsamplingTilingL = nullptr;       
          downsamplingTilingR = nullptr;       
          downsamplingTiling = 0;
       }
       else {
          // No valid information on tiling for left image. Assume full res
          if (statusL < 1) {
             zvmessage("No valid tiling info for INP 1; Assuming no tiling","");
             std::fill(downsamplingTilingL->linePtr(0), downsamplingTilingL->linePtr(0) + 
                       imgL->getNL() * imgL->getNS(), 1);
          }
          // No valid information on tiling for right image. Assume full res
          if (statusR < 1) {
             zvmessage("No valid tiling info for INP 2; Assuming no tiling","");
             std::fill(downsamplingTilingR->linePtr(0), downsamplingTilingR->linePtr(0)+
                       imgR->getNL() * imgR->getNS(), 1);
          }
       }
    }


   // if pyramid level is on, downsample the image and tiling map 
   if (pyrlevel) {

      downsample(imgL, imgL, downFactor, omp_on);
      downsample(imgR, imgR, downFactor, omp_on);

      if (downsamplingTiling) {
         downsampleTiling(downsamplingTilingL, downsamplingTilingL, downFactor);
         downsampleTiling(downsamplingTilingR, downsamplingTilingR, downFactor);
      }

      // Scale PIG camera models accordingly
      camera_in[0]->scaleCamera(1.0/downFactor, 1.0/downFactor);   
      camera_in[1]->scaleCamera(1.0/downFactor, 1.0/downFactor);   
   }



   // Apply gaussian filtering if needed.
   if (apply_filter) {
      zvmessage("Pre-filtering (low-pass) of the images","");
      if (filter_window_l > 1) {
         float sigma = 0.8 * sqrt(filter_window_l * filter_window_l - 1.0);
         GaussianBlur(imgL, sigma, sigma, omp_on);
      }
      if (filter_window_r > 1) {
         float sigma = 0.8 * sqrt(filter_window_r * filter_window_r - 1.0);
         GaussianBlur(imgR, sigma, sigma, omp_on);
      }
   }
   else
      zvmessage("No low-pass pre-filtering of the images","");


   // Recompute max view angle of current images (may be downsampled). This 
   // should be very minimal difference, if any, with full resolution view 
   // angles computed above. But it is cheap, so just for peace of mind
   getMinMaxLookAngle(camera_in[0], imgL->getNS(), imgL->getNL(), output_cs, 
                      leftMinCosine, leftMaxCosine);
   getMinMaxLookAngle(camera_in[1], imgR->getNS(), imgR->getNL(), output_cs,
                      rightMinCosine, rightMaxCosine);




   // Storing Left camera and image information in local structure (mostly for
   // ease of moving information around)
   camGeometry leftC;
   leftC.cm = camera_in[0];
   leftC.cs = output_cs;
   leftC.ns = imgL->getNS();
   leftC.nl = imgL->getNL();
   leftC.closenessLimit = 0;
   leftC.minCosine = leftMinCosine;
   leftC.downsamplingTile = downsamplingTilingL;
   leftC.cm->LStoLookVector((float)(leftC.nl-1)/2.0, (float)(leftC.ns-1)/2.0, 
                            leftC.pos, leftC.look, output_cs); 

   // Storing Right camera and image information
   camGeometry rightC;
   rightC.cm = camera_in[1];
   rightC.cs = output_cs;
   rightC.ns = imgR->getNS();
   rightC.nl = imgR->getNL();
   rightC.closenessLimit = rightClosenessLimit;
   rightC.minCosine = rightMinCosine;
   rightC.downsamplingTile = downsamplingTilingR;
   rightC.cm->LStoLookVector((float)(rightC.nl-1)/2.0, (float)(rightC.ns-1)/2.0, 
                             rightC.pos, rightC.look, output_cs); 



   // Get Ground Sampling Distance (GSD) of the L and R pixel centers at 1 meter
   // (assuming fronto-look) and the ratio of the GSD between the center of the 
   // image and any pixel in the image. The actual GSD depends on the depth, 
   // which we don't have, but we're interested in the ratio only. We are 
   // assuming radial optical distortions, such that we only need to compute the
   // ratio along a radius. In case of pinhole camera for instance, this ratio 
   // is about 1 for any pixel. This would be different in case of strong 
   // distortion and/or non-linear model such as fisheye.
   // At this stage this is mostly for information display
   computeRelativeGSDfromCenterPixel(leftC); 
   computeRelativeGSDfromCenterPixel(rightC); 
   
   // Print for infomation the min/max resolution ratio for each camera as well
   // as the GSD at one meter of the center pixel
   sprintf(msg,"Left center pixel GSD at 1 meter:%10.5f", leftC.GSDcenterAt1m);
   zvmessage(msg, "");
   sprintf(msg,"Right center pixel GSD at 1 meter:%10.5f",rightC.GSDcenterAt1m);
   zvmessage(msg, "");
   
   // Compute min/max of the relative GSD of pixels for L and R images
   auto minMax = std::minmax_element(begin(leftC.relativeGSD), end(leftC.relativeGSD));
   sprintf(msg, "Left image Min/Max GSD ratio w.r.t center pixel:%10.2f, %10.2f",
           *minMax.first, *minMax.second);
   zvmessage(msg, "");
   
   minMax = std::minmax_element(begin(rightC.relativeGSD), end(rightC.relativeGSD));
   sprintf(msg, "Right image Min/Max GSD ratio w.r.t center pixel:%10.2f, %10.2f",
          *minMax.first, *minMax.second);
   zvmessage(msg, "");



   // If only drawing the epipolar lines is requested, do it now and
   // exit (i.e., no correlation is done).
   // If epipolar line drawing, get for which L pixels should the epipolar 
   // lines be drawn:
   // If DRAW_COORD values are positive it indicates pixel coordinates of the
   // L point for which the corresponding epipolar curve in the R image is to
   // be drawn. 
   // If DRAW_COORD values are negative it indicates the grid spacing of pixel
   // to draw the epipolar curve
   int draw = 0;
   char nameDraw[256];
   zvp("OUT_DRAW", nameDraw, &draw);

   if (draw) {
      int drawCoord[2];
      zvp("DRAW_COORD", drawCoord, &count);
      if (count == 1)
         drawCoord[1] = drawCoord[0];

      drawCoord[0] /= downFactor;
      drawCoord[1] /= downFactor;

      std::vector<int> tmpX, tmpY, Xarr, Yarr;
      if (drawCoord[1] < 0)
         for (int i=0; i<imgL->getNS(); i += abs(drawCoord[1]))
            tmpX.push_back(i);
      else
         tmpX.push_back(drawCoord[1]);

      if (drawCoord[0] < 0)
         for(int i=0; i<imgL->getNL(); i += abs(drawCoord[0]))
            tmpY.push_back(i);
      else
         tmpY.push_back(drawCoord[0]);


      // Complete grid
      for (auto y:tmpY) {
         for (auto x:tmpX) {
            Xarr.push_back(x);
            Yarr.push_back(y);
         }
      }       

      drawEpipolarPoints(imgL, imgR, leftC, rightC, planeInfo, epiStep, 
                         Xarr, Yarr, nameDraw); 

      return; 
   }



   // How many pyramid levels necessary if having a pyramid approach?
   int pyrMax = 0;
   if (pyramid) { 
      // Compute the pyramid level necessary. Look a the smallest side (X and Y 
      // directions) between the Left and Right image. Then how many dowsampling
      // (by x2 each time) are necessary to get that smallest side to be smaller
      // than pyrSizeMax (500 pix by default)
      int minSideSize = imgL->getNS();
      if (imgL->getNL() < minSideSize) minSideSize = imgL->getNL();
      if (imgR->getNS() < minSideSize) minSideSize = imgR->getNS();
      if (imgR->getNL() < minSideSize) minSideSize = imgR->getNL();

      while (minSideSize > pyrSizeMax) {
         minSideSize /= 2;
         pyrMax++;
      }
      sprintf(msg, "Pyramidal approach ON (%d pyramid levels)", pyrMax);
      zvmessage(msg,"");
   }
   else
      zvmessage("Pyramidal approach OFF","");



   // Some global variables and containers

   
   // Affine coefficients maps
   // Amoeba 6 type.
   coefs_a = coefs_b = coefs_d = coefs_e = coefs_g = coefs_h = nullptr;

   // Left and Right image for the current pyramid level
   SimpleImage<float> *imgLc = nullptr;
   SimpleImage<float> *imgRc = nullptr;
   SimpleImage<int> *downsamplingTilingLc = nullptr;
   SimpleImage<int> *downsamplingTilingRc = nullptr;


   // Best range for a given pixel. Used only in a pyramid approach where
   // the best range at a given pyramid level is used to seed the range
   // search bracket at the next pyramid level
   SimpleImage<float> *ranges = nullptr;

   // Same thing with the projection plane normal. Used only in a pyramid
   // approach with multiplane.
   SimpleImage<PigVector> *normals = nullptr;

   // Epipolar line offset image. Used only if a correction of the offset in
   // the perpendicular epipolar direction is required
   SimpleImage<float> *epiOffX = nullptr;
   SimpleImage<float> *epiOffY = nullptr;



   // In case of pyramidal approach, this is the main loop of the process. If 
   // the pyramidal approach is not used, this is a 1-iteration loop
   for (int pyr = pyrMax; pyr >= 0; pyr--) {
   
      zvmessage(" ","");
      zvmessage(" ","");
      sprintf(msg, "Pyramid num: %d", pyr);
      zvmessage(msg,"");
   
   
      // Freeing data from previous loop
      // This is done here instead of at the end of loop as we need to save the
      // last loop results
      if (pyr != pyrMax) {

         corrScores.free();
         locX.free();
         locY.free();

         coefs_a->free();
         coefs_b->free();
         coefs_d->free();
         coefs_e->free();
         coefs_g->free();
         coefs_h->free();

         imgLc->free();
         delete imgLc;
         imgLc = nullptr;

         imgRc->free();
         delete imgRc;
         imgRc = nullptr;

         if (downsamplingTilingLc != nullptr) {
            downsamplingTilingLc->free();
            delete downsamplingTilingLc;
            downsamplingTilingLc = nullptr;

            downsamplingTilingRc->free();
            delete downsamplingTilingRc;
            downsamplingTilingRc = nullptr;
         }

         //TODO
         //At this stage it might be usefull (faster process) to reduce the 
         //SEARCH which is here mostly for perp epi direction search to a smaller
         //values as we're supposedly centered from now on if SHIFT_EPI is ON.

      }
   
   
   
   
      // Downsampling factor of the current pyramid iteration
      int downFactor = pow(2,pyr);
   
      // Downsample the images accordingly...
      downsample(imgL, imgLc, downFactor, omp_on);
      downsample(imgR, imgRc, downFactor, omp_on);
   
      // ...and the tiling maps if images are tiled
      if (downsamplingTiling) {
         downsampleTiling(downsamplingTilingL, downsamplingTilingLc, downFactor);
         downsampleTiling(downsamplingTilingR, downsamplingTilingRc, downFactor);
      }
   
   
      // Scale PIG camera models accordingly
      // On the first pass, the camera model is downsampled all the way to the
      // smallest "size", then upsampled by a factor of 2 at each subsequent 
      // pass. It is done this way because it is not clear if camera models can 
      // be copied easily
   
      if (pyr == pyrMax) {
         leftC.cm->scaleCamera(1.0/downFactor, 1.0/downFactor);   
         rightC.cm->scaleCamera(1.0/downFactor, 1.0/downFactor);   
      }
      else {
         leftC.cm->scaleCamera(2.0, 2.0);   
         rightC.cm->scaleCamera(2.0, 2.0);
      }   
   
   
      // Get the dimension of images at the current pyramid iteration
      int nsl = imgLc->getNS();
      int nll = imgLc->getNL();
      int nsr = imgRc->getNS();
      int nlr = imgRc->getNL();
   
   
      // Update Left camera and image information
      leftC.ns = nsl;
      leftC.nl = nll;
      leftC.downsamplingTile = downsamplingTilingLc;
      leftC.cm->LStoLookVector((float)(nll-1)/2.0, (float)(nsl-1)/2.0, 
                               leftC.pos, leftC.look, output_cs); 
   
      // Update Right camera and image information
      rightC.ns = nsr;
      rightC.nl = nlr;
      rightC.downsamplingTile = downsamplingTilingRc;
      rightC.cm->LStoLookVector((float)(nlr-1)/2.0, (float)(nsr-1)/2.0, 
                                rightC.pos, rightC.look, output_cs); 
   
   
      sprintf(msg, "Left Image : %dx%d pixels", nsl, nll); 
      zvmessage(msg,"");
      sprintf(msg, "Right Image : %dx%d pixels", nsr, nlr); 
      zvmessage(msg,"");
   
   
   
   
      // Define the tiling of the L image
      // Each tile will be processed independently of the others. At the tile 
      // level, the assumption is that the relationship between the L and R is
      // "fixed", either a global homography or affine.
   
      // Get the nominal tile size. All tiles will have the same size, except the
      // ones on the right and bottom edges of the L images which will be sized
      // according to what's left of the image.
      getAndCheckTileSize(tileSize, nll, nsl, corrSz);
   
      // Initialize the tiler structure that will contains all tiles information
      // and tile processing order
      tiler tiler = initializeTiler(leftC, nll, nsl, tileSize, corrSz);
   
   
      sprintf(msg, "Image tiling: %d x %d", tileSize[0], tileSize[1]);
      zvmessage(msg, "");
   
      sprintf(msg, "%d tiles in X, %d tiles in Y", tiler.nbTilesX, tiler.nbTilesY);
      zvmessage(msg, "");
   
      sprintf(msg, "Total number of tiles: %u", tiler.nbTiles); 
      zvmessage(msg, "");
   
   
   
   
   

   
      // Generate the raw topography prior for the tiles. That is, we'll update 
      // each tiles with a set of raw ranges and surface normal. Later on, the 
      // program will refine the raw ranges list such that successive ranges in the
      // list correspond to the user defined step along the epipolar line. We'll 
      // also discard ranges that are out of FOV.
      // The way the list of raw ranges are attributed to a tile depends on different
      // factors:
      // - If the pyramidal approach is chosen, then the ranges and normal(s) 
      // are defined from the ranges and normal(s) found in the previous pyramid
      // level. This is obviously for pyramid iterations other than first one.
      // - For the first pyramid iteration (or unique iteration in case of 
      // non-pyramidal approach), there are three ways to define the raw ranges 
      // (in priority order):
      // [1] The user supplied a mesh (.OBJ) that is used to define a small list of
      // ranges around the surface given by the mesh. The surface normal is defined
      // from the mesh too.
      // [2] The user supplied a range and/or surface normal file. This would 
      // typically be a vicar file _RNG and/or _UVW file. These files have to be in
      // the left image frame.
      // [3] No suface prior is supplied, the raw ranges will be defined from the 
      // MIN/MAX_RANGE and sammpling strategy. 
   
      if (pyr == pyrMax)
         getRangeNormalPrior(tiler, leftC, planeInfo);
      else
         getRangeFromPyr(tiler, leftC, planeInfo, ranges, normals);
  
   
      // Refine the raw ranges defined in the steps above into a list of ranges
      // that corresponds to a continuous (modulo EPI_STEP) epipolar line in the
      // right image. Retrieve also the corresponding tiling factor for each of
      // the pixels on the epipolar line.
      #pragma omp parallel for if (omp_on)
      for (int i = 0; i < tiler.nbTiles; i++) {
         tile &t = tiler.tiles[i];
         getTileDepths(leftC, rightC, epiStep, t);
         getTilingPerRange (leftC, rightC, t);
         if (userAdjustPlane)
            adjustPlaneNorm(leftC, minHitAngle, t);
         t.H.resize(t.ranges.size());
         t.A.resize(t.ranges.size());
      }


      // Run some statistics on the tiles' list data to estimates if the L and R
      // do have potential overlap and extract some information for ouput
   
      // Counter for tiles with ranges (ie, possible overlap with Right image)
      double numOverlapTile = 0; 
      for (const auto &t:tiler.tiles) 
         if (!t.ranges.empty())
            numOverlapTile++;

   
      // Save output variable that indicates the percentage of overlaping tiles.
      // That is, how many Left tiles do have an interesecting FOV with the Right
      // image. If no tile do overlap, then no need to go further.
   
      overlapCheck = numOverlapTile/tiler.nbTiles*100.0;
   
      // Save check flag score to TAE 
      q_init(&out_parb, 50, P_ABORT);
      q_real(&out_parb, "OVERLAP_CHECK", 1, &overlapCheck, P_ADD);
      zvq_out(&out_parb);
   
      if (overlapCheck == 0) {
         zvmessage(" ","");
         if (pyr == pyrMax)
            sprintf(msg,"No overlap between the images given. May need to adjust range bracket. Returning...");
         else
            sprintf(msg,"No matching pixels found in previous pyramid level. May need to adjust search size. Returning...");
         zvmessage(msg, "");
         zvmessage(" ","");
         return;
      }
   
      if (zvptst("CHECK")) {
         zvmessage(" ","");
         sprintf(msg,"%5.2f%% overlaps between the images. Check completed. Returning...",
                 overlapCheck);
         zvmessage(msg, "");
         zvmessage(" ","");
         return;
      }
   
   
   
   
   
      // Allocate container if correction of the perpendicular epipolar shift is
      // to be corrected between pyramid levels
      if (epiShift) {
         if (pyr != pyrMax) {
            updateEpiShift(tiler, leftC, rightC, epiOffX, epiOffY);
            epiOffX->free(); 
            epiOffY->free(); 
            delete epiOffX;
            delete epiOffY;
         }
         epiOffX = new SimpleImage<float>(nll, nsl);
         epiOffY = new SimpleImage<float>(nll, nsl);
         epiOffX->zero();
         epiOffY->zero();
      }


   
   
   
   
   
      // Processing start...
   
   
      zvmessage("","");
      zvmessage("Processing...","");
      auto t66 = high_resolution_clock::now();
   
   
      // Initialization of the output images, i.e.:
      // - correlation score
      // - x/y coordinates of best correlation (disparity map)
      // - Affine transform coefficients. Note that g/h coef parameters for trapezoid 
      //   functions are kept for consistency and set to 0 in output file
      corrScores = SimpleImage<float>(nll, nsl);
      locX = SimpleImage<float>(nll, nsl);
      locY = SimpleImage<float>(nll, nsl);
      corrScores.zero();
      locX.zero();
      locY.zero();
   
      //SimpleImage<float> *coefs_a, *coefs_b, *coefs_d, *coefs_e, *coefs_g, *coefs_h;
      coefs_a = new SimpleImage<double>(nll, nsl);
      coefs_b = new SimpleImage<double>(nll, nsl);
      coefs_d = new SimpleImage<double>(nll, nsl);
      coefs_e = new SimpleImage<double>(nll, nsl);
      coefs_g = new SimpleImage<double>(nll, nsl);
      coefs_h = new SimpleImage<double>(nll, nsl);
      coefs_a->zero();
      coefs_b->zero();
      coefs_d->zero();
      coefs_e->zero();
      coefs_g->zero();
      coefs_h->zero();
   



      // Initialize container that will store the best (correlation-based) 
      // range for a given pixel
      if (ranges != nullptr) 
         ranges->free(); 
      ranges = new SimpleImage<float>(nll, nsl);
      ranges->zero();

      // Same thing with plane normals
      if (normals != nullptr)
         normals->free();
      normals = new SimpleImage<PigVector>(nll, nsl);



      // Progress bar display
      printProgress(tiler.nbTiles);
   
   
      #pragma omp parallel if (omp_on)
      {
   
         // Some pre-initialization before the main loop over the Left tiles
         SimpleImage<float> *currentImgL, *currentImgR, *scores, *bestScores,
                            *bestLocX, *bestLocY, *bestRanges, 
                            *bestPerpX, *bestPerpY;
         SimpleImage<double> *bestCoefs_a, *bestCoefs_b, *bestCoefs_d, 
                             *bestCoefs_e;  
         SimpleImage<PigVector> *bestNormals; 
         SimpleImage<int> *mX, *mY;
   
         #pragma omp for schedule(dynamic) 
         for (int t = 0; t < tiler.nbTiles; t++) {
   
            // Get the current tile
            tile & ctile = tiler.tiles[t];
 
            // Get the size of the current tile
            int nbRows = ctile.ye - ctile.ys + 1;
            int nbCols = ctile.xe - ctile.xs + 1;
   
            // Initialize correlation result containers for that tile
            scores = new SimpleImage<float>(nbRows, nbCols);
            mX = new SimpleImage<int>(nbRows, nbCols);
            mY = new SimpleImage<int>(nbRows, nbCols);
   
            // Subset in full output images corresponding to that tile
            bestScores = new SimpleImage<float>(corrScores, ctile.ys, ctile.xs, nbRows, nbCols);
            bestLocX = new SimpleImage<float>(locX, ctile.ys, ctile.xs, nbRows, nbCols);
            bestLocY = new SimpleImage<float>(locY, ctile.ys, ctile.xs, nbRows, nbCols);
            bestCoefs_a = new SimpleImage<double>(*coefs_a, ctile.ys, ctile.xs, nbRows, nbCols);
            bestCoefs_b = new SimpleImage<double>(*coefs_b, ctile.ys, ctile.xs, nbRows, nbCols);
            bestCoefs_d = new SimpleImage<double>(*coefs_d, ctile.ys, ctile.xs, nbRows, nbCols);
            bestCoefs_e = new SimpleImage<double>(*coefs_e, ctile.ys, ctile.xs, nbRows, nbCols);
   
            bestRanges = new SimpleImage<float>(*ranges, ctile.ys, ctile.xs, nbRows, nbCols);
            bestNormals = new SimpleImage<PigVector>(*normals, ctile.ys, ctile.xs, nbRows, nbCols);
   
            if (epiShift) {
               bestPerpX = new SimpleImage<float>(*epiOffX, ctile.ys, ctile.xs, nbRows, nbCols);
               bestPerpY = new SimpleImage<float>(*epiOffY, ctile.ys, ctile.xs, nbRows, nbCols);
            }

            // Depending on the user input, define the subgrid of points in the tile
            // to define the tiepoints to compute the homography between L and R. Make
            // sure that the subgrid is not coarser than the tile size itself. This 
            // could happen for smaller tiles at the right or bottom sides of the L 
            // image.
            int gridX = (nbCols-1)/(gridTile[1]-1); 
            if (gridX < 1) gridX = 1;
   
            int gridY = (nbRows-1)/(gridTile[0]-1); 
            if (gridY < 1) gridY = 1;
   
   
  
            // Iterate over the list of plane orientation
            for (int n=0; n < ctile.planeNormArr.size(); n++) {

               // Iterate over the list of range for the current tile
               for (int r=0; r < ctile.ranges.size(); r++) {

                  // Get current plane orientation
                  ctile.planeNorm = ctile.planeNormArr[n];

                  // Get the current range
                  float cr = ctile.ranges[r]; 

   
                  // Compute the local homography between the L tile and the R image. 
                  // This is done in two steps. First project a subgrid of points of 
                  // the current L tile to plane, and then back to R image. That will
                  // form tie points between L and R. Second, use the tie points 
                  // between L and R pixels to define a local homography using the 
                  // DLT (Direct Linear Transform) algorithm.
   
                  // [1] - Project the subgrid of points from L tile to R using the 
                  // current range and plane orientation. That will return a list of 
                  // tiepoints between the L and R.
                  std::vector<tiePoint> ties = getTies(leftC, rightC, ctile,
                                                       gridX, gridY, 
                                                       cr, minHitAngle);
   
                  if (ties.size() < 4) 
                     continue;
   

                  // [2] - Compute the local homography between L and R from that list
                  // of tiepoints. The output is a 3x3 matrice mapping Left (x,y) to 
                  // R (x, y).
                  status = computeHomography(ties, ctile.H[r]);
                  if (status) {
                     zvmessage("The homography computation using the DTL failed", "");
                     continue;
                  }
  
   
                  // Compute the local affine transform. The output is a 3x2 
                  // matrice mapping Left (x,y) to R (x, y).
                  // In theory, assuming a linear cam model at the scale of a 
                  // tile, the transform between L and R is an homography. 
                  // However, for a couple of reasons, it is easier or necessary
                  // (depending on the reason) to also use an affine transform 
                  // approximation:
                  // - Marscor3, which uses the output of this program doesn't 
                  // support homography. Only affine or a trapezoid-of-sort 
                  // function (alas non-linear) that is not an homography. 
                  // - For sizing the lowpass kernel to apply on the Left image
                  // and/or sizing the resampling kernel on the Right image.
                  // Depending on the plane orientation, a large range of 
                  // directional image compression/extension can result. We use
                  // the affine approximation and its SVD to conveniently define
                  // the direction of compression/extension in L and R.
                  // Note that, locally, an homography is well approximated with
                  // an affine transform, and the further the object from the
                  // camera, the more correct it is. So in most situations, this
                  // approximation should be reasonable.
                  status = computeAffineTransform(ties, ctile.A[r]);
                  if (status) {
                     zvmessage("The affine transform computation failed", "");
                  }
 


                  // Analysis of the affine transformation matrix

                  // Get the affine transform matrix (not the translation part)
                  MatrixXf aMat(2,2);
                  aMat(0,0) = ctile.A[r][0];
                  aMat(0,1) = ctile.A[r][1];
                  aMat(1,0) = ctile.A[r][3];
                  aMat(1,1) = ctile.A[r][4];

                  // Sanity check.
                  // Determinant of affine matrice must be strictly positive. 
                  // Otherwise the transform involves a "reflection" which 
                  // shouldn't happen.
                  if (aMat.determinant() <= 0)
                     continue;

                  // Compute the SVD of the affine transform. It should be the
                  // simple composition of rotation and shear
                  JacobiSVD<MatrixXf> svd(aMat, ComputeThinU | ComputeThinV);
                  
                  MatrixXf U = svd.matrixU();
                  MatrixXf V = svd.matrixV();
                  VectorXf S = svd.singularValues();

                  // Sanity check on the U and V.
                  // - determinant of both should be either positive or negative
                  if (U.determinant() * V.determinant() < 0)
                     continue;

                  // Sanity check on the U and V.
                  // - amplitude of determinant should be ~ 1
                  if (fabs(fabs(U.determinant()) - 1) > 1e-5  || 
                      fabs(fabs(V.determinant()) - 1) > 1e-5)
                     continue;


                  // Account for the image tiling in the shear component (in the
                  // singular values). The image tiling factor will affect the 
                  // low-pass filtering involved in the resampling, but won't
                  // affect the grid onto which the R image is projected.
                  VectorXf Stiled = S;
                  //TODO not perfect. When tL*S ~ tR is not handled properly
                  //For instance tL=tR=2 S~1. Should be no prefilter. Only scaling of window
                  for (int i=0; i<2; i++) {
                     //if (ctile.tilingL * S[i] > ctile.tilingRs[s][r])
                     if (ctile.tilingL * S[i] > ctile.tilingRs[r])
                        Stiled[i] *= ctile.tilingL;
                     else
                        Stiled[i] /= ctile.tilingRs[r];
                  } 


                  // Check that the scaling factor between L and R doesn't go 
                  // above user defined limit. This is to avoid rogue situation
                  // where the smoothing is so high that [1] it'll take a long 
                  // time to process, [2] does not make sense in practice 
                  // because all features are gone, [3] is likely to provide
                  // the best correlation SNR.
                  if ((Stiled[0] > maxResRatioUser) || (Stiled[0] < 1.0/maxResRatioUser) ||
                      (Stiled[1] > maxResRatioUser) || (Stiled[1] < 1.0/maxResRatioUser))
                     continue;


                  // Compute the pseudo-inverse of S 
                  // Stiled component should not be == 0. Checked above with
                  // determinant of M matrix
                  VectorXf invStiled = Stiled;
                  invStiled[0] = 1/Stiled[0];
                  invStiled[1] = 1/Stiled[1];


                  // Generate a "tiled" affine matrix. This matrix will be used
                  // to define how much lowpass filtering needs to be applied
                  // to the L or R prior resampling (using the non-tiled
                  // affine matrix)
                  MatrixXf aMattiled(2,2);
                  aMattiled = U * Stiled.asDiagonal() * V.transpose();


                  // Projection of the R image onto the L tile.
                  // To project the R image onto the L tiles, the steps are:
                  // [1] - Define the grid in the L image that needs to be projected. 
                  // That is, the tile + some border to account for the correlation 
                  // template size + some extra border to account for search space due
                  // to error in the pointing (deviation from the strict epipolar 
                  // line) and the step in the epipolar line definition + the 
                  // enlargement depending on the ratio if needed.
                  // [2] - Using the homography, project the grid on the R image using
                  // the local homography.
                  // [3] - Interpolate the R image using the projection and the proper
                  // filtered R image (depends on resolution ratio).
   
               
                  // Top/Left and Bottom/Right pixels coordinates in the Left image
                  // that needs to be projected in the Right image to define the 
                  // necessary Right patch to be projected on the left.
                  int xtlR, ytlR, xbrR, ybrR, numPixInArea;
   
                  // Top/Left and Bottom/Right pixels coordinates of the Left image
                  // that are necessary to process the current Left tile. It is 
                  // essentially the current Left tile + correlation window size +
                  // (possibly) enlargement because of resolution difference.
                  // The difference between these coordinates and the ones above is 
                  // the search area that needs to be retrieved for the R image
                  int xtlL, ytlL, xbrL, ybrL;
   
                  // Depending on the resolution ratio between L and R, the 
                  // correlation window size and search will be increased. This 
                  // happens if L is high resolution compared to R.
                  // The increase of the correlation window size and search is done 
                  // such that in the R images, they corresponds to at least the 
                  // input corr and search size. 
                  int corrHalfLengthX, corrHalfLengthY, 
                      searchHalfLengthX, searchHalfLengthY,
                      searchStepX, searchStepY;
   


                  float scaleLx, scaleLy;
                  scaleLx = scaleLy = 1; // Default is to not scale the L tile
                  
                  // Check if scaling is needed in the X direction
                  double absX = fabs(aMattiled(0,0));
                  double absY = fabs(aMattiled(1,0));
                  if (absX < 1 && absY < 1)
                     scaleLx = (absX > absY) ? 1/absX : 1/absY;    

                  // Check if scaling is needed in the Y direction
                  absX = fabs(aMattiled(0,1));
                  absY = fabs(aMattiled(1,1));
                  if (absX < 1 && absY < 1)
                     scaleLy = (absX > absY) ? 1/absX : 1/absY;    

                  corrHalfLengthX = floor(((corrSz[0]-1)/2 * scaleLx)+ 0.5);
                  corrHalfLengthY = floor(((corrSz[1]-1)/2 * scaleLy)+ 0.5);
                  searchHalfLengthX = floor(search * scaleLx);
                  searchHalfLengthY = floor(search * scaleLy);
                  searchStepX = floor(scaleLx);
                  searchStepY = floor(scaleLy);

   
                  // Define the topLeft/bottomRight corners of the L area to project. 
               
                  // Start with tile coordinates
                  xtlL = ctile.xs;
                  ytlL = ctile.ys;
                  xbrL = ctile.xe;
                  ybrL = ctile.ye;

                  // Add the template size to the L area
                  xtlL -= corrHalfLengthX;
                  ytlL -= corrHalfLengthY;
                  xbrL += corrHalfLengthX;
                  ybrL += corrHalfLengthY;
 

                  // For the coordinates of the R patch, enlarge the boundaries of the
                  // L area by the search. Note this extension is only needed for the 
                  // R image (search space) so we only update the topLeft and botRight
                  // for the R image
                  xtlR = xtlL - searchHalfLengthX;
                  ytlR = ytlL - searchHalfLengthY;
                  xbrR = xbrL + searchHalfLengthX;
                  ybrR = ybrL + searchHalfLengthY;
 
   
                  // Project the R tile (onto L frame)
                  // First, compute the the mapping to project R to L for each pixel
                  // of the projected area
                  std::valarray<float> matX, matY;
                  computeRtoLprojectionMatrices(ctile.H[r], xtlR, ytlR, xbrR, ybrR,
                                                matX, matY);  // output


                  // If correcting for a perpendicular epipolar shift, apply the
                  // correction if available
                  if (epiShift && (pyr != pyrMax)) {
                     matX = matX + ctile.perpEpi[0];
                     matY = matY + ctile.perpEpi[1];
                  }


                  // Prefiltering is needed if at least one singular value of 
                  // the affine transform is > ewaT (user defined).
                  // This means R is higher resolution than L and will be 
                  // subsampled. We do use a limit (greater than theoretical 1) 
                  // to avoid prefiltering which is time consuming and will have
                  // little effect for these values. 
                  SimpleImage<float> *tileR = nullptr;
                  if ((Stiled[0] > ewaT) && ewaR) {
                     tileR = ewa(imgRc, xbrR-xtlR+1, ybrR-ytlR+1, matX, matY, 
                                 gaussP, U, Stiled, V);
                  }
                  else {
                     tileR = resampleImageBicubic(imgRc, 
                                                  xbrR-xtlR+1, ybrR-ytlR+1,
                                                  matX, matY);
                  }


                  //Prefiltering is needed if the smallest Singular value of the
                  //affine transform is < userThreshold
                  //This means L is higher resolution in at least one given 
                  //direction (in 2 directions if both singular values < 
                  //userThreshold), than R and needs to be low passed. We use this
                  //threshold limit to avoid prefiltering when it's almost not 
                  //necessary, which would be time consuming with little effect.
                  SimpleImage<float> * tileL = nullptr;
                  if ((Stiled[1] < ewaTi) && ewaL) {
                     std::valarray<float> matXL((xbrL-xtlL+1)*(ybrL-ytlL+1));
                     std::valarray<float> matYL((xbrL-xtlL+1)*(ybrL-ytlL+1));
                     int index = 0;
                     for (int y=ytlL; y<(ybrL+1); y++) {
                        for (int x=xtlL; x<(xbrL+1); x++) {
                           matXL[index] = x;
                           matYL[index] = y;
                           index++;
                        }
                     }
                     tileL = ewa(imgLc, xbrL-xtlL+1, ybrL-ytlL+1, matXL, matYL, 
                                 gaussP, V, invStiled, U);
                  }
                  else { 
                     tileL = getTile(imgLc, 
                                     xtlL, ytlL, xbrL-xtlL+1, ybrL-ytlL+1);
                  } 


  
                  // Correlation between L and R tiles using integral images summation
                  // The output is a maxCorrScore and X/Y location for each pixel of 
                  // the L tile (does not account for the exta border necessary for 
                  // patch and resolution extension)
                  scores->zero();
                  mX->zero();
                  mY->zero();
   
                  integralImageCorrelation(tileL, tileR, 
                                           corrHalfLengthX*2+1, corrHalfLengthY*2+1, 
                                           searchHalfLengthX, searchHalfLengthY, 
                                           searchStepX, searchStepY, 
                                           scores, mX, mY);   // output


                  // Save best
                  float currentScore;
                  int loc1D;
                  int offsetX = corrHalfLengthX + searchHalfLengthX;
                  int offsetY = 2*(corrHalfLengthX + searchHalfLengthX) + nbCols;
                  int offset = (corrHalfLengthY + searchHalfLengthY) * offsetY + offsetX;
   
                  for (int y=0; y<nbRows; y++) {
                     for (int x=0; x<nbCols; x++) {
                        currentScore = scores->get(y,x);
                        if (currentScore > bestScores->get(y,x)) {
                           bestScores->set(y, x, currentScore);
                           loc1D = offset + offsetY * (mY->get(y,x)) + mX->get(y,x);
                           bestLocX->set(y, x, matX[loc1D]);  
                           bestLocY->set(y, x, matY[loc1D]); 
                           bestCoefs_a->set(y, x, ctile.A[r][0]);
                           bestCoefs_b->set(y, x, ctile.A[r][1]);
                           bestCoefs_d->set(y, x, ctile.A[r][3]);
                           bestCoefs_e->set(y, x, ctile.A[r][4]);
                           bestRanges->set(y, x, cr);
                           bestNormals->set(y, x, ctile.planeNorm);

                           if (epiShift) {
                              int centerLoc = offset + offsetY*y + x;
                              bestPerpX->set(y, x, matX[loc1D] - matX[centerLoc] +
                                             ctile.perpEpi[0]);
                              bestPerpY->set(y, x, matY[loc1D] - matY[centerLoc] + 
                                             ctile.perpEpi[1]);
                           }
                        }
                     }
                  }
      
                  // Free memory
                  tileR->free();
                  tileL->free();
                  delete tileR;
                  delete tileL;

               } // end loop on ranges

            } // end loop on plane normals
   
   
            // Housekeeping 
            scores->free();
            mX->free();
            mY->free();
            bestScores->free();
            bestLocX->free();
            bestLocY->free();
            bestCoefs_a->free();
            bestCoefs_b->free();
            bestCoefs_d->free();
            bestCoefs_e->free();
            bestRanges->free();
            bestNormals->free();
            if (epiShift) {
               bestPerpX->free();
               bestPerpY->free();
            }
            delete scores;
            delete mX;
            delete mY;
            delete bestScores;
            delete bestLocX;
            delete bestLocY;
            delete bestCoefs_a;
            delete bestCoefs_b;
            delete bestCoefs_d;
            delete bestCoefs_e;
            delete bestRanges;
            delete bestNormals;
            if (epiShift) {  
               delete bestPerpX;
               delete bestPerpY;
            }
 

            // Display progress
            #pragma omp critical 
               printProgress();
   
         } // end loop on tiles
   
         } // end pragma omp   
     
         
    
      // Filter process 1 (applied systematically):
      // - Filter out disparities that are outside the Right image footprint
      // - Cancel disparities whose correlation score is less than user threshold
      // - Count number of valid correlation
      //Note that the score is still "squared", hence the minScore2
      nbCorrValid = simpleCorrelationFilter(&corrScores, &locX, &locY, nlr, nsr,
                                            minScore2);
   
      // Filter process 2 (if required by user):
      // Apply median-of-sort filter. Look at local disparities around each pixels
      // and decide if it is an outlier or not
      if (statFilter || (!statFilter && pyr != 0)) {
         SimpleImage<int> * mask = nullptr;
         localCoherenceFilter(&corrScores, &locX, &locY,
                              coefs_a, coefs_b, coefs_d, coefs_e,
                              corrSz, 
                              statFilterExtent,statFilterScalerThreshold,
                              statFilterThresholdN, omp_on, mask);
   
         applyLocalCoherenceFilter(&corrScores, mask, 0.f);
         applyLocalCoherenceFilter(&locX, mask, 0.f);
         applyLocalCoherenceFilter(&locY, mask, 0.f);

         applyLocalCoherenceFilter(coefs_a, mask, 0.);
         applyLocalCoherenceFilter(coefs_b, mask, 0.);
         applyLocalCoherenceFilter(coefs_d, mask, 0.);
         applyLocalCoherenceFilter(coefs_e, mask, 0.);

         applyLocalCoherenceFilter(ranges, mask, 0.f);
         applyLocalCoherenceFilter<PigVector>(normals, mask, PigVector(0,0,0));
    
         if (epiShift) {
            applyLocalCoherenceFilter(epiOffX, mask, 0.f);
            applyLocalCoherenceFilter(epiOffY, mask, 0.f);
         }

         nbCorrValid = std::count(mask->linePtr(0), mask->linePtr(0)+nsl*nll, 1);
   
         if (mask != nullptr) {
            mask->free();
            delete mask;
         }
      }
    

      // On the last iteration of the pyramid loop
      if (pyr == 0) {

         // Mask pixels whose DN values are part of a mask.
         // This is related to noise in xyz M20 where black borders in the 
         // image + zipper compression effects create false matches that are not
         // filtered out. The idea is to create a mask around DN that needs to 
         // be filtered out. Any pixel in the Left image that is part
         // of the mask gets its disparity value set to 0.
         if (zvptst("MASK_ON")) {

            // Get DN to be masked
            float arrayDN[MAX_DNS];
            int def;
            status = zvp("MASK_DN", arrayDN, &count);
            std::vector<double> maskDNs;

            for (int i = 0; i < count; i++) {

               if (arrayDN[i] < 0 && (i < 1 || arrayDN[i-1] < 0 || arrayDN[i-1] >= -arrayDN[i])) {
                     zvmessage("MASK_DN values less than 0 and prior ref not correct (neg or > this)", "");
                     zabend();
               }
	       maskDNs.push_back(arrayDN[i]);
            }

            // Set the dilation extent
            int maskExtentX = 0;
            int maskExtentY = 0;

            // Are there any extension?
            int arrayExtent[2];
            zvp("MASK_EXTENT", arrayExtent, &count); 
            maskExtentX += arrayExtent[0];  
            if (count == 1) 
               maskExtentY += arrayExtent[0]; 
            else 
               maskExtentY += arrayExtent[1]; 

            // Are we accounting for correlation template size
            if (zvptst("MASK_TEMPLATE")) {
               maskExtentX += (corrSz[0]-1)/2;  
               maskExtentY += (corrSz[1]-1)/2;  
            }


            // Check if current loaded Left image is full resolution. If not reload it
            if (pyrlevel && downFactor!= 1) {
               imgL->free();
               delete imgL;
               imgL = loadImage(file_models[0], bands[0]);
            }

            // Account for pyramid level in the maskExtent. This is not clear cut.
            // The check on the DN values is done on the full res image, because downsampling
            // smears the values and the DN gets slightly altered, but enough to fail a DN 
            // comparison.
            maskExtentX *= downFactor;
            maskExtentY *= downFactor;


            //Make mask based on full resolution input image
            SimpleImage<int> * mask = getMask(imgL, maskDNs, maskExtentX, maskExtentY);


            for (int j=0; j < nll; j++) {
               for (int i=0; i < nsl; i++) {
                  if (mask->get(j*downFactor,i*downFactor) == 1) {
                     locX.set(j, i, 0);
                     locY.set(j, i, 0);
                     corrScores.set(j, i, 0);
                     coefs_a->set(j, i, 0);
                     coefs_b->set(j, i, 0);
                     coefs_d->set(j, i, 0);
                     coefs_e->set(j, i, 0);
                     coefs_g->set(j, i, 0);
                     coefs_h->set(j, i, 0);
                  }
               }
            }

            nbCorrValid = nsl*nll - std::count(corrScores.linePtr(0), 
                                               corrScores.linePtr(0)+nsl*nll, 0);
         }


         // Compute the average scale difference between the Left and Right to 
         // be stored in the output labels.
         avgScale = 0.0;
         if (nbCorrValid) {
            for (int j=0; j<locX.getNL(); j++) {
               for (int i=0; i<locX.getNS(); i++) {
                  if (locX.get(j, i) != 0) {
                     avgScale += sqrt(coefs_a->get(j, i) * coefs_e->get(j,i) - 
                                      coefs_b->get(j, i) * coefs_d->get(j,i) );
                  }
               }
            }
            avgScale /= nbCorrValid;
         }

     }


   } // end for (pyramid)

    
   // Time end of processing
   auto t2 = high_resolution_clock::now();


   // Now that correlation is done, get correlation score number right (need
   // to sqrt the score). We didn't do the sqrt during the correlation along 
   // the epipolar line to save processing time.
   int nll = imgL->getNL();
   int nsl = imgL->getNS();
   int nlr = imgR->getNL();
   int nsr = imgR->getNS();

   float *p = corrScores.linePtr(0);
   std::transform(p, p+nsl*nll, p, [](float x){ return std::sqrt(x);});


   // Make the disparity maps 1-based to be compliant with vicar
   // standard
   std::for_each(locX.linePtr(0), locX.linePtr(0)+nsl*nll, [](float &x){ if (x!=0) x+=1.0;});
   std::for_each(locY.linePtr(0), locY.linePtr(0)+nsl*nll, [](float &y){ if (y!=0) y+=1.0;});

   // Print number of valid correlation to console
   zvmessage("","");
   sprintf(msg,"Correlation coverage: %5.2f%%", float(nbCorrValid) / (nsl*nll) * 100.);
   zvmessage(msg, " "); 
   zvmessage("","");


   // **************** WRITE DATA FILES ********************** /

   zvmessage("Writing files to disk...", "");

   int unit_inp, unit_out;
   char filename_inp[PIG_MAX_FILENAME_SIZE+1];

   zvunit(&unit_out, "OUT", 1, NULL);
   zvopen(unit_out, "OP", "WRITE", "U_FORMAT", "REAL", "O_FORMAT", "REAL",
          "U_NS", nsl, "U_NL", nll, "OPEN_ACT", "AS", "U_NB", 2, "U_ORG", 
          "BSQ", NULL);
   zvplabel(unit_out, 0, 1);



   // Write output label
   PigMission *m = NULL;
   m = PigMission::getMissionObject(mission);    
   if (m != NULL) {
      PigLabelModel *labelModel_0 = NULL;
      labelModel_0 = m->createLabelModel(unit_out);
      if (labelModel_0) { 
         labelModel_0->setDisparity(file_models, file_models[1], nids, 
                                    "DISPARITY_MAP");
         labelModel_0->setDisparityExtra(nbCorrValid, 
                                         avgScale, 
                                         overlapCheck,
                                         pyrlevel);
      }
    }
    else zvmessage("Mission unknown, no specific Label writing","");


   // Write disparities
   for (int j = 0; j < nll; j++) 
      zvwrit(unit_out, locY.linePtr(j), "LINE", j+1, "BAND", 1, NULL);
   for (int j = 0; j < nll; j++) 
      zvwrit(unit_out, locX.linePtr(j), "LINE", j+1, "BAND", 2, NULL);

   zvclose(unit_out, "CLOS_ACT", "FREE", NULL);

   // Free memory
   locX.free();
   locY.free();





    // write quality image if needed

   if (outQuality) {
    
      char name[256];

      zvp("OUT_QUALITY", name, &count);
      zvunit(&unit_out, "OUT_QUALITY", 1, "U_NAME", name, NULL);
      zvopen(unit_out, "OP", "WRITE", "U_FORMAT", "REAL", "O_FORMAT", "REAL",
             "U_NS", nsl, "U_NL", nll, "OPEN_ACT", "AS", "U_NB", 1, "U_ORG", 
             "BSQ", NULL);
      zvplabel(unit_out, 0, 1);
      if (m) {
         PigLabelModel *labelModel = m->createLabelModel(unit_out);
         labelModel->setIdentification(NULL);
         labelModel->setDerivedImageType("QUALITY");
      }

      for (int j=0; j < nll; j++) 
         zvwrit(unit_out, corrScores.linePtr(j), "LINE", j+1, NULL);

      zvclose(unit_out, "CLOS_ACT", "FREE", NULL);
   }
 
   // Free memory
   corrScores.free();




    // Write affine coefficients file if needed

    if (outCoefs) {

	char name[256];

	zvp("OUT_COEFS", name, &count);
	zvunit(&unit_out, "OUT_COEFS", 1, "U_NAME", name, NULL);
	zvopen(unit_out, "OP", "WRITE", "U_FORMAT", "DOUB", "O_FORMAT", "DOUB",
               "U_NS", nsl, "U_NL", nll, "OPEN_ACT", "AS", "U_NB", 6, "U_ORG", 
               "BSQ", NULL);
	zvplabel(unit_out, 0, 1);
        if (m) {
           PigLabelModel *labelModel = m->createLabelModel(unit_out);
           labelModel->setIdentification(NULL);
           labelModel->setDerivedImageType("COEFFICIENTS");
        }

	for (int j=0; j < nll; j++) 
           zvwrit(unit_out, coefs_a->linePtr(j), "LINE", j+1, "BAND", 1, NULL);
	for (int j=0; j < nll; j++) 
           zvwrit(unit_out, coefs_b->linePtr(j), "LINE", j+1, "BAND", 2, NULL);
	for (int j=0; j < nll; j++) 
           zvwrit(unit_out, coefs_d->linePtr(j), "LINE", j+1, "BAND", 3, NULL);
	for (int j=0; j < nll; j++) 
           zvwrit(unit_out, coefs_e->linePtr(j), "LINE", j+1, "BAND", 4, NULL);
	for (int j=0; j < nll; j++) 
           zvwrit(unit_out, coefs_g->linePtr(j), "LINE", j+1, "BAND", 5, NULL);
	for (int j=0; j < nll; j++) 
           zvwrit(unit_out, coefs_h->linePtr(j), "LINE", j+1, "BAND", 6, NULL);
        
	zvclose(unit_out, "CLOS_ACT", "FREE", NULL);
   
    }

    // Free memory
    coefs_a->free();
    coefs_b->free();
    coefs_d->free();
    coefs_e->free();
    coefs_g->free();
    coefs_h->free();
    

    // End of writing files
    auto t3 = high_resolution_clock::now();

    zvmessage("********** marsecorr process done ***********", "");

    auto du = duration_cast<seconds>(t2 - t1); 
    sprintf(msg, "Core processing: %lld seconds.", du.count());
    zvmessage(msg,"");
 
    du = duration_cast<seconds>(t3 - t2); 
    sprintf(msg, "Writing files: %lld seconds.", du.count());
    zvmessage(msg,"");

    du = duration_cast<seconds>(t3 - t1); 
    sprintf(msg, "Total processing time: %lld seconds.", du.count());
    zvmessage(msg,"");

    zvmessage("", "");


}  // end main







tiler initializeTiler(camGeometry & leftC, int nll, int nsl, int tileSize[2], int corrSz[2]) {

   PigPoint center, look;

   tiler newT;

   // Store the size of the main image to tile
   newT.nll = nll;
   newT.nsl = nsl;

   // How many tile can be fitted in X direction
   int nbTilesX = nsl/tileSize[1];
   // Is there enough pixel in the "left over" to get at least one correlation?
   if ((nsl % tileSize[1]) > corrSz[1])
      nbTilesX += 1;

   // How many tile can be fitted in Y direction
   int nbTilesY = nll/tileSize[1];
   // Is there enough pixel in the "left over" to get at least one correlation?
   if ((nll % tileSize[0]) > corrSz[0])
      nbTilesY += 1;

   // Total number of tiles
   newT.nbTilesX = nbTilesX;
   newT.nbTilesY = nbTilesY;
   newT.nbTiles = nbTilesX * nbTilesY;



   // Now that the number of tiles is defined, instanciate the list of tiles
   newT.tiles.resize(newT.nbTiles);

   // For each tile, compute some of its characteristics: boundaries, distance
   // from image center, neighbor tiles indices.
   int index = 0;
   for (int i=0; i<nbTilesY; i++) {

      int sY = i*tileSize[1];
      int eY = sY+tileSize[1]-1;
      if (i == (nbTilesY-1))
         eY = nll-1;

      for (int j=0; j<nbTilesX; j++) {

         int sX = j*tileSize[0];
         int eX = sX+tileSize[0]-1;
         if (j == (nbTilesX-1))
            eX = nsl-1;

         newT.tiles[index].xs = sX;
         newT.tiles[index].ys = sY;
         newT.tiles[index].xe = eX;
         newT.tiles[index].ye = eY;
         newT.tiles[index].xc = sX + (eX-sX)/2;
         newT.tiles[index].yc = sY + (eY-sY)/2;
         newT.tiles[index].radius = std::sqrt(
                                     std::pow(newT.tiles[index].xc - (nsl - 1)/2.0, 2) +
                                     std::pow(newT.tiles[index].yc - (nll - 1)/2.0, 2));

         // Get look angle of tile center pixel
         leftC.cm->LStoLookVector(newT.tiles[index].yc, newT.tiles[index].xc, 
                                  center, look, leftC.cs);
         look.normalize(); // Normalization, just in case
	 newT.tiles[index].look = look;

         // Get the indices of the tiles neighbors to the current one
         newT.tiles[index].N = getNeighbors(index, nbTilesX, nbTilesY);

         // Set status of tile to unprocessed yet
         // TODO nop
         //newT.tiles[index].status = UNPROCESSED;

         index++;
      }
   }
 
   return newT;
}


std::set<int> getNeighbors(int index, int nbTilesX, int nbTilesY) {

   int X = index % nbTilesX;
   int Y = index / nbTilesX;

   std::set<int> out;

   if (Y>0)
      out.insert(index - nbTilesX);
   if (X>0)
      out.insert(index - 1);
   if (X<(nbTilesX-1))
      out.insert(index + 1);
   if (Y<(nbTilesY-1))
      out.insert(index + nbTilesX);

   return out;
}










// Bicubic resampling function based on resampling matrices. Resampling matrices
// is a 2-band image containing the (x,y) location in input img that we want to
// be resampled. The function returns the resampled image, whose size is ASSUMED
// equal to the x/y size of the resampling matrices.
SimpleImage<float> * resampleImageBicubic(SimpleImage<float> *img, 
                                          int nbX, int nbY,
                                          std::valarray<float> &matX, 
                                          std::valarray<float> &matY) { 

   int m, n;
   unsigned int nsi, nli, ntot;
   float rsamp, rline, dec, dec2, dec3, f1_, f0, f1, f2, b1_, b0, b1, b2, dn;   

   nsi = img->getNS();
   nli = img->getNL();

   ntot = matX.size();

   // Check input matrices size and output image size are the same
   if (ntot != (nbX*nbY)) {
      zvmessage("input mapping matrices and output image size are different!","");
      zabend();
   }

   // Allocate output image
   SimpleImage<float> * out = nullptr;
   out = new (std::nothrow) SimpleImage<float>(nbY, nbX);
   if (out == nullptr) {
      zvmessage("Output image allocation failed!","");
      zabend();
   }
   out->zero();


   float * ptrOut = out->linePtr(0);

   // Iterate over the pixel and resample if matrices have valid values at that 
   // pixel
   for (int i=0 ; i<ntot; i++) {

      rsamp = matX[i];
      rline = matY[i];

      // Check that x,y coordinates are not inf/nan/subnormal
      // If proper work is done before, that shouldn't happen, but if 
      // "non-normal" values slip through, it's a segfault
      if (!std::isnormal(rsamp) || !std::isnormal(rline)) 
         continue;
      

      // if out of bound matrice values, move to next pixel. Account for
      // bicubic resampling margin.
      if (rsamp < 1  || rsamp > (nsi-3) || rline < 1 || rline > (nli-3)) 
         continue;
      

      m=(int)rsamp;
      n=(int)rline;
      dec = rsamp - m;
      dec2 = dec*dec;
      dec3 = dec2*dec;
                        
      // Computing coefficients in one direction 
                                                   
      f1_ = img->get(n-1,m-1);
      f0  = img->get(n-1,m  );
      f1  = img->get(n-1,m+1);
      f2  = img->get(n-1,m+2);
      b1_ = 0.5 * ( 2*f0 + dec*(f1-f1_) + dec2*(2*f1_ - 5*f0 + 4*f1 - f2)
                    + dec3*(3*f0 - 3*f1 + f2 -f1_));
                                                      
      f1_ = img->get(n,m-1);
      f0  = img->get(n,m  );
      f1  = img->get(n,m+1);
      f2  = img->get(n,m+2);
      b0  = 0.5 * ( 2*f0 + dec*(f1-f1_) + dec2*(2*f1_ - 5*f0 + 4*f1 - f2)
                    + dec3*(3*f0 - 3*f1 + f2 -f1_));
                                                      
      f1_ = img->get(n+1,m-1);
      f0  = img->get(n+1,m  );
      f1  = img->get(n+1,m+1);
      f2  = img->get(n+1,m+2);
      b1  = 0.5 * ( 2*f0 + dec*(f1-f1_) + dec2*(2*f1_ - 5*f0 + 4*f1 - f2)
                    + dec3*(3*f0 - 3*f1 + f2 -f1_));
                                                      
      f1_ = img->get(n+2,m-1);
      f0  = img->get(n+2,m  );
      f1  = img->get(n+2,m+1);
      f2  = img->get(n+2,m+2);
      b2  = 0.5 * ( 2*f0 + dec*(f1-f1_) + dec2*(2*f1_ - 5*f0 + 4*f1 - f2)
                    + dec3*(3*f0 - 3*f1 + f2 -f1_));
                                                  
      // Then combining in the other direction 
                                                    
      dec = rline - n;
      dn  = 0.5 * ( 2*b0 + dec*(b1-b1_) + dec*dec*(2*b1_ - 5*b0 + 4*b1 - b2)
                    + dec*dec*dec*(3*b0 - 3*b1 + b2 -b1_));
       
 
      // Saving resampled pixel
      //out[i] = dn;
      ptrOut[i] = dn;
      //out->set(i/ns, i%ns, dn);

      // For nearestneighbor: 
      //out[i] = img->get((int)(rline+0.5),(int)(rsamp+0.5));
      //out->set(i,j, img->get((int)(rline+0.5),(int)(rsamp+0.5)));
   }

   return out;
}






// Utility function to load an image in memory. Input is the index of the image 
// to load.
SimpleImage<float> * loadImage(PigFileModel * file_model, int band) {


   // If current file is open, close it first
   if (file_model->isFileOpen())
      file_model->closeFile();

   // Open the file for reading
   zvopen(file_model->getUnit(), "OP", "READ", "U_FORMAT", "REAL", 
          "OPEN_ACT", "SA", NULL);

   file_model->setFileOpen(TRUE);

   // Initialize a container to load the image in memory
   SimpleImage<float> * image = new SimpleImage<float>(file_model->getNL(), 
                                                       file_model->getNS());

   // Load the image in memory
   for (int j=0; j<file_model->getNL(); j++)
      zvread(file_model->getUnit(), image->linePtr(j), "BAND", band, 
             "LINE", j+1, NULL);

   // Close the file as it is not needed anymore
   file_model->closeFile();

   // Not sure if we need to zvclose the unit here:
   zvclose(file_model->getUnit(), "CLOS_ACT","FREE", NULL);

   return image;
}






// This function computes if a segment, given by the endpoints intersect a
// rectangle given by the top-left and bottom-right points. The algorithm is
// based on parametric line clipping method.
// This implementation only works for 2D situations and, although it is 
// currently tailored for a rectangle, it could easily be extended to a convex
// polygon. Only the function interface and early initialization would need to
// be changed.
//
int segmentFrameIntersect(const float &xmin, const float &ymin, 
                          const float &xmax, const float &ymax,
                          const float &x0, const float &y0, 
                          const float &x1, const float &y1,
                          float &xout0, float &yout0, 
                          float &xout1, float &yout1) {

   int n = 4;         // Number of corners in the rectangle
   float  tE = 0;     // the maximum entering segment parameter
   float  tL = 1;     // the minimum leaving segment parameter
   float  t, N, D;    // intersect parameter t = N / D
   float polygon[(n+1)*2];

  
   // Some checking on the segment point
   if (x0 == x1 && y0 == y1) {
      zvmessage("Segment cannot be defined from 2 identical points","");
      return 0;
   }

   polygon[0] = xmin; polygon[1] = ymin; 
   polygon[2] = xmax; polygon[3] = ymin; 
   polygon[4] = xmax; polygon[5] = ymax; 
   polygon[6] = xmin; polygon[7] = ymax; 
   polygon[8] = xmin; polygon[9] = ymin; 
   

   for (int i=0; i < n; i++) {            // polygon edge i -> i+1

      N = (polygon[(i+1)*2] - polygon[i*2]) * (y0 - polygon[i*2+1]) - 
          (polygon[(i+1)*2+1] - polygon[i*2+1]) * (x0 - polygon[i*2]);

      D = - ( (polygon[(i+1)*2] - polygon[i*2]) * (y1 - y0) -
              (polygon[(i+1)*2+1] - polygon[i*2+1]) * (x1 - x0) );

      if (fabs(D) < EPSILON) {    // Segment is parallel to this edge:
         if (N < 0)               // if S0 is outside this edge, then segment is
            return 0;             // parallel and outside of polygon -> no cross
         else                     // else, need to check other edges
            continue;          
      }

      t = N / D;
      if (D < 0) {            // segment is entering the polygon across this edge
         if (t > tE) {        // new max tE
            tE = t;
            if (tE > tL)      // segment enters after leaving polygon
               return 0;
         }
      }
      else {                  // segment is leaving across this edge
         if (t < tL) {        // new min tL
            tL = t;
            if (tL < tE)      // segment leaves before entering polygon
               return 0;
         }
      }
   }

    xout0 = x0 + (x1 - x0)*tE; //tEIS->P0 = S.P0 + tE * dS;   // = P(tE) = point where S enters polygon
    yout0 = y0 + (y1 - y0)*tE; //tEIS->P0 = S.P0 + tE * dS;   // = P(tE) = point where S enters polygon
    xout1 = x0 + (x1 - x0)*tL; //IS->P1 = S.P0 + tL * dS;   // = P(tL) = point where S leaves polygon
    yout1 = y0 + (y1 - y0)*tL; //IS->P1 = S.P0 + tL * dS;   // = P(tL) = point where S leaves polygon
    return 1;
}
 








// Downsample the image with a power 2 factor (2,4,8...).
// A gaussian prefiltering is applied before resampling the image.
// Note filter+resampling could be done in one pass, but re-used pieces of code
// eleswhere in vicar, so it was easier implementation.
void downsample(SimpleImage<float> *imgIn, SimpleImage<float> *&imgOut,
                const int &downFactor, int omp) {

   
    int nl = imgIn->getNL();
    int ns = imgIn->getNS();
    int nlNew = nl/downFactor;
    int nsNew = ns/downFactor;

    SimpleImage<float> *save_output = imgOut;

    // Copy input for inplace low-pass filtering
    SimpleImage<float> * imgBlur = new SimpleImage<float>(nl, ns);
    for (int l=0; l<nl; l++)       
       for (int s=0; s<ns; s++)
          imgBlur->set(l, s, imgIn->get(l,s));


    // Particular case where downFactor == 1, i.e. no downsampling, just a copy
    // Also for downFactor <1 which means upsampling, not dealt here
    if (downFactor <= 1) {

       imgOut = imgBlur;

       if (save_output != nullptr) {
          save_output->free();
          delete save_output;
       }
   
       return;
    }

 
    // Back at the intended business...

    // First, low-pass the image. This is done using a gaussian filtering

    // Get gaussian sigma - According to litterature, a sigma defined as:
    // sigma = Cst * SQRT(downFactor^2 - 1) with Cst ~= 0.8 is a good 
    // compromise for standard camera
    float sigma = 0.8 * sqrt(downFactor*downFactor - 1.0);
    GaussianBlur(imgBlur, sigma, sigma, omp);

   
    // Second, decimate the low-passed image according to the decimation. Need
    // to interpolate values as "new" pixel is in the middle of the pixel 
    // "patch".

    // Compute where to resample the low-passed image
    std::valarray<float> matX(nlNew*nsNew);
    std::valarray<float> matY(nlNew*nsNew);
    for (int j=0; j<nlNew ; j++) {
       for (int i=0; i<nsNew ; i++) {
          matX[j*nsNew+i] = (0.5+i)*downFactor-0.5;
          matY[j*nsNew+i] = (0.5+j)*downFactor-0.5;
       }
    }    

        
    // Resample at proper location
    imgOut = resampleImageBicubic(imgBlur, nsNew, nlNew, matX, matY); 

    // No need anymore of the blurred image
    imgBlur->free();
    delete imgBlur;

    if (save_output != nullptr) {
       save_output->free();
       delete save_output;
    }

}





void downsampleTiling(SimpleImage<int> *tile_in,
		      SimpleImage<int> *&tile_out,
                      const int &downFactor)
{

   // Adjust the tiling factor according to a x2 downsampling.
   // If image is upsample at a given pixel, lower the tilinge index by 1 level 
   // (factor of 2). Otherwise set tile index level to 1 (full frequency 
   // resolution w.r.t spatial resolution)
   int nl = tile_in->getNL();
   int ns = tile_in->getNS();
   SimpleImage<int> *save_output = tile_out;
   tile_out = new SimpleImage<int>(nl/downFactor, ns/downFactor);

   for (int j=0; j < nl; j+=downFactor) {
      int jj = j / downFactor;
      for (int i=0; i < ns; i+=downFactor) {
         int ii = i / downFactor;
         if (tile_in->get(j,i) > downFactor)
            tile_out->set(jj, ii, pow(2, log2(tile_in->get(j,i))-log2(downFactor)));
         else
	    tile_out->set(jj, ii, 1);
      }
   }

   if (save_output != NULL) {
      save_output->free();
      delete save_output;
   }
}







// Convolution of buffer (one line or column of the image) with the gaussian
// kernel. This is a time intensive routine, so deserves attention for 
// efficiency.
void ConvBufferFast(float *buffer, float *kernel, int rsize, int ksize)
{
   int i;
   float *bp, *kp, *endkp, *mp;
   float sum, sum2;
   float mask[rsize+ksize-1];
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
void ConvHorizontal(SimpleImage<float> *image, float *kernel, int ksize, int omp_on)
{
  int r, c, i, halfsize;
  int width = image->getNS();
  int height = image->getNL();
//  float buffer[width+ksize-1];
  halfsize = ksize / 2;


#pragma omp parallel private (r, c, i) if (omp_on)
{
  float buffer[width+ksize-1];

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
//void ConvVertical(vector<float>& image, int width, int height, 
void ConvVertical(SimpleImage<float> *image, float *kernel, int ksize, int omp_on)
{
  int r, c, i, halfsize;
  int width = image->getNS();
  int height = image->getNL();
//  float buffer[height+ksize-1];
  halfsize = ksize / 2;

#pragma omp parallel private(r, c, i) if (omp_on)
{
  float buffer[height+ksize-1];

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



// Convolve image with a Gaussian of width sigma and store result back in image.
// This routine creates the Gaussian kernel, and then applies it in horizontal 
// and vertical directions.
void GaussianBlur(SimpleImage<float> *image, const float sigmaX, const float sigmaY, int omp_on)
{
  int i, nbSigmas = 3;
  int ksizeX = 2*nbSigmas*sigmaX+1;
  int ksizeY = 2*nbSigmas*sigmaY+1;
  float x, kernelX[ksizeX], kernelY[ksizeY],sum = 0.0;

  // Fill in kernel values for X.
  for (i = 0; i < ksizeX; i++) {
    x = i - ksizeX / 2;
    kernelX[i] = exp(- x * x / (2.0 * sigmaX * sigmaX));
    sum += kernelX[i];
  }
  // Normalize kernel values to sum to 1.0.
  for (i = 0; i < ksizeX; i++)
    kernelX[i] /= sum;

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



  // Convolve image with gaussian kernel in the horizontal direction
  ConvHorizontal(image, kernelX, ksizeX, omp_on);

  // Convolve image with gaussian kernel in the vertical direction
  ConvVertical(image, kernelY, ksizeY, omp_on);
}



void computeRelativeGSDfromCenterPixel(camGeometry &camG) {

   PigPoint center1, center2, XYZcenter, XYZold, XYZnew;
   PigVector vector1, vector2;

   std::vector<double> & gsd = camG.relativeGSD;
   gsd.clear();

   double pixCenterX, pixCenterY;
   pixCenterX = (camG.ns-1.0)/2.0;
   pixCenterY = (camG.nl-1.0)/2.0;
   
   int length = floor(sqrt(pixCenterX*pixCenterX + pixCenterY*pixCenterY) + 1);

   // Compute XYZ of center pixel projected out at 1m 
   camG.cm->LStoLookVector(pixCenterY, pixCenterX, center1, vector1, camG.cs);
   
   vector1.normalize(); //necessary?
   XYZcenter = center1 + vector1;
   XYZold = XYZcenter;


   // Loop over each pixel along the radius (spaced 1 pixel between each loop),
   // project these pixels out on the plane and compute distance from previous 
   // XYZ.
   // Radial symetry is assumed, so we arbitrarily scan in the X direction.
   //
   // If FOV > 180 degrees, this approach does not work as the plane is unreachable.
   // We check for that and for pixel that are beyond this limit, we arbitrarily set
   // the resolution equals to the closest valid one.
   for (double i=1; i<length; i++) {

      camG.cm->LStoLookVector(pixCenterY, pixCenterX+i, center2, vector2, camG.cs);

      // Check that current pixel is not looking backward (lookDirection angle
      // between camera look direction and look direction of pixel at current 
      // radius larger than 90 degrees). If it does, copy the previous GSD which 
      // *should* be valid. If not then something is wrong.
      if ((vector1 % vector2) < 0) {
         gsd.push_back(gsd.back());
         continue;
      }

      // Get intersection (XYZ) of the current pixel ray with plane 
      // XYZcenter and vector1 define the plane to project the ray onto
      XYZnew = center2 + vector2 * (((XYZcenter - center2) % vector1) / (vector1 % vector2));

      // Get distance with XYZ from previous pixel (i.e., spaced 1 pix away)
      gsd.push_back((XYZnew-XYZold).magnitude());

      XYZold = XYZnew;      
   }

   // Save GSD of center pixel at 1m out
   camG.GSDcenterAt1m = (float)gsd[0];

   // Normalize GSD with GSD from center pixel
   for (auto& v: gsd) 
      v /= camG.GSDcenterAt1m;
   
}





// Get the tiling level of the Left tile center and the list of tiling
// levels corresponding to the Right image pixels associated with the
// range list to be tested
void getTilingPerRange (camGeometry & leftC, camGeometry & rightC, tile &t) {

   PigPoint OpL, OpR, P;
   PigVector VpL, VpR, OP;
   double x, y;


   // Get the Left image tiling factor for this tile if any
   if (leftC.downsamplingTile != nullptr)
      t.tilingL = (leftC.downsamplingTile)->get(t.yc, t.xc);
   else 
      t.tilingL = 1;

    

   // Get the look vector and optical center of the Left tile center pixel
   leftC.cm->LStoLookVector((double)t.yc, (double)t.xc, OpL, VpL, leftC.cs);


   // For each projection range, compute the XYZ and then the corresponding
   // x/y in the Right image. Look up the tiling factor for that pixel and
   // store it.
      
   // Clear containers for upcoming update
   t.tilingRs.clear();      

   for (auto rx:t.ranges) {

      // Get the XYZ coordinates of point at given range
      P = OpL + VpL * rx;

      // Get Right pixel corresponding the this XYZ
      rightC.cm->XYZtoLS(P, FALSE, &y, &x, rightC.cs); 

      // Get R image tiling (downsampling/upsampling stuff) at this location
      if (rightC.downsamplingTile != nullptr) {
         // Make sure no out of bound stepping
         // If so, get closest tiling factor
         int xInt = (int)(x + 0.5);
         xInt = (xInt < 0) ? 0 : xInt; 
         xInt = (xInt >= rightC.ns) ? rightC.ns-1 : xInt; 
         int yInt = (int)(y + 0.5);
         yInt = (yInt < 0) ? 0 : yInt; 
         yInt = (yInt >= rightC.nl) ? rightC.nl-1 : yInt; 
         t.tilingRs.push_back((rightC.downsamplingTile)->get(yInt, xInt));
      }
      else
         t.tilingRs.push_back(1);

   }    
}







inline int getImageIndex(std::set<float> &resSet, float res) {

   auto it = resSet.lower_bound(res);

   // If out of bound, return last element index
   if (it == resSet.end())
      return resSet.size()-1;

   // If we are at the beginning of the set, then nothing to do,
   // return
   if (it == resSet.begin())
      return 0;

   // Value of the "next" resolution available
   float resNext = *it;

   // Value of the "previous" resolution available.
   float resPrev = *(--it);

   // Check if the resolution asked is closer to previous or next.
   // Rounding of sort.
   // If closer to resNext, re-increment iterator to get to "next"
   // otherwise, do nothing as iterator is at the correct location
   // thanks to previous code line
   // Note that there is probably a better way of chosing the resolution
   // based on the distance between prev and next with respect to 
   // aliasing. For now, just a basic rounding.
   if ((res - resPrev) > (resNext - res))
      it++;

   // return the index of the selected resolution
   return std::distance(resSet.begin(), it);

}



void computeRtoLprojectionMatrices(const std::array<float,9> &H, 
                                   const int &xtlR, const int &ytlR,
                                   const int &xbrR, const int &ybrR,
                                   std::valarray<float> &matX,
                                   std::valarray<float> &matY) {

   int nbPix = (xbrR-xtlR+1) * (ybrR-ytlR+1);

   std::valarray<float> xarr(nbPix);  // x in L image
   std::valarray<float> yarr(nbPix);  // y in L image

   // Compute coordinates of all pixels in area in L
   long index = 0;
   for (int y=ytlR; y<(ybrR+1); y++) {
      for (int x=xtlR; x<(xbrR+1); x++) {
         xarr[index] = x;
         yarr[index] = y;
         index++;
      }
   }

   // [2] - Using homography, compute corresponding pixel in R
   applyHomography(H, xarr, yarr, matX, matY);  

}





void applyHomography(const std::array<float,9> &H, 
                     const std::valarray<float> &xarr,
                     const std::valarray<float> &yarr,
                     std::valarray<float> &matX,
                     std::valarray<float> &matY) {

   if (xarr.size() != yarr.size()) {
      zvmessage("Error: Input X and Y array must have same size","");
      zabend();
   }

   if (xarr.size() != matX.size())
      matX.resize(xarr.size());

   if (xarr.size() != matY.size())
      matY.resize(xarr.size());

   // compute the homogeneous to inhomogeneous normalization
   std::valarray<float> z = H[6] * xarr + H[7] * yarr + H[8];

   // compute the inhomogeneous x coordinates resulting from the projective
   // transformation applied to (xarr, yarr)
   matX = (H[0]*xarr + H[1]*yarr + H[2]) / z;
   matY = (H[3]*xarr + H[4]*yarr + H[5]) / z;

}



void applyAffine(const std::array<float,6> &A, 
                 const std::valarray<float> &xarr,
                 const std::valarray<float> &yarr,
                 std::valarray<float> &matX,
                 std::valarray<float> &matY) {

   if (xarr.size() != yarr.size()) {
      zvmessage("Error: Input X and Y array must have same size","");
      zabend();
   }

   if (xarr.size() != matX.size())
      matX.resize(xarr.size());

   if (xarr.size() != matY.size())
      matY.resize(xarr.size());

   // apply the affine transform
   matX = A[0]*xarr + A[1]*yarr + A[2];
   matY = A[3]*xarr + A[4]*yarr + A[5];

}


int computeHomography(const std::vector<tiePoint> & ties, 
                      std::array<float, 9> & H) {

   float txL, tyL, txR, tyR, sL, sR;

   int nbTies = ties.size();
   int nbTies2 = 2 * nbTies;

   // Need at least four ties to compute homography
   if (nbTies < 4)
      return 1;


   // [1] Normalization of the input data
   // Recenter and rescale the (x,y) coordinates of one reference (e.g., Left
   // image) such that points are centered around 0, and the average distance 
   // between points and 0 is sqrt(2). We have a translation+scale transform 
   // that we store.
   // Do same thing with points of the other reference (e.g., Right image) 
      
   // Compute the centroid of R and L points
   txL = tyL = txR = tyR = 0.0;
   for (auto p:ties) {
      txL += p.xL;
      tyL += p.yL;
      txR += p.xR;
      tyR += p.yR;
   }
   txL /= (float)(-nbTies);
   tyL /= (float)(-nbTies);
   txR /= (float)(-nbTies);
   tyR /= (float)(-nbTies);

   // Compute the scaling factor such that the average distance of the points
   // with respect to the centroid is sqrt(2) for both L and R
   sL = sR = 0.0;
   for (auto p:ties) {
      sL += sqrt((p.xL + txL) * (p.xL + txL) + (p.yL + tyL) * (p.yL + tyL));
      sR += sqrt((p.xR + txR) * (p.xR + txR) + (p.yR + tyR) * (p.yR + tyR));
   }
   if (sL == 0)
      return 1;
   else 
      sL  = sqrt(2) * (float)nbTies / sL; 


   if (sR == 0)
      return 1;
   else 
      sR  = sqrt(2) * (float)nbTies / sR; 

   // [2] Compute the new coordinates of the translated/scaled points and 
   // store them directly in a matrix form following the DLT algorithm 
   // and solve for the homography transform H.
   // 5 ties, gives 10 equations, with the 9 parameters of the Homography
   // transform
   MatrixXf mm(nbTies2, 9);
   for (int i=0; i < nbTies; i++ ) {
      mm(2*i,0) = 0.0;
      mm(2*i,1) = 0.0;
      mm(2*i,2) = 0.0;
      mm(2*i,3) = -(sL * (ties[i].xL + txL));
      mm(2*i,4) = -(sL * (ties[i].yL + tyL));
      mm(2*i,5) = -1.0;
      mm(2*i,6) = sR*(ties[i].yR+tyR)  *  sL*(ties[i].xL + txL);
      mm(2*i,7) = sR*(ties[i].yR+tyR)  *  sL*(ties[i].yL + tyL);
      mm(2*i,8) = sR*(ties[i].yR+tyR);

      mm(2*i+1,0) = sL*(ties[i].xL + txL);
      mm(2*i+1,1) = sL*(ties[i].yL + tyL);
      mm(2*i+1,2) = 1.0;
      mm(2*i+1,3) = 0.0;
      mm(2*i+1,4) = 0.0;
      mm(2*i+1,5) = 0.0;
      mm(2*i+1,6) = -sR*(ties[i].xR+txR) * sL*(ties[i].xL + txL);
      mm(2*i+1,7) = -sR*(ties[i].xR+txR) * sL*(ties[i].yL + tyL);
      mm(2*i+1,8) = -sR*(ties[i].xR+txR);
   }

   // Solve for x'=Hx using SVD
   JacobiSVD<MatrixXf> svd(mm, ComputeThinU | ComputeThinV);

   // The singular vector corresponding to the smallest singular value is the
   // solution (H), and corresponds to the last column of the V matrix.
   // Note that H is here with respect to the centered/scaled ref system. Let's
   // call it Hcentered
   int dim = (nbTies2 > 9) ? 9 : nbTies2;  // Because of ThinV rule - see doc.
   MatrixXf m(9, dim);
   m = svd.matrixV();

   // [3] Use the transform/scales to transform back the Hcentered to the 
   // original coordinate system 
   // H = (TR)^-1 Hcentered TL
   // with TL the "translation then scale" tranform matrix for the L coord sys
   // and TR^-1, the inverse of the "translation then scale" transform matrix
   // for the R coord sys

   // Htemp = Hcentered TL
   H[0] = m(0,dim-1) * sL;
   H[1] = m(1,dim-1) * sL;
   H[2] = m(0,dim-1) * sL * txL + m(1,dim-1) * sL * tyL + m(2,dim-1);
   H[3] = m(3,dim-1) * sL;
   H[4] = m(4,dim-1) * sL;
   H[5] = m(3,dim-1) * sL * txL + m(4,dim-1) * sL * tyL + m(5,dim-1);
   H[6] = m(6,dim-1) * sL;
   H[7] = m(7,dim-1) * sL;
   H[8] = m(6,dim-1) * sL * txL + m(7,dim-1) * sL * tyL + m(8,dim-1);


   // (TR)^-1 Htemp
   float txR_inv = -txR*sR;
   float tyR_inv = -tyR*sR;
   float sR_inv = 1.0/sR;
   H[0] = sR_inv * H[0] + sR_inv * txR_inv * H[6];
   H[1] = sR_inv * H[1] + sR_inv * txR_inv * H[7];
   H[2] = sR_inv * H[2] + sR_inv * txR_inv * H[8];
   H[3] = sR_inv * H[3] + sR_inv * tyR_inv * H[6];
   H[4] = sR_inv * H[4] + sR_inv * tyR_inv * H[7];
   H[5] = sR_inv * H[5] + sR_inv * tyR_inv * H[8];
   //H[6] = H[6];
   //H[7] = H[7];
   //H[8] = H[8];

//FOR DEBUG
/*
std::cout << "********* DEBUG HOMOGRAPHY *********" << std::endl;
float hX, hY, hW;
hX = H[0]*(ties[0].xL) + H[1]*(ties[0].yL) + H[2]; 
hY = H[3]*(ties[0].xL) + H[4]*(ties[0].yL) + H[5]; 
hW = H[6]*(ties[0].xL) + H[7]*(ties[0].yL) + H[8]; 

std::cout << hX << " " << hY << " " << hW << '\n';

std::cout << ties[0].xR << ": " << hX/hW << '\n';
std::cout << ties[0].yR << ": " << hY/hW << '\n' << '\n';

hX = H[0]*ties[1].xL + H[1]*ties[1].yL + H[2]; 
hY = H[3]*ties[1].xL + H[4]*ties[1].yL + H[5]; 
hW = H[6]*ties[1].xL + H[7]*ties[1].yL + H[8]; 
std::cout << ties[1].xR << ": " << hX/hW << '\n';
std::cout << ties[1].yR << ": " << hY/hW << '\n' << '\n';

hX = H[0]*ties[2].xL + H[1]*ties[2].yL + H[2]; 
hY = H[3]*ties[2].xL + H[4]*ties[2].yL + H[5]; 
hW = H[6]*ties[2].xL + H[7]*ties[2].yL + H[8]; 
std::cout << ties[2].xR << ": " << hX/hW << '\n';
std::cout << ties[2].yR << ": " << hY/hW << '\n' << '\n';

hX = H[0]*ties[3].xL + H[1]*ties[3].yL + H[2]; 
hY = H[3]*ties[3].xL + H[4]*ties[3].yL + H[5]; 
hW = H[6]*ties[3].xL + H[7]*ties[3].yL + H[8]; 
std::cout << ties[3].xR << ": " << hX/hW << '\n';
std::cout << ties[3].yR << ": " << hY/hW << '\n' << '\n';

hX = H[0]*ties[4].xL + H[1]*ties[4].yL + H[2]; 
hY = H[3]*ties[4].xL + H[4]*ties[4].yL + H[5]; 
hW = H[6]*ties[4].xL + H[7]*ties[4].yL + H[8]; 
std::cout << ties[4].xR << ": " << hX/hW << '\n';
std::cout << ties[4].yR << ": " << hY/hW << '\n';

std::cout << "*************************************" << '\n' << '\n';
*/

   return 0;
}




// Affine transform using SVD

int computeAffineTransform(const std::vector<tiePoint> & ties, 
                           std::array<float, 6> & outA) {

   int nbTies = ties.size();

   // Need at least three ties to compute affine transform
   if (nbTies < 3)
      return 1;

   // Use of standard Least Square to define the affine transform
   // parameters
   // AX = B ---> X = inverse(At A) (At B)
   MatrixXf A(nbTies*2, 6);
   VectorXf B(nbTies*2);

   // Fill the matrix A and vector B
   for (int i=0; i<nbTies; i++) {
      A(2*i, 0) = ties[i].xL;
      A(2*i, 1) = ties[i].yL;
      A(2*i, 2) = 1.0;
      A(2*i, 3) = 0.0;
      A(2*i, 4) = 0.0;
      A(2*i, 5) = 0.0;

      A(2*i+1, 0) = 0.0;
      A(2*i+1, 1) = 0.0;
      A(2*i+1, 2) = 0.0;
      A(2*i+1, 3) = ties[i].xL;
      A(2*i+1, 4) = ties[i].yL;
      A(2*i+1, 5) = 1.0;

      B(2*i)   = ties[i].xR;
      B(2*i+1) = ties[i].yR;
   }


   // Solve using SVD
   JacobiSVD<MatrixXf> svd(A, ComputeThinU | ComputeThinV);
   Matrix<float, 6, 1> X = svd.solve(B);


   // Save affine transform parameters
   outA[0] = X(0);
   outA[1] = X(1);
   outA[2] = X(2);
   outA[3] = X(3);
   outA[4] = X(4);
   outA[5] = X(5);

   return 0;

/*
   // For debug
    
   //for (int i=0; i< nbTies; i++)
   //   std::cout << ties[i].xR << " -> " 
   //             << outA[0]*ties[i].xL + outA[1]*ties[i].yL + outA[2] 
   //             << "      " << ties[i].yR << " -> " 
   //             << outA[3]*ties[i].xL + outA[4]*ties[i].yL + outA[5] << '\n';

   MatrixXf AA(2,2);
   AA(0,0) = outA[0];
   AA(0,1) = outA[1];
   AA(1,0) = outA[3];
   AA(1,1) = outA[4];
   JacobiSVD<MatrixXf> svd2(AA, ComputeThinU | ComputeThinV);

   std::cout << "Singular values: " << std::endl << svd2.singularValues() << '\n';
   std::cout << "Left Singular vectors: " << std::endl << svd2.matrixU() << '\n';
   std::cout << "Right Singular vectors: " << std::endl << svd2.matrixV() << '\n';
*/

}





// Extract a subset (hard copy) of an image from the coordinates of the left 
// pixel of the subset and number of samples and lines
SimpleImage<float> * getTile(SimpleImage<float> * imgIn,
                             const int xStart,
                             const int yStart,
                             const int nbX,
                             const int nbY) {


   int xEnd = imgIn->getNS()-1;
   int yEnd = imgIn->getNL()-1;


   SimpleImage<float> * imgOut = nullptr;
   imgOut = new (std::nothrow) SimpleImage<float>(nbY, nbX);
   if (imgOut == nullptr) {
      zvmessage("Failed SimpleImage allocation","");
      zabend();
   }
   imgOut->zero();


   int locX = xStart;
   int locY = yStart;
   for (int j=0; j<nbY; j++) {

      if (locY < 0 || locY > yEnd) {
         locY++;
         continue;
      } 

      locX = xStart;

      for (int i=0; i<nbX; i++) {
         if (locX > 0 && locX < xEnd) 
            imgOut->set(j, i, imgIn->get(locY, locX));
         locX++;
      }

      locY++;

   }

   return imgOut;
}






void blurImage(SimpleImage<float> *imgIn, 
               std::vector<SimpleImage<float>*> &imgArr, 
               std::set<float> &resRatio, 
               int numScale,
               float minRes, 
               float maxRes) {


   float scale, deltaScale, prevScale, initScale;
   int nl, ns, nbOctave, nbNumScale, oneBlurOnly=0;
   SimpleImage<float> *scaleImg;

   if (numScale == 0) {
      zvmessage("numScale must be > 0", "");
      zabend();
   }
   if (minRes < 1) {
      zvmessage("minRes must be >= 1", "");
      zabend();
   }
   if (minRes > maxRes)
      minRes = maxRes;

   float blurFactor = 1.0 / ((float)numScale);

   nl = imgIn->getNL();
   ns = imgIn->getNS();
 
   // Get the range of resolutions
   float resRange = maxRes - minRes;

   // If the range is significantly smaller than the numScale step, then we 
   // consider that only one blurring is needed. 
   if (resRange < (0.5 * blurFactor))
      oneBlurOnly = 1;
 

   // Specific case of the first blur.
   // If the minRes is close to 1 (*close* w.r.t blur Factor), then we save the
   // image as-is without any blur applied. Otherwise we blur the image with
   // minRes factor.
   // This first blur will be the starting base for the rest of the blurs which 
   // follow a power law
   scaleImg = new SimpleImage<float>(nl, ns);
   std::copy(imgIn->linePtr(0), imgIn->linePtr(0)+(ns*nl), scaleImg->linePtr(0));
   if ((minRes-1) < (0.5 * blurFactor)) {
      imgArr.push_back(scaleImg);
      resRatio.insert(1.0);
      initScale = 1.0;
   }
   else {
      float sigma = 0.8 * sqrt(minRes*minRes-1);
      GaussianBlur(scaleImg, sigma, sigma, 0);
      imgArr.push_back(scaleImg);
      resRatio.insert(minRes);
      initScale = minRes;
   }

   // If only 1 blur is needed, we're done.
   if (oneBlurOnly)
      return;
 

   // Get the number of octave
   nbOctave = ceil(log2(resRange+1));


   // Initialization of some variables.
   prevScale = 1; //starting scale 

   // Iterate over the number of octave. 
   for (int j=0; j<nbOctave; j++) { 
    
      // Iterate over the number of scale for the current octave. 
      for (int i=0; i<numScale; i++) { 

         // First blur already taken care of
         if ( (i==0) && (j==0) )
            continue;

         // Compute current blurring scale
         scale = pow(2,j) * (1 + i * blurFactor);   //pow(2,j): current octave scale
                                                    //(1+i*blur): relative scale from current octave

         // Define the blurring increment to apply to the last blurred image 
         // to obtain an image at the current scale.
         // Specific case for the first pass: No blurring applies, save original
         // image as is.
         deltaScale = sqrt(scale*scale - prevScale*prevScale + 1.0);
 
         // Generally accepted good sigma of gauss blur to obtain a low pass
         // filtering necessary to "downsample" the image by "deltaScale"
         float sigma = 0.8 * sqrt(deltaScale*deltaScale-1);


         scaleImg = new SimpleImage<float>(nl, ns);
         std::copy(imgArr.back()->linePtr(0), imgArr.back()->linePtr(0)+(ns*nl), scaleImg->linePtr(0));
         GaussianBlur(scaleImg, sigma, sigma, 0);

         // Save the image to output container 
         imgArr.push_back(scaleImg);
         resRatio.insert(scale + initScale - 1);
         prevScale = scale;
      }     
   }
}  




float getValidBlurLimit(const int corrLargeSide, const int imgSmallSide, 
                        const float userLimit, const float resFactor) {

   // resFactor is what is wanted. Need to check if it doesn't step out of 
   // what is acceptable
   float blurLimit = resFactor;

   // Check that it doesn't go beyond what the image and correlation window size
   // would allow
   float imgLimit = (float)imgSmallSide / pow(2,ceil(log2(corrLargeSide)));
   blurLimit = (std::min)(imgLimit, blurLimit);

   //Check that it is below a blur user threshold 
   blurLimit = (std::min)(userLimit, blurLimit);

   // Check that it is > 1.0, otherwise, required blur is "high-pass", so 
   // no need to low pass 
   blurLimit = (blurLimit > 1) ? blurLimit : 1.0;

   return blurLimit;

}






// This function will compute the ranges that are within FOV of Right image and
// spaced by EPI_STEP for the 4 corners of the given tile, and will return a 
// list of ranges that combines the 4 lists. It's not the just the union of the
// 4 lists. It first union the 4 list and then subsample the list such that at
// each time, the most conservative sampling (between the 4 lists) is satisfied.
// Example: ...(high sampling)   ---(medium sampling)    - - -(low sampling)
// r1:    .......----------- - - - - - - -------
// r2:        .......----------- - - - - - - --------------
// r3:        .......--------- - - - - - -------------
// r4:          .......----------- - - ---------- .......
// 
// rAll:  .............----------- - - ---------- .......--
// 
// IMPORTANT NOTE: In linear pinhole type situation the epipolar line in the 
// Right image corresponding to a pixel in the left image is 1 continuous curve
// (continuous meaning adjacent pixel in the fov of Right). However, for certain
// cameras (non pinhole; fisheye type), the epipolar curve is not a "line" and 
// can curve significantly. This means that the epipolar curve can get out of 
// FOV and get back in at another location. In theory we could have up to 4 
// non-continuous segments for an epipolar curve (circle epipolar curve). 
// getDepthList will return these 4 segments in separate container (array of
// vectors). HOWEVER, for now, once done, all the segments are collated into
// one as we don't have any use for identified disjoint segments. Might be
// usefull in the future
#define GET_CORNER_DEPTHS(x, y)                                                \
   getDepthList(leftC, rightC, x, y, stepEpipolar, rawRanges,                  \
                currentRanges, dumpEpiX, dumpEpiY);                            \
                                                                               \
   for (int s = 0; s < 4; s++) {                                               \
      for (int r = 0; r < currentRanges[s].size(); r++) {                      \
         if (r < currentRanges[s].size()-1)                                    \
            allRanges.insert(                                                  \
               std::make_pair(currentRanges[s][r],                             \
               currentRanges[s][r+1]-currentRanges[s][r]));                    \
         else                                                                  \
            allRanges.insert(std::make_pair(currentRanges[s][r],               \
                             (std::numeric_limits<float>::max)()));            \
      }                                                                        \
   }                                                                           \
                                                                               \
   for (int s = 0; s < 4; s++) {                                               \
      currentRanges[s].clear();                                                \
      dumpEpiX[s].clear();                                                     \
      dumpEpiY[s].clear();                                                     \
   }                                                                           \



void getTileDepths(camGeometry & leftC,
                   camGeometry & rightC,
                   const int stepEpipolar,
                   tile &tile) {

   std::vector<float> & rawRanges = tile.rawRanges;
   //List of ranges corresponding to a XYZ in Right FOV. Note that as explained
   //above, all possibly disconnected epipolar curves, are collated into one
   std::vector<float> & outputRanges = tile.ranges;

   // Clear any current ranges
   outputRanges.clear();

   std::array<std::vector<float>,4> currentRanges, dumpEpiX, dumpEpiY;   

   std::set<std::pair<float, float> > allRanges;
  
   // Get ranges for top/left pixel of the tile
   GET_CORNER_DEPTHS((float)tile.xs, (float)tile.ys);

   // Get ranges for bottom/left pixel of the tile
   GET_CORNER_DEPTHS((float)tile.xs, (float)tile.ye);

   // Get ranges for top/right pixel of the tile
   GET_CORNER_DEPTHS((float)tile.xe, (float)tile.ys);

   // Get ranges for bottom/right pixel of the tile
   GET_CORNER_DEPTHS((float)tile.xe, (float)tile.ye);

//   GET_CORNER_DEPTHS((float)tile.xc, (float)tile.yc);

   // Now, run through all the ranges (ordered from small to large), and keep 
   // only the ones with the smallest range step locally, that is as we move
   // through the range, keep the ranges with the smallest step.
   int index = 0;
   float minRangeStep, range;
   for (auto r: allRanges) {

      //first element. 
      if (index == 0) {
         minRangeStep = r.second;
         range = r.first;
         outputRanges.push_back(range);
         index = 1;
         continue;
      }

      //other elements.
      if ((r.first-range) >= minRangeStep) {
         minRangeStep = r.second;
         range = r.first;
         outputRanges.push_back(range);
         continue;
      }
      if (r.second < minRangeStep) {
         minRangeStep = r.second;
         if (fabs(r.first-range) > 0.25*minRangeStep){
            range = r.first;
            outputRanges.push_back(range);
         }
      }
   }

}



void getDepthList(camGeometry &leftC,
                  camGeometry &rightC,
                  const float xLeft,
                  const float yLeft,
                  const int stepEpipolar,
                  const std::vector<float> rawRanges,
                  std::array<std::vector<float>,4>& ranges,    // output
                  std::array<std::vector<float>,4>& epiPixX,   // output
                  std::array<std::vector<float>,4>& epiPixY)   // output
{

   float step, rangeVal;

   PigCameraModel *rightCameraModel = rightC.cm;
   PigCoordSystem *cs = rightC.cs;

   // Get necessary information from Right image
   float nsr = (float)rightC.ns;
   float nlr = (float)rightC.nl;

   // Get position and orientation of the R camera
   PigVector orientationR = rightC.look;
   PigPoint  positionR = rightC.pos;


   // Get the smallest acceptable cosine angle between R orientation vector and
   // viewing vector of any other pixel of R image. This will be used to filter 
   // out quickly XYZ points that are outside field of view of R image.
   float limitCosine = rightC.minCosine;

   // Given the plane sweep approach, we can't account for pixel looking at an
   // angle > 90 degrees. Cap it
   // For fisheye-type camera.
   if (limitCosine < 0.0 )
      limitCosine = 0.0;


   // The objective is to find the epipolar curve in R image corresponding to a 
   // given pixel in the L image. This function should handle any type of camera
   // (pinhole, non-pinhole, fish-eye types for instance). For pinhole, 
   // the epipolar curve is actually a line. In case of non-pinhole (fisheye 
   // type), it is a curve instead.
   // A semi brute force approach is used: the plausible range for the given
   // left pixel are projected into the R image. Due to the difficulty of
   // finding a proper range scan resolution (step in the range direction), a 
   // two steps approach is taken:
   // [1]- Set up a sampling of the range between minRange and maxRange. The
   // sampling can be linear (user has to know what he is doing to prevent very
   // long process) or follow a power law (i.e., step = a fixed fraction of 
   // range), that is the farther the range, the larger the step. The XYZs 
   // corresponding to this range sampling are projected in R image. Then we
   // scan each segment pix1->pix2, pix2->pix3, etc and see if they intersect
   // or are included in the frame.
   // [2] For any segment that intersect or is included in frame, we linearly
   // sample the corresponding range in as many elements as the length of the
   // segment in pixel. Then we project these new XYZ in R and record the (x,y)
   // of the R pixel, while paying attention that each successive pixel is not
   // too close (<0.9 pixel) from previous pixel (useless oversampling) nor too
   // far (>1.5 pixels) (might miss corresponding point).
   

   // Get the number of raw range samples
   int nbRawRange = rawRanges.size();


   // That should not happen unless the user messed up the sampling of the 
   // range
   // UPDATE: That is not true with getting rawranges computation out of the
   // loop. can be empty, which means do nothing
   if (nbRawRange == 0) {
        return;
   }


   // Need at least 2 rawRanges candidates. That should not happen unless the 
   // user messed up the sampling of the range. Should have much more than
   // that.
   if (nbRawRange < 2) {
      zvmessage("Need at least 2 samples along the range! Bad parameters?", "");
      zabend();
   }


   // Main part of the function

   // Initiate intermediate containers and variables needed for processing and 
   // preallocate memory 
   int alreadyWarned = 0;
   double x, y, xprev, yprev;
   PigPoint centerL, XYZ;
   PigVector look, rightToXYZ;
   std::vector<float> xarr, yarr, rangeTmp;


   xarr.reserve(nbRawRange);
   yarr.reserve(nbRawRange);
   rangeTmp.reserve(nbRawRange);

 
   // Get the look vector of the L image current pixel   
   leftC.cm->LStoLookVector(yLeft, xLeft, centerL, look, leftC.cs);

   // Project each raw range samples on the R image and store the (x,y)
   for (int k=0; k < nbRawRange; k++) {

      // Get XYZ at current range
      XYZ = centerL + look * rawRanges[k];

      // By construction, XYZ is in front of L camera, but it might be in
      // back or out of FOV of R camera. Check and skip that range if that
      // is the case.
      rightToXYZ = XYZ-positionR;


      // Check XYZ closeness to Right camera center
      if (rightC.closenessLimit > 0.0) {
         if (rightToXYZ.magnitude() < rightC.closenessLimit)
            continue;
      }

      rightToXYZ.normalize();

      // Check for within FOV of Right camera
      if ((orientationR % rightToXYZ) < limitCosine) {
         if (alreadyWarned == 0) {
            // zvmessage("Some ranges were discarded because out of R camera FOV","");
            // Set up a flag to avoid printing warning message for each
            // range outside FOV
            alreadyWarned++;  // Not thread-safe but doesn't matter
         }
         continue;
      }


      // Back-project XYZ in R image
      rightCameraModel->XYZtoLS(XYZ, FALSE, &y, &x, cs);


      // If current location in R image and previous location in R image
      // (for previous range) is less than half the step required in the 
      // epipolar direction, no need to store. We're looking for about 
      // stepEpipolar pixels difference between successive range. Skip.
      if (k != 0 && sqrt((x-xprev)*(x-xprev) + (y-yprev)*(y-yprev)) < 0.5*stepEpipolar)
         continue;

      // Store corresponding x,y and range
      xarr.push_back((float)x);
      yarr.push_back((float)y);
      rangeTmp.push_back(rawRanges[k]);

      xprev = x;
      yprev = y;
   }


   // In case all raw ranges were out of R FOV, exit
   // TODO: This is not entirely correct. Depending on the range sampling, we 
   // could miss the entire frame between 2 successive ranges if they are too
   // wide apart. 
   if (xarr.size() == 0)
      return;

   // Specific case of 1 range. Most likely the situation when both Left and
   // Right cameras have same C (optical center). No epipolar line in this case.
   if (xarr.size() == 1) {
      ranges[0].push_back(rangeTmp[0]);
      epiPixX[0].push_back(xarr[0]);   
      epiPixY[0].push_back(yarr[0]);   
      return;
   }




   // Iterate over the segments formed by consecutive (x,y) in R image.
   // If a segment intersect the frame, we look at how far appart the
   // endpoints are in the R image. Depending on that distance, we either
   // subdivide the segment (too many pixel apart) or cumulate that 
   // segment with next ones until the overall distance is close to one
   // stepEpipolar pixels.

   // Get the number of segments that are making the epipolar curve.
   // Unless there were some ranges that caused the XYZ to be "behind" 
   // the R camera, the number of segments is equals to the number of
   // raw ranges (-1).
   int nbSegments = xarr.size() - 1;

   // Initiate the cumulated length (in pixels in the R image) between
   // successive segments. If some segments are very short, less than
   // ~stepEpipolar pix, we want to *merge* them until the cumulated distance 
   // reaches about that distance.
   float distTot = 0;

   // Number of segments that have been merged.
   int numDist = 0;
       
  


   int outside, epiPartIndex, correctStepFound;
   float x0, y0, x1, y1;

   // Look at the start of the epipolar curve and see if it is inside or
   // outside the R frame. Initiate some variable accordingly, so we can
   // track when the epipolar curve is *entering* or *exiting* the frame
   if (xarr[0] >=0 && xarr[0] < nsr && yarr[0] >=0 && yarr[0] < nlr) {
      outside = 0;       // Flag saying if inside or outside frame
      epiPartIndex = 1;  // Number of epipolar curve parts. Given we're inside
                         // R frame, we have 1 at least
   }                     
   else {
      outside = 1;
      epiPartIndex = 0;
   }

 
   // Iterate over all the segments of the raw epipolar curve and:
   // - check if current segment intersects R frame
   // - If not, skip it
   for (int k = 0; k < nbSegments; k++) {
 
      // Does current segment intersect R frame?
      //
      // The current segment does not intersect the frame, no need to search for
      // a corresponding points, move on to next segment.
      // In case of non-linear camera, the actual epipolar curve between the 
      // endpoints of the segment is not a line, so although the two endpoints 
      // form a segment non intersecting the frame, the actual epipolar curve 
      // could "bend" and reach in the frame. That situation is a miss. 
      // Something like that (highly exagerated):
      //
      //        * endpoint 1
      //        |\
      //        | \ <--- actual epipolar curve
      //        |  \__________
      //        | | \         |
      //        | |  \        |
      // segment| |   |       | frame 
      //        | |   |       |    
      //        | |_ /________| 
      //        |   / 
      //        |  /  
      //        | /
      //        |/
      //        * endpoint 2
      //
      // However, in practice, if the raw sampling of the range is 
      // reasonable, such "miss" should not happen, and even if it does, 
      // it'll only be in the frame edge vicinity which is most likely not
      // very useful anyway because of strong non-linear deformation from 
      // camera.
      // Note that we're setting the boundaries at 0.5 pix within the frame.
      // The reason is to avoid situation where the segment is // to the frame
      // edge and about "on" the edge. For instance with linearized images.
      // Numerical precision will cause the segments to be randomly in or out 
      // the frame which will crash the program. This is a temporary fix that
      // solve the linearized image, but should be better handled as an 
      // unfortunate case where the segment is half a pixel within frame border
      // and // to frame edge will still cause issue. Much less likely to
      // happen though
      int inR = segmentFrameIntersect(0.5f, 0.5f, nsr-1.5f, nlr-1.5f, 
                                      xarr[k], yarr[k], xarr[k+1], yarr[k+1],
                                      x0, y0, x1, y1);


      // If current segment does not intersect R frame, make sure flag is
      // set to *outside* and reinitialize the cumulated distance of 
      // segments to 0, as once the curve will enter (or re-enter) the 
      // frame, it will be the start of a new epi curve part and we'll 
      // start to cumulate segment distance.
      if (inR == 0) {
         outside = 1;
         distTot = 0;
         numDist = 0;
         continue;                                        
      }

      // At this point, the current segment intersects the R frame. If 
      // the curve was outside, then we're entering the frame and starting
      // a new epipolar curve part
      if (outside) {
         epiPartIndex++;   // update number of epi curve part
         outside = 0;      // Not outside anymore
      }

      // Save current pixel location of segment origin if starting a new 
      // epi curve part. 
      if (distTot == 0) {
         epiPixX[epiPartIndex-1].push_back(xarr[k]);
         epiPixY[epiPartIndex-1].push_back(yarr[k]);
         ranges[epiPartIndex-1].push_back(rangeTmp[k]);
      }

      // Compute the  distance, in pixels, between the two endpoints of 
      // the segment. 
      float nbPixSegment = sqrt((xarr[k]-xarr[k+1])*(xarr[k]-xarr[k+1]) + 
                              (yarr[k]-yarr[k+1])*(yarr[k]-yarr[k+1]));


      // Update the total distance of current segment and update number
      // of segments cumulated.
      distTot += nbPixSegment;
      numDist ++;
   
      // If length of current (or cumulated) segment is less than the limit, 
      // move to next segment.
      // If we're at the end of the epipolar raw curve, save the end point as we
      // need to finish the curve part.
      if (distTot < 1.5*stepEpipolar) {
         if (k == (nbSegments-1)) {
            epiPixX[epiPartIndex-1].push_back(xarr[k+1]);
            epiPixY[epiPartIndex-1].push_back(yarr[k+1]);
            ranges[epiPartIndex-1].push_back(rangeTmp[k+1]);
         }
         continue;
      }
      // If the current or cumulated segment length is above limit, there
      // are two cases. Either that single segment is larger than limit
      // in which case we need to resample it with closer ranges OR it has
      // been cumulated and went over the limit in which case we stop the
      // cumul and save the segment points. And start a new cumul.
      else if (numDist > 1) {
         epiPixX[epiPartIndex-1].push_back(xarr[k]);
         epiPixY[epiPartIndex-1].push_back(yarr[k]);
         ranges[epiPartIndex-1].push_back(rangeTmp[k]);
         distTot = nbPixSegment;
         numDist=1;
         if (distTot < 1.5*stepEpipolar)  //TODO ????
            continue;
      }
             

      // At this point, that means that we need to sample with more 
      // resolution the raw range segment in order to have a segment 
      // length about stepEpipolar pixels.
      // Initialize the step as if we're doing a linear sampling of the
      // range, such that each successive range should be projected
      // about stepEpipolar pixels away in R from the previous one.

      step = (rangeTmp[k+1] - rangeTmp[k]) / (nbPixSegment/stepEpipolar);
      float range = rangeTmp[k]+step;            
      xprev = xarr[k];
      yprev = yarr[k];

      while (range < rangeTmp[k+1]) {

         // Get XYZ at given range for the current pixel
         XYZ = centerL + look * range;

         // Project XYZ in R image
         rightCameraModel->XYZtoLS(XYZ, FALSE, &y, &x, cs);

         //Addition to deal with non linear camera. If XYZ too close to 
         //optical center and/or with strong off-axis angle, XYZtoLS
         //showed instabilities/divergence. To alleviate the issue, 
         //discard range that causes XYZ to be too close to optical center.
         //For now, it is commanded by a keyword, but eventually it should 
         //be a camera model parameter
         if (rightC.closenessLimit != 0) {
            if ((XYZ-positionR).magnitude() < rightC.closenessLimit) {
               while (range < rangeTmp[k+1]) {
                  range += step;
                  XYZ = centerL + look * range;
                  float dist = (XYZ-positionR).magnitude();
                  if (dist > rightC.closenessLimit) {
                     rightCameraModel->XYZtoLS(XYZ, FALSE, &y, &x, cs);
                     epiPixX[epiPartIndex-1].push_back((float)x);
                     epiPixY[epiPartIndex-1].push_back((float)y); 
                     ranges[epiPartIndex-1].push_back(range);
                     range += step;
                     xprev = x;
                     yprev = y;
                     break;
                  }
               }
               if (range >= rangeTmp[k+1])
                  break;
               else {
                  XYZ = centerL + look * range;
                  rightCameraModel->XYZtoLS(XYZ, FALSE, &y, &x, cs);
               }
            } 
         }


         // Check how far in the R image is this point compared to the 
         // previous one (along the range). If too close (distance<0.9*
         // stepEpipolar pixels) then we're moving too slowly along the range, 
         // if too far (distance>1.5*stepEpipolar pixels) we're moving too fast 
         // and might miss too many pixels along the epipolar curve.
         float dist = sqrt((x-xprev)*(x-xprev) + (y-yprev)*(y-yprev));

         if (dist <= 0.9*stepEpipolar) {
            correctStepFound = 0;
            range -= step;  // backtrack before reajusting the step
             
            while (!correctStepFound) {
               step *=1.2;
               float range2 = range + step;
               if (range2 > rangeTmp[k+1])
                  break;
               XYZ = centerL + look * range2;
               rightCameraModel->XYZtoLS(XYZ, FALSE, &y, &x, cs);
               dist = sqrt((x-xprev)*(x-xprev) + (y-yprev)*(y-yprev));
               if (dist>0.9*stepEpipolar) {        // found step large enough
                  if (dist >= 1.5*stepEpipolar)    // make sure we don't overshoot
                     step /= 1.1;
                  correctStepFound = 1;
               }
            }

            // If a proper step wasn't found, that is a sign of something 
            // peculiar happening. The situation is "whatever the range, the 
            // projected pixel stays close to previous one". Most likely we are
            // very close to the epipole, or L and R rays are now near parallel.
            // In that case, the epipolar line stops here. Get out of that 
            // segment range refinement.
            if (correctStepFound) {
               range += step;
               XYZ = centerL + look * range;
               rightCameraModel->XYZtoLS(XYZ, FALSE, &y, &x, cs);
            }
            else
               break;
                
         }
         else if (dist > 1.5) {
            correctStepFound = 0;
            range -= step;  // backtrack before reajusting the step
             
            while (!correctStepFound) {
               step /=2;
               float range2 = range + step;
               XYZ = centerL + look * range2;
               rightCameraModel->XYZtoLS(XYZ, FALSE, &y, &x, cs);
               dist = sqrt((x-xprev)*(x-xprev) + (y-yprev)*(y-yprev));

               if (dist < 1.5*stepEpipolar) {
                  if (dist <= 0.9*stepEpipolar)
                      step *= 1.5;
                  correctStepFound = 1;
               }
            }

            // If a proper step wasn't found that is REALLY weird. 
            // Not sure what to do in that case. For now, quit that weird
            // range refinement segment.
            if (correctStepFound) {
               range += step;
               XYZ = centerL + look * range;
               rightCameraModel->XYZtoLS(XYZ, FALSE, &y, &x, cs);
            }
            else
              break;

         }
         else {}


         epiPixX[epiPartIndex-1].push_back((float)x);                 
         epiPixY[epiPartIndex-1].push_back((float)y);                  
         ranges[epiPartIndex-1].push_back(range);

         range += step;
         xprev = x;
         yprev = y;


      } // end while loop on range to fill in rawRanges values.

      // Now that we're done oversampling the current segment, we make
      // sure to start a new cumul.
      distTot = 0;
      numDist = 0;

   }

}




// Retrieve the min/max range and average normal for the given tile
void getRangeAndNormalFromMesh(MeshMan * mesh, PigCameraModel * cam, tile & t,
                               int multiplane) {

   int found, count;
   float range, NXsum, NYsum, NZsum, minRange, maxRange;
   PigVector look, normal;
   PigPoint origin;
   
   minRange = (std::numeric_limits<float>::max)();
   maxRange = (std::numeric_limits<float>::min)();


   // Just in case clear container
   t.rawRanges.clear();
   t.planeNormArr.clear();
   std::vector<PigVector> varr;

   count = 0;
   range = NXsum = NYsum = NZsum = 0.0;
   for (int j=t.ys; j<=t.ye; j++) {
      for (int i=t.xs; i<=t.xe; i++) {
         cam->LStoLookVector(j, i, origin, look, mesh->getCS()); 
         found = mesh->intersect(origin, look, nullptr, &normal, &range);
         if (found) {
            if (range > maxRange)
               maxRange = range;
            if (range < minRange)
               minRange = range;

            PigVector v(normal.getX(), normal.getY(), normal.getZ());
            v.normalize();
            varr.push_back(v);
         }
      }
   }


   if (count) {
      t.rawRanges.push_back(minRange);
      t.rawRanges.push_back(maxRange);

      // The plane orientations list will depends on the user choice of
      // using a multi-plane approach
      if (multiplane) {
         //Need to aggregate all plane normal orientations into a sublist of
         //orientations to have a limited number of orientations.  
         //All vectors within 10 degrees of a given vector are removed
         //TODO: Note this is naive approach, not optimized
         //TODO: Need some parameters
         float cosineLimit = cos(10.0 * PI / 180.0);
         int ind = 0;
         while (ind != varr.size()) {
            t.planeNormArr.push_back(varr[ind]);
            for (int i=varr.size()-1; i>ind; i--) {// easier indexing when reverse
               if (varr[ind] % varr[i] > cosineLimit) 
                  varr.erase(varr.begin() + i); 
            }
            ind++;
         }
      }
      else { // Compute the average orientation to return only one plane normal 
         PigVector avgVect(0,0,0);
         for (auto &v: varr) 
            avgVect += v;
         avgVect.normalize();
         t.planeNormArr.push_back(avgVect);
      }
   }
}



void getNormalFromFile(SimpleImage<float> * normalImg, tile & t, int multiplane) {


   // Clear the normals container just in case
   t.planeNormArr.clear();
   std::vector<PigVector> varr;

   // Read and save from the normal for each pixel of the tile
   for (int j=t.ys; j<=t.ye; j++) {
      for (int i=t.xs; i<=t.xe; i++) {
         PigVector v = PigVector(normalImg->get(0,j,i),
                                 normalImg->get(1,j,i),
                                 normalImg->get(2,j,i));
         if (!(v == PigVector(0,0,0))) {
            v.normalize();
            varr.push_back(v);
         } 
      }
   }

   // The plane orientations list will depends on the user choice of
   // using a multi-plane approach
   if (multiplane) {
      //Need to aggregate all plane normal orientations into a sublist of
      //orientations to have a limited number of orientations. 
      //All vectors within 10 degrees of a given vector are removed
      //TODO: Note this is naive approach, not optimized
      //TODO: Need some parameters
      int ind = 0;
      float cosineLimit = cos(10.0 * PI / 180.0);
      while (ind != varr.size()) {
         t.planeNormArr.push_back(varr[ind]);
         for (int i=varr.size()-1; i > ind ;i--) {// easier indexing when reverse
            if (varr[ind] % varr[i] > cosineLimit) 
               varr.erase(varr.begin() + i); 
         }
         ind++;
      }
   }
   else { // Compute the average orientation to return only one plane normal 
      PigVector avgVect(0,0,0);
      for (auto &v: varr) avgVect += v;
      avgVect.normalize();
      t.planeNormArr.push_back(avgVect);
   }

}




void getRangeFromFile(SimpleImage<float> * rangeImg, tile & t) {

   double range;

   float minRange = (std::numeric_limits<float>::max)();
   float maxRange = (std::numeric_limits<float>::min)();

   for (int j=t.ys; j<=t.ye; j++) {
      for (int i=t.xs; i<=t.xe; i++) {
         // Get range of current pixel and check value is valid
         range = rangeImg->get(j,i);
         if (range == 0)
            continue;

         // update min max if necessary
         if (range > maxRange)
            maxRange = range;
         if (range < minRange)
            minRange = range; 
      }
   }

   // Just in case
   t.rawRanges.clear();

   if (minRange != (std::numeric_limits<float>::max)()) { 
      t.rawRanges.push_back(minRange); 
      t.rawRanges.push_back(maxRange); 
   }
}




// Given a min/max bracket range and user sampling parameters,
// compute a list of initial ranges.

std::vector<float> getRangeFromDefault( const int samplingMode,   // linear or powerlaw
                                        const double samplingStep,   // step for linear or powelaw sampling
                                        const float minRange,
                                        const float maxRange) {

   float step;
   std::vector<float> ranges;

   // Pre-compute the raw sampling of the range
   float rangeVal = minRange;
   ranges.push_back(rangeVal);
   // Linear case
   if (samplingMode) {
      if (samplingStep < 0)
         step = (maxRange - minRange) / abs(samplingStep);
      else
         step = samplingStep;

      while (rangeVal < maxRange) {
         rangeVal += step;
         ranges.push_back(rangeVal);
      }
   }
   // Power law case
   else {
      while (rangeVal < maxRange) {
         step = pow(10.0, floor(log10(rangeVal)) - samplingStep);         
         rangeVal += step;
         ranges.push_back(rangeVal);
      }
   } 

   return ranges;
}


// Given a min/max bracket range and user sampling parameters,
// compute a list of initial ranges.
std::vector<float> getRangeFromDefault( const planeSpace & planeInfo) {

   return getRangeFromDefault(planeInfo.rangeSampling, planeInfo.rangeStep, 
                              planeInfo.minRange, planeInfo.maxRange);
}







template<class T>
void computeIntegralImage(SimpleImage<T> *imgIn,
                          SimpleImage<double> *& imgOut) {
  

  int nbX = imgIn->getNS();
  int nbY = imgIn->getNL();

  T * pIn  = imgIn->linePtr(0);
  double * pOut = imgOut->linePtr(0);

  // First pixel
  *pOut = *pIn;

  // First row
  for (int x = 1; x < nbX; x++)
     *pOut = *pOut++ + *++pIn;

  // Next rows
  double *pOut2 = (pOut - nbX);
  for (int y = 1; y < nbY; y++) {
   
      // First pixel of the current row
      double s = *++pIn;  // current row cumulative sum
      *++pOut = *++pOut2 + s;
      
      // Next pixels
      for (int x = 1; x < nbX; x++) {
         s = s + *++pIn;
         *++pOut = *++pOut2 + s;
      }
   }
}
   





inline double sumIntegralImage(SimpleImage<double>* img, 
                               int sX, int sY, int nbX, int nbY) {

   double sum = img->get(sY+nbY-1, sX+nbX-1);
   
   if (sX > 0) sum -= img->get(sY+nbY-1, sX-1);
   if (sY > 0) sum -= img->get(sY-1, sX+nbX-1);
   if (sX > 0 && sY > 0) sum += img->get(sY-1, sX-1);

   return sum;

}
                                             



// This version of the NCC uses integral images table to speed up the 
// processing time
void integralImageCorrelation( SimpleImage<float> * tL,
                               SimpleImage<float> * tR,
                               int corrX, int corrY,
                               int searchX, int searchY,
                               int searchStepX, int searchStepY,
                               SimpleImage<float> *& scores,
                               SimpleImage<int> *& locX, 
                               SimpleImage<int> *& locY) {
   
   int nsl, nll, nsr, nlr, szCorr, totCorrX, totCorrY;
   float *pInL, *pInR, invSzCorr; 
   double *pOut;

   nsl = tL->getNS();
   nll = tL->getNL();
   nsr = tR->getNS();
   nlr = tR->getNL();

   szCorr = corrX * corrY;
   totCorrX = nsl - corrX + 1;
   totCorrY = nll - corrY + 1;
   invSzCorr = 1.0 / (float) szCorr;

   // [1] Compute a serie of integral images necessary for the correlation
   // We need the integral image of:
   // - Left image
   // - (Left image)^2  (square of the Left image)
   // - Right image
   // - (Right image)^2
   // - Left*Right (for each of the search step in X and Y, i.e., nbSearch
   //   integral image). This is were processing time can grow large 
   //   depending on the search size


   // Integral Image for tile L
   SimpleImage<double>* tL_I = new SimpleImage<double>(nll, nsl);
   computeIntegralImage(tL, tL_I);
        
   // If image is empty (zeros, nothing to correlate, return)
   if (tL_I->get(nll-1, nsl-1) == 0) {
      tL_I->free();
      return;
   }

   // Integral Image for tile L^2
   // First generate a tileL^2
   SimpleImage<double>* tL2 = new SimpleImage<double>(nll, nsl);
   SimpleImage<double>* tL2_I = new SimpleImage<double>(nll, nsl);
   pInL = tL->linePtr(0);
   pOut = tL2->linePtr(0);
   for (int it=0; it<(nsl*nll); it++)
      pOut[it] = pInL[it]*pInL[it];
   // Second compute its integral image
   computeIntegralImage(tL2, tL2_I);


   // Pre-compute the avg/std of Left for each correlation location
   // Interleave avg and std for cache optimization
   float avgL, varL;
   SimpleImage<float> * preL = new SimpleImage<float>(totCorrY, 2*totCorrX);
   for (int l=0; l<totCorrY; l++) {
      for (int c=0; c<totCorrX; c++) {
         avgL = (float)sumIntegralImage(tL_I, c, l, corrX, corrY);
         avgL *= invSzCorr;
         varL = (float)(sumIntegralImage(tL2_I, c, l, corrX, corrY)  
                - (avgL*szCorr)*avgL)*invSzCorr;
         preL->set(l, c*2, avgL);
         preL->set(l, c*2+1, varL);
      }
   }

   // Housekeeping
   tL_I->free();
   tL2->free();
   tL2_I->free();

   // Integral Image for tile R
   SimpleImage<double>* tR_I = new SimpleImage<double>(nlr, nsr);
   computeIntegralImage(tR, tR_I);

   // If image is empty (zeros, nothing to correlate, return)
   if (tR_I->get(nll-1, nsl-1) == 0) {
      tR_I->free();
      return;
   }

   // Integral Image for tile R^2
   // First generate a tileR^2
   SimpleImage<double>* tR2 = new SimpleImage<double>(nlr, nsr);
   SimpleImage<double>* tR2_I = new SimpleImage<double>(nlr, nsr);
   pInR = tR->linePtr(0);
   pOut = tR2->linePtr(0);
   for (int it=0; it<(nlr*nsr); it++)
      pOut[it] = pInR[it]*pInR[it];
   // Second compute its integral image
   computeIntegralImage(tR2, tR2_I);


   // Pre-compute the avg/std of Right for each correlation location
   // Interleave avg and std for cache optimization
   float avgR, varR;
   SimpleImage<float> * preR = new SimpleImage<float>(totCorrY+2*searchY, 
                                                      2*(totCorrX+2*searchX));
   for (int l=0; l<(totCorrY+2*searchY); l++) {
      for (int c=0; c<(totCorrX+2*searchX); c++) {
         avgR = (float)sumIntegralImage(tR_I, c, l, corrX, corrY);
         avgR *= invSzCorr;
         varR = (float)(sumIntegralImage(tR2_I, c, l, corrX, corrY)  
                - (avgR*szCorr)*avgR)*invSzCorr;
         preR->set(l, c*2, avgR);
         preR->set(l, c*2+1, varR);
      }
   }

   // Housekeeping
   tR_I->free();
   tR2->free();
   tR2_I->free();



   int szSearchX = 2*searchX+1;
   int szSearchY = 2*searchY+1;
   pInL = tL->linePtr(0);

   SimpleImage<double> * tLR = new SimpleImage<double>(nll,nsl);
   pOut = tLR->linePtr(0);

   for (int sl=0; sl<szSearchY; sl+=searchStepY) {

      for (int sc=0; sc<szSearchX; sc+=searchStepX) {

         // Integral image of L*R for all search step
         // It is assumed that L input image is sized properly in accordance to 
         // the correlation window size and location for which we want 
         // correlation. That means that the size of L input image must account 
         // exactly for the correlation window size
         // Note this step is time consuming. To gain some time, the integral 
         // image computation is done on the fly with the LxR

         float * pInL = tL->linePtr(0);
         float * pInR = (tR->linePtr(sl)+sc);
         double * pOut = tLR->linePtr(0);
         int offset = nsr - nsl;

         // First pixel
         *pOut = *pInL * *pInR;

         // First row
         for (int x = 1; x < nsl; x++)
            *pOut = *pOut++ + *++pInL * *++pInR;

         // Next rows
         double *pOut2 = (pOut - nsl); 
         for (int y = 1; y < nll; y++) {
   
            pInR += offset;

            // First pixel of the current row
            double s = *++pInL * *++pInR;
            *++pOut = *++pOut2 + s;
            
            // Next pixels
            for (int x = 1; x < nsl; x++) {
               s = s + *++pInL * *++pInR;
               *++pOut = *++pOut2 + s;
            }
         }



         // This part is the actual correlation.
         // Optimized for speed, so a bit less readable than standard
         // pearson correlation implementation
         int scOffset = sc - searchX;
         int slOffset = sl - searchY;
         int off = corrX-1; // difference btw img size and num of corr in X 
         // Pointers to the 4 corners of the correlation template .
         double * tl = (tLR->linePtr(-1) - 1);
         double * tr = (tLR->linePtr(-1) + corrX - 1);
         double * bl = (tLR->linePtr(corrY-1) - 1);
         double * br = (tLR->linePtr(corrY-1) + corrX - 1);

         for (int l=0; l<totCorrY; l++) {

            float * scorePtr = scores->linePtr(l);            
            int * locXPtr = locX->linePtr(l);            
            int * locYPtr = locY->linePtr(l);            

            float *preLPtr = preL->linePtr(l);
            float *preRPtr = (preR->linePtr(sl+l) + sc*2);

            for (int c=0; c<totCorrX; c++) {

               // average and variance are pixel-interleaved
               float avg = *preLPtr++; //avg Left
               float var = *preLPtr++; //var Left

               avg = avg * (*preRPtr++); //avgLeft * avgRight
               var = var * (*preRPtr++); //varLeft * varRight

               if (var < EPSILON) {
                  br++; bl++; tl++; tr++;
                  continue;
               }

               // Inlining of the sumIntegralImage
               double prod = *br;
               if (c > 0) prod -= *bl;
               if (l > 0) prod -= *tr;
               if (c > 0 && l > 0) prod += *tl;
               prod = prod * invSzCorr;
               br++; bl++; tl++; tr++;

               float numerator = prod - avg;
               // To save processing time, we defer the sqrt to later. 
               // Should be: score = (prod - avgL*avgR) / sqrt(varL*varR);
               float score = numerator * numerator / var;
               // score > 1 can happen when variance too small
               if (score > (scorePtr[c]) && score < 1.01) {
                  scorePtr[c] = score;
                  locXPtr[c] = c + scOffset;
                  locYPtr[c] = l + slOffset;
               }
               
            }

            tl += off;
            tr += off;
            bl += off;
            br += off;

         }

      } // end loop on sc
   } // end loop on sl

   tLR->free();
   delete tLR;

   preL->free();
   preR->free();
   delete preL;
   delete preR;

}








////////////////////////////////////////////////////////////////////////
// Read an image into memory.  Returns the unit number (closed).
////////////////////////////////////////////////////////////////////////

static void read_image(char *param, SimpleImage<float> *&image)
{
    char filename[PIG_MAX_FILENAME_SIZE+1];
    char msg[150];
    int unit;

    zvpone(param, filename, 1, PIG_MAX_FILENAME_SIZE);
    zvunit(&unit, param, 1, "u_name", filename, NULL);
    zvopen(unit, "OP", "READ", "U_FORMAT", "REAL", "OPEN_ACT", "AS", NULL);

    /* get input image dimensions */

    int nl, ns, nb;

    zvget(unit, "NL", &nl, "NS", &ns, "NB", &nb, NULL);
    if (ns > MAX_NS) {
	zvmessage("Input images exceed buffer limit sizes", "");
	zabend();
    }

    /* read input(s) into memory */

    if (image != NULL)
	delete image;

    if (nb == 1) {
       image = new SimpleImage<float>(nl, ns);
       for (int j=0; j < nl; j++)
	   zvread(unit, image->linePtr(j), "BAND", 1, "LINE", j+1, NULL);
    }
    else {
       image = new SimpleImage<float>(nb, nl, ns);
       for (int k=0; k < nb; k++)
          for (int j=0; j < nl; j++)
  	      zvread(unit, image->linePtr(k,j), "BAND", k+1, "LINE", j+1, NULL);
    }

    zvclose(unit, NULL);
}









void drawEpipolarPoints(SimpleImage<float> *&imgL,
                        SimpleImage<float> *&imgR,
                        camGeometry &leftC,
                        camGeometry &rightC,
                        const planeSpace &planeInfo, 
                        const int epiStep, 
                        const std::vector<int> &Xarr,
                        const std::vector<int> &Yarr,
                        const std::string &fileName) {

   float avgL, stdL, avgR, stdR, minL, minR, maxL, maxR, spreadL, spreadR, val;


   // Get images size
   int nsl = imgL->getNS();
   int nll = imgL->getNL();
   int nsr = imgR->getNS();
   int nlr = imgR->getNL();

   // Compute average and standard deviation of Left and Right image for 
   // normalization to get good contrast between them and epipolar curve
   avgL = avgR = stdL = stdR = 0;
   minL = (std::numeric_limits<float>::max)();
   minR = (std::numeric_limits<float>::max)();
   maxL = (std::numeric_limits<float>::min)();
   maxR = (std::numeric_limits<float>::min)();


   // Left Average
   float *pixPtr = imgL->linePtr(0);
   for (int i=0; i<nsl*nll; i++){
      val = *pixPtr;
      if (val < minL) minL = val;
      if (val > maxL) maxL = val;
      avgL += val;
      pixPtr++;
   }
   avgL /= (nsl*nll);
   spreadL = maxL - minL;

   // Left Standard deviation
   pixPtr = imgL->linePtr(0);
   for (int i=0; i<nsl*nll; i++) {
      stdL += pow(*pixPtr - avgL, 2);
      pixPtr++;
   }
   stdL = sqrt(stdL/(nsl*nll));



   // Right Average
   pixPtr = imgR->linePtr(0);
   for (int i=0; i<nsr*nlr; i++){
      val = *pixPtr;
      if (val < minR) minR = val;
      if (val > maxR) maxR = val;
      avgR += val;
      pixPtr++;
   }
   avgR /= (nsr*nlr);
   spreadR = maxR - minR;

   // Right Standard deviation
   pixPtr = imgR->linePtr(0);
   for (int i=0; i<nsr*nlr; i++){
      stdR += pow(*pixPtr - avgR, 2);
      pixPtr++;
   }
   stdR = sqrt(stdR/(nsr*nlr));



   // Create an image container that will contain the left and right images
   // with the epipolar points overlaid on top of them
   int ns, nl;
   ns = nsl + 3 + nsr;  // L and R side by side  + 3 pixels margin
   if (nll > nlr)       // number of lines = max between L and R 
      nl = nll;
   else 
      nl = nlr;

   SimpleImage<float> * im = new SimpleImage<float>(3, nl, ns);

   // Copy the L image
   for (int j=0; j<nll; j++) { 
      for (int i=0; i<nsl; i++) {
         val = imgL->get(j,i);
         val = (val - minL) / spreadL;
         im->set(0,j,i, val);
         im->set(1,j,i, val);
         im->set(2,j,i, val);
      }
   }
   imgL->free();


   // Copy the R image
   for (int j=0; j<nlr; j++) { 
      for (int i=0; i<nsr; i++) {
         val = imgR->get(j,i);
         val = (val - minR) / spreadR;
         im->set(0,j,i+nsl+3, val);
         im->set(1,j,i+nsl+3, val);
         im->set(2,j,i+nsl+3, val);
      }
   }
   imgR->free();


   int rangeDN = (int)(6*stdL);
   int offsetDN = (int)(avgL-3*stdL);


   std::vector<float> rawRanges = getRangeFromDefault(planeInfo);


   float c1, c2, c3, x, y;
   std::array<std::vector<float>,4> ranges, epiPixX, epiPixY;

   // Get all drawn points coordinates
   for(int i=0; i<Xarr.size(); i++) {

      for (auto & k:epiPixX) k.clear();
      for (auto & k:epiPixY) k.clear();
      for (auto & k:ranges) k.clear();

      x = (float)Xarr[i];
      y = (float)Yarr[i];


      getDepthList(leftC, rightC, x, y, epiStep, rawRanges, 
                   ranges, epiPixX, epiPixY); 

      //c1 = rand()%rangeDN - offsetDN;
      //c2 = rand()%rangeDN - offsetDN;
      //c3 = rand()%rangeDN - offsetDN;
      c1 = (float)(rand()%1000)/1000.0;
      c2 = (float)(rand()%1000)/1000.0;
      c3 = (float)(rand()%1000)/1000.0;

      // Draw current point on the Left image, as a cross
      // Set a function for that?
      if (y > 2) {
         im->set(0, y-2, x, c1);
         im->set(1, y-2, x, c2);
         im->set(2, y-2, x, c3);
      }
      if (y > 1) {
         im->set(0, y-1, x, c1);
         im->set(1, y-1, x, c2);
         im->set(2, y-1, x, c3);
      }
      if (x > 2) {
         im->set(0, y, x-2, c1);
         im->set(1, y, x-2, c2);
         im->set(2, y, x-2, c3);
      }
      if (x > 1) {
         im->set(0, y, x-1, c1);
         im->set(1, y, x-1, c2);
         im->set(2, y, x-1, c3);
      }
      im->set(0, y, x, c1);
      im->set(1, y, x, c2);
      im->set(2, y, x, c3);
      if (x < nsl-1) {
         im->set(0, y, x+1, c1);
         im->set(1, y, x+1, c2);
         im->set(2, y, x+1, c3);
      }
      if (x < nsl-2) {
         im->set(0, y, x+2, c1);
         im->set(1, y, x+2, c2);
         im->set(2, y, x+2, c3);
      }
      if (y < nll-1) {
         im->set(0, y+1, x, c1);
         im->set(1, y+1, x, c2);
         im->set(2, y+1, x, c3);
      }
      if (y < nll-2) {
         im->set(0, y+2, x, c1);
         im->set(1, y+2, x, c2);
         im->set(2, y+2, x, c3);
      }
     
      

      // Draw corresponding epipolar points on the R image
      for (int u=0; u<4; u++) { 
         for (int v=0; v<epiPixX[u].size(); v++) { 
            x = floor(epiPixX[u][v]+0.5);
            y = floor(epiPixY[u][v]+0.5);
            if (x>0 && x < (nsr-1) && y>0 && y<(nlr-1)) {
               im->set(0, y, x+nsl+3, c1);
               im->set(1, y, x+nsl+3, c2);
               im->set(2, y, x+nsl+3, c3);
            }
         }
      }
   }

   // Save image to file
   saveImg(fileName, im);

   // Free memory
   im->free();

}




template<class T1, class T2>
float getRadius(T1 x, T1 y, T2 nbX, T2 nbY) {

   return std::sqrt(std::pow((double)x - ((double)nbX - 1.0) / 2.0 ,2)  +
                    std::pow((double)y - ((double)nbY - 1.0) / 2.0 ,2));

}





void getAndCheckTileSize(int tileSize[2], int nll, int nsl, int corrSz[2]) {

   char msg[150];

   // Get tile size 
   // Default tile size is 3 times template size
   int count = 0;
   zvp("TILE_SIZE", tileSize, &count);  
   if (count == 0) {
      tileSize[0] = 3 * corrSz[0]; 
      tileSize[1] = 3 * corrSz[1]; 
   }
   else if (count == 1) {
      if (tileSize[0] < 0) {// No tiling, full image process as one tile
         tileSize[0] = nll; 
         tileSize[1] = nsl;
      }
      else {
         int tmp = tileSize[0];
         if (tmp < corrSz[0]) {
            sprintf(msg, "Tile size cannot be smaller than correlation window size. Adjusting tile size...");
            zvmessage(msg, "");
            tileSize[0] = corrSz[0];
         }

         if (tmp < corrSz[1])
            tileSize[1] = corrSz[1];
         else
            tileSize[1] = tileSize[0];
      }
   }    
   else {
      if (tileSize[0] < 0) 
         tileSize[0] = nsl;
      else {
         if (tileSize[0] < corrSz[0]) {
            sprintf(msg, "Tile size cannot be smaller than correlation window size. Adjusting tile size...");
            zvmessage(msg, "");
            tileSize[0] = corrSz[0];
         }
      }

      if (tileSize[1] < 0)
         tileSize[1] = nll;
      else {
         if (tileSize[1] < corrSz[1]) {
            sprintf(msg, "Tile size cannot be smaller than correlation window size. Adjusting tile size...");
            zvmessage(msg, "");
            tileSize[1] = corrSz[1];
         }
      }
   }
}



void initializeTiles(std::vector<tile> &tiles, int &nbTileX, int &nbTileY, int nll, int nsl, int tileSize[2], int corrSz[2]) {

   // How many tile can be fitted in X direction
   nbTileX = nsl/tileSize[1];
   // Is there enough pixel in the "left over" to get at least one correlation?
   if ((nsl % tileSize[1]) > corrSz[1])
      nbTileX++;

   // How many tile can be fitted in Y direction
   nbTileY = nll/tileSize[1];
   // Is there enough pixel in the "left over" to get at least one correlation?
   if ((nll % tileSize[0]) > corrSz[0])
      nbTileY++;

   // Now, for each tile, compute its top-left, bottom-right and center pixels
   // coordinates
   for (int i=0; i<nbTileY; i++) {

      int sY = i*tileSize[1];
      int eY = sY+tileSize[1]-1;
      if (i == (nbTileY-1))
         eY = nll-1;

      for (int j=0; j<nbTileX; j++) {

         int sX = j*tileSize[0];
         int eX = sX+tileSize[0]-1;
         if (j == (nbTileX-1))
            eX = nsl-1;

         tile t;
         t.xs = sX;
         t.ys = sY;
         t.xe = eX;
         t.ye = eY;
         t.xc = sX + (eX-sX)/2;
         t.yc = sY + (eY-sY)/2;
         tiles.push_back(t);
      }
   }
}


// This function primarily computes ties points between the L and R images by
// projecting a list of pixels from the left images onto a plane (at a given
// distance and orientation) and backprojecting them to the right image.
// More importantly, there are a bunch of checks, both on the validity of the
// tiepoints but also on the validity of the projection plane:
// - Limit on the difference between projection plane orientation and Left and 
//   Right pixel look vectors to avoid seeing plane from "under" or with 
//   too "grazing" of an angle.
// - XYZ (Left pixel intersection with plane) within FOV of R camera. The XYZ
//   must be seen in the right image.
// - XYZ not too close (user defined limit) to the optical center of the R
//   camera. This is for instability if too close with non-linear (e.g. cahvore)
// - Check that at least 4 tiepoints whose locations in Left images are not
//   collinear. Ultimately, these tiespoints will define an homography. If 
//   aligned, we'll get junk.
std::vector<tiePoint> getTies(camGeometry & leftC, camGeometry & rightC, 
                             tile &tile, const int &gridX, const int &gridY, 
                             const float &range, const float &minIncidence) {

   double x, y;
   PigPoint Ccenter, PxyzCenter, Pxyz;
   PigVector look, vect;

   // Container for storing outpt tiepoints
   std::vector<tiePoint> ties;

   // Get the size of the current tile
   int nbRows = tile.ye - tile.ys + 1;
   int nbCols = tile.xe - tile.xs + 1;

   // Get the XYZ of the center of tile at the given range
   leftC.cm->LStoLookVector(tile.yc, tile.xc, Ccenter, look, leftC.cs);
   PxyzCenter = Ccenter + look * range;


   // Iterate over the subgrid of points on the Left tile and project them out
   // on the plane, then back to Right image, and store the corresponding R x,y
   for (int j=0; j<nbRows; j += gridY) {

      for (int i=0; i<nbCols; i += gridX) {
  
         // Get current L pixel of subgrid look direction and center
         leftC.cm->LStoLookVector(tile.ys + j, tile.xs + i, Ccenter, look, leftC.cs);


         // If the look direction is negative w.r.t the plane normal, then 
         // compute the XYZ intersection of the ray with the plane passing 
         // through PxyzCenter, which is the center of the tile projected at 
         // "range".

         // If the look direction is positive w.r.t. the plane normal, then the
         // current Left pixel is seeing the plane from "under" or there is no 
         // intersection possible (divergent). Skip that point.
         if ((tile.planeNorm % look) >= (-minIncidence)) 
            continue;

         Pxyz = Ccenter + look * (((PxyzCenter - Ccenter) % tile.planeNorm) /
                (tile.planeNorm % look));


         // Check that the XYZ is in FOV of the R camera
         vect = Pxyz-rightC.pos;
         vect.normalize();
         if ((vect % rightC.look) <= rightC.minCosine) 
            continue;

         // Check that the plane is not seen from "under" from R camera or at a
         // too grazing angle
         if (vect % tile.planeNorm >= (-minIncidence)) 
            continue; 

         // Check for some closeness limit (if required by user) between the XYZ
         // and the optical center of the R camera. If too close, can lead to 
         // XYZtoLS convergence issue with non-linear camera
         if ((rightC.closenessLimit > 0)  &&  
             ((Pxyz - rightC.pos).magnitude() < rightC.closenessLimit)) 
            continue;
                           

         // If all is good, get the R x,y that correspond to the XYZ and store 
         // the tiepoint
         rightC.cm->XYZtoLS(Pxyz, 0, &y, &x, rightC.cs);
         ties.push_back({(float)(tile.xs + i), (float)(tile.ys + j), 
                         (float)x, (float)y});
      }
   }

   // Before returning ties, check that there are at least 4 points that are not
   // colinear.
   int ycur = -1, nX = 0, nY = 0, newLine = 0;
   for (const auto & tie: ties) {
      if (ycur == tie.yL) {
         if (newLine) {
            nX++;
            newLine = 0;
         }
      }
      else {
         ycur = tie.yL;
         newLine = 1;
         nY++;
      }
   }  
   // If the minimum number of non-colinear points is not found, clear ties 
   // container.
   if (nX < 2 || nY < 2)
      ties.clear();

   return ties;
}
	


void printScales(const std::set<float> &scaleSet) {

   char * msg2 = new (std::nothrow) char[11 + scaleSet.size()*7]; 
   char *pos = msg2;
   pos+= sprintf(pos, "at scales:");
   for (auto pv:scaleSet)
      pos+= sprintf(pos, "%7.2f", pv);
   zvmessage(msg2, "");
   delete [] msg2;
}



void printProgress(int nbTiles) {

   static int counter = 0;
   static std::map<int,int> progress;
   //static char msg[10];
   char msg[10];

   // initialization
   if (nbTiles) {

      // Take care of a new initialization
      counter = 0;
      progress.clear();

      int inc = (int)(nbTiles/10.0 + 0.5);
      int val = 1;
      while (val < 10) {
         progress.insert(std::make_pair(val*inc,val*10));
         val++;
      } 
      progress.insert(std::make_pair(nbTiles-1,val*10));

      // Progress starting now...
      sprintf(msg, "0%%...");
      zvmessage(msg, "");
   }
   else {
      counter++;
      auto it = progress.find(counter);
      if (it != progress.end()) {
         sprintf(msg, "%d%%...", it->second);
         zvmessage(msg,"");
      } 
   }
}





   // Some post-filtering:
   // - Filter out disparities that are outside the Right image footprint
   // - Cancel disparities whose correlation score is less than user threshold
   // - Count number of valid correlation
int simpleCorrelationFilter(SimpleImage<float> * corrScores, 
                            SimpleImage<float> * locX, 
                            SimpleImage<float> * locY, 
                            int nlr, int nsr,
                            int minScore) {

   float dx, dy, score;
   int nl = corrScores->getNL();
   int ns = corrScores->getNS();
   int nbCorrValid = nl * ns;

   for (int j=0; j<nl; j++) {
      for (int i=0; i<ns; i++) {
         dx = locX->get(j,i);
         dy = locY->get(j,i);
         score = corrScores->get(j,i);
         if ((dx<0) || (dx>(nsr-1)) || (dy<0) || (dy>(nlr-1))) {
            locX->set(j, i, 0.0);
            locY->set(j, i, 0.0);
            corrScores->set(j, i, 0.0);
            nbCorrValid--;
         }   
         else if (score < minScore) {
            locX->set(j, i, 0.0);
            locY->set(j, i, 0.0);
            nbCorrValid--;
         }
         else {}
      }
   }

   return nbCorrValid;

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


 






// Function extracting/generating the raw topography prior for the tiles. That 
// is, we'll update each tiles with a set of raw ranges and surface normal. 
// Later on, the program will refine the raw ranges list such that successive 
// ranges in the list correspond to the user defined step along the epipolar 
// line. We'll also discard ranges that are out of FOV.
// There are three ways to define the raw ranges (in priority order):
// [1] The user supplied a mesh (.OBJ) that is used to define a small list of
// ranges around the surface given by the mesh. The surface normal is defined
// from the mesh too.
// [2] The user supplied a range and/or surface normal file. This would 
// typically be a vicar file _RNG and/or _UVW file. These files have to be in
// the left image frame.
// [3] No suface prior is supplied, the raw ranges will be defined from the 
// MIN/MAX_RANGE and sammpling strategy.
//
// For a given tile, the min/max of the ranges and the average normal 
// orientations of all pixels of that tile are computed. If no pixel of that 
// tile "overlap" with the topo prior (mesh or file), then the tile range and 
// normal are defined depending on the user's choice:
// [1] Empty: the tiles are left empty and not processed
// [2] Interpolate: the ranges and normals of the empty tiles are interpolated
// from the ranges and normals of the non-empty tiles using Radial Basis 
// Function (RBF) interpolator.
// [3] Full: The empty tiles are filled with the nominal full range scan
// between MIN/MAX_RANGE and using the range sampling method defined by
// SAMP_RANGE  
void getRangeNormalPrior(tiler &tiler, camGeometry &leftC,
                         const planeSpace &planeInfo) {

   // Any topo prior input?
   int wMesh = 0;
   int wRange = 0;
   int wNormal = 0;
   zvpcnt("IN_MESH", &wMesh);
   zvpcnt("IN_RANGE", &wRange);
   zvpcnt("IN_NORMAL", &wNormal);

   int priorInput = (wMesh == 0 && wRange == 0 && wNormal == 0) ? 0 : 1;


   // Some checks on the inputs as some combination of parameter are not allowed
   if ( (wRange || wMesh) && (planeInfo.rangeSampling && (planeInfo.rangeStep < 0))){
      zvmessage("LINEAR_STEP cannot be <0 when IN_RANGE or IN_MESH is used","");
      zabend();
   }


   // Should the tiles not covered by the topography prior get to scan the
   // full  min/max range/plane orientation or be left empty
   int fullSweep = zvptst("FULL_SWEEP");


   // If no input prior and FULL_SWEEP is deactivated, then no initialization of
   // the ranges. Nothing to do. Force FULL_SWEEP and display message
   if ((priorInput == 0) && (fullSweep == 0)) {
      zvmessage("Forcing FULL_SWEEP as input topo prior is null","");
      fullSweep = 1;
   }

   // Get the default ranges for full_sweep, between min/max ranges. It's cheap
   // so compute it even if full_sweep is off.
   std::vector<float> defaultRanges = getRangeFromDefault(planeInfo);


   // Do we have a user defined projection plane?
   int defaultPlane = (planeInfo.planeNormal == PigVector(0,0,0)) ? 0 : 1;


   // For planes defined not from a topo prior, should we add the horizontal plane
   int useHorizontalPlane = zvptst("LEVEL_PLANE");
   PigVector hplane = PigVector(0,0,-1);



   // If no a priori information on the range and normal have been supplied by
   // the user, the default is to sweep all ranges within MIN/MAX_RANGE and
   // use the default/user/multiplane normals. 
   // Do that and exit.
   if (!priorInput) {

      for (auto &t: tiler.tiles) {
         // List of ranges sampled between MIN/MAX_RANGES
         t.rawRanges = defaultRanges;

         // List of plane normals
         // 3 possibilities:
         // - User provided a specific plane orientation (PLANENORM)
         // - User asked for multiplane sampling (MULTI_PLANE)
         // - If no User input, default to plane perp to tile look vector
         //
         // In addition, the horizontal plane (flat surface) will be added to
         // the list if asked by the user (LEVEL_PLANE) 

         t.planeNormArr.clear();

         // User provided a unique plane orientation
         if (defaultPlane) 
            t.planeNormArr.push_back(planeInfo.planeNormal);
         // User asked for multi plane orientations
         else if (planeInfo.multiPlane)
            getPlaneOrientationList(t.look, planeInfo.numTilts, t.planeNormArr);
         // Otherwise, the default is to use the tile look direction as the 
         // normal plane
         else {
            t.planeNormArr.push_back(t.look * (-1));
         }

         // Specific case of the level plane (horizontal surface)
         if (useHorizontalPlane) {
            // No need to add horizontal plane if user ask specifically for
            // it as it's already there
            if (t.planeNormArr.size() != 1 || !(t.planeNormArr[0] == hplane))
               t.planeNormArr.push_back(hplane);
         }
      }

      return;

   }



   // User supplied some topo prior...
 

   // If both mesh and range/normal files were supplied, inform user that only
   // mesh will be used
   if ( (wRange && wMesh) || (wNormal && wMesh))
      zvmessage("IN_RANGE and/or IN_NORMAL ignored if IN_MESH is supplied","");



   MeshMan * mesh = nullptr;
   SimpleImage<float> * normalImg = nullptr;
   SimpleImage<float> * rangeImg = nullptr;

   // Mesh loading
   // If the user supplied a prior mesh file, now is the time to load it. The 
   // tiles will be populated with the raw ranges and plane normal defined from 
   // the mesh. Mesh input has precedence over range/normal files. 
   if (wMesh != 0) {
      if (wMesh != 2) {
         zvmessage("If using IN_MESH, the XYZ file must also be supplied","");
         zabend();
      }
      char filename[PIG_MAX_FILENAME_SIZE+1];
      
      // Open the XYZ file associated to the mesh to get the Coordinate Sytem.
      // This is a bit of a hack. There is no easy way to get the CS from the 
      // mesh ancillary, so we get it from the XYZ from which was derived the 
      // mesh. It is assumed that the XYZ CS and mesh CS are identical. 
      int xyz_unit;
      zvpone("IN_MESH", filename, 2, PIG_MAX_FILENAME_SIZE);
      PigMission * mXYZ = PigMission::getMissionObject(filename, &xyz_unit);
      PigFileModel * xyz_model = PigFileModel::create(filename);
      PigRoverStateManager * rsm = mXYZ->getRoverStateManager();
      if (rsm)
         rsm->addFileCoordSystems(xyz_model);
      PigCSReference *ref;
      xyz_model->getDerivedImageCS(ref);
      PigCoordSystem * cs_mesh = mXYZ->getCoordSystem(ref);
      zvclose(xyz_unit, NULL);

      // Load the mesh
      zvpone("IN_MESH", filename, 1, PIG_MAX_FILENAME_SIZE);
      mesh = new MeshMan(filename, cs_mesh);

      // Convert mesh to be sure.
      // Note: Should be able to check for CS equality instead of de facto
      // conversion. 
       mesh->convertCS(leftC.cs);
   } 
   else { 

      // If the user supplied a file for range and/or normal, now is the time to
      // load them. The tiles will be populated with the initial values of range
      // and plane normal based on the local values. 
      if (wRange != 0) 
         read_image("IN_RANGE", rangeImg);

      if (wNormal != 0) 
         read_image("IN_NORMAL", normalImg);
   }








   // Load the topographic priors and fill the tiles according to the user
   // choice

   // priority is given to mesh over range/normal files
   if (mesh != nullptr) {
      // Get the average range and surface normal for each tiles from the mesh
      for (auto &t: tiler.tiles)
         getRangeAndNormalFromMesh(mesh, leftC.cm, t, planeInfo.multiPlane);
      // No need of mesh anymore
      delete mesh;
   }
   else {
      if (rangeImg != nullptr) {
         // Get min/max ranges for each tile from the range file
         for (auto &t: tiler.tiles) 
            getRangeFromFile(rangeImg, t);
         // No need of range image anymore
         rangeImg->free();
         delete rangeImg;
      }
      if (normalImg != nullptr) {
         // Get surface normal for each tile from the normal file
         for (auto &t: tiler.tiles) 
            getNormalFromFile(normalImg, t, planeInfo.multiPlane);
         // No need of normal image anymore
         normalImg->free();
         delete normalImg;
      }
   }




   // Now that the range and normal priors are computed, enlarge the range
   // bracket (given by the min/max of input prior for a given tile) with 
   // +- step in range direction to define the raw search range bracket. This
   // is necessary to account for input prior error
   // Note that the same idea (enlarging the search space) is not applied to
   // the plane orientation at the moment.
   for (auto &t: tiler.tiles) {
      // If the number of rawRangesPrior is not 2, then this tile didn't have
      // coverage in the input prior (mesh or range file)
      //if (t.rawRangesPrior.size() == 2) {
      if (t.rawRanges.size() == 2) {
         float minStep, maxStep, minRange, maxRange;
         minRange = t.rawRanges.front();
         maxRange = t.rawRanges.back();
         if(!planeInfo.rangeSampling) {
            minStep = minRange * pow(10.0, floor(log10(minRange)) - planeInfo.rangeStep);
            maxStep = maxRange * pow(10.0, floor(log10(maxRange)) - planeInfo.rangeStep);
         }
         else {
            minStep = planeInfo.rangeStep;
            maxStep = planeInfo.rangeStep;
         }
         // and save the two additional ranges
         t.rawRanges.insert(t.rawRanges.begin(), minRange - minStep);
         t.rawRanges.push_back(maxRange + maxStep);
      }
   }


      
   // If fullSweep is asked by user for tiles that the mesh or files didn't 
   // cover, get the default raw ranges (MIN/MAX_RANGE) and normal(s).
   //if (tiler.seeding && fullSweep) {
   if (fullSweep) {

      for (auto &t: tiler.tiles) {
         // Fill the tile if it's empty
         if (t.rawRanges.size() == 0) { 

            // Fill the ranges
            t.rawRanges = defaultRanges;

            // Fill the plane normals

            t.planeNormArr.clear();
            // User provided a unique plane orientation
            if (defaultPlane)
               t.planeNormArr.push_back(planeInfo.planeNormal);
            // User asked for multi plane orientations
            else if (planeInfo.multiPlane)
               getPlaneOrientationList(t.look, planeInfo.numTilts, t.planeNormArr);
            // Default to tile look to define plane orientation
            else
               t.planeNormArr.push_back(t.look * (-1));

            if (useHorizontalPlane) {
               // No need to add horizontal plane if user ask specifically for
               // it as it's already there
               if (t.planeNormArr.size() != 1 || !(t.planeNormArr[0] == hplane))
                  t.planeNormArr.push_back(hplane);
            }
         }
      }
   }

   // End of the raw topography prior generation
}









// This function serves same objective as function above (getRandeNormalPrior),
// except than instead of finding the raw ranges/normal from a topo prior, it
// computes them from ranges found at a previous pyramid level.
// The previous pyramid level range image is stored in the planeSpace struct.
void getRangeFromPyr(tiler &tiler, camGeometry &leftC,
                     const planeSpace &planeInfo,
                     SimpleImage<float> *ranges,
                     SimpleImage<PigVector> * normals) {



   // Should the tiles not covered by successful correlation in the previous
   // pyramid level be run with a full  min/max range?
   int fullSweep = zvptst("FULL_SWEEP");


   // The input range image is from the previous pyramid level, so it is half
   // size compared to the current one. We'll do two things here, 
   // first interpolate the range value for the current image size (using a 
   // simple nearest neighboor), and count the number of pixels with valid
   // range values per tile. This will be useful for the subsequent 
   // extrapolation of the ranges for the tiles that don't have any. 
   // TODO: Note that this approach is not rigorous, and might lead to a off
   // ranges, especially if the plane normal was far from perpendicular to 
   // tile center view angle. The range of a new tile should account for the
   // plane orientation to "adjust" the range from previous pyr. Might need to
   // account for it. For now, leaving it as is.
   SimpleImage<float> *rangeImg = new SimpleImage<float>(tiler.nll, tiler.nsl);
   rangeImg->zero();
   std::vector<int> tindices(tiler.tiles.size());
   int index = 0;
   for (auto &t: tiler.tiles) { 
      int totNonNull = 0;
      for (int j=t.ys; j<=t.ye; j++) {
         for (int i=t.xs; i<=t.xe; i++) { 
            float r = ranges->get(j/2, i/2);
            if (r !=0) {
               rangeImg->set(j, i, r); 
               totNonNull++; 
            }
         }
      }
      tindices[index] = totNonNull;       
      index++;
   }



   // Same thing with Plane orientation. The extrapolation will be different 
   // than for range. They'll be replicated only. This might change in the futur.
   // Also, instead of using a simpleimage of pigVector, we use a 3 bands 
   // SimpleImage just because of re-use of the getNormalFromFile which was 
   // designed before. Would need to be homogeneized. 
   SimpleImage<float> *normalImg = new SimpleImage<float>(3, tiler.nll, tiler.nsl);
   normalImg->zero();
   for (auto &t: tiler.tiles) { 
      for (int j=t.ys; j<=t.ye; j++) {
         for (int i=t.xs; i<=t.xe; i++) { 
            PigVector v = normals->get(j/2, i/2); 
            if (!(v == PigVector(0,0,0))) {
               normalImg->set(0, j, i, v.getX()); 
               normalImg->set(1, j, i, v.getY()); 
               normalImg->set(2, j, i, v.getZ()); 
            }
         }
      }
   }



   //Once the range image from the previous pyramid level is interpolated,
   //we look at the empty tiles and see if their direct neighbors could be used
   //to extrapolate their range (linear fit) to fill these tiles and replicate 
   //the plan orientations. This is only a local (neighbor only) extrapolation/
   //replication mostly aiming at removing tiling artefact on correlated areas
   //borders.

   // Look up for each empty tiles if its direct neighboors have range values.
   // If that is the case, then a plane fit in the range space is done on the
   // neighbors tiles to extrapolate range values of the 4 corners of the empty
   // tile
   for (int i=0; i<tindices.size(); i++) {
   
      // If current tiles has range values, nothing to do. Move to next tile
      if (tindices[i] != 0)
         continue;

      tile &t = tiler.tiles[i];

      // Get number of elements to do the plane fit. That is get the number of
      // range values that all neighbors tiles have.
      int numE = 0;
      for (auto t2 : t.N) 
         numE += tindices[t2];

      // If 0, then all neighbors tiles are also empty. Nothing to fit, skip.
      if (numE == 0)
         continue;

      // Setup matrice and vector for Least Square
      MatrixXf A(numE, 3);
      VectorXf B(numE);

      // Set a container to list the plane orientation(s) of the neighbors
      std::vector<PigVector> planeNormals;      

      //Populate A and B
      int row = 0;
      for (auto j : t.N) { 
         if(tindices[j] == 0)
            continue;
         tile &t2 = tiler.tiles[j];
         for (int l=t2.ys; l<=t2.ye; l++) {
            for (int s=t2.xs; s<=t2.xe; s++) { 
               if (rangeImg->get(l,s) !=0) { 
                  A(row, 0) = s;
                  A(row, 1) = l;
                  A(row, 2) = 1;
                  B(row) = rangeImg->get(l,s);
                  row++;

                  // While we are iterating over the neighbors, get their plane
                  // orientations and store them if that vector hasn't.
                  // Note. This is not an efficient approach. To be improved
                  // eventually.
                  PigVector p = PigVector(normalImg->get(0,l,s),
                                          normalImg->get(1,l,s),
                                          normalImg->get(2,l,s));
                  auto loc = std::find(planeNormals.begin(), 
                                       planeNormals.end(), 
                                       p);
                  if (loc == planeNormals.end())
                     planeNormals.push_back(p);

               }
            }
         }

      }

      // Apply Least Square to define plane
      MatrixXf W(3, 1);
      //W = (A.transpose() * A).ldlt().solve(A.transpose() * B);
      W = A.colPivHouseholderQr().solve(B);


      // Apply plane fit to the 4 corners of the empty tile. Only the 4 corners
      // as next step is to look for min/max range
      // Filter ranges outside of min/max range. Extrapolation can bring range
      // "behind" camera.
      float range =  W(0) * t.xs + W(1) * t.ys + W(2);
      if (range > planeInfo.minRange && range < planeInfo.maxRange)
         rangeImg->set(t.ys, t.xs, range);
      range =  W(0) * t.xs + W(1) * t.ye + W(2);
      if (range > planeInfo.minRange && range < planeInfo.maxRange)
         rangeImg->set(t.ye, t.xs, range);
      range =  W(0) * t.xe + W(1) * t.ye + W(2);
      if (range > planeInfo.minRange && range < planeInfo.maxRange)
         rangeImg->set(t.ye, t.xe, range);
      range =  W(0) * t.xe + W(1) * t.ys + W(2);
      if (range > planeInfo.minRange && range < planeInfo.maxRange)
         rangeImg->set(t.ys, t.xe, range);


      // Assign all plane vectors found in the neighboring tiles to the 
      // current tiles. They are just put in the tile at random positions and
      // will be filtered out in next step.
      // This is hackish and ugly. The whole data management should be revised.
      int ind = 0;
      for (auto p: planeNormals) {
         if (!(p==PigVector(0,0,0))) {
            normalImg->set(0, t.ys, t.xs + ind, p.getX());
            normalImg->set(1, t.ys, t.xs + ind, p.getY());
            normalImg->set(2, t.ys, t.xs + ind, p.getZ());
            ind++;
         }
      }

   }



   // From here, the function is about the same as getRangeNormalPrior
   // Some mutualisation is in order. To be done eventually.
   

 
   // Get min/max ranges for each tile from the range file.
   for (auto &t: tiler.tiles) { 
      getRangeFromFile(rangeImg, t);
      getNormalFromFile(normalImg, t, planeInfo.multiPlane); 
   }


   // No need of range image anymore
   rangeImg->free();
   delete rangeImg;
   normalImg->free();
   delete normalImg;



   // Now that the range and normal priors are computed, augment the range 
   // list with +- step in range direction to define the raw search range 
   // bracket

   // Get the range step. We're considering range-step and range+sep in
   // addtion to current range (i.e., 3 ranges total).
   // For POWER_LAW sampling, the step depends on the range value
   for (auto &t: tiler.tiles) {
      // Only if we have 2 ranges (min and max found for the given tile from
      // the prior topo). If 0, then either the user asked ranges not covered
      // by prior to be left empty or asked a full_sweep which will be done 
      // below.
      // Note that the two ranges (min and max) could be the same, which would
      // correspond (may be not exclusively) to the situation when both camera
      // have the same C (optical centers)

      if (t.rawRanges.size() == 2) {
         float minStep, maxStep, minRange, maxRange;
         minRange = t.rawRanges.front();
         maxRange = t.rawRanges.back();
         if(!planeInfo.rangeSampling) {
            minStep = minRange * pow(10.0, floor(log10(minRange)) - planeInfo.rangeStep);
            maxStep = maxRange * pow(10.0, floor(log10(maxRange)) - planeInfo.rangeStep);
         }
         else {
            minStep = planeInfo.rangeStep;
            maxStep = planeInfo.rangeStep;
         }
         // and save the two additional ranges
         t.rawRanges.insert(t.rawRanges.begin(), minRange - minStep);
         t.rawRanges.push_back(maxRange + maxStep);
      }

   }



//When using FULL_SWEEP, there is a difference between the first iteration (getRangePrior)
//and PYRAMID ones. One migh want FULL_SWEEP on the first pass but not on the other
//ones as they could lead to large processing time if a lot of tiles are empty.
//There is no way with current PARAMETER to differentiate.
//For now, FULL_SWEEP is only used at first pyramid

}




void updateEpiShift(tiler &tiler, camGeometry &leftC, camGeometry &rightC, 
                    SimpleImage<float> *epiOffX, SimpleImage<float> *epiOffY) {


   std::vector<int> emptyTiles;

   // For each tile, compute the new X/Y offset to apply to the transform to 
   // "re-align" the epipolar line
   for (int i=0; i<tiler.tiles.size(); i++) {

      tile &t = tiler.tiles[i];

      // Compute the perp epipolar direction for the current tile. We assume
      // the epipolar curve to be a line at the scale of the tile. For that,
      // we project the center pixel of the tile onto the Right image at the
      // two extreme range (min, max) that are in the FOV of R
      double x1, y1, x2, y2;
      PigPoint point, p1, p2;
      PigVector vector;
      leftC.cm->LStoLookVector(t.yc, t.xc, point, vector, leftC.cs);

      // Case where ranges array is empty. For instance if all tile is out of
      // Right FOV or if that tile's previous result has been filtered out. 
      // Nothing to do in that case.
      // Also there's the case of 1-element which would correspond to the L and
      // R having same C (same optical center) - Normally epipolar shift 
      // correction should be disabled in that case, but just in case.
      if (t.ranges.size() <= 1) {
         t.perpEpi[0] = 0;
         t.perpEpi[1] = 0;
         continue;
      }

      p1 = point + vector * t.ranges.front();
      p2 = point + vector * t.ranges.back();
      rightC.cm->XYZtoLS(p1, FALSE, &y1, &x1, rightC.cs);
      rightC.cm->XYZtoLS(p2, FALSE, &y2, &x2, rightC.cs);
      float mag = sqrt((x2-x1)*(x2-x1) + (y2-y1)*(y2-y1));
      // Should not happen, but in case
      if (mag == 0.0) {
         t.perpEpi[0] = 0;
         t.perpEpi[1] = 0;
         continue;
      }
      float perpX = (float)((y2-y1)/mag);
      float perpY = (float)(-(x2-x1)/mag);


      // Now that the perp-epi vector is defined for the current tile,
      // get the average offset in perp-epi direction for that tile
      // using the previous pyramid layer correlation
      double avg=0;
      int num=0;
      for (int jj=t.ys; jj<=t.ye; jj++) {
         for (int ii=t.xs; ii<=t.xe; ii++) {
            if (epiOffX->get(jj/2,ii/2) != 0) {
               float offX = epiOffX->get(jj/2, ii/2);
               float offY = epiOffY->get(jj/2, ii/2);
               avg += perpX * offX + perpY * offY;
               num++;
            } 
         }
      }

      if (num!=0)
         avg /= num;
      else
         // At this stage, the tiles has valid ranges, but epiOff is null. This
         // most probably indicates that this tile has been cleared by STAT 
         // filter, but has been repopulated with range/normal extrapolation with
         // neighbors tiles. Save its id for later extrapolation of epiX/Y
         emptyTiles.push_back(i);

      // Get the shift to apply to the upcoming tile transform to realign
      // the perp epipolar offset
      t.perpEpi[0] = 2 * avg * perpX; //x2 because shift is from pre pyramid
      t.perpEpi[1] = 2 * avg * perpY;

           
   } //end for loop on tiles


   // Extra step
   // Look up empty tiles, and if they have neighbors which are non empty, assign the 
   // perp epi offset (or average between all neighbors having values). This is in
   // accordance with the treatment done with ranges and normals, and is meant to 
   // salvage the tiles that were filtered out by the STAT filter because they were on
   // the edges of valid correlation area
   for (int i = 0; i <emptyTiles.size(); i++) {

      tile &t = tiler.tiles[i];

      float perpEpiX = 0;
      float perpEpiY = 0;
      float num = 0;

      // Look up current tile neighbors
      for (int j : t.N) {
         // Is current neighbors empty?
         auto it = std::find(emptyTiles.begin(), emptyTiles.end(), j);
         if (it != emptyTiles.end()) {
            perpEpiX += tiler.tiles[j].perpEpi[0];
            perpEpiY += tiler.tiles[j].perpEpi[1];
            num++;
         }
      }
      if (num != 0) {
         t.perpEpi[0] = perpEpiX/num;
         t.perpEpi[1] = perpEpiX/num;
      }
   }

}





// This is a first attempt at outliers removal.
// It is based on the assumption that disparity changes smoothly, so we're 
// checking that the neighborood of a given pixel has a disparity similar to 
// that given pixel. The neighborood is defined as the correlation template size
// slightly augmented by a few pixels. The reason is that a salient feature will
// be seen in a stretch of contiguous pixels, depending on the template size. 
// This may cause a patch of uniform outliers (pixel locking effect, also known 
// as the fattening effect), which will satisfy the smoothness criteria. 
// Therefore a neighborood slightly larger than the template size is taken. 
// Two thresholds are used to check the validity of a pixel:
// - the allowed disparity amplitude difference between the given pixel and the
//   ones in the neighborood
// - the minimum number of pixels in the neighborood that need to satisfy the 
//   disparity amplitude criteria to deem the current pixel not an outlier
//
// The filter works like this (think of it as sort of median filter):
// For a given pixel:
// - Compute the disparity difference between the pixels of the neighborood and 
//   the disparity of that given pixel ("remove" the line/samp offset before). 
// - Count the number of pixel whose difference is less than a threshold
// - If that number is larger than a threshold, the current pixel is valid.
//   Otherwise, it is deemed an outliers.

void localCoherenceFilter(SimpleImage<float> * corrScores,    
                          SimpleImage<float> * locX,     
                          SimpleImage<float> * locY, 
                          SimpleImage<double> * coefs_a, 
                          SimpleImage<double> * coefs_b, 
                          SimpleImage<double> * coefs_d, 
                          SimpleImage<double> * coefs_e, 
                          int corrSz[2],
                          int statFilterExtent, 
                          float statFilterScalerThreshold, 
                          int statFilterThresholdN,
                          int omp_on,
                          SimpleImage<int> *& mask) { // output


   // Check the validity of mandatory inputs/outputs
   if (corrScores == nullptr || locX == nullptr || locY == nullptr) {
      zvmessage("Invalid mandatory input image for local coherence filter, returning...","");
      return;
   }

   // Get the size of the diparity maps to filter
   int ns = corrScores->getNS();
   int nl = corrScores->getNL();
   
   // Check validitiy of pointers to coefs_a(b)(d)(e)
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


   // Initialize the output mask
   mask = new SimpleImage<int>(nl, ns);
   mask->zero();


   // Loop over all the pixel of the disparity map and check the validity of the
   // disparity with respect to the disparity of the neigboring pixels
   #pragma omp parallel for if (omp_on)
   for (int loc = 0 ; loc < ns*nl; loc++) {

      int i = loc % ns;
      int j = loc / ns;

      // If current pixel is invalid, nothing to check, move to next one.
      if (corrScores->get(j, i) == 0.0) 
         continue;

      // Get the affine coefficients of the queried pixel. This is needed to
      // "center" the disparity with respect to the queried one and to define 
      // the neighborood area to check for consistency
      float _coef_a = coefs_a->get(j,i);
      float _coef_b = coefs_b->get(j,i);
      float _coef_d = coefs_d->get(j,i);
      float _coef_e = coefs_e->get(j,i);

      // Compute the determinant of the affine transform. This gives an idea of
      // the "scale" factor between the left and right image (it assumes same 
      // scaling in X and Y though). Both the "neighborhood" and allowed 
      // "pixel distance" depend on it.
      float det = std::abs(_coef_a * _coef_e - _coef_d*_coef_b);

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
      if (det < 1) {
         float scaler = 1.f / sqrt(det);
         hlx = (int)(scaler * (corrSz[0] - 1)/2) + statFilterExtent;
         hly = (int)(scaler * (corrSz[1] - 1)/2) + statFilterExtent;
         statFilterThreshold = scaler * statFilterScalerThreshold;
      }
      else {
         // In that case, L low res compared to R, only the correlation 
         // precision must be scaled. The correlation window size was not 
         // affected.
         float scaler = sqrt(det);
         hlx = (corrSz[0] - 1)/2 + statFilterExtent;
         hly = (corrSz[1] - 1)/2 + statFilterExtent;
         statFilterThreshold = scaler * statFilterScalerThreshold;
      }

      // Note, depending on the transform, hlx/hly can be very large. A check
      // in the main function prevent this. If this function is to be 
      // externalized, the check must be set

      // Number of pixel in the neighborood
      long int nbN = ((long int)(hly*2+1)) * ((long int)(hlx*2+1));

      // Get the offset of the affine transform of the queried pixel
      float offsetX = locX->get(j, i) - (_coef_a*i + _coef_b*j);
      float offsetY = locY->get(j, i) - (_coef_d*i + _coef_e*j);
   
      // Compute difference in disparity for neighboring pixels and queried one
      std::valarray<float> arrX((std::numeric_limits<float>::max)(), nbN);
      std::valarray<float> arrY((std::numeric_limits<float>::max)(), nbN);
      int countOutside = 0;      // counter for edge effect
      int pos = 0;
      for (int l = j-hly; l<j+hly+1; l++) {
         for (int s = i-hlx; s<i+hlx+1; s++) {
            if (l>=0 && l<nl && s>=0 && s<ns) {
               arrX[pos] = _coef_a * s + _coef_b * l + offsetX - locX->get(l,s);
               arrY[pos] = _coef_d * s + _coef_e * l + offsetY - locY->get(l,s);
            }
            pos++;
         }
      }


      // Threshold on the number of pixels in the neigborood that need to have a
      // disparity "similar" to the queried pixel
      float thresholdN = (int)(((nbN - countOutside) * statFilterThresholdN)/100.0) ;

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

   
   // Deallocate image if necessary
   if (!coefs_a_valid)
      coefs_a->free();

   if (!coefs_b_valid)
      coefs_b->free();

   if (!coefs_d_valid)
      coefs_d->free();

   if (!coefs_e_valid)
      coefs_e->free();

   return;

}



void applyLocalCoherenceFilter(SimpleImage<float> * input, 
                               SimpleImage<int> * mask)  {
                              
   applyLocalCoherenceFilter<float>(input, mask, 0);
}

template<class T>
void applyLocalCoherenceFilter(SimpleImage<T> * input, 
                               SimpleImage<int> * mask, 
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



// Compute the largest view angle between the camera orientation and 
// any pixels, usually one of the image corner. So we're checking only the four
// corners.
void getMinMaxLookAngle(PigCameraModel *cm, int ns, int nl, PigCoordSystem *cs,
                        float &minCosine, float &maxCosine) {

   PigVector viewCenter, view;
   PigPoint posCenter, pos;

   // Get position and view angle of center pixel. This is considered the 
   // camera orientation
   cm->LStoLookVector(((float)nl-1.0)/2.0, ((float)ns-1.0)/2.0, posCenter, 
                       viewCenter, cs); 

   // Check cosine angle between camera look angle and the image 4 corners.
   // The smallest the cosine, the larger the angle

   // Top-left
   cm->LStoLookVector(0, 0, pos, view, cs); 
   maxCosine = viewCenter % view;
   minCosine = maxCosine;

   // Top-right
   cm->LStoLookVector(0, ns-1, pos, view, cs); 
   float cosine = viewCenter % view;
   if (cosine > maxCosine) maxCosine = cosine;
   if (cosine < minCosine) minCosine = cosine;

   // Bottom-right
   cm->LStoLookVector(nl-1, ns-1, pos, view, cs); 
   cosine = viewCenter % view;
   if (cosine > maxCosine) maxCosine = cosine;
   if (cosine < minCosine) minCosine = cosine;

   // Bottom-left
   cm->LStoLookVector(nl-1, 0, pos, view, cs); 
   cosine = viewCenter % view;
   if (cosine > maxCosine) maxCosine = cosine;
   if (cosine < minCosine) minCosine = cosine;

   return;
}





void adjustPlaneNorm(camGeometry &camG,
                     const float minHitAngle,
                     tile &tile) {

   PigVector planeNorm, criticalView, viewTL, viewTR, viewBR, viewBL;
   PigPoint center;
   double viewPlaneCosine;

   // Get the tile four corners viewing vectors
   // Top-left corner
   camG.cm->LStoLookVector((float)tile.ys, (float)tile.xs, center, viewTL, camG.cs);
   viewTL.normalize();
   // Top-right corner
   camG.cm->LStoLookVector((float)tile.ys, (float)tile.xe, center, viewTR, camG.cs);
   viewTR.normalize();
   // Bottom-left corner
   camG.cm->LStoLookVector((float)tile.ye, (float)tile.xs, center, viewBL, camG.cs);
   viewBL.normalize();
   // Bottom-right corner
   camG.cm->LStoLookVector((float)tile.ye, (float)tile.xe, center, viewBR, camG.cs);
   viewBR.normalize();



   // Get the minimum angle between viewing angle and plane proper (not its 
   // normal). minHitAngle is > 0. 
   double minAngleRadian = PI/2.0 + minHitAngle;

   // Compute the corresponding cosine
   float maxCosine = cos(minAngleRadian);


   // Check each plane orientation validity, and reorient if necessary. The 4 
   // corners of the tile are checked against the plane to get the "worst"
   // angle possible
   for (int i=0; i < tile.planeNormArr.size(); i++) {

      planeNorm = tile.planeNormArr[i];
      planeNorm.normalize();

      // Test the 4 corners and select the one that would be the closest or most 
      // beyond the threshold (if any)
      viewPlaneCosine = viewTL % (planeNorm);
      criticalView = viewTL;


      double tmpCosine = viewTR % (planeNorm);
      if (tmpCosine > viewPlaneCosine) {
         viewPlaneCosine = tmpCosine;
         criticalView = viewTR;
      }

      tmpCosine = viewBL % (planeNorm);
      if (tmpCosine > viewPlaneCosine) {
         viewPlaneCosine = tmpCosine;
         criticalView = viewBL;
      }

      tmpCosine = viewBR % (planeNorm);
      if (tmpCosine > viewPlaneCosine) {
         viewPlaneCosine = tmpCosine;
         criticalView = viewBR;
      }



      // If plane orientation is good w.r.t. "worst" viewing angle, move to next
      // plane
      if (viewPlaneCosine < maxCosine)
         continue;


      // Plane orientation needs correction:      


      // Angle between the viewing vector and plane normal. 
      double viewPlaneAngle = acos(viewPlaneCosine);

      // How much should the plane normal must be rotated  
      // (around planeNorm x criticalView)
      double rotAngle = minAngleRadian - viewPlaneAngle;

      // Rotate the planeNorm vector
  //    tile.planeNormArr[i] = planeNorm * cos(rotAngle) - criticalView * sin(rotAngle);
      PigVector rotAxe = criticalView * planeNorm;
      rotAxe.normalize();
      tile.planeNormArr[i] = planeNorm * cos(rotAngle) + (rotAxe * planeNorm) * sin(rotAngle);
   }

}








// Construct a list of projection plane orientations.
// Given a look vector, construct a list of vectors that will represent
// the projection plane orientation.
// This approach is derived from the one used by Affine SIFT and is working well
// for sift type algorithms with a good balance between sparsity and coverage.
// However, it is not clear if it is a good one for correlation based technique.
// In particular with this approach, the minimum latitude (apart from the fronto
// plane) is 45 degrees. That means that no plane with an angle less than 45 
// degrees from look angle could be obtained. May/may not be a problem.
void getPlaneOrientationList(const PigVector look,
                             const int numTilts,
                             std::vector<PigVector> &outList) {

   // Make sure the output container is empty
   outList.clear();

   // Define a local reference system, XYZ, "centered" around the look vector.
   // We're only interested in the orientation of XYZ, not in the origin.
   // The tilt and rotation around that local reference system will define
   // the various plane orientations
   
   // Z is assumed to be the negative of the pixel look vector, i.e., Z is the 
   // orientation of a plane perpendicular to the look vector. The list of 
   // planes orientations will consists in "rotating" Z with a given  longitude
   // and latitude.
   PigVector Z = look * (-1);

   // The X vector is the cross product between Z and the ground normal,
   // which is assumed to be (0,0,-1). This implies that the output plane 
   // orientations and input look vector are expressed in the CS whose Z is
   // oriented down.
   PigVector N(0,0,-1);
   PigVector X = Z * N;

   // The Y vector is the cross product between Z and X
   PigVector Y = Z * X;

   // Special case where N = - Z. That is, look vector looking straight down.
   // In that case, the local and global CS are aligned (+- some sign). Manual 
   // initialization of the local CS
   if (N % Z == 1) {
      Z = PigVector(0,0,-1);
      Y = PigVector(1,0,0);
      X = PigVector(0,1,0);
   }



   // Depending on the number of tilts to simulate, compute the list of
   // plane orientation. This approach is derived from the one used by 
   // Affine SIFT.
   std::vector<std::pair<float,float> > tiltsRots;
   double a = sqrt(2.0); // good compromise per paper statement
   double b = 72.0;      // good compromise per paper statement

   for(int tiltIndex=0; tiltIndex<numTilts; tiltIndex++) {

      float tilt = pow(a,tiltIndex);

      // Case with no rotation. Ortho plane.
      if (tilt == 1) 
         outList.push_back(Z); 
      else {
         // Get latitude angle from tilt
         float latitude = acos(1.0/tilt);

         // Rotate Z given above latitude around Y
         PigVector Z1(sin(latitude), 0, cos(latitude));

         // Given current latitude, compute the number of longitude rotations.
         // The larger the latitude, the more longitude rotations
         int numRot = (int)(360.0 * tilt / b);

         for (int rot = 0; rot < numRot; rot++) {
            float theta = b * rot / tilt * PI / 180.;
            PigPoint Z2( Z1.getX() * cos(theta), 
                         Z1.getX() * sin (theta) + Z1.getZ() * cos(theta),
                         Z1.getZ());

            // Express Z2 in the global CS. Rotation matrix between local and 
            // global CS is given by X,Y,Z
            PigVector planeOrientation(Z2.getX()*X.getX() + Z2.getY()*Y.getX() + Z2.getZ()*Z.getX(),
                                       Z2.getX()*X.getY() + Z2.getY()*Y.getY() + Z2.getZ()*Z.getY(),
                                       Z2.getX()*X.getZ() + Z2.getY()*Y.getZ() + Z2.getZ()*Z.getZ());

            // Store plane orientation in output container
            outList.push_back(planeOrientation);
  
         }
      }
   } 

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
SimpleImage<float> * ewa(SimpleImage<float> *imgIn,
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
   SimpleImage<float> * out = nullptr;
   out = new (std::nothrow) SimpleImage<float>(nbY, nbX);
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


   float * ptrOut = out->linePtr(0);

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
         ptrOut[i] = (float)(num/den);

   }

 

   return out;

}
















////////////////////////////////////////////////////////////////////////////
// Read an image tiling labels and constructs a tiling map corresponding to 
// the current image downsampling residual. Output tiling image is the same
// size as the input image.
////////////////////////////////////////////////////////////////////////////
int getTilingInformation(PigFileModel * file_model, SimpleImage<int> *& tI, 
                         std::set<int> &tilingLevel) {

   int status, unit, nret, nret2;
   int sameNbElements = 1;
   int pixelAvgWidth, pixelAvgHeight, downsamplingX[20], downsamplingY[20], 
       startX[20], startY[20], nbX[20], nbY[20];

   try {

      // If current file is open, close it first
      if (file_model->isFileOpen())
         file_model->closeFile();

      // Open the file for reading
      unit = file_model->getUnit();
      zvopen(unit, "OP", "READ", "U_FORMAT", "REAL", "OPEN_ACT", "SA", NULL);
      file_model->setFileOpen(TRUE);



      ////////////////////////////////////////////////////////////////////////////
      // Extract relevant information about tiling parameters from the image label
      ////////////////////////////////////////////////////////////////////////////


      // Pixel averaging in X
      pixelAvgWidth = 1; //default
      status = zlget(unit, "PROPERTY", "PIXEL_AVERAGING_WIDTH", &pixelAvgWidth,
                     "FORMAT", "INT", "ERR_ACT", "", "PROPERTY", 
                     "INSTRUMENT_STATE_PARMS", NULL);
      if (status <= 0) 
         throw status;


      // Pixel averaging in Y
      pixelAvgHeight = 1; //default
      status = zlget(unit, "PROPERTY", "PIXEL_AVERAGING_HEIGHT", &pixelAvgHeight,
                     "FORMAT", "INT", "ERR_ACT", "", "PROPERTY", 
                     "INSTRUMENT_STATE_PARMS", NULL);
      if (status <= 0) 
         throw status;

   
      // Downsampling factor in X of each tiles
      downsamplingX[20]; //max should be 16 per M2020 Engineering cam spec
      nret = 0; // number of returned elements
      status = zlget(unit, "PROPERTY", "TILE_DOWNSAMPLE_X", downsamplingX,
                     "FORMAT", "INT", "ERR_ACT", "", "NELEMENT", -1, "NRET", &nret,
                     "PROPERTY", "INSTRUMENT_STATE_PARMS", NULL);
      if (status <= 0) 
         throw status;

      if (nret == 0) {
         zvmessage("TILE_DOWNSAMPLE_X label parameter returned 0 elements","");
         throw status;
      }

   
      // Downsampling factor in X of each tiles
      downsamplingY[20]; //max should be 16 per M2020 Engineering cam spec
      nret2 = 0; // number of returned elements
      status = zlget(unit, "PROPERTY", "TILE_DOWNSAMPLE_Y", downsamplingY,
                     "FORMAT", "INT", "ERR_ACT", "", "NELEMENT", -1, "NRET", &nret2,
                     "PROPERTY", "INSTRUMENT_STATE_PARMS", NULL);
      if (status <= 0) 
         throw status;

      if (nret != nret2) 
         sameNbElements = 0;



      // Starting position of each tile in X direction
      startX[20]; //max should be 16 per M2020 Engineering cam spec
      status = zlget(unit, "PROPERTY", "TILE_FIRST_LINE_SAMPLE", startX,
                     "FORMAT", "INT", "ERR_ACT", "", "NELEMENT", -1, "NRET", &nret2,
                     "PROPERTY", "INSTRUMENT_STATE_PARMS", NULL);
      if (status <= 0) 
         throw status;

      if (nret != nret2) 
         sameNbElements = 0;



      // Starting position of each tile in Y direction
      startY[20]; //max should be 16 per M2020 Engineering cam spec
      status = zlget(unit, "PROPERTY", "TILE_FIRST_LINE", startY,
                     "FORMAT", "INT", "ERR_ACT", "", "NELEMENT", -1, "NRET", &nret2,
                     "PROPERTY", "INSTRUMENT_STATE_PARMS", NULL);
      if (status <= 0) 
         throw status;

      if (nret != nret2) 
         sameNbElements = 0;



      // Number of samples of each tile
      nbX[20]; //max should be 16 per M2020 Engineering cam spec
      status = zlget(unit, "PROPERTY", "TILE_NUM_LINE_SAMPLES", nbX,
                     "FORMAT", "INT", "ERR_ACT", "", "NELEMENT", -1, "NRET", &nret2,
                     "PROPERTY", "INSTRUMENT_STATE_PARMS", NULL);
      if (status <= 0) 
         throw status;

      if (nret != nret2) 
         sameNbElements = 0;



      // Number of lines of each tile
      nbY[20]; //max should be 16 per M2020 Engineering cam spec
      status = zlget(unit, "PROPERTY", "TILE_NUM_LINES", nbY,
                     "FORMAT", "INT", "ERR_ACT", "", "NELEMENT", -1, "NRET", &nret2,
                     "PROPERTY", "INSTRUMENT_STATE_PARMS", NULL);
      if (status <= 0) 
         throw status;

      if (nret != nret2) 
         sameNbElements = 0;


      // If there is a discrepency between the number of tiles elements read from
      // the file, notify, and exit
      if (!sameNbElements) {
         zvmessage("Different number of elements read for TILE_DOWNSAMPLE_X","");
         zvmessage("or TILE_DOWNSAMPLE_Y or TILE_FIRST_LINE or TILE_FIRST_LINE_SAMPLE","");
         zvmessage("or TILE_NUM_LINES or TILE_NUM_LINE_SAMPLES","");
         throw 0;
      }
   
   } // end of try clause
   catch (int err) {
      // Close file
      // Close the file as it is not needed anymore
      file_model->closeFile();

      // Not sure if we need to zvclose the unit here:
      zvclose(unit, "CLOS_ACT","FREE", NULL);

      return 0;
   }

   ////////////////////////////////////////////////////////////////////////////
   // Construct tile map for the input image
   ////////////////////////////////////////////////////////////////////////////


   // Tiles of different downsampling factor can overlap. For instance, a x4
   // downsampling tile can cover the whole image, while other x2 tiles cover 
   // only part of it. x2 has precedence over x4 (we keep the highest 
   // resolution). The output will be x4 where there is no coverage with x2 
   // tiles, and x2 otherwise.
   // To achieve this, we fill the output downsampling factor image starting 
   // with the coarser tiles, down to the higher resolution ones.

   // Get the smaller average pixel size. Most of the time it will be the same.
   // This is to keep the highest resolution.
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


   // The image might be a crop of the full frame. In that case we need to 
   // identify the line/sample of the most top-left tile used in this crop w.r.t
   // full frame. This will provide the general line/sample offset to apply when
   // filling the tile map
   int minX = startX[0];
   for (int i=0; i<nret; i++) 
      if (startX[i] < minX) minX = startX[i];

   int minY = startY[0];
   for (int i=0; i<nret; i++) 
      if (startY[i] < minY) minY = startY[i];


   // Tiles origin and number of lines are expressed in the full 
   // resolution frame (raw frame). Scale the values with the average 
   // downsampling and offset with crop origin
   for (int i=0; i<nret; i++) startX[i] = (startX[i] - minX) / pixelAvgWidth;
   for (int i=0; i<nret; i++) nbX[i] /= pixelAvgWidth;
   for (int i=0; i<nret; i++) startY[i] = (startY[i] - minY) / pixelAvgHeight;
   for (int i=0; i<nret; i++) nbY[i] /= pixelAvgHeight;


   // Go over each tile and fill in the output
   for (const auto &p: dsVect) {
    
      // Get position in label output array and downsampling factor 
      int pos = p.first; 
      // Compute the "relative" downsampling factor w.r.t. to the averaging 
      // resolution, (as opposed to w.r.t. the nominal (full) resolution, which
      // is the one given in the label
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


   // Close file
   // Close the file as it is not needed anymore
   file_model->closeFile();

   // Not sure if we need to zvclose the unit here:
   zvclose(unit, "CLOS_ACT","FREE", NULL);


   return 1;
}



// Mask function
// Generate a mask image that identify pixel in the left image that need to be
// excluded from the disparity maps. These pixel disparity map values will be
// set to 0 after the processing is done (just before writting file).
// The mask identify pixels whose DN values are to be filtered out, along with 
// a border around these pixels. All based on User input.
template<class T>
SimpleImage<int> * getMask(SimpleImage<T> *&img, std::vector<double> &maskDN,
                           int maskExtentX, int maskExtentY) {

   // Get input image dimensions
   int ns = img->getNS();
   int nl = img->getNL();

   // Instanciate the mask
   SimpleImage<int> * mask = new SimpleImage<int>(nl, ns);
   mask->zero();

   // Update mask for pixel whose DN in the input image are in the list of 
   // masked values
   for (int j=0; j<nl; j++) {
      for (int i=0; i<ns; i++) {
         for (int k=0; k<maskDN.size(); k++) {
            // if mask value < 0 it indicates a range from previous value to abs(current value)
            if ( ( maskDN[k] < 0 && img->get(j,i) > maskDN[k-1] && img->get(j, i) <= -maskDN[k]) ||
                 (img->get(j, i) == maskDN[k]) ) {
               int xs = i - maskExtentX;
               int xe = i + maskExtentX;
               int ys = j - maskExtentY;
               int ye = j + maskExtentY;

               if (xs < 0) xs = 0;
               if (xe >= ns) xe = ns-1;
               if (ys < 0) ys = 0;
               if (ye >= nl) ye = nl-1;

               for (int l=ys; l<=ye; l++)
                  for (int s=xs; s<=xe; s++)
                     mask->set(l, s, 1);
            }
         }
      }
   }

   return mask;

}


