////////////////////////////////////////////////////////////////////////
// marsautotie2 - multimission tiepoint gathering program: automated
// Based on keypoints detection, description and matching
////////////////////////////////////////////////////////////////////////

#include "vicmain_c"

#include "mars_support.h"
#include "mars_tiepoints.h"

#include "PigMission.h"
#include "PigFileModel.h"
#include "PigCameraModel.h"
#include "PigPointingModel.h"
#include "PigSurfaceModel.h"
#include "PigVector.h"
#include "PigCoordSystem.h"

#include "SimpleImage.h"

#include <string.h>

#ifdef _OPENMP
#include <omp.h>
#endif

// buffer sizes in main program 
#define MAX_INPUTS 2000
#define MAX_NS MARS_MAX_NS
#define MAX_NL MARS_MAX_NL

#define MAX_THREADS 256

#include <random>
#include <algorithm>
#include "demo_lib_sift.h"
#include "compute_asift_keypoints.h"
#include "compute_asift_matches.h"
#include "gtm_filter.h"

# define IM_X 800   // Size of low-resolution image for fast
# define IM_Y 600   // estimation of optimal Affine parameters



#include <iostream>
using namespace std;


void computeBestRotTiltPairs(int nids, PigFileModel *file_models[], 
                             PigPointingModel *pointing_in[], PigCoordSystem *cs, 
                             int band, vector<pair<float,float> > tiltsRots,
                             vector<pair<affImg, affImg> > &imgsPairList, 
                             int crossCheck, float sep_cos, int nBestMatches,
                             siftPar &siftParameters, int omp_on);

bool sortByMatches(const pair<int,int> &i, const pair<int, int> &j);

void dilate_image(vector<float> &imgIn, vector<float> &imgOut,
                  int nbx, int nby,
                  float maskVal, int dilationFactor);

void generate_tiepoints_asift(TiePoint *&tiepoints, int &n_tiepoints, int nids,
		PigFileModel *file_models[], PigCameraModel *camera_in[],
		PigPointingModel *pointing_in[], int band, PigCoordSystem *cs, 
                vector<pair<int,int> > &imgIndicesPairList,  
                vector<pair<float, float> > &tiltsRots, int maxDistEpiline, 
                int maxDistPos, float matchRatio, int crossCheck, 
                int nBestMatches, int upsample, int mask, float valMask, 
                int dilationFactor, float sep_cos, int maxTies, int gtm, 
                int omp_on, siftPar &p);




////////////////////////////////////////////////////////////////////////
// Marsautotie2 program
////////////////////////////////////////////////////////////////////////

void main44()
{

    int i, j, k, status, omp_on;
    int count, def;
    const size_t msgLen = 150;
    char msg[msgLen];

    // Inputs

    int nids;
    PigFileModel *file_models[MAX_INPUTS];
    PigCameraModel *camera_in[MAX_INPUTS];
    PigPointingModel *pointing_in[MAX_INPUTS];
    PigSurfaceModel *surface_model;
    int homogeneous_inputs = TRUE;
    PigCoordSystem *cs, *dummy_cs;

    // Tiepoints

    struct TiePoint *tiepoints;
    int n_tiepoints;

    // User Parameters

    char outfilename[150];
    int band, numTilts, crossCheck, nBestMatches,
        pairing;
    int octave, scales, upsample;
    int mask, dilationFactor;
    int gtm, maxTies;
    float matchRatio, epiMaxDist, posMaxDist, initSigma, dogThreshold,
          edgeThreshold, border, valMask, sep_angle, sep_cos;
    char mission[64], instrument[64];

    zvmessage("MARSAUTOTIE2 version 1", "");

    mars_setup(nids, file_models, camera_in, pointing_in, NULL, dummy_cs,
                mission, instrument, homogeneous_inputs,
                MAX_NL, MAX_NS, MAX_INPUTS);



    // get output parameter file name
    zvp("OUT", outfilename, &count);

    // get which image band to use. It will be the same band for all the images.
    zvp("BAND", &band, &count);

    // Get matching sequencing strategy
    zvp("PAIR_MATCH", &pairing, &count);

    // Get the number of Tilts to simulate. From the number of tilts, a number
    // of affine transformation will be defined
    zvp("NUMTILTS", &numTilts, &count);
    if (numTilts <= 0)
       numTilts = 1;  // i.e., no affine transform

    // Get the maximum distance from the epipolar line to search for matching
    // points.
    zvp("EPIMAXDIST", &epiMaxDist, &count);

    // Get the maximum distance from Left pixel coordinates to seach for 
    // matching points in Right image.
    zvp("POSMAXDIST", &posMaxDist, &count);

    // Get the N best tilts/rots combination to proceed full resolution. If 
    // larger than 0, this value indicates that a first matching run is to be
    // carried on low resolution images, and the N fist tilts/rots combination
    // that gave the most matches are to be processed on full res images. 
    zvp("NBESTMATCHES", &nBestMatches, &count);


    // Do Left/Right Right/Left consistency check?
    crossCheck=0;
    if (zvptst("CROSSCHECK"))
       crossCheck = 1;


    // Is there a masking to apply?
    mask = 0;
    if (zvptst("MASK"))
       mask = 1;
     
    // Retrieve value of masked pixel
    zvp("VALMASK", &valMask, &count);

    // Get mask dilation factor if any
    zvp("DILATEMASK", &dilationFactor, &count);

    sep_cos = 10; //default value for no filtering
    zvp("SEP_ANGLE", &sep_angle, &count);       // max sep angle for frames
    if (count != 0)
       sep_cos = cos(PigDeg2Rad(sep_angle));

    // Limit the number of tiepoints per pair?
    maxTies = 0;
    zvp("MAX_TIES", &maxTies, &count);

    // Apply GTM filtering?
    zvp("GTM", &gtm, &count);
    if (gtm <= 1)
       gtm = 0;  // i.e., no filtering

    // Getting SIFT specific parameters
    siftPar siftparameters; 
    default_sift_parameters(siftparameters);

    // Number of Octave (i.e., number of times the image is dowsampled
    // by a factor of two)
    zvp("OCTAVE", &octave, &count);
    siftparameters.OctaveMax = octave;
  
    // Number of Scale per octave (i.e., number of low-pass filtering
    // is applied to each octave)
    zvp("SCALES", &scales, &count);
    siftparameters.Scales = scales;

    // Get sigma for a Gaussian smoothing (sigma stdev) to be applied to the
    // image at the beginning of each octave.
    zvp("INITSIGMA", &initSigma, &count);
    siftparameters.InitSigma = initSigma;

    // Get DoG contrast threshold below which an extrema detected in the DoG is 
    // discarded as being a potential keypoint
    zvp("DOGTHRESHOLD", &dogThreshold, &count);
    siftparameters.PeakThresh = dogThreshold;

    // Adjust DoG extrema detection threshold according to scales.
    // DoG threshold depends on the number of scales. Input DoG threshold 
    // assumes three scales.
    float factor =  (pow(2.0,1.0/float(siftparameters.Scales)) - 1.0) / 
               (pow(2.0,1.0/3.0) - 1.0); 
    siftparameters.PeakThresh = siftparameters.PeakThresh * factor;

    // Get Edge Threshold. Keypoints on edges are not good keypoints as they
    // are not robustely located along the edge (translation invariant).
    zvp("EDGETHRESHOLD", &edgeThreshold, &count);
    // Transform it to relevant value for SIFT library.
    // Also SIFT library uses 2 thresholds (EdgeThresh and EdgeThresh1) with
    // EdgeThresh1 applied to first scale and EdgeThresh applied to the other
    // scales. Didn't see any difference in results using same threshold
    // for both. 
    edgeThreshold = edgeThreshold/pow(edgeThreshold+1.0,2);
    siftparameters.EdgeThresh = edgeThreshold;
    siftparameters.EdgeThresh1 = edgeThreshold;

    // Get the border (in pixel) around the image that is to be excluded from
    // tiepoints selection - This is to avoid edge effect.
    zvp("BORDER", &border, &count);
    siftparameters.BorderDist = border;

    // Do an initial upsampling (factor of 2) before analysing the image?
    upsample=0;
    if (zvptst("UPSAMPLE"))
       siftparameters.DoubleImSize = 1;

    // Get the maximum "distance" ratio between the first and second best 
    // matches to declare a first match valid. If the ratio between the
    // first and second is too high (i.e., they are very similar), the 
    // discrimination is too weak to confirm a good match  
    zvp("MATCHRATIO", &matchRatio, &count);
    siftparameters.MatchRatio = matchRatio;


    // Create surface model based on the first input.  Looks at user
    // parameters.
    surface_model = PigSurfaceModel::create(file_models[0]);
    cs = surface_model->getCoordSystem();


    // Print out input status from labels
    mars_print_inputs(nids, pointing_in, camera_in, file_models,
			homogeneous_inputs, mission, instrument);


    if (nids == 1) {
	zvmessage("Only one input file; nothing to do!", "");
	return;
    }

    // Multithreading?
    omp_on = zvptst("OMP_ON");




   // Depending on the number of tilts to simulate, compute the list of
   // pairwise combination of tilts/rotations. Each of these pairwise 
   // tilt and rotation combination defines an affine simultation. Each image
   // will be affine-transformed simulated and SIFT will be applied to each 
   // pairwise combination of the affine-transformed images.
   vector<pair<float,float> > tiltsRots;
   double a = sqrt(2.0); // good compromise per paper statement
   double b = 72.0;      // good compromise per paper statement

   for(int tiltIndex=0; tiltIndex<numTilts; tiltIndex++) {
      float tilt = pow(a,tiltIndex);
      if (tilt == 1) 
         tiltsRots.push_back(pair<float,float>(tilt,0.0));
      else {
         int numRot = (int)(180.0 * tilt / b);
         for (int rot = 0; rot < numRot; rot++) {
            float theta = b * rot / tilt;
	tiltsRots.push_back(pair<float,float>(tilt,theta));
         }
      }
   } 


   // Create the container storing all the image pairwise combination depending
   // on the strategy chosen by the user. Store pair of image indices w.r.t. the
   // input list
   vector<pair<int, int> > imgsIndicesPairList;
   int limit;
   if (pairing > 0) {  // sliding overlapping window in the list
      for (int i = 0; i < (nids-1); i++) {
         limit = (i+pairing+1 < nids) ? i+pairing+1: nids;
         for (int j = i+1; j < limit; j++)
            imgsIndicesPairList.push_back(pair<int, int>({i,j}));
      }
   }
   else if (pairing < 0) { // sliding non-overalpping window in the list
      for (int i = 0; i < nids; i += (abs(pairing)+1)) {
         limit = (i+abs(pairing) + 1 > nids) ? nids: i+abs(pairing)+1;
         //for (int j = i+1; j < limit; j++) {
         for (int j = i; j < (limit-1); j++) {
            for (int k = j+1; k < limit; k++) 
               imgsIndicesPairList.push_back(pair<int, int>({j,k}));
               //imgsIndicesPairList.push_back(pair<int, int>({i,j}));
         }
      }
   }
   else {
      for (int i = 0; i < (nids-1); i++) { // full pairwise combination
         for (int j = i+1; j < nids; j++)
            imgsIndicesPairList.push_back(pair<int, int>({i,j}));
      }
   }



   // Further tweaking of the pair list to match with REFIMAGE, IGNORE, and
   // IGNORE_INTRA

   // REFIMAGE. Any image labelled as REFIMAGE won't be matched against another
   // REFIMAGE. Pair of REFIMAGE will be deleted from the list.

   // First, get the list of reference images
   int refImgs[nids];
   for (int i=0; i<nids; i++)
      refImgs[i]=0;
  
   int refUse = 0;
   int refimg_array[MAX_INPUTS];
   status = zvparm("REFIMAGE", refimg_array, &count, &def, MAX_INPUTS, 0);
   if (status == 1 && count > 0) {
      for (i=0; i < count; i++) {
          if (refimg_array[i] == -1){
             continue;		// set nothing in the array
          }
          if (refimg_array[i] > nids || refimg_array[i] < -nids) {
             zvmessage("REFIMAGE value bigger than # of inputs!", "");
             zabend();
          }
          if (refimg_array[i] <= 0) {
             if (i < 1 || refimg_array[i-1] <= 0 ||
		refimg_array[i-1] >= -refimg_array[i]) {
                zvmessage("REFIMAGE value less than 0 and prior ref not correct (neg or > this)", "");
                zabend();
             }
             for (int j = refimg_array[i-1]; j <= -refimg_array[i]; j++) {
                refImgs[j-1] = 1;
                refUse = 1;
             }
          }
          else {
            refImgs[refimg_array[i]-1] = 1;
            refUse = 1;
         }
      }
   }
   
   // Second, scan the pairwise list and remove any pairs for which both images
   // are references
   for (auto it = imgsIndicesPairList.begin(); it != imgsIndicesPairList.end(); ){
      if (refImgs[(*it).first] == 1 && refImgs[(*it).second] == 1)
         it = imgsIndicesPairList.erase(it);
      else 
         ++it;
   }


   // IGNORE_INTRA. Any pair for which both image are not a reference image are 
   // deleted. This keyword forces all comparison to be with respect to one 
   // reference image.
   int ignoreIntra = 0;
   ignoreIntra = zvptst("IGNORE_INTRA");
   if (refUse==1 && ignoreIntra==1) {
      for (auto it = imgsIndicesPairList.begin(); it != imgsIndicesPairList.end(); ){
         if (refImgs[(*it).first] == 0 && refImgs[(*it).second] == 0)
            it = imgsIndicesPairList.erase(it);
         else 
            ++it;
      }
   }


   // IGNORE. Any pair of images to be tiepointed containing an image labelled
   // as IGNORE will be deleted. It's essentially equivalent as if the image
   // were to be removed in the input list (not exactly though as the id of 
   // the image is kept in the output file)
   
   // First get the list of ignored images
   int ignoreImgs[nids];
   for (int i=0; i<nids; i++)
      ignoreImgs[i]=0;
  
   int ignore_array[MAX_INPUTS];
   status = zvparm("IGNORE", ignore_array, &count, &def, MAX_INPUTS, 0);
   if (status == 1 && count > 0) {
      for (i=0; i < count; i++) {
          if (ignore_array[i] == -1)
             continue;			// set nothing in the array
          if (ignore_array[i] > nids || refimg_array[i] < -nids) {
             zvmessage("IGNORE value bigger than # of inputs!", "");
             zabend();
          }
          if (ignore_array[i] <= 0) {
             if (i < 1 || ignore_array[i-1] <= 0 ||
		ignore_array[i-1] >= -ignore_array[i]) {
                zvmessage("IGNORE value less than 0 and prior ref not correct (neg or > this)", "");
                zabend();
             }
             for (int j = ignore_array[i-1]; j <= -ignore_array[i]; j++) {
                ignoreImgs[j-1] = 1;
             }
          }
          else {
            ignoreImgs[ignore_array[i]-1] = 1;
         }
      }
   }

   // Second, scan the pairwise list and remove any pairs for which one of the
   // image is part of the ignore list
   for (auto it = imgsIndicesPairList.begin(); it != imgsIndicesPairList.end(); ){
      if (ignoreImgs[(*it).first] == 1 || ignoreImgs[(*it).second] == 1)
         it = imgsIndicesPairList.erase(it);
      else 
         ++it;
   }


                       
    // Generate the tiepoints!
    generate_tiepoints_asift(tiepoints, n_tiepoints, nids, file_models, 
                             camera_in, pointing_in, band, cs,
                             imgsIndicesPairList,
                             tiltsRots,
                             epiMaxDist,
                             posMaxDist,
                             matchRatio,
                             crossCheck,
                             nBestMatches,
                             upsample,
                             mask,
                             valMask,
                             dilationFactor,
                             sep_cos,
                             maxTies,
                             gtm,
                             omp_on,
                             siftparameters);
   
    

    // Save tiepoints
    int start_key;
    zvp("START_KEY", &start_key, &count);

    if (zvptst("OLD"))
        status = mars_save_tiepoints(outfilename, tiepoints, n_tiepoints);
    else
	status = mars_save_tiepoints(outfilename, tiepoints, n_tiepoints,
		file_models, nids, start_key, cs);
    if (status != 0) {
	snprintf(msg, msgLen, "Error saving tiepoints!! code=%d", status);
	zvmessage(msg, "");
    }


    // Print out tiepoints
    zvmessage("Final tiepoint list (Camera coordinates)", "");
    mars_print_tiepoint_list(tiepoints, n_tiepoints);

    // Determine which pictures are represented (when present[i] is 1)
    int present[MAX_INPUTS];

    for (i=0; i < nids; i++)
	present[i] = FALSE;
    for (i=0; i < n_tiepoints; i++) {
	present[tiepoints[i].left_image] = TRUE;
	present[tiepoints[i].right_image] = TRUE;
    }

    for (i=0; i < nids; i++) {
	if (!present[i]) {
	    snprintf(msg, msgLen, "Input image %d not represented by tiepoints", i+1);
	    zvmessage(msg, "");
	}
    }

}





////////////////////////////////////////////////////////////////////////////////
// Main function running the ASIFT method -  This is the bulk of marsautotie2.
// Get some images to compare as input and outputs a list of tiepoints
////////////////////////////////////////////////////////////////////////////////
void generate_tiepoints_asift(TiePoint *&tiepoints, int &n_tiepoints, int nids,
           PigFileModel *file_models[], PigCameraModel *camera_in[],
           PigPointingModel *pointing_in[], int band, 
           PigCoordSystem *cs, vector<pair<int, int> > &imgsIndicesPairList,
           vector<pair<float,float> > &tiltsRots, int maxDistEpiline, 
           int maxDistPos, float matchRatio, int crossCheck, int nBestMatches, 
           int upsample, int mask, float valMask, int dilationFactor, 
           float sep_cos, int maxTies, int gtm, int omp_on, 
           siftPar &siftparameters)
{
   const size_t msgLen = 150;
   char msg[msgLen];

   int omp_on1, omp_on2;
   int numThreads = 1;
#ifdef _OPENMP
   if (omp_on)
      numThreads = omp_get_max_threads();
#endif

   // If we need to randomly subselect a limited amound of tiepoints
   std::random_device rd;
   std::mt19937 g(rd());

   // If we need low-resolution pass
   float wS = IM_X;
   float hS = IM_Y;
   float areaS = wS * hS;

   // Total number of affine simulations per image
   int nbTiltsRots = tiltsRots.size();

   // Initialize the container for storing keypoints for each image, for
   // each Tilt/Rotation combination
   vector<vector<keypointslist> > imgsKeypoints;        // Container
   imgsKeypoints.resize(nids);                          // Number of images
   for (int i=0; i<nids; i++)                           // Number of tilt/rot
      imgsKeypoints[i].resize(nbTiltsRots);

   // Compute all the pairwise Tilt/Rotation combination between two images.
   // These vectors contain indexes to the above Tilt/Rotation container.
   vector<int> leftTR;  
   vector<int> rightTR;
   for (int i=0; i<nbTiltsRots; i++) {
      for (int j=0; j<nbTiltsRots; j++) {
         leftTR.push_back(i);
         rightTR.push_back(j);
      }
   }

   // Create the container storing all the tilt/rot pair combinations for all
   // image pairing.
   vector<pair<affImg,affImg> > imgsPairList;
   for (int i=0; i<imgsIndicesPairList.size(); i++) {
      imgsPairList.push_back(
         pair<affImg,affImg>({imgsIndicesPairList[i].first,leftTR},
                             {imgsIndicesPairList[i].second,rightTR}));
   }


   // Display some information on the process to come.

   // Number of input images
   snprintf(msg, msgLen, "%d input images.", nids);
   zvmessage(msg,"");

   // Number of image pairs to be tiepointed
   snprintf(msg, msgLen, "%lu image pair(s) to be tiepointed.", imgsPairList.size());
   zvmessage(msg,"");

   // Number of affine simulation per image
   if (nbTiltsRots == 1) {
      snprintf(msg, msgLen, "No affine simulations - standard SIFT");
      zvmessage(msg,"");
   }
   else {
      snprintf(msg, msgLen, "%d affine simulation(s) per image.", nbTiltsRots);
      zvmessage(msg,"");

      // Number of comparison for each image pair
      if (nBestMatches > 0)
         snprintf(msg, msgLen, "%lu comparison(s) per pair (low res), %d comparison(s) per pair (full res).", 
              leftTR.size(), nBestMatches*nBestMatches);
      else
         snprintf(msg, msgLen, "%lu comparison(s) per pair.", leftTR.size());
      zvmessage(msg,"");
   }



   // Depending if the user asked for a first run at low resolution to keep the
   // best tilt/rot combinations to run at full resolution, we'll have a 1- or 
   // 2-loop process. Either 1-loop at full resolution, using all rot/tilt 
   // combinations or a 2-loop process with the first loop at reduced resolution
   // on all rot/tilt and the second loop at full resolution on the best N
   // tilt/rot combinations obtained in the first loop (imgsPairList is updated
   // accordingly).
   int nbIter = (nBestMatches > 0) ? 2 : 1;
   int lowResPass;

   // Some containers are needed to store information between the 2 loops
   std::vector<float> zoomArr(nids); // Store low-res factor for each image
   std::vector<std::pair<int,int> > imgSzArr(nids); // Images size
  
   // Container for the final tiepoints
   // Only used in the last loop
   vector<vector<TiePoint> > tieVect;



for (int iter = 0; iter < nbIter; iter++) {

   // Set flag about low-resolution pass
   if (iter == 0 && nbIter == 2)
      lowResPass = 1;
   else
      lowResPass = 0;

   // Print loop information
   if (lowResPass)
      zvmessage("******* Low resolution processing (1/2) *******","");
   else {
      if (nbIter == 2)
         zvmessage("******* Full resolution processing (2/2) *******","");
      else
         zvmessage("******* Full resolution processing (1/1) *******","");
   }

   // Reset initial image zoom to 1
   zoomArr.resize(nids, 1.0);

   // For the low resolution pass, disable the upsampling step.
   if (lowResPass) {
      if (upsample == 1)
         siftparameters.DoubleImSize = 0;
   }

   // For each image, identify which of the Tilt/Rotation simulations are 
   // needed. This identifies the affine transforms that are needed for
   // each images.
   // If nBestMatches <= 0 (i.e., NO lowres first pass; i.e. nbIter=1), all the 
   // simulations are needed. Otherwise, only the best Tilt/Rotation pairs 
   // are processed.  
   vector<vector<int> > sublistKeys;
   sublistKeys.resize(nids);
 
   // Get all the needed transforms (will have duplicates)
   int id1,id2;
   for(int i=0; i<imgsPairList.size(); i++) {
      id1 = imgsPairList[i].first.imgId;
      id2 = imgsPairList[i].second.imgId;
      if (imgsPairList[i].first.TRlist.size() != 0)
         sublistKeys[id1].insert(sublistKeys[id1].end(), 
                                 imgsPairList[i].first.TRlist.begin(),
                                 imgsPairList[i].first.TRlist.end());
      if (imgsPairList[i].second.TRlist.size() != 0)
         sublistKeys[id2].insert(sublistKeys[id2].end(), 
                                 imgsPairList[i].second.TRlist.begin(),
                                 imgsPairList[i].second.TRlist.end());
   }


   // Remove duplicates
   for (int i=0; i<nids; i++) {
      std::sort(sublistKeys[i].begin(), sublistKeys[i].end());
      auto last = unique(sublistKeys[i].begin(), sublistKeys[i].end());
      sublistKeys[i].erase(last, sublistKeys[i].end());
   }


   // Depending on the number of images vs the number of Tilt/Rots, the code
   // part to be multithreaded is different. If the number of image is larger
   // than the number of Tilt/Rot per images, it is best to multithread at the
   // image level. Otherwise, best to multithread at the Tilt/Rot level 
   omp_on1 = omp_on2 = 0;
   if (omp_on) {
      if (nids >= nbTiltsRots)
         omp_on1 = 1;
      else if (nids > nBestMatches && iter == 1)
      //else if (nBestMatches > 0 && nids > nBestMatches && iter == 1)
         omp_on1 = 1;
      else
         omp_on2 = 1;
   }


   // Compute the keypoints for all images, for all Tilt/Rotation needed.
   // This is the first main step of the program: keypoints detection and 
   // description
#pragma omp parallel for schedule(dynamic) private (msg) if (omp_on1)
   for (int i=0; i<nids; i++) {

      int ns, nl;
      SimpleImage<float> *imgRaw;  

      vector<int> imgKeys = sublistKeys[i];

      // If no Tilt/Rotation is needed for the current image, move to next one.
      // Note: this should not happen normally. What to do if that does happen?
      // Skip the image? Run all rot/tilts on the full image?
      if (imgKeys.empty()) {
         snprintf(msg, msgLen, "Image %d didn't get any matches on low res simulations or is ignored!",i);
         zvmessage(msg,"");
         continue;
      }
   
// Even if that seems not efficient to multi-thread on file reading with a
// critical section, file close/open/read/close is much faster than the
// keypoint extraction.   
#pragma omp critical
{
      // If current file is open, close it first
      if (file_models[i]->isFileOpen()) {
         file_models[i]->closeFile();
         zvclose(file_models[i]->getUnit(), "CLOS_ACT","FREE", NULL);
      }

      // Open the file for reading
      zvopen(file_models[i]->getUnit(), "OP", "READ", "U_FORMAT", "REAL", 
                                                        "OPEN_ACT", "SA", NULL);
      file_models[i]->setFileOpen(TRUE);

      // Get image size
      ns = file_models[i]->getNS();
      nl = file_models[i]->getNL();
 
      // Store for later access
      imgSzArr[i].first = ns;
      imgSzArr[i].second = nl;

      // Initialize a container to load the image in memory
      imgRaw = new SimpleImage<float>(nl, ns);

      // Load the image in memory
      for (int j=0; j<nl; j++)
         zvread(file_models[i]->getUnit(), imgRaw->linePtr(j), "BAND", band, 
                                                             "LINE", j+1, NULL);

      // Close the file as it is not needed anymore
      file_models[i]->closeFile();
      zvclose(file_models[i]->getUnit(), "CLOS_ACT","FREE", NULL);
}
      
      // Copy the image array to a stl::vector as this is the format expected
      // by the asift implementation. Not optimal but time penalty negligible
      // given the overall processing time
      vector<float> img(imgRaw->linePtr(0), imgRaw->linePtr(0) + ns*nl); 
     
      // Not needed anymore, free mem.
      imgRaw->free();
      delete imgRaw;


      // If low-res loop, downsample image accordingly
      // Note: there is a bit more image copying that could be necessary. No
      // time to correct for that now, but should be done eventually.
      if (lowResPass) {

         // Subsample the image so that it's about 600x800 pixels, keeping the
         // aspect ratio. Do this only if raw image size is larger than ~600x800
         float area = ns * nl;
         float zoom = sqrt(area/areaS);
         if (zoom > 1) {
	    // New downsampled image size	
            int wS = (int) (ns / zoom);
            int hS = (int) (nl / zoom);
            // Anti-aliasing filter
            float sigma_aa = siftparameters.InitSigma * zoom / 2;
            GaussianBlur1D(img,ns,nl,sigma_aa,1);
            GaussianBlur1D(img,ns,nl,sigma_aa,0);
            // Container for downsampled image		
            vector<float> img_zoom(wS*hS);
            imgResample(img, img_zoom, ns, nl, wS, hS);
            img = img_zoom;  // copy 
            ns = wS;        // set new image dimension
            nl = hS;
            imgSzArr[i].first = ns;  // update for later access
            imgSzArr[i].second = nl;

            // Update camera model to account for this downsampling
            if (camera_in[i] != NULL)
               camera_in[i]->scaleCamera(1.0/zoom, 1.0/zoom);   

            // Store zoom to scale back the camera model for the full resolution
            // loop
            zoomArr[i] = zoom;
         }

      } // end downsampling image if needed


      // Normalize image between 1 and 0
      // First find min and max of pixel values
      float scale, offset, minVal, maxVal;
      auto minmax = std::minmax_element(std::begin(img), std::end(img));
      minVal = *minmax.first;
      maxVal = *minmax.second;

      // Second linearly normalize
      if (maxVal != minVal) {
         scale = 1.0 / (maxVal-minVal);
         offset = minVal*scale;
         //std::for_each(img.begin(), img.end(), [](float &pix){pix*scale-offset;});
         for (int j=0; j<ns*nl; j++) 
            img[j] = scale*img[j] - offset;
      }
      else {
         snprintf(msg, msgLen, "WARNING: image %d contains unique pixel value", i); 
         zvmessage(msg,"");
      }



      // Run the ASIFT keypoint and description algorithm on all affine 
      // simulations needed.
      int counter = 0;
#pragma omp parallel for schedule(dynamic) reduction (+:counter) if (omp_on2)
      for(int j=0; j<imgKeys.size(); j++) {
         float tilt = tiltsRots[imgKeys[j]].first;
         float theta = tiltsRots[imgKeys[j]].second;
         counter += compute_asift_keypoints(img, ns, nl, tilt, theta, 
                            imgsKeypoints[i][imgKeys[j]], siftparameters); 
      }


      // Remove keypoints that are on (or close) to masked value.
      if (mask) {
         float valMask2 = valMask;
         if (maxVal != minVal) 
            valMask2 = scale * valMask - offset;
         
         // If a border around the maskVal is required, compute the "dilated"
         // mask footprint.
         if (dilationFactor > 0) {
            // Create output dilated image
            vector<float> imgOut(nl*ns);
            // Dilate image masked area
            dilate_image(img, imgOut, ns, nl, valMask2, dilationFactor/zoomArr[i]);
            //TODO swap/move instead of copy
            // Assign dilated image to img
            img = imgOut;
         }

         // Delete any keypoints that are located on a pixel with maskVal value
         int x, y;
         for (int j=0; j<imgKeys.size(); j++) {
            for (auto it = imgsKeypoints[i][j].begin(); it != imgsKeypoints[i][j].end(); ) {
               // Round current keypoint location
               x = (int) ((*it).x + 0.5);    
               y = (int) ((*it).y + 0.5);    
               if (img[y*ns+x] == valMask2)
                  it = imgsKeypoints[i][j].erase(it);
               else
                  ++it;
            }
         }         
      }  // end mask

      // How many keypoints were detected for that image?
      if (counter == 0) 
         snprintf(msg, msgLen,"WARNING: No keypoints were found for image %d",i);
      else 
         snprintf(msg, msgLen, "%d keypoints found for image %d", counter, i);
#pragma omp critical
{
      zvmessage(msg,"");
}

   } // End of iteration on list of input image for keypoints computations



   // If constraining the matching to satisfy the epipolar constrain given by
   // the image geometry, compute the linearized keypoints location (x,y) 
   PigCameraModel *linearCamera[nids];
   if (maxDistEpiline >= 0) {
      PigPoint origin;
      PigVector lookVector;
      double x, y;

      // Compute linear pinhole model for each camera and project each detected
      // keypoints in the linear model
      for (int i=0; i<nids; i++) {

         // Compute linear mode for the current camera
         linearCamera[i] = camera_in[i]->alignStereoCameras(
                             imgSzArr[i].first, imgSzArr[i].second, 
                             imgSzArr[i].first, imgSzArr[i].second, 
                             imgSzArr[i].first, imgSzArr[i].second, 
                             NULL);

         // For each keypoints detected in the current image, compute the (x,y)
         // coordinates in the linearModel
         for (int j=0; j<nbTiltsRots; j++) {
            for (int k=0; k<imgsKeypoints[i][j].size(); k++) {

               // Compute look direction of current keypointbased on real camera
               // model
               camera_in[i]->LStoLookVector(imgsKeypoints[i][j][k].y, 
                                            imgsKeypoints[i][j][k].x, 
                                            origin, lookVector, cs);
         
               // Define a XYZ point along that line of sight
               PigPoint XYZ = origin + lookVector * 1000.0; //arbitrary
         
               // Compute the (x,y) of the XYZ according to the linear camera 
               // model
               linearCamera[i]->XYZtoLS(XYZ, FALSE, &y, &x, cs); 

               // Update coordinates of the projected  keypoint
               imgsKeypoints[i][j][k].x = x;
               imgsKeypoints[i][j][k].y = y;
            }
         }   
      }
   }


   // Performing keypoints matching between images.
   // For each pair of image, a series of matchings will be performed based on 
   // list of Tilt/Rot simulation for each image.
   // This is the second main step of the program: keypoints matching. 

   // Again here, depending on the input, the multithreading should be either 
   // applied to the outer or inner loop. Nested parallelism did not satisfy
   // our need at the time of writing.
   // The outer loop is within this function, whereas the inner loop is within
   // compute_asift_matches which is called in this function.
   // Completely arbitrary: if the outer loop contains less than 3/4 of thread
   // capacity, the inner loop is multithreaded. Otherwise, the outer loop is
   // multithreaded
   omp_on1 = omp_on2 = 0;
   if (omp_on) {
      if (imgsPairList.size() * 3 / 4 > numThreads)
         omp_on1 = 1;
      else
         omp_on2 = 1;
   }



   // Container for the final tiepoints
   // Used only in the last loop
   tieVect.clear();
   tieVect.resize(imgsPairList.size());

#pragma omp parallel for schedule(dynamic) if (omp_on1) shared(tieVect) 
   for (int i=0; i<imgsPairList.size(); i++) {
      const size_t msg2Len = 150;
      char msg2[msg2Len];

      int imgIdLeft = imgsPairList[i].first.imgId;
      int imgIdRight = imgsPairList[i].second.imgId;
      vector<int> leftTR = imgsPairList[i].first.TRlist;
      vector<int> rightTR = imgsPairList[i].second.TRlist;
      vector<pair<pointxy,pointxy> > matchings;
      vector<pair<int,int> > matchesScores; // Contains nb matches and index of
                                            // tilt/rot

      // Check that the look directions of the current pair are not too
      // diverging. If they are, skip that pair
      if (sep_cos != 10) {  //10 means no filtering
         if ((pointing_in[imgIdLeft]->getCameraOrientation(cs) %
              pointing_in[imgIdRight]->getCameraOrientation(cs)) 
              < sep_cos) {
                 snprintf(msg2,msg2Len,"No match attempt  between image %d and %d - (%d/%lu)", 
                         imgIdLeft, imgIdRight, i, imgsPairList.size());
                 zvmessage(msg2,"");
                 continue;
         }
      }

      int num = compute_asift_matches(imgsKeypoints[imgIdLeft],
                                      imgsKeypoints[imgIdRight],
                                      leftTR,
                                      rightTR,
                                      siftparameters,
                                      crossCheck,
                                      matchings,
                                      &matchesScores,
                                      linearCamera[imgIdLeft],
                                      linearCamera[imgIdRight],
                                      cs,
                                      imgSzArr[imgIdLeft].first, 
                                      imgSzArr[imgIdLeft].second, 
                                      imgSzArr[imgIdRight].first, 
                                      imgSzArr[imgIdRight].second, 
                                      maxDistEpiline,
                                      maxDistPos,
                                      omp_on2);

      snprintf(msg2,msg2Len,"%d tiepoints found between image %d and %d - (%d/%lu)", num,
              imgIdLeft, imgIdRight, i, imgsPairList.size());
      zvmessage(msg2,"");


      // If num=0, this means that no matches were found between the two images.
      // In case we're in the low-resolution loop there are two different 
      // approaches: 
      // - Assume the images don't match at all and remove the pair from the
      // list (approach currently selected).
      // - Assume that the low resolution is the culprit. Give that pair a 
      // chance by either keeping the full tilt/rot list to be run on the full 
      // image size, or randomly select nBestMatches tilt/rot pairs. 
      if (num == 0) {
         imgsPairList[i].first.TRlist.clear();
         imgsPairList[i].second.TRlist.clear();
         continue;
      }


      // Depending if we're in the low-res loop or the final one, there needs to
      // be different processing. Either search for the best tilt/rot 
      // combination and reiterate, or save and exit

      if (lowResPass) {
         
         // Compute the number of tilts/rot combination to keep. Normally it
         // is given by nBestMatch, but it could be all of them if the total 
         // number of combinations is less than nBestMatch (this would not make
         // sense, but...)
         int maxSimulations = std::min(nBestMatches, (int)leftTR.size());

         // Sort the best matches, so we can keep only the tilt/rot combinations
         // that led to these best matches.
         std::sort(matchesScores.begin(), matchesScores.end(), sortByMatches);
      
         // Temporarily clear the current tilts/rots left and right lists
         imgsPairList[i].first.TRlist.clear();
         imgsPairList[i].second.TRlist.clear();

         // Repopulate with the best tilt/rot combinations
         for (int j=0; j<maxSimulations; j++) {
            imgsPairList[i].first.TRlist.push_back(leftTR[matchesScores[j].second]);
            imgsPairList[i].second.TRlist.push_back(rightTR[matchesScores[j].second]);
         }
      }
      else {

#pragma omp critical
{
         tieVect[i].resize(num);
}

         for (int k=0; k<num; k++) {
            TiePoint tie;
            tie.type = TIEPOINT_TRADITIONAL;
            tie.left_image = imgIdLeft; 
            tie.right_image = imgIdRight;
            tie.left_sample = (double) matchings[k].first.first;       //x
            tie.left_line = (double) matchings[k].first.second;        //y
            tie.corrected_sample = (double) matchings[k].second.first; //x
            tie.corrected_line = (double) matchings[k].second.second;  //y 
            tie.quality = 1;
            tie.interactive = 0;
            tie.active = 1;
            tie.cs = NULL;
            tieVect[i][k] = tie;
         }

         matchings.clear();



         // Apply GTM filtering if necessary
         // GTM filtering is a graph-based geometric filtering of the tiepoints.
         // Ref: https://www.sciencedirect.com/science/article/pii/S0262885608001078
         // gtm variable contains the graph size (noted K in the paper)
         int prevNb = tieVect[i].size();
         if (gtm != 0) {
            gtm_filter(tieVect[i], gtm); 

#pragma omp critical
{
         snprintf(msg2, msg2Len, "Pair %d: Before GTM: %d    After GTM: %lu tiepoints", i, 
                 prevNb, tieVect[i].size());
         zvmessage(msg2, "");
}      
         }
      } // end else between first and second loop

   } // end of matching loop

   // If ending low-res loop, some resetting is needed
   if (lowResPass) {

      // Scale back the camera models for the upcoming full res loop
      for (int i=0; i<nids; i++) {
         if ((zoomArr[i] > 1) && (camera_in[i] != NULL))
            camera_in[i]->scaleCamera(zoomArr[i], zoomArr[i]); 
      }

      // Re-enable upsampling if needed
      if (upsample == 1)
         siftparameters.DoubleImSize = 1;
 
      // Flush the list of tiepoints
      for (auto &it1:imgsKeypoints){
         it1.clear();
         it1.resize(nbTiltsRots);
      }

   }



} // end of 2-loop iteration between low- and full-resolution process



   // Reformatting the vector of tiepoints into an array of tiepoints
   // and limit the number of ties per pair saved if needed

   if (maxTies > 0) {
      snprintf(msg, msgLen, "Limiting the number of tie points to %d per pair", maxTies);
      zvmessage(msg, "");
   }

   n_tiepoints = 0;
   for(int i=0; i<imgsPairList.size(); i++) {
      // Remove tiepoints if necessary
      if ((maxTies > 0) && (tieVect[i].size() > maxTies)) {
         std::shuffle(tieVect[i].begin(), tieVect[i].end(), g);
         tieVect[i].erase(tieVect[i].begin()+maxTies, tieVect[i].end());
      }
      // Update general count of tiepoints
      n_tiepoints += tieVect[i].size();
   }

   tiepoints = new (std::nothrow) TiePoint[n_tiepoints];
   int index = 0;
   if (tiepoints != NULL) {
      for (int i=0; i < tieVect.size(); i++) {
         for (int j=0; j<tieVect[i].size(); j++) {
            tiepoints[index] = tieVect[i][j];
            index++;
         }
      }
   }
   else {
      zvmessage("Error while allocating tiepoints array memory","");
      zabend();
   }

}
// End of generate_tiepoints_asift.





////////////////////////////////////////////////////////////////////////////////
// Comparison function to sort matches returning the largest number of matches
///////////////////////////////////////////////////////////////////////////////
bool sortByMatches(const pair<int,int> &i, const pair<int, int> &j)
{
   return i.first > j.first;
}



////////////////////////////////////////////////////////////////////////////////
// Dilation image to extend the masked area by a certain number of pixel to 
// account for border effect of the SIFT technique.
// This function is NOT optimized for processing speed
///////////////////////////////////////////////////////////////////////////////
void dilate_image(vector<float> &imgIn, vector<float> &imgOut,
                  int nbx, int nby,
                  float maskVal, int dilationFactor)
{
      
   int xs, xe, ys, ye;
   vector<int> loc;

   // Copy input image to output image for non-Mask pixel and store index of
   // masked pixel
   for (int i=0; i<nby*nbx; i++) {
      if (imgIn[i] == maskVal)
         loc.push_back(i);
      else
         imgOut[i] = imgIn[i];
   }

   // Dilate the mask at the masked pixel locations
   for (int i=0; i<loc.size(); i++) {

      // Account for image edges in x direction
      xs = (loc[i] % nbx) - dilationFactor;
      xe = xs + 2*dilationFactor; // = (i%nbj) + dilationFactor
      if (xs<0) xs=0;
      if (xe>nbx) xe=nbx;
              
      // Account for image edges in y direction
      ys = (loc[i]/nbx) - dilationFactor;
      ye = ys + 2*dilationFactor;
      if (ys<0) ys=0;
      if (ye>nby) ye=nby;
  
      for (int k=ys; k<ye; k++) {
         for (int l=xs; l<xe; l++) {
            imgOut[k*nbx+l] = maskVal;
         }
      }
   }
}




////////////////////////////////////////////////////////////////////////////////
// Function to run the affine SIFT (ASIFT) on reduced images to minimize 
// processing time. Usually a good number of tilt/rot combination do not return 
// tiepoints because these simulations depart too much from the real scene 
// geometry. To avoid loosing time processing these useless combinations, a run
// on reduced images is carried out and the tilt/rot combinations that provided
// the most matches are returned. Full resolution processing is then run on 
// these best promising rot/tilt simulations.
///////////////////////////////////////////////////////////////////////////////
void computeBestRotTiltPairs(int nids, 
                             PigFileModel *file_models[], 
                             PigPointingModel *pointing_in[], 
                             PigCoordSystem *cs, 
                             int band, 
                             vector<pair<float,float> > tiltsRots,
                             vector<pair<affImg, affImg> > &imgsPairList, 
                             int crossCheck,
                             float sep_cos,
                             int nBestMatches,
                             siftPar &siftparameters,
                             int omp_on)
{
   const size_t msgLen = 150;
   char msg[msgLen];
   int omp_on1, omp_on2;

   float wS = IM_X;
   float hS = IM_Y;
   float areaS = wS * hS;
   
   // Initialize the container for storing keypoints for each image, for
   // each Tilt/Rotation combination
   vector<vector<keypointslist> > imgsKeypoints;        // Create container
   imgsKeypoints.resize(nids);                          // Number of image
   for (int i=0; i<nids; i++)                           // Number of tilt/rot
      imgsKeypoints[i].resize(tiltsRots.size());


   // Depending on the number of images vs the number of Tilt/Rots, the code
   // part to be multithreaded is different. If the number of image is larger
   // than the number of Tilt/Rot per images, it is best to multithread at the
   // image level. Otherwise, best to multithread at the Tilt/Rot level 
   omp_on1 = omp_on2 = 0;
   if (omp_on) {
      if (nids >= tiltsRots.size())
         omp_on1 = 1;
      else 
         omp_on2 = 1;
   }


   // Open each file, load image in memory, downsize to an equivalent 600x800
   // pixels, compute keypoints and descriptors, close file and free memory. 
  
#pragma omp parallel for schedule(dynamic) private(msg) if (omp_on1)
   for (int i=0; i<nids; i++) {

      int ns, nl;
      SimpleImage<float> *imgRaw;   
#pragma omp critical
{
      // If current file is open, close it first
      if (file_models[i]->isFileOpen()) {
         file_models[i]->closeFile();
         zvclose(file_models[i]->getUnit(), "CLOS_ACT","FREE", NULL);
      }

      // Open the file for reading
      zvopen(file_models[i]->getUnit(), "OP", "READ", "U_FORMAT", "REAL", 
                                                        "OPEN_ACT", "SA", NULL);
      file_models[i]->setFileOpen(TRUE);

      // Get image size
      ns = file_models[i]->getNS();
      nl = file_models[i]->getNL();

      // Initialize a container to load the image in memory
      imgRaw = new SimpleImage<float>(nl, ns);

      // Load the image in memory
      for (int j=0; j<nl; j++)
         zvread(file_models[i]->getUnit(), imgRaw->linePtr(j), "BAND", band, 
                                                             "LINE", j+1, NULL);

      // Close the file as it is not needed anymore
      file_models[i]->closeFile();
      zvclose(file_models[i]->getUnit(), "CLOS_ACT","FREE", NULL);
}
      
      // Warp the image array to a stl::vector as this is the format expected
      // by the asift implementation
      vector<float> img(imgRaw->linePtr(0), imgRaw->linePtr(0) + ns*nl); 

      imgRaw->free();
      delete imgRaw;


      // Subsample the image so that it's about 600x800 pixels, keeping the
      // aspect ratio

      float area = ns * nl;
      float zoom = sqrt(area/areaS);
		
      int wS = (int) (ns / zoom);
      int hS = (int) (nl / zoom);
		
      vector<float> img_zoom;

      // Subsample image if necessary	
      if (zoom > 1) {
         // Anti-aliasing filter
         float sigma_aa = siftparameters.InitSigma * zoom / 2;
         GaussianBlur1D(img,ns,nl,sigma_aa,1);
         GaussianBlur1D(img,ns,nl,sigma_aa,0);
         // Downsampling the image 
         img_zoom.resize(wS*hS);
         imgResample(img, img_zoom, ns, nl, wS, hS);
      }
      else {
         wS = ns;
         hS = nl;
         zoom = 1;
         img_zoom = img;
         snprintf(msg, msgLen, "Size of image %d is smaller than default 800x600",i);
         zvmessage(msg,"");
      }
			

      // For the low resolution pass, disable the upsampling step.
      int upsample = siftparameters.DoubleImSize;
      if (upsample == 1)
         siftparameters.DoubleImSize = 0;
     

      // Run the ASIFT keypoint and description algorithm.
      int counter = 0;
#pragma omp parallel for schedule(dynamic) reduction (+:counter) private(msg) if (omp_on2)
      for(int j=0; j<tiltsRots.size(); j++) {
         float tilt = tiltsRots[j].first;
         float theta = tiltsRots[j].second;
         counter += compute_asift_keypoints(img_zoom, wS, hS, tilt, theta, 
                            imgsKeypoints[i][j], siftparameters); 
      }

      // Now that keypoints are detected, re-enable the upsampling
      // Upsampling will be applied to the full resolution pass
      if (upsample == 1)
         siftparameters.DoubleImSize = 1;


      // Check that we got at least one keypoint. Normally we should get much
      // more than this
      if (counter == 0) {
         snprintf(msg, msgLen, "WARNING: No keypoints were found for image %d",i);
         zvmessage(msg,"");
      }
      else {
         snprintf(msg, msgLen, "%d keypoints detected in low resolution image %d", 
                      counter, i);
         zvmessage(msg,"");
      }


      // Release memory for the current image - Not needed anymore
      img.clear();
      img_zoom.clear();
   }


   
   // Now that we have all the keypoints dectected and described for all 
   // images and all tilt/rot, run the matching algorithm on  the keypoints.
#pragma omp parallel for schedule(dynamic) private(msg) if (omp_on)
   for( int i=0; i<imgsPairList.size(); i++) {

      // Get the left and right tilts/rots lists    
      // There is a one-to-one correspondance between left image tilt/rot and
      // right image tilt/rot.
      vector<int> leftTR = imgsPairList[i].first.TRlist;
      vector<int> rightTR = imgsPairList[i].second.TRlist;

      // Initiate output containers for storing tiepoints and number of 
      // tiepoints per tilts/rots pair
      vector<pair<pointxy,pointxy> > matchings;
      vector<pair<int,int> > matchesScores; // Contains nb matches and index of
                                            // tilt/rot

      // Check that the look directions of the current pair are not too
      // diverging. If they are, skip that pair
      if (sep_cos != 10) {  //10 means no filtering
         if ((pointing_in[imgsPairList[i].first.imgId]->getCameraOrientation(cs) %
              pointing_in[imgsPairList[i].second.imgId]->getCameraOrientation(cs)) 
              < sep_cos) {

                 snprintf(msg,msgLen,"No match attempt  between image %d and %d - (%d/%lu)", 
                 imgsPairList[i].first.imgId, 
                 imgsPairList[i].second.imgId,
                 i, imgsPairList.size());
                 zvmessage(msg,"");

                 imgsPairList[i].first.TRlist.clear();
                 imgsPairList[i].second.TRlist.clear();
                 continue;
         } 
      }


      // Run matching
      int num = compute_asift_matches(
                                    imgsKeypoints[imgsPairList[i].first.imgId],
                                    imgsKeypoints[imgsPairList[i].second.imgId],
                                    leftTR,
                                    rightTR,
                                    siftparameters,
                                    crossCheck,
                                    matchings,
                                    &matchesScores);
     
      // Display matching results
      snprintf(msg,msgLen,"%d tiepoints found between image %d and %d - (%d/%lu)", num,
              imgsPairList[i].first.imgId, 
              imgsPairList[i].second.imgId,
              i, imgsPairList.size());
      zvmessage(msg,"");

      // TBD: What to do if num=0? If num=0, this means that no matches were
      // found between the two low resolution images. Different approaches are 
      // available:
      // - Assume the images don't match at all and remove the pair from the
      // list (approach currently selected).
      // - Assume that the low resolution is the culprit. Give that pair a 
      // chance by either keeping the full tilt/rot list to be run on the full 
      // image size, or randomly select nBestMatches tilt/rot pairs. 
      if (num == 0) {
         imgsPairList[i].first.TRlist.clear();
         imgsPairList[i].second.TRlist.clear();
         continue;
      }

      // Sort the best matches, so we can keep only the tilt/rot combination
      // that led to these best matches.
      std::sort(matchesScores.begin(), matchesScores.end(), sortByMatches);
      
      // Temporarily clear the current tilts/rots left and right lists
      imgsPairList[i].first.TRlist.clear();
      imgsPairList[i].second.TRlist.clear();

      // Refill the lists with the nBestMatches first most promising tilts/rots 
      // combinations (or less if the number of simualtion is less than
      // nBestMatches).
      int maxSimulations = nBestMatches;
      if (nBestMatches > leftTR.size())
         maxSimulations = leftTR.size();

      for (int j=0; j<maxSimulations; j++) {
         imgsPairList[i].first.TRlist.push_back(
                                              leftTR[matchesScores[j].second]);
         imgsPairList[i].second.TRlist.push_back(
                                              rightTR[matchesScores[j].second]);
      }
                       
   }

}




