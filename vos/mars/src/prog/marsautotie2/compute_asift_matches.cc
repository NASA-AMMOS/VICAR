// The following code is derived from the below copyright holder.
// Steps have been taken to insure an authorized use of the 
// patent involved in this algorithm
// JPL-Caltech - 02/2018
//
// Copyright (c) 2008-2011, Guoshen Yu <yu@cmap.polytechnique.fr>
// Copyright (c) 2008-2011, Jean-Michel Morel <morel@cmla.ens-cachan.fr>
//
// WARNING: 
// This file implements an algorithm possibly linked to the patent
//
// Jean-Michel Morel and Guoshen Yu, Method and device for the invariant 
// affine recognition recognition of shapes (WO/2009/150361), patent pending. 
//
// This file is made available for the exclusive aim of serving as
// scientific tool to verify of the soundness and
// completeness of the algorithm description. Compilation,
// execution and redistribution of this file may violate exclusive
// patents rights in certain countries.
// The situation being different for every country and changing
// over time, it is your responsibility to determine which patent
// rights restrictions apply to you before you compile, use,
// modify, or redistribute this file. A patent lawyer is qualified
// to make this determination.
// If and only if they don't conflict with any patent terms, you
// can benefit from the following license terms attached to this
// file.
//
// This program is provided for scientific and educational only:
// you can use and/or modify it for these purposes, but you are
// not allowed to redistribute this work or derivative works in
// source or executable form. A license must be obtained from the
// patent right holders for any other use.
//
// 
//*------------------------ compute_asift_matches-- -------------------------*/
// Match the ASIFT keypoints. 
// 
// Please report bugs or send comments to Guoshen Yu yu@cmap.polytechnique.fr
// 
// Reference: J.M. Morel and G.Yu, ASIFT: A New Framework for Fully Affine 
//            Invariant Image Comparison, SIAM Journal on Imaging Sciences, 
//            vol. 2, issue 2, pp. 438-469, 2009. 
// Reference: ASIFT online demo (You can try ASIFT with your own images online.)
//			  http://www.ipol.im/pub/algo/my_affine_sift/
/*---------------------------------------------------------------------------*/


#include <assert.h>
#include <stdio.h>
#include <iostream>
#include <stdlib.h>
#include <math.h>

#include "mars_support.h"
#include "mars_tiepoints.h"
#include "PigCameraModel.h"

#include "demo_lib_sift.h"
#include <vector>
#include <valarray>
#include <numeric>
#include <array>
using namespace std;

#include "compute_asift_matches.h"


// Tie points definition. Just a pairing of 2 pixels between L and R images
struct tiePoints{float xL; float yL; float xR; float yR;};

int computeHomography(const std::vector<tiePoints> & ties, 
                      std::array<float, 9> & H);

void applyHomography(const std::array<float,9> &H, 
                     const std::valarray<float> &xarr,
                     const std::valarray<float> &yarr,
                     std::valarray<float> &matX,
                     std::valarray<float> &matY);

// Match the keypoints from the left image to the ones from the right images
void compute_matches(keypointslist & keys1,
                     keypointslist & keys2,
	             matchingslist2& matchings, siftPar &par);

void compute_matches(keypointslist & keys1,
                     keypointslist & keys2,
                     std::vector<int> & subsetKeys1,
                     std::vector<int> & subsetKeys2,
	             matchingslist2& matchings, siftPar &par);


// Main function to match a series of keypoint lists obtained from a number of 
// affine-transformed images
int compute_asift_matches(vector<keypointslist> &keysL,
                          vector<keypointslist> &keysR,
                          vector<int> &listL,
                          vector<int> &listR,
                          siftPar &siftparameters,
                          int crossCheck,
                          vector<pair<pointxy,pointxy> > &matchings,
                          vector<pair<int,int> > *matchesScores,
                          PigCameraModel * leftCamModel,
                          PigCameraModel * rightCamModel,
                          PigCoordSystem * commonCS,
                          int nsl, int nll, 
                          int nsr, int nlr, 
                          double maxDistEpiline,
                          double maxDistPos,
                          int omp_on)
{ 
   const size_t msgLen = 150;
   char msg[msgLen];

   if (listL.size() == 0) {
      snprintf(msg, msgLen, "List of keypoints is empty, nothing to match");
      zvmessage(msg,"");
      return 0;
   }

   if (listL.size() != listR.size()) {
      snprintf(msg, msgLen, "Error: Number of left and right keypoints must be identical");
      zvmessage(msg,"");
      return 0;
   }
   
#pragma omp parallel for schedule(dynamic) if (omp_on)
   for (int i=0; i<listL.size(); i++) {

      // Match the keypoints of left and right images 
      matchingslist2 matchings1;					
      compute_constrained_sift_matches(keysL[listL[i]],
                                       keysR[listR[i]],
	                               matchings1, 
                                       siftparameters,
                                       leftCamModel,
                                       rightCamModel,
                                       commonCS,
                                       nsl, nll,
                                       maxDistEpiline,
                                       maxDistPos);
 

      // If R-L L-R check is needed, inverse left and right and compute
      // the matches. Then filter out non-corresponding matches.
      // matchings1 will be updated with the filtered matches.
      if ((crossCheck == 1) && (matchings1.size() != 0) ) {

         matchingslist2 matchings2;
         compute_constrained_sift_matches(keysR[listR[i]],
                                       keysL[listL[i]],
	                               matchings2, 
                                       siftparameters,
                                       rightCamModel,
                                       leftCamModel,
                                       commonCS,
                                       nsr, nlr,
                                       maxDistEpiline,
                                       maxDistPos);

         crossCheckFilter(matchings1, matchings2);
      }     


#pragma omp critical
{
      // Saving the number of matches per Tilt/Rot combinations. This is useful 
      // to identify the tilt/rot combination that provide the most matches.
      if (matchesScores != NULL)
         matchesScores->push_back(pair<int,int>(matchings1.size(), i));

      // Appending the matches of the current tilt/rot to the overall matches
      // container for the current image pair
      if (matchings1.size() != 0)
         matchings.insert(matchings.end(), matchings1.begin(), matchings1.end());
}
 

   }  

   
   // Now that we're done matching keypoints from all the tilt/rot combinations,
   // we need to clean the matching list for similar entries, one-to-multiple,
   // and multiple-to-one matches. Because of several affine simulations, we
   // can have such duplicates.
   // Note that despite filtering out matches for one-to-multiple and 
   // multiple-to-one could be do at once, we filter them separately, in that
   // order, to avoid removing good matches: multiple-to-one matches appears  
   // more frequent than one-to-multiple. Sometimes some of the points in left 
   // that take part in "multiple-to-one" bad matches have also correct matches.

   if (!matchings.empty()) {
      // If duplicates, remove to keep just one
      unique_match(matchings); 

      // Remove multiple-to-one matches
      clean_match(matchings, 0);

      // Remove one-to-multiple matches
      clean_match(matchings, 1);
   } 

   // Number of matches left after all the filtering...
   return matchings.size();

}



// Matching function that applies any Geometric constraints (epipolar line
// proximity or localization proximity).
void compute_constrained_sift_matches(keypointslist & keysL,
                                      keypointslist & keysR,
	                              matchingslist2 &matchings, 
                                      siftPar & siftparameters,
                                      PigCameraModel * leftCamModel,
                                      PigCameraModel * rightCamModel,
                                      PigCoordSystem * commonCS,
                                      int nsl, int nll,
                                      double maxDistEpiline,
                                      double maxDistPos)
{
   
   double dist, dist2;
   std::vector<int> currentKeyL;
   std::vector<int> subsetKeysR;

   // If geometric constraint based on the epipolar line is required
   if (maxDistEpiline >=0) {
 
      double xn, yn, xf, yf, cline[3], normLine;
      PigPoint origin, nearXYZ, farXYZ;
      PigVector lookVector;

      std::vector<float> xRarr(keysR.size());
      std::vector<float> yRarr(keysR.size());


      // For each keypoint in Left, compute the corresponging epiline in Right
      // and subselect the keypoints in Right that are within maxDistEpiline 
      // pixel from the epiline.
      // The keypoint matching will be done between the current Left keypoint
      // and the selected subgroup of Right keypoints

      // For efficiency purpose, we copy the R keypoints coordinates in a simple
      // container as these data will be accessed a lot in the following loop.
      // Caching optimization
      for (int i=0; i<keysR.size(); i++) {
         xRarr[i] = keysR[i].x;
         yRarr[i] = keysR[i].y;
      }

      for (int i=0; i<keysL.size(); i++) {
        
         currentKeyL.clear();   
         subsetKeysR.clear();

         // Compute look direction of current Left keypoint
         leftCamModel->LStoLookVector(keysL[i].y, keysL[i].x, 
                                      origin, lookVector, commonCS);

         // Compute two XYZ points along that look direction
         nearXYZ = origin + lookVector * 1.0;     // arbitrary
         farXYZ  = origin + lookVector * 1000.0;  // arbitrary
  
         // Project these two points in Right image. The line passing through
         // these two points defines the epiline - Remember camera models are 
         // linear pinhole model at this stage
         rightCamModel->XYZtoLS(nearXYZ, FALSE, &yn, &xn, commonCS);
         rightCamModel->XYZtoLS(farXYZ, FALSE, &yf, &xf, commonCS);
 
         // Compute the line equation in homogeneous coordinates
         // i.e., ptNear x ptFar
         cline[0] = yn - yf;
         cline[1] = xf - xn;
         cline[2] = xn*yf - yn*xf;

         // Identify all the points in Right image that are close to the
         // epiline formed by (xn,yn) and (xf,yf).
         normLine = std::sqrt(cline[0]*cline[0] + cline[1]*cline[1]);

         float cline0 = (float)(cline[0]/normLine);
         float cline1 = (float)(cline[1]/normLine);
         float cline2 = (float)(cline[2]/normLine);

         int N = xRarr.size();
         for (int j=0; j< N; j++) {
            if (fabs(cline0*xRarr[j] + cline1*yRarr[j] + cline2) <= maxDistEpiline)
               subsetKeysR.push_back(j);
         }


         // If less than 2 points in Right are found along the epiline, then 
         // move to next point in Left list - Not enough point to match to 
         // consider the one found reliable.
         if (subsetKeysR.size() < 2) 
            continue;
         

         // Compute the matching between the Left keypoints and the sublist of
         // rigth keypoints that satisfy the epipolar constaint.
         // Successful match is added to matchings container.
         currentKeyL.push_back(i);
         compute_matches(keysL, keysR, currentKeyL, subsetKeysR, 
                              matchings, siftparameters);
      }
   }

   // If geometric constraint based on keypoints position is required:
   else if (maxDistPos > 0) {

      int maxDistPos2 = maxDistPos * maxDistPos;

      // For each keypoint in Left, compute the distance between the keypoint 
      // location (pixel coordinates) and the Right keypoints location and 
      // subselect the keypoints in Right that are within maxDistPos 
      // pixel from the Left keypoint location. This approach is mostly useful
      // for images of the same size and which are approximately registered.  
      // The keypoint matching will be done between the current Left keypoint
      // and the selected subgroup of Right keypoints.

      // First copy R keys coordinates to array for cache efficiency as they'll
      // be accessed a lot
      std::vector<float> xRarr(keysR.size());
      std::vector<float> yRarr(keysR.size());
      for (int i=0; i<keysR.size(); i++) {
         xRarr[i] = keysR[i].x;
         yRarr[i] = keysR[i].y;
      }

      for (int i=0; i<keysL.size(); i++) {
        
         currentKeyL.clear();   
         subsetKeysR.clear();
      
         for (int j=0; j<keysR.size(); j++) {
            dist = (keysL[i].x - xRarr[j]) * (keysL[i].x - xRarr[j]) +  
                   (keysL[i].y - yRarr[j]) * (keysL[i].y - yRarr[j]);  

            if (dist <= maxDistPos2) 
               subsetKeysR.push_back(j);
         }
  
         // If less than 2 points in Right are found within the required 
         // distance, then move to next point in Left list - Not enough point to
         // match to consider the one found reliable.
         if (subsetKeysR.size() < 2) 
            continue;

         // Compute the matching between the Left keypoints and the sublist of
         // rigth keypoints that satisfy the localization constaint.
         // Successful match is added to matchings container.
         currentKeyL.push_back(i);
         compute_matches(keysL, keysR, currentKeyL, subsetKeysR, 
                              matchings, siftparameters);
      }
   }


   else { 
      // No geometric constraint. Match all Left keypoints against all Right
      // keypoints
      compute_matches(keysL, keysR, matchings, siftparameters);
   }
}



// Compute matches between keypoint lists.
void compute_matches(keypointslist & keys1,
                     keypointslist & keys2,
	             matchingslist2& matchings, siftPar &par)
{
   std::vector<int> subset1(keys1.size());
   std::iota(subset1.begin(), subset1.end(), 0);

   std::vector<int> subset2(keys2.size());
   std::iota(subset2.begin(), subset2.end(), 0);

   compute_matches(keys1, keys2, subset1, subset2, matchings, par);
}



// Compute matches between keypoint lists.
void compute_matches(keypointslist & keys1,
                     keypointslist & keys2,
                     std::vector<int> & subsetKeys1,
                     std::vector<int> & subsetKeys2,
	             matchingslist2& matchings, siftPar &par)
{

   int bestIndex2;
   float sqminratio, dsq, dsq1, dsq2, *ik1, *ik2;

   // Get User threshold for maximum matching ratio between best and 
   // second-to-best keypoints
   sqminratio = par.MatchRatio * par.MatchRatio;

   // Compare each keypoints of the first list with...
   for (int index1 : subsetKeys1) {		

      dsq1 = dsq2 = std::numeric_limits<float>::max();
      ik1 = keys1[index1].vec;

      //... each keypoints of the second list
      for (int index2 : subsetKeys2) {

         dsq = 0;
         ik2 = keys2[index2].vec;

      //   for (int i = 0; i < VecLength ; i++) {
      //     //dsq += ABS(ik1[i] - ik2[i]);   //L1-norm
      //      diff = ik1[i] - ik2[i];      //L2-norm
      //      dsq += diff*diff;            //L2-norm
      //   }
         // WARNING: The following optimization assumes a VecLength of 128, 
         // which is the case by default and is unlikely to be changed. But 
         // still, if one change that value -> CRASH.
         // Optimization of the above loop
         for (int i = 0; i < VecLength ; i+=8) {
            dsq += (ik1[i  ] - ik2[i  ]) * (ik1[i  ] - ik2[i  ]) +
                   (ik1[i+1] - ik2[i+1]) * (ik1[i+1] - ik2[i+1]) +
                   (ik1[i+2] - ik2[i+2]) * (ik1[i+2] - ik2[i+2]) +
                   (ik1[i+3] - ik2[i+3]) * (ik1[i+3] - ik2[i+3]) +
                   (ik1[i+4] - ik2[i+4]) * (ik1[i+4] - ik2[i+4]) +
                   (ik1[i+5] - ik2[i+5]) * (ik1[i+5] - ik2[i+5]) +
                   (ik1[i+6] - ik2[i+6]) * (ik1[i+6] - ik2[i+6]) +
                   (ik1[i+7] - ik2[i+7]) * (ik1[i+7] - ik2[i+7]);
         }

         if (dsq < dsq1) {
            dsq2 = dsq1;
            dsq1 = dsq;
            bestIndex2 = index2;
         } 
         else if (dsq < dsq2) 
            dsq2 = dsq;
      }
	
      // If the ratio between the best matching pair and the second-to-best 
      // matching pair is sufficiently low, save the best pair.
      if ((dsq1/dsq2) < sqminratio)
         matchings.push_back(std::pair<pointxy,pointxy>(
                    pointxy(keys1[index1].x_ori, keys1[index1].y_ori),
                    pointxy(keys2[bestIndex2].x_ori, keys2[bestIndex2].y_ori)));
   }
}





// Function to cross check matches obtained between Left-Right and Right-Left
// matching.  A match is deemed valid only if it found in both the LR and RL 
// matches lists.
// matchings1 will be updated according the the crosscheck results.
void crossCheckFilter(matchingslist2 &matchings1, matchingslist2 &matchings2)
{
   int xL, yL, xR, yR;
   int nbPoints = matchings1.size();
   int index[nbPoints];

   for (int i=0; i<nbPoints; i++) {

      xL = (int)(matchings1[i].first.first * 100);  // to get rid of float 
      yL = (int)(matchings1[i].first.second * 100); // precision issue
      xR = (int)( matchings1[i].second.first * 100);
      yR = (int)(matchings1[i].second.second * 100);

      index[i] = 0; // assume point is bad

      for (int j=0; j<matchings2.size(); j++) {
         if ((xL == (int)(matchings2[j].second.first * 100)) && 
             (yL == (int)(matchings2[j].second.second * 100)) &&
             (xR == (int)(matchings2[j].first.first * 100)) && 
             (yR == (int)(matchings2[j].first.second * 100))) {
            index[i] = 1; // good tiepoint
            break; // move to next left-right point
         }
      }
   }

   // Delete non-corresponding matches
   for (int i=nbPoints-1; i >= 0; i--) {
      if (index[i] == 0) 
         matchings1.erase(matchings1.begin() + i);
   }
}





// Remove the repetitive matches that appear in different simulations and retain
// only one
void unique_match(vector<pair<pointxy, pointxy> > & matchings)
{
   int unique;
   float x1i,x2i,y1i,y2i,x1o,x2o,y1o,y2o,d1,d2,threshold=2;

   // If list contains only one element, no need to go further
   if (matchings.size() <= 1)
      return;

   // output vector. Similar to input vector but with duplicates removed
   vector<pair<pointxy,pointxy> > out;
    
   // Add first match to output container
   out.push_back(matchings[0]);

   // Iterate over the matches list, and add them to the output list iff 
   // no similar matches are already in the output list. Two matches are 
   // deemed similar if the distances between the left points and the 
   // right points are less than sqrt(2) pixels.
   for (int i=1; i<matchings.size(); i++) {
      x1i = matchings[i].first.first;
      y1i = matchings[i].first.second;
      x2i = matchings[i].second.first;
      y2i = matchings[i].second.second;

      // Current match is assumed unique
      unique = 1;

      for (int j=0; j<out.size(); j++) {
         x1o = out[j].first.first;
         y1o = out[j].first.second;
         x2o = out[j].second.first;
         y2o = out[j].second.second;

         d1 = (x1i-x1o)*(x1i-x1o) + (y1i-y1o)*(y1i-y1o); // dist left points
         d2 = (x2i-x2o)*(x2i-x2o) + (y2i-y2o)*(y2i-y2o); // dist right points

         if ((d1 <= threshold) && (d2 <= threshold)) {
            unique = 0;
            break;
         }
      }

      if (unique == 1)
         out.push_back(matchings[i]);
   }

   matchings.clear();
   matchings = out;
}            




// Remove the ALL one-to-multiple matches or multiple-to-one matches. 
// oneToMul = 0 --> one-to-multiple matches filter
// oneToMul != 0 --> multiple-to-one matches filter
void clean_match(vector<pair<pointxy,pointxy> > &matchings, int oneToMul) 
{

   float x1i,x2i,y1i,y2i,x1o,x2o,y1o,y2o,d1,d2,threshold1=1,threshold2=4;

   if (matchings.size() < 2)
      return;

   // temporary container to flag matches to remove
   vector<int> unique(matchings.size(), 1);

   // Iterate over the matches list, and for each entry, compare it to all the
   // other ones and see if there is a one-to-multiple (or multiple-to-one) 
   // situation. If yes, then flag current matches to-be-removed. A 
   // one-to-multiple situation is when the two left point are closer than 1 
   // pixel and the two right points are farther than 2 pixels. 
   // A multiple-to-one is the opposite.
   for (int i=0; i<(matchings.size()-1); i++) {
      x1i = matchings[i].first.first;    // Left x
      y1i = matchings[i].first.second;   // Left y
      x2i = matchings[i].second.first;   // Right x
      y2i = matchings[i].second.second;  // Right y

      for (int j=i+1; j<matchings.size(); j++) {
         x1o = matchings[j].first.first;
         y1o = matchings[j].first.second;
         x2o = matchings[j].second.first;
         y2o = matchings[j].second.second;

         d1 = (x1i-x1o)*(x1i-x1o) + (y1i-y1o)*(y1i-y1o);
         d2 = (x2i-x2o)*(x2i-x2o) + (y2i-y2o)*(y2i-y2o);

         if (((oneToMul == 1) && (d1 <= threshold1) && ( d2 > threshold2)) ||
             ((oneToMul != 1) && (d1 > threshold2) && (d2 <= threshold1))) {
            unique[i]=0;
            unique[j]=0;
         }
      }
   }
 
   // Remove flagged matches.
   // Note: Given that there might be a lot of erasing on a large vector,
   // using vector/erase might not be the best option in terms of processing
   // time... Not sure in practice that will be significant
   for (int i=matchings.size()-1; i>=0; i--) {
      if (unique[i] == 0) 
         matchings.erase(matchings.begin() + i);
   }
}            

