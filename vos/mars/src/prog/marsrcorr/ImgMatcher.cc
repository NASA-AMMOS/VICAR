///////////////////////////////////////////////////////////////////////////////
// Image Matching Classes
///////////////////////////////////////////////////////////////////////////////


#include <algorithm>
#include <cmath>
#include <limits>

#include "SimpleImage.h" 
#include "ImgUtilities.h"
#include "ImgMatcher.h"



std::string ImgMatcher::getName() { 
   return std::string("Base Image Matcher");
}


int ImgMatcher::getSearchNumX() {

   // Easiest case
   if (_searchX ==0 && _subpixX == 0)
      return 1; 

   // Number of subpixel positions 
   int subpixX = _subpixX + 1;

   // Subpixel step
   float subpixStepX = 1.0 / static_cast<float>(subpixX);

   // Number of searched positions per pixel. It depends on the search space and
   // subpixel precision
   return (_searchX == 0) ? 2 * std::ceil(0.5/subpixStepX) + 1
                          : 2 * subpixX * _searchX + 1;

}
   
int ImgMatcher::getSearchNumY() {


   // Easiest case
   if (_searchY ==0 && _subpixY == 0)
      return 1; 

   // Number of subpixel positions 
   int subpixY = _subpixY + 1;

   // Subpixel step
   float subpixStepY = 1.0 / static_cast<float>(subpixY);

   // Number of searched positions per pixel. It depends on the search space and
   // subpixel precision
   return (_searchY == 0) ? 2 * std::ceil(0.5/subpixStepY) + 1
                          : 2 * subpixY * _searchY + 1;

}





void ImgMatcher::findWTA(const std::valarray<float> & scores,
                           const float * priorX, const float * priorY,
                           const int ns, const int nl,
                           float *bestScore,
                           float *dispX,
                           float *dispY) {


   float badScore = getWorstScore();
   std::fill(bestScore, bestScore + ns*nl, badScore);
   std::fill(dispX, dispX + ns*nl, 0);
   std::fill(dispY, dispY + ns*nl, 0);

   int numSearchPerPixelX = getSearchNumX();
   int numSearchPerPixelY = getSearchNumY();
   int numSearchPerPixel = numSearchPerPixelX * numSearchPerPixelY; 
   float invSubpixX = 1 / static_cast<float>(_subpixX + 1);   
   float invSubpixY = 1 / static_cast<float>(_subpixY + 1);   

   float offsetX = (numSearchPerPixelX-1)/2 * invSubpixX; 
   float offsetY = (numSearchPerPixelY-1)/2 * invSubpixY; 
   if (priorX == nullptr && _centered == 0) {
      offsetX = 0;
      offsetY = 0;
   }

 
   #pragma omp parallel if (_omp)
   for (long j=0; j<nl; j++) {
      auto startLoc = begin(scores) + j * ns * numSearchPerPixel;
      for (long i=0; i<ns; i++) {
         auto bestLoc = std::min_element(startLoc, startLoc + numSearchPerPixel);
         bestScore[j*ns + i] =  *bestLoc;
         if (*bestLoc == badScore) {
            dispX[j*ns+i] = 0;
            dispY[j*ns+i] = 0;
            startLoc += numSearchPerPixel;
            continue;
         }
         auto distance = std::distance(startLoc, bestLoc);
 
         int samp = (priorX == nullptr) ? i : static_cast<int>(priorX[j*ns+i] + 0.5);
         int line = (priorX == nullptr) ? j : static_cast<int>(priorY[j*ns+i] + 0.5);

         dispX[j*ns+i] = (distance % numSearchPerPixelX) * invSubpixX - offsetX + samp;
         dispY[j*ns+i] = (distance / numSearchPerPixelX) * invSubpixY - offsetY + line;
         
         startLoc += numSearchPerPixel;
      }
   }
}




// Compute the average image values within a Template centered at each 
// pixel locations. The output (pAvg) has the same size as the input image 
// (I). The border corresponding of the template half size around the
// image is not modified.
// Inputs: 
// I: Integral image (summed table) of the image
// ns,nl: number of samples/lines of the input
// Outputs:
// pAvg: array (ns*nl) whose values represent the DN average of a template
// (_corrX * _corrY) centered at each pixel. pAvg needs to be allocated with
// at least ns*nl elements.
void ImgMatcher::computeAvg(const double * I, const int ns, const int nl, 
                              double * pAvg) {

   double invSzCorr = 1.0 / static_cast<double>(_corrX * _corrY);
   int off = _corrX-1; 

   const double *tl = (I - ns - 1);
   const double *tr = (I - ns  + _corrX  - 1);
   const double *bl = (I + (_corrY - 1) * ns - 1);
   const double *br = (I + (_corrY - 1) * ns + _corrX - 1);

   // Position cursor for average. Skip the border. 
   pAvg += (_corrY-1)/2 * ns + (_corrX-1)/2;

   for (int l = 0; l < (nl - _corrY + 1); l++) {

      for (int c = 0; c < (ns - _corrX + 1); c++) {
   
         double sum = *br;
         if (c > 0) sum -= *bl;
         if (l > 0) sum -= *tr;
         if (c > 0  && l > 0) sum += *tl;

         *pAvg = sum * invSzCorr;

         pAvg++; 
         tl++; tr++; bl++; br++;

      }

      tl += off; tr += off; bl += off; br += off;
      pAvg += off; 

   }
   
}




// Computes the Pearson score map between two images WITHOUT any prior. 
// Left and Right images do not have to be the same size. However, the output
// will be sized according to the Left image (same number of samples and lines),
// and the Right image will be cropped (or extended with 0) to the right and
// bottom to match Left image size (+ search space).
// Left and Right images are "tied" to top-left pixel. That is top-left pixel
// of Left image is top-left pixel of first Left template, while top-left pixel
// of Right image is top left pixel of the left-most Right template in the 
// search space
// The ouput score map will contain a "border" of 0, whose "thickness" 
// corresponds to half the template size (no correlation there).
void ImgMatcherNCC::matchAll(const float * left, const float * right, 
                              const int nsl, const int nll, 
                              const int nsr, const int nlr, 
                              std::valarray<float> & scores) {
   
   if (left == nullptr || right == nullptr)
      return;

   // Half correlation template size. Correlation template size is odd.
   int hCorrX = (_corrX - 1) / 2;
   int hCorrY = (_corrY - 1) / 2;

   // Number of subpixel positions.  
   int subpixX = _subpixX + 1;
   int subpixY = _subpixY + 1;

   // Subpixel step, in pixel
   double subpixStepX = 1.0 / static_cast<double>(subpixX);
   double subpixStepY = 1.0 / static_cast<double>(subpixY);

   // Number of searched positions per pixel. It depends on the search space and
   // subpixel precision
   int szSearchX = getSearchNumX();
   int szSearchY = getSearchNumY();
   int szSearch = szSearchX * szSearchY; // 2D total

   // Inverse of the number of pixels in the correlation template. Precomputed 
   // variable.
   float invSzCorr = 1.0 / static_cast<float>(_corrX * _corrY);
  
   // Get relative offset between a correlation pixel location (in Left), i.e., 
   // the center pixel of the template, and the corners of the corresponding 
   // template.
   long tlOffset = -(hCorrY+1) * nsl - (hCorrX+1);
   long trOffset = -(hCorrY+1) * nsl + hCorrX;
   long blOffset =  hCorrY * nsl - (hCorrX+1);
   long brOffset =  hCorrY * nsl + hCorrX;
 

   // Matching score output container initialization.
   // This container can be large, beware!
   // Set values to 1 (null correlation). Default Pearson correlation is 1 for 
   // perfect match, 0 for null match. But, imageMatching class matching score 
   // is opposite: less is better. Pearson score is reversed (1 <--> 0).
   // Note that Pearson score can be negative (-1 being perfect opposite). Not
   // considered here. 
   scores.resize(nsl * nll * static_cast<long>(szSearch), 1);


   // Size of the Right image container. That container is sized according to
   // the Left image size, the search/subpixel parameters, and the "centered"
   // parameters. It will receive the Right image, possibly shifted and/or
   // resampled (subpixel positions).
   // There are two cases:
   // - If search is NOT null, the size of the container is the Left size 
   // augmented with the the search size (only the pixel-wise search space, not 
   // accounting for the subpixel search).
   // - If search is null and subpixel is NOT null, the size of the container is 
   // the Left size augmented with a border of 1 pixel. This is to account for
   // the subpixel shift: For instance (in 1D), if shifting the Right by 0.5 
   // pix positive, the correlation will give the score between the left and 
   // the Right shifted by 0.5 pixel. But we can also correlate the Left and the
   // shifted Right offseted by -1 pixel (easy and cheap), which would give us
   // the correlation between the Left and the Right shifted by -0.5pix. So, 
   // one resampling, 2 shift positions queried.
   //
   // When search=0 and subpixel is not null, the Right image is resampled
   // around the 0 position, between -0.5 and 0.5 (at least). For instance 
   // consider a requested subpixel precision of 0.2 pixels. We need to 
   // correlate the Left with the Right at the -0.6, -0.4, -0.2, 0, 0.2, 0.4, 
   // 0.6 shifts. When Right is shifted by 0.2, we also have access
   // to the -0.8 shift (offset the R by 1 pixel - cheap and easy to do). 
   // However, we're not interested in the -0.8 shift, so we won't correlate at
   // the -0.8 shift. Then we'll resampled the Right with a 0.4 pix shift, which 
   // will give us access to the -0.6 pixel shift too, which we want. So we'll 
   // correlate at two subpixel shift positions -0.6 and 0.4 (with 1 resampling
   // only). Then we'll move to the 0.6 pix shift, which will give us access to 
   // the -0.4 pixel shift, which we need too. And finally, we'll move the 0.8 
   // pix shift, which we won't use, but it will give us access to the -0.2 pix
   // shift which we need.
   // So for the 6 subpixel positions we need (-0.6, -0.4, -0.2, 0.2, 0.4, 0.6)
   // we'll only resample 4 times. 
   // This adds a bit of logic complexity but it saves computing time. 
   int nsr2 = nsl + 2 * ( ((_searchX == 0) && (_subpixX != 0) ) ? 1 : _searchX );
   int nlr2 = nll + 2 * ( ((_searchY == 0) && (_subpixY != 0) ) ? 1 : _searchY );
   // Container for Right image resampled at any subpixel position
   float * right2 = new float[nsr2 * nlr2]();


   // Allocate intermediate container used to compute the cross-correlation
   // score.
   // Container for Left * Right; set it to double for large DN values
   double * LR = new double[nsl * nll]();
   // Container for integral image of Left * Right 
   double * LRI = new double[nsl * nll]();


   // Pre-compute Left image statistics for correlation

   // Integral Image for left and left^2
   double * I = new double[nsl * nll];
   double * I2 = new double[nsl * nll];
   computeIntegralImage<const float>(left, nsl, nll, I, I2);

   // Compute the average and stddev of L corr positions
   double * pAvgL = new double[nsl * nll]();
   double * pStdL = new double[nsl * nll]();
   computeAvgAndStdev(I, I2, nsl, nll, pAvgL, pStdL);

   delete [] I;
   delete [] I2;



   int startLX=-1;
   int startLY=-1;

   // Start of the iteration over subpixel position
   // Because we use Integral Image, it is more efficient to have the outer-most
   // loop on the subpixel positions, to avoid redundant resampling.
   for (int subY = 0; subY < subpixY; subY++) {

      startLX =  -1;

      for (int subX = 0; subX < subpixX; subX++) {

         // This is the logic to define, based on the subpixel position and
         // pixel-wise search how to reasmple the Right and which positions
         // get matched
         int minSearchX, maxSearchX, minSearchY, maxSearchY;
         if (_searchX == 0) {
            //maxSearchX = ((subX * subpixStepX - 0.5) < subpixStepX) ? 0 : -1;  
            //minSearchX = ((1 - subX * subpixStepX - 0.5) < subpixStepX) ? -1 : 0;
            minSearchX = (subpixX - 2 * subX < 2) ? -1 : 0;
            maxSearchX = (2 * subX - subpixX < 2) ?  0 : -1;
            if (subpixX == 1) minSearchX = 0;
            if (minSearchX == -1 && startLX == -1) startLX = subX;
         } 
         else {
            maxSearchX = (subX == 0) ? _searchX : _searchX - 1;  
            minSearchX = -_searchX;  
         }

         if (_searchY == 0) {
            //maxSearchY = ((subY * subpixStepY - 0.5) < subpixStepY) ? 0 : -1;  
            //minSearchY = ((1 - subY * subpixStepY - 0.5) < subpixStepY) ? -1 : 0;
            minSearchY = (subpixY - 2 * subY < 2) ? -1 : 0;
            maxSearchY = (2 * subY - subpixY < 2) ?  0 : -1;
            if (subpixY == 1) minSearchY = 0;
            if (minSearchY == -1 && startLY == -1) startLY = subY;
         } 
         else {
            maxSearchY = (subY == 0) ? _searchY : _searchY - 1;  
            minSearchY = -_searchY;  
         }


        
         // Copy or resample the Right image.
         // If no subpixel position, the Right image is simply copied, according
         // to the "centered" parameter.
         // If subpixel position, the Right image is resampled at a position that
         // depends if pixel-wise search is null or not. If pixel-wise search is
         // null, the resampling location depends if we need 1 or 2 measurements
         // (see comment above)
         std::fill(right2, right2 + nsr2 * nlr2, 0);

         if (subY != 0 || subX != 0) {

            // Resampling mapping matrices
            int offX, offY;
            std::valarray<float> matX(nsr2 * nlr2), matY(nsr2 * nlr2);
            offX = (_searchX==0 || _centered) ? minSearchX : 0;
            offY = (_searchY==0 || _centered) ? minSearchY : 0;

            long index = 0;
            for (float y=0; y<nlr2; y++) {
               for (float x=0; x<nsr2; x++) {
                  matX[index] = x + subX * subpixStepX + offX;
                  matY[index] = y + subY * subpixStepY + offY;
                  index++;
               }
            }

            // Bicubic resampling of the Right image at subpixel location
            resampleImageBicubic<const float>(right, nsr, nlr, matX, matY, right2, _omp);
         }  
         else { 

            // No subpixel position.
            // Copy the Right image (or a subset of it if needed) into the 
            // proper location into the "resampled" right image container
            int off, numX, numY;
            if (_centered) {
               numX = (nsr > nsl) ? nsl : nsr;
               numY = (nlr > nll) ? nll : nlr;
               off  = nsr2 * _searchY + _searchX;
            }
            else {
               numX = (nsr2 > nsr) ? nsr : nsr2;
               numY = (nlr2 > nlr) ? nlr : nlr2;
               off  = 0;
            }

            float *pRight2 = right2 + off;
            for (int y=0; y<numY; y++) {
               std::copy(right + y*nsr, right + y*nsr + numX, pRight2);
               pRight2 += nsr2;
            }

         }



         // Compute Right statistics for correlation

         // Integral Image for right and right^2
         double * I = new double[nsr2 * nlr2];
         double * I2 = new double[nsr2 * nlr2];
         computeIntegralImage<const float>(right2, nsr2, nlr2, I, I2);

         // Compute the average and stddev of R corr positions
         double * pAvgR = new double[nsr2 * nlr2]();
         double * pStdR = new double[nsr2 * nlr2]();
         computeAvgAndStdev(I, I2, nsr2, nlr2, pAvgR, pStdR);


         delete [] I;
         delete [] I2;



         // Iterate over the search space first. Traditionnaly, the opposite it 
         // done, i.e., for each pixel in Left, correlate at each location in 
         // search space. Because we're using integral image, it's best for 
         // performance to do the opposite: for each search space location, 
         // correlate all Left positions. 
         for (int sll = minSearchY; sll <= maxSearchY; sll++) {

            for (int scc = minSearchX; scc <= maxSearchX; scc++) {

               int sl = sll - minSearchY;
               int sc = scc - minSearchX;

               // Compute L*R at the current search location in R
               #pragma omp parallel for if (_omp)
               for (long l = 0; l < nll; l++) {
                  for (long c = 0; c < nsl; c++) {
                     LR[l*nsl + c] = left[l*nsl + c] * right2[(l + sl) * nsr2 + sc + c];
                  }
               } 

               // Compute the Integral Image of L*R
               std::fill(LRI, LRI + nsl * nll, 0);
               computeIntegralImage<const double>(LR, nsl, nll, LRI);


               // The correlation score computed in the most inner-loop (over 
               // pixel x/y location in Left) below, belong to a 2D slice in the
               // correlation score 3D cube. Compute the offset along that 3rd 
               // dimension corresponding to the current searchX/Y and subpixelX/Y.
                
               long offsetX, offsetY;
               if (_searchX == 0)
                  offsetX = (scc == 0) ? (szSearchX-1)/2 + subX : subX - startLX;
               else
                  offsetX = sc * subpixX + subX;
   
               if (_searchY == 0)
                  offsetY = (sll == 0) ? (szSearchY-1)/2 + subY : subY - startLY;
               else
                  offsetY = sl * subpixY + subY;

               long offset = offsetY * szSearchX + offsetX;


               // Iterate over all Left image pixel locations and compute score
               #pragma omp parallel for if (_omp)
               for (long l = hCorrY; l < (nll-hCorrY); l++) {

                  for (long c = hCorrX; c < (nsl-hCorrX); c++) {

                     double avg2 = pAvgL[l*nsl + c] * pAvgR[(sl+l)*nsr2 + sc + c];
                     if (pStdL[l*nsl + c] < _varL || pStdR[(sl+l)*nsr2 + sc + c] < _varR)
                        continue;
                     double std2 = pStdL[l*nsl + c] * pStdR[(sl+l)*nsr2 + sc + c];

                     long loc = l * nsl + c;
                     double prod = LRI[loc + brOffset];
                     if (c > hCorrX) prod -= LRI[loc + blOffset];
                     if (l > hCorrY) prod -= LRI[loc + trOffset];
                     if (c > hCorrX && l > hCorrY) prod += LRI[loc + tlOffset];

                     float score = static_cast<float>((prod * invSzCorr - avg2) / std::sqrt(std2));

                     // Not considering opposite correlation (i.e., -1 < score < 0).
                     //if (score <= 1 && score >= 0){ 
                     if (score >= 0) 
                        // Invert correlation score. Less is better.
                        scores[loc * szSearch + offset] = 1 - score;

                  }  // end iteration on pixel X


               } // end iteration on pixel Y



            } // end iteration on search X  

         } // end iteration on search Y

         delete [] pAvgR;
         delete [] pStdR;

      } // end iteration on subpixel X

   } // end iteration on subpixel Y

   delete [] pAvgL;
   delete [] pStdL;
   delete [] right2;
   delete [] LR;
   delete [] LRI;



} // end function













// Subpixel search match with prior
// Assumption: prior is same size as input left - Border pixel not used
// Output: Scores is same size as input left - Border set to 0
void ImgMatcherNCC::matchAllWithPrior(const float * left, const float * right, 
                                       const float *priorX, const float *priorY,
                                       const int nsl, const int nll, 
                                       const int nsr, const int nlr, 
                                       std::valarray<float> & scores) {
   
   if (left == nullptr || right == nullptr)
      return;

   if (priorX == nullptr || priorY == nullptr)
      return;

   // Half correlation template size
   int hCorrX = (_corrX - 1) / 2;
   int hCorrY = (_corrY - 1) / 2;

   // Number of subpixel positions 
   int subpixX = _subpixX + 1;
   int subpixY = _subpixY + 1;

   // Subpixel step, in pixel
   double subpixStepX = 1.0 / static_cast<double>(subpixX);
   double subpixStepY = 1.0 / static_cast<double>(subpixY);

   // Number of searched positions per pixel. It depends on the search space and
   // subpixel precision
   int szSearchX = getSearchNumX(); // in X direction
   int szSearchY = getSearchNumY(); // in Y direction
   int szSearch = szSearchX * szSearchY; // total

   // Inverse of the number of pixels in the correlation template. Precompute.
   float invSzCorr = 1.0 / static_cast<float>(_corrX * _corrY);

   // Output container initialization - set to 1 (null correlation) as we're
   // computing score= 1 - NCC for the subsequent regularization for which
   // less-is-better 
   scores.resize(nsl * nll * static_cast<long>(szSearch), 1);


   // Analyze the priors.
   // Two things are done:
   // - Flag the priors that point outside or too close to the image borders 
   //   (such that a template around the prior location doesn't fully fit 
   //   within the image).  Note that we're not considering the search space. 
   //   Priors can have a search space that encroaches outside the image 
   //   footprint.
   // - Store the min/max XY of the valid priors. Possibly only a subset of the
   //   Right image would be needed. This will avoid spending time resampling 
   //   the full Right image (for subpixel search) when only a subset is needed.
   int tlX = std::numeric_limits<int>::max();
   int brX = std::numeric_limits<int>::min();
   int tlY = std::numeric_limits<int>::max();
   int brY = std::numeric_limits<int>::min();

   // Flag 0/1 for valid/invalid priors
   char * valid = new char[nsl*nll]();

   // First scan and analyze the X prior.
   const float * p = priorX;
   char * v = valid;
   for (long i = 0; i < nsl*nll; i++) { 
      int val = static_cast<int>(*p + 0.5); // Rounding
      if (val >= hCorrX && val < (nsr-hCorrX)) {
         if (val < tlX) tlX = val;
         if (val > brX) brX = val;
         *v = 1;
      }
      v++; p++;
   } 

   // Second scan and analyze the Y prior. Skip
   // if corresponding X prior has been flagged as
   // invalid.
   p = priorY;
   v = valid;
   for (long i = 0; i < nsl*nll; i++) { 
      if (*v != 0) {
         int val = static_cast<int>(*p + 0.5);
         if (val < hCorrY || val >= (nlr-hCorrY))
            *v = 0;
         else {
            if (val < tlY) tlY = val;
            if (val > brY) brY = val;
         }
      }
      v++; p++;
   } 


   // Recycle the valid min/max prior locations to the top/left bot/right
   // Right2 subset that accounts for the template size
   tlX -= hCorrX;
   tlY -= hCorrY;
   brX += hCorrX;
   brY += hCorrY;
   

   // Size of the Right image subset, defined from accepted priors min/max, 
   // augmented by search size (or with 1 pixel for subpixel adjustment in case
   // search=0 but subpix != 0)
   int marginX = ((_searchX == 0) && (_subpixX != 0) ) ? 1 : _searchX;
   int marginY = ((_searchY == 0) && (_subpixY != 0) ) ? 1 : _searchY;
   int nsr2 = (brX - tlX + 1) + 2 * marginX;
   int nlr2 = (brY - tlY + 1) + 2 * marginY;
   // Container for Right image resampled at any subpixel position
   float * right2 = new float[nsr2 * nlr2]();


   // Pre-compute some statistic for Left image

   // Integral Image for left and left^2
   double * I = new double[nsl * nll];
   double * I2 = new double[nsl * nll];
   computeIntegralImage<const float>(left, nsl, nll, I, I2);

   // Compute the average and stddev of Left templates at each correlation
   // positions
   double * pAvgL = new double[nsl * nll]();
   double * pStdL = new double[nsl * nll]();
   computeAvgAndStdev(I, I2, nsl, nll, pAvgL, pStdL);

   delete [] I;
   delete [] I2;


   int startLX = -1;
   int startLY = -1;

   // Start of the iteration over subpixel position
   // Because we use Integral Image, it is more efficient to have the outer-most
   // loop on the subpixel positions, to avoid redundant resampling.
   for (int subY = 0; subY < subpixY; subY++) {

      startLX = -1;

      for (int subX = 0; subX < subpixX; subX++) {

         // This is the logic to define, based on the subpixel position and
         // pixel-wise search how to resample the Right and which positions
         // get matched
         int minSearchX, maxSearchX, minSearchY, maxSearchY;
         if (_searchX == 0) {
            //maxSearchX = ((subX * subpixStepX - 0.5) < subpixStepX) ? 0 : -1;  
            //minSearchX = ((1 - subX * subpixStepX - 0.5) < subpixStepX) ? -1 : 0;
            minSearchX = (subpixX - 2 * subX < 2) ? -1 : 0;
            maxSearchX = (2 * subX - subpixX < 2) ?  0 : -1;
            if (subpixX == 1) minSearchX = 0;
            if (minSearchX == -1 && startLX == -1) startLX = subX;
         } 
         else {
            maxSearchX = (subX == 0) ? _searchX : _searchX - 1;  
            minSearchX = -_searchX;  
         }

         if (_searchY == 0) {
            //maxSearchY = ((subY * subpixStepY - 0.5) < subpixStepY) ? 0 : -1;  
            //minSearchY = ((1 - subY * subpixStepY - 0.5) < subpixStepY) ? -1 : 0;
            minSearchY = (subpixY - 2 * subY < 2) ? -1 : 0;
            maxSearchY = (2 * subY - subpixY < 2) ?  0 : -1;
            if (subpixY == 1) minSearchY = 0;
            if (minSearchY == -1 && startLY == -1) startLY = subX;
         } 
         else {
            maxSearchY = (subY == 0) ? _searchY : _searchY - 1;  
            minSearchY = -_searchY;  
         }





         // Clear current Right image container
         std::fill(right2, right2 + nsr2 * nlr2, 0);

//TODO Tentative having only one resampling scheme. For integer pixel may be a bit
//less optimal, but simpler code and run only once
        // Compute the resampling locations
        std::valarray<float> matX(nsr2 * nlr2), matY(nsr2 * nlr2);
        long index = 0;
        for (int y = 0; y < nlr2; y++) {
           for (int x = 0; x < nsr2; x++) {
              matX[index] = x + tlX + subX * subpixStepX + minSearchX;
              matY[index] = y + tlY + subY * subpixStepY + minSearchY;
              //matX[index] = x + tlX + subX * subpixStepX - marginX;
              //matY[index] = y + tlY + subY * subpixStepY - marginY;
              index++;
           }
        }
        // Bicubic resampling of the Right image at subpixel location
        resampleImageBicubic<const float>(right, nsr, nlr, matX, matY, right2, _omp);

/*
         if (subY != 0 || subX != 0) {

            // Compute the resampling locations
            std::valarray<float> matX(nsr2 * nlr2), matY(nsr2 * nlr2);
            long index = 0;
            for (int y = 0; y < nlr2; y++) {
               for (int x = 0; x < nsr2; x++) {
                  matX[index] = x + tlX + subX * subpixStepX - marginX;
                  matY[index] = y + tlY + subY * subpixStepY - marginY;
                  index++;
               }
            }

            // Bicubic resampling of the Right image at subpixel location
            resampleImageBicubic<const float>(right, nsr, nlr, matX, matY, right2, _omp);
         }  
         else { 
            // No subpixel position
            // Copy the Right image (or a subset of it if needed) into the 
            // "resampled" right image container
            long off = static_cast<long>(nsr2) * marginY + marginX;
            float *pRight2 = (right2 + off);
            for (int y = tlY; y <= brY; y++) {
               std::copy(right + y*nsr + tlX, right + y*nsr + brX + 1, pRight2);
               pRight2 += nsr2;
            }

         }
*/

         //Compute some statistics for Right image

         // Integral Image for right and right^2
         I = new double[nsr2 * nlr2];
         I2 = new double[nsr2 * nlr2];
         computeIntegralImage<const float>(right2, nsr2, nlr2, I, I2);

         // Compute the average and stddev of R corr positions
         double * pAvgR = new double[nsr2 * nlr2]();
         double * pStdR = new double[nsr2 * nlr2]();
         computeAvgAndStdev(I, I2, nsr2, nlr2, pAvgR, pStdR);

         delete [] I;
         delete [] I2;





         // Iterate over all the pixel.
         // Note, compared to  matchAll function, the loop over the pixel and
         // search space are inversed.
         #pragma omp parallel for if (_omp)
         for (long l = hCorrY; l < (nll-hCorrY); l++) {

            for (long c = hCorrX; c < (nsl-hCorrX); c++) {

               // If prior is invalid, skip this pixel
               if (!valid[l*nsl+c])
                  continue;

               // Left patch average and stddev at current Left pixel
               double avgLc = pAvgL[l*nsl + c];
               double stdLc = pStdL[l*nsl + c];

               // If variance of Left patch too small, skip that pixel
               if (stdLc < _varL) 
                  continue;

               // Get the x/y of the corresponding Right pixel in the resampled
               // right container. Round it to nearest integer. This is the 
               // expected center of the template in the Right  
               int rXi = static_cast<int>(priorX[l*nsl + c] - tlX - minSearchX + 0.5);
               int rYi = static_cast<int>(priorY[l*nsl + c] - tlY - minSearchY + 0.5);

               // Get a pointer to the top/left pixel of the Left Template and 
               // to the top/left pixel of the central Right Template
               const float *itL = (left   + (l   - hCorrY) * nsl  + c   - hCorrX); 
               const float *itR = (right2 + (rYi - hCorrY) * nsr2 + rXi - hCorrX);
                
               for (long sl = minSearchY; sl <= maxSearchY; sl++) {

                  for (long sc = minSearchX; sc <= maxSearchX; sc++) {

                     long locR = (rYi + sl) * nsr2 + rXi + sc;
                     double avg2 = avgLc * pAvgR[locR];
                     double std2 = stdLc * pStdR[locR];

                     if (pStdR[locR] < _varR) 
                        continue;

                     double sumProd = 0;
                     const float *itLc = itL;
                     const float *itRc = itR + sl * nsr2  + sc;
                     for (int y=0; y<_corrY; y++) {
                        for (int x=0; x<_corrX; x++) {
                           sumProd += *itLc++ * *itRc++; 
                        }
                        itLc += (nsl  - _corrX);
                        itRc += (nsr2 - _corrX);
                     }
                     
                     float score = static_cast<float>((sumProd * invSzCorr - avg2) / sqrt(std2));


                     long offsetX, offsetY;
                     if (_searchX == 0)
                        offsetX = (sc == 0) ? (szSearchX-1)/2 + subX : subX - startLX;
                     else
                        offsetX = (sc - minSearchX) * subpixX + subX;
   
                     if (_searchY == 0)
                        offsetY = (sl == 0) ? (szSearchY-1)/2 + subY : subY - startLY;
                     else
                        offsetY = (sl - minSearchY) * subpixY + subY;

                     long offset = offsetY * szSearchX + offsetX;

                     
                     // Not considering opposite correlation (i.e., -1 < score < 0).
                     if (score >= 0) 
                        scores[ (l * nsl + c) * szSearch + offset] = 1 -score;

                  } // end iteration on search X

               } // end iteration on search Y  

            }  // end iteration on loc X

         } // end iteration on loc Y

         delete [] pAvgR;
         delete [] pStdR;

      } // end iteration on subpixel X

   } // end iteration on subpixel Y

   // Housekeeping
   delete [] pAvgL;
   delete [] pStdL;
   delete [] valid;

} // end function






















// Compute the average and standard deviation (the variance actually) of the
// image values within a Template centered at each pixel locations.
// The outputs (pAvg & pStd arrays) have the same size as the input image 
// (I, I2). The border corresponding of the template half size around the
// image is not modified.
// Inputs: 
// I: Integral image (summed table) of the image
// I2: Integral image of the image squared
// ns,nl: number of samples/lines of the inputs
// Outputs:
// pAvg: array (ns*nl) whose values represent the DN average of a template
// (_corrX * _corrY) centered at each pixel
// pStd: array (ns*nl) whose values represent the DN variance of a template
// (_corrX * _corrY) centered at each pixel
void ImgMatcherNCC::computeAvgAndStdev(const double * I, const double * I2, 
                                        const int ns, const int nl, 
                                        double * pAvg, double * pStd) {

   double invSzCorr = 1.0 / static_cast<double>(_corrX * _corrY);
   int off = _corrX-1; 

   const double *tl = (I - ns - 1);
   const double *tr = (I - ns  + _corrX  - 1);
   const double *bl = (I + (_corrY - 1) * ns - 1);
   const double *br = (I + (_corrY - 1) * ns + _corrX - 1);

   const double *tl2 = (I2 - ns - 1);
   const double *tr2 = (I2 - ns + _corrX  - 1);
   const double *bl2 = (I2 + (_corrY - 1) * ns - 1);
   const double *br2 = (I2 + (_corrY - 1) * ns + _corrX - 1);

   // Position cursor for average and stddev. Skip the border. 
   pAvg += (_corrY-1)/2 * ns + (_corrX-1)/2;
   pStd += (_corrY-1)/2 * ns + (_corrX-1)/2;

   for (int l = 0; l < (nl - _corrY + 1); l++) {

      for (int c = 0; c < (ns - _corrX + 1); c++) {
   
         double sum = *br;
         double sum2 = *br2;
         if (c > 0) {
            sum -= *bl;
            sum2 -= *bl2;
         }
         if (l > 0) {
            sum -= *tr;
            sum2 -= *tr2;
         }
         if (c > 0  && l > 0 ) {
            sum += *tl;
            sum2 += *tl2;
         } 

         *pAvg = sum * invSzCorr;
         *pStd = (sum2 - sum * sum * invSzCorr) * invSzCorr;

         pAvg++; 
         pStd++; 

         tl++; tr++; bl++; br++;
         tl2++; tr2++; bl2++; br2++;

      }

      tl += off; tr += off; bl += off; br += off;
      tl2 += off; tr2 += off; bl2 += off; br2 += off;
      pAvg += off; pStd += off;

   }
   
}













// Computes the ZSSD score map between two images WITHOUT any prior. 
// Left and Right images do not have to be the same size. However, the output
// will be sized according to the Left image (same number of samples and lines),
// and the Right image will be cropped (or extended with 0) to the right and
// bottom to match Left image size (+ search space).
// Left and Right images are "tied" to top-left pixel. That is top-left pixel
// of Left image is top-left pixel of first Left template, while top-left pixel
// of Right image is top left pixel of the left-most Right template in the 
// search space
// The ouput score map will contain a "border" of 0, whose "thickness" 
// corresponds to half the template size (no correlation there).
void ImgMatcherZSSD::matchAll(const float * left, const float * right, 
                           const int nsl, const int nll, 
                           const int nsr, const int nlr, 
                           std::valarray<float> & scores) {
   
   if (left == nullptr || right == nullptr)
      return;

   // Half correlation template size. Correlation template size is odd.
   int hCorrX = (_corrX - 1) / 2;
   int hCorrY = (_corrY - 1) / 2;

   // Number of subpixel positions.  
   int subpixX = _subpixX + 1;
   int subpixY = _subpixY + 1;

   // Subpixel step, in pixel
   double subpixStepX = 1.0 / static_cast<double>(subpixX);
   double subpixStepY = 1.0 / static_cast<double>(subpixY);

   // Number of searched positions per pixel. It depends on the search space and
   // subpixel precision
   int szSearchX = getSearchNumX();
   int szSearchY = getSearchNumY();
   int szSearch = szSearchX * szSearchY; // 2D total

   // Inverse of the number of pixels in the correlation template. Precomputed 
   // variable.
   float szCorr = _corrX * _corrY;
   float invSzCorr = 1.0 / static_cast<float>(szCorr);
  
   // Get relative offset between a correlation pixel location (in Left), i.e., 
   // the center pixel of the template, and the corners of the corresponding 
   // template.
   long tlOffset = -(hCorrY+1) * nsl - (hCorrX+1);
   long trOffset = -(hCorrY+1) * nsl + hCorrX;
   long blOffset =  hCorrY * nsl - (hCorrX+1);
   long brOffset =  hCorrY * nsl + hCorrX;
 
   // Matching score output container initialization.
   // This container can be large, beware!
   // Setting default to 0. ZSSD will fill all locations, so initial value does not matter
   scores.resize(nsl * nll * static_cast<long>(szSearch), 0);



   
   // Size of the Right image container. That container is sized according to
   // the Left image size, the search/subpixel parameters, and the "centered"
   // parameters. It will receive the Right image, possibly shifted and/or
   // resampled (subpixel positions).
   // There are two cases:
   // - If search is NOT null, the size of the container is the Left size 
   // augmented with the the search size (only the pixel-wise search space, not 
   // accounting for the subpixel search).
   // - If search is null and subpixel is NOT null, the size of the container is 
   // the Left size augmented with a border of 1 pixel. This is to account for
   // the subpixel shift: For instance (in 1D), if shifting the Right by 0.5 
   // pix positive, the correlation will give the score between the left and 
   // the Right shifted by 0.5 pixel. But we can also correlate the Left and the
   // shifted Right offseted by -1 pixel (easy and cheap), which would give us
   // the correlation between the Left and the Right shifted by -0.5pix. So, 
   // one resampling, 2 shift positions queried.
   //
   // When search=0 and subpixel is not null, the Right image is resampled
   // around the 0 position, between -0.5 and 0.5 (at least). For instance 
   // consider a requested subpixel precision of 0.2 pixels. We need to 
   // correlate the Left with the Right at the -0.6, -0.4, -0.2, 0, 0.2, 0.4, 
   // 0.6 shifts. When Right is shifted by 0.2, we also have access
   // to the -0.8 shift (offset the R by 1 pixel - cheap and easy to do). 
   // However, we're not interested in the -0.8 shift, so we won't correlate at
   // the -0.8 shift. Then we'll resampled the Right with a 0.4 pix shift, which 
   // will give us access to the -0.6 pixel shift too, which we want. So we'll 
   // correlate at two subpixel shift positions -0.6 and 0.4 (with 1 resampling
   // only). Then we'll move to the 0.6 pix shift, which will give us access to 
   // the -0.4 pixel shift, which we need too. And finally, we'll move the 0.8 
   // pix shift, which we won't use, but it will give us access to the -0.2 pix
   // shift which we need.
   // So for the 6 subpixel positions we need (-0.6, -0.4, -0.2, 0.2, 0.4, 0.6)
   // we'll only resample 4 times. 
   // This adds a bit of logic complexity but it saves computing time. 
   int nsr2 = nsl + 2 * ( ((_searchX == 0) && (_subpixX != 0) ) ? 1 : _searchX );
   int nlr2 = nll + 2 * ( ((_searchY == 0) && (_subpixY != 0) ) ? 1 : _searchY );
   // Container for Right image resampled at any subpixel position
   float * right2 = new float[nsr2 * nlr2]();


   // Allocate intermediate container used to compute the zssd score.
   // Container for (Left - Right)^2; set it to double for large DN values
   double * LR = new double[nsl * nll]();
   // Container for integral image of (Left * Right)^2 
   double * LRI = new double[nsl * nll]();


   // Pre-compute Left image statistics for correlation

   // Integral Image of left image
   double * I = new double[nsl * nll];
   computeIntegralImage<const float>(left, nsl, nll, I);

   // Compute the average of the Left image at each pixel positions over the 
   // matching window
   double * pAvgL = new double[nsl * nll]();
   computeAvg(I, nsl, nll, pAvgL);

   delete [] I;

   int startLX=-1;
   int startLY=-1;

   // Start of the iteration over subpixel position
   // Because we use Integral Image, it is more efficient to have the outer-most
   // loop on the subpixel positions, to avoid redundant resampling.
   for (int subY = 0; subY < subpixY; subY++) {

      startLX = -1;

      for (int subX = 0; subX < subpixX; subX++) {


         // This is the logic to define, based on the subpixel position and
         // pixel-wise search how to reasmple the Right and which positions
         // get matched
         int minSearchX, maxSearchX, minSearchY, maxSearchY;
         if (_searchX == 0) {
            //maxSearchX = ((subX * subpixStepX - 0.5) < subpixStepX) ? 0 : -1;  
            //minSearchX = ((1 - subX * subpixStepX - 0.5) < subpixStepX) ? -1 : 0;
            minSearchX = (subpixX - 2 * subX < 2) ? -1 : 0;
            maxSearchX = (2 * subX - subpixX < 2) ?  0 : -1;
            if (subpixX == 1) minSearchX = 0;
            if (minSearchX == -1 && startLX == -1) startLX = subX;
         } 
         else {
            maxSearchX = (subX == 0) ? _searchX : _searchX - 1;  
            minSearchX = -_searchX;  
         }

         if (_searchY == 0) {
            //maxSearchY = ((subY * subpixStepY - 0.5) < subpixStepY) ? 0 : -1;  
            //minSearchY = ((1 - subY * subpixStepY - 0.5) < subpixStepY) ? -1 : 0;
            minSearchY = (subpixY - 2 * subY < 2) ? -1 : 0;
            maxSearchY = (2 * subY - subpixY < 2) ?  0 : -1;
            if (subpixY == 1) minSearchY = 0;
            if (minSearchY == -1 && startLY == -1) startLY = subY;
         } 
         else {
            maxSearchY = (subY == 0) ? _searchY : _searchY - 1;  
            minSearchY = -_searchY;  
         }




         // Clear current Right image container
         std::fill(right2, right2 + nsr2 * nlr2, 0);

         if (subY != 0 || subX != 0) {

            // Resampling mapping matrices
            int offX, offY;
            std::valarray<float> matX(nsr2 * nlr2), matY(nsr2 * nlr2);
            offX = (_searchX==0 || _centered) ? minSearchX : 0;
            offY = (_searchY==0 || _centered) ? minSearchY : 0;

            long index = 0;
            for (float y=0; y<nlr2; y++) {
               for (float x=0; x<nsr2; x++) {
                  matX[index] = x + subX * subpixStepX + offX;
                  matY[index] = y + subY * subpixStepY + offY;
                  index++;
               }
            }

            // Bicubic resampling of the Right image at subpixel location
            resampleImageBicubic<const float>(right, nsr, nlr, matX, matY, right2, _omp);
         }  
         else { 

            // No subpixel position.
            // Copy the Right image (or a subset of it if needed) into the 
            // proper location into the "resampled" right image container
            int off, numX, numY;
            if (_centered) {
               numX = (nsr > nsl) ? nsl : nsr;
               numY = (nlr > nll) ? nll : nlr;
               off  = nsr2 * _searchY + _searchX;
            }
            else {
               numX = (nsr2 > nsr) ? nsr : nsr2;
               numY = (nlr2 > nlr) ? nlr : nlr2;
               off  = 0;
            }

            float *pRight2 = right2 + off;
            for (int y=0; y<numY; y++) {
               std::copy(right + y*nsr, right + y*nsr + numX, pRight2);
               pRight2 += nsr2;
            }

         }




         // Compute Right statistics for correlation

         // Integral Image of Right image
         double * I = new double[nsr2 * nlr2];
         computeIntegralImage<const float>(right2, nsr2, nlr2, I);

         // Compute the average of Right image at each pixel positions
         double * pAvgR = new double[nsr2 * nlr2]();
         computeAvg(I, nsr2, nlr2, pAvgR);

         delete [] I;




         // Iterate over the search space first. Traditionnaly, the opposite it 
         // done, i.e., for each pixel in Left, correlate at each location in 
         // search space. Because we're using integral image, it's best for 
         // performance to do the opposite: for each search space location, 
         // correlate all Left positions. 
         for (int sll = minSearchY; sll <= maxSearchY; sll++) {

            for (int scc = minSearchX; scc <= maxSearchX; scc++) {

               int sl = sll - minSearchY;
               int sc = scc - minSearchX;

               // Compute (L-R)^2 at the current search location in R
               #pragma omp parallel for if (_omp)
               for (long l = 0; l < nll; l++) {
                  for (long c = 0; c < nsl; c++) {
                     LR[l*nsl + c] = std::pow(left[l*nsl + c] - right2[(l + sl) * nsr2 + sc + c], 2);
                  }
               } 

               // Compute the Integral Image of (L-R)^2
               std::fill(LRI, LRI + nsl * nll, 0);
               computeIntegralImage<const double>(LR, nsl, nll, LRI);


               // The correlation score computed in the most inner-loop (over 
               // pixel x/y location in Left) below, belong to a 2D slice in the
               // correlation score 3D cube. Compute the offset along that 3rd 
               // dimension corresponding to the current searchX/Y and subpixelX/Y.
                
               long offsetX, offsetY;
               if (_searchX == 0)
                  offsetX = (scc == 0) ? (szSearchX-1)/2 + subX : subX - startLX;
               else
                  offsetX = sc * subpixX + subX;
   
               if (_searchY == 0)
                  offsetY = (sll == 0) ? (szSearchY-1)/2 + subY : subY - startLY;
               else
                  offsetY = sl * subpixY + subY;

               long offset = offsetY * szSearchX + offsetX;

               // Iterate over all Left image pixel locations and compute score
               #pragma omp parallel for if (_omp)
               for (long l = hCorrY; l < (nll-hCorrY); l++) {

                  for (long c = hCorrX; c < (nsl-hCorrX); c++) {

                     long loc = l * nsl + c;
                     double diff = LRI[loc + brOffset];
                     if (c > hCorrX) diff -= LRI[loc + blOffset];
                     if (l > hCorrY) diff -= LRI[loc + trOffset];
                     if (c > hCorrX && l > hCorrY) diff += LRI[loc + tlOffset];

                     double diffAvg = pAvgL[l*nsl+c] - pAvgR[(l + sl) * nsr2 + sc + c];
                     float score = static_cast<float>(diff - szCorr * diffAvg * diffAvg);

                     scores[loc * szSearch + offset] = score;

                  }  // end iteration on pixel X


               } // end iteration on pixel Y



            } // end iteration on search X  

         } // end iteration on search Y

           delete [] pAvgR;

      } // end iteration on subpixel X

   } // end iteration on subpixel Y

   delete [] pAvgL;
   delete [] right2;
   delete [] LR;
   delete [] LRI;



} // end function










// Subpixel search match with prior
// Assumption: Prior is same size as input left - Border pixel not used
// Output: scores is same size as input left - Border set to 0
void ImgMatcherZSSD::matchAllWithPrior(const float * left, const float * right, 
                                    const float *priorX, const float *priorY,
                                    const int nsl, const int nll, 
                                    const int nsr, const int nlr, 
                                    std::valarray<float> & scores) {
   
   if (left == nullptr || right == nullptr)
      return;

   if (priorX == nullptr || priorY == nullptr)
      return;

   // Half correlation template size
   int hCorrX = (_corrX - 1) / 2;
   int hCorrY = (_corrY - 1) / 2;

   // Number of subpixel positions 
   int subpixX = _subpixX + 1;
   int subpixY = _subpixY + 1;

   // Subpixel step, in pixel
   double subpixStepX = 1.0 / static_cast<double>(subpixX);
   double subpixStepY = 1.0 / static_cast<double>(subpixY);

   // Number of searched positions per pixel. It depends on the search space and
   // subpixel precision
   int szSearchX = getSearchNumX(); // in X direction
   int szSearchY = getSearchNumY(); // in Y direction
   int szSearch = szSearchX * szSearchY; // total

   // Number of pixels in the correlation template. Precompute.
   float szCorr = _corrX * _corrY;

   // Matching score output container initialization.
   // This container can be large, beware!
   // Setting default to 0. ZSSD will fill all locations, so initial value does not matter
   scores.resize(nsl * nll * static_cast<long>(szSearch), 0);


   // Analyze the priors.
   // Two things are done:
   // - Flag the priors that point outside or too close to the image borders 
   //   (such that a template around the prior location doesn't fully fit 
   //   within the image).  Note that we're not considering the search space. 
   //   Priors can have a search space that encroaches outside the image 
   //   footprint.
   // - Store the min/max XY of the valid priors. Possibly only a subset of the
   //   Right image would be needed. This will avoid spending time resampling 
   //   the full Right image (for subpixel search) when only a subset is needed.
   int tlX = std::numeric_limits<int>::max();
   int brX = std::numeric_limits<int>::min();
   int tlY = std::numeric_limits<int>::max();
   int brY = std::numeric_limits<int>::min();

   // Flag 0/1 for valid/invalid priors
   char * valid = new char[nsl*nll]();

   // First scan and analyze the X prior.
   const float * p = priorX;
   char * v = valid;
   for (long i = 0; i < nsl*nll; i++) { 
      int val = static_cast<int>(*p + 0.5); // Rounding
      if (val >= hCorrX && val < (nsr-hCorrX)) {
         if (val < tlX) tlX = val;
         if (val > brX) brX = val;
         *v = 1;
      }
      v++; p++;
   } 

   // Second scan and analyze the Y prior. Skip
   // if corresponding X prior has been flagged as
   // invalid.
   p = priorY;
   v = valid;
   for (long i = 0; i < nsl*nll; i++) { 
      if (*v != 0) {
         int val = static_cast<int>(*p + 0.5);
         if (val < hCorrY || val >= (nlr-hCorrY))
            *v = 0;
         else {
            if (val < tlY) tlY = val;
            if (val > brY) brY = val;
         }
      }
      v++; p++;
   } 


   // Recycle the valid min/max prior locations to the top/left bot/right
   // Right2 subset that accounts for the template size
   tlX -= hCorrX;
   tlY -= hCorrY;
   brX += hCorrX;
   brY += hCorrY;
   

   // Size of the Right image subset, defined from accepted priors min/max, 
   // augmented by search size (or with 1 pixel for subpixel adjustment in case
   // search=0 but subpix != 0)
   int marginX = ((_searchX == 0) && (_subpixX != 0) ) ? 1 : _searchX;
   int marginY = ((_searchY == 0) && (_subpixY != 0) ) ? 1 : _searchY;
   int nsr2 = (brX - tlX + 1) + 2 * marginX;
   int nlr2 = (brY - tlY + 1) + 2 * marginY;
   // Container for Right image resampled at any subpixel position
   float * right2 = new float[nsr2 * nlr2]();


   // Pre-compute some statistic for Left image

   // Integral Image for left image
   double * I = new double[nsl * nll];
   computeIntegralImage<const float>(left, nsl, nll, I);

   // Compute the average of Left templates at each correlation positions using
   // a averaging window same as template.
   double * pAvgL = new double[nsl * nll]();
   computeAvg(I, nsl, nll, pAvgL);

   delete [] I;


   int startLX = -1;
   int startLY = -1;


   // Start of the iteration over subpixel position
   // Because we use Integral Image, it is more efficient to have the outer-most
   // loop on the subpixel positions, to avoid redundant resampling.
   for (int subY = 0; subY < subpixY; subY++) {

      startLX = -1;

      for (int subX = 0; subX < subpixX; subX++) {

         // This is the logic to define, based on the subpixel position and
         // pixel-wise search how to resample the Right and which positions
         // get matched
         int minSearchX, maxSearchX, minSearchY, maxSearchY;
         if (_searchX == 0) {
            //maxSearchX = ((subX * subpixStepX - 0.5) < subpixStepX) ? 0 : -1;  
            //minSearchX = ((1 - subX * subpixStepX - 0.5) < subpixStepX) ? -1 : 0;
            minSearchX = (subpixX - 2 * subX < 2) ? -1 : 0;
            maxSearchX = (2 * subX - subpixX < 2) ?  0 : -1;
            if (subpixX == 1) minSearchX = 0;
            if (minSearchX == -1 && startLX == -1) startLX = subX;
         } 
         else {
            maxSearchX = (subX == 0) ? _searchX : _searchX - 1;  
            minSearchX = -_searchX;  
         }

         if (_searchY == 0) {
            //maxSearchY = ((subY * subpixStepY - 0.5) < subpixStepY) ? 0 : -1;  
            //minSearchY = ((1 - subY * subpixStepY - 0.5) < subpixStepY) ? -1 : 0;
            minSearchY = (subpixY - 2 * subY < 2) ? -1 : 0;
            maxSearchY = (2 * subY - subpixY < 2) ?  0 : -1;
            if (subpixY == 1) minSearchY = 0;
            if (minSearchY == -1 && startLY == -1) startLY = subX;
         } 
         else {
            maxSearchY = (subY == 0) ? _searchY : _searchY - 1;  
            minSearchY = -_searchY;  
         }





         // Clear current Right image container
         std::fill(right2, right2 + nsr2 * nlr2, 0);

//TODO Tentative having only one resampling scheme. For integer pixel may be a bit
//less optimal, but simpler code and run only once
        // Compute the resampling locations
        std::valarray<float> matX(nsr2 * nlr2), matY(nsr2 * nlr2);
        long index = 0;
        for (int y = 0; y < nlr2; y++) {
           for (int x = 0; x < nsr2; x++) {
              matX[index] = x + tlX + subX * subpixStepX + minSearchX;
              matY[index] = y + tlY + subY * subpixStepY + minSearchY;
              //matX[index] = x + tlX + subX * subpixStepX - marginX;
              //matY[index] = y + tlY + subY * subpixStepY - marginY;
              index++;
           }
        }
        // Bicubic resampling of the Right image at subpixel location
        resampleImageBicubic<const float>(right, nsr, nlr, matX, matY, right2, _omp);

/*
         if (subY != 0 || subX != 0) {

            // Compute the resampling locations
            std::valarray<float> matX(nsr2 * nlr2), matY(nsr2 * nlr2);
            long index = 0;
            for (int y = 0; y < nlr2; y++) {
               for (int x = 0; x < nsr2; x++) {
                  matX[index] = x + tlX + subX * subpixStepX - marginX;
                  matY[index] = y + tlY + subY * subpixStepY - marginY;
                  index++;
               }
            }

            // Bicubic resampling of the Right image at subpixel location
            resampleImageBicubic<const float>(right, nsr, nlr, matX, matY, right2, _omp);
         }  
         else { 
            // No subpixel position
            // Copy the Right image (or a subset of it if needed) into the 
            // "resampled" right image container
            long off = static_cast<long>(nsr2) * marginY + marginX;
            float *pRight2 = (right2 + off);
            for (int y = tlY; y <= brY; y++) {
               std::copy(right + y*nsr + tlX, right + y*nsr + brX + 1, pRight2);
               pRight2 += nsr2;
            }

         }
*/

         //Compute some statistics for Right image

         // Integral Image of Right image 
         I = new double[nsr2 * nlr2];
         computeIntegralImage<const float>(right2, nsr2, nlr2, I);

         // Compute the average of R image at each pixel positions
         double * pAvgR = new double[nsr2 * nlr2]();
         computeAvg(I,nsr2, nlr2, pAvgR);

         delete [] I;




         // Iterate over all the pixel.
         // Note, compared to  matchAll function, the loop over the pixel and
         // search space are inversed.
         #pragma omp parallel for if (_omp)
         for (long l = hCorrY; l < (nll-hCorrY); l++) {

            for (long c = hCorrX; c < (nsl-hCorrX); c++) {

               // If prior is invalid, skip this pixel
               if (!valid[l*nsl+c])
                  continue;

               // Get Left DN average
               double avgLc = pAvgL[l * nsl + c];

               // Get the x/y of the corresponding Right pixel in the resampled
               // right container. Round it to nearest integer. This is the 
               // expected center of the template in the Right  
               //int rXi = static_cast<int>(priorX[l*nsl + c] - tlX + marginX + 0.5);
               //int rYi = static_cast<int>(priorY[l*nsl + c] - tlY + marginY + 0.5);
               int rXi = static_cast<int>(priorX[l*nsl + c] - tlX - minSearchX + 0.5);
               int rYi = static_cast<int>(priorY[l*nsl + c] - tlY - minSearchY + 0.5);

               // Get a pointer to the top/left pixel of the Left Template and 
               // to the top/left pixel of the central Right Template
               const float *itL = (left   + (l   - hCorrY) * nsl  + c   - hCorrX); 
               const float *itR = (right2 + (rYi - hCorrY) * nsr2 + rXi - hCorrX);
               
 
               for (long sl = minSearchY; sl <= maxSearchY; sl++) {

                  for (long sc = minSearchX; sc <= maxSearchX; sc++) {

                     double sumDiff = 0;
                     const float *itLc = itL;
                     const float *itRc = itR + sl * nsr2  + sc;
                     for (int y=0; y<_corrY; y++) {
                        for (int x=0; x<_corrX; x++) {
                           double diff = *itLc++ - *itRc++; 
                           sumDiff += diff * diff; 
                        }
                        itLc += (nsl  - _corrX);
                        itRc += (nsr2 - _corrX);
                     }
                     
                     double diffAvg = avgLc - pAvgR[(rYi + sl) * nsr2 + rXi + sc];
                     float score = static_cast<float>(sumDiff - szCorr * diffAvg * diffAvg);

                     long offsetX, offsetY;
                     if (_searchX == 0)
                        offsetX = (sc == 0) ? (szSearchX-1)/2 + subX : subX - startLX;
                     else
                        offsetX = (sc - minSearchX) * subpixX + subX;
   
                     if (_searchY == 0)
                        offsetY = (sl == 0) ? (szSearchY-1)/2 + subY : subY - startLY;
                     else
                        offsetY = (sl - minSearchY) * subpixY + subY;

                     long offset = offsetY * szSearchX + offsetX;

                     
                     scores[(l * nsl + c) * szSearch + offset] = score;

                  } // end iteration on search X

               } // end iteration on search Y  

            }  // end iteration on loc X

         } // end iteration on loc Y

         delete [] pAvgR;

      } // end iteration on subpixel X

   } // end iteration on subpixel Y

   delete [] pAvgL;
   delete [] valid;

} // end function









