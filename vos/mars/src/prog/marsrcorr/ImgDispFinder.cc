///////////////////////////////////////////////////////////////////////////////
// DisparityFinder
//
// This is the base class for regularized disparity finder.
// This class is pure virtual
//
///////////////////////////////////////////////////////////////////////////////

#include <numeric>

#include "ImgDispFinder.h" 


DispFinder::DispFinder() {
   
   // Various pointers to containers set to null
   imgL = nullptr; 
   imgR = nullptr;
   imgMatcher = nullptr;
   dispX = nullptr;
   dispY = nullptr;
   scores = nullptr;

   // Default regularization scheme
   regularization = RegType::mgm;
   numDirections = 8;

   // Default Prior type
   priorType = PriorType::none;

   // Default OMP on
   _omp  = 1;
}


DispFinder::~DispFinder() {
   
   // No release of imgL and imgR as DispFinder does not have ownership 

   if (dispX != nullptr) 
      dispX->free();
   if (dispY != nullptr)
      dispY->free();
   if (scores != nullptr)
      scores->free();
}

PriorType DispFinder::getPriorType() {
   return priorType;
}

std::string DispFinder::doGetType() {
   return std::string("Base Disparity Finder");
}


std::string DispFinder::getType() {
   return doGetType();
}



void DispFinder::setImages(SimpleImage<float> * inImgL, SimpleImage<float> * inImgR) {
   imgL = inImgL; 
   imgR = inImgR;
}



void DispFinder::setImageMatcher(ImgMatcher * inImgMatcher){
   imgMatcher = inImgMatcher;
}


void DispFinder::setRegularization(RegType inRegularization, int inNumDirections) {

   // Set the type of regularization
   regularization = inRegularization;

   // Store number of regularization directions
   numDirections = inNumDirections;
}

RegType DispFinder::getRegularization() {
   return regularization;
}

std::string DispFinder::getRegularizationName() {

   if (regularization == RegType::sgm)
      return "SGM";
   else if (regularization == RegType::mgm)
      return "MGM";
   else
      return "NONE";

}

int DispFinder::getRegularizationNumDirections() {

   return numDirections;

}



int DispFinder::computeMatchScores() {
   return doComputeMatchScores();
}

void DispFinder::clearMatchScores() {
   rawScores.resize(0);
   regScores.resize(0);
}


//void DispFinder::leftRightConsistencyCheck(const DispFinder &right, const float thresholdScalar) {
//   doLeftRightConsistencyCheck(right, thresholdScalar);
//}
SimpleImage<int> DispFinder::leftRightConsistencyCheck(const DispFinder &right, const float thresholdScalar) {
   return doLeftRightConsistencyCheck(right, thresholdScalar);
}

 

SimpleImage<float> * DispFinder::getDisparityX() {

   if (dispX == nullptr)
      return nullptr;

   return new SimpleImage<float>(*dispX, 0, 0, dispX->getNL(), dispX->getNS());

}


SimpleImage<float> * DispFinder::getDisparityY() {

   if (dispY == nullptr)
      return nullptr;

   return new SimpleImage<float>(*dispY, 0, 0, dispY->getNL(), dispY->getNS());

}

SimpleImage<float> * DispFinder::getDisparityScores() {

   if (scores == nullptr)
      return nullptr;

   return new SimpleImage<float>(*scores, 0, 0, scores->getNL(), scores->getNS());

}



void DispFinder::replaceDisparityX(SimpleImage<float> * newDispX) {

   if (newDispX == nullptr || imgL == nullptr || imgR == nullptr)
      return;

   if (newDispX->getNL() == imgL->getNL() && newDispX->getNS() == imgL->getNS()){
      if (dispX != nullptr)
         dispX->free();
      dispX = newDispX;
   }
   else 
      zvmessage("DisparityX replace failed due to incompatible size with Left image","");
}



void DispFinder::replaceDisparityY(SimpleImage<float> * newDispY) {

   if (newDispY == nullptr || imgL == nullptr || imgR == nullptr)
      return;

   if (newDispY->getNL() == imgL->getNL() && newDispY->getNS() == imgL->getNS()){
      if (dispY != nullptr)
         dispY->free();
      dispY = newDispY;
   }
   else 
      zvmessage("DisparityY replace failed due to incompatible size with Left image","");
}


void DispFinder::replaceScores(SimpleImage<float> * newScores) {

   if (newScores == nullptr || imgL == nullptr || imgR == nullptr)
      return;

   if (newScores->getNL() == imgL->getNL() && newScores->getNS() == imgL->getNS()){
      if (scores != nullptr)
         scores->free();
      scores = newScores;
   }
   else 
      zvmessage("Scores replace failed due to incompatible size with Left image","");
}

// do we need empty implementation?
void DispFinder::initializeCorrNeighbor() {

}


void DispFinder::regularize() {

   // If the matching data term cube is empty, nothing to do
   if (!rawScores.size()) {
      zvmessage("Data term is empty, nothing to regularize","");
      return;
   }

   if (regularization == RegType::none) { 
      zvmessage("Regularization strategy set to none. Winner-Takes-All applied to raw matching scores.","");
      findWTA(rawScores);
      return;
   }
   else
      zvmessage("Running regularization","");
  

   // If only SGM, no "above" pixel consideration
   int withAbove = (regularization == RegType::mgm) ? 1 : 0;

   // Get the Left image size
   int ns = imgL->getNS();
   int nl = imgL->getNL();

   // Pre-compute the traversal
   initializeTraversals(ns, nl);

   // This is a function to do any pre-regularization computation if needed.
   // Function should be virtual, with an implementation for base class (doing nothing)
   // Could be redefined by derived class if needed
   // Should be more abstract than initializeCorrNeighbor?
   // like preRegularization()?
   // Pre-compute the neighbors of any pixel in the search space
   initializeCorrNeighbor();

  
   // Run regularization

   // Get number of searches for a given pixel
   // To be defined by derived class
   // Note, this assumes that all pixels will have the same number of searches. 
   long nc  = getSearchSpacePerPixel();

   // Allocate main container of regularized matching score (S in paper). 
   regScores.resize(ns * nl * nc, 0);

   // Temporary container for current traversal
   std::valarray<float> tr(0.f, ns * nl * nc);


   // Loop over each traversal (4 or 8 directions) and update the overall S 
   // score
   for (const auto &traversal : tfull) {

      // Get current traversal offsets to get the "previous" and "above" pixels
      // indices (in 1D)
      int previous_offset = traversal.previous;
      int above_offset    = traversal.above;

      // Loop over each "diagonal" of the current traversal 
      for (const auto &diagonal : traversal.tvect) {

         #pragma omp parallel if (_omp)
         {

         std::valarray<float> pscores(nc);

         //For each pixel of the current diagonal
         #pragma omp for 
         for (int pp = 0; pp < diagonal.size(); pp++) {
            
            long p = diagonal[pp];

            // previous and above pixels indices (in 1D)
            long previous = p + previous_offset;
            long above    = (withAbove == 1) ? p + above_offset : -1;


            // Compute the regularized score for the current pixel.
            // To be defined by the derived class
            getPixelRegularizationScores(p, previous, above, tr, pscores);

            // This is a critical (omp-wise) section. Use direct mem access for
            // concurrent writing. std::vector doesn't seems to handle this well
            // without a pragma critical which seems to slow down the process 
            // significantly. Not entirely sure about this explanation though.
            //#pragma omp critical
            //std::copy_n(begin(pscores), nc, tr_start + p*nc + 1);
            std::memcpy(&tr[p*nc], &pscores[0], nc*sizeof(float));

         }

         } // end pragma parallel

      }


      // Cumulate this traversal contribution to the overall cost
      regScores += tr; 

   } // end iteration on tfull


   // Add the corrective offset (See paper for O.C.) on the final sum.
   //regScores -= 7.0f * (1.0f - rawScores);
   regScores -= (numDirections - 1.f) * rawScores;


   // Getting final best correlation location for each pixel and disparity
   // Winner-takes-all strategy
   findWTA(regScores);

}








void DispFinder::initializeTraversals(const int ns, const int nl) {


   // Clear the traversals container. 
   tfull.clear();

   // Resize the container according to the number of regularization directions
   tfull.resize(numDirections);

   // Compute the traversals
   // In case of 4 directions, it will be the in the column and line directions
   // only, and if 8 directions it will be augmented with the diagonal
   // directions.


   // TRAVERSAL 1 ////////////////////////////////////////////////////////////
   tfull[0].tvect.resize(ns+nl-1);
    
   // Seed loc: position of the first pixel of each traversal
   std::vector<int> seeds;

   // First: seeds that are from one side of image (first row, going left to 
   // right)
   for (int i=0; i<ns; i++)
      seeds.push_back(i);
   // Second: seeds that are from the other side (last column, going top to 
   // bottom, except first pixel of last column as it has already been done)
   for (int i=1; i<nl; i++)
      seeds.push_back(ns*(i+1) - 1);

   // Iterate over the seeds and generate the traversal (diagonal in this case)
   int currentLoc, nextLoc;
   for (int i = 0; i<seeds.size(); i++) {
      currentLoc = seeds[i];
      tfull[0].tvect[i].push_back(currentLoc);
      nextLoc = currentLoc+ns-1;
      while ((nextLoc/ns != currentLoc/ns) && (nextLoc < (ns*nl))) {
         tfull[0].tvect[i].push_back(nextLoc);
         currentLoc = nextLoc;
         nextLoc += (ns-1);
      }
   }

   // Define "previous" and "above" pixel for that traversal
   tfull[0].previous = -1;
   tfull[0].above    = -ns;




   // TRAVERSAL 2 ////////////////////////////////////////////////////////////
   // This is the inverse of traversal 1. The diagonal are the same, it's only
   // their processing order that are reversed.
   //std::vector<std::vector<int> > t2;
   for (auto it=tfull[0].tvect.rbegin(); it!=tfull[0].tvect.rend(); it++)
      tfull[1].tvect.push_back(*it); 

   // Define "previous" and "above" pixel for that traversal
   tfull[1].previous = 1;
   tfull[1].above    = ns;




   // TRAVERSAL 3 /////////////////////////////////////////////////////////////
   tfull[2].tvect.resize(ns+nl-1);

   // First: seeds that are from one side of image (first column, going bottom
   // to top right)
   seeds.clear();
   for (int i=nl-1; i>=0; i--)
      seeds.push_back(i*ns);
   // Second: seeds that are from the other side (first line, going left to 
   // right, except first pixel as it has already been done)
   for (int i=1; i<ns; i++)
      seeds.push_back(i);

   // Iterate over the seeds and generate the traversal (diagonal in this case)
   for (int i = 0; i<seeds.size(); i++) {
      currentLoc = seeds[i];
      tfull[2].tvect[i].push_back(currentLoc);
      nextLoc = currentLoc+ns+1;
      while ((nextLoc/ns == (currentLoc/ns + 1)) && (nextLoc < (ns*nl))) {
         tfull[2].tvect[i].push_back(nextLoc);
         currentLoc = nextLoc;
         nextLoc += (ns+1);
      }
   }

   // Define "previous" and "above" pixel for that traversal
   tfull[2].previous = ns;
   tfull[2].above    = -1;




   // TRAVERSAL 4 /////////////////////////////////////////////////////////////
   // This is the inverse of traversal 3. The diagonal are the same, it's only
   // their processing order that are reversed.
   //std::vector<std::vector<int> > t4;
   for (auto it=tfull[2].tvect.rbegin(); it!=tfull[2].tvect.rend(); it++)
      tfull[3].tvect.push_back(*it); 

   // Define "previous" and "above" pixel for that traversal
   tfull[3].previous = -ns;
   tfull[3].above    = 1;




  // If number of regularization direction is 8 (with diagonals), define
  // them below, otherwise job is done
  if (numDirections == 4)
     return;



   // TRAVERSAL 5 /////////////////////////////////////////////////////////////
   // The traversal for this one is just line by line top to bottom
   // Main traveversal container
   //std::vector<std::vector<int> > t5(nl);
   tfull[4].tvect.resize(nl);

   // Generate each traversal
   for (int i=0; i<nl; i++) {
      tfull[4].tvect[i].resize(ns);
      std::iota(begin(tfull[4].tvect[i]), end(tfull[4].tvect[i]), i*ns);
   }

   // Define "previous" and "above" pixel for that traversal
   tfull[4].previous = -ns-1;
   tfull[4].above    = -ns+1;




   // TRAVERSAL 6 /////////////////////////////////////////////////////////////
   // This is the inverse of traversal 5. The diagonals are the same, it's
   // their processing order that are reversed
   //std::vector<std::vector<int> > t6;
   for (auto it=tfull[4].tvect.rbegin(); it!=tfull[4].tvect.rend(); it++)
      tfull[5].tvect.push_back(*it); 

   // Define "previous" and "above" pixel for that traversal
   tfull[5].previous = ns+1;
   tfull[5].above    = ns-1;




   // TRAVERSAL 7 /////////////////////////////////////////////////////////////
   // The traversal for this one is just column by column left to right
   // Main traveversal container
   //std::vector<std::vector<int> > t7(ns);
   tfull[6].tvect.resize(ns);

   // Generate each traversal
   for (int i=0; i<ns; i++) {
      tfull[6].tvect[i].resize(nl);
      std::generate(begin(tfull[6].tvect[i]), end(tfull[6].tvect[i]), 
                          [n=i, ns] () mutable{ n+=ns; return n-ns;});
   }

   // Define "previous" and "above" pixel for that traversal
   tfull[6].previous =  ns-1;
   tfull[6].above    = -ns-1;




   // TRAVERSAL 8 /////////////////////////////////////////////////////////////
   // This is the inverse of traversal 7. The diagonals are the same, it's
   // their processing order that are reversed
   //std::vector<std::vector<int> > t8;
   for (auto it=tfull[6].tvect.rbegin(); it!=tfull[6].tvect.rend(); it++)
      tfull[7].tvect.push_back(*it); 

   // Define "previous" and "above" pixel for that traversal
   tfull[7].previous = -ns+1;
   tfull[7].above    =  ns+1;


}






SimpleImage<int> DispFinder::doLeftRightConsistencyCheck(const DispFinder &right, const float thresholdScalar) {

   //For now, only the basic bilinear interpolation, similar to marsdispcompare
   //Eventually may implement a least square over larger region for more robustness



   if (dispX == nullptr || dispY == nullptr || 
       right.dispX == nullptr || right.dispY == nullptr) {
      zvmessage("Failed L/R-R/L consistency check - invalid disparities","");
      return SimpleImage<int>();
   }


   int nsl = dispX->getNS();
   int nll = dispX->getNL();
   int nsr = (right.dispX)->getNS();
   int nlr = (right.dispX)->getNL();


   // Get the acceptable distance between a given "left" pixel and the location
   // of the reprojected pixel (from "left" to "right" and back to "left").
   // That distance depends on the the nominal disparity accuracy and the input
   // scalar factor which acts as a knob on the disparity accuracy.
   // The disparity accuracy is given by the subpixel level of the matching. It
   // is expected that the subpixel level between the matching left/right and
   // right/left is the same, but doesn't have to. We'll take the coarser one.
   // Note that for efficiency, we use the square of the distance to avoid 
   // unnecessary sqrt
   auto lSubpixel = imgMatcher->getSubpixel();
   auto rSubpixel = (right.imgMatcher)->getSubpixel();

   float precisionX = (lSubpixel[0] < rSubpixel[0]) ? lSubpixel[0] : rSubpixel[0];
   precisionX = 1 / (precisionX + 1);  // convert to supixel precision

   float precisionY = (lSubpixel[1] < rSubpixel[1]) ? lSubpixel[1] : rSubpixel[1];
   precisionY = 1 / (precisionY + 1);

   // Get square of maximum acceptable misprojection distance
   float maxDist = thresholdScalar * thresholdScalar * 
                   (precisionX * precisionX + precisionY * precisionY);


   SimpleImage<int> mask(nll, nsl);
   mask.zero();
  
 
   #pragma omp parallel if (_omp)
   for (int j = 0; j < nll; j++) { 
   
      for (int i = 0; i < nsl; i++) { 

         // Get the location in "right" pointed at by the current pixel
         float xR = dispX->get(j,i);
         float yR = dispY->get(j,i);

         // Get the location integer value to locate the 4 surrounding
         // pixels
         int xRi = static_cast<int>(xR);
         int yRi = static_cast<int>(yR);


         // If location outside (or too close) to right image boundary, filter 
         // out that pixel and move to next one
         if (xRi < 0 || xRi > (nsr-2) || yRi < 0 || yRi > (nlr-2) ) 
            continue;

         // Get the locations in "left" pointed at by the 4 surrounding
         // "right" pixels
         // In sample direction
         float tlLx = (right.dispX)->get(yRi, xRi);
         float trLx = (right.dispX)->get(yRi, xRi+1);
         float blLx = (right.dispX)->get(yRi+1, xRi);
         float brLx = (right.dispX)->get(yRi+1, xRi+1);
         if (tlLx == 0 || trLx == 0 || blLx == 0 || brLx == 0)  
            continue;

         // In line direction
         float tlLy = (right.dispY)->get(yRi, xRi);
         float trLy = (right.dispY)->get(yRi, xRi+1);
         float blLy = (right.dispY)->get(yRi+1, xRi);
         float brLy = (right.dispY)->get(yRi+1, xRi+1);
         if (tlLy == 0 || trLy == 0 || blLy == 0 || brLy == 0)  
            continue;
         

         // Linear interpolation of the locations
         // In sample direction
         float up   = (1 - (xR-xRi)) * tlLx + (xR-xRi) * trLx;
         float down = (1 - (xR-xRi)) * blLx + (xR-xRi) * brLx;
         float xL   = (1 - (yR-yRi)) * up   + (yR-yRi) * down;

         // In line direction
         up   = (1 - (xR-xRi)) * tlLy + (xR-xRi) * trLy;
         down = (1 - (xR-xRi)) * blLy + (xR-xRi) * brLy;
         float yL   = (1 - (yR-yRi)) * up   + (yR-yRi) * down;
         

         // Check that the reprojected location in the Left image is within the
         // acceptable distance.
         if (((xL - i) * (xL - i) + (yL - j) * ( yL - j)) < maxDist) 
            mask.set(j,i,1);

      }

   }

   return mask;

}




void DispFinder::applyMask(SimpleImage<int> &mask) {

   if (dispX == nullptr || dispY == nullptr || scores == nullptr) {
      zvmessage("Invalid disparities, no mask to apply","");
      return;
   }


   int nsl = dispX->getNS();
   int nll = dispX->getNL();
   int nsm = mask.getNS();
   int nlm = mask.getNL();

   if (nsl != nsm || nll != nlm) {
      zvmessage("Mask and disparities have incompatible sizes, no mask to apply","");
      return;
   }


   std::transform(dispX->linePtr(0), dispX->linePtr(0) + nsl * nll, 
                  mask.linePtr(0), dispX->linePtr(0), std::multiplies<float>{});
   std::transform(dispY->linePtr(0), dispY->linePtr(0) + nsl * nll, 
                  mask.linePtr(0), dispY->linePtr(0), std::multiplies<float>{});
   std::transform(scores->linePtr(0), scores->linePtr(0) + nsl * nll, 
                  mask.linePtr(0), scores->linePtr(0), std::multiplies<float>{});

}




void DispFinder::autoCorrelation(float params[], int &count) {

   if (imgL == nullptr) {
      zvmessage("Input image not set, can't compute autocorrelation","");
      return;
   }

   if (imgMatcher == nullptr) {
      zvmessage("Image matcher not set, can't compute autocorrelation","");
      return;
   }

   // Make a copy of the imgMatcher parameters. We need to modify them and will
   // reset them to the original values afterwards
   auto searchBackup = imgMatcher->getSearch();
   auto subpixBackup = imgMatcher->getSubpixel();
   auto centerBackup = imgMatcher->getCentered();

   // Non-centered does not make sense
   imgMatcher->setCentered(1);

   count = 0;
   int ns = imgL->getNS();
   int nl = imgL->getNL();
   std::valarray<float> autocorr;   // Container for autocorrelation scores
   float badVal = imgMatcher->getWorstScore(); // scores for correlation failed or did not happen

   // Information message
   char msg[150];

   // Do pixel-wise autocorrelation first
   // Check if pixel wise autocorrelation is needed. If the search space is 0, 
   // only subpixel correlation.
   // The autocorrelation is done with shift in the X direction. This is arbitary.
   if (searchBackup[0] != 0 || searchBackup[1] != 0) {

      // Set a search space of 2 pixels. Default of master class.
      imgMatcher->setSearch(2,0);
      imgMatcher->setSubpixel(0,0);

      imgMatcher->matchAll(imgL->linePtr(0), imgL->linePtr(0), ns, nl, ns, nl,
                           autocorr);

      auto searchX = imgMatcher->getSearchNumX();
      auto searchY = imgMatcher->getSearchNumY();

      // Compute average matching score for offset 1 and 2 pixels.
      for (int i=0; i<3; i++) {

         int num = 0;
         double sum = 0;
         long start = (searchY-1)/2 * searchX + (searchX-1)/2 + i;
         for (long j=0; j<nl*ns; j++){
            if (autocorr[start + j * searchX * searchY] != badVal) {
               num++;
               sum += autocorr[start + j * searchX * searchY];
            } 
         }
         count++;
         params[count-1] = sum / num; // average corr score
      }

      sprintf(msg, "Autocorrelation score:  %8.4f (1-pix shift)  %8.4f (2-pix shift)", 
              params[count-2], params[count-1]);
      zvmessage(msg, "");
   }

   // Do subpixel-wise autocorrelation
   if (subpixBackup[0] != 0 || subpixBackup[1] != 0) {

      // Set pixel search to 1 to handle corner case subpixel precision of 0.5
      // (in this case correlator only match at 1 subpixel location)
      imgMatcher->setSearch(1,0);
      imgMatcher->setSubpixel(std::max(subpixBackup[0], subpixBackup[1]),0);

      imgMatcher->matchAll(imgL->linePtr(0), imgL->linePtr(0), ns, nl, ns, nl,
                           autocorr);

      auto searchX = imgMatcher->getSearchNumX();
      auto searchY = imgMatcher->getSearchNumY();

      // Compute average matching score for offset 1 and 2 pixels.
      for (int i=0; i<3; i++) {
      //for (int i=0; i<(searchX-1)/2; i++) {

         int num = 0;
         double sum = 0;
         long start = (searchY-1)/2 * searchX + (searchX-1)/2 + i;
         for (long j=0; j<nl*ns; j++){
            if (autocorr[start + j * searchX * searchY] != badVal) {
               num++;
               sum += autocorr[start + j * searchX * searchY];
            } 
         }
         count++;
         params[count-1] = sum / num; // average corr score
      }
      sprintf(msg, "Autocorrelation score: %8.4f (1-subpix shift)  %8.4f (2-subpix shift)", 
              params[count-2], params[count-1]);
      zvmessage(msg, "");
   }

   if (count == 0)
      zvmessage("Image matcher search and subpixel search not set. Can't compute autocorrelation", "");
   else {
      char msg[150];
      if (count == 6)
         sprintf(msg, "Regularization params: REG_PARAM=\\( %8.4f %8.4f %8.4f %8.4f \\)", 
                 params[1], params[2], params[4], params[5]);
      else
         sprintf(msg, "Regularization params: REG_PARAM=\\( %8.4f %8.4f \\)", 
                 params[1], params[2]);
      zvmessage(msg,"");
   }


   // Reset the original search and subpixel search
   imgMatcher->setSearch(searchBackup[0], searchBackup[1]);
   imgMatcher->setSubpixel(subpixBackup[0], subpixBackup[1]);
   imgMatcher->setCentered(centerBackup);



}


