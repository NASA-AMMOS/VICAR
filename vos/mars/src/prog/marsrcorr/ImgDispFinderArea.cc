///////////////////////////////////////////////////////////////////////////////
// Area-based Disparity Finder
//
// This class performs standard area-based (e.g., 2D search space) image
// matching
//
///////////////////////////////////////////////////////////////////////////////

#include "ImgUtilities.h"
#include "ImgDispFinderArea.h"


AreaDispFinder::AreaDispFinder() {

  // Set default Regularization parameters to values such that there's no
  // regularization, i.e., simple WTA on raw scores
  p1 = 0;
  p2 = 0;

  // Set default input prior to none
  priorType = PriorType::none;
  inputPrior.aT = {1, 0, 0, 0, 1, 0}; //identity transform
  inputPrior.pX = nullptr;
  inputPrior.pY = nullptr;

}


std::string AreaDispFinder::doGetType() {
   return std::string("Area-based Disparity Finder");
}


int AreaDispFinder::getRegularizationParamCount() {
   return 2;
}

std::string AreaDispFinder::getRegularizationParamName(const int index) {
   switch (index) {
      case 0: 
         return "P1";
      case 1: 
         return "P2";
      default: 
         return "unknown";
   }
}

void AreaDispFinder::setRegularizationParameters(const float params[], const int count) {
   
   if (count >= 2) {
      p1 = params[0];
      p2 = params[1];
   }
   else if (count >= 1) {
      p1 = params[0];
   }
   else {}
}

void AreaDispFinder::getRegularizationParameters(float params[], const int count) {

   if (count >= 1)
      params[0] = p1;
   if (count >= 2)
      params[1] = p2;
}





void AreaDispFinder::setInputPrior(const PriorType inId) {
   
   if (inId != PriorType::none && inId != PriorType::output && 
       inId != PriorType::affine && priorType != PriorType::map) {
      zvmessage("Prior type not supported for Area Disparity Finder","");
      return;
   }

   priorType = inId;
}

void AreaDispFinder::setInputPrior(const float priors[], const int count) {

   if (count == 8) {
      inputPrior.aT = {priors[0], priors[1], priors[2], priors[3], priors[4], priors[5]};
      inputPrior.ns = static_cast<int>(priors[6]);
      inputPrior.nl = static_cast<int>(priors[7]);
      priorType = PriorType::affine;
   }
   else {
      char msg[100];
      sprintf(msg, "%d number of prior parameters unsupported for Area Disparity Matcher.", count);
      zvmessage(msg,"");
   }
}


void AreaDispFinder::getInputPrior(float priors[], const int count) {

   if (count >= 1) 
      priors[0] = inputPrior.aT[0];
   if (count >= 2) 
      priors[1] = inputPrior.aT[1];
   if (count >= 3) 
      priors[2] = inputPrior.aT[2];
   if (count >= 4) 
      priors[3] = inputPrior.aT[3];
   if (count >= 5) 
      priors[4] = inputPrior.aT[4];
   if (count >= 6) 
      priors[5] = inputPrior.aT[5];
   if (count >= 7) 
      priors[6] = static_cast<float>(inputPrior.ns);
   if (count >= 8) 
      priors[7] = static_cast<float>(inputPrior.nl);

}



void AreaDispFinder::setInputPrior(SimpleImage<float> * pDisp) {

   if (pDisp != nullptr) {
      if (pDisp->getNB() >= 2) {
         if (inputPrior.pX != nullptr)
            (inputPrior.pX)->free();
         inputPrior.pX = new SimpleImage<float>(*pDisp, 1, 0, 0, 1, pDisp->getNL(), pDisp->getNS()); 

         if (inputPrior.pY != nullptr)
            (inputPrior.pY)->free();
         inputPrior.pY = new SimpleImage<float>(*pDisp, 0, 0, 0, 1, pDisp->getNL(), pDisp->getNS()); 

         priorType = PriorType::map;
      }
      else 
         zvmessage("Error: Prior maps should have 2 bands for lines and samples disparities", "");
   }
}


int AreaDispFinder::getInputPriorCount() {

   switch (priorType) {
      case PriorType::affine: return 8;
      case PriorType::map: return 1;
      case PriorType::output: return 1;
      default: return 0;
   }

}

std::string AreaDispFinder::getInputPriorName(const int index) {

   switch (priorType) {
      case PriorType::none: return "";
      case PriorType::affine: {
         switch (index) {
             case 0: return "a";
             case 1: return "b";
             case 2: return "c";
             case 3: return "d";
             case 4: return "e";
             case 5: return "f";
             case 6: return "nX";
             case 7: return "nY";
             default: return "unknown";
         }     
      }  
      case PriorType::map: return "Prior Disparities";
      case PriorType::output: return "Output Disparities";
      default: return "unknown";
   }

}




int AreaDispFinder::getSearchSpacePerPixel() {
   int tmpX, tmpY;
   return getSearchSpacePerPixel(tmpX, tmpY);
}


int AreaDispFinder::getSearchSpacePerPixel(int &fullSearchX, int &fullSearchY) {

   // For the AreaDispFinder, the search space per pixel only depends on the
   // image matcher parameters (search + subpixel). For different Disp Finder 
   // that might be entirely different.
   if (imgMatcher == nullptr) {
      zvmessage("Error: Null Image Matcher","");
      return 0;
   }

   fullSearchX = imgMatcher->getSearchNumX();
   fullSearchY = imgMatcher->getSearchNumY();

   return fullSearchX * fullSearchY; 


}



void AreaDispFinder::initializeCorrNeighbor() {

   // Clear container first
   for (auto & d: distNeighbors)
      d.clear();

   // Number of search positions in X, Y and
   // total
   int ncs, ncl;
   numCorrPerPixel = getSearchSpacePerPixel(ncs, ncl);

   // Size the container accordingly
   distNeighbors.resize(numCorrPerPixel);

   for (int loc = 0; loc < numCorrPerPixel; loc++) {
      int x = loc % ncs;
      int y = loc / ncs;

      // Get all pixels that are 1 pixel away from loc
      for (int p = 0; p < numCorrPerPixel; p++) 
         if ((pow(x - p%ncs, 2) + pow(y - p/ncs, 2)) < 2 && (loc != p)) 
             distNeighbors[loc].push_back(static_cast<short>(p));
   }

}



void AreaDispFinder::getPixelRegularizationScores(long p, long previous, long above, std::valarray<float> &tr, std::valarray<float> &pscores) {

   int ns = imgL->getNS();
   int nl = imgL->getNL();


   // check that "previous" and "above" pixels are inside the image
   int p_valid = 0;
   int x = previous % ns;
   int y = previous / ns;
   if ((x >= 0) && (x < ns) && (y >= 0) && (y < nl)) 
      p_valid = 1;   

   int a_valid = 0;
   if (above != -1) { // -1 means SGM only, "above" pixel not considered
      x = above % ns;
      y = above / ns;
      if ( (x >= 0) && (x < ns) && (y >= 0) && (y < nl)) 
         a_valid = 1;
   }

   // Do we have contribution of both "previous" and "above". If yes, split the 
   // contribution of each.
   float weight = (a_valid == p_valid) ? 0.5 : 1;


   // previous and above pixels indices not used anymore. Recyle the variables 
   // to the indices of the previous and above pixels costs in main container
   previous *= numCorrPerPixel;
   above *= numCorrPerPixel;


   // Compute smallest label for "previous" pixel
   float min_p;
   if (p_valid) {
      min_p = *(std::min_element(begin(tr) + previous, begin(tr) + previous + numCorrPerPixel));
      min_p += p2;
   } 

   // Compute smallest label for "above" pixel
   float min_a;
   if (a_valid) {
      min_a = *(std::min_element(begin(tr) + above, begin(tr) + above + numCorrPerPixel));
      min_a += p2;
   }


   // For each corr search loc 
   for (int l = 0; l < numCorrPerPixel; l++) {

      float score = rawScores[p * numCorrPerPixel + l];  


      if (p_valid) {
         float regVal = tr[previous + l]; 

         // neighboor
         for (auto d:distNeighbors[l]) {
            float tmp = tr[previous + d] + p1;
            if (tmp < regVal)
               regVal = tmp;
         }
 
         // others
         if (min_p < regVal)
            regVal = min_p;

         score += weight * regVal;
      }
            
      if (a_valid) {
         float regVal = tr[above +l]; 

         // neighboor
         for (auto d:distNeighbors[l]) {
            float tmp = tr[above + d] + p1;
            if (tmp < regVal)
               regVal = tmp;
         }
 
         // others
         if (min_a < regVal)
            regVal = min_a;

         score += weight * regVal;
      }

      //tr[p*nc + l] = score;  
      pscores[l] = score;  

   }


}





void AreaDispFinder::findWTA(const std::valarray<float> & scoresVolume) {


   int ns = imgL->getNS();
   int nl = imgL->getNL();

   // Delete previous Disparities 
   if (dispX != nullptr) { 
      dispX->free();
      delete dispX;
   }
   if (dispY != nullptr) {
      dispY->free();
      delete dispY;
   }
   if (scores != nullptr) {
      scores->free();
      delete scores;
   }

   // Allocate new Diparities
   scores = new SimpleImage<float>(nl, ns);
   dispX  = new SimpleImage<float>(nl, ns);
   dispY  = new SimpleImage<float>(nl, ns);


   // Get new Disparities for each pixel
   // Winner-takes-all strategy
   float *pX = nullptr;
   float *pY = nullptr;
   if (priorType != PriorType::none && 
       priorX.size() == ns*nl && priorY.size() == ns*nl) {
      pX = priorX.data();
      pY = priorY.data();
   }
   imgMatcher->findWTA(scoresVolume,
                       pX, pY,
                       ns, nl,
                       scores->linePtr(0),
                       dispX->linePtr(0),
                       dispY->linePtr(0));


}





int AreaDispFinder::doComputeMatchScores() {

   // Make sure that required data is available
   if (imgL == nullptr || imgR == nullptr || imgMatcher == nullptr) {
      zvmessage("Can't compute matching. Images or matcher are missing","");
      return 0;
   }

   // Generate any prior map if requested
   generateSeedMap();

   if (priorType == PriorType::none) 
      imgMatcher->matchAll(imgL->linePtr(0), imgR->linePtr(0), 
                           imgL->getNS(), imgL->getNL(),
                           imgR->getNS(), imgR->getNL(),
                           rawScores);
   else {
      // If generateSeedMap failed for some reason, inform and return
      if (priorX.size() != imgL->getNS() * imgL->getNL() || 
          priorY.size() != imgL->getNS() * imgL->getNL()) {
         zvmessage("Generation of prior seed map failed. No matching performed","");
         return 0;
      } 

      imgMatcher->matchAllWithPrior(imgL->linePtr(0), imgR->linePtr(0), 
                                    priorX.data(), priorY.data(),
                                    imgL->getNS(), imgL->getNL(),
                                    imgR->getNS(), imgR->getNL(),
                                    rawScores);
   }

   return 1;

}





void AreaDispFinder::generateSeedMap() {

   // Clear any prior.
   priorX.clear(); 
   priorY.clear(); 

   // If no prior, nothing to do, return
   if (priorType == PriorType::none)
      return;


   int ns = imgL->getNS();   
   int nl = imgL->getNL();   


   // Generate disparity seed using input affine coefficients
   if (priorType == PriorType::affine) {

      float scaleX = static_cast<float>(ns) / inputPrior.ns;
      float scaleY = static_cast<float>(nl) / inputPrior.nl;
      priorX.resize(ns * nl);
      priorY.resize(ns * nl);
      std::array<float, 6> aT = inputPrior.aT;
      for (int l=0; l<nl; l++) {
         for (int s=0; l<ns; s++) {
            priorX[l*ns + s] = s * aT[0] + l * aT[1] + aT[2] * scaleX;
            priorY[l*nl + s] = s * aT[3] + l * aT[4] + aT[5] * scaleY;
         }
      }
   }

   // ... or generate disparity seed using input maps
   // Two possibilities for the input maps:
   // - Supplied by the user (prior), obtained from an external process.
   //   Data is obtained from inputPrior structure
   // - Obtained from a previous disparity map computation (current object).
   //   Use case is pyramidal approach where disparity at previous level is 
   //   used to initialize search in current level. Another use case is a 
   //   2-iteration approach, first iteration search at pixel resolution, 
   //   and second iteration at subpixel resolution.
   //   Data is obtained from dispX/Y
   //
   // TODO Fill any gap? For now, simplistic down/upsample 
   else if (priorType == PriorType::map || priorType == PriorType::output)  {

      SimpleImage<float> *inMapX, *inMapY;
      if (priorType == PriorType::map) {
         inMapX = inputPrior.pX;
         inMapY = inputPrior.pY;
      }
      else {
         inMapX = dispX;
         inMapY = dispY;
      }

      if (inMapX != nullptr && inMapY != nullptr) {

         SimpleImage<float> *pX2 = nullptr;
         SimpleImage<float> *pY2 = nullptr;
         if (inMapX->getNS() != ns || inMapX->getNL() != nl) {
            int ins = inMapX->getNS();
            int inl = inMapX->getNL();
            float ratioX = static_cast<float>(ins) / ns;
            float ratioY = static_cast<float>(inl) / nl;
            // First, convert input maps values to the new size maps
            std::transform(inMapX->linePtr(0), inMapX->linePtr(0) + ins * inl, 
                           inMapX->linePtr(0), [ratioX](float x){return x/ratioX;}); 
            std::transform(inMapY->linePtr(0), inMapY->linePtr(0) + ins * inl, 
                           inMapY->linePtr(0), [ratioY](float y){return y/ratioY;}); 

            resampleImage(inMapX, pX2, ratioX, ratioY, _omp); 
            resampleImage(inMapY, pY2, ratioX, ratioY, _omp); 

         }
         else {
            pX2 = new SimpleImage<float>(*inMapX, 0, 0 , nl, ns);
            pY2 = new SimpleImage<float>(*inMapY, 0, 0 , nl, ns);
         }

         priorX.resize(ns * nl);
         priorY.resize(ns * nl);
         std::memcpy(priorX.data(), pX2->linePtr(0), ns * nl * sizeof(float));
         std::memcpy(priorY.data(), pY2->linePtr(0), ns * nl * sizeof(float));

         pX2->free();
         pY2->free();
      }
      else {
         zvmessage("Asked for prior map to initialize match search, but maps are null","");
      }

   }

   else {}

}










