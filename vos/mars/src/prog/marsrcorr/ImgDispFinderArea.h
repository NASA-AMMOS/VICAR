///////////////////////////////////////////////////////////////////////////////
// Area-based Disparity Finder
//
// This class performs standard area-based (e.g., 2D search space) image
// matching
//
///////////////////////////////////////////////////////////////////////////////

#ifndef IMGDISPFINDERAREA_H
#define IMGDISPFINDERAREA_H

#include "ImgDispFinder.h"



class AreaDispFinder : public DispFinder {

   public:

      AreaDispFinder();

      // Getter/setter for disparity prior
      void setInputPrior(const PriorType type); 
      void setInputPrior(const float priors[], const int count); 
      void getInputPrior(float priors[], const int count); 
      void setInputPrior(SimpleImage<float> * priorDisparity); 
      //virtual void setInputPrior(MeshMan * mesh); 
      int  getInputPriorCount(); 
      std::string getInputPriorName(const int index); 

      // Getter/setter for regularization parameters
      int getRegularizationParamCount();
      std::string getRegularizationParamName(const int index);
      void setRegularizationParameters(const float params[], const int count);
      void getRegularizationParameters(float params[], const int count);


   private:

      // Regularization items *****************
      float p1; // 1  pixel shift penalty
      float p2; // 2+ pixel shift penalty
      std::vector<std::vector<short> > distNeighbors;
      int numCorrPerPixel;

      // Search prior items *******************
      struct inputPrior { std::array<float,6> aT;
                          SimpleImage<float> * pX;
                          SimpleImage<float> * pY;
                          int ns;
                          int nl;
                        } inputPrior;

      // Disparity priors items ***************
      std::vector<float> priorX;
      std::vector<float> priorY;


      // Internal functions *******************
      std::string doGetType();
      int getSearchSpacePerPixel();      
      int getSearchSpacePerPixel(int &fullSearchX, int &fullSearchY);      
      void initializeCorrNeighbor();
      void getPixelRegularizationScores(long current, long previous, long above, std::valarray<float> & tscores, std::valarray<float> &pscores);
      void findWTA(const std::valarray<float> & scoresVolume);
      int  doComputeMatchScores(); 
      void generateSeedMap();

};


#endif

