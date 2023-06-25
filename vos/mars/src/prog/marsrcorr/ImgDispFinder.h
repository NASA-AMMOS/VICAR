///////////////////////////////////////////////////////////////////////////////
// DisparityFinder
//
// This is the base class for regularized disparity finder.
// This class is pure virtual
//
///////////////////////////////////////////////////////////////////////////////

#ifndef IMGDISPFINDER_H
#define IMGDISPFINDER_H

#include <string>
#include <cstring>
#include <valarray>
#include <vector>
#include <functional>

#include "SimpleImage.h" 
#include "ImgMatcher.h" 

// Type of regularization available
enum class RegType {none, sgm, mgm};

// Type of prior to be used. 
// This enum regroup all the prior type available from all the derived classes 
// of Disparity Finders. Not best software design, but will do for now. 
enum class PriorType {none, output, affine, map, mesh};

class DispFinder {

   public:

      // This is a pure virtual class. No direct constructor call.
      DispFinder();
      ~DispFinder();

      // Function to return type of disparity finder.
      std::string getType();

      // Set the Left and Right image.
      void setImages(SimpleImage<float> * imgL, SimpleImage<float> * imgR);

      // Define the type of matching regularization to apply. For now, choice
      // between SGM or MGM with a 4 or 8 regularization directions
      void setRegularization(RegType inRegularization, int inNumDirections);
      std::string getRegularizationName();
      RegType getRegularization();
      int getRegularizationNumDirections();

      // Define which image matcher algorithm to use (NCC, SAD, SDD, etc)
      void setImageMatcher(ImgMatcher * inImgMatcher);

      // Run the matching algorithm on the Left and Right images to construct 
      // the "cube" of matching score, i.e., the data term of the regularization 
      // process. 
      int computeMatchScores();

      // Delete matching scores to free up memory
      void clearMatchScores();

      // Run the regularization on the data term, i.e., on the correlation 
      // score "cube"
      void regularize();

      // Make a Left/Right Right/Left consistency check between two disparities
      // obtained by switching the Left and the Right image. This helps at 
      // removing gross errors and erroneous measurements due to occlusion
      //TODO may be overloaded in subclass if simple dispXY comparison is not enough
      SimpleImage<int> leftRightConsistencyCheck(const DispFinder &right, const float thresholdScalar); 
      //void leftRightConsistencyCheck(const DispFinder &right, const float thresholdScalar); 

      // Return a handle on the sample disparity image
      // The handle allows read/modification of the image, but not freeing the
      // memory
      SimpleImage<float> * getDisparityX();
      void replaceDisparityX(SimpleImage<float> * newDispX);

      // Return a handle on the line disparity image
      // The handle allows read/modification of the image, but not freeing the
      // memory
      SimpleImage<float> * getDisparityY();
      void replaceDisparityY(SimpleImage<float> * newDispY);

      // Return a handle on the matching score image. Note that the score will
      // depends on the matching algoritm used.
      // The handle allows read/modification of the image, but not freeing the
      // memory
      SimpleImage<float> * getDisparityScores();
      void replaceScores(SimpleImage<float> * newScores);

      // Generate a seed map prior to the matching to reduce the search space.
      // To be defined by the derived class
      virtual void generateSeedMap() = 0;

      // Apply a mask to the dispX, dispY, scores images. Mask 0 value will set
      // values to 0, while 1 will not change the values    
      void applyMask(SimpleImage<int> &mask);


      //Input priors getters/setters
      PriorType getPriorType();
      virtual void setInputPrior(const PriorType type) = 0; 
      virtual void setInputPrior(const float priors[], const int count) = 0; 
      virtual void getInputPrior(float priors[], const int count) = 0; 
      virtual void setInputPrior(SimpleImage<float> * priorDisparity) = 0; 
      //virtual void setInputPrior(MeshMan * mesh) = 0; 
      virtual int getInputPriorCount() = 0; 
      virtual std::string getInputPriorName(const int index) = 0; 


      //Regularization parameters getters/setters
      virtual void setRegularizationParameters(const float params[], const int maxCount) = 0;
      virtual void getRegularizationParameters(float params[], const int count) = 0;
      virtual std::string getRegularizationParamName(const int index) = 0;
      virtual int getRegularizationParamCount() = 0;
      virtual void autoCorrelation(float params[], int &count);

      
      // Flag to use openMP whenever possible
      void setOmp(int omp) { _omp = omp;};
      int  getOmp() { return _omp;};

   protected:

      // Images items *************************
      SimpleImage<float> * imgL;
      SimpleImage<float> * imgR;

      // Image matching items *****************
      // Matching engine
      ImgMatcher * imgMatcher;
      // Raw matching score container (data term to be regularized)
      std::valarray<float> rawScores;
      // Regularized matching scores (regularized data term)
      std::valarray<float> regScores;

      // Prior type items *********************
      PriorType priorType;

      // Output items *************************
      SimpleImage<float> * dispX;
      SimpleImage<float> * dispY;
      SimpleImage<float> * scores;


      // Regularization items *****************
      // Traversal definition
      RegType regularization;
      int numDirections;

      struct traversal {
         std::vector< std::vector<int> > tvect;
         int previous;
         int above;
      };
      // List of traversal (either 4 or 8)
      std::vector<traversal> tfull;

      int _omp;

   private:

      virtual int doComputeMatchScores() = 0; 
      void initializeTraversals(const int ns, const int nl);
      virtual void initializeCorrNeighbor(); 
      virtual int getSearchSpacePerPixel() = 0;
      virtual void getPixelRegularizationScores(const long current, const long previous, const long above, std::valarray<float> & tscores, std::valarray<float> &pscores) = 0;
      virtual SimpleImage<int> doLeftRightConsistencyCheck(const DispFinder &right, const float thresholdScalar); 
      
      virtual void findWTA(const std::valarray<float> &val) = 0;
      virtual std::string doGetType();


};

#endif

