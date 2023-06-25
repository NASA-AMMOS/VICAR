///////////////////////////////////////////////////////////////////////////////
// Image Matching Classes
// Base class and derived classes implementing various matching score
///////////////////////////////////////////////////////////////////////////////

#ifndef IMGMATCHER_H
#define IMGMATCHER_H

#include <valarray>
#include <array>


class ImgMatcher {

   protected:
      int _corrX,   _corrY;
      int _searchX, _searchY;
      int _subpixX, _subpixY;
      int _centered;
      int _omp;

      float _varL, _varR;

      void computeAvg(const double * I, const int ns, const int nl, double * pAvg);


   public:

      ImgMatcher() : _corrX(9), _corrY(9), 
                       _searchX(2), _searchY(2), 
                       _subpixX(0), _subpixY(0), 
                       _centered(1), 
                       _varL(1), _varR(1),
                       _omp(1)  {}; 

      ~ImgMatcher(){};


      void findWTA(const std::valarray<float> & scores,
                   const float * priorX, const float * priorY,
                   const int ns, const int nl,
                   float *bestScore,
                   float *dispX,
                   float *dispY);

      virtual void matchAll(const float * left, const float * right, 
                            const int nsl, const int nll,
                            const int nsr, const int nlr, 
                            std::valarray<float> & scores) = 0;

      virtual void matchAllWithPrior(const float * left, const float * right, 
                                     const float *priorX, const float *priorY,
                                     const int nsl, const int nll, 
                                     const int nsr, const int nlr, 
                                     std::valarray<float> & scores) = 0;

      

      void setTemplate(const unsigned int corrX, const unsigned int corrY) { _corrX = corrX; _corrY = corrY;};
      void setSearch(const unsigned int searchX, const unsigned int searchY) { _searchX = searchX; _searchY = searchY;};
      void setSubpixel(const unsigned int subpixX, const unsigned int subpixY) { _subpixX = subpixX; _subpixY = subpixY;};
      void setCentered(const int centered) { _centered = (centered != 0) ? 1 : 0;};
      void setOmp(const int omp) { _omp = (omp != 0) ? 1 : 0;};
      void setStddevL(const float stddev) { _varL = stddev * stddev;};
      void setStddevR(const float stddev) { _varR = stddev * stddev;};
      std::array<int, 2> getTemplate() { return {_corrX, _corrY};}; 
      std::array<int, 2> getSearch() { return {_searchX, _searchY};};
      std::array<int, 2> getSubpixel() { return {_subpixX, _subpixY};};
      virtual std::string getName();
      int getCentered() { return _centered;};
      int getOmp() { return _omp;};
      float getStdDevL() { return std::sqrt(_varL);};
      float getStdDevR() { return std::sqrt(_varR);};
      int getSearchNumX();
      int getSearchNumY();
      virtual float getWorstScore() {return std::numeric_limits<float>::max();};

   private:
};





class ImgMatcherNCC : public ImgMatcher {

   public:
      ImgMatcherNCC() {};
      ~ImgMatcherNCC() {};

      void matchAll(const float * left, const float * right, 
                            const int nsl, const int nll, 
                            const int nsr, const int nlr, 
                            std::valarray<float> & scores);

      void matchAllWithPrior(const float * left, const float * right, 
                             const float *priorX, const float *priorY,
                             const int nsl, const int nll, 
                             const int nsr, const int nlr, 
                             std::valarray<float> & scores);

      std::string getName() {return std::string("Pearson Correlation Coefficient (Normalized Cross Correlation)");}
      float getWorstScore() {return 1;}

   private:
      void computeAvgAndStdev(const double * I, const double * I2, 
                              const int ns, const int nl, 
                              double * pAvg, double * pStd); 

};


class ImgMatcherZSSD : public ImgMatcher {

   public:
      ImgMatcherZSSD() {};
      ~ImgMatcherZSSD() {};

      void matchAll(const float * left, const float * right, 
                            const int nsl, const int nll, 
                            const int nsr, const int nlr, 
                            std::valarray<float> & scores);

      void matchAllWithPrior(const float * left, const float * right, 
                             const float *priorX, const float *priorY,
                             const int nsl, const int nll, 
                             const int nsr, const int nlr, 
                             std::valarray<float> & scores);

      std::string getName() {return std::string("Zero-mean Sum of Squared Difference");}
      float getWorstScore() {return std::numeric_limits<float>::max();}

   private:


};


#endif
