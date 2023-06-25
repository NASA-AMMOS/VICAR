///////////////////////////////////////////////////////////////////////////////
// Imagery Utilities
//
// This is a collection of imagery-related functions that are commonly used in
// program manipulating images
//
///////////////////////////////////////////////////////////////////////////////

#ifndef IMGUTILITIES_H
#define IMGUTILITIES_H

#include "SimpleImage.h" 
#include "PigFileModel.h"
#include <string>
#include <vector>
#include <valarray>
#include <array>

// Reading/Writing VICAR images

void read_image(char *param, SimpleImage<float> *&image);
SimpleImage<float> * loadImage(PigFileModel * fileModel, int band);

template<class T>
void saveImg(const std::string name, SimpleImage<T> * img);



////////////////////////////////////////////////////////////////////////////////
// Integral Image (summation table) utilities
// An "integral image" of an image is an image in which each pixel DN value is 
// the sum of all the pixel of the input image contained in a "box" whose
// top-left corner is pix (0,0) and bottom-right corner is the considered pixel.
// Once computed, it allows to get the sum of any rectangular subset of the 
// input image in just 4 additions, which could speed up significantly any 
// process requiring frequent subset sum. A typical use is image matching.
////////////////////////////////////////////////////////////////////////////////

// Compute the integral image of the input image. 
// Size of the input image given by nbX, nbY.
// Output container must be allocated and must contain at least the same number
// of elements as the input image.
template<class T>
void computeIntegralImage(const T *in, int nbX, int nbY, double * out);

// Compute the integral image of the input image and the integral image of the
// square of the input image.
// Size of the input image given by nbX, nbY.
// Output containers must be allocated and must contain at least the same number
// of elements as the input image.
template<class T>
void computeIntegralImage(const T *in, int nbX, int nbY, double * out, double * out2);

// Compute the integral image of the product of two input images.
// Input images must have the same size.
// Size of the input image given by nbX, nbY.
// Output container must be allocated and must contain at least the same number
// of elements as the input image.
template<class T1, class T2>
void computeIntegralImage(const T1 *in1, const T2 *in2,  int nbX, int nbY, double * out);

// Compute the integral image of the product of the two input images.
// The second image can have a larger size than the first image, and the corresponding
// location of the subset to use for the image product is given by a x/y offset
// Size of the first input image given by n1X, n1Y.
// Size of the second input image given by n2X, n2Y.
// Starting location of the subset in the second image (0-based) sX, sY. It is the user
// responsability to make sure the subset in the second image does not fall out of 
// bounds.
// Output container must be allocated and must contain at least the same number
// of elements as the first input image.
template<class T1, class T2>
void computeIntegralImage(const T1 *in1, const T2 *in2,  int n1X, int n1Y, int n2X, int n2Y, int sX, int sY, double * out);

// Get the sum of a subset of an image from its integral image
// Input image is an integral image, whose line size is given by inX,inY. Top-left pixel
// of the subset to compute the sum is given by sX,sY and size of subset is nbX,nbY
double sumIntegralImage(double *in, int inX, int inY, int sX, int sY, int nbX, int nbY);

// SimpleImage input warpers
// Convenience warpers to use SimpleImage object as input/output images
template<class T>
void computeIntegralImage(SimpleImage<T> *in, SimpleImage<double> *out) {
   computeIntegralImage<T>(in->linePtr(0), in->getNS(), in->getNL(), out->linePtr(0));
}

template<class T>
void computeIntegralImage(SimpleImage<T> *in, SimpleImage<double> *out, SimpleImage<double> *out2) {
   computeIntegralImage<T>(in->linePtr(0), in->getNS(), in->getNL(), out->linePtr(0), out2->linePtr(0));
}

template<class T1, class T2>
void computeIntegralImage(SimpleImage<T1> *in1, SimpleImage<T2> *in2, SimpleImage<double> *out) {
   computeIntegralImage<T1,T2>(in1->linePtr(0), in2->linePtr(0), in1->getNS(), in1->getNL(), out->linePtr(0));
}

template<class T1, class T2>
void computeIntegralImage(SimpleImage<T1> *in1, SimpleImage<T2> *in2, int sX, int sY, SimpleImage<double> *out) {
   computeIntegralImage<T1,T2>(in1->linePtr(0), in2->linePtr(0), in1->getNS(), in1->getNL(), 
                               in2->getNS(), in2->getNL(), sX, sY, out->linePtr(0));
}

inline double sumIntegralImage(SimpleImage<double>* img, int sX, int sY, int nbX, int nbY) {
   return sumIntegralImage(img->linePtr(0), img->getNS(), img->getNL(), sX, sY, nbX, nbY);
}

 




////////////////////////////////////////////////////////////////////////////////
// Low-pass filtering and interpolation utilities
////////////////////////////////////////////////////////////////////////////////

// Gaussian blur of an input image
void ConvBufferFast(float *buffer, float *kernel, int rsize, int ksize);
void ConvHorizontal(SimpleImage<float> *image, float *kernel, int ksize, int omp);
void ConvVertical(SimpleImage<float> *image, float *kernel, int ksize, int omp);
void GaussianBlur(SimpleImage<float> *image, float sigmaX, float sigmaY, int omp);

// Bicubic resampling of an image from mapping matrices
// img: input image raw array
// nbX,nbY: Number of samples and lines of the input image
// matX,matY: resampling matrices containing the X and Y coordinates of the 
// locations in the input image to resample.
// out: output image raw array. The array must be allocated with at least
// matX.size() elements.
template<class T>
int resampleImageBicubic(const T *img, const int nbX, const int nbY,
                         const std::valarray<float> &matX, 
                         const std::valarray<float> &matY,
                         float * out,
                         int omp_on); 

// Convenience wrapper for bicubic resampling using SimpleImage as input and
// output. The output SimpleImage must be instanciated and large enough
template<class T>
int resampleImageBicubic(SimpleImage<T> *img, 
                         const std::valarray<float> &matX, 
                         const std::valarray<float> &matY,
                         SimpleImage<float> *out,
                         int omp_on); 

// Downsampling function by an integer factor. Downsampling factor can be
// different between X and Y.
// Downsampling is a 2-steps process: 
// - low-pass filtering (Gaussian low-pass) whose intensity is dependent on the
//   downsampling factor, to avoid aliasing.
// - bicubic resampling at the new positions. The new pixel position is in the
//   "middle" of the agreggated pixels. For instance, assume downsampling by a
//   factor or 2 (in 1-dimension). The first 2 pixels have coordinates 0 and 1 
//   in the original image. The position of the corresponding downsampled pixel
//   will be at 0.5 (in the original image)
// If downsampling factor is 1 or less, nothing to be done, the input is 
// copied to the output as-is.
// The output and input can be the same, in which case, the input will contain
// the downsampled image.
void downsample(SimpleImage<float> *imgIn, SimpleImage<float> *&imgOut,
                int downFactorX, int downFactorY, int omp);

inline void downsample(SimpleImage<float> *imgIn, SimpleImage<float> *&imgOut,
                int downFactor, int omp) {
   downsample(imgIn, imgOut, downFactor, downFactor, omp);
};



// General resampling function
// This function upsample or downsample an image by a given factor in X and Y.
// The output image size is defined from the input image size and resampling
// factors rounded to nearest integer.
// Input and Output can be the same container, in which case the input will
// be overwritten (if input image "owns" the data).
// factor > 1 means downsampling (reduction in resolution and size), while
// a factor < 1 means upsampling (increase in resolution and size).
// In case of downsampling, low-pass filtering is applied prior to resampling.
// Resampling is done using bicubic interpolation
void resampleImage(SimpleImage<float> *imgIn, SimpleImage<float> *&imgOut,
                const float &factorX, const float &factorY, int omp);



////////////////////////////////////////////////////////////////////////////////
// Image void filling
// There are numerous approaches for filling in image holes. 
// The list below is to be augmented as needs arise
////////////////////////////////////////////////////////////////////////////////

// Given a list of 2D points (x,y), return a list of 2D points defining the convex
// hull of the input dataset. The input needs to contain at least 3 non 
// collinear points.
void convexHull(const std::vector<std::array<int,2> > &inputPoints, 
                      std::vector<std::array<int,2> > &outputPoints); 


template<class T>
void invDistFill(SimpleImage<T> *inImg, SimpleImage<float> *&imgOut,
                 const int minCoverage, const int power, const int border);






#endif

