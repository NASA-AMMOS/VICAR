
#include <iostream>
#include "mars_support.h"
#include "ImgUtilities.h"

using namespace std;









////////////////////////////////////////////////////////////////////////
// Read an image into a SimpleImage container from its filename.  
// Returns the unit number (closed).
// Will load all bands available.
////////////////////////////////////////////////////////////////////////

void read_image(char *param, SimpleImage<float> *&image)
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
    if (ns > MARS_MAX_NS) {
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





////////////////////////////////////////////////////////////////////////
// Load an image into a SimpleImage container in float type from its
// PigFileModel object. User to select which band to load.
////////////////////////////////////////////////////////////////////////
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






////////////////////////////////////////////////////////////////////////
// Save a SimpleImage container into a vicar file to disk.
////////////////////////////////////////////////////////////////////////

template<class T>
void saveImg(const std::string name, SimpleImage<T> * img) {

   if (img == nullptr)
      return;

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








////////////////////////////////////////////////////////////////////////
// INTEGRAL IMAGE UTILITIES
////////////////////////////////////////////////////////////////////////



template<class T>
void computeIntegralImage(const T *in, int nbX, int nbY, double * out) {
  

  // First pixel
  *out = *in;

  // First row
  for (int x = 1; x < nbX; x++) {
     *out = *out++ + *++in;
  }

  // Next rows
  double *outp = (out - nbX);
  for (int y = 1; y < nbY; y++) {
   
      // First pixel of the current row
      double s = *++in;  // Current row cumulative sum
      *++out = *++outp + s;
      
      // Next pixels
      for (int x = 1; x < nbX; x++) {
         s = s + *++in;
         *++out = *++outp + s;
      }
   }
}




template<class T>
void computeIntegralImage(const T *in, int nbX, int nbY, double * out, double * out2) {

 
  // First pixel
  *out  = *in;
  *out2 = static_cast<double>(*in) * *in;

  // First row
  for (int x = 1; x < nbX; x++) {
     *out = *out++ + *++in;
     *out2 = *out2++ + static_cast<double>(*in) * *in;
  }


  // Next rows
  double *outp = (out - nbX);
  double *out2p = (out2 - nbX);
  for (int y = 1; y < nbY; y++) {
   
      // First pixel of the current row
      double s = *++in;  // Current row cumulative sum
      double s2 = static_cast<double>(*in) * *in;  // Current row cumul sum^2
      *++out = *++outp + s;
      *++out2 = *++out2p + s2;
      
      // Next pixels
      for (int x = 1; x < nbX; x++) {
         s = s + *++in;
         s2 = s2 + static_cast<double>(*in) * *in;
         *++out = *++outp + s;
         *++out2 = *++out2p + s2;
      }

   }

}




template<class T1, class T2>
void computeIntegralImage(const T1 *in1, const T2 *in2,  int nbX, int nbY, double * out) {
  

  // First pixel
  *out = static_cast<double>(*in1) * *in2;

  // First row
  for (int x = 1; x < nbX; x++) 
     *out = *out++ + static_cast<double>(*++in1) * *++in2;

  // Next rows
  double *outp = (out - nbX);
  for (int y = 1; y < nbY; y++) {
   
      // First pixel of the current row
      double s = static_cast<double>(*++in1) * *++in2;  // current row cumul sum
      *++out = *++outp + s;
      
      // Next pixels
      for (int x = 1; x < nbX; x++) {
         s = s + static_cast<double>(*++in1) * *++in2;
         *++out = *++outp + s;
      }
   }
}




template<class T1, class T2>
void computeIntegralImage(const T1 *in1, const T2 *in2,  int n1X, int n1Y, int n2X, int n2Y, int sX, int sY, 
                           double * out) {
 

  // Difference line stride between in1 and in2
  int d2 = n2X - n1X;

  // Go to start of in2 array
  T2 * in2_ = in2 + sY * n2X + sX; 


  // First pixel
  *out = static_cast<double>(*in1) * *in2_;

  // First row
  for (int x = 1; x < n1X; x++) 
     *out = *out++ + static_cast<double>(*++in1) * *++in2_;
  in2_ += d2;

  // Next rows
  double *outp = (out - n1X);
  for (int y = 1; y < n1Y; y++) {
   
      // First pixel of the current row
      double s = static_cast<double>(*++in1) * *++in2_;  // current row cumul sum
      *++out = *++outp + s;
      
      // Next pixels
      for (int x = 1; x < n1X; x++) {
         s = s + static_cast<double>(*++in1) * *++in2_;
         *++out = *++outp + s;
      }
      in2_ += d2;
   }
}



double sumIntegralImage(double * img, int ns, int nl, int sX, int sY, int nbX, int nbY) {

   double sum = img[(sY + nbY - 1) * ns + sX + nbX - 1];
   
   if (sX > 0) sum -= img[(sY + nbY - 1) * ns + sX - 1];  
   if (sY > 0) sum -= img[(sY - 1) * ns + sX + nbX - 1];  
   if (sX > 0 && sY > 0) sum += img[(sY - 1) * ns + sX - 1];  

//   double sum = img->get(sY+nbY-1, sX+nbX-1);
//   if (sX > 0) sum -= img->get(sY+nbY-1, sX-1);
//   if (sY > 0) sum -= img->get(sY-1, sX+nbX-1);
//   if (sX > 0 && sY > 0) sum += img->get(sY-1, sX-1);

   return sum;

}






////////////////////////////////////////////////////////////////////////
// IMAGE BICUBIC RESAMPLING
////////////////////////////////////////////////////////////////////////



template<class T>
int resampleImageBicubic(SimpleImage<T> *img, 
                         const std::valarray<float> &matX, 
                         const std::valarray<float> &matY,
                         SimpleImage<float> *out,
                         int omp_on) {  

   if (out == nullptr) {
      zvmessage("Output SimpleImage in resampleImageBicubic must be instanciated","");
      return 0;
   }

   if (out->getNL() * out->getNS() < matX.size()) {
      zvmessage("Output SimpleImage in resampleImageBicubic not large enough","");
      return 0;
   }

   // Bicubic resampling
   return  resampleImageBicubic<T>(img->linePtr(0), img->getNS(), img->getNL(), matX, matY, out->linePtr(0), omp_on);

}



// Bicubic resampling function based on resampling matrices. Resampling matrices
// is a 2-band image containing the (x,y) location in input img that we want to
// be resampled. The function returns the resampled image, whose size is ASSUMED
// equal to the x/y size of the resampling matrices.
template<class T>
int resampleImageBicubic(const T *img, const int nbX, const int nbY,
                         const std::valarray<float> &matX, 
                         const std::valarray<float> &matY, 
                         float *out, 
                         int omp_on) { 



   // Check that input resampling matrices have same number of elements
   if (matX.size() != matY.size()) {
      zvmessage("Bicubic resampling input mapping matrices have different length!","");
      return 0;
   }

   // If matrices are empty, nothing to do
   if (matX.size() == 0) { 
      zvmessage("Warning: Bicubic resampling matrices empty, nothing to do!","");
      return 1;
   }

   // Get number of pixel to resample
   int numSamples = matX.size();
  
   // Check that output array is allocated
   if (out == nullptr) {
      zvmessage("Error, output array NULL in resampleImageBicubic","");
      return 0;
   }

   // Iterate over the pixel and resample if matrices have valid values at that 
   // pixel
   #pragma omp parallel for if (omp_on)
   for (int i = 0 ; i < numSamples; i++) {

      float rsamp = matX[i];
      float rline = matY[i];

      // Check that x,y coordinates are not inf/nan/subnormal
      // If proper work is done before, that shouldn't happen, but if 
      // "non-normal" values slip through, it's a segfault
      if (!std::isnormal(rsamp) || !std::isnormal(rline)) 
         continue;
      

      // if out of bound matrice values, move to next pixel. Account for
      // bicubic resampling margin.
      if (rsamp < 1  || rsamp > (nbX-3) || rline < 1 || rline > (nbY-3)) 
         continue;
      

      int m= static_cast<int>(rsamp);
      int n= static_cast<int>(rline);
      float dec = rsamp - m;
      float dec2 = dec*dec;
      float dec3 = dec2*dec;
                        
      // Computing coefficients in one direction 
                                                   
      float f1_ = *(img + (n-1) * nbX + m-1);
      float f0  = *(img + (n-1) * nbX + m);
      float f1  = *(img + (n-1) * nbX + m+1);
      float f2  = *(img + (n-1) * nbX + m+2);
      float b1_ = 0.5 * (2*f0 + dec*(f1-f1_) + dec2*(2*f1_ - 5*f0 + 4*f1 - f2)
                  + dec3*(3*f0 - 3*f1 + f2 -f1_));

                                                      
      f1_ = *(img + n * nbX + m-1);
      f0  = *(img + n * nbX + m);
      f1  = *(img + n * nbX + m+1);
      f2  = *(img + n * nbX + m+2);
      float b0  = 0.5 * (2*f0 + dec*(f1-f1_) + dec2*(2*f1_ - 5*f0 + 4*f1 - f2)
                  + dec3*(3*f0 - 3*f1 + f2 -f1_));
                   
      f1_ = *(img + (n+1) * nbX + m-1);
      f0  = *(img + (n+1) * nbX + m);
      f1  = *(img + (n+1) * nbX + m+1);
      f2  = *(img + (n+1) * nbX + m+2);
      float b1 = 0.5 * (2*f0 + dec*(f1-f1_) + dec2*(2*f1_ - 5*f0 + 4*f1 - f2)
                 + dec3*(3*f0 - 3*f1 + f2 -f1_));
                                   
      f1_ = *(img + (n+2) * nbX + m-1);
      f0  = *(img + (n+2) * nbX + m);
      f1  = *(img + (n+2) * nbX + m+1);
      f2  = *(img + (n+2) * nbX + m+2);
      float b2 = 0.5 * (2*f0 + dec*(f1-f1_) + dec2*(2*f1_ - 5*f0 + 4*f1 - f2)
                 + dec3*(3*f0 - 3*f1 + f2 -f1_));
                                                      
                                                  
      // Then combining in the other direction 
                                                    
      dec = rline - n;
      float dn  = 0.5 * ( 2*b0 + dec*(b1-b1_) + dec*dec*(2*b1_ - 5*b0 + 4*b1 - b2)
                  + dec*dec*dec*(3*b0 - 3*b1 + b2 -b1_));
       
 
      // Saving resampled pixel
      out[i] = dn;

   }

   return 1;
}






////////////////////////////////////////////////////////////////////////
// LOW-PASS FILTER AND DOWNSAMPLING
////////////////////////////////////////////////////////////////////////


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




// Downsample the image with a power 2 factor (2,4,8...).
// A gaussian prefiltering is applied before resampling the image.
// Note filter+resampling could be done in one pass, but re-used pieces of code
// eleswhere in vicar, so it was easier implementation.
void downsample(SimpleImage<float> *imgIn, SimpleImage<float> *&imgOut,
                int downFactorX, int downFactorY, int omp) {

    // Check factors validity
    if (downFactorX < 1)
       downFactorX = 1;
    if (downFactorY < 1)
       downFactorY = 1;

    int ns = imgIn->getNS();
    int nl = imgIn->getNL();
    int nsNew = ns/downFactorX;
    int nlNew = nl/downFactorY;

    SimpleImage<float> *save_output = imgOut;

    // Copy input for inplace low-pass filtering
    SimpleImage<float> * imgBlur = new SimpleImage<float>(nl, ns);
    for (int l=0; l<nl; l++)       
       for (int s=0; s<ns; s++)
          imgBlur->set(l, s, imgIn->get(l,s));


    // Particular case where downFactor == 1, i.e. no downsampling, just a copy
    if (downFactorX <= 1 && downFactorY <= 1) {

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
    float sigmaX = 0.8 * sqrt(downFactorX * downFactorX - 1.0);
    float sigmaY = 0.8 * sqrt(downFactorY * downFactorY - 1.0);
    GaussianBlur(imgBlur, sigmaX, sigmaY, omp);

   
    // Second, decimate the low-passed image according to the decimation. Need
    // to interpolate values as "new" pixel is in the middle of the pixel 
    // "patch".

    // Compute where to resample the low-passed image
    std::valarray<float> matX(nlNew*nsNew);
    std::valarray<float> matY(nlNew*nsNew);
    for (int j=0; j<nlNew ; j++) {
       for (int i=0; i<nsNew ; i++) {
          matX[j*nsNew+i] = (0.5+i)*downFactorX-0.5;
          matY[j*nsNew+i] = (0.5+j)*downFactorY-0.5;
       }
    }    

    // Resample at proper location
    imgOut = new SimpleImage<float>(nlNew, nsNew);
    int status = resampleImageBicubic(imgBlur, matX, matY, imgOut, omp); 

    // No need anymore of the blurred image
    imgBlur->free();
    delete imgBlur;

    if (save_output != nullptr) {
       save_output->free();
       delete save_output;
    }

}






// Resample an image with a zoom/dezoom factor in X and Y
// factorX = 2, means downsampling by a factor of 2
// factorX = 0.5 means upsampling by a factor of 2
// A gaussian prefiltering is applied before resampling the image. In the case 
// downsampling, the gaussian filtering is sized accordingly. 
// Note filter+resampling could be done in one pass, but re-used pieces of code
// eleswhere in vicar, so it was easier implementation.
void resampleImage(SimpleImage<float> *imgIn, SimpleImage<float> *&imgOut,
                const float &factorX, const float &factorY, int omp) {

   
    int ns = imgIn->getNS();
    int nl = imgIn->getNL();
    int nsNew = static_cast<int>(ns/factorX + 0.5); //rounding
    int nlNew = static_cast<int>(nl/factorY + 0.5);

    SimpleImage<float> *save_output = imgOut;

    SimpleImage<float> *imgBlur = nullptr;

    // [1] Low-pass the image if downsampling. This is done using a gaussian 
    // filtering. In case of upsampling, no low pass filtering. 
    if (factorX > 1 || factorY > 1) {
   
       // Copy input for inplace low-pass filtering
       imgBlur = new SimpleImage<float>(nl, ns);
       for (int l=0; l<nl; l++)       
          for (int s=0; s<ns; s++)
             imgBlur->set(l, s, imgIn->get(l,s));

       // Get gaussian sigma - According to litterature, a sigma defined as:
       // sigma = Cst * SQRT(downFactor^2 - 1) with Cst ~= 0.8 is a good 
       // compromise for standard camera
       float stepX2 = (factorX < 1.01) ? 1.01 : factorX * factorX; 
       float stepY2 = (factorY < 1.01) ? 1.01 : factorY * factorY; 
       float sigmaX = 0.8 * sqrt(stepX2 - 1.0);
       float sigmaY = 0.8 * sqrt(stepY2 - 1.0);
       // Low pass with a Gaussian kernel
       GaussianBlur(imgBlur, sigmaX, sigmaY, omp);
    }
    else {
       imgBlur = new SimpleImage<float>(*imgIn, 0, 0, nl, ns);
    }
   
    // [2] Compute location of sampling locations

    std::valarray<float> matX(nlNew*nsNew);
    std::valarray<float> matY(nlNew*nsNew);
    // Account for the difference in pixel locations if upsampling or d
    // ownsampling.
    float offX = (factorX > 1) ? 0.5 * (factorX - 1) : 0;
    float offY = (factorY > 1) ? 0.5 * (factorY - 1) : 0;
    for (int j=0; j<nlNew ; j++) {
       for (int i=0; i<nsNew ; i++) {
          matX[j*nsNew+i] = i * factorX + offX;
          matY[j*nsNew+i] = j * factorY + offY;
       }
    }    

        
    // [3] Bicubic resampling  at proper location
    imgOut = new SimpleImage<float> (nlNew, nsNew);
    resampleImageBicubic(imgBlur, matX, matY, imgOut, omp); 


    // [4] House keeping
    // No need anymore of the blurred image
    imgBlur->free();
    delete imgBlur;
    // Free input if in-place resampling
    if (save_output != nullptr) {
       save_output->free();
       delete save_output;
    }

}




////////////////////////////////////////////////////////////////////////
// VOID FILL
////////////////////////////////////////////////////////////////////////

void convexHull(const std::vector<std::array<int,2> > &inputPoints, 
                      std::vector<std::array<int,2> > &outputPoints) {

   using ptType = std::array<int, 2>;

   // Check if enough points to compute a convex Hull
   if (inputPoints.size() < 3)
      return;

   // Clear out output container
   outputPoints.clear();

   // Working copy of the input list
   auto points = inputPoints;

   // Find the most bottom-left point in the input set
   auto it = std::max_element(begin(points), end(points),
                 [](const ptType &x1, const ptType &x2) {
                 return (x1[1] < x2[1] || (x1[1] == x2[1] && x1[0] < x2[0]));});


   // That's our first point in the ouput list. All the other points will be 
   // compared to this first point
   // Store it in a std::list. We use a list here for faster removal of 
   // intermediate points later on in the process
   auto p0 = *it;
   points.erase(it);
   points.insert(begin(points),p0);


   // Sort all the other points by polar angle w.r.t. first point

   // Define function returning if angle between p0/p and p0/q is 
   // clock or counterclock wise
   auto orientation = [](const ptType &x1, const ptType &x2, const ptType &x3) {
      int val = (x2[1] - x1[1]) * (x3[0] - x2[0]) - (x2[0] - x1[0]) * (x3[1] - x2[1]);
      if (val == 0)
         return 0; // points collinear
      return (val > 0) ? 1 : 2; // clock or counterclock wise
   };

   // Function to compare polar angle, ordering points from closest to farthest
   // in case polar angle is identical
   auto polarCompare = [&](const ptType &x1, const ptType &x2) {
      int o = orientation(p0, x1, x2);
      if (o == 0)
         return ( (std::pow(p0[0]-x1[0],2) + std::pow(p0[1]-x1[1],2)) <= 
                  (std::pow(p0[0]-x2[0],2) + std::pow(p0[1]-x2[1],2)) ) ? 1 : 0;
      return (o == 2) ? 0 : 1;
   };  

   // Sort list of points based on their polar angle w.r.t first point.
   std::sort(begin(points) + 1, end(points), polarCompare);


   // If two or more points make same angle with p0, remove all but the one that
   // is farthest from p0. Remember that, in above sorting, our criteria was to 
   // keep the farthest point at the end when more than one points have same 
   // angle.
   int m = 1; // Initialize size of modified array
   for (int i = 1; i < points.size(); i++) {
   
      // Keep removing i while angle of i and i+1 is same with respect to p0
      while ( (i < points.size()-1) && (orientation(p0, points[i], points[i+1]) == 0) )
         i++;
 
      points[m] = points[i];
      m++;  // Update size of modified array
   }


   // If modified array of points has less than 3 points, convex hull is not 
   // possible.
   if (m < 3) return;

   // Store the first three points into the output array
   outputPoints.push_back(points[0]);
   outputPoints.push_back(points[1]);
   outputPoints.push_back(points[2]);

   // Process remaining n-3 points
   for (int i = 3; i < m; i++) {
      // Keep removing top while the angle formed by points next-to-top, top, 
      // and points[i] makes a non-left turn
      while ( (points.size() > 1) && 
              (orientation(outputPoints[outputPoints.size()-2], outputPoints.back(), points[i]) != 1) )
         outputPoints.pop_back();

      outputPoints.push_back(points[i]);
   }
  

}





template<class T>
void invDistFill(SimpleImage<T> *imgIn, SimpleImage<float> *&imgOut,
                 const int minCoverage, const int power, 
                 const int border) {


   int nb = imgIn->getNB();
   int ns = imgIn->getNS();
   int nl = imgIn->getNL();


   // Allocate output image
   SimpleImage<float> *save_output = imgOut;
   imgOut = new SimpleImage<float>(nb, nl, ns);
   imgOut->zero();

   // Allocate a "boolean" map to locate valid(1)/invalid(0) pixel
   SimpleImage<int> mapValid(nl, ns);
   mapValid.zero();

   // If border is positive, need to compute the "pseudo convex hull" of the 
   // non null values. We assume, for multi-band inputs, that non null pixel
   // are common to all bands
   // The pseudo-convex hull is, for each line, the samples of the first and 
   // last non-null pixel.
   std::vector<std::array<int,2> > limitX(nl, (border != 0) ? 
                                               std::array<int,2>{{-1,-1}} : 
                                               std::array<int,2>{{0,ns-1}});

   // Iterate over the input image and copy the non-null pixels to the output.
   // These pixels are not modified. While we're iterating over the input 
   // image, fill up the "boolean" map and pseudo-convex hull if needed
   for (int k = 0; k < nb ; k++) {

      for (int j = 0; j < nl ; j++) {
   
         for (int i = 0; i < ns ; i++) {
     
            T val = imgIn->get(k,j,i);

            if (val != 0) 
               // Copy valid input pixel to output
               imgOut->set(k,j,i, static_cast<float>(val));
            else          
               continue;
            

            // Additional coverage info retrieval based on the first band only
            if (k == 0) {

               // Flag (x,y) location as valid
               mapValid.set(j, i, 1);

               // Save first/last non-null pixel of the given line if using
               // convex hull
               if (border != 0) {
                  if (limitX[j][0] == -1) 
                     limitX[j][0] = i;
                  limitX[j][1] = i;
               }

            }   

         }

      }

   }



// FOR DEBUG
//SimpleImage<int> * oo = new SimpleImage<int>(3,nl,ns);
//for (int k = 0; k < 3; k++)
//   for (int j = 0; j < nl; j++)
//      for (int i = 0; i < ns ; i++)
//         oo->set(k,j,i,mapValid.get(j,i));
//
//for (int j=0; j < nl; j++) {
//   if (limitX[j][0] == -1) 
//      continue;
//   oo->set(0,j,limitX[j][0], 255);
//   oo->set(1,j,limitX[j][0], 0);
//   oo->set(2,j,limitX[j][0], 0);
//   oo->set(0,j,limitX[j][1], 0);
//   oo->set(1,j,limitX[j][1], 255);
//   oo->set(2,j,limitX[j][1], 0);
//}


   // Compute the convex hull of the valid pixel using the pseudo one computed
   // above - Update limitX accordingly
   // This is most likely not efficient, but will do for now
   if (border) {

      // Reformat the pseudo convex-hull into a list of points suitable for the
      // convex hull function. Huh! :(
      std::vector<std::array<int,2> > inp, out;
      for (int y = 0; y < nl; y++) {
         if (limitX[y][0] != -1) {
            inp.push_back(std::array<int,2>{{limitX[y][0], y}});
            inp.push_back(std::array<int,2>{{limitX[y][1], y}});
         }
      }
      

      // Compute convex hull
      convexHull(inp, out);


      // Update the min/max sample coordinates (for each line) for the pixels
      // that needs to be filled, if they are null.

      // Add first element to last, for cycling all the way to the begining
      out.push_back(out.front());

      // Iterate over the points describing the convex hull and update the X
      // limit for each pixel between two successive points.
      // Note that first point in the convex hull function is the bottom-left
      // most points and the following points go counterclock wise.
      for (int i = 0; i < out.size()-1; i++) {
         auto current = out[i];
         auto next    = out[i+1];

         // If same Y, then the Xmin,Xmax are simply the X coord of the two 
         // points.
         if (current[1] == next[1]) {
            limitX[current[1]][0] = current[0];
            limitX[current[1]][1] = next[0];
         }
         // If Y is decreasing, we're on the "right" side of the convex hull and
         // are computing the maxX of the pixel
         if (current[1] > next[1]) {
            if (current[0] == next[0]) {
               for (int j = current[1]; j > next[1]; j--)
                  limitX[j][1] = current[0];
            }
            else {
               float a = static_cast<float>(next[0]-current[0]) / (next[1] - current[1]);
               float b = current[0] - a * current[1];
               for (int j = current[1]; j > next[1]; j--) {
                  int limit = static_cast<int>(a * j + b + 0.5);
                  limitX[j][1] = limit > ns-1 ? ns-1 : limit;
               } 
            }
         }
         // If Y is increasing, we're on the "left" side of the convex hull and
         // are computing the minX of the pixel
         else {
            if (current[0] == next[0]) {
               for (int j = current[1]; j < next[1]; j++)
                  limitX[j][0] = current[0];
            }
            else {
               float a = static_cast<float>(next[0]-current[0]) / (next[1] - current[1]);
               float b = current[0] - a * current[1];
               for (int j = current[1]; j < next[1]; j++) {
                  int limit = static_cast<int>(a * j + b + 0.5);
                  limitX[j][0] = limit > ns-1 ? ns-1 : limit;
               } 
            }
         }
      }

//FOR DEBUG
//for (int j=0; j < limitX.size(); j++) {
//   if (limitX[j][0] == -1) continue;
//  oo->set(0,j,limitX[j][0], 0);
//   oo->set(1,j,limitX[j][0], 0);
//   oo->set(2,j,limitX[j][0], 255);
//   oo->set(0,j,limitX[j][1], 0);
//   oo->set(1,j,limitX[j][1], 0);
//   oo->set(2,j,limitX[j][1], 255);
//}
//saveImg("debug.vic", oo);
////zabend();

   }






   // Precompute the inverse distance to the power (i.e., 1/dist^k)
   // In theory the kernel could go up to the full image size. This is
   // overkill but simplify things
   SimpleImage<double> invDist(nl % 2 ? nl : nl + 1, ns % 2 ? ns : ns + 1);
   invDist.zero();
   int hx = (invDist.getNS() - 1) / 2;
   int hy = (invDist.getNL() - 1) / 2;
   for (int j = 0; j < invDist.getNL(); j++) {
      for (int i = 0; i < invDist.getNS(); i++) {
         if (i != hx || j != hy)
            invDist.set(j,i, 1. / std::pow(std::sqrt(std::pow(i-hx,2) + std::pow(j-hy,2)),power));
      }
   }  



   // Compute the integral image of the flag map. That will be used to 
   // quickly count the number of valid pixel accounted for during the
   // interpolation
   SimpleImage<double> mapValidI(nl, ns);
   computeIntegralImage(&mapValid, &mapValidI);

   float minPercent = static_cast<float>(minCoverage) / 100;

   for (int j = 0; j < limitX.size(); j++) {

      for (int i = limitX[j][0]; i < limitX[j][1]; i++) {

         if (mapValid.get(j, i) == 1)
            continue;

 
         int enoughVal = 0;
         int h = 1;
         int sx, sy, n;
         while (1) {
            sx = i-h;
            sy = j-h;
            n = 2 * h + 1;


            if (!mapValid.inBounds(sy,sx) || !mapValid.inBounds(sy+n-1, sx+n-1))
               break;
            
            double tot = sumIntegralImage(&mapValidI, sx, sy, n, n);
            if (tot >= minPercent * (n * n - 1)) {     // -1 to remove center pixel from count
               enoughVal = 1; 
               break;
            }
            h++;
         }

         // Interpolate if neighborood has enough valid pixel
         if (enoughVal) {

            for (int k = 0; k < nb; k++) {

               double sumVal = 0;
               double sumWeight = 0;

               for (int y = 0; y < n; y++) {
                  for (int x = 0; x < n; x++) {
                     int valid = mapValid.get(y+sy,x+sx);
                     if (valid) {
                        float val = static_cast<float>(imgIn->get(k, y+sy, x+sx));
                        sumVal += val * invDist.get(y-h + hy, x-h + hx);
                        sumWeight += invDist.get(y-h + hy, x-h + hx);
                     }
                  }
               }
               
               imgOut->set(k, j, i, sumVal/sumWeight);
            }

         }
         

      }

   }

   mapValid.free();
   mapValidI.free();


   // Free input if in-place resampling
   if (save_output != nullptr) {
      save_output->free();
      delete save_output;
   }

}






// Template Specialization
// To be extented as needed


template void computeIntegralImage<const int>(const int *in, int nbX, int nbY, double * out);
template void computeIntegralImage<const float>(const float *in, int nbX, int nbY, double * out);
template void computeIntegralImage<const double>(const double *in, int nbX, int nbY, double * out);
template void computeIntegralImage<const int>(const int *in, int nbX, int nbY, double * out, double * out2);
template void computeIntegralImage<const float>(const float *in, int nbX, int nbY, double * out, double * out2);
template void computeIntegralImage<const int, const int>(const int *in1, const int *in2,  int nbX, int nbY, double * out);
template void computeIntegralImage<const float, const float>(const float *in1, const float *in2,  int nbX, int nbY, double * out);
template void computeIntegralImage<const int, const int>(const int *in1, const int *in2,  int n1X, int n1Y, int n2X, int n2Y, int sX, int sY, double * out);
template void computeIntegralImage<const float, const float>(const float *in1, const float *in2,  int n1X, int n1Y, int n2X, int n2Y, int sX, int sY, double * out);
template void computeIntegralImage<const int>(SimpleImage<const int> *in, SimpleImage<double> *out);
template void computeIntegralImage<const int>(SimpleImage<const int> *in, SimpleImage<double> *out, SimpleImage<double> *out2);
template void computeIntegralImage<const int, const int>(SimpleImage<const int> *in1, SimpleImage<const int> *in2, SimpleImage<double> *out);
template void computeIntegralImage<const int, const int>(SimpleImage<const int> *in1, SimpleImage<const int> *in2, int sX, int sY, SimpleImage<double> *out);

template int resampleImageBicubic<const float>(const float * img, const int ns, const int nl, const std::valarray<float> & matX, const std::valarray<float> &matY, float * out, int omp = 0);
template int resampleImageBicubic<const int>(const int * img, const int ns, const int nl, const std::valarray<float> & matX, const std::valarray<float> &matY, float * out, int omp = 0);


template int  resampleImageBicubic<float>(SimpleImage<float> * in, const std::valarray<float> & matX, const std::valarray<float> &matY, SimpleImage<float> * out, int omp = 0);
template int  resampleImageBicubic<int>(SimpleImage<int> * in, const std::valarray<float> & matX, const std::valarray<float> &matY, SimpleImage<float> * out, int omp = 0);


template void invDistFill<float>(SimpleImage<float> *inImg, SimpleImage<float> *&imgOut, const int minCoverage, const int power = 1, const int border=0);

template void saveImg<float>(const std::string name, SimpleImage<float> *img);
template void saveImg<double>(const std::string name, SimpleImage<double> *img);
template void saveImg<char>(const std::string name, SimpleImage<char> *img);
template void saveImg<int>(const std::string name, SimpleImage<int> *img);

