// The following code is derived from the below copyright holder.
// Steps have been taken to insure an authorized use of the 
// patent involved in this algorithm
// JPL-Caltech - 02/2018
//
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
//*------------------------ compute_asift_keypoints -------------------------*/
// Compute the ASIFT keypoints on the input image. 
// 
// Please report bugs or send comments to Guoshen Yu yu@cmap.polytechnique.fr
// 
// Reference: J.M. Morel and G.Yu, ASIFT: A New Framework for Fully Affine 
//            Invariant Image Comparison, SIAM Journal on Imaging Sciences, 
//            vol. 2, issue 2, pp. 438-469, 2009. 
// Reference: ASIFT online demo (You can try ASIFT with your own images online.)
//            http://www.ipol.im/pub/algo/my_affine_sift/
/*---------------------------------------------------------------------------*/

#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include "compute_asift_keypoints.h"


// Convolution of buffer (one line or column of the image) with the gaussian
// kernel. This is a time intensive routine in keypoint detection, so deserves 
// careful attention to efficiency.  Loop unrolling simply sums 5 
// multiplications at a time to allow the compiler to schedule operations better
// and avoid loop overhead.  
void ConvBufferFast(float *buffer, float *kernel, int rsize, int ksize)
{
  int i;
  float *bp, *kp, *endkp;
  float sum;

  for (i = 0; i < rsize; i++) {
    sum = 0.0;
    bp = &buffer[i];
    kp = &kernel[0];
    endkp = &kernel[ksize];

    // Loop unrolling: do 5 multiplications at a time.
    while (kp + 4 < endkp) {
       sum += (double) (bp[0]*kp[0] + bp[1]*kp[1] + bp[2]*kp[2] + bp[3]*kp[3] +
                        bp[4]*kp[4]);
       bp += 5;
       kp += 5;		  
    }
    
    // Do 2 multiplications at a time on remaining items.
    while (kp + 1 < endkp) {
       sum += (double) (bp[0]*kp[0] + bp[1]*kp[1]);
       bp += 2;
       kp += 2;
    }
    
    // Finish last one if needed.
    if (kp < endkp) {
       sum += (double)( (*bp) * (*kp) );
    }

    //while (kp < endkp) {
    //  sum += *bp++ * *kp++;			  
    //}

    buffer[i] = sum;
  }
}




// Convolve image with the 1-D kernel vector along image rows.  
// Pixels outside the image are set to the value of the closest image pixel.
void ConvHorizontal(vector<float>& image, int width, int height, 
                    float *kernel, int ksize)
{
  int r, c, i, halfsize;
  float buffer[width+ksize];
  halfsize = ksize / 2;

  for (r = 0; r < height; r++) {
    // Copy the row into buffer with pixels at ends replicated for
    // half the mask size.  This avoids need to check for ends
    // within inner loop. 
    for (i = 0; i < halfsize; i++)
      buffer[i] = image[r*width];
    for (i = 0; i < width; i++)
      buffer[halfsize + i] = image[r*width+i];
    for (i = 0; i < halfsize; i++)
      buffer[halfsize + width + i] = image[r*width+width-1];

    ConvBufferFast(buffer, kernel, width, ksize);
    for (c = 0; c < width; c++)
      image[r*width+c] = buffer[c];
  }
}




// Same as ConvHorizontal, but apply to vertical columns of image.
void ConvVertical(vector<float>& image, int width, int height, 
                  float *kernel, int ksize)
{
  int r, c, i, halfsize;
  float buffer[height+ksize];
  halfsize = ksize / 2;

  for (c = 0; c < width; c++) {
    for (i = 0; i < halfsize; i++)
      buffer[i] = image[c];
    for (i = 0; i < height; i++)
      buffer[halfsize + i] = image[i*width+c];
    for (i = 0; i < halfsize; i++)
      buffer[halfsize + height + i] = image[(height - 1)*width+c];

    ConvBufferFast(buffer, kernel, height, ksize);
    for (r = 0; r < height; r++)
      image[r*width+c] = buffer[r];
  }
}



// 1D Convolve image with a Gaussian of width sigma and store result back
// in image. This routine creates the Gaussian kernel, and then applies
// it in horizontal (flag_dir=0) OR vertical directions (flag_dir!=0).
void GaussianBlur1D(vector<float>& image, int width, int height, float sigma, 
                    int flag_dir)
{
  float x, kernel[MaxKernelSz], sum = 0.0;
  int ksize, i;

  // The Gaussian kernel is truncated at GaussTruncate sigmas from
  // center. The kernel size should be odd.
  ksize = (int)(2.0 * GaussTruncate1 * sigma + 1.0);
  ksize = MAX(3, ksize);    // Kernel must be at least 3.
  if (ksize % 2 == 0)       // Make kernel size odd.
    ksize++;
  assert(ksize < MaxKernelSz);

  // Fill in kernel values.
  for (i = 0; i <= ksize; i++) {
    x = i - ksize / 2;
    kernel[i] = exp(- x * x / (2.0 * sigma * sigma));
    sum += kernel[i];
  }
  // Normalize kernel values to sum to 1.0.
  for (i = 0; i < ksize; i++)
    kernel[i] /= sum;

  if (flag_dir == 0) 
    ConvHorizontal(image, width, height, kernel, ksize);
  else 
    ConvVertical(image, width, height, kernel, ksize);
}


// Function to express the pixel coordinates of the keypoints found in an affine
// transformed image back into the original image coordinate system.
void compensate_affine_coor(float *x0, float *y0, int w1, int h1, float t1, 
                             float t2, float Rtheta)
{
   float x_ori, y_ori;	
   float x_tmp, y_tmp;

   float x1 = *x0;
   float y1 = *y0;

   Rtheta = Rtheta*PI/180;

   if ( Rtheta <= PI/2 ) {
      x_ori = 0;
      y_ori = w1 * sin(Rtheta) / t1;
   }
   else {
      x_ori = -w1 * cos(Rtheta) / t2;
      y_ori = ( w1 * sin(Rtheta) + h1 * sin(Rtheta-PI/2) ) / t1;
   }

   float sin_Rtheta = sin(Rtheta);
   float cos_Rtheta = cos(Rtheta);

   // project the coordinates of im1 to original image before tilt-rotation 
   // transform 
   // Get the coordinates with respect to the 'origin' of the original image 
   // before transform 
   x1 = x1 - x_ori;
   y1 = y1 - y_ori;
   // Invert tilt 
   x1 = x1 * t2;
   y1 = y1 * t1;
   // Invert rotation. Note that the y direction (vertical) is inverse to the 
   // usual concention. Hence Rtheta instead of -Rtheta to inverse the rotation.
   x_tmp = cos_Rtheta*x1 - sin_Rtheta*y1;
   y_tmp = sin_Rtheta*x1 + cos_Rtheta*y1;
   x1 = x_tmp;
   y1 = y_tmp;		

   *x0 = x1;
   *y0 = y1;
}


// MAIN FUNCTION

// Compute ASIFT keypoints in the input image.
// Output: the number of keypoints 
int compute_asift_keypoints(vector<float> &image, int width, int height, 
                            float tilt, float theta, 
                            keypointslist &keypointlist, 
                            siftPar &siftparameters)
{
	
   vector<float> image_t, image_tmp;	
   float BorderFact=6*sqrt(2.);


   // Affine simulation (rotation+tilt simulation) 
   if (tilt == 1) { // no affine transform needed -  Plain original image 

      float *imageArr = new float[width*height];
      for (int pix = 0; pix < width*height; pix++)
         imageArr[pix] = image[pix];

      // Get SIFT keypoints
      compute_sift_keypoints(imageArr, keypointlist, width, height, 
                             siftparameters);

      delete[] imageArr;
   }
   else {
      vector<float> image_t;
      int width_r, height_r;

      // simulate a rotation: rotate the image with an angle theta.  The outside
      // of the rotated image are padded with the value 0.0.
      imgRotation(image, image_t, width, height, &width_r, &height_r, &theta, 0.0);

      // Get new dimensions			 
      int width_t = (int) (width_r);
      int height_t = (int) (height_r / tilt);  

      // Anti-aliasing filtering along vertical direction
      float sigma_aa = siftparameters.InitSigma * tilt / 2;
      GaussianBlur1D(image_t, width_r, height_r, sigma_aa, 1);

      // Simulate a tilt: subsample the image along the vertical axis by a 
      // factor of t.
      vector<float> image_tmp(width_t*height_t);			 
      imgResample(image_t, image_tmp, width_r, height_r, width_t, height_t); 


      // copy rotated/tilted image in an array
      // SIFT code comes from tiers implementation that uses array for image.
      float *imageArr = new float[width_t*height_t];
      for (int pix = 0; pix < width_t*height_t; pix++)
         imageArr[pix] = image_tmp[pix];	 

      // compute SIFT keypoints on simulated image. 	 
      compute_sift_keypoints(imageArr, keypointlist, width_t, height_t, 
                             siftparameters);
      delete[] imageArr;		


      // Check if the keypoint is located on the boundary of the parallelogram 
      // ,i.e., the boundary of the distorted input image. If so, remove it to 
      // avoid edge artifacts. 
      for (int i=keypointlist.size()-1;i>=0; i--) {		      

         float x0, y0, x1, y1, x2, y2, x3, y3 ,x4, y4, d1, d2, d3, d4, 
               scale1, theta1, sin_theta1, cos_theta1, BorderTh;

         x0 = keypointlist[i].x;
         y0 = keypointlist[i].y;
         scale1 = keypointlist[i].scale;

         theta1 = theta * PI / 180;
         sin_theta1 = sin(theta1);
         cos_theta1 = cos(theta1);

         // the coordinates of the 4 summits of the parallelogram
         if (theta <= 90) {
            x1 = height * sin_theta1;
            y1 = 0;			 
            y2 = width * sin_theta1;
            x3 = width * cos_theta1;
            x4 = 0;
            y4 = height * cos_theta1;
            x2 = x1 + x3;
            y3 = y2 + y4;

            // Warning: vertical direction goes from top to bottom!!! 
            // The calculation above assumes that the vertical direction 
            // goes from the bottom to top. Thus the vertical coordinates 
            // need to be reversed.
            y1 = y3 - y1;
            y2 = y3 - y2;
            y4 = y3 - y4;
            y3 = 0;

            y1 = y1 / tilt; 
            y2 = y2 / tilt;
            y3 = y3 / tilt;
            y4 = y4 / tilt;
         }
         else {
            y1 = -height * cos_theta1;
            x2 = height * sin_theta1;
            x3 = 0;
            y3 = width * sin_theta1;				 
            x4 = -width * cos_theta1;
            y4 = 0;
            x1 = x2 + x4;
            y2 = y1 + y3;

            // Warning: vertical direction goes from top to bottom!!! 
            y1 = y2 - y1;
            y3 = y2 - y3;
            y4 = y2 - y4;
            y2 = 0;

            y1 = y1 / tilt;
            y2 = y2 / tilt;
            y3 = y3 / tilt;
            y4 = y4 / tilt;
         }		       		    

         // the distances from the keypoint to the 4 sides of the 
         // parallelogram 
         d1 = ABS((x2-x1)*(y1-y0)-(x1-x0)*(y2-y1)) / 
              sqrt((x2-x1)*(x2-x1)+(y2-y1)*(y2-y1));
         d2 = ABS((x3-x2)*(y2-y0)-(x2-x0)*(y3-y2)) / 
              sqrt((x3-x2)*(x3-x2)+(y3-y2)*(y3-y2));
         d3 = ABS((x4-x3)*(y3-y0)-(x3-x0)*(y4-y3)) / 
              sqrt((x4-x3)*(x4-x3)+(y4-y3)*(y4-y3));
         d4 = ABS((x1-x4)*(y4-y0)-(x4-x0)*(y1-y4)) / 
              sqrt((x1-x4)*(x1-x4)+(y1-y4)*(y1-y4));

         BorderTh = BorderFact*scale1;

         if (!((d1<BorderTh) || (d2<BorderTh) || (d3<BorderTh) || 
               (d4<BorderTh) )) {
            // Normalize the coordinates of the matched points to the 
            // original image frame coordinate system (compensate for the
            // affine transform)
            compensate_affine_coor(&x0, &y0, width, height, tilt, 1, theta);
            keypointlist[i].x = x0;
            keypointlist[i].y = y0;
            keypointlist[i].x_ori = x0;
            keypointlist[i].y_ori = y0;
         }
         else
            keypointlist.erase(keypointlist.begin()+i);
      }
   }
  return keypointlist.size();;
}

