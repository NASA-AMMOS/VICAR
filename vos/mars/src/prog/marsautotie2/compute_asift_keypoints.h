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

#include "sift_library.h"
#include "demo_lib_sift.h"
#include <vector>

#define ABS(x)    (((x) > 0) ? (x) : (-(x)))
#define round(x) ((x)>=0?(long)((x)+0.5):(long)((x)-0.5))

// Gaussian convolution kernels are truncated at this many sigmas from
// the center.  While it is more efficient to keep this value small,
// experiments show that for consistent scale-space analysis it needs
// a value of about 3.0, at which point the Gaussian has fallen to
// only 1% of its central value.  A value of 2.0 greatly reduces
// keypoint consistency, and a value of 4.0 is better than 3.0.
const float GaussTruncate1 = 4.0;

// Max size of the Gaussian kernel footprint. It is computed
// as 2*Gausstruncate1*sigma +1.  
const int MaxKernelSz = 100;


using namespace std;


struct affImg {
   int imgId;
   vector<int> TRlist;
};



void ConvBufferFast(float *buffer, float *kernel, int rsize, int ksize);
void ConvHorizontal(vector<float>& image, int width, int height, float *kernel, 
                    int ksize);
void ConvVertical(vector<float>& image, int width, int height, float *kernel, 
                  int ksize);
void GaussianBlur1D(vector<float>& image, int width, int height, float sigma, 
                    int flag_dir);
void compensate_affine_coor(float *x0, float *y0, int w1, int h1, float t1, 
                             float t2, float Rtheta);

int compute_asift_keypoints(vector<float> &image, int width, int height, 
                            float tilt, float theta, 
                            keypointslist &keypointlist, 
                            siftPar &siftparameters);

