#include "vicmain_c"
#include <cmath>
#include <vector>
#include <numeric>
#include <random>
#include <algorithm>
#include "SimpleImage.h"
#include "imgIO.h"
#include "rbfFit.h"


////////////////////////////////////////////////////////////////////////

std::vector<long> getControlPoints(std::vector<long> &validLoc,
                                   int ns, int nl,
                                   int numControl,
                                   int skip);


void drawControlPoints(SimpleImage<float> *im, std::vector<float> &xarr, std::vector<float> &yarr,
                      float noVal, SimpleImage<int> * out);


////////////////////////////////////////////////////////////////////////


void main44()
{

    SimpleImage<float> * img = nullptr;
    SimpleImage<float> * out = nullptr;

    // Load input image
    readCmdImg("INP", img);

    // Get images dimensions
    int ns = img->getNS();
    int nl = img->getNL();
    int nb = img->getNB();

    // Get a specific band to process, if any. Otherwise do all of them.
    int band, count;
    zvp("BAND", &band, &count);
    if (band > nb) {
       zvmessage("Band requested is > to nb of bands. All of them will be processed","");
       band = 0;
    }

    // Get the subsetting parameter to defines the control point list.

    int skip = 0;
    zvp("SKIP", &skip, &count);
    int numControl = 0;
    zvp("NUM_CONTROL", &numControl, &count);
    int density = 0;
    zvp("DENSITY", &density, &count);
    if (skip <= 0 && numControl <= 0 && density <= 0) {
       zvmessage("SKIP, DENSITY  and NUM_CONTROL cannot be all null","");
       zabend();
    }

    // Get the type of RBF to apply
    RBFMode rbfMode;
    if(zvptst("LINEAR")) 
       rbfMode = LINEAR;
    else if (zvptst("CUBIC")) 
       rbfMode = CUBIC;
    else if (zvptst("QUINTIC"))
       rbfMode = QUINTIC;
    else if (zvptst("THIN_PLATE"))
       rbfMode = THIN_PLATE;
    else {
        zvmessage("RBF mode is undefined.\n", "");
        zabend();
    }

    // Get the no value, i.e., values that are considered NaN and which are not
    // taken into consideration for the surface fit and correction
    float noVal;
    zvp("NOVAL", &noVal, &count);


    // Get the smoothing regularization. Goes from 0 (no smoothing at all, the
    // RBF function goes through the control points exactly) to infinity (fit
    // a plane essentially).
    float smoothing;
    zvp("SMOOTHING", &smoothing, &count);

    // Check if the output surface needs to be computed for every pixel or only
    // the for the invalid ones 
    int fill = 0;
    fill = zvptst("FILL");

    int independent = 0;
    independent = zvptst("INDEPENDENT");

    int saveCPdisplay = 0;
    zvpcnt("CP_OUT", &saveCPdisplay);



    // Multithreading?
    int omp = zvptst("OMP_ON");

    

    // Get number of band to process. 1-based. If 0, that means that all bands
    // of the image are to be processed
    int bp = (band == 0 ? nb : 1);

    // Get band indices to process
    std::vector<int> bIndices(bp);
    if (band == 0)
       std::iota(begin(bIndices), end(bIndices), 0);
    else
       bIndices[0] = band - 1;

    // Initialize output container
    out = new SimpleImage<float>(bp, nl, ns);
    out->zero();

    // Print some input information
    int msgLen = 150;
    char msg[msgLen];
    snprintf(msg, msgLen, "Input image (nb x nl x ns): %d x %d x %d", nb, nl, ns);
    zvmessage(msg, "");

    if (band == 0)
       snprintf(msg, msgLen, "Band processed: All");
    else  
       snprintf(msg, msgLen, "Band processed: %d", bIndices[0]+1);
    zvmessage(msg, "");

    if (band == 0 && independent == 1) {
       snprintf(msg, msgLen, "Bands processed with independent control points selection");
       zvmessage(msg, "");
    }

    snprintf(msg, msgLen, "X/Y pixels skip: %d", skip);
    zvmessage(msg, "");

    snprintf(msg, msgLen, "Control points density: %d", density);
    zvmessage(msg, "");

    snprintf(msg, msgLen, "Max number of control points: %d", numControl);
    zvmessage(msg, "");

    snprintf(msg, msgLen, "Ignored value: %7.3f", noVal);
    zvmessage(msg, "");

    snprintf(msg, msgLen, "Smoothing scalar: %7.3f", smoothing);
    zvmessage(msg, "");

    std::vector<float> xControlArr, yControlArr, zControlArr;
    std::vector<double> coefs;



    // Start processing, sequentially on each band.
    for (int b=0; b<bIndices.size(); b++) {

       zvmessage("", "");
       snprintf(msg, msgLen,"Processing band %d", bIndices[b]+1);
       zvmessage(msg, "");


       // Extract the pixels that will be used for the surface fit definition
       int validPixels = 0;
       if (b == 0 || independent == 1) {

          // Get pixels indices with valid values
          std::vector<long> validLoc;
          auto startPtr = img->linePtr(bIndices[b], 0);
          for (long i=0; i<nl*ns; i++) {
             if (startPtr[i] != noVal)
                validLoc.push_back(i);
          }
          validPixels = validLoc.size();

          // Check if the number of control points to retrieve must be adjusted
          // based on user supplied density.
          if (density) {
             int val = validPixels / density;
             if (val < numControl)
                numControl = val;
          }

          // Extract control points from the valid pixels list
          std::vector<long> cpArr = getControlPoints(validLoc, ns, nl, numControl, skip);

          // Reformat control points as x/y/z value 
          // Clear containers for new band
          xControlArr.clear();
          yControlArr.clear();
          for (auto i: cpArr) {
             xControlArr.push_back(i%ns);
             yControlArr.push_back(i/ns);
          }
       }

       // Get the z values of the control points for the current band
       zControlArr.clear();
       for (int i = 0; i < xControlArr.size(); i++)
          zControlArr.push_back(img->get(bIndices[b], yControlArr[i], xControlArr[i]));



       snprintf(msg, msgLen, "Number of valid pixels: %d", validPixels);
       zvmessage(msg,"");
       snprintf(msg, msgLen, "Number of control points: %d", xControlArr.size());
       zvmessage(msg,"");

       // Compute the RBF coefficients
       zvmessage("Computing RBF...","");
       coefs.clear();

       int status = computeRBF(xControlArr, yControlArr, zControlArr, smoothing, rbfMode, coefs);

       if (status) {
          zvmessage("RBF function computation failed.","");
          zabend();
       }


       // Get the x/y where to compute the interpolated z. Could be either 
       // where there's invalid data or on the full image. Depends on user 
       // input.
       std::vector<float> xArr, yArr;
       for (int j=0; j<nl; j++) {
          for (int i=0; i<ns; i++) {
             float val = img->get(bIndices[b], j, i);
             if (val != noVal && fill) {
                out->set(b, j, i, val);
                continue;
             }
             xArr.push_back(i);
             yArr.push_back(j);
          }
       }

       // Apply RBF to these locations
       zvmessage("Applying RBF...","");
       std::vector<double> zArr;

       status = applyRBF(xArr, yArr, xControlArr, yControlArr, coefs, rbfMode, 
                         omp, zArr);

       // Save computed values to output file
       for (int i=0; i<xArr.size(); i++)
          out->set(b, yArr[i], xArr[i], zArr[i]);


    } // end iteration on bands

    
   // Save output image
   saveCmdImg("OUT", out); 

   // Save control points display if required
   if (!independent && saveCPdisplay) {
      SimpleImage<int> * out = new SimpleImage<int>(3, nl, ns);
      drawControlPoints(img, xControlArr, yControlArr, noVal, out);
      saveCmdImg<int>("CP_OUT", out, 0);
      out->free();
   }
   

}  // end main






std::vector<long> getControlPoints(std::vector<long> &validLoc,
                                   int ns, int nl,
                                   int numControl,
                                   int skip)
{

   std::random_device rd;
   std::mt19937 g(rd());
   std::vector<long> cpArr; 

   // If no valid location, then nothing to extract
   if (validLoc.empty())
      return cpArr;

   // numControl, and skip cannot be all 0. They define the control points 
   // sampling strategy.
   if (numControl <= 0 && skip <= 0)
      return cpArr;

   // If skip is not supplied, compute the theoretical skip corresponding
   // to a subgrid of the input image size (ns x nl) that would provide
   // numControl points
   if (skip == 0) 
      skip = std::ceil(std::sqrt((double) ns * nl / numControl));

   int keepSampling = 1;
   int iteration = 0;
   while(keepSampling) {


      // Define the sampling grid
      // The commented method is simpler but suffers from a drift in sampling
      // as the iteration increase which causes sampling to be different at
      // the top of the image vs at the bottom. It's due to rounding error
      // of the skip that cumulate
      // The current method seems a bit hackish, but it's cheap and does a 
      // better job.
/*
      // Define sampling grid
      int offset, cskip;
      if (iteration == 0) {
         offset = 0;
         cskip = skip;
      }
      else {
         offset = skip / std::pow(2,iteration);     // X/Y start of the grid subsampling
         cskip  = skip / std::pow(2,iteration-1);   // current skip 
      }
      std::vector<long> gridArr;
      for (int j=offset; j<nl; j += cskip) {
         for (int i=offset; i<ns; i += cskip) {
            gridArr.push_back(j*ns + i);
         }
      }

*/
      // Define sampling grid
      int offset, cskip, subsets;
      if (iteration == 0) {
         offset = 0;
         cskip = skip;
         subsets = 1;
      }
      else {
         offset = skip / std::pow(2,iteration);     // X/Y start of the grid subsampling
         cskip  = skip / std::pow(2,iteration-1);   // current skip 
         subsets = std::pow(2, iteration-1);
      }
      std::vector<long> gridArr;
      for (int j=0; j<nl; j += skip) {
         for (int sj = 0; sj < subsets; sj++){
            int y = j + offset + sj*cskip;
            if (y >= nl)
               continue;

            for (int i=0; i<ns; i += skip) {
               for (int si = 0; si < subsets; si++) {
                  int x = i + offset + si*cskip;
                  if (x >= ns)
                     continue;
                  gridArr.push_back(y*ns + x);
               }
            }
         }
      }


      // Get which valid pixels are also part of the  current sampling grid
      std::sort(validLoc.begin(), validLoc.end());
      std::sort(gridArr.begin(), gridArr.end());
      std::vector<long> locArr;
      std::set_intersection(validLoc.begin(), validLoc.end(), gridArr.begin(), gridArr.end(), std::back_inserter(locArr));

      // Depending on the amount of points found, update the control point array.
      // If more than necessary are found, random draw the number we need.
      if (locArr.size() >= numControl && numControl != 0) {
         std::shuffle(locArr.begin(), locArr.end(), g);
         cpArr.insert(cpArr.end(), locArr.begin(), locArr.begin() + numControl);
         keepSampling = 0; // maximum of control points found, exit
      }
      else {
         // Save all the points found
         cpArr.insert(cpArr.end(), locArr.begin(), locArr.end());
         // Remove from the pool of valid pixels the ones that have been added
         // to the control point list
         std::vector<long>tmp;
         std::set_difference(validLoc.begin(), validLoc.end(), locArr.begin(), locArr.end(), std::back_inserter(tmp));
         // If none are left, then there's no valid measurement left - exit
         // Or if numControl = 0, no iterative finds of control points.
         if (tmp.empty() || numControl == 0)
            keepSampling = 0;
         else
            validLoc.swap(tmp);
         // Update how many left we want to retrieve
         numControl -= locArr.size();
      }

      iteration++;
   }

   return cpArr;
}



void drawControlPoints(SimpleImage<float> *im, std::vector<float> &xarr, std::vector<float> &yarr, float noVal, SimpleImage<int> *out)
{
   int nl  = im->getNL();
   int ns  = im->getNS();

   for (int j=0; j< nl; j++) {
      for (int i=0; i< ns; i++) {
         if (im->get(0,j,i) == noVal) {
            out->set(0,j,i,0);
            out->set(1,j,i,0);
            out->set(2,j,i,0);
         } 
         else {
            out->set(0,j,i,255);
            out->set(1,j,i,255);
            out->set(2,j,i,255);
         } 
      }
   }


   // Control point in red
   int c1 = 255;
   int c2 = 0;
   int c3 = 0;

   // Get all drawn points coordinates
   for(int i=0; i<xarr.size(); i++) {

      int x = xarr[i];
      int y = yarr[i];

      // Control point location
      out->set(0, y, x, c1);
      out->set(1, y, x, c2);
      out->set(2, y, x, c3);


      // Draw "cross" around control point
      if (y > 2) {
         out->set(0, y-2, x, c1);
         out->set(1, y-2, x, c2);
         out->set(2, y-2, x, c3);
      }
      if (y > 1) {
         out->set(0, y-1, x, c1);
         out->set(1, y-1, x, c2);
         out->set(2, y-1, x, c3);
      }
      if (x > 2) {
         out->set(0, y, x-2, c1);
         out->set(1, y, x-2, c2);
         out->set(2, y, x-2, c3);
      }
      if (x > 1) {
         out->set(0, y, x-1, c1);
         out->set(1, y, x-1, c2);
         out->set(2, y, x-1, c3);
      }

      if (x < ns-1) {
         out->set(0, y, x+1, c1);
         out->set(1, y, x+1, c2);
         out->set(2, y, x+1, c3);
      }
      if (x < ns-2) {
         out->set(0, y, x+2, c1);
         out->set(1, y, x+2, c2);
         out->set(2, y, x+2, c3);
      }
      if (y < nl-1) {
         out->set(0, y+1, x, c1);
         out->set(1, y+1, x, c2);
         out->set(2, y+1, x, c3);
      }
      if (y < nl-2) {
         out->set(0, y+2, x, c1);
         out->set(1, y+2, x, c2);
         out->set(2, y+2, x, c3);
      }

   }
}


