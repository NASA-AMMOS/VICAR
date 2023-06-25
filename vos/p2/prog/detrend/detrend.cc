#include "vicmain_c"

#include <numeric>
#include <cmath>
#include <algorithm>
#include <vector>
#include "SimpleImage.h"
#include "polyFit.h"
#include "imgIO.h"


////////////////////////////////////////////////////////////////////////

double getStandardDeviation(const double *val, int num);

void main44()
{
   int msgLen = 150;

   SimpleImage<float> * img     = nullptr;
   SimpleImage<float> * out     = nullptr;
   SimpleImage<int>   * outMask = nullptr;

   // Load input image
   readCmdImg("INP", img);

   // Get images dimensions
   int ns = img->getNS();
   int nl = img->getNL();
   int nb = img->getNB();


   // Get the downsampling factor, that is, only every other SKIP pixels in X 
   // and Y will be used to define the surface fit. This is only to limit the
   // size of the matrix to invert.
   int skip, count;
   zvp("SKIP", &skip, &count);

   // Get polynomial max degree in x and y combined
   int degree;
   zvp("DEGREE", &degree, &count);

   // Get the no value, i.e., values that are considered NaN and which are not
   // taken into consideration for the surface fit and correction
   float noVal;
   zvp("NOVAL", &noVal, &count);

   // Get number of iteration for the surface fit to alleviate outlier bias
   int numIterations;
   zvp("ITER", &numIterations, &count);

   // Get the scaling factor to apply to the std deviation of residuals for
   // filtering out pixels for the next fit iteration.
   // All pixels whose residual is > stdev residual * stdevScaling are removed
   // from the next iteration fit
   float stdevScaling;
   zvp("SCALING", &stdevScaling, &count);

   // Check if we need to compute only the surface or also correct the image
   int surface_only = 0;
   surface_only = zvptst("SURFACE_ONLY");

   // Check if the output surface needs to be computed for every pixel or only
   // the valid ones - No effect if SURFACE_ONLY is not set
   int fill = 0;
   fill = zvptst("FILL");

   // OMP processing?
   int omp = zvptst("OMP_ON");

   // Get a specific band to process, if any. Otherwise do all of them.
   int band;
   zvp("BAND", &band, &count);
   if (band > nb) {
      zvmessage("Band requested is > to nb of bands. First band processed","");
      band = 1;
   }
   int bp = (band == 0 ? nb : 1);

   // Get band indices to process
   std::vector<int> bIndices(bp);
   if (band == 0)
      std::iota(begin(bIndices), end(bIndices), 0);
   else
      bIndices[0] = band - 1;

   // Initialize output container
   out = new SimpleImage<float>(bp, nl, ns);
   std::fill(out->linePtr(0), out->linePtr(0)+bp*nl*ns, noVal);
   
   // Do we need to save a fit mask?
   int saveMask = 0;
   zvpcnt("MASK_FIT", &saveMask);
   if (saveMask) {
      outMask = new SimpleImage<int>(bp, nl, ns);
      outMask->zero();
   }
    
    

   // Print some input information
   char msg[msgLen];
   snprintf(msg, msgLen, "Input image (nb x nl x ns): %d x %d x %d", nb, nl, ns);
   zvmessage(msg, "");

   if (band == 0)
      snprintf(msg, msgLen, "Band processed: All");
   else  
      snprintf(msg, msgLen, "Band processed: %d", bIndices[0]+1);
   zvmessage(msg, "");

   snprintf(msg,msgLen,  "X/Y pixels skip: %d", skip);
   zvmessage(msg, "");

   snprintf(msg, msgLen, "Polynomial surface degree: %d", degree);
   zvmessage(msg, "");

   snprintf(msg, msgLen, "Ignored value: %7.3f", noVal);
   zvmessage(msg, "");

   snprintf(msg, msgLen, "Number of surface fit iterations: %d", numIterations);
   zvmessage(msg, "");

   snprintf(msg, msgLen, "Outlier distance scaling factor: %7.3f", stdevScaling);
   zvmessage(msg, "");


   std::vector<int> xarr, yarr;
   std::vector<float> zarr, zarrin;
   std::vector<double> coefs, zfit;


   // Process each band individually
   for (int b=0; b<bIndices.size(); b++) {

      zvmessage("", "");
      snprintf(msg, msgLen, "Processing band %d", bIndices[b]+1);
      zvmessage(msg, "");

      // Clear containers for new band
      xarr.clear();
      yarr.clear();
      zarr.clear();

      // Extract the pixels that will be used for the surface fit definition
      for (int j=0; j<nl; j += skip) {
         for (int i=0; i<ns; i += skip) {
          
            float val = img->get(bIndices[b], j, i);

            if (val == noVal)
               continue;

            xarr.push_back(i);
            yarr.push_back(j);
            zarr.push_back(val);
         }
      }
       
      int nbTotalPoints = xarr.size();
      snprintf(msg, msgLen, "Initial number of points for surface fit: %d", nbTotalPoints);
      zvmessage(msg,"");




      // Compute the polynomial surface fit to data in an iterative refinement
      // scheme.
      

      // For printing coefficients
      //char msg2[nc * 15 + nc], *pt;  //nc:number of coefficients
      char msg2[8*(degree+1)*(degree+2)], *pt;

      for (int it=0; it<numIterations; it++) {
 
         snprintf(msg, msgLen, "Iteration %d :", it+1);
         zvmessage(msg, "");

         // Get the number of points that will be used for the surface fit on
         // the current iteration
         int nbPoints = xarr.size();
         zfit.resize(nbPoints);

         snprintf(msg, msgLen, "Num pixels for surface fit: %d (%6.2f%% of initial list)", 
                 nbPoints, (float)nbPoints/nbTotalPoints *100.0);
         zvmessage(msg, ""); 

         // Compute Polynomial surface fit
         int status = computePolyFit(xarr, yarr, zarr, degree, coefs, zfit);

         // Check that the fit succeeded
         if (status) 
            break;
             

         // Compute standard deviation of difference between data and fit
         std::transform(zfit.begin(), zfit.end(), zarr.begin(), zfit.begin(), 
                        std::minus<double>()); // zfit contains difference now
         double stdev = getStandardDeviation(zfit.data(), zfit.size());

         // Correct stdev with scaling factor (user input). This is a "knob" 
         // adjusting the tolerance to outliers
         stdev *= stdevScaling; 
   
         // Remove pixels that are "too far" from the fit. They are considered 
         // outliers. Do this from back to front to avoid messing up the indices 
         // during removal.
         // TODO Not efficient if vector is large and lots of points to erase, in
         // particular at the begining of the vectors
         for (int i=(nbPoints-1); i>=0; i--) {
            if(std::abs(zfit[i]) > stdev) {
               xarr.erase(xarr.begin()+i);
               yarr.erase(yarr.begin()+i);
               zarr.erase(zarr.begin()+i);
            }
         }       

         // Printing coefficients for information
         pt = msg2;
         for(auto c:coefs) 
            pt += sprintf(pt,"%.7e ",c);
         
         zvmessage("Surface fit coefficients (X coefs increasing the fastest):","");
         zvmessage(msg2, "");

      } // end of iterative fit


      // Check that the Suface fit succeeded
      if (coefs.empty()) { 
         snprintf(msg, msgLen, "Suface fit failed.");
         zvmessage(msg, "");
         break;
      }

      // Depending on SURFACE_ONLY, compute the surface and correct the image,
      // or only compute the surface
      // The following if-else statement does very similar things, but given the
      // simplicity, it was easier than adding a bunch of condition within the 
      // loop.
      xarr.clear();
      yarr.clear();
      zarr.clear();
      if (saveMask) 
         zarrin.clear();

      if (surface_only) {
         for (int l=0; l<nl; l++) {
            for (int s=0; s<ns; s++) {

               float val = img->get(bIndices[b],l,s);
               if (val != noVal || fill) { 
                  xarr.push_back(s);
                  yarr.push_back(l);
                  if (saveMask)
                     zarrin.push_back(val);
               }                
            }
         }
         applyPolyFit(xarr, yarr, degree, coefs, zarr, omp);
         for (int i=0; i<xarr.size(); i++)
            out->set(b, yarr[i], xarr[i], zarr[i]);
      }
      else {
         for (int l=0; l<nl; l++) {
            for (int s=0; s<ns; s++) {
               
               float val = img->get(bIndices[b],l,s);
               if (val == noVal)
                  continue;
            
               out->set(b, l, s, val);
               xarr.push_back(s);
               yarr.push_back(l);
               if (saveMask)
                  zarrin.push_back(val);
               
            }
         }

         applyPolyFit(xarr, yarr, degree, coefs, zarr, omp);
         for (int i=0; i<xarr.size(); i++)
            out->addto(b, yarr[i], xarr[i], -zarr[i]);
      }

      // Save mask of pixels that are within fit tolerance
      if (saveMask) {
         // Remove elements that have a noVal value. This step is only needed 
         // because of the FILL case
         std::vector<int> xarrValid, yarrValid;
         std::vector<double> diff;
         for (int i = 0; i < zarr.size(); i++) {
            if (zarrin[i] != noVal) {
               diff.push_back(zarrin[i] - zarr[i]);
               xarrValid.push_back(xarr[i]);
               yarrValid.push_back(yarr[i]);
            }
         }
         // Compute fit residual and save mask value accordingly 
         double stdev = getStandardDeviation(diff.data(), diff.size()) * stdevScaling;
         for (int i = 0; i < diff.size(); i++) {
            if (std::abs(diff[i]) < stdev) 
               outMask->set(b, yarrValid[i], xarrValid[i], 1);
         } 
 
      }
     

   }  // End iteration on band

    
   // Save output image
   saveCmdImg("OUT", out); 

   // Save mask fit if necessary
   if (saveMask)
      saveCmdImg("MASK_FIT", outMask); 

}  // end main



double getStandardDeviation(const double *val, int num) 
{
   if (num <= 1) return 0;

   double mean = std::accumulate(val, val+num, 0.) / num;
   double variance = std::accumulate(val, val+num, 0., 
                      [&mean](double sum, double v){return sum + std::pow(v-mean,2);});
   return std::sqrt(variance / (num-1));
}
