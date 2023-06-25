#include "vicmain_c"

#include "mars_support.h"

#include "PigMission.h"
#include "PigFileModel.h"
#include "PigLabelModel.h"
#include "PigCameraModel.h"
#include "PigPointingModel.h"
#include "PigSurfaceModel.h"

#ifdef _OPENMP
#include <omp.h>
#endif

#include "SimpleImage.h"
#include <iostream>
#include <cmath>


#include "ImgUtilities.h"
#include "ImgMatcher.h"
#include "ImgDispFinderArea.h"

// buffer sizes in main program - these should be made dynamic, eventually!
#define MAX_INPUTS 2
#define MAX_NL MARS_MAX_NL
#define MAX_NS MARS_MAX_NS






///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
// DEBUG AND TEST FUNCTIONS ONLY
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

//#include <chrono> 

//auto _t0 = high_resolution_clock::now();
//computeIntegralImage<const float>(imgL->linePtr(0), _ns, _nl, I, I2);
//auto _t1 = high_resolution_clock::now();
//auto _ti = duration_cast<milliseconds>(_t1 - _t0); 
//std::cout << "Combined integral image: " << _ti.count() << " milliseconds" << std::endl;


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
// END OF DEBUG AND TEST FUNCTIONS
///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////









////////////////////////////////////////////////////////////////////////

void main44()
{
   int nids, val, count, arri[10];
   float arrf[10];  
   char msg[150], mission[64], instrument[64];

   PigFileModel *file_models[MAX_INPUTS];
   PigCameraModel *camera_in[MAX_INPUTS];
   PigPointingModel *pointing_in[MAX_INPUTS];
   int homogeneous_inputs = TRUE;
   RadiometryModel *radiometric[MAX_INPUTS];
   PigCoordSystem *output_cs;
   PigSurfaceModel *surface_model;



   zvmessage("MARSRCORR", "");

   // Get the input file name, and set up initial camera/pointing model
   // for it.  Although we accept only single file input, mars_setup
   // does lots of other nice things for us.
   mars_setup(nids, file_models, camera_in, pointing_in, surface_model,
              radiometric, output_cs, mission, instrument,homogeneous_inputs,
              MAX_NL, MAX_NS, MAX_INPUTS);



   // Check that we have only two files (L and R)
   if (nids != 2) {
      zvmessage("Two files are expected (Left and Right)","");
      zabend();
   }

   if (file_models[0] == nullptr || file_models[1] == nullptr) {
      zvmessage("Necessary file models could not be created.","");
      zabend();
   }

   // Do the Left and Right images have valid camera models?
   int validCamLeft = (camera_in[0] != NULL) ? 1 : 0;
   int validCamRight = (camera_in[1] != NULL) ? 1 : 0;





   /******************************************/
   /* RETRIEVE IMAGE PRE-PROCESSING REQUESTS */
   /******************************************/

   // Which band of the input images are to be processed?
   int leftBand, rightBand;
   zvp("BANDS", arri, &count);    
   if (count == 1) {
       leftBand  = arri[0];     
       rightBand = arri[0];
   }
   else if (count == 2) {     
       leftBand  = arri[0];     
       rightBand = arri[1];
   }
   else {}

   // Check that band number is ok w.r.t image. Last band is selected if input 
   // is greater than image' number of bands
   if (leftBand > file_models[0]->getNB())
      leftBand = file_models[0]->getNB();
   if (rightBand > file_models[1]->getNB())
      rightBand = file_models[1]->getNB();



   // Low-pass pre-filtering of the input images with a Gaussian filter?
   float leftGaussFilter, rightGaussFilter, filterConst;
   int apply_filter = zvptst("FILTER"); 
   if (apply_filter) {
      zvp("FILTER_SIZE", arrf, &count);
      if (count == 1) {
         leftGaussFilter = arrf[0];
         rightGaussFilter = arrf[0];
      }
      else if (count == 2) {
         leftGaussFilter = arrf[0];
         rightGaussFilter = arrf[1];
      }
      else {}

      //Check on values for non-sense?
      filterConst = 0.6;
      zvp("FILTER_CONST", &filterConst, &count); 
   }



   // Is the process to be run using a pyramid approach?
   int pyramid = zvptst("RUN_PYR");

   // Minium size of any side (col or row) either Left or Right images when at
   // the top of the pyramid (smallest size)
   int pyrSizeMax;
   zvp("MAX_PYR_SIZE", &pyrSizeMax, &count);    

   // Is the disparity to be found on the full resolution images or at a 
   // downsampled level
   int downFactor = 1;
   int pyrLevel = 0;
   zvp( "PYRLEVEL", &pyrLevel, &count); 
   if (count)
      downFactor = pow(2, val);





   // ****************************************************/
   // * RETRIEVE THE IMAGE MATCHING TECHNIQUE TO BE USED */
   // ****************************************************/

   // Retrieve the type of matching technique and instanciate the corresponding
   // matcher
   ImgMatcher * corr = nullptr;
   if (zvptst("NCC"))
      corr = new ImgMatcherNCC();
   else if (zvptst("ZSAD"))
      zvmessage("SAD not yet implemented","");
//      corr = new SADMatcher<float>();
   else if (zvptst("ZSSD"))
      corr = new ImgMatcherZSSD();
   else {
      zvmessage("Unsupported Matching algorithm","");
      zabend();
   }

   // Check that instanciation succeeded
   if (corr == nullptr) {
      zvmessage("Instanciation of Image matcher failed","");
      zabend();
   }


   // Get the size of the correlation window
   zvp("TEMPLATE", arri, &count);             // Size of area to correlate
   if (count == 1)
       arri[1] = arri[0];                     // Make it square if needed
   if ((arri[0] % 2) == 0)                    // make sure it's odd-sized
       arri[0] += 1;
   if ((arri[1] % 2) == 0)
       arri[1] += 1; 
   corr->setTemplate(arri[1], arri[0]);   
 
   // Get the search area (nb pixel). Depending on the type of Disparity finder,
   // this will have different meaning. In the case of standard 2D area search,
   // this corresponds to the 2D search area (in X and Y direction).
   // In a Plane Sweep approach, this corresponds to the perpendicular-epipolar
   // search range (+/- accross the epipolar curve). Only the first element of 
   // the 2-elements array is used.
   zvp("SEARCH", arri, &count);
   if (count == 1)
       arri[1] = arri[0];                   
   // Adjust the search if the output is a downsampled version of the input
   arri[0] = arri[0] / downFactor;
   arri[1] = arri[1] / downFactor;
   corr->setSearch(arri[1], arri[0]);

   // Is subpixel matching required?
   zvp("SUBPIXEL", arri, &count);
   if (count == 1)
      arri[1] = arri[0];                   
   corr->setSubpixel(arri[1], arri[0]);



   // ************************************************/
   // * RETRIEVE THE DISPARTY SEARCH APPROACH TO USE */
   // ************************************************/


   // Get disparity finding strategy. There's currently two strategies
   // available:
   // - Standard local search. The Right image is search around an area
   // for the best matching score with the Left image
   // - Plane sweep (upcoming). The Right image is search around the epipolar 
   // line. This mode requires valid camera model for both the Left and Right
   // images.
   DispFinder * disp = nullptr; 
   if (zvptst("PLANE_SWEEP")) {
      //disp = new PlaneSweepFinder();
      zvmessage("Plane-sweep strategy not yet supported","");
      zabend();
   }  
   else if (zvptst("STD"))
      disp = new AreaDispFinder();
   else {
      zvmessage("Unsupported disparity finding mode","");
      zabend();
   }


   // Retrieve the parameters that are common to any strategy chosen

   // Set the image matcher
   disp->setImageMatcher(corr);

   // Set the regularization strategy?
   int numDirections;
   zvp("DIRECTIONS", &numDirections, &count);  
   if (zvptst("SGM"))
      disp->setRegularization(RegType::sgm, numDirections);
   else if(zvptst("MGM"))
      disp->setRegularization(RegType::mgm, numDirections);
   else
      disp->setRegularization(RegType::none, 0);


   // Get Regularization parameters 
   int autocorr = zvptst("AUTOCORRELATION");
   float regParams[10];
   int countRegParams;
   zvp("REG_PARAMS", regParams, &countRegParams);
   if (countRegParams == 0 && disp->getRegularization() != RegType::none &&
       !autocorr) {
      // Regularization parameters are missing.
      // Will estimate them from autocorrelation, print them out, and
      // bail out.
      autocorr = 1;
   }


   // Retrieve parameters that are specific to the chosen strategy

   auto typeDisp = disp->getType();

  // ***************** Plane Sweep specific parameters *******************
   if (typeDisp == "PlaneSweep Disparity Finder") {

   }
   // *************** Standard Search specific parameters *****************
   else if (typeDisp == "Area-based Disparity Finder") {

      // Check if there's any prior to guide the matching ****************
          
      // Any translation/rigid/affine transform prior?
      zvp("AFFINE_PARAM", arrf, &count);  
      // No prior transform
      if (count != 0) {
      
         float coefs[8];

         // Translation prior
         if (count == 2) {
            coefs[0] = 1; 
            coefs[1] = 0;
            coefs[2] = arrf[0];
            coefs[3] = 0;
            coefs[4] = 1;
            coefs[5] = arrf[1];
         }
         // Affine transform prior
         else if (count == 6) {
            coefs[0] = arrf[0]; 
            coefs[1] = arrf[1];
            coefs[2] = arrf[2];
            coefs[3] = arrf[3];
            coefs[4] = arrf[4];
            coefs[5] = arrf[5];
         }
         // Error, invalid number of arguments
         else {
            zvmessage("Error in AFFINE_PARAMS numbers:0, 2, or 6 only","");
            zabend();
         }

         // Get the size of the image these affine coefficients apply to.
         // They may not correspond to the size of the input image. For 
         // instance, the affine prior may comes from a downsampled version
         // of the input image.
         zvp("AFFINE_SIZE", arrf, &count);  
         if (count == 2) {
            coefs[6] = arrf[0];
            coefs[7] = arrf[1];
         }
         else {
            coefs[6] = file_models[0]->getNS();
            coefs[7] = file_models[0]->getNL();
         }

         disp->setInputPrior(coefs, 8);
      } 


      // Dense prior maps?
      zvpcnt("IN_DISP", &count);
      if (count != 0) {
         SimpleImage<float> * inDisp = nullptr;
         read_image("IN_DISP", inDisp);
         if (inDisp == nullptr) {
            zvmessage("Error: Invalid IN_DISP image","");
            zabend();
         }
         if (inDisp->getNB() < 2) {
            zvmessage("Error: IN_DISP must be a 2-band image with Line and Sample disparities","");
            zabend();
         }
         disp->setInputPrior(inDisp);
 
         // If pyramid approach was turned on, it needs to be turned off as it
         // can't be used with input disparity. The input disparity is 
         // interpolated to the final level
         if (pyramid) {
            zvmessage("Pyramid approach turned OFF because input disparity is supplied (IN_DISP)","");
            pyramid = 0;
         }
      }


      // Get Regularization parameters 
      if (countRegParams != 0 && disp->getRegularization() != RegType::none &&
          !autocorr) {
         auto pixVal = corr->getSearch();
         auto subpixVal = corr->getSubpixel();
         if (countRegParams >= 2) 
            disp->setRegularizationParameters(regParams, 2);
         else {
            zvmessage("Incorrect number of regularization parameters for Area-based Disparity finder regularization.","");
            zabend();
         }

         if ((subpixVal[0] !=0 || subpixVal[1] !=0) && (countRegParams < 4)) {
            zvmessage("If subpixel correlation, 4 regularization parameters are needed for Area-based Disparity finder regularization.","");
            zabend();
         }
      }
          

   }
   else {
      // *********************************************************************
      // ***************** Future Disparity Finder approaches ****************
      // *********************************************************************
      zvmessage("Unknown Disparity Finder","");
      zabend();
   }





   /****************************/
   /* MISCELLANEOUS */
   /****************************/

   // Multithreading?
   int omp_on = zvptst("OMP_ON");








   /***************************************************************************/
   // Done with retrieving of User inputs.
   // Print out some information about the input and process options 
   /***************************************************************************/

   zvmessage("","");
   zvmessage("************ Image information **************","");
   
   sprintf(msg, "Mission: %s", file_models[0]->getMissionName());
   zvmessage(msg, ""); 
  
   if (output_cs != NULL)
      sprintf(msg, "Default CS out of initialization: %s", output_cs->getFullName());
   else
      sprintf(msg, "Default CS out of initialization: NULL");
   zvmessage(msg, ""); 

   // Left image information
   sprintf(msg, "** Left image: **");
   zvmessage(msg, ""); 
   // Get Left image filename 
   sprintf(msg, "%s", file_models[0]->getFilename());
   zvmessage(msg, ""); 
   // Get Left image dimensions
   int nsl_original = file_models[0]->getNS();
   int nll_original = file_models[0]->getNL();
   sprintf(msg, "Original size: %dx%d pixels", nsl_original, nll_original);
   zvmessage(msg,"");
   // Get Left image camera model
   if (validCamLeft)
      sprintf(msg, "Camera model: %s", camera_in[0]->getModelName());
   else
      sprintf(msg, "No camera model");
   zvmessage(msg,"");
    
   // Right image information
   sprintf(msg, "** Right image: ** ");
   zvmessage(msg, ""); 
   // Get Right image filename 
   sprintf(msg, "%s", file_models[1]->getFilename());
   zvmessage(msg, ""); 
   // Get Right image dimensions
   int nsr_original = file_models[1]->getNS();
   int nlr_original = file_models[1]->getNL();
   sprintf(msg, "Original size: %dx%d pixels", nsr_original, nlr_original);
   zvmessage(msg,"");
   // Get Right image camera model
   if (validCamRight)
      sprintf(msg, "Camera model: %s", camera_in[1]->getModelName());
   else
      sprintf(msg, "No camera model");
   zvmessage(msg,"");
    
  


   zvmessage("","");
   zvmessage("********* Processing information ************","");



   // Print out some information about the disparity finding approach
   sprintf(msg, "Disparity search approach: %s", (disp->getType()).c_str());
   zvmessage(msg, "");
   sprintf(msg, "Regularization: %s", (disp->getRegularizationName()).c_str());
   zvmessage(msg, "");
   if ((disp->getRegularizationName()).c_str() != "NONE") {
      sprintf(msg, "Number of Regularization directions: %d", disp->getRegularizationNumDirections());
      zvmessage(msg, "");
      zvmessage("Regularization parameters:","");
      int nbParams = disp->getRegularizationParamCount();
      float params[nbParams];
      disp->getRegularizationParameters(params, nbParams);
      for (int i=0; i<nbParams; i++) {
         sprintf(msg, "%s: %6.3f", (disp->getRegularizationParamName(i)).c_str(), params[i]);
         zvmessage(msg,"");
      }
   }
   int priorNum = disp->getInputPriorCount();
   if (priorNum == 0)
      zvmessage("Prior disparity: None","");
   else if (priorNum == 1)
      zvmessage("Prior disparity: Input disparity file","");
   else if (priorNum == 8) {
      float params[8];
      disp->getInputPrior(params, 8);
      sprintf(msg, "Prior disparity: Transform (%6.3f, %6.3f, %6.3f, %6.3f, %6.3f, %6.3f)", 
                 params[0], params[1], params[2], params[3], params[4], params[5]); 
      zvmessage(msg, "");
      sprintf(msg, "Prior disparity: Transform ref size: %dx%d", static_cast<int>(params[6]), static_cast<int>(params[7]));
      zvmessage(msg, "");
   }
   else
      zvmessage("Prior disparity: unknown(suspicious!)","");


   zvmessage("","");

   // Print out some information abou the selected matching algorithm
   sprintf(msg, "Image matching technique: %s", (corr->getName()).c_str());
   zvmessage(msg,""); 
   auto v = corr->getTemplate();
   sprintf(msg, "Template (X, Y): %dx%d pixels", v[0], v[1]);
   zvmessage(msg,""); 
   v = corr->getSearch();
   sprintf(msg, "Search (X, Y): %dx%d pixels", v[0], v[1]);
   zvmessage(msg,""); 
   v = corr->getSubpixel();
   sprintf(msg, "Subpixel resolution (X, Y): %4.3fx%4.3f pixels", 1/static_cast<float>(v[0]+1), 1/static_cast<float>(v[1]+1));
   zvmessage(msg,""); 

   zvmessage("","");

   // Print out some information about downsampling
   if (downFactor > 1) {
      sprintf(msg, "Input downsampling factor %d (pyramid level: %d)", 
              downFactor, (int)std::log2(downFactor));
      zvmessage(msg, "");
      sprintf(msg, "Left Image downsampled size: %dx%d pixels", 
                   nsl_original/downFactor, nll_original/downFactor);
      zvmessage(msg,"");
      sprintf(msg, "Right Image downsampled size: %dx%d pixels", 
                   nsr_original/downFactor, nlr_original/downFactor);
      zvmessage(msg,"");
   }
   else zvmessage("Output pyramid level: 0 (full size)", "");





   /***************************************************************************/
   // Processing 
   /***************************************************************************/


   
   // Load L and R images 
   SimpleImage<float> * imgL = loadImage(file_models[0], leftBand);
   SimpleImage<float> * imgR = loadImage(file_models[1], rightBand);


   // if pyramid level is on, downsample the image
   if (downFactor > 1) {

      downsample(imgL, imgL, downFactor, omp_on);
      downsample(imgR, imgR, downFactor, omp_on);

      // Scale PIG camera models accordingly
      if (validCamLeft)
         camera_in[0]->scaleCamera(1.0/downFactor, 1.0/downFactor);   
      if (validCamRight)
         camera_in[1]->scaleCamera(1.0/downFactor, 1.0/downFactor);   
   }


   // Apply gaussian filtering if needed.
   if (apply_filter) {
      zvmessage("Pre-filtering (low-pass) of the images","");
      if (leftGaussFilter > 1) {
         float sigma = filterConst * sqrt(leftGaussFilter * leftGaussFilter - 1.0);
         GaussianBlur(imgL, sigma, sigma, omp_on);
      }
      if (rightGaussFilter > 1) {
         float sigma = filterConst * sqrt(rightGaussFilter * rightGaussFilter - 1.0);
         GaussianBlur(imgR, sigma, sigma, omp_on);
      }
   }
   else
      zvmessage("No low-pass pre-filtering of the images","");



   // Test for autocorrelation if requested
   if (autocorr) {
      zvmessage("SGM/MGM need regularization parameters (REG_PARAMS).","");
      zvmessage("Running Image auto-correlation to get estimates or run with -AUTOCORR.","");
   
      // Run the autocorrelation using the selected image matching algorithm
      float outarr[20];
      int countarr=0;
      disp->setImages(imgL, imgR);
      disp->autoCorrelation(outarr, countarr);
      return;
   }




   // How many pyramid levels necessary if having a pyramid approach?
   int pyrMax = 0;
   if (pyramid) { 
      // Compute the pyramid level necessary. Look a the smallest side (X and Y 
      // directions) between the Left and Right image. Then how many dowsampling
      // (by x2 each time) are necessary to get that smallest side to be smaller
      // than pyrSizeMax.
      int minSideSize = imgL->getNS();
      if (imgL->getNL() < minSideSize) minSideSize = imgL->getNL();
      if (imgR->getNS() < minSideSize) minSideSize = imgR->getNS();
      if (imgR->getNL() < minSideSize) minSideSize = imgR->getNL();

      while (minSideSize > pyrSizeMax) {
         minSideSize /= 2;
         pyrMax++;
      }
      sprintf(msg, "Pyramidal approach ON (%d pyramid levels)", pyrMax);
      zvmessage(msg,"");
   }
   else
      zvmessage("Pyramidal approach OFF","");


   






   // Temporary image containers for pyramidal matching approach
   SimpleImage<float> *imgLc = nullptr;
   SimpleImage<float> *imgRc = nullptr;

   // The subpixel matching, if any, is done at the very end of the process
   // as an extra pass. Whether of not the pyramidal approach is used, the
   // matching is done with integer accuracy only. Then, if subpixel accuracy
   // is required, an extra matching pass is done, using the last matching 
   // result as a prior.
   // Store subpixel factors and cancel subpixel match for now
   auto pixValues = corr->getSearch();
   auto subpixValues = corr->getSubpixel();
   if (pixValues[0] >= 1  && pixValues[1] >= 1)
      corr->setSubpixel(0,0);



   // In case of pyramidal approach, this is the main loop of the process. If 
   // the pyramidal approach is not used, this is a 1-iteration loop
   for (int pyr = pyrMax; pyr >= 0; pyr--) {
   
      sprintf(msg, "Pyramid num: %d", pyr);
      zvmessage(msg,"");
   
      if (pyr != pyrMax) {
         imgLc->free();
         imgRc->free();
         imgLc = nullptr;
         imgRc = nullptr;
      }
   
      // Downsampling factor of the current pyramid iteration
      int downFactor = pow(2,pyr);
   
      // Downsample the images accordingly...
      downsample(imgL, imgLc, downFactor, omp_on);
      downsample(imgR, imgRc, downFactor, omp_on);


      // Scale PIG camera models accordingly
      // On the first pass, the camera model is downsampled all the way to the
      // smallest "size", then upsampled by a factor of 2 at each subsequent 
      // pass. It is done this way because it is not clear if camera models can 
      // be copied easily
       
      if (pyr == pyrMax) {
         if (validCamLeft)
            //leftC.cm->scaleCamera(1.0/downFactor, 1.0/downFactor);   
            camera_in[0]->scaleCamera(1.0/downFactor, 1.0/downFactor);   
         if (validCamRight)
            //rightC.cm->scaleCamera(1.0/downFactor, 1.0/downFactor);   
            camera_in[1]->scaleCamera(1.0/downFactor, 1.0/downFactor);   
      }
      else {
         if (validCamLeft)
            //leftC.cm->scaleCamera(2.0, 2.0);   
            camera_in[0]->scaleCamera(2.0, 2.0);   
         if (validCamRight)
            //rightC.cm->scaleCamera(2.0, 2.0);
            camera_in[1]->scaleCamera(2.0, 2.0);
      }


      // Update DisparityFinder with new images
      disp->setImages(imgLc, imgRc);


      // Update the image matching engine if necessary
      // TODO for plane sweep that should be a DispFinder function. Corr object 
      // is useless in this case.
      //
      // If first pass, then scale down the search space according to the 
      // pyramid level.
      if (pyr == pyrMax) { 
         auto s = corr->getSearch();
         s[0] = (s[0] > downFactor) ? s[0] / downFactor : 1;
         s[1] = (s[1] > downFactor) ? s[1] / downFactor : 1;
         corr->setSearch(s[0], s[1]);
      }
      // Otherwise (not first pass), set the search space to 2 pixels around the
      // matching locations found in the previous pass, and use the previous
      // pass disparity as a prior for the upcoming matching.
      else {
         corr->setSearch(2, 2);
         disp->setInputPrior(PriorType::output);
      }

      // This is the core of the program.
      // Compute the matching score for all pixels and all search positions.
      // Then, regularize the matching score volume.
      disp->computeMatchScores();
      disp->regularize();


   } // end pyramid loop


   // Do subpixel pass if needed
   if (subpixValues[0] != 0 || subpixValues[1] != 0) {

      zvmessage("Subpixel processing","");

      // Reduce pixel search to 1
      corr->setSearch(1, 1);
      // Set subpixel accuracy
      corr->setSubpixel(subpixValues[0], subpixValues[1]);
      // Let the matcher know to use the previous disparity found as a prior
      disp->setInputPrior(PriorType::output);
     
      disp->setRegularizationParameters(regParams+2, 2);
      disp->computeMatchScores();
      disp->regularize();
   }

   // Get a handle on the final disparity maps. These are the main output
   // of the program.
   SimpleImage<float> *dispX = disp->getDisparityX();
   SimpleImage<float> *dispY = disp->getDisparityY();



   // For disparity output label
   int n = imgL->getNS() * imgL->getNL();
   float *p = dispX->linePtr(0);
   int numCorrValid =  std::count_if(p, p+n, [](int i){return i;});
   
   // Setting disparities to be 1-based for vicar compliance
   std::transform(p, p+n, p, [](float &val){return val+1;}); 
   p = dispY->linePtr(0);
   std::transform(p, p+n, p, [](float &val){return val+1;}); 



   /***************************************************************************/
   // WRITE DATA FILES ********************************************************/
   /***************************************************************************/

   zvmessage("Writing files to disk...", "");

   int unit_out;
   zvunit(&unit_out, "OUT", 1, NULL);
   zvopen(unit_out, "OP", "WRITE", "U_FORMAT", "REAL", "O_FORMAT", "REAL",
          "U_NS", dispX->getNS(), "U_NL", dispX->getNL(), "OPEN_ACT", "AS", "U_NB", 2, "U_ORG", 
          "BSQ", NULL);
   zvplabel(unit_out, 0, 1);

   // Write output label
   PigMission *m = NULL;
   m = PigMission::getMissionObject(mission);    
   if (m != NULL) {
      PigLabelModel *labelModel_0 = NULL;
      labelModel_0 = m->createLabelModel(unit_out);
      if (labelModel_0) { 
         labelModel_0->setDisparity(file_models, file_models[1], nids, 
                                    "DISPARITY_MAP");
         labelModel_0->setDisparityExtra(numCorrValid, 1, numCorrValid/n, 
                                         pyrLevel);
      }
    }
    else zvmessage("Mission unknown, no specific Label writing","");


   // Write disparities
   for (int j = 0; j < nll_original; j++) 
      zvwrit(unit_out, dispY->linePtr(j), "LINE", j+1, "BAND", 1, NULL);
   for (int j = 0; j < nll_original; j++) 
      zvwrit(unit_out, dispX->linePtr(j), "LINE", j+1, "BAND", 2, NULL);

   zvclose(unit_out, "CLOS_ACT", "FREE", NULL);



}



