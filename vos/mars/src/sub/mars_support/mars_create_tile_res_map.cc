////////////////////////////////////////////////////////////////////////
// mars_create_tile_res_map.cc
//
// Read an image tiling labels and constructs a tiling map corresponding to
// the current image downsampling residual. Output tiling image is the same
// size as the input image.
////////////////////////////////////////////////////////////////////////

#include "mars_support.h"

#include "zvproto.h"

#include "PigFileModel.h"
#include "PigPointingModel.h"
#include "PigCameraModel.h"
#include "PigCoordSystem.h"

#include <set>
#include <algorithm>
#include <vector>
#include <valarray>
#include <utility>
#include <iostream>

///////////////////////////////////////////////////////////////////////////
// Read an image tiling labels and constructs a tiling map corresponding to 
// the current image downsampling residual. Output tiling image is the same
// size as the input image.
////////////////////////////////////////////////////////////////////////////
int mars_create_tile_res_map(PigFileModel* file_model, SimpleImage<int> *& tI, 
                         std::set<int> &tilingLevel) {

   int status;
   int sameNbElements = 1;

   if (!file_model->isFileOpen())
       file_model->openFile();
   int unit = file_model->getUnit();
   //zvopen(unit, "OP", "READ", "U_FORMAT", "DOUB", "OPEN_ACT", "AS", NULL);


   ////////////////////////////////////////////////////////////////////////////
   // Extract relevant information about tiling parameters from the image label
   ////////////////////////////////////////////////////////////////////////////


   // Pixel averaging in X
   int pixelAvgWidth = 1; //default
   status = zlget(unit, "PROPERTY", "PIXEL_AVERAGING_WIDTH", &pixelAvgWidth,
                  "FORMAT", "INT", "ERR_ACT", "", "PROPERTY", 
                  "INSTRUMENT_STATE_PARMS", NULL);
   if (status <= 0) 
      return status;


   // Pixel averaging in Y
   int pixelAvgHeight = 1; //default
   status = zlget(unit, "PROPERTY", "PIXEL_AVERAGING_HEIGHT", &pixelAvgHeight,
                  "FORMAT", "INT", "ERR_ACT", "", "PROPERTY", 
                  "INSTRUMENT_STATE_PARMS", NULL);
   if (status <= 0) 
      return status;

   
   // Downsampling factor in X of each tiles
   int downsamplingX[20]; //max should be 16 per M2020 Engineering cam spec
   int nret = 0; // number of returned elements
   status = zlget(unit, "PROPERTY", "TILE_DOWNSAMPLE_X", downsamplingX,
                  "FORMAT", "INT", "ERR_ACT", "", "NELEMENT", -1, "NRET", &nret,
                  "PROPERTY", "INSTRUMENT_STATE_PARMS", NULL);
   if (status <= 0) 
      return status;

   if (nret == 0) {
      zvmessage("TILE_DOWNSAMPLE_X label parameter returned 0 elements","");
      return 0;
   }

   
   // Downsampling factor in X of each tiles
   int downsamplingY[20]; //max should be 16 per M2020 Engineering cam spec
   int nret2 = 0; // number of returned elements
   status = zlget(unit, "PROPERTY", "TILE_DOWNSAMPLE_Y", downsamplingY,
                  "FORMAT", "INT", "ERR_ACT", "", "NELEMENT", -1, "NRET", &nret2,
                  "PROPERTY", "INSTRUMENT_STATE_PARMS", NULL);
   if (status <= 0) 
      return status;

   if (nret != nret2) 
      sameNbElements = 0;



   // Starting position of each tile in X direction
   int startX[20]; //max should be 16 per M2020 Engineering cam spec
   status = zlget(unit, "PROPERTY", "TILE_FIRST_LINE_SAMPLE", startX,
                  "FORMAT", "INT", "ERR_ACT", "", "NELEMENT", -1, "NRET", &nret2,
                  "PROPERTY", "INSTRUMENT_STATE_PARMS", NULL);
   if (status <= 0) 
      return status;

   if (nret != nret2) 
      sameNbElements = 0;



   // Starting position of each tile in Y direction
   int startY[20]; //max should be 16 per M2020 Engineering cam spec
   status = zlget(unit, "PROPERTY", "TILE_FIRST_LINE", startY,
                  "FORMAT", "INT", "ERR_ACT", "", "NELEMENT", -1, "NRET", &nret2,
                  "PROPERTY", "INSTRUMENT_STATE_PARMS", NULL);
   if (status <= 0) 
      return status;

   if (nret != nret2) 
      sameNbElements = 0;



   // Number of samples of each tile
   int nbX[20]; //max should be 16 per M2020 Engineering cam spec
   status = zlget(unit, "PROPERTY", "TILE_NUM_LINE_SAMPLES", nbX,
                  "FORMAT", "INT", "ERR_ACT", "", "NELEMENT", -1, "NRET", &nret2,
                  "PROPERTY", "INSTRUMENT_STATE_PARMS", NULL);
   if (status <= 0) 
      return status;

   if (nret != nret2) 
      sameNbElements = 0;



   // Number of lines of each tile
   int nbY[20]; //max should be 16 per M2020 Engineering cam spec
   status = zlget(unit, "PROPERTY", "TILE_NUM_LINES", nbY,
                  "FORMAT", "INT", "ERR_ACT", "", "NELEMENT", -1, "NRET", &nret2,
                  "PROPERTY", "INSTRUMENT_STATE_PARMS", NULL);
   if (status <= 0) 
      return status;

   if (nret != nret2) 
      sameNbElements = 0;


   // If there is a discrepency between the number of tiles elements read from
   // the file, notify, and exit
   if (!sameNbElements) {
      zvmessage("Different number of elements read for TILE_DOWNSAMPLE_X","");
      zvmessage("or TILE_DOWNSAMPLE_Y or TILE_FIRST_LINE or TILE_FIRST_LINE_SAMPLE","");
      zvmessage("or TILE_NUM_LINES or TILE_NUM_LINE_SAMPLES","");
      return 0;
   }


   ////////////////////////////////////////////////////////////////////////////
   // Construct tile map for the input image
   ////////////////////////////////////////////////////////////////////////////


   // Tiles of different downsampling factor can overlap. For instance, a x4
   // downsampling tile can cover all the image, while other x2 tiles cover 
   // only part of it. x2 has precedence over x4 (we keep the highest 
   // resolution). The output will be x4 where there is no coverage with x2 
   // tiles, and x2 otherwise.
   // To achieve this, we fill the output downsampling factor image starting 
   // with the coarser tiles, down to the higher resolution ones.

   // Get the smaller average pixel size. Most of the time it will be the same
   int minAvg = pixelAvgHeight > pixelAvgWidth ? pixelAvgWidth : pixelAvgHeight; 


   // Generate a list of pair relating position in the label array (i.e., tile 
   // index) and downsampling factor. This will be used to order the tiles by
   // downsampling factor.
   std::vector<std::pair<int, int> > dsVect;
   for (int i=0; i<nret; i++)
      dsVect.push_back(std::make_pair(i,downsamplingX[i] > downsamplingY[i] ? 
                                        downsamplingX[i] : downsamplingY[i]));

   // Sort from coarser to higher resolution
   std::sort(dsVect.begin(), dsVect.end(),
             [](const std::pair<int,int> & d1, const std::pair<int,int> & d2)
                                               {return d1.second > d2.second;});


   // Image a crop of the full frame. Need to identify the line/sample of the
   // most top-left tile used in this image. This will provide the general
   // line/sample offset to apply when filling the tile map
   int minX = startX[0];
   for (int i=0; i<nret; i++) 
      if (startX[i] < minX) minX = startX[i];

   int minY = startY[0];
   for (int i=0; i<nret; i++) 
      if (startY[i] < minY) minY = startY[i];


   // Tiles origin and number of lines are expressed in the full 
   // resolution frame (raw frame). Scale the values with the average 
   // downsampling and offset with crop origin
   for (int i=0; i<nret; i++) startX[i] = (startX[i] - minX) / minAvg;
   for (int i=0; i<nret; i++) nbX[i] /= minAvg;
   for (int i=0; i<nret; i++) startY[i] = (startY[i] - minY) / minAvg;
   for (int i=0; i<nret; i++) nbY[i] /= minAvg;


   // Go over each tile and fill in the output
   for (const auto &p: dsVect) {
    
      // Get position in label output array and downsampling factor 
      int pos = p.first; 
      int dsFactor = pow(2, log2(p.second) - log2(minAvg));

      // If dsFactor < 1, that means that the averaging of pixel is larger than
      // the downsampling of the tiles. This would be equivalent of a simple
      // box or linear downsampling. dsFactor should be set to 1 as the 
      // frequency resolution is the one expected from the spatial resolution.
      if (dsFactor < 1)
         dsFactor = 1;
     
      // Save current tiling level in output list
      tilingLevel.insert(dsFactor);

      // Update the output tile accordingly to current tile
      for (int l = startY[pos]; l < startY[pos] + nbY[pos]; l++) 
         for (int s = startX[pos]; s < startX[pos] + nbX[pos]; s++) 
            tI->set(l,s,dsFactor);
   } 

   //zvclose(unit, NULL);
   file_model->closeFile();

   return 1;
}


