////////////////////////////////////////////////////////////////////////
// marsautoloco - Automatic registration of rover orthomosaic to basemap
////////////////////////////////////////////////////////////////////////

#include "vicmain_c"

#include "mars_support.h"

#include "PigMission.h"
#include "PigFileModel.h"
#include "PigSurfaceModel.h"
#include "PigCoordSystem.h"

//Temporary. Direct access to low-level label api until
//image_image_map projection fully integrated into
//PigFilemodel
#include "lbl_image_map_projection.h"
#include "PigLabelModel.h"
#include "PigUtilities.h"

#include "SimpleImage.h"

#include <iostream>
#include <sstream>
#include <string>
#include <fstream>
#include <vector>

// For output parameter
#include "return_status.h"
#include "taeconf.inp"
#include "parblk.inc"
#include "pgminc.inc"

#define MAX_NL 15000
#define MAX_NS 15000
#define MAX_INPUTS 2000

// Low-pass filtering and decimation utilities
void ConvBufferFast(float *buffer, float *kernel, int rsize, int ksize);
void ConvHorizontal(SimpleImage<float> *image, float *kernel, int ksize);
void ConvVertical(SimpleImage<float> *image, float *kernel, int ksize);
void GaussianBlur(SimpleImage<float> *image, float sigmaX, float sigmaY);
static void downsample(SimpleImage<float> *img_in, SimpleImage<float> *&img_out,
                       int factorX, int factorY); 

// Cross-correlation function
float correlateAtOnce(SimpleImage<float> *base, SimpleImage<float> *ortho, 
                  float &x, float &y, SimpleImage<float> *weight); 

// For variable (correlation score) output
extern "C" {
    int q_real(struct PARBLK *p, char *name, int count, double *real, int mode);
    int q_init(struct PARBLK *p, int pool_size, int mode);
}


////////////////////////////////////////////////////////////////////////
// MarsautoLoco program
////////////////////////////////////////////////////////////////////////

void main44()
{

   int i, j, k, count, countRMC, countDEM, def, nids, unitMap, unitOrtho, 
     unitDem=0, search, rmc[2], nl_b, ns_b, ns, nl, /* x, y, */ status, weight,
     proj_type, homogeneous_input = TRUE;
   const size_t msgLen = 150;
   char msg[msgLen], outputXML[150], orthoName[PIG_MAX_FILENAME_SIZE+1], 
     basemapName[PIG_MAX_FILENAME_SIZE+1], demName[PIG_MAX_FILENAME_SIZE+1],
     mission[64], instrument[64];
   float /* baselineResolution, orthoResolution, ratio, xc, yc, */ dy2, dist, dx, dy;
   double orthoNorthing, orthoEasting, orthoElevation=-99999,
     basemapNorthing, basemapEasting, demNorthing=0.0, demEasting=0.0, orthoX, 
     orthoY, orthoLineOffset, orthoSampOffset, basemapX=0.0, basemapY=0.0, corrScore, 
     demX=0.0, demY=0.0, basemapLineOffset, basemapSampOffset, demLineOffset=0.0, 
     demSampOffset=0.0 /*, line_offset, samp_offset, x_max, x_min, y_max, y_min */;
   PigFileModel *orthoFileModel, *basemapFileModel, *demFileModel;
   PigSurfaceModel *orthoSurfaceModel /* , *basemapSurface, *demSurface*/;
   PigCoordSystem *orthoCS /* , *basemapCS, *demCS*/;
   PigCameraModel *orthoCameraModel;
   PigPointingModel *orthoPointingModel;
   PigMission *missionModel;
   // PigLabelModel *basemapLabelModel;
   LblImageMapProjection_typ * lbl;

   struct PARBLK out_parb;

   zvmessage("MARSAUTOLOCO version 2019-10-02", "");

   // Open the orthomosaic. This is the image that needs to be registered to the
   // basemap
   mars_setup(nids, &orthoFileModel, &orthoCameraModel, &orthoPointingModel, 
              orthoSurfaceModel, NULL, orthoCS, mission, instrument, 
              homogeneous_input, MAX_NL, MAX_NS, MAX_INPUTS);

   if (orthoFileModel == NULL){
      zvp("INP", orthoName, &count);
      snprintf(msg, msgLen, "Unable to create file model for %s", orthoName);
      zvmessage(msg,"");
      zabend();
   }

   // The basemap, being an orbital image, is currently not supported by the 
   // PIG library, in particular the image map projection data.
   // For now, we do manual and low-level Pig function to access an 
   // initialize objects properly. Eventually, the "orbital" might be fully
   // supported by PIG and cleaning of code will be possible

   // Open the basemap
   zvp("BASEMAP", basemapName, &count);

   // Open the basemap using PigMission. Not valid information will be obtained
   // for mission name and host_id parameters. We use dump array for them. 
   // We just need to open the file and get a file unit id
   char dump[64]; 
   /* int missionId = */ PigGetMission(basemapName, &unitMap, dump, dump);
   missionModel = PigMission::getMissionObject(mission);
   basemapFileModel = missionModel->createFileModel(basemapName, unitMap);

   // Open the DEM if any. Will be used to estimate the elevation of the 
   // adjusted location
   // Same thing here regarding orbital limitation
   zvp("DEM", demName, &countDEM);
   if (countDEM) {
      demFileModel = PigFileModel::create(demName);
      if (demFileModel == NULL) {
         snprintf(msg, msgLen, "Unable to create file model for %s", demName);
         zvmessage(msg,"");
         zabend();
      }
      unitDem = demFileModel->getUnit();
   }







   // Query the orthoimage to get some information on the projection:
   // - scale of the pixel, i.e., resolution in m/pixel in X and Y directions
   // - line/sample projection offset. That is at which pixel location from the
   //   top/left pixel is located the origin of the reference system. 
   // - coordinates system of the projection
   double unused;
   status = orthoFileModel->getVertOrthoProjectionParams(&orthoSurfaceModel, 
                            orthoY, orthoX, orthoLineOffset, orthoSampOffset, 
                            unused, unused, unused, unused, proj_type, &orthoCS);

   orthoLineOffset -= 1; //pixel location is 1-based
   orthoSampOffset -= 1;


   // Overwrite label parameters with User input, if any.
   // If label reading failed because SURFACE_PROJECTION_PARMS property was not
   // in the label, parameter MUST be supplied by user.
   //
   // TODO: TEMPORARY
   // For now, the image map coordinates w.r.t baseline map coordinates (i.e., 
   // absolute map coordinates usually in Equidistant Cylindrical projection) 
   // needs to be supplied by user. This is because PIG does not support orbital
   // map projection. Eventually, a piece of code might be able to fetch these 
   // information directly from PLACES and/or might be supplied in the labels.
   
   // Get the northing and easting of origin of the orthomosaic CS in basemap
   // map projection coordinates. The location of that northing/easting w.r.t
   // the image itself is given by ORTHO_L_OFFSET/ORTHO_S_OFFSET or read from
   // the label
   zvparmd("ORTHO_NORTHING", &orthoNorthing, &count, &def, 1, 0);
   if (count == 0) {
      zvmessage("User needs to supply INP Northing","");
      zabend();
   }
   zvparmd("ORTHO_EASTING",  &orthoEasting,  &count, &def, 1, 0);
   if (count == 0) {
      zvmessage("User needs to supply INP Easting","");
      zabend();
   }

   // Get the pixel resolution of the orthomosaic
   zvparmd("ORTHO_X_SCALE", &orthoX, &count, &def, 1, 0);
   if (count == 0 && status != 0) {
      zvmessage("Scale in X not found in label for INP image, must be supplied via parameters", "");
      zabend();
   }
   zvparmd("ORTHO_Y_SCALE", &orthoY, &count, &def, 1, 0);
   if (count == 0 && status != 0) {
      zvmessage("Scale in Y not found in label for INP image, must be supplied via parameters", "");
      zabend();
   }

   // Get the pixel coordinates that correspond to the map coordinates supplied in 
   // ORTHO_NORTHING/ORTHO_EASTING
   zvparmd("ORTHO_L_OFFSET", &orthoLineOffset, &count, &def, 1, 0);
   if (count == 0 && status != 0) {
      zvmessage("Pixel Y offset not found in label for INP image, must be supplied via parameters", "");
      zabend();
   }
   if (count == 1)
      orthoLineOffset -= 1;

   zvparmd("ORTHO_S_OFFSET", &orthoSampOffset, &count, &def, 1, 0);
   if (count == 0 && status != 0) {
      zvmessage("Pixel X offset not found in label for INP image, must be supplied via parameters", "");
      zabend();
   }
   if (count == 1)
      orthoSampOffset -= 1;



   

   // Here, we use low-level Pig library to access basemap mapping information
   // because PIG does not support (yet?) orbital imagery with Image Map 
   // Projection properties
   lbl = new LblImageMapProjection_typ;
   LblSetImageMapProjection("IMAGE_MAP_PROJECTION");
   status = LblImageMapProjectionApi(unitMap, LBL_READ, lbl, 1);
   if (status != 0) 
      zvmessage("No Image Map Projection labels could be read from base map","");

   // Manually extract relevant mapping information on the base map from the
   // label structure

   if (lbl->MapScale.Valid) {
      basemapX = lbl->MapScale.Value;
      basemapY = lbl->MapScale.Value;
   }

   if (lbl->LineProjectionOffset.Valid)
      basemapNorthing = lbl->LineProjectionOffset.Value * basemapY;

   if (lbl->SampleProjectionOffset.Valid)
      basemapEasting = -(lbl->SampleProjectionOffset.Value) * basemapX;

   if (lbl->LineFirstPixel.Valid)
      basemapLineOffset = lbl->LineFirstPixel.Value - 1; // label 1-based

   if (lbl->SampleFirstPixel.Valid)
      basemapSampOffset= lbl->SampleFirstPixel.Value - 1; // label 1-based



   // Overwrite label parameters with User input, if any

   // Get the northing and easting of the basemap
   zvparmd("MAP_NORTHING", &basemapNorthing, &count, &def, 1, 0);
   if (count == 0 && lbl->LineProjectionOffset.Valid == 0) {
      zvmessage("Map northing not found in labels for base map, must be supplied via parameters", "");
      zabend();
   }

   zvparmd("MAP_EASTING",  &basemapEasting,  &count, &def, 1, 0);
   if (count == 0 && lbl->SampleProjectionOffset.Valid == 0) {
      zvmessage("Map easting not found in labels for base map, must be supplied via parameters", "");
      zabend();
   }

   // Get the pixel resolution of the basemap
   zvparmd("MAP_X_SCALE", &basemapX, &count, &def, 1, 0);
   if (count == 0 && lbl->MapScale.Valid == 0) {
      zvmessage("Base map X scale not found in labels for base map, must be supplied via parameters", "");
      zabend();
   }

   zvparmd("MAP_Y_SCALE", &basemapY, &count, &def, 1, 0);
   if (count == 0 && lbl->MapScale.Valid == 0) {
      zvmessage("Base map Y scale not found in labels for base map, must be supplied via parameters", "");
      zabend();
   }

   // Get the origin of the basemap. That is, at which pixel (line,samp) does
   // the northing and easting corresponds. Should usually be top/left
   zvparmd("MAP_L_OFFSET", &basemapLineOffset, &count, &def, 1, 0);
   if (count == 0 && lbl->LineFirstPixel.Valid == 0) {
      zvmessage("Pixel Y offset not found in labels for base map, must be supplied via parameters", "");
      zabend();
   }
   if (count == 1) {
      basemapLineOffset -= 1;
   }

   zvparmd("MAP_S_OFFSET", &basemapSampOffset, &count, &def, 1, 0);
   if (count == 0 && lbl->SampleFirstPixel.Valid == 0) {
      zvmessage("Pixel X offset not found in labels for base map, must be supplied via parameters", "");
      zabend();
   }
   if (count == 1) {
      basemapSampOffset -= 1;
   }



   // No need for basemap labels anymore
   delete lbl;




   // Get the DEM filename, if any
   zvp("DEM", demName, &countDEM);

   if (countDEM) {
      // Get the mapping information of the DEM image from the label. They will
      // be overwritten if supplied by user
      // Same thing here, we use low-level pig api
      lbl = new LblImageMapProjection_typ;
      LblSetImageMapProjection("IMAGE_MAP_PROJECTION");
      status = LblImageMapProjectionApi(unitDem, LBL_READ, lbl, 1);
      if (status != 0) 
         zvmessage("No Image Map Projection labels could be read from DEM file","");



      // Manually extract relevant mapping information on the base map from the
      // label structure

      if (lbl->MapScale.Valid) {
         demX = lbl->MapScale.Value;
         demY = lbl->MapScale.Value;
      }

      if (lbl->LineProjectionOffset.Valid)
         demNorthing = lbl->LineProjectionOffset.Value * demY;

      if (lbl->SampleProjectionOffset.Valid)
         demEasting = -(lbl->SampleProjectionOffset.Value) * demX;

      if (lbl->LineFirstPixel.Valid)
         demLineOffset = lbl->LineFirstPixel.Value - 1; // label 1-based

      if (lbl->SampleFirstPixel.Valid)
         demSampOffset= lbl->SampleFirstPixel.Value - 1; // label 1-based


   
      // Overwrite label parameters with User input, if any

      // Get the northing and easting of the DEM
      zvparmd("DEM_NORTHING", &demNorthing, &count, &def, 1, 0);
      if (count == 0 && lbl->LineProjectionOffset.Valid == 0) {
         zvmessage("Map northing not found in labels for DEM, must be supplied via parameters", "");
         zabend();
      }

      zvparmd("DEM_EASTING",  &demEasting,  &count, &def, 1, 0);
      if (count == 0 && lbl->SampleProjectionOffset.Valid == 0) {
         zvmessage("Map easting not found in labels for DEM, must be supplied via parameters", "");
         zabend();
      }

      // Get the pixel resolution of the DEM
      zvparmd("DEM_X_SCALE", &demX, &count, &def, 1, 0);
      if (count == 0 && lbl->MapScale.Valid == 0) {
         zvmessage("DEM X scale not found in labels for base map, must be supplied via parameters", "");
         zabend();
      }

      zvparmd("DEM_Y_SCALE", &demY, &count, &def, 1, 0);
      if (count == 0 && lbl->MapScale.Valid == 0) {
         zvmessage("DEM Y scale not found in labels for base map, must be supplied via parameters", "");
         zabend();
      }

      // Get the origin of the dem. That is, at which pixel (line,samp) does
      // the northing and easting corresponds. Should usually be top/left
      zvparmd("DEM_L_OFFSET", &demLineOffset, &count, &def, 1, 0);
      if (count == 0 && lbl->LineFirstPixel.Valid == 0) {
         zvmessage("Pixel X offset not found in labels for DEM, must be supplied via parameters", "");
         zabend();
      }
      if (count == 1) {
         demLineOffset -= 1;
      }

      zvparmd("DEM_S_OFFSET", &demSampOffset, &count, &def, 1, 0);
      if (count == 0 && lbl->SampleFirstPixel.Valid == 0) {
         zvmessage("Pixel Y offset not found in labels for DEM, must be supplied via parameters", "");
         zabend();
      }
      if (count == 1) {
         demSampOffset -= 1;
      }

      // No need for labels anymore
      delete lbl;

   }



   // Get the output XML filename
   zvp("OUT", outputXML, &count);


   // Get the search range for the baseline vs ortho-mosaic cross-correlation. 
   // SEARCH 
   zvparmd("SEARCH_RANGE", &search, &count, &def, 1, 0);

   // In default correlation, all pixels have the same weight during the
   // correlation. However, because of ortho and/or basemap residual, it is not 
   // always possible to get good registration everywhere. Weight is a parameter
   // allowing to force the registration to be good near the rover to the 
   // detriment of farther (less confidence in theses areas anyway).
   weight = 0;
   if (zvptst("WEIGHT"))
      weight = 1;


   // Retrieve the RMC.
   // The RMC is not used in this program, it's just transfered to the output.
   // It is a mandatory input, even if it could be retrieved from the label. 
   // The input is checked for safety against the one in the label
   status = zvparm("RMC", rmc, &countRMC, &def, 2, 0);
   if (status != 1 || countRMC == 0) {
      zvmessage("Wrong RMC input","");
      zabend();
   }

   // For safety, check that the RMC entered by the user matches the one in the
   // file.
   PigCSReference *csr = orthoCS->getIdentity();

   if (csr->getNumIndices() != countRMC) {
      zvmessage("WARNING: Number of RMC values do not correspond to parameters!","");
   }
  
   for (int i=0; i<countRMC; i++) {
      if (rmc[i] != csr->getIndex(i)) {
         if (i==0) zvmessage("WARNING: First RMC value from file and user differ!",""); 
         else  zvmessage("WARNING: Second RMC values from file and user differ!",""); 
      }
      
   }



   // Map coordinates are "attached" to the top/left corner of the pixel but the
   // actual value of the pixel is "attached" to the center.  This is the case 
   // for both the map  and orthomosaic images. For consistency with the 
   // registration process which is based on pixel values, we compute the map 
   // coordinates of the top/left pixels of the map and orthomosaic at the 
   // center of these pixels.
   double basemapOx = (basemapEasting - basemapSampOffset * basemapX) + basemapX/2.0;
   double basemapOy = (basemapNorthing + basemapLineOffset * basemapY) - basemapY/2.0;

   double orthoOx = orthoEasting  - orthoSampOffset * orthoX + orthoX/2.0;
   double orthoOy = orthoNorthing + orthoLineOffset * orthoY - orthoY/2.0;



   // To compare (via correlation) the basemap and the ortho image, we need 
   // first to downsample the ortho image to the same resolution of the basemap.
   // Once it is done, we'll be able to compute the size of the basemap subset
   // that needs to be retrieved from the file. Basemap can be very large, so
   // we want to avoid loading all of it in memory.

   // Get the ratio between the pixel resolution (pixel scale) of the basemap
   // image and the ortho image. This ratio is needed to appropriately 
   // downsample the orthomosaic to the resolution of the base image
   double ratioX = basemapX / orthoX;
   double ratioY = basemapY / orthoY;
   snprintf(msg, msgLen, "Resolution ratio between base map and image: (%5.2f, %5.2f).", 
           ratioX, ratioY);
   zvmessage(msg, "");
 
   // Open the ortho-mosaic and load the image in memory
   // Load current ortho-mosaic

   // Get ortho image dimensions
   zvget(orthoFileModel->getUnit(), "NL", &nl, "NS", &ns, NULL);

   // If current file is open, close it first
   if (orthoFileModel->isFileOpen()) {
      orthoFileModel->closeFile();
      zvclose(orthoFileModel->getUnit(), "CLOS_ACT","FREE", NULL);
   }
   
   // Open the file for reading
   zvopen(orthoFileModel->getUnit(), "OP", "READ", "U_FORMAT", "REAL", 
          "OPEN_ACT", "SA", NULL);
   orthoFileModel->setFileOpen(TRUE);

   unitOrtho = orthoFileModel->getUnit();

   // Load the image in memory
   SimpleImage<float> *mosaic = new SimpleImage<float>(nl, ns);
   for (j=0; j < nl; j++) 
      zvread(unitOrtho, mosaic->linePtr(j), "BAND", 1, "LINE", j+1, NULL);

   // Close the ortho file
   zvclose(unitOrtho, "clos_act", "free", NULL);




   // The downsampling of the ortho mosaic is done in two steps:
   // [1] - lowpass filtering
   // [2] - decimation 

   zvmessage("Rescaling the image to the base map scale...","");

   // [1] Gaussian lowpass filtering
   // The magnitude of the filtering depends on the sigma (stddev) of the 
   // Gaussian kernel which itself should depend on the resolution ratio.
   // It has been shown that: sigma = C * sqrt(t^2 -1 ) with C = 0.8, and 
   // t = decimation factor gives a good compromise
   double sigmaX = 0.8 * sqrt(ratioX*ratioX - 1);   
   double sigmaY = 0.8 * sqrt(ratioY*ratioY - 1);   
   GaussianBlur(mosaic, sigmaX, sigmaY);  // low pass the mosaic

   
   // [2] Decimation of the lowpass'd image
   // In theory we should interpolate instead of just decimating if the ratioX 
   // and/or ratioY are not integer. However, if ratio >> 1, the error made by 
   // rounding the ratio to the nearest integer is negligible.
   // Note that the downsample is made is such a way that the map coordinates
   // of the top/left pixel of the downsampled image is the same as the
   // original one.
   // mosaic will now contains the downsampled mosaic.
   downsample(mosaic, mosaic, (int)(ratioX + 0.5), (int)(ratioY + 0.5)); 

   // Get the number of line and sample of the downsampled ortho image
   ns = mosaic->getNS();
   nl = mosaic->getNL();





   // Compute the subset in the basemap to extract. This will depends on the 
   // estimated location of the orthomosaics and on its size

   // Compute the coordinates of the ortho top/left pixel in the basemap
   float xs_raw = (orthoOx - basemapOx) / basemapX; 
   float ys_raw = (basemapOy - orthoOy) / basemapY;

   // Depending on the orthoOx/y and mapOx/y, the xs_raw, ys_raw are not
   // necessarily integer. We round them, but we need to keep that information 
   // for later correction
   int xs = (int)(xs_raw + 0.5);
   int ys = (int)(ys_raw + 0.5);

   // Compute the bottom/right pixel coordinates of the ortho in the basemap
   int xe = xs + ns - 1;
   int ye = ys + nl - 1;

   // Enlarge that subset with the space needed for the search
   int xs_search = xs - search;
   int ys_search = ys - search;
   int xe_search = xe + search;
   int ye_search = ye + search;

   // Size of the basemap subset
   int ns_basemap = ns + 2*search;
   int nl_basemap = nl + 2*search;


   


   // Get base image dimensions
   zvget(basemapFileModel->getUnit(), "NL", &nl_b, "NS", &ns_b, NULL);

   // Check that subset is withing bounds
   if (xs_search < 0 || xe_search > (ns_b-1) || 
       ys_search < 0 || ye_search > (nl_b-1)) {
      zvmessage("Error. Required base map footprint steps out of bounds","");
      zabend();
   }

   // Load the basemap subset

   // Open the base file
   // If current file is open, close it first
   if (basemapFileModel->isFileOpen()) {
      basemapFileModel->closeFile();
      zvclose(basemapFileModel->getUnit(), "CLOS_ACT","FREE", NULL);
   }
   
   // Open the file for reading
   zvopen(basemapFileModel->getUnit(), "OP", "READ", "U_FORMAT", "REAL", 
          "OPEN_ACT", "SA", NULL);
   basemapFileModel->setFileOpen(TRUE);

   // Load base in memory
   unitMap = basemapFileModel->getUnit();
   SimpleImage<float> *base = new SimpleImage<float>(ns_basemap, nl_basemap);
   for (i=0; i < nl_basemap; i++) 
      zvread(unitMap, base->linePtr(i), "BAND", 1, "LINE", ys_search+i+1, 
             "SAMP", xs_search+1, "NSAMPS", ns_basemap, NULL);

   // Close the file
   zvclose(unitMap, "clos_act", "free", NULL);




   // The basemap subset and the downsampled orthomosaic are ready to be 
   // compared
   
   // Create a weigthing matrix and set the weight accordingly to user's choice.
   // The weighting matrix define a "weight" for each pixel of the downsampled 
   // orthomosaic. These weight will give the pixels more or less importance 
   // during the correlation.
   // There are currently two options:
   // - No weight: all weight are set to 1
   // - Weight: weights are set inverse proportional to the distance from the 
   // "special" pixel, the one to which is attached the map coordinate
   // (orthoSampOffset, orthoLineOffset). With rover mosaic, it should most of 
   // the time correspond to the rover location. The closer pixels are to the 
   // rover, the more weigth they have, because: 
   // 1) these matter most than far away, 
   // 2) orthomosaic quality and resolution degrades with distance from rover 
   SimpleImage<float> *weightMat = new SimpleImage<float>(nl, ns);

   // Set all weights to 0
   weightMat->zero(); 

   if (weight) {
      float xc = float(orthoSampOffset)/((int)(ratioX + 0.5));  
      float yc = float(orthoLineOffset)/((int)(ratioY + 0.5));
      for (j=0; j<nl; j++) {
         dy2 = (j-yc)*(j-yc); // dist in line direction pre-computation
         for (k=0; k<ns; k++) {
            dist = sqrt(dy2 + (k-xc)*(k-xc));
            if (dist > 1.0)
               weightMat->set(j,k, 1.0/dist); // we leave pixels closer than 1
         }                                    // with a null weight for 
                                              // numerical stability.
      }
   }
   else {
      for (j=0; j<nl; j++) {
         for (k=0; k<ns; k++) {
            weightMat->set(j,k, 1.0);
         }
      }
   }

   if (weight)
      zvmessage("Using weighted correlation.","");
   else
      zvmessage("Using non-weighted correlation.","");

   // CORRELATE!
   // This is where offset between basemap and orthomosaic is estimated.
   // The output of the correlation is an offset in x and y direction and
   // a flag that indicates if the correlation found a maximum similarity
   // within the search area. If corrScore = 0, that means that the maximum
   // correlation was found on the edges of the search area.
   corrScore = (double)correlateAtOnce(base, mosaic, dx, dy, weightMat); 

   // Done with actual images
   base->free();
   mosaic->free();


   // Save correlation correlation score to TAE 
   q_init(&out_parb, 50, P_ABORT);
   q_real(&out_parb, "CORR_SCORE", 1, &corrScore, P_ADD);
   zvq_out(&out_parb);

   // If correlation failed, nothing to compute or save, exit program
   if (corrScore == 0) {
      zvmessage("Correlation unsuccessful! No output generated","");
      return;
   }



   // Compute the new ortho map coordinate values based on the shift found 
   // between the basemap and the orthomosaic.
   // Essentially, dx and dy contain the map coordinates correction to bring 
   // to the rover location.

   // Convert location in the search grid in offset in basemap pixel.
   // dx/dy are now the pixel offset between basemap and orthomosaic
   dx = dx - search;
   dy = dy - search;

   // Correct that offset from the earlier shift caused by the rounding.
   dx = dx + (xs - xs_raw);
   dy = dy + (ys - ys_raw);

   // Finally update the ortho origin location
   orthoEasting  = orthoEasting + dx * basemapX;
   orthoNorthing = orthoNorthing - dy * basemapY;

   snprintf(msg, msgLen, "Correlation score: %3.2f   -   Offset (X,Y) meters: %5.2f, %5.2f",
           corrScore, dx*basemapX, -dy*basemapY);
   zvmessage(msg, "");

   // Get the elevation of the new rover location if a DEM has been suppled by 
   // the user.
   if (countDEM) {

      // Correct DEM coordinates for coordinates attachment to the top-left 
      // pixel. 
      double demOx = (demEasting - demSampOffset * demX) + demX/2.0;
      double demOy = (demNorthing + demLineOffset * demY) - demY/2.0;

      // Get the pixel location of the ortho map coordinates in the DEM
      double x = (orthoEasting - demOx) / demX;
      double y = (demOy - orthoNorthing) / demY;

      // Define the subset to extract from the DEM for a bilinear
      // interpolation of the elevation
      xs = (int)x;
      ys = (int)y;
      xe = xs + 1;
      ye = ys + 1;
   
      // Load the DEM subset

      // Get base image dimensions
      int nlDem, nsDem;
      zvget(unitDem, "NL", &nlDem, "NS", &nsDem, NULL);

      // Check that the needed elevation location is within DEM footprint
      if (x<0 || x>(nsDem-1) || y<0 || y>(nlDem-1)) {
         zvmessage("Location outside DEM footprint. No elevation retrieved","");
         zvclose(unitDem, "clos_act", "free", NULL);
         goto doneDEM;
      }

      // Load base in memory
      SimpleImage<float> *dem = new SimpleImage<float>(2, 2);
      for (i=0; i < 2; i++) 
         zvread(unitDem, dem->linePtr(i), "BAND", 1, "LINE", ys+i+1, 
                "SAMP", xs+1, "NSAMPS", 2, NULL);

      // Close the file
      zvclose(unitDem, "clos_act", "free", NULL);

      // Get location to interpolate
      x -= xs;
      y -= ys;
 
      // Linear interpolation of elevation
      orthoElevation = (1.0-x) * ((1.0-y)*dem->get(0,0) + y*dem->get(0,1)) + 
                       x * ((1-y)*dem->get(1,0) + y*dem->get(1,1));

      dem->free();
   }

   doneDEM:



   // Write outputXML file if correlaton succeeded.
   FILE *file = fopen(outputXML, "w");
   if (!file) {
      zvmessage("Unable to open output file","");
      zabend();
   }

   fprintf(file, "<solution solution_id=\"auto_loc_^\">\n");
   if (countRMC == 1 || rmc[1] == 0)
      fprintf(file, "<rmc frame=\"SITE\" site=\"%i\"/>\n", rmc[0]);
   else
      fprintf(file, "<rmc frame=\"ROVER\" site=\"%i\" drive=\"%i\"/>\n",
              rmc[0], rmc[1]);
   fprintf(file, "<offset x=\"%f\" y=\"%f\" z=\"%f\" >\n",
                 orthoNorthing, orthoEasting, orthoElevation);
   fprintf(file, "<solution_metadata>\n");
   fprintf(file, "<derived_from frame=\"ORBITAL\" site=\"1\"/>\n");
   fprintf(file, "<reference_rmc frame=\"ORBITAL\" site=\"0\"/>\n");
   fprintf(file, "<reference_view>telemetry</reference_view>\n");         
   fprintf(file, "</solution_metadata>\n");          
   fprintf(file, "</offset>\n");
   fprintf(file, "<toView>best_pos</toView>\n");          
   fprintf(file, "<toView>localized_pos</toView>\n");          
   fprintf(file, "<toView>auto_loc</toView>\n");          
   fprintf(file, "<origination institution=\"JPL\" method=\"Localization team update\" user=\"rdmslopgs\"/>\n");
   fprintf(file, "</solution>\n");

   fclose(file);

   zvmessage("New location written to OUT file","");

}

// end of main







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
void ConvHorizontal(SimpleImage<float> *image, float *kernel, int ksize)
{
  int r, c, i, halfsize;
  int width = image->getNS();
  int height = image->getNL();
  float buffer[width+ksize-1];
  halfsize = ksize / 2;

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
}




// Same as ConvHorizontal, but apply to vertical columns of image.
//void ConvVertical(vector<float>& image, int width, int height, 
void ConvVertical(SimpleImage<float> *image, float *kernel, int ksize)
{
  int r, c, i, halfsize;
  int width = image->getNS();
  int height = image->getNL();
  float buffer[height+ksize-1];
  halfsize = ksize / 2;

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
}



// Convolve image with a Gaussian of width sigma and store result back in image.
// This routine creates the Gaussian kernel, and then applies it in horizontal 
// and vertical directions.
void GaussianBlur(SimpleImage<float> *image, float sigmaX, float sigmaY)
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
  ConvHorizontal(image, kernelX, ksizeX);

  // Convolve image with gaussian kernel in the vertical direction
  ConvVertical(image, kernelY, ksizeY);
}





// Downsample the given image with a step of *factor*.  The output image is
// re-allocated if non-null, and then the original output is deleted
// *after* the operation.  This allows the input to be the same as the
// output on entry (in which case the input will be deleted).
static void downsample(SimpleImage<float> *img_in,
		       SimpleImage<float> *&img_out,
                       int factorX, int factorY)
{
    int nl = img_in->getNL();
    int ns = img_in->getNS();
    SimpleImage<float> *save_output = img_out;
    img_out = new SimpleImage<float>((nl-1)/factorY + 1, (ns-1)/factorX + 1);
    img_out->zero();

    for (int j=0; j < nl; j+=factorY) {
	int jj = j / factorY;
      for (int i=0; i < ns; i+=factorX) {
	    int ii = i / factorX;
	    img_out->set(jj,ii, img_in->get(j,i));
	}
    }
    if (save_output != NULL)
	delete save_output;
}






// Correlation of the entire image at once.
// Correlate all the valid points of the ortho-mosaic with the baseline in one
// single correlation. Valid points are points in the ortho that are non null. 
// It is assumed that the base does not contains null (=0) values. If it does, 
// they are not considered invalid values.
// We do a weighted correlation, that is, pixels have a different weight in the
// correlation result.


float correlateAtOnce(SimpleImage<float> *base, SimpleImage<float> *ortho, 
                   float &x, float &y, SimpleImage<float> * weight)
{

   int i, j, k, nb, searchX, searchY;
   int ns,nl,nsb,nlb;
   float meanOrtho, varOrtho, meanBase, varBase, sum, val, val2, covar, sumW,
         mC=-100;
   // char msg[150];
 
   // Get base and ortho sizes and search range
   ns = ortho->getNS();
   nl = ortho->getNL();
   nsb = base->getNS();
   nlb = base->getNL();
   searchX = nsb - ns + 1;
   searchY = nlb - nl + 1;


   // In case a weight matrix is not provided, create one with uniform weight
   if (weight == NULL) {
      weight = new SimpleImage<float>(nl, ns);
      for (i=0; i<nl; i++)
         for (j=0; j<ns; j++)
            weight->set(j,i,1.0);
   }
  
   // Initialize the correlation array 
   std::vector<std::vector<float> > correlation;
   correlation.resize(searchY);
   for (i=0; i<searchY; i++)
      correlation[i].resize(searchX);


   // Locate in the ortho the location of non-null pixels 
   // Take the opportunity of looping through the ortho to compute the weigthed
   // sum of the pixel values (to get the mean eventually) which will be used in
   // the correlation.
   std::vector<int> locX;
   std::vector<int> locY;
   sum = 0.0;
   sumW = 0.0;
   nb = 0;
   for (i=0; i<nl; i++) {
      for (j=0; j<ns; j++) {
         val = ortho->get(i,j);     // get ortho pixel value
         if (val != 0) {
            val2 = weight->get(i,j);   // get associated weight
            locX.push_back(j);
            locY.push_back(i);
            sum += val * val2;
            sumW += val2;
            nb++;
         }
      }
   }

   if (nb == 0) {
      zvmessage("Correlation failed, all pixels of ortho are null","");
      return 0;  
   }

   // Get the mean of the ortho values   
   meanOrtho = sum / sumW;

   // Compute the variance of the orthos
   varOrtho = 0;
   for (i=0; i<nb; i++)
      varOrtho += weight->get(locY[i], locX[i]) *
                  pow(meanOrtho - ortho->get(locY[i], locX[i]), 2);

   varOrtho /= sumW;



   // Compute the mean and variance of the base for each search position 

   for (i=0; i<searchY; i++) {
      for (j=0; j<searchX; j++) {

         sum = 0.0;
        
         // Compute the mean
         for (k=0; k<nb; k++) 
            sum += weight->get(locY[k], locX[k]) *
                   base->get(i+locY[k], j+locX[k]);
         

         meanBase = sum / sumW;

         // Compute the variance
         varBase = 0;
         for (k=0; k<nb; k++)        
            varBase += weight->get(locY[k], locX[k]) * 
                       pow(meanBase - base->get(i+locY[k], j+locX[k]),2);

         varBase /= sumW;

         // Compute covariance
         covar = 0;
         for (k=0; k<nb; k++)        
            covar += weight->get(locY[k], locX[k]) * 
                     (meanOrtho - ortho->get(locY[k], locX[k])) *
                     (meanBase - base->get(i+locY[k], j+locX[k])); 

         correlation[i][j] = (covar / sumW) / sqrt(varOrtho * varBase);

      }
   }

   // Locate the correlation maximum. 
   // No subpixel refinement here.
   for (i=0; i<searchY; i++) {
      for(j=0; j<searchX; j++) {
         if (correlation[i][j] > mC) {
            mC = correlation[i][j];
            x = (float)j;
            y = (float)i;
         }
      }
   }

   // If correlation maximum is on the search range border, return failed 
   // correlation as there is no guarantee that the maximum is reached.
   // Note that because of the simple max finding approach, a noisy situation
   // could lead to outliers.
   if (x != 0 && x != (searchX - 1) && y !=0 && y != (searchY - 1)) 
      return mC;
   else
      return 0.0;
}







