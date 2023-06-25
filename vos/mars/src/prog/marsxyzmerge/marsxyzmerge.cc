/////////////////////////////////////////////////////////////////////////
// marsxyzmerge - Point cloud merging program
////////////////////////////////////////////////////////////////////////
#include "vicmain_c"

#include "mars_support.h"
#include "mars_tiepoints.h"

#include "PigMission.h"
#include "PigFileModel.h"
#include "PigCameraModel.h"
#include "PigPointingModel.h"
#include "PigSurfaceModel.h"
#include "PigVector.h"
#include "RadiometryModel.h"
#include "PigCoordSystem.h"
#include "PigCAHV.h"
#include "PigCAHVOR.h"
#include "PigCAHVORE.h"
#include "PigCSReference.h"
#include "PigMER.h"
#include "PigSurfacePlane.h"

#include <string.h>

#include "octreeNode.h"

/* buffer sizes in main program */
#define MAX_INPUTS 200

#define MAX_NL MARS_MAX_NL
#define MAX_NS MARS_MAX_NS



void loadXYZ(float x[], float y[], float z[], PigFileModel * file_model);
void pointCloudFilter(OctreeNode * node, int split_max, char * strategy); 


////////////////////////////////////////////////////////////////////////
// MarsXYZmerge program
////////////////////////////////////////////////////////////////////////

void main44()
{
    int i, j, k, index, unit_out;
    int ns, nl, nb;
    int splitThreshold, count;

    // Inputs

    int nids, out_nids, def;
    PigFileModel *file_models[MAX_INPUTS];
    PigCameraModel *camera_in_all[MAX_INPUTS];
    PigPointingModel *pointing_in_all[MAX_INPUTS];
    PigSurfaceModel *surface_model_all;
    int homogeneous_inputs = TRUE;
    PigCoordSystem *output_cs, *cs, *xyz_cs;


    // User Parameters
    char mission[64], instrument[64];

    zvmessage("MARSXYZMERGE version 2019-12-13", "");

    // Read in the list of input files and initialize a bunch of geometry,
    // projection, surface, and other related  variables. We don't need all of
    // them in this program, but among them are what we need.
    mars_setup(nids, file_models, camera_in_all, pointing_in_all, 
               surface_model_all, NULL, output_cs, mission, instrument,
               homogeneous_inputs, MAX_NL, MAX_NS, MAX_INPUTS);

    PigMission *m = PigMission::getMissionObject(mission);
    

    // Read in the list of output filenames
    char **out_filenames = new (std::nothrow) char *[MAX_INPUTS];
    if (out_filenames == NULL) {
        zvmessage("Memory error in filename array", "");
        zabend();
    }
    mars_get_filelist("OUT", out_nids, out_filenames, MAX_INPUTS, FALSE);


    // Get the maximum number of points in the octree leaf to carry the
    // density analysis 
    zvparmd("MAX_SPLIT" , &splitThreshold, &count, &def, 1, 0);

    // Get the strategy for filtering the points. For now, two strategies are
    // available: Order and Density
    char strategy[128];
    zvp("STRATEGY", strategy, &count);
    

    // Iterate over the list of input files to retrieve some information about
    // the files: size of file (NS, NL), 3-band vs 3 1-band files.
   
    // triIndex tracks whether the current file is part of a 3 1-band file 
    // series. If a 1-band file is found, it is expected that 2 more 1-band 
    // files are directly following and that are all of same size.
    int triIndex = 0;
 
    // Containers to store information about the input XYZ files. By 
    // construction, the number of XYZ files is equal or smaller (if 1-band
    // files are used) to the number of input files (nids).
    int ns_all[nids];
    int nl_all[nids];
    int nb_all[nids];

    // Number of XYZ files. This is not necessarily equals to the number
    // of input files. 3 1-band files account for 1 XYZ dataset. If all input 
    // files were 3-bands file, then number of XYZ files would be equal to the 
    // number of input files (nids).
    int nbXYZ = 0;


    for (i=0; i<nids; i++) { 

       // Get the number of band of the current file
       int nb = file_models[i]->getNB();

       // If the number of bands is 3 and if we are not in the middle of a 
       // 1-band trio, then move on to save the file info. Otherwise, if
       // we are in an on-going 1-band file trio, throw an error
       if (nb == 3 && triIndex == 0) {
          ns_all[nbXYZ] = file_models[i]->getNS();
          nl_all[nbXYZ] = file_models[i]->getNL();
          nb_all[nbXYZ] = 3;
          nbXYZ += 1;
       }
       else if (nb == 1) {
          // triIndex is the 1-band file trio counter. Its value indicates if
          // a new trio is starting, if we are in an on-going trio
          // listing, or if we are ending a trio listing
          switch (triIndex) {
             // 0: This is a new 1-band file. Initialize counter. 2 1-band files
             // to go to have a complete trio.
             case 0: triIndex = 2; 
             // 1: This is the last 1-band file of a trio. The trio is 
             // complete, save NS/NL  
             case 1: {
                       triIndex -= 1;
                       int ns = file_models[i-2]->getNS();
                       int nl = file_models[i-2]->getNS();

                       // Check that the size of the three files are identical
                       if (ns != file_models[i-1]->getNS() || 
                           nl != file_models[i-1]->getNL() ||
                           ns != file_models[i]->getNS() || 
                           nl != file_models[i]->getNL()) {
                           zvmessage("Error: 1-band images trio have different size","");
                           zabend();
                       }

                       ns_all[nbXYZ] = ns;
                       nl_all[nbXYZ] = nl;
                       nb_all[nbXYZ] = 1;
                       nbXYZ += 1;
                    }
             // This is the second 1-band file. One more to go.
             case 2: triIndex -= 1;
          }
       }
       else {
          zvmessage("Incorrect listing of input XYZ files or number of bands","");
          zabend();
       }    
    }


    // Check that the number of input XYZ files is coherent with the number of 
    // entries in the output file list. There are three possibilities. Depending
    // on the number of output entries, the output will either be:
    // - 3-band files: All XYZ files are saved as 3-band files, whether the 
    //   inputs are in a 3-band or 1-band formats
    // - 1-band or 3-band files: All XYZ files are saved in the same input 
    //   format. 3-band input -> 3-band output,  1-band input -> 1-band output
    // - 1-band files: All XYZ files are saved as 3 1-band files, whether the 
    //   inputs are in a 3-band or 1-band formats
    // The following variable (outFormat) store the output format information:
    // -1: only 1-band image
    //  0: follow input format
    //  1: only 3-band image
    int outFormat = 0;
    if (out_nids == nbXYZ)
       outFormat = 1;
    else if (out_nids == 3*nbXYZ)
       outFormat = -1;
    else if (out_nids == nids)
       outFormat = 0;
    else {
       zvmessage("Number of entries in IN and OUT are inconsistent.","");
       zabend();
    }


    // Now that the verification are done, iterate over the files, load the 
    // X, Y, Z data in memory and store them into a vector container.
    
    // Get Coordinate system of the first file. This will be our common 
    // coordinate system. This is arbitrary and the actual coord system is not 
    // of importance. What matters is that all XYZ must be expressed in a 
    // common coord system to allow comparison.   
    PigCSReference *ref;
    file_models[0]->getDerivedImageCS(ref);
    xyz_cs = m->getCoordSystem(ref);

    // Initialize the point cloud vector and reserve memory space to avoid 
    // reallocation during loading of the XYZ values in memory
    vector< vector<Point> > pc(nbXYZ);
    for (i=0; i<nbXYZ; i++)
       pc[i].reserve(ns_all[i]*nl_all[i]);


    // Iterate over the files and load in the data 
    index = 0;
    for (i=0; i<nbXYZ; i++) {

       // Initialize the arrays that will contain the XYZ data of the current
       // file
       ns = ns_all[i];
       nl = nl_all[i];
       nb = nb_all[i];

       float* x = new (std::nothrow) float[ns*nl];
       float* y = new (std::nothrow) float[ns*nl];
       float* z = new (std::nothrow) float[ns*nl];
       if (x == NULL || y == NULL || z == NULL) {
          zvmessage("Unable to allocate memory for loading XYZ file.","");
          zabend();
       }

       // Load the XYZ data into the arrays
       loadXYZ(x, y, z, file_models[index]);

       // Check that the XYZ are expressed in the common coordinates system. If
       // not, convert.
       //
       // Get coordinate system of current file
       file_models[index]->getDerivedImageCS(ref);
       cs = m->getCoordSystem(ref);
       if (cs != xyz_cs) {
          for (j=0; j<ns*nl; j++) {
             if (x[j] == 0.0 && y[j] == 0.0 && z[j] == 0.0)
                continue;
             PigPoint xyz(x[j], y[j], z[j]);
             PigPoint new_xyz = xyz_cs->convertPoint(xyz, cs);
             x[j] = new_xyz.getX();
             y[j] = new_xyz.getY();
             z[j] = new_xyz.getZ();
          }
       }

       // Get the XYZ of the current file into a Point structure format and add 
       // it to the main point clouds vector
       for (j=0; j<ns*nl; j++) {
          if (x[j] == 0.0 && y[j] == 0.0 && z[j] == 0.0)
             continue;
          Point p = {x[j], y[j], z[j], j};
          pc[i].push_back(p);
       }

       delete[] x;
       delete[] y;
       delete[] z;

       // Update the index depending if file was a 3-bands or 1-band file
       index += (nb == 3) ? 1 : 3; 
    }



    // Initialize the Octree with the XYZ data
    // Note that octree takes ownership of pc and modify its content.
    OctreeNode octree = OctreeNode(&pc); 


    // Core part of the program, apply the spatial filtering.
    // The filtering keeps the points in the octree that are to be removed from
    // the XYZ files.
    pointCloudFilter(&octree, splitThreshold, strategy);

    // Time to retrieve the points that are left in the octree and that will be 
    // removed from the file

    // First, clear pc to reuse it as a container to store the points to be
    // removed from the files
    for (i=0; i<pc.size(); i++)
      vector<Point>().swap(pc[i]); // swap() instead of clear() to effectively
                                   // release the memory

    // Get the points
    octree.getPointClouds(pc);


    // Iterate over each point cloud, load the image in memory, set the 
    // corresponding  points to (0,0,0), and write the resulting image to disk. 
    // Note: the opening/loading of the rasters has already been done and we 
    // could keep them in memory instead of deleting them and reloading. It has 
    // been implemented this way to save memory footprint which can become large
    // if there are several images. Another option would be to map memory. Or do
    // a better job (i.e., different structure) for the octree class.

    index = 0;
    for (i=0; i<nbXYZ; i++) {

       ns = ns_all[i];
       nl = nl_all[i];
       nb = nb_all[i];

       float* x = new (std::nothrow) float[ns*nl];
       float* y = new (std::nothrow) float[ns*nl];
       float* z = new (std::nothrow) float[ns*nl];
       if (x == NULL || y == NULL || z == NULL) {
          zvmessage("Unable to allocate memory for loading XYZ file.","");
          zabend();
       }

       // Load the XYZ data into the array containers
       loadXYZ(x, y, z, file_models[index]);

       // For the current point cloud, check if there are points to remove. A 
       // point is removed by setting its XYZ values to 0,0,0
       if (pc[i].size() != 0) {
          for (j=0; j< pc[i].size(); j++) {
             Point point = pc[i][j];
             x[point.index] = 0.0;
             y[point.index] = 0.0;
             z[point.index] = 0.0;
          } 
       }


       // Ready to save the updated XYZ to disk

       // Case where the filtered XYZ image is to be saved as 3 1-band files
       if (outFormat == -1 || (outFormat == 0 && nb == 1)) {
          for (j=0; j<3; j++) {
             // First, make sure the correct Label is being 
             // transfered to the new file
             zvselpiu(file_models[index+j]->getUnit());

             // Open the new file and write output labels
             zvunit(&unit_out, "NONE", 1, "u_name", out_filenames[index+j], NULL);
             zvopen(unit_out, "op", "write", "u_ns", ns, "u_nl", nl, "u_nb", 1, 
                    "open_act", "sa", "u_org", "bsq", "u_format", "real", 
                    "o_format", "real", NULL);
             //PigLabelModel *labelModel = m->createLabelModel(unit_out);
             //labelModel->setRange(file_models, nids, rangeOrigin, cs);
             zvplabel(unit_out, 0, 1);      

             // Save XYZ to disk
             switch(j) {
                case(0):{
                         for (k=0; k<nl; k++) 
                            zvwrit(unit_out, x+k*ns, "BAND", 1, "LINE", k+1, NULL);
                        }
                case(1):{
                         for (k=0; k<nl; k++) 
                            zvwrit(unit_out, y+k*ns, "BAND", 1, "LINE", k+1, NULL);
                        }
                case(2):{
                         for (k=0; k<nl; k++) 
                            zvwrit(unit_out, z+k*ns, "BAND", 1, "LINE", k+1, NULL);
                        }
             }

             zvclose(unit_out,"CLOS_ACT","FREE", NULL);
          }
       }

       // Case where the filtered XYZ :wimage is to be saved as 3-band file
       else if (outFormat == 1 || (outFormat == 0 && nb == 3)) {
          // First, make sure the correct Label is being 
          // transfered to the new file
          zvselpiu(file_models[index]->getUnit());

          // Open the new file and write output labels
          zvunit(&unit_out, "NONE", 1, "u_name", 
                 out_filenames[outFormat == 1 ? i : index], NULL);
          zvopen(unit_out, "op", "write", "u_ns", ns,"u_nl", nl, "u_nb", 3, 
                 "open_act", "sa", "u_org", "bsq", "u_format", "real",
                 "o_format", "real", NULL);
          //PigLabelModel *labelModel = m->createLabelModel(unit_out);
          //labelModel->setRange(file_models, nids, rangeOrigin, cs);
          zvplabel(unit_out, 0, 1);      

          // Save XYZ to disk
          for (k=0; k<nl; k++) 
             zvwrit(unit_out, x+k*ns, "BAND", 1, "LINE", k+1, NULL);
          for (k=0; k<nl; k++) 
             zvwrit(unit_out, y+k*ns, "BAND", 2, "LINE", k+1, NULL);
          for (k=0; k<nl; k++) 
             zvwrit(unit_out, z+k*ns, "BAND", 3, "LINE", k+1, NULL);

          zvclose(unit_out,"CLOS_ACT","FREE", NULL);
       }

       else {    
          zvmessage("Internal error, invalid outFormat number", "");
          zabend();
       } 

       // Free memory
       delete [] x;
       delete [] y;
       delete [] z;
  
       // Update the index depending if current input was a 3-bands file or a 
       // 1-band trio files
       if (nb_all[i] == 3)
          index += 1;
       else
          index += 3;
    }   

}





void pointCloudFilter(OctreeNode * node, int split_max, char * strategy) {


   if (node->isLeaf()) {

      // Get number of points from each point clouds contained in that node.
      // pointCloud contains the number of points for each point clouds. For 
      // instance is pointClouds = [10, 0, 56, 0] this means that this node
      // contains 10 points (XYZ) from the first point cloud, 0 from the second
      // point cloud, 56 from the third, and 0 for the fourth. The number of 
      // elements in this vector (4 in the example) is defined when the octree
      // is initialized and is fixed througout the life of the octree.
      vector<int> pointClouds = node->getPointCloudsSize();

      // Compute total number of points in that node and the number of non-empty
      // point cloud represented in that node
      int nbPoints = 0; // Total number of points (XYZ) contained in the node
      int nbClouds = 0; // Total of point clouds represented in this node. Would
                        // be 2 in th previous example.
      for (int i=0; i<pointClouds.size(); i++) {
         nbPoints += pointClouds[i];
         if (pointClouds[i] != 0)
            nbClouds += 1;
      }


      // If number of point is 0, it's an empty node, nothing to do
      if (nbPoints == 0)
         return;

      // If number of point Cloud represented in the node is 1, then there is 
      // nothing to filter as there is only one point cloud available. 
      // Delete all data in the node and return
      if (nbClouds == 1) {
         for (int i=0; i<pointClouds.size();i++)
            node->clearPointCloud(i);
         return;
      }

      // If number of point is larger than limit, need to split the node and 
      // iterate the filtering process on the node's children- We're still too 
      // zoomed out to carry a local density analysis
      if (nbPoints > split_max) {
         int status = node->split();
         if (status) {
            zvmessage("Error during node split memory allocation","");
            zabend();
         }
         pointCloudFilter(node, split_max, strategy);
      }

      // Number of points is about right to filter the node. Depending on the 
      // strategy, the filtering will either be:
      // - based on density: The points from the most represented point cloud 
      //   are removed. Remember, the filtering leaves the points that are
      //   ultimately to be removed from the files, so by removing the points 
      //   from the densest point cloud, we are actually keeping them ultimately
      // or
      // - based on point clouds input order: The points from the first point 
      //   cloud in the list are removed. This strategy is usefull when merging 
      //   point clouds of similar density (e.g., mosaicing). Again, the removal
      //   of the points from the first point cloud in the list means that they
      //   are actually kept in the merged point cloud files.
      else {
         int winnerCloudId = 0;

         // Order strategy
         if (strcmp(strategy, "ORDER") == 0) {
            for (int i=0; i<pointClouds.size(); i++) {
               if (pointClouds[i] != 0) {
                  winnerCloudId = i;
                  break;
               }
            }
         }
         // Density strategy
         else {
            for (int i=1; i<pointClouds.size();i++) {
               if (pointClouds[winnerCloudId] < pointClouds[i])
                  winnerCloudId = i;
            }
         }

         // Delete the points from the winner point clouds - the other points
         // will ultimately be deleted from the files.
         node->clearPointCloud(winnerCloudId);
      }
         
   }
   // The node is not a leaf, keep digging.
   else {
      for (int i=0; i<8; i++) 
         pointCloudFilter(node->getChild(i), split_max, strategy);
   }
}




// Function to load in a vector container the XYZ raster of the given 
// PigFileModel. There are 2 cases. Either the file is a 3-band file in which
// case the bands get loaded one by one, or the file is a 1-band file. In case
// of 1-band file it is EXPECTED that file_model points to the first band (X)
// file AND that the 2 next file_model pointer locations points to the Y and
// Z band of the XYZ data. 
void loadXYZ(float x[], float y[], float z[], PigFileModel * file_model)
{
   int i;
   int units[3], bands[3], wasClosed=0;

   int ns = file_model->getNS();
   int nl = file_model->getNL();
   int nb = file_model->getNB();

   if (nb != 3 && nb != 1) {
      zvmessage("Incorrect number of bands in XYZ file (must be 3 or 1)","");
      zabend();
   } 


   // Depending if 1 3-band file or 3 1-band file, store corresponding unit
   // and band number
   for (i=0; i<3; i++) {
      units[i] = (file_model + ((nb == 3) ? 0 : i))->getUnit();
      bands[i] = (nb == 3) ? (i+1) : 1;
   }

   // Make sure the file is open
   if (!file_model->isFileOpen()) {
      file_model->openFile();
      file_model->setFileOpen(TRUE);
      wasClosed=1;
   }

   // Read the XYZ file(s)
   for (i=0; i<nl; i++) {
      zvread(units[0], x+i*ns, "BAND", bands[0], "LINE", i+1, NULL);
      zvread(units[1], y+i*ns, "BAND", bands[1], "LINE", i+1, NULL);
      zvread(units[2], z+i*ns, "BAND", bands[2], "LINE", i+1, NULL);
   }
  
   // Close the file if it was orginally closed 
   if (wasClosed)
      file_model->closeFile();
}


