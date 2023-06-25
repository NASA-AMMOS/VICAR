/////////////////////////////////////////////////////////////////////////
// marsrefmesh - Mesh Refinement based on photo-consistency program
////////////////////////////////////////////////////////////////////////

#include "vicmain_c"

#include "xvector.h"

#include "mars_support.h"


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


#ifdef _OPENMP
#include <omp.h>
#endif

#include "ceres/ceres.h"
//#include "ceres/types.h"
//#include "ceres/normal_prior.h"   
//#include "glog/logging.h"

#define ENABLE_STATIC_LIB 
#include "rtcore.h"
#include <unordered_set>
#include <memory>
#include <assert.h>
#include <ctime>
#include <limits>
#include "SimpleImage.h"
#include <iostream>
#include <fstream>
#include <iomanip>   //for cout with precision

#include <xmmintrin.h>
//#include <pmmintrin.h> // use this to get _MM_SET_DENORMALS_ZERO_MODE when compiling for SSE3 or higher

#if !defined(_MM_SET_DENORMALS_ZERO_MODE)
#define _MM_DENORMALS_ZERO_ON   (0x0040)
#define _MM_DENORMALS_ZERO_OFF  (0x0000)
#define _MM_DENORMALS_ZERO_MASK (0x0040)
#define _MM_SET_DENORMALS_ZERO_MODE(x) (_mm_setcsr((_mm_getcsr() & ~_MM_DENORMALS_ZERO_MASK) | (x)))
#endif

#ifndef PI
#define PI 3.14159265358979323846
#endif

#define MAX_VALENCE 20

#define TINYOBJLOADER_USE_DOUBLE
#ifdef TINYOBJLOADER_IMPLEMENTATION
#undef TINYOBJLOADER_IMPLEMENTATION
#endif
#include "tiny_obj_loader.h"

using namespace std;

/* buffer sizes in main program */
#define MAX_INPUTS 2000
#define MAX_THREADS 256

#define MAX_NL MARS_MAX_NL
#define MAX_NS MARS_MAX_NS


template<typename T> 
vector <vector<T> > union_find(vector< vector<T> > tiePointsVect);
int saveMesh(char * basename, double * vertices, unsigned int nbVertices,
             unsigned int * triangles, unsigned int nbTriangles, float * textures);
void parseFilename(char *name, std::string &fullName, std::string &path, 
                  std::string &filename, std::string &basename, std::string &ext); 




class MeshRefiner : public ceres::FirstOrderFunction {

   public:

      MeshRefiner(int nids, PigFileModel *file_models[], 
                  PigCameraModel *camera_in[], int *refimg, 
                  unsigned int nbVertices, double *vertices, 
                  unsigned int nbTriangles, unsigned int *triangles,
                  float lambda, float sigma, unsigned int nbSigma);
  
   
      virtual bool Evaluate(const double* parameters,
                            double* cost,
                            double* gradient) const; 

      virtual int NumParameters() const { return nbVertices*3; }

      void setPrintInfo(const int val) {printInfo = val;};


      void setLambda(const float lambdaIn) {lambda = lambdaIn;}; 
      float getLambda() {return lambda;}; 

      void setSigma(const float sigmaIn) {sigma = sigmaIn;}; 
      float getSigma() {return sigma;}; 

      void setNbSigma(const unsigned int nbSigmaIn) {nbSigma = nbSigmaIn;}; 
      float getNbSigma() {return nbSigma;};

      void applyTexture(const double * vertices, const int * textureIds, 
                        float * textures);
 
   private:


      // Parameters     
      const int nids;
      PigFileModel **file_models;
      PigCameraModel **camera_in;
      const int *refimg;
      unsigned int nbVertices;
      float *vertices_f;
      unsigned int nbTriangles;
      unsigned int *triangles;
      float sigma;
      unsigned int nbSigma;

      int printInfo;

      // Regularization weight
      float lambda;

      // Internal 
      std::vector<std::unordered_set<unsigned int> > oneRingTriangles;
      std::unordered_set<unsigned int> edgeVertices;
      std::unordered_set<unsigned int> edgeVertices2;
      std::vector<std::vector<unsigned int> > orientedOneRing;
      float inf = std::numeric_limits<float>::infinity();

      // Embree - Ray tracer variables
      RTCDevice device;
      RTCScene scene;
      RTCGeometry mesh;


      // Functions/Procedures

      // Compute the 1-ring vertices of each vertex of the mesh
      std::vector<std::unordered_set<unsigned int> > computeOneRingVertices();

      // Compute the 1-ring triangles of each vertex of the mesh
      std::vector<std::unordered_set<unsigned int> > computeOneRingTriangles();

      // Compute the "oriented" 1-ring vertices of each vertex
      std::vector<std::vector<unsigned int> > computeOrientedOneRing(
                    std::vector<std::unordered_set<unsigned int> > oneRingTriangles,
                    const std::unordered_set<unsigned int> edgeVertices);

      // Function to identify all vertices that are on the edge of the mesh
      std::unordered_set<unsigned int> locateEdgeVertices();

      // Generate an array of Embree::HitRay for each pixel of an image
      void computeImageRays(const int &index, const int &ns, const int &nl, 
                            RTCRayHit *& rays) const;

      // Load an image in memory
      SimpleImage<float> * loadImage(int index) const;

      // Resample image according to mapping matrices
      SimpleImage<float> * resampleImage(SimpleImage<float> *image, 
                                         SimpleImage<float> *& matrices) const;

      // Correlation
      void correlate(SimpleImage<float> * master, SimpleImage<float> * slave,
                     float *corrScore, float *corrDeriv) const;

      // Gauss kernel computation
      void getGaussKernel(float * kernel ) const; 

      // Compute the two principal curvature (K1, K2) of the mesh at each vertex
      void computePrincipalCurvatures(const double * vertices, 
                                      double * principalCurvatures) const; 

      // Scaled Umbrella operator to approximate the Laplacian
      void applyUmbrellaOperator(const double * vertices, double * laplacian,
                         std::unordered_set<unsigned int> edgeVertices) const;
};








////////////////////////////////////////////////////////////////////////
// Marsmeshrefine program
////////////////////////////////////////////////////////////////////////

// NOTES
// There are a series of improvements that can/need to be done:
// - Add a parameter that limits angle between face and look angle. A strong 
//   incidence angle could possibly bring more noise than information. Even more
//   so given the basic bicubic interpolation (no accounting of downsampling 
//   factor - aliasing).
// - By construction a lot of pixels will be projected right on the triangle
//   edges or vertices. Thid can lead to some instabilities. One way would be 
//   to wiggle a bit the vertices with some low amp noise. For instance, with 
//   a white noise of max 5% of average edge length.





void main44()
{

  // For best performence with Embree
  _MM_SET_FLUSH_ZERO_MODE(_MM_FLUSH_ZERO_ON);
  _MM_SET_DENORMALS_ZERO_MODE(_MM_DENORMALS_ZERO_ON);


   int i, status;
   int count, def;
   const size_t msgLen = 256;
   char msg[msgLen];
   unsigned int index;

   // Inputs

   int nids;
   PigFileModel *file_models[MAX_INPUTS];
   PigCameraModel *camera_in_all[MAX_INPUTS];
   PigPointingModel *pointing_in_all[MAX_INPUTS];
   PigSurfaceModel *surface_model;
   int homogeneous_inputs = TRUE;
   PigCoordSystem *output_cs;


   int refimg[MAX_INPUTS];
   int textureIds[MAX_INPUTS];

   // Original input params before nav file; used for saving nav file
   double pparams_in_save[MAX_INPUTS][PIG_MAX_PARAMS];

   // User Parameters
   char mission[64], instrument[64];
   int evalOnly;


   char meshIn[150];
   char meshOut[150];
   unsigned int minVerticesPerPatch, nbSigma, nbRemoved;
   double maxDist, minDist, lambda, sigma;
   double inf = std::numeric_limits<double>::infinity();

   // Some Ceres objects, 
   // There are a lot more options to adjust if needed in Ceres interface. 
   // Only a subset are parametrized so far.
   ceres::Problem::Options optionsProblem;     // Options for the problem
   ceres::Solver::Options optionsSolver;       // Options for the NLLS solver
   ceres::Problem::EvaluateOptions optionsEval;// Options for output evaluation



    ///////////////////////////////////////////////////////////////////////////



    zvmessage("MARSREFMESH version 2019-12-10", "");

    // Initialization of PIG elements.
    mars_setup(nids, file_models, camera_in_all, pointing_in_all, 
               surface_model, NULL, output_cs, mission, instrument,
	       homogeneous_inputs, MAX_NL, MAX_NS, MAX_INPUTS);


    // Get the initial pointing
    for (i = 0; i < nids; i++) {
	pointing_in_all[i]->getPointingParameters(pparams_in_save[i],
							PIG_MAX_PARAMS);
    }


    // Are we multi-threading?
    //int omp_on = zvptst("OMP_ON");

    // Get the input
    zvp("IN_MESH", meshIn, &count);

    zvp("OUT_MESH", meshOut, &count);

   
    // Dry run?
    // That is, simple estimation of the initial data and regularization cost 
    // and print out of some information. This will be useful to estimate a 
    // weigting (LAMBDA) for the regularization for which there is no hard and
    // fast rule.
    evalOnly = zvptst("EVALUATE");

    // Get the number of vertices that an isolated mesh patch must contains for
    // it to be part of the refinement. Below that value, that patch is not
    // refined
    zvparmd("MIN_VERTICES", &minVerticesPerPatch, &count, &def, 1, 0); 

    // Get the maximum distance allowed between a mesh vertex and the reference
    // cameras. Above that distance, faces containing that vertex are removed
    // from the refinement
    zvparmd("MAX_DIST", &maxDist, &count, &def, 1, 0); 
    if (maxDist < 0)
        maxDist = inf;
 
    // Get the minimum distance allowed between a mesh vertex and the reference
    // cameras. Below that distance, faces containing that vertex are removed
    // from the refinement
    zvparmd("MIN_DIST", &minDist, &count, &def, 1, 0); 

    // Get the weigthing to apply to the regularization. The higher the value,
    // the more regularized (smoothed) will be the mesh.
    zvparmd("LAMBDA", &lambda, &count, &def, 1, 0); 

    // Get the information on the correlation Gaussian patch size, i.e., std
    // deviation of the Gaussian (in pixel), and number of std deviation
    zvparmd("SIGMA", &sigma, &count, &def, 1, 0); 
    zvparmd("NB_SIGMA", &nbSigma, &count, &def, 1, 0); 


    // Get which images will be used to texture the mesh
    for (i=0; i<MAX_INPUTS; i++)
	textureIds[i] = -1;

    status = zvparm("TEXTURE_IDS", textureIds, &count, &def, MAX_INPUTS, 0);
    if (status==1) {
       for (i=0; i<count; i++) {
          if (textureIds[i] > nids) {
             zvmessage(" TEXTURE_IDS valu bigger than # of inputs!","");
             zabend();
          }
          textureIds[i] -= 1;
       }
    }
    else {
       zvmessage("TEXTURE_IDS could not be read correctly, defaulting to first image","");
       textureIds[0]=0;
    }


    // Get which images are references images. A reference image will have all 
    // the other images projected onto it and correlated. More than one image
    // can be references. In fact all images can be reference, which would mean
    // that each image will have all the others projected onto it.
    for (i=0; i<nids; i++)
	refimg[i] = FALSE;

    int refimg_array[MAX_INPUTS];
    status = zvparm("REFIMAGES", refimg_array, &count, &def, MAX_INPUTS, 0);

    if (status == 1 && count > 0) {
	for (i=0; i < count; i++) {
	    if (refimg_array[i] == -1) {
                refimg[0] = TRUE;
		continue;	
            }
	    if (refimg_array[i] > nids || refimg_array[i] < -nids) {
		zvmessage("REFIMAGES value bigger than # of inputs!", "");
		zabend();
	    }
	    if (refimg_array[i] <= 0) {
		if (i < 1 || refimg_array[i-1] <= 0 ||
			refimg_array[i-1] >= -refimg_array[i]) {
		    zvmessage("REFIMAGES value less than 0 and prior ref not correct (neg or > this)", "");
		    zabend();
		}
		for (int j = refimg_array[i-1]; j <= -refimg_array[i]; j++) {
		    refimg[j-1] = TRUE;
		}
	    }
	    else {

	        refimg[refimg_array[i]-1] = TRUE;
	    }
	}
    }
    else refimg[0] = TRUE; 


    // Update the list of reference images if needed
    if (zvptst("UNTIL")) {
	if (count < 1 || refimg_array[0] <= 0) {
	    zvmessage("Invalid entry for REFIMAGE with UNTIL specified", "");
	    zabend();
	}
	for (i=0; i < refimg_array[0]; i++)
	    refimg[i] = TRUE;
    }


    // Look at the ignore array.  Any image in this list will be removed from 
    // the reference image list.
    int ignore_array[MAX_INPUTS];
    status = zvparm("IGNORE", ignore_array, &count, &def, MAX_INPUTS, 0);
    if (status == 1 && count > 0) {
        for (i=0; i < count; i++) {
            if (ignore_array[i] > nids | ignore_array[i] < -nids) {
                zvmessage("IGNORE value bigger than # of inputs!", "");
                zabend();
            }
            if (ignore_array[i] <= 0) {
                if (i < 1 || ignore_array[i-1] <= 0 ||
                        ignore_array[i-1] >= -ignore_array[i]) {
                    zvmessage("IGNORE value less than 0 and prior ignore not correct (neg or > this)", "");
                    zabend();
                }
                for (int j = ignore_array[i-1]; j <= -ignore_array[i]; j++) {
                    refimg[j-1] = FALSE;
                }
            }
            else {
                refimg[ignore_array[i]-1] = FALSE;
            }
        }
    }






   
//*****************************************************************************
   zvmessage("","");
   zvmessage("**********************************************************","");
   zvmessage("","");
//*****************************************************************************

   // Read and Load the mesh (OBJ file)

   zvmessage("Reading the mesh file...","");
   zvmessage(" ","");
 
   tinyobj::attrib_t attrib;
   std::vector<tinyobj::shape_t> shapes;
   std::vector<tinyobj::material_t> materials;

   std::string err;
   bool ret = tinyobj::LoadObj(&attrib, &shapes, &materials, &err, meshIn,
                              NULL, false);

   if (!ret) {
      zvmessage("Failed to load/parse mesh file.","");
      if (!err.empty()) {
         strncpy(msg, err.c_str(), msgLen);         
         msg[msgLen-1] = '\0'; //strncpy doesn't always null terminate
         zvmessage(msg,"");
      }
      zabend();
   }
   else 
      zvmessage("Successfully loaded mesh file:","");

   // NOTE:
   // For code simplification, and because it is the OBJ format expected a the
   // time of this program writing, it is assumed that the OBJ file contains
   // only 1 shape and is made of triangular faces. Although tinyobj will 
   // read any OBJ properly, the code that follows won't process them correctly
   // and would need to be (slightly?) modified

   // Some verifications based on comment above for safety.
   
   // Check that number of shapes in OBJ is equal to 1
   if (shapes.size() != 1) {
      zvmessage("OBJ file should contain only 1 shape","");
      zabend();
   }  

   // Check that OBJ only contains triangular faces
   for (unsigned int f = 0; f<shapes[0].mesh.num_face_vertices.size(); f++) {
      if (shapes[0].mesh.num_face_vertices[f] != 3) {
         zvmessage("OBJ file should contain only triangular faces","");
         zabend();
      }
   }  


   // Retrieve relevant information on the loaded mesh   

   // Number of vertices
   unsigned int nbVertices = attrib.vertices.size() / 3;

   // Number of triangles
   unsigned int nbTriangles = shapes[0].mesh.num_face_vertices.size(); 

   // Vector of indices. Get the values of interest so we can get rid of 
   // the shapes[0].mesh path
   std::vector<tinyobj::index_t> indicesV = std::move(shapes[0].mesh.indices);


    snprintf(msg, msgLen, "# of vertices  : %d", nbVertices);
    zvmessage(msg,"");

    snprintf(msg, msgLen, "# of normals  : %ld",(attrib.normals.size() / 3));
    zvmessage(msg,"");

    snprintf(msg,msgLen, "# of texture coords  : %ld",(attrib.texcoords.size() / 3));
    zvmessage(msg,"");

    snprintf(msg, msgLen, "# of materials  : %ld", materials.size());
    zvmessage(msg,"");

    snprintf(msg, msgLen, "# of triangular faces  : %d", nbTriangles);
    zvmessage(msg,"");




   

   // Initialize vertices and triangles masks that will be used to identify 
   // which vertices/triangles need to be remove from the mesh refinement 
   // process. 
   // Several reasons can lead to the discard of a triangles and vertices:
   // - isolated mesh patch of small number of triangles (threshold to be 
   // defined by the user)
   // - faces that are too far/close from the camera, i.e., far background, 
   // spurious vertex in the sky, etc... (threshold for *far/close* to be 
   // defined by the user)


   vector<int> invalidVertices(nbVertices,0); 
   vector<int> invalidTriangles(nbTriangles,0); 

   // Need to convert all camera CS into the mesh CS.
   // WARNING: it is assumed the the output of mars_setup CS is the same as
   // mesh CS. This is not necessarily true. CS of mesh should be read from the
   // lbl. 
   for (int i=0; i<nids; i++)
      camera_in_all[i]->setCoordSystem(output_cs);



//*****************************************************************************
   zvmessage("","");
   zvmessage("**********************************************************","");
   zvmessage("","");
//*****************************************************************************


   // In the following two sections, we are identifying vertices that are 
   // considered invalid per user parameter. For now, there are two criteria
   // available:
   // - Vertices that are too far/close from the reference cameras
   // - Vertices belonging to small, isolated patches of triangles
    
   zvmessage("Pre-processing mesh filtering:","");
   zvmessage(" ",""); 


   // In this section we are flagging the faces that are far from the 
   // references camera as invalid faces.
   // The distance between the camera and each vertex is computed. Then all
   // faces containing vertices that are farther than distance threshold are
   // flagged as invalid
   // Note that this filtering is applied for all the references of the list.
   
   zvmessage("Checking for mesh vertices position w.r.t. cameras...","");
   zvmessage(" ",""); 

   // This is only done if the user set up a min/max distance
   if (maxDist < inf || minDist > 0) {
   
      double dist, X, Y, Z, Xref, Yref, Zref;
      PigPoint c;
      PigVector a, h, v;

      double closestDist = inf;
      double farthestDist = 0.0;
      nbRemoved = 0;

      for (int i=0; i < MAX_INPUTS; i++) {

         if (refimg[i] == false)
            continue;

         // Get X,Y,Z of reference camera
         c = pointing_in_all[i]->getCameraPosition(output_cs);
         Xref = c.getX();
         Yref = c.getY();
         Zref = c.getZ();

         // Iterate over the vertices and store the indices of the ones too far
         // or too close
         for (unsigned int v = 0; v < nbVertices; v++) {
            X = attrib.vertices[3 * v + 0];
            Y = attrib.vertices[3 * v + 1];
            Z = attrib.vertices[3 * v + 2];

            dist = sqrt((Xref-X)*(Xref-X) + 
                        (Yref-Y)*(Yref-Y) + 
                        (Zref-Z)*(Zref-Z));

            if (dist > farthestDist) farthestDist = dist;
            if (dist < closestDist) closestDist = dist;

            if (dist > maxDist || dist < minDist) {
               invalidVertices[v] = 1;
               nbRemoved++;
            }
         }
      }   

       // Now that the too far/close vertices have been identified, we flag the 
       // triangles which contain these vertices as invalid.

       for (unsigned int t = 0; t < nbTriangles; t++) {

          // Check that vertices contained in the current triangle are not 
          // part of the invalidVertices list. If one of the three vertices
          // is part of that list, flag the current triangle as invalid

          for (unsigned int v = 0; v < 3; v++) {
            
             index = indicesV[t*3+v].vertex_index;
          
             if (invalidVertices[index]) {
                invalidTriangles[t] = 1;
                break;
             }
            
          }
       }


       // Print out some information about the distance filtering that was just 
       // applied
       snprintf(msg, msgLen, "Closest vertex is %.3fm from reference cameras.", closestDist);
       zvmessage(msg,"");

       snprintf(msg,msgLen,"Farthest vertex is %.3fm from reference cameras.", farthestDist);
       zvmessage(msg,"");
       
       if (maxDist < inf)
          snprintf(msg, msgLen,"Min/Max user limits for valid range: %.3fm  %.3fm", 
                  minDist, maxDist);
       else 
          snprintf(msg, msgLen, "Min/Max user limits for valid range: %.3fm  inf",minDist);
       zvmessage(msg,"");

       snprintf(msg, msgLen, "%d (%4.2f%%) vertices out of valid range.",
               nbRemoved, (float)nbRemoved/nbVertices*100.0);
       zvmessage(msg,"");

    }
    else 
        zvmessage("No filtering based on distance between vertices and cameras.","");

    zvmessage(" ","");
    zvmessage("-------","");
    zvmessage(" ","");




   // This part is to identify the different triangles groups, that is we
   // want to know how many disconnected patch of triangles there are, and
   // the number of vertices contained in each groups.
   // The end goal is to discard isolated and small patches of vertices and 
   // triangles.
   // To do that we'll use the union-find algorithm developed for the 
   // bundle adjustment (looking at tracks among a set of tiepoints), by 
   // simulating tie points out of the triangles.
   // 1 triangle (v1,v2,v3) -> tp(v1,v2), tp(v1,v3)

   zvmessage("Checking for mesh connectivity...","");
   zvmessage(" ",""); 

   // Get number of vertices pairs (fake tiepoints) that will be created
   // i.e., 2 tiepoints per triangle:
   // triangle ABC ->  tiepoint (AB) and tiepoint (AC)
   long nbvp = nbTriangles * 2;


   // Initialize the vector containing all the *fake tiepoints*.
   // Each tiepoints contains the id of two (and only two) vertices
   vector<vector<unsigned int> > verticesPairs(nbvp);
   for (unsigned int j = 0; j < nbvp; j++)
      verticesPairs[j].reserve(2);
        

   // Iterate on the number of triangles and add 2 tiepoints for 
   // each triangle 
   for (unsigned int t = 0; t < nbTriangles; t++) {

      verticesPairs[t*2].push_back(indicesV[3*t].vertex_index);
      verticesPairs[t*2].push_back(indicesV[3*t+1].vertex_index);
  
      verticesPairs[t*2+1].push_back(indicesV[3*t].vertex_index);
      verticesPairs[t*2+1].push_back(indicesV[3*t+2].vertex_index);

   }


   // We have now a fake tiepoint list representing the mesh triangles
   // connectivity. Apply the union-find algorithm to find out the 
   // disconnected groups of triangles.
   vector< vector<unsigned int> > patches = union_find(verticesPairs);

   snprintf(msg, msgLen, "%ld disconnected mesh patches.", patches.size());
   zvmessage(msg, "");


   // Print out the size (in number of vertices) of each patches
   zvmessage("Size of disconnected mesh patches (in # vertices):","");
   
   // First compute the length of the char array
   int sz = 0;
   for (auto i:patches) 
      sz += std::to_string(i.size()).length();

   // Initiate char array with room for spacing patches size (2-element wide space)
   size_t numMsgChars = sz + 2 * patches.size();
   char *  msg2 = new (std::nothrow) char[numMsgChars];
   char * pos = msg2;

   // Fill the array
   for (unsigned int i=0; i < patches.size(); i++) {
      if (i) {
         size_t usedSpace = pos-msg2;
         if(numMsgChars <= usedSpace)
            break;
         pos += snprintf(pos,numMsgChars - usedSpace,", ");
      }

      size_t usedSpace = pos-msg2;
      if(numMsgChars <= usedSpace)
            break;

      pos += snprintf(pos,numMsgChars - usedSpace, "%ld", patches[i].size());
   }
   zvmessage(msg2, "");
   delete [] msg2;


   // Counter for number of removed vertices
   nbRemoved = 0;
   int nbPatchesRemoved = 0;
        

   // Get the the vertices that belong to too small isolated patches
   for (unsigned int j=0; j < patches.size(); j++) {
      if (patches[j].size() < minVerticesPerPatch) {
         for (unsigned int k=0; k < patches[j].size(); k++) {
            invalidVertices[patches[j][k]] = 1;
            nbRemoved++;
         }
         nbPatchesRemoved++;
      }
   }
  
        
   // Update the mask for triangles containing vertices that were identified
   // as being part of too small isolated patches

   for (unsigned int t = 0; t < nbTriangles; t++) {

      // Check that vertices contained in the current triangle are not 
      // part of the invalidVertices list. If one of the three vertices
      // is part of that list, flag the current triangle as invalid

      for (unsigned int v = 0; v < 3; v++) {
            
         index = indicesV[t*3+v].vertex_index;
          
         if (invalidVertices[index]) {
            invalidTriangles[t] = 1;
            break;
         }
      }
   }

   snprintf(msg, msgLen, "Patch size user limit (# of vertices): %d", minVerticesPerPatch);
   zvmessage(msg,"");
   snprintf(msg, msgLen, "%d patches too small.", nbPatchesRemoved);
   zvmessage(msg,"");
   snprintf(msg, msgLen, "%d (%4.2f%%) vertices removed from refinement.",
               nbRemoved, (float)nbRemoved/nbVertices*100.0);
   zvmessage(msg,"");

   zvmessage("","");
   zvmessage("-------","");
   zvmessage(" ","");






   // Last verification on the cleaned mesh.
   // Verify that all vertices are part of a triangle, i.e., no isolated vertex,
   // and that all triangles' vertices are part of the valid vertices list

   zvmessage("Checking for dangling vertices...","");
   zvmessage(" ","");

   nbRemoved = 0;

   // Initiate a vertices index array, with all vertices flag set to 1.
   std::vector<int> varr(nbVertices,1);

   // Set flag of identified invalid vertices to 0
   for (unsigned int i = 0; i < nbVertices; i++) {
      if (invalidVertices[i])
         varr[i] = 0;
   }

   // Iterate over all valid triangles and set vertices of the triangles to 0
   for (unsigned int t = 0; t < nbTriangles; t++) {

      if (invalidTriangles[t])
         continue;

      for (unsigned int v = 0; v < 3; v++) 
         varr[indicesV[t*3+v].vertex_index] = 0;
   }

   // Normally, there should not be some varr elements left to 1. If that is the
   // case that means that these particular vertices are isolated and not part
   // of a triangles. They need to be removed. This could happen given some
   // mesh configuration and filtering done previously
   
   // Check that all vertices flags are 0. If not, add that vertex to the 
   // invalid list.

   for (unsigned int i = 0; i < nbVertices; i++) {
      if (varr[i]) {
         invalidVertices[i] = 1;
         nbRemoved++;
      }
   }


   // Check that all valid triangles are made of vertices that are valid
   // Iterate over all valid triangles and set vertices of the triangles to 0
   for (unsigned int t = 0; t < nbTriangles; t++) {

      if (invalidTriangles[t])
         continue;

      for (unsigned int v = 0; v < 3; v++){ 
         if(invalidVertices[indicesV[t*3+v].vertex_index]) 
            invalidTriangles[t] = 1;
      }
   }


   snprintf(msg, msgLen,"%d (%4.2f%%) invalid vertices found during final check.",
               nbRemoved, (float)nbRemoved/nbVertices*100.0);
   zvmessage(msg,"");
   zvmessage("(Distance and patch filtering can leave dangling vertices)","");

   zvmessage(" ","");
   zvmessage("-------","");
   zvmessage(" ","");





   // Get the total number of vertices removed from the refinement process.

   nbRemoved = 0;
   for (unsigned int i = 0; i < nbVertices; i++) 
      if (invalidVertices[i])
         nbRemoved++;
   snprintf(msg, msgLen, "A total of %d (%4.2f%%) vertices are removed from refinement", 
           nbRemoved, (float)nbRemoved/nbVertices*100.0);
   zvmessage(msg, "");





//*****************************************************************************
   zvmessage("","");
   zvmessage("**********************************************************","");
   zvmessage("","");
//*****************************************************************************


   // Once invalid vertices, per user requirements, are identified, a new set of
   // arrays containing only the valid vertices and triangles are created with 
   // invalid ones removed. Alternatively, we could have kept the original 
   // arrays and use a mask, but it would not have been convenient for later use
   // with Embree (ray tracer) and Ceres Solver. In addition to that, Embree
   // requires special formatting of the vertices and triangles arrays:
   // "The start address must be aligned to 4 bytes; 
   // When the buffer will be used as a vertex buffer, the last buffer element 
   // must be readable using 16-byte SSE load instructions, thus padding the 
   // last element is required for certain layouts. E.g. a standard float3 
   // vertex buffer layout should add storage for at least one more float to the
   // end of the buffer."

   // At this point we have a list of invalid vertices (invalidVertices) and the
   // associated invalid triangles (invalidTriangles).

   
   // Create a std::map container which will associate old vertice indexes with
   // new vertices indexes
   std::map<unsigned int, unsigned int> verticesIndexMap;

   // Fill the container associating the key/value the following way:
   // key: old vertice index
   // value: new vertice index. If invalid vertice, value=nbVertices+1
   unsigned int newIndex = 0;
   unsigned int invalid = nbVertices + 1; // this number "defines" invalid pixel
   for (unsigned int v = 0; v < nbVertices; v++) {
      if (invalidVertices[v])
         verticesIndexMap.insert(std::make_pair(v, invalid));
      else {
         verticesIndexMap.insert(std::make_pair(v, newIndex));
         newIndex++;
      }
   }


   // Check that there is at least one valid vertex! The minimum should be much
   // larger than 1 though
   if (newIndex < 1) {
      zvmessage(msg,"All vertices are flagged as invalid - Returning...");
      zabend();
   }

   // Get number of valid vertices
   unsigned int newNumVertices = newIndex;

   // Get number of valid triangles
   unsigned int newNumTriangles = nbTriangles;
   for (int & n : invalidTriangles)
      newNumTriangles -= (unsigned int)n;



   // Create memory-aligned pointer to new vertices and triangles arrays.
   // Per Embree requirement:
   // The start address must be aligned to 4 bytes; 
   // When the buffer will be used as a vertex buffer, the last buffer element 
   // must be readable using 16-byte SSE load instructions, thus padding the 
   // last element is required for certain layouts. E.g. a standard float3 
   // vertex buffer layout should add storage for at least one more float to the
   // end of the buffer."

   // These arrays should be aligned by default anyway. May not need to align 
   // them specifically
   alignas(4) double * newVertices = new double[3*newNumVertices+1];
   if (newVertices == nullptr) {
      zvmessage(msg, "Unsuccessfull array allocation for cleaned vertice list");
      zabend();
   }

   alignas(4) unsigned int * newTriangles = new unsigned int[3*newNumTriangles];
   if (newTriangles == nullptr) {
      zvmessage(msg, "Unsuccessfull array allocation for cleaned triangles list");
      zabend();
   }

   // Checking for this alignement thing... just in case...
   if ((((size_t)newVertices)%4) != 0 || ((size_t)newTriangles)%4 != 0) {
      zvmessage("Unable to allocate arrays aligned to 4 bytes for Embree","");
      zabend();
   }

    
   // Fill the new vertices array with x,y,z values of only the valid vertices
   for (auto  it: verticesIndexMap) {
      if (it.second != invalid) {
         newVertices[3 * (it.second)]     = attrib.vertices[3 * (it.first)];
         newVertices[3 * (it.second) + 1] = attrib.vertices[3 * (it.first) + 1];
         newVertices[3 * (it.second) + 2] = attrib.vertices[3 * (it.first) + 2];
      }
   }

   // Fill the new triangles arrays with (new) vertices indexes of only the 
   // valid triangles
   index = 0;
   for (unsigned int t = 0; t < nbTriangles; t++) {

      // If invalid triangle, move to next triangle
      if (invalidTriangles[t]) 
         continue;

      // Get the vertices index of the current triangles and store the new
      // indexes in the triangles array
      for (unsigned int v = 0; v < 3; v++) 
         newTriangles[3*index+v] = verticesIndexMap[indicesV[t*3 + v].vertex_index];
            
      // Update index in newTriangles array and in mesh::indices array
      index++;

   }







   // Printing the full extend of the filtered mesh
   double minX, maxX, minY, maxY, minZ, maxZ;
   minX = minY = minZ = inf;
   maxX = maxY = maxZ = -inf;
   for(unsigned int i=0; i< newNumVertices; i++) {
      if (newVertices[i*3] < minX)
         minX = newVertices[i*3];
      if (newVertices[i*3] > maxX)
         maxX = newVertices[i*3]; 

      if (newVertices[i*3+1] < minY)
         minY = newVertices[i*3+1];
      if (newVertices[i*3+1] > maxY)
         maxY = newVertices[i*3+1]; 

      if (newVertices[i*3+2] < minZ)
         minZ = newVertices[i*3+2];
      if (newVertices[i*3+2] > maxZ)
         maxZ = newVertices[i*3+2]; 
   }
   snprintf(msg, msgLen, "Mesh \"box\" to be refined: ");
   zvmessage(msg,"");
   snprintf(msg, msgLen, "Min/Max X: %7.3f  %7.3f", minX, maxX); 
   zvmessage(msg,"");
   snprintf(msg, msgLen, "Min/Max Y: %7.3f  %7.3f", minY, maxY); 
   zvmessage(msg,"");
   snprintf(msg, msgLen, "Min/Max Z: %7.3f  %7.3f", minZ, maxZ); 
   zvmessage(msg,"");


   // Printing information on face' edge length
   // warning: not exacly the avg, as counting edge twice
   double sum=0.0;
   double ind = 0;
   double min = 10;
   double max = -10;
   for (unsigned int t=0; t<newNumTriangles;t++) {
      PigPoint p1 = PigPoint(&newVertices[newTriangles[t*3]*3]);
      PigPoint p2 = PigPoint(&newVertices[newTriangles[t*3+1]*3]);
      PigPoint p3 = PigPoint(&newVertices[newTriangles[t*3+2]*3]);
      PigVector p1p2 = p2-p1;
      PigVector p1p3 = p3-p1;
      PigVector p2p3 = p3-p2;
      if (p1p2.magnitude() > max) max = p1p2.magnitude();
      if (p1p2.magnitude() < min) min = p1p2.magnitude();
      if (p1p3.magnitude() > max) max = p1p3.magnitude();
      if (p1p3.magnitude() < min) min = p1p3.magnitude();
      if (p2p3.magnitude() > max) max = p2p3.magnitude();
      if (p2p3.magnitude() < min) min = p2p3.magnitude();
      sum += p1p2.magnitude();
      sum += p1p3.magnitude();
      sum += p2p3.magnitude();
      ind += 3;
   }
 
   snprintf(msg, msgLen, "Average triangle' edge length: %10.6f",sum/ind);
   zvmessage(msg,"");
   snprintf(msg, msgLen, "Smallest triangle' edge length: %10.6f",min);
   zvmessage(msg,"");
   snprintf(msg, msgLen, "Longest triangle' edge length: %10.6f",max);
   zvmessage(msg,"");



//*****************************************************************************
   zvmessage("","");
   zvmessage("**********************************************************","");
   zvmessage("","");
//*****************************************************************************




   // Initializing the mesh refiner (Ceres subclass) object
   MeshRefiner * mesh = new MeshRefiner(nids, file_models, camera_in_all,
          refimg, newNumVertices, newVertices, newNumTriangles, newTriangles, lambda, 
          sigma, nbSigma);


   // If User wants an evaluation of the data and regularization cost, just do 
   // that and return
   if (evalOnly) {

      mesh->setPrintInfo(1);
      
      double cost;
      mesh->Evaluate(newVertices, &cost, nullptr);

      delete mesh;

      zvmessage("Evaluation terminated, returning...","");
      return;
   }


   // Initialize the Ceres Unconstrained Non-Linear Optimization object
   ceres::GradientProblem problem(mesh);

   // Set up any optimization parameters
   ceres::GradientProblemSolver::Options options;
   options.minimizer_progress_to_stdout = true;
   ceres::GradientProblemSolver::Summary summary;

   // Go solve!
   ceres::Solve(options, problem, newVertices, &summary);

   // Once done, print out some optimization information
   std::string report = summary.FullReport();
   char * cstr = new char[report.length()+1];
   std::strcpy(cstr, report.c_str());
   zvmessage(cstr, "");
   delete [] cstr;






//*****************************************************************************

   // Define the texture.
   // Normally in a multi-view situation, the texture applied to the final mesh
   // will be from several images and not just one - the assumption being that
   // several images were used to model the ROI because no single image could
   // capture it whole.
   // However, the way meshing works in VICAR at the time of writting, a mesh 
   // can only be defined from a stereo-pair. Every single triangle is therefore
   // seen by the master image and a texturing of the mesh using the master 
   // image alone can be done.

   // Initialize an array that will contain, for each triangle:
   // - id of the image used to texture it
   // - normalize x coordinates in the image (u). Between 0->1.
   // - normalize y coordinates in the image (v). Between 0->1.
   float * textures = new float[newNumTriangles*3];
   if (textures == nullptr) {
      zvmessage("Unable to allocate array for texturing","");
      zabend();
   } 
   mesh->applyTexture(newVertices, textureIds, textures);





//*****************************************************************************

   // Save the optimized mesh

   saveMesh(meshOut, 
            newVertices, newNumVertices, 
            newTriangles, newNumTriangles,
            textures);

   delete [] textures;





 }  //end of main

















//*****************************************************************************
// MESHREFINER 
//*****************************************************************************


// Constructor
MeshRefiner::MeshRefiner(int nids, PigFileModel *file_models[], 
                         PigCameraModel *camera_in[], int *refimg, 
                         unsigned int nbVertices, double *vertices, 
                         unsigned int nbTriangles, unsigned int *triangles,
                         float lambda, float sigma, unsigned int nbSigma):
                         nids{nids},
                         file_models{file_models},
                         camera_in{camera_in},
                         refimg{refimg},
                         nbVertices{nbVertices},
                         nbTriangles{nbTriangles},
                         triangles{triangles},
                         lambda{lambda},
                         sigma{sigma},
                         nbSigma{nbSigma} {


   // By default, disable information printing
   printInfo = 0;


   // Due to Ceres solver using only double and Embree ray tracer using only 
   // float, we unfortunately have to keep 2 arrays of vertices, one in double
   // and the other one in float. This is a bummer and it is not clear if the
   // precision difference can be an issue with ceres.
   // Per Embree requirements:
   // When the buffer will be used as a vertex buffer, the last buffer element 
   // must be readable using 16-byte SSE load instructions, thus padding the 
   // last element is required for certain layouts. E.g. a standard float3 
   // vertex buffer layout should add storage for at least one more float to the
   // end of the buffer."
   // TODO:add destruction of array in object destructor?
   vertices_f = new (std::nothrow) float[nbVertices*3+1]();
   if (vertices_f == nullptr) {
      zvmessage("Unable to allocate float array for vertices","");
      zabend();
   }
   for (unsigned int i=0; i<nbVertices*3; i++)
      vertices_f[i] = (float)vertices[i];





   // Analysis of the mesh and computation of relations between vertices and
   // triangles that will be needed for the refinement
   zvmessage("Building mesh vertices relationals:","");


   // Compute the "oriented 1-ring" of each vertex
   // The "1-ring" of a vertex is the id of all the vertices that share an edge
   // with that vertex.
   // The "oriented 1-ring" of a vertex is the 1-ring list that has been ordered
   // in such a way that each vertex in the list share also an edge with the 
   // previous and next vertex in the list. Note that the first and last vertices
   // of the list share an edges
   // TODO. Apart from computing the oriented 1-ring, are we using this
   zvmessage("Computing vertices 1-ring...","");
   oneRingTriangles = computeOneRingTriangles();


  
   // Identify the vertices that are on the edge of the mesh. These vertices can
   // be on the edges of the mesh, but also around hole in the mesh.
   // Essentially, all vertices who belong to an edge that itself belong to one
   // and only one triangle
   zvmessage("Identifying edge vertices...","");
   edgeVertices = locateEdgeVertices();



   // The computation of the derivative of the surface fairing penalization 
   // involves the computation of the bi-laplacian of the surface. As a 
   // consequence, during that step, the vertices that are on the edges of the 
   // mesh and also the vertices that have edge vertex in their 1-ring must be
   // identified to be kept fix (no change).
   // Now that we've identified the vertices that are on mesh edges, we also 
   // identify the vertices that have a mesh edge vertex in their 1-ring
   zvmessage("Identifying edge vertices neighbors...","");
   edgeVertices2 = edgeVertices;  //copy of edge vertices
   for (auto i: edgeVertices) {
      for (auto j: oneRingTriangles[i]) {
         edgeVertices2.insert(triangles[j*3]); 
         edgeVertices2.insert(triangles[j*3+1]); 
         edgeVertices2.insert(triangles[j*3+2]); 
      }
   }


   // Compute the oriented 1-ring. This is similar to the 1-ring computed 
   // before, but this time the elements of the 1-ring are ordered by
   // neighborood. If an element of the oriented 1-ring is taken, the next
   // element is its direct neighbor in the mesh.
   zvmessage("Computing vertices oriented 1-ring...","");
   orientedOneRing = computeOrientedOneRing(oneRingTriangles, edgeVertices);




   // Initialize the Embree tool
   // This tool will allow the projection of any pixel of any image on the mesh.
   // It will return a list of information such as if the pixel hit the mesh, 
   // what is the distance from the optical center to the hit location, the
   // id of the hit triangle, the barycentric coordinates of the location of the
   // hit in the triangle,...

   zvmessage("Initializing Embree ray tracer...","");
   
   // Create a new Embree device
   // TODO: which verbosity needed?
   device = rtcNewDevice("verbose=0");
   if (rtcGetDeviceError(device) != RTC_ERROR_NONE) {
        zvmessage("Failed to create Embree device.","");
        zabend();
    }


   // Create a new Embree scene
   scene = rtcNewScene(device);
   if (rtcGetDeviceError(device) != RTC_ERROR_NONE) {
        zvmessage("Failed to create Embree scene.","");
        zabend();
    }

   // Create a new geometry consisting of the triangle mesh
   mesh = rtcNewGeometry(device, RTC_GEOMETRY_TYPE_TRIANGLE);
   if (rtcGetDeviceError(device) != RTC_ERROR_NONE) {
        zvmessage("Failed to create Embree geometry.","");
        zabend();
    }


   // Associating a view on the vertices buffer to embree vertices
   rtcSetSharedGeometryBuffer(mesh, 
                              RTC_BUFFER_TYPE_VERTEX, 
                              0, 
                              RTC_FORMAT_FLOAT3, 
                              vertices_f, 
                              0, 
                              3*sizeof(float), 
                              nbVertices);
   if (rtcGetDeviceError(device) != RTC_ERROR_NONE) {
        zvmessage("Failed to create Embree vertices shared buffer.","");
        zabend();
    }


   // Associating a view on the triangles buffer to embree triangles
   rtcSetSharedGeometryBuffer(mesh, 
                              RTC_BUFFER_TYPE_INDEX, 
                              0, 
                              RTC_FORMAT_UINT3, 
                              triangles, 
                              0, 
                              3*sizeof(unsigned int), 
                              nbTriangles);
   if (rtcGetDeviceError(device) != RTC_ERROR_NONE) {
        zvmessage("Failed to create Embree triangle shared buffer.","");
        zabend();
    }

   // Commit the geometry 
   rtcCommitGeometry(mesh);
   /* unsigned int geomID =*/ rtcAttachGeometry(scene,mesh);    
   rtcReleaseGeometry(mesh);                              



};




// Function to compute the 1-ring of each vertex, i.e., for each vertex in the
// mesh, get the list of vertices that are connected to this vertex with a
// segment
std::vector<std::unordered_set<unsigned int> > MeshRefiner::computeOneRingVertices() {
   
   std::vector<std::unordered_set<unsigned int> > output(nbVertices);

   for (unsigned int t=0; t<nbTriangles; t++) {

      output[triangles[t*3]].insert(triangles[t*3+1]);
      output[triangles[t*3]].insert(triangles[t*3+2]);

      output[triangles[t*3+1]].insert(triangles[t*3]);
      output[triangles[t*3+1]].insert(triangles[t*3+2]);

      output[triangles[t*3+2]].insert(triangles[t*3]);
      output[triangles[t*3+2]].insert(triangles[t*3+1]);
   }

   return output;

};





// Function to compute the 1-ring of each vertex, i.e., for each vertex in the
// mesh, get the list of triangles that contain that vertex
std::vector<std::unordered_set<unsigned int> > MeshRefiner::computeOneRingTriangles() {
   
   std::vector<std::unordered_set<unsigned int> > output(nbVertices);

   for (unsigned int t=0; t<nbTriangles; t++) {

      output[triangles[t*3]].insert(t);
      output[triangles[t*3+1]].insert(t);
      output[triangles[t*3+2]].insert(t);
   }

   return output;

};




// Function to compute the "oriented" 1-ring of each vertex, i.e., for each 
// vertex get a the list of 1-ring vertices ordered such that successive
// vertices are conneced in the graph by a segment.
// The orientation of 1-ring is only done on non-edges vertices
std::vector<std::vector<unsigned int> > MeshRefiner::computeOrientedOneRing(
                    std::vector<std::unordered_set<unsigned int> > oneRingTriangles,
                    const std::unordered_set<unsigned int> edgeVertices) {

   std::vector<std::vector<unsigned int> > oriented1ring(nbVertices);

   std::unordered_set<unsigned int>::iterator it;
   unsigned int currentV = 0, newV = 0;
   bool found = 0;

   for (unsigned int v=0; v<nbVertices; v++) {

      // If the queried vertex is on edge, move on to next one
      if (edgeVertices.find(v) != edgeVertices.end()) 
         continue;
    
      // Get a first vertex in the 1-ring of the queried vertex, no importance 
      // which one:
      // The first triangle of the list is taken and the first vertex of that 
      // triangle is chosen (need to make sure that the chosen vertex is 
      // different from the queried vertex (v)).
      it = oneRingTriangles[v].begin();
      for (int i=0; i<3; i++) {
         if (v != triangles[*it * 3 + i]) {
            currentV = triangles[*it * 3 + i];
            oriented1ring[v].push_back(currentV);
            break;
         }
      }


      // First triangle in the 1-ring of queried vertex (v) has been used. No 
      // need anymore
      oneRingTriangles[v].erase(it);
      found = 0;

      // Loop over all the triangle in the 1-ring list and find the triangle 
      // that contains a vertex identical to the current one. When found, push 
      // the third vertex of that triangle (i.e., not the queried vertex, 
      // nor the current vertex) in the list and make it the current one. Remove
      // the triangle from the list. And keep doing that until all the triangle
      // in the list are used.
      while (!oneRingTriangles[v].empty()) {

         for (it = oneRingTriangles[v].begin(); it != oneRingTriangles[v].end(); it++) {

            if (currentV == triangles[*it * 3]) {
               if ( v == triangles[*it * 3 + 1]) 
                  newV = triangles[*it * 3 + 2];
               else
                  newV = triangles[*it * 3 + 1];
               found = 1;
            }
            else if (currentV == triangles[*it * 3 + 1]) {
               if ( v == triangles[*it * 3]) 
                  newV = triangles[*it * 3 + 2];
               else
                  newV = triangles[*it * 3];
               found = 1;
            }
            else if (currentV == triangles[*it * 3 + 2]) {
               if ( v == triangles[*it * 3]) 
                  newV = triangles[*it * 3 + 1];
               else
                  newV = triangles[*it * 3];
               found = 1;
            }
            else {}

            if (found) {
               oriented1ring[v].push_back(newV);
               currentV = newV;
               oneRingTriangles[v].erase(it);
               found = 0;
               break;
            }

         } 
      }

   }

   return oriented1ring;

}




// Function to locate vertices that are on the edge of the mesh. It can be the
// outer edge of the mesh but also the inner edge due to hole in the mesh.
// During the mesh refinement, these vertices will be kept fixed.
// Note that some strategies can be used to soften the strict vertices pinned 
// down. For instance, in case of hole, a virtual vertex can be added in the 
// middle of the hole and refined along with the edges vertices. At the end of 
// the process, this virtual vertex is removed from the list.
//
// The method to find the edges uses a brute force approach: For each segment of
// each triangle, we'll iterate over all the other triangles and look for the
// same segment. If none is found, then that segment is an edge.
std::unordered_set<unsigned int> MeshRefiner::locateEdgeVertices() {
  
   // Output container that will receive the vertices indices that are on the
   // mesh edge
   std::unordered_set<unsigned int> output;


 //TODO Make omp on an option
 //#pragma omp parallel for schedule(dynamic) if (omp_on)
 #pragma omp parallel for schedule(dynamic)
   for (unsigned int t=0; t<nbTriangles; t++) {

      unsigned int segment1[2];
      unsigned int segment2[2];

      // For each edge of the current triangle
      for (int e1=0; e1<3; e1++) {

         // Get the two vertices of the current edge
         segment1[0] = triangles[t*3+(e1%3)];
         segment1[1] = triangles[t*3+((e1+1)%3)];

         // Compare current edge with edge of all other triangles
         for (unsigned int q=0; q<nbTriangles; q++) {

            // If same triangle, nothing to do 
            if (t == q)
               continue;

            // For each edge of the queried triangle
            for (int e2=0; e2<3; e2++) {

               // Get the two vertices of the queried edge
               segment2[0] = triangles[q*3+(e2%3)];
               segment2[1] = triangles[q*3+((e2+1)%3)];

               // Are the vertices the same between the two edges. If yes, then 
               // these vertices do not belong to an edge
               if ((segment1[0] == segment2[0] && segment1[1] == segment2[1]) ||
                   (segment1[0] == segment2[1] && segment1[1] == segment2[0])) {
                  goto found;
               }
            }
         }   

#pragma omp critical
{
         output.insert(segment1[0]);
         output.insert(segment1[1]);
}
         found:; // Found this segment in another triangle. It's not an edge.

      }

   }

   return output;

};



// This fills Embree array of structures containing the information necessary to
// project each pixel of an image onto the mesh. Namely the origin (camera 
// center) and viewing angle
void MeshRefiner::computeImageRays(const int &cameraIndex, 
                                   const int &ns, 
                                   const int &nl, 
                                   RTCRayHit *& rays) const {

   PigPoint origin;
   PigVector vector;
   unsigned int index = 0;


   // Initialize Embree Ray Hit structure
   rays = new (std::nothrow) RTCRayHit[ns*nl];
   if (rays == nullptr) {
      zvmessage("Memory allocation for Embree::RayHit array failed","");
      zabend();
   }


   // For each pixel of the image
   for (int i = 0; i < nl; i++) {

      for (int j = 0; j < ns; j++) {

         // Compute location of optical center and viewing angle
         // Note: optical center is the same for each pixel, but we need the 
         // viewing angle for each pixel
         camera_in[cameraIndex]->LStoLookVector(i, j, origin, vector, nullptr);


         // Fill rays with origin and viewing angle
         rays[index].ray.org_x = (float)origin.getX();
         rays[index].ray.org_y = (float)origin.getY();
         rays[index].ray.org_z = (float)origin.getZ();
         rays[index].ray.dir_x = (float)vector.getX();
         rays[index].ray.dir_y = (float)vector.getY();
         rays[index].ray.dir_z = (float)vector.getZ();
         rays[index].ray.tnear = 0.0f;
         rays[index].ray.tfar = inf;
         
         index++;
      }
   }


}



// Utility function to load an image in memory. Input is the index of the image 
// to load.
SimpleImage<float> * MeshRefiner::loadImage(int index) const {


   // If current file is open, close it first
   if (file_models[index]->isFileOpen())
      file_models[index]->closeFile();

   // Open the file for reading
   zvopen(file_models[index]->getUnit(), "OP", "READ", "U_FORMAT", "REAL", 
          "OPEN_ACT", "SA", NULL);

   file_models[index]->setFileOpen(TRUE);

   // Initialize a container to load the image in memory
   SimpleImage<float> * image = new SimpleImage<float>(file_models[index]->getNL(), 
                                                       file_models[index]->getNS());

   // Load the image in memory
   for (int j=0; j<file_models[index]->getNL(); j++)
      zvread(file_models[index]->getUnit(), image->linePtr(j), "BAND", 1, 
             "LINE", j+1, NULL);

   // Close the file as it is not needed anymore
   file_models[index]->closeFile();


   // Not sure if we need to zvclose the unit here:
   zvclose(file_models[index]->getUnit(), "CLOS_ACT","FREE", NULL);

   return image;
}





// Bicubic resampling function based on resampling matrices. Resampling matrices
// is a 2-band image containing the (x,y) location in input img that we want to
// be resampled. The function returns the resampled image, whose size is equal 
// to the x/y size of the resampling matrices.
SimpleImage<float> * MeshRefiner::resampleImage(SimpleImage<float> *img, 
                                         SimpleImage<float> *&matrices) const { 


   int m, n;
   unsigned int ns, nl;
   float rsamp, rline, dec, dec2, dec3, f1_, f0, f1, f2, b1_, b0, b1, b2, dn;   

   ns = matrices->getNS();
   nl = matrices->getNL();

   // Initialize the resampled image container
   SimpleImage<float> * output = new SimpleImage<float>(nl, ns);
   output->zero();

   // Iterate over the pixel and resample if matrices have valid values at that 
   // pixel
   for (unsigned int i=0 ; i<nl; i++) {

      for (unsigned int j=0; j<ns; j++) {

         // if invalid matrice values, move to next pixel
         if (matrices->get(0, i, j) == 0)
            continue;

         rline = matrices->get(0, i, j);
         rsamp = matrices->get(1, i, j);

         m=(int)rsamp;
         n=(int)rline;
         dec = rsamp - m;
         dec2 = dec*dec;
         dec3 = dec2*dec;
                        
         // Computing coefficients in one direction 
                                                     
         f1_ = img->get(n-1,m-1);
         f0  = img->get(n-1,m  );
         f1  = img->get(n-1,m+1);
         f2  = img->get(n-1,m+2);
         b1_ = 0.5 * ( 2*f0 + dec*(f1-f1_) + dec2*(2*f1_ - 5*f0 + 4*f1 - f2)
                       + dec3*(3*f0 - 3*f1 + f2 -f1_));
                                                      
         f1_ = img->get(n,m-1);
         f0  = img->get(n,m  );
         f1  = img->get(n,m+1);
         f2  = img->get(n,m+2);
         b0  = 0.5 * ( 2*f0 + dec*(f1-f1_) + dec2*(2*f1_ - 5*f0 + 4*f1 - f2)
                       + dec3*(3*f0 - 3*f1 + f2 -f1_));
                                                      
         f1_ = img->get(n+1,m-1);
         f0  = img->get(n+1,m  );
         f1  = img->get(n+1,m+1);
         f2  = img->get(n+1,m+2);
         b1  = 0.5 * ( 2*f0 + dec*(f1-f1_) + dec2*(2*f1_ - 5*f0 + 4*f1 - f2)
                       + dec3*(3*f0 - 3*f1 + f2 -f1_));
                                                      
         f1_ = img->get(n+2,m-1);
         f0  = img->get(n+2,m  );
         f1  = img->get(n+2,m+1);
         f2  = img->get(n+2,m+2);
         b2  = 0.5 * ( 2*f0 + dec*(f1-f1_) + dec2*(2*f1_ - 5*f0 + 4*f1 - f2)
                       + dec3*(3*f0 - 3*f1 + f2 -f1_));
                                                  
         // Then combining in the other direction 
                                                    
         dec = rline - n;
         dn  = 0.5 * ( 2*b0 + dec*(b1-b1_) + dec*dec*(2*b1_ - 5*b0 + 4*b1 - b2)
                       + dec*dec*dec*(3*b0 - 3*b1 + b2 -b1_));
       
 
         // Saving resampled pixel
         output->set(i, j, dn);

         // For nearestneighbor: 
         //output->set(i,j, img->get((int)(rline+0.5),(int)(rsamp+0.5)));

      }

   }

   img->free();
   return output;

}



// Compute a Gaussian kernel based on the standard deviation (in pixel) and the
// number of standard deviation
void MeshRefiner::getGaussKernel(float * gaussKer) const {

   int ksize = 2 * (int)(sigma*nbSigma) + 1;

   float x, kernel1D[ksize], sum = 0.0;

   // Fill in kernel values.
   for (int i = 0; i < ksize; i++) {
      x = i - ksize / 2;
      kernel1D[i] = exp(- x * x / (2.0 * sigma * sigma));
      sum += kernel1D[i];
   }
  // Normalize kernel values to sum to 1.0.
  for (int i = 0; i < ksize; i++)
     kernel1D[i] /= sum;

  // Make 2D kernel
  // Along the line
  for (int i = 0; i < ksize; i++)
     for (int j = 0; j < ksize; j++)
        gaussKer[i*ksize + j] = kernel1D[j];

  // Along the column
  for (int i = 0; i < ksize; i++)
     for (int j = 0; j < ksize; j++)
        gaussKer[i + j*ksize] *= kernel1D[j];

}



// Compute a simple cross-correlation between Gaussian weighted image patches as
// well as the derivative of the correlation with respect to the slave image
void MeshRefiner::correlate(SimpleImage<float> * master, 
                            SimpleImage<float> * slave,
                            float *corrScore,
                            float *corrDeriv) const {


   int kerSz, halfKerSz, i0, i1, j0, j1, index;
   int ns = master->getNS();
   int nl = master->getNL();

   float centerS, centerM, valS, valM, valS1, valM1, sumKer, sumM, sumS, 
         avgM, avgS, varM, varS, covar, sqrtVars, corr, alpha, beta, gamma, 
         kerElem, centerKer;

   float cst2 = 10.0; // Constant to avoid denominator being = 0  and related to
                      // the Parzen window

   // Width of the Gaussian kernel. Has to be odd.
   halfKerSz = (int)(sigma*nbSigma);
   kerSz = 2 * halfKerSz + 1;

   // Compute Gaussian kernal
   float gaussKer[kerSz * kerSz];
   getGaussKernel(gaussKer);

   centerKer = gaussKer[(kerSz*kerSz-1)/2]; // Get center kernel pixel value


   // Loop over each pixel of the master image
   // Slave image must have the same size... and it should by construction.


   for (int l=0; l<nl; l++) {

      for (int s=0; s<ns; s++) {

         centerS = slave->get(l,s);
         
         if (centerS == 0)
            continue;

         centerM = master->get(l,s);

         i0 = l-halfKerSz;
         i1 = l+halfKerSz+1;
         j0 = s-halfKerSz;
         j1 = s+halfKerSz+1;

         sumKer = sumM = sumS = varM = varS = covar = 0.0;
         index = 0;

         // Loop over each pixel of the patch 
         for (int i=i0; i<i1; i++) {
            for (int j=j0; j<j1; j++) {

               if (i<0 || i>=nl || j<0 ||j >=ns) {
                  index++;
                  continue;
               }

               // Get slave image pixel value. If 0, this is invalid pixel, move
               // to next one
               valS = slave->get(i,j);
               if(valS == 0) {
                  index++;
                  continue;
               }

               valM = master->get(i,j);

               // Get a series of temporary numbers needed to compute the 
               // correlation.                 

               kerElem = gaussKer[index];
               valM1 = valM * kerElem;
               valS1 = valS * kerElem;
               sumKer += kerElem;
               sumM += valM1;
               sumS += valS1;
               varM += valM * valM1;
               varS += valS * valS1;
               covar +=  valM * valS * kerElem;

               index++;
            }
         }
        
 
         // Normalizing all these sums to get the actual average, variance, 
         // covariance and correlation
         avgM = sumM / sumKer;
         avgS = sumS / sumKer;
         varM = varM / sumKer - avgM*avgM + cst2;
         varS = varS / sumKer - avgS*avgS + cst2;
         covar = covar / sumKer - avgM * avgS;
         sqrtVars = sqrt(varM * varS);
         corr = covar / sqrtVars;

         corrScore[l*ns+s] = corr; 

          
         // Computing the elements of the correlation derivative
         if (corrDeriv != nullptr) {
            alpha = centerKer * (-1.0 / (sumKer * sqrtVars)) * centerM;
            beta = centerKer * corr / (sumKer * varS) * centerS;
            gamma = centerKer * (avgM / (sumKer * sqrtVars) - avgS * corr / (sumKer * varS));

            corrDeriv[l*ns+s] = alpha + beta + gamma;
         }

      }

   }    

}







// Compute the principal curvature K1, K2 at each vertex using the cotangent
// method
void MeshRefiner::computePrincipalCurvatures(const double * vertices, 
                                             double * principalCurvatures) const {

   int sz1ring;
   float area, thetaSum, Kh, Kg, delta;
   unsigned int j;
   PigVector K, P0_P, P1_P, P0_P1, P1_P0; 
   PigPoint P, P0, P1;

   // Initialize containers. Normally the valence of the 1-ring is about 6 in
   // the situation of triangular mesh obtained from gridded correlation. Set 
   // to MAX_VALENCE for safety. 
   
   // Initialize obtuse array that will store information about each
   // triangle being obtuse or not:
   // 0: non-obtuse triangle
   // 1: triangle obtuse and obtuse angle at current vertex
   // 2: else
   int obtuse[MAX_VALENCE];

   // Initialize an array that will contain the cotangent of the 2 angles
   // opposite of current vertex angle of each triangle in the one ring.
   float cotangents[2*MAX_VALENCE];

   // Initialize an array that will contain edge vector oriented from 
   // 1-ring vetex to current vertex
   PigVector edgeVect[MAX_VALENCE];

   // Initialize an array that will contain the triangle edge length
   float edgeLength[MAX_VALENCE];



 
   for (unsigned int v = 0; v < nbVertices; v++) {

      // If the current vertex is on edge, move on to next one, as these
      // vertices are fixed
      if (edgeVertices.find(v) != edgeVertices.end()) 
         continue;

      
      // Get the number of vertices in the 1-ring of the current vertex.
      sz1ring = orientedOneRing[v].size();

      if (sz1ring > MAX_VALENCE) {
         zvmessage("Vertex exceeded allowed 1-ring valence","");
         zabend();
      }

      // Get current vertex as a PigPoint. This is only done to use the math
      // operators of the PigPoint and PigVector classes.
      P = PigPoint(vertices[v*3], vertices[v*3+1], vertices[v*3+2]);


      // Sum of the angle of each triangle connected to current vertex
      // This will be needed when computing the gaussian curvature
      thetaSum = 0;

      for (int i = 0; i < sz1ring; i++) {
 
         j = (i+1)%sz1ring;

         // vertex in the 1-ring of current vertex
         P0 = PigPoint(vertices[orientedOneRing[v][i]*3],
                       vertices[orientedOneRing[v][i]*3+1],
                       vertices[orientedOneRing[v][i]*3+2]);

         // next vertex in the oriented list (its neighbor)
         P1 = PigPoint(vertices[orientedOneRing[v][j]*3],
                       vertices[orientedOneRing[v][j]*3+1],
                       vertices[orientedOneRing[v][j]*3+2]);

         P0_P = P - P0;
         P1_P = P - P1;
         P0_P1 = P1 - P0;
         P1_P0 = P0 - P1;


         // Compute the cotangent of the 2 angles (opposite to current vertex) 
         // of the triangle P-P0-P1
         // cotan = V1 . V2 / ||V1 x V2||
         cotangents[i*2]   = (P0_P % P0_P1) / (P0_P * P0_P1).magnitude();
         cotangents[i*2+1] = (P1_P % P1_P0) / (P1_P * P1_P0).magnitude();


         // If one of the cotan is negative, this means that the angle is 
         // obtuse. Store that information for later needs
         // First, initialize obtuse of current triangle to 0
         obtuse[i] = 0;

         if (cotangents[i*2] < 0 || cotangents[i*2+1] < 0)
            obtuse[i] = 2;

         // Compute the angle (in radian) of the triangle at the current vertex
         float dotProd = P0_P % P1_P;
         thetaSum += acos(dotProd / (P0_P.magnitude() * P1_P.magnitude()));

         // If the dotProd is negative, that means that the angle of current 
         // triangle at current vertex is obtuse. 
         if (dotProd < 0)
            obtuse[i]=1;

         // Save the edge vector
         edgeVect[i] = P0_P;

         // Get the squared edge length
         edgeLength[i] = P0_P.magnitude_sq();

      }

      // Initialize the Laplace-Beltrami operator to 0
      K.setXYZ(0.0, 0.0, 0.0);

      // Compute the area of the Voronoi region, that is the sum of all the 
      // Voronoi area of each 1-ring triangle. Depending on the obtuse-ness of
      // the triangle, specific area computation is done as per 
      // [Meyers,2003]:
      // If triangle non-obtuse: area is Voronoi area
      // If angle at current vertex is obtuse: area = 1/2 triangle area
      // Else: area = 1/4 triangle area
      area = 0.0;

      for (int i = 0; i < sz1ring; i++) {

         // Cumulate the Vornoi region of the current triangle
         if (obtuse[i] == 0)
            area += (edgeLength[i] * cotangents[i*2+1] + 
                     edgeLength[(i+1)%sz1ring] * cotangents[i]) / 8.0;
         else if (obtuse[i] == 1)
            area += 0.5 * 0.5 * (edgeVect[i] * edgeVect[(i+1)%sz1ring]).magnitude(); 
         else
            area += 0.25 * 0.5 * (edgeVect[i] * edgeVect[(i+1)%sz1ring]).magnitude(); 

         // Cumulate component for mean curvature computation
         K += edgeVect[i]* (cotangents[(i-1+sz1ring)%sz1ring * 2] + 
                            cotangents[i*2+1]);

      }


      // Area normalization
      K /= (2.0 * area); 

      // Compute Mean Curvature value
      Kh = K.magnitude() / 2.0;

      // Compute Gaussian Curvature
      Kg = (2.0*PI - thetaSum) / area;

      // Finally, compute the principal curvature K1, K2
      delta = Kh * Kh - Kg;

      if (delta < 0.0)
         delta = 0.0;

      delta = sqrt(delta);

      principalCurvatures[v*2]   = Kh + delta;  // K1
      principalCurvatures[v*2+1] = Kh - delta;  // K2

   }

}




// Scaled umbrella operator function. 
// Used to compute the derivative of the fairing function (thin plate) with 
// respect to XYZ of vertex. This is an approximation of the Laplacian.
// In the case of the thin plate, the derivative is obtained from the 
// bi-lapalacian, that is, the umbrella operator is applied twice.
void MeshRefiner::applyUmbrellaOperator(const double * vertices, double * laplacian,
                         std::unordered_set<unsigned int> edgeVertices) const {

   PigPoint P, Pi;
   PigVector Pi_P, L;
   double edgeLength, sumEdgeLength; 


   for (unsigned int v = 0; v < nbVertices; v++) {

      // If the current vertex is on edge, move on to next one, as these
      // vertices are fixed
      if (edgeVertices.find(v) != edgeVertices.end()) 
         continue;

      P = PigPoint(vertices[v*3], vertices[v*3+1], vertices[v*3+2]);
      L.setXYZ(0.0, 0.0, 0.0);
      sumEdgeLength = 0.0;

      for (auto i: orientedOneRing[v]) {

         Pi = PigPoint(vertices[i*3], vertices[i*3+1], vertices[i*3+2]);
         Pi_P = Pi-P;
         edgeLength = Pi_P.magnitude();
         sumEdgeLength += edgeLength;
         L += Pi_P/edgeLength;
      }

      L *= (2.0/sumEdgeLength);

      laplacian[v*3] = L.getX();
      laplacian[v*3+1] = L.getY();
      laplacian[v*3+2] = L.getZ();

   }

}








// Find the location (u,v) for OBJ texture of each triangle of the mesh.
// There are potentially more than one image that can be used for texturing the
// mesh. For instance in a multi-view situation, no image can "see" all the 
// mesh faces. Texturing will be done through several images.
// The images ID that can be used for texturing is defined by the user.
// 
// - vertices:   input New vertices to be considered for the mesh
// - textureIds: input Id of the images that are going to be used for texturing
// the mesh. The order of the images indicates priority.
// - textures:   output Array containing the texturing location. Its size is 
// three times the number of triangles, and for each triangle i, we have the
// following triplets of information
// textures[i*3] = image ID used for the texturing
// textures[i*3+1] = u
// textures[i*3+1] = v

void MeshRefiner::applyTexture(const double * vertices, 
                               const int * textureIds, 
                               float * textures) {

   int nl, ns, id;
   unsigned int trId;
   RTCRayHit * rayHits = nullptr;

   
   // Initialize an index array that will identifying which triangle has
   // been textured
   int * texturedTriangles = new (std::nothrow) int[nbTriangles]();
   if (texturedTriangles == nullptr) {
      zvmessage("Unable to allocate array for texturing mesh!","");
      zabend();
   }

   // Initialize an array that will cumulate the number of pixel hitting a
   // triangle. To get the average of DN for a given triangle
   int * nbHitTriangles = new (std::nothrow) int[nbTriangles]();
   if (nbHitTriangles == nullptr) {
      zvmessage("Unable to allocate array for texturing mesh!","");
      zabend();
   }



   // Update the float vertice array for use in Embree. 
   for (unsigned int i = 0; i<nbVertices*3; i++)
      vertices_f[i] = (float)vertices[i];

   // Commit Embree with changes to vertex arrays
   rtcCommitGeometry(mesh);
   rtcCommitScene(scene);

   RTCIntersectContext context; 
   rtcInitIntersectContext(&context);



   // Iterate over the list of images to be used for texturing and keep
   // projecting the images onto the mesh until all triangles are
   // textured. The order of the images (given by user input) is important.
   // First image textures as much triangles as possible. Second image
   // textures as much triangles not textured with first image as possible, 
   // and so on...

   for (int k = 0; k < nids; k++) {
  
      id = textureIds[k];

      // Check if we've reach the end of the texture file id
      if (id == -1)
         break;
      
      // Get current image information
      ns = file_models[id]->getNS();
      nl = file_models[id]->getNL();

      // Compute Embree ray array for each pixel of the reference image.
      // That is, for each pixel of the reference image, we need the optical 
      // center 3D coordinates and the viewing vector.
      computeImageRays(id, ns, nl, rayHits);

      // Project every pixel of current image on the mesh
      // May be not optimal, especially at the end when only a couple of
      // triangles are left, but fast and easy enough.
      for (int i=0; i < nl; i++) {
         for (int j=0; j< ns; j++) {
            // intersect ray with mesh
            rtcIntersect1(scene, &context, &rayHits[i*ns + j]);
            if (rtcGetDeviceError(device) != RTC_ERROR_NONE) {
               zvmessage("Embree ray tracing query failed","");
               zabend();
            }
         }
      }
      
    
      // For each hit pixel, check that corresponding triangle is not yet
      // textured. If it is not, cumulate the x,y so that we can get an
      // average x,y for that given triangle.

      for (int i=0; i < nl; i++) {

         for (int j=0; j< ns; j++) {
 
            // If current pixel didn't hit the mesh, move on 
            if (rayHits[i*ns+j].ray.tfar == inf)
               continue; 

            // Get triangle Id hit by current pixel
            trId = rayHits[i * ns + j].hit.primID;

            // If triangle is already textured, move on
            if (texturedTriangles[trId] == 1) 
               continue;
           
            // Cumulate this pixel locations for that triangle 
            textures[trId*3] = k;  // Save the image id for that triangle
            textures[trId*3+1] += j; // Cumulate (x,y) of each pixel hitting
            textures[trId*3+2] += i; // that triangle (to get "barycenter")
            nbHitTriangles[trId] += 1; // Cumulate nb pixel hitting that triang

         }
      }


      // Compute avg location of texture (pixel) location for the triangles hit 
      // with the current image. Location is normalize between [0,1]
      for (unsigned int i = 0; i<nbTriangles; i++) {
         if (nbHitTriangles[i] != 0) {
            textures[i*3+1] /= (nbHitTriangles[i]*ns);
            textures[i*3+2] /= (nbHitTriangles[i]*nl);
            texturedTriangles[i] == 1; // TODO: was this intended to be an assignment?
         }
      }

      // Check if some triangles are still not yet textured
      int allDone = 1;
      for (unsigned int i = 0; i<nbTriangles; i++) {
         if (texturedTriangles[i] == 0) {
            allDone = 0;
            break;
         }
      }
      if (allDone)
         break;
  

      // Reset temporary container
      for (unsigned int i = 0; i<nbTriangles; i++)
         nbHitTriangles[i] = 0;

   }

}








// This function overload the "evaluate" function of the Ceres General 
// Unconstrained Minimization. This is the bulk of this program where,
// for a given set of vertices coordinates, the function return the
// global cost of the photo-consistency along with the derivative of
// that cost with respect to the XYZ.
// A regularization terms prevent the mesh to shrink to a null surface
// and enforce some sort of smoothing where strong bending is penalized.

bool MeshRefiner::Evaluate(const double * parameters,
                               double * cost,
                               double * gradient) const {


   // Local variables 
   int masterNS, masterNL, slaveNS, slaveNL, intX, intY;
   unsigned int trId, index, nbSlaveHit, nbOutFOV, nbHitMiss, 
                nbBadTriangleHit, * hitPixIndex, * hitPixIndex2;
   float *corrScore, *corrGradient = NULL;
   double * hitXYZ, * hitVector = NULL, * partialGradient = NULL, * partialGradient2 = NULL,
          b0, b1, b2, val, valX, valY, imGradX, imGradY, Ng_x, Ng_y, Ng_z, norm;
   double line, samp, range, derivative[2][3];
   const size_t msgLen = 256;
   char msg[msgLen];
   SimpleImage<float> * master, *slave, *projectedSlave, *matrices;
   PigPoint point, mOrigin;
   PigVector vector;


   // Array of Embree RayHit structure that will store ray tracing information
   // for each pixel of the reference image
   RTCRayHit * masterRayHits = nullptr;


   // Initialize cost to zero. Cost of the function will be cumulated
   // iteratively. Cost is always computed, whereas the gradient not
   // necessarily. It depends on Ceres call.
   cost[0] = 0.0;

   // If computing the gradient, set the gradient of all vertices to 0.
   // Some vertices can be fixed (i.e., not allowed to change), for instance for
   // vertices on edge of the mesh, or a vertice gradient can potentially be not
   // computable due to not enough valid information. In these case we need 
   // their gradient to be zero.  
   if (gradient != nullptr ) {
      for (unsigned int i = 0; i<nbVertices*3; i++)
         gradient[i] = 0.0;
   }

   // Update the float vertice array for use in Embree. Remember this is due
   // to Ceres using only double and Embree using only float.
   for (unsigned int i = 0; i<nbVertices*3; i++)
      vertices_f[i] = (float)parameters[i];


   // just to make code reading easier.
   // Keep "parameters" name to stay consistent with Ceres documentation, but use
   // "vertices" name to make code understanding easier.
   const double * vertices = parameters;

   // Commit Embree with changes to vertex arrays, i.e., new vertices proposed
   // by Ceres
   rtcCommitGeometry(mesh);
   // AND/OR  ??
   //rtcUpdateGeometryBuffer(mesh, RTC_BUFFER_TYPE_VERTEX,  0);

   //TODO Not sure about sequence here.
   rtcCommitScene(scene);

   RTCIntersectContext context; 
   rtcInitIntersectContext(&context);




   // Iterate over all reference images. All other images are projected onto the
   // mesh then back on the the reference image, and cost/gradient of matching
   // between reference and reprojected slave image is cumulated

   for (int r=0; r<nids; r++) {

      // Move to next image if current one is not a reference (User's choice)
      if (!refimg[r])
         continue;


      // Get reference image size
      masterNS = file_models[r]->getNS();
      masterNL = file_models[r]->getNL();

      // Get the optical center XYZ of current reference image.
      // There is probably a cleaner way to do that
      camera_in[r]->LStoLookVector(0, 0, mOrigin, vector, nullptr);

      // Compute Embree ray array for each pixel of the reference image.
      // That is, for each pixel of the reference image, we need the optical 
      // center 3D coordinates and the viewing vector.
      computeImageRays(r, masterNS, masterNL, masterRayHits);


      // Project every pixel of reference image on the mesh
      unsigned int nbHit = 0;
      for (int i=0; i < masterNL; i++) {

         for (int j=0; j< masterNS; j++) {
  
            // intersect ray with mesh
            rtcIntersect1(scene, &context, &masterRayHits[i*masterNS + j]);
            if (rtcGetDeviceError(device) != RTC_ERROR_NONE) {
               zvmessage("Embree ray tracing query failed","");
               zabend();
            }
  
            if (masterRayHits[i*masterNS+j].ray.tfar != inf) 
               nbHit ++;
         }

      }
     
      


      // For each hit pixel, compute and store the corresponding XYZ and the
      // hit triangle id, along with information needed for gradient.

      std::vector<std::set<unsigned int> >pixTriangleId(nbTriangles);

      hitPixIndex = new (std::nothrow) unsigned int[nbHit];
      hitXYZ = new (std::nothrow) double[nbHit*3];
      
      if (gradient != nullptr) {
         hitVector = new (std::nothrow) double[nbHit*3];
         partialGradient = new (std::nothrow) double[nbHit*3](); 
      }

      index = 0;

      for (int i=0; i < masterNL; i++) {

         for (int j=0; j< masterNS; j++) {
  
            if (masterRayHits[i*masterNS+j].ray.tfar == inf)
               continue; 

            // Save the location of the current pixel which hit the mesh 
            hitPixIndex[index] = i * masterNS + j;

            // Compute the XYZ on the mesh seen from the current pixel
            // (use u,v barycentric coordinates in the hitted triangle)
            b0 = (1.0 - masterRayHits[i * masterNS + j].hit.u
                      - masterRayHits[i * masterNS + j].hit.v);
            b1 = masterRayHits[i * masterNS + j].hit.u;
            b2 = masterRayHits[i * masterNS + j].hit.v;
            trId = masterRayHits[i * masterNS + j].hit.primID;

            // X
            hitXYZ[index*3] = b0 * vertices[triangles[trId*3]*3] +
                              b1 * vertices[triangles[trId*3+1]*3] +
                              b2 * vertices[triangles[trId*3+2]*3];

            // Y
            hitXYZ[index*3+1] = b0 * vertices[triangles[trId*3]*3+1] +
                                b1 * vertices[triangles[trId*3+1]*3+1] +
                                b2 * vertices[triangles[trId*3+2]*3+1];

            // Z
            hitXYZ[index*3+2] = b0 * vertices[triangles[trId*3]*3+2] +
                                b1 * vertices[triangles[trId*3+1]*3+2] +
                                b2 * vertices[triangles[trId*3+2]*3+2];

            // Add pixel id to corresponding triangle
            pixTriangleId[trId].insert(i * masterNS + j);
           
            // Compute intermediate values necessary for gradient if needed:
            if (gradient != nullptr) {
              
               // Vector between optical center and hit location
               hitVector[index*3] = hitXYZ[index*3] - mOrigin.getX();
               hitVector[index*3+1] = hitXYZ[index*3+1] - mOrigin.getY();
               hitVector[index*3+2] = hitXYZ[index*3+2] - mOrigin.getZ();

               // Normalize normal vector of the hit surface - Embree returns a 
               // non-normalized vector
               Ng_x = masterRayHits[i * masterNS + j].hit.Ng_x;
               Ng_y = masterRayHits[i * masterNS + j].hit.Ng_y;
               Ng_z = masterRayHits[i * masterNS + j].hit.Ng_z;
               norm = sqrt(Ng_x*Ng_x + Ng_y*Ng_y + Ng_z*Ng_z);
               Ng_x /= norm;
               Ng_y /= norm;
               Ng_z /= norm;
               masterRayHits[i * masterNS + j].hit.Ng_x = Ng_x;
               masterRayHits[i * masterNS + j].hit.Ng_y = Ng_y;
               masterRayHits[i * masterNS + j].hit.Ng_z = Ng_z;

               // intermediate gradient value (see Vu et al., 2012):
               // 1 / (transpose(surface normal) . hitVector) "." scalar product
               // If denominator=0 that means hit vector // face. This should 
               // not happen
               // TODO: Add a check on that for minimal angle.
               val = 1.0 / (Ng_x * hitVector[index*3] +
                            Ng_y * hitVector[index*3+1]+ 
                            Ng_z * hitVector[index*3+2]); 

               // multiply previous value (val) with hitVector and store it in 
               // intermediate gradient result.
               partialGradient[index*3] = val * hitVector[index*3];
               partialGradient[index*3+1] = val * hitVector[index*3+1];
               partialGradient[index*3+2] = val * hitVector[index*3+2];

            }

            index++;

         }

      }





      // Loading the master image in memory
      master = loadImage(r);

      // Initiate a container that will receive the (x,y) of the slave image
      // projected on the master image
      matrices = new SimpleImage<float>(2, masterNL, masterNS);

      // Now that ray tracing has been done for the reference image, move on to
      // the slave image.
      for (int s = 0; s < nids; s++) {

         // Skip slave image if it is the same as the reference image
         if (r == s)
            continue;

         // Get slave image dimensions
         slave  = loadImage(s);
         slaveNS = slave->getNS();
         slaveNL = slave->getNL();


         // Initialize a container which will store which hit pixel in master
         // are also hit in the current slave
         hitPixIndex2 = new (std::nothrow) unsigned int[nbHit]();
         if (hitPixIndex2 == nullptr) {
            zvmessage("Failed allocating memory for pixHitIndex2","");
            zabend();
         }

         if (gradient != nullptr) {
            partialGradient2 = new (std::nothrow) double[nbHit]();
            if (partialGradient2 == nullptr) {
               zvmessage("Failed allocating memory for partialGradient2","");
               zabend();
            }
         }

         nbSlaveHit = 0;
         nbOutFOV = 0;
         nbHitMiss = 0;
         nbBadTriangleHit = 0;
         matrices->zero();

         // Iterate over the hit pixel in master and see if the corresponding
         // XYZ is "seen" in current slave
         for (unsigned int j = 0; j < nbHit; j++) {

            point.setXYZ(&hitXYZ[j*3]);

            // Get slave (x,y) corresponding to the current XYZ point.
            if (gradient != nullptr) 
               camera_in[s]->XYZtoLS(point, 0, &line, &samp, nullptr, &range, derivative);
            else 
               camera_in[s]->XYZtoLS(point, 0, &line, &samp, nullptr);


            // If XYZ is projected outside of slave image or too close to 
            // the image edge to allow bicubic resampling and image gradient
            // computation, quit and move on to next pixel in master.
            if (line < 1 || line >= (slaveNL-2) || samp < 1 || samp > (slaveNS-2)) {
               nbOutFOV++;
               continue;
            }


            // Need to verify that the slave (x,y) found, if projected on the
            // mesh, does land on the same triangle than the master hit. This
            // is to avoid occlusion matching.

            // Get Origin and View Vector of slave pixel
            camera_in[s]->LStoLookVector(line, samp, point, vector, nullptr);

            // Project that slave pixel on the mesh
            RTCRayHit slaveRayHit;
            slaveRayHit.ray.org_x = (float)point.getX();            
            slaveRayHit.ray.org_y = (float)point.getY(); 
            slaveRayHit.ray.org_z = (float)point.getZ();            
            slaveRayHit.ray.dir_x = (float)vector.getX();            
            slaveRayHit.ray.dir_y = (float)vector.getY();            
            slaveRayHit.ray.dir_z = (float)vector.getZ();            
            slaveRayHit.ray.tnear = 0.0f;
            slaveRayHit.ray.tfar = inf;
             
            rtcIntersect1(scene, &context, &slaveRayHit);
            

            // If no hit -> something went wrong. By construction it should hit.
            // Except with numerical precision for pixel on mesh edge (mesh 
            // horizon).
            // TODO: Deal with numerical precision failure reason
            if (slaveRayHit.ray.tfar == inf) {
               nbHitMiss++;
               continue;
            }

            // Check that the triangle hit by the slave pixel is the same as the
            // reference triangle.
            if (slaveRayHit.hit.primID != masterRayHits[hitPixIndex[j]].hit.primID) {
               nbBadTriangleHit++;
               continue;
            }


            // TODO
            // Add check on normals scalar product. Should be positive otherwise
            // that means looking at face from different side. Also angle 
            // between look angle should probably not be larger than a threshold


            // This is a good pixel match. Set it to valid
            hitPixIndex2[j] = 1;
            nbSlaveHit++;
 
            // Save slave pixel coordinates corresponding to current master
            // pixel coordinates for later resampling of the slave image
            matrices->set(0, hitPixIndex[j]/masterNS, hitPixIndex[j]%masterNS, 
                          line);
            matrices->set(1, hitPixIndex[j]/masterNS, hitPixIndex[j]%masterNS, 
                          samp);

     
            if (gradient != nullptr) {
               
               // resume gradient computation:
               // Add partial derivative of the XYZ to LS function 

               valX = partialGradient[j*3] * derivative[0][0] +
                      partialGradient[j*3+1] * derivative[0][1] +
                      partialGradient[j*3+2] * derivative[0][2];

               valY = partialGradient[j*3] * derivative[1][0] +
                      partialGradient[j*3+1] * derivative[1][1] +
                      partialGradient[j*3+2] * derivative[1][2];

               // Combine with gradient of the slave image
               // TODO: For now the gradient is computed on integer pixel only.
               // Check to see if interpolated values for gradient computation
               // makes a difference or not.
               // TODO: Check if boundary is an issue - see nbOutFOV restriction
               intX = (int)(samp+0.5);
               intY = (int)(line+0.5);
               imGradX = (slave->get(intY, intX+1) - slave->get(intY, intX-1))/2;
               imGradY = (slave->get(intY+1, intX) - slave->get(intY-1, intX))/2;
               
               // The partial gradient reduces to one value for now.
               // Later, the correlation derivative at that pixel location will
               // be accounted for.
               partialGradient2[j] = valX * imGradX + valY * imGradY;


            }


         }


         // Resample slave image, i.e., project the slave image on the master
         // according the the mesh
         projectedSlave = resampleImage(slave,matrices);


        // Correlate the projected slave with the master image on all valid 
        // pixels.
        // If asked for gradient, compute the correlation gradient as well
        corrScore = new float[masterNS*masterNL]();

        if (gradient != nullptr)
           corrGradient = new float[masterNS*masterNL]();
        else
           corrGradient = nullptr;

        correlate(master, projectedSlave, corrScore, corrGradient);


         
        // This is the main part of that function. Cumulate the inverse of the 
        // correlation score of all slave image reprojected onto the reference 
        // image.
        // TODO:should get corr score directly in double. Same with corr grad
        // TODO: correct metric?
        for (unsigned int j=0; j<nbHit; j++) 
           if (hitPixIndex2[j] == 1){
             *cost += 1.0 - abs((double)corrScore[hitPixIndex[j]]); 
           }


        // Same thing with the discrete gradient.
        if (gradient != nullptr) {

           // For each valid pixel, update the gradient of the 3 vertices of the
           // triangle hit by that pixel.
           for (unsigned int j=0; j<nbHit; j++) {
              
              if (hitPixIndex2[j] == 0)
                 continue;
 
              partialGradient2[j] *= corrGradient[hitPixIndex[j]]; 

              b0 = (1.0 - masterRayHits[hitPixIndex[j]].hit.u
                        - masterRayHits[hitPixIndex[j]].hit.v);
              b1 = masterRayHits[hitPixIndex[j]].hit.u;
              b2 = masterRayHits[hitPixIndex[j]].hit.v;
              Ng_x = masterRayHits[hitPixIndex[j]].hit.Ng_x;
              Ng_y = masterRayHits[hitPixIndex[j]].hit.Ng_y;
              Ng_z = masterRayHits[hitPixIndex[j]].hit.Ng_z;
              trId = masterRayHits[hitPixIndex[j]].hit.primID;
              
              gradient[triangles[trId*3]*3]   += b0 * partialGradient2[j] * Ng_x;
              gradient[triangles[trId*3]*3+1] += b0 * partialGradient2[j] * Ng_y;
              gradient[triangles[trId*3]*3+2] += b0 * partialGradient2[j] * Ng_z;

              gradient[triangles[trId*3+1]*3]   += b1 * partialGradient2[j] * Ng_x;
              gradient[triangles[trId*3+1]*3+1] += b1 * partialGradient2[j] * Ng_y;
              gradient[triangles[trId*3+1]*3+2] += b1 * partialGradient2[j] * Ng_z;

              gradient[triangles[trId*3+2]*3]   += b2 * partialGradient2[j] * Ng_x;
              gradient[triangles[trId*3+2]*3+1] += b2 * partialGradient2[j] * Ng_y;
              gradient[triangles[trId*3+2]*3+2] += b2 * partialGradient2[j] * Ng_z;

           } 
           
 
        }


        delete [] hitPixIndex2;
        delete [] corrScore;
        if (gradient != nullptr) {
           delete [] corrGradient;        
           delete [] partialGradient2;
        }
        slave->free();
        projectedSlave->free();


      }

      delete [] masterRayHits;
      delete [] hitPixIndex;
      delete [] hitXYZ;
      if (gradient != nullptr) { 
         delete [] hitVector;
         delete [] partialGradient;
      }
             
      matrices->free();
      master->free();

     //zvmessage(" ","");
   }

   


   // Compute regularization term
   // A surface fairing term is added to the overall cost. We use a thin plate 
   // energy measuring the total curvature of the surface. Strong bending will
   // be penalized.
   double * principalCurvatures = new (std::nothrow) double[nbVertices*2]();
   if (principalCurvatures == nullptr) {
      zvmessage("Unable to allocate array for principal curvature","");
      zabend();
   }
   //computePrincipalCurvatures(principalCurvatures);
   computePrincipalCurvatures(vertices, principalCurvatures);
  
   double datacost = *cost;

 
   for (unsigned int v = 0; v < nbVertices; v++) {

      if (edgeVertices.find(v) != edgeVertices.end()) 
         continue;

      *cost += lambda*(principalCurvatures[v*2] * principalCurvatures[v*2] +
                       principalCurvatures[v*2+1] * principalCurvatures[v*2+1]);
   } 

   delete [] principalCurvatures;

   if (printInfo) {
      snprintf(msg, msgLen, "Data cost: %10.3f", datacost);
      zvmessage(msg, "");
      snprintf(msg, msgLen, "Regularization cost: %10.3f", *cost-datacost);
      zvmessage(msg, "");
   }

   // If gradient, add gradient of regularization term
   // If the mesh is close to isometric, which should be the case given the mesh
   // construction from dense correlation, the gradient of the penalization 
   // thin plate fairing term reduces to a bi-laplacian.
   if (gradient != nullptr) {


      // Apply the scale-dependent umbrella operator twice to approximate the 
      // bi-laplacian of the thin plate fairing function.

      // Initialize containers for intermediate and final results
      double * laplacian = new (std::nothrow) double[nbVertices*3]();
      double * curvatureGradient = new (std::nothrow) double[nbVertices*3]();
      if (laplacian == nullptr || curvatureGradient == nullptr) {
         zvmessage("Unable to allocate array for Umbrella operator","");
         zabend();
      }

      // Apply the umbrella operator twice to get the bi-laplacian.
      // The edge vertices cannot have their laplacian computed. Hence we use 
      // edgeVertices to identify them. Because we apply the operator twice, we 
      // use edgeVertices2 for the second run which identify the vertices who 
      // are on the edge but also the ones which have an edge vertices in their 
      // 1-ring.
      applyUmbrellaOperator(vertices, laplacian, edgeVertices);
      applyUmbrellaOperator(laplacian, curvatureGradient, edgeVertices2);

      delete [] laplacian;


      for (unsigned int v = 0; v < nbVertices; v++) {

         if (edgeVertices2.find(v) != edgeVertices2.end()) {
            gradient[v*3] = 0.0;
            gradient[v*3+1] = 0.0;
            gradient[v*3+2] = 0.0;
         }
         else {
            gradient[v*3] += lambda * curvatureGradient[v*3];
            gradient[v*3+1] += lambda * curvatureGradient[v*3+1];
            gradient[v*3+2] += lambda * curvatureGradient[v*3+2];
         }

      }

      delete [] curvatureGradient;
   }


   return true;

}








void parseFilename(char *name, std::string &fullname, std::string &path, 
                  std::string &filename, std::string &basename, std::string &ext) {


   fullname = std::string(name);
   filename = fullname;
   path = "";
   ext = "";

   size_t sep = fullname.find_last_of("\\/");
   if (sep != std::string::npos) {
      filename = fullname.substr(sep + 1, filename.size() - sep - 1);
      path = fullname.substr(0, sep+1);
   }

   size_t dot = filename.find_last_of(".");
   if (dot != std::string::npos) {
      basename = filename.substr(0, dot);
      ext  = filename.substr(dot+1, filename.size() - dot);
   }
   else
      std::string basename = filename;

}





int saveMesh(char * meshName, double * vertices, unsigned int nbVertices,
             unsigned int * triangles, unsigned int nbTriangles,
             float * textures) {


   std::string fullname, path, filename, basename, ext;
   parseFilename(meshName, fullname, path, filename, basename, ext);


   // Saving MTL file **********************************************************
   ofstream out_mtl (path + basename + ".mtl");
   if (!out_mtl.is_open()) {
      zvmessage("Unable to open output MTL file","");
      zabend();
    }

   out_mtl << "newmtl " << basename << "\r" << std::endl;
   out_mtl << "       Ka 0.2 0.2 0.2 1\r" << std::endl;
   out_mtl << "       Kd 0.8 0.8 0.8 1\r" << std::endl;
   out_mtl << "       Ks 0 0 0 1\r" << std::endl;
   out_mtl << "       map_Kd " << basename << ".mtl" << "\r" << std::endl;
   out_mtl.close();




   // Saving OBJ file **********************************************************
   ofstream tfile (path + basename + ".OBJ");
   if (!tfile.is_open()) {
      zvmessage("Unable to open output OBJ file","");
      zabend();
    }

   tfile << "mtllib " << basename << ".mtl\r" << std::endl;
   tfile << "usemtl " << basename << "\r" << std::endl;


   // Write vertex
   for (unsigned int i = 0; i < nbVertices; i++)
      tfile << "v \t" << vertices[i*3] << "\t" << vertices[i*3+1] << "\t" << vertices[i*3+2] <<"\r" << std::endl;


   // Write texture
   // NOTE: No accounting of image id for now
   if (textures != nullptr) {
      for (unsigned int i = 0; i < nbTriangles; i++) 
         tfile << "vt \t" << (double)textures[i*3+1] << "\t" << (double)textures[i*3+2] <<"\r" << std::endl;
   } 

   // Write faces
   // NOTE: For texture we set the texture of all vertices for a given triangle are the same.
   // Should be dependent on the hit location in the image.
   if (textures != nullptr) {
      for (unsigned int i = 0; i < nbTriangles; i++) 
         tfile << "f \t" << triangles[i*3]+1 << "/" << i << "\t" 
                         << triangles[i*3+1]+1 << "/" << i << "\t" 
                         << triangles[i*3+2]+1 << "/" << i << std::endl;
   }
   else {
      for (unsigned int i = 0; i < nbTriangles; i++) 
         tfile << "f \t" << triangles[i*3]+1 << "\t" << triangles[i*3+1]+1 << "\t" << triangles[i*3+2]+1 << std::endl;
   }

   tfile.close();

   return 0;

}





////////////////////////////////////////////////////////////////////////
// This function runs the Union-Find algorthim over a list of pairs to 
// return the underlying connected groups.
// When this function is applied to a list of tiepoints, it returns the tracks,
// which are the groups of observations that "look" at the same ground location.
// This funtion is also used to find the images connectivity, that is to 
// identify the groups of images that are connected based on a list of pairwise
// connection.
// NOTE: this function is a copy/paste of the original union-find algorithm
// developed for tiepoints connection. Vocabulary used in this function 
// is meant for that. Beware!
////////////////////////////////////////////////////////////////////////

template<typename T>
vector< vector<T> > union_find(vector< vector<T> > tiePointsVect)
{
    unsigned int i, idSecond, trackIndex = 0;

    //containers and iterators declaration
    map<T, int> points;
    typename map<T, int>::iterator it1, it2;
    
    set<T> track2;
    typename set<T>::iterator itTrack;
    
    vector< set<T> > allTracks;
    typename vector< set<T> >::iterator itAllTracks;
    vector< vector<T> > finalTracks;
 

    // First part of the Union-Find algorithm
    // Run through the list of tie points and save each unique point in 
    // a std::map. The map-key is a unique observation (pixel) identifier, and 
    // map-value is the identifier (the index really) of the corresponding track
    // (initialized to -1). std:map is usefull here to avoid duplicates.
    for (i=0; i<tiePointsVect.size(); i++) {
       points.insert(std::make_pair(tiePointsVect[i][0], -1));
       points.insert(std::make_pair(tiePointsVect[i][1], -1)); 
    } 


    // Second part of the Union-Find algorithm. The objective is to associate
    // each observation to a track ID. We're iterating over the tie-points 
    // (a pair of observations) and find the track ID of each point.
    // Each observation with identical track ID points to the same ground point.
    for (i=0; i<tiePointsVect.size(); i++) {

        // For the current tie-points (pair) find their corresponding track ID
        // using the std::map previously built.
        it1 = points.find(tiePointsVect[i][0]);
        it2 = points.find(tiePointsVect[i][1]);

        // If both track ID are -1, that means that these two points are 
        // currently not associated to an existing track. Create a new track
        // and update the observations corresponding track ID.
        if (it1->second == -1 && it2->second == -1) {
            set<T> newTrack;                    // Create a new track container.
            newTrack.insert(it1->first);        // Insert point ID in this new
            newTrack.insert(it2->first);        // track.
            allTracks.push_back(newTrack);      // Add the new track to the list
            it1->second = trackIndex;           // Update the track ID of these
            it2->second = trackIndex;           // these two observations.
            trackIndex++;                       // Update the track ID index
        }

        // If only track ID of "left" tie-point is -1, that means that
        // "left"  point belong to a track already initialized and identified by
        // "right" point track ID. Add "left" observation to the track and 
        // update its track ID
        else if (it1->second == -1) {
            (allTracks[it2->second]).insert(it1->first);
            it1->second = it2->second;
        }

        // Same as above but with opposite situation for "left" and "right"
        else if (it2->second == -1) {
            (allTracks[it1->second]).insert(it2->first);
            it2->second = it1->second;
        }

        // If both observations already have a track ID. If the track IDs are 
        // identical, there is nothing to do, as these two points have already 
        // been assigned to the same track. If they are different, this means 
        // that the two points belong to two different tracks that are actually
        // referring to the same ground point, and need to be merged.
        // To merge them, we are copying all points belonging to "right" track 
        // into "left" track. Update the "right" observations track ID 
        // accordingly, and finally empty the "right" track. Note, the track is
        // emptied, not erased, to preserve the track index coherence.
        else {
            if (it1->second != it2->second) {
               idSecond = it2->second;
               track2 = allTracks[it2->second];
               for (itTrack = track2.begin(); itTrack != track2.end(); itTrack++) { 
                  (allTracks[it1->second]).insert(*itTrack);
                  it2 = points.find(*itTrack);
                  it2->second = it1->second;
               }
               (allTracks[idSecond]).clear();
            }
        }   
    }


    // Reformating the tracks
    // std::map has been a useful container (non-duplicate and fast tree search)
    // for finding the tracks during the second part of the Union-Find algorithm.
    // However, for the rest of the work, it's not very handy to have the tracks
    // stored like that. We "reformat" the track as a vector of vector (i.e., 
    // first vector contains the tracks, second vector contains observations ID 
    // belonging to a given track.
    // TO DO: See if we could avoid this step. No significant time penalty
    // observed.  
    for (itAllTracks = allTracks.begin(); itAllTracks != allTracks.end(); 
                                                                itAllTracks++) {
        if (itAllTracks->size() == 0)
           continue;
        vector<T> track;
        std::copy(itAllTracks->begin(), itAllTracks->end(), 
                                                     std::back_inserter(track));
        finalTracks.push_back(track);
    }


    return finalTracks;
 }




