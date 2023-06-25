///////////////////////////////////////////////////////////////////////////////
// MeshMan
// A small class handling loading and using a mesh into vicar program
// The main objective of this class is to load a OBJ and do ray intersection of
// an origin and look vector with the mesh. 
// 
// This class is meant to evolve with needs.
//
// Restrictions (to be updated with future modifications)
// [1] The class supports OBJ that contains only 1 shape and is made of 
// triangular faces. This could easily be modified to support more complex OBJ.
// [2] The Pig coordinates system of the mesh vertices cannot be instanciated
// from reading the OBJ itself. This is a limitation due to the amount (or lack
// thereof) of CS information stored in the OBJ (mtl and lbl really).
// [3] Ray intersection with the mesh currently only support one ray at a time.
// However, if needed, Embree (the ray tracer used in this class), supports 
// bundle of ray tracing for faster speed. 
///////////////////////////////////////////////////////////////////////////////

#ifndef MESHMAN_H
#define MESHMAN_H

#include "PigCoordSystem.h"

#define ENABLE_STATIC_LIB 
#include "rtcore.h"

#include <xmmintrin.h>
#if !defined(_MM_SET_DENORMALS_ZERO_MODE)
#define _MM_DENORMALS_ZERO_ON   (0x0040)
#define _MM_DENORMALS_ZERO_OFF  (0x0000)
#define _MM_DENORMALS_ZERO_MASK (0x0040)
#define _MM_SET_DENORMALS_ZERO_MODE(x) (_mm_setcsr((_mm_getcsr() & ~_MM_DENORMALS_ZERO_MASK) | (x)))
#endif

class MeshMan {

   protected:

      // Mesh file name
      char * _meshName;
 
      // Mesh parameters and content
      int _nbVertices;
      int _nbTriangles;
      float * _vertices;
      unsigned int * _triangles;

      // Embree Ray Tracer elements
      RTCDevice _device;
      RTCScene _scene;
      RTCGeometry _mesh;
      unsigned int _geomID;

      // Mesh vertices coordinates system
      PigCoordSystem * _mesh_cs;

      // Clean mesher in case of construction failure
      inline void cleanMesher();

   public:

      MeshMan();
      MeshMan(const char * objFile);
      MeshMan(const char * objFile, PigCoordSystem * mesh_cs);

      ~MeshMan();

      // Load functions
      int loadMeshFile(const char * objFile);
      void setCS(PigCoordSystem * cs);

      // Query functions
      const char * getMeshName(){return _meshName;};
      int getNumVertices() { return _nbVertices; };
      int getNumTriangles() { return _nbTriangles; };
      PigCoordSystem * getCS() { return _mesh_cs; };

      // CS conversion
      void convertCS(PigCoordSystem *output_cs);

      // Ray intersection
      int intersect(const PigPoint &origin, const PigVector &view, 
                    PigPoint *XYZ = nullptr,     // output
                    PigVector *normal = nullptr, // ouput
                    float *range = nullptr);     // output
   
      // Temporary for debug only ?
      float * getVerticesPtr() {return _vertices;};
      unsigned int * getTrianglesPtr() {return _triangles;};

};
#endif
