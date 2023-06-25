///////////////////////////////////////////////////////////////////////////////
// MeshMan
// A small class handling loading and using a mesh into vicar program
// The main objective of this class is to load a OBJ and do ray intersection of
// an origin and look vector with the mesh. 
// 
// This class is meant to evolve with needs.
//
// Restrictions (to be updated with future modifications if needed)
// [1] The class supports OBJ that contains only 1 shape and is made of 
// triangular faces. This could easily be modified to support more complex OBJ.
// [2] The Pig coordinates system of the mesh vertices cannot be instanciated
// from reading the OBJ itself. This is a limitation due to the amount (or lack
// thereof) of CS information stored in the OBJ (mtl and lbl really).
// [3] Ray intersection with the mesh currently only support one ray at a time.
// However, if needed, Embree (the ray tracer used in this class), supports 
// bundle of ray tracing for faster interesection. 
///////////////////////////////////////////////////////////////////////////////


#include "meshman.h"

#define TINYOBJLOADER_IMPLEMENTATION
#include "tiny_obj_loader.h"

#include "zvproto.h"

using namespace std;

MeshMan::~MeshMan() {


   rtcReleaseGeometry(_mesh);
   if (rtcGetDeviceError(_device) != RTC_ERROR_NONE) 
      zvmessage("Failed to release Embree mesh object.","");

   rtcReleaseScene(_scene);
   if (rtcGetDeviceError(_device) != RTC_ERROR_NONE) 
      zvmessage("Failed to release Embree scene object.","");

   rtcReleaseDevice(_device);
   if (rtcGetDeviceError(nullptr) != RTC_ERROR_NONE) 
      zvmessage("Failed to release Embree device object.","");

   if (_vertices != nullptr)
      delete [] _vertices;

   if (_triangles != nullptr)
      delete [] _triangles;

   if (_meshName != nullptr)
      delete [] _meshName;
}


MeshMan::MeshMan() {

   MeshMan(nullptr, nullptr);
}

MeshMan::MeshMan(const char * objFile) {

   MeshMan(objFile, nullptr);
}


// Main constructor. It's a bit long and could be simpler but it's a first cut.
// The main constraint is to still return a valid, albeit empty, mesher in case
// the .OBJ failed to load or wasn't supplied.
MeshMan::MeshMan(const char * objFile, PigCoordSystem * mesh_cs) {

   // Default members ---------------------------------------------------------
   _meshName = nullptr;
   _mesh_cs = nullptr;
   _nbVertices = 0;
   _nbTriangles = 0;
   _vertices = nullptr;
   _triangles = nullptr;


   // Embree device initialization ---------------------------------------------
   // This cannot fails. If it does, bail out of the program.
   try {

      // Create a new Embree device
      _device = nullptr;
      _device = rtcNewDevice("verbose=0");
      if (rtcGetDeviceError(_device) != RTC_ERROR_NONE) 
         throw std::string("Failed to create Embree device");

      // Create a new Embree scene
      _scene = nullptr;
      _scene = rtcNewScene(_device);
      if (rtcGetDeviceError(_device) != RTC_ERROR_NONE) 
         throw std::string("Failed to create Embree scene");

      // Create a new geometry consisting of the triangle mesh
      _geomID = 0;
      _mesh = nullptr;
      _mesh = rtcNewGeometry(_device, RTC_GEOMETRY_TYPE_TRIANGLE);
      if (rtcGetDeviceError(_device) != RTC_ERROR_NONE) 
         throw std::string("Failed to create Embree geometry");
   }
   catch (std::string err) {
      if (!err.empty()) {
         char msg[150];
         sprintf(msg, "%s", err.c_str());
         zvmessage(msg,"");
         zabend();
      }
   }
   catch (...) {
      zvmessage("Error while Embree initialization","");
      zabend();
   }

   // If there is an input mesh file, load it. Otherwise, return, we'll have
   // an "empty" mesh, waiting to be loaded with vertices and faces
   if (objFile != nullptr)
      loadMeshFile(objFile);

   // Save the mesh CS 
   if (mesh_cs != nullptr)
      setCS(mesh_cs);

}





// Load the mesh -----------------------------------------------------------
// Need to check the validity of the loaded mesh.
// A mesh can fail the loading (incorrect file name, corrupted file, 
// incorrect formating, etc) in which case an empty mesher is returned. Or
// a mesh can be properly read, but have an unsupported format (for now only 
// triangle mesh is supported, one shape only per file). If such situations
// happen, clear out whatever has been initialized and return a valid empty 
// mesher.
int MeshMan::loadMeshFile(const char * objFile) {

   if (objFile == nullptr) {
     zvmessage("Invalid mesh filename (nullptr)","");
     return 0;
   }

   // Clean out any previously loaded meshes
   cleanMesher();
   


   // Some container for the OBJ reader
   tinyobj::attrib_t attrib;
   std::vector<tinyobj::shape_t> shapes;
   std::vector<tinyobj::material_t> materials;

   std::string err;

   try {

      // Load the mesh
      zvmessage("Loading mesh file. Can take a while for large mesh file...","");
      bool ret = tinyobj::LoadObj(&attrib, &shapes, &materials, &err, objFile,
                               NULL, false);

      // Check that mesh was successfully loaded
      if (!ret) 
         throw err;

      // It is assumed that the OBJ file contains only 1 shape and is made of 
      // triangular faces. Note that tinyobj class will read any OBJ properly,
      // but for now MeshMan does not support all situations.
      // Check that number of shapes in OBJ is equal to 1
      if (shapes.size() != 1) 
         throw std::string("OBJ file should contain only 1 shape");

      // Check that mesh is made out of triangular faces only
      for (unsigned int f = 0; f<shapes[0].mesh.num_face_vertices.size(); f++) {
         if (shapes[0].mesh.num_face_vertices[f] != 3) 
            throw std::string("OBJ file should contain only triangular faces");
      }


      // Retrieve relevant information on the loaded mesh   

      // Number of vertices
      _nbVertices = (int)(attrib.vertices.size() / 3 );

      // Number of triangles
      _nbTriangles = (int)(shapes[0].mesh.num_face_vertices.size()); 


      // Initialize EMBREE compatible containers for vertices and triangles
      _vertices = new (std::nothrow) float[3 * _nbVertices+1]; //+1 EMBREE requ.
      if (_vertices == nullptr) 
         throw std::string("Memory allocation error when allocating vertices array");

      _triangles = new (std::nothrow) unsigned int[3 * _nbTriangles];
      if (_triangles == nullptr) 
         throw std::string("Memory allocation error when allocating triangles array");


      // Copy of Vertices XYZ to new array. The copy could possibly be avoided 
      // by giving Embree a pointer to the data, but it's fast enough and 
      // guarantees that the alignement request from EMBREE is satisfied
      std::memcpy(_vertices, attrib.vertices.data(), 3*_nbVertices*sizeof(float));

      // Copy of Triangles to new array. This is a bit more complex due to the 
      // way they are stored in the ouput of the OBJ loader.
      for (unsigned int i=0; i<_nbTriangles*3; i++) 
         _triangles[i] = shapes[0].mesh.indices[i].vertex_index; 


      // Associate a view on the vertices buffer to embree vertices
      rtcSetSharedGeometryBuffer(_mesh, 
                                 RTC_BUFFER_TYPE_VERTEX, 
                                 0, 
                                 RTC_FORMAT_FLOAT3, 
                                 _vertices, 
                                 0, 
                                 3*sizeof(float), 
                                 _nbVertices);
      if (rtcGetDeviceError(_device) != RTC_ERROR_NONE) 
         throw std::string("Failed to create Embree vertices shared buffer.");


      // Associate a view on the triangles buffer to embree triangles
      rtcSetSharedGeometryBuffer(_mesh, 
                                 RTC_BUFFER_TYPE_INDEX, 
                                 0, 
                                 RTC_FORMAT_UINT3, 
                                 _triangles, 
                                 0, 
                                 3*sizeof(unsigned int), 
                                 _nbTriangles);
      if (rtcGetDeviceError(_device) != RTC_ERROR_NONE) 
         throw std::string("Failed to create Embree triangle shared buffer.");

      // Commit the geometry 
      rtcCommitGeometry(_mesh);

      // Let _scene take ownership of _mesh
      _geomID = rtcAttachGeometry(_scene,_mesh);

      // No need of geometry (_mesh) handle anymore, it's managed by _scene    
//      rtcReleaseGeometry(_mesh);                              

      // Commit the scene
      rtcCommitScene(_scene);


   } // end of try
   catch (std::string err) {
      if (!err.empty()) {
         char msg[250];
         sprintf(msg, "%s", err.c_str());
         zvmessage(msg,"");
      }
      cleanMesher();
      return 0;
   }
   catch (...) {
      zvmessage("Error while loading and checking mesh","");
      cleanMesher();
      return 0;
   }

   // Save mesh file name
   int n = strlen(objFile);
   _meshName = new char[n+1];
   strcpy(_meshName, objFile);

   // All is good
   return 1;

}


// Set the CS of the mesh
// If no mesh CS is set already, just store it
// If a mesh CS is already set and if a mesh is loaded, convert the mesh
// vertices and store the CS
void MeshMan::setCS(PigCoordSystem *cs) {

   if (cs == nullptr || _mesh_cs == nullptr) {
      _mesh_cs = cs;
      return;
   }

   if (_nbVertices != 0)
      convertCS(cs);

}



// This function cleans out any uncomplete mesher initialization. 
// In case the constructor partially succeed, clean whatever needs to
// be cleaned in order to have a working "empty" mesher.
inline void MeshMan::cleanMesher() {

   // Remove the mesh (vertices and faces) from the EMBREE structure.
   // In practice, just the view on the mesh is removed
   //rtcDetachGeometry(_scene, _geomID);

   // Deallocate any vertices and faces data if any, and sets the 
   // numeral to 0.
   _nbVertices = 0;
   if (_vertices != nullptr)
      delete [] _vertices;
   _vertices = nullptr;

   _nbTriangles = 0;
   if (_triangles != nullptr)
      delete [] _triangles;
   _triangles = nullptr;

   if (_meshName != nullptr) 
      delete [] _meshName;
   _meshName = nullptr;

 }



// Convert the vertices coordinates to another coordinates system.
void MeshMan::convertCS(PigCoordSystem *output_cs) {

   // Cases where there's no actual conversion to do
   if (_mesh_cs == nullptr || _nbVertices == 0) {
      _mesh_cs = output_cs;
      return;
   }


   PigPoint p;
  
   for (unsigned int v = 0; v < _nbVertices; v++) {
      p = output_cs->convertPoint(PigPoint(&_vertices[v*3]), _mesh_cs);
      _vertices[v*3]   = (float)p.getX();
      _vertices[v*3+1] = (float)p.getY();
      _vertices[v*3+2] = (float)p.getZ();
   }

   // Commit Embree with changes to vertex arrays
   rtcCommitGeometry(_mesh);
   rtcCommitScene(_scene);

   // Update mesh CS
   _mesh_cs = output_cs;

}



// This is the main goal of the mesher. 
// Project a ray, given by origin (camera center) and viewing vector, and find
// the interesection, if any, with the mesh.
// The function returns 1 or 0 (1: hit the mesh, 0: no hit) and returns
// (optionally) three measurements:
// - XYZ coordinates of the hit location on the mesh
// - Normal vector of the mesh surface at the hit location
// - range (distance) between the origin of the ray and the hit location
//
// Note there are three situations when the function would return 0:
// - when mesh is empty
// - when the mesh is not hit
// - when the ray projection failed for whatever reason, i.e., when there is an
//   exception (error). An error message is printed, but that's it. 
//
// The content of output parameter (XYZ, normal, range) are either undefined or
// untouched if no hit. Strongly recommended to check returned value prior to
// reading these parameters
int MeshMan::intersect(const PigPoint &origin, const PigVector &view, 
                       PigPoint *XYZ, PigVector *normal, float *range) {

   // HACK.
   // Normally, an empty but fully constructed embree mesh should be able to 
   // handle intersect query. However, the meshman construction when the OBJ is 
   // not found (for instance) is not fully completed and an intersect query 
   // may segfault. The check on the number of vertices prevents the segfault,
   // but a better solution would be to make sure the embree mesh is fully valid
   // during construction. To be done, but no time now.
   if (_nbVertices == 0)
      return 0;

   // Initialize Embree Ray Hit structure
   RTCRayHit rayHit;

   // Populate it with origin and look vector of ray
   rayHit.ray.org_x = (float)origin.getX();
   rayHit.ray.org_y = (float)origin.getY();
   rayHit.ray.org_z = (float)origin.getZ();
   rayHit.ray.dir_x = (float)view.getX();
   rayHit.ray.dir_y = (float)view.getY();
   rayHit.ray.dir_z = (float)view.getZ();
   rayHit.ray.tnear = 0.0f;
   rayHit.ray.tfar  = std::numeric_limits<float>::infinity();

   // Instanciation of Embree context structure, needed for intersect
   // function, but not used in our current code         
   // NOTE: Could that be a class member instead of initializing one for each
   // intersect query? Most likely. To be checked.
   RTCIntersectContext context;
   rtcInitIntersectContext(&context);

   // Interesect with mesh
   rtcIntersect1(_scene, &context, &rayHit);
   if (rtcGetDeviceError(_device) != RTC_ERROR_NONE) {
      zvmessage("Embree ray tracing query failed","");
      return 0;
   }

   // Retrieve ray tracing results and compute relevant information
   
   // No intersection
   if (rayHit.ray.tfar == std::numeric_limits<float>::infinity())
      return 0;


   // Get the range between origin and hitted mesh
   if (range != nullptr)
      *range = rayHit.ray.tfar;


   // Compute the XYZ on the mesh seen from the origin/view
   // (use u,v barycentric coordinates in the hitted triangle)
   if (XYZ != nullptr) {
      float b0 = 1.0 - rayHit.hit.u - rayHit.hit.v;
      float b1 = rayHit.hit.u;
      float b2 = rayHit.hit.v;
      unsigned int trId = rayHit.hit.primID;

      // X
      float X = b0 * _vertices[_triangles[trId*3]*3] +
                b1 * _vertices[_triangles[trId*3+1]*3] +
                b2 * _vertices[_triangles[trId*3+2]*3];

      // Y
      float Y = b0 * _vertices[_triangles[trId*3]*3+1] +
                b1 * _vertices[_triangles[trId*3+1]*3+1] +
                b2 * _vertices[_triangles[trId*3+2]*3+1];

      // Z
      float Z = b0 * _vertices[_triangles[trId*3]*3+2] +
                b1 * _vertices[_triangles[trId*3+1]*3+2] +
                b2 * _vertices[_triangles[trId*3+2]*3+2];

      // Populate output XYZ point
      XYZ->setX(X);       
      XYZ->setY(Y);       
      XYZ->setZ(Z);
   }       

   // Populate output Normal vector
   if (normal != nullptr) {
      normal->setX((float)rayHit.hit.Ng_x);
      normal->setY((float)rayHit.hit.Ng_y);
      normal->setZ((float)rayHit.hit.Ng_z);
      normal->normalize();
   }

   // All good, return 1 hit
   return 1;

}



