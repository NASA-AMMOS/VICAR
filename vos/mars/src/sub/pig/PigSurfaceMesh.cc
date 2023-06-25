

////////////////////////////////////////////////////////////////////////
// PigSurfaceMesh
//
// Surface model for a Mesh described by its .OBJ file.
// The main purpose of this model, method intersectRay() returns 
// closest intersection with the mesh.
////////////////////////////////////////////////////////////////////////

#include "PigSurfaceMesh.h"
#include "PigCoordSystem.h"
#include "iostream"
#include <unistd.h> 

// Mesh support activated depending on OS arch -  See comment in PigMission.h
// Pig supports .obj mesh for surface model. The ray tracer used internally to
// intersect the mesh with viewing pixels is handled by the Intel Embree lib, 
// which is only available on 64-bit OSes. The mesh support is activated 
// accordingly. See imake_util.tmp and imake.config
#if USES_PIG_MESH


using namespace std;

////////////////////////////////////////////////////////////////////////
// Constructors
////////////////////////////////////////////////////////////////////////

PigSurfaceMesh::PigSurfaceMesh(char *mission, char *instrument, char *target)
	: PigSurfaceModel(mission, instrument, target)
{
   _mesh = new MeshMan();
    if (_mesh == nullptr) {
       char msg[512];
       sprintf(msg, "Error while initializing the MeshMan object");
       printInfo(msg);
    }
}

PigSurfaceMesh::PigSurfaceMesh(char *target, PigCoordSystem *cs)
	: PigSurfaceModel(target, cs)
{
   _mesh = new MeshMan(nullptr, cs);
    if (_mesh == nullptr) {
       char msg[512];
       sprintf(msg, "Error while initializing the MeshMan object");
       printInfo(msg);
    }
}

PigSurfaceMesh::PigSurfaceMesh(PigCoordSystem *cs, char * meshFile)
	: PigSurfaceModel(NULL, cs)
{
    //_cs = cs; //Taken care by superclass

    _mesh = new MeshMan(meshFile, cs);
    if (_mesh == nullptr) {
       char msg[512];
       sprintf(msg, "Error while initializing the MeshMan object");
       printInfo(msg);
    }

    // Check that something was loaded from the meshFile
    if (meshFile != nullptr) {
       char msg[512];
       if (_mesh->getNumVertices() != 0) {
          sprintf(msg, "Loaded mesh file: %s", meshFile);
          printInfo(msg);
       }
       sprintf(msg, "Num mesh vertices: %d, num mesh faces: %d", 
               _mesh->getNumVertices(), _mesh->getNumTriangles());
       printInfo(msg);
    }

}

////////////////////////////////////////////////////////////////////////
// Destructor
////////////////////////////////////////////////////////////////////////

PigSurfaceMesh::~PigSurfaceMesh()
{
   if (_mesh != nullptr)
      delete _mesh;
}

////////////////////////////////////////////////////////////////////////
// Convert to the given coord system.  Convert all vertices.
////////////////////////////////////////////////////////////////////////
void PigSurfaceMesh::setCoordSystem(PigCoordSystem *cs)
{
    _mesh->convertCS(cs);
    _cs = cs;
}

////////////////////////////////////////////////////////////////////////
// This function is similar to the one below and is just there to keep 
// SurfaceModel functions signatures consistent. 
// which_intercept is ignored as the undelying mesh handler does not 
// currently have the capability of returning the number of hits. It can
// return the closest hit (the visible one, the one that matter usually)
// or if there is any hit.
// Return 1 if the given intercept is 1 and if there's an intersect with
// the mesh, return 0 otherwise.
////////////////////////////////////////////////////////////////////////
//
//
int PigSurfaceMesh::intersectRay(const PigPoint &origin,
                                 const PigVector &look_direction,
                                 const int which_intercept,
                                 PigPoint &intersect_point)
{

    int hit = intersectRay(origin, look_direction, intersect_point);

    if (hit == 1 && which_intercept == 1)
       return 1;
    else
       return 0;
        
}

////////////////////////////////////////////////////////////////////////
// Intersect a ray with Mesh, giving a 3-D world location.
// This is the main purpose of surface models.
//
// Return codes:
// 0 == no intersection
// 1 == intersects surface at least once.
// A return code of 1 means that the returned point is valid.  For return
// codes of 0, the returned point is actually a unit vector pointing in
// the "infinity" direction (usually the same as the look vector since
// origin doesn't matter at infinity).
// Note that the caller must pass in the PigPoint to be filled; this
// routine does not allocate it.  In the case of multiple intersections,
// it returns the "closest" one.
//
////////////////////////////////////////////////////////////////////////

int PigSurfaceMesh::intersectRay(const PigPoint &origin,
                                 const PigVector &look_direction,
                                 PigPoint &intersect_point)
{
    // Set intersect point to the look direction for the inifinity case.
    intersect_point = look_direction;

    return _mesh->intersect(origin, look_direction, &intersect_point, 
                            nullptr, nullptr);
}

// Counts the number of times a ray hits the surface.
// Return codes:
// 0 == no intersection
// 1 == 1 intersect point
int PigSurfaceMesh::countIntersections(const PigPoint &origin,
		       const PigVector &look_direction)
{
    PigPoint intercept;			// thrown away

    return intersectRay(origin, look_direction, intercept);
}



////////////////////////////////////////////////////////////////////////
// Copy from the label struct to instance vars.  The read framework is
// handled by the superclass.
////////////////////////////////////////////////////////////////////////

void PigSurfaceMesh::useLabelStruct(LblSurfaceModel_typ *SurfaceModel)
{

    char msg[512];
    char * meshFile = nullptr;

    if (! SurfaceModel->SurfaceMeshName.Valid) {
	printError("Surface MESH name parameter not valid in label");
	return;
    }

    // Check if the mesh file does exist. Because the filename is read from
    // the label which may/may not contain the path, we check first the full 
    // path (if it's there), then if the file is not found, we check locally.

    meshFile = SurfaceModel->SurfaceMeshName.Value;

    // Does the meshfile exists using full name stored in label (may/may not 
    // have the path)
    int result = access(meshFile, F_OK);

    // If not, try to find it at the local level (without path) if path
    // was supplied
    if (result = -1) {

       // First print a warning message
       sprintf(msg, "Mesh file path/name: %s read from labels not found",
               meshFile);
       printMsg(msg, "SurfaceMesh", PigMsgWarning);

       // Check if file exists locally
       meshFile = strrchr(meshFile, '/');
       if (meshFile != nullptr) {  // if null, then file name did not have path
          meshFile++;
          result = access(meshFile, F_OK);
      
          if (result == -1) 
             sprintf(msg, "Mesh file: %s read from labels not found locally",
                     meshFile);
          else
             sprintf(msg, "Mesh file: %s read from labels found locally",
                     meshFile);

          printMsg(msg, "SurfaceMesh", PigMsgWarning);
       }
    }

    if (result == -1) { 
       printError("Mesh file read from labels not found");
       return;
    }

    // Mesh file is found, load it to the mesh obj
    _mesh->loadMeshFile(meshFile);

    // Set the mesh CS
    _mesh->setCS(_cs);

    return;
}



////////////////////////////////////////////////////////////////////////
// Copy from instance vars to the label struct.  The write framework is
// handled by the superclass.
////////////////////////////////////////////////////////////////////////

void PigSurfaceMesh::fillLabelStruct(LblSurfaceModel_typ *SurfaceModel) const
{
    // If mesh is invalid, then no filename
    if (_mesh == nullptr) {
       strcpy(SurfaceModel->SurfaceMeshName.Value, "");
       SurfaceModel->SurfaceMeshName.Valid = 0;
       return;
    }

    // If mesh is empty, most probably no filename. Although we could have the
    // possibility of an empty meshfile.
    if (_mesh->getNumVertices() == 0) {
       strcpy(SurfaceModel->SurfaceMeshName.Value, "");
       SurfaceModel->SurfaceMeshName.Valid = 0;
       return;
    }

    // All seems good
    // Label size array is 256. If not enough will need to be changed
    strncpy(SurfaceModel->SurfaceMeshName.Value, _mesh->getMeshName(), strlen(_mesh->getMeshName()));
    SurfaceModel->SurfaceMeshName.Valid = 1;
 
}

////////////////////////////////////////////////////////////////////////
// Print the model in a readable form...  
////////////////////////////////////////////////////////////////////////

void PigSurfaceMesh::print()
{
    char msg[512];
    sprintf(msg, "Surface: %s, Num vertices=%d Num faces=%d", getModelName(),
		_mesh->getNumVertices(), _mesh->getNumTriangles());
    printInfo(msg);
}

////////////////////////////////////////////////////////////////////////
// These functions allow access to the parameters, in order to allow
// "tweaks" to the surface model.
// Parameters are:
//    X, Y, Z, Radius
////////////////////////////////////////////////////////////////////////

void PigSurfaceMesh::getPointingParameters(double params[],
							const int max_count)
{
/*
    double p[3];
    _center.getXYZ(p);

    if (max_count >= 1)
	params[0] = p[0];
    if (max_count >= 2)
	params[1] = p[1];
    if (max_count >= 3)
	params[2] = p[2];
    if (max_count >= 4)
	params[3] = _radius;
*/
   for (int i=0; i<max_count; i++)
      params[i] = 0.0;
}

void PigSurfaceMesh::setPointingParameters(const double params[],
						const int count)
{
    // Note that there's no "re-pointing" involved here...
/*
    double p[3];
    _center.getXYZ(p);

    if (count >= 1)
	p[0] = params[0];
    if (count >= 2)
	p[1] = params[1];
    if (count >= 3)
	p[2] = params[2];
    if (count >= 4)
	_radius = params[3];

    _center.setXYZ(p);
*/
}

const char *const PigSurfaceMesh::getPointingParamName(int i)
{
/*
    switch (i) {
	case 0:
	    return "X";
	case 1:
	    return "Y";
	case 2:
	    return "Z";
	case 3:
	    return "Radius";
	default:
	    return "Unknown";
    }
*/
   return "None";
}

void PigSurfaceMesh::getPointingErrorEstimate(double errors[],
					const int max_count)
{
/*
    int i;
    int n = max_count;
    if (n > 4) n = 4;		// only 4 params

    for (i=0; i < n; i++)
	errors[i] = .02;	// 2 cm error both in position and radius
*/
   for (int i=0; i<max_count; i++)
      errors[i] = 0.0;
}

#endif

