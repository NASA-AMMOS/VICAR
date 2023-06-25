//////////////////////////////////////////////////////////////////////////
// obj2plane
//
// This program provides the ability to fit a plane to set of vertices from a 
// triangle mesh. Either all or a decimated subset of vertices can be used. The 
// output will be a 3D plane average vector and 3D normal vector in 
// floating-point format.
//////////////////////////////////////////////////////////////////////////

#define TINYOBJLOADER_IMPLEMENTATION
#include "vicmain_c"
#include "tiny_obj_loader.h"

#include <stdlib.h>
#include <stdio.h>
#include <iostream>
#include <cfloat>
#include <math.h> 
#include <string>
#include <vector>
#include <Eigen/Dense>
#include <Eigen/SVD>

#include "taeconf.inp"
#include "parblk.inc"

using namespace std;
using namespace Eigen;

extern "C" {
    int q_real(struct PARBLK *p, char *name, int count, float *real, int mode);
    int q_init(struct PARBLK *p, int pool_size, int mode);
}

////////////////////////////////////////////////////////////////////////////////
// Read the set of vertices from a mesh.
////////////////////////////////////////////////////////////////////////////////
float * loadMesh(const char * objFile, int &numvertices);

////////////////////////////////////////////////////////////////////////////////
// Find the best-fit plane to a set of vertices via least-squares.
////////////////////////////////////////////////////////////////////////////////
float * computeBestFitPlaneNormal(float *vertices, int numvertices, int decimation, char algo[256], float vertex_avg[3], int vertsremaining); 


void main44() {
    zvmessage("Obj2plane version 2.0", "");

    char msg[256];
    char inp_filename[256], out_filename[256];
    int status, count, def; //def = default; not used but kept for compatibility
    int numvertices=0;
    
    // Inputs
    float * vertices = NULL;

    // Outputs
    float vertex_avg[3] = {0.0};
    float * plane_normal = NULL;
	
    // User parameters
    int decimation;
	char algo[256];

    // Read input mesh's filename
    zvp("INP", inp_filename, &count);
    sprintf(msg, "Processing mesh: %s", inp_filename);
    zvmessage(msg, "");

    // Read output text filename
    zvp("OUT", out_filename, &count);
    sprintf(msg, "Output filename: %s", out_filename);
    if (strlen(out_filename) != 0 && count == 0) {
        zvmessage("Parameter OUT is required.", "");
        zabend();
    } 
    zvmessage(msg, "");

    // Read and validate parameter DECIMATION 
    zvp("DECIMATION", &decimation, &count);
    if (count == 0) {
        zvmessage("Parameter DECIMATION is required.", "");
        zabend();
    } 
    sprintf(msg, "Decimation value: %d", decimation);
    zvmessage(msg, "");

	// Read and validate parameter ALGO
    zvp("ALGO", algo, &count);
    if (count == 0) {
        zvmessage("Parameter ALGO is required.", "");
        zabend();
    } 
    sprintf(msg, "Algorithm type: %s", algo);
    zvmessage(msg, "");


    //Read input mesh
    if (inp_filename != NULL) 
       vertices = loadMesh(inp_filename, numvertices);
// cout << "Number of vertices in mesh: " << numvertices << endl;
// for(int i=0; i<numvertices; i++) {
//     cout << vertices[3*i] << ", " << vertices[3*i+1] << ", " << vertices[3*i+2] << endl;
// } 

    // Main procedure
   
    // 1) Compute an average of all available vertices
	int vertex_counter = 0; 
    for(int i=0; i<numvertices; i+=decimation) {
        vertex_avg[0] += vertices[3*i];
	    vertex_avg[1] += vertices[3*i+1];
	    vertex_avg[2] += vertices[3*i+2];
		vertex_counter += 1;
    } 
	sprintf(msg, "Number of vertices after decimation: %d", vertex_counter);
    zvmessage(msg, "");
    vertex_avg[0] /= vertex_counter;
    vertex_avg[1] /= vertex_counter;
    vertex_avg[2] /= vertex_counter;

/////
// Simple test case: (0,0,0), (0,1,0), (1,0,0), (1,1,0)
//float vertices1[12] = {0.0,0.0,0.0,0.0,1.0,0.0,1.0,0.0,0.0,1.0,1.0,0.0};
//numvertices = 4;
/////

    // 2) Plane-fitting and surface normal computation
    zvmessage("Plane-fitting phase", "");
    plane_normal = computeBestFitPlaneNormal(vertices, numvertices, decimation, algo, vertex_avg, vertex_counter);
    if(plane_normal == NULL || vertex_counter < 3) {
		zvmessage("Not enough vertices (at least 3) for planar fit","");
        throw std::string("Not enough vertices (3) for planar fit");
		zabend();
    }
	sprintf(msg, "Computed plane average value = (%f %f %f), normal = (%f %f %f)\n", 
	vertex_avg[0], vertex_avg[1], vertex_avg[2], plane_normal[0], plane_normal[1], 
    plane_normal[2]);
    
    // 3) Output the computed vertex average and plane normal
    // The output text file should look like the following example:
    // 		Average=(1.2 3.4 5.6) normal=(0.2 -0.1 -0.8)
    //
    // Open output file
    FILE * pFile;
    pFile = fopen (out_filename,"w");
    // Write output
    fprintf(pFile, "Average = (%f %f %f), normal = (%f %f %f)\n", vertex_avg[0], 
    vertex_avg[1], vertex_avg[2], plane_normal[0], plane_normal[1], 
    plane_normal[2]);
    fclose(pFile);
    zvmessage("Saved output to file.", "");

    // 4) Output to TAE variables
    struct PARBLK par_block;
    q_init(&par_block, P_BYTES, P_ABORT);
    q_real(&par_block, "VERTEX_AVG", 3, vertex_avg, P_ADD);
    q_real(&par_block, "PLANE_NORMAL", 3, plane_normal, P_ADD);
    zvq_out(&par_block);

}


////////////////////////////////////////////////////////////////////////////////
// Read the set of vertices from a mesh.
////////////////////////////////////////////////////////////////////////////////
float * loadMesh(const char * objFile, int &numvertices) {

    char msg[256];

    if (objFile == NULL) { 
    	zvmessage("Invalid mesh filename (nullptr)","");
		zabend();
    }
   
    // Container for the OBJ reader
    tinyobj::attrib_t attrib;
    std::vector<tinyobj::shape_t> shapes;
    std::vector<tinyobj::material_t> materials;

    std::string err;

    // Load the mesh
    zvmessage("Loading mesh file. Can take a while for large mesh file...","");
    bool ret = tinyobj::LoadObj(&attrib, &shapes, &materials, &err, objFile,
                                NULL, false);

    // Check that mesh was successfully loaded
    if (!ret) {
        //throw err;
		throw std::string("Mesh not loaded, please check file name");
		zabend();
	}

    // It is assumed that the OBJ file contains only 1 shape and is made of 
    // triangular faces. Check that the number of shapes in OBJ is equal to 1.
    if (shapes.size() != 1) {
	zvmessage("OBJ file should contain only 1 shape","");
        throw std::string("OBJ file should contain only 1 shape");
		zabend();
    }

    // Check that mesh is made out of triangular faces only
    for (unsigned int f = 0; f<shapes[0].mesh.num_face_vertices.size(); f++) {
       	if (shapes[0].mesh.num_face_vertices[f] != 3) {
	    zvmessage("OBJ file should contain only triangular faces","");
            throw std::string("OBJ file should contain only triangular faces");
			zabend();
	    }
    }

    // Retrieve relevant information on the loaded mesh   

    // Number of vertices
    int nbVertices = (int)(attrib.vertices.size() / 3 );
    std::cout << "Number of points in mesh: " << nbVertices << '\n';
    numvertices = nbVertices;

    // Number of triangles
    int nbTriangles = (int)(shapes[0].mesh.num_face_vertices.size()); 
    std::cout << "Number of triangles in mesh: " << nbTriangles << '\n';

    // Initialize containers for vertices and triangles (+1 for EMBREE??)
    float * vertices = new (std::nothrow) float[3 * nbVertices + 1]; 
    if (vertices == NULL) {
	    zvmessage("Memory allocation error when allocating vertices array","");
        throw std::string("Memory allocation error when allocating vertices array");
		zabend();
    }

    unsigned int * triangles = new (std::nothrow) unsigned int[3 * nbTriangles];
    if (triangles == NULL) {
	    zvmessage("Memory allocation error when allocating triangles array","");
        throw std::string("Memory allocation error when allocating triangles array");
		zabend();
    }

    // Copy of vertices XYZ to new array
    std::memcpy(vertices, attrib.vertices.data(), 3*nbVertices*sizeof(float));

    // Copy of triangles to new array. This is a bit more complex due to the 
    // way they are stored in the ouput of the OBJ loader
    for (unsigned int i=0; i<nbTriangles*3; i++) 
        triangles[i] = shapes[0].mesh.indices[i].vertex_index; 
  
    return vertices;

}

////////////////////////////////////////////////////////////////////////////////
// Find the best-fit plane to a set of vertices via least-squares.
////////////////////////////////////////////////////////////////////////////////
float * computeBestFitPlaneNormal(float *vertices, int numvertices, int decimation, char algo[256], float vertex_avg[3], int vertsremaining) { 
   
    // Need at least three vertices to compute the best-fit plane
    if ((float)numvertices/(float)decimation < 3.0)
        return NULL;

    float * normal = new (std::nothrow) float[3]; 

	if(strcmp(algo, "regression") == 0) { 	//Regression-based method

		MatrixXf A = MatrixXf::Zero(3,3);
		VectorXf B = VectorXf::Zero(3);

		// Fill the matrix A and vector B, taking into account the decimation 
		// parameter
		int j = 0;
		for (int i=0; i<numvertices; i+=decimation) {
		    A(0,0) += vertices[3*i]*vertices[3*i];
			A(0,1) += vertices[3*i]*vertices[3*i+1];
			A(0,2) += vertices[3*i];
			A(1,0) += vertices[3*i]*vertices[3*i+1];
			A(1,1) += vertices[3*i+1]*vertices[3*i+1];
			A(1,2) += vertices[3*i+1];
			A(2,0) += vertices[3*i];
			A(2,1) += vertices[3*i+1];
			B(0) += vertices[3*i]*vertices[3*i+2];
		    B(1) += vertices[3*i+1]*vertices[3*i+2];
			B(2) += vertices[3*i+2];
		}
		A(2,2) = numvertices;

		// Solve using SVD
		JacobiSVD<MatrixXf> svd(A, ComputeThinU | ComputeThinV);
		Matrix<float, 3, 1> X = svd.solve(B);

		// Save plane normal parameters
    	normal[0] = X(0)/X.norm();
    	normal[1] = X(1)/X.norm();
    	normal[2] = X(2)/X.norm();

	} 
	else if(strcmp(algo, "svd") == 0) {  //SVD-based method

		MatrixXf points = MatrixXf::Zero(vertsremaining,3);

		// Subtract the centroid from all 
		for (int i=0; i<numvertices; i+=decimation) {
		    points(i,0) = vertices[3*i] - vertex_avg[0];
			points(i,1) = vertices[3*i+1] - vertex_avg[1];
			points(i,2) = vertices[3*i+2] - vertex_avg[2];
		}

		// Compute SVD; the solution is the singular vector for the smallest
		// singular value, typically the right-most column of V
		JacobiSVD<MatrixXf> svd(points, ComputeThinU | ComputeThinV);
		Matrix<float, 3, 1> X = svd.matrixV().rightCols<1>();  

		// Save plane normal parameters
    	normal[0] = X(0);
    	normal[1] = X(1);
    	normal[2] = X(2);

	} 
	else {
		zvmessage("Algorithm type should be 'regression' or 'svd'","");
		zabend();
	}

    return normal;

}

