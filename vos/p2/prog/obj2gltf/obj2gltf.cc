//////////////////////////////////////////////////////////////////////////
// obj2gltf
//
// This program converts .obj mesh files to gltf/glb files.
//
//////////////////////////////////////////////////////////////////////////

#define TINYOBJLOADER_IMPLEMENTATION
#define TINYGLTF_IMPLEMENTATION
#define STB_IMAGE_IMPLEMENTATION
#define STB_IMAGE_WRITE_IMPLEMENTATION

#include "vicmain_c"
#include "tiny_obj_loader.h"
#include "tiny_gltf.h"

#include <stdlib.h>
#include <stdio.h>
#include <iostream>
#include <cfloat>
#include <math.h>
#include <string>
#include <vector>
#include <limits>
#include <cctype>
#include <tuple>

#ifdef _OPENMP
#include <omp.h>
#endif

void loadMesh(const char *objFile,
              tinyobj::attrib_t &attrib,
              std::vector<tinyobj::shape_t> &shapes,
              std::vector<tinyobj::material_t> &materials,
              const char* mtlDir);

void writeGLTF(std::string filepath, tinyobj::attrib_t &in_attrib,
               std::vector<tinyobj::shape_t> &in_shapes,
               std::vector<tinyobj::material_t> &in_materials, 
               bool write_binary, bool flip_texture);

void packMesh( const tinyobj::attrib_t &in_attrib,
               const tinyobj::shape_t &in_shape,
               tinyobj::attrib_t &out_attrib,
               tinyobj::shape_t &out_shape);
               
void test();

const unsigned int floatsPerPosition = 3;
const unsigned int floatsPerTexCoord = 2;
const unsigned int vertsPerPolygon = 3; //triangle

void main44()
{
    zvmessage("obj2gltf version 1.0", "");

    char msg[256];
    #ifdef _OPENMP   
        int num_thr = omp_get_max_threads();
        sprintf(msg, "Using OpenMP with %d threads", num_thr);
        zvmessage(msg, "");    
    #else
        sprintf(msg, "No OpenMP multithreading");
        zvmessage(msg, "");
    #endif

    char inp_filename[256], out_filename[256];
    int status, count, def; //def = default; not used but kept for compatibility
    int numvertices = 0;

    // Inputs
    float *vertices = NULL;

    // Read input mesh's filename
    zvp("INP", inp_filename, &count);
    sprintf(msg, "Processing mesh: %s", inp_filename);
    zvmessage(msg, "");

    // Read output text filename
    zvp("OUT", out_filename, &count);
    sprintf(msg, "Output filename: %s", out_filename);
    if (strlen(out_filename) != 0 && count == 0)
    {
        zvmessage("Parameter OUT is required.", "");
        zabend();
    }
    zvmessage(msg, "");

    char mtlPath[1024];
    zvp("MTLDIR", mtlPath, &count);
    if(count > 0)
    {
        sprintf(msg, "Looking for mtl files in: %s", mtlPath);
        zvmessage(msg, "");
    }
    else
    {
        mtlPath[0] = '\0';
    }
     
    int flip_texture = zvptst("FLIPTEX");

    std::string outFileName(out_filename);
    std::string::size_type extPeriod = outFileName.find_last_of(".");
    if (extPeriod == std::string::npos)
    {
        zvmessage("Error: using file extension to determine whether output is binary or not, \
        please change out filename extension .glb or .gltf", "");
        throw std::string("Error: using file extension to determine whether output is binary or not, \
        please change out filename extension .glb or .gltf", "");
        zabend();
    }

    // it is possible to have your assets embedded in your .gltf json code as base64 or
    // to have it as an external .bin binary file.  a .glb implies a binary file contained 
    // within the glb file container. for now, the code just uses the requested file type as
    // determination as to whether files should be binary or not. gltf: ascii, glb: binary
    std::transform(outFileName.begin(),outFileName.end(), outFileName.begin(), [](unsigned char c)
    { return std::toupper(c);});

    bool binaryOutput = false;
    if (outFileName.substr(extPeriod + 1) == "GLB")
    {
        zvmessage("Writing output as binary...", "");
        binaryOutput = true;
    }

    //Read input mesh
    tinyobj::attrib_t attrib;
    std::vector<tinyobj::shape_t> shapes;
    std::vector<tinyobj::material_t> materials;
    if (inp_filename != NULL)
    {
        loadMesh(inp_filename, attrib, shapes, materials, mtlPath);
    }

    writeGLTF(out_filename, attrib, shapes, materials, binaryOutput, flip_texture);
}

void loadMesh(const char *objFile,
              tinyobj::attrib_t &attrib,
              std::vector<tinyobj::shape_t> &shapes,
              std::vector<tinyobj::material_t> &materials,
              const char* mtlDir)
{
    std::string err;


    // Load the mesh
    zvmessage("Loading mesh file. Can take a while for large mesh file...", "");
    bool ret = tinyobj::LoadObj(&attrib, &shapes, &materials, &err, objFile,
                                 mtlDir, false);

    // Check that mesh was successfully loaded
    if (!ret)
    {
        //throw err;
        throw std::string("Mesh not loaded, please check file name");
        zabend();
    }

    if( materials.size() > 1)
    {
        zvmessage("OBJ file should contain only 1 material", "");
        throw std::string("OBJ file should contain only 1 material");
        zabend();
    }

    // It is assumed that the OBJ file contains only 1 shape and is made of
    // triangular faces. Check that the number of shapes in OBJ is equal to 1.
    if (shapes.size() != 1)
    {
        zvmessage("OBJ file should contain only 1 shape", "");
        throw std::string("OBJ file should contain only 1 shape");
        zabend();
    }

    if(attrib.normals.size() > 1)
    {
        zvmessage("Normals not supported yet...skipping them", "");
    }

    if(attrib.colors.size() > 1)
    {
        zvmessage("Vertex colors not supported yet...skipping them", "");
    }

    // Check that mesh is made out of triangular faces only
    for (unsigned int f = 0; f < shapes[0].mesh.num_face_vertices.size(); f++)
    {
        if (shapes[0].mesh.num_face_vertices[f] != floatsPerPosition)
        {
            zvmessage("OBJ file should contain only triangular faces", "");
            throw std::string("OBJ file should contain only triangular faces");
            zabend();
        }
    }

    int numVertices = (int)(attrib.vertices.size() / floatsPerPosition);
    std::cout << "Number of positions in mesh: " << numVertices << '\n';

    int numTexCoords = (int)(attrib.texcoords.size() / floatsPerTexCoord);
    std::cout << "Number of texture coordinates in mesh: " << numTexCoords << '\n';

    // glb files only allow a single buffer in this library, which means
    // a single set of indices for all attributes meaning there needs to be the
    // same number of each attributes. the code below looks for opportunities to
    // collapse verts in the obj that share all their attributes. if it can't 
    // it will emit a unique vertex even if it has to replicate some of the 
    // attributes to do so.
    if (numVertices != numTexCoords)
    {
        zvmessage("Mismatched vertex attribute counts, attempting to pack", "");

        tinyobj::attrib_t packed_attrib;
        tinyobj::shape_t packed_shape;
        packMesh(attrib, shapes[0], packed_attrib, packed_shape);
        attrib = packed_attrib;
        shapes = {packed_shape};
    }

    // Number of triangles
    int nbTriangles = (int)(shapes[0].mesh.num_face_vertices.size());
    std::cout << "Number of triangles in mesh: " << nbTriangles << '\n';
}

void writeGLTF(std::string filepath, tinyobj::attrib_t &in_attrib,
               std::vector<tinyobj::shape_t> &in_shapes,
               std::vector<tinyobj::material_t> &in_materials,
               bool write_binary, bool flip_texture)
{

    enum layout
    {
        INDICES,
        POSITIONS,
        TEXCOORD0,
        NUM_BUFFERS
    };

    tinygltf::Buffer buffer;

    tinygltf::Model m;
    tinygltf::Scene scene;
    tinygltf::Mesh mesh;
    tinygltf::Primitive primitive;
    tinygltf::Node node;

    tinygltf::BufferView bufferViewIndices;
    tinygltf::BufferView bufferViewPositions;
    tinygltf::BufferView bufferViewTexCoords;

    tinygltf::Accessor accessorIndices;
    tinygltf::Accessor accessorPositions;
    tinygltf::Accessor accessorTexCoords;

    tinygltf::Asset asset;

    //setup buffer
    size_t numTriangles = (int)(in_shapes[0].mesh.num_face_vertices.size()); //one entry per face
    size_t num_indices = numTriangles * 3;
    size_t num_index_bytes = num_indices * sizeof(int);

    size_t num_verts = in_attrib.vertices.size() / floatsPerPosition;
    size_t num_position_bytes = in_attrib.vertices.size() * sizeof(float);

    //only 2d tcs supported by tinyobjloader 
    size_t num_texcoords = in_attrib.texcoords.size() / floatsPerTexCoord;
    size_t num_texCoord_bytes = in_attrib.texcoords.size() * sizeof(float);

    size_t num_total_bytes = num_index_bytes + num_position_bytes + num_texCoord_bytes;
    buffer.data.resize(num_total_bytes);
    
    //add data
    size_t offset_indices = 0;
    for (size_t idx = 0; idx < in_shapes[0].mesh.indices.size(); idx++)
    {
        unsigned int* dst = (unsigned*)(buffer.data.data() + idx * sizeof(unsigned int));
        *dst = in_shapes[0].mesh.indices[idx].vertex_index; 
     }
        
    size_t offset_position = num_index_bytes;
    memcpy(buffer.data.data() + offset_position, in_attrib.vertices.data(), num_position_bytes);
   
    size_t offset_tc = num_index_bytes + num_position_bytes;
    memcpy(buffer.data.data() + offset_tc, in_attrib.texcoords.data(), num_texCoord_bytes);
    
    //bookkeep min max
    unsigned int max_index = std::numeric_limits<unsigned int>::min();
    unsigned int min_index = std::numeric_limits<unsigned int>::max();
    for (size_t idx = 0; idx < in_shapes[0].mesh.indices.size(); idx++)
    {
        int &curIdx = in_shapes[0].mesh.indices[idx].vertex_index;
        max_index = std::max((unsigned int)curIdx, max_index);
        min_index = std::min((unsigned int)curIdx, min_index);
    }

    float minX = std::numeric_limits<float>::max();
    float maxX = std::numeric_limits<float>::lowest();
    float minY = std::numeric_limits<float>::max();
    float maxY = std::numeric_limits<float>::lowest();
    float minZ = std::numeric_limits<float>::max();
    float maxZ = std::numeric_limits<float>::lowest();
    for (size_t idx = 0; idx < in_attrib.vertices.size();)
    {
        float &curX = in_attrib.vertices[idx++];
        float &curY = in_attrib.vertices[idx++];
        float &curZ = in_attrib.vertices[idx++];

        minX = std::min(curX, minX);
        maxX = std::max(curX, maxX);
        minY = std::min(curY, minY);
        maxY = std::max(curY, maxY);
        minZ = std::min(curZ, minZ);
        maxZ = std::max(curZ, maxZ);
    }
   
    if(flip_texture)
    {
         zvmessage("Flipping vertical texture coordinates", "");

        //if you are flipping the texture coordinates, it will be 
        // going from a place the vertical origin is going from (0 to 1)
        // to (1, 0). However texture coordinates can be > 1 or < 0
        // this math accounts for that rather than a simple flip:
        // new value = (old value - old min)/(old max - old min) * (new max - new min) + new min
        // so:
        // new value = (old value - 0)/(1 - 0) * (0 - 1) + 1
        // new value = (old value * -1) + 1
        for (size_t idx = 0; idx < in_attrib.texcoords.size();)
        {
            idx++; //curU
            float &curV = in_attrib.texcoords[idx++];
            curV = (curV * -1.0) + 1.0;
        }

         memcpy(buffer.data.data() + offset_tc, in_attrib.texcoords.data(), num_texCoord_bytes);
    }

    float minU = std::numeric_limits<float>::max();
    float maxU = std::numeric_limits<float>::lowest();
    float minV = std::numeric_limits<float>::max();
    float maxV = std::numeric_limits<float>::lowest();
    for (size_t idx = 0; idx < in_attrib.texcoords.size();)
    {
        float &curU = in_attrib.texcoords[idx++];
        float &curV = in_attrib.texcoords[idx++];

        minU = std::min(curU, minU);
        maxU = std::max(curU, maxU);
        minV = std::min(curV, minV);
        maxV = std::max(curV, maxV);

    }

    //setup buffer views
    bufferViewIndices.buffer = 0;
    bufferViewIndices.byteOffset = offset_indices;
    bufferViewIndices.byteLength = num_index_bytes;
    bufferViewIndices.target = TINYGLTF_TARGET_ELEMENT_ARRAY_BUFFER;

    bufferViewPositions.buffer = 0;
    bufferViewPositions.byteOffset = offset_position;
    bufferViewPositions.byteLength = num_position_bytes;
    bufferViewPositions.target = TINYGLTF_TARGET_ARRAY_BUFFER;

    bufferViewTexCoords.buffer = 0;
    bufferViewTexCoords.byteOffset = offset_tc;
    bufferViewTexCoords.byteLength = num_texCoord_bytes;
    bufferViewTexCoords.target = TINYGLTF_TARGET_ARRAY_BUFFER;

    //setup accessors
    accessorIndices.bufferView = layout::INDICES;
    accessorIndices.byteOffset = 0;
    accessorIndices.componentType = TINYGLTF_COMPONENT_TYPE_UNSIGNED_INT;
    accessorIndices.count = num_indices;
    accessorIndices.type = TINYGLTF_TYPE_SCALAR;
    accessorIndices.maxValues.push_back((double)max_index);
    accessorIndices.minValues.push_back((double)min_index);

    accessorPositions.bufferView = layout::POSITIONS;
    accessorPositions.byteOffset = 0;
    accessorPositions.componentType = TINYGLTF_COMPONENT_TYPE_FLOAT;
    accessorPositions.count = num_verts;
    accessorPositions.type = TINYGLTF_TYPE_VEC3;
    accessorPositions.maxValues = {maxX, maxY, maxZ};
    accessorPositions.minValues = {minX, minY, minZ};

    accessorTexCoords.bufferView = layout::TEXCOORD0;
    accessorTexCoords.byteOffset = 0;
    accessorTexCoords.componentType = TINYGLTF_COMPONENT_TYPE_FLOAT;
    accessorTexCoords.count = num_texcoords;
    accessorTexCoords.type = TINYGLTF_TYPE_VEC2;
    accessorTexCoords.maxValues = {maxU, maxV};
    accessorTexCoords.minValues = {minU, minV};

    primitive.indices = layout::INDICES;
    primitive.attributes["POSITION"] = layout::POSITIONS;
    primitive.attributes["TEXCOORD_0"] = layout::TEXCOORD0;
    primitive.material = 0;
    primitive.mode = TINYGLTF_MODE_TRIANGLES;
    mesh.primitives.push_back(primitive);

    node.mesh = 0;
    scene.nodes.push_back(0);

    asset.version = "2.0";
    asset.generator = "tinygltf";

    m.scenes.push_back(scene);
    m.meshes.push_back(mesh);
    m.nodes.push_back(node);

    m.buffers.push_back(buffer);
    
    m.bufferViews.resize(static_cast<int>(layout::NUM_BUFFERS));
    m.bufferViews[layout::INDICES] = bufferViewIndices;
    m.bufferViews[layout::POSITIONS] = bufferViewPositions;
    m.bufferViews[layout::TEXCOORD0] = bufferViewTexCoords;
    
    m.accessors.resize(static_cast<int>(layout::NUM_BUFFERS));
    m.accessors[layout::INDICES] = accessorIndices;
    m.accessors[layout::POSITIONS] = accessorPositions;
    m.accessors[layout::TEXCOORD0] = accessorTexCoords;

    m.asset = asset;

    tinygltf::Material mat;
    tinygltf::Sampler colorSampler;
    tinygltf::Image colorImage;
    tinygltf::Texture colorTexture;
    tinygltf::TextureInfo colorTextureInfo;
    if(in_materials.size() == 1 && !in_materials[0].diffuse_texname.empty())
    {
        zvmessage("Found .mtl file with a single diffuse texture", "");

        colorSampler.name = "mipmap trilinear";
        colorSampler.minFilter = TINYGLTF_TEXTURE_FILTER_LINEAR_MIPMAP_LINEAR;
        colorSampler.magFilter = TINYGLTF_TEXTURE_FILTER_LINEAR;
        colorSampler.wrapS = TINYGLTF_TEXTURE_WRAP_CLAMP_TO_EDGE;
        colorSampler.wrapT = TINYGLTF_TEXTURE_WRAP_CLAMP_TO_EDGE;
        m.samplers.push_back(colorSampler);

        colorImage.name = in_materials[0].diffuse_texname;
        colorImage.uri = in_materials[0].diffuse_texname;
        m.images.push_back(colorImage);

        colorTexture.name = in_materials[0].diffuse_texname;
        colorTexture.sampler = 0;
        colorTexture.source = 0;
        m.textures.push_back(colorTexture);

        colorTextureInfo.index = 0;
        colorTextureInfo.texCoord = 0;

        mat.pbrMetallicRoughness.baseColorTexture = colorTextureInfo;
        mat.pbrMetallicRoughness.roughnessFactor = 1;
        mat.pbrMetallicRoughness.metallicFactor = 0;
        mat.doubleSided = false;
        
    }
    else
    {
        if(in_materials.size() > 1)
        {
            zvmessage("Only a single material is supported using a default gray", "");
        }
        else
        {
            zvmessage("Missing mtl file, using a default gray", "");
        }
            
        mat.pbrMetallicRoughness.baseColorFactor = {0.8f, 0.8f, 0.8f, 1.0f};
        mat.doubleSided = true;
    }
    m.materials.push_back(mat);
  
    tinygltf::TinyGLTF gltf;
    bool embedImages = false;
    bool embedBuffers = true;
    bool prettyPrint = true;
    if (!gltf.WriteGltfSceneToFile(&m, filepath,
                                   embedImages,
                                   embedBuffers,
                                   prettyPrint,
                                   write_binary))
    {
        zvmessage("Failed to write GLTF to file", "");
        throw std::string("Failed to write GLTF to file");
        zabend();
    }

    zvmessage("Saved output to file.", "");
}

bool eltsEqual(const std::vector<tinyobj::real_t>& vals, unsigned int numPerElt, unsigned int idx0, unsigned int idx1 )
{
    //skip loop setup if first doesn't match
    if(vals[idx0*numPerElt] != vals[idx1*numPerElt])
        return false;

    for(int offset = 1; offset < numPerElt; offset++)
    {
        if(vals[idx0*numPerElt+offset] != vals[idx1*numPerElt+offset])
            return false;
    }

    return true;
}

//hash the values by adding elements to reduce number of compares
void remap_dupes(const std::vector<tinyobj::real_t>& vals, unsigned int numPerElt, std::vector<unsigned int>& remappedIndices)
{
    const unsigned int numElts = vals.size() / numPerElt;
    char msg[1024];

    // initialize with the original indices
    remappedIndices.reserve(numElts);
    for (size_t idx0 = 0; idx0 < numElts; idx0++ )
    {
        remappedIndices.push_back(idx0);
    }

    // hash them (not uniquely)
    std::multimap<tinyobj::real_t, int> m;
    for (size_t idx = 0; idx < numElts; idx++ )
    {
        //make key
        tinyobj::real_t key = 0;
        for(int idxSub = 0; idxSub < numPerElt; idxSub++)
        {
            key += vals[idx*numPerElt+idxSub];
        }
         
         m.insert(std::pair<tinyobj::real_t, int>(key,idx));
    }

    // look for dupes
    size_t curKey = 0;
    size_t numKeys = m.size();
    int prevDonePct = 0;
    int deepestKeyMatch=0;
    auto keyUpperBound = m.upper_bound(m.begin()->first);
    for(auto it = m.begin(); it != m.end(); it = keyUpperBound)
    {
        //progress bar
        int donePct = (int)(100 * curKey/(float)numKeys);
        if(donePct > prevDonePct)
        {
            sprintf(msg, "Done %d%%...",donePct);
            zvmessage(msg, "");
            prevDonePct = donePct;
        }

        //get potential matching set of duplicate keys
        keyUpperBound = m.upper_bound(it->first);

        deepestKeyMatch = std::max(deepestKeyMatch,(int)std::distance(it,keyUpperBound));

        auto it0 = it;

        for(; it0 != keyUpperBound; ++it0)
        {
            size_t idx0 = it0->second;
            
            auto it1 = it0;
            for(++it1; it1 != keyUpperBound; ++it1)
            {
                //if already remapped, skip
                size_t idx1 = it1->second;
                if(remappedIndices[idx1] != idx1)
                    continue;

                //if dupe, remap with lower index (idx0)
                if(eltsEqual(vals,numPerElt,idx0,idx1))
                {
                    remappedIndices[idx1] = idx0;
                }
            }
        }

        curKey++;
    }

    sprintf(msg, "Most hash collisions %d",deepestKeyMatch);
    zvmessage(msg, "");
}

void packMesh( const tinyobj::attrib_t &in_attrib,
               const tinyobj::shape_t &in_shape,
               tinyobj::attrib_t &out_attrib,
               tinyobj::shape_t &out_shape)
{    
    //make a naive remapping of indices that have the same true value 
    zvmessage("Finding duplicate positions...", "");
    std::vector<unsigned int> remappedPositions;
    remap_dupes(in_attrib.vertices, floatsPerPosition, remappedPositions);

    zvmessage("Finding duplicate texture coordinates...", "");
    std::vector<unsigned int> remappedTexCoords;
    remap_dupes(in_attrib.texcoords, floatsPerTexCoord, remappedTexCoords);

#if _DEBUG
    const unsigned int numEltsPos = in_attrib.vertices.size() / floatsPerPosition;
    for(int idx=0; idx < numEltsPos; idx++)
    {        
        if(!eltsEqual(in_attrib.vertices,floatsPerPosition,idx,remappedPositions[idx]))
        {
            zabend();
        }
    }

   const unsigned int numEltsTex = in_attrib.texcoords.size() / floatsPerTexCoord;
    for(int idx=0; idx < numEltsTex; idx++)
    {        
        if(!eltsEqual(in_attrib.texcoords,floatsPerTexCoord,idx,remappedTexCoords[idx]))
        {
            zabend();
        }
    }
#endif

    //indices: pos, texture coord
    std::vector<std::tuple<unsigned int,unsigned int>> uniqueRemappedVerts; 
    uniqueRemappedVerts.reserve(in_shape.mesh.indices.size());

    std::vector<unsigned int> uniqueRemappedToIndexData;
    uniqueRemappedVerts.reserve(in_shape.mesh.indices.size());

    out_shape = in_shape;
    out_shape.mesh = tinyobj::mesh_t(); //clear out real data to rebuild

    zvmessage("Packing vertices...", "");
    char msg[1024];
    int globalVertIdx = 0;
    int prevDonePct = 0;
    unsigned int shared = 0;
    int numFaces = in_shape.mesh.indices.size()/vertsPerPolygon;
    for (unsigned int idxFace = 0; idxFace < numFaces; idxFace++)
    {
        int donePct = (int)(100 * idxFace/(float)numFaces);
        if(donePct > prevDonePct)
        {
            sprintf(msg, "Done %d%%...",donePct);
            zvmessage(msg, "");
            prevDonePct = donePct;
        }

       for(unsigned int idxFaceVert = 0; idxFaceVert < vertsPerPolygon; idxFaceVert++)
       {            
            const tinyobj::index_t& origIndices = in_shape.mesh.indices[globalVertIdx];

            //translate vert into the remapped indices to test if it is unique
            std::tuple<unsigned int,unsigned int> vertToTest(
                remappedPositions[origIndices.vertex_index],
                remappedTexCoords[origIndices.texcoord_index]);

            //if its already there use the remap 
            auto pos = std::find(uniqueRemappedVerts.begin(), uniqueRemappedVerts.end(),
                       vertToTest);

            if( pos == uniqueRemappedVerts.end())
            {                
                tinyobj::index_t out_Index = {(int)(out_attrib.vertices.size()/floatsPerPosition), -1, (int)(out_attrib.texcoords.size()/floatsPerTexCoord)};
             
                //record that this combo of indices was no seen and can be reused
                uniqueRemappedVerts.push_back(vertToTest);

                //record location of index data in index buffer to be reused
                uniqueRemappedToIndexData.push_back(out_shape.mesh.indices.size());

                //save index data to mesh
                out_shape.mesh.indices.push_back(out_Index);

                //this is the first time we've seen this combination of indices, add it
                for(int idx=0; idx < floatsPerPosition; idx++)
                {
                    out_attrib.vertices.push_back(in_attrib.vertices[origIndices.vertex_index * floatsPerPosition + idx]);
                }

                for(int idx=0; idx < floatsPerTexCoord; idx++)
                {
                    out_attrib.texcoords.push_back(in_attrib.texcoords[origIndices.texcoord_index * floatsPerTexCoord + idx]);
                }
            }
            else
            {
                //this vert is sharable, re-use old indices
                int uniqueRemapIndex = pos - uniqueRemappedVerts.begin();
                out_shape.mesh.indices.push_back(out_shape.mesh.indices[uniqueRemappedToIndexData[uniqueRemapIndex]]);
                shared++;
            }

            globalVertIdx++;
        }
             
        out_shape.mesh.num_face_vertices.push_back(vertsPerPolygon);

        //only support a single smoothing group, and single material
        out_shape.mesh.material_ids.push_back(in_shape.mesh.material_ids[0]);
        out_shape.mesh.smoothing_group_ids.push_back(in_shape.mesh.smoothing_group_ids[0]);
    }

    sprintf(msg, "Found %d unique verts and %d shared verts",  uniqueRemappedVerts.size(), shared);
    zvmessage(msg, "");

    out_shape.mesh.tags = in_shape.mesh.tags;
}