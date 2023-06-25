#ifndef _ASDVST_H_
#define _ASDVST_H_
// asdvst.h 1.2 02/07/10 15:37:41
/** \file
// Definitions for ASD to ViSTa file conversion
*/

/// data saved for each vertex attribute (merged with vertex data at output)
struct Attr {
	float s, t;		///< texture coordinates
};

/// output vertex info
struct SubVert {
	int attr;		///< attribute index (-1 = not assigned)
	int id;			///< output file index (-1 = not written yet)
};

/// input vertex data
struct Vertex {
	float xyz[3];		///< spatial coordinates
	SubVert *sv;		///< output info (allocated, one per texture)
};

/// data saved for each face
struct Face {
	Face *next;		///< list linkage
	Face *adj[3];		///< adjacent faces
	Vertex *v[3];		///< vertex pointers (v[0]==NULL if rejected)
	char final;		///< face has no kids -> used in remaining LODs?
	char nadj;		///< number of adjacent faces	
};

/// data saved for each LOD
struct LOD {
	float switchin;		///< LOD switch distance
	Face **face;		///< head of face list for each texture
};

int sort_faces(Face **flist, int sort);

#endif
