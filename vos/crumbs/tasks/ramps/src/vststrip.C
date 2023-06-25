// vststrip.C 1.2 02/08/13 15:56:53
/** \file
// Triangle strip sorting for ASD-to-ViSTa converter
*/
#include <stdio.h>
#include <assert.h>
#include "asdvst.h"

extern int fnum(Face *f);

// input triangle list, chained by face->next links
static Face *inlist;

// output triangle list, chained by face->adj[0] links
// (triangles moved to output list have nadj set to 4)
static Face *outlist;

// Rotate face f1 and f2 vertices so that f1,f2 is a valid initial
// triangle strip (f1's v[1]-v[2] is f2's v[0]-v[1])
static void rotate_initial(Face *f1, Face *f2)
{
	Vertex *a = f1->v[0];
	Vertex *b = f1->v[1];
	Vertex *c = f1->v[2];
	
	Vertex *d = f2->v[0];
	Vertex *e = f2->v[1];
	Vertex *f = f2->v[2];

	if (a == d) {
		if (b == f) {	// shared edge is ab and fd
			// set f1 as cab, f2 as abe
			f1->v[0] = c;
			f1->v[1] = a;
			f1->v[2] = b;
			f2->v[2] = e;
		} else {	// shared edge is ca and de
			assert(c==e);
			// set f1 as bca, f2 as caf
			f1->v[0] = b;
			f1->v[1] = c;
			f1->v[2] = a;
			f2->v[2] = f;
		}
	} else if (a == e) {
		if (b == d) {	// shared edge is ab and de
			// set f1 as cab, f2 as abf
			f1->v[0] = c;
			f1->v[1] = a;
			f1->v[2] = b;
			f2->v[2] = f;
		} else {	// shared edge is ca and ef
			assert(c==f);
			// set f1 as bca, f2 as cad
			f1->v[0] = b;
			f1->v[1] = c;
			f1->v[2] = a;
			f2->v[2] = d;
		}
	} else if (a == f) {
		if (b == e) {	// shared edge is ab and ef	
			// set f1 as cab, f2 as abd
			f1->v[0] = c;
			f1->v[1] = a;
			f1->v[2] = b;
			f2->v[2] = d;
		} else {	// shared edge is ca and fd
			assert(c==d);
			// set f1 as bca, f2 as cae
			f1->v[0] = b;
			f1->v[1] = c;
			f1->v[2] = a;
			f2->v[2] = e;
		}
	} else {	// a is unique, shared edge is bc
		if (b == d) {	// shared edge is bc and fd
			// set f1 as abc, f2 as bce
			f2->v[2] = e;
		} else if (b == f) {	// shared edge is bc and ef
			// set f1 as abc, f2 as bcd
			f2->v[2] = d;
		} else {	// shared edge is bc and de
			assert(b==e);
			// set f1 as abc, f2 as bcf
			;
		}
	}
	
	f2->v[0] = f1->v[1];
	f2->v[1] = f1->v[2];
}

// Before moving face f1 to output list, any adjacent faces
// other than f2 (a strip neighbor) need to be updated to no
// longer consider it adjacent.
static void unlink_adj(Face *f1, Face *f2)
{
	for (int i=0; i<f1->nadj; i++) {
		Face *f3 = f1->adj[i];
		if (f3 != f2) {
//printf("unlink %d from %d[%d]\n", fnum(f3), fnum(f1), i);
			// f3 needs to forget about neighbor f1
			int last = f3->nadj - 1;
			assert(last >= 0 && last <= 2);
			if (f3->adj[last] == f1) {
				; // easy case, f1 is last one
			} else if (f3->adj[last - 1] == f1) {
				// next-to-last one
				f3->adj[last - 1] = f3->adj[last];
			} else {
				// f1 is first of 3
				assert(last == 2 && f3->adj[0] == f1);
				f3->adj[0] = f3->adj[2];
			}
			f3->nadj--;
		}
	}
}

// Find the adjacent face of f that follows it in a triangle strip
// (uses f->v[1] and f->v[2]), and rotate vertices if necessary
// so that fnext->v[2] is the new vertex.
// Return NULL if no valid follower
static Face *next_face(Face *f)
{
	Vertex *v1 = f->v[1];
	Vertex *v2 = f->v[2];

	for (int i=0; i<f->nadj; i++) {
		Face *fnext = f->adj[i];
		if (fnext->nadj > 3)	// already stripped?
			continue;
		Vertex *a = fnext->v[0];
		Vertex *b = fnext->v[1];
		Vertex *c = fnext->v[2];

		if (a == v1) {
			if (b == v2) {
				return fnext;	// already good as abc
			} else if (c == v2) {
				// okay but reorder as acb
				fnext->v[1] = c;
				fnext->v[2] = b;
				return fnext;
			}
		} else if (b == v1) {
			if (a == v2) {
				// okay but reorder as bac
				fnext->v[0] = b;
				fnext->v[1] = a;
				return fnext;
			} else if (c == v2) {
				// okay but reorder as bca
				fnext->v[0] = b;
				fnext->v[1] = c;
				fnext->v[2] = a;
				return fnext;
			}
		} else if (c == v1) {
			if (a == v2) {
				// okay but reorder as cab
				fnext->v[0] = c;
				fnext->v[1] = a;
				fnext->v[2] = b;
				return fnext;
			} else if (b == v2) {
				// okay but reorder as cba
				fnext->v[0] = c;
				fnext->v[2] = a;
				return fnext;
			}
		}
	}
	return NULL;
}

// Reorder faces for triangle strips, starting with triangles having
// "nadj" adjacent faces. Return number moved to output list.
// Rotate triangle vertices as necessary.
static int sort_faces_with(int nadj)
{
	int nfaces = 0;	// number faces added to output list

	// scan input list
	for (Face *f = inlist; f; f = f->next) {
		if (f->nadj < 1 || f->nadj > nadj)
			continue;	// not a valid starting candidate

		// choose second face with min adjacent faces itself
		Face *fnext = f->adj[0];
		if (nadj > 1 && f->adj[1]->nadj < fnext->nadj)
			fnext = f->adj[1];
		if (nadj > 2 && f->adj[2]->nadj < fnext->nadj)
			fnext = f->adj[2];

		// rotate first faces to be valid strip start
		rotate_initial(f, fnext);

		// add first face to output list
//printf("Strip start %d (nadj %d)\n", fnum(f), f->nadj);
		unlink_adj(f, fnext);
		f->nadj = 4;		// mark as done
		Face *strip = f;	// save strip start
		nfaces++;

		// add following faces to strip
		while (fnext) {
			// add fnext to strip list
//printf(" strip add %d (nadj %d)\n", fnum(fnext), fnext->nadj);
			unlink_adj(fnext, f);
			f->adj[0] = fnext;
			f = fnext;
			nfaces++;

			// find next face to follow f (one of f->adj[i])
			fnext = next_face(f);
			f->nadj = 4;	// mark as done
		}
		
		// add strip to output list
		f->adj[0] = outlist;
		outlist = strip;
	}

	return nfaces;
}

/**
// Count, and optionally reorder faces for better triangle stripping.
// Rotate triangle vertices as necessary.
// Input list is chained on "next" links;
// Output list is chained on "adj[0]" links.
**/
int sort_faces(Face **flist, int sort)
{
	Face *f;

	// total face count
	int nfaces = 0;

	inlist = *flist;

	// If don't want to sort faces, just count them and
	// copy links to build output chain
	if (!sort) {
		for (f = inlist; f;) {
			nfaces++;
			f = (f->adj[0] = f->next);
		}
		return nfaces;
	}

	// Update adjacency info
	for (f = inlist; f; f = f->next) {
		for (int i=0; i<f->nadj; i++) {
			Face *a = f->adj[i];
			// If adjacent face was rejected during input
			// (e.g. backward facing), remove it from f's list
			if (a->v[0] == NULL) {
				if (i < 1)
					f->adj[0] = f->adj[1];
				if (i < 2)
					f->adj[1] = f->adj[2];
				f->nadj--;
				i--;

			// If adjacent face was carried over from LOD(n-1),
			// link back to LOD(n) face f.
			// This doesn't restore adjacency between
			// pairs of LOD(n-1) faces carried into LOD(n)
			} else if (a->nadj == 0) {
				a->adj[(a->nadj)++] = f;
			}
		}
	}

	// To produce longer strips on average, start with 
	// poorly-connected triangles.
	// It would be more efficient to remove stripped triangles
	// from the input list, but since they're randomly accessed
	// in a singly-linked list, we'll just tag 'em...
	outlist = NULL;
	for (;;) {
		// strip singly-connected faces
		int n = sort_faces_with(1);
		if (n == 0) {		// didn't find any, try doubles
			n = sort_faces_with(2);
			if (n == 0) {	// nope, do triply-connected
				n = sort_faces_with(3);
				if (n == 0)
					break;
			}
		}
		nfaces += n;
	}

	// take anything left - should just be isolated faces
	for (f = inlist; f; f = f->next) {
		if (f->nadj < 4) {
			nfaces++;
			f->adj[0] = outlist;
			outlist = f;
		}
	}

	// done
	*flist = outlist;
	return nfaces;
}
