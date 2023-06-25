// %M% %I% %E% %U%
// Calculate metrics for 3D meshes corresponding to an octree model

#include <assert.h>
#include "math.h"
#include "forest.h"
#include "statdist.h"

#define DEG2RAD(d)	((d)*M_PI/180.0)
#define RAD2DEG(r)	((r)*180.0/M_PI)
#define EPSILON		(1.0E-9)

static const char usage[] = 
"Usage: %s [-v] [-i input_model] objfile [objfile...]\n"
"Compute metrics for .obj mesh\n"
"-i = input octree model, default = standard input\n"
"-v = verbose output\n"
"objfile = wavefront .obj, must be triangle mesh\n";

enum { MAX_VTRI = 16 };	// max triangle per vertex

struct Triangle;

struct Vertex {		// mesh vertex
	double p[3];			// coordinates
	int ntri;			// number of triangles using vertex
	Triangle *t[MAX_VTRI];		// list of triangles using vertex
};

struct Triangle {	// mesh triangle
	Triangle *next;			// list linkage
	Vertex *p1, *p2, *p3;		// vertices (in vertex array)
	double n[3];			// normal (from vertices)
};

static Vertex *vertex;			// allocated vertex array
static int nvertex;			// currently used slots
static int nvalloc;			// current allocation limit

static Triangle *tri_list;		// head of triangle list
static Triangle *free_triangles;	// list of freed triangles
static SfcModel sfc;			// input octree

int verbose;

// store new vertex
void new_vertex(double x, double y, double z)
{
	if (vertex == NULL) {		// first time
		nvalloc = 10000;	// initial allocation
		vertex = (Vertex *)malloc(nvalloc * sizeof(Vertex));
		if (vertex == NULL) {
			fprintf(stderr, "meshstat: can't allocate vertices\n");
			exit(1);
		}
	} else if (nvertex >= nvalloc) {
		// need to allocate more space
		nvalloc *= 2;
		vertex = (Vertex *)realloc(vertex, nvalloc * sizeof(Vertex));
		if (vertex == NULL) {
			fprintf(stderr, "meshstat: can't extend vertices\n");
			exit(1);
		}
	}

	// store coordinates
	Vertex *v = &vertex[nvertex++];
	v->p[0] = x;
	v->p[1] = y;
	v->p[2] = z;
	v->ntri = 0;
}

// link face to one of its vertices
static void link_face(Triangle *t, Vertex *v)
{
	assert(v->ntri < MAX_VTRI);
	v->t[v->ntri++] = t;
}

// store new triangle, given zero-biased vertex indices
void new_triangle(int vi1, int vi2, int vi3)
{
	assert(vi1 < nvertex);
	assert(vi2 < nvertex);
	assert(vi3 < nvertex);

	// reuse off free list if one there
	Triangle *t;
	if (free_triangles) {
		t = free_triangles;
		free_triangles = t->next;
	} else {
		// allocate a new one
		t = new Triangle();
	}

	// add to list
	t->next = tri_list;
	tri_list = t;

	// store vertex pointers
	t->p1 = vertex + vi1;
	t->p2 = vertex + vi2;
	t->p3 = vertex + vi3;

	// compute face normal
	surface_normal(t->p1->p, t->p2->p, t->p3->p, t->n);
	
	// link vertices back to face
	link_face(t, t->p1);
	link_face(t, t->p2);
	link_face(t, t->p3);
}

// display statistics results
static void show_stat(StatDist *sd, char *str)
{
	if (verbose)
		printf("%s:\n %d samples, range %f to %f, mean %f, sdev %f\n",
			str, sd->samples(), sd->min(), sd->max(), 
			sd->mean(), sd->sdev());
	else
		printf("%s %d %f %f %f %f\n",
			str, sd->samples(), sd->min(), sd->max(), 
			sd->mean(), sd->sdev());
}

// find closest mesh vertex to given coordinates, also return distance
static Vertex *closest_vertex(double *p, double *dist)
{
	double d2 = FLT_MAX;	// smallest distance squared so far
	Vertex *cv;		// closest vertex so far
	Vertex *v;
	Vertex *ev = vertex + nvertex;
	
	for (v=vertex; v<ev; v++) {
		if (v->ntri == 0)	// skip if vertex never used
			continue;
		double vd2 = distance_sqr(p, v->p);
		if (vd2 < d2) {	// new shortest
			if (vd2 < EPSILON) {
				// exact match, no need to keep looking
				*dist = 0.0;
				return v;
			}
			d2 = vd2;
			cv = v;
		}
	}
	*dist = sqrt(d2);
	return cv;			
}

// find closest face in mesh to voxel at p, where closest
// mesh vertex is v; also return distance to face.
// On input, dist = distance to vertex.
static Triangle *closest_face(double *p, Vertex *v, double *dist)
{
	Triangle *best_tri = v->t[0];
		
	for (int i=0; i<v->ntri; i++) {
		Triangle *t = v->t[i];

		// project p into plane of triangle -> s[] = p + d*N
		double d = dot_product(t->n, t->p1->p) - dot_product(t->n, p);
		double s[3];
		vector_scale(t->n, d, s);
		vector_sum(p, s, s);
		
		// test whether s is inside the triangle
		// (maybe a simpler way?)
		double e1[3], e2[3], e3[3];	// triangle edges
		vector_diff(t->p2->p, t->p1->p, e1);
		vector_diff(t->p3->p, t->p2->p, e2);
		vector_diff(t->p1->p, t->p3->p, e3);

		double v1[3], v2[3], v3[3], cp[3];
		cross_product(e1, e2, cp);
		double a = vector_magnitude(cp);	// 2*triangle area
		a += EPSILON;

		vector_diff(s, t->p3->p, v3);
		cross_product(e2, v3, cp);
		double r = vector_magnitude(cp);
		if (r <= a) {		// inside so far
		
			vector_diff(s, t->p1->p, v1);
			cross_product(e3, v1, cp);
			r += vector_magnitude(cp);
			if (r <= a) {	// still inside

				vector_diff(s, t->p2->p, v2);
				cross_product(e1, v2, cp);
				r += vector_magnitude(cp);
				if (r <= a) {
					// inside, check distance
					d = fabs(d);
					if (d < *dist) {
						*dist = d;
						best_tri = t;
					}
					continue;
				}
			}
		}

		// Outside triangle; find distance to triangle edge segments.
		// Only care if projection is inside edge - 
		// already have vertex dist.
		vector_diff(p, t->p1->p, v1);
		vector_diff(p, t->p2->p, v2);
		vector_diff(p, t->p3->p, v3);
		if (dot_product(v1, e1) >= 0.0 && 
				dot_product(v2, e1) <= 0.0) {
			cross_product(v1, e1, cp);
			d = sqrt(vector_magnitude_sqr(cp) / 
					vector_magnitude_sqr(e1));
			if (d < *dist) {
				*dist = d;
				best_tri = t;
			}
		}
		if (dot_product(v2, e2) >= 0.0 && 
				dot_product(v3, e2) <= 0.0) {
			cross_product(v2, e2, cp);
			d = sqrt(vector_magnitude_sqr(cp) / 
					vector_magnitude_sqr(e2));
			if (d < *dist) {
				*dist = d;
				best_tri = t;
			}
		}
		if (dot_product(v3, e3) >= 0.0 && 
				dot_product(v1, e3) <= 0.0) {
			cross_product(v3, e3, cp);
			d = sqrt(vector_magnitude_sqr(cp) / 
					vector_magnitude_sqr(e3));
			if (d < *dist) {
				*dist = d;
				best_tri = t;
			}
		}

	}

	return best_tri;
}

// avoid acos() NaN's from numerical imprecision
double safe_acos(double a)
{
	if (a >= 1.0)
		return 0.0;
	if (a <= -1.0)
		return -M_PI;
	return acos(a);
}
#define acos(x) safe_acos(x)

// angle in radians at p2 formed by corner p1-p2-p3
static double angle(double *p1, double *p2, double *p3)
{
	double e1[3], e2[3];
	vector_diff(p1, p2, e1);	// e1 = p2 - p1
	vector_diff(p3, p2, e2);	// e2 = p3 - p1

	// e1 . e2 = |e1| * |e2| * cos(angle), solve for angle
	double mag = vector_magnitude_sqr(e1) * vector_magnitude_sqr(e2);
	return acos(dot_product(e1, e2) / sqrt(mag));
}

// average normals of all faces using vertex into one unit normal 
static void vertex_avg_normal(Vertex *v, double *avgn)
{
	vector_copy(v->t[0]->n, avgn);
	for (int i=1; i<v->ntri; i++)
		vector_sum(v->t[i]->n, avgn, avgn);
	normalize_vector(avgn);
}

// do stats for one mesh
static void do_mesh(char *name)
{
	if (verbose)
		printf("==== Mesh file %s: ====\n", name);
	else
		printf("%s\n", name);

	// reset mesh data structures
	nvertex = 0;
	free_triangles = tri_list;
	tri_list = NULL;

	// load obj file
	FILE *fp = fopen(name, "r");
	if (fp == NULL) {
		fprintf(stderr, "meshstat: Can't open %s\n", name);
		exit(1);
	}

	char buf[1024];
	int nv = 0;		// number of vertices and faces
	int nf = 0;
	while (fgets(buf, sizeof(buf), fp)) {
		double x, y, z;
		int a, b, c;
		if (sscanf(buf, "v %lf %lf %lf", &x, &y, &z) == 3) {
			nv++;
			new_vertex(x, y, z);
		} else if (sscanf(buf, "f %d %d %d", &a, &b, &c) == 3) {
			nf++;
			new_triangle(a-1, b-1, c-1);
		}
	}
	fclose(fp);
	if (verbose)
		printf("%d vertices, %d faces\n", nv, nf);
	
	// stats on triangle angles
	Triangle *t;
	StatDist sd;
	for (t=tri_list; t; t=t->next) {
		double a1 = RAD2DEG(angle(t->p1->p, t->p2->p, t->p3->p));
		sd.sample(a1);
		double a2 = RAD2DEG(angle(t->p2->p, t->p3->p, t->p1->p));
		sd.sample(a2);
		double a3 = 180.0 - a1 - a2;
		sd.sample(a3);
	}
	show_stat(&sd, "Triangle_angles");

	// stats on distance from voxels to surface
	sd.reset();		// overall voxel-mesh distance
	StatDist sd_nvd;	// non-vertex distance
	StatDist sd_vnd;	// vertex normal delta angles
	StatDist sd_nvnd;	// non-vertex normal delta angles

	for (NodeSpec_List_Element *n=sfc.mesh.pt_list; n; n=n->next) {
		// model voxel coordinates and normal vector
		double V[3], N[3];
		n->nodespec->get_global_center(V);
		n->nodespec->get_normal(N);

		// find closest mesh vertex to voxel
		double dist; 
		Vertex *cv = closest_vertex(V, &dist);
		if (dist==0.0) {	// vertex *is* voxel
			sd.sample(0.0);	// accumulate overall distance
			double avgn[3];	// avg normal of faces using vertex
			vertex_avg_normal(cv, avgn);
			sd_vnd.sample(RAD2DEG(acos(dot_product(N, avgn))));
		} else {
			// find closest face to non-vertex voxel
			t = closest_face(V, cv, &dist);
			sd.sample(dist);	// overall voxel-mesh distance
			sd_nvd.sample(dist);	// non-vertex distance
			// angle between voxel normal and face normal
			sd_nvnd.sample(RAD2DEG(acos(dot_product(N, t->n))));
		}
	}
	show_stat(&sd, "Overall_voxel-mesh_distance");
	show_stat(&sd_nvd, "Non-vertex_voxel-mesh_distance");
	show_stat(&sd_vnd, "Vertex_normal/face_avg_normal_angle");
	show_stat(&sd_nvnd, "Non-Vertex_normal_face_normal_angle");
}

int main(int argc, char **argv)
{
	int i;
	char *infile = NULL;

	for (i=1; i<argc && argv[i][0] == '-'; i++) {
		if (!strcmp(argv[i], "-i")) {
			infile = argv[++i];
		} else if (!strcmp(argv[i], "-v")) {
			verbose = 1;
		} else {
			fprintf(stderr, "Unknown argument %s\n", argv[i]);
			fprintf(stderr, usage, argv[0]);
			return 1;
		}
	}

	if (i >= argc) {
		fprintf(stderr, "Missing obj filename(s)\n");
		fprintf(stderr, usage, argv[0]);
		return 1;
	}

	// read octree (ignore mesh)
	FILE_Dataport fp;
	if (infile) {
		if (!fp.ropen(infile)) {
			fprintf(stderr, "%s: Can't open %s for reading\n",
				argv[0], infile);
			return 1;
		}
	} else {
		fp.open(stdin);
		infile = "stdin";
	}
	char token[4096];
	get_next_token(&fp, token);
	if (!strcmp(token, "SFC_MODEL_V1"))
		get_next_token(&fp, token);
	if (!strcmp(token, "OCTREE_V1"))
		sfc.Octree::parse_in(&fp);
	else {
		fprintf(stderr, "%s: unknown input format %s\n",
				argv[0], infile);
		return 1;
	}
	// setup point list
	sfc.add_pt_list();

	for (; i<argc; i++)
		do_mesh(argv[i]);

	return 0;
}
