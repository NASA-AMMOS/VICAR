// terrquad.C 1.2 02/11/04 13:03:33
// Construct terrain quad for ViSTa file.
// This is a pair of coplanar adjacent triangles approximating
// the range map surface.

#include <math.h>
#include "summitt_func.h"

// input range map 
static ImageData *imap;
static int xres, yres;

// Given four points v1, v2, v3, v4
// Find the best-fit plane equation coefficents a, b, c, d
// where the plane is ax+by+cz+d = 0 and (a,b,c) is a unit vector normal.
// Returns d, with n set to plane normal
static double fit_plane(double *v1, double *v2, double *v3, double *v4,
	double *n)
{
	int i;
	double avg[3];

	for (i=0; i<3; i++) {
		// normal vector component
		int i2 = (i+1) % 3;
		int i3 = (i+2) % 3;
		n[i] = 	(v1[i2] - v2[i2]) * (v1[i3] + v2[i3]) +
			(v2[i2] - v3[i2]) * (v2[i3] + v3[i3]) +
			(v3[i2] - v4[i2]) * (v3[i3] + v4[i3]) +
			(v4[i2] - v1[i2]) * (v4[i3] + v1[i3]);
		// average of vertices
		avg[i] = -0.25 * (v1[i] + v2[i] + v3[i] + v4[i]);
	}

	double d = dot_product(avg, n);

	// normalize result
	double mag = vector_magnitude(n);
	vector_scale(n, 1.0/mag, n);
	return d/mag;
}

// Given plane equation (unit normal n and scalar d), replace point v
// with projection into plane
static void project(double *v, double *n, double d)
{
	// compute signed distance from v to plane
	d -= dot_product(v, n);
	// projection is d units along normal from v
	for (int i=0; i<3; i++)
		v[i] += d*n[i];
}

// Get one XYZ point from range map, return TRUE if valid
static int get_range(double *p, int x, int y)
{
	p[0] = imap->get_float(x, y, 0);
	if (p[0] < -90000.0)
		return FALSE;
	p[1] = imap->get_float(x, y, 1);
	p[2] = imap->get_float(x, y, 2);
	if (p[0] == 0.0 && p[1] == 0.0 && p[2] == 0.0)
		return FALSE;
	return TRUE;
}

// look for "corners" of range map, return TRUE if okay
static int corners(double *c0, double *c1, double *c2, double *c3)
{
	int clim = (xres < yres ? xres : yres) / 2;
	int i, j;

	for (i=0; i<clim; i++) {
		if (get_range(c0, i, i))
			goto GOT_C0;
		for (j=0; j<i; j++)
			if (get_range(c0, i, j) || get_range(c0, j, i))
				goto GOT_C0;
	}
	return FALSE;
GOT_C0:
	for (i=0; i<clim; i++) {
		int x = xres-1 - i;
		if (get_range(c1, x, i))
			goto GOT_C1;
		for (j=0; j<i; j++)
			if (get_range(c1, x, j) || get_range(c1, xres-1-j, i))
				goto GOT_C1;
	}
	return FALSE;
GOT_C1:
	for (i=0; i<clim; i++) {
		int y = yres-1 - i;
		if (get_range(c2, i, y))
			goto GOT_C2;
		for (j=0; j<i; j++)
			if (get_range(c2, i, yres-1-j) || get_range(c2, j, y))
				goto GOT_C2;
	}
	return FALSE;
GOT_C2:
	for (i=0; i<clim; i++) {
		int x = xres-1 - i;
		int y = yres-1 - i;
		if (get_range(c3, x, y))
			return TRUE;
		for (j=0; j<i; j++)
			if (get_range(c3, x, yres-1-j) || 
					get_range(c3, xres-1-j, y))
				return TRUE;
	}
	return FALSE;
}

// Build terrain quad = 2 coplanar triangles covering entire wedge.
// The base octree is needed for object-to-model transform.
// Fills in surface model with pointers to static data.
// Return allocated surface model, or NULL if trouble.
SfcModel *terrain_quad(ImageData *rmap, SfcModel *base, 
			ZMatrix cxform, Boolean xcam)
{
	double v[4][3], n[3], t[3], d;
	int i;

	imap = rmap;
	imap->get_res(&xres, &yres);

	// find corners of range map
	if (!corners(v[0], v[1], v[2], v[3]))
		return NULL;

	// if necessary, convert to camera frame
	if (xcam) {
		for (i=0; i<4; i++) {
			MultPoints(v[i], cxform, t);
			vector_copy(t, v[i]);
		}
	}

	// convert corners from object space to model space
	ZMatrix xform;
	base->GetObjectToModelTransform(xform);
	for (i=0; i<4; i++) {
		MultPoints(v[i], xform, t);
		vector_copy(t, v[i]);
	}

	// fit plane to corners
	d = fit_plane(v[0], v[1], v[2], v[3], n);

	// project corners to plane
	for (i=0; i<4; i++)
		project(v[i], n, d);

	// create new SfcModel
	SfcModel *sfc = new SfcModel;

	// copy base model-object transform (no rotation)
	sfc->x = base->x;
	sfc->y = base->y;
	sfc->z = base->z;
	sfc->xscale = base->xscale;
	sfc->yscale = base->yscale;
	sfc->zscale = base->zscale;

	// add points to SfcModel
	static NodeSpec ns[4];

	sfc->mesh.pt_list = NULL;
	for (i=0; i<4; i++) {
		ns[i].set_global_center(v[i]);
		NodeSpec_List_Element *nle = new NodeSpec_List_Element;
		nle->nodespec = &ns[i];
		nle->next = sfc->mesh.pt_list;
		sfc->mesh.pt_list = nle;
	}
	sfc->mesh.pt_count = 4;

	// add triangles to SfcModel
	Mesh_Triangle *t1 = new Mesh_Triangle(&ns[2], &ns[1], &ns[0]);
	Mesh_Triangle *t2 = new Mesh_Triangle(&ns[3], &ns[1], &ns[2]);
	sfc->mesh.tri_list = t1;
	t1->next = t2;
	t2->next = NULL;	// actually, constructor does this...

	return sfc;
}
