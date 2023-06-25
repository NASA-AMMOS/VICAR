// vector_ops.C 1.8 02/07/18 12:49:46
/** \file
// 3D vector math
*/
#include "grape/vector_ops.h"

/// Find length or magnitude of a vector
double	vector_magnitude(double P1[3])
{
	return(sqrt(P1[0]*P1[0] + P1[1]*P1[1] + P1[2]*P1[2]));
}

/// Find square of length or magnitude of a vector
double  vector_magnitude_sqr(double P1[3])
{
	return(P1[0]*P1[0] + P1[1]*P1[1] + P1[2]*P1[2]);
}

/// Return malloced normalized version of input vector
double  *normalized_vector(double P1[3])
{
	double *PO = (double *)malloc(3 * sizeof(double));

	if(!PO) return(NULL);

	double mag = vector_magnitude(P1);
	if(mag > 0.0) {
		PO[0] = P1[0] / mag;
		PO[1] = P1[1] / mag;
		PO[2] = P1[2] / mag;
	}

	return(PO);
}

/// Return normalized version of input vector in output vector
void	normalized_vector(double P1[3], double PO[3])
{
	double mag = vector_magnitude(P1);
	if(mag > 0.0) {
		PO[0] = P1[0] / mag;
		PO[1] = P1[1] / mag;
		PO[2] = P1[2] / mag;
	}
}

/// Perform inplace vector normalization
void	normalize_vector(double P1[3])
{
	double mag = vector_magnitude(P1);
	if(mag > 0.0) {
		P1[0] /= mag;
		P1[1] /= mag;
		P1[2] /= mag;
	}
}

/// Compute dot product of two vectors
double	dot_product(double P1[3], double P2[3])
{
	return(P1[0]*P2[0] + P1[1]*P2[1] + P1[2]*P2[2]);
}

/// Compute cross product of two vectors
void	cross_product(double P1[3], double P2[3], double PO[3])
{
	PO[0] = P1[1]*P2[2] - P1[2]*P2[1];
	PO[1] = P1[2]*P2[0] - P1[0]*P2[2];
	PO[2] = P1[0]*P2[1] - P1[1]*P2[0];
}

/// Compute cross product of two vectors, return allocated results
double	*cross_product(double P1[3], double P2[3])
{
	double *PO = (double *)malloc(3 * sizeof(double));
	if(!PO) return(NULL);
	cross_product(P1, P2, PO);
	return(PO);
}

/// Compute surface normal of plane formed by P1P2 and P1P3
void	surface_normal(double P1[3], double P2[3], double P3[3], double PN[3])
{
	double V12[3], V13[3];
	vector_diff(P2, P1, V12);
	vector_diff(P3, P1, V13);
	cross_product(V12, V13, PN);
	normalize_vector(PN);
}

/// Compute surface normal of plane P1P2,P1P3, return allocated results
double	*surface_normal(double P1[3], double P2[3], double P3[3])
{
	double *PN = (double *)malloc(3 * sizeof(double));
	if(!PN) return(NULL);
	surface_normal(P1, P2, P3, PN);
	return(PN);
}

/// Subtract vector P2 from P1 returning diff in PO
void	vector_diff(double P1[3], double P2[3], double PO[3])
{
	PO[0] = P1[0] - P2[0];
	PO[1] = P1[1] - P2[1];
	PO[2] = P1[2] - P2[2];
}

/// Subtract P2 from P1, return allocated result
double	*vector_diff(double P1[3], double P2[3])
{
	double *PO = (double *)malloc(3 * sizeof(double));

	if(!PO) return(NULL);

	vector_diff(P1, P2, PO);

	return(PO);
}

/// Add vector P2 to P1 returning sum in PO
void	vector_sum(double P1[3], double P2[3], double PO[3])
{
	PO[0] = P1[0] + P2[0];
	PO[1] = P1[1] + P2[1];
	PO[2] = P1[2] + P2[2];
}

/// Add vector P2 to P1, return allocated sum
double	*vector_sum(double P1[3], double P2[3])
{
	double *PO = (double *)malloc(3 * sizeof(double));

	if(!PO) return(NULL);

	vector_sum(P1, P2, PO);

	return(PO);
}

/// Scale (multiply) P1 by ratio, returning scaled vector in PO
void	vector_scale(double P1[3], double ratio, double PO[3])
{
	PO[0] = P1[0] * ratio;
	PO[1] = P1[1] * ratio;
	PO[2] = P1[2] * ratio;
}

/// Scale (multiply) P1 by ratio, returning allocated result
double	*vector_scale(double P1[3], double ratio)
{
	double *PO = (double *)malloc(3 * sizeof(double));

	if(!PO) return(NULL);

	vector_scale(P1, ratio, PO);

	return(PO);
}

/// Copy vector P1 to PO
void	vector_copy(double P1[3], double PO[3])
{
	PO[0] = P1[0];
	PO[1] = P1[1];
	PO[2] = P1[2];
}

/// Return allocated copy of vector P1
double	*vector_dup(double P1[3])
{
	double *PO = (double *)malloc(3 * sizeof(double));

	if(!PO) return(NULL);

	vector_copy(P1, PO);

	return(PO);
}

/// Compute normalized vector from P1 to P2, returning in PO
void	get_vector(double P1[3], double P2[3], double PO[3])
{
	vector_diff(P2, P1, PO);
	normalize_vector(PO);
}

/// Compute normalized vector from P1 to P2, return allocated result
double	*get_vector(double P1[3], double P2[3])
{
	double *PO = (double *)malloc(3 * sizeof(double));

	if(!PO) return(NULL);

	get_vector(P1, P2, PO);

	return(PO);
}

/// Compute the distance between two points
double	distance(double P1[3], double P2[3])
{
	double	PO[3];

	vector_diff(P1, P2, PO);
	return(vector_magnitude(PO));
}

/// Compute the squared distance between two points
double	distance_sqr(double P1[3], double P2[3])
{
	double	PO[3];

	vector_diff(P1, P2, PO);
	return(vector_magnitude_sqr(PO));
}

/// Compute the vector perpendicular to a polygon (triangle) edge.  
/**
  Given points P1, P2, and P3 in 3-space, the
  The normal_poly_edge_vector function computes the vector perpendicular to
  a polygon (triangle) edge.  Given points P1, P2, and P3 in 3-space, the
  vector PO is computed which is perpendicular to edge P1P2 and in the plane
  of the triangle P1P2P3.  The vector is normalized, then multiplied by the
  input parameter ratio.  If ratio is > 0 then the vector points out from 
  the triangle.  If ratio is < 0, the vector points into the triangle.
  Absolute value of ratio == 1.0 preserves the vector normalization.
  The output vector, PO, is malloced here and should be freed by the calling
  routine when done.
*/
double	*normal_poly_edge_vector(double P1[3], double P2[3], double P3[3], double ratio)
{

	double x1=P1[0], y1=P1[1], z1=P1[2];
	double x2=P2[0], y2=P2[1], z2=P2[2];
	double x3=P3[0], y3=P3[1], z3=P3[2];
	double x1p, y1p, z1p, x2p, y2p, z2p, x3p, y3p, z3p;
	double dot, mag12, mag13, mag3p;

	/* translate to origin */
	x3p = x3 - x1;
	y3p = y3 - y1;
	z3p = z3 - z1;
	x2p = x2 - x1;
	y2p = y2 - y1;
	z2p = z2 - z1;

	/* normalize */
	mag12 = sqrt(x2p*x2p + y2p*y2p + z2p*z2p);
	if(mag12 <= 0.0) 	/* Points P1 and P2 are colocated so error */
		return(NULL);

	mag13 = sqrt(x3p*x3p + y3p*y3p + z3p*z3p);
	if(mag13 <= 0.0) 	/* Points P1 and P3 are colocated so error */
		return(NULL);

	x2p /= mag12;
	y2p /= mag12;
	z2p /= mag12;
	x3p /= mag13;
	y3p /= mag13;
	z3p /= mag13;

	/* compute dot product */
	dot = x2p*x3p + y2p*y3p + z2p*z3p;
	/* note that dot product is cosine of inner angle at P1 */

	/* compute nearest point to P3 along vector P1P2 */
	dot *= mag13/mag12;
	x1p = dot*x2 + (1.0-dot)*x1;
	y1p = dot*y2 + (1.0-dot)*y1;
	z1p = dot*z2 + (1.0-dot)*z1;

	/* compute distance from P3 to line throught P1P2 */
	mag3p = sqrt((x1p-x3)*(x1p-x3) + (y1p-y3)*(y1p-y3) + (z1p-z3)*(z1p-z3));
	if(mag3p<= 0.0)       /* Points P1, P@, and P3 are colinear so error */
		return(NULL);

	/* compute projected point */
	double *PO = (double *)malloc(3 * sizeof(double));
	if(!PO) return(NULL);
	PO[0] = (x1p-x3)*ratio/mag3p;
	PO[1] = (y1p-y3)*ratio/mag3p;
	PO[2] = (z1p-z3)*ratio/mag3p;

	return(PO);
}

/// Project along the vector perpendicular to a polygon (triangle) edge 
/**
  Computes the coordinates of a point along the
  vector perpendicular to a polygon (triangle) edge from the midpoint of
  the edge out a given distance.  Given points P1, P2, and P3 in 3-space, the
  vector PO is computed which is perpendicular to edge P1P2 and in the plane
  of the triangle P1P2P3.  The vector is multiplied by the distance, then
  added to the midpoint of P1P2. If distance (dist) > 0.0, then the point is
  guaranteed to be outside the triangle.  If distance < 0.0, then the point
  may be inside the triangle or on the other side.  The output popint, PO,
  is malloced here and should be freed by the calling routine.
*/
double *project_point(double P1[3], double P2[3], double P3[3], double dist)
{
	double *vctr = normal_poly_edge_vector(P1, P2, P3, 1.0);

	if(!vctr)
		return(NULL);

	/* compute projected point */
	vctr[0] = vctr[0]*dist + (P1[0] + P2[0])/2.0;
	vctr[1] = vctr[1]*dist + (P1[1] + P2[1])/2.0;
	vctr[2] = vctr[2]*dist + (P1[2] + P2[2])/2.0;

	return(vctr);
}

/// Compute the circumcenter point of a triangle in 3-space
/**
// Returns the center in PN and the radius as the result.
*/
double	circumcenter(double P1[3], double P2[3], double P3[3], double PN[3])
{
	/* Uses the theorem a/sin_alpha = b/sin_beta = c/sin_gamma = 2R */

	double	radius, cos_theta;
	double  d, *pt, d12, d13, d23;

	/* make sure edge from P1 to P2 is not longest */
	d12 = distance_sqr(P1, P2);
	d13 = distance_sqr(P1, P3);
	d23 = distance_sqr(P2, P3);
	if(d12 > d13 && d12 > d23) {
		pt = P2;	// swap 2 and 3
		P2 = P3;
		P3 = pt;
		d12 = d13;
	}

	double V12[3], V13[3];
	get_vector(P1,P2, V12);
	get_vector(P1,P3, V13);
	cos_theta = dot_product(V12, V13);
	if(cos_theta > 0.999999 || cos_theta < -0.999999) {
		// points are collinear so return -1
		return(-1.0);
	}

	radius = sqrt(d23) / (2.0 * sin(acos(cos_theta)));
	d = sqrt(radius*radius - d12/4.0);
	
	pt = project_point(P1, P2, P3, -d);
	if (pt == NULL)
		return -1.0;

	PN[0] = pt[0];
	PN[1] = pt[1];
	PN[2] = pt[2];

	free(pt);

	return(radius);
}
