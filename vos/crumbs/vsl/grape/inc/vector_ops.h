#ifndef	_VECTOR_OPS_H_
#define	_VECTOR_OPS_H_
// vector_ops.h 1.5 02/07/18 12:49:29
/** \file
 ** 3D vector math
 **/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>

/* find length or magnitude of a vector */
double	vector_magnitude(double P1[3]);

/* find square of length or magnitude of a vector */
double  vector_magnitude_sqr(double P1[3]);

/* return malloced normalized version of input vector */
double  *normalized_vector(double P1[3]);

/* return normalized version of input vector in output vector */
void	normalized_vector(double P1[3], double PO[3]);

/* perform inplace vector normalization */
void	normalize_vector(double P1[3]);

/* return dot product of input vectors */
double	dot_product(double P1[3], double P2[3]);

/* return cross product P1 x P2 in PO */
void	cross_product(double P1[3], double P2[3], double PO[3]);

/* return malloced cross product P1 x P2 */
double	*cross_product(double P1[3], double P2[3]);

/* find surface normal of polygon with vertices at P1,P2,P3 */
/* assumes righthand rule in ordering */
void    surface_normal(double P1[3], double P2[3], double P3[3], double PN[3]);

double  *surface_normal(double P1[3], double P2[3], double P3[3]);

/* subtract vector P2 from P1 returning diff in PO */
void	vector_diff(double P1[3], double P2[3], double PO[3]);

/* return malloced difference vector P1 - P2 */
double	*vector_diff(double P1[3], double P2[3]);

/* add vector P2 to P1 returning sum in PO */
void	vector_sum(double P1[3], double P2[3], double PO[3]);

/* return malloced sum vector P1 + P2 */
double	*vector_sum(double P1[3], double P2[3]);

/* scale (multiply) P1 by ratio, returning scaled vector in PO */
void	vector_scale(double P1[3], double ratio, double PO[3]);

/* return malloced vector ratio * P1*/
double	*vector_scale(double P1[3], double ratio);

/* copy vector P1 to PO */
void	vector_copy(double P1[3], double PO[3]);

/* return malloced copy of vector P1 */
double	*vector_dup(double P1[3]);

/* compute normalized vector from P1 to P2, returning in PO */
void	get_vector(double P1[3], double P2[3], double PO[3]);

/* return malloced normalized vector from P1 to P2 */
double	*get_vector(double P1[3], double P2[3]);

/* compute distance between two points */
double	distance(double P1[3], double P2[3]);

/* squared distance */
double	distance_sqr(double P1[3], double P2[3]);

/* compute the vector perpendicular to a polygon (triangle) edge. */
double	*normal_poly_edge_vector(double P1[3], double P2[3], double P3[3], double ratio);

/* computes the coordinates of a point along the
 * vector perpendicular to a polygon (triangle) edge from the midpoint of
 * the edge out a given distance
 */
double *project_point(double P1[3], double P2[3], double P3[3], double dist);

/* Compute the circumcenter point of a triangle in 3-space, 
 * returning the center in PN and the radius as the result. */
double	circumcenter(double P1[3], double P2[3], double P3[3], double PN[3]);

#endif
