/******************************************************************************
*                                                                             *
*                                     M A T 3                                 *
*                                                                             *
*                                       Todd Litwin                           *
*                                       Written: 12 Sep 1991                  *
*                                       Updated:  5 May 2015                  *
*                                                                             *
*******************************************************************************


	This file has miscellaneous mathematic functions to operate on
	3-dimensional double-precision vectors, matrices, and quaternions.
	The default data type can be changed from double to something else
	by using the MAT3_FLOAT preprocessor directive in mat3.h.

	NOTE ON CONVENTIONS: Vectors, when multiplied by matrices, are
	understood to be column vectors. Matrices are row-major, with
	premultiplication indicated for rotation. The components of
	quaternions are organized so that the scalar (cosine) term leads,
	and the vector (sine) terms trail.

	*/


#include <math.h>

#include "mat3.h"

#ifndef NULL
#define NULL (0)
#endif

#ifndef MAT3_EPSILON
#define MAT3_EPSILON (1e-7)
#endif

#ifdef  MAT3_INTEGER
typedef MAT3_INTEGER int3;
#else
typedef int          int3;
#endif


/******************************************************************************
********************************   ADD3   *************************************
*******************************************************************************

    This function adds two 3-vectors. Returns pointer to output vector. */

float3 *add3(
    const float3 a[3],	/* input addend vector */
    const float3 b[3],	/* input addend vector */
    float3 c[3])	/* output sum vector */
{
    /* Check for NULL inputs */
    if ((a == NULL) || (b == NULL) || (c == NULL)) {
	zero3(c);
	return NULL;
	}

    /* Add the two vectors */
    c[0] = a[0] + b[0];
    c[1] = a[1] + b[1];
    c[2] = a[2] + b[2];

    /* Return a pointer to the result */
    return c;
    }


/******************************************************************************
********************************   ADD33   ************************************
*******************************************************************************

    This function adds together two 3x3 matrices. Returns pointer to output
    matrix. */

float3 (*add33(
    float3 a[3][3],	/* input addend matrix */
    float3 b[3][3],	/* input addend matrix */
    float3 c[3][3]	/* output sum matrix */
    ))[3]
{
    /* Check for NULL inputs */
    if ((a == NULL) || (b == NULL) || (c == NULL)) {
	zero33(c);
	return NULL;
	}

    /* Calculate the sum */
    c[0][0] = a[0][0] + b[0][0];
    c[0][1] = a[0][1] + b[0][1];
    c[0][2] = a[0][2] + b[0][2];
    c[1][0] = a[1][0] + b[1][0];
    c[1][1] = a[1][1] + b[1][1];
    c[1][2] = a[1][2] + b[1][2];
    c[2][0] = a[2][0] + b[2][0];
    c[2][1] = a[2][1] + b[2][1];
    c[2][2] = a[2][2] + b[2][2];

    /* Return a pointer to the result */
    return c;
    }


/******************************************************************************
********************************   COPY3   ************************************
*******************************************************************************

    This function copies a 3-vector. Returns pointer to output vector. */

float3 *copy3(
    const float3 a[3],	/* input vector */
    float3 b[3])	/* output vector */
{
    /* Check for NULL inputs */
    if ((a == NULL) || (b == NULL)) {
	zero3(b);
	return NULL;
	}

    /* Copy the two vectors */
    b[0] = a[0];
    b[1] = a[1];
    b[2] = a[2];

    /* Return a pointer to the result */
    return b;
    }


/******************************************************************************
********************************   COPY33   ***********************************
*******************************************************************************

    This function copies a 3x3 matrix. Returns pointer to output matrix. */

float3 (*copy33(
    float3 a[3][3],	/* input matrix */
    float3 b[3][3]	/* output matrix */
    ))[3]
{
    /* Check for NULL inputs */
    if ((a == NULL) || (b == NULL)) {
	zero33(b);
	return NULL;
	}

    /* Copy all of the elements */
    b[0][0] = a[0][0];
    b[0][1] = a[0][1];
    b[0][2] = a[0][2];
    b[1][0] = a[1][0];
    b[1][1] = a[1][1];
    b[1][2] = a[1][2];
    b[2][0] = a[2][0];
    b[2][1] = a[2][1];
    b[2][2] = a[2][2];

    /* Return a pointer to the result */
    return b;
    }


/******************************************************************************
********************************   COPYQ   ************************************
*******************************************************************************

    This function copies a quaternion (a 4-vector). Returns pointer to output
    quaternion. */

float3 *copyq(
    const float3 a[4],	/* input quaternion */
    float3 b[4])	/* output quaternion */
{
    /* Check for NULL inputs */
    if ((a == NULL) || (b == NULL)) {
	zeroq(b);
	return NULL;
	}

    /* Copy the two vectors */
    b[0] = a[0];
    b[1] = a[1];
    b[2] = a[2];
    b[3] = a[3];

    /* Return a pointer to the result */
    return b;
    }


/******************************************************************************
********************************   CROSS3   ***********************************
*******************************************************************************

    This function performs the cross product of two 3-vectors. Returns pointer
    to output vector. */

float3 *cross3(
    const float3 a[3],	/* input vector */
    const float3 b[3],	/* input vector */
    float3 c[3])	/* output vector */
{
    float3 d[3];

    /* Check for NULL inputs */
    if ((a == NULL) || (b == NULL) || (c == NULL)) {
	zero3(c);
	return NULL;
	}

    /* Perform the cross product */
    d[0]  =  a[1] * b[2]  -  a[2] * b[1];
    d[1]  =  a[2] * b[0]  -  a[0] * b[2];
    d[2]  =  a[0] * b[1]  -  a[1] * b[0];

    /* Return a pointer to the result */
    c[0] = d[0];
    c[1] = d[1];
    c[2] = d[2];
    return c;
    }


/******************************************************************************
********************************   DET33   ************************************
*******************************************************************************

    This function computes the determinant of a 3x3 matrix. */

float3 det33(
    float3 a[3][3])	/* input matrix */
{
    float3 d;

    /* Check for NULL inputs */
    if (a == NULL) {
	return 0.0;
	}

    /* Calculate the determinant */
    d = a[0][0] * (a[1][1] * a[2][2]  -  a[1][2] * a[2][1]) -
	a[0][1] * (a[1][0] * a[2][2]  -  a[1][2] * a[2][0]) +
	a[0][2] * (a[1][0] * a[2][1]  -  a[1][1] * a[2][0]);

    /* Return determinant */
    return d;
    }


/******************************************************************************
********************************   DOT3   *************************************
*******************************************************************************

    This function computes the inner product of two 3-vectors. */

float3 dot3(
    const float3 a[3],	/* input vector */
    const float3 b[3])	/* input vector */
{
    float3 f;

    /* Check for NULL inputs */
    if ((a == NULL) || (b == NULL)) {
	return 0.0;
	}

    /* Dot the two vectors */
    f = a[0] * b[0] +
	a[1] * b[1] +
	a[2] * b[2];

    /* Return the dot product */
    return f;
    }


/******************************************************************************
********************************   DROTVA   ***********************************
*******************************************************************************

    This function computes the derivative of the rotation matrix that could
    be constructed from the given vector and angle of rotation about that
    vector. It is built for pre-multiplication. See quatva() and rotq().
    Returns pointer to output matrix. */

float3 (*drotva(
    const float3 v[3],	/* input vector */
    float3 a,		/* input angle of rotation */
    float3 drda[3][3]	/* output derivative matrix */
    ))[3]
{
    float3 c;
    float3 mag;
    float3 s;
    float3 u0;
    float3 u1;
    float3 u2;

    /* Check for NULL inputs */
    if ((v == NULL) || (drda == NULL)) {
	zero33(drda);
	return NULL;
	}

    /* Precompute some needed quantities */
    mag = sqrt(v[0] * v[0]  +  v[1] * v[1]  +  v[2] * v[2]);
    if (mag < MAT3_EPSILON) {
	zero33(drda);
	return NULL;
	}
    u0 = v[0] / mag;
    u1 = v[1] / mag;
    u2 = v[2] / mag;
    c = cos(a/2);
    s = sin(a/2);

    /* Compute the derivative (see Mathematica notes) */
    drda[0][0] = -(c*s) + u0*u0*c*s - u1*u1*c*s - u2*u2*c*s;
    drda[0][1] = 2*(-(u2*c*c)/2 + u0*u1*c*s + (u2*s*s)/2);
    drda[0][2] = 2*((u1*c*c)/2 + u0*u2*c*s - (u1*s*s)/2);
    drda[1][0] = 2*((u2*c*c)/2 + u0*u1*c*s - (u2*s*s)/2);
    drda[1][1] = -(c*s) - u0*u0*c*s + u1*u1*c*s - u2*u2*c*s;
    drda[1][2] = 2*(-(u0*c*c)/2 + u1*u2*c*s + (u0*s*s)/2);
    drda[2][0] = 2*(-(u1*c*c)/2 + u0*u2*c*s + (u1*s*s)/2);
    drda[2][1] = 2*((u0*c*c)/2 + u1*u2*c*s - (u0*s*s)/2);
    drda[2][2] = -(c*s) - u0*u0*c*s - u1*u1*c*s + u2*u2*c*s;

    /* Return a pointer to the result */
    return drda;
    }


/******************************************************************************
********************************   IDENT33   **********************************
*******************************************************************************

    This function creates a 3x3 identity matrix. Returns pointer to output
    matrix. */

float3 (*ident33(
    float3 a[3][3]	/* output matrix */
    ))[3]
{
    /* Check for NULL inputs */
    if (a == NULL) {
	return NULL;
	}

    /* Define the elements of the identity matrix */
    a[0][0] = 1;
    a[0][1] = 0;
    a[0][2] = 0;
    a[1][0] = 0;
    a[1][1] = 1;
    a[1][2] = 0;
    a[2][0] = 0;
    a[2][1] = 0;
    a[2][2] = 1;

    /* Return a pointer to the result */
    return a;
    }


/******************************************************************************
********************************   IDENTQ   ***********************************
*******************************************************************************

    This function creates an identity quaternion. Returns pointer to output
    quaternion. */

float3 *identq(
    float3 a[4])	/* output quaternion */
{
    /* Check for NULL input */
    if (a == NULL) {
	return NULL;
	}

    /* Define the elements of the identity quaternion */
    a[0] = 1;
    a[1] = 0;
    a[2] = 0;
    a[3] = 0;

    /* Return a pointer to the result */
    return a;
    }


/******************************************************************************
********************************   INV33   ************************************
*******************************************************************************

    This function inverts any invertible 3x3 matrix. Returns pointer to output
    matrix. */

float3 (*inv33(
    float3 a[3][3],	/* input matrix */
    float3 b[3][3]	/* output matrix */
    ))[3]
{
    float3 a2[3][3];
    float3 det;

    /* Check for NULL inputs */
    if ((a == NULL) || (b == NULL)) {
	zero33(b);
	return NULL;
	}

    /* Check for non-distinct output */
    if (a == b) {
	copy33(a, a2);
	a = a2;
	}

    /* Compute the determinant */
    det = a[0][0] * (a[1][1] * a[2][2] - a[2][1] * a[1][2])
	- a[1][0] * (a[0][1] * a[2][2] - a[2][1] * a[0][2])
	+ a[2][0] * (a[0][1] * a[1][2] - a[1][1] * a[0][2]);
    if ((det < MAT3_EPSILON) && (det > -MAT3_EPSILON)) {
	zero33(b);
	return NULL;
	}

    b[0][0] =  (a[1][1] * a[2][2] - a[2][1] * a[1][2]) / det;
    b[0][1] = -(a[0][1] * a[2][2] - a[2][1] * a[0][2]) / det;
    b[0][2] =  (a[0][1] * a[1][2] - a[1][1] * a[0][2]) / det;

    b[1][0] = -(a[1][0] * a[2][2] - a[2][0] * a[1][2]) / det;
    b[1][1] =  (a[0][0] * a[2][2] - a[2][0] * a[0][2]) / det;
    b[1][2] = -(a[0][0] * a[1][2] - a[1][0] * a[0][2]) / det;

    b[2][0] =  (a[1][0] * a[2][1] - a[2][0] * a[1][1]) / det;
    b[2][1] = -(a[0][0] * a[2][1] - a[2][0] * a[0][1]) / det;
    b[2][2] =  (a[0][0] * a[1][1] - a[1][0] * a[0][1]) / det;

    /* Return a pointer to the result */
    return b;
    }


/******************************************************************************
********************************   INV33PD   **********************************
*******************************************************************************

    This function inverts a positive-definite 3x3 matrix. Since the function
    inv33() is faster and is not restricted to positive-definite matrices,
    it is recommended over this one. The only reason for putting inv33pd()
    here is as an example of how to invert larger matrices. The recursive
    method used by inv33() does not scale nicely, and quickly becomes too
    slow. But this function scales up quite well. Returns pointer to output
    matrix. */

float3 (*inv33pd(
    float3 a[3][3],	/* input matrix */
    float3 b[3][3]	/* output matrix */
    ))[3]
{
    float3 f;
    int3 i;
    int3 j;
    int3 k;

    /* Check for NULL inputs */
    if ((a == NULL) || (b == NULL)) {
	zero33(b);
	return NULL;
	}

    /* Start with a copy of the input in the output */
    if (a != b) {
	b[0][0] = a[0][0];
	b[0][1] = a[0][1];
	b[0][2] = a[0][2];
	b[1][0] = a[1][0];
	b[1][1] = a[1][1];
	b[1][2] = a[1][2];
	b[2][0] = a[2][0];
	b[2][1] = a[2][1];
	b[2][2] = a[2][2];
	}

    /* Perform the matrix inversion */
    for (i=0; i<3; i++) {
	if ((b[i][i] < MAT3_EPSILON) && (b[i][i] > -MAT3_EPSILON)) {
	    zero33(b);
	    return NULL;
	    }
	f = 1.0 / b[i][i];
	b[i][i] = 1.0;
	for (j=0; j<3; j++) {
	    b[i][j] *= f;
	    }
	for (k=0; k<3; k++) {
	    if (i != k) {
		f = b[k][i];
		b[k][i] = 0.0;
		for (j=0; j<3; j++) {
                    b[k][j] = b[k][j]  -  f * b[i][j];
		    }
		}
	    }
	}

    /* Return a pointer to the result */
    return b;
    }


/******************************************************************************
********************************   INVQ   *************************************
*******************************************************************************

    This function inverts a quaternion. It assumes that quaternion is
    normalized. [See unitq() to normalize.] Returns pointer to output
    quaternion. */

float3 *invq(
    const float3 a[4],	/* input quaternion */
    float3 b[4])	/* output quaternion */
{
    /* Check for NULL inputs */
    if ((a == NULL) || (b == NULL)) {
	zeroq(b);
	return NULL;
	}

    /* Invert */
    b[0] =  a[0];
    b[1] = -a[1];
    b[2] = -a[2];
    b[3] = -a[3];

    /* Return a pointer to the result */
    return b;
    }


/******************************************************************************
********************************   MAG3   *************************************
*******************************************************************************

    This function computes the magnitude of a 3-vector. */

float3 mag3(
    const float3 a[3])	/* input vector */
{
    float3 mag;

    /* Check for NULL inputs */
    if (a == NULL) {
	return 0.0;
	}

    /* Calculate the magnitude */
    mag = sqrt(a[0] * a[0]  +  a[1] * a[1]  +  a[2] * a[2]);

    /* Return the result */
    return mag;
    }


/******************************************************************************
********************************   MULT133   **********************************
*******************************************************************************

    This function multiplies a 3-vector (1x3 matrix) by a 3x3 matrix. Returns
    pointer to output matrix. */

float3 *mult133(
    const float3 a[3],	/* input vector */
    float3 b[3][3],	/* input matrix */
    float3 c[3])	/* output vector */
{
    float3 a2[3];
    float3 b2[3][3];

    /* Check for NULL inputs */
    if ((a == NULL) || (b == NULL) || (c == NULL)) {
	zero3(c);
	return NULL;
	}

    /* Check for non-distinct output */
    if (a == c) {
	copy3(a, a2);
	a = a2;
	}
    if (b[0] == c) {
	copy33(b, b2);
	b = b2;
	}

    /* Perform the matrix multiply */
    c[0] = a[0] * b[0][0]  +  a[1] * b[1][0]  +  a[2] * b[2][0];
    c[1] = a[0] * b[0][1]  +  a[1] * b[1][1]  +  a[2] * b[2][1];
    c[2] = a[0] * b[0][2]  +  a[1] * b[1][2]  +  a[2] * b[2][2];

    /* Return a pointer to the result */
    return c;
    }


/******************************************************************************
********************************   MULT313   **********************************
*******************************************************************************

    This function multiplies a 3-vector (3x1 matrix) by a 3-vector
    (1x3 matrix). Returns pointer to output matrix. */

float3 (*mult313(
    const float3 a[3],	/* input vector */
    const float3 b[3],	/* input vector */
    float3 c[3][3]	/* output matrix */
    ))[3]
{
    float3 a2[3];
    float3 b2[3];

    /* Check for NULL inputs */
    if ((a == NULL) || (b == NULL) || (c == NULL)) {
	zero33(c);
	return NULL;
	}

    /* Check for non-distinct output */
    if (a == c[0]) {
	copy3(a, a2);
	a = a2;
	}
    if (b == c[0]) {
	copy3(b, b2);
	b = b2;
	}

    /* Perform the matrix multiply */
    c[0][0] = a[0] * b[0];
    c[0][1] = a[0] * b[1];
    c[0][2] = a[0] * b[2];
    c[1][0] = a[1] * b[0];
    c[1][1] = a[1] * b[1];
    c[1][2] = a[1] * b[2];
    c[2][0] = a[2] * b[0];
    c[2][1] = a[2] * b[1];
    c[2][2] = a[2] * b[2];

    /* Return a pointer to the result */
    return c;
    }


/******************************************************************************
********************************   MULT331   **********************************
*******************************************************************************

    This function multiplies a 3x3 matrix by a 3-vector (3x1 matrix). Returns
    pointer to output vector. */

float3 *mult331(
    float3 m[3][3],	/* input matrix */
    const float3 v[3],	/* input vector */
    float3 u[3])	/* output vector */
{
    float3 v2[3];

    /* Check for NULL inputs */
    if ((m == NULL) || (v == NULL) || (u == NULL)) {
	zero3(u);
	return NULL;
	}

    /* Check for non-distinct output */
    if (v == u) {
	copy3(v, v2);
	v = v2;
	}

    /* Perform the matrix multiply */
    u[0] = m[0][0] * v[0]  +  m[0][1] * v[1]  +  m[0][2] * v[2];
    u[1] = m[1][0] * v[0]  +  m[1][1] * v[1]  +  m[1][2] * v[2];
    u[2] = m[2][0] * v[0]  +  m[2][1] * v[1]  +  m[2][2] * v[2];

    /* Return a pointer to the result */
    return u;
    }


/******************************************************************************
********************************   MULT333   **********************************
*******************************************************************************

    This function multiplies two 3x3 matrices. Returns pointer to output
    matrix. */

float3 (*mult333(
    float3 a[3][3],	/* input matrix */
    float3 b[3][3],	/* input matrix */
    float3 c[3][3]	/* output matrix */
    ))[3]
{
    float3 a2[3][3];
    float3 b2[3][3];

    /* Check for NULL inputs */
    if ((a == NULL) || (b == NULL) || (c == NULL)) {
	zero33(c);
	return NULL;
	}

    /* Check for non-distinct output */
    if (a == c) {
	copy33(a, a2);
	a = a2;
	}
    if (b == c) {
	copy33(b, b2);
	b = b2;
	}

    /* Perform the matrix multiply */
    c[0][0] = a[0][0] * b[0][0]  +  a[0][1] * b[1][0]  +  a[0][2] * b[2][0];
    c[0][1] = a[0][0] * b[0][1]  +  a[0][1] * b[1][1]  +  a[0][2] * b[2][1];
    c[0][2] = a[0][0] * b[0][2]  +  a[0][1] * b[1][2]  +  a[0][2] * b[2][2];
    c[1][0] = a[1][0] * b[0][0]  +  a[1][1] * b[1][0]  +  a[1][2] * b[2][0];
    c[1][1] = a[1][0] * b[0][1]  +  a[1][1] * b[1][1]  +  a[1][2] * b[2][1];
    c[1][2] = a[1][0] * b[0][2]  +  a[1][1] * b[1][2]  +  a[1][2] * b[2][2];
    c[2][0] = a[2][0] * b[0][0]  +  a[2][1] * b[1][0]  +  a[2][2] * b[2][0];
    c[2][1] = a[2][0] * b[0][1]  +  a[2][1] * b[1][1]  +  a[2][2] * b[2][1];
    c[2][2] = a[2][0] * b[0][2]  +  a[2][1] * b[1][2]  +  a[2][2] * b[2][2];

    /* Return a pointer to the result */
    return c;
    }


/******************************************************************************
********************************   MULTQ   ************************************
*******************************************************************************

    This function multiplies two quaternions together. Returns pointer to
    output quaternion. */

float3 *multq(
    const float3 a[4],	/* input rotation quaternion */
    const float3 b[4],	/* input orientation quaternion */
    float3 c[4])	/* output orientation quaternion */
{
    float3 a0;
    float3 a1;
    float3 a2;
    float3 a3;
    float3 b0;
    float3 b1;
    float3 b2;
    float3 b3;

    /* Check for NULL inputs */
    if ((a == NULL) || (b == NULL) || (c == NULL)) {
	zeroq(c);
	return NULL;
	}

    /* Perform the multiplication */
    a0 = a[0];
    a1 = a[1];
    a2 = a[2];
    a3 = a[3];
    b0 = b[0];
    b1 = b[1];
    b2 = b[2];
    b3 = b[3];
    c[0] = a0 * b0 - a1 * b1 - a2 * b2 - a3 * b3;
    c[1] = a0 * b1 + a1 * b0 + a2 * b3 - a3 * b2;
    c[2] = a0 * b2 + a2 * b0 + a3 * b1 - a1 * b3;
    c[3] = a0 * b3 + a3 * b0 + a1 * b2 - a2 * b1;

    /* Return a pointer to the result */
    return c;
    }


/******************************************************************************
********************************   MULTQV   ***********************************
*******************************************************************************

    This function rotates a vector by a quaternion. To do so it premultiplies
    by the quaternion and postmultiplies by the inverse of the quaternion.
    The calculation used was derived by the author from first principles (see
    Wikipedia on Quaternions), and so is suspect. Returns pointer to output
    vector. */

float3 *multqv(
    const float3 q[4],	/* input rotation quaternion */
    const float3 v[3],	/* input vector */
    float3 u[3])	/* output vector */
{
    float3 q0;
    float3 q1;
    float3 q2;
    float3 q3;
    float3 q0q0;
    float3 q0q1;
    float3 q0q2;
    float3 q0q3;
    float3 q1q1;
    float3 q1q2;
    float3 q1q3;
    float3 q2q2;
    float3 q2q3;
    float3 q3q3;

    /* Check for NULL inputs */
    if ((q == NULL) || (v == NULL) || (u == NULL)) {
	zero3(u);
	return NULL;
	}

    /* Perform the multiplication */
    q0 = q[0];
    q1 = q[1];
    q2 = q[2];
    q3 = q[3];
    q0q0 = q0 * q0;
    q0q1 = q0 * q1;
    q0q2 = q0 * q2;
    q0q3 = q0 * q3;
    q1q1 = q1 * q1;
    q1q2 = q1 * q2;
    q1q3 = q1 * q3;
    q2q2 = q2 * q2;
    q2q3 = q2 * q3;
    q3q3 = q3 * q3;
    u[0] = v[0]*(q0q0+q1q1-q2q2-q3q3) + 2*v[1]*(q1q2-q0q3) + 2*v[2]*(q0q2+q1q3);
    u[1] = 2*v[0]*(q0q3+q1q2) + v[1]*(q0q0-q1q1+q2q2-q3q3) + 2*v[2]*(q2q3-q0q1);
    u[2] = 2*v[0]*(q1q3-q0q2) + 2*v[1]*(q0q1+q2q3) + v[2]*(q0q0-q1q1-q2q2+q3q3);

    /* Return a pointer to the result */
    return u;
    }


/******************************************************************************
********************************   QUATR   ************************************
*******************************************************************************

    This function converts a rotation matrix to an equivalent unit quaternion
    which represents rotation. Returns pointer to output quaternion. */

float3 *quatr(
    float3 r[3][3],	/* input rotation matrix */
    float3 q[4])	/* output quaternion */
{
    float3 b;
    float3 den;
    float3 fourq0;
    int3 i;
    int3 j;
    int3 k;
    float3 s;
    float3 t;
    float3 ui;
    float3 uj;
    float3 uk;
    float3 vj;
    float3 vk;

    /* Check for NULL inputs */
    if ((q == NULL) || (r == NULL)) {
	zeroq(q);
	return NULL;
	}

    /* Perform the conversion */
    t = r[0][0] + r[1][1] + r[2][2];
    if (t >= 1.0) {
	fourq0 = 2.0 * sqrt(1.0 + t);
	q[0] = 0.25 * fourq0;
	q[1] = (r[2][1] - r[1][2]) / fourq0;
	q[2] = (r[0][2] - r[2][0]) / fourq0;
	q[3] = (r[1][0] - r[0][1]) / fourq0;
	}
    else {
	i = 0;	/* we need to point i to largest diagonal element */
	s = r[0][0];
	if (r[1][1] > s) {
	    i = 1;
	    s = r[1][1];
	    }
	if (r[2][2] > s) {
	    i = 2;
	    }
	j = (i + 1) % 3;
	k = (j + 1) % 3;
	s = 0.5 * sqrt(3.0 - t);
	den = ((1.0 - r[k][k]) * (1.0 - r[j][j]) - r[j][k] * r[k][j]);
	if ((den < MAT3_EPSILON) && (den > -MAT3_EPSILON)) {
	    zeroq(q);
	    return NULL;
	    }
	den =  1.0 / den;
	vj = ((1.0 - r[k][k]) * r[j][i] + r[j][k] * r[k][i]) * den;
	vk = ((1.0 - r[j][j]) * r[k][i] + r[k][j] * r[j][i]) * den;
	ui = 1.0 / sqrt(1.0 + vj * vj + vk * vk);
	uj = vj * ui;
	uk = vk * ui;
	b = r[k][j] - r[j][k];
	if (b < 0.0) {
	    s = -s;
	    }
	den = (4.0 * s * ui);
	if ((den < MAT3_EPSILON) && (den > -MAT3_EPSILON)) {
	    zeroq(q);
	    return NULL;
	    }
	q[0]   = b / den;
	q[i+1] = s * ui;
	q[j+1] = s * uj;
	q[k+1] = s * uk;
	}

    /* Return a pointer to the result */
    return q;
    }


/******************************************************************************
********************************   QUATVA   ***********************************
*******************************************************************************

    This function converts a vector and an angle of rotation about that vector
    to an equivalent unit quaternion which represents rotation of an object
    in a fixed coordinate system. It is constructed for pre-multiplication.
    Returns pointer to output quaternion. */

float3 *quatva(
    const float3 v[3],	/* input vector */
    float3 a,		/* input angle of rotation */
    float3 q[4])	/* output quaternion */
{
    float3 c;
    float3 s;
    float3 vmag;

    /* Check for NULL inputs */
    if ((q == NULL) || (v == NULL)) {
	zeroq(q);
	return NULL;
	}

    /* Precompute some needed quantities */
    vmag = sqrt(v[0] * v[0]  +  v[1] * v[1]  +  v[2] * v[2]);
    if (vmag < MAT3_EPSILON) {
	zeroq(q);
	return NULL;
	}
    c = cos(a/2);
    s = sin(a/2);

    /* Construct the quaternion */
    q[0] = c;
    q[1] = s * v[0] / vmag;
    q[2] = s * v[1] / vmag;
    q[3] = s * v[2] / vmag;

    /* Return a pointer to the result */
    return q;
    }


/******************************************************************************
********************************   QUATXYZ   **********************************
*******************************************************************************

    This function converts three axis rotations into a quaternion. The
    rotations are the familiar roll, pitch, and yaw (heading), performed on
    an object about the static world coordinates. This is the inverse of the
    Euler angles about the Z axis, followed by the rotated Y axis, followed
    by the twice-rotated X axis. Returns pointer to output quaternion. */

float3 *quatxyz(
    float3 a,		/* input angle about X axis (roll) */
    float3 b,		/* input angle about Y axis (pitch) */
    float3 c,		/* input angle about Z axis (yaw) */
    float3 q[4])	/* output quaternion */
{
    float3 a0;
    float3 a1;
    float3 b0;
    float3 b2;
    float3 c0;
    float3 c3;

    /* Check for NULL inputs */
    if (q == NULL) {
	return NULL;
	}

    /* Precompute non-zero quaternion components for individual rotations */
    a0 = cos(a/2);
    a1 = sin(a/2);
    b0 = cos(b/2);
    b2 = sin(b/2);
    c0 = cos(c/2);
    c3 = sin(c/2);

    /* Compute rotation matrix: q = qc*qb*qa */
    q[0] =  c0 * b0 * a0  +  c3 * b2 * a1;
    q[1] =  c0 * b0 * a1  -  c3 * b2 * a0;
    q[2] =  c0 * b2 * a0  +  c3 * b0 * a1;
    q[3] = -c0 * b2 * a1  +  c3 * b0 * a0;

    /* Return a pointer to the result */
    return q;
    }


/******************************************************************************
********************************   ROTQ   *************************************
*******************************************************************************

    This function converts a unit quaternion which represents rotation to
    an equivalent rotation matrix. Returns pointer to output matrix. */

float3 (*rotq(
    const float3 q[4],	/* input quaternion */
    float3 r[3][3]	/* output rotation matrix */
    ))[3]
{
    float3 q0;
    float3 q1;
    float3 q2;
    float3 q3;
    float3 q0q0;
    float3 q0q1;
    float3 q0q2;
    float3 q0q3;
    float3 q1q1;
    float3 q1q2;
    float3 q1q3;
    float3 q2q2;
    float3 q2q3;
    float3 q3q3;

    /* Check for NULL inputs */
    if ((q == NULL) || (r == NULL)) {
	zero33(r);
	return NULL;
	}

    /* Perform the conversion */
    q0 = q[0];
    q1 = q[1];
    q2 = q[2];
    q3 = q[3];
    q0q0 = q0 * q0;
    q0q1 = q0 * q1;
    q0q2 = q0 * q2;
    q0q3 = q0 * q3;
    q1q1 = q1 * q1;
    q1q2 = q1 * q2;
    q1q3 = q1 * q3;
    q2q2 = q2 * q2;
    q2q3 = q2 * q3;
    q3q3 = q3 * q3;
    r[0][0] = q0q0 + q1q1 - q2q2 - q3q3;
    r[0][1] = 2.0 * (q1q2 - q0q3);
    r[0][2] = 2.0 * (q1q3 + q0q2);
    r[1][0] = 2.0 * (q1q2 + q0q3);
    r[1][1] = q0q0 + q2q2 - q1q1 - q3q3;
    r[1][2] = 2.0 * (q2q3 - q0q1);
    r[2][0] = 2.0 * (q1q3 - q0q2);
    r[2][1] = 2.0 * (q2q3 + q0q1);
    r[2][2] = q0q0 + q3q3 - q1q1 - q2q2;

    /* Return a pointer to the result */
    return r;
    }


/******************************************************************************
********************************   ROTXYZ   ***********************************
*******************************************************************************

    This function converts three axis rotations into a rotation matrix. The
    rotations are the familiar roll, pitch, and yaw (heading), performed on
    an object about the static world coordinates. This is the inverse of the
    Euler angles about the Z axis, followed by the rotated Y axis, followed
    by the twice-rotated X axis. This function was constructed from the
    transpose, which is the inverse in this case, of the equations found
    in Herbert Goldstein, "Classical Mechanics," second edition, p. 609,
    called there the "xyz convention." Returns pointer to output matrix. */

float3 (*rotxyz(
    float3 a,		/* input angle about X axis (roll) */
    float3 b,		/* input angle about Y axis (pitch) */
    float3 c,		/* input angle about Z axis (yaw) */
    float3 r[3][3]	/* output rotation matrix */
    ))[3]
{
    float3 cos_phi;
    float3 cos_psi;
    float3 cos_theta;
    float3 sin_phi;
    float3 sin_psi;
    float3 sin_theta;

    /* Check for NULL inputs */
    if (r == NULL) {
	return NULL;
	}

    /* Precompute sine and cosine values */
    sin_phi   = sin(c);
    cos_phi   = cos(c);
    sin_theta = sin(b);
    cos_theta = cos(b);
    sin_psi   = sin(a);
    cos_psi   = cos(a);

    /* Compute rotation matrix */
    r[0][0] =  cos_theta * cos_phi;
    r[1][0] =  cos_theta * sin_phi;
    r[2][0] = -sin_theta;
    r[0][1] =  sin_psi * sin_theta * cos_phi  -  cos_psi * sin_phi;
    r[1][1] =  sin_psi * sin_theta * sin_phi  +  cos_psi * cos_phi;
    r[2][1] =  cos_theta * sin_psi;
    r[0][2] =  cos_psi * sin_theta * cos_phi  +  sin_psi * sin_phi;
    r[1][2] =  cos_psi * sin_theta * sin_phi  -  sin_psi * cos_phi;
    r[2][2] =  cos_theta * cos_psi;

    /* Return a pointer to the result */
    return r;
    }


/******************************************************************************
********************************   ROTZXZ   ***********************************
*******************************************************************************

    This function converts three axis rotations into a rotation matrix. The
    rotations are as performed on an object about the static world Z, then
    X, then Z coordinates. This is the inverse of the Euler angles about the
    Z axis, followed by the rotated X axis, followed by the rotated Z axis.
    This function was constructed from the transpose, which is the inverse in
    this case, of the equations found in Herbert Goldstein, "Classical
    Mechanics," second edition, p. 147, called there the "x convention."
    Returns pointer to output matrix. */

float3 (*rotzxz(
    float3 a,		/* input angle about original Z axis */
    float3 b,		/* input angle about rotated  X axis */
    float3 c,		/* input angle about rotated  Z axis */
    float3 r[3][3]	/* output rotation matrix */
    ))[3]
{
    float3 cos_phi;
    float3 cos_psi;
    float3 cos_theta;
    float3 sin_phi;
    float3 sin_psi;
    float3 sin_theta;

    /* Check for NULL inputs */
    if (r == NULL) {
	return NULL;
	}

    /* Precompute sine and cosine values */
    sin_phi   = sin(a);
    cos_phi   = cos(a);
    sin_theta = sin(b);
    cos_theta = cos(b);
    sin_psi   = sin(c);
    cos_psi   = cos(c);

    /* Compute rotation matrix */
    r[0][0] =  cos_psi * cos_phi  -  cos_theta * sin_phi * sin_psi;
    r[1][0] =  cos_psi * sin_phi  +  cos_theta * cos_phi * sin_psi;
    r[2][0] =  sin_psi * sin_theta;
    r[0][1] = -sin_psi * cos_phi  -  cos_theta * sin_phi * cos_psi;
    r[1][1] = -sin_psi * sin_phi  +  cos_theta * cos_phi * cos_psi;
    r[2][1] =  cos_psi * sin_theta;
    r[0][2] =  sin_theta * sin_phi;
    r[1][2] = -sin_theta * cos_phi;
    r[2][2] =  cos_theta;

    /* Return a pointer to the result */
    return r;
    }


/******************************************************************************
********************************   ROTZYZ   ***********************************
*******************************************************************************

    This function converts three axis rotations into a rotation matrix. The
    rotations are as performed on an object about the static world Z, then
    Y, then Z coordinates. This is the inverse of the Euler angles about the
    Z axis, followed by the rotated Y axis, followed by the rotated Z axis.
    This function was constructed from the transpose, which is the inverse in
    this case, of the equations found in Herbert Goldstein, "Classical
    Mechanics," second edition, p. 147, called there the "y convention."
    Returns pointer to output matrix. */

float3 (*rotzyz(
    float3 a,		/* input angle about original Z axis */
    float3 b,		/* input angle about rotated  Y axis */
    float3 c,		/* input angle about rotated  Z axis */
    float3 r[3][3]	/* output rotation matrix */
    ))[3]
{
    float3 cos_phi;
    float3 cos_psi;
    float3 cos_theta;
    float3 sin_phi;
    float3 sin_psi;
    float3 sin_theta;

    /* Check for NULL inputs */
    if (r == NULL) {
	return NULL;
	}

    /* Precompute sine and cosine values */
    sin_phi   = sin(a);
    cos_phi   = cos(a);
    sin_theta = sin(b);
    cos_theta = cos(b);
    sin_psi   = sin(c);
    cos_psi   = cos(c);

    /* Compute rotation matrix */
    r[0][0] = -sin_psi * sin_phi  +  cos_theta * cos_phi * cos_psi;
    r[1][0] =  sin_psi * cos_phi  +  cos_theta * cos_phi * cos_psi;
    r[2][0] = -cos_psi * sin_theta;
    r[0][1] = -cos_psi * sin_phi  -  cos_theta * cos_phi * sin_psi;
    r[1][1] =  cos_psi * cos_phi  -  cos_theta * sin_phi * sin_psi;
    r[2][1] =  sin_psi * sin_theta;
    r[0][2] =  sin_theta * cos_psi;
    r[1][2] =  sin_theta * sin_psi;
    r[2][2] =  cos_theta;

    /* Return a pointer to the result */
    return r;
    }


/******************************************************************************
********************************   SCALE3   ***********************************
*******************************************************************************

    This function multipies a 3-vector by a scalar. Returns pointer to output
    vector. */

float3 *scale3(
    float3 s,		/* input scalar */
    const float3 a[3],	/* input vector */
    float3 b[3])	/* output vector */
{
    /* Check for NULL inputs */
    if ((a == NULL) || (b == NULL)) {
	zero3(b);
	return NULL;
	}

    /* Perform the scalar multiplication */
    b[0]  =  s * a[0];
    b[1]  =  s * a[1];
    b[2]  =  s * a[2];

    /* Return a pointer to the result */
    return b;
    }


/******************************************************************************
********************************   SCALE33   **********************************
*******************************************************************************

    This function multipies a 3x3 matrix by a scalar. Returns pointer to output
    matrix. */

float3 (*scale33(
    float3 s,		/* input scalar */
    float3 a[3][3],	/* input matrix */
    float3 b[3][3]	/* output matrix */
    ))[3]
{
    /* Check for NULL inputs */
    if ((a == NULL) || (b == NULL)) {
	zero33(b);
	return NULL;
	}

    /* Perform the scalar multiplication */
    b[0][0]  =  s * a[0][0];
    b[0][1]  =  s * a[0][1];
    b[0][2]  =  s * a[0][2];
    b[1][0]  =  s * a[1][0];
    b[1][1]  =  s * a[1][1];
    b[1][2]  =  s * a[1][2];
    b[2][0]  =  s * a[2][0];
    b[2][1]  =  s * a[2][1];
    b[2][2]  =  s * a[2][2];

    /* Return a pointer to the result */
    return b;
    }


/******************************************************************************
********************************   SUB3   *************************************
*******************************************************************************

    This function subtracts two 3-vectors. Returns pointer to output vector. */

float3 *sub3(
    const float3 a[3],	/* input minuend vector */
    const float3 b[3],	/* input subtrahend vector */
    float3 c[3])	/* output difference vector */
{
    /* Check for NULL inputs */
    if ((a == NULL) || (b == NULL) || (c == NULL)) {
	zero3(c);
	return NULL;
	}

    /* Subtract the two vectors */
    c[0] = a[0] - b[0];
    c[1] = a[1] - b[1];
    c[2] = a[2] - b[2];

    /* Return a pointer to the result */
    return c;
    }


/******************************************************************************
********************************   SUB33   ************************************
*******************************************************************************

    This function subtracts two 3x3 matrices. Returns pointer to output matrix.
    */

float3 (*sub33(
    float3 a[3][3],	/* input minuend matrix */
    float3 b[3][3],	/* input subtrahend matrix */
    float3 c[3][3]	/* output difference matrix */
    ))[3]
{
    /* Check for NULL inputs */
    if ((a == NULL) || (b == NULL) || (c == NULL)) {
	zero33(c);
	return NULL;
	}

    /* Calculate the difference */
    c[0][0] = a[0][0] - b[0][0];
    c[0][1] = a[0][1] - b[0][1];
    c[0][2] = a[0][2] - b[0][2];
    c[1][0] = a[1][0] - b[1][0];
    c[1][1] = a[1][1] - b[1][1];
    c[1][2] = a[1][2] - b[1][2];
    c[2][0] = a[2][0] - b[2][0];
    c[2][1] = a[2][1] - b[2][1];
    c[2][2] = a[2][2] - b[2][2];

    /* Return a pointer to the result */
    return c;
    }


/******************************************************************************
********************************   TRANS33   **********************************
*******************************************************************************

    This function transposes a 3x3 matrix. Returns pointer to output matrix. */

float3 (*trans33(
    float3 a[3][3],	/* input matrix */
    float3 b[3][3]	/* output matrix */
    ))[3]
{
    float3 a2[3][3];

    /* Check for NULL inputs */
    if ((a == NULL) || (b == NULL)) {
	zero33(b);
	return NULL;
	}

    /* Check for non-distinct output */
    if (a == b) {
	copy33(a, a2);
	a = a2;
	}

    /* Perform the matrix transpose */
    b[0][0] = a[0][0];
    b[1][0] = a[0][1];
    b[2][0] = a[0][2];
    b[0][1] = a[1][0];
    b[1][1] = a[1][1];
    b[2][1] = a[1][2];
    b[0][2] = a[2][0];
    b[1][2] = a[2][1];
    b[2][2] = a[2][2];

    /* Return a pointer to the result */
    return b;
    }


/******************************************************************************
********************************   UNIT3   ************************************
*******************************************************************************

    This function normalizes a 3-vector, producing a unit 3-vector. Returns
    pointer to output vector, or NULL on error. */

float3 *unit3(
    const float3 a[3],	/* input vector */
    float3 b[3])	/* output vector */
{
    float3 mag;

    /* Check for NULL inputs */
    if ((a == NULL) || (b == NULL)) {
	zero3(b);
	return NULL;
	}

    /* Calculate the magnitude of the vector */
    mag = sqrt(a[0] * a[0]  +  a[1] * a[1]  +  a[2] * a[2]);
    if (mag < MAT3_EPSILON) {
	zero3(b);
	return NULL;
	}

    /* Convert to a unit vector */
    b[0] = a[0] / mag;
    b[1] = a[1] / mag;
    b[2] = a[2] / mag;

    /* Return a pointer to the result */
    return b;
    }


/******************************************************************************
********************************   UNITQ   ************************************
*******************************************************************************

    This function normalizes a quaternion, producing a unit quaternion. Returns
    pointer to output quaternion, or NULL on error. */

float3 *unitq(
    const float3 a[4],	/* input quaternion */
    float3 b[4])	/* output quaternion */
{
    float3 mag;

    /* Check for NULL inputs */
    if ((a == NULL) || (b == NULL)) {
	zeroq(b);
	return NULL;
	}

    /* Calculate the magnitude of the quaternion */
    mag = sqrt(a[0] * a[0]  +  a[1] * a[1]  +  a[2] * a[2]  +  a[3] * a[3]);
    if (mag < MAT3_EPSILON) {
	zeroq(b);
	return NULL;
	}

    /* Convert to a unit quaternion */
    b[0] = a[0] / mag;
    b[1] = a[1] / mag;
    b[2] = a[2] / mag;
    b[3] = a[3] / mag;

    /* Return a pointer to the result */
    return b;
    }


/******************************************************************************
********************************   VAQUAT   ***********************************
*******************************************************************************

    This function converts a quaternion into a vector and an angle or rotation
    about that vector. If the quaternion is a unit quaternion, then the output
    vector will be a unit vector. See quatva() for comments on representation.
    Returns output angle. */

float3 vaquat(
    const float3 q[4],	/* input quaternion */
    float3 v[3],	/* output vector */
    float3 *a)		/* output angle of rotation */
{
    float3 aa;
    float3 mag;
    float3 s;

    /* Check for NULL inputs */
    if ((q == NULL) || (v == NULL) || (a == NULL)) {
	zero3(v);
	if (a != NULL) {
	    *a = 0;
	    }
	return 0.0;
	}

    /* Default values */
    v[0] = 1.0;
    v[1] = 0.0;
    v[2] = 0.0;
    *a   = 0;

    /* Compute the angle */
    mag = sqrt(q[0] * q[0]  +  q[1] * q[1]  +  q[2] * q[2]  +  q[3] * q[3]);
    if (mag < MAT3_EPSILON) {
	return 0.0;
	}
    *a = aa = 2 * acos(q[0]/mag);

    /* Isolate the vector rotation axis */
    s = sin(aa/2);
    if ((s < MAT3_EPSILON) && (s > -MAT3_EPSILON)) {
	return 0.0;
	}
    v[0] = q[1] / s;
    v[1] = q[2] / s;
    v[2] = q[3] / s;

    /* Return the angle */
    return aa;
    }


/******************************************************************************
********************************   XYZQUAT   **********************************
*******************************************************************************

    This function converts a quaternion into three axis rotations. The
    rotations are the familiar roll, pitch, and yaw (heading), performed on
    an object about the static world coordinates. This is the inverse of the
    Euler angles about the Z axis, followed by the rotated Y axis, followed
    by the twice-rotated X axis. See rotxyz(). Returns pointer to input
    quaternion. */

const float3 *xyzquat(
    const float3 q[4],	/* input quaternion */
    float3 *a,		/* output angle about X axis (roll) */
    float3 *b,		/* output angle about Y axis (pitch) */
    float3 *c)		/* output angle about Z axis (yaw) */
{
    float3 r[3][3];

    /* Check for NULL inputs */
    if ((q == NULL) || (a == NULL) || (b == NULL) || (c == NULL)) {
	if (a != NULL) {
	    *a = 0;
	    }
	if (b != NULL) {
	    *b = 0;
	    }
	if (c != NULL) {
	    *c = 0;
	    }
	return NULL;
	}

    /* Convert to a rotation matrix */
    rotq(q, r);

    /* Compute rotation angles */
    xyzrot(r, a, b, c);

    /* Return a pointer to the input */
    return q;
    }


/******************************************************************************
********************************   XYZROT   ***********************************
*******************************************************************************

    This function converts a rotation matrix into three axis rotations. The
    rotations are the familiar roll, pitch, and yaw (heading), performed on
    an object about the static world coordinates. This is the inverse of the
    Euler angles about the Z axis, followed by the rotated Y axis, followed
    by the twice-rotated X axis. See rotxyz(). Returns pointer to input
    quaternion. */

float3 (*xyzrot(
    float3 r[3][3],	/* input rotation matrix */
    float3 *a,		/* output angle about X axis (roll) */
    float3 *b,		/* output angle about Y axis (pitch) */
    float3 *c		/* output angle about Z axis (yaw) */
    ))[3]
{
    float3 r00;
    float3 r01;
    float3 r10;
    float3 r11;
    float3 r20;
    float3 r21;
    float3 r22;

    /* Check for NULL inputs */
    if ((r == NULL) || (a == NULL) || (b == NULL) || (c == NULL)) {
	if (a != NULL) {
	    *a = 0;
	    }
	if (b != NULL) {
	    *b = 0;
	    }
	if (c != NULL) {
	    *c = 0;
	    }
	return NULL;
	}

    /* Compute rotation angles */
    r00 = r[0][0];
    r01 = r[0][1];
    r10 = r[1][0];
    r11 = r[1][1];
    r20 = r[2][0];
    r21 = r[2][1];
    r22 = r[2][2];
    *b = atan2(-r20, sqrt(r00*r00 + r10*r10));
    if ((r00*r00 + r10*r10) > MAT3_EPSILON*MAT3_EPSILON) {
	*a = atan2(r21, r22);
	*c = atan2(r10, r00);
	}
    else {
	*a = 0;			/* atan2(-r12, r11) */
	*c = atan2(-r01, r11);	/* 0                */
	}

    /* Return a pointer to the input */
    return r;
    }


/******************************************************************************
********************************   ZERO3   ************************************
*******************************************************************************

    This function creates a zero 3-vector. Returns pointer to output vector. */

float3 *zero3(
    float3 a[3])	/* output vector */
{
    /* Check for NULL inputs */
    if (a == NULL) {
	return NULL;
	}

    /* Zero out the entire vector */
    a[0] = 0;
    a[1] = 0;
    a[2] = 0;

    /* Return a pointer to the result */
    return a;
    }


/******************************************************************************
********************************   ZERO33   ***********************************
*******************************************************************************

    This function creates a zero 3x3 matrix. Returns pointer to output matrix.
    */

float3 (*zero33(
    float3 a[3][3]	/* output matrix */
    ))[3]
{
    /* Check for NULL inputs */
    if (a == NULL) {
	return NULL;
	}

    /* Zero out the entire matrix */
    a[0][0] = 0;
    a[0][1] = 0;
    a[0][2] = 0;
    a[1][0] = 0;
    a[1][1] = 0;
    a[1][2] = 0;
    a[2][0] = 0;
    a[2][1] = 0;
    a[2][2] = 0;

    /* Return a pointer to the result */
    return a;
    }


/******************************************************************************
********************************   ZEROQ   ************************************
*******************************************************************************

    This function creates a zero quaternion, that is, one with all elements
    zero. This is not a usable quaternion. See identq() for one that represents
    a rotation by a zero angle. Returns pointer to output quaternion. */

float3 *zeroq(
    float3 a[4])	/* output quaternion */
{
    /* Check for NULL inputs */
    if (a == NULL) {
	return NULL;
	}

    /* Construct an all-zero quaternion */
    a[0] = 0;
    a[1] = 0;
    a[2] = 0;
    a[3] = 0;

    /* Return a pointer to the result */
    return a;
    }


/******************************************************************************
********************************   ZXZROT   ***********************************
*******************************************************************************

    This function converts a rotation matrix into three axis rotations. The
    rotations are as performed on an object about the static world Z, then
    X, then Z coordinates. This is the inverse of the Euler angles about the
    Z axis, followed by the rotated X axis, followed by the rotated Z axis.
    See rotzxz(). Note that this operation becomes ill-conditioned as the X
    term (b) approaches zero. */

float3 (*zxzrot(
    float3 r[3][3],	/* input rotation matrix */
    float3 *a,		/* output angle about original Z axis */
    float3 *b,		/* output angle about rotated  X axis */
    float3 *c		/* output angle about rotated  Z axis */
    ))[3]
{
    float3 r02;
    float3 r12;
    float3 r20;
    float3 r21;
    float3 r22;

    /* Check for NULL inputs */
    if ((r == NULL) || (a == NULL) || (b == NULL) || (c == NULL)) {
	if (a != NULL) {
	    *a = 0;
	    }
	if (b != NULL) {
	    *b = 0;
	    }
	if (c != NULL) {
	    *c = 0;
	    }
	return NULL;
	}

    /* Compute rotation angles */
    r02 = r[0][2];
    r12 = r[1][2];
    r20 = r[2][0];
    r21 = r[2][1];
    r22 = r[2][2];
    *a = atan2(r02, -r12);
    *b = acos(r22);
    *c = atan2(r20, r21);

    /* Return a pointer to the input */
    return r;
    }
