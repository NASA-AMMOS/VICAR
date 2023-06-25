/******************************************************************************
*                                                                             *
*		JPL Stereo Vision code					      *
*									      *
*		Developed by the Machine Vision and Tracking Sensors Group    *
*		at the Jet Propulsion Laboratory			      *
*								  	      *
*									      *
*	For information contact:					      *
*									      *
*			Larry Matthies	lhm@robotics.jpl.nasa.gov	      *
*                      	Todd Litwin     litwin@robotics.jpl.nasa.gov          *
*			Mark Maimone	mark.maimone@jpl.nasa.gov	      *
*			Yalin Xiong	yx@robotics.jpl.nasa.gov	      *
*								  	      *
*                                       Updated: 16 Nov 1998                  *
*                                                                             *
*                                       Copyright (C) 1993, 1994, 1995, 1996, *
*                                                     1997, 1998              *
*                                       California Institute of Technology    *
*                                       All Rights Reserved                   *
*                                                                             *
******************************************************************************/


#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "float_matrix.h"
#include "JPLPic.h"

// ControlsHell provides a nice facility for automatically building a
// single header file from all source modules, but it's not available
// everywhere.  Both cases (CS or non-CS) are handled here.

#ifdef CS_ACTIVE

#include "rtilib/makeheader.h"	// Allows building of single header file
#else
#define Public
#define Private static
#define Package
#define Peer
#endif


#define MAXDIM	32			/* The maximum dimension of a matrix */

// local typedef for ease of displaying code---should not impact efficiency
typedef float vec2[2], vec3[3], vec4[4];


/********************************************************************************
 ********************************************************************************
 **		Generic Matrix procedures
 ********************************************************************************
 ********************************************************************************/
// Where possible CopyMatrix, Add, Subtract etc are implemented as macros thru Vector functions


/********************************************************************************
 * IdentityMatrix() sets the given (nRows x nCols) matrix to a unit matrix.
 * Nonzero elements appear only on the diagonals, even for rectangular matrices.
 ********************************************************************************/
// was void MLUnitMatrix(float *m, long nRows, register long nCols)
Public void IdentityMatrix(float *m, long nRows, register long nCols)
{
	register float *p;
	register long i;
	
	for (i = nRows * nCols, p = m; i--; *p++ = 0.0F) ;	/* Clear matrix */
	i = (nRows <= nCols ? nRows : nCols);	/* Size of subsquare */
	nCols++;			/* Distance between one 1 and the next */
	for (p = m; i--; p+= nCols)
		*p = 1.0F;		/* Scatter the 1's (or whatever) along the diagonal */
}


/********************************************************************************
 * MLTransposeMatrix() copies the transpose of matrix M into matrix N
 * This works for rectangular matrices as well, transposing a
 * (nRows x nCols) matrix into a (nCols x nRows) matrix.
 * M^T --> N
 * IN PLACE SUPPORTED
 ********************************************************************************/

Public void TransposeMatrix(register const float *from, register float *to, long nRows, register long nCols)
{
	register long i, j;
	register const float *p;
	float temp[MAXDIM][MAXDIM];

	if (from == to) {
		for (i = 0, p = from; i < nRows; i++)	/* Copy matrix to temp */
			for (j = 0; j < nCols; j++)
				temp[i][j] = *p++;

		for (i = 0, p = from; i < nCols; i++)	/* Transpose and copy back */
			for (j = 0; j < nRows; j++)
				*to++ = temp[j][i];

	} else {
		for (i = nCols; i--; from++)		/* For each column */
			for (j = nRows, p = from; j--; p += nCols)
				*to++ = *p;			/* Copy columns to rows */
	}
}


/********************************************************************************
 * NormMatrix() computes the L-infinity norm (induced by ROW vectors)
 * of the largest square submatrix of the given matrix.
 ********************************************************************************/
// was PARAMfloat MLNormMatrix(register const float *g0, long height, register long width)
Public float NormMatrix(register const float *g0, long height, register long width)
{
	long subsquare = (width < height) ? width : height;
	register long i;
	long j;
	register const float *g;
	register double sum;
	register float max = 0;
	
	for (j = subsquare, g = g0; j--; g = ++g0) {
		for (i = subsquare, sum = 0; i--; g += width)
			sum += fabs(*g);
		if (max < sum)
			max = sum;
	}
	
	return(max);
}

/********************************************************************************
 * DeterminantMatrix() evaluates a determinant by triangularization with
 * searching for pivot in row and with scaling of the rows of the matrix
 * before triangularization.
 ********************************************************************************/

#define A(i,j) b[(n*(i))+(j)]
// was PARAMfloat MLDeterminantMatrix(register const float *a, register long n)
Public float DeterminantMatrix(register const float *a, register long n)
{
	double product, temp, mx;
	register long i, j, r, s;
	float mult[MAXDIM];
	float b[MAXDIM*MAXDIM];
	
	/* Copy matrix to working storage */
	{
		register float *p = b;
		register const float *q = a;
		for (i = n*n; i--; *p++ = *q++) ;
	}

	/* Equilibration of rows */ 
	for (i = n; i--; ) {
		mx = 0;
		for (j = n; j--; )
			if ((temp = fabs(A(i,j))) > mx)
				mx = temp;
		if (mx == 0)
			return(0);		/* Singular matrix */
		mult[i] = mx;			/* = base^ex for exact scaling */
		if (mx != 1)
			for (j = n; j--; )
				A(i,j) /= mx;	/* Use scalb() or ldexp() for speed & accuracy */
	}
		
	product = 1;
	for (r = 0; r < n-1; r++) {
		s = r;
		temp = fabs(A(r,r));
		for (j = r+1; j < n; j++) {
			if (temp < fabs(A(r,j))) {
				temp = fabs(A(r,j));
				s = j;
			}
		}
		if (temp == 0)
			return(0);
		if (s != r) {
			product = -product;
			for (i = r; i < n; i++) {
				temp = A(i,r);
				A(i,r) = A(i,s);
				A(i,s) = temp;
			}
		}
		product *= A(r,r);	/* Be on guard for overflow or underflow here */
		for (i = r+1; i < n; i++) {
			temp = A(i,r) / A(r,r);
			for (j = r+1; j < n; j++)
				A(i,j) -= A(r,j) * temp;
		}
	}
	temp = product * A(n-1,n-1);
	for (r = 0; r < n; r++)
		temp *= mult[r];	/* Again danger of overflow or underflow */
	return(temp);
}
#undef A

/********************************************************************************
 * InvertMatrix()
 *	Inverts square matrices
 *	With tall matrices, invert upper part and transform the bottom
 *	rows as would be expected if embedded into a larger matrix.
 *	Undefined for wide matrices.
 * M^(-1) --> Minv
 * IN PLACE SUPPORTED, no performance difference
 *
 * 1 is returned if the matrix was non-singular and the inversion was successful;
 * 0 is returned if the matrix was singular and the inversion failed.
 ********************************************************************************/

Public long InvertMatrix(const float *M, float *Minv, long nRows, register long n)
{
	float *m;
	long tallerBy = nRows - n;		/* Excess of rows over columns */
	register long j, i;
	float b[MAXDIM];
	double lu[MAXDIM*MAXDIM+MAXDIM];

	/* Decompose matrix into L and U triangular matrices */
// was	if ((tallerBy < 0) || (MLLUdecompose(M, lu, n) == 0)) {
	if ((tallerBy < 0) || (LUDecompose(M, lu, n) == 0)) {
		return(0);		/* Singular */
	}

	/* Invert matrix by solving n simultaneous equations n times */
	for (i = 0, m = Minv; i < n; i++, m += n) {
		for(j = 0; j < n; j++)
			b[j] = 0;
		b[i] = 1;
	// was	MLLUsolve(lu, m, b, n);	/* Into a row of m */

		LUSolve(lu, b, m, n);	/* Into a row of m */
	}
	
	/* Special post-processing for affine transformations (e.g. 4x3) */
	if (tallerBy) {			/* Affine transformation */
		register float *t = Minv+n*n;			/* Translation vector */
		m = Minv;			/* Reset m */
	// was	MLLinearTransformInPlace(t, m, tallerBy, n);	/* Invert translation */
		LinearTransform(t, m, t, tallerBy, n, n);	/* Invert translation */
		for (j = tallerBy * n; n--; t++)
			*t = -*t;				/* Negate translation vector */
	}

	return(1);
}

// ProjectiveTransformVectorMatrix
// Projective transform of a row vector
//  NO IN PLACE operations supported --- in and out may be of different dimension
// i.e., out[rcols-1]
// compute b = x[0..n-2].A / (x[n-1].A)
Public void ProjectiveTransformVectorMatrix(const float *x, const float *A, float *out, long nRows, long nCols)
{
	register long i, j;
	register double acc;
	register const float *ap, *xp;
	float *op = out;
	float scale;
	
	for(i = nRows-1; i--; ++A) {
		acc = 0;
		xp = x;
		ap = A;
		for(j = nCols; j--; ++xp, ap += nCols) {
			acc += (double) *ap * *xp;
		}
		*out++ = acc;
	}
	// compute the product of the last row and the vector x
	acc = 0.0;
	xp = x;
	ap = A;
	for(j = nCols; j--; ++xp, ap += nCols) {
		acc += (double) *ap * *xp;
	}
	scale = 1.0/ acc;
	// normalize the homogenous vector by scaling it
	out = op;
	for(i = nRows-1; i--;)
		*out++ *= scale;
}

// ProjectiveTransformMatrixVector
// Projective transform of a column vector
//  NO IN PLACE supported, out may be of different dimension than x
//  i.e., out[nRows-1]
// compute b = Ax[0..n-2] / (Ax[n-1])
Public void ProjectiveTransformMatrixVector(const float *A, const float *x, float *out, long nRows, long nCols)
{
	register long i, j;
	register double acc;
	register const float *ap, *xp;
	float *op = out;
	float scale;
	
	for(i = nRows-1; i--; A += nCols) {
		acc = 0;
		xp = x;
		ap = A;
		for(j = nCols; j--; ++xp, ++ap) {
			acc += (double) *ap * *xp;
		}
		*out++ = acc;
	}
	// compute the product of the last row and the vector x
	acc = 0.0;
	xp = x;
	ap = A;
	for(j = nCols; j--; ++xp, ++ap) {
		acc += (double) *ap * *xp;
	}
	scale = 1.0/ acc;
	// normalize the homogenous vector by scaling it
	out = op;
	for(i = nRows-1; i--;)
		*out++ *= scale;
}


/********************************************************************************
 *  ReorthogonalizeMatrix
 *
 * Matrix Orthogonalization
 * Eric Raible
 * from "Graphics Gems", Academic Press, 1990
 *
 * Reorthogonalize matrix R - that is find an orthogonal matrix that is
 * "close" to R by computing an approximation to the orthogonal matrix
 *
 *           T  -1/2
 *   RC = R(R R)
 *			        			 T      -1
 * [RC is orthogonal because (RC) = (RC) ]
 *				               	                    -1/2
 * To compute C, we evaluate the Taylor expansion of F(x) = (I + x)
 * (where x = C - I) about x=0.
 * This gives C = I - (1/2)x + (3/8)x^2 - (5/16)x^3 + ...
 ********************************************************************************/

static float coef[10] = 			/* From mathematica */
  { 1, -1/2., 3/8., -5/16., 35/128., -63/256.,
    231/1024., -429/2048., 6435/32768., -12155/65536. };

#define MAXORTHODIM 4

Public void
ReorthogonalizeMatrix(float *R, long n, long limit)
{
	float	I[MAXORTHODIM][MAXORTHODIM], X[MAXORTHODIM][MAXORTHODIM],
			X_power[MAXORTHODIM][MAXORTHODIM], Sum[MAXORTHODIM][MAXORTHODIM];
	int power;

	if (limit > 10) limit = 10;

	TransposeMatrix(R, X[0], n, n);		/* Rt */
	LinearTransform(X[0], R, X[0], n, n, n);	/* RtR */
	IdentityMatrix(I[0], n, n);
	SubtractMatrix(X[0], I[0], X[0], n, n);	/* RtR - I */
	IdentityMatrix(X_power[0], n, n);			/* X^0 */
	IdentityMatrix(Sum[0], n, n);				/* coef[0] * X^0 */

	for (power = 1; power < limit; ++power) {
		LinearTransform(X_power[0], X[0], X_power[0], n, n, n);			/* X^power */
		LinearAddMatrix(coef[power], X_power[0], Sum[0], Sum[0], n, n);
	}

	LinearTransform(R, Sum[0], R, n, n, n);
}


/* THE FOLLOWING ARE SPECIALIZED FOR 2x2 to 4x4 matrices...
 * probably should be Macros
 */
/********************************************************************************
 ********************************************************************************
 **	2x2 Matrix specialization routines (should be macros)
 ********************************************************************************
 ********************************************************************************/
/********************************************************************************
 *  AdjointMatrix2x2
 * computes the adjoint matrix of a 2x2 matrix M
 * Adjoint(M) --> N
 * IN PLACE SUPPORTED
 ********************************************************************************/
// was void MLAdjointMatrix2x2(float* M)
Public void AdjointMatrix2x2(float* M, float *N)
{
	register vec2* m = (vec2*) M;
	register float temp;

	if (M == N) { // IN PLACE
		temp = m[1][1]; m[1][1] = m[0][0]; m[0][0] = temp;	/* Swap (0,0) and (1,1) */
		m[1][0] = -m[1][0];	m[0][1] = -m[0][1];				/* Negate (0,1) and (1,0) */
	} else {
		register vec2* n = (vec2*)N;
		n[1][1] = m[0][0]; n[0][0] = m[1][1];	/* Swap (0,0) and (1,1) */
		n[1][0] = -m[1][0];	n[0][1] = -m[0][1];				/* Negate (0,1) and (1,0) */
	}
}

/********************************************************************************
 *  InvertMatrix2x2
 * M^(-1) --> Minv
 * IN PLACE SUPPORTED
 ********************************************************************************/
// was long MLInvertMatrix2x2(float* M)
Public long InvertMatrix2x2(float* M, float *Minv)
{
	register vec2* m = (vec2*) M;
	register float d;
	register float m00;

	if ((d = m[0][0]*m[1][1] - m[1][0]*m[0][1]) == 0)
		return(0);

	if (M == Minv) { // IN PLACE
		m00 = m[1][1] / d;
		m[0][1] /= -d;
		m[1][0] /= -d;
		m[1][1] = m[0][0] / d;
		m[0][0] = m00;
	} else {
		register vec2* n = (vec2*)Minv;

		n[0][0] = m[1][1] / d;
		n[0][1] = m[0][1]/(-d);
		n[1][0] = m[1][0]/(-d);
		n[1][1] = m[0][0] / d;
	}
	
	return(1);
}



/********************************************************************************
 *  L2NormMatrix2x2
 * compute and return the norm of a 2x2 matrix
 ********************************************************************************/
// was PARAMfloat MLL2NormMatrix2x2(float* M)
Public float L2NormMatrix2x2(const float* M)
{
	register const vec2* m = (const vec2*)M;
	register float diag = (m[0][0] + m[1][1]) * 0.5; 
	register float diag2 = diag * diag;
	register float disc = diag2 - ((double) m[0][0]*m[1][1] - (double) m[0][1]*m[1][0]);
	return((disc < 0) ? sqrt(diag2 - disc) : (fabs(diag) + sqrt(disc)));
}



/********************************************************************************
 ********************************************************************************
 **	3x3 Matrix specialization routines (should be macros)
 ********************************************************************************
 ********************************************************************************/
/********************************************************************************
 *  AdjointMatrix3x3
 * Adjoint(M) --> out
 * IN PLACE SUPPORTED
 ********************************************************************************/
// was void MLAdjointMatrix3x3(float *M)
Public void AdjointMatrix3x3(float *M, float *out)
{
	struct MX { float m[3][3]; };
	register const vec3* m = (const vec3*)M;
	
	if (out == M) { // IN PLACE
		float t[3][3];

		t[0][0] = (double)   m[1][1] * m[2][2] - (double) m[1][2] * m[2][1];	/* t(0,0) */
		t[0][1] = (double) - m[0][1] * m[2][2] + (double) m[0][2] * m[2][1];	/* t(0,1) */
		t[0][2] = (double)   m[0][1] * m[1][2] - (double) m[0][2] * m[1][1];	/* t(0,2) */
		t[1][0] = (double) - m[1][0] * m[2][2] + (double) m[1][2] * m[2][0];	/* t(1,0) */
		t[1][1] = (double)   m[0][0] * m[2][2] - (double) m[0][2] * m[2][0];	/* t(1,1) */
		t[1][2] = (double) - m[0][0] * m[1][2] + (double) m[0][2] * m[1][0];	/* t(1,2) */
		t[2][0] = (double)   m[1][0] * m[2][1] - (double) m[1][1] * m[2][0];	/* t(2,0) */
		t[2][1] = (double) - m[0][0] * m[2][1] + (double) m[0][1] * m[2][0];	/* t(2,1) */
		t[2][2] = (double)   m[0][0] * m[1][1] - (double) m[0][1] * m[1][0];	/* t(2,2) */
		
		*((struct MX*)(M)) = *((struct MX*)(t[0]));
	} else {
		register vec3* t = (vec3*) out;

		t[0][0] = (double)   m[1][1] * m[2][2] - (double) m[1][2] * m[2][1];	/* t(0,0) */
		t[0][1] = (double) - m[0][1] * m[2][2] + (double) m[0][2] * m[2][1];	/* t(0,1) */
		t[0][2] = (double)   m[0][1] * m[1][2] - (double) m[0][2] * m[1][1];	/* t(0,2) */
		t[1][0] = (double) - m[1][0] * m[2][2] + (double) m[1][2] * m[2][0];	/* t(1,0) */
		t[1][1] = (double)   m[0][0] * m[2][2] - (double) m[0][2] * m[2][0];	/* t(1,1) */
		t[1][2] = (double) - m[0][0] * m[1][2] + (double) m[0][2] * m[1][0];	/* t(1,2) */
		t[2][0] = (double)   m[1][0] * m[2][1] - (double) m[1][1] * m[2][0];	/* t(2,0) */
		t[2][1] = (double) - m[0][0] * m[2][1] + (double) m[0][1] * m[2][0];	/* t(2,1) */
		t[2][2] = (double)   m[0][0] * m[1][1] - (double) m[0][1] * m[1][0];	/* t(2,2) */
	}
}



/********************************************************************************
 ********************************************************************************
 **	3x4 Matrix specialization routines (should be macros)
 ********************************************************************************
 ********************************************************************************/
/********************************************************************************
 * RightInverseMatrix3x4
 *
 * Given a matrix M(3x4), this produces a matrix N(4x3) such that
 * M * N = I(3x3).  Note that N is not a left inverse, i.e. N * M != I(4x4)
 * This is useful for inverting projective transformations.
 * m^(-1) --> inv
 * IN PLACE SUPPORTED
 ********************************************************************************/
// was void MLRightInverseMatrix3x4(float *m, float *inv)
Public void RightInverseMatrix3x4(const float *m, float *inv)
{
	float Mt[4][3];
	float MMt[3][3];

	TransposeMatrix(m, Mt[0], 3, 4);	/* M transposed */
	LinearTransform(m, Mt[0], MMt[0], 3, 4, 3);	/* MMt = M * Mt */
	AdjointMatrix3x3(MMt[0],MMt[0]);	/* projective inverse: scaling doesn't matter */
	LinearTransform(Mt[0], MMt[0], inv, 4, 3, 3);
}



/********************************************************************************
 ********************************************************************************
 **	4x3 Matrix specialization routines (should be macros)
 ********************************************************************************
 ********************************************************************************/
/********************************************************************************
 *  LeftInverseMatrix4x3
 *
 * Given a matrix M(4x3), this produces a matrix N(3x4) such that
 * N * M = I(3x3).  Note that N is not a right inverse, i.e. M * N != I(4x4)
 * This is useful for inverting projective transformations.
 * m^(-1) --> inv
 * IN PLACE SUPPORTED
 ********************************************************************************/
// was void MLLeftInverseMatrix4x3(float *m, float *inv)
Public void LeftInverseMatrix4x3(const float *m, float *inv)
{
	float Mt[3][4];
	float MtM[3][3];

	TransposeMatrix(m, Mt[0], 4, 3);	/* M transposed */
	LinearTransform(Mt[0], m, MtM[0], 3, 4, 3);	/* MMt = M * Mt */
	AdjointMatrix3x3(MtM[0],MtM[0]);	/* projective inverse: scaling doesn't matter */
	LinearTransform(MtM[0], Mt[0], inv, 3, 3, 4);
}



/********************************************************************************
 ********************************************************************************
 **	4x4 Matrix specialization routines (should be macros)
 ********************************************************************************
 ********************************************************************************/
/********************************************************************************
 *  AdjointMatrix4x4
 * Adjoint(M) --> out
 * IN PLACE SUPPORTED
 ********************************************************************************/
// was void MLAdjointMatrix4x4(float *M)
Public void AdjointMatrix4x4(float *M, float *out)
{
	struct MC { float m[4][4]; };
	register const vec4 *m = (const vec4 *)M;
	
	if (out == M) { // IN PLACE
		float ad[4][4];

		ad[0][0] =  -(m[1][3]*m[2][2]*m[3][1]) +  m[1][2]*m[2][3]*m[3][1]
			+  m[1][3]*m[2][1]*m[3][2] -  m[1][1]*m[2][3]*m[3][2]
			-  m[1][2]*m[2][1]*m[3][3] +  m[1][1]*m[2][2]*m[3][3];
		ad[0][1] =  m[0][3]*m[2][2]*m[3][1] -  m[0][2]*m[2][3]*m[3][1]
			-  m[0][3]*m[2][1]*m[3][2] +  m[0][1]*m[2][3]*m[3][2]
			+  m[0][2]*m[2][1]*m[3][3] -  m[0][1]*m[2][2]*m[3][3];
		ad[0][2] =  -(m[0][3]*m[1][2]*m[3][1]) +  m[0][2]*m[1][3]*m[3][1]
			+  m[0][3]*m[1][1]*m[3][2] -  m[0][1]*m[1][3]*m[3][2]
			-  m[0][2]*m[1][1]*m[3][3] +  m[0][1]*m[1][2]*m[3][3];
		ad[0][3] =  m[0][3]*m[1][2]*m[2][1] -  m[0][2]*m[1][3]*m[2][1]
			-  m[0][3]*m[1][1]*m[2][2] +  m[0][1]*m[1][3]*m[2][2]
			+  m[0][2]*m[1][1]*m[2][3] -  m[0][1]*m[1][2]*m[2][3];
		
		ad[1][0] =  m[1][3]*m[2][2]*m[3][0] -  m[1][2]*m[2][3]*m[3][0]
			-  m[1][3]*m[2][0]*m[3][2] +  m[1][0]*m[2][3]*m[3][2]
			+  m[1][2]*m[2][0]*m[3][3] -  m[1][0]*m[2][2]*m[3][3];
		ad[1][1] =  -(m[0][3]*m[2][2]*m[3][0]) +  m[0][2]*m[2][3]*m[3][0]
			+  m[0][3]*m[2][0]*m[3][2] -  m[0][0]*m[2][3]*m[3][2]
			-  m[0][2]*m[2][0]*m[3][3] +  m[0][0]*m[2][2]*m[3][3];
		ad[1][2] =  m[0][3]*m[1][2]*m[3][0] -  m[0][2]*m[1][3]*m[3][0]
			-  m[0][3]*m[1][0]*m[3][2] +  m[0][0]*m[1][3]*m[3][2]
			+  m[0][2]*m[1][0]*m[3][3] -  m[0][0]*m[1][2]*m[3][3];
		ad[1][3] =  -(m[0][3]*m[1][2]*m[2][0]) +  m[0][2]*m[1][3]*m[2][0]
			+  m[0][3]*m[1][0]*m[2][2] -  m[0][0]*m[1][3]*m[2][2]
			-  m[0][2]*m[1][0]*m[2][3] +  m[0][0]*m[1][2]*m[2][3];
		
		ad[2][0] =  -(m[1][3]*m[2][1]*m[3][0]) +  m[1][1]*m[2][3]*m[3][0]
			+  m[1][3]*m[2][0]*m[3][1] -  m[1][0]*m[2][3]*m[3][1]
			-  m[1][1]*m[2][0]*m[3][3] +  m[1][0]*m[2][1]*m[3][3];
		ad[2][1] =  m[0][3]*m[2][1]*m[3][0] -  m[0][1]*m[2][3]*m[3][0]
			-  m[0][3]*m[2][0]*m[3][1] +  m[0][0]*m[2][3]*m[3][1]
			+  m[0][1]*m[2][0]*m[3][3] -  m[0][0]*m[2][1]*m[3][3];
		ad[2][2] =  -(m[0][3]*m[1][1]*m[3][0]) +  m[0][1]*m[1][3]*m[3][0]
			+  m[0][3]*m[1][0]*m[3][1] -  m[0][0]*m[1][3]*m[3][1]
			-  m[0][1]*m[1][0]*m[3][3] +  m[0][0]*m[1][1]*m[3][3];
		ad[2][3] =  m[0][3]*m[1][1]*m[2][0] -  m[0][1]*m[1][3]*m[2][0]
			-  m[0][3]*m[1][0]*m[2][1] +  m[0][0]*m[1][3]*m[2][1]
			+  m[0][1]*m[1][0]*m[2][3] -  m[0][0]*m[1][1]*m[2][3];
		
		ad[3][0] =  m[1][2]*m[2][1]*m[3][0] -  m[1][1]*m[2][2]*m[3][0]
			-  m[1][2]*m[2][0]*m[3][1] +  m[1][0]*m[2][2]*m[3][1]
			+  m[1][1]*m[2][0]*m[3][2] -  m[1][0]*m[2][1]*m[3][2];
		ad[3][1] =  -(m[0][2]*m[2][1]*m[3][0]) +  m[0][1]*m[2][2]*m[3][0]
			+  m[0][2]*m[2][0]*m[3][1] -  m[0][0]*m[2][2]*m[3][1]
			-  m[0][1]*m[2][0]*m[3][2] +  m[0][0]*m[2][1]*m[3][2];
		ad[3][2] =  m[0][2]*m[1][1]*m[3][0] -  m[0][1]*m[1][2]*m[3][0]
			-  m[0][2]*m[1][0]*m[3][1] +  m[0][0]*m[1][2]*m[3][1]
			+  m[0][1]*m[1][0]*m[3][2] -  m[0][0]*m[1][1]*m[3][2];
		ad[3][3] =  -(m[0][2]*m[1][1]*m[2][0]) +  m[0][1]*m[1][2]*m[2][0]
			+  m[0][2]*m[1][0]*m[2][1] -  m[0][0]*m[1][2]*m[2][1]
			-  m[0][1]*m[1][0]*m[2][2] +  m[0][0]*m[1][1]*m[2][2];
		
		*((struct MC *)M) = *((struct MC *)ad[0]);	 /* Copy matrix over */
	} else {
		register vec4 *ad = (vec4 *) out;


		ad[0][0] =  -(m[1][3]*m[2][2]*m[3][1]) +  m[1][2]*m[2][3]*m[3][1]
			+  m[1][3]*m[2][1]*m[3][2] -  m[1][1]*m[2][3]*m[3][2]
			-  m[1][2]*m[2][1]*m[3][3] +  m[1][1]*m[2][2]*m[3][3];
		ad[0][1] =  m[0][3]*m[2][2]*m[3][1] -  m[0][2]*m[2][3]*m[3][1]
			-  m[0][3]*m[2][1]*m[3][2] +  m[0][1]*m[2][3]*m[3][2]
			+  m[0][2]*m[2][1]*m[3][3] -  m[0][1]*m[2][2]*m[3][3];
		ad[0][2] =  -(m[0][3]*m[1][2]*m[3][1]) +  m[0][2]*m[1][3]*m[3][1]
			+  m[0][3]*m[1][1]*m[3][2] -  m[0][1]*m[1][3]*m[3][2]
			-  m[0][2]*m[1][1]*m[3][3] +  m[0][1]*m[1][2]*m[3][3];
		ad[0][3] =  m[0][3]*m[1][2]*m[2][1] -  m[0][2]*m[1][3]*m[2][1]
			-  m[0][3]*m[1][1]*m[2][2] +  m[0][1]*m[1][3]*m[2][2]
			+  m[0][2]*m[1][1]*m[2][3] -  m[0][1]*m[1][2]*m[2][3];
		
		ad[1][0] =  m[1][3]*m[2][2]*m[3][0] -  m[1][2]*m[2][3]*m[3][0]
			-  m[1][3]*m[2][0]*m[3][2] +  m[1][0]*m[2][3]*m[3][2]
			+  m[1][2]*m[2][0]*m[3][3] -  m[1][0]*m[2][2]*m[3][3];
		ad[1][1] =  -(m[0][3]*m[2][2]*m[3][0]) +  m[0][2]*m[2][3]*m[3][0]
			+  m[0][3]*m[2][0]*m[3][2] -  m[0][0]*m[2][3]*m[3][2]
			-  m[0][2]*m[2][0]*m[3][3] +  m[0][0]*m[2][2]*m[3][3];
		ad[1][2] =  m[0][3]*m[1][2]*m[3][0] -  m[0][2]*m[1][3]*m[3][0]
			-  m[0][3]*m[1][0]*m[3][2] +  m[0][0]*m[1][3]*m[3][2]
			+  m[0][2]*m[1][0]*m[3][3] -  m[0][0]*m[1][2]*m[3][3];
		ad[1][3] =  -(m[0][3]*m[1][2]*m[2][0]) +  m[0][2]*m[1][3]*m[2][0]
			+  m[0][3]*m[1][0]*m[2][2] -  m[0][0]*m[1][3]*m[2][2]
			-  m[0][2]*m[1][0]*m[2][3] +  m[0][0]*m[1][2]*m[2][3];
		
		ad[2][0] =  -(m[1][3]*m[2][1]*m[3][0]) +  m[1][1]*m[2][3]*m[3][0]
			+  m[1][3]*m[2][0]*m[3][1] -  m[1][0]*m[2][3]*m[3][1]
			-  m[1][1]*m[2][0]*m[3][3] +  m[1][0]*m[2][1]*m[3][3];
		ad[2][1] =  m[0][3]*m[2][1]*m[3][0] -  m[0][1]*m[2][3]*m[3][0]
			-  m[0][3]*m[2][0]*m[3][1] +  m[0][0]*m[2][3]*m[3][1]
			+  m[0][1]*m[2][0]*m[3][3] -  m[0][0]*m[2][1]*m[3][3];
		ad[2][2] =  -(m[0][3]*m[1][1]*m[3][0]) +  m[0][1]*m[1][3]*m[3][0]
			+  m[0][3]*m[1][0]*m[3][1] -  m[0][0]*m[1][3]*m[3][1]
			-  m[0][1]*m[1][0]*m[3][3] +  m[0][0]*m[1][1]*m[3][3];
		ad[2][3] =  m[0][3]*m[1][1]*m[2][0] -  m[0][1]*m[1][3]*m[2][0]
			-  m[0][3]*m[1][0]*m[2][1] +  m[0][0]*m[1][3]*m[2][1]
			+  m[0][1]*m[1][0]*m[2][3] -  m[0][0]*m[1][1]*m[2][3];
		
		ad[3][0] =  m[1][2]*m[2][1]*m[3][0] -  m[1][1]*m[2][2]*m[3][0]
			-  m[1][2]*m[2][0]*m[3][1] +  m[1][0]*m[2][2]*m[3][1]
			+  m[1][1]*m[2][0]*m[3][2] -  m[1][0]*m[2][1]*m[3][2];
		ad[3][1] =  -(m[0][2]*m[2][1]*m[3][0]) +  m[0][1]*m[2][2]*m[3][0]
			+  m[0][2]*m[2][0]*m[3][1] -  m[0][0]*m[2][2]*m[3][1]
			-  m[0][1]*m[2][0]*m[3][2] +  m[0][0]*m[2][1]*m[3][2];
		ad[3][2] =  m[0][2]*m[1][1]*m[3][0] -  m[0][1]*m[1][2]*m[3][0]
			-  m[0][2]*m[1][0]*m[3][1] +  m[0][0]*m[1][2]*m[3][1]
			+  m[0][1]*m[1][0]*m[3][2] -  m[0][0]*m[1][1]*m[3][2];
		ad[3][3] =  -(m[0][2]*m[1][1]*m[2][0]) +  m[0][1]*m[1][2]*m[2][0]
			+  m[0][2]*m[1][0]*m[2][1] -  m[0][0]*m[1][2]*m[2][1]
			-  m[0][1]*m[1][0]*m[2][2] +  m[0][0]*m[1][1]*m[2][2];
	}
}

#define luel(i, j)  lu[(i)*n+(j)]
#define ael(i, j)	a[(i)*n+(j)]
#define A(i,j)		a[(i)*n+(j)]
#define SQR(x)		((x)*(x))
#define SIGN_FIRST(a,b)	((b) >= 0.0 ? fabs(a) : -fabs(a))

/********************************************************************************
 * LUDecompose() decomposes the coefficient matrix A into upper and lower
 * triangular matrices, the composite being the LU matrix.
 * This is then followed by multiple applications of FELUSolve(),
 * to solve several problems with the same system matrix.
 *
 * 1 is returned if the matrix is non-singular and the decomposition was successful;
 * 0 is returned if the matrix is singular and the decomposition failed.
 ********************************************************************************/
Public long LUDecompose(
	register const float	*a,		/* the (n x n) coefficient matrix */
	register double		*lu, 	/* the (n x n) lu matrix augmented by an (n x 1) pivot sequence */
	register long			n		/* the order of the matrix */
)
{
	register long i, j, k;
	short pivotindex = -1;
	double pivot, biggest, mult, tempf;
	register long *ps;
	union { long *l; double *d; } u;
	double scales[MAXDIM];

	u.d = &lu[n*n];
	ps = u.l; /* Memory for ps[] comes after LU[][] */

	for (i = 0; i < n; i++) {	/* For each row */
		/* Find the largest element in each row for row equilibration */
		biggest = 0.0;
		for (j = 0; j < n; j++)
			if (biggest < (tempf = fabs(luel(i,j) = ael(j,i)))) /* A transposed for row vectors */
				biggest = tempf;
		if (biggest != 0.0)
			scales[i] = 1.0 / biggest;
		else {
			scales[i] = 0.0;
			return(0);	/* Zero row: singular matrix */
		}

		ps[i] = i;		/* Initialize pivot sequence */
	}

	for (k = 0; k < n-1; k++) { /* For each column */
		/* Find the largest element in each column to pivot around */
		biggest = 0.0;
		for (i = k; i < n; i++) {
			if (biggest < (tempf = fabs(luel(ps[i],k)) * scales[ps[i]])) {
				biggest = tempf;
				pivotindex = i;
			}
		}
		if (biggest == 0.0 || pivotindex == -1)
			return(0);	/* Zero column: singular matrix */
		if (pivotindex != k) {	/* Update pivot sequence */
			j = ps[k];
			ps[k] = ps[pivotindex];
			ps[pivotindex] = j;
		}

		/* Pivot, eliminating an extra variable each time */
		pivot = luel(ps[k],k);
		for (i = k+1; i < n; i++) {
			luel(ps[i],k) = mult = luel(ps[i],k) / pivot;
			if (mult != 0.0) {
				for (j = k+1; j < n; j++)
					luel(ps[i],j) -= mult * luel(ps[k],j);
			}
		}
	}
	return(luel(ps[n-1],n-1) != 0.0);	/* 0 if singular, 1 if not */
}	/* Decompose */


/********************************************************************************
 * Solve() solves the linear equation (xA = b) after the matrix A has
 * been decomposed with LUDecompose() into the lower and upper triangular
 * matrices L and U, giving the equivalent equation (xUL = b).
 ********************************************************************************/
Public void LUSolve(
	register const double	*lu,	/* the decomposed LU matrix */
	register const float	*b,		/* the constant vector */
	register float			*x,		/* the solution vector */
	register long			n		/* the order of the equation */
)
{
	register long i, j;
	double dot;
	union { long *l; const double *d; } u;
	register const long *ps;
	
	u.d = &lu[n*n];
	ps = u.l; /* Memory for ps[] comes after LU[][] */

	/* Vector reduction using U triangular matrix */
	for (i = 0; i < n; i++) {
		dot = 0.0;
		for (j = 0; j < i; j++)
			dot += luel(ps[i],j) * x[j];
		x[i] = b[ps[i]] - dot;
	}

	/* Back substitution, in L triangular matrix */
	for (i = n-1; i >= 0; i--) {
		dot = 0.0;
		for (j = i+1; j < n; j++)
			dot += luel(ps[i],j) * x[j];
		x[i] = (x[i] - dot) / luel(ps[i],i);
	}
}	/* LUSolve */


/********************************************************************************
 * CholeskyDecompose
 *
 * Given a positive-definite symmetric matrix in[0,..,n-1][0,..,n-1],
 * this routine constructs its Cholesky decomposition, a[0,..,n-1][0,..,n-1] = L . Lt.
 * On input, only the upper triangle need be given; it is not modified.
 * The Cholesky factor L is returned in the lower triangle of a,
 * except for its diagonal elements which are returned in p[0,..,n-1].
 *
 * The value 1 is returned if the decomposition was successful.
 * The value 0 is returned if the matrix was not positive definite.
 *
 * These routines were derived from "Numerical Methods in C", 2nd ed.
 ********************************************************************************/

Public long CholeskyDecompose(register const float *in, register float *a, float *p, register long n)
{
	long i, j, k;
	register double sum;
	
	if (in != a) /* for out of place operation */
		for (i = 0; i < n; i++) 
			for (j = 0; j < n; j++) 
				A(i,j) = in[(i)*n+(j)];
	
	for (i = 0; i < n; i++) {
		for (j = i; j < n; j++) {
			for (sum = A(i,j), k = i - 1; k >= 0; k--) {
				sum -= A(i,k) * A(j,k);
			}
			if (i == j) {
				if (sum <= 0) {
					return(0);	/* Not positive definite */
				}
				p[i] = sqrt(sum);
			}
			else {
				A(j,i) = sum / p[i];
			}
		}
	}
	return(1);
}


/********************************************************************************
 * CholeskySolve
 *
 * Sovles the set of linear equations A.x=b, where a is a positive-definite
 * symmetric matrix. a[0..n-1][0..n-1] and p[0..n-1] are input as the output of
 * the routine DCholeskyDecomposition. Only the lower triangle of a is accessed.
 * b[0..n-1] is input as the right-hand side vector. The solution vector is
 * returned in x[0..n-1]. a, n, and p are not modified and can be left in place
 * for successive calls with different right-hand sides b. b is not modified
 * unless you identify b and x in the calling sequence, which is allowed.
 ********************************************************************************/

Public void CholeskySolve(const float *a, const float *p, const float *b, float *x, long n)
{
	long i, k;
	register double sum;

	for (i = 0; i < n; i++) {		/* Solve L . y = b, storing y in x */
		for (sum = b[i], k = i-1; k >= 0; k--) {
			sum -= A(i,k) * x[k];
		}
		x[i] = sum / p[i];
	}
	for (i = n-1; i >= 0; i--) {	/* Solve Lt . x = y */
		for (sum = x[i], k = i+1; k < n; k++) {
			sum -= A(k,i) * x[k];
		}
		x[i] = sum / p[i];
	}
}


static float PYTHAG(float a, float b)
{
	float at, bt, ct;
	if ((at = fabs(a)) > (bt = fabs(b))) {
		ct = bt / at;
		return (at * sqrt(1.0 + ct * ct));
	} else if (bt != 0) {
		ct = at / bt;
		return (bt * sqrt(1.0 + ct * ct));
	} else {
		return (0.0);
	}
}


/********************************************************************************
 * SVDecomposition
 *	Converted from Numerical Recipes in C.
********************************************************************************/

Public long SVDecomposition(float *a, float *w, float *v, long m, long n)
{
	int flag, i, its, j, jj, k, l = -1, nm = -1;
	float c, f, h, s, x, y, z, t;
	float anorm = 0.0, g = 0.0, scale = 0.0;
	float * rv1;

	if (m < n) {
		fprintf(stdout, "SVDCMP: You must augment A with extra zero rows");
		return (0);
	}

/*	rv1 = (float *) MM_malloc (n * sizeof(float), "SVD float vector"); */
        rv1 = (float *) malloc(n * sizeof(float));


	for (i = 0; i < n; i++) {
		l = i + 1;
		rv1[i] = scale * g;
		g = s = scale = 0.0;
		if (i < m) {
			for (k = i; k < m; k++)
				scale += fabs(a[k * n + i]);
			if (scale) {
				for (k = i; k < m; k++) {
					a[k * n + i] /= scale;
					s += a[k * n + i] * a[k * n + i];
				}
				f = a[i * n + i];
				g = -SIGN_FIRST(sqrt(s), f);
				h = f * g - s;
				a[i * n + i] = f - g;
				if (i != (n - 1)) {
					for (j = l; j < n; j++) {
						for (s = 0.0, k = i; k < m; k++)
							s += a[k * n + i] * a[k * n + j];
						f = s / h;
						for (k = i; k < m; k++)
							a[k * n + j] += f * a[k * n + i];
					}
				}
				for (k = i; k < m; k++)
					a[k * n + i] *= scale;
			}
		}
		w[i] = scale * g;
		g = s = scale = 0.0;
		if (i < m && i != (n - 1)) {
			for (k = l; k < n; k++)
				scale += fabs(a[i * n + k]);
			if (scale) {
				for (k = l; k < n; k++) {
					a[i * n + k] /= scale;
					s += a[i * n + k] * a[i * n + k];
				}
				f = a[i * n + l];
				g = -SIGN_FIRST(sqrt(s), f);
				h = f * g - s;
				a[i * n + l] = f - g;
				for (k = l; k < n; k++)
					rv1[k] = a[i * n + k] / h;
				if (i != (m - 1)) {
					for (j = l; j < m; j++) {
						for (s = 0.0, k = l; k < n; k++)
							s += a[j * n + k] * a[i * n + k];
						for (k = l; k < n; k++)
							a[j * n + k] += s * rv1[k];
					}
				}
				for (k = l; k < n; k++)
					a[i * n + k] *= scale;
			}
		}
		if (anorm < (t = fabs(w[i]) + fabs(rv1[i])))
			anorm = t;
	}
	for (i = (n - 1); i >= 0; i--) {
		if (i < (n - 1)) {
			if (g) {
				for (j = l; j < n; j++)
					v[j * n + i] = (a[i * n + j] / a[i * n + l]) / g;
				for (j = l; j < n; j++) {
					for (s = 0.0, k = l; k < n; k++)
						s += a[i * n + k] * v[k * n + j];
					for (k = l; k < n; k++)
						v[k * n + j] += s * v[k * n + i];
				}
			}
			for (j = l; j < n; j++)
				v[i * n + j] = v[j * n + i] = 0.0;
		}
		v[i * n + i] = 1.0;
		g = rv1[i];
		l = i;
	}
	for (i = (n - 1); i >= 0; i--) {
		l = i + 1;
		g = w[i];
		if (i < (n - 1))
			for (j = l; j < n; j++)
				a[i * n + j] = 0.0;
		if (g) {
			g = 1.0 / g;
			if (i != (n - 1)) {
				for (j = l; j < n; j++) {
					for (s = 0.0, k = l; k < m; k++)
						s += a[k * n + i] * a[k * n + j];
					f = (s / a[i * n + i]) * g;
					for (k = i; k < m; k++)
						a[k * n + j] += f * a[k * n + i];
				}
			}
			for (j = i; j < m; j++)
				a[j * n + i] *= g;
		} else {
			for (j = i; j < m; j++)
				a[j * n + i] = 0.0;
		}
		++a[i * n + i];
	}
	for (k = (n - 1); k >= 0; k--) {
		for (its = 1; its <= 50; its++) {
			flag = 1;
			for (l = k; l >= 0; l--) {
				nm = l - 1;
				if (fabs(rv1[l]) + anorm == anorm) {
					flag = 0;
					break;
				}
				if (fabs(w[nm]) + anorm == anorm)
					break;
			}
			if (flag) {
				c = 0.0;
				s = 1.0;
				for (i = l; i <= k; i++) {
					f = s * rv1[i];
					if (fabs(f) + anorm != anorm) {
						g = w[i];
						h = PYTHAG(f, g);
						w[i] = h;
						h = 1.0 / h;
						c = g * h;
						s = (-f * h);
						for (j = 0; j < m; j++) {
							y = a[j * n + nm];
							z = a[j * n + i];
							a[j * n + nm] = y * c + z * s;
							a[j * n + i] = z * c - y * s;
						}
					}
				}
			}
			z = w[k];
			if (l == k) {
				if (z < 0.0) {
					w[k] = -z;
					for (j = 0; j < n; j++)
						v[j * n + k] = (-v[j * n + k]);
				}
				break;
			}
			if (its == 50) {
				printf("No convergence in 50 SVDCMP iterations");
				free (rv1);
				return (0);
			}
			x = w[l];
			nm = k - 1;
			y = w[nm];
			g = rv1[nm];
			h = rv1[k];
			f = ((y - z) * (y + z) + (g - h) * (g + h)) / (2.0 * h * y);
			g = PYTHAG(f, 1.0);
			f = ((x - z) * (x + z) + h * ((y / (f + SIGN_FIRST(g, f))) - h)) / x;
			c = s = 1.0;
			for (j = l; j <= nm; j++) {
				i = j + 1;
				g = rv1[i];
				y = w[i];
				h = s * g;
				g = c * g;
				z = PYTHAG(f, h);
				rv1[j] = z;
				c = f / z;
				s = h / z;
				f = x * c + g * s;
				g = g * c - x * s;
				h = y * s;
				y = y * c;
				for (jj = 0; jj < n; jj++) {
					x = v[jj * n + j];
					z = v[jj * n + i];
					v[jj * n + j] = x * c + z * s;
					v[jj * n + i] = z * c - x * s;
				}
				z = PYTHAG(f, h);
				w[j] = z;
				if (z) {
					z = 1.0 / z;
					c = f * z;
					s = h * z;
				}
				f = (c * g) + (s * y);
				x = (c * y) - (s * g);
				for (jj = 0; jj < m; jj++) {
					y = a[jj * n + j];
					z = a[jj * n + i];
					a[jj * n + j] = y * c + z * s;
					a[jj * n + i] = z * c - y * s;

				}
			}
			rv1[l] = 0.0;
			rv1[k] = f;
			w[k] = x;
		}
	}
	free(rv1);
	return (1);
}


/*******************************************************************************
 * QRDecomposition
 *	Adapted from Numerical Recipes in C.
 *	Constructs the QR decomposition of a[n][n].
 *	The upper triangular matrix R is returned in the upper triangle of A,
 *	except for the diagonal elements or R, which are returned in d[n].
 *	The orthogonal matrix Q is represented as a product of n-1 Householder
 *	matrices Q0...Qn-2, where Qj = I - uk X uj / cj. The ith component of uj
 *	is zero for i = 0..j-2 while the nonzero components are returned in a[i][j]
 *	for i = j,...,n-1. The returned value is 0 if a singularity was encountered
 *	during the decomposition, but the decomposition is still completed in this case;
 *	otherwise it returns as 1.
*******************************************************************************/

Public long QRDecomposition(float *a, float *c, float *d, long n)
{
	long i, j, k, n1;
	float scale = 0.0, sigma, sum, tau, t;
	long ok;

	ok = 1;
	n1 = n - 1;

	for (k = 0; k < n1; k++) {
		for (i = k; i <= n1; i++)
			if ((t = fabs(A(i, k))) > scale)
				scale = t;
		if (scale == 0.0) {
			ok = 0;
			c[k] = d[k] = 0.0;
		} else {
			for (i = k; i <= n1; i++)
				A(i, k) /= scale;
			for (sum = 0.0, i = k; i <= n1; i++)
				sum += SQR(A(i, k));
			sigma = SIGN_FIRST(sqrt(sum), A(k, k));
			A(k, k) += sigma;
			c[k] = sigma * A(k, k);
			d[k] = -scale * sigma;
			for (j = k + 1; j <= n1; j++) {
				for (sum = 0.0, i = k; i <= n1; i++)
					sum += A(i, k) * A(i, j);
				tau = sum / c[k];
				for (i = k; i <= n1; i++)
					A(i, j) -= tau * A(i, k);
			}
		}
	}
	d[n1] = A(n1, n1);

	if (d[n1] == 0.0)
		ok = 0;

	return (ok);
}


/*******************************************************************************
 * RSolve
*******************************************************************************/

Public void RSolve(const float *a, const float *d, float *b, long n)
{
	long i, j, n1;
	float sum;
	n1 = n - 1;

	b[n1] /= d[n1];
	for (i = n - 2; i >= 0; i--) {
		for (sum = 0.0, j = i + 1; j < n; j++)
			sum += A(i, j) * b[j];
		b[i] = (b[i] - sum) / d[i];
	}
}


/*******************************************************************************
 * QRSolve
*******************************************************************************/

Public void QRSolve(const float *a, const float *c, const float *d, float *b, long n)
{
	long i, j, n1;
	float sum, tau;

	n1 = n - 1;

	for (j = 0; j < n1; j++) {
		for (sum = 0.0, i = j; i <= n1; i++)
			sum += A(i, j) * b[i];
		tau = sum / c[j];
		for (i = j; i <= n1; i++)
			b[i] -= tau * A(i, j);
	}
	RSolve(a, d, b, n);
}

#if 0
/*******************************************************************************
 * QRMatrices
 *	Convert from (a, c, d) form to (q, r) form
*******************************************************************************/
#define Q(i,j)		q[(i)*n+(j)]
#define QT(i,j)		qt[(i)*n+(j)]
#define R(i,j)		r[(i)*n+(j)]

Public void QRMatrices(const float *a, const float *c, const float *d, float *q, float *r, float *qt, long n)
{
	long i, j, k, l;
	float con;
	
	if (c) ;	/* Get rid of "unused" messages */
	
	/* find the Q and R matrices */
	for (k = 0; k < n; k++) {
		for (l = 0; l < n; l++) {
			if (l > k) {
				R(k,l) = A(k,l);
				Q(k,l) = 0.0;
			} else if (l < k) {
				R(k,l) = Q(k,l) = 0.0;
			} else {
				R(k,l) = d[k];
				Q(k,l) = 1.0;
			}
		}
	}
	for (i = n - 2; i >= 0; i--) {
		for (con = 0.0, k = i; k < n; k++)
			con += A(k,i) * A(k,i);
		con /= 2.0;
		for (k = i; k < n; k++) {
			for (l = i; l < n; l++) {
				QT(k,l) = 0.0;
				for (j = i; j < n; j++) {
					QT(k,l) += Q(j,l) * A(k,i) * A(j,i) / con;
				}
			}
		}
		for (k = i; k < n; k++)
			for (l = i; l < n; l++)
				Q(k,l) -= QT(k,l);
	}
}

#undef Q
#undef QT
#undef R
#endif

/********************************************************************************
 * LinearTransform
 * Linear transformations, for transforming vectors and matrices.
 * This works for row vectors and column vectors alike.
 *	L[nRows][lCol]	- input (left) matrix
 *	rg[lCol][rCol]	- transformation (right) matrix
 *	P[nRows][rCol]	- output (product) matrix
 *
 * Examples:
 * v[3] * M[3][3] -> w[3] :			MLLinearTransform(&v[0], &M[0][0], &w[0], 1, 3, 3);
 * M[3][3] * v[3] -> w[3] :			MLLinearTransform(&M[0][0], &v[0], &w[0], 3, 3, 1);
 * M[4][4] * N[4][4] -> P[4][4]:	MLLinearTransform(&M[0][0], &N[0][0], &P[0][0], 4, 4, 4);
 * v[4] * M[4][3] -> w[3]:			MLLinearTransform(&v[0], &M[0][0], &w[0], 1, 4, 3);
 * v[3] tensor w[3] -> T[3][3]:		MLLinearTransform(&v[0], &w[0], T[3][3], 3, 1, 3);
 * This can be used In Place, i.e., 
 * to transform the left matrix
 * by the right matrix, placing the result back in the left.  By its nature,
 * then, this can only be used for transforming row vectors or concatenating
 * matrices from the right.
 ********************************************************************************/

Public void
LinearTransform(
  const float    *L,    /* The left matrix */
  const float    *R,    /* The right matrix */
  register float  *P,    /* The resultant matrix */
  long      nRows,  /* The number of rows of the left and resultant matrices */
  long      lCol,  /* The number of columns in the left matrix */
  long      rCol  /* The number of columns in the resultant matrix */
)
{
  register const float *lp;    /* Left matrix pointer for dot product */
  register AnythingT rp;    /* Right matrix pointer for dot product */
  register long k;	/* Loop counter */
  register double sum;      /* Extended precision for intermediate results */
  register long rowBytes = lCol * sizeof(float);
  register long rRowBytes = rCol * sizeof(float);
  register long j, i;	/* Loop counters */
  register long lRowBytes = lCol * sizeof(float);
  AnythingT lb;
  float temp[MAXDIM*MAXDIM]; // Temporary storage for in-place transformations 
  register float *tp;
  
  lb.cc = (const char*) L;
  if (P == L) {  // IN PLACE
    float *op = P;	/* Output geometry */
    for (i = nRows; i--; lb.c += rowBytes) {  /* Each row in L */
      {  
	for (k = lCol, lp = lb.f, tp = &temp[0]; k--; )
	  *tp++ = *lp++;      /* Copy one input vector to temp storage */
      }
      for (j = 0; j < lCol; j++) {    /* Each column in R */
	lp = &temp[0];	/* Left of ith row of L */
	rp.cc = (const char *)(R + j);  /* Top of jth column of R */
	sum = 0;
	for (k = lCol; k--; rp.c += rowBytes)
	  sum += *lp++ * (*(rp.f));  /* *P += L[i'][k'] * R[k'][j] */
	*op++ = sum;
      }
    }
  } else if (P != R) {
    for (i = nRows; i--; lb.c += lRowBytes) {  /* Each row in L */
      for (j = 0; j < rCol; j++) {  /* Each column in R */
	lp = (const float *) lb.f;    /* Left of ith row of L */
	rp.cc = (const char *)(R + j);  /* Top of jth column of R */
	sum = 0;
	for (k = lCol; k--; rp.c += rRowBytes)
	  sum += *lp++ * (*(rp.f));  /* *P += L[i'][k'] * R[k'][j] */
	*P++ = sum;
      }
    }
  } else { // P == R
    for (tp = temp, i = lCol * rCol; i--; ) *tp++ = *R++;  // copy R
    for (i = nRows; i--; lb.c += lRowBytes) {  /* Each row in L */
      for (j = 0; j < rCol; j++) {  /* Each column in R */
	lp = (const float *)lb.f;    /* Left of ith row of L */
	rp.cc = (const char *)(temp + j);  /* Top of jth column of R (now in temp) */
	sum = 0;
	for (k = lCol; k--; rp.c += rRowBytes)
	  sum += *lp++ * (*(rp.f));  /* *P += L[i'][k'] * R[k'][j] */
	*P++ = sum;
      }
    }
  } 
}


/********************************************************************************
 * AffineTransform
 * Affine transformations, for transforming points in row vector form.
 *	L[nRows][lCol]			- input
 *	R[lCol+1][rCol]	- matrix
 *	P[nRows][rCol]			- output
 *
 * Examples:
 * p[3] * M[4][3] -> q[3]:	MLAffineTransform(&p[0], &M[0][0], &q[0], 1, 3, 3);
 * Can transform IN PLACE if necessary
 ********************************************************************************/

Public void
AffineTransform(
  const float    *L,    /* The left matrix */
  const float    *R,    /* The right matrix */
  register float  *P,    /* The product matrix */
  long      nRows,  /* The number of rows of the left and resultant matrices */
  long      lCol,  /* The number columns in the left and rows in the right matrix */
  long      rCol  /* The number of columns in the resultant matrix */
)
{
  register const float *lp;    /* Left matrix pointer for dot product */
  register AnythingT rp;    /* Right matrix pointer for dot product */
  register long k;	/* Loop counter */
  register double sum;      /* Extended precision for intermediate results */
  register long rRowBytes = rCol * sizeof(float);
  register long j, i;	/* Loop counters */
  register long lRowBytes = lCol * sizeof(float);
  AnythingT lb;

  lb.cc = (const char *) L;
  if (P == L) {  // IN PLACE
    float temp[MAXDIM];      /* Temporary storage for in-place transformations */
    register long rowBytes = lCol * sizeof(float);
    float *op = P;	/* Output geometry */

    for (i = nRows; i--; lb.c += rowBytes) {  /* Each row in L */
      {  register float *tp;
	for (k = lCol, lp = lb.f, tp = &temp[0]; k--; )
	  *tp++ = *lp++;  /* Copy one input vector to temp storage */
      }
      for (j = 0; j < lCol; j++) {    /* Each column in R */
	register AnythingT rrp;  /* Matrix pointer for dot product */
	lp = &temp[0];	/* Left of ith row of L */
	rrp.cc = (const char *)(R + j);  /* Top of jth column of R */
	sum = 0;
	for (k = lCol; k--; rrp.c += rowBytes)
	  sum += *lp++ * (*(rrp.f));  /* *P += L[i'][k'] * R[k'][j] */
	*op++ = sum + (*(rrp.f));    /* Add translation at the end */
      }
    } 
  } else {
    for (i = nRows; i--; lb.c += lRowBytes) {  /* Each row in L */
      for (j = 0; j < rCol; j++) {  /* Each column in R */
	lp = (const float *) lb.f;    /* Left of ith row of L */
	rp.cc = (const char *)(R + j);  /* Top of jth column of R */
	sum = 0;
	for (k = lCol; k--; rp.c += rRowBytes)
	  sum += *lp++ * (*(rp.f));  /* *P += L[i'][k'] * R[k'][j] */
	*P++ = sum + (*(rp.f));      /* Add translation at the end */
      }
    }
  }
}


/********************************************************************************
 * FrameTransform
 * Frame transformations, for transforming frames (i.e. (N+1)xN matrices).
 *	L[nFrames][lCol+1][lCol] 	- input
 *	R[lCol+1][rCol]				- matrix
 *	P[nFrames][lCol+1][rCol]	- output
 ********************************************************************************/

Public void
FrameTransform(
  const float    *L,      /* The left matrix */
  const float    *R,      /* The right matrix */
  register float  *P,      /* The resultant matrix */
  long      nFrames,  /* The number of frames to transform */
  long      lCol,    /* The number columns in the left matrix */
  long      rCol    /* The number of columns in the resultant matrix */
)
{
  register const float *lp;    /* Left matrix pointer for dot product */
  register AnythingT rp;    /* Right matrix pointer for dot product */
  register long k;	/* Loop counter */
  register double sum;      /* Extended precision for intermediate results */
  register long rRowBytes = rCol * sizeof(float);
  register long j, i;	/* Loop counters */
  register long lRowBytes = lCol * sizeof(float);
  AnythingT lb;

  lb.cc = (const char*)L;
  while (nFrames--) {
    /* Transform the linear part */
    for (i = lCol; i--; lb.c += lRowBytes) {  /* Each row in L */
      for (j = 0; j < rCol; j++) {  /* Each column in R */
	lp = (const float *) lb.f;    /* Left of ith row of L */
	rp.cc = (const char *)(R + j);  /* Top of jth column of R */
	sum = 0;
	for (k = lCol; k--; rp.c += rRowBytes)
	  sum += *lp++ * (*(rp.f));  /* *P += L[i'][k'] * R[k'][j] */
	*P++ = sum;
      }
    }
    /* Transform the affine part */
    for (j = 0; j < rCol; j++) {  /* Each column in R */
      lp = (const float *) lb.f;    /* Left of ith row of L */
      rp.cc = (const char *)(R + j);  /* Top of jth column of R */
      sum = 0;
      for (k = lCol; k--; rp.c += rRowBytes)
	sum += *lp++ * (*(rp.f));  /* *P += L[i'][k'] * R[k'][j] */
      *P++ = sum + (*(rp.f));      /* Add translation at the end */
    }
    lb.c += lRowBytes;  /* Bump the row pointer past this affine part */
  }
}

Public void
SubtractMatrix (float *A, float *B, float *C, long m, long n)
{
  register long i, index = m * n;
  for (i = 0; i < index; i++)
    C[i] = A[i] - B[i];
}

/* A * B + C = D */

Public void
LinearAddMatrix (float A, float *B, float *C, float *D, long m, long n)
{
  register long i, index = m * n;
  for (i = 0; i < index; i++)
    D[i] = A * B[i] + C[i];
}
