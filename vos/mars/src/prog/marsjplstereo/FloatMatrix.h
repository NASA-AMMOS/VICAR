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

#ifndef __FLOAT_MATRIX__
#define __FLOAT_MATRIX__

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

void IdentityMatrix(float *m, long nRows, register long nCols);

void TransposeMatrix(register const float *from, 
		register float *to, long nRows, register long nCols);
		
float NormMatrix(register const float *g0, long height, register long width);

float DeterminantMatrix(register const float *a, register long n);

long InvertMatrix(const float *M, float *Minv, long nRows, register long n);

void ProjectiveTransformVectorMatrix(const float *x, const float *A, 
			float *out, long nRows, long nCols);
			
void ProjectiveTransformMatrixVector(const float *A, const float *x, 
			float *out, long nRows, long nCols);
			
void ReorthogonalizeMatrix(float *R, long n, long limit);

void AdjointMatrix2x2(const float* M, float *N);

long InvertMatrix2x2(const float* M, float *Minv);

float L2NormMatrix2x2(const float* M);

void AdjointMatrix3x3(float *M, float *out);

void RightInverseMatrix3x4(const float *m, float *inv);

void LeftInverseMatrix4x3(const float *m, float *inv);

void AdjointMatrix4x4(float *M, float *out);

long LUDecompose(register const float	*a,		/* the (n x n) coefficient matrix */
				register double		*lu, 	/* the (n x n) lu matrix augmented by an (n x 1) pivot sequence */
				register long			n		/* the order of the matrix */
				);
				
void LUSolve(
	register const double	*lu,	/* the decomposed LU matrix */
	register const float	*b,		/* the constant vector */
	register float			*x,		/* the solution vector */
	register long			n		/* the order of the equation */
);

long CholeskyDecompose(register const float *in, 
			register float *a, float *p, register long n);
			
void CholeskySolve(const float *a, const float *p, const float *b, float *x, long n);

long SVDecomposition(float *a, float *w, float *v, long m, long n);

long QRDecomposition(float *a, float *c, float *d, long n);

void RSolve(const float *a, const float *d, float *b, long n);

void QRSolve(const float *a, const float *c, const float *d, float *b, long n);

void QRMatrices(const float *a, const float *c, const float *d, float *q, 
		float *r, float *qt, long n);

void LinearTransform(
	const float	*L,		/* The left matrix */
	const float	*R,		/* The right matrix */
	register float	*P,		/* The resultant matrix */
	long	nRows,	/* The number of rows of the left and resultant matrices */
	long	lCol,	/* The number of columns in the left matrix */
	long	rCol	/* The number of columns in the resultant matrix */
);

void
AffineTransform(
	const float		*L,		/* The left matrix */
	const float		*R,		/* The right matrix */
	register float	*P,		/* The product matrix */
	long			nRows,	/* The number of rows of the left and resultant matrices */
	long			lCol,	/* The number columns in the left and rows in the right matrix */
	long			rCol	/* The number of columns in the resultant matrix */
);
void
FrameTransform(
	const float		*L,			/* The left matrix */
	const float		*R,			/* The right matrix */
	register float	*P,			/* The resultant matrix */
	long			nFrames,	/* The number of frames to transform */
	long			lCol,		/* The number columns in the left matrix */
	long			rCol		/* The number of columns in the resultant matrix */
);

void
SubtractMatrix (float *A, float *B, float *C, long m, long n);

void
LinearAddMatrix (float A, float *B, float *C, float *D, long m, long n);

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif
