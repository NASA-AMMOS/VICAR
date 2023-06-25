// matrix.C 1.5 02/07/10 15:02:52
/** \file
 ** 4x4 Matrix math
 **/
#include "grape/matrix.h"

/// Multiply "point" by "matrix" and put outcome in "result".
/****************************************************************************
*
* Parameters: zpoint : (input to function) An x,y,z coordinate.
*             matrix : (input to function) The matrix to muliply point by.
*             result : (output of function) An x,y,z coordinate.
*
* Return value: result
*
* Function method (description): Multiply a point vector by the matrix.
*                                Assumes the homogeneous coordinate of
*                                the point (the w in x,y,z,w) is 1.
*
* Created by: Zareh Gorjian
*
* Bugs, possible improvements: Can make it a macro.
*
*****************************************************************************/
void MultPoints( double zpoint[], ZMatrix matrix, double result[])
{

   result[0] = zpoint[0] * matrix[0][0] + 
               zpoint[1] * matrix[0][1] + 
               zpoint[2] * matrix[0][2] + matrix[0][3];
   result[1] = zpoint[0] * matrix[1][0] + 
               zpoint[1] * matrix[1][1] + 
               zpoint[2] * matrix[1][2] + matrix[1][3];
   result[2] = zpoint[0] * matrix[2][0] + 
               zpoint[1] * matrix[2][1] + 
               zpoint[2] * matrix[2][2] + matrix[2][3];

} /* MultPoints */

/// Matrix pre-multiplication
/****************************************************************************
*
* Function purpose: Pre-multiply "trafo" with "m" and put result in "trafo".
*                   The resultant matrix will apply "m" 1st then "trafo".
*
* Parameters: trafo : (input to function and output of function) trafo is
*                     a 4x4 matrix, which will be combined with m and the
*                     result will be put back into trafo thus destroying
*                     it's original values.
*             m     : (input to function) matrix to multiply with trafo.
*
* Return value: trafo
*
* Created by: Zareh Gorjian
*
*****************************************************************************/
void MatPreMult( ZMatrix trafo, ZMatrix m)
{
   ZMatrix  h;
   register int    i,j,k;
   double   sum;

   /*
   ** Store trafo into matrix h
   */
   MatCopy(trafo, h);

   /*
   ** Multiply matrix m and h and store result in matrix trafo
   */ 
   for(i=0 ; i<DIM_ZMAT ; i++)
      for(j=0 ; j<DIM_ZMAT ; j++)
	 {
         sum = 0.0;
         for (k=0 ; k<DIM_ZMAT ; k++)
            sum += h[j][k] * m[k][i];
         trafo[j][i] = sum;
         } /* for i,j */

}

/// Matrix post-multiplication
/****************************************************************************
*
* Function purpose: Post-multiply "trafo" with "m" and put result in "trafo".
*                   The resultant matrix will apply "trafo" then "m".
*
* Parameters: trafo : (input to function and output of function) trafo is
*                     a 4x4 matrix, which will be combined with m and the
*                     result will be put back into trafo thus destroying
*                     its original values.
*             m     : (input to function) matrix to multiply with trafo.
*
* Return value: trafo
*
*****************************************************************************/
void MatPostMult( ZMatrix trafo, ZMatrix m)
{
   ZMatrix  h;
   register int    i,j,k;
   double   sum;

   // Store trafo into matrix h
   MatCopy(trafo, h);

   // Multiply matrix m and h and store result in matrix trafo
   for(i=0 ; i<DIM_ZMAT ; i++)
      for(j=0 ; j<DIM_ZMAT ; j++)
	 {
         sum = 0.0;
         for (k=0 ; k<DIM_ZMAT ; k++)
            sum += m[j][k] * h[k][i];
         trafo[j][i] = sum;
         } /* for i,j */
}

/// Create rotation transform matrix from roll, pitch, and yaw
/****************************************************************************
*
* Function purpose: To make a matrix which will rotate the camera around 
*                   the sphere to the given roll, latitude and longitude. 
*
* Parameters: tmpMatrix : (output of function) This matrix will rotate the
*                         camera by the desired amount.
*             roll      : (input to function) The amount to roll. (in degrees)
*             pitch     : (input to function) The amount to pitch. (in degrees)
*             yaw       : (input to function) The amount to yaw. (in degrees)
*
* Return value: tmpMatrix
*
* Function method (description): Right handed coordinate system with Z up.
*                          Roll is rotation around the X axis.
*                          Pitch is rotation around the Y axis.
*                          Yaw is rotation around the Z axis.
*
* Created by: Zareh Gorjian
*
* Note:  This computes [RotMatrix] := [Rz] . [Ry] . [Rx]
*
*****************************************************************************/
void MakeRotationMatrix( ZMatrix tmpMatrix, 
				double roll, double pitch, double yaw)
{
   ZMatrix rotMatrix;            
   double  cosineAngle;
   double  sineAngle;

   /*
   ** Initialize the rotation matricies.
   */
   MatIdent(tmpMatrix);
   MatIdent(rotMatrix);

   sineAngle = sin(roll*TORADIANS);
   cosineAngle = cos(roll*TORADIANS);
   /* 
   ** Roll.
   ** Now put the cosineAngle and sineAngle into the rotation matrix
   ** for rotation around the X axis. Done for rotating the up vector.
   ** The look vector is not affected by this rotation.
   */
   tmpMatrix[1][1] = cosineAngle;
   tmpMatrix[1][2] = -sineAngle;
   tmpMatrix[2][1] = sineAngle;
   tmpMatrix[2][2] = cosineAngle;

   sineAngle = sin(pitch*TORADIANS);
   cosineAngle = cos(pitch*TORADIANS);
   /*
   ** Pitch.
   ** Now put the cosineAngle and sineAngle into the rotation matrix
   ** for rotation around the Y axis. 
   */
   rotMatrix[0][0] = cosineAngle;
   rotMatrix[0][2] = sineAngle;
   rotMatrix[2][0] = -sineAngle;
   rotMatrix[2][2] = cosineAngle;
   MatMult(rotMatrix,tmpMatrix); /* combine two rotations put result in rotMatrix */

   sineAngle = sin(yaw*TORADIANS);
   cosineAngle = cos(yaw*TORADIANS);
   /*
   ** Yaw.
   ** Now put the cosineAngle and sineAngle into the rotation matrix
   ** for rotation around the Z axis.
   */ 
   MatIdent(tmpMatrix);		/* clear it from the previous use */
   tmpMatrix[0][0] = cosineAngle;
   tmpMatrix[0][1] = -sineAngle;
   tmpMatrix[1][0] = sineAngle;
   tmpMatrix[1][1] = cosineAngle;
   MatMult(tmpMatrix,rotMatrix); /* combine with rotation derived above */

} /* MakeRotationMatrix */

/// Create transform matrix to perform the passed translation
void MakeTranslationMatrix( ZMatrix MatInOut, double x, double y, double z)
{
	MatIdent(MatInOut);		/* init with identity matrix */

	MatInOut[0][3] = x;		/* x translation */
	MatInOut[1][3] = y;		/* y translation */
	MatInOut[2][3] = z;		/* z translation */
}

/// Create transform matrix to perform the passed scaling
void MakeScaleMatrix( ZMatrix MatInOut, double xs, double ys, double zs)
{
	MatIdent(MatInOut);		/* init with identity matrix */

	MatInOut[0][0] = xs;		/* x scale */
	MatInOut[1][1] = ys;		/* y scale */
	MatInOut[2][2] = zs;		/* z scale */
}

/// Matrix inversion
/**
   Get the inverse of a matrix via the Simplex method.   Copied this 
   routine from some example OpenGL code and modified for our matrix
   type.

   Note: the scant comments are theirs, not mine (djf)

   Returns: 0 if non-invertible, 1 otherwise
*/
int MatInvert(ZMatrix src, ZMatrix inverse)
{
    int i, j, k, swap;
    double t;
	ZMatrix temp;
    
    MatCopy(src, temp);
    MatIdent(inverse);
    
    for (i = 0; i < DIM_ZMAT; i++) {
        /* 
         ** Look for largest element in column */
        swap = i;
        for (j = i + 1; j < DIM_ZMAT; j++) {
            if (fabs(temp[j][i]) > fabs(temp[i][i])) {
                swap = j;
            }
        }
        
        if (swap != i) {
            /* 
             ** Swap rows. */
            for (k = 0; k < DIM_ZMAT; k++) {
                t = temp[i][k];
                temp[i][k] = temp[swap][k];
                temp[swap][k] = t;
                
                t = inverse[i][k];
                inverse[i][k] = inverse[swap][k];
                inverse[swap][k] = t;
            }
        }
        if (temp[i][i] == 0) {
            /* 
             ** No non-zero pivot.  The matrix is singular, which
             shouldn't ** happen.  This means the user gave us a
             bad matrix. */
            return 0;			/* failure */
        }
        t = temp[i][i];
        for (k = 0; k < DIM_ZMAT; k++) {
            temp[i][k] /= t;
            inverse[i][k] /= t;
        }
        for (j = 0; j < DIM_ZMAT; j++) {
            if (j != i) {
                t = temp[j][i];
                for (k = 0; k < DIM_ZMAT; k++) {
                    temp[j][k] -= temp[i][k] * t;
                    inverse[j][k] -= inverse[i][k] * t;
                }
            }
        }
    }
    return 1;					/* success */
}

/// Transpose Matrix src to matrix transpose (may be the same matrix)
void MatTranspose(ZMatrix src, ZMatrix transpose)
{
	register int i, j;
	if (transpose == src) {		// transpose to self
		ZMatrix temp;		// make a copy
		MatCopy(src, temp);
		src = temp;		// point to copy
	}
	for (i=0; i<DIM_ZMAT; i++) 
		for (j=0; j<DIM_ZMAT; j++) 
			transpose[j][i] = src[i][j];
}

/// Dump matrix for debug 
void MatDump(FILE *fp, const char *name, ZMatrix m)
{
	for (int i=0; i<DIM_ZMAT; i++) {
		for (int j=0; j<DIM_ZMAT; j++)
			fprintf(fp, "%s[%d][%d]=%8.4f  ",
					name, i, j, m[i][j]);
		putc('\n', fp);
	}
}
