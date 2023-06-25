#ifndef _MATRIX_H_
#define _MATRIX_H_
// matrix.h 1.9 02/07/10 12:48:10
/** \file
 ** 3x3 Matrix math
 **/

#include <stdio.h>
#include <math.h>
#include <string.h>

#define TORADIANS  0.017453293	/* PI/180 to convert to radians */
#define TODEGREES  57.29578	/* 180/PI to convert to degrees */

#define DIM_ZMAT    4		/* dimensions */

typedef double ZMatrix[DIM_ZMAT][DIM_ZMAT]; /* Define a matrix type */

/// Set a matrix to all zeros
inline void MatClear(ZMatrix m) {
	memset(m, 0, sizeof(ZMatrix));
}

/// Set matrix to identity 
inline void MatIdent(ZMatrix m) {
	MatClear(m);
	m[0][0] = m[1][1] = m[2][2] = m[3][3] = 1;
}

/// Copy matrix msrc to matrix mdest
inline void MatCopy(ZMatrix src, ZMatrix dest) {
	memcpy(dest, src, sizeof(ZMatrix));
}

// old name
#define MatMult(t,m)	MatPreMult(t,m)

void MultPoints(double zpoint[3], ZMatrix matrix, double result[3]);
void MatPreMult(ZMatrix trafo, ZMatrix m);
void MatPostMult(ZMatrix trafo, ZMatrix m);
void MakeRotationMatrix(ZMatrix tmpMatrix, 
		double roll, double pitch, double yaw);
void MakeTranslationMatrix(ZMatrix MatInOut, double x, double y, double z);
void MakeScaleMatrix(ZMatrix MatInOut, double xs, double ys, double zs);
int  MatInvert(ZMatrix src, ZMatrix inverse);
void MatTranspose(ZMatrix src, ZMatrix transpose);
void MatDump(FILE *fp, const char *name, ZMatrix m);

#endif
