#define _DEBUG

#include <iostream>
#include <iomanip>
using namespace std;

#include <stdio.h>

#include "DoubleVector.h"  // Added by IGOR 11/06/2013
                           // New version of g++ will not compile w/o this.

#ifndef  __DoubleMatrix__
#define  __DoubleMatrix__
//
//######################################################################
//                        DoubleMatrix.cpp
//######################################################################
//
// (i,j) access : Indexing starts at i = 1 and j = 1
//
// 
// ((
//  Internally : The matrix data is allocated as a one dimensional
//  contiguous array, with the data being stored by rows (C convention).
//  This matrix data is accessed using two dimensional C 
//  array structure ... so any internal indexing starts at 0.
// ))
//
//#####################################################################
// Chris Anderson                                       Jan. 18, 2004
//#####################################################################
//
//
/*
#############################################################################
#
# Copyright 2015 Chris Anderson
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the Lesser GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# For a copy of the GNU General Public License see
# <http://www.gnu.org/licenses/>.
#
#############################################################################
*/

//class DoubleVector;  // Added by IGOR 11/06/2013
                       // Need either this or #include "DoubleVector.h"
                       // above.


class DoubleMatrix
{
    friend class  DoubleVector;

    public :

    long                   rows;
    long                 columns;

    public :

    DoubleMatrix();
    DoubleMatrix(const DoubleMatrix& P);

    DoubleMatrix(long m, long n);               // m x n matrix with  0's
    DoubleMatrix(long m, long n, double* d);    // m x n matrix with data d
    DoubleMatrix(long m, long n, float* d);     // Igor

//
//    Initialize
//
    void initialize();
    void initialize(const DoubleMatrix& M);
    void initialize(long m, long n);
    void initialize(long m, long n, double* data);
    void initialize(long m, long n, float* data);   // Igor


    ~DoubleMatrix();

    // Element access

    double& operator()(long i, long j);              // access A(i,j)
    const double& operator()(long i, long j) const;  // access A(i,j)

    // Algebraic operations
     
    void operator =(const DoubleMatrix& M);
    DoubleMatrix operator -();

    DoubleMatrix operator+(const DoubleMatrix& M);
    DoubleMatrix operator-(const DoubleMatrix& M);
    DoubleMatrix operator*(const DoubleMatrix& M);

    // scalar  operations

    DoubleMatrix operator*(const double d);
    DoubleMatrix operator/(const double d);

    friend DoubleMatrix operator*(double d, const DoubleMatrix& M);
   
    // vector operations

    DoubleVector operator*(const DoubleVector& V);     // M*V
    DoubleVector applyInverse(const DoubleVector& V);  // M^(-1)*V
 
//  Input/Output
 
    friend ostream&  operator <<(ostream& outStream, const DoubleMatrix& A);

    public :

    double**       matrixValues;
    double*          matrixData;

    // utility routine for solving linear systems

    void  SolveLinearSystem(double* MdataPtr, long M, long N, 
    double* BdataPtr, long P);

    static void dimensionError();
};
#endif
  

  
 
