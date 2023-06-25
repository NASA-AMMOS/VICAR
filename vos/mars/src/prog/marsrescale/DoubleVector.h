#include <iostream>
#include <iomanip>
using namespace std;

#include <stdio.h>
#ifndef  __DoubleVector__
#define  __DoubleVector__
//
//######################################################################
//                        DoubleVector.h
//######################################################################
//
// A Double vector class that stores a rectangular array of double 
// values. Indexing of of the elements with the (..) operator 
// starts at i = 1 
//
//#####################################################################
// Chris Anderson                                       Jan. 16, 2004
//#####################################################################
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
class DoubleVector
{
    friend class  DoubleMatrix;

    public :

    long                  size;

    public :

    DoubleVector();
    DoubleVector(const DoubleVector& V);

    DoubleVector(long n);               // n vector with  0's
    DoubleVector(long n, double* d);    // n vector with data d

    ~DoubleVector();
//
//  Initialize
//
    void initialize();
    void initialize(const DoubleVector& V);
    void initialize(long n);
    void initialize(long n, double* data);

    double& operator()(long i);                 // access v(i)
    const double& operator()(long i) const;     // access v(i)

//  Vector operations 

    void operator=(const DoubleVector& V);   
    DoubleVector operator-();  

    DoubleVector operator+(const DoubleVector& V);
    DoubleVector operator-(const DoubleVector& V);

    // scalar  operations

    DoubleVector operator*(const double d);
    DoubleVector operator/(const double d);

    friend DoubleVector operator*(const double d, const DoubleVector& V);


    void operator*=(double alpha);           // incremental scalar multiplication 

    void operator+=(const DoubleVector& V);  // incremental vector addition

    double dot(DoubleVector& V);             // dot product 

//  Input/Output

    friend ostream&  operator <<(ostream& outStream, const DoubleVector& V);

    public :

    double*          vectorData;
    static void dimensionError();
};
#endif
  

  
 
