#include <iostream>
#include <iomanip>
using namespace std;

#include "DoubleVector.h"

#include <stdio.h>
#include <stdlib.h>
//
//######################################################################
//                        DoubleVector.cpp
//######################################################################
//
// (*) access indexing starts at i = 1 
//
// ((
// Internally, the vector data is stored as a standard C array, so it's
// indexing starts at 0. 
// ))
//
//#####################################################################
// Chris Anderson                                    Jan. 16, 2004
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
DoubleVector::DoubleVector()
{
    vectorData   = 0;
    size         = 0;
}

DoubleVector::DoubleVector(const DoubleVector& V)
{
    vectorData   = 0;
    size         = 0;
    initialize(V);
}

DoubleVector::DoubleVector(long n)
{
    vectorData   = 0;
    size         = 0;
    initialize(n);
}

DoubleVector::DoubleVector(long n, double* data)
{
    vectorData   = 0;
    size         = 0;
    initialize(n,data);
}

DoubleVector::~DoubleVector()
{
    if(vectorData != 0)
    {
    delete [] vectorData;
    }
}
//
//    Initialize
//
void DoubleVector::initialize()
{
    // delete old data

    if(vectorData != 0)
    {
    delete [] vectorData;
    }

    // reset to null

    vectorData   = 0;
    size         = 0;
}

void DoubleVector::initialize(const DoubleVector& V)
{
    long i; 

    if(vectorData != 0)
    {initialize();}

    if(V.vectorData != 0)
    {
    initialize(V.size);
    }
    // deep copy the data

    for (i = 0; i < size;    i++)
    {
        vectorData[i] = V.vectorData[i];
    }
}

void DoubleVector::initialize(long n)
{
    
    long i;  

//  delete old data

    if(vectorData != 0)
    {initialize();}

// create new vector

    size    = n;
//
//  dynamically construct n vector
//
    vectorData   = new double[size];
//
//  initalize with 0's
//
    for (i = 0; i < size; i++)
    {
     vectorData[i] = 0.0;
    }
}

void DoubleVector::initialize(long n, double* data)
{
// delete old data

    long i;  

    if(vectorData != 0)
    {initialize();}

// create new vector

    size    = n;
//
//  dynamically construct an n vector
//
    vectorData   = new double[size];
//
//  initalize with data
//
    for (i = 0; i < size;    i++)
    {
     vectorData[i]= data[i];
    }

}

double& DoubleVector::operator()(long i)
{
#ifdef _DEBUG 
    if((i < 1)||(i>  size))
    {
        cerr << " Illegal index for DoubleVector Access  " << endl;
        exit(-1);
     }
#endif
    return vectorData[i-1];
}

const double& DoubleVector::operator()(long i) const
{
#ifdef _DEBUG 
    if((i < 1)||(i> size))
    {
        cerr << " Illegal index for DoubleVector Access  " << endl;
        cerr << " Hit return to exit " << endl;
        exit(-1);
     }
#endif
    return vectorData[i-1];
}

void DoubleVector::operator=(const DoubleVector& V)
{
    long i; 

    // create new vector if left hand side is a null vector

    if(vectorData == 0)
    {
    initialize(V.size);
    }

    // deep copy the data
    if(size == V.size)
    {
      for (i = 0; i < size;    i++)
      {
        vectorData[i] = V.vectorData[i];
      }
    }
    else
    {
     dimensionError();
    }
}
DoubleVector  DoubleVector::operator-()
{
    long i; 

    DoubleVector R(*this);

    for (i = 0; i < size; i++)
    {
        R.vectorData[i] = -vectorData[i];
    }
 
    return R;
}  

DoubleVector DoubleVector::operator+(const DoubleVector& V)
{
    long i; 

    DoubleVector R(*this);

    if(size == V.size)
    {
      for (i = 0; i < size;    i++)
      {
        R.vectorData[i] += V.vectorData[i];
      }
    }
    else
    {
     dimensionError();
    }
    return R;
}
DoubleVector DoubleVector::operator-(const DoubleVector& V) 
{
    long i;
    DoubleVector R(*this);

    if(size == V.size)
    {
      for (i = 0; i < size;    i++)
      {
        R.vectorData[i] -= V.vectorData[i];
      }
    }
    else
    {
     dimensionError();
    }
    return R;
}


DoubleVector DoubleVector::operator*(const double d)
{
    long i;

    DoubleVector R(*this);
    for (i = 0; i < size;    i++)
    {
        R.vectorData[i] *= d;
    }
    return R;
}

DoubleVector operator*(const double d, const DoubleVector& V)
{
    long i;

    DoubleVector R(V);
    for (i = 0; i < V.size;    i++)
    {
        R.vectorData[i] *= d;
    }
    return R;
}


DoubleVector DoubleVector::operator/(const double d)
{
    long i;
    DoubleVector R(*this);

    for (i = 0; i < size;    i++)
    {
        R.vectorData[i] /= d;
    }
    return R;
}

void DoubleVector::operator*=(double alpha)
{
    long i; 
    for(i = 0; i < size; i++)
    {
        vectorData[i]*= alpha;
    }
}

void DoubleVector::operator+=(const DoubleVector& V)
{
    long i; 
    for(i = 0; i < size; i++)
    {
        vectorData[i] += V.vectorData[i];
    }

}

double DoubleVector::dot(DoubleVector& V)
{
    long i; 
    double dotProd = 0.0;

    for(i = 0; i < size; i++)
    {
        dotProd += vectorData[i]*V.vectorData[i];
    }         

    return dotProd;
}

void DoubleVector::dimensionError()
{
     cerr << " Incompatable DoubleVector Sizes " << endl;
     cerr << " Hit return to exit " << endl;
     exit(-1);
}
//
//  Output
//
ostream& operator <<(ostream& outStream, const DoubleVector& V)
{

    long i; 
    for(i = 0; i <  V.size; i++)
    { 
      outStream <<  setw(5) << V.vectorData[i] << " ";
      outStream << endl;
    }
    return outStream;
}
 
