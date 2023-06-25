
#include <iostream>
#include <iomanip>
using namespace std;

#include "DoubleMatrix.h"
#include "DoubleVector.h" 

#include <math.h>
#include <stdio.h>
#include <stdlib.h>

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
*/DoubleMatrix::DoubleMatrix()
{
    matrixValues = 0;
    matrixData   = 0;
    rows         = 0;
    columns      = 0;
}

DoubleMatrix::DoubleMatrix(const DoubleMatrix& M)
{
    matrixValues = 0;
    matrixData   = 0;
    rows         = 0;
    columns      = 0;
    initialize(M);
}

DoubleMatrix::DoubleMatrix(long m, long n)
{
    matrixValues = 0;
    matrixData   = 0;
    rows         = 0;
    columns      = 0;
    initialize(m,n);
}

DoubleMatrix::DoubleMatrix(long m, long n, double* data)
{
    matrixValues = 0;
    matrixData   = 0;
    rows         = 0;
    columns      = 0;
    initialize(m,n,data);
}

DoubleMatrix::DoubleMatrix(long m, long n, float* data)   // Igor
{
    matrixValues = 0;
    matrixData   = 0;
    rows         = 0;
    columns      = 0;
    initialize(m,n,data);
}

DoubleMatrix::~DoubleMatrix()
{
    if(matrixData != 0)
    {
    delete [] matrixData;
    delete [] matrixValues;
    }
}
//
//    Initialize
//
void DoubleMatrix::initialize()
{
    // delete old data

    if(matrixData != 0)
    {
    delete [] matrixData;
    delete [] matrixValues;
    }

    // reset to null

    matrixValues = 0;
    matrixData   = 0;
    rows         = 0;
    columns      = 0;
}

void DoubleMatrix::initialize(const DoubleMatrix& M)
{
    long i; long j;

    if(matrixData != 0)
    {initialize();}

    if(M.matrixData != 0)
    {
    rows       = M.rows;
    columns    = M.columns;

    initialize(M.rows,M.columns);
    }
    // deep copy the data

    for (i = 0; i < rows;    i++)
    {
    for (j = 0; j < columns; j++)
    {
        matrixValues[i][j] = M.matrixValues[i][j];
    }}
}

void DoubleMatrix::initialize(long m, long n)
{
    // delete old data

    long i;  long j;

    if(matrixData != 0)
    {initialize();}

    // create new matrix

    rows    = m;
    columns = n;
//
//  dynamically construct an m by n matrix
//
    matrixData   = new double[rows*columns];
    matrixValues = new double*[rows];

    for(i = 0; i < rows; i++)
    {matrixValues[i] = &matrixData[i*columns];}
//
//  initalize with 0's
//
    for (i = 0; i < rows;    i++)
    {
    for (j = 0; j < columns; j++)
    {
     matrixValues[i][j] = 0.0;
    }}
}

void DoubleMatrix::initialize(long m, long n, double* data)
{
// delete old data

    long i;  long j;

    if(matrixData != 0)
    {initialize();}

// create new matrix

    rows    = m;
    columns = n;
//
//  dynamically construct an m by n matrix
//
    matrixData   = new double[rows*columns];
    matrixValues = new double*[rows];

    for(i = 0; i < rows; i++)
    {matrixValues[i] = &matrixData[i*columns];}
//
//  initalize with data
//
    for (i = 0; i < rows;    i++)
    {
    for (j = 0; j < columns; j++)
    {
     matrixValues[i][j] = data[i*columns + j];
    }}

}

void DoubleMatrix::initialize(long m, long n, float* data)   // Igor
{
    // delete old data
    
    long i;  long j;
    
    if(matrixData != 0)
    {initialize();}
    
    // create new matrix
    
    rows    = m;
    columns = n;
    //
    //  dynamically construct an m by n matrix
    //
    matrixData   = new double[rows*columns];
    matrixValues = new double*[rows];
    
    for(i = 0; i < rows; i++)
    {matrixValues[i] = &matrixData[i*columns];}
    //
    //  initalize with data
    //
    for (i = 0; i < rows;    i++)
    {
        for (j = 0; j < columns; j++)
        {
            matrixValues[i][j] = data[i*columns + j];
        }}
    
}


double& DoubleMatrix::operator()(long i, long j)
{
#ifdef _DEBUG 
    if((i < 1)||(i>  rows)||(j < 1)||(j > columns))
    {
        cerr << " Illegal index for DoubleMatrix Access  " << endl;
        exit(-1);
     }
#endif
    return matrixValues[i-1][j-1];
}

const double& DoubleMatrix::operator()(long i, long j) const
{
#ifdef _DEBUG 
    if((i < 1)||(i> rows)||(j < 1)||(j > columns))
    {
        cerr << " Illegal index for DoubleMatrix Access  " << endl;
        exit(-1);
     }
#endif
    return matrixValues[i-1][j-1];
}

void DoubleMatrix::operator=(const DoubleMatrix& M)
{
    long i; long j;

    // create new matrix if left hand side is a null matrix

    if(matrixValues == 0)
    {
    initialize(M.rows,M.columns);
    }

    // deep copy the data
    if((rows == M.rows)&&(columns == M.columns))
    {
      for (i = 0; i < rows;    i++)
      {
      for (j = 0; j < columns; j++)
      {
        matrixValues[i][j] = M.matrixValues[i][j];
      }}
    }
    else
    {
     dimensionError();
    }
}

DoubleMatrix DoubleMatrix::operator-()
{
    long i; long j;

    DoubleMatrix R(*this);

    for (i = 0; i < rows;    i++)
    {
    for (j = 0; j < columns; j++)
    {
       R.matrixValues[i][j] = -matrixValues[i][j];
    }}

    return R;
  
}
DoubleMatrix DoubleMatrix::operator+(const DoubleMatrix& M)
{
    long i; long j;

    // create return matrix 

    DoubleMatrix R(*this);

    if((rows == M.rows)&&(columns == M.columns))
    {
      for (i = 0; i < rows;    i++)
      {
      for (j = 0; j < columns; j++)
      {
        R.matrixValues[i][j] += M.matrixValues[i][j];
      }}
    }
    else
    {
     dimensionError();
    }

    return R;
}

DoubleMatrix DoubleMatrix::operator-(const DoubleMatrix& M)
{
    long i; long j;

    // create return matrix 

    DoubleMatrix R(*this);

    if((rows == M.rows)&&(columns == M.columns))
    {
      for (i = 0; i < rows;    i++)
      {
      for (j = 0; j < columns; j++)
      {
        R.matrixValues[i][j] -= M.matrixValues[i][j];
      }}
    }
    else
    {
     dimensionError();
    }

    return R;
}
DoubleMatrix DoubleMatrix::operator*(const DoubleMatrix& M)
{
    long i; long j; long k;

    // create return matrix 

    DoubleMatrix R(rows,M.columns);

    double sum = 0.0;

    if(columns == M.rows)
    {
      for (i = 0; i < rows;    i++)
      {
      for (j = 0; j < M.columns; j++)
      {
        sum = 0.0;
        for(k = 0; k < columns; k++)
        {
        sum += matrixValues[i][k]*M.matrixValues[k][j];
        }
        R.matrixValues[i][j] =sum;
      }}
    }
    else
    {
     dimensionError();
    }

    return R;
}


DoubleMatrix DoubleMatrix::operator*(const double d)
{
    long i; long j;

    // create return matrix 

    DoubleMatrix R(*this);

    for (i = 0; i < rows;    i++)
    {
    for (j = 0; j < columns; j++)
    {
        R.matrixValues[i][j] *= d;
    }}
  
    return R;
}
DoubleMatrix DoubleMatrix::operator/(const double d)
{
    long i; long j;

    // create return matrix 

    DoubleMatrix R(*this);

    for (i = 0; i < rows;    i++)
    {
    for (j = 0; j < columns; j++)
    {
        R.matrixValues[i][j] /= d;
    }}
  
    return R;
}

DoubleMatrix operator*(double d, const DoubleMatrix& M)
{
    long i; long j;

    // create return matrix 

    DoubleMatrix R(M);

    for (i = 0; i < M.rows;    i++)
    {
    for (j = 0; j < M.columns; j++)
    {
        R.matrixValues[i][j] *= d;
    }}
  
    return R;
}


DoubleVector DoubleMatrix::operator*(const DoubleVector& V)
{
    long i; long k;

    // create return vector

    DoubleVector R(rows);

    double sum = 0.0;

    if(columns == V.size)
    {
      for (i = 0; i < rows;    i++)
      {
        sum = 0.0;
        for (k = 0; k < columns; k++)
        {
        sum += matrixValues[i][k]*V.vectorData[k];
        }
        R.vectorData[i] = sum;
      }
    }
    else
    {
     dimensionError();
    }
    return R;
}

DoubleVector DoubleMatrix::applyInverse(const DoubleVector& V)
{
    // create return vector

    DoubleVector R(V);

    if(columns == V.size)
    {
      SolveLinearSystem(matrixData, rows, columns,R.vectorData,1);
    }
    else
    {
     dimensionError();
    }

    return R;
}

void DoubleMatrix::dimensionError()
{
     cerr << " Incompatable DoubleMatrix Sizes " << endl;
     exit(-1);
}
//
// The following is a "portable" routine for solving linear
// systems using the QR factorization. It internally
// estimates a condition number. 
//
// Warning : this routine is a temporary stand-in until I
// write a portable routine that assumes the input data is
// stored by rows ("C" convention). 
//
// Chris Andersion (C) UCLA   May 4, 2001
// 
void  DoubleMatrix::SolveLinearSystem(double* MdataPtr, long M, long N, 
double* BdataPtr, long P)
{
    long i,j,k;
    
    double machine_eps = ::pow(10.0,-12.0);  
    
    long Mindex1size = M;
    long Mindex2size = N;
    long Bindex1size = M;
    long Bindex2size = P;
//
// create double temporaries
//  
    double* RdataPtr = new double[Mindex1size*Mindex2size];
    double* XdataPtr = new double[Bindex1size*Bindex2size];
//
//  Put input data into temporaries : 
//
//  Transpose copy of the matrix, since the original routine was written 
//  assuming array data is stored by columns.
//
    for(i = 0; i < Mindex1size; i++)
    {
    for(j = 0; j < Mindex2size; j++)
    {
    *(RdataPtr + i + j*M) = *(MdataPtr + j + i*N);
    }}

//  Standard copy 
//
    double* Bdptr;
    double* Xdptr;    
    
    for(Xdptr = XdataPtr, Bdptr = BdataPtr; 
    Xdptr < XdataPtr + (M*P); Xdptr++, Bdptr++)
    {*(Xdptr) = *(Bdptr);}  

    long m     =   Mindex1size;
    long n     =   Mindex2size;
    long p     =   Bindex2size; 
    
    double tau;

    double* Rptr; double* Sptr; 
    double* Cptr; double* Tptr;
    double* Xptr;   
    
    register double* Top;
    register double* piv_elem;

    double *S_data = new double[m];
    double *C_data = new double[m];
    double *T_data = new double[m];           
   
    for( k = 1;  k <= n; k++)
    {
      piv_elem = RdataPtr + (k-1)*m + (k-1);    
      for( Rptr  = piv_elem + 1, Sptr = S_data, Cptr = C_data;
           Rptr <= piv_elem + (m-k); Rptr++, Sptr++, Cptr++)
      {
        if( *Rptr == 0.0 ){ *Cptr = 1.0; *Sptr =0.0;}
        else
        { if( fabs(*Rptr) > fabs(*piv_elem) )
          { tau   = -(*piv_elem)/(*Rptr); 
            *Sptr = 1.0/sqrt(1.0 + tau*tau);
            *Cptr = (*Sptr)*tau;
          }
          else
          { tau  = -(*Rptr)/(*piv_elem);
            *Cptr = 1.0/sqrt(1.0 + tau*tau);
            *Sptr = (*Cptr)*tau;
          }
        }                                                                         
       *piv_elem = ((*Cptr) * (*piv_elem)) - ((*Sptr) * (*Rptr));
      }

      for( j = k+1; j <= n; j++)
      {
       Top = RdataPtr + (j-1)*m + (k-1);
       for(Rptr = Top + 1, Sptr = S_data, Cptr = C_data, Tptr = T_data;
           Rptr <= Top + (m - k); Rptr++, Sptr++, Cptr++, Tptr++)
            {  *Tptr = (*Sptr) * (*Top);
               *Top  = ((*Cptr) * (*Top)) - ((*Sptr) * (*Rptr));
               *Rptr *= (*Cptr); 
            }

       for(Rptr = Top + 1, Tptr = T_data; Rptr <= Top + (m - k);  Rptr++, Tptr++)
           { *Rptr += *Tptr;}
      }
//
//    Transform the right hand side 
//
      for( j = 1; j <= p ; j++)
      {
         Top = XdataPtr + (j-1)*m + (k-1);
         for( Xptr = Top + 1, Sptr = S_data, Cptr = C_data, Tptr = T_data;
              Xptr <= Top + (m - k); Xptr++, Sptr++, Cptr++, Tptr++)
              { *Tptr = (*Sptr) * (*Top);
                *Top  = ((*Cptr) * (*Top)) - ((*Sptr) * (*Xptr));
                *Xptr *= (*Cptr); 
              }

         for( Xptr  = Top + 1, Tptr = T_data; Xptr <= Top + (m - k); 
              Xptr++, Tptr++)
              { *Xptr += *Tptr; }
      }
      
} 
//
//  Estimate the condition number 
//
    double R_norm = 0.0; 
    double R_col_norm; 
    for( k = 1; k <= n; k++)
    {  R_col_norm = 0.0;
       for( Rptr = RdataPtr + (k-1)*m; Rptr < RdataPtr + (k-1)*m + k; Rptr++ )
       {R_col_norm += fabs(*Rptr);}
       if(R_norm < R_col_norm ) R_norm = R_col_norm;
     }

    long singular_flag = 0;       
    for( j=1; j <= n ; j++)
    {if(fabs(*(RdataPtr + (j-1) + (j-1)*m)) <= machine_eps*R_norm ) singular_flag = 1;} 
    
    if( singular_flag == 1) 
    { 
      printf(" Matrix system singular or close to singular \n");
      printf("     Computed results may be inaccurate      \n");
     }
//
//  Back Substitute
//
    register double XJ;    
    for( k = 1; k <= p; k++)
    { 
      for( j=  n; j >= 2; j--)
      {
       XJ = (*(XdataPtr +(k-1)*m + (j-1))) / (*(RdataPtr + (j-1)*m + (j-1))); 
       *(XdataPtr +(k-1)*m + (j-1)) = XJ;
        for( Xptr = XdataPtr + (k-1)*m, Rptr = RdataPtr + (j-1)*m;
             Xptr < XdataPtr + (k-1)*m + (j-1); Xptr++, Rptr++)
            *Xptr -= XJ*(*Rptr);
      }
    *(XdataPtr + (k-1)*m) = (*(XdataPtr + (k-1)*m))/(*(RdataPtr));
    } 
 //
 // Copy the solution to B
 //

    for(Xdptr = XdataPtr, Bdptr = BdataPtr; 
    Xdptr < XdataPtr + (M*P); Xdptr++, Bdptr++)
    {*(Bdptr) = *(Xdptr);}  


    delete [] XdataPtr;
    delete [] RdataPtr;
    delete [] S_data;
    delete [] C_data;
    delete [] T_data;
}
//
//  Output
//
ostream& operator <<(ostream& outStream, const DoubleMatrix& M)
{
    long i; long j;
    for(i = 0; i <  M.rows; i++)
    {
     for(j = 0; j <  M.columns; j++)
      {
      outStream <<  setw(5) << M.matrixValues[i][j] << " ";
      }
      outStream << endl;
    }
    return outStream;
}
 
