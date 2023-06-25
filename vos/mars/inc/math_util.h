/*********************************************************************
** $Source: /appl/sde/cvsroots/utilities.cvsroot/utilities.prj/cat1.ar/math_util.mod/include.pub/math_util.h,v $
** $Date: 2010/07/16 18:05:28 $
** $Revision: 3.20 $
** Last checked in by: $Author: dhirsch $
**
** This work was performed for the Jet Propulsion Laboratory, California
** Institute of Technology, sponsored by the United States Government
** under the following contracts:
**
**     Contract 960457 (Mars Surveyor 1998),
**     Contract 960482 (Stardust),
**     Contract 961162 (Mars Surveyor 2001 Odyssey),
**     Contract 961202 (Genesis),
**     Contract 960668 (SIRTF),
**     Contract 1234906 (Mars Reconnaissance Orbiter), 
**     Contract 1255879 (Phoenix), and
**     Contract 1278766 (Juno).
**
** EXPORT CONTROL WARNING:
** This Document may contain Technical Data whose Export is Restricted by
** the Arms Export Control Act (Title 22, USC Sec 2751, et seq) or the
** Export Administration Act of 1979 (as amended, Title 22, USC Sec 2751,
** et seq).  Violations to these Export Laws are subject to SEVERE CRIMINAL
** PENALTIES.  Disseminate in accordance with the provisions of DOD
** Directive 5230.25.
**
**
** Description: This math utility library provides functions for scalars, 
**             vectors, quaternions, matrices, look-up tables, and filters.  
**             Type definitions for these are specified in "math_util.h".
**
**
***********************************************************************/
#ifndef __math_util_defined
/*#include "utilities_standards.h"*/
#include "math.h"

#ifdef __cplusplus
extern "C" {
#endif

/* Vector data types. */
typedef double vector_t[3];
typedef double quaternion_t[4];
typedef double matrix_t[3][3];


#define PI (3.14159265358979323846)
#define HALF_PI (0.5*PI)
#define TWO_PI (2.0*PI)
#define KM2_TO_AU2   (4.468370541E-17)
#define LOG_MIN_POS  (-355.0)      /* natural log of smallest pos num  */
#define FILTER_UNDERFLOW (1.0e-30) /* definition of zero for filters */
#define QNORM_TOL (1.48e-8)       /* sqrt(DBL_EPSILON) from float.h */
#define MIN_SIN_ANG  (0.0005)     /* 0 or 180 deg slew in VslewV */
#define ANG2Q_LIMIT (0.0008)      /* Taylor series limit in AxisAngles_2_Q */
#define LU_SIZE  (3)              /* max matrix size for Cholesky decomp */


/* following #define is same as built in FORTRAN SIGN function */
/* it transfers sign of 2nd argument to magnitude of 1st argument */
#define SIGN(a,b) ( ((b) < 0.0) ? -fabs(a) : fabs(a) ) 

/* following #defines mitagates precision problems */
#define ACOS(x) ((fabs(x)<1.0) ? acos(x) : ( ( ((x)>0.0) ? 0. : PI )))
#define ASIN(x) ((fabs(x)<1.0) ? asin(x) : ( ( ((x)>0.0) ? PI/2.:-PI/2.)))
#define ATAN2(x,y) ( ( ((x)>0.) || ((x)<0.) || ((y)>0.) || ((y)<0.) ) \
                   ? atan2(x,y) : 0.0 )
#define SQRT(x) ( ((x)>0.0) ? sqrt(x) : 0. )
#define LOG(x)  ( ((x)>0.0) ? log(x) : LOG_MIN_POS )

/* following #define uses SIGN #define */
/* limits magnitude of 1st argument to magnitude of 2nd argument */
#define    Sclamp(x,y) ( (fabs(x)>fabs(y)) ? SIGN(y,x) : (x)) 

/* following #define uses MAX and MIN #defines */
/* it limits first argument, x, to interval l <= x <= h  */
#define    Sclamp2(x,l,h) MIN(h, MAX(l,x)) 

/* following #define is same as kernel function iround*/
/* it rounds a double to an integer  */
#define IROUND(x) ( ((x)<0.0) ? (int)((x)-0.5) : (int)((x)+0.5) )

#define INT_TO_GRAY(x) (x)^((x)>>1)  /*converts any int x to gray code */

unsigned gray_to_int(unsigned code);/*converts 9-bit gray code to int*/

/* fmodulo same as kernel function fmod (which wasn't there) */
double fmodulo(double x, double y);

/* Round same as kernel function round (which wasn't there) */
double Round(double x);

/* float operations */
                
double  poly( double x, double coeff[], int order); /* x^n polynomial */

/* vector operations */

void    Vequate(vector_t V_in,
/*@out@*/       vector_t V_out);
                 
void    Vnormalize( vector_t Vect);       /* Vect <-- Vect/|Vect| */

double  Vmagnitude( vector_t Vect);      /* Vmagnitude <-- |Vect| */

void    Vadd( vector_t V_in1,            /* V_out <-- V_in1 + V_in2 */
              vector_t V_in2,
/*@out@*/     vector_t V_out);
              
void    Vsub( vector_t V_in1,            /* V_out <-- V_in1 - V_in2 */
              vector_t V_in2,
/*@out@*/     vector_t V_out);
              
double  VdotV( vector_t V_in1,           /* VdotV <-- <V_in1,V_in2> */
               vector_t V_in2);
               
void    VcrossV( vector_t V_in1,         /* V_out <-- V_in1 X V_in2 */
                 vector_t V_in2,
/*@out@*/        vector_t V_out);
                
void    SVmult( double in_value,        /* V_out <-- in_value * V_in */
                vector_t V_in,
/*@out@*/       vector_t V_out);
                
void    Vclamp( vector_t Subject_Vect,   /* Subject clamped by Limit */
                vector_t Limit_Vect);
                
void    Vclamp2( vector_t Subject_Vect,
                 vector_t Lower_Limits,
                 vector_t Upper_Limits);
                 
void    VQmult( vector_t InputVector,
                quaternion_t  Quat, 
/*@out@*/       vector_t OutputVector);

void    VslewV( vector_t V1,
                vector_t V2,
/*@out@*/      double *angle,
/*@out@*/       vector_t axis);

/* quaternion operations */

void    QQmult( quaternion_t  Q_in1,
                quaternion_t  Q_in2,
/*@out@*/       quaternion_t  Q_out);
               
void    QVmult( quaternion_t  Quat,
                vector_t InputVector,
/*@out@*/       vector_t OutputVector);
               
void    QcQmult( quaternion_t  Q_in1,
                 quaternion_t  Q_in2,
/*@out@*/        quaternion_t  Q_out);
                
void    QQcmult( quaternion_t  Q_in1,
                 quaternion_t  Q_in2,
/*@out@*/        quaternion_t  Q_out);
                 
void    Qequate(quaternion_t  Q_in,
/*@out@*/       quaternion_t  Q_out);
                
void    Qnormalize( quaternion_t  Quat);

void    Qsqrt(quaternion_t Q,
/*@out@*/     quaternion_t Sq);

/* matrix operations */

void    MVmult( matrix_t DCM_in,
                vector_t V_in,
/*@out@*/       vector_t V_out);
               
void    VMmult( vector_t V_in,
                matrix_t DCM_in,
/*@out@*/       vector_t V_out);
                
void    MMmult( matrix_t DCM_in1,
                matrix_t DCM_in2,
/*@out@*/       matrix_t DCM_out);
                
void    MMtmult( matrix_t DCM_in1,
                 matrix_t DCM_in2,
/*@out@*/        matrix_t DCM_out);
                 
void    MtMmult( matrix_t DCM_in1,
                 matrix_t DCM_in2,
/*@out@*/        matrix_t DCM_out);
                 
void    Mequate( matrix_t M_in,
/*@out@*/        matrix_t M_out);

/* Conversions */

void    Q_2_Vect_n_Float(quaternion_t  Q_in,
/*@out@*/                vector_t V_out,
/*@out@*/                double *S_out);
                          
void    Vect_n_Float_2_Q( vector_t V_in,
                          double S_in,
/*@out@*/                 quaternion_t  Q_out);
                          
void    Q_2_AxisAngles( quaternion_t  Q_in,
/*@out@*/               vector_t V_out);
                        
void    AxisAngles_2_Q( vector_t V_in,
/*@out@*/               quaternion_t  Q_out);
                        
void    Q_2_DCM( quaternion_t  Q_in,
/*@out@*/         matrix_t DCM_out);
                 
void    DCM_2_Q( matrix_t DCM_in,
/*@out@*/        quaternion_t  Q_out);

void    Qpropagate(quaternion_t  Q_in,
                   double        deltaT,
                   vector_t      rate,
/*@out@*/          quaternion_t  Q_out);

/* Cholesky triang decomposition of pos-definite symmetrix matrix */
/* returns -1 if not positive definite */
/* input matrix has lower triangular part replaced by result plus */
/* diagonal output is put in a diagonal vector  */
int     Mcholdc( matrix_t PDin,
/*@out@*/       vector_t diag);

/* Mcholsl: back subst with Mcholdc gets C_out=B_in'*(PDin inv); */
/* B_in' is change to fix Kalman Filter bug Numerical Recipes 2.9 */
void    Mcholsl(matrix_t PDin, vector_t diag, matrix_t B_in,
/*@out@*/       matrix_t C_out);

/* Mcholsr: back subst with Mcholdc gets X = PDin_inv * V  */
void    Mcholsr(matrix_t PDin, vector_t diag, vector_t V,
/*@out@*/       vector_t X );

/* lookup interpolates x-ascending table of size=N: */
/* {x0,y1, x2,y3, ...}  where N = sizeof(xyTab)/sizeof(xyTab[0]) */
double lookup(double xin, double xyTab[], int N);


/*-----------------------------------------------------------------
-- Table lookup for x vs multiple y's table with increasing x
-- Table consists of m-1 y values for each of k x's.
--  x's      run Tab[0]  < Tab[m]  <  Tab[2m]  ...  < Tab[(k-1)m]
--  y[1]'s   run Tab[1],   Tab[m+1],  Tab[2m+1] ... , Tab[(k-1)m+1]
--  ... ... ... ... ... ... ... ... ... ... ... ...
--  y[m-1]'s run Tab[m-1], Tab[2m-1], Tab[3m-1] ... , Tab[N-1]
--
--   NOTE: This is a table of N = m*k doubles.
--         Output is an m-1 dimensional vector, where m > 1.
-----------------------------------------------------------------*/
void lookup_many(double xin, double xyTab[], int , int N,
/*@out@*/       double out[]);


/*-----------------------------------------------------------------
-- Vector Table lookup function for t vs V (3D vector) table
-- with increasing t   NOTE: This is a table of N = 4*k integers
-- Similar to lookup_many for m=4, but input table entries are ints.
-----------------------------------------------------------------*/
void lookupVec(int tin, int VecTab[], int N,
/*@out@*/      vector_t Vout);



 /* 1st, 2nd & 4th order filters with double input and output */
 /* uses pointer to coefficient & history structure */
 /* The coefficient & history structure needs initialization */

typedef struct ord1FiltStruct {
    double numC0;
    double numC1;
    double denD1;
    double pastIn;
    double pastOut;
} Ord1Filt_t, *Ord1Filt_p;

double ord1Filt(double xin, Ord1Filt_p P);

void ord1FiltInit(const double     numCoef[2], 
                         double     denCoef, 
                         double     initialValue, 
             /*@out@*/ Ord1Filt_p P );
void ord1FiltReinit(Ord1Filt_p P ); 
void ord1FiltReset(double initialValue, Ord1Filt_p P);


typedef struct ord2FiltStruct {
    double numC[3];
    double denD[2];
    double inU[2];
    double outY[2];
} Ord2Filt_t, *Ord2Filt_p;

double ord2Filt(double xin, Ord2Filt_p P);

void ord2FiltInit(const double     numCoef[3], 
                  const double     denCoef[2], 
                         double     initialValue, 
              /*@out@*/ Ord2Filt_p P );
                  
void ord2FiltReinit(Ord2Filt_p P ); 
void ord2FiltReset(double initialValue, Ord2Filt_p P);

typedef struct ord4FiltStruct {
    double numC[5];
    double denD[4];
    double inU[4];
    double outY[4];
} Ord4Filt_t, *Ord4Filt_p;

double ord4Filt(double xin, Ord4Filt_p P);

void ord4FiltInit(const double     numCoef[5], 
                  const double     denCoef[4], 
                         double     initialValue, 
             /*@out@*/ Ord4Filt_p P );
void ord4FiltReset(double initialValue, Ord4Filt_p P);


#define __math_util_defined

#ifdef __cplusplus
}
#endif

#endif
