////////////////////////////////////////////////////////////////////////
// xvector.cc
//
// Routine that accepts two unit vectors and origins, and computes the
// intersection point of the vectors (actually, the point between the vectors
// that is closest to both).  Also returns the error (miss distance).
//
// Returns 0 for okay, or 1 if there is no solution
//
// Note that this subroutine uses C++ linkage and is thus not
// callable from C or Fortran without bridges.
////////////////////////////////////////////////////////////////////////

#include "xvector.h"

static void dsimq(double A[10], double B[10], int N, int *KS);

////////////////////////////////////////////////////////////////////////

/*********************************************************************
c Convert from image coordinates to xyz coordinates given two
c images forming a stereo pair.
c cam1=x,y,z object space position of camera 1 (PigPoint)
c cam2=x,y,z object space position of camera 2 (PigPoint)
c uvw1=direction cosines for left pixel (PigVector)
c uvw2=direction cosines for right pixel (PigVector)
c xyz= xyz object space coord of object (returned PigPoint)
c return value: 0=OK, 1=no solution
***********************************************************************/

int xvector(const PigPoint cam1, const PigPoint cam2,
	    const PigVector uvw1, const PigVector uvw2,
	    PigPoint &xyz, double *error)
{
    double a[10],b[4],c[10];
    int i, status;
    double d[3];

/* compute direction cosines u,v,w for ray1 and ray2 */
    
// They're already unit vectors...
//    uvw1.normalize();
//    uvw2.normalize();
 
/* solve for x,y,z point on ray1 nearest to ray2 */
    PigVector s = uvw1 * uvw2;
    PigVector s1 = s * uvw1;
    PigVector s2 = s * uvw2;

    a[1]=s.getX();
    a[2]=s1.getX();
    a[3]=s2.getX();
    a[4]=s.getY();
    a[5]=s1.getY();
    a[6]=s2.getY();
    a[7]=s.getZ();
    a[8]=s1.getZ();
    a[9]=s2.getZ();
    for (i=1; i < 10; i++)
	c[i]=a[i];
    b[1] = s % cam1;
    b[2] = s1 % cam1;
    b[3] = s2 % cam2;

    dsimq(a, b, 3, &status);
    if (status > 0)
	return status;
    PigPoint xyz1(b[1], b[2], b[3]);
 
/* solve for xx,yy,zz point on ray2 nearest to ray1 */
    b[1] = s % cam2;
    b[2] = s1 % cam1;
    b[3] = s2 % cam2;

    dsimq(c, b, 3, &status);
    if (status > 0)
	return status;
    PigPoint xyz2(b[1], b[2], b[3]);


/* point inbetween is the closest approach point to both vectors */
    *error = (xyz1-xyz2).magnitude();
 
    xyz = (xyz1 + xyz2) / 2.0;

   return 0;
}

/**************************************************************************
C        USAGE
C           CALL DSIMQ(A,B,N,KS)
C        DESCRIPTION OF PARAMETERS
C           A - MATRIX OF COEFFICIENTS STORED COLUMNWISE.  THESE ARE
C               DESTROYED IN THE COMPUTATION.  THE SIZE OF MATRIX A IS
C               N BY N.
C           B - VECTOR OF ORIGINAL CONSTANTS (LENGTH N). THESE ARE
C               REPLACED BY FINAL SOLUTION VALUES, VECTOR X.
C           N - NUMBER OF EQUATIONS AND VARIABLES. N MUST BE .GT. ONE.
C           KS - OUTPUT DIGIT
C                0 FOR A NORMAL SOLUTION
C                1 FOR A SINGULAR SET OF EQUATIONS
*************************************************************************/

static void dsimq(double A[10], double B[10], int N, int *KS)
{
      double BIGA,SAVE,TOL;   
      int JJ,I,IT,J,IJ,I1,I2,IMAX,IQS,JY,IXJ,IX,JX,IXJX,JJX;
      int K,NY,IA,IB,IC;
      TOL=0.0;
      *KS=0;
      JJ=-N;
      for(J=1; J < N+1; J++)                 /*  DO 65 J=1,N */
      {
        JY=J+1;
        JJ=JJ+N+1;
        BIGA=0.0;
        IT=JJ-J;
        for(I=J; I < N+1; I++)                /* DO 30 I=J,N */
        {
          IJ=IT+I;
                                      /* IF(dabs(BIGA)-dabs(A(IJ))) 20,30,30 */
          if(fabs(BIGA)-fabs(A[IJ]) < 0.0){
            BIGA=A[IJ];               /* 20 */
            IMAX=I;
          }
        }                            /* 30 CONTINUE */
        if(fabs(BIGA)-TOL <= 0.0){     /* IF(dabs(BIGA)-TOL) 35,35,40 */
          *KS=1;                          /* 35 */
          return;
        }
        I1=J+N*(J-2);                   /* 40 */
        IT=IMAX-J;
        for(K=J; K < N+1; K++){         /* DO 50 K=J,N */
          I1=I1+N;
          I2=I1+IT;
          SAVE=A[I1];
          A[I1]=A[I2];
          A[I2]=SAVE;
          A[I1]=A[I1]/BIGA;                /* 50 */
        }
        SAVE=B[IMAX];
        B[IMAX]=B[J];
        B[J]=SAVE/BIGA;
        if(J-N == 0) goto seventy;           /* IF(J-N) 55,70,55 */
        IQS=N*(J-1);                      /* 55 */
        for(IX=JY; IX < N+1; IX++){     /* DO 65 IX=JY,N */
          IXJ=IQS+IX;
          IT=J-IX;
          for(JX=JY; JX < N+1; JX++){   /* DO 60 JX=JY,N */
            IXJX=N*(JX-1)+IX;
            JJX=IXJX+IT;
            A[IXJX]=A[IXJX]-(A[IXJ]*A[JJX]); /* 60 */
          }
          B[IX]=B[IX]-(B[J]*A[IXJ]);          /* end of 2nd 65 */
        }
     }                                        /* end of 1st 65 */
  seventy: NY=N-1;
      IT=N*N;
      for(J=1; J < NY+1; J++){              /* DO 80 J=1,NY */
        IA=IT-J;
        IB=N-J;
        IC=N;
        for(K=1; K < J+1; K++){             /* DO 80 K=1,J */
          B[IB]=B[IB]-A[IA]*B[IC];
          IA=IA-N;
          IC=IC-1;                             /* 80 */
        }
      }
} 


