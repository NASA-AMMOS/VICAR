/* marscheckcm */
#include "vicmain_c"

#include "PigMission.h"
#include "PigFileModel.h"
#include "PigCameraModel.h"
#include "PigPointingModel.h"
#include "PigCoordSystem.h"

#include "mars_support.h"
#include "mars_tiepoints.h"

#include <stdio.h>

// buffer sizes in main program - these should be made dynamic, eventually!
#define MAX_INPUTS 2
#define MAX_OPEN 2
#define MAX_NL MARS_MAX_NL
#define MAX_NS MARS_MAX_NS

static int xvector(const PigPoint cam1, const PigPoint cam2,
		   const PigVector uvw1, const PigVector uvw2,
		   PigPoint &xyz, double *error);

static void dsimq(double A[10], double B[10], int N, int *KS);
////////////////////////////////////////////////////////////////////////

void main44()
{
    const size_t msgLen = 500;
    char msg[msgLen];

    int nids;		// # of inputs, outputs (must match!)
    char mission[64], instrument[64];

    // Inputs

    PigFileModel *file_models[MAX_INPUTS];
    PigCameraModel *camera_in[MAX_INPUTS];		// not used
    PigPointingModel *pointing_in[MAX_INPUTS];		// not used
    int homogeneous_inputs = TRUE;
    PigCoordSystem *cs;

    zvmessage("MARSTIEXYZ version 2019-12-10", "");

    // Get the input file list, and set up initial camera/pointing models
    // for each input (not used)
    mars_setup(nids, file_models, camera_in, pointing_in, NULL, 
	       cs, mission, instrument, homogeneous_inputs,
               MAX_NL, MAX_NS, MAX_INPUTS);

    // Print out input status from labels

    mars_print_inputs(nids, pointing_in, camera_in, file_models,
			homogeneous_inputs, mission, instrument);

    if (nids != 2) {
	zvmessage("MARSTIEXYZ requires 2 and only 2 image inputs", "");
	return;
    }

    snprintf(msg, msgLen, "Generating XYZ point using the %s coordinate frame",
			cs->getFrameName());
    zvmessage(msg, "");

    struct TiePoint tiepoints[1];
    tiepoints[0].left_image=0;
    tiepoints[0].right_image=1;
    tiepoints[0].left_sample=100;
    tiepoints[0].right_sample=100;
    tiepoints[0].left_line=100;
    tiepoints[0].right_line=100;
    tiepoints[0].interactive = 0;
    
    // If inp is direct file (no ascii), and first input is a 3-band, the 
    // 3-band-ness is getting transferred into the IBIS file via the primary 
    // input mechanism and this doesn’t normally come up because we normally 
    // provide a list file as input (to marstie) which means NO primary input
    // So, disabling primary input
    zvselpi(0);

    int n_overlaps;
    mars_edit_tiepoint(0, n_overlaps, tiepoints, file_models);
    // Convert points to camera coordinates

    double orig_l = tiepoints[0].left_line - 1 + file_models[0]->getYOffset();
    double orig_s = tiepoints[0].left_sample - 1 + file_models[0]->getXOffset();

    // disparity images are 1-based, subtract 1 to convert
    // to 0-base for camera model
    double disp_l = tiepoints[0].corrected_line - 1 + 
                    file_models[1]->getYOffset();
    double disp_s = tiepoints[0].corrected_sample - 1 + 
                    file_models[1]->getXOffset();

    // Compute unit vectors for both points

    PigPoint orig_origin, disp_origin;
    PigVector orig_vector, disp_vector;
    
    camera_in[0]->LStoLookVector(orig_l, orig_s,
				 orig_origin, orig_vector, cs);
    camera_in[1]->LStoLookVector(disp_l, disp_s,
				 disp_origin, disp_vector, cs);    

    // Compute x,y,z and error by finding the intersection of the
    // two rays.  We really compute the closest point between the lines
    // since they'll almost never intersect.
    
    PigPoint xyz;
    double error_value;
    xvector(orig_origin, disp_origin, orig_vector, disp_vector,
	    xyz, &error_value);

    zvmessage(
 "LEFT_L  LEFT_S  RIGHT_L RIGHT_S FRAME_NAME   X         Y         Z         ", " ");
    // Print out some values
      snprintf(msg, msgLen, "%-7.2f %-7.2f %-7.2f %-7.2f %-12s %+f %+f %+f",
	      orig_l+1, orig_s+1, disp_l+1, disp_s+1, cs->getFrameName(),
	      xyz.getX(), xyz.getY(), xyz.getZ());
      zvmessage(msg," ");

      // convert point
      PigCoordSystem *cs_rover = (PigMission::getMissionObject(mission))->getCoordSystem(file_models[0], "ROVER_FRAME");
      PigPoint pt = cs_rover->convertPoint(xyz, cs);

    // Print out some values
      snprintf(msg, msgLen, "%-7.2f %-7.2f %-7.2f %-7.2f %-12s %+f %+f %+f",
	      orig_l+1, orig_s+1, disp_l+1, disp_s+1, cs_rover->getFrameName(),
	      pt.getX(), pt.getY(), pt.getZ());
      zvmessage(msg," ");


}

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

static int xvector(const PigPoint cam1, const PigPoint cam2,
		   const PigVector uvw1, const PigVector uvw2,
		   PigPoint &xyz, double *error)
{
    double a[10],b[4],c[10];
    int i, status;

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
#if 0
///////////////////////////////////////////////////////////////////
//
// use lcross routine for comparison, added by jnm 15 April 1999

    double b1[3],b2[3],v1[3],v2[3],p1[3],p2[3];
    double k1,k2;
    int colinear;
    k1 =0;
    k2 =0;
    colinear =0;

// pull the points out into simple arrays
   cam1.getXYZ(b1);  // base point #1
   cam2.getXYZ(b2);  // base point #2
   uvw1.getXYZ(v1);  // pointing vector #1
   uvw2.getXYZ(v2);  // pointing vector #2
 
// cout << "Base point 1 is: " << b1[0] << "," <<b1[1] << "," << b1[2] << endl;
// cout << "Base point 2 is: " << b2[0] << "," <<b2[1] << "," << b2[2] << endl;
// cout << "Vector 1 is: " << v1[0] << "," <<v1[1] << "," << v1[2] << endl;
// cout << "Vector 2 is: " << v2[0] << "," <<v2[1] << "," << v2[2] << endl;

   lcross(b1, v1, b2, v2, &k1, p1, &k2, p2, &colinear);

// now put the points p1 and p2 into the xyz1 and xyz1 Point()s.
  
   PigPoint xyz1(p1[0], p1[1], p1[2]);
   PigPoint xyz2(p2[0], p2[1], p2[2]);

// now back to our regularly scheduled program.
//
///////////////////////////////////////////////////////////////////
#endif

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
      int JJ,I,IT,J,IJ,I1,I2,IMAX=0,IQS,JY,IXJ,IX,JX,IXJX,JJX;
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
