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

#ifndef __CAMERA__
#define __CAMERA__

#include <stdio.h>
#include "stereo.h"
#include "JPLPic.h"
#include "nav_memory.h"
#include "NamedCaches.h"

class CameraModel {
public:
	CameraModel (void);
	~CameraModel (void);
	
	// I/O
	long	ReadCameraModel (FILE *fp, long type);
	long	WriteCameraModel (FILE *fp);

	// projection functions
	long	Point3DToImage2D (double P[3], double ip[2],
				  double *range, double par[2][3]);
	long	Image2DToRay3D(double ip[2], double P[3],
			       double uvec[3], double par[3][2]);
	// utilities
	long	ResizeImageCoordinate (float rscale, float cscale);

protected:
	void 		*params;
	JMemoryManager	*mm;
};


#define CAHV_MODEL		1
#define CAHVOR_MODEL		2
#define CAHVORE_MODEL		3

class JPLCamera : public CameraModel {

public:
	JPLCamera (void);
	JPLCamera (JMemoryManager *mgr);
	~JPLCamera (void);
	
	void	Init (long type)
	  { Init (type, NULL); }
	void	Init (long type, JMemoryManager *mgr);
	long	ReadCameraModel (char *filename);
	long	ReadCameraModel (char *filename, long type);
	long	ReadCameraModel (FILE *fp, long type);
	long	WriteCameraModel (char *filename);
	long	WriteCameraModel (FILE *fp);
	void	PrintCameraModel (void)
	  { PrintCameraModel (stdout, ""); }
	void	PrintCameraModel (FILE *fp, char *prefix);
	void	PrintCameraModelSummary (void)
	  { PrintCameraModelSummary (stdout, ""); }
	void	PrintCameraModelSummary (FILE *fp, char *prefix);
	void	PrintCameraModelDetails (void);
	void	PrintCameraModelDetails (FILE *fp, char *prefix);
	void	PrintStereoDetails (JPLCamera *rightcam)
	  { PrintStereoDetails (stdout, rightcam, ""); }
	void	PrintStereoDetails (FILE *fp, JPLCamera *rightcam,
					    char *prefix);
	void	PrintExtrinsics (void)
	  { PrintExtrinsics (stdout, ""); }
	void	PrintExtrinsics (FILE *fp, char *prefix);

	void	CopyCamera (JPLCamera *dstCam);

// Initialize a JPLCamera object from a raw representation in memory.
// The first byte is an int which defines the type of the model.

	long	LoadFromMemory (char *memstart, int bytes)
	  {
	    union { int *i; char *c; } u;

	    if (memstart == NULL) return PARAM_ERR;
	    u.c = memstart;
	    return LoadFromMemory (u.c + sizeof(int), *u.i, bytes - sizeof(int));
	  }
	long	LoadFromMemory (char *memstart, int itype, int bytes);
	// projection functions
	long	Point3DToImage2D (double P[3], double ip[2],
				  double *range, double par[2][3]);
	long	Point3DToImage2D (double x, double y, double z,
				  double *row, double *col);
	long	Image2DToRay3D(double ip[2], double P[3],
			       double uvec[3], double par[3][2]);
	long	Image2DToRay3D (double row, double col,
				double ray[3]);
	double	AngleBetweenPoints (double row0, double col0,
				    double row1, double col1);

	// utilities
	long	TranslateModel(float x, float y, float z);
	long	TranslateModel(double T[3]);
	long	RotateModel(double P[3], double A[3], double rads);
	long	GetPointingDirections (double fwd[3], double up[3],
				       double right[3]);
	void	HandleOneVectorRepointing (double desired[3],
					   double current[3],
					   double orthog_from_current[3],
					   double other[3],
					   char *desc);
	
	long 	UpdatePointingDirection (double fwd[3], double up[3]);
	long	ResizeImageCoordinate (float rscale, float cscale);
	long	ResizeImageCoordinate (float rscale, float cscale,
				       JPLCamera *newCam);
	long	ResizeImageCoordinate (float iscale)
	  { return ResizeImageCoordinate (iscale, iscale); }
	long	ResizeImageCoordinate (float iscale, JPLCamera *newCam)
	  { return ResizeImageCoordinate (iscale, iscale, newCam); }

	long	TransformImageCoordinate (float scaleX, float scaleY, 
					float transX, float transY);
	void	M2D_3D (float M[9]);
	void	M2D_3D (double M[9]);
	void	M3D_2D (float M[9]);
	void	M3D_2D (double M[9]);
	void	InitJPLCamera (long type, double *inC,
			       double *inA, double *inH, double *inV,
			       double *inO, double *inR,
			       double *incenter, double *inscale,
			       double *intheta,
			       double *ins, double *ins_int)
	  { InitJPLCamera (type, inC, inA, inH, inV, inO, inR, NULL,
			   incenter, inscale, intheta, NULL, NULL,
			   ins, ins_int); }
	void	InitJPLCamera (long type, long *inRows, long *inCols,
			       double *inC,
			       double *inA, double *inH, double *inV,
			       double *inO, double *inR,
			       double *incenter, double *inscale,
			       double *intheta,
			       double *ins, double *ins_int)
	  { InitJPLCamera (type, inRows, inCols,
			   inC, inA, inH, inV, inO, inR, NULL,
			   incenter, inscale, intheta, NULL, NULL,
			   ins, ins_int); }
	void	InitJPLCamera (long type, double *inC,
			       double *inA, double *inH, double *inV,
			       double *inO, double *inR, double *inE,
			       double *incenter, double *inscale,
			       double *intheta,
			       double *inLin, double *inEtype,
			       double *ins, double *ins_int)
	  { InitJPLCamera (type, NULL, NULL,
			   inC, inA, inH, inV, inO, inR, inE,
			   incenter, inscale, intheta,
			   inLin, inEtype, ins, ins_int); }
	void	InitJPLCamera (long type, long *inRows, long *inCols,
			       double *inC,
			       double *inA, double *inH, double *inV,
			       double *inO, double *inR, double *inE,
			       double *incenter, double *inscale,
			       double *intheta,
			       double *inLin, double *inEtype,
			       double *ins, double *ins_int);
					
	// rectification based on lookup tables.
	long 	GenerateRectificationTable (long dstRows, long dstCols, JPLCamera *dstCam);

	long RectifyImageUsingLinearizedFisheye (JPLPic *srcPic,
						 JPLPic *dstPic,
						 JPLCamera *dstCam,
						 int verbosity);
	void SetRectificationMemoryCache (NamedCaches *nm);
	char *FindExistingRectificationMap (st_cahvore_t *cahvore,
					    st_cahv_t *cahv,
					    int xdim, int ydim, int bytes);

	// lut-based rectification
	long	RectifyImage (JPLPic *srcPic, JPLPic *dstPic);
	// non-lut-based rectification
	long 	RectifyImage (JPLPic *srcPic, JPLPic *dstPic, JPLCamera *dstCamera,
			      unsigned char numIntervals, int verbosity);
	long	       RectifyImage (JPLPic *srcPic, JPLPic *dstPic, 
				     JPLCamera *dstCam,
				     float Madd[9], unsigned char numInterval);

	int RectifyFisheye (JPLCamera *originalCam, JPLCamera *newCam,
			    JPLPic *originalPic, JPLPic *newPic);

	int RectifyFisheyePair (JPLCamera *leftCam, JPLCamera *rightCam,
				JPLCamera *leftRectCam, JPLCamera *rightRectCam,
				JPLPic *leftPic, JPLPic *rightPic,
				JPLPic *leftRectPic, JPLPic *rightRectPic);

	long	PlaneMap (JPLCamera *rightCam,
				float *groundPlane, float M[9]);
	long	TransformStereoCameras (JPLCamera *rightCam,
					JPLCamera *leftResult, 
					JPLCamera *rightResult,
					float leftNodalPos[3],
					float leftToRightDirection[3],
					float leftPointingDirection[3]);
	long	TransformStereoCameras (JPLCamera *rightCam,
					JPLCamera *leftResult, 
					JPLCamera *rightResult,
					double leftNodalPos[3],
					double leftToRightDirection[3],
					double leftPointingDirection[3]);
	long  TransformStereoCamerasRot (JPLCamera *rightCam,
					 JPLCamera *leftResult, 
					 JPLCamera *rightResult,
					 double leftNodalPos[3],
					 double rotation[3][3]);

	// parameters
	long		modelType;	// CAHV or CAHVOR model
	double 		*C, *A, *H, *V, *O, *R, *E;
	double		*center, *scale;	// horizontal/vertical center position and scale.
	double		*theta;
	double		*linearity_parm;// 0:Fisheye .... 1:Perspective
	double		*e_type;	// 1:Perspec 2:Fisheye 3:UseLinParm
	double		*s; // 18x18 covariance matrix
	double 		*s_int; // 5x5 covariance matrix

	int		num_reals;
	int		isValid;
	long		rows, cols;		// image dimensions

	NamedCaches	*rectification_tables;
protected:
	void		*rectificationTable;
};

typedef struct {
  double cahvore_hscale;
  double cahvore_hcenter;
  double cahvore_vscale;
  double cahvore_vcenter;
  double cahvore_r[3];
  double cahvore_e[3];
  int xdim;
  int ydim;
  double cahv_hscale;
  double cahv_hcenter;
  double cahv_vscale;
  double cahv_vcenter;
} RectMapKey;


int RectNameEq (unsigned char *n1, int l1, unsigned char *n2, int l2);
char *RectName2s (char *prefix, unsigned char *start, int len);

#endif
