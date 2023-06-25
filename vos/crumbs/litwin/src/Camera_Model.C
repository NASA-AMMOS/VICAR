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

#include <stdio.h>
#include <string.h>
#include <math.h>
#include "mat3.h"
#include "cmod_cahv.h"
#include "cmod_cahvor.h"
#include "cmod_cahvore.h"
#include "Camera_Model.h"


CameraModel :: CameraModel() { 
}


CameraModel :: ~CameraModel () { 
}

const char *CameraModel :: getCameraModelName() {
	static char *name="Base_Class";
	return(name);
}

void CameraModel :: FatalErr (char *err_string) {
	fprintf(stderr,"Whoops - Problem in CameraModel class type %s\n", getCameraModelName());
	fprintf(stderr,"%s\n", err_string);
}

long CameraModel::Image2DToAzEl(double row, double col, double *az, double *el) {
	double ray[3];
	long result = Image2DToRay3D(row, col, ray);

	if(NO_ERR == result) {
		*az = atan2(ray[1], ray[0]);
		*el = -atan2(ray[2], sqrt(ray[1]*ray[1] + ray[0]*ray[0]));
	}
	return(result);
}

long CameraModel::Point3DToAzElRng(double x, double y, double z,
			double *az, double *el, double *range) {
	double xdiff = x - getCameraX();
	double ydiff = y - getCameraY();
	double zdiff = z - getCameraZ();

	*range = sqrt(xdiff*xdiff + ydiff*ydiff + zdiff*zdiff);
	*az = atan2(ydiff, xdiff);
	*el = -atan2(zdiff, sqrt(xdiff*xdiff + ydiff*ydiff));
	return(NO_ERR);
}

long CameraModel:: Point3DToImage2D (double x, double y, double z,
			double *row, double *col)
{
	return(NO_ERR);
}

long CameraModel::Image2DToRay3D (double row, double col, double ray[3]) {
	return(NO_ERR);
}

long CameraModel::Image2DToPoint3D (double row, double col, double range, double xyz[3]) {
	double ray[3];
	long result = Image2DToRay3D(row, col, ray);

	if(NO_ERR == result) {
		xyz[0] = ray[0] * range + getCameraX();
		xyz[1] = ray[1] * range + getCameraY();
		xyz[2] = ray[2] * range + getCameraZ();
	}
	return(result);
}


/*******************************************************************************************/

JPLCamera::JPLCamera () {
  // All parameters are stored on the heap in a list of doubles.

	modelType = CAHV_MODEL;
	linearity = 1.0;
	
	double *data = new double[21
#ifdef FULL_MODEL_IMPLEMENTATION
		+5+18*18+5*5
#endif //FULL_MODEL_IMPLEMENTATION
	];
	isValid = 0;

	C = data; // set before check so test prior to deallocate works in destructor
	if (data == NULL) {
	  fprintf (stderr, "*** Unable to allocate new JPLCamera!\n");
	  return;
	}
	A = &data[3];
	H = &data[6];
	V = &data[9];
	O = &data[12];
	R = &data[15];
	E = &data[18];
	
#ifdef FULL_MODEL_IMPLEMENTATION
	center = &data[21];
	scale = &data[23];
	theta = &data[25];
	
	// Covariance matrix
	s = &data[26];

	// Another covariance matrix
	s_int = &data[26+18*18];
	
	rectificationTable = NULL;
	rows = cols = 0;
#endif //FULL_MODEL_IMPLEMENTATION
}



JPLCamera::~JPLCamera ()
{
  if(C) delete(C);
#ifdef FULL_MODEL_IMPLEMENTATION
  if (rectificationTable) {
#ifdef INIT_DEBUG
    fprintf (stderr, "JPLCamera destructor: rectificationTable was %lx\n",
	     (long) rectificationTable);
#endif
    delete rectificationTable;
  }
#endif //FULL_MODEL_IMPLEMENTATION
}


const char *JPLCamera :: getCameraModelName() {
	static char *name1="CAHV_Model";
	static char *name2="CAHVOR_Model";
	static char *name3="CAHVORE_Model";
	if(getmodelType() == CAHV_MODEL) {
		return(name1);
	} else if(getmodelType() == CAHVOR_MODEL) {
		return(name2);
	} else if(getmodelType() == CAHVORE_MODEL) {
		return(name3);
	} else {
		fprintf(stderr,"Whoops - Request for Camera Model Name with invalid type=%d\n",
			getmodelType());
		return(NULL);
	}
}


void
JPLCamera::CopyCamera (JPLCamera *dstCam)
{
	dstCam->setmodelType(modelType);
	dstCam->setC(C);
	dstCam->setA(A);
	dstCam->setH(H);
	dstCam->setV(V);
	dstCam->setO(O);
	dstCam->setR(R);
	dstCam->setE(E);
#ifdef FULL_MODEL_IMPLEMENTATION
	dstCam->setcenter(center);
	dstCam->setscale(scale);
	dstCam->settheta(gettheta());
	dstCam->sets(s);
	dstCam->sets_int(s_int);
#endif //FULL_MODEL_IMPLEMENTATION
}

void JPLCamera::setmodelType(long inmodelType) {
	if(inmodelType == CAHV_MODEL || inmodelType == CAHVOR_MODEL || inmodelType == CAHVORE_MODEL) {
		modelType = inmodelType;
	} else {
		fprintf(stderr,"Whoops - Input modeltype (%d) is invalid.\n", inmodelType);
	}
}

long JPLCamera::getmodelType() {
	return(modelType);
}

double JPLCamera::getCameraX() {
	return(C[0]);
}

double JPLCamera::getCameraY() {
	return(C[1]);
}

double JPLCamera::getCameraZ() {
	return(C[2]);
}

void JPLCamera::setC(double *inC) {
	if(inC) {
		setC(inC[0], inC[1], inC[2]);
	}
}

void JPLCamera::setA(double *inA) {
	if(inA) {
		setA(inA[0], inA[1], inA[2]);
	}
}

void JPLCamera::setH(double *inH) {
	if(inH) {
		setH(inH[0], inH[1], inH[2]);
	}
}

void JPLCamera::setV(double *inV) {
	if(inV) {
		setV(inV[0], inV[1], inV[2]);
	}
}

void JPLCamera::setO(double *inO) {
	if(inO) {
		setO(inO[0], inO[1], inO[2]);
	}
}

void JPLCamera::setR(double *inR) {
	if(inR) {
		setR(inR[0], inR[1], inR[2]);
	}
}

void JPLCamera::setE(double *inE) {
	if(inE) {
		setE(inE[0], inE[1], inE[2]);
	}
}

void JPLCamera::setlinearity(double inlin) {
	linearity = inlin;
}

void JPLCamera::setpupiltype(int intype) {
	if(intype != PERSPECTIVE_PUPIL && intype != FISHEYE_PUPIL && intype != GENERAL_PUPIL) {
		fprintf(stderr,"Whoops - Attempt to set invalid pupiltype = %d\n", intype);
		return;
	}
	pupiltype = intype;
}

void JPLCamera::setC(double term1, double term2, double term3) {
	C[0] = term1;
	C[1] = term2;
	C[2] = term3;
}

void JPLCamera::setA(double term1, double term2, double term3) {
	A[0] = term1;
	A[1] = term2;
	A[2] = term3;
}

void JPLCamera::setH(double term1, double term2, double term3) {
	H[0] = term1;
	H[1] = term2;
	H[2] = term3;
}

void JPLCamera::setV(double term1, double term2, double term3) {
	V[0] = term1;
	V[1] = term2;
	V[2] = term3;
}

void JPLCamera::setO(double term1, double term2, double term3) {
	O[0] = term1;
	O[1] = term2;
	O[2] = term3;
	if(modelType != CAHVORE_MODEL) modelType = CAHVOR_MODEL;
}

void JPLCamera::setR(double term1, double term2, double term3) {
	R[0] = term1;
	R[1] = term2;
	R[2] = term3;
	if(modelType != CAHVORE_MODEL) modelType = CAHVOR_MODEL;
}

void JPLCamera::setE(double term1, double term2, double term3) {
	E[0] = term1;
	E[1] = term2;
	E[2] = term3;
	modelType = CAHVORE_MODEL;
}

void JPLCamera::getC(double *outC) {
	getC(outC, outC+1, outC+2);
}

void JPLCamera::getC(double *term1, double *term2, double *term3) {
	if(term1)*term1 = C[0];
	if(term2)*term2 = C[1];
	if(term3)*term3 = C[2];
}

void JPLCamera::getA(double *outA) {
	getA(outA, outA+1, outA+2);
}

void JPLCamera::getA(double *term1, double *term2, double *term3) {
	if(term1)*term1 = A[0];
	if(term2)*term2 = A[1];
	if(term3)*term3 = A[2];
}

void JPLCamera::getH(double *outH) {
	getH(outH, outH+1, outH+2);
}

void JPLCamera::getH(double *term1, double *term2, double *term3) {
	if(term1)*term1 = H[0];
	if(term2)*term2 = H[1];
	if(term3)*term3 = H[2];
}

void JPLCamera::getV(double *outV) {
	getV(outV, outV+1, outV+2);
}

void JPLCamera::getV(double *term1, double *term2, double *term3) {
	if(term1)*term1 = V[0];
	if(term2)*term2 = V[1];
	if(term3)*term3 = V[2];
}

void JPLCamera::getO(double *outO) {
	getO(outO, outO+1, outO+2);
}

void JPLCamera::getO(double *term1, double *term2, double *term3) {
	if(term1)*term1 = O[0];
	if(term2)*term2 = O[1];
	if(term3)*term3 = O[2];
}

void JPLCamera::getR(double *outR) {
	getR(outR, outR+1, outR+2);
}

void JPLCamera::getR(double *term1, double *term2, double *term3) {
	if(term1)*term1 = R[0];
	if(term2)*term2 = R[1];
	if(term3)*term3 = R[2];
}

void JPLCamera::getE(double *outE) {
	getE(outE, outE+1, outE+2);
}

void JPLCamera::getE(double *term1, double *term2, double *term3) {
	if(term1)*term1 = E[0];
	if(term2)*term2 = E[1];
	if(term3)*term3 = E[2];
}

double JPLCamera::getlinearity() {
	return(linearity);
}

int JPLCamera::getpupiltype() {
	return(pupiltype);
}

void JPLCamera::InitJPLCamera (double *inC, double *inA, double *inH, double *inV,
		double *inO, double *inR, double *inE, double inlin) {
	setC(inC);
	setA(inA);
	setH(inH);
	setV(inV);
	setO(inO);
	setR(inR);
	setE(inE);
	linearity = inlin;
}

void JPLCamera::GetInternals(double *hs, double *hc, double *vs, double *vc,
                double *theta) {
	double ltheta;
	double covar[12][12];

	cmod_cahv_internal(C, A, H, V, covar, 
		//hs, hc, vs, vc, &ltheta, (double **)NULL);
		hs, hc, vs, vc, &ltheta, NULL);
	if(theta) *theta = ltheta;
}

long JPLCamera::Point3DToImage2D (double x, double y, double z,
			     double *row, double *col) {
  double P[3], ip[2], range;
  long result;

  P[0] = x; P[1] = y; P[2] = z;

  result = Point3DToImage2D (P, ip, &range, NULL);

  if(range > 0.0) {
  	*col = ip[0]; *row = ip[1];
  } else {
	*col = -1.0; *row = -1.0;
	return PARAM_ERR;
  }

  return result;
}



long JPLCamera::Point3DToImage2D (double P[3], double ip[2], double *range, 
					double **par) {
  if (modelType == CAHV_MODEL) 
    //cmod_cahv_3d_to_2d (P, C, A, H, V, range, ip, par);
    cmod_cahv_3d_to_2d (P, C, A, H, V, range, ip, NULL);
  else if (modelType == CAHVOR_MODEL)
    cmod_cahvor_3d_to_2d (P, C, A, H, V, O, R, FALSE, 
			      range, ip, NULL);
			      //range, ip, par);
  else if (modelType == CAHVORE_MODEL)
    cmod_cahvore_3d_to_2d (P, pupiltype, linearity, C, A, H, V, O, R, E, FALSE, 
			      range, ip, NULL);
			      //range, ip, par);
  else {
    FatalErr ("Wrong camera model type\n");
    return PARAM_ERR;
  }
  
  return NO_ERR;
}



long JPLCamera::Image2DToRay3D (double row, double col, double ray[3]) {
  double ip[2], center[3];

  ip[0] = col; ip[1] = row;
  return Image2DToRay3D (ip, center, ray, NULL);
}



long JPLCamera::Image2DToRay3D (double ip[2], double P[3], double uvec[3], double **par) {
  if (modelType == CAHV_MODEL) 
    cmod_cahv_2d_to_3d (ip, C, A, H, V, P, uvec, NULL);
    //cmod_cahv_2d_to_3d (ip, C, A, H, V, P, uvec, par);
  else if (modelType == CAHVOR_MODEL)
    cmod_cahvor_2d_to_3d (ip, C, A, H, V, O, R, FALSE, 
			      //P, uvec, par);
			      P, uvec, NULL);
  else if (modelType == CAHVORE_MODEL)
    cmod_cahvore_2d_to_3d (ip, pupiltype, linearity, C, A, H, V, O, R, E, FALSE, 
			      P, uvec, NULL, NULL);
			      //P, uvec, par, (double **)NULL);
  else {
    FatalErr ("Wrong camera model type\n");
    return PARAM_ERR;
  }
  
  return NO_ERR;
}
	
#ifdef FULL_MODEL_IMPLEMENTATION
void JPLCamera::InitJPLCamera (long type, double *inC, double *inA,
			  double *inH, double *inV, double *inO, double *inR,
			  double *incenter, double *inscale, double *intheta,
			  double *ins, double *ins_int) {
  int i;
	
  // #define INIT_DEBUG
#ifdef INIT_DEBUG
  fprintf (stderr, "*** Entering InitJPLCamera ***\n");
  fprintf (stderr, "Parameters: type=%lx inC=%lx inA=%lx inH=%lx inV=%lx "
	   "inO=%lx inR=%lx incenter=%lx inscale=%lx "
	   "intheta=%lx ins=%lx ins_int=%lx\n",
	   (long) type, (long) inC, (long) inA, (long) inH, (long) inV,
	   (long) inO, (long) inR, (long) incenter, (long) inscale,
	   (long) intheta, (long) ins, (long) ins_int);
#endif
  modelType = type;
  for (i = 3; i--; ) {
#ifdef INIT_DEBUG
    fprintf (stderr, "  *** Counting down to 0: %d\n", i);
    fprintf (stderr, "C=%lx\n", (long) C);
#endif
    if (inC) C[i] = inC[i];
#ifdef INIT_DEBUG
    fprintf (stderr, "A=%lx\n", (long) A);
#endif
    if (inA) A[i] = inA[i];
#ifdef INIT_DEBUG
    fprintf (stderr, "H=%lx\n", (long) H);
#endif
    if (inH) H[i] = inH[i];
#ifdef INIT_DEBUG
    fprintf (stderr, "V=%lx\n", (long) V);
#endif
    if (inV) V[i] = inV[i];
#ifdef INIT_DEBUG
    fprintf (stderr, "O=%lx\n", (long) O);
#endif
    if (inO) O[i] = inO[i];
#ifdef INIT_DEBUG
    fprintf (stderr, "R=%lx\n", (long) R);
#endif
    if (inR) R[i] = inR[i];
  }
	
#ifdef INIT_DEBUG
  fprintf (stderr, "*** Setting two center/scale values\n");
#endif
  for (i = 2; i-- ; ) {
    if (incenter) center[i] = incenter[i];
    if (inscale) scale[i] = inscale[i];
  }
  
  if (intheta)
    *theta = *intheta;
  
#ifdef INIT_DEBUG
  fprintf (stderr, "*** Setting covariance matrices\n");
#endif
  if (ins)
    for (i = 18 * 18; i--; ) s[i] = ins[i];
#ifdef INIT_DEBUG
  fprintf (stderr, "*** Middle of covariance\n");
#endif
  if (ins_int)
    for (i = 5 * 5; i--; ) s_int[i] = ins_int[i];
#ifdef INIT_DEBUG
  fprintf (stderr, "*** Bottom of InitJPLCamera\n");
#endif
  isValid = 1;
}



long JPLCamera::ReadCameraModel (char *filename) {
  long type = CAHVOR_MODEL;

  if (filename) {
    int len = strlen (filename);

    if (len >= 4 && strcmp (filename+len-4, "cahv") == 0)
      type = CAHV_MODEL;
    else if (len >= 7 && strcmp (filename+len-7, "cahvore") == 0)
      type = CAHVORE_MODEL;
  }

  return ReadCameraModel (filename, type);
}



long JPLCamera::ReadCameraModel (char *filename, long type) {
  FILE *fp;
  int status;

  if (filename == NULL) {
    FatalErr ("ReadCameraModel:  empty filename\n");
    return PARAM_ERR;
  }

  if ((fp = fopen (filename, "r")) == NULL) {
    fprintf (stderr, "ReadCameraModel:  cannot open \"%s\"\n", filename);
    return PARAM_ERR;
  }
  status = ReadCameraModel (fp, type);
  fclose (fp);

  return status;
}



long	JPLCamera::ReadCameraModel (FILE *fp, long type) {
  modelType = type;

#ifdef INIT_DEBUG
  fprintf (stderr, "ReadCameraModel: expecting type %ld\n", type);
#endif
#if defined(RTI_VXWORKS) || defined (__VXWORKS__) || defined (RTS_VXWORKS)
  //	taskSuspend(0);
#endif
  if (modelType == CAHV_MODEL) {
    if (jpl_cmod_cahv_read (fp, C, A, H, V, (double (*)[12])s,
			    &scale[0], &center[0], &scale[1], &center[1],
			    theta, (double (*)[5]) s_int) == FAILURE) {
      return FILE_ERR;
    }
    O[0] = A[0]; O[1] = A[1]; O[2] = A[2];
    R[0] = R[1] = R[2] = 0;
  } else if (modelType == CAHVOR_MODEL) {
#ifdef INIT_DEBUG
    fprintf (stderr, "ReadCameraModel: calling jpl_cmod_cahvor_read\n");
#endif
    
    if (jpl_cmod_cahvor_read(fp, C, A, H, V, O, R,
			     (double (*)[18]) s, &scale[0], &center[0], 
			     &scale[1], &center[1], theta, (double (*)[5]) s_int)
	== FAILURE)
      return FILE_ERR;
  } else {
    FatalErr ("Wrong camera model type\n");
    return PARAM_ERR;
  }
	
  isValid = 1;
  return NO_ERR;
}
	

	
long JPLCamera::WriteCameraModel (char *filename) {
  FILE *fp;
  int status;

    //make sure output file will have proper permissions
    umask(002);
  if ((fp = good_fopen (filename, "w")) == NULL) {
    FatalErr ("Cannot open camera model file for writing\n");
    return PARAM_ERR;
  }
  status = WriteCameraModel (fp);
  fclose (fp);
  return status;
}



long JPLCamera::WriteCameraModel (FILE *fp) {
  if (modelType == CAHV_MODEL) {
    if (jpl_cmod_cahv_write (fp, "Writing CAHV model into a file..",
			     C, A, H, V, (double (*)[12])s,
			     scale[0], center[0], scale[1], center[1],
			     *theta, (double (*)[5]) s_int) == FAILURE) {
      return FILE_ERR;
    }
  } else if (modelType == CAHVOR_MODEL) {
    if (jpl_cmod_cahvor_write(fp, "Writing CAHVOR model into a file..",
			      C, A, H, V, O, R,
			      (double (*)[18]) s, scale[0], center[0], 
			      scale[1], center[1], *theta, (double (*)[5]) s_int)
	== FAILURE)
      return FILE_ERR;
  } else {
    FatalErr ("Wrong camera model type\n");
    return PARAM_ERR;
  }
  
  return NO_ERR;
}
	


void JPLCamera::PrintCameraModel () {
}
	
void JPLCamera::setcenter(double *incenter) {
	if(incenter) {
		for(int i=0; i<2; i++) {
			center[i] = incenter[i]);
		}
	}
}

void JPLCamera::setscale(double *inscale) {
	if(inscale) {
		for(int i=0; i<2; i++) {
			scale[i] = inscale[i]);
		}
	}
}




// utilities
long	
JPLCamera::ResizeImageCoordinate (float new_scale)
{
  int i;
  double *data = (double *) params;
  
  if (params == NULL) return INIT_ERR;
  
  // for H & V
  for (i = 6; i < 12; i++)
    data[i] *= new_scale;
  
  // for centers & scales
  for (i = 18; i < 22; i++)
    data[i] *= new_scale;
  
  return NO_ERR;
}


long JPLCamera::TranslateModel(double T[3])
{
  if (C) {
    C[0] += T[0];
    C[1] += T[1];
    C[2] += T[2];
    return NO_ERR;
  }

  return INIT_ERR;
} // JPLCamera::TranslateModel



// HACK HACK HACK -- This doesn't work yet, don't know why.  the cahvor_move
// sets the models to the same values.

long JPLCamera::RotateModel (double P[3], double vec[3], double rads)
{
  double q1[4], q2[4], nC[3], nA[3], nH[3], nV[3], nO[3], nR[3], ns[21*21];

  quatva (vec, 0.0, q1);
  quatva (vec, rads, q2);

  if (C && A && H && V &&
      (modelType == CAHV_MODEL || (O && R))) {

    switch (modelType) {
    case CAHV_MODEL:
      jpl_cmod_cahv_move (P, q1,  C,  A,  H,  V,
			  P, q2, nC, nA, nH, nV);
      jpl_cmod_cahv_rotate_cov(q1, (double (*)[12]) s, q2,
			       (double (*)[12]) ns);
      break;
    case CAHVOR_MODEL:
      jpl_cmod_cahvor_move (P, q1,  C,  A,  H,  V,  O,  R,
			    P, q2, nC, nA, nH, nV, nO, nR);
      jpl_cmod_cahvor_rotate_cov(q1, (double (*)[18]) s, q2,
				 (double (*)[18]) ns);
      break;
    default:
      fprintf (stderr, "RotateModel:  Don't know about camera model %ld\n",
	       modelType);
      return INIT_ERR;
      break;
    }

    C = nC;
    A = nA;
    H = nH;
    V = nV;
    if (modelType != CAHV_MODEL) {
      O = nO;
      R = nR;
    }
  } else {
    return INIT_ERR;
  }

  return NO_ERR;
} // JPLCamera::RotateModel


// utilities
long	
JPLCamera::ResizeImageCoordinate (float new_scale, JPLCamera *newCam)
{
  CopyCamera (newCam);
  newCam->ResizeImageCoordinate (new_scale);
  
  return NO_ERR;
}



long
JPLCamera::TransformImageCoordinate (float scaleX, float scaleY, 
				     float transX, float transY)
{
  int i;
  
  if (params == NULL) return INIT_ERR;
  
  // for H & V
  for (i = 0; i < 3; i++) {
    H[i] = H[i] * scaleX + A[i] * transX;
    V[i] = V[i] * scaleY + A[i] * transY;
  }	
  
  // **** Need to update centers and scales. to be implemented
  
  return NO_ERR;
}

#if 0
long
JPLCamera::GenerateRectificationTable (long dstRows, long dstCols, JPLCamera *dstCam)
{
  if (dstCam->modelType != CAHV_MODEL) {
    FatalErr ("Cannot warp to a CAHVOR model\n");
    return INIT_ERR;
  }
  
  rows = dstRows;
  cols = dstCols;
  
  if (modelType == CAHV_MODEL) {
    for (int i = 0; i < 3; i++) R[i] = 0.0;
  }
  
  if (rectificationTable) delete rectificationTable;
  
  if (st_warp_bilinear_init_cahvor (
				    (st_cahvor_t *) C, (st_cahv_t *)dstCam->C, 
				    cols, rows, (char **) &rectificationTable)
      != FAILURE)
    return NO_ERR;
  else
    return INTERNAL_ERR;
}
	


// rectify one image using lut
long	
JPLCamera::RectifyImage (JPLPic *srcPic, JPLPic *dstPic)
{
  // test whether it is necessary to generate the rectification table
  if (rectificationTable == NULL ||
      rows != dstPic->rows || cols != dstPic->cols) {
    FatalErr ("Rectification table not initialized\n");
    return INIT_ERR;
  }
  
  if (srcPic->field) 
    st_warp_bilinear_field ((char *) rectificationTable,
			    cols, rows, 0, 0, cols, rows, srcPic->GetPixelAddress(),
			    dstPic->GetPixelAddress());
  else
    st_warp_bilinear ((char *) rectificationTable, 
		      cols, rows, 0, 0, cols, rows, srcPic->GetPixelAddress(),
		      dstPic->GetPixelAddress());
  
  return NO_ERR;
}
#endif



static void
TransposeMatrix (float *A, float *B, short m, short n)
{
  int i, j;
  
  for (i = 0; i < m; i++)
    for (j = 0; j < n; j++, B++)
      *B = A[j*m+i];
}



// A * B = C

#ifdef THIS_IS_UNUSED
static void
MultiplyMatrices (float *A, float *B, float *C, short m, short n, short p)
{
	int i, j, k;
	
	for (i = 0; i < m; i++)
		for (j = 0; j < p; j++, C++) {
			*C = 0.0;
			for (k = 0; k < n; k++)
				*C += A[i*n+k] * B[n*k+j];
		}
}
#endif

/*
 projective matrix mapping from 2D pixel locations to 3D ray directions
 R = (V - y A) x (H - x A) = M0 [x y 1]^T
	(without distortion)

 Re-Projecting:
 distorion coeff:
 tao = [x y 1] M2 [x y 1]^T / ([x y 1] M3 [x y 1]^T) where
      M2 = M0^T M0 - M3 = M0^T M1;
      M3 = M0^T O O^T M0;
      
  lambda = M1 [x y 1]^T
  	  M1 = M0 - O O^T M0;
  
  R' = R + (r0 + r1 * tao + r2 * tao^2) * lambda
  	 = (M0 + (r0 + r1 * tao + r2 * tao^2) * M1) [x y 1]^T
*/

void
JPLCamera::M2D_3D (float M0[9])
{
	float tmpM[9];
	
	M3D_2D(tmpM);
	
	InvertMatrix (tmpM, M0, 3, 3);
		}

void
JPLCamera::M3D_2D (float M0[9])
{
	int i;
	
	for (i = 0; i < 3; i++) {
		M0[i] = H[i];
		M0[3+i] = V[i];
		M0[6+i] = A[i];
	}
}

#define MIN(x, y) ((x) > (y) ? (y) : (x))

//#define COMP_ERR

// rectify one image without the rectification table
long	
JPLCamera::RectifyImage (JPLPic *srcPic, JPLPic *dstPic, JPLCamera *dstCam,
			unsigned char numInterval)
{
  float M0[9], M1[9], M2[9], M3[9]; 
  float dstM0[9], tmpM[9];
  float OF[3];
  int i, piecewise, interval, length;
  float x, y, Z, X, Y, tao, scale;
  float qa, qb, qc, qd, qe, qf;
  float qgx, qhx, qgy, qhy, qgz, qhz;
  float x0, x1, x2, y0, y1, y2;
  float ax, bx, ay, by;
  register long dstX, dstY, fracBits, xLimit, yLimit;
  long bits_scale;
  register long linearx, lineary, qxdd, qydd;
  //	JPLPic    fieldSrcPic, fieldDstPic;
#ifdef COMP_ERR
  float errx, erry, maxErrX = 0.0, maxErrY = 0.0;  
#ifdef COMP_TABLE
  double *tablex, *tabley;
  if (rectificationTable != NULL)
    GetTableAddress (rectificationTable, &tablex, &tabley);
#endif
#endif
	
  M3D_2D (M0);
  dstCam->M2D_3D (dstM0);
    
  dstPic->field = false;

  if (dstCam->modelType != CAHV_MODEL) {
    FatalErr ("Cannot rectify to a distortion model\n");
    return INIT_ERR;
  }
  if (modelType == CAHV_MODEL) {
    float qx, qy, qz;
    long srcRB, dstRB;
    unsigned char *dst;
    register unsigned char *src, *s, *d;
    long row, srcRows, srcCols;
    register long col, count;

    LinearTransform (M0, dstM0, M0, 3, 3, 3);
		
    srcRB = srcPic->GetRowBytes ();
    src = srcPic->GetPixelAddress ();
    dstRB = dstPic->GetRowBytes ();
    dst = dstPic->GetPixelAddress ();
    rows = dstPic->rows;
    cols = dstPic->cols;
    srcRows = srcPic->rows;
    srcCols = srcPic->cols;
    interval = cols / numInterval;
    if ((interval * numInterval) < cols) interval++;
    fracBits = 20;
    bits_scale = 1L << fracBits;
    xLimit = (srcCols - 1) << fracBits;
    yLimit = (srcRows - 1) << fracBits;

    for (row = 0; row < rows; row ++, dst += dstRB) {
      y = row;
			
      qx = M0[1] * y + M0[2];
      qy = M0[4] * y + M0[5];
      qz = M0[7] * y + M0[8];
			
      Z = 1.0 / qz;
      x2 = qx * Z;
      y2 = qy * Z;

      for (piecewise = 0, col = 0, d = dst; piecewise < numInterval;
	   piecewise++) {
	
	length = MIN ((cols - col), interval);
	scale = 1.0 / (float) length;
		
        // first column
	x0 = x2; y0 = y2;
				
	// middle column
	x = col + length * 0.5;
	X = M0[0] * x + qx;
	Y = M0[3] * x + qy;
	Z = M0[6] * x + qz;
	Z = 1.0 / Z;
	x1 = X * Z;
	y1 = Y * Z;
				
	// last column
	x = col + length;
	X = M0[0] * x + qx;
	Y = M0[3] * x + qy;
	Z = M0[6] * x + qz;
	Z = 1.0 / Z;
	x2 = X * Z;
	y2 = Y * Z;
				
	// calculate quadratic coeffs:
	ax = 2.0 * ((x2 - x0) - 2.0 * (x1 - x0)) * scale * scale;
	bx = (4.0 * (x1 - x0) - (x2 - x0)) * scale ;
	ay = 2.0 * ((y2 - y0) - 2.0 * (y1 - y0)) * scale * scale;
	by = (4.0 * (y1 - y0) - (y2 - y0)) * scale;
		
	// setup forward differences:
	dstX = (long int) (x0 * bits_scale);
	dstY = (long int) (y0 * bits_scale);
	linearx = (long int) ((bx + ax) * bits_scale);
	lineary = (long int) ((by + ay) * bits_scale);
	qxdd = (long) (2.0 * ax * bits_scale);
	qydd = (long) (2.0 * ay * bits_scale);

	col += length;
	for (count = length; count--; d++,
	       dstX += linearx, dstY += lineary,
	       linearx += qxdd, lineary += qydd) {
					
	  /* Changed > to >= to make life happier for linear interpolation;
	     not sure it's nexcessary, just in case -- 6 jan 99 yx mwm */

	  if (dstX < 0 || dstX >= xLimit ||
	      dstY < 0 || dstY >= yLimit) {
	    *d = 0;
	  } else {
	    register long ix, iy;
	    register long dx, dy, di0, dj0;
	    register long p00, p01, p11, p10;
					
	    ix = dstX >> fracBits; iy = dstY >> fracBits;
	    dx = (dstX - (ix << fracBits)) >> (fracBits - 8);
	    dy = (dstY - (iy << fracBits)) >> (fracBits - 8);
	    s = src + iy * srcRB + ix;

	    /* Fetch data */
	    p00 = (long int) *s++;
	    p01 = (long int) *s;
	    s += srcRB;
	    p11 = (long int) *s;
	    p10 = (long int) *--s;
	
	    /* Compute weights */
	    di0 = (1L << 8) - dx;
	    dj0 = (1L << 8) - dy;
					
	    // bilinear interpolation
	    *d = (dj0 * (di0 * p00 + dx * p01)
		  + dy  * (di0 * p10 + dx * p11)) >> 16;
	  }			
	}
      }
    }
    
    return NO_ERR;
  }
  
  for (i = 0; i < 3; i++) OF[i] = O[i];
  
  LinearTransform (OF, dstM0, M1, 1, 3, 3); // M1 = O^T dstM0
  LinearTransform (OF, M1, tmpM, 3, 1, 3);  // tmpM = O O^T dstM0
  TransposeMatrix (dstM0, M2, 3, 3); // M2 =dstM0^T
  LinearTransform (M2, tmpM, M3, 3, 3, 3); // M3 = dstM0^T O O^T dstM0
  SubtractMatrix (dstM0, tmpM, M1, 3, 3); // M1 = dstM0 - O O^T dstM0
  LinearTransform (M2, M1, M2, 3, 3, 3); // M2 = dstM0^T (dstM0 - O O^T dstM0)
  
  LinearTransform (M0, M1, M1, 3, 3, 3);
  LinearTransform (M0, dstM0, M0, 3, 3, 3);
  
  long srcRB, dstRB;
  unsigned char *dst;
  register unsigned char *src, *ss, *d;
  long row, nrows, ncols, srcRows, srcCols;
  register long col, count;
  
  srcRB = srcPic->GetRowBytes ();
  src = srcPic->GetPixelAddress ();
  dstRB = dstPic->GetRowBytes ();
  dst = dstPic->GetPixelAddress ();
  nrows = dstPic->rows;
  ncols = dstPic->cols;
  srcRows = srcPic->rows;
  srcCols = srcPic->cols;
  interval = ncols / numInterval;
  if ((interval * numInterval) < ncols) interval++;
  fracBits = 20;
  bits_scale = 1L << fracBits;
  xLimit = (srcCols - 1) << fracBits;
  yLimit = (srcRows - 1) << fracBits;
  
  for (row = 0; row < nrows; row ++, dst += dstRB) {
    y = row;
    
    /* quadrtic coeffs
       tao = (qa * x^2 + qb * x + qc) / (qd * x^2 + qe * x + qf)
    */
    qa = M2[0];
    qb = (M2[1] + M2[3]) * y + M2[2] + M2[6];
    qc = M2[4] * y * y + (M2[5] + M2[7]) * y + M2[8];
    qd = M3[0];
    qe = (M3[1] + M3[3]) * y + (M3[2] + M3[6]);
    qf = M3[4] * y * y + (M3[5] + M3[7]) * y + M3[8];
    qgx = M0[1] * y + M0[2];
    qhx = M1[1] * y + M1[2];
    qgy = M0[4] * y + M0[5];
    qhy = M1[4] * y + M1[5];
    qgz = M0[7] * y + M0[8];
    qhz = M1[7] * y + M1[8];
    
    // first column
    tao = qc / qf;
    tao = R[0] + R[1] * tao + R[2] * tao * tao;
    X = qgx + tao * qhx;//(M0[1] + tao * M1[1]) * y + (M0[2] + tao * M1[2]);
    Y = qgy + tao * qhy; //(M0[4] + tao * M1[4]) * y + (M0[5] + tao * M1[5]);
    Z = 1.0 / (qgz + tao * qhz); //(M0[7] + tao * M1[7]) * y + (M0[8] + tao * M1[8]);
    x2 = X * Z; 
    y2 = Y * Z;
    
    for (piecewise = 0, col = 0, d = dst; piecewise < numInterval; piecewise++) {
      
      length = MIN ((ncols - col), interval);
      scale = 1.0 / (float) length;
      
      // first column
      x0 = x2; y0 = y2;
      
      // middle column
      x = col + length * 0.5;
      tao = ((qa * x + qb) * x + qc) / ((qd * x + qe) * x + qf);
      tao = R[0] + (R[1] + R[2] * tao) * tao;
      X = (M0[0] + tao * M1[0]) * x + qgx + tao * qhx;
      Y = (M0[3] + tao * M1[3]) * x + qgy + tao * qhy;
      Z = 1.0 / ((M0[6] + tao * M1[6]) * x + qgz + tao * qhz);
      x1 = X * Z; y1 = Y * Z;
      
      // last column
      x = col+length;
      tao = ((qa * x + qb) * x + qc) / ((qd * x + qe) * x + qf);
      tao = R[0] + (R[1] + R[2] * tao) * tao;
      X = (M0[0] + tao * M1[0]) * x + qgx + tao * qhx;
      Y = (M0[3] + tao * M1[3]) * x + qgy + tao * qhy;
      Z = 1.0 / ((M0[6] + tao * M1[6]) * x + qgz + tao * qhz);
      x2 = X * Z; y2 = Y * Z;
      
      // calculate quadratic coeffs:
      ax = 2.0 * ((x2 - x0) - 2.0 * (x1 - x0)) * scale * scale;
      bx = (4.0 * (x1 - x0) - (x2 - x0)) * scale ;
      ay = 2.0 * ((y2 - y0) - 2.0 * (y1 - y0)) * scale * scale;
      by = (4.0 * (y1 - y0) - (y2 - y0)) * scale;
      
      // setup forward differences:
      dstX = (long) (x0 * bits_scale);
      dstY = (long) (y0 * bits_scale);
      linearx = (long) ((bx + ax) * bits_scale);
      lineary = (long) ((by + ay) * bits_scale);
      qxdd = (long) (2.0 * ax * bits_scale);
      qydd = (long) (2.0 * ay * bits_scale);
      
      for (count = length; count--; d++, col++,
	     dstX += linearx, dstY += lineary,
	     linearx += qxdd, lineary += qydd) {
	/*				
	x = length - 1 - count;
	dstX = (x0 + ax * x * x + bx * x) * bits_scale;
	dstY = (y0 + ay * x * x + by * x) * bits_scale;
	*/
#ifdef COMP_ERR
	x = col;
	tao = (qa * x * x + qb * x + qc) / 
	  (qd * x * x + qe * x + qf);
	tao = R[0] + R[1] * tao + R[2] * tao * tao;
	
	X = (M0[0] + tao * M1[0]) * x +
	  (M0[1] + tao * M1[1]) * y +
	  (M0[2] + tao * M1[2]);
	Y = (M0[3] + tao * M1[3]) * x +
	  (M0[4] + tao * M1[4]) * y +
	  (M0[5] + tao * M1[5]);
	Z = (M0[6] + tao * M1[6]) * x +
	  (M0[7] + tao * M1[7]) * y +
	  (M0[8] + tao * M1[8]);
	
	Z = 1.0 / Z;
	X *= Z;
	Y *= Z;
	errx = X - (float) dstX / bits_scale;
	erry = Y - (float) dstY / bits_scale;
#ifdef COMP_TABLE
	if (rectificationTable != NULL) {
	  errx = *tablex++ - (float) dstX / bits_scale;
	  erry = *tabley++ - (float) dstY / bits_scale;
	}
#endif
	if (fabs(errx) > maxErrX) maxErrX = fabs(errx);
	if (fabs(erry) > maxErrY) maxErrY = fabs(erry);
	if (row == 0) {
	  printf ("Error: (%f, %f) at col: %d\n",
		  errx, erry, col);
	}
#endif	
	
	if (dstX < 0 || dstX >= xLimit ||
	    dstY < 0 || dstY >= yLimit) {
	  *d = 0;
	} else {
	  register long ix, iy;
	  register long dx, dy, di0, dj0;
	  register long p00, p01, p11, p10;
	  
	  ix = dstX >> fracBits; iy = dstY >> fracBits;
	  dx = (dstX - (ix << fracBits)) >> (fracBits - 8);
	  dy = (dstY - (iy << fracBits)) >> (fracBits - 8);
	  ss = src + iy * srcRB + ix;
	  
	  /* Fetch data */
	  p00 = *ss++;
	  p01 = *ss;
	  ss += srcRB;
	  p11 = *ss;
	  p10 = *--ss;
	  
	  /* Compute weights */
	  di0 = (1L << 8) - dx;
	  dj0 = (1L << 8) - dy;
					
	  // bilinear interpolation
	  *d = (dj0 * (di0 * p00 + dx * p01)
		+ dy  * (di0 * p10 + dx * p11)) >> 16;
	}			
      }
    }
  }
  
#ifdef COMP_ERR	
  printf("Maximal errors: (%f, %f)\n", maxErrX,maxErrY);
#endif
  return NO_ERR;
}

/* given the following constraints:
   the nodal position of the left camera
   the basline orientation
   the pointing direction projected on the plane perpendicular
   to the baseline.
 */

long
JPLCamera::TransformStereoCameras (
			JPLCamera *rightCam,
			JPLCamera *leftResult, 
			JPLCamera *rightResult,
			float leftNodalPos[3],
			float leftToRightDirection[3],
			float leftPointingDirection[3])
{
  double P[3], icenter[2], vec[3], base[3], norm, lr[3], tmpC[3];
  double rot[3][3], tmp[3][3], tmp1[3][3], up[3], upRef[3];
  int i;
  
  for (i = 0; i < 3; i++) {
    leftResult->C[i] = leftNodalPos[i];
    tmpC[i] = rightCam->C[i] + leftNodalPos[i] - C[i];
    base[i] = rightCam->C[i] - C[i];
  }

  // tmpC is  leftNodalPos plus the baseline (base).
  // base is the baseline

  // The baseline direction
  norm = 0.0;
  for (i = 0; i < 3; i++)
    norm += base[i] * base[i];
  if (norm == 0.0) {
    FatalErr ("Two cameras are at the same place\n");
    return INIT_ERR;
  }
  norm = 1.0 / sqrt(norm);
  for (i = 0; i < 3; i++)
    base[i] *= norm;

  // base is now a unit vector
rows = 486; cols = 512;

  icenter[0] = (cols - 1.0) * 0.5;
  icenter[1] = (rows - 1.0) * 0.5;

  Image2DToRay3D (icenter, P, vec, NULL);

  // overwrites the computed image center ray (vec) with A???
#ifdef USE_A_NOT_CENTER
  vec[0] = A[0];
  vec[1] = A[1];
  vec[2] = A[2];
#endif

  printf("pointing: %f %f %f\n", vec[0], vec[1], vec[2]);
  norm = 0.0;
  for (i = 0; i < 3; i++)
    norm += vec[i] * base[i];
  for (i = 0; i < 3; i++)
    vec[i] -= norm * base[i];

  // vec now omits all components of A in base direction

  norm = 0.0;
  for (i = 0; i < 3; i++)
    norm += vec[i] * vec[i];
  if (norm == 0.0) {
    FatalErr ("camera pointing direction is baseline\n");
    return INIT_ERR;
  }
  norm = 1.0 / sqrt(norm);
  for (i = 0; i < 3; i++)
    vec[i] *= norm;
  // vec now unit vector

  /* orthogonalize the baseline and pointing direction in inputs */
  norm = 0.0;
  for (i = 0; i < 3; i++)
    norm += leftToRightDirection[i] * leftPointingDirection[i];
  for (i = 0; i < 3; i++)
    P[i] = leftPointingDirection[i] - norm * leftToRightDirection[i];

  // P now the desired leftPointingDirection without any components in 
  // desired leftToRightDir (i.e. desired baseline)

  norm = 0.0;
  for (i = 0; i < 3; i++)
    norm += P[i] * P[i];
  if (norm == 0.0) {
    FatalErr ("camera pointing direction is baseline\n");
    return INIT_ERR;
  }
  norm = 1.0 / sqrt(norm);
  for (i = 0; i < 3; i++)
    P[i] *= norm;

  // P now unit vector
  norm = 0.0;
  for (i = 0; i < 3; i++)
    norm += leftToRightDirection[i] * leftToRightDirection[i];
  if (norm == 0.0) {
    FatalErr ("Incorrect baseline direction\n");
    return INIT_ERR;
  }
  norm = 1.0 / sqrt(norm);
  for (i = 0; i < 3; i++)
    lr[i] = leftToRightDirection[i] * norm;

  // lr now leftToRightDirection unit vector

  printf("Target base = %f %f %f\n", lr[0], lr[1], lr[2]);
  printf("Target pointing = %f %f %f\n", P[0], P[1], P[2]);
  printf("Current base = %f %f %f\n", base[0], base[1], base[2]);
  printf("Current pointing = %f %f %f\n", vec[0], vec[1], vec[2]);
  /* We need a rotation matrix to map:
     base[] ---> lr[]
     vec[]  ---> P[]
     
     At this point the vectors are all orthonormal.
  */
  cross3 (lr, P, upRef);
  cross3 (base, vec, up);
  for (i = 0; i < 3; i++) {
    /* rot matrix from canonical to input frame */
    tmp[i][0] = base[i];
    tmp[i][1] = vec[i];
    tmp[i][2] = up[i];

    /* rot matrix from reference to canonical frame */
    tmp1[0][i] = lr[i];
    tmp1[1][i] = P[i];
    tmp1[2][i] = upRef[i];
  }
  mult333 (tmp, tmp1, rot);

  printf("rot = \t %f %f %f\n\t\t %f %f %f\n\t\t %f %f %f\n",
	 rot[0][0], rot[0][1], rot[0][2],
	 rot[1][0], rot[1][1], rot[1][2],
	 rot[2][0], rot[2][1], rot[2][2]);

  /* rotating the camera models */
  mult133 (A, rot, leftResult->A);
  mult133 (H, rot, leftResult->H);
  mult133 (V, rot, leftResult->V);
  if (modelType == CAHVOR_MODEL)
    mult133 (O, rot, leftResult->O);
  for (i = 0; i < 3; i++)
    leftResult->R[i] = R[i];
  for (i = 0; i < 3; i++)
    tmpC[i] = rightCam->C[i] - C[i];
  mult133 (tmpC, rot, rightResult->C);
  for (i = 0; i < 3; i++)
    rightResult->C[i] += leftResult->C[i];
  mult133 (rightCam->A, rot, rightResult->A);
  mult133 (rightCam->H, rot, rightResult->H);
  mult133 (rightCam->V, rot, rightResult->V);
  if (modelType == CAHVOR_MODEL)
    mult133 (rightCam->O, rot, rightResult->O);
  for (i = 0; i < 3; i++)
    rightResult->R[i] = rightCam->R[i];

  for (i = 0; i < 2; i++) {
    leftResult->center[i] = center[i];
    rightResult->center[i] = rightCam->center[i];
    
    leftResult->scale[i] = scale[i];
    rightResult->scale[i] = rightCam->scale[i];
  }

  *(leftResult->theta) = *theta;
  *(rightResult->theta) = *(rightCam->theta);
  for (i = 0; i < 18 * 18; i++){
    leftResult->s[i] = 0.0;
    rightResult->s[i] = 0.0;
  }
  for (i = 0; i < 5 * 5 ; i++){
    leftResult->s_int[i] = 0.0;
    rightResult->s_int[i] = 0.0;
  }

  return NO_ERR;
}
#endif //FULL_MODEL_IMPLEMENTATION

int JPLCamera::self_test() {
	fprintf(stderr,"Running JPLCamera::self_test\n");
	int	status = TRUE;
	double	xy[2];
	double c[3] = {-0.143069,-0.164667,-0.801773};
	double a[3] = {-0.248218,-0.857327,0.450975};
	double h[3] = {1036.24,-747.488,218.579};
	double v[3] = {47.4085,168.217,1249.87};
	double o[3] = {-0.248218,-0.857327,0.450975};
	double r[3] = {0.0,0.0,0.0};
	double pos3[3], uvec3[3];
	double az, el;
	JPLCamera *cam_model = new JPLCamera();

	// test initialization method and most set methods
	cam_model->InitJPLCamera(c, a, h, v);

	if(cam_model->getmodelType() != CAHV_MODEL) {
		fprintf(stderr,"Whoops - JPLCamera::self_test failed.\n");
		fprintf(stderr,"Expecting camera model to be CAHV_MODEL = %d but was %d\n",
			CAHV_MODEL, cam_model->getmodelType());
		status = FALSE;
	}

	if(strcmp(cam_model->getCameraModelName(), "CAHV_Model")) {
		fprintf(stderr,"Whoops - JPLCamera::self_test failed.\n");
		fprintf(stderr,"Expecting camera model name to be CAHV_Model but was %s\n",
			cam_model->getCameraModelName());
		status = FALSE;
	}

	if(cam_model->getCameraX() != -0.143069) {
		fprintf(stderr,"Whoops - JPLCamera::self_test failed.\n");
		fprintf(stderr,"Expecting camera X to be %f but was %f\n",
			-0.143069, cam_model->getCameraX());
		status = FALSE;
	}

	if(cam_model->getCameraY() != -0.164667) {
		fprintf(stderr,"Whoops - JPLCamera::self_test failed.\n");
		fprintf(stderr,"Expecting camera Y to be %f but was %f\n",
			-0.164667, cam_model->getCameraY());
		status = FALSE;
	}

	if(cam_model->getCameraZ() != -0.801773) {
		fprintf(stderr,"Whoops - JPLCamera::self_test failed.\n");
		fprintf(stderr,"Expecting camera Z to be %f but was %f\n",
			-0.801773, cam_model->getCameraZ());
		status = FALSE;
	}

	// check out the projection methods
	xy[0] = 1024.0; xy[1] = 0.0;  // should give (0.123536, -0.984082, 0.127759)
	if(cam_model->Image2DToRay3D(xy, pos3, uvec3) != NO_ERR) {
		fprintf(stderr,"Whoops - JPLCamera::self_test failed.\n");
		fprintf(stderr,"Returned status from Image2DToRay3D indicates error.\n");
		status = FALSE;
	}

	if(pos3[0] != -0.143069 || pos3[1] != -0.164667 || pos3[2] != -0.801773) {
		fprintf(stderr,"Whoops - JPLCamera::self_test failed.\n");
		fprintf(stderr,"Expected returned camera position to be (%f,%f,%f)\n",
			0.123536, -0.984082, 0.127759);
		fprintf(stderr,"but got (%f,%f,%f)\n", pos3[0], pos3[1], pos3[2]);
		status = FALSE;
	}

	if(uvec3[0] >= 0.123537 || uvec3[1] >= -0.984081 || uvec3[2] >= 0.127760 ||
	   uvec3[0] <= 0.123535 || uvec3[1] <= -0.984083 || uvec3[2] <= 0.127758) {
		fprintf(stderr,"Whoops - JPLCamera::self_test failed.\n");
		fprintf(stderr,"Expected returned ray vector to be (%f,%f,%f)\n",
			0.123536, -0.984082, 0.127759);
		fprintf(stderr,"but got (%f,%f,%f)\n", uvec3[0], uvec3[1], uvec3[2]);
		status = FALSE;
	}

	xy[0] = 1024.0; xy[1] = 0.0;  // should give (0.123536, -0.984082, 0.127759)
	if(cam_model->Image2DToRay3D(xy[1], xy[0], uvec3) != NO_ERR) {
		fprintf(stderr,"Whoops - JPLCamera::self_test failed.\n");
		fprintf(stderr,"Returned status from Image2DToRay3D indicates error.\n");
		status = FALSE;
	}

	if(uvec3[0] >= 0.123537 || uvec3[1] >= -0.984081 || uvec3[2] >= 0.127760 ||
	   uvec3[0] <= 0.123535 || uvec3[1] <= -0.984083 || uvec3[2] <= 0.127758) {
		fprintf(stderr,"Whoops - JPLCamera::self_test failed.\n");
		fprintf(stderr,"Expected returned ray vector to be (%f,%f,%f)\n",
			0.123536, -0.984082, 0.127759);
		fprintf(stderr,"but got (%f,%f,%f)\n", uvec3[0], uvec3[1], uvec3[2]);
		status = FALSE;
	}

	xy[0] = 1024.0; xy[1] = 0.0;  // should give (az=-1.445916, el=-0.128110)
	if(cam_model->Image2DToAzEl(xy[1], xy[0], &az, &el) != NO_ERR) {
		fprintf(stderr,"Whoops - JPLCamera::self_test failed.\n");
		fprintf(stderr,"Returned status from Image2DToAzEl indicates error.\n");
		status = FALSE;
	}

	if(az >= -1.445915 || az <= -1.445917 || el >= -0.128109 || el <= -0.128111) {
		fprintf(stderr,"Whoops - JPLCamera::self_test failed.\n");
		fprintf(stderr,"Expected returned azimuth and elevation to be (%f,%f)\n",
			-1.445916, -0.128110);
		fprintf(stderr,"but got (%f,%f)\n", az, el);
		status = FALSE;
	}

	if(cam_model->Point3DToImage2D(0.0, -100.0, 30.0, xy+1, xy) != NO_ERR) {
		fprintf(stderr,"Whoops - JPLCamera::self_test failed.\n");
		fprintf(stderr,"Returned status from Point3DToImage2D indicates error.\n");
		status = FALSE;
	}

	if(xy[0] >= 819.599546 || xy[1] >= 218.317583 ||
	   xy[0] <= 819.599544 || xy[1] <= 218.317581) {
		fprintf(stderr,"Whoops - JPLCamera::self_test failed.\n");
		fprintf(stderr,"Expected returned image xy to be (%f,%f)\n",
			819.599545, 218.317582);
		fprintf(stderr,"but got (%f,%f)\n", xy[0], xy[1]);
		status = FALSE;
	}

	// now project the image point out to az, el and compare to 3D az,el
	if(cam_model->Image2DToAzEl(xy[1], xy[0], &az, &el) != NO_ERR) {
		fprintf(stderr,"Whoops - JPLCamera::self_test failed.\n");
		fprintf(stderr,"Returned status from Image2DToAzEl indicates error.\n");
		status = FALSE;
	}

	if(cam_model->Point3DToAzElRng(0.0, -100.0, 30.0, pos3, pos3+1, pos3+2) != NO_ERR) {
		fprintf(stderr,"Whoops - JPLCamera::self_test failed.\n");
		fprintf(stderr,"Returned status from Point3DToAzElRng indicates error.\n");
		status = FALSE;
	}

	if(az >= pos3[0]+0.000001 || az <= pos3[0]-0.000001 || el >= pos3[1]+0.000001 || el <= pos3[1]-0.000001) {
		fprintf(stderr,"Whoops - JPLCamera::self_test failed.\n");
		fprintf(stderr,"Expected returned az,el from Point3DToAzElRng (%f,%f)\n",
			pos3[0], pos3[1]);
		fprintf(stderr,"to match az,el from Image2DToAzEl (%f,%f)\n", az, el);
		status = FALSE;
	}


	// test projecting image point to 3D space
	// basically, we project an image point 10000 units into 3D space
	// then we backproject the 3D point to az, el, range (the ranges should match)
	// the we reporject the image point to az, el and verify that those match too
	if(cam_model->Image2DToPoint3D(xy[1], xy[0], 10000.0, pos3) != NO_ERR) {
		fprintf(stderr,"Whoops - JPLCamera::self_test failed.\n");
		fprintf(stderr,"Returned status from Image2DToPoint3D indicates error.\n");
		status = FALSE;
	}

	if(cam_model->Point3DToAzElRng(pos3[0], pos3[1], pos3[2], uvec3, uvec3+1, uvec3+2) != NO_ERR) {
		fprintf(stderr,"Whoops - JPLCamera::self_test failed.\n");
		fprintf(stderr,"Returned status from Point3DToAzElRng indicates error.\n");
		status = FALSE;
	}

	if(cam_model->Image2DToAzEl(xy[1], xy[0], &az, &el) != NO_ERR) {
		fprintf(stderr,"Whoops - JPLCamera::self_test failed.\n");
		fprintf(stderr,"Returned status from Image2DToAzEl indicates error.\n");
		status = FALSE;
	}

	if(az >= uvec3[0]+0.000001 || az <= uvec3[0]-0.000001 || el >= uvec3[1]+0.000001 || el <= uvec3[1]-0.000001) {
		fprintf(stderr,"Whoops - JPLCamera::self_test failed.\n");
		fprintf(stderr,"Expected returned az,el from Point3DToAzElRng (%f,%f)\n",
			pos3[0], pos3[1]);
		fprintf(stderr,"to match az,el from Image2DToAzEl (%f,%f)\n", az, el);
		status = FALSE;
	}

	if(uvec3[2] <= 10000.0-0.000001 || uvec3[2] >= 10000.0+0.000001) {
		fprintf(stderr,"Whoops - JPLCamera::self_test failed.\n");
		fprintf(stderr,"Expected returned range from Point3DToAzElRng (%f)\n",
			uvec3[2]);
		fprintf(stderr,"to match range input to Image2DToPoint3D (%f)\n", uvec3[2]);
		status = FALSE;
	}

	
	cam_model->setO(o);
	cam_model->setR(r);

	if(cam_model->getmodelType() != CAHVOR_MODEL) {
		fprintf(stderr,"Whoops - JPLCamera::self_test failed.\n");
		fprintf(stderr,"After conversion to CAHVOR_MODEL...\n");
		fprintf(stderr,"Expecting camera model to be CAHVOR_MODEL = %d but was %d\n",
			CAHVOR_MODEL, cam_model->getmodelType());
		status = FALSE;
	}

	if(strcmp(cam_model->getCameraModelName(), "CAHVOR_Model")) {
		fprintf(stderr,"Whoops - JPLCamera::self_test failed.\n");
		fprintf(stderr,"After conversion to CAHVOR_MODEL...\n");
		fprintf(stderr,"Expecting camera model name to be CAHVOR_Model but was %s\n",
			cam_model->getCameraModelName());
		status = FALSE;
	}

	if(cam_model->getCameraX() != -0.143069) {
		fprintf(stderr,"Whoops - JPLCamera::self_test failed.\n");
		fprintf(stderr,"After conversion to CAHVOR_MODEL...\n");
		fprintf(stderr,"Expecting camera X to be %f but was %f\n",
			-0.143069, cam_model->getCameraX());
		status = FALSE;
	}

	if(cam_model->getCameraY() != -0.164667) {
		fprintf(stderr,"Whoops - JPLCamera::self_test failed.\n");
		fprintf(stderr,"After conversion to CAHVOR_MODEL...\n");
		fprintf(stderr,"Expecting camera Y to be %f but was %f\n",
			-0.164667, cam_model->getCameraY());
		status = FALSE;
	}

	if(cam_model->getCameraZ() != -0.801773) {
		fprintf(stderr,"Whoops - JPLCamera::self_test failed.\n");
		fprintf(stderr,"After conversion to CAHVOR_MODEL...\n");
		fprintf(stderr,"Expecting camera Z to be %f but was %f\n",
			-0.801773, cam_model->getCameraZ());
		status = FALSE;
	}

	// check out the projection methods
	xy[0] = 1024.0; xy[1] = 0.0;  // should give (0.123536, -0.984082, 0.127759)
	if(cam_model->Image2DToRay3D(xy, pos3, uvec3) != NO_ERR) {
		fprintf(stderr,"Whoops - JPLCamera::self_test failed.\n");
		fprintf(stderr,"After conversion to CAHVOR_MODEL...\n");
		fprintf(stderr,"Returned status from Image2DToRay3D indicates error.\n");
		status = FALSE;
	}

	if(pos3[0] != -0.143069 || pos3[1] != -0.164667 || pos3[2] != -0.801773) {
		fprintf(stderr,"Whoops - JPLCamera::self_test failed.\n");
		fprintf(stderr,"After conversion to CAHVOR_MODEL...\n");
		fprintf(stderr,"Expected returned camera position to be (%f,%f,%f)\n",
			0.123536, -0.984082, 0.127759);
		fprintf(stderr,"but got (%f,%f,%f)\n", pos3[0], pos3[1], pos3[2]);
		status = FALSE;
	}

	if(uvec3[0] >= 0.123537 || uvec3[1] >= -0.984081 || uvec3[2] >= 0.127760 ||
	   uvec3[0] <= 0.123535 || uvec3[1] <= -0.984083 || uvec3[2] <= 0.127758) {
		fprintf(stderr,"Whoops - JPLCamera::self_test failed.\n");
		fprintf(stderr,"After conversion to CAHVOR_MODEL...\n");
		fprintf(stderr,"Expected returned ray vector to be (%f,%f,%f)\n",
			0.123536, -0.984082, 0.127759);
		fprintf(stderr,"but got (%f,%f,%f)\n", uvec3[0], uvec3[1], uvec3[2]);
		status = FALSE;
	}

	xy[0] = 1024.0; xy[1] = 0.0;  // should give (0.123536, -0.984082, 0.127759)
	if(cam_model->Image2DToRay3D(xy[1], xy[0], uvec3) != NO_ERR) {
		fprintf(stderr,"Whoops - JPLCamera::self_test failed.\n");
		fprintf(stderr,"After conversion to CAHVOR_MODEL...\n");
		fprintf(stderr,"Returned status from Image2DToRay3D indicates error.\n");
		status = FALSE;
	}

	if(uvec3[0] >= 0.123537 || uvec3[1] >= -0.984081 || uvec3[2] >= 0.127760 ||
	   uvec3[0] <= 0.123535 || uvec3[1] <= -0.984083 || uvec3[2] <= 0.127758) {
		fprintf(stderr,"Whoops - JPLCamera::self_test failed.\n");
		fprintf(stderr,"After conversion to CAHVOR_MODEL...\n");
		fprintf(stderr,"Expected returned ray vector to be (%f,%f,%f)\n",
			0.123536, -0.984082, 0.127759);
		fprintf(stderr,"but got (%f,%f,%f)\n", uvec3[0], uvec3[1], uvec3[2]);
		status = FALSE;
	}

	xy[0] = 1024.0; xy[1] = 0.0;  // should give (az=-1.445916, el=-0.128110)
	if(cam_model->Image2DToAzEl(xy[1], xy[0], &az, &el) != NO_ERR) {
		fprintf(stderr,"Whoops - JPLCamera::self_test failed.\n");
		fprintf(stderr,"After conversion to CAHVOR_MODEL...\n");
		fprintf(stderr,"Returned status from Image2DToAzEl indicates error.\n");
		status = FALSE;
	}

	if(az >= -1.445915 || az <= -1.445917 || el >= -0.128109 || el <= -0.128111) {
		fprintf(stderr,"Whoops - JPLCamera::self_test failed.\n");
		fprintf(stderr,"After conversion to CAHVOR_MODEL...\n");
		fprintf(stderr,"Expected returned azimuth and elevation to be (%f,%f)\n",
			-1.445916, -0.128110);
		fprintf(stderr,"but got (%f,%f)\n", az, el);
		status = FALSE;
	}

	if(cam_model->Point3DToImage2D(0.0, -100.0, 30.0, xy+1, xy) != NO_ERR) {
		fprintf(stderr,"Whoops - JPLCamera::self_test failed.\n");
		fprintf(stderr,"After conversion to CAHVOR_MODEL...\n");
		fprintf(stderr,"Returned status from Point3DToImage2D indicates error.\n");
		status = FALSE;
	}

	if(xy[0] >= 819.599546 || xy[1] >= 218.317583 ||
	   xy[0] <= 819.599544 || xy[1] <= 218.317581) {
		fprintf(stderr,"Whoops - JPLCamera::self_test failed.\n");
		fprintf(stderr,"After conversion to CAHVOR_MODEL...\n");
		fprintf(stderr,"Expected returned image xy to be (%f,%f)\n",
			819.599545, 218.317582);
		fprintf(stderr,"but got (%f,%f)\n", xy[0], xy[1]);
		status = FALSE;
	}

	// now project the image point out to az, el and compare to 3D az,el
	if(cam_model->Image2DToAzEl(xy[1], xy[0], &az, &el) != NO_ERR) {
		fprintf(stderr,"Whoops - JPLCamera::self_test failed.\n");
		fprintf(stderr,"After conversion to CAHVOR_MODEL...\n");
		fprintf(stderr,"Returned status from Image2DToAzEl indicates error.\n");
		status = FALSE;
	}

	if(cam_model->Point3DToAzElRng(0.0, -100.0, 30.0, pos3, pos3+1, pos3+2) != NO_ERR) {
		fprintf(stderr,"Whoops - JPLCamera::self_test failed.\n");
		fprintf(stderr,"After conversion to CAHVOR_MODEL...\n");
		fprintf(stderr,"Returned status from Point3DToAzElRng indicates error.\n");
		status = FALSE;
	}

	if(az >= pos3[0]+0.000001 || az <= pos3[0]-0.000001 || el >= pos3[1]+0.000001 || el <= pos3[1]-0.000001) {

		fprintf(stderr,"Whoops - JPLCamera::self_test failed.\n");
		fprintf(stderr,"After conversion to CAHVOR_MODEL...\n");
		fprintf(stderr,"Expected returned az,el from Point3DToAzElRng (%f,%f)\n",
			pos3[0], pos3[1]);
		fprintf(stderr,"to match az,el from Image2DToAzEl (%f,%f)\n", az, el);
		status = FALSE;
	}

	// test the get methods 
	cam_model->getC(pos3);
	if(pos3[0] != c[0] || pos3[1] != c[1] || pos3[2] != c[2]) {
		fprintf(stderr,"Whoops - JPLCamera::self_test failed.\n");
		fprintf(stderr,"Expected getC to return (%f,%f,%f)\n",
			c[0], c[1],  c[2]);
		fprintf(stderr,"but got (%f,%f,%f)\n",
			pos3[0], pos3[1],  pos3[2]);
		status = FALSE;
	}

	cam_model->getA(pos3);
	if(pos3[0] != a[0] || pos3[1] != a[1] || pos3[2] != a[2]) {
		fprintf(stderr,"Whoops - JPLCamera::self_test failed.\n");
		fprintf(stderr,"Expected getA to return (%f,%f,%f)\n",
			a[0], a[1],  a[2]);
		fprintf(stderr,"but got (%f,%f,%f)\n",
			pos3[0], pos3[1],  pos3[2]);
		status = FALSE;
	}

	cam_model->getH(pos3);
	if(pos3[0] != h[0] || pos3[1] != h[1] || pos3[2] != h[2]) {
		fprintf(stderr,"Whoops - JPLCamera::self_test failed.\n");
		fprintf(stderr,"Expected getH to return (%f,%f,%f)\n",
			h[0], h[1],  h[2]);
		fprintf(stderr,"but got (%f,%f,%f)\n",
			pos3[0], pos3[1],  pos3[2]);
		status = FALSE;
	}

	cam_model->getV(pos3);
	if(pos3[0] != v[0] || pos3[1] != v[1] || pos3[2] != v[2]) {
		fprintf(stderr,"Whoops - JPLCamera::self_test failed.\n");
		fprintf(stderr,"Expected getV to return (%f,%f,%f)\n",
			v[0], v[1],  v[2]);
		fprintf(stderr,"but got (%f,%f,%f)\n",
			pos3[0], pos3[1],  pos3[2]);
		status = FALSE;
	}

	cam_model->getO(pos3);
	if(pos3[0] != o[0] || pos3[1] != o[1] || pos3[2] != o[2]) {
		fprintf(stderr,"Whoops - JPLCamera::self_test failed.\n");
		fprintf(stderr,"Expected getO to return (%f,%f,%f)\n",
			o[0], o[1],  o[2]);
		fprintf(stderr,"but got (%f,%f,%f)\n",
			pos3[0], pos3[1],  pos3[2]);
		status = FALSE;
	}

	cam_model->getR(pos3);
	if(pos3[0] != r[0] || pos3[1] != r[1] || pos3[2] != r[2]) {
		fprintf(stderr,"Whoops - JPLCamera::self_test failed.\n");
		fprintf(stderr,"Expected getR to return (%f,%f,%f)\n",
			r[0], r[1],  r[2]);
		fprintf(stderr,"but got (%f,%f,%f)\n",
			pos3[0], pos3[1],  pos3[2]);
		status = FALSE;
	}

	// test projecting image point to 3D space
	// basically, we project an image point 10000 units into 3D space
	// then we backproject the 3D point to az, el, range (the ranges should match)
	// the we reporject the image point to az, el and verify that those match too
	if(cam_model->Image2DToPoint3D(xy[1], xy[0], 10000.0, pos3) != NO_ERR) {
		fprintf(stderr,"Whoops - JPLCamera::self_test failed.\n");
		fprintf(stderr,"After conversion to CAHVOR_MODEL...\n");
		fprintf(stderr,"Returned status from Image2DToPoint3D indicates error.\n");
		status = FALSE;
	}

	if(cam_model->Point3DToAzElRng(pos3[0], pos3[1], pos3[2], uvec3, uvec3+1, uvec3+2) != NO_ERR) {
		fprintf(stderr,"Whoops - JPLCamera::self_test failed.\n");
		fprintf(stderr,"After conversion to CAHVOR_MODEL...\n");
		fprintf(stderr,"Returned status from Point3DToAzElRng indicates error.\n");
		status = FALSE;
	}

	if(cam_model->Image2DToAzEl(xy[1], xy[0], &az, &el) != NO_ERR) {
		fprintf(stderr,"Whoops - JPLCamera::self_test failed.\n");
		fprintf(stderr,"After conversion to CAHVOR_MODEL...\n");
		fprintf(stderr,"Returned status from Image2DToAzEl indicates error.\n");
		status = FALSE;
	}

	if(az >= uvec3[0]+0.000001 || az <= uvec3[0]-0.000001 || el >= uvec3[1]+0.000001 || el <= uvec3[1]-0.000001) {
		fprintf(stderr,"Whoops - JPLCamera::self_test failed.\n");
		fprintf(stderr,"After conversion to CAHVOR_MODEL...\n");
		fprintf(stderr,"Expected returned az,el from Point3DToAzElRng (%f,%f)\n",
			pos3[0], pos3[1]);
		fprintf(stderr,"to match az,el from Image2DToAzEl (%f,%f)\n", az, el);
		status = FALSE;
	}

	if(uvec3[2] <= 10000.0-0.000001 || uvec3[2] >= 10000.0+0.000001) {
		fprintf(stderr,"Whoops - JPLCamera::self_test failed.\n");
		fprintf(stderr,"After conversion to CAHVOR_MODEL...\n");
		fprintf(stderr,"Expected returned range from Point3DToAzElRng (%f)\n",
			uvec3[2]);
		fprintf(stderr,"to match range input to Image2DToPoint3D (%f)\n", uvec3[2]);
		status = FALSE;
	}

	// make sure we can force model type back to CAHV model
	cam_model->setO(r);
	cam_model->setR(r);
	cam_model->setmodelType(CAHV_MODEL);

	if(cam_model->getmodelType() != CAHV_MODEL) {
		fprintf(stderr,"Whoops - JPLCamera::self_test failed.\n");
		fprintf(stderr,"After conversion back to CAHV_MODEL...\n");
		fprintf(stderr,"Expecting camera model to be CAHV_MODEL = %d but was %d\n",
			CAHV_MODEL, cam_model->getmodelType());
		status = FALSE;
	}

	if(strcmp(cam_model->getCameraModelName(), "CAHV_Model")) {
		fprintf(stderr,"Whoops - JPLCamera::self_test failed.\n");
		fprintf(stderr,"After conversion back to CAHV_MODEL...\n");
		fprintf(stderr,"Expecting camera model name to be CAHV_Model but was %s\n",
			cam_model->getCameraModelName());
		status = FALSE;
	}

	if(cam_model->getCameraX() != -0.143069) {
		fprintf(stderr,"Whoops - JPLCamera::self_test failed.\n");
		fprintf(stderr,"After conversion back to CAHV_MODEL...\n");
		fprintf(stderr,"Expecting camera X to be %f but was %f\n",
			-0.143069, cam_model->getCameraX());
		status = FALSE;
	}

	if(cam_model->getCameraY() != -0.164667) {
		fprintf(stderr,"Whoops - JPLCamera::self_test failed.\n");
		fprintf(stderr,"After conversion back to CAHV_MODEL...\n");
		fprintf(stderr,"Expecting camera Y to be %f but was %f\n",
			-0.164667, cam_model->getCameraY());
		status = FALSE;
	}

	if(cam_model->getCameraZ() != -0.801773) {
		fprintf(stderr,"Whoops - JPLCamera::self_test failed.\n");
		fprintf(stderr,"After conversion back to CAHV_MODEL...\n");
		fprintf(stderr,"Expecting camera Z to be %f but was %f\n",
			-0.801773, cam_model->getCameraZ());
		status = FALSE;
	}

	// check out the projection methods
	xy[0] = 1024.0; xy[1] = 0.0;  // should give (0.123536, -0.984082, 0.127759)
	if(cam_model->Image2DToRay3D(xy, pos3, uvec3) != NO_ERR) {
		fprintf(stderr,"Whoops - JPLCamera::self_test failed.\n");
		fprintf(stderr,"After conversion back to CAHV_MODEL...\n");
		fprintf(stderr,"Returned status from Image2DToRay3D indicates error.\n");
		status = FALSE;
	}

	if(pos3[0] != -0.143069 || pos3[1] != -0.164667 || pos3[2] != -0.801773) {
		fprintf(stderr,"Whoops - JPLCamera::self_test failed.\n");
		fprintf(stderr,"After conversion back to CAHV_MODEL...\n");
		fprintf(stderr,"Expected returned camera position to be (%f,%f,%f)\n",
			0.123536, -0.984082, 0.127759);
		fprintf(stderr,"but got (%f,%f,%f)\n", pos3[0], pos3[1], pos3[2]);
		status = FALSE;
	}

	if(uvec3[0] >= 0.123537 || uvec3[1] >= -0.984081 || uvec3[2] >= 0.127760 ||
	   uvec3[0] <= 0.123535 || uvec3[1] <= -0.984083 || uvec3[2] <= 0.127758) {
		fprintf(stderr,"Whoops - JPLCamera::self_test failed.\n");
		fprintf(stderr,"After conversion back to CAHV_MODEL...\n");
		fprintf(stderr,"Expected returned ray vector to be (%f,%f,%f)\n",
			0.123536, -0.984082, 0.127759);
		fprintf(stderr,"but got (%f,%f,%f)\n", uvec3[0], uvec3[1], uvec3[2]);
		status = FALSE;
	}

	xy[0] = 1024.0; xy[1] = 0.0;  // should give (0.123536, -0.984082, 0.127759)
	if(cam_model->Image2DToRay3D(xy[1], xy[0], uvec3) != NO_ERR) {
		fprintf(stderr,"Whoops - JPLCamera::self_test failed.\n");
		fprintf(stderr,"After conversion back to CAHV_MODEL...\n");
		fprintf(stderr,"Returned status from Image2DToRay3D indicates error.\n");
		status = FALSE;
	}

	if(uvec3[0] >= 0.123537 || uvec3[1] >= -0.984081 || uvec3[2] >= 0.127760 ||
	   uvec3[0] <= 0.123535 || uvec3[1] <= -0.984083 || uvec3[2] <= 0.127758) {
		fprintf(stderr,"Whoops - JPLCamera::self_test failed.\n");
		fprintf(stderr,"After conversion back to CAHV_MODEL...\n");
		fprintf(stderr,"Expected returned ray vector to be (%f,%f,%f)\n",
			0.123536, -0.984082, 0.127759);
		fprintf(stderr,"but got (%f,%f,%f)\n", uvec3[0], uvec3[1], uvec3[2]);
		status = FALSE;
	}

	xy[0] = 1024.0; xy[1] = 0.0;  // should give (az=-1.445916, el=-0.128110)
	if(cam_model->Image2DToAzEl(xy[1], xy[0], &az, &el) != NO_ERR) {
		fprintf(stderr,"Whoops - JPLCamera::self_test failed.\n");
		fprintf(stderr,"After conversion back to CAHV_MODEL...\n");
		fprintf(stderr,"Returned status from Image2DToAzEl indicates error.\n");
		status = FALSE;
	}

	if(az >= -1.445915 || az <= -1.445917 || el >= -0.128109 || el <= -0.128111) {
		fprintf(stderr,"Whoops - JPLCamera::self_test failed.\n");
		fprintf(stderr,"After conversion back to CAHV_MODEL...\n");
		fprintf(stderr,"Expected returned azimuth and elevation to be (%f,%f)\n",
			-1.445916, -0.128110);
		fprintf(stderr,"but got (%f,%f)\n", az, el);
		status = FALSE;
	}

	if(cam_model->Point3DToImage2D(0.0, -100.0, 30.0, xy+1, xy) != NO_ERR) {
		fprintf(stderr,"Whoops - JPLCamera::self_test failed.\n");
		fprintf(stderr,"After conversion back to CAHV_MODEL...\n");
		fprintf(stderr,"Returned status from Point3DToImage2D indicates error.\n");
		status = FALSE;
	}

	if(xy[0] >= 819.599546 || xy[1] >= 218.317583 ||
	   xy[0] <= 819.599544 || xy[1] <= 218.317581) {
		fprintf(stderr,"Whoops - JPLCamera::self_test failed.\n");
		fprintf(stderr,"After conversion back to CAHV_MODEL...\n");
		fprintf(stderr,"Expected returned image xy to be (%f,%f)\n",
			819.599545, 218.317582);
		fprintf(stderr,"but got (%f,%f)\n", xy[0], xy[1]);
		status = FALSE;
	}

	// now project the image point out to az, el and compare to 3D az,el
	if(cam_model->Image2DToAzEl(xy[1], xy[0], &az, &el) != NO_ERR) {
		fprintf(stderr,"Whoops - JPLCamera::self_test failed.\n");
		fprintf(stderr,"After conversion back to CAHV_MODEL...\n");
		fprintf(stderr,"Returned status from Image2DToAzEl indicates error.\n");
		status = FALSE;
	}

	if(cam_model->Point3DToAzElRng(0.0, -100.0, 30.0, pos3, pos3+1, pos3+2) != NO_ERR) {
		fprintf(stderr,"Whoops - JPLCamera::self_test failed.\n");
		fprintf(stderr,"After conversion back to CAHV_MODEL...\n");
		fprintf(stderr,"Returned status from Point3DToAzElRng indicates error.\n");
		status = FALSE;
	}

	if(az >= pos3[0]+0.000001 || az <= pos3[0]-0.000001 || el >= pos3[1]+0.000001 || el <= pos3[1]-0.000001) {

		fprintf(stderr,"Whoops - JPLCamera::self_test failed.\n");
		fprintf(stderr,"After conversion back to CAHV_MODEL...\n");
		fprintf(stderr,"Expected returned az,el from Point3DToAzElRng (%f,%f)\n",
			pos3[0], pos3[1]);
		fprintf(stderr,"to match az,el from Image2DToAzEl (%f,%f)\n", az, el);
		status = FALSE;
	}

	// test projecting image point to 3D space
	// basically, we project an image point 10000 units into 3D space
	// then we backproject the 3D point to az, el, range (the ranges should match)
	// the we reporject the image point to az, el and verify that those match too
	if(cam_model->Image2DToPoint3D(xy[1], xy[0], 10000.0, pos3) != NO_ERR) {
		fprintf(stderr,"Whoops - JPLCamera::self_test failed.\n");
		fprintf(stderr,"After conversion back to CAHV_MODEL...\n");
		fprintf(stderr,"Returned status from Image2DToPoint3D indicates error.\n");
		status = FALSE;
	}

	if(cam_model->Point3DToAzElRng(pos3[0], pos3[1], pos3[2], uvec3, uvec3+1, uvec3+2) != NO_ERR) {
		fprintf(stderr,"Whoops - JPLCamera::self_test failed.\n");
		fprintf(stderr,"After conversion back to CAHV_MODEL...\n");
		fprintf(stderr,"Returned status from Point3DToAzElRng indicates error.\n");
		status = FALSE;
	}

	if(cam_model->Image2DToAzEl(xy[1], xy[0], &az, &el) != NO_ERR) {
		fprintf(stderr,"Whoops - JPLCamera::self_test failed.\n");
		fprintf(stderr,"After conversion back to CAHV_MODEL...\n");
		fprintf(stderr,"Returned status from Image2DToAzEl indicates error.\n");
		status = FALSE;
	}

	if(az >= uvec3[0]+0.000001 || az <= uvec3[0]-0.000001 || el >= uvec3[1]+0.000001 || el <= uvec3[1]-0.000001) {
		fprintf(stderr,"Whoops - JPLCamera::self_test failed.\n");
		fprintf(stderr,"After conversion back to CAHV_MODEL...\n");
		fprintf(stderr,"Expected returned az,el from Point3DToAzElRng (%f,%f)\n",
			pos3[0], pos3[1]);
		fprintf(stderr,"to match az,el from Image2DToAzEl (%f,%f)\n", az, el);
		status = FALSE;
	}

	if(uvec3[2] <= 10000.0-0.000001 || uvec3[2] >= 10000.0+0.000001) {
		fprintf(stderr,"Whoops - JPLCamera::self_test failed.\n");
		fprintf(stderr,"After conversion back to CAHV_MODEL...\n");
		fprintf(stderr,"Expected returned range from Point3DToAzElRng (%f)\n",
			uvec3[2]);
		fprintf(stderr,"to match range input to Image2DToPoint3D (%f)\n", uvec3[2]);
		status = FALSE;
	}

/* I don't know how to test these.
	double hs, hc, vs, vc, theta;
	cam_model->GetInternals(&hs, &hc, &vs, &vc, &theta);
	fprintf(stderr,"Internals are:\nhs=%f\nhc=%f\nvs=%f\nvc=%f\ntheta=%f\n",
		hs, hc, vs, vc, theta);
*/

	return(status);
}
