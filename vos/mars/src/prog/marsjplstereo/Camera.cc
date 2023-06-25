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
#include <math.h>
#include <string.h>

#include "ErrHandle.h"
#include "cahvor.h"
#include "Timing.h"
#include "Camera.h"
#include "real_helpers.h"
#include "nav_memory.h"
#include "stereo.h"	// st_cavhore_t, st_cahv_t
#include "st_warp.h"	// st_warp_bilinear, st_warp_bilinear_init_cahvore

#if defined(MSP)
#define good_fopen fopen
#define ERR(x) (0x6030+(x))
#else
#include "good_fopen.h"
#endif
#include "float_matrix.h"
#include "mat3.h"

/* for MIPL... not sure where it is for real. */
#define is_equal3(a, b, eps) (EQe((a)[0],(b)[0],(eps)) && EQe((a)[1],(b)[1],(eps)) && EQe((a)[2],(b)[2],(eps)))

#include "cmod_fp_stubs.h"

#if defined(RTI_VXWORKS) || defined (__VXWORKS__) || defined (RTS_VXWORKS)
#undef VXWORKS
#define VXWORKS
#endif

#ifdef VXWORKS

#include "taskLib.h"
#endif

#ifndef DBG
#define DBG(x) printf x
#endif

/* HACK - I'd rather use ((dbgvar) ? fprintf (...) : 0) in the definition
   of DBGMSG but GNU complains */

#define DBGMSG(dbgvar,str) { if (dbgvar) fprintf (stderr, "## %s\n", (str)); }
#define LOAD_DEBUG 0

/*#define LINUX_MEM_ERR*/

/*! Constructor for a generic camera model (useless except in derived
  classes)
*/
CameraModel :: CameraModel()
{
	params = NULL;
	mm = NULL;
}



/*!
  Destructor for a generic camera model object.  Deletes memory
  associated with camera model parameters.
*/
CameraModel :: ~CameraModel ()
{
	if (params) {
#ifdef LINUX_MEM_ERR
fprintf (stderr, "=== Deleting %x params %x\n", this, params);
#endif
	  DELETEV (mm,(char *) params);
	  params = NULL;
	}
}



/*!
 Default constructor for JPL CAHVOR camera model; use Init to reset the type.
*/
JPLCamera::JPLCamera ()
{
  Init (CAHVOR_MODEL, NULL);
}


/*!
  Default constructor for JPL CAHVOR camera model; use Init to reset
  the type.
\param mgr JMemoryManager pool from which internal memory
structures will be allocated.
*/

JPLCamera::JPLCamera (JMemoryManager *mgr)
{
  Init (CAHVOR_MODEL, mgr);
}

/*!
  Initialize a new camera model -- allocates enough storage to hold a
  CAHVORE camera model together with its covariance matrices.
  Creates storage for the parameters from the memory pool if none has
  already been allocated.

\param type Camera model type, one of CAHV_MODEL, CAHVOR_MODEL,
CAHVORE_MODEL.
\param mgr JMemoryManager from pool from which internal memory
structures will be allocated.
*/
void JPLCamera::Init (long type, JMemoryManager *mgr)
{
  // Allocate enough storage to support the largest camera model, the
  // CAHVORE type
  num_reals = 21;

  mm = mgr;
  if (params == NULL) {
    // All parameters are stored on the heap in a list of doubles.
    // Leave enough storage for a CAHVORE model, even if less space is
    // needed right now

    double *data = NEW(mm, "Camera model data")
      double[num_reals+7+num_reals*num_reals+5*5];

#ifdef LINUX_MEM_ERR
    fprintf (stderr, "*** %x allocated params %x\n", this, data);
#endif

    if (data == NULL) {
      DBG (("*** Unable to allocate new JPLCamera!\n"));
      return;
    }

    params = (void *) data;
  }

  double *dparams = (double *) params;
  int i;

  if (type != CAHV_MODEL && type != CAHVOR_MODEL && type != CAHVORE_MODEL) {
    DBG (("Warning!!! Only allocated storage for CAHVORE, not type=%ld\n",
	  type));
  }

  isValid = 0;

  C = dparams;
  A = &dparams[3];
  H = &dparams[6];
  V = &dparams[9];
  O = &dparams[12];
  R = &dparams[15];
  E = &dparams[18];

  center = &dparams[num_reals];
  scale = &dparams[num_reals+2];
  theta = &dparams[num_reals+4];
  linearity_parm = &dparams[num_reals+5];
  e_type = &dparams[num_reals+6];

  // Covariance matrix for model parameters
  s = &dparams[num_reals+7];

  // Covariance matrix for 5 derived quantities (2 centers, 2 scales,
  // and theta)

  s_int = &dparams[num_reals+7+num_reals*num_reals];
  for (i = 0; i < num_reals+7 + num_reals*num_reals + 5 * 5; i++)
    C[i] = 0.0;
	
  modelType = type;

  isValid = 0;
  rectification_tables = NULL;
  rectificationTable = NULL;
  rows = cols = -1;
}



/*!
  Destructor for JPLCamera objects.  CameraModel destructor gets
  invoked automatically, and that takes care of the memory allocated
  to hold camera model parameters.
*/

JPLCamera::~JPLCamera ()
{
  if (rectificationTable) {
#ifdef INIT_DEBUG
    fprintf (stderr, "JPLCamera destructor: rectificationTable was %lx\n",
	     (long) rectificationTable);
#endif
    DELETEV(mm, (char *) rectificationTable);
  }
}



/*!
  Copy this camera model into the already-allocated parameter

\param dstCam Destination camera model, which must have already been
allocated.  If this is NULL, nothing gets copied.
*/
void
JPLCamera::CopyCamera (JPLCamera *dstCam)
{
  int i;
	
  if (C == NULL || dstCam == NULL)
    return;

  for (i = 3; i--; ) {
    dstCam->C[i] = C[i];
    dstCam->A[i] = A[i];
    dstCam->H[i] = H[i];
    dstCam->V[i] = V[i];
    dstCam->O[i] = O[i];
    dstCam->R[i] = R[i];
    dstCam->E[i] = E[i];
  }
  for (i = 2; i--;) {
    dstCam->center[i] = center[i];
    dstCam->scale[i] = scale[i];
  }
  dstCam->theta[0] = theta[0];
  dstCam->linearity_parm[0] = linearity_parm[0];
  dstCam->e_type[0] = e_type[0];

  for (i = num_reals * num_reals; i--; )
    dstCam->s[i] = s[i];
  for (i = 5*5; i--; )
    dstCam->s_int[i] = s_int[i];
  
  dstCam->modelType = modelType;
  dstCam->num_reals = num_reals;
  dstCam->isValid = isValid;
  dstCam->rows = rows;
  dstCam->cols = cols;
  dstCam->rectification_tables = rectification_tables;
}



/*!
  Initialize a JPLCamera object from a raw representation in memory.
  The number of bytes tells you whether they're floats or doubles,
  and whether you should expect to load the derived params
  (Hs,Hc,Vs,Vc,theta)  and the covariance matrices.  This uses
  heuristics to determine the breakdown of the memory structure.

\param memstart Starting address of the camera model parameters
\param itype Camera model type, one of CAHV_MODEL, CAHVOR_MODEL,
CAHVORE_MODEL
\param bytes Number of bytes that comprise the camera model
parameters.
\return NO_ERR if the load succeeded, PARAM_ERR if memstart is NULL or
itype is invalid, INIT_ERR if load failed
*/
long JPLCamera::LoadFromMemory (char *memstart, int itype, int bytes)
{
  AnythingT aptr;
  int use_doubles = 0;
  int load_cahv = 0;
  int load_or = 0;
  int load_e = 0;
  int load_derived = 0;
  int load_e_params = 0;
  int load_rows_cols = 0;
  int load_covar = 0;
  long retval = NO_ERR;

  DBGMSG(LOAD_DEBUG, "Starting LoadFromMemory");

  if (memstart == NULL)
    return PARAM_ERR;

  aptr.c = memstart;

#define SET_LOAD(doubles,cahv,o_r,e,derived,e_params,rows_cols,covar) \
  (use_doubles = (doubles), load_cahv = (cahv), load_or = (o_r), \
   load_e = (e), load_derived = (derived), load_e_params = (e_params), \
   load_rows_cols = (rows_cols), load_covar = (covar))

  switch (itype) {
  case CAHV_MODEL:
    if (bytes == 12 * sizeof(float))
      SET_LOAD(0, 1, 0, 0, 0, 0, 0, 0);
    else if (bytes == 12 * sizeof(double))
      SET_LOAD(1, 1, 0, 0, 0, 0, 0, 0);
    else if (bytes == 17 * sizeof(float))
      SET_LOAD(0, 1, 0, 0, 1, 0, 0, 0);
    else if (bytes == 17 * sizeof(double))
      SET_LOAD(1, 1, 0, 0, 1, 0, 0, 0);
    else if (bytes == 19 * sizeof(float))
      SET_LOAD(0, 1, 0, 0, 1, 1, 0, 0);
    else if (bytes == 19 * sizeof(double))
      SET_LOAD(1, 1, 0, 0, 1, 1, 0, 0);
    else if (bytes == 12 * sizeof(float) + 2 * sizeof(long))
      SET_LOAD(0, 1, 0, 0, 0, 0, 1, 0);
    else if (bytes == 12 * sizeof(double) + 2 * sizeof(long))
      SET_LOAD(1, 1, 0, 0, 0, 0, 1, 0);
    // Alias with loading e parameters
    //    else if (bytes == 17 * sizeof(float) + 2 * sizeof(long))
    //      SET_LOAD(0, 1, 0, 0, 1, 0, 1, 0);
    else if (bytes == 17 * sizeof(double) + 2 * sizeof(long))
      SET_LOAD(1, 1, 0, 0, 1, 0, 1, 0);
    else if (bytes == 19 * sizeof(float) + 2 * sizeof(long))
      SET_LOAD(0, 1, 0, 0, 1, 1, 1, 0);
    else if (bytes == 19 * sizeof(double) + 2 * sizeof(long))
      SET_LOAD(1, 1, 0, 0, 1, 1, 1, 0);
    //    break; -- fall through
 case CAHVOR_MODEL:
    if (bytes == 18 * sizeof(float))
      SET_LOAD(0, 1, 1, 0, 0, 0, 0, 0);
    else if (bytes == 18 * sizeof(double))
      SET_LOAD(1, 1, 1, 0, 0, 0, 0, 0);
    else if (bytes == 23 * sizeof(float))
      SET_LOAD(0, 1, 1, 0, 1, 0, 0, 0);
    else if (bytes == 23 * sizeof(double))
      SET_LOAD(1, 1, 1, 0, 1, 0, 0, 0);
    else if (bytes == 25 * sizeof(float))
      SET_LOAD(0, 1, 1, 0, 1, 1, 0, 0);
    else if (bytes == 25 * sizeof(double))
      SET_LOAD(1, 1, 1, 0, 1, 1, 0, 0);
    else if (bytes == 18 * sizeof(float) + 2 * sizeof(long))
      SET_LOAD(0, 1, 1, 0, 0, 0, 1, 0);
    else if (bytes == 18 * sizeof(double) + 2 * sizeof(long))
      SET_LOAD(1, 1, 1, 0, 0, 0, 1, 0);
    // alias with loading E parameters
    //    else if (bytes == 23 * sizeof(float) + 2 * sizeof(long))
    //      SET_LOAD(0, 1, 1, 0, 1, 0, 1, 0);
    else if (bytes == 23 * sizeof(double) + 2 * sizeof(long))
      SET_LOAD(1, 1, 1, 0, 1, 0, 1, 0);
    else if (bytes == 25 * sizeof(float) + 2 * sizeof(long))
      SET_LOAD(0, 1, 1, 0, 1, 1, 1, 0);
    else if (bytes == 25 * sizeof(double) + 2 * sizeof(long))
      SET_LOAD(1, 1, 1, 0, 1, 1, 1, 0);
    //    break; -- fall through
 case CAHVORE_MODEL:
    if (bytes == 21 * sizeof(float))
      SET_LOAD(0, 1, 1, 1, 0, 0, 0, 0);
    else if (bytes == 21 * sizeof(double))
      SET_LOAD(1, 1, 1, 1, 0, 0, 0, 0);
    else if (bytes == 26 * sizeof(float))
      SET_LOAD(0, 1, 1, 1, 1, 0, 0, 0);
    else if (bytes == 26 * sizeof(double))
      SET_LOAD(1, 1, 1, 1, 1, 0, 0, 0);
    else if (bytes == 28 * sizeof(float))
      SET_LOAD(0, 1, 1, 1, 1, 1, 0, 0);
    else if (bytes == 28 * sizeof(double))
      SET_LOAD(1, 1, 1, 1, 1, 1, 0, 0);
    else if (bytes == 21 * sizeof(float) + 2 * sizeof(long))
      SET_LOAD(0, 1, 1, 1, 0, 0, 1, 0);
    else if (bytes == 21 * sizeof(double) + 2 * sizeof(long))
      SET_LOAD(1, 1, 1, 1, 0, 0, 1, 0);
    // alias with loading E parameters :-(
    //    else if (bytes == 26 * sizeof(float) + 2 * sizeof(long))
    //      SET_LOAD(0, 1, 1, 1, 1, 0, 1, 0);
    else if (bytes == 26 * sizeof(double) + 2 * sizeof(long))
      SET_LOAD(1, 1, 1, 1, 1, 0, 1, 0);
    else if (bytes == 28 * sizeof(float) + 2 * sizeof(long))
      SET_LOAD(0, 1, 1, 1, 1, 1, 1, 0);
    else if (bytes == 28 * sizeof(double) + 2 * sizeof(long))
      SET_LOAD(1, 1, 1, 1, 1, 1, 1, 0);
    break;
  default:
    retval = PARAM_ERR;
    break;
  } // switch

#if LOAD_DEBUG == 1
  fprintf (stderr,
	   "## Doubles %d, cahv %d, or %d, e %d, derived %d, e-params %d, "
	   "covar %d\n",
	   use_doubles, load_cahv, load_or, load_e, load_derived, 
	   load_e_params, load_covar);
#endif

  if (load_cahv == 0)
    retval = INIT_ERR;

  if (retval == NO_ERR) {
    if (use_doubles) {
      double *dptr = aptr.d;
      double *cahvptr = aptr.d;
      double *optr = NULL, *rptr = NULL, *eptr = NULL;
      double *linptr = NULL, *etypeptr = NULL;
      double *centerptr = NULL, *scaleptr = NULL;
      double *thetaptr = NULL, *scovarptr = NULL, *covarptr = NULL;
      long *rowptr = NULL, *colptr = NULL;

      // Skip over CAHV triples
      dptr += 12;

      // Should we load O and R triples?
      if (load_or) {
	optr = dptr;
	rptr = dptr + 3;
	dptr += 6;
      }

      // Should we load the E triple?
      if (load_e) {
	eptr = dptr;
	dptr += 3;
      }

      // Should we load the derived values?
      if (load_derived) {
	centerptr = dptr;
	scaleptr = dptr + 2;
	thetaptr = dptr + 4;
	dptr += 5;
      }

      // Should we load the E-related linearity parameters?
      if (load_e_params) {
	linptr = dptr;
	etypeptr = dptr + 1;
	dptr += 2;
      }

      // Should we load the row/column count?
      if (load_rows_cols) {
	union { double *d; long *l; } u;
	u.d = dptr;
	rowptr = u.l;
	colptr = rowptr + 1;
	dptr += 1;	// HACK two longs == one double, right?
      }

      // Should we load the covariances?
      if (load_covar) {
	covarptr = dptr;
	scovarptr = dptr + 18*18;
	dptr += 18*18 + 5*5;
      }

      InitJPLCamera (itype, rowptr, colptr,
		     cahvptr, cahvptr+3, cahvptr+6, cahvptr+9,
		     optr, rptr, eptr, centerptr, scaleptr, thetaptr, linptr,
		     etypeptr, covarptr, scovarptr);
    } else {
      double nC[3], nA[3], nH[3], nV[3], nO[3], nR[3], nE[3];
      double ncenter[2], nscale[2], ntheta, nlinparm, netype;
      float *fptr = aptr.f;
      double *optr = NULL, *rptr = NULL;
      double *centerptr = NULL, *scaleptr = NULL;
      double *linptr = NULL, *etypeptr = NULL;
      double *thetaptr = NULL, *scovarptr = NULL, *covarptr = NULL;
      long *rowptr = NULL, *colptr = NULL;

#define VEC3_INIT(V,addr) (V[0] = (double) *(addr), V[1] = (double) *(addr+1),\
			   V[2] = (double) *(addr+2))
#define VEC2_INIT(V,addr) (V[0] = (double) *(addr), V[1] = (double) *(addr+1))

      DBGMSG(LOAD_DEBUG, "Loading FLOAT C param");
      VEC3_INIT(nC,fptr);
      DBGMSG(LOAD_DEBUG, "Loading FLOAT A param");
      VEC3_INIT(nA,fptr+3);
      DBGMSG(LOAD_DEBUG, "Loading FLOAT H param");
      VEC3_INIT(nH,fptr+6);
      DBGMSG(LOAD_DEBUG, "Loading FLOAT V param");
      VEC3_INIT(nV,fptr+9);

      // Skip over CAHV triples
      fptr += 12;

      // Should we load O and R triples?
      if (load_or) {
	DBGMSG(LOAD_DEBUG, "Loading FLOAT OR params");
	VEC3_INIT(nO,fptr);
	VEC3_INIT(nR,fptr+3);
	optr = nO;
	rptr = nR;
	fptr += 6;
      }

      // Should we load the E triple?
      if (load_e) {
	DBGMSG(LOAD_DEBUG, "Loading FLOAT E param");
	VEC3_INIT(nE, fptr);
	fptr += 3;
      }

      // Should we load the derived values?
      if (load_derived) {
	DBGMSG(LOAD_DEBUG, "Loading FLOAT DERIVED PARAMS param");
	VEC2_INIT(ncenter,fptr);
	VEC2_INIT(nscale,fptr+2);
	centerptr = ncenter;
	scaleptr = nscale;
	ntheta = (double) *(fptr+4);
	thetaptr = &ntheta;
	fptr += 5;
      }

      // Should we load the E linearity params?
      if (load_e_params) {
	DBGMSG(LOAD_DEBUG, "Loading FLOAT E LINEARITY PARAMS");
	nlinparm = (double) *fptr;
	netype = (double) fptr[1];
	linptr = &nlinparm;
	etypeptr = &netype;
	fptr += 2;
      }

      // Should we load the row/column count?
      if (load_rows_cols) {
	DBGMSG(LOAD_DEBUG, "Loading LONG ROW/COL PARAMS");
	union { float *f; long *l; } u;
	u.f = fptr;
	rowptr = u.l;
	colptr = rowptr + 1;
	fptr += 2;
      }

      // Should we load the covariances?
      if (load_covar) {
	// HACK -- needs to be read and copied into (double) memory
	DBG(("WARNING!!! Skipping covariances"));
      }
      DBGMSG(LOAD_DEBUG, "Calling InitJPLCamera");
      InitJPLCamera (itype, rowptr, colptr, (double *) nC, (double *) nA,
		     (double *) nH, (double *) nV,
		     optr, rptr, (double *) nE, centerptr, scaleptr, thetaptr,
		     linptr, etypeptr, covarptr, scovarptr);
    }
  } // if retval == NO_ERR

  return retval;
}


/*!
  Initialize this JPLCamera object by copying values from memory.
  If the addresses given for  derived quantities (center, scale,
  theta) are NULL, they will be computed from the model
  parameters. 

  Any of the pointer parameters may be NULL, in which case the current
  values for that parameter will remain unchanged.

\brief Initialize camera model parameters
\param type Camera model type, one of CAHV_MODEL, CAHVOR_MODEL, CAHVORE_MODEL
\param inRows Address of number of rows in the image
\param inCols Address of number of columns in the image
\param inC Address of input C 3-vector
\param inA Address of input A 3-vector
\param inH Address of input H 3-vector
\param inV Address of input V 3-vector
\param inO Address of input O 3-vector
\param inR Address of input R 3-vector
\param inE Address of input E 3-vector
\param incenter Address of input image center 2-vector
\param inscale Address of input image scale 2-vector
\param intheta Address of input angle between focal plane axes
\param inLin Address of input linearity parameter
\param inEtype Address of input E type
\param ins Address of input covariance matrix
\param ins_int Address of input derived values covariance matrix
*/

void
JPLCamera::InitJPLCamera (long type, long *inRows, long *inCols,
			  double *inC, double *inA,
			  double *inH, double *inV, double *inO, double *inR,
			  double *inE,
			  double *incenter, double *inscale, double *intheta,
			  double *inLin, double *inEtype,
			  double *ins, double *ins_int)
{
  int i;
	
  modelType = type;

  for (i = 3; i--; ) {
    if (inC) C[i] = inC[i];
    if (inA) A[i] = inA[i];
    if (inH) H[i] = inH[i];
    if (inV) V[i] = inV[i];
    if (inO) O[i] = inO[i];
    if (inR) R[i] = inR[i];
    if (inE) E[i] = inE[i];
  }
	
  // This Initialization code (the dot/cross products) was copied from
  // cmod_cahv_internal()

  if (incenter)
    for (i = 2; i-- ; )
      center[i] = incenter[i];
  else if (inA && inH && inV) {
    center[0] = dot3(A, H);	// Hc
    center[1] = dot3(A, V);	// Vc
  }

  if (inscale)
    for (i = 2; i-- ; )
      scale[i] = inscale[i];
  else if (inA && inH && inV) {
    double cross[3];
    
    scale[0] = mag3(cross3(A, H, cross)); // Hs
    scale[1] = mag3(cross3(A, V, cross)); // Vs
  }
  
  if (intheta)
    *theta = *intheta;
  else if (inA && inH && inV) {
    double cross[3], cross1[3], cross2[3];

    *theta = atan2(dot3 (cross3 (V, H, cross), A),
		   dot3 (cross3 (A, V, cross1),
			 cross3 (A, H, cross2)));

  }
  if (inLin)
    *linearity_parm = *inLin;
  else
    *linearity_parm = 0.0;	/* only used if e_type is 3:TYPE_GENERAL */

  if (inEtype)
    *e_type = *inEtype;
  else
    *e_type = 1;			/* should be TYPE_PERSPECTIVE */

  if (inRows)
    rows = *inRows;
  if (inCols)
    cols = *inCols;

  if (ins)
    for (i = num_reals * num_reals; i--; ) s[i] = ins[i];

  if (ins_int)
    for (i = 5 * 5; i--; ) s_int[i] = ins_int[i];

  isValid = 1;
}



/*!
  Read a text-format camera model from a file.  Parse the input file looking
  for "Model =" text to determine the model type.

\brief Read a text-format camera model from a file
\param filename Input file
\return NO_ERR if successful, PARAM_ERR if filename is NULL, FILE_ERR
if the file could not be read

\sa WriteCameraModel
*/

long JPLCamera::ReadCameraModel (char *filename)
{
  FILE *fp;
  long type = CAHVOR_MODEL;

  if (filename && (fp = fopen (filename, "r")) != NULL) {
    char buf[1000], word[1000];

    while (fgets (buf, 1000, fp) != NULL) {
      if (sscanf (buf, "Model = %1000s", word) > 0) {
	if (strncmp (word, "CAHVORE", 7) == 0)
	  type = CAHVORE_MODEL;
	else if (strncmp (word, "CAHVOR", 6) == 0)
	  type = CAHVOR_MODEL;
	else if (strncmp (word, "CAHV", 4) == 0)
	  type = CAHV_MODEL;
	else
	  continue;
	break;
      }
    }
    fclose (fp);
  }

  return ReadCameraModel (filename, type);
}


/*!
  Read a text-format camera model from a file.  If the
  camera model does not include the image rows and columns, the
  current values will be left unchanged.

\brief Read a text-format camera model from a file
\param filename Input file
\param type Camera model type, one of CAHV_MODEL, CAHVOR_MODEL, CAHVORE_MODEL
\return NO_ERR if successful, PARAM_ERR if filename is NULL or type is
invalid, FILE_ERR if the file could not be read

\sa WriteCameraModel
*/

long JPLCamera::ReadCameraModel (char *filename, long type)
{
  int irows = (int) rows;
  int icols = (int) cols;

  if (filename == NULL) {
    FatalErr ("ReadCameraModel:  empty filename\n");
    return PARAM_ERR;
  }

  modelType = type;

#ifdef INIT_DEBUG
  fprintf (stderr, "ReadCameraModel: expecting type %ld\n", type);
#endif

  if (modelType == CAHV_MODEL) {
    if (cmod_cahv_read2 (filename, &icols, &irows,
			     C, A, H, V, (double (*)[12])s,
			     &scale[0], &center[0], &scale[1], &center[1],
			     theta, (double (*)[5]) s_int) == FAILURE) {
      return FILE_ERR;
    }
    O[0] = A[0]; O[1] = A[1]; O[2] = A[2];
    R[0] = R[1] = R[2] = 0;
  } else if (modelType == CAHVOR_MODEL) {
#ifdef INIT_DEBUG
    fprintf (stderr, "ReadCameraModel: calling cmod_cahvor_read2\n");
#endif
    
    if (cmod_cahvor_read2(filename, &icols, &irows, C, A, H, V, O, R,
				(double (*)[18]) s, &scale[0], &center[0], 
				&scale[1], &center[1], theta, (double (*)[5]) s_int)
	== FAILURE)
      return FILE_ERR;
#ifdef INIT_DEBUG
    fprintf (stderr, "ReadCameraModel: back from cmod_cahvor_read2\n");
#endif
  } else if (modelType == CAHVORE_MODEL) {
    int mtype;

    if (cmod_cahvore_read (filename, &icols, &irows,
			       &mtype, linearity_parm,
			       C, A, H, V, O, R, E,
			       (double (*)[21]) s, &scale[0], &center[0],
			       &scale[1], &center[1], theta,
			       (double (*)[5]) s_int) == FAILURE)
      return FILE_ERR;
    *e_type = mtype;
  } else {
    FatalErr ("Wrong camera model type\n");
    return PARAM_ERR;
  } 
	
  isValid = 1;
  if (irows > 0)
    rows = irows;
  if (icols > 0)
    cols = icols;
  return NO_ERR;
}


/*!
  Read a text-format camera model from a stream.  If the
  camera model does not include the image rows and columns, the
  current values will be left unchanged.  This functionality is only
  available on unix, since the standard vxworks build does not include
  the low-level routines that read from fils streams.

\brief Read a text-format camera model from a file
\param filename Input file
\param type Camera model type, one of CAHV_MODEL, CAHVOR_MODEL, CAHVORE_MODEL
\return NO_ERR if successful, PARAM_ERR if fp is NULL or type is
invalid, FILE_ERR if the file could not be read

\sa WriteCameraModel
*/
long	
JPLCamera::ReadCameraModel (FILE *fp, long type)
{
#ifdef __unix

  int irows = (int) rows;
  int icols = (int) cols;

  modelType = type;

  if (fp == NULL)
    return PARAM_ERR;

#ifdef INIT_DEBUG
  fprintf (stderr, "ReadCameraModel: expecting type %ld\n", type);
#endif

  if (modelType == CAHV_MODEL) {
    if (cmod_cahv_read2_fp (fp, &icols, &irows,
				C, A, H, V, (double (*)[12])s,
				&scale[0], &center[0], &scale[1], &center[1],
				theta, (double (*)[5]) s_int) == FAILURE) {
      return FILE_ERR;
    }
    O[0] = A[0]; O[1] = A[1]; O[2] = A[2];
    R[0] = R[1] = R[2] = 0;
  } else if (modelType == CAHVOR_MODEL) {
#ifdef INIT_DEBUG
    fprintf (stderr, "ReadCameraModel: calling cmod_cahvor_read\n");
#endif
    
    if (cmod_cahvor_read2_fp(fp, &icols, &irows, C, A, H, V, O, R,
				(double (*)[18]) s, &scale[0], &center[0], 
				&scale[1], &center[1], theta, (double (*)[5]) s_int)
	== FAILURE)
      return FILE_ERR;
#ifdef INIT_DEBUG
    fprintf (stderr, "ReadCameraModel: back from cmod_cahvor_read\n");
#endif
  } else if (modelType == CAHVORE_MODEL) {
    int mtype = 0;

    if (cmod_cahvore_read_fp (fp, &icols, &irows, &mtype, linearity_parm,
				  C, A, H, V, O, R, E,
				  (double (*)[21]) s, &scale[0], &center[0],
				  &scale[1], &center[1], theta,
				  (double (*)[5]) s_int) == FAILURE)
      return FILE_ERR;
    *e_type = mtype;
  } else {
    FatalErr ("Wrong camera model type\n");
    return PARAM_ERR;
  } 
	
  isValid = 1;
  if (irows > 0)
    rows = irows;
  if (icols > 0)
    cols = icols;
  return NO_ERR;
#else /* __unix */
  return FILE_ERR;
#endif
}
	
	
/*!
  Write camera model parameters to a file, in a format that can be
  easily parsed by the Read routines.

\brief Write camera model parameters to a file
\param filename Output file
\return NO_ERR if successful, PARAM_ERR if filename is NULL or the
modelType field is invalid, FILE_ERR if the fie could not be written.

\sa ReadCameraModel
*/

long JPLCamera::WriteCameraModel (char *filename)
{
  if (filename == NULL) {
    FatalErr ("WriteCameraModel: NULL filename!\n");
    return PARAM_ERR;
  }

  if (modelType == CAHV_MODEL) {
    if (rows > 0 && cols > 0) {
      if (cmod_cahv_write2 (filename, "Writing CAHV model into a file..",
				   cols, rows,
				   C, A, H, V, (double (*)[12])s,
				   scale[0], center[0], scale[1], center[1],
				   *theta, (double (*)[5]) s_int) == FAILURE) {
	return FILE_ERR;
      }
    } else {
      if (cmod_cahv_write (filename, "Writing CAHV model into a file..",
			       C, A, H, V, (double (*)[12])s,
			       scale[0], center[0], scale[1], center[1],
			       *theta, (double (*)[5]) s_int) == FAILURE) {
	return FILE_ERR;
      }
    }
  } else if (modelType == CAHVOR_MODEL) {
    if (rows > 0 && cols > 0) {
      if (cmod_cahvor_write2 (filename,
				  "Writing CAHVOR model into a file..",
				  cols, rows, 
				  C, A, H, V, O, R,
				  (double (*)[18]) s, scale[0], center[0], 
				  scale[1], center[1],
				  *theta, (double (*)[5]) s_int)
	  == FAILURE)
	return FILE_ERR;
    } else {
      if (cmod_cahvor_write (filename,
				 "Writing CAHVOR model into a file..",
				 C, A, H, V, O, R,
				 (double (*)[18]) s, scale[0], center[0], 
				 scale[1], center[1],
				 *theta, (double (*)[5]) s_int)
	  == FAILURE)
	return FILE_ERR;
    }
  } else if (modelType == CAHVORE_MODEL) {
    if (cmod_cahvore_write (filename,
				"Writing CAHVORE model into a file..",
				cols, rows, (int) *e_type, *linearity_parm,
				C, A, H, V, O, R, E, (double (*)[21]) s,
				scale[0], center[0], scale[1], center[1],
				*theta, (double (*)[5]) s_int) == FAILURE)
      return FILE_ERR;
  } else {
    FatalErr ("Wrong camera model type\n");
    return PARAM_ERR;
  }

  return NO_ERR;
}



/*!
  Write camera model parameters to a file, in a format that can be
  easily parsed by the Read routines.  This functionality is only
  available on unix, since the standard vxworks build does not include
  the low-level routines that read from fils streams.


\brief Write camera model parameters to a file
\param fp Output stream
\return NO_ERR if successful, PARAM_ERR if fp is NULL or the
modelType field is invalid, FILE_ERR if the fie could not be written.

\sa ReadCameraModel
*/

long		
JPLCamera::WriteCameraModel (FILE *fp)
{
#ifdef __unix
  if (fp == NULL)
    return PARAM_ERR;

  if (modelType == CAHV_MODEL) {
    if (rows > 0 && cols > 0) {
      if (cmod_cahv_write2_fp (fp, "Writing CAHV model into a file..",
				   cols, rows,
				   C, A, H, V, (double (*)[12])s,
				   scale[0], center[0], scale[1], center[1],
				   *theta, (double (*)[5]) s_int) == FAILURE) {
	return FILE_ERR;
      }
    } else {
      if (cmod_cahv_write_fp (fp, "Writing CAHV model into a file..",
				  C, A, H, V, (double (*)[12])s,
				  scale[0], center[0], scale[1], center[1],
				  *theta, (double (*)[5]) s_int) == FAILURE) {
	return FILE_ERR;
      }
    }
  } else if (modelType == CAHVOR_MODEL) {
    if (rows > 0 && cols > 0) {
      if (cmod_cahvor_write2_fp (fp, "Writing CAHVOR model into a file..",
				     cols, rows, 
				     C, A, H, V, O, R,
				     (double (*)[18]) s, scale[0], center[0], 
				     scale[1], center[1],
				     *theta, (double (*)[5]) s_int)
	  == FAILURE)
	return FILE_ERR;
    } else {
      if (cmod_cahvor_write_fp (fp, "Writing CAHVOR model into a file..",
				    C, A, H, V, O, R,
				    (double (*)[18]) s, scale[0], center[0], 
				    scale[1], center[1],
				    *theta, (double (*)[5]) s_int)
	  == FAILURE)
	return FILE_ERR;
    }
  } else if (modelType == CAHVORE_MODEL) {
    if (cmod_cahvore_write_fp (fp, "Writing CAHVORE model into a file..",
				   rows, cols, (int) *e_type, *linearity_parm,
				   C, A, H, V, O, R, E, (double (*)[21]) s,
				   scale[0], center[0], scale[1], center[1],
				   *theta, (double (*)[5]) s_int) == FAILURE)
      return FILE_ERR;
  } else {
    FatalErr ("Wrong camera model type\n");
    return PARAM_ERR;
  }
  return NO_ERR;
#else /* __unix */
  return FILE_ERR;
#endif
}
	
/*!
  Print the camera model parameters in human readable format.  Only
  the parameters that define the camera model will be displayed, not
  any derived properties.

\brief Print a human-readable camera model parameter
\param fp Output stream
\param prefix Prefix string to use on each line of the output, e.g. to
enable indentation. 
\sa PrintCameraModelDetails PrintCameraModelSummary PrintExtrinsics
*/


// Write messages displaying the contents of the principal vectors
// of the camera model
void		
JPLCamera::PrintCameraModel (FILE *fp, char *prefix)
{
  if (prefix == NULL)
    prefix = "";

  fprintf (fp, "%sC = [ %f %f %f ]\t# Focus point (X,Y,Z)\n",
	   prefix,C[0], C[1], C[2]);
  fprintf (fp, "%sA = [ %f %f %f ]\t# Optical Axis in 3D\n",
	   prefix,A[0], A[1], A[2]);
  fprintf (fp, "%sH = [ %f %f %f ]\n", prefix,H[0], H[1], H[2]);
  fprintf (fp, "%sV = [ %f %f %f ]\n", prefix,V[0], V[1], V[2]);
  if (modelType != CAHV_MODEL) {
    fprintf (fp, "%sO = [ %f %f %f ]\t# Radial distortion 3D axis\n",
	     prefix,O[0], O[1], O[2]);
    fprintf (fp, "%sR = [ %f %f %f ]\t# Radial distortion terms\n",
	     prefix,R[0], R[1], R[2]);
  }
  if (modelType == CAHVORE_MODEL) {
    fprintf (fp, "%sE = [ %f %f %f ]\t# Fisheye terms\n",
	     prefix,E[0], E[1], E[2]);
    fprintf (fp, "%sEtype = %d, Linparm=%g\n", prefix,
	     (int) *e_type, *linearity_parm);
  }
  if (rows > 0 && cols > 0)
    fprintf (fp, "%sRows %ld Cols %ld\n", prefix, rows, cols);
}


/*!
  Print a one-line summary of whether the camera model seems
  "reasonable".

\brief Print a one-line evaluation of the camera model.
\param fp Output stream
\param prefix Prefix string to use on each line of the output, e.g. to
enable indentation.

\sa PrintCameraModel PrintCameraModelDetails PrintExtrinsics
*/


void		
JPLCamera::PrintCameraModelSummary (FILE *fp, char *prefix)
{
  if (prefix == NULL)
    prefix = "";

  if (EQ (A[0], 0.0) && EQ (A[1], 0.0) && EQ (A[2], 0.0))
    fprintf (fp, "%sA vector is ZERO!!!\n", prefix);
  else if (modelType == CAHVORE_MODEL &&
	   (*e_type < 1 || *e_type > 3 || LT(*linearity_parm, 0.0) ||
	    GT(*linearity_parm, 1.0)))
    fprintf (fp, "%sE parameters out of range!\n", prefix);
  else
    fprintf (fp, "%sSeems Ok\n", prefix);
}

/*!
  Print detailed information derived from a pair of camera models in
  human readable format.  Derived quantities include baseline
  separation, number of rows and columns

\brief Print a human-readable list of quantities derived from a stereo
pair of camera models
\param fp Output stream
\param rightcam Right camera model
\param prefix Prefix string to use on each line of the output, e.g. to
enable indentation.

\sa PrintCameraModel PrintCameraModelSummary PrintExtrinsics
*/


void JPLCamera::PrintStereoDetails (FILE *fp, JPLCamera *rightcam,
				    char *prefix)
{
  float myrows = (float) rows , mycols = (float) cols;

  if (fp == NULL)
    return;
  if (prefix == NULL)
    prefix = "";
  if (rightcam == NULL) {
    fprintf (fp, "*** Unable to display %s stereo info, rightcam is NULL\n",
	     prefix);
    return;
  }
	     
  if (myrows < 1)
    myrows = (center[0] + rightcam->center[0]);
  if (mycols < 1)
    mycols = (center[1] + rightcam->center[1]);

  float baseline = sqrt (SQ(C[0]-rightcam->C[0]) +
			 SQ(C[1]-rightcam->C[1]) +
			 SQ(C[2]-rightcam->C[2]));

  float lhfov, lvfov, rhfov, rvfov;

  lhfov = AngleBetweenPoints (myrows / 2, 0, myrows / 2, mycols);
  lvfov = AngleBetweenPoints (0, mycols / 2, myrows, mycols / 2);

  myrows = rightcam->rows;
  mycols = rightcam->cols;

  rhfov = AngleBetweenPoints (myrows / 2, 0, myrows / 2, mycols);
  rvfov = AngleBetweenPoints (0, mycols / 2, myrows, mycols / 2);

  fprintf (fp, "%sBaseline:  %g meters\n", prefix, baseline);
  fprintf (fp, "%s   ASSUMING %g ROWS and %g COLUMNS:\n",
	   prefix, myrows, mycols);
  if (!EQ(lhfov, rhfov) || !EQ(lvfov, rvfov)) {
    fprintf (fp, "%s   Left Rectified FOV:\t\tHoriz %g degrees, Vert %g degrees\n",
	     prefix, lhfov * 180.0 / M_PI, lvfov * 180.0 / M_PI);
    fprintf (fp, "%s  Right Rectified FOV:\t\tHoriz %g degrees, Vert %g degrees\n",
	     prefix, rhfov * 180.0 / M_PI, rvfov * 180.0 / M_PI);
  } else
    fprintf (fp, "%s  Rectified FOV:\t\tHoriz %g degrees, Vert %g degrees\n",
	     prefix, rhfov * 180.0 / M_PI, rvfov * 180.0 / M_PI);
    
}


/*!
  Generate a string (stored temporarily in static storage) that
  describes the direction only (of magnitude) of the input vector.
  The nearest axis is found, and the name of that axis and the number
  of degrees of deviation from it will be written into the string.

\param in Input vector, treated as a ray in space.  Only the direction
of this vector is considered.
\return A string (in temporary static storage) that describes the
direction of the input vector.
*/

static char *axis_comment (double in[3])
{
  const size_t bufLen = 150;
  static char buf[bufLen];
  double unit[3], mag, axis[3];
  double tilt[6], cosine;
  char *tilt_names[] = {"X", "-X", "Y", "-Y", "Z", "-Z"};
  int i, mini = -1;

  mag = mag3 (in);
  if (EQ(mag, 0.0)) {
    snprintf (buf, bufLen, "a ZERO vector");
  } else {
    unit[0] = in[0] / mag;
    unit[1] = in[1] / mag;
    unit[2] = in[2] / mag;

    // Compute X axis angles
    axis[0] = 1.0;
    axis[1] = 0.0;
    axis[2] = 0.0;

    cosine = dot3 (unit, axis);
    tilt[0] = acos(cosine);
    tilt[1] = M_PI - tilt[0];
    
    // Compute Y axis angles
    axis[0] = 0.0;
    axis[1] = 1.0;
    axis[2] = 0.0;

    cosine = dot3 (unit, axis);
    tilt[2] = acos(cosine);
    tilt[3] = M_PI - tilt[2];

    // Compute Z axis angles
    axis[0] = 0.0;
    axis[1] = 0.0;
    axis[2] = 1.0;

    cosine = dot3 (unit, axis);
    tilt[4] = acos(cosine);
    tilt[5] = M_PI - tilt[4];

    // Find the axis whose angular distance from the vector is smallest
    for (i = 0; i < 6; i++)
      if (mini < 0 || LT(ABS(tilt[i]), ABS(tilt[mini])))
	mini = i;

    snprintf (buf, bufLen, "%g degrees from %s axis", tilt[mini] * 180.0 / M_PI,
	     tilt_names[mini]);
  }

  return buf;
}



/*!
  Print the external (nee extrinsic) parameters of the camera model in
  human readable format.  External parameters include the camera
  model's location in 3D world coordinates, and its pointing
  directions (forward, up, and right).  Each pointing direction will
  also have a comment describing the axis closest to it.

\brief Print a human-readable list of camera model external parameters
\param fp Output stream
\param prefix Prefix string to use on each line of the output, e.g. to
enable indentation.
\sa PrintCameraModel PrintCameraModelSummary PrintCameraModelDetails
*/

void
JPLCamera::PrintExtrinsics (FILE *fp, char *prefix)
{
  double fwd[3], up[3], rt[3];

  if (fp == NULL)
    return;

  if (prefix == NULL)
    prefix = "";

  GetPointingDirections (fwd, up, rt);

  if (C)
    fprintf (fp, "%s  Camera Location: %g %g %g\n",
	     prefix, C[0], C[1], C[2]);

  fprintf (fp, "%s  FWD  : %g %g %g\t(%s)\n",
	   prefix, fwd[0], fwd[1], fwd[2], axis_comment (fwd));
  fprintf (fp, "%s  RIGHT: %g %g %g\t(%s)\n",
	   prefix,  rt[0],  rt[1],  rt[2], axis_comment (rt));
  fprintf (fp, "%s  UP   : %g %g %g\t(%s)\n",
	   prefix,  up[0],  up[1],  up[2], axis_comment (up));
}


void JPLCamera::PrintCameraModelDetails (void)
{
  PrintCameraModelDetails (stdout, "");
}


/*!
  Print detailed information derived from the camera model in
  human readable format.  Internal parameters include number of  rows
  and columns, image center, focal lengths, and fields of view.
  External parameters (defined in PrintExternals) include camera model
  location in 3D world coordiantes, and its pointing
  directions (forward, up, and right).

\brief Print a human-readable list of quantities derived from camera
model internal and external parameters
\param fp Output stream
\param prefix Prefix string to use on each line of the output, e.g. to
enable indentation.

\sa PrintCameraModel PrintCameraModelSummary PrintExtrinsics
*/

void
JPLCamera::PrintCameraModelDetails (FILE *fp, char *prefix)
{
  double hfov = 0.0, vfov = 0.0, dfov1 = 0.0, dfov2 = 0.0;

  if (fp == NULL)
    return;

  if (prefix == NULL)
    prefix = "";

  fprintf (fp, "%sJPLCamera at memory address 0x%0lx:\n", prefix, (uintptr_t) this);
  PrintCameraModel (fp, prefix);
  fprintf (fp, "%sDerived quantities:\n", prefix);
  fprintf (fp, "%s  Dimensions:\t\t\t%ld rows by %ld cols %s\n", prefix,
	   rows, cols, (rows > 0 && cols > 0) ? "" : "[untrusted]");
  fprintf (fp, "%s  Vc,Hc (Image Center):\tRow %g, Col %g\n", prefix,
	   center[1], center[0]);
  fprintf (fp, "%s\t\t(Image center is defined with respect to\n",
	   prefix);
  fprintf (fp, "%s\t\t the upper left corner, which is 0,0)\n", prefix);
  fprintf (fp, "%s  Hs (horiz focal length):\t%g pixels\n", prefix, scale[0]);
  fprintf (fp, "%s  Vs (vert focal length):\t%g pixels\n", prefix, scale[1]);
  
  if (rows > 0 && cols > 0) {
    hfov = AngleBetweenPoints (rows / 2, 0, rows / 2, cols);
    vfov = AngleBetweenPoints (0, cols / 2, rows, cols / 2);
    dfov1 = AngleBetweenPoints (0, 0, rows, cols);
    dfov2 = AngleBetweenPoints (0, cols, rows, 0);

    fprintf (fp, "%s  HFOV:\t%g degrees\n",
	     prefix, hfov * 180.0 / M_PI);
    fprintf (fp, "%s  VFOV:\t%g degrees\n",
	     prefix, vfov * 180.0 / M_PI);
    fprintf (fp, "%s  DiagFOV:\t%g degrees downward %g degrees upward\n",
	     prefix, dfov1 * 180.0 / M_PI,
	     dfov2 * 180.0 / M_PI);
  }


  fprintf (fp, "%s  Theta (angle b/w sensor axes):\t%g degrees\n", prefix,
	   *theta * 180.0 / M_PI);
  PrintExtrinsics (fp, prefix);
}

/*!
  Find the image coordinates that correspond to a 3D point

\param x X world coordinate 
\param y Y world coordinate
\param z Z world coordinate 
\param row Address of double that will hold the row value.  May return
a row beyond the bounds of the image.
\param col Address of double that will hold the column value.  May
return a columnbeyond the bounds of the image.
\return NO_ERR if successful, PARAM_ERR if modelType is invalid.
*/

long JPLCamera::Point3DToImage2D (double x, double y, double z,
			     double *row, double *col)
{
  double P[3], ip[2], range;
  long result;

  P[0] = x; P[1] = y; P[2] = z;

  result = Point3DToImage2D (P, ip, &range, NULL);

  if (col)
    *col = ip[0];
  if (row)
    *row = ip[1];

  return result;
}


/*!
  Find the image coordinates that correspond to a 3D point.  In addition
  to the 2D projection, optional output values include the 3D perpendicular
  distance from the camera to the 3D point, and the partial derivative
  matrix of the 2D point with respect to the 3D point.

\param P World coordinates of the input 3D point
\param ip Image coordinates (x/y, not row/col) of where point P
projects into the image.
\param range Address that will hold the perpendicular distance from
the camera to the 3D point, or NULL
\param par partial derivative matrix of 2D point with respect to the
3D point.

\return NO_ERR if successful, PARAM_ERR if modelType is invalid.  */

long JPLCamera::Point3DToImage2D (double P[3], double ip[2], double *range, 
							double par[2][3]) 
{
  double range_incase;

  // The routines below do not check for NULL values, except in par.
  // Check here.
  if (range == NULL)
    range = &range_incase;

  if (modelType == CAHV_MODEL) 
    cmod_cahv_3d_to_2d (P, C, A, H, V, range, ip, par);
  else if (modelType == CAHVOR_MODEL)
    cmod_cahvor_3d_to_2d (P, C, A, H, V, O, R, FALSE, 
			      range, ip, par);
  else if (modelType == CAHVORE_MODEL)
    /* HACK -- not sure if "FALSE" is correct */
    cmod_cahvore_3d_to_2d (P, (int) *e_type, *linearity_parm,
			      C, A, H, V, O, R, E, FALSE,
			      range, ip, par);
  else {
    FatalErr ("Wrong camera model type\n");
    return PARAM_ERR;
  }
  
  return NO_ERR;
}


/*!
  Find the ray in 3D world coordinates that goes from the camera center
  through the input pixel location.

\brief Map 2D image coordinates to a 3D world coordinate vector.

\param row Row of the input pixel location
\param col Column of the input pixel location
\param ray Unit vector describing the direction of the ray that passes
through pixel (row, col).

\return NO_ERR if successful, PARAM_ERR if modelType is invalid.  */

long JPLCamera::Image2DToRay3D (double row, double col, double ray[3])
{
  double ip[2], point[3];

  ip[0] = col; ip[1] = row;
  return Image2DToRay3D (ip, point, ray, NULL);
}



/*!
  Find the ray in 3D world coordinates that goes from the camera center
  through the input pixel location.

\brief Map 2D image coordinates to a 3D world coordinate vector.

\param ip Image coordinates (x/y, not row/col) of the input point
\param P World coordinates of the origin of the computed direction.
This may or may not be the same as the camera model center, e.g. in a
fisheye lens the center of projection shifts up and down.
\param uvec Unit vector describing the computed 3D world coordinate
direction  
\param par partial derivative matrix of 2D point with respect to the
3D pointing vector.


\return NO_ERR if successful, PARAM_ERR if modelType is invalid.  */

long JPLCamera::Image2DToRay3D (double ip[2], double P[3], double uvec[3],
			   double par[3][2])
{
  if (modelType == CAHV_MODEL) 
    cmod_cahv_2d_to_3d (ip, C, A, H, V, P, uvec, par);
  else if (modelType == CAHVOR_MODEL)
    cmod_cahvor_2d_to_3d (ip, C, A, H, V, O, R, FALSE, 
			      P, uvec, par);
  else if (modelType == CAHVORE_MODEL)
    /* HACK not sure if FALSE and NULL are in the right places */
    cmod_cahvore_2d_to_3d (ip, (int) *e_type, *linearity_parm,
			       C, A, H, V, O, R, E, FALSE,
			       P, uvec, par, (double (*)[2]) NULL);
  else {
    FatalErr ("Wrong camera model type\n");
    return PARAM_ERR;
  }
  
  return NO_ERR;
}
	

/*!
  Compute the anglular difference between vectors that point through
  the given pixel coordinates.

\param row0 Point 0 row coordinate
\param col0 Point 0 column coordinate
\param row1 Point 1 row coordinate
\param col1 Point 1 column coordinate
\return Angle measurement in radians.

\sa Image2DtoRay3D
*/
double JPLCamera::AngleBetweenPoints (double row0, double col0,
				      double row1, double col1)
{
  double ray0[3], ray1[3];
  double angle = -1.0;

  if (Image2DToRay3D (row0, col0, ray0) == NO_ERR &&
      Image2DToRay3D (row1, col1, ray1) == NO_ERR)
    angle = acos (dot3 (ray0, ray1));

  return angle;
}


/*!
  Scale the size of the images reprented by this camera model.  After
  calling this function all internal camera model parameters will be
  scaled by the given amount.

\param new_rscale Scale factor for row coordinates
\param new_cscale Scale factor for column coordinates
\return NO_ERR if successful, INIT_ERR if this camera model has not
been initialized yet.
*/

long	
JPLCamera::ResizeImageCoordinate (float new_rscale, float new_cscale)
{
  int i;
  double *data = (double *) params;
  
  if (params == NULL) return INIT_ERR;
  
  // for H
  for (i = 6; i < 9; i++)
    data[i] *= new_cscale;

  // for V
  for (i = 9; i < 12; i++)
    data[i] *= new_rscale;

  // for centers & scales

  data[num_reals+0] *= new_cscale;
  data[num_reals+1] *= new_rscale;
  data[num_reals+2] *= new_cscale;
  data[num_reals+3] *= new_rscale;

  // rows and columns

  rows = (long) ((float) rows * new_rscale);
  cols = (long) ((float) cols * new_cscale);

  return NO_ERR;
}


// Translate appropriate camera model vectors
long JPLCamera::TranslateModel(float x, float y, float z)
{
  if (C) {
    C[0] += x;
    C[1] += y;
    C[2] += z;
    return NO_ERR;
  }
  return INIT_ERR;
}



// Translate appropriate camera model vectors
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



long JPLCamera::RotateModel (double P[3], double vec[3], double rads)
{
  double q1[4], q2[4], nC[3], nA[3], nH[3], nV[3], nO[3], nR[3], ns[21*21];
  double nE[3];

  quatva (vec, 0.0, q1);
  quatva (vec, rads, q2);

  if (C && A && H && V &&
      (modelType == CAHV_MODEL || (O && R))) {

    switch (modelType) {
    case CAHV_MODEL:
      cmod_cahv_move (P, q1,  C,  A,  H,  V,
			  P, q2, nC, nA, nH, nV);
      cmod_cahv_rotate_cov(q1, (double (*)[12]) s,
			       q2, (double (*)[12]) ns);
      break;
    case CAHVOR_MODEL:
      cmod_cahvor_move (P, q1,  C,  A,  H,  V,  O,  R,
			    P, q2, nC, nA, nH, nV, nO, nR);
      cmod_cahvor_rotate_cov(q1, (double (*)[18]) s,
				 q2, (double (*)[18]) ns);
      break;
    case CAHVORE_MODEL:
      cmod_cahvore_move (P, q1,  C,  A,  H,  V,  O,  R,  E,
			     P, q2, nC, nA, nH, nV, nO, nR, nE);
      cmod_cahvore_rotate_cov(q1, (double (*)[21]) s,
				  q2, (double (*)[21]) ns);
      break;
    default:
      DBG (("RotateModel:  Don't know about camera model %ld\n",
	    modelType));
      return INIT_ERR;
    }

    for (int i = 0; i < 3; i++) {
      C[i] = nC[i];
      A[i] = nA[i];
      H[i] = nH[i];
      V[i] = nV[i];
      if (modelType != CAHV_MODEL) {
	O[i] = nO[i];
	R[i] = nR[i];
	if (modelType == CAHVORE_MODEL)
	  E[i] = nE[i];
      }
    }
  } else {
    return INIT_ERR;
  }

  return NO_ERR;
} // JPLCamera::RotateModel


// utilities
long	
JPLCamera::ResizeImageCoordinate (float new_rscale, float new_cscale,
				  JPLCamera *newCam)
{
  if (newCam == NULL)
    return PARAM_ERR;

  CopyCamera (newCam);
  newCam->ResizeImageCoordinate (new_rscale, new_cscale);
  
  return NO_ERR;
}



long JPLCamera::TransformImageCoordinate (float scaleX, float scaleY, 
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
  //done. --Yalin 5/19/99
  cmod_cahv_internal (C, A, H, V, NULL, &scale[0], &center[0],
			  &scale[1], &center[1], theta, NULL);

  return NO_ERR;
}

#if 0
long JPLCamera::GenerateRectificationTable (long dstRows, long dstCols, JPLCamera *dstCam)
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
  
  if (rectificationTable) {
    DELETEV(mm, rectificationTable);
  }

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
JPLCamera::M2D_3D (double M0[9])
{
  float tmpM[9];
  int i;

  M2D_3D(tmpM);
  for (i = 0; i < 9; i++)
    M0[i] = tmpM[i];
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

void
JPLCamera::M3D_2D (double M0[9])
{
	int i;
	
	for (i = 0; i < 3; i++) {
		M0[i] = H[i];
		M0[3+i] = V[i];
		M0[6+i] = A[i];
	}
}


int RectNameEq (unsigned char *n1, int l1, unsigned char *n2, int l2)
{
  RectMapKey *key1 = (RectMapKey *) n1;
  RectMapKey *key2 = (RectMapKey *) n2;
  double eps = 1e-7;

  return EQe(key1->cahvore_hscale, key2->cahvore_hscale, eps) &&
    EQe (key1->cahvore_hcenter, key2->cahvore_hcenter, eps) &&
    EQe (key1->cahvore_vscale, key2->cahvore_vscale, eps) &&
    EQe (key1->cahvore_vcenter, key2->cahvore_vcenter, eps) &&
    EQe (key1->cahvore_r[0], key2->cahvore_r[0], eps) &&
    EQe (key1->cahvore_r[1], key2->cahvore_r[1], eps) &&
    EQe (key1->cahvore_r[2], key2->cahvore_r[2], eps) &&
    EQe (key1->cahvore_e[0], key2->cahvore_e[0], eps) &&
    EQe (key1->cahvore_e[1], key2->cahvore_e[1], eps) &&
    EQe (key1->cahvore_e[2], key2->cahvore_e[2], eps) &&
    key1->xdim == key2->xdim &&
    key1->ydim == key2->ydim &&
    EQe (key1->cahv_hscale, key2->cahv_hscale, eps) &&
    EQe (key1->cahv_hcenter, key2->cahv_hcenter, eps) &&
    EQe (key1->cahv_vscale, key2->cahv_vscale, eps) &&
    EQe (key1->cahv_vcenter, key2->cahv_vcenter, eps);
} // RectNameEq

char *RectName2s (char *prefix, unsigned char *start, int len)
{
  const size_t bufLen = 1000;
  static char buf[bufLen];
  union {
    RectMapKey *r;
    unsigned char *s;
  } u;

  if (prefix == NULL)
    prefix = "";
  if (start == NULL)
    snprintf (buf, bufLen, "%s[null]", prefix);
  u.s = start;
  snprintf (buf, bufLen, "%sH(%.6g %.6g) V(%.6g %.6g) R(%.6g %.6g %.6g) "
	   "E(%.6g %.6g %.6g)  RxC:%dx%d ==> H(%.6g %.6g) V(%.6g %.6g)",
	   prefix,
	   u.r->cahvore_hscale,
	   u.r->cahvore_hcenter,
	   u.r->cahvore_vscale,
	   u.r->cahvore_vcenter,
	   u.r->cahvore_r[0],
	   u.r->cahvore_r[1],
	   u.r->cahvore_r[2],
	   u.r->cahvore_e[0],
	   u.r->cahvore_e[1],
	   u.r->cahvore_e[2],
	   u.r->ydim, 
	   u.r->xdim,
	   u.r->cahv_hscale,
	   u.r->cahv_hcenter,
	   u.r->cahv_vscale,
	   u.r->cahv_vcenter);

  return buf;
	   
} // RectName2s

//////////////////////////////////////////////////////////////////////////////
///  SetRectificationMemoryCache -- Declare the buffer manager used for
///  semi-persistent storage of CAHVORE rectification maps.  It's only
///  semi-persistent because each invocation of
///  FindExistingRectificationMap has the potential to wipe out
///  earlier cached data, if doing so is needed to accomodate the
///  current request.  But as long as each individual request will fit
///  in the memory allocated in this manager, it will only draw from
///  this memory.

void
JPLCamera::SetRectificationMemoryCache (NamedCaches *nm)
{
  rectification_tables = nm;
} // JPLCamera::PreAllocateRectificationMemory




//////////////////////////////////////////////////////////////////////////////
///  FindExistingRectificationMap -- Returns memory needed to
///  accomodate each particular request for a rectification map.  If
///  the map has already been computed (and hasn't been kicked out of
///  the cache), the memory will contain the necessary map.  The three
///  sets of return states are:
///
///	1. result != NULL and points to warp map -- No recomputation
///	   is needed, the rectification map is still in the cache
///	2. result != NULL but contains all zeros -- The memory pointed
///	   at by result is adequate to hold the map, but is currently
///	   empty.
///	3. result == NULL -- No memory for a map of that size is
///	   available from the   NamedCaches *rectification_tables
///	   storage.  It must be found elsewhere.


char *
JPLCamera::FindExistingRectificationMap (st_cahvore_t *cahvore,
					 st_cahv_t *cahv,
					 int xdim, int ydim, int bytes)
{
  char *result = NULL;
  RectMapKey key;

  if (cahvore == NULL || cahv == NULL)
    return NULL;

  if (rectification_tables == NULL)
    return NULL;

  key.cahvore_hscale  = cahvore->hs;
  key.cahvore_hcenter = cahvore->hc;
  key.cahvore_vscale  = cahvore->vs;
  key.cahvore_vcenter = cahvore->vc;
  memcpy (key.cahvore_r, cahvore->r, sizeof(double) * 3);
  memcpy (key.cahvore_e, cahvore->e, sizeof(double) * 3);
  key.xdim            = xdim;
  key.ydim            = ydim;
  key.cahv_hscale     = cahv->hs;
  key.cahv_hcenter    = cahv->hc;
  key.cahv_vscale     = cahv->vs;
  key.cahv_vcenter    = cahv->vc;

  result = (char *) rectification_tables->Request
    ((unsigned char *) &key, sizeof(key), bytes);

  return result;
  
} // JPLCamera::FindExistingRectificationMap


#ifndef MIN
#define MIN(x, y) ((x) > (y) ? (y) : (x))
#endif

long
JPLCamera::RectifyImageUsingLinearizedFisheye (JPLPic *srcPic, JPLPic *dstPic,
					  JPLCamera *dstCam, int verbosity)
{
  TIME ("Beginning Fisheye Rectification");
  if (modelType != CAHVORE_MODEL) {
    FatalErr ("LinearizedFisheye Can only rectify CAHVORE models");
    return PARAM_ERR;
  } else if (dstCam == NULL || dstCam->modelType != CAHV_MODEL) {
    FatalErr ("LinearizedFisheye can only rectify to a CAHV model");
    return PARAM_ERR;
  } else if (srcPic == NULL || dstPic == NULL) {
    FatalErr ("LinearizedFisheye cannot handle NULL images");
    return PARAM_ERR;
  }

  st_cahvore_t cahvore;
  st_cahv_t cahv;

  // Copy data into Todd's format
  memcpy (cahvore.c, C, sizeof(double) * 3);
  memcpy (cahvore.a, A, sizeof(double) * 3);
  memcpy (cahvore.h, H, sizeof(double) * 3);
  memcpy (cahvore.v, V, sizeof(double) * 3);
  memcpy (cahvore.o, O, sizeof(double) * 3);
  memcpy (cahvore.r, R, sizeof(double) * 3);
  memcpy (cahvore.e, E, sizeof(double) * 3);
  cahvore.hs = scale[0];
  cahvore.hc = center[0];
  cahvore.vs = scale[1];
  cahvore.vc = center[1];
  cahvore.theta = *theta;
  cahvore.mparm = *linearity_parm;
  cahvore.mtype = (int) *e_type;
  cahvore.xdim = cols;
  cahvore.ydim = rows;

  if (verbosity > 1)
    printf ("RectifyUsingLinearizedFisheye: Rows/Cols is %ldx%ld, "
	    "srcPic is %ldx%ld\n",
	    rows, cols, srcPic->rows, srcPic->cols);

  memcpy (cahv.c, dstCam->C, sizeof(double) * 3);
  memcpy (cahv.a, dstCam->A, sizeof(double) * 3);
  memcpy (cahv.h, dstCam->H, sizeof(double) * 3);
  memcpy (cahv.v, dstCam->V, sizeof(double) * 3);
  cahv.hs = dstCam->scale[0];
  cahv.hc = dstCam->center[0];
  cahv.vs = dstCam->scale[1];
  cahv.vc = dstCam->center[1];
  cahv.theta = *dstCam->theta;

  /* See if the rectification map has already been computed, or if
     there is space for it in the NamedCaches list */

  int map_size = (srcPic->rows * srcPic->cols + 40) * sizeof(float) * 2;
  int free_warp_map = 0;
  char *warp_map = FindExistingRectificationMap (&cahvore, &cahv,
						 srcPic->cols,
						 srcPic->rows,
						 map_size);

  if (warp_map == NULL) {
    warp_map = NEW(mm, "Rectification Warp Map") char[map_size];
    memset (warp_map, 0, map_size);
    if (warp_map)
      free_warp_map = 1;
    else {
      FatalErr ("LinearizedFisheye failed to allocate rectification map!");
      return MEM_ERR;
    } // else
  } // if

  TIME ("Time to find warp map storage");
  warp_float_t *wm = (warp_float_t *) warp_map;
 
  if (wm->datatype != WARP_FLOAT || wm->xdim != srcPic->cols ||
      wm->ydim != srcPic->rows) {
    if (st_warp_bilinear_init_cahvore (&cahvore, &cahv, 1.0,
				       srcPic->cols, srcPic->rows,
				       &warp_map, map_size) == FAILURE) {
      FatalErr ("LinearizedFisheye failed to initialize cahvore warp!");
      if (free_warp_map)
	DELETEV (mm, warp_map);
      return INIT_ERR;
    }
    TIME ("Time to calculate warp map");
    if (verbosity > 1 && rectification_tables)
      rectification_tables->Show();
  }
  if (verbosity > 1)
    printf ("WarpMap 0x%lx Datatype %d, rows %d, cols %d need_to_free=%d\n",
	    (uintptr_t) wm, wm->datatype, wm->ydim, wm->xdim, free_warp_map);

#ifdef DISPLAY_RECTIFICATION_MAPS
  JPLPic wxpic(mm), wypic(mm);
  if (wxpic.LoadFromMemory (((unsigned char *) warp_map) + 3 * sizeof(int),
			    srcPic->rows, srcPic->cols, FLOAT_PIXEL) != NO_ERR)
    printf ("WARNING Failed to load wxpic\n");
  else {
    printf ("RECTIFICATION WARP MAP: X VALUES\n");
    wxpic.WriteStats();
    wxpic.RenderInASCII();
  }
  if (wypic.LoadFromMemory (((unsigned char *) warp_map) + 3 * sizeof(int) +
			    srcPic->rows * srcPic->cols * sizeof(float),
			    srcPic->rows, srcPic->cols, FLOAT_PIXEL) != NO_ERR)
    printf ("WARNING Failed to load wypic\n");
  else {
    printf ("RECTIFICATION WARP MAP: Y VALUES\n");
    wypic.WriteStats();
    wypic.RenderInASCII();
  }
#endif

  // Assume dstPic is pre-sized

  int x0 = 0, y0 = 0, nx = srcPic->cols, ny = srcPic->rows;

  if (st_warp_bilinear (warp_map, srcPic->cols, srcPic->rows,
			x0, y0, nx, ny,
			srcPic->GetPixelAddress(0,0),
			dstPic->GetPixelAddress(0,0)) == FAILURE) {
    FatalErr ("LinearizedFisheye failed to perform rectification");
    if (free_warp_map)
      DELETEV (mm, warp_map);
    return PARAM_ERR;
  }
  TIME ("Time to perform image rectification resampling");
  if (free_warp_map)
    DELETEV (mm, warp_map);

  return NO_ERR;

} // JPLCamera::RectifyImageUsingLinearizedFisheye


//#define COMP_ERR

// rectify one image without the rectification table
long	
JPLCamera::RectifyImage (JPLPic *srcPic, JPLPic *dstPic, JPLCamera *dstCam,
			unsigned char numInterval, int verbosity)
{
  float M0[9], M1[9], M2[9], M3[9]; 
  float dstM0[9], tmpM[9];
  float OF[3];
  int i, piecewise, interval, length;
  float x, y, Z, X, Y, tao, nscale;
  float qa, qb, qc, qd, qe, qf;
  float qgx, qhx, qgy, qhy, qgz, qhz;
  float x0, x1, x2, y_0, y_1, y2;
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

  if (modelType == CAHVORE_MODEL) {
    return RectifyImageUsingLinearizedFisheye (srcPic, dstPic, dstCam,
					       verbosity);
  }

  if (modelType == CAHV_MODEL) {
    float qx, qy, qz;
    long srcRB, dstRB;
    unsigned char *dst;
    register unsigned char *src, *ss, *d;
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
	nscale = 1.0 / (float) length;
		
        // first column
	x0 = x2; y_0 = y2;
				
	// middle column
	x = col + length * 0.5;
	X = M0[0] * x + qx;
	Y = M0[3] * x + qy;
	Z = M0[6] * x + qz;
	Z = 1.0 / Z;
	x1 = X * Z;
	y_1 = Y * Z;
				
	// last column
	x = col + length;
	X = M0[0] * x + qx;
	Y = M0[3] * x + qy;
	Z = M0[6] * x + qz;
	Z = 1.0 / Z;
	x2 = X * Z;
	y2 = Y * Z;
				
	// calculate quadratic coeffs:
	ax = 2.0 * ((x2 - x0) - 2.0 * (x1 - x0)) * nscale * nscale;
	bx = (4.0 * (x1 - x0) - (x2 - x0)) * nscale ;
	ay = 2.0 * ((y2 - y_0) - 2.0 * (y_1 - y_0)) * nscale * nscale;
	by = (4.0 * (y_1 - y_0) - (y2 - y_0)) * nscale;
		
	// setup forward differences:
	dstX = (long int) (x0 * bits_scale);
	dstY = (long int) (y_0 * bits_scale);
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
	    ss = src + iy * srcRB + ix;

	    /* Fetch data */
	    p00 = (long int) *ss++;
	    p01 = (long int) *ss;
	    ss += srcRB;
	    p11 = (long int) *ss;
	    p10 = (long int) *--ss;
	
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
      nscale = 1.0 / (float) length;
      
      // first column
      x0 = x2; y_0 = y2;
      
      // middle column
      x = col + length * 0.5;
      tao = ((qa * x + qb) * x + qc) / ((qd * x + qe) * x + qf);
      tao = R[0] + (R[1] + R[2] * tao) * tao;
      X = (M0[0] + tao * M1[0]) * x + qgx + tao * qhx;
      Y = (M0[3] + tao * M1[3]) * x + qgy + tao * qhy;
      Z = 1.0 / ((M0[6] + tao * M1[6]) * x + qgz + tao * qhz);
      x1 = X * Z; y_1 = Y * Z;
      
      // last column
      x = col+length;
      tao = ((qa * x + qb) * x + qc) / ((qd * x + qe) * x + qf);
      tao = R[0] + (R[1] + R[2] * tao) * tao;
      X = (M0[0] + tao * M1[0]) * x + qgx + tao * qhx;
      Y = (M0[3] + tao * M1[3]) * x + qgy + tao * qhy;
      Z = 1.0 / ((M0[6] + tao * M1[6]) * x + qgz + tao * qhz);
      x2 = X * Z; y2 = Y * Z;
      
      // calculate quadratic coeffs:
      ax = 2.0 * ((x2 - x0) - 2.0 * (x1 - x0)) * nscale * nscale;
      bx = (4.0 * (x1 - x0) - (x2 - x0)) * nscale ;
      ay = 2.0 * ((y2 - y_0) - 2.0 * (y_1 - y_0)) * nscale * nscale;
      by = (4.0 * (y_1 - y_0) - (y2 - y_0)) * nscale;
      
      // setup forward differences:
      dstX = (long) (x0 * bits_scale);
      dstY = (long) (y_0 * bits_scale);
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
	dstY = (y_0 + ay * x * x + by * x) * bits_scale;
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

long JPLCamera::TransformStereoCameras (
			JPLCamera *rightCam,
			JPLCamera *leftResult, 
			JPLCamera *rightResult,
			double leftNodalPos[3],
			double leftToRightDirection[3],
			double leftPointingDirection[3])
{
  float fNodalPos[3], fRight[3], fPoint[3];
  int i;

  for (i = 0; i < 3; i++) {
    fNodalPos[i] = (float) leftNodalPos[i];
    fRight[i] = (float) leftToRightDirection[i];
    fPoint[i] = (float) leftPointingDirection[i];
  }

  return TransformStereoCameras (rightCam, leftResult, rightResult,
				 fNodalPos, fRight, fPoint);
}


long JPLCamera::TransformStereoCameras (
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
  if (rows <= 0 || cols <= 0) {
    Warning ("***** Camera model ROWS/COLS not set; assuming 486x512\n");
    rows = 486; cols = 512;
  }

  icenter[0] = (cols - 1.0) * 0.5;
  icenter[1] = (rows - 1.0) * 0.5;

  Image2DToRay3D (icenter, P, vec, NULL);

  // overwrites the computed image center ray (vec) with A???
#define USE_A_NOT_CENTER
#ifdef USE_A_NOT_CENTER
  vec[0] = A[0];
  vec[1] = A[1];
  vec[2] = A[2];
#endif

#ifdef INIT_DEBUG
  printf("pointing: %f %f %f\n", vec[0], vec[1], vec[2]);
#endif

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

#ifdef INIT_DEBUG
  printf("Target base = %f %f %f\n", lr[0], lr[1], lr[2]);
  printf("Target pointing = %f %f %f\n", P[0], P[1], P[2]);
  printf("Current base = %f %f %f\n", base[0], base[1], base[2]);
  printf("Current pointing = %f %f %f\n", vec[0], vec[1], vec[2]);
#endif

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

#ifdef INIT_DEBUG
  printf("rot = \t %f %f %f\n\t\t %f %f %f\n\t\t %f %f %f\n",
	 rot[0][0], rot[0][1], rot[0][2],
	 rot[1][0], rot[1][1], rot[1][2],
	 rot[2][0], rot[2][1], rot[2][2]);
#endif

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


long
JPLCamera::TransformStereoCamerasRot (
			JPLCamera *rightCam,
			JPLCamera *leftResult, 
			JPLCamera *rightResult,
			double leftNodalPos[3],
			double rotation[3][3])
{
  double tmpC[3];

  int i;
  
  copy3(leftNodalPos, leftResult->C);

  /* rotating the camera models */
  mult331 ( rotation, A, leftResult->A);
  mult331 ( rotation, H, leftResult->H);
  mult331 ( rotation, V, leftResult->V);
  if (modelType == CAHVOR_MODEL)
    mult331 ( rotation, O, leftResult->O);
  copy3(R, leftResult->R);
 
  sub3(rightCam->C, C, tmpC);
 
  mult331 (rotation, tmpC,  rightResult->C);
  add3(rightResult->C, leftResult->C, rightResult->C);

  mult331 ( rotation, rightCam->A, rightResult->A);
  mult331 ( rotation, rightCam->H, rightResult->H);
  mult331 ( rotation, rightCam->V, rightResult->V);
  if (modelType == CAHVOR_MODEL)
    mult331 (rotation,rightCam->O,  rightResult->O);
  copy3(rightCam->R, rightResult->R);
  
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


long JPLCamera::GetPointingDirections (double ret_fwd[3], double ret_up[3],
				      double ret_right[3])
{
  double sensor_right[3], tmp[3];
  double fwd[3], up[3], right[3];
  double vc, hc, hs, mag;

  // Components of H and V are the image center in the A direction, and the
  // image scale in the HorizVec direction (which is known to be normal to
  // A).  So we take the dot product with A to extract the image center.

  vc = dot3 (A, V);
  hc = dot3 (A, H);
  hs = mag3 (cross3 (A, H, tmp));

  // A vector is the surface normal to the sensor plane, NOT the forward
  // vector.  To find the forward vector we project a ray out from the
  // image center.

  if (Image2DToRay3D (vc, hc, fwd) != NO_ERR)
    return PARAM_ERR;

  sensor_right[0] = (H[0] - hc * A[0]) / hs;
  sensor_right[1] = (H[1] - hc * A[1]) / hs;
  sensor_right[2] = (H[2] - hc * A[2]) / hs;

  if (!EQ(mag3(sensor_right), 1.0)) {
    return INIT_ERR;
  }
  if (cross3 (sensor_right, fwd, up) == NULL)
    return INIT_ERR;

  mag = mag3 (up);
  if (!EQ(mag, 0.0)) {
    up[0] /= mag;
    up[1] /= mag;
    up[2] /= mag;
  }

  if (cross3 (fwd, up, right) == NULL)
    return INIT_ERR;

  mag = mag3 (right);
  if (!EQ(mag, 0.0)) {
    right[0] /= mag;
    right[1] /= mag;
    right[2] /= mag;
  }

  if (ret_up) {
    ret_up[0] = up[0];
    ret_up[1] = up[1];
    ret_up[2] = up[2];
  }
  if (ret_right) {
    ret_right[0] = right[0];
    ret_right[1] = right[1];
    ret_right[2] = right[2];
  }
  if (ret_fwd) {
    ret_fwd[0] = fwd[0];
    ret_fwd[1] = fwd[1];
    ret_fwd[2] = fwd[2];
  }
  return NO_ERR;
}

static void apply_rotation_quaternion_to_vector (double quat[4], double vec[3])
{
  double invquat[4], q1[4], q2[4];
  int i;

  copyq (quat, invquat);
  for (i = 1; i < 4; i++)
    invquat[i] = -invquat[i];
 
  // Now invquat is the inverse of the quaternion.

  // Copy the vector into a temporary quaternion and perform the
  // multiplication to rotate the vector:  q * P * q'
  
  q1[0] = 0;
  (void) copy3 (vec, q1 + 1);
  (void) multq (quat, q1, q2);
  (void) multq (q2, invquat, q1);
  if (!EQ(q1[0],0))
    DBG(("apply_rot_q: WARNING!! Rotated point quaternion has %g nonzero first elt\n", q1[0]));
  (void) copy3 (q1 + 1, vec);
} // apply_rotation_quaternion_to_vector



void JPLCamera::HandleOneVectorRepointing (double desired[3],
					   double current[3],
					   double orthog_from_current[3],
					   double other[3], char *desc)
{
  double quat[4];
  double dotp, angle;

  // Compute the dot product between current and desired vectors.
  // Recall that for unit vectors,  cos ANGLE = A dot B

  dotp = dot3 (desired, current);

  if (EQ (dotp, 1.0)) {

    // If we're already pointing that way, don't bother repointing

#ifdef POINTING_DEBUG
    DBG(("Camera already pointing in desired %s direction\n", desc ?
	 desc : ""));
#endif
  } else if (EQ (dotp, -1.0)) {

    // If we're pointing in the other direction, flip by 180 degrees
    // We can rotate about any orthogonal axis (use the given one)
    // We can rotate about any point, we arbitrarily use C.

    RotateModel (C, orthog_from_current, M_PI);

    (void) quatva (orthog_from_current, M_PI, quat);
    apply_rotation_quaternion_to_vector (quat, current);
    apply_rotation_quaternion_to_vector (quat, other);
  } else {
    double res[3];

    // Have to re-target the forward vector from an arbitrary (but
    // nonsingular) orientation

    angle = acos (dotp);

    // Compute an orthogonal vector, about which to rotate

    if (cross3 (current, desired, res) == NULL) {
      DBG (("HandleOneVectorRepointing: Failed to reorient FWD vector!!\n"));
    } else {

      // Now we have a vector normal to the plane containing both the
      // current and the desired forward vectors.  And the amount to
      // rotate about that vector is stored in   angle.

      // First update the camera model.

      RotateModel (C, res, angle);

      // Now compute the quaternion so we can updated the current axes

      (void) quatva (res, angle, quat);

      // Now   quat   is a unit quaternion representing the rotation
      // about   res.

      apply_rotation_quaternion_to_vector (quat, current);
      apply_rotation_quaternion_to_vector (quat, other);
    }
  }

#ifdef POINTING_DEBUG
  fprintf (stderr, "AFTER %s ROTATION:\n", desc ? desc : "");
  fprintf (stderr, "Curr %s: (%.03lg,%.03lg,%.03lg)  "
	   "OTHER: (%.03lg,%.03lg,%.03lg)\n", desc ? desc : "",
	   current[0], current[1], current[2], other[0], other[1],
	   other[2]);
#endif
}

long JPLCamera::UpdatePointingDirection (double newfwd[3], double newup[3])
{
  double curfwd[3], curup[3], currt[3];
  double norm;

  ////////////////////////////////////////////////////////////////////////
  // Ensure inputs are in the proper format

  // Make sure the desired forward vector is a unit vector
  norm = mag3(newfwd);
  if (EQ(norm,0.0))
    return PARAM_ERR;
  else if (!EQ(norm, 1.0)) {
    newfwd[0] /= norm;
    newfwd[1] /= norm;
    newfwd[2] /= norm;
  }

  // Make sure the two input vectors are orthogonal
  if (!EQ(dot3 (newfwd, newup), 0.0)) {
    double tmprt[3];

    if (cross3 (newfwd, newup, tmprt) == NULL)
      return PARAM_ERR;
    if (cross3 (tmprt, newfwd, newup) == NULL)
      return PARAM_ERR;
  }

  // Make sure the desired up vector is a unit vector
  norm = mag3(newup);
  if (EQ(norm,0.0))
    return PARAM_ERR;
  else if (!EQ(norm, 1.0)) {
    newup[0] /= norm;
    newup[1] /= norm;
    newup[2] /= norm;
  }

  believe (EQ (mag3 (newfwd), 1.0));
  believe (EQ (mag3 (newup), 1.0));

  ////////////////////////////////////////////////////////////////////////
  // At this point, we're certain that newfwd and newup are orthogonal
  // unit vectors.  Now extract the current pointing vectors from the
  // camera model.


  if (GetPointingDirections (curfwd, curup, currt) != NO_ERR)
    return INIT_ERR;

  believe (EQ (mag3 (curfwd), 1.0));
  believe (EQ (mag3 (curup), 1.0));
  believe (EQ (mag3 (currt), 1.0));

#ifdef POINTING_DEBUG
  fprintf (stderr, "Curr FWD: (%.03lg,%.03lg,%.03lg)  UP: (%.03lg,%.03lg,%.03lg)\n",
	   curfwd[0], curfwd[1], curfwd[2], curup[0], curup[1], curup[2]);
#endif

  ////////////////////////////////////////////////////////////////////////
  // First rotate the FORWARD vectors into place.  Change both the
  // internal camera parameters and also the curfwd and curup vectors.

  HandleOneVectorRepointing (newfwd, curfwd, currt, curup, "FWD");


#ifdef POINTING_DEBUG
  fprintf (stderr, "New  FWD: (%.03lg,%.03lg,%.03lg)  UP: (%.03lg,%.03lg,%.03lg)\n",
	   newfwd[0], newfwd[1], newfwd[2], newup[0], newup[1], newup[2]);
#endif

  ////////////////////////////////////////////////////////////////////////
  // Now we know that   curfwd   is aligned with the desired FWD
  // axis.  Do what's needed to align the current UP axis with
  // the desired direction.  Note that we're just passing   currt
  // as a placeholder; we don't need currt, and after the previous
  // rotation, we know for sure that curfwd is already orthogonal to
  // the plane containing curup and newup (assuming it's unique).

  HandleOneVectorRepointing (newup, curup, curfwd, currt, "FWD");

  ////////////////////////////////////////////////////////////////////////
  // Verify that rotations worked

#ifdef POINTING_DEBUG
  fprintf (stderr, "END OF UPDATE:\n");
  fprintf (stderr, "Curr FWD: (%.03lg,%.03lg,%.03lg)  UP: (%.03lg,%.03lg,%.03lg)\n",
	   curfwd[0], curfwd[1], curfwd[2], curup[0], curup[1], curup[2]);
  fprintf (stderr, "New  FWD: (%.03lg,%.03lg,%.03lg)  UP: (%.03lg,%.03lg,%.03lg)\n",
	   newfwd[0], newfwd[1], newfwd[2], newup[0], newup[1], newup[2]);
#endif

  if (!is_equal3 (curfwd, newfwd, .00001)) {
    DBG (("**** UpdatePointingDirection: Failed to rotate FWD vector!!!\n"));
  }
  // HACK!! Had to decrease the precision here.  Somehow the curfwd and curup
  // vectors aren't quite as orthonormal as they used to be.
  if (!is_equal3 (curup, newup, .001)) {
    DBG (("**** UpdatePointingDirection: Failed to rotate UP vector!!!\n"));
  }
  return NO_ERR;
}



#ifndef OMIT_IMAGE_OPERATORS

/* Generate a 3x3 projective matrix from two CAHV cameras and a ground plane */

/* the matrix M will convert left coordinate to right */
long JPLCamera::PlaneMap (JPLCamera *rightCam,
		     float *groundPlane, float M[9])
{
  if (modelType != CAHV_MODEL ||
      rightCam->modelType != CAHV_MODEL) {
    FatalErr ("projective map is between CAHV models\n");
    return INIT_ERR;
  }
  if (groundPlane == NULL) {
    FatalErr ("no ground plane\n");
    return INIT_ERR;
  }
  float Ml_1[9], Mr[9];
  float Cl_Cr[3], ss;
  int i;
	
  M2D_3D (Ml_1);
  rightCam->M3D_2D (Mr);
  ss = 0.0;
  for (i = 0; i < 3; i++) {
    Cl_Cr[i] = C[i] - rightCam->C[i];
    ss += C[i] * groundPlane[i];
  }
  ss = -(ss + groundPlane[3]);
	
  LinearTransform (Mr, Ml_1, M, 3, 3, 3);
	
  LinearTransform (Mr, Cl_Cr, Cl_Cr, 3, 3, 1);
  LinearTransform (Cl_Cr, groundPlane, Mr, 3, 1, 3);
  LinearTransform (Mr, Ml_1, Mr, 3, 3, 3);

  for (i = 0; i < 9; i++)
    M[i] = M[i] * ss + Mr[i];
		
  return NO_ERR;
}

// rectify one image with an additional perspective warping.

long	
JPLCamera::RectifyImage (JPLPic *srcPic, JPLPic *dstPic, JPLCamera *dstCam,
				float Madd[9], unsigned char numInterval)
{
  float M0[9], M1[9], M2[9], M3[9], MaddCopy[9]; 
  float dstM0[9], tmpM[9];
  float OF[3];
  int i, piecewise, interval, length;
  float x, y, Z, X, Y, tao, nscale;
  float qa, qb, qc, qd, qe, qf;
  float qgx, qhx, qgy, qhy, qgz, qhz;
  float x0, x1, x2, y_0, y_1, y2;
  float ax, bx, ay, by;
  register long dstX, dstY, fracBits, xLimit, yLimit;
  long bits_scale;
  register long linearx, lineary, qxdd, qydd;
  JPLPic    fieldSrcPic(mm), fieldDstPic(mm);
  long srcRB, dstRB;
  unsigned char *dst;
  register unsigned char *src, *ss, *d;
  long row, nrows, ncols, srcRows, srcCols;
  register long col, count;
	
  if (srcPic->field) {
    float sM[9];
    srcPic->SeparateFields (&fieldSrcPic, NULL); // get even field
    srcPic = &fieldSrcPic;
    // decimate
    TransformImageCoordinate (1.0, 0.5, 0.0, 0.0);
    M3D_2D (M0);
    // restore
    TransformImageCoordinate (1.0, 2.0, 0.0, 0.0);
    dstCam->TransformImageCoordinate (1.0, 0.5, 0.0, 0.0);
    dstCam->M2D_3D (dstM0);
    dstCam->TransformImageCoordinate (1.0, 2.0, 0.0, 0.0);
		
    dstPic->field = true;
    dstPic->SeparateFields (&fieldDstPic, NULL);
    dstPic = &fieldDstPic;
		
    for (i = 0; i < 9; i++) sM[i] = 0;
    sM[0] = 1.0;
    sM[4] = 2.0;
    sM[8] = 1.0;
		
    LinearTransform (Madd, sM, MaddCopy, 3, 3, 3);
    sM[4] = 0.5;
    LinearTransform (sM, MaddCopy, MaddCopy, 3, 3, 3);
  } else {
    M3D_2D (M0);
    dstCam->M2D_3D (dstM0);
		
    dstPic->field = false;
    for (i = 0; i < 9; i++) 
      MaddCopy[i] = Madd[i];
  }
	
  if (dstCam->modelType != CAHV_MODEL) {
    FatalErr ("Cannot rectify to a distortion model\n");
    return INIT_ERR;
  }
  if (modelType == CAHV_MODEL) {
    float qx, qy, qz;
		
    LinearTransform (M0, dstM0, M0, 3, 3, 3);
    LinearTransform (M0, MaddCopy, M0, 3, 3, 3); // additional perspective transform
		
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
			
      qx = M0[1] * y + M0[2];
      qy = M0[4] * y + M0[5];
      qz = M0[7] * y + M0[8];
			
      Z = 1.0 / qz;
      x2 = qx * Z;
      y2 = qy * Z;
			
      for (piecewise = 0, col = 0, d = dst; piecewise < numInterval; 
	   piecewise++) {
				
	length = MIN ((ncols - col), interval);
	nscale = 1.0 / (float) length;
		
				// first column
	x0 = x2; y_0 = y2;
				
				// middle column
	x = col + length * 0.5;
	X = M0[0] * x + qx;
	Y = M0[3] * x + qy;
	Z = M0[6] * x + qz;
	Z = 1.0 / Z;
	x1 = X * Z;
	y_1 = Y * Z;
				
				// last column
	x = col + length;
	X = M0[0] * x + qx;
	Y = M0[3] * x + qy;
	Z = M0[6] * x + qz;
	Z = 1.0 / Z;
	x2 = X * Z;
	y2 = Y * Z;
				
				// calculate quadratic coeffs:
	ax = 2.0 * ((x2 - x0) - 2.0 * (x1 - x0)) * nscale * nscale;
	bx = (4.0 * (x1 - x0) - (x2 - x0)) * nscale ;
	ay = 2.0 * ((y2 - y_0) - 2.0 * (y_1 - y_0)) * nscale * nscale;
	by = (4.0 * (y_1 - y_0) - (y2 - y_0)) * nscale;
		
				// setup forward differences:
	dstX = (long) (x0 * bits_scale);
	dstY = (long) (y_0 * bits_scale);
	linearx = (long) ((bx + ax) * bits_scale);
	lineary = (long) ((by + ay) * bits_scale);
	qxdd = (long) (2.0 * ax * bits_scale);
	qydd = (long) (2.0 * ay * bits_scale);

	col += length;
	for (count = length; count--; d++,
	       dstX += linearx, dstY += lineary,
	       linearx += qxdd, lineary += qydd) {
					
	  if (dstX < 0 || dstX > xLimit ||
	      dstY < 0 || dstY > yLimit) {
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
  LinearTransform (M1, MaddCopy, M1, 3, 3, 3); // additional perspective transform
  LinearTransform (M0, dstM0, M0, 3, 3, 3);
  LinearTransform (M0, MaddCopy, M0, 3, 3, 3); // additional perspective transform

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
		
    for (piecewise = 0, col = 0, d = dst; piecewise < numInterval; 
	 piecewise++) {
		
      length = MIN ((ncols - col), interval);
      nscale = 1.0 / (float) length;
		
			// first column
      x0 = x2; y_0 = y2;
			
			// middle column
      x = col + length * 0.5;
      tao = ((qa * x + qb) * x + qc) / ((qd * x + qe) * x + qf);
      tao = R[0] + (R[1] + R[2] * tao) * tao;
      X = (M0[0] + tao * M1[0]) * x + qgx + tao * qhx;
      Y = (M0[3] + tao * M1[3]) * x + qgy + tao * qhy;
      Z = 1.0 / ((M0[6] + tao * M1[6]) * x + qgz + tao * qhz);
      x1 = X * Z; y_1 = Y * Z;

			// last column
      x = col+length;
      tao = ((qa * x + qb) * x + qc) / ((qd * x + qe) * x + qf);
      tao = R[0] + (R[1] + R[2] * tao) * tao;
      X = (M0[0] + tao * M1[0]) * x + qgx + tao * qhx;
      Y = (M0[3] + tao * M1[3]) * x + qgy + tao * qhy;
      Z = 1.0 / ((M0[6] + tao * M1[6]) * x + qgz + tao * qhz);
      x2 = X * Z; y2 = Y * Z;
		
			// calculate quadratic coeffs:
      ax = 2.0 * ((x2 - x0) - 2.0 * (x1 - x0)) * nscale * nscale;
      bx = (4.0 * (x1 - x0) - (x2 - x0)) * nscale ;
      ay = 2.0 * ((y2 - y_0) - 2.0 * (y_1 - y_0)) * nscale * nscale;
      by = (4.0 * (y_1 - y_0) - (y2 - y_0)) * nscale;
		
			// setup forward differences:
      dstX = (long) (x0 * bits_scale);
      dstY = (long) (y_0 * bits_scale);
      linearx = (long) ((bx + ax) * bits_scale);
      lineary = (long) ((by + ay) * bits_scale);
      qxdd = (long) (2.0 * ax * bits_scale);
      qydd = (long) (2.0 * ay * bits_scale);

      col += length;
      for (count = length; count--; d++,
	     dstX += linearx, dstY += lineary,
	     linearx += qxdd, lineary += qydd) {
/*				
				x = length - 1 - count;
				dstX = (x0 + ax * x * x  +
							bx *x) * bits_scale;
				dstY = (y_0 + ay * x * x +
							by * x) * bits_scale;
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

	if (dstX < 0 || dstX > xLimit ||
	    dstY < 0 || dstY > yLimit) {
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


#endif /* ! OMIT_IMAGE_OPERATORS */

#ifndef SQ
#define SQ(x)  ((x) * (x))
#endif

#ifdef THIS_USES_YALINS_OLD_CODE
int
JPLCamera::RectifyFisheye (JPLCamera *originalCam, JPLCamera *newCam,
			   JPLPic *originalPic, JPLPic *newPic)
{
  float horizontal[2], vertical[2];
  double E[3], RR[3], pos[2], pos3[3];
  double h[3], v[3], hnorm, vnorm, ray[3], p[3], dist;
  int i, nrows, ncols, row, col;
  double st, ct, sp, cp, pan, tilt, hres, vres;
  unsigned char *src, *dst, *d;
  long srcRB, dstRB;

  E[0] = E[1] = E[2] = 0.0;
  RR[0] = RR[1] = RR[2] = 0.0;

  nrows = originalPic->rows;
  ncols = originalPic->cols;

  /* project the boundary pixels to 3D to estimate the
     horizontal and vertical fov  of the new camera */
  /* horizontal */
  pos[0] = 0.0;
  pos[1] = newCam->center[1];
  cmod_cahvore_2d_to_3d_fisheye (pos, newCam->C, newCam->A,
				 newCam->H, newCam->V,
				 newCam->A, RR, E, FALSE, 
				 pos3, ray, NULL, NULL);
  horizontal[0] = -acos (ray[0] * newCam->A[0] + 
			ray[1] * newCam->A[1] +
			ray[2] * newCam->A[2]);
  pos[0] = ncols - 1;
  pos[1] = newCam->center[1];
  cmod_cahvore_2d_to_3d_fisheye (pos, newCam->C, newCam->A,
				 newCam->H, newCam->V,
				 newCam->A, RR, E, FALSE, 
				 pos3, ray, NULL, NULL);
  horizontal[1] = acos (ray[0] * newCam->A[0] + 
			ray[1] * newCam->A[1] +
			ray[2] * newCam->A[2]);

  /* vertical */
  pos[0] = newCam->center[0];
  pos[1] = 0;
  cmod_cahvore_2d_to_3d_fisheye (pos, newCam->C, newCam->A,
				 newCam->H, newCam->V,
				 newCam->A, RR, E, FALSE, 
				 pos3, ray, NULL, NULL);
  vertical[0] = -acos (ray[0] * newCam->A[0] + 
		      ray[1] * newCam->A[1] +
		      ray[2] * newCam->A[2]);
  pos[0] = newCam->center[0];
  pos[1] = nrows - 1;
  cmod_cahvore_2d_to_3d_fisheye (pos, newCam->C, newCam->A,
				 newCam->H, newCam->V,
				 newCam->A, RR, E, FALSE, 
				 pos3, ray, NULL, NULL);
  vertical[1] = acos (ray[0] * newCam->A[0] + 
		      ray[1] * newCam->A[1] +
		      ray[2] * newCam->A[2]);

  src = originalPic->GetPixelAddress ();
  srcRB = originalPic->GetRowBytes ();
  dst = newPic->GetPixelAddress ();
  dstRB = newPic->GetRowBytes ();

  /* compute the horizontal and vertical vector */
  hnorm = vnorm = 0.0;
  for (i = 0; i < 3; i++) {
    h[i] = newCam->H[i] - newCam->center[0] * newCam->A[i];
    hnorm += h[i] * h[i];
    v[i] = newCam->V[i] - newCam->center[1] * newCam->A[i];
    vnorm += v[i] * v[i];
  }
  if (hnorm == 0.0 || vnorm == 0.0) {
    FatalErr("Camera parameter incorrect \n");
    return INIT_ERR;
  }
  hnorm = 1.0 / sqrt (hnorm);
  vnorm = 1.0 / sqrt (vnorm);
  for (i = 0; i < 3; i++) {
    h[i] *= hnorm;
    v[i] *= vnorm;
  }
  printf("hvec = (%5f, %5f, %5f)\n", h[0], h[1], h[2]);
  printf("vvec = (%5f, %5f, %5f)\n", v[0], v[1], v[2]);
  printf ("vertical fov = %f degrees, horizontal fov = %f degrees\n",
	  (vertical[1] - vertical[0]) * 180.0 / 3.1415926535,
	  (horizontal[1] - horizontal[0]) * 180.0 / 3.1415926535);
  hres = (horizontal[1] - horizontal[0]) / ((double) ncols - 1);
  vres = (vertical[1] - vertical[0]) / ((double) nrows - 1);
  for (row = 0; row < nrows; row ++, dst += dstRB) {
    double phi, t0, t1;
    tilt = vertical[0] + vres * row;

    st = sin (tilt);
    ct = cos (tilt);

    d = dst;

    {
      //      int yoffset;
      //      yoffset = row;
		      
      phi = 1.0 / (sqrt(1 + SQ((row - newCam->center[1]) /
			       ((double) (nrows - 1) * 0.5)) * 0.1));
      t0 = asin (sin (horizontal[0]) * phi);
      t1 = asin (sin (horizontal[1]) * phi);
      //     printf("Row = %3d, Horizontal limits (%7.4f %7.4f)\n",
      //	     row, t0, t1);
      hres = (t1 - t0) / ((double) ncols - 1);
    }
    for (col = 0; col < ncols; col++, d++) {
      //      pan = horizontal[0] / phi + hres * col;
      pan = t0 + hres *col;

      sp = sin (pan);
      cp = cos (pan);

      p[0] = sp * h[0];
      p[1] = sp * h[1];
      p[2] = sp * h[2];

      p[0] += cp * (ct * newCam->A[0] + st * v[0]) + newCam->C[0];
      p[1] += cp * (ct * newCam->A[1] + st * v[1]) + newCam->C[1];
      p[2] += cp * (ct * newCam->A[2] + st * v[2]) + newCam->C[2];

      cmod_cahvore_3d_to_2d_fisheye (p, originalCam->C,
				     originalCam->A, originalCam->H,
				     originalCam->V, originalCam->O,
				     originalCam->R, E, 0,
				     &dist, pos, NULL);
      /*      if (row == 120) {
	printf ("Map (%3d, %3d) to (%6.2f, %6.2f)\n",
		row, col, pos[1], pos[0]);
	printf("pan = %f, p = (%f %f %f)\n",
	       pan, p[0], p[1], p[2]);
	       }*/
      if (pos[0] < 0 || pos[0] > (ncols - 1) ||
	  pos[1] < 0 || pos[1] > (nrows - 1)) 
	*d = 0;
      else
	*d = *(src + (int) (pos[1] + 0.5) * srcRB
	       + (int) (pos[0] + 0.5));
    }
  }

  return NO_ERR;
}

int
JPLCamera::RectifyFisheyePair (JPLCamera *leftCam, JPLCamera *rightCam,
			       JPLCamera *leftRectCam, JPLCamera *rightRectCam,
			       JPLPic *leftPic, JPLPic *rightPic,
			       JPLPic *leftRectPic, JPLPic *rightRectPic)
{
  int dims[2];
  double nA[3], nH[3], nV[3], ncenter[2], nscale[2], ntheta;
 
  if (leftCam == NULL || rightCam == NULL ||
      leftPic == NULL || rightPic == NULL) {
    FatalErr ("Init NULL\n");
    return INIT_ERR;
  }

  dims[0] = leftPic->cols;
  dims[1] = leftPic->rows;
  cmod_cahvor_warp_models (leftCam->C, leftCam->A,
				 leftCam->H, leftCam->V, leftCam->O,
				 leftCam->R, rightCam->C, rightCam->A,
				 rightCam->H, rightCam->V, rightCam->O,
				 rightCam->R, true, dims, dims, nA, nH, nV,
				 &nscale[0], &ncenter[0], &nscale[1],
				 &ncenter[1], &ntheta);

  leftRectCam->InitJPLCamera (CAHV_MODEL, leftCam->C, nA, nH, nV, NULL,
			  NULL, ncenter, nscale, &ntheta, NULL, NULL);
  rightRectCam->InitJPLCamera (CAHV_MODEL, rightCam->C, nA, nH, nV, NULL,
			   NULL, ncenter, nscale, &ntheta, NULL, NULL);


  RectifyFisheye (leftCam, leftRectCam, leftPic, leftRectPic);
  RectifyFisheye (rightCam, rightRectCam, rightPic, rightRectPic);

  return NO_ERR;
}
#endif
