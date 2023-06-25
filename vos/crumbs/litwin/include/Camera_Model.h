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
*			John Wright     john.r.wright@jpl.nasa.gov            *
*								  	      *
*                                       Updated: 12 Dec 2002                  *
*                                                                             *
*                                       Copyright (C) 1993, 1994, 1995, 1996, *
*                                                     1997, 1998, 2002        *
*                                       California Institute of Technology    *
*                                       All Rights Reserved                   *
*                                                                             *
******************************************************************************/

#ifndef __CAMERA_MODEL_H__
#define __CAMERA_MODEL_H__

#include <stdio.h>
#include <stdlib.h>

#ifndef TRUE
#define TRUE 1
#endif
#ifndef FALSE
#define FALSE 0
#endif

#define	NO_ERR		0
#define	PARAM_ERR	1

/// Base class for various types of camera models
/**
The CameraModel class implements a base class for various
types of camera models.  Camera models, in genral, are used
to map points in the world or vectors to and from camera
image space.  Various methods may be used for this purpose.
*/
class CameraModel {

    protected:

	/// Prints error messages to stderr
	/**
	The FatalErr method prints an error message identifying the type
	of camera model and echoing the string passed to it.  It does not
	terminate or do any additional cleanup at this time.
	*/
	void		FatalErr(char *);

    public:

	/// Optional pointer to user defined stuff
	void 		*params;

	/// Constructor
	CameraModel ();

	/// Destructor
	virtual ~CameraModel ();
	
#ifdef FULL_MODEL_IMPLEMENTATION
	// I/O
	/// Virtual Camera Model Read method
	virtual long		ReadCameraModel (FILE *fp, long type);
	/// Virtual Camera Model Write method
	virtual long		WriteCameraModel (FILE *fp);
	/// Virtual Camera Model Print method
	virtual void		PrintCameraModel ();
#endif

	/// Virtual Camera Model Name method
	virtual const char	*getCameraModelName();
		
	// projection functions
	/// Project a point in the world to the image plane
	/**
	The Point3DToImage2D method projects a point in world space to
	the camera image plane.  The 3D point, (x,y,z), is projected to
	the 2D coordinate, (row,column). Returns NO_ERR if no errors.
	*/
	virtual long	Point3DToImage2D (double x, double y, double z,
				  double *row, double *col);

	/// Project a point in the image plane to the world
	/**
	The Image2DToRay3D method projects a point on the screen in 2D
	coordinates, (row,column), to a vector in 3-space (ray). Note
	that row=Y and column=X so the argument order is (y,x).
	Returns NO_ERR if no errors.
	*/
	virtual long	Image2DToRay3D (double row, double col,
				double ray[3]);

	/// Project a point in the image plane to the world
	/**
	The Image2DToPoint3D method projects a point on the screen in 2D
	coordinates, (row,column), to a vector in 3-space (ray), then computes
	the (x,y,z) coordinates of the point at range distance from the camera. 
	Note that row=Y and column=X so the argument order is (y,x).
	Returns NO_ERR if no errors.
	*/
	virtual long	Image2DToPoint3D (double row, double col, double range,
				double xyz[3]);

	/// Project a point in the image plane to polar coordinates
	/**
	The Image2DToAzEl method converts a point in image coordinates,
	(row,col), to polar coordinates azimuth and elevation. Note
	that row=Y and column=X so the argument order is (y,x).
	Returns NO_ERR if no errors.
	*/
	virtual long	Image2DToAzEl(double row, double col, double *az, double *el);

	/// Project a point in the world to polar coordinates
	/**
	The Point3DToAzElRng method converts a point in world coordinates,
	(x,y,z), to polar coordinates azimuth, elevation, and range. 
	Returns NO_ERR if no errors.
	*/
	virtual long	Point3DToAzElRng(double x, double y, double z,
				double *az, double *el, double *range);
	
	// utilities
	// Get camera position methods
	/// Get X coordinate of camera
	virtual double getCameraX() = 0;
	/// Get Y coordinate of camera
	virtual double getCameraY() = 0;
	/// Get Z coordinate of camera
	virtual double getCameraZ() = 0;
};

#define CAHV_MODEL		1
#define CAHVOR_MODEL		2
#define CAHVORE_MODEL		3

#define PERSPECTIVE_PUPIL		1
#define FISHEYE_PUPIL			2
#define GENERAL_PUPIL			3

/// JPLCamera implements a CAHV(OR) based camera model
/**
The JPLCamera class implements a CAHV(OR) based camera model.
It may be used for either CAHV or CAHVOR without explicit
specification of the type desired.  If the O and R terms are
not set, then a CAHV model is implemented.  If either the O
or R terms are specified at any time, the model then implements
a CAHVOR model.  The model type may also be explicitly set.
*/
class JPLCamera : public CameraModel {

    public:
	JPLCamera ();
	~JPLCamera ();
	
    protected:

	// parameters
	long		isValid;
	long		modelType;	// CAHV or CAHVOR model
	const char	*getCameraModelName();
	double 		*C, *A, *H, *V, *O, *R, *E;
	double		linearity;
	int		pupiltype;

#ifdef FULL_MODEL_IMPLEMENTATION
	double		*center, *scale;	// horizontal/vertical center position and scale.
	double		*theta;
	double		*s; // 18x18 covariance matrix
	double 		*s_int; // 5x5 covariance matrix
	void		*rectificationTable;	
	long		rows, cols;		// image dimension for the rectification table
#endif

    public:

#ifdef FULL_MODEL_IMPLEMENTATION
	long		ReadCameraModel (char *filename);
	long		ReadCameraModel (char *filename, long type);
	long		ReadCameraModel (FILE *fp, long type);
	long		WriteCameraModel (char *filename);
	long		WriteCameraModel (FILE *fp);
	void		PrintCameraModel ();
#endif
	void		CopyCamera (JPLCamera *dstCam);
		
	// projection functions
	/// Project a point in the world to the image plane
	/**
	The Point3DToImage2D method projects a point in world space to
	the camera image plane.  The 3D coordinate, P, is projected to
	the 2D coordinate, ip, and the range from the camera to the
	3D position is also calculated and returned.  If the par argument
	is nonNULL, it is assumed to reference a 2D array (par[2][3]) in
	which is returned a set of partial derivatives from the underlying
	library. Returns NO_ERR if no errors.
	*/
	virtual long	Point3DToImage2D (double P[3], double ip[2], double *range,
				double **par=NULL);

	/// Project a point in the image plane to the world
	/**
	The Image2DToRay3D method projects a point on the screen in 2D
	coordinates, ip, to a vector in 3-space (uvec).  The projection
	point, the camera's location, is returned in P.  If the par argument
	is nonNULL, it is assumed to reference a 2D array (par[2][3]) in
	which is returned a set of partial derivatives from the underlying
	library. Returns NO_ERR if no errors.
	*/
	virtual long	Image2DToRay3D(double ip[2], double P[3], double uvec[3], 
					double **par=NULL);

	/// Project a point in the world to the image plane
	/**
	The Point3DToImage2D method projects a point in world space to
	the camera image plane.  The 3D point, (x,y,z), is projected to
	the 2D coordinate, (row,column). Returns NO_ERR if no errors.
	*/
	long		Point3DToImage2D (double x, double y, double z,
					  double *row, double *col);
	/// Project a point in the image plane to the world
	/**
	The Image2DToRay3D method projects a point on the screen in 2D
	coordinates, (row,column), to a vector in 3-space (ray). Note
	that row=Y and column=X so the argument order is (y,x).
	Returns NO_ERR if no errors.
	*/
	long		Image2DToRay3D (double row, double col,
					double ray[3]);

	/// Get camera model internals
	/**
	The GetInternals method returns the internal model parameters
	computed from the CAHV model parameters.  The internals are
	hs (horizontal scale factor), hc (horizontal center),
	vs (vertical scale factor), vc (vertical center), and
	theta (angle between axes), all doubles.
	*/
	void GetInternals(double *hs, double *hc, double *vs, double *vc,
		double *theta=NULL);


#ifdef FULL_MODEL_IMPLEMENTATION
	// utilities
	long		TranslateModel(double T[3]);
	long		RotateModel(double P[3], double A[3], double rads);
	long		ResizeImageCoordinate (float scale);
	long		ResizeImageCoordinate (float scale, JPLCamera *newCam);
	long		TransformImageCoordinate (float scaleX, float scaleY, 
						float transX, float transY);
	void		M2D_3D (float M[9]);
	void		M3D_2D (float M[9]);
	void		InitJPLCamera (long type, double *inC, double *inA,
					double *inH, double *inV, double *inO, double *inR,
					double *incenter, double *inscale, double *intheta,
					double *ins, double *ins_int);
#endif
	void		InitJPLCamera (double *inC, double *inA,
				double *inH, double *inV, 
				double *inO=NULL, double *inR=NULL,
				double *inE=NULL, double inlin=1.0);
	void		setmodelType(long inmodelType);
	long		getmodelType();
	/// Get X coordinate of camera
	double 		getCameraX();
	/// Get Y coordinate of camera
	double 		getCameraY();
	/// Get Z coordinate of camera
	double 		getCameraZ();
	void		setC (double *inC);
	void		setA (double *inA);
	void		setH (double *inH);
	void		setV (double *inV);
	void		setO (double *inO);
	void		setR (double *inR);
	void		setE (double *inE);
	void		setlinearity(double inlin);
	void		setpupiltype(int intype);
	void		setC (double term1, double term2, double term3);
	void		setA (double term1, double term2, double term3);
	void		setH (double term1, double term2, double term3);
	void		setV (double term1, double term2, double term3);
	void		setO (double term1, double term2, double term3);
	void		setR (double term1, double term2, double term3);
	void		setE (double term1, double term2, double term3);
	void		getC (double *inC);
	void		getA (double *inA);
	void		getH (double *inH);
	void		getV (double *inV);
	void		getO (double *inO);
	void		getR (double *inR);
	void		getE (double *inE);
	double		getlinearity();
	int		getpupiltype();
	void		getC (double *term1, double *term2, double *term3);
	void		getA (double *term1, double *term2, double *term3);
	void		getH (double *term1, double *term2, double *term3);
	void		getV (double *term1, double *term2, double *term3);
	void		getO (double *term1, double *term2, double *term3);
	void		getR (double *term1, double *term2, double *term3);
	void		getE (double *term1, double *term2, double *term3);

#ifdef FULL_MODEL_IMPLEMENTATION
	void		setcenter (double *incenter);
	void		setscale (double *inscale);
	void		sets (double *ins);
	void		sets_int (double *ins_int);
	void		setcenter (double xcenter, double ycenter);
	void		setscale (double xscale, double yscale);
	void		settheta (double intheta);
	void		getcenter (double *incenter);
	void		getscale (double *inscale);
	void		getcenter (double *xcenter, double *ycenter);
	void		getscale (double *xscale, double *yscale);
	double		gettheta();
					
	// rectification based on lookup tables.
	long 		GenerateRectificationTable (long dstRows, long dstCols, JPLCamera *dstCam);

/* Pic based stuff that we don't use and don't want for now
	// lut-based rectification
	long		RectifyImage (JPLPic *srcPic, JPLPic *dstPic);
	// non-lut-based rectification
	long 		RectifyImage (JPLPic *srcPic, JPLPic *dstPic, JPLCamera *dstCamera,
				      unsigned char numIntervals);
*/
	
	long            TransformStereoCameras (
				JPLCamera *rightCam,
				JPLCamera *leftResult, 
				JPLCamera *rightResult,
				float leftNodalPos[3],
				float leftToRightDirection[3],
				float leftPointingDirection[3]);
#endif

	/// Performs a unit test on the class
	/**
	self_test performs an encapsulated unit test on the
	class, returning TRUE if successful and FALSE if not.
	*/
	static int	self_test(void);
};

#endif
