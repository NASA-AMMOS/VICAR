// glrend.C
// oglrend.C 1.29 01/07/27 12:49:47

#include <signal.h>
#include <string.h>
#include "grape/oglrend.h"
#include "grape/poly_object_1.h"
#include "grape/camera.h"
#include "grape/light.h"
#include <GL/gl.h>
#include <GL/glu.h>
#include <X11/keysym.h>
#include "math.h"

#ifdef GET_TIMINGS
#include <sys/timers.h>
#include "special/timer.h"
#endif

// The OpenGL renderer class.
//

#define RGB_WHITE 0x00ffffff
#define RGB_RED   0x0000ff
#define RGB_GREEN 0x00ff00
#define RGB_BLUE  0xff0000
#define RGB_BLACK 0x000000

#define A_OFST 3
#define B_OFST 2
#define G_OFST 1
#define R_OFST 0

#define A_OFST_SWAP 0
#define B_OFST_SWAP 1
#define G_OFST_SWAP 2
#define R_OFST_SWAP 3

#define TORADIANS  0.017453293 /* PI/180 to convert to radians */
#define TODEGREES  57.29578    /* 180/PI to convert to degrees */

#define MAX_X_PCTAGE  0.25		// percentage of screen that is used when
								// rendering needs to be done in pieces.
#define MAX_Y_PCTAGE  0.25

#define ANG_ZERO(a) (fabs(a) < 1e-6 ? 1 : 0)

static int bDoNormals = 0;		// should we be dealing with normals?
#define MAX_DOUBLE 10000000.0
static double xmax=-MAX_DOUBLE,ymax=-MAX_DOUBLE,zmax=-MAX_DOUBLE;
static double xmin=MAX_DOUBLE,ymin=MAX_DOUBLE,zmin=MAX_DOUBLE;
static double totmin, totmax;

static int nDispListBase = 0;
static int num_polygon_objects = 0;
static GLXContext first_context = (GLXContext)NULL;

GLenum get_light_number( int nCurLightIndex);
void compute_light_params( ObjNode *pObjNode);

XVisualInfo *
get_visualinfo( Display *pXD, 
			   Window Xwin);

void
get_frustum( double fovy, double fovx, double dist_to_plane,
			int wx, int wy, int subwin_x, int subwin_y,
			double &fr_top, double &fr_bottom, 
			double &fr_left, double &fr_right,
			int ndxImage, int nSubWinX, int nSubWinY);

void
print_matrix( double *p);

void print_proj_matrix(void);
void print_modelview_matrix(void);
	
/****************************************************************************
*
* Function name: process_object
*
* Function purpose: To take an object in the scene and convert it to GL.
*
* Function History: 7/17/95  Created.
*
* Parameters: Scene *s : Input to function. A scene pointer.
*
* Return value: int
*
* Global variables/defines used: None.
*
* Module variables/defines used: None.
*
* Function method (description):  
*
* Created by: Zareh Gorjian
*
* Bugs, possible improvements: 
*
* This function IS NOT machine specific.
*
*****************************************************************************/
int OpenGLrenderer::process_object(int k)
{
	ObjNode *tmpObjectNode;
	PolyObject1     *tmpObject;

	tmpObjectNode = scene->get_obj(k);           /* get object node pointer */

								/* get object pointer */
	tmpObject = (PolyObject1 *)(tmpObjectNode->get_object()); 
	if(tmpObject->get_type() == POLY_V1) {
		process_poly_object( tmpObject, k);
	} else if (tmpObject->get_type() == POLY_TERRAIN_V1) {
		process_poly_object( tmpObject, k);

	} /* if polygon object */
	else if (tmpObject->get_type() == DIRECTIONAL_LIGHT_V1) {


		process_light_object( tmpObjectNode); // set up the light

	}
		

	return(0); // means okay

} /* process_scene */

/****************************************************************************
*
* Function name: set_scene
*
* Function purpose: To take a scene pointer and get the objects in the scene
*                   and convert them to GL objects.
*
* Function History: 7/17/95  Created.
*
* Parameters: Scene *s : Input to function. A scene pointer.
*
* Return value: int
*
* Global variables/defines used: None.
*
* Module variables/defines used: None.
*
* Function method (description):  
*
* Created by: Zareh Gorjian
*
* Bugs, possible improvements: 
*
* This function IS NOT machine specific.
*
*****************************************************************************/
int OpenGLrenderer::set_scene(Scene *s, ImgInfo *inimg)
{
    int num_objects;
    ObjNode *tmpObjectNode;
    int     k;
    ImgInfo	*tmpImgInfo;
    int windowx, windowy, bands;


								// use the passed image info if specified
    if(inimg) {
        tmpImgInfo = inimg;
    } else {					// use image info associated with the scene
        tmpObjectNode = s->get_camera();
        tmpImgInfo = ((Camera *)(tmpObjectNode->get_object()))->get_img_info();
    }

    tmpImgInfo->prepare();		// ready the image-related stuff

								// obtain image's resolution
    tmpImgInfo->get_res(&windowx, &windowy, &bands);

#if 0
	int minmax[2];
	glGetIntegerv( GL_MAX_VIEWPORT_DIMS, minmax);
	if (windowx > minmax[0] ||
		windowy > minmax[1]) {
		fprintf( stderr, "Unable to render images larger than %dw x %d h\n",
				minmax[0], minmax[1]);
		return 1;				// error
	}
#endif

	glEnable( GL_DEPTH_TEST);	// we want depth testing to occur
	glDepthRange( 0.0, 1.0);	// and we want it to be full-range

								// setup the OGL clear color and
								// depth-clearing values
	glClearColor( 0,0,0,0);		// black
	glClearDepth( 1.0);			// aka. infinity

	glViewport( 0, windowx-1, 0, windowy-1); // set the viewport

    this->wx = windowx;
	this->wy = windowy; // set obj's members to be same res

	this->bUseAlpha = tmpImgInfo->get_alpha_flag();
	this->bUseDepth = tmpImgInfo->get_z_flag();

    if(tmpImgInfo->get_output_method() != OUT_XWIN) {
                                // not outputting to X win: output to pixmap
#if 1
		if (init_render_to_pixmap( ))
			return 1;			// error occurred!

        if (bTestWin == GL_TRUE) // showing pixmap contents in X win, too?
            create_test_xwin( windowx, windowy, pXdisplay, pVis);
#else
		if (init_render_to_xwin())
			return 1;			// error
#endif

    }
	else {						// rendering into X window: setup OGL

		subwin_x = wx;			// there is 1 subwindow, of full size
		subwin_y = wy;

		if (init_render_to_xwin( tmpImgInfo->get_xdisplay(),
								tmpImgInfo->get_xwindow()))
			return 1;			// error occurred!
	}

    this->scene = s;                  // obj's member 'scene'

    num_objects = s->get_num_objects(); /* get the number of objects */

								// make an array of display list id's
    if(My_OGL_Objects)free(My_OGL_Objects);
    if ((My_OGL_Objects = (OGL_Object *)malloc(num_objects * sizeof(OGL_Object)))
		== NULL) {
		fprintf( stderr, "Unable to malloc My_OGL_Objects!\n");
		exit( 0);
	}

								// make array of TextureType, and set each
								// of them to 0
	if (pTextures) free(pTextures);
	if ((pTextures = (TextureType *)malloc( num_objects * sizeof(TextureType)))
		== NULL) {
		fprintf( stderr, "Unable to malloc pTextures!\n");
		exit( 0);
	}
	memset( pTextures, 0, num_objects * sizeof(TextureType) );

	this->nObjectsInScene = num_objects; // save as the size of the above 2
										 // arrays

	xmax=ymax=zmax=-MAX_DOUBLE;	// initialize before processing objects
	xmin=ymin=zmin=MAX_DOUBLE;

	num_polygon_objects = 0;
    for(k=0; k<num_objects; k++) { /* loop and create a GL object for each */
        process_object(k);         /* optional as render will do it if */
								   /* necessary */
    }

	nDispListBase += num_polygon_objects;

								// get max and min coordinate values
	totmax = xmax;
	if (ymax > totmax)
		totmax = ymax;
	if (zmax > totmax)
		totmax = zmax;

	totmin = xmin;
	if (ymin < totmin)
		totmin = ymin;
	if (zmin < totmin)
		totmin = zmin;

	if (nLights > 0) {			// is lighting on ?

								// this is debug lighting! must change djf!!!

		fprintf( stderr,
   "lighting is on, using default material specular & shininess values\n");
 
								// in the future these will come from files
		glMaterialfv( GL_FRONT, GL_SPECULAR, mat_specular);
		glMaterialfv( GL_FRONT, GL_SHININESS, mat_shininess);
	}

    last_rendered = CHANGE_COUNTER;

    if(parray)free(parray);		// make room for the output image data

    if ((parray = (unsigned long *)malloc(subwin_x * subwin_y * sizeof(long)))
		== NULL) {
		fprintf( stderr, "Unable to malloc parray!\n");
		exit( 0);
	}

	glReadBuffer( GL_BACK);		// the default: just make it obvious

								// setup the near and far clip planes
	dNearClipPlane = 1.0;
	int ir[2];
	glGetIntegerv( GL_DEPTH_RANGE, ir);	// max z depth
	dFarClipPlane = (double)ir[1];
	
    return(0);                  // means okay
    
}                               /* set_scene */



/****************************************************************************
 *
 * Function name: render
 *
 * Function purpose: To render the current frame.
 *
 * Function History: 7/17/95  Created.
 *
 * Parameters: double time: input to function. An optional parameter which 
 *                          specifies the time instance at which the scene is
 *                          to be rendered.
 *
 * Return value: int
 *
 * Global variables/defines used: None.
 *
 * Module variables/defines used: None.
 *
 * Function method (description):  
 *
 * Created by: Zareh Gorjian
 *
 * Bugs, possible improvements: 
 *
 * This function IS NOT machine specific.
 *
 *****************************************************************************/
int OpenGLrenderer::render(ObjNode *incam, ImgInfo *inimg)
{
    double fov;
    double	d, widthfov;
    int windowx=200,windowy=200, bands;
    int     num_objects;
    int     k;
    Obj     *tmpObject;
    ObjNode *tmpObjectNode;
    static int count = 0;
	int status;
    double x1,y1,z1,x2,y2,z2,ux,uy,uz;
	double fr_top, fr_bottom, fr_left, fr_right;
#ifdef GET_TIMINGS
	char s0[64];
	Timer ThisRoutine;
#endif

#ifdef GET_TIMINGS
	ThisRoutine.reset();
	glFinish();					// block until pending GL calls are done
	ThisRoutine.start();
#endif

    if (!scene) {
		fprintf( stderr,
				"Whoops - Do not call render before calling set_scene.\n");
		return FALSE;
	}

	inimg->prepare();
								// select _this_ renderer's context again OGL 
	if (bPixMap == GL_TRUE)
		status = glXMakeCurrent( pXdisplay, GLXPixMap, glx_context);
	else
		status = glXMakeCurrent( pXdisplay, win, glx_context);
	if (status != True) {
		fprintf( stderr, "Unable to switch rendering contexts\n");
	}
	
	glViewport( 0, 0, subwin_x, subwin_y);
								// must _always_ reselect the viewport, since
								// OGL seems to require it; if one does not,
								// then the viewport of the 1st rendering
								// context is all screwed up (ex: initially
								// at (0,0) w=400,h=300; after creating and
								// selecting 2nd context, was (0,399) (0,299),
								// which is wrong.



								// setup the field of view

    fov = ((Camera *)(incam->get_object()))->get_fov();

    
    inimg->get_res(&windowx, &windowy, &bands);	// output window size,
												// occording to image_info file

								// scale to multiple of subwin size

    d = (windowy / 2.0) / (tan((fov*TORADIANS)/2.0));
    widthfov = atan((windowx/2.0) / d) * 2.0;
    widthfov *= TODEGREES;
    // double aspect = widthfov / (fov / 2.0);
	

#if 0
    if (inimg->get_output_method() == OUT_FILL_IMG) {
        if (count % 2 == 0) {
            glDrawBuffer( GL_BACK);
            glReadBuffer( GL_BACK);
        }
        else {
            glDrawBuffer( GL_FRONT);
            glReadBuffer( GL_FRONT);
        }
    }
#else
	if (bPixMap == GL_TRUE) {

								// this is wierd: if doublebuffering pixmaps
								// then always render into and read out of the
								// front buffer (how do I get it to render into
								// the back buffer (pixmap)???

		glDrawBuffer( GL_FRONT); // else use default: the back
		glReadBuffer( GL_FRONT);
	}
#endif
    
    glClear( GL_COLOR_BUFFER_BIT| // clear color and zbuffer
            GL_DEPTH_BUFFER_BIT);
    glEnable( GL_DEPTH_TEST);	// turn it on
    glDepthRange( 0.0, 1.0);	// define z range
    
    
    num_objects = scene->get_num_objects(); /* get the number of objects */

	int nFrame;
#ifdef GET_TIMINGS
	Timer tTotal, tSubFrame, tRendAndXfer, tRendAndXferAndWrite;
	Timer p0, p1, p2, p3, p4;


	tTotal.reset();
	glFinish();					// block until pending GL calls are done
	tRendAndXfer.start();
	tRendAndXferAndWrite.start();
#endif
	for (nFrame = 0; nFrame < nSubFrame; nFrame++) {


#ifdef GET_TIMINGS
		p0.reset();p1.reset();p2.reset();p3.reset();
		p0.start();

		glFinish();					// block until pending GL calls are done
		tSubFrame.reset();
		tSubFrame.start();
		tTotal.start();
#endif

		if (nSubFrame > 1)
			fprintf( stderr,
					"  %02d/%02d t = ", nFrame+1, nSubFrame);

		glClear( GL_COLOR_BUFFER_BIT| // clear color and zbuffer
				GL_DEPTH_BUFFER_BIT);
		glEnable( GL_DEPTH_TEST);	// turn it on
		glDepthRange( 0.0, 1.0);	// define z range

#ifdef GET_TIMINGS
		glFinish();					// block until pending GL calls are done
		p0.stop();
		p1.start();
#endif
								// setup the viewing transformation
//		setup_camera( incam, fov, widthfov, near, far, nFrame);
		glMatrixMode( GL_PROJECTION); // specify projection matrix
		glLoadIdentity();			// initialize
    
#ifdef GET_TIMINGS
		glFinish();					// block until pending GL calls are done
		p1.stop();
		p2.start();
#endif

		get_frustum( fov, widthfov, dNearClipPlane, 
					wx, wy, subwin_x, subwin_y,
					fr_top, fr_bottom, fr_left, fr_right,
					nFrame, nSubFramesX, nSubFramesY);

		glFrustum( fr_left, fr_right, fr_bottom, fr_top, 
				  dNearClipPlane, dFarClipPlane);

		glMatrixMode( GL_MODELVIEW);
		glLoadIdentity();

		incam->get_object_pos( x1, y1, z1);

								// get camera's look vector
//		incam->get_look_vector(x2, y2, z2);
		incam->get_lookat_ref_point(x2, y2, z2);
    
								// get the up vector
//		incam->get_up_vector( ux, uy, uz);
//		ux -= x1; uy -= y1; uz -= z1;
		incam->get_up_vector( ux, uy, uz);

								// get viewing transformation
		gluLookAt(x1,y1,z1,x2,y2,z2,ux,uy,uz);

#ifdef GET_TIMINGS
		glFinish();					// block until pending GL calls are done
		p2.stop();
		p3.start();
#endif

								// zip thru each object and render it
		for(k=0; k<num_objects; k++) { 
								/* loop and display a GL object for each */

			tmpObjectNode = scene->get_obj(k); /* get object node pointer */
        
			tmpObject = tmpObjectNode->get_object(); /* get object pointer */

								// render polygon objects, define light
								// objects
		    if (((PolyObject1*)tmpObject)->get_hidden() == True) continue;
			if (tmpObject->get_type() == POLY_V1
				||
				tmpObject->get_type() == POLY_TERRAIN_V1) {

								// polygonal object

				if (tmpObject->get_changed() > last_rendered) {
					fprintf(stderr,"Reprocessing polygonal object %d\n", k);

								// account for any enveloped values
					process_object(k);
				}
				
				// object hasn't changed
                
				glPushMatrix();
				apply_object_transform( tmpObjectNode);
				
				apply_centroid_transform( tmpObject);

#if 0
								// do testure mapping, if specified
				if (pTextures[k].pTexture != NULL) {
					get_ready_to_texture_map(
						pTextures[k].w, pTextures[k].h, pTextures[k].pTexture);
				}
				else {
					glDisable(GL_TEXTURE_2D); // disable texture mapping
				}
#endif

#ifdef GET_TIMINGS
				glFinish();		// block until pending GL calls are done
				p4.start();
#endif

				glCallList(My_OGL_Objects[k]); // draw the obj

#ifdef GET_TIMINGS
				glFinish();
				p4.stop();
#endif

				glPopMatrix();                 
			}
		    else if (tmpObject->get_type() == DIRECTIONAL_LIGHT_V1) {

								// object is a light
				glPushMatrix();
				glLoadIdentity(); // light specified in world cooords
			
				if (tmpObject->get_changed() > last_rendered) { 
				
					fprintf(stderr,
							"Reprocessing directional light object %d\n", k);

								// account for any enveloped values
					process_object( k);
				}
				
				compute_light_params( tmpObjectNode);

				glPopMatrix();
				
			}
		}				// for k objects
#ifdef GET_TIMINGS
		p3.stop();
#endif


								// save any sub-images into the output image

		if (bTestWin == GL_TRUE) {
			glXWaitGL();		// wait for GL ops to finish before doing X ops
		}
		
//		glXWaitGL();
//		glXSwapBuffers( pXdisplay, win);
//		glReadBuffer( GL_FRONT);

		// do something with the rendering

//		glXWaitGL();
#ifdef GET_TIMINGS
		glFinish();					// block until pending GL calls are done
		tSubFrame.stop();
		tTotal.stop();

		if (nSubFrame > 1)
			fprintf( stderr, 
					"%s\n", tSubFrame.elapsed_as_string( s0));
#endif

		
		if(inimg->get_output_method() == OUT_FILL_IMG ||
		   inimg->get_output_method() == OUT_FILE) {
			
			// fill the image associated with the
			// image-info obj
			render_context_to_image( inimg, 
									dNearClipPlane, dFarClipPlane, 4, nFrame);
			
		} else if(inimg->get_output_method() == OUT_XWIN) {
			
			// do nothing: should already have rendered
			// into the X win
			
			
		} else if(inimg->get_output_method() == OUT_GLWIN) {
			fprintf( stderr,
					"Can not render to a GL window using the OpenGL renderer\n");
			// probably do nothing
		} else if(inimg->get_output_method() == OUT_NONE) {
			// definitely do nothing
		}


		glXSwapBuffers( pXdisplay, win);
		
	}

#ifdef GET_TIMINGS
	if (nSubFrame > 1)
		fprintf( stderr, 
				"Total render time = %s\n", 
				tTotal.elapsed_as_string( s0));


	glFinish();					// block until pending GL calls are done
	tRendAndXfer.stop();
	
	if (nSubFrame > 1)
		fprintf( stderr, 
				"Total render and transfer to Image time = %s\n", 
				tRendAndXfer.elapsed_as_string( s0));
#endif

	count++;
	
	inimg->dispose( CHANGE_COUNTER-1); // output to file, if required

#ifdef GET_TIMINGS
	glFinish();					// block until pending GL calls are done
	tRendAndXferAndWrite.stop();
	
	if (nSubFrame > 1)
		fprintf( stderr, 
				"Total render, transfer to Image, and write time = %s\n", 
				tRendAndXferAndWrite.elapsed_as_string( s0));
#endif
	
	last_rendered = CHANGE_COUNTER;

#ifdef GET_TIMINGS
	glFinish();					// block until pending GL calls are done
	ThisRoutine.stop();
#endif

	return(0);			// means okay
	
}						/* render */





/****************************************************************************
 *
 * Function name: WFUpToRoll
 *
 * Function purpose: To take the camera position, gaze location and up
 *                   vector and return a roll angle.
 *
 * Function History: 11/23/94  Created.
 *
 * Parameters: ex,ey,ez (double) : inputs to function. Specify the eye 
 *                                 position.
 *             gx,gy,gz (double) : inputs to function. Specify the gase
 *                                 point or target point.
 *             ux,uy,uz (double) : inputs to function. The up vector.
 *
 * Return value: roll angle.
 *
 * Global variables/defines used: None.
 *
 * Module variables/defines used: None.
 *
 * Function method (description):  WFUpToRoll will take the eye, gaze points 
 *                                 and an up vector and determine a roll 
 *                                 angle for passing to the lookat() 
 *                                 function. The numbers here are the 
 *                                 "render" style coordinate system which 
 *                                 is left handed with z up.
 * Created by: Zareh Gorjian
 *
 * Bugs, possible improvements: 
 *
 * This function IS NOT machine specific.
 *
 *****************************************************************************/
Angle OpenGLrenderer::WFUpToRoll( double ex, double ey, double ez,
								 double gx, double gy, double gz,
								 double ux, double uy, double uz)
    /* double ex,ey,ez;		 eye point              */
    /* double gx,gy,gz;		 gaze point             */
    /* double ux,uy,uz;		 up vector in viewplane */
{
	double roll;
	double lx, ly;	/* look vector (eye to gaze point) */
	double vx, vy; /* horizontal vector in view plane */
	double uMag, vMag; /* magnitude of u and v vectors	   */
	double dot;	/* dot product                     */
    
	lx = gx - ex;		/* look = gaze - eye */
	ly = gy - ey;
	// lz = gz - ez;
    
	vx = ly;			/* v = look x vertical [0 0 1] */
	vy = -lx;
	// vz = 0.0;
    
	uMag = sqrt(ux*ux + uy*uy + uz*uz); /* magnitude of u */
	vMag = sqrt(vx*vx + vy*vy);	/* magnitude of v */
	dot = vx*ux + vy*uy;
    
	roll = -acos(dot/(vMag * uMag)) + M_PI_2;
	if (uz < 0.0) 
        roll = M_PI - roll;
    
	/* 
	 ** GL wants the angle in 10ths of degrees, so convert it. 
	 */
	roll = (roll * 180.0 / M_PI) * 10 + 0.5;
    
	return ((Angle)roll);
    
}						/* WFUpToRoll */

void OpenGLrenderer::WFRollToUp( double roll, double ex, double ey, double ez,
								double gx, double gy, double gz,
								double *ux, double *uy, double *uz)
    /* double ex,ey,ez;		 eye point              */
    /* double gx,gy,gz;		 gaze point             */
    /* double ux,uy,uz;		 up vector in viewplane */
{
	double lx,ly,lz;	/* look vector (eye to gaze point) */
	double vx, vy, vz; /* horizontal vector in view plane */
	double rx, ry, rz; /* right vector in world xz plane */
	double bx, by, bz; /* right vector in world xz plane */
	double wux, wuy, wuz;
	double d;
	ZMatrix m, invs, a, c;
    
	*ux = 0.0; *uy = 1.0; *uz = 0.0; // default up to the y axis
	roll = (roll * M_PI)  / 180.0; // convet to rads
    
	lx = gx - ex;		/* look = gaze - eye */
	ly = gy - ey;
	lz = gz - ez;
    
	// normalize the look vector
	d = sqrt( lx*lx + ly*ly + lz*lz);
	if (d == 0.0) {
		fprintf( stderr,
				"gaze point equals eye point: can't find 'up' vector!\n");
		return;
	}
	lx /= d; ly /=d; lz /=d;
    
	// get the orthogonal 'right' vector
	wux = 0.0; wuy = 0.0; wuz = 1.0;
	rx = ly * wuz - lz * wuy;
	ry = lz * wux - lx * wuz;
	rz = lx * wuy - ly * wux;
	d = sqrt( rx*rx + ry*ry + rz*rz);
	if (ANG_ZERO(d)) {	// look vector parallel to z axis; use y axis
		wux = 0.0; wuy = 1.0; wuz = 0.0;
		rx = ly * wuz - lz * wuy;
		ry = lz * wux - lx * wuz;
		rz = lx * wuy - ly * wux;
		d = sqrt( rx*rx + ry*ry + rz*rz);
	}
	rx /= d; ry /= d; rz /= d;
    
	// get the orthoganal 'up' vector (unrolled)
	vx = ry * lz - rz * ly;
	vy = rz * lx - rx * lz;
	vz = rx * ly - ry * lx;
	d = sqrt( vx*vx + vy*vy + vz*vz);
	if (d == 0.0) {
		fprintf( stderr, "vertical vector has 0 length\n");
		return;
	}
	vx /= d; vy /= d; vz /=d;
    
	// get R, the transformation matrix
	// to rotate into the x,y,z axes
	MatIdent( m);
	m[2][0] = lx;
	m[2][1] = ly;
	m[2][2] = lz;
	m[0][0] = rx;
	m[0][1] = ry;
	m[0][2] = rz;
	m[1][0] = vx;
	m[1][1] = vy;
	m[1][2] = vz;
	if (!MatInvert( m, invs)) {
		fprintf( stderr, "error inverting matrix\n");
		return;
	}
    
	MatIdent( a);
	a[0][0] = vx; a[0][1] = vy; a[0][2] = vz;
	MatMult( a, m);		// make all vectors incident with x,y,z axes
	bx = a[0][0]; by = a[0][1]; bz = a[0][2];
    
	MatIdent( c);
	c[0][0] = bx * cos(roll) - by * sin(roll);
	c[0][1] = bx * sin(roll) + by * cos(roll);
	c[0][2] = bz;
    
	MatMult( c, invs);	// make all vectors incident with x,y,z axes
	*ux = c[0][0];
	*uy = c[0][1];
	*uz = c[0][2];
    
}						/* WFUpToRoll */


#ifdef FUNCT_HDR

/* ************************************************************************* 
   
   Function Name:           pixmap_to_image
   
   Purpose:                 move data from an OpenGL pixmap to an Image.
   
   Lacks support for Z buffer.
   
   Sample Call:             pixmap_to_image( img, 1, 1, 4);
   
   Input Parameters:        Image *pImage - ptr to image object
   int bGetColorData - get color data if nonzero
   int bGetAlpha - get alpha data if nonzero
   int iDataStride - number of bytes between
   data of the same color/alpha.
   
   Output Parameters:       none
   
   Return Value:            none
   
   Externals:               none
   
   Input Data:              none
   
   Output Data:             none
   
   Calling Functions:       render()
   
   Functions Called:        none
   
   Description:             none
   
   ****************************************************************************/ 
#endif

void OpenGLrenderer::pixmap_to_image( ImgInfo *inimg,
									 double near, double far,
									 int iDataStride)
{
	int i, j;
	ImageData *pImgDat;
	int bGetColorData = inimg->get_image_flag();
	int bGetAlpha = this->bUseAlpha; // get flag from object
	int bGetDepthData = this->bUseDepth; // ditto
	uchar *pbyte;
	uchar	*rbyte, *gbyte, *bbyte, *abyte;
    
	if (parray == NULL) {
		fprintf( stderr, "Unable to read pixmap: no memory allocated\n");
		exit( 0);
	}
    else if (inimg == NULL) {
		fprintf( stderr, "Unable to read pixmap into image: no image available\n");
		exit( 0);
	}
    
	// copy from context
	
	if (wx > 1024 && wy > 1024)
		fprintf( stderr, "reading data from pixmap...\n");
    
	glReadPixels( 0, 0, wx, wy, GL_RGBA, GL_UNSIGNED_BYTE, parray); 
	
	if (wx > 1024 && wy > 1024)
		fprintf( stderr, "Transferring data from pixmap to Image...\n");
    
	pImgDat = inimg->get_image(0)->get_data(); // get image data
	if (pImgDat == NULL) {
		fprintf( stderr,
				"Error: missing image object for color data\n");
	}
    
	if (bForceFastXfers && pImgDat->preferred_data_type() == UCHAR_DATA) {
		// fast byte transfers
		if(bGetColorData) { // should we get image color data?
			rbyte = ((ucharData *)pImgDat)->get_data(RED_BAND);
			gbyte = ((ucharData *)pImgDat)->get_data(GREEN_BAND);
			bbyte = ((ucharData *)pImgDat)->get_data(BLUE_BAND);
		} else {				// no
			rbyte = NULL;
			gbyte = NULL;
			bbyte = NULL;
		}
		if(bGetAlpha) {		// should we get alpha data?
			abyte = ((ucharData *)pImgDat)->get_data(ALPHA_BAND);
		} else {
			abyte = NULL;		// no
		}
        
		// assumption: data is in the follwing order:
		//   offset 0: alpha
		//   offset 1: blue
		//   offset 2: green
		//   offset 3: red
		for(i=wy-1; i>=0; i--) {
			pbyte = (uchar *)(parray + i*wx);
			pbyte += (bSwapOrder) ? A_OFST_SWAP : A_OFST;
			if(abyte) {
				for(j=0; j<wx; j++) {
					*abyte++ = *pbyte;
					pbyte += iDataStride;
				}
			}
			pbyte = (uchar *)(parray + i*wx);
			pbyte += (bSwapOrder) ? B_OFST_SWAP : B_OFST;
			if(bbyte) {
				for(j=0; j<wx; j++) {
					*bbyte++ = *pbyte;
					pbyte += iDataStride;
				}
			}
			pbyte = (uchar *)(parray + i*wx);
			pbyte += (bSwapOrder) ? G_OFST_SWAP : G_OFST;
			if(gbyte) {
				for(j=0; j<wx; j++) {
					*gbyte++ = *pbyte;
					pbyte += iDataStride;
				}
			}
			pbyte = (uchar *)(parray + i*wx);
			pbyte += (bSwapOrder) ? R_OFST_SWAP : R_OFST;
			if(rbyte) {
				for(j=0; j<wx; j++) {
					*rbyte++ = *pbyte;
					pbyte += iDataStride;
				}
			}
		}
		// djf!!!NOTE: need to do z buffer here
	}
    else {
		for (i = wy-1; i >= 0; i--) { // start at last row
			pbyte = (uchar *)(parray + (wy-i-1)*wx); // beg of current row
			for(j=0; j<wx; j++) {
                
				if (bSwapOrder) {
					if(bGetAlpha) {
						pImgDat->set(*pbyte++, j, i, ALPHA_BAND);
					}
                    else
                        pbyte++;
					pImgDat->set(*pbyte++, j, i, BLUE_BAND);
					pImgDat->set(*pbyte++, j, i, GREEN_BAND);
					pImgDat->set(*pbyte++, j, i, RED_BAND);
				}
                else {
					pImgDat->set(*pbyte++, j, i, RED_BAND);
					pImgDat->set(*pbyte++, j, i, GREEN_BAND);
					pImgDat->set(*pbyte++, j, i, BLUE_BAND);
					if(bGetAlpha) {
						pImgDat->set(*pbyte++, j, i, ALPHA_BAND);
					}
                    else {
						pbyte++;
						pImgDat->set( 0xff, j, i, ALPHA_BAND);
					}
				}
			}
		}
	}
	
	// get depth information, if requested
	if (bGetDepthData) {
		long	*pfloat;
		int	zband, ir[2];
		double	dist, N, F;
		
		
		// copy from pixmap
		if (wx > 1024 && wy > 1024)
			fprintf( stderr, "Reading depth data from pixmap...\n");
		
		glReadPixels( 0, 0, wx, wy, GL_DEPTH_COMPONENT,
					 GL_INT, parray); 
		
		if (wx > 1024 && wy > 1024)
			fprintf( stderr,
					"Transferring depth data from pixmap to Image...\n");
		
		// get image index
		if(bGetColorData || bGetAlpha) {
			zband = 1;
		} else {
			zband = 0;
		}
		
		pImgDat = inimg->get_image(zband)->get_data(); // get image data
		if (pImgDat == NULL) {
			fprintf( stderr,
					"Error: missing image object for z buffer\n");
		}
		
		// obtain near and far clip planes as used
		// in the viewport transformation
		
		glGetIntegerv( GL_DEPTH_RANGE, ir);	// get it as an int to avoid
		// normalization to range 0-1
		N = (double)ir[0]; F = (double)ir[1]; // for proper use in formula
		
		
		for(i=wy-1; i>=0; i--) {
			//			pfloat = (long *)(parray + (wy-i-1)*wx);
			pfloat = (long *)(parray + i*wx);
			for(j=0; j<wx; j++) {
				
				dist = -(((far*near*(F-N)) / (far - near)) /
						 (*pfloat++ - (((far+near)*(F-N))/(2.0*(far-near))) -((F+N)/2.0)));
				
				// needs Zareh's xlation
				//				pImgDat->set(dist,j,i,DEFAULT_BAND); 
				pImgDat->set(dist,j,wy-1-i,DEFAULT_BAND); 
			}
		}
	}
	
	
	fprintf( stderr, "Done\n");
	
}

void OpenGLrenderer::render_context_to_image( ImgInfo *inimg,
											 double near, double far,
											 int iDataStride,
											 int ndxImage)
{
	int i, j;
	ImageData *pImgDat = NULL;
	int bGetColorData = inimg->get_image_flag();
	int bGetAlpha = this->bUseAlpha; // get flag from object
	int bGetDepthData = this->bUseDepth; // ditto
	uchar *pbyte;
	uchar	*rbyte, *gbyte, *bbyte, *abyte;
	int nImgOfst; // in from base ptr to beg of this sub window in Image
	int nOffsetToSubWinFromImgLeftSide;
	int nImgYForSubWinYStart;
	int nSkipToNextLineInImg;
	int nSubWinStopPixX;		// where to stop transferring from context x
	int nSubWinStopPixY;		// where to stop transferring from context y
#ifdef GET_TIMINGS
	Timer tDepthCalc, tDepthRead, tRead, tXfer;
	char str0[64], str1[64], str2[64], str3[64];
#endif
    
	if (parray == NULL) {
		fprintf( stderr, "Unable to read context: no memory allocated\n");
		exit( 0);
	}
    else if (inimg == NULL) {
		fprintf( stderr,
				"Unable to read context into image: no image available\n");
		exit( 0);
	}
    
	// copy from rendering context
	if (eDebug & DbgAll)
		fprintf( stderr, "Reading data from context...\n");
	
//	XRaiseWindow( pXdisplay, win); // keep on the top
	glXWaitX();

	
//	XGrabServer( pXdisplay);
//	XMapRaised( pXdisplay, win);
	
#ifdef GET_TIMINGS
	glFinish();					// block until pending GL calls are done
	tRead.start();
#endif

	glReadPixels( 0, 0, subwin_x, subwin_y, GL_RGBA, GL_UNSIGNED_BYTE, parray); 

#ifdef GET_TIMINGS
	tRead.stop();
#endif
	
//	XUngrabServer( pXdisplay);


	if (eDebug & DbgAll)
		fprintf( stderr, "Transferring data from context to Image...\n");

								// grab access of image's data ptr
	if (inimg->get_image(0) != NULL)
		pImgDat = inimg->get_image(0)->get_data(); // get image data
	if (pImgDat == NULL) {
		fprintf( stderr,
				"Error: missing image object for color data\n");
	}

	// setup some pertinent #'s for handling sub-images; the simplest case
	// acts as if there is one subimage - and that subimage is the size of
	// the entire output image

	// int nImgLineSize = wx * iDataStride;
	// int nSubWinLineSize = subwin_x * iDataStride;

	nOffsetToSubWinFromImgLeftSide =
		(ndxImage % nSubFramesX) * subwin_x;
		
	nImgYForSubWinYStart = (ndxImage / nSubFramesX) * subwin_y;
	nImgOfst = wx * nImgYForSubWinYStart + nOffsetToSubWinFromImgLeftSide;
		
								// what's the stop pixel in the x and y
								// directions; this is because the Image
								// size may be smaller than the area
								// covered by the subwindows.
	if (wx % subwin_x == 0 ||
		ndxImage % nSubFramesX < nSubFramesX - 1)
		nSubWinStopPixX = subwin_x;
	else
		nSubWinStopPixX = wx % subwin_x;
	if (wy % subwin_y == 0 ||
		ndxImage / nSubFramesX < nSubFramesY - 1)
		nSubWinStopPixY = subwin_y;
	else
		nSubWinStopPixY = wy % subwin_y;
		
	nSkipToNextLineInImg = wx - nSubWinStopPixX;

#ifdef GET_TIMINGS
    tXfer.start();
#endif
	if (bForceFastXfers && pImgDat->preferred_data_type() == UCHAR_DATA) {
		// fast byte transfers
		if(bGetColorData) { // should we get image color data?
			rbyte = ((ucharData *)pImgDat)->get_data(RED_BAND);
			gbyte = ((ucharData *)pImgDat)->get_data(GREEN_BAND);
			bbyte = ((ucharData *)pImgDat)->get_data(BLUE_BAND);
		} else {				// no
			rbyte = NULL;
			gbyte = NULL;
			bbyte = NULL;
		}
		if(bGetAlpha) {		// should we get alpha data?
			abyte = ((ucharData *)pImgDat)->get_data(ALPHA_BAND);
		} else {
			abyte = NULL;		// no
		}
		
		// handle subimages, if any
		
		rbyte += nImgOfst;
		gbyte += nImgOfst;
		bbyte += nImgOfst;
		if (abyte != NULL)
			abyte += nImgOfst;
		
		// assumption: data is in the follwing order:
		//   offset 0: alpha
		//   offset 1: blue
		//   offset 2: green
		//   offset 3: red
		int k = subwin_y - 1;
		for(i=nSubWinStopPixY-1; i>=0; i--, k--) {
			
			pbyte = (uchar *)(parray + k*subwin_x);
			pbyte += (bSwapOrder) ? A_OFST_SWAP : A_OFST;
			if(abyte) {
				for(j=0; j<nSubWinStopPixX; j++) {
					*abyte++ = *pbyte;
					pbyte += iDataStride;
				}
			}
			pbyte = (uchar *)(parray + k*subwin_x);
			pbyte += (bSwapOrder) ? B_OFST_SWAP : B_OFST;
			if(bbyte) {
				for(j=0; j<nSubWinStopPixX; j++) {
					*bbyte++ = *pbyte;
					pbyte += iDataStride;
				}
			}
			pbyte = (uchar *)(parray + k*subwin_x);
			pbyte += (bSwapOrder) ? G_OFST_SWAP : G_OFST;
			if(gbyte) {
				for(j=0; j<nSubWinStopPixX; j++) {
					*gbyte++ = *pbyte;
					pbyte += iDataStride;
				}
			}
			pbyte = (uchar *)(parray + k*subwin_x);
			pbyte += (bSwapOrder) ? R_OFST_SWAP : R_OFST;
			if(rbyte) {
				for(j=0; j<nSubWinStopPixX; j++) {
					*rbyte++ = *pbyte;
					pbyte += iDataStride;
				}
			}
			
			// get to beg of next subwin line in Image
			if (rbyte)
				rbyte += nSkipToNextLineInImg;
			if (gbyte)
				gbyte += nSkipToNextLineInImg;
			if (bbyte)
				bbyte += nSkipToNextLineInImg;
			if (abyte)
				abyte += nSkipToNextLineInImg;
			
		}						// for (y...)

	}
    else {

		for (i = nSubWinStopPixY-1; i >= 0; i--) { // start at last row

								// point to beg of current row
			pbyte = (uchar *)(parray + (subwin_y-i-1)*subwin_x);

			for(j=0; j<nSubWinStopPixX; j++) {
                
				if (bSwapOrder) {
					if(bGetAlpha) {
						pImgDat->set(*pbyte++, j+nOffsetToSubWinFromImgLeftSide,
									 i+nImgYForSubWinYStart, 
									 ALPHA_BAND);
					}
                    else {
                        pbyte++;
						pImgDat->set( 0xff, j+nOffsetToSubWinFromImgLeftSide,
									 i+nImgYForSubWinYStart, 
									 ALPHA_BAND);
					}
					pImgDat->set(*pbyte++, j+nOffsetToSubWinFromImgLeftSide,
								 i+nImgYForSubWinYStart, BLUE_BAND);
					pImgDat->set(*pbyte++, j+nOffsetToSubWinFromImgLeftSide,
								 i+nImgYForSubWinYStart, GREEN_BAND);
					pImgDat->set(*pbyte++, j+nOffsetToSubWinFromImgLeftSide,
								 i+nImgYForSubWinYStart, RED_BAND);
				}
                else {
					pImgDat->set(*pbyte++, j+nOffsetToSubWinFromImgLeftSide,
								 i+nImgYForSubWinYStart, RED_BAND);
					pImgDat->set(*pbyte++, j+nOffsetToSubWinFromImgLeftSide,
								 i+nImgYForSubWinYStart, GREEN_BAND);
					pImgDat->set(*pbyte++, j+nOffsetToSubWinFromImgLeftSide,
								 i+nImgYForSubWinYStart, BLUE_BAND);
					if(bGetAlpha) {
						pImgDat->set(*pbyte++, j+nOffsetToSubWinFromImgLeftSide,
									 i+nImgYForSubWinYStart,
									 ALPHA_BAND);
					}
                    else {
						pbyte++;
						pImgDat->set( 0xff, j+nOffsetToSubWinFromImgLeftSide,
									 i+nImgYForSubWinYStart, 
									 ALPHA_BAND);
					}
				}
			}
		}
	}
	
#ifdef GET_TIMINGS
	tXfer.stop();
#endif

	// get depth information, if requested
	if (bGetDepthData) {
		int	zband, ir[2];
		double	dist, N, F;
		
		
		// copy from context
		if (eDebug & DbgAll)
			fprintf( stderr, "Reading depth data from context...\n");
		
//		XGrabServer( pXdisplay);
//		XMapRaised( pXdisplay, win);
		
#ifdef GET_TIMINGS
		glFinish();					// block until pending GL calls are done
		tDepthRead.start();
#endif
	
		long	*pfloat;
		glReadPixels( 0, 0, subwin_x, subwin_y, GL_DEPTH_COMPONENT,
					 GL_INT, parray); 
//		float	*pfloat;
//		glReadPixels( 0, 0, subwin_x, subwin_y, GL_DEPTH_COMPONENT,
//					 GL_FLOAT, parray); 
	
#ifdef GET_TIMINGS
		tDepthRead.stop();
#endif

//		XUngrabServer( pXdisplay);

		
		if (eDebug & DbgAll)
			fprintf( stderr,
				"Transferring depth data from context to Image...\n");
		
		// get image index
		if(bGetColorData || bGetAlpha) {
			zband = 1;
		} else {
			zband = 0;
		}
		
		pImgDat = inimg->get_image(zband)->get_data(); // get image data
		if (pImgDat == NULL) {
			fprintf( stderr,
					"Error: missing image object for z buffer\n");
		}
		
		// obtain near and far clip planes as used
		// in the viewport transformation
		
		glGetIntegerv( GL_DEPTH_RANGE, ir);	// get it as an int to avoid
		// normalization to range 0-1
		N = (double)ir[0]; F = (double)ir[1]; // for proper use in formula
		
		int k = subwin_y - nSubWinStopPixY;

#ifdef GET_TIMINGS
		tDepthCalc.start();
#endif

		for(i=nSubWinStopPixY-1; i>=0; i--, k++) {
			////			pfloat = (long *)(parray + (subwin_y-i-1)*subwin_x);
			//pfloat = (float *)(parray + k*subwin_x);
			pfloat = (long *)(parray + k*subwin_x);
			for(j=0; j<nSubWinStopPixX; j++) {
				
				dist = -(((far*near*(F-N)) / (far - near)) /
						 (*pfloat++ - (((far+near)*(F-N))/(2.0*(far-near))) -((F+N)/2.0)));
//						 ((2147483647.0*(*pfloat++)) - (((far+near)*(F-N))/(2.0*(far-near))) -((F+N)/2.0)));
				
				// needs Zareh's xlation
				//				pImgDat->set(dist,j,i,DEFAULT_BAND); 
				pImgDat->set(dist,
							 j+nOffsetToSubWinFromImgLeftSide,
							 i+nImgYForSubWinYStart,
							 DEFAULT_BAND); 
			}
		}
		
#ifdef GET_TIMINGS
		tDepthCalc.stop();
#endif
	}
	
	
	if (eDebug & DbgAll)
		fprintf( stderr, "Done\n");

#ifdef GET_TIMINGS
	fprintf( stderr,
			"read=%s xfer=%s rd z=%s calc z=%s\n",
			tRead.elapsed_as_string( str0),
			tXfer.elapsed_as_string( str1),
			tDepthRead.elapsed_as_string( str2),
			tDepthCalc.elapsed_as_string( str3));
#endif			
	
}

void OpenGLrenderer::create_test_xwin( int w, int h, Display *dpy, XVisualInfo *vi)
{
	XSetWindowAttributes swa;
	/* create a color map */
	swa.colormap = XCreateColormap(dpy, RootWindow(dpy, vi->screen),
								   vi->visual, AllocNone);
    
	/* create a window */
	swa.border_pixel = 0;
	swa.event_mask = ExposureMask | StructureNotifyMask | KeyPressMask;
	win = XCreateWindow(dpy, RootWindow(dpy, vi->screen), 0, 0, w, h,
						0, vi->depth, InputOutput, vi->visual,
						CWBorderPixel|CWColormap|CWEventMask, &swa);
	XStoreName(dpy, win, "pixmap contents");
	XMapWindow(dpy, win);
    
	xgc = XCreateGC(dpy, win, 0, NULL);
	
	XClearArea( dpy, win, 0, 0, w, h, 1);
	glXSwapBuffers( dpy, win);
	XClearArea( dpy, win, 0, 0, w, h, 1);
	glXSwapBuffers( dpy, win);
    
}

void OpenGLrenderer::set_debug_level( OGLDebugT flag)
{								/* set debug flag */
	eDebug = flag;
}

OGLDebugT OpenGLrenderer::get_debug_level( void)
{
	return eDebug;
}				/* get debug flag */


GLenum get_light_number( int nCurLightIndex)
{
	GLenum light_num;
	
	switch (nCurLightIndex) {
	case 0:
		light_num = GL_LIGHT0;
		break;
	case 1:
		light_num = GL_LIGHT1;
		break;
	case 2:
		light_num = GL_LIGHT2;
		break;
	case 3:
		light_num = GL_LIGHT3;
		break;
	case 4:
		light_num = GL_LIGHT4;
		break;
	case 5:
		light_num = GL_LIGHT5;
		break;
	case 6:
		light_num = GL_LIGHT6;
		break;
	case 7:
	default:
		light_num = GL_LIGHT7;
		break;
	}
	
	return light_num;
}

#ifdef FUNCT_HDR

/* ************************************************************************* 
   
   Function Name:           process_light_object
   
   Purpose:                 Establish the light object's presence in the
   scene
   
   Sample Call:             status = process_light_object();
   
   Input Parameters:        none
   
   Output Parameters:       none
   
   Return Value:            int - TRUE if success, FALSE otherwise
   
   Externals:               none
   
   Input Data:              none
   
   Output Data:             none
   
   Calling Functions:       process_object()
   
   Functions Called:        dparam::get_value()
   get_light_number()
   glLightfv()
   LightObject::get_amb()
   LightObject::get_diff()
   LightObject::get_spec()
   glIsEnabled()
   glEnable()
   
   Description:             none
   
   ****************************************************************************/ 
#endif


int OpenGLrenderer::process_light_object(void *p)
{
	int nMaxOGLLights; // max lights supported
	ObjNode *pObjNode = (ObjNode *)p;
	DirectionalLightObj *pDL =
		(DirectionalLightObj *)(pObjNode->get_object());
	LightObject *pLO =
		(LightObject *)(pObjNode->get_object());
	
	if (pDL->get_num_bands() != 3) {
		fprintf( stderr, 
				"Warning: light has %d bands, using first 3 if possible\n",
				pDL->get_num_bands());
	}
	if (pDL->using_alpha() == FALSE) {
		fprintf( stderr, 
				"Warning: light %d does not have alpha values, using default\n",
				nLights);
	}
	
	glGetIntegerv( GL_MAX_LIGHTS, 
				  (GLint *)&nMaxOGLLights); // query the OGL implementation
	if (nLights < nMaxOGLLights) { // still room for more lights
		
		// get and assign light's id
		pLO->light_id.set_value( get_light_number( nLights)); 
		
		// compute and set light parameters
		compute_light_params( pObjNode);
		
		glEnable( pLO->light_id.get_value()); // enable the current light
		
		nLights++;	// using one more light
	}
	else {						// not using this light: too many specified
		fprintf( stderr,
	 "Warning: maximum number of lights (%d) exceed, ignoring light\n", 
				nMaxOGLLights);
		return FALSE;
	}

								// enable lighting, if not already enabled
	if (glIsEnabled( GL_LIGHTING) == GL_FALSE)
		glEnable( GL_LIGHTING);

	return TRUE;

}


int OpenGLrenderer::process_poly_object( void *p, int nObjectNdx)
{
	int num_polys;
	int num_points_in_poly;
	int point_index;
	int nrml_index;
	int color_index;
	int     i,j;
	double  vert[3], texel[3], norm[3];
	float  fvert[3], fnorm[3];
	GLfloat cv[4] = { -1.0, -1.0, -1.0, 1.0};
	double  red,green,blue;
	float fred, fgreen, fblue;
	GL_Object  sgi_object;
	int     dummy;
	PolyObject1 *pPolyObj = (PolyObject1 *)p;


	// int num_colors = pPolyObj->get_num_colors();
	// int num_edges  = pPolyObj->get_num_edges();
	num_polys  = pPolyObj->get_num_faces();

								// see if we need to generate normals
	if (pPolyObj->get_obj_model_type() == SmoothShaded ||
		should_we_force_normal_calcs()) {
		bDoNormals = 1;		// we need to use normals
		glShadeModel( GL_SMOOTH); // change shade model

								// generate normals only if there are not
								// any point normals already defined; for now
								// assume that we will never want to re-generate
								// normals: in other words, calc normals one time
								// per object
//		if (pPolyObj->get_pt_normal( 0) == -1) { // none yet exist: calc them

			pPolyObj->build_normal_structure(); // make space
			pPolyObj->generate_normals();		  // calc normals
//		}
	}
	else if (pPolyObj->get_obj_model_type() == TextureMapped) {
		bDoNormals = 0;			// do not use normals
	}
	else if (pPolyObj->get_obj_model_type() == FlatShaded) {
		bDoNormals = 0;         // do not use normals
		glShadeModel( GL_FLAT);
	}

	if (get_debug_level() & DbgDumpObj)
		pPolyObj->dump();		// show the object

	sgi_object = glGenLists( 1); // get new display list ID
	My_OGL_Objects[nObjectNdx] = sgi_object; // save display list id for later
	glNewList( sgi_object, GL_COMPILE); /* create GL object */

								// handle textures, if any

								// 1st, load named texture, if named
	if (pPolyObj->get_texture_name() != NULL) {
		if (!pPolyObj->load_texture_image()) {
			fprintf( stderr, "Unable to load image file %s\n",
					pPolyObj->get_texture_name());
		}
	}

								// if Image exists, prepare it
	if (pPolyObj->surface.get_tmap_color() != NULL &&
		   pPolyObj->surface.get_tmap_color()->get_texture() != NULL) {
		glEnable(GL_TEXTURE_2D);
	    if (convert_to_texture == True) {
		    pTextures[nObjectNdx].pTexture = convert_image_to_opengl_texture( 
					 pPolyObj->surface.get_tmap_color()->get_texture(),
					 pTextures[nObjectNdx].w,
					 pTextures[nObjectNdx].h);
	    }
		get_ready_to_texture_map( pTextures[nObjectNdx].w, 
								 pTextures[nObjectNdx].h,
								 pTextures[nObjectNdx].pTexture);
	} else glDisable(GL_TEXTURE_2D); // disable texture mapping

	if (pPolyObj->get_type() == POLY_V1) {
		for(i=0; i<num_polys; i++) { /* loop for number of faces in object */
			num_points_in_poly = pPolyObj->get_num_verts(i);

                        if (pPolyObj->get_obj_model_type() == WireFrame) 
                                glBegin( GL_LINE_LOOP); // wire frame drawing
                        else glBegin( GL_POLYGON); // start polygon's definition
			
			/* set the color */
			color_index = pPolyObj->get_pcolor(i);
			
			if (pPolyObj->get_color(color_index,red,green,blue) != FALSE) {
				glColor3d(red,green,blue);
				
				if (red != cv[0] ||	// material color has changed, update it
					green != cv[1] ||
					blue != cv[2]) {
					
					cv[0] = red; cv[1] = green; cv[2] = blue; cv[3] = 1.0;
					glMaterialfv( GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE,
								 (const GLfloat *)cv);
				}
			}
			
			// for each vertex in the polygon
			for(j=0; j<num_points_in_poly; j++) {
				
				point_index = pPolyObj->get_vert(i,j); // get pt's index
				
				// deal with normals, if requested
				if (bDoNormals) {
					
					// get index of normal that corresponds to the
					// point_index-th vertex
					nrml_index = pPolyObj->get_pt_normal( point_index);
					
					// get the normal data
					if (pPolyObj->get_normal( nrml_index,
											 norm[0], norm[1], norm[2])
						== TRUE 
						&&
						bNoNormals == GL_FALSE)	// not skipping (a debug option)
						
						// got valid normal data
						glNormal3dv(norm); // specify normal of following pts
				}
				
				// get pt's coords
				pPolyObj->get_point(point_index,dummy,
									vert[0],vert[1],vert[2]);
				
				// specify texture coords, if applicable
				if (pTextures[nObjectNdx].pTexture != NULL) {
					pPolyObj->surface.get_tmap_color()->get_texture_coord( 
									  vert, texel);
					glTexCoord3dv( texel);
				}
				
				glVertex3dv(vert);	/* set the vertex */
				
				if (vert[0] > xmax)
					xmax = vert[0];
				if (vert[0] < xmin)
					xmin = vert[0];
				
				if (vert[1] > ymax)
					ymax = vert[1];
				if (vert[1] < ymin)
					ymin = vert[1];
				
				if (vert[2] > zmax)
					zmax = vert[2];
				if (vert[2] < zmin)
					zmin = vert[2];
				
			}						/* for j */
			
			glEnd();			// end of the current polygon
			
		}						/* for i */
	}							// end if
	else if (pPolyObj->get_type() == POLY_TERRAIN_V1) {
		int r,c, xf, yf, nTrianglesPerStrip;
		PolyTerrainObj *pTerr = (PolyTerrainObj *)pPolyObj;
	
		pTerr->get_mesh_factor( xf, yf);
		nTrianglesPerStrip = xf * 2;

		for (r = 0; r < yf; r++) { // for each row of triangles
			
			glBegin( GL_TRIANGLE_STRIP); // start strip's def
			
			for(c=0; c< nTrianglesPerStrip; c++) { 
				
				i = r * nTrianglesPerStrip + c;	// index of triangle in poly list
				
				/* loop for number of tirangles in row */
				
				num_points_in_poly = pPolyObj->get_num_verts(i);
				
				
				/* set the color */
				color_index = pPolyObj->get_pcolor(i);
				
				if (pPolyObj->get_color(color_index,fred,fgreen,fblue) != FALSE) {
					glColor3f(fred,fgreen,fblue);
					
					if (fred != cv[0] ||	// material color has changed, update it
						fgreen != cv[1] ||
						fblue != cv[2]) {
						
						cv[0] = fred; cv[1] = fgreen; cv[2] = fblue; cv[3] = 1.0;
						glMaterialfv( GL_FRONT_AND_BACK, GL_AMBIENT_AND_DIFFUSE,
									 (const GLfloat *)cv);
					}
				}
				
				// for each vertex in the polygon
				for(j=0; j<num_points_in_poly; j++) {
					
					if (c && j != num_points_in_poly - 1) // 3rd point only
						// unless first
						// triangle
						continue;
					
					point_index = pPolyObj->get_vert(i,j); // get pt's index
					
					// deal with normals, if requested
					if (bDoNormals) {
						
						// get index of normal that corresponds to the
						// point_index-th vertex
						nrml_index = pPolyObj->get_pt_normal( point_index);
						
						// get the normal data
						if (pPolyObj->get_normal( nrml_index,
												 norm[0], norm[1], norm[2])
							== TRUE 
							&&
							bNoNormals == GL_FALSE)	{ 
							// not skipping (a debug option)
							
							fnorm[0] = norm[0];
							fnorm[1] = norm[1];
							fnorm[2] = norm[2];
						
							// got valid normal data
							glNormal3fv(fnorm); // specify normal of following pts
						}
					}
					
					// get pt's coords
					pPolyObj->get_point(point_index,dummy,
										fvert[0],fvert[1],fvert[2]);
					
					// specify texture coords, if applicable
					if (pTextures[nObjectNdx].pTexture != NULL) {
			
						pPolyObj->get_texel(point_index, texel[0], texel[1],
														  texel[2]);
						glTexCoord3dv( texel);
					}
					
					glVertex3fv(fvert);	/* set the vertex */
					
				}						/* for j */
				
			}						/* for c */
			
			glEnd();			// end of the current polygon
			
		}						// for r
	}
		
		//new
		// do testure mapping, if specified
	if (pTextures[nObjectNdx].pTexture != NULL)	// current object is
			// texturemapped
		glDisable(GL_TEXTURE_2D); // disable texture mapping
		
		
	//new	
		
	glEndList();		// end of the entire display list
		
	num_polygon_objects++;
		
		
	return TRUE;		// success
		
}
	
void 
compute_light_params( ObjNode *pObjNode)
{
	//	double pos_vect[4] = { 0.0, 0.0, 0.0, 1.0};
	double pos_vect[4] = { 0.0, 0.0, 0.0, 0.0};
	GLfloat cv[4];				// RGBA color vector
	DirectionalLightObj *pDL =
		(DirectionalLightObj *)(pObjNode->get_object());
	LightObject *pLO =
		(LightObject *)(pObjNode->get_object());
	GLenum nLight = pLO->light_id.get_value();
	
	// get light's position
	pos_vect[0] = pObjNode->x.get_value();
	pos_vect[1] = pObjNode->y.get_value();
	pos_vect[2] = pObjNode->z.get_value();
	
	glLightfv( nLight, GL_POSITION, (GLfloat *)pos_vect);
	
	// get & set light's colors
	pDL->get_amb( cv, pDL->get_intensity()); // ambient light color
	glLightfv( nLight, GL_AMBIENT, cv);		 // tell renderer
	
	pDL->get_diff( cv, pDL->get_intensity()); // diffuse light color
	glLightfv( nLight, GL_DIFFUSE, cv);		  // tell renderer
	
	pDL->get_spec( cv, pDL->get_intensity()); // specular component
	glLightfv( nLight, GL_SPECULAR, cv);	  // tell renderer
	
}

XVisualInfo *
get_visualinfo( Display *pXD, 
			   Window Xwin)
{
	XVisualInfo *pXVis = NULL;	// returned ptr to vis info struct
	XWindowAttributes xwa;		// x win attribs struct
	VisualID vid;				// visual id associated with window
	XVisualInfo xvis;			// template visual info struct to match
	int num_ret;				// number of visuals that match
	long vimask = VisualIDMask;	// base selection on the id only
	
	XGetWindowAttributes( pXD, Xwin, &xwa);	// get win attribs
	
	vid = XVisualIDFromVisual( xwa.visual);	// get visual's id
	
	xvis.visualid = vid;		// we want to match this guy
	
	// get the best match per the template
	pXVis = XGetVisualInfo( pXD, vimask, &xvis, &num_ret);
	
	return pXVis;				// ptr to corresponding visual info struct
}

int
OpenGLrenderer::init_render_to_pixmap( void)
{
	int nDepthBits = 0;
	
	// open X connection to X server
	//	if ((pXdisplay = XOpenDisplay( NULL)) == NULL) {
	char *pHost = get_hostname_for_display();
	
	if (pHost != NULL)			// open display on the specified host - this
		// is intended to be used only when running
		// distributed renderers
		
		if ((pXdisplay = XOpenDisplay( pHost)) == NULL) {
			fprintf( stderr, "Unable to open connection to X server '%s'\n",
					pHost);
			return 1;
		}
		else					// open local display
			if ((pXdisplay = XOpenDisplay( NULL)) == NULL) {
				fprintf( stderr, "Unable to open connection to X server\n");
				return 1;
			}
	
	
	if ((pVis = MyChooseXVisual( pXdisplay, GL_FALSE, // no double buffering
								bUseAlpha, bUseDepth))
		== NULL)
		
		return 1;				// fails to choose visual
	
	glXGetConfig( pXdisplay,
						  pVis,
						  GLX_DEPTH_SIZE,
						  &nDepthBits);
	
	// create the rendering context
	if ((glx_context = glXCreateContext( pXdisplay, pVis, 
										(GLXContext)first_context, GL_FALSE))
		== NULL ){
		fprintf( stderr, "Unable create context\n");
		return 1;
	}
	//	if (first_context == NULL)
	//		first_context = glx_context;
	// create X pixmap
	XPixMap = XCreatePixmap( pXdisplay, DefaultRootWindow(pXdisplay),
							wx, wy, pVis->depth);
	
	// generate corresponding OGL pixmap
	GLXPixMap = glXCreateGLXPixmap( pXdisplay, pVis, XPixMap);
	
	// select OGL pixmap as rendering dest
	glXMakeCurrent( pXdisplay, GLXPixMap, glx_context);
	
	bPixMap = GL_TRUE;			// we are rendering into pixmaps!
	
	return 0;					// ok
}

int
OpenGLrenderer::init_render_to_xwin( Display *pXD,
									Window Xwin)
{
	XVisualInfo *pXVis;
	
	if (pXD == NULL)
		return 1;				// failure!
	
	this->pXdisplay = pXD;		// set display
	this->win = Xwin;			// set win id
	
	// get visual info from display & window
	pXVis = get_visualinfo( pXdisplay, win);
	
	// create an X rendering context
	if ((glx_context = glXCreateContext( pXdisplay, pXVis,
										(GLXContext)first_context, GL_FALSE))
		== NULL ){
		fprintf( stderr, "Unable create context\n");
		return 1;					// error
	}
	
	XFree( pXVis);				// return to free store
	
	// make the context be this one
	glXMakeCurrent( pXdisplay, win, glx_context); 
	
	return 0;					// ok
}

int
OpenGLrenderer::init_render_to_xwin( void)
	// 
	// 
	// rendering to xwins, but user did not create windows; hence, create xwins
	// and render into them.  If requested image size is larger than the largest
	// xwin (ie. the size of the display) then create one smaller window and render
	// portions of the final image, by compositing.
	// 
	// 
{
	Display *pXD;
	Window Xwin;
	
	// open X connection to X server
	if ((pXD = XOpenDisplay( NULL)) == NULL) {
		fprintf( stderr, "Unable to open connection to X server\n");
		return 1;					// fail
	}
	
	// choose appropriate visual
	if ((pVis = MyChooseXVisual( pXD,
								GL_TRUE,					// yes: doublebuffer
								bUseAlpha, bUseDepth)) == NULL) {
		return 1;					// fail
	}
	
	int nDepthBits = 0;
	glXGetConfig( pXD,
						  pVis,
						  GLX_DEPTH_SIZE,
						  &nDepthBits);
	
	XSetWindowAttributes swa;
	/* create a color map */
	swa.colormap = XCreateColormap( pXD,
								   RootWindow(pXD, pVis->screen),
								   pVis->visual, AllocNone);
	
	/* create a window */
	
	determine_render_size( pXD); // can render as one image, or must we break
	// it into pieces?
	
	swa.border_pixel = 0;
	swa.event_mask = ExposureMask | StructureNotifyMask | KeyPressMask;
	Xwin = XCreateWindow(pXD, RootWindow(pXD, pVis->screen), 
						 0, 0, subwin_x, subwin_y,
						 0, pVis->depth, InputOutput, pVis->visual,
						 CWBorderPixel|CWColormap|CWEventMask, &swa);
	
	XStoreName(pXD, Xwin, "X Rendering Area");
	XMapWindow( pXD, Xwin);
	
	XClearArea( pXD, Xwin, 0, 0, subwin_x, subwin_y, 1);
	
	init_render_to_xwin( pXD, Xwin);
	
	return 0;					// ok
}


XVisualInfo *
MyChooseXVisual( Display *pXdisplay, GL_Boolean bDoubleBuffer,
				GL_Boolean bAlpha, GL_Boolean bDepth)
{
	int confWithAlphaAndDepthDouble[] = { GLX_RGBA, 
										  GLX_DEPTH_SIZE, 1, 
										  GLX_RED_SIZE, 1,
										  GLX_GREEN_SIZE, 1,
										  GLX_BLUE_SIZE, 1, 
										  GLX_ALPHA_SIZE, 1, 
										  GLX_DOUBLEBUFFER,
										  None};
	
	int confWithAlphaAndDepth[] = { GLX_RGBA, 
									GLX_DEPTH_SIZE, 1, 
									GLX_RED_SIZE, 1, 
									GLX_GREEN_SIZE, 1, 
									GLX_BLUE_SIZE, 1, 
									GLX_ALPHA_SIZE, 1,
									None};
	
	int confWithAlpha[] = { GLX_RGBA, 
							GLX_RED_SIZE, 1, 
							GLX_GREEN_SIZE, 1,
							GLX_BLUE_SIZE, 1,
							GLX_ALPHA_SIZE, 1,
							None};
	
	int confWithAlphaDouble[] = { GLX_RGBA,
								  GLX_RED_SIZE, 1,
								  GLX_GREEN_SIZE, 1,
								  GLX_BLUE_SIZE, 1,
								  GLX_ALPHA_SIZE, 1,
								  GLX_DOUBLEBUFFER,
								  None};
	
	int confWithDepth[] = { GLX_RGBA,
							GLX_DEPTH_SIZE, 1,
							GLX_RED_SIZE, 1,
							GLX_GREEN_SIZE, 1,
							GLX_BLUE_SIZE, 1,
							None};
	
	int confWithDepthDouble[] = { GLX_RGBA,
								  GLX_DEPTH_SIZE, 1,
								  GLX_RED_SIZE, 1,
								  GLX_GREEN_SIZE, 1,
								  GLX_BLUE_SIZE, 1,
								  GLX_DOUBLEBUFFER,
								  None};
	
	int confMinimumDouble[] = { GLX_RGBA,
								GLX_RED_SIZE, 1,
								GLX_GREEN_SIZE, 1,
								GLX_BLUE_SIZE, 1,
								GLX_DOUBLEBUFFER,
								None};
	
	int confMinimum[] = { GLX_RGBA, None};
	
	XVisualInfo *pRetval = NULL; // assume the worst
	
	
	if (bAlpha == GL_TRUE && bDepth == GL_TRUE && bDoubleBuffer == GL_TRUE) {	
		// alpha & depth info requested?
		
		// try to get a visual with rgb,z,alpha 2x
		
		if ((pRetval = glXChooseVisual( pXdisplay, 
									   DefaultScreen( pXdisplay),
									   confWithAlphaAndDepthDouble))
			== NULL) {
			fprintf( stderr, "Unable choose doublebuffered X visual with alpha and depth\n");
		}
	}
	
	if (pRetval == NULL && bAlpha == GL_TRUE && bDepth == GL_TRUE) {	
		// alpha & depth info requested?
		
		// try to get a visual with rgb,z,alpha
		
		if ((pRetval = glXChooseVisual( pXdisplay, 
									   DefaultScreen( pXdisplay),
									   confWithAlphaAndDepth))
			== NULL) {
			fprintf( stderr, "Unable choose X visual with alpha and depth\n");
		}
	}
	
	if (pRetval == NULL && bDepth == GL_TRUE && bDoubleBuffer == GL_TRUE) {
		if ((pRetval = glXChooseVisual( pXdisplay, 
									   DefaultScreen( pXdisplay),
									   confWithDepthDouble))
			== NULL) {
			
			fprintf( stderr, "Unable choose doublebuffered X visual with depth\n");
		}								// can't get any type of visual
	}
	
	if (pRetval == NULL && bDepth == GL_TRUE) {
		if ((pRetval = glXChooseVisual( pXdisplay, 
									   DefaultScreen( pXdisplay),
									   confWithDepth))
			== NULL) {
			
			fprintf( stderr, "Unable choose X visual with depth\n");
		}								// can't get any type of visual
	}
	
	if (pRetval == NULL && bAlpha == GL_TRUE && bDoubleBuffer == GL_TRUE) {
		
		if ((pRetval = glXChooseVisual( pXdisplay, 
									   DefaultScreen( pXdisplay),
									   confWithAlphaDouble))
			== NULL) {
			
			fprintf( stderr, "Unable choose doublebuffered X visual with alpha\n");
		}								// can't get any type of visual
	}
	
	if (pRetval == NULL && bAlpha == GL_TRUE) {
		
		if ((pRetval = glXChooseVisual( pXdisplay, 
									   DefaultScreen( pXdisplay),
									   confWithAlpha))
			== NULL) {
			
			fprintf( stderr, "Unable choose X visual with alpha\n");
		}								// can't get any type of visual
	}
	
	if (pRetval == NULL && bDoubleBuffer == GL_TRUE) {
		if ((pRetval = glXChooseVisual( pXdisplay, 
									   DefaultScreen( pXdisplay),
									   confMinimumDouble))
			== NULL) {
			
			fprintf( stderr, "Unable choose doublebuffered X visual of any kind\n");
		}								// can't get any type of visual
	}
	
	if (pRetval == NULL) {
		if ((pRetval = glXChooseVisual( pXdisplay, 
									   DefaultScreen( pXdisplay),
									   confMinimum))
			== NULL) {
			
			fprintf( stderr, "Unable choose X visual of any kind\n");
		}								// can't get any type of visual
	}
	
	return pRetval;
	
	
	
}

void
get_frustum( double fovy, double fovx, double dist_to_plane,
			int wx, int wy, int subwin_x, int subwin_y,
			double &fr_top, double &fr_bottom, 
			double &fr_left, double &fr_right,
			int ndxImage, int nSubWinX, int nSubWinY)
{
	int ndxX = ndxImage % nSubWinX;
	int ndxY = ndxImage / nSubWinX;
	double l_ang, r_ang, t_ang, b_ang, sub_win_ang_x, sub_win_ang_y;
	
	if ( ANG_ZERO( fabs(fovy) - 90.0) ||
		ANG_ZERO( fabs(fovx) - 90.0)) {
		fprintf( stderr, "Warning: can't handle field of view of 90 degrees\n");
		return;
	}
	
	fovx *= TORADIANS;
	fovy *= TORADIANS;
	
	sub_win_ang_x = (((double)(nSubWinX * subwin_x) / (double)wx) * 
					 fovx) / (double)nSubWinX;
	sub_win_ang_y = (((double)(nSubWinY * subwin_y) / (double)wy) * 
					 fovy) / (double)nSubWinY;
	
	l_ang = -fovx/2.0 + (double)ndxX * sub_win_ang_x;
	r_ang = l_ang + sub_win_ang_x;
	
	t_ang = fovy/2.0 - (double)ndxY * sub_win_ang_y;
	b_ang = t_ang - sub_win_ang_y;
	
	fr_left = tan( l_ang) * dist_to_plane;
	fr_right = tan( r_ang) * dist_to_plane;
	fr_top = tan( t_ang) * dist_to_plane;
	fr_bottom = tan( b_ang) * dist_to_plane;
	
}

void OpenGLrenderer::determine_render_size( Display *dp)
	// 
	// 
	// called only if we are not rendering to a user-provided X window;  this
	// computes what size of X window we should create so that we can render
	// the output image:  should we break it up into sub-regions, or can we
	// render it all in one whack?
	// 
	// 
	
{
	int max_x_size = (int)(DisplayWidth( dp, DefaultScreen(dp)) * MAX_X_PCTAGE);
	int max_y_size = (int)(DisplayHeight( dp, DefaultScreen(dp)) * MAX_Y_PCTAGE);
	
	// determine if we need to split rendering
	// into subparts, and then composite
	if (wx > max_x_size)
		subwin_x = max_x_size;
	else
		subwin_x = wx;			// full size
	
	if (wy > max_y_size)
		subwin_y = max_y_size;
	else
		subwin_y = wy;			// full size
	
	// figure out how many subframes we need to
	// render
	nSubFramesX = (wx + subwin_x - 1)/ subwin_x; // across
	nSubFramesY = (wy + subwin_y - 1) / subwin_y; // and down
	
	nSubFrame = nSubFramesX * nSubFramesY; // total number of subframes
	
}

void
OpenGLrenderer::apply_object_transform( ObjNode *pObjNode)
	// 
	// 
	// apply object-node transformation to an object;
	// 
	// this moves from object space to world space
	// 
{
	// translate the centroid (i.e. object origin)
	// in world space to desired location
	glTranslated(pObjNode->x.get_value(),
				 pObjNode->y.get_value(),
				 pObjNode->z.get_value());
	
	// scale from object space into world space
	glScalef(pObjNode->xscale.get_value(),
			 pObjNode->yscale.get_value(),
			 pObjNode->zscale.get_value());
	
	// rotate object around centroid
	glRotatef(pObjNode->zrot.get_value(), 0.0, 0.0, 1.0);
	glRotatef(pObjNode->yrot.get_value(), 0.0, 1.0, 0.0);
	glRotatef(pObjNode->xrot.get_value(), 1.0, 0.0, 0.0);
	
}

void
OpenGLrenderer::apply_centroid_transform( Obj *pObj)
	// 
	// 
	// apply the centroid transformations to the current matrix on the stack;
	// 
	// this moves from model space to object space
	// 
{
	
	PolyObject1 *pPolyObj = (PolyObject1 *)pObj;
	
	// define the orientation of object space
	glRotatef(pPolyObj->zrot.get_value(), 0.0, 0.0, 1.0);
	glRotatef(pPolyObj->yrot.get_value(), 0.0, 1.0, 0.0);
	glRotatef(pPolyObj->xrot.get_value(), 1.0, 0.0, 0.0);
	
	// scale from model space into object space
	glScalef(pPolyObj->xscale.get_value(),
			 pPolyObj->yscale.get_value(),
			 pPolyObj->zscale.get_value());
	
	// make specified model coords be the new model
	// origin
	glTranslated(-pPolyObj->x.get_value(),
				 -pPolyObj->y.get_value(),
				 -pPolyObj->z.get_value());
	
	
}

void
OpenGLrenderer::setup_camera( ObjNode *incam,
							 double fov, double widthfov,
							 double near, double far,
							 int ndxFrame)
	// 
	// 
	// setup the viewing transform for the camera
	// 
	// 
{
	double fr_top, fr_bottom, fr_left, fr_right;
	double x1,y1,z1,x2,y2,z2,ux,uy,uz;
	
	glMatrixMode( GL_PROJECTION); // specify projection matrix
	glLoadIdentity();			  // initialize
	
	get_frustum( fov, widthfov, near, 
				wx, wy, subwin_x, subwin_y,
				fr_top, fr_bottom, fr_left, fr_right,
				ndxFrame, nSubFramesX, nSubFramesY);
	
	glFrustum( fr_left, fr_right, fr_bottom, fr_top, near, far);
	
	glMatrixMode( GL_MODELVIEW);
	glLoadIdentity();
	
	incam->get_object_pos( x1, y1, z1);
	
	// get camera's look vector
	incam->get_look_vector(x2, y2, z2);
	
	// get the up vector
	incam->get_up_vector( ux, uy, uz); // equivalent to the following line:
	
	// roll is around x axis
	gluLookAt(x1,y1,z1,x2,y2,z2,ux,uy,uz);
	
	
}

void
print_matrix( double *p)
{
	int i;
	
	for (i = 0; i < 4; i++) {
		fprintf( stderr, "%7.3f %7.3f %7.3f %7.3f\n",
				p[i+0], p[i+4], p[i+8], p[i+12]);
	}
	fprintf( stderr, "\n");
}

void print_proj_matrix(void)
{
	double m[16];
	
	glGetDoublev( GL_PROJECTION_MATRIX, m);
	print_matrix( m);
}

void print_modelview_matrix(void)
{
	double m[16];
	
	glGetDoublev( GL_MODELVIEW_MATRIX, m);
	print_matrix( m);
}


GLubyte *
OpenGLrenderer::convert_image_to_opengl_texture( 
												Image *pImg, int &newWidth, int &newHeight)
{
	int i, j, origWidth, origHeight, nBands;
	uchar r, g, b;
	unsigned char *pb;
	GLubyte *pOrigData = NULL;


    // would like to add member to Image class to convert from left
    // to right coordinats systems and vise versa

	
	// 
	// allocate the OGL texture
	// 
	pImg->get_res( &origWidth, &origHeight, &nBands);
	if ((pOrigData = 
//		 (GLubyte *)malloc( origWidth * origHeight * nBands))
		 (GLubyte *)malloc( origWidth * origHeight * 3))
		== NULL) {
		fprintf( stderr, "Error: can not malloc pOrigData\n");
		exit( 0);
	}
	
	
	fprintf( stderr, "\nConverting image to OpenGL format...");
	
	// this flips the input Image and stuffs the
	// inverted image into the texture; flipping is
	// needed due to OGL's right-handed coord system

    // do not do this any more. see note re: Image class above

	
	if (pImg->preferred_data_type() == UCHAR_DATA) { 
		
		for (i = 0; i < origHeight; i++) { // for each increasing row in
			// output image

        // get corresponding row in input image, starting from last row
        // and decreasing
   
/******************
			pb = (((unsigned char *)pOrigData) + // get corresponding row in
				  (origHeight-i-1)*origWidth*3); // input image, starting from
******************/

            // get rows from input image
            pb = (((unsigned char *)pOrigData) + i*origWidth*3);
			
			for (j = 0; j < origWidth; j++) {
				pImg->get_color( j, i, &r, &g, &b);
				*pb++ = (uchar) r;
				*pb++ = (uchar) g;
				*pb++ = (uchar) b;
				//			pImg->get_color( i, origWidth - j - 1, &r, &g, &b);
				//			*(pOrigData + j*origWidth + 3*i + 0) = (GLubyte) r;
				//			*(pOrigData + j*origWidth + 3*i + 1) = (GLubyte) g;
				//			*(pOrigData + j*origWidth + 3*i + 2) = (GLubyte) b;
			}
		}
	}
	else {
		FATAL_ERROR( "can only texture map UCHAR_DATA at this time, sorry!");
	}

								// we do not scale the image here at all
	newWidth = origWidth;
	newHeight = origHeight;

	return pOrigData;
}

void
OpenGLrenderer::get_ready_to_texture_map( int nImgWidth, int nImgHeight, 
										 GLubyte *pTexture)
{
	glPixelStorei(GL_UNPACK_ALIGNMENT, 1);
	
#if 0
	glTexImage2D(GL_TEXTURE_2D, 0, 3, nImgWidth, 
				 nImgHeight, 0, GL_RGB, GL_UNSIGNED_BYTE, 
				 pTexture);
#else
	int gluErr;
	if (gluErr = gluBuild2DMipmaps( GL_TEXTURE_2D,
						  3,
						  nImgWidth,
						  nImgHeight,
						  GL_RGB,
						  GL_UNSIGNED_BYTE,
						  pTexture)) {
		fprintf( stderr,
				"Unable to create mipmaps!: %s\n", gluErrorString( gluErr));
	}
#endif
	
#if 0
	glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
	glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
	//    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
	//    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
#else
	// trying this to see if spherical mapping
	// seams get handled automatically w/o
	// overlapping the seam explicitly
	glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
	glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);
	glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
	glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_NEAREST);
#endif
	glTexEnvf(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_DECAL);
	glEnable(GL_TEXTURE_2D);
}

void OpenGLrenderer::free_opengl_compatible_textures(void) 
	
{
#if 0
	int i;
	GLubyte **pp;
	
	if (ppTextures == NULL)
		return;
	
	pp = ppTextures;			// start at beg of list
	for (i = 0; i < nObjectsInScene; i++, pp++) {
		
		if (*pp != NULL)			// free the OGL-compatible texture, if any
			free (*pp);
	}
#else
	int i;
	TextureType *pT = pTextures;
	
	if (pT == NULL)
		return;
	
	for (i = 0; i < nObjectsInScene; i++, pT++) {
		
		if (pT->pTexture == NULL)
			free( pT->pTexture);
	}
	
	
	
#endif
}
