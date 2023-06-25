// oglrend.h

#ifndef _OGLREND_H_
#define _OGLREND_H_

#include <X11/Intrinsic.h>

#include "grape/renderer.h"

#ifdef DO_GL
#include <gl/gl.h>
#else
#ifndef GL_Boolean
#define GL_Boolean Boolean
#endif
#ifndef GL_Object
#define GL_Object unsigned int
#endif
#endif

#include <GL/glx.h>
#include <X11/Xlib.h>


typedef short Angle;

#define OGL_Object int

enum OGLDebugT {
	DbgOff = 0x00000000,
	DbgDumpObj = 0x00000001,		/* dump object when processing it */
    DbgAll = 0xffffffff			/* everything */
};


typedef struct {
	GLubyte *pTexture;
	int w, h;
} TextureType;

// The class for a OpenGL renderer.
// 
class OpenGLrenderer : public Renderer
   {
   private:

      OGL_Object *My_OGL_Objects; /* array of display list id's, one for */
								  /* each object in the scene*/
	  TextureType *pTextures;		/* array of ptrs to textures, parallel to */
								/* 'My_OGL_Objects' */
	  int nObjectsInScene;		/* num elements in the above 2 arrays */
      int	last_rendered;

	  Boolean convert_to_texture;	// should image be converted to GL texture 
      
      Angle WFUpToRoll(double ex,double ey,double ez,
                       double gx,double gy,double gz,
                       double ux,double uy,double uz);
      void WFRollToUp( double roll, double ex, double ey, double ez,
                              double gx, double gy, double gz, double
                              *ux, double *uy, double *uz);

	  void pixmap_to_image( ImgInfo *inimg,
						   double near, double far,
						   int iDataStride);

	  void render_context_to_image( ImgInfo *inimg,
								   double near, double far,
								   int iDataStride,
								   int ndxImage);


      void create_test_xwin( int w, int h, Display *dpy, XVisualInfo *vi);

	  void determine_render_size( Display *dp);
	  
      int	process_object(int);

	  void
	  apply_object_transform( ObjNode *pObjNode);

	  void
	  apply_centroid_transform( Obj *pObj);

	  void
	  setup_camera( ObjNode *incam,
				   double fov, double widthfov,
				   double near, double far,
				   int ndxFrame);
	  

      int wx, wy;				/* window width and height */
      unsigned long	*parray;	/* array into which rendered data may be read */

      Display *pXdisplay;		/* for use when rendering into pixmaps */
      XVisualInfo *pVis;		/* ptr to visual info */
      Pixmap XPixMap;			/* X pixmap id */
      GLXPixmap GLXPixMap;		/* OGL pixmap id */
      GLXContext glx_context;	/* OGL X rendering context */
      GL_Boolean bPixMap;       /* rendering to pixmap? */

      GL_Boolean bTestWin;      /* doing test window? */
      GL_Boolean bForceFastXfers; /* force fast pixmap to image xfers? */
      GL_Boolean bUseAlpha;     /* use alpha channel? */
	  GL_Boolean bUseDepth;		/* use depth buffer? */
      GL_Boolean bSwapOrder;    /* swap rgba order? */
	  GL_Boolean bNoNormals;	/* use normals in display list? */
      Window win;               /* test window */
      GC xgc;                   /* test win graphics context */

	  GLfloat mat_specular[4];
	  GLfloat mat_shininess[1];
	  GLfloat light_pos[4];
	  GL_Boolean bForceNormalCalcs;		/* forcing normall calcs? */
      
	  OGLDebugT eDebug;			/* debug flags */

	  int nLights;				/* number of lights that this renderer is */
								/* currently handling */

	  int subwin_x, subwin_y;
	  int nSubFrame;			/* if rendering subframes and compositing */
	  int nSubFramesX, nSubFramesY;
	  double dNearClipPlane, dFarClipPlane;
	  char *szHostName;			/* for display's name when opening X wins */

   protected:

   public:
      
      void display(void);

      int set_subwins(int sub_x, int sub_y) { 
                        subwin_x = sub_x; subwin_y = sub_y; 
                        return TRUE; }

      int set_scene(Scene *s, ImgInfo * =NULL);

      int render(ObjNode *, ImgInfo *);
      int render(void) {
		if(scene) {
			return(render(scene->get_camera(), 
				((Camera *)(scene->get_camera()->get_object()))->get_img_info()));
		} else {
			return(FALSE);
		}
	}

      void enable_test_win(void) {
          bTestWin = GL_TRUE;
      }

      void force_fast_transfers(void) {
          bForceFastXfers = GL_TRUE;
      }
      void use_alpha(void) {
          bUseAlpha = GL_TRUE;
      }
      void swap_rbga_order(void) {
          bSwapOrder = GL_TRUE;
      }
	  void disable_normal_usage(void) {
		  bNoNormals = GL_TRUE;
	  }

	  void set_debug_level( OGLDebugT flag);
	  OGLDebugT get_debug_level( void);
	  void set_debug_light_pos( float x, float y, float z, float w) {
		  light_pos[0] = x;
		  light_pos[1] = y;
		  light_pos[2] = z;
		  light_pos[3] = w;
	  }
	  void set_debug_specular( float x, float y, float z, float w) {
		  mat_specular[0] = x;
		  mat_specular[1] = y;
		  mat_specular[2] = z;
		  mat_specular[3] = w;
	  }
	  void set_debug_shininess( float s) {
		  mat_shininess[0] = s;
	  }

	  void force_normal_calcs( void) {
		  bForceNormalCalcs = GL_TRUE;
	  }
	  GL_Boolean should_we_force_normal_calcs( void) {
		  return bForceNormalCalcs;
	  }

	  int process_light_object( void *p);
	  int process_poly_object( void *p, int nObjectNdx);

	  void do_convert_to_texture(Boolean tf) { convert_to_texture = tf; }
		  
	  int init_render_to_pixmap( void);

	  int init_render_to_xwin( Display *pXD,
							  Window Xwin);

	  int init_render_to_xwin( void);

								/* prepare an Image to something suitable */
								/* for OpengGL: call this after loading an */
								/* Image */
	  GLubyte *convert_image_to_opengl_texture(
		   Image *pImg, int &newWidth, int &newHeight);

								/* After calling */
								/* convert_image_to_opengl_texture(), one must */
								/* call this for each display list that it */
								/* textured */
	  void get_ready_to_texture_map(
		   int nImgWidth, int nImgHeight, GLubyte *pTexture);

	  void free_opengl_compatible_textures(void);

	  double get_near_clip_plane(void) { return dNearClipPlane;}
	  double get_far_clip_plane(void) { return dFarClipPlane;}

	  char *set_hostname_for_display( char *pHost = NULL) {
		  if (pHost != NULL) {
			  if ((szHostName = (char *)malloc( strlen( pHost) + 16)) != NULL) {
				  strcpy( szHostName, pHost);
				  strcat( szHostName, ":0.0");
			  }
		  }
		  else
			  szHostName = pHost;
		  return szHostName;
	  }
	  char *get_hostname_for_display(void) { return szHostName;}
	  

	  OpenGLrenderer(void) {
		  set_hostname_for_display();
		  scene = NULL;
		  last_rendered = 0;
		  wy = wy = 0;
		  pXdisplay = NULL;
		  pVis = NULL;
		  My_OGL_Objects = NULL;
		  pTextures = NULL;
		  nObjectsInScene = 0;
		  parray = NULL;
		  bPixMap = GL_FALSE;
		  bTestWin = GL_FALSE;
		  bForceFastXfers = GL_FALSE;
		  bUseAlpha = GL_FALSE;
		  bUseDepth = GL_FALSE;
		  bSwapOrder = GL_FALSE;
		  bNoNormals = GL_FALSE;
		  bForceNormalCalcs = GL_FALSE;
		  set_debug_level( DbgOff);	/* don't output any debug info */
		  set_debug_light_pos( 1.0, 1.0, 1.0, 1.0);
		  set_debug_specular( 1.0, 1.0, 1.0, 1.0);
		  set_debug_shininess( 50.0);

	 	  convert_to_texture = True;

		  nLights = 0;

		  nSubFrame = 1;		/* assume can render whole image */
		  nSubFramesX = nSubFramesY = 1;
		  subwin_x = subwin_y = 200; /* size of subframes, if used */
		  dNearClipPlane = dFarClipPlane = -1.0; /* invalid */
	  }
	  ~OpenGLrenderer(void) {
		if(My_OGL_Objects)free(My_OGL_Objects);
		free_opengl_compatible_textures();
		if (bPixMap == GL_TRUE) {
			XFreePixmap( pXdisplay, XPixMap);
			glXDestroyGLXPixmap( pXdisplay,  GLXPixMap);
			glXDestroyContext( pXdisplay, glx_context);
			XCloseDisplay( pXdisplay);
		}
		if (szHostName != NULL)
			free( szHostName);
	}

  }; /* Class */

								/* miscellaneous functions */

XVisualInfo *
MyChooseXVisual( Display *pXdisplay, GL_Boolean bDoubleBuffer,
				GL_Boolean bAlpha, GL_Boolean bDepth);

#endif
