// glrend.C
// glrend.C 1.23 01/07/27 12:49:45

// This code is dependent on Irix GL and is not portable
// off the SGI platform.  For easy compilation and archiving,
// this code is enclosed in #ifdef to prevent any GL code
// from getting loose on other machines
#ifdef DO_GL

#include "grape/glrend.h"
#include "grape/poly_object_1.h"
#include "grape/camera.h"
#include "math.h"

// The GL renderer class.
//

#define RGB_WHITE 0x00ffffff
#define RGB_RED   0x0000ff
#define RGB_GREEN 0x00ff00
#define RGB_BLUE  0xff0000
#define RGB_BLACK 0x000000

#define TORADIANS  0.017453293 /* PI/180 to convert to radians */
#define TODEGREES  57.29578    /* 180/PI to convert to degrees */

Matrix Identity = { 1, 0, 0, 0,  
                    0, 1, 0, 0,  
                    0, 0, 1, 0,  
                    0, 0, 0, 1 };



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
int GLrenderer::process_object(int k)
{
int num_polys;
int num_points_in_poly;
int point_index;
int color_index;
ObjNode *tmpObjectNode;
PolyObject1     *tmpObject;
int     i,j;
double  vert[3];
double  red,green,blue;
unsigned	long    color_value;
GL_Object  sgi_object;
int     dummy;

      tmpObjectNode = scene->get_obj(k);           /* get object node pointer */
      tmpObject = (PolyObject1 *)(tmpObjectNode->get_object()); /* get object pointer */

      if(tmpObject->get_type() == POLY_V1) 
         {
         // int num_colors = tmpObject->get_num_colors();
         // int num_edges  = tmpObject->get_num_edges();
         num_polys  = tmpObject->get_num_faces();

         sgi_object = genobj();
         My_GL_Objects[k] = sgi_object;
         tmpObject->set_globject(sgi_object);
         makeobj(tmpObject->get_globject()); /* create GL object */

         for(i=0; i<num_polys; i++)  /* loop for number of faces in object */
            {
            num_points_in_poly = tmpObject->get_num_verts(i);

               /* set the color */
               color_index = tmpObject->get_pcolor(i);
               tmpObject->get_color(color_index,red,green,blue);
               red   = red * 255.0;   /* go from 0-1 to 0-255 */
               green = green * 255.0; /* go from 0-1 to 0-255 */
               blue  = blue * 255.0;  /* go from 0-1 to 0-255 */
//               color_value = 0;
               color_value = 0xff000000;
               color_value += ((long)blue  * (long)256 * (long)256);
               color_value += ((long)green * (long)256);
               color_value += (long)red;
               cpack(color_value);    /* store GL color for poly */
            bgnpolygon();
               for(j=0; j<num_points_in_poly; j++)
                  {
                  point_index = tmpObject->get_vert(i,j);
                  tmpObject->get_point(point_index,dummy,vert[0],vert[1],vert[2]);
                  v3d(vert);  /* set the vertex */
                  } /* for j */
            endpolygon();

            } /* for i */
         closeobj(); /* close this GL object */
         } /* if polygon object */

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
int GLrenderer::set_scene(Scene *s, ImgInfo *inimg)
{
int windowx=200,windowy=200;
double near,far;
int num_objects;
ObjNode *tmpObjectNode;
int     k;
ImgInfo	*tmpImgInfo;

	if(inimg) {
		tmpImgInfo = inimg;
	} else {
		tmpObjectNode = s->get_camera();
		tmpImgInfo = ((Camera *)(tmpObjectNode->get_object()))->get_img_info();
	}
fprintf(stderr,"ready to prepare image info\n");
tmpImgInfo->prepare();

	if(gl_win_id)winclose(gl_win_id);

	if(tmpImgInfo->get_output_method() != OUT_GLWIN || !(tmpImgInfo->get_GLwindow())) {
fprintf(stderr,"Creating gl window in gl renderer because method = %d and windowid = %d.\n", tmpImgInfo->get_output_method() , tmpImgInfo->get_GLwindow());
		near = 1.0;
		far = getgdesc(GD_ZMAX);

		tmpImgInfo->get_res(&windowx, &windowy);

		prefsize(windowx,windowy);     /* the size of the window */
		foreground();      /* keep GL from exiting this process      */
		glcompat(GLC_ZRANGEMAP,1);
		gl_win_id = winopen("test");     /* Give title of window */
		subpixel(TRUE);    /* added for subpixel accuracy. So polys don't wabble*/
		RGBmode();         /* Set to RGB mode instead of default Color Map mode */
		doublebuffer();    /* we will double buffer for smooth animations */
		gconfig();         /* Let system know of change */
		zbuffer(1);        /* enable ZBuffer */
		lsetdepth((long)near, far); /* set clip planes to min,max depths */
		mmode(MVIEWING);   /* a matrix mode */
		loadmatrix(Identity); /* initialize viewing matrix */
		cpack(RGB_BLACK);  /* clear both buffers. */
		czclear(0,far);
		swapbuffers(); 
		czclear(0,far);

		viewport(0,windowx-1,0,windowy-1);  
		window(-(windowx-1)/2.0, (windowx-1)/2.0, 
		       -(windowy-1)/2.0, (windowy-1)/2.0, near, far);

		czclear(0,far);    /* clear the Z and image buffers */
	} else {
//		gl_win_id = tmpImgInfo->get_GLwindow();
	}  // endif

	scene = s;

	num_objects = s->get_num_objects(); /* get the number of objects */
	if(My_GL_Objects)free(My_GL_Objects);
	My_GL_Objects = (GL_Object *)malloc(num_objects * sizeof(GL_Object));

	for(k=0; k<num_objects; k++) {    /* loop and create a GL object for each */
		process_object(k);	/* optional as render will do it if necessary */
	}
	last_rendered = CHANGE_COUNTER++;
	if(parray)free(parray);
	parray = (unsigned long *)malloc(windowx * windowy * sizeof(long));

	return(0); // means okay

} /* set_scene */



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
int GLrenderer::render(ObjNode *incam, ImgInfo *inimg)
{
Angle roll;
double near,far;
double fov,aspect;
double x1,y1,z1,x2,y2,z2;
double	d, widthfov;
//int	return_code;
int windowx=200,windowy=200;
int     num_objects;
int     k;
Obj     *tmpObject;
ObjNode *tmpObjectNode;
long	temp_gl_win;


   if(!scene) {
	fprintf(stderr,"Whoops - Do not call render before calling set_scene.\n");
	return(FALSE);
   }

   x1 = incam->x.get_value();
   y1 = incam->y.get_value();
   z1 = incam->z.get_value();
   incam->get_look_vector(x2, y2, z2);
   x2 += x1;
   y2 += y1;
   z2 += z1;
   roll = incam->xrot.get_value() * 10;
// fprintf(stderr," Camera at x=%f y=%f z=%f  look vector x=%f y=%f z=%f roll=%d\n", x1, y1,z1,x2,y2,z2, roll);

   near = 1.0;
   far = getgdesc(GD_ZMAX);
   fov = ((Camera *)(incam->get_object()))->get_fov();

	inimg->prepare();

	inimg->get_res(&windowx, &windowy);

   temp_gl_win = winget();
   if(gl_win_id) {
	winset(gl_win_id);
   } else {
	winset(inimg->get_GLwindow());
   }
   winpop();	// put window in front to prevent obscuration

   d = (windowy / 2.0) / (tan((fov*TORADIANS)/2.0));
   widthfov = atan((windowx/2.0) / d);
   widthfov *= TODEGREES;
   aspect = widthfov / (fov / 2.0);

   loadmatrix(Identity);
   perspective((int)(fov * 10),(float)aspect,near,far); 
   lookat(x1,z1,y1,x2,z2,y2,(Angle)roll);

   num_objects = scene->get_num_objects(); /* get the number of objects */
   czclear(0,far);

   for(k=0; k<num_objects; k++) /* loop and display a GL object for each */
      {
      tmpObjectNode = scene->get_obj(k);           /* get object node pointer */

      tmpObject = tmpObjectNode->get_object(); /* get object pointer */
      if (tmpObject->get_type() == POLY_V1) {  // if object is proper polygon type
         if (tmpObject->get_changed() > last_rendered) /* if object has changed */
            {
fprintf(stderr,"Reprocessing object %d\n", k);
		process_object(k);
            }

            pushmatrix();  /* save top of stack */
	    // set object position
            translate(tmpObjectNode->x.get_value(),
                      tmpObjectNode->y.get_value(),
                      tmpObjectNode->z.get_value());
		rot(tmpObjectNode->xrot.get_value(), 'x');
		rot(tmpObjectNode->yrot.get_value(), 'y');
		rot(tmpObjectNode->zrot.get_value(), 'z');
            scale(tmpObjectNode->xscale.get_value(),
                  tmpObjectNode->yscale.get_value(),
                  tmpObjectNode->zscale.get_value());
	    // adjust by object centroid
            translate(tmpObject->x.get_value(),
                      tmpObject->y.get_value(),
                      tmpObject->z.get_value());
		rot(tmpObject->xrot.get_value(), 'x');
		rot(tmpObject->yrot.get_value(), 'y');
		rot(tmpObject->zrot.get_value(), 'z');
            scale(tmpObject->xscale.get_value(),
                  tmpObject->yscale.get_value(),
                  tmpObject->zscale.get_value());

            callobj(My_GL_Objects[k]);    /* draw the object */
            popmatrix();   /* restore top of stack */

            }
      else 
         {
//         fprintf(stderr,"Object %d is not a POLY_V1 object\n", k);
         }
      } /* for k */

// dispose of image properly
	uchar	*pbyte;
	uchar	*rbyte, *gbyte, *bbyte, *abyte;
	int	i,j;
	ImageData	*temp_dat;
	long		alpha_bits;
	if(inimg->get_output_method() == OUT_FILL_IMG) {
		alpha_bits = getgconfig(GC_BITS_ALPHA);
		// lrectread then loop to put in image data (put in subroutine later
		pbyte = (uchar *)parray;
		readsource(SRC_BACK);
		i = lrectread(0, 0, windowx-1, windowy-1, parray);
		temp_dat = inimg->get_image(0)->get_data();
		if(temp_dat->preferred_data_type() == UCHAR_DATA) {
			// fast byte transfers
			if(inimg->get_image_flag()) {
				rbyte = ((ucharData *)temp_dat)->get_data(RED_BAND);
				gbyte = ((ucharData *)temp_dat)->get_data(GREEN_BAND);
				bbyte = ((ucharData *)temp_dat)->get_data(BLUE_BAND);
			} else {
				rbyte = NULL;
				gbyte = NULL;
				bbyte = NULL;
			}
			if(inimg->get_alpha_flag()) {
				abyte = ((ucharData *)temp_dat)->get_data(ALPHA_BAND);
			} else {
				abyte = NULL;
			}
			for(i=windowy-1; i>=0; i--) {
				pbyte = (uchar *)(parray + i*windowx);
				if(abyte) {
				    if(alpha_bits) {
					for(j=0; j<windowx; j++) {
						*abyte++ = *pbyte;
						pbyte += 4;
					}
				    } else {
					for(j=0; j<windowx; j++) {
						*abyte++ = 255;
					}
				    }
				}
				pbyte = (uchar *)(parray + i*windowx);
				pbyte += 1;
				if(bbyte) {
					for(j=0; j<windowx; j++) {
						*bbyte++ = *pbyte;
						pbyte += 4;
					}
				}
				pbyte = (uchar *)(parray + i*windowx);
				pbyte += 2;
				if(gbyte) {
					for(j=0; j<windowx; j++) {
						*gbyte++ = *pbyte;
						pbyte += 4;
					}
				}
				pbyte = (uchar *)(parray + i*windowx);
				pbyte += 3;
				if(rbyte) {
					for(j=0; j<windowx; j++) {
						*rbyte++ = *pbyte;
						pbyte += 4;
					}
				}
			}
		} else {
			// slow type cast data transfers
			for(i=windowy-1; i>=0; i--) {
				pbyte = (uchar *)(parray + i*windowx);
				for(j=0; j<windowx; j++) {
					if(inimg->get_alpha_flag()) {
						if(alpha_bits) {
							temp_dat->set(*pbyte++,j,i,ALPHA_BAND);
						} else {
							temp_dat->set(255,j,i,ALPHA_BAND);
						}
					} else {
						pbyte++;
					}
					if(inimg->get_image_flag()) {
						temp_dat->set(*pbyte++, j, i, RED_BAND);
						temp_dat->set(*pbyte++, j, i, GREEN_BAND);
						temp_dat->set(*pbyte++, j, i, BLUE_BAND);
					} else {
						pbyte += 3;
					}
				}
			}
		}
		// need to do z buffer here
		if(inimg->get_z_flag()) {
			long	*pfloat;
			int	zband;
			double	dist;
			readsource(SRC_ZBUFFER);
			i = lrectread(0, 0, windowx-1, windowy-1, parray);
			if(inimg->get_image_flag() || inimg->get_alpha_flag()) {
				zband = 1;
			} else {
				zband = 0;
			}
			temp_dat = inimg->get_image(zband)->get_data();
			for(i=windowy-1; i>=0; i--) {
				pfloat = (long *)(parray + i*windowx);
				for(j=0; j<windowx; j++) {
/*
	      				*pfloat = -(((far*near*(farvp-nearvp)) / (far - near)) /
					 (*pfloat - (((far+near)*(farvp-nearvp))/(2.0*(far-near))) -((farvp+nearvp)/2.0)));
*/
	      				dist = -(((far*near*(far-near)) / (far - near)) /
					 (*pfloat++ - (((far+near)*(far-near))/(2.0*(far-near))) -((far+near)/2.0)));
					temp_dat->set(dist,j,windowy-1-i,DEFAULT_BAND); // needs Zareh's xlation
				}
			}
		}
	} else if(inimg->get_output_method() == OUT_FILE) {
		// lrectread then loop to put in image data then image write
	} else if(inimg->get_output_method() == OUT_XWIN) {
		// lrectread then loop to put in x window or pixmap
	} else if(inimg->get_output_method() == OUT_GLWIN) {
		// probably do nothing
	} else if(inimg->get_output_method() == OUT_NONE) {
		// definitely do nothing
	}

   swapbuffers();  

	last_rendered = CHANGE_COUNTER;

   if(temp_gl_win > 0)winset(temp_gl_win);

   return(0);	// means okay

} /* render */

#endif
