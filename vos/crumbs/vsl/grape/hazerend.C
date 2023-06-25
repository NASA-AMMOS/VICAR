// hazerend.C 1.7 01/07/27 12:49:46
/****************************************************************************
*
* Module name: hazerend.C
*
* Module purpose: This class contains code for implementing various haze
*                 models. Currently Zareh's haze model used in Surveyor and 
*                 Render is implemented.
*
* Module History: 9/13/95 Created.
*
* MODULE CONTENTS:
*
* Global variables/defines used: 
*
* Created by: Zareh Gorjian.
*
* Bugs, possible improvements: 
*
* Special compilation requirements: 
*
* This module IS NOT machine specific.
*
****************************************************************************/
#include "grape/hazerend.h"
#include "grape/camera.h"
#include "math.h"

// The haze renderer class.
//

#define X 0			/* array location of x-coordinate */
#define Y 1			/*                   y-coordinate */
#define Z 2			/*                   z-coordinate */

#define MIN(a, b)       ((a) < (b) ? (a) :  (b))
#define MAX(a, b)       ((a) > (b) ? (a) :  (b))

/* The following macros define the vector functions:

   Vec A, B, C;
   double a, b;

   double VecLen(A)		vector length: returns|A|
   double VecDot(A, B)		dot product: returns A.B
   VecCopy(A, B)		vector copy: B = A
   VecNegate(A, B)		negative: B = -A
   VecAdd(A, B, C)		addition: C = A + B
   VecSub(A, B, C)		subtraction: C = A - B
   VecComb(a, A, b, B, C)	linear combination: C = aA + bB
   VecAddS(a, A, B, C)		add scalar multiple: C = aA + B
   VecUnit(A, B)		vector unitize: B = A/|A|
   VecCross(A, B, C)		vector product: C = AxB
   */

#define VecLenSq(A)	(A[X]*A[X] + A[Y]*A[Y] + A[Z]*A[Z])
#define VecLen(A)	(double)sqrt((double)VecLenSq(A))
#define VecNegate(A, B)	(B[X] = -A[X], B[Y] = -A[Y], B[Z] = -A[Z])
#define VecDot(A, B)	(A[X]*B[X] + A[Y]*B[Y] + A[Z]*B[Z])
#define VecCopy(A, B)	(B[X]=A[X], B[Y]=A[Y], B[Z]=A[Z])
#define VecAdd(A, B, C)	(C[X] = A[X]+B[X], C[Y] = A[Y]+B[Y], C[Z] = A[Z]+B[Z])
#define VecSub(A, B, C)	(C[X] = A[X]-B[X], C[Y] = A[Y]-B[Y], C[Z] = A[Z]-B[Z])
#define VecComb(a, A, b, B, C) 				\
			(C[X] = (a)*A[X] + (b)*B[X],	\
			 C[Y] = (a)*A[Y] + (b)*B[Y],	\
			 C[Z] = (a)*A[Z] + (b)*B[Z])
#define VecAddS(a, A, B, C) 				\
			(C[X] = (a)*A[X] + B[X],	\
			 C[Y] = (a)*A[Y] + B[Y],	\
			 C[Z] = (a)*A[Z] + B[Z])
#define VecUnit(A, B)	{ 				\
			   double len; 			\
			   len  = VecLen(A); 		\
			   B[X] = A[X]/len; 		\
			   B[Y] = A[Y]/len; 		\
			   B[Z] = A[Z]/len; 		\
			   }
#define VecCross(A, B, C)				\
			(C[X] = (A[Y]*B[Z] - B[Y]*A[Z]),\
			 C[Y] = (A[Z]*B[X] - B[Z]*A[X]),\
			 C[Z] = (A[X]*B[Y] - B[X]*A[Y]))
#define VecScale(a, A, B)				\
			(B[X] = a*A[X],			\
			 B[Y] = a*A[Y],			\
			 B[Z] = a*A[Z])
#define VecInvScale(a, A, B)				\
			(B[X] = A[X]/a,			\
			 B[Y] = A[Y]/a,			\
			 B[Z] = A[Z]/a)

#define VecMult(A, B, C)				\
			(C[X] = A[X]*B[X],		\
			 C[Y] = A[Y]*B[Y],		\
			 C[Z] = A[Z]*B[Z])
#define VecInterp(n, a, A, B, C)			\
			(C[X] = A[X] + (a) * (B[X] - A[X])/((n) - 1),	\
			 C[Y] = A[Y] + (a) * (B[Y] - A[Y])/((n) - 1),	\
			 C[Z] = A[Z] + (a) * (B[Z] - A[Z])/((n) - 1))

typedef double Vec[3];		/* a 3-vector */
typedef Vec Point;		/* xyz point data type */
typedef double Angle;		/* angle measurements */
typedef Vec Color;		/* a rays color */

/*
** Variables used in this module.
*/
double aspect; /* aspect ratio height/width */
double fov;
Vec Camera_D;
Vec Camera_UP;
Vec Camera_P;
Vec VP_U;
Vec VP_V;
Vec VP_P;
Vec VP_N;
Vec Ray_P;
Vec Ray_D;
Vec Ray_UP;
Vec Ray_UD;

int    HazeMode;
double HazeDistExp;
int    HazeSymmetric;
double HazeSkyPitch;
double HazeGroundPitch;
double HazeStartDist;
double HazeEndDist;
double HazeStartPerc;
double HazeEndPerc;
double HazeLength;             /* HazeEndDist - HazeStartDist ZG */
double HazePercLength;         /* HazeEndPerc - HazeStartPerc ZG */
Color  HazeColor;              /* Color of haze */
Color  SkyColor;               /* Color of sky  */

/****************************************************************************
*
* Function name: set_scene
*
* Function purpose: To take a scene pointer.
*
* Function History: 9/14/95  Created.
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
int ZarehHazeRenderer::set_scene(Scene *s, ImgInfo *)
{
   scene = s;

   return(0); // means okay

} /* set_scene */


/****************************************************************************
*
* Function name: AngleFrac
*
* Function purpose: Given the pitch angle of the ray compute the haze 
*                   intensity.
*
* Function History: 9/14/95  Created.
*
* Parameters: 
*
* Return value: double : Haze intensity.
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
double ZarehHazeRenderer::AngleFrac(void)
{
double pitch;   /* pitch angle of ray */

   if (HazeSymmetric == 1)
      {
      if (Ray_UD[Z] < 0.0) 
         pitch = pow(fabs((double)Ray_UD[Z]),HazeGroundPitch);
      else
         pitch = pow(fabs((double)Ray_UD[Z]),HazeSkyPitch);
      }
   else
      {
      if (Ray_UD[Z] < 0.0) 
         pitch = 0.0;  /* Do not modulate haze by pitch if looking down */
      else
         pitch = pow(fabs((double)Ray_UD[Z]),HazeSkyPitch);
      }

   return (1.0-pitch);

} /* AngleFrac */



/****************************************************************************
*
* Function name: HazeBlend
*
* Function purpose: To compute the haze intensity given distance to point to
*                   apply haze to, pitch of the ray passing through the 
*                   point and the color of the point..
*
* Function History: 9/14/95  Created.
*
* Parameters: dist (double) : input to function. The distance (range) to the
*                             point seen through the current pixel.
*             color (double array) : input/output. The color of the pixel is
*                   passed in and is modified by the haze.
*
* Return value: Haze color.
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
void ZarehHazeRenderer::HazeBlend(double dist, double *color)
{
double hzFrac, groundFrac;
Color  local_color;

   local_color[0] = *color;
   local_color[1] = *(color+1);
   local_color[2] = *(color+2);

   if ((HazeEndDist == 0.0) || (dist < HazeStartDist))  /* no haze */
      return;

   /*
   ** If dist is > HazeEndDist then hzFrac below will be > 1.
   */
   hzFrac = pow(((dist - HazeStartDist) / HazeLength) , HazeDistExp);
   if (hzFrac > 1.0) 
      hzFrac = 1.0;
   hzFrac = (HazePercLength * hzFrac) + HazeStartPerc;

   hzFrac *= AngleFrac(); /* modulate hzFrac with pitch of ray. */

   groundFrac = 1.0 - hzFrac;
   VecComb(hzFrac, HazeColor, groundFrac, local_color, local_color);

   *color     = local_color[0];
   *(color+1) = local_color[1];
   *(color+2) = local_color[2];

} /* HazeBlend */



/****************************************************************************
*
* Function name: SkyBlend
*
* Function purpose: To apply haze to the Sky (Sky is absence of terrain).
*
* Function History: 9/14/95  Created.
*
* Parameters: color (double array): output of function. The function outputs
*                                   the hazy sky color using this array.
*
* Return value: color
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
void ZarehHazeRenderer::SkyBlend(Color color)
{
double hzFrac;   /* fraction to be given to haze*/
double skyFrac;  /* fraction to be given to sky */

  hzFrac = AngleFrac();
  skyFrac = 1.0 - hzFrac;

  VecComb(hzFrac, HazeColor, skyFrac, SkyColor, color);

} /* SkyBlend */



/****************************************************************************
*
* Function name: render
*
* Function purpose: To render the haze.
*
* Function History: 9/14/95  Created.
*
* Parameters: 
*
* Return value: int
*
* Global variables/defines used: None.
*
* Module variables/defines used: None.
*
* Function method (description):  
*
* Created by: Zareh Gorjian & John Wright
*
* Bugs, possible improvements: 
*
* This function IS NOT machine specific.
*
*****************************************************************************/
int ZarehHazeRenderer::render(ObjNode *incam, ImgInfo *inimg)
{
double  fov,aspect;
double  up[3];
double  look[3];
int     windowx=200,windowy=200;
int     w,h,b;
int     x,y;
double  z, val;
ZarehHazeInfo *haze;
ImageData *temp_dat;
double  VP_dist;      /* distance to view plane from camera */
double  u,v;
Point   uvPoint;       /* actual location (x,y,z) of (u,v) */
Color   color;

   if(!scene) 
      {
      fprintf(stderr,"Whoops - Do not call render before calling set_scene.\n");
      return(FALSE);
      }

   // double x1 = incam->x.get_value();
   // double y1 = incam->y.get_value();
   // double z1 = incam->z.get_value();
   incam->get_look_vector(look[0], look[1],look[2]);
   incam->get_up_vector(up[0],up[1],up[2]);

   Camera_P[0] = incam->x.get_value();
   Camera_P[1] = incam->y.get_value();
   Camera_P[2] = incam->z.get_value();

   incam->get_up_vector(Camera_UP[0],Camera_UP[1],Camera_UP[2]);
   incam->get_look_vector(Camera_D[0], Camera_D[1],Camera_D[2]);

//  printf("Camera p is %lf,%lf,%lf Camera D is %lf,%lf,%lf Camera UP is %lf,%lf,%lf.\n",
//          Camera_P[0],Camera_P[1],Camera_P[2],
//          Camera_D[0],Camera_D[1],Camera_D[2],
//          Camera_UP[0],Camera_UP[1],Camera_UP[2]);
       
   haze = (ZarehHazeInfo *)scene->get_haze_info();
	if(!haze)return(FALSE);

   if (haze->get_haze_mode() != 1)
      {
      fprintf(stderr,"Wrong haze mode specified.\n");
      return(FALSE);
      }

   /*
   ** Get haze parameters.
   */
   haze->get_haze_color(&HazeColor[0],&HazeColor[1],&HazeColor[2]);
   haze->get_sky_color(&SkyColor[0],&SkyColor[1],&SkyColor[2]);
   HazeDistExp = haze->get_haze_dist_exp();
   HazeSymmetric = haze->get_haze_symmetric();
   HazeSkyPitch = haze->get_haze_sky_pitch();
   HazeGroundPitch = haze->get_haze_ground_pitch();
   HazeStartDist = haze->get_haze_start_dist();
   HazeEndDist = haze->get_haze_end_dist();
   HazeStartPerc = haze->get_haze_start_perc();
   HazeEndPerc = haze->get_haze_end_perc();

   HazeSkyPitch    = log(1.0 - HazeSkyPitch)    / log(0.5);
   HazeGroundPitch = log(1.0 - HazeGroundPitch) / log(0.5);
   HazeLength = HazeEndDist - HazeStartDist;
   HazeStartPerc /= 100.0;
   HazeEndPerc   /= 100.0;
   HazePercLength = HazeEndPerc - HazeStartPerc;

   fov = ((Camera *)(incam->get_object()))->get_fov();
// printf("fov is %lf.\n",fov);
   inimg->get_res(&windowx, &windowy);

/* Not needed. Taken from GL renderer in Surveyor.
   d = (windowy / 2.0) / (tan((fov*TORADIANS)/2.0));
   widthfov = atan((windowx/2.0) / d);
   widthfov *= TODEGREES;
   aspect = widthfov / (fov / 2.0);
*/
   aspect = 1.0; /* Need a way of getting aspect ratio into here */

   VecCopy(Camera_D, VP_N);            /* view plane normal (N) */

   VecCross(Camera_UP, VP_N, VP_U);    /* U is perp. to N and Up */
   VecUnit(VP_U, VP_U);                /* normalize U */

   VecCross(VP_U, VP_N, VP_V);         /* V is perp. to N and U */
   VecUnit(VP_V, VP_V);                /* normalize V */
   VecScale(aspect, VP_U, VP_U);       /* scale U vector for aspect ratio */

   /* calculate view plane origin */
   VP_dist = (aspect*windowx) / (2*tan(fov/2));
   VecAddS(VP_dist, Camera_D, Camera_P, VP_P);  /* VP center */
   VecAddS(-(windowy/2), VP_V, VP_P, VP_P);
   VecAddS(-(windowx/2), VP_U, VP_P, VP_P);


   /*
   ** Now compute the haze and output the image.
   */
      temp_dat = inimg->get_image(0)->get_data();

//if(img->get_image_flag() && img->get_alpha_flag() && img->get_z_flag()) {

      inimg->get_image(0)->get_data()->get_res(&w, &h, &b);
      for(x=0; x<w; x++) 
         {
         for(y=0; y<h; y++) 
            {
            z = inimg->get_image(1)->get_data()->get_double(x, y);
            /* alpha = inimg->get_image(0)->get_data()->get_double(x, y, ALPHA_BAND); */

            // compute haze weight

            VecCopy(Camera_P, Ray_P); /* ray origin, eye point */
	
            u = (double)x+0.5;
            v = (double)y+0.5;

            /*
            ** Now we figure out through what point (u,v) on the view 
            ** plane the ray will pass, and put the unitized vector from
            ** the eye to that point in ray->D, the direction vector.
            ** vp->P is the top left corner of the view plane.
            */
            VecAddS(u, VP_U, VP_P, uvPoint); /* Sample (horiz) loc of ray from */
                                             /* topleft of view plane */
            VecAddS(v, VP_V, uvPoint, uvPoint); /* add line (vert) offset to ray */
   
            VecSub(uvPoint, Ray_P, Ray_D); /* figuring out a ray from eye*/
                                           /* point to u,v coordinate on */	
                                           /* the screen (just computed).*/

            /*
            ** Copy the ray origin and direction to backup locations before they
            ** are scaled.
            */
            VecCopy(Ray_D,Ray_UD);
            VecUnit(Ray_UD,Ray_UD); /* unitize the unscaled direction vector */

            if (z > 1000000.0)
               {
               SkyBlend(color); 
               val = color[0];
               temp_dat->set(val, x, y, RED_BAND);

               val = color[1];
               temp_dat->set(val, x, y, GREEN_BAND);

               val = color[2];
               temp_dat->set(val, x, y, BLUE_BAND);
               }
            else
               {
	       color[0] = (double)temp_dat->get_double(x, y, RED_BAND);
	       color[1] = (double)temp_dat->get_double(x, y, GREEN_BAND);
	       color[2] = (double)temp_dat->get_double(x, y, BLUE_BAND);

//             printf("color at %d,%d is %lf,%lf,%lf.\n",x,y,color[0],color[1],color[2]);
               HazeBlend(z ,color);

               temp_dat->set(color[0], x, y, RED_BAND);
               temp_dat->set(color[1], x, y, GREEN_BAND);
               temp_dat->set(color[2], x, y, BLUE_BAND);
               }
            } /* for y */
         } /* for x */

//   last_rendered = CHANGE_COUNTER;

   return(0);	// means okay

} /* render */




int JohnHazeRenderer::set_scene(Scene *s, ImgInfo *)
{
   scene = s;

   return(0); // means okay

} /* set_scene */

int JohnHazeRenderer::render(ObjNode *incam, ImgInfo *inimg)
{
	double	pitch;
	double	theta, gamma, xscr, yscr, range;
	double	temp, A_factor;
	double	H, fov;
	double	k1, k2, k3;
	double	tan_pitch, cos_pitch, sin_pitch;
	double	cos_roll, sin_roll, x_center, y_center;
	double	pixel_weight, haze_weight;
	int	i, j, width, height, b;
	double	haze_red, haze_green, haze_blue;
	JohnHazeInfo *haze;
	ImageData *temp_dat;

	if(!scene) {
		fprintf(stderr,"Whoops - Do not call render before calling set_scene.\n");
		return(FALSE);
	}

	if(!(scene->get_haze_info()))return(FALSE);
	if(scene->get_haze_info()->get_model() == JOHN_HAZE) {
		haze = (JohnHazeInfo *)scene->get_haze_info();
	} else {
		return(FALSE);
	}

	sin_pitch = sin(incam->yrot.get_value() * TORADIANS);
	cos_pitch = cos(incam->yrot.get_value() * TORADIANS);
	tan_pitch = tan(incam->yrot.get_value() * TORADIANS);
	sin_roll = sin(incam->xrot.get_value() * TORADIANS);
	cos_roll = cos(incam->xrot.get_value() * TORADIANS);
	// double tan_roll = tan(incam->xrot.get_value() * TORADIANS);
	// double sin_yaw = sin(incam->zrot.get_value() * TORADIANS);
	// double cos_yaw = cos(incam->zrot.get_value() * TORADIANS);
	// double tan_yaw = tan(incam->zrot.get_value() * TORADIANS);

	temp_dat = inimg->get_image(0)->get_data();

	inimg->get_image(0)->get_data()->get_res(&width, &height, &b);

	fov = ((Camera *)(incam->get_object()))->get_fov() * TORADIANS;

	H = (width/2) / tan(fov/2); // is fov horiz or vert???
	k1 = tan_pitch * sin_pitch + cos_pitch;
	k2 = H * tan_pitch;
	k3 = 0.0;  /* used later */

	x_center = width/2;
	y_center = height/2;

	haze->get_haze_color(&haze_red, &haze_green, &haze_blue);

	if(haze->get_haze_B() <= 0.0)haze->set_haze_B(0.00000000001);
	A_factor = haze->get_haze_A() * exp(-(haze->get_haze_B() * incam->z.get_value()));


   /*
   ** Now compute the haze and output the image.
   */
    	for(i=height-1; i>=0; i--) {
    	    for(j=0; j<width; j++) {

            	range = inimg->get_image(1)->get_data()->get_double(j, i);

		xscr = (double)j;
		yscr = (double)(height-1-i);

#ifdef debug
		printf("Trying Ix=%lf  Iy=%lf range=%lf\n", xscr, yscr, range);
#endif

		/* remove roll */
		temp = (xscr - x_center) * cos_roll - (yscr - y_center) * sin_roll;
		yscr = (yscr - y_center) * cos_roll + (xscr - x_center) * sin_roll;
		xscr = temp;

		/* compute theta and gamma */
		k3 = yscr*tan_pitch + H;
		theta = atan2((xscr * k1) , k3);
		gamma = atan(cos(theta) * (k2 - yscr) / k3);
		
#ifdef debug
		printf(" Theta = %f\n", theta * TODEGREES);
		printf(" Gamma = %f\n", gamma * TODEGREES);
#endif

/*
  The other function of the pixel mapper is to implement a haze
  algorithm.  The haze appears as a replacement of some of the
  pixel's intensity, in IR or RGB, with some haze intensity.  The
  amount of the pixel's intensity remaining is a function of slant
  range, pitch, and haze density as follows:


          -BZeye    RsBsin(pitch)
        Ae      (1-e             )/Bsin(pitch)
  Wo = e                                       for pitch <> 0


              -BZeye
        -(RsAe      )
  Wo = e                                       for pitch = 0

  where Rs is slant range from the eye to the pixel, Zeye is the
  elevation of the eyepoint, pitch is the depression angle of the
  pixel from the horizon, and A and B are haze density parameters.
  The haze and original pixel are combined as follows:

             Ifinal = Ipixel * Wo + Ihaze * (1.0 - Wo)

  Note that for moving objects it is expected that the haze density
  will either not vary significantly across them (if they are far
  away they should be small) or not be very dense (if they are closee
  the haze should be thin) so the pitch to the centroid of the
  object is used instead of the pixel pitch.  This is because the
  pixel pitch of objects is in terms of the subframe coordinate
  space, not the terrain coordinate space where the haze exists.
*/
		    pitch = gamma;

	       if (pitch == 0)
		    pixel_weight = (float) exp ((double) (-1 * 
				   range * A_factor));
	       else
		    pixel_weight = (float) exp ((double) (A_factor * 
				 (1 - exp ((double) (range * 
				 haze->get_haze_B() * 
				 sin (pitch)))) / 
				 (haze->get_haze_B() *
				 sin (pitch))));

	       haze_weight = 1.0 - pixel_weight;
#ifdef debug
printf ("haze_weight = %f\n", haze_weight);
printf ("pixel_weight = %f\n", pixel_weight);
printf ("pitch = %f\n", pitch);
printf ("slant_range = %f\n", range);
printf ("\n");
#endif
	       temp_dat->set((temp_dat->get_double(j, i, RED_BAND) * pixel_weight)
		    + (haze_red * haze_weight), j, i, RED_BAND);
	       temp_dat->set((temp_dat->get_double(j, i, GREEN_BAND) * pixel_weight)
		    + (haze_green * haze_weight), j, i, GREEN_BAND);
	       temp_dat->set((temp_dat->get_double(j, i, BLUE_BAND) * pixel_weight)
		    + (haze_blue * haze_weight), j, i, BLUE_BAND);
	  }	/* haze for-loop */
     }	/* if haze */

	return(0);	// means okay

} /* render */
