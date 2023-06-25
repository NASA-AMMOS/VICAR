// hazerend.h

#ifndef _HAZEREND_H_
#define _HAZEREND_H_

#include <X11/Intrinsic.h>

#include "grape/renderer.h"
#include "grape/hazeinfo.h"

// The class for a Haze renderer using Zareh's haze model.
// 
class ZarehHazeRenderer : public Renderer
{
private:

protected:

public:

   int set_scene(Scene *s, ImgInfo * =NULL);

   double AngleFrac();

   void HazeBlend(double, double *);

   void SkyBlend(double *);

   int render(ObjNode *, ImgInfo *);
   int render(void) 
      {
      if(scene) 
         return(render(scene->get_camera(), 
               ((Camera *)(scene->get_camera()->get_object()))->get_img_info()));
      else 
         return(FALSE);
      } /* render */

   ZarehHazeRenderer(void) { ; }

   ~ZarehHazeRenderer(void) { ; }

}; /* Class */

// The class for a Haze renderer using John's haze model.
// 
class JohnHazeRenderer : public Renderer
{
private:

protected:

public:

   int set_scene(Scene *s, ImgInfo * =NULL);

   int render(ObjNode *, ImgInfo *);
   int render(void) 
      {
      if(scene) 
         return(render(scene->get_camera(), 
               ((Camera *)(scene->get_camera()->get_object()))->get_img_info()));
      else 
         return(FALSE);
      } /* render */

   JohnHazeRenderer(void) { ; }

   ~JohnHazeRenderer(void) { ; }

}; /* Class */

#endif


