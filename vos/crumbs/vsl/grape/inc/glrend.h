// glrend.h

#ifndef _GLREND_H_
#define _GLREND_H_

#include <X11/Intrinsic.h>

#include <gl/gl.h>
#include <gl/device.h>
#include "grape/renderer.h"

// The class for a GL renderer.
// 
class GLrenderer : public Renderer
   {
   private:

   protected:

      GL_Object *My_GL_Objects;
	int	last_rendered;
      
      Angle WFUpToRoll(double ex,double ey,double ez,
                       double gx,double gy,double gz,
                       double ux,double uy,double uz);

	int	process_object(int);

	long	gl_win_id;
	unsigned long	*parray;

   public:

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


	GLrenderer(void) {
		My_GL_Objects = NULL;
		gl_win_id = 0;
		parray = NULL;
	}
	~GLrenderer(void) {
		if(My_GL_Objects)free(My_GL_Objects);
		if(parray)free(parray);
	}

   }; /* Class */

#endif
