// renderer.h

#ifndef	_RENDERER_H_
#define _RENDERER_H_

#include "grape/rend_messages.h"
#include "grape/clock.h"
#include "grape/scene.h"
#include "grape/img_info.h"
#include "grape/camera.h"

// Type for render done "callback" function:
typedef	void (*RenderCB)(void *, int);

// This is the base class for all rederers (front ends):
// This defines the standard renderer interface.

class Renderer {

    private:

    protected:

	RenderCB	done_callback;

        Scene           *scene;

	void		*done_data;

	void	init() { 
			done_callback=NULL; 
			done_data=NULL; 
			flat_list = NULL;
		}

	void	call_done_callback(int stat) {
			if(done_callback) (*done_callback)(done_data,stat); }

	int		flatten(ObjNode ***);
	ObjNode		*flat_list;

    public:

        virtual int     set_subwins(int, int) { return FALSE; }

	// Pass the renderer scene data (replaces any old scene data)
	virtual	int	set_scene(Scene *, ImgInfo * =NULL) { return(FALSE); }

    // allow users to get the scene upon demand, if they want to...
	Scene *get_scene(void) { return scene;}

	// Pass the renderer information about the output image
	// virtual	int	image_info(ImgInfo *i);

	// Render a frame, return success status (waits until done)
	virtual int	render(ObjNode *, ImgInfo *) { return(FALSE); }
	int	render(ObjNode *cam) {
		return(render(cam, ((Camera *)(cam->get_object()))->get_img_info()));
	}
	int	render(ImgInfo *img) {
		return(render(scene->get_camera(), img));
	}
	virtual	int	render(void) { return(FALSE); }

	// Start asynchronous rendering, return success status
	virtual	int	render_start(void) { return(render()); }
	int	render_start(ImgInfo *img) {
		return(render(scene->get_camera(), img));
	}
	int	render_start(ObjNode *cam) {
		return(render(cam, ((Camera *)(cam->get_object()))->get_img_info()));
	}
	virtual int	render_start(ObjNode *on, ImgInfo *im) { 
		return(render(on, im)); 
	}

	// Returns true if done with asynchronous rendering
	virtual	int	render_done() { return(TRUE); }

	// Returns a unique id number for the renderer
	virtual	int	get_id() { return 0; }

	// Sets a function to be called when asynchronous rendering is done
	virtual	void	set_done_callback(RenderCB cb=NULL, void *dat=NULL) {
				done_callback=cb; done_data=dat; }

	// Mechanism for sending or requesting renderer information
	// Messages consist of a message type id, and optional data buffer
	// Returns true if message type is supported
	virtual	int	send_message(int, void * =NULL) { return 0; }

	// Function for doing inverse render queries, returns success status
	virtual	int	inverse_render(InvRender *) { return 0; }

	// Returns mask of supported inverse render queries
	virtual	int	inv_render_supported() { return 0; }

	// returns count of how many objects can be handled in a scene
	// 0 means unlimited
	virtual int object_limit(void) { return(0); }


	Renderer() { init(); }

	virtual	~Renderer() { ; }

};


// Class ForwardProjectRenderer
// Created by John Wright
// Created Thu Nov 20 11:54:52 1997

// This class implements some fo the transformations needed
// for mapping objects to the screen during rendering.  The
// process of rendering by mapping object features to the screen
// is known as forward projection.

#ifndef _ForwardProjectRenderer_H_
#define _ForwardProjectRenderer_H_

class ForwardProjectRenderer:public Renderer {

    private:

    protected:

	ZMatrix	obj_space_to_cam_space_xform;
	ZMatrix	cam_space_to_obj_space_xform;
	ZMatrix	persp_canonical_view_xform;
	ZMatrix	persp_screen_xform;

    public:

	void	gen_obj_space_to_cam_space_xforms(ObjNode *obj, ObjNode *cam) {
			ZMatrix temp, temp2;
			cam->GetTransformationMatrix(temp2);
			MatInvert(temp2, temp);
			obj->GetObjToWorldTransform(cam_space_to_obj_space_xform
);
			MatMult(cam_space_to_obj_space_xform, temp);
			MatInvert(cam_space_to_obj_space_xform, obj_space_to_cam_space_xform);
		}
	ZMatrix	*get_obj_space_to_cam_space_xform(void) { return(&obj_space_to_cam_space_xform); }
	ZMatrix *get_cam_space_to_obj_space_xform(void) { return(&cam_space_to_obj_space_xform); }

	void	gen_persp_canonical_view_xform(ObjNode *cam, ImgInfo *img);
	ZMatrix *get_persp_canonical_view_xform(void) { return(&persp_canonical_view_xform); }

	void	gen_persp_screen_xform(ObjNode *cam, ImgInfo *img);
	ZMatrix *get_persp_screen_xform(void) { return(&persp_screen_xform); }
		
			

    // Constructors

	ForwardProjectRenderer() {
		MatIdent(obj_space_to_cam_space_xform);
		MatIdent(cam_space_to_obj_space_xform);
		MatIdent(persp_canonical_view_xform);
		MatIdent(persp_screen_xform);
	}

    // Destructor

	~ForwardProjectRenderer() { ; }

};


#endif
#endif
