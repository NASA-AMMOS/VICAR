// distribute.h
#ifdef PVM_DATAPORT

#ifndef	_DISTRIBUTE_H_
#define _DISTRIBUTE_H_

#include <stdio.h>
#include "net/net_lib.h"
#include "grape/renderer.h"

#ifndef TRUE
#define TRUE	1
#define FALSE	0
#endif

// PVM Message types for SRI
#define	SET_SCENE_MSG		1
#define	RENDER_START_MSG	2
#define	SET_CLOCK_MSG		3
#define RENDER_DONE_MSG		4
#define	RENDER_CONTROL_MSG	5
#define	RENDER_QUERY_MSG	6
#define	RENDER_EXIT_MSG		7


class Distributed_Renderer : public Renderer {

    private:

    protected:

	// filename of distributed renderer executable
	char	*render_string;

	// flag for child renderer is running - set by set_scene
	int	render_active;

	// flag for frame currently being rendered
	int	render_in_progress;

	// pvm network (net_lib) being used
	Network	*network;

	// pvm child task (net_lib) pointer
	Distributed_PVM_Task	*dpt;

	// dataport for communication to and from child task
	PVM_Dataport		*port;

	// function which does the right thing with returned image
	void	process_image(void);

    public:

	// function to assign subtask executable filename
	void	set_render_string(char *str) {
		if(render_string)free(render_string);
		render_string = strdup(str);
	}
	char	*get_render_string(void) { return(render_string); }

	// Pass the renderer scene data (replaces any old scene data)
	virtual	int	set_scene(Scene *s, ImgInfo * =NULL) ;
//	virtual	int	set_scene(Scene *s) ;

	// Pass the renderer information about the output image
	// virtual	int	image_info(ImgInfo *i);

	// Render a frame, return success status (waits until done)
	virtual int	render(ObjNode *, ImgInfo *) ;
	virtual int	render(void) ;

	// Start asynchronous rendering, return success status
	virtual	int	render_start() ;
	virtual	int	render_start(ObjNode *on, ImgInfo *im) ;

	// Returns true if completed asynchronous rendering
	virtual	int	render_done() ;

	// Returns a unique id number for the renderer
	virtual	int	get_id() { return(FALSE); }

	// Sets a function to be called when asynchronous rendering is done
	virtual	void	set_done_callback(RenderCB cb=NULL, void *dat=NULL) {
				done_callback=cb; done_data=dat; }

	// Mechanism for sending or requesting renderer information
	// Messages consist of a message type id, and optional data buffer
	// Returns true if message type is supported
	virtual	int	send_message(int, void * =NULL){ return(FALSE); }

	// Function for doing inverse render queries, returns success status
	virtual	int	inverse_render(InvRender *){ return(FALSE); }

	// Returns mask of supported inverse render queries
	virtual	int	inv_render_supported(){ return(FALSE); }

	Distributed_Renderer(char *str=NULL) {
		if(str) {
			render_string = strdup(str);
		} else {
			render_string = NULL;
		}
		network = NULL;
		dpt = NULL;
		port = NULL;
		render_active = FALSE;
	}

	~Distributed_Renderer() {
		if(render_string)free(render_string);
		if(render_active) {
			port->flush(RENDER_EXIT_MSG);
			dpt->recv_buffer(RENDER_EXIT_MSG);
			delete(port);
			delete(dpt);
			delete(network);
		}
	}
};

// **************************************************************
// Provides a header for a renderer with SRI to be run on a
// distributed system

class Distributed_Renderer_Header {

    private:

    protected:

	// SRI renderer to be distributed
	Renderer	*renderer;

	// pvm master task (net_lib)
	Master_Task	*parent;

	// pvm data comm port
	PVM_Dataport	*port;

	void	init(void);

    public:

	// Event loop to receive renderer function calls from pvm
	void	event_loop(void);

	Distributed_Renderer_Header(Renderer *);
	~Distributed_Renderer_Header(void);
};

#endif
#endif
