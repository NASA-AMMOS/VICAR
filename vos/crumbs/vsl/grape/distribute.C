// distribute.C 1.7 01/10/11 15:05:11
// The distributed renderer class.
//
// Requires enabling of PVM_Dataport class.

#ifdef PVM_DATAPORT

#include "grape/distribute.h"

int Distributed_Renderer::set_scene(Scene *s, ImgInfo *img)
{
	Task_Status	status;

	if(!render_string) {
		fprintf(stderr,"Whoops - renderer string not set for distributed renderer.\n");
		return(1);
	}
	if(!render_active) {
		network = new Network();
		if(!network) {
			fprintf(stderr,"Whoops - Distributed_Renderer unable to start network.\n");
			return(1);
		}
		dpt = new Distributed_PVM_Task();
		if(!dpt) {
			fprintf(stderr,"Whoops - Distributed_Renderer unable to create task object.\n");
			delete(network);
			return(1);
		}
		dpt->parse_string(render_string);
		status = network->run_command(dpt);
		if(status != DIST_TASK_BUSY) {
			fprintf(stderr,"Whoops - Distributed_Renderer unable to execute remote renderer %s.\n", render_string);
			delete(dpt);
			delete(network);
			return((int)status);
		}
		port = new PVM_Dataport(dpt);
		if(!port) {
			fprintf(stderr,"Whoops - Distributed_Renderer unable to open port to remote renderer.\n");
			delete(dpt);
			delete(network);
			return(1);
		}
		port->wopen("dummy");
		render_active = TRUE;
	}
	scene = s;
	s->parse_out(port, TRUE);
	port->flush(SET_SCENE_MSG);
	// if(img)img->parse_out(port, TRUE);
	// send desired data type for image or alpha
	// send desired data type for z data
	// port->flush(SET_IMAGE_MSG);

	return(0);	// means okay

} /* set_scene */


int Distributed_Renderer::render(ObjNode *, ImgInfo *)
{
	return(FALSE);
}

int Distributed_Renderer::render()
{

	if(render_start()) {
		fprintf(stderr,"Whoops - Unable to start distributed renderer.\n");
		return(1);
	}

	process_image();
	render_in_progress = FALSE;

	return(0);	// means okay

} /* render */


int Distributed_Renderer::render_start()
{
	if(render_active) {
		dpt->reset_buffer();
		dpt->put_in_buffer(get_time());
		dpt->send_buffer(SET_CLOCK_MSG);
	
		dpt->reset_buffer();
		dpt->send_buffer(RENDER_START_MSG);
	
		render_in_progress = TRUE;
	
		return(0);
	} else {
		fprintf(stderr,"Whoops - Cannot initiate render until renderer is active.\n");
		return(1);
	}
} /* render_start */


int Distributed_Renderer::render_start(ObjNode *, ImgInfo *)
{
	if(render_active) {
		dpt->reset_buffer();
		dpt->put_in_buffer(get_time());
		dpt->send_buffer(SET_CLOCK_MSG);
	
		dpt->reset_buffer();
		dpt->send_buffer(RENDER_START_MSG);
	
		render_in_progress = TRUE;
	
		return(0);
	} else {
		fprintf(stderr,"Whoops - Cannot initiate render until renderer is active.\n");
		return(1);
	}
} /* render_start */


int Distributed_Renderer::render_done()
{
	if(render_in_progress) {
		if(dpt->message_arrived(RENDER_DONE_MSG)) {
			process_image();
			render_in_progress = FALSE;
			return(TRUE);
		} else {
			return(FALSE);
		}
	} else {
		return(TRUE);
	}
} /* render_done */

void Distributed_Renderer::process_image(void)
{
	dpt->recv_buffer(RENDER_DONE_MSG);	// wait for done msg from renderer
// do the right thing with the image info and buffers
}

// **********************  Distributed_Renderer_Header  *******************
Distributed_Renderer_Header::Distributed_Renderer_Header(Renderer *rndr)
{
	renderer = rndr;
	parent = new Master_Task;
	port = new PVM_Dataport(parent);
}

Distributed_Renderer_Header::~Distributed_Renderer_Header(void)
{
	if(port)delete(port);
	if(parent)delete(parent);
}

void Distributed_Renderer_Header::event_loop(void)
{
	int	tag;
	double	local_time;
	Scene	*s=NULL;

	do {
		parent->recv_buffer();
		tag = parent->mesg_tag();
		switch(tag) {
			case SET_SCENE_MSG:
				if(s)delete(s);
				s = new Scene();
				s->parse_in(port);
				renderer->set_scene(s);
				break;
			case RENDER_START_MSG:
				renderer->render();
// change later to properly dispose of image
parent->reset_buffer();
parent->send_buffer(RENDER_DONE_MSG);
				break;
			case SET_CLOCK_MSG:
				parent->get_from_buffer(&local_time);
				set_time(local_time);
				break;
			case RENDER_CONTROL_MSG:
				break;
			case RENDER_QUERY_MSG:
				break;
			case RENDER_EXIT_MSG:
				break;
			default:
				fprintf(stderr,"Whoops - Unrecognized message type in event loop.\n");
				break;
		}
	} while(tag != RENDER_EXIT_MSG);
	parent->reset_buffer();
	parent->send_buffer(RENDER_EXIT_MSG);
	exit(0);
}

#endif
