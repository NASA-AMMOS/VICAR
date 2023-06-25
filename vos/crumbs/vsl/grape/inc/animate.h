//  animate.h

#ifndef	_ANIMATE_H_
#define _ANIMATE_H_

#include "grape/scene.h"
#include "grape/renderer.h"

extern	int	CHANGE_COUNTER;


class Shot {

    private:

	double	start_time;
	double	end_time;

	char	*name;
	char	*version;
	char	*reference;

	Scene	*scene;
	ObjNode	*camera;

	int	changed;

    public:

	int	get_changed() { return changed; }
	void	set_changed(void) { changed=CHANGE_COUNTER; }

	void	set_reference(char *str) {
		if(reference)free(reference);
		if(str)reference = strdup(str);
		else reference = NULL;
	}
	char	*get_reference(void) { return(reference); }

	void	set_scene(Scene *s) { scene = s; changed=CHANGE_COUNTER; }
	Scene 	*get_scene(void) { return(scene); }
	void	set_camera(ObjNode *c) { camera = c; changed=CHANGE_COUNTER; }
	void	set_camera(int n) {
		if(scene) {
			scene->set_camera(n); 
			set_camera(scene->get_camera());
		}
	}
	ObjNode	*get_camera(void) { return(camera); }

	void	set_start_time(double t) { start_time = t; changed=CHANGE_COUNTER; }
	double	get_start_time(void) { return(start_time); }
	void	set_end_time(double t) { end_time = t; changed=CHANGE_COUNTER; }
	double	get_end_time(void) { return(end_time); }

	virtual	int	parse_in(Dataport *);
	virtual	int	parse_out(Dataport *, int);

	Shot(void) {
		scene = NULL;
		camera = NULL;
		name = NULL;
		version = NULL;
		reference = NULL;
		start_time = 0.0;
		end_time = 0.0;
		changed=CHANGE_COUNTER;
	}
	~Shot(void) {
	}

};

class Transition {

    private:

    protected:

	double	start_time;
	double	end_time;
	double	time_offset;
	double	time_scale;
	int	scene_set;

	char	*name;
	char	*version;
	char	*reference;

	Shot		*shot;
	Renderer	*rndr;

	int	changed;

    public:

	int	get_changed() { return changed; }
	void	set_changed(void) { changed=CHANGE_COUNTER; }

	void	set_reference(char *str) {
		if(reference)free(reference);
		if(str)reference = strdup(str);
		else reference = NULL;
	}
	char	*get_reference(void) { return(reference); }

	void	set_shot(Shot *c) { shot = c; changed=CHANGE_COUNTER; }
	Shot 	*get_shot(void) { return(shot); }

	void		set_rndr(Renderer *r) { rndr = r; changed=CHANGE_COUNTER; scene_set = FALSE; }
	Renderer 	*get_rndr(void) { return(rndr); }

	void	set_start_time(double t) { start_time = t; changed=CHANGE_COUNTER; }
	double	get_start_time(void) { return(start_time); }
	void	set_end_time(double t) { end_time = t; changed=CHANGE_COUNTER; }
	double	get_end_time(void) { return(end_time); }
	void	set_time_offset(double t) { time_offset = t; changed=CHANGE_COUNTER; }
	double	get_time_offset(void) { return(time_offset); }
	void	set_time_scale(double t) { time_scale = t; changed=CHANGE_COUNTER; }
	double	get_time_scale(void) { return(time_scale); }

	int	set_scene(ImgInfo * =NULL);
	int	scene_is_set(void) { return(scene_set); }

	virtual	void	render(ImgInfo * =NULL);

	virtual	int	parse_in(Dataport *);
	virtual	int	parse_out(Dataport *, int);

	Transition(void) {
		shot = NULL;
		rndr = NULL;
		name = NULL;
		version = NULL;
		reference = NULL;
		start_time = 0.0;
		end_time = 0.0;
		time_scale = 1.0;
		time_offset = 0.0;
		scene_set = FALSE;
		changed=CHANGE_COUNTER;
	}
	~Transition(void) {
	}

};

#define LINE_WIPE	1
#define	CIRCLE_WIPE	2
#define	RADAR_WIPE	3

class Wipe_Transition : public Transition {

    private:

    protected:

	int	type;
	double	angle;
	double	duration;

    public:

	void	set_type(int n) { type = n; }
	int	get_type(void) { return(type); }
	void	set_angle(double n) { angle = n; }
	double	get_angle(void) { return(angle); }
	void	set_duration(double n) { duration = n; }
	double	get_duration(void) { return(duration); }

	virtual	void	render(ImgInfo *);

	virtual	int	parse_in(Dataport *);
	virtual	int	parse_out(Dataport *, int);

	Wipe_Transition(void) {
		type = LINE_WIPE;
		angle = 0.0;
		duration = 0.0;
	}

};

class Fade_Transition : public Transition {

    private:

    protected:

	int	type;
	double	duration;

    public:

	void	set_type(int n) { type = n; }
	int	get_type(void) { return(type); }
	void	set_duration(double n) { duration = n; }
	double	get_duration(void) { return(duration); }

	virtual	void	render(ImgInfo *);

	virtual	int	parse_in(Dataport *);
	virtual	int	parse_out(Dataport *, int);

};

class Cut_Transition : public Transition {

    private:

    protected:

    public:

	virtual	void	render(ImgInfo *);

	virtual	int	parse_in(Dataport *);
	virtual	int	parse_out(Dataport *, int);


};

class Animation {

    private:

    protected:

	double	start_time;
	double	end_time;
	double	delta_time;
	int	frame_number;
	int	notify;

	char	*name;
	char	*version;
	char	*reference;

	Transition	**tran_list;
	int		tran_count;

	ImgInfo	*img_info;

	int	changed;

    public:

	int	get_changed() { return changed; }
	void	set_changed(void) { changed=CHANGE_COUNTER; }

	void	set_reference(char *str) {
		if(reference)free(reference);
		if(str)reference = strdup(str);
		else reference = NULL;
	}
	char	*get_reference(void) { return(reference); }

	void	set_start_time(double t) { start_time = t; changed=CHANGE_COUNTER; }
	double	get_start_time(void) { return(start_time); }
	void	set_end_time(double t) { end_time = t; changed=CHANGE_COUNTER; }
	double	get_end_time(void) { return(end_time); }
	void	set_delta_time(double t) { delta_time = t; changed=CHANGE_COUNTER; }
	double	get_delta_time(void) { return(delta_time); }
	void	set_frame_number(int f) { frame_number = f; changed=CHANGE_COUNTER; }
	int	get_frame_number(void) { return(frame_number); }
	int	get_frame_number(double t); // get fnum for given time
	void	set_frame_number(double t) { 
		set_frame_number(get_frame_number(t)); 
		changed=CHANGE_COUNTER; 
	}
	void	set_img_info(ImgInfo * inimg) { img_info = inimg; }
	ImgInfo	*get_img_info(void) { return(img_info); }

	void	render(void);
//	void	render(ImgInfo *);
	void	render(int fnum) { set_frame_number(fnum); render(); }
//	void	render(int fnum, ImgInfo *img) { set_frame_number(fnum); render(img); }
	void	animate(void);

	void	add_transition(Transition *);
	void	del_transition(int);
	Transition *get_transition(int n) { 
			if(tran_list && n>=0 && n<tran_count) {
				return(tran_list[n]);
			} else {
				return(NULL);
			}
		}
	int	get_num_transitions(void) { return(tran_count); }

	virtual	int	parse_in(Dataport *);
	virtual	int	parse_out(Dataport *, int);

	Animation(void) {
		tran_list = NULL;
		img_info = NULL;
		name = NULL;
		version = NULL;
		reference = NULL;
		notify = FALSE;
		tran_count = 0;
		start_time = 0.0;
		end_time = 0.0;
		delta_time = 1.0 / 30.0;
		frame_number = 1;
		changed=CHANGE_COUNTER;
	}
	~Animation(void) {
	}

};


#endif
