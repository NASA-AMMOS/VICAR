// scene.h

#ifndef	_SCENE_H_
#define _SCENE_H_

#include "object.h"
#include "hazeinfo.h"

#ifndef TRUE
#define TRUE	1
#define FALSE	0
#endif

// This is the class that contains scene data, and is passed to renderers:


class Scene {

    private:

    protected:

	char	*name;
	char	*reference;
	char	*version;
	int	changed;

	int	num_objects;
	ObjNode	**on_list;

	ObjNode	*active_camera;

        HazeInfo *haze_info;

	// Optional stuff for later
	int	do_shadows;
	int	do_shading;
	int	do_reflections;
	int	do_refractions;
	int	quality_level;

	void	init();

    public:

	// Scene parameters:
	int	do_fog;
	dparam	fog_start,fog_end;
	// etc...

	int	get_num_objects(void) { return(num_objects); }
	void	set_num_objects(int num) { num_objects = num; }
	void	add_object(Obj *);
	void	add_object(ObjNode *);

	void    replace_object(int num, Obj *); 

	ObjNode	*get_obj(int i) {
		if(i>=0 && i<num_objects) {
			return(on_list[i]);
		} else {
			return(NULL);
		}
	}
	void	delete_objects(void) {
		int	i;
		for(i=0; i<get_num_objects(); i++) {
			delete(get_obj(i)->get_object());
			delete(get_obj(i));
		}
		set_num_objects(0);
		on_list = 0;
	}

	ObjNode	*get_camera(void) { return(active_camera); }
	ObjNode	*get_camera(int);
	void	set_camera(ObjNode *cam_ptr) { active_camera = cam_ptr; }
	void	set_camera(int cam_num=1);
	int	get_camera_num(void);
	int	get_camera_num(ObjNode *);

        HazeInfo *get_haze_info(void) { return(haze_info); }
        void     set_haze_info(HazeInfo *haze_ptr) { haze_info = haze_ptr; }

	void	set_reference(char *str) { 
		if(reference)free(reference);
		reference = strdup(str);
	}
	char	*get_reference(void) { return(reference); }

	int	get_changed();

	int	parse_in(Dataport *fp);
	int	parse_out(Dataport *fp, int expand=FALSE);

	Scene() { 
		num_objects = 0; 
		on_list = NULL; 
		active_camera = NULL; 
		haze_info = NULL; 
		name = NULL;
		reference = NULL;
		version = NULL;
		init(); 
	}

	~Scene() {
		if(name)free(name);
		if(reference)free(reference);
		if(version)free(version);
		if(on_list)free(on_list);
	}
};


#endif
