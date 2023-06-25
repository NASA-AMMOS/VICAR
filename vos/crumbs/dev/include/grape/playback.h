// playback.h

#include "grape/renderer.h"
#include "image/types/rlafile.h"

// The class for a playback object
//
class PB_Obj : public Obj {

   private:

   protected:

	char	*file_root;
	char	*file_name;
	char	*z_file_root;
	char	*z_file_name;
	char	*version;
	double	start_time;
	double	end_time;
	double	delta_time;
	int	loop;
	int	hard_limit;
	int	interpolate;
	dparam	x_offset;
	dparam	y_offset;
	dparam	x_scale;
	dparam	y_scale;
	dparam	default_alpha;

   public:

	void	set_start_time(double t) { start_time = t; }
	double	get_start_time(void) { return(start_time); }
	void	set_end_time(double t) { end_time = t; }
	double	get_end_time(void) { return(end_time); }
	void	set_delta_time(double t) { delta_time = t; }
	double	get_delta_time(void) { return(delta_time); }

	void	set_interpolation(int b=TRUE) { interpolate = b; }
	int	interpolation(void) { return(interpolate); }
	void	set_looping(int b=TRUE) { loop = b; }
	int	looping(void) { return(loop); }
	void	set_hard_limiting(int b=TRUE) { hard_limit = b; }
	int	hard_limiting(void) { return(hard_limit); }

	void	set_offset(double _x, double _y) {
		x_offset.set_value(_x);
		y_offset.set_value(_y);
	}
	double	get_x_offset(void) { return(x_offset.get_value()); }
	double	get_y_offset(void) { return(y_offset.get_value()); }

	void	set_scale(double _x) {
		x_scale.set_value(_x);
		y_scale.set_value(_x);
	}
	void	set_scale(double _x, double _y) {
		x_scale.set_value(_x);
		y_scale.set_value(_y);
	}
	double	get_x_scale(void) { return(x_scale.get_value()); }
	double	get_y_scale(void) { return(y_scale.get_value()); }

	void	set_default_alpha(double _x) {
		default_alpha.set_value(_x);
	}
	double	get_default_alpha(void) { return(default_alpha.get_value()); }


	void	set_file_root(char *fn) {
		if(file_root)free(file_root);
		file_root = NULL;
		if(fn)file_root = strdup(fn);
	}
	char	*get_file_root(void) { return(file_root); }

	void	set_z_file_root(char *fn) {
		if(z_file_root)free(z_file_root);
		z_file_root = NULL;
		if(fn)z_file_root = strdup(fn);
	}
	char	*get_z_file_root(void) { return(z_file_root); }

	virtual int     get_type(void)  { return(PLAY_V1); }

	virtual	char	*get_file_name_at_time(void);
	virtual	char	*get_z_file_name_at_time(void);

	virtual	int	parse_in(Dataport *);
	virtual int     parse_out(Dataport *, int);

	PB_Obj(void) {
		file_root = NULL;
		file_name = NULL;
		z_file_root = NULL;
		z_file_name = NULL;
		version = NULL;
		start_time = 0.0;
		end_time = 0.0;
		delta_time = 1.0;
		interpolate = FALSE;
		hard_limit = FALSE;
		loop = FALSE;
		x_offset.set_value(0.0);
		y_offset.set_value(0.0);
		x_scale.set_value(1.0);
		y_scale.set_value(1.0);
		default_alpha.set_value(1.0);
	}
	~ PB_Obj(void) {
		if(file_root)free(file_root);
		if(file_name)free(file_name);
		if(z_file_root)free(z_file_root);
		if(z_file_name)free(z_file_name);
		if(version)free(version);
	}

};

// The class for a playback renderer.
// 
class PBrenderer : public Renderer {

   private:

   protected:

	char	*file_name;
	PB_Obj	*pb_obj;

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

	// returns count of how many objects can be handled in a scene
	// 0 means unlimited
	virtual int object_limit(void) { return(1); }

	void	set_pb_obj(PB_Obj *pb) { pb_obj = pb; }
	PB_Obj	*get_pb_obj(void) { return(pb_obj); }

	PBrenderer(void) {
		file_name = NULL;
		pb_obj = NULL;
	}
	~PBrenderer(void) {
		if(file_name)free(file_name);
	}

}; /* Class */
