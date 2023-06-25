// camera.h

#ifndef	_CAMERA_H_
#define _CAMERA_H_

#include "grape/object.h"
#include "grape/img_info.h"

// This is the class that contains camera data, may be passed to renderers:


class Camera : public Obj {

    private:

    protected:

	char	*version;
	int	changed;

	dparam	fov;	// in degrees 

	ImgInfo	*image_info;

	// Optional stuff for later
	double	sampling;
	double	depth_of_field;
	double	blur_length;
	double	focal_distance;
	int	do_motion_blur;
	int	do_shadows;
	int	do_shading;
	int	do_reflections;
	int	do_refractions;

	void	init();

    public:

	// Camera parameters:
	double	get_fov(void)	{ return((double)fov.get_value()); }
	void	set_fov(Data *d) { fov.set_data(d); }
	void	set_fov(double f) { 
		if(f > 0.0 && f < 180.0) {
			fov.set_value(f);
		} else {
			fprintf(stderr,"Whoops - Camera fov must be 0 - 180 degrees \n");
		}
	}

	void	get_persp_proj_matrix(ZMatrix InOut) {
			int xres, yres;
			MatIdent(InOut);
			InOut[3][3] = 0.0;
			if(image_info) {
				image_info->get_res(&xres, &yres);
				InOut[3][2] = 2.0 * tan(get_fov()*3.14159265358979/360.0) / xres;
			}
		}

	void	set_img_info(ImgInfo *i) { image_info = i; }
	ImgInfo	*get_img_info(void) { return(image_info); }
	void	delete_img_info(void) { 
		delete(image_info); 
		image_info = NULL; 
	}

	virtual	int	get_changed(void) { return(0); }

	virtual	int	is_camera(void) { return(TRUE); }
	virtual	int	get_type(void)	{ return(CAM_V1); }

	virtual	int	parse_in(Dataport *fp);
	virtual	int	parse_out(Dataport *fp, int expand=FALSE);

	Camera() { 
		fov = 45.0; // 45 degrees in radians
		version = NULL;
		image_info = NULL;
		init(); 
	}
	~Camera() {
		if(version)free(version);
	}
};


#endif
