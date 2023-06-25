// metarender.C

#include "grape/metarender.h"

int MetaRenderer::set_scene(Scene *s, ImgInfo *img)
{
	ObjNode		**master_list = NULL;
	int		obj_count;

//	ObjNode		**perf_list = NULL;	// for Performer Objects
//	int		perf_count;

	ObjNode		**cam_list = NULL;
	int		cam_count = 0;

	ObjNode		**lite_list = NULL;
	int		lite_count = 0;

	ObjNode		**pb_list = NULL;
	int		pb_count = 0;

	ObjNode		**poly_list = NULL;
	int		poly_count = 0;

	int		i, j, limit;
	Scene		*tscene;
	Renderer	*trndr;
	ImgInfo		*tinfo, *timg;
	

// NEED TO FREE UP img info lists, etc. here if they exist 
	// flatten hierarchies = base class member function
	scene = s;
	obj_count = flatten(&master_list);
	// group objects by type (lights, cameras, playback,...)
	for(i=0; i<obj_count; i++) {
		if(master_list[i]->get_object()->get_type() == POLY_V1) {
			poly_count++;
			poly_list = (ObjNode **)realloc((void *)poly_list, poly_count * sizeof(ObjNode *));
			poly_list[poly_count-1] = master_list[i];
		} else if(master_list[i]->get_object()->get_type() == PERF_V1) {
			fprintf(stderr,"Error: Performer Renderer not yet supported in MetaRenderer.\n");
			fprintf(stderr,"Ignoring object: %d\n",
					master_list[i]->get_object()->get_id());
//			perf_count++;
//			perf_list = (ObjNode **)realloc((void *)perf_list, perf_count * sizeof(ObjNode *));
//			perf_list[perf_count-1] = master_list[i];
		} else if(master_list[i]->get_object()->get_type() == CAM_V1) {
			cam_count++;
			cam_list = (ObjNode **)realloc((void *)cam_list, cam_count * sizeof(ObjNode *));
			cam_list[cam_count-1] = master_list[i];
		} else if(master_list[i]->get_object()->get_type() == PLAY_V1) {
			pb_count++;
			pb_list = (ObjNode **)realloc((void *)pb_list, pb_count * sizeof(ObjNode *));
			pb_list[pb_count-1] = master_list[i];
		} else if(master_list[i]->get_object()->get_type() == DIRECTIONAL_LIGHT_V1 ||
			master_list[i]->get_object()->get_type() == POINT_LIGHT_V1 ||
			master_list[i]->get_object()->get_type() == SPOT_LIGHT_V1 ||
			master_list[i]->get_object()->get_type() == AREA_LIGHT_V1) {
			lite_count++;
			lite_list = (ObjNode **)realloc((void *)lite_list, lite_count * sizeof(ObjNode *));
			lite_list[lite_count-1] = master_list[i];
		} else {
			fprintf(stderr,"Whoops - metarenderer found unknown object type = %d\n", master_list[i]->get_object()->get_type());
		}
	}
	// create scenes as appropriate & create matching renderers
	cleanup();
	if(poly_count > 0) {
//		trndr = new GLrenderer();
		trndr = new OpenGLrenderer();
		limit = trndr->object_limit();
		if(limit == 0) limit = poly_count;
		for(i=0; i<((poly_count+limit-1) / limit); i += limit) {
			add_rndr(trndr);
			tscene = new Scene;
			add_scene(tscene);
			// add cameras
			for(j=0; j<cam_count; j++) {
				tscene->add_object(cam_list[j]);
			}
			// add lights
			for(j=0; j<lite_count; j++) {
				tscene->add_object(lite_list[j]);
			}
			// add right number of poly objects
			for(j=0; (j<limit && i+j < poly_count); j++) {
				tscene->add_object(poly_list[i+j]);
			}
			tscene->set_camera(scene->get_camera_num());
			trndr->set_scene(tscene);
//			if(i+j < poly_count)trndr = new GLrenderer();
			if(i+j < poly_count)trndr = new OpenGLrenderer();
		}
		free(poly_list);
	}
/*	if(perf_count > 0) {
		trndr = new PerfRenderer();
		limit = trndr->object_limit();
		if(limit == 0) limit = perf_count;	// no object limit
		for(i=0; i<((perf_count+limit-1) / limit); i += limit) {
			add_rndr(trndr);
			tscene = new Scene;
			add_scene(tscene);
			// add cameras
			for(j=0; j<cam_count; j++) {
				tscene->add_object(cam_list[j]);
			}
			// add lights
			for(j=0; j<lite_count; j++) {
				tscene->add_object(lite_list[j]);
			}
			// add right number of perf objects
			for(j=0; (j<limit && i+j < perf_count); j++) {
				tscene->add_object(perf_list[i+j]);
			}
			tscene->set_camera(scene->get_camera_num());
			trndr->set_scene(tscene);
			if(i+j < perf_count)trndr = new PerfRenderer();
		}
		free(perf_list);
	}
*/
	if(pb_count > 0) {
		trndr = new PBrenderer();
		limit = trndr->object_limit();
		if(limit == 0) limit = pb_count;
		for(i=0; i<((pb_count+limit-1) / limit); i += limit) {
			add_rndr(trndr);
			tscene = new Scene;
			add_scene(tscene);
			// add cameras
			for(j=0; j<cam_count; j++) {
				tscene->add_object(cam_list[j]);
			}
			// add lights
			for(j=0; j<lite_count; j++) {
//				tscene->add_object(lite_list[j]);
			}
			// add right number of pb objects
			for(j=0; (j<limit && i+j < pb_count); j++) {
				tscene->add_object(pb_list[i+j]);
			}
			tscene->set_camera(scene->get_camera_num());
			trndr->set_scene(tscene);
			if(i+j < pb_count)trndr = new PBrenderer();
		}
		free(pb_list);
	}
	if(lite_count > 0) free(lite_list);
	if(cam_count > 0) free(cam_list);
	if(obj_count > 0) free(master_list);

	if(img) {
		timg = img;
	} else {
		timg = ((Camera *)(scene->get_camera()->get_object()))->get_img_info();
	}
	if(!timg) {
		for(i=0; i<num_rndrs; i++) {
			tinfo = timg->duplicate();
			tinfo->set_image_fill();
			tinfo->set_image_flag(TRUE);
			tinfo->set_alpha_flag(TRUE);
			tinfo->set_z_flag(TRUE);
			add_info(tinfo);
		}
	}

	if(scene->get_haze_info()) {
		if(scene->get_haze_info()->get_model() == ZAREH_HAZE) {
			trndr = new ZarehHazeRenderer();
			add_post_rndr(trndr);
			trndr->set_scene(scene);
		} else if(scene->get_haze_info()->get_model() == JOHN_HAZE) {
			trndr = new JohnHazeRenderer();  // change to john 
			add_post_rndr(trndr);
			trndr->set_scene(scene);
		} else {
			fprintf(stderr," Whoops - Unrecognized haze model in metarenderer.\n");
		}
	}

	return(TRUE);
}

int MetaRenderer::render()
{
	ImgInfo		*timg;
	ObjNode		*cam;

	cam = scene->get_camera();
	timg = ((Camera *)(scene->get_camera()->get_object()))->get_img_info();
	return(render(cam, timg));
}

int MetaRenderer::render(ObjNode *cam, ImgInfo *img)
{
	int	i;
	ImgInfo		*tinfo, *timg;
	int	x, y, w, h, n, nbands;
	double	val, val2, z, z2, alpha, alpha2;

//	cleanup_infos();
	if(num_rndrs > num_infos) {
		for(i=num_infos; i<num_rndrs; i++) {
			tinfo = img->duplicate();
			tinfo->set_image_fill();
			tinfo->set_image_flag(TRUE);
			tinfo->set_alpha_flag(TRUE);
			tinfo->set_z_flag(TRUE);
			add_info(tinfo);
		}
	}

/*****************************************************************************
The following code is used to resolve problems which arise when the
metarenderer needs to use the alpha and z channels but the user has
specified only returning one of the channels (usually image).  This
code checks to see if the incoming, caller-supplied img_info object
has all three necessary bands.  If so, it merely uses the incoming
object for all operations.  If not, it creates another img_info
object with pointers to the incoming image areas that exist and new
image areas for the ones that do not exist.  The newly allocated ones
are deleted at the end of the render.

NOTE: Problems still exist in the use of alpha when the user has
specified image but not alpha since the alpha band is part of the
image band image object.  We would prefer to not copy the image
contents so it may be best to have a separate alpha image.  Yuck.
*****************************************************************************/

	if(img->get_image_flag() && img->get_alpha_flag() && img->get_z_flag()) {
		timg = img;
		img->prepare();
	} else {
		timg = img->duplicate();
		timg->set_image_flag(TRUE);
		timg->set_alpha_flag(TRUE);
		timg->set_z_flag(TRUE);
		img->prepare();
		i = 0;
		if(img->get_image_flag() || img->get_alpha_flag()) {
			timg->add_image(img->get_image(i++));
		} else {
			timg->add_image(new Image());
		}
		if(img->get_z_flag()) {
			timg->add_image(img->get_image(i++));
		} else {
			timg->add_image(new Image());
		}
		timg->prepare();
	}

	img->get_image(0)->get_data()->get_res(&w, &h, &nbands);
	for(i=0; i<img->get_num_images(); i++) {
		img->get_image(i)->clear();
	}

	for(i=0; i<num_rndrs; i++) {
		// check scene for change
		// if changed - check each object for change
		// call each renderer
fprintf(stderr,"Running renderer %d in metarenderer.\n", i);
		rndr_list[i]->render(cam, info_list[i]);
		// composite using z and alpha
		for(x=0; x<w; x++) {
			for(y=0; y<h; y++) {
				alpha = info_list[i]->get_image(0)->get_data()->get_alpha(x, y) / 255.0;
				alpha2 = timg->get_image(0)->get_data()->get_alpha(x, y) / 255.0;
				z = info_list[i]->get_image(1)->get_data()->get_double(x, y);
				z2 = timg->get_image(1)->get_data()->get_double(x, y);
// if(x == 190) {
// fprintf(stderr,"x=%d  y=%d  currz=%f  inz=%f  cura=%f  ina=%f\n", x,y,z2 , z, alpha2, alpha);
// }
				// kluge to fix if z's are zero
				if(z2 <= 0.0 || i == 0 ) z2 = 1.0e20;
				if(z <= 0.0) z = z2 + 1.0;
				if( z < z2 ) {   // incoming is near
					for(n=0; n<nbands-1; n++) {
						val = info_list[i]->get_image(0)->get_data()->get_double(x, y, n);
						val2 = timg->get_image(0)->get_data()->get_double(x, y, n);
						val2 = val2 * alpha2 * (1.0 - alpha) + val * alpha;
						timg->get_image(0)->get_data()->set(val2, x, y, n);
					}
					timg->get_image(1)->get_data()->set(z, x, y);
				} else {		// incoming is farther
					for(n=0; n<nbands-1; n++) {
						val = info_list[i]->get_image(0)->get_data()->get_double(x, y, n);
						val2 = timg->get_image(0)->get_data()->get_double(x, y, n);
						val2 = val2 * alpha2 + val * alpha * (1.0 - alpha2);
						timg->get_image(0)->get_data()->set(val2, x, y, n);
					}
				}
				alpha2 = alpha2 + alpha * (1.0 - alpha2);
				alpha2 *= 255.0;
				timg->get_image(0)->get_data()->set(alpha2, x, y, ALPHA_BAND);
			}
		}
	}

	// apply any post process renderers (filters)
	if(num_post_rndrs > 0) {
		for(i=0; i<num_post_rndrs; i++) {
			post_rndr_list[i]->render(cam, timg);
		}
	}
			
	img->dispose();

	// cleanup the temporary image areas
	if(timg != img) {
		// free timg
		if(!(img->get_image_flag() || img->get_alpha_flag())) {
			timg->get_image(0)->free_data();
			timg->get_image(0)->free_disp();
			timg->get_image(0)->free_file();
			delete(timg->get_image(0));
		}
		if(!(img->get_z_flag())) {
			timg->get_image(1)->free_data();
			timg->get_image(1)->free_disp();
			timg->get_image(1)->free_file();
			delete(timg->get_image(1));
		}
		delete(timg);
	}

	return(TRUE);
}
