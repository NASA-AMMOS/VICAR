// scene.C

#include "dataport.h"
#include "grape/object_types.h"
#include "grape/scene.h"

void	Scene::init(void) {
	int     i;

	for(i=0; i<num_objects; i++) {
		if(on_list[i])delete(on_list[i]);
	}       
	if(on_list)free(on_list);
	on_list = NULL;
	if(name)free(name);
	name = NULL;
	if(reference)free(reference);
	reference = NULL;
	if(version)free(version);
	version = NULL;
	num_objects = 0;
	active_camera = NULL;
}

int	Scene::get_camera_num(void)
{
	int	i, cam_temp=0;
	ObjNode	*on;

	for(i=0; i<get_num_objects(); i++) {
		on = on_list[i];
		if(on->get_object()->is_camera()) {
			cam_temp++;
			if(on == get_camera()) {
				return(cam_temp);
			}
		}
	}
	return(0);
}

int	Scene::get_camera_num(ObjNode *cam)
{
	int	i, cam_temp=0;
	ObjNode	*on;

	for(i=0; i<get_num_objects(); i++) {
		on = on_list[i];
		if(on->get_object()->is_camera()) {
			cam_temp++;
			if(on == cam) {
				return(cam_temp);
			}
		}
	}
	return(0);
}

ObjNode	*Scene::get_camera(int num)
{
	int	i, cam_temp=0;
	ObjNode	*on;

	for(i=0; i<get_num_objects(); i++) {
		on = on_list[i];
		if(on->get_object()->is_camera()) {
			cam_temp++;
			if(cam_temp == num) {
				return(on);
			}
		}
	}
	return(NULL);
}

void    Scene::set_camera(int cam_num)
{
	int	i;
	ObjNode	*on;

	for(i=0; (i<get_num_objects() && cam_num > 0); i++) {
		on = on_list[i];
		Obj *obj = on->get_object();
		if(obj->is_camera()) {
			set_camera(on);
			cam_num--;
		}
	}
}

void	Scene::replace_object(int num, Obj *obj) 
{
	if (num < 0 || num >= num_objects) return;
	on_list[num]->x = 0.0;
	on_list[num]->y = 0.0;
	on_list[num]->z = 0.0;
	on_list[num]->xrot = 0.0;
	on_list[num]->yrot = 0.0;
	on_list[num]->zrot = 0.0;
	on_list[num]->xscale = 1.0;
	on_list[num]->yscale = 1.0;
	on_list[num]->zscale = 1.0;

	on_list[num]->set_object(obj);
}

void	Scene::add_object(Obj *obj) 
{
	int	i;

	i = num_objects;
	num_objects++;
	on_list = (ObjNode **)realloc((void *)on_list, num_objects*sizeof(ObjNode *));
	on_list[i] = new ObjNode();

	on_list[i]->x = 0.0;
	on_list[i]->y = 0.0;
	on_list[i]->z = 0.0;
	on_list[i]->xrot = 0.0;
	on_list[i]->yrot = 0.0;
	on_list[i]->zrot = 0.0;
	on_list[i]->xscale = 1.0;
	on_list[i]->yscale = 1.0;
	on_list[i]->zscale = 1.0;

	on_list[i]->set_object(obj);
}

void    Scene::add_object(ObjNode *obj) 
{
	int	i;

	i = num_objects;
	num_objects++;
	on_list = (ObjNode **)realloc((void *)on_list, num_objects*sizeof(ObjNode *));
	on_list[i] = obj;
}


int	Scene::parse_in(Dataport *fp)
{
	char	token[4096];
	int	i, temp, cam_temp=0;
	ObjNode	*tnode;
	Dataport        *tfp;


    init();

    do {
	if(!get_next_token(fp, token)) {
		fprintf(stderr," Whoops - Unexpected EOF in scene file\n");
		return(FALSE);
	}
	if(!strcmp(token, "SCENE_V1")) {
		// eat it
	} else if(!strcmp(token, "VERSION")) {
		if(!get_next_token(fp, token)) {
			fprintf(stderr," Whoops - Unexpected EOF in scene file\n");
			return(FALSE);
		}
		if(strcmp(token, "1.0")) {
			fprintf(stderr," Whoops - Unexpected version number encountered in parsing scene object\n");
			fprintf(stderr,"   Token=>%s<  Expecting >1.0<\n", token);
			return(FALSE);
		}
		if(version) {
			fprintf(stderr,"Whoops - Multiple scene versions in file - using latest = %s\n",token);
			free(version);
		}
		version = strdup(token);

	} else if(!strcmp(token, "SCENE_NAME")) {
		if(!get_next_token(fp, token)) {
			fprintf(stderr," Whoops - Unexpected EOF in scene file\n");
			return(FALSE);
		}
		if(name) {
			fprintf(stderr,"Whoops - Multiple scene names in file - using latest = %s\n",token);
			free(name);
		}
		name = strdup(token);

	} else if(!strcmp(token, "OBJECTS")) {
		if(!get_next_token(fp, token)) {
			fprintf(stderr," Whoops - Unexpected EOF in scene file\n");
			return(FALSE);
		}
		temp = atoi(token);
		for(i=0; i<temp; i++) {
			tnode = new ObjNode();
			if(tnode->parse_in(fp)) {
				add_object(tnode);
			} else {
				delete(tnode);
				return(FALSE);
			}
		}
		
	} else if(!strcmp(token, "RELATIONSHIPS")) {
		if(!get_next_token(fp, token)) {
			fprintf(stderr," Whoops - Unexpected EOF in scene file\n");
			return(FALSE);
		}
		temp = atoi(token);
		for(i=0; i<temp; i++) {
		}

	} else if(!strcmp(token, "CAMERA")) {
		if(!get_next_token(fp, token)) {
			fprintf(stderr," Whoops - Unexpected EOF in scene file\n");
			return(FALSE);
		}
		cam_temp = atoi(token);
		
	} else if(!strcmp(token, "HAZE")) {
		if(!get_next_token(fp, token)) {
			fprintf(stderr," Whoops - Unexpected EOF in scene file\n");
			return(FALSE);
		}
		if(!strcmp(token, "{")) {       // inline  haze reference
			haze_info = create_haze_info(fp);
			if(!haze_info) {
				fprintf(stderr,"Whoops - Unable to create haze info from inline data\n");
				return(FALSE);
			}
			if(!haze_info->parse_in(fp)) {
				fprintf(stderr,"Whoops - Problems parsing inline haze info \n");
				return(FALSE);
			}
			if(!get_next_token(fp, token)) {
				fprintf(stderr," Whoops - Unexpected EOF in scene file\n");
				return(FALSE);
			}
			if(strcmp(token, "}")) {
				fprintf(stderr," Whoops - Missing } after inline haze info\n");
				return(FALSE);
			}
		} else if(!strcmp(token, "NULL")) {     // NULL haze info reference
		} else if(!strcmp(token, "NONE")) {     // NULL haze info reference
		} else {                                // external haze info reference
			tfp = dataport_create(fp->get_type());
			if(tfp && tfp->ropen(token)) {
				haze_info = create_haze_info(tfp);
				if(!haze_info) {
					fprintf(stderr,"Whoops - Unable to create haze info from external reference\n");
					return(FALSE);
				}
				if(!haze_info->parse_in(tfp)) {
					fprintf(stderr,"Whoops - Problems parsing external haze info \n");
					return(FALSE);
				}
				haze_info->set_reference(token);
			} else {
				fprintf(stderr,"Whoops - Unable to open referenced haze info file %s\n", token);
				return(FALSE);
			}
		}

		
	} else if(strcmp(token, "NO_MO_SCENE")) {
		fprintf(stderr," Whoops - Unexpected token encountered in parsing scene object\n");
		fprintf(stderr,"   Token=>%s<  Expecting >NO_MO_SCENE<\n", token);
		return(FALSE);
	}

    set_camera(cam_temp);

    } while(strcmp(token, "NO_MO_SCENE"));

    return(TRUE);
	
}

int Scene::parse_out(Dataport *fp, int expand)
{
	char	token[4096];
	int	i;

	put_token(fp, "SCENE_V1");
	put_token(fp, "\nVERSION");
	put_token(fp, "1.0");
	if(name) {
		put_token(fp, "\nSCENE_NAME");
		put_token(fp, name);
	}
	put_token(fp, "\nOBJECTS");
	sprintf(token, "%d", num_objects);
	put_token(fp, token);
	for(i=0; i<num_objects; i++) {
		on_list[i]->parse_out(fp, expand);
	}

	put_token(fp, "\nRELATIONSHIPS");
	sprintf(token, "%d", 0);
	put_token(fp, token);

	put_token(fp, "\nCAMERA");
	sprintf(token, "%d", get_camera_num());
	put_token(fp, token);

	put_token(fp, "\nHAZE");
	if(haze_info) {
		if(expand || !haze_info->get_reference()) {
			put_token(fp, "{");
			haze_info->parse_out(fp, expand);
			put_token(fp, "}");
		} else {
			put_token(fp, haze_info->get_reference());
		}
	} else {
		put_token(fp, "NONE");
	}

	put_token(fp, "\nNO_MO_SCENE\n");

	return(TRUE);
}


