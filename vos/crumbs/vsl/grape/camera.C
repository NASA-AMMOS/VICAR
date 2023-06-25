// camera.C

#include "dataport.h"
#include "grape/object_types.h"
#include "grape/camera.h"

void	Camera::init(void) {
}


int	Camera::parse_in(Dataport *fp)
{
	char	token[4096];
	Dataport	*tfp;

	if(!Obj::parse_in(fp)) {
		return(FALSE);
	}

	do {
		if(!get_next_token(fp, token)) {
			fprintf(stderr," Whoops - Unexpected EOF in camera file\n");
			return(FALSE);
		}
		if(!strcmp(token, "VERSION")) {
			if(!get_next_token(fp, token)) {
				fprintf(stderr," Whoops - Unexpected EOF in camera file\n");
				return(FALSE);
			}
			if(strcmp(token, "1.0")) {
				fprintf(stderr," Whoops - Unexpected version number encountered in parsing camera object\n");
				fprintf(stderr,"   Token=>%s<  Expecting >1.0<\n", token);
				return(FALSE);
			}
			version = strdup(token);
		} else if(!strcmp(token, "FIELD_OF_VIEW")) {
			if(!fov.parse_in(fp)) {
				fprintf(stderr," Whoops - Problem parsing in camera fov.\n");
				return(FALSE);
			}
		} else if(!strcmp(token, "IMAGE_INFO")) {
			if(!get_next_token(fp, token)) {
				fprintf(stderr," Whoops - Unexpected EOF in camera file\n");
				return(FALSE);
			}
			if(!strcmp(token, "{")) {	// inline image info reference
				image_info = create_img_info(fp);	//new ImgInfo();
				if(!image_info) {
					fprintf(stderr,"Whoops - Unable to create image info from inline data\n");
					return(FALSE);
				}
				if(!image_info->parse_in(fp)) {
					fprintf(stderr,"Whoops - Problems parsing inline image info \n");
					return(FALSE);
				}
				if(!get_next_token(fp, token)) {
					fprintf(stderr," Whoops - Unexpected EOF in camera file\n");
					return(FALSE);
				}
				if(strcmp(token, "}")) {
					fprintf(stderr," Whoops - Missing } after inline image info\n");
					return(FALSE);
				}
			} else if(!strcmp(token, "NULL")) {	// NULL image info reference
			} else {				// external image info reference
				tfp = dataport_create(fp->get_type());
				if(tfp && tfp->ropen(token)) {
					image_info = create_img_info(tfp);	//new ImgInfo();
					if(!image_info) {
						fprintf(stderr,"Whoops - Unable to create image info from external reference %s\n",token);
						return(FALSE);
					}
					if(!image_info->parse_in(tfp)) {
						fprintf(stderr,"Whoops - Problems parsing external image info \n");
						return(FALSE);
					}
					image_info->set_reference(token);
				} else {
					fprintf(stderr,"Whoops - Unable to open referenced image info file %s\n", token);
					return(FALSE);
				}
			}
		} else if(strcmp(token, "CAMERA_END")) {
			fprintf(stderr," Whoops - Unexpected token encountered in parsing camera object\n");
			fprintf(stderr,"   Token=>%s<  Expecting >CAMERA_END<\n", token);
			return(FALSE);
		}
	} while (strcmp(token, "CAMERA_END"));

	return(TRUE);
	
}

int Camera::parse_out(Dataport *fp, int expand)
{
	put_token(fp, "CAM_V1");

	Obj::parse_out(fp);

	put_token(fp, "\nVERSION");
	put_token(fp, "1.0");
	put_token(fp, "\nFIELD_OF_VIEW");
	fov.parse_out(fp, expand);
	put_token(fp, "\nIMAGE_INFO");
	if(image_info) {
		if(expand || !image_info->get_reference()) {
			put_token(fp, "{");
			image_info->parse_out(fp, expand);
			put_token(fp, "}");
		} else {
			put_token(fp, image_info->get_reference());
		}
	} else {
		put_token(fp, "NULL");
	}

	put_token(fp, "\nCAMERA_END\n");

	return(TRUE);
}


