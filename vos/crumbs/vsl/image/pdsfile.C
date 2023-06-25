// pdsfile.C 1.9 03/03/13 08:44:31
//
// Written by John Wright 02/10/98
//
#include "image/types/pdsfile.h"
#include "image/geoimage.h"
#include "image/datatypes.h"
#include <libgen.h>
#include "machine_setup.h"

int PDSFile::read_header(char *fname)
{
int     e=0;
char	*local_key_value, buff[1024];;
PDSGeoData	*pdsgd;
CameraModelGeoData	*camgd;

	reset_error();
	if(!img) return error=ImageFileErr_NoImage;	// check for an Image
	if(fname) set_filename(fname);
	free_comments(); free_flags();
	if(open(FILE_READ)) return error;
	fread(buff,1023); buff[1023] = '\0';
	local_key_value = buff;  while(*local_key_value == ' ') local_key_value++;
	//!!!!ozp if(strncmp(local_key_value, "CCSD", 4) && strncmp(local_key_value, "PDS_", 4))return(error=ImageFileErr_BadFile);
	if(strncmp(local_key_value, "CCSD", 4) && strncmp(local_key_value, "PDS_", 4) && strncmp(local_key_value, "ODL_", 4))return(error=ImageFileErr_BadFile);

	// use lablib3 routines for label access
	if(label = OdlParseLabelFile(get_filename(), err_file, (MASK)NULL, (unsigned short)NULL)) {
		// get image info
		if(image_obj = OdlFindObjDesc (label, (char *)"IMAGE", NULL, NULL, 1, ODL_RECURSIVE_DOWN)) {
			local_key_value = get_key_value(image_obj, (char *)"LINES");
			if(local_key_value) file_yres = atoi(local_key_value);
			else e=ImageFileErr_BadFile;
			local_key_value = get_key_value(image_obj, (char *)"LINE_SAMPLES");
			if(local_key_value) file_xres = atoi(local_key_value);
			else e=ImageFileErr_BadFile;
			file_bands = 1;
			local_key_value = get_key_value(image_obj, (char *)"BANDS");
			if(local_key_value) file_bands = atoi(local_key_value);
			// get georeferencing info if there
//fprintf(stderr,"Checking for georeferencing in pds file.\n");
			if((img->image_class_type() == GEOIMAGE) && 
				(georef_obj = OdlFindObjDesc (label, (char *)"IMAGE_MAP_PROJECTION_CATALOG", NULL, NULL, 1, ODL_RECURSIVE_DOWN))) {
//fprintf(stderr,"Found the block IMAGE_MAP_PROJECTION_CATALOG in pds file.\n");
				pdsgd = new PDSGeoData();
				local_key_value = get_key_value(georef_obj, (char *)"^DATA_SET_MAP_PROJECTION_CATALOG");
				if(local_key_value) pdsgd->set_catalog(local_key_value);
				local_key_value = get_key_value(georef_obj, (char *)"MAP_PROJECTION_TYPE");
				pdsgd->set_projection(pdsgd->projection_type_from_string(local_key_value));
				local_key_value = get_key_value(georef_obj, (char *)"MAP_RESOLUTION");
				if(local_key_value) pdsgd->set_resolution(atof(local_key_value)); // units?? = pixels/deg
				local_key_value = get_key_value(georef_obj, (char *)"MAP_SCALE");
				if(local_key_value) pdsgd->set_scale(atof(local_key_value)); // units?? = km/pixel
				local_key_value = get_key_value(georef_obj, (char *)"MAXIMUM_LATITUDE");
				if(local_key_value) pdsgd->set_max_latitude(atof(local_key_value)); // units?? = degrees
				local_key_value = get_key_value(georef_obj, (char *)"MINIMUM_LATITUDE");
				if(local_key_value) pdsgd->set_min_latitude(atof(local_key_value)); // units?? = degrees
				local_key_value = get_key_value(georef_obj, (char *)"MAXIMUM_LONGITUDE");
				if(local_key_value) pdsgd->set_max_longitude(atof(local_key_value)); // units?? = degrees
				local_key_value = get_key_value(georef_obj, (char *)"MINIMUM_LONGITUDE");
				if(local_key_value) pdsgd->set_min_longitude(atof(local_key_value)); // units?? = degrees
				local_key_value = get_key_value(georef_obj, (char *)"X_AXIS_PROJECTION_OFFSET");
				if(local_key_value) pdsgd->set_x_axis_offset(atof(local_key_value)); // units??
				local_key_value = get_key_value(georef_obj, (char *)"Y_AXIS_PROJECTION_OFFSET");
				if(local_key_value) pdsgd->set_y_axis_offset(atof(local_key_value)); // units??
				local_key_value = get_key_value(georef_obj, (char *)"A_AXIS_RADIUS");
				if(local_key_value) pdsgd->set_a_axis_radius(atof(local_key_value)); // units??
				local_key_value = get_key_value(georef_obj, (char *)"B_AXIS_RADIUS");
				if(local_key_value) pdsgd->set_b_axis_radius(atof(local_key_value)); // units??
				local_key_value = get_key_value(georef_obj, (char *)"C_AXIS_RADIUS");
				if(local_key_value) pdsgd->set_c_axis_radius(atof(local_key_value)); // units??
				local_key_value = get_key_value(georef_obj, (char *)"FIRST_STANDARD_PARALLEL");
				if(local_key_value) pdsgd->set_first_std_parallel(atof(local_key_value)); // units??
				local_key_value = get_key_value(georef_obj, (char *)"SECOND_STANDARD_PARALLEL");
				if(local_key_value) pdsgd->set_second_std_parallel(atof(local_key_value)); // units??
				local_key_value = get_key_value(georef_obj, (char *)"POSITIVE_LONGITUDE_DIRECTION");
				if(local_key_value) {
					if(strchr(local_key_value, 'W') || strchr(local_key_value, 'w')) {
						pdsgd->set_long_dir_sign(-1.0);
					} else {
						pdsgd->set_long_dir_sign(1.0);
					}
				}
				local_key_value = get_key_value(georef_obj, (char *)"CENTER_LATITUDE");
				if(local_key_value) pdsgd->set_center_lat(atof(local_key_value)); // units??
				local_key_value = get_key_value(georef_obj, (char *)"CENTER_LONGITUDE");
				if(local_key_value) pdsgd->set_center_long(atof(local_key_value)); // units??
				local_key_value = get_key_value(georef_obj, (char *)"REFERENCE_LATITUDE");
				if(local_key_value) pdsgd->set_reference_lat(atof(local_key_value)); // units??
				local_key_value = get_key_value(georef_obj, (char *)"REFERENCE_LONGITUDE");
				if(local_key_value) pdsgd->set_reference_long(atof(local_key_value)); // units??
				local_key_value = get_key_value(georef_obj, (char *)"X_AXIS_FIRST_PIXEL");
				if(local_key_value) pdsgd->set_min_x(atoi(local_key_value)); // units??
				local_key_value = get_key_value(georef_obj, (char *)"X_AXIS_LAST_PIXEL");
				if(local_key_value) pdsgd->set_max_x(atoi(local_key_value)); // units??
				local_key_value = get_key_value(georef_obj, (char *)"Y_AXIS_FIRST_PIXEL");
				if(local_key_value) pdsgd->set_min_y(atoi(local_key_value)); // units??
				local_key_value = get_key_value(georef_obj, (char *)"Y_AXIS_LAST_PIXEL");
				if(local_key_value) pdsgd->set_max_y(atoi(local_key_value)); // units??
				local_key_value = get_key_value(georef_obj, (char *)"MAP_PROJECTION_ROTATION");
				if(local_key_value) pdsgd->set_map_rotn(atof(local_key_value)); // units??

				((GeoImage *)img)->set_geo_data(pdsgd);
			} else if(img->image_class_type() == GEOIMAGE && 
				(georef_obj = OdlFindObjDesc (label, (char *)"GEOMETRIC_CAMERA_MODEL", NULL, NULL, 1, ODL_RECURSIVE_DOWN))) {
//fprintf(stderr,"Found the block GEOMETRIC_CAMERA_MODEL in pds file.\n");
				camgd = new CameraModelGeoData();
				double c[3], a[3], h[3], v[3], o[3], r[3], e[3];
				local_key_value = get_key_value(georef_obj, (char *)"MODEL_COMPONENT_1");
				if(local_key_value) {
					sscanf(local_key_value,"(%lf,%lf,%lf)", &c[0], &c[1], &c[2]);
				}
				local_key_value = get_key_value(georef_obj, (char *)"MODEL_COMPONENT_2");
				if(local_key_value) {
					sscanf(local_key_value,"(%lf,%lf,%lf)", &a[0], &a[1], &a[2]);
				}
				local_key_value = get_key_value(georef_obj, (char *)"MODEL_COMPONENT_3");
				if(local_key_value) {
					sscanf(local_key_value,"(%lf,%lf,%lf)", &h[0], &h[1], &h[2]);
				}
				local_key_value = get_key_value(georef_obj, (char *)"MODEL_COMPONENT_4");
				if(local_key_value) {
					sscanf(local_key_value,"(%lf,%lf,%lf)", &v[0], &v[1], &v[2]);
				}
				local_key_value = get_key_value(georef_obj, (char *)"MODEL_COMPONENT_5");
				if(local_key_value) {
					sscanf(local_key_value,"(%lf,%lf,%lf)", &o[0], &o[1], &o[2]);
				}
				local_key_value = get_key_value(georef_obj, (char *)"MODEL_COMPONENT_6");
				if(local_key_value) {
					sscanf(local_key_value,"(%lf,%lf,%lf)", &r[0], &r[1], &r[2]);
				}
				local_key_value = get_key_value(georef_obj, (char *)"MODEL_COMPONENT_7");
				if(local_key_value) {
					sscanf(local_key_value,"(%lf,%lf,%lf)", &e[0], &e[1], &e[2]);
				}
				local_key_value = get_key_value(georef_obj, (char *)"MODEL_TYPE");
				JPLCamera *cam_model = NULL;
//fprintf(stderr,"Checking model type which is %s\n", local_key_value);
				if(local_key_value && !strncmp(local_key_value, "CAHVORE", 7)) {
//fprintf(stderr,"It is a CAHVORE model so setting it as so\n");
					cam_model = new JPLCamera();
					cam_model->InitJPLCamera(c, a, h, v, o, r, e);
				} else if(local_key_value && !strncmp(local_key_value, "CAHVOR", 6)) {
//fprintf(stderr,"It is a CAHVOR model so setting it as so\n");
					cam_model = new JPLCamera();
					cam_model->InitJPLCamera(c, a, h, v, o, r);
				} else if(local_key_value && !strncmp(local_key_value, "CAHV", 4)) {
//fprintf(stderr,"It is a CAHV model so setting it as so\n");
					cam_model = new JPLCamera();
					cam_model->InitJPLCamera(c, a, h, v);
				}
				camgd->set_cameraModel(cam_model);
				((GeoImage *)img)->set_geo_data(camgd);
				
			}
		} else {
			e=ImageFileErr_BadFile;
		}
	} else {
		e=ImageFileErr_BadFile;
	}

return error=e;
}

int PDSFile::read_image(char *fname)
{
	int e, rec_len=1, local_blocks;
	char	*local_key_value, lfname[1024], tfname[2048];
	FILE	*lfp;
	KEYWORD *key;
	OBJDESC *palette;

	e = read_header(fname);
	if(!e) {
		// get needed keys from label
		// get basic file info
		if(external_file)free(external_file);
		external_file = get_key_value(label, (char *)"FILE_NAME");
		local_key_value = get_key_value(label, (char *)"RECORD_TYPE");
		if(!strcmp(local_key_value, (char *)"FIXED_LENGTH")) {
			local_key_value = get_key_value(label, (char *)"RECORD_BYTES");
			if(local_key_value) rec_len = atoi(local_key_value);
		} else if(!strcmp(local_key_value, (char *)"VARIABLE_LENGTH")) {
		} else if(!strcmp(local_key_value, (char *)"STREAM")) {
		} else if(!strcmp(local_key_value, (char *)"UNDEFINED")) {
			fprintf(stderr,"Processing UNDEFINED Label Record Type as STREAM\n");
		} else {  // very bad as not recognized
			fprintf(stderr,"Whoops - Unrecognized RECORD_TYPE = %s\n", local_key_value);
			e=ImageFileErr_BadFile;
		}

		if(!e) {
			// get file pointer to right file and read data
			local_key_value = get_key_value(label, (char *)"^IMAGE");
			if(local_key_value) {
				if(local_key_value[0] >= '0' && local_key_value[0] <= '9') {
					// use block length to get to start of data
					local_blocks = atoi(local_key_value);
					local_blocks--;
					key = OdlFindKwd(label, (char *)"^IMAGE", NULL, 0, ODL_RECURSIVE_DOWN);
					local_key_value = OdlGetKwdUnit(key);
					local_blocks *= rec_len; // convert to bytes
					if(local_key_value) {
						if(strstr(local_key_value, "BYTES")) {
							// unconvert if already bytes
							local_blocks /= rec_len;
						}
					}
					rewind();
					seek(local_blocks);
					read_data(get_fp());
				} else if(local_key_value[0] == '(') {
					// image is in a different file (lfname, blocks [<units>])
					sscanf(&(local_key_value[1]), " \"%[^\"]\",%d", lfname, &local_blocks);
// fprintf(stderr,"Looking for PDS image data in file %s\n", lfname);
					local_blocks--;
					// see if in local dir or path is full
					if(lfp=::fopen(lfname,"r")) {  
					} else {
						// not so try adding path from input filename
						strcpy(tfname, get_filename());
						strcpy(tfname, dirname(tfname));
						strcat(tfname, "/");
						strcat(tfname, lfname);
// fprintf(stderr,"Looking for PDS image data in file %s\n", tfname);
						if(lfp=::fopen(tfname,"r")) {
						} else {
							// path doesn't work so try converting to lower case
							char *ptr = lfname;
							while(*ptr) { *ptr = tolower(*ptr); ptr++; }
// fprintf(stderr,"Looking for PDS image data in file %s\n", lfname);
							if(lfp=::fopen(lfname,"r")) {
							} else {
								// lower case no good so try adding path again
								strcpy(tfname, get_filename());
								strcpy(tfname, dirname(tfname));
								strcat(tfname, "/");
								strcat(tfname, lfname);
// fprintf(stderr,"Looking for PDS image data in file %s\n", tfname);
								lfp=::fopen(tfname,"r");  // last chance
							}
						}
					}
					if(lfp) {
						key = OdlFindKwd(label, (char *)"^IMAGE", NULL, 0, ODL_RECURSIVE_DOWN);
						local_key_value = OdlGetKwdUnit(key);
						local_blocks *= rec_len; // convert to bytes
						if(local_key_value) {
							if(strstr(local_key_value, "BYTES")) {
								// unconvert if already bytes
								local_blocks /= rec_len;
							}
						}
						::fseek(lfp, local_blocks, SEEK_SET);
						read_data(lfp);
					} else {
fprintf(stderr,"Whoops - Unable to open PDS external data file %s\n", lfname);
						e=ImageFileErr_BadFile;
					}
				} else {
fprintf(stderr,"Whoops - Unable to recognize ^IMAGE key %s\n", local_key_value);
					e=ImageFileErr_BadFile;
				}
			} else {
				e=ImageFileErr_BadFile;
			}
		}
		if(!e) {	// look for and load color lookup table
			if(palette = OdlFindObjDesc (label, (char *)"PALETTE", NULL, NULL, 1, ODL_RECURSIVE_DOWN)) {
fprintf(stderr,"Whoops - PDS Files with color palettes not implemented yet!!\n");
fprintf(stderr,"File will be treated as grayscale image.\n");
fprintf(stderr,"Palette reference=%x\n", palette);  // included to avoid compiler warnings for this unimplemented
			}
		}
	}
	return(error = e);
}

template<class T> int read_ascii_real_data(int file_xres, int file_yres, int file_bands, int pre_bytes, int post_bytes, long , char *local_key_value, FILE *f, ImageData *idat, T dummy)
{
	int i,j,k,e;
	unsigned char buff[32768];
	long local;

	if(dummy)return(ImageFileErr_BadImageType);
	if(!idat) return(ImageFileErr_BadImageType);
	if(!(idat->access() & DATA_WRITE))
		e=ImageFileErr_BadImageType;
	else if(!idat->allocate(file_xres, file_yres, file_bands)) 
		e=ImageFileErr_AllocFailed;
	else {
		// read data into idat
		// determine band ordering
		if(!local_key_value || !strcmp(local_key_value,"BAND_SEQUENTIAL")) {
			for(k=0; k<file_bands; k++) {
				for(i=0; i<file_yres; i++) {
					if(pre_bytes) ::fread(buff, pre_bytes, 1, f);
					for(j=0; j<file_xres; j++) {
						fscanf(f, "%ld", &local);
						idat->set(local, j, i, k);
					}
					if(post_bytes) ::fread(buff, post_bytes, 1, f);
				}
			}
		} else if(!strcmp(local_key_value,"LINE_INTERLEAVED")) {
			for(i=0; i<file_yres; i++) {
				for(k=0; k<file_bands; k++) {
					if(pre_bytes) ::fread(buff, pre_bytes, 1, f);
					for(j=0; j<file_xres; j++) {
						fscanf(f, "%ld", &local);
						idat->set(local, j, i, k);
					}
					if(post_bytes) ::fread(buff, post_bytes, 1, f);
				}
			}
		} else if(!strcmp(local_key_value,"SAMPLE_INTERLEAVED")) {
			for(i=0; i<file_yres; i++) {
				// skip prefix bytes
				if(pre_bytes) ::fread(buff, pre_bytes, 1, f);
				for(j=0; j<file_xres; j++) {
					for(k=0; k<file_bands; k++) {
						fscanf(f, "%ld", &local);
						idat->set(local, j, i, k);
					}
				}
				if(post_bytes) ::fread(buff, post_bytes, 1, f);
			}
		} else {
fprintf(stderr,"Whoops - Unrecognized Band Storage Type = %s\n", local_key_value);
			e=ImageFileErr_BadImageType;
		}
	}

	return(e);
}

// byte-swap one value to native endianness
// n = number of bytes: 2 for short, 4 for float, 8 for double
#define SWAP(x,y)	a=bp[x]; bp[x]=bp[y]; bp[y]=a;

void bswap(unsigned char *bp, int n)
{
	unsigned char a;
	if(n == 2) {
		SWAP(0,1);
	} else if (n == 4) {
		SWAP(0,3);
		SWAP(1,2);
	} else if (n == 8) {
		SWAP(0,7);
		SWAP(1,6);
		SWAP(2,5);
		SWAP(3,4);
	}
}

// read binary float or double image data, optionally byte-swap
template<class T> int read_real_data(int file_xres, int file_yres, 
	int file_bands, int pre_bytes, int post_bytes, long , 
	char *local_key_value, FILE *f, ImageData *idat, int swap, T dummy)
{
	int i,j,k,e;
	unsigned char buff[32768];
	T *local;

	local = (T *)buff;
	if(dummy)return(ImageFileErr_BadImageType);
	if(!idat) return(ImageFileErr_BadImageType);
	if(!(idat->access() & DATA_WRITE))
		e=ImageFileErr_BadImageType;
	else if(!idat->allocate(file_xres, file_yres, file_bands)) 
		e=ImageFileErr_AllocFailed;
	else {
		// read data into idat
		// determine band ordering
		if(!local_key_value || !strcmp(local_key_value,"BAND_SEQUENTIAL")) {
			for(k=0; k<file_bands; k++) {
				for(i=0; i<file_yres; i++) {
					if(pre_bytes) ::fread(buff, pre_bytes, 1, f);
					for(j=0; j<file_xres; j++) {
						::fread(buff, sizeof(T), 1, f);
						if (swap)
							bswap(buff, sizeof(T));
						idat->set(*local, j, i, k);
					}
					if(post_bytes) ::fread(buff, post_bytes, 1, f);
				}
			}
		} else if(!strcmp(local_key_value,"LINE_INTERLEAVED")) {
			for(i=0; i<file_yres; i++) {
				for(k=0; k<file_bands; k++) {
					if(pre_bytes) ::fread(buff, pre_bytes, 1, f);
					for(j=0; j<file_xres; j++) {
						::fread(buff, sizeof(T), 1, f);
						if (swap)
							bswap(buff, sizeof(T));
						idat->set(*local, j, i, k);
					}
					if(post_bytes) ::fread(buff, post_bytes, 1, f);
				}
			}
		} else if(!strcmp(local_key_value,"SAMPLE_INTERLEAVED")) {
			for(i=0; i<file_yres; i++) {
				// skip prefix bytes
				if(pre_bytes) ::fread(buff, pre_bytes, 1, f);
				for(j=0; j<file_xres; j++) {
					for(k=0; k<file_bands; k++) {
						if (swap)
							bswap(buff, sizeof(T));
						::fread(buff, sizeof(T), 1, f);
						idat->set(*local, j, i, k);
					}
				}
				if(post_bytes) ::fread(buff, post_bytes, 1, f);
			}
		} else {
			fprintf(stderr,"Whoops - Unrecognized Band Storage"
					" Type = %s\n", local_key_value);
			e=ImageFileErr_BadImageType;
		}
	}

	return(e);
}

template<class T> int read_ascii_int_data(int file_xres, int file_yres, int file_bands, int pre_bytes, int post_bytes, long mask, char *local_key_value, FILE *f, ImageData *idat, T dummy)
{
	int i,j,k,e;
	unsigned char buff[32768];
	T local_mask = (T)mask;
	long local;

	if(dummy)return(ImageFileErr_BadImageType);
	if(!idat) return(ImageFileErr_BadImageType);
	if(!(idat->access() & DATA_WRITE))
		e=ImageFileErr_BadImageType;
	else if(!idat->allocate(file_xres, file_yres, file_bands)) 
		e=ImageFileErr_AllocFailed;
	else {
		// read data into idat
		// determine band ordering
		if(!local_key_value || !strcmp(local_key_value,"BAND_SEQUENTIAL")) {
			for(k=0; k<file_bands; k++) {
				for(i=0; i<file_yres; i++) {
					if(pre_bytes) ::fread(buff, pre_bytes, 1, f);
					for(j=0; j<file_xres; j++) {
						fscanf(f, "%ld", &local);
						local &= local_mask;
						idat->set(local, j, i, k);
					}
					if(post_bytes) ::fread(buff, post_bytes, 1, f);
				}
			}
		} else if(!strcmp(local_key_value,"LINE_INTERLEAVED")) {
			for(i=0; i<file_yres; i++) {
				for(k=0; k<file_bands; k++) {
					if(pre_bytes) ::fread(buff, pre_bytes, 1, f);
					for(j=0; j<file_xres; j++) {
						fscanf(f, "%ld", &local);
						local &= local_mask;
						idat->set(local, j, i, k);
					}
					if(post_bytes) ::fread(buff, post_bytes, 1, f);
				}
			}
		} else if(!strcmp(local_key_value,"SAMPLE_INTERLEAVED")) {
			for(i=0; i<file_yres; i++) {
				// skip prefix bytes
				if(pre_bytes) ::fread(buff, pre_bytes, 1, f);
				for(j=0; j<file_xres; j++) {
					for(k=0; k<file_bands; k++) {
						fscanf(f, "%ld", &local);
						local &= local_mask;
						idat->set(local, j, i, k);
					}
				}
				if(post_bytes) ::fread(buff, post_bytes, 1, f);
			}
		} else {
			fprintf(stderr,"Whoops - Unrecognized Band Storage"
					" Type = %s\n", local_key_value);
			e=ImageFileErr_BadImageType;
		}
	}

	return(e);
}

template<class T> int read_int_data(int file_xres, int file_yres, int file_bands, int pre_bytes, int post_bytes, long mask, char *local_key_value, FILE *f, ImageData *idat, int swap, T dummy)
{
	int i,j,k,e;
	unsigned char buff[32768];
	T local_mask = (T)mask;
	T *local;

	local = (T *)buff;
	if(dummy)return(ImageFileErr_BadImageType);
	if(!idat) return(ImageFileErr_BadImageType);
	if(!(idat->access() & DATA_WRITE))
		e=ImageFileErr_BadImageType;
	else if(!idat->allocate(file_xres, file_yres, file_bands)) 
		e=ImageFileErr_AllocFailed;
	else {
		// read data into idat
		// determine band ordering
		if(!local_key_value || !strcmp(local_key_value,"BAND_SEQUENTIAL")) {
			for(k=0; k<file_bands; k++) {
				for(i=0; i<file_yres; i++) {
					if(pre_bytes) ::fread(buff, pre_bytes, 1, f);
					for(j=0; j<file_xres; j++) {
						::fread(buff, sizeof(T), 1, f);
						if (swap)
							bswap(buff, sizeof(T));
						*local &= local_mask;
						idat->set(*local, j, i, k);
					}
					if(post_bytes) ::fread(buff, post_bytes, 1, f);
				}
			}
		} else if(!strcmp(local_key_value,"LINE_INTERLEAVED")) {
			for(i=0; i<file_yres; i++) {
				for(k=0; k<file_bands; k++) {
					if(pre_bytes) ::fread(buff, pre_bytes, 1, f);
					for(j=0; j<file_xres; j++) {
						::fread(buff, sizeof(T), 1, f);
						if (swap)
							bswap(buff, sizeof(T));
						*local &= local_mask;
						idat->set(*local, j, i, k);
					}
					if(post_bytes) ::fread(buff, post_bytes, 1, f);
				}
			}
		} else if(!strcmp(local_key_value,"SAMPLE_INTERLEAVED")) {
			for(i=0; i<file_yres; i++) {
				// skip prefix bytes
				if(pre_bytes) ::fread(buff, pre_bytes, 1, f);
				for(j=0; j<file_xres; j++) {
					for(k=0; k<file_bands; k++) {
						::fread(buff, sizeof(T), 1, f);
						if (swap)
							bswap(buff, sizeof(T));
						*local &= local_mask;
						idat->set(*local, j, i, k);
					}
				}
				if(post_bytes) ::fread(buff, post_bytes, 1, f);
			}
		} else {
			fprintf(stderr,"Whoops - Unrecognized Band Storage"
					" Type = %s\n", local_key_value);
			e=ImageFileErr_BadImageType;
		}
	}

	return(e);
}


int PDSFile::read_data(FILE *f)
{
	int e, pre_bytes=0, post_bytes=0, local_bits=8, i;
	int swap;
	long local_mask=-1;
	char	*local_key_value;
	ImageData	*idat;
	// dummy declarations to help templates under linux
	double	double_dummy=0.0;
	float	float_dummy =0.0;

// fprintf(stderr,"In pds read_data\n");

	// read in image data and store in ImageData object
	// get image specific info
	local_key_value = get_key_value(image_obj, (char *)"LINE_PREFIX_BYTES");
	if(local_key_value) pre_bytes = atoi(local_key_value);
	local_key_value = get_key_value(image_obj, (char *)"LINE_SUFFIX_BYTES");
	if(local_key_value) post_bytes = atoi(local_key_value);
	local_key_value = get_key_value(image_obj, (char *)"SAMPLE_BIT_MASK");
	if(local_key_value) {
		local_mask = 0;
		for(i=2; i<34 && (local_key_value[i]=='0' || local_key_value[i]=='1'); i++) {
			local_mask <<= 1;
			if(local_key_value[i] == '1')local_mask++;
		}
	}
	local_key_value = get_key_value(image_obj, (char *)"SAMPLE_BITS");
	if(local_key_value) local_bits = atoi(local_key_value);

	// allocate image data object
	
	local_key_value = get_key_value(image_obj, (char *)"SAMPLE_TYPE");
	if(strstr(local_key_value, "INTEGER")) {
		if(!strcmp(local_key_value,"INTEGER") ||
			!strcmp(local_key_value,"MSB_INTEGER") ||
			!strcmp(local_key_value,"MAC_INTEGER") ||
			!strcmp(local_key_value,"SUN_INTEGER") ) {
			swap = INT_BIGENDIAN ? 0 : 1;
			if(local_bits <= 8) {
				// allocate a char data object
				idat = *img;
				if(!idat) {
					idat = new charData();
					*img = idat;
				}
				local_key_value = get_key_value(image_obj, (char *)"BAND_STORAGE_TYPE");
				e = read_int_data(file_xres, file_yres, file_bands, pre_bytes, post_bytes, 
					local_mask, local_key_value, f, idat, swap, (char)NULL);
			} else if(local_bits <= 16) {
				// allocate a short data object
				idat = *img;
				if(!idat) {
					idat = new shortData();
					*img = idat;
				}
				local_key_value = get_key_value(image_obj, (char *)"BAND_STORAGE_TYPE");
				e = read_int_data(file_xres, file_yres, file_bands, pre_bytes, post_bytes, 
					local_mask, local_key_value, f, idat, swap, (short)NULL);
			} else if(local_bits <= 32) {
				// allocate a long data object
				idat = *img;
				if(!idat) {
					idat = new longData();
					*img = idat;
				}
				local_key_value = get_key_value(image_obj, (char *)"BAND_STORAGE_TYPE");
				e = read_int_data(file_xres, file_yres, file_bands, pre_bytes, post_bytes, 
					local_mask, local_key_value, f, idat, swap, (long)NULL);
			} else {
				fprintf(stderr,"Whoops - Unable to handle data of type %s"
					" with more than 4 bytes\n", local_key_value);
				e=ImageFileErr_BadFile;
			}
		} else if(!strcmp(local_key_value,"ASCII_INTEGER") ) {
			swap = 0; // irrelevant for ASCII data
			if(local_bits <= 8) {
				// allocate a char data object
				idat = *img;
				if(!idat) {
					idat = new charData();
					*img = idat;
				}
				local_key_value = get_key_value(image_obj, (char *)"BAND_STORAGE_TYPE");
				e = read_ascii_int_data(file_xres, file_yres, file_bands, pre_bytes, post_bytes, 
					local_mask, local_key_value, f, idat, (char)NULL);
			} else if(local_bits <= 16) {
				// allocate a short data object
				idat = *img;
				if(!idat) {
					idat = new shortData();
					*img = idat;
				}
				local_key_value = get_key_value(image_obj, (char *)"BAND_STORAGE_TYPE");
				e = read_ascii_int_data(file_xres, file_yres, file_bands, pre_bytes, post_bytes, 
					local_mask, local_key_value, f, idat, (short)NULL);
			} else if(local_bits <= 32) {
				// allocate a long data object
				idat = *img;
				if(!idat) {
					idat = new longData();
					*img = idat;
				}
				local_key_value = get_key_value(image_obj, (char *)"BAND_STORAGE_TYPE");
				e = read_ascii_int_data(file_xres, file_yres, file_bands, pre_bytes, post_bytes, 
					local_mask, local_key_value, f, idat, (long)NULL);
			} else {
				fprintf(stderr,"Whoops - Unable to handle data of type %s"
					" with more than 4 bytes\n", local_key_value);
				e=ImageFileErr_BadFile;
			}
		} else if(!strcmp(local_key_value,"MSB_UNSIGNED_INTEGER") ||
			!strcmp(local_key_value,"UNSIGNED_INTEGER") ||
			!strcmp(local_key_value,"SUN_UNSIGNED_INTEGER") ) {
			swap = INT_BIGENDIAN ? 0 : 1;
			if(local_bits <= 8) {
				// allocate an unsigned char data object
				idat = *img;
				if(!idat) {
					idat = new ucharData();
					*img = idat;
				}
				local_key_value = get_key_value(image_obj, (char *)"BAND_STORAGE_TYPE");
				e = read_int_data(file_xres, file_yres, file_bands, pre_bytes, post_bytes, 
					local_mask, local_key_value, f, idat, swap, (uchar)NULL);
			} else if(local_bits <= 16) {
				// allocate an unsigned short data object
				idat = *img;
				if(!idat) {
					idat = new ushortData();
					*img = idat;
				}
				local_key_value = get_key_value(image_obj, (char *)"BAND_STORAGE_TYPE");
				e = read_int_data(file_xres, file_yres, file_bands, pre_bytes, post_bytes, 
					local_mask, local_key_value, f, idat, swap, (ushort)NULL);
			} else if(local_bits <= 32) {
				// allocate an unsigned long data object
				idat = *img;
				if(!idat) {
					idat = new ulongData();
					*img = idat;
				}
				local_key_value = get_key_value(image_obj, (char *)"BAND_STORAGE_TYPE");
				e = read_int_data(file_xres, file_yres, file_bands, pre_bytes, post_bytes, 
					local_mask, local_key_value, f, idat, swap, (ulong)NULL);
			} else {
				fprintf(stderr,"Whoops - Unable to handle data of type %s"
					" with more than 4 bytes\n", local_key_value);
				e=ImageFileErr_BadFile;
			}
		} else {
			fprintf(stderr,"Whoops - Unable to handle data of type %s\n", local_key_value);
			e=ImageFileErr_BadFile;
		}
	} else if(strstr(local_key_value, "REAL")) {
		swap = -1;		// unknown
		if(!strcmp(local_key_value,"REAL") ||
				!strcmp(local_key_value,"FLOAT"))
			swap = 0;	// assume native endian?
		else if (!strcmp(local_key_value,"SUN_REAL") ||
				!strcmp(local_key_value,"IEEE_REAL") ||
				!strcmp(local_key_value,"MAC_REAL"))
			swap = REAL_BIGENDIAN ? 0 : 1;
		else if (!strcmp(local_key_value,"RIEEE_REAL"))
			swap = REAL_BIGENDIAN ? 1 : 0;
		if (swap >= 0) {	// known binary real format
			if(local_bits <= 32) {
				// allocate a float data object
				idat = *img;
				if(!idat) {
					idat = new floatData();
					*img = idat;
				}
				local_key_value = get_key_value(image_obj, (char *)"BAND_STORAGE_TYPE");
				e = read_real_data(file_xres, file_yres, file_bands, pre_bytes, post_bytes, 
					local_mask, local_key_value, f, idat, swap, float_dummy);
			} else if(local_bits <= 64) {
				// allocate a double data object
				idat = *img;
				if(!idat) {
					idat = new floatData();
					*img = idat;
				}
				local_key_value = get_key_value(image_obj, (char *)"BAND_STORAGE_TYPE");
				e = read_real_data(file_xres, file_yres, file_bands, pre_bytes, post_bytes, 
					local_mask, local_key_value, f, idat, swap, double_dummy);
			} else {
				fprintf(stderr,"Whoops - Unable to handle data of type %s"
					" with more than 8 bytes\n", local_key_value);
				e=ImageFileErr_BadFile;
			}
		} else if(!strcmp(local_key_value,"ASCII_REAL")) {
			if(local_bits <= 32) {
				// allocate a float data object
				idat = *img;
				if(!idat) {
					idat = new floatData();
					*img = idat;
				}
				local_key_value = get_key_value(image_obj, (char *)"BAND_STORAGE_TYPE");
				e = read_ascii_real_data(file_xres, file_yres, file_bands, pre_bytes, post_bytes, 
					local_mask, local_key_value, f, idat, float_dummy);
			} else if(local_bits <= 64) {
				// allocate a double data object
				idat = *img;
				if(!idat) {
					idat = new floatData();
					*img = idat;
				}
				local_key_value = get_key_value(image_obj, (char *)"BAND_STORAGE_TYPE");
				e = read_ascii_real_data(file_xres, file_yres, file_bands, pre_bytes, post_bytes, 
					local_mask, local_key_value, f, idat, double_dummy);
			} else {
				fprintf(stderr,"Whoops - Unable to handle data of type %s"
					" with more than 8 bytes\n", local_key_value);
				e=ImageFileErr_BadFile;
			}
		} else {
			fprintf(stderr,"Whoops - Unable to handle data of type %s\n", local_key_value);
			e=ImageFileErr_BadFile;
		}
			
	} else {
		fprintf(stderr,"Whoops - Unrecognized SAMPLE_TYPE = %s\n", local_key_value);
		e=ImageFileErr_BadFile;
	}

	local_key_value = get_key_value(image_obj, (char *)"BAND_SEQUENCE");
	if(!local_key_value) local_key_value = get_key_value(image_obj, (char *)"BAND_NAME");
	if(local_key_value) {
		int rband=0, gband=0, bband=0, bnd=0;
		char *ptr = local_key_value, lband[1024];;
		ptr = strstr(ptr,"(");
//		while(ptr && bnd < file_bands) {
		while(ptr) {
			ptr++;
			sscanf(ptr, "%[A-Za-z]", lband);
			if(lband[0] == 'R' || lband[0] == 'r') rband = bnd;
			if(lband[0] == 'G' || lband[0] == 'g') gband = bnd;
			if(lband[0] == 'B' || lband[0] == 'b') bband = bnd;
			bnd++;
			ptr = strstr(ptr,",");
		}
		idat->set_rgb_bands(rband,gband,bband);
	} else {
		if(file_bands == 1)idat->set_rgb_bands(0,0,0);
		else idat->set_rgb_bands(0,1,2);
	}
	
	idat->set_default_band();
	idat->set_alpha_band(-1);
	idat->free_map();
	
	return(error = e);
}

int PDSFile::write_image(char *fname)
{
	int	e=FileErr_None;

	close();
	set_filename(fname);
	if(!fopen(FILE_WRITE)) {
		e = write_data(get_fp());
	} else {
		fprintf(stderr,"Whoops - Unable to open file %s for writing.\n", fname);
		e = FileErr_CouldNotOpen;
	}

	return(error = e);
}



template<class T> int write_img_data(int xres, int yres, int bands, FILE *f, ImageData *idat, T )
{
	int i,j,k,e;
	T local;
	for(k=0; k<bands; k++) {
		for(j=0; j<yres; j++) {
			for(i=0; i<xres; i++) {
				idat->cx = i; idat->cy = j; idat->cband = k;
				local = (T)(*idat);
				e = ::fwrite(&local, sizeof(T), 1, f);
			}
		}
	}
	return(e);
}

int PDSFile::write_data(FILE *f)
{
#define	DEFAULT_HEADER_LENGTH 4096
// The DEFAULT_HEADER_LENGTH is the assumed length of the PDS label in bytes before the image data starts
// This value includes all the standard label stuff as well as any color palette when implemented
	int	curr_hdr_lng = 0, xres, yres, bands;
	char	buff[DEFAULT_HEADER_LENGTH];
	ImageData	*idat;
	GeoData		*geodat;
	PDSGeoData	*pdsgd;

	if(!f)error=1;

	if(!img) return error=ImageFileErr_NoImage;		// check for an Image

	if(f) attach(f);					// set file pointer

	// Get an image data pointer:
	idat=*img;
	if(!idat)
		return error=ImageFileErr_BadImageType;
	if(!(idat->access() & DATA_WRITE))
		return error=ImageFileErr_BadImageType;

	// Get resolution:
	idat->get_res(&xres,&yres,&bands);
	if(xres<1 || yres<1)
		return error=ImageFileErr_BadImageData;

	// Write the header:
	sprintf(buff,"CCSD3ZF0000100000001NJPL3IF0PDS200000001 = SFDU_LABEL\n");
	curr_hdr_lng += strlen(buff);
	fprintf(f,"%s", buff);
	sprintf(buff,"/* This image file was written by the JPL VESA Image Class */\n");
	curr_hdr_lng += strlen(buff);
	fprintf(f,"%s", buff);
	sprintf(buff,"/* The label is a fixed %d bytes in length followed by the image data.\n", DEFAULT_HEADER_LENGTH);
	curr_hdr_lng += strlen(buff);
	fprintf(f,"%s", buff);
	sprintf(buff,"RECORD_TYPE          = STREAM\n\n");
	curr_hdr_lng += strlen(buff);
	fprintf(f,"%s", buff);
	sprintf(buff,"^IMAGE               = %d <BYTES>\n\n", DEFAULT_HEADER_LENGTH+1);
	curr_hdr_lng += strlen(buff);
	fprintf(f,"%s", buff);

	// put color palette offset here if needed (^PALETTE???)

	sprintf(buff,"OBJECT               = IMAGE\n");
	curr_hdr_lng += strlen(buff);
	fprintf(f,"%s", buff);
	sprintf(buff,"  LINES              = %d\n", yres);
	curr_hdr_lng += strlen(buff);
	fprintf(f,"%s", buff);
	sprintf(buff,"  LINE_SAMPLES       = %d\n", xres);
	curr_hdr_lng += strlen(buff);
	fprintf(f,"%s", buff);
	if(bands > 1) {
		sprintf(buff,"  BANDS              = %d\n", bands);
		curr_hdr_lng += strlen(buff);
		fprintf(f,"%s", buff);
		sprintf(buff,"  BAND_SEQUENCE      = (R,G,B)\n");
		curr_hdr_lng += strlen(buff);
		fprintf(f,"%s", buff);
		sprintf(buff,"  BAND_STORAGE_TYPE  = BAND_SEQUENTIAL\n");
		curr_hdr_lng += strlen(buff);
		fprintf(f,"%s", buff);
	}
	if(idat->preferred_data_type() == CHAR_DATA) {
		sprintf(buff,"  SAMPLE_BITS        = 8\n");
		curr_hdr_lng += strlen(buff);
		fprintf(f,"%s", buff);
		sprintf(buff,"  SAMPLE_TYPE        = INTEGER\n");
		curr_hdr_lng += strlen(buff);
		fprintf(f,"%s", buff);
	} else if(idat->preferred_data_type() == UCHAR_DATA) {
		sprintf(buff,"  SAMPLE_BITS        = 8\n");
		curr_hdr_lng += strlen(buff);
		fprintf(f,"%s", buff);
		sprintf(buff,"  SAMPLE_TYPE        = UNSIGNED_INTEGER\n");
		curr_hdr_lng += strlen(buff);
		fprintf(f,"%s", buff);
	} else if(idat->preferred_data_type() == SHORT_DATA) {
		sprintf(buff,"  SAMPLE_BITS        = 16\n");
		curr_hdr_lng += strlen(buff);
		fprintf(f,"%s", buff);
		sprintf(buff,"  SAMPLE_TYPE        = INTEGER\n");
		curr_hdr_lng += strlen(buff);
		fprintf(f,"%s", buff);
	} else if(idat->preferred_data_type() == USHORT_DATA) {
		sprintf(buff,"  SAMPLE_BITS        = 16\n");
		curr_hdr_lng += strlen(buff);
		fprintf(f,"%s", buff);
		sprintf(buff,"  SAMPLE_TYPE        = UNSIGNED_INTEGER\n");
		curr_hdr_lng += strlen(buff);
		fprintf(f,"%s", buff);
	} else if(idat->preferred_data_type() == LONG_DATA) {
		sprintf(buff,"  SAMPLE_BITS        = 32\n");
		curr_hdr_lng += strlen(buff);
		fprintf(f,"%s", buff);
		sprintf(buff,"  SAMPLE_TYPE        = INTEGER\n");
		curr_hdr_lng += strlen(buff);
		fprintf(f,"%s", buff);
	} else if(idat->preferred_data_type() == ULONG_DATA) {
		sprintf(buff,"  SAMPLE_BITS        = 32\n");
		curr_hdr_lng += strlen(buff);
		fprintf(f,"%s", buff);
		sprintf(buff,"  SAMPLE_TYPE        = UNSIGNED_INTEGER\n");
		curr_hdr_lng += strlen(buff);
		fprintf(f,"%s", buff);
	} else if(idat->preferred_data_type() == FLOAT_DATA) {
		sprintf(buff,"  SAMPLE_BITS        = 32\n");
		curr_hdr_lng += strlen(buff);
		fprintf(f,"%s", buff);
		sprintf(buff,"  SAMPLE_TYPE        = %s\n",
			REAL_BIGENDIAN ? "IEEE_REAL" : "RIEEE_REAL");
		curr_hdr_lng += strlen(buff);
		fprintf(f,"%s", buff);
	} else if(idat->preferred_data_type() == DOUBLE_DATA) {
		sprintf(buff,"  SAMPLE_BITS        = 64\n");
		curr_hdr_lng += strlen(buff);
		fprintf(f,"%s", buff);
		sprintf(buff,"  SAMPLE_TYPE        = %s\n",
			REAL_BIGENDIAN ? "IEEE_REAL" : "RIEEE_REAL");
		curr_hdr_lng += strlen(buff);
		fprintf(f,"%s", buff);
	} else {
		sprintf(buff,"  SAMPLE_BITS        = 8\n");
		curr_hdr_lng += strlen(buff);
		fprintf(f,"%s", buff);
		sprintf(buff,"  SAMPLE_TYPE        = UNKNOWN\n");
		curr_hdr_lng += strlen(buff);
		fprintf(f,"%s", buff);
fprintf(stderr,"Whoops - Attempting to write a PDS file with an unexpected data type (BIT or COMPLEX?)\n");
	}
	sprintf(buff,"END_OBJECT           = IMAGE\n\n");
	curr_hdr_lng += strlen(buff);
	fprintf(f,"%s", buff);

// put color palette offset here if needed (^PALETTE???)


// check for georeferenced image
	if(img->image_class_type() == GEOIMAGE) {
		if(geodat = ((GeoImage *)img)->get_geo_data()) {
			// only know how to handle PDSGeoData so far
			if(geodat->data_class_type() == PDS_GEO_DATA) {
				pdsgd = (PDSGeoData *)geodat;
			} else {
				geodat = NULL;
			}
		}
	} else {
		geodat = NULL;
	}
	if(geodat) {
fprintf(stderr,"Writing georeferencing data\n");
		sprintf(buff,"OBJECT               = IMAGE_MAP_PROJECTION_CATALOG\n");
		curr_hdr_lng += strlen(buff);
		fprintf(f,"%s", buff);
		if(pdsgd->get_catalog()) {
			sprintf(buff,"  ^DATA_SET_MAP_PROJECTION_CATALOG = %s\n", pdsgd->get_catalog());
			curr_hdr_lng += strlen(buff);
			fprintf(f,"%s", buff);
		}
		sprintf(buff,"  MAP_PROJECTION_TYPE = %s\n", pdsgd->projection_type_to_string(pdsgd->get_projection()));
		curr_hdr_lng += strlen(buff);
		fprintf(f,"%s", buff);
		sprintf(buff,"  MAP_RESOLUTION     = %f  <PIXEL/DEG>\n", pdsgd->get_resolution());
		curr_hdr_lng += strlen(buff);
		fprintf(f,"%s", buff);
		sprintf(buff,"  MAP_SCALE          = %f  <KM/PIXEL>\n", pdsgd->get_scale());
		curr_hdr_lng += strlen(buff);
		fprintf(f,"%s", buff);
		sprintf(buff,"  MAXIMUM_LATITUDE   = %f\n", pdsgd->get_max_latitude());
		curr_hdr_lng += strlen(buff);
		fprintf(f,"%s", buff);
		sprintf(buff,"  MINIMUM_LATITUDE   = %f\n", pdsgd->get_min_latitude());
		curr_hdr_lng += strlen(buff);
		fprintf(f,"%s", buff);
		sprintf(buff,"  MAXIMUM_LONGITUDE   = %f\n", pdsgd->get_max_longitude());
		curr_hdr_lng += strlen(buff);
		fprintf(f,"%s", buff);
		sprintf(buff,"  MINIMUM_LONGITUDE   = %f\n", pdsgd->get_min_longitude());
		curr_hdr_lng += strlen(buff);
		fprintf(f,"%s", buff);
		sprintf(buff,"  X_AXIS_PROJECTION_OFFSET = %f\n", pdsgd->get_x_axis_offset());
		curr_hdr_lng += strlen(buff);
		fprintf(f,"%s", buff);
		sprintf(buff,"  Y_AXIS_PROJECTION_OFFSET = %f\n", pdsgd->get_y_axis_offset());
		curr_hdr_lng += strlen(buff);
		fprintf(f,"%s", buff);
		sprintf(buff,"  A_AXIS_RADIUS            = %f\n", pdsgd->get_a_axis_radius());
		curr_hdr_lng += strlen(buff);
		fprintf(f,"%s", buff);
		sprintf(buff,"  B_AXIS_RADIUS            = %f\n", pdsgd->get_b_axis_radius());
		curr_hdr_lng += strlen(buff);
		fprintf(f,"%s", buff);
		sprintf(buff,"  C_AXIS_RADIUS            = %f\n", pdsgd->get_c_axis_radius());
		curr_hdr_lng += strlen(buff);
		fprintf(f,"%s", buff);
		sprintf(buff,"  FIRST_STANDARD_PARALLEL  = %f\n", pdsgd->get_first_std_parallel());
		curr_hdr_lng += strlen(buff);
		fprintf(f,"%s", buff);
		sprintf(buff,"  SECOND_STANDARD_PARALLEL = %f\n", pdsgd->get_second_std_parallel());
		curr_hdr_lng += strlen(buff);
		fprintf(f,"%s", buff);
		if(pdsgd->get_long_dir_sign() > 0.0) {
			sprintf(buff,"  POSITIVE_LONGITUDE_DIRECTION = EAST\n");
		} else {
			sprintf(buff,"  POSITIVE_LONGITUDE_DIRECTION = WEST\n");
		}
		curr_hdr_lng += strlen(buff);
		fprintf(f,"%s", buff);
		sprintf(buff,"  CENTER_LATITUDE       = %f\n", pdsgd->get_center_lat());
		curr_hdr_lng += strlen(buff);
		fprintf(f,"%s", buff);
		sprintf(buff,"  CENTER_LONGITUDE      = %f\n", pdsgd->get_center_long());
		curr_hdr_lng += strlen(buff);
		fprintf(f,"%s", buff);
		sprintf(buff,"  REFERENCE_LATITUDE    = %f\n", pdsgd->get_reference_lat());
		curr_hdr_lng += strlen(buff);
		fprintf(f,"%s", buff);
		sprintf(buff,"  REFERENCE_LONGITUDE    = %f\n", pdsgd->get_reference_long());
		curr_hdr_lng += strlen(buff);
		fprintf(f,"%s", buff);
		sprintf(buff,"  X_AXIS_FIRST_PIXEL     = %d\n", pdsgd->get_min_x());
		curr_hdr_lng += strlen(buff);
		fprintf(f,"%s", buff);
		sprintf(buff,"  Y_AXIS_FIRST_PIXEL     = %d\n", pdsgd->get_min_y());
		curr_hdr_lng += strlen(buff);
		fprintf(f,"%s", buff);
		sprintf(buff,"  X_AXIS_LAST_PIXEL     = %d\n", pdsgd->get_max_x());
		curr_hdr_lng += strlen(buff);
		fprintf(f,"%s", buff);
		sprintf(buff,"  Y_AXIS_LAST_PIXEL     = %d\n", pdsgd->get_max_y());
		curr_hdr_lng += strlen(buff);
		fprintf(f,"%s", buff);
		sprintf(buff,"  MAP_PROJECTION_ROTATION = %f\n", pdsgd->get_map_rotn());
		curr_hdr_lng += strlen(buff);
		fprintf(f,"%s", buff);
		sprintf(buff,"END_OBJECT           = IMAGE_MAP_PROJECTION_CATALOG\n\n");
		curr_hdr_lng += strlen(buff);
		fprintf(f,"%s", buff);
	}
	//Output END and \n to terminate header
	sprintf(buff,"END\n");
	curr_hdr_lng += strlen(buff);
	fprintf(f,"%s", buff);
	sprintf(buff,"Trailing * are used to fill label to %d bytes\n", DEFAULT_HEADER_LENGTH);
	curr_hdr_lng += strlen(buff);
	fprintf(f,"%s", buff);
	// Output enough '*' to fill header to full size
	for( ; curr_hdr_lng < DEFAULT_HEADER_LENGTH; curr_hdr_lng++) fprintf(f, "*");

// Write the image:

	if(idat->preferred_data_type() == CHAR_DATA) {
// template<class T> int write_img_data(int xres, int yres, int bands, FILE *f, ImageData *idat, T dummy)
		write_img_data(xres, yres, bands, get_fp(), idat, (char)NULL);
	} else if(idat->preferred_data_type() == UCHAR_DATA) {
		write_img_data(xres, yres, bands, get_fp(), idat, (uchar)NULL);
	} else if(idat->preferred_data_type() == SHORT_DATA) {
		write_img_data(xres, yres, bands, get_fp(), idat, (short)NULL);
	} else if(idat->preferred_data_type() == USHORT_DATA) {
		write_img_data(xres, yres, bands, get_fp(), idat, (ushort)NULL);
	} else if(idat->preferred_data_type() == LONG_DATA) {
		write_img_data(xres, yres, bands, get_fp(), idat, (long)NULL);
	} else if(idat->preferred_data_type() == ULONG_DATA) {
		write_img_data(xres, yres, bands, get_fp(), idat, (ulong)NULL);
	} else if(idat->preferred_data_type() == FLOAT_DATA) {
		float junk_dummy=0.0;
		write_img_data(xres, yres, bands, get_fp(), idat, junk_dummy);
	} else if(idat->preferred_data_type() == DOUBLE_DATA) {
		double junk_dummy=0.0;
		write_img_data(xres, yres, bands, get_fp(), idat, junk_dummy);
	} else {
fprintf(stderr,"Whoops - Attempting to write a PDS file with an unexpected data type (BIT or COMPLEX?)\n");
	}

return error=FileErr_None;
}
