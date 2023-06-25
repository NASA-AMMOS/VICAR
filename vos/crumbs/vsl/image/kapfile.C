// kapfile.C
//
// Written by John Wright 03/12/97

#include "image/types/kapfile.h"
#include "image/geoimage.h"
#include "image/datatypes.h"

long  bsb_compress(long value, long number, long format, uchar *buff)
{
	long	temp;
	int	count,i,length=1;

	temp = number;
	while(temp >>= 1)length++;
	length += (int)format;
	count = (length-1)/7 + 1;
	for(i=0; i<count-1; i++) {
		buff[i] = (uchar)(((number >> 7*(count-i-1)) & 127 ) | 128);
	}
	buff[count-1] = (uchar)(number & 127);
	buff[0] |= (uchar)(value << (7-format));
	return(count);
}
	


int KAPFile::read_header(char *fname)
{
int     e;
char  buf[256];

reset_error();
if(fname) set_filename(fname);
free_comments(); free_flags();
if(open(FILE_READ)) return error;
if(strlen(filename)>2) {
#ifndef _NO_PIPES_
        if(!strcmp(filename+strlen(filename)-2,".Z")) {     // Compressed file
                close();
                sprintf(buf,"zcat %s",filename);
                e=read_head(popen(buf,"r"));
                if(fp) pclose(fp);
                return error=e;
                }
#endif // _NO_PIPES_
        }
e=read_head();
close();
return error=e;
}


int KAPFile::read_head(FILE *f)
{
ImageData	*idat;
char		buf[25600];
  char *temp_str = NULL;
  char buff[4096];		/* line buffer for reading file	*/
  int  xres, yres;		/* image width and height */
  float ver;
  int  index, r, g, b, format;			/* lut index */
  int  max_colors = 0;
  int  i;
  Image	*lut = NULL;		/* color map image */
  KAPGeoData	*geodat;
  int	temp_int, temp_int2, temp_int3;
  char	temp_string[1024];
  double	temp_double, temp_double2;


if(!img) return error=ImageFileErr_NoImage;		// check for an Image

if(f) attach(f);					// set file pointer

// Read header:
buf[0] = '\0';
fread(buf,256); 
buf[256] = '\0';

int p=0;  while(p<246 && buf[p] == ' ')p++;  // skip leading spaces
if(strncmp(&(buf[p]),"!Copyright",10)) return error=ImageFileErr_BadFile;
// read rest of header into buf until null byte encountered
	i = 256;
	for(buf[i] = getc(fp); !feof(fp) && buf[i]; buf[i] = getc(fp)) i++;
	set_comments(buf);

/*	read important stuff from bsb header	*/
	if(temp_str = strstr(buf,"RA=")) {
		sscanf(temp_str,"RA=%d,%d", &xres, &yres);
		file_xres = xres;
		file_yres = yres;
		file_bands = 1;
	}
	if(temp_str = strstr(buf,"VER/")) {
		temp_str = buff+4;
		sscanf(temp_str,"%f", &ver);
/* We don't care what version it is here
		if(ver <1.1) {
			fprintf(stderr,"Old Version Chart %s\n", buff);
			fprintf(stderr,"May have incorrect georeference data.\n");
		}
*/
	}
	if(temp_str = strstr(buf,"IFM/")) {
		sscanf(temp_str, "IFM/%d", &format);
		max_colors = 1 << format;
		lut = new Image();
		lut->create_data_type(3);	// uchar type lut
		lut->allocate(max_colors, 1, 3);
	}
	temp_str = buf;
	while((temp_str = strstr(temp_str,"RGB/")) && lut) {
		sscanf(temp_str, "RGB/%d,%d,%d,%d", &index, &r, &g, &b);
		if(index > max_colors) {
			fprintf(stderr,"Whoops - Color index %d greater than max %d - Skipping\n", index, max_colors);
		} else {
			lut->set_color(index,0,(uchar)r,(uchar)g,(uchar)b);
		}
		temp_str++;
	}

if(xres<1 || yres<1)
	return error=ImageFileErr_BadFile;

// check for georeferenced image
	if(img->image_class_type() == GEOIMAGE) {	// GeoImage parent
// fprintf(stderr,"Using georeferenced Image class\n");
		// create appropriate type of GeoData object
		geodat = new KAPGeoData();
		// get georef data from header and put in GeoData object
		// get name, number, and units from BSB record
		if(temp_str = strstr(buf,"BSB/")) {
			if(temp_str = strstr(temp_str,"NA=")) {
				sscanf(temp_str,"NA=%[^,\n\r]",temp_string);
				geodat->set_kapname(temp_string);
			}
		}
		if(temp_str = strstr(buf,"BSB/")) {
			if(temp_str = strstr(temp_str,"NU=")) {
				sscanf(temp_str,"NU=%d",&temp_int);
				geodat->set_kapnumber(temp_int);
			}
		}
		if(temp_str = strstr(buf,"BSB/")) {
			if(temp_str = strstr(temp_str,"DU=")) {
				sscanf(temp_str,"DU=%d",&temp_int);
				geodat->set_draw_units(temp_int);
			}
		}
		// get stuff from KNP record
		if(temp_str = strstr(buf,"KNP/")) {
			if(temp_str = strstr(temp_str,"SC=")) {
				sscanf(temp_str,"SC=%d",&temp_int);
				geodat->set_scale((long)temp_int);
			}
		}
		if(temp_str = strstr(buf,"KNP/")) {
			if(temp_str = strstr(temp_str,"GD=")) {
				sscanf(temp_str,"GD=%[^,\n\r]",temp_string);
				geodat->set_datum(geodat->datum_type_from_string(temp_string));
			}
		}
		if(temp_str = strstr(buf,"KNP/")) {
			if(temp_str = strstr(temp_str,"PR=")) {
				sscanf(temp_str,"PR=%[^,\n\r]",temp_string);
				geodat->set_projection(geodat->projection_type_from_string(temp_string));
			}
		}
		if(temp_str = strstr(buf,"KNP/")) {
			if(temp_str = strstr(temp_str,"PP=")) {
				sscanf(temp_str,"PP=%lf",&temp_double);
				geodat->set_projparm(temp_double);
			}
		}
		if(temp_str = strstr(buf,"KNP/")) {
			if(temp_str = strstr(temp_str,"PI=")) {
				sscanf(temp_str,"PI=%lf",&temp_double);
				geodat->set_projint(temp_double);
			}
		}
		if(temp_str = strstr(buf,"KNP/")) {
			if(temp_str = strstr(temp_str,"SP=")) {
				temp_string[0] = '\0';
				sscanf(temp_str,"SP=%[^,\n\r]",temp_string);
				while(strlen(temp_string) > 0 && !strstr(temp_string,"=")) {
					temp_str = strstr(temp_str,",") + 1;
					sscanf(temp_str,"%lf", &temp_double);
					geodat->add_state_plane(temp_string, temp_double);
					temp_str = strstr(temp_str,",") + 1;
					sscanf(temp_str,"%[^,\n\r]",temp_string);
				}
			}
		}
		if(temp_str = strstr(buf,"KNP/")) {
			if(temp_str = strstr(temp_str,"SK=")) {
				sscanf(temp_str,"SK=%lf",&temp_double);
				geodat->set_skew_angle(temp_double);
			}
		}
		if(temp_str = strstr(buf,"KNP/")) {
			if(temp_str = strstr(temp_str,"TA=")) {
				sscanf(temp_str,"TA=%lf",&temp_double);
				geodat->set_text_angle(temp_double);
			}
		}
		if(temp_str = strstr(buf,"KNP/")) {
			if(temp_str = strstr(temp_str,"UN=")) {
				sscanf(temp_str,"UN=%[^,\n\r]",temp_string);
				geodat->set_units(temp_string);
			}
		}
		if(temp_str = strstr(buf,"KNP/")) {
			if(temp_str = strstr(temp_str,"SD=")) {
				sscanf(temp_str,"SD=%[^,\n\r]",temp_string);
				geodat->set_sound_datum(temp_string);
			}
		}
		if(temp_str = strstr(buf,"KNP/")) {
			if(temp_str = strstr(temp_str,"DX=")) {
				sscanf(temp_str,"DX=%lf",&temp_double);
				geodat->set_x_meters(temp_double);
			}
		}
		if(temp_str = strstr(buf,"KNP/")) {
			if(temp_str = strstr(temp_str,"DY=")) {
				sscanf(temp_str,"DY=%lf",&temp_double);
				geodat->set_y_meters(temp_double);
			}
		}

		// get stuff from CED record
		if(temp_str = strstr(buf,"CED/")) {
			if(temp_str = strstr(temp_str,"SE=")) {
				sscanf(temp_str,"SE=%d",&temp_int);
				geodat->set_edition(temp_int);
			}
		}
		if(temp_str = strstr(buf,"CED/")) {
			if(temp_str = strstr(temp_str,"RE=")) {
				sscanf(temp_str,"RE=%d",&temp_int);
				geodat->set_raster_ed(temp_int);
			}
		}
		if(temp_str = strstr(buf,"CED/")) {
			if(temp_str = strstr(temp_str,"ED=")) {
				sscanf(temp_str,"ED=%[^,\n\r]",temp_string);
				geodat->set_ed_date(temp_string);
			}
		}

		// get stuff from VER record
		if(temp_str = strstr(buf,"VER/")) {
			sscanf(temp_str,"VER/%d.%d",&temp_int, &temp_int2);
			geodat->set_major(temp_int);
			geodat->set_minor(temp_int2);
		}
		
		// get stuff from OST record
		if(temp_str = strstr(buf,"OST/")) {
			sscanf(temp_str,"OST/%d",&temp_int);
			geodat->set_image_lines(temp_int);
		}
		
		// get stuff from IFM record
		// Unnecessary because already took care of it
		
		// get stuff from RGB record
		// Unnecessary because already took care of it
		
		// get stuff from REF records
		temp_str = strstr(buf,"REF/");
		while(temp_str) {
			sscanf(temp_str,"REF/%d,%d,%d,%lf,%lf",
				&temp_int, &temp_int2, &temp_int3, &temp_double, &temp_double2);
			while(temp_int > geodat->get_num_ref_points()) {
				geodat->add_reference_point(temp_int2, temp_int3, temp_double, temp_double2);
			}
			if(temp_int < geodat->get_num_ref_points()) {
				geodat->set_reference_point(temp_int, temp_int2, temp_int3, temp_double, temp_double2);
			}
			temp_str = strstr(temp_str+4,"REF/");
		}
		
		// get stuff from PLY records
		temp_str = strstr(buf,"PLY/");
		while(temp_str) {
			sscanf(temp_str,"PLY/%d,%lf,%lf",
				&temp_int, &temp_double, &temp_double2);
			geodat->add_poly_vert(temp_double, temp_double2);
			temp_str = strstr(temp_str+4,"PLY/");
		}
		
		// get stuff from DTM record
		if(temp_str = strstr(buf,"DTM/")) {
			sscanf(temp_str,"DTM/%lf,%lf",&temp_double, &temp_double2);
			geodat->set_lat_shift(temp_double);
			geodat->set_long_shift(temp_double2);
		}
		
		// get stuff from CPH record
		if(temp_str = strstr(buf,"CPH/")) {
			sscanf(temp_str,"CPH/%lf",&temp_double);
			geodat->set_shift_value(temp_double);
		}
		

		// get stuff from WPX record
		if(temp_str = strstr(buf,"WPX/")) {
			sscanf(temp_str,"WPX/%d%*[ ,\t\n\r]%n",&temp_int, &temp_int2);
			for(i=0; i<geodat->num_terms_from_code(temp_int); i++) {
				temp_str += temp_int2;
				sscanf(temp_str,"%lf%*[ ,\t\n\r]%n", &temp_double, &temp_int2);
				geodat->add_wpx_term(temp_double);
			}
		}
		
		// get stuff from PWX record
		if(temp_str = strstr(buf,"PWX/")) {
			sscanf(temp_str,"PWX/%d%*[ ,\t\n\r]%n",&temp_int, &temp_int2);
			for(i=0; i<geodat->num_terms_from_code(temp_int); i++) {
				temp_str += temp_int2;
				sscanf(temp_str,"%lf%*[ ,\t\n\r]%n", &temp_double, &temp_int2);
				geodat->add_pwx_term(temp_double);
			}
		}
		
		// get stuff from WPY record
		if(temp_str = strstr(buf,"WPY/")) {
			sscanf(temp_str,"WPY/%d%*[ ,\t\n\r]%n",&temp_int, &temp_int2);
			for(i=0; i<geodat->num_terms_from_code(temp_int); i++) {
				temp_str += temp_int2;
				sscanf(temp_str,"%lf%*[ ,\t\n\r]%n", &temp_double, &temp_int2);
				geodat->add_wpy_term(temp_double);
			}
		}
		
		// get stuff from PWY record
		if(temp_str = strstr(buf,"PWY/")) {
			sscanf(temp_str,"PWY/%d%*[ ,\t\n\r]%n",&temp_int, &temp_int2);
			for(i=0; i<geodat->num_terms_from_code(temp_int); i++) {
				temp_str += temp_int2;
				sscanf(temp_str,"%lf%*[ ,\t\n\r]%n", &temp_double, &temp_int2);
				geodat->add_pwy_term(temp_double);
			}
		}
		
		// get stuff from ERR records
		temp_str = strstr(buf,"ERR/");
		while(temp_str) {
			sscanf(temp_str,"ERR/%d,%lf,%lf,%n", &temp_int, &temp_double, &temp_double2, &temp_int2);
			geodat->set_pixel_error(temp_int-1, temp_double, temp_double2);
			temp_str += temp_int2;
			sscanf(temp_str,"%lf,%lf", &temp_double, &temp_double2);
			geodat->set_latlong_error(temp_int-1, temp_double, temp_double2);
			temp_str = strstr(temp_str,"ERR/");
		}

		// check for missing polynomials (no pwy, wpy, pwx, wpx, or err terms)
		// if missing generate them
		if(!geodat->get_num_wpx_terms()) {
			geodat->gen_wpx_terms();
		}
		if(!geodat->get_num_pwx_terms()) {
			geodat->gen_pwx_terms();
		}
		if(!geodat->get_num_wpy_terms()) {
			geodat->gen_wpy_terms();
		}
		if(!geodat->get_num_pwy_terms()) {
			geodat->gen_pwy_terms();
		}
		if(geodat->get_num_ref_points()) {
			geodat->gen_err_terms();
		}

		// put geodata object into parent geoimage
		((GeoImage *)img)->set_geo_data(geodat);
	}

// Get an image data pointer:
idat=*img;
if(!idat) {
	idat=new  RLEData();  // used to be ucharData();
	*img=idat;
	}

// Read the image:
//idat->set_rgb_bands(0,1,2);
idat->set_default_band();
idat->set_alpha_band(-1);
idat->free_map();
idat->set_map(lut);
idat->set_default_alpha();

return error;
}

int KAPFile::read_data(FILE *f)
{
ImageData	*idat;
char		buf[125600];
  char *temp_str = NULL;
  char buff[4096];		/* line buffer for reading file	*/
  int  xres, yres;		/* image width and height */
  float ver;
  unsigned char   color_mask, run_mask, cont_mask, more_run_mask;
  int  index, r, g, b, format;			/* lut index */
  int  max_colors = 0;
  int  num_pixels = 0;
  int  color, length, i, j, line;
  Image	*lut = NULL;		/* color map image */
  KAPGeoData	*geodat;
  int	temp_int, temp_int2, temp_int3;
  char	temp_string[1024];
  double	temp_double, temp_double2;


if(!img) return error=ImageFileErr_NoImage;		// check for an Image

if(f) attach(f);					// set file pointer

// Read header:
buf[0] = '\0';
fread(buf,256); 
buf[256] = '\0';
int p=0;  while(p<246 && buf[p] == ' ')p++;  // skip leading spaces
if(strncmp(&(buf[p]),"!Copyright",10)) return error=ImageFileErr_BadFile;
// read rest of header into buf until null byte encountered
	i = 256;
	for(buf[i] = getc(fp); !feof(fp) && buf[i]; buf[i] = getc(fp)) i++;
	set_comments(buf);

	num_pixels = -1;
/*	read important stuff from bsb header	*/
	if(temp_str = strstr(buf,"RA=")) {
		sscanf(temp_str,"RA=%d,%d", &xres, &yres);
		num_pixels = xres * yres;
	}
	if(temp_str = strstr(buf,"VER/")) {
		temp_str = buff+4;
		sscanf(temp_str,"%f", &ver);
/* We don't care what version it is here
		if(ver <1.1) {
			fprintf(stderr,"Old Version Chart %s\n", buff);
			fprintf(stderr,"May have incorrect georeference data.\n");
		}
*/
	}
	if(temp_str = strstr(buf,"IFM/")) {
		sscanf(temp_str, "IFM/%d", &format);
		max_colors = 1 << format;
		color_mask = (max_colors - 1) << (7 - format);
		run_mask = 127 ^ color_mask;
		cont_mask = 128;
		more_run_mask = 127;
		lut = new Image();
		lut->create_data_type(3);	// uchar type lut
		lut->allocate(max_colors, 1, 3);
	}
	temp_str = buf;
	while((temp_str = strstr(temp_str,"RGB/")) && lut) {
		sscanf(temp_str, "RGB/%d,%d,%d,%d", &index, &r, &g, &b);
		if(index > max_colors) {
			fprintf(stderr,"Whoops - Color index %d greater than max %d - Skipping\n", index, max_colors);
		} else {
			lut->set_color(index,0,(uchar)r,(uchar)g,(uchar)b);
		}
		temp_str++;
	}

if(xres<1 || yres<1)
	return error=ImageFileErr_BadFile;

// check for georeferenced image
	if(img->image_class_type() == GEOIMAGE) {	// GeoImage parent
// fprintf(stderr,"Using georeferenced Image class\n");
		// create appropriate type of GeoData object
		geodat = new KAPGeoData();
		// get georef data from header and put in GeoData object
		// get name, number, and units from BSB record
		if(temp_str = strstr(buf,"BSB/")) {
			if(temp_str = strstr(temp_str,"NA=")) {
				sscanf(temp_str,"NA=%[^,\n\r]",temp_string);
				geodat->set_kapname(temp_string);
			}
		}
		if(temp_str = strstr(buf,"BSB/")) {
			if(temp_str = strstr(temp_str,"NU=")) {
				sscanf(temp_str,"NU=%d",&temp_int);
				geodat->set_kapnumber(temp_int);
			}
		}
		if(temp_str = strstr(buf,"BSB/")) {
			if(temp_str = strstr(temp_str,"DU=")) {
				sscanf(temp_str,"DU=%d",&temp_int);
				geodat->set_draw_units(temp_int);
			}
		}
		// get stuff from KNP record
		if(temp_str = strstr(buf,"KNP/")) {
			if(temp_str = strstr(temp_str,"SC=")) {
				sscanf(temp_str,"SC=%d",&temp_int);
				geodat->set_scale((long)temp_int);
			}
		}
		if(temp_str = strstr(buf,"KNP/")) {
			if(temp_str = strstr(temp_str,"GD=")) {
				sscanf(temp_str,"GD=%[^,\n\r]",temp_string);
				geodat->set_datum(geodat->datum_type_from_string(temp_string));
			}
		}
		if(temp_str = strstr(buf,"KNP/")) {
			if(temp_str = strstr(temp_str,"PR=")) {
				sscanf(temp_str,"PR=%[^,\n\r]",temp_string);
				geodat->set_projection(geodat->projection_type_from_string(temp_string));
			}
		}
		if(temp_str = strstr(buf,"KNP/")) {
			if(temp_str = strstr(temp_str,"PP=")) {
				sscanf(temp_str,"PP=%lf",&temp_double);
				geodat->set_projparm(temp_double);
			}
		}
		if(temp_str = strstr(buf,"KNP/")) {
			if(temp_str = strstr(temp_str,"PI=")) {
				sscanf(temp_str,"PI=%lf",&temp_double);
				geodat->set_projint(temp_double);
			}
		}
		if(temp_str = strstr(buf,"KNP/")) {
			if(temp_str = strstr(temp_str,"SP=")) {
				temp_string[0] = '\0';
				sscanf(temp_str,"SP=%[^,\n\r]",temp_string);
				while(strlen(temp_string) > 0 && !strstr(temp_string,"=")) {
					temp_str = strstr(temp_str,",") + 1;
					sscanf(temp_str,"%lf", &temp_double);
					geodat->add_state_plane(temp_string, temp_double);
					temp_str = strstr(temp_str,",") + 1;
					sscanf(temp_str,"%[^,\n\r]",temp_string);
				}
			}
		}
		if(temp_str = strstr(buf,"KNP/")) {
			if(temp_str = strstr(temp_str,"SK=")) {
				sscanf(temp_str,"SK=%lf",&temp_double);
				geodat->set_skew_angle(temp_double);
			}
		}
		if(temp_str = strstr(buf,"KNP/")) {
			if(temp_str = strstr(temp_str,"TA=")) {
				sscanf(temp_str,"TA=%lf",&temp_double);
				geodat->set_text_angle(temp_double);
			}
		}
		if(temp_str = strstr(buf,"KNP/")) {
			if(temp_str = strstr(temp_str,"UN=")) {
				sscanf(temp_str,"UN=%[^,\n\r]",temp_string);
				geodat->set_units(temp_string);
			}
		}
		if(temp_str = strstr(buf,"KNP/")) {
			if(temp_str = strstr(temp_str,"SD=")) {
				sscanf(temp_str,"SD=%[^,\n\r]",temp_string);
				geodat->set_sound_datum(temp_string);
			}
		}
		if(temp_str = strstr(buf,"KNP/")) {
			if(temp_str = strstr(temp_str,"DX=")) {
				sscanf(temp_str,"DX=%lf",&temp_double);
				geodat->set_x_meters(temp_double);
			}
		}
		if(temp_str = strstr(buf,"KNP/")) {
			if(temp_str = strstr(temp_str,"DY=")) {
				sscanf(temp_str,"DY=%lf",&temp_double);
				geodat->set_y_meters(temp_double);
			}
		}

		// get stuff from CED record
		if(temp_str = strstr(buf,"CED/")) {
			if(temp_str = strstr(temp_str,"SE=")) {
				sscanf(temp_str,"SE=%d",&temp_int);
				geodat->set_edition(temp_int);
			}
		}
		if(temp_str = strstr(buf,"CED/")) {
			if(temp_str = strstr(temp_str,"RE=")) {
				sscanf(temp_str,"RE=%d",&temp_int);
				geodat->set_raster_ed(temp_int);
			}
		}
		if(temp_str = strstr(buf,"CED/")) {
			if(temp_str = strstr(temp_str,"ED=")) {
				sscanf(temp_str,"ED=%[^,\n\r]",temp_string);
				geodat->set_ed_date(temp_string);
			}
		}

		// get stuff from VER record
		if(temp_str = strstr(buf,"VER/")) {
			sscanf(temp_str,"VER/%d.%d",&temp_int, &temp_int2);
			geodat->set_major(temp_int);
			geodat->set_minor(temp_int2);
		}
		
		// get stuff from OST record
		if(temp_str = strstr(buf,"OST/")) {
			sscanf(temp_str,"OST/%d",&temp_int);
			geodat->set_image_lines(temp_int);
		}
		
		// get stuff from IFM record
		// Unnecessary because already took care of it
		
		// get stuff from RGB record
		// Unnecessary because already took care of it
		
		// get stuff from REF records
		temp_str = strstr(buf,"REF/");
		while(temp_str) {
			sscanf(temp_str,"REF/%d,%d,%d,%lf,%lf",
				&temp_int, &temp_int2, &temp_int3, &temp_double, &temp_double2);
			while(temp_int > geodat->get_num_ref_points()) {
				geodat->add_reference_point(temp_int2, temp_int3, temp_double, temp_double2);
			}
			if(temp_int < geodat->get_num_ref_points()) {
				geodat->set_reference_point(temp_int, temp_int2, temp_int3, temp_double, temp_double2);
			}
			temp_str = strstr(temp_str+4,"REF/");
		}
		
		// get stuff from PLY records
		temp_str = strstr(buf,"PLY/");
		while(temp_str) {
			sscanf(temp_str,"PLY/%d,%lf,%lf",
				&temp_int, &temp_double, &temp_double2);
			geodat->add_poly_vert(temp_double, temp_double2);
			temp_str = strstr(temp_str+4,"PLY/");
		}
		
		// get stuff from DTM record
		if(temp_str = strstr(buf,"DTM/")) {
			sscanf(temp_str,"DTM/%lf,%lf",&temp_double, &temp_double2);
			geodat->set_lat_shift(temp_double);
			geodat->set_long_shift(temp_double2);
		}
		
		// get stuff from CPH record
		if(temp_str = strstr(buf,"CPH/")) {
			sscanf(temp_str,"CPH/%lf",&temp_double);
			geodat->set_shift_value(temp_double);
		}
		

		// get stuff from WPX record
		if(temp_str = strstr(buf,"WPX/")) {
			sscanf(temp_str,"WPX/%d%*[ ,\t\n\r]%n",&temp_int, &temp_int2);
			for(i=0; i<geodat->num_terms_from_code(temp_int); i++) {
				temp_str += temp_int2;
				sscanf(temp_str,"%lf%*[ ,\t\n\r]%n", &temp_double, &temp_int2);
				geodat->add_wpx_term(temp_double);
			}
		}
		
		// get stuff from PWX record
		if(temp_str = strstr(buf,"PWX/")) {
			sscanf(temp_str,"PWX/%d%*[ ,\t\n\r]%n",&temp_int, &temp_int2);
			for(i=0; i<geodat->num_terms_from_code(temp_int); i++) {
				temp_str += temp_int2;
				sscanf(temp_str,"%lf%*[ ,\t\n\r]%n", &temp_double, &temp_int2);
				geodat->add_pwx_term(temp_double);
			}
		}
		
		// get stuff from WPY record
		if(temp_str = strstr(buf,"WPY/")) {
			sscanf(temp_str,"WPY/%d%*[ ,\t\n\r]%n",&temp_int, &temp_int2);
			for(i=0; i<geodat->num_terms_from_code(temp_int); i++) {
				temp_str += temp_int2;
				sscanf(temp_str,"%lf%*[ ,\t\n\r]%n", &temp_double, &temp_int2);
				geodat->add_wpy_term(temp_double);
			}
		}
		
		// get stuff from PWY record
		if(temp_str = strstr(buf,"PWY/")) {
			sscanf(temp_str,"PWY/%d%*[ ,\t\n\r]%n",&temp_int, &temp_int2);
			for(i=0; i<geodat->num_terms_from_code(temp_int); i++) {
				temp_str += temp_int2;
				sscanf(temp_str,"%lf%*[ ,\t\n\r]%n", &temp_double, &temp_int2);
				geodat->add_pwy_term(temp_double);
			}
		}
		
		// get stuff from ERR records
		temp_str = strstr(buf,"ERR/");
		while(temp_str) {
			sscanf(temp_str,"ERR/%d,%lf,%lf,%n", &temp_int, &temp_double, &temp_double2, &temp_int2);
			geodat->set_pixel_error(temp_int-1, temp_double, temp_double2);
			temp_str += temp_int2;
			sscanf(temp_str,"%lf,%lf", &temp_double, &temp_double2);
			geodat->set_latlong_error(temp_int-1, temp_double, temp_double2);
			temp_str = strstr(temp_str,"ERR/");
		}

		// check for missing polynomials (no pwy, wpy, pwx, wpx, or err terms)
		// if missing generate them
		if(!geodat->get_num_wpx_terms()) {
			geodat->gen_wpx_terms();
		}
		if(!geodat->get_num_pwx_terms()) {
			geodat->gen_pwx_terms();
		}
		if(!geodat->get_num_wpy_terms()) {
			geodat->gen_wpy_terms();
		}
		if(!geodat->get_num_pwy_terms()) {
			geodat->gen_pwy_terms();
		}
		if(geodat->get_num_ref_points()) {
			geodat->gen_err_terms();
		}

		// put geodata object into parent geoimage
		((GeoImage *)img)->set_geo_data(geodat);
	}


// begin reading binary image data
	/* get format */
	r = getc(fp);
	if(r != format) {
		fprintf(stderr," Whoops - format %d in header does not match format %d in image\n", format, r);
		return error=ImageFileErr_BadFile;
	}

// Get an image data pointer:
idat=*img;
if(!idat) {
	idat=new  RLEData();  // used to be ucharData();
	*img=idat;
	}
if(!(idat->access() & DATA_WRITE))
	return error=ImageFileErr_BadImageType;

// Allocate memory:
if(!idat->allocate(xres,yres,1))
	return error=ImageFileErr_AllocFailed;

// Read the image:
//idat->set_rgb_bands(0,1,2);
idat->set_default_band();
idat->set_alpha_band(-1);
idat->free_map();
idat->set_map(lut);
idat->set_default_alpha();
	/* translate image bytes */
	for(j=0; j<yres; j++) {
		/* get line number */
		b = getc(fp);
		line = b & more_run_mask;
		 while(b & cont_mask) {
			b = getc(fp);
			line = line * 128 + (b & more_run_mask);
		}
// fprintf(stderr,"Line #%d\n", line);
		/* generate width pixels for line */
		num_pixels = 0;
		while(num_pixels <xres) {
			b = getc(fp);
			color = (b & color_mask) >> (7 - format);
			length = b & run_mask;
			while(b & cont_mask) {
				b = getc(fp);
				length = length * 128 + (b & more_run_mask);
			}
			length ++;
			// Use convenience function to directly fill with RLE strings
			((RLEData *)idat)->line_append((long)color, (long)length, (long)j);
//			for(i=num_pixels; i<num_pixels+length; i++) idat->set_long(color, i, j);
			num_pixels += length;
		}
		/* get NULL at end of line */
		b = getc(fp);
 	}

return error;
}


int KAPFile::write_data(FILE *f)
{
int		i,xres,yres,x,y, format;
long		value, count, *index;
uchar		r,g,b, buff[6];
ImageData	*idat;
Image		*map;
KAPGeoData	*geodat;
int		temp_int, temp_int2;
double		temp_double, temp_double2;

if(!img) return error=ImageFileErr_NoImage;		// check for an Image

if(f) attach(f);					// set file pointer

// Get an image data pointer:
idat=*img;
if(!idat)
	return error=ImageFileErr_BadImageType;
if(!(idat->access() & DATA_WRITE))
	return error=ImageFileErr_BadImageType;
map = idat->get_map();
if(!map) return error=ImageFileErr_BadImageType;

// Get resolution:
idat->get_res(&xres,&yres);
if(xres<1 || yres<1)
	return error=ImageFileErr_BadImageData;

// Write the header:
fprintf(fp,"!Copyright 199?, BSB Electronic Charts.  All Rights Reserved.\n");
fprintf(fp,"!File Created using Image Class from Jet Propulsion Lab.\n");
// check for georeferenced image
	if(img->image_class_type() == GEOIMAGE) {
		geodat = (KAPGeoData *)(((GeoImage *)img)->get_geo_data());
	} else {
		geodat = NULL;
	}
	if(geodat) {
fprintf(stderr,"Writing georeferenced Image class\n");
		// output VER record
		fprintf(fp,"VER/%d.%d\n", geodat->get_major(), geodat->get_minor());
		// output BSB record
		fprintf(fp,"BSB/");
		fprintf(fp,"NA=");
		if(geodat->get_kapname())fprintf(fp,"%s",geodat->get_kapname());
		fprintf(fp,",NU=%d",geodat->get_kapnumber());
		fprintf(fp,",RA=%d,%d",xres,yres);
		fprintf(fp,",DU=%d",geodat->get_draw_units());
		fprintf(fp,"\n");
		// output KNP record
		fprintf(fp,"KNP/");
		fprintf(fp,"SC=%d", geodat->get_scale());
		fprintf(fp,",GD=%s", geodat->datum_type_to_string(geodat->get_datum()));
		fprintf(fp,",PR=%s", geodat->projection_type_to_string(geodat->get_projection()));
		fprintf(fp,",PP=%f", geodat->get_projparm());
		fprintf(fp,",PI=%f", geodat->get_projint());
		fprintf(fp,",SP=");
		for(i=0; i<geodat->get_num_state_planes(); i++) {
			fprintf(fp,"%s,%f",geodat->get_state_plane(i),geodat->get_state_plane_interval(i));
			if((i+1) < geodat->get_num_state_planes()) fprintf(fp,",");
		}
		fprintf(fp,",SK=%f", geodat->get_skew_angle());
		fprintf(fp,",TA=%f", geodat->get_text_angle());
		fprintf(fp,",UN=%s", geodat->get_units());
		fprintf(fp,",SD=%s", geodat->get_sound_datum());
		fprintf(fp,",DX=%f", geodat->get_x_meters());
		fprintf(fp,",DY=%f", geodat->get_y_meters());
		fprintf(fp,"\n");
		// output CED record
		fprintf(fp,"CED/");
		fprintf(fp,"SE=%d", geodat->get_edition());
		fprintf(fp,",RE=%d", geodat->get_raster_ed());
		fprintf(fp,",ED=%s", geodat->get_ed_date());
		fprintf(fp,"\n");
		// output OST record
		fprintf(fp,"OST/");
		fprintf(fp,"%d", geodat->get_image_lines());
		fprintf(fp,"\n");
		// output IFM record
		map->get_res(&x, &y);
		y = x;
		format = 0;
		while(x >>= 1) format++;
		fprintf(fp,"IFM/%d\n", format);
		// output RGB records
		for(i=1; i<y; i++) {
			idat->get_map()->get_color(i, 0, &r, &g, &b);
			fprintf(fp,"RGB/%d,%d,%d,%d\n",i,r,g,b);
		}
		// output REF records
		for(i=0; i<geodat->get_num_ref_points(); i++) {
			geodat->get_reference_point(i, &temp_int, &temp_int2, &temp_double, &temp_double2);
			fprintf(fp,"REF/%d,%d,%d,%.10f,%.10f\n",i+1, temp_int, temp_int2, temp_double, temp_double2);
		}
		// output PLY records
		for(i=0; i<geodat->get_num_poly_vertices(); i++) {
			geodat->get_poly_vert(i, &temp_double, &temp_double2);
			fprintf(fp,"PLY/%d,%.10f,%.10f\n",i+1, temp_double, temp_double2);
		}
		// output DTM record
		fprintf(fp,"DTM/%.10f,%.10f\n", geodat->get_lat_shift(), geodat->get_long_shift());
		// output CPH record
		fprintf(fp,"CPH/%.10f\n", geodat->get_shift_value());
		// output Polynomial records
		fprintf(fp,"WPX/%d",geodat->num_terms_to_code(geodat->get_num_wpx_terms()));
		for(i=0; i<geodat->get_num_wpx_terms(); i++) {
			if(fabs(geodat->get_wpx_term(i)) < 0.001) {
				fprintf(fp,",%.12e", geodat->get_wpx_term(i));
			} else {
				fprintf(fp,",%.12f", geodat->get_wpx_term(i));
			}
		}
		fprintf(fp,"\n");
		fprintf(fp,"PWX/%d",geodat->num_terms_to_code(geodat->get_num_pwx_terms()));
		for(i=0; i<geodat->get_num_pwx_terms(); i++) {
			if(fabs(geodat->get_pwx_term(i)) < 0.001) {
				fprintf(fp,",%.12e", geodat->get_pwx_term(i));
			} else {
				fprintf(fp,",%.12f", geodat->get_pwx_term(i));
			}
		}
		fprintf(fp,"\n");
		fprintf(fp,"WPY/%d",geodat->num_terms_to_code(geodat->get_num_wpy_terms()));
		for(i=0; i<geodat->get_num_wpy_terms(); i++) {
			if(fabs(geodat->get_wpy_term(i)) < 0.001) {
				fprintf(fp,",%.12e", geodat->get_wpy_term(i));
			} else {
				fprintf(fp,",%.12f", geodat->get_wpy_term(i));
			}
		}
		fprintf(fp,"\n");
		fprintf(fp,"PWY/%d",geodat->num_terms_to_code(geodat->get_num_pwy_terms()));
		for(i=0; i<geodat->get_num_pwy_terms(); i++) {
			if(fabs(geodat->get_pwy_term(i)) < 0.001) {
				fprintf(fp,",%.12e", geodat->get_pwy_term(i));
			} else {
				fprintf(fp,",%.12f", geodat->get_pwy_term(i));
			}
		}
		fprintf(fp,"\n");
		// output ERR records
		for(i=0; i<geodat->get_num_ref_points(); i++) {
			geodat->get_pixel_error(i, &temp_double, &temp_double2);
			fprintf(fp,"ERR/%d,%.10f,%.10f",i+1, temp_double, temp_double2);
			geodat->get_latlong_error(i, &temp_double, &temp_double2);
			fprintf(fp,",%.10e,%.10e\n", temp_double, temp_double2);
		}
	} else {
fprintf(stderr,"Writing regular Image class\n");
		if(comments) fprintf(fp,"! %s\n",comments);
		fprintf(fp,"BSB/RA=%d,%d\n",xres,yres);

		// Write the format
		map->get_res(&x, &y);
		y = x;
		format = 0;
		while(x >>= 1) format++;
		fprintf(fp,"IFM/%d\n", format);

		// Output color map
		for(i=1; i<y; i++) {
			idat->get_map()->get_color(i, 0, &r, &g, &b);
			fprintf(fp,"RGB/%d,%d,%d,%d\n",i,r,g,b);
		}
	}
	//Output EOF and null to terminate header
	putc(26,fp);	// EOF = 1AH
	putc(0,fp);	// NULL = 0

// Write the image:
// output format byte
putc(format,fp);
for(y=0;y<yres;y++) {
	//output line number
	fwrite(buff, bsb_compress(0,y+1,0,buff));
	if(idat->type() == 50) {	// RLE data
		index = ((RLEData *)idat)->get_line(y);
		x = 0;
		while(x<xres) {
			count = *index++;
			value = *index++;
			fwrite(buff,bsb_compress(value,count-1,format, buff));
			x += count;
		}
	} else {			// non RLE data
		for(x=0;x<xres;) {
			count = 1;
			// count successive identical values
			value = idat->get_long(x++,y);
			while(x<xres && idat->get_long(x,y) == value) {
				count++;
				x++;
			}
			fwrite(buff,bsb_compress(value,count-1,format, buff));
		}
	}
	//Output null to terminate line
	putc(0,fp);
}

return error;
}

