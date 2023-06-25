// pds_geodata.h
//
// Written by John Wright 02/15/98

#include "image/types/pds_geodata.h"

#ifndef TRUE
#define TRUE	1
#define FALSE	0
#endif

#ifndef DEG_TO_RAD
#define DEG_TO_RAD	(3.14159265358979/180.0)
#endif

// This data is used to convert to/from character strings in the
// pds file header to our internal datum and projection indexes.
// These MUST correspond to the indexes found in ../geodatatypes.h


	static	char	*pds_datum_strings[] = {
			(char *)"UNKNOWN",
			(char *)"NAD02",
			(char *)"NAD27",
			(char *)"NAD83",
			(char *)"WGS84",
			(char *)"Astronomic Datum",
			(char *)"Astronomic Datum (general)",
			(char *)"Astronomic Datums (general)",
			(char *)"Astronomic Datum 1928 (field)",
			(char *)"Astronomic Datum 1930 (field)",
			(char *)"Astronomic Datum 1931 (field)",
			(char *)"Astro Datum 1939",
			(char *)"Local Astronomic Datum",
			(char *)"Local Datum (undetermined)",
			(char *)"Local Datum",
			(char *)"Gardners Pinnacles 1929",
			(char *)"Neckers 1928",
			(char *)"Nihoa 1928",
			(char *)"Old Hawaiian",
			(char *)"Togcha Datum",
			(char *)"Johnston Isl 1961 Astro Datum",
			(char *)"Junk Place Holder Insert above Here" };
			

	static	char	*pds_projection_strings[] = {
			(char *)"UNKNOWN",
			(char *)"MERCATOR",
			(char *)"UNIVERSAL TRANSVERSE MERCATOR",
			(char *)"POLYCONIC",
			(char *)"LAMBERT CONFORMAL CONIC",
			(char *)"GEOGRAPHIC LAT LON",
			(char *)"SINUSOIDAL",
			(char *)"Junk Place Holder Insert above Here" };


void	PDSGeoData::init(void)
{
	datum_strings = pds_datum_strings;
	projection_strings = pds_projection_strings;

	datum = UNKNOWN;
	projection = UNKNOWN;
        catalog = NULL;
	resolution = 1.0;
	scale = 1.0;
	min_latitude = -90.0;
	max_latitude = 90.0;
	min_longitude = -180.0;
	max_longitude = 180.0;
	x_axis_offset = 0.0;
	y_axis_offset = 0.0;
	a_axis_radius = 0.0;
	b_axis_radius = 0.0;
	c_axis_radius = 0.0;
	first_std_parallel = 0.0;
	second_std_parallel = 0.0;
	long_dir_sign = 1.0;
	center_lat = 0.0;
	center_long = 0.0;
	reference_lat = 0.0;
	reference_long = 0.0;
	min_x = max_x = min_y = max_y = 0;
	map_rotn = 0.0;
	
}


/* some notes on georeferencing - see end of file for more details

MDIM's are presented in a Sinusoidal Equal-area map projection. In this
projection, parallels of latitude are straight lines, with constant
distances between equal latitude intervals. Lines of constant longitude
on either side of the projection meridian are curved since longitude
intervals decrease with the cosine of latitude to account for their
convergence toward the poles. This projection offers a number of
advantages for storing and managing global digital data; in particular,
it is computationally simple, and data are stored in a compact form. 
 
The Sinusoidal Equal-area projection is characterized by a projection 
longitude, which is the center meridian of the projection, and a scale,
which is given in units of pixels/degree. The center latitude for all
MDIM's is the equator. Each MDIM file contains its own central meridian.

The transformation from latitude and longitude to line and sample for
planets with a direction of positive longitude of WEST is given by the
following equations:

  line = INT(X_AXIS_PROJECTION_OFFSET - lat*MAP_RESOLUTION + 1.0)

  sample = INT(Y_AXIS_PROJECTION_OFFSET - (lon - CENTER_LONGITUDE)*
       MAP_RESOLUTION*cos(lat) + 1.0)
                                            
                                       
Note that integral values of line and sample correspond to center of
a pixel. Lat and lon are the latitude and longitude of a given spot
on the surface. 
*/ 


	// coordinate space converters (instantiation of virtuals)
double PDSGeoData::lltox(double lat, double lng)
{
/*
  sample = INT(Y_AXIS_PROJECTION_OFFSET - (lon - CENTER_LONGITUDE)*
       MAP_RESOLUTION*cos(lat) + 1.0)

Note that this formula has been modified slightly to always work with
positive input longitude=East as standard for our georeferencing.  The
long_dir_sign variable performs the correct mapping from whichever
positive longitude direction is in the PDS file.
*/
	double	temp;

	if(projection == SINUSOIDAL) {
		lng *= long_dir_sign;
        	while(lng < min_longitude) lng += 360.0;
        	while(lng > max_longitude) lng -= 360.0;
		temp = y_axis_offset + (lng - center_long)*resolution*cos(lat*DEG_TO_RAD)*long_dir_sign + 1.0;
	} else {
		temp = 0.0;
	}

	return(temp);
}
double PDSGeoData::lltoy(double lat, double )
{
/*
  line = INT(X_AXIS_PROJECTION_OFFSET - lat*MAP_RESOLUTION + 1.0)
*/
	double	temp;

	if(projection == SINUSOIDAL) {
		temp = x_axis_offset - lat * resolution + 1.0;
	} else {
		temp = 0.0;
	}
	return(temp);
}

double PDSGeoData::xytolat(double , double y)
{
	double	temp = 0.0;

	if(projection == SINUSOIDAL) {
		temp = (x_axis_offset + 1.0 - y) / resolution;
	} else {
		temp = 0.0;
	}
	return(temp);
}
double PDSGeoData::xytolong(double x, double y)
{
	double	temp = 0.0;

	if(projection == SINUSOIDAL) {
		temp = center_long - long_dir_sign * (y_axis_offset - x + 1.0)/(resolution*cos(xytolat(x,y)*DEG_TO_RAD));
		// add a shift value type thing here if max long > 180
        	while(temp < -180.00000000) temp += 360.0;
        	while(temp >  180.00000000) temp -= 360.0;
		temp *= long_dir_sign;
	} else {
		temp = 0.0;
	}
	return(temp);
}

char    *append_token(char *in_string, char *add_string)
{
	if(in_string) {
        	in_string = (char *)realloc(in_string, strlen(in_string) + strlen(add_string) + 2);
                strcat(in_string," ");
                strcat(in_string,add_string);
	} else {
        	in_string = strdup(add_string);
	}

        return(in_string);
}

char	*PDSGeoData::to_string(void)
{
	char	*out_string = NULL;
	char	buff[1024];

		out_string = append_token(out_string, (char *)"PDSGeoData");
		sprintf(buff,"OBJECT               = IMAGE_MAP_PROJECTION_CATALOG\n");
		out_string = append_token(out_string, buff);
		if(this->get_catalog()) {
			sprintf(buff,"  ^DATA_SET_MAP_PROJECTION_CATALOG = %s\n", this->get_catalog());
			out_string = append_token(out_string, buff);
		}
		sprintf(buff,"  MAP_PROJECTION_TYPE = %s\n", this->projection_type_to_string(this->get_projection()));
		out_string = append_token(out_string, buff);
		sprintf(buff,"  MAP_RESOLUTION     = %g  <PIXEL/DEG>\n", this->get_resolution());
		out_string = append_token(out_string, buff);
		sprintf(buff,"  MAP_SCALE          = %g  <KM/PIXEL>\n", this->get_scale());
		out_string = append_token(out_string, buff);
		sprintf(buff,"  MAXIMUM_LATITUDE   = %g\n", this->get_max_latitude());
		out_string = append_token(out_string, buff);
		sprintf(buff,"  MINIMUM_LATITUDE   = %g\n", this->get_min_latitude());
		out_string = append_token(out_string, buff);
		sprintf(buff,"  MAXIMUM_LONGITUDE   = %g\n", this->get_max_longitude());
		out_string = append_token(out_string, buff);
		sprintf(buff,"  MINIMUM_LONGITUDE   = %g\n", this->get_min_longitude());
		out_string = append_token(out_string, buff);
		sprintf(buff,"  X_AXIS_PROJECTION_OFFSET = %g\n", this->get_x_axis_offset());
		out_string = append_token(out_string, buff);
		sprintf(buff,"  Y_AXIS_PROJECTION_OFFSET = %g\n", this->get_y_axis_offset());
		out_string = append_token(out_string, buff);
		sprintf(buff,"  A_AXIS_RADIUS            = %g\n", this->get_a_axis_radius());
		out_string = append_token(out_string, buff);
		sprintf(buff,"  B_AXIS_RADIUS            = %g\n", this->get_b_axis_radius());
		out_string = append_token(out_string, buff);
		sprintf(buff,"  C_AXIS_RADIUS            = %g\n", this->get_c_axis_radius());
		out_string = append_token(out_string, buff);
		sprintf(buff,"  FIRST_STANDARD_PARALLEL  = %g\n", this->get_first_std_parallel());
		out_string = append_token(out_string, buff);
		sprintf(buff,"  SECOND_STANDARD_PARALLEL = %g\n", this->get_second_std_parallel());
		out_string = append_token(out_string, buff);
		if(this->get_long_dir_sign() > 0.0) {
			sprintf(buff,"  POSITIVE_LONGITUDE_DIRECTION = EAST\n");
		} else {
			sprintf(buff,"  POSITIVE_LONGITUDE_DIRECTION = WEST\n");
		}
		out_string = append_token(out_string, buff);
		sprintf(buff,"  CENTER_LATITUDE       = %g\n", this->get_center_lat());
		out_string = append_token(out_string, buff);
		sprintf(buff,"  CENTER_LONGITUDE      = %g\n", this->get_center_long());
		out_string = append_token(out_string, buff);
		sprintf(buff,"  REFERENCE_LATITUDE    = %g\n", this->get_reference_lat());
		out_string = append_token(out_string, buff);
		sprintf(buff,"  REFERENCE_LONGITUDE    = %g\n", this->get_reference_long());
		out_string = append_token(out_string, buff);
		sprintf(buff,"  X_AXIS_FIRST_PIXEL     = %d\n", this->get_min_x());
		out_string = append_token(out_string, buff);
		sprintf(buff,"  Y_AXIS_FIRST_PIXEL     = %d\n", this->get_min_y());
		out_string = append_token(out_string, buff);
		sprintf(buff,"  X_AXIS_LAST_PIXEL     = %d\n", this->get_max_x());
		out_string = append_token(out_string, buff);
		sprintf(buff,"  Y_AXIS_LAST_PIXEL     = %d\n", this->get_max_y());
		out_string = append_token(out_string, buff);
		sprintf(buff,"  MAP_PROJECTION_ROTATION = %g\n", this->get_map_rotn());
		out_string = append_token(out_string, buff);
		sprintf(buff,"  ELEVATION_SCALE       = %g\n",this->get_elev_scale());
		out_string = append_token(out_string, buff);
		sprintf(buff,"  ELEVATION_BASE        = %g\n", this->get_elev_offset());
		out_string = append_token(out_string, buff);
		sprintf(buff,"END_OBJECT           = IMAGE_MAP_PROJECTION_CATALOG\n\n");
		out_string = append_token(out_string, buff);


	return(out_string);
}

char *local_get_key_value(char *buf, char *key)
{
	char	out_value[1024];
	char	*substring;

	substring = strstr(buf, key);
	if(substring) {
		sscanf(substring,"%*s = %s",out_value);
		substring = strdup(out_value);
	}
	return(substring);
}

int	PDSGeoData::from_string(char *buf)
{
	char	*local_key_value;
	char	temp[256];

	sscanf(buf,"%255s", temp);
	if(!strcmp(temp,"PDSGeoData")) {
		local_key_value = local_get_key_value(buf, (char *)"^DATA_SET_MAP_PROJECTION_CATALOG");
		if(local_key_value) {
			this->set_catalog(local_key_value);
			free(local_key_value);
		}
		local_key_value = local_get_key_value(buf, (char *)"MAP_PROJECTION_TYPE");
		if(local_key_value) {
			this->set_projection(this->projection_type_from_string(local_key_value));
			free(local_key_value);
		}
		local_key_value = local_get_key_value(buf, (char *)"MAP_RESOLUTION");
		if(local_key_value) {
			this->set_resolution(atof(local_key_value)); // units?? = pixels/deg
			free(local_key_value);
		}
		local_key_value = local_get_key_value(buf, (char *)"MAP_SCALE");
		if(local_key_value) {
			this->set_scale(atof(local_key_value)); // units?? = km/pixel
			free(local_key_value);
		}
		local_key_value = local_get_key_value(buf, (char *)"MAXIMUM_LATITUDE");
		if(local_key_value) {
			this->set_max_latitude(atof(local_key_value)); // units?? = degrees
			free(local_key_value);
		}
		local_key_value = local_get_key_value(buf, (char *)"MINIMUM_LATITUDE");
		if(local_key_value) {
			this->set_min_latitude(atof(local_key_value)); // units?? = degrees
			free(local_key_value);
		}
		local_key_value = local_get_key_value(buf, (char *)"MAXIMUM_LONGITUDE");
		if(local_key_value) {
			this->set_max_longitude(atof(local_key_value)); // units?? = degrees
			free(local_key_value);
		}
		local_key_value = local_get_key_value(buf, (char *)"MINIMUM_LONGITUDE");
		if(local_key_value) {
			this->set_min_longitude(atof(local_key_value)); // units?? = degrees
			free(local_key_value);
		}
		local_key_value = local_get_key_value(buf, (char *)"X_AXIS_PROJECTION_OFFSET");
		if(local_key_value) {
			this->set_x_axis_offset(atof(local_key_value)); // units??
			free(local_key_value);
		}
		local_key_value = local_get_key_value(buf, (char *)"Y_AXIS_PROJECTION_OFFSET");
		if(local_key_value) {
			this->set_y_axis_offset(atof(local_key_value)); // units??
			free(local_key_value);
		}
		local_key_value = local_get_key_value(buf, (char *)"A_AXIS_RADIUS");
		if(local_key_value) {
			this->set_a_axis_radius(atof(local_key_value)); // units??
			free(local_key_value);
		}
		local_key_value = local_get_key_value(buf, (char *)"B_AXIS_RADIUS");
		if(local_key_value) {
			this->set_b_axis_radius(atof(local_key_value)); // units??
			free(local_key_value);
		}
		local_key_value = local_get_key_value(buf, (char *)"C_AXIS_RADIUS");
		if(local_key_value) {
			this->set_c_axis_radius(atof(local_key_value)); // units??
			free(local_key_value);
		}
		local_key_value = local_get_key_value(buf, (char *)"FIRST_STANDARD_PARALLEL");
		if(local_key_value) {
			this->set_first_std_parallel(atof(local_key_value)); // units??
			free(local_key_value);
		}
		local_key_value = local_get_key_value(buf, (char *)"SECOND_STANDARD_PARALLEL");
		if(local_key_value) {
			this->set_second_std_parallel(atof(local_key_value)); // units??
			free(local_key_value);
		}
		local_key_value = local_get_key_value(buf, (char *)"POSITIVE_LONGITUDE_DIRECTION");
		if(local_key_value) {
			if(strchr(local_key_value, 'W') || strchr(local_key_value, 'w')) {
				this->set_long_dir_sign(-1.0);
			} else {
				this->set_long_dir_sign(1.0);
			}
			free(local_key_value);
		}
		local_key_value = local_get_key_value(buf, (char *)"CENTER_LATITUDE");
		if(local_key_value) {
			this->set_center_lat(atof(local_key_value)); // units??
			free(local_key_value);
		}
		local_key_value = local_get_key_value(buf, (char *)"CENTER_LONGITUDE");
		if(local_key_value) {
			this->set_center_long(atof(local_key_value)); // units??
			free(local_key_value);
		}
		local_key_value = local_get_key_value(buf, (char *)"REFERENCE_LATITUDE");
		if(local_key_value) {
			this->set_reference_lat(atof(local_key_value)); // units??
			free(local_key_value);
		}
		local_key_value = local_get_key_value(buf, (char *)"REFERENCE_LONGITUDE");
		if(local_key_value) {
			this->set_reference_long(atof(local_key_value)); // units??
			free(local_key_value);
		}
		local_key_value = local_get_key_value(buf, (char *)"X_AXIS_FIRST_PIXEL");
		if(local_key_value) {
			this->set_min_x(atoi(local_key_value)); // units??
			free(local_key_value);
		}
		local_key_value = local_get_key_value(buf, (char *)"X_AXIS_LAST_PIXEL");
		if(local_key_value) {
			this->set_max_x(atoi(local_key_value)); // units??
			free(local_key_value);
		}
		local_key_value = local_get_key_value(buf, (char *)"Y_AXIS_FIRST_PIXEL");
		if(local_key_value) {
			this->set_min_y(atoi(local_key_value)); // units??
			free(local_key_value);
		}
		local_key_value = local_get_key_value(buf, (char *)"Y_AXIS_LAST_PIXEL");
		if(local_key_value) {
			this->set_max_y(atoi(local_key_value)); // units??
			free(local_key_value);
		}
		local_key_value = local_get_key_value(buf, (char *)"MAP_PROJECTION_ROTATION");
		if(local_key_value) {
			this->set_map_rotn(atof(local_key_value)); // units??
			free(local_key_value);
		}

		local_key_value = local_get_key_value(buf, (char *)"ELEVATION_SCALE");
		if(local_key_value) {
			this->set_elev_scale(atof(local_key_value));
			free(local_key_value);
		}

		local_key_value = local_get_key_value(buf, (char *)"ELEVATION_BASE");
		if(local_key_value) {
			this->set_elev_offset(atof(local_key_value));
			free(local_key_value);
		}

		return(TRUE);

	} else {
		fprintf(stderr,"Whoops - Token >%s< is not PDSGeoData in from_string.\n", temp);
		return(FALSE);
	}

}


/* This is an excerpt from some PDS documentation which discusses
the use of pixel coordinates and georeferencing with the sinusoidal
equal area projection.


             APPENDIX D - GEOMETRIC DEFINITION OF A PIXEL




The purpose here is to describe the spatial or geometric definition of a
pixel used in the generation and utilization of the MDIM digital image
products. A broad range of factors enters into this question. For
example, is a pixel to be conceived of as a point or as an area? The
point definition would be most convenient, for instance, when dealing
with coordinate grid overlays. This results in an odd number of pixels
across a map that has an even number of spatial increments.  For
changing scales (for instance by even powers of 2) this definition
becomes a problem. In this case it makes more sense to treat a pixel as
a finite area.  Then an even number of pixels covers an even number of
spatial increments and decreasing/increasing scales by a power of 2
becomes trivial. However, grids now fall between pixels, at least in a
mathematical sense. Their treatment in the generation of hardcopy
therefore becomes an issue.

It was decided that the area concept of a pixel was the better choice;
we would have to live with the asymmetries introduced in things like
cartographic grids. There are various solutions: (1) use two pixels for
the width of a grid line, (2) stagger grid pixels back-and-forth across
the mathematical position, (3) use a convention whereby grid lines are
systematically drawn offset from their mathematical position.

The next issue is the conversion between integer coordinates and real
coordinates of the pixel mesh. We adopt the convention that pixels are
numbered (or named if you like) beginning in the upper left corner with
line 1, sample 1 (pixel 1,1); lines increase downward; samples increase
to the right. (Even this is not a universal standard; some astronomical
systems begin, perhaps more logically, in the lower left corner.) There
are three reasonable possibilities for aligning a real, or floating
point, coordinate system with the pixel mesh: the coordinate 1.0, 1.0
could be the upper left, the center, or the lower right of pixel 1,1.
The convention historically used for geometric calibration files (reseau
positions) and also used in the Multimission Image Processing Laboratory
at the Jet Propulsion Laboratory, is that the center of the pixel is
defined as its location in real coordinates. In other words, the real
coordinates of the center of pixel 1,1 are 1.0, 1.0. The top left corner
of the pixel is .5, .5 and the bottom right corner is 1.49999...,
1.499999. The bottom and right edge of a pixel is the mathematically
open boundary. This is the standard adopted in the MDIM image products.

Cartographic conventions must also be defined. The map projection
representation of a pixel is mathematically open at the increasing
(right and lower) boundaries, and mathematically closed at its left and
upper boundaries. An exception occurs at the physical limits of the
projection; the lower boundary of the lowest pixel is closed to include
the limit of the projection (e. g. the south pole).  Figure D.1 shows
the coordinates of Pixel 1,1.


              

                 
                 Figure D.1 - Coordinates of Pixel 1,1

                  longitude  180.0         179.00001
                              |               |
                  latitude    |               |   line
                     90.0  -- ----------------- -- .5
                              |               |
                              |               |
                              |               |
                              |               |
                              |       +       |
                              |   (1.0,1.0)   |
                              |               |
                              |               |
                              |               |
                  89.00001 -- ----------------- -- 1.49999
                              |               |
                              |               |
                     sample  .5            1.49999


Finally, we must select a convention for drawing grid lines for various
cartographic coordinates on planetary images and maps. The convention
used for MDIM image products is that a grid line is drawn in the pixels
that contain its floating point value until the open boundary is reached
and then an exception is made so that the outer range of latitude and
longitude will always appear on the image.  This means, in the example
given above, a 10 degree grid would start on pixel 1 and be drawn on
every tenth pixel (11,21,31,...) until the open boundary is reached.
Then the line would be drawn on the pixel previous to the open boundary
(line 180 instead of line 181, or sample 360 instead of 361).


To summarize, the MDIM conventions are:

  1) Pixels are treated as areas, not as points.

  2) The integer coordinates begin with 1,1 (read "line 1, sample 1")
     for the upper-left-most pixel; lines increase downward; samples
     increase to the right.

  3) Integer and floating point image coordinates are the same at the
     center of a pixel.

  4) Grids will be drawn in the pixels that contain the floating point
     location of the grid lines except for open boundaries, which will
     be drawn to the left or above the open boundary.





           APPENDIX E - SINUSOIDAL EQUAL-AREA PROJECTION EQUATION




MDIM's are presented in a Sinusoidal Equal-area map projection. In this
projection, parallels of latitude are straight lines, with constant
distances between equal latitude intervals. Lines of constant longitude
on either side of the projection meridian are curved since longitude
intervals decrease with the cosine of latitude to account for their
convergence toward the poles. This projection offers a number of
advantages for storing and managing global digital data; in particular,
it is computationally simple, and data are stored in a compact form. 
 
The Sinusoidal Equal-area projection is characterized by a projection 
longitude, which is the center meridian of the projection, and a scale,
which is given in units of pixels/degree. The center latitude for all
MDIM's is the equator. Each MDIM file contains its own central meridian.

The transformation from latitude and longitude to line and sample for
planets with a direction of positive longitude of WEST is given by the
following equations:

  line = INT(X_AXIS_PROJECTION_OFFSET - lat*MAP_RESOLUTION + 1.0)

  sample = INT(Y_AXIS_PROJECTION_OFFSET - (lon - CENTER_LONGITUDE)*
       MAP_RESOLUTION*cos(lat) + 1.0)
                                            
                                       
Note that integral values of line and sample correspond to center of
a pixel. Lat and lon are the latitude and longitude of a given spot
on the surface. 
 
INT is the fortran equivalent floating point to integer function. This
function converts floating point values to integer by truncation of the
fractional part of the floating point value.

X_AXIS_PROJECTION_OFFSET is the line number minus one on which the map
projection origin occurs. The map projection origin is the intersection
of the equator and the projection longitude. The value of
X_AXIS_PROJECTION_OFFSET is positive for images starting north of the
equator and is negative for images starting south of the equator. The
X_AXIS_PROJECTION_OFFSET is found in the labels of each image file.
                                                                    
Y_AXIS_PROJECTION_OFFSET is the nearest sample number to the left of the
projection longitude. The value of Y_AXIS_PROJECTION_OFFSET is positive
for images starting to the west of the projection longitude and is
negative for images starting to the east of the projection longitude.
The Y_AXIS_PROJECTION_OFFSET is found in the labels of each image file. 

CENTER_LONGITUDE is the value of the projection longitude, which is the
longitude that passes through the center of the projection. The
CENTER_LONGITUDE is found in the labels of each image file.
                                                                             
MAP_RESOLUTION is the number of pixels per degree on the planet. The
values for MDIM products will be 256, 64, 16, and 4. The MAP_RESOLUTION
is found in the labels of each image file.

There are four PDS parameters that specify the latitude and longitude
boundaries of an image.  MAXIMUM_LATITUDE and MINIMUM_LATITUDE specify
the latitude boundaries of the image, and MAXIMUM_LONGITUDE and
MINIMUM_LONGITUDE specify the longitudinal boundaries of the map.

A special note is required for the MAXIMUM_LONGITUDE and
MINIMUM_LONGITUDE parameters that define the boundaries of an image. The
MAXIMUM_LONGITUDE will be greater than the MINIMUM_LONGITUDE except when
the image map crosses the zero meridian. When the zero meridian is
contained in the image area, then the MINIMUM_LONGITUDE will be greater
than the MAXIMUM_LONGITUDE. When this occurs, it may be convenient for a
computer algorithm that uses these parameters to subtract 360.0 degrees
from the MINIMUM_LONGITUDE. For example, if an image had longitude
boundary limits from 10.0 degrees longitude (MAXIMUM_LONGITUDE) to 350.0
degrees longitude (MINIMUM_LONGITUDE) then it is implied that the zero
meridian is in the middle of the image file. One could think of the
longitude limits of the file going from 10.0 to -10.0 degrees longitude.

For global maps that cover the entire 360 degrees of a planet, the
MINIMUM_LONGITUDE will equal the MAXIMUM_LONGITUDE indicating that the
"left" edge of the map has the same longitude as the "right" edge of the
map.



The IMAGE_MAP_PROJECTION object is one of two distinct objects that define 
the map projection used in creating the digital images in a PDS data set.The 
name of the other associated object that completes the definition is called
DATA_SET_MAP_PROJECTION.(see Appendix B)

The map projection information resides in these two objects, essentially to 
reduce data reduncdancy and at the same time allow the inclusion of elements 
needed to process the data at the image level. Basically, static information 
that is applicable to the complete data set reside in the DATA_SET_MAP_PROJECTION 
object, while dynamic information that is applicable to the individual images 
reside in the IMAGE_MAP_PROJECTION object.

The line_first_pixel, line_last_pixel, sample_first_pixel, and sample_last_pixel 
keywords are used to indicate which way is up in an image. Sometimes an image 
can be shifted or flipped prior to it being physically recorded. These keywords 
are used in calculating the mapping of pixels between the original image and the 
stored image.

The following equations give the byte offsets needed to determine the mapping 
of a pixel (X,Y) from the original image to a pixel in the stored image:

The sample offset from the first pixel is:


    sample_bits * (Y - sample_first_pixel) * line_samples
    -----------------------------------------------------
       8 * (sample_last_pixel - sample_first_pixel + 1)


The line offset from the first image line is:


        (X - line_first_pixel) * lines
   ---------------------------------------- 
   (line_last_pixel - line_first_pixel + 1)


Additionally, in any image, ABS (sample_last_pixel - sample_first_pixel + 1) is 
always equal to line_samples, and ABS (line_last_pixel - line_first_pixel + 1) 
is always equal to lines.

Example

Take a 1K by 1K 8-bit image which is rotated about the x-axis 180 degrees 
prior to being physically recorded.


Original Image: Positive direction is to the right and down




Stored Image: Positive direction is to the right and up




These pixel location values (*) are the positions from the original image. 
For example, the first pixel in the stored image (normally referred to as (1,1)) 
came from the position (1,1024) in the original image. These original values 
are used for the following IMAGE_MAP_PROJECTION keywords in the PDS label 
for the stored image:


sample_first_pixel = 1 
sample_last_pixel = 1024
line_first_pixel = 1024
line_last_pixel = 1


Now, given a pixel on the original image, P(X,Y) = (2,2) determine its 
location (P') in the stored image.


sample offset = (8 * (2 - 1) * 1024) / (8 * (1024 - 1 + 1)) = 1
line offset = ((2 - 1024) * 1024) / (1 - 1024 + 1) = (-1022)


Therefore, P' is located at 1 byte from the first sample, and 1022 bytes 
(in the negative direction) from the first line in the stored image. See 
diagram above.

Required Keywords

   1.MAP_PROJECTION_TYPE 
   2.A_AXIS_RADIUS 
   3.B_AXIS_RADIUS 
   4.C_AXIS_RADIUS 
   5.FIRST_STANDARD_PARALLEL 
   6.SECOND_STANDARD_PARALLEL 
   7.POSITIVE_LONGITUDE_DIRECTION 
   8.CENTER_LATITUDE 
   9.CENTER_LONGITUDE 
  10.REFERENCE_LATITUDE 
  11.REFERENCE_LONGITUDE 
  12.LINE_FIRST_PIXEL 
  13.LINE_LAST_PIXEL 
  14.SAMPLE_FIRST_PIXEL 
  15.SAMPLE_LAST_PIXEL 
  16.MAP_PROJECTION_ROTATION 
  17.MAP_RESOLUTION 
  18.MAP_SCALE 
  19.MAXIMUM_LATITUDE 
  20.MINIMUM_LATITUDE 
  21.EASTERNMOST_LONGITUDE 
  22.WESTERNMOST_LONGITUDE 
  23.LINE_PROJECTION_OFFSET 
  24.SAMPLE_PROJECTION_OFFSET 
  25.COORDINATE_SYSTEM_TYPE 
  26.COORDINATE_SYSTEM_NAME 

Optional Keywords

   1.DATA_SET_ID 
   2.IMAGE_ID 
   3.HORIZONTAL_FRAMELET_OFFSET 
   4.VERTICAL_FRAMELET_OFFSET 

Required Objects

   1.DATA_SET_MAP_PROJECTION 

Optional Objects

None 
*/
