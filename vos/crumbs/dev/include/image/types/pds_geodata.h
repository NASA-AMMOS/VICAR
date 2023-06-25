// Class PDSGeoData
// Created by John Wright
// Created Sun Feb 15 18:49:45 1998
// This class implements the georeferencing tools needed for
// PDS (Planetary Data Systems) images, i.e. sinusoidal projections

#ifndef _PDSGeoData_H_
#define _PDSGeoData_H_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "image/geodata.h"

class PDSGeoData:public GeoData {

    private:

    protected:

	void	init(void); 

	DatumType	datum;		// geodetic datum (see geodatatypes.h)
	ProjectionType	projection;	// map projection (see geodatatypes.h)
	char		*catalog;	// map projection catalog file
	double		resolution;	// resolution in pixels per degree
	double		scale;		// map scale
	double		min_latitude;
	double		max_latitude;
	double		min_longitude;
	double		max_longitude;
	double		x_axis_offset;
	double		y_axis_offset;
	double		a_axis_radius;
	double		b_axis_radius;
	double		c_axis_radius;
	double		first_std_parallel;
	double		second_std_parallel;
	double		long_dir_sign;	// set to cause long west - east +
					// =-1.0 if file says west +
	double		center_lat;
	double		center_long;
	double		reference_lat;
	double		reference_long;
	int		min_x, max_x, min_y, max_y;
	double		map_rotn;

    public:

	virtual	int	data_class_type(void) { return(PDS_GEO_DATA); }	// PDS Image georeferencing class type

	// coordinate space converters (virtuals)
	virtual	double	lltox(double latitude, double longitude);
	virtual	double	lltoy(double latitude, double longitude);

	virtual	double	xytolat(double , double );
	virtual	double	xytolong(double , double );

	// get, set, and clear methods for specific data items
	double	get_scale(void) { return(scale); }
	void	set_scale(double num) { scale = num; }

	double	get_resolution(void) { return(resolution); }
	void	set_resolution(double num) { resolution = num; }

	DatumType	get_datum(void) { return(datum); }
	void		set_datum(DatumType d) { datum = d; }
	
	ProjectionType	get_projection(void) { return(projection); }
	void		set_projection(ProjectionType d) { projection = d; }

	char		*get_catalog(void) { return(catalog); }
	void		set_catalog(char *nm) { 
				free(catalog);
				if(nm) catalog = strdup(nm);
				else catalog = NULL;
			}
	
	double	get_map_rotn(void) { return(map_rotn); }
	void	set_map_rotn(double num) { map_rotn = num; }

	double		get_min_latitude(void) { return(min_latitude); }
	void		set_min_latitude(double val) { min_latitude = val; }
	double		get_max_latitude(void) { return(max_latitude); }
	void		set_max_latitude(double val) { max_latitude = val; }

	double		get_min_longitude(void) { return(min_longitude); }
	void		set_min_longitude(double val) { min_longitude = val; }
	double		get_max_longitude(void) { return(max_longitude); }
	void		set_max_longitude(double val) { max_longitude = val; }

	double		get_x_axis_offset(void) { return(x_axis_offset); }
	void		set_x_axis_offset(double val) { x_axis_offset = val; }
	double		get_y_axis_offset(void) { return(y_axis_offset); }
	void		set_y_axis_offset(double val) { y_axis_offset = val; }
	double		get_a_axis_radius(void) { return(a_axis_radius); }
	void		set_a_axis_radius(double val) { a_axis_radius = val; }
	double		get_b_axis_radius(void) { return(b_axis_radius); }
	void		set_b_axis_radius(double val) { b_axis_radius = val; }
	double		get_c_axis_radius(void) { return(c_axis_radius); }
	void		set_c_axis_radius(double val) { c_axis_radius = val; }
	double		get_first_std_parallel(void) { return(first_std_parallel); }
	void		set_first_std_parallel(double val) { first_std_parallel = val; }
	double		get_second_std_parallel(void) { return(second_std_parallel); }
	void		set_second_std_parallel(double val) { second_std_parallel = val; }
	double		get_long_dir_sign(void) { return(long_dir_sign); }
	void		set_long_dir_sign(double val) { long_dir_sign = val; }
	double		get_center_lat(void) { return(center_lat); }
	void		set_center_lat(double val) { center_lat = val; }
	double		get_center_long(void) { return(center_long); }
	void		set_center_long(double val) { center_long = val; }
	double		get_reference_lat(void) { return(reference_lat); }
	void		set_reference_lat(double val) { reference_lat = val; }
	double		get_reference_long(void) { return(reference_long); }
	void		set_reference_long(double val) { reference_long = val; }
	int		get_min_x(void) { return(min_x); }
	void		set_min_x(int val) { min_x = val; }
	int		get_max_x(void) { return(max_x); }
	void		set_max_x(int val) { max_x = val; }
	int		get_min_y(void) { return(min_y); }
	void		set_min_y(int val) { min_y = val; }
	int		get_max_y(void) { return(max_y); }
	void		set_max_y(int val) { max_y = val; }

	char		*to_string(void);
	int		from_string(char *);

    // Constructors

	PDSGeoData() { init(); }

    // Destructor

	~PDSGeoData() { if(catalog) free(catalog); }

};

/* this is a sample georeferencing block from a pds file
   note that it may be incomplete but this version is based on it

OBJECT               = IMAGE_MAP_PROJECTION_CATALOG
  ^DATA_SET_MAP_PROJECTION_CATALOG = "DSMAPDIM.LBL"
  MAP_PROJECTION_TYPE =  SINUSOIDAL
  MAP_RESOLUTION     = 64  <PIXEL/DEG>
  MAP_SCALE          = 0.92540634<KM/PIXEL>
  MAXIMUM_LATITUDE   = 37.50000 
  MINIMUM_LATITUDE   = 22.50000 
  MAXIMUM_LONGITUDE  = 75.00000 
  MINIMUM_LONGITUDE  = 59.91416 
  X_AXIS_PROJECTION_OFFSET = 2400.00   
  Y_AXIS_PROJECTION_OFFSET = 443.46    
  A_AXIS_RADIUS      = 3393.40
  B_AXIS_RADIUS      = 3393.40
  C_AXIS_RADIUS      = 3375.73
  FIRST_STANDARD_PARALLEL = "N/A"
  SECOND_STANDARD_PARALLEL = "N/A"
  POSITIVE_LONGITUDE_DIRECTION = WEST
  CENTER_LATITUDE    = 0.00000
  CENTER_LONGITUDE   = 67.50000 
  REFERENCE_LATITUDE = "N/A"
  REFERENCE_LONGITUDE = "N/A"
  X_AXIS_FIRST_PIXEL = 1
  Y_AXIS_FIRST_PIXEL = 1
  X_AXIS_LAST_PIXEL  = 960 
  Y_AXIS_LAST_PIXEL  = 892 
  MAP_PROJECTION_ROTATION = "N/A"
END_OBJECT           = IMAGE_MAP_PROJECTION_CATALOG


OBJECT                 = IMAGE_MAP_PROJECTION_CATALOG
  ^DATA_SET_MAP_PROJECTION_CATALOG = "DSMAPDIM.LBL"
  MAP_PROJECTION_TYPE  = SINUSOIDAL
  MAP_RESOLUTION       = x<PIXEL/DEG>
  MAP_SCALE            = x.xxxxx<KM/PIXEL>
  MAXIMUM_LATITUDE     = x.xxxxx
  MINIMUM_LATITUDE     = x.xxxxx
  MAXIMUM_LONGITUDE    = x.xxxxx
  MINIMUM_LONGITUDE    = x.xxxxx
  X_AXIS_PROJECTION_OFFSET = x.xxxxx
  Y_AXIS_PROJECTION_OFFSET = x.xxxxx
  A_AXIS_RADIUS        = 3393.40
  B_AXIS_RADIUS        = 3393.40
  C_AXIS_RADIUS        = 3375.73
  FIRST_STANDARD_PARALLEL = "N/A"
  SECOND_STANDARD_PARALLEL = "N/A"
  POSITIVE_LONGITUDE_DIRECTION = WEST
  CENTER_LATITUDE      = 0.00000
  CENTER_LONGITUDE     = x.xxxxx
  REFERENCE_LATITUDE   = "N/A"
  REFERENCE_LONGITUDE  = "N/A"
  X_AXIS_FIRST_PIXEL   = 1
  Y_AXIS_FIRST_PIXEL   = 1
  X_AXIS_LAST_PIXEL    = xxxx
  Y_AXIS_LAST_PIXEL    = xxxx
  MAP_PROJECTION_ROTATION = "N/A"
END_OBJECT             = IMAGE_MAP_PROJECTION_CATALOG
    This keyword sequence describes the cartographic keywords that 
    define the mapping parameters of the image.
       ^DATA_SET_MAP_PROJECTION_CATALOG = "DSMAPDIM.LBL"
           This keyword points to a separate file (DSMAPDIM.LBL) on
           the CDROM that contains supplemental and nonessential
           keyword descriptors for map projection parameters. By
           convention, supplemental labels are found in the LABEL
           directory.
       MAP_PROJECTION_TYPE  = SINUSOIDAL
           This element identifies the type of projection used in the
            map. This value is always SINUSOIDAL for the MDIM products
            and signifies a Sinusoidal Equal-Area projection.
       MAP_RESOLUTION       = x<PIXEL/DEG>
           This element identifies the scale of the MDIM image file. The
           resolution is defined in pixels per degree.
       MAP_SCALE            = x.xxxxx<KM/PIXEL>
           This element identifies the scale of the MDIM image file
           and is defined in kilometers per pixel.
       MAXIMUM_LATITUDE     = x.xxxxx
           This element specifies the northern most latitude in the MDIM
           image file.
       MINIMUM_LATITUDE     = x.xxxxx
           This element specifies the southern most latitude in the MDIM
           image file.
       MAXIMUM_LONGITUDE    = x.xxxxx
           This element specifies the left-most longitude of the image
           file.
       MINIMUM_LONGITUDE    = x.xxxxx
           This element specifies the right-most longitude of the image
           file.
       X_AXIS_PROJECTION_OFFSET = x.xxxxx
           This element provides the line offset value of the map
           projection origin position from line and sample 1,1. Note
           that the positive direction is to the right and down. See
           Appendix E for the use of this element.
       Y_AXIS_PROJECTION_OFFSET = x.xxxxx
           This element provides the sample offset value of the map
           projection origin position from line and sample 1,1. Note
           that the positive direction is to the right and down. See
           Appendix E for the use of this element. 
       A_AXIS_RADIUS        = 3393.40
       B_AXIS_RADIUS        = 3393.40
       C_AXIS_RADIUS        = 3375.73
           These elements provide the semi-major axis (A), intermediate
           axis (B), and semi-minor axis of the ellipsoid that defines
           the shape of the body defined in kilometers. These values
           are always 3393.40, 3393.40, and 3375.73 respectively.
       FIRST_STANDARD_PARALLEL = "N/A"
           This element is a mapping transformation parameter. The
           Sinusoidal Equal-Area projection does not used this element.
       SECOND_STANDARD_PARALLEL = "N/A"
           This element is a mapping transformation parameter. The
           Sinusoidal Equal-Area projection does not used this element.
       POSITIVE_LONGITUDE_DIRECTION = WEST
           This element identifies the direction of longitude
           (EAST,WEST) for a planet. The IAU definition for direction
           of positive longitude is adopted. For MARS this direction
           is WEST.        
       CENTER_LATITUDE      = 0.00000
           This element identifies the center latitude of the
            projection. For Sinusoidal Equal-Area projections, this
            value is zero.
       CENTER_LONGITUDE     = x.xxxxx
           This element identifies the center longitude of the
           projection. Each MDIM image file has its own center
           longitude. See Appendix E for the use of this mapping
           parameter.
       REFERENCE_LATITUDE   = "N/A"
           This element is a mapping transformation parameter. The
           Sinusoidal Equal-Area projection does not used this element.
       REFERENCE_LONGITUDE  = "N/A"
           This element is a mapping transformation parameter. The
           Sinusoidal Equal-Area projection does not used this element.
       X_AXIS_FIRST_PIXEL   = 1
           This element provides the x-dimension index to be assigned
           the first pixel that was physically recorded at the
           beginning of the image array. This value always 1 for MDIM
           image files.
       Y_AXIS_FIRST_PIXEL   = 1
           This element provides the y-dimension index to be assigned
           the first pixel that was physically recorded at the
           beginning of the image array. This value always 1 for MDIM
           image files.
       X_AXIS_LAST_PIXEL    = xxxx
           This element provides the x-dimension index to be assigned
           the last pixel that was physically recorded at the end of
           the image array. For MDIM image files, this element equals
           the number of lines in the image.
       Y_AXIS_LAST_PIXEL    = xxxx
           This element provides the y-dimension index to be assigned
           the last pixel that is physically recorded at the end of
           the image array. For MDIM image files, this element equals
           the number of samples in the image.
       MAP_PROJECTION_ROTATION = "N/A"
           This element is a mapping transformation parameter. The
           Sinusoidal Equal-Area projection does not used this element.


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


*/

#endif

