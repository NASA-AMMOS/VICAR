// map_geodata.h
//
// Written by John Wright 10/04/00

#ifndef _MAP_GEODATA_H_
#define _MAP_GEODATA_H_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "image/geodata.h"
#include "image/image.h"
#include "image/datatypes.h"

// This class contains georeferencing information for image that utilize
// other images to perform corssreferencing.  To convert xy to latlong,
// you lookup the latlong values in an image whose size matches that of
// the main image.  To convert latlong to xy, you lookup the xy values
// in an image that is 360 x 180 and perform interpolation.

// define the band numbers of the two images which correspond to x,y,lat, and long
#define	LAT_BAND	0
#define	LNG_BAND	1
#define	X_BAND		0
#define Y_BAND		1

class MAPGeoData : public GeoData {

    private:

    protected:

	double	min_lat, max_lat;
	double	min_lng, max_lng;

	Image	*ll_image, *xy_image;

	void	init(void);

    public:

	virtual	int	data_class_type(void) { return(MAP_GEO_DATA); }	// base class type

	// coordinate space converters (virtuals)
	virtual	double	lltox(double latitude, double longitude);
	virtual	double	lltoy(double latitude, double longitude);

	virtual	double	utmtox(double easting, double northing);
	virtual	double	utmtoy(double easting, double northing);

	virtual	double	utmtolat(double easting, double northing);
	virtual	double	utmtolong(double easting, double northing);

	virtual	double	lltoe(double latitude, double longitude);
	virtual	double	llton(double latitude, double longitude);

	virtual	double	xytolat(double , double );
	virtual	double	xytolong(double , double );

	virtual	double	xytoe(double , double );
	virtual	double	xyton(double , double );

	// get, set, and clear methods for specific data items
	void	create_xy_image(void);	// creates xy image from ll image
	void	create_ll_image(void);	// creates ll image from xy image
	void	new_xy_image(Image *img) { xy_image = img; }
	void	new_ll_image(Image *img) { ll_image = img; }
	void	delete_xy_image(void) { set_xy_image(NULL); }
	void	delete_ll_image(void) { set_ll_image(NULL); }
	void	set_xy_image(Image *img) { 
			if(xy_image) delete(xy_image);
			xy_image = img;
		}
	void	set_ll_image(Image *img) { 
			if(ll_image) delete(ll_image);
			ll_image = img;
		}
	Image	*get_xy_image(void) { return(xy_image); }
	Image	*get_ll_image(void) { return(ll_image); }

	void	clear_min_max(void) {
			min_lat=90.0;
			max_lat=-90.0;
			min_lng=180.0;
			max_lng=-180.0;
		}
	void	set_min_max_lat(double lo, double hi) {
                        min_lat = lo;
                        max_lat = hi;
		}
	void	set_min_max_lng(double lo, double hi) {
                        min_lng = lo;
                        max_lng = hi;
		}
	void	min_max_lat_lng(double lat, double lng) {
                        if(lat < min_lat)min_lat = lat;
                        if(lat > max_lat)max_lat = lat;
                        if(lng < min_lng)min_lng = lng;
                        if(lng > max_lng)max_lng = lng;
		}
	int	in_lat(double lat) {
			return( lat >= min_lat && lat <= max_lat );
		}
	int	in_lng(double lng) {
			return( lng >= min_lng && lng <= max_lng );
		}
	int	in_image(double lat, double lng) {
			return( in_lat(lat) && in_lng(lng) );
		}
			
	// functions to dump the geodata contents to a string or parse out of a string
	char	*to_string(void);
	int	from_string(char *);
		

	// Constructors:
	MAPGeoData()  { init(); }

	~MAPGeoData(); 
	};

#endif // _MAP_GEODATA_H_
