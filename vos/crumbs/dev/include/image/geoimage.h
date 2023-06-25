// geoimage.h
//
// Written by John Wright 03/27/97

#ifndef _GEOIMAGE_H_
#define _GEOIMAGE_H_


#include "image/image.h"
#include "image/geodata.h"

// This class adds georeferencing information to the image class.
// The same imagefile types, datatypes, and displays are supported.
// An additional GeoData pointer is added with methods for access.

class GeoImage : public Image {

    protected:

	GeoData	*geo_data;	// pointer to georeferencing data

	void	init(void) { geo_data = NULL; }

    public:

	virtual	int	image_class_type(void) { return(GEOIMAGE); }	// GeoImage class type

	void	free_geo_data(void) {
			if(geo_data) delete geo_data; geo_data = NULL;
		}

	void	set_geo_data(GeoData *gd) {
			if(geo_data) delete geo_data; geo_data = gd;
		}
	GeoData	*get_geo_data(void) { 
			if(geo_data) {
				return(geo_data); 
			} else if(data) {
				return(data->get_geo_data());
			} else return(NULL); 
		}
	operator GeoData*() { return(get_geo_data()); }
	void	operator=(GeoData *d) { set_geo_data(d); }

	// coordinate space converters (virtuals)
	virtual	double	lltox(double lat, double lng) { 
			if(get_geo_data()) {
				return(get_geo_data()->lltox(lat, lng));
			} else {
				return(0.0);
			}
		}
	virtual	double	lltoy(double lat, double lng) { 
			if(get_geo_data()) {
				return(get_geo_data()->lltoy(lat, lng));
			} else {
				return(0.0);
			}
		}
	virtual	void	lltoxy(double latitude, double longitude, double &x, double&y) {
			x = lltox(latitude, longitude);
			y = lltoy(latitude, longitude);
		}
	virtual	double	utmtox(double easting, double northing) {
			if(get_geo_data()) {
				return(get_geo_data()->utmtox(easting, northing));
			} else {
				return(0.0);
			}
		}
	virtual	double	utmtoy(double easting, double northing) {
			if(get_geo_data()) {
				return(get_geo_data()->utmtoy(easting, northing));
			} else {
				return(0.0);
			}
		}
	virtual	void	utmtoxy(double easting, double northing, double &x, double&y) {
			x = utmtox(easting, northing);
			y = utmtoy(easting, northing);
		}
	virtual	double	utmtolat(double easting, double northing) { 
			if(get_geo_data()) {
				return(get_geo_data()->utmtolat(easting, northing));
			} else {
				return(0.0);
			}
		}
	virtual	double	utmtolong(double easting, double northing) { 
			if(get_geo_data()) {
				return(get_geo_data()->utmtolong(easting, northing));
			} else {
				return(0.0);
			}
		}
			
	virtual	void	utmtoll(double easting, double northing, double &latitude, double &longitude) {
			latitude = utmtolat(easting, northing);
			longitude = utmtolong(easting, northing);
		}
	virtual	double	lltoe(double lat, double lng) { 
			if(get_geo_data()) {
				return(get_geo_data()->lltoe(lat, lng));
			} else {
				return(0.0);
			}
		}
	virtual	double	llton(double lat, double lng) { 
			if(get_geo_data()) {
				return(get_geo_data()->llton(lat, lng));
			} else {
				return(0.0);
			}
		}
	virtual	void	lltoutm(double latitude, double longitude, double &easting, double &northing) {
			easting = lltoe(latitude, longitude);
			northing = llton(latitude, longitude);
		}

	virtual	double	xytolat(double x, double y) { 
			if(get_geo_data()) {
				return(get_geo_data()->xytolat(x, y));
			} else {
				return(0.0);
			}
		}
	virtual	double	xytolong(double x, double y) { 
			if(get_geo_data()) {
				return(get_geo_data()->xytolong(x, y));
			} else {
				return(0.0);
			}
		}
	virtual	void	xytoll(double x, double y, double &latitude, double &longitude) {
			latitude = xytolat(x, y);
			longitude = xytolong(x, y);
		}
			
	virtual	double	xytoe(double x, double y) { 
			if(get_geo_data()) {
				return(get_geo_data()->xytoe(x, y));
			} else {
				return(0.0);
			}
		}
	virtual	double	xyton(double x, double y) { 
			if(get_geo_data()) {
				return(get_geo_data()->xyton(x, y));
			} else {
				return(0.0);
			}
		}
	virtual	void	xytoutm(double x, double y, double &e, double &n) {
			e = xytoe(x, y);
			n = xyton(x, y);
		}
	// projection and datum types - danp
	virtual ProjectionType get_projection(void) { 
		if (get_geo_data()) return get_geo_data()->get_projection();
		else return(UNKNOWN);
	}

	virtual DatumType get_datum(void) { 
		if (get_geo_data()) return get_geo_data()->get_datum();
		else return(UNKNOWN);
	}
			
	// Constructors:
	GeoImage() : Image() { init(); }
	GeoImage(char *fname, int t=0) : Image(fname, t) { init(); }
#ifndef _NO_IMAGEDISP_
	GeoImage(char *fname, Display *d) : Image(fname, d) { init(); }
#endif // _NO_IMAGEDISP_
	GeoImage(ImageData *d, ImageFile *f=NULL, ImageDisp *id=NULL) 
		: Image(d, f, id) { init(); }
	GeoImage(ImageData *d, ImageFile *f, ImageDisp *id, GeoData *gd) 
		: Image(d, f, id) { init(); set_geo_data(gd); }
	GeoImage(ImageFile *f, ImageDisp *id=NULL) : Image(f, id) { init(); }
	GeoImage(ImageData *d, ImageDisp *id) : Image(d, id) { init(); }
	GeoImage(ImageDisp *id) : Image (id) { init(); }
	GeoImage(GeoData *gd) : Image () { init(); set_geo_data(gd); }

	~GeoImage() { if(clean) { free_geo_data(); } }
	};


#endif // _GEOIMAGE_H_
