// geodata.h
//
// Written by John Wright 03/27/97

#ifndef _GEODATA_H_
#define _GEODATA_H_

#ifndef TRUE
#define	TRUE	1
#define	FALSE	0
#endif

#include "stdio.h"
#include "string.h"
#include "image/geodatatypes.h"

// This class contains georeferencing information for the image class.
// This contains map projection, spheroid, lat/long type stuff.
// This is a base class for derived classes which might work other ways.

class GeoData {

    protected:

	void	init(void) { 
			datum_strings = NULL; 
			projection_strings = NULL; 
			elev_scale = 1.0;
			elev_offset = 0.0;
		}

	char	**datum_strings;
	char	**projection_strings;

	double	elev_scale;	// multiplier for z to elev mapping
	double	elev_offset;	// constant for z to elev mapping (elev = x * scale + offset)

    public:

	virtual	int	data_class_type(void) { return(BASE_GEO_DATA); }	// base class type

	virtual	char	*datum_type_to_string(DatumType dt) { 
			if(datum_strings && dt >= 0 && dt < NUM_DATUM_TYPES) {
				return(datum_strings[dt]);
			} else {
				return((char *)"UNKNOWN"); 
			}
		}
	virtual	DatumType	datum_type_from_string(char *ds) { 
			int	i;
			if(datum_strings && ds) {
				for(i=0; i<NUM_DATUM_TYPES; i++) {
					if(!strcmp(ds, datum_strings[i])) {
						return((DatumType)i);
					}
				}
			}
			return(UNKNOWN); 
		}

	virtual	char	*projection_type_to_string(ProjectionType dt) { 
			if(projection_strings && dt >= 0 && dt < NUM_DATUM_TYPES) {
				return(projection_strings[dt]);
			} else {
				return((char *)"UNKNOWN"); 
			}
		}
	virtual	ProjectionType	projection_type_from_string(char *ds) { 
			int	i;
			if(projection_strings && ds) {
				for(i=0; i<NUM_PROJECTION_TYPES; i++) {
					if(!strcmp(ds, projection_strings[i])) {
						return((ProjectionType)i);
					}
				}
			}
			return(UNKNOWN); 
		}

	// get projection and datum from  correct file type - danp
	virtual ProjectionType get_projection() { return(UNKNOWN); }
	virtual DatumType get_datum() { return(UNKNOWN); }

	virtual void	set_elev_scale(double value) { elev_scale = value; }
	virtual double	get_elev_scale(void) { return(elev_scale); }

	virtual void	set_elev_offset(double value) { elev_offset = value; }
	virtual double	get_elev_offset(void) { return(elev_offset); }

	// coordinate space converters (virtuals)

	virtual	double	z_to_elev(double z) { return(z*elev_scale + elev_offset); }
	virtual double	elev_to_z(double el) { return((el - elev_offset)/elev_scale); }

	virtual void	xyztolle(double x, double y, double z, double &lat, double &lng, double &el) {
			lat = xytolat(x,y);
			lng = xytolong(x,y);
			el = z_to_elev(z);
		}
	virtual void	lletoxyz(double lat, double lng, double el, double &x, double &y, double &z) {
			x = lltox(lat, lng);
			y = lltoy(lat, lng);
			z = elev_to_z(el);
		}

	virtual	double	lltox(double , double ) { return(0.0); }
	virtual	double	lltoy(double , double ) { return(0.0); }
	virtual	void	lltoxy(double latitude, double longitude, double &x, double&y) {
			x = lltox(latitude, longitude);
			y = lltoy(latitude, longitude);
		}
	virtual	double	utmtox(double , double ) { return(0.0); }
	virtual	double	utmtoy(double , double ) { return(0.0); }
	virtual	void	utmtoxy(double easting, double northing, double &x, double&y) {
			x = utmtox(easting, northing);
			y = utmtoy(easting, northing);
		}
	virtual	double	utmtolat(double , double ) { return(0.0); }
	virtual	double	utmtolong(double , double ) { return(0.0); }
	virtual	void	utmtoll(double easting, double northing, double &latitude, double &longitude) {
			latitude = utmtolat(easting, northing);
			longitude = utmtolong(easting, northing);
		}
	virtual	double	lltoe(double , double ) { return(0.0); }
	virtual	double	llton(double , double ) { return(0.0); }
	virtual	void	lltoutm(double latitude, double longitude, double &easting, double &northing) {
			easting = lltoe(latitude, longitude);
			northing = llton(latitude, longitude);
		}
	virtual	double	xytolat(double , double ) { return(0.0); }
	virtual	double	xytolong(double , double ) { return(0.0); }
	virtual	void	xytoll(double x, double y, double &latitude, double &longitude) {
			latitude = xytolat(x, y);
			longitude = xytolong(x, y);
		}
	virtual	double	xytoe(double , double ) { return(0.0); }
	virtual	double	xyton(double , double ) { return(0.0); }
	virtual	void	xytoutm(double x, double y, double &easting, double &northing) {
			easting = xytoe(x, y);
			northing = xyton(x, y);
		}

	virtual	char	*to_string(void) {
			char	buff[256];
			sprintf(buff,"El_Scale = %g\nEl_Base = %g\n",elev_scale, elev_offset);
			return(strdup(buff));
		}
	virtual	int	from_string(char *buff) {
			char	lbuf[256];
			sscanf(buff,"%8s", lbuf);
			if(!strcmp(lbuf, "El_Scale")) {
				sscanf(buff,"El_Scale = %lf\nEl_Base = %lf\n", &elev_scale, &elev_offset);
				return(TRUE);
			} else {
				return(FALSE);
			}
		}

	// Constructors:
	GeoData()  { init(); }

	virtual ~GeoData() { ; }
	};


// convenience function for instantiating derived classes
// this function looks at the keyword at the beginning of
// the argument string, which is the name of the derived
// class of geodata, and instantiates the correct class
// and feeds it the string by calling from_string.
GeoData	*parse_object_geodata(char *);

#endif // _GEODATA_H_
