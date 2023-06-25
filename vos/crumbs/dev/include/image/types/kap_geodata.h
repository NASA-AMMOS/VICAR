// kap_geodata.h
//
// Written by John Wright 04/01/97

#ifndef _KAP_GEODATA_H_
#define _KAP_GEODATA_H_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "image/geodata.h"

// This class contains georeferencing information for BSB kap images.
// This contains map projection, spheroid, lat/long type stuff.

class KAPGeoData : public GeoData {

    private:

    protected:

	// BSB Record
	char	*kapname;		// chart name
	int	kapnumber;		// chart number
	int	draw_units;		// pixels/inch

	// KNP Record
	long		scale;		// chart scale
	DatumType	datum;		// geodetic datum (see geodatatypes.h)
	ProjectionType	projection;	// map projection (see geodatatypes.h)
	double		projparm;	// Projection Parameter
	double		projint;	// Projection interval
	int		num_planes;	// number of State Plane Systems
	char		**state_plane_system;	// State Plane System Names
	double		*state_plane_intervals;	// State Plane Intervals
	double		skew_angle;	// Rotation skew angle from north
	double		text_angle;	// Text rotation angle
	char		*units;		// Depth Measurement Units
	char		*sound_datum;	// Sounding Datum
	double		x_meters;	// X pixel res in meters
	double		y_meters;	// Y pixel res in meters

	// MDF Record
	double		mag_north;	// Magnetic North
	int		year_estab;	// Year measured
	double		variance;	// Declination variance in seconds

	// CED Record
	int		edition;	// source edition
	int		raster_ed;	// raster edition
	char		*ed_date;	// edition date

	// VER record
	int		major;		// major version number
	int		minor;		// minor version number

	// OST Record
	int		image_lines;	// number of image lines between strip offsets

	// IFM Record	saved in color map info
	// RGB Records	saved in color map

	// REF Records
	int		ref_count;	// number of reference points
	int		*ref_x;		// list of reference point x values
	int		*ref_y;		// list of reference point y values
	double		*ref_lat;	// list of reference point latitude values
	double		*ref_long;	// list of reference point longitude values

	// CPH Record
	double		shift_value;	// Shift value when near central meridians

	// WPX Record
	int		wpx_term_count;	// number of polynomial terms
	double		*wpx_terms;	// polynomial terms

	// PWX Record
	int		pwx_term_count;	// number of polynomial terms
	double		*pwx_terms;	// polynomial terms

	// WPY Record
	int		wpy_term_count;	// number of polynomial terms
	double		*wpy_terms;	// polynomial terms

	// PWY Record
	int		pwy_term_count;	// number of polynomial terms
	double		*pwy_terms;	// polynomial terms

	// ERR Record	referenced paralle to REF records above
	double		*pixel_x_err;	// polynomial error in generated x pixel positions
	double		*pixel_y_err;	// polynomial error in generated y pixel positions
	double		*latitude_err;	// polynomial error in generated latitude positions
	double		*longitude_err;	// polynomial error in generated longitude positions

	// PLY Record
	int		vertices;	// Number of lat/long vertices to bounding polygon
	double		*v_latitude;	// latitude vlaues of vertices
	double		*v_longitude;	// longitude vlaues of vertices
	double		min_lat, max_lat, min_lng, max_lng;
	double		max_lat_ref_err, max_lng_ref_err;

	// DTM Record
	double		lat_shift;	// Latitude shift for different datums
	double		long_shift;	// Longitude shift for different datums

	void	init(void);

    public:

	virtual	int	data_class_type(void) { return(KAP_GEO_DATA); }	// base class type

	// coordinate space converters (virtuals)
	virtual	double	lltox(double latitude, double longitude);
	virtual	double	lltoy(double latitude, double longitude);
	virtual	void	lltoxy_checker(double latitude, double longitude, double &x, double&y, int check=0) {
			x = lltox(latitude, longitude);
			y = lltoy(latitude, longitude);
			if(check) { // see if answer is reasonable
				double tlat, tlng;
				xytoll(x, y, tlat, tlng);
				if(fabs(tlat-latitude) > 50.0*max_lat_err() ||
				   fabs(tlng-longitude) > 50.0*max_lng_err() ) { // bad
					x = -1.0;
					y = -1.0;
				}
			}
		}

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
	char	*get_kapname(void) { return(kapname); }
	void	set_kapname(char *name = NULL) {
			if(kapname)free(kapname);
			if(name) {
				kapname = strdup(name);
			} else {
				kapname = NULL;
			}
		}

	int	get_kapnumber(void) { return(kapnumber); }
	void	set_kapnumber(int num) { kapnumber = num; }

	int	get_draw_units(void) { return(draw_units); }
	void	set_draw_units(int num) { draw_units = num; }

	long	get_scale(void) { return(scale); }
	void	set_scale(long num) { scale = num; }

	DatumType	get_datum(void) { return(datum); }
	void		set_datum(DatumType d) { datum = d; }
	
	ProjectionType	get_projection(void) { return(projection); }
	void		set_projection(ProjectionType d) { projection = d; }
	
	double	get_projparm(void) { return(projparm); }
	void	set_projparm(double num) { projparm = num; }

	double	get_projint(void) { return(projint); }
	void	set_projint(double num) { projint = num; }

	int	get_num_state_planes(void) { return(num_planes); }
	char	*get_state_plane(int i) { 
			if(i >= 0 && i < num_planes) {
				return(state_plane_system[i]);
			} else {
				return(NULL);
			}
		}
	void	set_state_plane(char *pl, int i=0) {
			if(i >= 0 && i < num_planes) {
				if(state_plane_system[i])free(state_plane_system[i]);
				state_plane_system[i] = strdup(pl);
			} else {
				fprintf(stderr,"Attempt to set nonexistent state plane #%d\n", i);
			}
		}
			
	double	get_state_plane_interval(int i) { 
			if(i >= 0 && i < num_planes) {
				return(state_plane_intervals[i]);
			} else {
				return(0.0);
			}
		}
	void	set_state_plane_interval(double si, int i=0) {
			if(i >= 0 && i < num_planes) {
				state_plane_intervals[i] = si;
			} else {
				fprintf(stderr,"Attempt to set nonexistent state plane interval#%d\n", i);
			}
		}
			
	void	add_state_plane(char *pl, double si) {
			num_planes++;
			state_plane_system = (char **)realloc(state_plane_system, num_planes * sizeof(char *));
			state_plane_system[num_planes-1] = strdup(pl);
			state_plane_intervals = (double *)realloc(state_plane_intervals, num_planes * sizeof(double));
			state_plane_intervals[num_planes-1] = si;
		}
	void	clear_state_planes(void) {
			int	i;
			for(i=0; i<num_planes; i++) {
				if(state_plane_system[i])free(state_plane_system[i]);
			}
			if(state_plane_system)free(state_plane_system);
			state_plane_system = NULL;
			if(state_plane_intervals)free(state_plane_intervals);
			state_plane_intervals = NULL;
			num_planes = 0;
		}
			
	double	get_skew_angle(void) { return(skew_angle); }
	void	set_skew_angle(double num) { skew_angle = num; }

	double	get_text_angle(void) { return(text_angle); }
	void	set_text_angle(double num) { text_angle = num; }

	char	*get_units(void) { return(units); }
	void	set_units(char *name = NULL) {
			if(units)free(units);
			if(name) {
				units = strdup(name);
			} else {
				units = NULL;
			}
		}

	char	*get_sound_datum(void) { return(sound_datum); }
	void	set_sound_datum(char *name = NULL) {
			if(sound_datum)free(sound_datum);
			if(name) {
				sound_datum = strdup(name);
			} else {
				sound_datum = NULL;
			}
		}

	double	get_x_meters(void) { return(x_meters); }
	void	set_x_meters(double num) { x_meters = num; }

	double	get_y_meters(void) { return(y_meters); }
	void	set_y_meters(double num) { y_meters = num; }

	double	get_mag_north(void) { return(mag_north); }
	void	set_mag_north(double num) { mag_north = num; }

	int	get_year_estab(void) { return(year_estab); }
	void	set_year_estab(int num) { year_estab = num; }

	double	get_variance(void) { return(variance); }
	void	set_variance(double num) { variance = num; }

	int	get_edition(void) { return(edition); }
	void	set_edition(int num) { edition = num; }

	int	get_raster_ed(void) { return(raster_ed); }
	void	set_raster_ed(int num) { raster_ed = num; }

	char	*get_ed_date(void) { return(ed_date); }
	void	set_ed_date(char *name = NULL) {
			if(ed_date)free(ed_date);
			if(name) {
				ed_date = strdup(name);
			} else {
				ed_date = NULL;
			}
		}

	int	get_major(void) { return(major); }
	void	set_major(int num) { major = num; }

	int	get_minor(void) { return(minor); }
	void	set_minor(int num) { minor = num; }

	int	get_image_lines(void) { return(image_lines); }
	void	set_image_lines(int num) { image_lines = num; }

	int	get_num_ref_points(void) { return(ref_count); }
	void	delete_reference_point(int num) {
			ref_count--;
			for(int i=num; i<ref_count; i++) {
				ref_x[i] = ref_x[i+1];
				ref_y[i] = ref_y[i+1];
				ref_lat[i] = ref_lat[i+1];
				ref_long[i] = ref_long[i+1];
			}
		}
	void	add_reference_point(int x, int y, double lat, double lng) {
			ref_count++;
			ref_x = (int *)realloc(ref_x, ref_count*sizeof(int));
			ref_x[ref_count-1] = x;
			ref_y = (int *)realloc(ref_y, ref_count*sizeof(int));
			ref_y[ref_count-1] = y;
			ref_lat = (double *)realloc(ref_lat, ref_count*sizeof(double));
			ref_lat[ref_count-1] = lat;
			ref_long = (double *)realloc(ref_long, ref_count*sizeof(double));
			ref_long[ref_count-1] = lng;
		}
	void	clear_reference_points(void) {
			if(ref_x)free(ref_x);
			ref_x = NULL;
			if(ref_y)free(ref_y);
			ref_y = NULL;
			if(ref_lat)free(ref_lat);
			ref_lat = NULL;
			if(ref_long)free(ref_long);
			ref_long = NULL;
			if(pixel_x_err)free(pixel_x_err);
			pixel_x_err = NULL;
			if(pixel_y_err)free(pixel_y_err);
			pixel_y_err = NULL;
			if(latitude_err)free(latitude_err);
			latitude_err = NULL;
			if(longitude_err)free(longitude_err);
			longitude_err = NULL;
			ref_count = 0;
		}
	void	get_reference_point(int i, int *x, int *y, double *lat, double *lng) {
			if(i >= 0 && i < ref_count) {
				*x = ref_x[i];
				*y = ref_y[i];
				*lat = ref_lat[i];
				*lng = ref_long[i];
			} else {
				fprintf(stderr,"Whoops - Reference Point index %d out of range %d to %d\n", i, 0, ref_count);
			}
		}
	void	set_reference_point(int i, int x, int y, double lat, double lng) {
			if(i >= 0 && i < ref_count) {
				ref_x[i] = x;
				ref_y[i] = y;
				ref_lat[i] = lat;
				ref_long[i] = lng;
			} else {
				fprintf(stderr,"Whoops - Reference Point index %d out of range %d to %d\n", i, 0, ref_count);
			}
		}
			
	double	get_shift_value(void) { return(shift_value); }
	void	set_shift_value(double num) { shift_value = num; }
	int	gen_shift_value(void);
	int	force_shift_value(void);

	int	num_terms_from_code(int code) ;
	int	num_terms_to_code(int num) ;
	int	get_num_wpx_terms(void) { return(wpx_term_count); }
	double	get_wpx_term(int i) {
			if(i >= 0 && i < wpx_term_count) {
				return(wpx_terms[i]);
			} else {
				fprintf(stderr,"Whoops - Polynomial Term index %d out of range %d to %d\n", i, 0, wpx_term_count);
				return(0.0);
			}
		}
	void	set_wpx_term(int i, double term) {
			if(i >= 0 && i < wpx_term_count) {
				wpx_terms[i] = term;
			} else {
				fprintf(stderr,"Whoops - Polynomial Term index %d out of range %d to %d\n", i, 0, wpx_term_count);
			}
		}
	void	add_wpx_term(double term) {
			wpx_term_count++;
			wpx_terms = (double *)realloc(wpx_terms, wpx_term_count*sizeof(double));
			wpx_terms[wpx_term_count-1] = term;
		}
	void	clear_wpx_terms(void) {
			if(wpx_terms) free(wpx_terms);
			wpx_term_count = 0;
			wpx_terms = NULL;
		}
	int	gen_wpx_terms(int = 4);
		
	int	get_num_pwx_terms(void) { return(pwx_term_count); }
	double	get_pwx_term(int i) {
			if(i >= 0 && i < pwx_term_count) {
				return(pwx_terms[i]);
			} else {
				fprintf(stderr,"Whoops - Polynomial Term index %d out of range %d to %d\n", i, 0, pwx_term_count);
				return(0.0);
			}
		}
	void	set_pwx_term(int i, double term) {
			if(i >= 0 && i < pwx_term_count) {
				pwx_terms[i] = term;
			} else {
				fprintf(stderr,"Whoops - Polynomial Term index %d out of range %d to %d\n", i, 0, pwx_term_count);
			}
		}
	void	add_pwx_term(double term) {
			pwx_term_count++;
			pwx_terms = (double *)realloc(pwx_terms, pwx_term_count*sizeof(double));
			pwx_terms[pwx_term_count-1] = term;
		}
	void	clear_pwx_terms(void) {
			if(pwx_terms) free(pwx_terms);
			pwx_term_count = 0;
			pwx_terms = NULL;
		}
	int	gen_pwx_terms(int = 4);
		
	int	get_num_wpy_terms(void) { return(wpy_term_count); }
	double	get_wpy_term(int i) {
			if(i >= 0 && i < wpy_term_count) {
				return(wpy_terms[i]);
			} else {
				fprintf(stderr,"Whoops - Polynomial Term index %d out of range %d to %d\n", i, 0, wpy_term_count);
				return(0.0);
			}
		}
	void	set_wpy_term(int i, double term) {
			if(i >= 0 && i < wpy_term_count) {
				wpy_terms[i] = term;
			} else {
				fprintf(stderr,"Whoops - Polynomial Term index %d out of range %d to %d\n", i, 0, wpy_term_count);
			}
		}
	void	add_wpy_term(double term) {
			wpy_term_count++;
			wpy_terms = (double *)realloc(wpy_terms, wpy_term_count*sizeof(double));
			wpy_terms[wpy_term_count-1] = term;
		}
	void	clear_wpy_terms(void) {
			if(wpy_terms) free(wpy_terms);
			wpy_term_count = 0;
			wpy_terms = NULL;
		}
	int	gen_wpy_terms(int = 6);
		
	int	get_num_pwy_terms(void) { return(pwy_term_count); }
	double	get_pwy_term(int i) {
			if(i >= 0 && i < pwy_term_count) {
				return(pwy_terms[i]);
			} else {
				fprintf(stderr,"Whoops - Polynomial Term index %d out of range %d to %d\n", i, 0, pwy_term_count);
				return(0.0);
			}
		}
	void	set_pwy_term(int i, double term) {
			if(i >= 0 && i < pwy_term_count) {
				pwy_terms[i] = term;
			} else {
				fprintf(stderr,"Whoops - Polynomial Term index %d out of range %d to %d\n", i, 0, pwy_term_count);
			}
		}
	void	add_pwy_term(double term) {
			pwy_term_count++;
			pwy_terms = (double *)realloc(pwy_terms, pwy_term_count*sizeof(double));
			pwy_terms[pwy_term_count-1] = term;
		}
	void	clear_pwy_terms(void) {
			if(pwy_terms) free(pwy_terms);
			pwy_term_count = 0;
			pwy_terms = NULL;
		}
	int	gen_pwy_terms(int = 6);

	int	gen_map_terms(void) {
// same as gen_all_terms except doesn't regen shift_value
// can be used in cases where set of ref pts is known to not
// cross -180/180, yet is large (such as entire world)
			int	status;

			if(status = (gen_pwx_terms() && gen_pwy_terms() &&
			       	gen_wpx_terms() && gen_wpy_terms())) {
				status = gen_err_terms();
			}
			return(status);
		}
		
	int	gen_all_terms(void) {
			int	status;

			if(status = gen_shift_value()) {
				if(status = (gen_pwx_terms() && gen_pwy_terms() &&
					gen_wpx_terms() && gen_wpy_terms())) {
					status = gen_err_terms();
				}
			}
			return(status);
		}
		
	int	gen_err_terms(void);
	void	get_pixel_error(int i, double *x_err, double *y_err) {
			if(i >= 0 && i < ref_count) {
				*x_err = pixel_x_err[i];
				*y_err = pixel_y_err[i];
			} else {
				fprintf(stderr,"Whoops - Pixel Error index %d out of range %d to %d\n", i, 0, ref_count);
			}
		}
	void	set_pixel_error(int i, double x_err, double y_err) {
			if(ref_count && !pixel_x_err) pixel_x_err = (double *)malloc(ref_count * sizeof(double));
			if(ref_count && !pixel_y_err) pixel_y_err = (double *)malloc(ref_count * sizeof(double));
			if(i >= 0 && i < ref_count) {
				pixel_x_err[i] = x_err;
				pixel_y_err[i] = y_err;
			} else {
				fprintf(stderr,"Whoops - Pixel Error index %d out of range %d to %d\n", i, 0, ref_count);
			}
		}

	void	get_latlong_error(int i, double *lat_err, double *long_err) {
			if(i >= 0 && i < ref_count) {
				*lat_err = latitude_err[i];
				*long_err = longitude_err[i];
			} else {
				fprintf(stderr,"Whoops - Pixel Error index %d out of range %d to %d\n", i, 0, ref_count);
			}
		}
	void	set_latlong_error(int i, double lat_err, double long_err) {
			if(ref_count && !latitude_err) latitude_err = (double *)malloc(ref_count * sizeof(double));
			if(ref_count && !longitude_err) longitude_err = (double *)malloc(ref_count * sizeof(double));
			if(i >= 0 && i < ref_count) {
				latitude_err[i] = lat_err;
				longitude_err[i] = long_err;
			} else {
				fprintf(stderr,"Whoops - Pixel Error index %d out of range %d to %d\n", i, 0, ref_count);
			}
		}
	double  max_lat_err(void) {
			double local=0.0;
			for(int i=0; i<ref_count; i++) {
				if(latitude_err[i] > local) local = latitude_err[i];
			}
			return(local);
		}
	double  max_lng_err(void) {
			double local=0.0;
			for(int i=0; i<ref_count; i++) {
				if(longitude_err[i] > local) local = longitude_err[i];
			}
			return(local);
		}
			
	int	gen_poly_verts(void);
	int	get_num_poly_vertices(void) { return(vertices); }
	void	get_poly_vert(int i, double *lat, double *lng) {
			if(i >= 0 && i < vertices) {
				*lat = v_latitude[i];
				*lng = v_longitude[i];
			} else {
				fprintf(stderr,"Whoops - Poly Vertex index %d out of range %d to %d\n", i, 0, vertices);
			}
		}
	void	add_poly_vert(double lat, double lng) {
			vertices++;
			v_latitude = (double *)realloc(v_latitude, vertices*sizeof(double));
			v_longitude = (double *)realloc(v_longitude, vertices*sizeof(double));
			v_latitude[vertices-1] = lat;
			v_longitude[vertices-1] = lng;
			min_max_lat_lng(lat, lng);
		}
	void	set_poly_vert(int i, double lat, double lng) {
			if(i >= 0 && i < vertices) {
				v_latitude[i] = lat;
				v_longitude[i] = lng;
			} else {
				fprintf(stderr,"Whoops - Poly Vertex index %d out of range %d to %d\n", i, 0, vertices);
			}
			min_max_lat_lng(lat, lng);
		}
	void	clear_poly_verts(void) {
			if(v_latitude) free(v_latitude);
			v_latitude = NULL;
			if(v_longitude) free(v_longitude);
			v_longitude = NULL;
			vertices = 0;
			clear_min_max();
		}
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
			
	double	get_lat_shift(void) { return(lat_shift); }
	void	set_lat_shift(double num) { lat_shift = num; }

	double	get_long_shift(void) { return(long_shift); }
	void	set_long_shift(double num) { long_shift = num; }

	// functions to dump the geodata contents to a string or parse out of a string
	char	*to_string(void);
	int	from_string(char *);
		

	// Constructors:
	KAPGeoData()  { init(); }

	~KAPGeoData(); 
	};

#endif // _KAP_GEODATA_H_
