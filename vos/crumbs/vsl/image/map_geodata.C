// map_geodata.h
//
// Written by John Wright 04/01/97

#include "image/types/map_geodata.h"

#ifndef TRUE
#define TRUE	1
#define FALSE	0
#endif


void	MAPGeoData::init(void)
{
	clear_min_max();
	ll_image = NULL;
	xy_image = NULL;
}

MAPGeoData::~MAPGeoData(void)
{
	delete_xy_image();
	delete_ll_image();
}

// create ll to xy conversion image from xy to ll image
void	MAPGeoData::create_xy_image(void)
{
	int	width, height, i, j, k, l;
	double	lat_floor, lat_ceil, lng_floor, lng_ceil;
	double	x1, y1, x2, y2, xval, yval;
	double	lng_val, lat_val, lat1, lat2;
	double	t_val1, t_val2, fraction, add_val;

	if(!ll_image) return;

	// create new empty image for the ll to xy conversion data
	delete_xy_image();
	xy_image = new Image(new floatData(361, 181, 2));
	//xy_image->get_data()->clear();
	for(i=0; i<181; i++) {
		for(j=0; j<361; j++) {	
					xy_image->get_data()->set_double(-1000000.0,j,i,X_BAND);
					xy_image->get_data()->set_double(-1000000.0,j,i,Y_BAND);
		}
	}
	// image has 2 bands, band 0 = x values, band 1 = y values
	// for ll image, band 0 = lat values, band 1 = long values

	// loop through all pixels in the xy to ll image
	ll_image->get_res(&width, &height);
	for(i=0; i<height-1; i++) {
//fprintf(stderr,"Processing row %d\n", i);
		for(j=0; j<width-1; j++) {	
			// find the upper and lower bounds of this cell
			lat_floor = 90.0; lat_ceil = -90.0;
			lng_floor = 360.0; lng_ceil = -360.0;
			for(k=i; k<=i+1; k++) {
				for(l=j; l<=j+1; l++) {
					lat_val = xytolat(l,k);
					if(floor(lat_val) < lat_floor && lat_val >= -90.0 && lat_val <= 90.0) lat_floor = floor(lat_val);
					if(ceil(lat_val) > lat_ceil && lat_val >= -90.0 && lat_val <= 90.0) lat_ceil = ceil(lat_val);
					lng_val = xytolong(l,k);
					if(floor(lng_val) < lng_floor && lng_val >= -180.0 && lng_val <= 180.0) lng_floor = floor(lng_val);
					if(ceil(lng_val) > lng_ceil && lng_val >= -180.0 && lng_val <= 180.0) lng_ceil = ceil(lng_val);
				}
			}

			// process all integral lat/long intersections within this cell
			if(lat_ceil - lat_floor > 1.1) {
			if(lng_ceil-lng_floor > 180.0) {	// must cross +/-180
				t_val1 = lng_ceil;
				lng_ceil = lng_floor + 360.0 + 1.0;
				lng_floor = t_val1-1.0;
				add_val = 360.0;
//fprintf(stderr,"Cell %d,%d\n",j,i);
//fprintf(stderr,"(%f,%f) (%f,%f)\n",xytolat(j,i),xytolong(j,i),xytolat(j+1,i),xytolong(j+1,i));
//fprintf(stderr,"(%f,%f) (%f,%f)\n",xytolat(j,i+1),xytolong(j,i+1),xytolat(j+1,i+1),xytolong(j+1,i+1));
//fprintf(stderr,"lat_floor,lat_ceil=%f,%f  lng_floor,lng_ceil=%f,%f\n",lat_floor,lat_ceil,lng_floor,lng_ceil);
			} else {
				add_val = 0.0;
			}
			for(lng_val=lng_floor+1.0; lng_val<=lng_ceil-0.9; lng_val += 1.0) {
//fprintf(stderr,"Doing some intersections in cell %d,%d\n",j,i);
//fprintf(stderr,"Long floor = %f ceil=%f\n", lng_floor, lng_ceil);
				// find 2 edge intersections (ignore unlikely chance of more than 2)
				x1 = -1.0; y1 = -1.0; x2 = -1.0; y2 = -1.0;
				// check top and bottom edges
				for(k=i; k<=i+1; k++) {
					t_val1 = xytolong(j, k);
					t_val2 = xytolong(j+1, k);
					if(t_val1 < 0.0) t_val1 += add_val;
					if(t_val2 < 0.0) t_val2 += add_val;
					if(((t_val1 <= lng_val && t_val2 >= lng_val) ||
					    (t_val1 >= lng_val && t_val2 <= lng_val)) &&
					   t_val1 != t_val2) {
						// compute interpolated/intersection point
						x1 = x2; y1 = y2;
						fraction = (lng_val-t_val1)/(t_val2-t_val1);
						x2 = (double)j + fraction;
						y2 = (double)k;
						// compute latitude at this point
						t_val1 = xytolat(j,k);
						t_val2 = xytolat(j+1, k);
						lat1 = lat2;
						lat2 = t_val1 + fraction * (t_val2-t_val1);
					}
				}
				// check left and right edges
				for(k=j; k<=j+1; k++) {
					t_val1 = xytolong(k, i);
					t_val2 = xytolong(k, i+1);
					if(t_val1 < 0.0) t_val1 += add_val;
					if(t_val2 < 0.0) t_val2 += add_val;
					if(((t_val1 <= lng_val && t_val2 >= lng_val) ||
					    (t_val1 >= lng_val && t_val2 <= lng_val)) &&
					   t_val1 != t_val2) {
						// compute interpolated/intersection point
						x1 = x2; y1 = y2;
						x2 = (double)k;
						fraction = (lng_val-t_val1)/(t_val2-t_val1);
						y2 = (double)i + fraction;
						// compute latitude at this point
						t_val1 = xytolat(k, i);
						t_val2 = xytolat(k, i+1);
						lat1 = lat2;
						lat2 = t_val1 + fraction * (t_val2-t_val1);
					}
				}
				if(x1 < 0.0 || y1 < 0.0) break;  // didn't find at least 2 edge intersections

				// now check integer latitude values for intersection with this longitude
				for(lat_val=lat_floor+1; lat_val <= lat_ceil-1; lat_val += 1.0) {
					if((lat1 >= lat_val && lat2 <= lat_val) ||
					   (lat1 <= lat_val && lat2 >= lat_val)) {
						// found an intersection - compute position
						fraction = (lat_val - lat1) / (lat2 - lat1);
						xval = x1 + fraction*(x2-x1);
						yval = y1 + fraction*(y2-y1);
						// insert it into the conversion image
						k = (int)floor(lng_val+0.5) + 180;
						if(k > 360) k -= 360;
						l = (int)floor(lat_val+0.5) + 90;
						xy_image->get_data()->set_double(xval, k, l, X_BAND);
						xy_image->get_data()->set_double(yval, k, l, Y_BAND);
					}
				}
			}
			}
		}
	}
//xy_image->write("XYpre.pds",11);
	int	count, total=0;
	double	t_val3, t_val4;
	for(i=0; i<181; i++) {
		t_val1 = xy_image->get_data()->get_double(360,i,X_BAND);
		xy_image->get_data()->set_double(t_val1,0,i,X_BAND);
		t_val1 = xy_image->get_data()->get_double(360,i,Y_BAND);
		xy_image->get_data()->set_double(t_val1,0,i,Y_BAND);
		for(j=0; j<361; j++) {	
			t_val1 = xy_image->get_data()->get_double(j,i,X_BAND);
			t_val2 = xy_image->get_data()->get_double(j,i,Y_BAND);
			if(t_val1 < 0.0 && t_val2 < 0.0) {
				count = 0; t_val1 = 0.0; t_val2 = 0.0;
				for(k=i-1; k<=i+1; k++) {
					for(l=j-1; l<=j+1; l++) {
						t_val3 = xy_image->get_data()->get_double(l,k,X_BAND);
						t_val4 = xy_image->get_data()->get_double(l,k,Y_BAND);
						if(t_val3 > 0.0 && t_val4 > 0.0) {
							t_val1 += t_val3;
							t_val2 += t_val4;
							count++;
						}
					}
				}
				if(count >= 6) {
					total++;
					xy_image->get_data()->set_double(t_val1/count,j,i,X_BAND);
					xy_image->get_data()->set_double(t_val2/count,j,i,Y_BAND);
				}
			}
		}
	}
//xy_image->write("XY.pds",11);
//fprintf(stderr,"A total of %d pixels were filled in.\n", total);
}

// create xy to ll conversion image from ll to xy image
void	MAPGeoData::create_ll_image(void)
{
}

	// coordinate space converters (instantiation of virtuals)
double MAPGeoData::lltox(double lat, double lng)
{
	double	temp = 0.0;

//	if(!in_image(lat, lng)) return(-1.0);
	// shift lat/long to -180 to +180 and -90 to +90
	lng += 180.0;
	//if(lng < 0.0) lng -= (int)(lng/360.0 + 1.0) * 360.0;
	//if(lng > 360.0) lng -= (int)(lng/360.0) * 360.0;
	lat += 90.0;
	if(xy_image && xy_image->get_data()) temp = xy_image->get_data()->iget_double(lng, lat, X_BAND);
	if(temp < 0.0) temp = -1.0;
	return(temp);
}
double MAPGeoData::lltoy(double lat, double lng)
{
	double	temp = 0.0;

//	if(!in_image(lat, lng)) return(-1.0);
	// shift lat/long to -180 to +180 and -90 to +90
	lng += 180.0;
	//if(lng < 0.0) lng -= (int)(lng/360.0 + 1.0) * 360.0;
	//if(lng > 360.0) lng -= (int)(lng/360.0) * 360.0;
	lat += 90.0;
	if(xy_image && xy_image->get_data()) temp = xy_image->get_data()->iget_double(lng, lat, Y_BAND);
	if(temp < 0.0) temp = -1.0;
	return(temp);
}

double MAPGeoData::utmtox(double /* easting */, double /* northing */)
{
	return(0.0);
}
double MAPGeoData::utmtoy(double /* easting */, double /* northing */)
{
	return(0.0);
}

double MAPGeoData::utmtolat(double /* easting */, double /* northing */)
{
	return(0.0);
}
double MAPGeoData::utmtolong(double /* easting */, double /* northing */)
{
	return(0.0);
}

double MAPGeoData::lltoe(double /* latitude */, double /* longitude */)
{
	return(0.0);
}
double MAPGeoData::llton(double /* latitude */, double /* longitude */)
{
	return(0.0);
}

double MAPGeoData::xytolat(double x, double y)
{
	double	temp = 0.0;

	if(ll_image && ll_image->get_data()) temp = ll_image->get_data()->iget_double(x, y, LAT_BAND);
	
	return(temp);
}
double MAPGeoData::xytolong(double x, double y)
{
	double	temp = 0.0;

	if(ll_image && ll_image->get_data()) temp = ll_image->get_data()->iget_double(x, y, LNG_BAND);
	
	return(temp);
}

double MAPGeoData::xytoe(double /* x */, double /* y */)
{
	return(0.0);
}
double MAPGeoData::xyton(double /* x */, double /* y */)
{
	return(0.0);
}


char	*MAPGeoData::to_string(void)
{
	char	buff[218192];	// this should change to handle any size through realloc but I can't think of a good way
/* Not yet implemented so commented out to avoid compile warnings
	char	lbuf[1024];
	int	i;
	int	temp_int, temp_int2;
	double	temp_double, temp_double2;
	MAPGeoData	*geodat;

	geodat = this;
*/

		buff[0] = '\0';
		// output magic string to define data type
		strcat(buff,"MAPGeoData\n");

	return(strdup(buff));
}

int	MAPGeoData::from_string(char *buf)
{
/* Not yet implemented so commented out to avoid compile warnings
	MAPGeoData	*geodat;
	char *temp_str = NULL;
	int	i;
	int	temp_int, temp_int2, temp_int3;
	double	temp_double, temp_double2;
	char	temp_string[1024];

	geodat = this;
*/
		// check magic string in buffer
		if(strncmp(buf, "MAPGeoData", strlen("MAPGeoData"))) {
			return(FALSE);
		}
		

	return(TRUE);
}

