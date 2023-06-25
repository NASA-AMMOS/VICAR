// camera_geodata.C
//
//  Written by John Wright 02/18/2003

#include "image/types/camera_geodata.h"

#ifndef TRUE
#define TRUE    1
#define FALSE   0
#endif

#ifndef DEG_TO_RAD
#define DEG_TO_RAD      (3.14159265358979/180.0)
#endif

CameraModelGeoData::CameraModelGeoData() {
	cameraModel = NULL;
}

CameraModelGeoData::~CameraModelGeoData() {
	if(cameraModel) delete(cameraModel);
}

double	CameraModelGeoData::lltox(double latitude, double longitude) {
	double x, y;
	lltoxy(latitude, longitude, x, y);
	return(x);
}

double	CameraModelGeoData::lltoy(double latitude, double longitude) {
	double x, y;
	lltoxy(latitude, longitude, x, y);
	return(y);
}

double  CameraModelGeoData::xytolat(double x, double y) {
	double lat, lng;
	xytoll( x, y, lat, lng);
	return(lat);
}

double  CameraModelGeoData::xytolong(double x, double y) {
	double lat, lng;
	xytoll( x, y, lat, lng);
	return(lng);
}

void	CameraModelGeoData::lltoxy(double latitude, double longitude, double &x, double&y) {
	double az, el, tx=0.0, ty=0.0;
	double rayx, rayy, rayz;
	if(cameraModel) {
		az = longitude * DEG_TO_RAD;
		el = latitude * DEG_TO_RAD;
		rayz = -sin(el) + cameraModel->getCameraZ();
		rayx = cos(az)*cos(el) + cameraModel->getCameraX();
		rayy = sin(az)*cos(el) + cameraModel->getCameraY();
		cameraModel->Point3DToImage2D(rayx, rayy, rayz, &ty, &tx);
	}
	x = tx;
	y = ty;
}

void    CameraModelGeoData::xytoll(double x, double y, double &latitude, double &longitude) {
	double az=0.0, el=0.0;
	if(cameraModel) cameraModel->Image2DToAzEl(y, x, &az, &el);
	longitude = az / DEG_TO_RAD;
	latitude = el / DEG_TO_RAD;
}
