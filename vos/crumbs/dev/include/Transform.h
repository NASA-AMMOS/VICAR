 /*                                      3/30/92         tr_form.c
 *
 * Utility routines for working with transformations (tr_form) in
 * a 3D space.  A world transform can be combined with an object
 * transform to give a single transform.  A 3d vector can be
 * transformed to a 3D vector which can in turn be projected to
 * a 2D viewing plane.
 */

#ifndef _TRANSFORM_
#define _TRANSFORM_
#include <math.h>

typedef         double tr_data_t;

const double PII = 3.141592653589793238462643;

class Transform {
private:
	tr_data_t form[3][4];
public:
	Transform();	// creates transform matrix with no rotations or translations
	Transform(const tr_data_t _form[3][4]) { form_put(_form); }

	void form_put( const tr_data_t inform[3][4]);
	void form_get( tr_data_t inform[][4]);
	void rotate_x(double x);
	void rotate_y(double y);
	void rotate_z(double z);
	void translate(double x, double y, double z);

	void to_local(double xin, double yin, double zin, double *xout, 
				double *yout, double *zout);
	void to_world(double xin, double yin, double zin, double *xout, 
				double *yout, double *zout);

	double to_degrees(double radians) { return radians*180.0/PII; }
	double to_radians(double degrees) { return degrees*PII/180.0; }

	void print_tf();
};

#endif
