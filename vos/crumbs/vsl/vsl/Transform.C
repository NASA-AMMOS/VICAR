#include <iostream>
#include "Transform.h"

using namespace std;
//
// gram-schmitt routine to insure the transformation matrix remains
// orthogonal

void gram_schmitt(tr_data_t transform[][4])
{

        int neg_1 = 0, neg_2 = 0, neg_3 = 0;

        double x_comp1 = transform[0][0];
        double y_comp1 = transform[1][0];
        double z_comp1 = transform[2][0];

        if (x_comp1 < 0 || y_comp1 < 0 || z_comp1 < 0) neg_1 = 1;

        double tmpmag = (x_comp1*x_comp1 + y_comp1*y_comp1 + z_comp1*z_comp1);
	double magnitude = sqrt(tmpmag);

        x_comp1 /= magnitude;
        y_comp1 /= magnitude;
        z_comp1 /= magnitude;

        transform[0][0] = x_comp1;
        transform[1][0] = y_comp1;
        transform[2][0] = z_comp1;

        double x_comp2 = transform[0][1];
        double y_comp2 = transform[1][1];
        double z_comp2 = transform[2][1];

        if (x_comp2 < 0 || y_comp2 < 0 || z_comp2 < 0) neg_2 = 1;

        double t = x_comp2*x_comp1 + y_comp2*y_comp1 + z_comp2*z_comp1;
        double tx = t * x_comp1;
        double ty = t * y_comp1;
        double tz = t * z_comp1;

        x_comp2 -= tx;
        y_comp2 -= ty;
        z_comp2 -= tz;

        transform[0][1] = x_comp2;
        transform[1][1] = y_comp2;
        transform[2][1] = z_comp2;

        if (transform[0][2] < 0 || transform[1][2] < 0 || 
				transform[2][2] < 0) neg_3 = 1;

        double x_comp3 = y_comp1*z_comp2 - z_comp1*y_comp2;
        double y_comp3 = z_comp1*x_comp2 - x_comp1*z_comp2;
        double z_comp3 = x_comp1*y_comp2 - y_comp1*x_comp2;

        if (neg_1 || neg_2 || neg_3) {
                double x = transform[0][2];
                double y = transform[1][2];
                double z = transform[2][2];

                double tt = x*x_comp3 + y*y_comp3 + z*z_comp3;
                if (tt < 0) {
                        x_comp3 = -x_comp3;
                        y_comp3 = -y_comp3;
                        z_comp3 = -z_comp3;
                }
        }
        transform[0][2] = x_comp3;
        transform[1][2] = y_comp3;
        transform[2][2] = z_comp3;
}


Transform::Transform()
{
	for (int i = 0; i < 3; i++) {
	   for (int j = 0; j < 4; j++) {
	       if (i == j) form[i][j] = 1.0;
	       else form[i][j] = 0.0;
	   }
	}
}


// given (x,y,z) in local(oriented) system, returns vector components
// (xout, yout, zout) relative to world(non-oriented) system
// i.e. let V=vector w=world l=local l-w=local relative to world. Then
// Vw = Vl + Vl-w; this function returns Vl 

void Transform::to_local(double xin, double yin, double zin, double *xout, 
					double *yout, double *zout)
{
	*xout = form[0][0] * xin + form[0][1] * yin + form[0][2] * zin;
	*yout = form[1][0] * xin + form[1][1] * yin + form[1][2] * zin;
	*zout = form[2][0] * xin + form[2][1] * yin + form[2][2] * zin;

	return;
}

// given (x,y,z) in local(oriented) system, returns vector
// (xout, yout, zout) in world(non-oriented) system
// i.e. let V=vector w=world l=local l-w=local relative to world. Then
// Vw = Vl + Vl-w; this function returns Vw 
void Transform::to_world(double xin, double yin, double zin, double *xout, 
					double *yout, double *zout)
{
	*xout = form[0][0] * xin + form[0][1] * yin + form[0][2] * zin + 
								form[0][3];
	*yout = form[1][0] * xin + form[1][1] * yin + form[1][2] * zin + 
								form[1][3];
	*zout = form[2][0] * xin + form[2][1] * yin + form[2][2] * zin + 
								form[2][3];

	return;
}

void Transform::form_get( tr_data_t outform[][4] )
{
   outform[0][0] = form[0][0];
   outform[0][1] = form[0][1];
   outform[0][2] = form[0][2];
   outform[0][3] = form[0][3];
   outform[1][0] = form[1][0];
   outform[1][1] = form[1][1];
   outform[1][2] = form[1][2];
   outform[1][3] = form[1][3];
   outform[2][0] = form[2][0];
   outform[2][1] = form[2][1];
   outform[2][2] = form[2][2];
   outform[2][3] = form[2][3];

   return;
}

void Transform::form_put( const tr_data_t inform[3][4] )
{
   form[0][0] = inform[0][0];
   form[0][1] = inform[0][1];
   form[0][2] = inform[0][2];
   form[0][3] = inform[0][3];
   form[1][0] = inform[1][0];
   form[1][1] = inform[1][1];
   form[1][2] = inform[1][2];
   form[1][3] = inform[1][3];
   form[2][0] = inform[2][0];
   form[2][1] = inform[2][1];
   form[2][2] = inform[2][2];
   form[2][3] = inform[2][3];

   return;
}

void Transform::rotate_x(double x )
{
   double cos_temp, sin_temp, temp01, temp11, temp21;
   double temp02, temp12, temp22;

   cos_temp = cos( x );
   sin_temp = sin( x );;
   temp01 = cos_temp * form[0][1] + sin_temp * form[0][2];
   temp11 = cos_temp * form[1][1] + sin_temp * form[1][2];
   temp21 = cos_temp * form[2][1] + sin_temp * form[2][2];
   temp02 = -sin_temp * form[0][1] + cos_temp * form[0][2];
   temp12 = -sin_temp * form[1][1] + cos_temp * form[1][2];
   temp22 = -sin_temp * form[2][1] + cos_temp * form[2][2];
   form[0][1] = temp01;
   form[1][1] = temp11;
   form[2][1] = temp21;
   form[0][2] = temp02;
   form[1][2] = temp12;
   form[2][2] = temp22;
   
   gram_schmitt(form);
   
   return;
}

void Transform::rotate_y(double y)
{
   double cos_temp, sin_temp, temp00, temp10, temp20;
   double temp02, temp12, temp22;

   cos_temp = cos( y );
   sin_temp = sin( y );
   temp00 = cos_temp * form[0][0] - sin_temp * form[0][2];
   temp10 = cos_temp * form[1][0] - sin_temp * form[1][2];
   temp20 = cos_temp * form[2][0] - sin_temp * form[2][2];
   temp02 =  + sin_temp * form[0][0] + cos_temp * form[0][2];
   temp12 =  + sin_temp * form[1][0] + cos_temp * form[1][2];
   temp22 =  + sin_temp * form[2][0] + cos_temp * form[2][2];
   form[0][0] = temp00;
   form[1][0] = temp10;
   form[2][0] = temp20;
   form[0][2] = temp02;
   form[1][2] = temp12;
   form[2][2] = temp22;

   gram_schmitt(form);
   return;
}

void Transform::rotate_z(double z)
{
   double cos_temp, sin_temp, temp00, temp10, temp20;
   double temp01, temp11, temp21;

   cos_temp = cos( z );
   sin_temp = sin( z );

   temp00 = cos_temp * form[0][0] + sin_temp * form[0][1];
   temp10 = cos_temp * form[1][0] + sin_temp * form[1][1];
   temp20 = cos_temp * form[2][0] + sin_temp * form[2][1];

   temp01 = -sin_temp * form[0][0] + cos_temp * form[0][1];
   temp11 = -sin_temp * form[1][0] + cos_temp * form[1][1];
   temp21 = -sin_temp * form[2][0] + cos_temp * form[2][1];

   form[0][0] = temp00;
   form[1][0] = temp10;
   form[2][0] = temp20;
   form[0][1] = temp01;
   form[1][1] = temp11;
   form[2][1] = temp21;

   gram_schmitt(form);
   return;
}

void Transform::translate(double x, double y, double z )
{

   if( x != 0 ) {
      form[0][3] += x;
   }
   if( y != 0 ) {
      form[1][3] += y;
   }
   if( z != 0 ) {
      form[2][3] += z;
   }

   return;
}

void Transform::print_tf() {
             cout << form[0][0] << " " << form[0][1] << " " << form[0][2]
                << endl;
             cout << form[1][0] << " " << form[1][1] << " " << form[1][2]
                << endl;
             cout << form[2][0] << " " << form[2][1] << " " << form[2][2]
                << endl;
        }

