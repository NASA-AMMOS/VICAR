#ifndef _CAHVOR_H_
#define _CAHVOR_H_
// cahvor.h 1.4 03/05/01 07:51:05
/** \file
 ** C++ wrapper around Todd Litwin's CAHVOR camera model code
 **/
extern "C" {
//#include "cmod.h"
#include "cmod_cahv.h"
#include "cmod_cahvor.h"
#include "cmod_cahvore.h"
}
#include "grape/vector_ops.h"

class CAHVOR {
public:
	double c[3];			///< basic components
	double a[3];
	double h[3];
	double v[3];
	double o[3];
	double r[3];
	
	double s[18][18], si[5][5];	///< extra stuff
	double hs, hc, vs, vc, theta;
	
	/// read from file, return 0 if okay
	int read(char *name) {
		// handle CAHV model, promote to CAHVOR
		if (name[strlen(name)-1] == 'v') {
			if (cmod_cahv_read(name, c, a, h, v, (double (*)[12])s,
					&hs, &hc, &vs, &vc, &theta, si))
				return -1;
			vector_copy(a, o);
			//memset(r, 0, sizeof(r));
            r[0]=r[1]=r[2]=0.0;
			return 0;
		} 
		return cmod_cahvor_read(name, c, a, h, v, o, r, s,
			&hs, &hc, &vs, &vc, &theta, si);
	}
	
	/// write to file, return 0 if okay
	int write(char *name, char *comment = (char *)"") {
		return cmod_cahvor_write(name, comment,
				c, a, h, v, o, r, s,
				hs, hc, vs, vc, theta, si);
	}

	/// project 3D point to image plane
	void To_2D(double p3d[3], double p2d[2], int approx=0) {
		double range;
		cmod_cahvor_3d_to_2d(p3d, c, a, h, v, o, r, approx,
			&range, p2d, NULL);
	}
	
	/// transform camera model
	void move(double pi[3], double qi[4], double po[3], double qo[4],
			CAHVOR *ccmod) {
		*ccmod = *this;	// start with a copy
		cmod_cahvor_move(pi, qi, c, a, h, v, o, r, 
				po, qo,	ccmod->c, ccmod->a, ccmod->h, 
				ccmod->v, ccmod->o, ccmod->r);
	}

	/// convert to/from 6*3 array of doubles. 
	// Assumes c a h v o r stuff is all adjacent
	double *get()		{ return c; }
	void set(double *array[6*3])	{ 
		memcpy(c, array, 6*sizeof(c));
		memset(s, 0, sizeof(s) + sizeof(si));
	}

	// get horizontal field of view in degrees
	double hfov(int xres, int yres)		{
        /*
                // the old way, don't use!
		double x = dot_product(a, h) / 
		  		(vector_magnitude(a) * vector_magnitude(h));
		return 180.0 - 2*180/3.14159 * acos(x);
        */

            double ls[2];
            double pos1[3], uvec1[3], pos2[3], uvec2[3];
            
            ls[0] = 1;
            ls[1] = (int)(yres/2);
            
            cmod_cahvor_2d_to_3d(ls, c, a, h, v, o, r, FALSE, pos1, uvec1, NULL);
            //printf("pos1 = %f %f %f\n", pos1[0], pos1[1], pos1[2]);
            //printf("uvec1 = %f %f %f\n", uvec1[0], uvec1[1], uvec1[2]);
            
            ls[0] = xres;
            //ls[1] = (int)(yres/2);

            cmod_cahvor_2d_to_3d(ls, c, a, h, v, o, r, FALSE, pos2, uvec2, NULL);
            //printf("pos2 = %f %f %f\n", pos2[0], pos2[1], pos2[2]);
            //printf("uvec2 = %f %f %f\n", uvec2[0], uvec2[1], uvec2[2]);
             
            // both vectors are unit vectors, no need to divide by magnitude
            double theta = 180/3.14159 * acos(dot_product(uvec1, uvec2));
            printf("hfov = %f degreees\n", theta);
            return theta;

	}
	
	/// constructors
	CAHVOR() 		{ }
	CAHVOR(char *name) 	{ read(name); }
};

int summitt_cmod_to_pointing(CAHVOR *cmod, double rot[3], CAHVOR *ccmod);
void summitt_pointing_xform(double prot[3], double ptrans[3],
	double rts_orient[4], double rts_trans[3]);

#endif
