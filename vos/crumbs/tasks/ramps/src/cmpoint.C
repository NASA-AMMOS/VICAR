// cmpoint.C 1.5 03/05/01 07:51:05
/** \file
// Extract SUMMITT camera pointing data from CAHV* camera model,
// and transform camera model to camera frame.
// Method from Marty Vona.
*/
#include <stdio.h>
#include <string.h>
#include "grape/aej.h"
#include "grape/aejMatrix.h"
#include "grape/aejTransform.h"
#include "grape/vector_ops.h"
#include "grape/matrix.h"
#include "cahvor.h"

#ifndef PI
#define PI 3.141592654
#endif

/**
// Convert CAHVOR components of camera model into
// SUMMITT camera pointing angles in degrees
// (rot[X]=roll, rot[Y]=pitch, rot[Z]=roll), and optionally
// (ccmod != NUL) rotate the camera model to the new frame.
// Note that C corresponds to SUMMITT camera translation.
// Returns zero if no problems found.
**/
int summitt_cmod_to_pointing(CAHVOR *cmod, double crot[3], CAHVOR *ccmod)
{
	double an[3], hn[3], t[3];

	// normalize A, make sure it's not zero -> new X (forward)
	double amag = vector_magnitude(cmod->a);
	if (amag == 0.0) {
		fprintf(stderr, "Bad camera model, A length is zero\n");
		return 1;
	}
	vector_scale(cmod->a, 1.0/amag, an);

	// project H onto image plane -> new Y (left)
	double ahdot = dot_product(an, cmod->h);
	vector_scale(an, ahdot, t);
	vector_diff(t, cmod->h, hn);

	// normalize
	normalized_vector(hn, hn);

	// orthogonal third vector -> new Z (up)
	cross_product(an, hn, t);

	// rotation matrix cam->world is (an, hn, t), we want transpose
	aejMatrix rot(3, 3);
	rot(0,0) = an[0];
	rot(1,0) = an[1];
	rot(2,0) = an[2];
	rot(0,1) = hn[0];
	rot(1,1) = hn[1];
	rot(2,1) = hn[2];
	rot(0,2) = t[0];
	rot(1,2) = t[1];
	rot(2,2) = t[2];

	// extract rotation angles from matrix, and convert to degrees
	aejVector rv = FixedAnglesFromRotation(rot);
	crot[0] = rv[0]*180.0/PI;
	crot[1] = rv[1]*180.0/PI;
	crot[2] = rv[2]*180.0/PI;

	// don't care about camera-frame model?
	if (ccmod == NULL)
		return 0;

	// convert matrix to quaternion for rotating camera model
	rot = rot.transpose();
	aejVector q = QuaternionFromRotation(rot);

	// revise camera model 
	double z[3]  = { 0.0, 0.0, 0.0 };
	double qi[4] = { 1.0, 0.0, 0.0, 0.0 };
	double qr[4] = { q[0], q[1], q[2], q[3] };
	cmod->move(cmod->c, qi, z, qr, ccmod);

	return 0;
}

/**
// Append a rover-to-site transform, given as quaternion and translation,
// to SUMMITT pointing data.
**/
void summitt_pointing_xform(double prot[3], double ptrans[3],
	double rts_orient[4], double rts_trans[3])
{
	int i;

	// convert pointing to aejTransform
	aejTransform ptg;
	ptg.setTrans(aejVector(ptrans[0], ptrans[1], ptrans[2]));
	ZMatrix r;
	MakeRotationMatrix(r, prot[0], prot[1], prot[2]);
	aejMatrix rot(3,3);
	for (i=0; i<3; i++)
		rot.setRow(i, aejVector(r[i][0], r[i][1], r[i][2]));
	ptg.setRot(rot);

	// convert rover-to-site to transform
	aejTransform rts;
	rts.setTrans(aejVector(rts_trans[0], rts_trans[1], rts_trans[2]));
	rts.setRot(RotationFromQuaternion(aejVector(rts_orient[0], 
			rts_orient[1], rts_orient[2], rts_orient[3])));

	// append rts to pointing
	ptg = ptg * rts;

	// convert back to pointing 
	aejVector rv = FixedAnglesFromRotation(ptg.rot());
	aejVector pv = ptg.trans();
	for (i=0; i<3; i++) {
		prot[i] = rv[i]*180.0/PI;
		ptrans[i] = pv[i];
	}
}
