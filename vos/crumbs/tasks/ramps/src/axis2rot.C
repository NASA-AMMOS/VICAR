// convert rotation in form (axis, angle) to pitch/roll/yaw
#include <stdio.h>
#include "grape/aej.h"
#include "grape/aejMatrix.h"
#include "grape/aejTransform.h"
#include "grape/matrix.h"

#ifndef PI
#define PI 3.141592654
#endif

int main(int argc, char **argv)
{
	if (argc != 5) {
		printf("usage: %s axis-x axis-y axis-z angle-deg\n", argv[0]);
		return 1;
	}
	aejVector axis(atof(argv[1]), atof(argv[2]), atof(argv[3]));
	aejMatrix rot = RotationFromAxisAngle(axis, 
		atof(argv[4])*PI/180.0);
	aejVector r = FixedAnglesFromRotation(rot);
	printf("Rx = %f Ry = %f Rz = %f\n", 
		r[0]*180.0/PI, r[1]*180.0/PI, r[2]*180.0/PI);

	// verify
	ZMatrix m;
	MakeRotationMatrix(m, r[0]*180.0/PI, r[1]*180.0/PI, r[2]*180.0/PI);
	//MatTranspose(m, m);
	double fwd[3] = { 1.0, 0.0, 0.0 };
	double up[3] = { 0.0, 0.0, -1.0 };
	double a[3];
	MultPoints(fwd, m, a);
	printf("Fwd vector=%f %f %f\n", a[0], a[1], a[2]);
	MultPoints(up, m, a);
	printf("Up vector=%f %f %f\n", a[0], a[1], a[2]);
}
