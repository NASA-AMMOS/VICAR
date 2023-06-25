#include <stdio.h>
#include <string.h>
#include <grape/object.h>
#include "cahvor.h"

int main()
{
	char buf[256];

	for (;;) {
		double r[3], t[3], q[4], t2[3];
		printf("Orig rot: ");
		gets(buf);
		sscanf(buf, "%lf %lf %lf", &r[0], &r[1], &r[2]);
		printf("Orig trans: ");
		gets(buf);
		sscanf(buf, "%lf %lf %lf", &t[0], &t[1], &t[2]);
		printf("RTS trans: ");
		gets(buf);
		sscanf(buf, "%lf %lf %lf", &t2[0], &t2[1], &t2[2]);
		printf("RTS quat: ");
		gets(buf);
		sscanf(buf, "%lf %lf %lf %lf", &q[0], &q[1], &q[2], &q[3]);

		summitt_pointing_xform(r, t, q, t2);
		printf("New rot = %f %f %f\n", r[0], r[1], r[2]);
		printf("New trans = %f %f %f\n", t[0], t[1], t[2]);

		ObjNode on;
		on.x = t[0]; on.y = t[1], on.z = t[2];
		on.xrot = r[0]; on.yrot = r[1]; on.zrot = r[2];
		ZMatrix m;
		on.GetObjToWorldTransform(m);
		printf("Input XYZ: ");
		gets(buf);
		sscanf(buf, "%lf %lf %lf", &t[0], &t[1], &t[2]);
		MultPoints(t, m, t2);
		printf("Transformed point: %f %f %f\n\n", t2[0], t2[1], t2[2]);
	}
}
