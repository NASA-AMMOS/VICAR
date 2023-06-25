// test bounding volume transformation
#include <stdio.h>
#include "grape/object.h"
#include "summitt_func.h"
#include "range.h"

int main()
{
	Summitt_range r1, r2;

	r1.xmin = -1.0;
	r1.xmax = 2.0;
	r1.ymin = 1.5;
	r1.ymax = 4.1;
	r1.zmin = -5.7;
	r1.zmax = -0.01;
	r2 = r1;

	Obj obj;
	ObjNode node;
	node.set_object(&obj);
#if 1
	node.x = 5.5;
	node.y = -8.9;
	node.z = 12.0;
	node.xrot = -0.4;
	node.yrot = 1.8;
	node.zrot = -0.03;
	node.xscale = 1.0;
	node.yscale = 1.1;
	node.zscale = 5.0;
#endif

	// old way
	world_volume(&node, &r1);
	r1.dump(stderr, "old way");
	
	// new way
	ZMatrix m2w;
	node.GetTransformationMatrix(m2w);
	r2.transform(m2w);
	r2.dump(stderr, "new way");
}
