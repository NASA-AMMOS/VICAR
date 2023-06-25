// test object group functions

#include <stdio.h>
#include "grape/object.h"

void dump_group(char *s, GroupObj &g)
{
	printf("%s: group has %d children: ", s, g.get_num_children());
	for (int i=0; i<g.get_num_children(); i++) 
		printf("%X ", g.get_child(i));
	putchar('\n');
}

int main()
{
	// create a group
	GroupObj grp;

	dump_group("start", grp);

	grp.add_child((ObjNode *)0xA);
	grp.add_child((ObjNode *)0xB);
	grp.add_child((ObjNode *)0xC);
	dump_group("add3", grp);

	grp.remove_child(1);
	dump_group("rm1", grp);

	grp.add_child((ObjNode *)0xD, 0);
	dump_group("add0" , grp);

	grp.remove_child((ObjNode *)0xC);
	dump_group("rm2", grp);

	printf("is_child(D) = %d\n", grp.is_child((ObjNode *)0xD));
	printf("is_child(X) = %d\n", grp.is_child((ObjNode *)0x1234));

	// (dies at implied destructor here, since we haven't been
	// using valid ObjNodes...)
}
