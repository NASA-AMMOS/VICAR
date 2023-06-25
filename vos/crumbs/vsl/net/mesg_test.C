/***************** ACTORS Software System *******************************
 *
 *      Copyright (C) 1994, California Institute of Technology
 *      All rights reserved.
 *
 ************************************************************************
 *      Developed by the Visualization & Earth Science Applications Group,
 *      Image Processing Applications and Development Section,
 *      Jet Propulsion Laboratory,
 *      California Institute of Technology
 ************************************************************************
 * Module: mesg_test.C
 *
 * Purpose: Slave test task
 *
 * Limitations:
 *
 * Original Author: John Wright
 *
 * Current cognizant programmer: John Wright
 *
 * Created: 08/03/94
 *
 * Last Modified: 24 Nov 1994  1.0
 *
 * Modification History:
 *
 ************************************************************************
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <math.h>
#include "net/net_lib.h"


main(int , char **argv)
{
	int	i, j, k, stat;
	char	fname[256];
	char	*tname;

	Master_Task	*my_master;

	Distributed_PVM_Task	*sibling;
	Distributed_PVM_Task	dummy;
	FILE			*fp;

	sscanf(argv[1], "%d", &j);

	my_master = new Master_Task();

	sprintf(fname,"/home/john/mesg_test_out%d.dat",j);
	fp = fopen(fname, "w");

for(k=0; k<3; k++) {
	my_master->recv_buffer();
	my_master->get_from_buffer(&i);
	// my_master->get_from_buffer(&stat);
// fprintf(fp,"Got the following bytes after first 4 %08x\n",stat);
	my_master->get_from_buffer(&tname);
fprintf(fp,"Got message of length %d from master with value=%d and string>%s<\n", my_master->mesg_length(),i,tname);
fflush(fp);
	my_master->reset_buffer();
	my_master->put_in_buffer(j);
	my_master->put_in_buffer(i);
	my_master->put_in_buffer(tname, 10);
	stat = my_master->send_buffer();
fprintf(fp,"Sent message to master with status=%d\n",stat);
fflush(fp);
}

	sibling = &dummy;
	sibling->recv_itself(my_master);
	sibling->reset_buffer();
	sibling->put_in_buffer(j);
	sibling->put_in_buffer(i);
	sibling->send_buffer();
	sibling->recv_buffer();
	sibling->get_from_buffer(&i);
	sibling->get_from_buffer(&i);
	sleep(j);
	my_master->reset_buffer();
	my_master->put_in_buffer(j);
	my_master->put_in_buffer(i);
	my_master->send_buffer();

	sleep(j);

	delete my_master;


  exit(0);
}


