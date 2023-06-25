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
 * Module: net_test.C
 *
 * Purpose: Master task for testing PVM network classes
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
#include <sys/types.h>
#include <malloc.h>
#include "net/net_lib.h"

#define NUM_TASKS	20

main(int , char **)
{
	Network	*my_net;
	Distributed_Command *my_cmd[10];
	Distributed_PVM_Task *dpt[10];
	Pool_Of_Tasks *pool;
	int	i, j, k, l, done, stat;
	char	cmd[256];
	char	*args[10];

// mallopt(M_DEBUG, 1);

	// printf("Initial net status = %d\n",my_net->get_net_status());

	my_net = new Network();
	printf("Done allocating new network\n");
	printf("Now net status = %d\n",my_net->get_net_status());

// Test multiple distributed commands
	for(i=0; i<10; i++) {
	my_cmd[i] = new Distributed_Command();
	sprintf(cmd,"sleep %2d ; echo",((i*3)&7)+30);
	my_cmd[i]->set_task_string(cmd);
	args[0] = strdup("$PVM_ARCH");
	args[1] = strdup("temporary");
	args[2] = strdup("xyzabc");
	args[3] = NULL;
	my_cmd[i]->set_args_string(args);
	my_net->run_command(my_cmd[i]);
	sleep(3);
	}

	done = FALSE;
	while(!done) {
	done = TRUE;
	for(i=0; i<10; i++) {
		stat = my_cmd[i]->get_status();
		if(stat == DIST_TASK_COMPLETE) printf("Task %d complete\n",i);
		else done = FALSE;
	}
	printf("\n"); sleep(1);
	}
	printf("Distributed commands complete\n");
	for(i=0; i<10; i++) {
		delete(my_cmd[i]);
	}

// Test pool of distributed pvm tasks with message passing
	pool = new Pool_Of_Tasks(NUM_TASKS);

	args[0] = strdup("10");
	args[1] = NULL;

	for(i=0; i<NUM_TASKS; i++) {
		pool->get_task(i)->set_task_string("mesg_test");
		sprintf(args[0], "%d", i);
//		pool->get_task(i)->set_args_string(args);
		(*pool)[i]->set_args_string(args);
	}

	pool->set_tasks_per_cpu(2.6);

	fprintf(stderr,"Task running...will sleep 5\n");
	sleep(5);

	my_net->run_pool_blasted(pool);

	fprintf(stderr,"Tasks fired off.\n");
	sleep(5);

	strcpy(cmd, "****************************");

Distributed_PVM_Task *temp;

for(l=0; l<3; l++) {
	for(i=NUM_TASKS-1; i>=0; i--) {
		pool->get_task(i)->reset_buffer();
		pool->get_task(i)->put_in_buffer(i);
		pool->get_task(i)->put_in_buffer("This is a test.", 10);
		temp = pool->get_task(i);
		stat = temp->send_buffer();
	}
	fprintf(stderr,"Messages fired off.\n");

	i = 0;
	while(i < NUM_TASKS) {
	//	sleep(1);
		pool->get_task(i)->recv_buffer();
		pool->get_task(i)->get_from_buffer(&j);
		pool->get_task(i)->get_from_buffer(&k);
		pool->get_task(i)->get_from_buffer(cmd, 10);
		fprintf(stderr,"Recieved message from task %d which received argument %d and message %d with string %s\n",i, j, k, cmd);
		i++;
	}
	sleep(5);
}

	for(i=0; i<NUM_TASKS; i++) {
		if(i % 2) {
//			fprintf(stderr,"Sending task %d to task %d\n",i, i-1);
			pool->get_task(i)->send_itself(pool->get_task(i-1));
		} else {
//			fprintf(stderr,"Sending task %d to task %d\n",i, i+1);
			pool->get_task(i)->send_itself(pool->get_task(i+1));
		}
	}

	for(i=0; i<NUM_TASKS; i++) {
		sleep(1);
		pool->get_task(i)->recv_buffer();
		pool->get_task(i)->get_from_buffer(&j);
		pool->get_task(i)->get_from_buffer(&k);
		fprintf(stderr,"Recieved message from task %d which received argument %d and message %d\n",i, j, k);
	}

//	if(pool->get_pool_status() != POOL_COMPLETE) {
//		my_net->pool_recover_faults(pool);
//	}

//	printf("All jobs dispatched.\n");
	printf("All jobs complete.\n");


	delete(my_net);
	printf("Done deleting new network\n");
	delete(pool);
	printf("Now net status = %d\n",my_net->get_net_status());

  exit(0);
}


