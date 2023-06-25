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
 * Module: list_test.C
 *
 * Purpose: Master task for testing PVM task list network classes
 *
 * Limitations:
 *
 * Original Author: John Wright
 *
 * Current cognizant programmer: John Wright
 *
 * Created: 09/13/99
 *
 * Last Modified: 
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
#include "net/pvm_task_list.h"

#define NUM_TASKS	20

main(int , char **)
{
	Network	*my_net;
	Distributed_Command *my_cmd[10];
	Distributed_PVM_Task *dpt[10];
	Pool_Of_Tasks *pool;
	PVM_Task_List	*list;
	int	i, j, k, l, done, stat;
	char	cmd[256];
	char	*args[10];

// mallopt(M_DEBUG, 1);

	// printf("Initial net status = %d\n",my_net->get_net_status());

	my_net = new Network();
	printf("Done allocating new network\n");
	printf("Now net status = %d\n",my_net->get_net_status());

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

// Test list of distributed pvm tasks with message passing
	list = new PVM_Task_List();

	if(! list->set_default_host("tone")) fprintf(stderr,"Default host tone does not work.\n");
	if(! list->set_default_host("lux")) fprintf(stderr,"Default host lux does not work.\n");

	args[0] = strdup("10");
	args[1] = NULL;

	for(i=0; i<NUM_TASKS; i++) {
		sprintf(cmd,"/net/cassis/usr/people/john/proj/vsl/net/mesg_test %d\n",i);
		list->add_task(cmd, "lux");
	}

	fprintf(stderr,"%d Tasks running...will sleep 5\n", list->get_num_tasks());
	sleep(5);

Distributed_PVM_Task *temp;

for(l=0; l<3; l++) {
	for(i=NUM_TASKS-1; i>=0; i--) {
		// list->get_task(i)->reset_buffer();
		// list->get_task(i)->put_in_buffer(i);
		// list->get_task(i)->put_in_buffer("This is a test.", 10);
		// temp = list->get_task(i);
		// stat = temp->send_buffer();
	}
	list->reset_buffer();
	list->put_in_buffer(-1);
	list->put_in_buffer("This is a test.", 10);
	stat = list->send_buffer();

	fprintf(stderr,"Messages fired off.\n");

	i = 0;
	while(i < NUM_TASKS) {
	//	sleep(1);
		list->get_task(i)->recv_buffer();
		list->get_task(i)->get_from_buffer(&j);
		list->get_task(i)->get_from_buffer(&k);
		list->get_task(i)->get_from_buffer(cmd, 10);
		fprintf(stderr,"Recieved message from task %d which received argument %d and message %d with string %s\n",i, j, k, cmd);
		i++;
	}
	sleep(5);
}

	for(i=0; i<NUM_TASKS; i++) {
		if(i % 2) {
//			fprintf(stderr,"Sending task %d to task %d\n",i, i-1);
			list->get_task(i)->send_itself(list->get_task(i-1));
		} else {
//			fprintf(stderr,"Sending task %d to task %d\n",i, i+1);
			list->get_task(i)->send_itself(list->get_task(i+1));
		}
	}

	for(i=0; i<NUM_TASKS; i++) {
		sleep(1);
		// list->get_task(i)->recv_buffer();
		// list->get_task(i)->get_from_buffer(&j);
		// list->get_task(i)->get_from_buffer(&k);
		j = list->task_index_for_next_message();
		fprintf(stderr,"Index of task for next message is %d\n", j);
		if(j >= 0) {
			temp = list->task_for_next_message();
			temp->recv_buffer();
			temp->get_from_buffer(&k);
			temp->get_from_buffer(&l);
			fprintf(stderr,"Recieved message %d from task %d which received argument %d and message %d\n",i, j, k, l);
		} else {
			fprintf(stderr,"No messages for any task in list.\n");
		}
	}

	printf("All jobs complete.\n");


	delete(list);
	delete(my_net);
	printf("Done deleting new network\n");
	printf("Now net status = %d\n",my_net->get_net_status());

  exit(0);
}


