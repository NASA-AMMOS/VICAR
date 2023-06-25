/***************** ACTORS Software System *******************************
 *
 *	Copyright (C) 1994, California Institute of Technology
 *	All rights reserved.
 *
 ************************************************************************
 *	Developed by the Visualization & Earth Science Applications Group,
 *	Image Processing Applications and Development Section,
 *	Jet Propulsion Laboratory,
 *	California Institute of Technology
 ************************************************************************
 * Module: net_lib.C
 *
 * Purpose: Provide library of tools for distributing tasks across net
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
#include <math.h>
#include <unistd.h>
//#include <malloc.h>
#include "net/net_lib.h"

// ****************************Generic utility routines ********

int count_args(char **arglist)
{
	int	count = 0;

	while(*arglist) {
		count++;
		arglist++;
	}
	return(count);
}
void free_all(char **arglist)
{
	char	**temp_arglist;

	temp_arglist = arglist;
	while(*temp_arglist) {
		free(*temp_arglist);
		temp_arglist++;
	}
	free((char *)arglist);
	arglist = NULL;
}

int pvm_myhalt(void)
{
	char	*tname, *aname, command[1024];

	tname = getenv("PVM_ROOT");
	if(!tname) {
		fprintf(stderr," PVM Root Directory not set -- check .cshrc\n");
		return(PvmCantStart);
	}
	aname = getenv("PVM_ARCH");
	if(!aname) {
		fprintf(stderr," PVM Architecture Environment not set -- check .cshrc\n");
		return(PvmCantStart);
	}

	fprintf(stderr,"Shutting down PVM net.\n");
	sprintf(command,"echo halt | %s/lib/%s/pvm >/dev/null", tname, aname);
	system(command);
	return(NET_OFFLINE);
}

int pvm_start(void)
{
	int	tid;
	FILE	*dummy;
	char	*tname, *aname, command[1024];

	tname = getenv("PVM_ROOT");
	if(!tname) {
		fprintf(stderr," PVM Root Directory not set -- check .cshrc\n");
		return(NET_OFFLINE);
	}
	aname = getenv("PVM_ARCH");
	if(!aname) {
		fprintf(stderr," PVM Architecture Environment not set -- check .cshrc\n");
		return(NET_OFFLINE);
	}

	dummy = fopen(".hostfile","r");

	if(dummy) {
		fclose(dummy);
		sprintf(command,"echo | %s/lib/%s/pvm .hostfile >/dev/null", tname, aname);
		system(command);
	} else {
		sprintf(command,"%s/hostfile", tname);
		dummy = fopen(command,"r");
		if(dummy) {
			fclose(dummy);
			sprintf(command,"echo | %s/lib/%s/pvm %s/hostfile >/dev/null", tname, aname, tname);
			system(command);
		} else {
			sprintf(command,"echo | %s/lib/%s/pvm >/dev/null", tname, aname);
			system(command);
		}
	}
	tid = pvm_mytid();
	if(tid <0) {
		fprintf(stderr,"\n\nUnable to start PVM\n");
		return(NET_OFFLINE);
	}
	fprintf(stderr,"\n\nPVM started up okay.\n");
	return(tid);
}


// **************************** Distributed Task Messages ***********

Task_Status Generic_Distributed_Task::get_status()
{
	int	local_status;

	local_status = pvm_pstat(task_id);
	if(local_status == PvmOk) {
		local_status = DIST_TASK_BUSY;
	} else if(local_status == PvmNoTask) {
		local_status = DIST_TASK_COMPLETE;
	} else if(local_status == PvmBadParam) {
		local_status = DIST_TASK_IDLE;
		fprintf(stderr," Whoops -- Bad task id in get status\n");
	} else {
		local_status = DIST_TASK_IDLE;
	}
	return(local_status);
}

void Child_Task::set_task_string(char *user_task_string)
{
	if(task_string != user_task_string) {
		if(task_string)free(task_string);
		task_string = strdup(user_task_string);
	}
}

void Child_Task::set_args_string(char **user_args_string)
{
	char	**temp_ptr;
	int	i, num_args;

	if(args_string != user_args_string) {
		if(args_string)free_all(args_string);
		num_args = count_args(user_args_string);
		args_string = (char **)malloc(sizeof(char *) * (num_args+1));
		temp_ptr = args_string;
		for(i=0; i<num_args; i++) {
			temp_ptr[i] = strdup(user_args_string[i]);
		}
		temp_ptr[num_args] = NULL;
	}
}

void Child_Task::parse_string(char *user_args_string)
{
	char *temp_string, *t2;

	temp_string = strdup(user_args_string);
	t2 = strtok(temp_string, " \t\n\0");
	set_task_string(t2);
	parse_args_string(user_args_string + strlen(t2) + 1);
	free(temp_string);
}

void Child_Task::parse_args_string(char *user_args_string)
{
	char *temp_string, *t2, **t3 = NULL;
	int	num_tokens = 0;

	temp_string = strdup(user_args_string);
	t2 = strtok(temp_string, " \t\n\0");
	do {
		num_tokens++;
		t3 = (char **)realloc((void *)t3,num_tokens*sizeof(char *));
		t3[num_tokens-1] = t2;
		if(t2)t2 = strtok(NULL, " \t\n\0");
	} while(t2);
	num_tokens++;
	t3 = (char **)realloc((void *)t3,num_tokens*sizeof(char *));
	t3[num_tokens-1] = t2;
	set_args_string(t3);
	free(temp_string);
}

Task_Status Child_Task::kill_task()
{
	int	status;

	if(task_id) {
		if(get_status() == DIST_TASK_BUSY) {
			status = pvm_kill(task_id);
			return(status);
		} else {
			return((Task_Status)0);
		}
	} else {
		return((Task_Status)0);
	}
}

Child_Task::~Child_Task()
{
	kill_task();
	if(task_string)free(task_string);
	if(args_string)free_all(args_string);
}

Child_Task::Child_Task()
{
	task_string = NULL;
	args_string = (char **)malloc(sizeof(char *) * 2);
	args_string[0] = NULL;
	args_string[1] = NULL;
	data = NULL;
	pre_notify_func = NULL;
	post_notify_func = NULL;
}

Generic_Distributed_Task::~Generic_Distributed_Task()
{
}

Generic_Distributed_Task::Generic_Distributed_Task()
{
	task_id = (int)0;
}

Message_Passing_Task::Message_Passing_Task(int packmode)
{
	send_msg_buff_id = pvm_mkbuf(packmode);
	recv_msg_buff_id = 0;
}

Message_Passing_Task::~Message_Passing_Task()
{
}

// **************************Distributed_Command Class Messages ********

void Distributed_Command::set_task_string(char *user_task_string)
{
	if(args_string[0] != user_task_string) {
		if(args_string[0])free(args_string[0]);
		args_string[0] = strdup(user_task_string);
	}
}

void Distributed_Command::set_args_string(char **user_args_string)
{
	char	**temp_ptr;
	int	i, num_args;

	if(&args_string[1] != user_args_string) {
		temp_ptr = args_string;
		num_args = count_args(user_args_string);
		args_string = (char **)malloc(sizeof(char *) * (num_args+2));
		args_string[0] = NULL;
		set_task_string(temp_ptr[0]);
		free_all(temp_ptr);
		temp_ptr = &args_string[1];
		for(i=0; i<num_args; i++) {
			temp_ptr[i] = strdup(user_args_string[i]);
		}
		temp_ptr[num_args] = NULL;
	}
}

Distributed_Command::Distributed_Command()
{
//	set_task_string("any_cmd");
	Child_Task::set_task_string((char *)"any_cmd");
	
}

Distributed_Command::~Distributed_Command()
{
	char	**temp_arglist;

	temp_arglist = args_string;
	if(*temp_arglist)free(*temp_arglist); // conditionally free command
	temp_arglist++;
	while(*temp_arglist) {
		free(*temp_arglist);
		temp_arglist++;
	}
	free((char *)args_string);
}

// ************************** Generic Pool of Tasks Class Messages *****

void  Generic_Pool_Of_Tasks::reset_task_list()
{
	int	i;

	next_task = 0;
	for(i=0; i<num_tasks; i++) task_list[i]->kill_task();
	for(i=0; i<num_tasks; i++) task_list[i]->set_task_id((int)0);
}

Child_Task *Generic_Pool_Of_Tasks::find_task_by_id(int tid)
{
	for (int i=0; i<num_tasks; i++) {
		if (task_list[i]->task_id == tid)
			return task_list[i];
	}
	return NULL;
}

Pool_Status Generic_Pool_Of_Tasks::get_pool_status()
{
	int	i;
        Pool_Status     status;


	status = POOL_COMPLETE;
	for(i=0; i<next_task; i++) {
		if(get_task(i)->get_status() == DIST_TASK_BUSY) {
			status = POOL_BUSY;
		}
	}
	if(status == POOL_COMPLETE) {
		for(i=0; i<next_task; i++) {
			if(get_task(i)->get_status() == DIST_TASK_IDLE) {
				status = POOL_IDLE;
			}
		}
	}
	if(status == POOL_COMPLETE && next_task < num_tasks) {
		status = POOL_IDLE;
	}
	return(status);
}
		
// ************************** Pool of Tasks Class Messages *************

void Pool_Of_Tasks::init(int task_count)
{
	int	i;

	num_tasks = task_count;
	if(num_tasks > 0) {
		task_list = (Child_Task **)
			malloc(sizeof(Distributed_PVM_Task *) * num_tasks);
		for(i=0; i<num_tasks; i++) {
			task_list[i] = new Distributed_PVM_Task();
		}
	} else {
		task_list = NULL;
	}
	tasks_per_cpu = 1.0;
	reset_task_list();
}

void Pool_Of_Tasks::set_num_tasks(int task_count)
{
	int	i;

	if(task_list) {
		for(i=0; i<num_tasks; i++) {
			delete((Distributed_PVM_Task *)task_list[i]);
		}
		free((char *)task_list);
		task_list = NULL;
	}
	num_tasks = 0;		//Just for completeness

	init(task_count);
}

Distributed_PVM_Task *Pool_Of_Tasks::operator()(int index)
{
	return(get_task(index));
}


Pool_Of_Tasks::Pool_Of_Tasks(int task_count)
{
	init(task_count);
}

Pool_Of_Tasks::~Pool_Of_Tasks()
{
	int	i;

	if(task_list) {
		for(i=0; i<num_tasks; i++) {
			delete((Distributed_PVM_Task *)task_list[i]);
		}
		free((char *)task_list);
		task_list = NULL;
	}
	num_tasks = 0;		//Just for completeness
}


// ************************** Pool of Commands Class Messages *************
void Pool_Of_Commands::init(int task_count)
{
	int	i;

	num_tasks = task_count;
	if(num_tasks > 0) {
		task_list = (Child_Task **)
			malloc(sizeof(Distributed_Command *) * num_tasks);
		for(i=0; i<num_tasks; i++) {
			task_list[i] = new Distributed_Command();
		}
	} else {
		task_list = NULL;
	}
	tasks_per_cpu = 1.0;
	reset_task_list();
}

void Pool_Of_Commands::set_num_tasks(int task_count)
{
	int	i;

	if(task_list) {
		for(i=0; i<num_tasks; i++) {
			delete((Distributed_Command *)task_list[i]);
		}
		free((char *)task_list);
		task_list = NULL;
	}
	num_tasks = 0;		//Just for completeness

	init(task_count);
}

Pool_Of_Commands::Pool_Of_Commands(int task_count)
{
	init(task_count);
}

Pool_Of_Commands::~Pool_Of_Commands()
{
	int	i;

	if(task_list) {
		for(i=0; i<num_tasks; i++) {
			delete((Distributed_Command *)task_list[i]);
		}
		free((char *)task_list);
		task_list = NULL;
	}
	num_tasks = 0;		//Just for completeness
}


// **************************Network Host Class Messages ****************

Network_Host::Network_Host(struct pvmhostinfo *host_ptr)
{
	name = strdup(host_ptr->hi_name);
	if(pvm_mstat(name) != PvmOk) {
	    fprintf(stderr,"Whoops - Host %s not in virtual net.\n",name);
	    host_status = HOST_UNAVAILABLE;
	} else {
	    host_status = HOST_IDLE;
	}
	architecture = strdup(host_ptr->hi_arch);
	speed = host_ptr->hi_speed;
	if(speed != 1000) {
		num_cpus = speed & 0x3ff;
		speed = speed & ~0x3ff;
	} else {
		num_cpus = 1;
	}
	demon_task_id = host_ptr->hi_tid;
	master_task_id = pvm_mytid();
	if(master_task_id < 0) {
		pvm_perror((char *)" Attempting to start PVM.\n");
		master_task_id = pvm_start();
	}
}

Network_Host::Network_Host(char *hostname)
{
	int	num_hosts;
	int	num_archs;
	int	status;
	int	i;
	struct	pvmhostinfo	*host_ptr;

	status = pvm_config(&num_hosts, &num_archs, &host_ptr);
	if(status < 0) {
		fprintf(stderr,"Whoops - PVM Network not responding to congifuration request.  Status=%d\n", status);
		host_status = HOST_UNAVAILABLE;
		return;
	}
	if(pvm_mstat(hostname) != PvmOk) {
	    fprintf(stderr,"Whoops - Host %s not in virtual net.\n",hostname);
	    host_status = HOST_UNAVAILABLE;
	} else {
	    host_status = HOST_IDLE;
	    for(i=0; i<num_hosts; i++) {
		if(!strcmp(hostname,host_ptr[i].hi_name)) {
			name = strdup(host_ptr[i].hi_name);
			architecture = strdup(host_ptr[i].hi_arch);
			speed = host_ptr[i].hi_speed;
			if(speed != 1000) {
				num_cpus = speed & 0x3ff;
				speed = speed & ~0x3ff;
			} else {
				num_cpus = 1;
			}
			demon_task_id = host_ptr[i].hi_tid;
			master_task_id = pvm_mytid();
			if(master_task_id < 0) {
				pvm_perror((char *)" Attempting to start PVM.\n");
				master_task_id = pvm_start();
			}
		}
	    }
	}
}

Network_Host::~Network_Host()
{
	if(host_status != HOST_UNAVAILABLE) {
		free(name);
		free(architecture);
	}
}

Host_Status Network_Host::get_host_status(float scale)
{
	int			status;
	int			num_tasks;
	struct	pvmtaskinfo	*task_ptr;
	Host_Status		local_status = 0;

	status = pvm_mstat(name);
	if(status != PvmOk) {
		local_status = HOST_UNAVAILABLE;
	} else {
		status = pvm_tasks( demon_task_id, &num_tasks, &task_ptr);
		if(status < 0) {
			local_status = HOST_UNAVAILABLE;
		} else {
			if(num_tasks == 0) {
				local_status = HOST_IDLE;
			} else  if(num_tasks < (int)(num_cpus*scale+0.1)) {
				local_status = HOST_BUSY;
			} else {
				local_status = HOST_FULL;
			}
//			local_status |= host_locked_status;
		}
	}
	host_status = local_status;
	return(local_status);
}

Task_Status Network_Host::run_command(Child_Task *dpt)
{
	int		ntasks;
	int		task_id;
	Task_Status	status;

	if(dpt->get_task_string() && !(get_host_status() & (HOST_LOCKED | HOST_UNAVAILABLE))){
		if(dpt->pre_notify_func) (*dpt->pre_notify_func)(dpt, dpt->data);
		ntasks = pvm_spawn(dpt->get_task_string(), dpt->get_args_string(), 
			PvmTaskHost, name, 1, &task_id);
		((Generic_Distributed_Task *)dpt)->set_task_id(task_id);
		if(ntasks != 1) {
		  fprintf(stderr,"Whoops - Problem starting command on host %s\n", name);
		  fprintf(stderr,"   executable \"%s\" may be missing\n",dpt->get_task_string());
		  fprintf(stderr,"   Selected host may be locked.\n");
		  status =  DIST_TASK_ERROR;
		} else {
		  status =  DIST_TASK_BUSY;
		}
		if(dpt->post_notify_func)(*dpt->post_notify_func)(dpt, dpt->data, status);
	} else {
		status =  DIST_TASK_ERROR;
	}
	return(status);
}

// **************************Network Class Messages ****************

Network::Network( int use_host)
{
	auto_started = FALSE;
	master_task_id = pvm_mytid();
	if(master_task_id < 0) {
		pvm_perror((char *)" Attempting to start PVM.\n");
		master_task_id = pvm_start();
		if(master_task_id > 0)auto_started = TRUE;
	}
	init(use_host);
	control_string = NULL;
	control_flag = PvmTaskDefault;
	current_task_id = master_task_id;
}

Network::Network(int cf, char *cs, int use_host)
{
	auto_started = FALSE;
	master_task_id = pvm_mytid();
	if(master_task_id < 0) {
		pvm_perror((char *)" Attempting to start PVM.\n");
		master_task_id = pvm_start();
		if(master_task_id > 0)auto_started = TRUE;
	}
	init(use_host);
	control_string = NULL;
	set_control_flag(cf, cs);
	current_task_id = master_task_id;
}

Network::~Network()
{
	int	i;
	int	ntasks, status;
	struct	pvmtaskinfo	*taskp;

	if(master_task_id >= 0) {
		for(i=0; i<number_of_hosts; i++) {
			delete host_list[i];
		}
		delete(host_list);
	}
	status = pvm_tasks(0, &ntasks, &taskp);
	if(pvm_parent() == PvmNoParent) {
		pvm_exit();
		if(auto_started) {
			if(status >= 0 && ntasks <= 1) {
				pvm_myhalt();
			}
		}
	}
}

void Network::init(int use_host)
{
	int	num_hosts;
	int	num_archs;
	int	status;
	int	i;
	struct	pvmhostinfo	*host_ptr;

	if(master_task_id >= 0) {
		status = pvm_config(&num_hosts, &num_archs, &host_ptr);
		if(status < 0) {
			fprintf(stderr,"Whoops - PVM Network not responding to congifuration request.  Status=%d\n", status);
			return;
		}
		number_of_hosts = num_hosts;
		host_list = new Network_Host *[num_hosts];
		for(i=0; i<num_hosts; i++) {
			host_list[i] = new Network_Host(&(host_ptr[i]));
			if(use_host && host_list[i]->host_is_local()) {
				host_list[i]->set_num_cpus(host_list[i]->get_num_cpus() + 1);
			}
		}
#ifdef debug
		char	buff[256];
		for(i=0; i<num_hosts; i++) {
			host_list[i]->get_host_name(buff);
			printf(" Host: %20s  ", buff);
			host_list[i]->get_host_architecture(buff);
			printf(" Arch: %10s  ", buff);
			printf(" Speed: %7d", host_list[i]->get_host_speed());
			printf(" CPUs: %4d\n", host_list[i]->get_num_cpus());
		}
#endif
	}
}

void Network::set_control_flag(int cf, char *cs)
{
	control_flag = cf;
	if(control_string)free(control_string);
	if(cs) {
		control_string = strdup(cs);
	} else {
		control_string = NULL;
	}
}

Net_Status Network::get_net_status()
{
	int	num_hosts;
	int	num_archs;
	int	status;
	struct	pvmhostinfo	*host_ptr;

	if(master_task_id < 0) {
		return(NET_OFFLINE);
	}
	status = pvm_config(&num_hosts, &num_archs, &host_ptr);
	if(status < 0) {
		return(NET_OFFLINE);
	} else if(num_hosts <= 0) {
		return(NET_EMPTY);
	} else {
		return(NET_READY);
	}
}

Task_Status Network::run_command(Child_Task *dpt)
{
	int		ntasks;
	int		task_id;
	Task_Status	status;

	if(dpt->get_task_string() && get_net_status() == NET_READY){
		if(dpt->pre_notify_func) (*dpt->pre_notify_func)(dpt, dpt->data);
		ntasks = pvm_spawn(dpt->get_task_string(), dpt->get_args_string(), 
			control_flag, control_string, 1, &task_id);
		((Generic_Distributed_Task *)dpt)->set_task_id(task_id);
		if(ntasks != 1) {
		   fprintf(stderr,"Whoops - Problem starting command on net\n");
		   fprintf(stderr,"   Control flags may be too restrictive\n");
		   fprintf(stderr,"   executable \"%s\" may be missing\n",dpt->get_task_string());
		   fprintf(stderr,"   or selected hosts are all locked.\n");
		   status =  DIST_TASK_ERROR;
		} else {
		   status =  DIST_TASK_BUSY;
		}
		if(dpt->post_notify_func)(*dpt->post_notify_func)(dpt, dpt->data, status);
	} else {
		status =  DIST_TASK_ERROR;
	}
	return(status);
}

Task_Status Network::run_command(Child_Task dpt)
{
	return(run_command(&dpt));
}

void Network::run_pool_blocked(Generic_Pool_Of_Tasks *dpt)
{
	int		i;
	Child_Task	*curr_task;
	Task_Status	status;

	if(dpt->notify_func)(*dpt->notify_func)(dpt, dpt->data);
	dpt->reset_task_list();
	curr_task = dpt->get_next_task();
	while(curr_task) {
		for(i=0; i<number_of_hosts; i++) {
			while(host_list[i]->get_host_status(dpt->get_tasks_per_cpu()) != HOST_FULL &&
				curr_task) {
			    status = host_list[i]->run_command(curr_task);
			    if(status != DIST_TASK_ERROR) {
			        curr_task = dpt->get_next_task();
			    } else {
				break;
			    }
			}
		}
	}
}

void Network::run_pool_blasted(Generic_Pool_Of_Tasks *dpt)
{
	Child_Task	*curr_task;

	if(dpt->notify_func)(*dpt->notify_func)(dpt, dpt->data);
	dpt->reset_task_list();
	curr_task = dpt->get_next_task();
	while(curr_task) {
	    run_command(curr_task);
	    curr_task = dpt->get_next_task();
	}
}

void Network::run_pool_start(Generic_Pool_Of_Tasks *dpt)
{
	int		i;
	int		tasks_left = TRUE;
	Child_Task	*curr_task;
	Task_Status	status = DIST_TASK_IDLE;

	if(dpt->notify_func)(*dpt->notify_func)(dpt, dpt->data);
	dpt->reset_task_list();
	for(i=0; i<number_of_hosts; i++) {
		while(host_list[i]->get_host_status(dpt->get_tasks_per_cpu()) != HOST_FULL &&
			tasks_left) {
		    curr_task = dpt->get_next_task();
		    if(curr_task) {
			status = host_list[i]->run_command(curr_task);
			if(status == DIST_TASK_ERROR) {
				dpt->backup_one_task();
				break;
			}
		    } else {
			tasks_left = FALSE;
		    }
		}
	}
}

void Network::run_pool_continue(Generic_Pool_Of_Tasks *dpt)
{
	int		i;
	int		tasks_left = TRUE;
	Child_Task	*curr_task;
	Task_Status	status = DIST_TASK_IDLE;

	for(i=0; i<number_of_hosts; i++) {
		while(host_list[i]->get_host_status(dpt->get_tasks_per_cpu()) != HOST_FULL &&
			tasks_left) {
		    curr_task = dpt->get_next_task();
		    if(curr_task) {
			status = host_list[i]->run_command(curr_task);
			if(status == DIST_TASK_ERROR) {
				dpt->backup_one_task();
				break;
			}
		    } else {
			tasks_left = FALSE;
		    }
		}
	}
}

void Network::pool_recover_faults(Generic_Pool_Of_Tasks *dpt)
{
        int             i;
        Child_Task      *curr_task;
        Task_Status     status = DIST_TASK_IDLE;

fprintf(stderr,"Attempting to recover faults.\n");
	dpt->reset_task_list();
	curr_task = dpt->get_next_task();
	while(curr_task) {
	    if(curr_task->get_status() != DIST_TASK_COMPLETE) {
fprintf(stderr,"Found an incomplete job...rerunning.\n");
		for(i=0; i<number_of_hosts; i++) {
			if(host_list[i]->get_host_status(dpt->get_tasks_per_cpu()) != HOST_FULL &&
				curr_task) {
			    status = host_list[i]->run_command(curr_task);
			    if(status != DIST_TASK_ERROR) {
			        curr_task = dpt->get_next_task();
			    }
			    break;
			}
		}
	    } else {
		curr_task = dpt->get_next_task();
	    }
	}
}

Pool_Of_Tasks *Network::create_host_pool_of_tasks(char *cmd)
{
	int		i, count;
	Pool_Of_Tasks	*pool;
        Distributed_PVM_Task      *curr_task;
	char		buff[256];
	Task_Status	status;

	count = get_num_hosts();
	pool = new Pool_Of_Tasks(count);
	if(!pool) {
		fprintf(stderr," Whoops - Unable to create host pool of tasks.\n");
		return(pool);
	}
	if(cmd) {
		pool->reset_task_list();
		for(i=0; i<count; i++) {
			curr_task = pool->get_next_task();
			curr_task->parse_string(cmd);
			status = host_list[i]->run_command(curr_task);
			if(status == DIST_TASK_ERROR) {
				host_list[i]->get_host_name(buff);
				fprintf(stderr," Whoops - Problem launching host pool task on host %s\n",buff);
			}
		}
	}
	return(pool);
}

Pool_Of_Commands *Network::create_host_pool_of_commands(char *cmd)
{
	int		i, count;
	Pool_Of_Commands	*pool;
        Distributed_Command      *curr_task;
	char		buff[256];
	Task_Status	status;

	count = get_num_hosts();
	pool = new Pool_Of_Commands(count);
	if(!pool) {
		fprintf(stderr," Whoops - Unable to create host pool of commands.\n");
		return(pool);
	}
	if(cmd) {
		pool->reset_task_list();
		for(i=0; i<count; i++) {
			curr_task = pool->get_next_task();
			curr_task->parse_string(cmd);
			status = host_list[i]->run_command(curr_task);
			if(status == DIST_TASK_ERROR) {
				host_list[i]->get_host_name(buff);
				fprintf(stderr," Whoops - Problem launching host pool command on host %s\n",buff);
			}
		}
	}
	return(pool);
}
	

Pool_Of_Tasks *Network::create_cpu_pool_of_tasks(char *cmd)
{
	int		i, count, j;
	Pool_Of_Tasks	*pool;
        Distributed_PVM_Task      *curr_task;
	char		buff[256];
	Task_Status	status;

	pool = new Pool_Of_Tasks(get_num_cpus());
	if(!pool) {
		fprintf(stderr," Whoops - Unable to create cpu pool of tasks.\n");
		return(pool);
	}
	if(cmd) {
		count = 0;
		pool->reset_task_list();
		for(i=0; i<get_num_hosts(); i++) {
		    for(j=0; j<host_list[i]->get_num_cpus(); j++) {
			curr_task = pool->get_next_task();
			curr_task->parse_string(cmd);
			status = host_list[i]->run_command(curr_task);
			if(status == DIST_TASK_ERROR) {
				host_list[i]->get_host_name(buff);
				fprintf(stderr," Whoops - Problem launching cpu pool task on host %s\n",buff);
			}
			count++;
		    }
		}
		if(count != get_num_cpus()) {
			fprintf(stderr," Whoops - Internal fault in cpu pool task creation.\n");
		}
	}
	return(pool);
}

Pool_Of_Commands *Network::create_cpu_pool_of_commands(char *cmd)
{
	int		i, count, j;
	Pool_Of_Commands	*pool;
        Distributed_Command      *curr_task;
	char		buff[256];
	Task_Status	status;

	pool = new Pool_Of_Commands(get_num_cpus());
	if(!pool) {
		fprintf(stderr," Whoops - Unable to create cpu pool of commands.\n");
	} else if(cmd) {
		count = 0;
		pool->reset_task_list();
		for(i=0; i<get_num_hosts(); i++) {
		    for(j=0; j<host_list[i]->get_num_cpus(); j++) {
			curr_task = pool->get_next_task();
			curr_task->parse_string(cmd);
			status = host_list[i]->run_command(curr_task);
			if(status == DIST_TASK_ERROR) {
				host_list[i]->get_host_name(buff);
				fprintf(stderr," Whoops - Problem launching cpu pool command on host %s\n",buff);
			}
			count++;
		    }
		}
		if(count != get_num_cpus()) {
			fprintf(stderr," Whoops - Internal fault in cpu pool command creation.\n");
		}
	}
	return(pool);
}
	

//**************************************************************
//	Master Task Messages
Master_Task::Master_Task()
{
	task_id = pvm_parent();
	if(task_id == PvmNoParent) {
		fprintf(stderr,"Whoops -- No parent task for master.\n");
	}
}

Master_Task::~Master_Task()
{
	pvm_exit();
}

Task_Status Master_Task::get_status()
{
	int	local_status;

	local_status = pvm_pstat(task_id);
	if(pvm_parent() == PvmNoParent) {
                local_status = DIST_TASK_ERROR;
        } else if(local_status == PvmOk) {
		local_status = DIST_TASK_BUSY;
	} else if(local_status == PvmNoTask) {
		local_status = DIST_TASK_COMPLETE;
	} else if(local_status == PvmBadParam) {
		local_status = DIST_TASK_ERROR;
		fprintf(stderr," Whoops -- Bad task id in get master status\n");
	} else {
		local_status = DIST_TASK_ERROR;
	}
	return(local_status);
}

