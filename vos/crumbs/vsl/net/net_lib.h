// net_lib.h
//
// Written by John Wright for distributed processing
//
// Date created:	08/03/94
//
// Last modified:	11/06/94
//
// Modification history:
//	11/06/94	Implementation of message passing

#ifndef _NET_DIST_H_
#define _NET_DIST_H_

#include <malloc.h>
#include <string.h>
#include <pvm3.h>

#ifndef TRUE
#define TRUE	1
#define FALSE	0
#endif

typedef int	Task_Status;

#define	DIST_TASK_BUSY		-1
#define	DIST_TASK_COMPLETE	-2
#define	DIST_TASK_IDLE		-3
#define	DIST_TASK_ERROR		-4

typedef int	Host_Status;

#define	HOST_IDLE		0
#define	HOST_BUSY		1
#define	HOST_FULL		2
#define	HOST_UNAVAILABLE	4
#define	HOST_LOCKED		8

#define	NOT_USE_HOST		0
#define	USE_HOST		-1

typedef	int	Net_Status;

#define	NET_READY		0
#define	NET_EMPTY		-1
#define	NET_OFFLINE		-2

typedef int	Pool_Status;

#define POOL_BUSY		-1
#define POOL_COMPLETE		-2
#define POOL_IDLE		-3



// *********************** Base Class - Generic Distributed Task ****
// Encapsulates pvm variables necessary for communicating with and
// controlling the task

class Generic_Distributed_Task {

// friend classes declared only to allow access to task_id field
	friend Network;
	friend Network_Host;
	friend Generic_Pool_Of_Tasks;

    protected:

	void    init();

	int	task_id;			// pvm task id number
	void	set_task_id(int tid) {task_id = tid;}

    private:

    public:

	virtual	Task_Status get_status();
	Task_Status wait_for_completion() 
		{ while(get_status() == DIST_TASK_BUSY); return(get_status()); }

			Generic_Distributed_Task();
			~Generic_Distributed_Task();
};

// ********************* Base Class for Message Passing Tasks **
// 

class Message_Passing_Task : virtual public Generic_Distributed_Task {

    protected:

        void    init();

	int	send_msg_buff_id;		// pvm send buffer id for message passing
	int	recv_msg_buff_id;		// pvm receive buffer id for message passing

    private:


    public:

	void	reset_buffer(int code = PvmDataDefault) {
			pvm_setsbuf(send_msg_buff_id);
			send_msg_buff_id = pvm_initsend(code);
		}
	int	send_buffer(int tag = 0) {
			pvm_setsbuf(send_msg_buff_id);
			return(pvm_send(task_id ,tag));
		}
	int	send_this(void *mesg, int mesg_size, int count=1, int tag = 0) {
			return(pvm_psend(task_id, tag, mesg, mesg_size*count,
				PVM_BYTE));
		}
	int	send_itself(Message_Passing_Task *dest) {
			dest->reset_buffer();
			dest->put_in_buffer(task_id);
			return(dest->send_buffer());
		}
	int	put_in_buffer(char cp) {
			pvm_setsbuf(send_msg_buff_id);
			return(pvm_pkbyte(&cp, 1, 1));}
	int	put_in_buffer(short cp) {
			pvm_setsbuf(send_msg_buff_id);
			return(pvm_pkshort(&cp, 1, 1));}
	int	put_in_buffer(int cp) {
			pvm_setsbuf(send_msg_buff_id);
			return(pvm_pkint(&cp, 1, 1));}
	int	put_in_buffer(long cp) {
			pvm_setsbuf(send_msg_buff_id);
			return(pvm_pklong(&cp, 1, 1));}
	int	put_in_buffer(float cp) {
			pvm_setsbuf(send_msg_buff_id);
			return(pvm_pkfloat(&cp, 1, 1));}
	int	put_in_buffer(double cp) {
			pvm_setsbuf(send_msg_buff_id);
			return(pvm_pkdouble(&cp, 1, 1));}
	int	put_in_buffer(unsigned char cp) {
			pvm_setsbuf(send_msg_buff_id);
			return(pvm_pkbyte((char *)&cp, 1, 1));}
	int	put_in_buffer(unsigned short cp) {
			pvm_setsbuf(send_msg_buff_id);
			return(pvm_pkushort(&cp, 1, 1));}
	int	put_in_buffer(unsigned int cp) {
			pvm_setsbuf(send_msg_buff_id);
			return(pvm_pkuint(&cp, 1, 1));}
	int	put_in_buffer(unsigned long cp) {
			pvm_setsbuf(send_msg_buff_id);
			return(pvm_pkulong(&cp, 1, 1));}
	int	put_in_buffer(char *cp) {
			pvm_setsbuf(send_msg_buff_id);
			return(pvm_pkstr(cp));}
	int	put_in_buffer(char *cp, int count) {
			pvm_setsbuf(send_msg_buff_id);
			return(pvm_pkbyte(cp, count, 1));}
	int	put_in_buffer(short *cp, int count=1) {
			pvm_setsbuf(send_msg_buff_id);
			return(pvm_pkshort(cp, count, 1));}
	int	put_in_buffer(int *cp, int count=1) {
			pvm_setsbuf(send_msg_buff_id);
			return(pvm_pkint(cp, count, 1));}
	int	put_in_buffer(long *cp, int count=1) {
			pvm_setsbuf(send_msg_buff_id);
			return(pvm_pklong(cp, count, 1));}
	int	put_in_buffer(float *cp, int count=1) {
			pvm_setsbuf(send_msg_buff_id);
			return(pvm_pkfloat(cp, count, 1));}
	int	put_in_buffer(double *cp, int count=1) {
			pvm_setsbuf(send_msg_buff_id);
			return(pvm_pkdouble(cp, count, 1));}
	int	put_in_buffer(unsigned char *cp, int count=1) {
			pvm_setsbuf(send_msg_buff_id);
			return(pvm_pkbyte((char *)cp, count, 1));}
	int	put_in_buffer(unsigned short *cp, int count=1) {
			pvm_setsbuf(send_msg_buff_id);
			return(pvm_pkushort(cp, count, 1));}
	int	put_in_buffer(unsigned int *cp, int count=1) {
			pvm_setsbuf(send_msg_buff_id);
			return(pvm_pkuint(cp, count, 1));}
	int	put_in_buffer(unsigned long *cp, int count=1) {
			pvm_setsbuf(send_msg_buff_id);
			return(pvm_pkulong(cp, count, 1));}

	int	message_arrived(int tag = -1) {return(pvm_probe(task_id ,tag));}
	int	recv_buffer(int tag = -1) {
			pvm_setrbuf(recv_msg_buff_id);
			recv_msg_buff_id = pvm_recv(task_id ,tag);
			pvm_setrbuf(0);
			return(recv_msg_buff_id);
		}
	int	recv_this(void *mesg, int mesg_size, int count=1, int tag = -1) {
			return(pvm_precv(task_id, tag, mesg, mesg_size*count,
				PVM_BYTE, &count, &tag, &mesg_size));
		}
	void	mesg_info(int *bytes, int *tag, int *tid) {
			pvm_bufinfo(recv_msg_buff_id, bytes, tag, tid);
		}
	int	mesg_length(void) {
			int bytes, tag, tid;
			mesg_info(&bytes, &tag, &tid);
			return(bytes);
		}
	int	mesg_tag(void) {
			int bytes, tag, tid;
			mesg_info(&bytes, &tag, &tid);
			return(tag);
		}
	int	mesg_tid(void) {
			int bytes, tag, tid;
			mesg_info(&bytes, &tag, &tid);
			return(tid);
		}
	int	recv_itself(Message_Passing_Task *src) {
			int status;
			status = src->recv_buffer();
			src->get_from_buffer(&task_id);
			return(status);
		}
	int get_from_buffer(char *cp, int count=1) {
		pvm_setrbuf(recv_msg_buff_id);
		return(pvm_upkbyte(cp, count, 1));}
	int get_from_buffer(short *cp, int count=1) {
		pvm_setrbuf(recv_msg_buff_id);
		return(pvm_upkshort(cp, count, 1));}
	int get_from_buffer(int *cp, int count=1) {
		pvm_setrbuf(recv_msg_buff_id);
		return(pvm_upkint(cp, count, 1));}
	int get_from_buffer(long *cp, int count=1) {
		pvm_setrbuf(recv_msg_buff_id);
		return(pvm_upklong(cp, count, 1));}
	int get_from_buffer(float *cp, int count=1) {
		pvm_setrbuf(recv_msg_buff_id);
		return(pvm_upkfloat(cp, count, 1));}
	int get_from_buffer(double *cp, int count=1) {
		pvm_setrbuf(recv_msg_buff_id);
		return(pvm_upkdouble(cp, count, 1));}
	int get_from_buffer(unsigned char *cp, int count=1) {
		pvm_setrbuf(recv_msg_buff_id);
		return(pvm_upkbyte((char *)cp, count, 1));}
	int get_from_buffer(unsigned short *cp, int count=1) {
		pvm_setrbuf(recv_msg_buff_id);
		return(pvm_upkushort(cp, count, 1));}
	int get_from_buffer(unsigned int *cp, int count=1) {
		pvm_setrbuf(recv_msg_buff_id);
		return(pvm_upkuint(cp, count, 1));}
	int get_from_buffer(unsigned long *cp, int count=1) {
		pvm_setrbuf(recv_msg_buff_id);
		return(pvm_upkulong(cp, count, 1));}
	int get_from_buffer(char **cp) {
		int bytes, msgtag, tid;
		pvm_setrbuf(recv_msg_buff_id);
		if(!pvm_bufinfo(recv_msg_buff_id, &bytes, &msgtag, &tid)) {
			*cp = (char *)malloc(bytes);
			return(pvm_upkstr(*cp));
		} else {
			return(-1);
		}
	}

	Message_Passing_Task(int packmode=PvmDataDefault);
	~Message_Passing_Task();

};

// ********************* Child Task Class **********************
// Task which is spawned by parent

class Child_Task : virtual public Generic_Distributed_Task {

    protected:

        void    init();

	char	*task_string;			// Pointer to command string
	char	**args_string;			// Pointer to array of argument strings

    private:

        void print_whoops() {
                fprintf(stderr,"Whoops -- Distributed_Command cannot perform message passing.\n");
        }

    public:

	void	set_task_string(char *user_task_string);
	void	set_args_string(char **user_args_string);
	void	parse_string(char *user_task_string);
	void	parse_args_string(char *user_args_string);
	char	*get_task_string() {return(task_string);}
	char	**get_args_string() {return(args_string);}

	void	*data;				// User storage space
	void	(*pre_notify_func)(Child_Task *, void *);	// Function to be called before task is run
	void	(*post_notify_func)(Child_Task *, void *, Task_Status);	// Function to be called after task is run

	Task_Status	kill_task();

	~Child_Task();
	Child_Task();

};


// ********************* Shell Command Level Task Class ********
// Task which executes a C-shell executable command

class Distributed_Command : public Child_Task {

    protected:

        void    init();

    private:

    public:

	void	set_task_string(char *user_task_string);
	void	set_args_string(char **user_args_string);
	char	*get_task_string() {return(args_string[0]);}
	char	**get_args_string() {return(&args_string[1]);}

		Distributed_Command();
		~Distributed_Command();

};

// ********************* Distributed Task with PVM class ***********
// Task which executes a single executable file and may
// perform message passing

class Distributed_PVM_Task : public Message_Passing_Task , public Child_Task { 

    protected:

        void    init();

    private:


    public:

	Distributed_PVM_Task() { }


};


//********************** Master Class for slave processes *******

class Master_Task : public Message_Passing_Task {

    public:
	Task_Status get_status();

		Master_Task();
		~Master_Task();

};



// ********************* Generic Pool of Tasks Class ***********************
// Base Class for pools of tasks to be executed in parallel across pvm net
// implemented as an array of tasks

class Generic_Pool_Of_Tasks {

    protected:

        void    init();

	int				num_tasks;	// Number of tasks in pool
	int				next_task;	// Next task to be executed
	float				tasks_per_cpu;	// Number of tasks each cpu can handle
	Child_Task		**task_list;	// Array of distributed tasks

    private:


    public:

	int  get_num_tasks()		{return(num_tasks);}
	void reset_task_list();
	float get_tasks_per_cpu()	{return(tasks_per_cpu);}
	void  set_tasks_per_cpu(float tpcpu)	{tasks_per_cpu = tpcpu;}

	void	*data;					// User storage space
	void	(*notify_func)(Generic_Pool_Of_Tasks *, void *);	// Function to be called before pool is run

	Child_Task *get_task(int index) {
		if(index >= 0 && index < num_tasks) {
			return((Child_Task *)task_list[index]);
		} else {
			return(NULL);
		}
	}
	Child_Task *operator()(int index) {
		return(get_task(index));
	}
	Child_Task *operator[](int index) {
		return(get_task(index));
	}
	Child_Task *get_next_task() {
		if(next_task >= 0 && next_task < num_tasks) {
			return(get_task(next_task++));
		} else {
			return(NULL);
		}
	}
	void backup_one_task() { if(next_task > 0) next_task--;}
	Pool_Status get_pool_status();
	
	Generic_Pool_Of_Tasks::Generic_Pool_Of_Tasks(void) {
		notify_func = NULL;
		data = NULL;
		num_tasks = 0;
		next_task = 0;
		tasks_per_cpu = 1.0;
		task_list = NULL;
	}
};


// ********************* Pool of Commands Class ***********************
// Pool of C-shell executable commands

class Pool_Of_Commands : public Generic_Pool_Of_Tasks {

    protected:

        void    init(int num_tasks);

    private:


    public:

	void	set_num_tasks(int task_count);

	Distributed_Command *get_task(int index) {
		if(index >= 0 && index < num_tasks) {
			return((Distributed_Command *)task_list[index]);
		} else {
			return(NULL);
		}
	}
	Distributed_Command *operator()(int index) {
		return(get_task(index));
	}
	Distributed_Command *operator[](int index) {
		return(get_task(index));
	}
	Distributed_Command *get_next_task() {
		if(next_task >= 0 && next_task < num_tasks) {
			return(get_task(next_task++));
		} else {
			return(NULL);
		}
	}

	Pool_Of_Commands(int task_count = 0);
	~Pool_Of_Commands();

};


// ********************* Pool of Tasks Class ***********************
// Pool of message passing tasks

class Pool_Of_Tasks : public Generic_Pool_Of_Tasks {

    protected:

        void    init(int num_tasks);

    private:


    public:

	void	set_num_tasks(int task_count);

	Distributed_PVM_Task *get_task(int index) {
		if(index >= 0 && index < num_tasks) {
			return((Distributed_PVM_Task *)task_list[index]);
		} else {
			return(NULL);
		}
	}
	Distributed_PVM_Task *operator()(int index) ;
	Distributed_PVM_Task *operator[](int index) {
		return(get_task(index));
	}
	Distributed_PVM_Task *get_next_task() {
		if(next_task >= 0 && next_task < num_tasks) {
			return(get_task(next_task++));
		} else {
			return(NULL);
		}
	}

	Pool_Of_Tasks(int task_count = 0);
	~Pool_Of_Tasks();

};



// ********************* Network Host Class ***********************
// Class representing an individual host in pvm net

class Network_Host {

    protected:

        void    init();

    private:

	char		*name;			// Host name
	char		*architecture;		// pvm identified host architecture
	Host_Status	host_status;		// Last status returned by get status funcs
	int		current_task_id;	// id of task currently running
	int		master_task_id;		// id of parent task
	int		demon_task_id;		// id of pvm demon
	int		speed;			// speed value used for load balancing
	int		num_cpus;		// number of cpus in host for load balance

    public:

	void get_host_name(char *buffer) {strcpy(buffer, name);}
	void get_host_architecture(char *buffer) {strcpy(buffer, architecture);}
	int  get_host_speed() { return(speed); }
	int  get_num_cpus() {return(num_cpus);}
	void  set_num_cpus(int num) { num_cpus = num;}
	Host_Status  host_is_local() 
		{return(demon_task_id == pvm_tidtohost(master_task_id));}
	Host_Status get_host_status(float scale = 1.0);
	Task_Status run_command(Child_Task *dc);
	Host_Status lock_host();
	Host_Status unlock_host();
	void get_lock_holder(char *buffer);

	Network_Host(struct pvmhostinfo *host_ptr);
	Network_Host(char *hostname);
	~Network_Host();

};

// ********************* Network Class ****************************

class Network {

    protected:

        void    init(int use_host);

    private:

	Network_Host	**host_list;
	int		control_flag;
	char		*control_string;
	int		current_task_id;
	int		master_task_id;
	int		number_of_hosts;
	int		auto_started;

    public:

	Task_Status run_command(Child_Task *dt);
	Task_Status run_command(Child_Task dt);

	void run_pool_blocked(Generic_Pool_Of_Tasks *dpt);
	void run_pool_blasted(Generic_Pool_Of_Tasks *dpt);
	void run_pool_start(Generic_Pool_Of_Tasks *dpt);
	void run_pool_continue(Generic_Pool_Of_Tasks *dpt);
	void pool_recover_faults(Generic_Pool_Of_Tasks *dpt);

	Pool_Of_Tasks *create_host_pool_of_tasks(char *buff=NULL);
	Pool_Of_Commands *create_host_pool_of_commands(char *buff=NULL);
	Pool_Of_Tasks *create_cpu_pool_of_tasks(char *buff=NULL);
	Pool_Of_Commands *create_cpu_pool_of_commands(char *buff=NULL);

	Net_Status get_net_status();
	void set_control_flag(int cf, char *cs);
	int get_num_hosts(void) { return(number_of_hosts); }
	int get_num_cpus(void) {
		int i, count=0;
		for(i=0; i<number_of_hosts; i++) {
			count += host_list[i]->get_num_cpus();
		}
		return(count);
	}

	Network(int cf, char *cs, int use_host = USE_HOST);
	Network(int use_host = USE_HOST);
	~Network();

};


#endif
