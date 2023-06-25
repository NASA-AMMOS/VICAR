// Class PVM_Task_List
// Created by John Wright
// Created Fri Aug  6 07:54:08 1999


#ifndef _PVM_Task_List_H_
#define _PVM_Task_List_H_

#include "net/net_lib.h"

// ********************* List of Tasks Class ***********************
// List of heterogeneous tasks 
class PVM_Task_List {

    private:

    protected:

	Distributed_PVM_Task	**task_list;	// List of tasks within the class
	int		num_tasks;	// Number of tasks within the list
	char		*default_host_name;	// Name of host to run tasks on
	Network_Host	*default_host;		// Host object to run tasks on
	Network_Host	*temp_host;		// Host object to run tasks on
	int		send_msg_buff_id;	// pvm send buffer id for message passing

    public:

	int	get_num_tasks(void) { return(num_tasks); }

	int	set_default_host(char *def) {
		if(default_host_name) free(default_host_name);
		default_host_name = NULL;
		if(default_host) delete(default_host);
		default_host = NULL;
		if(def) {
			default_host_name = strdup(def);
			default_host = new Network_Host(def);
		}
		return(default_host->get_host_status() != HOST_UNAVAILABLE);
	}
	char	*get_default_host_name(void) { return(default_host_name); }

	int	add_task(char *task_string, char *host = NULL) {
		task_list = (Distributed_PVM_Task **)realloc(task_list,sizeof(Distributed_PVM_Task *) * (num_tasks+1));
		task_list[num_tasks] = new Distributed_PVM_Task();
		task_list[num_tasks]->parse_string(task_string);
		if(host) {
			temp_host = new Network_Host(host);
			if(temp_host->get_host_status() != HOST_UNAVAILABLE) {
				temp_host->run_command(task_list[num_tasks]);
				delete(temp_host);
			} else {
				fprintf(stderr,"Whoops - Unable to run commands on host %s\n",host);
			}
		} else if(default_host) {
			default_host->run_command(task_list[num_tasks]);
		} else {
			fprintf(stderr,"Whoops - Unable to run commands - No host specified\n");
			// Should probably change this to run on any host in network
			// except we don't have a pointer to the network object here
		}
		num_tasks++;
		return(num_tasks-1);
	}

	int	del_task(int indx) {
		if(task_list[indx]) {
			delete(task_list[indx]);
			task_list[indx] = NULL;
			return(TRUE);
		} else {
			return(FALSE);
		}
	}

	int	task_index_for_next_message() {	// -1 means none in list
		// get task id for first message in input queue
		int bufid = pvm_probe(-1, -1);	// probe for any message
		int bytes, msgtag, tid;
		int status = pvm_bufinfo(bufid, &bytes, &msgtag, &tid);
		// search list for matching id
		int i;
		for(i=0; i<num_tasks; i++) {
		    if(task_list[i]) {
			if(task_list[i]->get_task_id() == tid) {
				return(i);
			}
		    }
		}
		// return failed index
		return(-1);
	}
	Distributed_PVM_Task	*task_for_next_message() {	// NULL means none in list
			return(get_task(task_index_for_next_message()));
	}

	Distributed_PVM_Task *get_task(int index) {
		if(index >= 0 && index < num_tasks) {
			return((Distributed_PVM_Task *)task_list[index]);
		} else {
			return(NULL);
		}
	}
	Distributed_PVM_Task *operator()(int index) {
		return(get_task(index));
	}
	Distributed_PVM_Task *operator[](int index) {
		return(get_task(index));
	}

    // Message broadcast methods
	// standard procedure is to reset the buffer, put all
	// desired data into the buffer, then send the buffer
	// additional sensible option is sending an existing
	// buffer that might have been received from some
	// child task
	void	reset_buffer(int code = PvmDataDefault) {
			pvm_setsbuf(send_msg_buff_id);
			send_msg_buff_id = pvm_initsend(code);
		}
	int	send_buffer(int tag = 0) {
			int	i, ltasks=0, *task_ids;
			task_ids = (int *)malloc(num_tasks*sizeof(int));
			for(i=0; i<num_tasks; i++) {
				if(task_list[i]) {
					task_ids[ltasks] = task_list[i]->get_task_id();
					ltasks++;
				}
			}

			pvm_setsbuf(send_msg_buff_id);
			if(ltasks > 0) return(pvm_mcast(task_ids, ltasks ,tag));
			else return(PvmBadParam);
		}
	int	send_existing_buffer(int buff_id, int tag = 0) {
			int	i, ltasks=0, *task_ids;
			task_ids = (int *)malloc(num_tasks*sizeof(int));
			for(i=0; i<num_tasks; i++) {
				if(task_list[i]) {
					task_ids[ltasks] = task_list[i]->get_task_id();
					ltasks++;
				}
			}

			pvm_setsbuf(buff_id);
			if(ltasks > 0) return(pvm_mcast(task_ids, ltasks ,tag));
			else return(PvmBadParam);
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


    // Constructors

	PVM_Task_List(int packmode=PvmDataDefault) {
		task_list = NULL;
		default_host = NULL;
		default_host_name = NULL;
		num_tasks = 0;
		send_msg_buff_id = pvm_mkbuf(packmode);
	}

    // Destructor

	~PVM_Task_List() {
		if(default_host) delete(default_host);
		if(default_host_name) free(default_host_name);
		// halt and clean up all tasks in list
		while(num_tasks > 0) {
			if(task_list[num_tasks-1]) delete(task_list[num_tasks-1]);
			num_tasks--;
		}
		// free the task list
		if(task_list) free(task_list);
		pvm_freebuf(send_msg_buff_id);
	}

};


#endif
