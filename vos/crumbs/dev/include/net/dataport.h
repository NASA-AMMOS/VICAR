// dataport.h 1.1 01/10/11 13:28:11
// PVM dataport
// This is included from "../dataport.h" if PVM_DATAPORT is defined

#ifndef _DATAPORT_H_
#define PVM_DATAPORT
#include "../dataport.h"
#endif

#ifndef	_PVM_DATAPORT_H_
#define _PVM_DATAPORT_H_

#include "net/net_lib.h"

class PVM_Dataport : public Dataport {

    private:

    protected:

	Message_Passing_Task	*pvm_task;

	void	init() { ; }

    public:

	virtual	int	open(FILE *) { 
		if(pvm_task) {
			pvm_task->reset_buffer();
			return(TRUE); 
		} else {
			return(FALSE); 
		}
	}
	virtual	int	open(const char *) { 
		if(pvm_task) {
			pvm_task->reset_buffer();
			return(TRUE); 
		} else {
			return(FALSE); 
		}
	}
	virtual	int	ropen(const char *) { 
		if(pvm_task) {
			pvm_task->recv_buffer();
			return(TRUE); 
		} else {
			return(FALSE); 
		}
	}
	virtual	int	wopen(const char *) { 
		if(pvm_task) {
			pvm_task->reset_buffer();
			return(TRUE); 
		} else {
			return(FALSE); 
		}
	}
	virtual	int	rwopen(const char *) { 
		return(FALSE); 
	}
	virtual	void	close(void) { 
		if(pvm_task) {
			flush();
		}
	}
	virtual	void	flush(int msg_type=0) { 
		if(pvm_task) {
			pvm_task->send_buffer(msg_type);
			pvm_task->reset_buffer();
		}
	}
	virtual int	get_next_string(char *);
	virtual int	put_string(const char *);
	virtual	int	pgetc(void) { return(FALSE); }
	virtual	int	get_type(void) { return(PVM_PORT); }

	void	set_task(Message_Passing_Task *tsk) {
		pvm_task = tsk;
	}
	Message_Passing_Task	*get_task(void) { return(pvm_task); }
	void	delete_task(void) {
		if(pvm_task)delete(pvm_task);
		pvm_task = NULL;
	}

	int	write(void *, unsigned) { return -1; }
	int	read(void *, unsigned) { return -1; }

	PVM_Dataport(Message_Passing_Task *tsk=NULL) { 
		init(); 
		pvm_task = tsk;
	}
};
#endif
