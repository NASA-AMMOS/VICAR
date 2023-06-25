#ifndef _SLAVES_H_
#define _SLAVES_H_
// slaves.h 1.2 02/07/10 15:38:57
/** \file
// Slave task pool for SUMMITT Terrain merge master.
//
// This object tracks PVM slave tasks, and parallels a
// net_lib Pool_Of_Tasks.
**/

#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include "net/net_lib.h"
#include "summitt_func.h"
#include "tp_req.h"

class Slave {
public:
	enum { SL_DEAD, SL_STARTING, SL_IDLE, SL_BUSY } state;
	int tid;			///< PVM task ID
	Distributed_PVM_Task *task;	///< net pool task
	TP_Req *request;		///< current terrain processing request

#ifdef TBD
	// which models it has loaded?
#endif
};

class Slave_Pool {

public:
	Slave *list;		///< allocated list of slaves

	int num()		{ return num_slave; }

	/// lookup entry from PVM task ID
	Slave *find(int tid) {
		Slave *s = list;
		for (int i=0; i<num_slave; i++, s++) {
			if (s->tid == tid)
				return s;
		}
		return NULL;
	}

	/// Count number of idle slaves
	int num_idle()		{
		int n = 0;
		for (int i=0; i<num_slave; i++) {
			if (list[i].state == Slave::SL_IDLE)
				n++;
		}
		return n;
	}

	/// Count number of busy slaves
	int num_busy()		{
		int n = 0;
		for (int i=0; i<num_slave; i++) {
			if (list[i].state == Slave::SL_BUSY)
				n++;
		}
		return n;
	}

	/// Count number of live slaves
	int num_alive()		{
		int n = 0;
		for (int i=0; i<num_slave; i++) {
			if (list[i].state != Slave::SL_DEAD)
				n++;
		}
		return n;
	}

	/// diagnostic dump of slave status
	void dump(FILE *fp) {
		const char *state_name[] = { 
			"DEAD", "STARTING", "IDLE", "BUSY" };
		fprintf(stderr, "%d slave(s) spawned\n", num_slave);
		Slave *s = list;
		for (int i=0; i<num_slave; i++, s++) {
			fprintf(stderr, "%2d: tid %X state %s\n", 
				i, s->tid, state_name[s->state]);
			if (s->state == Slave::SL_BUSY)
				fprintf(stderr, "  req: %s\n", 
					s->request->name);
		}
	}

	/// constructor
	Slave_Pool(int n) 	{
		list = new Slave[num_slave = n];
	}
	
	~Slave_Pool()		{ delete [] list; }
	
private:
	int num_slave;		///< allocated size
};
#endif
