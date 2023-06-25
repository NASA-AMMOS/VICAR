#ifndef TP_REQ_H
#define TP_REQ_H
// tp_req.h 1.4 03/03/26 15:41:29
/** \file
// Terrain processing request queue. Elements track the processing
// of one terrain wedge.
//
// Example usage:
//	TPR_Queue q;
//
//	q.put(name, &model);
//
//	TP_Req *r = q.get();
//	if (r) {
//		process(r->name(), r->model());
//		delete r;
//	}
*/

#include <stdio.h>
#include <string.h>
#include "patch.h"

class TP_Req {				///< one request
private:
	TP_Req *next;			///< list linkage

public:
	char name[1024];		///< working pathname
	Patch *patch;			///< terrain patch

	/// constructor
	TP_Req(const char *wname, Patch *p) {
		strncpy(name, wname, sizeof(name));
		patch = p;
	}

	friend class TPR_Queue;
};

class TPR_Queue {			///< queue of requests
private:
	TP_Req *head;
	TP_Req *tail;

public:
	TPR_Queue() : head(NULL) {};	///< constructor - start empty

	void put(TP_Req *req) {		///< store to tail of queue
		if (head == NULL)	// was empty
			head = req;
		else
			tail->next = req;
		tail = req;
		req->next = NULL;
	}

	void put(const char *wname, Patch *p) {
		put(new TP_Req(wname, p));
	}

	TP_Req *get() {			///< remove from head of queue
		TP_Req *req = head;
		if (req)
			head = req->next;
		return req;
	}

	int empty() { return head == NULL; }

	void dump(FILE *fp) {		///< debug
		if (empty()) {
			fprintf(fp, "Request queue empty\n");
		} else {
			fprintf(fp, "Request queue, oldest to newest:\n");
			TP_Req *r = head;
			while (r) {
				fprintf(fp, "%s\n", r->name);
				r = r->next;
			}
		}
	}
};

#endif
