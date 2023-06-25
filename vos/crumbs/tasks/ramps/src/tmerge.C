// tmerge.C 1.5 03/08/28 12:53:51
/** \file
// SUMMITT Terrain merge master (client)
//
// This program supervises parallel SUMMITT processing of new terrain data,
// dispatching requests to slave PVM tasks. It is invoked at command level
// as each new range map is produced. There is no prioritization of requests,
// and this pipeline doesn't handle product (e.g. ASD model) generation.
//
// This file contains the top-level control logic for the program.
//
// If a master is already running, the command-line request is 
// forwarded to the existing master as a PVM message. Otherwise, 
// this invocation becomes the master itself, and spawns slave tasks.
// (The terrain merge master PVM task ID is published in the PVM database.)
//
// The master accepts PVM requests to process new range data,
// dispatching the request to an idle slave (and queueing requests
// if no slave is idle). Slave tasks are responsible for
//	- converting the XYZ range map in site frame to an octree in 
//		camera frame
//	- matching the octree to an existing site forest (producing revised
//		camera position and pointing data)
//	- generating a multiresolution VST mesh
// Once processing has completed, the slave sends results back to the
// master.
//
// The master also transfers files between an external (OSS) file system 
// and shared local disk space for access by slaves, unless no local path
// is specified (in particular, for a single-CPU configuration).
//
// This program can also be invoked to terminate SUMMITT processing,
// either cleanly (waiting until all previous requests have completed)
// or immediately.
//
// Configuration options for SUMMITT processing are stored in file
// ~/.summitt, or in the file named in environment variable SUMMITT_CFG.
// (see config.C). Command-line options override configuration options.
*/

static const char usage[] = 
"usage:\n"
"tmerge [options] -r <xyz-file> -i <img-file> [-c <cmod-file>]\n"
"  Process terrain data in range map xyz-file with corresponding\n"
"  image img-file. Camera model is in cmod-file or xyz-file header.\n"
"tmerge [options] [-nowait] finish\n"
"  Terminate all SUMMITT processes once all previous requests\n"
"  have completed. Unless -nowait is specified, tmerge does not\n"
"  return until processing has completed.\n"
"tmerge [options] abort\n"
"  Terminate all SUMMITT processes immediately.\n"
"tmerge [options] status\n"
"  Ask master to dump status to stderr.\n"
"options:\n"
"  -v = verbose output to stderr\n"
"  -svf   <name> = pathname for preprocessed site vector file\n"
"  -opath <name> = base path for output files\n"
"  -tpath <name> = base path (shared local disk) for temporary files\n"
"  -xpath <name> = executable pathname for tmslave program\n";

#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include "net/net_lib.h"
#include "summitt_func.h"
#include "tm_msg.h"
#include "tp_req.h"
#include "slaves.h"

#define MAX_PATH 1024

// basic master program state
static enum { RUNNING, FINISHING, DONE } mpstate = RUNNING;

// request, arguments from command line/config file/peer message
static enum { REQ_XYZ, REQ_FINISH, REQ_ABORT, REQ_STATUS } request = REQ_XYZ;
static char *xyz_file, *img_file, *cmod_file, *svf_file;
static int op_nowait;		// don't wait for reply to "finish"?
char *base_path = ".";		// base SUMMITT mission path in OSS file sys
char *temp_path;		// base temporary path in shared local file sys
char *xpath = "tmslave";	// executable pathname for slave PVM task

extern int verbose;

static Pool_Of_Tasks *pool;	// net_lib pool of slave PVM tasks
static Slave_Pool *spool;	// additional tracking info

static TPR_Queue req_que;	// terrain processing request queue

static void dispatch_xyz_request(TP_Req *req);

// support functions in tmproc.C
void 	master_setup(const char *svf_file);
TP_Req *setup_request(char *xyz, char *img, char *cmod);
void    finish_request(TP_Req *req, int status, double posn[3], double rot[3],
			Summitt_range *bvol, double obj_res);

// Perform the peer role, sending request to master and possibly
// waiting for reply. Return 0 if okay.
static int peer_request(int mtid)
{
	Peer_Task master(mtid);		// connection to master

	if (verbose)
		fprintf(stderr, "Peer sending request %d to master TID %X\n",
			request, mtid);

	switch (request) {
	case REQ_XYZ:
		if (psend_master_request(&master, xyz_file, img_file, 
							cmod_file) < 0) {
			fprintf(stderr, "Can't send XYZ request to master\n");
			return 1;
		}
		return 0;
	case REQ_FINISH:
		if (psend_finish(&master) < 0) {
			fprintf(stderr, "Can't send FINISH to master\n");
			return 1;
		}
		if (!op_nowait) 	// wait for reply from master
			master.recv_buffer();
		return 0;
	case REQ_ABORT:
		if (psend_abort(&master) < 0) {
			fprintf(stderr, "Can't send ABORT to master\n");
			return 1;
		}
		return 0;
	case REQ_STATUS:
		if (psend_status(&master) < 0) {
			fprintf(stderr, "Can't send STATUS to master\n");
			return 1;
		}
		return 0;
	}

	fprintf(stderr, "Peer invalid request %d\n", request);
	return 1;
}

// Queue or dispatch a patch processing request
static void submit_request(TP_Req *req)
{
	if (spool->num_idle() > 0)
		dispatch_xyz_request(req);
	else
		req_que.put(req);
}

// Communication failure with slave, mark it as dead and resubmit
// the request it was supposed to handle
static void slave_died(Slave *s)
{
	s->state = Slave::SL_DEAD;
	if (spool->num_alive() < 1) {
		fprintf(stderr, "No more slaves left standing!\n");
		exit(1);
	}
	submit_request(s->request);
}

// Dispatch request to process new patch (XYZ range data) to a slave.
// Request data has been set up, and we know there's at least one idle slave.
static void dispatch_xyz_request(TP_Req *req)
{
	// choose an appropriate idle slave
	// ** for now, just choose first idle slave
	Slave *s = spool->list;
	for (int i=0; i<spool->num(); i++, s++) {
		if (s->state == Slave::SL_IDLE)
			break;
	}

	if (verbose)
		fprintf(stderr, "Dispatching request %s to slave tid %X\n",
			req->name, s->tid);
	s->request = req;
	s->state = Slave::SL_BUSY;
	if (psend_slave_request(s->task, req->name, &req->patch->cmod,
					req->patch->site_vector) < 0) {
		fprintf(stderr, "Can't send XYZ request to slave tid %X\n",
			s->tid);
		slave_died(s);
	}
}

// A slave task is idle (ready for the first time, or finished a wedge).
// Give it the next queued request if one is ready.
static void slave_idle(int tid)
{
	Slave *s = spool->find(tid);
	if (s == NULL)			// oops, unknown task
		return;
	
	s->state = Slave::SL_IDLE;

	// if any requests are queued, dispatch the oldest one
	TP_Req *req = req_que.get();
	if (req)
		dispatch_xyz_request(req);

	// none queued, see if we're waiting to finish up
	else if (mpstate == FINISHING && spool->num_busy() == 0)
		mpstate = DONE;
}
	
// Handle request from peer to process new XYZ range map
static void msg_xyz_request(int tid)
{
	char xyz[MAX_PATH], img[MAX_PATH], cmod[MAX_PATH];
	if (precv_master_request(xyz, img, cmod) < 0) {
		fprintf(stderr, "Invalid XYZ request from peer %X\n", tid);
		return;
	}

	TP_Req *req = setup_request(xyz, img, cmod);
	if (req == NULL)	// ignore failed request
		return;

	// if at least one slave is ready, dispatch request
	if (spool->num_idle() > 0)
		dispatch_xyz_request(req);

	else	// queue to wait for an idle slave
		req_que.put(req);
}

// Handle notification from slave that terrain request is done
static void msg_xyz_done(int tid)
{
	Slave *s = spool->find(tid);
	if (s == NULL)			// oops, unknown task
		return;

	// check that we know what he was working on
	if (s->state != Slave::SL_BUSY) {
		fprintf(stderr, "tmerge: slave task %X done but not busy?\n",
			tid);
	} else {
		if (verbose)
			fprintf(stderr, "Slave task %X finished request\n",
				tid);

		double posn[3], rot[3], res;
		Summitt_range bvol;
		int status;
		char name[MAX_PATH];
		if (precv_terrain_done(name, &status, posn, rot, 
							&bvol, &res) < 0) {
			fprintf(stderr, "Can't get result from slave %X\n", 
				tid);
			slave_died(s);
			
		// sanity check
		} else if (strcmp(name, s->request->name)) {
			fprintf(stderr, "Whoops - slave says he finished %s,"
				" I thought he was doing %s!\n",
				name, s->request->name);
			slave_died(s);

		} else {
			finish_request(s->request, status, posn, rot, 
							&bvol, res);
			delete s->request;	// free old request info
			s->request = NULL;
		}
	}
	
	slave_idle(tid);		// slave is idle now
}

// Try to become the master by storing my task ID in the PVM database.
// If fails (presumably because another task is already the master),
// return the existing master's task ID. Otherwise, return -1.
static int master_tid()
{
	int mtid = pvm_mytid();

	// try to store my task id as master
	pvm_initsend(PvmDataRaw);
	pvm_pkint(&mtid, 1, 1);
	int status = pvm_putinfo("TM_Master", pvm_getsbuf(), PvmMboxDefault);
	if (status >= 0)	// successful, we are the new master
		return -1;

	// failed, should mean a master is already there; get his task ID
	int bufid = pvm_recvinfo("TM_Master", 0, PvmMboxFirstAvail);
	if (bufid < 0) {
		fprintf(stderr, "Master task lookup failed, status=%d\n", 
			bufid);
		exit(1);
	}
	pvm_setrbuf(bufid);
	int id;
	pvm_upkint(&id, 1, 1);
	return id;
}

// Initialization as new master task
static void master_init(Network *net)
{
	if (verbose)
		fprintf(stderr, "Initializing master task,"
			" %d hosts, %d cpus\n", net->get_num_hosts(),
			net->get_num_cpus());

	// load forest and update shared local copies
	master_setup(svf_file);
	
	// Put first request, from command line, into processing queue.
	// First slave to be ready will get this
	TP_Req *req = setup_request(xyz_file, img_file, cmod_file);
	if (req)	// make sure setup was successful
		req_que.put(req);
	
	// spawn PVM slave tasks
	char cmdline[2*MAX_PATH];
	// get slave configuration options
	char *sarg[100];
	int ncfg = summitt_config("tmslave", sarg, 100);
	strcpy(cmdline, xpath);
	int i;
	for (i=0; i<ncfg; i++) {	// ** no check for buffer overflow
		strcat(cmdline, " ");
		strcat(cmdline, sarg[i]);
		free(sarg[i]);
	}
	if (verbose)
		fprintf(stderr, "Slave cmd line: [%s]\n", cmdline);
	pool = net->create_cpu_pool_of_tasks(cmdline);

	// initialize slave data structure
	spool = new Slave_Pool(pool->get_num_tasks());
	Slave *s = spool->list;
	for (i=0; i<spool->num(); i++, s++) {
		s->task = pool->get_task(i);
		s->tid = s->task->get_task_id();
		s->state = Slave::SL_STARTING;
		// ** more TBD
	}
}

// Extract and validate argument list
static void parse_args(int argc, char **argv)
{
	for (int i=0; i<argc; i++) {
		if (!strcmp(argv[i], "-nowait"))
			op_nowait = TRUE;
		else if (!strcmp(argv[i], "-v"))
			verbose = TRUE;
		else if (!strcmp(argv[i], "-r") && i < argc-1)
			xyz_file = argv[++i];
		else if (!strcmp(argv[i], "-i") && i < argc-1)
			img_file = argv[++i];
		else if (!strcmp(argv[i], "-c") && i < argc-1)
			cmod_file = argv[++i];
		else if (!strcmp(argv[i], "-opath") && i < argc-1)
			base_path = argv[++i];
		else if (!strcmp(argv[i], "-tpath") && i < argc-1)
			temp_path = argv[++i];
		else if (!strcmp(argv[i], "-xpath") && i < argc-1)
			xpath = argv[++i];
		else if (!strcmp(argv[i], "-svf") && i < argc-1)
			svf_file = argv[++i];
		else if (!strcmp(argv[i], "finish"))
			request = REQ_FINISH;
		else if (!strcmp(argv[i], "abort"))
			request = REQ_ABORT;
		else if (!strcmp(argv[i], "status"))
			request = REQ_STATUS;
		else {
			fprintf(stderr, "Unknown argument %s\n", argv[i]);
			fprintf(stderr, usage);
			exit(1);
		}
	}
}

// Get my program arguments. First check config file, then do
// command line (so command line has priority).
static void get_args(int argc, char **argv)
{
	char *cfgarg[100];
	int ncfg = summitt_config("tmerge", cfgarg, 100);
	parse_args(ncfg, cfgarg);
	
	parse_args(argc-1, argv+1);	// (skip program name)
	
	if (request == REQ_XYZ && (xyz_file==NULL || img_file==NULL)) {
		fprintf(stderr, "%s: Missing XYZ and/or image filenames\n",
			argv[0]);
		fprintf(stderr, usage);
		exit(1);
	}
}

// SUMMITT Terrain merge master main program
int main(int argc, char **argv)
{
	get_args(argc, argv);

	Network net;			// start PVM network

	// make sure that worked
	if (net.get_net_status() != NET_READY) {
		fprintf(stderr, "%s: Can't start PVM network, status %d\n",
				argv[0], net.get_net_status());
		return 1;
	}

	int mtid = master_tid();	// get master task ID
	if (mtid >= 0) 			// found him, send request as peer
		return peer_request(mtid);

	// We are the master
	if (request != REQ_XYZ)
		return 0;		// nothing to do

	master_init(&net);		// prepare for masterly stuff
	int finish_req = -1;		// no task waiting for "finished" reply

	// Loop servicing messages from peers and slaves
	while (mpstate != DONE) {

		// get next message, extract tag and sender's task ID
		int bufid = pvm_recv(-1, -1);
		int nbytes, tag, tid;
		pvm_bufinfo(bufid, &nbytes, &tag, &tid);

		switch (tag) {
		case MSG_SLAVE_READY:	// slave is ready for requests
			slave_idle(tid);
			break;
		case MSG_XYZ_MREQ:	// peer has a new request for us
			msg_xyz_request(tid);
			break;
		case MSG_XYZ_DONE:	// slave is finished with job
			msg_xyz_done(tid);
			break;
		case MSG_FINISH:	// peer wants clean termination
			// save requesting peer task ID for later notification
			finish_req = tid;

			// if any slaves are currently busy, need to wait
			mpstate = (spool->num_busy() > 0) ? FINISHING : DONE;
			break;
		case MSG_ABORT:		// peer wants immediate shutdown
			mpstate = DONE;
			break;
		case MSG_STATUS:	// peer triggering diag status dump
			req_que.dump(stderr);
			spool->dump(stderr);
			break;
		default:
			fprintf(stderr, "Master got unexpected msg tag %d\n",
				tag);
		}
	}

	// Done, clean up and shut down
	if (verbose)
		fprintf(stderr, "Master shutting down\n");

	// kill off the slaves
	pool->reset_task_list();

	// notify "finish" requester
	if (finish_req >= 0) {
		Peer_Task fin_task(finish_req);
		psend_finished(&fin_task);
	}

	// allow time for the peer that requested exit, and slaves,
	// to go away so that we can fully shut down the network
	sleep(8);

	return 0;
}
