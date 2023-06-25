#ifndef _TM_MSG_H
#define _TM_MSG_H
// tm_msg.h 1.4 03/08/28 12:54:33
/** \file
// PVM messages for terrain merge tasks
**/

/// PVM message tags
enum {	MSG_SLAVE_READY = 1,	///< slave->master: initialization complete
	MSG_STATUS = 2,		///< peer->master: show status

	MSG_XYZ_MREQ = 10,	///< peer->master: terrain processing request
	MSG_XYZ_SREQ = 11,	///< master->slave: terrain processing request
	MSG_XYZ_DONE = 12,	///< slave->master: terrain wedge finished

	MSG_FINISH = 90,	///< peer->master: finish up
	MSG_ABORT = 91,		///< peer->master: abort
	MSG_DONE = 92		///< master->peer: finished
};

/// pack-and-send services
int psend_slave_ready(Message_Passing_Task *t);
int psend_status(Message_Passing_Task *t);
int psend_master_request(Message_Passing_Task *t, char *xyzname, 
				char *imgname, char *cmodname);
int psend_slave_request(Message_Passing_Task *t, char *name, 
				CAHVOR *cmod, double site[3]);
int psend_finish(Message_Passing_Task *t);
int psend_abort(Message_Passing_Task *t);
int psend_finished(Message_Passing_Task *t);
int psend_terrain_done(Message_Passing_Task *t, char *name, int status, 
	double posn[3], double rot[3], Summitt_range *bvol, double obj_res);

/// unpack from received messages
int precv_master_request(char *xyzname, char *imgname, char *cmodname);
int precv_slave_request(Master_Task *t, char **name, 
				CAHVOR *cmod, double site[3]);
int precv_terrain_done(char *name, int *status, 
	double posn[3], double rot[3], Summitt_range *bvol, double *obj_res);
#endif
