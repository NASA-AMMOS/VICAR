// tm_msg.C 1.5 03/08/28 12:53:51
/** \file
// These functions pack/send or unpack PVM messages between the
// terrain merge peer, master, and slave.
// Slaves can use get_from_buffer() since messages are received from
// a known task (tmerge master), but master has to do his own
// receive and unpack.
*/

#include <stdio.h>
#include "net/net_lib.h"
#include "cahvor.h"
#include "range.h"
#include "tm_msg.h"

#define ENCODING	PvmDataRaw	// message encoding method

/// slave to master: initialization complete (no data)
int psend_slave_ready(Message_Passing_Task *t)
{
	t->reset_buffer(ENCODING);
	return t->send_buffer(MSG_SLAVE_READY);
}

/// send peer->master: process this terrain wedge. cmodname may be NULL
int psend_master_request(Message_Passing_Task *t, char *xyzname, 
				char *imgname, char *cmodname)
{
	t->reset_buffer(ENCODING);
	t->put_in_buffer(xyzname);
	t->put_in_buffer(imgname);
	t->put_in_buffer(cmodname ? cmodname : (char *)"-");
	return t->send_buffer(MSG_XYZ_MREQ);
}

/// receive peer->master: process this terrain wedge. 
int precv_master_request(char *xyzname, char *imgname, char *cmodname)
{
	pvm_upkstr(xyzname);
	pvm_upkstr(imgname);
	return pvm_upkstr(cmodname);
}

/// send master->slave: process terrain wedge
int psend_slave_request(Message_Passing_Task *t, char *name, 
				CAHVOR *cmod, double site[3])
{
	t->reset_buffer(ENCODING);
	t->put_in_buffer(name);
	t->put_in_buffer(cmod->c, 3*6);
	t->put_in_buffer(site, 3);
	return t->send_buffer(MSG_XYZ_SREQ);
}

/// receive master->slave: process terrain wedge
// caller needs to free name string when done
int precv_slave_request(Master_Task *t, char **name, CAHVOR *cmod, 
				double site[3])
{
	t->get_from_buffer(name);
	t->get_from_buffer(cmod->c, 3*6);
	return t->get_from_buffer(site, 3);
}

/// peer->master: finish up
int psend_finish(Message_Passing_Task *t)
{
	t->reset_buffer(ENCODING);
	return t->send_buffer(MSG_FINISH);
}

/// peer->master: abort
int psend_abort(Message_Passing_Task *t)
{
	t->reset_buffer(ENCODING);
	return t->send_buffer(MSG_ABORT);
}

/// peer->master: dump status
int psend_status(Message_Passing_Task *t)
{
	t->reset_buffer(ENCODING);
	return t->send_buffer(MSG_STATUS);
}

/// master->peer: finished
int psend_finished(Message_Passing_Task *t)
{
	t->reset_buffer(ENCODING);
	return t->send_buffer(MSG_DONE);
}

/// slave->master: terrain wedge finished
int psend_terrain_done(Message_Passing_Task *t, char *name, int status, 
	double posn[3], double rot[3], Summitt_range *bvol, double obj_res)
{
	t->reset_buffer(ENCODING);
	t->put_in_buffer(name);
	t->put_in_buffer(status);
	t->put_in_buffer(posn, 3);
	t->put_in_buffer(rot, 3);
	t->put_in_buffer(&(bvol->xmin), 6);
	t->put_in_buffer(obj_res);
	return t->send_buffer(MSG_XYZ_DONE);
}

/// extract fields from MSG_XYZ_DONE
int precv_terrain_done(char *name, int *status, 
	double posn[3], double rot[3], Summitt_range *bvol, double *obj_res)
{
	pvm_upkstr(name);
	pvm_upkint(status, 1, 1);
	pvm_upkdouble(posn, 3, 1);
	pvm_upkdouble(rot, 3, 1);
	pvm_upkdouble(&(bvol->xmin), 6, 1);
	return pvm_upkdouble(obj_res, 1, 1);
}
