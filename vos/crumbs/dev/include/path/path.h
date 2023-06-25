// path.h is a -*-C-*- header file.
/***************** ACTORS Software **************************************
 *
 *	Copyright (C) 1994, California Institute of Technology
 *	All rights reserved.
 ************************************************************************
 *	Developed by the Visualization & Earth Science Applications Group,
 *	Image Processing Applications and Development Section,
 *	Jet Propulsion Laboratory,
 *	California Institute of Technology
 ************************************************************************
 * Module: path.h
 *
 * Purpose: This is the base class for all animation paths.
 *
 * Limitations:
 *              
 * Original Author: Stephen H. Watson
 *
 * Current cognizant programmer: Stephen H. Watson
 *
 * Created: April 1994
 *
 * Last Modified: 12 Oct 1994  4.1
 *
 ************************************************************************
 */

#ifndef _PATH_H_
#define _PATH_H_

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "dataport.h"
#include "path/pathglobals.h"

class path {

  double start_time, end_time;	/* Start and end time of the path */

 protected:

  char pathname[80];		/* Name of the path */

 public:

  void set_start_end(double s, double e) {start_time = s; end_time = e; } 
  void get_start_end(double &s, double &e) {s = start_time; e = end_time; }

  char *get_pathname() { return pathname; }
  void set_pathname(char *pn) { strncpy(pathname,pn,79); pathname[79] = '\0'; }

  virtual int parse_in(Dataport *fileptr);
  virtual int parse_out(Dataport *fileptr, int expand_flag);

  virtual int pathtype() {return PATH; }

  virtual double get_value(double currenttime);
  virtual path *copy_path();

  path() { pathname[0] = '\0'; start_time = 0.0; end_time = 0.0; }
  virtual ~path(){ }
};

#endif // _PATH_H_

