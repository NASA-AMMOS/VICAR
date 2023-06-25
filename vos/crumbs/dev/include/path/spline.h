// spline.h is a -*-C-*- header file.
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
 * Module: spline.h
 *
 * Purpose: This class defines general splines, although only portions
 *          are implemented, since ACTORS actually uses the derived
 *          class of weighted nu-splines.
 *
 * Limitations: Can't actually evaluate the spline at a time t.  Primarily
 *              this class is here for later extensions, and to define
 *              the knot times and values arrays and their associated
 *              operations.
 *              Maximum number of knots is 2048 (should be dynamic, but
 *              if you start defining splines with more than 2000 points,
 *              you need a different kind of interpolant!).
 *              
 * Original Author: Stephen H. Watson
 *
 * Current cognizant programmer: Stephen H. Watson
 *
 * Created: April 1994
 *
 * Last Modified: 14 Dec 1994  4.2
 *
 ************************************************************************
 */

#ifndef _SPLINE_H_
#define _SPLINE_H_

#include <iostream>

#include "path/polypath.h"
#include <math.h>
#define MAXPOINTS 2048

using namespace std;

class spline : public polypath {

 protected:

  double *timearr;		/* Array of times for each knot */
  double *valarr;		/* Array of values for each knot */
  double *interval;	/* Array of intervals between knots */

  int numpts, currpt;		/* Number of points in spline, current pt. */
  int nalloc;			// space allocated
  enum { REALLOC_INC = 100 };	// how much more space to allocate
  virtual void more_mem();	// does the reallocation
  
 public:

  virtual void insert_knot(double knottime, double knotval);
  virtual void delete_knot(int index);
  virtual void change_knot_value(int index, double knotval);
  virtual void change_knot_time(int index, double knottime);
  virtual double get_knot_value(int index);
  virtual double get_knot_time(int index);
  virtual double get_min();
  virtual double get_max();
  int get_numpts() { return numpts; }

  virtual int parse_in(Dataport *fileptr);
  virtual int parse_out(Dataport *fileptr, int expand_flag);

  int pathtype() { return SPLINE; }

  double get_value(double currenttime);
  path *copy_path();

  int check_time(double time); // check if knot at time already exists;
			       // return 1 if it exists else return 0

  spline(int num=100);
  ~spline();
};

#define BAD_WGT		-10000000
#define BAD_TNS		-10000000

class vis_spline : public spline {
public:

  virtual void insert_knot(double knottime, double knotval) = 0;
  virtual void delete_knot(int index) = 0;

  virtual void set_time(int index, double knottime) = 0;
  virtual void set_value(int index, double knotval) = 0;

  virtual double get_value_at_index(int index) = 0;
  virtual double get_time_at_index(int index) = 0;

// not defined by linearspline
  virtual void solve() { }

// these are here in case the spline is not a wvspline
  virtual void set_tension(int, double) { }
  virtual void set_weight(int, double) { }
  virtual double get_tension(int) { return BAD_WGT; }
  virtual double get_weight(int) { return BAD_TNS; }

  vis_spline(int num=100) : spline(num) {};
  ~vis_spline() {}
};


#endif // _SPLINE_H_
