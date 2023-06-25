// wvspline.h is -*-C-*- code
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
 * Module: wvspline.h
 *
 * Purpose: Header file for weighted nu-splines
 *
 * Limitations:
 *              
 * Original Author: Stephen H. Watson
 *
 * Current cognizant programmer: Stephen H. Watson
 *
 * Created: April 1994
 *
 * Last Modified: 06 Jul 1994  2.2
 *
 ************************************************************************
 */

#ifndef _WVSPLINE_H_
#define _WVSPLINE_H_

#include "path/spline.h"

class wvspline : public vis_spline {  

  double dval[MAXPOINTS];		/* Array of derivatives of the curve at the knots */
  double weight[MAXPOINTS];		/* Interval weights */
  double tension[MAXPOINTS];		/* Point tensions */
  
 public:

  void set_time(int index, double newtime);
  void set_value(int index, double val);
  void set_weight(int interval, double wgt);
  void set_tension(int index, double tens);

  double get_value_at_index(int index);
  double get_time_at_index(int index);

  double get_weight(int interval);
  double get_tension(int index);

  void insert_knot(double knottime, double val) {
	insert_knot(knottime, val, 0.0, 1.0); }
  void insert_knot(double knottime, double val, double pttens, double intwgt);
  void delete_knot(int index);

  int parse_in(Dataport *fileptr);
  int parse_out(Dataport *fileptr, int expand_flag);

  void solve();

  int pathtype() {return WVSPLINE; }
  path *copy_path();

  double get_value(double currenttime);
  
  wvspline();
  ~wvspline();
};

#endif // _WVSPLINE_H_
