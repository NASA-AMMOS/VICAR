/***************** GRAPE Software **************************************
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

#ifndef _SURVSPLINE_H_
#define _SURVSPLINE_H_

#include <iostream> 
#include "path/spline.h"

using namespace std;

class survspline : public vis_spline {  

  double a[MAXPOINTS],
         b[MAXPOINTS],
         c[MAXPOINTS],
         d[MAXPOINTS];				/* coefficients */

 public:

  void set_time(int index, double newtime);
  void set_value(int index, double val);

  double get_value_at_index(int index);
  double get_time_at_index(int index);

  void insert_knot(double knottime, double val);
  void delete_knot(int index);

  int parse_in(Dataport *fileptr);
  int parse_out(Dataport *fileptr, int expand_flag);

  void solve();					/* computes coefficients abcd */

  int pathtype() {return SURVSPLINE; }
  path *copy_path();

  double get_value(double currenttime);
  
  survspline();
  ~survspline();
};

#endif // _SURVSPLINE_H_
