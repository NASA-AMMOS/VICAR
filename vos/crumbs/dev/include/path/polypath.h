// polypath.h is a -*-C-*- header file.
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
 * Module: polypath.h
 *
 * Purpose: This class defines non-rational polynomial curves of degree
 *          up to 30.
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

#ifndef _POLYPATH_H_
#define _POLYPATH_H_

#include "path/path.h"

class polypath : public path {

  int degree;			/* Degree of this polynomial */
  double coeffs[30];		/* Polynomial coefficients in incr. order */

 public:

  void set_degree(int d) {degree = d; }
  void get_degree(int &d) {d = degree; }

  virtual int parse_in(Dataport *fileptr);
  virtual int parse_out(Dataport *fileptr, int expand_flag);

  int pathtype() {return POLYPATH; }

  double get_value(double currenttime);
  void change_coeff(int which, double value);
  void add_coeff(double value);
  double get_coeff(int i) { if ((i >= 0) && (i < degree)) return coeffs[i]; else return 0.0; }
  path *copy_path();

  polypath();
  ~polypath();
};

#endif // _POLYPATH_H_
