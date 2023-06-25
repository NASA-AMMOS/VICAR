/***************** *************************************
 *
 *	Copyright (C) 1998, California Institute of Technology
 *	All rights reserved.
 ************************************************************************
 *	Developed by the Visualization & Earth Science Applications Group,
 *	Image Processing Applications and Development Section,
 *	Jet Propulsion Laboratory,
 *	California Institute of Technology
 ************************************************************************
 * Module: linearspline.h
 *
 * Purpose: Header file for linear splines
 *
 * Limitations:
 *              
 * Original Author: Dan P. Peters 
 *
 * Current cognizant programmer: Dan P. Peters
 *
 * Created: MArch 1998
 *
 * Last Modified: 
 *
 ************************************************************************
 */

#ifndef _LINEARSPLINE_H_
#define _LINEARSPLINE_H_

#include "path/spline.h"

class linearspline : public vis_spline {  
private:
public:
  	void set_time(int index, double newtime);
  	void set_value(int index, double val);

  	double get_value_at_index(int index);
  	double get_time_at_index(int index);

	void insert_knot(double knottime, double knotval);
	void delete_knot(int index);

  	int pathtype() { return LINEARSPLINE; }
  	path *copy_path();

  	double get_value(double currenttime);
  
	linearspline(int num=100) : vis_spline(num) {};
	static int self_test(void);
};

#endif // _LINEARSPLINE_H_


/**
********************************************************
    Rover Sequencing and Visualization Program (RSVP)

       Copyright 2001
       California Institute of Technology.
       ALL RIGHTS RESERVED.
       U.S. Government sponsorship acknowledged.
*********************************************************
  Class wrapped_linearspline
  Created by John Wright
  Created Fri Aug 16 10:08:05 2002
*********************************************************
  Modification History
  Date        Programmer       Change

*********************************************************
  Current Version (CVS): $Id:$
*********************************************************
*/

#ifndef _wrapped_linearspline_H_
#define _wrapped_linearspline_H_

/// insert single line doxygen summary comment here
/**
 insert multi-line doxygen comment text here
*/

class wrapped_linearspline:public linearspline {

    private:

    protected:

	/* Added features for wrapped splines */
	int    *direction;		/* Array of curve/interpolation directions for wrapped splines (-1=down, 0=unspecified, +1=up) */
	int    wrap_flag;		/* Flag to indicate direction of curve (TRUE=wrapped, FALSE=not wrapped) */
	double wrap_max;		/* Maximum allowed value in spline if wrap_flag==TRUE */
	double wrap_min;		/* Minimum allowed value in spline if wrap_flag==TRUE */
	int    default_direction;	/* Default direction for unspecified knots (-1=down, 0=closest, +1=up) */

    public:

  	int pathtype() { return WRAPPED_LINEARSPLINE; }

	virtual void insert_knot(double knottime, double knotval, int knotdir);
	void insert_knot(double knottime, double knotval) {
		insert_knot(knottime, knotval, 0);
	}
	void delete_knot(int index);
	void more_mem();

	void change_knot_value(int index, double knotval);
	void change_knot_direction(int index, int knotdir);
	int get_knot_direction(int index);

	double wrap_value(double input_value);

	void set_global_direction(int knotdir);
	int get_global_direction();

	double get_min() { return(wrap_min); }
	double get_max() { return(wrap_max); }

	int parse_in(Dataport *fileptr);
	int parse_out(Dataport *fileptr, int expand_flag);

	double get_value(double currenttime);
	path *copy_path();


    // Constructors

	wrapped_linearspline(double minimum_value, double maximum_value, int num=100);

    // Destructor

	~wrapped_linearspline();

    // Unit Self Test Method

	static int self_test(void);

};


#endif
