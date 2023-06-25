// globals.h is -*-C-*- code
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
 * Module: pathglobals.h
 *
 * Purpose: Global definitions of various types of paths, etc.
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


#ifndef _PATHGLOBALS_H_
#define _PATHGLOBALS_H_

#include <stdio.h>
#include <stdlib.h>

enum flag {PATH_FLAG_FALSE, PATH_FLAG_TRUE};
#define PATH 0
#define POLYPATH 1
#define SPLINE 2
#define WVSPLINE 3
#define DIGITAL 4
#define COMPOSITE 5
#define SURVSPLINE 6
#define LINEARSPLINE 7
#define WRAPPED_LINEARSPLINE 8

#endif // _PATHGLOBALS_H_
