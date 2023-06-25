// machine_setup.h	Machine setup file
//
// Written by Dave Kagels for the ASV task 4/3/94
//
// Copyright (C) 1994, California Institute of Technology
// All rights reserved.

#ifndef _MACHINE_SETUP_H_
#define _MACHINE_SETUP_H_


// System:	This defines what type of system the code will be compiled for
//
#if defined(linux) || defined(__APPLE__)
#ifndef linux
#define linux
#endif
#define	LINUX
#else
		ERROR__No_machine_architecture_defined;
#endif


// Machine specific macros, "fixes", etc.: ---------------------------------
#ifdef SOLARIS
 // as of 03/14/00 solaris on tone uses older complex class
#define COMPLEX_TYPE   complex
#define _NON_TEMPLATE_COMPLEX
#define trunc(x)	((double)(int)(x))
#include <stdlib.h>
// hope this is Solaris on sparc, not intel!
#define INT_BIGENDIAN 1
#define REAL_BIGENDIAN 1
#define NO_XPM
#endif

#ifdef linux
 // as of 03/14/00 linux uses ansi std g++ which expects newer templates
#define	COMPLEX_TYPE	complex<double>
#include <stdlib.h>
#define INT_BIGENDIAN 0
#define REAL_BIGENDIAN 0

#include <stdexcept>
//#ifdef RedHat8
 // as of 10/25/2002 redhat 8 wants the newer include filenames
//#include <string>
//#else
//#include <bastring.h>
//#endif
#endif

#endif // _MACHINE_SETUP_H_
