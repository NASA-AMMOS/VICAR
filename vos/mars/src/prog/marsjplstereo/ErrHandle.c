/******************************************************************************
*                                                                             *
*		JPL Stereo Vision code					      *
*									      *
*		Developed by the Machine Vision and Tracking Sensors Group    *
*		at the Jet Propulsion Laboratory			      *
*								  	      *
*									      *
*	For information contact:					      *
*									      *
*			Larry Matthies	lhm@robotics.jpl.nasa.gov	      *
*                      	Todd Litwin     litwin@robotics.jpl.nasa.gov          *
*			Mark Maimone	mark.maimone@jpl.nasa.gov	      *
*			Yalin Xiong	yx@robotics.jpl.nasa.gov	      *
*								  	      *
*                                       Updated: 16 Nov 1998                  *
*                                                                             *
*                                       Copyright (C) 1993, 1994, 1995, 1996, *
*                                                     1997, 1998              *
*                                       California Institute of Technology    *
*                                       All Rights Reserved                   *
*                                                                             *
******************************************************************************/


#include <stdio.h>
#include <string.h>
#include "ErrHandle.h"

#define Public
#define Private static
#define Package
#define Peer


/*** This is a function ***/
Public int Warning(char *c)
{
	return fprintf(stderr, "%s", c);
}

Public int Message (char *c)
{
	return fprintf(stderr, "%s", c);
}

Public int FatalErr (char *c)
{
	return fprintf(stderr, "%s", c);
}

#if !defined(__GNUC__) || defined(LINUX)

#include <stdarg.h>
#include <stdio.h>
void error(int code, enum ErrSeverity severity, int flags, ...)
{
#ifndef __ghs__
  va_list args;
  char buf[1000];

  va_start (args, flags);
  vprintf (buf, args);
  va_end (args);
  buf[999] = '\0';
  err_print (code, severity, "Somewhere", -1, "%s", buf);
#else
  err_print (code, severity, "Somewhere", -1, "*** Some error message ***\n");
#endif
}
#endif /* !defined(__GNUC__) || defined(LINUX) */

#ifndef MSP

#include <stdarg.h>

Public void err_print (int code, enum ErrSeverity severity, const char *srcloc,
		       int srcline, ...)
{
#ifndef __ghs__
    va_list args;
    char buf[1000];

    buf[999] = '\0';
    snprintf (buf, 1000, "[%s] line %d: ", srcloc, srcline);
    va_start (args, srcline);
    vprintf (buf + strlen (buf), args);
    va_end (args);
    if (buf[999] != '\0') {
      fprintf (stderr, "*** Truncating error message to %d bytes ***\n", 999);
      buf[999] = '\0';
    }
    FatalErr (buf);
#else
    FatalErr ("*** Some error occurred ***\n");
#endif
}
#endif
