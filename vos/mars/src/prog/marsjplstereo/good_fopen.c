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

#include "good_fopen.h"

#include <string.h>
#include <unistd.h>

#if defined(RTI_VXWORKS) || defined (__VXWORKS__) || defined (RTS_VXWORKS)

#include <stat.h>
#else

#include <sys/stat.h>
#ifndef OK
#define OK 0
#endif
#endif


FILE *good_fopen (char *filename, char *mode)
{
  FILE *fp;
  struct stat Stat;

  if (filename == NULL || mode == NULL)
    return NULL;

  if (strchr (mode, 'w') != NULL || strchr (mode, 'W') != NULL) {

    /* Under VxWorks, fopen will not write onto an already-existing file.
     * So we'll remove it if it already exists.
     */

    if (stat (filename, &Stat) == OK)
      remove (filename);
  }
  fp = fopen (filename, mode);
  return fp;
}

int file_exists (char *filename)
{
#ifdef __unix__
  return (access (filename, 0) == 0);
#else
  struct stat __stat_buf;
  return (stat (filename, &__stat_buf) == OK);
#endif

}
