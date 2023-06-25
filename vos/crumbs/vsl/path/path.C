// path.C is a -*-C-*- program
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
 * Module: path.C
 *
 * Purpose: Animation path methods
 *
 * Limitations:
 *              
 * Original Author: Stephen H. Watson
 *
 * Current cognizant programmer: Stephen H. Watson
 *
 * Created: January 1994
 *
 * Last Modified: 14 Dec 1994  4.2
 *
 ************************************************************************
 */
#include <strings.h>
#include <stdlib.h>
#include "path/path.h"
#include "path/polypath.h"
#include "path/spline.h"
#include "path/wvspline.h"

int path::parse_in(Dataport *)

/************************************************************************ 
  Read in a path from a file.  Gets overloaded by derived path classes.

  Parameter       Type        Mode    Description
  ----------------------------------------------------------------------
  fileptr         Dataport*       In      File from which to read.
  RETURN VALUE    path*       Out     Path which was read in.
*************************************************************************/

{
/*
  char token[80];
  double s, e;
  path *curpath;

  token[0] = '\0';
  fileptr = fileptr;

  fscanf(fileptr, "%s", token);
  strcpy(pathname,token);

  fscanf(fileptr, "%s", token);
  if (strcasecmp(token,"start") == 0)
    {
      fscanf(fileptr, "%s", token);
      s = atof(token);
      fscanf(fileptr, "%s", token);
      if (strcasecmp(token,"end") == 0)
	{
	  fscanf(fileptr, "%s", token);
	  e = atof(token);
	}
      if (s > e)
	{
	  fprintf(stderr,"path: start value %f is greater than end value %f, swapping\n",s,e);
	  set_start_end(e, s);
	}
      else
	set_start_end(s, e);
    }
  else
    {
      printf("Start and End are not defined correctly\n");
      exit(0);
    }
  return NULL;
*/
	return(FALSE);
};

int path::parse_out(Dataport *, int)

/************************************************************************ 
  Write out a path to a file.  Since the base class does so little,
  this is effectively useless, but gets overloaded for more complete
  derived classes.

  Parameter       Type        Mode    Description
  ----------------------------------------------------------------------
  fileptr         Dataport*       Out     File to write path into.
*************************************************************************/

{
  return(0);
};

double path::get_value(double )

/************************************************************************ 
  Returns the value of the current path given a specific time.  Since
  the base class is not really utilized, this only returns 0.0.

  Parameter       Type        Mode    Description
  ----------------------------------------------------------------------
  currenttime     double      In      Time at which to evaluate path
  RETURN VALUE    double      Out     Value of path at currenttime
*************************************************************************/

{
  return 0.0;
};

path *path::copy_path()

/************************************************************************ 
  Copy the current path and return a pointer to the new one.

  Parameter       Type        Mode    Description
  ----------------------------------------------------------------------
  RETURN VALUE    path*       Out     Pointer to copy of path
*************************************************************************/

{
  path *thePath;
  double s, e;

  thePath = new path;
  get_start_end(s,e);
  thePath->set_start_end(s,e);
  return thePath;
}

path *new_path_by_name(Dataport *fileptr)

/************************************************************************ 
  Create a new path of the correct type, using the next keyword
  in the file to determine the type of path to create.

  Parameter       Type        Mode    Description
  ----------------------------------------------------------------------
  RETURN VALUE    path*       Out     Pointer to new path
*************************************************************************/

{
  path *thePath;
  char token[80];

/*
  token[0] = '\0';
  fscanf(fileptr,"%s",token);
*/
   if(!get_next_token(fileptr, token)) {
           fprintf(stderr," Whoops - Unexpected EOF in scene file\n");
           return(FALSE);
   }

  if (strcasecmp(token, "polypath") == 0)
    {
      thePath = new polypath; 
      return thePath;
    }
  else if (strcasecmp(token, "spline") == 0)
    {
      thePath = new spline; 
      return thePath;
    }
  else if (strcasecmp(token, "wvspline") == 0)
    {
      thePath = new wvspline; 
      return thePath;
    }
  else
    return NULL;
};

