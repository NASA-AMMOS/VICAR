// polypath.C is a -*-C-*- program
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
 * Module: polypath.C
 *
 * Purpose: Animation path methods for polynomial curves
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
#include "path/polypath.h"

int polypath::parse_in(Dataport *fileptr)

/************************************************************************ 
  Read in a path from a file.  Reads all appropriate keyword/value pairs
  for a polynomial path, including degree of the curve, the coefficient
  values, etc.

  Parameter       Type        Mode    Description
  ----------------------------------------------------------------------
  fileptr         Dataport*       In      File from which to read.
  RETURN VALUE    path*       Out     Path which was read in.
*************************************************************************/

{
  char token[80];
  int deg, i;
/*
  token[0] = '\0';

  fscanf(fileptr, "%s", token);
  if (strcasecmp(token,"degree") == 0)
    {
      fscanf(fileptr, "%s", token);
      deg = atoi(token);
      if (deg < 0)
	{
	  fprintf(stderr,"polypath: Negative value for degree of curve, %d, is incorrect\n",deg);
	  fprintf(stderr,"          Unable to continue processing\n");
	  return NULL;
	}
      set_degree(deg);
      for (i = 0; i <= deg; i++)
	{
	  fscanf(fileptr,"%s", token);
	  coeffs[i] = atof(token);
	}
    };
  return NULL;
*/
	if(!get_next_token(fileptr, token)) {
		fprintf(stderr," Whoops - Unexpected EOF in polypath file\n");
		return(FALSE);
	}
	if(!strcmp(token, "DEGREE")) {
		if(!get_next_token(fileptr, token)) {
			fprintf(stderr," Whoops - Unexpected EOF in polypath file\n");
			return(FALSE);
		}
		deg = atoi(token);
		if (deg < 0) {
			fprintf(stderr,"polypath: Negative value for degree of curve, %d, is incorrect\n",deg);
			fprintf(stderr,"          Unable to continue processing\n");
			return(FALSE);
		}
		set_degree(deg);
		for (i = 0; i <= deg; i++) {
			if(!get_next_token(fileptr, token)) {
				fprintf(stderr," Whoops - Unexpected EOF in polypath file\n");
				return(FALSE);
			}
			coeffs[i] = atof(token);
		}
	}
	return(TRUE);
};

int polypath::parse_out(Dataport *fileptr, int)

/************************************************************************ 
  Writes a polynomial path (curve) to a file, printing out the degree
  and the coefficient values for the curve.

  Parameter       Type        Mode    Description
  ----------------------------------------------------------------------
  fileptr         Dataport*       In      File from which to read.
*************************************************************************/

{
  int i, deg;
  char token[80];

/*
  fprintf(fileptr,"polypath degree %d",degree);
  for (i = 0; i < degree; i++)
    fprintf(fileptr,"%5.4f ",coeffs[i]);
  return 0;
*/
	put_token(fileptr, (char *)"DEGREE");
	get_degree(deg);
	sprintf(token, "%d", deg);
	put_token(fileptr, token);
	for (i = 0; i <= deg; i++) {
		sprintf(token, "%f", coeffs[i]);
		put_token(fileptr, token);
	}
	return(TRUE);
};

double polypath::get_value(double currenttime)

/************************************************************************ 
  Returns the value of the polynomial at the time currenttime.  Since
  (non-rational) polynomials are defined for all time t, there is no
  need to check the time.

  Parameter       Type        Mode    Description
  ----------------------------------------------------------------------
  currenttime     double      In      Time at which to evaluate curve
  RETURN VALUE    double      Out     Value of curve at currenttime
*************************************************************************/

{
  int i;
  double aprime, aprime_prev;
  
  aprime_prev = coeffs[degree];

  for (i = degree; i >= 0; i--)
    {
      aprime = coeffs[i] + currenttime * aprime_prev;
      aprime_prev = aprime;
    }
  return aprime;
};

void polypath::change_coeff(int which, double value)

/************************************************************************ 
  Set the value of the which-th coefficient to value.

  Parameter       Type        Mode    Description
  ----------------------------------------------------------------------
  which           int         In      Index of coefficient to change
  value           double      In      New value of coefficient
*************************************************************************/

{
  if ((which >= 0) && (which <= degree))
    coeffs[which] = value;
};

path *polypath::copy_path()

/************************************************************************ 
  Copy the current path into a new path, and return a pointer to that
  path.

  Parameter       Type        Mode    Description
  ----------------------------------------------------------------------
  RETURN VALUE    polypath*   Out     Pointer to new path
*************************************************************************/

{
  polypath *thePath;
  int d, i;

  thePath = new polypath;
  if (thePath == NULL)
    {
      fprintf(stderr,"polypath: Failure during creation of new polypath during copy\n");
      return NULL;
    }
  get_degree(d);
  for (i = 0; i < d; i++)
    thePath->coeffs[i] = get_coeff(i);
  return thePath;
}

void polypath::add_coeff(double value)

/************************************************************************ 
  Add a coefficient to the path.  The coefficient that is added is
  the coefficient for the highest (new) degree.

  Parameter       Type        Mode    Description
  ----------------------------------------------------------------------
  value           double      In      New coefficient value
*************************************************************************/

{
  degree = degree + 1;
  coeffs[degree] = value;
}

polypath::polypath()

/************************************************************************ 
  Constructor for polypath class.

  Parameter       Type        Mode    Description
  ----------------------------------------------------------------------
  NONE
*************************************************************************/

{
  degree = 0;
  coeffs[0] = 0;
};

polypath::~polypath()

/************************************************************************ 
  Destructor for polypath class.

  Parameter       Type        Mode    Description
  ----------------------------------------------------------------------
  NONE
*************************************************************************/

{
  degree = 0;
  coeffs[0] = 0;
};
