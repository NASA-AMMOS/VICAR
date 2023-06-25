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
 * Module: survspline.C
 *
 * Purpose: Methods for weighted nu-splines
 *
 * Limitations:
 *              
 * Original Author: Stephen H. Watson
 *
 * Current cognizant programmer: Stephen H. Watson
 *
 * Created: April 1994
 *
 * Last Modified: 14 Dec 1994  4.2
 *
 ************************************************************************
 */
#include <stdio.h>
#include <stdlib.h>
#include <iostream>
#include <strings.h>
#include <float.h>
#include <math.h>
#include "path/survspline.h"

using namespace std;


void survspline::set_time(int index, double newtime)

/************************************************************************ 
  Set the time of the index-th knot.

  Parameter       Type        Mode    Description
  ----------------------------------------------------------------------
  index           int         In      Index of knot to set time for
  newtime         double      In      New time for index-th knot
*************************************************************************/

{
  change_knot_time(index, newtime); /* May have to check for times */
  solve();			/* outside of i-1, i+1 ranges */
};

void survspline::set_value(int index, double val)

/************************************************************************ 
  Set the value of the index-th knot.

  Parameter       Type        Mode    Description
  ----------------------------------------------------------------------
  index           int         In      Index of knot to set time for
  value           double      In      New value for index-th knot
*************************************************************************/

{
  change_knot_value(index, val);
  solve();		
};

double survspline::get_value_at_index(int index)

/************************************************************************ 
  Returns the knot value for a given knot index.

  Parameter       Type        Mode    Description
  ----------------------------------------------------------------------
  index           int         In      Index of knot in question
  RETURN VALUE    double      Out     Value of index-th knot
*************************************************************************/

{
  if ((index >= 0) && (index < numpts))
    return valarr[index];
  else
    return 0.0;
};

double survspline::get_time_at_index(int index)

/************************************************************************ 
  Returns the knot time for a given knot index.

  Parameter       Type        Mode    Description
  ----------------------------------------------------------------------
  index           int         In      Index of knot in question
  RETURN VALUE    double      Out     Time of index-th knot
*************************************************************************/

{
  if ((index >= 0) && (index < numpts))
    return timearr[index];
  else
    return 0.0;
};

void survspline::insert_knot(double knottime, double val)

/************************************************************************ 
  Inserts a knot and all of the associated values (none at this time)
  into the list of knots at time knottime.

  Parameter       Type        Mode    Description
  ----------------------------------------------------------------------
  knottime        double      In      Time of new knot
  val             double      In      Value of the new knot
*************************************************************************/

{
	int i, j;

	/* find 1st index beyond where knottime */
	/* corresponds */
	i = 0;
	while ((timearr[i] < knottime) && (i < numpts)) {
		i = i + 1;
	}
  
	/* move all 'higher' knots to make room for */
	/* new knot */
	for (j = numpts; j > i; j--) {
		timearr[j] = timearr[j-1];
		valarr[j] = valarr[j-1];
    	}

	/* insert the knot */
	timearr[i] = knottime;
	valarr[i] = val;

	if ((numpts > 1) && 
		((i == 0) && (fabs(timearr[1] - timearr[0]) < (10.0 * DBL_MIN))) ||
		((i != 0) && (timearr[i+1] - timearr[i] == 0.0)) &&
		(i < numpts-1))
    {
		
		fprintf(stderr,"survspline: Knots at %d and %d have the same time\n",i,i+1);
		fprintf(stderr,"          This will cause division by zero during matrix inversion\n");
		fprintf(stderr,"          Attempting to adjust knot %d by a small amount\n",i);
		fprintf(stderr,"          This may result in effects that are unpredictable\n");
		fprintf(stderr,"          and certainly not what you want.  Adjust one or both\n");
		fprintf(stderr,"          knot locations accordingly.\n");
		timearr[i + 1] = timearr[i] + 10.0 * DBL_MIN;
    }
      
	numpts = numpts + 1;
	currpt = i;

	solve();
}

void survspline::delete_knot(int index)

/************************************************************************ 
  Deletes the index-th knot from the list of knots and updates all arrays.

  Parameter       Type        Mode    Description
  ----------------------------------------------------------------------
  index           int         In      Index of knot to delete
*************************************************************************/

{
	int i;
  
	if ((index >= 0) && (index < numpts))
    {
		for (i = index; i < numpts - 1; i++)
		{
			timearr[i] = timearr[i+1];
			valarr[i]  = valarr[i+1];
		}

		numpts = numpts - 1;

		solve();
    }
}

int survspline::parse_in(Dataport *fp)

/************************************************************************ 
  Read in survspline information  from a file.  Reads in keyword/value-list
  pairs for knot times, values, etc.

  Parameter       Type        Mode    Description
  ----------------------------------------------------------------------
  fileptr         Dataport*       In      File from which to read.
  RETURN VALUE    survspline*   Out     survspline which was read in.
*************************************************************************/

{
	char token[4096];
	flag eos;
	double pttime, ptval;
	double s, e;

	eos = PATH_FLAG_FALSE;
	while (eos == PATH_FLAG_FALSE) {
		if(!get_next_token(fp, token)) {
			fprintf(stderr," Whoops - Unexpected EOF in survspline file\n");
			return(FALSE);
		}

		if (strcasecmp(token,"END_SPLINE") == 0)
			eos = PATH_FLAG_TRUE;
		else if (strcasecmp(token,"START") == 0) {
			if(!get_next_token(fp, token)) {
				fprintf(stderr," Whoops - Unexpected EOF in survspline file\n");
				return(FALSE);
			}
			s = atof(token);
		} else if (strcasecmp(token,"END") == 0) {
			if(!get_next_token(fp, token)) {
				fprintf(stderr," Whoops - Unexpected EOF in survspline file\n");
				return(FALSE);
			}
			e = atof(token);
		} else if (strcasecmp(token,"VERSION") == 0) {
			if(!get_next_token(fp, token)) {
				fprintf(stderr," Whoops - Unexpected EOF in survspline file\n");
				return(FALSE);
			}
			if(strcmp(token, "1.0")) {
				fprintf(stderr," Whoops - Unexpected version in survspline file - %s should be 1.0\n", token);
				return(FALSE);
			}
		} else if (strcasecmp(token,"TYPE") == 0) {
			if(!get_next_token(fp, token)) {
				fprintf(stderr," Whoops - Unexpected EOF in survspline file\n");
				return(FALSE);
			}
			if(strcmp(token, "SURVEYOR_SPLINE")) {
				fprintf(stderr," Whoops - Unexpected type in survspline file - %s should be '%s'\n", token, "SURVEYOR_SPLINE");
				return(FALSE);
			}
		} else {
			pttime = atof(token);
			if(!get_next_token(fp, token)) {
				fprintf(stderr," Whoops - Unexpected EOF in survspline file\n");
				return(FALSE);
			}
			ptval = atof(token);

			insert_knot(pttime, ptval);
		}
	}
	if (s > e) {
		fprintf(stderr,"survspline: Start value %f is greater than end value %f, swapping\n",s,e);
		set_start_end(e, s);
	} else
		set_start_end(s, e);

	return (TRUE);
};

int survspline::parse_out(Dataport *fp, int )

/************************************************************************ 
  Writes out a spline to a file.  Writes out the list of times and
  values, weights, etc.

  Parameter       Type        Mode    Description
  ----------------------------------------------------------------------
  fileptr         Dataport*       In      File from which to read.
*************************************************************************/

{
  int i;
  double s, e;
  char	token[4096];

  get_start_end(s, e);
  put_token(fp, (char *)"\nSURVSPLINE_V1");
  put_token(fp, (char *)"\nVERSION");
  put_token(fp, (char *)"1.0");
  put_token(fp, (char *)"\nTYPE");
  put_token(fp, (char *)"SURVEYOR_SPLINE");
  put_token(fp, (char *)"\nSTART");
  sprintf(token, "%5.4f", s);
  put_token(fp, token);
  put_token(fp, (char *)"END");
  sprintf(token, "%5.4f", e);
  put_token(fp, token);
  for (i = 0; i < numpts; i++) {
  	sprintf(token, "\n%5.4f", timearr[i]);
  	put_token(fp, token);
  	sprintf(token, "%5.4f", valarr[i]);
  	put_token(fp, token);
  }
  put_token(fp, (char *)"\nEND_SPLINE");
  return (TRUE);
};

void survspline::solve()

/************************************************************************ 
  Solves the tridiagonal system which results from all of the spline
  parameters.  For details of the nature of the system of equations,
  see "Interpolation with Interval and Point Tension Controls Using
  Cubic Weighted nu-Splines", Thomas A. Foley, ACM Transactions on
  Mathematical Software, Vol. 13, No. 1, March 1987, Pages 68-96, and
  "Rectangular nu-Splines", Gregory M. Nielson, IEEE Computer Graphics
  and Applications, February 1986, Pages 35-40.

  This routine solves for the arrays of coefficients of the Hermite
  basis functions, which are then used in the get_value() routine
  to evaluate the curve at any time. 

  Parameter       Type        Mode    Description
  ----------------------------------------------------------------------
  NONE
*************************************************************************/

{
	int    i,j,n;
	double *h;
	double *l;
	double *alpha;
	double *mu;
	double *z;
	double *x = timearr;
	double *y = valarr;
	double total, dist;

	if (numpts > 1)    {

		total = 0.0;
		for (i = 0; i < numpts - 1; i++) {
			dist = timearr[i+1] - timearr[i];
			total = total + dist;
			interval[i] = dist;
			if (interval[i] <= (10.0*DBL_MIN)) {
				interval[i] = 1.0;
			}
		}
	}


	n = numpts - 1;

	/*
	 ** Allocate room for the intermediate variables.
	 */
	h     = (double *)malloc((unsigned)( numpts * sizeof (double) ));
	l     = (double *)malloc((unsigned)( numpts * sizeof (double) ));
	alpha = (double *)malloc((unsigned)( numpts * sizeof (double) ));
	mu    = (double *)malloc((unsigned)( numpts * sizeof (double) ));
	z     = (double *)malloc((unsigned)( numpts * sizeof (double) ));

	a[n] = y[n];
	for ( i = 0; i < n; i++ )
	{
		a[i] = y[i];
		h[i] = x[i+1] - x[i];
	}

	for ( i = 1; i < n; i++ )
		alpha[i] = 3*(a[i+1]*h[i-1] - a[i]*(x[i+1] - x[i-1]) +
					  a[i-1]*h[i]) / (h[i-1]*h[i]);
	l[0]  = 1;
	mu[0] = 0;
	z[0]  = 0;

	for ( i = 1; i < n; i++ )
	{
		l[i]  = 2*(x[i+1] - x[i-1]) - h[i-1]*mu[i-1];
		mu[i] = h[i]/l[i];
		z[i]  = (alpha[i] - h[i-1]*z[i-1])/l[i];
	}

	l[n] = 1;
	z[n] = 0;
	c[n] = z[n];

	for ( j = n-1; j >= 0; j-- )
	{
		c[j] = z[j] - mu[j]*c[j+1];
		b[j] = (a[j+1] - a[j])/h[j] - h[j]*(c[j+1] + 2*c[j])/3;
		d[j] = (c[j+1] - c[j])/(3*h[j]);
	}

	free( h );
	free( l );
	free( alpha );
	free( mu );
	free( z );


}
  
double survspline::get_value(double currenttime)

/************************************************************************ 
  This routine uses for the arrays of coefficients of the Hermite
  basis functions which were calculated in the solve() routine
  to evaluate the curve at time currenttime.

  Parameter       Type        Mode    Description
  ----------------------------------------------------------------------
  currenttime     double      In      Time at which to evaluate spline
  RETURN VALUE    double      Out     Value of spline at currenttime
*************************************************************************/

{

	double *xpts = timearr;
	double dx, outval;
	int linterval;


	if (numpts > 1) {
		if (currenttime < timearr[0])
			outval = valarr[0];
		else if (currenttime > timearr[numpts-1])
			outval = valarr[numpts-1];
		else	{

			for ( linterval = 0; linterval < numpts-1; linterval++ )
				if ( xpts[linterval+1] > currenttime ) break;

			dx = currenttime - xpts[linterval];
			outval = a[linterval] + dx*(b[linterval] + dx*(c[linterval] + dx*d[linterval]));

   
		}
	}
	else if (numpts == 1) {
		outval = valarr[0];
	} else {
		outval = 0.0;
	}
  
	return outval;
}

path *survspline::copy_path()

/************************************************************************ 
  Copy a survspline, returning a pointer to the copy.

  Parameter       Type        Mode    Description
  ----------------------------------------------------------------------
  RETURN VALUE    survspline*   Out     Pointer to copy of survspline
*************************************************************************/

{
  survspline *thePath;
  int n, i;
  double copytime, copyval;

  thePath = new survspline;
  if (thePath == NULL)
    {
      fprintf(stderr,"survspline: Failure to create new survspline during copy\n");
      fprintf(stderr,"          survspline not copied\n");
      return NULL;
    }
  n = get_numpts();
  for (i = 0; i < n; i++)
    {
      copytime = get_knot_time(i);
      copyval = get_knot_value(i);
      thePath->insert_knot(copytime,copyval);
    }
  return thePath;
}
     
survspline::survspline()

/************************************************************************ 
  Constructor for survspline class.

  Parameter       Type        Mode    Description
  ----------------------------------------------------------------------
  NONE
*************************************************************************/

{
	int i;

	numpts = 0;
	currpt = 0;

	for (i = 0; i < MAXPOINTS; i++) {
		a[i] = b[i] = c[i] = d[i] = 0.0;
	}
};

survspline::~survspline()

/************************************************************************ 
  Destructor for survspline class.

  Parameter       Type        Mode    Description
  ----------------------------------------------------------------------
  NONE
*************************************************************************/

{
  numpts = 0;
  currpt = 0;
};



