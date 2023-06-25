// wvspline.C is -*-C-*- code
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
 * Module: wvspline.C
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
#include <iostream>
#include <stdio.h>
#include <stdlib.h>
#include <strings.h>
#include <float.h>
#include <math.h>
#include "path/wvspline.h"

using namespace std;


void wvspline::set_time(int index, double newtime)

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

void wvspline::set_value(int index, double val)

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

void wvspline::set_weight(int linterval, double wgt)

/************************************************************************ 
  Set the weight of the given interval.

  Parameter       Type        Mode    Description
  ----------------------------------------------------------------------
  linterval        int         In      Interval for which to set weight
  wgt             double      In      New weight value for interval
*************************************************************************/

{
  if (wgt == 0.0)
    {
      fprintf(stderr,"wvspline: Weight for interval %d is specified as 0.0\n",linterval);
      fprintf(stderr,"          Exact zero values for weights is not allowed\n");
      fprintf(stderr,"          Setting weight to very small positive value\n");
      wgt = 10.0 * DBL_MIN;
    }
  if ((linterval >= 0) && (linterval < numpts))
    weight[linterval] = wgt;
  solve();
};

void wvspline::set_tension(int index, double tens)

/************************************************************************ 
  Set the tension for the index-th knot.

  Parameter       Type        Mode    Description
  ----------------------------------------------------------------------
  index           int         In      Index of knot to set tension 
  tens            double      In      New tension for index-th knot
*************************************************************************/

{
  if ((index >= 0) && (index < numpts))
    tension[index] = tens;
  solve();
};

double wvspline::get_weight(int linterval)

/************************************************************************ 
  Returns the weight for a given interval.

  Parameter       Type        Mode    Description
  ----------------------------------------------------------------------
  linterval        int         In      Interval to get weight from
  RETURN VALUE    double      Out     Weight of the interval
*************************************************************************/

{
  if ((linterval >= 0) && (linterval < numpts))
    return weight[linterval];
  else
    return 0.0;
};

double wvspline::get_tension(int index)

/************************************************************************ 
  Returns the tension for a given knot.

  Parameter       Type        Mode    Description
  ----------------------------------------------------------------------
  index           int         In      Index of knot in question
  RETURN VALUE    double      Out     Tension of knot
*************************************************************************/

{
  if ((index >= 0) && (index < numpts))
    return tension[index];
  else
    return 0.0;
};

double wvspline::get_value_at_index(int index)

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

double wvspline::get_time_at_index(int index)

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

void wvspline::insert_knot(double knottime, double val, double pttens, double intwgt)

/************************************************************************ 
  Inserts a knot and all of the associated values (tension, weight, etc.)
  into the list of knots at time knottime.

  Parameter       Type        Mode    Description
  ----------------------------------------------------------------------
  knottime        double      In      Time of new knot
  val             double      In      Value of the new knot
  pttens          double      In      Tension at the new knot
  intwgt          double      In      Weight of the interval ff. the knot
*************************************************************************/

{
  int i, j;
  
  i = 0;
  while ((timearr[i] < knottime) &&
	 (i < numpts))
    i = i + 1;
  
  for (j = numpts; j > i; j--)
    {
      timearr[j] = timearr[j-1];
      valarr[j] = valarr[j-1];
      tension[j] = tension[j-1];
      weight[j] = weight[j-1];
    }

  timearr[i] = knottime;
  valarr[i] = val;
  tension[i] = pttens;
  if (intwgt == 0.0)
    {
      fprintf(stderr,"wvspline: Weight for interval %d is specified as 0.0\n",i);
      fprintf(stderr,"          Exact zero values for weights is not allowed\n");
      fprintf(stderr,"          Setting weight to very small positive value\n");
      intwgt = 10.0 * DBL_MIN;
    }
  weight[i] = intwgt;

  if ((numpts > 1) && 
	   ((i == 0) && (fabs(timearr[1] - timearr[0]) < (10.0 * DBL_MIN))) ||
	   ((i != 0) && (timearr[i+1] - timearr[i] == 0.0)))
    {
      fprintf(stderr,"wvspline: Knots at %d and %d have the same time\n",i,i+1);
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

void wvspline::delete_knot(int index)

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
	  weight[i]  = weight[i+1];
	  tension[i] = tension[i+1];
	}

      numpts = numpts - 1;

      solve();
    }
}

int wvspline::parse_in(Dataport *fp)

/************************************************************************ 
  Read in wvspline information  from a file.  Reads in keyword/value-list
  pairs for knot times, values, etc.

  Parameter       Type        Mode    Description
  ----------------------------------------------------------------------
  fileptr         Dataport*       In      File from which to read.
  RETURN VALUE    wvspline*   Out     wvspline which was read in.
*************************************************************************/

{
	  char token[4096];
	  flag eos;
	  double pttime, ptval, pttens, intwgt;
	  double s, e;

	  eos = PATH_FLAG_FALSE;
	  while (eos == PATH_FLAG_FALSE) {
		if(!get_next_token(fp, token)) {
			fprintf(stderr," Whoops - Unexpected EOF in wvspline file\n");
			return(FALSE);
		}

		if (strcasecmp(token,"END_SPLINE") == 0)
			eos = PATH_FLAG_TRUE;
		else if (strcasecmp(token,"START") == 0) {
			if(!get_next_token(fp, token)) {
				fprintf(stderr," Whoops - Unexpected EOF in wvspline file\n");
				return(FALSE);
			}
			s = atof(token);
		} else if (strcasecmp(token,"END") == 0) {
			if(!get_next_token(fp, token)) {
				fprintf(stderr," Whoops - Unexpected EOF in wvspline file\n");
				return(FALSE);
			}
			e = atof(token);
		} else if (strcasecmp(token,"VERSION") == 0) {
			if(!get_next_token(fp, token)) {
				fprintf(stderr," Whoops - Unexpected EOF in wvspline file\n");
				return(FALSE);
			}
			if(strcmp(token, "1.0")) {
				fprintf(stderr," Whoops - Unexpected version in wvspline file - %s should be 1.0\n", token);
				return(FALSE);
			}
		} else if (strcasecmp(token,"TYPE") == 0) {
			if(!get_next_token(fp, token)) {
				fprintf(stderr," Whoops - Unexpected EOF in wvspline file\n");
				return(FALSE);
			}
			if(strcmp(token, "WEIGHTED_NU_SPLINE")) {
				fprintf(stderr," Whoops - Unexpected version in wvspline file - %s should be 1.0\n", token);
				return(FALSE);
			}
		} else {
			pttime = atof(token);
			if(!get_next_token(fp, token)) {
				fprintf(stderr," Whoops - Unexpected EOF in wvspline file\n");
				return(FALSE);
			}
	    		ptval = atof(token);
			if(!get_next_token(fp, token)) {
				fprintf(stderr," Whoops - Unexpected EOF in wvspline file\n");
				return(FALSE);
			}
	    		pttens = atof(token);
			if(!get_next_token(fp, token)) {
				fprintf(stderr," Whoops - Unexpected EOF in wvspline file\n");
				return(FALSE);
			}
	    		intwgt = atof(token);
	    		insert_knot(pttime, ptval, pttens, intwgt);
		}
	}
	if (s > e) {
		fprintf(stderr,"wvspline: Start value %f is greater than end value %f, swapping\n",s,e);
		set_start_end(e, s);
	} else
	      set_start_end(s, e);

	return (TRUE);
};

int wvspline::parse_out(Dataport *fp, int )

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
  put_token(fp, (char *)"\nWVSPLINE_V1");
  put_token(fp, (char *)"\nVERSION");
  put_token(fp, (char *)"1.0");
  put_token(fp, (char *)"\nTYPE");
  put_token(fp, (char *)"WEIGHTED_NU_SPLINE");
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
  	sprintf(token, "%5.4f", tension[i]);
  	put_token(fp, token);
  	sprintf(token, "%5.4f", weight[i]);
  	put_token(fp, token);
  }
  put_token(fp, (char *)"\nEND_SPLINE");
  return (TRUE);
};

void wvspline::solve()

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

  int i;			/* Loop counter */
  double c[1000], r[1000];	/* Used to compute solution factors */
  double fact,			/* Factor for forward-elimination, back-substitution */
    valtemp1, valtemp2;		/* Temporary variables for solution of tridiagonal system */
  double dist, total;

/* The division by zero error messages should *never* appear, since
   that can only happen if the weight is zero, which it can never
   be (we checked when we inserted/changed the points, or if the
   interval is zero, which we also checked when inserting/changing
   points.  Nevertheless, these checks are in for completeness sake. */

  if (numpts > 1)
    {

      total = 0.0;
      for (i = 0; i < numpts - 1; i++) 
	{
	  dist = timearr[i+1] - timearr[i];
	  total = total + dist;
	  interval[i] = dist;
	  if (interval[i] <= (10.0*DBL_MIN))
	    {
	      interval[i] = 1.0;
	    }
	}

/* Set up zero-th row of system */

      c[0] = weight[0]/interval[0];
      r[0] = 0.5 * tension[0] + 2.0 * c[0];
      dval[0] = 3.0 * c[0] * (valarr[1] - valarr[0]);
      valtemp1 = dval[0];

/* Forward Elimination rows 1 to n-2 */

      for (i = 1; i < numpts-1; i++)
	{
	  c[i] = weight[i]/interval[i];
	  if (r[i-1] > (10.0*DBL_MIN))
	    fact = c[i-1]/r[i-1];
	  else
	    {
	      fprintf(stderr,"wvspline: Division by zero in forward elimination of matrix\n");
	      fprintf(stderr,"          Attempting to proceed by defaulting divisor to  1.0.\n");
	      fact = c[i-1];
	    }
	  r[i] = 0.5 * tension[i] + 2.0 * (c[i-1] + c[i]) - fact * c[i-1];
	  valtemp2 = 3.0 * c[i] * (valarr[i+1] - valarr[i]);
	  dval[i] = valtemp1 + valtemp2 - fact * dval[i-1];
	  valtemp1 = valtemp2;
	}

/* Set up row n - 1 */

      if (r[numpts - 2] > (10.0*DBL_MIN))
	fact = c[numpts - 2]/r[numpts - 2];
      else
	{
	  fprintf(stderr,"wvspline: Division by zero in forward elimination of matrix\n");
	  fprintf(stderr,"          Attempting to proceed by defaulting divisor to  1.0.\n");
	  fact = c[numpts - 2];
	}
      r[numpts - 1] = 0.5 * tension[numpts-1] + (2.0 - fact) * c[numpts - 2];
      dval[numpts - 1] = valtemp1 - fact * dval[numpts - 2];

/* Backward Substitution */

      if (r[numpts - 2] > (10.0*DBL_MIN))
	dval[numpts - 1] = dval[numpts - 1]/r[numpts - 1];
      else
	{
	  fprintf(stderr,"wvspline: Division by zero in backward substitution of matrix\n");
	  fprintf(stderr,"          Attempting to proceed by defaulting divisor to  1.0.\n");
	  dval[numpts - 1] = dval[numpts - 1];
	}

      for (i = numpts - 2; i >= 0; i--)
	{
	  if (r[i] > (10.0*DBL_MIN))
	    dval[i] = (dval[i] - c[i] * dval[i+1])/r[i];
	  else
	    {
	      fprintf(stderr,"wvspline: Division by zero in backward substitution of matrix\n");
	      fprintf(stderr,"          Attempting to proceed by defaulting divisor to  1.0.\n");
	      dval[i] = dval[i] - c[i] * dval[i+1];
	    }
	}
    }
}
  
double wvspline::get_value(double currenttime)

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

  int i;			// Which control points to use
  double 
    t, t2, tless1_2, t_2,	// Used to compute Hermite ftns cleanly
    p0, p1, d0, d1,		// Hermite functions
    outval;


  if (numpts > 1)
    {
      if (currenttime < timearr[0])
	outval = valarr[0];
      else if (currenttime > timearr[numpts-1])
	outval = valarr[numpts-1];
      else
	{
	  i = 0;
	  while ((i < numpts) &&
		 (timearr[i] <= currenttime))
	    i = i + 1;
	  if (i >= numpts) i = numpts - 1;
	  
	  i = i - 1;
	  
	  t = currenttime - timearr[i];

	  if (interval[i] > (10.0*DBL_MIN))
	    t = t/interval[i];
	  else
	    {
	      fprintf(stderr,"wvspline: Division by zero in get_value due to interval length\n");
	      fprintf(stderr,"          Attempting to proceed by defaulting interval %d to 1.0.\n",i);
	      fprintf(stderr,"          Results may be unpredictable and certainly will not\n");
	      fprintf(stderr,"          be what you want.  Adjust variables around knot %d to\n",i);
	      fprintf(stderr,"          ensure non-zero intervals length\n");
	    }
	  
	  t2 = 2.0 * t;
	  tless1_2 = (1.0 - t) * (1.0 - t);
	  t_2 = t * t;
	  p0 = tless1_2 * (t2 + 1.0);
	  p1 = t_2 * (3.0 - t2);
	  d0 = t * tless1_2;
	  d1 = t_2 * (t - 1.0);
	  
	  outval = valarr[i] * p0 + valarr[i+1] * p1 +
	    dval[i] * d0 * interval[i] + dval[i+1] * d1 * interval[i];
	}
    }
  else
    if (numpts == 1)
      outval = valarr[0];
    else
      outval = 0.0;
  
  return outval;
}

path *wvspline::copy_path()

/************************************************************************ 
  Copy a wvspline, returning a pointer to the copy.

  Parameter       Type        Mode    Description
  ----------------------------------------------------------------------
  RETURN VALUE    wvspline*   Out     Pointer to copy of wvspline
*************************************************************************/

{
  wvspline *thePath;
  int n, i;
  double copytime, copyval, copyweight, copytension;

  thePath = new wvspline;
  if (thePath == NULL)
    {
      fprintf(stderr,"wvspline: Failure to create new wvspline during copy\n");
      fprintf(stderr,"          wvspline not copied\n");
      return NULL;
    }
  n = get_numpts();
  for (i = 0; i < n; i++)
    {
      copytime = get_knot_time(i);
      copyval = get_knot_value(i);
      copyweight = get_weight(i);
      copytension = get_tension(i);
      thePath->insert_knot(copytime,copyval, copytension, copyweight);
    }
  return thePath;
}
     
wvspline::wvspline()

/************************************************************************ 
  Constructor for wvspline class.

  Parameter       Type        Mode    Description
  ----------------------------------------------------------------------
  NONE
*************************************************************************/

{
  numpts = 0;
  currpt = 0;
};

wvspline::~wvspline()

/************************************************************************ 
  Destructor for wvspline class.

  Parameter       Type        Mode    Description
  ----------------------------------------------------------------------
  NONE
*************************************************************************/

{
  numpts = 0;
  currpt = 0;
};



