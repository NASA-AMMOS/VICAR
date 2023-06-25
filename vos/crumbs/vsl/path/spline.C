// spline.C is a -*-C-*- program
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
 * Module: spline.C
 *
 * Purpose: Animation path methods for splines
 *
 * Limitations: Much of this is unimplemented because ACTORS doesn't
 * use splines, but rather wv-splines.
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
#include <iostream>
#include <strings.h>
#include <stdlib.h>
#include <float.h>
#include "path/spline.h"

using namespace std;

void spline::more_mem()
{
        double *tt = timearr;
        double *tv = valarr;
        double *ti = interval;
        timearr = new double[nalloc+REALLOC_INC];
        valarr = new double[nalloc+REALLOC_INC];
        interval = new double[nalloc+REALLOC_INC];
        memcpy(timearr, tt, sizeof(double)*nalloc);
        memcpy(valarr, tv, sizeof(double)*nalloc);
        memcpy(interval, ti, sizeof(double)*nalloc);
        nalloc += REALLOC_INC;
/* Kluge - these deletes sometimes crash - should be a better way
        delete[] tt; delete[] tv; delete[] ti;
*/
}

int spline::parse_in(Dataport *fileptr)

/************************************************************************ 
  Read in spline information  from a file.  Reads in keyword/value
  pairs for degree of spline and knot times and values.

  Parameter       Type        Mode    Description
  ----------------------------------------------------------------------
  fileptr         Dataport*       In      File from which to read.
  RETURN VALUE    spline*     Out     Spline which was read in.
*************************************************************************/

{
  char token[80];
  int deg;
  double pttime, ptval;
/*
  flag eof;
  token[0] = '\0';
  eof = PATH_FLAG_FALSE;
  while (eof == PATH_FLAG_FALSE)
    {
      if ((fscanf(fileptr, "%s", token)) == EOF) 
	{
	  printf("End of file \n");
	  exit(0);
	}
      else if
	(strcasecmp(token,"end") == 0)
	  eof = PATH_FLAG_TRUE;
      else if
	(strcasecmp(token,"degree") == 0)
	  {
	    fscanf(fileptr, "%s", token);
	    deg = atoi(token);
	    if (deg < 0)
	      {
		fprintf(stderr,"spline: Negative degree for spline incorrect\n");
		fprintf(stderr,"        Unable to set degree of spline curve\n");
	      }
	    else
	      set_degree(deg);
	  }
      else
	  {
	    pttime = atof(token);
	    fscanf(fileptr, "%s", token);
	    ptval = atof(token);
	    insert_knot(pttime, ptval);
	  }
    }
  return NULL;
*/
	if(!get_next_token(fileptr, token)) {
		fprintf(stderr," Whoops - Unexpected EOF in spline file\n");
		return(FALSE);
	}
	while(strcmp(token, "END")) {
		if(!strcmp(token, "DEGREE")) {
			if(!get_next_token(fileptr, token)) {
				fprintf(stderr," Whoops - Unexpected EOF in spline file\n");
				return(FALSE);
			}
			deg = atoi(token);
			if (deg < 0) {
				fprintf(stderr,"spline: Negative value for degree of curve, %d, is incorrect\n",deg);
				fprintf(stderr,"          Unable to continue processing\n");
				return(FALSE);
			}
			set_degree(deg);
		} else if(strcmp(token, "END")) {
			pttime = atof(token);
			if(!get_next_token(fileptr, token)) {
				fprintf(stderr," Whoops - Unexpected EOF in spline file\n");
				return(FALSE);
			}
			ptval = atof(token);
			insert_knot(pttime, ptval);
		}
	}
	return(TRUE);
};

int spline::parse_out(Dataport *fileptr, int)

/************************************************************************ 
  Writes out a spline to a file.  Writes out the degree and the
  knot times and values.

  Parameter       Type        Mode    Description
  ----------------------------------------------------------------------
  fileptr         Dataport*       In      File from which to read.
*************************************************************************/

{
  int i, deg;
  char token[80];

/*
  fprintf(fileptr,"spline \n");
  get_degree(deg);
  fprintf(fileptr,"degree %5.4f\n",deg);
  for (i = 0; i < numpts; i++)
    fprintf(fileptr,"%5.4f %5.4f\n",
	    timearr[i], valarr[i]);
  fprintf(fileptr,"end\n");	    
  return 0;
*/
	put_token(fileptr, (char *)"DEGREE");
	get_degree(deg);
	sprintf(token, "%d", deg);
	put_token(fileptr, token);
	for (i = 0; i <= numpts; i++) {
		sprintf(token, "%f", timearr[i]);
		put_token(fileptr, token);
		sprintf(token, "%f", valarr[i]);
		put_token(fileptr, token);
	}
	put_token(fileptr, (char *)"END");
	return(TRUE);
};

void spline::insert_knot(double knottime, double knotval)

/************************************************************************ 
  Insert a knot into a spline.  Locates the interval at which the
  new knot should be inserted, and creates a new knot at that time,
  adjusting all following knots appropriately.

  Parameter       Type        Mode    Description
  ----------------------------------------------------------------------
  knottime        double      In      Time of new knot
  knotval         double      In      Value of new knot
*************************************************************************/

{
        if (check_time(knottime)) return;

        /* find 1st index beyond where knottime */
        /* corresponds */
        int i = 0;
        while ((timearr[i] < knottime) && (i < numpts)) {
                i = i + 1;
        }

        if (i == nalloc) more_mem();

        /* move all 'higher' knots to make room for */
        /* new knot */
        for (int j = numpts; j > i; j--) {
                timearr[j] = timearr[j-1];
                valarr[j] = valarr[j-1];
        }
	
        /* insert the knot */
        timearr[i] = knottime;
        valarr[i] = knotval;

        numpts++;
        currpt = i;

};

void spline::delete_knot(int index)

/************************************************************************ 
  Deletes the index-th knot from the list of knots in the spline, and
  adjusts the various knot/value arrays appropriately.

  Parameter       Type        Mode    Description
  ----------------------------------------------------------------------
  index           int         In      Index of knot to delete
*************************************************************************/

{
        if (index < 0 || index > numpts-1) {
                cout << "Specified index outside acceptable range" << endl;
                return;
        }
        for (int i = index; i < numpts; i++) {
                timearr[i] = timearr[i+1];
                valarr[i] = valarr[i+1];
        }
        timearr[numpts-1] = valarr[numpts-1] = 0.0;
        numpts--;

};

void spline::change_knot_value(int index, double knotval)

/************************************************************************ 
  Changes the value of the index-th knot to knotval.

  Parameter       Type        Mode    Description
  ----------------------------------------------------------------------
  index           int         In      Index of knot to change
  knotval         double      In      New value of knot
*************************************************************************/

{
  if ((index >= 0) && (index < numpts))
    {
      valarr[index] = knotval;
    }
};

void spline::change_knot_time(int index, double knottime)

/************************************************************************ 
  Changes the time of the index-th knot to knottime.  N.B. This routine
  should check to see that the new time is not outside of timearr[index-1]
  to timearr[index+1], but for now it is the GUI's job to ensure that
  that is the case.

  Parameter       Type        Mode    Description
  ----------------------------------------------------------------------
  index           int         In      Index of knot to change
  knottime        double      In      New time of knot
*************************************************************************/

{
  if ((index >= 0) && (index < numpts))
    {
      if (((index == 0) && (fabs(knottime - timearr[1]) <= (10.0 * DBL_MIN))) ||
	  ((index > 0) && (fabs(knottime - timearr[index - 1]) <= (10.0 * DBL_MIN))))
	{
	  fprintf(stderr,"spline: Unable to change knot time for knot %d\n",index);
	  fprintf(stderr,"        Setting to %f would cause a zero-length interval\n",knottime);
	}
      else
	timearr[index] = knottime;
    }
};

double spline::get_knot_value(int index)

/************************************************************************ 
  Returns the value of index-th knot.

  Parameter       Type        Mode    Description
  ----------------------------------------------------------------------
  index           int         In      Index of knot to change
  RETURN VALUE    double      Out     Value of knot
*************************************************************************/

{
  if ((index >= 0) && (index < numpts))
      return valarr[index];
  else
    return 0.0;
};

double spline::get_min()
{
	int i;
	double minval;
	if(numpts <= 0) return(0.0);
	minval = valarr[0];
	for(i=1; i<numpts; i++) {
		if(valarr[i] < minval) minval = valarr[i];
	}
	return(minval);
}

double spline::get_max()
{
	int i;
	double maxval;
	if(numpts <= 0) return(0.0);
	maxval = valarr[0];
	for(i=1; i<numpts; i++) {
		if(valarr[i] > maxval) maxval = valarr[i];
	}
	return(maxval);
}

double spline::get_knot_time(int index)

/************************************************************************ 
  Returns the time of index-th knot.

  Parameter       Type        Mode    Description
  ----------------------------------------------------------------------
  index           int         In      Index of knot to change
  RETURN VALUE    double      Out     Time of knot
*************************************************************************/

{
  if ((index >= 0) && (index < numpts))
    {
      return timearr[index];
    }
  else
    return 0.0;
};

double spline::get_value(double )

/************************************************************************ 
  Returns the value of the spline at time currenttime.
  *** Currently unimplemented because ACTORS uses the derived class
  wvspline! ***

  Parameter       Type        Mode    Description
  ----------------------------------------------------------------------
  currenttime     double      In      Time at which to evaluate spline
  RETURN VALUE    double      Out     Value of spline at time currenttime
*************************************************************************/

{
  return 0.0;
};

path *spline::copy_path()

/************************************************************************ 
  Copy a spline, returning a pointer to the copy.

  Parameter       Type        Mode    Description
  ----------------------------------------------------------------------
  RETURN VALUE    spline*     Out     Pointer to copy of spline
*************************************************************************/

{
  spline *thePath;
  int n, i;
  double copytime, copyval;

  thePath = new spline;
  if (thePath == NULL)
    {
      fprintf(stderr,"spline: Failure during creation of new spline during copy\n");
      fprintf(stderr,"        Spline not copied\n");
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

int spline::check_time(double time)
{
	int ret = 0;
	for (int i = 0; i < numpts; i++) {
	   if (time == timearr[i]) {
		ret = 1;
		break;
	   }
	}
	return ret;
}


spline::spline(int num)

/************************************************************************ 
  Constructor for spline class.

  Parameter       Type        Mode    Description
  ----------------------------------------------------------------------
  NONE
*************************************************************************/

{
  	numpts = 0;
  	currpt = 0;
  	nalloc = num;
  	timearr = new double[num];
  	valarr = new double[num];
  	interval = new double[num];
};

spline::~spline()

/************************************************************************ 
  Destructor for spline class.

  Parameter       Type        Mode    Description
  ----------------------------------------------------------------------
  NONE
*************************************************************************/

{
  	numpts = 0;
  	currpt = 0;
  	delete[] timearr;
  	delete[] valarr;
  	delete[] interval;
};
