#include <iostream>

#include "path/linearspline.h"

using namespace std;

void linearspline::delete_knot(int index)

/************************************************************************
  Deletes the index-th knot from the list of knots in the spline, and
  adjusts the various knot/value arrays appropriately.
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

void linearspline::insert_knot(double knottime, double knotval)
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

void linearspline::set_time(int index, double time)
{
	if (index < 0 || index > numpts-1) {
		cout << "Specified index outside acceptable range" << endl;
		return;
	}
	change_knot_time(index, time);
}

void linearspline::set_value(int index, double value)
{
	if (index < 0 || index > numpts-1) {
		cout << "Specified index outside acceptable range" << endl;
		return;
	}
	change_knot_value(index, value);
}

double linearspline::get_value_at_index(int index)
{
	if (index < 0) {
		cout << "Specified index less than 0, returning first value" 
				<< endl;
		return get_knot_value(0);
	} else if (index > numpts-1) {
		cout << 
	"Specified index greater than last index, returning last value" << endl;
		return get_knot_value(numpts-1);
	} else return get_knot_value(index);
}	

double linearspline::get_time_at_index(int index)
{
	if (index < 0) {
		cout << "Specified index less than 0, returning first time" 
				<< endl;
		return timearr[0];
	} else if (index > numpts-1) {
		cout << 
	"Specified index greater than last index, returning last time" << endl;
		return timearr[numpts-1];
	} else return timearr[index];
}	

path *linearspline::copy_path()
{
  	linearspline *thePath = new linearspline;
  	if (!thePath) {
      		cout << 
	"linearspline: Failure to create new linearspline during copy" << endl;
      		return 0;
  	}
  	int n = get_numpts();
  	for (int i = 0; i < n; i++) {
      		double copytime = get_knot_time(i);
      		double copyval = get_knot_value(i);
      		thePath->insert_knot(copytime,copyval);
    	}
  	return thePath;
}

double linearspline::get_value(double currenttime)
{
	double retval = 0.0;
	if (!numpts) {
		retval = 0.0;
	} else if (numpts == 1) {
		retval = get_knot_value(0);
	} else {
		if (currenttime < timearr[0]) retval = get_knot_value(0);
		else if (currenttime > timearr[numpts-1]) retval = 
							get_knot_value(numpts-1);
		else {
		   int	i;
		   for (i = 0; i < numpts; i++) {
			if (timearr[i] > currenttime) break;
		   }
		   double slope = (valarr[i] - valarr[i-1]) / 
				   (timearr[i] - timearr[i-1]);
		   double t = currenttime - timearr[i-1];
		   retval = slope * t + valarr[i-1];
		}
	}
	return retval;
}

/*************************************************************************/

double wrapped_linearspline::get_value(double currenttime)
{
	double t, slope, retval = 0.0;
	if (!numpts) {
		retval = 0.0;
	} else if (numpts == 1) {
		retval = get_knot_value(0);
	} else {
		if (currenttime < timearr[0]) retval = get_knot_value(0);
		else if (currenttime > timearr[numpts-1]) retval = 
							get_knot_value(numpts-1);
		else {
		   int	i;
		   for (i = 0; i < numpts; i++) {
			if (timearr[i] > currenttime) break;
		   }
			if(valarr[i] == valarr[i-1]) {
				retval = valarr[i];
			} else if(valarr[i] > valarr[i-1]) {
				if(direction[i-1] >0) {	// normal
		   			slope = (valarr[i] - valarr[i-1]) / 
			   			(timearr[i] - timearr[i-1]);
		   			t = currenttime - timearr[i-1];
		   			retval = slope * t + valarr[i-1];
				} else if(direction[i-1] <0) {	// forced wrap downward
		   			slope = (valarr[i] - (valarr[i-1] + wrap_max - wrap_min)) / 
			   			(timearr[i] - timearr[i-1]);
		   			t = currenttime - timearr[i-1];
		   			retval = slope * t + (valarr[i-1] + wrap_max - wrap_min);
				} else {			// check global wrap direction
					if(default_direction > 0) {	// normal
		   				slope = (valarr[i] - valarr[i-1]) / 
			   				(timearr[i] - timearr[i-1]);
		   				t = currenttime - timearr[i-1];
		   				retval = slope * t + valarr[i-1];
					} else if(default_direction < 0) {	// forced wrap
		   				slope = (valarr[i] - (valarr[i-1] + wrap_max - wrap_min)) / 
			   				(timearr[i] - timearr[i-1]);
		   				t = currenttime - timearr[i-1];
		   				retval = slope * t + (valarr[i-1] + wrap_max - wrap_min);
					} else {				// find shortest distance
						if(fabs(valarr[i] - valarr[i-1]) > (wrap_max - wrap_min)/2.0) {	// need to wrap
		   					slope = (valarr[i] - (valarr[i-1] + wrap_max - wrap_min)) / 
			   					(timearr[i] - timearr[i-1]);
		   					t = currenttime - timearr[i-1];
		   					retval = slope * t + (valarr[i-1] + wrap_max - wrap_min);
						} else {
		   					slope = (valarr[i] - valarr[i-1]) / 
			   					(timearr[i] - timearr[i-1]);
		   					t = currenttime - timearr[i-1];
		   					retval = slope * t + valarr[i-1];
						}
					}
				}
			} else if(valarr[i] < valarr[i-1]) {
				if(direction[i-1] <0) {	// normal
		   			slope = (valarr[i] - valarr[i-1]) / 
			   			(timearr[i] - timearr[i-1]);
		   			t = currenttime - timearr[i-1];
		   			retval = slope * t + valarr[i-1];
				} else if(direction[i-1] >0) {	// forced wrap upward
		   			slope = (valarr[i] - valarr[i-1] + wrap_max - wrap_min) / 
			   			(timearr[i] - timearr[i-1]);
		   			t = currenttime - timearr[i-1];
		   			retval = slope * t + (valarr[i-1] + wrap_max - wrap_min);
				} else {			// check global wrap direction
					if(default_direction < 0) {	// normal
		   				slope = (valarr[i] - valarr[i-1]) / 
			   				(timearr[i] - timearr[i-1]);
		   				t = currenttime - timearr[i-1];
		   				retval = slope * t + valarr[i-1];
					} else if(default_direction > 0) {	// forced wrap
		   				slope = (valarr[i] - valarr[i-1] + wrap_max - wrap_min) / 
			   				(timearr[i] - timearr[i-1]);
		   				t = currenttime - timearr[i-1];
		   				retval = slope * t + (valarr[i-1] + wrap_max - wrap_min);
					} else {				// find shortest distance
						if(fabs(valarr[i] - valarr[i-1]) > (wrap_max - wrap_min)/2.0) {	// need to wrap
		   					slope = (valarr[i] - valarr[i-1] + wrap_max - wrap_min) / 
			   					(timearr[i] - timearr[i-1]);
		   					t = currenttime - timearr[i-1];
		   					retval = slope * t + (valarr[i-1] + wrap_max - wrap_min);
						} else {
		   					slope = (valarr[i] - valarr[i-1]) / 
			   					(timearr[i] - timearr[i-1]);
		   					t = currenttime - timearr[i-1];
		   					retval = slope * t + valarr[i-1];
						}
					}
				}
			}
		}
	}
	if(wrap_flag) retval = wrap_value(retval); //fmod(retval-wrap_min, wrap_max-wrap_min) + wrap_min;
	return retval;
}

void wrapped_linearspline::delete_knot(int index)

/************************************************************************
  Deletes the index-th knot from the list of knots in the spline, and
  adjusts the various knot/value arrays appropriately.
*************************************************************************/

{
        if (index < 0 || index > numpts-1) {
                cout << "Specified index outside acceptable range" << endl;
                return;
        }
        for (int i = index; i < numpts; i++) {
                timearr[i] = timearr[i+1];
                valarr[i] = valarr[i+1];
		direction[i] = direction[i+1];
        }
        timearr[numpts-1] = valarr[numpts-1] = 0.0;
	direction[numpts-1] = 0;
        numpts--;
};

void wrapped_linearspline::insert_knot(double knottime, double knotval, int knotdir)

/************************************************************************ 
  Insert a knot into a spline.  Locates the interval at which the
  new knot should be inserted, and creates a new knot at that time,
  adjusting all following knots appropriately.

  Parameter       Type        Mode    Description
  ----------------------------------------------------------------------
  knottime        double      In      Time of new knot
  knotval         double      In      Value of new knot
  knotdir         int         In      Direction of traverse to next knot (-1=down, 0=?, +1=up)
*************************************************************************/

{
        if (check_time(knottime)) return;

        if (numpts == nalloc) more_mem();

        /* find 1st index beyond where knottime */
        /* corresponds */
        int i = 0;
        while ((timearr[i] < knottime) && (i < numpts)) {
                i = i + 1;
        }

        /* move all 'higher' knots to make room for */
        /* new knot */
        for (int j = numpts; j > i; j--) {
                timearr[j] = timearr[j-1];
                valarr[j] = valarr[j-1];
		direction[j] = direction[j-1];
        }
	
        /* insert the knot */
        timearr[i] = knottime;
	if(wrap_flag) knotval = wrap_value(knotval); //fmod(knotval-wrap_min, wrap_max-wrap_min) + wrap_min;
        valarr[i] = knotval;
	if (knotdir > 1) knotdir = 1;
	if (knotdir < -1) knotdir = -1;
	direction[i] = knotdir;

        numpts++;
        currpt = i;

};

void wrapped_linearspline::more_mem()
{
	linearspline::more_mem();
	direction = (int *)realloc(direction, sizeof(int)*nalloc);
}

void wrapped_linearspline::change_knot_direction(int index, int knotdir)
{
        if (index < 0 || index > numpts-1) {
                cout << "Specified index outside acceptable range" << endl;
                return;
        }
	if (knotdir > 1) knotdir = 1;
	if (knotdir < -1) knotdir = -1;

	direction[index] = knotdir;
}

double wrapped_linearspline::wrap_value(double input_value)
{
	if(wrap_flag) {
		double mod_val = fmod(input_value - wrap_min, wrap_max-wrap_min);
		if(mod_val < 0.0) {
			mod_val += wrap_max;
		} else {
			mod_val += wrap_min;
		}
		return(mod_val);
	} else {
		return(input_value);
	}
}

void wrapped_linearspline::change_knot_value(int index, double knotval)
{
        if (index < 0 || index > numpts-1) {
                cout << "Specified index outside acceptable range" << endl;
                return;
        }
	knotval = wrap_value(knotval);
        valarr[index] = knotval;
}

void wrapped_linearspline::set_global_direction(int knotdir)
{
	if (knotdir > 1) knotdir = 1;
	if (knotdir < -1) knotdir = -1;

	default_direction = knotdir;
}

int wrapped_linearspline::get_global_direction() {
	return(default_direction);
}

int wrapped_linearspline::get_knot_direction(int index)
{
        if (index < 0 || index > numpts-1) {
                cout << "Specified index outside acceptable range" << endl;
                return(0);
        }
	return(direction[index]);
}

path *wrapped_linearspline::copy_path()

/************************************************************************ 
  Copy a spline, returning a pointer to the copy.

  Parameter       Type        Mode    Description
  ----------------------------------------------------------------------
  RETURN VALUE    spline*     Out     Pointer to copy of spline
*************************************************************************/

{
  wrapped_linearspline *thePath;
  int n, i, copydir;
  double copytime, copyval;

  thePath = new wrapped_linearspline(wrap_min, wrap_max);
  thePath->set_global_direction(default_direction);
  if (thePath == NULL)
    {
      fprintf(stderr,"spline: Failure during creation of new wrapped_linearspline during copy\n");
      fprintf(stderr,"        Spline not copied\n");
      return NULL;
    }
  n = get_numpts();
  for (i = 0; i < n; i++)
    {
      copytime = get_knot_time(i);
      copyval = get_knot_value(i);
      copydir = get_knot_direction(i);
      thePath->insert_knot(copytime,copyval,copydir);
    }
  return thePath;
}

int wrapped_linearspline::parse_in(Dataport *fileptr)

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
  int deg, dir;
  double pttime, ptval;

	if(!get_next_token(fileptr, token)) {
		fprintf(stderr," Whoops - Unexpected EOF in wrapped_linearspline file\n");
		return(FALSE);
	}
	while(strcmp(token, "END")) {
		if(!strcmp(token, "DEGREE")) {
			if(!get_next_token(fileptr, token)) {
				fprintf(stderr," Whoops - Unexpected EOF in wrapped_linearspline file\n");
				return(FALSE);
			}
			deg = atoi(token);
			if (deg < 0) {
				fprintf(stderr,"wrapped_linearspline: Negative value for degree of curve, %d, is incorrect\n",deg);
				fprintf(stderr,"          Unable to continue processing\n");
				return(FALSE);
			}
			set_degree(deg);
		} else if(!strcmp(token, "WRAPPED")) {
			wrap_flag = TRUE;
		} else if(!strcmp(token, "WRAP_MIN")) {
			if(!get_next_token(fileptr, token)) {
				fprintf(stderr," Whoops - Unexpected EOF in wrapped_linearspline file\n");
				return(FALSE);
			}
	    		wrap_min = atof(token);
		} else if(!strcmp(token, "WRAP_MAX")) {
			if(!get_next_token(fileptr, token)) {
				fprintf(stderr," Whoops - Unexpected EOF in wrapped_linearspline file\n");
				return(FALSE);
			}
	    		wrap_max = atof(token);
		} else if(!strcmp(token, "WRAP_DIR")) {
			if(!get_next_token(fileptr, token)) {
				fprintf(stderr," Whoops - Unexpected EOF in wrapped_linearspline file\n");
				return(FALSE);
			}
	    		default_direction = atoi(token);
		} else if(strcmp(token, "END")) {
			pttime = atof(token);
			if(!get_next_token(fileptr, token)) {
				fprintf(stderr," Whoops - Unexpected EOF in wrapped_linearspline file\n");
				return(FALSE);
			}
			ptval = atof(token);
	    		if(wrap_flag) {
				if(!get_next_token(fileptr, token)) {
					fprintf(stderr," Whoops - Unexpected EOF in wrapped_linearspline file\n");
					return(FALSE);
				}
				dir = atoi(token);
	    		} else {
				dir = 0;
	    		}
			insert_knot(pttime, ptval, dir);
		}
	}
	return(TRUE);
};

int wrapped_linearspline::parse_out(Dataport *fileptr, int)

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

	put_token(fileptr, (char *)"DEGREE");
	get_degree(deg);
	sprintf(token, "%d\n", deg);
	put_token(fileptr, token);
	if(wrap_flag) {
		put_token(fileptr, (char *)"WRAPPED\n");
		sprintf(token, "WRAP_MIN %14g\n", wrap_min);
		put_token(fileptr, token);
		sprintf(token, "WRAP_MAX %14g\n", wrap_max);
		put_token(fileptr, token);
		sprintf(token, "WRAP_DIR %2i\n", default_direction);
		put_token(fileptr, token);
	}
	for (i = 0; i < numpts; i++) {
		sprintf(token, "%f", timearr[i]);
		put_token(fileptr, token);
		sprintf(token, "%f", valarr[i]);
		put_token(fileptr, token);
		if(wrap_flag) {
			sprintf(token, "%d", direction[i]);
			put_token(fileptr, token);
		}
		put_token(fileptr, "\n");
	}
	put_token(fileptr, (char *)"END");
	return(TRUE);
};

wrapped_linearspline::wrapped_linearspline(double minval, double maxval, int initial_size)
	:linearspline(initial_size)
{
	wrap_flag = TRUE;
	wrap_min = minval;
	wrap_max = maxval;
	default_direction = 0;
	direction = new int[initial_size];
}

wrapped_linearspline::~wrapped_linearspline()
{
	delete(direction);
}

int wrapped_linearspline::self_test(void) 
{
	fprintf(stderr,"Running wrapped_linearspline::self_test method.\n");

	int	status = TRUE;
	wrapped_linearspline *wls = new wrapped_linearspline(-90.0, 120.0);

	// test pathtype
	if(wls->pathtype() != WRAPPED_LINEARSPLINE) {
		fprintf(stderr,"Whoops - wrapped_linearspline::self_test failed.\n");
		fprintf(stderr,"Expected pathtype to be WRAPPED_LINEARSPLINE(%d) but was %d\n", WRAPPED_LINEARSPLINE, wls->pathtype());
		status = FALSE;
	}

	// test default wrap direction
	if(wls->get_global_direction() != 0) {
		fprintf(stderr,"Whoops - wrapped_linearspline::self_test failed.\n");
		fprintf(stderr,"Expected initialized wrap direction to be 0 but was %d\n", wls->get_global_direction());
		status = FALSE;
	}

	// test default wrap flag - better be true
	if(!wls->wrap_flag) {
		fprintf(stderr,"Whoops - wrapped_linearspline::self_test failed.\n");
		fprintf(stderr,"Expected initialized wrap flag to be TRUE but was %d\n", wls->wrap_flag);
		status = FALSE;
	}

	// test initial wrap limits
	if(wls->wrap_min != -90.0) {
		fprintf(stderr,"Whoops - wrapped_linearspline::self_test failed.\n");
		fprintf(stderr,"Expected initialized wrap minimum to be -90.0 but was %f\n", wls->wrap_min);
		status = FALSE;
	}
	if(wls->wrap_max != 120.0) {
		fprintf(stderr,"Whoops - wrapped_linearspline::self_test failed.\n");
		fprintf(stderr,"Expected initialized wrap maximum to be -90.0 but was %f\n", wls->wrap_max);
		status = FALSE;
	}

	// test return value with no knots
	if(wls->get_value(137.315) != 0.0) {
		fprintf(stderr,"Whoops - wrapped_linearspline::self_test failed.\n");
		fprintf(stderr,"Expected spline value with no knots to be 0.0 but was %f\n", wls->get_value(137.315));
		status = FALSE;
	}

	// test return value with a single knot
	wls->insert_knot(20.0, 33.0);
	if(wls->get_numpts() != 1) {
		fprintf(stderr,"Whoops - wrapped_linearspline::self_test failed.\n");
		fprintf(stderr,"Expected number of knots in spline to be 1 but was %d\n", wls->get_numpts());
		status = FALSE;
	}
	if(wls->get_value(137.315) != 33.0) {
		fprintf(stderr,"Whoops - wrapped_linearspline::self_test failed.\n");
		fprintf(stderr,"Expected spline value with one knot to be 33.0 but was %f\n", wls->get_value(137.315));
		status = FALSE;
	}
	if(wls->get_value(-137.315) != 33.0) {
		fprintf(stderr,"Whoops - wrapped_linearspline::self_test failed.\n");
		fprintf(stderr,"Expected spline value with one knot to be 33.0 but was %f\n", wls->get_value(-137.315));
		status = FALSE;
	}

	// add another knot and test return values
	wls->insert_knot(40.0, -33.0);
	if(wls->get_numpts() != 2) {
		fprintf(stderr,"Whoops - wrapped_linearspline::self_test failed.\n");
		fprintf(stderr,"Expected number of knots in spline to be 2 but was %d\n", wls->get_numpts());
		status = FALSE;
	}
	if(wls->get_value(137.315) != -33.0) {
		fprintf(stderr,"Whoops - wrapped_linearspline::self_test failed.\n");
		fprintf(stderr,"Expected spline value with two knots to be -33.0 but was %f\n", wls->get_value(137.315));
		status = FALSE;
	}
	if(wls->get_value(-137.315) != 33.0) {
		fprintf(stderr,"Whoops - wrapped_linearspline::self_test failed.\n");
		fprintf(stderr,"Expected spline value with two knots to be 33.0 but was %f\n", wls->get_value(-137.315));
		status = FALSE;
	}
	if(wls->get_value(30.0) != 0.0) {
		fprintf(stderr,"Whoops - wrapped_linearspline::self_test failed.\n");
		fprintf(stderr,"Expected spline value with two knots to be 0.0 but was %f\n", wls->get_value(30.0));
		status = FALSE;
	}

	// force a wrap on first point and test again
	wls->change_knot_direction(0,1);
	if(wls->get_numpts() != 2) {
		fprintf(stderr,"Whoops - wrapped_linearspline::self_test failed.\n");
		fprintf(stderr,"Expected number of knots in spline to be 2 but was %d\n", wls->get_numpts());
		status = FALSE;
	}
	if(wls->get_value(137.315) != -33.0) {
		fprintf(stderr,"Whoops - wrapped_linearspline::self_test failed.\n");
		fprintf(stderr,"Expected spline value with two knots to be -33.0 but was %f\n", wls->get_value(137.315));
		status = FALSE;
	}
	if(wls->get_value(-137.315) != 33.0) {
		fprintf(stderr,"Whoops - wrapped_linearspline::self_test failed.\n");
		fprintf(stderr,"Expected spline value with two knots to be 33.0 but was %f\n", wls->get_value(-137.315));
		status = FALSE;
	}
	if(wls->get_value(30.0) != 105.0) {
		fprintf(stderr,"Whoops - wrapped_linearspline::self_test failed.\n");
		fprintf(stderr,"Expected spline value with two knots to be 105.0 but was %f\n", wls->get_value(30.0));
		status = FALSE;
	}

	// check global wrapping flag
	wls->change_knot_direction(0,0);
	wls->set_global_direction(1);
	if(wls->get_numpts() != 2) {
		fprintf(stderr,"Whoops - wrapped_linearspline::self_test failed.\n");
		fprintf(stderr,"Expected number of knots in spline to be 2 but was %d\n", wls->get_numpts());
		status = FALSE;
	}
	if(wls->get_value(137.315) != -33.0) {
		fprintf(stderr,"Whoops - wrapped_linearspline::self_test failed.\n");
		fprintf(stderr,"Expected spline value with two knots to be -33.0 but was %f\n", wls->get_value(137.315));
		status = FALSE;
	}
	if(wls->get_value(-137.315) != 33.0) {
		fprintf(stderr,"Whoops - wrapped_linearspline::self_test failed.\n");
		fprintf(stderr,"Expected spline value with two knots to be 33.0 but was %f\n", wls->get_value(-137.315));
		status = FALSE;
	}
	if(wls->get_value(30.0) != 105.0) {
		fprintf(stderr,"Whoops - wrapped_linearspline::self_test failed.\n");
		fprintf(stderr,"Expected spline value with two knots to be 105.0 but was %f\n", wls->get_value(30.0));
		status = FALSE;
	}


	// check global wrapping flag
	wls->change_knot_direction(0,0);
	wls->set_global_direction(-1);
	if(wls->get_numpts() != 2) {
		fprintf(stderr,"Whoops - wrapped_linearspline::self_test failed.\n");
		fprintf(stderr,"Expected number of knots in spline to be 2 but was %d\n", wls->get_numpts());
		status = FALSE;
	}
	if(wls->get_value(137.315) != -33.0) {
		fprintf(stderr,"Whoops - wrapped_linearspline::self_test failed.\n");
		fprintf(stderr,"Expected spline value with two knots to be -33.0 but was %f\n", wls->get_value(137.315));
		status = FALSE;
	}
	if(wls->get_value(-137.315) != 33.0) {
		fprintf(stderr,"Whoops - wrapped_linearspline::self_test failed.\n");
		fprintf(stderr,"Expected spline value with two knots to be 33.0 but was %f\n", wls->get_value(-137.315));
		status = FALSE;
	}
	if(wls->get_value(30.0) != 0.0) {
		fprintf(stderr,"Whoops - wrapped_linearspline::self_test failed.\n");
		fprintf(stderr,"Expected spline value with two knots to be 0.0 but was %f\n", wls->get_value(30.0));
		status = FALSE;
	}

	// stretch the values out 
	wls->change_knot_value(0,80.0);
	wls->change_knot_value(1,-80.0);
	wls->change_knot_direction(0,0);
	wls->set_global_direction(0);	// should wrap to take shortest path
	if(wls->get_numpts() != 2) {
		fprintf(stderr,"Whoops - wrapped_linearspline::self_test failed.\n");
		fprintf(stderr,"Expected number of knots in spline to be 2 but was %d\n", wls->get_numpts());
		status = FALSE;
	}
	if(wls->get_value(137.315) != -80.0) {
		fprintf(stderr,"Whoops - wrapped_linearspline::self_test failed.\n");
		fprintf(stderr,"Expected spline value with two knots to be -80.0 but was %f\n", wls->get_value(137.315));
		status = FALSE;
	}
	if(wls->get_value(-137.315) != 80.0) {
		fprintf(stderr,"Whoops - wrapped_linearspline::self_test failed.\n");
		fprintf(stderr,"Expected spline value with two knots to be 80.0 but was %f\n", wls->get_value(-137.315));
		status = FALSE;
	}
	if(wls->get_value(30.0) != 105.0) {
		fprintf(stderr,"Whoops - wrapped_linearspline::self_test failed.\n");
		fprintf(stderr,"Expected spline value with two knots to be 105.0 but was %f\n", wls->get_value(30.0));
		status = FALSE;
	}

	// appears to wrap too many times but give right answer at midpoint so try another point
	if(wls->get_value(25.0) != 92.5) {
		fprintf(stderr,"Whoops - wrapped_linearspline::self_test failed.\n");
		fprintf(stderr,"Expected spline value with two knots to be 92.5 but was %f\n", wls->get_value(25.0));
		status = FALSE;
	}
	

	// force a wrap on first point and test again
	wls->change_knot_direction(0,1);
	if(wls->get_numpts() != 2) {
		fprintf(stderr,"Whoops - wrapped_linearspline::self_test failed.\n");
		fprintf(stderr,"Expected number of knots in spline to be 2 but was %d\n", wls->get_numpts());
		status = FALSE;
	}
	if(wls->get_value(137.315) != -80.0) {
		fprintf(stderr,"Whoops - wrapped_linearspline::self_test failed.\n");
		fprintf(stderr,"Expected spline value with two knots to be -80.0 but was %f\n", wls->get_value(137.315));
		status = FALSE;
	}
	if(wls->get_value(-137.315) != 80.0) {
		fprintf(stderr,"Whoops - wrapped_linearspline::self_test failed.\n");
		fprintf(stderr,"Expected spline value with two knots to be 80.0 but was %f\n", wls->get_value(-137.315));
		status = FALSE;
	}
	if(wls->get_value(30.0) != 105.0) {
		fprintf(stderr,"Whoops - wrapped_linearspline::self_test failed.\n");
		fprintf(stderr,"Expected spline value with two knots to be 105.0 but was %f\n", wls->get_value(30.0));
		status = FALSE;
	}

	// check global wrapping flag
	wls->change_knot_direction(0,0);
	wls->set_global_direction(1);
	if(wls->get_numpts() != 2) {
		fprintf(stderr,"Whoops - wrapped_linearspline::self_test failed.\n");
		fprintf(stderr,"Expected number of knots in spline to be 2 but was %d\n", wls->get_numpts());
		status = FALSE;
	}
	if(wls->get_value(137.315) != -80.0) {
		fprintf(stderr,"Whoops - wrapped_linearspline::self_test failed.\n");
		fprintf(stderr,"Expected spline value with two knots to be -80.0 but was %f\n", wls->get_value(137.315));
		status = FALSE;
	}
	if(wls->get_value(-137.315) != 80.0) {
		fprintf(stderr,"Whoops - wrapped_linearspline::self_test failed.\n");
		fprintf(stderr,"Expected spline value with two knots to be 80.0 but was %f\n", wls->get_value(-137.315));
		status = FALSE;
	}
	if(wls->get_value(30.0) != 105.0) {
		fprintf(stderr,"Whoops - wrapped_linearspline::self_test failed.\n");
		fprintf(stderr,"Expected spline value with two knots to be 105.0 but was %f\n", wls->get_value(30.0));
		status = FALSE;
	}


	// check global wrapping flag
	wls->change_knot_direction(0,0);
	wls->set_global_direction(-1);
	if(wls->get_numpts() != 2) {
		fprintf(stderr,"Whoops - wrapped_linearspline::self_test failed.\n");
		fprintf(stderr,"Expected number of knots in spline to be 2 but was %d\n", wls->get_numpts());
		status = FALSE;
	}
	if(wls->get_value(137.315) != -80.0) {
		fprintf(stderr,"Whoops - wrapped_linearspline::self_test failed.\n");
		fprintf(stderr,"Expected spline value with two knots to be -80.0 but was %f\n", wls->get_value(137.315));
		status = FALSE;
	}
	if(wls->get_value(-137.315) != 80.0) {
		fprintf(stderr,"Whoops - wrapped_linearspline::self_test failed.\n");
		fprintf(stderr,"Expected spline value with two knots to be 80.0 but was %f\n", wls->get_value(-137.315));
		status = FALSE;
	}
	if(wls->get_value(30.0) != 0.0) {
		fprintf(stderr,"Whoops - wrapped_linearspline::self_test failed.\n");
		fprintf(stderr,"Expected spline value with two knots to be 0.0 but was %f\n", wls->get_value(30.0));
		status = FALSE;
	}

	// test setting values out of range
	wls->change_knot_value(0,180.0);
	wls->change_knot_value(1,-180.0);
	wls->change_knot_direction(0,0);
	wls->set_global_direction(0);	// should wrap to take shortest path
	if(wls->get_numpts() != 2) {
		fprintf(stderr,"Whoops - wrapped_linearspline::self_test failed.\n");
		fprintf(stderr,"Expected number of knots in spline to be 2 but was %d\n", wls->get_numpts());
		status = FALSE;
	}
	if(wls->get_value(137.315) != 30.0) {
		fprintf(stderr,"Whoops - wrapped_linearspline::self_test failed.\n");
		fprintf(stderr,"Expected spline value with two knots to be 30.0 but was %f\n", wls->get_value(137.315));
		status = FALSE;
	}
	if(wls->get_value(-137.315) != -30.0) {
		fprintf(stderr,"Whoops - wrapped_linearspline::self_test failed.\n");
		fprintf(stderr,"Expected spline value with two knots to be -30.0 but was %f\n", wls->get_value(-137.315));
		status = FALSE;
	}
	if(wls->get_value(30.0) != 0.0) {
		fprintf(stderr,"Whoops - wrapped_linearspline::self_test failed.\n");
		fprintf(stderr,"Expected spline value with two knots to be 0.0 but was %f\n", wls->get_value(30.0));
		status = FALSE;
	}

	// appears to wrap too many times but give right answer at midpoint so try another point
	if(wls->get_value(25.0) != -15.0) {
		fprintf(stderr,"Whoops - wrapped_linearspline::self_test failed.\n");
		fprintf(stderr,"Expected spline value with two knots to be -15.0 but was %f\n", wls->get_value(25.0));
		status = FALSE;
	}
	
	Dataport *dp = new FILE_Dataport();
	dp->wopen("/tmp/test_wrapped_path.dat");
	wls->parse_out(dp, FALSE);

	delete(dp);
	delete(wls);

	return(status);
}
