// pathedit.h		Path Editor Window Classes
//
// Written by Dave Kagels for the ASV task - 4/23/94
//
// Copyright (C) 1994, California Institute of Technology
// All rights reserved.

#ifndef	_PATHEDIT_H_
#define	_PATHEDIT_H_

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "motif/TopLevelShell.h"		// For shell
#include "motif/BulletinBoard.h"		// For form
#include "motif/GPClasses.h"			// For buttons
#include "motif/PClasses.h"			// For text

#include "gui/UserCallback.h"
#include "gui/RowOfButtons.h"
#include "gui/HelpTool.h"

#include "time_value.h"
#include "path/ParEdit.h"			// For path editors

// This class defines a node in a linked list that stores information about
// each path and the editor for it.

class par_node {

	ParEdit		*par;		// pointer to editor interface object
	String		name;		// name of the path
	par_node	*next;		// linked list "next" pointer

    public:

	// Return the pointer to the next node
	par_node *get_next() { return next; }

	// Return the name of the path
	String	get_name() { return name; }

	// Return the pointer to the path editor interface object
	ParEdit *get_par() { return par; }

	// Find the name n (I) in the linked list
	// Return found/not found (T/F)
	Boolean	find_name(String n) { par_node *c; Boolean found;
			found=False;
			for(c=this;c;c=c->next)
				if(!strcmp(n,c->name)) { found=True; break; }
			return found; }

	// Set the time for all path editors in list to t (I)
	void	set_time(double t) { par_node *c;
			for(c=this;c;c=c->next)
				if(c->par) c->par->set_time(t); }

	// Set the delta for all path editors in list to t (I)
	void	set_delta(double t) { par_node *c;
			for(c=this;c;c=c->next)
				if(c->par) c->par->set_delta(t); }

	// Expose all path editors in list at time t (I)
	void	expose(double t) { par_node *c;
			for(c=this;c;c=c->next)
				if(c->par) c->par->expose(t); }

	// Remove this node from the list with head node l (I)
	void	remove(par_node *&l) { par_node *c;
			if(this==l) { l=next; next=NULL; }
			else for(c=l;c->next;c=c->next)
				if(this==c->next) {
					c->next=next; next=NULL; break; }
			}

	// Remove the node with ParEdit p (I) from the list with head node l (I)
	// Return a pointer to the removed node
	static par_node *remove_par(par_node *&l, ParEdit *p) { par_node *c,*d;
			d=NULL;
			if(p==l->par) { d=l; l=l->next; d->next=NULL; }
			else for(c=l;c->next;c=c->next)
				if(p==c->next->par) { d=c->next;
					c->next=c->next->next; d->next=NULL; break; }
			return d; }

	// Constructor: initialize to NULL
	par_node() { par=NULL; next=NULL; name=NULL; }

	// Constructor: initialize with ParEdit p (I), name n (I), in list l (I)
	par_node(ParEdit *p, par_node *&l, String n) {	// init & place at beginning
		par=p; next=l; l=this;
		name=new char[strlen(n)+1]; strcpy(name,n); }

	// Destructor: free data for clean deletion
	~par_node() { if(par) delete par; if(name) delete[] name; }
	};


// This class contains a user interface for entering and editing paths
// of type imgtiepath and wvspline for the Animator application.

// used to perform an action when a TimeBarEvent arrives via ButtonPress
typedef void (*TimeBarAction)(double time1, double time2, XtPointer);

class pathedit {

    protected:

	void	init();
	void	set_delta_text(double d);
	void	set_scroll_value();
	void	set_scroll_size();

	// Set the Visible Range scale based on the value of delta
	void	set_delta_scale() { delta_scale.value=(int)(0.0-delta*100.0); }

	// Callback functions used internally to the path editor object:
	static	void	par_done(WObject *, XtPointer);
	static	void	time_drag(ScrollBar *, XtPointer);
	static	void	time_changed(ScrollBar *, XtPointer);
	static	void	delta_drag(WObject *, XtPointer);
	static	void	delta_changed(WObject *, XtPointer);
	static	void	delta_text_changed(WObject *, XtPointer);

	static  void    TimeBarEvent(double, double, XtPointer);
	TimeBarAction   timebar_action;

	// set these to overide the default done action
	UserCB user_done;
	XtPointer user_done_data;
	int drawing_area_size;
	SetDrawFunc draw_func;
	XtPointer draw_func_data;

	par_node	*par_list;			// head of linked list
	String		*nlist;				// phoneme name list
	double		s_time,delta;			// display range
	double		start,end;			// start & end times
	Boolean		delta_text_update,scroll_update; // update flags
	String delta_label;				// label of delta

	// Widgets and other objects in the path editor interface:
	TopLevelShell		shell;
	Form			form;
	PanedWindow		panes;
	ScrollBar		time_scroll;
	Scale			delta_scale;
	Label			vis_lab,delta_lab;
	TextField		delta_text;
	Separator		time_sep;
	TimeBar			*tb;

	Separator 		sep;

	RowOfButtons		prob;
	Form			*pbuttonForm;

	HelpToolDialog  helpTool;
	static	void	ClearCB(WObject *, XtPointer, XtPointer);
	static	void	DoneCB(WObject *, XtPointer, XtPointer);
	static	void	HelpCB(WObject *, XtPointer, XtPointer);

        static void VisibleAction(Widget, XtPointer, XEvent*, Boolean*);

    public:

	void set_timebar_label(String s) { tb->set_label(s); }

	void set_timebar_event_action(TimeBarAction);

	// Check if path with name n (I) is being edited.  Return T/F
	Boolean	is_edited(String n) {			// only after create
			return par_list->find_name(n); }

	// Redraw all the paths
	void	redraw();

	// Display the path editor window
	void	popup() { if((Widget)shell) XtPopup(shell,XtGrabNone); }

	// Remove the path editor window from the display
	void	popdown() { if((Widget)shell) XtPopdown(shell); }

	// Set the phoneme name list to l (I)
	void	set_name_list(String *l) { nlist=l; }	// do before add_path

	Form*	create(String n, WObject &p);
	Form*	create(String n, Widget &p);
	Form*	create_unmanaged(String n, WObject &p);
	Form*	create_unmanaged(String n, Widget &p);
// only after create
	SplineEdit* add_path(vis_spline *wv, String n, char *color=0);	
	SplineEdit* replace_path(vis_spline *wv, String n, char *color=0);
	void remove_path(ParEdit*);	// remove current path with ParEdit pe
	void remove_all_paths(); // remove all paths

	void    get_start_end(double &start, double &end);
	void	set_start_end(double s, double e);
	void	call_done() { DoneCB(0, 0, this); }

	void set_done_callback(UserCB user_cb, XtPointer userdata=0) {
			user_done = user_cb, user_done_data = userdata; }

	// create an additional drawing area below the path drawing area
	// must call before adding path
	void set_drawing_area(int size) { drawing_area_size = size; }

	// use this to have the editor do additional drawing in the drawing area
	// must call before add_path
	void set_draw_func(SetDrawFunc drawfunc, XtPointer data) {
		draw_func = drawfunc; draw_func_data = data; }

	void set_delta_label(String lab) { delta_label = lab; }

	// call TimeBars like function to draw an indicator at time
	void draw_time_indicator(double time) 
	{ tb->draw_time_indicator(time); }


	// must call after creating the path
	void always_on_top() { XtAddEventHandler(shell.get_widget(), 
		VisibilityChangeMask, False, VisibleAction, this); }


	// Constructor: initialize
	pathedit() { init(); }

	// Destructor: empty function
	~pathedit() { ; }
	};

// telemedit uses days as the base for time keeping, therefore your paths
// should also be specified in terms of days.  However, we have added a
// time scale factor to allow different units for paths and the timebar.
// This is currently limited to a single value applied to all paths.  In
// a better system, the scale would be for each path, allowing differently
// defined paths to be compared.  Next revision, perhaps (JRW - 08 Sep 02)

// As of 24 Sep 2002, the ParEdit class has a scale and offset for 
// aligning paths with different ranges and units.  Such applies to the
// time bar as well.  Thus, the scale here may be unnecessary.  The
// start and end times may be specified in seconds or character string.
// The ScEventTime objects used for converting back and forth may be
// extended with NAIF/SPICE tools, in derived classes, for any formats
// and conversions desired.

// Logically, the internal units of the display and the time bar are
// seconds and these are converted to and from strings of the form:
// year:doy:hr:mn:sc.msc by the attached objects of base type ScEventTime.
// This base type uses the unix epoch time, in seconds from 1/1/1970, for
// the internal time and uses internal, often flaky, routines for 
// conversions.  For real SCET time, derive a new class from ScEventTime
// that rpovides the same conversions but uses ephemeris time and the NAIF/
// SPICE kernels.  Then, derive a new class from telemedit and TelemTimeBar 
// that uses the new SCET time class and all should be well.


class telemedit : public pathedit {

    private:

    protected:

	TextField		time_start;
	TextField		time_end;

	Form		*buttonForm;
	RowOfButtons 	rob;

	void    	update_start_end_text();
	void	set_delta_text(double d);	// hides pathedit version

	static	void	telem_time_drag(ScrollBar *, XtPointer);
	static	void	telem_time_changed(ScrollBar *, XtPointer);
	static	void	telem_time_increment(ScrollBar *, XtPointer);
	static	void	telem_time_decrement(ScrollBar *, XtPointer);
	static	void	telem_delta_drag(WObject *, XtPointer);
	static	void	telem_delta_changed(WObject *, XtPointer);
	static	void	telem_delta_text_changed(WObject *, XtPointer);

	static  void 	timeStartActivateCB(WObject *, XtPointer);
	static  void 	timeStartLosingFocusCB(WObject *, XtPointer);
	static  void 	timeEndActivateCB(WObject *, XtPointer);
	static  void 	timeEndLosingFocusCB(WObject *, XtPointer);

	static  void    UserResetCB(WObject*, XtPointer, XtPointer);
	static  void    UserZoomInCB(WObject*, XtPointer, XtPointer);
	static  void    UserZoomOutCB(WObject*, XtPointer, XtPointer);

	static  void    telem_timebar_action(double, double, XtPointer);
	double time_1, time_2;

	ScEventTime* 	start_time;	// time in seconds since 1/1/1970
	ScEventTime* 	end_time;	// time in seconds since 1/1/1970
	ScEventTime* 	temp_time;	// time in seconds since 1/1/1970

	char scet_start[25];
	char scet_end[25];
	char prev_scet_start[25];
	char prev_scet_end[25];

	double factor;			// linearity factor
	double minres;
	double time_scale_factor;	// number of path units per day
					// e.g. use 86400.0 for paths defined in seconds

	void update_display();

	// A virtual method to allow derived classes to have different type of time bar
	virtual TelemTimeBar *select_timebar() {
		return(new TelemTimeBar());
	}

public:
	virtual TimeUnits::units get_current_units();

	void set_reset_times(String s, String e) {
		strcpy(scet_start, s); strcpy(scet_end, e);
	}

	void set_reset_times(double s, double e) {
		temp_time->set_time_in_sec(s);
		strcpy(scet_start, temp_time->get_scet_time());
		temp_time->set_time_in_sec(e);
		strcpy(scet_end, temp_time->get_scet_time());
	}

	void    set_time_scale_factor(double d) { time_scale_factor = d; }

	// minres is specified in seconds
	void	set_start_end(double s, double e, double minres=1.0);
	void set_start_end(String s, String e, double minres=1.0);
	Form*	create(String n, WObject &p);
	Form*	create(String n, Widget &pw);
	Form*	create_unmanaged(String n, WObject &p);
	Form*	create_unmanaged(String n, Widget &pw);
	telemedit() : time_1(-1), time_2(-1) { 
		factor = -1; minres = -1;
		start_time = new ScEventTime();
		end_time = new ScEventTime();
		temp_time = new ScEventTime();
		buttonForm = 0; time_scale_factor = 1.0; }
	~telemedit() { }
};

#endif // _PATHEDIT_H_
