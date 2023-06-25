// ParEdit.h	Classes for editing paths
//
// Written by Dave Kagels for the ASV task - 4/13/94
//
// Copyright (C) 1994, California Institute of Technology
// All rights reserved.

#ifndef _PAREDIT_H_
#define _PAREDIT_H_

#include "motif/GPClasses.h"			// For buttons
#include "motif/BulletinBoard.h"		// For form
#include "motif/MClasses.h"			// For drawing area
#include "motif/RCTypes.h"			// For popups 

#include "gui/events.h"

#define	TFLAG	-1048576.0

// Prototype for global function:
Pixel	color_alloc(Display *d, Colormap c, int r, int g, int b);

// get a color by name. status=0 on success else status=-1
Pixel named_color_alloc(Display *d, char *name, int &status);



// ***************************** ParEdit **********************************

// This is the base class for the path editors.  It has virtual functions
// for mouse events, and common functions such as redraw and expose, and
// setting the time scale.  It contains the common parts of the interface
// and the code to create it.
// To edit various path types, path editor classes are derived from this
// base class, including a time "ruler" bar for reference.  An array of
// base class objects can be created, with elements that are specific
// path editor types or time bars, and they can all be matched up and
// controlled using the virtual or common functions in the base class.

class ParEdit {

    protected:

	void	init();
	virtual	void	setup_display();
	virtual	void	set_label()=0;

	static	void	exposed(DrawingArea *da, XtPointer);
	static	void	resized(DrawingArea *da, XtPointer);
	static	void	pressed(Widget, XtPointer, XEvent *e, Boolean *);
	static	void	moved(Widget, XtPointer, XEvent *e, Boolean *);
	static	void	released(Widget, XtPointer, XEvent *e, Boolean *);

	// Virtual mouse button handler functions: (defined in derived classes)
	virtual	void	button_pressed(int b, int x, int y, double t, double w)=0;
	virtual	void	mouse_moved(int b, int x, int y, double t, double w)=0;
	virtual	void	button_released(int b, int x, int y, double t, double w)=0;

	Display		*disp;			// display pointer
	Window		dwin;			// window of drawing area
	GC		gc;			// graphics context
	Pixmap		pmap;			// for drawing off screen
	int		bcur;			// current mouse button
	int		mousex,mousey;		// mouse cursor location
	int		oldx,oldy;		// old position
	int		vx,vy;			// drawing area size

	// Widgets (objects) common to all path editors
	Form		form;
	Label		namelab;
	DrawingArea	draw;

	double	s_time,delta;			// visible time range start & size

	// conversion from input time to internal path time is : 
	// pathtime = intime * time_scale_factor + time_offset
	double	time_scale_factor;		// scale for converting to and from path units
						// e.g. if path units are seconds and system units
						// are days, set to 86400.0 (default=1.0)
	double	time_offset;			// offset for converting to and from path units


    public:

	XtPointer	data;			// user data

	virtual	void	redraw(double t1=TFLAG, double t2=TFLAG)=0;
	virtual	void	expose(double t=TFLAG);

	// Set the visible time range to d (I)
	void	set_delta(double d) { d *= time_scale_factor; if(delta!=d) {
			delta=d; redraw(); expose(); } }

	void	set_time(double t);

	void	set_time_scale_factor(double d) { time_scale_factor = d; }
	void	set_time_offset(double d) { time_offset = d; }

	virtual	Form	*create(String name, WObject &p);


	// Constructor: initialize
	ParEdit() { init(); }

	virtual ~ParEdit();
	};


// ***************************** TimeBar **********************************
// This class is a time "ruler" bar that can be displayed with the path
// editors for reference.

#include "time_value.h"	// used for the TimeUnits enumeration

typedef void (*ButtonPressCB)(double, double, XtPointer);

class TimeBar : public ParEdit {

    protected:

	void	init();
	void	setup_display();

	// Set the label
	void set_label() { namelab.labelString = (char *)"Frame"; }

	// Empty functions:
	void	button_pressed(int, int, int, double, double);
		
	void	mouse_moved(int , int , int , double , double );
	void	button_released(int , int , int , double , double );

	XFontStruct	*finfo;			// font information
	int		f_off,f_height;	       // font offset & height in pixels

	Pixel	linecol,bgcol,textcol, markcol, indcol;	// Color indicies

	int mark_x1;
	int mark_x2;
	ButtonPressCB button_press;
	XtPointer button_press_data;

    public:
        virtual void set_time_units(TimeUnits::units) { }
        virtual TimeUnits::units get_time_units() { return TimeUnits::SEC; }

	virtual void	redraw(double t1=TFLAG, double t2=TFLAG);
	virtual void	expose(double t=TFLAG);

	void	set_label(String s) { namelab.labelString=s; }

	void set_button_press_action(ButtonPressCB bp, XtPointer ldata) {
		button_press = bp; button_press_data = ldata;
	}

	void draw_time_indicator(double time);	// draw an indicator at time

	Form	*create(String name, WObject &p);

	// Constructor: initialize
	TimeBar() { init(); }

	// Destructor: empty function
	~TimeBar() { ; }
	};

// this class creates a time bar which uses year, doy, hr, min, sec, msec as
// the scale
class TelemTimeBar : public TimeBar {
	private:
		TimeUnits::units time_units; 
	protected:
		ScEventTime *stime, *etime, *sstime;
	public:
		virtual void set_time_units(TimeUnits::units units) { 
							time_units = units; }
		virtual TimeUnits::units    get_time_units() { 
						return time_units; }
		virtual void redraw(double t1=TFLAG, double t2=TFLAG);

		TelemTimeBar() {
			stime = new ScEventTime;
			etime = new ScEventTime;
			sstime = new ScEventTime;
		}
};


// ***************************** SplineEdit ******************************
#include "path/spline.h"

// This class is for editing paths of floating point values of type spline.

typedef void (*SendSplineEditEvents)(Event::events, int key_frame, 
		double time, vis_spline *sp, XtPointer userdata);

// use this to do additional drawing in the editor
typedef void (*SetDrawFunc)(Display *d, Pixmap p, GC gc, int height,
	int startx, int endx, double start_time, double time_per_pixel, 
	XtPointer data);

class SplineEdit : public ParEdit {

    protected:

	void	init();
	void	setup_display();
	void	button_pressed(int b, int x, int y, double t, double w);
	void	mouse_moved(int b, int x, int y, double t, double w);
	void	button_released(int b, int x, int y, double t, double w);

	// Set the label to the name of the path
	void	set_label() { if(sp) namelab.labelString=sp->get_pathname(); }

	// maximum number of auxilliary paths tha can be edited
	enum    	{ MAX_AUX_PATHS = 100 };
	vis_spline	*sp;			// Primary Data to be edited
	Pixel 		path_color;
	vis_spline	*sp_aux[MAX_AUX_PATHS];	// Auxilliary Data paths
	Pixel		aux_pixels[MAX_AUX_PATHS];
	int 		num_aux_paths;
	double		minval,maxval;		// limits for visible range
	double		sel_time;		// selected time
	double		sel_val;		// selected value
	int		sel_kf,kfedit;		// selected keyframe
	int		sel_wgt;		// selected weight interval
	Boolean		grid;			// grid on/off
	Boolean		textleft,textright;	// scale text positioning
	XFontStruct	*finfo;			// font information
	int		f_off;			// font offset in pixels
	
	Pixel	linecol,bgcol,kfcol,boxcol,	// Color indicies
		gridcol,grid10col,hicol,textcol,tcol;

	// Widgets (objects) in the imgtiepath editor interface:
	PopupMenu	*pop;
	PulldownMenu	*sub;
	WObject		*pop_items[8],*sub_items[2];

	Boolean move_horizontal;	// allow for time  editing
	Boolean editable;		// set editable state of path
	Boolean draw_kf_lines;
	Boolean draw_primary_boxes;
	Boolean draw_aux_boxes[MAX_AUX_PATHS];

    public:

	// these are used to send back spline events to a class
	// the data member should be a pointer to the class object
	static SendSplineEditEvents send_spline_edit_events;
	static XtPointer send_spline_edit_events_data;

	// use this to supress the calling of the set_draw_func which is
	// called from redraw. Use when settin limits within the set_draw_fun,
	// otherwise an infinite loop can occur since set_limits calls redraw
	int call_set_draw_func_on_redraw;
	SetDrawFunc set_draw_func;
	XtPointer set_draw_func_data;	

	// Callback functions used internally to the path editor:
	static	void	add_keyframe(WObject *, XtPointer);
	static	void	del_keyframe(WObject *, XtPointer);
	static	void	grid_changed(WObject *, XtPointer);
	static	void	left_changed(WObject *, XtPointer);
	static	void	right_changed(WObject *, XtPointer);
	static	void	primary_control_boxes(WObject *, XtPointer);
	static	void	aux_control_boxes(WObject *, XtPointer);
	static	void	keyframe_lines(WObject *, XtPointer);

	void get_drawing_params(Display **d, GC *gc, Pixmap *pmap, 
			int *x1, int *x2, double *t1,
			double *dt, double *dv, int *vy);

	void set_selected(int select) { sel_kf = select; redraw(); expose(); }

	// Set the spline path to i (I)
	void	set_path(vis_spline *i, char *color=0) { 
			sp=i; sel_kf=-1; set_label();
			if (color) {
				int stat;
				Pixel px = named_color_alloc(form, color, stat);
				if (stat < 0) px = linecol;
				path_color = px;
			} else path_color = linecol;
			resized(&draw,NULL); exposed(&draw,NULL); }

	// set up to 100 additional splines. only after primary spline is set
	void add_aux_path(vis_spline *vs, String color=0);

	// replace aux path with new path; default is to remove path
	void replace_aux_path(int which, vis_spline *vs=0);

	// Get the limits of the visible range (vertical) in x1,x2 (O)
	void	get_limits(double *x1, double *x2) { *x1=minval; *x2=maxval; }

	// Set the callback for the done button to function cb (I)
	void	set_done_callback(CallbackFunc cb) {
		if(pop_items[7]) {
			((PushButton *)pop_items[7])->data=(XtPointer)this;
			((PushButton *)pop_items[7])->activateCallback=cb;
		}
	}

	Form	*create(String name, WObject &p);
	void	set_limits(double x1, double x2, int do_redraw=1);
	void	redraw(double t1=TFLAG, double t2=TFLAG);

	void set_move_horizontal(Boolean tf=True) { move_horizontal = tf; }
	void set_editable(Boolean tf=True);
	void set_draw_grid(Boolean tf=True);
	void set_draw_scales(Boolean tleft=True, Boolean tright=True);
	void set_draw_keyframe_lines(Boolean tf=True) { draw_kf_lines = tf; }
	void set_draw_primary_boxes(Boolean tf=True) {draw_primary_boxes = tf; }
	void set_draw_aux_boxes(int which, Boolean tf=True) { 
		if (which < num_aux_paths) draw_aux_boxes[which] = tf; 
		else cout << "Not that many auxilliary paths" << endl;
	}

	// Constructor: initialize
	SplineEdit() { init(); }

	// Destructor: empty function
	~SplineEdit() { ; }
	};

#endif // _PAREDIT_H_
