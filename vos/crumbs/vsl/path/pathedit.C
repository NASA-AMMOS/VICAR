// pathedit.C		Path Editor Window Classes - Member Functions
//
// Written by Dave Kagels for the ASV task - 4/23/94
//
// Copyright (C) 1994, California Institute of Technology
// All rights reserved.

#include "path/pathedit.h"

static String telem_help =
(char *)"1.  Telemetry Editor \n\n\
The Telemetry Editor displays interpolated plots of channel data, over a time \
range from years to milliseconds. The \
Editor consists of a Delta Time Slider which specifies the current visible \
time range, a Visible Time Scroll Bar for changing the visible time within \
the range, two Text Time areas which display the beginning and ending visible \
times, Zoom In, Zoom Out, and Reset buttons, a Time Axis, and any number of \
Plot Areas.\n\n\
1.1  Delta Time Slider\n\n\
The Delta Time Slider is used to change the amount of time shown on the time \
axis. Moving the slider decreases / increases the delta time displayed. The \
time axis and current units are updated to reflect the new delta.  The range \
is indicated in the adjacent text area, and is in the current units \
(i.e. days, hours).\n\n\
1.2  Visible Time Scroll\n\n\
The Visible Time Scroll is used to changed the beginning and ending times \
displayed within the delta time range. By dragging the slider, it is always \
possible to view the entire data set (unless the data has been Zoomed). For \
large time deltas, this slider may move in quite large time increments. For \
this reason, the slider arrows have been provided. These arrows will move the \
time in small increments, based on the current units.\n\n\
1.3  Text Time Areas\n\n\
The beginning and ending Text times are displayed in the two Text time areas.\
The format of these times is year:day of year:hour:minute:second:millisec. \
The time shown is based on the current units, and is always in fractions of \
those units. For example, if the current units are Hours, the the displayed \
times would indicate the years, days, hours, and minutes.\n\n\
The units of the Text time areas are dependent on the internals of the tool \
being used and other factors.  These time areas may display UTC, SCET, Local \
Solar Time, and other units, although the format will be the same.  The label \
on the time bar will indicate the displayed units.\n\n\
1.4  Zoom In Button\n\n\
The Zoom In Button is used zoom in on a portion of the data. Zooming in \
changes the start and end times of the viewable data (e.g. data outside these\
 times cannot be viewed). To Zoom in on data, press the left mouse button in \
the time axis area. A red line will appear. Drag this line to the desired \
start or end time and release the button. Repeat this for the other time \
limit, and then press the Zoom In button. The limit lines can be moved by \
pressing the left mouse button on or close to a line and moving it to the new \
position. Once the time has been zoomed, all the controls will now function \
only within the new range. Zooming can be performed any number of times, but \
the range cannot be finer than the minimum resolution.\n\n\
1.5  Zoom Out\n\n\
The Zoom Out button undoes the last executed Zoom In and returns to the \
previous state.\n\n\
1.6  Reset\n\n\
The Reset button resets the time range back to the original range \
(i.e. before any zooming was performed).\n\n\
1.7  Time Axis\n\n\
The Time Axis is the time scale over which the plots are shown. The labels \
are displayed in the current units.\n\n\
1.8  Current Units\n\n\
The Current Units are displayed in the left margin of the Editor, next to \
the Time Axis. It always shows the current units of the Axis.\n\n\
1.9  Plot Areas\n\n\
The Plot Areas are Motif paned windows where the plots are displayed. \
Each Plot is contained within its own pane, and can be resized by moving the \
pane control sash (located at the bottom of the window) with the left mouse \
button.  Within each window is a left and right scale indicating the values \
displayed within the window. The scale range can be increased / decreased by \
holding the left mouse button in the window and moving it right / left.  Also, \
holding the left mouse button and dragging it up / down, shifts the scale \
range down / up.\n\
Pressing the right mouse button in the window, pops up a menu, from which \
the scales and grid lines can be turned on or off, and the window can \
be removed.\n\
Pressing and holding the middle button, displays a line indicating the scale \
value at the cursor.";

// Initialize variables to default settings
void pathedit::init()
{
par_list=NULL;
nlist=NULL;
s_time=0.0;
delta=10101.0;	// flag value - larger than any useful value
start=0.0;
end=0.0;
delta_text_update=True;
delta_label = (char *)"Time Range";
scroll_update=True;
user_done = 0;
user_done_data = 0;
drawing_area_size = 0;
draw_func = 0;
draw_func_data = 0;
timebar_action = 0;
}


// Create the interface objects with name n (I) and parent object p (I)
Form* pathedit::create(String n, WObject &p)
{
	return(create(n,(Widget&)p));
}

Form* pathedit::create(String n, Widget &p)
{
	Form *temp = create_unmanaged(n, p);
	popup();
	return(temp);
}

Form* pathedit::create_unmanaged(String n, WObject &p)
{
	return(create_unmanaged(n,(Widget&)p));
}

Form* pathedit::create_unmanaged(String n, Widget &p)
{

Form	*f;

// create the shell
shell.width=705;
shell.title=(char *)"Path Editor";
shell.iconName=(char *)"Path Editor";
shell.deleteResponse=XmDO_NOTHING;
shell.minWidth=300;
shell.minHeight=165;
shell.create_unmanaged(n,p);

form.create((char *)"form",shell);			// form for resizability

String labs[] = { (char *)"Done", (char *)"Help" };
pbuttonForm = prob.create((char *)"buttons", form, 2, labs);
prob.set_parent(this);
pbuttonForm->bottom.attach();
pbuttonForm->bottom.offset = 5;
pbuttonForm->left.attach();
pbuttonForm->right.attach();
for (int i = 0; i < 2; i++) prob.user_activate_data[i] = this;
prob.add_callback(0, DoneCB);
prob.add_callback(1, HelpCB);

sep.orientation = XmHORIZONTAL;
sep.create((char *)"sep2", form);
sep.bottom = pbuttonForm;
sep.bottom.offset = 5;
sep.left.attach();
sep.right.attach();

// visible time range label
delta_lab.labelString=delta_label;
delta_lab.left.attach();
delta_lab.bottom = sep;
delta_lab.bottom.offset = 10;
delta_lab.width=130;
delta_lab.height=23;
delta_lab.marginLeft=2;
delta_lab.create((char *)"delta_lab",form);

vis_lab.labelString=(char *)"Visible";
vis_lab.left.attach();
vis_lab.bottom=delta_lab;
vis_lab.width=130;
vis_lab.height=23;
vis_lab.marginTop=8;
vis_lab.marginHeight=0;
vis_lab.create((char *)"vis_lab",form);

// visible time range text field
delta_text.value=(char *)"";
delta_text.left=delta_lab;
delta_text.bottom = sep;
delta_text.bottom.offset = 10;
delta_text.width=70;
// delta_text.height=23;
delta_text.highlightThickness=0;
delta_text.marginHeight=2;
delta_text.marginWidth=3;
delta_text.create((char *)"delta_text",form);
delta_text.data=(XtPointer)this;
delta_text.activateCallback=delta_text_changed;
delta_text.losingFocusCallback=delta_text_changed;

// visible time range scale
delta_scale.maximum=-10;
if((int)((start-end)*100.0) >= -10) delta_scale.minimum=-11;
else    delta_scale.minimum=(int)((start-end)*100.0);
delta_scale.value=(int)delta_scale.minimum;
delta_scale.orientation=XmHORIZONTAL;
delta_scale.left=delta_text;
delta_scale.right.attach();
delta_scale.bottom = sep;
delta_scale.bottom.offset = 10;
delta_scale.height=23;
delta_scale.scaleHeight=23;
delta_scale.highlightThickness=0;
delta_scale.create((char *)"delta_scale",form);
delta_scale.data=(XtPointer)this;
delta_scale.dragCallback=delta_drag;
delta_scale.valueChangedCallback=delta_changed;

// time scrollbar
if(end-start > 0.01) {
	time_scroll.minimum=(int)(start*100.0);
	time_scroll.maximum=(int)(end*100.0);
	set_scroll_size();
	set_scroll_value();
	}
else {	time_scroll.minimum=0;
	time_scroll.maximum=10;
	time_scroll.sliderSize=10;
	time_scroll.value=0;
	}
time_scroll.orientation=XmHORIZONTAL;
time_scroll.left=vis_lab;
time_scroll.right.attach();
time_scroll.bottom=delta_scale;
time_scroll.height=23;
time_scroll.create((char *)"time_scroll",form);
time_scroll.data=(XtPointer)this;
time_scroll.dragCallback=(CallbackFunc)time_drag;
time_scroll.valueChangedCallback=(CallbackFunc)time_changed;

// time bar "ruler"
tb=new TimeBar;
f=tb->create((char *)"time_bar",form);
tb->set_time(s_time);
tb->set_delta(delta);
tb->data=(XtPointer)this;
new par_node(tb,par_list,(char *)"frame");	// insert into linked list
f->left.attach();
f->right.attach();
f->bottom=vis_lab;
f->height=75;

time_sep.orientation=XmHORIZONTAL;			// separator
time_sep.height=8;
time_sep.left.attach();
time_sep.right.attach();
time_sep.bottom=*f;
time_sep.create((char *)"time_sep",form);

// paned window
panes.marginWidth=0;
panes.marginHeight=0;
panes.shadowThickness=0;
panes.sashShadowThickness=2;
panes.sashWidth=50;
panes.left.attach();
panes.right.attach();
panes.top.attach();
panes.bottom=time_sep;
panes.create((char *)"paned_window",form);

helpTool.create_unmanaged((char *)"helpTool", form);
helpTool.set_text(telem_help);
helpTool.make_contents();

return &form;

}

// Set the time scrollbar position
void pathedit::set_scroll_value()
{
if((int)(s_time*100.0)+(int)time_scroll.sliderSize > (int)time_scroll.maximum)
	time_scroll.value=(int)time_scroll.maximum-(int)time_scroll.sliderSize;
else	time_scroll.value=(int)(s_time*100.0);
}


// Set the time scrollbar size
void pathedit::set_scroll_size()
{
if((end-start)-delta <= 0.01) time_scroll.sliderSize=
		(int)time_scroll.maximum-(int)time_scroll.minimum;
else time_scroll.sliderSize=(int)(delta*100.0);
}


void pathedit::get_start_end(double &lstart, double &lend)
{
	lstart = s_time;
	lend = s_time + delta;
	return;
}

// Set the start and end times for the path editor to s,e (I)
void pathedit::set_start_end(double s, double e)
{
int	i;

start=s; end=e;

// recompute s_time & delta
if(delta>e-s) delta=e-s;
if(s_time<s) s_time=s;
else if(s_time+delta>e) s_time=e-delta;

if(!par_list) return;

par_list->set_time(s_time);
par_list->set_delta(delta);

// reset delta scale
i=(int)((start-end)*100.0);
if(i>=-10) i=-11;
if(delta_scale.value < i) delta_scale.value=i;
delta_scale.minimum=i;
if(i<=-20) delta_scale.scaleMultiple=10;
set_delta_scale();
set_delta_text(delta);

// reset time scrollbar
if(end-start > 0.01) {
	int max = time_scroll.maximum;
	int min = time_scroll.minimum;
	int value = time_scroll.value;
	int new_start = (int)(start*100);
	int new_end = (int)(end*100);

	// if (value < new_start) time_scroll.value = new_start;
	// else if (value > new_end) time_scroll.value = new_end;
	if (new_start >= max) {
		time_scroll.maximum=new_end;
		time_scroll.minimum=new_start;
	} else time_scroll.minimum = new_start;

	if (new_end <= min) {
		time_scroll.minimum=new_start;
		time_scroll.maximum=new_end;
	} else time_scroll.maximum = new_end;

	set_scroll_size();
	set_scroll_value();
	}
else {	time_scroll.minimum=0;
	time_scroll.maximum=10;
	time_scroll.sliderSize=10;
	time_scroll.value=0;
	}
}

void pathedit::redraw()
{
	par_node *pn = par_list;
	while(pn) {
		pn->get_par()->redraw();
		pn = pn->get_next();
	}
	tb->redraw();
	tb->expose();
}

// Add a new wvspline path wv (I) with name n (I) to the path editor window
// only after create
SplineEdit *pathedit::add_path(vis_spline *wv, String n, char *color)  
{
SplineEdit	*se;
Form	*f;

if(delta==10101.0) set_start_end(s_time,s_time+0.2);	// flag value

// create the wvspline editor interface object
se=new SplineEdit();
f=se->create((char *)"spline_edit",panes);
f->paneMinimum=50;
f->paneMaximum=1000;
// Adjust window:
int height = 300 + drawing_area_size;
if(par_list) {
	if(!par_list->get_next()) {
		f->height=300;
		shell.height=height;
	} else	{
		f->height=80;
		shell.height+=110;
	}
}

// initialize the wvspline editor interface object
se->set_path(wv, color);
se->set_done_callback(&pathedit::par_done);
se->data=(XtPointer)this;
se->set_time(s_time);
se->set_delta(delta);
if (draw_func) se->set_draw_func = draw_func;
if (draw_func_data) se->set_draw_func_data = draw_func_data;

new par_node(se,par_list,n);			// insert into linked list
return se;
}


// Replace wvspline path with name n (I) with new wvspline path wv (I)
// only after create
SplineEdit *pathedit::replace_path(vis_spline *wv, String n, char *color)
{
	par_node *pn;
	SplineEdit *se = 0;

	for(pn=par_list;pn;pn=pn->get_next()) 
		if(!strcmp(n,pn->get_name())) break;
	if (pn) {
		se = (SplineEdit *)(pn->get_par());
		se->set_path(wv, color);
	}

	return se;
}

void pathedit::remove_all_paths()
{
	par_node *pn = par_list;
	while (pn) {
		if (strcmp(pn->get_name(), "frame")) remove_path(pn->get_par());
		else break;
		pn = par_list;
	}
}

// remove a given path from the interface
void pathedit::remove_path(ParEdit *pe)
{
// remove it from the list
par_node *r = par_node::remove_par(par_list , pe);		
if(r)	delete r;		// delete it

if(par_list) if(!par_list->get_next()) shell.height= 229+drawing_area_size;
else	shell.height-=108;
}


// Set the text in the Visible Time Range text field to show value d (I)
void pathedit::set_delta_text(double d)
{
char buf[10];
sprintf(buf,"%.2lf",d);
delta_text.value=buf;
}


// ************************ Callback Functions:


// Called when Done is selected from a path editor popup menu
void pathedit::par_done(WObject *obj, XtPointer)
{
ParEdit *pe=(ParEdit *)(obj->data);
pathedit *p=(pathedit *)(pe->data);

p->remove_path(pe);
}

// Called when Clear button is pushed
void pathedit::ClearCB(WObject*, XtPointer, XtPointer udata)
{
	pathedit *p=(pathedit *)udata;

	p->remove_all_paths();
}

void pathedit::HelpCB(WObject*, XtPointer, XtPointer udata)
{
   	pathedit *p=(pathedit *)udata;
   	p->helpTool.manage();
}

// Called when the Done button is pressed
void pathedit::DoneCB(WObject *obj, XtPointer data, XtPointer udata)
{
   pathedit *p=(pathedit *)udata;
   if (!p->user_done) {
      par_node *n;

      if(p->par_list) {
	while(p->par_list->get_par() != p->tb) {
		n=p->par_list;
		p->par_list->remove(p->par_list);
		delete n;
		if(!p->par_list) { p->popdown(); return; }
	}
	n=p->par_list->get_next();
	while(n) {
		n->remove(p->par_list);
		delete n;
		n=p->par_list->get_next();
	}
      }
      p->popdown();
   } else p->user_done(obj, data, p->user_done_data);
}

// Called when the time scrollbar is dragged
void pathedit::time_drag(ScrollBar *sb, XtPointer)
{
pathedit *p=(pathedit *)sb->data;

p->par_list->expose(sb->value*0.01);		// expose at new time
}


// Called when the time scrollbar is released
void pathedit::time_changed(ScrollBar *sb, XtPointer)
{
pathedit *p=(pathedit *)sb->data;

if(!p->scroll_update) { p->scroll_update=True; return; }

p->s_time=sb->value*0.01;			// compute new time
p->par_list->set_time(p->s_time);		// set to new time
}


// Called when the Visible Time Range scale is dragged
void pathedit::delta_drag(WObject *obj, XtPointer)
{
pathedit *p=(pathedit *)(obj->data);
double	d,s;

// compute new values
p->delta_text_update=False;
d=(double)(p->delta_scale.value*(-0.01));
p->set_delta_text(d);
if(p->s_time+d>p->end) s=p->end-d;
else	s=p->s_time;


// update the time bar
p->tb->set_time(s);
p->tb->set_delta(d);
}


// Called when the Visible Time Range scale is released
void pathedit::delta_changed(WObject *obj, XtPointer)
{
pathedit *p=(pathedit *)(obj->data);
double	d;

// compute new values
d=(double)(p->delta_scale.value*(-0.01));
p->delta_text_update=True;
if(d==p->delta) return;
p->delta=d;
p->delta_text_update=False;
p->set_delta_text(p->delta);
p->delta_text_update=True;
if(p->s_time+p->delta > p->end) {
	p->scroll_update=False;
	p->time_scroll.value=p->time_scroll.minimum;
	p->set_scroll_size();
	p->s_time=p->end-p->delta;
	p->scroll_update=False;
	p->set_scroll_value();
	p->scroll_update=True;
	if(p->par_list) p->par_list->set_time(p->s_time);
	}
else	p->set_scroll_size();


// update all the path editor objects
if(p->par_list) p->par_list->set_delta(p->delta);
}


// Called when the text in the Visible Time Range text field is changed
void pathedit::delta_text_changed(WObject *obj, XtPointer)
{
pathedit *p=(pathedit *)(obj->data);
String	buf;
double	d;

if(!p->delta_text_update) { p->delta_text_update=True; return; }

// compute new values
buf=p->delta_text.value;
d=atof(buf);
XtFree(buf);
if(d==p->delta) return;
// if(d<0.1) d=0.1;
else if(d>p->end-p->start) d=p->end-p->start;
p->delta=d;
p->delta_text_update=False;
p->set_delta_text(p->delta);
p->delta_text_update=False;
p->set_delta_scale();
p->delta_text_update=True;
if(p->s_time+p->delta > p->end) {
	p->scroll_update=False;
	p->time_scroll.value=p->time_scroll.minimum;
	p->set_scroll_size();
	p->s_time=p->end-p->delta;
	p->scroll_update=False;
	p->set_scroll_value();
	p->scroll_update=True;
	if(p->par_list) p->par_list->set_time(p->s_time);
	}
else	p->set_scroll_size();

// update all the path editor objects
if(p->par_list) p->par_list->set_delta(p->delta);
}

void pathedit::TimeBarEvent(double time1, double time2, XtPointer data)
{
	pathedit *pe = (pathedit*)data;

	if (pe->timebar_action) (*pe->timebar_action)(time1, time2,  data);
}

void pathedit::set_timebar_event_action(TimeBarAction tba)
{
	timebar_action = tba;
	tb->set_button_press_action(TimeBarEvent, this);
}



/***********************Telemetry Editor************************/

static int scale_size = -1;   // current delta scale size, used to reset factor

// member functions

// Create the interface objects with name n (I) and parent object p (I)
Form* telemedit::create(String n, WObject &p)
{
	return(create(n,(Widget&)p));
}

Form* telemedit::create(String n, Widget &p)
{
	Form *temp = create_unmanaged(n, p);
	popup();
	return(temp);
}

Form* telemedit::create_unmanaged(String n, WObject &p)
{
	return(create_unmanaged(n,(Widget&)p));
}

Form* telemedit::create_unmanaged(String n, Widget &p)
{

//Display *disp = XtDisplay(p.get_widget());
Display *disp = XtDisplay(p);
XFontStruct *fontStruct = XLoadQueryFont(disp, (char *)"6x12");
XmFontList tfont;
if (!fontStruct) cout << "Unable to find 6x10 font" << endl;
else {
   XmFontListEntry fontEntry = XmFontListEntryCreate(XmFONTLIST_DEFAULT_TAG,
					XmFONT_IS_FONT, fontStruct);
   tfont = XmFontListAppendEntry(0, fontEntry);
   XmFontListEntryFree(&fontEntry);
}

Form	*f;

// create the shell
shell.width=705;
shell.title=(char *)"Telemetry Editor";
shell.iconName=(char *)"Telem Editor";
shell.deleteResponse=XmDO_NOTHING;
shell.minWidth=300;
shell.minHeight=165;
shell.create_unmanaged(n,p);

form.create((char *)"form",shell);				// form for resizability

String labs[] = { (char *)"Clear", (char *)"Done", (char *)"Help" };
pbuttonForm = prob.create((char *)"buttons", form, 3, labs);
prob.set_parent(this);
pbuttonForm->bottom.attach();
pbuttonForm->left.attach();
pbuttonForm->right.attach();
for (int i = 0; i < 3; i++) prob.user_activate_data[i] = this;
prob.add_callback(0, ClearCB);
prob.add_callback(1, DoneCB);
prob.add_callback(2, HelpCB);

sep.orientation = XmHORIZONTAL;
sep.create((char *)"sep2", form);
sep.bottom = pbuttonForm;
sep.bottom.offset = 5;
sep.left.attach();
sep.right.attach();

// visible time range label
delta_lab.labelString=delta_label;
delta_lab.left.attach();
delta_lab.bottom = sep;
delta_lab.bottom.offset = 10;
delta_lab.width=130;
delta_lab.height=23;
delta_lab.marginLeft=2;
delta_lab.create((char *)"delta_lab",form);

vis_lab.labelString=(char *)"Visible";
vis_lab.left.attach();
vis_lab.bottom=delta_lab;
vis_lab.width=130;
vis_lab.height=23;
vis_lab.marginTop=8;
vis_lab.marginHeight=0;
vis_lab.create((char *)"vis_lab",form);

// visible time range text field
if (fontStruct) delta_text.fontList = tfont;
delta_text.value=(char *)"";
delta_text.left=delta_lab;
delta_text.bottom = sep;
delta_text.bottom.offset = 10;
delta_text.width=70;
delta_text.height=23;
delta_text.highlightThickness=0;
// delta_text.marginHeight=8;
delta_text.marginWidth=3;
delta_text.create((char *)"delta_text",form);
delta_text.data=(XtPointer)this;
delta_text.activateCallback=telem_delta_text_changed;
delta_text.losingFocusCallback=telem_delta_text_changed;

// visible time range scale
delta_scale.orientation=XmHORIZONTAL;
delta_scale.left=delta_text;
delta_scale.right.attach();
delta_scale.bottom = sep;
delta_scale.bottom.offset = 10;
delta_scale.height=23;
delta_scale.scaleHeight=23;
delta_scale.value = 0;
delta_scale.minimum = 0;
delta_scale.highlightThickness=0;
delta_scale.create((char *)"delta_scale",form);
delta_scale.data=(XtPointer)this;
delta_scale.dragCallback=telem_delta_drag;
delta_scale.valueChangedCallback=telem_delta_changed;

// time scrollbar
time_scroll.orientation=XmHORIZONTAL;
time_scroll.left=vis_lab;
time_scroll.right.attach();
time_scroll.bottom=delta_scale;
time_scroll.height=23;
time_scroll.create((char *)"time_scroll",form);
time_scroll.data=(XtPointer)this;
time_scroll.dragCallback=(CallbackFunc)telem_time_drag;
time_scroll.valueChangedCallback=(CallbackFunc)telem_time_changed;
time_scroll.incrementCallback=(CallbackFunc)telem_time_increment;
time_scroll.decrementCallback=(CallbackFunc)telem_time_decrement;

time_scroll.sliderSize=1;
time_scroll.minimum=0;
time_scroll.maximum=1;
time_scroll.value=0;

if (fontStruct) time_start.fontList = tfont;
time_start.left = vis_lab;
time_start.bottom = time_scroll;
time_start.columns = 21;
time_start.editable = False;
time_start.marginHeight = 2;
time_start.marginWidth = 1;
time_start.maxLength = 21;

time_start.create((char *)"delta_text",form);
time_start.data=(XtPointer)this;
time_start.activateCallback= timeStartActivateCB;
time_start.losingFocusCallback= timeStartLosingFocusCB;

String blabs[] = { (char *)"Reset", (char *)"Zoom In", (char *)"Zoom out" };
buttonForm = rob.create((char *)"buttons", form, 3, blabs);
rob.set_parent(this);
rob.add_callback(0, UserResetCB);
rob.add_callback(1, UserZoomInCB);
rob.add_callback(2, UserZoomOutCB);
for (int k = 0; k < 3; k++) {
        rob.user_activate_data[k] = this;
        rob.set_sensitive(k, False);
	if (fontStruct) rob.get_button(k)->fontList = tfont;
}
rob.set_sensitive(0, True);

buttonForm->left = time_start;
buttonForm->right.attach();
buttonForm->right.offset = 135;
buttonForm->bottom = time_scroll;

time_end.right.attach();
time_end.bottom = time_scroll;
time_end.columns = 21;
time_end.editable = False;
time_end.marginHeight = 2;
time_end.marginWidth = 1;
time_end.maxLength = 21;

if (fontStruct) time_end.fontList = tfont;
time_end.create((char *)"delta_text",form);
time_end.data=(XtPointer)this;
time_end.activateCallback= timeStartActivateCB;
time_end.losingFocusCallback= timeStartLosingFocusCB;


// time bar "ruler"
tb = select_timebar();  // new TelemTimeBar;
f=tb->create((char *)"time_bar",form);
tb->set_time_units(get_current_units());
tb->set_time(s_time);
tb->set_delta(delta);
tb->data=(XtPointer)this;
tb->set_time_scale_factor(time_scale_factor);
set_timebar_event_action(telem_timebar_action);
new par_node(tb,par_list,(char *)"frame");	// insert into linked list
f->left.attach();
f->right.attach();
f->bottom=buttonForm;
f->height=75;

TimeUnits::units start_units;
switch(get_current_units()) {
   case TimeUnits::YEAR:
	set_timebar_label(start_time->get_units_label(TimeUnits::YEAR));
	start_units = TimeUnits::DOY;
	break;
   case TimeUnits::DOY:
	set_timebar_label(start_time->get_units_label(TimeUnits::DOY));
	start_units = TimeUnits::HR;
	break;
   case TimeUnits::HR:
	set_timebar_label(start_time->get_units_label(TimeUnits::HR));
	start_units = TimeUnits::MIN;
	break;
   case TimeUnits::MIN:
	set_timebar_label(start_time->get_units_label(TimeUnits::MIN));
	start_units = TimeUnits::SEC;
	break;
   case TimeUnits::SEC:
	set_timebar_label(start_time->get_units_label(TimeUnits::SEC));
	start_units = TimeUnits::MSEC;
	break;
   case TimeUnits::MSEC:
	set_timebar_label(start_time->get_units_label(TimeUnits::MSEC));
	break;
}
char *s = start_time->get_scet_time(start_units);
if (s) time_start.value = s;
else time_start.value = (char *)"";

char *e = end_time->get_scet_time(start_units);
if (e) time_end.value = e;
else time_end.value = (char *)"";

time_sep.orientation=XmHORIZONTAL;			// separator
time_sep.height=8;
time_sep.left.attach();
time_sep.right.attach();
time_sep.bottom=*f;
time_sep.create((char *)"time_sep",form);

// paned window
panes.marginWidth=0;
panes.marginHeight=0;
panes.shadowThickness=0;
panes.sashShadowThickness=2;
panes.sashWidth=50;
panes.left.attach();
panes.right.attach();
panes.top.attach();
panes.bottom=time_sep;
panes.create((char *)"paned_window",form);

helpTool.create_unmanaged((char *)"helpTool", form);
helpTool.set_text(telem_help);
helpTool.make_contents();

return &form;
}

void pathedit::VisibleAction(Widget w, XtPointer, XEvent*, Boolean*)
{
        XRaiseWindow(XtDisplay(w), XtWindow(w));
}

void telemedit::set_start_end(String s, String e, double mres)
{
   	if (!s || !e) {
      		cout << "Start and End must not be NULL" << endl;
      		return;
   	}	
	
   	int slen = strlen(s);
   	int elen = strlen(e);
	
   	if (slen != elen) {
      		cout << "Start and End strings must be the same length" << endl;
fprintf(stdout,"Start time string=>%s<\n", s);
fprintf(stdout,"End   time string=>%s<\n", e);
      		return;
   	} 

	if (slen < 21) {
		cout << "Must supply full Event Time" << endl;
		return;
	}

        start_time->set_scet_time(s);
        end_time->set_scet_time(e);
	start = start_time->get_time_in_sec() * time_scale_factor;
	end = end_time->get_time_in_sec() * time_scale_factor;

	minres = mres / 86400.0;		// in seconds
	minres = mres * time_scale_factor;

	update_display();
}

void telemedit::set_start_end(double local_start, double local_end, double mres)
{
	start = local_start;
	end = local_end;
	minres = mres * time_scale_factor;
	start_time->set_time_in_sec(start / time_scale_factor);
	end_time->set_time_in_sec(end / time_scale_factor);
	update_display();
}

void telemedit::update_display()
{
	static int first = 1;

	int dsize = delta_scale.width;
	if (dsize > 0) {
		double nl = log((end - start)/minres);
		factor = exp(nl/dsize);
	}

// recompute s_time & delta
	delta = end - start;
	if(s_time < start) s_time = start;
	else if(s_time+delta > end) s_time = end-delta;

	TimeUnits::units ou;

	switch(get_current_units()) {
	   case TimeUnits::YEAR:
		if (!first) set_timebar_label(start_time->get_units_label(TimeUnits::YEAR));
		ou = TimeUnits::DOY;
		//rob.set_sensitive(0, False);
		break;
	   case TimeUnits::DOY:
		if (!first) set_timebar_label(start_time->get_units_label(TimeUnits::DOY));
		ou = TimeUnits::HR;
		break;
	   case TimeUnits::HR:
		if (!first) set_timebar_label(start_time->get_units_label(TimeUnits::HR));
		ou = TimeUnits::MIN;
		break;
	   case TimeUnits::MIN:
		if (!first) set_timebar_label(start_time->get_units_label(TimeUnits::MIN));
		ou = TimeUnits::SEC;
		break;
	   case TimeUnits::SEC:
		if (!first) set_timebar_label(start_time->get_units_label(TimeUnits::SEC));
		ou = TimeUnits::MSEC;
		break;
	   case TimeUnits::MSEC:
		if (!first) set_timebar_label(start_time->get_units_label(TimeUnits::MSEC));
		ou = TimeUnits::MSEC;
	}

	if (first) {
		strcpy(scet_start, start_time->get_scet_time());
		strcpy(scet_end, end_time->get_scet_time());
		first = 0;
	} else {
    		time_start.value = start_time->get_scet_time(ou);
    		time_end.value = end_time->get_scet_time(ou);
		tb->set_time_units(get_current_units());
	}

	if(!par_list) return;

	par_list->set_time(s_time);
	par_list->set_delta(delta);

// reset delta scale
	delta_scale.value=0;
	set_delta_text(delta);

// reset time scrollbar
	time_scroll.value=0;
	time_scroll.sliderSize = time_scroll.maximum - time_scroll.minimum;
}

void telemedit::telem_time_drag(ScrollBar *sb, XtPointer)
{
	telemedit *te = (telemedit*)(sb->data);

	int tsize = sb->width;
	if (scale_size != te->delta_scale.width) {
		int dsize = te->delta_scale.width;
		double nl = log((te->end-te->start)/te->minres);
		te->factor = exp(nl/dsize);
		te->delta_scale.maximum = dsize; 
		if (scale_size < 0) sb->sliderSize=tsize;
		sb->maximum=tsize;
		scale_size = dsize;
	}

	// time units per pixel
	double  inc = te->end - te->start - te->delta;
	double  tpp = inc / (tsize-sb->sliderSize);
	double time = te->start + sb->value * tpp;

	te->par_list->expose(time);       // expose at new time

	te->rob.set_sensitive(1, False);
}

void telemedit::telem_time_changed(ScrollBar *sb, XtPointer)
{
	telemedit *te = (telemedit*)(sb->data);

	int tsize = sb->width;
	if (scale_size != te->delta_scale.width) {
		int dsize = te->delta_scale.width;
		double nl = log((te->end-te->start)/te->minres);
		te->factor = exp(nl/dsize);
		te->delta_scale.maximum = dsize; 
		sb->maximum=tsize;
		if (scale_size < 0) sb->sliderSize=tsize;
		scale_size = dsize;
	}

	te->rob.set_sensitive(1, False);
	if(!te->scroll_update) { te->scroll_update=True; return; }

	// time units per pixel
	double  inc = te->end - te->start - te->delta;
	double  tpp = inc / (tsize-sb->sliderSize);
	double time = te->start + sb->value * tpp;

	te->s_time = time;
	te->par_list->set_time(time);               // set to new time

	// update start_time
	te->update_start_end_text();
}

void telemedit::telem_time_decrement(ScrollBar *sb, XtPointer)
{
	telemedit *te = (telemedit*)(sb->data);

	int tsize = sb->width;
	if (scale_size != te->delta_scale.width) {
		int dsize = te->delta_scale.width;
		double nl = log((te->end-te->start)/te->minres);
		te->factor = exp(nl/dsize);
		te->delta_scale.maximum = dsize; 
		sb->maximum=tsize;
		if (scale_size < 0) sb->sliderSize=tsize;
		scale_size = dsize;
	}

	// time units per pixel
	double  inc = te->end - te->start - te->delta;
	double  tpp = inc / (tsize-sb->sliderSize);

	switch(te->get_current_units()) {
	   case TimeUnits::YEAR:
		if (tpp > 1) tpp = 1;	// 1 day
		break;
	   case TimeUnits::DOY:
		if (tpp > 1/24) tpp = 1.0/24;	// 1 hr
		break;
	   case TimeUnits::HR:
		if (tpp > 1/24/12.0) tpp = 1.0/24/12; 	// 5 minutes
		break;
	   case TimeUnits::MIN:
		if (tpp > 1/86400.0) tpp = 1.0/86400;
		break;
	   case TimeUnits::SEC:
		if (tpp > 1/86400.0/10) tpp = 1.0/86400/10;
		break;
	   case TimeUnits::MSEC:
		break;
	}
	double time = te->s_time - tpp;

	te->s_time = time;
	te->par_list->set_time(time);               // set to new time

	// update start_time
	te->update_start_end_text();
}


void telemedit::telem_time_increment(ScrollBar *sb, XtPointer)
{
	telemedit *te = (telemedit*)(sb->data);

	int tsize = sb->width;
	if (scale_size != te->delta_scale.width) {
		int dsize = te->delta_scale.width;
		double nl = log((te->end-te->start)/te->minres);
		te->factor = exp(nl/dsize);
		te->delta_scale.maximum = dsize; 
		sb->maximum=tsize;
		if (scale_size < 0) sb->sliderSize=tsize;
		scale_size = dsize;
	}

	// time units per pixel
	double  inc = te->end - te->start - te->delta;
	double  tpp = inc / (tsize-sb->sliderSize);

	switch(te->get_current_units()) {
	   case TimeUnits::YEAR:
		if (tpp > 1) tpp = 1;	// 1 day
		break;
	   case TimeUnits::DOY:
		if (tpp > 1/24) tpp = 1.0/24;	// 1 hr
		break;
	   case TimeUnits::HR:
		if (tpp > 1/24/12.0) tpp = 1.0/24/12; 	// 5 minutes
		break;
	   case TimeUnits::MIN:
		if (tpp > 1/86400.0) tpp = 1.0/86400;	// 1 second
		break;
	   case TimeUnits::SEC:
		if (tpp > 1/86400.0/10.0) tpp = 1.0/86400/10;	// tenth of sec
		break;
	   case TimeUnits::MSEC:
		break;
	}
	double time = te->s_time + tpp;

	te->s_time = time;
	te->par_list->set_time(time);               // set to new time

	// update start_time
	te->update_start_end_text();
}


void telemedit::telem_delta_drag(WObject *sb, XtPointer)
{
	telemedit *te = (telemedit*)(sb->data);

	if (scale_size != te->delta_scale.width) {
		int dsize = te->delta_scale.width;
		double nl = log((te->end-te->start)/te->minres);
		te->factor = exp(nl/dsize);
		te->delta_scale.maximum = dsize; 
		int tsize = te->time_scroll.width;
		te->time_scroll.maximum=tsize;
		if (scale_size < 0) te->time_scroll.sliderSize=tsize;
		scale_size = dsize;
	}

// compute new values
	te->delta_text_update=False;
	int size = te->delta_scale.width;
	int pos = te->delta_scale.value;
	double d = te->minres * pow(te->factor, (size-pos));
	te->set_delta_text(d);

	// time units per pixel
	// int ssize = te->time_scroll.width;
	// double  inc = te->end - te->start - te->delta;
	// double  tpp = inc / (ssize-te->time_scroll.sliderSize);
	// double time = te->start + te->time_scroll.value * tpp;

	if (te->s_time+d < te->end) {
		//te->end_time->set_time_in_days((te->s_time+d) / te->time_scale_factor);
		te->end_time->set_time_in_sec(te->s_time+d);
	} else {
		te->s_time = te->end - d;
		//te->start_time->set_time_in_days((te->s_time) / te->time_scale_factor);
		te->start_time->set_time_in_sec(te->s_time);
	}

	TimeUnits::units current_units = te->get_current_units();

	switch(current_units) {
	   case TimeUnits::YEAR: 
		te->set_timebar_label(te->start_time->get_units_label(TimeUnits::YEAR));
		break;
	   case TimeUnits::DOY: 
		te->set_timebar_label(te->start_time->get_units_label(TimeUnits::DOY));
		break;
	   case TimeUnits::HR: 
		te->set_timebar_label(te->start_time->get_units_label(TimeUnits::HR));
		break;
	   case TimeUnits::MIN: 
		te->set_timebar_label(te->start_time->get_units_label(TimeUnits::MIN));
		break;
	   case TimeUnits::SEC: 
		te->set_timebar_label(te->start_time->get_units_label(TimeUnits::SEC));
		break;
	   case TimeUnits::MSEC: 
		te->set_timebar_label(te->start_time->get_units_label(TimeUnits::MSEC));
		break;
	}
	te->tb->set_time_units(current_units);

	double s;
	if(te->s_time+d>te->end) s=te->end-d;
	else	s=te->s_time;

// update the time bar
	te->tb->set_time(s);
	te->tb->set_delta(d);

	te->rob.set_sensitive(1, False);
}

TimeUnits::units telemedit::get_current_units()
{
	double st = start_time->get_time_in_sec();
	double et = end_time->get_time_in_sec();
	double del = et - st;

	if (del >= 31536000) return TimeUnits::YEAR;
	else if (del >= 86400) return TimeUnits::DOY;
	else if (del >= 3600) return TimeUnits::HR;
	else if (del >= 60) return TimeUnits::MIN;
	else if (del >= 1) return TimeUnits::SEC;
	else return TimeUnits::MSEC;
}

void telemedit::telem_delta_changed(WObject *sb, XtPointer)
{
	telemedit *te = (telemedit*)(sb->data);

	if (scale_size != te->delta_scale.width) {
		int dsize = te->delta_scale.width;
		double nl = log((te->end-te->start)/te->minres);
		te->factor = exp(nl/dsize);
		te->delta_scale.maximum = dsize; 
		int tsize = te->time_scroll.width;
		te->time_scroll.maximum=tsize;
		if (scale_size < 0) te->time_scroll.sliderSize=tsize;
		scale_size = dsize;
	}


// compute new values
	int size = te->delta_scale.width;
	int pos = te->delta_scale.value;
	double d = te->minres * pow(te->factor, (size-pos));
	te->delta_text_update=True;

	if(d==te->delta) return;
	te->delta=d;
	te->delta_text_update=False;
	te->set_delta_text(d);
	te->delta_text_update=True;

	// te->end_time->set_time_in_days(te->s_time+d);
	te->update_start_end_text();

	TimeUnits::units current_units = te->get_current_units();

	switch(current_units) {
	   case TimeUnits::YEAR: 
		te->set_timebar_label(te->start_time->get_units_label(TimeUnits::YEAR));
		break;
	   case TimeUnits::DOY: 
		te->set_timebar_label(te->start_time->get_units_label(TimeUnits::DOY));
		break;
	   case TimeUnits::HR: 
		te->set_timebar_label(te->start_time->get_units_label(TimeUnits::HR));
		break;
	   case TimeUnits::MIN: 
		te->set_timebar_label(te->start_time->get_units_label(TimeUnits::MIN));
		break;
	   case TimeUnits::SEC: 
		te->set_timebar_label(te->start_time->get_units_label(TimeUnits::SEC));
		break;
	   case TimeUnits::MSEC: 
		te->set_timebar_label(te->start_time->get_units_label(TimeUnits::MSEC));
		break;
	}
	te->tb->set_time_units(current_units);

	double ratio = ((double)pos / (double)size);
	int max = te->time_scroll.maximum; 
	int tsize = (int)(max * (1.0-ratio)); 
	if (tsize == 0) tsize = 1;

	if (tsize + te->time_scroll.value > max) {
		te->scroll_update=False;
		te->time_scroll.value = max - tsize;
		te->time_scroll.sliderSize = tsize;
		if(te->par_list) te->par_list->set_time(te->s_time);
		te->scroll_update=True;
	} else te->time_scroll.sliderSize = tsize;

// update all the path editor objects
	if(te->par_list) te->par_list->set_delta(te->delta);


	te->rob.set_sensitive(1, False);
}

void telemedit::telem_delta_text_changed(WObject *sb, XtPointer)
{
	telemedit *te = (telemedit*)(sb->data);

	if (scale_size != te->delta_scale.width) {
		int dsize = te->delta_scale.width;
		double nl = log((te->end-te->start)/te->minres);
		te->factor = exp(nl/dsize);
		te->delta_scale.maximum = dsize; 
		int tsize = te->time_scroll.width;
		te->time_scroll.maximum=tsize;
		if (scale_size < 0) te->time_scroll.sliderSize=tsize;
		scale_size = dsize;
	}

        String  buf;
        double  d;

        if(!te->delta_text_update) { te->delta_text_update=True; return; }

// compute new values
        buf=te->delta_text.value;
        d=atof(buf);
        XtFree(buf);
	switch (te->get_current_units()) {
	  case TimeUnits::YEAR:
	    d *= 365.0;
	    break;
	  case TimeUnits::DOY:
	    break;
	  case TimeUnits::HR:
	    d /= 24.0;
	    break;
	  case TimeUnits::MIN:
	    d /= 1440.0;
	    break;
	  case TimeUnits::SEC:
	    d /= 86400.0;
	    break;
	  case TimeUnits::MSEC:
	    d /= (86400.0*1000.0);
	    break;
	}
        if(d==te->delta) return;
        if(d > te->end - te->start) d = te->end - te->start;
	else if (d < te->minres) d = te->minres;
        te->delta=d;
        te->delta_text_update=False;
        te->set_delta_text(d);
        te->delta_text_update=False;
	double dpos = scale_size - log(d/te->minres) / log(te->factor);
	te->delta_scale.value = (int)dpos;
        te->delta_text_update=True;

	int size = te->delta_scale.width;
	double ratio = ((double)dpos / (double)size);
	int max = te->time_scroll.maximum; 
	int tsize = (int)(max * (1.0-ratio)); 
	if (tsize == 0) tsize = 1;

	if (tsize + te->time_scroll.value > max) {
		te->scroll_update=False;
		te->time_scroll.value = max - tsize;
		te->time_scroll.sliderSize = tsize;
		if(te->par_list) te->par_list->set_time(te->s_time);
		te->scroll_update=True;
	} else te->time_scroll.sliderSize = tsize;


// update all the path editor objects
        if(te->par_list) te->par_list->set_delta(te->delta);

	// te->end_time->set_time_in_days(te->s_time+d);

	// update start_time
	te->update_start_end_text();

	te->rob.set_sensitive(1, False);
} 

void telemedit::timeStartActivateCB(WObject *, XtPointer)
{
}

void telemedit::timeStartLosingFocusCB(WObject *, XtPointer)
{
}

void telemedit::timeEndActivateCB(WObject *, XtPointer)
{
}

void telemedit::timeEndLosingFocusCB(WObject *, XtPointer)
{
}

void telemedit::telem_timebar_action(double time1, double time2, XtPointer data)
{
	telemedit *te = (telemedit*)data;

	if (time1 < 0 || time2 < 0) return;

	if (time1 < time2) {
		te->time_1 = time1;
		te->time_2 = time2;
	} else {
		te->time_1 = time2;
		te->time_2 = time1;
	}

	te->rob.set_sensitive(1, True);
}

void    telemedit::UserResetCB(WObject*, XtPointer, XtPointer udata)
{
	telemedit *te = (telemedit*)udata;

	te->set_start_end(te->scet_start, te->scet_end);
	//te->rob.set_sensitive(0, False);
	te->rob.set_sensitive(1, False);
	te->rob.set_sensitive(2, False);
	strcpy(te->prev_scet_start, te->scet_start);
	strcpy(te->prev_scet_end, te->scet_end);

}

void    telemedit::UserZoomInCB(WObject*, XtPointer, XtPointer udata)
{
	telemedit *te = (telemedit*)udata;

	if (te->time_2 - te->time_1 <= te->minres) {
		cout << "Cannot Zoom finer than minimum resolution" << endl;
		return;
	}

	te->rob.set_sensitive(0, True);
	te->rob.set_sensitive(1, False);
	te->rob.set_sensitive(2, True);

	char *s1 = te->start_time->get_scet_time();
	strcpy(te->prev_scet_start, s1);
	char *s2 = te->end_time->get_scet_time();
	strcpy(te->prev_scet_end, s2);

	te->start_time->set_time_in_days((te->time_1) / te->time_scale_factor);
	te->end_time->set_time_in_days((te->time_2) / te->time_scale_factor);
	te->start_time->set_time_in_sec(te->time_1);
	te->end_time->set_time_in_sec(te->time_2);

	double delta = te->time_2 = te->time_1;

	char st[25], end[25];

	char *s = te->start_time->get_scet_time();
	strcpy(st, s);
	char *e = te->end_time->get_scet_time();
	strcpy(end, e);

	te->delta = delta;
	te->set_start_end(st, end);

	te->time_1 = te->time_2 = -1;
}

void    telemedit::UserZoomOutCB(WObject*, XtPointer, XtPointer udata)
{
	telemedit *te = (telemedit*)udata;
	te->set_start_end(te->prev_scet_start, te->prev_scet_end);
	te->rob.set_sensitive(1, False);
	te->rob.set_sensitive(2, False);
}

void telemedit::update_start_end_text()
{
	start_time->set_time_in_days((s_time) / time_scale_factor);
	end_time->set_time_in_days((s_time+delta) / time_scale_factor);
	start_time->set_time_in_sec(s_time);
	end_time->set_time_in_sec(s_time+delta);

	TimeUnits::units ou; 

	switch (get_current_units()) {
	  case TimeUnits::YEAR:
	    ou = TimeUnits::DOY;
	    break;
	  case TimeUnits::DOY:
	    ou = TimeUnits::HR;
	    break;
	  case TimeUnits::HR:
	    ou = TimeUnits::MIN;
	    break;
	  case TimeUnits::MIN:
	    ou = TimeUnits::SEC;
	    break;
	  case TimeUnits::SEC:
	  case TimeUnits::MSEC:
	    ou = TimeUnits::MSEC;
	    break;
	}

    	time_start.value = start_time->get_scet_time(ou);
    	time_end.value = end_time->get_scet_time(ou);
}

// Set the text in the Visible Time Range text field to show value d (I)
void telemedit::set_delta_text(double d)
{
d /= 86400.0;
	switch (get_current_units()) {
	  case TimeUnits::YEAR:
	    d /= 365.0;
	    break;
	  case TimeUnits::DOY:
	    break;
	  case TimeUnits::HR:
	    d *= 24.0;
	    break;
	  case TimeUnits::MIN:
	    d *= 1440.0;
	    break;
	  case TimeUnits::SEC:
	    d *= 86400.0;
	    break;
	  case TimeUnits::MSEC:
	    d *= (86400.0*1000.0);
	    break;
	}

char buf[20];
sprintf(buf,"%.4g",d);
delta_text.value=buf;
}

