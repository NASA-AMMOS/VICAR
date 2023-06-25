//  ParEdit.C	Classes for editing paths - Member Functions
//
// Written by Dave Kagels for the ASV task - 4/13/94
//
// Copyright (C) 1994, California Institute of Technology
// All rights reserved.

#include <iostream.h>

#include "path/ParEdit.h"
#include <math.h>
#include <iomanip.h>

#define HIPRESS	0

// Allocate a color on display d (I), in colormap c (I), with components
// r,g,b (I).  Return the color index (Pixel)
Pixel color_alloc(Display *d, Colormap c, int r, int g, int b)
{
XColor	xc;

xc.red=(unsigned short)(r<<8);
xc.green=(unsigned short)(g<<8);
xc.blue=(unsigned short)(b<<8);
if(!XAllocColor(d,c,&xc)) {
	fprintf(stderr,"Unable to allocate color %d,%d,%d.\n",r,g,b);
	return (unsigned long)0;
	}
return xc.pixel;
}

// Allocate a read only color on display d (I) from the default colormap
// using the default screen, given a color name name(I).
// Return the color index (Pixel)
Pixel named_color_alloc(Display *d, char *name, int &status)
{
        XColor xc_screen;
        XColor xc_exact;
	status = 0;

        Status s = XAllocNamedColor(d, DefaultColormap(d, DefaultScreen(d)), 
		name, &xc_screen, &xc_exact);

	if (!s) status = -1;
	return xc_screen.pixel;
}


// ***************************** ParEdit **********************************

// Initialize variables to default settings
void ParEdit::init()
{
s_time=0.0;
delta=0.0;
disp=NULL;
dwin=(Window)NULL;
pmap=(Pixmap)NULL;
gc=NULL;
bcur=-1;
time_scale_factor = 1.0;
time_offset = 0.0;
}


// Create the basic interface objects and set up mouse event handlers
// with form name (I) and parent object p (I)
// Return a pointer to the form object that the interface is built on
Form *ParEdit::create(String name, WObject &p)
{
form.create(name,p);
form.data=(XtPointer)this;

// Create the name label:
namelab.marginWidth=1;
namelab.marginLeft=2;
namelab.marginHeight=0;
namelab.alignment=XmALIGNMENT_BEGINNING;
namelab.width=130;
namelab.recomputeSize=False;
namelab.left.attach();
namelab.top.attach();
namelab.bottom.attach();
namelab.bottom.offset = 15;
namelab.highlightThickness=0;
namelab.create((char *)"namelabel",form);
namelab.data=(XtPointer)this;

// Create the drawing area (where the paths will be displayed and edited)
draw.top.attach();
draw.bottom.attach();
draw.right.attach();
draw.left=namelab;
draw.height = 300;
draw.create((char *)"draw",form);
draw.exposeCallback=(CallbackFunc)ParEdit::exposed;
draw.resizeCallback=(CallbackFunc)ParEdit::resized;
draw.data=(XtPointer)this;
// has no size yet
vx = -1;
vy = -1;

// Set up mouse button event handlers:
XtAddEventHandler(draw, ButtonPressMask, FALSE, ParEdit::pressed,
	(XtPointer)this);
XtAddEventHandler(draw, ButtonMotionMask, FALSE, ParEdit::moved,
	(XtPointer)this);
XtAddEventHandler(draw, ButtonReleaseMask, FALSE, ParEdit::released,
	(XtPointer)this);

return &form;
}


// Destructor: clean up for nice termination
ParEdit::~ParEdit()
{
if(gc) XFreeGC(disp,gc);
if(pmap) XFreePixmap(disp,pmap);
disp=NULL;
dwin=(Window)NULL;
if((Widget)draw) {
	draw.exposeCallback=(CallbackFunc)NULL;
	draw.resizeCallback=(CallbackFunc)NULL;
	}
if((Widget)form) XtUnmanageChild(form);
}


// Set up the display related information
void ParEdit::setup_display()
{
disp=(Display *)draw;
dwin=(Window)draw;
if(!dwin) { disp=NULL; return; }
gc=XCreateGC(disp,dwin,0,NULL);
}


// Set the time shown at the left edge of the window to t (I)
void ParEdit::set_time(double t)
{
t = t * time_scale_factor + time_offset;
if(s_time==t) return;
s_time=t;
redraw();
expose();
}


// Copy the path pixmap to the window, with time t (I) at left edge
void ParEdit::expose(double t)
{
int xoff;


// default case: copy to window with no offset
if(t==TFLAG) { exposed(&draw,NULL); return; }
t = t * time_scale_factor + time_offset;

if(!dwin) {
	setup_display();
	if(!dwin) return;
	}
if(!pmap) {
	resized(&draw,NULL);
	if(!pmap) return;
	}

xoff=(int)((s_time-t)/delta*(double)vx);	// pixel offset for pixmap

XSetClipMask(disp,gc,None);
if(t<s_time)
	XClearArea(disp,dwin,0,0,xoff,vy,False);
else	XClearArea(disp,dwin,xoff+vx,0,0-xoff,vy,False);
XCopyArea(disp,pmap,dwin,gc,0,0,vx,vy,xoff,0);

}


// Callback function: called when an area of the path window is exposed
void ParEdit::exposed(DrawingArea *da, XtPointer)
{
ParEdit *p=(ParEdit *)(da->data);

if(!p->dwin) {
	p->setup_display();
	if(!p->dwin) return;
	}
if(!p->pmap) {
	resized(&p->draw,NULL);
	if(!p->pmap) return;
	}

XSetClipMask(p->disp,p->gc,None);
XCopyArea(p->disp,p->pmap,p->dwin,p->gc,0,0,p->vx,p->vy,0,0);
}


// Callback function: called when the path window is resized
void ParEdit::resized(DrawingArea *da, XtPointer)
{
ParEdit *p=(ParEdit *)(da->data);

p->vx=da->width;
p->vy=da->height;

if(!p->dwin) {
	p->setup_display();
	if(!p->dwin) return;
	}

// get a new pixmap to fit the window:
if(p->pmap) XFreePixmap(p->disp,p->pmap);
p->pmap=XCreatePixmap(p->disp,p->dwin,p->vx,p->vy,da->depth);

p->redraw();
p->expose();
}


// Mouse button handler function: called when a mouse button is pressed
void ParEdit::pressed(Widget, XtPointer cd, XEvent *e, Boolean *)
{
ParEdit *p=(ParEdit *)cd;
double	t,w;

if(p->bcur!=-1) return;

p->mousex=e->xbutton.x;
p->mousey=e->xbutton.y;
p->bcur=e->xbutton.button;
if(p->bcur==Button3) { p->bcur=-1; }

// Compute time and vertical fraction at mouse location:
t=(double)(p->mousex)*p->delta/(double)(p->vx)+p->s_time;
w=(double)(p->mousey)/(double)(p->vy);

// Call virtual mouse button handler function (defined in derived classes)
#if HIPRESS
cout << "Called Pressed in PArEdit" << endl;
#endif
p->button_pressed(p->bcur,p->mousex,p->mousey,t,w);
}


// Mouse button handler function: called when a mouse button is dragged
void ParEdit::moved(Widget, XtPointer cd, XEvent *e, Boolean *)
{
ParEdit *p=(ParEdit *)cd;
int	nx,ny;
double	t,w;

if(p->bcur==-1) return;

nx=e->xmotion.x;
ny=e->xmotion.y;

// Compute time and vertical fraction at mouse location:
t=(double)nx*p->delta/(double)(p->vx)+p->s_time;
w=(double)ny/(double)(p->vy);

// Call virtual mouse button handler function (defined in derived classes)
p->mouse_moved(p->bcur,nx,ny,t,w);
}


// Mouse button handler function: called when a mouse button is released
void ParEdit::released(Widget, XtPointer cd, XEvent *e, Boolean *)
{
ParEdit *p=(ParEdit *)cd;
int	nx,ny;
double	t,w;

if(e->xbutton.button!=p->bcur) return;

nx=e->xmotion.x;
ny=e->xmotion.y;

// Compute time and vertical fraction at mouse location:
t=(double)nx*p->delta/(double)(p->vx)+p->s_time;
w=(double)ny/(double)(p->vy);

// Call virtual mouse button handler function (defined in derived classes)
p->button_released(p->bcur,nx,ny,t,w);
p->bcur=-1;
}


// ***************************** TmimeBar **********************************

// Initialize variables to default settings
void TimeBar::init()
{
mark_x1 = -1;
mark_x2 = -1;
button_press = 0;
button_press_data = 0;
finfo=NULL;
}


// Create the interface objects by calling the base class create function
// with form name (I) and parent object p (I)
// Return a pointer to the form object that the interface is built on
Form *TimeBar::create(String name, WObject &p)
{
ParEdit::create(name,p);				// base class create
set_label();

return &form;
}


// Set up the display related information
void TimeBar::setup_display()
{
ParEdit::setup_display();
if(!dwin) return;

// allocate colors:
bgcol=color_alloc(disp,draw.colormap,80,105,185);		// dk blue
linecol=color_alloc(disp,draw.colormap,0,0,0);			// black
textcol=color_alloc(disp,draw.colormap,255,255,255);		// white
markcol=color_alloc(disp,draw.colormap,255,0,0);		// red
indcol=color_alloc(disp,draw.colormap,0,255,0);			// green
draw.background=bgcol;

// load the font:
finfo=XLoadQueryFont(disp,"7x14");
//finfo=XLoadQueryFont(disp,"--*-*-*-normal-*-10-*-72-72-*-*-*-*");
if(!finfo) finfo=XLoadQueryFont(disp,"fixed");
f_off=0-finfo->descent;
f_height=finfo->descent+finfo->ascent;
XSetFont(disp,gc,finfo->fid);
}

static int selected = 0;
static int save_x = -1;
static int reset_marks = 1;
static double time_1 = -1, time_2 = -1;

void TimeBar::button_pressed(int, int x, int, double t, double)
{
#if HIPRESS
	cout << "Called button_pressed in TimeBar" << endl;
#endif
     if (button_press) {
	XSetForeground(disp,gc,markcol);
	if (mark_x1 < 0) {
	   XDrawLine(disp,pmap,gc,x,0,x,vy);
	   mark_x1 = x;
	   selected =  1;
	   time_1 = t;
	} else if (mark_x2 < 0) {
	   if (abs(x - mark_x1) > 5) {
	      XDrawLine(disp,pmap,gc,x,0,x,vy);
	      mark_x2 = x;
	      save_x = mark_x1;
	      selected = 2;
	      time_2 = t;
	   } else selected = 1;
	} else {
	   int abs_x1 = abs(x - mark_x1);
	   int abs_x2 = abs(x - mark_x2);

	   if (abs_x1 > 5 && abs_x2 > 5) selected = 0;
	   else if (abs_x1 <= 5 && abs_x2 > 5) {
	      selected = 1;
	      save_x = mark_x2;
	   } else if (abs_x2 <= 5 && abs_x1 > 5) {
	      selected = 2;
	      save_x = mark_x1;
	   } else {
	      if (abs_x1 <= abs_x2) {
	         save_x = mark_x2;
	         selected = 1;
	      } else {
		 save_x = mark_x1;
	         selected = 2;
	      }
	   }
	}
	expose();
     }
}

void TimeBar::button_released(int, int, int, double, double)
{
     if (button_press) {
	// Note that here we are sending local time values out to an external interface
	// Therefore, we scale from path units to global units.
	(*button_press)((time_1 - time_offset) / time_scale_factor, (time_2 - time_offset) / time_scale_factor, 
		button_press_data);
        selected = 0;
     }
}

void TimeBar::mouse_moved(int, int x, int, double time, double)
{
     if (button_press && selected) {
	   reset_marks = 0;
	   redraw();
	   XSetForeground(disp,gc,markcol);
	   if (save_x) XDrawLine(disp,pmap,gc,save_x,0,save_x,vy);
	   XDrawLine(disp,pmap,gc,x,0,x,vy);
	   expose();
	   if (selected == 1) {
		mark_x1 = x;
		time_1 = time;
	   } else {
		mark_x2 = x;
		time_2 = time;
	   }
     }
}

// Set the time at the left edge of the window to t (I) and redraw
void TimeBar::expose(double t)
{
if(t==TFLAG ) { exposed(&draw,NULL); return; }
t = t * time_scale_factor + time_offset;
if(t==s_time) { exposed(&draw,NULL); return; }
set_time(t);
}


// Redraw the timebar in the time range t1 (I) to t2 (I)
void TimeBar::redraw(double t1, double t2)
{
double	dt,t,st,it,et;
int	x1,x2,i,x,y,len;
char	buf[16];
XRectangle	xrect;


if(!pmap) return;

if (reset_marks) {
	mark_x1 = mark_x2 = -1;  // reset any drawn marks
	time_1 = time_2 = -1;
	save_x = -1;
}
reset_marks = 1;

if((!delta) || vx<3) {
	// draw blank window:
	XSetClipMask(disp,gc,None);
	XSetForeground(disp,gc,bgcol);
	XFillRectangle(disp,pmap,gc,0,0,vx,vy);
	return;
	}

if(t1==TFLAG || t2==TFLAG) {			// redraw whole thing - default
	t1=s_time;
	t2=s_time+delta;
} else {
	t1 = t1 * time_scale_factor + time_offset;
	t2 = t2 * time_scale_factor + time_offset;
}

dt=delta/(double)vx;				// compute time per pixel


// compute boundaries
x1=(int)((t1-s_time)/dt);
x2=(int)((t2-s_time)/dt)+1;

// clip graphics to boundaries
xrect.x=(short)x1;
xrect.y=(short)0;
xrect.width=(short)(x2-x1);
xrect.height=(short)vy;
XSetClipRectangles(disp,gc,0,0,&xrect,1,Unsorted);

// erase old stuff
XSetForeground(disp,gc,bgcol);
XFillRectangle(disp,pmap,gc,x1,0,x2-x1,vy);

// draw new stuff
XSetForeground(disp,gc,linecol);
y=(int)((vy-f_height)*0.98);
if(t1<0.0) st=(double)((int)t1-1);
else	st=(double)((int)t1);
int mult = 1;
if (dt > .18) {
  int i = 1;
  while (delta / i++ > 100);
  mult = i-1;
}

for(t=st;t<=t2;t+=(1.0*mult)) {				// whole mult seconds
	x=(int)((t-s_time)/dt);
	XDrawLine(disp,pmap,gc,x,0,x,y);
	}

if(dt<=0.1) {						// half seconds
	y=(int)((vy-f_height)*0.73);
	for(t=st+0.5;t<=t2;t+=1.0) {
		x=(int)((t-s_time)/dt);
		XDrawLine(disp,pmap,gc,x,0,x,y);
		}
	}
if(dt<=0.02) {						// tenths of seconds
	y=(int)((vy-f_height)*0.5);
	i=1;
	for(t=st+0.1;t<t2;t+=0.1) {
		if(!((i++)%5)) continue;
		x=(int)((t-s_time)/dt);
		XDrawLine(disp,pmap,gc,x,0,x,y);
		}
	}
if(dt<=0.0066667) {					// thirtieths of seconds
	y=(int)((vy-f_height)*0.3);
	i=1;
	for(t=st+0.0333333333333333;t<t2;t+=0.0333333333333333) {
		if(!((i++)%3)) continue;
		x=(int)((t-s_time)/dt);
		XDrawLine(disp,pmap,gc,x,0,x,y);
		}
	}
if(dt<=0.0033334) {					// sixtieths of seconds
	y=(int)((vy-f_height)*0.17);
	for(t=st+0.01666666666666667;t<t2;t+=0.0333333333333333) {
		x=(int)((t-s_time)/dt);
		XDrawLine(disp,pmap,gc,x,0,x,y);
		}
	}

// draw the text
XSetForeground(disp,gc,textcol);
if(dt>0.04) it=10.0*mult;
else	it=1.0;
if(t1<0.0) st=(double)((int)(t1/it)-1)*it;
else	st=(double)((int)(t1/it))*it;
et=t2+15.0*dt;
for(t=st;t<=et;t+=it) {					// seconds
	x=(int)((t-s_time)/dt);
	sprintf(buf,"%.0lf",t);
	len=strlen(buf);
	if(t==t1) i=x;
	else	i=x-XTextWidth(finfo,buf,len)/2;
	XDrawString(disp,pmap,gc,i,vy+f_off,buf,len);
	}
if(dt<=0.0025) {					// tenths of seconds
	if(t1<0.0) i=(int)(t1*10.0)-1;
	else	i=(int)(t1*10.0);
	y=vy-(int)((vy-f_height)*0.2);
	for(t=i*0.1;t<=et;t+=0.1) {
		if(!((i++)%10)) continue;
		sprintf(buf,"%.1lf",t);
		len=strlen(buf);
		x=(int)((t-s_time)/dt)-XTextWidth(finfo,buf,len)/2;
		XDrawString(disp,pmap,gc,x,y+f_off,buf,len);
		}
	}
}

void TimeBar::draw_time_indicator(double tt)
{
	tt = tt * time_scale_factor + time_offset;
	if (vx < 0 || vy < 0) return;
	int x = (int)((tt - s_time) * vx / delta + 0.5);
	// erase the old indicator
	redraw();
	XSetForeground(disp,gc,indcol);
	XDrawLine(disp,pmap,gc,x,0,x,vy);
	expose();
}

// Redraw the  scet timebar in the time range t1 (I) to t2 (I)
void TelemTimeBar::redraw(double t1, double t2)
{
double	dt,t,st,it,et;
int	x1,x2,i,x,y,len;
char	buf[16];
XRectangle	xrect;

if(!pmap) return;

if (reset_marks) {
	mark_x1 = mark_x2 = -1;  // reset any drawn marks
	time_1 = time_2 = -1;
	save_x = -1;
}
reset_marks = 1;

if((!delta) || vx<3) {
	// draw blank window:
	XSetClipMask(disp,gc,None);
	XSetForeground(disp,gc,bgcol);
	XFillRectangle(disp,pmap,gc,0,0,vx,vy);
	return;
	}

if(t1==TFLAG || t2==TFLAG) {			// redraw whole thing - default
	t1=s_time;
	t2=s_time+delta;
} else {
	t1 = t1 * time_scale_factor + time_offset;
	t2 = t2 * time_scale_factor + time_offset;
}

int y_s, d_s, h_s, m_s, s_s, ms_s;
int y_e, d_e, h_e, m_e, s_e, ms_e;
//ScEventTime stime, etime, sstime;
//stime->set_time_in_days(t1 / time_scale_factor);
//etime->set_time_in_days(t2 / time_scale_factor);
//sstime->set_time_in_days(s_time / time_scale_factor);
stime->set_time_in_sec(t1);
etime->set_time_in_sec(t2);
sstime->set_time_in_sec(s_time);

stime->get_scet_time(&y_s, &d_s, &h_s, &m_s, &s_s, &ms_s);
etime->get_scet_time(&y_e, &d_e, &h_e, &m_e, &s_e, &ms_e);

double st_time;
switch(time_units) {
   case TimeUnits::YEAR:
        t1 = stime->get_year();
        t2 = etime->get_year();
        st_time = sstime->get_year();
        break;
   case TimeUnits::DOY:
        t1 = stime->get_doy();
        t2 = etime->get_doy();
	if (y_e != y_s) t2 += 365;
        st_time = sstime->get_doy();
        break;
   case TimeUnits::HR:
        t1 = stime->get_hour();
        t2 = etime->get_hour();
	if (d_e != d_s) t2 += 24;
        st_time = sstime->get_hour();
        break;
   case TimeUnits::MIN:
        t1 = stime->get_min();
        t2 = etime->get_min();
	if (h_e != h_s) t2 += 60;
        st_time = sstime->get_min();
        break;
   case TimeUnits::SEC:
        t1 = stime->get_sec();
        t2 = etime->get_sec();
	if (m_e != m_s) t2 += 60;
        st_time = sstime->get_sec();
	break;
   case TimeUnits::MSEC:
	t1 = ms_s;
	t2 = ms_e;
	if (s_e != s_s) t2 += 1000;
        st_time = ms_s;
        break;
}

double del = t2 - t1;

dt=del/(double)vx;                      // compute time per pixel

// compute boundaries
x1=(int)((t1-st_time)/dt);
x2=(int)((t2-st_time)/dt)+1;

// clip graphics to boundaries
xrect.x=(short)x1;
xrect.y=(short)0;
xrect.width=(short)(x2-x1);
xrect.height=(short)vy;
XSetClipRectangles(disp,gc,0,0,&xrect,1,Unsorted);

// erase old stuff
XSetForeground(disp,gc,bgcol);
XFillRectangle(disp,pmap,gc,x1,0,x2-x1,vy);

// draw new stuff
XSetForeground(disp,gc,linecol);
y=(int)((vy-f_height)*0.98);
if(t1<0.0) st=(double)((int)t1-1);
else	st=(double)((int)t1);

int mult = 1;
if (dt > .18) {
  int i = 1;
  while (del / i++ > 100);
  mult = i-1;
}

for(t=st;t<=t2;t+=(1.0*mult)) {				// whole units 
	x=(int)((t-st_time)/dt);
	XDrawLine(disp,pmap,gc,x,0,x,y);
}

if(dt<=0.1) {						// half units 
	y=(int)((vy-f_height)*0.73);
	for(t=st+0.5;t<=t2;t+=1.0) {
		x=(int)((t-st_time)/dt);
		XDrawLine(disp,pmap,gc,x,0,x,y);
	}
}

if (time_units != TimeUnits::SEC && time_units != TimeUnits::MSEC)
{
   if(dt<=0.05) {				// quarter of units 
	y=(int)((vy-f_height)*0.63);
	i=1;
	for(t=st+0.25; t<t2; t+=.25) {
		if(!((i++)%2)) continue;
		x=(int)((t-st_time)/dt);
		XDrawLine(disp,pmap,gc,x,0,x,y);
	}
   }
}

if (time_units == TimeUnits::SEC || time_units == TimeUnits::MSEC) {
   if(dt<=0.02) {		// tenths
	y=(int)((vy-f_height)*0.5);
	i=1;
	for(t=st+0.1;t<t2;t+=0.1) {
		if(!((i++)%5)) continue;
		x=(int)((t-st_time)/dt);
		XDrawLine(disp,pmap,gc,x,0,x,y);
	}
   }
} 

if (time_units == TimeUnits::YEAR || time_units == TimeUnits::HR || 
					time_units == TimeUnits::MIN) {

   if(dt<=0.016666667) {		// twelvths
	y=(int)((vy-f_height)*0.5);
	i=1;
	for(t=st+0.0833333;t<t2;t+=0.0833333) {
		if(!((i++)%3)) continue;
		x=(int)((t-st_time)/dt);
		XDrawLine(disp,pmap,gc,x,0,x,y);
	}
   }
} 

if (time_units == TimeUnits::YEAR) {
   i = 1;
   if(dt<=0.00833334) {			// 1/24 of units 
	y=(int)((vy-f_height)*0.3);
	for(t=st+0.041666666667;t<t2;t+=0.041666666667) {
		if(!((i++)%2)) continue;
		x=(int)((t-st_time)/dt);
		XDrawLine(disp,pmap,gc,x,0,x,y);
	}
   }
}

if (time_units == TimeUnits::DOY) {
   if(dt<=0.00833334) {			// 1/24 of units 
   	i = 1;
	y=(int)((vy-f_height)*0.3);
	for(t=st+0.041666666667;t<t2;t+=0.041666666667) {
		if(!((i++)%6)) continue;
		x=(int)((t-st_time)/dt);
		XDrawLine(disp,pmap,gc,x,0,x,y);
	}
   }

   if(dt<=0.004166667) {			// 1/48 of units 
	i = 1;
	y=(int)((vy-f_height)*0.2);
	for(t=st+0.0208333333;t<t2;t+=0.0208333333) {
		if(!((i++)%2)) continue;
		x=(int)((t-st_time)/dt);
		XDrawLine(disp,pmap,gc,x,0,x,y);
	}
   }
}

if (time_units == TimeUnits::MIN) {
   if(dt<=0.0033334) {					// sixtieths of seconds
	i = 1;
	y=(int)((vy-f_height)*0.17);
	for(t=st+0.01666666666666667;t<t2;t+=0.01666666666666667) {
		if(!((i++)%5)) continue;
		x=(int)((t-st_time)/dt);
		XDrawLine(disp,pmap,gc,x,0,x,y);
	}
   }
}


// draw the text
XSetForeground(disp,gc,textcol);
if(dt>0.04) it=10.0*mult;
else	it=1.0;
st=(double)((int)(t1/it))*it;
et=t2+15.0*dt;
int prev_end=-10000;

   double unit;
   for(t=st;t<=et;t+=it) {					// units 
	x=(int)((t-st_time)/dt);
	unit = t;
/*
        if (time_units == TimeUnits::DOY && unit > 364) unit -= 365;
        if (time_units == TimeUnits::HR && unit > 23) unit -= 24;
        if (time_units == TimeUnits::MIN && unit > 59) unit -= 60;
        if (time_units == TimeUnits::SEC && unit > 59) unit -= 60;
*/
	sprintf(buf,"%.0lf",unit);
	len=strlen(buf);
	if(t==t1) i=x;
	else	i=x-XTextWidth(finfo,buf,len)/2;
	if(i > prev_end+2) {	// do not write over previous value
		XDrawString(disp,pmap,gc,i,vy+f_off,buf,len);
		prev_end = 2*x-i;
	}
   }
   if(dt<=0.0025) {					// tenths of seconds
      if (time_units == TimeUnits::SEC || time_units == TimeUnits::MSEC) {
	if(t1<0.0) i=(int)(t1*10.0)-1;
	else	i=(int)(t1*10.0);
	y=vy-(int)((vy-f_height)*0.2);
	for(t=i*0.1;t<=et;t+=0.1) {
		unit = t;
		if(!((i++)%10)) continue;
        	if (time_units == TimeUnits::SEC && unit > 59) unit -= 60;
		sprintf(buf,"%.1lf",t);
		len=strlen(buf);
		x=(int)((t-st_time)/dt)-XTextWidth(finfo,buf,len)/2;
		XDrawString(disp,pmap,gc,x,y+f_off,buf,len);
	}
      } else if (time_units == TimeUnits::DOY) {
	int doy, hr;
	y=vy-(int)((vy-f_height)*0.2);
	i = 1;
	int ct = 0;
	int tt = (int)t1;
	for(t=tt+0.25;t<=et;t+=0.25) {
		if(!((i++)%4)) {
			ct = 0;
			continue;
		} else ct++;
		doy = (int)t;
		if (doy > 364) doy -= 365;
		hr = ct * 6;
		sprintf(buf,"%d:%.02d",doy, hr);
		len=strlen(buf);
		x=(int)((t-st_time)/dt)-XTextWidth(finfo,buf,len)/2;
		XDrawString(disp,pmap,gc,x,y+f_off,buf,len);
	}
      } else {			// years, hours, minutes
	int u1, u2;
	y=vy-(int)((vy-f_height)*0.2);
	i = 1;
	int ct = 0;
	int tt = (int)t1;
	for(t=tt+0.083333333;t<=et;t+=0.083333333) {
		if(!((i++)%12)) { 
			ct = 0; 
			continue; 
		} else ct++;
		u1 = (int)t;
		if (time_units == TimeUnits::YEAR) {
			if (u1 >= 1900) u1 -= 1900;
			else if (u1 >= 2000) u1 -= 2000;
			u2 = ct;
		} else {
			if (u1 > 59) u1 -= 60;
			u2 = ct*5;
		}
		sprintf(buf,"%d:%.02d",u1, u2);
		len=strlen(buf);
		x=(int)((t-st_time)/dt)-XTextWidth(finfo,buf,len)/2;
		XDrawString(disp,pmap,gc,x,y+f_off,buf,len);
	}
     }
   }
}


// ***************************** SplineEdit ******************************

// Popup menu data:

String	spop_names[]={ (char *)"Add Keyframe", (char *)"Delete Keyframe",
		(char *)"Scale", (char *)"Grid", (char *)"Primary Boxes", (char *)"Aux Boxes", 
		(char *)"Keyframe Lines", (char *)"Done" };

XmButtonType	spop_types[]= { XmPUSHBUTTON, XmPUSHBUTTON,
			XmCASCADEBUTTON, XmCHECKBUTTON, XmPUSHBUTTON, 
			XmPUSHBUTTON, XmPUSHBUTTON, XmPUSHBUTTON };

Boolean	spop_states[]={ False, False, False, True, False, False, False, 
			 False };

CallbackFunc	spop_funcs[]={ 
			SplineEdit::add_keyframe,
			SplineEdit::del_keyframe,
			NULL,
			SplineEdit::grid_changed,
			SplineEdit::primary_control_boxes,
			SplineEdit::aux_control_boxes,
			SplineEdit::keyframe_lines,
			NULL };

String	sub_names[2]={ (char *)"Left", (char *)"Right" };

XmButtonType	sub_types[2]= { XmCHECKBUTTON, XmCHECKBUTTON };

Boolean	sub_states[2]={ True, True };

CallbackFunc	sub_funcs[2]={	SplineEdit::left_changed,
				SplineEdit::right_changed };

SendSplineEditEvents SplineEdit::send_spline_edit_events= 0;
XtPointer SplineEdit::send_spline_edit_events_data= 0;

// Initialize variables to default settings
void SplineEdit::init()
{
sp=NULL;
sel_time=TFLAG;
sel_val=TFLAG;
sel_wgt=-1;
sel_kf=-1;
grid=True;
textleft=True;
textright=True;

minval=-1.0;
maxval=1.0;

num_aux_paths = 0;
for (int i = 0; i < MAX_AUX_PATHS; i++) {
	sp_aux[i] = 0;
	draw_aux_boxes[i] = False;
}

move_horizontal = 1;
editable = True;
draw_kf_lines = True;
draw_primary_boxes = True;

pop_items[0]=NULL;
pop_items[4]=NULL;
sub_items[0]=NULL;
finfo=NULL;
call_set_draw_func_on_redraw = 1;
set_draw_func = 0;
set_draw_func_data = 0;

}

void SplineEdit::set_editable(Boolean tf)
{
	editable = tf;
	move_horizontal = tf;
	for (int i = 0; i < 2; i++) {
		((PushButton*)pop_items[i])->sensitive = tf;
	}
}

void SplineEdit::get_drawing_params(Display **d, GC *g, Pixmap *p, 
                        int *x11, int *x22, double *t11,
			double *dtt, double *dvv, int *vyy)
{
	*d = disp; *g = gc, *p = pmap; *vyy = vy;
	*t11 = s_time;
	*dtt = delta/(double)vx;  // compute time per pixel
	*dvv = (double)vy / (maxval - minval);

	// compute boundaries
	double t2 = s_time + delta;
	*x11 = (int)((*t11 - s_time) / *dtt);
	*x22 = (int)((t2 - s_time) / *dtt) + 1;
}


// Create the interface objects by calling the base class create function
// with form name (I) and parent object p (I), and creating a popup menu
// Return a pointer to the form object that the interface is built on
Form *SplineEdit::create(String name, WObject &p)
{
int	i;

ParEdit::create(name,p);

spop_states[3]=grid;


pop=CreatePopupMenu((char *)"popup_menu",&form,8,pop_items,spop_types,spop_names,
	spop_states,spop_funcs);
sub=CreatePulldownMenu((char *)"sub_menu",(CascadeButton *)pop_items[2],2,sub_items,
	sub_types,sub_names,sub_states,sub_funcs);

for(i=0;i<8;i++) {
	pop_items[i]->data=(XtPointer)this;
	((Label *)pop_items[i])->marginHeight=0;
	if(i!=3) ((Label *)pop_items[i])->marginLeft=0;
	else	((ToggleButton *)pop_items[i])->visibleWhenOff=True;
	}
for(i=0;i<2;i++) {
	sub_items[i]->data=(XtPointer)this;
	((Label *)sub_items[i])->marginHeight=0;
	((ToggleButton *)sub_items[i])->visibleWhenOff=True;
	}

return &form;
}


// Set the visible range of values (vertical) to x1 (I) to x2 (I)
void SplineEdit::set_limits(double x1, double x2, int do_redraw)
{
	if(x2-x1<0.0001) x2=x1+0.0001;

	if(x1==minval && x2==maxval) return;

	minval=x1;
	maxval=x2;
	if (do_redraw) {
		redraw();
		expose();
	}
}


// Callback function: called when the Grid option is toggled
void SplineEdit::grid_changed(WObject *obj, XtPointer)
{
SplineEdit *p=(SplineEdit *)(obj->data);

p->grid=((ToggleButton *)obj)->set;
p->redraw();
p->expose();
}

// Method to set the grid drawing status
void SplineEdit::set_draw_grid(Boolean tf)
{
	grid = tf;
	((ToggleButton *)(pop_items[3]))->set = tf;
	redraw();
	expose();
}


// Method to set the scale drawing status
void SplineEdit::set_draw_scales(Boolean tl, Boolean tr)
{
	textleft = tl;
	textright = tr;
	((ToggleButton *)(sub_items[0]))->set = textleft;
	((ToggleButton *)(sub_items[1]))->set = textright;
	redraw();
	expose();
}


// Callback function: called when the Scale - Left option is toggled
void SplineEdit::left_changed(WObject *obj, XtPointer)
{
SplineEdit *p=(SplineEdit *)(obj->data);

p->textleft=((ToggleButton *)obj)->set;
p->redraw();
p->expose();
}


// Callback function: called when the Scale - Right option is toggled
void SplineEdit::right_changed(WObject *obj, XtPointer)
{
SplineEdit *p=(SplineEdit *)(obj->data);

p->textright=((ToggleButton *)obj)->set;
p->redraw();
p->expose();
}


// Callback function: called when user selects Delete Keyframe
void SplineEdit::del_keyframe(WObject *obj, XtPointer)
{
	SplineEdit *p=(SplineEdit *)(obj->data);

	if(p->sel_kf<0 || !(p->sp)) return;

	int save_kf = p->sel_kf;
	(p->sp)->delete_knot(p->sel_kf);
	p->sel_kf=-1;
	p->sel_wgt=-1;

	if (p->send_spline_edit_events) (*(p->send_spline_edit_events))
		(Event::DELETE, save_kf, -1.0, p->sp, 
		send_spline_edit_events_data);

	p->redraw();
	p->expose();
}


// Callback function: called when user selects Add Keyframe
void SplineEdit::add_keyframe(WObject *obj, XtPointer)
{
	SplineEdit *p=(SplineEdit *)(obj->data);
	int	i,n;

	if(p->sel_time==TFLAG || !(p->sp)) return;

	n=(p->sp)->get_numpts();
	for(i=0;i<n;i++) {
		if((p->sp)->get_time_at_index(i)==p->sel_time) return;
	}
	double value = (p->sp)->get_value(p->sel_time);
	(p->sp)->insert_knot(p->sel_time,value);

	for(i=0;i<n;i++) {
		if((p->sp)->get_time_at_index(i)==p->sel_time) break;
	}
	p->sel_kf=i;
	p->sel_wgt=-1;
	double save_time = p->sel_time;
	p->sel_time=TFLAG;

	if (p->send_spline_edit_events) {
   		if (save_time > p->sp->get_time_at_index(n-1)) {
      			(*(p->send_spline_edit_events))(Event::ADD, 
		p->sel_kf, save_time, p->sp, send_spline_edit_events_data);
   		} else {
      			(*(p->send_spline_edit_events))(Event::INSERT, 
		p->sel_kf, save_time, p->sp, send_spline_edit_events_data);
		}
   	}
	p->redraw();
	p->expose();
}

// Callback function: called when the Grid option is toggled
void SplineEdit::aux_control_boxes(WObject *obj, XtPointer)
{
	SplineEdit *p=(SplineEdit *)(obj->data);

	Boolean tf = True;
	for (int i = 0; i < p->num_aux_paths; i++) {
		if (p->draw_aux_boxes[i] == True) tf = False;	
		p->draw_aux_boxes[i]=tf;
	}
	p->redraw();
	p->expose();
}

// Callback function: called when the Grid option is toggled
void SplineEdit::primary_control_boxes(WObject *obj, XtPointer)
{
	SplineEdit *p=(SplineEdit *)(obj->data);

	Boolean tf = True;
	if (p->draw_primary_boxes == True) tf = False;
	p->draw_primary_boxes=tf;
	p->redraw();
	p->expose();
}

// Callback function: called when the Grid option is toggled
void SplineEdit::keyframe_lines(WObject *obj, XtPointer)
{
	SplineEdit *p=(SplineEdit *)(obj->data);

	Boolean tf = True;
	if (p->draw_kf_lines == True) tf = False;

	p->draw_kf_lines=tf;
	p->redraw();
	p->expose();
}

// Set up the display related information
void SplineEdit::setup_display()
{
ParEdit::setup_display();
if(!dwin) return;

// allocate colors:
bgcol=color_alloc(disp,draw.colormap,80,105,185);		// dk blue
gridcol=color_alloc(disp,draw.colormap,90,120,210);		// dk blue
grid10col=color_alloc(disp,draw.colormap,100,135,235);		// dk blue
linecol=color_alloc(disp,draw.colormap,0,0,0);			// black
kfcol=color_alloc(disp,draw.colormap,0,225,30);			// green
textcol=color_alloc(disp,draw.colormap,255,255,255);		// white
hicol=color_alloc(disp,draw.colormap,255,0,0);			// red
boxcol=color_alloc(disp,draw.colormap,255,235,0);		// yellow
tcol=color_alloc(disp,draw.colormap,100,225,255);		// blue-green
draw.background=bgcol;

// load the font:
finfo=XLoadQueryFont(disp,"6x13");
if(!finfo) finfo=XLoadQueryFont(disp,"fixed");
f_off=(finfo->ascent - finfo->descent)/2;
XSetFont(disp,gc,finfo->fid);
}


// Redraw the path in the time range t1 (I) to t2 (I)
void SplineEdit::redraw(double t1, double t2)
{
double	dt,t,v,dv;
long long int	x1,x2,i,n,x,y,py,len,wx1,wx2;
XRectangle	xrect;
char	buf[20],*str;

if(!pmap) return;
if((!sp) || (!delta) || vx<3) {
	// draw blank window:
	XSetClipMask(disp,gc,None);
	XSetForeground(disp,gc,bgcol);
	XFillRectangle(disp,pmap,gc,0,0,vx,vy);
	return;
	}


if(t1==TFLAG || t2==TFLAG) {			// redraw whole thing - default
	t1=s_time;
	t2=s_time+delta;
} else {
	t1 = t1 * time_scale_factor + time_offset;
	t2 = t2 * time_scale_factor + time_offset;
}

dt=delta/(double)vx;				// compute time per pixel
// compute boundaries
x1=(int)((t1-s_time)/dt);
x2=(int)((t2-s_time)/dt)+1;


// clip graphics to boundaries
xrect.x=(short)x1;
xrect.y=(short)0;
xrect.width=(short)(x2-x1);
xrect.height=(short)vy;
XSetClipRectangles(disp,gc,0,0,&xrect,1,Unsorted);

// erase old stuff
XSetForeground(disp,gc,bgcol);
XFillRectangle(disp,pmap,gc,x1,0,x2-x1,vy);

// draw new stuff


dv=(double)vy/(maxval-minval);

if(grid) {
  // value grid lines:
  v=pow(10.0,(double)((long long int)log10(500000.0/dv)-4));
  if(minval>=0.0) x=(long long int)(minval/v+0.79);
  else	x=(long long int)(minval/v-0.21);
  for(t=(double)x*v;t<maxval;t+=v) {
	y=vy-(int)((t-minval)*dv);
	if((x++)%10) XSetForeground(disp,gc,gridcol);
	else	XSetForeground(disp,gc,grid10col);
	XDrawLine(disp,pmap,gc,x1,y,x2,y);
	}
  y=vy-(long long int)((t-minval)*dv);
  if(x%10) XSetForeground(disp,gc,gridcol);
  else	XSetForeground(disp,gc,grid10col);
  XDrawLine(disp,pmap,gc,x1,y,x2,y);

  // time grid lines:
  v=pow(10.0,(double)((long long int)log10(dt*500000.0)-4));
  if(t1>=0.0) y=(long long int)(t1/v+0.79);
  else	y=(long long int)(t1/v-0.21);
  for(t=(double)y*v;t<t2;t+=v) {
	x=(long long int)((t-s_time)/dt);
	if((y++)%10) XSetForeground(disp,gc,gridcol);
	else	XSetForeground(disp,gc,grid10col);
	XDrawLine(disp,pmap,gc,x,0,x,vy);
	}
  x=(int)((t-s_time)/dt);
  if(y%10) XSetForeground(disp,gc,gridcol);
  else	XSetForeground(disp,gc,grid10col);
  XDrawLine(disp,pmap,gc,x,0,x,vy);
  }

if(sel_time>=t1-dt && sel_time <=t2+dt) {	// draw selected lines
	x=(long long int)((sel_time-s_time)/dt);
	XSetForeground(disp,gc,hicol);
	XDrawLine(disp,pmap,gc,x,0,x,vy);
	}
if(sel_val>=minval && sel_val <=maxval+1.1/dv && sel_val!=TFLAG) {
	y=vy-(long long int)((sel_val-minval)*dv);
	XSetForeground(disp,gc,hicol);
	XDrawLine(disp,pmap,gc,x1,y,x2,y);
	}


n=sp->get_numpts();

if (draw_kf_lines) {
   for(i=0;i<n;i++) {				// draw keyframe lines
	t=sp->get_time_at_index(i);
	x=(int)((t-s_time)/dt);
	if(t<t1) { continue; }
	if(i==sel_kf) {
		XSetForeground(disp,gc,boxcol);
		XDrawLine(disp,pmap,gc,x,0,x,vy);
		}
	else {	XSetForeground(disp,gc,kfcol);
		XDrawLine(disp,pmap,gc,x,0,x,vy);
		}
	if(t>t2) break;
	}
}

// draw the spline curve:
XSetForeground(disp,gc,path_color);
if(sel_wgt>=0) {
	wx1=(int)((sp->get_time_at_index(sel_wgt)-s_time)/dt);
	wx2=(int)((sp->get_time_at_index(sel_wgt+1)-s_time)/dt);
	if(wx1<=x1) XSetForeground(disp,gc,textcol);
	}
else {	wx1=-1; wx2=-1; }
py=vy-(int)((sp->get_value(t1)-minval)*dv);
t=t1+dt;
for(x=x1+1;x<=x2;x++) {
	y=vy-(int)((sp->get_value(t)-minval)*dv);
	if(x==wx1) XSetForeground(disp,gc,textcol);
	XDrawLine(disp,pmap,gc,x-1,py,x,y);
	if(x==wx2) XSetForeground(disp,gc,path_color);
	py=y;
	t+=dt;
	}

// draw the auxilliary paths if present
for (int ll = 0; ll < num_aux_paths; ll++) {
   XSetForeground(disp,gc,aux_pixels[ll]);
   py=vy-(int)((sp_aux[ll]->get_value(t1)-minval)*dv);
   t=t1+dt;
   for(x=x1+1;x<=x2;x++) {
	y=vy-(int)((sp_aux[ll]->get_value(t)-minval)*dv);
	if(x==wx1) XSetForeground(disp,gc,textcol);
	XDrawLine(disp,pmap,gc,x-1,py,x,y);
	if(x==wx2) XSetForeground(disp,gc,aux_pixels[ll]);
	py=y;
	t+=dt;
   }
}

if (draw_primary_boxes) {
   for(i=0;i<n;i++) {				// draw control boxes
	t=sp->get_time_at_index(i);
	if(t<t1-dt*5.0) continue;
	if(t>t2+dt*5.0) break;
	x=(int)((t-s_time)/dt);
	y=vy-(int)((sp->get_value_at_index(i)-minval)*dv);
	if(i==sel_kf)
		XSetForeground(disp,gc,hicol);
	else	XSetForeground(disp,gc,boxcol);
	XDrawRectangle(disp,pmap,gc,x-3,y-3,6,6);
   }
}

for (int jj = 0; jj < num_aux_paths; jj++) {
      if (draw_aux_boxes[jj]) {
         for(i=0;i<n;i++) {	// draw auxilliary control boxes
	    t=sp_aux[jj]->get_time_at_index(i);
	    if(t<t1-dt*5.0) continue;
	    if(t>t2+dt*5.0) break;
	    x=(int)((t-s_time)/dt);
	    y=vy-(int)((sp_aux[jj]->get_value_at_index(i)-minval)*dv);
	    if(i==sel_kf)
		   XSetForeground(disp,gc,hicol);
	    else	XSetForeground(disp,gc,boxcol);
	     	   XDrawRectangle(disp,pmap,gc,x-3,y-3,6,6);
         }
      }
}

if(textleft || textright) {			// draw value text
	XSetForeground(disp,gc,textcol);
	v=pow(10.0,(double)((int)log10(1200000.0/dv)-4));
	if(minval>=0.0) x=(int)(minval/v+0.79);
	else	x=(int)(minval/v-0.21);
	if(v>=0.9) str=(char *)"%.0lf";
	else if(v>=0.09) str=(char *)"%.1lf";
	else if(v>=0.009) str=(char *)"%.2lf";
	else str=(char *)"%.3lf";
	for(t=(double)x*v;t<maxval;t+=v) {
		y=vy-(int)((t-minval)*dv);
		sprintf(buf,str,t);
		len=strlen(buf);
		if(textleft)  XDrawString(disp,pmap,gc,2,y+f_off,buf,len);
		if(textright) XDrawString(disp,pmap,gc,vx-
				XTextWidth(finfo,buf,len)-2,y+f_off,buf,len);
		x++;
		}
	y=vy-(int)((t-minval)*dv);
	sprintf(buf,str,t);
	len=strlen(buf);
	if(textleft)  XDrawString(disp,pmap,gc,2,y+f_off,buf,len);
	if(textright) XDrawString(disp,pmap,gc,vx-
			XTextWidth(finfo,buf,len)-2,y+f_off,buf,len);
	}

// call external drawing function if set and call_set_draw_func_on_redraw
if (set_draw_func && call_set_draw_func_on_redraw) {
	XSetForeground(disp,gc,kfcol);
	(*set_draw_func)(disp, pmap, gc, vy, x1, x2, t1, dt, 
				set_draw_func_data);
}

}

void SplineEdit::add_aux_path(vis_spline *vs, char *color) {
        if (!sp) cout << "Primary path must be adde first" << endl;
        else if (num_aux_paths >= MAX_AUX_PATHS) cout <<
                        "Exceeded max number of auxilliary paths" << endl;
        else {
		sp_aux[num_aux_paths] = vs;
		if (color) {
			int stat;
			Pixel px = named_color_alloc(form, color, stat);
			if (stat < 0) px = linecol;
			aux_pixels[num_aux_paths] = px;
		} else aux_pixels[num_aux_paths] = linecol;
		num_aux_paths++;
	}
}

void SplineEdit::replace_aux_path(int which, vis_spline *vs) {
        if (which < num_aux_paths) {
           if (vs) sp_aux[which] = vs;
           else {
              for (int j = which; j < num_aux_paths-1; j++) {
                  sp_aux[j] = sp_aux[j+1];
              }
              num_aux_paths--;
              sp_aux[num_aux_paths] = 0;
           }
        } else cout << "Not that many auxilliary paths" << endl;
}


// global variables for mouse motion:
int	b1mode;			// 0 nothing, 1 value, 2 weight, 3 move
int	b2mode;			// 0 nothing, 1 tension
int	ox,oy;			// for move
double	owgt,oavg,odev,otns;	// store original values


// Mouse button handler function: called when mouse button b (I) is pressed
// at location x,y (I), at time location t (I) and weight location w (I)
void SplineEdit::button_pressed(int b, int x, int y, double t, double w)
{
double	kt,v,s,e;
int	i,n;
char	buf[20];
#if HIPRESS
cout << "Called Button_Pressed in splineEdit" << endl;
#endif

switch(b) {
    case Button1:				// left mouse button pressed
	n=sp->get_numpts();
	// check for keyframe selection:
	for(i=0;i<n;i++) {
		kt=sp->get_time_at_index(i);
		if(fabs(t-kt) <= 4.5*delta/vx) {
		   if(t>kt && i<n-1) if(sp->get_time_at_index(i+1)-t < t-kt)
			continue;
		   sel_kf=i; break; 
		}
	}
	if(i==n) {
		v=1.0-(sp->get_value(t)-minval)/(maxval-minval);
		// check for weight selection:
		if(fabs(w-v) <=4.5/vy) {
			b1mode=2;			// weight selected
			ox=x; oy=y;
			for(i=0;i<n;i++) if(t<sp->get_time_at_index(i)) break;
			if(i==0 || i==n) {
				b1mode=3;
				oavg=(minval+maxval)*0.5;
				odev=maxval-oavg;
			} else {	
				sel_wgt=i-1;
				owgt=sp->get_weight(sel_wgt);
				redraw();
				if (owgt > BAD_WGT) {
				   sprintf(buf,"%.2lf",owgt);
				   XSetForeground(disp,gc,linecol);
				   n=strlen(buf);
				   i=ox-XTextWidth(finfo,buf,n)/2;
				   XDrawString(disp,pmap,gc,i,oy-10+f_off,
								buf,n);
				}
			}
		} else {	
			b1mode=3;
			ox=x; oy=y;
			oavg=(minval+maxval)*0.5;
			odev=maxval-oavg;
		}
	} else {	redraw();
		// check for value selection:
		v=1.0-(sp->get_value_at_index(sel_kf)-minval)/(maxval-minval);
		if(fabs(w-v) <=4.5/vy) {		// value selected
			b1mode=1;
			sprintf(buf,"%.3lf",sp->get_value_at_index(sel_kf));
			XSetForeground(disp,gc,textcol);
			n=strlen(buf);
			i=x-XTextWidth(finfo,buf,n)/2;
			XDrawString(disp,pmap,gc,i,y-10+f_off,buf,n);
		} else	b1mode=0;			// kf selected
		if (send_spline_edit_events) {
			double tm = sp->get_time_at_index(sel_kf);
			(*send_spline_edit_events)(
				Event::SELECT, sel_kf, tm, sp, 
				send_spline_edit_events_data); 
		}
					
	}
	expose();
	break;

    case Button2:				// middle mouse button pressed
	// check for keyframe drag selection:
	b2mode=0;
	n=sp->get_numpts();
	for(i=0;i<n;i++) {
		kt=sp->get_time_at_index(i);
		if(fabs(t-kt) <= 4.5*delta/vx) {
			if(t>kt && i<n-1) 
			   if(sp->get_time_at_index(i+1)-t < t-kt) continue;
			kfedit=i;
			break;
			}
		}
	if(i==n) kfedit=-1;
	else {	v=1.0-(sp->get_value_at_index(kfedit)-minval)/(maxval-minval);
		// check for tension selection:
		if(fabs(w-v) <=4.5/vy) {  // tension selected
			b2mode=1;
			sel_kf=kfedit;
			kfedit=-1;
			ox=x; oy=y;
			redraw();
			otns=sp->get_tension(sel_kf);
			if (otns > BAD_TNS) {
			   sprintf(buf,"%.2lf",otns);
			   XSetForeground(disp,gc,tcol);
			   n=strlen(buf);
			   i=ox-XTextWidth(finfo,buf,n)/2;
			   XDrawString(disp,pmap,gc,i,oy-10+f_off,buf,n);
			}
			expose();
		        if (send_spline_edit_events) {
				double tm = sp->get_time_at_index(sel_kf);
			   	(*send_spline_edit_events)(Event::SELECT, 
				sel_kf, tm, sp, send_spline_edit_events_data);
			}

		} else {	// draw keyframe time text:
		   if (move_horizontal) {
			redraw();
			sprintf(buf,"%.3lf",sp->get_time_at_index(kfedit));
			XSetForeground(disp,gc,boxcol);
			n=strlen(buf);
			i=x-XTextWidth(finfo,buf,n)/2;
			XDrawString(disp,pmap,gc,i,y-10+f_off,buf,n);
			expose();
		   } else {
			kfedit = -1;
			goto draw_text;
		   }
		}
		break;
	}
draw_text:
	// draw selected time & value text:
	sp->get_start_end(s,e);
	if(t<s) t=s;
	else if(t>e) t=e;
	sel_time=t;
	sel_val=maxval+w*(minval-maxval);
	redraw();
	sprintf(buf,"%.3lf",t);
	XSetForeground(disp,gc,boxcol);
	n=strlen(buf);
	i=x-XTextWidth(finfo,buf,n)/2;
	XDrawString(disp,pmap,gc,i,vy/2+f_off,buf,n);
	sprintf(buf,"%.3lf",sel_val);
	n=strlen(buf);
	i=vx/2-XTextWidth(finfo,buf,n)/2;
	XDrawString(disp,pmap,gc,i,y+f_off,buf,n);
	expose();
    }
}


// Mouse button handler function: called when mouse button b (I) is dragged
// at location x,y (I), at time location t (I) and weight location w (I)
void SplineEdit::mouse_moved(int b, int x, int y, double t, double w)
{
double	s,e,v,a;
int	n,i;
char	buf[20];

switch(b) {
    case Button1:				// left mouse button dragged
	switch(b1mode) {
	    case 1:					// edit value
		if (editable == False) break;
		v=maxval+w*(minval-maxval);
		sp->set_value(sel_kf,v);
		redraw();
		sprintf(buf,"%.3lf",v);
		XSetForeground(disp,gc,textcol);
		n=strlen(buf);
		i=x-XTextWidth(finfo,buf,n)/2;
		XDrawString(disp,pmap,gc,i,y-10+f_off,buf,n);
		expose();
		if (send_spline_edit_events) (*send_spline_edit_events)(
				Event::NONE, sel_kf, -1.0, sp, 
				send_spline_edit_events_data);
		break;
	    case 2:					// edit weight
	    if (editable == False) break;
	    if (sp->get_weight(sel_kf) > BAD_WGT) {
		v=owgt+((x-ox)+(y-oy))*0.05;
//		if(v<1.0) v=1.0;
//		else if(v>50.0) v=50.0;
		sp->set_weight(sel_wgt,v);
		redraw();
		sprintf(buf,"%.2lf",v);
		XSetForeground(disp,gc,linecol);
		n=strlen(buf);
		i=ox-XTextWidth(finfo,buf,n)/2;
		XDrawString(disp,pmap,gc,i,oy-10+f_off,buf,n);
		expose();
	    }
		break;
	    case 3:					// set new limits
		v=odev*pow(1.006,(double)(x-ox));
		a=oavg+(y-oy)*2.0*v/(double)vy;
		set_limits(a-v,a+v);
	    }
	break;

    case Button2:				// middle mouse button dragged
	if(b2mode==1) {
	    if (editable == False) break;
	    if (sp->get_tension(sel_kf) > BAD_TNS) {
		v=otns+((x-ox)+(y-oy))*0.05;		// edit tension
//		if(v<0.0) v=0.0;
//		else if(v>50.0) v=50.0;
		sp->set_tension(sel_kf,v);
		redraw();
		sprintf(buf,"%.2lf",v);
		XSetForeground(disp,gc,tcol);
		n=strlen(buf);
		i=ox-XTextWidth(finfo,buf,n)/2;
		XDrawString(disp,pmap,gc,i,oy-10+f_off,buf,n);
		expose();
	    }
	    break;
	 }
	if(kfedit<0) {
		// drag selected time & value bars & text:
		sp->get_start_end(s,e);
		if(t<s) t=s;
		else if(t>e) t=e;
		sel_time=t;
		sel_val=maxval+w*(minval-maxval);
		redraw();
		sprintf(buf,"%.3lf",t);
		XSetForeground(disp,gc,boxcol);
		n=strlen(buf);
		i=x-XTextWidth(finfo,buf,n)/2;
		XDrawString(disp,pmap,gc,i,vy/2+f_off,buf,n);
		sprintf(buf,"%.3lf",sel_val);
		n=strlen(buf);
		i=vx/2-XTextWidth(finfo,buf,n)/2;
		XDrawString(disp,pmap,gc,i,y+f_off,buf,n);
		expose();
		break;
		}
	// drag a keyframe:
	sp->get_start_end(s,e); s-=0.001; e+=0.001;
	n=sp->get_numpts();
	if(kfedit>0) s=sp->get_time_at_index(kfedit-1);
	if(kfedit<n-1) e=sp->get_time_at_index(kfedit+1);
	if(t<s+0.001) t=s+0.001;
	else if(t>e-0.001) t=e-0.001;
	sp->set_time(kfedit,t);
	redraw();
	// draw keyframe time text:
	sprintf(buf,"%.3lf",t);
	XSetForeground(disp,gc,boxcol);
	n=strlen(buf);
	i=x-XTextWidth(finfo,buf,n)/2;
	XDrawString(disp,pmap,gc,i,y-10+f_off,buf,n);
	if (send_spline_edit_events) (*send_spline_edit_events)(Event::NONE,
			kfedit, -1.0, sp, send_spline_edit_events_data);
	expose();
    }
}


// Mouse button handler function: called when mouse button b (I) is released
// at location x,y (I), at time location t (I) and weight location w (I)
void SplineEdit::button_released(int b, int x, int y, double t, double w)
{
double	s,e,v,a;
int	n;

switch(b) {
    case Button1:				// left mouse button released
	switch(b1mode) {
	    case 1:					// finish editing value
		if (editable == False) break;
		v=maxval+w*(minval-maxval);
		sp->set_value(sel_kf,v);
		if (send_spline_edit_events) (*send_spline_edit_events)(
				Event::MODIFY, sel_kf, -1.0, sp, 
				send_spline_edit_events_data);
		redraw();
		expose();
		break;
	    case 2:					// finish editing weight
		if (editable == False) break;
		v=owgt+((x-ox)+(y-oy))*0.05;
//		if(v<1.0) v=1.0;
//		else if(v>50.0) v=50.0;
		sp->set_weight(sel_wgt,v);
		sel_wgt=-1;
		redraw();
		expose();
		break;
	    case 3:					// finish setting limits
		v=odev*pow(1.006,(double)(x-ox));
		a=oavg+(y-oy)*2.0*v/(double)vy;
		set_limits(a-v,a+v);
	    }
	break;

    case Button2:				// middle mouse button released
	if(b2mode==1) {
		if (editable == False) break;
		v=otns+((x-ox)+(y-oy))*0.05;		// finish editing tension
//		if(v<0.0) v=0.0;
//		else if(v>50.0) v=50.0;
		sp->set_tension(sel_kf,v);
		redraw();
		expose();
		break;
		}
	if(kfedit<0) {
		// remove selected time & value text:
		sp->get_start_end(s,e);
		if(t<s) t=s;
		else if(t>e) t=e;
		sel_time=t;
		sel_val=TFLAG;
		redraw();
		expose();
		break;
		}
	// finish dragging a keyframe:
	sp->get_start_end(s,e); s-=0.001; e+=0.001;
	n=sp->get_numpts();
	if(kfedit>0) s=sp->get_time_at_index(kfedit-1);
	if(kfedit<n-1) e=sp->get_time_at_index(kfedit+1);
	if(t<s+0.001) t=s+0.001;
	else if(t>e-0.001) t=e-0.001;
	sp->set_time(kfedit,t);
	if (send_spline_edit_events) (*send_spline_edit_events)(Event::MODIFY, 
				kfedit, -1.0, sp, send_spline_edit_events_data);
	redraw();
	expose();
	kfedit=-1;
    }
}

