// xdraw.h
//
// Written by Dave Kagels 12/19/94

#ifndef _XDRAW_H_
#define _XDRAW_H_

#include <X11/X.h>
#include <X11/Xlib.h>
#include <X11/Intrinsic.h>
#ifndef NO_XPM
#include <X11/xpm.h> //used for Xpm bitmap drawing funcs m. pomerantz
#endif
#include <stdio.h>


// This class is for drawing into window or pixmaps using Xlib routines.
// It stores the necessary information (display pointer, drawable, and
// graphics context).
// 

class Xdraw {

    protected:

	Display		*display;		// Connection to X server
	Drawable	drawable;		// Window or Pixmap
	GC		gc;			// Graphics Context for drawing

	int		is_window,is_pixmap;	// Drawable type flags

	// Initialize member variables:
	void	init() { display=NULL; drawable=0; gc=NULL;
			 is_window=0; is_pixmap=0; }

    public:

	// Get functions:
	Display		*get_display() { return display; }
	Drawable	get_drawable() { return drawable; }
	GC		get_gc() { return gc; }

	// Set functions:
	void	set_display(Display *d) { display=d; }
	void	set_drawable(Drawable d) { drawable=d; is_window=0; is_pixmap=0; }
	void	set_window(Window w) { drawable=w; is_window=1; is_pixmap=0; }
	void	set_pixmap(Pixmap p) { drawable=p; is_window=0; is_pixmap=1; }
	void	set_gc(GC g) { gc=g; }

	// Typecast operators:
	operator Display*() { return get_display(); }
	operator Drawable() { return get_drawable(); }
	operator GC() { return get_gc(); }

	// Overloaded equals operators:
	void	operator=(Display *d) { set_display(d); }
	void	operator=(Drawable d) { set_drawable(d); }
	void	operator=(GC g) { set_gc(g); }

	// For opening the display (not normally used)
	Display	*OpenDisplay(char *dn=NULL) {
			display=XOpenDisplay(dn); return display; }

	// Graphics Context creation/setting functions:
	GC	CreateGC(unsigned long vm=0, XGCValues *xgcv=NULL) {
			gc=XCreateGC(display,drawable,vm,xgcv); return gc; }
	void	FreeGC() { if(gc) XFreeGC(display,gc); gc=NULL; }
	void	use_default_GC() { gc=DefaultGC(display,DefaultScreen(display)); }


#ifndef NO_XPM
	//Xpm bitmap drawing functions:   m. pomerantz 9-16-99
	void CreateXpmBitmapFromData(char** name,Pixmap* src,Pixmap* mask,XpmAttributes* atts)
	{
		Window	root;
		int	x,y;
		unsigned int	width, height, border_width, depth;

		if(display && drawable) {
			XGetGeometry(display, drawable, &root, &x, &y, &width, &height, &border_width, &depth);
		} else {
			return;
		}
		if(! (atts->valuemask & XpmVisual)) {
			//atts->visual = v;
			//atts->valuemask |= XpmVisual;
		}
		if(! (atts->valuemask & XpmDepth)) {
			atts->depth = depth;
			atts->valuemask |= XpmDepth;
		}
		if(! (atts->valuemask & XpmColormap)) {
			XWindowAttributes	win_atts;
			XGetWindowAttributes(display, root, &win_atts);
			atts->colormap = win_atts.colormap;
			atts->valuemask |= XpmColormap;
		}
		XpmCreatePixmapFromData(display,drawable,
                        name,src,mask,atts);
	}

	void PutBitmap(Pixmap map,int srcx,int srcy,int destx,int desty,int srcw,int srch,Pixmap mask=0)
	{
	//mask is used if the bitmap will be drawn, not as a rectangluar image,
	//but is an irregular shaped image where the black pixels are masked out



		if (mask)
		{
		SetClipMask(mask);
		SetClipOrigin(destx,desty);
		}

		XCopyArea(display,map,drawable,gc,srcx,srcy,srcw,srch,destx,desty);

		//reset the clipmask for the next drawing function
		if (mask)
		SetClipMask(0);

	}
#endif

/* test if really needed

	//added color stuff.  m. pomerantz
	Pixel get_named_color( char *name)
	{
        XColor xc_screen;
        XColor xc_exact;

        XAllocNamedColor(display, DefaultColormap(display, DefaultScreen(display)), name,
                        &xc_screen, &xc_exact);

        return xc_screen.pixel;
	}
*/


	// Pixmap creation functions:
	Pixmap	CreatePixmap(Drawable dr, unsigned int w, unsigned int h, unsigned int d) {
			drawable=XCreatePixmap(display,dr,w,h,d);
			is_window=0; is_pixmap=1;
			return (Pixmap)drawable; }
	Pixmap	CreatePixmap(unsigned int w, unsigned int h, unsigned int d=0) {
			if(!d) d=DefaultDepth(display,DefaultScreen(display));
			drawable=XCreatePixmap(display,DefaultRootWindow(display),w,h,d);
			is_window=0; is_pixmap=1;
			return (Pixmap)drawable; }
	Pixmap	CreatePixmapFromBitmapData(Drawable dr, char *data, unsigned int w, unsigned int h,
				Pixel fg, Pixel bg, unsigned int d) {
			drawable=XCreatePixmapFromBitmapData(display,dr,data,w,h,fg,bg,d);
			is_window=0; is_pixmap=1;
			return (Pixmap)drawable; }
	Pixmap	CreatePixmapFromBitmapData(char *data, unsigned int w, unsigned int h,
				Pixel fg, Pixel bg, unsigned int d=0) {
			if(!d) d=DefaultDepth(display,DefaultScreen(display));
			drawable=XCreatePixmapFromBitmapData(display,DefaultRootWindow(display),
				data,w,h,fg,bg,d);
			is_window=0; is_pixmap=1;
			return (Pixmap)drawable; }
	void	FreePixmap() {
			if(is_pixmap) { XFreePixmap(display,(Pixmap)drawable);
					drawable=0; is_pixmap=0; } }

	void	FreePixmap(Pixmap ldrawable) 
	{

	 XFreePixmap(display,(Pixmap)ldrawable);

	}

	// Window creation/setting functions:
	Window	CreateSimpleWindow(Window p, int x, int y, unsigned int w, unsigned int h,
				unsigned int bw=0, Pixel border=0, Pixel bg=0) {
			drawable=XCreateSimpleWindow(display,p,x,y,w,h,bw,border,bg);
			is_window=1; is_pixmap=0;
			return (Window)drawable; }
	Window	CreateWindow(Window p, int x, int y, unsigned int w, unsigned int h,
				unsigned int bw=0, int d=(int)CopyFromParent,
				unsigned int cl=(int)CopyFromParent, Visual *v=CopyFromParent,
				unsigned long vm=0, XSetWindowAttributes *att=NULL) {
			drawable=XCreateWindow(display,p,x,y,w,h,bw,d,cl,v,vm,att);
			is_window=1; is_pixmap=0;
			return (Window)drawable; }
	void	DestroyWindow() {
			if(is_window) { XDestroyWindow(display,(Window)drawable);
					drawable=0; is_window=0; } }
	void	use_root_window() { drawable=DefaultRootWindow(display);
			is_window=1; is_pixmap=0; }
	void	use_root_window(int s) { drawable=RootWindow(display,s);
			is_window=1; is_pixmap=0; }

	// Graphics Context Access Functions:
	void	ChangeGC(unsigned long vm, XGCValues *xgcv) {
			XChangeGC(display,gc,vm,xgcv); }
	void	CopyGCFrom(GC src, unsigned long vm=0x7fffff) {
			XCopyGC(display,src,vm,gc); }
	void	CopyGCTo(GC dst, unsigned long vm=0x7fffff) {
			XCopyGC(display,gc,vm,dst); }
	Status	GetGCValues(unsigned long vm, XGCValues *xgcv) {
			return XGetGCValues(display,gc,vm,xgcv); }
	void	SetArcMode(int am) { XSetArcMode(display,gc,am); }
	void	SetBackground(Pixel p) { XSetBackground(display,gc,p); }
	void	SetClipMask(Pixmap p) { XSetClipMask(display,gc,p); }
	void	SetClipOrigin(int x=0, int y=0) { XSetClipOrigin(display,gc,x,y); }
	void	SetClipRectangles(int x, int y, XRectangle *xr, int num, int o=Unsorted) {
			XSetClipRectangles(display,gc,x,y,xr,num,o); }
	void	SetDashes(int off, char *dl, int num) {
			XSetDashes(display,gc,off,dl,num); }
	void	SetFillRule(int r=EvenOddRule) { XSetFillRule(display,gc,r); }
	void	SetFillStyle(int s=FillSolid) { XSetFillStyle(display,gc,s); }
	void	SetFont(Font f) { XSetFont(display,gc,f); }
	void	SetForeground(Pixel p) { XSetForeground(display,gc,p); }
	void	SetFunction(int f) { XSetFunction(display,gc,f); }
	void	SetGraphicsExposures(Boolean ge) { XSetGraphicsExposures(display,gc,ge); }
	void	SetLineAttributes(unsigned int lw=0, int ls=LineSolid,
				int cs=CapButt, int js=JoinBevel) {
			XSetLineAttributes(display,gc,lw,ls,cs,js); }
	void	SetPlaneMask(unsigned long pm) { XSetPlaneMask(display,gc,pm); }
	void	SetPlaneMask() { XSetPlaneMask(display,gc,AllPlanes); }
	void	SetRegion(Region r) { XSetRegion(display,gc,r); }
	void	SetState(Pixel fg, Pixel bg, int f, unsigned long pm) {
			XSetState(display,gc,fg,bg,f,pm); }
	void	SetState(Pixel fg, Pixel bg) {
			XSetState(display,gc,fg,bg,GXcopy,AllPlanes); }
	void	SetStipple(Pixmap p) { XSetStipple(display,gc,p); }
	void	SetSubwindowMode(int m=ClipByChildren) { XSetSubwindowMode(display,gc,m); }
	void	SetTile(Pixmap p) { XSetTile(display,gc,p); }
	void	SetTSOrigin(int x=0, int y=0) { XSetTSOrigin(display,gc,x,y); }

	// Drawing functions:
	void	ClearArea(int x, int y, unsigned int w=0, unsigned int h=0, Boolean exp=False) {
			if(is_window) {
				XClearArea(display,(Window)drawable,x,y,w,h,exp); }
			else {	XGCValues	xgcv;
				if(!w) w=32767; if(!h) h=32767;
				XGetGCValues(display,gc,GCForeground|GCBackground,&xgcv);
				SetForeground(xgcv.background);
				XFillRectangle(display,drawable,gc,x,y,w,h);
				SetForeground(xgcv.foreground);
				}
			}
	void	ClearWindow() {
			if(is_window) {
				XClearWindow(display,(Window)drawable); }
			else {	XGCValues	xgcv;
				XGetGCValues(display,gc,GCForeground|GCBackground,&xgcv);
				SetForeground(xgcv.background);
				XFillRectangle(display,drawable,gc,0,0,32767,32767);
				SetForeground(xgcv.foreground);
				}
			}
	void	CopyAreaFrom(Drawable src, int sx, int sy, unsigned int w, unsigned int h,
				int dx=0, int dy=0) {
			XCopyArea(display,src,drawable,gc,sx,sy,w,h,dx,dy); }
	void	CopyAreaTo(Drawable dst, int sx, int sy, unsigned int w, unsigned int h,
				int dx=0, int dy=0) {
			XCopyArea(display,drawable,dst,gc,sx,sy,w,h,dx,dy); }
	void	CopyFrom(Drawable src) {
			XCopyArea(display,src,drawable,gc,0,0,32767,32767,0,0); }
	void	CopyTo(Drawable dst) {
			XCopyArea(display,drawable,dst,gc,0,0,32767,32767,0,0); }
	void	CopyPlaneFrom(Drawable src, int sx, int sy, unsigned int w, unsigned int h,
				int dx=0, int dy=0, unsigned long p=1) {
			XCopyPlane(display,src,drawable,gc,sx,sy,w,h,dx,dy,p); }
	void	CopyPlaneTo(Drawable dst, int sx, int sy, unsigned int w, unsigned int h,
				int dx=0, int dy=0, unsigned long p=1) {
			XCopyPlane(display,drawable,dst,gc,sx,sy,w,h,dx,dy,p); }
	void	DrawArc(int x, int y, unsigned int w, unsigned int h, int a1, int a2) {
			XDrawArc(display,drawable,gc,x,y,w,h,a1,a2); }
	void	DrawArc(int x, int y, unsigned int w, unsigned int h, double a1, double a2) {
			XDrawArc(display,drawable,gc,x,y,w,h,
				 (int)(a1*64.0),(int)(a2*64.0)); }
	void	DrawArcs(XArc *arcs, int num) {
			XDrawArcs(display,drawable,gc,arcs,num); }
	void	DrawImageString(int x, int y, char *str, int len=0) {
			if(!len) len=strlen(str);
			XDrawImageString(display,drawable,gc,x,y,str,len); }
	void	DrawImageString16(int x, int y, XChar2b *str, int len) {
			XDrawImageString16(display,drawable,gc,x,y,str,len); }
	void	DrawLine(int x1, int y1, int x2, int y2) {
			XDrawLine(display,drawable,gc,x1,y1,x2,y2); }
	void	DrawLines(XPoint *pts, int num, int mode=CoordModeOrigin) {
			XDrawLines(display,drawable,gc,pts,num,mode); }
	void	DrawPoint(int x, int y) {
			XDrawPoint(display,drawable,gc,x,y); }
	void	DrawPoints(XPoint *pts, int num, int mode=CoordModeOrigin) {
			XDrawPoints(display,drawable,gc,pts,num,mode); }
	void	DrawRectangle(int x, int y, unsigned int w, unsigned int h) {
			XDrawRectangle(display,drawable,gc,x,y,w,h); }
	void	DrawRectangles(XRectangle *rects, int num) {
			XDrawRectangles(display,drawable,gc,rects,num); }
	void	DrawSegments(XSegment *segs, int num) {
			XDrawSegments(display,drawable,gc,segs,num); }
	void	DrawString(int x, int y, char *str, int len=0) {
			if(!len) len=strlen(str);
			XDrawString(display,drawable,gc,x,y,str,len); }
	void	DrawString16(int x, int y, XChar2b *str, int len) {
			XDrawString16(display,drawable,gc,x,y,str,len); }
	void	DrawText(int x, int y, XTextItem *items, int num) {
			XDrawText(display,drawable,gc,x,y,items,num); }
	void	DrawText16(int x, int y, XTextItem16 *items, int num) {
			XDrawText16(display,drawable,gc,x,y,items,num); }

	void	FillArc(int x, int y, unsigned int w, unsigned int h, int a1, int a2) {
			XFillArc(display,drawable,gc,x,y,w,h,a1,a2); }
	void	FillArc(int x, int y, unsigned int w, unsigned int h, double a1, double a2) {
			XFillArc(display,drawable,gc,x,y,w,h,
				 (int)(a1*64.0),(int)(a2*64.0)); }
	void	FillArcs(XArc *arcs, int num) {
			XFillArcs(display,drawable,gc,arcs,num); }
	void	FillPolygon(XPoint *pts, int num, int shape=Complex, int mode=CoordModeOrigin) {
			XFillPolygon(display,drawable,gc,pts,num,shape,mode); }
	void	FillRectangle(int x, int y, unsigned int w, unsigned int h) {
			XFillRectangle(display,drawable,gc,x,y,w,h); }
	void	FillRectangles(XRectangle *rects, int num) {
			XFillRectangles(display,drawable,gc,rects,num); }

	// XImage functions:
	XImage	*GetImage(int x, int y, unsigned int w, unsigned int h,
				unsigned long pm=0, int f=ZPixmap) {
			if(!pm) pm=AllPlanes;
			return XGetImage(display,drawable,x,y,w,h,pm,f); }
	XImage	*GetSubImage(int x, int y, unsigned int w, unsigned int h,
				unsigned long pm, int f, XImage *dst, int dx=0, int dy=0) {
			if(!pm) pm=AllPlanes;
			return XGetSubImage(display,drawable,x,y,w,h,pm,f,dst,dx,dy); }
	void	PutImage(XImage *xi, int sx, int sy, int dx, int dy,
				unsigned int w, unsigned int h) {
			XPutImage(display,drawable,gc,xi,sx,sy,dx,dy,w,h); }

	// Flushing functions:
	void	Flush() { XFlush(display); }
	void	FlushGC() { XFlushGC(display,gc); }

	// Constructors:
	Xdraw() { init(); }
	Xdraw(Display *d, Drawable dr=0, GC g=NULL) { init();
		set_display(d); set_drawable(dr); set_gc(g); }
	Xdraw(Display *d, GC g) { init(); set_display(d); set_gc(g); }
	Xdraw(Display *d, Drawable dr, unsigned int w, unsigned int h, int depth) {
		init(); set_display(d); CreatePixmap(dr,w,h,depth); CreateGC(); }
	Xdraw(Display *d, unsigned int w, unsigned int h, int depth=0) {
		init(); set_display(d); CreatePixmap(w,h,depth); CreateGC(); }

	~Xdraw() { ; }
	};

#endif // _XDRAW_H_
