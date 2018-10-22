////////////////////////////////////////////////////////////////////////
// Component that implements a magnifying glass tool.  It presents a 
// small zoomed-out portion of the same image.  The window has to be 
// positioned in the way that the cursor is always in the middle of the 
// magnifying glass.  Moving the cursor within the main image will change
// what's displayed inside the magnifying glass.  The main image is further
// referred to as 'big' image, and the magnifying glass image is referred 
// to as 'mag'.
////////////////////////////////////////////////////////////////////////

#include "MagTool.h"
#include "XvicImage.h"
#include "CursorPositionView.h"
#include "CursorDnView.h"
#include "MagInfo.h"
#include "LutToImageWidgetGlue.h"
#include <Xm/PushB.h>
#include <X11/IntrinsicP.h>
#include <math.h>
#include <iostream>
using namespace std;

MagTool::MagTool(Widget parent, const char *name, ImageData *model,
		Widget big_iw,
		Lut *rlut, Lut *glut, Lut *blut, 
		Lut *rpseudo, Lut *gpseudo, Lut *bpseudo,
		MagInfo *magInfo,
		CursorPositionView *posView, CursorDnView *dnView,
		Dimension height, Dimension width) 
	: ImageDisplayView(parent, name, model, height, width)
{
   _big_iw = big_iw;
   _magInfo = magInfo;
   _posView = posView;
   _dnView = dnView;
   _height = height;
   _width = width;

   copyDisplayModeResources();
   copyDataRangeResources();

   XtVaSetValues(_iw,
		XvicNscrollBarDisplayPolicy, XvicNEVER,
		XvicNtrackFloatingCursor, True,
		XvicNconstrainPan, XvicNONE,
		XtNborderWidth, 0,
		NULL);

   setInitZoom();

   // Add callbacks to keep track of the big widget

   XtAddCallback(_big_iw, XvicNvisibleAreaCallback,
		&MagTool::bigWidgetChangeCallback, (XtPointer) this);
   XtAddCallback(_big_iw, XvicNcursorCallback,
		&MagTool::cursorCallback, (XtPointer) this);
   XtAddEventHandler(_iw, PointerMotionMask, False,
		&MagTool::motionHandler, (XtPointer)this);
   XtAddCallback(_iw, XvicNinputCallback, 
		&MagTool::inputCallback, (XtPointer) this);

   _magInfo->printSize(_width, _height);
   _magInfo->manage();

   _lTIWG = new LutToImageWidgetGlue(
                rlut, glut, blut, _iw, True);
   _pseudoLTIWG = new LutToImageWidgetGlue(
		rpseudo, gpseudo, bpseudo, _iw, False);
}

//////////////////////////////////////////////////////////////////////
// ~MagTool(): for the performance, it is very important to remove 
// all the declared callbacks and event handlers.
//////////////////////////////////////////////////////////////////////
MagTool::~MagTool()
{
   delete _lTIWG;
   delete _pseudoLTIWG;
   XtRemoveCallback(_big_iw, XvicNvisibleAreaCallback, 
		&MagTool::bigWidgetChangeCallback, (XtPointer) this);
   XtRemoveCallback(_big_iw, XvicNresizeCallback,
                &MagTool::bigWidgetChangeCallback, (XtPointer) this);
   XtRemoveCallback(_big_iw, XvicNpanCallback,
                &MagTool::bigWidgetChangeCallback, (XtPointer) this);
   XtRemoveCallback(_big_iw, XvicNcursorCallback,
                &MagTool::cursorCallback, (XtPointer) this);
   XtRemoveEventHandler(_iw, PointerMotionMask, False,
                &MagTool::motionHandler, (XtPointer)this);

   _magInfo->unmanage();
}

///////////////////////////////////////////////////////////////////////
// manage():  in addition to the regular staff that superclass does, 
// there are two more things.  First, the shell resize should be disable 
// and then restored when the component is managed.  The reason is that 
// the parent of the component -- a scroll window -- is designed to have 
// only one child.  When the mag glass gets managed, the scroll window 
// sees a new child and resizes.  We clearly don't want this to happen.
// The second thing that needs to be done is setting save_under flag.
// Save_under dramatically improves the performance because uncovering 
// the area in the big widget does not cause an expose event.
///////////////////////////////////////////////////////////////////////
void MagTool::manage()
{
   // Find the shell widget
   Widget shell = _w;
   do {
	shell = XtParent(shell);
   } while (shell && !XtIsShell(shell));

   // Save shell resize value, then disable resize
   Boolean shell_resize;
   XtVaGetValues(shell, XtNallowShellResize, &shell_resize, NULL);
   XtVaSetValues(shell, XtNallowShellResize, FALSE, NULL);

   ImageDisplayView::manage();

   // Set the save-under parameter to true to disable expose events on the 
   // big image
   Display *display = XtDisplay(_w);
   Window window = XtWindow(_w);
   XSetWindowAttributes setWinAttr;
   unsigned long valuemask;
   valuemask = CWSaveUnder;
   setWinAttr.save_under = True;
   XChangeWindowAttributes(display, window, valuemask, &setWinAttr);

   // Restore resize
   XtVaSetValues(shell, XtNallowShellResize, shell_resize, NULL);
}

///////////////////////////////////////////////////////////////////////
// setInitZoom(): set mag's zoom twice the zoom of the big image.
///////////////////////////////////////////////////////////////////////
void MagTool::setInitZoom()
{
   int xin, xout, yin, yout;
   XtVaGetValues(_big_iw,
        XvicNxZoomIn, &xin,
        XvicNxZoomOut, &xout,
        XvicNyZoomIn, &yin,
        XvicNyZoomOut, &yout,
        NULL);

   _init_xin = 2*xin; 
   _init_yin = 2*yin;
   _init_xout = xout;
   _init_yout = yout;

   ZoomFactor zoom(_init_xin, _init_yin, _init_xout, _init_yout);
   setUserZoom(zoom);

   if (xout == 0 || yout == 0) return; 	// to avoid div by 0
   _magInfo->printRatio((float)(2*xin)/(float)(1*xout), 
			(float)(2*yin)/(float)(1*yout));
}

////////////////////////////////////////////////////////////////////////
// Callback from the big widget to let us know something changed.
////////////////////////////////////////////////////////////////////////

void MagTool::bigWidgetChangeCallback(Widget, XtPointer clientData,
					      XtPointer callData)
{
   MagTool *obj = (MagTool *)clientData;

   obj->bigWidgetChange(callData);
}

////////////////////////////////////////////////////////////////////////
// Something changed in the big widget.  Deal with it.
////////////////////////////////////////////////////////////////////////

void MagTool::bigWidgetChange(XtPointer callData)
{
   XvicImageCallbackStruct *cb = (XvicImageCallbackStruct *) callData;

   switch (cb->reason) {

      case XvicCR_VISIBLE_AREA:
         if (cb->flags & XvicZOOM_CHANGED) {
	    setInitZoom();
         }
         if (cb->flags & XvicDITHER_CHANGED) {
            copyDisplayModeResources();
         }
         if (cb->flags & XvicRANGE_CHANGED) {
            copyDataRangeResources();
         }
         // MODE_CHANGED is ignored because we must be notified of that
         // (color/bw and data type) via the model.

         break;
   }
}

////////////////////////////////////////////////////////////////////////
// Callback from mag widget when the user takes an action.
////////////////////////////////////////////////////////////////////////
void MagTool::inputCallback(Widget, XtPointer clientData,
				XtPointer callData)
{
   MagTool *obj = (MagTool *)clientData;

   obj->input(callData);
}

void MagTool::input(XtPointer callData)
{
   XvicImageCallbackStruct *cb = (XvicImageCallbackStruct *) callData;

   if (cb->reason != XvicCR_INPUT)
      return;				// oops

   if (cb->input_num_params != 2)
      return;				// oops

   // Increase or decrease mag size by 10%
   if (strcmp(cb->input_params[0], "mag_size") == 0) {
      Dimension newWidth, newHeight;
      if (strcmp(cb->input_params[1], "increase") == 0) {
	 newWidth = (Dimension) (_width * 1.1);
	 newHeight = (Dimension) (_height * 1.1);
	 if (newWidth == _width) 
	    newWidth++;
	 if (newHeight == _height)
	    newHeight++;
	 setSize(newWidth, newHeight);
      }
      else if (strcmp(cb->input_params[1], "decrease") == 0) {
	 newWidth = (Dimension) (_width * 0.9);
	 newHeight = (Dimension) (_height * 0.9);
	 setSize(newWidth, newHeight);
      }
      _magInfo->printSize(_width, _height);
   }

   // Increase or decrease magnification by 1
   if (strcmp(cb->input_params[0], "mag_ratio") == 0) {
      ZoomFactor zf = getImageZoom();
      unmanage();
      if (strcmp(cb->input_params[1], "increase") == 0) {
	 zf.setX(zf.getXIn() + 1, zf.getXOut());
	 zf.setY(zf.getYIn() + 1, zf.getYOut());
      }
      else if (strcmp(cb->input_params[1], "decrease") == 0) {
	 if (zf.getXIn() <= 1 || zf.getYIn() <= 1 
		|| zf.getXIn() < zf.getXOut() 
		|| zf.getYIn() < zf.getYOut()) {
	    if (_init_xin < _init_xout) {
	       zf.setX(_init_xin, _init_xout);
	    }
	    if (_init_yin < _init_yout) {
	       zf.setY(_init_yin, _init_yout);
	    }
	 }
	 else {
	    zf.setX(zf.getXIn() - 1, zf.getXOut());
	    zf.setY(zf.getYIn() - 1, zf.getYOut());
	 }
      }
      setUserZoom(zf);
      manage();
      _magInfo->printRatio((float)zf.getXIn()/(float)zf.getXOut(), 
			   (float)zf.getYIn()/(float)zf.getYOut());
   }
}

////////////////////////////////////////////////////////////////////////
// Callback from big widget when the user takes an action.
////////////////////////////////////////////////////////////////////////

void MagTool::cursorCallback(Widget, XtPointer clientData,
                                XtPointer callData)
{
   MagTool *obj = (MagTool *)clientData;

   obj->cursor(callData);
}

void MagTool::cursor(XtPointer callData)
{
   XvicImageCallbackStruct *cb = (XvicImageCallbackStruct *) callData;

   if (cb->reason != XvicCR_CURSOR)
      return;				// oops

   ZoomFactor zf = getImageZoom();
   int xin = zf.getXIn();
   int xout = zf.getXOut();
   int yin = zf.getYIn();
   int yout = zf.getYOut();
   XtVaSetValues(_iw, 
	XvicNxPan, cb->x-(int)(_width/(2.0*((float)xin/(float)xout))),
	XvicNyPan, cb->y-(int)(_height/(2.0*((float)yin/(float)yout))),
	NULL);

   int x1, y1, x2, y2;
   XvicImageDisplayBounds(_big_iw, &x1, &y1, &x2, &y2);

   Dimension width, height, bwidth, bheight;
   XtVaGetValues(_big_iw, 
	XvicNviewHeight, &height,
	XvicNviewWidth, &width,
	NULL);
   XtVaGetValues(_iw,
        XtNwidth, &bwidth, 
        XtNheight, &bheight, 
        NULL);

   XtMoveWidget(_w,
	(int)((float)(cb->x - x1) / (float)(x2 - x1) * (float)width) - bwidth/2,
	(int)((float)(cb->y - y1) / (float)(y2 - y1) * (float)height) - bheight/2);

   if (isOnImage(cb->x, cb->y)) {
      _posView->cursorMoved(cb);
      _dnView->cursorMoved(cb);
   }
   else {
      _posView->blankSubViews();
      _dnView->blankSubViews();
   }
}

////////////////////////////////////////////////////////////////////////
// Callback from mag widget when the user takes an action.
////////////////////////////////////////////////////////////////////////
void MagTool::motionHandler(Widget, XtPointer clientData,
                                    XEvent *event, Boolean *)
{
   XMotionEvent *mev = (XMotionEvent *)event;
   MagTool *obj = (MagTool *)clientData;

   obj->motion(mev);
}

void MagTool::motion(XMotionEvent *event)
{
   // Root coords of the callback
   Position root_x, root_y;
   root_x = event->x_root;
   root_y = event->y_root;

   // Root coords of the big image 
   Position big_root_x, big_root_y;
   XtTranslateCoords(_big_iw, 0, 0, &big_root_x, &big_root_y);

   // Pixel coordinates of the cursor relative to the big image window
   Position xp = root_x - big_root_x;
   Position yp = root_y - big_root_y;

   int x1, y1, x2, y2;
   XvicImageDisplayBounds(_big_iw, &x1, &y1, &x2, &y2);

   Dimension bw, bh, bwidth, bheight;
   XtVaGetValues(_big_iw,
        XvicNviewHeight, &bh,
        XvicNviewWidth, &bw,
	NULL);
   XtVaGetValues(_iw,
	XtNwidth, &bwidth, 
	XtNheight, &bheight, 
        NULL);

   // Image coordinates of the cursor relative to the big image window
   int icx = (int)((float)xp * (float)(x2 - x1 + 1) / (float)bw) + x1;
   int icy = (int)((float)yp * (float)(y2 - y1 + 1) / (float)bh) + y1;

   ZoomFactor zf = getImageZoom();
   int xin = zf.getXIn(); 
   int xout = zf.getXOut();
   int yin = zf.getYIn();
   int yout = zf.getYOut();
   XtVaSetValues(_iw,
        XvicNxPan, icx-(int)(_width/(2.0*((float)xin/(float)xout))),
        XvicNyPan, icy-(int)(_height/(2.0*((float)yin/(float)yout))),
        NULL);

   XtMoveWidget(_w,
        xp - bwidth/2,
        yp - bheight/2);

   // Pass 'origin 1' image coordinates to position view components
   if (isOnImage(icx, icy)) {
      _posView->cursorMoved(icx+1, icy+1);
      _dnView->cursorMoved(icx, icy);
   }
   else {
      _posView->blankSubViews();
      _dnView->blankSubViews();
   }
}

////////////////////////////////////////////////////////////////////////
// Copy the display-related resources (those that could affect what colormap
// is used) from the big widget to here, to prevent flashing.
////////////////////////////////////////////////////////////////////////
void MagTool::copyDisplayModeResources()
{
   unsigned char bw_dither, color_dither, visual_type;
   unsigned char stretch_policy, colormap_policy, lut_type;
   int gray_levels, red_levels, green_levels, blue_levels;

   XtVaGetValues(_big_iw,
		XvicNvisualType, &visual_type,
		XvicNbwDither, &bw_dither,
                XvicNcolorDither, &color_dither,
                XvicNstretchPolicy, &stretch_policy,
                XvicNcolormapPolicy, &colormap_policy,
                XvicNlutType, &lut_type,
                XvicNgrayLevels, &gray_levels,
                XvicNredLevels, &red_levels,
                XvicNgreenLevels, &green_levels,
                XvicNblueLevels, &blue_levels,
                NULL);

   XtVaSetValues(_iw,
                XvicNvisualType, visual_type,
                XvicNbwDither, bw_dither,
                XvicNcolorDither, color_dither,
                XvicNstretchPolicy, stretch_policy,
                XvicNcolormapPolicy, colormap_policy,
                XvicNlutType, lut_type,
                XvicNgrayLevels, gray_levels,
                XvicNredLevels, red_levels,
                XvicNgreenLevels, green_levels,
                XvicNblueLevels, blue_levels,
                NULL);
}

////////////////////////////////////////////////////////////////////////
// Copy the data-type resources from the big widget to here, so the
// display will be visible.
////////////////////////////////////////////////////////////////////////
void MagTool::copyDataRangeResources()
{
   double raw_data_min, raw_data_max;
   int scaled_data_max, output_data_max;

   XtVaGetValues(_big_iw,
		XvicNrawDataMin, &raw_data_min,
		XvicNrawDataMax, &raw_data_max,
		XvicNscaledDataMax, &scaled_data_max,
		XvicNoutputDataMax, &output_data_max,
                NULL);

   XtVaSetValues(_iw,
		XvicNrawDataMin, XvicDOUBLE_ARG(raw_data_min),
		XvicNrawDataMax, XvicDOUBLE_ARG(raw_data_max),
		XvicNscaledDataMax, scaled_data_max,
		XvicNoutputDataMax, output_data_max,
                NULL);
}

//////////////////////////////////////////////////////////////////////
// isOnImage: return True if image callback was generated over the 
// image area, return False otherwise.
//////////////////////////////////////////////////////////////////////
Boolean MagTool::isOnImage(XvicImageCallbackStruct * cb)
{
   return (cb->on_screen &&
		isOnImage(cb->x, cb->y));
}

//////////////////////////////////////////////////////////////////////
// isOnImage: return True if x and y are within the image area.
// x and y are the image coordinates.  Note, origin is (0,0).
//////////////////////////////////////////////////////////////////////
Boolean MagTool::isOnImage(int x, int y)
{
   return (x >= 0 &&
		x < _model->getNumbSamples() &&
		y >= 0 &&
		y < _model->getNumbLines());
}

//////////////////////////////////////////////////////////////////////
// setSize: set size of the mag in pixels.
//////////////////////////////////////////////////////////////////////
void MagTool::setSize(Dimension width, Dimension height)
{
   _width = width;
   _height = height;

   unmanage();

   setViewSize(width, height, False);

   manage();

   _magInfo->printSize(width, height);
}
