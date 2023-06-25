// image.C
//
// Written by Dave Kagels 10/18/94

#include "image/image.h"
#include "image/filetypes.h"
#include "image/disptypes.h"


// Global flag for creating DismMap object if image has a colormap:
int	DISP_USE_MAP=True;


int Image::read_header(char *fname, int t)
{
int	r;
char	*str;

// Type specified:
if(t) {	if(file) { if(t!=file->type()) free_file(); }
	if(!file) set_file(new_file_by_type(t));
	if(!file) return ImageFileErr_BadFileType;
	return file->read_header(fname);
	}

// Type not specified:
str=NULL;
if(file) {
	r=file->read_header(fname);
	if(r!=ImageFileErr_BadFile) return r;
	if(!fname && file->get_filename()) {
		str=(char *)malloc(strlen(file->get_filename())+1);
		file->get_filename(str); fname=str;
		}
	free_file();
	}
// Figure out image type:
set_file(get_new_file_type(fname));
if(str) ::free(str);
if(!file) return ImageFileErr_GetNewTypeFailed;
return file->read_header();
}

int Image::read(char *fname, int t)
{
int	r;
char	*str;

// Type specified:
if(t) {	if(file) { if(t!=file->type()) free_file(); }
	if(!file) set_file(new_file_by_type(t));
	if(!file) return ImageFileErr_BadFileType;
	return file->read_image(fname);
	}

// Type not specified:
str=NULL;
if(file) {
	r=file->read_image(fname);
	if(r!=ImageFileErr_BadFile) return r;
	if(!fname && file->get_filename()) {
		str=(char *)malloc(strlen(file->get_filename())+1);
		file->get_filename(str); fname=str;
		}
	free_file();
	}

// Figure out image type:
set_file(get_new_file_type(fname));
if(str) ::free(str);
if(!file) return ImageFileErr_GetNewTypeFailed;
return file->read_image();
}


int Image::write(char *fname, int t)
{
if(t) {	if(file) { if(t!=file->type()) free_file(); }
	if(!file) {
		if(!t) t=preferred_file_type();
		set_file(new_file_by_type(t));
		}
	}
if(!file) return ImageFileErr_BadFileType;
return file->write_image(fname);
}


int Image::find_file_type(char *fname)
{
free_file();
set_file(get_new_file_type(fname));
if(!file) return ImageFileErr_GetNewTypeFailed;
return FileErr_None;
}


int Image::create_file_type(int t, char *fname)
{
free_file();
if(!t) t=preferred_file_type();
set_file(new_file_by_type(t,fname));
if(!file) return ImageFileErr_BadFileType;
return FileErr_None;
}

#ifndef _NO_IMAGEDISP_
int Image::create_disp(Display *d, Visual *v, Colormap c)
{
XVisualInfo	vinfo,*vis;
int	depth,i;
Image	*map=get_map();

if(map&&DISP_USE_MAP) {
	disp=new DispMap(this,d,v,c);
	return 1;
	}
if(v) {	vinfo.visualid=XVisualIDFromVisual(v);
	vis=XGetVisualInfo(d,VisualIDMask,&vinfo,&i);
	if(!vis) return 0;
	depth=vis->depth;
	XFree(vis);
	if(depth==24) disp=new Disp24(this,d,v,c);
	else	disp=new DispDither(this,d,v,c);
	return 1;
	}
if(!XMatchVisualInfo(d,DefaultScreen(d),24,TrueColor,&vinfo)) {
	disp=new DispDither(this,d,v,c);
	return 1;
	}
v=vinfo.visual;
disp=new Disp24(this,d,v,c);
return 1;
}


int Image::create_disp_noinit(Display *d, Visual *v, Colormap c)
{
XVisualInfo	vinfo,*vis;
int	depth,i;
Image	*map=get_map();

if(map) {
	disp=new DispMap(this);
	set_display(d);
	if(v) set_visual(v);
	if(c) set_colormap(c);
	return 1;
	}
if(v) {	vinfo.visualid=XVisualIDFromVisual(v);
	vis=XGetVisualInfo(d,VisualIDMask,&vinfo,&i);
	if(!vis) return 0;
	depth=vis->depth;
	XFree(vis);
	if(depth==24) disp=new Disp24(this);
	else	disp=new DispDither(this);
	set_display(d);
	set_visual(v);
	if(c) set_colormap(c);
	return 1;
	}
if(!XMatchVisualInfo(d,DefaultScreen(d),24,TrueColor,&vinfo)) {
	disp=new DispDither(this);
	set_display(d);
	if(c) set_colormap(c);
	return 1;
	}
v=vinfo.visual;
disp=new Disp24(this);
set_display(d);
set_visual(v);
if(c) set_colormap(c);
return 1;
}


#endif // _NO_IMAGEDISP_
