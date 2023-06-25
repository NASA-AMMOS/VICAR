// datamod.C
//
// Written by Dave Kagels 1/12/95

#include "image/datamod.h"
#include <math.h>


// ****************************** transData *******************************


void transData::init()
{
img=NULL;
lev_off=0.0;

tcx=0.0; tcy=0.0;
txx=1.0; txy=0.0;
tyx=0.0; tyy=1.0;

t2i();
}


void transData::t2i()
{
double	temp,temp1,l1,l2;

temp=txx*tyy-txy*tyx;
if(!temp) temp=1.0;	// error condition

ixx=tyy/temp;
ixy=-txy/temp;
iyx=-tyx/temp;
iyy=txx/temp;
icx=-tcx*ixx-tcy*iyx;
icy=-tcx*ixy-tcy*iyy;

temp=txx*txx+txy*txy;
temp1=tyx*tyx+tyy*tyy;
if(temp1>temp) { l1=sqrt(temp1); l2=sqrt(temp); }
else {	l1=sqrt(temp); l2=sqrt(temp1); }
if(l1==0.0) { lev_off=0.0; return; }
temp=fabs(txx*tyx+txy*tyy)/l1;
temp=l2*l2-temp*temp; if(temp<=0.0) { lev_off=0.0; return; }
temp=sqrt(temp);
lev_off=0.0-log(temp)*1.442695041;
}


void transData::i2t()
{

}


int transData::allocate(int x, int y, int)
{
xres=x; yres=y;
return 1;
}


void transData::free()
{
xres=0; yres=0; bands=0;
if(clean && img) delete img;
img=NULL;
}


int transData::add_band()
{
ImageData	*dat;
int		r=0;

if(img) dat=img->get_data();
if(dat)	r=dat->add_band();
if(img) bands=img->get_bands();
return r;
}


int transData::remove_band(int b)
{
ImageData	*dat;
int		r=0;

if(img) dat=img->get_data();
if(dat)	r=dat->remove_band(b);
if(img) bands=img->get_bands();
return r;
}


int transData::clear(int b)
{
if(img)	return img->clear(b);
else return 0;
}


void transData::get_color(int x, int y, uchar *r, uchar *g, uchar *b, uchar *a)
{
if(!img) { *r=0; *g=0; *b=0; *a=0; return; }

img->get_color((int)(icx+x*ixx+y*iyx),(int)(icy+x*ixy+y*iyy),r,g,b,a);
}


void transData::iget_color(double x, double y, uchar *r, uchar *g, uchar *b, uchar *a)
{
if(!img) { *r=0; *g=0; *b=0; *a=0; return; }

(img->get_data())->iget_color(icx+x*ixx+y*iyx,icy+x*ixy+y*iyy,r,g,b,a);
}


void transData::set_color(int x, int y, uchar r, uchar g, uchar b, uchar a)
{
if(img) img->set_color((int)(icx+x*ixx+y*iyx),(int)(icy+x*ixy+y*iyy),r,g,b,a);
}


uchar transData::get_alpha(int x, int y)
{
return	img->get_alpha((int)(icx+x*ixx+y*iyx),(int)(icy+x*ixy+y*iyy));
}


uchar transData::iget_alpha(double x, double y)
{
return	(img->get_data())->iget_alpha(icx+x*ixx+y*iyx,icy+x*ixy+y*iyy);
}


void transData::set_alpha(int x, int y, uchar a)
{
img->set_alpha((int)(icx+x*ixx+y*iyx),(int)(icy+x*ixy+y*iyy),a);
}

void transData::set_wrap_state( Image_Wrap_State_Type st)
{
	ImageData *id = img->get_data();
	if (img != NULL) {
		(img->get_data())->flgWrapState=st;
	}
	flgWrapState = st;
}

Image_Wrap_State_Type  transData::get_wrap_state()
{
	ImageData *id = img->get_data();
	if (img != NULL) {
		return (img->get_data())->flgWrapState;
	}
	else {
		return flgWrapState;
	}
	
}


// ****************************** compData *******************************

// crossmap tests two images to see if they are both geoimages
// if so, it maps xy in image1 to lat/long to xy in image2 returning
// the new xy in the old xy location
int  compData::crossmap(Image *i1, Image *i2, double *x, double *y)
{
	double	lat, lng;
	int	status = 0;
	if(i1->image_class_type() == GEOIMAGE) {
		if(((GeoImage *)i1)->get_geo_data()) {
			lat = ((GeoImage *)i1)->xytolat(*x, *y);
			lng = ((GeoImage *)i1)->xytolong(*x, *y);
			if(i2->image_class_type() == GEOIMAGE) {
				if(((GeoImage *)i2)->get_geo_data()) {
					*x = ((GeoImage *)i2)->lltox(lat, lng);
					*y = ((GeoImage *)i2)->lltoy(lat, lng);
					status = 1;
				}
			}
		}
	}
	return(status);
}

compData::compData(Image **i)
{
Image **ip;
init();
for(ip=i;*ip;ip++) add_image(*ip);
}


compData::compData(Image **i, int n)
{
int a;
init();
for(a=0;a<n;a++) add_image(i[a]);
}


compData::~compData()
{
free();
}


void compData::init()
{
img=NULL;
num=0;
}


int compData::allocate(int x, int y, int b)
{
xres=x; yres=y; bands=b;
return 1;
}


void compData::free()
{
int	i;

if(clean&&img)
	for(i=0;i<num;i++) delete img[i];
if(img) ::free((char *)img);
img=NULL;
xres=0; yres=0; bands=0;
num=0;
}


void compData::add_image(Image *i, int p)
{
int	n,x,y;

if(p==-1) p=num;
if(p<0 || p>num || !i) return;
num++;
img=(Image **)realloc(img,num*sizeof(Image *));
for(n=num-1;n>p;n--) img[n]=img[n-1];
img[p]=i;
if(p == 0) {  // set global xy res to res of first image
   i->get_res(&x,&y);
   if(x>xres) xres=x;
   if(y>yres) yres=y;
}
}


void compData::remove_image(int p)
{
int	n;

if(p==-1) p=num-1;
if(p<0 || p>num) return;
num--;
if(!num) { ::free((char *)img); img=NULL; return; }
for(n=p;n<num;n++) img[n]=img[n+1];
img=(Image **)realloc(img,num*sizeof(Image *));
}


void compData::set_level(double l)
{
int	i;

if(!img) return;

for(i=0;i<num;i++) img[i]->set_level(l);
}


void compData::get_color(int x, int y, uchar *r, uchar *g, uchar *b, uchar *a)
{
double	rt,gt,bt,at,a1,a2;
uchar	cr,cg,cb,ca;
int	i;

if(!img) { *r=0; *g=0; *b=0; if(a) *a=0; return; }

double dx, dy;

rt=0.0; gt=0.0; bt=0.0; at=1.0;
for(i=0;i<num;i++) {
	dx = (double)x; dy = (double)y;
	if(i) {
		crossmap(img[0], img[i], &dx, &dy);
	}
	img[i]->get_color((int)dx,(int)dy,&cr,&cg,&cb,&ca);
	a1=ca*0.0039215686274;
	a2=1.0-a1;
	rt=rt*a2+cr*a1; gt=gt*a2+cg*a1; bt=bt*a2+cb*a1; at*=a2;
	}
if(at<0.000001) {
	*r=(uchar)(rt+0.5);
	*g=(uchar)(gt+0.5);
	*b=(uchar)(bt+0.5);
	if(a) *a=255;
	return;
	}
at=1.0-at;
if(at==0.0) { *r=0; *g=0; *b=0; if(a) *a=0; return; }
a1=1.0/at;
*r=(uchar)(rt*a1+0.5);
*g=(uchar)(gt*a1+0.5);
*b=(uchar)(bt*a1+0.5);
if(a) *a=(uchar)(at*255.0+0.5);
}


void compData::iget_color(double x, double y, uchar *r, uchar *g, uchar *b, uchar *a)
{
double	rt,gt,bt,at,a1,a2;
uchar	cr,cg,cb,ca;
int	i;
double dx, dy;

if(!img) { *r=0; *g=0; *b=0; if(a) *a=0; return; }

rt=0.0; gt=0.0; bt=0.0; at=1.0;
for(i=0;i<num;i++) {
	dx = (double)x; dy = (double)y;
	if(i) {
		crossmap(img[0], img[i], &dx, &dy);
	}
	(img[i]->get_data())->iget_color(dx,dy,&cr,&cg,&cb,&ca);
	a1=ca*0.0039215686274;
	a2=1.0-a1;
	rt=rt*a2+cr*a1; gt=gt*a2+cg*a1; bt=bt*a2+cb*a1; at*=a2;
	}
if(at<0.000001) {
	*r=(uchar)(rt+0.5);
	*g=(uchar)(gt+0.5);
	*b=(uchar)(bt+0.5);
	if(a) *a=255;
	return;
	}
at=1.0-at;
if(at==0.0) { *r=0; *g=0; *b=0; if(a) *a=0; return; }
a1=1.0/at;
*r=(uchar)(rt*a1+0.5);
*g=(uchar)(gt*a1+0.5);
*b=(uchar)(bt*a1+0.5);
if(a) *a=(uchar)(at*255.0+0.5);
}


uchar compData::get_alpha(int x, int y)
{
int	i;
double	a;
double dx, dy;

a=1.0;
for(i=0;i<num;i++) {
	dx = (double)x; dy = (double)y;
	if(i) {
		crossmap(img[0], img[i], &dx, &dy);
	}
	a*=1.0-(img[i]->get_data())->get_alpha((int)dx,(int)dy)*0.0039215686274;
}
return (uchar)(255.0*(1.0-a)+0.5);
}


uchar compData::iget_alpha(double x, double y)
{
int	i;
double	a;
double dx, dy;

a=1.0;
for(i=0;i<num;i++) {
	dx = (double)x; dy = (double)y;
	if(i) {
		crossmap(img[0], img[i], &dx, &dy);
	}
	a*=1.0-(img[i]->get_data())->iget_alpha((int)dx,(int)dy)*0.0039215686274;
}
return (uchar)(255.0*(1.0-a)+0.5);
}


int compData::get_bit(int x, int y, int b)
{
int	i;
double	a1,a2,at,t;
double dx, dy;

t=0.0; at=1.0;
for(i=0;i<num;i++) {
	dx = (double)x; dy = (double)y;
	if(i) {
		crossmap(img[0], img[i], &dx, &dy);
	}
	a1=(img[i]->get_data())->get_alpha((int)dx,(int)dy)*0.0039215686274; a2=1.0-a1;
	t=t*a2+(img[i]->get_data())->get_bit((int)dx,(int)dy,b)*a1; at*=a2;
	}
if(at<0.000001) return (int)(t+0.5);
if(at==1.0) return 0;
return (int)(t/(1.0-at)+0.5);
}


int compData::iget_bit(double x, double y, int b)
{
int	i;
double	a1,a2,at,t;
double dx, dy;

t=0.0; at=1.0;
for(i=0;i<num;i++) {
	dx = (double)x; dy = (double)y;
	if(i) {
		crossmap(img[0], img[i], &dx, &dy);
	}
	a1=(img[i]->get_data())->iget_alpha((int)dx,(int)dy)*0.0039215686274; a2=1.0-a1;
	t=t*a2+(img[i]->get_data())->iget_bit((int)dx,(int)dy,b)*a1; at*=a2;
	}
if(at<0.000001) return (int)(t+0.5);
if(at==1.0) return 0;
return (int)(t/(1.0-at)+0.5);
}


char compData::get_char(int x, int y, int b)
{
int	i;
double	a1,a2,at,t;
double dx, dy;

t=0.0; at=1.0;
for(i=0;i<num;i++) {
	dx = (double)x; dy = (double)y;
	if(i) {
		crossmap(img[0], img[i], &dx, &dy);
	}
	a1=(img[i]->get_data())->get_alpha((int)dx,(int)dy)*0.0039215686274; a2=1.0-a1;
	t=t*a2+(img[i]->get_data())->get_char((int)dx,(int)dy,b)*a1; at*=a2;
	}
if(at<0.000001) return (char)t;
if(at==1.0) return 0;
return (char)(t/(1.0-at));
}


char compData::iget_char(double x, double y, int b)
{
int	i;
double	a1,a2,at,t;
double dx, dy;

t=0.0; at=1.0;
for(i=0;i<num;i++) {
	dx = (double)x; dy = (double)y;
	if(i) {
		crossmap(img[0], img[i], &dx, &dy);
	}
	a1=(img[i]->get_data())->iget_alpha((int)dx,(int)dy)*0.0039215686274; a2=1.0-a1;
	t=t*a2+(img[i]->get_data())->iget_char((int)dx,(int)dy,b)*a1; at*=a2;
	}
if(at<0.000001) return (char)t;
if(at==1.0) return 0;
return (char)(t/(1.0-at));
}


uchar compData::get_uchar(int x, int y, int b)
{
int	i;
double	a1,a2,at,t;
double dx, dy;

t=0.0; at=1.0;
for(i=0;i<num;i++) {
	dx = (double)x; dy = (double)y;
	if(i) {
		crossmap(img[0], img[i], &dx, &dy);
	}
	a1=(img[i]->get_data())->get_alpha((int)dx,(int)dy)*0.0039215686274; a2=1.0-a1;
	t=t*a2+(img[i]->get_data())->get_uchar((int)dx,(int)dy,b)*a1; at*=a2;
	}
if(at<0.000001) return (uchar)(t+0.5);
if(at==1.0) return 0;
return (uchar)(t/(1.0-at)+0.5);
}


uchar compData::iget_uchar(double x, double y, int b)
{
int	i;
double	a1,a2,at,t;
double dx, dy;

t=0.0; at=1.0;
for(i=0;i<num;i++) {
	dx = (double)x; dy = (double)y;
	if(i) {
		crossmap(img[0], img[i], &dx, &dy);
	}
	a1=(img[i]->get_data())->iget_alpha((int)dx,(int)dy)*0.0039215686274; a2=1.0-a1;
	t=t*a2+(img[i]->get_data())->iget_uchar((int)dx,(int)dy,b)*a1; at*=a2;
	}
if(at<0.000001) return (uchar)(t+0.5);
if(at==1.0) return 0;
return (uchar)(t/(1.0-at)+0.5);
}


short compData::get_short(int x, int y, int b)
{
int	i;
double	a1,a2,at,t;
double dx, dy;

t=0.0; at=1.0;
for(i=0;i<num;i++) {
	dx = (double)x; dy = (double)y;
	if(i) {
		crossmap(img[0], img[i], &dx, &dy);
	}
	a1=(img[i]->get_data())->get_alpha((int)dx,(int)dy)*0.0039215686274; a2=1.0-a1;
	t=t*a2+(img[i]->get_data())->get_short((int)dx,(int)dy,b)*a1; at*=a2;
	}
if(at<0.000001) return (short)t;
if(at==1.0) return 0;
return (short)(t/(1.0-at));
}


short compData::iget_short(double x, double y, int b)
{
int	i;
double	a1,a2,at,t;
double dx, dy;

t=0.0; at=1.0;
for(i=0;i<num;i++) {
	dx = (double)x; dy = (double)y;
	if(i) {
		crossmap(img[0], img[i], &dx, &dy);
	}
	a1=(img[i]->get_data())->iget_alpha((int)dx,(int)dy)*0.0039215686274; a2=1.0-a1;
	t=t*a2+(img[i]->get_data())->iget_short((int)dx,(int)dy,b)*a1; at*=a2;
	}
if(at<0.000001) return (short)t;
if(at==1.0) return 0;
return (short)(t/(1.0-at));
}


ushort compData::get_ushort(int x, int y, int b)
{
int	i;
double	a1,a2,at,t;
double dx, dy;

t=0.0; at=1.0;
for(i=0;i<num;i++) {
	dx = (double)x; dy = (double)y;
	if(i) {
		crossmap(img[0], img[i], &dx, &dy);
	}
	a1=(img[i]->get_data())->get_alpha((int)dx,(int)dy)*0.0039215686274; a2=1.0-a1;
	t=t*a2+(img[i]->get_data())->get_ushort((int)dx,(int)dy,b)*a1; at*=a2;
	}
if(at<0.000001) return (ushort)(t+0.5);
if(at==1.0) return 0;
return (ushort)(t/(1.0-at)+0.5);
}


ushort compData::iget_ushort(double x, double y, int b)
{
int	i;
double	a1,a2,at,t;
double dx, dy;

t=0.0; at=1.0;
for(i=0;i<num;i++) {
	dx = (double)x; dy = (double)y;
	if(i) {
		crossmap(img[0], img[i], &dx, &dy);
	}
	a1=(img[i]->get_data())->iget_alpha((int)dx,(int)dy)*0.0039215686274; a2=1.0-a1;
	t=t*a2+(img[i]->get_data())->iget_ushort((int)dx,(int)dy,b)*a1; at*=a2;
	}
if(at<0.000001) return (ushort)(t+0.5);
if(at==1.0) return 0;
return (ushort)(t/(1.0-at)+0.5);
}


long compData::get_long(int x, int y, int b)
{
int	i;
double	a1,a2,at,t;
double dx, dy;

t=0.0; at=1.0;
for(i=0;i<num;i++) {
	dx = (double)x; dy = (double)y;
	if(i) {
		crossmap(img[0], img[i], &dx, &dy);
	}
	a1=(img[i]->get_data())->get_alpha((int)dx,(int)dy)*0.0039215686274; a2=1.0-a1;
	t=t*a2+(img[i]->get_data())->get_long((int)dx,(int)dy,b)*a1; at*=a2;
	}
if(at<0.000001) return (long)t;
if(at==1.0) return 0;
return (long)(t/(1.0-at));
}


long compData::iget_long(double x, double y, int b)
{
int	i;
double	a1,a2,at,t;
double dx, dy;

t=0.0; at=1.0;
for(i=0;i<num;i++) {
	dx = (double)x; dy = (double)y;
	if(i) {
		crossmap(img[0], img[i], &dx, &dy);
	}
	a1=(img[i]->get_data())->iget_alpha((int)dx,(int)dy)*0.0039215686274; a2=1.0-a1;
	t=t*a2+(img[i]->get_data())->iget_long((int)dx,(int)dy,b)*a1; at*=a2;
	}
if(at<0.000001) return (long)t;
if(at==1.0) return 0;
return (long)(t/(1.0-at));
}


ulong compData::get_ulong(int x, int y, int b)
{
int	i;
double	a1,a2,at,t;
double dx, dy;

t=0.0; at=1.0;
for(i=0;i<num;i++) {
	dx = (double)x; dy = (double)y;
	if(i) {
		crossmap(img[0], img[i], &dx, &dy);
	}
	a1=(img[i]->get_data())->get_alpha((int)dx,(int)dy)*0.0039215686274; a2=1.0-a1;
	t=t*a2+(img[i]->get_data())->get_ulong((int)dx,(int)dy,b)*a1; at*=a2;
	}
if(at<0.000001) return (ulong)(t+0.5);
if(at==1.0) return 0;
return (ulong)(t/(1.0-at)+0.5);
}


ulong compData::iget_ulong(double x, double y, int b)
{
int	i;
double	a1,a2,at,t;
double dx, dy;

t=0.0; at=1.0;
for(i=0;i<num;i++) {
	dx = (double)x; dy = (double)y;
	if(i) {
		crossmap(img[0], img[i], &dx, &dy);
	}
	a1=(img[i]->get_data())->iget_alpha((int)dx,(int)dy)*0.0039215686274; a2=1.0-a1;
	t=t*a2+(img[i]->get_data())->iget_ulong((int)dx,(int)dy,b)*a1; at*=a2;
	}
if(at<0.000001) return (ulong)(t+0.5);
if(at==1.0) return 0;
return (ulong)(t/(1.0-at)+0.5);
}


float compData::get_float(int x, int y, int b)
{
int	i;
double	a1,a2,at,t;
double dx, dy;

t=0.0; at=1.0;
for(i=0;i<num;i++) {
	dx = (double)x; dy = (double)y;
	if(i) {
		crossmap(img[0], img[i], &dx, &dy);
	}
	a1=(img[i]->get_data())->get_alpha((int)dx,(int)dy)*0.0039215686274; a2=1.0-a1;
	t=t*a2+(img[i]->get_data())->get_float((int)dx,(int)dy,b)*a1; at*=a2;
	}
if(at<0.000001) return (float)t;
if(at==1.0) return 0.0;
return (float)(t/(1.0-at));
}


float compData::iget_float(double x, double y, int b)
{
int	i;
double	a1,a2,at,t;
double dx, dy;

t=0.0; at=1.0;
for(i=0;i<num;i++) {
	dx = (double)x; dy = (double)y;
	if(i) {
		crossmap(img[0], img[i], &dx, &dy);
	}
	a1=(img[i]->get_data())->iget_alpha((int)dx,(int)dy)*0.0039215686274; a2=1.0-a1;
	t=t*a2+(img[i]->get_data())->iget_float((int)dx,(int)dy,b)*a1; at*=a2;
	}
if(at<0.000001) return (float)t;
if(at==1.0) return 0.0;
return (float)(t/(1.0-at));
}


double compData::get_double(int x, int y, int b)
{
int	i;
double	a1,a2,at,t;
double dx, dy;

t=0.0; at=1.0;
for(i=0;i<num;i++) {
	dx = (double)x; dy = (double)y;
	if(i) {
		crossmap(img[0], img[i], &dx, &dy);
	}
	a1=(img[i]->get_data())->get_alpha((int)dx,(int)dy)*0.0039215686274; a2=1.0-a1;
	t=t*a2+(img[i]->get_data())->get_double((int)dx,(int)dy,b)*a1; at*=a2;
	}
if(at<0.000001) return t;
if(at==1.0) return 0.0;
return t/(1.0-at);
}


double compData::iget_double(double x, double y, int b)
{
int	i;
double	a1,a2,at,t;
double dx, dy;

t=0.0; at=1.0;
for(i=0;i<num;i++) {
	dx = (double)x; dy = (double)y;
	if(i) {
		crossmap(img[0], img[i], &dx, &dy);
	}
	a1=(img[i]->get_data())->iget_alpha((int)dx,(int)dy)*0.0039215686274; a2=1.0-a1;
	t=t*a2+(img[i]->get_data())->iget_double((int)dx,(int)dy,b)*a1; at*=a2;
	}
if(at<0.000001) return t;
if(at==1.0) return 0.0;
return t/(1.0-at);
}


COMPLEX_TYPE compData::get_complex(int x, int y, int b)
{
int	i;
double	a1,a2,at;
COMPLEX_TYPE	t;
double dx, dy;

t=complex_zero; at=1.0;
for(i=0;i<num;i++) {
	dx = (double)x; dy = (double)y;
	if(i) {
		crossmap(img[0], img[i], &dx, &dy);
	}
	a1=(img[i]->get_data())->get_alpha((int)dx,(int)dy)*0.0039215686274; a2=1.0-a1;
	t=t*a2+(img[i]->get_data())->get_complex((int)dx,(int)dy,b)*a1; at*=a2;
	}
if(at<0.000001) return t;
if(at==1.0) return complex_zero;
return t/(1.0-at);
}


COMPLEX_TYPE compData::iget_complex(double x, double y, int b)
{
int	i;
double	a1,a2,at;
COMPLEX_TYPE	t;
double dx, dy;

t=complex_zero; at=1.0;
for(i=0;i<num;i++) {
	dx = (double)x; dy = (double)y;
	if(i) {
		crossmap(img[0], img[i], &dx, &dy);
	}
	a1=(img[i]->get_data())->iget_alpha((int)dx,(int)dy)*0.0039215686274; a2=1.0-a1;
	t=t*a2+(img[i]->get_data())->iget_complex((int)dx,(int)dy,b)*a1; at*=a2;
	}
if(at<0.000001) return t;
if(at==1.0) return complex_zero;
return t/(1.0-at);
}


// ****************************** multiBand *******************************


multiBand::multiBand(Image **i)
{
Image **ip;
init();
for(ip=i;*ip;ip++) add_image(*ip);
}


multiBand::multiBand(Image **i, int n)
{
int a;
init();
for(a=0;a<n;a++) add_image(i[a]);
}


multiBand::~multiBand()
{
free();
}


void multiBand::init()
{
img=NULL;
}


int multiBand::allocate(int x, int y, int)
{
xres=x; yres=y;
return 1;
}


void multiBand::free()
{
int	i;

if(clean&&img)
	for(i=0;i<bands;i++) delete img[i];
if(img) ::free((char *)img);
img=NULL;
xres=0; yres=0; bands=0;
}


void multiBand::add_image(Image *i, int p)
{
int	n,x,y;

if(p==-1) p=bands;
if(p<0 || p>bands || !i) return;
bands++;
img=(Image **)realloc(img,bands*sizeof(Image *));
for(n=bands-1;n>p;n--) img[n]=img[n-1];
img[p]=i;
i->get_res(&x,&y);
if(x>xres) xres=x;
if(y>yres) yres=y;
}


void multiBand::remove_image(int p)
{
int	n;

if(p==-1) p=bands-1;
if(p<0 || p>bands) return;
bands--;
if(!bands) { ::free((char *)img); img=NULL; return; }
for(n=p;n<bands;n++) img[n]=img[n+1];
img=(Image **)realloc(img,bands*sizeof(Image *));
}


void multiBand::set_level(double l)
{
int	i;

if(!img) return;

for(i=0;i<bands;i++) img[i]->set_level(l);
}


uchar multiBand::get_uchar(int x, int y, int b)
{
if(check_band(b)) return 0;
return (img[b]->get_data())->get_uchar(x,y);
}


uchar multiBand::iget_uchar(double x, double y, int b)
{
if(check_band(b)) return 0;
return (img[b]->get_data())->iget_uchar(x,y);
}


void multiBand::set_uchar(uchar v, int x, int y, int b)
{
if(!check_band(b)) (img[b]->get_data())->set_uchar(v,x,y);
}


// ****************************** fadeBorder *******************************


int fadeBorder::allocate(int x, int y, int)
{
xres=x; yres=y;
return 1;
}


void fadeBorder::free()
{
if(clean&&img) delete img;
xres=0; yres=0; bands=0;
img=NULL;
}


int fadeBorder::add_band()
{
ImageData	*dat;
int		r=0;

if(img) dat=img->get_data();
if(dat)	r=dat->add_band();
if(img) bands=img->get_bands();
return r;
}


int fadeBorder::remove_band(int b)
{
ImageData	*dat;
int		r=0;

if(img) dat=img->get_data();
if(dat)	r=dat->remove_band(b);
if(img) bands=img->get_bands();
return r;
}


int fadeBorder::clear(int b)
{
if(img)	return img->clear(b);
else return 0;
}


void fadeBorder::get_color(int x, int y, uchar *r, uchar *g, uchar *b, uchar *a)
{
register int	d,t;

if(!img) { *r=0; *g=0; *b=0; *a=0; return; }

img->get_color(x,y,r,g,b,a);

if(!a) return;
if(!*a) return;
if(check_bounds(x,y)) { *a=0; return; }

if(x<y) d=x; else d=y;
if((t=xres-x-1)<d) d=t;
if((t=yres-y-1)<d) d=t;

if((d -= cropwidth) < 0) { *a=0; return; }
if(d<bwidth) *a=*a*d/bwidth;
}


void fadeBorder::iget_color(double x, double y, uchar *r, uchar *g, uchar *b, uchar *a)
{
double	d,t;

if(!img) { *r=0; *g=0; *b=0; *a=0; return; }

(img->get_data())->iget_color(x,y,r,g,b,a);

if(!a) return;
if(!*a) return;
if(check_bounds(x,y)) { *a=0; return; }

if(x<y) d=x; else d=y;
if((t=xres-x-1.0)<d) d=t;
if((t=yres-y-1.0)<d) d=t;

if((d -= cropwidth) < 0) { *a=0; return; }
if(d<(double)bwidth) *a=(uchar)(*a*d/bwidth);
}


void fadeBorder::set_color(int x, int y, uchar r, uchar g, uchar b, uchar a)
{
if(img) img->set_color(x,y,r,g,b,a);
}


uchar fadeBorder::get_alpha(int x, int y)
{
uchar	a;
register int	d,t;

if(check_bounds(x,y)) return 0;
a=img->get_alpha(x,y);
if(!a) return 0;

if(x<y) d=x; else d=y;
if((t=xres-x-1)<d) d=t;
if((t=yres-y-1)<d) d=t;

if((d -= cropwidth) < 0) { a=0; return 0; }
if(d<bwidth) a=a*d/bwidth;
return a;
}


uchar fadeBorder::iget_alpha(double x, double y)
{
uchar	a;
double	d,t;

if(check_bounds(x,y)) return 0;
a=(img->get_data())->iget_alpha(x,y);
if(!a) return 0;

if(x<y) d=x; else d=y;
if((t=xres-x-1.0)<d) d=t;
if((t=yres-y-1.0)<d) d=t;

if((d -= cropwidth) < 0) { a=0; return 0; }
if(d<(double)bwidth) a=(uchar)(a*d/bwidth);
return a;
}


void fadeBorder::set_alpha(int x, int y, uchar a)
{
img->set_alpha(x,y,a);
}


// ****************************** cropData *******************************


int cropData::allocate(int x, int y, int)
{
xres=x; yres=y;
return 1;
}


void cropData::free()
{
if(img&&clean) delete img;
xres=0; yres=0; bands=0;
img=NULL;
}


int cropData::add_band()
{
ImageData	*dat;
int		r=0;

if(img) dat=img->get_data();
if(dat)	r=dat->add_band();
if(img) bands=img->get_bands();
return r;
}


int cropData::remove_band(int b)
{
ImageData	*dat;
int		r=0;

if(img) dat=img->get_data();
if(dat)	r=dat->remove_band(b);
if(img) bands=img->get_bands();
return r;
}


int cropData::clear(int b)
{
if(img)	return img->clear(b);
else return 0;
}


void cropData::get_color(int x, int y, uchar *r, uchar *g, uchar *b, uchar *a)
{
x+=xmin; y+=ymin;
if(!img || x<xmin || x>=xmax || y<ymin || y>=ymax) { *r=0; *g=0; *b=0; *a=0; return; }

img->get_color(x,y,r,g,b,a);
}


void cropData::iget_color(double x, double y, uchar *r, uchar *g, uchar *b, uchar *a)
{
x+=dxmin; y+=dymin;
if(!img || x<dxmin || x>=dxmax || y<dymin || y>=dymax) { *r=0; *g=0; *b=0; *a=0; return; }

(img->get_data())->iget_color(x,y,r,g,b,a);
}


void cropData::set_color(int x, int y, uchar r, uchar g, uchar b, uchar a)
{
x+=xmin; y+=ymin;
if(img) img->set_color(x,y,r,g,b,a);
}


uchar cropData::get_alpha(int x, int y)
{
x+=xmin; y+=ymin;
if(x<xmin || x>=xmax || y<ymin || y>=ymax) return 0;
return img->get_alpha(x,y);
}


uchar cropData::iget_alpha(double x, double y)
{
x+=dxmin; y+=dymin;
if(x<dxmin || x>=dxmax || y<dymin || y>=dymax) return 0;
return (img->get_data())->iget_alpha(x,y);
}


void cropData::set_alpha(int x, int y, uchar a)
{
x+=xmin; y+=ymin;
img->set_alpha(x,y,a);
}


int cropData::get_bit(int x, int y, int b)
{
x+=xmin; y+=ymin;
if(x<xmin || x>=xmax || y<ymin || y>=ymax) return 0;
return	(img->get_data())->get_bit(x,y,b);
}


int cropData::iget_bit(double x, double y, int b)
{
x+=dxmin; y+=dymin;
if(x<dxmin || x>=dxmax || y<dymin || y>=dymax) return 0;
return	(img->get_data())->iget_bit(x,y,b);
}


char cropData::get_char(int x, int y, int b)
{
x+=xmin; y+=ymin;
if(x<xmin || x>=xmax || y<ymin || y>=ymax) return 0;
return	(img->get_data())->get_char(x,y,b);
}


char cropData::iget_char(double x, double y, int b)
{
x+=dxmin; y+=dymin;
if(x<dxmin || x>=dxmax || y<dymin || y>=dymax) return 0;
return	(img->get_data())->iget_char(x,y,b);
}


uchar cropData::get_uchar(int x, int y, int b)
{
x+=xmin; y+=ymin;
if(x<xmin || x>=xmax || y<ymin || y>=ymax) return 0;
return	(img->get_data())->get_uchar(x,y,b);
}


uchar cropData::iget_uchar(double x, double y, int b)
{
x+=dxmin; y+=dymin;
if(x<dxmin || x>=dxmax || y<dymin || y>=dymax) return 0;
return	(img->get_data())->iget_uchar(x,y,b);
}


short cropData::get_short(int x, int y, int b)
{
x+=xmin; y+=ymin;
if(x<xmin || x>=xmax || y<ymin || y>=ymax) return 0;
return	(img->get_data())->get_short(x,y,b);
}


short cropData::iget_short(double x, double y, int b)
{
x+=dxmin; y+=dymin;
if(x<dxmin || x>=dxmax || y<dymin || y>=dymax) return 0;
return	(img->get_data())->iget_short(x,y,b);
}


ushort cropData::get_ushort(int x, int y, int b)
{
x+=xmin; y+=ymin;
if(x<xmin || x>=xmax || y<ymin || y>=ymax) return 0;
return	(img->get_data())->get_ushort(x,y,b);
}


ushort cropData::iget_ushort(double x, double y, int b)
{
x+=dxmin; y+=dymin;
if(x<dxmin || x>=dxmax || y<dymin || y>=dymax) return 0;
return	(img->get_data())->iget_ushort(x,y,b);
}


long cropData::get_long(int x, int y, int b)
{
x+=xmin; y+=ymin;
if(x<xmin || x>=xmax || y<ymin || y>=ymax) return 0;
return	(img->get_data())->get_long(x,y,b);
}


long cropData::iget_long(double x, double y, int b)
{
x+=dxmin; y+=dymin;
if(x<dxmin || x>=dxmax || y<dymin || y>=dymax) return 0;
return	(img->get_data())->iget_long(x,y,b);
}


ulong cropData::get_ulong(int x, int y, int b)
{
x+=xmin; y+=ymin;
if(x<xmin || x>=xmax || y<ymin || y>=ymax) return 0;
return	(img->get_data())->get_ulong(x,y,b);
}


ulong cropData::iget_ulong(double x, double y, int b)
{
x+=dxmin; y+=dymin;
if(x<dxmin || x>=dxmax || y<dymin || y>=dymax) return 0;
return	(img->get_data())->iget_ulong(x,y,b);
}


float cropData::get_float(int x, int y, int b)
{
x+=xmin; y+=ymin;
if(x<xmin || x>=xmax || y<ymin || y>=ymax) return 0.0;
return	(img->get_data())->get_float(x,y,b);
}


float cropData::iget_float(double x, double y, int b)
{
x+=dxmin; y+=dymin;
if(x<dxmin || x>=dxmax || y<dymin || y>=dymax) return 0.0;
return	(img->get_data())->iget_float(x,y,b);
}


double cropData::get_double(int x, int y, int b)
{
x+=xmin; y+=ymin;
if(x<xmin || x>=xmax || y<ymin || y>=ymax) return 0.0;
return	(img->get_data())->get_double(x,y,b);
}


double cropData::iget_double(double x, double y, int b)
{
x+=dxmin; y+=dymin;
if(x<dxmin || x>=dxmax || y<dymin || y>=dymax) return 0.0;
return	(img->get_data())->iget_double(x,y,b);
}


COMPLEX_TYPE cropData::get_complex(int x, int y, int b)
{
x+=xmin; y+=ymin;
if(x<xmin || x>=xmax || y<ymin || y>=ymax) return complex_zero;
return	(img->get_data())->get_complex(x,y,b);
}


COMPLEX_TYPE cropData::iget_complex(double x, double y, int b)
{
x+=dxmin; y+=dymin;
if(x<dxmin || x>=dxmax || y<dymin || y>=dymax) return complex_zero;
return	(img->get_data())->iget_complex(x,y,b);
}


// ****************************** pyrpixData *******************************


int pyrpixData::allocate(int x, int y, int)
{
xres=x; yres=y;
return 1;
}


void pyrpixData::free()
{
if(img&&clean) delete img;
xres=0; yres=0; bands=0;
img=NULL;
}


int pyrpixData::add_band()
{
ImageData	*dat;
int		r=0;

if(img) dat=img->get_data();
if(dat)	r=dat->add_band();
if(img) bands=img->get_bands();
return r;
}


int pyrpixData::remove_band(int b)
{
ImageData	*dat;
int		r=0;

if(img) dat=img->get_data();
if(dat)	r=dat->remove_band(b);
if(img) bands=img->get_bands();
return r;
}


int pyrpixData::clear(int b)
{
if(img)	return img->clear(b);
else return 0;
}


void pyrpixData::get_color(int x, int y, uchar *r, uchar *g, uchar *b, uchar *a)
{
if(!img) { *r=0; *g=0; *b=0; *a=0; return; }
img->set_level(clev);
img->get_color(x,y,r,g,b,a);
}


void pyrpixData::iget_color(double x, double y, uchar *r, uchar *g, uchar *b, uchar *a)
{
if(!img) { *r=0; *g=0; *b=0; *a=0; return; }
img->set_level(clev);
(img->get_data())->iget_color(x,y,r,g,b,a);
}


void pyrpixData::set_color(int x, int y, uchar r, uchar g, uchar b, uchar a)
{
if(img) { img->set_level(clev); img->set_color(x,y,r,g,b,a); }
}


uchar pyrpixData::get_alpha(int x, int y)
{
img->set_level(clev);
return img->get_alpha(x,y);
}


uchar pyrpixData::iget_alpha(double x, double y)
{
img->set_level(clev);
return (img->get_data())->iget_alpha(x,y);
}


void pyrpixData::set_alpha(int x, int y, uchar a)
{
img->set_level(clev);
img->set_alpha(x,y,a);
}


uchar pyrpixData::get_uchar(int x, int y, int b)
{
img->set_level(clev);
return	(img->get_data())->get_uchar(x,y,b);
}


uchar pyrpixData::iget_uchar(double x, double y, int b)
{
img->set_level(clev);
return	(img->get_data())->iget_uchar(x,y,b);
}


// ****************************** doubleMap *******************************


void doubleMap::init()
{
img=NULL;
num=0;
col=NULL;
root=NULL;
}


int doubleMap::allocate(int, int, int)
{
return 1;
}


void doubleMap::free()
{
if(img&&clean) delete img;
img=NULL;
}


void doubleMap::alloc_col()
{
if(!num) free_col();
else	col=(colcell *)realloc(col,num*sizeof(colcell));
}


void doubleMap::free_col()
{
::free((char *)col);
col=NULL;
num=0;
}


void doubleMap::free_tree()
{
int	i;

for(i=0;i<num;i++) if(col[i].vnode) {
			::free((char *)col[i].vnode); col[i].vnode=NULL; }
root=NULL;
}


void doubleMap::build_tree()
{
if(!num) { root=NULL; return; }
root=build_range(0,num);
}


doubleMap::valnode *doubleMap::build_range(int n1, int n2)
{
int	n;
valnode	*v;

if(n2==n1+1) return (valnode *)n1;

n=(n1+n2+1)>>1;

v=(valnode *)malloc(sizeof(valnode));
col[n].vnode=v;
v->val=col[n].val;
v->less=build_range(n1,n);
v->geq=build_range(n,n2);
return v;
}


void doubleMap::comp_col(int n)
{
double	d;

col[n].sr=(double)col[n].gr+0.5;
col[n].sg=(double)col[n].gg+0.5;
col[n].sb=(double)col[n].gb+0.5;
if(n>0) {
	d=col[n].val-col[n-1].val;
	if(d>0.0) {
		d=1.0/d;
		col[n-1].dr=((double)col[n].lr-(double)col[n-1].gr)*d;
		col[n-1].dg=((double)col[n].lg-(double)col[n-1].gg)*d;
		col[n-1].db=((double)col[n].lb-(double)col[n-1].gb)*d;
		}
	}
if(n<num-1) {
	d=col[n+1].val-col[n].val;
	if(d>0.0) {
		d=1.0/d;
		col[n].dr=((double)col[n+1].lr-(double)col[n].gr)*d;
		col[n].dg=((double)col[n+1].lg-(double)col[n].gg)*d;
		col[n].db=((double)col[n+1].lb-(double)col[n].gb)*d;
		}
	}
else {	col[n].dr=0.0;
	col[n].dg=0.0;
	col[n].db=0.0;
	}
}


void doubleMap::set_val(int n, double v)
{
if(n<0 || n>=num) return;

if(n>0) if(v<col[n-1].val) v=col[n-1].val;
if(n<num-1) if(v>col[n+1].val) v=col[n+1].val;
col[n].val=v;
if(col[n].vnode) (col[n].vnode)->val=v;
comp_col(n);
}


void doubleMap::add_val(double val, uchar lr, uchar lg, uchar lb,
				    uchar gr, uchar gg, uchar gb)
{
int	i,n;

free_tree();

for(n=0;n<num;n++) if(val<col[n].val) break;
num++;
alloc_col();
for(i=num-1;i>n;i--) col[i]=col[i-1];
col[n].vnode=NULL;
set_data(n,val,lr,lg,lb,gr,gg,gb);
build_tree();
}


void doubleMap::add_val(double val)
{
int	i,n;
uchar	r,g,b;

get_vcolor(val,&r,&g,&b);
free_tree();

for(n=0;n<num;n++) if(val<col[n].val) break;
num++;
alloc_col();
for(i=num-1;i>n;i--) col[i]=col[i-1];
col[n].vnode=NULL;
set_data(n,val,r,g,b,r,g,b);
build_tree();
}


void doubleMap::remove(int n)
{
int	i;

if(n<0 || n>=num) return;

free_tree();
num--;
for(i=n;i<num;i++) col[i]=col[i+1];
alloc_col();
if(n>0) comp_col(n-1);
build_tree();
}


void doubleMap::remove_all()
{
free_tree();
free_col();
}


void doubleMap::copy(doubleMap &dm)
{
int	i;

remove_all();
num=dm.num;
alloc_col();
for(i=0;i<num;i++) {
	col[i]=dm.col[i];
	col[i].vnode=NULL;
	}
build_tree();
}


void doubleMap::set_lcolor(int n, uchar r, uchar g, uchar b)
{
if(n<0 || n>=num) return;

col[n].lr=r;
col[n].lg=g;
col[n].lb=b;
comp_col(n);
}


void doubleMap::set_gcolor(int n, uchar r, uchar g, uchar b)
{
if(n<0 || n>=num) return;

col[n].gr=r;
col[n].gg=g;
col[n].gb=b;
comp_col(n);
}


void doubleMap::set_data(int n, double v, uchar lr, uchar lg, uchar lb,
					  uchar gr, uchar gg, uchar gb)
{
if(n<0 || n>=num) return;

col[n].lr=lr;
col[n].lg=lg;
col[n].lb=lb;
col[n].gr=gr;
col[n].gg=gg;
col[n].gb=gb;
set_val(n,v);
}


void doubleMap::get_vcolor(double v, uchar *r, uchar *g, uchar *b)
{
valnode	*vn;
colcell	*c;

if(!img || !num) { *r=0; *g=0; *b=0; return; }

if(v<col->val) { *r=col->lr; *g=col->lg; *b=col->lb; }
else {	for(vn=root;(long)vn>=num;) {
		if(v<vn->val) vn=vn->less;
		else	vn=vn->geq;
		}
	c=&col[(long)vn];
	v-=c->val;
	*r=(uchar)(c->sr+v*c->dr);
	*g=(uchar)(c->sg+v*c->dg);
	*b=(uchar)(c->sb+v*c->db);
	}
}


void doubleMap::get_color(int x, int y, uchar *r, uchar *g, uchar *b, uchar *a)
{
double	value;
valnode	*vn;
colcell	*c;

if(!img || !num) { *r=0; *g=0; *b=0; if(a) *a=0; return; }

value=(img->get_data())->get_double(x,y);

if(value<col->val) { *r=col->lr; *g=col->lg; *b=col->lb; }
else {	for(vn=root;(long)vn>=num;) {
		if(value<vn->val) vn=vn->less;
		else	vn=vn->geq;
		}
	c=&col[(long)vn];
	value-=c->val;
	*r=(uchar)(c->sr+value*c->dr);
	*g=(uchar)(c->sg+value*c->dg);
	*b=(uchar)(c->sb+value*c->db);
	}
if(a) *a=img->get_alpha(x,y);
}


void doubleMap::iget_color(double x, double y, uchar *r, uchar *g, uchar *b, uchar *a)
{
double	value;
valnode	*vn;
colcell	*c;

if(!img || !num) { *r=0; *g=0; *b=0; if(a) *a=0; return; }

value=(img->get_data())->iget_double(x,y);

if(value<col->val) { *r=col->lr; *g=col->lg; *b=col->lb; }
else {	for(vn=root;(long)vn>=num;) {
		if(value<vn->val) vn=vn->less;
		else	vn=vn->geq;
		}
	c=&col[(long)vn];
	value-=c->val;
	*r=(uchar)(c->sr+value*c->dr);
	*g=(uchar)(c->sg+value*c->dg);
	*b=(uchar)(c->sb+value*c->db);
	}
if(a) *a=(img->get_data())->iget_alpha(x,y);
}


// ****************************** pyrData *******************************


double pyr_fraction[32] = {	0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
				0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
				0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
				0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0  };

pyrData::pyrData(Image **i)
{
Image **ip;
init();
for(ip=i;*ip;ip++) add_image(*ip);
}


pyrData::pyrData(Image **i, int n)
{
int a;
init();
for(a=0;a<n;a++) add_image(i[a]);
}


pyrData::~pyrData()
{
free();
}


void pyrData::init()
{
int	i;

if(pyr_fraction[0]==0.0) { pyr_fraction[0]=1.0;
	for(i=1;i<32;i++) pyr_fraction[i]=pyr_fraction[i-1]*0.5; }

img=NULL; levels=0;
lev_int=0;
lev_frac=0.0;
}


int pyrData::allocate(int x, int y, int b)
{
xres=x; yres=y; bands=b;
return 1;
}


void pyrData::free()
{
int	i;

if(clean&&img)
	for(i=0;i<levels;i++) delete img[i];
if(img) ::free((char *)img);
img=NULL;
xres=0; yres=0; bands=0; levels=0;
}


void pyrData::add_image(Image *i, int p)
{
int	n,x,y;

if(p==-1) p=levels;
if(p<0 || p>levels || !i) return;
levels++; levmax=(double)(levels-1);
img=(Image **)realloc(img,levels*sizeof(Image *));
for(n=levels-1;n>p;n--) img[n]=img[n-1];
img[p]=i;
i->get_res(&x,&y);
if(x>xres) xres=x;
if(y>yres) yres=y;
}


void pyrData::remove_image(int p)
{
int	n;

if(p==-1) p=levels-1;
if(p<0 || p>levels) return;
levels--; levmax=(double)(levels-1);
if(!levels) { ::free((char *)img); img=NULL; return; }
for(n=p;n<levels;n++) img[n]=img[n+1];
img=(Image **)realloc(img,levels*sizeof(Image *));
}


void pyrData::get_color(int x, int y, uchar *r, uchar *g, uchar *b, uchar *a)
{
(img[lev_int]->get_data())->get_color(x>>lev_int,y>>lev_int,r,g,b,a);
}


void pyrData::iget_color(double x, double y, uchar *r, uchar *g, uchar *b, uchar *a)
{
uchar	r1,g1,b1,a1;
double	f;

if(lev_int) { x*=pyr_fraction[lev_int]; y*=pyr_fraction[lev_int]; }
if(lev_frac<0.000001) (img[lev_int]->get_data())->iget_color(x,y,r,g,b,a);
else {	f=1.0-lev_frac;
	(img[lev_int]->get_data())->iget_color(x,y,r,g,b,a);
	if(a) { (img[lev_int+1]->get_data())->iget_color(x*0.5,y*0.5,&r1,&g1,&b1,&a1);
		*a=(uchar)(*a*f+a1*lev_frac); }
	else	(img[lev_int+1]->get_data())->iget_color(x*0.5,y*0.5,&r1,&g1,&b1,NULL);
	*r=(uchar)(*r*f+r1*lev_frac);
	*g=(uchar)(*g*f+g1*lev_frac);
	*b=(uchar)(*b*f+b1*lev_frac);
	}
}


void pyrData::set_color(int x, int y, uchar r, uchar g, uchar b, uchar a)
{
(img[lev_int]->get_data())->set_color(x>>lev_int,y>>lev_int,r,g,b,a);
}


int pyrData::iget_bit(double x, double y, int b)
{
if(lev_int) { x*=pyr_fraction[lev_int]; y*=pyr_fraction[lev_int]; }
if(lev_frac<0.000001) return (img[lev_int]->get_data())->iget_bit(x,y,b);
else return (int)((1.0-lev_frac)*(img[lev_int]->get_data())->iget_bit(x,y,b)+
	lev_frac*(img[lev_int+1]->get_data())->iget_bit(x*0.5,y*0.5,b));
}


char pyrData::iget_char(double x, double y, int b)
{
if(lev_int) { x*=pyr_fraction[lev_int]; y*=pyr_fraction[lev_int]; }
if(lev_frac<0.000001) return (img[lev_int]->get_data())->iget_char(x,y,b);
else return (char)((1.0-lev_frac)*(img[lev_int]->get_data())->iget_char(x,y,b)+
	lev_frac*(img[lev_int+1]->get_data())->iget_char(x*0.5,y*0.5,b));
}


uchar pyrData::iget_uchar(double x, double y, int b)
{
if(lev_int) { x*=pyr_fraction[lev_int]; y*=pyr_fraction[lev_int]; }
if(lev_frac<0.000001) return (img[lev_int]->get_data())->iget_uchar(x,y,b);
else return (uchar)((1.0-lev_frac)*(img[lev_int]->get_data())->iget_uchar(x,y,b)+
	lev_frac*(img[lev_int+1]->get_data())->iget_uchar(x*0.5,y*0.5,b));
}


short pyrData::iget_short(double x, double y, int b)
{
if(lev_int) { x*=pyr_fraction[lev_int]; y*=pyr_fraction[lev_int]; }
if(lev_frac<0.000001) return (img[lev_int]->get_data())->iget_short(x,y,b);
else return (short)((1.0-lev_frac)*(img[lev_int]->get_data())->iget_short(x,y,b)+
	lev_frac*(img[lev_int+1]->get_data())->iget_short(x*0.5,y*0.5,b));
}


ushort pyrData::iget_ushort(double x, double y, int b)
{
if(lev_int) { x*=pyr_fraction[lev_int]; y*=pyr_fraction[lev_int]; }
if(lev_frac<0.000001) return (img[lev_int]->get_data())->iget_ushort(x,y,b);
else return (ushort)((1.0-lev_frac)*(img[lev_int]->get_data())->iget_ushort(x,y,b)+
	lev_frac*(img[lev_int+1]->get_data())->iget_ushort(x*0.5,y*0.5,b));
}


long pyrData::iget_long(double x, double y, int b)
{
if(lev_int) { x*=pyr_fraction[lev_int]; y*=pyr_fraction[lev_int]; }
if(lev_frac<0.000001) return (img[lev_int]->get_data())->iget_long(x,y,b);
else return (long)((1.0-lev_frac)*(img[lev_int]->get_data())->iget_long(x,y,b)+
	lev_frac*(img[lev_int+1]->get_data())->iget_long(x*0.5,y*0.5,b));
}


ulong pyrData::iget_ulong(double x, double y, int b)
{
if(lev_int) { x*=pyr_fraction[lev_int]; y*=pyr_fraction[lev_int]; }
if(lev_frac<0.000001) return (img[lev_int]->get_data())->iget_ulong(x,y,b);
else return (ulong)((1.0-lev_frac)*(img[lev_int]->get_data())->iget_ulong(x,y,b)+
	lev_frac*(img[lev_int+1]->get_data())->iget_ulong(x*0.5,y*0.5,b));
}


float pyrData::iget_float(double x, double y, int b)
{
if(lev_int) { x*=pyr_fraction[lev_int]; y*=pyr_fraction[lev_int]; }
if(lev_frac<0.000001) return (img[lev_int]->get_data())->iget_float(x,y,b);
else return (float)((1.0-lev_frac)*(img[lev_int]->get_data())->iget_float(x,y,b)+
	lev_frac*(img[lev_int+1]->get_data())->iget_float(x*0.5,y*0.5,b));
}


double pyrData::iget_double(double x, double y, int b)
{
if(lev_int) { x*=pyr_fraction[lev_int]; y*=pyr_fraction[lev_int]; }
if(lev_frac<0.000001) return (img[lev_int]->get_data())->iget_double(x,y,b);
else return (1.0-lev_frac)*(img[lev_int]->get_data())->iget_double(x,y,b)+
	lev_frac*(img[lev_int+1]->get_data())->iget_double(x*0.5,y*0.5,b);
}


COMPLEX_TYPE pyrData::iget_complex(double x, double y, int b)
{
if(lev_int) { x*=pyr_fraction[lev_int]; y*=pyr_fraction[lev_int]; }
if(lev_frac<0.000001) return (img[lev_int]->get_data())->iget_complex(x,y,b);
else return (1.0-lev_frac)*(img[lev_int]->get_data())->iget_complex(x,y,b)+
	lev_frac*(img[lev_int+1]->get_data())->iget_complex(x*0.5,y*0.5,b);
}


// ****************************** interpData *******************************


int interpData::allocate(int x, int y, int)
{
xres=x; yres=y;
return 1;
}


void interpData::free()
{
xres=0; yres=0; bands=0;
if(clean && img) delete img;
img=NULL;
}


int interpData::add_band()
{
ImageData	*dat;
int		r=0;

if(img) dat=img->get_data();
if(dat)	r=dat->add_band();
if(img) bands=img->get_bands();
return r;
}


int interpData::remove_band(int b)
{
ImageData	*dat;
int		r=0;

if(img) dat=img->get_data();
if(dat)	r=dat->remove_band(b);
if(img) bands=img->get_bands();
return r;
}


int interpData::clear(int b)
{
if(img)	return img->clear(b);
else return 0;
}


void interpData::get_color(int x, int y, uchar *r, uchar *g, uchar *b, uchar *a)
{
//if(!img) { *r=0; *g=0; *b=0; *a=0; return; }
(img->get_data())->iget_color((double)x,(double)y,r,g,b,a);
}


void interpData::iget_color(double x, double y, uchar *r, uchar *g, uchar *b, uchar *a)
{
//if(!img) { *r=0; *g=0; *b=0; *a=0; return; }
(img->get_data())->iget_color(x,y,r,g,b,a);
}


void interpData::set_color(int x, int y, uchar r, uchar g, uchar b, uchar a)
{
if(img) img->set_color(x,y,r,g,b,a);
}


uchar interpData::get_alpha(int x, int y)
{
return	(img->get_data())->iget_alpha((double)x,(double)y);
}


uchar interpData::iget_alpha(double x, double y)
{
return	(img->get_data())->iget_alpha(x,y);
}


void interpData::set_alpha(int x, int y, uchar a)
{
img->set_alpha(x,y,a);
}


