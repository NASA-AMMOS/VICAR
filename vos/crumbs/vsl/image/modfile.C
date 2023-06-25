// modfile.C
//
// Written by Dave Kagels 1/26/95

#include "image/types/modfile.h"
#include "image/geoimage.h"
#include "image/datatypes.h"
#include "image/datamod.h"
#include "image/types/kap_geodata.h"
#include "image/types/map_geodata.h"

int MODFile::read_data(FILE *f)
{
char	buf[4];

if(!img) return error=ImageFileErr_NoImage;		// check for an Image

if(f) attach(f);					// set file pointer

// Read header:
fread(buf,2);
if(strncmp(buf,"I~",2))
	return error=ImageFileErr_BadFile;
root=scan_image();
if(!root) {
	fprintf(stderr,"No block found\n"); free_list();
	return error=ImageFileErr_BadFile; }
if(root==(Image *)1) { free_list(); return error=ImageFileErr_BadFile; }
img->set_data(root->get_data());
if(img->image_class_type() == GEOIMAGE && ((GeoImage *)root)->get_geo_data() && !root->get_data()->get_geo_data()) {
	((GeoImage *)img)->set_geo_data(((GeoImage *)root)->get_geo_data());
}

free_list();
return error;
}


int MODFile::write_data(FILE *)
{

return error=ImageFileErr_BadFileType;
}


int MODFile::find_char(int ch)
{
int	c;

while(c=getc(fp)) {
	if(c==EOF) return -1;
	if(c==' ' || c=='\n' || c=='\t') continue;
	if(c==ch) return 1;
	ungetc(c,fp); return 0;
	}
return(0);
}


Image *MODFile::scan_image()
{
Image	*i=NULL;
char	buf[256];

if(find_char('<')>0) { // reference
	fscanf(fp,"%s",buf);
	if(buf[strlen(buf)-1]=='>') {
		buf[strlen(buf)-1]=(char)0;
		i=find_image(buf);
		if(!i) return (Image *)1;
		}
	else {	i=find_image(buf);
		if(!i) return (Image *)1;
		if(find_char('>')<1) {
			fprintf(stderr,"No reference end found\n");
			return (Image *)1;
			}
		}
	return i;
	}
if(find_char('{')<1) return NULL;
i=new GeoImage();
if(!read_block(i)) return (Image *)1;
return i;
}


void MODFile::new_name(char *n, Image *i)
{
if(!num) {
	name_list=(char **)malloc(sizeof(char *));
	img_list=(Image **)malloc(sizeof(Image *));
	num=1;
	}
else {	num++;
	name_list=(char **)realloc(name_list,num*sizeof(char *));
	img_list=(Image **)realloc(img_list,num*sizeof(Image *));
	}
name_list[num-1]=strdup(n);
img_list[num-1]=i;
}


void MODFile::free_list()
{
int	i;

if(!num) return;
for(i=0;i<num;i++) ::free(name_list[i]);
free((char *)name_list);
free((char *)img_list);
name_list=NULL; img_list=NULL; num=0;
}


Image *MODFile::find_image(char *n)
{
int	i;

for(i=0;i<num;i++)
	if(!strcmp(name_list[i],n)) return img_list[i];
	
fprintf(stderr,"Referenced image not found: %s\n",n);
return NULL;
}


int MODFile::read_block(Image *i)
{
char	buf[256];
int	r=0;

if(find_char('(')>0) { // name
	fscanf(fp,"%s",buf);
	if(buf[strlen(buf)-1]==')') {
		buf[strlen(buf)-1]=(char)0;
		new_name(buf,i);
		}
	else {	new_name(buf,i);
		if(find_char(')')<1) {
			fprintf(stderr,"No name end found\n");
			return 0;
			}
		}
	}

fscanf(fp,"%s",buf);

if(!strcmp(buf,"file"))			r=block_file(i);
else if(!strcmp(buf,"black"))		r=block_black(i);
else if(!strcmp(buf,"transform"))	r=block_transform(i);
else if(!strcmp(buf,"composite"))	r=block_composite(i);
else if(!strcmp(buf,"color"))		r=block_color(i);
else if(!strcmp(buf,"multiband"))	r=block_multiband(i);
else if(!strcmp(buf,"ramp"))		r=block_ramp(i);
else if(!strcmp(buf,"pyramid"))		r=block_pyramid(i);
else if(!strcmp(buf,"fade"))		r=block_fade(i);
else if(!strcmp(buf,"crop"))		r=block_crop(i);
else if(!strcmp(buf,"pyrpix"))		r=block_pyrpix(i);

else { fprintf(stderr,"Unknown block type %s\n",buf); return 0; }

if(!r) { fprintf(stderr,"Error reading block %s\n",buf); return 0; }

if(find_char('[')>0) { if(!read_params(i)) return 0; }
if(find_char('}')<1) {
	fprintf(stderr,"No block end found\n");
	return 0; }
else return 1;
}


int MODFile::read_params(Image *i)
{
char	buf[256];
int	n,n1,n2,num_refs;
Image	*mimg;
int	x,y;
double	lat,lng;
char	*llimgname=NULL, *xyimgname=NULL;

do {	fscanf(fp,"%s",buf);

	     if(!strcmp(buf,"default_alpha")) {
		fscanf(fp,"%d",&n); i->set_default_alpha((uchar)n); }
	else if(!strcmp(buf,"red_band")) {
		fscanf(fp,"%d",&n); i->set_red_band(n); }
	else if(!strcmp(buf,"green_band")) {
		fscanf(fp,"%d",&n); i->set_green_band(n); }
	else if(!strcmp(buf,"blue_band")) {
		fscanf(fp,"%d",&n); i->set_blue_band(n); }
	else if(!strcmp(buf,"alpha_band")) {
		fscanf(fp,"%d",&n); i->set_alpha_band(n); }
	else if(!strcmp(buf,"default_band")) {
		fscanf(fp,"%d",&n); i->set_default_band(n); }
	else if(!strcmp(buf,"allocate")) {
		fscanf(fp,"%d%d%d",&n,&n1,&n2); i->allocate(n,n1,n2); }
	else if(!strcmp(buf,"colormap")) {
		if(find_char('{')<1) { 
			fprintf(stderr,"No colormap block found\n"); return 0; }
		mimg=new Image();
		if(!read_block(mimg)) return 0;
		i->set_map(mimg);
		}
	else if(!strcmp(buf,"georeference") && img->image_class_type() == GEOIMAGE) {
		fscanf(fp, "%d", &num_refs);
		if(num_refs) {
			// create geodata object
			KAPGeoData *kgd = new KAPGeoData();
			for(n=0; n<abs(num_refs); n++) {
				fscanf(fp,"%s", buf);
				// parse ref line
				sscanf(buf,"REF/%*d,%d,%d,%lf,%lf",&x,&y,&lat,&lng);
				// add ref point to geodata
				kgd->add_reference_point(x,y,lat,lng);
			}
			// gen poly coeffs for geodata & add geodata to img
			if(num_refs>0) {
				if(kgd->gen_map_terms()) {
					((GeoImage *)i)->set_geo_data(kgd);
				} else {
					fprintf(stderr,"Unable to generate polynomials from %d reference points.\n", num_refs);
					delete(kgd);
				}
			} else {
				kgd->set_shift_value(180.0);
				if(kgd->gen_map_terms()) { 
					kgd->set_min_max_lng(-180.0, 180.0);
					((GeoImage *)i)->set_geo_data(kgd);
				} else {
					fprintf(stderr,"Unable to generate polynomials from %d reference points.\n", num_refs);
					delete(kgd);
				}
			}
		}
	} else if(!strcmp(buf,"geomap") && img->image_class_type() == GEOIMAGE) {
		fscanf(fp,"%s", buf);
		if(!strcmp(buf,"lat_long_img")) {
			fscanf(fp,"%s", buf);
			llimgname = strdup(buf);
		} else {
			fprintf(stderr,"Unknown parameter in geomap block %s\n",buf); return 0;
		} 
		fscanf(fp,"%s", buf);
		if(!strcmp(buf,"x_y_img")) {
			fscanf(fp,"%s", buf);
			xyimgname = strdup(buf);
		} else {
			fprintf(stderr,"Unknown parameter in geomap block %s\n",buf); return 0;
		}

		// create geodata object
		MAPGeoData *mgd = new MAPGeoData();
		Image	*timg;

		// open the conversion image files
		if(llimgname && strcmp(llimgname,"NONE")) {
			timg = new Image(llimgname);
			mgd->set_ll_image(timg);
		}
		if(xyimgname && strcmp(xyimgname,"NONE")) {
			timg = new Image(xyimgname);
			mgd->set_xy_image(timg);
		}
		if(!strcmp(llimgname,"NONE"))  mgd->create_ll_image();
		if(!strcmp(xyimgname,"NONE"))  mgd->create_xy_image();
		((GeoImage *)i)->set_geo_data(mgd);
	} else { fprintf(stderr,"Unknown parameter %s\n",buf); return 0; }

	n=find_char(']');
	} while(n==0);
if(n>0) return 1;
return 0;
}


int MODFile::block_file(Image *i)
{
char	buf[256];

fscanf(fp,"%s",buf);

if(i->read(buf)) {
	fprintf(stderr,"Error reading image file %s\n",buf); return 0; }
return 1;
}


int MODFile::block_black(Image *i)
{
char	buf[256];
int	x,y;

if(find_char('r')>0) {
	fscanf(fp,"%s",buf);
	if(strncmp(buf,"ect",3)) { fprintf(stderr,"Unknown parameter r%s\n",buf); return 0; }
	fscanf(fp,"%d%d",&x,&y);
	*i=new blackRect(x,y);
	}
else	*i=new blackData();
return 1;
}


int MODFile::block_color(Image *i)
{
char	buf[256];
int	x,y,r,g,b;

if(find_char('r')>0) {
	fscanf(fp,"%s",buf);
	if(strncmp(buf,"ect",3)) { fprintf(stderr,"Unknown parameter r%s\n",buf); return 0; }
	fscanf(fp,"%d%d",&x,&y);
	*i=new rgbRect(x,y);
	}
else	*i=new rgbData();
fscanf(fp,"%d%d%d",&r,&g,&b);
i->set_color(0,0,r,g,b);
return 1;
}


int MODFile::block_ramp(Image *i)
{
*i=new XData();
return 1;
}


int MODFile::block_transform(Image *i)
{
Image		*newimg;
double		cx,cy,xx,xy,yx,yy;

newimg=scan_image();
if(!newimg)  {
	fprintf(stderr,"No image block found in transform\n"); return 0; }
if(newimg==(Image *)1) return 0;

fscanf(fp,"%lf%lf%lf%lf%lf%lf",&cx,&cy,&xx,&xy,&yx,&yy);

*i=new transData(newimg,cx,cy,xx,xy,yx,yy);
return 1;
}


int MODFile::block_composite(Image *i)
{
Image		*newimg;
compData	*comp;

comp=new compData();
*i=comp;

while((newimg=scan_image())>(Image *)1) comp->add_image(newimg);
if(newimg==(Image *)1) return 0;
return 1;
}


int MODFile::block_multiband(Image *i)
{
Image		*newimg;
multiBand	*mb;

mb=new multiBand();
*i=mb;

while((newimg=scan_image())>(Image *)1) mb->add_image(newimg);
if(newimg==(Image *)1) return 0;
return 1;
}


int MODFile::block_pyramid(Image *i)
{
Image	*newimg;
pyrData	*p;

p=new pyrData();
*i=p;

while((newimg=scan_image())>(Image *)1) p->add_image(newimg);
if(newimg==(Image *)1) return 0;
return 1;
}


int MODFile::block_fade(Image *i)
{
Image	*newimg;
int	bw, cw=0;

newimg=scan_image();
if(!newimg)  {
	fprintf(stderr,"No image block found in fade\n"); return 0; }
if(newimg==(Image *)1) return 0;

fscanf(fp,"%d %d",&bw, &cw);

*i=new fadeBorder(newimg,bw,cw);
return 1;
}


int MODFile::block_crop(Image *i)
{
Image		*newimg;
double		x1,y1,x2,y2;

newimg=scan_image();
if(!newimg)  {
	fprintf(stderr,"No image block found in crop\n"); return 0; }
if(newimg==(Image *)1) return 0;

fscanf(fp,"%lf%lf%lf%lf",&x1,&y1,&x2,&y2);

*i=new cropData(newimg,x1,y1,x2,y2);
return 1;
}


int MODFile::block_pyrpix(Image *i)
{
Image		*newimg;

newimg=scan_image();
if(!newimg)  {
	fprintf(stderr,"No image block found in pyrpix\n"); return 0; }
if(newimg==(Image *)1) return 0;

*i=new pyrpixData(newimg);
return 1;
}

