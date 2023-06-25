// poly_object_1.h

#ifndef	_POLY_OBJECT_1_H_
#define _POLY_OBJECT_1_H_

#include <X11/Intrinsic.h>

#include "grape/object.h"
#include "image/image.h"
#include "image/datatypes.h"
#include "image/filetypes.h"

#ifdef DO_GL
#include <gl/gl.h>
#include <gl/device.h>
#endif

#define PERROR(txt) { fprintf( stderr, "error: %s:%d : %s\n", \
						 __FILE__, __LINE__, txt);}

#define	POLY_NUM_OBJ_TYPES		4

#if 0
#define WIREFRAME		0
#define SMOOTH_SHADED		1
#define TEXTURE_MAPPED		2
#else
enum PolyObjectType { 
	BadPolyObj = -1,
	WireFrame, /* this must be 0, or nasty things will occur */
	SmoothShaded,
	TextureMapped,
	FlatShaded
};
#endif

#if 0
extern	char	*object_type_master_list[POLY_NUM_OBJ_TYPES];
#endif

// The class for a polygonal object
// 
class PolyObject1 : public Obj
{
 private:

 protected:

	int	hidden;
#if 0
	int	object_type;
#else
	PolyObjectType object_type;
#endif

	char	*version;
	char	*file_type;
	char	*group_name;
	char    *pTextureName;		/* Name of texture Image file, if any */
	Image   *pTextureImg;		/* Texture Image, if any */

	ldparam	r, g, b;			/* parallel lists of color values */

				/* parallel lists of vertex coords */
	ldparam		x_vert,
				y_vert,
				z_vert; 

	liparam	point_colors;	/* list of indeces of vertex colors */
				/* NOTE: 1-to-1 with x_vert, y_vert, z_vert */
				/* NOTE: indexes into r,g,b */

	liparam	point_normals;	/* list of indeces of vertex normals */
				/* NOTE: 1-to-1 with x_vert, y_vert, z_vert*/
				/* NOTE: indexes into x_norm,y_norm,z_norm */

	lliparam	vrtx_poly_users; /* list of lists of indeces of polys that */
								 /* use each vertex */
				/* NOTE: 1-to-1 with x_vert, y_vert, z_vert*/
				/* NOTE: each contained list indexes into poly_verts, the */
				/* polys that use a vertex */

				/* parallel lists of normal vectors */
	ldparam		x_norm,
				y_norm,
				z_norm; 

				/* list of indeces of edge vertices */
	liparam		edge_start,
				edge_end;

	liparam		edge_colors; /* list of indeces of edge colors */

	lliparam	poly_verts; /* list of lists of indeces of poly verts */
	liparam		poly_colors; /* list of indeces of poly colors */
	liparam		poly_normals; /* list of indeces of poly normals */

	lliparam    text_verts;		/* list of lists of indeces of texture */
								/* vertices */
//	liparam text_verts;			/* NOTE: 1-to-1 with x_vert, y_vert, z_vert */
//								/* indexes into x_text, y_text, z_text */
	ldparam x_text, y_text, z_text; /* texture coordinate list */
	

#ifdef DO_GL
	GL_Object	globject;
#endif

	void	init(void);

			   /* create polygon normals */
	int generate_poly_normals( void);

				/* create vertex normals */
	int generate_vertex_normals( void);

 public:

// do not display this object if hidden is true
	void set_hidden(Boolean tf) { hidden = tf; } 
	Boolean get_hidden() { return hidden; }

	virtual int	get_changed();

	virtual int	get_type(void)	{ return(POLY_V1); }

	PolyObjectType get_obj_model_type(void) {
		return object_type;
	}
		

			/*****************************************/
			/* these operate on the raw data that */
			/* holds the actual values */
			/*****************************************/

				/* get the number of points in the object */
	int	get_num_points(void);

				/* get (x,y,z) coords and color index of i-th */
	int	get_point(int i, int &c, double &x, double &y, double &z);
	int	get_point(int i, int &c, float &x, float &y, float &z);

	// set point of the ith vertex and color
	void    set_point(int inx, int c, double x, double y, double z);

				/* get number of edges */
	int	get_num_edges(void);

				/* get color index, start/end vertex indeces */
	int	get_edge(int i, int &c, int &s, int &e);

				/* number of colors in object */
	int	get_num_colors(void);

				/* get rbg values for the i-th color */
	int	get_color(int i, double &r, double &g, double &b);
	int	get_color(int i, float &r, float &g, float &b);

				/* number of normal vectors in object */
	int	get_num_normal_vects(void);

				/* get i-th vector */
	int	get_normal( int i, double &ni, double &nj, double &nk);

			/*****************************************/
			/* these operate on the polygonal object */
			/*****************************************/

	int	get_num_faces(void);	/* num polys in object */
	int	get_num_verts(int i);	/* vertices in i-th poly */

				/* index of the vert_ndx-th vertex in the */
				/* poly_ndx-th polygon */
	int	get_vert(int poly_ndx, int vert_ndx);

				/* get color index of face_index-th poly */
	int	get_pcolor(int face_index); 

				/* get normal index of pt_ndx-th point */
				/* use the result and call get_normal() for the actual vector */
	int	get_pt_normal( int pt_ndx); 

				/* get normal index of the poly_ndx-th polygon */
	int	get_poly_normal( int poly_ndx);


	int build_normal_structure(void); /* alloc space for normals, do not calc */
	int generate_normals( void); /* calc them: 1 if success, 0 otherwise */

	PolyObjectType poly_object_from_string(char *type);

	void dump();				/* dump obj's contents to stdout (debugging) */

#ifdef DO_GL
	void	set_globject(GL_Object g) { globject = g; }
	GL_Object	get_globject(void) { return(globject); }
#endif

	void set_texture_name( char *p) { 
		if (pTextureName)
			free( pTextureName);
		if ((pTextureName = (char *)malloc( strlen(p) + 1)) == NULL) {
			PERROR( "Unable to malloc pTextureName");
		}
		strcpy( pTextureName, p);
	}
	char *get_texture_name( void) { return pTextureName;}

	void set_texture_image( Image *p) {
		if (pTextureImg != NULL)
			delete pTextureImg;	/* get rid of old image */
		pTextureImg = p;		/* set the new image */
	}
	Image *get_texture_image(void) { return pTextureImg; }
	int load_texture_image(void);
	int load_texture_image( char *p) {
		set_texture_name( p);
		return load_texture_image();
	}

	int get_texel_ndx( int poly, int vrtx_ndx);
	int	get_texel(int i, double &x, double &y, double &z);

	virtual int	parse_in(Dataport *fp);
	virtual int	parse_out(Dataport *fp, int expand=FALSE);

	PolyObject1(void) 
	{
		version = NULL;
		file_type = NULL;
		group_name = NULL;
		object_type = WireFrame;
		hidden = FALSE;
		pTextureName = NULL;
		pTextureImg = NULL;
		init();
	} /* PolyObject1 */

	~PolyObject1(void) {
		if(version)free(version);
		if(file_type)free(file_type);
		if(group_name)free(group_name);
		if (pTextureName) free(pTextureName);
		if (pTextureImg) delete pTextureImg;
	}

};								/* Class */

class PolyTerrainObj : public PolyObject1 {

 protected:

	int xfactor, yfactor;
	double zscale;

	double sphere_radius, z_exaggeration;	// used only for spherical mode

 public:

	virtual int	get_type(void)	{ return( POLY_TERRAIN_V1); }

	void set_mesh_factor( int _x, int _y) { xfactor = _x; yfactor = _y; set_changed();}
	void set_mesh_factor_x( int _x) { xfactor = _x; set_changed();};
	void set_mesh_factor_y( int _y) { yfactor = _y; set_changed();};
	void get_mesh_factor( int &_x, int &_y) { _x = xfactor; _y = yfactor; }

	void set_zscale( double _z) { zscale = _z; set_changed();}
	double get_zscale( void) { return zscale; }

								/* this takes a tmesh object and an x and y */
								/* meshing factor and fills the tmesh object */
								/* with the mesh according to the elevation */
								/* surface texture map */
	int gen_tmesh( void);

	// returns 1 if successful -1 on error or 0 if already created
	int make_spherical(double radius, double zexag=1.0);
	// returns 1 if successful -1 on error or 0 if already flat
	int make_flat();

    virtual int parse_in(Dataport *fp);
	virtual int parse_out(Dataport *fp, int expand=FALSE);
	

	PolyTerrainObj() {
		xfactor = yfactor = 1;
		zscale = 1.0;
		sphere_radius = -1, z_exaggeration = -1;
	}
	~PolyTerrainObj() {;}

};

PolyTerrainObj *CreatePolyTerrainObj(Image *terrain, Image *elevation, 
		int xfactor=128, int yfactor=128, double xscale=1.0, 
		double yscale=1.0, double zscale=1.0);


class PolySphereObj : public PolyObject1 {

 private:
	double radius;
	int lon_res, lat_res;	// in degrees

 public:

	virtual int	get_type(void)	{ return( POLY_V1); }

	void set_radius(double rad) { radius = rad; }
	double get_radius() { return radius; }

	void set_ll_res( int lat, int lon) { lat_res = lat; lon_res = lon; 
					set_changed();}
	void get_ll_res( int &lat, int &lon) { lat = lat_res; lon = lon_res; }

	void set_lon_res( int lon) { lon_res = lon; set_changed();};
	void set_lat_res( int lat) { lat_res = lat; set_changed();};

	int gen_sphere();

    	// virtual int parse_in(Dataport *fp) { return 0; }
	// virtual int parse_out(Dataport *fp, int expand=FALSE) { return 0; }
	
	PolySphereObj() {
		lon_res = lat_res = 10; radius = 1.0;
		x = y = z = xrot = yrot = zrot = 0.0;
		xscale = yscale = zscale = 1.0;
	}

	PolySphereObj(int lat, int lon, double rad) {
		lon_res = lon; lat_res = lat; radius = rad;
		x = y = z = xrot = yrot = zrot = 0.0;
		xscale = yscale = zscale = 1.0;
		gen_sphere();
	}
	~PolySphereObj() {}

};
#endif
