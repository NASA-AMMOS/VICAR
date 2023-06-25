#ifndef	_3DOBJECT_H_
#define _3DOBJECT_H_
// object.h 1.35 03/04/10 09:27:23
/** \file
 ** Definitions for Obj, ObjNode, and GroupObj 3D Object classes
 **/

#ifndef TRUE
#define	TRUE	1
#define FALSE	0
#endif

#include <string.h>
#include "dataport.h"
#include "image/geodata.h"
#include "grape/object_types.h"
#include "grape/matrix.h"
#include "grape/parameter.h"
#include "grape/surface.h"
#include "grape/range.h"

int     object_file_num_from_type(char *type);

/// The base class for all 3D objects.
/**
 ** Primarily defining a model to object transformation
 **/
class Obj {

 protected:
	int	id;
	char	*name;
	int	changed;

	double (*obj2mdl)[DIM_ZMAT]; 	///< cached transform matrices
	double (*norm2mdl)[DIM_ZMAT];
	
	/// Initialize/reset
	void	init() { changed=CHANGE_COUNTER; name=NULL;
			 x = y = z = 0.0;
			 xrot = yrot = zrot = 0.0;
			 xscale = yscale = zscale = 1.0;
			 obj2mdl = norm2mdl = NULL;
			 }

 public:
	dparam	x,y,z;		///< model to object translation
	dparam	xrot,yrot,zrot;	///< model to object rotation in degrees
	dparam	xscale,yscale,zscale;	/// < model to object scaling

	/// Surface properties of the object (i.e. texture mapping info)
	GrapeSurface surface;

	/// Get object type
	virtual int	get_type(void)  { return(BASE_OBJ); }

	/// Compute and cache object-to-model transforms.
	/**
	// This speeds up data input. Calling this function means 
	// promising not to change transform components until the 
	// next call to unfreeze_xform().
	*/
	void freeze_xform();

	/// Invalidate cached transforms
	void unfreeze_xform();

	/// Compute (or get cached) model to object transformation
	void GetModelToObjectTransform( ZMatrix InOut);

	/// Compute (or get cached) transform from object to model space
	void GetObjectToModelTransform(ZMatrix InOut);

	/// Compute (or get cached) normal object to model transform
	void GetNormalToModelTransform(ZMatrix InOut);

	void	get_look_vector(double &a, double &b, double &c);
	void	get_up_vector(double &a, double &b, double &c);
	void	get_rpy(double &a, double &b, double &c) {
		a = xrot.get_value();
		b = yrot.get_value();
		c = zrot.get_value();
	}

	/// Clip data to specifed volume in world frame
	// default is to do nothing
	virtual void clip(Range *volume, ZMatrix m2w) { }

	void	set_id(int n) { id=n; }		// should this be automatic?
	int	get_id() { return id; }

	void	set_name(char *n) {
		if(name)free(name);
		name = n ? strdup(n) : NULL;
	}
	char	*get_name() { return name; }
	virtual	void	set_changed() { changed=CHANGE_COUNTER; }
	virtual int	get_changed();

	virtual	int	is_camera(void) { return(FALSE); }
	virtual	int	is_light(void) { return(FALSE); }

	/// Load from file
	virtual	int	parse_in(Dataport *fp);

	/// Store to file
	virtual	int	parse_out(Dataport *fp, int expand=FALSE);

	/// Assignment operator, avoid trouble with allocated members
	Obj& operator=(const Obj& other);

	/// Constructors
	Obj() { init(); }
	Obj(const Obj& other) { *this = other; }
	
	/// Destructor
	virtual ~Obj(void) {
		if(name)free(name);
		unfreeze_xform();
	}
};

/// Object node class
/**
 ** This is the class for placing objects in a scene,
 ** primarily defining an object to world transformation
 ** and georeferencing.
 **/
class ObjNode {

 protected:

	Obj	*obj;
	char	*name;
	char	*reference;

	GeoData *geo_data;      ///< pointer to georeferencing data

	int	changed;
	int	object_changed;

	/// Initialize/reset
	void	init() { changed=CHANGE_COUNTER; object_changed=CHANGE_COUNTER;
			 obj=NULL; name=NULL; reference=NULL; geo_data = NULL;
			 x = y = z = 0.0;
			 xrot = yrot = zrot = 0.0;
			 xscale = yscale = zscale = 1.0;
			}

 public:

	dparam	x,y,z;		///< object to world 
	dparam	xrot,yrot,zrot;	///< object to world rotation in degrees
	dparam	xscale,yscale,zscale;	/// < object to world scaling

	/// Get object to world transformation matrix
	void    GetObjToWorldTransform( ZMatrix InOut);

	/// Get full model-to-world transformation matrix
	void    GetTransformationMatrix( ZMatrix InOut);

	void    get_object_pos( double &x, double &y, double &z);
	void	get_object_coords( double &lat, double &lng, double &elev) {
			double la, lb, lc;
			get_object_pos(la,lb,lc);
			xyztolle( la, lb, lc, lat, lng, elev);
		}

	void    get_lookat_ref_point( double &a, double &b, double &c);
	void	get_look_vector(double &a, double &b, double &c);

	void    get_up_vector_ref_point( double &a, double &b, double &c);
	void	get_up_vector(double &a, double &b, double &c);

	void	get_rpy(double &a, double &b, double &c) {
		a = xrot.get_value();
		b = yrot.get_value();
		c = zrot.get_value();
	}

	/// Clip my object to specified volume in world frame
	void clip_object(Range *world_volume) {
		if (obj) {
			ZMatrix m2w;	// model to world transform
			GetTransformationMatrix(m2w);
			obj->clip(world_volume, m2w);
			set_object_changed();
		}
	}

	void	release_geo_data(void) {
			geo_data = NULL;
		}

	void	free_geo_data(void) {
			delete geo_data; geo_data = NULL;
		}

	void	set_geo_data(GeoData *gd) {
			delete geo_data; geo_data = gd;
		}
	GeoData	*get_geo_data(void) { return(geo_data); }
	operator GeoData*() { return(get_geo_data()); }
	void	operator=(GeoData *d) { set_geo_data(d); }

	/// coordinate space converters (virtuals)
        virtual double  z_to_elev(double _z) {
			if(get_geo_data())
				return(get_geo_data()->z_to_elev(_z));
			return(0.0);
		}
        virtual double  elev_to_z(double el) {
			if(get_geo_data())
				return(get_geo_data()->elev_to_z(el));
			return(1.0);
		}

	virtual void    xyztolle(double _x, double _y, double _z, double &lat, double &lng, double &el) {
			if(get_geo_data()) {
				get_geo_data()->xyztolle(_x,_y,_z,lat,lng,el);
			}
		}
	virtual void    lletoxyz(double lat, double lng, double el, double &_x, double &_y, double &_z) {
			if(get_geo_data()) {
				get_geo_data()->lletoxyz(lat,lng,el,_x,_y,_z);
			}
		}

	virtual	double	lltox(double lat, double lng) { 
			if(get_geo_data())
				return(get_geo_data()->lltox(lat, lng));
			return(0.0);
		}
	virtual	double	lltoy(double lat, double lng) { 
			if(get_geo_data())
				return(get_geo_data()->lltoy(lat, lng));
			return(0.0);
		}
	virtual	void	lltoxy(double latitude, double longitude, double &_x, double&_y) {
			_x = lltox(latitude, longitude);
			_y = lltoy(latitude, longitude);
		}
	virtual	double	utmtox(double easting, double northing) {
			if(get_geo_data())
				return(get_geo_data()->utmtox(easting, northing));
			return(0.0);
		}
	virtual	double	utmtoy(double easting, double northing) {
			if(get_geo_data())
				return(get_geo_data()->utmtoy(easting, northing));
			return(0.0);
		}
	virtual	void	utmtoxy(double easting, double northing, double &_x, double&_y) {
			_x = utmtox(easting, northing);
			_y = utmtoy(easting, northing);
		}
	virtual	double	utmtolat(double easting, double northing) { 
			if(get_geo_data())
				return(get_geo_data()->utmtolat(easting, northing));
			return(0.0);
		}
	virtual	double	utmtolong(double easting, double northing) { 
			if(get_geo_data())
				return(get_geo_data()->utmtolong(easting, northing));
			return(0.0);
		}
			
	virtual	void	utmtoll(double easting, double northing, double &latitude, double &longitude) {
			latitude = utmtolat(easting, northing);
			longitude = utmtolong(easting, northing);
		}
	virtual	double	lltoe(double lat, double lng) { 
			if(get_geo_data())
				return(get_geo_data()->lltoe(lat, lng));
			return(0.0);
		}
	virtual	double	llton(double lat, double lng) { 
			if(get_geo_data())
				return(get_geo_data()->llton(lat, lng));
			return(0.0);
		}
	virtual	void	lltoutm(double latitude, double longitude, double &easting, double &northing) {
			easting = lltoe(latitude, longitude);
			northing = llton(latitude, longitude);
		}

	virtual	double	xytolat(double _x, double _y) { 
			if(get_geo_data()) 
				return(get_geo_data()->xytolat(_x, _y));
			return(0.0);
		}
	virtual	double	xytolong(double _x, double _y) { 
			if(get_geo_data())
				return(get_geo_data()->xytolong(_x, _y));
			return(0.0);
		}
	virtual	void	xytoll(double _x, double _y, double &latitude, double &longitude) {
			latitude = xytolat(_x, _y);
			longitude = xytolong(_x, _y);
		}
			
	virtual	double	xytoe(double _x, double _y) { 
			if(get_geo_data())
				return(get_geo_data()->xytoe(_x, _y));
			return(0.0);
		}
	virtual	double	xyton(double _x, double _y) { 
			if(get_geo_data())
				return(get_geo_data()->xyton(_x, _y));
			return(0.0);
		}
	virtual	void	xytoutm(double _x, double _y, double &e, double &n) {
			e = xytoe(_x, _y);
			n = xyton(x, y);
		}

	/// projection and datum types - danp
	virtual ProjectionType get_projection(void) { 
		if (get_geo_data()) return get_geo_data()->get_projection();
		return(UNKNOWN);
	}

	virtual DatumType get_datum(void) { 
		if (get_geo_data()) return get_geo_data()->get_datum();
		return(UNKNOWN);
	}

	void	set_object(Obj *o) { obj=o; object_changed=CHANGE_COUNTER; }
	void	set_name(char *nm) { 
			if(name)free(name); 
			name = nm ? strdup(nm) : NULL; }
	char	*get_name(void) { return(name); }
	void	set_reference(char *nm) { 
			if(reference)free(reference); 
			reference = nm ? strdup(nm) : NULL; }
	char	*get_reference(void) { return(reference); }

	Obj	*get_object() { return obj; }

	void	set_changed() { changed=CHANGE_COUNTER; }
	int	get_changed() { register int c;
			if(changed!=CHANGE_COUNTER) {
				if((c=x.get_changed()) > changed) changed=c;
				if((c=y.get_changed()) > changed) changed=c;
				if((c=z.get_changed()) > changed) changed=c;
				if((c=xrot.get_changed()) > changed) changed=c;
				if((c=yrot.get_changed()) > changed) changed=c;
				if((c=zrot.get_changed()) > changed) changed=c;
				if((c=xscale.get_changed()) > changed) changed=c;
				if((c=yscale.get_changed()) > changed) changed=c;
				if((c=zscale.get_changed()) > changed) changed=c; }
			return changed; }

	void	set_object_changed() { object_changed=CHANGE_COUNTER; }
	int	get_object_changed() { register int c;
			if(object_changed!=CHANGE_COUNTER && obj)
				if((c=obj->get_changed()) > object_changed)
					object_changed=c;
			return object_changed; }

	/// Load from file, optionally loading referenced object
	virtual int	parse_in(Dataport *fp, int expand=TRUE);

	/// Delayed load of referenced object
	virtual int	parse_reference(Dataport *fp);

	/// Save to file
	virtual int	parse_out(Dataport *fp, int expand=FALSE);

	/// Assignment operator, avoid trouble with allocated members
	ObjNode& operator=(const ObjNode& other);

	/// Constructors
	ObjNode() { init(); }
	ObjNode(const ObjNode& other) { *this = other; }

	/// Destructor
	virtual ~ObjNode(void) {
		if(name)free(name);
		if(reference)free(reference);
	}
};

/// Hierarchical scene graph node
/**
 ** Really, just a simple list
 **/
class GroupObj : public Obj {

    protected:

	ObjNode	**children;	///< allocated array 

	int	num_children;

	void	init() { children=NULL; num_children=0; }

	int	parse_start(Dataport *fp);

    public:

	// these return TRUE if successful

	/// Add child node to the list
	// WARNING: child nodes must be dynamically allocated and
	// point to dynamically allocated Obj's, as these are deleted
	// in the GroupObj destructor!
	int	add_child(ObjNode *on, int pos=-1);

	/// remove children from the list, optionally deleting them
	// (both the node and its contained object)
	int	remove_child(ObjNode *on, int delete_it=TRUE);
	int	remove_child(int n, int delete_it=TRUE);

	ObjNode	*get_child(int n) {
			if (n >= num_children)
				return NULL;
			return children[n];
		}

	int	is_child(ObjNode *on);

	int	get_num_children() { return num_children; }

	/// Load from file, optionally deferring ObjNode data loading
	virtual int parse_in(Dataport *fp, int expand);

	/// Load from file, load all data (overrides Obj virtual method)
	virtual int parse_in(Dataport *fp) { return parse_in(fp, TRUE); }

	/// Save to file
	virtual int parse_out(Dataport *fp, int expand=FALSE);

	/// Constructor
	GroupObj() { init(); }

	/// Destructor, deletes the child nodes and their objects too
	~GroupObj(void) {
		for (int i=0; i<num_children; i++) {
			delete(children[i]->get_object());
			delete(children[i]);
		}
		free(children);
	}

};

Obj	*object_creator(Dataport *fp);

#endif
