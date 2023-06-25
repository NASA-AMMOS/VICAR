////////////////////////////////////////////////////////////////////////
// FilterShapes
//
// Define the various kinds of filters and how to read and process them.
////////////////////////////////////////////////////////////////////////
#ifndef FILTERSHAPES_H
#define FILTERSHAPES_H

#include "PigXerces.h"
#include "PigModelBase.h"
#include "PigQuaternion.h"

#include <stdio.h>

class PigFileModel;
class PigCameraModel;
class PigCoordSystem;
class PigMission;

#define PIG_MAX_FILTER_PARAMS 10

typedef enum {
    FilterTypeBase,
    FilterTypeImagePolygon,
    FilterTypeProjectedPolygon,
    FilterTypeProjectedHorizon,
    FilterTypeVolumeBox,
    FilterTypeVolumeZCylinder,
    FilterTypeVolumeYWasher
} FilterType;

////////////////////////////////////////////////////////////////////////
// Base class.  readFromXML() returns 0 on success, 1 on some kind of error.
////////////////////////////////////////////////////////////////////////

class FilterShape {
  protected:
    PigFileModel *_file;
    PigCameraModel *_camera;
    PigCoordSystem *_cs, *_site_cs;
    double *_xyz[3];
    int _nl, _ns;
    double _params[PIG_MAX_FILTER_PARAMS];
    double _min_fov;
    char *_id;

    static int _do_print;

    // Simple cache for min_fov calculations
    static int _cache_valid;
    static PigFileModel *_cached_file;
    static PigCameraModel *_cached_camera;
    static double _cached_min_fov;

  public:
    FilterShape() { _file = NULL; _camera = NULL; _xyz[0]=_xyz[1]=_xyz[2]=NULL;
			_nl=0; _ns=0, _id = NULL; ;
		for (int i=0; i < PIG_MAX_FILTER_PARAMS; i++)
		    _params[i] = 0.0;
		}
    virtual ~FilterShape() { if (_id != NULL) delete _id;};

    virtual int readFromXML(DOMElement *element) = 0;
    virtual void print() = 0;
    static void enablePrint(int enable) { _do_print = enable; }

    virtual FilterType getFilterType() { return FilterTypeBase; }

    // Note that xyz can be passed as NULL, which indicates there is no
    // XYZ data available.  Any volume-based shapes will be quietly ignored.
    // Params is a pointer to the params array, which is copied internally.
    // It can be null, in which case all parameters are 0.
    virtual void setSourceImage(PigFileModel *file, PigCameraModel *camera,
		PigCoordSystem *cs, PigCoordSystem *site_cs, double *xyz[3],
		int nl, int ns, double *params);

    virtual void addFilterToMask(unsigned char *mask, int nlo, int nso,
							int dn=255) { }

    // Determine if the given image point is in the mask or not.
    // Note that this applies only to image-based masks; volume (xyz) masks
    // are not supported and just return FALSE (not masked).

    virtual int isPointFiltered(int line, int samp) { return FALSE; }

    // Set the ID.  ID is used only for docuemntation.  This could be a
    // memory leak if the ID is reset, but that shouldn't be done.

    virtual void setId(char *id) { if (id == NULL) _id = NULL;
				    else _id=strdup(id); }
    virtual char *getId() { return _id; }

    // The filename actually used is returned in out_filename (which must
    // be preallocated, or NULL if you don't want the filename).

    static int readFilters(PigMission *m, const char *filename,
		const char *basename, const char *instrument,
		FilterShape **&filters, int &num_filters,
		char *out_filename, int rmc[], int num_rmc);
};

////////////////////////////////////////////////////////////////////////
// Utility classes...
////////////////////////////////////////////////////////////////////////

class Point2D {
  public:
    double _x, _y;

    Point2D() { _x = 0; _y = 0; }

    int readFromXML(DOMElement *element);
};

class Point3D {
  public:
    double _x, _y, _z;

    Point3D() { _x = 0; _y = 0; _z = 0; }

    int readFromXML(DOMElement *element);
};

class FilterRotate {
  public:
    int _use_rotate;			// True if non-0 rotation
    double _x, _y, _z;
    double _axis_x, _axis_y, _axis_z;
    double _angle;
    int _add_angle;

    PigQuaternion _quat;		// set by computeRotation()
    PigVector _origin;			// ditto

    FilterRotate() { clear(); }
    void clear() { _x = _y = _z = _axis_x = _axis_y = _axis_z = _angle = 0.0;
		   _add_angle = 0;
		   _use_rotate = FALSE; };

    int readFromXML(DOMElement *element);

    void computeRotation(double *params);
    void Rotate(double &x, double &y, double &z);  // modifies args in-place
    void print();
};

////////////////////////////////////////////////////////////////////////
// Polygon in image space.  Should be a convex polygon but others may work
// with the implementation.
//
// XML looks like:
//
//    <image shape="polygon"> 
//      <vertex x="1.0" y="2.0" />
//      <vertex x="2.0" y="3.0" />
//      ...
//    </image>
//
////////////////////////////////////////////////////////////////////////

class FilterShapeImagePolygon : public FilterShape {
  protected:
    int _nVertices;
    Point2D *_vertices;

    int pointInPoly(int x, int y);

  public:
    FilterShapeImagePolygon() { _nVertices = 0; _vertices = NULL; }
    virtual ~FilterShapeImagePolygon() { if (_vertices) delete _vertices; }

    virtual int readFromXML(DOMElement *element);
    virtual void print();
    virtual FilterType getFilterType() { return FilterTypeImagePolygon; }
    virtual void addFilterToMask(unsigned char *mask, int nlo, int nso,
								int dn=255);
    virtual void addFilterToMask2(unsigned char *mask, int nlo, int nso,
								int dn=255);
    virtual int isPointFiltered(int line, int samp);

    virtual void setPolygon(int n, Point2D *vertices);
    virtual void convertToImageSpace();
};

////////////////////////////////////////////////////////////////////////
// Polygon in 3-D space, projected into image space.  This is just a
// generalization of the old FilterShapeProjectedTriangle, which has been
// removed.  For backward compatibility, the parser will accept either
// "triangle" or "polygon" as the shape name.
//
// XML looks like:
//
//    <projected shape="polygon">
//      <vertex x="1.0" y="2.0" z="3.0" />
//      <vertex x="2.0" y="3.0" z="4.0" />
//      <vertex x="3.0" y="4.0" z="5.0" />
//      <vertex x="4.0" y="5.0" z="6.0" />
//      ...
//    </projected>
//
////////////////////////////////////////////////////////////////////////

class FilterShapeProjectedPolygon : public FilterShape {
  protected:
    int _nVertices;
    Point3D *_vertices;
    FilterShapeImagePolygon _projected_poly;
    int _need_projection;
    int _projection_good;
    virtual void projectPoly();

  public:
    FilterShapeProjectedPolygon()
		{ _need_projection = TRUE; _projection_good = FALSE; }
    virtual ~FilterShapeProjectedPolygon() { }

    virtual int readFromXML(DOMElement *element);
    virtual void print();
    virtual FilterType getFilterType() { return FilterTypeProjectedPolygon; }
    virtual void addFilterToMask(unsigned char *mask, int nlo, int nso,
								int dn=255);
    virtual int isPointFiltered(int line, int samp);
};

////////////////////////////////////////////////////////////////////////
// Horizon in 3-D space, projected into image space.  Specified via an
// elevation maximum (in degrees) above which everything is masked.
//
// XML looks like:
//
//    <projected shape="horizon">
//      <max elevation="-3.0" />
//    </projected>
//
////////////////////////////////////////////////////////////////////////

class FilterShapeProjectedHorizon : public FilterShape {
  protected:
    double _elevation;		// in radians... file is in degrees

  public:
    FilterShapeProjectedHorizon() { }
    virtual ~FilterShapeProjectedHorizon() { }

    virtual int readFromXML(DOMElement *element);
    virtual void print();
    virtual FilterType getFilterType() { return FilterTypeProjectedHorizon; }
    virtual void addFilterToMask(unsigned char *mask, int nlo, int nso,
								int dn=255);
    virtual int isPointFiltered(int line, int samp);

    double getElevation() { return _elevation; }	// in radians!
};

////////////////////////////////////////////////////////////////////////
// Box in 3-D space, aligned with axes
//
// XML looks like:
//
//    <volume shape="box">
//      <min x="2.0" y="4.0" z="0.0" />
//      <max x="4.0" y="8.0" z="1.0" />
//    </volume>
//
////////////////////////////////////////////////////////////////////////

class FilterShapeVolumeBox : public FilterShape {
  protected:
    Point3D _min, _max;
    FilterRotate _rotate;

  public:
    FilterShapeVolumeBox() { }
    virtual ~FilterShapeVolumeBox() { }

    virtual int readFromXML(DOMElement *element);
    virtual void print();
    virtual FilterType getFilterType() { return FilterTypeVolumeBox; }
    virtual void addFilterToMask(unsigned char *mask, int nlo, int nso,
								int dn=255);
};

////////////////////////////////////////////////////////////////////////
// Cylinder in 3-D space, aligned with Z axis
//
// XML looks like:
//
//    <volume shape="z_cylinder">
//      <axis x="1.0" y="2.0" />
//      <min z="3.0" />
//      <max z="4.0" radius="5.0" />
//    </volume>
//
////////////////////////////////////////////////////////////////////////

class FilterShapeVolumeZCylinder : public FilterShape {
  protected:
    double _axis_x, _axis_y;
    double _min_z, _max_z;
    double _radius;
    FilterRotate _rotate;

  public:
    FilterShapeVolumeZCylinder() { }
    virtual ~FilterShapeVolumeZCylinder() { }

    virtual int readFromXML(DOMElement *element);
    virtual void print();
    virtual FilterType getFilterType() { return FilterTypeVolumeZCylinder; }
    virtual void addFilterToMask(unsigned char *mask, int nlo, int nso,
								int dn=255);
};

////////////////////////////////////////////////////////////////////////
// Washer-shaped area in 3-D space.  Washer axis aligned with Y axis.
// Another way to look at it is a cylinder with a smaller cylinder inside
// excluded.
//
// XML looks like:
//
//    <volume shape="y_washer">
//      <axis x="1.0" z="2.0" />
//      <min y="3.0" radius="4.0" />
//      <max y="5.0" radius="6.0" />
//    </volume>
//
////////////////////////////////////////////////////////////////////////

class FilterShapeVolumeYWasher : public FilterShape {
  protected:
    double _axis_x, _axis_z;
    double _min_y, _max_y;
    double _min_radius, _max_radius;
    FilterRotate _rotate;

  public:
    FilterShapeVolumeYWasher() { }
    virtual ~FilterShapeVolumeYWasher() { }

    virtual int readFromXML(DOMElement *element);
    virtual void print();
    virtual FilterType getFilterType() { return FilterTypeVolumeYWasher; }
    virtual void addFilterToMask(unsigned char *mask, int nlo, int nso,
								int dn=255);
};

#endif

