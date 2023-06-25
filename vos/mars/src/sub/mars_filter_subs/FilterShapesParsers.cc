////////////////////////////////////////////////////////////////////////
// FilterShapesParsers
//
// Read in the shapes from the XML file.  Taken out of FilterShapes.cc
// because it's a lot of boring, repetitive parsing code.
//
// Also, read the entire filter file.
////////////////////////////////////////////////////////////////////////

#include "FilterShapes.h"
#include "PigVector.h"
#include "PigMission.h"

#include "zvproto.h"

#include <stdlib.h>
#include <string.h>

// Static values for caching of the filter document

static DOMDocument *cached_doc = NULL;
static char cached_filename[PIG_MAX_FILENAME_SIZE];

////////////////////////////////////////////////////////////////////////
// Macro to read a single Real element from XML
////////////////////////////////////////////////////////////////////////
#define RealAttr(el, attr, name, result)				\
  {									\
    result = PigXerces::getAttributeDouble(el, attr, -9.999e-30);	\
    if (result == -9.999e-30) {						\
	char msg[256];							\
        sprintf(msg, "Unable to find %s in %s element", attr, name);	\
        zvmessage(msg, "");						\
	result = 0.0;							\
	return 1;							\
    }									\
  }


////////////////////////////////////////////////////////////////////////
// Read a Point2D from XML
////////////////////////////////////////////////////////////////////////

int Point2D::readFromXML(DOMElement *element)
{
    char *str;
    _x = _y = 0.0;

    RealAttr(element, "x", "2D vertex", _x);
    RealAttr(element, "y", "2D vertex", _y);

    return 0;
}

////////////////////////////////////////////////////////////////////////
// Read a Point3D from XML
////////////////////////////////////////////////////////////////////////

int Point3D::readFromXML(DOMElement *element)
{
    char *str;
    _x = _y = _z = 0.0;

    RealAttr(element, "x", "3D vertex", _x);
    RealAttr(element, "y", "3D vertex", _y);
    RealAttr(element, "z", "3D vertex", _z);

    return 0;
}

////////////////////////////////////////////////////////////////////////
// Read a Rotate from XML
////////////////////////////////////////////////////////////////////////

int FilterRotate::readFromXML(DOMElement *element)
{
    char *str;
    clear();

    RealAttr(element, "x", "Rotate", _x);
    RealAttr(element, "y", "Rotate", _y);
    RealAttr(element, "z", "Rotate", _z);
    RealAttr(element, "axis_x", "Rotate", _axis_x);
    RealAttr(element, "axis_y", "Rotate", _axis_y);
    RealAttr(element, "axis_z", "Rotate", _axis_z);
    RealAttr(element, "angle", "Rotate", _angle);

    _add_angle = PigXerces::getAttributeInt(element, "add_angle", 0);

    _use_rotate = TRUE;

    return 0;
}

void FilterRotate::print()
{
    char msg[256];
    if (!_use_rotate)
	return;

    sprintf(msg,
	"Rotate origin=(%f, %f, %f), axis=(%f, %f, %f), angle=%f, param=%d",
	_x, _y, _z, _axis_x, _axis_y, _axis_z, _angle, _add_angle);
    zvmessage(msg, "");
}

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

int FilterShapeImagePolygon::readFromXML(DOMElement *element)
{
    // Find all <vertex> entries

    DOMNodeList *entries = PigXerces::getElementsByTagName(element, "vertex");

    int n = entries->getLength();
    if (n == 0) {
	zvmessage("Warning: no vertex entries in image polygon", "");
	return 1;				// nothing to do...
    }

    _vertices = new Point2D[n];
    if (_vertices == NULL) {
	zvmessage("Out of memory!  reading polygons", "");
	return 1;
    }

    for (int i=0; i < n; i++) {
	DOMElement *vertex = PigXerces::nextElement(entries, i);
	int status = _vertices[i].readFromXML(vertex);
	if (status != 0) {
	    zvmessage("parse failure in reading image polygon", "");
	    return status;
	}
    }

    _nVertices = n;

    return 0;
}

////////////////////////////////////////////////////////////////////////

void FilterShapeImagePolygon::print()
{
    char msg[512];
    sprintf(msg, "Image Polygon, %d vertices", _nVertices);
    zvmessage(msg, "");
    for (int i=0; i < _nVertices; i++) {
	sprintf(msg,"Vertex %d = (%f, %f)", i, _vertices[i]._x,_vertices[i]._y);
	zvmessage(msg, "");
    }
}

////////////////////////////////////////////////////////////////////////
// Polygon in 3-D space, projected into image space.
//
// XML looks like:
//
//    <projected shape="polygon">
//      <vertex x="1.0" y="2.0" z="3.0" />
//      <vertex x="2.0" y="3.0" z="4.0" />
//      <vertex x="3.0" y="4.0" z="5.0" />
//      ...
//    </projected>
//
////////////////////////////////////////////////////////////////////////

int FilterShapeProjectedPolygon::readFromXML(DOMElement *element)
{
    _need_projection = TRUE;

    // Find all <vertex> entries

    DOMNodeList *entries = PigXerces::getElementsByTagName(element, "vertex");

    _nVertices = entries->getLength();
    if (_nVertices < 3) {	// minimum 3 vertices for real polygon
	char msg[256];
	sprintf(msg, "Warning: less than 3 vertex entries (%d) in projected polygon", _nVertices);
	zvmessage(msg, "");
	return 1;				// nothing to do...
    }

    _vertices = new Point3D[_nVertices];
    if (_vertices == NULL) {
	zvmessage("Out of memory!  reading projected polygons", "");
	return 1;
    }

    for (int i=0; i < _nVertices; i++) {
	DOMElement *vertex = PigXerces::nextElement(entries, i);
	int status = _vertices[i].readFromXML(vertex);
	if (status != 0) {
	    zvmessage("parse failure in reading projected polygon", "");
	    return status;
	}
    }

    return 0;
}

////////////////////////////////////////////////////////////////////////

void FilterShapeProjectedPolygon::print()
{
    char msg[512];
    zvmessage("Projected Polygon", "");
    for (int i=0; i < _nVertices; i++) {
	sprintf(msg,"Vertex %d = (%f, %f, %f)", i,
		_vertices[i]._x, _vertices[i]._y, _vertices[i]._z);
	zvmessage(msg, "");
    }
}

////////////////////////////////////////////////////////////////////////
// Horizon in 3-D space, projected into image space.
//
// XML looks like:
//
//    <projected shape="horizon">
//      <max elevation="-3.0" />
//    </projected>
//
// IMPORTANT:  This looks at the "horizon" parameter.  If it exists, and
// is specified, that that value overrides the value given in <elevation>.
//
////////////////////////////////////////////////////////////////////////

int FilterShapeProjectedHorizon::readFromXML(DOMElement *element)
{
    char msg[256];

    // Find all <max> entries

    DOMNodeList *entries = PigXerces::getElementsByTagName(element, "max");

    int n = entries->getLength();
    if (n != 1) {
	sprintf(msg, "Warning: invalid # of <max> entries (%d) in projected horizon", n);
	zvmessage(msg, "");
	return 1;
    }
    DOMElement *max = PigXerces::nextElement(entries, 0);

    double el = PigXerces::getAttributeDouble(max, "elevation", -9.999e-30);
    if (el == -9.999e-30) {
	zvmessage("Missing max.elevation in projected horizon", "");
	return 1;
    }
    _elevation = PigDeg2Rad(el);

    // Look for parameter override

    double override;
    int count;
    PigModelBase::getStaticParam("HORIZON", &override, &count, 1, 0);
    if (count != 0) {
	_elevation = PigDeg2Rad(override);
	sprintf(msg, "Note: horizon overriden to %f via parameter", override);
	zvmessage(msg, "");
    }

    return 0;
}

////////////////////////////////////////////////////////////////////////

void FilterShapeProjectedHorizon::print()
{
    char msg[512];
    zvmessage("Projected Horizon", "");
    sprintf(msg, "Max Elevation = %f (%f degrees)", _elevation,
						PigRad2Deg(_elevation));
    zvmessage(msg, "");
}

////////////////////////////////////////////////////////////////////////
// Box in 3-D space, aligned with axes
//
// XML looks like:
//
//    <volume shape="box">
//      <min x="2.0" y="4.0" z="0.0" />
//      <max x="4.0" y="8.0" z="1.0" />
//      <rotate ... />
//    </volume>
//
////////////////////////////////////////////////////////////////////////

int FilterShapeVolumeBox::readFromXML(DOMElement *element)
{
    // Find all <min> entries

    DOMNodeList *entries = PigXerces::getElementsByTagName(element, "min");

    int n = entries->getLength();
    if (n != 1) {
	char msg[256];
	sprintf(msg, "Warning: invalid # of <min> entries (%d) in volume box", n);
	zvmessage(msg, "");
	return 1;
    }
    DOMElement *min = PigXerces::nextElement(entries, 0);
    int status = _min.readFromXML(min);
    if (status != 0) {
	zvmessage("parse failure in reading volume box", "");
	return status;
    }

    // Find all <max> entries

    entries = PigXerces::getElementsByTagName(element, "max");

    n = entries->getLength();
    if (n != 1) {
	char msg[256];
	sprintf(msg, "Warning: invalid # of <max> entries (%d) in volume box", n);
	zvmessage(msg, "");
	return 1;
    }
    DOMElement *max = PigXerces::nextElement(entries, 0);
    status = _max.readFromXML(max);
    if (status != 0) {
	zvmessage("parse failure in reading volume box", "");
	return status;
    }

    entries = PigXerces::getElementsByTagName(element, "rotate");
    n = entries->getLength();
    if (n != 0 && n != 1) {
	char msg[256];
	sprintf(msg, "Warning: invalid # of <rotate> entries (%d) in volume box", n);
	zvmessage(msg, "");
	return 1;
    }
    _rotate.clear();
    if (n == 1) {
	DOMElement *rotate = PigXerces::nextElement(entries, 0);
        status = _rotate.readFromXML(rotate);
        if (status != 0) {
	    zvmessage("parse failure in reading volume box", "");
	    return status;
        }
    }

    return 0;
}

////////////////////////////////////////////////////////////////////////

void FilterShapeVolumeBox::print()
{
    char msg[512];
    zvmessage("Volume Box", "");
    sprintf(msg, "Min = (%f, %f, %f)", _min._x, _min._y, _min._z);
    zvmessage(msg, "");
    sprintf(msg, "Max = (%f, %f, %f)", _max._x, _max._y, _max._z);
    zvmessage(msg, "");
    _rotate.print();
}

////////////////////////////////////////////////////////////////////////
// Cylinder in 3-D space, aligned with Z axis
//
// XML looks like:
//
//    <volume shape="z_cylinder">
//      <axis x="1.0" y="2.0" />
//      <min z="3.0" />
//      <max z="4.0" radius="5.0" />
//	<rotate .../>
//    </volume>
//
////////////////////////////////////////////////////////////////////////

int FilterShapeVolumeZCylinder::readFromXML(DOMElement *element)
{
    char *str;

    // Find all <axis> entries

    DOMNodeList *entries = PigXerces::getElementsByTagName(element, "axis");
    int n = entries->getLength();
    if (n != 1) {
	char msg[256];
	sprintf(msg, "Warning: invalid # of <axis> entries (%d) in volume ZCylinder", n);
	zvmessage(msg, "");
	return 1;
    }
    DOMElement *axis = PigXerces::nextElement(entries, 0);

    RealAttr(axis, "x", "volume ZCylinder axis", _axis_x);
    RealAttr(axis, "y", "volume ZCylinder axis", _axis_y);

    // Find all <min> entries

    entries = PigXerces::getElementsByTagName(element, "min");
    n = entries->getLength();
    if (n != 1) {
	char msg[256];
	sprintf(msg, "Warning: invalid # of <min> entries (%d) in volume ZCylinder", n);
	zvmessage(msg, "");
	return 1;
    }
    DOMElement *min = PigXerces::nextElement(entries, 0);

    RealAttr(min, "z", "volume ZCylinder min", _min_z);

    // Find all <max> entries

    entries = PigXerces::getElementsByTagName(element, "max");
    n = entries->getLength();
    if (n != 1) {
	char msg[256];
	sprintf(msg, "Warning: invalid # of <max> entries (%d) in volume ZCylinder", n);
	zvmessage(msg, "");
	return 1;
    }
    DOMElement *max = PigXerces::nextElement(entries, 0);

    RealAttr(max, "z", "volume ZCylinder max", _max_z);
    RealAttr(max, "radius", "volume ZCylinder max", _radius);

    entries = PigXerces::getElementsByTagName(element, "rotate");
    n = entries->getLength();
    if (n != 0 && n != 1) {
	char msg[256];
	sprintf(msg, "Warning: invalid # of <rotate> entries (%d) in volume box", n);
	zvmessage(msg, "");
	return 1;
    }
    _rotate.clear();
    if (n == 1) {
	DOMElement *rotate = PigXerces::nextElement(entries, 0);
        int status = _rotate.readFromXML(rotate);
        if (status != 0) {
	    zvmessage("parse failure in reading volume box", "");
	    return status;
        }
    }
    return 0;
}

////////////////////////////////////////////////////////////////////////

void FilterShapeVolumeZCylinder::print()
{
    char msg[512];
    zvmessage("Volume Z Cylinder", "");
    sprintf(msg, "axis X = %f, axis Y = %f", _axis_x, _axis_y);
    zvmessage(msg, "");
    sprintf(msg, "min Z = %f, max Z = %f", _min_z, _max_z);
    zvmessage(msg, "");
    sprintf(msg, "Radius = %f", _radius);
    zvmessage(msg, "");
    _rotate.print();
}

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
//	<rotate ... />
//    </volume>
//
////////////////////////////////////////////////////////////////////////

int FilterShapeVolumeYWasher::readFromXML(DOMElement *element)
{
    char *str;

    // Find all <axis> entries

    DOMNodeList *entries = PigXerces::getElementsByTagName(element, "axis");
    int n = entries->getLength();
    if (n != 1) {
	char msg[256];
	sprintf(msg, "Warning: invalid # of <axis> entries (%d) in volume YWasher", n);
	zvmessage(msg, "");
	return 1;
    }
    DOMElement *axis = PigXerces::nextElement(entries, 0);

    RealAttr(axis, "x", "volume YWasher axis", _axis_x);
    RealAttr(axis, "z", "volume YWasher axis", _axis_z);

    // Find all <min> entries

    entries = PigXerces::getElementsByTagName(element, "min");
    n = entries->getLength();
    if (n != 1) {
	char msg[256];
	sprintf(msg, "Warning: invalid # of <min> entries (%d) in volume YWasher", n);
	zvmessage(msg, "");
	return 1;
    }
    DOMElement *min = PigXerces::nextElement(entries, 0);

    RealAttr(min, "y", "volume YWasher min", _min_y);
    RealAttr(min, "radius", "volume YWasher min", _min_radius);

    // Find all <max> entries

    entries = PigXerces::getElementsByTagName(element, "max");
    n = entries->getLength();
    if (n != 1) {
	char msg[256];
	sprintf(msg, "Warning: invalid # of <max> entries (%d) in volume YWasher", n);
	zvmessage(msg, "");
	return 1;
    }
    DOMElement *max = PigXerces::nextElement(entries, 0);

    RealAttr(max, "y", "volume YWasher max", _max_y);
    RealAttr(max, "radius", "volume YWasher max", _max_radius);

    entries = PigXerces::getElementsByTagName(element, "rotate");
    n = entries->getLength();
    if (n != 0 && n != 1) {
	char msg[256];
	sprintf(msg, "Warning: invalid # of <rotate> entries (%d) in volume box", n);
	zvmessage(msg, "");
	return 1;
    }
    _rotate.clear();
    if (n == 1) {
	DOMElement *rotate = PigXerces::nextElement(entries, 0);
        int status = _rotate.readFromXML(rotate);
        if (status != 0) {
	    zvmessage("parse failure in reading volume box", "");
	    return status;
        }
    }
    return 0;
}

////////////////////////////////////////////////////////////////////////

void FilterShapeVolumeYWasher::print()
{
    char msg[512];
    zvmessage("Volume Y Washer", "");
    sprintf(msg, "axis X = %f, axis Z = %f", _axis_x, _axis_z);
    zvmessage(msg, "");
    sprintf(msg, "min Y = %f, max Y = %f", _min_y, _max_y);
    zvmessage(msg, "");
    sprintf(msg, "min radius = %f, max radius = %f", _min_radius, _max_radius);
    zvmessage(msg, "");
    _rotate.print();
}


////////////////////////////////////////////////////////////////////////
// Read the filters from the given file.  If the filename is NULL, the
// default filters are obtained from the config database and the basename.
// Note that this routine allocates "filters"; the caller must free.
// The filename actually used is returned in out_filename (which must
// be preallocated, or NULL if you don't want the filename).
//
// If the read-in instrument is "any", it matches any instrument.  This
// is for the mslfilter case.  Also if rmc1...rmc10 exist as attributes
// for "camera", they are matched against the given RMC, and the entry is
// only used if they match.  If the num_rmc is 0, no matching is done (i.e.
// it is assumed to match).  This allows entries for multiple poses to be
// stored in the same XML file.
////////////////////////////////////////////////////////////////////////

int FilterShape::readFilters(PigMission *m, const char *filename,
			const char *basename, const char *instrument,
			FilterShape **&filters, int &num_filters,
			char *out_filename, int rmc[], int num_rmc)
{
    char filter_filename[PIG_MAX_FILENAME_SIZE];
    char msg[1024];
    int i, c;

    if (filename == NULL) {		// Default, obtain from the database
	strcpy(filter_filename, "param_files/");
	strcat(filter_filename, m->getHostID());
	strcat(filter_filename, "_");
	strcat(filter_filename, basename);
	strcat(filter_filename, ".xmlf");

	char found_path[PIG_MAX_FILENAME_SIZE];
	FILE *fp = PigModelBase::openConfigFile(filter_filename, found_path);
	if (fp == NULL) {

	    // Didn't find .xmlf, so look for (older) .xml

	    filter_filename[strlen(filter_filename)-1] = '\0';

	    fp = PigModelBase::openConfigFile(filter_filename,found_path);
	    if (fp == NULL) {
	        sprintf(msg,"Error opening rover filter file '%s'",
							filter_filename);
	        zvmessage(msg, "");
	        return 1;
	    }
	}
	fclose(fp);

	strcpy(filter_filename, found_path);
    }
    else {
	strcpy(filter_filename, filename);
    }

    if (out_filename != NULL)			// save for caller
	strcpy(out_filename, filter_filename);

    // See if we can reuse the saved document

    DOMDocument *doc;
    if (cached_doc != NULL && strcmp(cached_filename, filter_filename) == 0) {
	doc = cached_doc;
        zvmessage("Using cache\n", "");
    } else {
	// Nope, (re)read it
	sprintf(msg, "Reading filter file: %s\n", filter_filename);
	zvmessage(msg, "");
        doc = PigXerces::parseFile(filter_filename);
        if (doc == NULL) {
	    sprintf(msg, "Error reading filter XML file '%s'", filter_filename);
	    zvmessage(msg, "");
	    return 1;
        }
	cached_doc = doc;
	strcpy(cached_filename, filter_filename);
    }

    DOMElement *root = doc->getDocumentElement();

    // Verify the host_id against the host ID.

    const char *host_id_file = m->getHostID();
    if (!PigXerces::attrEqualsIgnoreCase(root, "host_id", host_id_file)) {

	sprintf(msg, "Host_id from filter file '%s' does not match given host ID of '%s'!\n",
			filter_filename, host_id_file);
	zvmessage(msg, "");
	return 1;
    }

    // Find the proper <camera> entry(s)

    DOMNodeList *cameras = PigXerces::getElementsByTagName(root, "camera");
    DOMElement *camera;

    DOMElement **camera_list = new DOMElement *[cameras->getLength()];
    int cameras_found = 0;

    for (c=0; c < cameras->getLength(); c++) {

	camera = PigXerces::nextElement(cameras, c);

	if (!PigXerces::attrEqualsIgnoreCase(camera, "id", instrument) &&
	    !PigXerces::attrEqualsIgnoreCase(camera, "id", "any")) {
	    continue;		// Doesn't match, or not a wildcard
	}

	// Check to see if the RMC's match, if any are given

	if (num_rmc != 0) {
	    int match = TRUE;
	    for (int idx=0; idx < num_rmc; idx++) {
		char attr[255];
		sprintf(attr, "rmc%d", idx+1);
		int rmc_idx = PigXerces::getAttributeInt(camera, attr, -1);
		if (rmc_idx == -1)
		    continue;			// doesn't exist
		if (rmc_idx != rmc[idx]) {
		    match = FALSE;		// doesn't match
		    break;
		}
	    }
	    if (!match)
		continue;		// doesn't match, skip this entry
	}

	camera_list[cameras_found++] = camera;
    }

    // Count the number of filter entries

    num_filters = 0;
    for (c=0; c < cameras_found; c++) {
	camera = camera_list[c];

        DOMNodeList *images = PigXerces::getElementsByTagName(camera, "image");
        DOMNodeList *projecteds = PigXerces::getElementsByTagName(camera,
								"projected");
        DOMNodeList *volumes = PigXerces::getElementsByTagName(camera,"volume");

        num_filters += images->getLength() + projecteds->getLength() +
						volumes->getLength();
    }

    filters = new FilterShape *[num_filters];
    num_filters = 0;

    // Now read in the elements under each camera

    for (c=0; c < cameras_found; c++) {
	camera = camera_list[c];

        DOMNodeList *images = PigXerces::getElementsByTagName(camera, "image");
        DOMNodeList *projecteds = PigXerces::getElementsByTagName(camera,
								"projected");
        DOMNodeList *volumes = PigXerces::getElementsByTagName(camera,"volume");

        // Read all <image> tags

        for (i=0; i < images->getLength(); i++) {
	    DOMElement *image = PigXerces::nextElement(images, i);

	    char *shape = PigXerces::getAttribute(image, "shape");

	    if (strcmp(shape, "polygon") != 0) {
	        sprintf(msg, "Invalid <image> shape: '%s'", shape);
	        zvmessage(msg, "");
		XMLString::release(&shape);
	        return 1;
	    }
	    XMLString::release(&shape);

	    filters[num_filters] = new FilterShapeImagePolygon();
	    int status = filters[num_filters]->readFromXML(image);
	    if (status != 0) {
	        zvmessage("Failure reading Image Polygon", "");
	        return 1;
	    }
	    num_filters++;
        }

        // Read all <projected> tags

        for (i=0; i < projecteds->getLength(); i++) {
	    DOMElement *projected = PigXerces::nextElement(projecteds, i);

	    char *shape = PigXerces::getAttribute(projected, "shape");

	    char *id = PigXerces::getAttribute(projected, "id");

	    // Triangle is read as a Polygon
	    if (strcmp(shape, "triangle") == 0) {
	        filters[num_filters] = new FilterShapeProjectedPolygon();
	    }
	    else if (strcmp(shape, "polygon") == 0) {
	        filters[num_filters] = new FilterShapeProjectedPolygon();
	    }
	    else if (strcmp(shape, "horizon") == 0) {
	        filters[num_filters] = new FilterShapeProjectedHorizon();
	    }
	    else {
	        sprintf(msg, "Invalid <projected> shape: '%s'", shape);
	        zvmessage(msg, "");
		XMLString::release(&shape);
	        return 1;
	    }
	    if (id != NULL && strlen(id) != 0)
		filters[num_filters]->setId(id);

	    int status = filters[num_filters]->readFromXML(projected);
	    if (status != 0) {
	        sprintf(msg, "Failure reading Projected %s", shape);
		zvmessage(msg, "");
		XMLString::release(&shape);
	        return 1;
	    }
	    XMLString::release(&shape);

	    num_filters++;
        }

        // Read all <volume> tags

        for (i=0; i < volumes->getLength(); i++) {
	    DOMElement *volume = PigXerces::nextElement(volumes, i);

	    char *shape = PigXerces::getAttribute(volume, "shape");

	    if (strcmp(shape, "box") == 0) {
	        filters[num_filters] = new FilterShapeVolumeBox();
	    }
	    else if (strcmp(shape, "z_cylinder") == 0) {
	        filters[num_filters] = new FilterShapeVolumeZCylinder();
	    }
	    else if (strcmp(shape, "y_washer") == 0) {
	        filters[num_filters] = new FilterShapeVolumeYWasher();
	    }
	    else {
	        sprintf(msg, "Invalid <volume> shape: '%s'", shape);
	        zvmessage(msg, "");
		XMLString::release(&shape);
	        return 1;
	    }

	    int status = filters[num_filters]->readFromXML(volume);
	    if (status != 0) {
	        sprintf(msg, "Failure reading Volume %s", shape);
	        zvmessage(msg, "");
		XMLString::release(&shape);
	        return 1;
	    }
	    XMLString::release(&shape);

	    num_filters++;
        }

    }
    if (cameras_found == 0) {
	sprintf(msg, "Unable to find <camera> entry for instrument '%s'",
								instrument);
	zvmessage(msg, "");
	return 1;
    }

    return 0;
}

