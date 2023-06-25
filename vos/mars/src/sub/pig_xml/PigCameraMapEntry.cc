////////////////////////////////////////////////////////////////////////
// PigCameraMapEntry
//
// Data structure holding a single Camera Mapping file entry.
// See PigCameraMapper for details.
//
// PigCameraMapper subclasses may subclass this as well, if additional
// information needs to be stored.
//
// Note that the PigCameraMapEntry object is immutable once created.
////////////////////////////////////////////////////////////////////////

#include "PigCameraMapEntry.h"
#include "PigXerces.h"

#include "PigModelBase.h"		// for NULL

////////////////////////////////////////////////////////////////////////
// Create the entry.  This is the only time the fields should be modified.
// The constructor assumes management of the strings, which are assumed
// to be dynamically allocated, and will be deallocated via "delete".
// NULL is allowed for any entry.
////////////////////////////////////////////////////////////////////////

PigCameraMapEntry::PigCameraMapEntry(
		char *id, char *stereo_id, char *short_id,
		char *eye_id, char *name, char *serial_number,
		char *filters, char *focus, char *zoom, char *temperature,
		char *type, char *nl, char *ns, char *color, char *var_flat)
{
    _id = id;
    _stereo_id = stereo_id;
    _short_id = short_id;
    _eye_id = eye_id;
    _name = name;
    _serial_number = serial_number;
    _filters = filters;
    _focus = focus;
    _zoom = zoom;
    _temperature = temperature;
    _type = type;
    _nl = nl;
    _ns = ns;
    _color = color;
    _var_flat = var_flat;
}

////////////////////////////////////////////////////////////////////////
// Destroy the entry.  Since the ctor assumed memory management of the
// strings, we can just delete them here.
////////////////////////////////////////////////////////////////////////

PigCameraMapEntry::~PigCameraMapEntry()
{
    if (_id != NULL)
	delete _id;

    if (_stereo_id != NULL)
	delete _stereo_id;
    
    if (_short_id != NULL)
	delete _short_id;

    if (_eye_id != NULL)
	delete _eye_id;

    if (_name != NULL)
	delete _name;

    if (_serial_number != NULL)
	delete _serial_number;

    if (_filters != NULL)
	delete _filters;

    if (_focus != NULL)
	delete _focus;

    if (_zoom != NULL)
	delete _zoom;

    if (_temperature != NULL)
	delete _temperature;

    if (_type != NULL)
	delete _type;

    if (_nl != NULL)
	delete _nl;

    if (_ns != NULL)
	delete _ns;

    if (_color != NULL)
	delete _color;

    if (_var_flat != NULL)
	delete _var_flat;
}

////////////////////////////////////////////////////////////////////////
// Accessor for "filters".  Returns 0 for false and 1 for true.
////////////////////////////////////////////////////////////////////////

int PigCameraMapEntry::getFilters()
{
    if (_filters == NULL)
	return 0;
    if (_filters[0] == 't' || _filters[0] == 'T' ||
	_filters[0] == 'y' || _filters[0] == 'Y')
	return 1;

    return 0;
}

////////////////////////////////////////////////////////////////////////
// Accessor for "focus".  Returns 0 for false and 1 for true.
////////////////////////////////////////////////////////////////////////

int PigCameraMapEntry::getFocus()
{
    if (_focus == NULL)
	return 0;
    if (_focus[0] == 't' || _focus[0] == 'T' ||
	_focus[0] == 'y' || _focus[0] == 'Y')
	return 1;

    return 0;
}

////////////////////////////////////////////////////////////////////////
// Accessor for "zoom".  Returns 0 for false and 1 for true.
////////////////////////////////////////////////////////////////////////

int PigCameraMapEntry::getZoom()
{
    if (_zoom == NULL)
	return 0;
    if (_zoom[0] == 't' || _zoom[0] == 'T' ||
	_zoom[0] == 'y' || _zoom[0] == 'Y')
	return 1;

    return 0;
}

////////////////////////////////////////////////////////////////////////
// Accessor for "temperature".  Returns 0 for false and 1 for true.
////////////////////////////////////////////////////////////////////////

int PigCameraMapEntry::getTemperature()
{
    if (_temperature == NULL)
	return 0;
    if (_temperature[0] == 't' || _temperature[0] == 'T' ||
	_temperature[0] == 'y' || _temperature[0] == 'Y')
	return 1;

    return 0;
}

////////////////////////////////////////////////////////////////////////
// Accessor for "color".  Returns 0 for false and 1 for true.
////////////////////////////////////////////////////////////////////////

int PigCameraMapEntry::getColor()
{
    if (_color == NULL)
	return 0;
    if (_color[0] == 't' || _color[0] == 'T' ||
	_color[0] == 'y' || _color[0] == 'Y')
	return 1;

    return 0;
}

////////////////////////////////////////////////////////////////////////
// Accessor for "var_flat".  Returns 0 for false and 1 for true.
////////////////////////////////////////////////////////////////////////

int PigCameraMapEntry::getVarFlat()
{
    if (_var_flat == NULL)
	return 0;
    if (_var_flat[0] == 't' || _var_flat[0] == 'T' ||
	_var_flat[0] == 'y' || _var_flat[0] == 'Y')
	return 1;

    return 0;
}

////////////////////////////////////////////////////////////////////////
// Accessors for "nl" and "ns".  Returns 1024 if value not specified.
////////////////////////////////////////////////////////////////////////

int PigCameraMapEntry::getNL()
{
    if (_nl == NULL)
	return 1024;
    int nl = atoi(_nl);
    if (nl < 100 || nl > 10000)		// sanity check
	return 1024;

    return nl;
}

int PigCameraMapEntry::getNS()
{
    if (_ns == NULL)
	return 1024;
    int ns = atoi(_ns);
    if (ns < 100 || ns > 10000)		// sanity check
	return 1024;

    return ns;
}

