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
#ifndef PIGCAMERAMAPENTRY_H
#define PIGCAMERAMAPENTRY_H

class PigCameraMapEntry {

  protected:

    char *_id;
    char *_stereo_id;
    char *_short_id;
    char *_eye_id;
    char *_name;
    char *_serial_number;	// treated as string
    char *_filters;
    char *_focus;
    char *_zoom;
    char *_temperature;
    char *_type;
    char *_nl;			// Nominal size of camera
    char *_ns;
    char *_color;			//
    char *_var_flat;

  public:

    // Create the entry.  This is the only time the fields should be modified.
    // The constructor assumes management of the strings, which are assumed
    // to be dynamically allocated, and will be deallocated via "delete".
    // NULL is allowed for any entry.

    PigCameraMapEntry(char * id, char * stereo_id, char * short_id,
		char * eye_id, char * name, char * serial_number,
		char * filters, char * focus, char *zoom, char * temperature,
		char * type, char * nl, char * ns, char * color,
		char *var_flat);

    virtual ~PigCameraMapEntry();

    // Accessors for the fields.  Note that all of them return a pointer to
    // the internal string!!  This must not be modified.  In addition, the
    // object could be deleted at any time, so the returned string must be
    // used or copied right away.

    virtual char *getID()		{ return _id; }
    virtual char *getStereoID()		{ return _stereo_id; }
    virtual char *getShortID()		{ return _short_id; }
    virtual char *getEyeID()		{ return _eye_id; }
    virtual char *getName()		{ return _name; }
    virtual char *getSerialNumber()	{ return _serial_number; }
    virtual char *getFiltersAsString()	{ return _filters; }
    virtual char *getFocusAsString()    { return _focus; }
    virtual char *getZoomAsString()     { return _zoom; }
    virtual char *getTemperatureAsString()    { return _temperature; }
    virtual int getFilters();		// 0==false, 1==true
    virtual int getFocus();		// 0==false, 1==true
    virtual int getZoom();		// 0==false, 1==true
    virtual int getTemperature();	// 0==false, 1==true
    virtual char *getType()		{ return _type; }
    virtual int getNL();
    virtual int getNS();
    virtual int getColor();		// 0==false, 1==true
    virtual int getVarFlat();		// 0==false, 1==true

};

#endif

