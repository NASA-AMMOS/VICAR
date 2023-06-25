////////////////////////////////////////////////////////////////////////
// RadiometryCalImage
//
// Simple class to hold and maintain a cal image for use by the
// radiometry classes.  This is intended to cache one type of image,
// which is replaced if a different instrument is loaded.
//
// Everything is public here for ease of access.
////////////////////////////////////////////////////////////////////////
#ifndef RADIOMETRYCALIMAGE_H
#define RADIOMETRYCALIMAGE_H

#include <string.h>

class RadiometryCalImage {

  protected:

  public:
    char *_mission;
    char _short_mission_name[50];	// without the :host_id

    int _dbl;			// true == use _double_image, else _float_image
    float *_float_image;
    double *_double_image;

    char _tag[256];
    int _valid;

    int _nb, _nl, _ns;

    char *_product_id;		// ID for this cal file (if any)
    char *_product_desc;	// description for this cal file (if any)

    RadiometryCalImage(const char *mission, int dbl);
    virtual ~RadiometryCalImage();

    virtual int loadFile(const char *serial_number, const char *filter,
			const char *mode);
    virtual int loadFile(const char *filename);

    virtual char *constructFilename(const char *serial_number,
					const char *filter, const char *mode)
	{ return constructFilename(serial_number, filter, mode, FALSE, NULL); }

    virtual char *constructFilename(const char *serial_number,
					const char *filter, const char *mode,
					int var_flat, const char *type);

    virtual char *getPathname(const char *serial_number, const char *filter,
				const char *mode);

    virtual double getValue(int band, int line, int samp)
	{ if (_dbl)
	    return getValueDouble(band, line, samp);
	  return (double)getValueFloat(band, line, samp);
	}

    virtual float getValueFloat(int band, int line, int samp)
	{ if (_float_image)
	      return _float_image[(band<_nb?band:0)*_nl*_ns + line*_ns + samp];
	  return 0.0; }

    virtual double getValueDouble(int band, int line, int samp)
	{ if (_double_image)
	      return _double_image[(band<_nb?band:0)*_nl*_ns + line*_ns + samp];
	  return 0.0; }
};

////////////////////////////////////////////////////////////////////////

class RadiometryCalImageDark : public RadiometryCalImage {
  public:
    char *_dark_type;
    RadiometryCalImageDark(const char *mission, int dbl, const char *dark_type)
		: RadiometryCalImage(mission, dbl)
			{ _dark_type = strdup(dark_type); }

    virtual ~RadiometryCalImageDark() { if (_dark_type) delete _dark_type; }

    virtual char *constructFilename(const char *serial_number,
					const char *filter, const char *mode);
};

#endif

