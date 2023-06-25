// %M% %I% %E% %U%

** ABANDONED ATTEMPT TO USE MIPL Library for SUMMITT **

#ifndef _MIPLIMAGE_H_
#define _MIPLIMAGE_H_


#include "image/geoimage.h"

// This class defines a version of GeoImage that is read in using
// the MIPL VICAR Run-Time Library, handling whatever formats that
// library supports (in particular VICAR and PDS-wrapped VICAR).
// While a new ImageFile subclass might seem to make more sense,
// the VICAR RTL doesn't use normal file pointers.
//
// Note that the "GEOIMAGE" class type is not overridden.
//
// TO DO: get georef data from file header if present?
// TO DO: support writing files?

class MIPLImage : public GeoImage {

    protected:
    	int  unit;			// VICAR RTL file unit number
    	
    	void cleanup();			// close and release file unit

    public:

	// read image data from file, using VICAR RTL functions.
	// returns 0 if okay
    	int read(const char *fname);
    	
    	// get CAHV camera model vectors from file, return 0 if okay
    	int get_cahv(double C[3], double A[3], double H[3], double V[3]);
			
	MIPLImage() 			{ unit = -1; }
	MIPLImage(const char *fname)	{ read(fname); }
	~MIPLImage()			{ cleanup(); }
};

#endif
