////////////////////////////////////////////////////////////////////////
// RadiometryCalImage
//
// Simple class to hold and maintain a cal image for use by the
// radiometry classes.  This is intended to cache one type of image,
// which is replaced if a different instrument is loaded.
//
// Everything is public here for ease of access.
////////////////////////////////////////////////////////////////////////

#include "PigModelBase.h"
#include "RadiometryCalImage.h"
#include "PigXerces.h"
#include "PigCameraMapper.h"
#include "PigCameraMapEntry.h"

#include "zvproto.h"
#include "applic.h"

#include "lbl_identification.h"
#include "return_status.h"

////////////////////////////////////////////////////////////////////////
RadiometryCalImage::RadiometryCalImage(const char *mission, int dbl)
{
    _float_image = NULL;
    _double_image = NULL;
    _valid = FALSE;
    _mission = strdup(mission);
    _dbl = dbl;			// flag: true==double, false==float
    _product_id = NULL;
    _product_desc = NULL;

    strcpy(_short_mission_name, _mission);
    char *p;
    if ((p = strchr(_short_mission_name, ':')) != NULL)
	*p = '\0';			// strip off the host_id

    _nb = 0;
    _nl = 0;
    _ns = 0;
}

////////////////////////////////////////////////////////////////////////
RadiometryCalImage::~RadiometryCalImage()
{
    if (_float_image != NULL)
	delete _float_image;
    if (_double_image != NULL)
	delete _double_image;
    if (_mission)
	delete _mission;
    if (_product_id)
	delete _product_id;
    if (_product_desc)
	delete _product_desc;
}

////////////////////////////////////////////////////////////////////////
int RadiometryCalImage::loadFile(const char *serial_number, const char *filter,
				const char *mode)
{
    char *filename = getPathname(serial_number, filter, mode);

    return loadFile(filename);
}

////////////////////////////////////////////////////////////////////////
int RadiometryCalImage::loadFile(const char *filename)
{
    char path[PIG_MAX_FILENAME_SIZE];
    char msg[256];
    static int inst = 1;		// instance for zvunit

    // Load the file if necessary

    if (filename != NULL && !_valid) {
	strcpy(_tag, filename);
 	_valid = FALSE;

	int unit = -1, status = -1;
	FILE *f = PigModelBase::openConfigFile((char *)filename, path);
	if (f != NULL) {
	    fclose(f);
	    sprintf(msg, "loading rad cal file %s", path);
	    PigModelBase::printUniqueStaticMsg(msg, PigMsgInfo);

	    // re-open the flat field image using VICAR

	    zvunit(&unit, "rad_cal", inst++, "U_NAME", path, NULL);
	    status = zvopen(unit, "U_FORMAT", _dbl ? "DOUB" : "REAL",
			"OPEN_ACT", "", "IO_ACT", "S", NULL);
	}

	if (status != SUCCESS) {
	    sprintf(msg, "Unable to open rad cal file %s (%s)", path, filename);
	    PigModelBase::printUniqueStaticMsg(msg, PigMsgError);
	    if (unit != -1)
		zvclose(unit, "clos_act", "free", NULL);
	    return FALSE;
	}
	zvget(unit, "NS", &_ns, "NL", &_nl, "NB", &_nb, NULL);

	if (_double_image != NULL) {
	    delete _double_image;
	    _double_image = NULL;
	}
	if (_float_image != NULL) {
	    delete _float_image;
	    _float_image = NULL;
	}
	if (_dbl)
	    _double_image = new double[_nb*_nl*_ns];
	else
	    _float_image = new float[_nb*_nl*_ns];

	if (_double_image == NULL && _float_image == NULL) {
	    sprintf(msg, "Error allocating memory for rad cal file %s", path);
	    PigModelBase::printStaticMsg(msg, PigMsgError);
	    if (unit != -1)
		zvclose(unit, "clos_act", "free", NULL);
	    return FALSE;
	}

	for (int b = 0; b < _nb; b++) {
	    for (int i = 0; i < _nl; i++) {
	        if (_dbl)
		    status = zvread(unit, &_double_image[b*_nl*_ns+i*_ns],
				"band", b+1, "line", i+1, NULL);
	        else
		    status = zvread(unit, &_float_image[b*_nl*_ns+i*_ns],
				"band", b+1, "line", i+1, NULL);
	        if (status != SUCCESS) {
		    sprintf(msg, "I/O error reading rad cal file %s", path);
		    PigModelBase::printStaticMsg(msg, PigMsgError);
		    PigModelBase::printStaticMsg(
			"No flat field correction applied", PigMsgWarning);
		    zvclose(unit, "clos_act", "free", NULL);
		    return FALSE;
	        }
	    }
	}

	// Read PRODUCT_ID label, if it exists...

	LblIdentification_typ lblIdent;
	status = LblIdentification(unit, LBL_READ, &lblIdent, 1);
	if (! RTN_FAILURE(status)) { 		// ignore on error
	    if (lblIdent.ProductId.Valid) {

		_product_id = strdup(lblIdent.ProductId.Value);
	    }
	    if (lblIdent.ProcessingHistoryText.Valid) {
		_product_desc = strdup(lblIdent.ProcessingHistoryText.Value);
	    }
	}

	zvclose(unit, "clos_act", "free", NULL);
	_valid = TRUE;
    }

    return TRUE;


}

////////////////////////////////////////////////////////////////////////
// Construct the terminal pathname for the file.  Can be overridden.
//
// NOTE:  Returned string may be a pointer to a static buffer.  Use it
// quickly or copy it.  Not reentrant!!
////////////////////////////////////////////////////////////////////////

char *RadiometryCalImage::constructFilename(const char *serial_number,
			const char *filter, const char *mode, int var_flat,
			const char *type)
{
    static char filename[PIG_MAX_FILENAME_SIZE];

    strcpy(filename, "flat_fields/");
    strcat(filename, _short_mission_name);
    strcat(filename, "_FLAT");
    if (serial_number) {
	strcat(filename, "_SN_");
	strcat(filename, serial_number);
    }
    if (mode) {
	strcat(filename, "_M_");
	strcat(filename, mode);
    }
    if (type) {
	strcat(filename, "_T_");
	strcat(filename, type);
    }
    if (filter && strlen(filter) != 0) {
	strcat(filename, "_F_");
	strcat(filename, filter);
    }

    // If var_flat is set, we actually build the name of the file containing
    // the variable flat names instead of .IMG

    if (var_flat)
	strcat(filename, ".varflat");
    else
	strcat(filename, ".IMG");

    return filename;
}

////////////////////////////////////////////////////////////////////////
// Override of the above for dark-current-style files.  Filter is ignored.
////////////////////////////////////////////////////////////////////////

char *RadiometryCalImageDark::constructFilename(const char *serial_number,
					const char *filter, const char *mode)
{
    static char filename[PIG_MAX_FILENAME_SIZE];

    strcpy(filename, "dark_current/");
    strcat(filename, _short_mission_name);
    if (serial_number) {
	strcat(filename, "_SN_");
	strcat(filename, serial_number);
    }
    strcat(filename, "_dark_");
    strcat(filename, _dark_type);

    if (mode) {
	strcat(filename, "_M_");
	strcat(filename, mode);
    }

    strcat(filename, ".IMG");

    return filename;
}



////////////////////////////////////////////////////////////////////////
// Get and return the pathname to the file.  Checks the cached image to
// see if it needs replacing or not.  A NULL return means the cache is
// good; otherwise returns the filename to load.
//
// NOTE:  Returned string may be a pointer to a static buffer.  Use it
// quickly or copy it.  Not reentrant!!
////////////////////////////////////////////////////////////////////////
char *RadiometryCalImage::getPathname(const char *serial_number,
					const char *filter, const char *mode)
{
    char path[PIG_MAX_FILENAME_SIZE];

    char *filename = constructFilename(serial_number, filter, mode);

    // See if we need to load a new file

    if (_valid && (strcmp(_tag, filename) == 0)) {
	return NULL;				// nope, existing one is good
    }

    return filename;
}

