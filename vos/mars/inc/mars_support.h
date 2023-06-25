////////////////////////////////////////////////////////////////////////
// mars_support.h
//
// Miscellaneous routines used by the mars* suite of programs.
//
// Note that these subroutines all use C++ linkage and are thus not
// callable from C or Fortran without bridges.
////////////////////////////////////////////////////////////////////////

#ifndef _MARS_SUPPORT_H
#define _MARS_SUPPORT_H

// RTL library has fundamental string limit of 250 characters.
// Also the same constant is defined in PigModelBase.h.  Thus
// if we ever should change value here, we also need to change
// it in PigModelBase.h
#ifndef PIG_MAX_FILENAME_SIZE
#define PIG_MAX_FILENAME_SIZE 250
#endif

// defines image's max number of lines and samples
// used for static allocation of arrays
#define MARS_MAX_NL 9000
#define MARS_MAX_NS 9000
#define MARS_MAX_NB 32

#include <stdio.h>		// for NULL
#include <set>

class PigFileModel;
class PigCameraModel;
class PigPointingModel;
class RadiometryModel;
class PigCoordSystem;
class PigSurfaceModel;
class PigPointingCorrections;
class PigBrtCorrModel;

#include "lbl_identification.h"
#include "lbl_instrument_state.h"
#include "lbl_image_data.h"
#include "lbl_surface_model.h"
#include "lbl_derived_geometry.h"
#include "PigPointingCorrections.h"
#include "SimpleImage.h"

////////////////////////////////////////////////////////////////////////
// Does the standard processing needed by all multi-input mars* mosaic
// programs (including nav).  This involves reading the file list (or
// input parameter) from INP, and setting up the initial camera/pointing
// and radiometry models for each input.  The caller is responsible for
// allocating all arrays.  This routine also handles calling mars_read_rsf
// and mars_setup_coords for you.  The coordinate system to use is returned
// in the PigCoordSystem parameter.
//
// This routine expects the following parameters be in the PDF:
//
// PARM NAVTABLE TYPE=STRING COUNT=(0:1) DEFAULT=--
// PARM FORMAT TYPE=STRING COUNT=(0:1) +
//  VALID=("XML","TXT") DEFAULT="XML"
// PARM SOLUTION_ID TYPE=STRING COUNT=0:1 DEFAULT=--
// PARM RAD TYPE=KEYWORD VALID=("RAD", "NORAD") DEFAULT=RAD
// PARM RSF TYPE=STRING COUNT=0:100 DEFAULT=--
// PARM COORD TYPE=KEYWORD VALID=("FIXED", "INSTRUMENT", "SITE", "ROVER", +
//            "LOCAL_LEVEL") DEFAULT="FIXED"
// PARM COORD_INDEX TYPE=INTEGER COUNT=0:10 DEFAULT=--
// PARM FIXED_SITE TYPE=INTEGER COUNT=0:1 DEFAULT=--
//
// where RAD controls whether or not to radiometrically correct the images.
// If the radiometry array is passed in as NULL (meaning the caller doesn't
// want rad at all), this parameter is not checked.
// See mars_read_rsf below for the meaning of the RSF parameter.
// See mars_setup_coords below for the meaning of the COORD, COORD_INDEX,
// and FIXED_SITE parameters.
// If the brt_corr array is passed in as NULL (meaning the caller doesn't
// want brt corr at all), the BRTCORR parameter is not checked at all and
// may be absent.
//
// If a file in the list of input filenames does not exist, then that
// file is ignored, and its corresponding elements in the output arrays
// are left unchanged (untouched).
////////////////////////////////////////////////////////////////////////

void mars_setup(int &nids, PigFileModel *files[],
        PigCameraModel *cameras[], PigPointingModel *pointings[],
        PigSurfaceModel *&surface_model, RadiometryModel *radiometric[], 
 	PigBrtCorrModel *brt_corr_models[],
        PigCoordSystem *&def_cs, char *mission, char *instrument, 
        int &homogeneous_inputs, const int max_nl, const int max_ns, 
        const int max_inputs);

// Same as above except no PigBrtCorrModel parameter.
// Kept here for legacy purposes.
void mars_setup(int &nids, PigFileModel *files[],
        PigCameraModel *cameras[], PigPointingModel *pointings[],
        PigSurfaceModel *&surface_model, RadiometryModel *radiometric[], 
        PigCoordSystem *&def_cs, char *mission, char *instrument, 
        int &homogeneous_inputs, const int max_nl, const int max_ns, 
        const int max_inputs);

//  Same as above except no PigSurfaceModel or PigBrtCorrModel parameters.
//  Kept here for legacy purposes.
void mars_setup(int &nids, PigFileModel *files[],
        PigCameraModel *cameras[], PigPointingModel *pointings[],
	RadiometryModel *radiometric[], PigCoordSystem *&def_cs,
        char *mission, char *instrument, int &homogeneous_inputs,
        const int max_nl, const int max_ns, const int max_inputs);

////////////////////////////////////////////////////////////////////////
// Takes a given filelist, opens all the files, and reads/initializes all of
// the given arrays.  This is much of what used to be in mars_setup but it has
// been separated out so the caller can read more than one input file list.
//
// Note that mars_setup does other one-time initialization and should be
// called instead of this in most cases, and before this when reading more
// than one list.  In other words, this should be called ONLY when reading
// more than one list, and after mars_setup.
//
// The caller should call mars_get_filelist() first in order to populate
// nids and filenames.
//
// Note that the def_cs return is used by mars_setup and should generally
// be ignored when calling this routine directly (the default CS's really
// should match after all...).  Likewise, the pointing_corrections and
// solution_id are for mars_setup and should generally be ignored by other
// callers (they can be passed in as NULL if you don't need them.
//
// This routine expects items in the PDF as for mars_setup.
//
// The only input parameters are nids and filenames... but the routine
// expects most other arrays to be set up so they can just be filled in
// as outputs.
//
// If a file in the list of input filenames (char **) does not exist, then
// that file is ignored, and its corresponding elements in the output arrays
// are left unchanged (untouched).
////////////////////////////////////////////////////////////////////////

void mars_read_filelist(int nids, char **filenames,             // inputs
        PigFileModel *files[],
        PigCameraModel *cameras[], PigPointingModel *pointings[],
        RadiometryModel *radiometric[],
        PigBrtCorrModel *brt_corr_models[],
        PigCoordSystem *&def_cs, char *mission, char *instrument,
        int &homogeneous_inputs, const int max_nl, const int max_ns,
        PigPointingCorrections **pointing_corrections,
        char **solution_id);


////////////////////////////////////////////////////////////////////////
// Read in the Rover State files from standard parameters.  This should be
// done before reading in files, so the reference chain can be used during
// file read.
//
// This method really should be called once per mission, not just once
// overall.  If we mix mission data, the objects created here are not
// really singletons, but are singletons per mission.
//
// For non-rover missions (where RSF's are not applicable), this routine
// is essentially a no-op.
//
// This routine expects the following parameter in the PDF:
//
// PARM RSF TYPE=STRING COUNT=0:100 DEFAULT=--
//
// RSF is a list of Rover State Files to load (highest priority first).
////////////////////////////////////////////////////////////////////////

void mars_read_rsf(const char *mission);

////////////////////////////////////////////////////////////////////////
// Does the setup of coordinate systems.  This includes returning the default
// coordinate system to use, and initializing the Fixed site for the mission.
// The default coordinate system is returned.
//
// For non-rover missions (where the Fixed site is... fixed), this routine is
// still useful in that it handles the COORD parameter for you.
//
// This routine expects the following parameters in the PDF:
//
// PARM COORD TYPE=KEYWORD VALID=("FIXED", "INSTRUMENT", "SITE", "ROVER", +
//            "LOCAL_LEVEL") DEFAULT="FIXED"
// PARM COORD_INDEX TYPE=INTEGER COUNT=0:10 DEFAULT=--
// PARM FIXED_SITE TYPE=INTEGER COUNT=0:1 DEFAULT=--
// PARM SOLUTION_ID TYPE=STRING COUNT=0:1 DEFAULT=--
// PARM DEBUG_RSF TYPE=KEYWORD VALID=DEBUG_RSF COUNT=0:1 DEFAULT=--
//
// COORD is the coordinate system to use (other mission-specific names may
// be included in the valid list, but are not required).  COORD_INDEX is the
// coordinate system index (needed for some rover-based CS's - it's the RMC).
// FIXED_SITE defines which major Site is the FIXED site for this run.
//
// If COORD_INDEX is defaulted (and needed), it is obtained from the
// first entry in the provided File list.
//
// If FIXED_SITE is defaulted (and needed), then the minimum Site number
// (first index of motion counter) found in the provided File list is used.
// DEBUG_RSF turns on a statement that prints out the RMC database.  It
// does not actually need to be in the delivered PDF; the app can be copied 
// and the value added to the PDF when needed.
////////////////////////////////////////////////////////////////////////

PigCoordSystem *mars_setup_coords(const char *mission,
			int num_files, PigFileModel *files[]);

////////////////////////////////////////////////////////////////////////
// Retrieves a file list from the given parameter name.  The files are either
// specified in a file list (simple ASCII text) which is the only value in
// the parameter, or the files may be listed directly in the parameter itself.
// If only one value is specified, a file list is assumed.  However, if the
// file starts with LBLSIZE=, or if it doesn't exist and outputs is TRUE,
// then the single parameter is assumed to be the actual filename, not the
// name of a file list.
//
// The filename array must be passed in; the filenames themselves are
// dynamically allocated and must be freed by the caller.
////////////////////////////////////////////////////////////////////////

void mars_get_filelist(const char *param_name, int &nids, char *filenames[],
                        const int max_inputs, const int outputs=0);

////////////////////////////////////////////////////////////////////////
// Reads in a navigation table (specified by the NAVTABLE parameter)
// and applies the corrections to the pointing objects.  Returns 0 if
// corrections were applied, 1 if NAVTABLE was not specified, or 2 if
// there was an error in the file.  The status return can be ignored if
// desired; the pointing will be unchanged unless the file is read okay,
// and this routine will print its own error messages.
////////////////////////////////////////////////////////////////////////

int mars_apply_navtable(int nids, PigPointingModel *pointing_in[], 
			PigFileModel *files_in[]);

////////////////////////////////////////////////////////////////////////
// Reads in a navigation table(if one could be found).  This routine
// creates and returns PigPointingCorrections object that manages 
// nav data.
/////////////////////////////////////////////////////////////////////// 
PigPointingCorrections *mars_get_pointing_corrections(PigMission *m);

////////////////////////////////////////////////////////////////////////
// Creates and returns PigPointingModel based on input and command-line
// parameters.  Applies pointing correction if applicable.  In addition
// to returning PigPointingModel object, returns status code.
// Valid status return values:
// -1 Pointing correction file exists but doesn't contain matching solution
//    default PointingModel has been created
//  0  No Pointing Correction file has been provided, create default model
//  1  Match has been found in Pointing Correction file

/////////////////////////////////////////////////////////////////////// 
PigPointingModel *mars_create_pointing_model(PigCameraModel *camera_model, 
                               PigFileModel *file_model,
			       const char *sid,
			       PigPointingCorrections *pointing_corrections,
                               int &status);

/////////////////////////////////////////////////////////////////////// 
// Simplified form of mars_create_pointing_model that does all the
// grunt work to get sid and pointing_corrections and deals with the
// output.  The caller need only check for null (and does not need to
// print any errors, they're already printed).  The camera will have been
// pointed, and the navtable applied.  "descr" simply describes the
// file for error messages (e.g. "image 1", "stereo partner", etc).
/////////////////////////////////////////////////////////////////////// 
PigPointingModel *mars_create_pointing_model(PigCameraModel *camera_model,
				PigFileModel *file_model, char *descr);

////////////////////////////////////////////////////////////////////////
// Determines the minimum and maximum azimuth and elevation (in the passed-in
// coordinate system) for a set of input images by examining the corner and
// center-of-edge points.  This routine is not precise because the actual
// projection is not done... it examines only at the look vectors for each
// camera, ignoring any changes in camera location.  The centers-of-edges
// are done because some projections result in a convex curved image edge.
// Wrap_az defines the azimuth (degrees) you wish to wrap a complete mosaic 
// at.  Used in underlying functions. Values are returned in radians.
////////////////////////////////////////////////////////////////////////

void mars_get_azel_minmax(const int nids, PigPointingModel *pointing_in[],
			PigCameraModel *camera_in[], PigFileModel *file_in[],
			double &min_elev, double &max_elev,
			double &min_az, double &max_az, PigCoordSystem *cs,
			double wrap_az);

////////////////////////////////////////////////////////////////////////
// Determines the azimuth range for a set of images, where each is marked
// by az_low[i] and az_high[i].  This is complicated by the possibility
// of wrapping around (az=0/360); we want to find the smallest contiguous
// range that includes all the images.  The min_az and max_az are returned.
// If min_az > max_az, then the image wrapped around, i.e. the 0 point is
// in the middle.  All values are in radians.  Edges are determined only
// to the nearest degree. If the mosaic is full, the wrap will be placed
// at the wrap_az azimuth.  Note that coord system doesn't matter here.
////////////////////////////////////////////////////////////////////////

void mars_get_az_range(const int nids, double az_low[], double az_high[],
                double &min_az, double &max_az, double wrap_az);

////////////////////////////////////////////////////////////////////////
// Prints various information about the inputs and their pointing.
////////////////////////////////////////////////////////////////////////

void mars_print_inputs(int nids, PigPointingModel *pointing_in[],
		PigCameraModel *camera_in[], PigFileModel *file_models[],
		int homogeneous_inputs, char *mission, char *instrument);

////////////////////////////////////////////////////////////////////////
// mars_read_inputs.cc
//
// Reads a set of input files into memory, and optionally marks their
// borders (for footprinting).
//
// This routine requires the following parameters be in the PDF:
//
// PARM FOOTPRT TYPE=KEYWORD VALID=("NOFOOTPRINT", "FOOTPRINT", "OVERLAP") +
//              DEFAULT=NOFOOTPRINT
// PARM FOOT_DN TYPE=REAL DEFAULT=4096 COUNT=(1:32)
// PARM DNSCALE_IN TYPE=KEYWORD COUNT=1 VALID=("AUTOSCALE", "RESCALE", "NOSCALE") +
//              DEFAULT=AUTOSCALE
// PARM DNSCALE_OUT TYPE=KEYWORD COUNT=1 VALID=("STATIC", "DYNAMIC", "IDENTITY") +
//              DEFAULT=STATIC
//
// (only the FOOTPRINT keyword is used from FOOTPRT).  If the parameters
// are not present, no footprinting will be done.
// You can avoid freeing the input units by setting free_units to FALSE
// (in case they need to be re-used).
// If the radiometry model array itself is NULL, no rad correction will
// If an individual element is NULL, no correction will be done on that 
// file.  mars_setup() examines the RAD parameter to turn this on and off.
//
// This routine has the capability of reading a set of input files into
// SimpleImage objects, short int buffers, and float buffers.  
//
// If radiometric model exists and is provided, then radiometric correction
// will be performed on the input files.
//
// If brightness model exists and is provided, then brightness correction
// will be performed on the input files.
//
// If an element of the input (PigFileModel *) array is NULL, then that
// element is ignored, and its corresponding element in the output
// (SimpleImage<T> *) array is returned unchanged (NULL if passed in
// as NULL, unchanged image if passed in as an image).
////////////////////////////////////////////////////////////////////////

template <class T>
void mars_read_inputs(int first_input, int last_input, PigFileModel *files[],
                 SimpleImage<T> *images[], int MAX_NL, int MAX_NS, int band,
                 RadiometryModel *radiometric[] = NULL, 
                 PigBrtCorrModel *brt_corr[] = NULL);

template <class T>
void mars_read_inputs_hsi(int first_input, int last_input, PigFileModel *files[],
                 SimpleImage<T> *siImages[], int MAX_NL, int MAX_NS, int band,
                 RadiometryModel *radiometric[] = NULL, 
                 PigBrtCorrModel *brt_corr[] = NULL);

void mars_read_inputs(int first_input, int last_input, PigFileModel *files[],
                 short int *ibuf[], int MAX_NL, int MAX_NS, int band,
                 RadiometryModel *radiometric[] = NULL, 
                 PigBrtCorrModel *brt_corr[] = NULL);

//No radiometric correction.
void mars_read_inputs(int first_input, int last_input, PigFileModel *files[],
                 float *fbuf[], int MAX_NL, int MAX_NS, int band,
                 RadiometryModel *radiometric[] = NULL, 
                 PigBrtCorrModel *brt_corr[] = NULL);

//Does radiometric correction.
void mars_read_inputs_rad(int first_input,int last_input, PigFileModel *files[],
                 float *fbuf[], int MAX_NL, int MAX_NS, int band,
                 RadiometryModel *radiometric[] = NULL, 
                 PigBrtCorrModel *brt_corr[] = NULL);

void mars_read_inputs_hsi(int first_input, int last_input,PigFileModel *files[],
                 short int *ibuf[], int MAX_NL, int MAX_NS, int band,
                 RadiometryModel *radiometric[] = NULL, 
                 PigBrtCorrModel *brt_corr[] = NULL);

////////////////////////////////////////////////////////////////////////
// Adds overlap footprints and image numbers into the output image.
// Caller must supply a function to project an input coordinate (camera coords)
// into the the output (phys. coords, 0-based) with the following signature:
//    extern "C" int ProjectFunc(double in_line, double in_samp,
//				double *out_line, double *out_samp,
//				int input_number, void *proj_args);
// where "proj_args" is usually a structure of other parameters defined by
// the caller (the function should return 0 for success or non-zero for
// error, such as unprojectable point).  The routine writes directly to the
// given unit number, which must have a U_FORMAT of HALF and be open for
// update.
//
// This routine requires the following parameters be in the PDF:
//
// PARM NUMBER TYPE=KEYWORD VALID=("NUMBER","NONUMBER") DEFAULT="NONUMBER"
// PARM NUMBER_DN TYPE=REAL DEFAULT=4096 COUNT=(1:32)
// PARM NUMBER_ZOOM TYPE=INTEGER DEFAULT=1
// PARM FOOTPRT TYPE=KEYWORD VALID=("NOFOOTPRINT", "FOOTPRINT", "OVERLAP") +
//		DEFAULT=NOFOOTPRINT
// PARM FOOT_DN TYPE=REAL DEFAULT=4096 COUNT=(1:32)
// PARM FOOT_RANGE TYPE=INTEGER COUNT=0:2 DEFAULT=--
//
// (only the OVERLAP keyword is used from FOOTPRT).
////////////////////////////////////////////////////////////////////////

extern "C" {
    typedef int (*ProjectFunc)(double in_line, double in_samp,
				double *out_line, double *out_samp,
				int input_number, void *proj_args);
}

void mars_footprints(int unit, int nids, int nso, int nlo, int nbo,
		PigCameraModel *camera_in[], PigFileModel *file_in[],
		ProjectFunc func, void *proj_args);

////////////////////////////////////////////////////////////////////////
// mars_mosaic_bbox.cc
//
// Computes the bounding box for each input in the mosaic (in terms of output
// mosaic pixels) and writes it to a CSV output file (bbox_filee parameter).
// The corners will always be projected.  Additionally, it will project
// bbox_count pixels along each edge (evenly distributed).
//
// Caller must supply a function to project an input coordinate into the
// the output with the following signature:
//    extern "C" int ProjectFunc(double in_line, double in_samp,
//                              double *out_line, double *out_samp,
//                              int input_number, void *proj_args);
// where "proj_args" is usually a structure of other parameters defined by
// the caller (the function should return 0 for success or non-zero for
// error, such as unprojectable point).
//
// It requires a second function, which returns the width in pixels of
// the given number of degrees (usually 360) at the given point, as well
// as the line number for elevation=0 (for those projections where it makes
// sense) with the following signature:
//    extern "C" int MosaicWidthFunc(double line, double samp, double ndeg,
//		int *out_pix, double *out_line_zeroel,
//		int input_number, void *proj_args);
// where el is the elevation in degrees (needed for sin proj).  For projections
// with no azimuth wrap possible, return 0 for both values.
//
// The following parameters are expected in the PDF:
// PARM BBOX TYPE=STRING COUNT=(0:1) DEFAULT=--
// PARM BBOX_COUNT TYPE=INTEGER COUNT=1 DEFAULT=3
//
// If the bbox parameter is not supplied, we return right away.
//
// Polygons that wrap around in azimuth (possible only for CYL, CYP, SIN),
// will be duplicated with coordinates adjusted for both sides of the wrap.
// In other words, one instance of the polygon for the left side of the
// mosaic, another for the right.
//
// Images that include the zenith or nadir, have a few extra segments
// added to close the polygon around the zenith or nadir.  See the specific
// comments in the code.
//
// File format is a CSV where the each line is a separate image (image is
// on two lines if it wraps).  First column is the filename, subsequent 
// columns are line,samp pairs (1-based integer coordinates).  There are
// thus (4*bbox_count + 4 + 3) * 2 + 1 columns in the file.  That's 4 sides * 
// bbox_count, 4 corners, 3 extras for zenith/nadir wrap, times 2 for 
// line/samp, plus 1 for the filename.
////////////////////////////////////////////////////////////////////////

extern "C" {
    typedef int (*MosaicWidthFunc)(double line, double samp, double ndeg,
		int *out_pix, double *out_line_zeroel,
		int input_number, void *proj_args);
}
void mars_mosaic_bbox(int nids,
	PigCameraModel *camera_in[], PigFileModel *file_in[], int nlo, int nso,
        ProjectFunc func, MosaicWidthFunc wfunc, void *proj_args);

////////////////////////////////////////////////////////////////////////
// Writes the specified text to the given location in an output image.
// The routine writes directly to the given unit number, which must have
// a U_FORMAT of HALF and be open for update.  The center flag specifies
// where the given location is relative to the text.  Coordinates are 0-based.
//
// Suggested parameters for the PDF (to be read and passed in):
//
// PARM DN TYPE=REAL DEFAULT=4096 COUNT=(1:32)
// PARM ZOOM TYPE=INTEGER DEFAULT=1
//
// The given number of bands are written.  Any 0 values in dn are ignored
// (not written).
////////////////////////////////////////////////////////////////////////

enum WriteTextCenterFlag { Center, Upper_Left, Upper_Right,
			   Lower_Left, Lower_Right, Upper_Center,
			   Lower_Center, Center_Left, Center_Right };

void mars_write_text(int unit, int nso, int nlo, int nbo, char *string,
		     int x, int y,
                     int zoom, float dn[], WriteTextCenterFlag center);

int mars_create_tile_res_map(PigFileModel* file_model, SimpleImage<int> *& tI,
                         std::set<int> &tilingLevel);

////////////////////////////////////////////////////////////////////////
// Grow a window around the given point.  Window grows by adding one pixel
// in all four dimensions, creating a "box" (outline) of new pixels.  Window
// stops growing when there are no good points found in the last box, or when
// the window size reaches a maximum.
//
// Caller-supplied function determines whether or not a given pixel is "good"
// according to whatever criteria is desired.  The function can also store
// the pixel in another array, or gather statistics off it, or whatever.
////////////////////////////////////////////////////////////////////////

extern "C" {
    typedef int (*MarsWindowFunc)(void *p, int x, int y);
}

int mars_grow_window(
    MarsWindowFunc func,		// Function to call on each pt
    void *p,                            // arbitrary data passed to func
    int max_window,                     // conservative window max
    int x_current, int y_current,       // Center point of window
    int num_cols, int num_rows);        // size of image


#endif

