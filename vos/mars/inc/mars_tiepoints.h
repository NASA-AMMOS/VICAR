////////////////////////////////////////////////////////////////////////
// mars_tiepoints.h
//
// Tiepoint-related routines used by the mars* suite of programs.
//
// Note that these subroutines all use C++ linkage and are thus not
// callable from C or Fortran without bridges.
////////////////////////////////////////////////////////////////////////

#ifndef _MARS_TIEPOINTS_H
#define _MARS_TIEPOINTS_H

#include <stdio.h>		// for NULL
#include "mars_support.h"
#include "PigVector.h"

// Types of tiepoints

#define TIEPOINT_TRADITIONAL    0
#define TIEPOINT_FIDUCIAL       1
#define TIEPOINT_1D             2
#define TIEPOINT_INFINITY       3
#define TIEPOINT_ORBITAL_XY     4
#define TIEPOINT_ORBITAL_XYZ    5
#define TIEPOINT_AZIMUTH        6
#define TIEPOINT_ELEVATION      7
#define TIEPOINT_Z_SURFACE      8
#define TIEPOINT_DYNAMIC_XYZ    9
#define TIEPOINT_MISS_DISTANCE  10

#define NUM_TIEPOINT_TYPES	11

// Human-readable names for each type

static const char *tiepoint_types[NUM_TIEPOINT_TYPES] = {
        "Traditional",
        "Fiducial",
        "1-D",
        "Infinity",
        "Orbital XY",
        "Orbital XYZ",
        "Azimuth",
        "Elevation",
	"Z Surface",
	"Dynamic XYZ",
        "Miss Distance" };

// Structure for holding tiepoints

// Note:  Tiepoints are in PHYSICAL IMAGE coordinates!  Not camera coords.

struct TiePoint {
    int type;			// see TIEPOINT_* above
    int left_image;		// index into file list
    int right_image;		// index into file list
    int left_key;		// key used in tiepoint file for left image
    int right_key;		// key used in tiepoint file for right image
    double left_sample;
    double left_line;
// "right" and "corrected" are used for: Traditional, 1D, Infinity, Z-Surface,
// and Dynamic XYZ; "right" is used by Miss Distance
    double right_sample;	// Projected (estimated) values
    double right_line;
    double corrected_sample;	// Actual values in right image
    double corrected_line;
    double angle;		// for 1D, Azimuth, and Elevation (degrees)
    PigPoint xyz;		// for Fiducial, Orbital XY, Orbital XYZ,
				// Z Surface.  Also Dynamic XYZ (internal only)
    double quality;
    double residual;		// Error^2 after projection
    int interactive;		// true=human edited, false=computer generated
    int active;			// active or not
    PigCoordSystem *cs;		// Coordinate system for tiepoint's xyz

    int nav_residual = 0;              // Nav output?
    double left_init_residual_sample;  // left preBA residual in sample
    double left_init_residual_line;    
    double left_final_residual_sample; // left postBA residual in sample
    double left_final_residual_line;
    double right_init_residual_sample; // right preBA residual in sample
    double right_init_residual_line;
    double right_final_residual_sample;// right postBA residual in sample
    double right_final_residual_line;
    double init_miss_dist;             // preBA miss distance
    double final_miss_dist;
    PigPoint init_xyz;                 // preBA triangulated XYZ
    PigPoint final_xyz;
    int track;                         // track id (multi img obs, same XYZ)

};


#define MARS_MAX_TIEPOINTS_PER_EDIT 20000   // arbitrary
#define MARS_MAX_TIEPOINTS 500000	    // arbitrary

////////////////////////////////////////////////////////////////////////
// OLD-STYLE TIEPOINT FILE
////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////
// Load the tiepoints from a file.  Returns 0 for success, 1 for error
// opening file, 2 for EOF while reading file.  Error messages are NOT
// printed.
//
// For version 1, the file format is:
// mpfnav_tiepoints_list			(constant string)
// refimg					(integer)
// num_tiepoints				(integer)
//
// followed by lines of the format "%d %d %f %f %f %f %f %f %f" and
// the values left_image, right_image, left_sample, left_line,
// right_sample, right_line, corrected_sample, corrected_line, quality.
// refimg is ignored (it's a part of the nav solution, not the tiepoints
// themselves).
//
// For version 2, the file format is:
// mars_tiepoint_list v2			(constant string)
// num_tiepoints				(integer)
//
// followed by lines as above, with one field added to the end:  " %d"
// with the value interactive (a boolean).
////////////////////////////////////////////////////////////////////////

int mars_load_tiepoints(char *name, TiePoint tiepoints[], int &n);

////////////////////////////////////////////////////////////////////////
// Save the tiepoints to a file.  Returns 0 for success, 1 for file open
// error, and 2 for file write error.  Error messages are NOT printed.
// See mars_load_tiepoints for file format definition (only version 2 is
// supported).
////////////////////////////////////////////////////////////////////////

int mars_save_tiepoints(char *name, TiePoint tiepoints[], int n);

////////////////////////////////////////////////////////////////////////
// NEW-STYLE TIEPOINT FILE
////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////
// Load tiepoints from a file, the easy way.  Any tiepoints referencing
// images not found in the supplied image list will be discarded.  Note
// that returned keys may not match what was in the file, but will be
// self-consistent.  This means multiple tiepoint sets can be read at
// once, and any files with multiple keys will be consolidated into a
// single key.
//
// If the coord system is null, or if there is none in the file, no
// translations are done.  Otherwise, all XY/XYZ coordinates and az/el
// angles are translated into the frame supplied in the parameter.
// Note that if you are intending to adjust coordinate systems, you do
// NOT want translation here; you must pass in NULL.  If you are not
// adjusting coordinate systems, then allowing the transform to be done
// at load time improves efficiency.  Bottom line:  if NULL is given, the
// ties will contain the CS from the file; if a CS is given, all will
// contain that CS, with values translated accordingly.
//
// Returns 0 for success, -1 for memory allocation error, -2 for error
// reading the file, -3 for file not found.  (actually, it tries to continue
// on memory alloc errors, just skipping the item it failed on).
//
// Will also detect if the file is in the old tiepoint format, and call
// that read routine instead.
////////////////////////////////////////////////////////////////////////

int mars_load_tiepoints(
        char *name,             // filename
        TiePoint tiepoints[],   // Array of tiepoints to fill in
        int &n,                 // INPUT: size of tiepoints[]. OUT: # active
        PigFileModel *files[],  // File list
        int n_files,            // # in file list
        PigCoordSystem *cs,     // Desired CS (NOT what's in file) null==no conv
	PigMission *mission);	// Mission object

////////////////////////////////////////////////////////////////////////
// Load tiepoints from a file, the hard way.  Use this when you care about
// tiepoints not in your file list.  The list of ID's is returned (NOT input);
// indices are matched to that list.  The caller must do its own matching
// with its file list.  Note that returned keys may not match what was in
// the file, but will be self-consistent.  This means multiple tiepoint
// sets can be read at once, and any files with multiple keys will be
// consolidated into a single key.
//
// If the coord system is null, or if there is none in the file, no
// translations are done.  Otherwise, all XY/XYZ coordinates and az/el
// angles are translated into the frame supplied in the parameter.
// Note that if you are intending to adjust coordinate systems, you do
// NOT want translation here; you must pass in NULL.  If you are not
// adjusting coordinate systems, then allowing the transform to be done
// at load time improves efficiency.  Bottom line:  if NULL is given, the
// ties will contain the CS from the file; if a CS is given, all will
// contain that CS, with values translated accordingly.
//
// Returns 0 for success, -1 for memory allocation error, -2 for error
// reading the file, -3 for file not found.  (actually, it tries to continue
// on memory alloc errors, just skipping the item it failed on).
////////////////////////////////////////////////////////////////////////

int mars_load_tiepoints(
        char *name,             // filename
        TiePoint tiepoints[],   // Array of tiepoints to fill in
        int &n,                 // INPUT: size of tiepoints[]. OUT: # active
        char *unique_ids[],     // Returned list of unique ID's from file
        int keys[],             // Returned list of keys from file
        int &n_files,           // IN: size of arrays OUT: # in array
        PigCoordSystem *cs,     // Desired CS (NOT what's in file) null==no conv
	PigMission *m);		// Mission object

////////////////////////////////////////////////////////////////////////
// Save tiepoints to a file, the easy way.  This corresponds with the
// easy load; all tiepoints must reference only images in the file list.
// New keys are assigned based on the order of the supplied file list,
// starting at start_key (this allows later merging of separately-gathered
// tiepoint files).
//
// To reiterate:  left_key and right_key in the tiepoint list are ignored
// and are in fact reset by this method based on left_image/right_image.
//
// If the coordinate system is given, all points will be translated into
// that CS and written out.  If it is NOT given, no translation will be
// done.
//!!!! WARNING!!!! Currently, all CS's in the tiepoint list MUST BE
//!!!! CONSISTENT!!!!  What should happen is that they are grouped by
//!!!! CS and written out in different tiepoint sets based on CS (since
//!!!! the CS name is in the set header).  THIS IS NOT DONE NOW.  There is
//!!!! no facility to write multiple CS's in the file.  This could easily
//!!!! be done when needed, but no programs support this now so development
//!!!! has been deferred.  Hand-editing of the file works for the moment.
//
// Returns 0 for succes, -1 for error allocating memory, -2 for error
// writing the file, -3 for error opening the file.
////////////////////////////////////////////////////////////////////////

int mars_save_tiepoints(
        char *name,             // filename
        TiePoint tiepoints[],   // tiepoints to save
        int n,                  // # of tiepoints
        PigFileModel *files[],  // list of files
        int n_files,            // size of file list
        int start_key,          // starting # for keys
        PigCoordSystem *cs);    // what XYZ's/angles are in, may be null

////////////////////////////////////////////////////////////////////////
// Save tiepoints to a file, the hard way.  Use this when you want to
// maintain existing tiepoints, which may not be in the current file list.
// Basically, you're rolling your own file list.  Keys must be supplied,
// and must be valid in the tiepoint list.
//
// If the coordinate system is given, all points will be translated into
// that CS and written out.  If it is NOT given, no translation will be
// done.
//!!!! WARNING!!!! Currently, all CS's in the tiepoint list MUST BE
//!!!! CONSISTENT!!!!  What should happen is that they are grouped by
//!!!! CS and written out in different tiepoint sets based on CS (since
//!!!! the CS name is in the set header).  THIS IS NOT DONE NOW.  There is
//!!!! no facility to write multiple CS's in the file.  This could easily
//!!!! be done when needed, but no programs support this now so development
//!!!! has been deferred.  Hand-editing of the file works for the moment.
//
// Returns 0 for succes, -1 for error allocating memory, -2 for error
// writing the file, -3 for error opening the file.
////////////////////////////////////////////////////////////////////////

int mars_save_tiepoints(
        char *name,
        TiePoint tiepoints[],
        int n,
        char *unique_ids[],     // unique ID's from files
        int keys[],             // specific keys per file (MAY NOT be null)
        int n_files,            // number of both the above
        PigCoordSystem *cs);

////////////////////////////////////////////////////////////////////////
// Print out a tiepoint list, or a single tie from the list
////////////////////////////////////////////////////////////////////////

void mars_print_tiepoint_list(TiePoint *tiepoints, int n);
void mars_print_one_tiepoint(TiePoint *tiepoint, int which);

////////////////////////////////////////////////////////////////////////
// Edits a single tiepoint.  Returns 0 if okay, -1 if the user didn't change
// the tiepoint, or 1 if the user aborted (so the caller should exit the
// program).
////////////////////////////////////////////////////////////////////////

int mars_edit_tiepoint(int i, int &n_overlaps, TiePoint tiepoints[],
			PigFileModel *file_models[]);

////////////////////////////////////////////////////////////////////////
// Edits a whole set of tiepoints.  Returns 0 if okay, or 1 if the user
// aborted with Special Exit Status (so the caller should exit the program).
//
// All tiepoints in the array must belong to the same image pair as
// specified in img1 and img2 (although they may be in either order).
// img1 and img2 are specified rather than getting it from the tiepoints
// in case there are 0 input tiepoints.
////////////////////////////////////////////////////////////////////////

int mars_edit_tiepoints(int &n_overlaps, int img1, int img2,
                        TiePoint tiepoints[], PigFileModel *file_models[]);

////////////////////////////////////////////////////////////////////////
// Remove bad points from the tiepoint list.  Bad points are defined as
// points with a quality <= thresh (usually 0.0).
////////////////////////////////////////////////////////////////////////

void mars_remove_bad_points(TiePoint tiepoints[], int &n_overlaps,
			double thresh);

////////////////////////////////////////////////////////////////////////

#endif

