////////////////////////////////////////////////////////////////////////
// mars_overlaps.h
//
// Overlap-related routines used by marsint/marsbias.  These are very similar
// to tiepoints; see mars_tiepoints.h.
//
// Note that these subroutines all use C++ linkage and are thus not
// callable from C or Fortran without bridges.
////////////////////////////////////////////////////////////////////////

#ifndef _MARS_OVERLAPS_H
#define _MARS_OVERLAPS_H

#include <stdio.h>		// for NULL
#include "mars_support.h"

// Max # of overlaps allowed (just for array dimensioning)

#define MAX_OVERLAPS 100000

// Types of overlaps

#define OVERLAP_MEAN_STDEV	0

// Max number of images in one overlap.  Should be made dynamic, eventually!

#define MAX_OVERLAP_IMAGES	100

#define OVERLAP_MEAN_STDEV	0
#define OVERLAP_OVERALL		1
#define OVERLAP_HSI		2
#define OVERLAP_OVERALL_HSI	3

#define NUM_OVERLAP_TYPES	4

// Human-readable names for each type

static const char *overlap_types[NUM_OVERLAP_TYPES] = {
        "Mean/Stdev",
	"Overall Mean/Stdev",
	"HSI Space Mean/Stdev",
	"Overall HSI Mean/Stdev" };

// Structure for holding statistics for one image in an overlap

struct OverlapStats {
    int image;			// index into file list
    int key;			// key used in overlap file

    // location in the image - saved

    double line;
    double samp;

    // actual stats - saved
    double mean;
    double stdev;

    // for gathering stats - not saved
    double sum_dn;		// Sum of DN
    double sum2_dn;		// Sum of DN^2
};

// Note that OVERLAP_MEAN_STDEV and OVERLAP_OVERALL look the same except that
// there is only one image for OVERLAP_OVERALL, and radius is not relevant.
// OVERLAP_HSI and OVERLAP_MEAN_STDEV look the same.

struct Overlap {
    int active;			// true if we should use this one
    int type;			// see OVERLAP_* above.
    int n_images;		// number of images in overlap (0==inactive)
    int n_pixels;		// number of pixels in overlap
    double radius;		// radius in pixels used to limit size (0==none)
    double error;		// error when fitting (not saved)
    OverlapStats stats[MAX_OVERLAP_IMAGES];	// make this dynamic!
};

////////////////////////////////////////////////////////////////////////
// Load overlaps from a file, the easy way.  Any overlaps referencing
// images not found in the supplied image list will be discarded.  Note
// that returned keys may not match what was in the file, but will be
// self-consistent.  This means multiple overlap sets can be read at
// once, and any files with multiple keys will be consolidated into a
// single key.
//
// Returns 0 for success, -1 for memory allocation error, -2 for error
// reading the file, -3 for file not found.  (actually, it tries to continue
// on memory alloc errors, just skipping the item it failed on).
////////////////////////////////////////////////////////////////////////

int mars_load_overlaps(
        char *name,             // filename
        Overlap overlaps[],	// Array of overlaps to fill in
        int &n,                 // INPUT: size of overlaps[]. OUT: # active
        PigFileModel *files[],  // File list
        int n_files);           // # in file list

////////////////////////////////////////////////////////////////////////
// Load overlaps from a file, the hard way.  Use this when you care about
// overlaps not in your file list.  The list of ID's is returned (NOT input);
// indices are matched to that list.  The caller must do its own matching
// with its file list.  Note that returned keys may not match what was in
// the file, but will be self-consistent.  This means multiple overlap
// sets can be read at once, and any files with multiple keys will be
// consolidated into a single key.
//
// Returns 0 for success, -1 for memory allocation error, -2 for error
// reading the file, -3 for file not found.  (actually, it tries to continue
// on memory alloc errors, just skipping the item it failed on).
////////////////////////////////////////////////////////////////////////

int mars_load_overlaps(
        char *name,             // filename
        Overlap overlaps[],	// Array of overlaps to fill in
        int &n,                 // INPUT: size of overlaps[]. OUT: # active
        char *unique_ids[],     // Returned list of unique ID's from file
        int keys[],             // Returned list of keys from file
        int &n_files,           // IN: size of arrays OUT: # in array
	PigMission *m);		// Mission object

////////////////////////////////////////////////////////////////////////
// Save overlaps to a file, the easy way.  This corresponds with the
// easy load; all overlaps must reference only images in the file list.
// New keys are assigned based on the order of the supplied file list,
// starting at start_key (this allows later merging of separately-gathered
// overlap files).
//
// To reiterate:  "key" in the overlap list is ignored and is in fact
// reset by this method based on "image".
//
// If the coordinate system is given, all points will be translated into
// that CS and written out.  If it is NOT given, no translation will be
// done.
//
// Returns 0 for succes, -1 for error allocating memory, -2 for error
// writing the file, -3 for error opening the file.
////////////////////////////////////////////////////////////////////////

int mars_save_overlaps(
        char *name,             // filename
        Overlap overlaps[],	// overlaps to save
        int n,                  // # of overlaps
        PigFileModel *files[],  // list of files
        int n_files,            // size of file list
        int start_key);         // starting # for keys

////////////////////////////////////////////////////////////////////////
// Save overlaps to a file, the hard way.  Use this when you want to
// maintain existing overlaps, which may not be in the current file list.
// Basically, you're rolling your own file list.  Keys must be supplied,
// and must be valid in the overlap list.
//
// Returns 0 for succes, -1 for error allocating memory, -2 for error
// writing the file, -3 for error opening the file.
////////////////////////////////////////////////////////////////////////

int mars_save_overlaps(
        char *name,
        Overlap overlaps[],
        int n,
        char *unique_ids[],     // unique ID's from files
        int keys[],             // specific keys per file (MAY NOT be null)
        int n_files);           // number of both the above

////////////////////////////////////////////////////////////////////////
// Print out a overlap list
////////////////////////////////////////////////////////////////////////

// void mars_print_overlap_list(Overlap *overlaps, int n);

#endif

