#include "mars_tiepoints.h"

#include "PigModelBase.h"
#include "PigMission.h"
#include "PigCoordSystem.h"
#include "PigFileModel.h"
#include "PigXerces.h"

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define PARSE_ERROR -999999

////////////////////////////////////////////////////////////////////////
// WARNING
// Methods in this file are VERY similar to those in mars_load_overlaps.cc.
// Make sure to maintain in parallel!!!!
////////////////////////////////////////////////////////////////////////

// Internal support routines.  See each for usage comments.

static int mars_read_tiepoint_set(DOMElement *tpt_set, int &n_images,
	char **&ids, int *&keys, int &n_tiepoints, TiePoint *&tiepoints,
	PigCoordSystem *&set_cs, PigMission *mission);

// These are internal support routines but they are reused in
// mars_load_overlaps.cc.  So they are not declared static, but they
// are not in the include file, either.

int mars_tiepoint_parse_int(DOMElement *elem, char *attrib);
double mars_tiepoint_parse_float(DOMElement *elem, char *attrib);
double mars_tiepoint_parse_float_noerr(DOMElement *elem, char *attrib);
int mars_tiepoint_map_key(int key, int *old_map, int *new_map,
							int n_maps);
int mars_tiepoints_xlate_key(int set_key, int n_set_files,
	int *set_keys, char **set_ids, int n_files, int *keys, char **ids);
int mars_tiepoints_xlate_key2(int set_key, int n_set_files,
	int *set_keys, char **set_ids, int n_files, PigFileModel **models);

////////////////////////////////////////////////////////////////////////
// OLD STYLE
//
// mars_load_tiepoints.cc
//
// Loads the tiepoints from a file.  Returns 0 for success, 1 for error
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

int mars_load_tiepoints(char *name, TiePoint tiepoints[], int &n)
{
    FILE *fd;
    char line[150];
    int i;
    int version = 1;

    // read tiepoints list

    if ((fd = fopen(name, "r")) == NULL) {
	return 1;		// Error opening file!
    }
    fgets(line, sizeof(line), fd);		// header
    if (strstr(line, "v2") != NULL)
	version = 2;

    if (version == 1) {
        fgets(line, sizeof(line), fd);		// reference image (ignored)
    }
    fgets(line, sizeof(line), fd);
    sscanf(line, "%d", &n);                       // # of tiepoints

    for (i = 0; i < n; i++) {
        if (fgets(line, sizeof(line), fd) == NULL) {
	    fclose(fd);
	    return 2;			// Error: EOF while reading file!
        }
	if (version == 1) {
            sscanf(line, "%d %d %lf %lf %lf %lf %lf %lf %lf",
                        &tiepoints[i].left_image,
                        &tiepoints[i].right_image,
                        &tiepoints[i].left_sample, &tiepoints[i].left_line,
                        &tiepoints[i].right_sample, &tiepoints[i].right_line,
                        &tiepoints[i].corrected_sample,
                        &tiepoints[i].corrected_line,
                        &tiepoints[i].quality);
	    // Version 1 had no interactive flag, so set it if quality == 1.0
	    // (this was used as a flag)
	    tiepoints[i].interactive = (tiepoints[i].quality == 1.0);
	}
	else {			// version 2
            sscanf(line, "%d %d %lf %lf %lf %lf %lf %lf %lf %d",
                        &tiepoints[i].left_image,
                        &tiepoints[i].right_image,
                        &tiepoints[i].left_sample, &tiepoints[i].left_line,
                        &tiepoints[i].right_sample, &tiepoints[i].right_line,
                        &tiepoints[i].corrected_sample,
                        &tiepoints[i].corrected_line,
                        &tiepoints[i].quality,
			&tiepoints[i].interactive);
	}
	tiepoints[i].left_key = tiepoints[i].left_image;
	tiepoints[i].right_key = tiepoints[i].right_image;
	tiepoints[i].cs = NULL;		// no CS in old files
	tiepoints[i].active = TRUE;
	tiepoints[i].type = TIEPOINT_TRADITIONAL;
    }
    fclose(fd);

    return 0;			// success
}

////////////////////////////////////////////////////////////////////////
// NEW STYLE
//
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
        int &n_ties,            // INPUT: size of tiepoints[]. OUT: # active
        char *unique_ids[],     // Returned list of unique ID's from file
        int keys[],             // Returned list of keys from file
        int &n_files,           // IN: size of arrays OUT: # in array
        PigCoordSystem *cs,     // Desired CS (NOT what's in file) null==no conv
	PigMission *mission)	// Mission object
{
    char msg[1024];
    PigCoordSystem *file_cs;
    int max_files = n_files;	// save maxes
    n_files = 0;
    int n_tiepoints_max = n_ties;
    n_ties = 0;

    PigXerces::initialize();

    DOMDocument *doc = PigXerces::parseFile(name);
    if (doc == NULL) {
	PigXerces::close();
	return -3;		// problem with document
    }

    DOMElement *root = doc->getDocumentElement();   // should be tiepoint_file
    char *version = PigXerces::getAttribute(root, "version");
    if (strcmp(version,"1.0") != 0) {
	PigModelBase::printStaticMsg("Version error reading tiepoint file!  Continuing as version 1.0", PigMsgError);
    }
    XMLString::release(&version);

    ////////
    // Get all the tiepoint sets, process each in turn
    ////////

    DOMNodeList *sets = PigXerces::getElementsByTagName(root, "tiepoint_set");

    for (int set = 0; set < sets->getLength(); set++) {

	DOMElement *tpt_set = PigXerces::nextElement(sets, set);
	if (tpt_set == NULL) {
	    PigModelBase::printStaticMsg(
		    "Expected Element type not found in tiepoint_set; ignored",
		    PigMsgWarning);
	    continue;
	}

	char **set_ids;
	int *set_keys;
	TiePoint *set_tiepoints;
	int n_set_files, n_set_tiepoints;
	PigCoordSystem *set_cs;

	// Read in the tiepoint set

	mars_read_tiepoint_set(tpt_set, n_set_files, set_ids, set_keys,
		n_set_tiepoints, set_tiepoints, set_cs, mission);

	if (n_set_files == 0 || n_set_tiepoints == 0) {
	    if (set_ids) {
		for (int q=0; q<n_set_files; q++)
		    delete set_ids[q];
		delete set_ids;
	    }
	    if (set_keys) delete set_keys;
	    if (set_tiepoints) delete set_tiepoints;
	    continue;
	}

	// Merge this set in with the main set
	// First, merge in the files

	int i, j;

	for (i=0; i < n_set_files; i++) {
	    int found = FALSE;
	    for (j=0; j < n_files; j++) {
		if (strcasecmp(set_ids[i], unique_ids[j]) == 0) { // found match
		    found = TRUE;
		    break;
		}
	    }

	    // If we found a match, ignore the entry... it'll get matched.
	    // If not, add it to the list.

	    if (!found) {
		if (n_files == max_files) {
		    sprintf(msg, "Out of memory reading tiepoint file.  Must ignore image %s", set_ids[i]);
		    PigModelBase::printStaticMsg(msg, PigMsgError);
		    continue;
		}
		unique_ids[n_files] = strdup(set_ids[i]);
		keys[n_files] = n_files;		// use index as key
		n_files++;
	    }
	}

	// Now, merge in the tiepoints.  We don't need the keys any more...
	// we simply use them to look up the name in the set's table, then
	// look up that name in the main table.

	for (i = 0; i < n_set_tiepoints; i++) {

	    // Left-side first

	    int key = mars_tiepoints_xlate_key(
			set_tiepoints[i].left_key,
			n_set_files, set_keys, set_ids,
			n_files, keys, unique_ids);

	    if (key == -1) {
		PigModelBase::printStaticMsg("Problem is in a left_key", PigMsgError);
		continue;
	    }
	    set_tiepoints[i].left_key = key;

	    for (int k = 0; k < n_files; k++) {	// Set image based on key
	        if (key == keys[k]) {
		    set_tiepoints[i].left_image = k;
		    break;
	        }
	    }

	    // Now right-side
	    if (set_tiepoints[i].type == TIEPOINT_TRADITIONAL ||
		set_tiepoints[i].type == TIEPOINT_MISS_DISTANCE ||
	        set_tiepoints[i].type == TIEPOINT_1D ||
	        set_tiepoints[i].type == TIEPOINT_INFINITY ||
	        set_tiepoints[i].type == TIEPOINT_Z_SURFACE ||
		set_tiepoints[i].type == TIEPOINT_DYNAMIC_XYZ) {

	        key = mars_tiepoints_xlate_key(
			set_tiepoints[i].right_key,
			n_set_files, set_keys, set_ids,
			n_files, keys, unique_ids);

	        if (key == -1) {
		    PigModelBase::printStaticMsg("Problem is in a right_key", PigMsgError);
		    continue;
	        }
		set_tiepoints[i].right_key = key;

	        for (int k = 0; k < n_files; k++) {  // Set image based on key
	            if (key == keys[k]) {
		        set_tiepoints[i].right_image = k;
		        break;
	            }
	        }
	    }

	    // Finally, translate coord systems if necessary

	    if (set_cs != NULL && cs != NULL) {	// either NULL -> no xlate
		if (set_tiepoints[i].type == TIEPOINT_FIDUCIAL ||
		    set_tiepoints[i].type == TIEPOINT_ORBITAL_XY ||
		    set_tiepoints[i].type == TIEPOINT_ORBITAL_XYZ) {

		    PigPoint xyz = cs->convertPoint(set_tiepoints[i].xyz,
						set_cs);
		    set_tiepoints[i].xyz = xyz;
		    set_tiepoints[i].cs = cs;
		}

		if (set_tiepoints[i].type == TIEPOINT_AZIMUTH ||
		    set_tiepoints[i].type == TIEPOINT_ELEVATION) {

		    if (cs != set_cs) {
			PigModelBase::printStaticMsg("Coord system translations for Azimuth, Elevation, and Z-Surface tiepoints are not yet implemented.  Translation not performed", PigMsgWarning);
		    }
		}
	    }

	    // We have a good tiepoint.  Copy it over, and activate it.

	    if (n_ties == n_tiepoints_max) {
		PigModelBase::printStaticMsg("Memory error in final tiepoint array!", PigMsgError);
		continue;			// sigh...
	    }

	    tiepoints[n_ties] = set_tiepoints[i];
	    tiepoints[n_ties].active = TRUE;
	    n_ties++;
	}

	// Deallocate memory from reading

	for (int q=0; q<n_set_files; q++)
	    delete set_ids[q];
	delete set_ids;
	delete set_keys;
	delete set_tiepoints;

    }

    doc->release();

    PigXerces::close();

    return 0;		// done!!!
}

////////////////////////////////////////////////////////////////////////
// Internal routine to read a single tiepoint_set.
// ids, keys, and tiepoints are allocated, as well as each element in ids.
// Caller must free them.
////////////////////////////////////////////////////////////////////////
int mars_read_tiepoint_set(
		DOMElement *tpt_set,		// "tiepoint_set" element
		int &n_images,			// number of images (ids/keys)
		char **&ids,			// list of id's (alloc'd)
		int *&keys,			// list of keys
		int &n_tiepoints,		// number of tiepoints
		TiePoint *&tiepoints,		// list of tiepoints
		PigCoordSystem *&set_cs,	// returned coord system
		PigMission *mission)		// Mission object
{
    char msg[1024];

    ids = NULL;				// in case of early return
    keys = NULL;
    tiepoints = NULL;
    n_images = n_tiepoints = 0;

    ////////
    // Get the coordinate system reference, if any
    ////////

    PigCSReference *csref = new PigCSReference(mission, tpt_set,
					"reference_frame");

    set_cs = NULL;
    if (csref->getFrameName() != NULL) {		// only if given
	set_cs = mission->getCoordSystem(csref);
    }

    ////////
    // Get the list of images and keys that go with them
    ////////

    DOMElement *images_el = PigXerces::getOneElement(tpt_set, "images");
    if (images_el == NULL)
	return 0;			// No images, give up on this set


    DOMNodeList *image_nodes = PigXerces::getElementsByTagName(
							images_el, "image");

    int n_images_max = image_nodes->getLength();
    if (n_images_max == 0)
	return 0;			// can't use the tiepoints...

    ids = new char *[n_images_max];
    keys = new int[n_images_max];
    if (ids == NULL || keys == NULL) {
	PigModelBase::printStaticMsg("ERROR: memory error reading tiepoint file image list!", PigMsgError);
	if (keys) delete keys;
	if (ids) delete ids;
	keys = NULL;
	ids = NULL;
	return -1;				// done with this set
    }

    // Hold mapping of old (in file) to new (reassigned) keys, in case
    // the same image occurs twice in the <image> list.

    int *old_map = new int[n_images_max];
    int *new_map = new int[n_images_max];
    if (old_map == NULL || new_map == NULL) {
	PigModelBase::printStaticMsg("ERROR: memory error alloc'ing map while reading tiepoint file!  File not read!", PigMsgError);
	if (old_map) delete old_map;
	if (new_map) delete new_map;
	if (keys) delete keys;
	if (ids) delete ids;
	keys = NULL;
	ids = NULL;
	return -1;
    }
    int n_map = 0;

    // Now read each <image> entry

    for (int img = 0; img < n_images_max; img++) {

	DOMElement *image_el = PigXerces::nextElement(image_nodes, img);

	char *id = PigXerces::getAttribute(image_el, "unique_id");

	if (id == NULL || strlen(id) == 0) {
	    sprintf(msg, "Error: Malformed unique_id attribute in <image> tag; id=%s, key=%s",
			(id==NULL)?"NULL":id);
	    PigModelBase::printStaticMsg(msg, PigMsgError);
	    XMLString::release(&id);
	    continue;			// skip this entry
	}
	int key = mars_tiepoint_parse_int(image_el, "key");
	if (key == PARSE_ERROR) {
	    XMLString::release(&id);
	    continue;				// A problem: next!
	}

	// See if this entry exists in ids already

	int handled = FALSE;
	for (int j=0; j < n_images; j++) {
	    if (strcasecmp(id, ids[j]) == 0) {		// Duplicate ID!
		handled = TRUE;

		if (key == keys[j]) {
		    XMLString::release(&id);
		    continue;		// keys match, it's okay
		}

		// Add entry to old->new map

		old_map[n_map] = key;
		new_map[n_map] = keys[j];
		n_map++;
		XMLString::release(&id);
		break;			// done with loop
	    }

	    // At this point the unique ID is different.  Sanity-check
	    // the key, make sure we don't have dups.

	    if (key == keys[j]) {
		sprintf(msg, "DUPLICATE KEY in tiepoint file: %d!!  Second entry is ignored; results may be suspect", key);
		PigModelBase::printStaticMsg(msg, PigMsgError);
		handled = TRUE;
		XMLString::release(&id);
		break;			// done with loop
	    }
	}

	if (!handled) {			// new entry needed
	    ids[n_images] = strdup(id);
	    keys[n_images] = key;
	    n_images++;
	}
	XMLString::release(&id);
    }

    ////////
    // Now read the actual tiepoints
    // note: at this point, ids/keys are good, so we return them even if
    // there is an error in the tiepoint processing
    ////////

    DOMElement *tiepoints_el = PigXerces::getOneElement(tpt_set, "tiepoints");
    if (tiepoints_el == NULL) {
	if (old_map) delete old_map;
	if (new_map) delete new_map;
	return -1;			// No tiepoints; give up on this set
    }

    DOMNodeList *tiepoint_nodes = PigXerces::getElementsByTagName(
							tiepoints_el, "tie");

    int n_tiepoints_max = tiepoint_nodes->getLength();
    tiepoints = new (std::nothrow) TiePoint[n_tiepoints_max];
    if (tiepoints == NULL) {
	PigModelBase::printStaticMsg("ERROR: memory error (tiepoint array) reading tiepoint file!", PigMsgError);
	if (old_map) delete old_map;
	if (new_map) delete new_map;
	return -1;				// done with this set
    }

    for (int t = 0; t < n_tiepoints_max; t++) {

	DOMElement *tie_el = PigXerces::nextElement(tiepoint_nodes, t);

	int type = mars_tiepoint_parse_int(tie_el, "type");
	if (type == PARSE_ERROR)
	    continue;			// a problem (message printed already)

	TiePoint tt;			// temp tiepoint for use during parsing
	tt.type = type;			// set "invalid" values
	tt.left_image = -1;
	tt.right_image = -1;
	tt.left_key = -1;
	tt.right_key = -1;
	tt.active = FALSE;

	// Parse each tiepoint type slightly differently

	////////
	// ALL have left_key
	////////

	tt.left_key = mars_tiepoint_parse_int(tie_el, "left_key");
	if (tt.left_key == PARSE_ERROR)
	    continue;			// bad, next (msg printed already)

	tt.left_key = mars_tiepoint_map_key(tt.left_key,
						old_map, new_map, n_map);

	////////
	// SOME have right_key
	////////

	if (type == TIEPOINT_TRADITIONAL ||
	    type == TIEPOINT_MISS_DISTANCE ||
	    type == TIEPOINT_1D ||
	    type == TIEPOINT_INFINITY ||
	    type == TIEPOINT_Z_SURFACE ||
	    type == TIEPOINT_DYNAMIC_XYZ) {

	    tt.right_key = mars_tiepoint_parse_int(tie_el, "right_key");
	    if (tt.right_key == PARSE_ERROR)
		continue;		// bad, next (msg printed already)

	    tt.right_key = mars_tiepoint_map_key(tt.right_key,
						old_map, new_map, n_map);
	}

	////////
	// ALL have <left>
	////////

	DOMElement *left_el = PigXerces::getOneElement(tie_el, "left");
	if (left_el == NULL) {
	    PigModelBase::printStaticMsg("ERROR: missing <left> tag in tiepoint file", PigMsgError);
	    continue;				// skip this one
	}

	tt.left_sample = mars_tiepoint_parse_float(left_el, "samp");
	if (tt.left_sample == PARSE_ERROR)
	    continue;				// skip it
	tt.left_line = mars_tiepoint_parse_float(left_el, "line");
	if (tt.left_line == PARSE_ERROR)
	    continue;				// skip it

	////////
	// SOME have <projected>
	////////

	if (type == TIEPOINT_TRADITIONAL ||
	    type == TIEPOINT_MISS_DISTANCE ||
	    type == TIEPOINT_1D ||
	    type == TIEPOINT_INFINITY ||
	    type == TIEPOINT_Z_SURFACE ||
	    type == TIEPOINT_DYNAMIC_XYZ) {

	    DOMElement *proj_el = PigXerces::getOneElement(tie_el,"projected");
	    if (proj_el == NULL) {
		PigModelBase::printStaticMsg("WARNING: missing <projected> tag in tiepoint file", PigMsgWarning);
		// but keep parsing if it's missing
	    }
	    else {				// found it
		tt.right_sample = mars_tiepoint_parse_float(proj_el, "samp");
		if (tt.right_sample == PARSE_ERROR)
		    continue;				// skip it
		tt.right_line = mars_tiepoint_parse_float(proj_el, "line");
		if (tt.right_line == PARSE_ERROR)
		    continue;				// skip it
	    }
	}

	////////
	// SOME have <right>
	////////

	if (type == TIEPOINT_TRADITIONAL ||
	    type == TIEPOINT_MISS_DISTANCE ||
	    type == TIEPOINT_1D ||
	    type == TIEPOINT_INFINITY ||
	    type == TIEPOINT_Z_SURFACE ||
	    type == TIEPOINT_DYNAMIC_XYZ) {

	    DOMElement *right_el = PigXerces::getOneElement(tie_el, "right");
	    if (right_el == NULL) {
		PigModelBase::printStaticMsg("WARNING: missing <right> tag in tiepoint file", PigMsgWarning);
		continue;		// skip it
	    }

	    tt.corrected_sample = mars_tiepoint_parse_float(right_el, "samp");
	    if (tt.corrected_sample == PARSE_ERROR)
		    continue;				// skip it
	    tt.corrected_line = mars_tiepoint_parse_float(right_el, "line");
		if (tt.corrected_line == PARSE_ERROR)
		    continue;				// skip it
	}

	////////
	// SOME have <xyz> or <xy> or <z>
	////////

	if (type == TIEPOINT_FIDUCIAL ||
	    type == TIEPOINT_ORBITAL_XYZ ||
	    type == TIEPOINT_ORBITAL_XY ||
	    type == TIEPOINT_Z_SURFACE) {

	    DOMElement *xyz_el;

	    if (type == TIEPOINT_ORBITAL_XY)
	        xyz_el = PigXerces::getOneElement(tie_el, "xy");
	    else if (type == TIEPOINT_Z_SURFACE)
	        xyz_el = PigXerces::getOneElement(tie_el, "z");
	    else
	        xyz_el = PigXerces::getOneElement(tie_el, "xyz");

	    if (xyz_el == NULL) {
		PigModelBase::printStaticMsg("WARNING: missing <xy> or <xyz> or <z> tag in tiepoint file", PigMsgWarning);
		continue;		// skip it
	    }

	    if (type == TIEPOINT_Z_SURFACE)
		tt.xyz.setX(0.0);
	    else {
	        tt.xyz.setX(mars_tiepoint_parse_float(xyz_el, "x"));
	        if (tt.xyz.getX() == PARSE_ERROR)
		    continue;				// skip it
	    }

	    if (type == TIEPOINT_Z_SURFACE)
		tt.xyz.setY(0.0);
	    else {
	        tt.xyz.setY(mars_tiepoint_parse_float(xyz_el, "y"));
	        if (tt.xyz.getY() == PARSE_ERROR)
		    continue;				// skip it
	    }

	    if (type == TIEPOINT_ORBITAL_XY)
		tt.xyz.setZ(0.0);
	    else {
	        tt.xyz.setZ(mars_tiepoint_parse_float(xyz_el, "z"));
	        if (tt.xyz.getZ() == PARSE_ERROR)
		    continue;				// skip it
	    }
	}

	////////
	// SOME have <angle>
	////////

	if (type == TIEPOINT_1D ||
	    type == TIEPOINT_AZIMUTH ||
	    type == TIEPOINT_ELEVATION) {

	    DOMElement *angle_el = PigXerces::getOneElement(tie_el, "angle");
	    if (angle_el == NULL) {
		PigModelBase::printStaticMsg("WARNING: missing <angle> tag in tiepoint file", PigMsgWarning);
		continue;		// skip it
	    }

	    tt.angle = mars_tiepoint_parse_float(angle_el, "degrees");
	    if (tt.angle == PARSE_ERROR)
		continue;				// skip it
	}

	////////
	// ALL have <flags>
	////////

	DOMElement *flags_el = PigXerces::getOneElement(tie_el, "flags");
	if (flags_el == NULL) {
	    PigModelBase::printStaticMsg("ERROR: missing <flags> tag in tiepoint file", PigMsgError);
	    continue;				// skip this one
	}

	tt.quality = mars_tiepoint_parse_float(flags_el, "quality");
	if (tt.quality == PARSE_ERROR)
	    continue;				// skip it
	tt.interactive = mars_tiepoint_parse_int(flags_el, "interactive");
	if (tt.interactive == PARSE_ERROR)
	    continue;				// skip it

	tt.residual = mars_tiepoint_parse_float_noerr(flags_el, "residual");
	if (tt.residual == PARSE_ERROR)		// okay, this is new
	    tt.residual = 0.0;
	else
	    tt.residual = tt.residual * tt.residual;

	////////
	// DONE READING - check map for any keys to be translated
//!!!! is this not done above, by mars_tiepoint_map_key????!!!!
	////////

	int k;
	for (k=0; k < n_map; k++) {
	    if (tt.left_key == old_map[k]) {
		tt.left_key = new_map[k];	// replace
		break;
	    }
	}

	if (type == TIEPOINT_TRADITIONAL ||
	    type == TIEPOINT_MISS_DISTANCE ||
	    type == TIEPOINT_1D ||
	    type == TIEPOINT_INFINITY ||
	    type == TIEPOINT_Z_SURFACE ||
	    type == TIEPOINT_DYNAMIC_XYZ) {		// right side

	    for (k=0; k < n_map; k++) {
	        if (tt.right_key == old_map[k]) {
		    tt.right_key = new_map[k];	// replace
		    break;
	        }
	    }
	}

	////////
	// Set up left_image/right_image indices
	////////

	tt.left_image = tt.right_image = -1;

	for (k = 0; k < n_images; k++) {
	    if (tt.left_key == keys[k]) {
		tt.left_image = k;
		break;
	    }
	}
	if (type == TIEPOINT_TRADITIONAL ||
	    type == TIEPOINT_MISS_DISTANCE ||
	    type == TIEPOINT_1D ||
	    type == TIEPOINT_INFINITY ||
	    type == TIEPOINT_Z_SURFACE ||
	    type == TIEPOINT_DYNAMIC_XYZ) {		// right side

	    for (k = 0; k < n_images; k++) {
		if (tt.right_key == keys[k]) {
		    tt.right_image = k;
		    break;
		}
	    }
	}

	if (tt.left_image == -1) {
	    sprintf(msg, "Left key %d not found in tiepoint set!", tt.left_key);
	    PigModelBase::printStaticMsg(msg, PigMsgError);
	    continue;			// ignore the point
	}

	if (type == TIEPOINT_TRADITIONAL ||
	    type == TIEPOINT_MISS_DISTANCE ||
	    type == TIEPOINT_1D ||
	    type == TIEPOINT_INFINITY ||
	    type == TIEPOINT_Z_SURFACE ||
	    type == TIEPOINT_DYNAMIC_XYZ) {		// right side
	    
	    if (tt.right_image == -1) {
	        sprintf(msg, "Right key %d not found in tiepoint set!", tt.right_key);
	        PigModelBase::printStaticMsg(msg, PigMsgError);
	        continue;			// ignore the point
	    }
	}

	// Set the coordinate system... all get it

	tt.cs = set_cs;

	////////
	// DONE, save the tiepoint!
	////////

	tiepoints[n_tiepoints++] = tt;
    }

    // WHEW!!!
	return 0;
}

////////////////////////////////////////////////////////////////////////
// Internal routine to parse an integer attribute.  Prints appropriate
// message and returns PARSE_ERROR if an error, or the attribute doesn't exist.
////////////////////////////////////////////////////////////////////////

int mars_tiepoint_parse_int(DOMElement *elem, char *attrib)
{
    char msg[1024];

    int key = PigXerces::getAttributeInt(elem, attrib, -9999999);

    if (key == -9999999) {
	sprintf(msg, "Error: Malformed or missing %s attribute", attrib);
	PigModelBase::printStaticMsg(msg, PigMsgError);
	return PARSE_ERROR;
    }

    return key;
}

////////////////////////////////////////////////////////////////////////
// Internal routine to parse a float attribute.  Prints appropriate
// message and returns PARSE_ERROR if an error, or the attribute doesn't exist.
////////////////////////////////////////////////////////////////////////

double mars_tiepoint_parse_float(DOMElement *elem, char *attrib)
{
    char msg[1024];

    double key = PigXerces::getAttributeDouble(elem, attrib, -9.999e-9);

    if (key == -9.999e-9) {
	sprintf(msg, "Error: Malformed or missing %s attribute", attrib);
	PigModelBase::printStaticMsg(msg, PigMsgError);
	return PARSE_ERROR;
    }

    return key;
}

////////////////////////////////////////////////////////////////////////
// Internal routine to parse a float attribute.  Like above but does not
// print a message, just returns PARSE_ERROR.  Intended for attributes that
// are optional.
////////////////////////////////////////////////////////////////////////

double mars_tiepoint_parse_float_noerr(DOMElement *elem, char *attrib)
{
    char msg[1024];

    double key = PigXerces::getAttributeDouble(elem, attrib, -9.999e-9);

    if (key == -9.999e-9) {
	return PARSE_ERROR;
    }

    return key;
}

////////////////////////////////////////////////////////////////////////
// Internal routine to map a key entry to the proper value.
// Returns the "new_map" corresponding to the entry in "old_map", or
// the raw key otherwise.
////////////////////////////////////////////////////////////////////////

int mars_tiepoint_map_key(
	int key,			// key number
	int *old_map,			// list of "old" values
	int *new_map,			// list of "new" values
	int n_maps)
{
    for (int i=0; i < n_maps; i++) {
	if (key == old_map[i]) {		// found a match!
	    return new_map[i];
	}
    }

    return key;
}

////////////////////////////////////////////////////////////////////////
// Internal routine to search for a key in the set list, extract the
// name, then search the main list for that name, and return the key.
// Returns -1 on error.
////////////////////////////////////////////////////////////////////////

int mars_tiepoints_xlate_key(int set_key,
		int n_set_files, int *set_keys, char **set_ids,
		int n_files, int *keys, char **ids)
{
    int j;
    char msg[1024];

    // Get the name for this key

    char *name = NULL;
    for (j = 0; j < n_set_files; j++) {
	if (set_key == set_keys[j]) {
	    name = set_ids[j];
	    break;
	}
    }
    if (name == NULL) {		// really shouldn't happen
	sprintf(msg, "Unknown key found in the set!! %d", set_key);
	PigModelBase::printStaticMsg(msg, PigMsgError);
	return -1;		// skip the tiepoint
    }

    // Find the key corresponding to this name in the main list

    int key = -1;
    for (j = 0; j < n_files; j++) {
	if (strcasecmp(name, ids[j]) == 0) {
	    key = keys[j];
	    break;			// found it!
	}
    }
    if (key == -1) {		// really shouldn't happen
	sprintf(msg, "Key not found in main image list!! %s", name);
	PigModelBase::printStaticMsg(msg, PigMsgError);
	return -1;		// skip the tiepoint
    }
 
    return key;
}

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
        int &n_ties,            // INPUT: size of tiepoints[]. OUT: # active
        PigFileModel *files[],  // File list
        int n_files,            // # in file list
        PigCoordSystem *cs,     // Desired CS (NOT what's in file) null==no conv
	PigMission *mission)	// Mission object
{
    char msg[1024];

    // Check for old file format

    FILE *fd;
    if ((fd = fopen(name, "r")) != NULL) {	// cont. on error; handled later
	char line[128];
        fgets(line, sizeof(line), fd);
	fclose(fd);
        if (strncmp(line, "<?xml", 5) != 0) {	// not XML
	    PigModelBase::printStaticMsg("Old-format tiepoint file detected",
								PigMsgInfo);
	    int status = mars_load_tiepoints(name, tiepoints, n_ties);
	    if (status == 1) status = -3;
	    if (status == 2) status = -2;
	    return status;
	}
    }

    int n_loaded_tiepoints = n_ties * 5;	// assume 5x bad tiepoints max
    if (n_loaded_tiepoints < 5000)
	n_loaded_tiepoints = 5000;

    int n_loaded_files = n_files * 5;		// same on files
    if (n_loaded_files < 500)
	n_loaded_files = 500;

    int max_ties = n_ties;
    n_ties = 0;

    char **loaded_ids = new char *[n_loaded_files];
    int *loaded_keys = new int[n_loaded_files];
    TiePoint *loaded_tiepoints = new (std::nothrow) TiePoint[n_loaded_tiepoints];

    if (loaded_ids == NULL || loaded_keys == NULL || loaded_tiepoints == NULL) {
	PigModelBase::printStaticMsg("Memory alloc error in easy mars_load_tiepoints", PigMsgError);
	if (loaded_ids) delete loaded_ids;
	if (loaded_keys) delete loaded_keys;
	if (loaded_tiepoints) delete loaded_tiepoints;
	return -1;
    }

    int status = mars_load_tiepoints(name, loaded_tiepoints, n_loaded_tiepoints,
		loaded_ids, loaded_keys, n_loaded_files, cs, mission);

    if (status != 0) {
	sprintf(msg, "Error loading tiepoint file %s", name);
	PigModelBase::printStaticMsg(msg, PigMsgError);
	return status;
    }

    // See if the keys exist, and if so, transfer the tiepoint over

    for (int i=0; i < n_loaded_tiepoints; i++) {

	// Left-side

	int key = mars_tiepoints_xlate_key2(loaded_tiepoints[i].left_key,
		n_loaded_files, loaded_keys, loaded_ids,
		n_files, files);
	if (key == -1) {
	    PigModelBase::printStaticMsg("Problem is in a left key", PigMsgError);
	    continue;
	}
	loaded_tiepoints[i].left_key = key;
	loaded_tiepoints[i].left_image = key;	// xlate2 defines: same as key

	// Now right-side

	if (loaded_tiepoints[i].type == TIEPOINT_TRADITIONAL ||
	    loaded_tiepoints[i].type == TIEPOINT_MISS_DISTANCE ||
	    loaded_tiepoints[i].type == TIEPOINT_1D ||
	    loaded_tiepoints[i].type == TIEPOINT_INFINITY ||
	    loaded_tiepoints[i].type == TIEPOINT_Z_SURFACE ||
	    loaded_tiepoints[i].type == TIEPOINT_DYNAMIC_XYZ) {

	    key = mars_tiepoints_xlate_key2(
			loaded_tiepoints[i].right_key,
			n_loaded_files, loaded_keys, loaded_ids,
			n_files, files);
	    if (key == -1) {
		PigModelBase::printStaticMsg("Problem is in a right key", PigMsgError);
		continue;
	    }
	    loaded_tiepoints[i].right_key = key;
	    loaded_tiepoints[i].right_image = key; // xlate2 def's: same as key
	}

	// We have a good tiepoint.  Copy it over, and make sure it's active

	if (n_ties == max_ties) {
	    PigModelBase::printStaticMsg("Size of tiepoint array supplied to mars_load_tiepoints (easy) is too small.  Ignoring extra tiepoints.", PigMsgError);
	    continue;			// sigh...
	}

	tiepoints[n_ties] = loaded_tiepoints[i];
	tiepoints[n_ties].active = TRUE;
	n_ties++;
    }

    for (int q=0; q<n_loaded_files; q++)
	delete loaded_ids[q];
    delete loaded_ids;
    delete loaded_keys;
    delete loaded_tiepoints;

    return 0;			// yay!!!
}

////////////////////////////////////////////////////////////////////////
// Internal routine to search for a key in the list, extract the
// name, then search the main list for that name, and return the key.
// Returns -1 on error.  Difference with the above is that the main list
// is a list of PigFileModel's, and the main key is the index.
// Note that "key" is the same as image number here.
////////////////////////////////////////////////////////////////////////

int mars_tiepoints_xlate_key2(int set_key,
		int n_set_files, int *set_keys, char **set_ids,
		int n_files, PigFileModel **models)
{
    int j;
    char msg[1024];

    // Get the name for this key

    char *name = NULL;
    for (j = 0; j < n_set_files; j++) {
	if (set_key == set_keys[j]) {
	    name = set_ids[j];
	    break;
	}
    }
    if (name == NULL) {		// really shouldn't happen
	sprintf(msg, "Unknown key found in the file!! %d", set_key);
	PigModelBase::printStaticMsg(msg, PigMsgError);
	return -1;		// skip the tiepoint
    }

    // Find the key corresponding to this name in the main list

    int key = -1;
    for (j = 0; j < n_files; j++) {
	char unique_id[512];
	models[j]->getUniqueId(unique_id);
	if (strcasecmp(name, unique_id) == 0) {
	    key = j;
	    break;			// found it!
	}
	models[j]->getUniqueId2(unique_id);
	if (strcasecmp(name, unique_id) == 0) {
	    key = j;
	    break;			// found it!
	}
    }
    if (key == -1) {		// really shouldn't happen
	sprintf(msg, "Key not found in main image list!!! %s", name);
	PigModelBase::printStaticMsg(msg, PigMsgError);
	return -1;		// skip the tiepoint
    }
 
    return key;
}

