#include "mars_overlaps.h"

#include "PigModelBase.h"
#include "PigMission.h"
#include "PigCoordSystem.h"
#include "PigFileModel.h"
#include "PigXerces.h"

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

////////////////////////////////////////////////////////////////////////
// WARNING
// Methods in this file are VERY similar to those in mars_load_tiepoints.cc.
// Make sure to maintain in parallel!!!!
////////////////////////////////////////////////////////////////////////

#define PARSE_ERROR -999999

// Internal support routines.  See each for usage comments.

static int mars_read_overlap_set(DOMElement *tpt_set, int &n_images,
	char **&ids, int *&keys, int &n_overlaps, Overlap *&overlaps);

// Internal routines from mars_load_tiepoints.cc reused here

int mars_tiepoint_parse_int(DOMElement *elem, char *attrib);
double mars_tiepoint_parse_float(DOMElement *elem, char *attrib);
int mars_tiepoint_map_key(int key, int *old_map, int *new_map, int n_maps);
int mars_tiepoints_xlate_key(int set_key, int n_set_files,
        int *set_keys, char **set_ids, int n_files, int *keys, char **ids);
int mars_tiepoints_xlate_key2(int set_key, int n_set_files,
        int *set_keys, char **set_ids, int n_files, PigFileModel **models);

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
        int &n_overs,           // INPUT: size of overlaps[]. OUT: # active
        char *unique_ids[],     // Returned list of unique ID's from file
        int keys[],             // Returned list of keys from file
        int &n_files)           // IN: size of arrays OUT: # in array
{
    char msg[1024];
    int max_files = n_files;	// save maxes
    n_files = 0;
    int n_overlaps_max = n_overs;
    n_overs = 0;

    PigXerces::initialize();

    DOMDocument *doc = PigXerces::parseFile(name);
    if (doc == NULL) {
	PigXerces::close();
	return -3;		// problem with document
    }

    DOMElement *root = doc->getDocumentElement();   // should be overlap_file
    char *version = PigXerces::getAttribute(root, "version");
    if (strcmp(version,"1.0") != 0) {
	PigModelBase::printStaticMsg("Version error reading overlap file!  Continuing as version 1.0", PigMsgError);
    }
    XMLString::release(&version);

    ////////
    // Get all the overlap sets, process each in turn
    ////////

    DOMNodeList *sets = PigXerces::getElementsByTagName(root, "overlap_set");

    for (int set = 0; set < sets->getLength(); set++) {

	DOMElement *ovr_set = PigXerces::nextElement(sets, set);
	if (ovr_set == NULL) {
	    PigModelBase::printStaticMsg(
		    "Expected Element type not found in overlap_set; ignored",
		    PigMsgWarning);
	    continue;
	}

	char **set_ids;
	int *set_keys;
	Overlap *set_overlaps;
	int n_set_files, n_set_overlaps;

	// Read in the overlap set

	mars_read_overlap_set(ovr_set, n_set_files, set_ids, set_keys,
		n_set_overlaps, set_overlaps);

	if (n_set_files == 0 || n_set_overlaps == 0) {
	    if (set_ids) {
		for (int q=0; q<n_set_files; q++)
		    delete set_ids[q];
		delete set_ids;
	    }
	    if (set_keys) delete set_keys;
	    if (set_overlaps) delete set_overlaps;
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
		    sprintf(msg, "Out of memory reading overlap file.  Must ignore image %s", set_ids[i]);
		    PigModelBase::printStaticMsg(msg, PigMsgError);
		    continue;
		}
		unique_ids[n_files] = strdup(set_ids[i]);
		keys[n_files] = n_files;		// use index as key
		n_files++;
	    }
	}

	// Now, merge in the overlaps.  We don't need the keys any more...
	// we simply use them to look up the name in the set's table, then
	// look up that name in the main table.

	for (i = 0; i < n_set_overlaps; i++) {

	    int good = TRUE;
	    for (j = 0; j < set_overlaps[i].n_images; j++) {

		OverlapStats *st = &set_overlaps[i].stats[j];

	        int key = mars_tiepoints_xlate_key(
			st->key,
			n_set_files, set_keys, set_ids,
			n_files, keys, unique_ids);

	        if (key == -1) {
		    sprintf(msg, "Problem is in key for index %d", j);
		    PigModelBase::printStaticMsg(msg, PigMsgError);
		    good = FALSE;
		    break;
	        }
		st->key = key;

	        for (int k = 0; k < n_files; k++) {   // Set image based on key
	            if (key == keys[k]) {
		        st->image = k;
		        break;
	            }
	        }
	    }

	    // We have a good overlap.  Copy it over.

	    if (n_overs == n_overlaps_max) {
		PigModelBase::printStaticMsg("Memory error in final overlap array!", PigMsgError);
		continue;			// sigh...
	    }

	    if (good) {
	        overlaps[n_overs] = set_overlaps[i];
	        n_overs++;
	    }
	}

	// Deallocate memory from reading

	for (int q=0; q<n_set_files; q++)
	    delete set_ids[q];
	delete set_ids;
	delete set_keys;
	delete set_overlaps;

    }

    doc->release();

    PigXerces::close();

    return 0;		// done!!!
}

////////////////////////////////////////////////////////////////////////
// Internal routine to read a single overlap_set.
// ids, keys, and overlaps are allocated, as well as each element in ids.
// Caller must free them.
////////////////////////////////////////////////////////////////////////
int mars_read_overlap_set(
		DOMElement *ovr_set,		// "overlap_set" element
		int &n_images,			// number of images (ids/keys)
		char **&ids,			// list of id's (alloc'd)
		int *&keys,			// list of keys
		int &n_overlaps,		// number of overlaps
		Overlap *&overlaps)		// list of overlaps
{
    char msg[1024];
    int j, k;

    ids = NULL;				// in case of early return
    keys = NULL;
    overlaps = NULL;
    n_images = n_overlaps = 0;

    ////////
    // Get the list of images and keys that go with them
    ////////

    DOMElement *images_el = PigXerces::getOneElement(ovr_set, "images");
    if (images_el == NULL)
	return 0;			// No images, give up on this set


    DOMNodeList *image_nodes = PigXerces::getElementsByTagName(
							images_el, "image");

    int n_images_max = image_nodes->getLength();
    if (n_images_max == 0)
	return 0;			// can't use the overlaps...

    ids = new char *[n_images_max];
    keys = new int[n_images_max];
    if (ids == NULL || keys == NULL) {
	PigModelBase::printStaticMsg("ERROR: memory error reading overlap file image list!", PigMsgError);
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
	PigModelBase::printStaticMsg("ERROR: memory error alloc'ing map while reading overlap file!  File not read!", PigMsgError);
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
	for (j=0; j < n_images; j++) {
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
		sprintf(msg, "DUPLICATE KEY in overlap file: %d!!  Second entry is ignored; results may be suspect", key);
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
    // Now read the actual overlaps
    // note: at this point, ids/keys are good, so we return them even if
    // there is an error in the overlap processing
    ////////

    DOMElement *overlaps_el = PigXerces::getOneElement(ovr_set, "overlaps");
    if (overlaps_el == NULL) {
	if (old_map) delete old_map;
	if (new_map) delete new_map;
	return -1;			// No overlaps; give up on this set
    }

    DOMNodeList *overlap_nodes = PigXerces::getElementsByTagName(
							overlaps_el, "overlap");

    int n_overlaps_max = overlap_nodes->getLength();
    overlaps = new Overlap[n_overlaps_max];
    if (overlaps == NULL) {
	PigModelBase::printStaticMsg("ERROR: memory error (overlap array) reading overlap file!", PigMsgError);
	if (old_map) delete old_map;
	if (new_map) delete new_map;
	return -1;				// done with this set
    }

    for (int t = 0; t < n_overlaps_max; t++) {

	DOMElement *overlap_el = PigXerces::nextElement(overlap_nodes, t);

	int type = mars_tiepoint_parse_int(overlap_el, "type");
	if (type == PARSE_ERROR)
	    continue;			// a problem (message printed already)

	// The only difference between OVERLAP_MEAN_STDEV and OVERLAP_OVERALL
	// is that there's only one image for OVERALL and radius is not
	// relevant.  Since radius is optional anyway and there's no check on
	// the # of images, there is no difference in the load between them.
	// OVERLAP_HSI and OVERLAP_MEAN_STDEV are the same except for the type.

	Overlap oo;			// temp overlap for use during parsing
	oo.type = type;			// set "invalid" values
	oo.active = FALSE;
	oo.n_images = 0;
	oo.n_pixels = 0;
	memset(oo.stats, 0, sizeof(oo.stats));

	oo.n_images = mars_tiepoint_parse_int(overlap_el, "n_images");
	if (oo.n_images == PARSE_ERROR)
	    continue;			// bad, next (msg printed already)

	oo.n_pixels = mars_tiepoint_parse_int(overlap_el, "n_pixels");
	if (oo.n_pixels == PARSE_ERROR) {
	    oo.n_images = 0;
	    continue;			// bad, next (msg printed already)
	}

	oo.radius = mars_tiepoint_parse_float(overlap_el, "radius");
	if (oo.radius == PARSE_ERROR) {
	    oo.radius = 0.0;		// not a fatal error, so keep going
	}

        DOMNodeList *img_nodes = PigXerces::getElementsByTagName(
							overlap_el, "img");

        int n_img_max = img_nodes->getLength();
	if (n_img_max > MAX_OVERLAP_IMAGES) {
	    sprintf(msg, "ERROR: too many overlaps (%d) for node.  Increase max and recompile.", n_img_max);
	    PigModelBase::printStaticMsg(msg, PigMsgError);
	    if (old_map) delete old_map;
	    if (new_map) delete new_map;
	    return -1;				// done with this set
        }

        for (int im = 0; im < n_img_max; im++) {

	    DOMElement *img_el = PigXerces::nextElement(img_nodes, im);

	    ////////
	    // key
	    ////////

	    oo.stats[im].key = mars_tiepoint_parse_int(img_el, "key");
	    if (oo.stats[im].key == PARSE_ERROR) {
		oo.n_images = 0;
	        continue;		// a problem (message printed already)
	    }

	    oo.stats[im].key = mars_tiepoint_map_key(oo.stats[im].key,
						old_map, new_map, n_map);

	    ////////
	    // mean
	    ////////

	    oo.stats[im].mean = mars_tiepoint_parse_float(img_el, "mean");
	    if (oo.stats[im].mean == PARSE_ERROR) {
		oo.n_images = 0;
		continue;
	    }

	    ////////
	    // stdev
	    ////////

	    oo.stats[im].stdev = mars_tiepoint_parse_float(img_el, "stdev");
	    if (oo.stats[im].stdev == PARSE_ERROR) {
		oo.n_images = 0;
		continue;
	    }

	    ////////
	    // line
	    ////////

	    oo.stats[im].line = mars_tiepoint_parse_float(img_el, "line");
	    if (oo.stats[im].line == PARSE_ERROR) {
		oo.stats[im].line = 0.0;	// not fatal so keep going
	    }

	    ////////
	    // samp
	    ////////

	    oo.stats[im].samp = mars_tiepoint_parse_float(img_el, "samp");
	    if (oo.stats[im].samp == PARSE_ERROR) {
		oo.stats[im].samp = 0.0;	// not fatal so keep going
	    }
	}

	////////
	// Set up image indices
	////////

	for (j = 0; j < oo.n_images; j++) {
	    oo.stats[j].image = -1;

	    for (k = 0; k < n_images; k++) {
	        if (oo.stats[j].key == keys[k]) {
		    oo.stats[j].image = k;
		    break;
	        }
	    }

	    if (oo.stats[j].image == -1) {
	        sprintf(msg, "key[%d] %d not found in overlap set!", j, oo.stats[j].key);
	        PigModelBase::printStaticMsg(msg, PigMsgError);
	        continue;			// ignore the overlap
	    }

	}

	////////
	// DONE, save the overlap!
	////////

	if (oo.n_images != 0) {
	    oo.active = TRUE;
	    overlaps[n_overlaps++] = oo;
	}
    }

    // WHEW!!!
	return 0;
}

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
        int &n_overlaps,        // INPUT: size of overlaps[]. OUT: # active
        PigFileModel *files[],  // File list
        int n_files)            // # in file list
{
    char msg[1024];

    int n_loaded_overlaps = n_overlaps * 5;	// assume 5x bad overlaps max
    if (n_loaded_overlaps < 5000)
	n_loaded_overlaps = 5000;

    int n_loaded_files = n_files * 5;		// same on files
    if (n_loaded_files < 500)
	n_loaded_files = 500;

    int max_overlaps = n_overlaps;
    n_overlaps = 0;

    char **loaded_ids = new char *[n_loaded_files];
    int *loaded_keys = new int[n_loaded_files];
    Overlap *loaded_overlaps = new Overlap[n_loaded_overlaps];

    if (loaded_ids == NULL || loaded_keys == NULL || loaded_overlaps == NULL) {
	PigModelBase::printStaticMsg("Memory alloc error in easy mars_load_overlaps", PigMsgError);
	if (loaded_ids) delete loaded_ids;
	if (loaded_keys) delete loaded_keys;
	if (loaded_overlaps) delete loaded_overlaps;
	return -1;
    }

    int status = mars_load_overlaps(name, loaded_overlaps, n_loaded_overlaps,
		loaded_ids, loaded_keys, n_loaded_files);

    if (status != 0) {
	sprintf(msg, "Error loading overlap file %s", name);
	PigModelBase::printStaticMsg(msg, PigMsgError);
	return status;
    }

    // See if the keys exist, and if so, transfer the overlap over

    for (int i=0; i < n_loaded_overlaps; i++) {

	int good = TRUE;
	for (int j=0; j < loaded_overlaps[i].n_images; j++) {
	    OverlapStats *st = &loaded_overlaps[i].stats[j];
	    int key = mars_tiepoints_xlate_key2(st->key,
		n_loaded_files, loaded_keys, loaded_ids,
		n_files, files);
	    if (key == -1) {
		sprintf(msg, "Problem is in a key index %d", j);
	        PigModelBase::printStaticMsg(msg, PigMsgError);
		good = FALSE;
	        break;
	    }
	    st->key = key;
	    st->image = key;		// xlate2 defines: same as key

	}

	// We have a good overlap.  Copy it over, and make sure it's active

	if (n_overlaps == max_overlaps) {
	    PigModelBase::printStaticMsg("Size of overlap array supplied to mars_load_overlaps (easy) is too small.  Ignoring extra overlaps.", PigMsgError);
	    continue;			// sigh...
	}

	if (good && loaded_overlaps[i].n_images > 0) {
	    overlaps[n_overlaps] = loaded_overlaps[i];
	    n_overlaps++;
        }
    }

    for (int q=0; q<n_loaded_files; q++)
	delete loaded_ids[q];
    delete loaded_ids;
    delete loaded_keys;
    delete loaded_overlaps;

    return 0;			// yay!!!
}

