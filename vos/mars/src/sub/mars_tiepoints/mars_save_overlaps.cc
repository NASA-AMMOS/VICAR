#include "mars_overlaps.h"

#include "PigFileModel.h"

#include <stdio.h>
#include <string.h>

////////////////////////////////////////////////////////////////////////
// WARNING
// Methods in this file are VERY similar to those in mars_save_tiepoints.cc.
// Make sure to maintain in parallel!!!!
////////////////////////////////////////////////////////////////////////

// Note that the "active" status is ignored when saving - all are saved

////////////////////////////////////////////////////////////////////////
// Save overlaps to a file, the easy way.  This corresponds with the
// easy load; all overlaps must reference only images in the file list.
// New keys are assigned based on the order of the supplied file list,
// starting at start_key (this allows later merging of separately-gathered
// overlap files).
//
// To reiterate:  "key" in the tiepoint list is ignored and is in fact
// reset by this method based on "image".
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
        int start_key)          // starting # for keys
{
    int i, j, count;
    char uid[255];
    char *uid_tmp;

// Create a list of unique_id's and keys for use by the other save method

    char **unique_ids = new char *[n_files];
    int *keys = new int[n_files];
    if (unique_ids == NULL || keys == NULL)
	return -1;

    // Check to see if we want to use UniqueId2 in the file instead
    int use_uniqueid2 = false;
    char point_method[256];
    char *value;
    PigModelBase::getStaticParam("POINT_METHOD", point_method, &count, 1, 0);
    if (count != 0) {
        value = PigModelBase::parseParamString(point_method, "USE_UNIQUEID2");
        if (value != NULL)
            use_uniqueid2 = true;
    }

    int current_key = start_key;
    for (i = 0; i < n_files; i++) {
	if (use_uniqueid2)
	    files[i]->getUniqueId2(uid);
	else
	    files[i]->getUniqueId(uid);
	uid_tmp = strdup(uid);
	if (uid_tmp == NULL)
	    return -1;
	unique_ids[i] = uid_tmp;
	keys[i] = current_key++;
    }

// Now set up the keys in the overlap list.  Conveniently, the key just
// happens to be equal to the image number plus start_key.

    for (i = 0; i < n; i++) {
	for (j = 0; j < overlaps[i].n_images; j++) {
	    overlaps[i].stats[j].key = overlaps[i].stats[j].image + start_key;
	}
    }

    int status = mars_save_overlaps(name, overlaps, n,
				unique_ids, keys, n_files);

    for (i = 0; i < n_files; i++) {
	delete unique_ids[i];
    }
    delete unique_ids;
    delete keys;

    return status;

}


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
        char *name,             // filename
        Overlap overlaps[],	// overlaps to save
        int n,                  // # of overlaps
        char *unique_ids[],     // unique ID's from files
        int keys[],             // specific keys per file (MAY NOT be null)
        int n_files)            // number of both the above
{
    FILE *fout;
    char msg[255];
    int i, j, s;
 
#define CHK if (s<0) goto error;

    if ((fout = fopen(name, "w")) == NULL) {
	return -3;			// Error opening file!
    }

    // Write header

    s=fprintf(fout, "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");
    CHK
    s=fprintf(fout, "<overlap_file version=\"1.0\">\n");
    CHK
    s=fprintf(fout, "<overlap_set>\n");
    CHK

    // Write image list

    s=fprintf(fout, "  <images>\n");
    CHK

    for (i = 0; i < n_files; i++) {
	s=fprintf(fout, "    <image unique_id=\"%s\" key=\"%d\"/>\n",
				unique_ids[i], keys[i]);
	CHK
    }
    s=fprintf(fout, "  </images>\n");
    CHK

    // Write overlap list

    s=fprintf(fout, "  <overlaps>\n");
    CHK

    for (i = 0; i < n; i++) {
	if (overlaps[i].n_images == 0)
	    continue;			// skip

	// The only difference between OVERLAP_MEAN_STDEV and OVERLAP_OVERALL
	// is that OVERALL has only one image, and radius is not used.  So
	// there's no difference in how they are saved.  OVERLAP_MEAN_STDEV
	// is the same as OVERLAP_HSI except for the type.

	switch (overlaps[i].type) {

	    case OVERLAP_MEAN_STDEV:
	    case OVERLAP_OVERALL:
	    case OVERLAP_HSI:
	    case OVERLAP_OVERALL_HSI:
		s=fprintf(fout, "    <overlap type=\"%d\" n_images=\"%d\" n_pixels=\"%d\" radius=\"%f\">\n",
			overlaps[i].type, overlaps[i].n_images,
			overlaps[i].n_pixels, overlaps[i].radius);
		CHK

		for (j = 0; j < overlaps[i].n_images; j++) {
		    OverlapStats *st = &overlaps[i].stats[j];
		    s=fprintf(fout, "      <img key=\"%d\" mean=\"%f\" stdev=\"%f\" line=\"%f\" samp=\"%f\"/>\n",
			st->key, st->mean, st->stdev, st->line, st->samp);
		    CHK
		}
		break;

	    default:
		char msg[256];
		sprintf(msg, "ERROR: Unknown overlap type %d encountered saving file.  Overlap ignored.", overlaps[i].type);
		PigModelBase::printStaticMsg(msg, PigMsgError);
		continue;		// skip the trailer below
	}

	// Common trailer for all tiepoints

	s=fprintf(fout, "    </overlap>\n");
	CHK
    }

    // Clean up end of file

    s=fprintf(fout, "  </overlaps>\n");
    CHK
    s=fprintf(fout, "</overlap_set>\n");
    CHK
    s=fprintf(fout, "</overlap_file>\n");
    CHK

    fclose(fout);

    return 0;		// success

error:
    fclose(fout);
    return -2;		// write error

}

